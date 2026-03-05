# =============================================================================
# 00a_build_training_data.R
# Build a labeled dataset of confirmed violent events (positive class)
# and non-strike thermal anomalies (negative class)
#
# DATA SOURCE: UCDP Georeferenced Event Dataset (GED)
#   - API currently requires access token header x-ucdp-access-token
#   - API docs: https://ucdp.uu.se/apidocs/
#   - Version 25.1 covers 1989-2024 globally
#   - Candidate datasets provide monthly near-real-time updates
#
# Extract spectral + contextual features from Sentinel-2 + FIRMS
# =============================================================================

library(httr2)
library(tidyverse)
library(sf)
library(terra)
library(lubridate)
library(glue)
library(jsonlite)

# â”€â”€ Configuration â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

FIRMS_API_KEY    <- Sys.getenv("FIRMS_API_KEY")
SH_CLIENT_ID     <- Sys.getenv("SH_CLIENT_ID")
SH_CLIENT_SECRET <- Sys.getenv("SH_CLIENT_SECRET")
UCDP_ACCESS_TOKEN <- Sys.getenv("UCDP_ACCESS_TOKEN")
if (!nzchar(UCDP_ACCESS_TOKEN)) {
  # Backward-compatible alias
  UCDP_ACCESS_TOKEN <- Sys.getenv("UCDP_API_KEY")
}

# UCDP API
UCDP_BASE_URL    <- "https://ucdpapi.pcr.uu.se/api"
UCDP_GED_VERSION <- "25.1"                     # latest yearly release
UCDP_CANDIDATE   <- "25.0.12"                  # latest monthly candidate
UCDP_CSV_FALLBACK_PATH <- "data/GEDEvent_v25_1.csv"

# Training data parameters
SAMPLE_BUFFER_KM   <- 15
NEGATIVE_RATIO     <- 2       # 2 negatives per positive
BAND_RESOLUTION    <- 20
MIN_DATE           <- "2019-01-01"
MAX_DATE           <- "2024-12-31"

# UCDP country IDs for the Middle East
# See https://ucdp.uu.se/downloads/ for full list
# Key countries: Iraq=645, Syria=652, Yemen=678, Lebanon=660,
# Israel=666, Palestine=667 (State of Palestine), Turkey=640, Iran=630
UCDP_COUNTRY_IDS <- c(645, 652, 678, 660, 666, 667, 640, 630)

# Region of interest bounding box (same as detection pipeline)
ROI <- list(west = 34.0, south = 28.0, east = 48.0, north = 37.5)


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# PART 1: COLLECT LABELED EVENTS FROM UCDP-GED
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

# â”€â”€ 1.1 Pull Violent Events from UCDP GED API â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

pull_ucdp_events <- function(version = UCDP_GED_VERSION,
                              country_ids = UCDP_COUNTRY_IDS,
                              start_date = MIN_DATE,
                              end_date = MAX_DATE,
                              type_of_violence = c(1, 3)) {
  # UCDP type_of_violence:
  #   1 = state-based (government vs. rebel / interstate â€” includes airstrikes)
  #   2 = non-state (rebel vs. rebel)
  #   3 = one-sided (actor vs. civilians â€” includes bombings, shelling)
  # We want types 1 and 3 as they include aerial bombardment and shelling
  
  message("Pulling events from UCDP GED v", version, "...")
  message("  Countries: ", paste(country_ids, collapse = ", "))
  message("  Date range: ", start_date, " to ", end_date)
  if (!nzchar(UCDP_ACCESS_TOKEN)) {
    message("  UCDP_ACCESS_TOKEN is empty; request may fail with HTTP 401.")
  }
  
  country_str <- paste(country_ids, collapse = ",")
  violence_str <- paste(type_of_violence, collapse = ",")
  
  all_events <- tibble()
  page <- 0
  total_pages <- 1  # will be updated after first request
  page_size <- 1000  # max allowed
  
  while (page < total_pages) {
    url <- glue("{UCDP_BASE_URL}/gedevents/{version}")
    
    req <- request(url) |>
      req_url_query(
        pagesize       = page_size,
        page           = page,
        Country        = country_str,
        StartDate      = start_date,
        EndDate        = end_date,
        TypeOfViolence = violence_str
      )
    
    if (nzchar(UCDP_ACCESS_TOKEN)) {
      req <- req |> req_headers(`x-ucdp-access-token` = UCDP_ACCESS_TOKEN)
    }
    
    resp <- tryCatch(
      req |>
        req_timeout(120) |>
        req_perform(),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("401", msg, fixed = TRUE)) {
          stop(
            paste(
              "UCDP request failed with HTTP 401.",
              "Set UCDP_ACCESS_TOKEN in your environment",
              "(header: x-ucdp-access-token)."
            ),
            call. = FALSE
          )
        }
        stop(msg, call. = FALSE)
      }
    )
    
    data <- resp_body_json(resp)
    
    total_pages <- data$TotalPages
    total_count <- data$TotalCount
    
    if (page == 0) {
      message(glue("  Total events: {total_count} across {total_pages} pages"))
    }
    
    # Parse results
    if (length(data$Result) > 0) {
      page_df <- map_dfr(data$Result, function(evt) {
        tibble(
          event_id       = evt$id,
          relid          = evt$relid %||% NA_character_,
          year           = evt$year,
          date_start     = evt$date_start %||% NA_character_,
          date_end       = evt$date_end %||% NA_character_,
          violence_type  = evt$type_of_violence,
          conflict_name  = evt$conflict_name %||% NA_character_,
          dyad_name      = evt$dyad_name %||% NA_character_,
          side_a         = evt$side_a %||% NA_character_,
          side_b         = evt$side_b %||% NA_character_,
          country        = evt$country %||% NA_character_,
          region         = evt$region %||% NA_character_,
          adm_1          = evt$adm_1 %||% NA_character_,
          adm_2          = evt$adm_2 %||% NA_character_,
          lat            = as.numeric(evt$latitude %||% NA),
          lon            = as.numeric(evt$longitude %||% NA),
          where_prec     = evt$where_prec %||% NA_integer_,
          where_desc     = evt$where_description %||% NA_character_,
          deaths_a       = evt$deaths_a %||% 0,
          deaths_b       = evt$deaths_b %||% 0,
          deaths_civilians = evt$deaths_civilians %||% 0,
          deaths_unknown = evt$deaths_unknown %||% 0,
          best_est       = evt$best %||% 0,
          high_est       = evt$high %||% 0,
          low_est        = evt$low %||% 0,
          source_article = evt$source_article %||% NA_character_
        )
      })
      
      all_events <- bind_rows(all_events, page_df)
    }
    
    page <- page + 1
    
    if (page %% 10 == 0) {
      message(glue("  Page {page}/{total_pages} ({nrow(all_events)} events so far)"))
    }
    
    # Be respectful of the API
    Sys.sleep(0.3)
  }
  
  message(glue("  â†’ Retrieved {nrow(all_events)} events total"))
  return(all_events)
}


# â”€â”€ 1.2 Also Pull Candidate (Near-Real-Time) Data â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

pull_ucdp_candidate <- function(version = UCDP_CANDIDATE) {
  # Candidate data covers the most recent months not yet in the yearly release
  message("Pulling UCDP Candidate data v", version, "...")
  
  pull_ucdp_events(
    version = version,
    country_ids = UCDP_COUNTRY_IDS,
    start_date = "2025-01-01",
    end_date = format(Sys.Date(), "%Y-%m-%d")
  )
}


# â”€â”€ 1.3 Filter for Airstrike / Bombardment Events â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

identify_airstrike_events <- function(events) {
  # UCDP doesn't have a dedicated "airstrike" field like ACLED, but we can
  # identify likely aerial attacks through:
  #   1. Source text mentions (airstrike, bombing, drone, aerial)
  #   2. State-based violence (type 1) with government as side_a
  #   3. High precision geolocation (where_prec == 1 or 2)
  #   4. One-sided violence (type 3) indicating bombardment of civilians
  
  message("Filtering for airstrike/bombardment signatures...")
  
  # Keywords suggesting aerial/remote violence
  strike_keywords <- c(
    "airstrike", "air strike", "air raid", "aerial",
    "bombardment", "bombed", "bombing",
    "drone", "UAV", "unmanned",
    "missile", "rocket",
    "shelling", "shelled",
    "barrel bomb"
  )
  
  keyword_pattern <- paste(strike_keywords, collapse = "|")
  
  events <- events %>%
    mutate(
      # Check source text for strike keywords
      source_lower = str_to_lower(source_article %||% ""),
      has_strike_keyword = str_detect(source_lower, keyword_pattern),
      
      # Government actor on side_a (typical for airstrikes)
      is_state_violence = violence_type == 1 &
        str_detect(side_a, "^Government of"),
      
      # One-sided violence against civilians (often shelling/bombing)
      is_onesided = violence_type == 3,
      
      # High precision location
      high_precision = where_prec %in% c(1, 2),
      
      # Composite airstrike score
      strike_score = as.integer(has_strike_keyword) * 3 +
        as.integer(is_state_violence) * 2 +
        as.integer(is_onesided) * 1 +
        as.integer(high_precision) * 1
    )
  
  # Tier 1: Definite airstrikes (source mentions + state actor)
  tier1 <- events %>%
    filter(has_strike_keyword & (is_state_violence | is_onesided)) %>%
    mutate(strike_tier = "confirmed")
  
  # Tier 2: Probable (state violence with deaths, high precision)
  tier2 <- events %>%
    filter(!event_id %in% tier1$event_id) %>%
    filter(is_state_violence & best_est >= 1 & high_precision) %>%
    mutate(strike_tier = "probable")
  
  # Tier 3: Possible (one-sided violence with high fatalities)
  tier3 <- events %>%
    filter(!event_id %in% c(tier1$event_id, tier2$event_id)) %>%
    filter(is_onesided & best_est >= 5) %>%
    mutate(strike_tier = "possible")
  
  positives <- bind_rows(tier1, tier2, tier3) %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    mutate(label = 1L)
  
  message(glue("  â†’ {nrow(tier1)} confirmed, {nrow(tier2)} probable, ",
               "{nrow(tier3)} possible"))
  message(glue("  â†’ {nrow(positives)} total positive samples"))
  
  return(positives)
}


# â”€â”€ 1.4 Alternative: Direct CSV Download â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

# If you prefer, download the full GED CSV from https://ucdp.uu.se/downloads/
# and load it directly.
load_ucdp_csv <- function(path = UCDP_CSV_FALLBACK_PATH,
                          country_ids = UCDP_COUNTRY_IDS,
                          start_date = MIN_DATE,
                          end_date = MAX_DATE,
                          type_of_violence = c(1, 3)) {
  if (!file.exists(path)) {
    stop(
      paste0(
        "UCDP CSV fallback file not found at ", path, ". ",
        "Download GED CSV from https://ucdp.uu.se/downloads/."
      ),
      call. = FALSE
    )
  }
  
  message("Loading UCDP CSV fallback: ", path)
  raw <- read_csv(path, show_col_types = FALSE)
  
  pick_col <- function(df, candidates) {
    hits <- match(tolower(candidates), tolower(names(df)))
    hits <- hits[!is.na(hits)]
    if (length(hits) == 0) return(NULL)
    names(df)[hits[[1]]]
  }
  
  get_chr <- function(df, candidates, default = NA_character_) {
    col <- pick_col(df, candidates)
    if (is.null(col)) rep(default, nrow(df)) else as.character(df[[col]])
  }
  
  get_num <- function(df, candidates, default = NA_real_) {
    col <- pick_col(df, candidates)
    if (is.null(col)) rep(default, nrow(df)) else suppressWarnings(as.numeric(df[[col]]))
  }
  
  get_int <- function(df, candidates, default = NA_integer_) {
    col <- pick_col(df, candidates)
    if (is.null(col)) rep(default, nrow(df)) else suppressWarnings(as.integer(df[[col]]))
  }
  
  country_id_col <- pick_col(raw, c("country_id", "countryid"))
  date_col <- pick_col(raw, c("date_start", "date"))
  violence_col <- pick_col(raw, c("type_of_violence", "typeofviolence"))
  
  if (is.null(date_col) || is.null(violence_col)) {
    stop(
      "UCDP CSV is missing required columns (date_start/date and type_of_violence).",
      call. = FALSE
    )
  }
  
  date_vec <- as.Date(raw[[date_col]])
  violence_vec <- suppressWarnings(as.integer(raw[[violence_col]]))
  keep <- !is.na(date_vec) &
    date_vec >= as.Date(start_date) &
    date_vec <= as.Date(end_date) &
    violence_vec %in% type_of_violence
  
  if (!is.null(country_id_col)) {
    country_vec <- suppressWarnings(as.integer(raw[[country_id_col]]))
    keep <- keep & country_vec %in% country_ids
  } else {
    message("  CSV has no country_id column; country filter skipped.")
  }
  
  filtered <- raw[keep, , drop = FALSE]
  
  year_col <- pick_col(filtered, c("year"))
  date_start_col <- pick_col(filtered, c("date_start", "date"))
  id_col <- pick_col(filtered, c("id", "event_id"))
  
  out <- tibble(
    event_id         = if (is.null(id_col)) paste0("csv_", seq_len(nrow(filtered))) else as.character(filtered[[id_col]]),
    relid            = get_chr(filtered, c("relid")),
    year             = if (is.null(year_col)) suppressWarnings(year(as.Date(get_chr(filtered, c("date_start", "date"))))) else suppressWarnings(as.integer(filtered[[year_col]])),
    date_start       = get_chr(filtered, c("date_start", "date")),
    date_end         = get_chr(filtered, c("date_end")),
    violence_type    = get_int(filtered, c("type_of_violence", "typeofviolence")),
    conflict_name    = get_chr(filtered, c("conflict_name")),
    dyad_name        = get_chr(filtered, c("dyad_name")),
    side_a           = get_chr(filtered, c("side_a")),
    side_b           = get_chr(filtered, c("side_b")),
    country          = get_chr(filtered, c("country")),
    region           = get_chr(filtered, c("region")),
    adm_1            = get_chr(filtered, c("adm_1", "adm1")),
    adm_2            = get_chr(filtered, c("adm_2", "adm2")),
    lat              = get_num(filtered, c("latitude", "lat")),
    lon              = get_num(filtered, c("longitude", "lon")),
    where_prec       = get_int(filtered, c("where_prec", "whereprecision")),
    where_desc       = get_chr(filtered, c("where_description", "where_desc")),
    deaths_a         = get_num(filtered, c("deaths_a"), default = 0),
    deaths_b         = get_num(filtered, c("deaths_b"), default = 0),
    deaths_civilians = get_num(filtered, c("deaths_civilians"), default = 0),
    deaths_unknown   = get_num(filtered, c("deaths_unknown"), default = 0),
    best_est         = get_num(filtered, c("best"), default = 0),
    high_est         = get_num(filtered, c("high"), default = 0),
    low_est          = get_num(filtered, c("low"), default = 0),
    source_article   = get_chr(filtered, c("source_article", "source_headline", "source"))
  )
  
  message(glue("  -> Loaded {nrow(out)} events from CSV fallback"))
  out
}


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# PART 2: BUILD NEGATIVE SAMPLES
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

build_negative_samples <- function(n_negatives) {
  message("Building negative samples...")
  
  # â”€â”€ 2a. Known gas flare locations â”€â”€
  gas_flares <- tribble(
    ~lat,    ~lon,    ~type,          ~country,
    30.55,   47.32,   "gas_flare",    "Iraq",
    31.03,   47.15,   "gas_flare",    "Iraq",
    35.47,   44.39,   "gas_flare",    "Iraq",
    34.93,   43.49,   "gas_flare",    "Iraq",
    30.35,   48.10,   "gas_flare",    "Iraq",
    36.35,   43.15,   "gas_flare",    "Iraq",
    28.92,   48.08,   "gas_flare",    "Kuwait",
    25.35,   51.18,   "gas_flare",    "Qatar",
    26.20,   50.55,   "gas_flare",    "Bahrain",
    34.80,   36.70,   "gas_flare",    "Syria",
    33.40,   44.38,   "gas_flare",    "Iraq",
    30.10,   47.95,   "gas_flare",    "Iraq"
  ) %>%
    mutate(label = 0L, source = "gas_flare_inventory", date = NA_Date_)
  
  # â”€â”€ 2b. Agricultural burn locations â”€â”€
  ag_burns <- tribble(
    ~lat,    ~lon,    ~type,          ~country,
    36.80,   40.20,   "ag_burn",      "Syria",
    36.55,   41.90,   "ag_burn",      "Iraq",
    35.50,   44.00,   "ag_burn",      "Iraq",
    33.30,   44.40,   "ag_burn",      "Iraq",
    32.60,   36.10,   "ag_burn",      "Jordan",
    37.00,   36.20,   "ag_burn",      "Turkey",
    36.20,   37.15,   "ag_burn",      "Syria",
    34.50,   43.50,   "ag_burn",      "Iraq",
    36.10,   43.80,   "ag_burn",      "Iraq",
    33.00,   43.00,   "ag_burn",      "Iraq"
  ) %>%
    mutate(label = 0L, source = "ag_burn_known", date = NA_Date_)
  
  # â”€â”€ 2c. Random FIRMS background detections â”€â”€
  n_firms <- n_negatives - nrow(gas_flares) - nrow(ag_burns)
  firms_negatives <- pull_firms_negatives(n = max(n_firms, 50))
  
  # Combine
  negatives <- bind_rows(
    gas_flares %>% select(lat, lon, type, country, label, source, date),
    ag_burns %>% select(lat, lon, type, country, label, source, date),
    firms_negatives
  ) %>%
    mutate(
      event_id = paste0("neg_", row_number()),
      date = if_else(
        is.na(date),
        as.Date(MIN_DATE) +
          sample(0:as.numeric(as.Date(MAX_DATE) - as.Date(MIN_DATE)),
                 n(), replace = TRUE),
        date
      )
    )
  
  message(glue("  â†’ {nrow(negatives)} negative samples"))
  return(negatives)
}


pull_firms_negatives <- function(n = 200) {
  message("  Pulling FIRMS background detections...")
  
  if (!nzchar(FIRMS_API_KEY) || FIRMS_API_KEY == "your_firms_api_key_here") {
    message("    FIRMS_API_KEY missing/placeholder; skipping FIRMS negatives.")
    return(tibble(
      lat = numeric(), lon = numeric(), type = character(),
      country = character(), label = integer(),
      source = character(), date = as.Date(character())
    ))
  }
  
  sample_dates <- seq(as.Date(MIN_DATE), as.Date(MAX_DATE), by = "month") %>%
    sample(min(12, length(.)))
  
  all_firms <- map_dfr(sample_dates, function(d) {
    bbox_str <- paste(ROI$west, ROI$south, ROI$east, ROI$north, sep = ",")
    url <- glue(
      "https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
      "{FIRMS_API_KEY}/VIIRS_SNPP_NRT/{bbox_str}/1/",
      "{format(d, '%Y-%m-%d')}"
    )
    tryCatch({
      resp <- request(url) |> req_timeout(60) |> req_perform()
      body <- resp_body_string(resp)
      if (startsWith(body, "Invalid MAP_KEY")) return(tibble())
      read_csv(I(body), show_col_types = FALSE) %>%
        mutate(sample_date = d)
    }, error = function(e) {
      message(glue("    FIRMS failed for {d}: {e$message}"))
      tibble()
    })
  })
  
  if (nrow(all_firms) == 0) {
    message("    No FIRMS data retrieved â€” using coordinates only")
    return(tibble(
      lat = numeric(), lon = numeric(), type = character(),
      country = character(), label = integer(),
      source = character(), date = as.Date(character())
    ))
  }
  
  all_firms %>%
    filter(frp > 2, frp < 50, confidence >= 50) %>%
    slice_sample(n = min(n, nrow(.))) %>%
    transmute(
      lat     = latitude,
      lon     = longitude,
      type    = "firms_background",
      country = NA_character_,
      label   = 0L,
      source  = "firms_random",
      date    = as.Date(acq_date)
    )
}


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# PART 3: EXTRACT SPECTRAL FEATURES
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

get_sh_token <- function() {
  resp <- request("https://services.sentinel-hub.com/auth/realms/main/protocol/openid-connect/token") |>
    req_body_form(
      grant_type    = "client_credentials",
      client_id     = SH_CLIENT_ID,
      client_secret = SH_CLIENT_SECRET
    ) |>
    req_perform()
  resp_body_json(resp)$access_token
}


EVALSCRIPT_STATS <- '
//VERSION=3
function setup() {
  return {
    input: [{
      bands: ["B02","B03","B04","B05","B06","B07","B08","B8A","B11","B12","SCL"],
      units: "DN"
    }],
    output: { bands: 10, sampleType: "FLOAT32" },
    mosaicking: "ORBIT"
  };
}
function evaluatePixel(samples) {
  let s = samples[0];
  return [s.B02/10000, s.B03/10000, s.B04/10000, s.B05/10000, s.B06/10000,
          s.B07/10000, s.B08/10000, s.B8A/10000, s.B11/10000, s.B12/10000];
}
'


extract_spectral_features <- function(lat, lon, event_date, token,
                                       buffer_km = 5) {
  pt <- st_point(c(lon, lat)) %>% st_sfc(crs = 4326)
  bbox <- pt %>%
    st_transform(3857) %>%
    st_buffer(buffer_km * 1000) %>%
    st_transform(4326) %>%
    st_bbox()
  
  date_from <- format(event_date - days(2), "%Y-%m-%dT00:00:00Z")
  date_to   <- format(event_date + days(2), "%Y-%m-%dT23:59:59Z")
  
  body <- list(
    input = list(
      bounds = list(
        bbox = unname(as.list(bbox[c("xmin", "ymin", "xmax", "ymax")])),
        properties = list(crs = "http://www.opengis.net/def/crs/EPSG/0/4326")
      ),
      data = list(list(
        type = "sentinel-2-l2a",
        dataFilter = list(
          timeRange = list(from = date_from, to = date_to),
          maxCloudCoverage = 50
        )
      ))
    ),
    output = list(
      width  = 128,
      height = 128,
      responses = list(
        list(identifier = "default", format = list(type = "image/tiff"))
      )
    ),
    evalscript = EVALSCRIPT_STATS
  )
  
  resp <- tryCatch({
    request("https://services.sentinel-hub.com/api/v1/process") |>
      req_headers(
        Authorization  = paste("Bearer", token),
        `Content-Type` = "application/json",
        Accept         = "image/tiff"
      ) |>
      req_body_json(body) |>
      req_timeout(120) |>
      req_perform()
  }, error = function(e) NULL)
  
  if (is.null(resp) || resp_status(resp) != 200) return(NULL)
  
  tmp <- tempfile(fileext = ".tif")
  writeBin(resp_body_raw(resp), tmp)
  
  r <- tryCatch(rast(tmp), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  
  names(r) <- c("B02", "B03", "B04", "B05", "B06", "B07",
                 "B08", "B8A", "B11", "B12")
  
  compute_features_from_raster(r, lat, lon)
}


compute_features_from_raster <- function(r, center_lat, center_lon) {
  
  # Band statistics
  band_means <- global(r, "mean", na.rm = TRUE)$mean
  band_sds   <- global(r, "sd", na.rm = TRUE)$sd
  band_maxs  <- global(r, "max", na.rm = TRUE)$max
  names(band_means) <- paste0(names(r), "_mean")
  names(band_sds)   <- paste0(names(r), "_sd")
  names(band_maxs)  <- paste0(names(r), "_max")
  
  # Spectral indices
  ndvi <- (r$B8A - r$B04) / (r$B8A + r$B04 + 1e-6)
  aerosol_idx <- (r$B02 - r$B04) / (r$B02 + r$B04 + 1e-6)
  swir_smoke <- (r$B12 - r$B8A) / (r$B12 + r$B8A + 1e-6)
  nbr <- (r$B8A - r$B12) / (r$B8A + r$B12 + 1e-6)
  swir_ratio <- r$B12 / (r$B11 + 1e-6)
  brightness <- (r$B02 + r$B03 + r$B04) / 3
  re_idx1 <- (r$B05 - r$B04) / (r$B05 + r$B04 + 1e-6)
  re_idx2 <- (r$B07 - r$B05) / (r$B07 + r$B05 + 1e-6)
  
  # Spatial: center vs edge
  center_mask <- r[[1]]
  values(center_mask) <- 0
  cx <- ncol(center_mask) %/% 2
  cy <- nrow(center_mask) %/% 2
  hw <- 10
  center_mask[max(1,cy-hw):min(nrow(center_mask),cy+hw),
              max(1,cx-hw):min(ncol(center_mask),cx+hw)] <- 1
  
  center_b <- global(brightness * center_mask, "sum", na.rm = TRUE)$sum /
    max(global(center_mask, "sum", na.rm = TRUE)$sum, 1e-6)
  edge_b <- global(brightness * (1 - center_mask), "sum", na.rm = TRUE)$sum /
    max(global(1 - center_mask, "sum", na.rm = TRUE)$sum, 1e-6)
  brightness_contrast <- (center_b - edge_b) / (center_b + edge_b + 1e-6)
  
  center_swir <- global(r$B12 * center_mask, "sum", na.rm = TRUE)$sum /
    max(global(center_mask, "sum", na.rm = TRUE)$sum, 1e-6)
  edge_swir <- global(r$B12 * (1 - center_mask), "sum", na.rm = TRUE)$sum /
    max(global(1 - center_mask, "sum", na.rm = TRUE)$sum, 1e-6)
  swir_contrast <- (center_swir - edge_swir) / (center_swir + edge_swir + 1e-6)
  
  b12_variance <- global(r$B12, "sd", na.rm = TRUE)$sd^2
  
  # Collect index statistics
  index_stats <- function(x, name) {
    vals <- global(x, c("mean", "sd", "max", "min"), na.rm = TRUE)
    tibble(
      !!paste0(name, "_mean")  := vals$mean,
      !!paste0(name, "_sd")    := vals$sd,
      !!paste0(name, "_max")   := vals$max,
      !!paste0(name, "_min")   := vals$min,
      !!paste0(name, "_range") := vals$max - vals$min
    )
  }
  
  idx_feats <- bind_cols(
    index_stats(ndvi, "ndvi"),
    index_stats(aerosol_idx, "aerosol"),
    index_stats(swir_smoke, "swir_smoke"),
    index_stats(nbr, "nbr"),
    index_stats(swir_ratio, "swir_ratio"),
    index_stats(brightness, "brightness"),
    index_stats(re_idx1, "re1"),
    index_stats(re_idx2, "re2")
  )
  
  bind_cols(
    tibble(lat = center_lat, lon = center_lon),
    as_tibble(as.list(band_means)),
    as_tibble(as.list(band_sds)),
    as_tibble(as.list(band_maxs)),
    idx_feats,
    tibble(
      brightness_contrast = brightness_contrast,
      swir_contrast       = swir_contrast,
      b12_variance        = b12_variance
    )
  )
}


# â”€â”€ Contextual Features â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

add_contextual_features <- function(df) {
  df %>%
    mutate(
      month        = month(date),
      day_of_year  = yday(date),
      month_sin    = sin(2 * pi * month / 12),
      month_cos    = cos(2 * pi * month / 12),
      lat_abs      = abs(lat)
    )
}


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# PART 4: ASSEMBLE TRAINING DATASET
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

build_training_data <- function(use_cached = TRUE,
                                extract_spectral = TRUE,
                                ucdp_csv_path = UCDP_CSV_FALLBACK_PATH) {
  
  cache_path <- "data/training_features.rds"
  
  if (use_cached && file.exists(cache_path)) {
    message("Loading cached training features...")
    return(readRDS(cache_path))
  }
  
  dir.create("data", showWarnings = FALSE)
  
  message("\n=== BUILDING TRAINING DATA ===\n")
  
  # Pull UCDP events (API first, CSV fallback if needed)
  used_csv_fallback <- FALSE
  ucdp_events <- tryCatch(
    pull_ucdp_events(),
    error = function(e) {
      msg <- conditionMessage(e)
      can_fallback <- !nzchar(UCDP_ACCESS_TOKEN) || grepl("401", msg, fixed = TRUE)
      
      if (can_fallback && file.exists(ucdp_csv_path)) {
        message("  UCDP API unavailable; using local CSV fallback.")
        used_csv_fallback <<- TRUE
        return(load_ucdp_csv(path = ucdp_csv_path))
      }
      
      if (can_fallback) {
        stop(
          paste0(
            "UCDP API unavailable and CSV fallback not found at ",
            ucdp_csv_path, ". Download GED CSV from https://ucdp.uu.se/downloads/."
          ),
          call. = FALSE
        )
      }
      
      stop(msg, call. = FALSE)
    }
  )
  
  # Also try candidate data for most recent events (API only)
  ucdp_candidate <- if (used_csv_fallback) {
    message("  Candidate API pull skipped (CSV fallback mode).")
    tibble()
  } else {
    tryCatch(
      pull_ucdp_candidate(),
      error = function(e) {
        message("  Candidate data unavailable: ", e$message)
        tibble()
      }
    )
  }
  
  all_events <- bind_rows(ucdp_events, ucdp_candidate) %>%
    distinct(event_id, .keep_all = TRUE)
  
  if (nrow(all_events) == 0) {
    stop("No UCDP events available after pull/fallback.", call. = FALSE)
  }
  
  message(glue("\nTotal UCDP events: {nrow(all_events)}"))
  
  # Save raw events
  write_csv(all_events, "data/ucdp_events_raw.csv")
  
  # Filter for airstrike signatures
  positives <- identify_airstrike_events(all_events)
  
  # Limit to manageable training set size
  if (nrow(positives) > 2000) {
    message(glue("  Sampling 2000 positives from {nrow(positives)}"))
    positives <- positives %>%
      slice_sample(n = 2000, weight_by = strike_score)
  }
  
  # Build negatives
  n_neg <- nrow(positives) * NEGATIVE_RATIO
  negatives <- build_negative_samples(n_neg)
  
  # Combine labeled dataset
  labeled <- bind_rows(
    positives %>%
      mutate(date = as.Date(date_start)) %>%
      select(event_id, date, lat, lon, country, label),
    negatives %>%
      select(event_id, date, lat, lon, country, label)
  )
  
  message(glue("\nLabeled dataset: {sum(labeled$label == 1)} positives, ",
               "{sum(labeled$label == 0)} negatives"))
  
  if (!extract_spectral) {
    message("\nSkipping spectral extraction (extract_spectral = FALSE).")
    training_data <- labeled %>%
      add_contextual_features()
    
    saveRDS(training_data, cache_path)
    write_csv(training_data, "data/training_features.csv")
    
    message(glue("\nTraining data saved: {nrow(training_data)} samples, ",
                 "{ncol(training_data)} features (context-only)"))
    return(training_data)
  }
  
  if (!nzchar(SH_CLIENT_ID) || !nzchar(SH_CLIENT_SECRET)) {
    stop(
      paste(
        "Sentinel Hub credentials missing.",
        "Set SH_CLIENT_ID and SH_CLIENT_SECRET,",
        "or run build_training_data(..., extract_spectral = FALSE)."
      ),
      call. = FALSE
    )
  }
  
  # Extract spectral features
  message("\nExtracting spectral features (this will take a while)...\n")
  
  token <- get_sh_token()
  token_time <- Sys.time()
  
  features_list <- vector("list", nrow(labeled))
  
  for (i in seq_len(nrow(labeled))) {
    # Refresh token every 15 minutes
    if (difftime(Sys.time(), token_time, units = "mins") > 15) {
      token <- get_sh_token()
      token_time <- Sys.time()
    }
    
    if (i %% 50 == 0) {
      message(glue("  Processing {i} / {nrow(labeled)}..."))
    }
    
    features_list[[i]] <- tryCatch(
      extract_spectral_features(
        labeled$lat[i], labeled$lon[i], labeled$date[i], token
      ),
      error = function(e) NULL
    )
    
    Sys.sleep(0.5)  # rate limiting
  }
  
  # Combine
  has_features <- !sapply(features_list, is.null)
  message(glue("\n  Features extracted for {sum(has_features)} / {nrow(labeled)} events"))
  
  training_data <- labeled[has_features, ] %>%
    bind_cols(bind_rows(features_list[has_features])) %>%
    add_contextual_features()
  
  # Save
  saveRDS(training_data, cache_path)
  write_csv(training_data, "data/training_features.csv")
  
  message(glue("\nTraining data saved: {nrow(training_data)} samples, ",
               "{ncol(training_data)} features"))
  
  return(training_data)
}
if (sys.nframe() == 0) {
  extract_spectral_default <- nzchar(SH_CLIENT_ID) && nzchar(SH_CLIENT_SECRET)
  if (!extract_spectral_default) {
    message("SH credentials missing; running with extract_spectral = FALSE.")
  }
  
  training_data <- build_training_data(
    use_cached = FALSE,
    extract_spectral = extract_spectral_default
  )
  
  message("\n=== Label distribution ===")
  print(table(training_data$label))
  
  message("\n=== Feature means by class ===")
  training_data %>%
    select(label, starts_with("B0"), starts_with("ndvi"),
           starts_with("aerosol"), starts_with("swir_smoke")) %>%
    group_by(label) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    print()
}

