# =============================================================================
# 01_detect_hotspots.R
# Pull near-real-time thermal anomalies from NASA FIRMS
# Filter to Middle East conflict zones and flag potential strike signatures
# =============================================================================

library(httr2)
library(tidyverse)
library(sf)
library(lubridate)
library(jsonlite)

# ── Configuration ──────────────────────────────────────────────────────────────

# Your NASA FIRMS MAP_KEY (get one at https://firms.modaps.eosdis.nasa.gov/api/)
FIRMS_API_KEY <- Sys.getenv("FIRMS_API_KEY")  # set in .Renviron

# Define region of interest: bounding box covering key conflict areas
# Adjust these coordinates to your specific area of interest
ROI <- list(
  west  = 34.0,   # Eastern Mediterranean
  south = 28.0,   # Northern edge of Sinai / southern border areas
  east  = 48.0,   # Eastern Iraq / Western Iran border
  north = 37.5    # Northern Iraq / Syria / Turkey border region
)

# Detection parameters
CONFIDENCE_THRESHOLD <- 70    # VIIRS confidence score (0-100)
FRP_THRESHOLD        <- 10    # Fire Radiative Power in MW — filters out small agricultural fires
CLUSTER_DISTANCE_KM  <- 2     # Group nearby detections into single events
LOOKBACK_HOURS       <- 24    # How far back to query


# ── 1. Query FIRMS API ────────────────────────────────────────────────────────

pull_firms_data <- function(source = "VIIRS_SNPP_NRT",
                            days_back = 1) {
  # FIRMS API: area query

  # Sources: VIIRS_SNPP_NRT, VIIRS_NOAA20_NRT, MODIS_NRT
  
  bbox_str <- paste(ROI$west, ROI$south, ROI$east, ROI$north, sep = ",")
  
  url <- glue::glue(
    "https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
    "{FIRMS_API_KEY}/{source}/{bbox_str}/{days_back}"
  )
  
  message("Querying FIRMS: ", source, " | last ", days_back, " day(s)")
  
  resp <- request(url) |>
    req_timeout(60) |>
    req_perform()
  
  if (resp_status(resp) != 200) {
    stop("FIRMS API returned status ", resp_status(resp))
  }
  
  raw_text <- resp_body_string(resp)
  
  # Parse CSV response
  df <- read_csv(raw_text, show_col_types = FALSE)
  
  message("  → Retrieved ", nrow(df), " detections")
  return(df)
}


# ── 2. Filter for Strike-Like Signatures ──────────────────────────────────────

filter_hotspots <- function(df) {
  # Strike-related fires tend to have:
  #   - High confidence scores
  #   - Elevated FRP (more intense than crop burns)
  #   - Occur in non-agricultural, non-industrial areas (contextual)
  #   - Nighttime detections are especially informative
  
  filtered <- df %>%
    filter(
      confidence >= CONFIDENCE_THRESHOLD | confidence == "high",
      frp >= FRP_THRESHOLD
    ) %>%
    mutate(
      acq_datetime = ymd_hm(paste(acq_date, acq_time)),
      is_nighttime = hour(acq_datetime) < 6 | hour(acq_datetime) > 18,
      # Basic urban proximity flag — refine with actual settlement data
      lat_round = round(latitude, 2),
      lon_round = round(longitude, 2)
    )
  
  message("  → ", nrow(filtered), " detections pass confidence/FRP thresholds")
  return(filtered)
}


# ── 3. Cluster Nearby Detections ──────────────────────────────────────────────

cluster_detections <- function(df) {
  if (nrow(df) == 0) return(tibble())
  
  # Convert to spatial
  pts <- df %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Cluster points within CLUSTER_DISTANCE_KM of each other
  dist_matrix <- st_distance(pts) %>% units::set_units("km")
  clusters <- cutree(hclust(as.dist(dist_matrix), method = "complete"),
                     h = CLUSTER_DISTANCE_KM)
  
  df$cluster_id <- clusters
  
  # Summarize each cluster
  events <- df %>%
    group_by(cluster_id) %>%
    summarise(
      n_detections   = n(),
      center_lat     = mean(latitude),
      center_lon     = mean(longitude),
      max_frp        = max(frp),
      mean_frp       = mean(frp),
      total_frp      = sum(frp),
      max_confidence = max(confidence),
      first_detected = min(acq_datetime),
      last_detected  = max(acq_datetime),
      any_nighttime  = any(is_nighttime),
      .groups = "drop"
    ) %>%
    # Score events: higher score = more likely to be significant
    mutate(
      score = (total_frp / 100) +
        (n_detections * 2) +
        (any_nighttime * 5) +
        (max_confidence / 20)
    ) %>%
    arrange(desc(score))
  
  message("  → ", nrow(events), " clustered events")
  return(events)
}


# ── 4. Contextual Filtering ───────────────────────────────────────────────────

# Known persistent heat sources to exclude (oil/gas facilities, industrial sites)
# In production, load this from a maintained database
known_industrial <- tribble(
  ~name,                    ~lat,    ~lon,    ~radius_km,
  "Baiji Refinery",         34.93,   43.49,   3,
  "Kirkuk Oil Fields",      35.47,   44.39,   10,
  "Rumaila Oil Field",      30.55,   47.32,   15,
  "Basra Refinery",         30.52,   47.85,   5
  # Add more known persistent sources as you encounter them
)

exclude_known_sources <- function(events) {
  if (nrow(events) == 0) return(events)
  
  events_sf <- events %>%
    st_as_sf(coords = c("center_lon", "center_lat"), crs = 4326)
  
  industrial_sf <- known_industrial %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Flag events near known industrial sources
  events$near_industrial <- FALSE
  
  for (i in seq_len(nrow(industrial_sf))) {
    buffer <- st_buffer(
      st_transform(industrial_sf[i, ], 3857),
      dist = known_industrial$radius_km[i] * 1000
    ) %>% st_transform(4326)
    
    hits <- st_intersects(events_sf, buffer, sparse = FALSE)[, 1]
    events$near_industrial[hits] <- TRUE
  }
  
  excluded <- sum(events$near_industrial)
  if (excluded > 0) message("  → Excluded ", excluded, " events near known industrial sites")
  
  events %>% filter(!near_industrial) %>% select(-near_industrial)
}


# ── 5. Reverse Geocode Events ─────────────────────────────────────────────────

reverse_geocode <- function(events) {
  # Use Nominatim (free, rate-limited)
  events %>%
    rowwise() %>%
    mutate(
      location_name = tryCatch({
        Sys.sleep(1)  # respect rate limits
        resp <- request(glue::glue(
          "https://nominatim.openstreetmap.org/reverse?",
          "lat={center_lat}&lon={center_lon}&format=json&zoom=10"
        )) |>
          req_headers("User-Agent" = "smoke-plume-monitor/1.0") |>
          req_perform()
        
        data <- resp_body_json(resp)
        paste0(
          data$address$city %||% data$address$town %||% data$address$county %||% "Unknown",
          ", ",
          data$address$country %||% "Unknown"
        )
      }, error = function(e) "Unknown location")
    ) %>%
    ungroup()
}


# ── 6. Main Detection Pipeline ────────────────────────────────────────────────

run_detection <- function(days_back = 1) {
  message("=== SMOKE PLUME DETECTION PIPELINE ===")
  message("Time: ", Sys.time())
  message("")
  
  # Pull from both VIIRS sensors for better coverage
  viirs_snpp  <- pull_firms_data("VIIRS_SNPP_NRT", days_back)
  viirs_noaa  <- pull_firms_data("VIIRS_NOAA20_NRT", days_back)
  
  all_detections <- bind_rows(viirs_snpp, viirs_noaa) %>%
    distinct(latitude, longitude, acq_date, acq_time, .keep_all = TRUE)
  
  message("\nTotal unique detections: ", nrow(all_detections))
  
  # Filter, cluster, exclude known sources
  events <- all_detections %>%
    filter_hotspots() %>%
    cluster_detections() %>%
    exclude_known_sources()
  
  if (nrow(events) == 0) {
    message("\n✓ No significant events detected.")
    return(tibble())
  }
  
  # Geocode top events
  events <- events %>%
    head(20) %>%  # only geocode the top 20
    reverse_geocode()
  
  message("\n=== TOP EVENTS ===")
  events %>%
    head(10) %>%
    select(cluster_id, location_name, n_detections, total_frp, score) %>%
    print()
  
  # Save to disk for the next pipeline stage
  output_path <- glue::glue("data/events_{format(Sys.time(), '%Y%m%d_%H%M')}.rds")
  dir.create("data", showWarnings = FALSE)
  saveRDS(events, output_path)
  message("\nSaved to: ", output_path)
  
  return(events)
}


# ── Run ───────────────────────────────────────────────────────────────────────

if (sys.nframe() == 0) {
  events <- run_detection(days_back = 1)
}
