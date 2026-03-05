# =============================================================================
# 02_pull_imagery.R
# Fetch Sentinel-2 imagery for flagged thermal anomaly events
# Uses Copernicus Data Space Ecosystem (CDSE) API
# =============================================================================

library(httr2)
library(tidyverse)
library(sf)
library(terra)
library(jsonlite)
library(lubridate)
library(glue)

# ── Configuration ──────────────────────────────────────────────────────────────

# Copernicus Data Space credentials
# Register at https://dataspace.copernicus.eu/
CDSE_CLIENT_ID     <- Sys.getenv("CDSE_CLIENT_ID")
CDSE_CLIENT_SECRET <- Sys.getenv("CDSE_CLIENT_SECRET")

# Sentinel Hub credentials (for Processing API — rendering imagery)
SH_CLIENT_ID     <- Sys.getenv("SH_CLIENT_ID")
SH_CLIENT_SECRET <- Sys.getenv("SH_CLIENT_SECRET")

# Image parameters
BBOX_BUFFER_KM     <- 15     # km around event center for image extent
MAX_CLOUD_COVER    <- 40     # % — accept partly cloudy scenes (smoke isn't cloud)
SEARCH_WINDOW_DAYS <- 5      # days before/after event to search for imagery


# ── 1. Authenticate with Copernicus ───────────────────────────────────────────

get_cdse_token <- function() {
  resp <- request("https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token") |>
    req_body_form(
      grant_type    = "client_credentials",
      client_id     = CDSE_CLIENT_ID,
      client_secret = CDSE_CLIENT_SECRET
    ) |>
    req_perform()
  
  resp_body_json(resp)$access_token
}

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


# ── 2. Search for Available Sentinel-2 Scenes ─────────────────────────────────

search_sentinel2 <- function(lat, lon, event_date, token) {
  
  # Build bounding box
  pt <- st_point(c(lon, lat)) %>% st_sfc(crs = 4326)
  bbox <- pt %>%
    st_transform(3857) %>%
    st_buffer(BBOX_BUFFER_KM * 1000) %>%
    st_transform(4326) %>%
    st_bbox()
  
  # Time window
  date_from <- format(event_date - days(SEARCH_WINDOW_DAYS), "%Y-%m-%dT00:00:00Z")
  date_to   <- format(event_date + days(SEARCH_WINDOW_DAYS), "%Y-%m-%dT23:59:59Z")
  
  # OData query to CDSE catalog
  filter_str <- glue(
    "Collection/Name eq 'SENTINEL-2' and ",
    "Attributes/OData.CSC.DoubleAttribute/any(att:att/Name eq 'cloudCover' and att/OData.CSC.DoubleAttribute/Value le {MAX_CLOUD_COVER}) and ",
    "OData.CSC.Intersects(area=geography'SRID=4326;POLYGON((",
    "{bbox['xmin']} {bbox['ymin']},",
    "{bbox['xmax']} {bbox['ymin']},",
    "{bbox['xmax']} {bbox['ymax']},",
    "{bbox['xmin']} {bbox['ymax']},",
    "{bbox['xmin']} {bbox['ymin']}))')",
    " and ContentDate/Start ge {date_from} and ContentDate/Start le {date_to}"
  )
  
  resp <- request("https://catalogue.dataspace.copernicus.eu/odata/v1/Products") |>
    req_url_query(`$filter` = filter_str, `$top` = 10, `$orderby` = "ContentDate/Start desc") |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_perform()
  
  results <- resp_body_json(resp)$value
  
  if (length(results) == 0) {
    message("  No Sentinel-2 scenes found for event at ", lat, ", ", lon)
    return(NULL)
  }
  
  # Return metadata for best scenes
  tibble(
    product_id   = map_chr(results, "Id"),
    name         = map_chr(results, "Name"),
    date         = map_chr(results, ~ .x$ContentDate$Start) %>% ymd_hms(),
    cloud_cover  = map_dbl(results, ~ {
      attrs <- .x$Attributes
      cc <- keep(attrs, ~ .x$Name == "cloudCover")
      if (length(cc) > 0) cc[[1]]$Value else NA_real_
    })
  ) %>%
    arrange(cloud_cover, desc(date))
}


# ── 3. Render Imagery via Sentinel Hub Processing API ──────────────────────────

# Evalscript: True Color + SWIR composite for smoke visualization
EVALSCRIPT_TRUE_COLOR <- '
//VERSION=3
function setup() {
  return {
    input: [{ bands: ["B02", "B03", "B04"], units: "DN" }],
    output: { bands: 3, sampleType: "AUTO" }
  };
}

function evaluatePixel(sample) {
  return [
    sample.B04 * 3.5 / 10000,
    sample.B03 * 3.5 / 10000,
    sample.B02 * 3.5 / 10000
  ];
}
'

# SWIR composite (B12, B8A, B04) — smoke appears distinctly in SWIR
EVALSCRIPT_SMOKE <- '
//VERSION=3
function setup() {
  return {
    input: [{ bands: ["B04", "B8A", "B12"], units: "DN" }],
    output: { bands: 3, sampleType: "AUTO" }
  };
}

function evaluatePixel(sample) {
  return [
    sample.B12 * 2.5 / 10000,
    sample.B8A * 2.5 / 10000,
    sample.B04 * 2.5 / 10000
  ];
}
'

# Aerosol Index custom script — highlights smoke specifically
EVALSCRIPT_AEROSOL <- '
//VERSION=3
function setup() {
  return {
    input: [{ bands: ["B02", "B04", "B12", "B8A"], units: "DN" }],
    output: { bands: 3, sampleType: "AUTO" }
  };
}

function evaluatePixel(sample) {
  // Approximate aerosol/smoke enhancement
  let smoke_idx = (sample.B12 - sample.B8A) / (sample.B12 + sample.B8A);
  let brightness = (sample.B04 + sample.B02) / 20000;

  if (smoke_idx > 0.1 && brightness > 0.1) {
    // Highlight smoke in red-orange
    return [brightness + smoke_idx * 0.5, brightness * 0.6, brightness * 0.3];
  }
  // Normal terrain in muted tones
  return [
    sample.B04 * 2.0 / 10000,
    sample.B04 * 1.8 / 10000,
    sample.B02 * 2.2 / 10000
  ];
}
'

render_image <- function(lat, lon, event_date, evalscript, token,
                         width = 1024, height = 1024) {
  
  # Build bounding box
  pt <- st_point(c(lon, lat)) %>% st_sfc(crs = 4326)
  bbox <- pt %>%
    st_transform(3857) %>%
    st_buffer(BBOX_BUFFER_KM * 1000) %>%
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
      data = list(
        list(
          type = "sentinel-2-l2a",
          dataFilter = list(
            timeRange = list(from = date_from, to = date_to),
            maxCloudCoverage = MAX_CLOUD_COVER
          )
        )
      )
    ),
    output = list(
      width = width,
      height = height,
      responses = list(
        list(identifier = "default", format = list(type = "image/png"))
      )
    ),
    evalscript = evalscript
  )
  
  resp <- request("https://services.sentinel-hub.com/api/v1/process") |>
    req_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json",
      Accept         = "image/png"
    ) |>
    req_body_json(body) |>
    req_perform()
  
  if (resp_status(resp) == 200) {
    return(resp_body_raw(resp))
  } else {
    warning("Sentinel Hub returned status ", resp_status(resp))
    return(NULL)
  }
}


# ── 4. Process All Flagged Events ──────────────────────────────────────────────

pull_imagery_for_events <- function(events) {
  dir.create("imagery", showWarnings = FALSE)
  
  sh_token <- get_sh_token()
  
  results <- events %>%
    rowwise() %>%
    mutate(
      event_date = as.Date(first_detected),
      # Pull three composites per event
      true_color_path = {
        path <- glue("imagery/event_{cluster_id}_truecolor.png")
        img <- render_image(center_lat, center_lon, event_date,
                            EVALSCRIPT_TRUE_COLOR, sh_token)
        if (!is.null(img)) writeBin(img, path)
        if (file.exists(path)) path else NA_character_
      },
      swir_path = {
        path <- glue("imagery/event_{cluster_id}_swir.png")
        img <- render_image(center_lat, center_lon, event_date,
                            EVALSCRIPT_SMOKE, sh_token)
        if (!is.null(img)) writeBin(img, path)
        if (file.exists(path)) path else NA_character_
      },
      aerosol_path = {
        path <- glue("imagery/event_{cluster_id}_aerosol.png")
        img <- render_image(center_lat, center_lon, event_date,
                            EVALSCRIPT_AEROSOL, sh_token)
        if (!is.null(img)) writeBin(img, path)
        if (file.exists(path)) path else NA_character_
      }
    ) %>%
    ungroup()
  
  n_with_imagery <- sum(!is.na(results$true_color_path))
  message("Retrieved imagery for ", n_with_imagery, " / ", nrow(events), " events")
  
  return(results)
}


# ── 5. Pre/Post Comparison ────────────────────────────────────────────────────

# Pull a "before" image (1-3 days before event) for visual comparison
pull_before_image <- function(lat, lon, event_date, sh_token) {
  before_date <- event_date - days(3)
  render_image(lat, lon, before_date, EVALSCRIPT_TRUE_COLOR, sh_token)
}


# ── Run ───────────────────────────────────────────────────────────────────────

if (sys.nframe() == 0) {
  # Load events from detection stage
  event_files <- list.files("data", pattern = "events_.*\\.rds$", full.names = TRUE)
  latest <- sort(event_files, decreasing = TRUE)[1]
  events <- readRDS(latest)
  
  events_with_imagery <- pull_imagery_for_events(events)
  saveRDS(events_with_imagery, gsub("events_", "events_imagery_", latest))
}
