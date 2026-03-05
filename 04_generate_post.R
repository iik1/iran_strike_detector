# =============================================================================
# 04_generate_post.R
# Create publication-ready satellite image + map composites for X posts
# =============================================================================

library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(glue)
library(magick)
library(showtext)

# ── Fonts ──────────────────────────────────────────────────────────────────────

font_add_google("Source Sans Pro", "source")
font_add_google("JetBrains Mono", "mono")
showtext_auto()


# ── Configuration ──────────────────────────────────────────────────────────────

# Visual style
STYLE <- list(
  bg          = "#0d1117",
  fg          = "#e6edf3",
  accent      = "#f97316",
  accent2     = "#ef4444",
  grid        = "#21262d",
  land        = "#161b22",
  border      = "#30363d",
  water       = "#0a0e12",
  font_body   = "source",
  font_mono   = "mono"
)

# Output dimensions (optimized for X media — 16:9)
OUTPUT_WIDTH  <- 1920
OUTPUT_HEIGHT <- 1080


# ── 1. Create Locator Map ─────────────────────────────────────────────────────

make_locator_map <- function(lat, lon, location_name, event_date) {
  
  # Load country boundaries
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Define map extent — regional context
  map_bbox <- c(
    xmin = lon - 8, xmax = lon + 8,
    ymin = lat - 5, ymax = lat + 5
  )
  
  event_pt <- st_point(c(lon, lat)) %>%
    st_sfc(crs = 4326) %>%
    st_sf()
  
  p <- ggplot() +
    # Land
    geom_sf(data = world, fill = STYLE$land, color = STYLE$border,
            linewidth = 0.3) +
    # Event point — pulsing ring effect
    geom_sf(data = event_pt, color = STYLE$accent, fill = NA,
            shape = 21, size = 18, stroke = 0.8, alpha = 0.3) +
    geom_sf(data = event_pt, color = STYLE$accent, fill = NA,
            shape = 21, size = 12, stroke = 1.0, alpha = 0.5) +
    geom_sf(data = event_pt, color = STYLE$accent2, fill = STYLE$accent2,
            shape = 21, size = 4, stroke = 1.2) +
    # Coordinates label
    annotate("text",
             x = lon, y = lat - 1.2,
             label = glue("{abs(round(lat, 2))}°{ifelse(lat >= 0, 'N', 'S')}, ",
                          "{abs(round(lon, 2))}°{ifelse(lon >= 0, 'E', 'W')}"),
             color = STYLE$accent, family = STYLE$font_mono, size = 3.5) +
    # Extent
    coord_sf(xlim = c(map_bbox["xmin"], map_bbox["xmax"]),
             ylim = c(map_bbox["ymin"], map_bbox["ymax"])) +
    # Theme
    theme_void() +
    theme(
      plot.background  = element_rect(fill = STYLE$bg, color = NA),
      panel.background = element_rect(fill = STYLE$water, color = NA),
      panel.border     = element_rect(fill = NA, color = STYLE$border,
                                      linewidth = 0.5)
    )
  
  return(p)
}


# ── 2. Annotate Satellite Image ───────────────────────────────────────────────

annotate_satellite_image <- function(image_path, event, output_path) {
  
  if (is.na(image_path) || !file.exists(image_path)) {
    message("No image to annotate for event ", event$cluster_id)
    return(NULL)
  }
  
  img <- image_read(image_path) %>%
    image_resize("1024x1024")
  
  # Add north arrow and scale context as overlay text
  img <- img %>%
    # Dark vignette border
    image_border(color = STYLE$bg, geometry = "4x4") %>%
    # Metadata bar at bottom
    image_annotate(
      text = glue("Sentinel-2 | {format(event$first_detected, '%Y-%m-%d %H:%M UTC')}"),
      gravity = "south",
      location = "+0+8",
      color = STYLE$fg,
      boxcolor = paste0(STYLE$bg, "CC"),
      font = "monospace",
      size = 18
    ) %>%
    # Event label at top
    image_annotate(
      text = glue(" {event$location_name} "),
      gravity = "northwest",
      location = "+8+8",
      color = STYLE$fg,
      boxcolor = paste0(STYLE$accent2, "DD"),
      font = "monospace",
      size = 20,
      weight = 700
    )
  
  # Add thermal anomaly indicator if applicable
  if (!is.null(event$has_thermal) && event$has_thermal) {
    img <- img %>%
      image_annotate(
        text = " THERMAL ANOMALY DETECTED ",
        gravity = "northeast",
        location = "+8+8",
        color = "#FFFFFF",
        boxcolor = "#DC2626DD",
        font = "monospace",
        size = 16,
        weight = 700
      )
  }
  
  image_write(img, output_path, format = "png")
  return(output_path)
}


# ── 3. Compose Final Post Image ───────────────────────────────────────────────

compose_post_image <- function(event, output_dir = "output") {
  
  dir.create(output_dir, showWarnings = FALSE)
  
  # Generate locator map
  map_plot <- make_locator_map(
    event$center_lat, event$center_lon,
    event$location_name, event$first_detected
  )
  
  # Save map to temporary file
  map_path <- file.path(output_dir, glue("map_{event$cluster_id}.png"))
  ggsave(map_path, map_plot, width = 6, height = 5, dpi = 200,
         bg = STYLE$bg)
  
  # Annotate satellite image
  sat_path <- file.path(output_dir, glue("sat_{event$cluster_id}.png"))
  annotate_satellite_image(event$true_color_path, event, sat_path)
  
  if (is.null(sat_path) || !file.exists(sat_path)) {
    message("Cannot compose — missing satellite image for event ", event$cluster_id)
    return(NULL)
  }
  
  # Compose: satellite left (60%), map right (40%)
  sat_img <- image_read(sat_path) %>% image_resize("1152x1080!")
  map_img <- image_read(map_path) %>% image_resize("768x1080!")
  
  # Side by side
  composite <- image_append(c(sat_img, map_img))
  
  # Add header bar
  header <- image_blank(OUTPUT_WIDTH, 64, color = STYLE$bg) %>%
    image_annotate(
      text = glue("SATELLITE DETECTION — SMOKE PLUME — ",
                   "{format(event$first_detected, '%d %b %Y %H:%M UTC')}"),
      gravity = "west",
      location = "+20+0",
      color = STYLE$accent,
      font = "monospace",
      size = 22,
      weight = 700
    )
  
  # Add footer with data attribution
  footer <- image_blank(OUTPUT_WIDTH, 40, color = STYLE$bg) %>%
    image_annotate(
      text = "Data: Copernicus Sentinel-2 / NASA FIRMS | ",
      gravity = "west",
      location = "+20+0",
      color = paste0(STYLE$fg, "88"),
      font = "monospace",
      size = 14
    )
  
  # Stack: header + composite + footer
  final <- image_append(c(header, composite, footer), stack = TRUE)
  
  final_path <- file.path(output_dir, glue("post_{event$cluster_id}.png"))
  image_write(final, final_path, format = "png", quality = 95)
  
  message("Composed post image: ", final_path)
  return(final_path)
}


# ── 4. Generate Post Text ─────────────────────────────────────────────────────

generate_post_text <- function(event) {
  
  confidence_emoji <- switch(event$smoke_confidence,
                             high   = "🔴",
                             medium = "🟡",
                             low    = "🟠",
                             "⚪")
  
  text <- glue(
    "{confidence_emoji} Smoke plume detected — {event$location_name}\n",
    "\n",
    "📅 {format(event$first_detected, '%d %b %Y, %H:%M UTC')}\n",
    "📍 {round(event$center_lat, 3)}°N, {round(event$center_lon, 3)}°E\n",
    "🔥 {event$n_detections} thermal anomalies | FRP: {round(event$total_frp, 1)} MW\n",
    "📡 Confidence: {str_to_upper(event$smoke_confidence)}\n",
    "\n",
    "Sentinel-2 imagery + VIIRS/FIRMS thermal data.\n",
    "Automated detection — context may vary.\n",
    "\n",
    "#OSINT #SatelliteImagery #RemoteSensing"
  )
  
  # X character limit: 280 (or 25,000 for X Premium)
  if (nchar(text) > 280) {
    # Compact version
    text <- glue(
      "{confidence_emoji} Smoke plume — {event$location_name}\n",
      "{format(event$first_detected, '%d %b %Y %H:%M UTC')}\n",
      "{round(event$center_lat, 3)}°N, {round(event$center_lon, 3)}°E\n",
      "{event$n_detections} thermal hits | FRP {round(event$total_frp, 1)} MW\n",
      "Conf: {str_to_upper(event$smoke_confidence)}\n",
      "#OSINT #SatelliteImagery"
    )
  }
  
  return(text)
}


# ── 5. Batch Generate All Posts ────────────────────────────────────────────────

generate_all_posts <- function(classified_events, min_confidence = "medium") {
  
  # Only generate for events meeting confidence threshold
  conf_levels <- c("none" = 0, "low" = 1, "medium" = 2, "high" = 3)
  
  postable <- classified_events %>%
    filter(smoke_detected) %>%
    filter(conf_levels[smoke_confidence] >= conf_levels[min_confidence])
  
  if (nrow(postable) == 0) {
    message("No events meet the confidence threshold for posting.")
    return(tibble())
  }
  
  message(glue("Generating posts for {nrow(postable)} events..."))
  
  posts <- postable %>%
    rowwise() %>%
    mutate(
      post_image_path = compose_post_image(cur_data(), "output"),
      post_text       = generate_post_text(cur_data()),
      status          = "pending_review"  # always starts as pending
    ) %>%
    ungroup()
  
  # Save post queue
  saveRDS(posts, "data/post_queue.rds")
  message("Post queue saved: data/post_queue.rds")
  
  return(posts)
}


# ── Run ───────────────────────────────────────────────────────────────────────

if (sys.nframe() == 0) {
  event_files <- list.files("data", pattern = "events_classified_.*\\.rds$",
                            full.names = TRUE)
  latest <- sort(event_files, decreasing = TRUE)[1]
  events <- readRDS(latest)
  
  posts <- generate_all_posts(events, min_confidence = "medium")
  
  posts %>%
    select(cluster_id, location_name, smoke_confidence, status) %>%
    print()
}
