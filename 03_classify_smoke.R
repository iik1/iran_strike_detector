# =============================================================================
# 03_classify_smoke.R  (v2 — model-based)
# Classify detected events using the trained strike classifier
# Falls back to rule-based scoring if model is unavailable
# =============================================================================

library(terra)
library(tidyverse)
library(tidymodels)
library(sf)
library(glue)

# ── Configuration ──────────────────────────────────────────────────────────────

MODEL_PATH   <- "models/strike_classifier.rds"
ENSEMBLE_PATH <- "models/all_models.rds"
USE_ENSEMBLE  <- TRUE   # average predictions from RF + XGB for robustness

# Rule-based fallback thresholds (same as v1)
SMOKE_THRESHOLDS <- list(
  aod_min          = 0.05,
  swir_smoke_min   = 0.08,
  brightness_min   = 0.08,
  min_plume_area_ha = 5,
  thermal_bright_min = 0.15
)


# ══════════════════════════════════════════════════════════════════════════════
# PART 1: LOAD MODEL
# ══════════════════════════════════════════════════════════════════════════════

load_model <- function() {
  if (!file.exists(MODEL_PATH)) {
    message("⚠ No trained model found at ", MODEL_PATH)
    message("  Falling back to rule-based classification.")
    message("  Run 00a_build_training_data.R and 00b_train_model.R first.")
    return(NULL)
  }
  
  bundle <- readRDS(MODEL_PATH)
  message(glue("Loaded model: {bundle$model_name}"))
  message(glue("  Trained: {bundle$trained_at}"))
  message(glue("  Threshold: {bundle$threshold}"))
  message(glue("  Test PR-AUC: {round(bundle$metrics %>% ",
               "filter(model == bundle$model_name) %>% pull(pr_auc), 3)}"))
  
  return(bundle)
}

load_ensemble <- function() {
  if (!file.exists(ENSEMBLE_PATH)) return(NULL)
  readRDS(ENSEMBLE_PATH)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 2: EXTRACT FEATURES FOR NEW EVENTS
# ══════════════════════════════════════════════════════════════════════════════

extract_features_for_event <- function(event, sh_token = NULL) {
  
  features <- list()
  center_mask <- NULL
  
  # ── 2a. Features from rendered imagery (PNG-based) ──
  if (!is.na(event$true_color_path) && file.exists(event$true_color_path)) {
    tc <- rast(event$true_color_path)
    r <- tc[[1]] / 255
    g <- tc[[2]] / 255
    b <- tc[[3]] / 255
    
    brightness <- (r + g + b) / 3
    color_spread <- max(c(r, g, b)) - min(c(r, g, b))
    
    features$brightness_mean  <- global(brightness, "mean", na.rm = TRUE)$mean
    features$brightness_sd    <- global(brightness, "sd", na.rm = TRUE)$sd
    features$brightness_max   <- global(brightness, "max", na.rm = TRUE)$max
    features$color_spread_mean <- global(color_spread, "mean", na.rm = TRUE)$mean
    features$color_spread_sd  <- global(color_spread, "sd", na.rm = TRUE)$sd
    
    # Center vs edge brightness (hotspot signature)
    center_mask <- brightness
    values(center_mask) <- 0
    cx <- ncol(center_mask) %/% 2
    cy <- nrow(center_mask) %/% 2
    hw <- ncol(center_mask) %/% 8
    center_mask[max(1,cy-hw):min(nrow(center_mask),cy+hw),
                max(1,cx-hw):min(ncol(center_mask),cx+hw)] <- 1
    
    center_b <- global(brightness * center_mask, "sum", na.rm = TRUE)$sum /
      max(global(center_mask, "sum", na.rm = TRUE)$sum, 1e-6)
    edge_b <- global(brightness * (1 - center_mask), "sum", na.rm = TRUE)$sum /
      max(global(1 - center_mask, "sum", na.rm = TRUE)$sum, 1e-6)
    features$brightness_contrast <- (center_b - edge_b) / (center_b + edge_b + 1e-6)
  }
  
  # ── 2b. Features from SWIR composite ──
  if (!is.na(event$swir_path) && file.exists(event$swir_path)) {
    swir <- rast(event$swir_path)
    swir_brightness <- (swir[[1]] + swir[[2]] + swir[[3]]) / (3 * 255)
    
    features$swir_brightness_mean <- global(swir_brightness, "mean", na.rm = TRUE)$mean
    features$swir_brightness_sd   <- global(swir_brightness, "sd", na.rm = TRUE)$sd
    features$swir_brightness_max  <- global(swir_brightness, "max", na.rm = TRUE)$max
    
    # SWIR enhancement relative to visible
    if (!is.null(features$brightness_mean)) {
      features$swir_enhancement <- features$swir_brightness_mean - features$brightness_mean
    }
    
    # SWIR center-edge contrast
    if (!is.null(center_mask)) {
      swir_center <- global(swir_brightness * center_mask, "sum", na.rm = TRUE)$sum /
        max(global(center_mask, "sum", na.rm = TRUE)$sum, 1e-6)
      swir_edge <- global(swir_brightness * (1 - center_mask), "sum", na.rm = TRUE)$sum /
        max(global(1 - center_mask, "sum", na.rm = TRUE)$sum, 1e-6)
      features$swir_contrast <- (swir_center - swir_edge) / (swir_center + swir_edge + 1e-6)
    }
  }
  
  # ── 2c. Features from aerosol enhancement ──
  if (!is.na(event$aerosol_path) && file.exists(event$aerosol_path)) {
    aer <- rast(event$aerosol_path)
    aer_r <- aer[[1]] / 255  # red channel = smoke enhancement
    
    features$aerosol_r_mean <- global(aer_r, "mean", na.rm = TRUE)$mean
    features$aerosol_r_sd   <- global(aer_r, "sd", na.rm = TRUE)$sd
    features$aerosol_r_max  <- global(aer_r, "max", na.rm = TRUE)$max
    
    # Fraction of pixels with strong smoke signal
    smoke_frac <- global(aer_r > 0.4, "mean", na.rm = TRUE)$mean
    features$smoke_pixel_fraction <- smoke_frac
  }
  
  # ── 2d. FIRMS-derived features ──
  features$total_frp      <- event$total_frp %||% NA_real_
  features$max_frp        <- event$max_frp %||% NA_real_
  features$mean_frp       <- event$mean_frp %||% NA_real_
  features$n_detections   <- event$n_detections %||% NA_integer_
  features$log_frp        <- log1p(features$total_frp)
  features$log_n_detect   <- log1p(features$n_detections)
  features$has_nighttime  <- as.integer(event$any_nighttime %||% FALSE)
  features$score          <- event$score %||% NA_real_
  
  # ── 2e. Contextual features ──
  features$lat_abs   <- abs(event$center_lat)
  features$month     <- month(event$first_detected)
  features$month_sin <- sin(2 * pi * features$month / 12)
  features$month_cos <- cos(2 * pi * features$month / 12)
  features$day_of_year <- yday(event$first_detected)
  
  as_tibble(features)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 3: MODEL-BASED CLASSIFICATION
# ══════════════════════════════════════════════════════════════════════════════

classify_with_model <- function(events, model_bundle, ensemble = NULL) {
  
  threshold <- model_bundle$threshold
  
  message(glue("Classifying {nrow(events)} events with trained model ",
               "(threshold = {threshold})..."))
  
  # Extract features for all events
  features_list <- vector("list", nrow(events))
  
  for (i in seq_len(nrow(events))) {
    features_list[[i]] <- tryCatch(
      extract_features_for_event(events[i, ]),
      error = function(e) {
        message(glue("  Feature extraction failed for event {events$cluster_id[i]}"))
        NULL
      }
    )
  }
  
  has_features <- !sapply(features_list, is.null)
  
  if (sum(has_features) == 0) {
    message("  No features extracted — using rule-based fallback for all events")
    return(classify_rule_based_all(events))
  }
  
  features_df <- bind_rows(features_list[has_features])
  
  # ── Predict with primary model ──
  primary_probs <- predict(
    model_bundle$model, features_df, type = "prob"
  )$.pred_strike
  
  # ── Ensemble: average RF + XGB ──
  if (USE_ENSEMBLE && !is.null(ensemble)) {
    rf_probs  <- tryCatch(
      predict(ensemble$rf, features_df, type = "prob")$.pred_strike,
      error = function(e) NULL
    )
    xgb_probs <- tryCatch(
      predict(ensemble$xgb, features_df, type = "prob")$.pred_strike,
      error = function(e) NULL
    )
    
    if (!is.null(rf_probs) && !is.null(xgb_probs)) {
      ensemble_probs <- (rf_probs + xgb_probs) / 2
      
      # Use ensemble when both models agree on direction
      final_probs <- ifelse(
        sign(rf_probs - 0.5) == sign(xgb_probs - 0.5),
        ensemble_probs,
        primary_probs
      )
      message("  Using RF+XGB ensemble predictions")
    } else {
      final_probs <- primary_probs
    }
  } else {
    final_probs <- primary_probs
  }
  
  # ── Apply threshold ──
  predicted_class <- ifelse(final_probs >= threshold, "strike", "non_strike")
  
  # ── Map to confidence levels ──
  confidence <- case_when(
    final_probs >= 0.9              ~ "high",
    final_probs >= threshold        ~ "medium",
    final_probs >= threshold * 0.7  ~ "low",
    TRUE                            ~ "none"
  )
  
  # ── Attach results to events ──
  results <- events
  results$smoke_detected[has_features]        <- predicted_class == "strike"
  results$smoke_confidence[has_features]      <- confidence
  results$model_probability[has_features]     <- round(final_probs, 4)
  results$smoke_fraction[has_features]        <- NA_real_
  results$classification_method[has_features] <- "model"
  
  # ── Fallback for events without features ──
  if (any(!has_features)) {
    message(glue("  {sum(!has_features)} events without features — using rule-based fallback"))
    no_feat_idx <- which(!has_features)
    for (idx in no_feat_idx) {
      fb <- classify_from_png_rules(events[idx, ])
      results$smoke_detected[idx]        <- fb$has_plume
      results$smoke_confidence[idx]      <- fb$confidence
      results$model_probability[idx]     <- NA_real_
      results$smoke_fraction[idx]        <- fb$smoke_fraction %||% NA_real_
      results$classification_method[idx] <- "rule_based"
    }
  }
  
  # Summary
  n_strike <- sum(results$smoke_detected, na.rm = TRUE)
  n_high   <- sum(results$smoke_confidence == "high", na.rm = TRUE)
  message(glue("  Results: {n_strike} strikes detected ({n_high} high confidence)"))
  
  results %>% arrange(desc(model_probability))
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 4: RULE-BASED FALLBACK (from v1)
# ══════════════════════════════════════════════════════════════════════════════

classify_from_png_rules <- function(event) {
  
  true_color_path <- event$true_color_path
  swir_path       <- event$swir_path
  
  if (is.na(true_color_path) || !file.exists(true_color_path)) {
    return(list(has_plume = FALSE, confidence = "none",
                reason = "No imagery available"))
  }
  
  tc <- rast(true_color_path)
  r <- tc[[1]] / 255
  g <- tc[[2]] / 255
  b <- tc[[3]] / 255
  
  brightness_tc <- (r + g + b) / 3
  color_spread  <- max(c(r, g, b)) - min(c(r, g, b))
  
  smoke_candidate <- (brightness_tc > 0.3 & brightness_tc < 0.85 &
                        color_spread < 0.15)
  
  if (!is.na(swir_path) && file.exists(swir_path)) {
    swir <- rast(swir_path)
    swir_brightness <- (swir[[1]] + swir[[2]] + swir[[3]]) / (3 * 255)
    swir_enhancement <- swir_brightness - brightness_tc
    smoke_candidate <- smoke_candidate & (swir_enhancement > 0.05)
  }
  
  smoke_fraction <- global(smoke_candidate, "mean", na.rm = TRUE)$mean
  
  confidence <- case_when(
    smoke_fraction > 0.15 ~ "high",
    smoke_fraction > 0.05 ~ "medium",
    smoke_fraction > 0.01 ~ "low",
    TRUE                  ~ "none"
  )
  
  list(
    has_plume      = smoke_fraction > 0.01,
    smoke_fraction = round(smoke_fraction, 4),
    confidence     = confidence
  )
}

classify_rule_based_all <- function(events) {
  events %>%
    rowwise() %>%
    mutate(
      .fb = list(classify_from_png_rules(cur_data())),
      smoke_detected        = .fb$has_plume,
      smoke_confidence      = .fb$confidence,
      smoke_fraction        = .fb$smoke_fraction %||% NA_real_,
      model_probability     = NA_real_,
      classification_method = "rule_based"
    ) %>%
    ungroup() %>%
    select(-.fb) %>%
    arrange(desc(smoke_confidence), desc(score))
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 5: MAIN ENTRY POINT
# ══════════════════════════════════════════════════════════════════════════════

classify_all_events <- function(events_with_imagery) {
  
  model_bundle <- load_model()
  ensemble     <- if (USE_ENSEMBLE) load_ensemble() else NULL
  
  if (!is.null(model_bundle)) {
    classify_with_model(events_with_imagery, model_bundle, ensemble)
  } else {
    message("Using rule-based classification (no trained model available)")
    classify_rule_based_all(events_with_imagery)
  }
}


# ── Run ───────────────────────────────────────────────────────────────────────

if (sys.nframe() == 0) {
  event_files <- list.files("data", pattern = "events_imagery_.*\\.rds$",
                            full.names = TRUE)
  latest <- sort(event_files, decreasing = TRUE)[1]
  events <- readRDS(latest)
  
  classified <- classify_all_events(events)
  
  classified %>%
    filter(smoke_detected) %>%
    select(cluster_id, location_name, smoke_confidence, model_probability,
           classification_method, total_frp, score) %>%
    print(n = 20)
  
  saveRDS(classified, gsub("events_imagery_", "events_classified_", latest))
}
