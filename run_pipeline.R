# =============================================================================
# run_pipeline.R
# Orchestrator — runs the full detection + classification pipeline
# Schedule with cron: 0 */6 * * * Rscript /path/to/run_pipeline.R
#
# Pipeline order:
#   00a → 00b (run ONCE to build & train model)
#   01 → 02 → 03 → 04 (run every 6 hours)
# =============================================================================

library(glue)

setwd(dirname(sys.frame(1)$ofile %||% "."))

# ── Parse arguments ───────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) > 0) args[1] else "detect"
# Modes:
#   "train"    — build training data + train model (run once / periodically)
#   "detect"   — run detection pipeline (run on schedule)
#   "full"     — train then detect
#   "retrain"  — rebuild training data from scratch + retrain

message("╔══════════════════════════════════════════════════════════════╗")
message("║          SMOKE PLUME DETECTION PIPELINE                     ║")
message(glue("║  Mode: {mode}"))
message(glue("║  Time: {format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}"))
message("╚══════════════════════════════════════════════════════════════╝")


# ── Training Mode ──────────────────────────────────────────────────────────────

run_training <- function(use_cached = TRUE) {
  
  message("\n▶ Step 0a: Building training data...")
  source("00a_build_training_data.R", local = TRUE)
  training_data <- build_training_data(use_cached = use_cached)
  
  message("\n▶ Step 0b: Training classifier...")
  source("00b_train_model.R", local = TRUE)
  
  df <- prepare_data(training_data)
  spatial_folds <- create_spatial_folds(training_data)
  models <- train_models(df, spatial_folds)
  eval_results <- evaluate_models(models)
  
  optimal_threshold <- optimize_threshold(
    eval_results$predictions,
    model_col = "xgb_prob",
    target_precision = 0.85
  )
  
  model_diagnostics(models, eval_results)
  model_bundle <- export_model(models, optimal_threshold, eval_results)
  
  message("\n✓ Model training complete.")
  return(model_bundle)
}


# ── Detection Mode ─────────────────────────────────────────────────────────────

run_detection_pipeline <- function() {
  
  tryCatch({
    
    # ── Step 1: Detect thermal anomalies ──
    message("\n▶ Step 1/4: Detecting thermal hotspots...")
    source("01_detect_hotspots.R", local = TRUE)
    events <- run_detection(days_back = 1)
    
    if (nrow(events) == 0) {
      message("\n✓ Pipeline complete — no significant events detected.")
      return(invisible(NULL))
    }
    
    # ── Step 2: Pull satellite imagery ──
    message("\n▶ Step 2/4: Retrieving Sentinel-2 imagery...")
    source("02_pull_imagery.R", local = TRUE)
    events <- pull_imagery_for_events(events)
    
    # ── Step 3: Classify with trained model ──
    message("\n▶ Step 3/4: Running strike classification...")
    source("03_classify_smoke.R", local = TRUE)
    events <- classify_all_events(events)
    
    n_strike <- sum(events$smoke_detected, na.rm = TRUE)
    n_model  <- sum(events$classification_method == "model", na.rm = TRUE)
    n_rule   <- sum(events$classification_method == "rule_based", na.rm = TRUE)
    message(glue("  → {n_strike} strikes detected ",
                 "({n_model} model-based, {n_rule} rule-based fallback)"))
    
    if (n_strike == 0) {
      message("\n✓ Pipeline complete — no strike plumes confirmed.")
      return(invisible(NULL))
    }
    
    # ── Step 4: Generate post assets ──
    message("\n▶ Step 4/4: Generating post images and text...")
    source("04_generate_post.R", local = TRUE)
    posts <- generate_all_posts(events, min_confidence = "medium")
    
    # ── Summary ──
    message("\n╔══════════════════════════════════════════════════════════════╗")
    message(glue("║  PIPELINE COMPLETE                                         ║"))
    message(glue("║  Total hotspots:    {nrow(events)}"))
    message(glue("║  Strikes detected:  {n_strike}"))
    message(glue("║  Posts queued:      {nrow(posts)}"))
    message("║                                                              ║")
    message("║  → Review dashboard:  shiny::runApp('app.R')                 ║")
    message("║  → Publish approved:  source('05_post_to_x.R')               ║")
    message("╚══════════════════════════════════════════════════════════════╝")
    
  }, error = function(e) {
    message("\n✗ PIPELINE ERROR: ", e$message)
    message(traceback())
  })
}


# ── Dispatch ───────────────────────────────────────────────────────────────────

switch(mode,
  "train"   = run_training(use_cached = TRUE),
  "retrain" = run_training(use_cached = FALSE),
  "detect"  = run_detection_pipeline(),
  "full"    = { run_training(use_cached = TRUE); run_detection_pipeline() },
  {
    message("Unknown mode: ", mode)
    message("Usage: Rscript run_pipeline.R [train|retrain|detect|full]")
  }
)
