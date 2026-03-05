# =============================================================================
# 00b_train_model.R
# Train a supervised classifier to distinguish airstrike smoke from
# other thermal anomalies (gas flares, ag burns, industrial fires)
#
# Models: Random Forest + XGBoost, with spatial cross-validation
# =============================================================================

library(tidyverse)
library(tidymodels)
library(xgboost)
library(ranger)
library(vip)
library(probably)     # calibration
library(sf)
library(glue)
library(patchwork)

set.seed(42)

# ── Configuration ──────────────────────────────────────────────────────────────

# Model hyperparameter search space
N_CV_FOLDS        <- 5
N_TUNE_GRID       <- 30
POSITIVE_WEIGHT   <- 2.0      # up-weight strikes (class imbalance)
PROBABILITY_THRESH <- 0.5     # default; will be tuned on validation set
MIN_FEATURES_IMPORTANCE <- 0.01  # drop features below this importance


# ══════════════════════════════════════════════════════════════════════════════
# PART 1: DATA PREPARATION
# ══════════════════════════════════════════════════════════════════════════════

prepare_data <- function(training_data) {
  
  message("Preparing data for modeling...")
  
  # ── Select feature columns ──
  # Drop identifiers, coordinates used for features (keep lat_abs), raw lat/lon
  id_cols <- c("event_id", "date", "lat", "lon", "country", "location",
               "fatalities", "source", "notes", "type", "admin1",
               "dist_to_nearest_city_km", "dist_to_nearest_road_km")
  
  df <- training_data %>%
    select(-any_of(id_cols)) %>%
    mutate(label = factor(label, levels = c(0, 1),
                          labels = c("non_strike", "strike")))
  
  # ── Handle missing values ──
  # Drop columns with >50% missing
  missing_pct <- colMeans(is.na(df))
  drop_cols <- names(missing_pct[missing_pct > 0.5])
  if (length(drop_cols) > 0) {
    message(glue("  Dropping {length(drop_cols)} columns with >50% missing"))
    df <- df %>% select(-all_of(drop_cols))
  }
  
  # Impute remaining NAs with median
  numeric_cols <- df %>% select(where(is.numeric)) %>% names()
  df <- df %>%
    mutate(across(all_of(numeric_cols), ~ replace_na(.x, median(.x, na.rm = TRUE))))
  
  # ── Remove near-zero variance features ──
  nzv <- df %>%
    select(where(is.numeric)) %>%
    summarise(across(everything(), ~ sd(.x, na.rm = TRUE))) %>%
    pivot_longer(everything()) %>%
    filter(value < 1e-8) %>%
    pull(name)
  
  if (length(nzv) > 0) {
    message(glue("  Removing {length(nzv)} near-zero variance features"))
    df <- df %>% select(-all_of(nzv))
  }
  
  # ── Check correlation ──
  cor_matrix <- df %>%
    select(where(is.numeric)) %>%
    cor(use = "pairwise.complete.obs")
  
  # Flag highly correlated pairs (>0.95)
  high_cor <- which(abs(cor_matrix) > 0.95 & upper.tri(cor_matrix), arr.ind = TRUE)
  if (nrow(high_cor) > 0) {
    drop_corr <- unique(colnames(cor_matrix)[high_cor[, 2]])
    # Keep at most one from each correlated pair
    # For now, just warn — let the model handle it
    message(glue("  Warning: {length(drop_corr)} features highly correlated (>0.95)"))
  }
  
  n_features <- ncol(df) - 1  # minus label
  message(glue("  Final: {nrow(df)} samples × {n_features} features"))
  message(glue("  Class balance: {sum(df$label == 'strike')} strikes, ",
               "{sum(df$label == 'non_strike')} non-strikes"))
  
  return(df)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 2: SPATIAL CROSS-VALIDATION
# ══════════════════════════════════════════════════════════════════════════════

# Standard k-fold CV can leak information when nearby events share spectral
# signatures. Spatial CV groups nearby events into the same fold.

create_spatial_folds <- function(training_data, n_folds = N_CV_FOLDS) {
  
  message("Creating spatial cross-validation folds...")
  
  # Cluster events by location using k-means on coordinates
  coords <- training_data %>%
    select(lat, lon) %>%
    as.matrix()
  
  # Use more clusters than folds, then assign clusters to folds
  n_clusters <- n_folds * 4
  km <- kmeans(coords, centers = min(n_clusters, nrow(coords) - 1), nstart = 10)
  
  # Assign clusters to folds (balanced)
  cluster_sizes <- table(km$cluster)
  fold_assignment <- rep(NA, max(km$cluster))
  
  # Greedy assignment: add largest remaining cluster to smallest fold
  fold_sizes <- rep(0, n_folds)
  for (cl in as.integer(names(sort(cluster_sizes, decreasing = TRUE)))) {
    smallest_fold <- which.min(fold_sizes)
    fold_assignment[cl] <- smallest_fold
    fold_sizes[smallest_fold] <- fold_sizes[smallest_fold] + cluster_sizes[as.character(cl)]
  }
  
  spatial_fold <- fold_assignment[km$cluster]
  
  message(glue("  Fold sizes: {paste(table(spatial_fold), collapse = ', ')}"))
  
  return(spatial_fold)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 3: MODEL TRAINING
# ══════════════════════════════════════════════════════════════════════════════

train_models <- function(df, spatial_folds) {
  
  # ── Train/test split (keeping spatial structure) ──
  # Hold out fold 1 as final test set
  test_idx  <- which(spatial_folds == 1)
  train_idx <- which(spatial_folds != 1)
  
  train_df <- df[train_idx, ]
  test_df  <- df[test_idx, ]
  
  message(glue("\nTrain: {nrow(train_df)} | Test: {nrow(test_df)}"))
  
  # ── Spatial CV folds for training set ──
  train_folds_vec <- spatial_folds[train_idx]
  # Remap fold numbers to 1:k
  unique_folds <- sort(unique(train_folds_vec))
  train_folds_vec <- match(train_folds_vec, unique_folds)
  
  cv_splits <- manual_rset(
    lapply(sort(unique(train_folds_vec)), function(f) {
      make_splits(
        list(analysis = which(train_folds_vec != f),
             assessment = which(train_folds_vec == f)),
        data = train_df
      )
    }),
    ids = paste0("Fold", sort(unique(train_folds_vec)))
  )
  
  # ── Preprocessing recipe ──
  base_recipe <- recipe(label ~ ., data = train_df) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_zv(all_predictors())
  
  # ── Model 1: Random Forest ──
  message("\n── Training Random Forest ──")
  
  rf_spec <- rand_forest(
    mtry  = tune(),
    trees = 1000,
    min_n = tune()
  ) %>%
    set_engine("ranger",
               importance    = "impurity",
               class.weights = c(non_strike = 1, strike = POSITIVE_WEIGHT),
               seed          = 42) %>%
    set_mode("classification")
  
  rf_workflow <- workflow() %>%
    add_recipe(base_recipe) %>%
    add_model(rf_spec)
  
  rf_grid <- grid_latin_hypercube(
    mtry(range = c(5, min(40, ncol(train_df) - 1))),
    min_n(range = c(2, 20)),
    size = N_TUNE_GRID
  )
  
  rf_tuned <- tune_grid(
    rf_workflow,
    resamples = cv_splits,
    grid      = rf_grid,
    metrics   = metric_set(roc_auc, pr_auc, f_meas, accuracy),
    control   = control_grid(verbose = TRUE, save_pred = TRUE)
  )
  
  rf_best <- select_best(rf_tuned, metric = "roc_auc")
  message(glue("  Best RF — mtry: {rf_best$mtry}, min_n: {rf_best$min_n}"))
  
  rf_final <- rf_workflow %>%
    finalize_workflow(rf_best) %>%
    fit(train_df)
  
  # ── Model 2: XGBoost ──
  message("\n── Training XGBoost ──")
  
  xgb_spec <- boost_tree(
    trees          = 1000,
    tree_depth     = tune(),
    learn_rate     = tune(),
    min_n          = tune(),
    loss_reduction = tune(),
    sample_size    = tune(),
    mtry           = tune()
  ) %>%
    set_engine("xgboost",
               scale_pos_weight = POSITIVE_WEIGHT,
               eval_metric      = "aucpr",
               early_stop       = 50) %>%
    set_mode("classification")
  
  xgb_workflow <- workflow() %>%
    add_recipe(base_recipe) %>%
    add_model(xgb_spec)
  
  xgb_grid <- grid_latin_hypercube(
    tree_depth(range = c(3, 10)),
    learn_rate(range = c(-3, -1)),        # 0.001 to 0.1
    min_n(range = c(5, 30)),
    loss_reduction(range = c(-2, 2)),
    sample_size = sample_prop(range = c(0.5, 0.9)),
    mtry(range = c(5, min(40, ncol(train_df) - 1))),
    size = N_TUNE_GRID
  )
  
  xgb_tuned <- tune_grid(
    xgb_workflow,
    resamples = cv_splits,
    grid      = xgb_grid,
    metrics   = metric_set(roc_auc, pr_auc, f_meas, accuracy),
    control   = control_grid(verbose = TRUE, save_pred = TRUE)
  )
  
  xgb_best <- select_best(xgb_tuned, metric = "roc_auc")
  message(glue("  Best XGB — depth: {xgb_best$tree_depth}, ",
               "lr: {round(xgb_best$learn_rate, 4)}"))
  
  xgb_final <- xgb_workflow %>%
    finalize_workflow(xgb_best) %>%
    fit(train_df)
  
  # ── Model 3: Logistic Regression (baseline) ──
  message("\n── Training Logistic Regression (baseline) ──")
  
  lr_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("classification")
  
  lr_workflow <- workflow() %>%
    add_recipe(base_recipe) %>%
    add_model(lr_spec)
  
  lr_grid <- grid_regular(penalty(range = c(-4, 0)), levels = 20)
  
  lr_tuned <- tune_grid(
    lr_workflow,
    resamples = cv_splits,
    grid      = lr_grid,
    metrics   = metric_set(roc_auc, pr_auc, f_meas),
    control   = control_grid(save_pred = TRUE)
  )
  
  lr_best <- select_best(lr_tuned, metric = "roc_auc")
  lr_final <- lr_workflow %>%
    finalize_workflow(lr_best) %>%
    fit(train_df)
  
  return(list(
    rf  = rf_final,
    xgb = xgb_final,
    lr  = lr_final,
    rf_tuned  = rf_tuned,
    xgb_tuned = xgb_tuned,
    lr_tuned  = lr_tuned,
    train_df  = train_df,
    test_df   = test_df
  ))
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 4: EVALUATION
# ══════════════════════════════════════════════════════════════════════════════

evaluate_models <- function(models) {
  
  test_df <- models$test_df
  
  message("\n=== MODEL EVALUATION ON HELD-OUT SPATIAL FOLD ===\n")
  
  # ── Predictions ──
  preds <- tibble(
    truth = test_df$label,
    
    rf_class  = predict(models$rf, test_df)$.pred_class,
    rf_prob   = predict(models$rf, test_df, type = "prob")$.pred_strike,
    
    xgb_class = predict(models$xgb, test_df)$.pred_class,
    xgb_prob  = predict(models$xgb, test_df, type = "prob")$.pred_strike,
    
    lr_class  = predict(models$lr, test_df)$.pred_class,
    lr_prob   = predict(models$lr, test_df, type = "prob")$.pred_strike
  )
  
  # ── Metrics ──
  evaluate_one <- function(truth, predicted_class, predicted_prob, name) {
    tibble(
      model    = name,
      accuracy = accuracy_vec(truth, predicted_class),
      f1       = f_meas_vec(truth, predicted_class, event_level = "second"),
      precision = precision_vec(truth, predicted_class, event_level = "second"),
      recall   = recall_vec(truth, predicted_class, event_level = "second"),
      roc_auc  = roc_auc_vec(truth, predicted_prob, event_level = "second"),
      pr_auc   = pr_auc_vec(truth, predicted_prob, event_level = "second")
    )
  }
  
  metrics <- bind_rows(
    evaluate_one(preds$truth, preds$rf_class, preds$rf_prob, "Random Forest"),
    evaluate_one(preds$truth, preds$xgb_class, preds$xgb_prob, "XGBoost"),
    evaluate_one(preds$truth, preds$lr_class, preds$lr_prob, "Logistic Reg.")
  )
  
  message("── Test Set Performance ──")
  print(metrics %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
  
  # ── Confusion Matrices ──
  message("\n── Confusion Matrices ──")
  for (m in c("rf", "xgb", "lr")) {
    message(glue("\n{m}:"))
    print(table(
      Predicted = preds[[paste0(m, "_class")]],
      Actual    = preds$truth
    ))
  }
  
  return(list(predictions = preds, metrics = metrics))
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 5: THRESHOLD OPTIMIZATION
# ══════════════════════════════════════════════════════════════════════════════

optimize_threshold <- function(preds, model_col = "xgb_prob",
                                target_precision = 0.85) {
  # For conflict monitoring, we want HIGH PRECISION (few false positives)
  # even at the cost of missing some real events
  
  message(glue("\n── Optimizing threshold for target precision ≥ {target_precision} ──"))
  
  thresholds <- seq(0.1, 0.95, by = 0.01)
  
  results <- map_dfr(thresholds, function(t) {
    pred_class <- factor(
      ifelse(preds[[model_col]] >= t, "strike", "non_strike"),
      levels = c("non_strike", "strike")
    )
    tibble(
      threshold = t,
      precision = precision_vec(preds$truth, pred_class, event_level = "second"),
      recall    = recall_vec(preds$truth, pred_class, event_level = "second"),
      f1        = f_meas_vec(preds$truth, pred_class, event_level = "second")
    )
  }) %>%
    filter(!is.nan(precision))
  
  # Find threshold that meets precision target with maximum recall
  valid <- results %>% filter(precision >= target_precision)
  
  if (nrow(valid) == 0) {
    message("  Cannot meet target precision; using threshold with max F1")
    best <- results %>% slice_max(f1, n = 1)
  } else {
    best <- valid %>% slice_max(recall, n = 1)
  }
  
  message(glue("  Optimal threshold: {best$threshold}"))
  message(glue("  Precision: {round(best$precision, 3)} | ",
               "Recall: {round(best$recall, 3)} | ",
               "F1: {round(best$f1, 3)}"))
  
  # Plot precision-recall vs threshold
  p <- results %>%
    pivot_longer(c(precision, recall, f1), names_to = "metric") %>%
    ggplot(aes(x = threshold, y = value, color = metric)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = best$threshold, linetype = "dashed", color = "gray40") +
    annotate("text", x = best$threshold + 0.02, y = 0.5,
             label = glue("t = {best$threshold}"), hjust = 0) +
    labs(title = "Threshold Optimization",
         subtitle = glue("Target: precision ≥ {target_precision}"),
         x = "Classification Threshold", y = "Metric Value") +
    theme_minimal(base_size = 13) +
    scale_color_manual(values = c(f1 = "#8b5cf6", precision = "#ef4444",
                                   recall = "#3b82f6"))
  
  ggsave("output/threshold_optimization.png", p, width = 8, height = 5, dpi = 200)
  
  return(best$threshold)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 6: FEATURE IMPORTANCE & DIAGNOSTICS
# ══════════════════════════════════════════════════════════════════════════════

model_diagnostics <- function(models, eval_results) {
  
  dir.create("output", showWarnings = FALSE)
  
  # ── 6.1 Feature Importance (XGBoost) ──
  message("\n── Feature Importance ──")
  
  xgb_importance <- models$xgb %>%
    extract_fit_parsnip() %>%
    vip(num_features = 30, geom = "point") +
    labs(title = "XGBoost — Top 30 Feature Importances") +
    theme_minimal(base_size = 12)
  
  ggsave("output/feature_importance_xgb.png", xgb_importance,
         width = 10, height = 8, dpi = 200)
  
  # ── 6.2 ROC Curves ──
  preds <- eval_results$predictions
  
  roc_data <- bind_rows(
    roc_curve(preds, truth, rf_prob, event_level = "second") %>%
      mutate(model = "Random Forest"),
    roc_curve(preds, truth, xgb_prob, event_level = "second") %>%
      mutate(model = "XGBoost"),
    roc_curve(preds, truth, lr_prob, event_level = "second") %>%
      mutate(model = "Logistic Reg.")
  )
  
  roc_plot <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity,
                                    color = model)) +
    geom_line(linewidth = 1) +
    geom_abline(linetype = "dashed", color = "gray60") +
    labs(title = "ROC Curves — Spatial Hold-Out Test Set",
         x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal(base_size = 13) +
    scale_color_manual(values = c("Random Forest" = "#22c55e",
                                   "XGBoost" = "#f97316",
                                   "Logistic Reg." = "#64748b"))
  
  ggsave("output/roc_curves.png", roc_plot, width = 8, height = 6, dpi = 200)
  
  # ── 6.3 PR Curves ──
  pr_data <- bind_rows(
    pr_curve(preds, truth, rf_prob, event_level = "second") %>%
      mutate(model = "Random Forest"),
    pr_curve(preds, truth, xgb_prob, event_level = "second") %>%
      mutate(model = "XGBoost"),
    pr_curve(preds, truth, lr_prob, event_level = "second") %>%
      mutate(model = "Logistic Reg.")
  )
  
  pr_plot <- ggplot(pr_data, aes(x = recall, y = precision, color = model)) +
    geom_line(linewidth = 1) +
    labs(title = "Precision-Recall Curves",
         subtitle = "Important when classes are imbalanced",
         x = "Recall", y = "Precision") +
    theme_minimal(base_size = 13) +
    scale_color_manual(values = c("Random Forest" = "#22c55e",
                                   "XGBoost" = "#f97316",
                                   "Logistic Reg." = "#64748b"))
  
  ggsave("output/pr_curves.png", pr_plot, width = 8, height = 6, dpi = 200)
  
  # ── 6.4 Calibration Plot ──
  cal_data <- preds %>%
    mutate(
      prob_bin = cut(xgb_prob, breaks = seq(0, 1, 0.1), include.lowest = TRUE),
      is_strike = as.integer(truth == "strike")
    ) %>%
    group_by(prob_bin) %>%
    summarise(
      mean_predicted = mean(xgb_prob),
      mean_actual    = mean(is_strike),
      n = n(),
      .groups = "drop"
    )
  
  cal_plot <- ggplot(cal_data, aes(x = mean_predicted, y = mean_actual)) +
    geom_abline(linetype = "dashed", color = "gray60") +
    geom_point(aes(size = n), color = "#f97316") +
    geom_line(color = "#f97316") +
    labs(title = "XGBoost Calibration Plot",
         subtitle = "Are predicted probabilities well-calibrated?",
         x = "Mean Predicted Probability", y = "Observed Strike Rate") +
    theme_minimal(base_size = 13) +
    scale_size_continuous(range = c(2, 8))
  
  ggsave("output/calibration_plot.png", cal_plot, width = 7, height = 6, dpi = 200)
  
  # ── 6.5 Cross-validation performance across folds ──
  cv_results <- collect_metrics(models$xgb_tuned) %>%
    filter(.metric == "roc_auc") %>%
    slice_max(mean, n = 5)
  
  message("\n── Top 5 XGBoost configs by CV ROC-AUC ──")
  print(cv_results)
  
  message("\nDiagnostic plots saved to output/")
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 7: EXPORT MODEL FOR PRODUCTION
# ══════════════════════════════════════════════════════════════════════════════

export_model <- function(models, optimal_threshold, eval_results) {
  
  dir.create("models", showWarnings = FALSE)
  
  # Choose best model based on PR-AUC (most relevant for imbalanced detection)
  best_model_name <- eval_results$metrics %>%
    slice_max(pr_auc, n = 1) %>%
    pull(model)
  
  message(glue("\n=== EXPORTING BEST MODEL: {best_model_name} ==="))
  
  # Map name to object
  best_key <- case_when(
    str_detect(best_model_name, "Random") ~ "rf",
    str_detect(best_model_name, "XGB")    ~ "xgb",
    TRUE                                   ~ "lr"
  )
  
  best_model <- models[[best_key]]
  
  # Save model bundle
  model_bundle <- list(
    model            = best_model,
    model_name       = best_model_name,
    threshold        = optimal_threshold,
    metrics          = eval_results$metrics,
    feature_names    = best_model %>%
      extract_recipe() %>%
      prep() %>%
      bake(new_data = NULL) %>%
      select(-label) %>%
      names(),
    trained_at       = Sys.time(),
    training_samples = nrow(models$train_df),
    test_samples     = nrow(models$test_df)
  )
  
  saveRDS(model_bundle, "models/strike_classifier.rds")
  
  # Also save all models for ensemble potential
  saveRDS(list(
    rf  = models$rf,
    xgb = models$xgb,
    lr  = models$lr,
    threshold = optimal_threshold
  ), "models/all_models.rds")
  
  message(glue("  Model saved: models/strike_classifier.rds"))
  message(glue("  Threshold: {optimal_threshold}"))
  message(glue("  Features: {length(model_bundle$feature_names)}"))
  
  # Print final summary
  message("\n╔══════════════════════════════════════════════════════════════╗")
  message(glue("║  BEST MODEL: {best_model_name}"))
  best_metrics <- eval_results$metrics %>% filter(model == best_model_name)
  message(glue("║  ROC-AUC:   {round(best_metrics$roc_auc, 3)}"))
  message(glue("║  PR-AUC:    {round(best_metrics$pr_auc, 3)}"))
  message(glue("║  Precision: {round(best_metrics$precision, 3)}"))
  message(glue("║  Recall:    {round(best_metrics$recall, 3)}"))
  message(glue("║  F1:        {round(best_metrics$f1, 3)}"))
  message(glue("║  Threshold: {optimal_threshold}"))
  message("╚══════════════════════════════════════════════════════════════╝")
  
  return(model_bundle)
}


# ══════════════════════════════════════════════════════════════════════════════
# MAIN
# ══════════════════════════════════════════════════════════════════════════════

if (sys.nframe() == 0) {
  
  # ── Load training data ──
  training_data <- readRDS("data/training_features.rds")
  
  # ── Prepare features ──
  df <- prepare_data(training_data)
  
  # ── Spatial CV folds ──
  spatial_folds <- create_spatial_folds(training_data)
  
  # ── Train models ──
  models <- train_models(df, spatial_folds)
  
  # ── Evaluate ──
  eval_results <- evaluate_models(models)
  
  # ── Optimize threshold ──
  optimal_threshold <- optimize_threshold(
    eval_results$predictions,
    model_col = "xgb_prob",
    target_precision = 0.85  # we want few false alarms
  )
  
  # ── Diagnostics ──
  model_diagnostics(models, eval_results)
  
  # ── Export ──
  model_bundle <- export_model(models, optimal_threshold, eval_results)
}
