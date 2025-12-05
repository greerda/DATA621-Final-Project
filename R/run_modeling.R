# ================================================
# run_modeling.R - Complete Modeling Pipeline
# ================================================
# DATA 621 Final Project - Insurance Cost Prediction
#
# This script runs the complete modeling pipeline:
#   1. Data preparation (train/test split, CV folds)
#   2. Fit all regression models
#   3. Model diagnostics (residuals, VIF, Cook's D)
#   4. Model comparison (R², Adj R², RMSE, MAE, AIC/BIC)
#   5. Cross-validation for final model selection
#   6. Save results and diagnostic plots
#
# Models fitted:
#   - Model 1: Multiple Linear Regression (baseline)
#   - Model 2: MLR with log-transformed charges
#   - Model 3: Polynomial + interaction terms
#   - Model 4: Stepwise selection (AIC and BIC)
# ================================================

# ------------------------------------------------
# SETUP
# ------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidymodels)
library(MASS)
library(purrr)

# Source helper functions
source("R/data_load.R")
source("R/utils.R")
source("R/models.R")

# Create output directories
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/diagnostics", showWarnings = FALSE)
dir.create("plots/diagnostics", showWarnings = FALSE, recursive = TRUE)

cat("\n")
cat("================================================\n")
cat("  INSURANCE COST PREDICTION - MODELING PIPELINE\n")
cat("================================================\n\n")

# ------------------------------------------------
# 1. DATA PREPARATION
# ------------------------------------------------
cat("STEP 1: DATA PREPARATION\n")
cat("------------------------\n")

# Load and preprocess data
data_splits <- prepare_modeling_data(
  path = "data/insurance.csv",
  train_prop = 0.8,
  cv_folds = 10,
  seed = 123
)

train_data <- data_splits$train
test_data <- data_splits$test
cv_folds <- data_splits$cv_folds

# Data quality report
quality_report <- data_quality_report(train_data)

# ------------------------------------------------
# 2. FIT ALL MODELS
# ------------------------------------------------
cat("\nSTEP 2: FITTING MODELS\n")
cat("----------------------\n")

models <- fit_all_models(train_data, verbose = TRUE)

# Print model summaries
cat("\n--- Model Summaries ---\n")
for (nm in names(models)) {
  cat("\n", paste(rep("-", 50), collapse = ""), "\n")
  cat(" ", toupper(nm), "\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  print(summary(models[[nm]]))
}

# ------------------------------------------------
# 3. MODEL COMPARISON (Training Data)
# ------------------------------------------------
cat("\n\nSTEP 3: MODEL COMPARISON (Training Metrics)\n")
cat("--------------------------------------------\n")

# Compare models using training data metrics (R², AIC, BIC)
# Note: Only non-log models can be directly compared via AIC/BIC
non_log_models <- models[c("baseline", "poly_interact", "stepwise_aic", "stepwise_bic")]
train_comparison <- compare_lm_models(non_log_models)
print_model_comparison(train_comparison)

# Save training comparison
write.csv(train_comparison, "outputs/model_comparison_train.csv", row.names = FALSE)

# ------------------------------------------------
# 4. TEST SET EVALUATION
# ------------------------------------------------
cat("\n\nSTEP 4: TEST SET EVALUATION\n")
cat("---------------------------\n")

test_comparison <- evaluate_all_on_test(models, test_data)

cat("\n--- Test Set Performance (All Models) ---\n")
print(test_comparison)

# Identify best model
best_model_name <- test_comparison$model[1]
cat(sprintf("\nBest model by RMSE: %s (RMSE = $%.2f)\n",
            best_model_name, test_comparison$rmse[1]))

# Save test comparison
write.csv(test_comparison, "outputs/model_comparison_test.csv", row.names = FALSE)

# ------------------------------------------------
# 5. MODEL DIAGNOSTICS
# ------------------------------------------------
cat("\n\nSTEP 5: MODEL DIAGNOSTICS\n")
cat("-------------------------\n")

# Run diagnostics on key models
diagnostic_results <- list()

for (model_name in c("baseline", "poly_interact", "stepwise_aic")) {
  cat(sprintf("\nRunning diagnostics for: %s\n", model_name))
  diag <- run_diagnostics(models[[model_name]], model_name)
  diagnostic_results[[model_name]] <- diag

  # Save diagnostic plots
  for (plot_name in names(diag$plots$residual_plots)) {
    ggsave(
      filename = sprintf("plots/diagnostics/%s_%s.png", model_name, plot_name),
      plot = diag$plots$residual_plots[[plot_name]],
      width = 7, height = 5
    )
  }

  ggsave(
    filename = sprintf("plots/diagnostics/%s_qq.png", model_name),
    plot = diag$plots$qq_plot,
    width = 7, height = 5
  )

  ggsave(
    filename = sprintf("plots/diagnostics/%s_cooks.png", model_name),
    plot = diag$plots$cooks_plot,
    width = 7, height = 5
  )
}

# Save VIF results
vif_all <- do.call(rbind, lapply(names(diagnostic_results), function(nm) {
  vif_df <- diagnostic_results[[nm]]$vif
  vif_df$model <- nm
  vif_df
}))
write.csv(vif_all, "outputs/vif_results.csv", row.names = FALSE)

# ------------------------------------------------
# 6. CROSS-VALIDATION
# ------------------------------------------------
cat("\n\nSTEP 6: CROSS-VALIDATION\n")
cat("------------------------\n")

# Build tidymodels workflows for CV
cat("Building workflows for cross-validation...\n")

# Basic recipe
rec_basic <- build_recipe_basic(train_data)
rec_interact <- build_recipe_interactions(train_data)

# Workflows to compare
cv_workflows <- list(
  lm_basic = build_workflow(rec_basic, spec_linear_reg()),
  lm_interact = build_workflow(rec_interact, spec_linear_reg())
)

# Run CV for each workflow
cat("Running 10-fold cross-validation...\n")
cv_results_list <- list()

for (wf_name in names(cv_workflows)) {
  cat(sprintf("  - %s\n", wf_name))
  cv_res <- cv_evaluate(cv_workflows[[wf_name]], cv_folds)
  cv_results_list[[wf_name]] <- summarize_cv(cv_res)
}

# Combine CV results
cv_summary <- do.call(rbind, lapply(names(cv_results_list), function(nm) {
  df <- cv_results_list[[nm]]
  df$model <- nm
  df
}))

cat("\n--- Cross-Validation Results ---\n")
print(cv_summary %>%
        tidyr::pivot_wider(names_from = .metric, values_from = c(mean, std_err)) %>%
        dplyr::select(model, mean_rmse, std_err_rmse, mean_rsq, std_err_rsq))

# Save CV results
write.csv(cv_summary, "outputs/cv_results.csv", row.names = FALSE)

# ------------------------------------------------
# 7. FINAL MODEL SELECTION
# ------------------------------------------------
cat("\n\nSTEP 7: FINAL MODEL SELECTION\n")
cat("-----------------------------\n")

# Based on test set and CV performance
cat("\nSelection Criteria:\n")
cat("  1. Test set RMSE (primary)\n")
cat("  2. Cross-validation stability\n")
cat("  3. Model interpretability\n")
cat("  4. Diagnostic quality (normality, VIF)\n")

cat(sprintf("\nRecommended Model: %s\n", best_model_name))
cat("Rationale: Lowest test RMSE with acceptable diagnostics\n")

# Final model summary
final_model <- models[[best_model_name]]
cat("\n--- Final Model Summary ---\n")
print(summary(final_model))

# ------------------------------------------------
# 8. SAVE FINAL OUTPUTS
# ------------------------------------------------
cat("\n\nSTEP 8: SAVING OUTPUTS\n")
cat("----------------------\n")

# Generate predictions on test set
is_log_model <- best_model_name %in% c("log_transform", "full_log", "stepwise_log")
if (is_log_model) {
  final_predictions <- exp(predict(final_model, newdata = test_data))
} else {
  final_predictions <- predict(final_model, newdata = test_data)
}

# Create prediction dataframe
pred_df <- test_data %>%
  mutate(
    predicted_charges = final_predictions,
    residual = charges - predicted_charges,
    pct_error = abs(residual / charges) * 100
  )

# Save predictions
write.csv(pred_df, "outputs/final_predictions.csv", row.names = FALSE)

# Summary statistics
cat("\nPrediction Summary:\n")
cat(sprintf("  Mean Absolute Error: $%.2f\n", mean(abs(pred_df$residual))))
cat(sprintf("  Mean Percentage Error: %.2f%%\n", mean(pred_df$pct_error)))
cat(sprintf("  Predictions within 20%%: %.1f%%\n",
            100 * mean(pred_df$pct_error < 20)))

cat("\nOutputs saved to:\n")
cat("  - outputs/model_comparison_train.csv\n")
cat("  - outputs/model_comparison_test.csv\n")
cat("  - outputs/cv_results.csv\n")
cat("  - outputs/vif_results.csv\n")
cat("  - outputs/final_predictions.csv\n")
cat("  - plots/diagnostics/*.png\n")

cat("\n================================================\n")
cat("  MODELING PIPELINE COMPLETE\n")
cat("================================================\n\n")
