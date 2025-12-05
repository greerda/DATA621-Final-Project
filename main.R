library(dplyr)
library(ggplot2)
library(reshape2)
library(tidymodels)
library(skimr)
library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(yardstick)
library(tune)
library(workflowsets)
library(purrr)

source("R/utils.R")
source("R/data_load.R")
source("R/eda.R")
source("R/models.R")
source("R/plots.R")

# Load data
insurance <- load_insurance_data()
cat("Data loaded. Rows:", nrow(insurance), "Columns:", ncol(insurance), "\n")

# Quick peek
print(head(insurance))
str(insurance)

# --- Run EDA and collect plots ---
eda_results <- run_eda(insurance)

uni_plots    <- eda_results$uni_plots    # list of ggplots
bi_plots     <- eda_results$bi_plots     # list of ggplots
p_corr       <- eda_results$corr_plot    # single ggplot
p_pairs      <- eda_results$pair_plot    # pair plot matrix
outlier_info <- eda_results$outlier_info # outlier detection results

# --- Key report plots from plots.R ---
p1 <- plot_charges_by_smoker(insurance)
p2 <- plot_charges_vs_bmi(insurance)
p3 <- plot_region_charges(insurance)

print(p1)
print(p2)
print(p3)
print(p_corr)

# --- Create folders if needed ---
dir.create("plots", showWarnings = FALSE)
dir.create("plots/univariate", showWarnings = FALSE, recursive = TRUE)
dir.create("plots/bivariate", showWarnings = FALSE, recursive = TRUE)

# --- Save univariate plots ---
for (nm in names(uni_plots)) {
  ggsave(
    filename = file.path("plots/univariate", paste0(nm, ".png")),
    plot     = uni_plots[[nm]],
    width    = 7,
    height   = 5
  )
}

# --- Save bivariate plots ---
for (nm in names(bi_plots)) {
  ggsave(
    filename = file.path("plots/bivariate", paste0(nm, ".png")),
    plot     = bi_plots[[nm]],
    width    = 7,
    height   = 5
  )
}

# --- Save correlation heatmap ---
ggsave(
  "plots/correlation_heatmap.png",
  p_corr,
  width = 7,
  height = 6
)

# --- Save pair plot (base R plot, not ggplot) ---
png("plots/pair_plot.png", width = 800, height = 800)
eda_pairplot(insurance)
dev.off()

# --- Save key report plots ---
ggsave("plots/plot_region_charges.png",    p3, width = 7, height = 5)
ggsave("plots/plot_charges_vs_bmi.png",    p2, width = 7, height = 5)
ggsave("plots/plot_charges_by_smoker.png", p1, width = 7, height = 5)

# Optional: see what got written
print(list.files("plots", recursive = TRUE))

# ================================================
# MODEL TRAINING WITH PROPER TRAIN/TEST SPLIT
# ================================================
# Using Stepwise AIC model (best performer from model comparison)
# Formula: charges ~ I(age^2) + bmi + I(bmi^2) + children + sex +
#                    smoker + region + smoker:bmi
# ================================================

## ---- Prepare data with train/test split ----
data_splits <- prepare_modeling_data("data/insurance.csv",
                                      train_prop = 0.8,
                                      cv_folds = 10,
                                      seed = 123)

train_data <- data_splits$train
test_data  <- data_splits$test
cv_folds   <- data_splits$cv_folds

## ---- Fit best model (Stepwise AIC) ----
cat("\n\n========== FITTING BEST MODEL (Stepwise AIC) ==========\n")
best_model <- fit_model_stepwise_aic(train_data, trace = FALSE)

cat("\nModel Formula:\n")
print(formula(best_model))

cat("\nModel Summary:\n")
summary(best_model)

## ---- Evaluate on test set ----
cat("\n\n========== TEST SET EVALUATION ==========\n")
test_pred <- predict(best_model, newdata = test_data)
test_actual <- test_data$charges

test_metrics <- data.frame(
  rmse = sqrt(mean((test_actual - test_pred)^2)),
  mae = mean(abs(test_actual - test_pred)),
  mape = mean(abs((test_actual - test_pred) / test_actual)) * 100,
  r2 = 1 - sum((test_actual - test_pred)^2) / sum((test_actual - mean(test_actual))^2)
)

cat(sprintf("\n  RMSE:        $%.2f\n", test_metrics$rmse))
cat(sprintf("  MAE:         $%.2f\n", test_metrics$mae))
cat(sprintf("  MAPE:        %.2f%%\n", test_metrics$mape))
cat(sprintf("  R-squared:   %.4f\n", test_metrics$r2))

## ---- Model Diagnostics ----
cat("\n\n========== MODEL DIAGNOSTICS ==========\n")
diag_results <- run_diagnostics(best_model, "Stepwise AIC")

## ---- Generate predictions on test set ----
insurance_pred <- test_data %>%
  mutate(
    predicted_charges = test_pred,
    residual = charges - predicted_charges,
    pct_error = abs(residual / charges) * 100
  )

cat("\nSample Predictions:\n")
print(head(insurance_pred %>% dplyr::select(charges, predicted_charges, residual, pct_error)))

## ---- Save outputs ----
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
write.csv(insurance_pred, "outputs/insurance_predictions.csv", row.names = FALSE)
write.csv(test_metrics, "outputs/model_metrics.csv", row.names = FALSE)

# Save diagnostic plots
ggsave("plots/diagnostics/best_model_resid_vs_fitted.png",
       diag_results$plots$residual_plots$resid_vs_fitted, width = 7, height = 5)
ggsave("plots/diagnostics/best_model_qq.png",
       diag_results$plots$qq_plot, width = 7, height = 5)
ggsave("plots/diagnostics/best_model_cooks.png",
       diag_results$plots$cooks_plot, width = 7, height = 5)

cat("\nOutputs saved to outputs/ directory\n")
cat("Diagnostic plots saved to plots/diagnostics/\n")
