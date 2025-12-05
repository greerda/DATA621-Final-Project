# ================================================
# utils.R - Helper Functions & Transformations
# ================================================
# Common utilities and data transformation functions
# used across the analysis pipeline.
#
# TRANSFORMATION STRATEGY (based on EDA findings):
# ------------------------------------------------
# 1. LOG TRANSFORM CHARGES
#    - charges is heavily right-skewed
#    - log(charges) normalizes the distribution
#    - improves linear model assumptions
#
# 2. BMI CATEGORIES
#    - BMI as continuous may miss threshold effects
#    - WHO categories: underweight/normal/overweight/obese
#    - Captures non-linear relationship with charges
#
# 3. INTERACTION TERMS
#    - smoker:bmi - smokers with high BMI have much higher charges
#    - smoker:age - smoking effect may increase with age
#    - These interactions are visible in the pair plots
# ================================================

library(dplyr)
library(tidymodels)

# ------------------------------------------------
# Log Transform for Charges
# ------------------------------------------------
# Use log1p to handle any zero values safely
add_log_charges <- function(df) {
  df <- df %>%
    mutate(log_charges = log(charges))
  return(df)
}

# ------------------------------------------------
# BMI Categories (WHO Standard)
# ------------------------------------------------
# Underweight: < 18.5
# Normal:      18.5 - 24.9
# Overweight:  25 - 29.9
# Obese:       >= 30
add_bmi_category <- function(df) {
  df <- df %>%
    mutate(
      bmi_cat = cut(
        bmi,
        breaks = c(-Inf, 18.5, 25, 30, Inf),
        labels = c("underweight", "normal", "overweight", "obese"),
        right = FALSE
      )
    )
  return(df)
}

# ------------------------------------------------
# Age Groups
# ------------------------------------------------
# Bin age into decades for categorical analysis
add_age_group <- function(df) {
  df <- df %>%
    mutate(
      age_group = cut(
        age,
        breaks = c(18, 30, 40, 50, 60, Inf),
        labels = c("18-29", "30-39", "40-49", "50-59", "60+"),
        right = FALSE
      )
    )
  return(df)
}

# ------------------------------------------------
# Apply All Transformations
# ------------------------------------------------
# Convenience function to prep data for modeling
apply_transformations <- function(df) {
  df <- df %>%
    add_log_charges() %>%
    add_bmi_category() %>%
    add_age_group()

  cat("Transformations applied:\n")
  cat("  - log_charges (log of charges)\n")
  cat("  - bmi_cat (WHO BMI categories)\n")
  cat("  - age_group (decade bins)\n")

  return(df)
}

# ------------------------------------------------
# Check Skewness
# ------------------------------------------------
# Helper to compare before/after log transform
check_skewness <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  skew <- sum((x - m)^3) / (n * s^3)
  return(round(skew, 3))
}

#----------------------------------------------
# Checking for Empty Fields
#----------------------------------------------
# Helper to check if there are empty fields or NA in the datasource

find_empty_cells <- function(df) {
  # Create a logical matrix where TRUE means the cell is empty or NA
  bad_matrix <- is.na(df) | df == ""
  
  # Convert TRUE positions into row/column indices
  which(bad_matrix, arr.ind = TRUE)
}



# ================================================
# RECIPE BUILDERS
# ================================================
# Flexible recipe construction for different modeling needs.
# Supports: basic, with interactions, with log transform
# ================================================

# ------------------------------------------------
# Basic Recipe (encoding + scaling only)
# ------------------------------------------------
build_recipe_basic <- function(df) {
  recipe(charges ~ ., data = df) %>%
    step_string2factor(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors())
}

# ------------------------------------------------
# Recipe with Interaction Terms
# ------------------------------------------------
# Adds smoker:bmi and smoker:age interactions (identified in EDA)
build_recipe_interactions <- function(df) {
  recipe(charges ~ ., data = df) %>%
    step_string2factor(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_interact(terms = ~ starts_with("smoker"):bmi) %>%
    step_interact(terms = ~ starts_with("smoker"):age) %>%
    step_normalize(all_numeric_predictors())
}

# ------------------------------------------------
# Recipe with Log Transform on Target
# ------------------------------------------------
# For models where log(charges) improves normality
build_recipe_log_target <- function(df) {
  recipe(charges ~ ., data = df) %>%
    step_log(charges, base = exp(1)) %>%
    step_string2factor(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors())
}

# ------------------------------------------------
# Full Recipe (interactions + optional log transform)
# ------------------------------------------------
# Most comprehensive recipe for best model performance
build_recipe_full <- function(df, log_target = FALSE) {
  rec <- recipe(charges ~ ., data = df)

  if (log_target) {
    rec <- rec %>% step_log(charges, base = exp(1))
  }

  rec %>%
    step_string2factor(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_interact(terms = ~ starts_with("smoker"):bmi) %>%
    step_interact(terms = ~ starts_with("smoker"):age) %>%
    step_normalize(all_numeric_predictors())
}

# ------------------------------------------------
# Default Recipe (backward compatible)
# ------------------------------------------------
build_insurance_recipe <- function(df) {
  build_recipe_basic(df)
}

# ================================================
# MODEL EVALUATION METRICS
# ================================================
# Reusable functions for computing and reporting
# regression model performance metrics.
# ================================================

# ------------------------------------------------
# Core Metric Calculations
# ------------------------------------------------

# Root Mean Squared Error
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Mean Absolute Error
calc_mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Mean Absolute Percentage Error
calc_mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# R-squared (Coefficient of Determination)
calc_r2 <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - (ss_res / ss_tot)
}

# Adjusted R-squared
calc_adj_r2 <- function(actual, predicted, n_predictors) {
  n <- length(actual)
  r2 <- calc_r2(actual, predicted)
  1 - ((1 - r2) * (n - 1) / (n - n_predictors - 1))
}

# ------------------------------------------------
# Comprehensive Metrics Function
# ------------------------------------------------
# Returns all key metrics in a named list/tibble
calculate_metrics <- function(actual, predicted, n_predictors = NULL) {
  metrics <- list(
    rmse = calc_rmse(actual, predicted),
    mae  = calc_mae(actual, predicted),
    mape = calc_mape(actual, predicted),
    r2   = calc_r2(actual, predicted)
  )

  if (!is.null(n_predictors)) {
    metrics$adj_r2 <- calc_adj_r2(actual, predicted, n_predictors)
  }

  tibble::as_tibble(metrics)
}

# ------------------------------------------------
# Evaluate Fitted Workflow
# ------------------------------------------------
# Takes a fitted tidymodels workflow and test data
# Returns metrics tibble
evaluate_model <- function(fitted_workflow, test_data, target_col = "charges") {
  # Generate predictions
  preds <- predict(fitted_workflow, new_data = test_data) %>%
    dplyr::bind_cols(test_data)

  # Extract actual and predicted values
  actual <- preds[[target_col]]
  predicted <- preds$.pred

  # Count predictors from the workflow
  n_pred <- length(fitted_workflow$pre$actions$recipe$recipe$var_info$variable) - 1

  # Calculate metrics
  metrics <- calculate_metrics(actual, predicted, n_predictors = n_pred)

  # Add useful context
  metrics$n_obs <- nrow(test_data)

  metrics
}

# ------------------------------------------------
# Compare Multiple Models
# ------------------------------------------------
# Takes a named list of fitted workflows and test data
# Returns comparison tibble
compare_models <- function(model_list, test_data, target_col = "charges") {
  results <- purrr::map_dfr(names(model_list), function(nm) {
    metrics <- evaluate_model(model_list[[nm]], test_data, target_col)
    metrics$model <- nm
    metrics
  })

  # Reorder columns
  results %>%
    dplyr::select(model, dplyr::everything())
}

# ------------------------------------------------
# Print Metrics Summary
# ------------------------------------------------
# Pretty prints evaluation metrics
print_metrics <- function(metrics, model_name = "Model") {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat(" ", model_name, "Performance Metrics\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat(sprintf("  RMSE:        $%.2f\n", metrics$rmse))
  cat(sprintf("  MAE:         $%.2f\n", metrics$mae))
  cat(sprintf("  MAPE:        %.2f%%\n", metrics$mape))
  cat(sprintf("  R-squared:   %.4f\n", metrics$r2))

  if ("adj_r2" %in% names(metrics)) {
    cat(sprintf("  Adj R-sq:    %.4f\n", metrics$adj_r2))
  }

  cat(sprintf("  N obs:       %d\n", metrics$n_obs))
  cat("\n", paste(rep("-", 50), collapse = ""), "\n")
}

# ------------------------------------------------
# Cross-Validation Metrics
# ------------------------------------------------
# Evaluates model across CV folds using tidymodels
cv_evaluate <- function(workflow, cv_folds) {
  # Define metrics set
  reg_metrics <- yardstick::metric_set(
    yardstick::rmse,
    yardstick::mae,
    yardstick::rsq,
    yardstick::mape
  )

  # Fit and evaluate across folds
  cv_results <- tune::fit_resamples(
    workflow,
    resamples = cv_folds,
    metrics = reg_metrics,
    control = tune::control_resamples(save_pred = TRUE)
  )

  cv_results
}

# Summarize CV results
summarize_cv <- function(cv_results) {
  tune::collect_metrics(cv_results) %>%
    dplyr::select(.metric, mean, std_err, n)
}

# ================================================
# MODEL DIAGNOSTICS
# ================================================
# Functions for checking regression assumptions:
# - Residual analysis
# - Normality (QQ plots)
# - Multicollinearity (VIF)
# - Influential observations (Cook's distance)
# ================================================

# ------------------------------------------------
# Residual Plots
# ------------------------------------------------
# Creates standard diagnostic plots for regression
plot_residuals <- function(model, title_prefix = "") {
  # Extract fitted values and residuals
  fitted_vals <- fitted(model)
  resids <- residuals(model)
  std_resids <- rstandard(model)

  plots <- list()

  # 1. Residuals vs Fitted
  df1 <- data.frame(fitted = fitted_vals, residuals = resids)
  p1 <- ggplot2::ggplot(df1, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5, color = "steelblue") +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "orange") +
    ggplot2::labs(
      title = paste(title_prefix, "Residuals vs Fitted"),
      x = "Fitted Values",
      y = "Residuals"
    ) +
    ggplot2::theme_minimal()

  # 2. Scale-Location (sqrt of standardized residuals)
  df2 <- data.frame(fitted = fitted_vals, sqrt_std_resid = sqrt(abs(std_resids)))
  p2 <- ggplot2::ggplot(df2, ggplot2::aes(x = fitted, y = sqrt_std_resid)) +
    ggplot2::geom_point(alpha = 0.5, color = "steelblue") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "orange") +
    ggplot2::labs(
      title = paste(title_prefix, "Scale-Location"),
      x = "Fitted Values",
      y = expression(sqrt("Standardized Residuals"))
    ) +
    ggplot2::theme_minimal()

  # 3. Histogram of residuals
  df3 <- data.frame(residuals = resids)
  p3 <- ggplot2::ggplot(df3, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    ggplot2::labs(
      title = paste(title_prefix, "Residual Distribution"),
      x = "Residuals",
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  plots$resid_vs_fitted <- p1
  plots$scale_location <- p2
  plots$resid_hist <- p3

  return(plots)
}

# ------------------------------------------------
# QQ Plot for Residuals
# ------------------------------------------------
plot_qq_residuals <- function(model, title_prefix = "") {
  std_resids <- rstandard(model)
  df <- data.frame(resid = std_resids)

  p <- ggplot2::ggplot(df, ggplot2::aes(sample = resid)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(
      title = paste(title_prefix, "Normal Q-Q Plot"),
      x = "Theoretical Quantiles",
      y = "Standardized Residuals"
    ) +
    ggplot2::theme_minimal()

  return(p)
}

# ------------------------------------------------
# Variance Inflation Factor (VIF)
# ------------------------------------------------
# Checks for multicollinearity
# VIF > 5 suggests moderate, VIF > 10 suggests high multicollinearity
calc_vif <- function(model) {
  # Get model matrix (excluding intercept)
  X <- model.matrix(model)[, -1, drop = FALSE]

  # Calculate VIF for each predictor
  vif_vals <- sapply(1:ncol(X), function(i) {
    y <- X[, i]
    x <- X[, -i, drop = FALSE]
    if (ncol(x) == 0) return(1)
    r2 <- summary(lm(y ~ x))$r.squared
    1 / (1 - r2)
  })

  names(vif_vals) <- colnames(X)

  # Create summary data frame
  vif_df <- data.frame(
    variable = names(vif_vals),
    vif = round(vif_vals, 3),
    concern = ifelse(vif_vals > 10, "HIGH",
                     ifelse(vif_vals > 5, "Moderate", "OK"))
  )

  vif_df <- vif_df[order(-vif_df$vif), ]
  rownames(vif_df) <- NULL

  return(vif_df)
}

# ------------------------------------------------
# Cook's Distance
# ------------------------------------------------
# Identifies influential observations
# Threshold: 4/n or 4/(n-k-1)
calc_cooks_distance <- function(model) {
  cooks_d <- cooks.distance(model)
  n <- length(cooks_d)
  threshold <- 4 / n

  influential <- which(cooks_d > threshold)

  list(
    cooks_d = cooks_d,
    threshold = threshold,
    n_influential = length(influential),
    influential_indices = influential,
    influential_values = cooks_d[influential]
  )
}

# Plot Cook's distance
plot_cooks_distance <- function(model, title_prefix = "") {
  cooks_d <- cooks.distance(model)
  n <- length(cooks_d)
  threshold <- 4 / n

  df <- data.frame(
    index = 1:n,
    cooks_d = cooks_d
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = index, y = cooks_d)) +
    ggplot2::geom_point(alpha = 0.5, color = "steelblue") +
    ggplot2::geom_hline(yintercept = threshold, color = "red", linetype = "dashed") +
    ggplot2::labs(
      title = paste(title_prefix, "Cook's Distance"),
      subtitle = sprintf("Threshold = 4/n = %.4f", threshold),
      x = "Observation Index",
      y = "Cook's Distance"
    ) +
    ggplot2::theme_minimal()

  return(p)
}

# ------------------------------------------------
# Full Diagnostic Suite
# ------------------------------------------------
# Runs all diagnostics and returns comprehensive results
run_diagnostics <- function(model, model_name = "Model") {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("  DIAGNOSTIC REPORT:", model_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  # 1. VIF
  cat("\n--- Variance Inflation Factors (VIF) ---\n")
  vif_results <- calc_vif(model)
  print(vif_results)

  high_vif <- sum(vif_results$vif > 5)
  if (high_vif > 0) {
    cat(sprintf("\nWarning: %d variable(s) with VIF > 5\n", high_vif))
  } else {
    cat("\nNo multicollinearity concerns (all VIF < 5)\n")
  }

  # 2. Cook's Distance
  cat("\n--- Cook's Distance (Influential Points) ---\n")
  cooks_results <- calc_cooks_distance(model)
  cat(sprintf("Threshold: %.4f (4/n)\n", cooks_results$threshold))
  cat(sprintf("Influential observations: %d (%.1f%%)\n",
              cooks_results$n_influential,
              100 * cooks_results$n_influential / length(cooks_results$cooks_d)))

  # 3. Normality test on residuals
  cat("\n--- Normality of Residuals ---\n")
  resids <- residuals(model)
  if (length(resids) > 5000) {
    set.seed(123)
    resids_sample <- sample(resids, 5000)
  } else {
    resids_sample <- resids
  }
  shapiro_test <- shapiro.test(resids_sample)
  cat(sprintf("Shapiro-Wilk test: W = %.4f, p-value = %.4e\n",
              shapiro_test$statistic, shapiro_test$p.value))
  if (shapiro_test$p.value < 0.05) {
    cat("Result: Residuals deviate from normality (p < 0.05)\n")
  } else {
    cat("Result: Cannot reject normality (p >= 0.05)\n")
  }

  # 4. Generate plots
  plots <- list()
  plots$residual_plots <- plot_residuals(model, model_name)
  plots$qq_plot <- plot_qq_residuals(model, model_name)
  plots$cooks_plot <- plot_cooks_distance(model, model_name)

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")

  invisible(list(
    vif = vif_results,
    cooks = cooks_results,
    shapiro = shapiro_test,
    plots = plots
  ))
}

# ================================================
# MODEL COMPARISON TABLE
# ================================================
# Creates comprehensive comparison with AIC/BIC

# Compare multiple lm models
compare_lm_models <- function(model_list) {
  results <- lapply(names(model_list), function(nm) {
    m <- model_list[[nm]]
    s <- summary(m)

    data.frame(
      model = nm,
      r2 = round(s$r.squared, 4),
      adj_r2 = round(s$adj.r.squared, 4),
      rmse = round(sqrt(mean(s$residuals^2)), 2),
      mae = round(mean(abs(s$residuals)), 2),
      aic = round(AIC(m), 2),
      bic = round(BIC(m), 2),
      n_predictors = length(coef(m)) - 1,
      n_obs = nrow(m$model)
    )
  })

  comparison <- do.call(rbind, results)
  comparison <- comparison[order(comparison$aic), ]
  rownames(comparison) <- NULL

  return(comparison)
}

# Print formatted comparison table
print_model_comparison <- function(comparison) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("                    MODEL COMPARISON TABLE\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")

  print(comparison, row.names = FALSE)

  cat("\n--- Best Models ---\n")
  cat(sprintf("  By R²:      %s (%.4f)\n",
              comparison$model[which.max(comparison$r2)],
              max(comparison$r2)))
  cat(sprintf("  By Adj R²:  %s (%.4f)\n",
              comparison$model[which.max(comparison$adj_r2)],
              max(comparison$adj_r2)))
  cat(sprintf("  By AIC:     %s (%.2f)\n",
              comparison$model[which.min(comparison$aic)],
              min(comparison$aic)))
  cat(sprintf("  By BIC:     %s (%.2f)\n",
              comparison$model[which.min(comparison$bic)],
              min(comparison$bic)))
  cat(sprintf("  By RMSE:    %s ($%.2f)\n",
              comparison$model[which.min(comparison$rmse)],
              min(comparison$rmse)))

  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
}

