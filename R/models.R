# ================================================
# models.R - Model Specifications and Builders
# ================================================
# Defines all regression models for the insurance analysis:
#   Model 1: Multiple Linear Regression (baseline)
#   Model 2: MLR with log-transformed charges
#   Model 3: Polynomial regression + interaction terms
#   Model 4: Stepwise selection (AIC/BIC)
#
# Also includes tidymodels workflow builders for CV.
# ================================================

library(tidymodels)
library(MASS)  # for stepAIC

# ================================================
# MODEL 1: MULTIPLE LINEAR REGRESSION (BASELINE)
# ================================================
# Standard OLS with all predictors

fit_model_baseline <- function(train_data) {
  lm(charges ~ age + sex + bmi + children + smoker + region,
     data = train_data)
}

# ================================================
# MODEL 2: MLR WITH LOG-TRANSFORMED CHARGES
# ================================================
# Log transform improves normality of residuals

fit_model_log <- function(train_data) {
  lm(log(charges) ~ age + sex + bmi + children + smoker + region,
     data = train_data)
}

# Predict function for log model (back-transforms)
predict_log_model <- function(model, newdata) {
  log_pred <- predict(model, newdata = newdata)
  exp(log_pred)  # back-transform to original scale
}

# ================================================
# MODEL 3: POLYNOMIAL + INTERACTIONS
# ================================================
# Includes:
#   - age² and bmi² (polynomial terms)
#   - smoker:bmi interaction (identified in EDA)
#   - smoker:age interaction

fit_model_poly_interact <- function(train_data) {
  lm(charges ~ age + I(age^2) +
       bmi + I(bmi^2) +
       children +
       sex +
       smoker +
       region +
       smoker:bmi +
       smoker:age,
     data = train_data)
}

# Full model with log transform + poly + interactions
fit_model_full <- function(train_data) {
  lm(log(charges) ~ age + I(age^2) +
       bmi + I(bmi^2) +
       children +
       sex +
       smoker +
       region +
       smoker:bmi +
       smoker:age,
     data = train_data)
}

# ================================================
# MODEL 4: STEPWISE SELECTION (AIC/BIC)
# ================================================
# Starts with full model and uses backward elimination

# Stepwise with AIC
fit_model_stepwise_aic <- function(train_data, trace = FALSE) {
  # Start with full polynomial + interaction model
  full_model <- lm(charges ~ age + I(age^2) +
                     bmi + I(bmi^2) +
                     children +
                     sex +
                     smoker +
                     region +
                     smoker:bmi +
                     smoker:age,
                   data = train_data)

  # Backward stepwise selection using AIC
  step_model <- stepAIC(full_model, direction = "both", trace = trace)

  return(step_model)
}

# Stepwise with BIC (more parsimonious)
fit_model_stepwise_bic <- function(train_data, trace = FALSE) {
  # Start with full polynomial + interaction model
  full_model <- lm(charges ~ age + I(age^2) +
                     bmi + I(bmi^2) +
                     children +
                     sex +
                     smoker +
                     region +
                     smoker:bmi +
                     smoker:age,
                   data = train_data)

  n <- nrow(train_data)

  # Backward stepwise selection using BIC
  step_model <- stepAIC(full_model, direction = "both",
                        k = log(n), trace = trace)

  return(step_model)
}

# Stepwise on log-transformed target
fit_model_stepwise_log <- function(train_data, trace = FALSE) {
  full_model <- lm(log(charges) ~ age + I(age^2) +
                     bmi + I(bmi^2) +
                     children +
                     sex +
                     smoker +
                     region +
                     smoker:bmi +
                     smoker:age,
                   data = train_data)

  step_model <- stepAIC(full_model, direction = "both", trace = trace)

  return(step_model)
}

# ================================================
# FIT ALL MODELS
# ================================================
# Convenience function to fit all models at once

fit_all_models <- function(train_data, verbose = TRUE) {
  models <- list()

  if (verbose) cat("Fitting Model 1: Baseline MLR...\n")
  models$baseline <- fit_model_baseline(train_data)

  if (verbose) cat("Fitting Model 2: Log-transformed MLR...\n")
  models$log_transform <- fit_model_log(train_data)

  if (verbose) cat("Fitting Model 3: Polynomial + Interactions...\n")
  models$poly_interact <- fit_model_poly_interact(train_data)

  if (verbose) cat("Fitting Model 4a: Stepwise AIC...\n")
  models$stepwise_aic <- fit_model_stepwise_aic(train_data, trace = FALSE)

  if (verbose) cat("Fitting Model 4b: Stepwise BIC...\n")
  models$stepwise_bic <- fit_model_stepwise_bic(train_data, trace = FALSE)

  if (verbose) cat("Fitting Model 5: Full (log + poly + interactions)...\n")
  models$full_log <- fit_model_full(train_data)

  if (verbose) cat("Fitting Model 6: Stepwise on log target...\n")
  models$stepwise_log <- fit_model_stepwise_log(train_data, trace = FALSE)

  if (verbose) cat("\nAll models fitted successfully!\n")

  return(models)
}

# ================================================
# TIDYMODELS WORKFLOW SPECS (for CV)
# ================================================

# Model specifications for tidymodels
spec_linear_reg <- function() {
  linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
}

spec_ridge <- function(penalty = 0.01) {
  linear_reg(penalty = penalty, mixture = 0) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
}

spec_lasso <- function(penalty = 0.01) {
  linear_reg(penalty = penalty, mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
}

spec_elastic_net <- function(penalty = 0.01, mixture = 0.5) {
  linear_reg(penalty = penalty, mixture = mixture) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
}

# ================================================
# WORKFLOW BUILDERS
# ================================================

build_workflow <- function(recipe, spec) {
  workflow() %>%
    add_recipe(recipe) %>%
    add_model(spec)
}

# Basic workflow (for backward compatibility)
build_insurance_workflow <- function(df) {
  rec <- build_recipe_basic(df)
  spec <- spec_linear_reg()
  build_workflow(rec, spec)
}

# Workflow with interactions
build_workflow_interactions <- function(df) {
  rec <- build_recipe_interactions(df)
  spec <- spec_linear_reg()
  build_workflow(rec, spec)
}

# Build multiple workflows for CV comparison
build_workflow_set <- function(df) {
  rec_basic <- build_recipe_basic(df)
  rec_interact <- build_recipe_interactions(df)

  list(
    lm_basic = build_workflow(rec_basic, spec_linear_reg()),
    lm_interactions = build_workflow(rec_interact, spec_linear_reg()),
    ridge = build_workflow(rec_basic, spec_ridge()),
    lasso = build_workflow(rec_basic, spec_lasso()),
    elastic_net = build_workflow(rec_basic, spec_elastic_net())
  )
}

# ================================================
# TEST SET EVALUATION
# ================================================
# Evaluate lm models on test data

evaluate_on_test <- function(model, test_data, log_model = FALSE) {
  if (log_model) {
    # For log-transformed models
    pred <- exp(predict(model, newdata = test_data))
  } else {
    pred <- predict(model, newdata = test_data)
  }

  actual <- test_data$charges

  data.frame(
    rmse = sqrt(mean((actual - pred)^2)),
    mae = mean(abs(actual - pred)),
    mape = mean(abs((actual - pred) / actual)) * 100,
    r2 = 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
  )
}

# Evaluate all models on test set
evaluate_all_on_test <- function(model_list, test_data) {
  # Identify which models use log transform
  log_models <- c("log_transform", "full_log", "stepwise_log")

  results <- lapply(names(model_list), function(nm) {
    is_log <- nm %in% log_models
    metrics <- evaluate_on_test(model_list[[nm]], test_data, log_model = is_log)
    metrics$model <- nm
    metrics
  })

  comparison <- do.call(rbind, results)
  comparison <- comparison[, c("model", "rmse", "mae", "mape", "r2")]
  comparison <- comparison[order(comparison$rmse), ]
  rownames(comparison) <- NULL

  return(comparison)
}
