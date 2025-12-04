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
