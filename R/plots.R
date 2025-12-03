## ==============================================
## plots.R - Project Visualization Functions
## ==============================================

library(ggplot2)
library(dplyr)

# ------------------------------------------------
# Plot 1: Charges by Smoking Status
# ------------------------------------------------
plot_charges_by_smoker <- function(df) {
  ggplot(df, aes(x = smoker, y = charges, fill = smoker)) +
    geom_boxplot(alpha = 0.8) +
    labs(
      title = "Charges by Smoking Status",
      x = "Smoking Status",
      y = "Insurance Charges"
    ) +
    theme_minimal()
}

# ------------------------------------------------
# Plot 2: Charges vs BMI with Smoker Color
# ------------------------------------------------
plot_charges_vs_bmi <- function(df) {
  ggplot(df, aes(x = bmi, y = charges, color = smoker)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess") +
    labs(
      title = "Charges vs BMI",
      x = "BMI",
      y = "Charges"
    ) +
    theme_minimal()
}

# ------------------------------------------------
# Plot 3: Region Average Charges
# ------------------------------------------------
plot_region_charges <- function(df) {
  df_summary <- df %>%
    group_by(region) %>%
    summarize(avg_charge = mean(charges))
  
  ggplot(df_summary, aes(x = region, y = avg_charge, fill = region)) +
    geom_col() +
    labs(
      title = "Average Charges by Region",
      x = "Region",
      y = "Average Charges"
    ) +
    theme_minimal()
}

# ------------------------------------------------
# Plot 4: Residual Plot (requires model)
# ------------------------------------------------
plot_residuals <- function(model) {
  df <- data.frame(
    fitted = fitted(model),
    residuals = resid(model)
  )
  
  ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red") +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal()
}

