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

plot_charges_vs_bmi <- function(df) {
  ggplot(df, aes(x = bmi, y = charges, color = smoker)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(
      title = "Charges vs BMI",
      x = "BMI",
      y = "Insurance Charges",
      color = "Smoker"
    ) +
    theme_minimal()
}

plot_region_charges <- function(df) {
  ggplot(df, aes(x = region, y = charges, fill = region)) +
    geom_boxplot(alpha = 0.8) +
    labs(
      title = "Charges by Region",
      x = "Region",
      y = "Insurance Charges"
    ) +
    theme_minimal()
}
