# ================================================
# eda.R - Exploratory Data Analysis
# ================================================
# Runs full EDA on the insurance dataset:
#   - Summary stats and structure
#   - Univariate distributions
#   - Bivariate relationships
#   - Correlation analysis
#   - Outlier detection (IQR method)
#   - Pair plots
# ================================================

library(dplyr)
library(ggplot2)
library(reshape2)

# ------------------------------------------------
# Basic Structure & Summary
# ------------------------------------------------
eda_summary <- function(insurance) {
  cat("----- Structure -----\n")
  print(str(insurance))

  cat("\n----- First Rows -----\n")
  print(head(insurance))

  cat("\n----- Summary Stats -----\n")
  print(summary(insurance))

  cat("\n----- Missing Values -----\n")
  print(colSums(is.na(insurance)))
}

# ------------------------------------------------
# Outlier Detection (IQR Method)
# ------------------------------------------------
# Flags observations outside 1.5*IQR from Q1/Q3
# Returns a list with outlier counts and flagged rows
eda_outliers <- function(insurance) {
  numeric_vars <- c("age", "bmi", "children", "charges")
  outlier_summary <- list()

  cat("\n----- Outlier Detection (IQR Method) -----\n")

  for (v in numeric_vars) {
    x <- insurance[[v]]
    q1 <- quantile(x, 0.25)
    q3 <- quantile(x, 0.75)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr

    outliers <- which(x < lower | x > upper)
    n_outliers <- length(outliers)

    outlier_summary[[v]] <- list(
      n_outliers = n_outliers,
      pct = round(100 * n_outliers / nrow(insurance), 2),
      lower_bound = lower,
      upper_bound = upper,
      indices = outliers
    )

    cat(sprintf("%s: %d outliers (%.2f%%) | bounds: [%.2f, %.2f]\n",
                v, n_outliers, outlier_summary[[v]]$pct, lower, upper))
  }

  return(outlier_summary)
}

# ------------------------------------------------
# Univariate Distributions
# ------------------------------------------------
# Histograms for numeric, bar charts for categorical
eda_univariate <- function(insurance) {
  plots <- list()

  # Numeric variables - histograms
  numeric_vars <- c("age", "bmi", "children", "charges")

  for (v in numeric_vars) {
    p <- ggplot(insurance, aes(x = .data[[v]])) +
      geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
      labs(title = paste("Distribution of", v),
           x = v,
           y = "Count") +
      theme_minimal()

    print(p)
    plots[[paste0("univariate_hist_", v)]] <- p
  }

  # Categorical variables - bar charts
  cat_vars <- c("sex", "smoker", "region")

  for (v in cat_vars) {
    p <- ggplot(insurance, aes(x = .data[[v]])) +
      geom_bar(fill = "steelblue", alpha = 0.8) +
      labs(title = paste("Counts of", v),
           x = v,
           y = "Count") +
      theme_minimal()

    print(p)
    plots[[paste0("univariate_bar_", v)]] <- p
  }

  return(plots)
}

# ------------------------------------------------
# Bivariate Distributions
# ------------------------------------------------
# Scatter plots for numeric, box plots for categorical
eda_bivariate <- function(insurance) {
  plots <- list()

  # Charges vs numeric predictors
  numeric_vars <- c("age", "bmi", "children")

  for (v in numeric_vars) {
    p <- ggplot(insurance, aes(x = .data[[v]], y = charges)) +
      geom_point(alpha = 0.5, color = "steelblue") +
      geom_smooth(method = "loess", se = FALSE, color = "darkred") +
      labs(title = paste("Charges vs", v),
           x = v,
           y = "Charges") +
      theme_minimal()

    print(p)
    plots[[paste0("bivariate_charges_vs_", v)]] <- p
  }

  # Charges vs categorical predictors
  cat_vars <- c("sex", "smoker", "region")

  for (v in cat_vars) {
    p <- ggplot(insurance, aes(x = .data[[v]], y = charges)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7) +
      labs(title = paste("Charges by", v),
           x = v,
           y = "Charges") +
      theme_minimal()

    print(p)
    plots[[paste0("bivariate_charges_by_", v)]] <- p
  }

  return(plots)
}

# ------------------------------------------------
# Correlation Heatmap
# ------------------------------------------------
eda_correlation <- function(insurance) {
  numeric_df <- insurance %>% select(age, bmi, children, charges)

  corr_matrix <- round(cor(numeric_df), 3)
  cat("\n----- Correlation Matrix -----\n")
  print(corr_matrix)

  melted <- melt(corr_matrix)

  p <- ggplot(melted, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = value), size = 4) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = "Correlation Heatmap",
         x = "", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p)
  return(p)
}

# ------------------------------------------------
# Pair Plot (base R version)
# ------------------------------------------------
# Scatterplot matrix colored by smoker status
# Uses base R pairs() - works without GGally dependency
eda_pairplot <- function(insurance) {
  cat("\n----- Generating Pair Plot -----\n")

  # Color by smoker status
  cols <- ifelse(insurance$smoker == "yes", "#E41A1C", "#377EB8")

  # Create pairs plot
  numeric_df <- insurance[, c("age", "bmi", "children", "charges")]

  pairs(
    numeric_df,
    col = cols,
    pch = 19,
    cex = 0.5,
    main = "Pair Plot: Numeric Variables (Red = Smoker)",
    lower.panel = NULL
  )

  # Return NULL since base pairs() doesn't return a ggplot object
  # The plot is saved separately in main.R using png()/dev.off()
  return(NULL)
}

# ------------------------------------------------
# Master Function - Runs All EDA
# ------------------------------------------------
run_eda <- function(insurance) {
  # Basic summary
  eda_summary(insurance)

  # Outlier detection
  outlier_info <- eda_outliers(insurance)

  # Visualizations
  uni_plots  <- eda_univariate(insurance)
  bi_plots   <- eda_bivariate(insurance)
  corr_plot  <- eda_correlation(insurance)
  pair_plot  <- eda_pairplot(insurance)

  # Return everything for downstream use
  list(
    uni_plots    = uni_plots,
    bi_plots     = bi_plots,
    corr_plot    = corr_plot,
    pair_plot    = pair_plot,
    outlier_info = outlier_info
  )
}
