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
# Distribution Statistics (for transformation decisions)
# ------------------------------------------------
# Computes skewness, kurtosis, and normality tests
# Returns a data frame suitable for report tables
eda_distribution_stats <- function(insurance) {
  numeric_vars <- c("age", "bmi", "children", "charges")

  # Helper functions
  calc_skewness <- function(x) {
    n <- length(x)
    m <- mean(x)
    s <- sd(x)
    sum((x - m)^3) / (n * s^3)
  }

  calc_kurtosis <- function(x) {
    n <- length(x)
    m <- mean(x)
    s <- sd(x)
    sum((x - m)^4) / (n * s^4) - 3  # excess kurtosis
  }

  stats_list <- lapply(numeric_vars, function(v) {
    x <- insurance[[v]]

    # Shapiro-Wilk test (sample if n > 5000)
    if (length(x) > 5000) {
      set.seed(123)
      x_sample <- sample(x, 5000)
    } else {
      x_sample <- x
    }
    shapiro_p <- shapiro.test(x_sample)$p.value

    data.frame(
      variable = v,
      mean     = round(mean(x), 2),
      sd       = round(sd(x), 2),
      median   = round(median(x), 2),
      min      = round(min(x), 2),
      max      = round(max(x), 2),
      skewness = round(calc_skewness(x), 3),
      kurtosis = round(calc_kurtosis(x), 3),
      shapiro_p = round(shapiro_p, 4),
      normal   = ifelse(shapiro_p > 0.05, "Yes", "No")
    )
  })

  stats_df <- do.call(rbind, stats_list)

  cat("\n----- Distribution Statistics -----\n")
  print(stats_df)

  cat("\nInterpretation:\n")
  cat("  - Skewness: |value| > 1 indicates high skew (consider log transform)\n")
  cat("  - Kurtosis: |value| > 2 indicates heavy tails\n")
  cat("  - Shapiro p < 0.05 rejects normality\n")

  return(stats_df)
}

# ------------------------------------------------
# Compare Original vs Log-Transformed Distribution
# ------------------------------------------------
# Shows before/after stats for charges (key for methodology)
eda_transformation_comparison <- function(insurance) {
  charges <- insurance$charges
  log_charges <- log(charges)

  calc_skewness <- function(x) {
    n <- length(x)
    m <- mean(x)
    s <- sd(x)
    sum((x - m)^3) / (n * s^3)
  }

  comparison <- data.frame(
    metric = c("Mean", "Median", "SD", "Skewness", "Min", "Max"),
    original = c(
      round(mean(charges), 2),
      round(median(charges), 2),
      round(sd(charges), 2),
      round(calc_skewness(charges), 3),
      round(min(charges), 2),
      round(max(charges), 2)
    ),
    log_transformed = c(
      round(mean(log_charges), 2),
      round(median(log_charges), 2),
      round(sd(log_charges), 2),
      round(calc_skewness(log_charges), 3),
      round(min(log_charges), 2),
      round(max(log_charges), 2)
    )
  )

  cat("\n----- Charges: Original vs Log-Transformed -----\n")
  print(comparison)

  return(comparison)
}

# ------------------------------------------------
# QQ Plots for Normality Assessment
# ------------------------------------------------
eda_qq_plots <- function(insurance) {
  plots <- list()

  # Original charges
  p1 <- ggplot(insurance, aes(sample = charges)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Q-Q Plot: Charges (Original)",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()

  # Log-transformed charges
  p2 <- ggplot(insurance, aes(sample = log(charges))) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Q-Q Plot: Log(Charges)",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()

  print(p1)
  print(p2)

  plots$qq_charges_original <- p1
  plots$qq_charges_log <- p2

  return(plots)
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
# Interaction Effect Visualization
# ------------------------------------------------
# Key interactions: smoker:bmi, smoker:age (visible in EDA)
eda_interactions <- function(insurance) {
  plots <- list()

  # Smoker x BMI interaction on charges
  p1 <- ggplot(insurance, aes(x = bmi, y = charges, color = smoker)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Interaction: Smoker x BMI on Charges",
         subtitle = "Different slopes suggest interaction effect",
         x = "BMI", y = "Charges") +
    theme_minimal() +
    scale_color_manual(values = c("no" = "#377EB8", "yes" = "#E41A1C"))

  # Smoker x Age interaction on charges
  p2 <- ggplot(insurance, aes(x = age, y = charges, color = smoker)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Interaction: Smoker x Age on Charges",
         subtitle = "Different slopes suggest interaction effect",
         x = "Age", y = "Charges") +
    theme_minimal() +
    scale_color_manual(values = c("no" = "#377EB8", "yes" = "#E41A1C"))

  print(p1)
  print(p2)

  plots$interaction_smoker_bmi <- p1
  plots$interaction_smoker_age <- p2

  return(plots)
}

# ------------------------------------------------
# Generate Summary Table for Report Appendix
# ------------------------------------------------
# Creates a formatted table of all numeric summaries
eda_summary_table <- function(insurance) {
  numeric_vars <- c("age", "bmi", "children", "charges")
  cat_vars <- c("sex", "smoker", "region")

  # Numeric summaries
  numeric_summary <- do.call(rbind, lapply(numeric_vars, function(v) {
    x <- insurance[[v]]
    data.frame(
      Variable = v,
      N = length(x),
      Mean = round(mean(x), 2),
      SD = round(sd(x), 2),
      Median = round(median(x), 2),
      Min = round(min(x), 2),
      Max = round(max(x), 2)
    )
  }))

  # Categorical summaries
  cat_summary <- do.call(rbind, lapply(cat_vars, function(v) {
    tbl <- table(insurance[[v]])
    data.frame(
      Variable = v,
      Levels = paste(names(tbl), collapse = ", "),
      Counts = paste(as.vector(tbl), collapse = ", "),
      Mode = names(which.max(tbl))
    )
  }))

  list(
    numeric = numeric_summary,
    categorical = cat_summary
  )
}

# ------------------------------------------------
# Master Function - Runs All EDA
# ------------------------------------------------
run_eda <- function(insurance) {
  # Basic summary
  eda_summary(insurance)

  # Distribution statistics (justifies transformations)
  dist_stats <- eda_distribution_stats(insurance)
  transform_comparison <- eda_transformation_comparison(insurance)

  # Outlier detection
  outlier_info <- eda_outliers(insurance)

  # Visualizations
  uni_plots    <- eda_univariate(insurance)
  bi_plots     <- eda_bivariate(insurance)
  qq_plots     <- eda_qq_plots(insurance)
  corr_plot    <- eda_correlation(insurance)
  interaction_plots <- eda_interactions(insurance)
  pair_plot    <- eda_pairplot(insurance)

  # Summary tables for report
  summary_tables <- eda_summary_table(insurance)

  # Return everything for downstream use
  list(
    uni_plots       = uni_plots,
    bi_plots        = bi_plots,
    qq_plots        = qq_plots,
    corr_plot       = corr_plot,
    pair_plot       = pair_plot,
    interaction_plots = interaction_plots,
    outlier_info    = outlier_info,
    dist_stats      = dist_stats,
    transform_comparison = transform_comparison,
    summary_tables  = summary_tables
  )
}
