## ================================
## Exploratory Data Analysis (EDA)
## ================================
## Data: insurance.csv
## Assumes data is already loaded into `insurance`
## from R/data_load.R
## ================================

library(dplyr)
library(ggplot2)
library(skimr)
library(reshape2)

# --------------------------------
# Basic Structure
# --------------------------------
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

# --------------------------------
# Univariate Distributions
# --------------------------------
eda_univariate <- function(insurance) {
  
  plots <- list()
  
  # Numeric variables
  numeric_vars <- c("age", "bmi", "children", "charges")
  
  for (v in numeric_vars) {
    p <- ggplot(insurance, aes_string(x = v)) +
      geom_histogram(bins = 30, alpha = 0.7) +
      labs(title = paste("Distribution of", v),
           x = v,
           y = "Count") +
      theme_minimal()
    
    print(p)  # show in Viewer
    plots[[paste0("univariate_hist_", v)]] <- p
  }
  
  # Categorical variables
  cat_vars <- c("sex", "smoker", "region")
  
  for (v in cat_vars) {
    p <- ggplot(insurance, aes_string(x = v)) +
      geom_bar(alpha = 0.8) +
      labs(title = paste("Counts of", v),
           x = v,
           y = "Count") +
      theme_minimal()
    
    print(p)
    plots[[paste0("univariate_bar_", v)]] <- p
  }
  
  return(plots)
}

# --------------------------------
# Bivariate Distributions
# --------------------------------
eda_bivariate <- function(insurance) {
  
  plots <- list()
  
  # Charges vs numeric predictors
  numeric_vars <- c("age", "bmi", "children")
  
  for (v in numeric_vars) {
    p <- ggplot(insurance, aes_string(x = v, y = "charges")) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE) +
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
    p <- ggplot(insurance, aes_string(x = v, y = "charges")) +
      geom_boxplot() +
      labs(title = paste("Charges by", v),
           x = v,
           y = "Charges") +
      theme_minimal()
    
    print(p)
    plots[[paste0("bivariate_charges_by_", v)]] <- p
  }
  
  return(plots)
}

# --------------------------------
# Correlations
# --------------------------------
eda_correlation <- function(insurance) {
  
  numeric_df <- insurance %>% select(age, bmi, children, charges)
  
  corr_matrix <- round(cor(numeric_df), 3)
  print(corr_matrix)
  
  melted <- melt(corr_matrix)
  
  p <- ggplot(melted, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = value)) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
    labs(title = "Correlation Heatmap") +
    theme_minimal()
  
  print(p)
  return(p)
}

# --------------------------------
# Master Function that Runs All EDA
# --------------------------------
run_eda <- function(insurance) {
  eda_summary(insurance)
  
  uni_plots <- eda_univariate(insurance)
  bi_plots  <- eda_bivariate(insurance)
  corr_plot <- eda_correlation(insurance)
  
  # return everything in a list
  list(
    uni_plots  = uni_plots,
    bi_plots   = bi_plots,
    corr_plot  = corr_plot
  )
}
