# ================================================
# data_load.R - Data Import and Preprocessing
# ================================================
# Loads the insurance dataset and prepares it for analysis.
# Includes data quality checks and validation.
# ================================================

library(rsample)

# ------------------------------------------------
# Basic Data Loader
# ------------------------------------------------
load_insurance_data <- function(path = "data/insurance.csv") {
  # Read CSV with factors for categorical vars
  df <- read.csv(path, stringsAsFactors = TRUE)

  # Basic validation - make sure we got the expected columns
  expected_cols <- c("age", "sex", "bmi", "children", "smoker", "region", "charges")
  if (!all(expected_cols %in% names(df))) {
    stop("Missing expected columns in insurance data")
  }

  return(df)
}
#--------------------------------------------
# Main preprocessing function
#--------------------------------------------

preprocess_insurance <- function(path = "data/insurance.csv") {
  # Read raw CSV
  insurance_raw <- read.csv(path, stringsAsFactors = FALSE)
  
  # Optional: check for empty or NA cells and report them
  positions <- find_empty_cells(insurance_raw)
  if (nrow(positions) > 0) {
    message("Warning: found empty or NA cells in the raw data.")
    print(positions)
    # you can decide whether to stop() here instead
  }
  
  # Basic type conversions + encoding
  insurance_clean <- within(insurance_raw, {
    # numeric / integer
    age      <- as.numeric(age)
    bmi      <- as.numeric(bmi)
    children <- as.integer(children)
    charges  <- as.numeric(charges)
    
    # categorical as factors
    sex      <- factor(sex)      # male/female
    smoker   <- factor(smoker)   # yes/no
    region   <- factor(region)   # northeast/northwest/...
  })
  
  insurance_clean
}

# ------------------------------------------------
# Data Quality Report
# ------------------------------------------------
# Generates a comprehensive data quality summary for methodology section
data_quality_report <- function(df) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("            DATA QUALITY REPORT\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Basic dimensions
  cat("DIMENSIONS\n")
  cat(sprintf("  Observations: %d\n", nrow(df)))
  cat(sprintf("  Variables:    %d\n", ncol(df)))

  # Column types
  cat("\nVARIABLE TYPES\n")
  types <- sapply(df, class)
  type_counts <- table(types)
  for (t in names(type_counts)) {
    cat(sprintf("  %s: %d\n", t, type_counts[t]))
  }

  # Missing values
  cat("\nMISSING VALUES\n")
  missing <- colSums(is.na(df))
  total_missing <- sum(missing)
  if (total_missing == 0) {
    cat("  No missing values detected\n")
  } else {
    for (col in names(missing[missing > 0])) {
      cat(sprintf("  %s: %d (%.1f%%)\n", col, missing[col],
                  100 * missing[col] / nrow(df)))
    }
  }

  # Duplicates
  cat("\nDUPLICATE ROWS\n")
  n_dup <- sum(duplicated(df))
  cat(sprintf("  Duplicate rows: %d (%.1f%%)\n", n_dup, 100 * n_dup / nrow(df)))

  # Categorical variable levels
  cat("\nCATEGORICAL VARIABLES\n")
  cat_vars <- names(df)[sapply(df, is.factor)]
  for (v in cat_vars) {
    levels_str <- paste(levels(df[[v]]), collapse = ", ")
    cat(sprintf("  %s: %d levels (%s)\n", v, nlevels(df[[v]]), levels_str))
  }

  # Numeric variable ranges
  cat("\nNUMERIC VARIABLES (Range)\n")
  num_vars <- names(df)[sapply(df, is.numeric)]
  for (v in num_vars) {
    cat(sprintf("  %s: [%.2f, %.2f]\n", v, min(df[[v]]), max(df[[v]])))
  }

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")

  # Return summary as list for programmatic use
  invisible(list(
    n_rows = nrow(df),
    n_cols = ncol(df),
    missing = missing,
    n_duplicates = n_dup,
    var_types = types
  ))
}

# ------------------------------------------------
# Train/Test Split
# ------------------------------------------------
# Creates stratified train/test split (default 80/20)
# Stratifies on smoker to maintain class balance
create_train_test_split <- function(df, prop = 0.8, seed = 123) {
  set.seed(seed)

  split <- rsample::initial_split(df, prop = prop, strata = smoker)

  list(
    train = rsample::training(split),
    test  = rsample::testing(split),
    split = split
  )
}

# ------------------------------------------------
# Cross-Validation Folds
# ------------------------------------------------
# Creates k-fold CV splits (default 10-fold)
# Stratifies on smoker for balanced folds
create_cv_folds <- function(df, v = 10, seed = 123) {
  set.seed(seed)
  rsample::vfold_cv(df, v = v, strata = smoker)
}

# ------------------------------------------------
# Full Preprocessing Pipeline
# ------------------------------------------------
# Loads, cleans, and splits data in one call
prepare_modeling_data <- function(path = "data/insurance.csv",
                                   train_prop = 0.8,
                                   cv_folds = 10,
                                   seed = 123) {
  # Load and preprocess
  df <- preprocess_insurance(path)

  # Create train/test split
  splits <- create_train_test_split(df, prop = train_prop, seed = seed)

  # Create CV folds on training data
  cv <- create_cv_folds(splits$train, v = cv_folds, seed = seed)

  cat("Data preparation complete:\n")
  cat("  - Total observations:", nrow(df), "\n")
  cat("  - Training set:", nrow(splits$train), "\n")
  cat("  - Test set:", nrow(splits$test), "\n")
  cat("  - CV folds:", cv_folds, "\n")

  list(
    full_data = df,
    train     = splits$train,
    test      = splits$test,
    split     = splits$split,
    cv_folds  = cv
  )
}
