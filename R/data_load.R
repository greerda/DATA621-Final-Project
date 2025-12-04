# ================================================
# data_load.R - Data Import and Preprocessing
# ================================================
# Loads the insurance dataset and prepares it for analysis.
# Categorical variables are converted to factors automatically.
# ================================================

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
