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