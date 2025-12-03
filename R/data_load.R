load_insurance_data <- function() {
  df <- read.csv("data/insurance.csv", stringsAsFactors = TRUE)
  return(df)
}
