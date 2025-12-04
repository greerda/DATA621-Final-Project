
library(tidymodels)

build_insurance_recipe <- function(df) {
  recipe(charges ~ ., data = df) %>%
    step_string2factor(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors(), -all_outcomes())
}

build_insurance_lm_spec <- function() {
  linear_reg() %>%
    set_engine("lm")
}

build_insurance_workflow <- function(df) {
  rec  <- build_insurance_recipe(df)
  spec <- build_insurance_lm_spec()
  
  workflow() %>%
    add_model(spec) %>%
    add_recipe(rec)
}
