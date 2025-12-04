library(dplyr)
library(ggplot2)
library(reshape2)
library(tidymodels)
library(skimr)
library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(yardstick)
library(tune)
library(workflowsets)

source("R/utils.R")
source("R/data_load.R")
source("R/eda.R")
source("R/models.R")
source("R/plots.R")

# Load data
insurance <- load_insurance_data()
cat("Data loaded. Rows:", nrow(insurance), "Columns:", ncol(insurance), "\n")

# Quick peek
print(head(insurance))
str(insurance)

# --- Run EDA and collect plots ---
eda_results <- run_eda(insurance)

uni_plots    <- eda_results$uni_plots    # list of ggplots
bi_plots     <- eda_results$bi_plots     # list of ggplots
p_corr       <- eda_results$corr_plot    # single ggplot
p_pairs      <- eda_results$pair_plot    # pair plot matrix
outlier_info <- eda_results$outlier_info # outlier detection results

# --- Key report plots from plots.R ---
p1 <- plot_charges_by_smoker(insurance)
p2 <- plot_charges_vs_bmi(insurance)
p3 <- plot_region_charges(insurance)

print(p1)
print(p2)
print(p3)
print(p_corr)

# --- Create folders if needed ---
dir.create("plots", showWarnings = FALSE)
dir.create("plots/univariate", showWarnings = FALSE, recursive = TRUE)
dir.create("plots/bivariate", showWarnings = FALSE, recursive = TRUE)

# --- Save univariate plots ---
for (nm in names(uni_plots)) {
  ggsave(
    filename = file.path("plots/univariate", paste0(nm, ".png")),
    plot     = uni_plots[[nm]],
    width    = 7,
    height   = 5
  )
}

# --- Save bivariate plots ---
for (nm in names(bi_plots)) {
  ggsave(
    filename = file.path("plots/bivariate", paste0(nm, ".png")),
    plot     = bi_plots[[nm]],
    width    = 7,
    height   = 5
  )
}

# --- Save correlation heatmap ---
ggsave(
  "plots/correlation_heatmap.png",
  p_corr,
  width = 7,
  height = 6
)

# --- Save pair plot (base R plot, not ggplot) ---
png("plots/pair_plot.png", width = 800, height = 800)
eda_pairplot(insurance)
dev.off()

# --- Save key report plots ---
ggsave("plots/plot_region_charges.png",    p3, width = 7, height = 5)
ggsave("plots/plot_charges_vs_bmi.png",    p2, width = 7, height = 5)
ggsave("plots/plot_charges_by_smoker.png", p1, width = 7, height = 5)

# Optional: see what got written
print(list.files("plots", recursive = TRUE))

ins_workflow <- build_insurance_workflow(insurance)

ins_fit <- fit(ins_workflow, data = insurance)

print(ins_fit)

## ---- 5. Inspect model results ----
# Extract underlying lm object
lm_obj <- extract_fit_engine(ins_fit)
summary(lm_obj)

## ---- 6. Make predictions (example) ----
insurance_pred <- predict(ins_fit, new_data = insurance) %>%
  bind_cols(insurance)

head(insurance_pred)

## ---- 7. Save outputs (optional) ----
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
write.csv(insurance_pred, "outputs/insurance_predictions.csv", row.names = FALSE)
