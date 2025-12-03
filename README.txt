Final Project – DATA 621 (Data Mining)

This project organizes all code, data, and outputs for the final assignment in CUNY SPS DATA 621.
Everything is structured to support reproducible analysis, modular R scripts, and clean reporting.

Project Structure

Final Project/
│
├── Final Project.Rproj        # RStudio project file
├── main.R                     # Main script – runs the entire workflow
│
├── R/                         # All modular R scripts
│   ├── utils.R                # Helper functions, shared utilities
│   ├── data_load.R            # Data import and preprocessing
│   ├── eda.R                  # Exploratory data analysis
│   ├── models.R               # Model building, tuning, and evaluation
│   └── plots.R                # Visualization functions
│
├── data/                      # Raw data files
│   └── insurance.csv          # Primary dataset
│
├── outputs/                   # Generated model objects, RDS files, diagnostics
│
├── reports/                   # Knitted reports (HTML, PDF, Word)
│
└── .Rproj.user/               # RStudio internal files (ignore)


How to Run the Project

1. Open the RStudio Project

Double-click:
	Final Project.Rproj

2. Run the Workflow

You can run the whole project from one line:

source("main.R")


main.R loads all scripts from /R, reads the data, performs EDA, fits models, and produces output files.

What Each Script Does

	R/utils.R

		General helper functions

		Shared utilities used by multiple scripts

	R/data_load.R

		Loads data/insurance.csv

		Cleans and formats variables

		Handles missing values or encoding tasks

	R/eda.R

		Summary statistics

		Visual exploration

		Checks for distributions, relationships, and anomalies

	R/models.R

		Builds regression or classification models

		Trains, tunes, and validates

		Outputs predictions and performance metrics

	R/plots.R

		All ggplot2-based charts

		Plot wrappers for EDA, model diagnostics, and final results

	Data
	
	data/insurance.csv

	This is the main dataset used for all modeling tasks.
	It is loaded through data_load.R—do not load it directly in the analysis scripts.

	Outputs

	Generated files appear in:

		outputs/

	This may include:

	Cleaned datasets

	Saved model objects

	Diagnostic plots

	Prediction files

	Reports

	Knitted R Markdown files go into:

		reports/

	This may include:

		Final project report

		Section summaries

		Model comparison write-ups

Testing the Connections

From the project root, you can verify everything is wired correctly:

	source("R/utils.R")
	source("R/data_load.R")
	source("R/eda.R")
	source("R/models.R")
	source("R/plots.R")

	insurance <- read.csv("data/insurance.csv")
	str(insurance)
