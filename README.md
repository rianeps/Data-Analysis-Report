# Predicting MOOC Completion from Early Engagement Patterns

Analysis of FutureLearn Cyber Security course data to predict completion from early engagement patterns.

The aim is to identify:
Patterns of learner engagement and completion across course runs (Cycle 1)
Whether Week 1 engagement predicts overall course completion (Cycle 2)
The analysis follows the CRISP-DM framework and is implemented using ProjectTemplate.

## Project Structure

```
Data Analysis Report_v2/
├── config/                 # ProjectTemplate configuration
│   └── global.dcf
├── data/                   # Raw CSV data 
├── munge/                  # Data preparation and feature engineering
│   ├── 01-load-and-combine.R
│   ├── 02-create-completion-metrics.R
│   └── 03-create-week1-metrics.R
├── src/                    # Reusable analysis & plotting functions
│   └── visualization_functions.R
├── cache/                  # Cached intermediate objects for faster rendering
├── reports/                # RMarkdown report and knitted outputs
│   └── Data_Analysis_Report.Rmd
├── renv/                   # Reproducible package environment
├── renv.lock               # Locked package versions
├── Data Analysis Report_v2.Rproj
└── README.md

```

## Requirements

- R 4.0+
- RStudio (recommended)
- LaTeX (for PDF generation)(recommended)

## Setup

```r
# Install renv if needed
install.packages("renv")

# Restore dependencies
renv::restore()
```
### Windows users (Rtools)
On Windows systems, some packages (e.g. `purrr`) may require compilation.
If `renv::restore()` fails with an error such as:

> "make not found"

please install **Rtools** appropriate for your R version:
https://cran.r-project.org/bin/windows/Rtools/

After installing Rtools, restart R and re-run:

```r
renv::restore()


## Usage

### Generate Report

Knit the report 
Open `reports/Data_Analysis_Report.Rmd`
Click Knit
This will:
Load required objects (from cache and/or munge scripts)
Generate all figures and tables
Produce the final analytical report

### Run Analysis

```r
library(ProjectTemplate)
load.project()

# Access processed data
head(learner_summary)
head(completion_overall)
```

## Key Findings

- Week 1 engagement predicts completion with 25× difference between high/low engagers
- 7.6% overall completion rate
- Learners completing <40% of Week 1 are at high dropout risk

## Author

Nepoliyan Ria
