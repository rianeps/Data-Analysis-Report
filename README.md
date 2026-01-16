# MOOC Completion Prediction

Analysis of FutureLearn Cyber Security course data to predict completion from early engagement patterns.

## Project Structure

```
.
├── cache/              # Cached processed data (.RData files)
├── config/             # ProjectTemplate configuration
├── data/               # Raw CSV files (enrolments.csv, step-activity.csv)
├── munge/              # Data preprocessing scripts
├── src/                # Analysis functions
├── reports/            # R Markdown report
├── renv/               # Package dependency management
└── README.md
```

## Requirements

- R 4.0+
- RStudio (recommended)
- LaTeX (for PDF generation)

## Setup

```r
# Install renv if needed
install.packages("renv")

# Restore dependencies
renv::restore()
```

## Usage

### Generate Report

```r
# Load project
library(ProjectTemplate)
load.project()

# Knit report
rmarkdown::render("reports/MOOC_Analysis_Report.Rmd")
```

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