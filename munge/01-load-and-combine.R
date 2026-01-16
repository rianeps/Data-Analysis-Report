# Munge Script 01: Load and combine key raw data across all course runs
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)

#helpers
parse_utc <- function(x) {
    # Convert "" to NA, strip trailing " UTC", then parse
    x <- na_if(x, "")
    x <- str_replace(x, " UTC$", "")
    ymd_hms(x, tz = "UTC")
}

course_runs <- paste0("cyber-security-", 1:7)

#1) Step activity
step_activity_all <- map_dfr(course_runs, \(run) {
    file_path <- file.path("Data", paste0(run, "_step-activity.csv"))
    if (!file.exists(file_path)) return(NULL)

    read_csv(file_path, show_col_types = FALSE) |>
        mutate(course_run = run)
}) |>
    mutate(
        first_visited_at  = parse_utc(first_visited_at),
        last_completed_at = parse_utc(last_completed_at),
        step_completed    = !is.na(last_completed_at)
    )

# 2) Enrolments
enrolments_all <- map_dfr(course_runs, \(run) {
    file_path <- file.path("Data", paste0(run, "_enrolments.csv"))
    if (!file.exists(file_path)) return(NULL)

    read_csv(file_path, show_col_types = FALSE) |>
        mutate(course_run = run)
}) |>
    mutate(
        enrolled_at            = parse_utc(enrolled_at),
        unenrolled_at          = parse_utc(unenrolled_at),
        fully_participated_at  = parse_utc(fully_participated_at),
        purchased_statement_at = parse_utc(purchased_statement_at)
    )

# 3) checking
stopifnot(all(c("learner_id", "step", "week_number", "step_number") %in% names(step_activity_all)))
stopifnot(all(c("learner_id", "enrolled_at", "course_run") %in% names(enrolments_all)))

inactive_summary <- tibble(
    enrolled_learners = n_distinct(enrolments_all$learner_id),
    active_learners   = n_distinct(step_activity_all$learner_id)
) |>
    mutate(never_active = enrolled_learners - active_learners)

dup_groups_step_activity <- step_activity_all |>
    count(course_run, learner_id, step) |>
    filter(n > 1)

stopifnot(is.data.frame(step_activity_all))
stopifnot(is.data.frame(enrolments_all))

