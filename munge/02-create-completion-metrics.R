stopifnot(is.data.frame(step_activity_all))

# Munge Script 02: Create completion flag + learner-level engagement metrics (Cycle 1)
library(dplyr)
library(tidyr)

# 1) De-duplicate learner-step records
step_activity_clean <- step_activity_all |>
    mutate(last_touch = coalesce(last_completed_at, first_visited_at)) |>
    group_by(course_run, learner_id, step) |>
    arrange(desc(last_touch)) |>
    slice(1) |>
    ungroup() |>
    select(-last_touch)

# 2) Learner-level engagement metrics
learner_engagement <- step_activity_clean |>
    mutate(last_touch = coalesce(last_completed_at, first_visited_at)) |>
    group_by(course_run, learner_id) |>
    summarise(
        total_steps_visited   = n(),
        total_steps_completed = sum(step_completed, na.rm = TRUE),

        first_activity = min(first_visited_at, na.rm = TRUE),
        last_activity  = max(last_touch, na.rm = TRUE),

        num_weeks_active = n_distinct(week_number),
        max_week_reached = max(week_number, na.rm = TRUE),

        avg_steps_per_week = total_steps_visited / num_weeks_active,

        .groups = "drop"
    ) |>
    mutate(
        engagement_duration_days = as.numeric(difftime(last_activity, first_activity, units = "days"))
    )

# 3) Total steps per run
steps_per_run <- step_activity_clean |>
    distinct(course_run, step) |>
    count(course_run, name = "total_steps_in_course")

# 4) Build learner-level dataset
learner_summary <- enrolments_all |>
    select(
        learner_id, course_run, enrolled_at, fully_participated_at,
        gender, country, age_range, highest_education_level,
        employment_status, employment_area
    ) |>
    left_join(learner_engagement, by = c("course_run", "learner_id")) |>
    left_join(steps_per_run,      by = "course_run") |>
    mutate(
        # Completion definition
        completed = !is.na(fully_participated_at),

        # For enrolled-but-inactive learners
        total_steps_visited   = replace_na(total_steps_visited, 0L),
        total_steps_completed = replace_na(total_steps_completed, 0L),
        num_weeks_active      = replace_na(num_weeks_active, 0L),
        max_week_reached      = replace_na(max_week_reached, 0L),

        # If no activity, these stay NA
        avg_steps_per_week = if_else(num_weeks_active > 0, avg_steps_per_week, NA_real_),

        completion_percentage = if_else(
            total_steps_in_course > 0,
            100 * total_steps_completed / total_steps_in_course,
            NA_real_
        ),

        # Clean demographics (Unknown -> NA)
        gender                  = na_if(gender, "Unknown"),
        country                 = na_if(country, "Unknown"),
        age_range               = na_if(age_range, "Unknown"),
        highest_education_level = na_if(highest_education_level, "Unknown"),
        employment_status       = na_if(employment_status, "Unknown"),
        employment_area         = na_if(employment_area, "Unknown")
    )

learner_summary_active <- learner_summary |>
    filter(total_steps_visited > 0)

#5) Cached summaries
completion_overall <- learner_summary |>
    summarise(
        n_enrolled      = n(),
        n_active        = sum(total_steps_visited > 0),
        n_completed     = sum(completed),
        completion_rate = mean(completed)
    )

engagement_by_completion <- learner_summary_active |>
    group_by(completed) |>
    summarise(
        n = n(),
        mean_steps_completed   = mean(total_steps_completed),
        median_steps_completed = median(total_steps_completed),
        mean_weeks_active      = mean(num_weeks_active),
        .groups = "drop"
    )


