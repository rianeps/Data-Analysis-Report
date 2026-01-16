# Munge Script 03: Week 1 engagement metrics (Cycle 2)
library(dplyr)
library(tidyr)

week1_activity <- step_activity_clean |>
    filter(week_number == 1) |>
    mutate(last_touch = coalesce(last_completed_at, first_visited_at))

week1_steps_per_run <- week1_activity |>
    distinct(course_run, step) |>
    count(course_run, name = "week1_total_steps")

week1_metrics <- week1_activity |>
    group_by(course_run, learner_id) |>
    summarise(
        week1_steps_visited   = n(),
        week1_steps_completed = sum(step_completed, na.rm = TRUE),
        week1_first_visit     = min(first_visited_at, na.rm = TRUE),
        week1_last_visit      = max(last_touch, na.rm = TRUE),
        week1_max_step        = max(step_number, na.rm = TRUE),
        .groups = "drop"
    ) |>
    left_join(week1_steps_per_run, by = "course_run") |>
    mutate(
        week1_completion_rate = if_else(
            week1_total_steps > 0,
            100 * week1_steps_completed / week1_total_steps,
            NA_real_
        ),
        week1_duration_days = as.numeric(difftime(week1_last_visit, week1_first_visit, units = "days")),
        week1_engagement_level = case_when(
            week1_completion_rate >= 75 ~ "High",
            week1_completion_rate >= 40 ~ "Medium",
            week1_completion_rate >= 10 ~ "Low",
            week1_completion_rate >= 0  ~ "Very Low",
            TRUE ~ NA_character_
        )
    )

learner_with_week1 <- learner_summary |>
    left_join(
        week1_metrics |>
            select(
                learner_id, course_run,
                week1_steps_visited, week1_steps_completed,
                week1_first_visit, week1_last_visit,
                week1_max_step, week1_total_steps,
                week1_completion_rate, week1_duration_days,
                week1_engagement_level
            ),
        by = c("learner_id", "course_run")
    ) |>
    mutate(
        week1_steps_visited       = replace_na(week1_steps_visited, 0L),
        week1_steps_completed     = replace_na(week1_steps_completed, 0L),
        week1_completion_rate     = replace_na(week1_completion_rate, 0),
        week1_engagement_level    = replace_na(week1_engagement_level, "None"),
        week1_engagement_level    = factor(
            week1_engagement_level,
            levels = c("None", "Very Low", "Low", "Medium", "High"),
            ordered = TRUE
        )
    )

learner_with_week1_active <- learner_with_week1 |>
    filter(week1_steps_visited > 0)

week1_summary <- learner_with_week1 |>
    summarise(
        n_enrolled   = n(),
        n_week1_active = sum(week1_steps_visited > 0),
        prop_week1_active = mean(week1_steps_visited > 0)
    )

week1_by_completion <- learner_with_week1_active |>
    group_by(completed) |>
    summarise(
        n = n(),
        mean_week1_steps_completed = mean(week1_steps_completed),
        median_week1_steps_completed = median(week1_steps_completed),
        mean_week1_completion_rate = mean(week1_completion_rate),
        .groups = "drop"
    )

completion_by_week1_level <- learner_with_week1_active |>
    group_by(week1_engagement_level) |>
    summarise(
        n = n(),
        completion_rate = mean(completed),
        .groups = "drop"
    )

