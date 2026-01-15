# ggplot helpers for the FutureLearn MOOC analysis

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(scales)

theme_mooc <- function() {
    theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", size = 14, hjust = 0),
            plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0),
            plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
            panel.grid.minor = element_blank(),
            legend.position = "top",
            axis.title = element_text(face = "bold")
        )
}

colors_completion <- c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")
colors_engagement <- c(
    "None" = "#95a5a6",
    "Very Low" = "#e74c3c",
    "Low" = "#e67e22",
    "Medium" = "#f39c12",
    "High" = "#2ecc71"
)

# Enrolled -> active -> completed
plot_completion_funnel <- function(completion_overall) {
    funnel_data <- tibble(
        stage = factor(c("Enrolled", "Active", "Completed"),
                       levels = c("Enrolled", "Active", "Completed")
        ),
        count = c(
            completion_overall$n_enrolled,
            completion_overall$n_active,
            completion_overall$n_completed
        )
    ) |>
        mutate(
            percentage = count / completion_overall$n_enrolled * 100,
            label = paste0(comma(count), "\n(", round(percentage, 1), "%)")
        )

    ggplot(funnel_data, aes(x = stage, y = count)) +
        geom_col(aes(fill = stage), width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = label), vjust = -0.5, size = 4, fontface = "bold") +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
        scale_fill_manual(values = c("#3498db", "#f39c12", "#2ecc71")) +
        labs(
            title = "Learner Engagement Funnel",
            subtitle = "Progression from enrollment to course completion",
            x = NULL,
            y = "Number of Learners",
            caption = "Active = completed at least one step"
        ) +
        theme_mooc()
}

# Steps completed (active learners), split by completion
plot_steps_distribution <- function(learner_summary_active, total_steps = NULL) {
    medians <- learner_summary_active |>
        group_by(completed) |>
        summarise(median_steps = median(total_steps_completed), .groups = "drop")

    p <- ggplot(learner_summary_active, aes(x = total_steps_completed, fill = completed)) +
        geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
        geom_vline(
            data = medians,
            aes(xintercept = median_steps, color = completed),
            linetype = "dashed", linewidth = 1
        ) +
        scale_fill_manual(
            values = colors_completion,
            name = "Completed:",
            labels = c("FALSE" = "No", "TRUE" = "Yes")
        ) +
        scale_color_manual(values = colors_completion, guide = "none") +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        labs(
            title = "Distribution of Steps Completed",
            subtitle = "Bimodal pattern: early dropouts vs. completers",
            x = "Total Steps Completed",
            y = "Number of Learners",
            caption = "Dashed lines show medians"
        ) +
        theme_mooc()

    if (!is.null(total_steps)) {
        p <- p +
            geom_vline(xintercept = total_steps, linetype = "dotted", color = "black", linewidth = 0.8) +
            annotate(
                "text",
                x = total_steps, y = Inf,
                label = paste("Total steps:", total_steps),
                hjust = 1.1, vjust = 1.5, size = 3, fontface = "italic"
            )
    }

    p
}

# Engagement metrics by completion
plot_engagement_comparison <- function(learner_summary_active) {
    comparison_data <- learner_summary_active |>
        select(completed, total_steps_completed, num_weeks_active, engagement_duration_days) |>
        pivot_longer(
            cols = c(total_steps_completed, num_weeks_active, engagement_duration_days),
            names_to = "metric",
            values_to = "value"
        ) |>
        mutate(
            metric = factor(metric,
                            levels = c("total_steps_completed", "num_weeks_active", "engagement_duration_days"),
                            labels = c("Steps Completed", "Weeks Active", "Duration (Days)")
            )
        )

    ggplot(comparison_data, aes(x = completed, y = value, fill = completed)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
        facet_wrap(~metric, scales = "free_y", ncol = 3) +
        scale_fill_manual(
            values = colors_completion,
            name = "Completed:",
            labels = c("FALSE" = "No", "TRUE" = "Yes")
        ) +
        scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
        labs(
            title = "Engagement Metrics by Completion Status",
            subtitle = "Completers show higher engagement across metrics",
            x = "Course Completed",
            y = NULL,
            caption = "Box = median/IQR; points = outliers"
        ) +
        theme_mooc() +
        theme(legend.position = "none")
}

# Retention by week (relative to Week 1 active learners)
plot_weekly_retention <- function(step_activity_clean) {
    retention_data <- step_activity_clean |>
        group_by(week_number) |>
        summarise(active_learners = n_distinct(learner_id), .groups = "drop") |>
        arrange(week_number) |>
        mutate(retention_rate = active_learners / first(active_learners) * 100)

    ggplot(retention_data, aes(x = week_number)) +
        geom_line(aes(y = retention_rate), color = "#3498db", linewidth = 1.2) +
        geom_point(aes(y = retention_rate), color = "#3498db", size = 3) +
        geom_area(aes(y = retention_rate), fill = "#3498db", alpha = 0.2) +
        scale_x_continuous(breaks = 1:max(retention_data$week_number)) +
        scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
        labs(
            title = "Learner Retention by Week",
            subtitle = "Percentage of Week 1 active learners remaining engaged",
            x = "Week Number",
            y = "Retention Rate (%)",
            caption = "Retention calculated relative to learners active in Week 1"
        ) +
        theme_mooc()
}

# Completion rate across demographic groups
plot_completion_by_demographic <- function(learner_summary, demographic_var = "age_range") {
    demo_summary <- learner_summary |>
        filter(!is.na(.data[[demographic_var]])) |>
        group_by(demographic = .data[[demographic_var]]) |>
        summarise(
            n = n(),
            completion_rate = mean(completed) * 100,
            .groups = "drop"
        ) |>
        filter(n >= 30) |>
        arrange(desc(completion_rate))

    var_labels <- c(
        age_range = "Age Range",
        gender = "Gender",
        highest_education_level = "Education Level",
        employment_status = "Employment Status"
    )

    ggplot(demo_summary, aes(x = reorder(demographic, completion_rate), y = completion_rate)) +
        geom_col(fill = "#3498db", alpha = 0.8) +
        geom_text(aes(label = paste0(round(completion_rate, 1), "%")), hjust = -0.2, size = 3.5) +
        geom_text(aes(label = paste0("n=", comma(n))), hjust = 1.1, size = 3, color = "white", fontface = "bold") +
        coord_flip() +
        scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
        labs(
            title = paste("Completion Rate by", var_labels[[demographic_var]]),
            subtitle = "Groups with fewer than 30 learners excluded",
            x = NULL,
            y = "Completion Rate (%)",
            caption = "Numbers inside bars show sample size"
        ) +
        theme_mooc()
}

# Week 1 engagement level distribution
plot_week1_distribution <- function(learner_with_week1) {
    week1_dist <- learner_with_week1 |>
        count(week1_engagement_level) |>
        mutate(
            percentage = n / sum(n) * 100,
            label = paste0(comma(n), "\n(", round(percentage, 1), "%)")
        )

    ggplot(week1_dist, aes(x = "", y = n, fill = week1_engagement_level)) +
        geom_col(width = 1) +
        geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold") +
        scale_fill_manual(values = colors_engagement, name = "Week 1 Engagement:") +
        labs(
            title = "Distribution of Week 1 Engagement Levels",
            subtitle = "How learners engaged in their first week",
            x = NULL,
            y = "Number of Learners",
            caption = "Levels based on Week 1 completion percentage"
        ) +
        theme_mooc() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

# Completion rate by Week 1 engagement level
plot_completion_by_week1 <- function(learner_with_week1_active) {
    week1_completion <- learner_with_week1_active |>
        group_by(week1_engagement_level) |>
        summarise(
            n = n(),
            n_completed = sum(completed),
            completion_rate = mean(completed) * 100,
            .groups = "drop"
        )

    ggplot(week1_completion, aes(x = week1_engagement_level, y = completion_rate, fill = week1_engagement_level)) +
        geom_col(alpha = 0.8, show.legend = FALSE) +
        geom_text(aes(label = paste0(round(completion_rate, 1), "%")), vjust = -0.5, size = 5, fontface = "bold") +
        geom_text(aes(label = paste0("n=", comma(n))), vjust = 1.5, size = 3.5, color = "white", fontface = "bold") +
        scale_fill_manual(values = colors_engagement) +
        scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
        labs(
            title = "Course Completion Rate by Week 1 Engagement",
            subtitle = "Higher Week 1 engagement predicts completion",
            x = "Week 1 Engagement Level",
            y = "Completion Rate (%)",
            caption = "Sample sizes shown inside bars"
        ) +
        theme_mooc()
}

# Completion rate by Week 1 steps completed (grouped)
plot_week1_scatter <- function(learner_with_week1_active, week1_total_steps) {
    step_summary <- learner_with_week1_active |>
        group_by(week1_steps_completed) |>
        summarise(
            n = n(),
            completion_rate = mean(completed) * 100,
            .groups = "drop"
        ) |>
        filter(n >= 10)

    ggplot(step_summary, aes(x = week1_steps_completed, y = completion_rate)) +
        geom_point(aes(size = n), alpha = 0.6, color = "#3498db") +
        geom_smooth(method = "loess", se = TRUE, color = "#e74c3c", linewidth = 1.2, fill = "#e74c3c", alpha = 0.2) +
        geom_vline(xintercept = week1_total_steps * 0.5, linetype = "dashed", color = "gray40") +
        annotate(
            "text",
            x = week1_total_steps * 0.5, y = 5,
            label = "50% of Week 1", hjust = -0.1, size = 3,
            fontface = "italic", color = "gray40"
        ) +
        scale_size_continuous(name = "Number of\nLearners:", range = c(2, 10)) +
        scale_x_continuous(breaks = seq(0, week1_total_steps, by = 5)) +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(
            title = "Week 1 Steps Completed vs. Course Completion Rate",
            subtitle = "Each point represents learners who completed X steps in Week 1",
            x = "Week 1 Steps Completed",
            y = "Overall Completion Rate (%)",
            caption = "Groups with 10+ learners; LOESS trend line"
        ) +
        theme_mooc()
}

# Week 1 vs overall completion percentage
plot_week1_vs_overall <- function(learner_with_week1_active) {
    comparison_data <- learner_with_week1_active |>
        mutate(week1_rate = week1_completion_rate, overall_rate = completion_percentage) |>
        select(learner_id, completed, week1_rate, overall_rate) |>
        pivot_longer(
            cols = c(week1_rate, overall_rate),
            names_to = "period",
            values_to = "completion_pct"
        ) |>
        mutate(
            period = factor(period,
                            levels = c("week1_rate", "overall_rate"),
                            labels = c("Week 1 Only", "Overall Course")
            )
        )

    ggplot(comparison_data, aes(x = completion_pct, fill = completed)) +
        geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
        facet_wrap(~period, ncol = 1) +
        scale_fill_manual(
            values = colors_completion,
            name = "Completed Course:",
            labels = c("FALSE" = "No", "TRUE" = "Yes")
        ) +
        scale_x_continuous(labels = label_percent(scale = 1)) +
        labs(
            title = "Week 1 Completion Rate vs. Overall Completion Rate",
            subtitle = "Completers show strong early engagement",
            x = "Completion Percentage",
            y = "Number of Learners",
            caption = "Completion % = steps completed / total steps available"
        ) +
        theme_mooc()
}

# Completion rate above Week 1 completion thresholds
plot_threshold_analysis <- function(learner_with_week1_active) {
    thresholds <- seq(0, 100, by = 10)

    threshold_data <- map_dfr(thresholds, function(threshold) {
        learner_with_week1_active |>
            filter(week1_completion_rate >= threshold) |>
            summarise(
                threshold = threshold,
                n = n(),
                completion_rate = mean(completed) * 100,
                .groups = "drop"
            )
    })

    overall_rate <- mean(learner_with_week1_active$completed) * 100

    ggplot(threshold_data, aes(x = threshold, y = completion_rate)) +
        geom_line(color = "#3498db", linewidth = 1.2) +
        geom_point(aes(size = n), color = "#3498db", alpha = 0.7) +
        geom_hline(yintercept = overall_rate, linetype = "dashed", color = "#e74c3c", linewidth = 1) +
        annotate(
            "text",
            x = 5, y = overall_rate,
            label = paste0("Overall rate: ", round(overall_rate, 1), "%"),
            vjust = -0.5, hjust = 0, size = 3.5, color = "#e74c3c",
            fontface = "italic"
        ) +
        scale_size_continuous(name = "Learners\nAbove\nThreshold:", range = c(3, 10)) +
        scale_x_continuous(labels = label_percent(scale = 1), breaks = seq(0, 100, 20)) +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(
            title = "Completion Rate by Week 1 Engagement Threshold",
            subtitle = "Completing more of Week 1 increases the chance of finishing",
            x = "Week 1 Completion Rate Threshold",
            y = "Course Completion Rate (%)",
            caption = "Point size indicates number of learners above each threshold"
        ) +
        theme_mooc()
}

# Format numeric columns for display (returns character columns)
format_summary_table <- function(data, digits = 2) {
    data |>
        mutate(across(where(is.numeric), ~round(.x, digits))) |>
        mutate(across(where(is.numeric), ~format(.x, big.mark = ",")))
}
