#!/usr/bin/env Rscript

# Observed-only publication weight trajectory plot.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(grid)
})

resolve_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)

  if (length(file_arg)) {
    candidate <- sub("^--file=", "", file_arg[1])
    candidate <- gsub("~+~", " ", candidate, fixed = TRUE)
    if (file.exists(candidate)) {
      return(normalizePath(candidate))
    }
  }

  ofile <- sys.frames()[[1]]$ofile
  if (!is.null(ofile) && file.exists(ofile)) {
    return(normalizePath(ofile))
  }

  normalizePath(file.path(getwd(), "08_make_weight_observed_trajectory_plot.R"), mustWork = FALSE)
}

script_path <- resolve_script_path()
pipeline_dir <- dirname(dirname(script_path))
data_dir <- file.path(pipeline_dir, "output", "data")
pub_plot_dir <- file.path(pipeline_dir, "output", "pub plots")

dir.create(pub_plot_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(file.path(data_dir, "weight_long.csv"))
stopifnot(all(file.exists(required_files)))

arm_levels <- c("Control", "Noom", "MTM")
arm_palette <- c("Control" = "#7A7A7A", "Noom" = "#2E7D32", "MTM" = "#E68613")

save_plot_high_res <- function(plot_object, stem, width = 7.2, height = 4.4, dpi = 900) {
  ggsave(
    file.path(pub_plot_dir, paste0(stem, ".pdf")),
    plot_object,
    width = width,
    height = height,
    device = cairo_pdf,
    bg = "white"
  )

  png_path <- file.path(pub_plot_dir, paste0(stem, ".png"))
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(
      png_path,
      plot_object,
      width = width,
      height = height,
      dpi = dpi,
      device = ragg::agg_png,
      bg = "white"
    )
  } else {
    ggsave(
      png_path,
      plot_object,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
  }
}

weight_long <- read_csv(file.path(data_dir, "weight_long.csv"), show_col_types = FALSE) %>%
  mutate(
    arm = factor(arm, levels = arm_levels),
    visit_month = as.numeric(visit_month)
  )

visit_breaks <- sort(unique(weight_long$visit_month))
visit_labels <- ifelse(visit_breaks == 1, "Baseline", paste("Month", visit_breaks - 1))
first_followup_visit <- visit_breaks[visit_breaks > min(visit_breaks)][1]

# Figure set: baseline weight plus at least one post-baseline weight.
eligible_weight_ids <- weight_long %>%
  group_by(study_id, arm) %>%
  summarise(
    baseline_observed = any(month_since_baseline == 0 & !is.na(weight_kg)),
    postbaseline_observed = any(month_since_baseline > 0 & !is.na(weight_kg)),
    .groups = "drop"
  ) %>%
  filter(baseline_observed, postbaseline_observed)

weight_plot_data <- weight_long %>%
  semi_join(eligible_weight_ids, by = c("study_id", "arm")) %>%
  filter(!is.na(weight_kg), !is.na(pct_weight_change))

# Points and error bars are observed means with +/- 1 SE.
observed_summary <- weight_plot_data %>%
  group_by(arm, visit_month) %>%
  summarise(
    n = n(),
    mean_pct_change = mean(pct_weight_change, na.rm = TRUE),
    sd_pct_change = sd(pct_weight_change, na.rm = TRUE),
    se_pct_change = if_else(n > 1, sd_pct_change / sqrt(n), NA_real_),
    lower_se = mean_pct_change - se_pct_change,
    upper_se = mean_pct_change + se_pct_change,
    .groups = "drop"
  ) %>%
  arrange(arm, visit_month)

observed_smooth <- observed_summary %>%
  group_by(arm) %>%
  group_modify(function(.x, .y) {
    if (nrow(.x) < 3) {
      return(.x %>% transmute(visit_month, mean_pct_change))
    }

    spline_method <- if (
      all(diff(.x$mean_pct_change) >= 0) ||
        all(diff(.x$mean_pct_change) <= 0)
    ) {
      "hyman"
    } else {
      "natural"
    }

    smoothed <- stats::spline(
      x = .x$visit_month,
      y = .x$mean_pct_change,
      n = 160,
      method = spline_method
    )

    tibble(
      visit_month = smoothed$x,
      mean_pct_change = smoothed$y
    )
  }) %>%
  ungroup() %>%
  group_by(arm) %>%
  mutate(
    mean_pct_change = if_else(
      visit_month <= first_followup_visit,
      pmax(mean_pct_change, 0),
      mean_pct_change
    )
  ) %>%
  ungroup() %>%
  select(arm, visit_month, mean_pct_change)

y_limits <- c(-2, 14)
y_breaks <- seq(y_limits[1], y_limits[2], by = 2)

observed_weight_trajectory_plot <- ggplot() +
  geom_hline(yintercept = 0, color = "#AFAFAF", linewidth = 0.45) +
  geom_line(
    data = observed_smooth,
    aes(x = visit_month, y = mean_pct_change, color = arm, group = arm),
    linewidth = 1.08,
    alpha = 0.88,
    lineend = "round",
    linejoin = "round"
  ) +
  geom_errorbar(
    data = observed_summary %>% filter(visit_month > 1),
    aes(x = visit_month, ymin = lower_se, ymax = upper_se, color = arm),
    width = 0.045,
    linewidth = 0.44,
    alpha = 0.88
  ) +
  geom_point(
    data = observed_summary,
    aes(x = visit_month, y = mean_pct_change, color = arm),
    size = 2.8,
    alpha = 0.98
  ) +
  scale_x_continuous(
    breaks = visit_breaks,
    labels = visit_labels,
    limits = c(min(visit_breaks), max(visit_breaks) + 0.05),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = y_breaks,
    limits = y_limits,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_color_manual(values = arm_palette, breaks = arm_levels) +
  labs(
    x = NULL,
    y = "Percent Body Weight Change (%)"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 1.15, linetype = "solid", shape = 16, alpha = 1)
    )
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.line = element_line(color = "#5F5F5F", linewidth = 0.45),
    axis.ticks = element_line(color = "#5F5F5F", linewidth = 0.35),
    axis.text = element_text(color = "#5C526B", size = 9),
    axis.title.y = element_text(color = "#1F2540", size = 9.5, margin = margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8.8),
    legend.key.width = unit(0.42, "in"),
    legend.margin = margin(t = 0),
    plot.margin = margin(8, 12, 4, 8)
  )

save_plot_high_res(observed_weight_trajectory_plot, "figure_1_weight_trajectories")
