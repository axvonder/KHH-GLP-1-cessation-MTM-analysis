#!/usr/bin/env Rscript

# Compact stacked forest plot of model-based contrasts vs Control.
# One row per outcome; the Wellness Application and MTM contrasts are color-dodged within row.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
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

  normalizePath(file.path(getwd(), "09_make_compact_model_contrasts_forest.R"), mustWork = FALSE)
}

script_path <- resolve_script_path()
pipeline_dir <- dirname(dirname(script_path))
data_dir <- file.path(pipeline_dir, "output", "data")
pub_plot_dir <- file.path(pipeline_dir, "output", "pub plots")

dir.create(pub_plot_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- file.path(
  data_dir,
  c(
    "weight_model_mean_contrasts.csv",
    "weight_slope_contrasts.csv",
    "mini_eat_control_contrasts.csv",
    "pdq_contrasts.csv",
    "phq_contrasts.csv",
    "tapq_contrasts.csv",
    "tsqm_effectiveness_contrasts.csv",
    "tsqm_convenience_contrasts.csv",
    "tsqm_global_contrasts.csv"
  )
)

stopifnot(all(file.exists(required_files)))

arm_palette <- c("Noom" = "#2E7D32", "MTM" = "#E68613")
contrast_levels <- c("Noom vs Control", "MTM vs Control")
arm_display_labels <- c("Noom" = "Wellness Application", "MTM" = "MTM")

display_arm_label <- function(x) {
  ifelse(as.character(x) == "Noom", arm_display_labels[["Noom"]], as.character(x))
}

save_plot_high_res <- function(plot_object, stem, width = 8.7, height = 7.9, dpi = 900) {
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

standardize_contrast_label <- function(x) {
  dplyr::recode(
    x,
    `Noom - Control` = "Noom vs Control",
    `MTM - Control` = "MTM vs Control",
    .default = x
  )
}

weight_mean_contrasts <- read_csv(file.path(data_dir, "weight_model_mean_contrasts.csv"), show_col_types = FALSE)
weight_slope_contrasts <- read_csv(file.path(data_dir, "weight_slope_contrasts.csv"), show_col_types = FALSE)
mini_eat_contrasts <- read_csv(file.path(data_dir, "mini_eat_control_contrasts.csv"), show_col_types = FALSE)
pdq_contrasts <- read_csv(file.path(data_dir, "pdq_contrasts.csv"), show_col_types = FALSE)
phq_contrasts <- read_csv(file.path(data_dir, "phq_contrasts.csv"), show_col_types = FALSE)
tapq_contrasts <- read_csv(file.path(data_dir, "tapq_contrasts.csv"), show_col_types = FALSE)
tsqm_effectiveness_contrasts <- read_csv(file.path(data_dir, "tsqm_effectiveness_contrasts.csv"), show_col_types = FALSE)
tsqm_convenience_contrasts <- read_csv(file.path(data_dir, "tsqm_convenience_contrasts.csv"), show_col_types = FALSE)
tsqm_global_contrasts <- read_csv(file.path(data_dir, "tsqm_global_contrasts.csv"), show_col_types = FALSE)

final_weight_month <- max(weight_mean_contrasts$month_since_baseline, na.rm = TRUE)

contrast_data <- bind_rows(
  weight_mean_contrasts %>%
    filter(month_since_baseline == final_weight_month) %>%
    transmute(
      section = "Longitudinal weight change",
      outcome = "Weight change total",
      contrast = standardize_contrast_label(contrast),
      estimate,
      lower = lower.CL,
      upper = upper.CL
    ),
  weight_slope_contrasts %>%
    transmute(
      section = "Longitudinal weight change",
      outcome = "Weight change slope",
      contrast = standardize_contrast_label(contrast),
      estimate,
      lower = lower.CL,
      upper = upper.CL
    ),
  mini_eat_contrasts %>%
    transmute(
      section = "Mini-EAT",
      outcome = "Mini-EAT",
      contrast = standardize_contrast_label(contrast),
      estimate,
      lower = lower.CL,
      upper = upper.CL
    ),
  pdq_contrasts %>%
    filter(contrast %in% contrast_levels) %>%
    transmute(
      section = "PDQ / PHQ",
      outcome = "PDQ",
      contrast,
      estimate,
      lower = lower.CL,
      upper = upper.CL
    ),
  phq_contrasts %>%
    filter(contrast %in% contrast_levels) %>%
    transmute(
      section = "PDQ / PHQ",
      outcome = "PHQ",
      contrast,
      estimate,
      lower = lower.CL,
      upper = upper.CL
    ),
  tapq_contrasts %>%
    filter(contrast %in% contrast_levels) %>%
    transmute(
      section = "TAPQ / TSQM",
      outcome = "TAPQ",
      contrast,
      estimate,
      lower = lower.CL,
      upper = upper.CL
    ),
  tsqm_effectiveness_contrasts %>%
    filter(contrast %in% contrast_levels) %>%
    transmute(
      section = "TAPQ / TSQM",
      outcome = "TSQM effectiveness",
      contrast,
      estimate,
      lower = lower.CL,
      upper = upper.CL
    ),
  tsqm_convenience_contrasts %>%
    filter(contrast %in% contrast_levels) %>%
    transmute(
      section = "TAPQ / TSQM",
      outcome = "TSQM convenience",
      contrast,
      estimate,
      lower = lower.CL,
      upper = upper.CL
    ),
  tsqm_global_contrasts %>%
    filter(contrast %in% contrast_levels) %>%
    transmute(
      section = "TAPQ / TSQM",
      outcome = "TSQM global satisfaction",
      contrast,
      estimate,
      lower = lower.CL,
      upper = upper.CL
    )
) %>%
  mutate(
    contrast = factor(contrast, levels = contrast_levels),
    arm = recode(as.character(contrast), `Noom vs Control` = "Noom", `MTM vs Control` = "MTM"),
    arm = factor(arm, levels = c("Noom", "MTM"))
  )

format_axis_ticks <- function(x) {
  gsub("\\.0$", "", formatC(x, digits = 1, format = "f"))
}

make_stacked_section <- function(
  data,
  outcome_order,
  x_lab,
  show_x_title = TRUE,
  left_direction = "Favors Control",
  right_direction = "Favors Wellness Application/MTM",
  section_height = 1
) {
  plot_data <- data %>%
    filter(outcome %in% outcome_order) %>%
    mutate(outcome = factor(outcome, levels = rev(outcome_order)))

  section_limit <- max(abs(c(plot_data$lower, plot_data$upper)), na.rm = TRUE) * 1.10
  section_breaks <- pretty(c(-section_limit, section_limit), n = 5)
  bottom_outcome <- levels(plot_data$outcome)[1]
  y_expand_lower <- 0.78
  y_expand_upper <- 0.35
  y_lower_bound <- 0.5 - y_expand_lower
  y_upper_bound <- length(outcome_order) + 0.5 + y_expand_upper
  y_range <- y_upper_bound - y_lower_bound
  direction_nudge_y <- y_lower_bound + 0.255 * y_range / section_height - 1

  estimate_labels <- plot_data %>%
    mutate(
      label = paste0(
        display_arm_label(arm),
        ": ",
        formatC(estimate, digits = 2, format = "f"),
        " (",
        formatC(lower, digits = 2, format = "f"),
        ", ",
        formatC(upper, digits = 2, format = "f"),
        ")"
      ),
      label_x = section_limit * 1.88
    )

  direction_labels <- tibble(
    x = c(-section_limit * 0.08, section_limit * 0.08),
    outcome = factor(c(bottom_outcome, bottom_outcome), levels = levels(plot_data$outcome)),
    label = c(left_direction, right_direction),
    hjust = c(1, 0)
  )

  dodge <- position_dodge(width = 0.48)

  ggplot(plot_data, aes(x = estimate, y = outcome, color = arm)) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey55", linewidth = 0.42) +
    geom_hline(
      yintercept = seq_along(outcome_order),
      color = "#E8E8E8",
      linewidth = 0.30
    ) +
    geom_errorbar(
      aes(xmin = lower, xmax = upper),
      position = dodge,
      width = 0,
      linewidth = 0.52,
      orientation = "y"
    ) +
    geom_point(
      position = dodge,
      size = 2.45
    ) +
    geom_label(
      data = estimate_labels,
      aes(x = label_x, y = outcome, label = label, group = arm),
      position = dodge,
      inherit.aes = FALSE,
      hjust = 1,
      size = 2.25,
      color = "#4A4A4A",
      label.padding = unit(0.08, "lines"),
      linewidth = 0,
      fill = "white"
    ) +
    geom_text(
      data = direction_labels,
      aes(x = x, y = outcome, label = label, hjust = hjust),
      inherit.aes = FALSE,
      nudge_y = direction_nudge_y,
      size = 2.00,
      color = "#817D88",
      fontface = "italic",
      vjust = 0
    ) +
    scale_color_manual(
      values = arm_palette,
      breaks = c("Noom", "MTM"),
      labels = unname(arm_display_labels[c("Noom", "MTM")])
    ) +
    scale_x_continuous(
      limits = c(-section_limit, section_limit * 1.95),
      breaks = section_breaks,
      labels = format_axis_ticks
    ) +
    scale_y_discrete(expand = expansion(add = c(y_expand_lower, y_expand_upper))) +
    labs(
      title = NULL,
      x = if (show_x_title) x_lab else NULL,
      y = NULL
    ) +
    theme_classic(base_size = 10.5) +
    theme(
      panel.background = element_rect(fill = "#FCFCFC", color = NA),
      panel.border = element_rect(color = "#D9D9D9", fill = NA, linewidth = 0.55),
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = "#5F5F5F", linewidth = 0.32),
      axis.text = element_text(color = "#4F4A5F"),
      axis.text.y = element_text(size = 8.8),
      axis.title.x = element_text(color = "#1F2540", size = 9.1, margin = margin(t = 4)),
      plot.title = element_text(face = "bold", size = 10.3, margin = margin(b = 4)),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      legend.key.width = unit(0.30, "in"),
      plot.margin = margin(5, 8, 3, 8)
    )
}

weight_plot <- make_stacked_section(
  contrast_data %>% filter(section == "Longitudinal weight change"),
  outcome_order = c("Weight change total", "Weight change slope"),
  x_lab = "Difference vs Control",
  show_x_title = FALSE,
  left_direction = "Favors Wellness Application/MTM",
  right_direction = "Favors Control",
  section_height = 1.35
)

mini_eat_plot <- make_stacked_section(
  contrast_data %>% filter(section == "Mini-EAT"),
  outcome_order = c("Mini-EAT"),
  x_lab = "Difference vs Control",
  show_x_title = FALSE,
  section_height = 1.00
)

pdq_phq_plot <- make_stacked_section(
  contrast_data %>% filter(section == "PDQ / PHQ"),
  outcome_order = c("PDQ", "PHQ"),
  x_lab = "Difference vs Control",
  show_x_title = FALSE,
  left_direction = "Favors Wellness Application/MTM",
  right_direction = "Favors Control",
  section_height = 1.32
)

tapq_tsqm_plot <- make_stacked_section(
  contrast_data %>% filter(section == "TAPQ / TSQM"),
  outcome_order = c("TAPQ", "TSQM effectiveness", "TSQM convenience", "TSQM global satisfaction"),
  x_lab = "Difference vs Control",
  section_height = 2.22
)

stacked_forest <- weight_plot / pdq_phq_plot / mini_eat_plot / tapq_tsqm_plot +
  plot_layout(guides = "collect", heights = c(1.35, 1.32, 1.00, 2.22)) &
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

save_plot_high_res(stacked_forest, "figure_2_compact_model_contrasts_forest")
