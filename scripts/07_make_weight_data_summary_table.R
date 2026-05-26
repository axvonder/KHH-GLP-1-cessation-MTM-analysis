#!/usr/bin/env Rscript

# Step 07. Build observed weight summaries and figure QA.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(flextable)
  library(officer)
})

# Resolve script path for `Rscript` or `source()`.
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

  normalizePath(file.path(getwd(), "07_make_weight_data_summary_table.R"), mustWork = FALSE)
}

script_path <- resolve_script_path()

# Pipeline paths.
pipeline_dir <- dirname(dirname(script_path))
data_dir <- file.path(pipeline_dir, "output", "data")
table_dir <- file.path(pipeline_dir, "output", "tables")
table_support_dir <- file.path(table_dir, "supporting files")
qa_dir <- file.path(pipeline_dir, "output", "qa")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_support_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

# Inputs from Steps 1 and 2.
weight_long_file <- file.path(data_dir, "weight_long.csv")
weight_model_means_file <- file.path(data_dir, "weight_model_means.csv")
required_files <- c(weight_long_file, weight_model_means_file)
stopifnot(all(file.exists(required_files)))

arm_levels <- c("Control", "Noom", "MTM")
tol <- 1e-8

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (sum(!is.na(x)) <= 1) NA_real_ else sd(x, na.rm = TRUE)
}

safe_quantile <- function(x, prob) {
  if (all(is.na(x))) NA_real_ else as.numeric(quantile(x, prob, na.rm = TRUE, names = FALSE))
}

fmt_n_pct <- function(n, pct) {
  ifelse(is.na(n) | is.na(pct), "", sprintf("%d (%.1f%%)", n, pct))
}

fmt_mean_sd <- function(mean, sd, digits = 1) {
  ifelse(is.na(mean), "", sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean, sd))
}

fmt_mean_se <- function(mean, se, digits = 1) {
  ifelse(is.na(mean), "", sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean, se))
}

fmt_range <- function(minimum, maximum, digits = 1) {
  ifelse(is.na(minimum) | is.na(maximum), "", sprintf(paste0("%.", digits, "f to %.", digits, "f"), minimum, maximum))
}

visit_month_label <- function(visit_month) {
  ifelse(visit_month == 1L, "Baseline", paste("Month", visit_month - 1L))
}

weight_long <- read_csv(weight_long_file, show_col_types = FALSE) %>%
  mutate(
    arm = factor(arm, levels = arm_levels),
    visit_month = as.integer(visit_month),
    month_since_baseline = as.integer(month_since_baseline)
  )

weight_model_means <- read_csv(weight_model_means_file, show_col_types = FALSE) %>%
  transmute(
    arm = factor(arm, levels = arm_levels),
    visit_month = as.integer(month_since_baseline + 1),
    model_emmean = emmean,
    model_se = SE
  )

stopifnot(nrow(weight_long) == nrow(distinct(weight_long, study_id, visit_month)))

# One participant-level row documents the figure-eligibility rule.
weight_wide <- weight_long %>%
  select(study_id, arm, visit_month, weight_kg, pct_weight_change) %>%
  pivot_wider(
    names_from = visit_month,
    values_from = c(weight_kg, pct_weight_change),
    names_glue = "month_{visit_month}_{.value}"
  )

visit_months <- sort(unique(weight_long$visit_month))
baseline_visit_month <- visit_months[which.min(abs(visit_months - 1L))]
post_visit_months <- visit_months[visit_months != baseline_visit_month]
weight_cols <- paste0("month_", visit_months, "_weight_kg")
pct_cols <- paste0("month_", visit_months, "_pct_weight_change")

for (visit in visit_months) {
  weight_col <- paste0("month_", visit, "_weight_kg")
  if (!weight_col %in% names(weight_wide)) {
    weight_wide[[weight_col]] <- NA_real_
  }
  weight_wide[[paste0("month_", visit, "_has_weight")]] <- !is.na(weight_wide[[weight_col]])
}

post_weight_cols <- paste0("month_", post_visit_months, "_weight_kg")
post_weight_matrix <- !is.na(as.matrix(weight_wide[post_weight_cols]))
post_months <- apply(post_weight_matrix, 1, function(x) paste(visit_month_label(post_visit_months[x]), collapse = "; "))
post_months[post_months == ""] <- NA_character_
postbaseline_observed_vector <- rowSums(post_weight_matrix) > 0

participant_status <- weight_wide %>%
  mutate(
    baseline_observed = .data[[paste0("month_", baseline_visit_month, "_has_weight")]],
    postbaseline_observed = postbaseline_observed_vector,
    eligible_observed_trajectory = baseline_observed & postbaseline_observed,
    post_months = post_months
  ) %>%
  arrange(arm, study_id)

participant_status_csv <- participant_status %>%
  select(
    study_id,
    arm,
    all_of(paste0("month_", visit_months, "_has_weight")),
    all_of(weight_cols),
    all_of(pct_cols),
    baseline_observed,
    postbaseline_observed,
    eligible_observed_trajectory
  )

eligibility_csv <- participant_status %>%
  transmute(
    study_id,
    arm,
    baseline_observed,
    postbaseline_observed,
    post_months,
    eligible_observed_plot = eligible_observed_trajectory
  )

arm_denominators <- participant_status %>%
  group_by(arm) %>%
  summarise(
    participants_in_arm = n(),
    participants_with_baseline_weight = sum(baseline_observed),
    participants_with_any_postbaseline_weight = sum(postbaseline_observed),
    participants_eligible_for_observed_trajectory = sum(eligible_observed_trajectory),
    .groups = "drop"
  )

eligible_ids <- participant_status %>%
  filter(eligible_observed_trajectory) %>%
  select(study_id, arm)

summarise_weight_rows <- function(data, denominator_df, denominator_col) {
  data %>%
    left_join(denominator_df, by = "arm") %>%
    group_by(arm, visit_month) %>%
    summarise(
      denominator = first(.data[[denominator_col]]),
      weight_observed_n = sum(!is.na(weight_kg)),
      weight_missing_n = denominator - weight_observed_n,
      weight_missing_pct = 100 * weight_missing_n / denominator,
      weight_mean_kg = safe_mean(weight_kg),
      weight_sd_kg = safe_sd(weight_kg),
      weight_median_kg = safe_quantile(weight_kg, 0.50),
      weight_min_kg = if (all(is.na(weight_kg))) NA_real_ else min(weight_kg, na.rm = TRUE),
      weight_max_kg = if (all(is.na(weight_kg))) NA_real_ else max(weight_kg, na.rm = TRUE),
      pct_change_observed_n = sum(!is.na(pct_weight_change)),
      pct_change_missing_n = denominator - pct_change_observed_n,
      pct_change_missing_pct = 100 * pct_change_missing_n / denominator,
      pct_change_mean = safe_mean(pct_weight_change),
      pct_change_sd = safe_sd(pct_weight_change),
      pct_change_se = if_else(pct_change_observed_n > 1, pct_change_sd / sqrt(pct_change_observed_n), NA_real_),
      pct_change_median = safe_quantile(pct_weight_change, 0.50),
      pct_change_min = if (all(is.na(pct_weight_change))) NA_real_ else min(pct_weight_change, na.rm = TRUE),
      pct_change_max = if (all(is.na(pct_weight_change))) NA_real_ else max(pct_weight_change, na.rm = TRUE),
      month_label = visit_month_label(first(visit_month)),
      .groups = "drop"
    ) %>%
    arrange(arm, visit_month)
}

# Formal table denominator: all participants in the Fitabase-derived weight dataset.
all_participant_summary <- summarise_weight_rows(
  data = weight_long,
  denominator_df = arm_denominators,
  denominator_col = "participants_in_arm"
) %>%
  left_join(arm_denominators, by = "arm") %>%
  transmute(
    arm,
    month_label,
    visit_month,
    participants_in_arm,
    participants_with_baseline_weight,
    participants_with_any_postbaseline_weight,
    participants_eligible_for_observed_trajectory,
    weight_observed_n,
    weight_missing_n,
    weight_missing_pct,
    weight_mean_kg,
    weight_sd_kg,
    weight_median_kg,
    weight_min_kg,
    weight_max_kg,
    pct_change_observed_n,
    pct_change_missing_n,
    pct_change_missing_pct,
    pct_change_mean,
    pct_change_sd,
    pct_change_se,
    pct_change_median,
    pct_change_min,
    pct_change_max
  )

# Formal table denominator: baseline weight plus at least one post-baseline weight.
observed_trajectory_table <- weight_long %>%
  semi_join(eligible_ids, by = c("study_id", "arm")) %>%
  summarise_weight_rows(
    denominator_df = arm_denominators,
    denominator_col = "participants_eligible_for_observed_trajectory"
  ) %>%
  transmute(
    arm,
    visit_month,
    participants_eligible_for_observed_trajectory = denominator,
    weight_observed_n,
    weight_missing_n,
    weight_missing_pct,
    weight_mean_kg,
    weight_sd_kg,
    weight_median_kg,
    weight_min_kg,
    weight_max_kg,
    pct_change_observed_n,
    pct_change_missing_n,
    pct_change_missing_pct,
    pct_change_mean,
    pct_change_sd,
    pct_change_se,
    pct_change_median,
    pct_change_min,
    pct_change_max,
    month_label
  )

formal_table_summary <- observed_trajectory_table

observed_plot_data <- weight_long %>%
  semi_join(eligible_ids, by = c("study_id", "arm")) %>%
  filter(!is.na(weight_kg), !is.na(pct_weight_change))

observed_arm_month_summary <- observed_plot_data %>%
  group_by(arm, visit_month) %>%
  summarise(
    n = n(),
    mean = mean(pct_weight_change),
    sd = if_else(n > 1, sd(pct_weight_change), NA_real_),
    se = if_else(n > 1, sd / sqrt(n), NA_real_),
    lower_se = mean - se,
    upper_se = mean + se,
    ci95_halfwidth = if_else(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
    lower_95 = mean - ci95_halfwidth,
    upper_95 = mean + ci95_halfwidth,
    min = min(pct_weight_change),
    q1 = safe_quantile(pct_weight_change, 0.25),
    median = median(pct_weight_change),
    q3 = safe_quantile(pct_weight_change, 0.75),
    max = max(pct_weight_change),
    range = max - min,
    .groups = "drop"
  ) %>%
  arrange(arm, visit_month)

values_by_arm_month <- observed_plot_data %>%
  arrange(arm, visit_month, pct_weight_change, study_id) %>%
  group_by(arm, visit_month) %>%
  summarise(
    values = paste0("ID ", study_id, ": ", sprintf("%.2f%%", pct_weight_change), collapse = "; "),
    .groups = "drop"
  )

n_needed_for_small_se <- observed_arm_month_summary %>%
  transmute(
    arm,
    visit_month,
    sd,
    current_n = n,
    current_se = se,
    n_for_se_0_75 = ceiling((sd / 0.75)^2),
    n_for_se_1_00 = ceiling((sd / 1.00)^2),
    n_for_se_1_50 = ceiling((sd / 1.50)^2)
  )

excel_series_se <- observed_arm_month_summary %>%
  group_by(arm) %>%
  summarise(
    excel_series_standard_error = sd(mean) / sqrt(n()),
    .groups = "drop"
  )

total_trial_n <- n_distinct(weight_long$study_id)
all_pct_obs_n <- sum(!is.na(weight_long$pct_weight_change))

error_bar_formula_scenarios <- observed_arm_month_summary %>%
  filter(visit_month > 1) %>%
  left_join(excel_series_se, by = "arm") %>%
  left_join(weight_model_means, by = c("arm", "visit_month")) %>%
  transmute(
    arm,
    visit_month,
    n,
    mean = round(mean, 2),
    sd = round(sd, 2),
    correct_se = round(se, 2),
    sd_div_n = round(sd / n, 2),
    se_using_total_trial_n = round(sd / sqrt(total_trial_n), 2),
    se_using_all_pct_obs_n = round(sd / sqrt(all_pct_obs_n), 2),
    excel_series_standard_error = round(excel_series_standard_error, 2),
    model_emmean = round(model_emmean, 2),
    model_se = round(model_se, 2)
  )

plot_summary_for_join <- observed_arm_month_summary %>%
  transmute(
    arm,
    visit_month,
    plot_n = n,
    plot_mean_pct_change = mean,
    plot_sd_pct_change = sd,
    plot_se_pct_change = se,
    plot_lower_se = lower_se,
    plot_upper_se = upper_se
  )

plot_vs_observed_table <- plot_summary_for_join %>%
  left_join(
    observed_trajectory_table %>%
      transmute(
        arm,
        visit_month,
        obs_table_pct_n = pct_change_observed_n,
        obs_table_mean_pct_change = pct_change_mean,
        obs_table_sd_pct_change = pct_change_sd,
        obs_table_se_pct_change = pct_change_se
      ),
    by = c("arm", "visit_month")
  ) %>%
  mutate(
    n_diff = plot_n - obs_table_pct_n,
    mean_diff = plot_mean_pct_change - obs_table_mean_pct_change,
    sd_diff = plot_sd_pct_change - obs_table_sd_pct_change,
    se_diff = plot_se_pct_change - obs_table_se_pct_change,
    status = if_else(
      n_diff == 0 &
        abs(mean_diff) <= tol &
        abs(sd_diff) <= tol &
        abs(se_diff) <= tol,
      "PASS",
      "FLAG"
    )
  )

plot_vs_formal_table <- plot_summary_for_join %>%
  left_join(
    formal_table_summary %>%
      transmute(
        arm,
        visit_month,
        table_denominator = participants_eligible_for_observed_trajectory,
        table_pct_n = pct_change_observed_n,
        table_mean_pct_change = pct_change_mean,
        table_sd_pct_change = pct_change_sd,
        table_se_pct_change = pct_change_se,
        table_lower_se = pct_change_mean - pct_change_se,
        table_upper_se = pct_change_mean + pct_change_se
      ),
    by = c("arm", "visit_month")
  ) %>%
  mutate(
    n_diff = plot_n - table_pct_n,
    mean_diff = plot_mean_pct_change - table_mean_pct_change,
    sd_diff = plot_sd_pct_change - table_sd_pct_change,
    se_diff = plot_se_pct_change - table_se_pct_change,
    lower_diff = plot_lower_se - table_lower_se,
    upper_diff = plot_upper_se - table_upper_se,
    status = if_else(
      n_diff == 0 &
        abs(mean_diff) <= tol &
        abs(sd_diff) <= tol &
        abs(se_diff) <= tol &
        abs(lower_diff) <= tol &
        abs(upper_diff) <= tol,
      "PASS",
      "FLAG"
    )
  )

stopifnot(all(plot_vs_observed_table$status == "PASS"))
stopifnot(all(plot_vs_formal_table$status == "PASS"))

# Write reproducible weight tables and QA companions.
write_csv(all_participant_summary, file.path(table_support_dir, "Observed Weight by Month - All Participants.csv"))
write_csv(observed_trajectory_table, file.path(table_support_dir, "Observed Weight by Month - Model Eligible.csv"))
write_csv(participant_status_csv, file.path(qa_dir, "qa_weight_participant_month_missingness_status.csv"))
write_csv(eligibility_csv, file.path(qa_dir, "qa_observed_weight_trajectory_eligibility.csv"))
write_csv(observed_arm_month_summary, file.path(qa_dir, "qa_observed_weight_trajectory_arm_month_summary.csv"))
write_csv(values_by_arm_month, file.path(qa_dir, "qa_observed_weight_trajectory_values_by_arm_month.csv"))
write_csv(n_needed_for_small_se, file.path(qa_dir, "qa_observed_weight_trajectory_n_needed_for_small_se.csv"))
write_csv(error_bar_formula_scenarios, file.path(qa_dir, "qa_weight_error_bar_formula_scenarios.csv"))
write_csv(plot_vs_observed_table, file.path(qa_dir, "qa_weight_plot_vs_observed_trajectory_table_comparison.csv"))
write_csv(plot_vs_formal_table, file.path(qa_dir, "qa_weight_plot_vs_formal_table_comparison.csv"))
display_table_for_csv <- formal_table_summary %>%
  transmute(
    Arm = as.character(arm),
    Month = month_label,
    `Observed weight, n/N` = paste0(weight_observed_n, "/", participants_eligible_for_observed_trajectory),
    `Missing weight, n (%)` = fmt_n_pct(weight_missing_n, weight_missing_pct),
    `Weight, kg, mean (SD)` = fmt_mean_sd(weight_mean_kg, weight_sd_kg),
    `% weight change, mean (SE)` = fmt_mean_se(pct_change_mean, pct_change_se),
    `% weight change, range` = fmt_range(pct_change_min, pct_change_max)
  )

display_table <- bind_rows(lapply(seq_along(arm_levels), function(i) {
  arm_name <- arm_levels[i]
  arm_rows <- display_table_for_csv %>%
    filter(Arm == arm_name) %>%
    mutate(Arm = if_else(row_number() == 1L, Arm, ""))

  if (i < length(arm_levels)) {
    spacer_row <- arm_rows[1, , drop = FALSE]
    spacer_row[] <- ""
    bind_rows(arm_rows, spacer_row)
  } else {
    arm_rows
  }
}))

group_rows <- which(display_table$Arm != "")
spacer_rows <- which(display_table$Month == "")

weight_summary_ft <- flextable(display_table) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 8.8, part = "all") %>%
  align(align = "left", j = c("Arm", "Month"), part = "all") %>%
  align(align = "center", j = 3:ncol(display_table), part = "all") %>%
  padding(padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4, part = "all") %>%
  line_spacing(space = 0.9, part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border(color = "#444444", width = 1.25), part = "header") %>%
  hline(border = fp_border(color = "#444444", width = 0.8), part = "header") %>%
  hline_bottom(border = fp_border(color = "#444444", width = 1.25), part = "body") %>%
  bold(part = "header") %>%
  bold(i = group_rows, j = "Arm", bold = TRUE, part = "body") %>%
  height(i = spacer_rows, height = 0.10, part = "body") %>%
  padding(i = spacer_rows, padding.top = 3, padding.bottom = 3, padding.left = 4, padding.right = 4, part = "body") %>%
  width(j = "Arm", width = 0.78) %>%
  width(j = "Month", width = 0.78) %>%
  width(j = "Observed weight, n/N", width = 1.05) %>%
  width(j = "Missing weight, n (%)", width = 1.15) %>%
  width(j = "Weight, kg, mean (SD)", width = 1.25) %>%
  width(j = "% weight change, mean (SE)", width = 1.35) %>%
  width(j = "% weight change, range", width = 1.20) %>%
  set_header_labels(
    Arm = "Arm",
    Month = "Month",
    `Observed weight, n/N` = "Observed weight\nn/N",
    `Missing weight, n (%)` = "Missing weight\nn (%)",
    `Weight, kg, mean (SD)` = "Weight, kg\nmean (SD)",
    `% weight change, mean (SE)` = "% weight change\nmean (SE)",
    `% weight change, range` = "% weight change\nrange"
  ) %>%
  align(align = "center", part = "header") %>%
  add_footer_lines(values = paste(
    "Denominators are participants with baseline weight plus at least one post-baseline weight in each arm.",
    "Percent weight change was calculated as 100 x (visit weight - baseline weight) / baseline weight."
  )) %>%
  align(part = "footer", align = "left") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 7.8, part = "footer")

title_par <- officer::fpar(
  officer::ftext(
    "Observed Weight Data by Arm and Month",
    prop = officer::fp_text(font.family = "Times New Roman", bold = TRUE, font.size = 11)
  ),
  fp_p = officer::fp_par(text.align = "left", padding.bottom = 6)
)

doc <- officer::read_docx() %>%
  officer::body_set_default_section(
    officer::prop_section(
      page_size = officer::page_size(orient = "landscape"),
      page_margins = officer::page_mar(top = 0.5, bottom = 0.5, left = 0.45, right = 0.45)
    )
  ) %>%
  officer::body_add_fpar(title_par) %>%
  flextable::body_add_flextable(weight_summary_ft)

write_csv(display_table_for_csv, file.path(table_support_dir, "SI Table 2 - Observed Weight by Month.csv"))
print(doc, target = file.path(table_dir, "SI Table 2 - Observed Weight by Month.docx"))

if (requireNamespace("webshot2", quietly = TRUE)) {
  weight_table_png <- file.path(table_support_dir, "SI Table 2 - Observed Weight by Month.png")
  flextable::save_as_image(
    weight_summary_ft,
    path = weight_table_png,
    zoom = 3
  )

  if (requireNamespace("magick", quietly = TRUE)) {
    magick::image_read(weight_table_png) %>%
      magick::image_background("white", flatten = TRUE) %>%
      magick::image_write(weight_table_png)
  }
}

message("Step 07 complete: observed weight summaries and QA written to ", table_dir)
