#!/usr/bin/env Rscript

# Step 03. Fit models and write tables.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(emmeans)
  library(lme4)
  library(lmerTest)
  library(broom)
  library(lmtest)
  library(sandwich)
  library(flextable)
  library(officer)
  library(kableExtra)
  library(tinytex)
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

  normalizePath(file.path(getwd(), "03_make_tables.R"), mustWork = FALSE)
}

script_path <- resolve_script_path()

# Pipeline paths.
pipeline_dir <- dirname(dirname(script_path))
data_dir <- file.path(pipeline_dir, "output", "data")
table_dir <- file.path(pipeline_dir, "output", "tables")
table_support_dir <- file.path(table_dir, "supporting files")
diagnostic_dir <- file.path(pipeline_dir, "output", "diagnostics")
qa_dir <- file.path(pipeline_dir, "output", "qa")

# Step outputs.
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_support_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

# Step-02 inputs.
participants_file <- file.path(data_dir, "participants.csv")
questionnaire_long_file <- file.path(data_dir, "questionnaire_long.csv")
weight_long_file <- file.path(data_dir, "weight_long.csv")
baseline_file <- file.path(data_dir, "baseline_analysis.csv")

# Required inputs.
stopifnot(file.exists(participants_file))
stopifnot(file.exists(questionnaire_long_file))
stopifnot(file.exists(weight_long_file))
stopifnot(file.exists(baseline_file))

# Arm order.
arm_levels <- c("Control", "Noom", "MTM")
noom_display_label <- "Wellness Application"

display_noom_text <- function(x) {
  stringr::str_replace_all(x, "Noom", noom_display_label)
}

display_arm_label <- function(x) {
  display_noom_text(as.character(x))
}

rename_noom_display_columns <- function(df) {
  names(df) <- display_noom_text(names(df))
  df
}

# Formatting helpers.
format_number <- function(x, digits = 2) {
  formatC(x, digits = digits, format = "f")
}

# PDF p-value format.
format_p_latex <- function(x) {
  ifelse(x < 0.001, "$<$0.001", formatC(x, digits = 3, format = "f"))
}

# Word p-value format.
fmt_p <- function(x) {
  if (is.na(x)) {
    return("")
  }
  if (x < 0.001) {
    return("<0.001")
  }
  digits <- ifelse(x < 0.05, 3, 2)
  formatC(x, digits = digits, format = "f")
}

# Estimate + CI in one cell.
fmt_est_ci <- function(estimate, lower, upper, digits = 2) {
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"),
    estimate,
    lower,
    upper
  )
}

# Mean (SD).
fmt_mean_sd <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return("NA")
  }
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    mean(x),
    sd(x)
  )
}

# n (%) with non-missing denominator.
fmt_n_pct <- function(x) {
  n <- sum(x, na.rm = TRUE)
  d <- sum(!is.na(x))
  if (d == 0) {
    return("NA")
  }
  sprintf("%d (%.1f%%)", n, 100 * n / d)
}

# LaTeX table helper.
latex_table <- function(data, caption, widths, font_size = 9) {
  # Column widths in cm.
  out <- kableExtra::kbl(
    data,
    format = "latex",
    booktabs = TRUE,
    caption = caption,
    longtable = FALSE,
    linesep = "",
    escape = FALSE
  )
  out <- out %>% kable_styling(font_size = font_size, full_width = FALSE, latex_options = c("hold_position"))
  for (i in seq_along(widths)) {
    out <- out %>% column_spec(i, width = widths[i])
  }
  out
}

# Build PDF from LaTeX wrapper.
write_pdf_from_tex <- function(tex_lines, pdf_file) {
  output_dir <- dirname(pdf_file)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_stem <- tools::file_path_sans_ext(basename(pdf_file))
  tex_stem <- gsub("[^A-Za-z0-9_-]+", "_", pdf_stem)

  # Compile inside the support folder so LaTeX scratch files do not land in root.
  old_wd <- getwd()
  setwd(output_dir)
  on.exit(setwd(old_wd), add = TRUE)
  on.exit(unlink(file.path(output_dir, paste0(tex_stem, c(".tex", ".log", ".aux", ".out"))), force = TRUE), add = TRUE)

  tex_file <- paste0(tex_stem, ".tex")
  generated_pdf <- file.path(output_dir, paste0(tex_stem, ".pdf"))
  writeLines(tex_lines, tex_file)
  tinytex::pdflatex(tex_file, clean = TRUE)
  if (!identical(generated_pdf, pdf_file) && file.exists(generated_pdf)) {
    file.rename(generated_pdf, pdf_file)
  }
  stopifnot(file.exists(pdf_file))
}

# Write a DOCX table without automatic numbered headings.
write_table_docx <- function(ft, title, docx_file, section) {
  title_par <- officer::fpar(
    officer::ftext(
      title,
      prop = officer::fp_text(
        font.family = "Times New Roman",
        bold = TRUE,
        italic = FALSE,
        font.size = 11
      )
    ),
    fp_p = officer::fp_par(text.align = "left", padding.bottom = 6)
  )

  doc <- officer::read_docx() %>%
    officer::body_set_default_section(section) %>%
    officer::body_add_fpar(title_par) %>%
    flextable::body_add_flextable(ft)

  print(doc, target = docx_file)
}

# Standard `lm()` diagnostics.
save_lm_diagnostics <- function(model, pdf_file) {
  # `plot.lm()` panels 1-4.
  grDevices::pdf(pdf_file, width = 8.5, height = 8.5)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mfrow = c(2, 2))
  plot(model, which = 1:4)
}

# Weight mixed-model diagnostics.
save_weight_diagnostics <- function(model, pdf_file) {
  # Residual fit, residual Q-Q, random-intercept Q-Q, random-intercept histogram.
  residuals_df <- tibble(
    fitted = fitted(model),
    resid = residuals(model)
  )
  ranef_df <- tibble(random_intercept = ranef(model)$study_id[[1]])

  grDevices::pdf(pdf_file, width = 8.5, height = 8.5)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mfrow = c(2, 2))
  plot(residuals_df$fitted, residuals_df$resid, pch = 16, col = "#355C7D", xlab = "Fitted", ylab = "Residual", main = "Residuals vs fitted")
  abline(h = 0, lty = 2, col = "grey50")
  qqnorm(residuals_df$resid, pch = 16, col = "#355C7D", main = "Residual Q-Q plot")
  qqline(residuals_df$resid, col = "#C06C84")
  qqnorm(ranef_df$random_intercept, pch = 16, col = "#355C7D", main = "Random intercept Q-Q plot")
  qqline(ranef_df$random_intercept, col = "#C06C84")
  hist(ranef_df$random_intercept, breaks = 10, col = "#355C7D", border = "white", main = "Random intercepts", xlab = "BLUP")
}

# Arm-vs-Control contrasts. Table output.
trt_vs_control <- function(emm) {
  # Exploratory pilot: no multiplicity adjustment.
  as.data.frame(summary(
    contrast(emm, "trt.vs.ctrl", ref = 1),
    infer = TRUE,
    adjust = "none"
  ))
}

# All arm contrasts. Figure output.
all_arm_contrasts <- function(emm) {
  # Arm order: Control, Noom, MTM.
  as.data.frame(summary(
    contrast(
      emm,
      method = list(
        "Noom vs Control" = c(-1, 1, 0),
        "MTM vs Control" = c(-1, 0, 1),
        "Noom vs MTM" = c(0, 1, -1)
      )
    ),
    infer = TRUE,
    adjust = "none"
  ))
}

# Baseline-adjusted follow-up model.
baseline_adjusted_model <- function(data, followup, baseline) {
  # Available cases with both baseline and follow-up.
  model_data <- data %>%
    filter(!is.na(.data[[followup]]), !is.na(.data[[baseline]]))

  # follow-up ~ arm + baseline
  model <- lm(stats::reformulate(c("arm", baseline), response = followup), data = model_data)
  # Adjusted means at the outcome-specific mean baseline value.
  emm <- emmeans(model, ~ arm, at = stats::setNames(list(mean(model_data[[baseline]], na.rm = TRUE)), baseline))

  list(
    data = model_data,
    model = model,
    coefficients = broom::tidy(model, conf.int = TRUE),
    omnibus = broom::tidy(anova(model)),
    adjusted_means = as.data.frame(summary(emm, infer = TRUE)),
    control_contrasts = trt_vs_control(emm),
    all_contrasts = all_arm_contrasts(emm),
    heterosk = broom::tidy(lmtest::bptest(model))
  )
}

# Follow-up-only model.
followup_only_model <- function(data, outcome) {
  # Available follow-up cases only.
  model_data <- data %>% filter(!is.na(.data[[outcome]]))
  # follow-up ~ arm
  model <- lm(stats::reformulate("arm", response = outcome), data = model_data)
  emm <- emmeans(model, ~ arm)

  list(
    data = model_data,
    model = model,
    coefficients = broom::tidy(model, conf.int = TRUE),
    omnibus = broom::tidy(anova(model)),
    adjusted_means = as.data.frame(summary(emm, infer = TRUE)),
    control_contrasts = trt_vs_control(emm),
    all_contrasts = all_arm_contrasts(emm),
    heterosk = broom::tidy(lmtest::bptest(model))
  )
}

# PDF coefficient table.
lm_coefficients_pdf <- function(result, mapping) {
  # Map model terms to reader-facing labels.
  result$coefficients %>%
    transmute(
      Term = dplyr::recode(term, !!!mapping),
      `B` = format_number(estimate),
      `SE` = format_number(std.error),
      `95\\% CI` = paste0(format_number(conf.low), " to ", format_number(conf.high)),
      `df` = format_number(df.residual(result$model), 1),
      `t` = format_number(statistic),
      `P` = format_p_latex(p.value)
    )
}

lm_omnibus_pdf <- function(result, mapping) {
  result$omnibus %>%
    filter(term != "Residuals") %>%
    transmute(
      Term = dplyr::recode(term, !!!mapping),
      `Df` = df,
      `F` = format_number(statistic),
      `P` = format_p_latex(p.value)
    )
}

pdf_section <- function(title, coefficients, omnibus) {
  # One section = coefficients + omnibus test.
  c(
    paste0("\\section*{", title, "}"),
    latex_table(coefficients, "Fixed-effect regression coefficients", c("4.4cm", "1.5cm", "1.5cm", "4.1cm", "1.2cm", "1.3cm", "1.5cm"), font_size = 8.8),
    "",
    latex_table(omnibus, "Global fixed-effect tests", c("5.2cm", "1.7cm", "1.7cm", "1.5cm"), font_size = 9),
    ""
  )
}

# Word-table styling.
add_table_style <- function(
  ft,
  group_rows,
  first_col_width,
  other_width,
  font_size = 9,
  padding_h = 5,
  padding_v = 3,
  use_autofit = FALSE
) {
  # Keep widths and spacing explicit.
  n_cols <- length(ft$col_keys)

  ft <- ft %>%
    fontsize(size = font_size, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    align(align = "left", j = 1, part = "all") %>%
    align(align = "center", j = 2:n_cols, part = "all") %>%
    padding(
      padding.top = padding_v,
      padding.bottom = padding_v,
      padding.left = padding_h,
      padding.right = padding_h,
      part = "all"
    ) %>%
    border_remove() %>%
    hline_top(border = fp_border(color = "#444444", width = 1.25), part = "header") %>%
    hline(border = fp_border(color = "#444444", width = 0.8), part = "header") %>%
    hline_bottom(border = fp_border(color = "#444444", width = 1.25), part = "body") %>%
    hline(i = group_rows, border = fp_border(color = "#b5b5b5", width = 0.8), part = "body") %>%
    bold(part = "header") %>%
    bold(i = group_rows, bold = TRUE, part = "body") %>%
    bg(i = group_rows, bg = "white", part = "body") %>%
    italic(i = group_rows, italic = FALSE, part = "body") %>%
    width(j = 1, width = first_col_width) %>%
    width(j = 2:n_cols, width = other_width)

  if (use_autofit) {
    # Table 1 only.
    ft <- autofit(ft)
  }

  ft
}

participants <- read_csv(participants_file, show_col_types = FALSE) %>%
  mutate(arm = factor(arm, levels = arm_levels))

questionnaire_long <- read_csv(questionnaire_long_file, show_col_types = FALSE) %>%
  mutate(
    arm = factor(arm, levels = arm_levels),
    timepoint = factor(timepoint, levels = c("baseline", "followup"))
  )

weight_long <- read_csv(weight_long_file, show_col_types = FALSE) %>%
  mutate(arm = factor(arm, levels = arm_levels))

baseline_df <- read_csv(baseline_file, show_col_types = FALSE) %>%
  mutate(
    # Binary Table-1 fields.
    arm = factor(arm, levels = arm_levels),
    # Keep prefer-not-to-answer codes out of descriptive denominators.
    female = case_when(
      gender == 1 ~ TRUE,
      gender == 2 ~ FALSE,
      gender %in% c(3, 4) ~ NA,
      TRUE ~ NA
    ),
    male = case_when(
      gender == 2 ~ TRUE,
      gender == 1 ~ FALSE,
      gender %in% c(3, 4) ~ NA,
      TRUE ~ NA
    ),
    hispanic_latino = case_when(
      ethinicty == 1 ~ TRUE,
      ethinicty == 2 ~ FALSE,
      ethinicty == 3 ~ NA,
      TRUE ~ NA
    ),
    black_race = case_when(
      race == 1 ~ TRUE,
      race %in% c(2, 3, 4, 5, 6) ~ FALSE,
      race == 7 ~ NA,
      TRUE ~ NA
    ),
    asian_race = case_when(
      race == 2 ~ TRUE,
      race %in% c(1, 3, 4, 5, 6) ~ FALSE,
      race == 7 ~ NA,
      TRUE ~ NA
    ),
    multiethnic_race = case_when(
      race == 5 ~ TRUE,
      race %in% c(1, 2, 3, 4, 6) ~ FALSE,
      race == 7 ~ NA,
      TRUE ~ NA
    ),
    white_race = case_when(
      race == 6 ~ TRUE,
      race %in% c(1, 2, 3, 4, 5) ~ FALSE,
      race == 7 ~ NA,
      TRUE ~ NA
    ),
    # Employment checkbox codes follow the REDCap data dictionary.
    employed = case_when(
      employment_code %in% c(1, 2) ~ TRUE,
      employment_code %in% c(3, 4, 5, 6, 7, 8, 9) ~ FALSE,
      employment_code == 10 ~ NA,
      TRUE ~ NA
    ),
    unemployed = case_when(
      employment_code %in% c(3, 4, 9) ~ TRUE,
      employment_code %in% c(1, 2, 5, 6, 7, 8) ~ FALSE,
      employment_code == 10 ~ NA,
      TRUE ~ NA
    ),
    disability_payments = case_when(
      employment_code == 5 ~ TRUE,
      employment_code %in% c(1, 2, 3, 4, 6, 7, 8, 9) ~ FALSE,
      employment_code == 10 ~ NA,
      TRUE ~ NA
    ),
    retired = case_when(
      employment_code == 6 ~ TRUE,
      employment_code %in% c(1, 2, 3, 4, 5, 7, 8, 9) ~ FALSE,
      employment_code == 10 ~ NA,
      TRUE ~ NA
    ),
    homemaker = case_when(
      employment_code == 7 ~ TRUE,
      employment_code %in% c(1, 2, 3, 4, 5, 6, 8, 9) ~ FALSE,
      employment_code == 10 ~ NA,
      TRUE ~ NA
    ),
    college_grad_or_higher = case_when(
      education %in% c(17, 18, 19) ~ TRUE,
      education %in% 1:16 ~ FALSE,
      education == 20 ~ NA,
      TRUE ~ NA
    ),
    income_30k_or_more = case_when(
      income == 2 ~ TRUE,
      income == 1 ~ FALSE,
      income == 3 ~ NA,
      TRUE ~ NA
    ),
    always_medical_coverage = case_when(
      med_coverage == 1 ~ TRUE,
      med_coverage == 2 ~ FALSE,
      med_coverage == 3 ~ NA,
      TRUE ~ NA
    ),
    forgone_medical_care = case_when(
      seek_care == 1 ~ TRUE,
      seek_care == 2 ~ FALSE,
      seek_care == 3 ~ NA,
      TRUE ~ NA
    ),
    children_in_household = case_when(
      children == 1 ~ TRUE,
      children == 2 ~ FALSE,
      children == 3 ~ NA,
      TRUE ~ NA
    )
)

table_1_binary_recode_check <- function(label, source, derived, pna_code) {
  tibble(
    display_variable = label,
    raw_pna_n = sum(source == pna_code, na.rm = TRUE),
    expected_nonmissing_n = sum(!is.na(source) & source != pna_code),
    recoded_nonmissing_n = sum(!is.na(derived)),
    recoded_true_n = sum(derived, na.rm = TRUE),
    pna_rows_still_nonmissing = sum(!is.na(derived[source == pna_code])),
    status = ifelse(
      expected_nonmissing_n == recoded_nonmissing_n &&
        pna_rows_still_nonmissing == 0,
      "PASS",
      "FLAG"
    )
  )
}

table_1_binary_recode_checks <- bind_rows(
  table_1_binary_recode_check("female", baseline_df$gender, baseline_df$female, 4),
  table_1_binary_recode_check("male", baseline_df$gender, baseline_df$male, 4),
  table_1_binary_recode_check("hispanic_latino", baseline_df$ethinicty, baseline_df$hispanic_latino, 3),
  table_1_binary_recode_check("black_race", baseline_df$race, baseline_df$black_race, 7),
  table_1_binary_recode_check("asian_race", baseline_df$race, baseline_df$asian_race, 7),
  table_1_binary_recode_check("multiethnic_race", baseline_df$race, baseline_df$multiethnic_race, 7),
  table_1_binary_recode_check("white_race", baseline_df$race, baseline_df$white_race, 7),
  table_1_binary_recode_check("employed", baseline_df$employment_code, baseline_df$employed, 10),
  table_1_binary_recode_check("unemployed", baseline_df$employment_code, baseline_df$unemployed, 10),
  table_1_binary_recode_check("disability_payments", baseline_df$employment_code, baseline_df$disability_payments, 10),
  table_1_binary_recode_check("retired", baseline_df$employment_code, baseline_df$retired, 10),
  table_1_binary_recode_check("homemaker", baseline_df$employment_code, baseline_df$homemaker, 10),
  table_1_binary_recode_check("college_grad_or_higher", baseline_df$education, baseline_df$college_grad_or_higher, 20),
  table_1_binary_recode_check("income_30k_or_more", baseline_df$income, baseline_df$income_30k_or_more, 3),
  table_1_binary_recode_check("always_medical_coverage", baseline_df$med_coverage, baseline_df$always_medical_coverage, 3),
  table_1_binary_recode_check("forgone_medical_care", baseline_df$seek_care, baseline_df$forgone_medical_care, 3),
  table_1_binary_recode_check("children_in_household", baseline_df$children, baseline_df$children_in_household, 3)
)

readr::write_csv(table_1_binary_recode_checks, file.path(qa_dir, "qa_table_1_binary_recodes.csv"))
stopifnot(all(table_1_binary_recode_checks$status == "PASS"))

# One row per participant.
stopifnot(nrow(participants) == dplyr::n_distinct(participants$study_id))
stopifnot(nrow(baseline_df) == dplyr::n_distinct(baseline_df$study_id))

# Primary models.
mini_eat_primary <- baseline_adjusted_model(participants, "followup_mini_eat_score", "baseline_mini_eat_score")
pdq_primary <- baseline_adjusted_model(participants, "followup_pdq", "baseline_pdq")
phq_primary <- baseline_adjusted_model(participants, "followup_phq", "baseline_phq")
tapq_primary <- followup_only_model(participants, "tapq_score_0_100")
tsqm_effectiveness_primary <- followup_only_model(participants, "tsqm_effectiveness")
tsqm_convenience_primary <- followup_only_model(participants, "tsqm_convenience")
tsqm_global_primary <- followup_only_model(participants, "tsqm_global_satisfaction")

# Weight model: baseline + at least one post-baseline weight.
eligible_weight_ids <- weight_long %>%
  group_by(study_id, arm) %>%
  summarise(
    baseline_observed = any(month_since_baseline == 0 & !is.na(weight_kg)),
    postbaseline_observed = any(month_since_baseline > 0 & !is.na(weight_kg)),
    n_weight_observations = sum(!is.na(weight_kg)),
    .groups = "drop"
  ) %>%
  filter(baseline_observed, postbaseline_observed)

weight_model_data <- weight_long %>%
  semi_join(eligible_weight_ids, by = c("study_id", "arm")) %>%
  filter(month_since_baseline > 0, !is.na(weight_kg))

# Weight mixed model.
# pct_weight_change ~ arm * month_since_baseline + (1 | study_id)
# Random intercept = participant.
weight_model <- lmer(pct_weight_change ~ arm * month_since_baseline + (1 | study_id), data = weight_model_data)
weight_tests <- as.data.frame(anova(weight_model))
weight_tests$term <- rownames(weight_tests)
rownames(weight_tests) <- NULL

weight_coef <- as.data.frame(summary(weight_model)$coefficients)
weight_coef$term <- rownames(weight_coef)
rownames(weight_coef) <- NULL

weight_conf <- as.data.frame(confint(weight_model, parm = names(fixef(weight_model)), method = "Wald"))
weight_conf$term <- rownames(weight_conf)
rownames(weight_conf) <- NULL
names(weight_conf)[1:2] <- c("conf.low", "conf.high")

weight_fixed_effects <- weight_coef %>%
  left_join(weight_conf, by = "term")

model_months <- sort(unique(weight_model_data$month_since_baseline))
weight_means <- as.data.frame(summary(
  # Arm means at each observed follow-up month.
  emmeans(weight_model, ~ arm | month_since_baseline, at = list(month_since_baseline = model_months)),
  infer = TRUE
))
weight_mean_contrasts <- as.data.frame(summary(
  # Between-arm mean differences vs Control at each observed follow-up month.
  contrast(
    emmeans(weight_model, ~ arm | month_since_baseline, at = list(month_since_baseline = model_months)),
    "trt.vs.ctrl",
    ref = 1
  ),
  infer = TRUE,
  adjust = "none"
))
weight_slopes <- as.data.frame(summary(
  # Monthly slope within each arm.
  emtrends(weight_model, ~ arm, var = "month_since_baseline"),
  infer = TRUE
))
weight_slope_contrasts <- as.data.frame(summary(
  # Between-arm slope differences vs Control.
  contrast(emtrends(weight_model, ~ arm, var = "month_since_baseline"), "trt.vs.ctrl", ref = 1),
  infer = TRUE,
  adjust = "none"
))

# Sensitivity: adjust weight model for pre-randomization GLP-1 loss.
stopifnot("glp1_weight_loss_pct" %in% names(weight_model_data))

# Restrict to participants with GLP-1 loss data.
weight_glp1_complete_case_data <- weight_model_data %>%
  filter(!is.na(glp1_weight_loss_pct))

stopifnot(nrow(weight_glp1_complete_case_data) > 0)
stopifnot(dplyr::n_distinct(weight_glp1_complete_case_data$study_id) > length(arm_levels))

# Complete-case primary model.
weight_model_complete_case <- lmer(
  pct_weight_change ~ arm * month_since_baseline + (1 | study_id),
  data = weight_glp1_complete_case_data
)

# Add pre-randomization GLP-1 loss.
weight_model_glp1_adjusted <- lmer(
  pct_weight_change ~ arm * month_since_baseline + glp1_weight_loss_pct + (1 | study_id),
  data = weight_glp1_complete_case_data
)

# Add pre-randomization GLP-1 loss and baseline weight.
weight_glp1_baseline_complete_case_data <- weight_glp1_complete_case_data %>%
  filter(!is.na(baseline_weight_kg))

stopifnot(nrow(weight_glp1_baseline_complete_case_data) > 0)
stopifnot(dplyr::n_distinct(weight_glp1_baseline_complete_case_data$study_id) > length(arm_levels))

weight_model_glp1_baseline_adjusted <- lmer(
  pct_weight_change ~ arm * month_since_baseline + glp1_weight_loss_pct + baseline_weight_kg + (1 | study_id),
  data = weight_glp1_baseline_complete_case_data
)

# Summarize final-month means and monthly slopes.
summarise_weight_sensitivity_model <- function(model, label, model_data) {
  sensitivity_months <- sort(unique(model_data$month_since_baseline))
  final_sensitivity_month <- max(sensitivity_months, na.rm = TRUE)

  # Arm means by follow-up month.
  means <- as.data.frame(summary(
    emmeans(model, ~ arm | month_since_baseline, at = list(month_since_baseline = sensitivity_months)),
    infer = TRUE
  ))
  # Arm contrasts by follow-up month.
  mean_contrasts <- as.data.frame(summary(
    contrast(
      emmeans(model, ~ arm | month_since_baseline, at = list(month_since_baseline = sensitivity_months)),
      "trt.vs.ctrl",
      ref = 1
    ),
    infer = TRUE,
    adjust = "none"
  ))
  # Arm-specific monthly slopes.
  slopes <- as.data.frame(summary(
    emtrends(model, ~ arm, var = "month_since_baseline"),
    infer = TRUE
  ))
  # Slope contrasts vs Control.
  slope_contrasts <- as.data.frame(summary(
    contrast(emtrends(model, ~ arm, var = "month_since_baseline"), "trt.vs.ctrl", ref = 1),
    infer = TRUE,
    adjust = "none"
  ))

  # Keep final follow-up month.
  final_means <- means %>%
    filter(month_since_baseline == final_sensitivity_month)

  final_mean_contrasts <- mean_contrasts %>%
    filter(month_since_baseline == final_sensitivity_month)

  # Pull one arm estimate.
  get_arm_value <- function(df, arm_name, column_name) {
    df %>%
      filter(arm == arm_name) %>%
      pull(.data[[column_name]])
  }

  # Pull one contrast estimate.
  get_contrast <- function(df, contrast_name, column_name) {
    df %>%
      filter(contrast == contrast_name) %>%
      pull(.data[[column_name]])
  }

  bind_rows(
    tibble(
      outcome = "Weight change total, %",
      model = label,
      n_observations = nrow(model_data),
      n_participants = dplyr::n_distinct(model_data$study_id),
      control_estimate = get_arm_value(final_means, "Control", "emmean"),
      control_lower = get_arm_value(final_means, "Control", "lower.CL"),
      control_upper = get_arm_value(final_means, "Control", "upper.CL"),
      noom_estimate = get_arm_value(final_means, "Noom", "emmean"),
      noom_lower = get_arm_value(final_means, "Noom", "lower.CL"),
      noom_upper = get_arm_value(final_means, "Noom", "upper.CL"),
      mtm_estimate = get_arm_value(final_means, "MTM", "emmean"),
      mtm_lower = get_arm_value(final_means, "MTM", "lower.CL"),
      mtm_upper = get_arm_value(final_means, "MTM", "upper.CL"),
      noom_vs_control = get_contrast(final_mean_contrasts, "Noom - Control", "estimate"),
      noom_vs_control_lower = get_contrast(final_mean_contrasts, "Noom - Control", "lower.CL"),
      noom_vs_control_upper = get_contrast(final_mean_contrasts, "Noom - Control", "upper.CL"),
      noom_vs_control_p = get_contrast(final_mean_contrasts, "Noom - Control", "p.value"),
      mtm_vs_control = get_contrast(final_mean_contrasts, "MTM - Control", "estimate"),
      mtm_vs_control_lower = get_contrast(final_mean_contrasts, "MTM - Control", "lower.CL"),
      mtm_vs_control_upper = get_contrast(final_mean_contrasts, "MTM - Control", "upper.CL"),
      mtm_vs_control_p = get_contrast(final_mean_contrasts, "MTM - Control", "p.value")
    ),
    tibble(
      outcome = "Weight change slope, %/month",
      model = label,
      n_observations = nrow(model_data),
      n_participants = dplyr::n_distinct(model_data$study_id),
      control_estimate = get_arm_value(slopes, "Control", "month_since_baseline.trend"),
      control_lower = get_arm_value(slopes, "Control", "lower.CL"),
      control_upper = get_arm_value(slopes, "Control", "upper.CL"),
      noom_estimate = get_arm_value(slopes, "Noom", "month_since_baseline.trend"),
      noom_lower = get_arm_value(slopes, "Noom", "lower.CL"),
      noom_upper = get_arm_value(slopes, "Noom", "upper.CL"),
      mtm_estimate = get_arm_value(slopes, "MTM", "month_since_baseline.trend"),
      mtm_lower = get_arm_value(slopes, "MTM", "lower.CL"),
      mtm_upper = get_arm_value(slopes, "MTM", "upper.CL"),
      noom_vs_control = get_contrast(slope_contrasts, "Noom - Control", "estimate"),
      noom_vs_control_lower = get_contrast(slope_contrasts, "Noom - Control", "lower.CL"),
      noom_vs_control_upper = get_contrast(slope_contrasts, "Noom - Control", "upper.CL"),
      noom_vs_control_p = get_contrast(slope_contrasts, "Noom - Control", "p.value"),
      mtm_vs_control = get_contrast(slope_contrasts, "MTM - Control", "estimate"),
      mtm_vs_control_lower = get_contrast(slope_contrasts, "MTM - Control", "lower.CL"),
      mtm_vs_control_upper = get_contrast(slope_contrasts, "MTM - Control", "upper.CL"),
      mtm_vs_control_p = get_contrast(slope_contrasts, "MTM - Control", "p.value")
    )
  )
}

# Compare primary, complete-case, and sensitivity-adjusted models.
weight_sensitivity_results <- bind_rows(
  summarise_weight_sensitivity_model(weight_model, "Primary model", weight_model_data),
  summarise_weight_sensitivity_model(weight_model_complete_case, "Complete-case primary model", weight_glp1_complete_case_data),
  summarise_weight_sensitivity_model(weight_model_glp1_adjusted, "Adjusted for weight loss while on GLP-1", weight_glp1_complete_case_data),
  summarise_weight_sensitivity_model(weight_model_glp1_baseline_adjusted, "Adjusted for weight loss while on GLP-1 and baseline weight", weight_glp1_baseline_complete_case_data)
) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("Weight change total, %", "Weight change slope, %/month")
    ),
    model = factor(
      model,
      levels = c(
        "Primary model",
        "Complete-case primary model",
        "Adjusted for weight loss while on GLP-1",
        "Adjusted for weight loss while on GLP-1 and baseline weight"
      )
    )
  ) %>%
  arrange(outcome, model) %>%
  mutate(
    outcome = as.character(outcome),
    model = as.character(model)
  )

# Save adjusted fixed effects.
weight_glp1_adjusted_coef <- as.data.frame(summary(weight_model_glp1_adjusted)$coefficients)
weight_glp1_adjusted_coef$term <- rownames(weight_glp1_adjusted_coef)
rownames(weight_glp1_adjusted_coef) <- NULL

weight_glp1_adjusted_conf <- as.data.frame(confint(
  weight_model_glp1_adjusted,
  parm = names(fixef(weight_model_glp1_adjusted)),
  method = "Wald"
))
weight_glp1_adjusted_conf$term <- rownames(weight_glp1_adjusted_conf)
rownames(weight_glp1_adjusted_conf) <- NULL
names(weight_glp1_adjusted_conf)[1:2] <- c("conf.low", "conf.high")

weight_glp1_adjusted_fixed_effects <- weight_glp1_adjusted_coef %>%
  left_join(weight_glp1_adjusted_conf, by = "term")

weight_glp1_baseline_adjusted_coef <- as.data.frame(summary(weight_model_glp1_baseline_adjusted)$coefficients)
weight_glp1_baseline_adjusted_coef$term <- rownames(weight_glp1_baseline_adjusted_coef)
rownames(weight_glp1_baseline_adjusted_coef) <- NULL

weight_glp1_baseline_adjusted_conf <- as.data.frame(confint(
  weight_model_glp1_baseline_adjusted,
  parm = names(fixef(weight_model_glp1_baseline_adjusted)),
  method = "Wald"
))
weight_glp1_baseline_adjusted_conf$term <- rownames(weight_glp1_baseline_adjusted_conf)
rownames(weight_glp1_baseline_adjusted_conf) <- NULL
names(weight_glp1_baseline_adjusted_conf)[1:2] <- c("conf.low", "conf.high")

weight_glp1_baseline_adjusted_fixed_effects <- weight_glp1_baseline_adjusted_coef %>%
  left_join(weight_glp1_baseline_adjusted_conf, by = "term")

# Save model output for plots.
readr::write_csv(mini_eat_primary$coefficients, file.path(data_dir, "mini_eat_coefficients.csv"))
readr::write_csv(mini_eat_primary$adjusted_means, file.path(data_dir, "mini_eat_adjusted_means.csv"))
readr::write_csv(mini_eat_primary$control_contrasts, file.path(data_dir, "mini_eat_control_contrasts.csv"))

readr::write_csv(pdq_primary$coefficients, file.path(data_dir, "pdq_coefficients.csv"))
readr::write_csv(pdq_primary$adjusted_means, file.path(data_dir, "pdq_adjusted_means.csv"))
readr::write_csv(pdq_primary$all_contrasts, file.path(data_dir, "pdq_contrasts.csv"))

readr::write_csv(phq_primary$coefficients, file.path(data_dir, "phq_coefficients.csv"))
readr::write_csv(phq_primary$adjusted_means, file.path(data_dir, "phq_adjusted_means.csv"))
readr::write_csv(phq_primary$all_contrasts, file.path(data_dir, "phq_contrasts.csv"))

readr::write_csv(tapq_primary$coefficients, file.path(data_dir, "tapq_coefficients.csv"))
readr::write_csv(tapq_primary$adjusted_means, file.path(data_dir, "tapq_adjusted_means.csv"))
readr::write_csv(tapq_primary$all_contrasts, file.path(data_dir, "tapq_contrasts.csv"))

readr::write_csv(tsqm_effectiveness_primary$coefficients, file.path(data_dir, "tsqm_effectiveness_coefficients.csv"))
readr::write_csv(tsqm_effectiveness_primary$adjusted_means, file.path(data_dir, "tsqm_effectiveness_adjusted_means.csv"))
readr::write_csv(tsqm_effectiveness_primary$all_contrasts, file.path(data_dir, "tsqm_effectiveness_contrasts.csv"))

readr::write_csv(tsqm_convenience_primary$coefficients, file.path(data_dir, "tsqm_convenience_coefficients.csv"))
readr::write_csv(tsqm_convenience_primary$adjusted_means, file.path(data_dir, "tsqm_convenience_adjusted_means.csv"))
readr::write_csv(tsqm_convenience_primary$all_contrasts, file.path(data_dir, "tsqm_convenience_contrasts.csv"))

readr::write_csv(tsqm_global_primary$coefficients, file.path(data_dir, "tsqm_global_coefficients.csv"))
readr::write_csv(tsqm_global_primary$adjusted_means, file.path(data_dir, "tsqm_global_adjusted_means.csv"))
readr::write_csv(tsqm_global_primary$all_contrasts, file.path(data_dir, "tsqm_global_contrasts.csv"))

readr::write_csv(weight_fixed_effects, file.path(data_dir, "weight_fixed_effects.csv"))
readr::write_csv(weight_tests, file.path(data_dir, "weight_global_tests.csv"))
readr::write_csv(weight_means, file.path(data_dir, "weight_model_means.csv"))
readr::write_csv(weight_mean_contrasts, file.path(data_dir, "weight_model_mean_contrasts.csv"))
readr::write_csv(weight_slopes, file.path(data_dir, "weight_slopes_by_arm.csv"))
readr::write_csv(weight_slope_contrasts, file.path(data_dir, "weight_slope_contrasts.csv"))
readr::write_csv(weight_sensitivity_results, file.path(data_dir, "weight_glp1_loss_sensitivity_results.csv"))
readr::write_csv(weight_glp1_adjusted_fixed_effects, file.path(data_dir, "weight_glp1_loss_adjusted_fixed_effects.csv"))
readr::write_csv(weight_glp1_baseline_adjusted_fixed_effects, file.path(data_dir, "weight_glp1_loss_baseline_weight_adjusted_fixed_effects.csv"))

# Observed PDQ/PHQ summaries for plots.
pdq_numeric_summary <- questionnaire_long %>%
  filter(!is.na(pdq_score)) %>%
  group_by(arm, timepoint) %>%
  summarise(
    n = n(),
    mean = mean(pdq_score),
    sd = sd(pdq_score),
    .groups = "drop"
  )

phq_numeric_summary <- questionnaire_long %>%
  filter(!is.na(phq_score)) %>%
  group_by(arm, timepoint) %>%
  summarise(
    n = n(),
    mean = mean(phq_score),
    sd = sd(phq_score),
    .groups = "drop"
  )

readr::write_csv(pdq_numeric_summary, file.path(data_dir, "pdq_numeric_summary.csv"))
readr::write_csv(phq_numeric_summary, file.path(data_dir, "phq_numeric_summary.csv"))

# Diagnostic plots.
save_lm_diagnostics(mini_eat_primary$model, file.path(diagnostic_dir, "mini_eat_primary_diagnostics.pdf"))
save_lm_diagnostics(pdq_primary$model, file.path(diagnostic_dir, "pdq_primary_diagnostics.pdf"))
save_lm_diagnostics(phq_primary$model, file.path(diagnostic_dir, "phq_primary_diagnostics.pdf"))
save_lm_diagnostics(tapq_primary$model, file.path(diagnostic_dir, "tapq_primary_diagnostics.pdf"))
save_lm_diagnostics(tsqm_effectiveness_primary$model, file.path(diagnostic_dir, "tsqm_effectiveness_diagnostics.pdf"))
save_lm_diagnostics(tsqm_convenience_primary$model, file.path(diagnostic_dir, "tsqm_convenience_diagnostics.pdf"))
save_lm_diagnostics(tsqm_global_primary$model, file.path(diagnostic_dir, "tsqm_global_diagnostics.pdf"))
save_weight_diagnostics(weight_model, file.path(diagnostic_dir, "weight_primary_diagnostics.pdf"))
save_weight_diagnostics(weight_model_glp1_adjusted, file.path(diagnostic_dir, "weight_glp1_loss_adjusted_sensitivity_diagnostics.pdf"))

mini_terms <- c(
  "(Intercept)" = "Intercept",
  "armNoom" = display_noom_text("Noom vs Control"),
  "armMTM" = "MTM vs Control",
  "baseline_mini_eat_score" = "Baseline Mini-EAT"
)

mini_tests_terms <- c(
  "arm" = "Arm",
  "baseline_mini_eat_score" = "Baseline Mini-EAT"
)

# Mini-EAT PDF.
mini_tex <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.75in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{array}",
  "\\usepackage{float}",
  "\\usepackage[table]{xcolor}",
  "\\setlength{\\parindent}{0pt}",
  "\\begin{document}",
  "{\\centering{\\LARGE\\bfseries Mini-EAT Baseline-Adjusted Linear Regression (ANCOVA)\\par}}",
  "\\vspace{0.5em}",
  "This table reports the baseline-adjusted linear regression (ANCOVA) for follow-up Mini-EAT score, with randomized arm and baseline Mini-EAT score as predictors.",
  "",
  latex_table(lm_coefficients_pdf(mini_eat_primary, mini_terms), "Fixed-effect regression coefficients", c("4.3cm", "1.6cm", "1.6cm", "4.0cm", "1.2cm", "1.3cm", "1.5cm"), font_size = 8.9),
  "",
  latex_table(lm_omnibus_pdf(mini_eat_primary, mini_tests_terms), "Global fixed-effect tests", c("5.2cm", "1.7cm", "1.7cm", "1.5cm"), font_size = 9),
  "\\end{document}"
)
write_pdf_from_tex(mini_tex, file.path(table_support_dir, "Model Detail - Mini-EAT Primary Results.pdf"))

# Weight PDF: fixed effects + omnibus tests.
weight_fixed_pdf <- weight_fixed_effects %>%
  transmute(
    term,
    estimate = Estimate,
    std.error = `Std. Error`,
    df = df,
    statistic = `t value`,
    p.value = `Pr(>|t|)`,
    conf.low,
    conf.high
  ) %>%
  transmute(
    Term = dplyr::recode(
      term,
      "(Intercept)" = "Intercept",
      "armNoom" = display_noom_text("Noom vs Control"),
      "armMTM" = "MTM vs Control",
      "month_since_baseline" = "Month since baseline",
      "armNoom:month_since_baseline" = display_noom_text("Noom x month since baseline"),
      "armMTM:month_since_baseline" = "MTM x month since baseline"
    ),
    `B` = format_number(estimate),
    `SE` = format_number(std.error),
    `95\\% CI` = paste0(format_number(conf.low), " to ", format_number(conf.high)),
    `df` = format_number(df, 1),
    `t` = format_number(statistic),
    `P` = format_p_latex(p.value)
  )

derived_slope_pdf <- weight_slopes %>%
  filter(arm != "Control") %>%
  transmute(
    Term = paste0(display_arm_label(arm), " month since baseline"),
    `B` = format_number(month_since_baseline.trend),
    `SE` = format_number(SE),
    `95\\% CI` = paste0(format_number(lower.CL), " to ", format_number(upper.CL)),
    `df` = format_number(df, 1),
    `t` = format_number(t.ratio),
    `P` = format_p_latex(p.value)
  )

weight_fixed_pdf <- bind_rows(weight_fixed_pdf, derived_slope_pdf)

weight_tests_pdf <- weight_tests %>%
  transmute(
    Term = dplyr::recode(term, "arm" = "Arm", "month_since_baseline" = "Month since baseline", "arm:month_since_baseline" = "Arm x month"),
    `Num df` = NumDF,
    `Den df` = format_number(DenDF, 1),
    `F` = format_number(`F value`),
    `P` = format_p_latex(`Pr(>F)`)
  )

weight_tex <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.75in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{array}",
  "\\usepackage{float}",
  "\\usepackage[table]{xcolor}",
  "\\setlength{\\parindent}{0pt}",
  "\\begin{document}",
  "{\\centering{\\LARGE\\bfseries Weight Change Linear Mixed-Effects Model\\par}}",
  "\\vspace{0.5em}",
  "This table reports the linear mixed-effects model for percent weight change from baseline, with trial arm, month since baseline, and arm:month interaction as fixed effects, with a random participant intercept.",
  "",
  latex_table(weight_fixed_pdf, "Fixed-effect regression coefficients", c("4.6cm", "1.6cm", "1.6cm", "4.0cm", "1.2cm", "1.3cm", "1.5cm"), font_size = 8.7),
  "",
  latex_table(weight_tests_pdf, "Global fixed-effect tests", c("5.0cm", "1.4cm", "1.7cm", "1.5cm", "1.5cm"), font_size = 9),
  "\\end{document}"
)
write_pdf_from_tex(weight_tex, file.path(table_support_dir, "Model Detail - Weight Primary Results.pdf"))

pdq_terms <- c(
  "(Intercept)" = "Intercept",
  "armNoom" = display_noom_text("Noom vs Control"),
  "armMTM" = "MTM vs Control",
  "baseline_pdq" = "Baseline PDQ"
)

phq_terms <- c(
  "(Intercept)" = "Intercept",
  "armNoom" = display_noom_text("Noom vs Control"),
  "armMTM" = "MTM vs Control",
  "baseline_phq" = "Baseline PHQ"
)

# PDQ/PHQ PDF.
pdq_phq_tex <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.75in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{array}",
  "\\usepackage{float}",
  "\\usepackage[table]{xcolor}",
  "\\setlength{\\parindent}{0pt}",
  "\\begin{document}",
  "{\\centering{\\LARGE\\bfseries PDQ and PHQ Baseline-Adjusted Linear Regression (ANCOVA)\\par}}",
  "\\vspace{0.5em}",
  "This document reports the baseline-adjusted linear regression (ANCOVA) models for follow-up PDQ and PHQ. Lower values indicate better perceived diet or health quality.",
  "",
  pdf_section("PDQ", lm_coefficients_pdf(pdq_primary, pdq_terms), lm_omnibus_pdf(pdq_primary, c("arm" = "Arm", "baseline_pdq" = "Baseline PDQ"))),
  pdf_section("PHQ", lm_coefficients_pdf(phq_primary, phq_terms), lm_omnibus_pdf(phq_primary, c("arm" = "Arm", "baseline_phq" = "Baseline PHQ"))),
  "\\end{document}"
)
write_pdf_from_tex(pdq_phq_tex, file.path(table_support_dir, "Model Detail - PDQ and PHQ Primary Results.pdf"))

followup_terms <- c(
  "(Intercept)" = "Intercept",
  "armNoom" = display_noom_text("Noom vs Control"),
  "armMTM" = "MTM vs Control"
)

# TAPQ/TSQM PDF.
tapq_tsqm_tex <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.75in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{array}",
  "\\usepackage{float}",
  "\\usepackage[table]{xcolor}",
  "\\setlength{\\parindent}{0pt}",
  "\\begin{document}",
  "{\\centering{\\LARGE\\bfseries TAPQ and TSQM Linear Regression Models\\par}}",
  "\\vspace{0.5em}",
  "This document reports the follow-up-only linear regression models for TAPQ perceived behavior and the non-side-effect TSQM-II domains.",
  "",
  pdf_section("TAPQ Perceived Behavior", lm_coefficients_pdf(tapq_primary, followup_terms), lm_omnibus_pdf(tapq_primary, c("arm" = "Arm"))),
  pdf_section("TSQM Effectiveness", lm_coefficients_pdf(tsqm_effectiveness_primary, followup_terms), lm_omnibus_pdf(tsqm_effectiveness_primary, c("arm" = "Arm"))),
  pdf_section("TSQM Convenience", lm_coefficients_pdf(tsqm_convenience_primary, followup_terms), lm_omnibus_pdf(tsqm_convenience_primary, c("arm" = "Arm"))),
  pdf_section("TSQM Global Satisfaction", lm_coefficients_pdf(tsqm_global_primary, followup_terms), lm_omnibus_pdf(tsqm_global_primary, c("arm" = "Arm"))),
  "\\end{document}"
)
write_pdf_from_tex(tapq_tsqm_tex, file.path(table_support_dir, "Model Detail - TAPQ and TSQM Primary Results.pdf"))

arm_n <- baseline_df %>%
  count(arm, .drop = FALSE) %>%
  mutate(header = sprintf("%s\n(n = %d)", arm, n))

# Header paragraph helpers.
header_linebreak <- function() {
  as_chunk("\n")
}

header_chunk <- function(text, size, bold = TRUE) {
  as_chunk(
    text,
    props = fp_text(
      font.family = "Times New Roman",
      font.size = size,
      bold = bold
    )
  )
}

# Table 1 row helpers.
cont_row <- function(label, column, digits = 1) {
  # Continuous row.
  tibble(
    characteristic = paste0("   ", label),
    Control = fmt_mean_sd(baseline_df %>% filter(arm == "Control") %>% pull({{ column }}), digits),
    Noom = fmt_mean_sd(baseline_df %>% filter(arm == "Noom") %>% pull({{ column }}), digits),
    MTM = fmt_mean_sd(baseline_df %>% filter(arm == "MTM") %>% pull({{ column }}), digits),
    Total = fmt_mean_sd(baseline_df %>% pull({{ column }}), digits),
    row_type = "data"
  )
}

bin_row <- function(label, column, indent = 1) {
  # Binary row.
  tibble(
    characteristic = paste0(strrep("   ", indent), label),
    Control = fmt_n_pct(baseline_df %>% filter(arm == "Control") %>% pull({{ column }})),
    Noom = fmt_n_pct(baseline_df %>% filter(arm == "Noom") %>% pull({{ column }})),
    MTM = fmt_n_pct(baseline_df %>% filter(arm == "MTM") %>% pull({{ column }})),
    Total = fmt_n_pct(baseline_df %>% pull({{ column }})),
    row_type = "data"
  )
}

group_row <- function(label) {
  # Section row.
  tibble(
    characteristic = label,
    Control = "",
    Noom = "",
    MTM = "",
    Total = "",
    row_type = "group"
  )
}

subgroup_row <- function(label) {
  # Variable label row within a section.
  tibble(
    characteristic = paste0("   ", label),
    Control = "",
    Noom = "",
    MTM = "",
    Total = "",
    row_type = "subgroup"
  )
}

# Table 1. Descriptive only.
table_1_df <- bind_rows(
  group_row("Demographic and household characteristics"),
  cont_row("Age, y", age, 1),
  subgroup_row("Sex"),
  bin_row("Female", female, indent = 2),
  bin_row("Male", male, indent = 2),
  subgroup_row("Race/ethnicity"),
  bin_row("White race", white_race, indent = 2),
  bin_row("African American or Black race", black_race, indent = 2),
  bin_row("Hispanic/Latino ethnicity", hispanic_latino, indent = 2),
  bin_row("Asian race", asian_race, indent = 2),
  bin_row("Multiethnic or more than one race", multiethnic_race, indent = 2),
  subgroup_row("Employment"),
  bin_row("Employed (full-time or part-time)", employed, indent = 2),
  bin_row("Unemployed", unemployed, indent = 2),
  bin_row("Receiving disability payments", disability_payments, indent = 2),
  bin_row("Retired", retired, indent = 2),
  bin_row("Homemaker", homemaker, indent = 2),
  bin_row("College graduate or higher", college_grad_or_higher),
  bin_row("Household income ≥ $30,000", income_30k_or_more),
  bin_row("Always had health insurance or medical coverage", always_medical_coverage),
  bin_row("Forgone medical care in past 2 years", forgone_medical_care),
  bin_row("Children in household", children_in_household),
  cont_row("Household size, no.", ppl_home, 1),
  group_row("Baseline clinical and questionnaire measures"),
  cont_row("Weight, kg", baseline_weight_kg, 1),
  cont_row("Weight loss while on GLP-1, %", glp1_weight_loss_pct, 1),
  cont_row("BMI, kg/m²", baseline_bmi_kg_m2, 1),
  cont_row("Mini-EAT score", baseline_mini_eat_score, 1),
  cont_row("PDQ (1 = Excellent, 5 = Poor)", baseline_pdq, 1),
  cont_row("PHQ (1 = Excellent, 5 = Poor)", baseline_phq, 1)
)

table_1_group_rows <- which(table_1_df$row_type == "group")
table_1_subgroup_rows <- which(table_1_df$row_type == "subgroup")

table_1_ft <- flextable(table_1_df, col_keys = c("characteristic", "Control", "Noom", "MTM", "Total")) %>%
  add_table_style(
    # Wider first column for row labels.
    group_rows = table_1_group_rows,
    first_col_width = 3.4,
    other_width = 1.25,
    use_autofit = TRUE
  ) %>%
  bold(i = table_1_subgroup_rows, bold = FALSE, part = "body") %>%
  italic(i = table_1_subgroup_rows, italic = FALSE, part = "body") %>%
  bg(i = table_1_subgroup_rows, bg = "white", part = "body") %>%
  set_header_labels(
    characteristic = "",
    Control = "",
    Noom = "",
    MTM = "",
    Total = ""
  ) %>%
  font(fontname = "Times New Roman", part = "header") %>%
  fontsize(size = 9, part = "header") %>%
  align(align = "left", j = 1, part = "header") %>%
  align(align = "center", j = 2:5, part = "header") %>%
  bold(part = "header") %>%
  compose(
    i = 1,
    j = "characteristic",
    part = "header",
    value = as_paragraph(header_chunk("Characteristic", size = 9, bold = TRUE))
  ) %>%
  compose(
    i = 1,
    j = "Control",
    part = "header",
    value = as_paragraph(
      header_chunk("Control", size = 9, bold = TRUE),
      header_linebreak(),
      header_chunk(sprintf("(n = %d)", arm_n$n[arm_n$arm == "Control"]), size = 9, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "Noom",
    part = "header",
    value = as_paragraph(
      header_chunk(noom_display_label, size = 9, bold = TRUE),
      header_linebreak(),
      header_chunk(sprintf("(n = %d)", arm_n$n[arm_n$arm == "Noom"]), size = 9, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "MTM",
    part = "header",
    value = as_paragraph(
      header_chunk("MTM", size = 9, bold = TRUE),
      header_linebreak(),
      header_chunk(sprintf("(n = %d)", arm_n$n[arm_n$arm == "MTM"]), size = 9, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "Total",
    part = "header",
    value = as_paragraph(
      header_chunk("Total", size = 9, bold = TRUE),
      header_linebreak(),
      header_chunk(sprintf("(N = %d)", nrow(baseline_df)), size = 9, bold = FALSE)
    )
  )

# Write CSV + DOCX.
readr::write_csv(
  table_1_df %>%
    select(-row_type) %>%
    rename_noom_display_columns(),
  file.path(table_support_dir, "Table 1 - Baseline Characteristics.csv")
)

# Table 1 fits in portrait.
portrait_section <- prop_section(
  page_size = page_size(orient = "portrait"),
  page_margins = page_mar(top = 0.75, bottom = 0.75, left = 0.75, right = 0.75)
)

write_table_docx(
  ft = table_1_ft,
  title = "Table 1. Baseline Characteristics of Participants by Randomized Arm in a GLP-1 Cessation Support Pilot Trial",
  docx_file = file.path(table_dir, "Table 1 - Baseline Characteristics.docx"),
  section = portrait_section
)

# Table 2 helpers.
read_means <- function(df, estimate_col = "emmean", digits = 2) {
  # Convert `emmeans` output to one cell per arm.
  df %>%
    mutate(value = fmt_est_ci(.data[[estimate_col]], lower.CL, upper.CL, digits)) %>%
    select(arm, value)
}

fmt_table_2_est_ci <- function(x, digits = 2) {
  fmt_est_ci(x$estimate, x$lower.CL, x$upper.CL, digits = digits)
}

extract_vs_control <- function(df, arm_name) {
  # Always return arm minus Control.
  row <- df %>%
    filter(str_detect(contrast, arm_name), str_detect(contrast, "Control")) %>%
    slice(1)

  stopifnot(nrow(row) == 1)

  first_is_arm <- str_detect(row$contrast, paste0("^", arm_name))
  if (first_is_arm) {
    estimate <- row$estimate
    lower <- row$lower.CL
    upper <- row$upper.CL
  } else {
    estimate <- -row$estimate
    lower <- -row$upper.CL
    upper <- -row$lower.CL
  }

  tibble(estimate = estimate, lower.CL = lower, upper.CL = upper, p.value = row$p.value)
}

# Table 2 rows.
table_2_rows <- bind_rows(
  tibble(
    outcome = "Baseline-adjusted follow-up outcomes",
    Control = "", Noom = "", MTM = "", `Noom vs Control` = "", `P (Noom)` = "", `MTM vs Control` = "", `P (MTM)` = "",
    is_group = TRUE
  ),
  {
    means <- read_means(mini_eat_primary$adjusted_means, digits = 1)
    noom <- extract_vs_control(mini_eat_primary$control_contrasts, "Noom")
    mtm <- extract_vs_control(mini_eat_primary$control_contrasts, "MTM")
    tibble(
      outcome = "   Mini-EAT score",
      Control = means$value[means$arm == "Control"],
      Noom = means$value[means$arm == "Noom"],
      MTM = means$value[means$arm == "MTM"],
      `Noom vs Control` = fmt_table_2_est_ci(noom, digits = 1),
      `P (Noom)` = fmt_p(noom$p.value),
      `MTM vs Control` = fmt_table_2_est_ci(mtm, digits = 1),
      `P (MTM)` = fmt_p(mtm$p.value),
      is_group = FALSE
    )
  },
  {
    means <- read_means(pdq_primary$adjusted_means)
    noom <- extract_vs_control(pdq_primary$all_contrasts, "Noom")
    mtm <- extract_vs_control(pdq_primary$all_contrasts, "MTM")
    tibble(
      outcome = "   PDQ (lower = better)",
      Control = means$value[means$arm == "Control"],
      Noom = means$value[means$arm == "Noom"],
      MTM = means$value[means$arm == "MTM"],
      `Noom vs Control` = fmt_est_ci(noom$estimate, noom$lower.CL, noom$upper.CL),
      `P (Noom)` = fmt_p(noom$p.value),
      `MTM vs Control` = fmt_est_ci(mtm$estimate, mtm$lower.CL, mtm$upper.CL),
      `P (MTM)` = fmt_p(mtm$p.value),
      is_group = FALSE
    )
  },
  {
    means <- read_means(phq_primary$adjusted_means)
    noom <- extract_vs_control(phq_primary$all_contrasts, "Noom")
    mtm <- extract_vs_control(phq_primary$all_contrasts, "MTM")
    tibble(
      outcome = "   PHQ (lower = better)",
      Control = means$value[means$arm == "Control"],
      Noom = means$value[means$arm == "Noom"],
      MTM = means$value[means$arm == "MTM"],
      `Noom vs Control` = fmt_est_ci(noom$estimate, noom$lower.CL, noom$upper.CL),
      `P (Noom)` = fmt_p(noom$p.value),
      `MTM vs Control` = fmt_est_ci(mtm$estimate, mtm$lower.CL, mtm$upper.CL),
      `P (MTM)` = fmt_p(mtm$p.value),
      is_group = FALSE
    )
  },
  tibble(
    outcome = "Follow-up-only outcomes",
    Control = "", Noom = "", MTM = "", `Noom vs Control` = "", `P (Noom)` = "", `MTM vs Control` = "", `P (MTM)` = "",
    is_group = TRUE
  ),
  {
    means <- read_means(tapq_primary$adjusted_means, digits = 1)
    noom <- extract_vs_control(tapq_primary$all_contrasts, "Noom")
    mtm <- extract_vs_control(tapq_primary$all_contrasts, "MTM")
    tibble(
      outcome = "   TAPQ perceived behavior, 0-100",
      Control = means$value[means$arm == "Control"],
      Noom = means$value[means$arm == "Noom"],
      MTM = means$value[means$arm == "MTM"],
      `Noom vs Control` = fmt_table_2_est_ci(noom, digits = 1),
      `P (Noom)` = fmt_p(noom$p.value),
      `MTM vs Control` = fmt_table_2_est_ci(mtm, digits = 1),
      `P (MTM)` = fmt_p(mtm$p.value),
      is_group = FALSE
    )
  },
  {
    means <- read_means(tsqm_effectiveness_primary$adjusted_means, digits = 1)
    noom <- extract_vs_control(tsqm_effectiveness_primary$all_contrasts, "Noom")
    mtm <- extract_vs_control(tsqm_effectiveness_primary$all_contrasts, "MTM")
    tibble(
      outcome = "   TSQM effectiveness, 0-100",
      Control = means$value[means$arm == "Control"],
      Noom = means$value[means$arm == "Noom"],
      MTM = means$value[means$arm == "MTM"],
      `Noom vs Control` = fmt_table_2_est_ci(noom, digits = 1),
      `P (Noom)` = fmt_p(noom$p.value),
      `MTM vs Control` = fmt_table_2_est_ci(mtm, digits = 1),
      `P (MTM)` = fmt_p(mtm$p.value),
      is_group = FALSE
    )
  },
  {
    means <- read_means(tsqm_convenience_primary$adjusted_means, digits = 1)
    noom <- extract_vs_control(tsqm_convenience_primary$all_contrasts, "Noom")
    mtm <- extract_vs_control(tsqm_convenience_primary$all_contrasts, "MTM")
    tibble(
      outcome = "   TSQM convenience, 0-100",
      Control = means$value[means$arm == "Control"],
      Noom = means$value[means$arm == "Noom"],
      MTM = means$value[means$arm == "MTM"],
      `Noom vs Control` = fmt_table_2_est_ci(noom, digits = 1),
      `P (Noom)` = fmt_p(noom$p.value),
      `MTM vs Control` = fmt_table_2_est_ci(mtm, digits = 1),
      `P (MTM)` = fmt_p(mtm$p.value),
      is_group = FALSE
    )
  },
  {
    means <- read_means(tsqm_global_primary$adjusted_means, digits = 1)
    noom <- extract_vs_control(tsqm_global_primary$all_contrasts, "Noom")
    mtm <- extract_vs_control(tsqm_global_primary$all_contrasts, "MTM")
    tibble(
      outcome = "   TSQM global satisfaction, 0-100",
      Control = means$value[means$arm == "Control"],
      Noom = means$value[means$arm == "Noom"],
      MTM = means$value[means$arm == "MTM"],
      `Noom vs Control` = fmt_table_2_est_ci(noom, digits = 1),
      `P (Noom)` = fmt_p(noom$p.value),
      `MTM vs Control` = fmt_table_2_est_ci(mtm, digits = 1),
      `P (MTM)` = fmt_p(mtm$p.value),
      is_group = FALSE
    )
  },
  tibble(
    outcome = "Longitudinal weight outcome",
    Control = "", Noom = "", MTM = "", `Noom vs Control` = "", `P (Noom)` = "", `MTM vs Control` = "", `P (MTM)` = "",
    is_group = TRUE
  ),
  {
    # Total weight change row: model-based Month 4 estimate from the longitudinal model.
    final_model_month <- max(weight_means$month_since_baseline, na.rm = TRUE)
    means <- weight_means %>%
      filter(month_since_baseline == final_model_month) %>%
      transmute(arm, value = fmt_est_ci(emmean, lower.CL, upper.CL))
    final_contrasts <- weight_mean_contrasts %>%
      filter(month_since_baseline == final_model_month)
    noom <- extract_vs_control(final_contrasts, "Noom")
    mtm <- extract_vs_control(final_contrasts, "MTM")
    tibble(
      outcome = "   Weight change total, %",
      Control = means$value[means$arm == "Control"],
      Noom = means$value[means$arm == "Noom"],
      MTM = means$value[means$arm == "MTM"],
      `Noom vs Control` = fmt_est_ci(noom$estimate, noom$lower.CL, noom$upper.CL),
      `P (Noom)` = fmt_p(noom$p.value),
      `MTM vs Control` = fmt_est_ci(mtm$estimate, mtm$lower.CL, mtm$upper.CL),
      `P (MTM)` = fmt_p(mtm$p.value),
      is_group = FALSE
    )
  },
  {
    # Weight slope row: model-based monthly rate from the longitudinal model.
    means <- weight_slopes %>%
      transmute(arm, value = fmt_est_ci(month_since_baseline.trend, lower.CL, upper.CL))
    noom <- extract_vs_control(weight_slope_contrasts, "Noom")
    mtm <- extract_vs_control(weight_slope_contrasts, "MTM")
    tibble(
      outcome = "   Weight change slope, %/month",
      Control = means$value[means$arm == "Control"],
      Noom = means$value[means$arm == "Noom"],
      MTM = means$value[means$arm == "MTM"],
      `Noom vs Control` = fmt_est_ci(noom$estimate, noom$lower.CL, noom$upper.CL),
      `P (Noom)` = fmt_p(noom$p.value),
      `MTM vs Control` = fmt_est_ci(mtm$estimate, mtm$lower.CL, mtm$upper.CL),
      `P (MTM)` = fmt_p(mtm$p.value),
      is_group = FALSE
    )
  }
)

table_2_ft <- flextable(
  table_2_rows,
  col_keys = c("outcome", "Control", "Noom", "MTM", "Noom vs Control", "P (Noom)", "MTM vs Control", "P (MTM)")
) %>%
  add_table_style(
    # Manual widths for the wide table.
    group_rows = which(table_2_rows$is_group),
    first_col_width = 2.0,
    other_width = 1.0,
    font_size = 8.0,
    padding_h = 1,
    padding_v = 1
  ) %>%
  # Column widths for one landscape page.
  width(j = "outcome", width = 1.95) %>%
  width(j = c("Control", "Noom", "MTM"), width = 1.22) %>%
  width(j = c("Noom vs Control", "MTM vs Control"), width = 1.38) %>%
  width(j = c("P (Noom)", "P (MTM)"), width = 0.42) %>%
  # Tight line spacing for fit.
  line_spacing(space = 0.9, part = "all") %>%
  set_header_labels(
    outcome = "",
    Control = "",
    Noom = "",
    MTM = "",
    `Noom vs Control` = "",
    `P (Noom)` = "",
    `MTM vs Control` = "",
    `P (MTM)` = ""
  ) %>%
  font(fontname = "Times New Roman", part = "header") %>%
  fontsize(size = 8.0, part = "header") %>%
  align(align = "left", j = 1, part = "header") %>%
  align(align = "center", j = 2:8, part = "header") %>%
  bold(part = "header") %>%
  compose(
    i = 1,
    j = "outcome",
    part = "header",
    value = as_paragraph(header_chunk("Outcome", size = 8.0, bold = TRUE))
  ) %>%
  compose(
    i = 1,
    j = "Control",
    part = "header",
    value = as_paragraph(
      header_chunk("Control", size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("Estimate", size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("(95% CI)", size = 8.0, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "Noom",
    part = "header",
    value = as_paragraph(
      header_chunk(noom_display_label, size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("Estimate", size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("(95% CI)", size = 8.0, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "MTM",
    part = "header",
    value = as_paragraph(
      header_chunk("MTM", size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("Estimate", size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("(95% CI)", size = 8.0, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "Noom vs Control",
    part = "header",
    value = as_paragraph(
      header_chunk(display_noom_text("Noom vs Control"), size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("Difference", size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("(95% CI)", size = 8.0, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "P (Noom)",
    part = "header",
    value = as_paragraph(header_chunk("P", size = 8.0, bold = TRUE))
  ) %>%
  compose(
    i = 1,
    j = "MTM vs Control",
    part = "header",
    value = as_paragraph(
      header_chunk("MTM vs Control", size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("Difference", size = 8.0, bold = TRUE),
      header_linebreak(),
      header_chunk("(95% CI)", size = 8.0, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "P (MTM)",
    part = "header",
    value = as_paragraph(header_chunk("P", size = 8.0, bold = TRUE))
  )

# Table 2 and sensitivity tables require landscape.
landscape_section <- prop_section(
  page_size = page_size(orient = "landscape"),
  page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.35, right = 0.35)
)

format_est_p <- function(estimate, lower, upper, p_value) {
  paste0(fmt_est_ci(estimate, lower, upper), "; P = ", vapply(p_value, fmt_p, character(1)))
}

sup_chunk <- function(text, size = 5.5) {
  as_chunk(
    text,
    props = fp_text(
      font.family = "Times New Roman",
      font.size = size,
      vertical.align = "superscript"
    )
  )
}

weight_sensitivity_model_lookup <- tibble(
  model = c(
    "Primary model",
    "Complete-case primary model",
    "Adjusted for weight loss while on GLP-1",
    "Adjusted for weight loss while on GLP-1 and baseline weight"
  ),
  model_label = c("Primary", "Complete-case", "GLP-1 adjusted", "GLP-1 + baseline weight")
)

# Group heading row for the sensitivity table.
make_sensitivity_group_row <- function(outcome) {
  tibble(
    row_type = "group",
    Outcome = outcome,
    Model = "",
    Sample = "",
    Control = "",
    Noom = "",
    MTM = "",
    `Noom vs Control` = "",
    `MTM vs Control` = ""
  )
}

# Build display rows from model results.
weight_sensitivity_detail_rows <- weight_sensitivity_results %>%
  mutate(
    outcome_order = case_when(
      outcome == "Weight change total, %" ~ 1,
      outcome == "Weight change slope, %/month" ~ 2,
      TRUE ~ 99
    )
  ) %>%
  left_join(weight_sensitivity_model_lookup, by = "model") %>%
  group_by(outcome) %>%
  mutate(model_order = row_number()) %>%
  ungroup() %>%
  arrange(outcome_order, model_order) %>%
  transmute(
    row_type = "detail",
    Outcome = "",
    Model = model_label,
    Sample = paste0(n_participants, "/", n_observations),
    Control = fmt_est_ci(control_estimate, control_lower, control_upper),
    Noom = fmt_est_ci(noom_estimate, noom_lower, noom_upper),
    MTM = fmt_est_ci(mtm_estimate, mtm_lower, mtm_upper),
    `Noom vs Control` = format_est_p(noom_vs_control, noom_vs_control_lower, noom_vs_control_upper, noom_vs_control_p),
    `MTM vs Control` = format_est_p(mtm_vs_control, mtm_vs_control_lower, mtm_vs_control_upper, mtm_vs_control_p)
  )

weight_sensitivity_table_rows <- bind_rows(
  make_sensitivity_group_row("Weight change total, %"),
  weight_sensitivity_detail_rows[1:4, ],
  make_sensitivity_group_row("Weight change slope, %/month"),
  weight_sensitivity_detail_rows[5:8, ]
)

# Add superscripts for footnotes.
weight_sensitivity_table_display <- weight_sensitivity_table_rows %>%
  select(-row_type) %>%
  mutate(
    Model = case_when(
      Model == "Primary" ~ paste0("Primary", "\u00B9"),
      Model == "Complete-case" ~ paste0("Complete-case", "\u00B2"),
      Model == "GLP-1 adjusted" ~ paste0("GLP-1 adjusted", "\u00B3"),
      Model == "GLP-1 + baseline weight" ~ paste0("GLP-1 + baseline weight", "\u2074"),
      TRUE ~ Model
    )
  ) %>%
  rename_noom_display_columns()

readr::write_csv(weight_sensitivity_table_display, file.path(table_support_dir, "SI Table 1 - Weight Sensitivity Analysis.csv"))

# Locate rows for table styling.
weight_sensitivity_group_rows <- which(weight_sensitivity_table_rows$row_type == "group")
weight_sensitivity_primary_rows <- which(weight_sensitivity_table_rows$Model == "Primary")
weight_sensitivity_complete_case_rows <- which(weight_sensitivity_table_rows$Model == "Complete-case")
weight_sensitivity_glp1_adjusted_rows <- which(weight_sensitivity_table_rows$Model == "GLP-1 adjusted")
weight_sensitivity_glp1_baseline_rows <- which(weight_sensitivity_table_rows$Model == "GLP-1 + baseline weight")

# Format GLP-1 sensitivity DOCX table.
weight_sensitivity_ft <- flextable(
  weight_sensitivity_table_rows,
  col_keys = c("Outcome", "Model", "Sample", "Control", "Noom", "MTM", "Noom vs Control", "MTM vs Control")
) %>%
  add_table_style(
    group_rows = weight_sensitivity_group_rows,
    first_col_width = 1.45,
    other_width = 1.0,
    font_size = 7.3,
    padding_h = 2,
    padding_v = 2
  ) %>%
  set_header_labels(
    Outcome = "",
    Model = "",
    Sample = "",
    Control = "",
    Noom = "",
    MTM = "",
    `Noom vs Control` = "",
    `MTM vs Control` = ""
  ) %>%
  width(j = "Outcome", width = 1.35) %>%
  width(j = "Model", width = 1.55) %>%
  width(j = "Sample", width = 0.55) %>%
  width(j = c("Control", "Noom", "MTM"), width = 1.03) %>%
  width(j = c("Noom vs Control", "MTM vs Control"), width = 1.72) %>%
  line_spacing(space = 0.9, part = "all") %>%
  align(j = "Model", align = "left", part = "body") %>%
  align(j = 3:8, align = "center", part = "body") %>%
  align(j = 2:8, align = "center", part = "header") %>%
  compose(
    i = 1,
    j = "Outcome",
    part = "header",
    value = as_paragraph(header_chunk("Outcome", size = 7.3, bold = TRUE))
  ) %>%
  compose(
    i = 1,
    j = "Model",
    part = "header",
    value = as_paragraph(header_chunk("Model", size = 7.3, bold = TRUE))
  ) %>%
  compose(
    i = 1,
    j = "Sample",
    part = "header",
    value = as_paragraph(
      header_chunk("Sample", size = 7.3, bold = TRUE),
      header_linebreak(),
      header_chunk("n/obs.", size = 7.3, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "Control",
    part = "header",
    value = as_paragraph(
      header_chunk("Control", size = 7.3, bold = TRUE),
      header_linebreak(),
      header_chunk("Estimate", size = 7.3, bold = TRUE),
      header_linebreak(),
      header_chunk("(95% CI)", size = 7.3, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "Noom",
    part = "header",
    value = as_paragraph(
      header_chunk(noom_display_label, size = 7.3, bold = TRUE),
      header_linebreak(),
      header_chunk("Estimate", size = 7.3, bold = TRUE),
      header_linebreak(),
      header_chunk("(95% CI)", size = 7.3, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "MTM",
    part = "header",
    value = as_paragraph(
      header_chunk("MTM", size = 7.3, bold = TRUE),
      header_linebreak(),
      header_chunk("Estimate", size = 7.3, bold = TRUE),
      header_linebreak(),
      header_chunk("(95% CI)", size = 7.3, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "Noom vs Control",
    part = "header",
    value = as_paragraph(
      header_chunk(display_noom_text("Noom vs Control"), size = 7.3, bold = TRUE),
      header_linebreak(),
      header_chunk("Difference (95% CI); P", size = 7.3, bold = FALSE)
    )
  ) %>%
  compose(
    i = 1,
    j = "MTM vs Control",
    part = "header",
    value = as_paragraph(
      header_chunk("MTM vs Control", size = 7.3, bold = TRUE),
      header_linebreak(),
      header_chunk("Difference (95% CI); P", size = 7.3, bold = FALSE)
    )
  ) %>%
  compose(
    i = weight_sensitivity_primary_rows,
    j = "Model",
    part = "body",
    value = as_paragraph(
      header_chunk("Primary", size = 7.3, bold = FALSE),
      sup_chunk("1")
    )
  ) %>%
  compose(
    i = weight_sensitivity_complete_case_rows,
    j = "Model",
    part = "body",
    value = as_paragraph(
      header_chunk("Complete-case", size = 7.3, bold = FALSE),
      sup_chunk("2")
    )
  ) %>%
  compose(
    i = weight_sensitivity_glp1_adjusted_rows,
    j = "Model",
    part = "body",
    value = as_paragraph(
      header_chunk("GLP-1 adjusted", size = 7.3, bold = FALSE),
      sup_chunk("3")
    )
  ) %>%
  compose(
    i = weight_sensitivity_glp1_baseline_rows,
    j = "Model",
    part = "body",
    value = as_paragraph(
      header_chunk("GLP-1 + baseline weight", size = 7.3, bold = FALSE),
      sup_chunk("4")
    )
  ) %>%
  add_footer_lines(
    values = c(
      paste0("\u00B9", " Primary is the main-paper longitudinal weight model among participants with baseline and at least one follow-up weight."),
      paste0("\u00B2", " Complete-case repeats the primary model among participants with non-missing pre-randomization percent weight loss while on GLP-1."),
      paste0("\u00B3", " GLP-1 adjusted adds pre-randomization percent weight loss while on GLP-1 to the complete-case model."),
      paste0("\u2074", " GLP-1 + baseline weight adds baseline weight in kg to the GLP-1 adjusted model."),
      "Weight loss while on GLP-1 was measured before randomization and is distinct from weight change during trial follow-up. Sample denotes participants/follow-up observations. Total change is the model-estimated percent weight change at Month 4; slope is percent body-weight change per month. All models include fixed effects for randomized arm, month, and arm-by-month interaction, with a participant-level random intercept."
    )
  ) %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 7.0, part = "footer") %>%
  align(align = "left", part = "footer")

write_table_docx(
  ft = weight_sensitivity_ft,
  title = "SI Table 1. Sensitivity Analysis of Longitudinal Weight Change",
  docx_file = file.path(table_dir, "SI Table 1 - Weight Sensitivity Analysis.docx"),
  section = landscape_section
)

# Write main Table 2 CSV + DOCX.
readr::write_csv(
  table_2_rows %>%
    select(-is_group) %>%
    rename_noom_display_columns(),
  file.path(table_support_dir, "Table 2 - Model-Based Outcomes.csv")
)

write_table_docx(
  ft = table_2_ft,
  title = "Table 2. Model-Based Adjusted Estimates for Exploratory Outcomes by Randomized Arm in a GLP-1 Cessation Support Pilot Trial",
  docx_file = file.path(table_dir, "Table 2 - Model-Based Outcomes.docx"),
  section = landscape_section
)

message("Step 03 complete: tables and model outputs written to ", table_dir)
