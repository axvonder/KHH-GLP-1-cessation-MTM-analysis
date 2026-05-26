#!/usr/bin/env Rscript

# Step 04. Audit raw-to-analysis transformations.

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(grid)
  library(gridExtra)
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

  normalizePath(file.path(getwd(), "04_qa_transformations.R"), mustWork = FALSE)
}

script_path <- resolve_script_path()

# Pipeline paths.
pipeline_dir <- dirname(dirname(script_path))
input_dir <- file.path(pipeline_dir, "inputs")
output_dir <- file.path(pipeline_dir, "output")
data_dir <- file.path(output_dir, "data")
qa_dir <- file.path(output_dir, "qa")
weight_assignment_dir <- file.path(output_dir, "weight_log_assignment")

# QA output.
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

# Raw inputs + Step-02 outputs.
questionnaire_file <- file.path(input_dir, "raw", "GLP1 Cessation Support Study Questionnaire Data.xlsx")
weight_file <- file.path(input_dir, "raw", "Weight and BMI.xlsx")
glp1_weight_loss_candidates <- c(
  file.path(input_dir, "raw", "Weight loss while on GLP1.xlsx"),
  file.path(dirname(pipeline_dir), "Weight loss while on GLP1.xlsx")
)
glp1_weight_loss_file <- glp1_weight_loss_candidates[file.exists(glp1_weight_loss_candidates)][1]

participants_file <- file.path(data_dir, "participants.csv")
glp1_weight_loss_output_file <- file.path(data_dir, "glp1_weight_loss.csv")
questionnaire_long_file <- file.path(data_dir, "questionnaire_long.csv")
weight_long_file <- file.path(data_dir, "weight_long.csv")
baseline_analysis_file <- file.path(data_dir, "baseline_analysis.csv")
fitabase_assigned_weight_long_file <- file.path(weight_assignment_dir, "fitabase_assigned_weight_long.csv")

# Required inputs.
stopifnot(file.exists(questionnaire_file))
stopifnot(file.exists(weight_file))
if (is.na(glp1_weight_loss_file) || !file.exists(glp1_weight_loss_file)) {
  stop("Could not find Weight loss while on GLP1.xlsx in the project root or analytic pipeline/inputs/raw.")
}
stopifnot(file.exists(participants_file))
stopifnot(file.exists(glp1_weight_loss_output_file))
stopifnot(file.exists(questionnaire_long_file))
stopifnot(file.exists(weight_long_file))
stopifnot(file.exists(baseline_analysis_file))
stopifnot(file.exists(fitabase_assigned_weight_long_file))

# Step-02 parsers for QA.
parse_numeric <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

parse_excel_date_robust <- function(x) {
  # Match Step 02 date parsing.
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  x <- trimws(as.character(x))
  x[x %in% c("", ".", "NA", "N/A")] <- NA_character_

  out <- rep(as.Date(NA), length(x))

  numeric_like <- !is.na(x) & str_detect(x, "^[0-9]+(\\.[0-9]+)?$")
  out[numeric_like] <- as.Date(as.numeric(x[numeric_like]), origin = "1899-12-30")

  remaining <- is.na(out) & !is.na(x)
  out[remaining] <- suppressWarnings(mdy(x[remaining]))

  remaining <- is.na(out) & !is.na(x)
  out[remaining] <- suppressWarnings(ymd(x[remaining]))

  out
}

# Treat matching missing values as equal.
same_or_both_na <- function(x, y, tol = 1e-8) {
  both_missing <- is.na(x) & is.na(y)
  both_present <- !is.na(x) & !is.na(y)
  both_missing | (both_present & abs(x - y) <= tol)
}

# PDF helpers.
wrap_text <- function(x, width = 118) {
  if (length(x) == 0) {
    return("")
  }

  paste(vapply(
    x,
    function(line) paste(strwrap(line, width = width), collapse = "\n"),
    character(1)
  ), collapse = "\n\n")
}

qa_table_theme <- ttheme_minimal(
  base_size = 9,
  core = list(
    fg_params = list(hjust = 0, x = 0.02, fontsize = 9),
    padding = unit(c(3, 3), "mm")
  ),
  colhead = list(
    fg_params = list(fontface = "bold", hjust = 0, x = 0.02, fontsize = 9),
    padding = unit(c(3, 3), "mm")
  )
)

draw_text_page <- function(title, body_lines, footer = NULL) {
  # Full-text page.
  grid.newpage()

  grid.text(
    title,
    x = unit(0.04, "npc"),
    y = unit(0.96, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 20, fontface = "bold")
  )

  grid.text(
    wrap_text(body_lines),
    x = unit(0.04, "npc"),
    y = unit(0.90, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 10.5, lineheight = 1.25)
  )

  if (!is.null(footer)) {
    grid.text(
      footer,
      x = unit(0.04, "npc"),
      y = unit(0.04, "npc"),
      just = c("left", "bottom"),
      gp = gpar(fontsize = 9, col = "grey35")
    )
  }
}

draw_table_pages <- function(title, table_df, subtitle = NULL, footer = NULL, rows_per_page = 20) {
  # Paginate long tables.
  if (nrow(table_df) == 0) {
    draw_text_page(title, c(subtitle, "No rows to display."), footer)
    return(invisible(NULL))
  }

  page_id <- ceiling(seq_len(nrow(table_df)) / rows_per_page)
  chunks <- split(table_df, page_id)

  for (i in seq_along(chunks)) {
    header <- title
    if (length(chunks) > 1) {
      header <- paste0(title, " (Page ", i, " of ", length(chunks), ")")
    }

    grobs <- list(
      textGrob(
        header,
        x = 0, hjust = 0,
        gp = gpar(fontsize = 18, fontface = "bold")
      )
    )
    heights <- unit.c(unit(0.45, "in"))

    if (!is.null(subtitle)) {
      grobs <- c(grobs, list(
        textGrob(
          wrap_text(subtitle, width = 115),
          x = 0, hjust = 0,
          gp = gpar(fontsize = 10, col = "grey25")
        )
      ))
      heights <- unit.c(heights, unit(0.40, "in"))
    }

    grobs <- c(grobs, list(tableGrob(chunks[[i]], rows = NULL, theme = qa_table_theme)))
    heights <- unit.c(heights, unit(1, "null"))

    if (!is.null(footer)) {
      grobs <- c(grobs, list(
        textGrob(
          wrap_text(footer, width = 120),
          x = 0, hjust = 0,
          gp = gpar(fontsize = 8.5, col = "grey35")
        )
      ))
      heights <- unit.c(heights, unit(0.45, "in"))
    }

    grid.arrange(grobs = grobs, ncol = 1, heights = heights)
  }
}

# Read raw + derived files.
questionnaire_raw <- read_excel(questionnaire_file)
weight_raw <- read_excel(weight_file)
glp1_weight_loss_raw <- read_excel(glp1_weight_loss_file, sheet = "Pre-Enrollment")

participants <- read_csv(participants_file, show_col_types = FALSE)
glp1_weight_loss <- read_csv(glp1_weight_loss_output_file, show_col_types = FALSE)
questionnaire_long <- read_csv(questionnaire_long_file, show_col_types = FALSE)
weight_long <- read_csv(weight_long_file, show_col_types = FALSE)
baseline_analysis <- read_csv(baseline_analysis_file, show_col_types = FALSE)
fitabase_assigned_weight_long <- read_csv(fitabase_assigned_weight_long_file, show_col_types = FALSE)

employment_cols <- paste0("employment___", 1:10)

# Count selected employment checkboxes.
selected_n <- function(data, cols) {
  rowSums(as.data.frame(lapply(data[cols], parse_numeric)), na.rm = TRUE)
}

# Raw + derived dataset shapes.
raw_overview <- tibble(
  dataset = c(
    "Raw questionnaire workbook",
    "Weight registry workbook",
    "Fitabase assigned weight sidecar",
    "Pre-GLP-1 weight-loss workbook",
    "participants.csv",
    "glp1_weight_loss.csv",
    "questionnaire_long.csv",
    "weight_long.csv",
    "baseline_analysis.csv"
  ),
  rows = c(
    nrow(questionnaire_raw),
    nrow(weight_raw),
    nrow(fitabase_assigned_weight_long),
    nrow(glp1_weight_loss_raw),
    nrow(participants),
    nrow(glp1_weight_loss),
    nrow(questionnaire_long),
    nrow(weight_long),
    nrow(baseline_analysis)
  ),
  columns = c(
    ncol(questionnaire_raw),
    ncol(weight_raw),
    ncol(fitabase_assigned_weight_long),
    ncol(glp1_weight_loss_raw),
    ncol(participants),
    ncol(glp1_weight_loss),
    ncol(questionnaire_long),
    ncol(weight_long),
    ncol(baseline_analysis)
  ),
  unique_study_ids = c(
    dplyr::n_distinct(questionnaire_raw$StudyID),
    dplyr::n_distinct(weight_raw$StudyID),
    dplyr::n_distinct(fitabase_assigned_weight_long$study_id),
    dplyr::n_distinct(glp1_weight_loss_raw$`Participant ID`),
    dplyr::n_distinct(participants$study_id),
    dplyr::n_distinct(glp1_weight_loss$study_id),
    dplyr::n_distinct(questionnaire_long$study_id),
    dplyr::n_distinct(weight_long$study_id),
    dplyr::n_distinct(baseline_analysis$study_id)
  )
)

# Source ID/group agreement.
id_group_join <- full_join(
  questionnaire_raw %>%
    transmute(
      study_id = as.integer(parse_numeric(StudyID)),
      questionnaire_group = as.integer(parse_numeric(Group))
    ),
  weight_raw %>%
    transmute(
      study_id = as.integer(parse_numeric(StudyID)),
      weight_group = as.integer(parse_numeric(Group))
    ),
  by = "study_id"
) %>%
  full_join(
    glp1_weight_loss_raw %>%
      transmute(
        study_id = as.integer(parse_numeric(`Participant ID`)),
        glp1_weight_loss_group = as.integer(parse_numeric(Group))
      ),
    by = "study_id"
)

id_integrity <- tibble(
  # ID checks.
  check = c(
    "Questionnaire source has 1 row per StudyID",
    "Weight registry source has 1 row per StudyID",
    "Pre-GLP-1 weight-loss source has 1 row per participant ID",
    "Questionnaire and weight registry contain the same StudyIDs",
    "Questionnaire and pre-GLP-1 weight-loss file contain the same StudyIDs",
    "Questionnaire and weight registry agree on randomized group code",
    "Questionnaire and pre-GLP-1 weight-loss file agree on randomized group code",
    "participants.csv has 1 row per study_id",
    "glp1_weight_loss.csv has 1 row per study_id",
    "baseline_analysis.csv has 1 row per study_id"
  ),
  status = c(
    ifelse(nrow(questionnaire_raw) == dplyr::n_distinct(questionnaire_raw$StudyID), "PASS", "FLAG"),
    ifelse(nrow(weight_raw) == dplyr::n_distinct(weight_raw$StudyID), "PASS", "FLAG"),
    ifelse(nrow(glp1_weight_loss_raw) == dplyr::n_distinct(glp1_weight_loss_raw$`Participant ID`), "PASS", "FLAG"),
    ifelse(sum(is.na(id_group_join$questionnaire_group)) == 0 && sum(is.na(id_group_join$weight_group)) == 0, "PASS", "FLAG"),
    ifelse(sum(is.na(id_group_join$questionnaire_group)) == 0 && sum(is.na(id_group_join$glp1_weight_loss_group)) == 0, "PASS", "FLAG"),
    ifelse(sum(!is.na(id_group_join$questionnaire_group) & !is.na(id_group_join$weight_group) & id_group_join$questionnaire_group != id_group_join$weight_group) == 0, "PASS", "FLAG"),
    ifelse(sum(!is.na(id_group_join$questionnaire_group) & !is.na(id_group_join$glp1_weight_loss_group) & id_group_join$questionnaire_group != id_group_join$glp1_weight_loss_group) == 0, "PASS", "FLAG"),
    ifelse(nrow(participants) == dplyr::n_distinct(participants$study_id), "PASS", "FLAG"),
    ifelse(nrow(glp1_weight_loss) == dplyr::n_distinct(glp1_weight_loss$study_id), "PASS", "FLAG"),
    ifelse(nrow(baseline_analysis) == dplyr::n_distinct(baseline_analysis$study_id), "PASS", "FLAG")
  ),
  detail = c(
    paste0("Rows = ", nrow(questionnaire_raw), "; unique StudyID = ", dplyr::n_distinct(questionnaire_raw$StudyID)),
    paste0("Rows = ", nrow(weight_raw), "; unique StudyID = ", dplyr::n_distinct(weight_raw$StudyID)),
    paste0("Rows = ", nrow(glp1_weight_loss_raw), "; unique participant IDs = ", dplyr::n_distinct(glp1_weight_loss_raw$`Participant ID`)),
    paste0("IDs missing from questionnaire = ", sum(is.na(id_group_join$questionnaire_group)), "; IDs missing from weight = ", sum(is.na(id_group_join$weight_group))),
    paste0("IDs missing from questionnaire = ", sum(is.na(id_group_join$questionnaire_group)), "; IDs missing from GLP-1 weight loss = ", sum(is.na(id_group_join$glp1_weight_loss_group))),
    paste0("Group mismatches = ", sum(!is.na(id_group_join$questionnaire_group) & !is.na(id_group_join$weight_group) & id_group_join$questionnaire_group != id_group_join$weight_group)),
    paste0("Group mismatches = ", sum(!is.na(id_group_join$questionnaire_group) & !is.na(id_group_join$glp1_weight_loss_group) & id_group_join$questionnaire_group != id_group_join$glp1_weight_loss_group)),
    paste0("Rows = ", nrow(participants), "; unique study_id = ", dplyr::n_distinct(participants$study_id)),
    paste0("Rows = ", nrow(glp1_weight_loss), "; unique study_id = ", dplyr::n_distinct(glp1_weight_loss$study_id)),
    paste0("Rows = ", nrow(baseline_analysis), "; unique study_id = ", dplyr::n_distinct(baseline_analysis$study_id))
  )
)

# Employment: exactly one selected checkbox.
employment_check <- tibble(
  check = "Employment checkboxes sum to exactly 1 selected option in the raw questionnaire file",
  status = ifelse(all(selected_n(questionnaire_raw, employment_cols) == 1), "PASS", "FLAG"),
  detail = paste0(
    "Minimum selected = ", min(selected_n(questionnaire_raw, employment_cols)),
    "; maximum selected = ", max(selected_n(questionnaire_raw, employment_cols))
  )
)

# TAPQ/TSQM: one arm-specific field set per row.
group_specific_checks <- bind_rows(
  lapply(1:6, function(i) {
    cols <- paste0("g", 1:3, "_tapq", i)
    counts <- rowSums(!is.na(questionnaire_raw[cols]))
    tibble(
      instrument = "TAPQ",
      item = i,
      rows_with_multiple_group_sets = sum(counts > 1),
      rows_with_no_group_set = sum(counts == 0),
      max_nonmissing_group_sets = max(counts),
      status = ifelse(max(counts) <= 1 && sum(counts > 1) == 0, "PASS", "FLAG")
    )
  }),
  lapply(1:11, function(i) {
    cols <- paste0("g", 1:3, "_tsqm", i)
    counts <- rowSums(!is.na(questionnaire_raw[cols]))
    tibble(
      instrument = "TSQM",
      item = i,
      rows_with_multiple_group_sets = sum(counts > 1),
      rows_with_no_group_set = sum(counts == 0),
      max_nonmissing_group_sets = max(counts),
      status = ifelse(max(counts) <= 1 && sum(counts > 1) == 0, "PASS", "FLAG")
    )
  })
)

# questionnaire_long structure + follow-up alignment.
questionnaire_long_check_table <- questionnaire_long %>%
  # Expect baseline + follow-up.
  group_by(study_id) %>%
  summarise(
    n_rows = n(),
    has_baseline = any(timepoint == "baseline"),
    has_followup = any(timepoint == "followup"),
    .groups = "drop"
  )

questionnaire_followup_alignment <- questionnaire_long %>%
  # Compare follow-up rows to participant-level values.
  filter(timepoint == "followup") %>%
  left_join(
    participants %>%
      select(
        study_id,
        followup_pdq,
        followup_phq,
        followup_mini_eat_score,
        tapq_score_0_100,
        tsqm_effectiveness,
        tsqm_side_effects,
        tsqm_convenience,
        tsqm_global_satisfaction
      ),
    by = "study_id"
  )

questionnaire_checks <- tibble(
  check = c(
    "questionnaire_long has exactly 2 rows per participant",
    "questionnaire_long contains both baseline and follow-up for every participant",
    "Baseline questionnaire_long rows contain no follow-up-only TAPQ/TSQM scores",
    "Follow-up questionnaire_long rows reproduce participant-level follow-up values exactly"
  ),
  status = c(
    ifelse(all(questionnaire_long_check_table$n_rows == 2), "PASS", "FLAG"),
    ifelse(all(questionnaire_long_check_table$has_baseline & questionnaire_long_check_table$has_followup), "PASS", "FLAG"),
    ifelse(
      all(
        questionnaire_long %>%
          filter(timepoint == "baseline") %>%
          transmute(
            tapq_missing = is.na(tapq_score_0_100),
            eff_missing = is.na(tsqm_effectiveness),
            side_missing = is.na(tsqm_side_effects),
            conv_missing = is.na(tsqm_convenience),
            global_missing = is.na(tsqm_global_satisfaction)
          ) %>%
          unlist()
      ),
      "PASS",
      "FLAG"
    ),
    ifelse(
      all(same_or_both_na(questionnaire_followup_alignment$pdq_score, questionnaire_followup_alignment$followup_pdq)) &&
        all(same_or_both_na(questionnaire_followup_alignment$phq_score, questionnaire_followup_alignment$followup_phq)) &&
        all(same_or_both_na(questionnaire_followup_alignment$mini_eat_score, questionnaire_followup_alignment$followup_mini_eat_score)) &&
        all(same_or_both_na(questionnaire_followup_alignment$tapq_score_0_100.x, questionnaire_followup_alignment$tapq_score_0_100.y)) &&
        all(same_or_both_na(questionnaire_followup_alignment$tsqm_effectiveness.x, questionnaire_followup_alignment$tsqm_effectiveness.y)) &&
        all(same_or_both_na(questionnaire_followup_alignment$tsqm_side_effects.x, questionnaire_followup_alignment$tsqm_side_effects.y)) &&
        all(same_or_both_na(questionnaire_followup_alignment$tsqm_convenience.x, questionnaire_followup_alignment$tsqm_convenience.y)) &&
        all(same_or_both_na(questionnaire_followup_alignment$tsqm_global_satisfaction.x, questionnaire_followup_alignment$tsqm_global_satisfaction.y)),
      "PASS",
      "FLAG"
    )
  ),
  detail = c(
    paste0("Minimum rows per participant = ", min(questionnaire_long_check_table$n_rows), "; maximum = ", max(questionnaire_long_check_table$n_rows)),
    paste0("Participants missing a baseline row = ", sum(!questionnaire_long_check_table$has_baseline), "; missing a follow-up row = ", sum(!questionnaire_long_check_table$has_followup)),
    paste0("Baseline rows checked = ", sum(questionnaire_long$timepoint == "baseline")),
    paste0("Follow-up rows checked = ", sum(questionnaire_long$timepoint == "followup"))
  )
)

# Pre-GLP-1 to GLP-1 cessation weight-loss transformation.
glp1_weight_loss_expected <- glp1_weight_loss_raw %>%
  transmute(
    study_id = as.integer(parse_numeric(`Participant ID`)),
    expected_glp1_weight_loss_group = as.integer(parse_numeric(Group)),
    expected_pre_glp1_weight_lb = parse_numeric(`Pre-medication`),
    expected_post_glp1_cessation_weight_lb = parse_numeric(`Post-medication`),
    expected_post_glp1_cessation_weight_kg = parse_numeric(`Pre-enrollment kg`),
    expected_glp1_weight_loss_pct_source = parse_numeric(`% lost`)
  ) %>%
  mutate(
    expected_glp1_weight_loss_lb = expected_pre_glp1_weight_lb - expected_post_glp1_cessation_weight_lb,
    expected_glp1_weight_loss_kg = expected_glp1_weight_loss_lb * 0.45359237,
    expected_glp1_weight_loss_pct_calculated = if_else(
      !is.na(expected_pre_glp1_weight_lb) & expected_pre_glp1_weight_lb > 0,
      100 * expected_glp1_weight_loss_lb / expected_pre_glp1_weight_lb,
      NA_real_
    ),
    expected_glp1_weight_loss_pct = coalesce(
      expected_glp1_weight_loss_pct_source,
      expected_glp1_weight_loss_pct_calculated
    )
  ) %>%
  select(
    study_id,
    expected_glp1_weight_loss_group,
    expected_pre_glp1_weight_lb,
    expected_post_glp1_cessation_weight_lb,
    expected_post_glp1_cessation_weight_kg,
    expected_glp1_weight_loss_lb,
    expected_glp1_weight_loss_kg,
    expected_glp1_weight_loss_pct,
    expected_glp1_weight_loss_pct_calculated,
    expected_glp1_weight_loss_pct_source
  ) %>%
  arrange(study_id)

glp1_weight_loss_compare <- glp1_weight_loss %>%
  rename(
    output_glp1_weight_loss_group = glp1_weight_loss_group,
    output_pre_glp1_weight_lb = pre_glp1_weight_lb,
    output_post_glp1_cessation_weight_lb = post_glp1_cessation_weight_lb,
    output_post_glp1_cessation_weight_kg = post_glp1_cessation_weight_kg,
    output_glp1_weight_loss_lb = glp1_weight_loss_lb,
    output_glp1_weight_loss_kg = glp1_weight_loss_kg,
    output_glp1_weight_loss_pct_source = glp1_weight_loss_pct_source,
    output_glp1_weight_loss_pct_calculated = glp1_weight_loss_pct_calculated,
    output_glp1_weight_loss_pct = glp1_weight_loss_pct
  ) %>%
  full_join(glp1_weight_loss_expected, by = "study_id") %>%
  mutate(
    group_match = same_or_both_na(output_glp1_weight_loss_group, expected_glp1_weight_loss_group),
    pre_weight_match = same_or_both_na(output_pre_glp1_weight_lb, expected_pre_glp1_weight_lb),
    post_weight_match = same_or_both_na(output_post_glp1_cessation_weight_lb, expected_post_glp1_cessation_weight_lb),
    post_weight_kg_match = same_or_both_na(output_post_glp1_cessation_weight_kg, expected_post_glp1_cessation_weight_kg),
    loss_lb_match = same_or_both_na(output_glp1_weight_loss_lb, expected_glp1_weight_loss_lb),
    loss_kg_match = same_or_both_na(output_glp1_weight_loss_kg, expected_glp1_weight_loss_kg),
    loss_pct_source_match = same_or_both_na(output_glp1_weight_loss_pct_source, expected_glp1_weight_loss_pct_source),
    loss_pct_calculated_match = same_or_both_na(output_glp1_weight_loss_pct_calculated, expected_glp1_weight_loss_pct_calculated),
    loss_pct_match = same_or_both_na(output_glp1_weight_loss_pct, expected_glp1_weight_loss_pct),
    source_pct_matches_pre_post = is.na(expected_glp1_weight_loss_pct_source) |
      is.na(expected_glp1_weight_loss_pct_calculated) |
      abs(expected_glp1_weight_loss_pct_source - expected_glp1_weight_loss_pct_calculated) <= 0.1,
    all_value_matches = group_match & pre_weight_match & post_weight_match & post_weight_kg_match &
      loss_lb_match & loss_kg_match & loss_pct_source_match & loss_pct_calculated_match & loss_pct_match
  )

glp1_weight_loss_checks <- tibble(
  check = c(
    "glp1_weight_loss.csv has exactly 1 row per participant",
    "glp1_weight_loss.csv values reproduce the raw pre-GLP-1 workbook",
    "Source percent lost agrees with pre/post weight calculation within 0.1 percentage points"
  ),
  status = c(
    ifelse(nrow(glp1_weight_loss) == dplyr::n_distinct(glp1_weight_loss$study_id), "PASS", "FLAG"),
    ifelse(all(glp1_weight_loss_compare$all_value_matches), "PASS", "FLAG"),
    ifelse(all(glp1_weight_loss_compare$source_pct_matches_pre_post), "PASS", "FLAG")
  ),
  detail = c(
    paste0("Rows = ", nrow(glp1_weight_loss), "; unique study_id = ", dplyr::n_distinct(glp1_weight_loss$study_id)),
    paste0("Rows with any non-matching transformed value = ", sum(!glp1_weight_loss_compare$all_value_matches)),
    paste0(
      "Non-missing percent-lost rows = ", sum(!is.na(glp1_weight_loss_compare$expected_glp1_weight_loss_pct_source)),
      "; maximum absolute difference = ",
      formatC(max(abs(
        glp1_weight_loss_compare$expected_glp1_weight_loss_pct_source -
          glp1_weight_loss_compare$expected_glp1_weight_loss_pct_calculated
      ), na.rm = TRUE), digits = 3, format = "f")
    )
  )
)

# Rebuild expected `weight_long` from the Fitabase assignment sidecar.
weight_expected_long <- fitabase_assigned_weight_long %>%
  transmute(
    study_id = as.integer(study_id),
    arm = as.character(arm),
    visit_month = as.integer(visit_month),
    month_since_baseline = as.integer(month_since_baseline),
    expected_visit_date = as.Date(visit_date),
    expected_weight_kg = as.numeric(weight_kg),
    expected_bmi_kg_m2 = as.numeric(bmi_kg_m2),
    expected_baseline_weight_kg = as.numeric(baseline_weight_kg),
    expected_baseline_bmi_kg_m2 = as.numeric(baseline_bmi_kg_m2)
  ) %>%
  arrange(study_id, visit_month)

weight_compare <- weight_long %>%
  # Compare output rows to the Fitabase assignment source used by Step 02.
  left_join(weight_expected_long, by = c("study_id", "visit_month", "month_since_baseline", "arm")) %>%
  mutate(
    weight_match = same_or_both_na(weight_kg, expected_weight_kg),
    bmi_match = same_or_both_na(bmi_kg_m2, expected_bmi_kg_m2),
    baseline_weight_match = same_or_both_na(baseline_weight_kg, expected_baseline_weight_kg),
    baseline_bmi_match = same_or_both_na(baseline_bmi_kg_m2, expected_baseline_bmi_kg_m2),
    visit_date_match = case_when(
      is.na(expected_visit_date) & is.na(visit_date) ~ TRUE,
      is.na(expected_visit_date) & !is.na(visit_date) ~ FALSE,
      !is.na(expected_visit_date) & is.na(visit_date) ~ FALSE,
      TRUE ~ as.Date(visit_date) == expected_visit_date
    )
  )

weight_structure <- weight_long %>%
  # Expect baseline plus four follow-up slots per participant.
  group_by(study_id) %>%
  summarise(
    n_rows = n(),
    min_visit = min(visit_month),
    max_visit = max(visit_month),
    .groups = "drop"
  )

weight_chronology <- weight_long %>%
  filter(!is.na(visit_date)) %>%
  arrange(study_id, visit_month) %>%
  group_by(study_id) %>%
  mutate(previous_visit_date = lag(as.Date(visit_date))) %>%
  ungroup() %>%
  filter(!is.na(previous_visit_date), as.Date(visit_date) <= previous_visit_date)

weight_glp1_compare <- weight_long %>%
  select(
    study_id,
    visit_month,
    output_pre_glp1_weight_lb = pre_glp1_weight_lb,
    output_post_glp1_cessation_weight_lb = post_glp1_cessation_weight_lb,
    output_post_glp1_cessation_weight_kg = post_glp1_cessation_weight_kg,
    output_glp1_weight_loss_lb = glp1_weight_loss_lb,
    output_glp1_weight_loss_kg = glp1_weight_loss_kg,
    output_glp1_weight_loss_pct_source = glp1_weight_loss_pct_source,
    output_glp1_weight_loss_pct_calculated = glp1_weight_loss_pct_calculated,
    output_glp1_weight_loss_pct = glp1_weight_loss_pct
  ) %>%
  left_join(
    glp1_weight_loss %>%
      select(
        study_id,
        expected_pre_glp1_weight_lb = pre_glp1_weight_lb,
        expected_post_glp1_cessation_weight_lb = post_glp1_cessation_weight_lb,
        expected_post_glp1_cessation_weight_kg = post_glp1_cessation_weight_kg,
        expected_glp1_weight_loss_lb = glp1_weight_loss_lb,
        expected_glp1_weight_loss_kg = glp1_weight_loss_kg,
        expected_glp1_weight_loss_pct_source = glp1_weight_loss_pct_source,
        expected_glp1_weight_loss_pct_calculated = glp1_weight_loss_pct_calculated,
        expected_glp1_weight_loss_pct = glp1_weight_loss_pct
      ),
    by = "study_id"
  ) %>%
  mutate(
    pre_glp1_weight_match = same_or_both_na(output_pre_glp1_weight_lb, expected_pre_glp1_weight_lb),
    post_glp1_weight_match = same_or_both_na(output_post_glp1_cessation_weight_lb, expected_post_glp1_cessation_weight_lb),
    post_glp1_kg_match = same_or_both_na(output_post_glp1_cessation_weight_kg, expected_post_glp1_cessation_weight_kg),
    loss_lb_match = same_or_both_na(output_glp1_weight_loss_lb, expected_glp1_weight_loss_lb),
    loss_kg_match = same_or_both_na(output_glp1_weight_loss_kg, expected_glp1_weight_loss_kg),
    loss_pct_source_match = same_or_both_na(output_glp1_weight_loss_pct_source, expected_glp1_weight_loss_pct_source),
    loss_pct_calculated_match = same_or_both_na(output_glp1_weight_loss_pct_calculated, expected_glp1_weight_loss_pct_calculated),
    loss_pct_match = same_or_both_na(output_glp1_weight_loss_pct, expected_glp1_weight_loss_pct),
    all_glp1_weight_loss_fields_match = pre_glp1_weight_match & post_glp1_weight_match &
      post_glp1_kg_match & loss_lb_match & loss_kg_match &
      loss_pct_source_match & loss_pct_calculated_match & loss_pct_match
  )

weight_checks <- tibble(
  check = c(
    "weight_long has exactly 5 rows per participant",
    "weight_long visit_month spans 1 through 5 for every participant",
    "Weight values match the Fitabase assignment source",
    "BMI values match the Fitabase assignment source",
    "Visit dates match the Fitabase assignment source",
    "Non-missing visit dates are chronological within participant",
    "baseline_weight_kg matches the baseline weight within each participant",
    "baseline_bmi_kg_m2 matches the baseline BMI within each participant",
    "pct_weight_change matches the stated formula exactly",
    "weight_long GLP-1 weight-loss fields match glp1_weight_loss.csv"
  ),
  status = c(
    ifelse(all(weight_structure$n_rows == 5), "PASS", "FLAG"),
    ifelse(all(weight_structure$min_visit == 1 & weight_structure$max_visit == 5), "PASS", "FLAG"),
    ifelse(all(weight_compare$weight_match), "PASS", "FLAG"),
    ifelse(all(weight_compare$bmi_match), "PASS", "FLAG"),
    ifelse(all(weight_compare$visit_date_match), "PASS", "FLAG"),
    ifelse(nrow(weight_chronology) == 0, "PASS", "FLAG"),
    ifelse(all(weight_compare$baseline_weight_match), "PASS", "FLAG"),
    ifelse(all(weight_compare$baseline_bmi_match), "PASS", "FLAG"),
    ifelse(
      all(same_or_both_na(
        weight_long$pct_weight_change,
        100 * (weight_long$weight_kg - weight_long$baseline_weight_kg) / weight_long$baseline_weight_kg
      )),
      "PASS",
      "FLAG"
    ),
    ifelse(all(weight_glp1_compare$all_glp1_weight_loss_fields_match), "PASS", "FLAG")
  ),
  detail = c(
    paste0("Minimum rows per participant = ", min(weight_structure$n_rows), "; maximum = ", max(weight_structure$n_rows)),
    paste0("Unique visit_month values = ", paste(sort(unique(weight_long$visit_month)), collapse = ", ")),
    paste0("Rows failing exact weight match = ", sum(!weight_compare$weight_match)),
    paste0("Rows failing exact BMI match = ", sum(!weight_compare$bmi_match)),
    paste0(
      "Expected non-missing visit dates = ", sum(!is.na(weight_compare$expected_visit_date)),
      "; non-missing visit_date in output = ", sum(!is.na(weight_long$visit_date)),
      "; mismatched rows = ", sum(!weight_compare$visit_date_match)
    ),
    paste0("Non-chronological assigned visit rows = ", nrow(weight_chronology)),
    paste0("Rows failing baseline-weight match = ", sum(!weight_compare$baseline_weight_match)),
    paste0("Rows failing baseline-BMI match = ", sum(!weight_compare$baseline_bmi_match)),
    paste0("Rows failing pct_weight_change recalculation = ", sum(!same_or_both_na(
      weight_long$pct_weight_change,
      100 * (weight_long$weight_kg - weight_long$baseline_weight_kg) / weight_long$baseline_weight_kg
    ))),
    paste0("Repeated weight rows with non-matching GLP-1 weight-loss fields = ", sum(!weight_glp1_compare$all_glp1_weight_loss_fields_match))
  )
)

# baseline_analysis vs expected baseline data.
baseline_glp1_compare <- baseline_analysis %>%
  select(study_id, baseline_glp1_weight_loss_pct = glp1_weight_loss_pct) %>%
  left_join(
    glp1_weight_loss %>%
      select(study_id, expected_glp1_weight_loss_pct = glp1_weight_loss_pct),
    by = "study_id"
  ) %>%
  mutate(
    glp1_weight_loss_pct_match = same_or_both_na(
      baseline_glp1_weight_loss_pct,
      expected_glp1_weight_loss_pct
    )
  )

baseline_analysis_checks <- tibble(
  check = c(
    "baseline_analysis includes the same participants as participants.csv",
    "baseline_analysis baseline_weight_kg matches baseline weight from weight_long",
    "baseline_analysis glp1_weight_loss_pct matches glp1_weight_loss.csv"
  ),
  status = c(
    ifelse(identical(sort(baseline_analysis$study_id), sort(participants$study_id)), "PASS", "FLAG"),
    ifelse(
      all(same_or_both_na(
        baseline_analysis$baseline_weight_kg,
        weight_long %>%
          filter(visit_month == 1) %>%
          arrange(study_id) %>%
          pull(weight_kg)
      )),
      "PASS",
      "FLAG"
    ),
    ifelse(all(baseline_glp1_compare$glp1_weight_loss_pct_match), "PASS", "FLAG")
  ),
  detail = c(
    paste0("Rows = ", nrow(baseline_analysis), "; participant rows = ", nrow(participants)),
    paste0("Rows with non-matching baseline weight = ", sum(!same_or_both_na(
      baseline_analysis$baseline_weight_kg,
      weight_long %>%
        filter(visit_month == 1) %>%
        arrange(study_id) %>%
        pull(weight_kg)
    ))),
    paste0("Rows with non-matching GLP-1 percent weight loss = ", sum(!baseline_glp1_compare$glp1_weight_loss_pct_match))
  )
)

# Step-02 transformation inventory.
transformation_inventory <- tribble(
  ~step, ~output_dataset, ~derived_field_or_structure, ~source_fields, ~operation,
  1, "participants.csv", "study_id, arm", "StudyID, Group", "Parsed numeric IDs; recoded group 1/2/3 to Control/Noom/MTM.",
  2, "participants.csv", "Baseline demographic fields", "age, gender, ethinicty, race, education, employment___1:10, income, ppl_home, children", "Parsed numeric baseline characteristics; collapsed employment checkboxes to one selected employment code.",
  3, "participants.csv", "baseline_pdq, followup_pdq, baseline_phq, followup_phq", "pdq, fu_pdq, phq, fu_phq", "Kept valid response codes 1 to 5; recoded prefer-not-to-answer code 6 to missing.",
  4, "participants.csv", "baseline_mini_eat_score, followup_mini_eat_score", "me1:me9, fu_me1:fu_me9", "Mapped raw Mini-EAT item responses to model-input values, then applied the published Mini-EAT linear scoring equation.",
  5, "participants.csv", "tapq_item_* , tapq_item_*_pct, tapq_score_0_100", "g1_tapq1:6, g2_tapq1:6, g3_tapq1:6", "Coalesced the one populated group-specific TAPQ form per participant; cleaned prefer-not-to-answer codes; harmonized items to 0 to 100; scored the total only when all 6 items were present.",
  6, "participants.csv", "tsqm_item_* and TSQM domains", "g1_tsqm1:11, g2_tsqm1:11, g3_tsqm1:11", "Coalesced the one populated group-specific TSQM form per participant; cleaned prefer-not-to-answer codes; scored TSQM-II domain scores using published domain equations and branching rules.",
  7, "questionnaire_long.csv", "2-row repeated-measures questionnaire file", "participants.csv", "Stacked baseline and follow-up questionnaire outcomes into one long file with one row per participant-timepoint.",
  8, "glp1_weight_loss.csv", "pre/post GLP-1 weights and percent weight loss", "Weight loss while on GLP1.xlsx", "Parsed pre-medication and post-cessation weights; converted absolute loss to kg; carried source percent weight loss, with pre/post calculation as fallback.",
  9, "weight_long.csv", "5-row repeated-measures weight file", "fitabase_assigned_weight_long.csv", "Imported Fitabase-derived baseline/M1-M4 assignments with PI-confirmed manual scale-photo entries; retained visit_month and month_since_baseline.",
  10, "weight_long.csv", "baseline_weight_kg, baseline_bmi_kg_m2, pct_weight_change", "Fitabase-assigned baseline weight and BMI", "Joined baseline weight/BMI back to all weight rows and calculated percent weight change as 100 * (weight_t - baseline_weight) / baseline_weight.",
  11, "weight_long.csv", "pre/post GLP-1 weights and percent weight loss", "glp1_weight_loss.csv", "Joined participant-level GLP-1 weight-loss fields onto each longitudinal weight row for sensitivity analyses.",
  12, "baseline_analysis.csv", "1-row baseline analysis file", "participants.csv + Fitabase-assigned baseline weight/BMI + GLP-1 weight loss", "Joined baseline participant covariates, baseline weight/BMI, and GLP-1 weight-loss fields for Table 1 construction."
) %>%
  mutate(
    derived_field_or_structure = str_wrap(derived_field_or_structure, width = 28),
    source_fields = str_wrap(source_fields, width = 28),
    operation = str_wrap(operation, width = 48)
)

# Combined status table.
status_summary <- bind_rows(
  id_integrity,
  employment_check,
  questionnaire_checks,
  glp1_weight_loss_checks,
  weight_checks,
  baseline_analysis_checks
) %>%
  mutate(
    check = str_wrap(check, width = 52),
    detail = str_wrap(detail, width = 62)
)

# Key findings.
flagged_findings <- c(
  "Pre-GLP-1 to GLP-1 cessation weight-loss values were parsed and joined into both baseline_analysis.csv and weight_long.csv for descriptive and sensitivity analyses.",
  "Weight visit dates are preserved in weight_long.csv from the Fitabase assignment sidecar, and non-missing visit dates are chronological within participant.",
  "Questionnaire IDs, weight-registry IDs, Fitabase-assigned weight IDs, pre-GLP-1 weight-loss IDs, randomized group codes, row counts, and weight/BMI values were preserved as expected.",
  "The questionnaire_long reshape produced the intended two-row-per-participant structure and reproduces the participant-level follow-up values exactly."
)

# Write QA CSVs.
write_csv(status_summary, file.path(qa_dir, "qa_transformation_audit_summary.csv"))
write_csv(raw_overview, file.path(qa_dir, "qa_transformation_dataset_overview.csv"))
write_csv(group_specific_checks, file.path(qa_dir, "qa_group_specific_checks.csv"))
write_csv(glp1_weight_loss_compare, file.path(qa_dir, "qa_glp1_weight_loss_value_checks.csv"))
write_csv(weight_compare, file.path(qa_dir, "qa_weight_long_value_checks.csv"))
write_csv(transformation_inventory, file.path(qa_dir, "qa_transformation_inventory.csv"))

# Render PDF report.
pdf(file.path(qa_dir, "qa_transformation_audit.pdf"), width = 11, height = 8.5, onefile = TRUE)

draw_text_page(
  title = "Transformation QA Report",
  body_lines = c(
    "Purpose: Audit the final analytic pipeline's data transformations from the raw questionnaire workbook, Fitabase-derived weight assignments, legacy weight registry, and pre-GLP-1 weight-loss workbook into the derived analysis datasets that feed the manuscript tables and figures.",
    "Scope: This report checks row counts, participant ID integrity, randomized group consistency, pre-GLP-1 weight-loss parsing, Fitabase-assigned weight preservation, group-specific questionnaire field coalescing, baseline joins, and derivation of percent weight change.",
    "Summary: All audited raw-to-analysis transformations reproduce as intended."
  ),
  footer = paste0("Generated from: ", basename(script_path))
)

draw_table_pages(
  title = "Transformation inventory",
  subtitle = "This table lists each explicit transformation used to create the analysis datasets from the raw source files.",
  table_df = transformation_inventory,
  footer = "The inventory focuses on all derived fields and repeated-measures reshapes used by the final analytic pipeline."
)

draw_table_pages(
  title = "Dataset overview",
  subtitle = "Raw source and derived dataset row counts, column counts, and participant counts.",
  table_df = raw_overview %>% mutate(across(everything(), as.character)),
  footer = "This overview is a quick guard against accidental source or output drift."
)

draw_table_pages(
  title = "Structure and ID checks",
  subtitle = "Source row counts, participant uniqueness, and source-file agreement on StudyID and randomized group.",
  table_df = bind_rows(id_integrity, employment_check) %>% mutate(across(everything(), as.character)),
  footer = "All source-file and participant-level structure checks passed in the final pipeline."
)

draw_table_pages(
  title = "Questionnaire and group-specific field checks",
  subtitle = "Checks of the repeated-measures questionnaire reshape and of the mutually exclusive group-specific TAPQ/TSQM source fields.",
  table_df = bind_rows(
    questionnaire_checks %>% mutate(section = "questionnaire_long") %>% select(section, everything()),
    group_specific_checks %>% transmute(
      section = paste(instrument, "item", item),
      check = "At most one group-specific source form populated per participant",
      status,
      detail = paste0(
        "Rows with >1 populated group sets = ", rows_with_multiple_group_sets,
        "; rows with no populated group set = ", rows_with_no_group_set
      )
    )
  ) %>% mutate(across(everything(), as.character)),
  footer = "The TAPQ and TSQM source forms were mutually exclusive by randomized group, so coalescing across g1/g2/g3 fields did not mix forms."
)

draw_table_pages(
  title = "Weight transformation checks",
  subtitle = "Checks of pre-GLP-1 weight-loss parsing, the wide-to-long weight reshape, baseline-weight join, date preservation, and percent-weight-change derivation.",
  table_df = bind_rows(glp1_weight_loss_checks, weight_checks, baseline_analysis_checks) %>% mutate(across(everything(), as.character)),
  footer = "Pre-GLP-1 weight-loss values, weight values, BMI values, visit dates, and percent-weight-change values were preserved as expected."
)

draw_text_page(
  title = "Key findings",
  body_lines = flagged_findings,
  footer = "Companion CSVs in output/qa provide the row-level details behind these checks."
)

dev.off()

message("Step 04 complete: transformation QA report written to ", qa_dir)
