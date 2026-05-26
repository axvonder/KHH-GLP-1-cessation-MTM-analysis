#!/usr/bin/env Rscript

# Step 02. Build analysis datasets from the raw workbooks.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
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

  normalizePath(file.path(getwd(), "02_build_analysis_data.R"), mustWork = FALSE)
}

script_path <- resolve_script_path()

# Pipeline paths.
pipeline_dir <- dirname(dirname(script_path))
input_dir <- file.path(pipeline_dir, "inputs")
output_dir <- file.path(pipeline_dir, "output")
data_dir <- file.path(output_dir, "data")
qa_dir <- file.path(output_dir, "qa")
weight_assignment_dir <- file.path(output_dir, "weight_log_assignment")

# Step outputs.
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

# Raw inputs.
questionnaire_file <- file.path(input_dir, "raw", "GLP1 Cessation Support Study Questionnaire Data.xlsx")
weight_file <- file.path(input_dir, "raw", "Weight and BMI.xlsx")
glp1_weight_loss_candidates <- c(
  file.path(input_dir, "raw", "Weight loss while on GLP1.xlsx"),
  file.path(pipeline_dir, "Weight loss while on GLP1.xlsx")
)
glp1_weight_loss_file <- glp1_weight_loss_candidates[file.exists(glp1_weight_loss_candidates)][1]
mini_eat_map_file <- file.path(input_dir, "reference", "MiniEAT user data to model mapping 11-2023.csv")
fitabase_weight_long_file <- file.path(weight_assignment_dir, "fitabase_assigned_weight_long.csv")

# Required inputs.
stopifnot(file.exists(questionnaire_file))
stopifnot(file.exists(weight_file))
if (!file.exists(fitabase_weight_long_file)) {
  stop("Could not find fitabase_assigned_weight_long.csv. Run scripts/00_make_fitabase_with_manual_imputations.R and scripts/01_make_fitabase_assigned_weight_dataset.R first.")
}
if (is.na(glp1_weight_loss_file) || !file.exists(glp1_weight_loss_file)) {
  stop("Could not find Weight loss while on GLP1.xlsx in the project root or inputs/raw.")
}
stopifnot(file.exists(mini_eat_map_file))

# Parse mixed Excel exports to numeric.
parse_numeric <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

# Parse Excel dates, serials, and placeholders.
parse_excel_date <- function(x) {
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

  needs_mdy <- is.na(out) & !is.na(x)
  out[needs_mdy] <- suppressWarnings(mdy(x[needs_mdy]))

  needs_ymd <- is.na(out) & !is.na(x)
  out[needs_ymd] <- suppressWarnings(ymd(x[needs_ymd]))
  out
}

# PDQ/PHQ: keep 1-5, set code 6 to missing.
clean_1_to_5_with_pna_6 <- function(x) {
  x <- parse_numeric(x)
  ifelse(x %in% 1:5, x, NA_real_)
}

# Count selected employment checkboxes.
selected_n <- function(data, cols) {
  rowSums(as.data.frame(lapply(data[cols], parse_numeric)), na.rm = TRUE)
}

# Collapse multiple binary employment fields to one multi-level variable
single_selected_code <- function(data, cols) {
  counts <- selected_n(data, cols)
  stopifnot(all(counts == 1))
  codes <- apply(as.data.frame(lapply(data[cols], parse_numeric)), 1, function(row) which(row == 1)[1])
  as.integer(codes)
}

# Mini-EAT item recode.
# me4/me5/me6 map to fish/whole grains/refined grains, not Question 4/5/6.
# `item_number` follows the Mayo scoring item number.
mini_eat_model_value <- function(item_number, x) {
  x <- parse_numeric(x)
  stopifnot(item_number %in% 1:9)

  dplyr::case_when(
    # Item 1: fruits.
    item_number == 1 & x %in% c(1, 2) ~ 1,
    item_number == 1 & x == 3 ~ 2,
    item_number == 1 & x == 4 ~ 3,
    item_number == 1 & x == 5 ~ 4,
    item_number == 1 & x == 6 ~ 5,
    item_number == 1 & x == 7 ~ 6,
    item_number == 1 & x %in% c(8, 9) ~ 7,

    # Item 2: vegetables.
    item_number == 2 & x %in% c(1, 2, 3) ~ 1,
    item_number == 2 & x == 4 ~ 2,
    item_number == 2 & x == 5 ~ 3,
    item_number == 2 & x == 6 ~ 4,
    item_number == 2 & x == 7 ~ 5,
    item_number == 2 & x %in% c(8, 9) ~ 6,

    # Item 3: legumes / nuts / seeds.
    item_number == 3 & x %in% c(1, 2) ~ 1,
    item_number == 3 & x == 3 ~ 2,
    item_number == 3 & x == 4 ~ 3,
    item_number == 3 & x == 5 ~ 4,
    item_number == 3 & x == 6 ~ 5,
    item_number == 3 & x %in% c(7, 8, 9) ~ 6,

    # Item 4: whole grains.
    item_number == 4 & x == 1 ~ 1,
    item_number == 4 & x == 2 ~ 2,
    item_number == 4 & x == 3 ~ 3,
    item_number == 4 & x == 4 ~ 4,
    item_number == 4 & x == 5 ~ 5,
    item_number == 4 & x == 6 ~ 6,
    item_number == 4 & x %in% c(7, 8, 9) ~ 7,

    # Item 5: refined grains.
    item_number == 5 & x == 1 ~ 1,
    item_number == 5 & x == 2 ~ 2,
    item_number == 5 & x == 3 ~ 3,
    item_number == 5 & x == 4 ~ 4,
    item_number == 5 & x == 5 ~ 5,
    item_number == 5 & x == 6 ~ 6,
    item_number == 5 & x %in% c(7, 8, 9) ~ 7,

    # Item 6: fish / seafood. Codes 4-9 collapse to 4.
    item_number == 6 & x == 1 ~ 1,
    item_number == 6 & x == 2 ~ 2,
    item_number == 6 & x == 3 ~ 3,
    item_number == 6 & x %in% c(4, 5, 6, 7, 8, 9) ~ 4,

    # Item 7: low-fat dairy.
    item_number == 7 & x == 1 ~ 1,
    item_number == 7 & x == 2 ~ 2,
    item_number == 7 & x == 3 ~ 3,
    item_number == 7 & x == 4 ~ 4,
    item_number == 7 & x == 5 ~ 5,
    item_number == 7 & x == 6 ~ 6,
    item_number == 7 & x %in% c(7, 8, 9) ~ 7,

    # Item 8: higher-fat dairy / saturated fat.
    item_number == 8 & x == 1 ~ 1,
    item_number == 8 & x == 2 ~ 2,
    item_number == 8 & x == 3 ~ 3,
    item_number == 8 & x == 4 ~ 4,
    item_number == 8 & x == 5 ~ 5,
    item_number == 8 & x == 6 ~ 6,
    item_number == 8 & x %in% c(7, 8, 9) ~ 7,

    # Item 9: sweets.
    item_number == 9 & x %in% c(1, 2) ~ 1,
    item_number == 9 & x == 3 ~ 2,
    item_number == 9 & x == 4 ~ 3,
    item_number == 9 & x == 5 ~ 4,
    item_number == 9 & x == 6 ~ 5,
    item_number == 9 & x %in% c(7, 8, 9) ~ 6,

    # Code 10 = prefer not to answer.
    x == 10 ~ NA_real_,

    # Keep missing as missing.
    is.na(x) ~ NA_real_,

    # Unexpected codes -> missing.
    TRUE ~ NA_real_
  )
}

# Published Mini-EAT linear scoring equation.
mini_eat_score <- function(fruits, vegetable, legume_nut_seed, grains_whole, grains_refined, fish, dairy_low_fat, dairy_high_fat_satur, sweets) {
  inputs <- data.frame(
    fruits,
    vegetable,
    legume_nut_seed,
    grains_whole,
    grains_refined,
    fish,
    dairy_low_fat,
    dairy_high_fat_satur,
    sweets
  )

  # Score only complete 9-item sets.
  complete <- complete.cases(inputs)
  score <- rep(NA_real_, nrow(inputs))
  score[complete] <-
    49.3695 +
    fruits[complete] * 2.1166 +
    vegetable[complete] * 0.8501 +
    legume_nut_seed[complete] * 0.7158 +
    grains_whole[complete] * 1.4428 -
    grains_refined[complete] * 1.1402 +
    fish[complete] * 0.8952 +
    dairy_low_fat[complete] * 0.8146 -
    dairy_high_fat_satur[complete] * 1.0268 -
    sweets[complete] * 0.8923

  score
}

# TAPQ item cleaning; raw scales differ by item.
tapq_clean_item <- function(item_number, x) {
  x <- parse_numeric(x)
  stopifnot(item_number %in% 1:6)

  if (item_number %in% c(1, 3, 4, 5)) {
    return(ifelse(x %in% 1:6, x, NA_real_))
  }

  if (item_number == 2) {
    return(ifelse(x %in% 1:8, x, NA_real_))
  }

  if (item_number == 6) {
    return(ifelse(x %in% 1:7, x, NA_real_))
  }

  NA_real_
}

# TAPQ item rescaling to 0-100.
tapq_percent_item <- function(item_number, x) {
  x <- tapq_clean_item(item_number, x)

  dplyr::case_when(
    # Published anchors for items 1, 3, and 4.
    item_number %in% c(1, 3, 4) & x == 1 ~ 0,
    item_number %in% c(1, 3, 4) & x == 2 ~ 25,
    item_number %in% c(1, 3, 4) & x == 3 ~ 50,
    item_number %in% c(1, 3, 4) & x == 4 ~ 75,
    item_number %in% c(1, 3, 4) & x == 5 ~ 90,
    item_number %in% c(1, 3, 4) & x == 6 ~ 100,

    # Item 5: project harmonization to equal 0-100 spacing.
    item_number == 5 & x == 1 ~ 0,
    item_number == 5 & x == 2 ~ 20,
    item_number == 5 & x == 3 ~ 40,
    item_number == 5 & x == 4 ~ 60,
    item_number == 5 & x == 5 ~ 80,
    item_number == 5 & x == 6 ~ 100,

    # Items 2 and 6: linear rescaling.
    item_number == 2 & x >= 1 & x <= 8 ~ (x - 1) / 7 * 100,
    item_number == 6 & x >= 1 & x <= 7 ~ (x - 1) / 6 * 100,
    is.na(x) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# TAPQ total: mean of all six harmonized items.
tapq_score <- function(item_percent_df) {
  score <- rowMeans(item_percent_df, na.rm = FALSE)
  score
}

# Validate TSQM raw codes before domain scoring.
tsqm_clean_item <- function(item_number, x) {
  x <- parse_numeric(x)
  stopifnot(item_number %in% 1:11)

  if (item_number == 3) {
    return(ifelse(x %in% 1:2, x, NA_real_))
  }

  if (item_number %in% 4:6) {
    return(ifelse(x %in% 1:5, x, NA_real_))
  }

  if (item_number %in% c(1, 2, 7, 8, 9, 10, 11)) {
    return(ifelse(x %in% 1:7, x, NA_real_))
  }

  NA_real_
}

# TSQM-II effectiveness.
tsqm_effectiveness_score <- function(item_1, item_2) {
  items <- data.frame(item_1, item_2)
  n_answered <- rowSums(!is.na(items))
  score <- rep(NA_real_, nrow(items))
  score[n_answered == 2] <- ((rowSums(items[n_answered == 2, , drop = FALSE]) - 2) / 12) * 100
  score[n_answered == 1] <- ((rowSums(items[n_answered == 1, , drop = FALSE], na.rm = TRUE) - 1) / 6) * 100
  score
}

# TSQM-II side effects.
tsqm_side_effects_score <- function(item_3, item_4, item_5, item_6) {
  side_effect_items <- data.frame(item_4, item_5, item_6)
  n_answered <- rowSums(!is.na(side_effect_items))
  score <- rep(NA_real_, length(item_3))
  score[item_3 == 2] <- 100
  score[item_3 == 1 & n_answered == 3] <- ((rowSums(side_effect_items[item_3 == 1 & n_answered == 3, , drop = FALSE]) - 3) / 12) * 100
  score[item_3 == 1 & n_answered == 2] <- ((rowSums(side_effect_items[item_3 == 1 & n_answered == 2, , drop = FALSE], na.rm = TRUE) - 2) / 8) * 100
  score
}

# TSQM-II convenience.
tsqm_convenience_score <- function(item_7, item_8, item_9) {
  items <- data.frame(item_7, item_8, item_9)
  n_answered <- rowSums(!is.na(items))
  score <- rep(NA_real_, nrow(items))
  score[n_answered == 3] <- ((rowSums(items[n_answered == 3, , drop = FALSE]) - 3) / 18) * 100
  score[n_answered == 2] <- ((rowSums(items[n_answered == 2, , drop = FALSE], na.rm = TRUE) - 2) / 12) * 100
  score
}

# TSQM-II global satisfaction.
tsqm_global_score <- function(item_10, item_11) {
  items <- data.frame(item_10, item_11)
  complete <- complete.cases(items)
  score <- rep(NA_real_, nrow(items))
  score[complete] <- ((rowSums(items[complete, , drop = FALSE]) - 2) / 12) * 100
  score
}

# Read raw workbooks and Mini-EAT map.
questionnaire_raw <- read_excel(questionnaire_file)
weight_raw <- read_excel(weight_file)
fitabase_weight_long_raw <- read_csv(fitabase_weight_long_file, show_col_types = FALSE)
glp1_weight_loss_raw <- read_excel(glp1_weight_loss_file, sheet = "Pre-Enrollment")
mini_eat_map <- read_csv(mini_eat_map_file, show_col_types = FALSE)

# Guard against empty or misread inputs.
stopifnot(nrow(questionnaire_raw) > 0)
stopifnot(nrow(weight_raw) > 0)
stopifnot(nrow(fitabase_weight_long_raw) > 0)
stopifnot(nrow(glp1_weight_loss_raw) > 0)
stopifnot(nrow(mini_eat_map) == 57)

# Required questionnaire fields.
questionnaire_required <- c(
  "RecordID", "StudyID", "Group", "age", "gender", "ethinicty", "race", "education",
  paste0("employment___", 1:10),
  "income", "seek_care", "med_coverage", "ppl_home", "children",
  "pdq", "phq", paste0("me", 1:9),
  "fu_pdq", "fu_phq", paste0("fu_me", 1:9),
  paste0("g1_tapq", 1:6), paste0("g2_tapq", 1:6), paste0("g3_tapq", 1:6),
  paste0("g1_tsqm", 1:11), paste0("g2_tsqm", 1:11), paste0("g3_tsqm", 1:11)
)

# Required weight fields.
weight_required <- c(
  "StudyID", "Group", "Consent Date", "M1Date", "M1Weight", "M1BMI",
  "M2Date", "M2Weight", "M2BMI", "M3Date", "M3Weight", "M3BMI", "M4Date", "M4Weight", "M4BMI"
)
fitabase_weight_required <- c(
  "study_id", "group", "arm", "consent_date", "visit_month", "visit_date",
  "weight_kg", "bmi_kg_m2", "month_since_baseline",
  "baseline_weight_kg", "baseline_bmi_kg_m2", "pct_weight_change"
)
glp1_weight_loss_required <- c(
  "Participant ID", "Group", "Pre-medication", "Post-medication",
  "Pre-enrollment kg", "% lost"
)

# Required raw fields.
stopifnot(all(questionnaire_required %in% names(questionnaire_raw)))
stopifnot(all(weight_required %in% names(weight_raw)))
stopifnot(all(fitabase_weight_required %in% names(fitabase_weight_long_raw)))
stopifnot(all(glp1_weight_loss_required %in% names(glp1_weight_loss_raw)))

# REDCap employment fields.
employment_cols <- paste0("employment___", 1:10)

# Baseline Mini-EAT inputs. Keep mapping explicit for QA.
baseline_model <- tibble(
  study_id = questionnaire_raw$StudyID,
  # Direct matches to Mayo items 1-3.
  fruits = mini_eat_model_value(1, questionnaire_raw$me1),
  vegetable = mini_eat_model_value(2, questionnaire_raw$me2),
  legume_nut_seed = mini_eat_model_value(3, questionnaire_raw$me3),
  # REMAP: me4 -> fish; me5 -> whole grains; me6 -> refined grains.
  fish = mini_eat_model_value(6, questionnaire_raw$me4),
  grains_whole = mini_eat_model_value(4, questionnaire_raw$me5),
  grains_refined = mini_eat_model_value(5, questionnaire_raw$me6),
  dairy_low_fat = mini_eat_model_value(7, questionnaire_raw$me7),
  dairy_high_fat_satur = mini_eat_model_value(8, questionnaire_raw$me8),
  sweets = mini_eat_model_value(9, questionnaire_raw$me9)
) %>%
  mutate(
    # Score only complete 9-item sets.
    baseline_mini_eat_score = mini_eat_score(
      fruits, vegetable, legume_nut_seed, grains_whole, grains_refined,
      fish, dairy_low_fat, dairy_high_fat_satur, sweets
    )
  )

# Follow-up Mini-EAT inputs.
followup_model <- tibble(
  study_id = questionnaire_raw$StudyID,
  fruits = mini_eat_model_value(1, questionnaire_raw$fu_me1),
  vegetable = mini_eat_model_value(2, questionnaire_raw$fu_me2),
  legume_nut_seed = mini_eat_model_value(3, questionnaire_raw$fu_me3),
  fish = mini_eat_model_value(6, questionnaire_raw$fu_me4),
  grains_whole = mini_eat_model_value(4, questionnaire_raw$fu_me5),
  grains_refined = mini_eat_model_value(5, questionnaire_raw$fu_me6),
  dairy_low_fat = mini_eat_model_value(7, questionnaire_raw$fu_me7),
  dairy_high_fat_satur = mini_eat_model_value(8, questionnaire_raw$fu_me8),
  sweets = mini_eat_model_value(9, questionnaire_raw$fu_me9)
) %>%
  mutate(
    followup_mini_eat_score = mini_eat_score(
      fruits, vegetable, legume_nut_seed, grains_whole, grains_refined,
      fish, dairy_low_fat, dairy_high_fat_satur, sweets
    )
  )

# One row per participant.
participants <- questionnaire_raw %>%
  transmute(
    # IDs and arm.
    record_id = RecordID,
    study_id = as.integer(parse_numeric(StudyID)),
    group = as.integer(parse_numeric(Group)),
    arm = case_when(
      Group == 1 ~ "Control",
      Group == 2 ~ "Noom",
      Group == 3 ~ "MTM",
      TRUE ~ NA_character_
    ),
    # Baseline demographics.
    age = parse_numeric(age),
    gender = parse_numeric(gender),
    ethinicty = parse_numeric(ethinicty),
    race = parse_numeric(race),
    education = parse_numeric(education),
    employment_code = single_selected_code(questionnaire_raw, employment_cols),
    income = parse_numeric(income),
    seek_care = parse_numeric(seek_care),
    med_coverage = parse_numeric(med_coverage),
    ppl_home = parse_numeric(ppl_home),
    children = parse_numeric(children),
    # PDQ/PHQ ... direction: 1 = excellent, 5 = poor.
    baseline_pdq = clean_1_to_5_with_pna_6(pdq),
    baseline_phq = clean_1_to_5_with_pna_6(phq),
    followup_pdq = clean_1_to_5_with_pna_6(fu_pdq),
    followup_phq = clean_1_to_5_with_pna_6(fu_phq),
    # Mini-EAT scores from helper tables above.
    baseline_mini_eat_score = baseline_model$baseline_mini_eat_score,
    followup_mini_eat_score = followup_model$followup_mini_eat_score,
    # One arm-specific TAPQ/TSQM form is populated per participant.
    tapq_item_1 = tapq_clean_item(1, coalesce(g1_tapq1, g2_tapq1, g3_tapq1)),
    tapq_item_2 = tapq_clean_item(2, coalesce(g1_tapq2, g2_tapq2, g3_tapq2)),
    tapq_item_3 = tapq_clean_item(3, coalesce(g1_tapq3, g2_tapq3, g3_tapq3)),
    tapq_item_4 = tapq_clean_item(4, coalesce(g1_tapq4, g2_tapq4, g3_tapq4)),
    tapq_item_5 = tapq_clean_item(5, coalesce(g1_tapq5, g2_tapq5, g3_tapq5)),
    tapq_item_6 = tapq_clean_item(6, coalesce(g1_tapq6, g2_tapq6, g3_tapq6)),
    tsqm_item_1 = tsqm_clean_item(1, coalesce(g1_tsqm1, g2_tsqm1, g3_tsqm1)),
    tsqm_item_2 = tsqm_clean_item(2, coalesce(g1_tsqm2, g2_tsqm2, g3_tsqm2)),
    tsqm_item_3 = tsqm_clean_item(3, coalesce(g1_tsqm3, g2_tsqm3, g3_tsqm3)),
    tsqm_item_4 = tsqm_clean_item(4, coalesce(g1_tsqm4, g2_tsqm4, g3_tsqm4)),
    tsqm_item_5 = tsqm_clean_item(5, coalesce(g1_tsqm5, g2_tsqm5, g3_tsqm5)),
    tsqm_item_6 = tsqm_clean_item(6, coalesce(g1_tsqm6, g2_tsqm6, g3_tsqm6)),
    tsqm_item_7 = tsqm_clean_item(7, coalesce(g1_tsqm7, g2_tsqm7, g3_tsqm7)),
    tsqm_item_8 = tsqm_clean_item(8, coalesce(g1_tsqm8, g2_tsqm8, g3_tsqm8)),
    tsqm_item_9 = tsqm_clean_item(9, coalesce(g1_tsqm9, g2_tsqm9, g3_tsqm9)),
    tsqm_item_10 = tsqm_clean_item(10, coalesce(g1_tsqm10, g2_tsqm10, g3_tsqm10)),
    tsqm_item_11 = tsqm_clean_item(11, coalesce(g1_tsqm11, g2_tsqm11, g3_tsqm11))
  ) %>%
  mutate(
    # TAPQ harmonized items and complete-case total.
    tapq_item_1_pct = tapq_percent_item(1, tapq_item_1),
    tapq_item_2_pct = tapq_percent_item(2, tapq_item_2),
    tapq_item_3_pct = tapq_percent_item(3, tapq_item_3),
    tapq_item_4_pct = tapq_percent_item(4, tapq_item_4),
    tapq_item_5_pct = tapq_percent_item(5, tapq_item_5),
    tapq_item_6_pct = tapq_percent_item(6, tapq_item_6),
    tapq_score_0_100 = tapq_score(data.frame(
      tapq_item_1_pct,
      tapq_item_2_pct,
      tapq_item_3_pct,
      tapq_item_4_pct,
      tapq_item_5_pct,
      tapq_item_6_pct
    )),
    # TSQM-II domains.
    tsqm_effectiveness = tsqm_effectiveness_score(tsqm_item_1, tsqm_item_2),
    tsqm_side_effects = tsqm_side_effects_score(tsqm_item_3, tsqm_item_4, tsqm_item_5, tsqm_item_6),
    tsqm_convenience = tsqm_convenience_score(tsqm_item_7, tsqm_item_8, tsqm_item_9),
    tsqm_global_satisfaction = tsqm_global_score(tsqm_item_10, tsqm_item_11),
    # Keep side effects as a descriptive flag.
    tsqm_any_side_effect = case_when(
      tsqm_item_3 == 1 ~ "Yes",
      tsqm_item_3 == 2 ~ "No",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(study_id)

# Pre-GLP-1 to cessation weight-loss source.
glp1_weight_loss <- glp1_weight_loss_raw %>%
  transmute(
    study_id = as.integer(parse_numeric(`Participant ID`)),
    glp1_weight_loss_group = as.integer(parse_numeric(Group)),
    pre_glp1_weight_lb = parse_numeric(`Pre-medication`),
    post_glp1_cessation_weight_lb = parse_numeric(`Post-medication`),
    post_glp1_cessation_weight_kg = parse_numeric(`Pre-enrollment kg`),
    glp1_weight_loss_pct_source = parse_numeric(`% lost`)
  ) %>%
  mutate(
    # Calculate absolute loss from pre/post pounds.
    glp1_weight_loss_lb = pre_glp1_weight_lb - post_glp1_cessation_weight_lb,
    glp1_weight_loss_kg = glp1_weight_loss_lb * 0.45359237,
    # Recompute percent loss when source percent is absent.
    glp1_weight_loss_pct_calculated = if_else(
      !is.na(pre_glp1_weight_lb) & pre_glp1_weight_lb > 0,
      100 * glp1_weight_loss_lb / pre_glp1_weight_lb,
      NA_real_
    ),
    # Preserve source percent as primary; use calculation as fallback.
    glp1_weight_loss_pct = coalesce(glp1_weight_loss_pct_source, glp1_weight_loss_pct_calculated)
  ) %>%
  select(
    study_id,
    glp1_weight_loss_group,
    pre_glp1_weight_lb,
    post_glp1_cessation_weight_lb,
    post_glp1_cessation_weight_kg,
    glp1_weight_loss_lb,
    glp1_weight_loss_kg,
    glp1_weight_loss_pct_source,
    glp1_weight_loss_pct_calculated,
    glp1_weight_loss_pct
  ) %>%
  arrange(study_id)

# Check GLP-1 weight-loss group codes.
glp1_weight_loss_join_check <- participants %>%
  select(study_id, group) %>%
  left_join(
    glp1_weight_loss %>% select(study_id, glp1_weight_loss_group),
    by = "study_id"
  )

# Long questionnaire file for summaries and plots.
# TAPQ/TSQM are follow-up only by design.
questionnaire_long <- bind_rows(
  participants %>%
    transmute(
      study_id,
      arm,
      timepoint = "baseline",
      pdq_score = baseline_pdq,
      phq_score = baseline_phq,
      mini_eat_score = baseline_mini_eat_score,
      tapq_score_0_100 = NA_real_,
      tsqm_effectiveness = NA_real_,
      tsqm_side_effects = NA_real_,
      tsqm_convenience = NA_real_,
      tsqm_global_satisfaction = NA_real_
    ),
  participants %>%
    transmute(
      study_id,
      arm,
      timepoint = "followup",
      pdq_score = followup_pdq,
      phq_score = followup_phq,
      mini_eat_score = followup_mini_eat_score,
      tapq_score_0_100,
      tsqm_effectiveness,
      tsqm_side_effects,
      tsqm_convenience,
      tsqm_global_satisfaction
    )
) %>%
  mutate(timepoint = factor(timepoint, levels = c("baseline", "followup"))) %>%
  arrange(study_id, timepoint)

# Standardize the legacy weight workbook for archived comparison only.
legacy_weight_wide <- weight_raw %>%
  transmute(
    # IDs and arm.
    study_id = as.integer(parse_numeric(StudyID)),
    group = as.integer(parse_numeric(Group)),
    arm = case_when(
      Group == 1 ~ "Control",
      Group == 2 ~ "Noom",
      Group == 3 ~ "MTM",
      TRUE ~ NA_character_
    ),
    # Retain consent date to keep the derived file matched to the raw workbook.
    consent_date = parse_excel_date(`Consent Date`),
    # Month-specific fields before pivot.
    month_1_date = parse_excel_date(M1Date),
    month_1_weight_kg = parse_numeric(M1Weight),
    month_1_bmi_kg_m2 = parse_numeric(M1BMI),
    month_2_date = parse_excel_date(M2Date),
    month_2_weight_kg = parse_numeric(M2Weight),
    month_2_bmi_kg_m2 = parse_numeric(M2BMI),
    month_3_date = parse_excel_date(M3Date),
    month_3_weight_kg = parse_numeric(M3Weight),
    month_3_bmi_kg_m2 = parse_numeric(M3BMI),
    month_4_date = parse_excel_date(M4Date),
    month_4_weight_kg = parse_numeric(M4Weight),
    month_4_bmi_kg_m2 = parse_numeric(M4BMI)
  )

# Legacy wide-to-long weight reshape.
# `month_since_baseline` is the model time scale.
weight_long_workbook_legacy <- legacy_weight_wide %>%
  pivot_longer(
    cols = c(
      month_1_date, month_1_weight_kg, month_1_bmi_kg_m2,
      month_2_date, month_2_weight_kg, month_2_bmi_kg_m2,
      month_3_date, month_3_weight_kg, month_3_bmi_kg_m2,
      month_4_date, month_4_weight_kg, month_4_bmi_kg_m2
    ),
    names_to = c("visit_month", ".value"),
    names_pattern = "month_(\\d+)_(date|weight_kg|bmi_kg_m2)"
  ) %>%
  mutate(
    # Workbook month 1-4; model time 0-3.
    visit_month = as.integer(visit_month),
    month_since_baseline = visit_month - 1L
  ) %>%
  rename(visit_date = date) %>%
  arrange(study_id, visit_month)

# Month 1 weight/BMI reused as baseline in the legacy workbook-derived file.
legacy_baseline_weight <- weight_long_workbook_legacy %>%
  filter(visit_month == 1) %>%
  transmute(
    study_id,
    baseline_weight_kg = weight_kg,
    baseline_bmi_kg_m2 = bmi_kg_m2
  )

# Add baseline weight and percent change from baseline to the legacy file.
weight_long_workbook_legacy <- weight_long_workbook_legacy %>%
  left_join(legacy_baseline_weight, by = "study_id") %>%
  mutate(
    pct_weight_change = 100 * (weight_kg - baseline_weight_kg) / baseline_weight_kg
  )

# Primary longitudinal weight data now come from the Fitabase-derived assignment
# sidecar, which includes PI-confirmed manual scale-photo entries.
weight_long <- fitabase_weight_long_raw %>%
  transmute(
    study_id = as.integer(study_id),
    group = as.integer(group),
    arm = as.character(arm),
    consent_date = as.Date(consent_date),
    visit_month = as.integer(visit_month),
    visit_date = as.Date(visit_date),
    weight_kg = as.numeric(weight_kg),
    bmi_kg_m2 = as.numeric(bmi_kg_m2),
    month_since_baseline = as.integer(month_since_baseline),
    baseline_weight_kg = as.numeric(baseline_weight_kg),
    baseline_bmi_kg_m2 = as.numeric(baseline_bmi_kg_m2),
    pct_weight_change = as.numeric(pct_weight_change)
  ) %>%
  left_join(
    glp1_weight_loss %>%
      select(
        study_id,
        pre_glp1_weight_lb,
        post_glp1_cessation_weight_lb,
        post_glp1_cessation_weight_kg,
        glp1_weight_loss_lb,
        glp1_weight_loss_kg,
        glp1_weight_loss_pct_source,
        glp1_weight_loss_pct_calculated,
        glp1_weight_loss_pct
      ),
    by = "study_id"
  ) %>%
  arrange(study_id, visit_month)

baseline_weight <- weight_long %>%
  filter(month_since_baseline == 0) %>%
  transmute(
    study_id,
    baseline_weight_kg = weight_kg,
    baseline_bmi_kg_m2 = bmi_kg_m2
  )

weight_registry <- legacy_weight_wide %>%
  select(study_id, group, arm, consent_date)

# Table 1 baseline file.
baseline_analysis <- participants %>%
  select(
    # Table 1 variables only.
    study_id, arm, age, gender, ethinicty, race, education, employment_code,
    income, seek_care, med_coverage, ppl_home, children, baseline_pdq, baseline_phq, baseline_mini_eat_score
  ) %>%
  left_join(
    baseline_weight,
    by = "study_id"
  ) %>%
  left_join(
    glp1_weight_loss %>%
      select(
        study_id,
        pre_glp1_weight_lb,
        post_glp1_cessation_weight_lb,
        post_glp1_cessation_weight_kg,
        glp1_weight_loss_lb,
        glp1_weight_loss_kg,
        glp1_weight_loss_pct_source,
        glp1_weight_loss_pct_calculated,
        glp1_weight_loss_pct
      ),
    by = "study_id"
  ) %>%
  arrange(study_id)

# Fail if participant structure broke.
stopifnot(nrow(participants) == dplyr::n_distinct(participants$study_id))
stopifnot(nrow(glp1_weight_loss) == dplyr::n_distinct(glp1_weight_loss$study_id))
stopifnot(nrow(weight_registry) == dplyr::n_distinct(weight_registry$study_id))
stopifnot(nrow(weight_long) == nrow(dplyr::distinct(weight_long, study_id, visit_month)))
stopifnot(all(weight_long$visit_month == weight_long$month_since_baseline + 1L))
stopifnot(identical(sort(participants$study_id), sort(weight_registry$study_id)))
stopifnot(identical(sort(participants$study_id), sort(unique(weight_long$study_id))))
stopifnot(identical(sort(participants$study_id), sort(glp1_weight_loss$study_id)))
stopifnot(all(glp1_weight_loss_join_check$group == glp1_weight_loss_join_check$glp1_weight_loss_group))

# Compact Step-02 QA CSVs.
row_counts <- tibble(
  # Dataset shapes.
  object = c(
    "questionnaire_raw",
    "weight_raw_legacy_workbook",
    "fitabase_assigned_weight_long_raw",
    "glp1_weight_loss_raw",
    "participants",
    "glp1_weight_loss",
    "questionnaire_long",
    "weight_long_workbook_legacy",
    "weight_long",
    "baseline_analysis"
  ),
  rows = c(
    nrow(questionnaire_raw),
    nrow(weight_raw),
    nrow(fitabase_weight_long_raw),
    nrow(glp1_weight_loss_raw),
    nrow(participants),
    nrow(glp1_weight_loss),
    nrow(questionnaire_long),
    nrow(weight_long_workbook_legacy),
    nrow(weight_long),
    nrow(baseline_analysis)
  ),
  columns = c(
    ncol(questionnaire_raw),
    ncol(weight_raw),
    ncol(fitabase_weight_long_raw),
    ncol(glp1_weight_loss_raw),
    ncol(participants),
    ncol(glp1_weight_loss),
    ncol(questionnaire_long),
    ncol(weight_long_workbook_legacy),
    ncol(weight_long),
    ncol(baseline_analysis)
  )
)

score_ranges <- tibble(
  # Observed score ranges.
  variable = c(
    "baseline_mini_eat_score", "followup_mini_eat_score",
    "baseline_pdq", "followup_pdq",
    "baseline_phq", "followup_phq",
    "tapq_score_0_100",
    "tsqm_effectiveness", "tsqm_side_effects", "tsqm_convenience", "tsqm_global_satisfaction",
    "glp1_weight_loss_pct",
    "pct_weight_change"
  ),
  n_non_missing = c(
    sum(!is.na(participants$baseline_mini_eat_score)),
    sum(!is.na(participants$followup_mini_eat_score)),
    sum(!is.na(participants$baseline_pdq)),
    sum(!is.na(participants$followup_pdq)),
    sum(!is.na(participants$baseline_phq)),
    sum(!is.na(participants$followup_phq)),
    sum(!is.na(participants$tapq_score_0_100)),
    sum(!is.na(participants$tsqm_effectiveness)),
    sum(!is.na(participants$tsqm_side_effects)),
    sum(!is.na(participants$tsqm_convenience)),
    sum(!is.na(participants$tsqm_global_satisfaction)),
    sum(!is.na(glp1_weight_loss$glp1_weight_loss_pct)),
    sum(!is.na(weight_long$pct_weight_change))
  ),
  min = c(
    min(participants$baseline_mini_eat_score, na.rm = TRUE),
    min(participants$followup_mini_eat_score, na.rm = TRUE),
    min(participants$baseline_pdq, na.rm = TRUE),
    min(participants$followup_pdq, na.rm = TRUE),
    min(participants$baseline_phq, na.rm = TRUE),
    min(participants$followup_phq, na.rm = TRUE),
    min(participants$tapq_score_0_100, na.rm = TRUE),
    min(participants$tsqm_effectiveness, na.rm = TRUE),
    min(participants$tsqm_side_effects, na.rm = TRUE),
    min(participants$tsqm_convenience, na.rm = TRUE),
    min(participants$tsqm_global_satisfaction, na.rm = TRUE),
    min(glp1_weight_loss$glp1_weight_loss_pct, na.rm = TRUE),
    min(weight_long$pct_weight_change, na.rm = TRUE)
  ),
  max = c(
    max(participants$baseline_mini_eat_score, na.rm = TRUE),
    max(participants$followup_mini_eat_score, na.rm = TRUE),
    max(participants$baseline_pdq, na.rm = TRUE),
    max(participants$followup_pdq, na.rm = TRUE),
    max(participants$baseline_phq, na.rm = TRUE),
    max(participants$followup_phq, na.rm = TRUE),
    max(participants$tapq_score_0_100, na.rm = TRUE),
    max(participants$tsqm_effectiveness, na.rm = TRUE),
    max(participants$tsqm_side_effects, na.rm = TRUE),
    max(participants$tsqm_convenience, na.rm = TRUE),
    max(participants$tsqm_global_satisfaction, na.rm = TRUE),
    max(glp1_weight_loss$glp1_weight_loss_pct, na.rm = TRUE),
    max(weight_long$pct_weight_change, na.rm = TRUE)
  )
)

id_checks <- tibble(
  # ID checks.
  check = c(
    "questionnaire participant IDs unique",
    "legacy weight registry participant IDs unique",
    "GLP-1 weight-loss participant IDs unique",
    "derived Fitabase weight participant-visit rows unique",
    "participant IDs match across questionnaire and legacy weight registry",
    "participant IDs match across questionnaire and Fitabase assigned weights",
    "participant IDs match across questionnaire and GLP-1 weight-loss file",
    "GLP-1 weight-loss file group codes match questionnaire",
    "employment selection exactly one per participant"
  ),
  passed = c(
    nrow(participants) == dplyr::n_distinct(participants$study_id),
    nrow(weight_raw) == dplyr::n_distinct(weight_raw$StudyID),
    nrow(glp1_weight_loss) == dplyr::n_distinct(glp1_weight_loss$study_id),
    nrow(weight_long) == nrow(dplyr::distinct(weight_long, study_id, visit_month)),
    identical(sort(participants$study_id), sort(weight_registry$study_id)),
    identical(sort(participants$study_id), sort(unique(weight_long$study_id))),
    identical(sort(participants$study_id), sort(glp1_weight_loss$study_id)),
    all(glp1_weight_loss_join_check$group == glp1_weight_loss_join_check$glp1_weight_loss_group),
    all(selected_n(questionnaire_raw, employment_cols) == 1)
  )
)

# Write analysis datasets.
readr::write_csv(participants, file.path(data_dir, "participants.csv"), na = "")
readr::write_csv(glp1_weight_loss, file.path(data_dir, "glp1_weight_loss.csv"), na = "")
readr::write_csv(questionnaire_long, file.path(data_dir, "questionnaire_long.csv"), na = "")
readr::write_csv(weight_long_workbook_legacy, file.path(data_dir, "weight_long_workbook_legacy.csv"), na = "")
readr::write_csv(weight_long, file.path(data_dir, "weight_long.csv"), na = "")
readr::write_csv(baseline_analysis, file.path(data_dir, "baseline_analysis.csv"), na = "")

# Write Step-02 QA CSVs.
readr::write_csv(row_counts, file.path(qa_dir, "step_02_row_counts.csv"))
readr::write_csv(score_ranges, file.path(qa_dir, "step_02_score_ranges.csv"))
readr::write_csv(id_checks, file.path(qa_dir, "step_02_id_checks.csv"))

message("Step 02 complete: cleaned analysis data written to ", data_dir)
