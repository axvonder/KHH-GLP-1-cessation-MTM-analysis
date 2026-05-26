#!/usr/bin/env Rscript

# Step 00. Append confirmed manual entries to the Fitabase weight log.

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
})

# Resolve script path for terminal use or `source()`.
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

  normalizePath(file.path(getwd(), "00_make_fitabase_with_manual_imputations.R"), mustWork = FALSE)
}

# Parse mixed numeric exports.
parse_numeric <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

# Parse Excel dates and placeholders.
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

# Format appended rows like Fitabase timestamps.
format_fitabase_date <- function(date) {
  ifelse(
    is.na(date),
    NA_character_,
    paste0(month(date), "/", day(date), "/", year(date), " 23:59")
  )
}

# Pipeline paths.
script_path <- resolve_script_path()
pipeline_dir <- dirname(dirname(script_path))
sidecar_dir <- file.path(pipeline_dir, "output", "weight_log_assignment")
dir.create(sidecar_dir, recursive = TRUE, showWarnings = FALSE)

# Inputs.
fitabase_file <- file.path(pipeline_dir, "weightLogInfo_merged.csv")
weight_workbook_file <- file.path(pipeline_dir, "inputs", "raw", "Weight and BMI.xlsx")

# Required inputs.
stopifnot(file.exists(fitabase_file))
stopifnot(file.exists(weight_workbook_file))

# Read source files.
fitabase_raw <- read_csv(
  fitabase_file,
  col_types = cols(
    .default = col_guess(),
    LogId = col_character()
  )
)
weight_workbook <- read_excel(weight_workbook_file)

# Required source fields.
required_fitabase_cols <- c("Id", "Date", "WeightKg", "WeightPounds", "Fat", "BMI", "IsManualReport", "LogId")
required_weight_cols <- c(
  "StudyID", "Group", "Consent Date",
  "M1Date", "M1Weight", "M1BMI",
  "M2Date", "M2Weight", "M2BMI",
  "M3Date", "M3Weight", "M3BMI",
  "M4Date", "M4Weight", "M4BMI"
)

stopifnot(all(required_fitabase_cols %in% names(fitabase_raw)))
stopifnot(all(required_weight_cols %in% names(weight_workbook)))

# PI-confirmed workbook rows to recover.
manual_confirmed_ids <- c(16L, 25L, 37L)
weight_tolerance_kg <- 0.05

# Add audit columns to existing Fitabase rows.
fitabase_standard <- fitabase_raw %>%
  mutate(
    ImputedFromWeightAndBMI = FALSE,
    ImputationSource = NA_character_,
    OriginalWeightSlot = NA_character_,
    OriginalWeightDate = NA_character_,
    OriginalWeightDateWasMissing = NA,
    DateImputationBasis = NA_character_,
    ManualEntryNote = NA_character_
  )

# Reshape confirmed workbook weights.
workbook_long <- weight_workbook %>%
  transmute(
    Id = as.integer(parse_numeric(StudyID)),
    consent_date = parse_excel_date(`Consent Date`),
    M1_date = parse_excel_date(M1Date),
    M1_weight_kg = parse_numeric(M1Weight),
    M1_bmi = parse_numeric(M1BMI),
    M2_date = parse_excel_date(M2Date),
    M2_weight_kg = parse_numeric(M2Weight),
    M2_bmi = parse_numeric(M2BMI),
    M3_date = parse_excel_date(M3Date),
    M3_weight_kg = parse_numeric(M3Weight),
    M3_bmi = parse_numeric(M3BMI),
    M4_date = parse_excel_date(M4Date),
    M4_weight_kg = parse_numeric(M4Weight),
    M4_bmi = parse_numeric(M4BMI)
  ) %>%
  filter(Id %in% manual_confirmed_ids) %>%
  pivot_longer(
    cols = matches("^M[1-4]_(date|weight_kg|bmi)$"),
    names_to = c("OriginalWeightSlot", ".value"),
    names_pattern = "^(M[1-4])_(date|weight_kg|bmi)$"
  ) %>%
  rename(
    original_date = date,
    WeightKg = weight_kg,
    BMI = bmi
  ) %>%
  filter(!is.na(WeightKg))

# Make raw Fitabase rows matchable.
fitabase_matchable <- fitabase_raw %>%
  transmute(
    Id = as.integer(parse_numeric(Id)),
    log_date = parse_excel_date(str_extract(as.character(Date), "^[^ ]+")),
    raw_date_text = trimws(as.character(Date)),
    WeightKg = parse_numeric(WeightKg)
  )

# Keep only confirmed rows not already in Fitabase.
manual_rows_to_append <- workbook_long %>%
  rowwise() %>%
  mutate(
    matching_fitabase_row = any(
      fitabase_matchable$Id == Id &
        abs(fitabase_matchable$WeightKg - WeightKg) <= weight_tolerance_kg &
        (is.na(original_date) | fitabase_matchable$log_date == original_date),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(!matching_fitabase_row) %>%
  mutate(
    final_date = coalesce(original_date, consent_date %m+% months(1)),
    OriginalWeightDateWasMissing = is.na(original_date),
    DateImputationBasis = if_else(
      OriginalWeightDateWasMissing,
      "One calendar month after consent date used because original M1Date is missing",
      "Original Weight and BMI date"
    ),
    Id = as.double(Id),
    Date = format_fitabase_date(final_date),
    WeightPounds = WeightKg * 2.20462262185,
    Fat = NA_real_,
    IsManualReport = TRUE,
    LogId = paste0("manual-entry-", Id, "-", OriginalWeightSlot),
    ImputedFromWeightAndBMI = TRUE,
    ImputationSource = "PI-confirmed manual scale-photo entry from Weight and BMI.xlsx",
    OriginalWeightDate = as.character(original_date),
    ManualEntryNote = "Manual row appended because weight existed in Weight and BMI.xlsx but not in weightLogInfo_merged.csv"
  ) %>%
  select(
    all_of(names(fitabase_raw)),
    ImputedFromWeightAndBMI,
    ImputationSource,
    OriginalWeightSlot,
    OriginalWeightDate,
    OriginalWeightDateWasMissing,
    DateImputationBasis,
    ManualEntryNote
  )

# Append confirmed manual rows.
fitabase_with_imputations <- bind_rows(
  fitabase_standard,
  manual_rows_to_append
)

# Outputs.
output_file <- file.path(sidecar_dir, "fitabase_with_imputations.csv")
audit_file <- file.path(sidecar_dir, "fitabase_manual_imputation_audit.csv")

# Write imputed Fitabase log and audit.
write_csv(fitabase_with_imputations, output_file, na = "")
write_csv(manual_rows_to_append, audit_file, na = "")

message("Wrote ", output_file)
message("Wrote ", audit_file)
message("Step 00 complete: rows appended = ", nrow(manual_rows_to_append))
