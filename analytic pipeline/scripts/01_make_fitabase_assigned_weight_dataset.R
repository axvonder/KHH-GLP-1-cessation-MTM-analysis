#!/usr/bin/env Rscript

# Step 01. Assign Fitabase weights to baseline and monthly follow-up visits.

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(grid)
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

  normalizePath(file.path(getwd(), "01_make_fitabase_assigned_weight_dataset.R"), mustWork = FALSE)
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

# Parse Fitabase datetimes in local time.
parse_log_datetime <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", ".", "NA", "N/A")] <- NA_character_

  out <- suppressWarnings(mdy_hm(x, tz = "America/New_York"))

  needs_parse <- is.na(out) & !is.na(x)
  out[needs_parse] <- suppressWarnings(mdy_hms(x[needs_parse], tz = "America/New_York"))

  needs_parse <- is.na(out) & !is.na(x)
  out[needs_parse] <- suppressWarnings(ymd_hm(x[needs_parse], tz = "America/New_York"))

  needs_parse <- is.na(out) & !is.na(x)
  out[needs_parse] <- suppressWarnings(ymd_hms(x[needs_parse], tz = "America/New_York"))

  needs_parse <- is.na(out) & !is.na(x)
  out[needs_parse] <- as.POSIXct(
    suppressWarnings(mdy(x[needs_parse])),
    tz = "America/New_York"
  )

  needs_parse <- is.na(out) & !is.na(x)
  out[needs_parse] <- as.POSIXct(
    suppressWarnings(ymd(x[needs_parse])),
    tz = "America/New_York"
  )

  out
}

# Define baseline plus four monthly slots.
visit_spec <- function() {
  tibble(
    timepoint = c("baseline", paste0("M", 1:4)),
    timepoint_label = c("Baseline", paste("Month", 1:4)),
    visit_month = 1:5,
    month_since_baseline = 0:4,
    target_offset_days = c(0L, 30L, 60L, 90L, 120L)
  )
}

# Return a typed empty assignment table.
empty_assignment <- function() {
  tibble(
    timepoint = character(),
    daily_row_id = integer(),
    selected_fitabase_row_id = integer(),
    log_datetime = as.POSIXct(character()),
    log_date = as.Date(character()),
    weight_kg = double(),
    weight_pounds = double(),
    bmi_kg_m2 = double(),
    fat = double(),
    is_manual_report = logical(),
    imputed_from_weight_and_bmi = logical(),
    log_id = character(),
    n_logs_same_day = integer(),
    n_distinct_weights_same_day = integer(),
    daily_selection_rule = character(),
    same_day_weight_range_kg = double(),
    same_day_log_values = character(),
    same_day_large_discrepancy = logical(),
    days_from_target = integer(),
    abs_days_from_target = integer(),
    assignment_pass = integer()
  )
}

# Greedily match closest unused logs to visit targets.
select_assignment_pairs <- function(targets, logs, assignment_pass, window_days = Inf) {
  if (nrow(targets) == 0 || nrow(logs) == 0) {
    return(empty_assignment())
  }

  # Candidate all target/log pairs.
  candidates <- crossing(
    targets %>%
      select(timepoint, visit_month, month_since_baseline, target_date),
    logs
  ) %>%
    mutate(
      days_from_target = as.integer(log_date - target_date),
      abs_days_from_target = abs(days_from_target)
    ) %>%
    filter(abs_days_from_target <= window_days) %>%
    arrange(abs_days_from_target, visit_month, log_date, log_datetime, daily_row_id)

  if (nrow(candidates) == 0) {
    return(empty_assignment())
  }

  selected <- list()
  used_timepoints <- character()
  used_daily_rows <- integer()
  i <- 1L

  # Pick closest pairs without reusing visits or logs.
  repeat {
    remaining <- candidates %>%
      filter(
        !(timepoint %in% used_timepoints),
        !(daily_row_id %in% used_daily_rows)
      )

    if (nrow(remaining) == 0) {
      break
    }

    pick <- remaining[1, , drop = FALSE] %>%
      mutate(assignment_pass = assignment_pass)

    selected[[i]] <- pick
    used_timepoints <- c(used_timepoints, pick$timepoint)
    used_daily_rows <- c(used_daily_rows, pick$daily_row_id)
    candidates <- remaining[-1, , drop = FALSE]
    i <- i + 1L
  }

  bind_rows(selected) %>%
    select(
      timepoint,
      daily_row_id,
      selected_fitabase_row_id,
      log_datetime,
      log_date,
      weight_kg,
      weight_pounds,
      bmi_kg_m2,
      fat,
      is_manual_report,
      imputed_from_weight_and_bmi,
      log_id,
      n_logs_same_day,
      n_distinct_weights_same_day,
      daily_selection_rule,
      same_day_weight_range_kg,
      same_day_log_values,
      same_day_large_discrepancy,
      days_from_target,
      abs_days_from_target,
      assignment_pass
    )
}

# Fallback: maximize assignments while preserving visit order.
select_ordered_fallback_pairs <- function(targets, logs, fixed_assignments, assignment_pass = 2L) {
  if (nrow(targets) == 0 || nrow(logs) == 0) {
    return(empty_assignment())
  }

  target_rows <- targets %>%
    select(timepoint, visit_month, month_since_baseline, target_date) %>%
    arrange(visit_month)

  fixed_order <- fixed_assignments %>%
    filter(!is.na(log_date)) %>%
    select(timepoint, log_date) %>%
    left_join(
      visit_spec() %>% select(timepoint, visit_month),
      by = "timepoint"
    )

  # Prebuild target-specific candidate lists.
  candidates_by_target <- lapply(seq_len(nrow(target_rows)), function(i) {
    target_row <- target_rows[i, , drop = FALSE]

    logs %>%
      mutate(
        timepoint = target_row$timepoint,
        visit_month = target_row$visit_month,
        month_since_baseline = target_row$month_since_baseline,
        target_date = target_row$target_date,
        days_from_target = as.integer(log_date - target_date),
        abs_days_from_target = abs(days_from_target),
        assignment_pass = assignment_pass
      ) %>%
      arrange(abs_days_from_target, log_date, log_datetime, daily_row_id)
  })

  empty <- empty_assignment()
  best <- NULL

  # Prefer more assignments, then shorter distances.
  is_better_solution <- function(candidate, incumbent) {
    if (is.null(incumbent)) {
      return(TRUE)
    }

    candidate_n <- nrow(candidate)
    incumbent_n <- nrow(incumbent)
    if (candidate_n != incumbent_n) {
      return(candidate_n > incumbent_n)
    }

    candidate_total_distance <- sum(candidate$abs_days_from_target, na.rm = TRUE)
    incumbent_total_distance <- sum(incumbent$abs_days_from_target, na.rm = TRUE)
    if (candidate_total_distance != incumbent_total_distance) {
      return(candidate_total_distance < incumbent_total_distance)
    }

    candidate_max_distance <- max(candidate$abs_days_from_target, na.rm = TRUE)
    incumbent_max_distance <- max(incumbent$abs_days_from_target, na.rm = TRUE)
    if (is.infinite(candidate_max_distance)) candidate_max_distance <- 0
    if (is.infinite(incumbent_max_distance)) incumbent_max_distance <- 0
    if (candidate_max_distance != incumbent_max_distance) {
      return(candidate_max_distance < incumbent_max_distance)
    }

    candidate_dates <- paste(candidate$log_date, collapse = "|")
    incumbent_dates <- paste(incumbent$log_date, collapse = "|")
    candidate_dates < incumbent_dates
  }

  # Keep assigned dates strictly increasing.
  is_chronologically_valid <- function(candidate, chosen) {
    assigned_order <- bind_rows(
      fixed_order,
      chosen %>%
        select(timepoint, log_date) %>%
        left_join(
          visit_spec() %>% select(timepoint, visit_month),
          by = "timepoint"
        )
    )

    previous_dates <- assigned_order$log_date[
      !is.na(assigned_order$log_date) &
        assigned_order$visit_month < candidate$visit_month
    ]
    next_dates <- assigned_order$log_date[
      !is.na(assigned_order$log_date) &
        assigned_order$visit_month > candidate$visit_month
    ]

    after_previous <- length(previous_dates) == 0 ||
      candidate$log_date > max(previous_dates)
    before_next <- length(next_dates) == 0 ||
      candidate$log_date < min(next_dates)

    after_previous && before_next
  }

  # Exhaustively search the small fallback space.
  search <- function(target_index, chosen, used_daily_rows) {
    if (target_index > nrow(target_rows)) {
      chosen_out <- chosen %>%
        arrange(match(timepoint, target_rows$timepoint)) %>%
        select(all_of(names(empty)))

      if (is_better_solution(chosen_out, best)) {
        best <<- chosen_out
      }
      return(invisible(NULL))
    }

    search(target_index + 1L, chosen, used_daily_rows)

    available_candidates <- candidates_by_target[[target_index]] %>%
      filter(!(daily_row_id %in% used_daily_rows))

    if (nrow(available_candidates) == 0) {
      return(invisible(NULL))
    }

    for (i in seq_len(nrow(available_candidates))) {
      candidate <- available_candidates[i, , drop = FALSE]

      if (!is_chronologically_valid(candidate, chosen)) {
        next
      }

      search(
        target_index + 1L,
        bind_rows(chosen, candidate %>% select(all_of(names(empty)))),
        c(used_daily_rows, candidate$daily_row_id)
      )
    }

    invisible(NULL)
  }

  search(1L, empty, integer())

  if (is.null(best)) {
    return(empty)
  }

  best
}

# Assign one participant's baseline and monthly weights.
assign_participant <- function(registry_row, daily_logs, window_days = 15) {
  # Use logs on/after consent only.
  participant_logs <- daily_logs %>%
    filter(
      study_id == registry_row$study_id,
      log_date >= registry_row$consent_date
    ) %>%
    arrange(log_date, log_datetime, daily_row_id)

  # Attach participant metadata to visit targets.
  targets_template <- visit_spec() %>%
    mutate(
      study_id = registry_row$study_id,
      group = registry_row$group,
      arm = registry_row$arm,
      consent_date = registry_row$consent_date
    )

  if (nrow(participant_logs) == 0) {
    return(targets_template %>%
      mutate(
        baseline_date = as.Date(NA),
        target_date = as.Date(NA)
      ) %>%
      left_join(empty_assignment(), by = "timepoint"))
  }

  # Baseline is the first daily weight on/after consent.
  baseline_log <- participant_logs[1, , drop = FALSE] %>%
    transmute(
      timepoint = "baseline",
      daily_row_id,
      selected_fitabase_row_id,
      log_datetime,
      log_date,
      weight_kg,
      weight_pounds,
      bmi_kg_m2,
      fat,
      is_manual_report,
      imputed_from_weight_and_bmi,
      log_id,
      n_logs_same_day,
      n_distinct_weights_same_day,
      daily_selection_rule,
      same_day_weight_range_kg,
      same_day_log_values,
      same_day_large_discrepancy,
      days_from_target = 0L,
      abs_days_from_target = 0L,
      assignment_pass = 0L
    )

  baseline_date <- baseline_log$log_date[1]

  targets <- targets_template %>%
    mutate(
      baseline_date = baseline_date,
      target_date = baseline_date + target_offset_days
    )

  followup_targets <- targets %>%
    filter(timepoint != "baseline")

  followup_logs <- participant_logs %>%
    filter(daily_row_id != baseline_log$daily_row_id[1])

  # Pass 1: closest log within +/-15 days.
  pass_1 <- select_assignment_pairs(
    targets = followup_targets,
    logs = followup_logs,
    assignment_pass = 1L,
    window_days = window_days
  )

  # Pass 2: closest remaining log, chronological.
  pass_2 <- select_ordered_fallback_pairs(
    targets = followup_targets %>% filter(!(timepoint %in% pass_1$timepoint)),
    logs = followup_logs %>% filter(!(daily_row_id %in% pass_1$daily_row_id)),
    fixed_assignments = bind_rows(baseline_log, pass_1),
    assignment_pass = 2L
  )

  targets %>%
    left_join(bind_rows(baseline_log, pass_1, pass_2), by = "timepoint")
}

# Pipeline paths.
script_path <- resolve_script_path()
pipeline_dir <- dirname(dirname(script_path))
input_dir <- file.path(pipeline_dir, "inputs")
sidecar_dir <- file.path(pipeline_dir, "output", "weight_log_assignment")
dir.create(sidecar_dir, recursive = TRUE, showWarnings = FALSE)

# Inputs.
fitabase_file <- file.path(sidecar_dir, "fitabase_with_imputations.csv")
weight_registry_file <- file.path(input_dir, "raw", "Weight and BMI.xlsx")

# Required inputs.
stopifnot(file.exists(fitabase_file))
stopifnot(file.exists(weight_registry_file))

# Read source files.
fitabase_raw <- read_csv(
  fitabase_file,
  col_types = cols(
    .default = col_guess(),
    LogId = col_character()
  )
)
weight_registry_raw <- read_excel(weight_registry_file)

# Required source fields.
required_fitabase_cols <- c("Id", "Date", "WeightKg", "WeightPounds", "Fat", "BMI", "IsManualReport", "LogId")
required_registry_cols <- c("StudyID", "Group", "Consent Date")
stopifnot(all(required_fitabase_cols %in% names(fitabase_raw)))
stopifnot(all(required_registry_cols %in% names(weight_registry_raw)))

# Participant registry.
registry <- weight_registry_raw %>%
  transmute(
    study_id = as.integer(parse_numeric(StudyID)),
    group = as.integer(parse_numeric(Group)),
    arm = case_when(
      group == 1 ~ "Control",
      group == 2 ~ "Noom",
      group == 3 ~ "MTM",
      TRUE ~ NA_character_
    ),
    consent_date = parse_excel_date(`Consent Date`)
  ) %>%
  arrange(study_id)

# Standardized Fitabase logs.
logs <- fitabase_raw %>%
  mutate(
    fitabase_row_id = row_number(),
    study_id = as.integer(parse_numeric(Id)),
    raw_log_datetime = Date,
    log_datetime = parse_log_datetime(Date),
    log_date = as.Date(log_datetime, tz = "America/New_York"),
    log_time = format(log_datetime, "%H:%M"),
    is_2359 = log_time == "23:59",
    weight_kg = parse_numeric(WeightKg),
    weight_pounds = parse_numeric(WeightPounds),
    bmi_kg_m2 = parse_numeric(BMI),
    fat = parse_numeric(Fat),
    is_manual_report = as.logical(IsManualReport),
    imputed_from_weight_and_bmi = if ("ImputedFromWeightAndBMI" %in% names(.)) {
      as.logical(ImputedFromWeightAndBMI)
    } else {
      FALSE
    },
    log_id = as.character(LogId)
  ) %>%
  select(
    fitabase_row_id,
    study_id,
    raw_log_datetime,
    log_datetime,
    log_date,
    log_time,
    is_2359,
    weight_kg,
    weight_pounds,
    bmi_kg_m2,
    fat,
    is_manual_report,
    imputed_from_weight_and_bmi,
    log_id
  ) %>%
  filter(!is.na(study_id), !is.na(log_date), !is.na(weight_kg)) %>%
  arrange(study_id, log_date, log_datetime, fitabase_row_id)

# Exact duplicate audit.
duplicate_groups <- logs %>%
  count(
    study_id,
    raw_log_datetime,
    log_date,
    weight_kg,
    weight_pounds,
    bmi_kg_m2,
    fat,
    is_manual_report,
    imputed_from_weight_and_bmi,
    log_id,
    name = "n_duplicate_rows"
  ) %>%
  filter(n_duplicate_rows > 1) %>%
  arrange(study_id, log_date, raw_log_datetime)

# Remove exact duplicate rows.
logs_deduped <- logs %>%
  group_by(
    study_id,
    raw_log_datetime,
    log_date,
    weight_kg,
    weight_pounds,
    bmi_kg_m2,
    fat,
    is_manual_report,
    imputed_from_weight_and_bmi,
    log_id
  ) %>%
  arrange(fitabase_row_id, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# Collapse same-day logs to one daily value.
daily_selection_audit <- logs_deduped %>%
  group_by(study_id, log_date) %>%
  arrange(log_datetime, fitabase_row_id, .by_group = TRUE) %>%
  mutate(
    preferred_daily_rank = case_when(
      any(!is_2359) & !is_2359 ~ row_number(),
      any(!is_2359) & is_2359 ~ 100000L + row_number(),
      TRUE ~ row_number()
    ),
    selected_for_daily_weight = preferred_daily_rank == min(preferred_daily_rank)
  ) %>%
  summarise(
    n_logs_same_day = n(),
    n_distinct_weights_same_day = n_distinct(round(weight_kg, 4)),
    selected_fitabase_row_id = fitabase_row_id[selected_for_daily_weight][1],
    selected_log_datetime = log_datetime[selected_for_daily_weight][1],
    selected_log_time = log_time[selected_for_daily_weight][1],
    selected_weight_kg = weight_kg[selected_for_daily_weight][1],
    selected_bmi_kg_m2 = bmi_kg_m2[selected_for_daily_weight][1],
    selected_fat = fat[selected_for_daily_weight][1],
    selected_is_manual_report = is_manual_report[selected_for_daily_weight][1],
    selected_imputed_from_weight_and_bmi = imputed_from_weight_and_bmi[selected_for_daily_weight][1],
    selected_log_id = log_id[selected_for_daily_weight][1],
    daily_selection_rule = if_else(
      any(!is_2359),
      "Earliest non-23:59 same-day log",
      "Earliest available same-day log"
    ),
    same_day_weight_range_kg = max(weight_kg, na.rm = TRUE) - min(weight_kg, na.rm = TRUE),
    same_day_log_values = paste(log_time, sprintf("%.3fkg", weight_kg), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(study_id, log_date)

# Daily weights used for visit assignment.
daily_logs <- daily_selection_audit %>%
  transmute(
    daily_row_id = row_number(),
    study_id,
    log_date,
    log_datetime = selected_log_datetime,
    selected_fitabase_row_id,
    weight_kg = selected_weight_kg,
    weight_pounds = selected_weight_kg * 2.20462262185,
    bmi_kg_m2 = selected_bmi_kg_m2,
    fat = selected_fat,
    is_manual_report = selected_is_manual_report,
    imputed_from_weight_and_bmi = selected_imputed_from_weight_and_bmi,
    log_id = selected_log_id,
    n_logs_same_day,
    n_distinct_weights_same_day,
    daily_selection_rule,
    same_day_weight_range_kg,
    same_day_log_values,
    same_day_large_discrepancy = same_day_weight_range_kg > 2
  ) %>%
  arrange(study_id, log_date, log_datetime, daily_row_id)

# Assign weights for every registered participant.
assignments <- bind_rows(lapply(seq_len(nrow(registry)), function(i) {
  assign_participant(registry[i, , drop = FALSE], daily_logs, window_days = 15)
})) %>%
  arrange(study_id, visit_month)

# Guard against non-chronological visit dates.
chronology_violations <- assignments %>%
  filter(!is.na(log_date)) %>%
  arrange(study_id, visit_month) %>%
  group_by(study_id) %>%
  mutate(
    previous_timepoint = lag(timepoint),
    previous_log_date = lag(log_date),
    chronology_violation = !is.na(previous_log_date) & log_date <= previous_log_date
  ) %>%
  ungroup() %>%
  filter(chronology_violation)

if (nrow(chronology_violations) > 0) {
  stop(
    "Weight assignment produced non-chronological visit dates for study IDs: ",
    paste(unique(chronology_violations$study_id), collapse = ", "),
    call. = FALSE
  )
}

# Long analysis-ready assignment sidecar.
weight_long <- assignments %>%
  transmute(
    study_id,
    group,
    arm,
    consent_date,
    baseline_date,
    visit_month,
    timepoint,
    timepoint_label,
    target_date,
    visit_date = log_date,
    weight_kg,
    bmi_kg_m2,
    month_since_baseline,
    weight_assignment_pass = assignment_pass,
    weight_days_from_target = days_from_target,
    weight_abs_days_from_target = abs_days_from_target,
    daily_row_id,
    selected_fitabase_row_id,
    log_id,
    is_manual_report,
    imputed_from_weight_and_bmi,
    n_logs_same_day,
    n_distinct_weights_same_day,
    daily_selection_rule,
    same_day_weight_range_kg,
    same_day_log_values,
    same_day_large_discrepancy
  ) %>%
  left_join(
    assignments %>%
      filter(timepoint == "baseline") %>%
      transmute(
        study_id,
        baseline_weight_kg = weight_kg,
        baseline_bmi_kg_m2 = bmi_kg_m2
      ),
    by = "study_id"
  ) %>%
  mutate(
    pct_weight_change = 100 * (weight_kg - baseline_weight_kg) / baseline_weight_kg
  )

# Compact wide sidecar.
weight_wide <- weight_long %>%
  select(
    study_id,
    group,
    arm,
    consent_date,
    timepoint,
    visit_date,
    weight_kg,
    bmi_kg_m2
  ) %>%
  pivot_wider(
    names_from = timepoint,
    values_from = c(
      visit_date,
      weight_kg,
      bmi_kg_m2
    ),
    names_glue = "{timepoint}_{.value}"
  ) %>%
  arrange(study_id)

# Wide sidecar for analysis checks.
weight_wide_analysis <- weight_long %>%
  select(
    study_id,
    group,
    arm,
    consent_date,
    timepoint,
    visit_date,
    weight_kg,
    bmi_kg_m2,
    pct_weight_change,
    weight_days_from_target
  ) %>%
  pivot_wider(
    names_from = timepoint,
    values_from = c(
      visit_date,
      weight_kg,
      bmi_kg_m2,
      pct_weight_change,
      weight_days_from_target
    ),
    names_glue = "{timepoint}_{.value}"
  ) %>%
  arrange(study_id)

# Wide sidecar with audit fields.
weight_wide_audit <- weight_long %>%
  select(
    study_id,
    group,
    arm,
    consent_date,
    timepoint,
    target_date,
    visit_date,
    weight_kg,
    bmi_kg_m2,
    pct_weight_change,
    weight_assignment_pass,
    weight_days_from_target,
    weight_abs_days_from_target,
    selected_fitabase_row_id,
    log_id,
    is_manual_report,
    imputed_from_weight_and_bmi,
    n_logs_same_day,
    n_distinct_weights_same_day,
    daily_selection_rule,
    same_day_weight_range_kg,
    same_day_large_discrepancy
  ) %>%
  pivot_wider(
    names_from = timepoint,
    values_from = c(
      target_date,
      visit_date,
      weight_kg,
      bmi_kg_m2,
      pct_weight_change,
      weight_assignment_pass,
      weight_days_from_target,
      weight_abs_days_from_target,
      selected_fitabase_row_id,
      log_id,
      is_manual_report,
      imputed_from_weight_and_bmi,
      n_logs_same_day,
      n_distinct_weights_same_day,
      daily_selection_rule,
      same_day_weight_range_kg,
      same_day_large_discrepancy
    ),
    names_glue = "{timepoint}_{.value}"
  ) %>%
  arrange(study_id)

# Participant-level completion status.
participant_status <- assignments %>%
  group_by(study_id, group, arm, consent_date) %>%
  summarise(
    n_required_timepoints = n(),
    n_daily_logs_on_or_after_consent = sum(daily_logs$study_id == first(study_id) & daily_logs$log_date >= first(consent_date)),
    has_baseline = any(timepoint == "baseline" & !is.na(weight_kg)),
    n_assigned_pass_1 = sum(!is.na(weight_kg) & assignment_pass == 1L),
    n_assigned_pass_2 = sum(!is.na(weight_kg) & assignment_pass == 2L),
    n_assigned_total = sum(!is.na(weight_kg)),
    n_missing_after_pass_2 = sum(is.na(weight_kg)),
    complete_after_pass_1 = n_assigned_pass_1 == 4L & has_baseline,
    complete_after_pass_2 = n_assigned_total == n_required_timepoints,
    completed_by_fallback = complete_after_pass_2 & !complete_after_pass_1,
    .groups = "drop"
  ) %>%
  arrange(study_id)

# Flowchart counts.
flow_counts <- tibble(
  metric = c(
    "participants_in_registry",
    "participants_with_any_daily_log_on_or_after_consent",
    "complete_after_pass_1",
    "additional_completed_by_pass_2",
    "complete_after_pass_2_total",
    "not_complete_after_pass_2"
  ),
  n = c(
    nrow(participant_status),
    sum(participant_status$n_daily_logs_on_or_after_consent > 0),
    sum(participant_status$complete_after_pass_1),
    sum(participant_status$completed_by_fallback),
    sum(participant_status$complete_after_pass_2),
    sum(!participant_status$complete_after_pass_2)
  ),
  first_pass_window_days = 15L,
  required_timepoints = nrow(visit_spec())
)

# Timepoint-level assignment counts.
timepoint_counts <- assignments %>%
  mutate(
    assignment_source = case_when(
      assignment_pass == 0L ~ "Baseline: first weight on/after consent",
      assignment_pass == 1L ~ "Pass 1: within +/-15 days",
      assignment_pass == 2L ~ "Pass 2: ordered closest unassigned",
      TRUE ~ "Unassigned"
    )
  ) %>%
  count(
    visit_month,
    month_since_baseline,
    timepoint,
    timepoint_label,
    assignment_source,
    name = "n_participants"
  ) %>%
  arrange(visit_month, assignment_source)

# Unused daily weights.
assigned_daily_ids <- stats::na.omit(assignments$daily_row_id)
unassigned_daily_logs <- daily_logs %>%
  filter(!(daily_row_id %in% assigned_daily_ids)) %>%
  arrange(study_id, log_date, log_datetime, daily_row_id)

# Pull a single flow count.
get_flow_n <- function(metric_name) {
  value <- flow_counts %>%
    filter(metric == metric_name) %>%
    pull(n)

  stopifnot(length(value) == 1)
  as.integer(value)
}

total_n <- get_flow_n("participants_in_registry")
daily_log_n <- get_flow_n("participants_with_any_daily_log_on_or_after_consent")
pass_1_complete_n <- get_flow_n("complete_after_pass_1")
additional_pass_2_n <- get_flow_n("additional_completed_by_pass_2")
pass_2_complete_n <- get_flow_n("complete_after_pass_2_total")
incomplete_n <- get_flow_n("not_complete_after_pass_2")
needs_pass_2_n <- total_n - pass_1_complete_n

# Flowchart node labels.
flowchart_nodes <- tibble(
  id = c("total", "pass1", "needs2", "pass2", "complete2", "incomplete"),
  x = c(0, -2.35, 2.35, 2.35, 0.95, 3.75),
  y = c(4.2, 2.65, 2.65, 1.35, 0.05, 0.05),
  w = c(3.2, 2.85, 2.85, 3.2, 2.7, 2.7),
  h = c(0.82, 0.82, 0.82, 0.78, 0.92, 0.92),
  fill = c("#EEF2F7", "#E8F4EA", "#FFF4DF", "#FFF4DF", "#E8F4EA", "#F7E8E8"),
  label = c(
    paste0("Study participants in registry\nn = ", total_n, "\ndaily logs after consent: ", daily_log_n),
    paste0("Complete after pass 1\nall 5 timepoints assigned\nn = ", pass_1_complete_n),
    paste0("Needed ordered fallback\nnot complete after pass 1\nn = ", needs_pass_2_n),
    "Pass 2 fallback\nclosest remaining unassigned daily weight\nwhile preserving visit order",
    paste0("Additional complete after pass 2\nn = ", additional_pass_2_n, "\ncomplete total: ", pass_2_complete_n),
    paste0("Still not complete\nafter pass 2\nn = ", incomplete_n)
  )
)

# Flowchart arrows.
flowchart_edges <- tibble(
  x = c(0, 0, 2.35, 2.35, 2.35),
  y = c(3.78, 3.78, 2.24, 0.96, 0.96),
  xend = c(-2.35, 2.35, 2.35, 0.95, 3.75),
  yend = c(3.06, 3.06, 1.74, 0.52, 0.52)
)

# Render assignment flowchart.
flowchart <- ggplot() +
  geom_segment(
    data = flowchart_edges,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.45,
    color = "#59616B",
    arrow = arrow(length = unit(0.11, "in"), type = "closed")
  ) +
  geom_rect(
    data = flowchart_nodes,
    aes(
      xmin = x - w / 2,
      xmax = x + w / 2,
      ymin = y - h / 2,
      ymax = y + h / 2,
      fill = fill
    ),
    color = "#2F3842",
    linewidth = 0.45
  ) +
  geom_text(
    data = flowchart_nodes,
    aes(x = x, y = y, label = label),
    family = "Arial",
    size = 3.3,
    lineheight = 0.92,
    color = "#18212B"
  ) +
  annotate(
    "text",
    x = 0,
    y = -0.92,
    label = "Pass 1 uses the closest daily weight within +/-15 days of each target date. Pass 2 only uses unassigned daily weights and cannot break chronological visit order.",
    size = 3.05,
    family = "Arial",
    color = "#4D5560"
  ) +
  scale_fill_identity() +
  coord_cartesian(xlim = c(-4.4, 5.15), ylim = c(-1.25, 4.85), expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

# Write sidecar CSVs.
write_csv(weight_wide, file.path(sidecar_dir, "fitabase_assigned_weight_wide.csv"), na = "")
write_csv(weight_wide_analysis, file.path(sidecar_dir, "fitabase_assigned_weight_wide_analysis.csv"), na = "")
write_csv(weight_wide_audit, file.path(sidecar_dir, "fitabase_assigned_weight_wide_audit.csv"), na = "")
write_csv(weight_long, file.path(sidecar_dir, "fitabase_assigned_weight_long.csv"), na = "")
write_csv(assignments, file.path(sidecar_dir, "fitabase_assigned_weight_detail.csv"), na = "")
write_csv(participant_status, file.path(sidecar_dir, "fitabase_assigned_participant_status.csv"), na = "")
write_csv(flow_counts, file.path(sidecar_dir, "fitabase_assigned_flow_counts.csv"), na = "")
write_csv(timepoint_counts, file.path(sidecar_dir, "fitabase_assigned_timepoint_counts.csv"), na = "")
write_csv(unassigned_daily_logs, file.path(sidecar_dir, "fitabase_assigned_unassigned_daily_weights.csv"), na = "")
write_csv(duplicate_groups, file.path(sidecar_dir, "fitabase_exact_duplicate_weight_rows.csv"), na = "")
write_csv(
  daily_selection_audit %>% filter(n_logs_same_day > 1),
  file.path(sidecar_dir, "fitabase_same_day_daily_selection_audit.csv"),
  na = ""
)

# Save flowchart PDF.
ggsave(
  file.path(sidecar_dir, "figure_fitabase_assigned_weight_flowchart.pdf"),
  flowchart,
  width = 8.6,
  height = 5.2,
  device = cairo_pdf,
  bg = "white"
)

# Save flowchart PNG.
if (requireNamespace("ragg", quietly = TRUE)) {
  ggsave(
    file.path(sidecar_dir, "figure_fitabase_assigned_weight_flowchart.png"),
    flowchart,
    width = 8.6,
    height = 5.2,
    dpi = 450,
    device = ragg::agg_png,
    bg = "white"
  )
} else {
  ggsave(
    file.path(sidecar_dir, "figure_fitabase_assigned_weight_flowchart.png"),
    flowchart,
    width = 8.6,
    height = 5.2,
    dpi = 450,
    bg = "white"
  )
}

message("Step 01 complete: Fitabase assignment sidecar written to ", sidecar_dir)
message("Complete after pass 2: ", sum(participant_status$complete_after_pass_2), " / ", nrow(participant_status))
