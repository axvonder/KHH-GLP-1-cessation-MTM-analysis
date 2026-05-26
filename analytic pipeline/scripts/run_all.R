#!/usr/bin/env Rscript

# Run the full analytic pipeline.

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

  normalizePath(file.path(getwd(), "run_all.R"), mustWork = FALSE)
}

# Pipeline script directory.
script_path <- resolve_script_path()
script_dir <- dirname(script_path)

# Run one script and fail fast.
run_step <- function(file_name) {
  script_file <- file.path(script_dir, file_name)
  stopifnot(file.exists(script_file))

  message("Running ", file_name)
  status <- system2("Rscript", shQuote(script_file))
  if (status != 0) {
    stop("Pipeline step failed: ", file_name, call. = FALSE)
  }
}

# Source-of-truth run order.
pipeline_steps <- c(
  "00_make_fitabase_with_manual_imputations.R",
  "01_make_fitabase_assigned_weight_dataset.R",
  "02_build_analysis_data.R",
  "03_make_tables.R",
  "04_qa_transformations.R",
  "05_qa_score_checks.R",
  "06_make_pub_plots.R",
  "07_make_weight_data_summary_table.R"
)

# Execute every step.
for (step_file in pipeline_steps) {
  run_step(step_file)
}

# Completion message.
message("Pipeline complete.")
