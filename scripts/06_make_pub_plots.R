#!/usr/bin/env Rscript

# Step 06. Build publication-style figures.
# Official manuscript figure files:
# - Figure 1: observed weight trajectories
# - Figure 2: compact model-contrast forest plot

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

  normalizePath(file.path(getwd(), "06_make_pub_plots.R"), mustWork = FALSE)
}

script_path <- resolve_script_path()
script_dir <- dirname(script_path)
pipeline_dir <- dirname(script_dir)
pub_plot_dir <- file.path(pipeline_dir, "output", "pub plots")

dir.create(pub_plot_dir, recursive = TRUE, showWarnings = FALSE)

run_step <- function(file_name) {
  script_file <- file.path(script_dir, file_name)
  stopifnot(file.exists(script_file))

  status <- system2("Rscript", shQuote(script_file))
  stopifnot(status == 0)
}

remove_deprecated_outputs <- function() {
  deprecated_stems <- c(
    "figure_1_weight_regain_publication",
    "figure_2_secondary_outcomes_publication",
    "figure_weight_trajectory_observed_only_standalone",
    "figure_compact_stacked_model_contrasts_forest"
  )

  deprecated_files <- unlist(lapply(
    deprecated_stems,
    function(stem) file.path(pub_plot_dir, paste0(stem, c(".pdf", ".png")))
  ))

  unlink(deprecated_files, force = TRUE)
}

remove_deprecated_outputs()
run_step("08_make_weight_observed_trajectory_plot.R")
run_step("09_make_compact_model_contrasts_forest.R")
remove_deprecated_outputs()

message("Step 06 complete: publication-style plots written to ", pub_plot_dir)
