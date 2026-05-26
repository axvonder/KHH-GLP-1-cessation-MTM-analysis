# Analytic Pipeline

This folder contains the reproducible analytic pipeline for the manuscript tables, publication-style figures, model diagnostics, and QA reports.

## Run

Most users should run the full pipeline rather than individual scripts:

```bash
Rscript scripts/run_all.R
```

`run_all.R` executes the full pipeline in order and stops immediately if any step fails.

Generated files are written under `output/`, except protected inputs, which must be supplied separately by approved study-team users. See `../DATA_ACCESS_README.md`.

## Requirements

The pipeline was last verified with R 4.5.1. Required R packages are:

`broom`, `dplyr`, `emmeans`, `flextable`, `ggplot2`, `gridExtra`, `kableExtra`, `lme4`, `lmerTest`, `lmtest`, `lubridate`, `officer`, `patchwork`, `pdftools`, `readr`, `readxl`, `sandwich`, `stringr`, `tidyr`, and `tinytex`.

`webshot2`, `magick`, and `ragg` are used when available for image export.

## Inputs

No data files are distributed with this repository. The paths below document the local files required to reproduce the analysis after appropriate data access has been granted.

Raw data:

- `inputs/raw/Weight and BMI.xlsx`
- `inputs/raw/Weight loss while on GLP1.xlsx`
- `inputs/raw/GLP1 Cessation Support Study Questionnaire Data.xlsx`
- `../weightLogInfo_merged.csv`

Reference files used by QA/scoring:

- `inputs/reference/MiniEAT user data to model mapping 11-2023.csv`
- `inputs/reference/NORC_DataDictionary_2026-03-19.csv`
- `inputs/reference/Mini-EAT scoring algorithm 11-2023 PDF.pdf`
- `inputs/reference/TAPQ - Treatment Adherence Perception Questionnaire .pdf`
- `inputs/reference/TSQM-9 scoring.pdf`
- `inputs/reference/sample-tsqm-v-ii_united-states_english.pdf`

## Run Order

The numbered scripts are ordered for a human reader. `scripts/run_all.R` is the source of truth and runs these steps:

0. `scripts/00_make_fitabase_with_manual_imputations.R`
   - appends PI-confirmed manual scale-photo entries from `Weight and BMI.xlsx` to `weightLogInfo_merged.csv`
   - writes `output/weight_log_assignment/fitabase_with_imputations.csv` and an audit file

1. `scripts/01_make_fitabase_assigned_weight_dataset.R`
   - collapses same-day Fitabase weights to one daily value
   - uses the first weight on/after consent as baseline
   - assigns M1-M4 to 30/60/90/120 days from baseline using a +/-15-day first pass plus an ordered closest-unassigned fallback pass
   - writes Fitabase-derived wide/long/audit files and a flowchart to `output/weight_log_assignment/`

2. `scripts/02_build_analysis_data.R`
   - reads the raw questionnaire workbook, legacy weight workbook, Fitabase-derived weight assignments, and pre-GLP-1 to GLP-1 cessation weight-loss workbook
   - scores PDQ, PHQ, Mini-EAT, TAPQ, and TSQM-II
   - writes clean analysis datasets to `output/data/`
   - writes basic Step-02 QA checks to `output/qa/`

3. `scripts/03_make_tables.R`
   - fits the primary exploratory models
   - runs the longitudinal weight-change sensitivity model adjusted for weight loss while on GLP-1
   - writes model-estimate CSVs to `output/data/`
   - writes manuscript DOCX tables to `output/tables/` and supporting CSV/PDF table files to `output/tables/supporting files/`
   - writes model diagnostics to `output/diagnostics/`

4. `scripts/04_qa_transformations.R`
   - audits raw-to-analysis transformations
   - verifies row counts, IDs, reshapes, pre-GLP-1 weight-loss joins, baseline joins, and percent-weight-change calculations
   - writes a dataset overview CSV for repository QA
   - writes a PDF report and companion CSVs to `output/qa/`

5. `scripts/05_qa_score_checks.R`
   - audits score coding against local source documentation
   - independently recalculates PDQ/PHQ, Mini-EAT, TAPQ, and TSQM-II
   - writes a PDF report and companion CSVs to `output/qa/`

6. `scripts/06_make_pub_plots.R`
   - rebuilds the manuscript-style figure set
   - delegates Figure 1 to `08_make_weight_observed_trajectory_plot.R`
   - delegates Figure 2 to `09_make_compact_model_contrasts_forest.R`
   - removes deprecated publication-figure filenames from `output/pub plots/`

7. `scripts/07_make_weight_data_summary_table.R`
   - rebuilds observed weight-data summary tables directly from `output/data/weight_long.csv`
   - writes the formal observed weight-data DOCX table to `output/tables/` and supporting CSV/PNG files to `output/tables/supporting files/`
   - writes weight-figure denominator and error-bar QA checks to `output/qa/`

Support scripts:

- `scripts/08_make_weight_observed_trajectory_plot.R` and `scripts/09_make_compact_model_contrasts_forest.R` are called by Step 06.

## Output Folders

- `output/data/`: cleaned datasets and model outputs used by downstream tables/figures
- `output/tables/`: main manuscript and supplement DOCX tables
- `output/tables/supporting files/`: table CSV exports, model-detail PDFs, and table PNGs
- `output/pub plots/`: publication-style manuscript figures
- `output/diagnostics/`: model diagnostic PDFs
- `output/qa/`: QA reports and companion CSV checks
- `output/weight_log_assignment/`: Fitabase assignment sidecar, audit files, and flowchart used to build the main longitudinal weight dataset

## Main Outputs

Tables:

- `output/tables/Table 1 - Baseline Characteristics.docx`
- `output/tables/Table 2 - Model-Based Outcomes.docx`
- `output/tables/SI Table 1 - Weight Sensitivity Analysis.docx`
- `output/tables/SI Table 2 - Observed Weight by Month.docx`
- supporting table CSVs, model-detail PDFs, and PNGs are in `output/tables/supporting files/`

Publication-style figures:

- `output/pub plots/figure_1_weight_trajectories.pdf`
- `output/pub plots/figure_1_weight_trajectories.png`
- `output/pub plots/figure_2_compact_model_contrasts_forest.pdf`
- `output/pub plots/figure_2_compact_model_contrasts_forest.png`

QA reports:

- `output/qa/qa_transformation_audit.pdf`
- `output/qa/qa_score_audit.pdf`

## Notes

- Mini-EAT uses the raw dictionary field meanings: `me4/fu_me4 -> fish`, `me5/fu_me5 -> whole grains`, and `me6/fu_me6 -> refined grains`.
- The main pipeline writes `output/data/weight_long.csv` from the Fitabase-derived assignment sidecar; it writes the older workbook-derived longitudinal weight file separately as `output/data/weight_long_workbook_legacy.csv`.
- The pipeline writes `output/data/glp1_weight_loss.csv` from `inputs/raw/Weight loss while on GLP1.xlsx` and joins the GLP-1 weight-loss fields into both `baseline_analysis.csv` and `weight_long.csv`.
- `glp1_weight_loss.csv` preserves source percent loss, recalculated percent loss from pre/post pounds, and the final percent-loss field used by the sensitivity model.
- The formal GLP-1 weight-loss sensitivity analysis compares the primary weight model with a complete-case primary model and a complete-case model adjusted for percent weight loss while on GLP-1, reporting both model-based Month 4 total change and monthly slope.
- To append PI-confirmed manual scale-photo entries to the merged Fitabase export, run `Rscript scripts/00_make_fitabase_with_manual_imputations.R`; it writes `output/weight_log_assignment/fitabase_with_imputations.csv` and an audit file.
- To build the current Fitabase-derived baseline/M1-M4 sidecar, run `Rscript scripts/01_make_fitabase_assigned_weight_dataset.R`; it uses the first weight on/after consent as baseline, assigns M1-M4 to 30/60/90/120 days from baseline, uses a +/-15-day first pass plus an ordered closest-unassigned fallback pass, and writes wide/long assignment files plus `figure_fitabase_assigned_weight_flowchart.*` to `output/weight_log_assignment/`.
- The observed weight trajectory figure uses participants with baseline weight plus at least one post-baseline weight.
- The formal observed-weight table uses participants with baseline weight plus at least one post-baseline weight as arm denominators.
- The QA comparison CSVs verify that the observed-weight figure and formal observed-weight table use matching summary values.
- The root `.gitignore` keeps raw data, generated outputs, Office lock files, `.DS_Store`, and `rendered_*` QA scratch folders out of Git by default.
