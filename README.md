# GLP-1 Cessation Support Study Analytic Pipeline

This repository contains the R code used for the manuscript **"Piloting strategies to curtail weight gain after GLP-1 agonist therapy cessation"**.

The study was a pilot randomized trial testing strategies to reduce early weight gain after GLP-1 receptor agonist cessation. Participants were randomized to usual care, Noom, or medically tailored meals. The first author is **Kelseanna Hollis-Hansen, PhD, MPH**.

The pipeline prepares the analytic datasets, fits the study models, creates the manuscript tables and figures, and runs diagnostic and quality-control checks.

## Data Access Required

Study data are not included in this repository.

Running the code requires protected participant-level data files and scoring documents. See [DATA_ACCESS_README.md](DATA_ACCESS_README.md) before running the pipeline.

## Run

After the required data files are in place, run the full pipeline from the repository root:

```bash
Rscript scripts/run_all.R
```

`scripts/run_all.R` runs each numbered script in order and stops if any step fails.

## Software Requirements

The pipeline was last verified with R 4.5.1. Required R packages are:

`broom`, `dplyr`, `emmeans`, `flextable`, `ggplot2`, `gridExtra`, `kableExtra`, `lme4`, `lmerTest`, `lmtest`, `lubridate`, `officer`, `patchwork`, `pdftools`, `readr`, `readxl`, `sandwich`, `stringr`, `tidyr`, and `tinytex`.

`webshot2`, `magick`, and `ragg` are used when available for image export.

## Run Order

`scripts/run_all.R` runs these scripts in order:

0. `scripts/00_make_fitabase_with_manual_imputations.R`
   - adds PI-confirmed scale-photo weights from `Weight and BMI.xlsx` to `weightLogInfo_merged.csv`
   - creates `output/weight_log_assignment/fitabase_with_imputations.csv` and an audit file

1. `scripts/01_make_fitabase_assigned_weight_dataset.R`
   - reduces same-day Fitabase weights to one daily value
   - uses the first weight on or after consent as baseline
   - assigns M1-M4 to 30, 60, 90, and 120 days from baseline using a +/-15-day first pass and an ordered closest-unassigned fallback pass
   - creates Fitabase wide, long, audit, and flowchart files in `output/weight_log_assignment/`

2. `scripts/02_build_analysis_data.R`
   - reads the questionnaire workbook, legacy weight workbook, Fitabase weight assignments, and pre-cessation GLP-1 weight-loss workbook
   - scores PDQ, PHQ, Mini-EAT, TAPQ, and TSQM-II
   - creates analysis datasets in `output/data/`
   - creates Step-02 quality-control checks in `output/qa/`

3. `scripts/03_make_tables.R`
   - fits the primary exploratory models
   - runs the longitudinal weight-change sensitivity model adjusted for weight loss while on GLP-1
   - creates model-estimate CSVs in `output/data/`
   - creates manuscript DOCX tables in `output/tables/` and supporting files in `output/tables/supporting files/`
   - creates model diagnostics in `output/diagnostics/`

4. `scripts/04_qa_transformations.R`
   - audits raw-to-analysis transformations
   - checks row counts, IDs, reshaping, pre-GLP-1 weight-loss joins, baseline joins, and percent-weight-change calculations
   - creates a dataset overview CSV and a quality-control report in `output/qa/`

5. `scripts/05_qa_score_checks.R`
   - audits questionnaire scoring against the local scoring documentation
   - independently recalculates PDQ/PHQ, Mini-EAT, TAPQ, and TSQM-II
   - creates a score quality-control report and CSV checks in `output/qa/`

6. `scripts/06_make_pub_plots.R`
   - creates the manuscript figure files
   - calls `08_make_weight_observed_trajectory_plot.R` for Figure 1
   - calls `09_make_compact_model_contrasts_forest.R` for Figure 2
   - removes deprecated figure filenames from earlier drafts

7. `scripts/07_make_weight_data_summary_table.R`
   - creates observed weight-data summary tables from `output/data/weight_long.csv`
   - creates the formal observed weight-data DOCX table and supporting CSV/PNG files
   - creates denominator and error-bar quality-control checks in `output/qa/`

Support scripts:

- `scripts/08_make_weight_observed_trajectory_plot.R` and `scripts/09_make_compact_model_contrasts_forest.R` are called by Step 06.

## Output Files

The pipeline creates files under `output/`. This folder is excluded from Git because it contains files produced from protected study data.

Main output folders:

- `output/data/`: analysis datasets and model outputs used in tables and figures
- `output/tables/`: main manuscript and supplement DOCX tables
- `output/tables/supporting files/`: table CSV exports, model-detail PDFs, and table PNGs
- `output/pub plots/`: manuscript figure files
- `output/diagnostics/`: model diagnostic PDFs
- `output/qa/`: quality-control reports and CSV checks
- `output/weight_log_assignment/`: Fitabase assignment files, audit files, and flowchart used to build the longitudinal weight dataset

Main manuscript outputs:

- `output/tables/Table 1 - Baseline Characteristics.docx`
- `output/tables/Table 2 - Model-Based Outcomes.docx`
- `output/tables/SI Table 1 - Weight Sensitivity Analysis.docx`
- `output/tables/SI Table 2 - Observed Weight by Month.docx`
- `output/pub plots/figure_1_weight_trajectories.pdf`
- `output/pub plots/figure_1_weight_trajectories.png`
- `output/pub plots/figure_2_compact_model_contrasts_forest.pdf`
- `output/pub plots/figure_2_compact_model_contrasts_forest.png`

## Analysis Notes

- Mini-EAT uses the raw dictionary field meanings: `me4/fu_me4 -> fish`, `me5/fu_me5 -> whole grains`, and `me6/fu_me6 -> refined grains`.
- The pipeline creates `output/data/weight_long.csv` from the Fitabase assignment files and keeps the older workbook-based longitudinal weight file as `output/data/weight_long_workbook_legacy.csv`.
- The pipeline creates `output/data/glp1_weight_loss.csv` from `inputs/raw/Weight loss while on GLP1.xlsx` and joins the GLP-1 weight-loss fields into both `baseline_analysis.csv` and `weight_long.csv`.
- `glp1_weight_loss.csv` preserves source percent loss, recalculated percent loss from pre/post pounds, and the final percent-loss field used by the sensitivity model.
- The observed weight trajectory figure and formal observed-weight table both use participants with baseline weight plus at least one post-baseline weight.
