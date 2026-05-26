# GLP-1 Cessation Support Study Analytic Pipeline

This repository contains the analytic code for the manuscript **"Piloting strategies to curtail weight gain after GLP-1 agonist therapy cessation"**.

The study is a pilot randomized trial evaluating strategies to mitigate early weight gain after GLP-1 receptor agonist cessation. Participants were randomized to usual care, a digital behavioral support program, or medically tailored meals. The first author is **Kelseanna Hollis-Hansen, PhD, MPH**.

This code rebuilds the analysis datasets, manuscript tables, publication-style figures, model diagnostics, and QA reports used for the manuscript.

## Data Access Required

This repository does **not** include participant-level data, raw study-source files, scoring references, or generated outputs derived from protected inputs.

Those files are required to run the pipeline. Approved study-team users must place the protected files in the expected local paths before running anything below. See [DATA_ACCESS_README.md](DATA_ACCESS_README.md) for the required file list and data-access note.

## Run

Run the full pipeline from the repository root:

```bash
Rscript scripts/run_all.R
```

`scripts/run_all.R` executes every step in order and stops immediately if any step fails.

## Software Requirements

The pipeline was last verified with R 4.5.1. Required R packages are:

`broom`, `dplyr`, `emmeans`, `flextable`, `ggplot2`, `gridExtra`, `kableExtra`, `lme4`, `lmerTest`, `lmtest`, `lubridate`, `officer`, `patchwork`, `pdftools`, `readr`, `readxl`, `sandwich`, `stringr`, `tidyr`, and `tinytex`.

`webshot2`, `magick`, and `ragg` are used when available for image export.

## Expected Input Locations

Raw data:

- `inputs/raw/Weight and BMI.xlsx`
- `inputs/raw/Weight loss while on GLP1.xlsx`
- `inputs/raw/GLP1 Cessation Support Study Questionnaire Data.xlsx`
- `weightLogInfo_merged.csv`

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

## Generated Outputs

Generated files are written under `output/`, which is excluded from Git because outputs are derived from protected inputs.

Main output folders:

- `output/data/`: cleaned datasets and model outputs used by downstream tables/figures
- `output/tables/`: main manuscript and supplement DOCX tables
- `output/tables/supporting files/`: table CSV exports, model-detail PDFs, and table PNGs
- `output/pub plots/`: publication-style manuscript figures
- `output/diagnostics/`: model diagnostic PDFs
- `output/qa/`: QA reports and companion CSV checks
- `output/weight_log_assignment/`: Fitabase assignment sidecar, audit files, and flowchart used to build the main longitudinal weight dataset

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
- The main pipeline writes `output/data/weight_long.csv` from the Fitabase-derived assignment sidecar; it writes the older workbook-derived longitudinal weight file separately as `output/data/weight_long_workbook_legacy.csv`.
- The pipeline writes `output/data/glp1_weight_loss.csv` from `inputs/raw/Weight loss while on GLP1.xlsx` and joins the GLP-1 weight-loss fields into both `baseline_analysis.csv` and `weight_long.csv`.
- `glp1_weight_loss.csv` preserves source percent loss, recalculated percent loss from pre/post pounds, and the final percent-loss field used by the sensitivity model.
- The observed weight trajectory figure and formal observed-weight table both use participants with baseline weight plus at least one post-baseline weight.
