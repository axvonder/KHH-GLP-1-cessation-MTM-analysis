# GLP-1 Cessation Support Study Analytic Pipeline

This is the R analysis code for the manuscript **"Piloting strategies to curtail weight gain after GLP-1 agonist therapy cessation"**.

The study was a pilot randomized trial evaluating usual care, Noom, and medically tailored meals after GLP-1 receptor agonist cessation. The first author is **Kelseanna Hollis-Hansen, PhD, MPH**.

The code builds analytic datasets, estimates models, produces manuscript tables and figures, and runs QA checks.

## Data Access Required

Study data and scoring documents are protected and not included here. They are required to run the pipeline. See [DATA_ACCESS_README.md](DATA_ACCESS_README.md) before running the code.

## Run

From the project root:

```bash
Rscript scripts/run_all.R
```

`scripts/run_all.R` runs the numbered scripts in order and stops if any step fails.

## Software

The pipeline was last verified with R 4.5.1. Required R packages are:

`broom`, `dplyr`, `emmeans`, `flextable`, `ggplot2`, `gridExtra`, `kableExtra`, `lme4`, `lmerTest`, `lmtest`, `lubridate`, `officer`, `patchwork`, `pdftools`, `readr`, `readxl`, `sandwich`, `stringr`, `tidyr`, and `tinytex`.

`webshot2`, `magick`, and `ragg` are used when available for image export.

## Scripts

- `scripts/00_make_fitabase_with_manual_imputations.R`: add weights from `Weight and BMI.xlsx` to `weightLogInfo_merged.csv`
- `scripts/01_make_fitabase_assigned_weight_dataset.R`: assign Fitabase weights to baseline and monthly visits
- `scripts/02_build_analysis_data.R`: build analysis datasets and questionnaire scores
- `scripts/03_make_tables.R`: fit models and make manuscript tables
- `scripts/04_qa_transformations.R`: check raw-to-analysis transformations
- `scripts/05_qa_score_checks.R`: check questionnaire scoring
- `scripts/06_make_pub_plots.R`: make manuscript figures
- `scripts/07_make_weight_data_summary_table.R`: make observed weight-data summary tables and QA checks

Support scripts:

- `scripts/08_make_weight_observed_trajectory_plot.R`: Figure 1
- `scripts/09_make_compact_model_contrasts_forest.R`: Figure 2

## Outputs

The pipeline writes files under `output/`, which is excluded from Git.

Main folders:

- `output/data/`: analysis datasets and model outputs
- `output/tables/`: manuscript and supplement DOCX tables
- `output/tables/supporting files/`: table CSVs, model-detail PDFs, and table PNGs
- `output/pub plots/`: manuscript figure files
- `output/diagnostics/`: model diagnostic PDFs
- `output/qa/`: QA reports and CSV checks
- `output/weight_log_assignment/`: Fitabase assignment and audit files

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
- `output/data/weight_long.csv` comes from the Fitabase assignment files. The older workbook-based file is kept as `output/data/weight_long_workbook_legacy.csv`.
- `output/data/glp1_weight_loss.csv` preserves source percent loss, recalculated percent loss from pre/post pounds, and the final percent-loss field used by the sensitivity model.
- The observed weight trajectory figure and observed-weight table both use participants with baseline weight plus at least one post-baseline weight.
