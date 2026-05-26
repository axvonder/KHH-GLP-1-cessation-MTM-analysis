# Scripts

Run the pipeline from the project root:

```bash
Rscript scripts/run_all.R
```

## Main Run Order

- `00_make_fitabase_with_manual_imputations.R`
- `01_make_fitabase_assigned_weight_dataset.R`
- `02_build_analysis_data.R`
- `03_make_tables.R`
- `04_qa_transformations.R`
- `05_qa_score_checks.R`
- `06_make_pub_plots.R`
- `07_make_weight_data_summary_table.R`

## Support Scripts

- `08_make_weight_observed_trajectory_plot.R`: called by Step 06.
- `09_make_compact_model_contrasts_forest.R`: called by Step 06.
