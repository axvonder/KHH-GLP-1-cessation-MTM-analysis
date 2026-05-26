# Data Access

This repository intentionally does not include study data.

The analysis uses participant-level and study-source files that are protected and are not approved for public release. The public GitHub repository should contain the analytic code, documentation, and empty input-folder placeholders only.

Approved study-team users who have access to the protected files can run the pipeline by placing the required local inputs in the expected paths:

- `inputs/raw/Weight and BMI.xlsx`
- `inputs/raw/Weight loss while on GLP1.xlsx`
- `inputs/raw/GLP1 Cessation Support Study Questionnaire Data.xlsx`
- `inputs/reference/MiniEAT user data to model mapping 11-2023.csv`
- `inputs/reference/NORC_DataDictionary_2026-03-19.csv`
- `inputs/reference/Mini-EAT scoring algorithm 11-2023 PDF.pdf`
- `inputs/reference/TAPQ - Treatment Adherence Perception Questionnaire .pdf`
- `inputs/reference/TSQM-9 scoring.pdf`
- `inputs/reference/sample-tsqm-v-ii_united-states_english.pdf`
- `weightLogInfo_merged.csv` in the project root

Generated analysis outputs under `output/` are also excluded from Git because they are derived from protected inputs.

When preparing the public GitHub repository, use Git so `.gitignore` rules are applied. Do not manually upload the local working folder with protected data files included.
