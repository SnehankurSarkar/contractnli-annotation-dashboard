# ContractNLI Annotation Dashboard 

This version uses only the three current CSV files:

```text
data/ToBeAnnotated.csv
data/NDAs.csv
data/labels.csv
```

## What changed in v4

- Fixed progress counting: only rows with non-empty `Primary_Pattern` are counted as annotated.
- Removed the editable annotator field.
- Removed the Flag Review button.
- Added a separate `Next →` button beside `Save & Next →`.
- Redesigned the Guide page into readable cards/sections.
- Saves directly into `data/ToBeAnnotated.csv`.

## Local setup

Open this folder in RStudio and run:

```r
install.packages(c(
  "shiny", "bslib", "readr", "dplyr", "stringr",
  "htmltools"
))

shiny::runApp()
```

## Save behaviour

When you click Save, the app updates:

```text
data/ToBeAnnotated.csv
```
