# Reaction Time Survey

An R Shiny application for exploring reaction-time survey responses from
University of Illinois students. The application provides:

- reaction-time histograms filtered by class and correctly stacked by fatigue
  or stress level;
- automatic plots and exploratory association tests for every meaningful
  survey variable;
- class-level descriptive statistics;
- previews and downloads of the cleaned dataset and generated plots.

The analyses are descriptive and unadjusted. They identify patterns in this
sample but do not establish causal effects.

## Requirements

- R 4.3 or newer
- `shiny`
- `ggplot2`

The project uses `renv` to record exact package versions. From the project
directory, restore the environment with:

```r
install.packages("renv")
renv::restore()
```

The project keeps `renv`'s package sandbox disabled so it can run in restricted
Windows environments; package versions are still isolated in `renv/library`.

If no lockfile is available yet, install the two runtime packages directly:

```r
install.packages(c("shiny", "ggplot2"))
```

## Run the application

Open `Reaction Time.Rproj` in RStudio and click **Run App**, or run:

```r
shiny::runApp()
```

## Project structure

- `app.R`: Shiny UI, server, plotting, downloads, and exploratory statistics.
- `R/data_processing.R`: the single canonical data-cleaning implementation.
- `Data/Reaction Time Survey.csv`: original survey responses.
- `Data/Dataprocess.R`: compatibility entry point for the former standalone
  preprocessing workflow.
- `tests/test_project.R`: data and plotting regression checks.

## Data cleaning

The cleaning pipeline:

1. assigns short, stable names to the 24 original questionnaire columns;
2. parses numeric answers that contain units or common free-text forms;
3. retains reaction times from 101 through 700 ms;
4. converts ordered survey responses to ordered factors;
5. preserves the original device description while deriving separate device
   type and operating-system fields;
6. records the excluded rows in the `cleaning_summary` attribute.

The original CSV remains unchanged.

## Tests

After restoring dependencies, run:

```r
source("tests/test_project.R")
```
