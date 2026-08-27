# Compatibility entry point for the original standalone preprocessing script.
# The canonical cleaning logic now lives in R/data_processing.R and is shared
# with the Shiny application.

project_root <- if (file.exists(file.path("R", "data_processing.R"))) "." else ".."
source(file.path(project_root, "R", "data_processing.R"))

survey <- read_and_clean_survey(
  file.path(project_root, "Data", "Reaction Time Survey.csv")
)
