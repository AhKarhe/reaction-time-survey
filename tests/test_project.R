source(file.path("R", "data_processing.R"))

survey <- read_and_clean_survey()
cleaning <- attr(survey, "cleaning_summary")

stopifnot(
  cleaning$raw_rows == 144L,
  cleaning$retained_rows == 140L,
  nrow(cleaning$excluded_rows) == 4L,
  ncol(survey) == 26L,
  min(survey$Reaction.time) == 165,
  max(survey$Reaction.time) == 475,
  all(names(survey_variable_labels) %in% names(survey)),
  !anyNA(survey[c("Reaction.time", "Class", "Fatigue.level", "Stress.level")]),
  identical(parse_survey_number(c("7:30", "7h4min", "1~3", "Thank 7")),
            c(7.5, 7.1, 2, 7)),
  identical(reaction_histogram_breaks(c(165, 174, 191)), c(160, 170, 180, 190, 200)),
  all(c("Android", "ChromeOS", "IOS", "macOS", "Windows") %in%
        levels(survey$Operating.system)),
  all(c("Smartphone", "Tablet") %in% levels(survey$Device.type))
)

class_counts <- table(survey$Class)
stopifnot(
  unname(class_counts["Sophomore"]) == 49L,
  unname(class_counts["Junior"]) == 65L,
  unname(class_counts["Senior"]) == 16L,
  unname(class_counts["Graduate"]) == 10L
)

if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Install project dependencies before running the full test suite.")
}

app_environment <- new.env(parent = globalenv())
sys.source("app.R", envir = app_environment)
stopifnot(inherits(app_environment$app, "shiny.appobj"))

ggplot_file <- tempfile(fileext = ".png")
ggplot2::ggsave(
  ggplot_file,
  app_environment$make_reaction_plot(survey, "Fatigue.level"),
  width = 8,
  height = 5,
  dpi = 100
)
stopifnot(file.exists(ggplot_file), file.info(ggplot_file)$size > 1000)

base_file <- tempfile(fileext = ".png")
grDevices::png(base_file, width = 800, height = 500)
app_environment$draw_base_reaction_plot(survey, "Stress.level")
grDevices::dev.off()
stopifnot(file.exists(base_file), file.info(base_file)$size > 1000)

unlink(c(ggplot_file, base_file))
message("All project checks passed.")
