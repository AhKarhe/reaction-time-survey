survey_column_names <- c(
  "mockID", "Reaction.time", "Class", "Age", "Avg.sleep.time",
  "last.night.sleep.time", "Awake.hours", "Fatigue.level",
  "Stress.level", "Distraction", "Noise.level", "Temp.level",
  "Game.freq", "Sport.freq", "Avg.hours.exercise",
  "Caffeine.intake", "Alcohol.intake", "Visual.acuity",
  "Primary.hand", "Use.primary.hand", "Cautious.level",
  "Input.device", "Device.description", "WiFi.stable"
)

survey_variable_labels <- c(
  Age = "Age",
  Avg.sleep.time = "Average daily sleep (hours)",
  last.night.sleep.time = "Last night's sleep (hours)",
  Awake.hours = "Hours continuously awake",
  Fatigue.level = "Fatigue level",
  Stress.level = "Stress level",
  Distraction = "Experienced distractions",
  Noise.level = "Noise level (1-10)",
  Temp.level = "Surrounding temperature",
  Game.freq = "Quick-reaction video game frequency",
  Sport.freq = "Reaction-related sport frequency",
  Avg.hours.exercise = "Exercise per week (hours)",
  Caffeine.intake = "Caffeine in the last 3 hours",
  Alcohol.intake = "Alcohol in the last 5 hours",
  Visual.acuity = "Corrected visual acuity",
  Primary.hand = "Primary hand",
  Use.primary.hand = "Used primary hand during gameplay",
  Cautious.level = "Self-reported cautiousness",
  Input.device = "Input device",
  Device.type = "Device type",
  Operating.system = "Operating system",
  WiFi.stable = "Wi-Fi stability"
)

parse_survey_number <- function(x) {
  value <- trimws(tolower(as.character(x)))
  value[value == "7:30"] <- "7.5"
  value[value == "7h4min"] <- "7.1"
  value[value == "1~3"] <- "2"
  value <- gsub("[^0-9.+-]", "", value)
  suppressWarnings(as.numeric(value))
}

extract_device_type <- function(x) {
  result <- sub("\\s+-.*$", "", x)
  result <- sub("\\s+\\(.*$", "", result)
  result[grepl("^iPad", x, ignore.case = TRUE)] <- "Tablet"
  result[grepl("^iPhone", x, ignore.case = TRUE)] <- "Smartphone"
  result
}

extract_operating_system <- function(x) {
  result <- ifelse(
    grepl(" - ", x, fixed = TRUE),
    sub("^.* - ([^)]+)\\)?$", "\\1", x),
    NA_character_
  )
  result[grepl("^Chromebook$", x, ignore.case = TRUE)] <- "ChromeOS"
  result
}

read_and_clean_survey <- function(
  path = file.path("Data", "Reaction Time Survey.csv")
) {
  if (!file.exists(path)) {
    stop("Survey data file was not found: ", path, call. = FALSE)
  }

  survey <- read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (ncol(survey) != length(survey_column_names)) {
    stop(
      "Expected ", length(survey_column_names), " survey columns but found ",
      ncol(survey), ".",
      call. = FALSE
    )
  }

  raw_row_count <- nrow(survey)
  names(survey) <- survey_column_names

  numeric_fields <- c(
    "Reaction.time", "Age", "Avg.sleep.time", "last.night.sleep.time",
    "Awake.hours", "Noise.level", "Avg.hours.exercise"
  )
  survey[numeric_fields] <- lapply(survey[numeric_fields], parse_survey_number)

  invalid_reaction <- is.na(survey$Reaction.time) |
    survey$Reaction.time <= 100 |
    survey$Reaction.time > 700
  excluded_rows <- survey[invalid_reaction, c("mockID", "Reaction.time", "Class")]
  survey <- survey[!invalid_reaction, , drop = FALSE]

  survey$Visual.acuity <- sub(" \\(.*$", "", survey$Visual.acuity)
  survey$Device.type <- extract_device_type(survey$Device.description)
  survey$Operating.system <- extract_operating_system(survey$Device.description)

  survey$Class <- factor(
    survey$Class,
    levels = c("Freshman", "Sophomore", "Junior", "Senior", "Graduate"),
    ordered = TRUE
  )
  survey$Fatigue.level <- factor(
    survey$Fatigue.level,
    levels = c(
      "Not fatigued at all", "Slightly Fatigued", "Moderately fatigued",
      "Very fatigued", "Extremely fatigued"
    ),
    ordered = TRUE
  )
  survey$Stress.level <- factor(
    survey$Stress.level,
    levels = c("Very Low", "Low", "Moderate", "High", "Very High"),
    ordered = TRUE
  )
  survey$Temp.level <- factor(
    survey$Temp.level,
    levels = c("Very Cold", "Cold", "Neutral", "Warm", "Very Warm"),
    ordered = TRUE
  )
  frequency_levels <- c(
    "Never", "Rarely", "Several times a month", "Once a week",
    "Several times a week", "Daily"
  )
  survey$Game.freq <- factor(
    survey$Game.freq, levels = frequency_levels, ordered = TRUE
  )
  survey$Sport.freq <- factor(
    survey$Sport.freq, levels = frequency_levels, ordered = TRUE
  )
  survey$Cautious.level <- factor(
    survey$Cautious.level,
    levels = c(
      "Not cautious at all", "Slightly cautious", "Moderately cautious",
      "Very cautious", "Extremely cautious"
    ),
    ordered = TRUE
  )
  survey$Visual.acuity <- factor(
    survey$Visual.acuity,
    levels = c("Very Poor", "Poor", "Average", "Good", "Excellent"),
    ordered = TRUE
  )

  factor_fields <- c(
    "Distraction", "Caffeine.intake", "Alcohol.intake", "Primary.hand",
    "Use.primary.hand", "Input.device", "Device.type", "Operating.system",
    "WiFi.stable"
  )
  survey[factor_fields] <- lapply(survey[factor_fields], factor)
  rownames(survey) <- NULL

  required_fields <- c(
    "Reaction.time", "Class", "Fatigue.level", "Stress.level"
  )
  if (anyNA(survey[required_fields])) {
    stop("Cleaning produced missing values in required analysis fields.", call. = FALSE)
  }

  attr(survey, "cleaning_summary") <- list(
    raw_rows = raw_row_count,
    retained_rows = nrow(survey),
    excluded_rows = excluded_rows
  )
  survey
}

available_class_choices <- function(survey) {
  observed <- levels(droplevels(survey$Class))
  c("All classes" = "All", stats::setNames(observed, observed))
}

filter_survey_class <- function(survey, selected_class) {
  if (is.null(selected_class) || identical(selected_class, "All")) {
    return(survey)
  }
  droplevels(survey[as.character(survey$Class) == selected_class, , drop = FALSE])
}

reaction_histogram_breaks <- function(x, bin_width = 10) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    stop("No finite reaction-time values are available.", call. = FALSE)
  }
  lower <- floor(min(x) / bin_width) * bin_width
  upper <- ceiling(max(x) / bin_width) * bin_width
  if (lower == upper) {
    upper <- lower + bin_width
  }
  seq(lower, upper, by = bin_width)
}
