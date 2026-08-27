library(shiny)
library(ggplot2)

source(file.path("R", "data_processing.R"), local = TRUE)

survey_all <- read_and_clean_survey()
class_choices <- available_class_choices(survey_all)
analysis_choices <- stats::setNames(
  names(survey_variable_labels),
  unname(survey_variable_labels)
)

format_p_value <- function(p_value) {
  if (is.na(p_value)) {
    return("not available")
  }
  if (p_value < 0.001) {
    return("< 0.001")
  }
  format(round(p_value, 3), nsmall = 3)
}

make_reaction_plot <- function(data, group_var = "") {
  plot_data <- data[is.finite(data$Reaction.time), , drop = FALSE]
  breaks <- reaction_histogram_breaks(plot_data$Reaction.time)

  plot <- ggplot(plot_data, aes(x = Reaction.time))
  if (nzchar(group_var)) {
    plot <- plot +
      geom_histogram(
        aes(fill = .data[[group_var]]),
        breaks = breaks,
        color = "white",
        linewidth = 0.25,
        position = "stack"
      ) +
      scale_fill_brewer(
        palette = "YlOrRd",
        drop = FALSE,
        name = unname(survey_variable_labels[[group_var]])
      )
  } else {
    plot <- plot +
      geom_histogram(
        breaks = breaks,
        fill = "#2C7FB8",
        color = "white",
        linewidth = 0.25
      )
  }

  plot +
    labs(
      title = "Distribution of reaction time",
      subtitle = paste(nrow(plot_data), "students; 10 ms bins"),
      x = "Reaction time (ms)",
      y = "Number of students"
    ) +
    scale_x_continuous(breaks = pretty(range(breaks), n = 8)) +
    scale_y_continuous(
      breaks = function(limits) seq(0, ceiling(max(limits)), by = 1),
      expand = expansion(c(0, 0.05))
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot"
    )
}

draw_base_reaction_plot <- function(data, group_var = "") {
  plot_data <- data[is.finite(data$Reaction.time), , drop = FALSE]
  reaction_time <- plot_data$Reaction.time
  breaks <- reaction_histogram_breaks(reaction_time)

  if (!nzchar(group_var)) {
    histogram <- hist(
      reaction_time,
      breaks = breaks,
      plot = FALSE,
      right = FALSE,
      include.lowest = TRUE
    )
    hist(
      reaction_time,
      breaks = breaks,
      col = "#2C7FB8",
      border = "white",
      right = FALSE,
      include.lowest = TRUE,
      main = "Distribution of reaction time",
      sub = paste(nrow(plot_data), "students; 10 ms bins"),
      xlab = "Reaction time (ms)",
      ylab = "Number of students",
      yaxt = "n"
    )
    axis(2, at = seq(0, max(histogram$counts), by = 1), las = 1)
    return(invisible(NULL))
  }

  group <- droplevels(factor(plot_data[[group_var]]))
  group_levels <- levels(group)
  count_matrix <- vapply(
    group_levels,
    function(level) {
      hist(
        reaction_time[group == level],
        breaks = breaks,
        plot = FALSE,
        right = FALSE,
        include.lowest = TRUE
      )$counts
    },
    numeric(length(breaks) - 1)
  )
  if (is.null(dim(count_matrix))) {
    count_matrix <- matrix(count_matrix, ncol = 1)
  }

  totals <- rowSums(count_matrix)
  max_count <- max(totals, 1)
  colors <- grDevices::hcl.colors(length(group_levels), "YlOrRd", rev = TRUE)

  plot(
    NA,
    xlim = range(breaks),
    ylim = c(0, max_count * 1.08),
    xaxs = "i",
    yaxs = "i",
    xlab = "Reaction time (ms)",
    ylab = "Number of students",
    main = "Distribution of reaction time",
    sub = paste(nrow(plot_data), "students; 10 ms bins"),
    axes = FALSE
  )
  axis(1, at = pretty(range(breaks), n = 8))
  axis(2, at = seq(0, max_count, by = 1), las = 1)
  box()

  bottom <- rep(0, length(totals))
  for (index in seq_along(group_levels)) {
    top <- bottom + count_matrix[, index]
    rect(
      breaks[-length(breaks)], bottom,
      breaks[-1], top,
      col = colors[index], border = "white"
    )
    bottom <- top
  }
  legend(
    "topright",
    legend = group_levels,
    fill = colors,
    border = NA,
    title = unname(survey_variable_labels[[group_var]]),
    cex = 0.8,
    bg = "white"
  )
  invisible(NULL)
}

make_relationship_plot <- function(data, variable) {
  label <- unname(survey_variable_labels[[variable]])
  complete <- is.finite(data$Reaction.time) & !is.na(data[[variable]])
  plot_data <- data[complete, , drop = FALSE]

  if (is.numeric(plot_data[[variable]])) {
    plot <- ggplot(
      plot_data,
      aes(x = .data[[variable]], y = Reaction.time)
    ) +
      geom_point(color = "#2C7FB8", alpha = 0.7, size = 2)
    if (length(unique(plot_data[[variable]])) >= 2) {
      plot <- plot +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#D95F0E")
    }
    plot <- plot + labs(x = label)
  } else {
    plot_data[[variable]] <- droplevels(factor(plot_data[[variable]]))
    plot <- ggplot(
      plot_data,
      aes(x = .data[[variable]], y = Reaction.time, fill = .data[[variable]])
    ) +
      geom_boxplot(outlier.shape = NA, alpha = 0.75, show.legend = FALSE) +
      geom_jitter(width = 0.15, alpha = 0.45, size = 1.5, show.legend = FALSE) +
      scale_fill_brewer(palette = "Set3") +
      labs(x = label) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }

  plot +
    labs(
      title = paste("Reaction time and", tolower(label)),
      subtitle = paste(nrow(plot_data), "complete observations"),
      y = "Reaction time (ms)"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
}

ui <- navbarPage(
  title = "Reaction Time Survey",
  id = "main_navigation",
  header = tags$head(
    tags$style(HTML(
      ".navbar { margin-bottom: 0; }
       .app-note { color: #555; max-width: 900px; }
       .table-scroll { overflow-x: auto; }
       .well { background-color: #f7f9fb; }"
    ))
  ),

  tabPanel(
    "Distribution",
    sidebarLayout(
      sidebarPanel(
        selectInput("class_filter", "Class", choices = class_choices),
        selectInput(
          "histogram_group",
          "Stack bars by",
          choices = c(
            "No grouping" = "",
            "Fatigue level" = "Fatigue.level",
            "Stress level" = "Stress.level"
          )
        ),
        radioButtons(
          "plot_library",
          "Plotting library",
          choices = c("ggplot2", "Base R"),
          inline = TRUE
        ),
        downloadButton("download_plot", "Download plot")
      ),
      mainPanel(
        h2("Reaction-time distribution"),
        p(
          class = "app-note",
          "Each student is counted exactly once. Colored segments show the actual group composition within each 10 ms bin."
        ),
        plotOutput("reaction_plot", height = "560px")
      )
    )
  ),

  tabPanel(
    "Variable Explorer",
    sidebarLayout(
      sidebarPanel(
        selectInput("explorer_class", "Class", choices = class_choices),
        selectInput(
          "explorer_variable",
          "Survey variable",
          choices = analysis_choices,
          selected = "Avg.sleep.time"
        )
      ),
      mainPanel(
        h2("Explore every survey variable"),
        p(
          class = "app-note",
          "Numeric variables use a scatterplot and an unadjusted Pearson correlation. Categorical variables use boxplots and an unadjusted Kruskal-Wallis test. These are exploratory associations, not causal estimates."
        ),
        plotOutput("relationship_plot", height = "500px"),
        h4("Exploratory association"),
        verbatimTextOutput("association_result"),
        h4("Descriptive summary"),
        tableOutput("relationship_summary")
      )
    )
  ),

  tabPanel(
    "Dataset",
    fluidPage(
      h2("Cleaned survey dataset"),
      textOutput("cleaning_summary"),
      h3("Reaction time by class"),
      tableOutput("class_summary"),
      h3("Preview"),
      downloadButton("download_clean_data", "Download cleaned CSV"),
      div(class = "table-scroll", tableOutput("data_preview"))
    )
  ),

  tabPanel(
    "About",
    fluidPage(
      h2("About the dataset"),
      p(
        "This survey records reaction time alongside sleep, fatigue, stress, environment, lifestyle, vision, handedness, device, operating system, and connection conditions for University of Illinois students."
      ),
      h2("About the app"),
      p(
        "The app supports transparent descriptive exploration of the survey. It filters implausible reaction times at or below 100 ms and values above the survey maximum of 700 ms. Statistical results are unadjusted and should not be interpreted as evidence of causation."
      ),
      h3("Developer: Yuda Zhu")
    )
  )
)

server <- function(input, output, session) {
  histogram_data <- reactive({
    filter_survey_class(survey_all, input$class_filter)
  })

  output$reaction_plot <- renderPlot({
    data <- histogram_data()
    validate(need(nrow(data) > 0, "No observations match this class."))

    if (identical(input$plot_library, "Base R")) {
      draw_base_reaction_plot(data, input$histogram_group)
    } else {
      print(make_reaction_plot(data, input$histogram_group))
    }
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("reaction-time-", Sys.Date(), ".png")
    },
    content = function(file) {
      data <- histogram_data()
      if (identical(input$plot_library, "Base R")) {
        grDevices::png(file, width = 1200, height = 760, res = 130)
        on.exit(grDevices::dev.off(), add = TRUE)
        draw_base_reaction_plot(data, input$histogram_group)
      } else {
        ggplot2::ggsave(
          filename = file,
          plot = make_reaction_plot(data, input$histogram_group),
          width = 10,
          height = 6.4,
          dpi = 130
        )
      }
    }
  )

  explorer_data <- reactive({
    filter_survey_class(survey_all, input$explorer_class)
  })

  output$relationship_plot <- renderPlot({
    data <- explorer_data()
    variable <- input$explorer_variable
    validate(
      need(variable %in% names(survey_variable_labels), "Choose a survey variable."),
      need(sum(!is.na(data[[variable]])) > 1, "Not enough observations to plot this variable.")
    )
    print(make_relationship_plot(data, variable))
  })

  output$association_result <- renderText({
    data <- explorer_data()
    variable <- input$explorer_variable
    complete <- is.finite(data$Reaction.time) & !is.na(data[[variable]])
    x <- data[[variable]][complete]
    y <- data$Reaction.time[complete]

    if (is.numeric(x)) {
      if (length(x) < 3 || length(unique(x)) < 2) {
        return("Not enough variation for a correlation test.")
      }
      test <- stats::cor.test(x, y, method = "pearson")
      return(sprintf(
        "Pearson r = %.3f; p-value %s; n = %d.",
        unname(test$estimate), format_p_value(test$p.value), length(x)
      ))
    }

    group <- droplevels(factor(x))
    if (nlevels(group) < 2) {
      return("Only one group is present, so a group comparison is not available.")
    }
    test <- stats::kruskal.test(y ~ group)
    sprintf(
      "Kruskal-Wallis chi-squared = %.3f; df = %d; p-value %s; n = %d.",
      unname(test$statistic), unname(test$parameter),
      format_p_value(test$p.value), length(y)
    )
  })

  output$relationship_summary <- renderTable({
    data <- explorer_data()
    variable <- input$explorer_variable
    complete <- is.finite(data$Reaction.time) & !is.na(data[[variable]])
    x <- data[[variable]][complete]
    y <- data$Reaction.time[complete]

    if (is.numeric(x)) {
      return(data.frame(
        Measure = c(
          "Complete observations", "Variable mean", "Variable SD",
          "Reaction-time mean (ms)", "Reaction-time SD (ms)"
        ),
        Value = c(
          length(x), round(mean(x), 2), round(stats::sd(x), 2),
          round(mean(y), 2), round(stats::sd(y), 2)
        ),
        check.names = FALSE
      ))
    }

    group <- droplevels(factor(x))
    split_reaction <- split(y, group, drop = TRUE)
    data.frame(
      Group = names(split_reaction),
      N = vapply(split_reaction, length, integer(1)),
      Mean = round(vapply(split_reaction, mean, numeric(1)), 1),
      Median = round(vapply(split_reaction, stats::median, numeric(1)), 1),
      SD = round(vapply(split_reaction, stats::sd, numeric(1)), 1),
      check.names = FALSE,
      row.names = NULL
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$cleaning_summary <- renderText({
    summary <- attr(survey_all, "cleaning_summary")
    paste0(
      summary$raw_rows, " original responses; ", summary$retained_rows,
      " retained; ", nrow(summary$excluded_rows),
      " excluded because reaction time was outside 101-700 ms."
    )
  })

  output$class_summary <- renderTable({
    reaction_by_class <- split(
      survey_all$Reaction.time,
      droplevels(survey_all$Class),
      drop = TRUE
    )
    data.frame(
      Class = names(reaction_by_class),
      N = vapply(reaction_by_class, length, integer(1)),
      Mean = round(vapply(reaction_by_class, mean, numeric(1)), 1),
      Median = round(vapply(reaction_by_class, stats::median, numeric(1)), 1),
      Minimum = vapply(reaction_by_class, min, numeric(1)),
      Maximum = vapply(reaction_by_class, max, numeric(1)),
      check.names = FALSE,
      row.names = NULL
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$data_preview <- renderTable({
    utils::head(survey_all, 20)
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")

  output$download_clean_data <- downloadHandler(
    filename = function() {
      paste0("reaction-time-survey-cleaned-", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(survey_all, file, row.names = FALSE, na = "")
    }
  )
}

app <- shinyApp(ui = ui, server = server)
app
