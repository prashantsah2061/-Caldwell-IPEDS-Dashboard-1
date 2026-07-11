library(shiny)
library(dplyr)
library(ggplot2)

# These helpers are also sourced by app.R. Keeping this here makes this file
# understandable to RStudio/VS Code diagnostics when opened on its own.
source("functions.R")

server <- function(input, output, session) {

  # Read and prepare the enrollment data once when the app starts.
  all_university_enrollment_2 <- load_enrollment_data(
    "data/all_university_enrollment_2020_2024.csv"
  )

  radar_metrics <- c(
    "TotalEnrollment",
    "UndergraduateEnrollment",
    "GraduateEnrollment",
    "FirstTimeStudents",
    "TransferStudents",
    "NewStudentEnrollment"
  )

  radar_labels <- c(
    TotalEnrollment = "Total Enrollment",
    UndergraduateEnrollment = "Undergraduate Enrollment",
    GraduateEnrollment = "Graduate Enrollment",
    FirstTimeStudents = "First-Time Students",
    TransferStudents = "Transfer Students",
    NewStudentEnrollment = "New Student Enrollment"
  )

  radar_enrollment <- read.csv(
    "data/enrollment_dashboard.csv",
    stringsAsFactors = FALSE,
    na.strings = c("NA", "")
  ) %>%
    mutate(
      Year = as.integer(Year),
      UNITID = as.integer(UNITID),
      across(all_of(radar_metrics), as.numeric)
    )

  radar_school_choices <- radar_enrollment %>%
    distinct(UNITID, Institution, State) %>%
    arrange(Institution) %>%
    mutate(Label = paste0(Institution, " (", State, ")"))

  # Read and prepare Caldwell BSN Nursing completion data once when the app starts.
  nursing_graduates <- load_nursing_data(
    "data/caldwell_bsn_nursing_completions_2010_2024.csv"
  )

  # Read and prepare admissions data once when the app starts.
  admissions_data <- load_admissions_data(
    "data/admissions_dashboard.csv"
  )

  # Make the nursing year selector follow the actual data file.
  updateSliderInput(
    session,
    "nursing_year_range",
    min = min(nursing_graduates$Year, na.rm = TRUE),
    max = max(nursing_graduates$Year, na.rm = TRUE),
    value = c(min(nursing_graduates$Year, na.rm = TRUE), max(nursing_graduates$Year, na.rm = TRUE))
  )

  # Make the admissions year selector follow the actual admissions data file.
  updateSliderInput(
    session,
    "admissions_years",
    min = min(admissions_data$Year, na.rm = TRUE),
    max = max(admissions_data$Year, na.rm = TRUE),
    value = c(min(admissions_data$Year, na.rm = TRUE), max(admissions_data$Year, na.rm = TRUE))
  )

  # Fill the Enrollment Comparison school dropdown.
  updateSelectizeInput(
    session,
    "school",
    choices = build_enrollment_choices(all_university_enrollment_2),
    selected = "183910",
    server = TRUE
  )

  # Fill the Radar Chart school dropdown.
  updateSelectizeInput(
    session,
    "radar_school",
    choices = setNames(as.character(radar_school_choices$UNITID), radar_school_choices$Label),
    selected = "183910",
    server = TRUE
  )

  # Fill the Gender Comparison university dropdown.
  updateSelectizeInput(
    session,
    "gender_universities",
    choices = build_gender_choices(all_university_enrollment_2),
    selected = "Caldwell University",
    server = TRUE
  )

  # Fill the Admissions Dashboard school dropdown.
  updateSelectizeInput(
    session,
    "admissions_schools",
    choices = build_admissions_choices(admissions_data),
    selected = "183910",
    server = TRUE
  )

  # Filter enrollment data using the controls inside the Enrollment Comparison tab.
  selected_enrollment <- reactive({
    req(input$school, input$years)

    filter_enrollment_data(
      enrollment_data = all_university_enrollment_2,
      selected_schools = input$school,
      selected_years = input$years
    )
  })

  # Filter radar data using only the controls inside the Radar Chart tab.
  selected_radar_enrollment <- reactive({
    req(input$radar_school, input$radar_years)

    radar_enrollment %>%
      filter(
        UNITID %in% as.integer(input$radar_school),
        Year >= input$radar_years[1],
        Year <= input$radar_years[2]
      )
  })

  # Draw the enrollment trend line chart.
  output$enrollment_trend <- renderPlot({
    plot_data <- selected_enrollment()

    validate(
      need(nrow(plot_data) > 0, "No enrollment data is available for the selected schools and years.")
    )

    ggplot(plot_data, aes(x = Year, y = TotalEnrollment, color = Institution, group = Institution)) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = seq(input$years[1], input$years[2], by = 1)) +
      scale_y_continuous(labels = function(x) format_count(x)) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#333333"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      labs(
        title = "Total Enrollment Comparison",
        x = "Year",
        y = "Total Enrollment",
        color = "University"
      )
  })

  # Draw the radar chart from the existing Enrollment Comparison filters.
  output$enrollment_radar <- renderPlot({
    plot_data <- selected_radar_enrollment() %>%
      group_by(Institution) %>%
      summarise(
        across(
          all_of(radar_metrics),
          ~ ifelse(all(is.na(.x)), NA_real_, mean(.x, na.rm = TRUE))
        ),
        .groups = "drop"
      )

    validate(
      need(nrow(plot_data) > 0, "No radar data is available for the selected schools and years."),
      need(any(!is.na(plot_data[radar_metrics])), "Radar values are missing for the selected schools and years.")
    )

    max_values <- radar_enrollment %>%
      summarise(across(all_of(radar_metrics), ~ max(.x, na.rm = TRUE))) %>%
      mutate(across(all_of(radar_metrics), ~ ifelse(is.infinite(.x) | .x <= 0, NA_real_, .x)))

    normalized_data <- plot_data %>%
      mutate(
        across(
          all_of(radar_metrics),
          ~ ifelse(
            is.na(.) | is.na(max_values[[cur_column()]]),
            NA_real_,
            pmin((. / max_values[[cur_column()]]) * 100, 100)
          )
        )
      ) %>%
      tidyr::pivot_longer(
        cols = all_of(radar_metrics),
        names_to = "Metric",
        values_to = "NormalizedValue"
      ) %>%
      mutate(
        Metric = factor(radar_labels[Metric], levels = radar_labels),
        NormalizedValue = ifelse(is.na(NormalizedValue), 0, NormalizedValue)
      )

    ggplot(normalized_data, aes(x = Metric, y = NormalizedValue, group = Institution, color = Institution, fill = Institution)) +
      geom_polygon(alpha = 0.14, linewidth = 1) +
      geom_point(size = 2.8) +
      coord_polar() +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      labs(
        title = "Enrollment Profile on Normalized 0-100 Scale",
        x = NULL,
        y = "Normalized Value",
        color = "University",
        fill = "University"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "#333333", size = 11),
        axis.text.y = element_text(color = "#666666"),
        plot.title = element_text(face = "bold", color = "#333333"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })

  # Display selected enrollment rows with integer counts.
  output$enrollment_summary <- renderTable({
    selected_enrollment() %>%
      select(Year, Institution, City, State, TotalEnrollment, Men, Women) %>%
      mutate(
        TotalEnrollment = as.integer(TotalEnrollment),
        Men = as.integer(Men),
        Women = as.integer(Women)
      ) %>%
      arrange(Institution, Year)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Latest enrollment KPI uses Caldwell University because it is the default school.
  output$kpi_enrollment <- renderText({
    latest <- all_university_enrollment_2 %>%
      filter(UNITID == 183910) %>%
      arrange(desc(Year)) %>%
      slice(1)

    format_count(latest$TotalEnrollment)
  })

  # Enrollment growth KPI updates dynamically with the Enrollment tab year slider.
  output$kpi_growth <- renderText({
    caldwell <- all_university_enrollment_2 %>%
      filter(
        UNITID == 183910,
        Year >= input$years[1],
        Year <= input$years[2]
      ) %>%
      arrange(Year)

    validate(
      need(nrow(caldwell) > 1, "Select at least two years"),
      need(caldwell$TotalEnrollment[1] != 0, "N/A")
    )

    first <- caldwell$TotalEnrollment[1]
    last <- caldwell$TotalEnrollment[nrow(caldwell)]
    growth <- ((last - first) / first) * 100

    paste0(round(growth, 1), "%")
  })

  # Summarize gender data by selected university and selected year mode.
  gender_data <- reactive({
    req(input$gender_universities, input$gender_year_mode)

    if (input$gender_year_mode == "single") {
      req(input$gender_single_year)
    } else {
      req(input$gender_year_range)
    }

    summarize_gender_data(
      enrollment_data = all_university_enrollment_2,
      universities = input$gender_universities,
      year_mode = input$gender_year_mode,
      single_year = input$gender_single_year,
      year_range = input$gender_year_range
    )
  })

  # Calculate percentages and sort universities based on the selected sort option.
  sorted_gender_data <- reactive({
    plot_data <- gender_data() %>%
      add_gender_percentages()

    validate(
      need(nrow(plot_data) > 0, "No enrollment data is available for the selected universities and years.")
    )

    sort_gender_data(plot_data, input$gender_sort_by)
  })

  # Convert one row per university into male/female rows for the stacked bar chart.
  stacked_gender_data <- reactive({
    make_stacked_gender_data(sorted_gender_data())
  })

  # Increase chart height automatically as more universities are selected.
  gender_plot_height <- reactive({
    gender_plot_height_value(nrow(sorted_gender_data()))
  })

  # Render the gender plot with dynamic height inside a scrollable container.
  output$gender_plot_ui <- renderUI({
    plotOutput(
      "gender_plot",
      height = paste0(gender_plot_height(), "px"),
      hover = hoverOpts(
        id = "gender_hover",
        delay = 80,
        delayType = "debounce",
        nullOutside = TRUE
      )
    )
  })

  # Draw the horizontal 100% stacked gender composition chart.
  output$gender_plot <- renderPlot({
    plot_data <- stacked_gender_data()

    ggplot(plot_data, aes(x = Percent, y = Institution, fill = Gender)) +
      geom_col(width = 0.72, color = "white", linewidth = 0.4) +
      geom_text(
        aes(label = ifelse(Percent >= 8, format_percent(Percent), "")),
        position = position_stack(vjust = 0.5),
        color = "white",
        fontface = "bold",
        size = 3.8
      ) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        labels = function(x) paste0(x, "%"),
        expand = c(0, 0)
      ) +
      scale_fill_manual(values = c("Male" = "#2F6F9F", "Female" = "#C84C6A")) +
      labs(
        x = "Percent of Total Enrollment",
        y = NULL,
        fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11, color = "#333333"),
        axis.text.x = element_text(color = "#555555"),
        axis.title.x = element_text(color = "#333333", margin = margin(t = 10)),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })

  # Show enrollment details when hovering near a university bar.
  output$gender_hover_details <- renderUI({
    hover <- input$gender_hover

    if (is.null(hover) || is.null(hover$y)) {
      return(div(class = "hover-box", "Hover over a university bar to view enrollment counts and percentages."))
    }

    plot_data <- sorted_gender_data()
    y_position <- as.integer(round(hover$y))
    university_levels <- levels(plot_data$Institution)

    if (is.na(y_position) || y_position < 1 || y_position > length(university_levels)) {
      return(div(class = "hover-box", "Hover over a university bar to view enrollment counts and percentages."))
    }

    hovered_university <- university_levels[y_position]
    selected_row <- plot_data %>%
      filter(as.character(Institution) == hovered_university)

    if (nrow(selected_row) == 0) {
      return(div(class = "hover-box", "Hover over a university bar to view enrollment counts and percentages."))
    }

    div(
      class = "hover-box",
      tags$strong(as.character(selected_row$Institution)), br(),
      paste("Year:", selected_row$YearLabel), br(),
      paste("Male Count:", format_count(selected_row$Men)), br(),
      paste("Female Count:", format_count(selected_row$Women)), br(),
      paste("Total Enrollment:", format_count(selected_row$TotalEnrollment)), br(),
      paste("Male %:", format_percent(selected_row$MalePercent)), br(),
      paste("Female %:", format_percent(selected_row$FemalePercent))
    )
  })

  # Display the sortable gender summary table.
  output$gender_summary <- renderDataTable({
    sorted_gender_data() %>%
      transmute(
        Institution = as.character(Institution),
        Year = YearLabel,
        `Total Enrollment` = as.integer(round(TotalEnrollment, 0)),
        Men = as.integer(round(Men, 0)),
        Women = as.integer(round(Women, 0)),
        `Male %` = round(MalePercent, 1),
        `Female %` = round(FemalePercent, 1)
      )
  },
  options = list(
    pageLength = 15,
    lengthMenu = c(10, 15, 25, 50, 100),
    autoWidth = TRUE
  ))

  # Filter the nursing data once so KPIs, chart, summary, and table stay consistent.
  selected_nursing <- reactive({
    req(input$nursing_year_range)

    filtered_data <- filter_nursing_data(
      nursing_data = nursing_graduates,
      selected_years = input$nursing_year_range
    )

    validate(
      need(nrow(filtered_data) > 0, "No nursing graduate data is available for the selected years.")
    )

    filtered_data
  })

  # Calculate dynamic nursing KPIs from the filtered nursing data.
  nursing_kpis <- reactive({
    summarize_nursing_kpis(selected_nursing())
  })

  output$nursing_total_graduates <- renderText({
    format_count(nursing_kpis()$total_graduates)
  })

  output$nursing_first_year <- renderText({
    nursing_kpis()$first_year
  })

  output$nursing_first_year_graduates <- renderText({
    format_count(nursing_kpis()$first_year_graduates)
  })

  output$nursing_latest_year <- renderText({
    nursing_kpis()$latest_year
  })

  output$nursing_latest_year_graduates <- renderText({
    format_count(nursing_kpis()$latest_year_graduates)
  })

  output$nursing_highest_year <- renderText({
    nursing_kpis()$highest_year
  })

  output$nursing_highest_graduates <- renderText({
    format_count(nursing_kpis()$highest_graduates)
  })

  output$nursing_lowest_year <- renderText({
    nursing_kpis()$lowest_year
  })

  output$nursing_lowest_graduates <- renderText({
    format_count(nursing_kpis()$lowest_graduates)
  })

  output$nursing_average_graduates <- renderText({
    format_count(nursing_kpis()$average_graduates)
  })

  output$nursing_growth <- renderText({
    format_metric_percent(nursing_kpis()$growth)
  })

  output$nursing_cagr <- renderText({
    format_metric_percent(nursing_kpis()$cagr)
  })

  output$nursing_largest_increase <- renderText({
    kpis <- nursing_kpis()

    if (is.na(kpis$largest_increase)) {
      "N/A"
    } else {
      paste0("+", format_count(kpis$largest_increase), " in ", kpis$largest_increase_year)
    }
  })

  output$nursing_largest_decrease <- renderText({
    kpis <- nursing_kpis()

    if (is.na(kpis$largest_decrease)) {
      "N/A"
    } else {
      paste0(format_count(kpis$largest_decrease), " in ", kpis$largest_decrease_year)
    }
  })

  # Draw the nursing graduate trend chart and highlight unusual year-over-year changes.
  output$nursing_trend <- renderPlot({
    plot_data <- selected_nursing() %>%
      mutate(YearOverYearChange = TotalGraduates - lag(TotalGraduates))

    increase_year <- nursing_kpis()$largest_increase_year
    decrease_year <- nursing_kpis()$largest_decrease_year

    unusual_data <- plot_data %>%
      mutate(
        Highlight = case_when(
          Year == increase_year ~ "Largest increase",
          Year == decrease_year ~ "Largest decrease",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(Highlight))

    nursing_plot <- ggplot(plot_data, aes(x = Year, y = TotalGraduates)) +
      scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 1)) +
      scale_y_continuous(labels = function(x) format_count(x)) +
      labs(
        title = "BSN Nursing Graduates Over Time",
        x = "Year",
        y = "Total Nursing Graduates",
        fill = "Highlighted year"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#333333"),
        axis.title = element_text(color = "#333333"),
        axis.text = element_text(color = "#555555"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )

    if (nrow(plot_data) >= 2) {
      nursing_plot <- nursing_plot +
        geom_line(color = "#CC0000", linewidth = 1.2)
    }

    nursing_plot <- nursing_plot +
      geom_point(color = "#CC0000", fill = "white", shape = 21, size = 3.8, stroke = 1.2)

    if (nrow(unusual_data) > 0) {
      nursing_plot <- nursing_plot +
        geom_point(
          data = unusual_data,
          aes(fill = Highlight),
          color = "#2f3437",
          shape = 21,
          size = 5,
          stroke = 1.2
        ) +
        scale_fill_manual(
          values = c("Largest increase" = "#2F6F9F", "Largest decrease" = "#C84C6A"),
          na.translate = FALSE
        )
    }

    nursing_plot
  })

  # Display the exact graduate count when users hover near a year on the chart.
  output$nursing_hover_details <- renderUI({
    hover <- input$nursing_hover

    if (is.null(hover) || is.null(hover$x)) {
      return(div(class = "hover-box", "Hover over a year to view the exact BSN Nursing graduate count."))
    }

    plot_data <- selected_nursing() %>%
      mutate(YearOverYearChange = TotalGraduates - lag(TotalGraduates))

    nearest_year <- plot_data$Year[which.min(abs(plot_data$Year - hover$x))]
    selected_row <- plot_data %>% filter(Year == nearest_year)

    if (nrow(selected_row) == 0) {
      return(div(class = "hover-box", "Hover over a year to view the exact BSN Nursing graduate count."))
    }

    change_text <- ifelse(
      is.na(selected_row$YearOverYearChange),
      "First year in selected range",
      paste0("Change from previous selected year: ", format_count(selected_row$YearOverYearChange))
    )

    div(
      class = "hover-box",
      tags$strong(paste("Year:", selected_row$Year)), br(),
      paste("Total BSN Nursing Graduates:", format_count(selected_row$TotalGraduates)), br(),
      change_text
    )
  })

  # Automatically update the executive summary for the selected range.
  output$nursing_executive_summary <- renderText({
    build_nursing_summary(nursing_kpis())
  })

  # Display the selected nursing rows in a simple Shiny table.
  output$nursing_table <- renderTable({
    selected_nursing() %>%
      mutate(`Year-over-Year Change` = TotalGraduates - lag(TotalGraduates)) %>%
      transmute(
        Year,
        Institution,
        `CIP Code` = CIPCode,
        `Award Level` = AwardLevel,
        `Total Graduates` = as.integer(TotalGraduates),
        `Male Graduates` = as.integer(MaleGraduates),
        `Female Graduates` = as.integer(FemaleGraduates),
        `Year-over-Year Change` = ifelse(
          is.na(`Year-over-Year Change`),
          NA,
          as.integer(`Year-over-Year Change`)
        )
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Filter admissions data once so KPIs and charts use the same selected rows.
  selected_admissions <- reactive({
    req(input$admissions_schools, input$admissions_years)

    filtered_data <- filter_admissions_data(
      admissions_data = admissions_data,
      selected_schools = input$admissions_schools,
      selected_years = input$admissions_years
    )

    validate(
      need(nrow(filtered_data) > 0, "No admissions data is available for the selected schools and years.")
    )

    filtered_data
  })

  admissions_kpis <- reactive({
    summarize_admissions_kpis(selected_admissions())
  })

  output$admissions_applications <- renderText({
    format_count(admissions_kpis()$applications)
  })

  output$admissions_accepted <- renderText({
    format_count(admissions_kpis()$accepted)
  })

  output$admissions_enrolled <- renderText({
    format_count(admissions_kpis()$enrolled)
  })

  output$admissions_acceptance_rate <- renderText({
    format_metric_percent(admissions_kpis()$acceptance_rate)
  })

  output$admissions_yield_rate <- renderText({
    format_metric_percent(admissions_kpis()$yield_rate)
  })

  output$admissions_funnel <- renderPlot({
    plot_data <- selected_admissions() %>%
      select(Year, Institution, Applications, Accepted, Enrolled) %>%
      tidyr::pivot_longer(
        cols = c(Applications, Accepted, Enrolled),
        names_to = "Stage",
        values_to = "Count"
      ) %>%
      mutate(
        Stage = factor(Stage, levels = c("Applications", "Accepted", "Enrolled"))
      )

    ggplot(plot_data, aes(x = Stage, y = Count, fill = Institution)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.68) +
      facet_wrap(~ Year) +
      scale_y_continuous(labels = function(x) format_count(x)) +
      labs(
        x = NULL,
        y = "Students",
        fill = "University"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#555555"),
        axis.title = element_text(color = "#333333"),
        strip.text = element_text(face = "bold", color = "#333333"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })

  output$admissions_line <- renderPlot({
    req(input$admissions_line_metric)

    selected_metrics <- input$admissions_line_metric

    plot_data <- selected_admissions() %>%
      select(Year, Institution, all_of(selected_metrics)) %>%
      tidyr::pivot_longer(
        cols = all_of(selected_metrics),
        names_to = "Metric",
        values_to = "Count"
      ) %>%
      mutate(
        Series = paste(Institution, Metric)
      )

    ggplot(plot_data, aes(x = Year, y = Count, color = Series, group = Series)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      scale_x_continuous(breaks = seq(input$admissions_years[1], input$admissions_years[2], by = 1)) +
      scale_y_continuous(labels = function(x) format_count(x)) +
      labs(
        x = "Year",
        y = "Students",
        color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#555555"),
        axis.title = element_text(color = "#333333"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })

  output$admissions_rate_bar <- renderPlot({
    plot_data <- summarize_admissions_comparison(selected_admissions())

    validate(
      need(nrow(plot_data) > 0, "No admissions rate data is available for the selected schools and years.")
    )

    ggplot(plot_data, aes(x = reorder(Institution, Rate), y = Rate, fill = RateType)) +
      geom_col(position = position_dodge(width = 0.74), width = 0.68) +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
      scale_fill_manual(values = c("Acceptance Rate" = "#2F6F9F", "Yield Rate" = "#5A8F3A")) +
      labs(
        x = NULL,
        y = "Rate",
        fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#555555"),
        axis.title = element_text(color = "#333333"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })
}
