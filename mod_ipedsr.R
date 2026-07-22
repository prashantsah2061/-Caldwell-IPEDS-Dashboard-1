server <- function(input, output, session) {
  if (!exists("dashboard_data", inherits = TRUE)) {
    dashboard_data <<- load_dashboard_data()
  }

  # Graduation Rate Analytics dashboard module.
  gr_dashboard_server("graduation_rate")

  # Retention Analysis dashboard module.
  ra_dashboard_server("retention_analysis")

  # Institutional Efficiency dashboard module.
  ie_dashboard_server("institutional_efficiency")

  enrollment_data <- dashboard_data$enrollment_data
  default_universities <- default_university_ids(enrollment_data)

  # ------------------------------------------------------------
  # 12-month enrollment data loading
  # ------------------------------------------------------------

  e12_demo_data <- dashboard_data$e12_demo_data
  e12_attendance_data <- dashboard_data$e12_attendance_data
  e12_fte_data <- dashboard_data$e12_fte_data
  e12_metadata <- dashboard_data$e12_metadata
  e12_default_selection <- e12_default_universities(e12_metadata)

  # ------------------------------------------------------------
  # Reactive filters
  # ------------------------------------------------------------

  filtered_universe <- reactive({
    filter_institution_universe(
      enrollment_data = enrollment_data,
      level_filter = input$level_filter,
      control_filter = input$control_filter
    )
  })

  observe({
    available_choices <- filtered_universe() |>
      distinct(UNITID, University) |>
      arrange(University)

    current_selection <- isolate(input$selected_universities)
    valid_selection <- intersect(current_selection, available_choices$UNITID)

    if (length(valid_selection) == 0) {
      preferred_defaults <- intersect(default_universities, available_choices$UNITID)

      valid_selection <- if (length(preferred_defaults) > 0) {
        preferred_defaults
      } else {
        available_choices |>
          slice_head(n = 5) |>
          pull(UNITID)
      }
    }

    updateSelectizeInput(
      session = session,
      inputId = "selected_universities",
      choices = build_university_choices(filtered_universe()),
      selected = valid_selection,
      server = TRUE
    )
  })

  observeEvent(input$reset_filters, {
    updateSelectInput(session, "level_filter", selected = "All")
    updateSelectInput(session, "control_filter", selected = "All")
    updateSliderInput(session, "year_range", value = c(2010, 2024))

    reset_selection <- if (length(default_universities) > 0) {
      default_universities
    } else {
      enrollment_data |>
        distinct(UNITID, University) |>
        arrange(University) |>
        slice_head(n = 5) |>
        pull(UNITID)
    }

    updateSelectizeInput(
      session = session,
      inputId = "selected_universities",
      choices = build_university_choices(enrollment_data),
      selected = reset_selection,
      server = TRUE
    )
  })

  filtered_data <- reactive({
    req(input$year_range)

    validate(
      need(
        length(input$selected_universities) > 0,
        "Select at least one university to display enrollment data."
      )
    )

    data <- filtered_universe() |>
      filter_selected_enrollment(
        selected_universities = input$selected_universities,
        year_range = input$year_range
      )

    validate(
      need(
        nrow(data) > 0,
        "No enrollment records match the selected filters."
      )
    )

    data
  })

  # ------------------------------------------------------------
  # KPI cards
  # ------------------------------------------------------------

  output$kpi_universities <- renderText({
    data <- filtered_data()
    format_count(n_distinct(data$UNITID))
  })

  output$kpi_latest_total <- renderText({
    kpis <- summarize_enrollment_kpis(filtered_data())
    format_count(kpis$latest_total)
  })

  output$kpi_average <- renderText({
    kpis <- summarize_enrollment_kpis(filtered_data())
    format_count(kpis$average_enrollment)
  })

  # ------------------------------------------------------------
  # Graph
  # ------------------------------------------------------------

  output$enrollment_plot <- renderPlot({
    data <- filtered_data()

    ggplot(
      data,
      aes(
        x = Year,
        y = TotalEnrollment,
        color = University,
        group = UNITID
      )
    ) +
      geom_line(linewidth = 1.05, alpha = 0.9) +
      geom_point(size = 2.6, alpha = 0.95) +
      scale_x_continuous(
        breaks = seq(input$year_range[1], input$year_range[2], by = 1)
      ) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = NULL,
        y = "Total enrollment",
        color = "University"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#eeeeee"),
        panel.grid.major.y = element_line(color = "#e6e6e6"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      )
  })

  # ------------------------------------------------------------
  # Table
  # ------------------------------------------------------------

  output$enrollment_table <- renderDT({
    datatable(
      prepare_enrollment_table(filtered_data()),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE,
        order = list(list(1, "asc"), list(2, "asc"))
      )
    ) |>
      formatCurrency(
        columns = "TotalEnrollment",
        currency = "",
        interval = 3,
        mark = ",",
        digits = 0
      )
  })

  # ------------------------------------------------------------
  # 12-month enrollment shared filters
  # ------------------------------------------------------------

  updateSelectInput(
    session,
    "e12_state",
    choices = c("All", sort(unique(e12_metadata$State))),
    selected = "All"
  )

  updateSelectInput(
    session,
    "e12_demo_student_level",
    choices = sort(unique(e12_demo_data$StudentLevel)),
    selected = "Total"
  )

  updateSelectInput(
    session,
    "e12_demo_gender",
    choices = sort(unique(e12_demo_data$Gender)),
    selected = "Total"
  )

  updateSelectInput(
    session,
    "e12_demo_ethnicity",
    choices = sort(unique(e12_demo_data$Ethnicity)),
    selected = "Total"
  )

  updateSelectInput(
    session,
    "e12_attendance_student_level",
    choices = sort(unique(e12_attendance_data$StudentLevel)),
    selected = "Combined"
  )

  updateSelectInput(
    session,
    "e12_attendance_status",
    choices = sort(unique(e12_attendance_data$AttendanceStatus)),
    selected = "Both"
  )

  e12_available_metadata <- reactive({
    e12_filter_metadata(
      metadata = e12_metadata,
      state_filter = input$e12_state,
      control_filter = input$e12_control,
      level_filter = input$e12_level
    )
  })

  observe({
    available_metadata <- e12_available_metadata()
    current_selection <- isolate(input$e12_institution)
    valid_selection <- intersect(current_selection, available_metadata$UNITID)

    if (length(valid_selection) == 0) {
      preferred_defaults <- intersect(e12_default_selection, available_metadata$UNITID)

      valid_selection <- if (length(preferred_defaults) > 0) {
        preferred_defaults
      } else {
        available_metadata |>
          arrange(University) |>
          slice_head(n = 3) |>
          pull(UNITID)
      }
    }

    updateSelectizeInput(
      session = session,
      inputId = "e12_institution",
      choices = e12_university_choices(available_metadata),
      selected = valid_selection,
      server = TRUE
    )
  })

  observeEvent(input$e12_reset_filters, {
    updateSelectInput(session, "e12_state", selected = "All")
    updateSelectInput(session, "e12_control", selected = "All")
    updateSelectInput(session, "e12_level", selected = "All")
    updateSliderInput(session, "e12_year_range", value = c(2010, 2024))
    updateSelectInput(session, "e12_demo_student_level", selected = "Total")
    updateSelectInput(session, "e12_demo_gender", selected = "Total")
    updateSelectInput(session, "e12_demo_ethnicity", selected = "Total")
    updateSelectInput(session, "e12_attendance_student_level", selected = "Combined")
    updateSelectInput(session, "e12_attendance_status", selected = "Both")

    updateSelectizeInput(
      session = session,
      inputId = "e12_institution",
      choices = e12_university_choices(e12_metadata),
      selected = e12_default_selection,
      server = TRUE
    )
  })

  # ------------------------------------------------------------
  # 12-month demographic reactive data
  # ------------------------------------------------------------

  e12_demo_filtered <- reactive({
    req(
      input$e12_institution,
      input$e12_year_range,
      input$e12_demo_student_level,
      input$e12_demo_gender,
      input$e12_demo_ethnicity
    )

    data <- e12_demo_data |>
      e12_apply_shared_filters(
        institutions = input$e12_institution,
        year_range = input$e12_year_range,
        state_filter = input$e12_state,
        control_filter = input$e12_control,
        level_filter = input$e12_level
      ) |>
      filter(
        StudentLevel == input$e12_demo_student_level,
        Gender == input$e12_demo_gender,
        Ethnicity == input$e12_demo_ethnicity
      )

    validate(
      need(length(input$e12_institution) > 0, "Select at least one institution."),
      need(nrow(data) > 0, "No demographic enrollment data matches the selected filters.")
    )

    data
  })

  # ------------------------------------------------------------
  # 12-month undergraduate/graduate reactive data
  # ------------------------------------------------------------

  e12_attendance_filtered <- reactive({
    req(
      input$e12_institution,
      input$e12_year_range,
      input$e12_attendance_student_level,
      input$e12_attendance_status
    )

    data <- e12_attendance_data |>
      e12_apply_shared_filters(
        institutions = input$e12_institution,
        year_range = input$e12_year_range,
        state_filter = input$e12_state,
        control_filter = input$e12_control,
        level_filter = input$e12_level
      ) |>
      filter(
        StudentLevel == input$e12_attendance_student_level,
        AttendanceStatus == input$e12_attendance_status
      )

    validate(
      need(length(input$e12_institution) > 0, "Select at least one institution."),
      need(nrow(data) > 0, "No undergraduate/graduate enrollment data matches the selected filters.")
    )

    data
  })

  e12_attendance_kpi_data <- reactive({
    req(input$e12_institution, input$e12_year_range, input$e12_attendance_student_level)

    e12_attendance_data |>
      e12_apply_shared_filters(
        institutions = input$e12_institution,
        year_range = input$e12_year_range,
        state_filter = input$e12_state,
        control_filter = input$e12_control,
        level_filter = input$e12_level
      ) |>
      filter(StudentLevel == input$e12_attendance_student_level)
  })

  # ------------------------------------------------------------
  # 12-month FTE reactive data
  # ------------------------------------------------------------

  e12_fte_filtered <- reactive({
    req(input$e12_institution, input$e12_year_range)

    data <- e12_fte_data |>
      e12_apply_shared_filters(
        institutions = input$e12_institution,
        year_range = input$e12_year_range,
        state_filter = input$e12_state,
        control_filter = input$e12_control,
        level_filter = input$e12_level
      )

    validate(
      need(length(input$e12_institution) > 0, "Select at least one institution."),
      need(nrow(data) > 0, "No FTE data matches the selected filters.")
    )

    data
  })

  # ------------------------------------------------------------
  # 12-month KPI outputs
  # ------------------------------------------------------------

  output$e12_demo_latest_enrollment <- renderText({
    e12_format_count(e12_latest_change_kpis(e12_demo_filtered(), "EnrollmentCount")$latest)
  })

  output$e12_demo_enrollment_change <- renderText({
    e12_format_count(e12_latest_change_kpis(e12_demo_filtered(), "EnrollmentCount")$change)
  })

  output$e12_demo_percentage_growth <- renderText({
    e12_format_percent(e12_latest_change_kpis(e12_demo_filtered(), "EnrollmentCount")$growth)
  })

  output$e12_demo_average_enrollment <- renderText({
    e12_format_count(e12_latest_change_kpis(e12_demo_filtered(), "EnrollmentCount")$average)
  })

  output$e12_attendance_latest_enrollment <- renderText({
    e12_format_count(e12_latest_change_kpis(e12_attendance_filtered(), "EnrollmentCount")$latest)
  })

  output$e12_attendance_full_time <- renderText({
    data <- e12_attendance_kpi_data()
    latest_year <- max(data$Year, na.rm = TRUE)

    data |>
      filter(Year == latest_year, AttendanceStatus == "Full-time") |>
      summarise(
        total = ifelse(
          all(is.na(EnrollmentCount)),
          NA_real_,
          sum(EnrollmentCount, na.rm = TRUE)
        ),
        .groups = "drop"
      ) |>
      pull(total) |>
      e12_format_count()
  })

  output$e12_attendance_part_time <- renderText({
    data <- e12_attendance_kpi_data()
    latest_year <- max(data$Year, na.rm = TRUE)

    data |>
      filter(Year == latest_year, AttendanceStatus == "Part-time") |>
      summarise(
        total = ifelse(
          all(is.na(EnrollmentCount)),
          NA_real_,
          sum(EnrollmentCount, na.rm = TRUE)
        ),
        .groups = "drop"
      ) |>
      pull(total) |>
      e12_format_count()
  })

  output$e12_attendance_full_time_pct <- renderText({
    data <- e12_attendance_kpi_data()
    latest_year <- max(data$Year, na.rm = TRUE)
    latest_data <- data |> filter(Year == latest_year)
    full_values <- latest_data$EnrollmentCount[latest_data$AttendanceStatus == "Full-time"]
    part_values <- latest_data$EnrollmentCount[latest_data$AttendanceStatus == "Part-time"]
    full_time <- ifelse(all(is.na(full_values)), NA_real_, sum(full_values, na.rm = TRUE))
    part_time <- ifelse(all(is.na(part_values)), NA_real_, sum(part_values, na.rm = TRUE))

    e12_format_percent(ifelse(full_time + part_time > 0, full_time / (full_time + part_time), NA_real_))
  })

  output$e12_fte_latest_total <- renderText({
    e12_format_count(e12_latest_change_kpis(e12_fte_filtered(), "TotalFTE")$latest)
  })

  output$e12_fte_growth <- renderText({
    e12_format_count(e12_latest_change_kpis(e12_fte_filtered(), "TotalFTE")$change)
  })

  output$e12_fte_average <- renderText({
    e12_format_count(e12_latest_change_kpis(e12_fte_filtered(), "TotalFTE")$average)
  })

  output$e12_fte_ratio <- renderText({
    data <- e12_fte_filtered()
    latest_year <- max(data$Year, na.rm = TRUE)

    data |>
      filter(Year == latest_year) |>
      summarise(
        total_headcount = ifelse(all(is.na(TotalHeadcount)), NA_real_, sum(TotalHeadcount, na.rm = TRUE)),
        total_fte = ifelse(all(is.na(TotalFTE)), NA_real_, sum(TotalFTE, na.rm = TRUE)),
        ratio = ifelse(total_headcount > 0, total_fte / total_headcount, NA_real_),
        .groups = "drop"
      ) |>
      pull(ratio) |>
      e12_format_percent()
  })

  # ------------------------------------------------------------
  # 12-month plot outputs
  # ------------------------------------------------------------

  output$e12_demo_plot <- renderPlot({
    data <- e12_demo_filtered()

    validate(
      need(any(!is.na(data$EnrollmentCount)), "Enrollment counts are missing for the selected demographic filters.")
    )

    ggplot(data, aes(x = Year, y = EnrollmentCount, color = University, group = UNITID)) +
      geom_line(linewidth = 1.05, alpha = 0.9) +
      geom_point(size = 2.4, alpha = 0.95) +
      scale_x_continuous(breaks = seq(input$e12_year_range[1], input$e12_year_range[2], by = 1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "12-month enrollment", color = "Institution") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })

  output$e12_attendance_plot <- renderPlot({
    data <- e12_attendance_filtered()

    validate(
      need(any(!is.na(data$EnrollmentCount)), "Enrollment counts are missing for the selected attendance filters.")
    )

    ggplot(data, aes(x = Year, y = EnrollmentCount, color = University, group = UNITID)) +
      geom_line(linewidth = 1.05, alpha = 0.9) +
      geom_point(size = 2.4, alpha = 0.95) +
      scale_x_continuous(breaks = seq(input$e12_year_range[1], input$e12_year_range[2], by = 1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "12-month enrollment", color = "Institution") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })

  output$e12_fte_trend <- renderPlot({
    data <- e12_fte_filtered()

    validate(
      need(any(!is.na(data$TotalFTE)), "Total FTE values are missing for the selected filters.")
    )

    ggplot(data, aes(x = Year, y = TotalFTE, color = University, group = UNITID)) +
      geom_line(linewidth = 1.05, alpha = 0.9) +
      geom_point(size = 2.4, alpha = 0.95) +
      scale_x_continuous(breaks = seq(input$e12_year_range[1], input$e12_year_range[2], by = 1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Total FTE", color = "Institution") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })

  output$e12_fte_headcount_plot <- renderPlot({
    data <- e12_fte_filtered() |>
      select(UNITID, University, Year, TotalHeadcount, TotalFTE) |>
      pivot_longer(
        cols = c(TotalHeadcount, TotalFTE),
        names_to = "Measure",
        values_to = "Value"
      ) |>
      mutate(
        Measure = recode(
          Measure,
          TotalHeadcount = "Total Headcount",
          TotalFTE = "Total FTE"
        ),
        Series = paste(University, Measure, sep = " - ")
      )

    validate(
      need(any(!is.na(data$Value)), "Headcount and FTE values are missing for the selected filters.")
    )

    ggplot(data, aes(x = Year, y = Value, color = Measure, linetype = Measure, group = Series)) +
      geom_line(linewidth = 1.05, alpha = 0.9) +
      geom_point(size = 2.2, alpha = 0.95) +
      facet_wrap(~ University, scales = "free_y") +
      scale_x_continuous(breaks = seq(input$e12_year_range[1], input$e12_year_range[2], by = 1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Count", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })

  output$e12_fte_institution_bar <- renderPlot({
    data <- e12_fte_filtered()
    latest_year <- max(data$Year, na.rm = TRUE)
    latest_data <- data |>
      filter(Year == latest_year) |>
      arrange(desc(TotalFTE))

    validate(
      need(any(!is.na(latest_data$TotalFTE)), "Total FTE values are missing for the latest selected year.")
    )

    ggplot(latest_data, aes(x = reorder(University, TotalFTE), y = TotalFTE)) +
      geom_col(fill = "#b31b1b", width = 0.7) +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Total FTE") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })

  # ------------------------------------------------------------
  # 12-month table outputs
  # ------------------------------------------------------------

  output$e12_demo_table <- renderDT({
    e12_demo_filtered() |>
      select(
        UNITID, Institution, Year, State, Control, InstitutionLevel,
        StudentLevel, Gender, Ethnicity, EnrollmentCount
      ) |>
      arrange(Institution, Year) |>
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE)
      ) |>
      formatCurrency("EnrollmentCount", currency = "", interval = 3, mark = ",", digits = 0)
  })

  output$e12_attendance_table <- renderDT({
    e12_attendance_filtered() |>
      select(
        UNITID, Institution, Year, State, Control, InstitutionLevel,
        StudentLevel, AttendanceStatus, EnrollmentCount
      ) |>
      arrange(Institution, Year) |>
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE)
      ) |>
      formatCurrency("EnrollmentCount", currency = "", interval = 3, mark = ",", digits = 0)
  })

  output$e12_fte_table <- renderDT({
    e12_fte_filtered() |>
      select(
        UNITID, Institution, Year, State, Control, InstitutionLevel,
        UndergraduateHeadcount, GraduateHeadcount, TotalHeadcount,
        UndergraduateFTE, GraduateFTE, TotalFTE, FTEtoHeadcountRatio
      ) |>
      arrange(Institution, Year) |>
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE)
      ) |>
      formatCurrency(
        c(
          "UndergraduateHeadcount", "GraduateHeadcount", "TotalHeadcount",
          "UndergraduateFTE", "GraduateFTE", "TotalFTE"
        ),
        currency = "",
        interval = 3,
        mark = ",",
        digits = 0
      ) |>
      formatRound("FTEtoHeadcountRatio", digits = 3)
  })
}

# ============================================================
# Graduation Rate Analytics server module
# ============================================================
gr_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rates_data <- gr_load_rate_data()
    demographic_data <- gr_load_demographic_data()
    cohort_years <- sort(unique(rates_data$CohortYear))
    default_range <- tail(cohort_years, min(10, length(cohort_years)))
    caldwell_id <- rates_data |> filter(Institution == "Caldwell University") |> slice(1) |> pull(UNITID)
    peer_choices <- rates_data |> filter(Institution %in% gr_peer_institutions) |> distinct(UNITID, Institution) |> arrange(Institution)
    main_choices <- rates_data |> distinct(UNITID, Institution) |> arrange(desc(Institution == "Caldwell University"), Institution)

    updateSelectInput(session, "main_institution", choices = stats::setNames(main_choices$UNITID, main_choices$Institution), selected = caldwell_id)
    updateSelectizeInput(session, "peer_institutions", choices = stats::setNames(peer_choices$UNITID, peer_choices$Institution), selected = peer_choices$UNITID, server = TRUE)
    updateSliderInput(session, "cohort_range", min = min(cohort_years), max = max(cohort_years), value = range(default_range), step = 1)

    observeEvent(input$select_all_peers, {
      updateSelectizeInput(session, "peer_institutions", selected = peer_choices$UNITID)
    })

    observeEvent(input$clear_all_peers, {
      updateSelectizeInput(session, "peer_institutions", selected = character(0))
    })

    selected_rate_key <- reactive({
      if (identical(input$rate_type, "all")) "rate_150" else input$rate_type
    })

    full_graduation_data <- reactive({
      rates_data
    })

    selected_main_institution <- reactive({
      req(input$main_institution)
      input$main_institution
    })

    selected_peers <- reactive({
      input$peer_institutions %||% character(0)
    })

    selected_cohort_range <- reactive({
      req(input$cohort_range)
      input$cohort_range
    })

    selected_rate_type <- reactive({
      req(input$rate_type)
      input$rate_type
    })

    base_selected_data <- reactive({
      req(selected_main_institution(), selected_cohort_range())
      ids <- unique(c(selected_main_institution(), selected_peers()))

      validate(need(length(ids) > 0, "Select a main institution or at least one peer institution."))

      full_graduation_data() |>
        filter(
          UNITID %in% ids,
          CohortYear >= selected_cohort_range()[1],
          CohortYear <= selected_cohort_range()[2]
        )
    })

    selected_metric_data <- reactive({
      gr_add_selected_rate(base_selected_data(), selected_rate_key())
    })

    latest_valid_selected_cohort <- reactive({
      caldwell_valid <- selected_metric_data() |>
        filter(UNITID == selected_main_institution(), !is.na(SelectedGraduationRate)) |>
        arrange(desc(CohortYear))

      validate(need(nrow(caldwell_valid) > 0, "No valid Caldwell graduation-rate data is available for the selected cohort range."))
      caldwell_valid$CohortYear[1]
    })

    observe({
      valid_years <- selected_metric_data() |>
        filter(UNITID == selected_main_institution(), !is.na(SelectedGraduationRate)) |>
        arrange(CohortYear) |>
        pull(CohortYear)

      selected <- if (length(valid_years) > 0) max(valid_years) else latest_valid_selected_cohort()
      updateSelectInput(session, "selected_cohort", choices = valid_years, selected = selected)
    })

    selected_cohort_year <- reactive({
      req(input$selected_cohort)
      as.numeric(input$selected_cohort)
    })

    institution_level_comparison_data <- reactive({
      selected_metric_data() |>
        filter(CohortYear == selected_cohort_year())
    })

    peer_weighted_average_data <- reactive({
      peer_data <- institution_level_comparison_data() |>
        filter(UNITID %in% selected_peers(), UNITID != selected_main_institution())

      gr_weighted_average(peer_data)
    })

    peer_median_data <- reactive({
      peer_data <- institution_level_comparison_data() |>
        filter(UNITID %in% selected_peers(), UNITID != selected_main_institution())

      gr_peer_median(peer_data)
    })

    ranking_data <- reactive({
      data <- institution_level_comparison_data()
      caldwell_rate <- data |> filter(UNITID == selected_main_institution()) |> pull(SelectedGraduationRate) |> dplyr::first()
      peer_average <- peer_weighted_average_data()$SelectedGraduationRate[1]

      data |>
        mutate(
          DifferenceFromCaldwell = SelectedGraduationRate - caldwell_rate,
          DifferenceFromPeerAverage = SelectedGraduationRate - peer_average
        ) |>
        filter(!is.na(SelectedGraduationRate)) |>
        arrange(desc(SelectedGraduationRate), Institution) |>
        mutate(Rank = min_rank(desc(SelectedGraduationRate)))
    })

    comparison_download_data <- reactive({
      ranking_data() |>
        select(
          UNITID, Institution, State, CohortYear, GRReportingYear, GR200ReportingYear,
          AdjustedCohort, SelectedCompleters, SelectedGraduationRate,
          DifferenceFromCaldwell, DifferenceFromPeerAverage, Rank, PeerGroup,
          IsCaldwell, DataAvailabilityStatus
        )
    })

    trend_data <- reactive({
      if (identical(selected_rate_type(), "all")) {
        data <- base_selected_data()

        if (length(selected_peers()) > 0) {
          data <- data |> filter(UNITID == selected_main_institution())
        }

        gr_full_long_rates(data)
      } else {
        selected_metric_data()
      }
    })

    cohort_outcome_data <- reactive({
      full_graduation_data() |>
        filter(
          UNITID == selected_main_institution(),
          CohortYear >= selected_cohort_range()[1],
          CohortYear <= selected_cohort_range()[2]
        )
    })

    peer_trend_weighted_data <- reactive({
      selected_metric_data() |>
        filter(UNITID %in% selected_peers(), UNITID != selected_main_institution()) |>
        group_by(CohortYear) |>
        group_modify(~ gr_weighted_average(.x)) |>
        ungroup() |>
        mutate(Institution = "Weighted peer average", UNITID = "peer_weighted", SelectedRateType = gr_rate_label(selected_rate_key()))
    })

    peer_trend_median_data <- reactive({
      selected_metric_data() |>
        filter(UNITID %in% selected_peers(), UNITID != selected_main_institution()) |>
        group_by(CohortYear) |>
        summarise(
          SelectedGraduationRate = {
            valid <- SelectedGraduationRate[!is.na(SelectedGraduationRate)]
            if (length(valid) == 0) NA_real_ else median(valid)
          },
          InstitutionsIncluded = n_distinct(UNITID[!is.na(SelectedGraduationRate)]),
          .groups = "drop"
        ) |>
        mutate(Institution = "Peer median", UNITID = "peer_median", SelectedRateType = gr_rate_label(selected_rate_key()))
    })

    caldwell_current <- reactive({
      institution_level_comparison_data() |> filter(UNITID == selected_main_institution()) |> slice(1)
    })

    caldwell_previous <- reactive({
      selected_metric_data() |>
        filter(UNITID == selected_main_institution(), CohortYear < selected_cohort_year(), !is.na(SelectedGraduationRate)) |>
        arrange(desc(CohortYear)) |>
        slice(1)
    })

    rank_over_time_data <- reactive({
      data <- selected_metric_data()

      purrr::map_dfr(sort(unique(data$CohortYear)), function(cohort_year) {
        cohort_data <- data |> filter(CohortYear == cohort_year)
        caldwell_row <- cohort_data |> filter(UNITID == selected_main_institution(), !is.na(SelectedGraduationRate))
        valid <- cohort_data |> filter(!is.na(SelectedGraduationRate))

        if (nrow(caldwell_row) == 0 || nrow(valid) == 0) {
          return(tibble::tibble())
        }

        ranked <- valid |> mutate(Rank = min_rank(desc(SelectedGraduationRate)))
        weighted <- gr_weighted_average(valid |> filter(UNITID != selected_main_institution()))

        ranked |>
          filter(UNITID == selected_main_institution()) |>
          transmute(
            CohortYear = cohort_year,
            CaldwellRate = SelectedGraduationRate,
            CaldwellRank = Rank,
            InstitutionsIncluded = n_distinct(valid$UNITID),
            PeerWeightedAverage = weighted$SelectedGraduationRate[1],
            DifferenceFromPeerAverage = SelectedGraduationRate - weighted$SelectedGraduationRate[1]
          )
      })
    })

    data_quality_warnings <- reactive({
      data <- selected_metric_data()
      warnings <- character(0)

      if (any(is.na(data$SelectedGraduationRate))) warnings <- c(warnings, "Some selected graduation-rate values are missing.")
      if (any(is.na(data$AdjustedCohort))) warnings <- c(warnings, "Some adjusted-cohort values are missing.")
      if (any(data$AdjustedCohort == 0, na.rm = TRUE)) warnings <- c(warnings, "Some adjusted cohorts equal zero.")
      if (any(data$Completers100 > data$AdjustedCohort | data$Completers150 > data$AdjustedCohort | data$Completers200 > data$AdjustedCohort, na.rm = TRUE)) warnings <- c(warnings, "Some completer counts are greater than adjusted cohort.")
      if (any(data$SelectedGraduationRate < 0 | data$SelectedGraduationRate > 100, na.rm = TRUE)) warnings <- c(warnings, "Some graduation rates are below 0% or above 100%.")
      if (any(data$Completers100 > data$Completers150, na.rm = TRUE)) warnings <- c(warnings, "Some rows have Completers100 greater than Completers150.")
      if (any(data$Completers150 > data$Completers200, na.rm = TRUE)) warnings <- c(warnings, "Some rows have Completers150 greater than Completers200.")
      if (nrow(data |> filter(UNITID == selected_main_institution(), is.na(SelectedGraduationRate))) > 0) warnings <- c(warnings, "Caldwell or selected-main-institution data is missing for at least one selected cohort.")
      if (peer_weighted_average_data()$InstitutionsIncluded[1] < 5) warnings <- c(warnings, "Fewer than five peers have valid data for the selected cohort.")
      if (any(is.na(data$GR200ReportingYear))) warnings <- c(warnings, "Some GR200 reporting-year values are missing.")
      duplicates <- data |> count(UNITID, CohortYear) |> filter(n > 1)
      if (nrow(duplicates) > 0) warnings <- c(warnings, "Duplicate institution and CohortYear records are present.")

      warnings
    })

    render_gr_plot <- function(output_id, plot_expr) {
      if (requireNamespace("plotly", quietly = TRUE)) {
        output[[output_id]] <- plotly::renderPlotly(plotly::ggplotly(plot_expr(), tooltip = "text") |> plotly::layout(hovermode = "closest"))
      } else {
        output[[output_id]] <- renderPlot(suppressWarnings(plot_expr()))
      }
    }

    output$kpi_caldwell_rate <- renderText(gr_format_percent(caldwell_current()$SelectedGraduationRate[1]))
    output$kpi_peer_weighted <- renderText(paste0(gr_format_percent(peer_weighted_average_data()$SelectedGraduationRate[1]), " (n=", peer_weighted_average_data()$InstitutionsIncluded[1], ")"))
    output$kpi_peer_median <- renderText(paste0(gr_format_percent(peer_median_data()$SelectedGraduationRate[1]), " (n=", peer_median_data()$InstitutionsIncluded[1], ")"))
    output$kpi_difference <- renderText(gr_format_pp(caldwell_current()$SelectedGraduationRate[1] - peer_weighted_average_data()$SelectedGraduationRate[1]))
    output$kpi_rank <- renderText({
      rank <- ranking_data() |> filter(UNITID == selected_main_institution()) |> pull(Rank) |> dplyr::first()
      ifelse(is.na(rank), "N/A", paste0(rank, " of ", nrow(ranking_data())))
    })
    output$kpi_adjusted_cohort <- renderText(gr_format_count(caldwell_current()$AdjustedCohort[1]))
    output$kpi_completers <- renderText(gr_format_count(caldwell_current()$SelectedCompleters[1]))
    output$kpi_change <- renderText({
      previous <- caldwell_previous()
      if (nrow(previous) == 0) return("N/A")
      gr_format_pp(caldwell_current()$SelectedGraduationRate[1] - previous$SelectedGraduationRate[1])
    })

    render_gr_plot("trend_plot", reactive({
      data <- trend_data() |>
        mutate(
          LineGroup = paste(UNITID, SelectedRateType),
          LineType = dplyr::case_when(
            UNITID == selected_main_institution() ~ "Main institution",
            grepl("peer_", UNITID) ~ Institution,
            TRUE ~ "Peer institution"
          ),
          text = paste0(
            "Institution: ", Institution,
            "<br>Entering cohort year: ", CohortYear,
            "<br>Rate type: ", SelectedRateType,
            "<br>Graduation rate: ", gr_format_percent(SelectedGraduationRate),
            "<br>Adjusted cohort: ", gr_format_count(AdjustedCohort),
            "<br>Selected completers: ", gr_format_count(SelectedCompleters),
            "<br>GR reporting year: ", GRReportingYear,
            "<br>GR200 reporting year: ", GR200ReportingYear,
            "<br>Data availability status: ", DataAvailabilityStatus
          )
        )

      comparison_lines <- bind_rows(peer_trend_weighted_data(), peer_trend_median_data()) |>
        mutate(
          LineGroup = UNITID,
          LineType = Institution,
          text = paste0(
            "Institution: ", Institution,
            "<br>Entering cohort year: ", CohortYear,
            "<br>Rate type: ", SelectedRateType,
            "<br>Graduation rate: ", gr_format_percent(SelectedGraduationRate),
            "<br>Institutions included: ", InstitutionsIncluded
          )
        )

      if (!identical(selected_rate_type(), "all") && input$comparison_mode %in% c("Weighted peer-group average", "Caldwell versus peers", "New Jersey peers")) {
        data <- bind_rows(data, comparison_lines |> filter(Institution == "Weighted peer average"))
      }
      if (!identical(selected_rate_type(), "all") && input$comparison_mode %in% c("Peer median", "Caldwell versus peers")) {
        data <- bind_rows(data, comparison_lines |> filter(Institution == "Peer median"))
      }

      validate(need(any(!is.na(data$SelectedGraduationRate)), "No graduation rates are available for the selected trend filters."))

      ggplot(data, aes(x = CohortYear, y = SelectedGraduationRate, color = Institution, linetype = SelectedRateType, group = LineGroup, text = text)) +
        geom_line(linewidth = ifelse(data$UNITID == selected_main_institution(), 1.35, 0.85), alpha = ifelse(data$UNITID == selected_main_institution(), 1, 0.62), na.rm = TRUE) +
        geom_point(aes(size = UNITID == selected_main_institution()), alpha = 0.95, na.rm = TRUE) +
        scale_size_manual(values = c(`TRUE` = 3.2, `FALSE` = 1.8), guide = "none") +
        scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
        scale_x_continuous(breaks = sort(unique(data$CohortYear))) +
        labs(x = "Entering cohort year", y = "Graduation rate", color = "Institution", linetype = "Rate type") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.background = element_rect(fill = "white", color = NA))
    }))

    render_gr_plot("outcomes_plot", reactive({
      data <- cohort_outcome_data()

      if (identical(input$outcome_display, "Rates")) {
        plot_data <- data |>
          select(CohortYear, GraduationRate100, GraduationRate150, GraduationRate200) |>
          pivot_longer(-CohortYear, names_to = "Measure", values_to = "Value") |>
          mutate(Measure = recode(Measure, GraduationRate100 = "100%", GraduationRate150 = "150%", GraduationRate200 = "200%"), text = paste0(Measure, ": ", gr_format_percent(Value)))

        ggplot(plot_data, aes(x = factor(CohortYear), y = Value, fill = Measure, text = text)) +
          geom_col(position = position_dodge(width = 0.72), width = 0.68, na.rm = TRUE) +
          scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
          labs(x = "Entering cohort year", y = "Graduation rate", fill = NULL) +
          theme_minimal(base_size = 13) +
          theme(legend.position = "bottom", panel.grid.minor = element_blank())
      } else {
        plot_data <- data |>
          select(CohortYear, AdjustedCohort, Completers100, Completers150, Completers200) |>
          pivot_longer(-CohortYear, names_to = "Measure", values_to = "Value") |>
          mutate(
            Measure = recode(Measure, AdjustedCohort = "Adjusted cohort", Completers100 = "Completers within 100%", Completers150 = "Completers within 150%", Completers200 = "Completers within 200%"),
            text = paste0(Measure, ": ", gr_format_count(Value))
          )

        ggplot(plot_data, aes(x = factor(CohortYear), y = Value, fill = Measure, text = text)) +
          geom_col(position = position_dodge(width = 0.72), width = 0.68, na.rm = TRUE) +
          scale_y_continuous(labels = scales::comma) +
          labs(x = "Entering cohort year", y = "Count", fill = NULL) +
          theme_minimal(base_size = 13) +
          theme(legend.position = "bottom", panel.grid.minor = element_blank())
      }
    }))

    render_gr_plot("ranking_plot", reactive({
      data <- ranking_data() |>
        mutate(
          LabelInstitution = reorder(paste0(Rank, ". ", Institution), SelectedGraduationRate),
          text = paste0(
            "Institution: ", Institution,
            "<br>State: ", State,
            "<br>CohortYear: ", CohortYear,
            "<br>Graduation rate: ", gr_format_percent(SelectedGraduationRate),
            "<br>Adjusted cohort: ", gr_format_count(AdjustedCohort),
            "<br>Selected completers: ", gr_format_count(SelectedCompleters),
            "<br>Difference from Caldwell: ", gr_format_pp(DifferenceFromCaldwell),
            "<br>Difference from peer average: ", gr_format_pp(DifferenceFromPeerAverage),
            "<br>Rank: ", Rank
          )
        )

      validate(need(nrow(data) > 0, "No valid institution rates are available for the selected cohort."))

      ggplot(data, aes(x = LabelInstitution, y = SelectedGraduationRate, fill = UNITID == selected_main_institution(), text = text)) +
        geom_col(width = 0.72, na.rm = TRUE) +
        geom_text(aes(label = gr_format_percent(SelectedGraduationRate)), hjust = -0.08, size = 3.2, na.rm = TRUE) +
        geom_hline(yintercept = peer_weighted_average_data()$SelectedGraduationRate[1], color = "#444444", linetype = "dashed") +
        geom_hline(yintercept = peer_median_data()$SelectedGraduationRate[1], color = "#777777", linetype = "dotted") +
        coord_flip() +
        scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
        scale_fill_manual(values = c(`TRUE` = "#b31b1b", `FALSE` = "#9ca3af"), guide = "none") +
        labs(x = NULL, y = "Graduation rate") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())
    }))

    output$missing_ranking_note <- renderUI({
      missing <- institution_level_comparison_data() |> filter(is.na(SelectedGraduationRate))
      if (nrow(missing) == 0) return(NULL)

      div(class = "gr-small", "Missing selected-rate data: ", paste(missing$Institution, collapse = ", "))
    })

    render_gr_plot("rank_over_time_plot", reactive({
      data <- rank_over_time_data() |>
        mutate(text = paste0(
          "CohortYear: ", CohortYear,
          "<br>Caldwell graduation rate: ", gr_format_percent(CaldwellRate),
          "<br>Caldwell rank: ", CaldwellRank,
          "<br>Institutions included: ", InstitutionsIncluded,
          "<br>Peer weighted average: ", gr_format_percent(PeerWeightedAverage),
          "<br>Difference from peer average: ", gr_format_pp(DifferenceFromPeerAverage)
        ))

      validate(need(nrow(data) > 0, "Caldwell cannot be ranked for the selected cohort range."))

      ggplot(data, aes(x = CohortYear, y = CaldwellRank, group = 1, text = text)) +
        geom_line(color = "#b31b1b", linewidth = 1.15) +
        geom_point(color = "#b31b1b", size = 3) +
        scale_y_reverse(breaks = sort(unique(data$CaldwellRank))) +
        scale_x_continuous(breaks = sort(unique(data$CohortYear))) +
        labs(x = "Entering cohort year", y = "Rank") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())
    }))

    render_gr_plot("distribution_plot", reactive({
      peer_data <- institution_level_comparison_data() |> filter(UNITID %in% selected_peers(), UNITID != selected_main_institution(), !is.na(SelectedGraduationRate))
      caldwell <- caldwell_current()

      validate(need(nrow(peer_data) > 0, "No valid peer rates are available for the selected cohort."))

      ggplot(peer_data, aes(x = "Peers", y = SelectedGraduationRate, text = paste0(Institution, ": ", gr_format_percent(SelectedGraduationRate)))) +
        geom_boxplot(width = 0.28, outlier.shape = NA, fill = "#f3f4f6", color = "#6b7280") +
        geom_jitter(width = 0.08, height = 0, color = "#6b7280", size = 2.4, alpha = 0.8) +
        geom_point(data = caldwell, aes(x = "Peers", y = SelectedGraduationRate, text = paste0(Institution, ": ", gr_format_percent(SelectedGraduationRate))), color = "#b31b1b", size = 4) +
        geom_text(data = caldwell, aes(x = "Peers", y = SelectedGraduationRate, label = "Caldwell"), nudge_x = 0.18, color = "#b31b1b", size = 3.3, na.rm = TRUE) +
        scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
        labs(x = NULL, y = "Graduation rate") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())
    }))

    output$distribution_stats <- renderTable({
      peer_values <- institution_level_comparison_data() |>
        filter(UNITID %in% selected_peers(), UNITID != selected_main_institution(), !is.na(SelectedGraduationRate)) |>
        pull(SelectedGraduationRate)
      caldwell_rate <- caldwell_current()$SelectedGraduationRate[1]

      validate(need(length(peer_values) > 0, "No valid peer values for summary statistics."))

      tibble::tibble(
        Metric = c("Peer minimum", "First quartile", "Peer median", "Third quartile", "Peer maximum", "Caldwell graduation rate"),
        Value = gr_format_percent(c(min(peer_values), quantile(peer_values, 0.25), median(peer_values), quantile(peer_values, 0.75), max(peer_values), caldwell_rate))
      )
    }, striped = TRUE, spacing = "s")

    render_gr_plot("heatmap_plot", reactive({
      data <- selected_metric_data() |>
        mutate(
          InstitutionOrdered = forcats::fct_reorder(Institution, DisplayOrder, .fun = min, .na_rm = TRUE),
          DisplayRate = ifelse(is.na(SelectedGraduationRate), -5, SelectedGraduationRate),
          text = paste0(
            "Institution: ", Institution,
            "<br>Cohort: ", CohortYear,
            "<br>Adjusted cohort: ", gr_format_count(AdjustedCohort),
            "<br>Completers: ", gr_format_count(SelectedCompleters),
            "<br>Rate: ", gr_format_percent(SelectedGraduationRate)
          )
        )

      ggplot(data, aes(x = factor(CohortYear), y = InstitutionOrdered, fill = DisplayRate, text = text)) +
        geom_tile(color = "white", linewidth = 0.35) +
        scale_fill_gradientn(colors = c("#d9d9d9", "#f7fbff", "#9ecae1", "#3182bd", "#08519c"), values = scales::rescale(c(-5, 0, 50, 75, 100)), limits = c(-5, 100), labels = function(x) ifelse(x < 0, "Missing", paste0(x, "%")), name = "Rate") +
        labs(x = "Entering cohort year", y = NULL) +
        theme_minimal(base_size = 13) +
        theme(panel.grid = element_blank())
    }))

    render_gr_plot("transfer_plot", reactive({
      data <- selected_metric_data() |>
        filter(UNITID == selected_main_institution()) |>
        mutate(
          OtherOrUnresolvedRate = 100 - GraduationRate150 - TransferOutRate,
          OtherOrUnresolvedRate = ifelse(OtherOrUnresolvedRate < 0, NA_real_, OtherOrUnresolvedRate)
        ) |>
        select(CohortYear, GraduationRate150, TransferOutRate, OtherOrUnresolvedRate) |>
        pivot_longer(-CohortYear, names_to = "Outcome", values_to = "Rate") |>
        mutate(
          Outcome = recode(Outcome, GraduationRate150 = "150% graduation rate", TransferOutRate = "Transfer-out rate", OtherOrUnresolvedRate = "Other or unresolved outcomes"),
          text = paste0(Outcome, ": ", gr_format_percent(Rate))
        )

      validate(need(any(!is.na(data$Rate)), "Transfer-out data are not available for the selected institution."))

      ggplot(data, aes(x = factor(CohortYear), y = Rate, fill = Outcome, text = text)) +
        geom_col(position = position_dodge(width = 0.72), width = 0.68, na.rm = TRUE) +
        scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
        labs(x = "Entering cohort year", y = "Rate", fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
    }))

    output$transfer_warning <- renderUI({
      invalid <- selected_metric_data() |>
        filter(UNITID == selected_main_institution()) |>
        mutate(OtherOrUnresolvedRate = 100 - GraduationRate150 - TransferOutRate) |>
        filter(OtherOrUnresolvedRate < 0)

      if (nrow(invalid) == 0) return(NULL)

      div(class = "gr-small", "Some other-or-unresolved outcome values are negative and were set to missing. Populations or reporting definitions may not be compatible.")
    })

    output$comparison_table <- renderDT({
      comparison_download_data() |>
        arrange(Rank, Institution) |>
        datatable(
          rownames = FALSE,
          filter = "top",
          extensions = "Buttons",
          options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, dom = "Bfrtip", buttons = c("csv"))
        ) |>
        formatRound(c("SelectedGraduationRate", "DifferenceFromCaldwell", "DifferenceFromPeerAverage"), digits = 1) |>
        formatCurrency(c("AdjustedCohort", "SelectedCompleters"), currency = "", interval = 3, mark = ",", digits = 0)
    })

    nj_comparison_data <- reactive({
      data <- selected_metric_data() |> filter(Institution %in% gr_nj_institutions, CohortYear == selected_cohort_year())
      caldwell <- data |> filter(Institution == "Caldwell University") |> slice(1)
      peers <- data |> filter(Institution != "Caldwell University")
      weighted <- gr_weighted_average(peers)
      median_data <- gr_peer_median(peers)
      ranked <- data |> filter(!is.na(SelectedGraduationRate)) |> arrange(desc(SelectedGraduationRate)) |> mutate(Rank = min_rank(desc(SelectedGraduationRate)))

      tibble::tibble(
        Metric = c("Caldwell rate", "New Jersey peer weighted average", "New Jersey peer median", "Caldwell difference from weighted average", "Caldwell rank among New Jersey institutions", "NJ peers included"),
        Value = c(
          gr_format_percent(caldwell$SelectedGraduationRate[1]),
          gr_format_percent(weighted$SelectedGraduationRate[1]),
          gr_format_percent(median_data$SelectedGraduationRate[1]),
          gr_format_pp(caldwell$SelectedGraduationRate[1] - weighted$SelectedGraduationRate[1]),
          paste0((ranked |> filter(Institution == "Caldwell University") |> pull(Rank) |> dplyr::first()) %||% NA_integer_, " of ", nrow(ranked)),
          weighted$InstitutionsIncluded[1]
        )
      )
    })

    output$nj_table <- renderDT({
      datatable(nj_comparison_data(), rownames = FALSE, options = list(dom = "t", pageLength = 10))
    })

    output$demographic_filters <- renderUI({
      if (is.null(demographic_data) || nrow(demographic_data) == 0) return(NULL)

      fluidRow(
        column(3, selectInput(session$ns("demo_gender"), "Gender", choices = c("All", sort(unique(demographic_data$Gender))), selected = "All")),
        column(3, selectInput(session$ns("demo_ethnicity"), "Race/ethnicity", choices = c("All", sort(unique(demographic_data$Ethnicity))), selected = "All")),
        column(3, numericInput(session$ns("small_cohort_threshold"), "Small-cohort warning threshold", value = 10, min = 1, step = 1))
      )
    })

    demographic_filtered <- reactive({
      validate(need(!is.null(demographic_data) && nrow(demographic_data) > 0, "Demographic graduation-rate data file is not available."))

      data <- demographic_data |>
        filter(
          UNITID == selected_main_institution(),
          CohortYear >= selected_cohort_range()[1],
          CohortYear <= selected_cohort_range()[2]
        ) |>
        gr_add_selected_rate(selected_rate_key())

      if (!is.null(input$demo_gender) && input$demo_gender != "All") data <- data |> filter(Gender == input$demo_gender)
      if (!is.null(input$demo_ethnicity) && input$demo_ethnicity != "All") data <- data |> filter(Ethnicity == input$demo_ethnicity)

      data |>
        mutate(SmallCohortFlag = ifelse(AdjustedCohort < (input$small_cohort_threshold %||% 10), "Small cohort - interpret cautiously", ""))
    })

    output$demographic_section <- renderUI({
      if (is.null(demographic_data) || nrow(demographic_data) == 0) return(NULL)

      tagList(
        fluidRow(
          column(6, div(class = "card", h2(class = "section-title", "Graduation Rate by Gender"), gr_plot_output(session$ns("demo_gender_plot"), "380px"))),
          column(6, div(class = "card", h2(class = "section-title", "Graduation Rate by Race/Ethnicity"), gr_plot_output(session$ns("demo_ethnicity_plot"), "380px")))
        ),
        fluidRow(
          column(6, div(class = "card", h2(class = "section-title", "Demographic Trend over CohortYear"), gr_plot_output(session$ns("demo_trend_plot"), "420px"))),
          column(6, div(class = "card", h2(class = "section-title", "Demographic Heatmap"), gr_plot_output(session$ns("demo_heatmap_plot"), "420px")))
        ),
        div(class = "card", h2(class = "section-title", "Graduation-Rate Gap Table"), DTOutput(session$ns("demo_gap_table")))
      )
    })

    if (!is.null(demographic_data) && nrow(demographic_data) > 0) {
      demographic_summary <- function(group_column) {
        demographic_filtered() |>
          filter(!is.na(.data[[group_column]])) |>
          group_by(.data[[group_column]], CohortYear) |>
          summarise(
            AdjustedCohort = sum(AdjustedCohort, na.rm = TRUE),
            SelectedCompleters = sum(SelectedCompleters, na.rm = TRUE),
            SelectedGraduationRate = ifelse(AdjustedCohort > 0, SelectedCompleters / AdjustedCohort * 100, NA_real_),
            SmallCohortFlag = ifelse(AdjustedCohort < (input$small_cohort_threshold %||% 10), "Small cohort - interpret cautiously", ""),
            .groups = "drop"
          ) |>
          rename(Group = 1)
      }

      render_gr_plot("demo_gender_plot", reactive({
        data <- demographic_summary("Gender") |> filter(CohortYear == selected_cohort_year())
        ggplot(data, aes(x = reorder(Group, SelectedGraduationRate), y = SelectedGraduationRate, text = paste0(Group, ": ", gr_format_percent(SelectedGraduationRate), "<br>", SmallCohortFlag))) +
          geom_col(fill = "#b31b1b", na.rm = TRUE) +
          coord_flip() +
          scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
          labs(x = NULL, y = "Graduation rate") +
          theme_minimal(base_size = 13)
      }))

      render_gr_plot("demo_ethnicity_plot", reactive({
        data <- demographic_summary("Ethnicity") |> filter(CohortYear == selected_cohort_year())
        ggplot(data, aes(x = reorder(Group, SelectedGraduationRate), y = SelectedGraduationRate, text = paste0(Group, ": ", gr_format_percent(SelectedGraduationRate), "<br>", SmallCohortFlag))) +
          geom_col(fill = "#6b7280", na.rm = TRUE) +
          coord_flip() +
          scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
          labs(x = NULL, y = "Graduation rate") +
          theme_minimal(base_size = 13)
      }))

      render_gr_plot("demo_trend_plot", reactive({
        data <- demographic_filtered() |> mutate(Group = paste(Gender, Ethnicity, sep = " - "))
        ggplot(data, aes(x = CohortYear, y = SelectedGraduationRate, color = Group, group = Group, text = paste0(Group, ": ", gr_format_percent(SelectedGraduationRate), "<br>", SmallCohortFlag))) +
          geom_line(na.rm = TRUE) +
          geom_point(na.rm = TRUE) +
          scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
          labs(x = "Entering cohort year", y = "Graduation rate", color = NULL) +
          theme_minimal(base_size = 13) +
          theme(legend.position = "bottom")
      }))

      render_gr_plot("demo_heatmap_plot", reactive({
        data <- demographic_filtered() |> mutate(Group = paste(Gender, Ethnicity, sep = " - "))
        ggplot(data, aes(x = factor(CohortYear), y = Group, fill = SelectedGraduationRate, text = paste0(Group, "<br>Cohort: ", CohortYear, "<br>Rate: ", gr_format_percent(SelectedGraduationRate), "<br>", SmallCohortFlag))) +
          geom_tile(color = "white") +
          scale_fill_gradient(low = "#f7fbff", high = "#08519c", limits = c(0, 100), na.value = "#d9d9d9", labels = function(x) paste0(x, "%")) +
          labs(x = "Entering cohort year", y = NULL, fill = "Rate") +
          theme_minimal(base_size = 13)
      }))

      output$demo_gap_table <- renderDT({
        data <- demographic_filtered() |>
          filter(CohortYear == selected_cohort_year()) |>
          mutate(
            DemographicGroup = paste(Gender, Ethnicity, sep = " - "),
            DifferenceFromInstitutionAverage = SelectedGraduationRate - mean(SelectedGraduationRate, na.rm = TRUE)
          ) |>
          select(DemographicGroup, CohortYear, SelectedRateType, SelectedGraduationRate, AdjustedCohort, SelectedCompleters, DifferenceFromInstitutionAverage, SmallCohortFlag)

        datatable(data, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE)) |>
          formatRound(c("SelectedGraduationRate", "DifferenceFromInstitutionAverage"), digits = 1) |>
          formatCurrency(c("AdjustedCohort", "SelectedCompleters"), currency = "", interval = 3, mark = ",", digits = 0)
      })
    }

    output$insights_panel <- renderUI({
      rank_row <- ranking_data() |> filter(UNITID == selected_main_institution()) |> slice(1)
      highest <- ranking_data() |> arrange(desc(SelectedGraduationRate), Institution) |> slice(1)
      lowest <- ranking_data() |> arrange(SelectedGraduationRate, Institution) |> slice(1)
      previous <- caldwell_previous()
      change <- if (nrow(previous) == 0) NA_real_ else caldwell_current()$SelectedGraduationRate[1] - previous$SelectedGraduationRate[1]
      trend_changes <- selected_metric_data() |>
        filter(!is.na(SelectedGraduationRate)) |>
        arrange(UNITID, CohortYear) |>
        group_by(UNITID, Institution) |>
        summarise(Change = last(SelectedGraduationRate) - first(SelectedGraduationRate), .groups = "drop")
      largest_increase <- trend_changes |> arrange(desc(Change)) |> slice(1)
      largest_decrease <- trend_changes |> arrange(Change) |> slice(1)
      warning_text <- data_quality_warnings()

      tagList(
        tags$p(
          paste0(
            caldwell_current()$Institution[1], "'s ", gr_rate_label(selected_rate_key()), " for the ",
            selected_cohort_year(), " entering cohort was ", gr_format_percent(caldwell_current()$SelectedGraduationRate[1]), ". ",
            "This was ", gr_format_pp(caldwell_current()$SelectedGraduationRate[1] - peer_weighted_average_data()$SelectedGraduationRate[1]),
            " relative to the weighted peer average. ",
            caldwell_current()$Institution[1], " ranked ", rank_row$Rank[1], " among ", nrow(ranking_data()), " institutions with valid data."
          )
        ),
        tags$ul(
          tags$li("Change from previous available cohort: ", gr_format_pp(change), ifelse(is.na(change), "", ifelse(change >= 0, " increased.", " decreased."))),
          tags$li("Peer weighted average: ", gr_format_percent(peer_weighted_average_data()$SelectedGraduationRate[1])),
          tags$li("Peer median: ", gr_format_percent(peer_median_data()$SelectedGraduationRate[1])),
          tags$li("Highest-rate institution: ", highest$Institution[1], " (", gr_format_percent(highest$SelectedGraduationRate[1]), ")"),
          tags$li("Lowest-rate institution: ", lowest$Institution[1], " (", gr_format_percent(lowest$SelectedGraduationRate[1]), ")"),
          tags$li("Largest increase across selected range: ", largest_increase$Institution[1], " (", gr_format_pp(largest_increase$Change[1]), ")"),
          tags$li("Largest decrease across selected range: ", largest_decrease$Institution[1], " (", gr_format_pp(largest_decrease$Change[1]), ")"),
          tags$li("Peers with valid data: ", peer_weighted_average_data()$InstitutionsIncluded[1])
        ),
        if (length(warning_text) > 0) tags$p("Warnings: ", paste(warning_text, collapse = " "))
      )
    })

    output$data_quality_warnings <- renderUI({
      warnings <- data_quality_warnings()

      if (length(warnings) == 0) {
        return(tags$p("No data-quality warnings for the current selection."))
      }

      tags$ul(class = "gr-warning-list", lapply(warnings, tags$li))
    })

    output$download_demographic_button <- renderUI({
      if (is.null(demographic_data) || nrow(demographic_data) == 0) return(NULL)
      downloadButton(session$ns("download_demographic"), "Demographic data")
    })

    output$download_full <- downloadHandler(
      filename = function() "graduation_rate_full.csv",
      content = function(file) readr::write_csv(full_graduation_data(), file)
    )
    output$download_trend <- downloadHandler(
      filename = function() "graduation_rate_filtered_trend.csv",
      content = function(file) readr::write_csv(trend_data(), file)
    )
    output$download_ranking <- downloadHandler(
      filename = function() "graduation_rate_selected_cohort_ranking.csv",
      content = function(file) readr::write_csv(comparison_download_data(), file)
    )
    output$download_caldwell_trend <- downloadHandler(
      filename = function() "graduation_rate_caldwell_trend.csv",
      content = function(file) readr::write_csv(selected_metric_data() |> filter(UNITID == selected_main_institution()), file)
    )
    output$download_weighted <- downloadHandler(
      filename = function() "graduation_rate_peer_weighted_average.csv",
      content = function(file) readr::write_csv(peer_trend_weighted_data(), file)
    )
    output$download_summary <- downloadHandler(
      filename = function() "graduation_rate_peer_summary_statistics.csv",
      content = function(file) {
        summary <- selected_metric_data() |>
          filter(UNITID %in% selected_peers(), UNITID != selected_main_institution(), !is.na(SelectedGraduationRate)) |>
          group_by(CohortYear) |>
          summarise(
            PeerMinimum = min(SelectedGraduationRate),
            FirstQuartile = quantile(SelectedGraduationRate, 0.25),
            PeerMedian = median(SelectedGraduationRate),
            ThirdQuartile = quantile(SelectedGraduationRate, 0.75),
            PeerMaximum = max(SelectedGraduationRate),
            PeersIncluded = n_distinct(UNITID),
            .groups = "drop"
          )
        readr::write_csv(summary, file)
      }
    )
    output$download_nj <- downloadHandler(
      filename = function() "graduation_rate_new_jersey_comparison.csv",
      content = function(file) readr::write_csv(nj_comparison_data(), file)
    )
    output$download_demographic <- downloadHandler(
      filename = function() "graduation_rate_demographic_data.csv",
      content = function(file) readr::write_csv(demographic_filtered(), file)
    )
  })
}

# ============================================================
# Institutional Efficiency server module
# ============================================================
ie_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    efficiency_data <- if (exists("dashboard_data", inherits = TRUE) && "efficiency_data" %in% names(dashboard_data)) {
      dashboard_data$efficiency_data
    } else {
      ie_load_data()
    }

    missing_required <- ie_data_missing_required(efficiency_data)
    available_years <- sort(unique(efficiency_data$Year[!is.na(efficiency_data$Year)]))
    default_years <- tail(available_years, min(10, length(available_years)))
    latest_institutions <- efficiency_data |>
      arrange(UNITID, desc(Year), Institution) |>
      group_by(UNITID) |>
      slice(1) |>
      ungroup() |>
      arrange(desc(Institution == "Caldwell University"), Institution)
    peer_choices <- latest_institutions |> filter(!IsCaldwell) |> arrange(Institution)
    caldwell_id <- latest_institutions |> filter(IsCaldwell) |> slice(1) |> pull(UNITID)

    output$data_validation_warning <- renderUI({
      if (length(missing_required) == 0) return(NULL)
      div(
        class = "card ie-note",
        h2(class = "section-title", "Institutional Efficiency Data Warning"),
        p("The Institutional Efficiency CSV is missing required column(s): ", paste(missing_required, collapse = ", "), ". The tab will show available content where possible instead of stopping the dashboard.")
      )
    })

    updateSelectizeInput(session, "primary_institution", choices = stats::setNames(latest_institutions$UNITID, latest_institutions$Institution), selected = caldwell_id, server = TRUE)
    updateSelectizeInput(session, "comparison_institutions", choices = stats::setNames(peer_choices$UNITID, peer_choices$Institution), selected = peer_choices$UNITID, server = TRUE)
    updateSliderInput(session, "year_range", min = min(available_years), max = max(available_years), value = range(default_years), step = 1)

    observe({
      req(input$primary_institution)
      ids <- unique(c(input$primary_institution, input$comparison_institutions %||% character(0)))
      year <- ie_latest_common_year(efficiency_data, ids, "DegreesPer100FTE", min_coverage = 2)
      updateSelectInput(session, "comparison_year", choices = available_years, selected = year)
    })

    observeEvent(input$select_all_peers, {
      updateSelectizeInput(session, "comparison_institutions", selected = peer_choices$UNITID, server = TRUE)
    })

    observeEvent(input$clear_peers, {
      updateSelectizeInput(session, "comparison_institutions", selected = character(0), server = TRUE)
    })

    observeEvent(input$caldwell_only, {
      updateSelectizeInput(session, "primary_institution", selected = caldwell_id, server = TRUE)
      updateSelectizeInput(session, "comparison_institutions", selected = character(0), server = TRUE)
    })

    selected_ids <- reactive({
      req(input$primary_institution)
      unique(c(input$primary_institution, input$comparison_institutions %||% character(0)))
    })

    peer_ids <- reactive({
      req(input$primary_institution)
      setdiff(input$comparison_institutions %||% character(0), input$primary_institution)
    })

    selected_data <- reactive({
      req(input$year_range)
      validate(need(length(selected_ids()) > 0, "Select at least one institution."))
      ie_selected_data(efficiency_data, selected_ids(), input$year_range)
    })

    selected_year_data <- reactive({
      req(input$comparison_year)
      selected_data() |> filter(Year == as.integer(input$comparison_year))
    })

    metric_peer_stats <- function(metric) {
      ie_peer_stats(selected_data(), input$primary_institution, peer_ids(), metric)
    }

    current_ranking <- function(metric) {
      ie_ranking_data(
        selected_data(),
        selected_ids(),
        input$primary_institution,
        metric,
        input$ranking_method,
        as.integer(input$comparison_year)
      )
    }

    render_kpi <- function(output_id, metric) {
      output[[output_id]] <- renderText({
        ie_kpi_text(selected_data(), input$primary_institution, peer_ids(), metric)
      })
    }

    render_kpi("overview_kpi_degrees_fte", "DegreesPer100FTE")
    render_kpi("overview_kpi_instruction_degree", "InstructionExpensesPerDegree")
    render_kpi("overview_kpi_core_degree", "CoreExpensesPerDegree")
    render_kpi("overview_kpi_core_revenue_fte", "CoreRevenuePerFTE")
    render_kpi("overview_kpi_degrees_staff", "DegreesPerInstructionalStaffFTE")
    render_kpi("overview_kpi_staff_fte", "StaffHeadcountPer100FTE")

    render_kpi("degree_kpi_total", "TotalDegreesAwarded")
    render_kpi("degree_kpi_bachelors", "BachelorsDegrees")
    render_kpi("degree_kpi_masters", "MastersDegrees")
    render_kpi("degree_kpi_doctoral", "DoctoralDegrees")
    render_kpi("degree_kpi_per_fte", "DegreesPer100FTE")
    render_kpi("degree_kpi_per_staff", "DegreesPerInstructionalStaffFTE")

    render_kpi("revenue_kpi_core", "TotalCoreRevenue")
    render_kpi("revenue_kpi_tuition", "TuitionAndFeeRevenue")
    render_kpi("revenue_kpi_core_fte", "CoreRevenuePerFTE")
    render_kpi("revenue_kpi_tuition_fte", "TuitionRevenuePerFTE")
    render_kpi("revenue_kpi_dependency", "TuitionDependency")
    output$revenue_kpi_peer_dependency <- renderText({
      latest <- ie_latest_primary_row(selected_data(), input$primary_institution, "TuitionDependency")
      if (nrow(latest) == 0) return("Not available")
      peer <- metric_peer_stats("TuitionDependency") |> filter(Year == latest$Year[1]) |> slice(1)
      if (nrow(peer) == 0) return("Not available")
      paste0(ie_format_value(peer$PeerMedian[1], "TuitionDependency"), "\nPeers: ", peer$ValidPeers[1], "\nYear: ", peer$Year[1])
    })

    render_kpi("staff_kpi_ft_instruction", "FullTimeInstructionalStaff")
    render_kpi("staff_kpi_pt_instruction", "PartTimeInstructionalStaff")
    render_kpi("staff_kpi_total", "TotalStaffHeadcount")
    render_kpi("staff_kpi_per_fte", "StaffHeadcountPer100FTE")
    render_kpi("staff_kpi_degrees_ft", "DegreesPerFullTimeInstructionalStaff")
    output$staff_kpi_degrees_fte <- renderText({
      if (!"InstructionalStaffFTE" %in% names(selected_data()) || all(is.na(selected_data()$InstructionalStaffFTE))) {
        return("Staff FTE data not available")
      }
      ie_kpi_text(selected_data(), input$primary_institution, peer_ids(), "DegreesPerInstructionalStaffFTE")
    })

    ie_render_plot(output, "overview_trend_plot", reactive({
      req(input$overview_metric)
      ie_metric_line_plot(selected_data(), metric_peer_stats(input$overview_metric), input$primary_institution, input$overview_metric)
    }))

    ie_render_plot(output, "overview_bar_plot", reactive({
      req(input$overview_bar_metric)
      primary <- ie_valid_metric_data(selected_data(), input$overview_bar_metric) |>
        filter(UNITID == input$primary_institution, Year == as.integer(input$comparison_year))
      peer <- metric_peer_stats(input$overview_bar_metric) |>
        filter(Year == as.integer(input$comparison_year)) |>
        slice(1)
      ie_bar_compare_plot(primary, peer, input$overview_bar_metric)
    }))

    output$overview_summary <- renderUI({
      req(input$overview_metric)
      latest <- ie_latest_primary_row(selected_data(), input$primary_institution, input$overview_metric)
      if (nrow(latest) == 0) return(tags$p("Not available."))
      previous <- ie_previous_primary_row(selected_data(), input$primary_institution, input$overview_metric, latest$Year[1])
      peer <- metric_peer_stats(input$overview_metric) |> filter(Year == latest$Year[1]) |> slice(1)
      primary_history <- ie_valid_metric_data(selected_data(), input$overview_metric) |> filter(UNITID == input$primary_institution)
      high <- primary_history |> slice_max(MetricValue, n = 1, with_ties = FALSE)
      low <- primary_history |> slice_min(MetricValue, n = 1, with_ties = FALSE)
      sentences <- c(
        paste0(latest$Institution[1], " reported ", ie_format_value(latest$MetricValue[1], input$overview_metric), " for ", ie_metric_label(input$overview_metric), " in ", latest$Year[1], ".")
      )
      if (nrow(previous) > 0) {
        sentences <- c(sentences, paste0("The change from the previous reporting year was ", ie_format_change(latest$MetricValue[1] - previous$MetricValue[1], input$overview_metric), "."))
      }
      if (nrow(peer) > 0 && !is.na(peer$PeerMedian[1])) {
        sentences <- c(sentences, paste0("The difference from the selected peer median was ", ie_format_change(latest$MetricValue[1] - peer$PeerMedian[1], input$overview_metric), " with ", peer$ValidPeers[1], " valid peers reporting."))
      }
      if (nrow(high) > 0 && nrow(low) > 0) {
        sentences <- c(sentences, paste0("Across the selected years, the highest value was ", ie_format_value(high$MetricValue[1], input$overview_metric), " in ", high$Year[1], " and the lowest was ", ie_format_value(low$MetricValue[1], input$overview_metric), " in ", low$Year[1], ". ", nrow(primary_history), " valid years are available."))
      }
      if (!is.na(latest$FinanceParentWarning[1]) && nzchar(latest$FinanceParentWarning[1])) sentences <- c(sentences, latest$FinanceParentWarning[1])
      tags$p(paste(sentences, collapse = " "))
    })

    ie_render_plot(output, "degree_trend_plot", reactive({
      ie_metric_line_plot(selected_data(), metric_peer_stats(input$degree_metric), input$primary_institution, input$degree_metric)
    }))

    ie_render_plot(output, "degree_composition_plot", reactive({
      if (input$degree_composition_mode == "Primary over time") {
        data <- selected_data() |> filter(UNITID == input$primary_institution)
      } else {
        data <- selected_year_data()
      }
      plot_data <- data |>
        select(UNITID, Institution, Year, BachelorsDegrees, MastersDegrees, DoctoralDegrees) |>
        tidyr::pivot_longer(c(BachelorsDegrees, MastersDegrees, DoctoralDegrees), names_to = "AwardLevel", values_to = "Degrees") |>
        mutate(
          AwardLevel = dplyr::recode(AwardLevel, BachelorsDegrees = "Bachelor's", MastersDegrees = "Master's", DoctoralDegrees = "Doctoral"),
          Axis = if (input$degree_composition_mode == "Primary over time") as.character(Year) else Institution,
          text = paste0("Institution: ", Institution, "<br>Year: ", Year, "<br>Award level: ", AwardLevel, "<br>Degrees: ", ie_format_value(Degrees, "TotalDegreesAwarded"))
        ) |>
        filter(!is.na(Degrees))
      validate(need(nrow(plot_data) > 0, "No degree-composition data are available."))
      ggplot(plot_data, aes(x = Axis, y = Degrees, fill = AwardLevel, text = text)) +
        geom_col(na.rm = TRUE) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = NULL, y = "Degrees awarded", fill = "Award level") +
        ie_base_theme() +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))
    }))

    ie_render_plot(output, "degree_scatter_plot", reactive({
      ie_scatter_plot(selected_data(), "TotalFTE", "TotalDegreesAwarded", input$primary_institution, as.integer(input$comparison_year), input$show_trend_line)
    }))

    ie_render_plot(output, "degree_ranking_plot", reactive({
      ie_ranking_plot(current_ranking(input$degree_rank_metric), input$degree_rank_metric, "Rank 1 indicates the highest value, not automatically the best institution.")
    }))

    output$degree_summary <- renderUI({
      latest <- ie_latest_primary_row(selected_data(), input$primary_institution, "DegreesPer100FTE")
      if (nrow(latest) == 0) return(tags$p("Not available."))
      peer <- metric_peer_stats("DegreesPer100FTE") |> filter(Year == latest$Year[1]) |> slice(1)
      text <- paste0(latest$Institution[1], " awarded ", ie_format_value(latest$MetricValue[1], "DegreesPer100FTE"), " degrees per 100 FTE in ", latest$Year[1])
      if (nrow(peer) > 0) text <- paste0(text, ", compared with a selected peer median of ", ie_format_value(peer$PeerMedian[1], "DegreesPer100FTE"), ".")
      tags$p(text)
    })

    ie_render_plot(output, "expense_trend_plot", reactive({
      ie_metric_line_plot(selected_data(), metric_peer_stats(input$expense_metric), input$primary_institution, input$expense_metric)
    }))

    ie_render_plot(output, "expense_comparison_plot", reactive({
      metric <- if (input$expense_basis == "Per degree") "InstructionExpensesPerDegree" else "InstructionExpensesPerFTE"
      ie_ranking_plot(current_ranking(metric), metric, "Lower spending does not automatically indicate greater efficiency or educational quality.")
    }))

    output$expense_composition_warning <- renderUI({
      invalid <- selected_data() |>
        filter(
          !is.na(OtherCoreExpenses) & OtherCoreExpenses < 0 |
            InstructionShareOfCoreExpenses + StudentServicesShareOfCoreExpenses +
              AcademicSupportShareOfCoreExpenses + InstitutionalSupportShareOfCoreExpenses +
              OtherCoreExpensesShare > 100.1
        )
      if (nrow(invalid) == 0) return(NULL)
      p(class = "ie-small", "Some records have incompatible expense shares or negative other core expenses and are excluded from the composition chart.")
    })

    ie_render_plot(output, "expense_composition_plot", reactive({
      data <- selected_year_data() |>
        mutate(ShareTotal = InstructionShareOfCoreExpenses + StudentServicesShareOfCoreExpenses + AcademicSupportShareOfCoreExpenses + InstitutionalSupportShareOfCoreExpenses + OtherCoreExpensesShare) |>
        filter(is.na(OtherCoreExpenses) | OtherCoreExpenses >= 0, ShareTotal <= 100.1)
      plot_data <- data |>
        select(Institution, Year, InstructionShareOfCoreExpenses, StudentServicesShareOfCoreExpenses, AcademicSupportShareOfCoreExpenses, InstitutionalSupportShareOfCoreExpenses, OtherCoreExpensesShare) |>
        tidyr::pivot_longer(-c(Institution, Year), names_to = "Category", values_to = "Share") |>
        mutate(
          Category = dplyr::recode(Category, InstructionShareOfCoreExpenses = "Instruction", StudentServicesShareOfCoreExpenses = "Student services", AcademicSupportShareOfCoreExpenses = "Academic support", InstitutionalSupportShareOfCoreExpenses = "Institutional support", OtherCoreExpensesShare = "Other core expenses"),
          text = paste0("Institution: ", Institution, "<br>Year: ", Year, "<br>Category: ", Category, "<br>Share: ", ie_format_value(Share, "TuitionDependency"))
        ) |>
        filter(!is.na(Share), Share >= 0)
      validate(need(nrow(plot_data) > 0, "No compatible expense-composition records are available."))
      ggplot(plot_data, aes(x = Institution, y = Share, fill = Category, text = text)) +
        geom_col(position = "fill", na.rm = TRUE) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = NULL, y = "Share of total core expenses", fill = "Category") +
        ie_base_theme() +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))
    }))

    expense_table_data <- reactive({
      selected_data() |>
        select(Institution, Year, InstructionExpenses, TotalCoreExpenses, InstructionExpensesPerDegree, CoreExpensesPerDegree, InstructionExpensesPerFTE, StudentServicesExpensesPerFTE, AcademicSupportExpensesPerFTE, InstitutionalSupportExpensesPerFTE) |>
        arrange(Institution, Year)
    })

    output$expense_table <- renderDT({
      datatable(expense_table_data(), rownames = FALSE, filter = "top", extensions = "Buttons", options = list(pageLength = 12, scrollX = TRUE, dom = "Bfrtip", buttons = c("csv"))) |>
        formatCurrency(c("InstructionExpenses", "TotalCoreExpenses", "InstructionExpensesPerDegree", "CoreExpensesPerDegree", "InstructionExpensesPerFTE", "StudentServicesExpensesPerFTE", "AcademicSupportExpensesPerFTE", "InstitutionalSupportExpensesPerFTE"), currency = "$", digits = 0)
    })

    output$download_expense <- downloadHandler(
      filename = function() "institutional_efficiency_expenses_filtered.csv",
      content = function(file) readr::write_csv(expense_table_data(), file, na = "")
    )

    output$expense_summary <- renderUI({
      latest <- ie_latest_primary_row(selected_data(), input$primary_institution, input$expense_metric)
      if (nrow(latest) == 0) return(tags$p("Not available."))
      previous <- ie_previous_primary_row(selected_data(), input$primary_institution, input$expense_metric, latest$Year[1])
      sentence <- paste0(latest$Institution[1], " reported ", ie_format_value(latest$MetricValue[1], input$expense_metric), " for ", ie_metric_label(input$expense_metric), " in ", latest$Year[1], ".")
      if (nrow(previous) > 0) sentence <- paste0(sentence, " The change from the previous reporting year was ", ie_format_change(latest$MetricValue[1] - previous$MetricValue[1], input$expense_metric), ".")
      tags$p(sentence)
    })

    ie_render_plot(output, "revenue_trend_plot", reactive({
      ie_metric_line_plot(selected_data(), metric_peer_stats(input$revenue_metric), input$primary_institution, input$revenue_metric)
    }))

    ie_render_plot(output, "tuition_dependency_plot", reactive({
      ie_ranking_plot(current_ranking("TuitionDependency"), "TuitionDependency", "Tuition dependency is not automatically good or bad.")
    }))

    ie_render_plot(output, "revenue_enrollment_scatter", reactive({
      ie_scatter_plot(selected_data(), "TotalFTE", "TotalCoreRevenue", input$primary_institution, as.integer(input$comparison_year), input$show_trend_line)
    }))

    ie_render_plot(output, "tuition_degrees_scatter", reactive({
      ie_scatter_plot(selected_data(), "TotalDegreesAwarded", "TuitionAndFeeRevenue", input$primary_institution, as.integer(input$comparison_year), input$show_trend_line)
    }))

    output$revenue_summary <- renderUI({
      latest <- ie_latest_primary_row(selected_data(), input$primary_institution, "TuitionDependency")
      if (nrow(latest) == 0) return(tags$p("Not available."))
      tags$p(paste0("Tuition and fees represented ", ie_format_value(latest$MetricValue[1], "TuitionDependency"), " of ", latest$Institution[1], "'s core revenue in ", latest$Year[1], ". This chart shows relationships only and does not make causal claims."))
    })

    ie_render_plot(output, "staff_trend_plot", reactive({
      ie_metric_line_plot(selected_data(), metric_peer_stats(input$staff_metric), input$primary_institution, input$staff_metric)
    }))

    ie_render_plot(output, "staff_composition_plot", reactive({
      plot_data <- selected_year_data() |>
        mutate(
          OtherFullTimeStaff = dplyr::if_else(
            !is.na(FullTimeStaff) & !is.na(FullTimeInstructionalStaff),
            pmax(FullTimeStaff - FullTimeInstructionalStaff, 0),
            NA_real_
          ),
          OtherPartTimeStaff = dplyr::if_else(
            !is.na(PartTimeStaff) & !is.na(PartTimeInstructionalStaff),
            pmax(PartTimeStaff - PartTimeInstructionalStaff, 0),
            NA_real_
          )
        ) |>
        select(Institution, Year, FullTimeInstructionalStaff, PartTimeInstructionalStaff, OtherFullTimeStaff, OtherPartTimeStaff) |>
        tidyr::pivot_longer(-c(Institution, Year), names_to = "StaffGroup", values_to = "Headcount") |>
        mutate(
          StaffGroup = dplyr::recode(StaffGroup, FullTimeInstructionalStaff = "Full-time instructional staff", PartTimeInstructionalStaff = "Part-time instructional staff", OtherFullTimeStaff = "Other full-time staff", OtherPartTimeStaff = "Other part-time staff"),
          text = paste0("Institution: ", Institution, "<br>Year: ", Year, "<br>Staff group: ", StaffGroup, "<br>Headcount: ", ie_format_value(Headcount, "TotalStaffHeadcount"))
        ) |>
        filter(!is.na(Headcount))
      validate(need(nrow(plot_data) > 0, "No staffing-composition data are available."))
      ggplot(plot_data, aes(x = Institution, y = Headcount, fill = StaffGroup, text = text)) +
        geom_col(na.rm = TRUE) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = NULL, y = "Staff headcount", fill = "Staff group") +
        ie_base_theme() +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))
    }))

    ie_render_plot(output, "staff_index_plot", reactive({
      base_data <- selected_data() |>
        filter(UNITID == input$primary_institution) |>
        select(Year, TotalFTE, FullTimeInstructionalStaff, TotalStaffHeadcount) |>
        tidyr::pivot_longer(-Year, names_to = "Measure", values_to = "Value") |>
        filter(!is.na(Value), Value > 0) |>
        arrange(Measure, Year) |>
        group_by(Measure) |>
        mutate(IndexedValue = Value / first(Value) * 100) |>
        ungroup() |>
        mutate(
          Measure = dplyr::recode(Measure, TotalFTE = "Total FTE", FullTimeInstructionalStaff = "Full-time instructional staff", TotalStaffHeadcount = "Total staff headcount"),
          text = paste0("Measure: ", Measure, "<br>Year: ", Year, "<br>Indexed value: ", scales::number(IndexedValue, accuracy = 0.1), "<br>Raw value: ", scales::comma(Value))
        )
      validate(need(nrow(base_data) > 0, "No valid enrollment and staffing growth data are available."))
      ggplot(base_data, aes(x = Year, y = IndexedValue, color = Measure, text = text)) +
        geom_hline(yintercept = 100, color = "#6b7280", linewidth = 0.5) +
        geom_line(linewidth = 1.05, na.rm = TRUE) +
        geom_point(size = 2.5, na.rm = TRUE) +
        labs(x = NULL, y = "Indexed value, first valid selected year = 100", color = "Measure") +
        ie_base_theme()
    }))

    ie_render_plot(output, "staff_ranking_plot", reactive({
      ie_ranking_plot(current_ranking(input$staff_rank_metric), input$staff_rank_metric, "Higher staffing levels are not automatically better.")
    }))

    output$staff_summary <- renderUI({
      latest <- ie_latest_primary_row(selected_data(), input$primary_institution, "StaffHeadcountPer100FTE")
      if (nrow(latest) == 0) return(tags$p("Not available."))
      tags$p(paste0(latest$Institution[1], " reported ", ie_format_value(latest$MetricValue[1], "StaffHeadcountPer100FTE"), " staff headcount per 100 FTE in ", latest$Year[1], ". Staff headcount is not treated as staff FTE."))
    })

    ie_render_plot(output, "peer_ranking_plot", reactive({
      caption <- if (input$ranking_method == "Latest common year") {
        paste0("Common year: ", input$comparison_year, ". Rank 1 indicates the highest value, not automatically the best institution.")
      } else {
        "Using the latest available year for each institution. Rank 1 indicates the highest value, not automatically the best institution."
      }
      ie_ranking_plot(current_ranking(input$peer_metric), input$peer_metric, caption)
    }))

    ie_render_plot(output, "peer_trend_plot", reactive({
      metric <- input$peer_metric
      primary <- ie_valid_metric_data(selected_data(), metric) |>
        filter(UNITID == input$primary_institution) |>
        transmute(Year, Series = "Primary institution", Value = MetricValue, ValidPeers = NA_integer_)
      peers <- metric_peer_stats(metric) |>
        select(Year, PeerAverage, PeerMedian, PeerMinimum, PeerMaximum, ValidPeers) |>
        tidyr::pivot_longer(c(PeerAverage, PeerMedian, PeerMinimum, PeerMaximum), names_to = "Series", values_to = "Value") |>
        mutate(Series = dplyr::recode(Series, PeerAverage = "Peer average", PeerMedian = "Peer median", PeerMinimum = "Peer minimum", PeerMaximum = "Peer maximum"))
      plot_data <- bind_rows(primary, peers) |>
        filter(!is.na(Value)) |>
        mutate(text = paste0("Series: ", Series, "<br>Year: ", Year, "<br>", ie_metric_label(metric), ": ", vapply(Value, ie_format_value, character(1), metric = metric), ifelse(is.na(ValidPeers), "", paste0("<br>Valid peers: ", ValidPeers))))
      validate(need(nrow(plot_data) > 0, "No peer trend data are available."))
      ggplot(plot_data, aes(x = Year, y = Value, color = Series, text = text)) +
        geom_line(linewidth = 1.05, na.rm = TRUE) +
        geom_point(size = 2.4, na.rm = TRUE) +
        scale_y_continuous(labels = function(x) vapply(x, ie_format_value, character(1), metric = metric)) +
        labs(x = NULL, y = ie_metric_label(metric), color = NULL) +
        ie_base_theme()
    }))

    ie_render_plot(output, "profile_plot", reactive({
      profile_metrics <- c("DegreesPer100FTE", "InstructionExpensesPerDegree", "CoreExpensesPerDegree", "CoreRevenuePerFTE", "InstructionExpensesPerFTE", "StaffHeadcountPer100FTE", "TuitionDependency", "DegreesPerInstructionalStaffFTE")
      year <- as.integer(input$comparison_year)
      plot_data <- lapply(profile_metrics, function(metric) {
        primary <- ie_valid_metric_data(selected_data(), metric) |> filter(UNITID == input$primary_institution, Year == year) |> slice(1)
        peers <- ie_valid_metric_data(selected_data(), metric) |> filter(UNITID %in% peer_ids(), UNITID != input$primary_institution, Year == year)
        if (nrow(primary) == 0 || nrow(peers) == 0) return(NULL)
        percentile <- mean(peers$MetricValue <= primary$MetricValue[1], na.rm = TRUE) * 100
        tibble::tibble(
          Metric = ie_metric_label(metric),
          MetricKey = metric,
          PrimaryValue = primary$MetricValue[1],
          PeerMedian = median(peers$MetricValue, na.rm = TRUE),
          Difference = primary$MetricValue[1] - median(peers$MetricValue, na.rm = TRUE),
          Percentile = percentile,
          ValidPeers = n_distinct(peers$UNITID)
        )
      }) |> bind_rows()
      validate(need(nrow(plot_data) > 0, "No profile metrics are available for the selected year."))
      plot_data <- plot_data |>
        mutate(
          IndexedPrimary = PrimaryValue / PeerMedian * 100,
          text = paste0(
            "Metric: ", Metric,
            "<br>Primary value: ", mapply(ie_format_value, PrimaryValue, MetricKey),
            "<br>Peer median: ", mapply(ie_format_value, PeerMedian, MetricKey),
            "<br>Difference: ", mapply(ie_format_change, Difference, MetricKey),
            "<br>Percentile rank: ", scales::number(Percentile, accuracy = 1), "%",
            "<br>Valid peers: ", ValidPeers
          )
        )
      ggplot(plot_data, aes(x = IndexedPrimary, y = reorder(Metric, IndexedPrimary), text = text)) +
        geom_vline(xintercept = 100, color = "#6b7280", linetype = "dashed") +
        geom_segment(aes(x = 100, xend = IndexedPrimary, yend = Metric), color = "#b31b1b", linewidth = 1) +
        geom_point(color = "#b31b1b", size = 3) +
        scale_x_continuous(labels = function(x) paste0(scales::number(x, accuracy = 1), "%")) +
        labs(x = "Primary institution as percent of selected peer median", y = NULL, caption = "Values above 100 indicate the primary institution is above the selected peer median. No metric direction is reversed.") +
        ie_base_theme()
    }))

    output$peer_summary <- renderUI({
      latest <- ie_latest_primary_row(selected_data(), input$primary_institution, input$peer_metric)
      if (nrow(latest) == 0) return(tags$p("Not available."))
      peer <- metric_peer_stats(input$peer_metric) |> filter(Year == latest$Year[1]) |> slice(1)
      sentence <- paste0(latest$Institution[1], " reported ", ie_format_value(latest$MetricValue[1], input$peer_metric), " for ", ie_metric_label(input$peer_metric), " in ", latest$Year[1], ".")
      if (nrow(peer) > 0) sentence <- paste0(sentence, " The selected peer median was ", ie_format_value(peer$PeerMedian[1], input$peer_metric), " with ", peer$ValidPeers[1], " valid peers reporting.")
      tags$p(sentence)
    })

    detail_table_data <- reactive({
      selected_data() |>
        select(Year, UNITID, Institution, State, Control, TotalFTE, TotalDegreesAwarded, DegreesPer100FTE, InstructionExpensesPerDegree, CoreExpensesPerDegree, DegreesPerFullTimeInstructionalStaff, DegreesPerInstructionalStaffFTE, CoreRevenuePerFTE, TuitionRevenuePerFTE, InstructionExpensesPerFTE, StudentServicesExpensesPerFTE, AcademicSupportExpensesPerFTE, InstitutionalSupportExpensesPerFTE, StaffHeadcountPer100FTE, StaffFTEPer100StudentFTE, TuitionDependency, FinanceReportingStandard, FinanceParentChildStatus, FinanceParentWarning) |>
        arrange(Institution, Year)
    })

    output$detail_table <- renderDT({
      datatable(
        detail_table_data(),
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = list(list(extend = "csv", text = "Download filtered CSV", exportOptions = list(modifier = list(search = "applied", order = "applied"))))
        )
      ) |>
        formatCurrency(c("InstructionExpensesPerDegree", "CoreExpensesPerDegree", "CoreRevenuePerFTE", "TuitionRevenuePerFTE", "InstructionExpensesPerFTE", "StudentServicesExpensesPerFTE", "AcademicSupportExpensesPerFTE", "InstitutionalSupportExpensesPerFTE"), currency = "$", digits = 0) |>
        formatRound(c("TotalFTE", "TotalDegreesAwarded", "DegreesPer100FTE", "DegreesPerFullTimeInstructionalStaff", "DegreesPerInstructionalStaffFTE", "StaffHeadcountPer100FTE", "StaffFTEPer100StudentFTE", "TuitionDependency"), digits = 1)
    })
  })
}

# ============================================================
# Retention Analysis server module
# ============================================================
ra_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    retention_data <- ra_load_data()
    available_years <- sort(unique(retention_data$Year))
    default_years <- tail(available_years, min(10, length(available_years)))
    institution_choices <- retention_data |> distinct(UNITID, Institution) |> arrange(desc(Institution == "Caldwell University"), Institution)
    peer_choices <- retention_data |> filter(Institution %in% ra_peer_institutions) |> distinct(UNITID, Institution) |> arrange(Institution)
    caldwell_id <- retention_data |> filter(Institution == "Caldwell University") |> slice(1) |> pull(UNITID)

    updateSelectizeInput(session, "primary_institution", choices = stats::setNames(institution_choices$UNITID, institution_choices$Institution), selected = caldwell_id, server = TRUE)
    updateSelectizeInput(session, "comparison_institutions", choices = stats::setNames(peer_choices$UNITID, peer_choices$Institution), selected = peer_choices$UNITID, server = TRUE)
    updateSliderInput(session, "year_range", min = min(available_years), max = max(available_years), value = range(default_years), step = 1)

    selected_ids <- reactive({
      req(input$primary_institution)
      unique(c(input$primary_institution, input$comparison_institutions %||% character(0)))
    })

    selected_wide_data <- reactive({
      req(input$year_range)
      validate(need(length(selected_ids()) > 0, "Select at least one institution."))

      retention_data |>
        filter(
          UNITID %in% selected_ids(),
          Year >= input$year_range[1],
          Year <= input$year_range[2]
        ) |>
        arrange(Institution, Year)
    })

    selected_long_data <- reactive({
      req(input$attendance_status)
      ra_status_data(selected_wide_data(), input$attendance_status)
    })

    rank_long_data <- reactive({
      req(input$rank_attendance_status)
      ra_status_data(selected_wide_data(), input$rank_attendance_status) |>
        filter(AttendanceStatus == input$rank_attendance_status)
    })

    primary_status_data <- reactive({
      selected_long_data() |>
        filter(UNITID == input$primary_institution, !is.na(RetentionRate)) |>
        arrange(AttendanceStatus, Year)
    })

    latest_primary_rows <- reactive({
      data <- primary_status_data()
      validate(need(nrow(data) > 0, "No valid retention rates are available for the selected primary institution."))

      if (identical(input$attendance_status, "Both")) {
        data |>
          group_by(AttendanceStatus) |>
          slice_max(Year, n = 1, with_ties = FALSE) |>
          ungroup()
      } else {
        data |> slice_max(Year, n = 1, with_ties = FALSE)
      }
    })

    latest_primary_for_comparison <- reactive({
      rows <- latest_primary_rows()
      if (identical(input$attendance_status, "Both")) {
        rows |> filter(AttendanceStatus == "Full-time") |> slice(1)
      } else {
        rows |> slice(1)
      }
    })

    peer_stats_by_year <- reactive({
      req(input$primary_institution)
      selected_long_data() |>
        filter(UNITID != input$primary_institution, !is.na(RetentionRate)) |>
        group_by(Year, AttendanceStatus) |>
        summarise(
          PeerAverage = mean(RetentionRate),
          PeerMedian = median(RetentionRate),
          PeerMinimum = min(RetentionRate),
          PeerMaximum = max(RetentionRate),
          NumberOfPeersReporting = n_distinct(UNITID),
          .groups = "drop"
        )
    })

    ranking_data <- reactive({
      data <- rank_long_data() |> filter(!is.na(RetentionRate))
      validate(need(nrow(data) > 0, "No valid retention rates are available for ranking."))

      if (identical(input$ranking_method, "Latest common year")) {
        common_years <- data |>
          group_by(Year) |>
          summarise(InstitutionCount = n_distinct(UNITID), .groups = "drop") |>
          filter(InstitutionCount == length(selected_ids()))

        validate(need(nrow(common_years) > 0, "No common year has valid retention values for every selected institution."))
        ranking_year <- max(common_years$Year)
        data <- data |> filter(Year == ranking_year)
      } else {
        data <- data |>
          group_by(UNITID, Institution) |>
          slice_max(Year, n = 1, with_ties = FALSE) |>
          ungroup()
      }

      data |>
        arrange(desc(RetentionRate), Institution) |>
        mutate(Rank = min_rank(desc(RetentionRate)))
    })

    primary_rank_row <- reactive({
      ranking_data() |> filter(UNITID == input$primary_institution) |> slice(1)
    })

    latest_gap_row <- reactive({
      selected_wide_data() |>
        filter(UNITID == input$primary_institution, !is.na(FullTimeRetentionRate), !is.na(PartTimeRetentionRate)) |>
        arrange(desc(Year)) |>
        slice(1)
    })

    table_data <- reactive({
      data <- selected_long_data() |>
        left_join(peer_stats_by_year(), by = c("Year", "AttendanceStatus")) |>
        group_by(Year, AttendanceStatus) |>
        mutate(Rank = ifelse(is.na(RetentionRate), NA_integer_, min_rank(desc(RetentionRate)))) |>
        ungroup() |>
        mutate(
          DifferenceFromPeerMedian = RetentionRate - PeerMedian,
          YearOverYearChange = CalculatedYoYChange
        ) |>
        select(
          Year, UNITID, Institution, State, AttendanceStatus, CohortSize,
          StudentsRetained, RetentionRate, YearOverYearChange, RetentionGap,
          PeerAverage, PeerMedian, DifferenceFromPeerMedian, Rank,
          NumberOfPeersReporting
        ) |>
        arrange(Institution, AttendanceStatus, Year)

      data
    })

    stability_data <- reactive({
      selected_long_data() |>
        filter(!is.na(RetentionRate)) |>
        group_by(UNITID, Institution, AttendanceStatus) |>
        summarise(
          AverageRetentionRate = mean(RetentionRate),
          MinimumRetentionRate = min(RetentionRate),
          MaximumRetentionRate = max(RetentionRate),
          RetentionRange = MaximumRetentionRate - MinimumRetentionRate,
          StandardDeviation = ifelse(dplyr::n() > 1, stats::sd(RetentionRate), NA_real_),
          NumberOfReportingYears = dplyr::n(),
          LatestYearOverYearChange = dplyr::last(na.omit(CalculatedYoYChange), default = NA_real_),
          .groups = "drop"
        ) |>
        mutate(
          RetentionCategory = dplyr::case_when(
            AverageRetentionRate >= median(AverageRetentionRate, na.rm = TRUE) & StandardDeviation <= median(StandardDeviation, na.rm = TRUE) ~ "High retention and stable",
            AverageRetentionRate >= median(AverageRetentionRate, na.rm = TRUE) & StandardDeviation > median(StandardDeviation, na.rm = TRUE) ~ "High retention and variable",
            AverageRetentionRate < median(AverageRetentionRate, na.rm = TRUE) & StandardDeviation <= median(StandardDeviation, na.rm = TRUE) ~ "Lower retention and stable",
            AverageRetentionRate < median(AverageRetentionRate, na.rm = TRUE) & StandardDeviation > median(StandardDeviation, na.rm = TRUE) ~ "Lower retention and variable",
            TRUE ~ "Not enough data"
          )
        )
    })

    output$kpi_latest_rate <- renderText({
      rows <- latest_primary_rows()
      paste(
        paste0(ra_format_percent(rows$RetentionRate), " in ", rows$Year, " (", rows$AttendanceStatus, ")"),
        collapse = " | "
      )
    })

    output$kpi_previous_change <- renderText({
      rows <- latest_primary_rows()
      values <- purrr::map_chr(seq_len(nrow(rows)), function(i) {
        row <- rows[i, ]
        previous <- selected_long_data() |>
          filter(
            UNITID == row$UNITID,
            AttendanceStatus == row$AttendanceStatus,
            Year < row$Year,
            !is.na(RetentionRate)
          ) |>
          arrange(desc(Year)) |>
          slice(1)

        change <- if (nrow(previous) == 0) NA_real_ else row$RetentionRate - previous$RetentionRate
        paste0(ra_format_pp(change), " (", row$AttendanceStatus, ")")
      })
      paste(values, collapse = " | ")
    })

    output$kpi_average <- renderText({
      data <- primary_status_data()
      validate(need(nrow(data) > 0, "Not available"))

      data |>
        group_by(AttendanceStatus) |>
        summarise(Average = mean(RetentionRate), ValidYears = dplyr::n(), .groups = "drop") |>
        mutate(Label = paste0(ra_format_percent(Average), " across ", ValidYears, " years (", AttendanceStatus, ")")) |>
        pull(Label) |>
        paste(collapse = " | ")
    })

    output$kpi_highest <- renderText({
      rows <- primary_status_data() |>
        group_by(AttendanceStatus) |>
        slice_max(RetentionRate, n = 1, with_ties = FALSE) |>
        ungroup()
      paste(paste0(ra_format_percent(rows$RetentionRate), " in ", rows$Year, " (", rows$AttendanceStatus, ")"), collapse = " | ")
    })

    output$kpi_lowest <- renderText({
      rows <- primary_status_data() |>
        group_by(AttendanceStatus) |>
        slice_min(RetentionRate, n = 1, with_ties = FALSE) |>
        ungroup()
      paste(paste0(ra_format_percent(rows$RetentionRate), " in ", rows$Year, " (", rows$AttendanceStatus, ")"), collapse = " | ")
    })

    output$kpi_gap <- renderText({
      row <- latest_gap_row()
      if (nrow(row) == 0 || is.na(row$RetentionGap[1])) return("Not available")

      interpretation <- dplyr::case_when(
        row$RetentionGap[1] > 0 ~ "full-time retention is higher",
        row$RetentionGap[1] < 0 ~ "part-time retention is higher",
        TRUE ~ "both rates are equal"
      )
      paste0(ra_format_pp(row$RetentionGap[1]), " in ", row$Year[1], "; ", interpretation)
    })

    output$kpi_peer_median <- renderText({
      latest <- latest_primary_for_comparison()
      peer_row <- peer_stats_by_year() |> filter(Year == latest$Year[1], AttendanceStatus == latest$AttendanceStatus[1]) |> slice(1)
      if (nrow(peer_row) == 0 || is.na(peer_row$PeerMedian[1])) return("Not available")

      diff <- latest$RetentionRate[1] - peer_row$PeerMedian[1]
      if (diff == 0) {
        paste0("Equal to peer median (n=", peer_row$NumberOfPeersReporting[1], ")")
      } else {
        direction <- ifelse(diff > 0, "above", "below")
        paste0(scales::number(abs(diff), accuracy = 0.1), " percentage points ", direction, " peer median (n=", peer_row$NumberOfPeersReporting[1], ")")
      }
    })

    output$kpi_rank <- renderText({
      row <- primary_rank_row()
      if (nrow(row) == 0 || is.na(row$Rank[1])) return("Not available")
      paste0(row$Rank[1], " of ", nrow(ranking_data()), " in ", row$Year[1], " (", row$AttendanceStatus[1], ")")
    })

    ra_render_plot(output, "trend_plot", reactive({
      data <- selected_long_data() |>
        filter(!is.na(RetentionRate)) |>
        mutate(
          LineGroup = paste(UNITID, AttendanceStatus),
          Emphasis = UNITID == input$primary_institution | Institution == "Caldwell University",
          text = paste0(
            "Institution: ", Institution,
            "<br>Year: ", Year,
            "<br>Attendance status: ", AttendanceStatus,
            "<br>Retention rate: ", ra_format_percent(RetentionRate),
            ifelse(is.na(CohortSize), "", paste0("<br>Cohort size: ", ra_format_count(CohortSize))),
            ifelse(is.na(StudentsRetained), "", paste0("<br>Number retained: ", ra_format_count(StudentsRetained)))
          )
        )

      validate(need(nrow(data) > 0, "No valid retention rates are available for the selected filters."))

      plot <- ggplot(data, aes(x = Year, y = RetentionRate, color = Institution, linetype = AttendanceStatus, group = LineGroup, text = text)) +
        geom_line(aes(linewidth = Emphasis, alpha = Emphasis), na.rm = TRUE) +
        geom_point(aes(size = Emphasis), na.rm = TRUE) +
        scale_linewidth_manual(values = c(`TRUE` = 1.35, `FALSE` = 0.85), guide = "none") +
        scale_size_manual(values = c(`TRUE` = 3.2, `FALSE` = 1.9), guide = "none") +
        scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.6), guide = "none") +
        scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
        scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], by = 1)) +
        labs(x = NULL, y = "Retention rate", color = "Institution", linetype = "Attendance status") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())

      if (identical(input$attendance_status, "Both")) {
        plot + facet_wrap(~ AttendanceStatus)
      } else {
        plot
      }
    }))

    ra_render_plot(output, "peer_summary_plot", reactive({
      data <- selected_long_data() |> filter(UNITID == caldwell_id, !is.na(RetentionRate))
      peer <- peer_stats_by_year() |>
        filter(AttendanceStatus %in% unique(data$AttendanceStatus)) |>
        pivot_longer(c(PeerAverage, PeerMedian, PeerMinimum, PeerMaximum), names_to = "Statistic", values_to = "RetentionRate") |>
        mutate(
          Institution = dplyr::recode(
            Statistic,
            PeerAverage = "Peer average",
            PeerMedian = "Peer median",
            PeerMinimum = "Peer minimum",
            PeerMaximum = "Peer maximum"
          ),
          text = paste0(
            Institution,
            "<br>Year: ", Year,
            "<br>Attendance status: ", AttendanceStatus,
            "<br>Retention rate: ", ra_format_percent(RetentionRate),
            "<br>Peers reporting: ", NumberOfPeersReporting
          )
        )

      caldwell <- data |>
        mutate(
          Institution = "Caldwell University",
          text = paste0(
            Institution,
            "<br>Year: ", Year,
            "<br>Attendance status: ", AttendanceStatus,
            "<br>Retention rate: ", ra_format_percent(RetentionRate)
          )
        )

      plot_data <- bind_rows(
        caldwell |> select(Year, Institution, AttendanceStatus, RetentionRate, text),
        peer |> select(Year, Institution, AttendanceStatus, RetentionRate, text)
      ) |> filter(!is.na(RetentionRate))

      validate(need(nrow(plot_data) > 0, "No Caldwell or peer summary retention rates are available."))

      ggplot(plot_data, aes(x = Year, y = RetentionRate, color = Institution, linetype = Institution, group = paste(Institution, AttendanceStatus), text = text)) +
        geom_line(linewidth = 1.05, na.rm = TRUE) +
        geom_point(size = 2.3, na.rm = TRUE) +
        scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
        scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], by = 1)) +
        labs(x = NULL, y = "Retention rate", color = NULL, linetype = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
    }))

    ra_render_plot(output, "ftpt_plot", reactive({
      data <- selected_wide_data() |> filter(UNITID == input$primary_institution)

      if (identical(input$ftpt_display, "Retention Gap")) {
        gap_data <- data |>
          filter(!is.na(FullTimeRetentionRate), !is.na(PartTimeRetentionRate)) |>
          mutate(text = paste0(
            "Institution: ", Institution,
            "<br>Year: ", Year,
            "<br>Full-time retention rate: ", ra_format_percent(FullTimeRetentionRate),
            "<br>Part-time retention rate: ", ra_format_percent(PartTimeRetentionRate),
            "<br>Retention gap: ", ra_format_pp(RetentionGap)
          ))

        validate(need(nrow(gap_data) > 0, "No years have both full-time and part-time retention rates."))

        ggplot(gap_data, aes(x = Year, y = RetentionGap, text = text)) +
          geom_hline(yintercept = 0, color = "#6b7280", linewidth = 0.6) +
          geom_col(fill = "#b31b1b", width = 0.7, na.rm = TRUE) +
          scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], by = 1)) +
          labs(x = NULL, y = "Percentage-point difference") +
          theme_minimal(base_size = 13) +
          theme(panel.grid.minor = element_blank())
      } else {
        rate_data <- ra_status_data(data, "Both") |>
          filter(!is.na(RetentionRate)) |>
          mutate(text = paste0(
            "Institution: ", Institution,
            "<br>Year: ", Year,
            "<br>Attendance status: ", AttendanceStatus,
            "<br>Retention rate: ", ra_format_percent(RetentionRate)
          ))

        validate(need(nrow(rate_data) > 0, "No full-time or part-time retention rates are available."))

        ggplot(rate_data, aes(x = Year, y = RetentionRate, color = AttendanceStatus, group = AttendanceStatus, text = text)) +
          geom_line(linewidth = 1.05, na.rm = TRUE) +
          geom_point(size = 2.6, na.rm = TRUE) +
          scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
          scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], by = 1)) +
          labs(x = NULL, y = "Retention rate", color = NULL) +
          theme_minimal(base_size = 13) +
          theme(legend.position = "bottom", panel.grid.minor = element_blank())
      }
    }))

    ra_render_plot(output, "ranking_plot", reactive({
      data <- ranking_data() |>
        mutate(
          Highlight = Institution == "Caldwell University" | UNITID == input$primary_institution,
          Label = paste0("#", Rank, "  ", ra_format_percent(RetentionRate), " (", Year, ")"),
          text = paste0(
            "Rank: ", Rank,
            "<br>Institution: ", Institution,
            "<br>Reporting year: ", Year,
            "<br>Attendance status: ", AttendanceStatus,
            "<br>Retention rate: ", ra_format_percent(RetentionRate)
          )
        )

      ggplot(data, aes(x = reorder(Institution, RetentionRate), y = RetentionRate, fill = Highlight, text = text)) +
        geom_col(width = 0.72, na.rm = TRUE) +
        geom_text(aes(label = Label), hjust = -0.05, size = 3.4) +
        coord_flip() +
        scale_fill_manual(values = c(`TRUE` = "#b31b1b", `FALSE` = "#6b7280"), guide = "none") +
        scale_y_continuous(limits = c(0, 105), labels = function(x) paste0(x, "%")) +
        labs(x = NULL, y = "Retention rate") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())
    }))

    ra_render_plot(output, "gap_heatmap", reactive({
      data <- selected_wide_data() |>
        mutate(
          GapLabel = ifelse(is.na(RetentionGap), "Not reported", ra_format_pp(RetentionGap)),
          text = paste0(
            "Institution: ", Institution,
            "<br>Year: ", Year,
            "<br>Full-time retention rate: ", ra_format_percent(FullTimeRetentionRate),
            "<br>Part-time retention rate: ", ra_format_percent(PartTimeRetentionRate),
            "<br>Retention gap: ", GapLabel
          )
        )

      validate(need(nrow(data) > 0, "No retention-gap rows are available."))

      ggplot(data, aes(x = factor(Year), y = Institution, fill = RetentionGap, text = text)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0, na.value = "#eeeeee") +
        labs(x = "Year", y = NULL, fill = "Full-time minus part-time") +
        theme_minimal(base_size = 13) +
        theme(panel.grid = element_blank())
    }))

    yoy_data <- reactive({
      selected_long_data() |>
        filter(!is.na(CalculatedYoYChange)) |>
        arrange(Institution, AttendanceStatus, Year)
    })

    output$yoy_largest_improvement <- renderText({
      data <- yoy_data()
      if (nrow(data) == 0) return("Not available")
      row <- data |> slice_max(CalculatedYoYChange, n = 1, with_ties = FALSE)
      paste0(row$Institution[1], ": ", ra_format_pp(row$CalculatedYoYChange[1]), " in ", row$Year[1])
    })

    output$yoy_largest_decline <- renderText({
      data <- yoy_data()
      if (nrow(data) == 0) return("Not available")
      row <- data |> slice_min(CalculatedYoYChange, n = 1, with_ties = FALSE)
      paste0(row$Institution[1], ": ", ra_format_pp(row$CalculatedYoYChange[1]), " in ", row$Year[1])
    })

    output$yoy_positive_negative <- renderText({
      data <- yoy_data() |> filter(UNITID == input$primary_institution)
      if (nrow(data) == 0) return("Not available")
      paste0(sum(data$CalculatedYoYChange > 0), " positive; ", sum(data$CalculatedYoYChange < 0), " negative")
    })

    output$yoy_average_change <- renderText({
      data <- yoy_data() |> filter(UNITID == input$primary_institution)
      if (nrow(data) == 0) return("Not available")
      ra_format_pp(mean(data$CalculatedYoYChange, na.rm = TRUE))
    })

    ra_render_plot(output, "yoy_plot", reactive({
      data <- yoy_data() |>
        mutate(
          Highlight = UNITID == input$primary_institution | Institution == "Caldwell University",
          text = paste0(
            "Institution: ", Institution,
            "<br>Year: ", Year,
            "<br>Attendance status: ", AttendanceStatus,
            "<br>Annual change: ", ra_format_pp(CalculatedYoYChange)
          )
        )

      validate(need(nrow(data) > 0, "No consecutive-year retention changes are available."))

      ggplot(data, aes(x = Year, y = CalculatedYoYChange, color = Institution, group = paste(UNITID, AttendanceStatus), text = text)) +
        geom_hline(yintercept = 0, color = "#6b7280", linewidth = 0.6) +
        geom_line(aes(linewidth = Highlight, alpha = Highlight), na.rm = TRUE) +
        geom_point(aes(size = Highlight), na.rm = TRUE) +
        scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.75), guide = "none") +
        scale_size_manual(values = c(`TRUE` = 3.1, `FALSE` = 1.8), guide = "none") +
        scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.62), guide = "none") +
        labs(x = NULL, y = "Percentage-point difference", color = "Institution") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
    }))

    ra_render_plot(output, "stability_plot", reactive({
      data <- stability_data() |>
        mutate(
          Highlight = UNITID == input$primary_institution | Institution == "Caldwell University",
          text = paste0(
            "Institution: ", Institution,
            "<br>Attendance status: ", AttendanceStatus,
            "<br>Average retention rate: ", ra_format_percent(AverageRetentionRate),
            "<br>Standard deviation: ", ra_format_pp(StandardDeviation, short = TRUE),
            "<br>Category: ", RetentionCategory,
            "<br>Reporting years: ", NumberOfReportingYears
          )
        )

      validate(need(nrow(data) > 0, "No valid retention rates are available for stability analysis."))

      ggplot(data, aes(x = AverageRetentionRate, y = StandardDeviation, color = RetentionCategory, shape = AttendanceStatus, text = text)) +
        geom_point(aes(size = Highlight), alpha = 0.9, na.rm = TRUE) +
        scale_size_manual(values = c(`TRUE` = 4.5, `FALSE` = 2.8), guide = "none") +
        scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
        labs(x = "Average retention rate", y = "Standard deviation", color = "Dashboard category", shape = "Attendance status") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
    }))

    output$stability_table <- renderDT({
      stability_data() |>
        arrange(desc(AverageRetentionRate)) |>
        datatable(rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE)) |>
        formatRound(c("AverageRetentionRate", "MinimumRetentionRate", "MaximumRetentionRate", "RetentionRange", "StandardDeviation", "LatestYearOverYearChange"), digits = 1)
    })

    output$written_insight <- renderUI({
      latest <- latest_primary_for_comparison()
      if (nrow(latest) == 0) return(tags$p("Not available."))

      sentences <- character(0)
      institution_name <- latest$Institution[1]
      status_label <- tolower(latest$AttendanceStatus[1])

      if (!is.na(latest$RetentionRate[1])) {
        sentences <- c(sentences, paste0(
          institution_name, "'s latest ", status_label,
          " first-year retention rate was ", ra_format_percent(latest$RetentionRate[1]),
          " in ", latest$Year[1], "."
        ))
      }

      previous <- selected_long_data() |>
        filter(UNITID == latest$UNITID[1], AttendanceStatus == latest$AttendanceStatus[1], Year < latest$Year[1], !is.na(RetentionRate)) |>
        arrange(desc(Year)) |>
        slice(1)

      if (nrow(previous) > 0) {
        change <- latest$RetentionRate[1] - previous$RetentionRate[1]
        direction <- ifelse(change >= 0, "higher", "lower")
        sentences <- c(sentences, paste0(
          "This was ", scales::number(abs(change), accuracy = 0.1),
          " percentage points ", direction, " than the previous reporting year."
        ))
      }

      peer_row <- peer_stats_by_year() |> filter(Year == latest$Year[1], AttendanceStatus == latest$AttendanceStatus[1]) |> slice(1)
      if (nrow(peer_row) > 0 && !is.na(peer_row$PeerMedian[1])) {
        diff <- latest$RetentionRate[1] - peer_row$PeerMedian[1]
        direction <- ifelse(diff >= 0, "above", "below")
        sentences <- c(sentences, paste0(
          "It was ", scales::number(abs(diff), accuracy = 0.1),
          " percentage points ", direction, " the selected peer median."
        ))
      }

      avg_row <- primary_status_data() |>
        filter(AttendanceStatus == latest$AttendanceStatus[1]) |>
        summarise(Average = mean(RetentionRate), .groups = "drop")
      if (nrow(avg_row) > 0 && !is.na(avg_row$Average[1])) {
        sentences <- c(sentences, paste0(
          "Over the selected period, the average ", status_label,
          " retention rate was ", ra_format_percent(avg_row$Average[1]), "."
        ))
      }

      high_row <- primary_status_data() |>
        filter(AttendanceStatus == latest$AttendanceStatus[1]) |>
        slice_max(RetentionRate, n = 1, with_ties = FALSE)
      if (nrow(high_row) > 0) {
        sentences <- c(sentences, paste0(
          "Its highest selected-period rate was ", ra_format_percent(high_row$RetentionRate[1]),
          " in ", high_row$Year[1], "."
        ))
      }

      tags$p(paste(sentences, collapse = " "))
    })

    output$detail_table <- renderDT({
      datatable(
        table_data(),
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("csv"),
          columnDefs = list(
            list(
              targets = c(7, 10, 11),
              render = JS(
                "function(data, type, row, meta) {",
                "  if (data === null || data === undefined || data === '') return '';",
                "  if (type === 'display' || type === 'filter') return parseFloat(data).toFixed(1) + '%';",
                "  return data;",
                "}"
              )
            ),
            list(
              targets = c(8, 9, 12),
              render = JS(
                "function(data, type, row, meta) {",
                "  if (data === null || data === undefined || data === '') return '';",
                "  if (type === 'display' || type === 'filter') return parseFloat(data).toFixed(1) + ' pp';",
                "  return data;",
                "}"
              )
            )
          )
        )
      ) |>
        formatCurrency(c("CohortSize", "StudentsRetained"), currency = "", interval = 3, mark = ",", digits = 0) |>
        formatStyle(
          "YearOverYearChange",
          color = styleInterval(c(-0.00001, 0.00001), c("#b31b1b", "#333333", "#1b7f3a"))
        )
    })

    output$download_detail <- downloadHandler(
      filename = function() "retention_analysis_filtered_data.csv",
      content = function(file) {
        readr::write_csv(table_data(), file, na = "")
      }
    )
  })
}
