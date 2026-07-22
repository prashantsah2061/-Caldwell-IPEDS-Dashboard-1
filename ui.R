e12_kpi_card_ui <- function(output_id, label) {
  div(
    class = "card kpi-card",
    div(class = "kpi-label", label),
    div(class = "kpi-value", textOutput(output_id, inline = TRUE))
  )
}

e12_shared_filters_ui <- function() {
  div(
    class = "card filter-card",
    h2(class = "section-title", "12-Month Enrollment Filters"),
    fluidRow(
      column(
        4,
        selectizeInput(
          inputId = "e12_institution",
          label = "Institutions",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Select one or more institutions",
            plugins = list("remove_button"),
            maxItems = 6,
            maxOptions = 1000
          )
        )
      ),
      column(
        2,
        sliderInput(
          inputId = "e12_year_range",
          label = "Year range",
          min = 2010,
          max = 2024,
          value = c(2010, 2024),
          step = 1,
          sep = ""
        )
      ),
      column(
        2,
        selectInput(
          inputId = "e12_state",
          label = "State",
          choices = "All",
          selected = "All"
        )
      ),
      column(
        2,
        selectInput(
          inputId = "e12_control",
          label = "Institution control",
          choices = c("All", "Public", "Private nonprofit", "Private for-profit"),
          selected = "All"
        )
      ),
      column(
        2,
        selectInput(
          inputId = "e12_level",
          label = "Institution level",
          choices = c("All", "2-year", "4-year", "Other"),
          selected = "All"
        )
      )
    ),
    actionButton(
      inputId = "e12_reset_filters",
      label = "Reset 12-month filters",
      class = "btn-reset"
    )
  )
}


# ============================================================
# Graduation Rate Analytics UI
# ============================================================
gr_css_ui <- function(ns) {
  tags$style(HTML(sprintf("
    #%s.gr-dashboard { --gr-accent: var(--accent, #b31b1b); --gr-muted: var(--muted, #6b7280); --gr-border: var(--border, #e8e8e8); }
    #%s .gr-subtitle { color: var(--gr-muted); font-size: 13px; margin: -8px 0 14px; }
    #%s .gr-filter-actions { display: grid; gap: 8px; grid-template-columns: repeat(2, minmax(0, 1fr)); margin-top: 25px; }
    #%s .gr-filter-actions .btn { width: 100%%; }
    #%s .gr-kpi-grid { display: grid; gap: 16px; grid-template-columns: repeat(4, minmax(0, 1fr)); }
    #%s .gr-kpi-card { border-left: 5px solid var(--gr-accent); min-height: 112px; }
    #%s .gr-small { color: var(--gr-muted); font-size: 12px; margin-top: 6px; }
    #%s .gr-note, #%s .gr-methodology { background: #fff8f8; border-left: 5px solid var(--gr-accent); color: #333; }
    #%s .gr-warning-list { margin: 0; padding-left: 18px; }
    #%s .gr-download-grid { display: grid; gap: 10px; grid-template-columns: repeat(4, minmax(0, 1fr)); }
    #%s .gr-download-grid .btn { width: 100%%; white-space: normal; }
    @media (max-width: 1200px) { #%s .gr-kpi-grid, #%s .gr-download-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
    @media (max-width: 800px) { #%s .gr-kpi-grid, #%s .gr-download-grid, #%s .gr-filter-actions { grid-template-columns: 1fr; } }
  ",
    ns("dashboard"), ns("dashboard"), ns("dashboard"), ns("dashboard"),
    ns("dashboard"), ns("dashboard"), ns("dashboard"), ns("dashboard"),
    ns("dashboard"), ns("dashboard"), ns("dashboard"), ns("dashboard"),
    ns("dashboard"), ns("dashboard"), ns("dashboard"), ns("dashboard"),
    ns("dashboard")
  )))
}

gr_kpi_card_ui <- function(output_id, label, ns) {
  div(
    class = "card kpi-card gr-kpi-card",
    div(class = "kpi-label", label),
    div(class = "kpi-value", textOutput(ns(output_id), inline = TRUE))
  )
}

gr_dashboard_ui <- function(id) {
  ns <- NS(id)

  div(
    id = ns("dashboard"),
    class = "gr-dashboard",
    gr_css_ui(ns),
    div(
      class = "card filter-card",
      h2(class = "section-title", "Graduation Rate Analytics Filters"),
      fluidRow(
        column(3, selectInput(ns("main_institution"), "Main institution", choices = NULL)),
        column(
          5,
          selectizeInput(
            ns("peer_institutions"),
            "Peer institutions",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Select peer institutions")
          )
        ),
        column(
          2,
          div(
            class = "gr-filter-actions",
            actionButton(ns("select_all_peers"), "Select All"),
            actionButton(ns("clear_all_peers"), "Clear All")
          )
        ),
        column(2, selectInput(ns("rate_type"), "Rate type", choices = gr_rate_choices, selected = "rate_150"))
      ),
      fluidRow(
        column(3, sliderInput(ns("cohort_range"), "Cohort-year range", min = 2009, max = 2018, value = c(2009, 2018), step = 1, sep = "")),
        column(3, selectInput(ns("selected_cohort"), "Selected cohort year", choices = NULL)),
        column(
          3,
          selectInput(
            ns("comparison_mode"),
            "Comparison mode",
            choices = c(
              "Individual peer institutions",
              "Weighted peer-group average",
              "Peer median",
              "Caldwell versus peers",
              "New Jersey peers"
            ),
            selected = "Caldwell versus peers"
          )
        ),
        column(3, selectInput(ns("chart_display"), "Chart display", choices = c("Graduation rate", "Cohort and completer counts"), selected = "Graduation rate"))
      ),
      uiOutput(ns("demographic_filters"))
    ),
    div(
      class = "gr-kpi-grid",
      gr_kpi_card_ui("kpi_caldwell_rate", "Caldwell selected rate", ns),
      gr_kpi_card_ui("kpi_peer_weighted", "Peer weighted rate", ns),
      gr_kpi_card_ui("kpi_peer_median", "Peer median rate", ns),
      gr_kpi_card_ui("kpi_difference", "Caldwell difference", ns),
      gr_kpi_card_ui("kpi_rank", "Caldwell rank", ns),
      gr_kpi_card_ui("kpi_adjusted_cohort", "Caldwell adjusted cohort", ns),
      gr_kpi_card_ui("kpi_completers", "Caldwell completers", ns),
      gr_kpi_card_ui("kpi_change", "Change from previous cohort", ns)
    ),
    div(
      class = "card",
      h2(class = "section-title", "Graduation Rate by Entering Cohort"),
      div(class = "gr-subtitle", "Each year represents a different entering student cohort. The 150% bachelor's graduation rate generally measures completion within approximately six years."),
      gr_plot_output(ns("trend_plot"), "500px")
    ),
    fluidRow(
      column(
        6,
        div(
          class = "card",
          h2(class = "section-title", "Cohort Size and Completion Outcomes"),
          radioButtons(ns("outcome_display"), NULL, choices = c("Counts", "Rates"), selected = "Counts", inline = TRUE),
          gr_plot_output(ns("outcomes_plot"), "430px")
        )
      ),
      column(
        6,
        div(
          class = "card",
          h2(class = "section-title", "Institution Ranking"),
          gr_plot_output(ns("ranking_plot"), "430px"),
          uiOutput(ns("missing_ranking_note"))
        )
      )
    ),
    fluidRow(
      column(6, div(class = "card", h2(class = "section-title", "Caldwell University Peer Rank by Entering Cohort"), gr_plot_output(ns("rank_over_time_plot"), "420px"))),
      column(6, div(class = "card", h2(class = "section-title", "Peer Graduation Rate Distribution"), gr_plot_output(ns("distribution_plot"), "420px"), tableOutput(ns("distribution_stats"))))
    ),
    fluidRow(
      column(6, div(class = "card", h2(class = "section-title", "Graduation Rate Across Institutions and Cohorts"), gr_plot_output(ns("heatmap_plot"), "520px"))),
      column(6, div(class = "card", h2(class = "section-title", "Transfer-Out Analysis"), gr_plot_output(ns("transfer_plot"), "420px"), uiOutput(ns("transfer_warning"))))
    ),
    div(class = "card", h2(class = "section-title", "New Jersey Peer Comparison"), DTOutput(ns("nj_table"))),
    div(class = "card", h2(class = "section-title", "Caldwell Versus Peers"), DTOutput(ns("comparison_table"))),
    uiOutput(ns("demographic_section")),
    div(class = "card gr-note", h2(class = "section-title", "Insights"), uiOutput(ns("insights_panel"))),
    div(class = "card gr-note", h2(class = "section-title", "Data-Quality Warnings"), uiOutput(ns("data_quality_warnings"))),
    div(
      class = "card gr-methodology",
      h2(class = "section-title", "Methodology"),
      tags$ul(
        tags$li("The standard IPEDS graduation-rate cohort does not include every student enrolled at the university."),
        tags$li("It generally follows first-time, full-time, degree- or certificate-seeking undergraduates."),
        tags$li("Each CohortYear represents a different entering group of students."),
        tags$li("For bachelor's programs, 100%, 150%, and 200% of normal time generally correspond to approximately four, six, and eight years."),
        tags$li("AdjustedCohort is the denominator used in the rate."),
        tags$li("ReportingYear and CohortYear are different."),
        tags$li("Peer averages are weighted using total completers divided by total adjusted cohort."),
        tags$li("Missing values are not treated as zero."),
        tags$li("Institution comparisons do not explain why the rates differ.")
      )
    ),
    div(
      class = "card",
      h2(class = "section-title", "Downloads"),
      div(
        class = "gr-download-grid",
        downloadButton(ns("download_full"), "Full graduation-rate CSV"),
        downloadButton(ns("download_trend"), "Filtered trend data"),
        downloadButton(ns("download_ranking"), "Selected cohort ranking"),
        downloadButton(ns("download_caldwell_trend"), "Caldwell trend data"),
        downloadButton(ns("download_weighted"), "Peer weighted average"),
        downloadButton(ns("download_summary"), "Peer summary statistics"),
        downloadButton(ns("download_nj"), "New Jersey comparison"),
        uiOutput(ns("download_demographic_button"))
      )
    )
  )
}


# ============================================================
# Retention Analysis UI
# ============================================================
ra_kpi_card_ui <- function(output_id, label, ns) {
  div(
    class = "card kpi-card ra-kpi-card",
    div(class = "kpi-label", label),
    div(class = "kpi-value", textOutput(ns(output_id), inline = TRUE))
  )
}

ra_mini_kpi_ui <- function(output_id, label, ns) {
  div(
    class = "ra-mini-kpi",
    div(class = "kpi-label", label),
    div(class = "kpi-value", textOutput(ns(output_id), inline = TRUE))
  )
}

ra_css_ui <- function(ns) {
  tags$style(HTML(sprintf("
    #%s.ra-dashboard { --ra-accent: var(--accent, #b31b1b); --ra-muted: var(--muted, #6b7280); --ra-border: var(--border, #e8e8e8); }
    #%s .ra-definition { color: var(--ra-muted); margin: -4px 0 0; }
    #%s .ra-kpi-grid { display: grid; gap: 16px; grid-template-columns: repeat(4, minmax(0, 1fr)); }
    #%s .ra-kpi-card { border-left: 5px solid var(--ra-accent); min-height: 124px; }
    #%s .ra-kpi-card .kpi-value { font-size: 21px; line-height: 1.3; }
    #%s .ra-mini-kpi { border-left: 4px solid var(--ra-accent); padding: 8px 10px; }
    #%s .ra-mini-kpi .kpi-value { font-size: 18px; line-height: 1.35; }
    #%s .ra-small { color: var(--ra-muted); font-size: 12px; margin-top: 6px; }
    #%s .ra-note { background: #fff8f8; border-left: 5px solid var(--ra-accent); }
    #%s .ra-download { margin-top: 10px; }
    @media (max-width: 1200px) { #%s .ra-kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
    @media (max-width: 800px) { #%s .ra-kpi-grid { grid-template-columns: 1fr; } }
  ",
    ns("dashboard"), ns("dashboard"), ns("dashboard"), ns("dashboard"),
    ns("dashboard"), ns("dashboard"), ns("dashboard"), ns("dashboard"),
    ns("dashboard"), ns("dashboard"), ns("dashboard"), ns("dashboard"),
    ns("dashboard")
  )))
}

ra_dashboard_ui <- function(id) {
  ns <- NS(id)

  div(
    id = ns("dashboard"),
    class = "ra-dashboard",
    ra_css_ui(ns),
    div(
      class = "card",
      h2(class = "section-title", "Retention Analysis"),
      p(class = "ra-definition", "First-year retention is the percentage of first-time students who return to the institution in the following fall reporting period. Differences between rates are shown as percentage-point differences.")
    ),
    div(
      class = "card filter-card",
      h2(class = "section-title", "Retention Filters"),
      fluidRow(
        column(3, selectizeInput(ns("primary_institution"), "Primary Institution", choices = NULL, selected = NULL, multiple = FALSE)),
        column(
          5,
          selectizeInput(
            ns("comparison_institutions"),
            "Comparison Institutions",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Select comparison institutions")
          )
        ),
        column(2, selectInput(ns("attendance_status"), "Attendance status", choices = ra_rate_choices, selected = "Full-time")),
        column(2, selectInput(ns("ranking_method"), "Ranking method", choices = ra_ranking_methods, selected = "Latest common year"))
      ),
      fluidRow(
        column(4, sliderInput(ns("year_range"), "Year range", min = 2015, max = 2024, value = c(2015, 2024), step = 1, sep = "")),
        column(4, selectInput(ns("rank_attendance_status"), "Ranking attendance status", choices = ra_rank_status_choices, selected = "Full-time")),
        column(4, radioButtons(ns("ftpt_display"), "Full-time versus part-time display", choices = c("Retention Rates", "Retention Gap"), selected = "Retention Rates", inline = TRUE))
      )
    ),
    div(
      class = "ra-kpi-grid",
      ra_kpi_card_ui("kpi_latest_rate", "Latest retention rate", ns),
      ra_kpi_card_ui("kpi_previous_change", "Previous-year change", ns),
      ra_kpi_card_ui("kpi_average", "Ten-year average", ns),
      ra_kpi_card_ui("kpi_highest", "Highest retention rate", ns),
      ra_kpi_card_ui("kpi_lowest", "Lowest retention rate", ns),
      ra_kpi_card_ui("kpi_gap", "Retention gap", ns),
      ra_kpi_card_ui("kpi_peer_median", "Peer median comparison", ns),
      ra_kpi_card_ui("kpi_rank", "Peer rank", ns)
    ),
    div(class = "card", h2(class = "section-title", "Ten-Year Retention Trend"), ra_plot_output(ns("trend_plot"), "500px")),
    div(class = "card", h2(class = "section-title", "Caldwell Versus Peer Summary"), ra_plot_output(ns("peer_summary_plot"), "470px")),
    div(class = "card", h2(class = "section-title", "Full-Time Versus Part-Time Comparison"), ra_plot_output(ns("ftpt_plot"), "440px")),
    div(class = "card", h2(class = "section-title", "Latest-Year Retention Ranking"), ra_plot_output(ns("ranking_plot"), "520px")),
    div(class = "card", h2(class = "section-title", "Retention-Gap Heatmap"), ra_plot_output(ns("gap_heatmap"), "520px")),
    div(
      class = "card",
      h2(class = "section-title", "Year-over-Year Retention Change"),
      div(class = "ra-kpi-grid",
          ra_mini_kpi_ui("yoy_largest_improvement", "Largest improvement", ns),
          ra_mini_kpi_ui("yoy_largest_decline", "Largest decline", ns),
          ra_mini_kpi_ui("yoy_positive_negative", "Positive and negative years", ns),
          ra_mini_kpi_ui("yoy_average_change", "Average annual change", ns)
      ),
      ra_plot_output(ns("yoy_plot"), "450px")
    ),
    div(
      class = "card",
      h2(class = "section-title", "Retention Stability Analysis"),
      ra_plot_output(ns("stability_plot"), "470px"),
      DTOutput(ns("stability_table"))
    ),
    div(class = "card ra-note", h2(class = "section-title", "Dynamic Written Insight"), uiOutput(ns("written_insight"))),
    div(
      class = "card",
      h2(class = "section-title", "Detailed Downloadable Data"),
      DTOutput(ns("detail_table")),
      div(class = "ra-download", downloadButton(ns("download_detail"), "Download filtered retention CSV"))
    )
  )
}


# ============================================================
# Institutional Efficiency UI
# ============================================================
ie_css_ui <- function(ns) {
  dashboard_id <- ns("dashboard")
  tags$style(HTML(do.call(sprintf, c(list("
    #%s.ie-dashboard { --ie-accent: var(--accent, #b31b1b); --ie-muted: var(--muted, #6b7280); --ie-border: var(--border, #e8e8e8); }
    #%s .ie-note { background: #fff8f8; border-left: 5px solid var(--ie-accent); }
    #%s .ie-kpi-grid { display: grid; gap: 16px; grid-template-columns: repeat(3, minmax(0, 1fr)); }
    #%s .ie-kpi-card { border-left: 5px solid var(--ie-accent); min-height: 156px; white-space: pre-line; }
    #%s .ie-kpi-card .kpi-value { font-size: 18px; line-height: 1.35; }
    #%s .ie-filter-actions { display: grid; gap: 8px; grid-template-columns: repeat(3, minmax(0, 1fr)); margin-top: 25px; }
    #%s .ie-filter-actions .btn { width: 100%%; white-space: normal; }
    #%s .ie-small { color: var(--ie-muted); font-size: 12px; margin-top: 6px; }
    #%s .ie-tabset > .nav > li > a { font-weight: 650; }
    #%s .ie-download { margin-top: 10px; }
    @media (max-width: 1200px) { #%s .ie-kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
    @media (max-width: 900px) { #%s .ie-kpi-grid, #%s .ie-filter-actions { grid-template-columns: 1fr; } }
  "), rep(dashboard_id, 13)))))
}

ie_kpi_card_ui <- function(output_id, label, ns) {
  div(
    class = "card kpi-card ie-kpi-card",
    div(class = "kpi-label", label),
    div(class = "kpi-value", textOutput(ns(output_id), inline = TRUE))
  )
}

ie_metric_select_ui <- function(ns, input_id, label, choices, selected) {
  selectInput(ns(input_id), label, choices = choices, selected = selected)
}

ie_dashboard_ui <- function(id) {
  ns <- NS(id)

  div(
    id = ns("dashboard"),
    class = "ie-dashboard",
    ie_css_ui(ns),
    uiOutput(ns("data_validation_warning")),
    div(
      class = "card ie-note",
      h2(class = "section-title", "Institutional Efficiency"),
      p("These indicators are exploratory comparisons derived from IPEDS data. Differences in institutional mission, program mix, size, research activity and student population can affect the results. A higher or lower value does not automatically mean better performance.")
    ),
    div(
      class = "card filter-card",
      h2(class = "section-title", "Institutional Efficiency Filters"),
      fluidRow(
        column(3, selectizeInput(ns("primary_institution"), "Primary Institution", choices = NULL, selected = NULL, multiple = FALSE)),
        column(
          5,
          selectizeInput(
            ns("comparison_institutions"),
            "Comparison Institutions",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Select comparison institutions")
          )
        ),
        column(
          4,
          div(
            class = "ie-filter-actions",
            actionButton(ns("select_all_peers"), "Select all peers"),
            actionButton(ns("clear_peers"), "Clear peers"),
            actionButton(ns("caldwell_only"), "Caldwell only")
          )
        )
      ),
      fluidRow(
        column(3, sliderInput(ns("year_range"), "Year range", min = 2015, max = 2024, value = c(2015, 2024), step = 1, sep = "")),
        column(3, selectInput(ns("comparison_year"), "Common comparison year", choices = NULL)),
        column(3, selectInput(ns("ranking_method"), "Ranking method", choices = c("Latest common year", "Latest available year for each institution"), selected = "Latest common year")),
        column(3, checkboxInput(ns("show_trend_line"), "Trend line on scatterplots", value = FALSE))
      )
    ),
    div(
      class = "ie-tabset",
      tabsetPanel(
        tabPanel(
          "Overview",
          div(
            class = "ie-kpi-grid",
            ie_kpi_card_ui("overview_kpi_degrees_fte", "Degrees per 100 FTE", ns),
            ie_kpi_card_ui("overview_kpi_instruction_degree", "Instructional expenses per degree", ns),
            ie_kpi_card_ui("overview_kpi_core_degree", "Core expenses per degree", ns),
            ie_kpi_card_ui("overview_kpi_core_revenue_fte", "Core revenue per FTE", ns),
            ie_kpi_card_ui("overview_kpi_degrees_staff", "Degrees per instructional staff", ns),
            ie_kpi_card_ui("overview_kpi_staff_fte", "Staff headcount per 100 FTE", ns)
          ),
          div(class = "card", h2(class = "section-title", "Main Metric Trend"), ie_metric_select_ui(ns, "overview_metric", "Metric", ie_metric_choices(c("DegreesPer100FTE", "InstructionExpensesPerDegree", "CoreExpensesPerDegree", "CoreRevenuePerFTE", "TuitionRevenuePerFTE", "InstructionExpensesPerFTE", "StaffHeadcountPer100FTE", "DegreesPerInstructionalStaffFTE", "TuitionDependency")), "DegreesPer100FTE"), ie_plot_output(ns("overview_trend_plot"), "500px")),
          div(class = "card", h2(class = "section-title", "Executive Comparison"), ie_metric_select_ui(ns, "overview_bar_metric", "Metric", ie_metric_choices(c("DegreesPer100FTE", "InstructionExpensesPerDegree", "CoreExpensesPerDegree", "CoreRevenuePerFTE", "TuitionRevenuePerFTE", "InstructionExpensesPerFTE", "StaffHeadcountPer100FTE", "DegreesPerInstructionalStaffFTE", "TuitionDependency")), "CoreRevenuePerFTE"), ie_plot_output(ns("overview_bar_plot"), "390px")),
          div(class = "card ie-note", h2(class = "section-title", "Dynamic Summary"), uiOutput(ns("overview_summary")))
        ),
        tabPanel(
          "Degree Productivity",
          div(
            class = "ie-kpi-grid",
            ie_kpi_card_ui("degree_kpi_total", "Total degrees awarded", ns),
            ie_kpi_card_ui("degree_kpi_bachelors", "Bachelor's degrees", ns),
            ie_kpi_card_ui("degree_kpi_masters", "Master's degrees", ns),
            ie_kpi_card_ui("degree_kpi_doctoral", "Doctoral degrees", ns),
            ie_kpi_card_ui("degree_kpi_per_fte", "Degrees per 100 FTE", ns),
            ie_kpi_card_ui("degree_kpi_per_staff", "Degrees per instructional staff", ns)
          ),
          div(class = "card", h2(class = "section-title", "Degree Trend"), ie_metric_select_ui(ns, "degree_metric", "Measure", ie_metric_choices(c("TotalDegreesAwarded", "BachelorsDegrees", "MastersDegrees", "DoctoralDegrees", "DegreesPer100FTE", "DegreesPerFullTimeInstructionalStaff", "DegreesPerInstructionalStaffFTE")), "TotalDegreesAwarded"), ie_plot_output(ns("degree_trend_plot"), "470px")),
          fluidRow(
            column(6, div(class = "card", h2(class = "section-title", "Award-Level Composition"), radioButtons(ns("degree_composition_mode"), NULL, choices = c("Primary over time", "Selected institutions for one year"), selected = "Primary over time", inline = TRUE), ie_plot_output(ns("degree_composition_plot"), "430px"))),
            column(6, div(class = "card", h2(class = "section-title", "Degrees Versus FTE"), p(class = "ie-small", "This relationship measures degree production relative to institutional size. It is not a graduation-rate measure."), ie_plot_output(ns("degree_scatter_plot"), "430px")))
          ),
          div(class = "card", h2(class = "section-title", "Degree-Productivity Ranking"), ie_metric_select_ui(ns, "degree_rank_metric", "Metric", ie_metric_choices(c("DegreesPer100FTE", "DegreesPerFullTimeInstructionalStaff", "DegreesPerInstructionalStaffFTE")), "DegreesPer100FTE"), ie_plot_output(ns("degree_ranking_plot"), "520px")),
          div(class = "card ie-note", h2(class = "section-title", "Dynamic Summary"), uiOutput(ns("degree_summary")))
        ),
        tabPanel(
          "Expenses",
          div(class = "card", h2(class = "section-title", "Expense Trend"), ie_metric_select_ui(ns, "expense_metric", "Expense metric", ie_metric_choices(c("InstructionExpensesPerDegree", "CoreExpensesPerDegree", "InstructionExpensesPerFTE", "StudentServicesExpensesPerFTE", "AcademicSupportExpensesPerFTE", "InstitutionalSupportExpensesPerFTE")), "InstructionExpensesPerDegree"), ie_plot_output(ns("expense_trend_plot"), "470px")),
          fluidRow(
            column(6, div(class = "card", h2(class = "section-title", "Expense Comparison"), radioButtons(ns("expense_basis"), NULL, choices = c("Per degree", "Per FTE"), selected = "Per degree", inline = TRUE), ie_plot_output(ns("expense_comparison_plot"), "430px"))),
            column(6, div(class = "card", h2(class = "section-title", "Expense Composition"), uiOutput(ns("expense_composition_warning")), ie_plot_output(ns("expense_composition_plot"), "430px")))
          ),
          div(class = "card ie-note", p("Lower spending does not automatically indicate greater efficiency or educational quality.")),
          div(class = "card", h2(class = "section-title", "Expense Table"), DTOutput(ns("expense_table")), div(class = "ie-download", downloadButton(ns("download_expense"), "Download filtered expense CSV"))),
          div(class = "card ie-note", h2(class = "section-title", "Dynamic Summary"), uiOutput(ns("expense_summary")))
        ),
        tabPanel(
          "Revenue & Tuition",
          div(
            class = "ie-kpi-grid",
            ie_kpi_card_ui("revenue_kpi_core", "Total core revenue", ns),
            ie_kpi_card_ui("revenue_kpi_tuition", "Tuition and fee revenue", ns),
            ie_kpi_card_ui("revenue_kpi_core_fte", "Core revenue per FTE", ns),
            ie_kpi_card_ui("revenue_kpi_tuition_fte", "Tuition revenue per FTE", ns),
            ie_kpi_card_ui("revenue_kpi_dependency", "Tuition dependency", ns),
            ie_kpi_card_ui("revenue_kpi_peer_dependency", "Peer-median tuition dependency", ns)
          ),
          div(class = "card", h2(class = "section-title", "Revenue Trend"), ie_metric_select_ui(ns, "revenue_metric", "Revenue metric", ie_metric_choices(c("TotalCoreRevenue", "TuitionAndFeeRevenue", "CoreRevenuePerFTE", "TuitionRevenuePerFTE", "TuitionDependency")), "CoreRevenuePerFTE"), ie_plot_output(ns("revenue_trend_plot"), "470px")),
          div(class = "card", h2(class = "section-title", "Tuition Dependency"), p(class = "ie-small", "The percentage of total core revenue coming from tuition and fees."), ie_plot_output(ns("tuition_dependency_plot"), "430px")),
          fluidRow(
            column(6, div(class = "card", h2(class = "section-title", "Revenue Versus Enrollment"), ie_plot_output(ns("revenue_enrollment_scatter"), "430px"))),
            column(6, div(class = "card", h2(class = "section-title", "Tuition Revenue Versus Degrees"), ie_plot_output(ns("tuition_degrees_scatter"), "430px")))
          ),
          div(class = "card ie-note", h2(class = "section-title", "Dynamic Summary"), uiOutput(ns("revenue_summary")))
        ),
        tabPanel(
          "Staffing",
          div(
            class = "ie-kpi-grid",
            ie_kpi_card_ui("staff_kpi_ft_instruction", "Full-time instructional staff", ns),
            ie_kpi_card_ui("staff_kpi_pt_instruction", "Part-time instructional staff", ns),
            ie_kpi_card_ui("staff_kpi_total", "Total staff headcount", ns),
            ie_kpi_card_ui("staff_kpi_per_fte", "Staff headcount per 100 FTE", ns),
            ie_kpi_card_ui("staff_kpi_degrees_ft", "Degrees per full-time instructional staff", ns),
            ie_kpi_card_ui("staff_kpi_degrees_fte", "Degrees per instructional staff FTE", ns)
          ),
          div(class = "card", h2(class = "section-title", "Staffing Trend"), ie_metric_select_ui(ns, "staff_metric", "Staffing measure", ie_metric_choices(c("FullTimeInstructionalStaff", "PartTimeInstructionalStaff", "TotalStaffHeadcount", "StaffHeadcountPer100FTE", "StaffFTEPer100StudentFTE", "DegreesPerFullTimeInstructionalStaff", "DegreesPerInstructionalStaffFTE")), "StaffHeadcountPer100FTE"), ie_plot_output(ns("staff_trend_plot"), "470px")),
          fluidRow(
            column(6, div(class = "card", h2(class = "section-title", "Staffing Composition"), ie_plot_output(ns("staff_composition_plot"), "430px"))),
            column(6, div(class = "card", h2(class = "section-title", "Enrollment Versus Staffing Growth"), ie_plot_output(ns("staff_index_plot"), "430px")))
          ),
          div(class = "card", h2(class = "section-title", "Staffing Ranking"), ie_metric_select_ui(ns, "staff_rank_metric", "Metric", ie_metric_choices(c("StaffHeadcountPer100FTE", "DegreesPerInstructionalStaffFTE", "FullTimeInstructionalStaff")), "StaffHeadcountPer100FTE"), ie_plot_output(ns("staff_ranking_plot"), "520px")),
          div(class = "card ie-note", h2(class = "section-title", "Dynamic Summary"), uiOutput(ns("staff_summary")))
        ),
        tabPanel(
          "Peer Comparison",
          div(class = "card", h2(class = "section-title", "Latest-Year Ranking"), ie_metric_select_ui(ns, "peer_metric", "Metric", ie_metric_choices(c("DegreesPer100FTE", "DegreesPerFullTimeInstructionalStaff", "DegreesPerInstructionalStaffFTE", "InstructionExpensesPerDegree", "CoreExpensesPerDegree", "InstructionExpensesPerFTE", "StudentServicesExpensesPerFTE", "AcademicSupportExpensesPerFTE", "InstitutionalSupportExpensesPerFTE", "CoreRevenuePerFTE", "TuitionRevenuePerFTE", "TuitionDependency", "StaffHeadcountPer100FTE", "StaffFTEPer100StudentFTE", "InstructionShareOfCoreExpenses", "StudentServicesShareOfCoreExpenses", "AcademicSupportShareOfCoreExpenses", "InstitutionalSupportShareOfCoreExpenses")), "DegreesPer100FTE"), ie_plot_output(ns("peer_ranking_plot"), "540px")),
          div(class = "card", h2(class = "section-title", "Peer Trend Comparison"), ie_plot_output(ns("peer_trend_plot"), "470px")),
          div(class = "card", h2(class = "section-title", "Institutional Profile"), ie_plot_output(ns("profile_plot"), "520px")),
          div(class = "card ie-note", h2(class = "section-title", "Dynamic Summary"), uiOutput(ns("peer_summary"))),
          div(class = "card", h2(class = "section-title", "Detailed Data Table"), DTOutput(ns("detail_table")))
        )
      )
    )
  )
}


ui <- fluidPage(
  tags$head(
    tags$title("University Enrollment Dashboard"),
    tags$style(HTML("
      :root {
        --accent: #b31b1b;
        --accent-dark: #8f1414;
        --border: #e8e8e8;
        --text: #252525;
        --muted: #6b7280;
        --bg: #ffffff;
        --soft-bg: #f7f7f8;
      }

      body {
        background: var(--bg);
        color: var(--text);
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }

      .dashboard-shell {
        max-width: 1440px;
        margin: 0 auto;
        padding: 24px 22px 36px;
      }

      .dashboard-header {
        align-items: center;
        border-bottom: 3px solid var(--accent);
        display: flex;
        gap: 20px;
        margin-bottom: 20px;
        padding-bottom: 14px;
      }

      .dashboard-logo {
        height: 72px;
        width: auto;
      }

      .dashboard-header h1 {
        font-size: 30px;
        font-weight: 700;
        margin: 0 0 6px;
      }

      .dashboard-header p {
        color: var(--muted);
        font-size: 15px;
        margin: 0;
      }

      .card {
        background: #fff;
        border: 1px solid var(--border);
        border-radius: 8px;
        box-shadow: 0 8px 22px rgba(0, 0, 0, 0.07);
        margin-bottom: 18px;
        padding: 18px;
      }

      .filter-card label {
        color: #2d2d2d;
        font-weight: 650;
      }

      .btn-reset {
        background: var(--accent);
        border-color: var(--accent);
        color: #fff;
        font-weight: 650;
        width: 100%;
      }

      .btn-reset:hover,
      .btn-reset:focus {
        background: var(--accent-dark);
        border-color: var(--accent-dark);
        color: #fff;
      }

      .kpi-grid {
        display: grid;
        gap: 16px;
        grid-template-columns: repeat(3, minmax(0, 1fr));
      }

      .e12-kpi-grid-four {
        grid-template-columns: repeat(4, minmax(0, 1fr));
      }

      .kpi-card {
        border-left: 5px solid var(--accent);
        min-height: 110px;
      }

      .kpi-label {
        color: var(--muted);
        font-size: 13px;
        font-weight: 700;
        letter-spacing: 0.02em;
        text-transform: uppercase;
      }

      .kpi-value {
        color: var(--accent);
        font-size: 30px;
        font-weight: 750;
        line-height: 1.2;
        margin-top: 12px;
      }

      .section-title {
        font-size: 19px;
        font-weight: 700;
        margin: 0 0 14px;
      }

      .selectize-input.focus {
        border-color: var(--accent);
        box-shadow: 0 0 0 3px rgba(179, 27, 27, 0.12);
      }

      .irs-bar,
      .irs-from,
      .irs-to,
      .irs-single {
        background: var(--accent) !important;
        border-color: var(--accent) !important;
      }

      .irs-handle > i:first-child {
        background: var(--accent) !important;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: var(--accent) !important;
        border-color: var(--accent) !important;
        color: #fff !important;
      }

      @media (max-width: 1100px) {
        .e12-kpi-grid-four {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }

      @media (max-width: 900px) {
        .dashboard-header {
          align-items: flex-start;
          flex-direction: column;
        }

        .kpi-grid,
        .e12-kpi-grid-four {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  div(
    class = "dashboard-shell",
    div(
      class = "dashboard-header",
      img(src = "assets/CUlogo.png", class = "dashboard-logo", alt = "Caldwell University logo"),
      div(
        h1("University Enrollment Dashboard"),
        p("Filtered IPEDS enrollment comparison, 2010-2024")
      )
    ),
    tabsetPanel(
      tabPanel(
        "Enrollment Comparison",
        fluidRow(
          column(
            width = 3,
            div(
              class = "card filter-card",
              h2(class = "section-title", "Filters"),
              selectInput(
                inputId = "level_filter",
                label = "Institution level",
                choices = c(
                  "All" = "All",
                  "2-year" = "2-year",
                  "4-year" = "4-year or above"
                ),
                selected = "All"
              ),
              selectInput(
                inputId = "control_filter",
                label = "Institution control",
                choices = c(
                  "All" = "All",
                  "Public" = "Public",
                  "Private nonprofit" = "Private nonprofit",
                  "Private for-profit" = "Private for-profit"
                ),
                selected = "All"
              ),
              sliderInput(
                inputId = "year_range",
                label = "Year range",
                min = 2010,
                max = 2024,
                value = c(2010, 2024),
                step = 1,
                sep = ""
              ),
              selectizeInput(
                inputId = "selected_universities",
                label = "Universities",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Select one or more universities",
                  plugins = list("remove_button"),
                  maxOptions = 1000
                )
              ),
              actionButton(
                inputId = "reset_filters",
                label = "Reset filters",
                class = "btn-reset"
              )
            )
          ),
          column(
            width = 9,
            div(
              class = "kpi-grid",
              div(
                class = "card kpi-card",
                div(class = "kpi-label", "Selected universities"),
                div(class = "kpi-value", textOutput("kpi_universities", inline = TRUE))
              ),
              div(
                class = "card kpi-card",
                div(class = "kpi-label", "Latest-year total enrollment"),
                div(class = "kpi-value", textOutput("kpi_latest_total", inline = TRUE))
              ),
              div(
                class = "card kpi-card",
                div(class = "kpi-label", "Average enrollment"),
                div(class = "kpi-value", textOutput("kpi_average", inline = TRUE))
              )
            ),
            div(
              class = "card",
              h2(class = "section-title", "Enrollment Over Time"),
              plotOutput("enrollment_plot", height = "500px")
            ),
            div(
              class = "card",
              h2(class = "section-title", "Filtered Data"),
              DTOutput("enrollment_table")
            )
          )
        )
      ),
      tabPanel(
        "12-Month Enrollment",
        e12_shared_filters_ui(),
        tabsetPanel(
          tabPanel(
            "Demographics",
            div(
              class = "card filter-card",
              fluidRow(
                column(4, selectInput("e12_demo_student_level", "Student level", choices = NULL)),
                column(4, selectInput("e12_demo_gender", "Gender", choices = NULL)),
                column(4, selectInput("e12_demo_ethnicity", "Ethnicity", choices = NULL))
              )
            ),
            div(
              class = "kpi-grid e12-kpi-grid-four",
              e12_kpi_card_ui("e12_demo_latest_enrollment", "Latest enrollment"),
              e12_kpi_card_ui("e12_demo_enrollment_change", "Enrollment change"),
              e12_kpi_card_ui("e12_demo_percentage_growth", "Percentage growth"),
              e12_kpi_card_ui("e12_demo_average_enrollment", "Average annual enrollment")
            ),
            div(
              class = "card",
              h2(class = "section-title", "12-Month Enrollment by Demographic"),
              plotOutput("e12_demo_plot", height = "470px")
            ),
            div(
              class = "card",
              h2(class = "section-title", "Filtered Demographic Rows"),
              DTOutput("e12_demo_table")
            )
          ),
          tabPanel(
            "Undergraduate and Graduate Enrollment",
            div(
              class = "card filter-card",
              fluidRow(
                column(6, selectInput("e12_attendance_student_level", "Student level", choices = NULL)),
                column(6, selectInput("e12_attendance_status", "Attendance status", choices = NULL))
              )
            ),
            div(
              class = "kpi-grid e12-kpi-grid-four",
              e12_kpi_card_ui("e12_attendance_latest_enrollment", "Latest enrollment"),
              e12_kpi_card_ui("e12_attendance_full_time", "Full-time enrollment"),
              e12_kpi_card_ui("e12_attendance_part_time", "Part-time enrollment"),
              e12_kpi_card_ui("e12_attendance_full_time_pct", "Full-time percentage")
            ),
            div(
              class = "card",
              h2(class = "section-title", "Undergraduate and Graduate Enrollment"),
              plotOutput("e12_attendance_plot", height = "470px")
            ),
            div(
              class = "card",
              h2(class = "section-title", "Filtered Attendance Rows"),
              DTOutput("e12_attendance_table")
            )
          ),
          tabPanel(
            "FTE",
            div(
              class = "kpi-grid e12-kpi-grid-four",
              e12_kpi_card_ui("e12_fte_latest_total", "Latest total FTE"),
              e12_kpi_card_ui("e12_fte_growth", "FTE growth"),
              e12_kpi_card_ui("e12_fte_average", "Average FTE"),
              e12_kpi_card_ui("e12_fte_ratio", "Latest FTE-to-headcount ratio")
            ),
            div(
              class = "card",
              h2(class = "section-title", "FTE Trend"),
              plotOutput("e12_fte_trend", height = "430px")
            ),
            div(
              class = "card",
              h2(class = "section-title", "Headcount vs. FTE"),
              plotOutput("e12_fte_headcount_plot", height = "430px")
            ),
            div(
              class = "card",
              h2(class = "section-title", "FTE by Institution"),
              plotOutput("e12_fte_institution_bar", height = "430px")
            ),
            div(
              class = "card",
              h2(class = "section-title", "Filtered FTE Rows"),
              DTOutput("e12_fte_table")
            )
          )
        )
      ),
      tabPanel(
        "Graduation Rate Analytics",
        gr_dashboard_ui("graduation_rate")
      ),
      tabPanel(
        "Retention Analysis",
        ra_dashboard_ui("retention_analysis")
      ),
      tabPanel(
        "Institutional Efficiency",
        ie_dashboard_ui("institutional_efficiency")
      )
    )
  )
)
