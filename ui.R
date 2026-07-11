library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #f4f6f9;
          font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
          color: #2f3437;
        }

        #title_area {
          background-color: #ffffff;
          display: flex;
          align-items: center;
          padding: 14px 28px;
          border-bottom: 4px solid #CC0000;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
          margin-bottom: 18px;
        }

        .main-container {
          width: 100%;
          max-width: 1500px;
          margin: 0 auto;
          padding: 0 18px 24px 18px;
        }

        .nav-tabs {
          margin-bottom: 16px;
        }

        .control-card,
        .dashboard-card,
        .kpi-card {
          background: #ffffff;
          border: 1px solid #e1e5ea;
          border-radius: 8px;
          box-shadow: 0 3px 8px rgba(0,0,0,0.05);
        }

        .control-card {
          padding: 14px 16px 4px 16px;
          margin-bottom: 14px;
        }

        .dashboard-card {
          padding: 16px;
          margin-bottom: 16px;
        }

        .kpi-card {
          padding: 18px;
          border-top: 5px solid #CC0000;
          text-align: center;
          min-height: 112px;
          margin-bottom: 14px;
        }

        .kpi-number {
          font-size: 28px;
          font-weight: bold;
          color: #CC0000;
          word-wrap: break-word;
        }

        .kpi-label {
          font-size: 14px;
          color: #666666;
        }

        .nursing-kpi-card {
          min-height: 104px;
          padding: 14px 12px;
        }

        .nursing-kpi-card .kpi-number {
          font-size: 23px;
          line-height: 1.2;
        }

        .nursing-kpi-card .kpi-label {
          min-height: 36px;
          display: flex;
          align-items: center;
          justify-content: center;
        }

        .section-title {
          font-weight: 700;
          color: #333333;
          margin-top: 0;
          margin-bottom: 6px;
        }

        .section-subtitle {
          color: #666666;
          margin-bottom: 14px;
        }

        .plot-scroll {
          max-height: 780px;
          overflow-y: auto;
          overflow-x: hidden;
          padding-right: 8px;
        }

        .hover-box {
          background: #f8fafc;
          border-left: 4px solid #CC0000;
          padding: 10px 12px;
          margin-top: 12px;
          line-height: 1.55;
          color: #333333;
        }

        .hover-box strong {
          color: #222222;
        }

        .summary-callout {
          background: #f8fafc;
          border-left: 5px solid #CC0000;
          padding: 14px 16px;
          font-size: 16px;
          line-height: 1.6;
          color: #2f3437;
        }
      ")
    )
  ),

  div(
    id = "title_area",
    img(src = "assets/CUlogo.png", height = "92px", style = "margin-right: 24px;"),
    div(
      h2("Executive Academic Dashboard", style = "font-weight: 800; margin: 0; color: #333;"),
      p("Office of Institutional Research - Intern Sandbox", style = "margin: 0; color: #666; font-style: italic;")
    )
  ),

  div(
    class = "main-container",
    tabsetPanel(
      tabPanel(
        "Enrollment Comparison",
        div(
          class = "control-card",
          fluidRow(
            column(
              8,
              selectizeInput(
                "school",
                "Select School:",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(placeholder = "Search for one or more universities")
              )
            ),
            column(
              4,
              sliderInput(
                "years",
                "Select Academic Years Range:",
                min = 2020,
                max = 2024,
                value = c(2020, 2024),
                step = 1,
                sep = ""
              )
            )
          )
        ),

        fluidRow(
          column(
            6,
            div(
              class = "kpi-card",
              div(class = "kpi-number", textOutput("kpi_enrollment")),
              div(class = "kpi-label", "Latest Enrollment")
            )
          ),
          column(
            6,
            div(
              class = "kpi-card",
              div(class = "kpi-number", textOutput("kpi_growth")),
              div(class = "kpi-label", "Enrollment Growth")
            )
          )
        ),

        div(
          class = "dashboard-card",
          h3("Comparison Line Graph of Each University", class = "section-title"),
          plotOutput("enrollment_trend", height = "440px")
        ),

        div(
          class = "dashboard-card",
          h3("Enrollment Summary", class = "section-title"),
          tableOutput("enrollment_summary")
        )
      ),

      tabPanel(
        "Radar Chart",
        div(
          class = "control-card",
          fluidRow(
            column(
              8,
              selectizeInput(
                "radar_school",
                "Select School:",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(placeholder = "Search for one or more universities")
              )
            ),
            column(
              4,
              sliderInput(
                "radar_years",
                "Select Academic Years Range:",
                min = 2020,
                max = 2024,
                value = c(2020, 2024),
                step = 1,
                sep = ""
              )
            )
          )
        ),

        div(
          class = "dashboard-card",
          h3("Enrollment Profile Radar Chart", class = "section-title"),
          plotOutput("enrollment_radar", height = "520px")
        )
      ),

      tabPanel(
        "Gender Comparison",
        div(
          class = "control-card",
          fluidRow(
            column(
              12,
              selectizeInput(
                "gender_universities",
                "Select Universities:",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(placeholder = "Search for one or more universities")
              )
            )
          ),
          fluidRow(
            column(
              3,
              radioButtons(
                "gender_year_mode",
                "Year Selection Mode:",
                choices = c(
                  "Single Year" = "single",
                  "Average Across Years" = "average"
                ),
                selected = "single"
              )
            ),
            column(
              3,
              conditionalPanel(
                condition = "input.gender_year_mode == 'single'",
                selectInput(
                  "gender_single_year",
                  "Select Year:",
                  choices = 2020:2024,
                  selected = 2024
                )
              ),
              conditionalPanel(
                condition = "input.gender_year_mode == 'average'",
                sliderInput(
                  "gender_year_range",
                  "Select Year Range:",
                  min = 2020,
                  max = 2024,
                  value = c(2020, 2024),
                  step = 1,
                  sep = ""
                )
              )
            ),
            column(
              6,
              selectInput(
                "gender_sort_by",
                "Sort Universities By:",
                choices = c(
                  "Alphabetical (A-Z)" = "az",
                  "Alphabetical (Z-A)" = "za",
                  "Female % (Highest to Lowest)" = "female_high",
                  "Female % (Lowest to Highest)" = "female_low",
                  "Male % (Highest to Lowest)" = "male_high",
                  "Male % (Lowest to Highest)" = "male_low"
                ),
                selected = "female_high"
              )
            )
          )
        ),

        div(
          class = "dashboard-card",
          h3("University Gender Composition", class = "section-title"),
          p("Each horizontal bar totals 100% and compares male and female enrollment share.", class = "section-subtitle"),
          div(class = "plot-scroll", uiOutput("gender_plot_ui")),
          uiOutput("gender_hover_details")
        ),

        div(
          class = "dashboard-card",
          h3("Gender Summary Table", class = "section-title"),
          dataTableOutput("gender_summary")
        )
      ),

      tabPanel(
        "Nursing Graduate Trend",
        div(
          class = "control-card",
          fluidRow(
            column(
              8,
              h3("Caldwell University BSN Nursing Graduates", class = "section-title"),
              p("Dynamic completion trend and administrative KPIs for the selected year range.", class = "section-subtitle")
            ),
            column(
              4,
              sliderInput(
                "nursing_year_range",
                "Select Year Range:",
                min = 2012,
                max = 2024,
                value = c(2012, 2024),
                step = 1,
                sep = ""
              )
            )
          )
        ),

        fluidRow(
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_total_graduates")), div(class = "kpi-label", "Total Graduates in Selected Range"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_first_year")), div(class = "kpi-label", "First Year in Selected Range"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_first_year_graduates")), div(class = "kpi-label", "First Year Graduates"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_latest_year")), div(class = "kpi-label", "Latest Year in Selected Range")))
        ),

        fluidRow(
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_latest_year_graduates")), div(class = "kpi-label", "Latest Year Graduates"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_highest_year")), div(class = "kpi-label", "Highest Graduation Year"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_highest_graduates")), div(class = "kpi-label", "Highest Number of Graduates"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_lowest_year")), div(class = "kpi-label", "Lowest Graduation Year")))
        ),

        fluidRow(
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_lowest_graduates")), div(class = "kpi-label", "Lowest Number of Graduates"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_average_graduates")), div(class = "kpi-label", "Average Graduates per Year"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_growth")), div(class = "kpi-label", "Growth Since First Selected Year"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_cagr")), div(class = "kpi-label", "CAGR for Selected Range")))
        ),

        fluidRow(
          column(6, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_largest_increase")), div(class = "kpi-label", "Largest Year-over-Year Increase"))),
          column(6, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("nursing_largest_decrease")), div(class = "kpi-label", "Largest Year-over-Year Decrease")))
        ),

        div(
          class = "dashboard-card",
          h3("Annual BSN Nursing Graduate Trend", class = "section-title"),
          p("Hover over a point to view the exact graduate count for that year.", class = "section-subtitle"),
          plotOutput(
            "nursing_trend",
            height = "460px",
            hover = hoverOpts(
              id = "nursing_hover",
              delay = 80,
              delayType = "debounce",
              nullOutside = TRUE
            )
          ),
          uiOutput("nursing_hover_details")
        ),

        div(
          class = "dashboard-card",
          h3("Executive Summary", class = "section-title"),
          div(class = "summary-callout", textOutput("nursing_executive_summary"))
        ),

        div(
          class = "dashboard-card",
          h3("Selected Range Data Table", class = "section-title"),
          tableOutput("nursing_table")
        )
      ),

      tabPanel(
        "Admissions Dashboard",
        div(
          class = "control-card",
          fluidRow(
            column(
              8,
              selectizeInput(
                "admissions_schools",
                "Select Schools:",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(placeholder = "Search for one or more universities")
              )
            ),
            column(
              4,
              sliderInput(
                "admissions_years",
                "Select Year Range:",
                min = 2010,
                max = 2024,
                value = c(2010, 2024),
                step = 1,
                sep = ""
              )
            )
          )
        ),

        fluidRow(
          column(2, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("admissions_applications")), div(class = "kpi-label", "Applications"))),
          column(2, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("admissions_accepted")), div(class = "kpi-label", "Accepted"))),
          column(2, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("admissions_enrolled")), div(class = "kpi-label", "Enrolled"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("admissions_acceptance_rate")), div(class = "kpi-label", "Acceptance Rate"))),
          column(3, div(class = "kpi-card nursing-kpi-card", div(class = "kpi-number", textOutput("admissions_yield_rate")), div(class = "kpi-label", "Yield Rate")))
        ),

        fluidRow(
          column(
            5,
            div(
              class = "dashboard-card",
              h3("Admissions Funnel", class = "section-title"),
              plotOutput("admissions_funnel", height = "390px")
            )
          ),
          column(
            7,
            div(
              class = "dashboard-card",
              h3("Admissions Counts Over Time", class = "section-title"),
              selectInput(
                "admissions_line_metric",
                "Select Metrics:",
                choices = c("Applications", "Accepted", "Enrolled"),
                selected = "Applications",
                multiple = TRUE
              ),
              plotOutput("admissions_line", height = "390px")
            )
          )
        ),

        div(
          class = "dashboard-card",
          h3("Selected University Rate Comparison", class = "section-title"),
          plotOutput("admissions_rate_bar", height = "460px")
        )
      )
    )
  )
)
