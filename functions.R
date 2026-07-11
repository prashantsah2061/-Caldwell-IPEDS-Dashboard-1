library(dplyr)

# Load and prepare the IPEDS enrollment data.
load_enrollment_data <- function(file_path) {
  read.csv(file_path, stringsAsFactors = FALSE) %>%
    mutate(
      Year = as.integer(Year),
      UNITID = as.integer(UNITID),
      TotalEnrollment = as.numeric(TotalEnrollment),
      Men = as.numeric(Men),
      Women = as.numeric(Women)
    )
}

# Format whole-number counts with commas.
format_count <- function(x) {
  format(round(x, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Format dashboard percentages with one decimal place.
format_percent <- function(x) {
  paste0(round(x, 1), "%")
}

# Build searchable choices for the Enrollment Comparison tab.
build_enrollment_choices <- function(enrollment_data) {
  school_choices <- enrollment_data %>%
    distinct(UNITID, Institution, City, State) %>%
    arrange(Institution) %>%
    mutate(Label = paste0(Institution, " (", City, ", ", State, ")"))

  setNames(as.character(school_choices$UNITID), school_choices$Label)
}

# Build searchable choices for the Gender Comparison tab.
build_gender_choices <- function(enrollment_data) {
  enrollment_data %>%
    distinct(Institution) %>%
    arrange(Institution) %>%
    pull(Institution)
}

# Filter rows for the Enrollment Comparison tab.
filter_enrollment_data <- function(enrollment_data, selected_schools, selected_years) {
  enrollment_data %>%
    filter(
      UNITID %in% as.integer(selected_schools),
      Year >= selected_years[1],
      Year <= selected_years[2]
    )
}

# Summarize gender counts using either a single year or an average year range.
summarize_gender_data <- function(enrollment_data, universities, year_mode, single_year, year_range) {
  if (year_mode == "single") {
    enrollment_data %>%
      filter(
        Institution %in% universities,
        Year == as.integer(single_year)
      ) %>%
      group_by(Institution) %>%
      summarise(
        YearLabel = as.character(as.integer(single_year)),
        TotalEnrollment = sum(TotalEnrollment, na.rm = TRUE),
        Men = sum(Men, na.rm = TRUE),
        Women = sum(Women, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    enrollment_data %>%
      filter(
        Institution %in% universities,
        Year >= year_range[1],
        Year <= year_range[2]
      ) %>%
      group_by(Institution) %>%
      summarise(
        YearLabel = paste0("Average ", year_range[1], "-", year_range[2]),
        TotalEnrollment = mean(TotalEnrollment, na.rm = TRUE),
        Men = mean(Men, na.rm = TRUE),
        Women = mean(Women, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

# Add male and female percentages after counts have been summarized.
add_gender_percentages <- function(gender_data) {
  gender_data %>%
    mutate(
      MalePercent = ifelse(TotalEnrollment > 0, (Men / TotalEnrollment) * 100, NA_real_),
      FemalePercent = ifelse(TotalEnrollment > 0, (Women / TotalEnrollment) * 100, NA_real_)
    )
}

# Sort gender data based on the dashboard sort dropdown.
sort_gender_data <- function(gender_data, sort_by) {
  if (sort_by == "az") {
    gender_data <- gender_data %>% arrange(Institution)
  } else if (sort_by == "za") {
    gender_data <- gender_data %>% arrange(desc(Institution))
  } else if (sort_by == "female_high") {
    gender_data <- gender_data %>% arrange(desc(FemalePercent), Institution)
  } else if (sort_by == "female_low") {
    gender_data <- gender_data %>% arrange(FemalePercent, Institution)
  } else if (sort_by == "male_high") {
    gender_data <- gender_data %>% arrange(desc(MalePercent), Institution)
  } else if (sort_by == "male_low") {
    gender_data <- gender_data %>% arrange(MalePercent, Institution)
  }

  gender_data %>%
    mutate(Institution = factor(Institution, levels = rev(Institution)))
}

# Convert one row per university into male/female rows for the stacked bar chart.
make_stacked_gender_data <- function(gender_data) {
  bind_rows(
    gender_data %>%
      transmute(
        Institution,
        YearLabel,
        Gender = "Male",
        Percent = MalePercent,
        Men,
        Women,
        TotalEnrollment,
        MalePercent,
        FemalePercent
      ),
    gender_data %>%
      transmute(
        Institution,
        YearLabel,
        Gender = "Female",
        Percent = FemalePercent,
        Men,
        Women,
        TotalEnrollment,
        MalePercent,
        FemalePercent
      )
  ) %>%
    mutate(Gender = factor(Gender, levels = c("Male", "Female")))
}

# Keep charts readable when many universities are selected.
gender_plot_height_value <- function(number_of_universities) {
  max(460, number_of_universities * 34 + 140)
}

# Load Caldwell BSN Nursing completion data and use beginner-friendly column names.
load_nursing_data <- function(file_path) {
  read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE) %>%
    transmute(
      Year = as.integer(Year),
      Institution = Institution,
      CIPCode = `CIP Code`,
      CIPDescription = `CIP Description`,
      AwardLevel = `Award Level`,
      TotalGraduates = as.numeric(`Total Graduate in that year`),
      MaleGraduates = as.numeric(`Male Graduate`),
      FemaleGraduates = as.numeric(`Female Graduate`),
      White = as.numeric(White),
      Black = as.numeric(Black),
      Hispanic = as.numeric(Hispanic),
      Asian = as.numeric(Asian),
      NativeAmerican = as.numeric(`Native American`),
      PacificIslander = as.numeric(`Pacific Islander`),
      TwoOrMoreRaces = as.numeric(`Two or More Races`),
      RaceUnknown = as.numeric(`Race Unknown`),
      NonresidentAlien = as.numeric(`Nonresident Alien`)
    ) %>%
    arrange(Year)
}

# Filter nursing data to the selected year range.
filter_nursing_data <- function(nursing_data, selected_years) {
  nursing_data %>%
    filter(
      Year >= selected_years[1],
      Year <= selected_years[2]
    ) %>%
    arrange(Year)
}

# Calculate all dynamic KPIs from the same filtered nursing dataset.
summarize_nursing_kpis <- function(nursing_data) {
  nursing_data <- nursing_data %>%
    arrange(Year) %>%
    mutate(YearOverYearChange = TotalGraduates - lag(TotalGraduates))

  first_row <- nursing_data %>% slice(1)
  latest_row <- nursing_data %>% slice(n())
  highest_row <- nursing_data %>% arrange(desc(TotalGraduates), Year) %>% slice(1)
  lowest_row <- nursing_data %>% arrange(TotalGraduates, Year) %>% slice(1)
  increase_row <- nursing_data %>% filter(!is.na(YearOverYearChange), YearOverYearChange > 0) %>% arrange(desc(YearOverYearChange), Year) %>% slice(1)
  decrease_row <- nursing_data %>% filter(!is.na(YearOverYearChange), YearOverYearChange < 0) %>% arrange(YearOverYearChange, Year) %>% slice(1)

  first_graduates <- first_row$TotalGraduates
  latest_graduates <- latest_row$TotalGraduates
  number_of_years <- latest_row$Year - first_row$Year

  growth <- ifelse(
    first_graduates > 0,
    ((latest_graduates - first_graduates) / first_graduates) * 100,
    NA_real_
  )

  cagr <- ifelse(
    first_graduates > 0 && number_of_years > 0,
    ((latest_graduates / first_graduates)^(1 / number_of_years) - 1) * 100,
    NA_real_
  )

  list(
    total_graduates = sum(nursing_data$TotalGraduates, na.rm = TRUE),
    first_year = first_row$Year,
    first_year_graduates = first_graduates,
    latest_year = latest_row$Year,
    latest_year_graduates = latest_graduates,
    highest_year = highest_row$Year,
    highest_graduates = highest_row$TotalGraduates,
    lowest_year = lowest_row$Year,
    lowest_graduates = lowest_row$TotalGraduates,
    average_graduates = mean(nursing_data$TotalGraduates, na.rm = TRUE),
    growth = growth,
    cagr = cagr,
    largest_increase_year = ifelse(nrow(increase_row) == 0, NA_integer_, increase_row$Year),
    largest_increase = ifelse(nrow(increase_row) == 0, NA_real_, increase_row$YearOverYearChange),
    largest_decrease_year = ifelse(nrow(decrease_row) == 0, NA_integer_, decrease_row$Year),
    largest_decrease = ifelse(nrow(decrease_row) == 0, NA_real_, decrease_row$YearOverYearChange)
  )
}

# Format percentages and handle one-year ranges where CAGR cannot be calculated.
format_metric_percent <- function(x) {
  ifelse(is.na(x), "N/A", paste0(round(x, 1), "%"))
}

# Build a short executive summary for administrators.
build_nursing_summary <- function(kpis) {
  trend_phrase <- if (is.na(kpis$growth)) {
    "The selected range does not support a growth calculation."
  } else if (kpis$growth > 5) {
    "The long-term trend indicates program growth."
  } else if (kpis$growth < -5) {
    "The long-term trend indicates a decline in annual graduates."
  } else {
    "The long-term trend is relatively stable."
  }

  paste0(
    "Between ", kpis$first_year, " and ", kpis$latest_year,
    ", Caldwell University's BSN Nursing program graduated ",
    format_count(kpis$total_graduates), " students. Annual graduates changed by ",
    format_metric_percent(kpis$growth), ", with the highest number recorded in ",
    kpis$highest_year, ". The selected range averaged ",
    format_count(kpis$average_graduates), " graduates per year. ",
    trend_phrase
  )
}

# Load and prepare admissions data from the admissions dashboard CSV.
load_admissions_data <- function(file_path) {
  read.csv(file_path, stringsAsFactors = FALSE) %>%
    mutate(
      Year = as.integer(Year),
      UNITID = as.integer(UNITID),
      Applications = as.numeric(Applications),
      Accepted = as.numeric(Accepted),
      Enrolled = as.numeric(Enrolled)
    )
}

# Build searchable choices for the Admissions Dashboard tab.
build_admissions_choices <- function(admissions_data) {
  admissions_data %>%
    distinct(UNITID, Institution, State) %>%
    arrange(Institution) %>%
    mutate(Label = paste0(Institution, " (", State, ")")) %>%
    { setNames(as.character(.$UNITID), .$Label) }
}

# Add admissions rates using the required formulas.
add_admissions_rates <- function(admissions_data) {
  admissions_data %>%
    mutate(
      AcceptanceRate = ifelse(Applications > 0, (Accepted / Applications) * 100, NA_real_),
      YieldRate = ifelse(Accepted > 0, (Enrolled / Accepted) * 100, NA_real_)
    )
}

# Filter admissions rows by selected schools and year range.
filter_admissions_data <- function(admissions_data, selected_schools, selected_years) {
  admissions_data %>%
    filter(
      UNITID %in% as.integer(selected_schools),
      Year >= selected_years[1],
      Year <= selected_years[2]
    ) %>%
    add_admissions_rates()
}

# Summarize admissions KPIs from the filtered data.
summarize_admissions_kpis <- function(admissions_data) {
  applications <- sum(admissions_data$Applications, na.rm = TRUE)
  accepted <- sum(admissions_data$Accepted, na.rm = TRUE)
  enrolled <- sum(admissions_data$Enrolled, na.rm = TRUE)

  list(
    applications = applications,
    accepted = accepted,
    enrolled = enrolled,
    acceptance_rate = ifelse(applications > 0, (accepted / applications) * 100, NA_real_),
    yield_rate = ifelse(accepted > 0, (enrolled / accepted) * 100, NA_real_)
  )
}

# Convert admissions KPI counts into funnel rows.
make_admissions_funnel_data <- function(kpis) {
  data.frame(
    Stage = factor(c("Applications", "Accepted", "Enrolled"), levels = c("Applications", "Accepted", "Enrolled")),
    Count = c(kpis$applications, kpis$accepted, kpis$enrolled)
  )
}

# Summarize selected universities for acceptance-rate and yield-rate comparison.
summarize_admissions_comparison <- function(admissions_data) {
  admissions_data %>%
    group_by(Institution) %>%
    summarise(
      Applications = sum(Applications, na.rm = TRUE),
      Accepted = sum(Accepted, na.rm = TRUE),
      Enrolled = sum(Enrolled, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    add_admissions_rates() %>%
    tidyr::pivot_longer(
      cols = c(AcceptanceRate, YieldRate),
      names_to = "RateType",
      values_to = "Rate"
    ) %>%
    mutate(
      RateType = recode(
        RateType,
        AcceptanceRate = "Acceptance Rate",
        YieldRate = "Yield Rate"
      )
    )
}
