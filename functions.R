required_enrollment_columns <- c(
  "Institution",
  "Year",
  "TotalEnrollment",
  "InstitutionLevel",
  "Control",
  "UNITID"
)

cache_file_for <- function(file_path) {
  file.path(app_path("data"), ".cache", paste0(tools::file_path_sans_ext(basename(file_path)), ".rds"))
}

load_prepared_cache <- function(file_path) {
  cache_path <- cache_file_for(file_path)

  if (
    file.exists(cache_path) &&
      file.info(cache_path)$mtime >= file.info(file_path)$mtime
  ) {
    return(readRDS(cache_path))
  }

  NULL
}

save_prepared_cache <- function(file_path, data) {
  cache_path <- cache_file_for(file_path)
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data, cache_path)
  data
}

read_csv_fast <- function(file_path, select = NULL) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(
      data.table::fread(
        file_path,
        select = select,
        showProgress = FALSE,
        data.table = FALSE
      ) |>
        tibble::as_tibble()
    )
  }

  readr::read_csv(file_path, col_select = dplyr::all_of(select), show_col_types = FALSE)
}

resolve_enrollment_file <- function() {
  data_path <- app_path("data", "all_university_enrollment_2010_2024.csv")

  if (file.exists(data_path)) {
    return(data_path)
  }

  root_path <- app_path("all_university_enrollment_2010_2024.csv")

  if (file.exists(root_path)) {
    return(root_path)
  }

  stop(
    "Could not find all_university_enrollment_2010_2024.csv in data/ or the app root.",
    call. = FALSE
  )
}

validate_enrollment_columns <- function(enrollment_data) {
  missing_columns <- setdiff(required_enrollment_columns, names(enrollment_data))

  if (length(missing_columns) > 0) {
    stop(
      "The CSV is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_no_unitid_year_duplicates <- function(enrollment_data) {
  unit_year_duplicates <- suppressMessages(
    enrollment_data |>
      janitor::get_dupes(UNITID, Year)
  )

  if (nrow(unit_year_duplicates) > 0) {
    stop(
      "Duplicate rows were found for the same UNITID and Year. ",
      "This app intentionally stops instead of aggregating because each ",
      "university-year should represent one actual enrollment value.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

load_enrollment_data <- function(file_path = resolve_enrollment_file()) {
  cached <- load_prepared_cache(file_path)

  if (!is.null(cached)) {
    return(cached)
  }

  raw_enrollment <- read_csv_fast(file_path, required_enrollment_columns)

  validate_enrollment_columns(raw_enrollment)
  validate_no_unitid_year_duplicates(raw_enrollment)

  prepared_enrollment <- raw_enrollment |>
    transmute(
      Year = as.integer(Year),
      UNITID = as.character(UNITID),
      Institution = as.character(Institution),
      InstitutionLevel = as.character(InstitutionLevel),
      Control = as.character(Control),
      TotalEnrollment = as.numeric(TotalEnrollment)
    ) |>
    filter(
      !is.na(Year),
      Year >= 2010,
      Year <= 2024,
      !is.na(TotalEnrollment)
    )

  latest_university_labels <- prepared_enrollment |>
    arrange(UNITID, desc(Year), Institution) |>
    group_by(UNITID) |>
    slice(1) |>
    ungroup() |>
    transmute(
      UNITID,
      University = paste0(Institution, " (", UNITID, ")")
    )

  prepared_enrollment <- prepared_enrollment |>
    left_join(latest_university_labels, by = "UNITID")

  save_prepared_cache(file_path, prepared_enrollment)
}

filter_institution_universe <- function(enrollment_data, level_filter, control_filter) {
  filtered_data <- enrollment_data

  if (!is.null(level_filter) && level_filter != "All") {
    filtered_data <- filtered_data |>
      filter(InstitutionLevel == level_filter)
  }

  if (!is.null(control_filter) && control_filter != "All") {
    filtered_data <- filtered_data |>
      filter(Control == control_filter)
  }

  filtered_data
}

build_university_choices <- function(enrollment_data) {
  choices <- enrollment_data |>
    distinct(UNITID, University) |>
    arrange(University)

  stats::setNames(choices$UNITID, choices$University)
}

default_university_ids <- function(enrollment_data, max_universities = 5) {
  enrollment_data |>
    filter(InstitutionLevel == "4-year or above", Control == "Public") |>
    distinct(UNITID, University) |>
    arrange(University) |>
    slice_head(n = max_universities) |>
    pull(UNITID)
}

filter_selected_enrollment <- function(enrollment_data, selected_universities, year_range) {
  enrollment_data |>
    filter(
      UNITID %in% selected_universities,
      Year >= year_range[1],
      Year <= year_range[2]
    ) |>
    arrange(University, Year)
}

summarize_enrollment_kpis <- function(enrollment_data) {
  latest_year <- max(enrollment_data$Year, na.rm = TRUE)

  list(
    selected_universities = dplyr::n_distinct(enrollment_data$UNITID),
    latest_year = latest_year,
    latest_total = enrollment_data |>
      filter(Year == latest_year) |>
      summarise(total = sum(TotalEnrollment, na.rm = TRUE), .groups = "drop") |>
      pull(total),
    average_enrollment = mean(enrollment_data$TotalEnrollment, na.rm = TRUE)
  )
}

format_count <- function(x, accuracy = 1) {
  scales::comma(x, accuracy = accuracy)
}

prepare_enrollment_table <- function(enrollment_data) {
  enrollment_data |>
    select(
      UNITID,
      Institution,
      Year,
      InstitutionLevel,
      Control,
      TotalEnrollment
    ) |>
    arrange(Institution, Year)
}

# ============================================================
# 12-month enrollment helpers
# ============================================================

e12_required_demo_columns <- c(
  "UNITID", "Institution", "Year", "City", "State", "Sector", "Control",
  "InstitutionLevel", "StudentLevel", "Gender", "Ethnicity", "EnrollmentCount"
)

e12_required_attendance_columns <- c(
  "UNITID", "Institution", "Year", "City", "State", "Sector", "Control",
  "InstitutionLevel", "StudentLevel", "AttendanceStatus", "EnrollmentCount"
)

e12_required_fte_columns <- c(
  "UNITID", "Institution", "Year", "City", "State", "Sector", "Control",
  "InstitutionLevel", "UndergraduateHeadcount", "GraduateHeadcount",
  "TotalHeadcount", "UndergraduateFTE", "GraduateFTE", "TotalFTE",
  "FTEtoHeadcountRatio"
)

e12_level_group <- function(institution_level) {
  dplyr::case_when(
    institution_level == "2-year" ~ "2-year",
    institution_level == "4-year or above" ~ "4-year",
    TRUE ~ "Other"
  )
}

e12_validate_columns <- function(data, required_columns, file_label) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      file_label, " is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

e12_validate_no_duplicates <- function(data, key_columns, file_label) {
  duplicate_count <- if (anyDuplicated(data[key_columns]) > 0) {
    sum(duplicated(data[key_columns]))
  } else {
    0
  }

  if (duplicate_count > 0) {
    stop(
      file_label, " contains duplicate rows for key: ",
      paste(key_columns, collapse = " + "),
      ". Duplicate row count: ", duplicate_count,
      ". The app stops instead of silently aggregating these records.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

e12_add_university_label <- function(data) {
  latest_labels <- data |>
    arrange(UNITID, desc(Year), Institution) |>
    group_by(UNITID) |>
    slice(1) |>
    ungroup() |>
    transmute(
      UNITID,
      University = paste0(Institution, " (", UNITID, ")")
    )

  data |>
    left_join(latest_labels, by = "UNITID")
}

e12_load_demographics_data <- function(file_path = app_path("data", "all_university_12month_demographics_2010_2024.csv")) {
  cached <- load_prepared_cache(file_path)

  if (!is.null(cached)) {
    return(cached)
  }

  data <- read_csv_fast(file_path, e12_required_demo_columns)

  e12_validate_columns(data, e12_required_demo_columns, basename(file_path))
  e12_validate_no_duplicates(
    data,
    c("UNITID", "Year", "StudentLevel", "Gender", "Ethnicity"),
    basename(file_path)
  )

  prepared_data <- data |>
    transmute(
      UNITID = as.character(UNITID),
      Institution = as.character(Institution),
      Year = as.integer(Year),
      City = as.character(City),
      State = as.character(State),
      Sector = as.character(Sector),
      Control = as.character(Control),
      InstitutionLevel = as.character(InstitutionLevel),
      InstitutionLevelGroup = e12_level_group(InstitutionLevel),
      StudentLevel = as.character(StudentLevel),
      Gender = as.character(Gender),
      Ethnicity = as.character(Ethnicity),
      EnrollmentCount = as.numeric(EnrollmentCount)
    ) |>
    e12_add_university_label()

  save_prepared_cache(file_path, prepared_data)
}

e12_load_attendance_data <- function(file_path = app_path("data", "all_university_12month_ug_gr_attendance_2010_2024.csv")) {
  cached <- load_prepared_cache(file_path)

  if (!is.null(cached)) {
    return(cached)
  }

  data <- read_csv_fast(file_path, e12_required_attendance_columns)

  e12_validate_columns(data, e12_required_attendance_columns, basename(file_path))
  e12_validate_no_duplicates(
    data,
    c("UNITID", "Year", "StudentLevel", "AttendanceStatus"),
    basename(file_path)
  )

  prepared_data <- data |>
    transmute(
      UNITID = as.character(UNITID),
      Institution = as.character(Institution),
      Year = as.integer(Year),
      City = as.character(City),
      State = as.character(State),
      Sector = as.character(Sector),
      Control = as.character(Control),
      InstitutionLevel = as.character(InstitutionLevel),
      InstitutionLevelGroup = e12_level_group(InstitutionLevel),
      StudentLevel = as.character(StudentLevel),
      AttendanceStatus = as.character(AttendanceStatus),
      EnrollmentCount = as.numeric(EnrollmentCount)
    ) |>
    e12_add_university_label()

  save_prepared_cache(file_path, prepared_data)
}

e12_load_fte_data <- function(file_path = app_path("data", "all_university_fte_headcount_2010_2024.csv")) {
  cached <- load_prepared_cache(file_path)

  if (!is.null(cached)) {
    return(cached)
  }

  data <- read_csv_fast(file_path, e12_required_fte_columns)

  e12_validate_columns(data, e12_required_fte_columns, basename(file_path))
  e12_validate_no_duplicates(data, c("UNITID", "Year"), basename(file_path))

  prepared_data <- data |>
    transmute(
      UNITID = as.character(UNITID),
      Institution = as.character(Institution),
      Year = as.integer(Year),
      City = as.character(City),
      State = as.character(State),
      Sector = as.character(Sector),
      Control = as.character(Control),
      InstitutionLevel = as.character(InstitutionLevel),
      InstitutionLevelGroup = e12_level_group(InstitutionLevel),
      UndergraduateHeadcount = as.numeric(UndergraduateHeadcount),
      GraduateHeadcount = as.numeric(GraduateHeadcount),
      TotalHeadcount = as.numeric(TotalHeadcount),
      UndergraduateFTE = as.numeric(UndergraduateFTE),
      GraduateFTE = as.numeric(GraduateFTE),
      TotalFTE = as.numeric(TotalFTE),
      FTEtoHeadcountRatio = as.numeric(FTEtoHeadcountRatio)
    ) |>
    e12_add_university_label()

  save_prepared_cache(file_path, prepared_data)
}

e12_institution_metadata <- function(...) {
  bind_rows(
    lapply(
      list(...),
      function(data) {
        data |>
          distinct(UNITID, Institution, University, State, Control, InstitutionLevel, InstitutionLevelGroup)
      }
    )
  ) |>
    arrange(UNITID, University) |>
    distinct(UNITID, .keep_all = TRUE)
}

e12_filter_metadata <- function(metadata, state_filter, control_filter, level_filter) {
  filtered <- metadata

  if (!is.null(state_filter) && state_filter != "All") {
    filtered <- filtered |> filter(State == state_filter)
  }

  if (!is.null(control_filter) && control_filter != "All") {
    filtered <- filtered |> filter(Control == control_filter)
  }

  if (!is.null(level_filter) && level_filter != "All") {
    filtered <- filtered |> filter(InstitutionLevelGroup == level_filter)
  }

  filtered
}

e12_university_choices <- function(metadata) {
  choices <- metadata |>
    distinct(UNITID, University) |>
    arrange(University)

  stats::setNames(choices$UNITID, choices$University)
}

e12_default_universities <- function(metadata, max_universities = 3) {
  caldwell <- metadata |>
    filter(UNITID == "183910" | Institution == "Caldwell University") |>
    arrange(desc(Institution == "Caldwell University"))

  if (nrow(caldwell) > 0) {
    return(caldwell |> slice(1) |> pull(UNITID))
  }

  metadata |>
    arrange(University) |>
    slice_head(n = max_universities) |>
    pull(UNITID)
}

e12_apply_shared_filters <- function(data, institutions, year_range, state_filter, control_filter, level_filter) {
  filtered <- data |>
    filter(
      UNITID %in% institutions,
      Year >= year_range[1],
      Year <= year_range[2]
    )

  if (!is.null(state_filter) && state_filter != "All") {
    filtered <- filtered |> filter(State == state_filter)
  }

  if (!is.null(control_filter) && control_filter != "All") {
    filtered <- filtered |> filter(Control == control_filter)
  }

  if (!is.null(level_filter) && level_filter != "All") {
    filtered <- filtered |> filter(InstitutionLevelGroup == level_filter)
  }

  filtered
}

e12_latest_change_kpis <- function(data, value_column) {
  yearly <- data |>
    group_by(Year) |>
    summarise(
      Value = ifelse(
        all(is.na(.data[[value_column]])),
        NA_real_,
        sum(.data[[value_column]], na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    arrange(Year)

  first_value <- yearly$Value[1]
  latest_value <- yearly$Value[nrow(yearly)]
  change_value <- latest_value - first_value

  list(
    latest = latest_value,
    change = change_value,
    growth = ifelse(first_value > 0, change_value / first_value, NA_real_),
    average = mean(yearly$Value, na.rm = TRUE)
  )
}

e12_format_count <- function(x) {
  ifelse(is.na(x), "N/A", scales::comma(x, accuracy = 1))
}

e12_format_percent <- function(x) {
  ifelse(is.na(x), "N/A", scales::percent(x, accuracy = 0.1))
}

load_dashboard_data <- function() {
  enrollment_data <- load_enrollment_data()
  e12_demo_data <- e12_load_demographics_data()
  e12_attendance_data <- e12_load_attendance_data()
  e12_fte_data <- e12_load_fte_data()
  efficiency_data <- ie_load_data()

  list(
    enrollment_data = enrollment_data,
    e12_demo_data = e12_demo_data,
    e12_attendance_data = e12_attendance_data,
    e12_fte_data = e12_fte_data,
    efficiency_data = efficiency_data,
    e12_metadata = e12_institution_metadata(
      e12_demo_data,
      e12_attendance_data,
      e12_fte_data
    )
  )
}

# ============================================================
# Institutional Efficiency helpers
# ============================================================

ie_peer_institutions <- c(
  "Centenary University",
  "Drew University",
  "Emory & Henry University",
  "Felician University",
  "Georgian Court University",
  "Holy Family University",
  "McKendree University",
  "Mount Saint Mary College",
  "Neumann University",
  "Saint Peter's University",
  "University of Mount Saint Vincent",
  "University of Evansville",
  "Wagner College",
  "Westminster College"
)

ie_required_columns <- c(
  "Year", "UNITID", "Institution", "State", "Control", "TotalFTE",
  "TotalDegreesAwarded", "BachelorsDegrees", "MastersDegrees",
  "DoctoralDegrees", "InstructionExpenses", "TotalCoreExpenses",
  "TotalCoreRevenue", "TuitionAndFeeRevenue", "StudentServicesExpenses",
  "AcademicSupportExpenses", "InstitutionalSupportExpenses",
  "ResearchExpenses", "FullTimeInstructionalStaff",
  "PartTimeInstructionalStaff", "InstructionalStaffFTE", "FullTimeStaff",
  "PartTimeStaff", "TotalStaffHeadcount", "TotalStaffFTE",
  "FinanceReportingStandard", "FinanceParentChildStatus",
  "DegreesPer100FTE", "DegreesPerFullTimeInstructionalStaff",
  "DegreesPerInstructionalStaffFTE", "InstructionExpensesPerDegree",
  "CoreExpensesPerDegree", "InstructionExpensesPerFTE",
  "StudentServicesExpensesPerFTE", "AcademicSupportExpensesPerFTE",
  "InstitutionalSupportExpensesPerFTE", "CoreRevenuePerFTE",
  "TuitionRevenuePerFTE", "TuitionDependency",
  "StaffHeadcountPer100FTE", "StaffFTEPer100StudentFTE",
  "InstructionShareOfCoreExpenses", "StudentServicesShareOfCoreExpenses",
  "AcademicSupportShareOfCoreExpenses", "InstitutionalSupportShareOfCoreExpenses",
  "OtherCoreExpenses", "OtherCoreExpensesShare"
)

ie_numeric_columns <- c(
  "Year", "TotalFTE", "UndergraduateFTE", "GraduateFTE",
  "TwelveMonthHeadcount", "TotalDegreesAwarded", "TotalCompleters",
  "BachelorsDegrees", "MastersDegrees", "DoctoralDegrees",
  "InstructionExpenses", "TotalCoreExpenses", "TotalCoreRevenue",
  "TuitionAndFeeRevenue", "StudentServicesExpenses",
  "AcademicSupportExpenses", "InstitutionalSupportExpenses",
  "ResearchExpenses", "FullTimeInstructionalStaff",
  "PartTimeInstructionalStaff", "InstructionalStaffFTE", "FullTimeStaff",
  "PartTimeStaff", "TotalStaffHeadcount", "TotalStaffFTE",
  "DegreesPer100FTE", "DegreesPerFullTimeInstructionalStaff",
  "DegreesPerInstructionalStaffFTE", "InstructionExpensesPerDegree",
  "CoreExpensesPerDegree", "InstructionExpensesPerFTE",
  "StudentServicesExpensesPerFTE", "AcademicSupportExpensesPerFTE",
  "InstitutionalSupportExpensesPerFTE", "CoreRevenuePerFTE",
  "TuitionRevenuePerFTE", "TuitionDependency",
  "StaffHeadcountPer100FTE", "StaffFTEPer100StudentFTE",
  "InstructionShareOfCoreExpenses", "StudentServicesShareOfCoreExpenses",
  "AcademicSupportShareOfCoreExpenses", "InstitutionalSupportShareOfCoreExpenses",
  "OtherCoreExpenses", "OtherCoreExpensesShare"
)

ie_metric_metadata <- tibble::tribble(
  ~Metric, ~Label, ~Format, ~RawValue, ~Denominator,
  "DegreesPer100FTE", "Degrees per 100 FTE", "number", "TotalDegreesAwarded", "TotalFTE",
  "DegreesPerFullTimeInstructionalStaff", "Degrees per full-time instructional staff", "number", "TotalDegreesAwarded", "FullTimeInstructionalStaff",
  "DegreesPerInstructionalStaffFTE", "Degrees per instructional staff FTE", "number", "TotalDegreesAwarded", "InstructionalStaffFTE",
  "InstructionExpensesPerDegree", "Instructional expenses per degree", "currency", "InstructionExpenses", "TotalDegreesAwarded",
  "CoreExpensesPerDegree", "Core expenses per degree", "currency", "TotalCoreExpenses", "TotalDegreesAwarded",
  "InstructionExpensesPerFTE", "Instructional expenses per FTE", "currency", "InstructionExpenses", "TotalFTE",
  "StudentServicesExpensesPerFTE", "Student-services expenses per FTE", "currency", "StudentServicesExpenses", "TotalFTE",
  "AcademicSupportExpensesPerFTE", "Academic-support expenses per FTE", "currency", "AcademicSupportExpenses", "TotalFTE",
  "InstitutionalSupportExpensesPerFTE", "Institutional-support expenses per FTE", "currency", "InstitutionalSupportExpenses", "TotalFTE",
  "CoreRevenuePerFTE", "Core revenue per FTE", "currency", "TotalCoreRevenue", "TotalFTE",
  "TuitionRevenuePerFTE", "Tuition revenue per FTE", "currency", "TuitionAndFeeRevenue", "TotalFTE",
  "TuitionDependency", "Tuition dependency", "percent", "TuitionAndFeeRevenue", "TotalCoreRevenue",
  "StaffHeadcountPer100FTE", "Staff headcount per 100 FTE", "number", "TotalStaffHeadcount", "TotalFTE",
  "StaffFTEPer100StudentFTE", "Staff FTE per 100 student FTE", "number", "TotalStaffFTE", "TotalFTE",
  "InstructionShareOfCoreExpenses", "Instruction share of core expenses", "percent", "InstructionExpenses", "TotalCoreExpenses",
  "StudentServicesShareOfCoreExpenses", "Student-services share of core expenses", "percent", "StudentServicesExpenses", "TotalCoreExpenses",
  "AcademicSupportShareOfCoreExpenses", "Academic-support share of core expenses", "percent", "AcademicSupportExpenses", "TotalCoreExpenses",
  "InstitutionalSupportShareOfCoreExpenses", "Institutional-support share of core expenses", "percent", "InstitutionalSupportExpenses", "TotalCoreExpenses",
  "TotalDegreesAwarded", "Total degrees awarded", "count", "TotalDegreesAwarded", NA_character_,
  "BachelorsDegrees", "Bachelor's degrees", "count", "BachelorsDegrees", NA_character_,
  "MastersDegrees", "Master's degrees", "count", "MastersDegrees", NA_character_,
  "DoctoralDegrees", "Doctoral degrees", "count", "DoctoralDegrees", NA_character_,
  "TotalCoreRevenue", "Total core revenue", "currency", "TotalCoreRevenue", NA_character_,
  "TuitionAndFeeRevenue", "Tuition and fee revenue", "currency", "TuitionAndFeeRevenue", NA_character_,
  "InstructionExpenses", "Instruction expenses", "currency", "InstructionExpenses", NA_character_,
  "TotalCoreExpenses", "Core expenses", "currency", "TotalCoreExpenses", NA_character_,
  "FullTimeInstructionalStaff", "Full-time instructional staff", "count", "FullTimeInstructionalStaff", NA_character_,
  "PartTimeInstructionalStaff", "Part-time instructional staff", "count", "PartTimeInstructionalStaff", NA_character_,
  "TotalStaffHeadcount", "Total staff headcount", "count", "TotalStaffHeadcount", NA_character_
)

ie_resolve_file <- function() {
  data_path <- app_path("data", "institutional_efficiency_peer_analysis.csv")
  if (file.exists(data_path)) return(data_path)

  root_path <- app_path("institutional_efficiency_peer_analysis.csv")
  if (file.exists(root_path)) return(root_path)

  stop("Could not find institutional_efficiency_peer_analysis.csv in data/ or the app root.", call. = FALSE)
}

ie_load_data <- function(file_path = ie_resolve_file()) {
  efficiency_data <- read.csv(
    file_path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "N/A", "Not available")
  )

  missing_columns <- setdiff(ie_required_columns, names(efficiency_data))

  if ("Year" %in% names(efficiency_data)) {
    efficiency_data$Year <- suppressWarnings(as.integer(efficiency_data$Year))
  }
  if ("UNITID" %in% names(efficiency_data)) {
    efficiency_data$UNITID <- as.character(efficiency_data$UNITID)
  }

  for (column_name in intersect(ie_numeric_columns, names(efficiency_data))) {
    efficiency_data[[column_name]] <- suppressWarnings(as.numeric(efficiency_data[[column_name]]))
    efficiency_data[[column_name]][is.infinite(efficiency_data[[column_name]]) | is.nan(efficiency_data[[column_name]])] <- NA_real_
  }

  efficiency_data <- efficiency_data |>
    mutate(
      UNITID = if ("UNITID" %in% names(efficiency_data)) as.character(UNITID) else NA_character_,
      Institution = if ("Institution" %in% names(efficiency_data)) as.character(Institution) else NA_character_,
      State = if ("State" %in% names(efficiency_data)) as.character(State) else NA_character_,
      Control = if ("Control" %in% names(efficiency_data)) as.character(Control) else NA_character_,
      FinanceReportingStandard = if ("FinanceReportingStandard" %in% names(efficiency_data)) as.character(FinanceReportingStandard) else NA_character_,
      FinanceParentChildStatus = if ("FinanceParentChildStatus" %in% names(efficiency_data)) as.character(FinanceParentChildStatus) else NA_character_,
      IsCaldwell = Institution == "Caldwell University" | UNITID == "183910",
      FinanceParentWarning = dplyr::if_else(
        !is.na(FinanceParentChildStatus) &
          !stringr::str_detect(stringr::str_to_lower(FinanceParentChildStatus), "campus|institution-level"),
        "Financial data may represent a parent-level reporting entity and may not reflect this campus alone.",
        ""
      )
    )

  attr(efficiency_data, "missing_required_columns") <- missing_columns
  efficiency_data |>
    arrange(Institution, Year)
}

ie_data_missing_required <- function(data) {
  missing_columns <- attr(data, "missing_required_columns", exact = TRUE)
  if (is.null(missing_columns)) character(0) else missing_columns
}

ie_has_required_columns <- function(data, columns = ie_required_columns) {
  length(setdiff(columns, names(data))) == 0
}

ie_choices <- function(data) {
  choices <- data |>
    distinct(UNITID, Institution) |>
    arrange(desc(Institution == "Caldwell University"), Institution)

  stats::setNames(choices$UNITID, choices$Institution)
}

ie_peer_choices <- function(data) {
  data |>
    filter(!IsCaldwell) |>
    distinct(UNITID, Institution) |>
    arrange(Institution)
}

ie_metric_choices <- function(metrics) {
  metadata <- ie_metric_metadata |>
    filter(Metric %in% metrics)

  stats::setNames(metadata$Metric, metadata$Label)
}

ie_metric_label <- function(metric) {
  label <- ie_metric_metadata$Label[match(metric, ie_metric_metadata$Metric)]
  ifelse(is.na(label), metric, label)
}

ie_metric_format <- function(metric) {
  format <- ie_metric_metadata$Format[match(metric, ie_metric_metadata$Metric)]
  ifelse(is.na(format), "number", format)
}

ie_raw_column <- function(metric) {
  ie_metric_metadata$RawValue[match(metric, ie_metric_metadata$Metric)]
}

ie_denominator_column <- function(metric) {
  ie_metric_metadata$Denominator[match(metric, ie_metric_metadata$Metric)]
}

ie_format_value <- function(x, metric = NULL, accuracy = NULL) {
  format <- if (is.null(metric)) "number" else ie_metric_format(metric)
  values <- rep("Not available", length(x))
  valid <- !(is.na(x) | is.infinite(x) | is.nan(x))

  if (!any(valid)) {
    return(values)
  }

  if (format == "currency") {
    values[valid] <- scales::dollar(x[valid], accuracy = accuracy %||% 1)
  } else if (format == "percent") {
    values[valid] <- paste0(scales::number(x[valid], accuracy = accuracy %||% 0.1), "%")
  } else if (format == "count") {
    values[valid] <- scales::comma(x[valid], accuracy = accuracy %||% 1)
  } else {
    values[valid] <- scales::number(x[valid], accuracy = accuracy %||% 0.1)
  }

  values
}

ie_format_change <- function(x, metric) {
  if (length(x) == 0 || is.na(x) || is.infinite(x) || is.nan(x)) {
    return("Not available")
  }
  prefix <- ifelse(x > 0, "+", "")
  paste0(prefix, ie_format_value(x, metric))
}

ie_valid_metric_data <- function(data, metric) {
  validate(need(metric %in% names(data), paste(ie_metric_label(metric), "is not available in the CSV.")))
  data |>
    mutate(MetricValue = .data[[metric]]) |>
    filter(!is.na(MetricValue), !is.infinite(MetricValue), !is.nan(MetricValue))
}

ie_selected_data <- function(data, selected_ids, year_range) {
  data |>
    filter(
      UNITID %in% selected_ids,
      Year >= year_range[1],
      Year <= year_range[2]
    ) |>
    arrange(Institution, Year)
}

ie_add_line_runs <- function(data) {
  data |>
    arrange(UNITID, Year) |>
    group_by(UNITID) |>
    mutate(LineRun = cumsum(is.na(lag(Year)) | Year - lag(Year) > 1)) |>
    ungroup()
}

ie_peer_stats <- function(data, primary_id, peer_ids, metric) {
  ie_valid_metric_data(data, metric) |>
    filter(UNITID %in% peer_ids, UNITID != primary_id) |>
    group_by(Year) |>
    summarise(
      PeerAverage = mean(MetricValue, na.rm = TRUE),
      PeerMedian = median(MetricValue, na.rm = TRUE),
      PeerMinimum = min(MetricValue, na.rm = TRUE),
      PeerMaximum = max(MetricValue, na.rm = TRUE),
      ValidPeers = n_distinct(UNITID),
      .groups = "drop"
    )
}

ie_latest_common_year <- function(data, ids, metric, min_coverage = 2) {
  valid <- ie_valid_metric_data(data, metric) |>
    filter(UNITID %in% ids) |>
    group_by(Year) |>
    summarise(Institutions = n_distinct(UNITID), .groups = "drop") |>
    filter(Institutions >= min(min_coverage, length(ids)))

  if (nrow(valid) == 0) return(max(data$Year, na.rm = TRUE))
  max(valid$Year, na.rm = TRUE)
}

ie_ranking_data <- function(data, ids, primary_id, metric, ranking_method, common_year = NULL) {
  valid <- ie_valid_metric_data(data, metric) |>
    filter(UNITID %in% ids)

  if (identical(ranking_method, "Latest available year for each institution")) {
    valid <- valid |>
      arrange(UNITID, desc(Year)) |>
      group_by(UNITID) |>
      slice(1) |>
      ungroup()
  } else {
    if (is.null(common_year) || is.na(common_year)) {
      common_year <- ie_latest_common_year(data, ids, metric)
    }
    valid <- valid |> filter(Year == common_year)
  }

  valid |>
    arrange(desc(MetricValue), Institution) |>
    mutate(
      Rank = row_number(),
      Highlight = UNITID == primary_id | Institution == "Caldwell University"
    )
}

ie_latest_primary_row <- function(data, primary_id, metric) {
  ie_valid_metric_data(data, metric) |>
    filter(UNITID == primary_id) |>
    arrange(desc(Year)) |>
    slice(1)
}

ie_previous_primary_row <- function(data, primary_id, metric, latest_year) {
  ie_valid_metric_data(data, metric) |>
    filter(UNITID == primary_id, Year < latest_year) |>
    arrange(desc(Year)) |>
    slice(1)
}

ie_kpi_summary <- function(data, primary_id, peer_ids, metric) {
  latest <- ie_latest_primary_row(data, primary_id, metric)
  if (nrow(latest) == 0) {
    return(tibble::tibble(
      Value = NA_real_, Year = NA_integer_, PreviousChange = NA_real_,
      PeerMedian = NA_real_, Difference = NA_real_, ValidPeers = 0L
    ))
  }

  previous <- ie_previous_primary_row(data, primary_id, metric, latest$Year[1])
  peer <- ie_peer_stats(data, primary_id, peer_ids, metric) |>
    filter(Year == latest$Year[1]) |>
    slice(1)

  tibble::tibble(
    Value = latest$MetricValue[1],
    Year = latest$Year[1],
    PreviousChange = if (nrow(previous) == 0) NA_real_ else latest$MetricValue[1] - previous$MetricValue[1],
    PeerMedian = if (nrow(peer) == 0) NA_real_ else peer$PeerMedian[1],
    Difference = if (nrow(peer) == 0) NA_real_ else latest$MetricValue[1] - peer$PeerMedian[1],
    ValidPeers = if (nrow(peer) == 0) 0L else peer$ValidPeers[1]
  )
}

ie_kpi_text <- function(data, primary_id, peer_ids, metric) {
  row <- ie_kpi_summary(data, primary_id, peer_ids, metric)
  paste0(
    ie_format_value(row$Value[1], metric),
    " (", ifelse(is.na(row$Year[1]), "year N/A", row$Year[1]), ")",
    "\nYoY: ", ie_format_change(row$PreviousChange[1], metric),
    "\nPeer median: ", ie_format_value(row$PeerMedian[1], metric),
    "\nDiff: ", ie_format_change(row$Difference[1], metric),
    "\nPeers: ", row$ValidPeers[1]
  )
}

ie_finance_warning_text <- function(status) {
  ifelse(
    is.na(status) | stringr::str_detect(stringr::str_to_lower(status), "campus|institution-level"),
    "",
    "Financial data may represent a parent-level reporting entity and may not reflect this campus alone."
  )
}

ie_plot_output <- function(output_id, height = "430px") {
  if (requireNamespace("plotly", quietly = TRUE)) {
    plotly::plotlyOutput(output_id, height = height)
  } else {
    plotOutput(output_id, height = height)
  }
}

ie_render_plot <- function(output, output_id, plot_expr) {
  if (requireNamespace("plotly", quietly = TRUE)) {
    output[[output_id]] <- plotly::renderPlotly({
      plotly::ggplotly(plot_expr(), tooltip = "text") |>
        plotly::layout(hovermode = "closest")
    })
  } else {
    output[[output_id]] <- renderPlot(suppressWarnings(plot_expr()))
  }
}

ie_base_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#e6e6e6"),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

ie_metric_line_plot <- function(data, peer_stats, primary_id, metric, title = NULL) {
  plot_data <- ie_valid_metric_data(data, metric) |>
    ie_add_line_runs() |>
    mutate(
      Highlight = UNITID == primary_id | Institution == "Caldwell University",
      text = paste0(
        "Institution: ", Institution,
        "<br>Year: ", Year,
        "<br>", ie_metric_label(metric), ": ", vapply(MetricValue, ie_format_value, character(1), metric = metric),
        "<br>Total FTE: ", ie_format_value(TotalFTE, "TotalDegreesAwarded"),
        "<br>Total degrees: ", ie_format_value(TotalDegreesAwarded, "TotalDegreesAwarded"),
        "<br>Finance reporting standard: ", FinanceReportingStandard,
        ifelse(FinanceParentWarning == "", "", paste0("<br>", FinanceParentWarning))
      )
    )

  validate(need(nrow(plot_data) > 0, "No valid observations are available for this metric."))

  ggplot(plot_data, aes(x = Year, y = MetricValue, color = Institution, group = interaction(UNITID, LineRun), text = text)) +
    geom_line(aes(linewidth = Highlight, alpha = Highlight), na.rm = TRUE) +
    geom_point(aes(size = Highlight), na.rm = TRUE) +
    geom_line(data = peer_stats, aes(x = Year, y = PeerMedian, group = 1, text = paste0("Peer median<br>Year: ", Year, "<br>", ie_metric_label(metric), ": ", vapply(PeerMedian, ie_format_value, character(1), metric = metric), "<br>Valid peers: ", ValidPeers)), inherit.aes = FALSE, color = "#252525", linetype = "dashed", linewidth = 1) +
    scale_linewidth_manual(values = c(`TRUE` = 1.35, `FALSE` = 0.75), guide = "none") +
    scale_size_manual(values = c(`TRUE` = 3.2, `FALSE` = 2), guide = "none") +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.55), guide = "none") +
    scale_y_continuous(labels = function(x) vapply(x, ie_format_value, character(1), metric = metric)) +
    labs(title = title %||% paste(ie_metric_label(metric), "Trend"), x = NULL, y = ie_metric_label(metric), color = "Institution") +
    ie_base_theme()
}

ie_ranking_plot <- function(data, metric, caption = NULL) {
  plot_data <- data |>
    mutate(
      Label = paste0("#", Rank, "  ", vapply(MetricValue, ie_format_value, character(1), metric = metric), " (", Year, ")"),
      text = paste0(
        "Rank: ", Rank,
        "<br>Institution: ", Institution,
        "<br>Reporting year: ", Year,
        "<br>", ie_metric_label(metric), ": ", vapply(MetricValue, ie_format_value, character(1), metric = metric),
        "<br>Finance reporting standard: ", FinanceReportingStandard,
        ifelse(FinanceParentWarning == "", "", paste0("<br>", FinanceParentWarning))
      )
    )

  validate(need(nrow(plot_data) > 0, "No institutions have valid values for this ranking."))

  ggplot(plot_data, aes(x = reorder(Institution, MetricValue), y = MetricValue, fill = Highlight, text = text)) +
    geom_col(width = 0.72, na.rm = TRUE) +
    geom_text(aes(label = Label), hjust = -0.04, size = 3.3) +
    coord_flip() +
    scale_fill_manual(values = c(`TRUE` = "#b31b1b", `FALSE` = "#6b7280"), guide = "none") +
    scale_y_continuous(labels = function(x) vapply(x, ie_format_value, character(1), metric = metric), expand = expansion(mult = c(0, 0.18))) +
    labs(x = NULL, y = ie_metric_label(metric), caption = caption) +
    ie_base_theme()
}

ie_bar_compare_plot <- function(primary_row, peer_row, metric) {
  validate(need(nrow(primary_row) > 0, "No primary institution value is available."))
  validate(need(nrow(peer_row) > 0 && !is.na(peer_row$PeerMedian[1]), "No peer median is available for this year."))

  plot_data <- tibble::tibble(
    Group = c(primary_row$Institution[1], "Selected peer median"),
    Value = c(primary_row$MetricValue[1], peer_row$PeerMedian[1]),
    text = c(
      paste0(primary_row$Institution[1], "<br>Year: ", primary_row$Year[1], "<br>", ie_metric_label(metric), ": ", ie_format_value(primary_row$MetricValue[1], metric)),
      paste0("Selected peer median<br>Year: ", peer_row$Year[1], "<br>", ie_metric_label(metric), ": ", ie_format_value(peer_row$PeerMedian[1], metric), "<br>Valid peers: ", peer_row$ValidPeers[1])
    )
  )

  ggplot(plot_data, aes(x = reorder(Group, Value), y = Value, fill = Group == primary_row$Institution[1], text = text)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = vapply(Value, ie_format_value, character(1), metric = metric)), hjust = -0.08, size = 3.7) +
    coord_flip() +
    scale_fill_manual(values = c(`TRUE` = "#b31b1b", `FALSE` = "#6b7280"), guide = "none") +
    scale_y_continuous(labels = function(x) vapply(x, ie_format_value, character(1), metric = metric), expand = expansion(mult = c(0, 0.2))) +
    labs(x = NULL, y = ie_metric_label(metric)) +
    ie_base_theme()
}

ie_scatter_plot <- function(data, x_metric, y_metric, primary_id, selected_year, add_trend = FALSE) {
  plot_data <- data |>
    filter(Year == selected_year) |>
    mutate(
      XValue = .data[[x_metric]],
      YValue = .data[[y_metric]],
      Highlight = UNITID == primary_id | Institution == "Caldwell University",
      text = paste0(
        "Institution: ", Institution,
        "<br>Year: ", Year,
        "<br>", ie_metric_label(x_metric), ": ", vapply(XValue, ie_format_value, character(1), metric = x_metric),
        "<br>", ie_metric_label(y_metric), ": ", vapply(YValue, ie_format_value, character(1), metric = y_metric),
        "<br>Total FTE: ", ie_format_value(TotalFTE, "TotalDegreesAwarded")
      )
    ) |>
    filter(!is.na(XValue), !is.na(YValue))

  validate(need(nrow(plot_data) > 1, "Not enough valid institutions are available for this scatterplot."))

  p <- ggplot(plot_data, aes(x = XValue, y = YValue, size = TotalFTE, color = Highlight, text = text)) +
    geom_point(alpha = 0.85, na.rm = TRUE) +
    scale_color_manual(values = c(`TRUE` = "#b31b1b", `FALSE` = "#6b7280"), guide = "none") +
    scale_size_continuous(labels = scales::comma) +
    scale_x_continuous(labels = function(x) vapply(x, ie_format_value, character(1), metric = x_metric)) +
    scale_y_continuous(labels = function(x) vapply(x, ie_format_value, character(1), metric = y_metric)) +
    labs(x = ie_metric_label(x_metric), y = ie_metric_label(y_metric), size = "Total FTE") +
    ie_base_theme()

  if (isTRUE(add_trend)) {
    p <- p + geom_smooth(method = "lm", se = FALSE, color = "#252525", linewidth = 0.7, linetype = "dashed")
  }

  p
}

# ============================================================
# Graduation Rate Analytics helpers
# ============================================================
# ============================================================
# Graduation Rate Analytics module
# ============================================================

gr_peer_institutions <- c(
  "Centenary University",
  "Drew University",
  "Emory & Henry University",
  "Felician University",
  "Georgian Court University",
  "Holy Family University",
  "McKendree University",
  "Mount Saint Mary College",
  "Neumann University",
  "Saint Peter's University",
  "University of Mount Saint Vincent",
  "University of Evansville",
  "Wagner College",
  "Westminster College"
)

gr_nj_institutions <- c(
  "Caldwell University",
  "Centenary University",
  "Drew University",
  "Felician University",
  "Georgian Court University",
  "Saint Peter's University"
)

gr_rate_choices <- c(
  "100% graduation rate" = "rate_100",
  "150% graduation rate" = "rate_150",
  "200% graduation rate" = "rate_200",
  "Show all three" = "all"
)

gr_rate_metadata <- tibble::tribble(
  ~RateKey, ~RateType, ~RateColumn, ~CompleterColumn, ~RateLabel,
  "rate_100", "100% graduation rate", "GraduationRate100", "Completers100", "100%",
  "rate_150", "150% graduation rate", "GraduationRate150", "Completers150", "150%",
  "rate_200", "200% graduation rate", "GraduationRate200", "Completers200", "200%"
)

gr_numeric_columns <- c(
  "CohortYear", "GRReportingYear", "GR200ReportingYear", "InitialCohort",
  "Exclusions", "AdjustedCohort", "Completers100", "Completers150",
  "Completers200", "TransferOut", "GraduationRate100", "GraduationRate150",
  "GraduationRate200", "TransferOutRate", "DisplayOrder"
)

gr_required_columns <- c(
  "UNITID", "Institution", "State", "CohortYear", "GRReportingYear",
  "GR200ReportingYear", "CohortType", "AwardLevel", "AdjustedCohort",
  "Completers100", "Completers150", "Completers200", "GraduationRate100",
  "GraduationRate150", "GraduationRate200", "PeerGroup", "DisplayOrder",
  "IsCaldwell"
)

gr_plot_output <- function(output_id, height = "430px") {
  if (requireNamespace("plotly", quietly = TRUE)) {
    plotly::plotlyOutput(output_id, height = height)
  } else {
    plotOutput(output_id, height = height)
  }
}

gr_rate_column <- function(rate_key) {
  gr_rate_metadata$RateColumn[match(rate_key, gr_rate_metadata$RateKey)]
}

gr_completer_column <- function(rate_key) {
  gr_rate_metadata$CompleterColumn[match(rate_key, gr_rate_metadata$RateKey)]
}

gr_rate_label <- function(rate_key) {
  gr_rate_metadata$RateType[match(rate_key, gr_rate_metadata$RateKey)]
}

gr_format_percent <- function(x) {
  ifelse(is.na(x), "N/A", paste0(scales::number(x, accuracy = 0.1), "%"))
}

gr_format_pp <- function(x) {
  ifelse(is.na(x), "N/A", paste0(ifelse(x > 0, "+", ""), scales::number(x, accuracy = 0.1), " pp"))
}

gr_format_count <- function(x) {
  ifelse(is.na(x), "N/A", scales::comma(x, accuracy = 1))
}

gr_safe_rate <- function(completers, adjusted_cohort) {
  dplyr::if_else(
    is.na(adjusted_cohort) | adjusted_cohort == 0 | is.na(completers),
    NA_real_,
    completers / adjusted_cohort * 100
  )
}

gr_validate_columns <- function(data, required_columns, file_label) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      file_label, " is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

gr_resolve_rates_file <- function() {
  data_path <- app_path("data", "caldwell_peer_graduation_rates.csv")

  if (file.exists(data_path)) {
    return(data_path)
  }

  root_path <- app_path("caldwell_peer_graduation_rates.csv")

  if (file.exists(root_path)) {
    return(root_path)
  }

  stop("Could not find caldwell_peer_graduation_rates.csv in data/ or the app root.", call. = FALSE)
}

gr_load_rate_data <- function(file_path = gr_resolve_rates_file()) {
  data <- read_csv_fast(file_path)
  gr_validate_columns(data, gr_required_columns, basename(file_path))

  for (column_name in intersect(gr_numeric_columns, names(data))) {
    data[[column_name]] <- suppressWarnings(as.numeric(data[[column_name]]))
  }

  data |>
    mutate(
      UNITID = as.character(UNITID),
      Institution = as.character(Institution),
      City = as.character(.data$City),
      State = as.character(State),
      Sector = as.character(.data$Sector),
      Control = as.character(.data$Control),
      CohortType = as.character(CohortType),
      AwardLevel = as.character(AwardLevel),
      PeerGroup = as.character(PeerGroup),
      PeerGroupName = as.character(.data$PeerGroupName),
      IsCaldwell = tolower(as.character(IsCaldwell)) %in% c("true", "t", "1", "yes"),
      GraduationRate100 = dplyr::if_else(is.na(GraduationRate100), gr_safe_rate(Completers100, AdjustedCohort), GraduationRate100),
      GraduationRate150 = dplyr::if_else(is.na(GraduationRate150), gr_safe_rate(Completers150, AdjustedCohort), GraduationRate150),
      GraduationRate200 = dplyr::if_else(is.na(GraduationRate200), gr_safe_rate(Completers200, AdjustedCohort), GraduationRate200),
      TransferOutRate = dplyr::if_else(is.na(TransferOutRate), gr_safe_rate(TransferOut, AdjustedCohort), TransferOutRate),
      DataAvailabilityStatus = dplyr::case_when(
        is.na(AdjustedCohort) ~ "Missing adjusted cohort",
        AdjustedCohort == 0 ~ "Adjusted cohort is zero",
        is.na(GraduationRate100) & is.na(GraduationRate150) & is.na(GraduationRate200) ~ "Missing graduation rates",
        TRUE ~ "Available"
      )
    ) |>
    filter(Institution == "Caldwell University" | Institution %in% gr_peer_institutions) |>
    arrange(DisplayOrder, Institution, CohortYear)
}

gr_load_demographic_data <- function(file_path = app_path("data", "caldwell_peer_graduation_demographics.csv")) {
  if (!file.exists(file_path)) {
    return(NULL)
  }

  data <- read_csv_fast(file_path)
  required <- c("UNITID", "Institution", "CohortYear", "AdjustedCohort", "Completers100", "Completers150", "Completers200")
  gr_validate_columns(data, required, basename(file_path))

  for (column_name in intersect(gr_numeric_columns, names(data))) {
    data[[column_name]] <- suppressWarnings(as.numeric(data[[column_name]]))
  }

  data |>
    mutate(
      UNITID = as.character(UNITID),
      Institution = as.character(Institution),
      Gender = if ("Gender" %in% names(data)) as.character(Gender) else "Not reported",
      Ethnicity = if ("Ethnicity" %in% names(data)) as.character(Ethnicity) else if ("RaceEthnicity" %in% names(data)) as.character(RaceEthnicity) else "Not reported",
      StudentGroup = if ("StudentGroup" %in% names(data)) as.character(StudentGroup) else "Demographic subgroup",
      GraduationRate100 = if ("GraduationRate100" %in% names(data)) as.numeric(GraduationRate100) else gr_safe_rate(Completers100, AdjustedCohort),
      GraduationRate150 = if ("GraduationRate150" %in% names(data)) as.numeric(GraduationRate150) else gr_safe_rate(Completers150, AdjustedCohort),
      GraduationRate200 = if ("GraduationRate200" %in% names(data)) as.numeric(GraduationRate200) else gr_safe_rate(Completers200, AdjustedCohort)
    ) |>
    filter(
      Institution == "Caldwell University" | Institution %in% gr_peer_institutions,
      !tolower(StudentGroup) %in% c("overall", "overall cohort", "total")
    )
}

gr_add_selected_rate <- function(data, rate_key) {
  rate_column <- gr_rate_column(rate_key)
  completer_column <- gr_completer_column(rate_key)

  data |>
    mutate(
      SelectedGraduationRate = .data[[rate_column]],
      SelectedCompleters = .data[[completer_column]],
      SelectedRateType = gr_rate_label(rate_key)
    )
}

gr_valid_rate_rows <- function(data) {
  data |>
    filter(
      !is.na(AdjustedCohort),
      AdjustedCohort > 0,
      !is.na(SelectedCompleters),
      !is.na(SelectedGraduationRate)
    )
}

gr_weighted_average <- function(data) {
  valid <- gr_valid_rate_rows(data)

  if (nrow(valid) == 0 || sum(valid$AdjustedCohort, na.rm = TRUE) <= 0) {
    return(tibble::tibble(
      SelectedGraduationRate = NA_real_,
      AdjustedCohort = NA_real_,
      SelectedCompleters = NA_real_,
      InstitutionsIncluded = 0L
    ))
  }

  valid |>
    summarise(
      SelectedGraduationRate = sum(SelectedCompleters, na.rm = TRUE) / sum(AdjustedCohort, na.rm = TRUE) * 100,
      AdjustedCohort = sum(AdjustedCohort, na.rm = TRUE),
      SelectedCompleters = sum(SelectedCompleters, na.rm = TRUE),
      InstitutionsIncluded = n_distinct(UNITID),
      .groups = "drop"
    )
}

gr_peer_median <- function(data) {
  valid <- gr_valid_rate_rows(data)

  tibble::tibble(
    SelectedGraduationRate = if (nrow(valid) == 0) NA_real_ else median(valid$SelectedGraduationRate, na.rm = TRUE),
    InstitutionsIncluded = n_distinct(valid$UNITID)
  )
}

gr_full_long_rates <- function(data) {
  data |>
    select(
      UNITID, Institution, State, CohortYear, GRReportingYear, GR200ReportingYear,
      AdjustedCohort, Completers100, Completers150, Completers200,
      GraduationRate100, GraduationRate150, GraduationRate200,
      DisplayOrder, IsCaldwell, DataAvailabilityStatus
    ) |>
    tidyr::pivot_longer(
      cols = c(GraduationRate100, GraduationRate150, GraduationRate200),
      names_to = "RateColumn",
      values_to = "SelectedGraduationRate"
    ) |>
    mutate(
      SelectedRateType = dplyr::recode(
        RateColumn,
        GraduationRate100 = "100% graduation rate",
        GraduationRate150 = "150% graduation rate",
        GraduationRate200 = "200% graduation rate"
      ),
      SelectedCompleters = dplyr::case_when(
        RateColumn == "GraduationRate100" ~ Completers100,
        RateColumn == "GraduationRate150" ~ Completers150,
        RateColumn == "GraduationRate200" ~ Completers200,
        TRUE ~ NA_real_
      )
    )
}


# ============================================================
# Retention Analysis helpers
# ============================================================
# ============================================================
# Retention Analysis module
# ============================================================

ra_peer_institutions <- c(
  "Centenary University",
  "Drew University",
  "Emory & Henry University",
  "Felician University",
  "Georgian Court University",
  "Holy Family University",
  "McKendree University",
  "Mount Saint Mary College",
  "Neumann University",
  "Saint Peter's University",
  "University of Mount Saint Vincent",
  "University of Evansville",
  "Wagner College",
  "Westminster College"
)

ra_required_columns <- c(
  "Year", "UNITID", "Institution", "State", "FullTimeCohort",
  "FullTimeRetained", "FullTimeRetentionRate", "PartTimeCohort",
  "PartTimeRetained", "PartTimeRetentionRate", "FullTimeYoYChange",
  "PartTimeYoYChange", "RetentionGap"
)

ra_rate_choices <- c("Full-time", "Part-time", "Both")
ra_rank_status_choices <- c("Full-time", "Part-time")
ra_ranking_methods <- c(
  "Latest common year",
  "Latest available year for each institution"
)

ra_resolve_file <- function() {
  data_path <- app_path("data", "retention_analysis_peer_institutions.csv")

  if (file.exists(data_path)) {
    return(data_path)
  }

  root_path <- app_path("retention_analysis_peer_institutions.csv")

  if (file.exists(root_path)) {
    return(root_path)
  }

  stop("Could not find retention_analysis_peer_institutions.csv in data/ or the app root.", call. = FALSE)
}

ra_validate_columns <- function(data, required_columns, file_label) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      file_label, " is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

ra_load_data <- function(file_path = ra_resolve_file()) {
  retention_data <- read.csv(
    file_path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "N/A", "Not available")
  )

  ra_validate_columns(retention_data, ra_required_columns, basename(file_path))

  retention_data$Year <- as.integer(retention_data$Year)

  retention_data$FullTimeRetentionRate <-
    as.numeric(retention_data$FullTimeRetentionRate)

  retention_data$PartTimeRetentionRate <-
    as.numeric(retention_data$PartTimeRetentionRate)

  count_columns <- c("FullTimeCohort", "FullTimeRetained", "PartTimeCohort", "PartTimeRetained")
  change_columns <- c("FullTimeYoYChange", "PartTimeYoYChange", "RetentionGap")
  for (column_name in intersect(c(count_columns, change_columns), names(retention_data))) {
    retention_data[[column_name]] <- suppressWarnings(as.numeric(retention_data[[column_name]]))
  }

  invalid_rates <- retention_data |>
    filter(
      (!is.na(FullTimeRetentionRate) & (FullTimeRetentionRate < 0 | FullTimeRetentionRate > 100)) |
        (!is.na(PartTimeRetentionRate) & (PartTimeRetentionRate < 0 | PartTimeRetentionRate > 100))
    )

  if (nrow(invalid_rates) > 0) {
    stop("Retention rates must be between 0 and 100.", call. = FALSE)
  }

  retention_data |>
    mutate(
      UNITID = as.character(UNITID),
      Institution = as.character(Institution),
      State = as.character(State),
      IsCaldwell = Institution == "Caldwell University",
      RetentionGap = dplyr::if_else(
        !is.na(FullTimeRetentionRate) & !is.na(PartTimeRetentionRate),
        FullTimeRetentionRate - PartTimeRetentionRate,
        NA_real_
      )
    ) |>
    filter(Institution == "Caldwell University" | Institution %in% ra_peer_institutions) |>
    arrange(Institution, Year)
}

ra_format_percent <- function(x) {
  ifelse(is.na(x), "Not available", paste0(scales::number(x, accuracy = 0.1), "%"))
}

ra_format_pp <- function(x, short = FALSE) {
  suffix <- if (short) " pp" else " percentage points"
  ifelse(is.na(x), "Not available", paste0(ifelse(x > 0, "+", ""), scales::number(x, accuracy = 0.1), suffix))
}

ra_format_count <- function(x) {
  ifelse(is.na(x), "Not available", scales::comma(x, accuracy = 1))
}

ra_status_data <- function(data, attendance_status) {
  long_data <- data |>
    tidyr::pivot_longer(
      cols = c(FullTimeRetentionRate, PartTimeRetentionRate),
      names_to = "RateColumn",
      values_to = "RetentionRate"
    ) |>
    mutate(
      AttendanceStatus = dplyr::recode(
        RateColumn,
        FullTimeRetentionRate = "Full-time",
        PartTimeRetentionRate = "Part-time"
      ),
      CohortSize = dplyr::if_else(AttendanceStatus == "Full-time", FullTimeCohort, PartTimeCohort),
      StudentsRetained = dplyr::if_else(AttendanceStatus == "Full-time", FullTimeRetained, PartTimeRetained),
      YearOverYearChange = dplyr::if_else(AttendanceStatus == "Full-time", FullTimeYoYChange, PartTimeYoYChange)
    )

  if (!identical(attendance_status, "Both")) {
    long_data <- long_data |> filter(AttendanceStatus == attendance_status)
  }

  long_data |>
    arrange(Institution, AttendanceStatus, Year) |>
    group_by(UNITID, Institution, AttendanceStatus) |>
    mutate(
      PreviousYear = dplyr::lag(Year),
      CalculatedYoYChange = dplyr::if_else(
        Year - PreviousYear == 1,
        RetentionRate - dplyr::lag(RetentionRate),
        NA_real_
      )
    ) |>
    ungroup()
}

ra_plot_output <- function(output_id, height = "430px") {
  if (requireNamespace("plotly", quietly = TRUE)) {
    plotly::plotlyOutput(output_id, height = height)
  } else {
    plotOutput(output_id, height = height)
  }
}

ra_render_plot <- function(output, output_id, plot_expr) {
  if (requireNamespace("plotly", quietly = TRUE)) {
    output[[output_id]] <- plotly::renderPlotly({
      plotly::ggplotly(plot_expr(), tooltip = "text") |>
        plotly::layout(hovermode = "closest")
    })
  } else {
    output[[output_id]] <- renderPlot(suppressWarnings(plot_expr()))
  }
}
