library(IPEDSR)
library(DBI)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  normalizePath(getwd(), mustWork = TRUE)
}

project_dir <- get_script_dir()
db_path <- file.path(project_dir, "ipeds_data", "ipeds.duckdb")
if (!file.exists(db_path)) {
  stop("Could not find IPEDS DuckDB file at: ", db_path)
}

idbc <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)




#list all tables
DBI :: dbListTables(idbc)
# DBI::dbListFields(idbc, "ADM2023")

DBI::dbGetQuery(idbc, "SELECT * FROM ADM2023 LIMIT 5")

tbl(idbc, 'chararcteristics') %>%
  summarise(Numberofuniversities = n_distinct(UNITID)) %>%
  collect()



# grep("ef", tables, value = TRUE)  #get just the tables that have the regex



# caldwell_unitid <- 182634
# years <- 2020:2024

# get_completers_year <- function(year) {
  
#   table_name <- paste0("C", year, "_A")
  
#   tbl(idbc, table_name) %>%
#     filter(
#       UNITID == caldwell_unitid,
#       AWLEVEL == 5
#     ) %>%
#     group_by(CIPCODE) %>%
#     summarize(
#       Graduates = sum(CTOTALT, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     mutate(Year = year) %>%
#     collect()
# }

# top_majors_all_years <- map_dfr(years, get_completers_year)

# cip_lookup <- IPEDSR::get_cipcodes(idbc) %>%
#   select(CIPCODE, Subject)

# top_majors_named <- top_majors_all_years %>%
#   left_join(cip_lookup, by = "CIPCODE") %>%
#   filter(!is.na(Subject), Graduates > 0) %>%
#   select(
#     Year,
#     CIPCODE,
#     Major = Subject,
#     Graduates
#   ) %>%
#   arrange(Year, desc(Graduates))

# write_csv(
#   top_majors_named,
#   "caldwell_top_majors_by_year_v2.csv"
# )

# print(top_majors_named)


# enrollment <- IPEDSR::ipeds_get_enrollment(idbc, unitids)

# total_enrollment <- enrollment %>%
#   filter(
#     StudentType == "All students total",
#     Year >= 2020,
#     Year <= 2024
#   ) %>%
#   select(UNITID, Year, Total)

# write.csv(
#   total_enrollment,
#   file.path(project_dir, "total_enrollment_2020_2024.csv"),
#   row.names = FALSE
# )


# years <- 2020:2024
# awlevel <- 5L

# get_completion_table <- function(idbc, year) {
#   table_name <- sprintf("c%d_a", year)
#   tables <- DBI::dbListTables(idbc)
#   matches <- tables[tolower(tables) == table_name]

#   if (length(matches) == 0) {
#     stop("Could not find completions table for ", year, ": ", table_name)
#   }

#   matches[[1]]
# }

# cipcodes <- IPEDSR::get_cipcodes(idbc) %>%
#   filter(grepl("^[0-9]{2}\\.[0-9]{4}$", CIPCODE)) %>%
#   distinct(CIPCODE, Subject)

# completers <- map_dfr(years, function(yr) {
#   tname <- get_completion_table(idbc, yr)

#   tbl(idbc, tname) %>%
#     filter(
#       UNITID %in% !!unitids,
#       as.integer(AWLEVEL) == !!awlevel,
#       MAJORNUM == 1,
#       CIPCODE != "99"
#     ) %>%
#     select(UNITID, CIPCODE, Graduates = CTOTALT) %>%
#     collect() %>%
#     filter(grepl("^[0-9]{2}\\.[0-9]{4}$", CIPCODE)) %>%
#     inner_join(cipcodes, by = "CIPCODE") %>%
#     mutate(Year = yr) %>%
#     select(Year, UNITID, CIPCODE, Subject, Graduates)
# })

# if (interactive()) {
#   View(completers)
# }

# write_csv(completers, file.path(project_dir, "completers_2020_2024.csv"))

# DBI::dbDisconnect(idbc, shutdown = TRUE)




#enrollment of all institutions script

project_dir <- normalizePath(getwd(), mustWork = TRUE)
db_path <- file.path(project_dir, "ipeds_data", "ipeds.duckdb")
out_path <- file.path(project_dir, "all_university_enrollment_2020_2024.csv")

if (!file.exists(db_path)) {
  stop("Could not find IPEDS DuckDB file at: ", db_path)
}

con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

years <- 2020:2024
tables <- dbListTables(con)

find_table <- function(name) {
  matched <- tables[tolower(tables) == tolower(name)]
  if (length(matched) == 0) {
    stop("Missing required IPEDS table: ", name)
  }
  matched[[1]]
}

quote_ident <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}

year_enrollment <- function(year) {
  ef_table <- quote_ident(find_table(sprintf("EF%dA", year)))
  hd_table <- quote_ident(find_table(sprintf("HD%d", year)))

  sql <- sprintf(
    "
    SELECT
      %d AS Year,
      e.UNITID,
      h.INSTNM AS Institution,
      h.CITY AS City,
      h.STABBR AS State,
      h.SECTOR AS Sector,
      e.EFTOTLT AS TotalEnrollment,
      e.EFTOTLM AS Men,
      e.EFTOTLW AS Women,
      e.EFWHITT AS White,
      e.EFBKAAT AS Black,
      e.EFHISPT AS Hispanic,
      e.EFASIAT AS Asian,
      e.EFAIANT AS AmericanIndianOrAlaskaNative,
      e.EFNHPIT AS NativeHawaiianOrPacificIslander,
      e.EF2MORT AS TwoOrMoreRaces,
      e.EFUNKNT AS RaceEthnicityUnknown,
      e.EFNRALT AS NonresidentAlien
    FROM %s e
    LEFT JOIN %s h
      ON e.UNITID = h.UNITID
    WHERE e.EFALEVEL = 1
    ",
    year,
    ef_table,
    hd_table
  )

  dbGetQuery(con, sql)
}

enrollment <- map_dfr(years, year_enrollment)

write_csv(enrollment, out_path)

message("Wrote ", nrow(enrollment), " rows to ", out_path)







# Data for nursing field??
project_dir <- normalizePath(getwd(), mustWork = TRUE)
db_path <- file.path(project_dir, "ipeds_data", "ipeds.duckdb")
out_path <- file.path(project_dir, "caldwell_bsn_nursing_completions_2010_2024.csv")
query_path <- file.path(project_dir, "caldwell_bsn_nursing_completions_2010_2024.sql")

if (!file.exists(db_path)) {
  stop("Could not find IPEDS DuckDB file at: ", db_path)
}

con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

unitid <- 183910L
years <- 2012:2024
cip_code <- "51.3801"
cip_description <- "Registered Nursing/Registered Nurse"
award_level <- 5L
award_level_description <- "Bachelor's degree"

tables <- dbListTables(con)

find_table <- function(name) {
  matched <- tables[tolower(tables) == tolower(name)]
  if (length(matched) == 0) {
    stop("Missing required IPEDS table: ", name)
  }
  matched[[1]]
}

quote_ident <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}

year_query <- function(year) {
  c_table <- quote_ident(find_table(sprintf("C%d_A", year)))
  hd_table <- quote_ident(find_table(sprintf("HD%d", year)))

  sprintf(
    "
    SELECT
      %d AS \"Year\",
      h.INSTNM AS \"Institution\",
      h.STABBR AS \"State\",
      '%s' AS \"CIP Code\",
      '%s' AS \"CIP Description\",
      '%s' AS \"Award Level\",
      COALESCE(c.CTOTALT, 0) AS \"Total Graduate in that year\",
      COALESCE(c.CTOTALM, 0) AS \"Male Graduate\",
      COALESCE(c.CTOTALW, 0) AS \"Female Graduate\",
      COALESCE(c.CWHITT, 0) AS \"White\",
      COALESCE(c.CBKAAT, 0) AS \"Black\",
      COALESCE(c.CHISPT, 0) AS \"Hispanic\",
      COALESCE(c.CASIAT, 0) AS \"Asian\",
      COALESCE(c.CAIANT, 0) AS \"Native American\",
      COALESCE(c.CNHPIT, 0) AS \"Pacific Islander\",
      COALESCE(c.C2MORT, 0) AS \"Two or More Races\",
      COALESCE(c.CUNKNT, 0) AS \"Race Unknown\",
      COALESCE(c.CNRALT, 0) AS \"Nonresident Alien\"
    FROM %s h
    LEFT JOIN (
      SELECT
        UNITID,
        SUM(CTOTALT) AS CTOTALT,
        SUM(CTOTALM) AS CTOTALM,
        SUM(CTOTALW) AS CTOTALW,
        SUM(CWHITT) AS CWHITT,
        SUM(CBKAAT) AS CBKAAT,
        SUM(CHISPT) AS CHISPT,
        SUM(CASIAT) AS CASIAT,
        SUM(CAIANT) AS CAIANT,
        SUM(CNHPIT) AS CNHPIT,
        SUM(C2MORT) AS C2MORT,
        SUM(CUNKNT) AS CUNKNT,
        SUM(CNRALT) AS CNRALT
      FROM %s
      WHERE UNITID = %d
        AND CIPCODE = '%s'
        AND AWLEVEL = %d
        AND MAJORNUM = 1
      GROUP BY UNITID
    ) c
      ON c.UNITID = h.UNITID
    WHERE h.UNITID = %d
    ",
    year,
    cip_code,
    gsub("'", "''", cip_description, fixed = TRUE),
    gsub("'", "''", award_level_description, fixed = TRUE),
    hd_table,
    c_table,
    unitid,
    cip_code,
    award_level,
    unitid
  )
}

sql <- paste(vapply(years, year_query, character(1)), collapse = "\nUNION ALL\n")
sql <- paste(sql, "\nORDER BY \"Year\"")

writeLines(sql, query_path)

result <- dbGetQuery(con, sql)

write.csv(result, out_path, row.names = FALSE, na = "")

cat("Wrote ", nrow(result), " rows to ", out_path, "\n", sep = "")
cat("Wrote query to ", query_path, "\n", sep = "")






#queries to create student enrollment dashboard and more information for all institutions

db_path <- file.path(project_dir, "ipeds_data", "ipeds.duckdb")
out_path <- file.path(project_dir, "enrollment_dashboard.csv")

if (!file.exists(db_path)) {
  stop("Could not find IPEDS DuckDB file at: ", db_path)
}

con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

years <- 2020:2024
tables <- dbListTables(con)

find_table <- function(name) {
  matched <- tables[tolower(tables) == tolower(name)]
  if (length(matched) == 0) {
    stop("Missing required IPEDS table: ", name)
  }
  matched[[1]]
}

quote_ident <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}

year_enrollment <- function(year) {
  ef_table <- quote_ident(find_table(sprintf("EF%dA", year)))
  efd_table <- quote_ident(find_table(sprintf("EF%dD", year)))
  hd_table <- quote_ident(find_table(sprintf("HD%d", year)))

  sql <- sprintf(
    "
    WITH enrollment_by_level AS (
      SELECT
        UNITID,
        SUM(CASE WHEN EFALEVEL = 1 THEN EFTOTLT END) AS TotalEnrollment,
        SUM(CASE WHEN EFALEVEL = 2 THEN EFTOTLT END) AS UndergraduateEnrollment,
        SUM(CASE WHEN EFALEVEL = 12 THEN EFTOTLT END) AS GraduateEnrollment
      FROM %s
      WHERE EFALEVEL IN (1, 2, 12)
      GROUP BY UNITID
    )
    SELECT
      %d AS Year,
      h.UNITID,
      h.INSTNM AS Institution,
      h.STABBR AS State,
      e.TotalEnrollment,
      e.UndergraduateEnrollment,
      e.GraduateEnrollment,
      d.GRCOHRT AS FirstTimeStudents,
      d.UGENTERN - d.GRCOHRT AS TransferStudents,
      d.UGENTERN AS NewStudentEnrollment
    FROM enrollment_by_level e
    LEFT JOIN %s h
      ON e.UNITID = h.UNITID
    LEFT JOIN %s d
      ON e.UNITID = d.UNITID
    ORDER BY h.UNITID
    ",
    ef_table,
    year,
    hd_table,
    efd_table
  )

  dbGetQuery(con, sql)
}

enrollment_dashboard <- map_dfr(years, year_enrollment)

write_csv(enrollment_dashboard, out_path)

message("Wrote ", nrow(enrollment_dashboard), " rows to ", out_path)





#queries to create student enrollment dashboard of institution and wheter the institution is public or private and more information for all institutions

library(DBI)
  library(duckdb)

  conn <- dbConnect(
    duckdb::duckdb(),
    dbdir = "ipeds_data/ipeds.duckdb",
    read_only = TRUE
  )

  years <- 2010:2024
  tables <- dbListTables(con)

  find_table <- function(name) {
    hit <- tables[tolower(tables) == tolower(name)]
    if (length(hit) == 0) stop("Missing table: ", name)
    hit[[1]]
  }

  quote_ident <- function(x) {
    paste0('"', gsub('"', '""', x), '"')
  }

  year_queries <- lapply(years, function(y) {
    ef <- quote_ident(find_table(paste0("EF", y, "A")))
    hd <- quote_ident(find_table(paste0("HD", y)))

    sprintf("
      SELECT
        %d AS Year,
        e.UNITID,
        h.INSTNM AS Institution,
        h.CITY AS City,
        h.STABBR AS State,

        h.SECTOR AS SectorCode,
        CASE h.SECTOR
          WHEN 0 THEN 'Administrative unit'
          WHEN 1 THEN 'Public, 4-year or above'
          WHEN 2 THEN 'Private nonprofit, 4-year or above'
          WHEN 3 THEN 'Private for-profit, 4-year or above'
          WHEN 4 THEN 'Public, 2-year'
          WHEN 5 THEN 'Private nonprofit, 2-year'
          WHEN 6 THEN 'Private for-profit, 2-year'
          WHEN 7 THEN 'Public, less-than-2-year'
          WHEN 8 THEN 'Private nonprofit, less-than-2-year'
          WHEN 9 THEN 'Private for-profit, less-than-2-year'
          WHEN 99 THEN 'Sector unknown'
          ELSE 'Unknown'
        END AS Sector,

        h.ICLEVEL AS LevelCode,
        CASE h.ICLEVEL
          WHEN 1 THEN '4-year or above'
          WHEN 2 THEN '2-year'
          WHEN 3 THEN 'Less-than-2-year'
          ELSE 'Unknown'
        END AS InstitutionLevel,

        h.CONTROL AS ControlCode,
        CASE h.CONTROL
          WHEN 1 THEN 'Public'
          WHEN 2 THEN 'Private nonprofit'
          WHEN 3 THEN 'Private for-profit'
          ELSE 'Unknown'
        END AS Control,

        e.EFTOTLT AS TotalEnrollment,
        e.EFTOTLM AS Men,
        e.EFTOTLW AS Women

      FROM %s e
      LEFT JOIN %s h
        ON e.UNITID = h.UNITID
      WHERE e.EFALEVEL = 1
    ", y, ef, hd)
  })

  sql <- paste(year_queries, collapse = "\nUNION ALL\n")

  dbExecute(con, sprintf(
    "COPY (%s)
     TO 'all_university_enrollment_2010_2024.csv'
     (HEADER, DELIMITER ',');",
    sql
  ))

  dbDisconnect(con, shutdown = TRUE)

























####Caldwell Graduation
#!/usr/bin/env Rscript

# Extract approximately 10 entering cohorts of IPEDS Graduation Rates data for
# Caldwell University and selected peers from a local IPEDSR DuckDB database.
#
# The database is opened read-only. Set IPEDSR_DUCKDB_PATH to override the
# default path of ipeds_data/ipeds.duckdb under the current project directory.

# suppressPackageStartupMessages({
#   library(DBI)
#   library(duckdb)
#   library(dplyr)
#   library(dbplyr)
#   library(tidyr)
#   library(purrr)
#   library(stringr)
#   library(readr)
#   library(tibble)
# })

project_dir <- normalizePath(getwd(), mustWork = TRUE)
db_path <- Sys.getenv(
  "IPEDS_DUCKDB_PATH",
  unset = file.path(project_dir, "ipeds_data", "ipeds.duckdb")
)
out_path <- file.path(project_dir, "caldwell_peer_graduation_rates.csv")

if (!file.exists(db_path)) {
  stop("Could not find IPEDS DuckDB database at: ", db_path)
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
on.exit({
  try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
}, add = TRUE)

all_tables <- DBI::dbListTables(con)
print(all_tables)

qident <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))

table_year <- function(x) {
  yy <- str_match(x, "(\\d{4}|\\d{2})$")[, 2]
  out <- suppressWarnings(as.integer(yy))
  ifelse(!is.na(out) & out < 100, 2000 + out, out)
}

detect_yearly_tables <- function(pattern) {
  tibble(TableName = all_tables) %>%
    mutate(SurveyYear = table_year(TableName)) %>%
    filter(str_detect(str_to_lower(TableName), regex(pattern, ignore_case = TRUE))) %>%
    filter(!is.na(SurveyYear)) %>%
    arrange(SurveyYear)
}

metadata_tables <- tibble(TableName = all_tables) %>%
  mutate(
    lower_name = str_to_lower(TableName),
    SurveyYear = table_year(TableName),
    Kind = case_when(
      str_detect(lower_name, "^tables\\d{2}$") ~ "tables",
      str_detect(lower_name, "^vartable\\d{2}$") ~ "vartable",
      str_detect(lower_name, "^valuesets\\d{2}$") ~ "valuesets",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Kind), !is.na(SurveyYear)) %>%
  select(SurveyYear, Kind, TableName) %>%
  arrange(SurveyYear, Kind)

print(metadata_tables)

gr_tables <- detect_yearly_tables("^gr\\d{4}$|^gr\\d{4}_gender$")
gr_tables <- gr_tables %>%
  filter(str_detect(str_to_lower(TableName), "^gr\\d{4}$"))

gr200_tables <- detect_yearly_tables("^gr200_\\d{2}$")
hd_tables <- detect_yearly_tables("^hd\\d{4}$")

if (nrow(gr_tables) == 0) stop("No GR survey tables found.")
if (nrow(hd_tables) == 0) stop("No HD institutional directory tables found.")

read_table_if_exists <- function(table_name) {
  if (length(table_name) != 1 || is.na(table_name) || !(table_name %in% all_tables)) {
    return(tibble())
  }
  as_tibble(DBI::dbReadTable(con, table_name))
}

metadata_for_year <- function(year) {
  mt <- metadata_tables %>% filter(SurveyYear == year)
  t_name <- mt %>% filter(Kind == "tables") %>% pull(TableName) %>% first()
  v_name <- mt %>% filter(Kind == "vartable") %>% pull(TableName) %>% first()
  s_name <- mt %>% filter(Kind == "valuesets") %>% pull(TableName) %>% first()

  table_meta <- read_table_if_exists(t_name)
  var_meta <- read_table_if_exists(v_name)
  values_meta <- read_table_if_exists(s_name)

  if (nrow(var_meta) == 0) {
    return(list(tables = tibble(), vars = tibble(), values = tibble()))
  }

  list(
    tables = table_meta %>% mutate(SurveyYear = year),
    vars = var_meta %>% mutate(SurveyYear = year),
    values = values_meta %>% mutate(SurveyYear = year)
  )
}

metadata_years <- sort(unique(metadata_tables$SurveyYear))
metadata_list <- set_names(map(metadata_years, metadata_for_year), metadata_years)

table_metadata <- map_dfr(metadata_list, "tables")
variable_metadata <- map_dfr(metadata_list, "vars")
valueset_metadata <- map_dfr(metadata_list, "values")

metadata_search_terms <- c(
  "Graduation rate", "Graduation Rates", "GR200", "Adjusted cohort",
  "Revised cohort", "Initial cohort", "Exclusions",
  "Completers within 100% of normal time",
  "Completers within 150% of normal time",
  "Completers within 200% of normal time", "Transfer-out",
  "Bachelor", "bachelor's degree-seeking", "First-time", "Full-time",
  "Degree-seeking", "Certificate-seeking", "Men", "Women",
  "Race/ethnicity", "Total students"
)

metadata_pattern <- regex(
  str_c(str_replace_all(metadata_search_terms, "([\\W])", "\\\\\\1"), collapse = "|"),
  ignore_case = TRUE
)

metadata_verification <- variable_metadata %>%
  left_join(
    table_metadata %>%
      select(SurveyYear, TableName, SurveyNameFromTables = Survey, TableTitleFromTables = TableTitle),
    by = c("SurveyYear", "TableName")
  ) %>%
  mutate(
    SurveyName = coalesce(SurveyNameFromTables, Survey),
    TableTitle = coalesce(TableTitleFromTables, TableTitle),
    SearchText = str_squish(str_c(SurveyName, TableName, TableTitle, varName, varTitle, longDescription, sep = " | ")),
    SelectedForExtraction = str_detect(str_to_lower(TableName), "^gr\\d{4}$|^gr200_\\d{2}$") &
      str_detect(SearchText, metadata_pattern),
    VariableName = varName,
    VariableLabel = coalesce(varTitle, longDescription)
  ) %>%
  filter(
    str_detect(str_to_lower(TableName), "^gr\\d{4}$|^gr200_\\d{2}$") |
      str_detect(str_to_lower(SurveyName), "graduation") |
      str_detect(str_to_lower(TableTitle), "graduation")
  ) %>%
  filter(str_detect(SearchText, metadata_pattern) | SelectedForExtraction) %>%
  transmute(
    SurveyYear,
    SurveyName,
    TableName,
    VariableName,
    VariableLabel,
    SelectedForExtraction
  ) %>%
  arrange(SurveyYear, TableName, VariableName)

cat("\nMetadata verification table:\n")
print(metadata_verification, n = Inf)

selected_meta <- metadata_verification %>%
  filter(SelectedForExtraction)

required_gr200_vars <- c(
  "BAREVCT", "BAEXCLU", "BAAC150", "BANC100", "BANC150",
  "BAAEXCL", "BAAC200", "BANC200"
)

if (nrow(selected_meta) == 0) {
  stop("Could not identify Graduation Rates metadata. Review Tables/vartable/valuesets tables.")
}

if (!any(required_gr200_vars %in% selected_meta$VariableName)) {
  stop("Could not verify bachelor GR200 variables in metadata.")
}

latest_hd <- hd_tables %>% filter(SurveyYear == max(SurveyYear)) %>% pull(TableName) %>% first()

state_lookup <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California",
  CO = "Colorado", CT = "Connecticut", DE = "Delaware", DC = "District of Columbia",
  FL = "Florida", GA = "Georgia", HI = "Hawaii", ID = "Idaho", IL = "Illinois",
  IN = "Indiana", IA = "Iowa", KS = "Kansas", KY = "Kentucky", LA = "Louisiana",
  ME = "Maine", MD = "Maryland", MA = "Massachusetts", MI = "Michigan",
  MN = "Minnesota", MS = "Mississippi", MO = "Missouri", MT = "Montana",
  NE = "Nebraska", NV = "Nevada", NH = "New Hampshire", NJ = "New Jersey",
  NM = "New Mexico", NY = "New York", NC = "North Carolina", ND = "North Dakota",
  OH = "Ohio", OK = "Oklahoma", OR = "Oregon", PA = "Pennsylvania",
  RI = "Rhode Island", SC = "South Carolina", SD = "South Dakota",
  TN = "Tennessee", TX = "Texas", UT = "Utah", VT = "Vermont", VA = "Virginia",
  WA = "Washington", WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming"
)

sector_lookup <- c(
  `0` = "Administrative Unit",
  `1` = "Public, 4-year or above",
  `2` = "Private nonprofit, 4-year or above",
  `3` = "Private for-profit, 4-year or above",
  `4` = "Public, 2-year",
  `5` = "Private nonprofit, 2-year",
  `6` = "Private for-profit, 2-year",
  `7` = "Public, less-than 2-year",
  `8` = "Private nonprofit, less-than 2-year",
  `9` = "Private for-profit, less-than 2-year"
)

control_lookup <- c(
  `1` = "Public",
  `2` = "Private nonprofit",
  `3` = "Private for-profit"
)

requested_institutions <- tribble(
  ~DisplayOrder, ~RequestedName, ~ExpectedCity, ~ExpectedState, ~HistoricalNames,
  1L, "Caldwell University", "Caldwell", "NJ", "Caldwell College",
  2L, "Centenary University", "Hackettstown", "NJ", "Centenary College",
  3L, "Drew University", "Madison", "NJ", NA_character_,
  4L, "Emory & Henry University", "Emory", "VA", "Emory & Henry College",
  5L, "Felician University", "Lodi", "NJ", NA_character_,
  6L, "Georgian Court University", "Lakewood", "NJ", NA_character_,
  7L, "Holy Family University", "Philadelphia", "PA", NA_character_,
  8L, "McKendree University", "Lebanon", "IL", NA_character_,
  9L, "Mount Saint Mary College", "Newburgh", "NY", NA_character_,
  10L, "Neumann University", "Aston", "PA", NA_character_,
  11L, "Saint Peter's University", "Jersey City", "NJ", "Saint Peters University|Saint Peter’s University",
  12L, "University of Mount Saint Vincent", "Bronx", "NY", "College of Mount Saint Vincent",
  13L, "University of Evansville", "Evansville", "IN", NA_character_,
  14L, "Wagner College", "Staten Island", "NY", NA_character_,
  15L, "Westminster College", "New Wilmington", "PA", NA_character_
)

normalize_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[\u2018\u2019`]", "'") %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

hd_history <- map_dfr(hd_tables$TableName, function(tbl) {
  yr <- hd_tables %>% filter(TableName == tbl) %>% pull(SurveyYear) %>% first()
  cols <- DBI::dbGetQuery(con, paste0("DESCRIBE SELECT * FROM ", qident(tbl)))$column_name
  keep <- intersect(c("UNITID", "INSTNM", "IALIAS", "CITY", "STABBR", "SECTOR", "CONTROL"), cols)
  DBI::dbGetQuery(
    con,
    paste0("SELECT ", str_c(qident(keep), collapse = ", "), " FROM ", qident(tbl))
  ) %>%
    as_tibble() %>%
    mutate(DirectoryYear = yr)
})

resolve_one_institution <- function(req) {
  aliases <- c(req$RequestedName, str_split(coalesce(req$HistoricalNames, ""), "\\|")[[1]]) %>%
    discard(~ is.na(.x) || .x == "")
  alias_norm <- normalize_name(aliases)

  candidates <- hd_history %>%
    mutate(
      NameNorm = normalize_name(INSTNM),
      AliasNorm = normalize_name(coalesce(IALIAS, "")),
      ExpectedState = req$ExpectedState,
      ExpectedCity = req$ExpectedCity,
      NameMatchType = case_when(
        NameNorm == normalize_name(req$RequestedName) ~ "Exact match",
        NameNorm %in% alias_norm | str_detect(AliasNorm, regex(str_c(alias_norm, collapse = "|"), ignore_case = TRUE)) ~
          "Historical-name match",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(NameMatchType), STABBR == ExpectedState, str_to_lower(CITY) == str_to_lower(ExpectedCity))

  unitids <- candidates %>% distinct(UNITID) %>% pull(UNITID)

  if (length(unitids) == 0) {
    return(tibble(
      RequestedName = req$RequestedName,
      MatchedIPEDSName = NA_character_,
      UNITID = NA_integer_,
      City = req$ExpectedCity,
      State = req$ExpectedState,
      PeerGroup = if_else(req$DisplayOrder == 1L, "Selected Institution", "Peer Institution"),
      DisplayOrder = req$DisplayOrder,
      IsCaldwell = req$DisplayOrder == 1L,
      MatchStatus = "Not found",
      SectorCode = NA_integer_,
      ControlCode = NA_integer_
    ))
  }

  if (length(unitids) > 1) {
    stop("Multiple UNITIDs matched for ", req$RequestedName, ": ", str_c(unitids, collapse = ", "))
  }

  selected <- candidates %>%
    arrange(desc(DirectoryYear), desc(NameMatchType == "Exact match")) %>%
    slice(1)

  tibble(
    RequestedName = req$RequestedName,
    MatchedIPEDSName = selected$INSTNM,
    UNITID = as.integer(selected$UNITID),
    City = selected$CITY,
    State = selected$STABBR,
    PeerGroup = if_else(req$DisplayOrder == 1L, "Selected Institution", "Peer Institution"),
    DisplayOrder = req$DisplayOrder,
    IsCaldwell = req$DisplayOrder == 1L,
    MatchStatus = selected$NameMatchType,
    SectorCode = as.integer(selected$SECTOR),
    ControlCode = as.integer(selected$CONTROL)
  )
}

institution_lookup <- pmap_dfr(requested_institutions, function(...) {
  resolve_one_institution(tibble(...))
}) %>%
  mutate(
    StateName = unname(state_lookup[State]),
    Sector = unname(sector_lookup[as.character(SectorCode)]),
    Control = unname(control_lookup[as.character(ControlCode)])
  )

cat("\nVerified institution lookup table:\n")
print(
  institution_lookup %>%
    select(RequestedName, MatchedIPEDSName, UNITID, City, State, PeerGroup,
           DisplayOrder, IsCaldwell, MatchStatus),
  n = Inf
)

if (any(institution_lookup$MatchStatus %in% c("Not found", "Manual review required"))) {
  stop("At least one requested institution was not verified.")
}
if (n_distinct(institution_lookup$UNITID) != nrow(institution_lookup)) {
  stop("Duplicate UNITID detected in institution lookup.")
}
if (nrow(institution_lookup) != 15L) stop("Institution lookup must contain exactly 15 institutions.")
if (sum(institution_lookup$IsCaldwell) != 1L) stop("Institution lookup must contain exactly one Caldwell row.")
if (any(institution_lookup$State != requested_institutions$ExpectedState)) {
  stop("At least one institution matched to the wrong state.")
}

extract_cohort_year <- function(labels, reporting_year, offset) {
  label_text <- str_c(labels, collapse = " | ")
  yrs <- str_extract_all(label_text, "\\b(19|20)\\d{2}\\b")[[1]]
  yrs <- as.integer(yrs)
  yrs <- yrs[yrs >= 1990 & yrs <= reporting_year]
  if (length(yrs) > 0) {
    max(yrs, na.rm = TRUE)
  } else {
    reporting_year - offset
  }
}

label_for_code <- function(year, table_name, var_name, code_value) {
  valueset_metadata %>%
    filter(
      SurveyYear == year,
      str_to_lower(TableName) == str_to_lower(table_name),
      str_to_upper(varName) == str_to_upper(var_name),
      as.character(Codevalue) == as.character(code_value)
    ) %>%
    pull(valueLabel) %>%
    first()
}

clean_ipeds_value <- function(df, var_name) {
  if (!(var_name %in% names(df))) return(rep(NA_real_, nrow(df)))
  flags <- paste0("X", var_name)
  x <- suppressWarnings(as.numeric(df[[var_name]]))
  if (flags %in% names(df)) {
    flag_value <- df[[flags]]
    x[!is.na(flag_value) & !(flag_value %in% c("R", "Z"))] <- NA_real_
  }
  x
}

select_gr_value <- function(df, grtype_codes) {
  if (nrow(df) == 0) return(NA_real_)
  val <- df %>%
    filter(GRTYPE %in% grtype_codes) %>%
    arrange(match(GRTYPE, grtype_codes)) %>%
    slice(1) %>%
    pull(Value)
  if (length(val) == 0) NA_real_ else as.numeric(val[[1]])
}

extract_gr_year <- function(gr_table, reporting_year) {
  cols <- DBI::dbGetQuery(con, paste0("DESCRIBE SELECT * FROM ", qident(gr_table)))$column_name
  required <- c("UNITID", "GRTYPE", "SECTION", "COHORT", "GRTOTLT")
  if (!all(required %in% cols)) {
    warning("Skipping ", gr_table, " because required GR columns are missing.")
    return(tibble())
  }

  value_labels <- valueset_metadata %>%
    filter(SurveyYear == reporting_year, str_to_lower(TableName) == str_to_lower(gr_table))

  bachelor_section_codes <- value_labels %>%
    filter(str_to_upper(varName) %in% c("SECTION", "COHORT")) %>%
    filter(str_detect(valueLabel, regex("bachelor", ignore_case = TRUE))) %>%
    filter(!str_detect(valueLabel, regex("\\+|other degree|other degree/cert", ignore_case = TRUE))) %>%
    mutate(code = suppressWarnings(as.integer(Codevalue))) %>%
    pull(code) %>%
    unique()

  if (length(bachelor_section_codes) == 0) bachelor_section_codes <- 2L

  grtype_map <- value_labels %>%
    filter(str_to_upper(varName) == "GRTYPE") %>%
    mutate(
      Code = suppressWarnings(as.integer(Codevalue)),
      LabelLower = str_to_lower(valueLabel)
    )

  revised_codes <- grtype_map %>%
    filter(str_detect(LabelLower, "bachelor.*subcohort")) %>%
    filter(!str_detect(LabelLower, "exclusion|other degree|other degree/cert|\\+")) %>%
    pull(Code)
  exclusion_codes <- grtype_map %>%
    filter(str_detect(LabelLower, "bachelor.*exclusion|exclusion.*bachelor")) %>%
    pull(Code)
  adjusted_codes <- grtype_map %>%
    filter(str_detect(LabelLower, "adjusted cohort")) %>%
    pull(Code)
  completers150_codes <- grtype_map %>%
    filter(str_detect(LabelLower, "bachelor")) %>%
    filter(str_detect(LabelLower, "150%|150 percent")) %>%
    filter(str_detect(LabelLower, "completers")) %>%
    filter(!str_detect(LabelLower, "less than 2|2 but less than 4|100%|100 percent|4 years|5 years|6 years")) %>%
    pull(Code)
  transfer_codes <- grtype_map %>% filter(str_detect(LabelLower, "transfer-out")) %>% pull(Code)

  sql <- paste0(
    "SELECT * FROM ", qident(gr_table),
    " WHERE UNITID IN (", str_c(institution_lookup$UNITID, collapse = ","), ")"
  )

  raw <- as_tibble(DBI::dbGetQuery(con, sql)) %>%
    mutate(
      ReportingYear = reporting_year,
      Value = clean_ipeds_value(., "GRTOTLT")
    )

  if (nrow(raw) == 0) return(tibble())

  selected <- raw %>%
    filter(SECTION %in% bachelor_section_codes & COHORT %in% bachelor_section_codes)

  if (nrow(selected) == 0) selected <- raw %>% filter(GRTYPE %in% c(6, 7, 8, 9, 12, 16))

  cohort_labels <- c(
    label_for_code(reporting_year, gr_table, "SECTION", first(na.omit(selected$SECTION))),
    label_for_code(reporting_year, gr_table, "COHORT", first(na.omit(selected$COHORT)))
  )
  metadata_cohort_year <- extract_cohort_year(cohort_labels, reporting_year, offset = 6L)

  # For the bachelor's cohort, the GR survey reports the 150% outcome about
  # six years after entry. Metadata labels are printed above for verification;
  # older IPEDSR metadata can be incomplete or inconsistent, so this explicit
  # derivation is used for a stable UNITID + CohortYear join key.
  cohort_year <- reporting_year - 6L
  if (!is.na(metadata_cohort_year) && abs(metadata_cohort_year - cohort_year) > 1L) {
    warning(
      "GR metadata cohort year for ", gr_table, " suggested ", metadata_cohort_year,
      "; using reporting year - 6 = ", cohort_year, "."
    )
  }

  selected %>%
    group_split(UNITID) %>%
    map_dfr(function(one_inst) {
      tibble(
      UNITID = as.integer(first(one_inst$UNITID)),
      CohortYear = cohort_year,
      GRReportingYear = reporting_year,
      CohortType = "First-time, full-time degree/certificate-seeking students",
      AwardLevel = "Bachelor's degree-seeking cohort",
      InitialCohort = select_gr_value(one_inst, revised_codes),
      Exclusions = select_gr_value(one_inst, exclusion_codes),
      AdjustedCohort = select_gr_value(one_inst, adjusted_codes),
      Completers150 = select_gr_value(one_inst, completers150_codes),
      TransferOut = select_gr_value(one_inst, transfer_codes)
      )
    })
}

extract_gr200_year <- function(gr200_table, reporting_year) {
  cols <- DBI::dbGetQuery(con, paste0("DESCRIBE SELECT * FROM ", qident(gr200_table)))$column_name
  needed <- intersect(required_gr200_vars, cols)
  if (!all(c("UNITID", "BAREVCT", "BAAC150", "BANC100", "BANC150", "BAAC200", "BANC200") %in% cols)) {
    warning("Skipping ", gr200_table, " because required bachelor GR200 columns are missing.")
    return(tibble())
  }

  vars_for_year <- variable_metadata %>%
    filter(SurveyYear == reporting_year, str_to_lower(TableName) == str_to_lower(gr200_table))

  metadata_cohort_year <- vars_for_year %>%
    filter(varName %in% c("BAREVCT", "BAAC150", "BANC100", "BANC150", "BAAC200", "BANC200")) %>%
    transmute(label = str_c(varTitle, longDescription, sep = " | ")) %>%
    pull(label) %>%
    extract_cohort_year(reporting_year = reporting_year, offset = 8L)

  # GR200 reports 200% bachelor's outcomes about eight years after entry.
  cohort_year <- reporting_year - 8L
  if (!is.na(metadata_cohort_year) && abs(metadata_cohort_year - cohort_year) > 1L) {
    warning(
      "GR200 metadata cohort year for ", gr200_table, " suggested ", metadata_cohort_year,
      "; using reporting year - 8 = ", cohort_year, "."
    )
  }

  sql <- paste0(
    "SELECT * FROM ", qident(gr200_table),
    " WHERE UNITID IN (", str_c(institution_lookup$UNITID, collapse = ","), ")"
  )

  raw <- as_tibble(DBI::dbGetQuery(con, sql))
  if (nrow(raw) == 0) return(tibble())

  tibble(
    UNITID = as.integer(raw$UNITID),
    CohortYear = cohort_year,
    GR200ReportingYear = reporting_year,
    GR200InitialCohort = clean_ipeds_value(raw, "BAREVCT"),
    GR200Exclusions150 = clean_ipeds_value(raw, "BAEXCLU"),
    GR200AdjustedCohort150 = clean_ipeds_value(raw, "BAAC150"),
    Completers100 = clean_ipeds_value(raw, "BANC100"),
    GR200Completers150 = clean_ipeds_value(raw, "BANC150"),
    AdditionalExclusions200 = clean_ipeds_value(raw, "BAAEXCL"),
    GR200AdjustedCohort200 = clean_ipeds_value(raw, "BAAC200"),
    Completers200 = clean_ipeds_value(raw, "BANC200")
  )
}

gr_data <- pmap_dfr(gr_tables, function(TableName, SurveyYear) {
  extract_gr_year(TableName, SurveyYear)
})

gr200_data <- pmap_dfr(gr200_tables, function(TableName, SurveyYear) {
  extract_gr200_year(TableName, SurveyYear)
})

if (nrow(gr_data) == 0) stop("No usable GR data were extracted.")

caldwell_unitid <- institution_lookup %>% filter(IsCaldwell) %>% pull(UNITID)

eligible_cohorts <- gr_data %>%
  filter(UNITID == caldwell_unitid, !is.na(Completers150), !is.na(AdjustedCohort), AdjustedCohort > 0) %>%
  distinct(CohortYear) %>%
  arrange(desc(CohortYear)) %>%
  slice_head(n = 10) %>%
  arrange(CohortYear) %>%
  pull(CohortYear)

if (length(eligible_cohorts) < 8L) {
  stop("Fewer than 8 Caldwell cohort years with valid 150% Graduation Rates data were found.")
}

cat("\nSelected cohort years from Caldwell valid 150% data:\n")
print(eligible_cohorts)

cohort_grid <- expand_grid(
  UNITID = institution_lookup$UNITID,
  CohortYear = eligible_cohorts
)

final_graduation_data <- cohort_grid %>%
  left_join(gr_data, by = c("UNITID", "CohortYear")) %>%
  left_join(gr200_data, by = c("UNITID", "CohortYear")) %>%
  left_join(
    institution_lookup %>%
      select(UNITID, Institution = MatchedIPEDSName, City, State, Sector, Control,
             PeerGroup, DisplayOrder, IsCaldwell),
    by = "UNITID"
  ) %>%
  mutate(
    InitialCohort = coalesce(GR200InitialCohort, InitialCohort),
    Exclusions = coalesce(GR200Exclusions150, Exclusions),
    AdjustedCohort = coalesce(GR200AdjustedCohort150, AdjustedCohort),
    Completers150 = coalesce(GR200Completers150, Completers150),
    # Some GR tables omit a bachelor-only exclusion row while still reporting
    # the revised and adjusted bachelor cohorts. In that case, retain the
    # official cohorts and derive the exclusion count from their difference.
    Exclusions = if_else(
      !is.na(InitialCohort) & !is.na(AdjustedCohort) &
        (is.na(Exclusions) | AdjustedCohort != InitialCohort - Exclusions),
      InitialCohort - AdjustedCohort,
      Exclusions
    ),
    CohortType = coalesce(CohortType, "First-time, full-time degree/certificate-seeking students"),
    AwardLevel = coalesce(AwardLevel, "Bachelor's degree-seeking cohort"),
    GraduationRate100 = if_else(!is.na(AdjustedCohort) & AdjustedCohort > 0 & !is.na(Completers100),
                                Completers100 / AdjustedCohort * 100, NA_real_),
    GraduationRate150 = if_else(!is.na(AdjustedCohort) & AdjustedCohort > 0 & !is.na(Completers150),
                                Completers150 / AdjustedCohort * 100, NA_real_),
    GraduationRate200 = if_else(!is.na(GR200AdjustedCohort200) & GR200AdjustedCohort200 > 0 & !is.na(Completers200),
                                Completers200 / GR200AdjustedCohort200 * 100, NA_real_),
    TransferOutRate = if_else(!is.na(AdjustedCohort) & AdjustedCohort > 0 & !is.na(TransferOut),
                              TransferOut / AdjustedCohort * 100, NA_real_),
    PeerGroupName = "Caldwell University Graduation Rate Peer Group"
  ) %>%
  select(
    UNITID, Institution, City, State, Sector, Control, CohortYear,
    GRReportingYear, GR200ReportingYear, CohortType, AwardLevel,
    InitialCohort, Exclusions, AdjustedCohort, Completers100,
    Completers150, Completers200, TransferOut, GraduationRate100,
    GraduationRate150, GraduationRate200, TransferOutRate, PeerGroup,
    DisplayOrder, IsCaldwell, PeerGroupName
  ) %>%
  arrange(DisplayOrder, CohortYear)

# Dashboard example: calculate weighted peer rates from counts, excluding Caldwell.
#
# weighted_peer_rates <- final_graduation_data %>%
#   filter(!IsCaldwell) %>%
#   group_by(CohortYear) %>%
#   summarise(
#     WeightedPeerRate150 =
#       sum(Completers150[!is.na(Completers150) & !is.na(AdjustedCohort)], na.rm = TRUE) /
#       sum(AdjustedCohort[!is.na(Completers150) & !is.na(AdjustedCohort)], na.rm = TRUE) * 100,
#     PeerMedianRate150 = median(GraduationRate150, na.rm = TRUE),
#     .groups = "drop"
#   )

validation_failures <- character()

add_failure <- function(condition, message) {
  if (!isTRUE(condition)) validation_failures <<- c(validation_failures, message)
}

rate_cols <- c("GraduationRate100", "GraduationRate150", "GraduationRate200", "TransferOutRate")
invalid_rate_counts <- final_graduation_data %>%
  summarise(across(all_of(rate_cols), ~ sum(!is.na(.x) & (.x < 0 | .x > 100)))) %>%
  pivot_longer(everything(), names_to = "Rate", values_to = "InvalidCount")

duplicate_count <- final_graduation_data %>%
  count(UNITID, CohortYear, name = "n") %>%
  filter(n > 1) %>%
  nrow()

add_failure(n_distinct(final_graduation_data$UNITID) == 15L, "Main dataset does not contain exactly 15 unique institutions.")
add_failure(sum(distinct(final_graduation_data, UNITID, IsCaldwell)$IsCaldwell) == 1L, "Main dataset does not contain exactly one selected institution.")
add_failure(sum(!distinct(final_graduation_data, UNITID, IsCaldwell)$IsCaldwell) == 14L, "Main dataset does not contain exactly 14 peer institutions.")
add_failure(any(final_graduation_data$UNITID == caldwell_unitid), "Caldwell University is missing.")
add_failure(all(count(institution_lookup, UNITID)$n == 1L), "One or more institutions does not have one verified UNITID.")
add_failure(sum(invalid_rate_counts$InvalidCount) == 0L, "One or more graduation/transfer rates are outside 0-100.")
add_failure(!any(!is.na(final_graduation_data$Completers100) & !is.na(final_graduation_data$AdjustedCohort) & final_graduation_data$Completers100 > final_graduation_data$AdjustedCohort), "Completers100 exceeds AdjustedCohort.")
add_failure(!any(!is.na(final_graduation_data$Completers150) & !is.na(final_graduation_data$AdjustedCohort) & final_graduation_data$Completers150 > final_graduation_data$AdjustedCohort), "Completers150 exceeds AdjustedCohort.")
add_failure(!any(!is.na(final_graduation_data$Completers200) & !is.na(final_graduation_data$AdjustedCohort) & final_graduation_data$Completers200 > final_graduation_data$AdjustedCohort), "Completers200 exceeds AdjustedCohort.")
add_failure(!any(!is.na(final_graduation_data$Completers100) & !is.na(final_graduation_data$Completers150) & final_graduation_data$Completers100 > final_graduation_data$Completers150), "Completers100 is greater than Completers150.")
add_failure(!any(!is.na(final_graduation_data$Completers150) & !is.na(final_graduation_data$Completers200) & final_graduation_data$Completers150 > final_graduation_data$Completers200), "Completers150 is greater than Completers200.")
add_failure(!any(!is.na(final_graduation_data$InitialCohort) & !is.na(final_graduation_data$Exclusions) & !is.na(final_graduation_data$AdjustedCohort) & final_graduation_data$AdjustedCohort != final_graduation_data$InitialCohort - final_graduation_data$Exclusions), "AdjustedCohort does not equal InitialCohort minus Exclusions for one or more rows.")
add_failure(duplicate_count == 0L, "Duplicate UNITID + CohortYear rows found.")
add_failure(n_distinct(final_graduation_data$CohortYear) >= 8L & n_distinct(final_graduation_data$CohortYear) <= 11L, "Selected cohort range is not approximately 10 cohort years.")
add_failure(any(is.na(final_graduation_data$GraduationRate100) | is.na(final_graduation_data$GraduationRate150) | is.na(final_graduation_data$GraduationRate200) | is.na(final_graduation_data$TransferOutRate)), "No missing values found in rate fields; verify NA preservation.")
add_failure(all(is.na(final_graduation_data$GR200ReportingYear) | final_graduation_data$GR200ReportingYear - final_graduation_data$CohortYear %in% 7:9), "GR200 records do not align to the expected entering cohort window.")
add_failure(nrow(institution_lookup) == n_distinct(institution_lookup$UNITID), "Historical institution names created duplicate institutions.")

cohort_availability <- final_graduation_data %>%
  group_by(UNITID, Institution) %>%
  summarise(
    EarliestCohortYear = min(CohortYear, na.rm = TRUE),
    LatestCohortYear = max(CohortYear, na.rm = TRUE),
    NumberOfCohorts = n_distinct(CohortYear),
    CohortsWith100PercentData = sum(!is.na(GraduationRate100)),
    CohortsWith150PercentData = sum(!is.na(GraduationRate150)),
    CohortsWith200PercentData = sum(!is.na(GraduationRate200)),
    .groups = "drop"
  ) %>%
  arrange(match(UNITID, institution_lookup$UNITID))

missing_value_counts <- final_graduation_data %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "MissingCount")

validation_summary <- tibble(
  NumberOfInstitutions = n_distinct(final_graduation_data$UNITID),
  NumberOfPeerInstitutions = n_distinct(final_graduation_data$UNITID[!final_graduation_data$IsCaldwell]),
  NumberOfCohortYears = n_distinct(final_graduation_data$CohortYear),
  EarliestCohortYear = min(final_graduation_data$CohortYear, na.rm = TRUE),
  LatestCohortYear = max(final_graduation_data$CohortYear, na.rm = TRUE),
  TotalNumberOfRows = nrow(final_graduation_data),
  NumberOfCaldwellUniversityRows = sum(final_graduation_data$IsCaldwell),
  RowsWith100PercentRateData = sum(!is.na(final_graduation_data$GraduationRate100)),
  RowsWith150PercentRateData = sum(!is.na(final_graduation_data$GraduationRate150)),
  RowsWith200PercentRateData = sum(!is.na(final_graduation_data$GraduationRate200)),
  DuplicateCounts = duplicate_count,
  InvalidRateCounts = sum(invalid_rate_counts$InvalidCount),
  FailedValidationChecks = if_else(length(validation_failures) == 0L, "None", str_c(validation_failures, collapse = " | "))
)

cat("\nCohort availability table:\n")
print(cohort_availability, n = Inf)

cat("\nValidation summary:\n")
print(validation_summary)

cat("\nMissing-value counts:\n")
print(missing_value_counts, n = Inf)

cat("\nInvalid-rate counts:\n")
print(invalid_rate_counts, n = Inf)

if (length(validation_failures) > 0L) {
  stop("Validation failed: ", str_c(validation_failures, collapse = " | "))
}

readr::write_csv(final_graduation_data, out_path)

cat("\nFirst 30 rows:\n")
print(head(final_graduation_data, 30), n = 30)

cat("\nColumn names:\n")
print(names(final_graduation_data))

cat("\nData types:\n")
print(vapply(final_graduation_data, function(x) paste(class(x), collapse = "/"), character(1)))

cat("\nFinal institution list:\n")
print(
  final_graduation_data %>%
    distinct(DisplayOrder, UNITID, Institution, City, State, PeerGroup, IsCaldwell) %>%
    arrange(DisplayOrder),
  n = Inf
)

cat("\nFinal cohort-year list:\n")
print(sort(unique(final_graduation_data$CohortYear)))

cat("\nFile location:\n")
cat(normalizePath(out_path, mustWork = FALSE), "\n")

