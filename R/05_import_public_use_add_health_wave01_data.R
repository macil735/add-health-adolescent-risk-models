# ============================================================
# Project: add-health-adolescent-risk-models
# Script 05: Import Public-Use Add Health Wave I Data
# Author: Gelo Picol
#
# Purpose:
#   Import only confirmed Add Health Wave I public-use variables
#   from the local Wave I SPSS file:
#
#       data/raw/21600-0001-Data.sav
#
# Important:
#   - This script reads only Wave I.
#   - It imports only variables confirmed in Script 04.
#   - It excludes non-Wave I confirmed variables from this import.
#   - It saves individual-level processed data only in data/processed/.
#   - Raw and processed microdata must not be committed to GitHub.
#   - Public outputs are limited to aggregate diagnostics.
# ============================================================


# ============================================================
# 0. Project root and display options
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"

options(na.print = "NA")


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "haven",
  "dplyr",
  "tibble",
  "readr",
  "stringr",
  "tidyr",
  "purrr",
  "openxlsx"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(haven)
library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(openxlsx)


# ============================================================
# 2. Define paths
# ============================================================

data_raw_dir       <- file.path(project_root, "data/raw")
data_processed_dir <- file.path(project_root, "data/processed")
outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir   <- file.path(project_root, "outputs/diagnostics")
docs_dir           <- file.path(project_root, "docs")

dir.create(data_raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

wave_id <- "Wave I"

sav_path <- file.path(
  data_raw_dir,
  "21600-0001-Data.sav"
)

confirmed_dictionary_path <- file.path(
  outputs_tables_dir,
  "confirmed_variable_dictionary_script04.csv"
)

processed_rds_path <- file.path(
  data_processed_dir,
  "add_health_wave01_confirmed_variables_local_only.rds"
)


# ============================================================
# 3. Check required inputs
# ============================================================

required_inputs <- c(
  sav_path,
  confirmed_dictionary_path
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    paste0(
      "Missing required input files:\n",
      paste(missing_inputs, collapse = "\n")
    )
  )
}


# ============================================================
# 4. Helper functions
# ============================================================

clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  stringr::str_trim(x)
}

is_wave_i <- function(x) {
  x_clean <- stringr::str_to_lower(clean_chr(x))

  stringr::str_detect(x_clean, "\\bwave\\s*i\\b") |
    stringr::str_detect(x_clean, "\\bwave\\s*1\\b") |
    stringr::str_detect(x_clean, "\\bwave\\s*01\\b")
}

parse_variable_names <- function(x) {
  x <- clean_chr(x)

  if (x == "") {
    return(character())
  }

  x <- stringr::str_replace_all(x, "\\band\\b", ";")
  x <- stringr::str_replace_all(x, "\\+", ";")
  x <- stringr::str_replace_all(x, "/", ";")

  vars <- stringr::str_split(x, "[;,|]+")[[1]]
  vars <- stringr::str_trim(vars)
  vars <- vars[vars != ""]
  vars <- vars[
    !stringr::str_detect(
      stringr::str_to_lower(vars),
      "to_be|pending|verify|confirmed_from"
    )
  ]

  unique(vars)
}

get_variable_label <- function(x) {
  lab <- attr(x, "label")

  if (is.null(lab)) {
    return(NA_character_)
  }

  as.character(lab)
}

to_numeric_clean <- function(x) {
  suppressWarnings(as.numeric(haven::zap_labels(x)))
}

count_true <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_integer_)
  }

  sum(x %in% TRUE, na.rm = TRUE)
}

count_nonmissing <- function(x) {
  if (length(x) == 0) {
    return(NA_integer_)
  }

  sum(!is.na(x))
}

safe_count_public <- function(n, threshold = 10) {
  if (is.na(n)) {
    return(NA_character_)
  }

  if (n < threshold) {
    return(paste0("<", threshold))
  }

  as.character(n)
}

detect_sensitivity_group <- function(block, label) {
  txt <- stringr::str_to_lower(paste(block, label, collapse = " "))

  dplyr::case_when(
    stringr::str_detect(txt, "pregnancy|pregnant") ~ "sensitive_reproductive_health",
    stringr::str_detect(txt, "hiv|sti|std|sexually transmitted") ~ "sensitive_hiv_sti",
    stringr::str_detect(txt, "sex|sexual|condom|contraceptive|partner") ~ "sensitive_sexual_behaviour",
    stringr::str_detect(txt, "school|family|peer|parent") ~ "contextual_predictor",
    TRUE ~ "general_covariate"
  )
}


# ============================================================
# 5. Load confirmed dictionary and restrict to Wave I
# ============================================================

confirmed_dictionary_all_waves <- readr::read_csv(
  confirmed_dictionary_path,
  show_col_types = FALSE
)

if (!"add_health_variable_name" %in% names(confirmed_dictionary_all_waves)) {
  stop("Column add_health_variable_name not found in confirmed dictionary.")
}

if (!"add_health_wave" %in% names(confirmed_dictionary_all_waves)) {
  stop("Column add_health_wave not found in confirmed dictionary.")
}

confirmed_dictionary_all_waves <- confirmed_dictionary_all_waves %>%
  mutate(
    dictionary_row_id = row_number(),
    is_wave_i_row = is_wave_i(add_health_wave)
  )

confirmed_dictionary <- confirmed_dictionary_all_waves %>%
  filter(is_wave_i_row)

excluded_non_wave1_variables <- confirmed_dictionary_all_waves %>%
  filter(!is_wave_i_row)

if (nrow(confirmed_dictionary) == 0) {
  stop("No confirmed Wave I variables were found in the confirmed dictionary.")
}


# ============================================================
# 6. Parse confirmed Wave I variable names
# ============================================================

expanded_confirmed_variables <- confirmed_dictionary %>%
  mutate(
    parsed_variable_names = purrr::map(add_health_variable_name, parse_variable_names)
  ) %>%
  tidyr::unnest(parsed_variable_names) %>%
  rename(
    requested_variable_name = parsed_variable_names
  ) %>%
  mutate(
    requested_variable_name = stringr::str_trim(requested_variable_name),
    requested_variable_name_upper = stringr::str_to_upper(requested_variable_name)
  ) %>%
  filter(
    requested_variable_name != ""
  ) %>%
  distinct(
    requested_variable_name_upper,
    requested_variable_name,
    .keep_all = TRUE
  )

if (nrow(expanded_confirmed_variables) == 0) {
  stop("No confirmed Wave I variable names were parsed from the confirmed dictionary.")
}


# ============================================================
# 7. Read Add Health Wave I .sav file
# ============================================================

raw_data <- haven::read_sav(
  sav_path,
  user_na = TRUE
)

raw_data <- haven::zap_missing(raw_data)

sav_variable_map <- tibble(
  sav_variable_name = names(raw_data),
  sav_variable_name_upper = stringr::str_to_upper(names(raw_data))
)


# ============================================================
# 8. Match confirmed Wave I variables to .sav variables
# ============================================================

import_variable_check <- expanded_confirmed_variables %>%
  left_join(
    sav_variable_map,
    by = c("requested_variable_name_upper" = "sav_variable_name_upper")
  ) %>%
  mutate(
    available_in_sav = !is.na(sav_variable_name),
    import_decision = ifelse(
      available_in_sav,
      "import",
      "not_found_in_wave01_sav"
    )
  ) %>%
  select(
    dictionary_row_id,
    priority_id,
    thesis_construct_block,
    thesis_item_codes,
    theoretical_model,
    add_health_target_domain,
    expected_analytic_use,
    analysis_role,
    variable_level,
    equivalence_strategy,
    expected_wave_priority,
    add_health_wave,
    requested_variable_name,
    sav_variable_name,
    available_in_sav,
    import_decision,
    add_health_variable_label,
    public_use_status,
    mapping_quality,
    final_decision,
    import_priority,
    reviewer_notes
  )

variables_to_import <- import_variable_check %>%
  filter(available_in_sav) %>%
  pull(sav_variable_name) %>%
  unique()

missing_requested_variables <- import_variable_check %>%
  filter(!available_in_sav)

if (length(variables_to_import) == 0) {
  stop("None of the confirmed Wave I variables were found in 21600-0001-Data.sav.")
}


# ============================================================
# 9. Import only confirmed Wave I variables
# ============================================================

imported_data <- raw_data %>%
  select(all_of(variables_to_import))

rm(raw_data)


# ============================================================
# 10. Derive Wave I sample alignment variables
# ============================================================

available_names_upper <- stringr::str_to_upper(names(imported_data))

find_var <- function(candidates) {
  candidates_upper <- stringr::str_to_upper(candidates)
  hit <- candidates_upper[candidates_upper %in% available_names_upper]

  if (length(hit) == 0) {
    return(NA_character_)
  }

  names(imported_data)[match(hit[1], available_names_upper)]
}

grade_var <- find_var(c("H1GI20", "GRADE"))
sex_var   <- find_var(c("BIO_SEX"))

birth_year_var      <- find_var(c("H1GI1Y"))
birth_month_var     <- find_var(c("H1GI1M"))
interview_year_var  <- find_var(c("IYEAR"))
interview_month_var <- find_var(c("IMONTH"))

age_derivation_possible <- all(
  !is.na(c(
    birth_year_var,
    birth_month_var,
    interview_year_var,
    interview_month_var
  ))
)

if (!is.na(grade_var)) {
  grade_numeric <- to_numeric_clean(imported_data[[grade_var]])

  imported_data <- imported_data %>%
    mutate(
      derived_grade_wave1 = grade_numeric,
      derived_grade_10_12_flag = derived_grade_wave1 %in% 10:12
    )
} else {
  imported_data <- imported_data %>%
    mutate(
      derived_grade_wave1 = NA_real_,
      derived_grade_10_12_flag = NA
    )
}

if (age_derivation_possible) {
  birth_year <- to_numeric_clean(imported_data[[birth_year_var]])
  birth_month <- to_numeric_clean(imported_data[[birth_month_var]])
  interview_year <- to_numeric_clean(imported_data[[interview_year_var]])
  interview_month <- to_numeric_clean(imported_data[[interview_month_var]])

  birth_year <- ifelse(!is.na(birth_year) & birth_year < 100, birth_year + 1900, birth_year)
  interview_year <- ifelse(!is.na(interview_year) & interview_year < 100, interview_year + 1900, interview_year)

  birth_month <- ifelse(birth_month %in% 1:12, birth_month, NA_real_)
  interview_month <- ifelse(interview_month %in% 1:12, interview_month, NA_real_)

  derived_age_wave1 <- interview_year - birth_year -
    ifelse(
      !is.na(interview_month) &
        !is.na(birth_month) &
        interview_month < birth_month,
      1,
      0
    )

  derived_age_wave1 <- ifelse(
    derived_age_wave1 >= 0 & derived_age_wave1 <= 120,
    derived_age_wave1,
    NA_real_
  )

  imported_data <- imported_data %>%
    mutate(
      derived_age_wave1 = derived_age_wave1,
      derived_age_15_19_flag = derived_age_wave1 %in% 15:19
    )
} else {
  imported_data <- imported_data %>%
    mutate(
      derived_age_wave1 = NA_real_,
      derived_age_15_19_flag = NA
    )
}

imported_data <- imported_data %>%
  mutate(
    derived_main_sample_flag = derived_grade_10_12_flag,
    derived_strict_sample_flag = derived_grade_10_12_flag & derived_age_15_19_flag
  )


# ============================================================
# 11. Save processed local-only Wave I microdata
# ============================================================

saveRDS(
  imported_data,
  processed_rds_path
)


# ============================================================
# 12. Variable metadata and missingness summary
# ============================================================

variable_construct_lookup <- import_variable_check %>%
  filter(available_in_sav) %>%
  group_by(sav_variable_name) %>%
  summarise(
    thesis_construct_block = paste(sort(unique(thesis_construct_block)), collapse = "; "),
    theoretical_model = paste(sort(unique(theoretical_model)), collapse = "; "),
    analysis_role = paste(sort(unique(analysis_role)), collapse = "; "),
    variable_level = paste(sort(unique(variable_level)), collapse = "; "),
    add_health_target_domain = paste(sort(unique(add_health_target_domain)), collapse = "; "),
    mapping_quality = paste(sort(unique(mapping_quality)), collapse = "; "),
    import_priority = paste(sort(unique(import_priority)), collapse = "; "),
    .groups = "drop"
  )

variable_metadata <- tibble(
  sav_variable_name = names(imported_data),
  variable_label_from_sav = sapply(imported_data, get_variable_label),
  r_variable_class = sapply(imported_data, function(x) paste(class(x), collapse = "; ")),
  n_unique_values = sapply(imported_data, function(x) dplyr::n_distinct(x, na.rm = FALSE))
) %>%
  left_join(
    variable_construct_lookup,
    by = "sav_variable_name"
  ) %>%
  mutate(
    thesis_construct_block = replace_na(thesis_construct_block, "derived_variable"),
    theoretical_model = replace_na(theoretical_model, "derived_project_variable"),
    analysis_role = replace_na(analysis_role, "sample_filter"),
    variable_level = replace_na(variable_level, "derived_sample_alignment"),
    add_health_target_domain = replace_na(add_health_target_domain, "derived_sample_alignment"),
    mapping_quality = replace_na(mapping_quality, "derived_from_confirmed_variables"),
    import_priority = replace_na(import_priority, "derived_after_import"),
    sensitivity_group = mapply(
      detect_sensitivity_group,
      thesis_construct_block,
      variable_label_from_sav
    )
  )

missingness_summary <- tibble(
  sav_variable_name = names(imported_data),
  n_observations = nrow(imported_data),
  n_missing = sapply(imported_data, function(x) sum(is.na(x))),
  n_nonmissing = sapply(imported_data, function(x) sum(!is.na(x)))
) %>%
  mutate(
    pct_missing = round(100 * n_missing / n_observations, 2),
    pct_nonmissing = round(100 * n_nonmissing / n_observations, 2)
  ) %>%
  left_join(
    variable_metadata,
    by = "sav_variable_name"
  ) %>%
  select(
    sav_variable_name,
    variable_label_from_sav,
    thesis_construct_block,
    theoretical_model,
    analysis_role,
    variable_level,
    sensitivity_group,
    n_observations,
    n_missing,
    n_nonmissing,
    pct_missing,
    pct_nonmissing,
    n_unique_values,
    r_variable_class,
    mapping_quality,
    import_priority
  )


# ============================================================
# 13. Wave I sample restriction summary
# ============================================================

n_total <- nrow(imported_data)

n_grade_nonmissing <- count_nonmissing(imported_data$derived_grade_wave1)
n_grade_10_12 <- count_true(imported_data$derived_grade_10_12_flag)

n_age_nonmissing <- count_nonmissing(imported_data$derived_age_wave1)
n_age_15_19 <- count_true(imported_data$derived_age_15_19_flag)

n_main_sample <- count_true(imported_data$derived_main_sample_flag)
n_strict_sample <- count_true(imported_data$derived_strict_sample_flag)

sample_restriction_summary <- tibble(
  summary_id = 1:14,
  item = c(
    "Raw Wave I public-use file detected",
    "Confirmed dictionary rows, all waves",
    "Confirmed Wave I rows retained",
    "Confirmed non-Wave I rows excluded",
    "Parsed confirmed Wave I variables",
    "Wave I variables found in .sav",
    "Wave I variables not found in .sav",
    "Observations in imported Wave I file",
    "Grade variable used",
    "Nonmissing grade observations",
    "Grade 10-12 observations",
    "Age derivation possible",
    "Age 15-19 observations",
    "Strict grade 10-12 and age 15-19 observations"
  ),
  value = c(
    file.exists(sav_path),
    nrow(confirmed_dictionary_all_waves),
    nrow(confirmed_dictionary),
    nrow(excluded_non_wave1_variables),
    nrow(expanded_confirmed_variables),
    length(variables_to_import),
    nrow(missing_requested_variables),
    n_total,
    ifelse(is.na(grade_var), "not_found", grade_var),
    n_grade_nonmissing,
    n_grade_10_12,
    age_derivation_possible,
    n_age_15_19,
    n_strict_sample
  ),
  public_value = c(
    as.character(file.exists(sav_path)),
    as.character(nrow(confirmed_dictionary_all_waves)),
    as.character(nrow(confirmed_dictionary)),
    as.character(nrow(excluded_non_wave1_variables)),
    as.character(nrow(expanded_confirmed_variables)),
    as.character(length(variables_to_import)),
    as.character(nrow(missing_requested_variables)),
    safe_count_public(n_total),
    ifelse(is.na(grade_var), "not_found", grade_var),
    safe_count_public(n_grade_nonmissing),
    safe_count_public(n_grade_10_12),
    as.character(age_derivation_possible),
    safe_count_public(n_age_15_19),
    safe_count_public(n_strict_sample)
  ),
  note = c(
    "The .sav file remains local in data/raw/.",
    "Rows in confirmed_variable_dictionary_script04.csv before Wave I filtering.",
    "Only Wave I rows are used in Script 05.",
    "Excluded from Script 05 because this script reads only Wave I.",
    "Variable names parsed from retained Wave I dictionary rows.",
    "Only these variables were imported from the Wave I .sav file.",
    "These should be checked before modeling.",
    "Aggregate count only.",
    "Main sample is based on grade 10-12 at Wave I.",
    "Aggregate count only.",
    "Primary analytical sample criterion.",
    "Age is derived from birth and interview month/year.",
    "Complementary sample alignment criterion.",
    "Sensitivity sample combining grade and age criteria."
  )
)


# ============================================================
# 14. Import summaries by construct and role
# ============================================================

construct_import_summary <- import_variable_check %>%
  group_by(
    priority_id,
    thesis_construct_block,
    theoretical_model,
    add_health_target_domain,
    expected_wave_priority
  ) %>%
  summarise(
    n_requested_variables = n(),
    n_available_in_sav = sum(available_in_sav, na.rm = TRUE),
    n_missing_from_sav = sum(!available_in_sav, na.rm = TRUE),
    import_status = case_when(
      n_available_in_sav == n_requested_variables ~ "all_requested_variables_imported",
      n_available_in_sav > 0 ~ "partial_import_check_missing_variables",
      TRUE ~ "no_requested_variables_imported"
    ),
    imported_variables = paste(sort(unique(na.omit(sav_variable_name))), collapse = "; "),
    missing_variables = paste(sort(unique(requested_variable_name[!available_in_sav])), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(priority_id)

variable_role_summary <- variable_metadata %>%
  count(
    analysis_role,
    variable_level,
    sensitivity_group,
    name = "n_variables"
  ) %>%
  arrange(
    analysis_role,
    variable_level,
    sensitivity_group
  )

excluded_non_wave1_summary <- excluded_non_wave1_variables %>%
  select(
    dictionary_row_id,
    priority_id,
    thesis_construct_block,
    add_health_wave,
    add_health_variable_name,
    add_health_variable_label,
    final_decision
  )


# ============================================================
# 15. Safe public output policy
# ============================================================

safe_public_output_policy_script05 <- tibble(
  policy_id = 1:9,
  policy_area = c(
    "Raw Wave I microdata",
    "Processed Wave I microdata",
    "Public outputs",
    "Sensitive variables",
    "Small cells",
    "Derived age",
    "Identifiers",
    "Non-Wave I variables",
    "GitHub"
  ),
  rule = c(
    "Raw Add Health Wave I .sav file remains in data/raw/ only.",
    "Processed individual-level Wave I file remains in data/processed/ only.",
    "Only aggregate diagnostics should be exported to outputs/tables/.",
    "Pregnancy, HIV/STI and sexual behaviour variables require careful aggregate reporting.",
    "Cells below the disclosure threshold should be suppressed in later descriptive tables.",
    "Age is derived locally and should be used only for sample alignment and sensitivity checks.",
    "Identifiers must never be exported in public tables.",
    "Variables from other waves are excluded from Script 05 and require separate wave-specific import scripts.",
    "Do not commit data/raw/, data/processed/ or outputs/private_review/."
  )
)


# ============================================================
# 16. Script 05 methodological notes
# ============================================================

script05_methodological_notes <- tibble(
  note_id = 1:11,
  note = c(
    "Script 05 imports only confirmed Add Health Wave I variables.",
    "The input microdata file is data/raw/21600-0001-Data.sav.",
    "The raw Add Health Wave I public-use .sav file remains local in data/raw/.",
    "The processed individual-level Wave I file is saved locally in data/processed/.",
    "No individual-level microdata are exported to public outputs.",
    "Public outputs contain only variable availability, missingness and sample restriction diagnostics.",
    "Confirmed non-Wave I variables are excluded from this script.",
    "The main sample criterion is grade 10-12 at Wave I.",
    "Age 15-19 is used as a complementary alignment or sensitivity criterion.",
    "Derived age is computed from birth month/year and interview month/year when available.",
    "The next script should clean, recode and document analytical variables before descriptive analysis."
  )
)


# ============================================================
# 17. Execution checklist
# ============================================================

script05_checklist <- tibble(
  check_id = 1:20,
  check_item = c(
    "Project root exists",
    "Raw Wave I .sav file exists locally",
    "Confirmed variable dictionary exists",
    "Confirmed dictionary loaded",
    "Confirmed dictionary filtered to Wave I",
    "Non-Wave I confirmed variables excluded",
    "Raw Wave I .sav file read",
    "Confirmed Wave I variable names parsed",
    "Wave I variable availability checked",
    "At least one confirmed Wave I variable found in .sav",
    "Confirmed Wave I variables imported",
    "Grade variable checked",
    "Age derivation variables checked",
    "Sample flags created",
    "Processed local-only Wave I RDS saved",
    "Import variable check exported",
    "Missingness summary exported",
    "Sample restriction summary exported",
    "Excel diagnostic workbook exported",
    "Markdown documentation exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(sav_path), "OK", "FAIL"),
    ifelse(file.exists(confirmed_dictionary_path), "OK", "FAIL"),
    "OK",
    ifelse(nrow(confirmed_dictionary) > 0, "OK", "FAIL"),
    "OK",
    "OK",
    ifelse(nrow(expanded_confirmed_variables) > 0, "OK", "FAIL"),
    "OK",
    ifelse(length(variables_to_import) > 0, "OK", "FAIL"),
    "OK",
    ifelse(is.na(grade_var), "WARNING_GRADE_VARIABLE_NOT_FOUND", "OK"),
    ifelse(age_derivation_possible, "OK", "WARNING_AGE_DERIVATION_NOT_AVAILABLE"),
    "OK",
    ifelse(file.exists(processed_rds_path), "OK", "FAIL"),
    "PENDING",
    "PENDING",
    "PENDING",
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 18. Export public CSV outputs
# ============================================================

readr::write_csv(
  import_variable_check,
  file.path(outputs_tables_dir, "script05_wave01_import_variable_check.csv")
)

readr::write_csv(
  construct_import_summary,
  file.path(outputs_tables_dir, "script05_wave01_construct_import_summary.csv")
)

readr::write_csv(
  missingness_summary,
  file.path(outputs_tables_dir, "script05_wave01_missingness_summary.csv")
)

readr::write_csv(
  sample_restriction_summary,
  file.path(outputs_tables_dir, "script05_wave01_sample_restriction_summary.csv")
)

readr::write_csv(
  variable_metadata,
  file.path(outputs_tables_dir, "script05_wave01_variable_metadata.csv")
)

readr::write_csv(
  variable_role_summary,
  file.path(outputs_tables_dir, "script05_wave01_variable_role_summary.csv")
)

readr::write_csv(
  excluded_non_wave1_summary,
  file.path(outputs_tables_dir, "script05_excluded_non_wave01_confirmed_variables.csv")
)

readr::write_csv(
  safe_public_output_policy_script05,
  file.path(outputs_tables_dir, "script05_wave01_safe_public_output_policy.csv")
)

readr::write_csv(
  script05_methodological_notes,
  file.path(outputs_tables_dir, "script05_wave01_methodological_notes.csv")
)

script05_checklist$status[
  script05_checklist$check_item %in% c(
    "Import variable check exported",
    "Missingness summary exported",
    "Sample restriction summary exported"
  )
] <- "OK"


# ============================================================
# 19. Export public diagnostic Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script05_wave01_import_public_use_add_health_diagnostics.xlsx"
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "import_variable_check")
openxlsx::writeData(wb, "import_variable_check", import_variable_check)

openxlsx::addWorksheet(wb, "construct_import_summary")
openxlsx::writeData(wb, "construct_import_summary", construct_import_summary)

openxlsx::addWorksheet(wb, "missingness_summary")
openxlsx::writeData(wb, "missingness_summary", missingness_summary)

openxlsx::addWorksheet(wb, "sample_restriction")
openxlsx::writeData(wb, "sample_restriction", sample_restriction_summary)

openxlsx::addWorksheet(wb, "variable_metadata")
openxlsx::writeData(wb, "variable_metadata", variable_metadata)

openxlsx::addWorksheet(wb, "variable_role_summary")
openxlsx::writeData(wb, "variable_role_summary", variable_role_summary)

openxlsx::addWorksheet(wb, "excluded_non_wave01")
openxlsx::writeData(wb, "excluded_non_wave01", excluded_non_wave1_summary)

openxlsx::addWorksheet(wb, "safe_policy")
openxlsx::writeData(wb, "safe_policy", safe_public_output_policy_script05)

openxlsx::addWorksheet(wb, "methodological_notes")
openxlsx::writeData(wb, "methodological_notes", script05_methodological_notes)

openxlsx::addWorksheet(wb, "script05_checklist")
openxlsx::writeData(wb, "script05_checklist", script05_checklist)

for (sheet in names(wb)) {
  openxlsx::setColWidths(wb, sheet = sheet, cols = 1:60, widths = "auto")
  openxlsx::freezePane(wb, sheet = sheet, firstRow = TRUE)
}

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script05_checklist$status[
  script05_checklist$check_item == "Excel diagnostic workbook exported"
] <- "OK"


# ============================================================
# 20. Export Markdown documentation
# ============================================================

script05_doc <- c(
  "# Import Public-Use Add Health Wave I Data",
  "",
  "Script 05 imports only confirmed public-use Add Health Wave I variables.",
  "",
  "## Input file",
  "",
  "`data/raw/21600-0001-Data.sav`",
  "",
  "This file is treated as the Add Health Wave I public-use data file.",
  "",
  "## Wave restriction",
  "",
  "The confirmed variable dictionary is filtered to Wave I only before import.",
  "",
  "Confirmed variables from other waves are excluded from this script and should be handled in separate wave-specific import scripts if those data files are available.",
  "",
  "## Microdata policy",
  "",
  "Raw microdata remain local in `data/raw/`.",
  "",
  "Processed individual-level data are saved locally in `data/processed/`.",
  "",
  "Neither raw nor processed microdata should be committed to GitHub.",
  "",
  "## Processed local-only file",
  "",
  "`data/processed/add_health_wave01_confirmed_variables_local_only.rds`",
  "",
  "## Public outputs",
  "",
  "- Wave I variable availability diagnostics;",
  "- construct import summary;",
  "- missingness summary;",
  "- sample restriction summary;",
  "- safe public output policy.",
  "",
  "## Sample alignment",
  "",
  "The main analytical sample is restricted to students in grades 10 to 12 at Wave I.",
  "",
  "Age 15 to 19 is retained as a complementary alignment and sensitivity criterion.",
  "",
  "## Next step",
  "",
  "Script 06 should clean, recode and document the imported Wave I analytical variables before descriptive analysis."
)

writeLines(
  script05_doc,
  con = file.path(docs_dir, "import_public_use_add_health_wave01_data_script05.md")
)

script05_checklist$status[
  script05_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 21. Save final checklist
# ============================================================

readr::write_csv(
  script05_checklist,
  file.path(outputs_diag_dir, "script05_execution_checklist.csv")
)


# ============================================================
# 22. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 05 completed: Import Public-Use Add Health Wave I Data\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Input files:\n")
cat("- Raw Wave I .sav: ", sav_path, "\n", sep = "")
cat("- Confirmed dictionary: ", confirmed_dictionary_path, "\n\n", sep = "")

cat("Wave filtering:\n")
cat("- Confirmed dictionary rows, all waves: ", nrow(confirmed_dictionary_all_waves), "\n", sep = "")
cat("- Confirmed Wave I rows retained: ", nrow(confirmed_dictionary), "\n", sep = "")
cat("- Confirmed non-Wave I rows excluded: ", nrow(excluded_non_wave1_variables), "\n\n", sep = "")

cat("Import summary:\n")
cat("- Parsed confirmed Wave I variables: ", nrow(expanded_confirmed_variables), "\n", sep = "")
cat("- Variables found in Wave I .sav: ", length(variables_to_import), "\n", sep = "")
cat("- Variables not found in Wave I .sav: ", nrow(missing_requested_variables), "\n", sep = "")
cat("- Observations imported: ", nrow(imported_data), "\n", sep = "")
cat("- Variables in processed local file, including derived flags: ", ncol(imported_data), "\n\n", sep = "")

cat("Sample alignment:\n")
cat("- Grade variable used: ", ifelse(is.na(grade_var), "not_found", grade_var), "\n", sep = "")
cat("- Grade 10-12 observations: ", n_grade_10_12, "\n", sep = "")
cat("- Age derivation possible: ", age_derivation_possible, "\n", sep = "")
cat("- Age 15-19 observations: ", n_age_15_19, "\n", sep = "")
cat("- Strict grade 10-12 and age 15-19 observations: ", n_strict_sample, "\n\n", sep = "")

cat("Local-only processed file created:\n")
cat(processed_rds_path, "\n\n")

cat("Public outputs created:\n")
cat("- outputs/tables/script05_wave01_import_variable_check.csv\n")
cat("- outputs/tables/script05_wave01_construct_import_summary.csv\n")
cat("- outputs/tables/script05_wave01_missingness_summary.csv\n")
cat("- outputs/tables/script05_wave01_sample_restriction_summary.csv\n")
cat("- outputs/tables/script05_wave01_variable_metadata.csv\n")
cat("- outputs/tables/script05_wave01_variable_role_summary.csv\n")
cat("- outputs/tables/script05_excluded_non_wave01_confirmed_variables.csv\n")
cat("- outputs/tables/script05_wave01_safe_public_output_policy.csv\n")
cat("- outputs/tables/script05_wave01_methodological_notes.csv\n")
cat("- outputs/tables/script05_wave01_import_public_use_add_health_diagnostics.xlsx\n")
cat("- outputs/diagnostics/script05_execution_checklist.csv\n")
cat("- docs/import_public_use_add_health_wave01_data_script05.md\n\n")

cat("Variable import status summary:\n")
print(
  import_variable_check %>%
    count(import_decision, name = "n_variables")
)

cat("\nConstruct import summary:\n")
print(
  construct_import_summary %>%
    count(import_status, name = "n_constructs")
)

cat("\nExecution checklist:\n")
print(script05_checklist)

cat("\nImportant note:\n")
cat("Do not commit data/raw/, data/processed/ or outputs/private_review/ to GitHub.\n\n")