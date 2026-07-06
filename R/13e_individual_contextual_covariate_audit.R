# ============================================================
# Script 13e — Individual and Contextual Covariate Audit
# Project: Add Health Adolescent Risk Models
# Purpose:
#   Audit, recover, document and prepare individual/contextual
#   covariates for subsequent protection/risk index models.
#
# Main covariates:
#   - gender / sex
#   - age
#   - school grade
#   - residence context: H1IR12
#   - survey weight
#
# Author: Project pipeline
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "purrr",
  "stringr",
  "readr",
  "tidyr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script."
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(readr)
  library(tidyr)
})

has_officer <- requireNamespace("officer", quietly = TRUE)
has_flextable <- requireNamespace("flextable", quietly = TRUE)

if (has_officer) {
  suppressPackageStartupMessages(library(officer))
}
if (has_flextable) {
  suppressPackageStartupMessages(library(flextable))
}

# ------------------------------------------------------------
# 1. Project root and output folders
# ------------------------------------------------------------

project_root <- "D:/GitHub/add-health-adolescent-risk-models"

if (!dir.exists(project_root)) {
  stop("Project root not found: ", project_root)
}

setwd(project_root)

output_dir <- file.path(project_root, "outputs", "audits")
doc_dir    <- file.path(project_root, "docs")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 13e started: Individual and Contextual Covariate Audit\n")
cat("============================================================\n\n")
cat("Project root:\n", project_root, "\n\n")

# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

is_data_frame_like <- function(x) {
  inherits(x, c("data.frame", "tbl_df", "tbl"))
}

safe_n_distinct <- function(x) {
  tryCatch(dplyr::n_distinct(x, na.rm = TRUE), error = function(e) NA_integer_)
}

safe_non_missing <- function(x) {
  tryCatch(sum(!is.na(x)), error = function(e) NA_integer_)
}

safe_missing <- function(x) {
  tryCatch(sum(is.na(x)), error = function(e) NA_integer_)
}

safe_min <- function(x) {
  tryCatch({
    if (!is.numeric(x)) return(NA_real_)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    suppressWarnings(min(x))
  }, error = function(e) NA_real_)
}

safe_max <- function(x) {
  tryCatch({
    if (!is.numeric(x)) return(NA_real_)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    suppressWarnings(max(x))
  }, error = function(e) NA_real_)
}

safe_mean <- function(x) {
  tryCatch({
    if (!is.numeric(x)) return(NA_real_)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    suppressWarnings(mean(x))
  }, error = function(e) NA_real_)
}

safe_sd <- function(x) {
  tryCatch({
    if (!is.numeric(x)) return(NA_real_)
    x <- x[!is.na(x)]
    if (length(x) <= 1) return(NA_real_)
    suppressWarnings(stats::sd(x))
  }, error = function(e) NA_real_)
}
clean_chr <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x
}

as_numeric_safely <- function(x) {

  if (is.null(x)) return(NA_real_)

  if (inherits(x, "haven_labelled") || !is.null(attr(x, "labels"))) {
    return(suppressWarnings(as.numeric(unclass(x))))
  }

  if (is.numeric(x) || is.integer(x)) {
    return(as.numeric(x))
  }

  if (is.logical(x)) {
    return(as.numeric(x))
  }

  if (is.factor(x)) {
    x_chr <- as.character(x)
  } else {
    x_chr <- as.character(x)
  }

  x_chr <- stringr::str_squish(x_chr)

  out <- suppressWarnings(as.numeric(x_chr))

  if (all(is.na(out)) && any(!is.na(x_chr))) {
    extracted <- stringr::str_extract(x_chr, "^[0-9]+")
    out <- suppressWarnings(as.numeric(extracted))
  }

  out
}
weighted_mean_safe <- function(x, w) {
  x <- as_numeric_safely(x)
  w <- as_numeric_safely(w)
  valid <- !is.na(x) & !is.na(w) & w > 0
  if (sum(valid) == 0) return(NA_real_)
  sum(x[valid] * w[valid]) / sum(w[valid])
}

weighted_n_safe <- function(w) {
  w <- as_numeric_safely(w)
  valid <- !is.na(w) & w > 0
  if (sum(valid) == 0) return(NA_real_)
  sum(w[valid])
}

load_rda_data_frames <- function(path) {
  env <- new.env(parent = emptyenv())
  loaded_objects <- tryCatch(load(path, envir = env), error = function(e) character(0))

  if (length(loaded_objects) == 0) {
    return(tibble())
  }

  out <- purrr::map_dfr(loaded_objects, function(obj_name) {
    obj <- get(obj_name, envir = env)

    if (!is_data_frame_like(obj)) {
      return(tibble())
    }

    tibble(
      source_file = path,
      object_name = obj_name,
      n_rows = nrow(obj),
      n_cols = ncol(obj),
      columns = list(names(obj)),
      data = list(as_tibble(obj))
    )
  })

  out
}

load_rds_data_frame <- function(path) {
  obj <- tryCatch(readRDS(path), error = function(e) NULL)

  if (!is_data_frame_like(obj)) {
    return(tibble())
  }

  tibble(
    source_file = path,
    object_name = tools::file_path_sans_ext(basename(path)),
    n_rows = nrow(obj),
    n_cols = ncol(obj),
    columns = list(names(obj)),
    data = list(as_tibble(obj))
  )
}

# ------------------------------------------------------------
# 3. Locate candidate data files
# ------------------------------------------------------------

all_files <- list.files(
  file.path(project_root, "data"),
  recursive = TRUE,
  full.names = TRUE
)

all_files <- all_files[
  !stringr::str_detect(all_files, fixed(".git")) &
    !stringr::str_detect(all_files, fixed("/outputs/")) &
    !stringr::str_detect(all_files, fixed("\\outputs\\"))
]
data_files <- all_files[
  stringr::str_detect(
    tolower(all_files),
    "\\.(rda|rdata|rds)$"
  )
]

if (length(data_files) == 0) {
  stop("No .rda, .RData or .rds data files found in the project.")
}

data_file_inventory <- tibble(
  source_file = data_files,
  file_name = basename(data_files),
  extension = tolower(tools::file_ext(data_files)),
  size_mb = round(file.info(data_files)$size / 1024^2, 3)
) %>%
  arrange(desc(size_mb))

readr::write_csv(
  data_file_inventory,
  file.path(output_dir, "script13e_data_file_inventory.csv")
)

cat("Data files detected:\n")
print(data_file_inventory)

# ------------------------------------------------------------
# 4. Load available data frames
# ------------------------------------------------------------

rda_files <- data_files[
  stringr::str_detect(tolower(data_files), "\\.(rda|rdata)$")
]

rds_files <- data_files[
  stringr::str_detect(tolower(data_files), "\\.rds$")
]

loaded_rda <- purrr::map_dfr(rda_files, load_rda_data_frames)
loaded_rds <- purrr::map_dfr(rds_files, load_rds_data_frame)

data_objects <- bind_rows(loaded_rda, loaded_rds)

if (nrow(data_objects) == 0) {
  stop("No data frame objects were recovered from the detected data files.")
}

data_object_inventory <- data_objects %>%
  transmute(
    source_file,
    file_name = basename(source_file),
    object_name,
    n_rows,
    n_cols
  ) %>%
  arrange(desc(n_rows), desc(n_cols))

readr::write_csv(
  data_object_inventory,
  file.path(output_dir, "script13e_data_object_inventory.csv")
)

cat("\nData frame objects recovered:\n")
print(data_object_inventory)

# ------------------------------------------------------------
# 5. Variable discovery
# ------------------------------------------------------------

variable_inventory <- data_objects %>%
  select(source_file, object_name, n_rows, n_cols, columns) %>%
  tidyr::unnest_longer(columns, values_to = "variable") %>%
  mutate(
    variable_upper = toupper(variable),
    file_name = basename(source_file)
  ) %>%
  select(file_name, source_file, object_name, n_rows, n_cols, variable, variable_upper)

# Priority dictionary.
# Exact known candidates are preferred. Broad patterns are used only
# for discovery and documentation.
covariate_dictionary <- tibble::tribble(
  ~covariate_domain,     ~priority, ~candidate_name, ~match_type,
  "sex_gender",               1,    "BIO_SEX",       "exact",
  "sex_gender",               2,    "SEX",           "contains",
  "sex_gender",               3,    "GENDER",        "contains",

  "age",                      1,    "AGE",           "exact",
  "age",                      2,    "H1AGE",         "exact",
  "age",                      3,    "AGE",           "contains",

  "school_grade",             1,    "GRADE",         "exact",
  "school_grade",             2,    "SCHOOL_GRADE",  "contains",
  "school_grade",             3,    "GRADE",         "contains",

  "residence_context",        1,    "H1IR12",        "exact",

  "survey_weight",            1,    "GSWGT1",        "exact",
  "survey_weight",            2,    "WEIGHT",        "contains",
  "survey_weight",            3,    "WGT",           "contains"
)

match_dictionary <- function(variable_inventory, dictionary) {
  purrr::pmap_dfr(dictionary, function(covariate_domain, priority, candidate_name, match_type) {

    if (match_type == "exact") {
      hits <- variable_inventory %>%
        filter(variable_upper == toupper(candidate_name))
    } else {
      hits <- variable_inventory %>%
        filter(stringr::str_detect(variable_upper, fixed(toupper(candidate_name))))
    }

    if (nrow(hits) == 0) {
      return(tibble())
    }

    hits %>%
      mutate(
        covariate_domain = covariate_domain,
        priority = priority,
        candidate_name = candidate_name,
        match_type = match_type
      )
  })
}

covariate_candidates <- match_dictionary(variable_inventory, covariate_dictionary) %>%
  arrange(covariate_domain, priority, desc(n_rows), desc(n_cols), variable)

readr::write_csv(
  covariate_candidates,
  file.path(output_dir, "script13e_covariate_candidates.csv")
)

cat("\nCovariate candidates detected:\n")
print(
  covariate_candidates %>%
    select(covariate_domain, variable, object_name, file_name, n_rows, n_cols, priority) %>%
    distinct()
)

# ------------------------------------------------------------
# 6. Resolve preferred variables by domain
# ------------------------------------------------------------

resolved_covariates <- covariate_candidates %>%
  group_by(covariate_domain) %>%
  arrange(priority, desc(n_rows), desc(n_cols), variable, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    covariate_domain,
    resolved_variable = variable,
    object_name,
    file_name,
    source_file,
    n_rows,
    n_cols,
    priority,
    resolution_status = "resolved"
  )

expected_domains <- tibble(
  covariate_domain = c(
    "sex_gender",
    "age",
    "school_grade",
    "residence_context",
    "survey_weight"
  )
)

resolved_covariates <- expected_domains %>%
  left_join(resolved_covariates, by = "covariate_domain") %>%
  mutate(
    resolution_status = if_else(
      is.na(resolved_variable),
      "not_resolved",
      resolution_status
    )
  )

readr::write_csv(
  resolved_covariates,
  file.path(output_dir, "script13e_resolved_covariates.csv")
)

cat("\nResolved covariates:\n")
print(resolved_covariates)

# ------------------------------------------------------------
# 7. Select primary analysis data object
# ------------------------------------------------------------

resolved_non_missing <- resolved_covariates %>%
  filter(resolution_status == "resolved")

if (nrow(resolved_non_missing) == 0) {
  stop("No covariates could be resolved. Review the candidate output.")
}

primary_object_choice <- resolved_non_missing %>%
  count(source_file, object_name, file_name, sort = TRUE) %>%
  arrange(desc(n), file_name, object_name) %>%
  slice(1)

primary_source_file <- primary_object_choice$source_file[[1]]
primary_object_name <- primary_object_choice$object_name[[1]]

primary_data <- data_objects %>%
  filter(
    source_file == primary_source_file,
    object_name == primary_object_name
  ) %>%
  pull(data) %>%
  .[[1]]

cat("\nPrimary data object selected:\n")
print(primary_object_choice)

# ------------------------------------------------------------
# 8. Pull resolved variables from primary object
# ------------------------------------------------------------

resolved_in_primary <- resolved_covariates %>%
  mutate(
    available_in_primary = !is.na(resolved_variable) &
      resolved_variable %in% names(primary_data)
  )

readr::write_csv(
  resolved_in_primary,
  file.path(output_dir, "script13e_resolved_covariates_primary_check.csv")
)

cat("\nResolved covariates available in primary object:\n")
print(resolved_in_primary)

# ------------------------------------------------------------
# 9. Build standardized analysis covariates
# ------------------------------------------------------------

get_var_or_na <- function(data, var_name) {
  if (is.na(var_name) || !var_name %in% names(data)) {
    return(rep(NA, nrow(data)))
  }
  data[[var_name]]
}

resolved_name <- function(domain) {
  out <- resolved_covariates %>%
    filter(covariate_domain == domain) %>%
    pull(resolved_variable)

  if (length(out) == 0 || is.na(out[1])) return(NA_character_)
  out[1]
}

var_sex    <- resolved_name("sex_gender")
var_age    <- resolved_name("age")
var_grade  <- resolved_name("school_grade")
var_resid  <- resolved_name("residence_context")
var_weight <- resolved_name("survey_weight")

get_resolved_vector <- function(domain, target_n) {

  resolved_row <- resolved_covariates %>%
    filter(covariate_domain == domain) %>%
    slice(1)

  if (
    nrow(resolved_row) == 0 ||
    is.na(resolved_row$resolved_variable[[1]]) ||
    resolved_row$resolution_status[[1]] != "resolved"
  ) {
    return(rep(NA, target_n))
  }

  source_file_i <- resolved_row$source_file[[1]]
  object_name_i <- resolved_row$object_name[[1]]
  variable_i    <- resolved_row$resolved_variable[[1]]

  source_object <- data_objects %>%
    filter(
      source_file == source_file_i,
      object_name == object_name_i
    )

  if (nrow(source_object) == 0) {
    warning("Source object not found for domain: ", domain)
    return(rep(NA, target_n))
  }

  source_data <- source_object$data[[1]]

  if (!variable_i %in% names(source_data)) {
    warning("Variable not found in source object for domain: ", domain)
    return(rep(NA, target_n))
  }

  out <- source_data[[variable_i]]

  if (length(out) != target_n) {
    warning(
      "Row count mismatch for domain: ", domain,
      ". Expected ", target_n,
      " rows but found ", length(out),
      ". Returning missing values for safety."
    )
    return(rep(NA, target_n))
  }

  out
}

target_n <- nrow(primary_data)

analysis_covariates <- tibble(
  row_id_script13e = seq_len(target_n),

  sex_gender_original = get_resolved_vector("sex_gender", target_n),
  age_original = get_resolved_vector("age", target_n),
  school_grade_original = get_resolved_vector("school_grade", target_n),
  H1IR12_original = get_resolved_vector("residence_context", target_n),
  survey_weight_original = get_resolved_vector("survey_weight", target_n)
) %>%
  mutate(
    sex_gender_num = as_numeric_safely(sex_gender_original),
    age_num = as_numeric_safely(age_original),
    school_grade_num = as_numeric_safely(school_grade_original),
    H1IR12_num = as_numeric_safely(H1IR12_original),
    survey_weight = as_numeric_safely(survey_weight_original),

    sex_gender_chr = stringr::str_to_lower(clean_chr(sex_gender_original)),

sex_gender_clean = case_when(
  sex_gender_num == 1 ~ "Male",
  sex_gender_num == 2 ~ "Female",
  stringr::str_detect(sex_gender_chr, "female") ~ "Female",
  stringr::str_detect(sex_gender_chr, "male") &
    !stringr::str_detect(sex_gender_chr, "female") ~ "Male",
  TRUE ~ NA_character_
),
H1IR12_chr = stringr::str_to_lower(clean_chr(H1IR12_original)),

    residence_context = case_when(
  H1IR12_num == 1 | stringr::str_detect(H1IR12_chr, "rural") ~ "Rural",
  H1IR12_num == 2 | stringr::str_detect(H1IR12_chr, "suburban") ~ "Suburban",
  H1IR12_num %in% c(3, 4, 5) ~ "Urban",
  stringr::str_detect(H1IR12_chr, "urban") &
    !stringr::str_detect(H1IR12_chr, "suburban") ~ "Urban",
  stringr::str_detect(H1IR12_chr, "commercial") ~ "Urban",
  H1IR12_num == 6 | stringr::str_detect(H1IR12_chr, "other") ~ "Other",
  H1IR12_num %in% c(96, 98, 99) ~ NA_character_,
  stringr::str_detect(H1IR12_chr, "refused|don't know|dont know|not applicable") ~ NA_character_,
  TRUE ~ NA_character_
),

    residence_context_detailed = case_when(
  H1IR12_num == 1 | stringr::str_detect(H1IR12_chr, "rural") ~ "Rural",

  H1IR12_num == 2 | stringr::str_detect(H1IR12_chr, "suburban") ~ "Suburban",

  H1IR12_num == 3 |
    stringr::str_detect(H1IR12_chr, "urban") &
    stringr::str_detect(H1IR12_chr, "residential") &
    !stringr::str_detect(H1IR12_chr, "suburban") ~ "Urban residential only",

  H1IR12_num == 4 |
    stringr::str_detect(H1IR12_chr, "retail") ~ "Urban/commercial retail",

  H1IR12_num == 5 |
    stringr::str_detect(H1IR12_chr, "wholesale|industrial") ~ "Urban/commercial wholesale or industrial",

  H1IR12_num == 6 | stringr::str_detect(H1IR12_chr, "other") ~ "Other",

  H1IR12_num == 96 | stringr::str_detect(H1IR12_chr, "refused") ~ "Refused",

  H1IR12_num == 98 | stringr::str_detect(H1IR12_chr, "don't know|dont know") ~ "Don't know",

  H1IR12_num == 99 | stringr::str_detect(H1IR12_chr, "not applicable") ~ "Not applicable",

  TRUE ~ NA_character_
),

    residence_context_model = case_when(
      residence_context %in% c("Rural", "Suburban", "Urban") ~ residence_context,
      TRUE ~ NA_character_
    ),

    valid_survey_weight = !is.na(survey_weight) & survey_weight > 0
  )

# ------------------------------------------------------------
# 10. Descriptive audit
# ------------------------------------------------------------

covariate_summary <- tibble(
  covariate = c(
    "sex_gender_original",
    "age_original",
    "school_grade_original",
    "H1IR12_original",
    "survey_weight_original"
  ),
  resolved_variable = c(var_sex, var_age, var_grade, var_resid, var_weight)
) %>%
  mutate(
    non_missing_n = purrr::map_int(covariate, ~ safe_non_missing(analysis_covariates[[.x]])),
    missing_n = purrr::map_int(covariate, ~ safe_missing(analysis_covariates[[.x]])),
    distinct_n = purrr::map_int(covariate, ~ safe_n_distinct(analysis_covariates[[.x]])),
    min_value = purrr::map_dbl(covariate, ~ safe_min(as_numeric_safely(analysis_covariates[[.x]]))),
    max_value = purrr::map_dbl(covariate, ~ safe_max(as_numeric_safely(analysis_covariates[[.x]]))),
    mean_value = purrr::map_dbl(covariate, ~ safe_mean(as_numeric_safely(analysis_covariates[[.x]]))),
    sd_value = purrr::map_dbl(covariate, ~ safe_sd(as_numeric_safely(analysis_covariates[[.x]])))
  )

readr::write_csv(
  covariate_summary,
  file.path(output_dir, "script13e_covariate_summary.csv")
)

cat("\nCovariate summary:\n")
print(covariate_summary)

# ------------------------------------------------------------
# 11. Frequency tables
# ------------------------------------------------------------

frequency_table <- function(data, variable_name, weight_name = "survey_weight") {
  x <- data[[variable_name]]
  w <- data[[weight_name]]

  tibble(
    value = clean_chr(x),
    weight = as_numeric_safely(w)
  ) %>%
    mutate(value = if_else(is.na(value) | value == "", "Missing", value)) %>%
    group_by(value) %>%
    summarise(
      unweighted_n = n(),
      weighted_n = weighted_n_safe(weight),
      .groups = "drop"
    ) %>%
    mutate(
      variable = variable_name,
      unweighted_percent = round(100 * unweighted_n / sum(unweighted_n), 2),
      weighted_percent = round(100 * weighted_n / sum(weighted_n, na.rm = TRUE), 2)
    ) %>%
    select(variable, value, unweighted_n, unweighted_percent, weighted_n, weighted_percent) %>%
    arrange(variable, value)
}

frequency_outputs <- bind_rows(
  frequency_table(analysis_covariates, "sex_gender_clean"),
  frequency_table(analysis_covariates, "residence_context"),
  frequency_table(analysis_covariates, "residence_context_detailed"),
  frequency_table(analysis_covariates, "residence_context_model")
)

readr::write_csv(
  frequency_outputs,
  file.path(output_dir, "script13e_covariate_frequency_tables.csv")
)

cat("\nFrequency tables:\n")
print(frequency_outputs)

# ------------------------------------------------------------
# 12. Cross-tabulations relevant for later models
# ------------------------------------------------------------

cross_tab_weighted <- function(data, row_var, col_var, weight_name = "survey_weight") {
  data %>%
    transmute(
      row_value = clean_chr(.data[[row_var]]),
      col_value = clean_chr(.data[[col_var]]),
      weight = as_numeric_safely(.data[[weight_name]])
    ) %>%
    mutate(
      row_value = if_else(is.na(row_value) | row_value == "", "Missing", row_value),
      col_value = if_else(is.na(col_value) | col_value == "", "Missing", col_value)
    ) %>%
    group_by(row_value, col_value) %>%
    summarise(
      unweighted_n = n(),
      weighted_n = weighted_n_safe(weight),
      .groups = "drop"
    ) %>%
    mutate(
      row_variable = row_var,
      column_variable = col_var
    ) %>%
    select(row_variable, column_variable, row_value, col_value, unweighted_n, weighted_n)
}

cross_tabs <- bind_rows(
  cross_tab_weighted(analysis_covariates, "sex_gender_clean", "residence_context_model"),
  cross_tab_weighted(analysis_covariates, "sex_gender_clean", "school_grade_num"),
  cross_tab_weighted(analysis_covariates, "residence_context_model", "school_grade_num")
)

readr::write_csv(
  cross_tabs,
  file.path(output_dir, "script13e_covariate_cross_tabs.csv")
)

cat("\nCross-tabulations:\n")
print(cross_tabs)

# ------------------------------------------------------------
# 13. Save clean covariate dataset
# ------------------------------------------------------------

clean_covariate_dataset <- analysis_covariates %>%
  select(
    row_id_script13e,
    sex_gender_original,
    sex_gender_clean,
    age_original,
    age_num,
    school_grade_original,
    school_grade_num,
    H1IR12_original,
    H1IR12_num,
    residence_context_detailed,
    residence_context,
    residence_context_model,
    survey_weight_original,
    survey_weight,
    valid_survey_weight
  )

readr::write_csv(
  clean_covariate_dataset,
  file.path(output_dir, "script13e_clean_individual_contextual_covariates.csv")
)

saveRDS(
  clean_covariate_dataset,
  file.path(output_dir, "script13e_clean_individual_contextual_covariates.rds")
)

# ------------------------------------------------------------
# 14. Methodological decision table
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Sex/gender", "Use the recovered sex/gender variable as an individual covariate and as a heterogeneity dimension.",
  "Age", "Use age as an individual developmental covariate. If age is not recovered automatically, review candidate variables and codebook labels.",
  "School grade", "Use school grade as an educational progression covariate. If not recovered automatically, review candidate variables and codebook labels.",
  "Residence context", "Use H1IR12 as the interviewer-observed immediate residential context.",
  "H1IR12 recoding", "Recode H1IR12 into Rural, Suburban and Urban; keep Other separately and set refused, don't know and not applicable to missing.",
  "Survey weight", "Use GSWGT1 or the resolved Wave I survey weight for weighted descriptive summaries and later weighted models.",
  "Model use", "Include sex/gender, age, school grade and residence context as baseline controls in later protection/risk index models.",
  "Heterogeneity", "Estimate interaction checks for psychosocial constructs by sex/gender and residence context where sample size permits."
)

readr::write_csv(
  methodological_decisions,
  file.path(output_dir, "script13e_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 15. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_individual_contextual_covariate_audit_script13e.docx"
)

if (has_officer && has_flextable) {

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — Individual and Contextual Covariate Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 13e audits and prepares individual and contextual covariates for subsequent models of protection, risk and adolescent health behavior.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Resolved covariates", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = flextable::autofit(flextable::flextable(resolved_covariates))
    ) %>%
    officer::body_add_par("Covariate summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = flextable::autofit(flextable::flextable(covariate_summary))
    ) %>%
    officer::body_add_par("Frequency tables", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = flextable::autofit(flextable::flextable(frequency_outputs))
    ) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = flextable::autofit(flextable::flextable(methodological_decisions))
    ) %>%
    officer::body_add_par("Technical note", style = "heading 2") %>%
    officer::body_add_par(
      "H1IR12 is treated as a contextual residence measure. It describes the immediate area or street where the respondent lives. The analytical recoding distinguishes Rural, Suburban and Urban areas, while preserving detailed categories for audit and sensitivity checks.",
      style = "Normal"
    )

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}# ------------------------------------------------------------
# 16. Final audit status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "data_files_detected",
    "data_objects_recovered",
    "sex_gender_resolved",
    "age_resolved",
    "school_grade_resolved",
    "H1IR12_resolved",
    "survey_weight_resolved",
    "sex_gender_clean_nonmissing",
    "residence_context_model_nonmissing",
    "clean_covariate_dataset_saved",
    "word_report_created"
  ),
  status = c(
    length(data_files) > 0,
    nrow(data_objects) > 0,
    resolved_covariates$resolution_status[
      resolved_covariates$covariate_domain == "sex_gender"
    ] == "resolved",
    resolved_covariates$resolution_status[
      resolved_covariates$covariate_domain == "age"
    ] == "resolved",
    resolved_covariates$resolution_status[
      resolved_covariates$covariate_domain == "school_grade"
    ] == "resolved",
    resolved_covariates$resolution_status[
      resolved_covariates$covariate_domain == "residence_context"
    ] == "resolved",
    resolved_covariates$resolution_status[
      resolved_covariates$covariate_domain == "survey_weight"
    ] == "resolved",
    sum(!is.na(analysis_covariates$sex_gender_clean)) > 0,
    sum(!is.na(analysis_covariates$residence_context_model)) > 0,
    file.exists(file.path(output_dir, "script13e_clean_individual_contextual_covariates.csv")),
    !is.na(word_report_path) && file.exists(word_report_path)
  )
)
readr::write_csv(
  final_status,
  file.path(output_dir, "script13e_final_status.csv")
)

cat("\n============================================================\n")
cat("Script 13e completed: Individual and Contextual Covariate Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nOutputs created:\n")
cat("- ", file.path(output_dir, "script13e_data_file_inventory.csv"), "\n")
cat("- ", file.path(output_dir, "script13e_data_object_inventory.csv"), "\n")
cat("- ", file.path(output_dir, "script13e_covariate_candidates.csv"), "\n")
cat("- ", file.path(output_dir, "script13e_resolved_covariates.csv"), "\n")
cat("- ", file.path(output_dir, "script13e_covariate_summary.csv"), "\n")
cat("- ", file.path(output_dir, "script13e_covariate_frequency_tables.csv"), "\n")
cat("- ", file.path(output_dir, "script13e_covariate_cross_tabs.csv"), "\n")
cat("- ", file.path(output_dir, "script13e_clean_individual_contextual_covariates.csv"), "\n")
cat("- ", file.path(output_dir, "script13e_clean_individual_contextual_covariates.rds"), "\n")
cat("- ", file.path(output_dir, "script13e_methodological_decisions.csv"), "\n")
cat("- ", file.path(output_dir, "script13e_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nPrimary data object:\n")
print(primary_object_choice)

cat("\nResolved covariates:\n")
print(resolved_covariates)

cat("\nRecommended next step:\n")
cat("Review script13e_resolved_covariates.csv and script13e_final_status.csv before committing.\n")