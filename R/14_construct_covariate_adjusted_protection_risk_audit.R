# ============================================================
# Script 14 — Construct and Covariate-Adjusted Protection/Risk Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Audit candidate construct variables from Sections 8, 9, 17, 18,
#   19 and 20 and evaluate candidate protection/risk scores by
#   individual and contextual covariates.
#
# Covariates:
#   - sex/gender
#   - age
#   - school grade
#   - residence context: H1IR12
#   - survey weight
#
# Important:
#   This script creates candidate protection/risk summaries.
#   Final classification of items as protective or risky still
#   requires theoretical review before substantive modelling.
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
# 1. Project root
# ------------------------------------------------------------

project_root_candidates <- c(
  "D:/GitHub/add-health-adolescent-risk-models-clean",
  "D:/GitHub/add-health-adolescent-risk-models"
)

project_root <- project_root_candidates[
  vapply(project_root_candidates, dir.exists, logical(1))
][1]

if (is.na(project_root) || !dir.exists(project_root)) {
  stop("Project root not found.")
}

setwd(project_root)

output_dir <- file.path(project_root, "outputs", "audits")
doc_dir    <- file.path(project_root, "docs")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 14 started: Construct and Covariate-Adjusted Protection/Risk Audit\n")
cat("============================================================\n\n")
cat("Project root:\n", project_root, "\n\n")

# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

is_data_frame_like <- function(x) {
  inherits(x, c("data.frame", "tbl_df", "tbl"))
}

clean_chr <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x
}

label_text <- function(x) {
  lab <- attr(x, "label") %||% attr(x, "var_label") %||% ""
  lab <- as.character(lab)[1]
  if (is.na(lab)) lab <- ""
  stringr::str_squish(lab)
}

value_label_text <- function(x) {
  labs <- attr(x, "labels")
  if (is.null(labs)) return("")
  names_labs <- names(labs)
  if (is.null(names_labs)) return("")
  stringr::str_squish(paste(names_labs, collapse = "; "))
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

  x_chr <- clean_chr(x)
  out <- suppressWarnings(as.numeric(x_chr))

  if (all(is.na(out)) && any(!is.na(x_chr))) {
    extracted <- stringr::str_extract(x_chr, "^[0-9]+")
    out <- suppressWarnings(as.numeric(extracted))
  }

  out
}

bad_label_codes <- function(x) {
  labs <- attr(x, "labels")

  if (is.null(labs)) {
    return(c(96, 97, 98, 99, 996, 997, 998, 999, 9996, 9997, 9998, 9999))
  }

  lab_names <- names(labs)

  if (is.null(lab_names)) {
    return(c(96, 97, 98, 99, 996, 997, 998, 999, 9996, 9997, 9998, 9999))
  }

  bad <- labs[
    stringr::str_detect(
      stringr::str_to_lower(lab_names),
      "refused|don't know|dont know|not applicable|legitimate skip|skip|missing"
    )
  ]

  unique(c(
    suppressWarnings(as.numeric(bad)),
    96, 97, 98, 99, 996, 997, 998, 999, 9996, 9997, 9998, 9999
  ))
}

clean_addhealth_numeric <- function(x) {
  x_num <- as_numeric_safely(x)
  bad_codes <- bad_label_codes(x)
  x_num[x_num %in% bad_codes] <- NA_real_
  x_num
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
    min(x)
  }, error = function(e) NA_real_)
}

safe_max <- function(x) {
  tryCatch({
    if (!is.numeric(x)) return(NA_real_)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    max(x)
  }, error = function(e) NA_real_)
}

safe_mean <- function(x) {
  tryCatch({
    if (!is.numeric(x)) return(NA_real_)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    mean(x)
  }, error = function(e) NA_real_)
}

safe_sd <- function(x) {
  tryCatch({
    if (!is.numeric(x)) return(NA_real_)
    x <- x[!is.na(x)]
    if (length(x) <= 1) return(NA_real_)
    stats::sd(x)
  }, error = function(e) NA_real_)
}

weighted_mean_safe <- function(x, w) {
  x <- as_numeric_safely(x)
  w <- as_numeric_safely(w)

  valid <- !is.na(x) & !is.na(w) & w > 0

  if (sum(valid) == 0) return(NA_real_)

  sum(x[valid] * w[valid]) / sum(w[valid])
}

weighted_sum_safe <- function(w) {
  w <- as_numeric_safely(w)
  valid <- !is.na(w) & w > 0

  if (sum(valid) == 0) return(NA_real_)

  sum(w[valid])
}

standardize_safe <- function(x) {
  x <- as_numeric_safely(x)
  s <- safe_sd(x)
  m <- safe_mean(x)

  if (is.na(s) || s == 0 || is.na(m)) {
    return(rep(NA_real_, length(x)))
  }

  (x - m) / s
}

# ------------------------------------------------------------
# 3. Load data objects
# ------------------------------------------------------------

load_rda_data_frames <- function(path) {
  env <- new.env(parent = emptyenv())
  loaded_objects <- tryCatch(load(path, envir = env), error = function(e) character(0))

  if (length(loaded_objects) == 0) {
    return(tibble())
  }

  purrr::map_dfr(loaded_objects, function(obj_name) {
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

data_files <- list.files(
  file.path(project_root, "data"),
  recursive = TRUE,
  full.names = TRUE
)

data_files <- data_files[
  stringr::str_detect(tolower(data_files), "\\.(rda|rdata|rds)$")
]

if (length(data_files) == 0) {
  stop("No .rda, .RData or .rds files found under the data folder.")
}

rda_files <- data_files[
  stringr::str_detect(tolower(data_files), "\\.(rda|rdata)$")
]

rds_files <- data_files[
  stringr::str_detect(tolower(data_files), "\\.rds$")
]

data_objects <- bind_rows(
  purrr::map_dfr(rda_files, load_rda_data_frames),
  purrr::map_dfr(rds_files, load_rds_data_frame)
)

if (nrow(data_objects) == 0) {
  stop("No data frame objects recovered from data files.")
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
  file.path(output_dir, "script14_data_object_inventory.csv")
)

cat("Data objects recovered:\n")
print(data_object_inventory)

target_n <- data_object_inventory %>%
  filter(n_rows == max(n_rows, na.rm = TRUE)) %>%
  pull(n_rows) %>%
  .[1]

# ------------------------------------------------------------
# 4. Variable metadata inventory
# ------------------------------------------------------------

variable_metadata <- function(data, source_file, object_name, n_rows, n_cols) {
  purrr::map_dfr(names(data), function(v) {
    x <- data[[v]]
    x_num <- clean_addhealth_numeric(x)

    tibble(
      source_file = source_file,
      file_name = basename(source_file),
      object_name = object_name,
      n_rows = n_rows,
      n_cols = n_cols,
      variable = v,
      variable_upper = toupper(v),
      label = label_text(x),
      value_labels = value_label_text(x),
      storage_class = paste(class(x), collapse = ";"),
      non_missing_n = safe_non_missing(x),
      missing_n = safe_missing(x),
      distinct_n = safe_n_distinct(x),
      numeric_non_missing_n = safe_non_missing(x_num),
      numeric_min = safe_min(x_num),
      numeric_max = safe_max(x_num),
      numeric_mean = safe_mean(x_num),
      numeric_sd = safe_sd(x_num)
    )
  })
}

variable_inventory <- purrr::pmap_dfr(
  data_objects %>%
    select(source_file, object_name, n_rows, n_cols, data),
  function(source_file, object_name, n_rows, n_cols, data) {
    variable_metadata(data, source_file, object_name, n_rows, n_cols)
  }
)

readr::write_csv(
  variable_inventory,
  file.path(output_dir, "script14_full_variable_metadata_inventory.csv")
)

# ------------------------------------------------------------
# 5. Build individual/contextual covariates
# ------------------------------------------------------------

covariate_dictionary <- tibble::tribble(
  ~covariate_domain,     ~priority, ~candidate_name,   ~match_type,
  "sex_gender",               1,    "BIO_SEX",         "exact",
  "sex_gender",               2,    "SEX",             "contains",
  "sex_gender",               3,    "GENDER",          "contains",

  "age",                      1,    "a_age_wave1",     "exact",
  "age",                      2,    "derived_age_wave1", "exact",
  "age",                      3,    "H1AGE",           "exact",
  "age",                      4,    "AGE",             "contains",

  "school_grade",             1,    "a_grade_wave1",   "exact",
  "school_grade",             2,    "GRADE",           "contains",

  "residence_context",        1,    "H1IR12",          "exact",

  "survey_weight",            1,    "GSWGT1",          "exact",
  "survey_weight",            2,    "WEIGHT",          "contains",
  "survey_weight",            3,    "WGT",             "contains"
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

covariate_candidates <- match_dictionary(variable_inventory, covariate_dictionary)

expected_covariates <- tibble(
  covariate_domain = c(
    "sex_gender",
    "age",
    "school_grade",
    "residence_context",
    "survey_weight"
  )
)

resolved_covariates <- covariate_candidates %>%
  group_by(covariate_domain) %>%
  arrange(
    priority,
    desc(n_rows == target_n),
    desc(n_cols),
    variable,
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    covariate_domain,
    resolved_variable = variable,
    source_file,
    file_name,
    object_name,
    n_rows,
    n_cols,
    priority,
    resolution_status = "resolved"
  )

resolved_covariates <- expected_covariates %>%
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
  file.path(output_dir, "script14_resolved_covariates.csv")
)

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
    return(rep(NA, target_n))
  }

  source_data <- source_object$data[[1]]

  if (!variable_i %in% names(source_data)) {
    return(rep(NA, target_n))
  }

  out <- source_data[[variable_i]]

  if (length(out) != target_n) {
    return(rep(NA, target_n))
  }

  out
}

covariates <- tibble(
  row_id = seq_len(target_n),
  sex_gender_original = get_resolved_vector("sex_gender", target_n),
  age_original = get_resolved_vector("age", target_n),
  school_grade_original = get_resolved_vector("school_grade", target_n),
  H1IR12_original = get_resolved_vector("residence_context", target_n),
  survey_weight_original = get_resolved_vector("survey_weight", target_n)
) %>%
  mutate(
    sex_gender_num = as_numeric_safely(sex_gender_original),
    sex_gender_chr = stringr::str_to_lower(clean_chr(sex_gender_original)),

    sex_gender_clean = case_when(
      sex_gender_num == 1 ~ "Male",
      sex_gender_num == 2 ~ "Female",
      stringr::str_detect(sex_gender_chr, "female") ~ "Female",
      stringr::str_detect(sex_gender_chr, "male") &
        !stringr::str_detect(sex_gender_chr, "female") ~ "Male",
      TRUE ~ NA_character_
    ),

    age_num = as_numeric_safely(age_original),
    school_grade_num = as_numeric_safely(school_grade_original),
    H1IR12_num = as_numeric_safely(H1IR12_original),
    H1IR12_chr = stringr::str_to_lower(clean_chr(H1IR12_original)),
    survey_weight = as_numeric_safely(survey_weight_original),

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

    residence_context_model = case_when(
      residence_context %in% c("Rural", "Suburban", "Urban") ~ residence_context,
      TRUE ~ NA_character_
    ),

    valid_survey_weight = !is.na(survey_weight) & survey_weight > 0
  ) %>%
  select(
    row_id,
    sex_gender_clean,
    age_num,
    school_grade_num,
    residence_context,
    residence_context_model,
    survey_weight,
    valid_survey_weight
  )

readr::write_csv(
  covariates,
  file.path(output_dir, "script14_covariates_used.csv")
)

# ------------------------------------------------------------
# 6. Detect Section 8, 9, 17, 18, 19 and 20 candidate variables
# ------------------------------------------------------------

target_sections <- tibble::tribble(
  ~section_id, ~section_name, ~keyword_regex,
  "S08", "Section 8: School and education context",
  "school|teacher|class|classes|grade|student|homework|education|connected|belong|safe at school",

  "S09", "Section 9: Family, household and parental context",
  "mother|father|parent|parents|family|household|home|close to|talk with|care|supervision|warm|relationship",

  "S17", "Section 17: Motivation, aspirations and future orientation",
  "motivation|future|expect|expectation|college|university|graduate|job|work|ambition|plan|aspire|want to be",

  "S18", "Section 18: Personality, self-perception and emotional orientation",
  "personality|self|temper|impulsive|depress|sad|happy|lonely|trouble|risk|feel|emotion|confidence|esteem",

  "S19", "Section 19: Religion, values and moral orientation",
  "religion|religious|church|pray|prayer|god|faith|belief|service|worship|moral",

  "S20", "Section 20: Community, peer and residential context",
  "community|neighborhood|neighbourhood|friend|friends|peer|area|street|residence|resident|safe|unsafe|urban|rural"
)

exclude_variable_regex <- paste(
  c(
    "^A_",
    "SAMPLE",
    "FLAG",
    "WEIGHT",
    "WGT",
    "GSWGT",
    "BIO_SEX",
    "AGE",
    "GRADE",
    "H1IR12",
    "ID",
    "CASEID"
  ),
  collapse = "|"
)

fallback_section_candidates <- purrr::pmap_dfr(
  target_sections,
  function(section_id, section_name, keyword_regex) {

    variable_inventory %>%
      mutate(
        search_text = stringr::str_to_lower(
          paste(variable, label, value_labels, sep = " ")
        )
      ) %>%
      filter(
        n_rows == target_n,
        numeric_non_missing_n > 0,
        distinct_n >= 2,
        distinct_n <= 30,
        stringr::str_detect(search_text, keyword_regex),
        !stringr::str_detect(variable_upper, exclude_variable_regex)
      ) %>%
      mutate(
        section_id = section_id,
        section_name = section_name,
        detection_source = "keyword_fallback"
      )
  }
)

# Try to incorporate prior script13c/13d outputs if available.
prior_csv_files <- list.files(
  output_dir,
  pattern = "script13(c|d).*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

read_prior_candidate_file <- function(path) {

  x <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) tibble()
  )

  if (nrow(x) == 0) return(tibble())

  names_lower <- tolower(names(x))

  var_col <- names(x)[
    match(
      TRUE,
      names_lower %in% c(
        "variable",
        "variable_name",
        "item_variable",
        "raw_variable",
        "construct_variable",
        "resolved_variable"
      )
    )
  ]

  if (is.na(var_col)) return(tibble())

  section_col <- names(x)[
    match(TRUE, names_lower %in% c("section_id", "section"))
  ]

  section_name_col <- names(x)[
    match(TRUE, names_lower %in% c("section_name", "section_label"))
  ]

  out <- tibble(
    variable = as.character(x[[var_col]]),
    section_id = if (!is.na(section_col)) as.character(x[[section_col]]) else NA_character_,
    section_name = if (!is.na(section_name_col)) as.character(x[[section_name_col]]) else NA_character_,
    prior_source_file = basename(path)
  )

  out %>%
    filter(!is.na(variable), variable != "")
}

prior_candidates_raw <- purrr::map_dfr(prior_csv_files, read_prior_candidate_file)

if (nrow(prior_candidates_raw) == 0 || !"section_id" %in% names(prior_candidates_raw)) {
  prior_candidates_raw <- tibble(
    variable = character(),
    section_id = character(),
    section_name = character(),
    prior_source_file = character()
  )
}
prior_section_candidates <- prior_candidates_raw %>%
  mutate(
    section_id = toupper(section_id),
    section_id = case_when(
      section_id %in% c("8", "08") ~ "S08",
      section_id %in% c("9", "09") ~ "S09",
      section_id %in% c("17") ~ "S17",
      section_id %in% c("18") ~ "S18",
      section_id %in% c("19") ~ "S19",
      section_id %in% c("20") ~ "S20",
      TRUE ~ section_id
    )
  ) %>%
  filter(section_id %in% target_sections$section_id) %>%
  left_join(
    variable_inventory,
    by = "variable"
  ) %>%
  filter(!is.na(source_file), n_rows == target_n) %>%
  mutate(
    section_name = if_else(
      is.na(section_name) | section_name == "",
      target_sections$section_name[match(section_id, target_sections$section_id)],
      section_name
    ),
    detection_source = paste0("prior_output:", prior_source_file)
  )

section_candidates <- bind_rows(
  prior_section_candidates,
  fallback_section_candidates
) %>%
  filter(variable %in% variable_inventory$variable) %>%
  group_by(section_id, variable) %>%
  arrange(
    desc(stringr::str_detect(detection_source, "prior_output")),
    desc(n_rows == target_n),
    desc(label != ""),
    desc(n_cols),
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(section_id, variable)

if (nrow(section_candidates) == 0) {
  stop(
    "No candidate variables were detected for Sections 8, 9, 17, 18, 19 and 20. ",
    "Review keyword rules or prior script13c/script13d outputs."
  )
}

readr::write_csv(
  section_candidates,
  file.path(output_dir, "script14_section_variable_candidates.csv")
)

cat("\nSection candidate variables detected:\n")
print(
  section_candidates %>%
    count(section_id, section_name, detection_source, name = "variables_detected")
)

# ------------------------------------------------------------
# 7. Preliminary protection/risk classification
# ------------------------------------------------------------

protective_regex <- paste(
  c(
    "close",
    "care",
    "support",
    "warm",
    "talk",
    "connected",
    "belong",
    "safe",
    "happy",
    "confidence",
    "esteem",
    "future",
    "expect",
    "college",
    "graduate",
    "relig",
    "church",
    "pray",
    "supervision",
    "parent",
    "teacher"
  ),
  collapse = "|"
)

risk_regex <- paste(
  c(
    "risk",
    "trouble",
    "fight",
    "weapon",
    "unsafe",
    "sad",
    "depress",
    "lonely",
    "impulsive",
    "temper",
    "anger",
    "problem",
    "smoke",
    "drink",
    "drunk",
    "drug",
    "marijuana",
    "gang",
    "suspend"
  ),
  collapse = "|"
)

index_item_classification <- section_candidates %>%
  mutate(
    classification_text = stringr::str_to_lower(
      paste(variable, label, value_labels, sep = " ")
    ),
    protective_keyword_hit = stringr::str_detect(classification_text, protective_regex),
    risk_keyword_hit = stringr::str_detect(classification_text, risk_regex),
    direction_class = case_when(
      protective_keyword_hit & !risk_keyword_hit ~ "potential_protection",
      risk_keyword_hit & !protective_keyword_hit ~ "potential_risk",
      protective_keyword_hit & risk_keyword_hit ~ "ambiguous_review",
      TRUE ~ "unclassified_review"
    ),
    classification_status = case_when(
      direction_class %in% c("potential_protection", "potential_risk") ~
        "candidate_classified_by_keywords",
      direction_class == "ambiguous_review" ~
        "requires_manual_theoretical_review",
      TRUE ~
        "requires_manual_theoretical_review"
    )
  ) %>%
  select(
    section_id,
    section_name,
    variable,
    label,
    value_labels,
    object_name,
    file_name,
    source_file,
    n_rows,
    n_cols,
    non_missing_n,
    distinct_n,
    numeric_non_missing_n,
    numeric_min,
    numeric_max,
    numeric_mean,
    numeric_sd,
    detection_source,
    direction_class,
    classification_status
  )

readr::write_csv(
  index_item_classification,
  file.path(output_dir, "script14_index_item_classification.csv")
)

cat("\nPreliminary item classification:\n")
print(
  index_item_classification %>%
    count(section_id, direction_class, name = "items")
)

# ------------------------------------------------------------
# 8. Extract item-level numeric scores
# ------------------------------------------------------------

get_item_vector <- function(source_file_i, object_name_i, variable_i) {

  source_object <- data_objects %>%
    filter(
      source_file == source_file_i,
      object_name == object_name_i
    )

  if (nrow(source_object) == 0) {
    return(rep(NA_real_, target_n))
  }

  source_data <- source_object$data[[1]]

  if (!variable_i %in% names(source_data)) {
    return(rep(NA_real_, target_n))
  }

  out <- source_data[[variable_i]]

  if (length(out) != target_n) {
    return(rep(NA_real_, target_n))
  }

  clean_addhealth_numeric(out)
}

item_long <- purrr::pmap_dfr(
  index_item_classification %>%
    select(
      section_id,
      section_name,
      variable,
      direction_class,
      source_file,
      object_name
    ),
  function(section_id, section_name, variable, direction_class, source_file, object_name) {

    values <- get_item_vector(source_file, object_name, variable)

    tibble(
      row_id = seq_len(target_n),
      section_id = section_id,
      section_name = section_name,
      variable = variable,
      direction_class = direction_class,
      value_numeric = values
    )
  }
) %>%
  group_by(variable) %>%
  mutate(
    value_z = standardize_safe(value_numeric)
  ) %>%
  ungroup()

readr::write_csv(
  item_long,
  file.path(output_dir, "script14_item_long_numeric_scores.csv")
)

# ------------------------------------------------------------
# 9. Candidate protection/risk indices
# ------------------------------------------------------------

candidate_index_long <- item_long %>%
  filter(direction_class %in% c("potential_protection", "potential_risk")) %>%
  group_by(row_id, direction_class) %>%
  summarise(
    valid_items = sum(!is.na(value_z)),
    candidate_score = if_else(
      valid_items > 0,
      mean(value_z, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

if (nrow(candidate_index_long) > 0) {

  candidate_index_scores <- candidate_index_long %>%
    tidyr::pivot_wider(
      names_from = direction_class,
      values_from = c(candidate_score, valid_items),
      names_sep = "__"
    ) %>%
    rename(
      protection_index_candidate = candidate_score__potential_protection,
      risk_index_candidate = candidate_score__potential_risk,
      protection_valid_items = valid_items__potential_protection,
      risk_valid_items = valid_items__potential_risk
    )

} else {

  candidate_index_scores <- tibble(
    row_id = seq_len(target_n),
    protection_index_candidate = NA_real_,
    risk_index_candidate = NA_real_,
    protection_valid_items = NA_integer_,
    risk_valid_items = NA_integer_
  )
}

candidate_index_scores <- tibble(row_id = seq_len(target_n)) %>%
  left_join(candidate_index_scores, by = "row_id") %>%
  mutate(
    protection_index_candidate = as_numeric_safely(protection_index_candidate),
    risk_index_candidate = as_numeric_safely(risk_index_candidate),
    protection_valid_items = as_numeric_safely(protection_valid_items),
    risk_valid_items = as_numeric_safely(risk_valid_items),
    protection_risk_balance_candidate =
      protection_index_candidate - risk_index_candidate
  )

readr::write_csv(
  candidate_index_scores,
  file.path(output_dir, "script14_candidate_protection_risk_index_scores.csv")
)

saveRDS(
  candidate_index_scores,
  file.path(output_dir, "script14_candidate_protection_risk_index_scores.rds")
)

# ------------------------------------------------------------
# 10. Section-specific candidate scores
# ------------------------------------------------------------

section_score_long <- item_long %>%
  filter(direction_class %in% c("potential_protection", "potential_risk")) %>%
  group_by(row_id, section_id, section_name, direction_class) %>%
  summarise(
    valid_items = sum(!is.na(value_z)),
    section_score = if_else(
      valid_items > 0,
      mean(value_z, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

readr::write_csv(
  section_score_long,
  file.path(output_dir, "script14_section_candidate_scores_long.csv")
)

# ------------------------------------------------------------
# 11. Covariate-adjusted descriptive summaries
# ------------------------------------------------------------

analysis_data <- covariates %>%
  left_join(candidate_index_scores, by = "row_id")

summarise_score_by_group <- function(data, group_var, score_var) {

  data %>%
    transmute(
      group_value = clean_chr(.data[[group_var]]),
      score = as_numeric_safely(.data[[score_var]]),
      survey_weight = as_numeric_safely(survey_weight)
    ) %>%
    mutate(
      group_value = if_else(is.na(group_value) | group_value == "", "Missing", group_value)
    ) %>%
    group_by(group_value) %>%
    summarise(
      unweighted_n = n(),
      valid_score_n = sum(!is.na(score)),
      weighted_n = weighted_sum_safe(survey_weight),
      weighted_mean = weighted_mean_safe(score, survey_weight),
      unweighted_mean = if_else(
        valid_score_n > 0,
        mean(score, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    mutate(
      covariate = group_var,
      score_variable = score_var
    ) %>%
    select(
      score_variable,
      covariate,
      group_value,
      unweighted_n,
      valid_score_n,
      weighted_n,
      unweighted_mean,
      weighted_mean
    )
}

score_variables <- c(
  "protection_index_candidate",
  "risk_index_candidate",
  "protection_risk_balance_candidate"
)

covariate_variables <- c(
  "sex_gender_clean",
  "age_num",
  "school_grade_num",
  "residence_context_model"
)

covariate_group_summaries <- purrr::map_dfr(score_variables, function(score_var) {
  purrr::map_dfr(covariate_variables, function(group_var) {
    summarise_score_by_group(analysis_data, group_var, score_var)
  })
})

readr::write_csv(
  covariate_group_summaries,
  file.path(output_dir, "script14_covariate_group_index_summaries.csv")
)

section_covariate_data <- section_score_long %>%
  left_join(covariates, by = "row_id")

summarise_section_score_by_group <- function(data, group_var) {

  data %>%
    transmute(
      section_id,
      section_name,
      direction_class,
      group_value = clean_chr(.data[[group_var]]),
      section_score = as_numeric_safely(section_score),
      survey_weight = as_numeric_safely(survey_weight)
    ) %>%
    mutate(
      group_value = if_else(is.na(group_value) | group_value == "", "Missing", group_value)
    ) %>%
    group_by(section_id, section_name, direction_class, group_value) %>%
    summarise(
      unweighted_n = n(),
      valid_score_n = sum(!is.na(section_score)),
      weighted_n = weighted_sum_safe(survey_weight),
      unweighted_mean = if_else(
        valid_score_n > 0,
        mean(section_score, na.rm = TRUE),
        NA_real_
      ),
      weighted_mean = weighted_mean_safe(section_score, survey_weight),
      .groups = "drop"
    ) %>%
    mutate(covariate = group_var) %>%
    select(
      section_id,
      section_name,
      direction_class,
      covariate,
      group_value,
      unweighted_n,
      valid_score_n,
      weighted_n,
      unweighted_mean,
      weighted_mean
    )
}

section_covariate_summaries <- purrr::map_dfr(
  covariate_variables,
  ~ summarise_section_score_by_group(section_covariate_data, .x)
)

readr::write_csv(
  section_covariate_summaries,
  file.path(output_dir, "script14_section_covariate_score_summaries.csv")
)

# ------------------------------------------------------------
# 12. Methodological decision table
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Target sections", "Audit Sections 8, 9, 17, 18, 19 and 20 as candidate sources of protection and risk constructs.",
  "Covariates", "Use sex/gender, age, school grade, H1IR12 residence context and survey weight as baseline covariates.",
  "Residence context", "Use H1IR12 as Rural, Suburban and Urban in the main descriptive stratification; Other and missing are retained for audit.",
  "Protection/risk classification", "Use keyword-based classification only as a preliminary candidate classification; final item direction requires theoretical review.",
  "Index construction", "Construct candidate protection and risk scores as row means of standardized candidate items.",
  "Weighting", "Report weighted descriptive means using the recovered Wave I survey weight.",
  "Next modelling step", "After manual review of item direction, estimate adjusted models linking protection/risk indices to adolescent risk outcomes."
)

readr::write_csv(
  methodological_decisions,
  file.path(output_dir, "script14_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 13. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_construct_covariate_adjusted_protection_risk_audit_script14.docx"
)

section_counts <- index_item_classification %>%
  count(section_id, section_name, direction_class, name = "items") %>%
  arrange(section_id, direction_class)

resolved_covariates_display <- resolved_covariates %>%
  transmute(
    domain = covariate_domain,
    variable = resolved_variable,
    source = file_name,
    rows = n_rows,
    status = resolution_status
  )

section_counts_display <- section_counts %>%
  transmute(
    section = section_id,
    construct_area = section_name,
    classification = direction_class,
    items = items
  )

index_coverage_summary <- candidate_index_scores %>%
  summarise(
    total_n = n(),
    protection_valid_n = sum(!is.na(protection_index_candidate)),
    risk_valid_n = sum(!is.na(risk_index_candidate)),
    balance_valid_n = sum(!is.na(protection_risk_balance_candidate)),
    protection_items_mean = round(mean(protection_valid_items, na.rm = TRUE), 2),
    risk_items_mean = round(mean(risk_valid_items, na.rm = TRUE), 2)
  ) %>%
  mutate(
    protection_valid_percent = round(100 * protection_valid_n / total_n, 2),
    risk_valid_percent = round(100 * risk_valid_n / total_n, 2),
    balance_valid_percent = round(100 * balance_valid_n / total_n, 2)
  )

index_coverage_summary_display <- index_coverage_summary %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      metric == "total_n" ~ "Total respondents",
      metric == "protection_valid_n" ~ "Protection index valid N",
      metric == "risk_valid_n" ~ "Risk index valid N",
      metric == "balance_valid_n" ~ "Protection-risk balance valid N",
      metric == "protection_items_mean" ~ "Mean valid protection items",
      metric == "risk_items_mean" ~ "Mean valid risk items",
      metric == "protection_valid_percent" ~ "Protection index valid %",
      metric == "risk_valid_percent" ~ "Risk index valid %",
      metric == "balance_valid_percent" ~ "Protection-risk balance valid %",
      TRUE ~ metric
    ),
    value = round(value, 2)
  )

word_covariate_summary <- covariate_group_summaries %>%
  filter(
    score_variable %in% c(
      "protection_index_candidate",
      "risk_index_candidate",
      "protection_risk_balance_candidate"
    ),
    covariate %in% c(
      "sex_gender_clean",
      "school_grade_num",
      "residence_context_model"
    )
  ) %>%
  mutate(
    covariate = case_when(
      covariate == "sex_gender_clean" ~ "Sex/gender",
      covariate == "school_grade_num" ~ "School grade",
      covariate == "residence_context_model" ~ "Residence",
      TRUE ~ covariate
    ),
    weighted_n = round(weighted_n, 0),
    weighted_mean = round(weighted_mean, 3)
  ) %>%
  select(
    score_variable,
    covariate,
    group_value,
    unweighted_n,
    valid_score_n,
    weighted_n,
    weighted_mean
  ) %>%
  arrange(score_variable, covariate, group_value)

protection_summary_display <- word_covariate_summary %>%
  filter(score_variable == "protection_index_candidate") %>%
  select(-score_variable)

risk_summary_display <- word_covariate_summary %>%
  filter(score_variable == "risk_index_candidate") %>%
  select(-score_variable)

balance_summary_display <- word_covariate_summary %>%
  filter(score_variable == "protection_risk_balance_candidate") %>%
  select(-score_variable)

methodological_decisions_display <- methodological_decisions %>%
  transmute(
    area = decision_area,
    decision = decision
  )

editorial_note <- tibble::tribble(
  ~issue, ~interpretation,
  "Candidate classification", "The protection/risk classification is preliminary and based on keywords. Final classification requires manual review of item wording and coding direction.",
  "Risk index", "The candidate risk index is fragile at this stage because only two potential risk items were detected, both in Section 18.",
  "Protection index", "The candidate protection index has broader coverage, especially in Sections 8 and 9.",
  "Balance score", "The protection-risk balance score should not be interpreted substantively until the risk index is strengthened or theoretically confirmed.",
  "Covariate analysis", "Descriptive comparisons by sex/gender, school grade and residence context are valid as an audit step, but not yet as final inferential evidence."
)

if (has_officer && has_flextable) {

  make_ft <- function(x) {
    flextable::flextable(x) %>%
      flextable::fontsize(size = 8, part = "all") %>%
      flextable::padding(padding = 2, part = "all") %>%
      flextable::theme_booktabs() %>%
      flextable::autofit() %>%
      flextable::set_table_properties(width = 1, layout = "autofit")
  }

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — Construct and Covariate-Adjusted Protection/Risk Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 14 audits candidate construct variables from Sections 8, 9, 17, 18, 19 and 20. It summarizes preliminary protection and risk scores by sex/gender, school grade and residence context. The results are intended for audit and item-review purposes, not for final substantive interpretation.",
      style = "Normal"
    ) %>%

    officer::body_add_par("Resolved covariates", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(resolved_covariates_display)
    ) %>%

    officer::body_add_par("Candidate item classification by section", style = "heading 2") %>%
    officer::body_add_par(
      "The table below reports the preliminary classification of candidate items. Items classified as unclassified_review require manual theoretical review before index construction.",
      style = "Normal"
    ) %>%
    flextable::body_add_flextable(
      value = make_ft(section_counts_display)
    ) %>%

    officer::body_add_par("Candidate index coverage", style = "heading 2") %>%
    officer::body_add_par(
      "Coverage statistics indicate how many respondents have non-missing candidate protection, risk and balance scores. Low coverage for a score signals that it should be treated cautiously.",
      style = "Normal"
    ) %>%
    flextable::body_add_flextable(
      value = make_ft(index_coverage_summary_display)
    ) %>%

    officer::body_add_par("Protection index by covariates", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(protection_summary_display)
    ) %>%

    officer::body_add_par("Risk index by covariates", style = "heading 2") %>%
    officer::body_add_par(
      "The candidate risk index is retained only as an audit output. It is based on a small number of items and should not yet be interpreted as a stable risk construct.",
      style = "Normal"
    ) %>%
    flextable::body_add_flextable(
      value = make_ft(risk_summary_display)
    ) %>%

    officer::body_add_par("Protection-risk balance by covariates", style = "heading 2") %>%
    officer::body_add_par(
      "The balance score is exploratory. It depends on both the candidate protection index and the candidate risk index. Since the risk index is still fragile, the balance score also requires caution.",
      style = "Normal"
    ) %>%
    flextable::body_add_flextable(
      value = make_ft(balance_summary_display)
    ) %>%

    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(methodological_decisions_display)
    ) %>%

    officer::body_add_par("Editorial and methodological caution", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(editorial_note)
    )

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}# ------------------------------------------------------------
# 14. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "data_objects_recovered",
    "covariates_resolved",
    "section_candidates_detected",
    "potential_protection_items_detected",
    "potential_risk_items_detected",
    "candidate_index_scores_saved",
    "covariate_group_summaries_saved",
    "section_covariate_summaries_saved",
    "word_report_created"
  ),
  status = c(
    nrow(data_objects) > 0,
    all(resolved_covariates$resolution_status == "resolved"),
    nrow(section_candidates) > 0,
    sum(index_item_classification$direction_class == "potential_protection") > 0,
    sum(index_item_classification$direction_class == "potential_risk") > 0,
    file.exists(file.path(output_dir, "script14_candidate_protection_risk_index_scores.csv")),
    file.exists(file.path(output_dir, "script14_covariate_group_index_summaries.csv")),
    file.exists(file.path(output_dir, "script14_section_covariate_score_summaries.csv")),
    !is.na(word_report_path) && file.exists(word_report_path)
  )
)

readr::write_csv(
  final_status,
  file.path(output_dir, "script14_final_status.csv")
)

cat("\n============================================================\n")
cat("Script 14 completed: Construct and Covariate-Adjusted Protection/Risk Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nCandidate item classification by section:\n")
print(section_counts)

cat("\nCovariate group summaries preview:\n")
print(head(covariate_group_summaries, 30))

cat("\nOutputs created:\n")
cat("- ", file.path(output_dir, "script14_data_object_inventory.csv"), "\n")
cat("- ", file.path(output_dir, "script14_full_variable_metadata_inventory.csv"), "\n")
cat("- ", file.path(output_dir, "script14_resolved_covariates.csv"), "\n")
cat("- ", file.path(output_dir, "script14_covariates_used.csv"), "\n")
cat("- ", file.path(output_dir, "script14_section_variable_candidates.csv"), "\n")
cat("- ", file.path(output_dir, "script14_index_item_classification.csv"), "\n")
cat("- ", file.path(output_dir, "script14_item_long_numeric_scores.csv"), "\n")
cat("- ", file.path(output_dir, "script14_candidate_protection_risk_index_scores.csv"), "\n")
cat("- ", file.path(output_dir, "script14_candidate_protection_risk_index_scores.rds"), "\n")
cat("- ", file.path(output_dir, "script14_section_candidate_scores_long.csv"), "\n")
cat("- ", file.path(output_dir, "script14_covariate_group_index_summaries.csv"), "\n")
cat("- ", file.path(output_dir, "script14_section_covariate_score_summaries.csv"), "\n")
cat("- ", file.path(output_dir, "script14_methodological_decisions.csv"), "\n")
cat("- ", file.path(output_dir, "script14_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRecommended next step:\n")
cat("Review script14_index_item_classification.csv before interpreting protection/risk scores as final constructs.\n")