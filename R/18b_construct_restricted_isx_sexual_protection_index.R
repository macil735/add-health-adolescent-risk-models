# ============================================================
# Script 18b — Construct Restricted ISX-Equivalent Sexual Protection Index
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Construct a restricted ISX-equivalent sexual protection behavior
#   index for Add Health Wave I.
#
# Dependent variable to be used later:
#   restricted_isx_sexual_protection_index
#
# Restricted ISX dimensions:
#   isx1 — sexual initiation / timing of first sex
#   isx3 — condom / contraceptive / birth-control use
#
# Dimensions not available in public-use local files:
#   isx2 — number of partners / partner exposure
#   isx4 — sexual frequency / exposure intensity
#
# Methodological basis:
#   Script 18a v5 found no defensible behavioral candidates for isx2
#   and isx4. Therefore, Script 18b constructs a restricted index and
#   documents the limitation.
#
# Main outputs:
#   outputs/indices/script18b_restricted_isx_sexual_protection_index_LOCAL_ONLY.csv
#   outputs/audits/script18b_restricted_isx_index_summary.csv
#   outputs/audits/script18b_restricted_isx_component_distribution.csv
#   outputs/audits/script18b_restricted_isx_missingness_summary.csv
#   outputs/audits/script18b_restricted_isx_scoring_audit.csv
#   outputs/audits/script18b_methodological_decisions.csv
#   outputs/audits/script18b_final_status.csv
#   docs/add_health_wave01_restricted_isx_sexual_protection_index_script18b.docx
#
# Data protection:
#   The row-level index output is marked LOCAL_ONLY and should not be
#   committed to GitHub.
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "readr",
  "stringr",
  "tidyr",
  "purrr"
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
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

has_haven <- requireNamespace("haven", quietly = TRUE)
has_officer <- requireNamespace("officer", quietly = TRUE)
has_flextable <- requireNamespace("flextable", quietly = TRUE)

if (has_officer) {
  suppressPackageStartupMessages(library(officer))
}

if (has_flextable) {
  suppressPackageStartupMessages(library(flextable))
}

# ------------------------------------------------------------
# 1. Project root and folders
# ------------------------------------------------------------

project_root <- "C:/Users/LENOVO/GitHub/add-health-adolescent-risk-models"

if (!dir.exists(project_root)) {
  stop("Project root not found: ", project_root)
}

setwd(project_root)

audit_dir <- file.path(project_root, "outputs", "audits")
indices_dir <- file.path(project_root, "outputs", "indices")
doc_dir <- file.path(project_root, "docs")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(indices_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 18b started: Restricted ISX Sexual Protection Index\n")
cat("============================================================\n\n")
cat("Project root:\n", project_root, "\n\n")

# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

normalize_path <- function(x) {
  normalizePath(x, winslash = "/", mustWork = FALSE)
}

clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_squish(x)
}

lower_clean <- function(x) {
  stringr::str_to_lower(clean_chr(x))
}

to_numeric_safe <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- as.numeric(x)
  }
  suppressWarnings(as.numeric(as.character(x)))
}

variable_label_string <- function(x) {
  lbl <- attr(x, "label", exact = TRUE)

  if (is.null(lbl)) {
    return("")
  }

  as.character(lbl)
}

value_label_string <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)

  if (is.null(labels)) {
    return("")
  }

  paste0(
    as.numeric(labels),
    "=",
    names(labels),
    collapse = "; "
  )
}

extract_missing_codes_from_labels <- function(value_labels) {

  value_labels_l <- lower_clean(value_labels)

  # Do not treat 6, 7, 8, 9 as missing by default.
  # They can be valid values for month or frequency variables.
  standard_high_missing <- c(
    96, 97, 98, 99,
    996, 997, 998, 999,
    9996, 9997, 9998, 9999
  )

  if (is.na(value_labels_l) || value_labels_l == "") {
    return(standard_high_missing)
  }

  parts <- unlist(stringr::str_split(value_labels_l, ";|\\||\\n"))

  missing_terms <- paste(
    c(
      "refused",
      "don't know",
      "dont know",
      "not applicable",
      "missing",
      "skip",
      "legitimate skip",
      "no answer"
    ),
    collapse = "|"
  )

  parts_missing <- parts[stringr::str_detect(parts, missing_terms)]

  extracted <- unlist(stringr::str_extract_all(parts_missing, "-?\\d+(\\.\\d+)?"))
  extracted <- suppressWarnings(as.numeric(extracted))
  extracted <- extracted[!is.na(extracted)]

  unique(c(standard_high_missing, extracted))
}

valid_numeric_vector <- function(x, value_labels = "") {

  x_num <- to_numeric_safe(x)
  missing_codes <- extract_missing_codes_from_labels(value_labels)

  x_num[x_num %in% missing_codes] <- NA_real_

  x_num
}

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  max(x)
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  mean(x)
}

safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) {
    return(NA_real_)
  }
  sd(x)
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

summarise_value_distribution <- function(x, max_values = 25) {

  tab <- tibble(value = x) %>%
    filter(!is.na(value)) %>%
    count(value, name = "n") %>%
    arrange(value)

  if (nrow(tab) == 0) {
    return("")
  }

  if (nrow(tab) > max_values) {
    tab <- tab %>% slice_head(n = max_values)
    suffix <- "; ..."
  } else {
    suffix <- ""
  }

  paste0(
    paste0(tab$value, ":", tab$n, collapse = "; "),
    suffix
  )
}

convert_two_digit_year <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x >= 70 & x <= 99 ~ 1900 + x,
    x >= 0 & x <= 30 ~ 2000 + x,
    x >= 1900 & x <= 2030 ~ x,
    TRUE ~ NA_real_
  )
}

# ------------------------------------------------------------
# 3. Data file readers
# ------------------------------------------------------------

read_data_objects <- function(file_path) {

  file_path <- normalize_path(file_path)

  if (!file.exists(file_path)) {
    return(list())
  }

  ext <- stringr::str_to_lower(tools::file_ext(file_path))

  out <- list()

  if (ext %in% c("rda", "rdata")) {

    env <- new.env(parent = emptyenv())
    loaded_names <- load(file_path, envir = env)

    for (nm in loaded_names) {
      obj <- get(nm, envir = env)
      if (is.data.frame(obj)) {
        out[[nm]] <- tibble::as_tibble(obj)
      }
    }

  } else if (ext == "rds") {

    obj <- readRDS(file_path)

    if (is.data.frame(obj)) {
      out[[basename(file_path)]] <- tibble::as_tibble(obj)
    }

  } else if (ext == "csv") {

    obj <- suppressMessages(readr::read_csv(file_path, show_col_types = FALSE))

    if (is.data.frame(obj)) {
      out[[basename(file_path)]] <- tibble::as_tibble(obj)
    }

  } else if (ext == "sas7bdat" && has_haven) {

    obj <- haven::read_sas(file_path)

    if (is.data.frame(obj)) {
      out[[basename(file_path)]] <- tibble::as_tibble(obj)
    }
  }

  out
}

read_object_from_source <- function(source_file, object_name) {

  source_file <- normalize_path(source_file)

  if (!file.exists(source_file)) {
    stop("Source file not found: ", source_file)
  }

  ext <- stringr::str_to_lower(tools::file_ext(source_file))

  if (ext %in% c("rda", "rdata")) {

    env <- new.env(parent = emptyenv())
    loaded_names <- load(source_file, envir = env)

    if (object_name %in% loaded_names) {
      obj <- get(object_name, envir = env)
    } else {
      data_objects <- loaded_names[
        vapply(
          loaded_names,
          function(nm) is.data.frame(get(nm, envir = env)),
          logical(1)
        )
      ]

      if (length(data_objects) == 0) {
        stop("No data frame object found in: ", source_file)
      }

      obj <- get(data_objects[1], envir = env)
    }

    return(tibble::as_tibble(obj))
  }

  if (ext == "rds") {
    obj <- readRDS(source_file)
    return(tibble::as_tibble(obj))
  }

  if (ext == "csv") {
    obj <- readr::read_csv(source_file, show_col_types = FALSE)
    return(tibble::as_tibble(obj))
  }

  if (ext == "sas7bdat" && has_haven) {
    obj <- haven::read_sas(source_file)
    return(tibble::as_tibble(obj))
  }

  stop("Unsupported source file type: ", source_file)
}

# ------------------------------------------------------------
# 4. Required audit inputs
# ------------------------------------------------------------

h1co_best_source_path <- file.path(
  audit_dir,
  "script18a_v4_h1co_best_source_by_variable.csv"
)

v5_feasibility_path <- file.path(
  audit_dir,
  "script18a_v5_index_feasibility_decision.csv"
)

if (!file.exists(h1co_best_source_path)) {
  stop(
    "Missing Script 18a v4 output:\n",
    h1co_best_source_path,
    "\nRun Script 18a v4 before Script 18b."
  )
}

if (!file.exists(v5_feasibility_path)) {
  stop(
    "Missing Script 18a v5 feasibility output:\n",
    v5_feasibility_path,
    "\nRun Script 18a v5 before Script 18b."
  )
}

h1co_best_source <- readr::read_csv(
  h1co_best_source_path,
  show_col_types = FALSE
)

v5_feasibility <- readr::read_csv(
  v5_feasibility_path,
  show_col_types = FALSE
)

cat("Loaded Script 18a v4 best-source map:\n")
cat(h1co_best_source_path, "\n\n")

cat("Loaded Script 18a v5 feasibility decision:\n")
cat(v5_feasibility_path, "\n\n")

# ------------------------------------------------------------
# 5. ISX variable set
# ------------------------------------------------------------

restricted_isx_variables <- c(
  "H1CO1",
  "H1CO2Y",
  "H1CO2M",
  "H1CO3",
  "H1CO6",
  "H1CO8",
  "H1CO9",
  "H1CO13"
)

available_isx_variables <- h1co_best_source %>%
  filter(variable %in% restricted_isx_variables)

missing_isx_variables <- setdiff(
  restricted_isx_variables,
  available_isx_variables$variable
)

if (length(missing_isx_variables) > 0) {
  warning(
    "The following restricted ISX variables are missing from the v4 best-source map: ",
    paste(missing_isx_variables, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 6. Recover H1CO vectors from best sources
# ------------------------------------------------------------

target_n <- h1co_best_source %>%
  filter(variable %in% restricted_isx_variables) %>%
  summarise(target_n = max(object_rows, na.rm = TRUE)) %>%
  pull(target_n)

if (is.na(target_n) || target_n <= 0) {
  target_n <- 6504L
}

align_vector <- function(x, target_n) {
  if (length(x) == target_n) {
    return(x)
  }

  if (length(x) > target_n) {
    return(x[seq_len(target_n)])
  }

  c(x, rep(NA, target_n - length(x)))
}

isx_data <- tibble(row_id = seq_len(target_n))

variable_source_audit <- list()

for (var in restricted_isx_variables) {

  row_info <- h1co_best_source %>%
    filter(variable == var) %>%
    arrange(desc(valid_n), desc(distinct_valid_n)) %>%
    slice_head(n = 1)

  if (nrow(row_info) == 0) {
    isx_data[[var]] <- rep(NA_real_, target_n)

    variable_source_audit[[length(variable_source_audit) + 1]] <- tibble(
      variable = var,
      recovered = FALSE,
      source_file = "",
      file_name = "",
      object_name = "",
      source_valid_n = NA_integer_,
      source_distinct_valid_n = NA_integer_,
      variable_label_effective = "",
      value_labels_effective = "",
      note = "Variable not found in v4 best-source map."
    )

    next
  }

  source_df <- read_object_from_source(
    row_info$source_file[1],
    row_info$object_name[1]
  )

  if (!var %in% names(source_df)) {
    isx_data[[var]] <- rep(NA_real_, target_n)

    variable_source_audit[[length(variable_source_audit) + 1]] <- tibble(
      variable = var,
      recovered = FALSE,
      source_file = row_info$source_file[1],
      file_name = row_info$file_name[1],
      object_name = row_info$object_name[1],
      source_valid_n = row_info$valid_n[1],
      source_distinct_valid_n = row_info$distinct_valid_n[1],
      variable_label_effective = row_info$variable_label_effective[1],
      value_labels_effective = row_info$value_labels_effective[1],
      note = "Variable was listed in v4 map but not found in selected object."
    )

    next
  }

  raw_vec <- source_df[[var]]
  value_labels <- row_info$value_labels_effective[1]

  isx_data[[var]] <- align_vector(
    valid_numeric_vector(raw_vec, value_labels),
    target_n
  )

  variable_source_audit[[length(variable_source_audit) + 1]] <- tibble(
    variable = var,
    recovered = TRUE,
    source_file = row_info$source_file[1],
    file_name = row_info$file_name[1],
    object_name = row_info$object_name[1],
    source_valid_n = row_info$valid_n[1],
    source_distinct_valid_n = row_info$distinct_valid_n[1],
    variable_label_effective = row_info$variable_label_effective[1],
    value_labels_effective = row_info$value_labels_effective[1],
    note = "Variable recovered from v4 best source."
  )
}

variable_source_audit <- bind_rows(variable_source_audit)

# ------------------------------------------------------------
# 7. Recover age, survey weight and respondent id if available
# ------------------------------------------------------------

candidate_data_files <- list.files(
  project_root,
  pattern = "\\.(rda|RData|rds|csv|sas7bdat)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

candidate_data_files <- normalize_path(candidate_data_files)

candidate_data_files <- candidate_data_files[
  !stringr::str_detect(candidate_data_files, "/\\.git/") &
    !stringr::str_detect(candidate_data_files, "/outputs/") &
    !stringr::str_detect(candidate_data_files, "/docs/")
]

candidate_data_files <- candidate_data_files[file.exists(candidate_data_files)]

find_best_variable_occurrence <- function(candidate_names) {

  occurrence_list <- list()

  for (fp in candidate_data_files) {

    objects <- read_data_objects(fp)

    if (length(objects) == 0) {
      next
    }

    for (obj_name in names(objects)) {

      df <- objects[[obj_name]]

      if (!is.data.frame(df) || nrow(df) == 0) {
        next
      }

      names(df) <- as.character(names(df))

      matched_vars <- names(df)[
        stringr::str_to_lower(names(df)) %in%
          stringr::str_to_lower(candidate_names)
      ]

      if (length(matched_vars) == 0) {
        next
      }

      for (var in matched_vars) {
        x <- df[[var]]
        val_labels <- value_label_string(x)
        x_num <- valid_numeric_vector(x, val_labels)

        occurrence_list[[length(occurrence_list) + 1]] <- tibble(
          variable = var,
          source_file = normalize_path(fp),
          file_name = basename(fp),
          object_name = obj_name,
          object_rows = nrow(df),
          valid_n = sum(!is.na(x_num)),
          distinct_valid_n = length(unique(x_num[!is.na(x_num)])),
          variable_label = variable_label_string(x),
          value_labels = value_label_string(x)
        )
      }
    }
  }

  if (length(occurrence_list) == 0) {
    return(tibble())
  }

  bind_rows(occurrence_list) %>%
    arrange(
      desc(valid_n),
      desc(distinct_valid_n),
      desc(object_rows),
      variable
    ) %>%
    slice_head(n = 1)
}

recover_optional_variable <- function(candidate_names, target_n) {

  occurrence <- find_best_variable_occurrence(candidate_names)

  if (nrow(occurrence) == 0) {
    return(list(
      vector = rep(NA_real_, target_n),
      audit = tibble(
        requested_names = paste(candidate_names, collapse = ", "),
        recovered = FALSE,
        variable = "",
        source_file = "",
        file_name = "",
        object_name = "",
        valid_n = NA_integer_,
        note = "No matching variable found."
      )
    ))
  }

  df <- read_object_from_source(
    occurrence$source_file[1],
    occurrence$object_name[1]
  )

  var <- occurrence$variable[1]

  if (!var %in% names(df)) {
    return(list(
      vector = rep(NA_real_, target_n),
      audit = tibble(
        requested_names = paste(candidate_names, collapse = ", "),
        recovered = FALSE,
        variable = var,
        source_file = occurrence$source_file[1],
        file_name = occurrence$file_name[1],
        object_name = occurrence$object_name[1],
        valid_n = occurrence$valid_n[1],
        note = "Variable not found in recovered source object."
      )
    ))
  }

  x_num <- valid_numeric_vector(df[[var]], occurrence$value_labels[1])

  list(
    vector = align_vector(x_num, target_n),
    audit = tibble(
      requested_names = paste(candidate_names, collapse = ", "),
      recovered = TRUE,
      variable = var,
      source_file = occurrence$source_file[1],
      file_name = occurrence$file_name[1],
      object_name = occurrence$object_name[1],
      valid_n = occurrence$valid_n[1],
      note = "Variable recovered."
    )
  )
}

age_recovery <- recover_optional_variable(
  candidate_names = c(
    "a_age_wave1",
    "age_wave1",
    "age",
    "H1GI1Y",
    "H1GI1"
  ),
  target_n = target_n
)

weight_recovery <- recover_optional_variable(
  candidate_names = c(
    "GSWGT1",
    "gswgt1",
    "weight",
    "survey_weight"
  ),
  target_n = target_n
)

id_recovery <- recover_optional_variable(
  candidate_names = c(
    "respondent_id",
    "AID",
    "aid",
    "ID",
    "id"
  ),
  target_n = target_n
)

isx_data$age_wave1 <- age_recovery$vector
isx_data$survey_weight <- weight_recovery$vector

if (all(is.na(id_recovery$vector))) {
  isx_data$respondent_id <- paste0("row_", isx_data$row_id)
} else {
  isx_data$respondent_id <- as.character(id_recovery$vector)
}

optional_variable_audit <- bind_rows(
  age_recovery$audit %>% mutate(field = "age_wave1"),
  weight_recovery$audit %>% mutate(field = "survey_weight"),
  id_recovery$audit %>% mutate(field = "respondent_id")
) %>%
  select(field, everything())

# ------------------------------------------------------------
# 8. Construct operational sexual behavior status
# ------------------------------------------------------------

downstream_behavior_vars <- c(
  "H1CO2Y",
  "H1CO2M",
  "H1CO3",
  "H1CO6",
  "H1CO8",
  "H1CO9",
  "H1CO13"
)

isx_data <- isx_data %>%
  mutate(
    downstream_behavior_evidence = if_any(
      all_of(downstream_behavior_vars),
      ~ !is.na(.x)
    ),
    explicit_ever_sex_evidence = !is.na(H1CO1) & H1CO1 == 1,
    ever_had_sex_operational =
      explicit_ever_sex_evidence | downstream_behavior_evidence,
    never_had_sex_operational =
      !ever_had_sex_operational,
    ever_sex_status_source = case_when(
      explicit_ever_sex_evidence ~ "H1CO1_explicit_or_valid_positive",
      !explicit_ever_sex_evidence & downstream_behavior_evidence ~
        "downstream_behavior_evidence",
      never_had_sex_operational ~
        "operational_never_or_not_routed",
      TRUE ~ "undetermined"
    )
  )

# ------------------------------------------------------------
# 9. Construct ISX1: sexual initiation / timing score
# ------------------------------------------------------------

assumed_wave1_survey_year <- 1995

isx_data <- isx_data %>%
  mutate(
    first_sex_year_full = convert_two_digit_year(H1CO2Y),
    birth_year_proxy = ifelse(
      !is.na(age_wave1),
      assumed_wave1_survey_year - age_wave1,
      NA_real_
    ),
    age_at_first_sex_proxy = ifelse(
      !is.na(first_sex_year_full) & !is.na(birth_year_proxy),
      first_sex_year_full - birth_year_proxy,
      NA_real_
    ),
    age_at_first_sex_proxy = ifelse(
      !is.na(age_at_first_sex_proxy) &
        age_at_first_sex_proxy >= 10 &
        age_at_first_sex_proxy <= pmax(age_wave1, 10, na.rm = TRUE),
      age_at_first_sex_proxy,
      NA_real_
    ),
    isx1_timing_score = case_when(
      never_had_sex_operational ~ 4,
      ever_had_sex_operational & !is.na(age_at_first_sex_proxy) &
        age_at_first_sex_proxy >= 18 ~ 3,
      ever_had_sex_operational & !is.na(age_at_first_sex_proxy) &
        age_at_first_sex_proxy >= 16 &
        age_at_first_sex_proxy <= 17 ~ 2,
      ever_had_sex_operational & !is.na(age_at_first_sex_proxy) &
        age_at_first_sex_proxy <= 15 ~ 1,
      TRUE ~ NA_real_
    ),
    isx1_timing_score_source = case_when(
      never_had_sex_operational ~ "never_sex_operational_score_4",
      !is.na(age_at_first_sex_proxy) ~
        "age_at_first_sex_proxy_from_H1CO2Y_age_wave1_assumed_1995",
      ever_had_sex_operational ~
        "ever_sex_but_timing_not_scorable",
      TRUE ~ "not_scorable"
    )
  )

# ------------------------------------------------------------
# 10. Construct ISX3: condom / contraceptive behavior score
# ------------------------------------------------------------

isx_data <- isx_data %>%
  mutate(
    condom_ever_indicator = !is.na(H1CO8) & H1CO8 == 1,
    condom_frequency_available = !is.na(H1CO9),
    birthcontrol_first_sex_indicator = !is.na(H1CO3) & H1CO3 == 1,
    birthcontrol_most_recent_indicator = !is.na(H1CO6) & H1CO6 == 1,
    birthcontrol_pill_ever_indicator = !is.na(H1CO13) & H1CO13 == 1,
    any_protective_method_indicator =
      condom_ever_indicator |
      birthcontrol_first_sex_indicator |
      birthcontrol_most_recent_indicator |
      birthcontrol_pill_ever_indicator,

    condom_frequency_score = case_when(
      never_had_sex_operational ~ 4,
      ever_had_sex_operational & H1CO9 %in% c(1) ~ 3,
      ever_had_sex_operational & H1CO9 %in% c(2, 3) ~ 2,
      ever_had_sex_operational & H1CO9 %in% c(4, 5) ~ 1,
      TRUE ~ NA_real_
    ),

    method_indicator_proxy_score = case_when(
      never_had_sex_operational ~ 4,
      ever_had_sex_operational & any_protective_method_indicator ~ 3,
      TRUE ~ NA_real_
    ),

    isx3_method_score = case_when(
      never_had_sex_operational ~ 4,
      ever_had_sex_operational & !is.na(condom_frequency_score) ~
        condom_frequency_score,
      ever_had_sex_operational & is.na(condom_frequency_score) &
        !is.na(method_indicator_proxy_score) ~
        method_indicator_proxy_score,
      TRUE ~ NA_real_
    ),

    isx3_method_score_source = case_when(
      never_had_sex_operational ~ "never_sex_operational_score_4",
      ever_had_sex_operational & !is.na(condom_frequency_score) ~
        "H1CO9_condom_frequency_score",
      ever_had_sex_operational & any_protective_method_indicator ~
        "proxy_any_condom_or_birthcontrol_indicator",
      ever_had_sex_operational ~
        "ever_sex_but_method_not_scorable",
      TRUE ~ "not_scorable"
    )
  )

# ------------------------------------------------------------
# 11. Construct restricted ISX index
# ------------------------------------------------------------

isx_data <- isx_data %>%
  mutate(
    restricted_isx_valid_component_count =
      rowSums(
        cbind(
          !is.na(isx1_timing_score),
          !is.na(isx3_method_score)
        )
      ),
    restricted_isx_index_primary_1_4 = ifelse(
      restricted_isx_valid_component_count >= 1,
      rowMeans(
        cbind(isx1_timing_score, isx3_method_score),
        na.rm = TRUE
      ),
      NA_real_
    ),
    restricted_isx_index_strict_1_4 = ifelse(
      restricted_isx_valid_component_count == 2,
      rowMeans(
        cbind(isx1_timing_score, isx3_method_score),
        na.rm = FALSE
      ),
      NA_real_
    ),
    restricted_isx_index_primary_0_1 =
      (restricted_isx_index_primary_1_4 - 1) / 3,
    restricted_isx_index_strict_0_1 =
      (restricted_isx_index_strict_1_4 - 1) / 3,
    restricted_isx_index_quality_flag = case_when(
      restricted_isx_valid_component_count == 2 ~
        "both_components_available",
      restricted_isx_valid_component_count == 1 ~
        "one_component_available",
      TRUE ~
        "no_component_available"
    )
  )

# ------------------------------------------------------------
# 12. Row-level local-only output
# ------------------------------------------------------------

row_level_output <- isx_data %>%
  select(
    row_id,
    respondent_id,
    ever_had_sex_operational,
    never_had_sex_operational,
    ever_sex_status_source,
    age_wave1,
    first_sex_year_full,
    age_at_first_sex_proxy,
    isx1_timing_score,
    isx1_timing_score_source,
    isx3_method_score,
    isx3_method_score_source,
    restricted_isx_valid_component_count,
    restricted_isx_index_primary_1_4,
    restricted_isx_index_primary_0_1,
    restricted_isx_index_strict_1_4,
    restricted_isx_index_strict_0_1,
    restricted_isx_index_quality_flag,
    survey_weight
  )

row_level_index_path <- file.path(
  indices_dir,
  "script18b_restricted_isx_sexual_protection_index_LOCAL_ONLY.csv"
)

readr::write_csv(
  row_level_output,
  row_level_index_path
)

# ------------------------------------------------------------
# 13. Aggregate summaries
# ------------------------------------------------------------

index_summary <- tibble(
  metric = c(
    "respondents_total",
    "ever_had_sex_operational_n",
    "never_had_sex_operational_n",
    "primary_index_valid_n",
    "strict_index_valid_n",
    "primary_index_mean_1_4",
    "primary_index_sd_1_4",
    "primary_index_min_1_4",
    "primary_index_max_1_4",
    "primary_index_mean_0_1",
    "strict_index_mean_1_4",
    "weighted_primary_index_mean_1_4",
    "weighted_primary_index_mean_0_1"
  ),
  value = c(
    nrow(isx_data),
    sum(isx_data$ever_had_sex_operational, na.rm = TRUE),
    sum(isx_data$never_had_sex_operational, na.rm = TRUE),
    sum(!is.na(isx_data$restricted_isx_index_primary_1_4)),
    sum(!is.na(isx_data$restricted_isx_index_strict_1_4)),
    safe_mean(isx_data$restricted_isx_index_primary_1_4),
    safe_sd(isx_data$restricted_isx_index_primary_1_4),
    safe_min(isx_data$restricted_isx_index_primary_1_4),
    safe_max(isx_data$restricted_isx_index_primary_1_4),
    safe_mean(isx_data$restricted_isx_index_primary_0_1),
    safe_mean(isx_data$restricted_isx_index_strict_1_4),
    weighted_mean_safe(
      isx_data$restricted_isx_index_primary_1_4,
      isx_data$survey_weight
    ),
    weighted_mean_safe(
      isx_data$restricted_isx_index_primary_0_1,
      isx_data$survey_weight
    )
  )
)

component_distribution <- bind_rows(
  isx_data %>%
    count(isx1_timing_score, name = "n") %>%
    mutate(
      component = "isx1_timing_score",
      score = as.character(isx1_timing_score)
    ) %>%
    select(component, score, n),

  isx_data %>%
    count(isx3_method_score, name = "n") %>%
    mutate(
      component = "isx3_method_score",
      score = as.character(isx3_method_score)
    ) %>%
    select(component, score, n),

  isx_data %>%
    count(restricted_isx_valid_component_count, name = "n") %>%
    mutate(
      component = "valid_component_count",
      score = as.character(restricted_isx_valid_component_count)
    ) %>%
    select(component, score, n),

  isx_data %>%
    count(restricted_isx_index_quality_flag, name = "n") %>%
    mutate(
      component = "index_quality_flag",
      score = restricted_isx_index_quality_flag
    ) %>%
    select(component, score, n)
)

missingness_summary <- tibble(
  variable = c(
    "H1CO1",
    "H1CO2Y",
    "H1CO2M",
    "H1CO3",
    "H1CO6",
    "H1CO8",
    "H1CO9",
    "H1CO13",
    "age_wave1",
    "survey_weight",
    "isx1_timing_score",
    "isx3_method_score",
    "restricted_isx_index_primary_1_4",
    "restricted_isx_index_strict_1_4"
  )
) %>%
  mutate(
    non_missing_n = purrr::map_int(
      variable,
      function(v) {
        if (!v %in% names(isx_data)) {
          return(NA_integer_)
        }
        sum(!is.na(isx_data[[v]]))
      }
    ),
    missing_n = purrr::map_int(
      variable,
      function(v) {
        if (!v %in% names(isx_data)) {
          return(NA_integer_)
        }
        sum(is.na(isx_data[[v]]))
      }
    ),
    missing_rate = missing_n / nrow(isx_data)
  )

scoring_audit <- tibble::tribble(
  ~component, ~input_variable, ~scoring_rule, ~interpretation,
  "ever-sex support", "H1CO1 and downstream H1CO behavior variables", "H1CO1 == 1 or any downstream behavior item observed implies ever-sex evidence; absence of such evidence is treated as operational never-sex/not-routed.", "Used to assign score 4 for respondents operationally classified as never having had sex.",
  "isx1", "H1CO2Y + age_wave1 + assumed Wave I survey year 1995", "Approximate age at first sex = first sex year - (1995 - age at Wave I). Score 4 if never sex; 3 if age >=18; 2 if 16-17; 1 if <=15.", "Higher score means later or no sexual initiation.",
  "isx3", "H1CO9", "Score 4 if never sex; among sexually active respondents: H1CO9=1 -> 3; H1CO9=2 or 3 -> 2; H1CO9=4 or 5 -> 1.", "Higher score means more consistent condom use.",
  "isx3", "H1CO3, H1CO6, H1CO8, H1CO13", "If H1CO9 is unavailable but any condom/birth-control indicator is present, assign method proxy score 3.", "Proxy for protective method behavior when frequency is unavailable.",
  "restricted index", "isx1_timing_score and isx3_method_score", "Primary index is the mean of available components if at least one component is valid; strict index requires both components.", "Higher index means higher behavioral sexual protection."
)

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Index type", "A restricted ISX-equivalent sexual protection behavior index is constructed because public-use local files did not provide defensible behavioral variables for partner exposure and sexual frequency.",
  "Dependent variable", "The constructed restricted ISX index is intended as the dependent variable in later models.",
  "Included dimensions", "The restricted index includes sexual initiation/timing and condom/contraceptive/birth-control behavior.",
  "Excluded dimensions", "Partner exposure and sexual frequency are not included because Script 18a v5 found no defensible behavioral candidates.",
  "Ever-sex handling", "Respondents with H1CO1 == 1 or downstream sexual behavior evidence are treated as ever having had sex. Respondents without such evidence are treated as operational never-sex/not-routed for scoring purposes.",
  "Operational caution", "The never-sex classification is an operational skip-pattern inference, not an observed explicit zero response.",
  "Timing approximation", "Age at first sex is approximated using first-sex year, Wave I age, and assumed survey year 1995. Implausible ages below 10 or above current age are set to missing.",
  "Method-use scoring", "Condom frequency is preferred when available. If condom frequency is unavailable, observed condom/birth-control indicators are used as a proxy for protective method behavior.",
  "Primary versus strict index", "The primary index uses at least one valid component; the strict index requires both components and is provided for sensitivity analysis.",
  "Data protection", "The row-level index file is marked LOCAL_ONLY and should not be committed to GitHub."
)

# Optional two-item reliability among cases with both components.
complete_two_component <- isx_data %>%
  filter(
    !is.na(isx1_timing_score),
    !is.na(isx3_method_score)
  )

if (nrow(complete_two_component) >= 10) {
  item_correlation <- suppressWarnings(
    cor(
      complete_two_component$isx1_timing_score,
      complete_two_component$isx3_method_score,
      use = "complete.obs"
    )
  )

  two_item_alpha <- ifelse(
    is.na(item_correlation) || item_correlation <= -1,
    NA_real_,
    (2 * item_correlation) / (1 + item_correlation)
  )
} else {
  item_correlation <- NA_real_
  two_item_alpha <- NA_real_
}

reliability_summary <- tibble(
  metric = c(
    "complete_two_component_n",
    "pearson_correlation_between_components",
    "two_item_alpha"
  ),
  value = c(
    nrow(complete_two_component),
    item_correlation,
    two_item_alpha
  )
)

readr::write_csv(
  variable_source_audit,
  file.path(audit_dir, "script18b_variable_source_audit.csv")
)

readr::write_csv(
  optional_variable_audit,
  file.path(audit_dir, "script18b_optional_variable_audit.csv")
)

readr::write_csv(
  index_summary,
  file.path(audit_dir, "script18b_restricted_isx_index_summary.csv")
)

readr::write_csv(
  component_distribution,
  file.path(audit_dir, "script18b_restricted_isx_component_distribution.csv")
)

readr::write_csv(
  missingness_summary,
  file.path(audit_dir, "script18b_restricted_isx_missingness_summary.csv")
)

readr::write_csv(
  scoring_audit,
  file.path(audit_dir, "script18b_restricted_isx_scoring_audit.csv")
)

readr::write_csv(
  reliability_summary,
  file.path(audit_dir, "script18b_restricted_isx_reliability_summary.csv")
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18b_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 14. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_restricted_isx_sexual_protection_index_script18b.docx"
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
      "Add Health Wave I — Restricted ISX-Equivalent Sexual Protection Index",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18b constructs a restricted behavioral sexual protection index to serve as the dependent variable in later models. The index is restricted because the public-use local files did not provide defensible behavioral variables for partner exposure or sexual frequency.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Index summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(index_summary)) %>%
    officer::body_add_par("Component distribution", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(component_distribution)) %>%
    officer::body_add_par("Reliability summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(reliability_summary)) %>%
    officer::body_add_par("Missingness summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(missingness_summary)) %>%
    officer::body_add_par("Variable source audit", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(variable_source_audit)) %>%
    officer::body_add_par("Scoring audit", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(scoring_audit)) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(methodological_decisions))

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 15. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "v4_best_source_loaded",
    "v5_feasibility_loaded",
    "restricted_isx_variables_recovered",
    "age_variable_recovered",
    "row_level_local_only_index_created",
    "index_summary_created",
    "component_distribution_created",
    "missingness_summary_created",
    "scoring_audit_created",
    "reliability_summary_created",
    "methodological_decisions_created",
    "word_report_created",
    "ready_for_script18c_predictor_blocks"
  ),
  status = c(
    file.exists(h1co_best_source_path),
    file.exists(v5_feasibility_path),
    all(
      c("H1CO1", "H1CO2Y", "H1CO2M", "H1CO9") %in%
        variable_source_audit$variable[variable_source_audit$recovered]
    ),
    age_recovery$audit$recovered[1],
    file.exists(row_level_index_path),
    file.exists(file.path(audit_dir, "script18b_restricted_isx_index_summary.csv")),
    file.exists(file.path(audit_dir, "script18b_restricted_isx_component_distribution.csv")),
    file.exists(file.path(audit_dir, "script18b_restricted_isx_missingness_summary.csv")),
    file.exists(file.path(audit_dir, "script18b_restricted_isx_scoring_audit.csv")),
    file.exists(file.path(audit_dir, "script18b_restricted_isx_reliability_summary.csv")),
    file.exists(file.path(audit_dir, "script18b_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    sum(!is.na(isx_data$restricted_isx_index_primary_1_4)) > 0
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18b_final_status.csv")
)

# ------------------------------------------------------------
# 16. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18b completed: Restricted ISX Sexual Protection Index\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nIndex summary:\n")
print(index_summary)

cat("\nComponent distribution:\n")
print(component_distribution)

cat("\nMissingness summary:\n")
print(missingness_summary)

cat("\nReliability summary:\n")
print(reliability_summary)

cat("\nVariable source audit:\n")
print(
  variable_source_audit %>%
    select(
      variable,
      recovered,
      variable_label_effective,
      source_valid_n,
      source_distinct_valid_n,
      file_name,
      object_name,
      note
    ),
  n = 50
)

cat("\nOptional variable audit:\n")
print(optional_variable_audit, n = 20)

cat("\nMethodological decisions:\n")
print(methodological_decisions)

cat("\nOutputs created:\n")
cat("- ", row_level_index_path, "\n")
cat("- ", file.path(audit_dir, "script18b_variable_source_audit.csv"), "\n")
cat("- ", file.path(audit_dir, "script18b_optional_variable_audit.csv"), "\n")
cat("- ", file.path(audit_dir, "script18b_restricted_isx_index_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18b_restricted_isx_component_distribution.csv"), "\n")
cat("- ", file.path(audit_dir, "script18b_restricted_isx_missingness_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18b_restricted_isx_scoring_audit.csv"), "\n")
cat("- ", file.path(audit_dir, "script18b_restricted_isx_reliability_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18b_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18b_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nImportant Git note:\n")
cat("Do not commit the LOCAL_ONLY row-level index file.\n")
cat("Review outputs before moving to Script 18c.\n")