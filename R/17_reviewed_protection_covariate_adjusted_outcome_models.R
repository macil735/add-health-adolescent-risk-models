# ============================================================
# Script 17 — Reviewed Protection Index in Manually Selected
#             Outcome Models
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Use reviewed_protection_index from Script 16 in outcome models
#   for manually selected adolescent sexual health diagnosis outcomes.
#
# Methodological position:
#   - Outcomes are not selected automatically in this script.
#   - Outcomes are read from the completed manual selection file
#     created by Scripts 17a and 17b.
#   - The selected outcomes are rare sexual health diagnosis outcomes.
#   - Weighted quasibinomial models are estimated as diagnostics only,
#     because they produced numerical instability for these rare outcomes.
#   - Unweighted binomial logistic models are retained as the primary
#     stable modelling specification.
#   - Results are descriptive adjusted associations, not causal estimates.
#
# Main inputs:
#   outputs/indices/script16_reviewed_protection_index.csv
#   outputs/audits/script17a_manual_outcome_selection_COMPLETED.csv
#
# Main outputs:
#   outputs/audits/script17_selected_outcomes.csv
#   outputs/audits/script17_outcome_variable_recovery_registry.csv
#   outputs/audits/script17_outcome_join_check.csv
#   outputs/models/script17_model_results_all_terms.csv
#   outputs/models/script17_model_results_focal_effects.csv
#   outputs/models/script17_model_fit_summary.csv
#   outputs/models/script17_model_sample_summary.csv
#   docs/add_health_wave01_reviewed_protection_outcome_models_script17.docx
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
  "purrr",
  "stats"
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
index_dir <- file.path(project_root, "outputs", "indices")
model_dir <- file.path(project_root, "outputs", "models")
doc_dir <- file.path(project_root, "docs")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 17 started: Reviewed Protection Outcome Models\n")
cat("============================================================\n\n")
cat("Project root:\n", project_root, "\n\n")

# ------------------------------------------------------------
# 2. Load reviewed protection index
# ------------------------------------------------------------

index_path <- file.path(
  index_dir,
  "script16_reviewed_protection_index.csv"
)

if (!file.exists(index_path)) {
  stop(
    "Reviewed protection index file not found:\n",
    index_path,
    "\nRun Script 16 before Script 17."
  )
}

index_data <- readr::read_csv(
  index_path,
  show_col_types = FALSE
)

required_index_columns <- c(
  "respondent_id",
  "reviewed_protection_index"
)

missing_index_columns <- setdiff(required_index_columns, names(index_data))

if (length(missing_index_columns) > 0) {
  stop(
    "The Script 16 index file is missing required columns: ",
    paste(missing_index_columns, collapse = ", ")
  )
}

cat("Reviewed protection index loaded:\n")
cat(index_path, "\n\n")
cat("Rows in index file:", nrow(index_data), "\n")
cat("Valid reviewed_protection_index:", sum(!is.na(index_data$reviewed_protection_index)), "\n\n")

# ------------------------------------------------------------
# 3. Load completed manual outcome selection
# ------------------------------------------------------------

manual_selection_path <- file.path(
  audit_dir,
  "script17a_manual_outcome_selection_COMPLETED.csv"
)

if (!file.exists(manual_selection_path)) {
  stop(
    "Completed manual outcome selection file not found:\n",
    manual_selection_path,
    "\nRun Scripts 17a and 17b before Script 17."
  )
}

manual_selection <- readr::read_csv(
  manual_selection_path,
  show_col_types = FALSE
)

required_manual_columns <- c(
  "manual_use_in_script17",
  "manual_outcome_domain",
  "manual_event_code",
  "manual_non_event_code",
  "manual_outcome_label",
  "manual_decision_rationale",
  "variable",
  "variable_label",
  "value_labels"
)

missing_manual_columns <- setdiff(required_manual_columns, names(manual_selection))

if (length(missing_manual_columns) > 0) {
  stop(
    "The completed manual outcome selection file is missing required columns: ",
    paste(missing_manual_columns, collapse = ", ")
  )
}

selected_outcome_spec <- manual_selection %>%
  mutate(
    manual_use_in_script17 = stringr::str_to_lower(
      stringr::str_squish(as.character(manual_use_in_script17))
    ),
    variable = as.character(variable),
    manual_event_code = as.character(manual_event_code),
    manual_non_event_code = as.character(manual_non_event_code)
  ) %>%
  filter(manual_use_in_script17 == "yes") %>%
  distinct(variable, .keep_all = TRUE) %>%
  arrange(variable)

if (nrow(selected_outcome_spec) == 0) {
  stop(
    "No outcomes are selected for Script 17 in:\n",
    manual_selection_path
  )
}

selected_outcome_variables <- selected_outcome_spec$variable

cat("Completed manual outcome selection loaded:\n")
cat(manual_selection_path, "\n\n")
cat("Outcomes selected for modelling:", nrow(selected_outcome_spec), "\n")
cat(paste(selected_outcome_variables, collapse = ", "), "\n\n")

readr::write_csv(
  selected_outcome_spec,
  file.path(audit_dir, "script17_manual_outcome_selection_used.csv")
)

# ------------------------------------------------------------
# 4. Helper functions
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

to_numeric_safe <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- as.numeric(x)
  }
  suppressWarnings(as.numeric(as.character(x)))
}

sd_safe <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) {
    return(NA_real_)
  }
  sd(x)
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

variable_label_string <- function(x) {

  lbl <- attr(x, "label", exact = TRUE)

  if (is.null(lbl)) {
    return("")
  }

  as.character(lbl)
}

extract_missing_codes_from_labels <- function(value_labels) {

  value_labels <- stringr::str_to_lower(clean_chr(value_labels))

  standard_high_missing <- c(
    6, 7, 8, 9,
    96, 97, 98, 99,
    996, 997, 998, 999,
    9996, 9997, 9998, 9999
  )

  if (is.na(value_labels) || value_labels == "") {
    return(standard_high_missing)
  }

  parts <- unlist(stringr::str_split(value_labels, ";|\\||\\n"))

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

parse_outcome_numeric <- function(x) {

  if (inherits(x, "haven_labelled")) {
    return(as.numeric(x))
  }

  if (is.numeric(x) || is.integer(x)) {
    return(as.numeric(x))
  }

  x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))

  out <- suppressWarnings(as.numeric(x_chr))

  out[is.na(out) & stringr::str_detect(x_chr, "\\bno\\b|never|not diagnosed")] <- 0
  out[is.na(out) & stringr::str_detect(x_chr, "\\byes\\b|diagnosed|ever diagnosed")] <- 1

  extracted <- suppressWarnings(
    as.numeric(stringr::str_extract(x_chr, "-?\\d+(\\.\\d+)?"))
  )

  out[is.na(out) & !is.na(extracted)] <- extracted[is.na(out) & !is.na(extracted)]

  out
}

binary_recode_manual <- function(x, event_code, non_event_code, value_labels) {

  x_num <- parse_outcome_numeric(x)

  event_num <- suppressWarnings(as.numeric(event_code))
  non_event_num <- suppressWarnings(as.numeric(non_event_code))

  if (is.na(event_num) || is.na(non_event_num)) {
    return(rep(NA_real_, length(x_num)))
  }

  missing_codes <- extract_missing_codes_from_labels(value_labels)
  x_num[x_num %in% missing_codes] <- NA_real_

  out <- rep(NA_real_, length(x_num))
  out[!is.na(x_num) & x_num == event_num] <- 1
  out[!is.na(x_num) & x_num == non_event_num] <- 0

  out
}

safe_factor <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == "" | x == "NaN"] <- "Missing"
  factor(x)
}

has_enough_factor_variation <- function(x, min_level_n = 20) {

  x <- droplevels(as.factor(x))
  tab <- table(x)

  length(tab) > 1 && all(tab >= min_level_n)
}

safe_exp <- function(x) {
  ifelse(is.finite(x) & abs(x) < 700, exp(x), NA_real_)
}

capture_glm <- function(formula, family, data, weights = NULL) {

  warning_messages <- character(0)

  fit <- withCallingHandlers(
    tryCatch(
      {
        if (is.null(weights)) {
          stats::glm(
            formula = formula,
            family = family,
            data = data
          )
        } else {
          stats::glm(
            formula = formula,
            family = family,
            weights = weights,
            data = data
          )
        }
      },
      error = function(e) e
    ),
    warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    fit = fit,
    warnings = unique(warning_messages)
  )
}

# ------------------------------------------------------------
# 5. Data file readers
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

id_candidates <- c(
  "AID",
  "aid",
  "CASEID",
  "caseid",
  "RESPID",
  "respid",
  "respondent_id",
  "id"
)

# ------------------------------------------------------------
# 6. Locate respondent-level data files
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

if (length(candidate_data_files) == 0) {
  stop(
    "No respondent-level candidate data files were found.\n",
    "Copy the local data folder into the project directory, but do not commit it to GitHub."
  )
}

cat("Candidate respondent-level data files found:", length(candidate_data_files), "\n\n")

# ------------------------------------------------------------
# 7. Recover manually selected outcome variables
# ------------------------------------------------------------

index_id_values <- unique(as.character(index_data$respondent_id))

choose_best_id_alignment <- function(df, vars_here, id_candidates, index_id_values) {

  candidates <- list()

  id_here <- intersect(id_candidates, names(df))

  if (length(id_here) > 0) {

    id_var <- id_here[1]

    temp_id <- df %>%
      select(all_of(id_var), all_of(vars_here)) %>%
      mutate(respondent_id = as.character(.data[[id_var]])) %>%
      select(respondent_id, all_of(vars_here)) %>%
      distinct(respondent_id, .keep_all = TRUE)

    candidates[["explicit_id"]] <- list(
      data = temp_id,
      source_id_variable = id_var,
      alignment_method = "explicit_id",
      overlap_n = sum(temp_id$respondent_id %in% index_id_values)
    )
  }

  temp_row <- df %>%
    select(all_of(vars_here)) %>%
    mutate(respondent_id = as.character(row_number())) %>%
    select(respondent_id, all_of(vars_here)) %>%
    distinct(respondent_id, .keep_all = TRUE)

  candidates[["row_number"]] <- list(
    data = temp_row,
    source_id_variable = ".row_id_created",
    alignment_method = "row_number",
    overlap_n = sum(temp_row$respondent_id %in% index_id_values)
  )

  overlap_values <- purrr::map_int(candidates, "overlap_n")
  best_name <- names(which.max(overlap_values))

  candidates[[best_name]]
}

outcome_data <- NULL

outcome_registry <- tibble(
  variable = character(),
  source_file = character(),
  object_name = character(),
  source_id_variable = character(),
  alignment_method = character(),
  overlap_with_index_n = integer(),
  variable_label_from_data = character(),
  value_labels_from_data = character()
)

recovered_outcomes <- character(0)

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

    needed_here <- setdiff(selected_outcome_variables, recovered_outcomes)
    vars_here <- intersect(needed_here, names(df))

    if (length(vars_here) == 0) {
      next
    }

    best_alignment <- choose_best_id_alignment(
      df = df,
      vars_here = vars_here,
      id_candidates = id_candidates,
      index_id_values = index_id_values
    )

    temp <- best_alignment$data

    if (is.null(outcome_data)) {
      outcome_data <- temp
    } else {
      outcome_data <- outcome_data %>%
        full_join(temp, by = "respondent_id")
    }

    outcome_registry <- bind_rows(
      outcome_registry,
      tibble(
        variable = vars_here,
        source_file = fp,
        object_name = obj_name,
        source_id_variable = best_alignment$source_id_variable,
        alignment_method = best_alignment$alignment_method,
        overlap_with_index_n = best_alignment$overlap_n,
        variable_label_from_data = purrr::map_chr(vars_here, ~ variable_label_string(df[[.x]])),
        value_labels_from_data = purrr::map_chr(vars_here, ~ value_label_string(df[[.x]]))
      )
    )

    recovered_outcomes <- unique(c(recovered_outcomes, vars_here))
  }
}

if (is.null(outcome_data) || nrow(outcome_data) == 0) {
  stop("No manually selected outcome variables could be recovered.")
}

missing_selected_outcomes <- setdiff(selected_outcome_variables, names(outcome_data))

if (length(missing_selected_outcomes) > 0) {
  stop(
    "The following manually selected outcomes were not recovered from respondent-level data: ",
    paste(missing_selected_outcomes, collapse = ", ")
  )
}

readr::write_csv(
  outcome_registry,
  file.path(audit_dir, "script17_outcome_variable_recovery_registry.csv")
)

cat("Outcome recovery registry:\n")
print(outcome_registry)

# ------------------------------------------------------------
# 8. Merge outcomes with reviewed protection index
# ------------------------------------------------------------

model_base_id_join <- index_data %>%
  mutate(respondent_id = as.character(respondent_id)) %>%
  left_join(
    outcome_data %>%
      mutate(respondent_id = as.character(respondent_id)),
    by = "respondent_id"
  )

outcome_join_check_id <- tibble(
  outcome_variable = selected_outcome_variables,
  non_missing_after_join = purrr::map_int(
    selected_outcome_variables,
    ~ sum(!is.na(model_base_id_join[[.x]]))
  ),
  join_method = "respondent_id"
)

if (
  all(outcome_join_check_id$non_missing_after_join == 0) &&
    nrow(outcome_data) == nrow(index_data)
) {

  model_base <- index_data %>%
    mutate(.row_join_id = row_number()) %>%
    left_join(
      outcome_data %>%
        mutate(.row_join_id = row_number()) %>%
        select(-respondent_id),
      by = ".row_join_id"
    ) %>%
    select(-.row_join_id)

  outcome_join_method_used <- "row_position_fallback"

} else {

  model_base <- model_base_id_join
  outcome_join_method_used <- "respondent_id"

}

outcome_join_check <- tibble(
  outcome_variable = selected_outcome_variables,
  non_missing_after_join = purrr::map_int(
    selected_outcome_variables,
    ~ sum(!is.na(model_base[[.x]]))
  ),
  join_method = outcome_join_method_used
)

readr::write_csv(
  bind_rows(
    outcome_join_check_id,
    outcome_join_check
  ),
  file.path(audit_dir, "script17_outcome_join_check.csv")
)

cat("\nOutcome join check:\n")
print(outcome_join_check)

if (all(outcome_join_check$non_missing_after_join == 0)) {
  stop(
    "Selected outcomes were recovered but did not align with the reviewed protection index. ",
    "Both respondent_id join and row-position fallback failed. ",
    "Inspect script17_outcome_variable_recovery_registry.csv and script17_outcome_join_check.csv."
  )
}

# ------------------------------------------------------------
# 9. Prepare covariates and weights
# ------------------------------------------------------------

if (!"survey_weight" %in% names(model_base)) {
  model_base <- model_base %>%
    mutate(survey_weight = NA_real_)
}

model_base <- model_base %>%
  mutate(
    survey_weight = to_numeric_safe(survey_weight),
    survey_weight = ifelse(is.na(survey_weight) | survey_weight <= 0, 1, survey_weight)
  )

if ("a_age_wave1" %in% names(model_base)) {
  model_base <- model_base %>%
    mutate(age_wave1_numeric = to_numeric_safe(a_age_wave1))
} else {
  model_base <- model_base %>%
    mutate(age_wave1_numeric = NA_real_)
}

if (!"sex_gender_clean" %in% names(model_base)) {
  model_base <- model_base %>%
    mutate(sex_gender_clean = NA_character_)
}

if (!"school_grade_clean" %in% names(model_base)) {
  model_base <- model_base %>%
    mutate(school_grade_clean = NA_character_)
}

if (!"residence_context_model" %in% names(model_base)) {
  model_base <- model_base %>%
    mutate(residence_context_model = NA_character_)
}

# ------------------------------------------------------------
# 10. Recode manually selected outcomes
# ------------------------------------------------------------

selected_outcomes_list <- list()

for (i in seq_len(nrow(selected_outcome_spec))) {

  spec <- selected_outcome_spec[i, ]

  var <- spec$variable
  outcome_binary_var <- paste0("outcome_binary_", var)

  x_bin <- binary_recode_manual(
    x = model_base[[var]],
    event_code = spec$manual_event_code,
    non_event_code = spec$manual_non_event_code,
    value_labels = spec$value_labels
  )

  model_base[[outcome_binary_var]] <- x_bin

  valid_n <- sum(!is.na(x_bin))
  event_n <- sum(x_bin == 1, na.rm = TRUE)
  non_event_n <- sum(x_bin == 0, na.rm = TRUE)
  event_rate <- ifelse(valid_n > 0, event_n / valid_n, NA_real_)

  selected_outcomes_list[[i]] <- tibble(
    outcome_variable = var,
    outcome_label = spec$manual_outcome_label,
    original_variable_label = spec$variable_label,
    value_labels = spec$value_labels,
    outcome_domain = spec$manual_outcome_domain,
    event_code = spec$manual_event_code,
    non_event_code = spec$manual_non_event_code,
    valid_n = valid_n,
    event_n = event_n,
    non_event_n = non_event_n,
    event_rate = event_rate,
    manual_decision_rationale = spec$manual_decision_rationale
  )
}

selected_outcomes <- bind_rows(selected_outcomes_list) %>%
  arrange(outcome_variable) %>%
  mutate(
    model_eligible = valid_n >= 500 &
      event_n >= 20 &
      non_event_n >= 20 &
      !is.na(event_rate) &
      event_rate > 0.005 &
      event_rate < 0.995,
    model_eligibility_note = case_when(
      model_eligible ~ "eligible_for_logistic_model",
      valid_n < 500 ~ "not_eligible_low_valid_n",
      event_n < 20 ~ "not_eligible_too_few_events",
      non_event_n < 20 ~ "not_eligible_too_few_non_events",
      event_rate <= 0.005 | event_rate >= 0.995 ~ "not_eligible_extreme_event_rate",
      TRUE ~ "not_eligible_other"
    )
  )

readr::write_csv(
  selected_outcomes,
  file.path(audit_dir, "script17_selected_outcomes.csv")
)

eligible_outcomes <- selected_outcomes %>%
  filter(model_eligible)

if (nrow(eligible_outcomes) == 0) {
  cat("\nSelected outcomes after manual recoding:\n")
  print(selected_outcomes)

  stop(
    "No manually selected outcomes passed model eligibility checks.\n",
    "Inspect outputs/audits/script17_selected_outcomes.csv."
  )
}

cat("\nSelected outcomes loaded:", nrow(selected_outcomes), "\n")
cat("Model-eligible outcomes:", nrow(eligible_outcomes), "\n\n")
print(selected_outcomes)

# ------------------------------------------------------------
# 11. Model fitting helpers
# ------------------------------------------------------------

prepare_model_data <- function(data, outcome_binary_var) {

  df <- data %>%
    transmute(
      model_outcome = .data[[outcome_binary_var]],
      reviewed_protection_index = reviewed_protection_index,
      sex_gender_clean = safe_factor(sex_gender_clean),
      age_wave1_numeric = age_wave1_numeric,
      school_grade_clean = safe_factor(school_grade_clean),
      residence_context_model = safe_factor(residence_context_model),
      survey_weight = survey_weight
    ) %>%
    filter(
      !is.na(model_outcome),
      !is.na(reviewed_protection_index)
    )

  df$sex_gender_clean <- droplevels(df$sex_gender_clean)
  df$school_grade_clean <- droplevels(df$school_grade_clean)
  df$residence_context_model <- droplevels(df$residence_context_model)

  if ("Male" %in% levels(df$sex_gender_clean)) {
    df$sex_gender_clean <- stats::relevel(df$sex_gender_clean, ref = "Male")
  }

  if ("Rural" %in% levels(df$residence_context_model)) {
    df$residence_context_model <- stats::relevel(df$residence_context_model, ref = "Rural")
  }

  df
}

build_model_formula <- function(df, model_spec = "full") {

  predictors <- c("reviewed_protection_index")

  if (model_spec %in% c("basic_adjusted", "full")) {

    if (has_enough_factor_variation(df$sex_gender_clean, min_level_n = 20)) {
      predictors <- c(predictors, "sex_gender_clean")
    }

    if (
      sum(!is.na(df$age_wave1_numeric)) > 10 &&
        sd_safe(df$age_wave1_numeric) > 0
    ) {
      predictors <- c(predictors, "age_wave1_numeric")
    }
  }

  if (model_spec == "full") {

    if (has_enough_factor_variation(df$school_grade_clean, min_level_n = 20)) {
      predictors <- c(predictors, "school_grade_clean")
    }

    if (has_enough_factor_variation(df$residence_context_model, min_level_n = 20)) {
      predictors <- c(predictors, "residence_context_model")
    }
  }

  stats::as.formula(
    paste("model_outcome ~", paste(predictors, collapse = " + "))
  )
}

extract_model_results <- function(fit,
                                  outcome_variable,
                                  outcome_label,
                                  outcome_domain,
                                  model_spec,
                                  model_engine,
                                  n_model,
                                  event_n,
                                  non_event_n,
                                  model_warning) {

  sm <- summary(fit)
  coef_tab <- as.data.frame(sm$coefficients)
  coef_tab$term <- rownames(coef_tab)
  rownames(coef_tab) <- NULL

  names(coef_tab) <- stringr::str_replace_all(names(coef_tab), " ", "_")
  names(coef_tab) <- stringr::str_replace_all(names(coef_tab), "\\(|\\)|>|\\|", "")
  names(coef_tab) <- stringr::str_to_lower(names(coef_tab))

  estimate_col <- names(coef_tab)[stringr::str_detect(names(coef_tab), "^estimate$")]
  se_col <- names(coef_tab)[stringr::str_detect(names(coef_tab), "std")]
  p_col <- names(coef_tab)[stringr::str_detect(names(coef_tab), "^pr")]

  if (length(estimate_col) == 0 || length(se_col) == 0) {
    return(tibble())
  }

  if (length(p_col) == 0) {
    coef_tab$p_value <- NA_real_
    p_col <- "p_value"
  }

  out <- coef_tab %>%
    transmute(
      outcome_variable = outcome_variable,
      outcome_label = outcome_label,
      outcome_domain = outcome_domain,
      model_spec = model_spec,
      model_engine = model_engine,
      n_model = n_model,
      event_n = event_n,
      non_event_n = non_event_n,
      term = term,
      estimate = .data[[estimate_col[1]]],
      std_error = .data[[se_col[1]]],
      p_value = .data[[p_col[1]]]
    ) %>%
    mutate(
      coefficient_extreme_or_nonfinite =
        !is.finite(estimate) |
        !is.finite(std_error) |
        abs(estimate) > 25 |
        abs(std_error) > 100,
      focal_effect_retained =
        term == "reviewed_protection_index" &
        model_engine == "unweighted_binomial_primary" &
        !coefficient_extreme_or_nonfinite,
      odds_ratio = ifelse(
        coefficient_extreme_or_nonfinite,
        NA_real_,
        safe_exp(estimate)
      ),
      ci_low = ifelse(
        coefficient_extreme_or_nonfinite,
        NA_real_,
        safe_exp(estimate - 1.96 * std_error)
      ),
      ci_high = ifelse(
        coefficient_extreme_or_nonfinite,
        NA_real_,
        safe_exp(estimate + 1.96 * std_error)
      ),
      odds_ratio_per_0_10_index = ifelse(
        term == "reviewed_protection_index" & !coefficient_extreme_or_nonfinite,
        safe_exp(estimate * 0.10),
        NA_real_
      ),
      ci_low_per_0_10_index = ifelse(
        term == "reviewed_protection_index" & !coefficient_extreme_or_nonfinite,
        safe_exp((estimate - 1.96 * std_error) * 0.10),
        NA_real_
      ),
      ci_high_per_0_10_index = ifelse(
        term == "reviewed_protection_index" & !coefficient_extreme_or_nonfinite,
        safe_exp((estimate + 1.96 * std_error) * 0.10),
        NA_real_
      ),
      model_warning = case_when(
        coefficient_extreme_or_nonfinite ~
          "coefficient_extreme_or_nonfinite_not_retained_for_or_interpretation",
        model_warning != "none" ~ model_warning,
        TRUE ~ "none"
      )
    )

  out
}

fit_one_model <- function(outcome_row, data, model_engine) {

  outcome_variable <- outcome_row$outcome_variable
  outcome_binary_var <- paste0("outcome_binary_", outcome_variable)

  df <- prepare_model_data(data, outcome_binary_var)

  n_model <- nrow(df)
  event_n <- sum(df$model_outcome == 1, na.rm = TRUE)
  non_event_n <- sum(df$model_outcome == 0, na.rm = TRUE)

  if (n_model < 500 || event_n < 20 || non_event_n < 20) {
    return(list(
      model_results = tibble(),
      fit_summary = tibble(
        outcome_variable = outcome_variable,
        outcome_label = outcome_row$outcome_label,
        outcome_domain = outcome_row$outcome_domain,
        model_spec = outcome_row$model_spec,
        model_engine = model_engine,
        model_status = "not_estimated_insufficient_sample_or_events",
        n_model = n_model,
        event_n = event_n,
        non_event_n = non_event_n,
        event_rate = ifelse(n_model > 0, event_n / n_model, NA_real_),
        formula = NA_character_,
        converged = NA,
        focal_effect_retained = FALSE,
        null_deviance = NA_real_,
        residual_deviance = NA_real_,
        aic = NA_real_,
        model_warning = "sample_or_event_threshold_failed"
      )
    ))
  }

  fml <- build_model_formula(
    df = df,
    model_spec = outcome_row$model_spec
  )

  if (model_engine == "unweighted_binomial_primary") {

    glm_result <- capture_glm(
      formula = fml,
      family = stats::binomial(link = "logit"),
      data = df,
      weights = NULL
    )

  } else if (model_engine == "weighted_quasibinomial_diagnostic") {

    glm_result <- capture_glm(
      formula = fml,
      family = stats::quasibinomial(link = "logit"),
      data = df,
      weights = df$survey_weight
    )

  } else {
    stop("Unknown model_engine: ", model_engine)
  }

  fit <- glm_result$fit
  warning_messages <- glm_result$warnings

  if (inherits(fit, "error")) {
    return(list(
      model_results = tibble(),
      fit_summary = tibble(
        outcome_variable = outcome_variable,
        outcome_label = outcome_row$outcome_label,
        outcome_domain = outcome_row$outcome_domain,
        model_spec = outcome_row$model_spec,
        model_engine = model_engine,
        model_status = paste0("model_error: ", fit$message),
        n_model = n_model,
        event_n = event_n,
        non_event_n = non_event_n,
        event_rate = event_n / n_model,
        formula = paste(deparse(fml), collapse = " "),
        converged = NA,
        focal_effect_retained = FALSE,
        null_deviance = NA_real_,
        residual_deviance = NA_real_,
        aic = NA_real_,
        model_warning = "model_error"
      )
    ))
  }

  base_warning <- ifelse(
    length(warning_messages) > 0,
    paste(warning_messages, collapse = " | "),
    "none"
  )

  if (!isTRUE(fit$converged)) {
    base_warning <- paste(
      unique(c(base_warning, "glm_did_not_report_convergence")),
      collapse = " | "
    )
  }

  model_results <- extract_model_results(
    fit = fit,
    outcome_variable = outcome_variable,
    outcome_label = outcome_row$outcome_label,
    outcome_domain = outcome_row$outcome_domain,
    model_spec = outcome_row$model_spec,
    model_engine = model_engine,
    n_model = n_model,
    event_n = event_n,
    non_event_n = non_event_n,
    model_warning = base_warning
  )

  focal_row <- model_results %>%
    filter(term == "reviewed_protection_index")

  focal_effect_retained <- any(focal_row$focal_effect_retained, na.rm = TRUE)

  model_status <- case_when(
    model_engine == "weighted_quasibinomial_diagnostic" &
      any(focal_row$coefficient_extreme_or_nonfinite, na.rm = TRUE) ~
      "estimated_diagnostic_weighted_focal_unstable",
    model_engine == "weighted_quasibinomial_diagnostic" ~
      "estimated_diagnostic_weighted",
    model_engine == "unweighted_binomial_primary" &
      focal_effect_retained ~
      "estimated_primary_focal_retained",
    model_engine == "unweighted_binomial_primary" &
      !focal_effect_retained ~
      "estimated_primary_focal_not_retained",
    TRUE ~
      "estimated"
  )

  fit_summary <- tibble(
    outcome_variable = outcome_variable,
    outcome_label = outcome_row$outcome_label,
    outcome_domain = outcome_row$outcome_domain,
    model_spec = outcome_row$model_spec,
    model_engine = model_engine,
    model_status = model_status,
    n_model = n_model,
    event_n = event_n,
    non_event_n = non_event_n,
    event_rate = event_n / n_model,
    formula = paste(deparse(fml), collapse = " "),
    converged = isTRUE(fit$converged),
    focal_effect_retained = focal_effect_retained,
    null_deviance = fit$null.deviance,
    residual_deviance = fit$deviance,
    aic = fit$aic,
    model_warning = ifelse(base_warning == "", "none", base_warning)
  )

  list(
    model_results = model_results,
    fit_summary = fit_summary
  )
}

# ------------------------------------------------------------
# 12. Estimate primary and diagnostic models
# ------------------------------------------------------------

model_spec_grid <- tidyr::crossing(
  eligible_outcomes,
  tibble(
    model_spec = c(
      "unadjusted_index_only",
      "basic_adjusted",
      "full"
    )
  )
)

model_engines <- c(
  "unweighted_binomial_primary",
  "weighted_quasibinomial_diagnostic"
)

model_grid <- tidyr::crossing(
  model_spec_grid,
  tibble(model_engine = model_engines)
)

model_outputs <- purrr::map(
  seq_len(nrow(model_grid)),
  function(i) {
    fit_one_model(
      outcome_row = model_grid[i, ],
      data = model_base,
      model_engine = model_grid$model_engine[i]
    )
  }
)

model_results_all_terms <- bind_rows(
  purrr::map(model_outputs, "model_results")
)

model_fit_summary <- bind_rows(
  purrr::map(model_outputs, "fit_summary")
)

if (nrow(model_fit_summary) == 0) {
  stop("No model fit summaries were produced.")
}

if (nrow(model_results_all_terms) == 0) {
  stop("No model term results were produced.")
}

model_results_focal <- model_results_all_terms %>%
  filter(
    term == "reviewed_protection_index",
    model_engine == "unweighted_binomial_primary",
    focal_effect_retained
  ) %>%
  mutate(
    focal_interpretation = case_when(
      is.na(odds_ratio_per_0_10_index) ~ "not estimated",
      odds_ratio_per_0_10_index < 1 ~
        "Higher reviewed protection is associated with lower unweighted adjusted odds of the outcome.",
      odds_ratio_per_0_10_index > 1 ~
        "Higher reviewed protection is associated with higher unweighted adjusted odds of the outcome.",
      TRUE ~ "No adjusted association detected."
    ),
    statistical_flag = case_when(
      !is.na(p_value) & p_value < 0.001 ~ "p < 0.001",
      !is.na(p_value) & p_value < 0.01 ~ "p < 0.01",
      !is.na(p_value) & p_value < 0.05 ~ "p < 0.05",
      TRUE ~ "not statistically flagged at 5%"
    )
  ) %>%
  arrange(outcome_variable, model_spec)

# ------------------------------------------------------------
# 13. Model sample summary
# ------------------------------------------------------------

model_sample_summary <- selected_outcomes %>%
  left_join(
    model_fit_summary %>%
      select(
        outcome_variable,
        model_spec,
        model_engine,
        model_status,
        n_model,
        event_n,
        non_event_n,
        event_rate,
        formula,
        converged,
        focal_effect_retained,
        model_warning
      ),
    by = "outcome_variable"
  ) %>%
  arrange(outcome_variable, model_engine, model_spec)

# ------------------------------------------------------------
# 14. Methodological decisions
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Main predictor", "reviewed_protection_index from Script 16 is used as the main predictor.",
  "Outcome selection", "Outcomes are read from the completed manual outcome selection file produced by Scripts 17a and 17b. Script 17 does not perform automatic outcome selection.",
  "Selected outcomes", "The current modelling step uses H1CO16A and H1CO16C, defined as self-reported diagnoses of chlamydia and gonorrhea.",
  "Outcome recoding", "Manual event and non-event codes are used. Code 1 is treated as the event and code 0 as the non-event for the selected outcomes.",
  "Outcome alignment", "Outcomes are first joined by respondent_id. If no respondent_id alignment is possible and row counts match, the script uses row-position fallback alignment and records this in script17_outcome_join_check.csv.",
  "Primary model engine", "Because the selected sexual health diagnosis outcomes are rare, unweighted binomial logistic regression is retained as the primary stable model engine.",
  "Weighted model diagnostic", "Weighted quasibinomial models are estimated only as diagnostics. They are not retained for focal interpretation when coefficients are extreme or non-finite.",
  "Covariate adjustment", "Three model specifications are estimated: index-only, basic adjustment for sex and age, and full adjustment including school grade and residence context when available.",
  "Rare outcomes", "Because diagnosis outcomes are rare, model stability checks flag extreme or non-finite coefficients and prevent odds ratio interpretation for unstable models.",
  "Interpretation", "Results are adjusted associations and should not be interpreted causally.",
  "Focal effect scaling", "The focal effect is reported as the odds ratio for a 0.10-point increase in the 0–1 reviewed_protection_index."
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script17_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 15. Save outputs
# ------------------------------------------------------------

readr::write_csv(
  model_results_all_terms,
  file.path(model_dir, "script17_model_results_all_terms.csv")
)

readr::write_csv(
  model_results_focal,
  file.path(model_dir, "script17_model_results_focal_effects.csv")
)

readr::write_csv(
  model_fit_summary,
  file.path(model_dir, "script17_model_fit_summary.csv")
)

readr::write_csv(
  model_sample_summary,
  file.path(model_dir, "script17_model_sample_summary.csv")
)

# ------------------------------------------------------------
# 16. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_reviewed_protection_outcome_models_script17.docx"
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

  focal_report <- model_results_focal %>%
    transmute(
      outcome_variable,
      outcome_domain,
      model_spec,
      model_engine,
      n_model,
      event_n,
      event_rate = round(event_n / n_model, 4),
      OR_0_10_index = round(odds_ratio_per_0_10_index, 3),
      CI_0_10 = paste0(
        "[",
        round(ci_low_per_0_10_index, 3),
        ", ",
        round(ci_high_per_0_10_index, 3),
        "]"
      ),
      p_value = signif(p_value, 3),
      statistical_flag,
      interpretation = focal_interpretation
    )

  if (nrow(focal_report) == 0) {
    focal_report <- tibble(
      note = "No stable focal reviewed_protection_index effects were retained after model stability checks."
    )
  }

  weighted_diagnostic_report <- model_fit_summary %>%
    filter(model_engine == "weighted_quasibinomial_diagnostic") %>%
    select(
      outcome_variable,
      model_spec,
      model_status,
      n_model,
      event_n,
      event_rate,
      converged,
      focal_effect_retained,
      model_warning
    )

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — Reviewed Protection Index and Manually Selected Outcomes",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 17 estimates outcome models using the reviewed_protection_index from Script 16 and the manually selected outcomes created by Scripts 17a and 17b. Because the selected diagnosis outcomes are rare, unweighted binomial logistic models are retained as the primary stable specification. Weighted quasibinomial models are reported only as diagnostics.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Selected outcomes", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(
        selected_outcomes %>%
          select(
            outcome_variable,
            outcome_label,
            outcome_domain,
            valid_n,
            event_n,
            non_event_n,
            event_rate,
            model_eligibility_note
          )
      )
    ) %>%
    officer::body_add_par("Outcome join check", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(outcome_join_check)
    ) %>%
    officer::body_add_par("Model fit summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(
        model_fit_summary %>%
          select(
            outcome_variable,
            model_spec,
            model_engine,
            model_status,
            n_model,
            event_n,
            non_event_n,
            event_rate,
            converged,
            focal_effect_retained,
            model_warning
          )
      )
    ) %>%
    officer::body_add_par("Primary focal adjusted associations", style = "heading 2") %>%
    officer::body_add_par(
      "The focal association is reported from the unweighted binomial primary model as the odds ratio for a 0.10-point increase in reviewed_protection_index.",
      style = "Normal"
    ) %>%
    flextable::body_add_flextable(
      value = make_ft(focal_report)
    ) %>%
    officer::body_add_par("Weighted model diagnostic", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(weighted_diagnostic_report)
    ) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(methodological_decisions)
    ) %>%
    officer::body_add_par("Interpretation note", style = "heading 2") %>%
    officer::body_add_par(
      "A focal odds ratio below 1 indicates that higher reviewed protection is associated with lower unweighted adjusted odds of the selected diagnosis outcome. A focal odds ratio above 1 indicates higher adjusted odds. These results are descriptive associations and should not be interpreted causally.",
      style = "Normal"
    )

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 17. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "reviewed_protection_index_loaded",
    "manual_outcome_selection_loaded",
    "candidate_data_files_found",
    "selected_outcomes_recovered",
    "selected_outcomes_created",
    "eligible_outcomes_available",
    "primary_models_estimated",
    "weighted_diagnostic_models_attempted",
    "primary_focal_effects_created",
    "model_fit_summary_created",
    "word_report_created"
  ),
  status = c(
    file.exists(index_path),
    file.exists(manual_selection_path),
    length(candidate_data_files) > 0,
    all(selected_outcome_variables %in% names(outcome_data)),
    file.exists(file.path(audit_dir, "script17_selected_outcomes.csv")),
    nrow(eligible_outcomes) > 0,
    nrow(model_fit_summary %>%
           filter(model_engine == "unweighted_binomial_primary")) > 0,
    nrow(model_fit_summary %>%
           filter(model_engine == "weighted_quasibinomial_diagnostic")) > 0,
    nrow(model_results_focal) > 0,
    file.exists(file.path(model_dir, "script17_model_fit_summary.csv")),
    !is.na(word_report_path) && file.exists(word_report_path)
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script17_final_status.csv")
)

# ------------------------------------------------------------
# 18. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 17 completed: Reviewed Protection Outcome Models\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nSelected outcomes:\n")
print(selected_outcomes)

cat("\nModel fit summary:\n")
print(
  model_fit_summary %>%
    select(
      outcome_variable,
      model_spec,
      model_engine,
      model_status,
      n_model,
      event_n,
      non_event_n,
      event_rate,
      converged,
      focal_effect_retained,
      model_warning
    )
)

cat("\nPrimary focal effects of reviewed_protection_index:\n")
print(
  model_results_focal %>%
    select(
      outcome_variable,
      model_spec,
      model_engine,
      estimate,
      std_error,
      p_value,
      odds_ratio_per_0_10_index,
      ci_low_per_0_10_index,
      ci_high_per_0_10_index,
      statistical_flag,
      focal_interpretation
    )
)

cat("\nWeighted diagnostic focal terms:\n")
print(
  model_results_all_terms %>%
    filter(
      term == "reviewed_protection_index",
      model_engine == "weighted_quasibinomial_diagnostic"
    ) %>%
    select(
      outcome_variable,
      model_spec,
      model_engine,
      estimate,
      std_error,
      p_value,
      model_warning
    )
)

cat("\nOutputs created:\n")
cat("- ", file.path(audit_dir, "script17_selected_outcomes.csv"), "\n")
cat("- ", file.path(audit_dir, "script17_manual_outcome_selection_used.csv"), "\n")
cat("- ", file.path(audit_dir, "script17_outcome_variable_recovery_registry.csv"), "\n")
cat("- ", file.path(audit_dir, "script17_outcome_join_check.csv"), "\n")
cat("- ", file.path(model_dir, "script17_model_results_all_terms.csv"), "\n")
cat("- ", file.path(model_dir, "script17_model_results_focal_effects.csv"), "\n")
cat("- ", file.path(model_dir, "script17_model_fit_summary.csv"), "\n")
cat("- ", file.path(model_dir, "script17_model_sample_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script17_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script17_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Review the primary focal effects and weighted diagnostic instability before committing.\n")
cat("Do not interpret results causally.\n")