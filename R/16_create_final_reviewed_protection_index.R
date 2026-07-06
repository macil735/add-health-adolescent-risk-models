# ============================================================
# Script 16 — Final Reviewed Protection Index Construction
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Construct the final reviewed protection index using the
#   completed item-direction review produced by Scripts 15 and 15b.
#
# Methodological position:
#   - Build a reviewed protection index from items manually or
#     semi-automatically retained as protection.
#   - Do not force a reviewed risk index unless enough items are
#     retained as defensible risk indicators.
#
# Main input:
#   outputs/audits/script15_manual_item_direction_review_COMPLETED.csv
#
# Main outputs:
#   outputs/indices/script16_reviewed_protection_index.csv
#   outputs/indices/script16_reviewed_protection_item_scores.csv
#   outputs/audits/script16_reviewed_index_item_audit.csv
#   outputs/audits/script16_reviewed_index_summary.csv
#   outputs/audits/script16_reviewed_index_by_covariates.csv
#   docs/add_health_wave01_final_reviewed_protection_index_script16.docx
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
doc_dir <- file.path(project_root, "docs")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 16 started: Final Reviewed Protection Index\n")
cat("============================================================\n\n")
cat("Project root:\n", project_root, "\n\n")

# ------------------------------------------------------------
# 2. Input: completed item-direction review
# ------------------------------------------------------------

completed_review_path <- file.path(
  audit_dir,
  "script15_manual_item_direction_review_COMPLETED.csv"
)

if (!file.exists(completed_review_path)) {
  stop(
    "Completed review file not found:\n",
    completed_review_path,
    "\nRun Scripts 15 and 15b, then create the COMPLETED review file before Script 16."
  )
}

review_completed <- readr::read_csv(
  completed_review_path,
  show_col_types = FALSE
)

cat("Completed item-direction review loaded:\n")
cat(completed_review_path, "\n\n")

required_review_columns <- c(
  "section_id",
  "section_name",
  "variable",
  "label",
  "value_labels",
  "manual_final_role",
  "manual_score_direction",
  "manual_reverse_score",
  "manual_include_in_index",
  "manual_construct_label",
  "manual_decision_rationale"
)

missing_review_columns <- setdiff(required_review_columns, names(review_completed))

if (length(missing_review_columns) > 0) {
  stop(
    "The completed review file is missing required columns: ",
    paste(missing_review_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 3. Select reviewed protection and risk item sets
# ------------------------------------------------------------

review_completed <- review_completed %>%
  mutate(
    manual_final_role = stringr::str_to_lower(stringr::str_squish(as.character(manual_final_role))),
    manual_score_direction = stringr::str_to_lower(stringr::str_squish(as.character(manual_score_direction))),
    manual_reverse_score = stringr::str_to_lower(stringr::str_squish(as.character(manual_reverse_score))),
    manual_include_in_index = stringr::str_to_lower(stringr::str_squish(as.character(manual_include_in_index))),
    variable = as.character(variable)
  )

protection_items_raw <- review_completed %>%
  filter(
    manual_include_in_index == "yes",
    manual_final_role == "protection"
  ) %>%
  arrange(section_id, variable)

protection_item_duplicates <- protection_items_raw %>%
  count(variable, name = "review_rows") %>%
  filter(review_rows > 1)

protection_items <- protection_items_raw %>%
  group_by(variable) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(section_id, variable)

risk_items_raw <- review_completed %>%
  filter(
    manual_include_in_index == "yes",
    manual_final_role == "risk"
  ) %>%
  arrange(section_id, variable)

risk_item_duplicates <- risk_items_raw %>%
  count(variable, name = "review_rows") %>%
  filter(review_rows > 1)

risk_items <- risk_items_raw %>%
  group_by(variable) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(section_id, variable)

excluded_items <- review_completed %>%
  filter(manual_include_in_index != "yes") %>%
  arrange(section_id, variable)

if (nrow(protection_items) == 0) {
  stop("No reviewed protection items were retained. Script 16 cannot construct the protection index.")
}

risk_index_min_items <- 3
risk_index_constructed <- nrow(risk_items) >= risk_index_min_items

cat("Reviewed protection rows retained:", nrow(protection_items_raw), "\n")
cat("Reviewed unique protection variables retained:", nrow(protection_items), "\n")
cat("Reviewed risk rows retained:", nrow(risk_items_raw), "\n")
cat("Reviewed unique risk variables retained:", nrow(risk_items), "\n")
cat("Risk index constructed:", risk_index_constructed, "\n\n")

# ------------------------------------------------------------
# 4. Variable recovery helpers
# ------------------------------------------------------------

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

covariate_candidates <- c(
  "BIO_SEX",
  "a_age_wave1",
  "a_grade_wave1",
  "H1GI20",
  "H1IR12",
  "GSWGT1"
)

item_variables <- unique(c(protection_items$variable, risk_items$variable))
variables_to_recover <- unique(c(item_variables, covariate_candidates))

normalize_path <- function(x) {
  normalizePath(x, winslash = "/", mustWork = FALSE)
}

find_file_by_name <- function(file_name, project_root) {

  if (is.na(file_name) || file_name == "") {
    return(NA_character_)
  }

  file_name <- as.character(file_name)

  direct_candidates <- c(
    file_name,
    file.path(project_root, file_name),
    file.path(project_root, "data", file_name),
    file.path(project_root, "data", "raw", file_name),
    file.path(project_root, "data", "processed", file_name)
  )

  direct_candidates <- normalize_path(direct_candidates)

  existing_direct <- direct_candidates[file.exists(direct_candidates)]

  if (length(existing_direct) > 0) {
    return(existing_direct[1])
  }

  all_project_files <- list.files(
    project_root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )

  all_project_files <- normalize_path(all_project_files)

  matches <- all_project_files[basename(all_project_files) == basename(file_name)]

  matches <- matches[
    !stringr::str_detect(matches, "/\\.git/") &
      !stringr::str_detect(matches, "/outputs/") &
      !stringr::str_detect(matches, "/docs/")
  ]

  if (length(matches) > 0) {
    return(matches[1])
  }

  NA_character_
}

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
        out[[nm]] <- as_tibble(obj)
      }
    }

  } else if (ext == "rds") {

    obj <- readRDS(file_path)
    if (is.data.frame(obj)) {
      out[[basename(file_path)]] <- as_tibble(obj)
    }

  } else if (ext == "csv") {

    obj <- suppressMessages(readr::read_csv(file_path, show_col_types = FALSE))
    if (is.data.frame(obj)) {
      out[[basename(file_path)]] <- as_tibble(obj)
    }

  } else if (ext == "sas7bdat" && requireNamespace("haven", quietly = TRUE)) {

    obj <- haven::read_sas(file_path)
    if (is.data.frame(obj)) {
      out[[basename(file_path)]] <- as_tibble(obj)
    }
  }

  out
}

# ------------------------------------------------------------
# 5. Identify candidate data files
# ------------------------------------------------------------

source_files_from_review <- review_completed %>%
  filter("file_name" %in% names(.)) %>%
  pull(file_name) %>%
  unique()

source_paths_from_review <- purrr::map_chr(
  source_files_from_review,
  find_file_by_name,
  project_root = project_root
)

source_paths_from_review <- source_paths_from_review[
  !is.na(source_paths_from_review) & file.exists(source_paths_from_review)
]

all_project_data_files <- list.files(
  project_root,
  pattern = "\\.(rda|RData|rds|csv|sas7bdat)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

all_project_data_files <- normalize_path(all_project_data_files)

all_project_data_files <- all_project_data_files[
  !stringr::str_detect(all_project_data_files, "/\\.git/") &
    !stringr::str_detect(all_project_data_files, "/outputs/") &
    !stringr::str_detect(all_project_data_files, "/docs/")
]

candidate_data_files <- unique(c(
  source_paths_from_review,
  all_project_data_files
))

if (length(candidate_data_files) == 0) {
  stop(
    "No candidate data files were found in the project folder.\n",
    "The reviewed item list exists, but respondent-level data are needed to construct the index."
  )
}

candidate_data_files <- candidate_data_files[file.exists(candidate_data_files)]

cat("Candidate data files found:", length(candidate_data_files), "\n\n")

# ------------------------------------------------------------
# 6. Recover item and covariate variables
# ------------------------------------------------------------

combined_data <- NULL
recovered_registry <- tibble(
  variable = character(),
  source_file = character(),
  object_name = character(),
  source_id_variable = character()
)

variables_found <- character(0)

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

    vars_still_needed <- setdiff(variables_to_recover, variables_found)
    vars_here <- intersect(vars_still_needed, names(df))

    if (length(vars_here) == 0) {
      next
    }

    id_here <- intersect(id_candidates, names(df))

    if (length(id_here) > 0) {
      id_here <- id_here[1]
      temp <- df %>%
        select(all_of(id_here), all_of(vars_here)) %>%
        mutate(respondent_id = as.character(.data[[id_here]])) %>%
        select(respondent_id, all_of(vars_here))
    } else {
      id_here <- ".row_id_created"
      temp <- df %>%
        select(all_of(vars_here)) %>%
        mutate(respondent_id = as.character(row_number())) %>%
        select(respondent_id, all_of(vars_here))
    }

    temp <- temp %>%
      distinct(respondent_id, .keep_all = TRUE)

    if (is.null(combined_data)) {
      combined_data <- temp
    } else {
      combined_data <- combined_data %>%
        full_join(temp, by = "respondent_id")
    }

    recovered_registry <- bind_rows(
      recovered_registry,
      tibble(
        variable = vars_here,
        source_file = fp,
        object_name = obj_name,
        source_id_variable = id_here
      )
    )

    variables_found <- unique(c(variables_found, vars_here))
  }
}

if (is.null(combined_data) || nrow(combined_data) == 0) {
  stop("No respondent-level variables could be recovered from the available data files.")
}

missing_item_variables <- setdiff(item_variables, names(combined_data))
missing_covariates <- setdiff(covariate_candidates, names(combined_data))

if (length(missing_item_variables) > 0) {
  stop(
    "The following reviewed index items were not recovered from respondent-level data: ",
    paste(missing_item_variables, collapse = ", ")
  )
}

if (length(missing_covariates) > 0) {
  warning(
    "The following covariates were not recovered and will be omitted from grouped summaries: ",
    paste(missing_covariates, collapse = ", ")
  )
}

cat("Respondent-level data recovered.\n")
cat("Rows:", nrow(combined_data), "\n")
cat("Recovered variables:", length(variables_found), "\n\n")

readr::write_csv(
  recovered_registry,
  file.path(audit_dir, "script16_variable_recovery_registry.csv")
)

# ------------------------------------------------------------
# 7. Scoring helpers
# ------------------------------------------------------------

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

recode_residence_context <- function(x) {

  x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  x_num <- to_numeric_safe(x)

  dplyr::case_when(
    x_num == 1 ~ "Rural",
    x_num == 2 ~ "Suburban",
    x_num %in% c(3, 4, 5) ~ "Urban",

    stringr::str_detect(x_chr, "^1|rural") ~ "Rural",
    stringr::str_detect(x_chr, "^2|suburban") ~ "Suburban",
    stringr::str_detect(x_chr, "^3|^4|^5|urban|commercial|industrial|residential") ~ "Urban",

    TRUE ~ "Missing"
  )
}

extract_missing_codes_from_labels <- function(value_labels) {

  value_labels <- stringr::str_to_lower(clean_chr(value_labels))

  standard_high_missing <- c(
    96, 97, 98, 99,
    996, 997, 998, 999,
    9996, 9997, 9998, 9999
  )

  if (is.na(value_labels) || value_labels == "") {
    return(standard_high_missing)
  }

  parts <- unlist(stringr::str_split(value_labels, ";|\\||\\n"))

  missing_terms <- "refused|don't know|dont know|not applicable|missing|skip|legitimate skip|no answer"

  parts_missing <- parts[stringr::str_detect(parts, missing_terms)]

  extracted <- unlist(stringr::str_extract_all(parts_missing, "-?\\d+(\\.\\d+)?"))
  extracted <- suppressWarnings(as.numeric(extracted))
  extracted <- extracted[!is.na(extracted)]

  unique(c(standard_high_missing, extracted))
}

score_item_01 <- function(x, score_direction, value_labels) {

  x_num <- to_numeric_safe(x)

  missing_codes <- extract_missing_codes_from_labels(value_labels)

  x_num[x_num %in% missing_codes] <- NA_real_

  valid_x <- x_num[!is.na(x_num)]

  if (length(valid_x) == 0) {
    return(rep(NA_real_, length(x_num)))
  }

  min_x <- min(valid_x, na.rm = TRUE)
  max_x <- max(valid_x, na.rm = TRUE)

  if (!is.finite(min_x) || !is.finite(max_x) || max_x == min_x) {
    return(rep(NA_real_, length(x_num)))
  }

  score_direction <- stringr::str_to_lower(clean_chr(score_direction))

  if (score_direction %in% c("higher_is_more_protective", "higher_is_more_risky")) {
    score <- (x_num - min_x) / (max_x - min_x)
  } else if (score_direction %in% c("lower_is_more_protective", "lower_is_more_risky")) {
    score <- (max_x - x_num) / (max_x - min_x)
  } else {
    score <- rep(NA_real_, length(x_num))
  }

  score[score < 0 | score > 1] <- NA_real_

  score
}

construct_index <- function(data, items, index_prefix, min_valid_share = 0.50) {

  if (nrow(items) == 0) {
    return(list(
      scored_data = data %>% select(respondent_id),
      item_audit = tibble(),
      index_summary = tibble(
        index_name = paste0(index_prefix, "_index"),
        items_in_index = 0,
        min_valid_items_required = NA_integer_,
        respondents_total = nrow(data),
        respondents_with_index = 0,
        index_mean = NA_real_,
        index_sd = NA_real_,
        index_min = NA_real_,
        index_max = NA_real_
      )
    ))
  }

  scored_data <- data %>% select(respondent_id)

  item_audit <- list()

  for (i in seq_len(nrow(items))) {

    item <- items[i, ]

    var <- item$variable
    score_var <- paste0(index_prefix, "_score_", var)

    raw_x <- data[[var]]
    score_x <- score_item_01(
      x = raw_x,
      score_direction = item$manual_score_direction,
      value_labels = item$value_labels
    )

    scored_data[[score_var]] <- score_x

    raw_num <- to_numeric_safe(raw_x)
    missing_codes <- extract_missing_codes_from_labels(item$value_labels)
    raw_num[raw_num %in% missing_codes] <- NA_real_

    item_audit[[i]] <- tibble(
      index_prefix = index_prefix,
      variable = var,
      section_id = item$section_id,
      section_name = item$section_name,
      label = item$label,
      manual_final_role = item$manual_final_role,
      manual_score_direction = item$manual_score_direction,
      manual_reverse_score = item$manual_reverse_score,
      manual_construct_label = item$manual_construct_label,
      manual_decision_rationale = item$manual_decision_rationale,
      raw_valid_n = sum(!is.na(raw_num)),
      raw_missing_n = sum(is.na(raw_num)),
      raw_min_observed = suppressWarnings(min(raw_num, na.rm = TRUE)),
      raw_max_observed = suppressWarnings(max(raw_num, na.rm = TRUE)),
      score_valid_n = sum(!is.na(score_x)),
      score_missing_n = sum(is.na(score_x)),
      score_min = suppressWarnings(min(score_x, na.rm = TRUE)),
      score_max = suppressWarnings(max(score_x, na.rm = TRUE)),
      score_mean = suppressWarnings(mean(score_x, na.rm = TRUE)),
      score_sd = suppressWarnings(sd(score_x, na.rm = TRUE))
    )
  }

  item_audit <- bind_rows(item_audit)

  score_columns <- setdiff(names(scored_data), "respondent_id")

  score_matrix <- as.matrix(scored_data[, score_columns, drop = FALSE])

  valid_items_n <- rowSums(!is.na(score_matrix))
  valid_items_share <- valid_items_n / length(score_columns)

  min_valid_items_required <- max(
    1,
    ceiling(length(score_columns) * min_valid_share)
  )

  index_values <- rowMeans(score_matrix, na.rm = TRUE)
  index_values[valid_items_n < min_valid_items_required] <- NA_real_

  index_var <- paste0(index_prefix, "_index")
  valid_n_var <- paste0(index_prefix, "_valid_items_n")
  valid_share_var <- paste0(index_prefix, "_valid_items_share")

  scored_data[[valid_n_var]] <- valid_items_n
  scored_data[[valid_share_var]] <- valid_items_share
  scored_data[[index_var]] <- index_values

  index_summary <- tibble(
    index_name = index_var,
    items_in_index = length(score_columns),
    min_valid_items_required = min_valid_items_required,
    respondents_total = nrow(scored_data),
    respondents_with_index = sum(!is.na(index_values)),
    index_mean = mean(index_values, na.rm = TRUE),
    index_sd = sd(index_values, na.rm = TRUE),
    index_min = min(index_values, na.rm = TRUE),
    index_max = max(index_values, na.rm = TRUE)
  )

  list(
    scored_data = scored_data,
    item_audit = item_audit,
    index_summary = index_summary
  )
}

# ------------------------------------------------------------
# 8. Construct reviewed protection index
# ------------------------------------------------------------

protection_result <- construct_index(
  data = combined_data,
  items = protection_items,
  index_prefix = "reviewed_protection",
  min_valid_share = 0.50
)

protection_scores <- protection_result$scored_data
protection_item_audit <- protection_result$item_audit
protection_index_summary <- protection_result$index_summary

# ------------------------------------------------------------
# 9. Risk index decision
# ------------------------------------------------------------

if (risk_index_constructed) {

  risk_result <- construct_index(
    data = combined_data,
    items = risk_items,
    index_prefix = "reviewed_risk",
    min_valid_share = 0.50
  )

  risk_scores <- risk_result$scored_data
  risk_item_audit <- risk_result$item_audit
  risk_index_summary <- risk_result$index_summary

} else {

  risk_scores <- combined_data %>%
    select(respondent_id) %>%
    mutate(
      reviewed_risk_index = NA_real_,
      reviewed_risk_valid_items_n = NA_integer_,
      reviewed_risk_valid_items_share = NA_real_
    )

  risk_item_audit <- risk_items %>%
    mutate(
      index_prefix = "reviewed_risk",
      construction_status = "not_constructed_insufficient_defensible_risk_items"
    )

  risk_index_summary <- tibble(
    index_name = "reviewed_risk_index",
    items_in_index = nrow(risk_items),
    min_valid_items_required = NA_integer_,
    respondents_total = nrow(combined_data),
    respondents_with_index = 0,
    index_mean = NA_real_,
    index_sd = NA_real_,
    index_min = NA_real_,
    index_max = NA_real_
  )
}

# ------------------------------------------------------------
# 10. Add covariates and final index file
# ------------------------------------------------------------

final_index_data <- protection_scores %>%
  left_join(risk_scores, by = "respondent_id") %>%
  left_join(
    combined_data %>%
      select(respondent_id, any_of(covariate_candidates)),
    by = "respondent_id"
  )

if ("BIO_SEX" %in% names(final_index_data)) {
  final_index_data <- final_index_data %>%
    mutate(
      sex_gender_clean = case_when(
        to_numeric_safe(BIO_SEX) == 1 ~ "Male",
        to_numeric_safe(BIO_SEX) == 2 ~ "Female",
        TRUE ~ "Missing"
      )
    )
}

if ("a_grade_wave1" %in% names(final_index_data)) {
  final_index_data <- final_index_data %>%
    mutate(
      school_grade_clean = ifelse(
        is.na(to_numeric_safe(a_grade_wave1)),
        "Missing",
        as.character(to_numeric_safe(a_grade_wave1))
      )
    )
} else if ("H1GI20" %in% names(final_index_data)) {
  final_index_data <- final_index_data %>%
    mutate(
      school_grade_clean = ifelse(
        is.na(to_numeric_safe(H1GI20)),
        "Missing",
        as.character(to_numeric_safe(H1GI20))
      )
    )
}

if ("H1IR12" %in% names(final_index_data)) {
  final_index_data <- final_index_data %>%
    mutate(
      residence_context_model = recode_residence_context(H1IR12)
    )
}

if ("GSWGT1" %in% names(final_index_data)) {
  final_index_data <- final_index_data %>%
    mutate(
      survey_weight = to_numeric_safe(GSWGT1),
      survey_weight = ifelse(is.na(survey_weight) | survey_weight <= 0, NA_real_, survey_weight)
    )
} else {
  final_index_data <- final_index_data %>%
    mutate(survey_weight = NA_real_)
}

# ------------------------------------------------------------
# 11. Group summaries
# ------------------------------------------------------------

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  weighted.mean(x[ok], w[ok])
}

weighted_sd_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) <= 1) {
    return(NA_real_)
  }
  m <- weighted.mean(x[ok], w[ok])
  sqrt(weighted.mean((x[ok] - m)^2, w[ok]))
}

summarise_index_by_group <- function(data, group_var, index_var) {

  if (!group_var %in% names(data)) {
    return(tibble())
  }

  data %>%
    mutate(group_value = as.character(.data[[group_var]])) %>%
    group_by(group_value) %>%
    summarise(
      n = n(),
      valid_index_n = sum(!is.na(.data[[index_var]])),
      unweighted_mean = mean(.data[[index_var]], na.rm = TRUE),
      unweighted_sd = sd(.data[[index_var]], na.rm = TRUE),
      weighted_mean = weighted_mean_safe(.data[[index_var]], survey_weight),
      weighted_sd = weighted_sd_safe(.data[[index_var]], survey_weight),
      .groups = "drop"
    ) %>%
    mutate(
      grouping_variable = group_var,
      index_name = index_var
    ) %>%
    select(
      index_name,
      grouping_variable,
      group_value,
      n,
      valid_index_n,
      unweighted_mean,
      unweighted_sd,
      weighted_mean,
      weighted_sd
    )
}

overall_summary <- final_index_data %>%
  summarise(
    n = n(),
    valid_index_n = sum(!is.na(reviewed_protection_index)),
    unweighted_mean = mean(reviewed_protection_index, na.rm = TRUE),
    unweighted_sd = sd(reviewed_protection_index, na.rm = TRUE),
    weighted_mean = weighted_mean_safe(reviewed_protection_index, survey_weight),
    weighted_sd = weighted_sd_safe(reviewed_protection_index, survey_weight)
  ) %>%
  mutate(
    index_name = "reviewed_protection_index",
    grouping_variable = "overall",
    group_value = "overall"
  ) %>%
  select(
    index_name,
    grouping_variable,
    group_value,
    n,
    valid_index_n,
    unweighted_mean,
    unweighted_sd,
    weighted_mean,
    weighted_sd
  )

group_summary <- bind_rows(
  overall_summary,
  summarise_index_by_group(final_index_data, "sex_gender_clean", "reviewed_protection_index"),
  summarise_index_by_group(final_index_data, "school_grade_clean", "reviewed_protection_index"),
  summarise_index_by_group(final_index_data, "residence_context_model", "reviewed_protection_index")
)

# ------------------------------------------------------------
# 12. Reliability check
# ------------------------------------------------------------

cronbach_alpha_complete <- function(score_data, score_columns) {

  if (length(score_columns) < 2) {
    return(NA_real_)
  }

  mat <- as.matrix(score_data[, score_columns, drop = FALSE])
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]

  if (nrow(mat) < 2 || ncol(mat) < 2) {
    return(NA_real_)
  }

  item_vars <- apply(mat, 2, var, na.rm = TRUE)
  total_var <- var(rowSums(mat), na.rm = TRUE)

  if (!is.finite(total_var) || total_var <= 0) {
    return(NA_real_)
  }

  k <- ncol(mat)

  (k / (k - 1)) * (1 - sum(item_vars, na.rm = TRUE) / total_var)
}

protection_score_columns <- names(protection_scores)[
  stringr::str_detect(names(protection_scores), "^reviewed_protection_score_")
]

reliability_summary <- tibble(
  index_name = "reviewed_protection_index",
  items_in_index = length(protection_score_columns),
  complete_case_n = sum(stats::complete.cases(protection_scores[, protection_score_columns, drop = FALSE])),
  cronbach_alpha_complete_cases = cronbach_alpha_complete(
    protection_scores,
    protection_score_columns
  ),
  reliability_note = "Cronbach alpha is computed using complete cases only and is reported as a diagnostic, not as a final psychometric validation."
)

# ------------------------------------------------------------
# 13. Methodological decision summary
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Protection index", "A reviewed protection index is constructed from items retained as protection in the completed Script 15 review.",
  "Scoring", "Each retained item is converted to a 0–1 score. Higher values of the final index indicate higher reviewed protection.",
  "Reverse direction", "Items coded so that lower raw values indicate more protection are reversed during scoring.",
  "Missing values", "Refused, don't know, not applicable and high-value missing codes are set to missing before scoring.",
  "Minimum item rule", "A respondent receives an index value only if at least 50 percent of retained protection items are non-missing.",
  "Risk index", ifelse(
    risk_index_constructed,
    "A reviewed risk index is constructed because at least three defensible risk items were retained.",
    "A reviewed risk index is not constructed because fewer than three defensible risk items were retained."
  ),
  "Interpretation", "The reviewed protection index is an internally constructed candidate index for modelling and robustness checks, not a validated clinical or diagnostic scale.",
  "Next step", "Use the reviewed protection index in covariate-adjusted models of adolescent risk outcomes."
)

duplicate_item_audit <- bind_rows(
  protection_item_duplicates %>%
    mutate(index_role = "protection"),
  risk_item_duplicates %>%
    mutate(index_role = "risk")
)

readr::write_csv(
  duplicate_item_audit,
  file.path(audit_dir, "script16_duplicate_review_item_audit.csv")
)

residence_recode_check <- final_index_data %>%
  count(residence_context_model, name = "n") %>%
  arrange(residence_context_model)

readr::write_csv(
  residence_recode_check,
  file.path(audit_dir, "script16_residence_recode_check.csv")
)

# ------------------------------------------------------------
# 14. Save outputs
# ------------------------------------------------------------

final_index_path <- file.path(
  index_dir,
  "script16_reviewed_protection_index.csv"
)

protection_scores_path <- file.path(
  index_dir,
  "script16_reviewed_protection_item_scores.csv"
)

item_audit_path <- file.path(
  audit_dir,
  "script16_reviewed_index_item_audit.csv"
)

index_summary_path <- file.path(
  audit_dir,
  "script16_reviewed_index_summary.csv"
)

group_summary_path <- file.path(
  audit_dir,
  "script16_reviewed_index_by_covariates.csv"
)

reliability_summary_path <- file.path(
  audit_dir,
  "script16_reliability_summary.csv"
)

methodological_decisions_path <- file.path(
  audit_dir,
  "script16_methodological_decisions.csv"
)

risk_decision_path <- file.path(
  audit_dir,
  "script16_risk_index_decision.csv"
)

readr::write_csv(final_index_data, final_index_path)
readr::write_csv(protection_scores, protection_scores_path)

readr::write_csv(
  bind_rows(
    protection_item_audit,
    risk_item_audit %>%
      mutate(across(everything(), as.character)) %>%
      mutate(index_prefix = "reviewed_risk")
  ),
  item_audit_path
)

readr::write_csv(
  bind_rows(
    protection_index_summary,
    risk_index_summary
  ),
  index_summary_path
)

readr::write_csv(group_summary, group_summary_path)
readr::write_csv(reliability_summary, reliability_summary_path)
readr::write_csv(methodological_decisions, methodological_decisions_path)

risk_index_decision <- tibble(
  risk_items_retained = nrow(risk_items),
  minimum_risk_items_required = risk_index_min_items,
  risk_index_constructed = risk_index_constructed,
  decision = ifelse(
    risk_index_constructed,
    "reviewed_risk_index_constructed",
    "reviewed_risk_index_not_constructed"
  ),
  rationale = ifelse(
    risk_index_constructed,
    "At least three reviewed risk items were retained.",
    "Fewer than three reviewed risk items were retained; constructing a risk index would not be methodologically defensible at this stage."
  )
)

readr::write_csv(risk_index_decision, risk_decision_path)

# ------------------------------------------------------------
# 15. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_final_reviewed_protection_index_script16.docx"
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
      "Add Health Wave I — Final Reviewed Protection Index",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 16 constructs the final reviewed protection index using the completed item-direction review produced by Scripts 15 and 15b. The index is constructed only from items retained as protection after the reviewed classification process.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Index summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(bind_rows(protection_index_summary, risk_index_summary))
    ) %>%
    officer::body_add_par("Risk index decision", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(risk_index_decision)
    ) %>%
    officer::body_add_par("Reliability diagnostic", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(reliability_summary)
    ) %>%
    officer::body_add_par("Summary by covariates", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(group_summary)
    ) %>%
    officer::body_add_par("Protection item audit", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(
        protection_item_audit %>%
          select(
            variable,
            section_id,
            manual_construct_label,
            manual_score_direction,
            raw_valid_n,
            score_mean,
            score_sd
          )
      )
    ) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(methodological_decisions)
    ) %>%
    officer::body_add_par("Interpretation note", style = "heading 2") %>%
    officer::body_add_par(
      "Higher values of reviewed_protection_index indicate higher relative protection among the retained candidate items. The measure should be interpreted as a reviewed analytical index for modelling and robustness checks, not as a validated clinical or diagnostic scale.",
      style = "Normal"
    )

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 16. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "completed_review_file_loaded",
    "protection_items_retained",
    "respondent_level_data_recovered",
    "all_protection_items_recovered",
    "protection_index_created",
    "risk_index_decision_created",
    "group_summary_created",
    "reliability_summary_created",
    "word_report_created"
  ),
  status = c(
    file.exists(completed_review_path),
    nrow(protection_items) > 0,
    nrow(combined_data) > 0,
    length(missing_item_variables) == 0,
    file.exists(final_index_path),
    file.exists(risk_decision_path),
    file.exists(group_summary_path),
    file.exists(reliability_summary_path),
    !is.na(word_report_path) && file.exists(word_report_path)
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script16_final_status.csv")
)

# ------------------------------------------------------------
# 17. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 16 completed: Final Reviewed Protection Index\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nProtection index summary:\n")
print(protection_index_summary)

cat("\nRisk index decision:\n")
print(risk_index_decision)

cat("\nReliability summary:\n")
print(reliability_summary)

cat("\nGroup summary:\n")
print(group_summary)

cat("\nOutputs created:\n")
cat("- ", final_index_path, "\n")
cat("- ", protection_scores_path, "\n")
cat("- ", item_audit_path, "\n")
cat("- ", index_summary_path, "\n")
cat("- ", group_summary_path, "\n")
cat("- ", reliability_summary_path, "\n")
cat("- ", methodological_decisions_path, "\n")
cat("- ", risk_decision_path, "\n")
cat("- ", file.path(audit_dir, "script16_variable_recovery_registry.csv"), "\n")
cat("- ", file.path(audit_dir, "script16_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Review the Script 16 console output and the Word report before committing.\n")
cat("Do not run Script 17 until the reviewed_protection_index is validated.\n")