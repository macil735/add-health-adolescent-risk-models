# ============================================================
# Script 18a v5 — Partner and Frequency Targeted Recovery Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Close the ISX-equivalent audit for:
#     isx2 — number of partners / sexual partner exposure
#     isx4 — sexual frequency / sexual exposure intensity
#
# Why this script is needed:
#   Script 18a v4 recovered strong candidates for:
#     isx1: sexual initiation / timing
#     isx3: condom / contraceptive / birth-control use
#
#   However, it did not clearly recover behavioral candidates for:
#     isx2: partner exposure
#     isx4: sexual frequency / exposure intensity
#
# Strategy:
#   1. Search all local respondent-level files for variables whose
#      names or labels suggest partner exposure or sexual frequency.
#   2. Avoid using psychosocial predictors as dependent-index items.
#   3. Re-check H1CO variables that may be related to partners or
#      frequency, including variables that appeared empty in v4.
#   4. Produce a manual selection template and an index feasibility
#      decision table.
#
# Main outputs:
#   outputs/audits/script18a_v5_partner_frequency_inventory.csv
#   outputs/audits/script18a_v5_partner_frequency_best_candidates.csv
#   outputs/audits/script18a_v5_h1co_targeted_recheck.csv
#   outputs/audits/script18a_v5_isx2_isx4_manual_selection_TEMPLATE.csv
#   outputs/audits/script18a_v5_index_feasibility_decision.csv
#   docs/add_health_wave01_partner_frequency_targeted_recovery_script18a_v5.docx
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
doc_dir <- file.path(project_root, "docs")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 18a v5 started: Partner and Frequency Targeted Recovery Audit\n")
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
  # They may be valid response values in month/frequency items.
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

valid_numeric_vector <- function(x, value_labels) {

  x_num <- to_numeric_safe(x)
  missing_codes <- extract_missing_codes_from_labels(value_labels)

  x_num[x_num %in% missing_codes] <- NA_real_

  x_num
}

summarise_value_distribution <- function(x, value_labels, max_values = 40) {

  x_num <- valid_numeric_vector(x, value_labels)

  tab <- tibble(value = x_num) %>%
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

# ------------------------------------------------------------
# 3. Previous audit label recovery
# ------------------------------------------------------------

previous_audit_files <- c(
  file.path(audit_dir, "script17a_all_variable_inventory.csv"),
  file.path(audit_dir, "script17a_candidate_outcome_inventory.csv"),
  file.path(audit_dir, "script18a_isx_candidate_variable_inventory.csv"),
  file.path(audit_dir, "script18a_v2_isx_equivalent_candidate_inventory.csv"),
  file.path(audit_dir, "script18a_v3_clean_behavior_variable_inventory.csv"),
  file.path(audit_dir, "script18a_v4_h1co_all_occurrences.csv"),
  file.path(audit_dir, "script18a_v4_h1co_best_source_by_variable.csv")
)

previous_audit_files <- previous_audit_files[file.exists(previous_audit_files)]

recover_previous_labels <- function(files) {

  if (length(files) == 0) {
    return(tibble(
      variable = character(),
      recovered_variable_label = character(),
      recovered_value_labels = character(),
      recovered_label_source = character()
    ))
  }

  label_list <- list()

  for (fp in files) {

    tmp <- tryCatch(
      readr::read_csv(fp, show_col_types = FALSE),
      error = function(e) NULL
    )

    if (is.null(tmp) || !"variable" %in% names(tmp)) {
      next
    }

    var_label_col <- intersect(
      c(
        "variable_label",
        "variable_label_effective",
        "variable_label_original",
        "label",
        "item_label",
        "question_label"
      ),
      names(tmp)
    )

    value_label_col <- intersect(
      c(
        "value_labels",
        "value_labels_effective",
        "value_labels_original",
        "value_label",
        "labels"
      ),
      names(tmp)
    )

    if (length(var_label_col) == 0 && length(value_label_col) == 0) {
      next
    }

    label_list[[length(label_list) + 1]] <- tibble(
      variable = as.character(tmp$variable),
      recovered_variable_label = if (length(var_label_col) > 0) {
        as.character(tmp[[var_label_col[1]]])
      } else {
        ""
      },
      recovered_value_labels = if (length(value_label_col) > 0) {
        as.character(tmp[[value_label_col[1]]])
      } else {
        ""
      },
      recovered_label_source = basename(fp)
    ) %>%
      mutate(
        recovered_variable_label = clean_chr(recovered_variable_label),
        recovered_value_labels = clean_chr(recovered_value_labels)
      ) %>%
      filter(
        recovered_variable_label != "" |
          recovered_value_labels != ""
      )
  }

  if (length(label_list) == 0) {
    return(tibble(
      variable = character(),
      recovered_variable_label = character(),
      recovered_value_labels = character(),
      recovered_label_source = character()
    ))
  }

  bind_rows(label_list) %>%
    arrange(
      variable,
      desc(nchar(recovered_variable_label)),
      desc(nchar(recovered_value_labels))
    ) %>%
    group_by(variable) %>%
    summarise(
      recovered_variable_label = first(recovered_variable_label[recovered_variable_label != ""]),
      recovered_value_labels = first(recovered_value_labels[recovered_value_labels != ""]),
      recovered_label_source = first(recovered_label_source),
      .groups = "drop"
    ) %>%
    mutate(
      recovered_variable_label = ifelse(
        is.na(recovered_variable_label),
        "",
        recovered_variable_label
      ),
      recovered_value_labels = ifelse(
        is.na(recovered_value_labels),
        "",
        recovered_value_labels
      )
    )
}

previous_label_lookup <- recover_previous_labels(previous_audit_files)

readr::write_csv(
  previous_label_lookup,
  file.path(audit_dir, "script18a_v5_previous_label_lookup.csv")
)

# ------------------------------------------------------------
# 4. Target definitions for isx2 and isx4
# ------------------------------------------------------------

target_h1co_variables <- tibble::tribble(
  ~variable, ~target_reason,
  "H1CO4A",  "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO4B",  "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO4C",  "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO5M",  "H1CO targeted recheck; possible timing/frequency related item.",
  "H1CO5Y",  "H1CO targeted recheck; possible timing/frequency related item.",
  "H1CO6",   "Recovered in v4; most recent sex birth-control related item.",
  "H1CO7A",  "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO7B",  "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO7C",  "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO8",   "Recovered in v4; ever use condom.",
  "H1CO9",   "Recovered in v4; frequency of condom use.",
  "H1CO10",  "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO11",  "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO12A", "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO12B", "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO12C", "H1CO targeted recheck; possible sexual behavior item.",
  "H1CO13",  "Recovered in v4; ever use birth control.",
  "H1CO14M", "H1CO targeted recheck; possible timing/frequency related item.",
  "H1CO14Y", "H1CO targeted recheck; possible timing/frequency related item.",
  "H1CO15",  "H1CO targeted recheck; possible sexual behavior item."
)

is_partner_label <- function(text_l) {
  stringr::str_detect(
    text_l,
    paste(
      c(
        "number.*partner",
        "how many.*partner",
        "sex.*partner",
        "sexual.*partner",
        "partner.*sex",
        "partners",
        "people.*sex",
        "many.*people",
        "lifetime.*partner",
        "past.*year.*partner",
        "last.*year.*partner"
      ),
      collapse = "|"
    )
  )
}

is_frequency_label <- function(text_l) {
  stringr::str_detect(
    text_l,
    paste(
      c(
        "how often.*sex",
        "how oft.*sex",
        "frequency.*sex",
        "times.*sex",
        "sex.*times",
        "intercourse.*times",
        "times.*intercourse",
        "sexual activity",
        "sexually active",
        "past.*year.*sex",
        "last.*year.*sex",
        "recent.*sex",
        "most recent sex"
      ),
      collapse = "|"
    )
  )
}

is_method_label <- function(text_l) {
  stringr::str_detect(
    text_l,
    paste(
      c(
        "condom",
        "birth control",
        "contracep",
        "contraceptive",
        "use.*method",
        "used.*method"
      ),
      collapse = "|"
    )
  )
}

is_outcome_label <- function(text_l) {
  stringr::str_detect(
    text_l,
    paste(
      c(
        "diagnos",
        "chlamydia",
        "gonorrhea",
        "std",
        "sti",
        "hiv",
        "aids",
        "pregnan"
      ),
      collapse = "|"
    )
  )
}

is_psychosocial_label <- function(variable, text_l) {

  psychosocial_prefix <- stringr::str_detect(
    stringr::str_to_upper(variable),
    "^H1PF|^H1SE|^H1ED|^H1PR|^H1MO|^H1RP|^H1BC|^H1WP|^H1PA"
  )

  psychosocial_terms <- stringr::str_detect(
    text_l,
    paste(
      c(
        "know",
        "knowledge",
        "learned",
        "heard",
        "risk of",
        "chance of",
        "likely",
        "feel",
        "safe",
        "close",
        "parents",
        "mother",
        "mom",
        "father",
        "dad",
        "friend",
        "friends",
        "approve",
        "disapprove",
        "moral",
        "wrong",
        "attitude",
        "believe",
        "expect",
        "if sex",
        "if pregnant",
        "would",
        "resist",
        "refuse",
        "easy to get",
        "expensive",
        "bothersome",
        "make own"
      ),
      collapse = "|"
    )
  )

  psychosocial_prefix | psychosocial_terms
}

classify_target_component <- function(variable, effective_label) {

  text_l <- lower_clean(paste(variable, effective_label, sep = " | "))
  var_u <- stringr::str_to_upper(variable)

  dplyr::case_when(
    is_partner_label(text_l) ~
      "isx2_partner_exposure",

    is_frequency_label(text_l) ~
      "isx4_sexual_frequency_exposure",

    is_method_label(text_l) ~
      "isx3_protective_method_use",

    is_outcome_label(text_l) ~
      "sexual_health_outcome_not_isx",

    var_u %in% target_h1co_variables$variable ~
      "h1co_target_manual_review",

    TRUE ~
      "target_unclassified_manual_review"
  )
}

component_label <- function(component) {
  dplyr::case_when(
    component == "isx2_partner_exposure" ~
      "ISX2: number of partners / partner exposure",
    component == "isx4_sexual_frequency_exposure" ~
      "ISX4: sexual frequency / exposure intensity",
    component == "isx3_protective_method_use" ~
      "ISX3: condom / contraceptive / birth-control use",
    component == "sexual_health_outcome_not_isx" ~
      "Sexual health outcome, not ISX item",
    component == "h1co_target_manual_review" ~
      "Targeted H1CO variable requiring manual review",
    TRUE ~
      "Target variable requiring manual review"
  )
}

candidate_role <- function(component,
                           valid_n,
                           distinct_valid_n,
                           psychosocial_flag) {

  dplyr::case_when(
    psychosocial_flag ~
      "exclude_psychosocial_predictor_not_dependent_index",

    component %in% c(
      "isx2_partner_exposure",
      "isx4_sexual_frequency_exposure"
    ) &
      valid_n >= 500 &
      distinct_valid_n >= 2 ~
      "strong_candidate_isx2_isx4",

    component %in% c(
      "isx2_partner_exposure",
      "isx4_sexual_frequency_exposure"
    ) &
      valid_n > 0 &
      distinct_valid_n >= 2 ~
      "low_valid_n_candidate_isx2_isx4",

    component == "isx3_protective_method_use" &
      valid_n > 0 ~
      "method_use_candidate_not_isx2_isx4",

    component == "sexual_health_outcome_not_isx" ~
      "outcome_not_isx_item",

    valid_n > 0 &
      distinct_valid_n >= 2 ~
      "manual_review_available_behavioral_candidate",

    valid_n > 0 ~
      "single_value_support_or_skip_pattern",

    TRUE ~
      "unavailable_or_empty_target"
  )
}

role_rank <- function(role) {
  dplyr::case_when(
    role == "strong_candidate_isx2_isx4" ~ 1L,
    role == "low_valid_n_candidate_isx2_isx4" ~ 2L,
    role == "manual_review_available_behavioral_candidate" ~ 3L,
    role == "single_value_support_or_skip_pattern" ~ 4L,
    role == "method_use_candidate_not_isx2_isx4" ~ 5L,
    role == "outcome_not_isx_item" ~ 6L,
    role == "exclude_psychosocial_predictor_not_dependent_index" ~ 7L,
    role == "unavailable_or_empty_target" ~ 8L,
    TRUE ~ 9L
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

# ------------------------------------------------------------
# 6. Locate respondent-level files
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
    "No respondent-level data files were found.\n",
    "Copy the local Add Health data folder into the project directory, but do not commit it."
  )
}

cat("Candidate respondent-level data files found:", length(candidate_data_files), "\n\n")

# ------------------------------------------------------------
# 7. Build targeted inventory
# ------------------------------------------------------------

inventory_list <- list()

target_regex <- paste(
  c(
    "partner",
    "partners",
    "how many",
    "number",
    "people.*sex",
    "how often",
    "how oft",
    "frequency",
    "times",
    "sexual activity",
    "sexually active",
    "most recent sex",
    "last year",
    "past year",
    "intercourse"
  ),
  collapse = "|"
)

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

    for (var in names(df)) {

      x <- df[[var]]

      var_label_original <- clean_chr(variable_label_string(x))
      value_labels_original <- clean_chr(value_label_string(x))

      lookup_row <- previous_label_lookup %>%
        filter(variable == var) %>%
        slice_head(n = 1)

      recovered_label <- if (nrow(lookup_row) > 0) {
        lookup_row$recovered_variable_label[1]
      } else {
        ""
      }

      recovered_value_labels <- if (nrow(lookup_row) > 0) {
        lookup_row$recovered_value_labels[1]
      } else {
        ""
      }

      effective_label <- dplyr::case_when(
        var_label_original != "" ~ var_label_original,
        recovered_label != "" ~ recovered_label,
        TRUE ~ ""
      )

      effective_value_labels <- dplyr::case_when(
        value_labels_original != "" ~ value_labels_original,
        recovered_value_labels != "" ~ recovered_value_labels,
        TRUE ~ ""
      )

      text_l <- lower_clean(
        paste(var, effective_label, effective_value_labels, sep = " | ")
      )

      is_target_h1co <- stringr::str_to_upper(var) %in% target_h1co_variables$variable
      label_target_match <- stringr::str_detect(text_l, target_regex)
      partner_match <- is_partner_label(text_l)
      frequency_match <- is_frequency_label(text_l)
      psychosocial_flag <- is_psychosocial_label(var, text_l)

      include_target <- is_target_h1co | label_target_match | partner_match | frequency_match

      if (!include_target) {
        next
      }

      x_num_valid <- valid_numeric_vector(x, effective_value_labels)

      valid_values <- sort(unique(x_num_valid[!is.na(x_num_valid)]))
      valid_n <- sum(!is.na(x_num_valid))
      missing_n <- sum(is.na(x_num_valid))
      distinct_valid_n <- length(valid_values)

      component <- classify_target_component(var, effective_label)

      role <- candidate_role(
        component = component,
        valid_n = valid_n,
        distinct_valid_n = distinct_valid_n,
        psychosocial_flag = psychosocial_flag
      )

      inventory_list[[length(inventory_list) + 1]] <- tibble(
        source_file = fp,
        file_name = basename(fp),
        object_name = obj_name,
        object_rows = nrow(df),
        variable = var,
        variable_label_original = var_label_original,
        variable_label_recovered = recovered_label,
        variable_label_effective = effective_label,
        value_labels_original = value_labels_original,
        value_labels_recovered = recovered_value_labels,
        value_labels_effective = effective_value_labels,
        is_target_h1co = is_target_h1co,
        label_target_match = label_target_match,
        partner_match = partner_match,
        frequency_match = frequency_match,
        psychosocial_flag = psychosocial_flag,
        target_component = component,
        target_component_label = component_label(component),
        target_candidate_role = role,
        target_candidate_role_rank = role_rank(role),
        valid_n = valid_n,
        missing_n = missing_n,
        distinct_valid_n = distinct_valid_n,
        valid_values = paste(valid_values, collapse = ", "),
        value_distribution = summarise_value_distribution(x, effective_value_labels),
        numeric_min = safe_min(x_num_valid),
        numeric_max = safe_max(x_num_valid),
        numeric_mean = safe_mean(x_num_valid),
        numeric_sd = safe_sd(x_num_valid),
        recovered_label_source = if (nrow(lookup_row) > 0) {
          lookup_row$recovered_label_source[1]
        } else {
          ""
        }
      )
    }
  }
}

target_inventory_all <- bind_rows(inventory_list)

if (nrow(target_inventory_all) == 0) {
  stop("No partner/frequency targeted variables were detected.")
}

# Keep all occurrences first.
readr::write_csv(
  target_inventory_all,
  file.path(audit_dir, "script18a_v5_partner_frequency_all_occurrences.csv")
)

# Select best occurrence per variable.
target_inventory <- target_inventory_all %>%
  mutate(
    data_quality_score =
      ifelse(valid_n > 0, 10L, 0L) +
      ifelse(distinct_valid_n >= 2, 5L, 0L) +
      ifelse(variable_label_effective != "", 3L, 0L) +
      ifelse(value_labels_effective != "", 1L, 0L),
    total_priority_score =
      data_quality_score +
      ifelse(target_candidate_role == "strong_candidate_isx2_isx4", 20L, 0L) +
      ifelse(target_candidate_role == "low_valid_n_candidate_isx2_isx4", 15L, 0L) +
      ifelse(target_candidate_role == "manual_review_available_behavioral_candidate", 10L, 0L) +
      ifelse(is_target_h1co, 5L, 0L) -
      ifelse(psychosocial_flag, 10L, 0L)
  ) %>%
  arrange(
    variable,
    target_candidate_role_rank,
    desc(valid_n),
    desc(distinct_valid_n),
    desc(total_priority_score),
    desc(nchar(variable_label_effective)),
    file_name,
    object_name
  ) %>%
  group_by(variable) %>%
  mutate(
    occurrence_count_for_variable = n(),
    selected_best_occurrence = row_number() == 1
  ) %>%
  ungroup() %>%
  filter(selected_best_occurrence) %>%
  arrange(
    target_candidate_role_rank,
    target_component,
    desc(valid_n),
    variable
  )

readr::write_csv(
  target_inventory,
  file.path(audit_dir, "script18a_v5_partner_frequency_inventory.csv")
)

# ------------------------------------------------------------
# 8. H1CO targeted recheck
# ------------------------------------------------------------
h1co_targeted_recheck <- target_inventory %>%
  filter(is_target_h1co | stringr::str_detect(stringr::str_to_upper(variable), "^H1CO")) %>%
  select(
    variable,
    variable_label_effective,
    target_component,
    target_component_label,
    target_candidate_role,
    target_candidate_role_rank,
    valid_n,
    distinct_valid_n,
    valid_values,
    value_distribution,
    occurrence_count_for_variable,
    file_name,
    object_name,
    source_file,
    value_labels_effective
  ) %>%
  arrange(
    target_candidate_role_rank,
    target_component,
    variable
  )

readr::write_csv(
  h1co_targeted_recheck,
  file.path(audit_dir, "script18a_v5_h1co_targeted_recheck.csv")
)

# ------------------------------------------------------------
# 9. Best candidates for isx2 and isx4
# ------------------------------------------------------------

partner_frequency_best_candidates <- target_inventory %>%
  filter(
    target_component %in% c(
      "isx2_partner_exposure",
      "isx4_sexual_frequency_exposure"
    ) |
      target_candidate_role %in% c(
        "strong_candidate_isx2_isx4",
        "low_valid_n_candidate_isx2_isx4",
        "manual_review_available_behavioral_candidate"
      )
  ) %>%
  arrange(
    target_candidate_role_rank,
    target_component,
    desc(valid_n),
    variable
  )

readr::write_csv(
  partner_frequency_best_candidates,
  file.path(audit_dir, "script18a_v5_partner_frequency_best_candidates.csv")
)

# ------------------------------------------------------------
# 10. Manual selection template
# ------------------------------------------------------------

manual_selection_template <- partner_frequency_best_candidates %>%
  mutate(
    manual_use_in_script18b = case_when(
      target_candidate_role %in% c(
        "strong_candidate_isx2_isx4",
        "low_valid_n_candidate_isx2_isx4",
        "manual_review_available_behavioral_candidate"
      ) ~ "review",
      TRUE ~ "no"
    ),
    manual_final_isx_component = case_when(
      target_component %in% c(
        "isx2_partner_exposure",
        "isx4_sexual_frequency_exposure"
      ) ~ target_component,
      TRUE ~ "manual_decision_required"
    ),
    manual_final_role = case_when(
      target_candidate_role == "strong_candidate_isx2_isx4" ~ "main_or_proxy_isx2_isx4_item",
      target_candidate_role == "low_valid_n_candidate_isx2_isx4" ~ "low_valid_n_proxy_item",
      target_candidate_role == "manual_review_available_behavioral_candidate" ~ "manual_review_behavioral_candidate",
      TRUE ~ "exclude_or_auxiliary"
    ),
    manual_score_4_rule = "",
    manual_score_3_rule = "",
    manual_score_2_rule = "",
    manual_score_1_rule = "",
    manual_never_had_sex_rule = "If respondent never had sexual intercourse, assign highest protection score where conceptually appropriate.",
    manual_proxy_justification = "",
    manual_missing_codes = "",
    manual_reverse_score_needed = "",
    manual_decision_rationale = "",
    manual_reviewer = "",
    manual_review_date = ""
  ) %>%
  select(
    manual_use_in_script18b,
    manual_final_isx_component,
    manual_final_role,
    manual_score_4_rule,
    manual_score_3_rule,
    manual_score_2_rule,
    manual_score_1_rule,
    manual_never_had_sex_rule,
    manual_proxy_justification,
    manual_missing_codes,
    manual_reverse_score_needed,
    manual_decision_rationale,
    manual_reviewer,
    manual_review_date,
    variable,
    variable_label_effective,
    target_component,
    target_component_label,
    target_candidate_role,
    valid_n,
    missing_n,
    distinct_valid_n,
    valid_values,
    value_distribution,
    value_labels_effective,
    is_target_h1co,
    partner_match,
    frequency_match,
    psychosocial_flag,
    file_name,
    object_name,
    source_file
  )

readr::write_csv(
  manual_selection_template,
  file.path(audit_dir, "script18a_v5_isx2_isx4_manual_selection_TEMPLATE.csv")
)

# ------------------------------------------------------------
# 11. Feasibility decision table
# ------------------------------------------------------------

isx2_strong_n <- partner_frequency_best_candidates %>%
  filter(
    target_component == "isx2_partner_exposure",
    target_candidate_role %in% c(
      "strong_candidate_isx2_isx4",
      "low_valid_n_candidate_isx2_isx4",
      "manual_review_available_behavioral_candidate"
    )
  ) %>%
  nrow()

isx4_strong_n <- partner_frequency_best_candidates %>%
  filter(
    target_component == "isx4_sexual_frequency_exposure",
    target_candidate_role %in% c(
      "strong_candidate_isx2_isx4",
      "low_valid_n_candidate_isx2_isx4",
      "manual_review_available_behavioral_candidate"
    )
  ) %>%
  nrow()

index_feasibility_decision <- tibble(
  decision_area = c(
    "isx2_partner_exposure",
    "isx4_sexual_frequency_exposure",
    "full_four_component_isx",
    "restricted_isx_alternative",
    "next_step"
  ),
  evidence = c(
    paste0("Candidate rows detected for isx2: ", isx2_strong_n),
    paste0("Candidate rows detected for isx4: ", isx4_strong_n),
    ifelse(
      isx2_strong_n > 0 & isx4_strong_n > 0,
      "Four-component ISX may be possible after manual review.",
      "Four-component ISX is not yet empirically supported by recovered behavioral variables."
    ),
    "If isx2/isx4 remain unavailable, construct a restricted ISX-equivalent index using sexual initiation/timing and condom/contraceptive behavior, and document the limitation.",
    "Review the manual selection template before Script 18b."
  ),
  provisional_decision = c(
    ifelse(isx2_strong_n > 0, "review_candidates", "not_available_or_not_recovered"),
    ifelse(isx4_strong_n > 0, "review_candidates", "not_available_or_not_recovered"),
    ifelse(isx2_strong_n > 0 & isx4_strong_n > 0, "possible_after_review", "not_ready"),
    "available_if_full_isx_not_possible",
    "manual_review_required"
  )
)

readr::write_csv(
  index_feasibility_decision,
  file.path(audit_dir, "script18a_v5_index_feasibility_decision.csv")
)

# ------------------------------------------------------------
# 12. Summaries
# ------------------------------------------------------------

target_component_summary <- target_inventory %>%
  count(target_component, target_component_label, target_candidate_role, name = "variables") %>%
  arrange(target_component, role_rank(target_candidate_role))

best_candidate_summary <- partner_frequency_best_candidates %>%
  group_by(target_component, target_component_label) %>%
  summarise(
    candidate_variables = n(),
    best_variable = first(variable),
    best_label = first(variable_label_effective),
    best_role = first(target_candidate_role),
    best_valid_n = first(valid_n),
    best_values = first(valid_values),
    .groups = "drop"
  ) %>%
  arrange(target_component)

role_summary <- target_inventory %>%
  count(target_candidate_role, name = "variables") %>%
  arrange(role_rank(target_candidate_role))

readr::write_csv(
  target_component_summary,
  file.path(audit_dir, "script18a_v5_target_component_summary.csv")
)

readr::write_csv(
  best_candidate_summary,
  file.path(audit_dir, "script18a_v5_best_candidate_summary.csv")
)

readr::write_csv(
  role_summary,
  file.path(audit_dir, "script18a_v5_role_summary.csv")
)

# ------------------------------------------------------------
# 13. Methodological decisions
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Reason for Script 18a v5", "Script 18a v5 targets the two ISX dimensions not closed by Script 18a v4: partner exposure and sexual frequency/exposure intensity.",
  "Dependent variable logic", "The final dependent variable should remain behavioral: higher ISX-equivalent scores represent higher sexual protection.",
  "Psychosocial exclusion", "Variables about knowledge, attitudes, risk perceptions, parents, peers, school, morality or self-efficacy are excluded from the dependent ISX index and retained for later predictor blocks.",
  "Proxy rule", "If exact last-year partner/frequency items are unavailable, a functionally equivalent behavioral proxy may be used only if it measures actual sexual behavior or exposure.",
  "Restricted index option", "If partner exposure and sexual frequency are unavailable in the public-use data, a restricted ISX-equivalent index using initiation/timing and protective method behavior is methodologically acceptable if clearly documented.",
  "Manual review", "Script 18a v5 does not construct the index. It prepares candidate evidence for the manual decision before Script 18b.",
  "Data protection", "Only variable-level summaries are exported; respondent-level data are not written."
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18a_v5_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 14. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_partner_frequency_targeted_recovery_script18a_v5.docx"
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

  report_candidates <- partner_frequency_best_candidates %>%
    select(
      variable,
      variable_label_effective,
      target_component_label,
      target_candidate_role,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution
    ) %>%
    slice_head(n = 80)

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — Partner and Frequency Targeted Recovery Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18a v5 targets the partner-exposure and sexual-frequency dimensions of the ISX-equivalent sexual protection index. It separates behavioral candidates from psychosocial predictors and produces a manual review template before index construction.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Index feasibility decision", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(index_feasibility_decision)) %>%
    officer::body_add_par("Target component summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(target_component_summary)) %>%
    officer::body_add_par("Best candidate summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(best_candidate_summary)) %>%
    officer::body_add_par("Partner/frequency candidate variables", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(report_candidates)) %>%
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
    "candidate_data_files_found",
    "all_occurrences_created",
    "partner_frequency_inventory_created",
    "h1co_targeted_recheck_created",
    "best_candidates_created",
    "manual_selection_template_created",
    "index_feasibility_decision_created",
    "target_component_summary_created",
    "best_candidate_summary_created",
    "methodological_decisions_created",
    "word_report_created",
    "manual_review_still_required"
  ),
  status = c(
    length(candidate_data_files) > 0,
    file.exists(file.path(audit_dir, "script18a_v5_partner_frequency_all_occurrences.csv")),
    file.exists(file.path(audit_dir, "script18a_v5_partner_frequency_inventory.csv")),
    file.exists(file.path(audit_dir, "script18a_v5_h1co_targeted_recheck.csv")),
    file.exists(file.path(audit_dir, "script18a_v5_partner_frequency_best_candidates.csv")),
    file.exists(file.path(audit_dir, "script18a_v5_isx2_isx4_manual_selection_TEMPLATE.csv")),
    file.exists(file.path(audit_dir, "script18a_v5_index_feasibility_decision.csv")),
    file.exists(file.path(audit_dir, "script18a_v5_target_component_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_v5_best_candidate_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_v5_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18a_v5_final_status.csv")
)

# ------------------------------------------------------------
# 16. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18a v5 completed: Partner and Frequency Targeted Recovery Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nIndex feasibility decision:\n")
print(index_feasibility_decision)

cat("\nTarget component summary:\n")
print(target_component_summary)

cat("\nBest candidate summary:\n")
print(best_candidate_summary)

cat("\nH1CO targeted recheck:\n")
print(
  h1co_targeted_recheck %>%
    select(
      variable,
      variable_label_effective,
      target_component,
      target_candidate_role,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution
    ),
  n = 100
)

cat("\nPartner/frequency best candidates:\n")
print(
  partner_frequency_best_candidates %>%
    select(
      variable,
      variable_label_effective,
      target_component,
      target_component_label,
      target_candidate_role,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution,
      psychosocial_flag
    ),
  n = 100
)

cat("\nOutputs created:\n")
cat("- ", file.path(audit_dir, "script18a_v5_previous_label_lookup.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_partner_frequency_all_occurrences.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_partner_frequency_inventory.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_h1co_targeted_recheck.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_partner_frequency_best_candidates.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_isx2_isx4_manual_selection_TEMPLATE.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_index_feasibility_decision.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_target_component_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_best_candidate_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_role_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v5_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Review the index feasibility decision and the manual selection template before Script 18b.\n")
cat("Do not commit until the final ISX structure is decided.\n")