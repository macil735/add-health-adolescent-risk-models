# ============================================================
# Script 18a v4 — H1CO Best Source Recovery Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Recover the best available source for each H1CO* variable across
#   all local respondent-level files.
#
# Problem detected in Script 18a v3:
#   Some H1CO* variables appeared with valid_n = 0 and blank labels
#   because the script retained an empty occurrence instead of the
#   best available occurrence across files/objects.
#
# Methodological purpose:
#   Before constructing the ISX-equivalent sexual protection index,
#   we must identify which H1CO* variables are genuinely available,
#   which file/object contains the best version, and which variables
#   are behavior items, support variables, or outcomes.
#
# Main outputs:
#   outputs/audits/script18a_v4_h1co_all_occurrences.csv
#   outputs/audits/script18a_v4_h1co_best_source_by_variable.csv
#   outputs/audits/script18a_v4_h1co_recovered_behavior_candidates.csv
#   outputs/audits/script18a_v4_isx_candidate_map.csv
#   outputs/audits/script18a_v4_isx_manual_selection_TEMPLATE.csv
#   outputs/audits/script18a_v4_final_status.csv
#   docs/add_health_wave01_h1co_best_source_recovery_script18a_v4.docx
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
cat("Script 18a v4 started: H1CO Best Source Recovery Audit\n")
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

valid_numeric_vector <- function(x, value_labels) {

  x_num <- to_numeric_safe(x)
  missing_codes <- extract_missing_codes_from_labels(value_labels)

  x_num[x_num %in% missing_codes] <- NA_real_

  x_num
}

summarise_value_distribution <- function(x, value_labels, max_values = 30) {

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

has_nonblank_label <- function(x) {
  !is.na(x) & clean_chr(x) != ""
}

# ------------------------------------------------------------
# 3. Optional label recovery from previous audit outputs
# ------------------------------------------------------------

previous_audit_files <- c(
  file.path(audit_dir, "script17a_all_variable_inventory.csv"),
  file.path(audit_dir, "script17a_candidate_outcome_inventory.csv"),
  file.path(audit_dir, "script18a_isx_candidate_variable_inventory.csv"),
  file.path(audit_dir, "script18a_v2_isx_equivalent_candidate_inventory.csv"),
  file.path(audit_dir, "script18a_v3_clean_behavior_variable_inventory.csv")
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
      c("variable_label", "label", "item_label", "question_label"),
      names(tmp)
    )

    value_label_col <- intersect(
      c("value_labels", "value_label", "labels"),
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
      filter(stringr::str_detect(stringr::str_to_upper(variable), "^H1CO")) %>%
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
  file.path(audit_dir, "script18a_v4_previous_label_lookup.csv")
)

# ------------------------------------------------------------
# 4. Conservative known H1CO hints from earlier validated outputs
# ------------------------------------------------------------

known_h1co_hints <- tibble::tribble(
  ~variable, ~known_label_hint, ~known_component_hint,
  "H1CO1",  "S24Q1 EVER HAVE SEX-W1", "isx_support_ever_sex",
  "H1CO2Y", "S24Q2Y FIRST TIME SEX-YEAR-W1", "isx1_sexual_initiation_timing",
  "H1CO2M", "S24Q2M FIRST TIME SEX-MONTH-W1", "isx1_sexual_initiation_timing",
  "H1CO9",  "S24Q9 HOW OFTEN USE ...", "isx3_protective_method_use",
  "H1CO16A", "S24Q16A EVER DIAGNOSED-CHLAMYDIA-W1", "sexual_health_outcome_not_isx",
  "H1CO16C", "S24Q16A EVER DIAGNOSED-GONORRHEA-W1", "sexual_health_outcome_not_isx"
)

# ------------------------------------------------------------
# 5. Component classification
# ------------------------------------------------------------

classify_h1co_component <- function(variable, variable_label, known_hint = "") {

  text_l <- lower_clean(paste(variable, variable_label, known_hint, sep = " | "))
  var_u <- stringr::str_to_upper(variable)

  dplyr::case_when(
    var_u == "H1CO1" |
      stringr::str_detect(text_l, "ever have sex|ever had sex|ever.*sexual intercourse") ~
      "isx_support_ever_sex",

    var_u %in% c("H1CO2Y", "H1CO2M") |
      stringr::str_detect(
        text_l,
        "first time sex|first.*time.*sex|first.*intercourse|first.*sex.*year|first.*sex.*month|age.*first.*sex"
      ) ~
      "isx1_sexual_initiation_timing",

    stringr::str_detect(
      text_l,
      "number.*partner|how many.*partner|sex.*partner|sexual.*partner|partners"
    ) ~
      "isx2_partner_exposure",

    var_u == "H1CO9" |
      stringr::str_detect(
        text_l,
        "condom|birth control|contraceptive|contracep|used.*method|use.*method|how often use"
      ) ~
      "isx3_protective_method_use",

    stringr::str_detect(
      text_l,
      "how often.*sex|how oft.*sex|times.*sex|sex.*times|intercourse.*times|frequency.*sex|sexual activity"
    ) ~
      "isx4_sexual_frequency_exposure",

    stringr::str_detect(
      text_l,
      "pregnan|diagnos|diagnosed|chlamydia|gonorrhea|std|sti|hiv|aids"
    ) ~
      "sexual_health_outcome_not_isx",

    TRUE ~
      "h1co_unclassified_manual_review"
  )
}

component_label <- function(component) {
  dplyr::case_when(
    component == "isx_support_ever_sex" ~
      "Support variable: ever had sexual intercourse",
    component == "isx1_sexual_initiation_timing" ~
      "ISX1: sexual initiation / timing of first sex",
    component == "isx2_partner_exposure" ~
      "ISX2: number of partners / partner exposure",
    component == "isx3_protective_method_use" ~
      "ISX3: condom / contraceptive / birth-control use",
    component == "isx4_sexual_frequency_exposure" ~
      "ISX4: sexual frequency / exposure intensity",
    component == "sexual_health_outcome_not_isx" ~
      "Sexual health outcome, not ISX behavior item",
    TRUE ~
      "H1CO variable requiring manual review"
  )
}

candidate_role_from_component <- function(component, valid_n, distinct_valid_n) {
  dplyr::case_when(
    component == "isx_support_ever_sex" &
      valid_n > 0 ~
      "isx_support_variable",

    component %in% c(
      "isx1_sexual_initiation_timing",
      "isx2_partner_exposure",
      "isx3_protective_method_use",
      "isx4_sexual_frequency_exposure"
    ) &
      valid_n >= 500 &
      distinct_valid_n >= 2 ~
      "candidate_main_or_proxy_isx_item",

    component %in% c(
      "isx1_sexual_initiation_timing",
      "isx2_partner_exposure",
      "isx3_protective_method_use",
      "isx4_sexual_frequency_exposure"
    ) &
      valid_n > 0 &
      distinct_valid_n >= 2 ~
      "candidate_low_valid_n_isx_item",

    component == "sexual_health_outcome_not_isx" ~
      "outcome_not_isx_item",

    valid_n > 0 ~
      "manual_review_available_h1co",

    TRUE ~
      "unavailable_or_empty_h1co"
  )
}

role_rank <- function(role) {
  dplyr::case_when(
    role == "candidate_main_or_proxy_isx_item" ~ 1L,
    role == "candidate_low_valid_n_isx_item" ~ 2L,
    role == "isx_support_variable" ~ 3L,
    role == "manual_review_available_h1co" ~ 4L,
    role == "outcome_not_isx_item" ~ 5L,
    role == "unavailable_or_empty_h1co" ~ 6L,
    TRUE ~ 9L
  )
}

# ------------------------------------------------------------
# 6. Data file readers
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
# 7. Locate local respondent-level data files
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
# 8. Build all H1CO occurrences inventory
# ------------------------------------------------------------

occurrence_list <- list()
source_file_summary_list <- list()

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

    h1co_vars <- names(df)[stringr::str_detect(stringr::str_to_upper(names(df)), "^H1CO")]

    source_file_summary_list[[length(source_file_summary_list) + 1]] <- tibble(
      source_file = fp,
      file_name = basename(fp),
      object_name = obj_name,
      rows = nrow(df),
      columns = ncol(df),
      h1co_variables_detected = length(h1co_vars)
    )

    if (length(h1co_vars) == 0) {
      next
    }

    for (var in h1co_vars) {

      x <- df[[var]]

      var_label <- clean_chr(variable_label_string(x))
      val_labels <- clean_chr(value_label_string(x))

      previous_lookup_row <- previous_label_lookup %>%
        filter(variable == var) %>%
        slice_head(n = 1)

      recovered_label <- if (nrow(previous_lookup_row) > 0) {
        previous_lookup_row$recovered_variable_label[1]
      } else {
        ""
      }

      recovered_value_labels <- if (nrow(previous_lookup_row) > 0) {
        previous_lookup_row$recovered_value_labels[1]
      } else {
        ""
      }

      known_row <- known_h1co_hints %>%
        filter(variable == var) %>%
        slice_head(n = 1)

      known_hint <- if (nrow(known_row) > 0) {
        known_row$known_label_hint[1]
      } else {
        ""
      }

      effective_label <- dplyr::case_when(
        var_label != "" ~ var_label,
        recovered_label != "" ~ recovered_label,
        known_hint != "" ~ known_hint,
        TRUE ~ ""
      )

      effective_value_labels <- dplyr::case_when(
        val_labels != "" ~ val_labels,
        recovered_value_labels != "" ~ recovered_value_labels,
        TRUE ~ ""
      )

      x_num_raw <- to_numeric_safe(x)
      x_num_valid <- valid_numeric_vector(x, effective_value_labels)

      raw_non_missing_n <- sum(!is.na(x_num_raw))
      valid_n <- sum(!is.na(x_num_valid))
      missing_n <- sum(is.na(x_num_valid))
      valid_values <- sort(unique(x_num_valid[!is.na(x_num_valid)]))
      distinct_valid_n <- length(valid_values)

      occurrence_list[[length(occurrence_list) + 1]] <- tibble(
        source_file = fp,
        file_name = basename(fp),
        object_name = obj_name,
        object_rows = nrow(df),
        variable = var,
        variable_label_original = var_label,
        variable_label_recovered = recovered_label,
        variable_label_known_hint = known_hint,
        variable_label_effective = effective_label,
        value_labels_original = val_labels,
        value_labels_recovered = recovered_value_labels,
        value_labels_effective = effective_value_labels,
        raw_non_missing_n = raw_non_missing_n,
        valid_n = valid_n,
        missing_n = missing_n,
        distinct_valid_n = distinct_valid_n,
        valid_values = paste(valid_values, collapse = ", "),
        value_distribution = summarise_value_distribution(x, effective_value_labels),
        numeric_min = safe_min(x_num_valid),
        numeric_max = safe_max(x_num_valid),
        numeric_mean = safe_mean(x_num_valid),
        numeric_sd = safe_sd(x_num_valid),
        has_original_label = var_label != "",
        has_effective_label = effective_label != "",
        has_effective_value_labels = effective_value_labels != "",
        recovered_label_source = if (nrow(previous_lookup_row) > 0) {
          previous_lookup_row$recovered_label_source[1]
        } else {
          ""
        }
      )
    }
  }
}

h1co_all_occurrences <- bind_rows(occurrence_list)
source_file_summary <- bind_rows(source_file_summary_list)

if (nrow(h1co_all_occurrences) == 0) {
  stop("No H1CO variables were detected in the local respondent-level files.")
}

readr::write_csv(
  source_file_summary,
  file.path(audit_dir, "script18a_v4_source_file_summary.csv")
)

readr::write_csv(
  h1co_all_occurrences,
  file.path(audit_dir, "script18a_v4_h1co_all_occurrences.csv")
)

# ------------------------------------------------------------
# 9. Select best source for each H1CO variable
# ------------------------------------------------------------

h1co_best_source <- h1co_all_occurrences %>%
  mutate(
    label_quality_score =
      ifelse(has_original_label, 3L, 0L) +
      ifelse(has_effective_label, 2L, 0L) +
      ifelse(has_effective_value_labels, 1L, 0L),
    data_quality_score =
      ifelse(valid_n > 0, 10L, 0L) +
      ifelse(distinct_valid_n >= 2, 5L, 0L) +
      ifelse(raw_non_missing_n > 0, 3L, 0L),
    total_source_score =
      data_quality_score +
      label_quality_score +
      pmin(valid_n / 1000, 5)
  ) %>%
  arrange(
    variable,
    desc(valid_n),
    desc(distinct_valid_n),
    desc(total_source_score),
    desc(nchar(variable_label_effective)),
    desc(nchar(value_labels_effective)),
    file_name,
    object_name
  ) %>%
  group_by(variable) %>%
  mutate(
    occurrence_count_for_variable = n(),
    selected_best_source = row_number() == 1,
    alternative_sources_with_valid_data =
      sum(valid_n > 0, na.rm = TRUE) - as.integer(valid_n[1] > 0)
  ) %>%
  ungroup() %>%
  filter(selected_best_source) %>%
  left_join(
    known_h1co_hints,
    by = "variable"
  ) %>%
  mutate(
    known_label_hint = ifelse(is.na(known_label_hint), "", known_label_hint),
    known_component_hint = ifelse(is.na(known_component_hint), "", known_component_hint),
    isx_component = classify_h1co_component(
      variable,
      variable_label_effective,
      known_label_hint
    ),
    isx_component = ifelse(
      known_component_hint != "" &
        isx_component == "h1co_unclassified_manual_review",
      known_component_hint,
      isx_component
    ),
    isx_component_label = component_label(isx_component),
    clean_candidate_role = candidate_role_from_component(
      isx_component,
      valid_n,
      distinct_valid_n
    ),
    clean_candidate_role_rank = role_rank(clean_candidate_role),
    recovery_status = case_when(
      valid_n > 0 & has_effective_label ~ "recovered_with_data_and_label",
      valid_n > 0 & !has_effective_label ~ "recovered_with_data_label_missing",
      valid_n == 0 & has_effective_label ~ "label_recovered_but_no_valid_data",
      TRUE ~ "not_recovered_empty_or_unlabeled"
    )
  ) %>%
  arrange(
    clean_candidate_role_rank,
    isx_component,
    variable
  )

readr::write_csv(
  h1co_best_source,
  file.path(audit_dir, "script18a_v4_h1co_best_source_by_variable.csv")
)

# ------------------------------------------------------------
# 10. Recovered behavioral candidates and ISX map
# ------------------------------------------------------------

h1co_recovered_behavior_candidates <- h1co_best_source %>%
  filter(
    clean_candidate_role %in% c(
      "candidate_main_or_proxy_isx_item",
      "candidate_low_valid_n_isx_item",
      "isx_support_variable",
      "manual_review_available_h1co"
    )
  ) %>%
  select(
    variable,
    variable_label_effective,
    isx_component,
    isx_component_label,
    clean_candidate_role,
    clean_candidate_role_rank,
    recovery_status,
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
    clean_candidate_role_rank,
    isx_component,
    variable
  )


isx_candidate_map <- h1co_best_source %>%
  filter(
    isx_component %in% c(
      "isx_support_ever_sex",
      "isx1_sexual_initiation_timing",
      "isx2_partner_exposure",
      "isx3_protective_method_use",
      "isx4_sexual_frequency_exposure"
    )
  ) %>%
  select(
    isx_component,
    isx_component_label,
    variable,
    variable_label_effective,
    clean_candidate_role,
    recovery_status,
    valid_n,
    distinct_valid_n,
    valid_values,
    value_distribution,
    file_name,
    object_name,
    source_file,
    value_labels_effective
  ) %>%
  arrange(
    isx_component,
    role_rank(clean_candidate_role),
    desc(valid_n),
    variable
  )

readr::write_csv(
  h1co_recovered_behavior_candidates,
  file.path(audit_dir, "script18a_v4_h1co_recovered_behavior_candidates.csv")
)

readr::write_csv(
  isx_candidate_map,
  file.path(audit_dir, "script18a_v4_isx_candidate_map.csv")
)

# ------------------------------------------------------------
# 11. Manual selection template for Script 18b
# ------------------------------------------------------------

manual_selection_template <- isx_candidate_map %>%
  mutate(
    manual_use_in_script18b = case_when(
      clean_candidate_role %in% c(
        "candidate_main_or_proxy_isx_item",
        "candidate_low_valid_n_isx_item",
        "isx_support_variable"
      ) ~ "review",
      TRUE ~ "no"
    ),
    manual_final_isx_component = isx_component,
    manual_final_role = case_when(
      clean_candidate_role == "isx_support_variable" ~ "support_ever_sex_variable",
      clean_candidate_role == "candidate_main_or_proxy_isx_item" ~ "main_or_proxy_isx_item",
      clean_candidate_role == "candidate_low_valid_n_isx_item" ~ "low_valid_n_proxy_item",
      TRUE ~ "manual_review"
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
    isx_component,
    isx_component_label,
    variable,
    variable_label_effective,
    clean_candidate_role,
    recovery_status,
    valid_n,
    distinct_valid_n,
    valid_values,
    value_distribution,
    value_labels_effective,
    file_name,
    object_name,
    source_file
  )

readr::write_csv(
  manual_selection_template,
  file.path(audit_dir, "script18a_v4_isx_manual_selection_TEMPLATE.csv")
)

# ------------------------------------------------------------
# 12. Summaries
# ------------------------------------------------------------

h1co_recovery_summary <- h1co_best_source %>%
  count(recovery_status, clean_candidate_role, name = "variables") %>%
  arrange(recovery_status, role_rank(clean_candidate_role))

isx_component_summary <- h1co_best_source %>%
  group_by(isx_component, isx_component_label) %>%
  summarise(
    variables = n(),
    variables_with_valid_data = sum(valid_n > 0),
    variables_with_label = sum(has_effective_label),
    main_or_proxy_candidates = sum(clean_candidate_role == "candidate_main_or_proxy_isx_item"),
    low_valid_n_candidates = sum(clean_candidate_role == "candidate_low_valid_n_isx_item"),
    support_variables = sum(clean_candidate_role == "isx_support_variable"),
    manual_review_available = sum(clean_candidate_role == "manual_review_available_h1co"),
    outcome_variables = sum(clean_candidate_role == "outcome_not_isx_item"),
    best_variable = first(variable),
    best_label = first(variable_label_effective),
    best_valid_n = first(valid_n),
    .groups = "drop"
  ) %>%
  arrange(isx_component)

source_recovery_summary <- source_file_summary %>%
  group_by(file_name, object_name) %>%
  summarise(
    rows = max(rows, na.rm = TRUE),
    columns = max(columns, na.rm = TRUE),
    h1co_variables_detected = max(h1co_variables_detected, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(h1co_variables_detected), file_name, object_name)

readr::write_csv(
  h1co_recovery_summary,
  file.path(audit_dir, "script18a_v4_h1co_recovery_summary.csv")
)

readr::write_csv(
  isx_component_summary,
  file.path(audit_dir, "script18a_v4_isx_component_summary.csv")
)

readr::write_csv(
  source_recovery_summary,
  file.path(audit_dir, "script18a_v4_source_recovery_summary.csv")
)

# ------------------------------------------------------------
# 13. Scoring guide and methodological decisions
# ------------------------------------------------------------

scoring_guide <- tibble::tribble(
  ~isx_component, ~component_label, ~score_direction, ~score_4_meaning, ~score_3_meaning, ~score_2_meaning, ~score_1_meaning,
  "isx_support_ever_sex", "Support variable: ever had sexual intercourse", "Used to assign never-sex status", "Never had sexual intercourse", "Not applicable", "Not applicable", "Ever had sexual intercourse",
  "isx1_sexual_initiation_timing", "Sexual initiation / timing of first sex", "Higher score = later or no sexual initiation", "Never had sex", "Late initiation", "Intermediate initiation", "Early initiation",
  "isx2_partner_exposure", "Number of partners / partner exposure", "Higher score = fewer partners", "Never had sex", "One partner or low exposure", "Moderate exposure", "Multiple partners or high exposure",
  "isx3_protective_method_use", "Condom / contraceptive / birth-control use", "Higher score = greater protective method use", "Never had sex", "Consistent use", "Inconsistent use", "No use",
  "isx4_sexual_frequency_exposure", "Sexual frequency / exposure intensity", "Higher score = lower sexual exposure", "Never had sex", "Low frequency", "Moderate frequency", "High frequency"
)

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Reason for Script 18a v4", "Script 18a v3 revealed H1CO variables with blank labels and valid_n = 0. Script 18a v4 searches all occurrences and selects the best source per variable.",
  "Best-source rule", "The preferred occurrence is the one with highest valid_n, then highest distinct_valid_n, then strongest label and value-label information.",
  "Missing-code correction", "Values 6, 7, 8 and 9 are not treated as missing by default because they may be valid values for month or frequency variables.",
  "Label recovery", "If the selected source lacks a label, the script attempts to recover labels from previous audit outputs and conservative validated H1CO hints.",
  "ISX construction status", "This script still does not construct the ISX-equivalent index. It prepares the best-source map for manual review before Script 18b.",
  "Psychosocial separation", "Psychosocial variables remain excluded from the behavioral ISX dependent variable and will be handled later as independent variable blocks.",
  "Data protection", "Only aggregate variable-level summaries are exported; respondent-level data are not written."
)

readr::write_csv(
  scoring_guide,
  file.path(audit_dir, "script18a_v4_isx_scoring_guide.csv")
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18a_v4_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 14. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_h1co_best_source_recovery_script18a_v4.docx"
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

  report_best_source <- h1co_best_source %>%
    select(
      variable,
      variable_label_effective,
      isx_component_label,
      clean_candidate_role,
      recovery_status,
      valid_n,
      distinct_valid_n,
      valid_values,
      file_name,
      object_name
    ) %>%
    arrange(
      role_rank(clean_candidate_role),
      isx_component_label,
      variable
    ) %>%
    slice_head(n = 100)

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — H1CO Best Source Recovery Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18a v4 identifies the best available source for each H1CO variable across all local respondent-level files. This corrects the prior issue where some H1CO variables appeared as empty because an inferior occurrence was retained.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Source recovery summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(source_recovery_summary)) %>%
    officer::body_add_par("H1CO recovery summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(h1co_recovery_summary)) %>%
    officer::body_add_par("ISX component summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(isx_component_summary)) %>%
    officer::body_add_par("Best source by H1CO variable", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(report_best_source)) %>%
    officer::body_add_par("ISX scoring guide", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(scoring_guide)) %>%
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
    "h1co_all_occurrences_created",
    "h1co_best_source_created",
    "h1co_recovered_behavior_candidates_created",
    "isx_candidate_map_created",
    "manual_selection_template_created",
    "h1co_recovery_summary_created",
    "isx_component_summary_created",
    "scoring_guide_created",
    "methodological_decisions_created",
    "word_report_created",
    "manual_review_still_required"
  ),
  status = c(
    length(candidate_data_files) > 0,
    file.exists(file.path(audit_dir, "script18a_v4_h1co_all_occurrences.csv")),
    file.exists(file.path(audit_dir, "script18a_v4_h1co_best_source_by_variable.csv")),
    file.exists(file.path(audit_dir, "script18a_v4_h1co_recovered_behavior_candidates.csv")),
    file.exists(file.path(audit_dir, "script18a_v4_isx_candidate_map.csv")),
    file.exists(file.path(audit_dir, "script18a_v4_isx_manual_selection_TEMPLATE.csv")),
    file.exists(file.path(audit_dir, "script18a_v4_h1co_recovery_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_v4_isx_component_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_v4_isx_scoring_guide.csv")),
    file.exists(file.path(audit_dir, "script18a_v4_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18a_v4_final_status.csv")
)

# ------------------------------------------------------------
# 16. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18a v4 completed: H1CO Best Source Recovery Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nSource recovery summary:\n")
print(source_recovery_summary)

cat("\nH1CO recovery summary:\n")
print(h1co_recovery_summary)

cat("\nISX component summary:\n")
print(isx_component_summary)

cat("\nISX candidate map:\n")
print(
  isx_candidate_map %>%
    select(
      isx_component,
      isx_component_label,
      variable,
      variable_label_effective,
      clean_candidate_role,
      recovery_status,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution
    ),
  n = 100
)

cat("\nTop recovered H1CO variables by valid_n:\n")
print(
  h1co_best_source %>%
    arrange(desc(valid_n), variable) %>%
    select(
      variable,
      variable_label_effective,
      isx_component,
      clean_candidate_role,
      recovery_status,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution,
      file_name,
      object_name
    ) %>%
    slice_head(n = 60),
  n = 60
)

cat("\nOutputs created:\n")
cat("- ", file.path(audit_dir, "script18a_v4_source_file_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_previous_label_lookup.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_h1co_all_occurrences.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_h1co_best_source_by_variable.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_h1co_recovered_behavior_candidates.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_isx_candidate_map.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_isx_manual_selection_TEMPLATE.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_h1co_recovery_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_isx_component_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_source_recovery_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_isx_scoring_guide.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v4_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Review script18a_v4_isx_candidate_map.csv and script18a_v4_isx_manual_selection_TEMPLATE.csv before Script 18b.\n")
cat("Do not commit until the best-source recovery and ISX candidate selection are reviewed.\n")