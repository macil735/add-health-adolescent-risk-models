# ============================================================
# Script 18a — ISX Sexual Protection Variable Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Audit Add Health Wave I variables that can be used to construct
#   an ISX-style sexual protection behavior index, inspired by the
#   user's thesis index.
#
# ISX target dependent variable:
#   sexual_protection_index
#
# Thesis-style components:
#   isx1 — Age at first sexual intercourse
#   isx2 — Number of sexual partners in the last year
#   isx3 — Condom / contraceptive use in the last year
#   isx4 — Frequency of sexual intercourse in the last year
#
# Methodological position:
#   - Script 18a does not construct the index yet.
#   - Script 18a audits candidate Add Health variables.
#   - Script 18a creates a manual selection template.
#   - Script 18b will construct the sexual protection index after
#     manual variable selection is reviewed.
#
# Main outputs:
#   outputs/audits/script18a_isx_candidate_variable_inventory.csv
#   outputs/audits/script18a_isx_shortlisted_candidates.csv
#   outputs/audits/script18a_isx_manual_selection_TEMPLATE.csv
#   outputs/audits/script18a_isx_item_summary.csv
#   outputs/audits/script18a_isx_methodological_decisions.csv
#   docs/add_health_wave01_isx_sexual_protection_variable_audit_script18a.docx
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
cat("Script 18a started: ISX Sexual Protection Variable Audit\n")
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

  value_labels <- lower_clean(value_labels)

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

valid_numeric_vector <- function(x, value_labels) {

  x_num <- to_numeric_safe(x)
  missing_codes <- extract_missing_codes_from_labels(value_labels)

  x_num[x_num %in% missing_codes] <- NA_real_

  x_num
}

summarise_value_distribution <- function(x, value_labels, max_values = 20) {

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
# 3. ISX target item dictionary
# ------------------------------------------------------------

isx_dictionary <- tibble::tribble(
  ~isx_item, ~isx_item_label, ~thesis_scoring_logic, ~search_regex, ~negative_regex,
  "isx1_age_first_sex",
  "Age at first sexual intercourse",
  "Never had sex = 4; 18 years or older = 3; 16–17 years = 2; 15 years or younger = 1.",
  "age at first|first.*sex|first.*intercourse|first time.*sex|first time.*intercourse|first.*vaginal|age.*intercourse|sex.*age",
  "birth.*control|contracep|condom|pregnan|diagnos|std|sti|hiv|aids|partner|frequency|times",

  "isx2_partners_last_year",
  "Number of sexual partners in the last year",
  "Never had sex = 4; one partner = 3; two partners = 2; three or more partners = 1.",
  "number.*partner|partners.*last|partner.*past|sex partners|sexual partners|how many.*partner|many people.*sex",
  "first time|age at first|condom|contracep|birth.*control|frequency|times.*sex|diagnos|std|sti|hiv|aids",

  "isx3_condom_contraceptive_use",
  "Frequency of condom or contraceptive use in the last year",
  "Never had sex = 4; always = 3; sometimes = 2; never = 1.",
  "condom|contracep|birth control|protection|prevent.*pregnan|used.*method|use.*method",
  "diagnos|std|sti|hiv|aids|risk of|chance of|likely|get pregnant|attitude|morally|expensive|bothersome|easy to get",

  "isx4_sex_frequency_last_year",
  "Frequency of sexual intercourse in the last year",
  "Never had sex = 4; 1–2 times per year = 3; 1–3 times per month = 2; weekly or more = 1.",
  "frequency.*sex|how often.*sex|times.*sex|sex.*times|sexual intercourse.*times|intercourse.*times|past year.*sex|last year.*sex",
  "first time|age at first|partner|condom|contracep|birth.*control|diagnos|std|sti|hiv|aids"
)

# ------------------------------------------------------------
# 4. Data file readers
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
# 5. Locate respondent-level data files
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
# 6. Build candidate variable inventory
# ------------------------------------------------------------

candidate_inventory_list <- list()

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
      var_label <- variable_label_string(x)
      val_labels <- value_label_string(x)

      text_name_label <- paste(var, var_label, sep = " | ")
      text_all <- paste(var, var_label, val_labels, sep = " | ")

      text_l <- lower_clean(text_name_label)

      x_num <- valid_numeric_vector(x, val_labels)

      valid_values <- sort(unique(x_num[!is.na(x_num)]))
      valid_n <- sum(!is.na(x_num))
      missing_n <- sum(is.na(x_num))
      distinct_valid_n <- length(valid_values)

      for (i in seq_len(nrow(isx_dictionary))) {

        item <- isx_dictionary[i, ]

        positive_match <- stringr::str_detect(text_l, item$search_regex)
        negative_match <- stringr::str_detect(text_l, item$negative_regex)

        if (!positive_match) {
          next
        }

        candidate_priority_score <- 0

        candidate_priority_score <- candidate_priority_score +
          ifelse(positive_match, 5, 0) -
          ifelse(negative_match, 3, 0) +
          ifelse(valid_n >= 500, 2, 0) +
          ifelse(distinct_valid_n >= 2, 1, 0)

        candidate_inventory_list[[length(candidate_inventory_list) + 1]] <- tibble(
          isx_item = item$isx_item,
          isx_item_label = item$isx_item_label,
          thesis_scoring_logic = item$thesis_scoring_logic,
          source_file = fp,
          file_name = basename(fp),
          object_name = obj_name,
          variable = var,
          variable_label = var_label,
          value_labels = val_labels,
          candidate_text = text_all,
          positive_match = positive_match,
          negative_match = negative_match,
          candidate_priority_score = candidate_priority_score,
          valid_n = valid_n,
          missing_n = missing_n,
          distinct_valid_n = distinct_valid_n,
          valid_values = paste(valid_values, collapse = ", "),
          value_distribution = summarise_value_distribution(x, val_labels),
          numeric_min = safe_min(x_num),
          numeric_max = safe_max(x_num),
          numeric_mean = safe_mean(x_num),
          numeric_sd = safe_sd(x_num)
        )
      }
    }
  }
}

candidate_inventory <- bind_rows(candidate_inventory_list)

if (nrow(candidate_inventory) == 0) {
  stop(
    "No ISX-style candidate variables were detected. ",
    "Search terms may need to be expanded after inspecting the codebook."
  )
}

candidate_inventory <- candidate_inventory %>%
  arrange(
    isx_item,
    desc(candidate_priority_score),
    negative_match,
    variable
  )

readr::write_csv(
  candidate_inventory,
  file.path(audit_dir, "script18a_isx_candidate_variable_inventory.csv")
)

# ------------------------------------------------------------
# 7. Shortlist candidates
# ------------------------------------------------------------

shortlisted_candidates <- candidate_inventory %>%
  group_by(isx_item) %>%
  arrange(
    desc(candidate_priority_score),
    negative_match,
    desc(valid_n),
    variable,
    .by_group = TRUE
  ) %>%
  slice_head(n = 15) %>%
  ungroup() %>%
  mutate(
    audit_recommendation = case_when(
      negative_match ~ "review_carefully_possible_false_positive",
      valid_n < 200 ~ "review_low_valid_n",
      distinct_valid_n < 2 ~ "review_no_variation",
      TRUE ~ "review_candidate_for_manual_selection"
    )
  )

readr::write_csv(
  shortlisted_candidates,
  file.path(audit_dir, "script18a_isx_shortlisted_candidates.csv")
)

# ------------------------------------------------------------
# 8. Manual selection template
# ------------------------------------------------------------

manual_selection_template <- shortlisted_candidates %>%
  mutate(
    manual_select_for_isx = "review",
    manual_isx_item = isx_item,
    manual_final_variable_role = "",
    manual_score_4_rule = "",
    manual_score_3_rule = "",
    manual_score_2_rule = "",
    manual_score_1_rule = "",
    manual_never_had_sex_handling = "Score 4 when respondent never had sexual intercourse, if a valid ever-sex indicator is available.",
    manual_missing_codes = "",
    manual_reverse_score_needed = "",
    manual_decision_rationale = "",
    manual_reviewer = "",
    manual_review_date = ""
  ) %>%
  select(
    manual_select_for_isx,
    manual_isx_item,
    manual_final_variable_role,
    manual_score_4_rule,
    manual_score_3_rule,
    manual_score_2_rule,
    manual_score_1_rule,
    manual_never_had_sex_handling,
    manual_missing_codes,
    manual_reverse_score_needed,
    manual_decision_rationale,
    manual_reviewer,
    manual_review_date,
    isx_item,
    isx_item_label,
    thesis_scoring_logic,
    variable,
    variable_label,
    value_labels,
    valid_n,
    missing_n,
    distinct_valid_n,
    valid_values,
    value_distribution,
    candidate_priority_score,
    audit_recommendation,
    negative_match,
    file_name,
    object_name,
    source_file,
    numeric_min,
    numeric_max,
    numeric_mean,
    numeric_sd
  )

readr::write_csv(
  manual_selection_template,
  file.path(audit_dir, "script18a_isx_manual_selection_TEMPLATE.csv")
)

# ------------------------------------------------------------
# 9. Item-level summaries
# ------------------------------------------------------------

isx_item_summary <- candidate_inventory %>%
  group_by(isx_item, isx_item_label, thesis_scoring_logic) %>%
  summarise(
    candidate_variables_detected = n_distinct(variable),
    candidate_rows_detected = n(),
    max_priority_score = max(candidate_priority_score, na.rm = TRUE),
    candidates_with_valid_n_500 = sum(valid_n >= 500),
    candidates_with_variation = sum(distinct_valid_n >= 2),
    candidates_flagged_negative_match = sum(negative_match),
    .groups = "drop"
  ) %>%
  arrange(isx_item)

shortlist_summary <- shortlisted_candidates %>%
  group_by(isx_item, isx_item_label) %>%
  summarise(
    shortlisted_rows = n(),
    unique_shortlisted_variables = n_distinct(variable),
    best_candidate = first(variable),
    best_candidate_label = first(variable_label),
    best_candidate_valid_n = first(valid_n),
    best_candidate_values = first(valid_values),
    .groups = "drop"
  ) %>%
  arrange(isx_item)

readr::write_csv(
  isx_item_summary,
  file.path(audit_dir, "script18a_isx_item_summary.csv")
)

readr::write_csv(
  shortlist_summary,
  file.path(audit_dir, "script18a_isx_shortlist_summary.csv")
)

# ------------------------------------------------------------
# 10. ISX scoring guide
# ------------------------------------------------------------

isx_scoring_guide <- tibble::tribble(
  ~isx_item, ~score_4, ~score_3, ~score_2, ~score_1, ~required_manual_decision,
  "isx1_age_first_sex",
  "Never had sex.",
  "First sex at age 18 or older.",
  "First sex at age 16–17.",
  "First sex at age 15 or younger.",
  "Identify Add Health variables for ever had sex and age at first intercourse.",

  "isx2_partners_last_year",
  "Never had sex.",
  "One partner in the last year.",
  "Two partners in the last year.",
  "Three or more partners in the last year.",
  "Identify Add Health variable for number of sexual partners in the past year or closest available period.",

  "isx3_condom_contraceptive_use",
  "Never had sex.",
  "Always used condom or contraceptive method.",
  "Sometimes used condom or contraceptive method.",
  "Never used condom or contraceptive method.",
  "Identify whether the available variable measures condom use, contraceptive use, first sex, most recent sex, or last-year frequency.",

  "isx4_sex_frequency_last_year",
  "Never had sex.",
  "Sexual intercourse 1–2 times per year.",
  "Sexual intercourse 1–3 times per month.",
  "Sexual intercourse weekly or more.",
  "Identify Add Health variable for frequency of sexual intercourse in the past year or closest available period."
)

readr::write_csv(
  isx_scoring_guide,
  file.path(audit_dir, "script18a_isx_scoring_guide.csv")
)

# ------------------------------------------------------------
# 11. Methodological decisions
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Dependent variable", "The intended dependent variable is an ISX-style sexual protection behavior index, not the psychosocial reviewed_protection_index.",
  "Index source logic", "The ISX-style index is inspired by the user's thesis table and focuses on sexual behavior protection: age at first sex, number of partners, condom/contraceptive use, and sexual frequency.",
  "Script 18a scope", "Script 18a audits candidate Add Health variables only. It does not construct the final index.",
  "Manual review requirement", "Candidate variables must be manually reviewed before Script 18b constructs the index.",
  "Never had sex category", "The thesis-style scoring treats never having had sexual intercourse as the highest protection score. Script 18b should implement this only if an appropriate ever-sex indicator is available.",
  "Score direction", "Higher ISX scores should represent higher sexual protection.",
  "Psychosocial predictors", "Perceptions, self-efficacy, peers, family, school and future orientation should be treated as independent variables in later models predicting the ISX-style dependent variable.",
  "Avoid circularity", "The psychosocial reviewed_protection_index should not be treated as the same construct as the ISX-style sexual protection behavior index."
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18a_isx_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 12. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_isx_sexual_protection_variable_audit_script18a.docx"
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

  report_shortlist <- shortlisted_candidates %>%
    select(
      isx_item,
      variable,
      variable_label,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution,
      candidate_priority_score,
      audit_recommendation
    ) %>%
    slice_head(n = 60)

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — ISX Sexual Protection Variable Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18a audits Add Health variables that may be used to construct an ISX-style sexual protection behavior index. This index is intended to serve as the dependent variable in later models. Psychosocial blocks such as perceptions, self-efficacy, peer context, school connectedness and family support will later be treated as independent variables.",
      style = "Normal"
    ) %>%
    officer::body_add_par("ISX scoring guide", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(isx_scoring_guide)) %>%
    officer::body_add_par("ISX item summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(isx_item_summary)) %>%
    officer::body_add_par("Shortlist summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(shortlist_summary)) %>%
    officer::body_add_par("Shortlisted candidate variables", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(report_shortlist)) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(methodological_decisions))

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 13. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "candidate_data_files_found",
    "candidate_inventory_created",
    "shortlisted_candidates_created",
    "manual_selection_template_created",
    "isx_item_summary_created",
    "isx_scoring_guide_created",
    "methodological_decisions_created",
    "word_report_created",
    "manual_review_still_required"
  ),
  status = c(
    length(candidate_data_files) > 0,
    file.exists(file.path(audit_dir, "script18a_isx_candidate_variable_inventory.csv")),
    file.exists(file.path(audit_dir, "script18a_isx_shortlisted_candidates.csv")),
    file.exists(file.path(audit_dir, "script18a_isx_manual_selection_TEMPLATE.csv")),
    file.exists(file.path(audit_dir, "script18a_isx_item_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_isx_scoring_guide.csv")),
    file.exists(file.path(audit_dir, "script18a_isx_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18a_isx_final_status.csv")
)

# ------------------------------------------------------------
# 14. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18a completed: ISX Sexual Protection Variable Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nISX item summary:\n")
print(isx_item_summary)

cat("\nShortlist summary:\n")
print(shortlist_summary)

cat("\nShortlisted candidate variables preview:\n")
print(
  shortlisted_candidates %>%
    select(
      isx_item,
      variable,
      variable_label,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution,
      candidate_priority_score,
      audit_recommendation
    ) %>%
    print(n = 60)
)

cat("\nOutputs created:\n")
cat("- ", file.path(audit_dir, "script18a_isx_candidate_variable_inventory.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_isx_shortlisted_candidates.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_isx_manual_selection_TEMPLATE.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_isx_item_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_isx_shortlist_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_isx_scoring_guide.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_isx_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_isx_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Review script18a_isx_manual_selection_TEMPLATE.csv before constructing the ISX-style index in Script 18b.\n")
cat("Do not commit until the candidate variables are reviewed.\n")