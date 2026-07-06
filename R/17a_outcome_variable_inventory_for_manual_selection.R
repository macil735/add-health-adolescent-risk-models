# ============================================================
# Script 17a — Outcome Variable Inventory for Manual Selection
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Create a broad inventory of possible adolescent risk outcome
#   variables available in the local respondent-level Add Health data.
#
# Why this script exists:
#   Script 17 should not automatically model variables that are not
#   substantive risk outcomes, such as knowledge, perception, education
#   or attitude variables. Script 17a creates a manual selection template
#   so that final outcomes can be selected transparently before modelling.
#
# Main outputs:
#   outputs/audits/script17a_all_variable_inventory.csv
#   outputs/audits/script17a_candidate_outcome_inventory.csv
#   outputs/audits/script17a_manual_outcome_selection_TEMPLATE.csv
#   outputs/audits/script17a_domain_summary.csv
#   docs/add_health_wave01_outcome_variable_inventory_script17a.docx
#
# Next step:
#   Manually review script17a_manual_outcome_selection_TEMPLATE.csv
#   and save the approved version as:
#   outputs/audits/script17a_manual_outcome_selection_COMPLETED.csv
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
cat("Script 17a started: Outcome Variable Inventory\n")
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

valid_numeric_values <- function(x, value_labels) {

  x_num <- to_numeric_safe(x)
  missing_codes <- extract_missing_codes_from_labels(value_labels)
  x_num[x_num %in% missing_codes] <- NA_real_

  sort(unique(x_num[!is.na(x_num)]))
}

summarise_value_distribution <- function(x, value_labels, max_values = 12) {

  x_num <- to_numeric_safe(x)
  missing_codes <- extract_missing_codes_from_labels(value_labels)
  x_num[x_num %in% missing_codes] <- NA_real_

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
# 3. Domain dictionaries
# ------------------------------------------------------------

sexual_initiation_regex <- paste(
  c(
    "sexual intercourse",
    "vaginal intercourse",
    "ever had sex",
    "ever have sex",
    "had sex",
    "have sex",
    "sexually active",
    "first sex",
    "first intercourse",
    "intercourse"
  ),
  collapse = "|"
)

pregnancy_regex <- paste(
  c(
    "pregnan",
    "got pregnant",
    "been pregnant",
    "made someone pregnant",
    "birth",
    "gave birth",
    "childbearing",
    "had a baby",
    "live birth"
  ),
  collapse = "|"
)

sexual_health_outcome_regex <- paste(
  c(
    "std",
    "sti",
    "sexually transmitted",
    "chlamydia",
    "gonorrhea",
    "syphilis",
    "hiv positive",
    "tested positive",
    "diagnosed"
  ),
  collapse = "|"
)

tobacco_regex <- paste(
  c(
    "cigarette",
    "cigarettes",
    "smok",
    "tobacco",
    "chewing tobacco",
    "dip",
    "snuff"
  ),
  collapse = "|"
)

alcohol_regex <- paste(
  c(
    "alcohol",
    "drink",
    "drinks",
    "drunk",
    "binge"
  ),
  collapse = "|"
)

substance_regex <- paste(
  c(
    "marijuana",
    "weed",
    "drug",
    "substance",
    "cocaine",
    "crack",
    "inhalant",
    "illegal drug",
    "pills",
    "steroids"
  ),
  collapse = "|"
)

violence_regex <- paste(
  c(
    "fight",
    "fighting",
    "weapon",
    "knife",
    "gun",
    "shot",
    "stab",
    "violence",
    "violent",
    "gang",
    "delinquen",
    "arrest",
    "steal",
    "stole",
    "damage property",
    "property damage"
  ),
  collapse = "|"
)

school_risk_regex <- paste(
  c(
    "skip school",
    "skipped school",
    "suspend",
    "suspended",
    "expelled",
    "trouble at school",
    "school trouble",
    "absent",
    "truancy"
  ),
  collapse = "|"
)

exclude_construct_or_knowledge_regex <- paste(
  c(
    "risk of",
    "chance of",
    "likely",
    "how likely",
    "would get",
    "without protection",
    "without condom",
    "without birth control",
    "learned",
    "heard",
    "knowledge",
    "taught",
    "education",
    "class",
    "course",
    "information",
    "aids education",
    "hiv education",
    "attitude",
    "approve",
    "disapprove",
    "belief",
    "feel",
    "felt",
    "close to",
    "understand",
    "safe in your school",
    "school connectedness",
    "family",
    "parent",
    "mother",
    "father",
    "relig",
    "church",
    "pray",
    "future",
    "expect",
    "college",
    "grade",
    "aspiration"
  ),
  collapse = "|"
)

administrative_regex <- paste(
  c(
    "caseid",
    "aid",
    "respondent id",
    "weight",
    "wgt",
    "sample",
    "flag",
    "wave",
    "section",
    "interviewer"
  ),
  collapse = "|"
)

assign_domain <- function(text_l) {

  dplyr::case_when(
    stringr::str_detect(text_l, sexual_health_outcome_regex) ~ "sexual health diagnosis",
    stringr::str_detect(text_l, pregnancy_regex) ~ "pregnancy or childbearing",
    stringr::str_detect(text_l, sexual_initiation_regex) ~ "sexual initiation",
    stringr::str_detect(text_l, tobacco_regex) ~ "tobacco use",
    stringr::str_detect(text_l, alcohol_regex) ~ "alcohol use",
    stringr::str_detect(text_l, substance_regex) ~ "substance use",
    stringr::str_detect(text_l, violence_regex) ~ "violence or delinquency",
    stringr::str_detect(text_l, school_risk_regex) ~ "school risk behavior",
    TRUE ~ "other"
  )
}

domain_priority <- function(domain) {
  dplyr::case_when(
    domain == "sexual initiation" ~ 1L,
    domain == "pregnancy or childbearing" ~ 2L,
    domain == "sexual health diagnosis" ~ 3L,
    domain == "tobacco use" ~ 4L,
    domain == "alcohol use" ~ 5L,
    domain == "substance use" ~ 6L,
    domain == "violence or delinquency" ~ 7L,
    domain == "school risk behavior" ~ 8L,
    TRUE ~ 99L
  )
}

# ------------------------------------------------------------
# 4. Read local respondent-level data files
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
    "Copy the local data folder into the project directory, but do not commit it."
  )
}

cat("Candidate respondent-level data files found:", length(candidate_data_files), "\n\n")

# ------------------------------------------------------------
# 5. Build full variable inventory
# ------------------------------------------------------------

inventory_list <- list()

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

text_l <- stringr::str_to_lower(clean_chr(text_name_label))
text_all_l <- stringr::str_to_lower(clean_chr(text_all))

      x_num <- to_numeric_safe(x)
      missing_codes <- extract_missing_codes_from_labels(val_labels)
      x_num_clean <- x_num
      x_num_clean[x_num_clean %in% missing_codes] <- NA_real_

      valid_values <- sort(unique(x_num_clean[!is.na(x_num_clean)]))
      valid_n <- sum(!is.na(x_num_clean))
      missing_n <- sum(is.na(x_num_clean))
      distinct_valid_n <- length(valid_values)

      domain <- assign_domain(text_l)

      is_admin <- stringr::str_detect(
        stringr::str_to_lower(paste(var, var_label)),
        administrative_regex
      )

      is_excluded_construct <- stringr::str_detect(
        text_l,
        exclude_construct_or_knowledge_regex
      )

      is_candidate_domain <- domain != "other"

      likely_binary <- distinct_valid_n == 2
      plausible_binary <- distinct_valid_n <= 4 & distinct_valid_n >= 2

      candidate_status <- dplyr::case_when(
        is_admin ~ "exclude_administrative",
        is_candidate_domain & is_excluded_construct ~ "exclude_knowledge_attitude_perception_or_construct",
        is_candidate_domain & likely_binary & valid_n >= 500 ~ "high_priority_candidate_binary_outcome",
        is_candidate_domain & plausible_binary & valid_n >= 500 ~ "possible_candidate_needs_binary_review",
        is_candidate_domain & valid_n >= 500 ~ "candidate_domain_nonbinary_review",
        is_candidate_domain & valid_n < 500 ~ "candidate_domain_low_valid_n",
        TRUE ~ "not_candidate_by_dictionary"
      )

      inventory_list[[length(inventory_list) + 1]] <- tibble(
        source_file = fp,
        file_name = basename(fp),
        object_name = obj_name,
        variable = var,
        variable_label = var_label,
        value_labels = val_labels,
        candidate_text = text_all,
        inferred_domain = domain,
        domain_priority = domain_priority(domain),
        candidate_status = candidate_status,
        likely_binary = likely_binary,
        plausible_binary = plausible_binary,
        valid_n = valid_n,
        missing_n = missing_n,
        distinct_valid_n = distinct_valid_n,
        valid_values = paste(valid_values, collapse = ", "),
        value_distribution = summarise_value_distribution(x, val_labels),
        numeric_min = safe_min(x_num_clean),
        numeric_max = safe_max(x_num_clean),
        numeric_mean = safe_mean(x_num_clean),
        numeric_sd = safe_sd(x_num_clean)
      )
    }
  }
}

all_variable_inventory <- bind_rows(inventory_list)

if (nrow(all_variable_inventory) == 0) {
  stop("No variables were inventoried from the available data files.")
}

all_variable_inventory <- all_variable_inventory %>%
  arrange(
    domain_priority,
    candidate_status,
    file_name,
    object_name,
    variable
  )

readr::write_csv(
  all_variable_inventory,
  file.path(audit_dir, "script17a_all_variable_inventory.csv")
)

# ------------------------------------------------------------
# 6. Candidate outcome inventory
# ------------------------------------------------------------

all_variable_inventory_unique <- all_variable_inventory %>%
  group_by(variable) %>%
  arrange(
    domain_priority,
    candidate_status,
    desc(valid_n),
    file_name,
    object_name
  ) %>%
  slice(1) %>%
  ungroup()

candidate_outcome_inventory <- all_variable_inventory_unique %>%
  filter(
    inferred_domain != "other",
    !candidate_status %in% c(
      "exclude_administrative",
      "not_candidate_by_dictionary"
    )
  ) %>%
  arrange(
    domain_priority,
    candidate_status,
    variable
  )

readr::write_csv(
  candidate_outcome_inventory,
  file.path(audit_dir, "script17a_candidate_outcome_inventory.csv")
)

high_priority_candidates <- candidate_outcome_inventory %>%
  filter(candidate_status == "high_priority_candidate_binary_outcome")

possible_candidates <- candidate_outcome_inventory %>%
  filter(candidate_status != "high_priority_candidate_binary_outcome")

# ------------------------------------------------------------
# 7. Manual selection template
# ------------------------------------------------------------

manual_selection_template <- candidate_outcome_inventory %>%
  mutate(
    manual_select_outcome = case_when(
      candidate_status == "high_priority_candidate_binary_outcome" ~ "review",
      TRUE ~ "no"
    ),
    manual_outcome_domain = inferred_domain,
    manual_event_code = "",
    manual_non_event_code = "",
    manual_outcome_label = "",
    manual_use_in_script17 = "no",
    manual_decision_rationale = case_when(
      candidate_status == "exclude_knowledge_attitude_perception_or_construct" ~
        "Excluded by default because the item appears to measure knowledge, education, perception, attitude or construct content rather than a behavioral/event outcome.",
      candidate_status == "high_priority_candidate_binary_outcome" ~
        "Review manually. If this is a real behavioral/event outcome, set manual_use_in_script17 to yes and define event/non-event codes.",
      TRUE ~
        "Manual review required before use."
    ),
    manual_reviewer = "",
    manual_review_date = ""
  ) %>%
  select(
    manual_select_outcome,
    manual_use_in_script17,
    manual_outcome_domain,
    manual_event_code,
    manual_non_event_code,
    manual_outcome_label,
    manual_decision_rationale,
    manual_reviewer,
    manual_review_date,
    variable,
    variable_label,
    value_labels,
    inferred_domain,
    candidate_status,
    likely_binary,
    plausible_binary,
    valid_n,
    missing_n,
    distinct_valid_n,
    valid_values,
    value_distribution,
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
  file.path(audit_dir, "script17a_manual_outcome_selection_TEMPLATE.csv")
)

# ------------------------------------------------------------
# 8. Summaries
# ------------------------------------------------------------

domain_summary <- candidate_outcome_inventory %>%
  count(
    inferred_domain,
    candidate_status,
    name = "variables"
  ) %>%
  arrange(
    domain_priority(inferred_domain),
    candidate_status
  )

candidate_status_summary <- all_variable_inventory %>%
  count(candidate_status, name = "variables") %>%
  arrange(desc(variables))

file_summary <- all_variable_inventory %>%
  count(file_name, object_name, name = "variables_inventoried") %>%
  arrange(file_name, object_name)

manual_review_burden <- tibble(
  total_variables_inventoried = nrow(all_variable_inventory),
  candidate_outcome_variables = nrow(candidate_outcome_inventory),
  high_priority_binary_candidates = nrow(high_priority_candidates),
  possible_or_excluded_candidates_for_review = nrow(possible_candidates),
  manual_template_rows = nrow(manual_selection_template)
)

readr::write_csv(
  domain_summary,
  file.path(audit_dir, "script17a_domain_summary.csv")
)

readr::write_csv(
  candidate_status_summary,
  file.path(audit_dir, "script17a_candidate_status_summary.csv")
)

readr::write_csv(
  file_summary,
  file.path(audit_dir, "script17a_file_summary.csv")
)

readr::write_csv(
  manual_review_burden,
  file.path(audit_dir, "script17a_manual_review_burden.csv")
)

# ------------------------------------------------------------
# 9. Methodological decisions
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Purpose", "Script 17a inventories possible outcome variables before modelling, because fully automatic outcome selection may select knowledge, education or perception variables instead of behavioral risk outcomes.",
  "Candidate identification", "Candidate domains are identified using variable names, labels and value labels.",
  "Outcome domains", "Domains include sexual initiation, pregnancy/childbearing, sexual health diagnosis, tobacco use, alcohol use, substance use, violence/delinquency and school risk behavior.",
  "Default exclusion", "Variables that appear to measure knowledge, education, attitudes, perceptions, family, school connectedness, religiosity or future orientation are excluded by default from direct outcome modelling.",
  "Manual template", "The manual selection template must be reviewed before Script 17 is re-run.",
  "Event coding", "For each selected binary outcome, the analyst must define the event code and non-event code.",
  "No respondent-level data publication", "The generated CSV outputs are local audit outputs and should not be committed if they contain respondent-level summaries that are not intended for public release.",
  "Next step", "Save the reviewed template as script17a_manual_outcome_selection_COMPLETED.csv and then revise Script 17 to use that completed manual selection."
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script17a_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 10. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_outcome_variable_inventory_script17a.docx"
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

  report_candidates <- candidate_outcome_inventory %>%
    select(
      variable,
      variable_label,
      inferred_domain,
      candidate_status,
      valid_n,
      distinct_valid_n,
      valid_values
    ) %>%
    slice_head(n = 40)

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — Outcome Variable Inventory for Manual Selection",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 17a creates an inventory of possible adolescent risk outcome variables. The purpose is to prevent Script 17 from modelling knowledge, education, perception or construct variables as if they were behavioral risk outcomes.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Manual review burden", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(manual_review_burden)
    ) %>%
    officer::body_add_par("Candidate status summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(candidate_status_summary)
    ) %>%
    officer::body_add_par("Domain summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(domain_summary)
    ) %>%
    officer::body_add_par("Candidate outcome inventory preview", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(report_candidates)
    ) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(methodological_decisions)
    ) %>%
    officer::body_add_par("Required manual action", style = "heading 2") %>%
    officer::body_add_par(
      paste0(
        "Open outputs/audits/script17a_manual_outcome_selection_TEMPLATE.csv. ",
        "Select only true behavioral or event outcomes. For each selected outcome, define manual_use_in_script17 = yes, ",
        "manual_event_code, manual_non_event_code and manual_outcome_label. ",
        "Save the reviewed file as outputs/audits/script17a_manual_outcome_selection_COMPLETED.csv."
      ),
      style = "Normal"
    )

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 11. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "candidate_data_files_found",
    "all_variable_inventory_created",
    "candidate_outcome_inventory_created",
    "manual_selection_template_created",
    "domain_summary_created",
    "word_report_created",
    "manual_outcome_selection_still_required"
  ),
  status = c(
    length(candidate_data_files) > 0,
    file.exists(file.path(audit_dir, "script17a_all_variable_inventory.csv")),
    file.exists(file.path(audit_dir, "script17a_candidate_outcome_inventory.csv")),
    file.exists(file.path(audit_dir, "script17a_manual_outcome_selection_TEMPLATE.csv")),
    file.exists(file.path(audit_dir, "script17a_domain_summary.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script17a_final_status.csv")
)

# ------------------------------------------------------------
# 12. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 17a completed: Outcome Variable Inventory\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nManual review burden:\n")
print(manual_review_burden)

cat("\nCandidate status summary:\n")
print(candidate_status_summary)

cat("\nDomain summary:\n")
print(domain_summary)

cat("\nHigh-priority binary candidates preview:\n")
print(
  high_priority_candidates %>%
    select(
      variable,
      variable_label,
      inferred_domain,
      candidate_status,
      valid_n,
      valid_values,
      value_distribution
    ) %>%
    slice_head(n = 20)
)

cat("\nCandidate outcome inventory preview:\n")
print(
  candidate_outcome_inventory %>%
    select(
      variable,
      variable_label,
      inferred_domain,
      candidate_status,
      valid_n,
      distinct_valid_n,
      valid_values
    ) %>%
    slice_head(n = 30)
)

cat("\nOutputs created:\n")
cat("- ", file.path(audit_dir, "script17a_all_variable_inventory.csv"), "\n")
cat("- ", file.path(audit_dir, "script17a_candidate_outcome_inventory.csv"), "\n")
cat("- ", file.path(audit_dir, "script17a_manual_outcome_selection_TEMPLATE.csv"), "\n")
cat("- ", file.path(audit_dir, "script17a_domain_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script17a_candidate_status_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script17a_file_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script17a_manual_review_burden.csv"), "\n")
cat("- ", file.path(audit_dir, "script17a_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script17a_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Open outputs/audits/script17a_manual_outcome_selection_TEMPLATE.csv.\n")
cat("Select true behavioral/event outcomes only.\n")
cat("Save the reviewed file as:\n")
cat("outputs/audits/script17a_manual_outcome_selection_COMPLETED.csv\n")
cat("Do not commit respondent-level data or audit CSV outputs.\n")