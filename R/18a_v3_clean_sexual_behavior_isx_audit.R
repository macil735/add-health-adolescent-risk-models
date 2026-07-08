# ============================================================
# Script 18a v3 — Clean Sexual Behavior ISX Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Produce a cleaner audit of behavioral sexual variables that may
#   support an ISX-equivalent sexual protection index.
#
# Methodological correction:
#   - The dependent variable should be a behavioral sexual protection
#     index, not a psychosocial index.
#   - Psychosocial items such as knowledge, risk perception, friends,
#     parents, school connectedness, self-efficacy or attitudes should
#     be excluded from the dependent ISX index and retained later as
#     independent variables.
#
# Main focus:
#   - H1CO* variables, because these appear to contain sexual behavior
#     items in Wave I.
#   - Additional variables are included only if their labels indicate
#     actual sexual behavior.
#
# ISX-equivalent dimensions:
#   isx1 — sexual initiation / timing of first sex
#   isx2 — partner exposure / number of sexual partners
#   isx3 — condom / contraceptive / birth-control use behavior
#   isx4 — sexual frequency / sexual exposure intensity
#
# Main outputs:
#   outputs/audits/script18a_v3_clean_behavior_variable_inventory.csv
#   outputs/audits/script18a_v3_clean_isx_candidate_shortlist.csv
#   outputs/audits/script18a_v3_clean_isx_manual_selection_TEMPLATE.csv
#   outputs/audits/script18a_v3_h1co_behavior_codebook_audit.csv
#   outputs/audits/script18a_v3_clean_isx_component_summary.csv
#   docs/add_health_wave01_clean_sexual_behavior_isx_audit_script18a_v3.docx
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
cat("Script 18a v3 started: Clean Sexual Behavior ISX Audit\n")
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

# ------------------------------------------------------------
# 3. Behavioral classification functions
# ------------------------------------------------------------

is_h1co <- function(variable) {
  stringr::str_detect(stringr::str_to_upper(variable), "^H1CO")
}

is_probably_psychosocial <- function(variable, variable_label) {

  text_l <- lower_clean(paste(variable, variable_label, sep = " | "))

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
        "part of",
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

is_behavioral_label <- function(variable, variable_label) {

  text_l <- lower_clean(paste(variable, variable_label, sep = " | "))

  behavior_terms <- stringr::str_detect(
    text_l,
    paste(
      c(
        "ever had sex",
        "ever have sex",
        "sexual intercourse",
        "first time sex",
        "first time.*sex",
        "first.*intercourse",
        "how often",
        "how oft",
        "times",
        "number",
        "partner",
        "partners",
        "condom",
        "birth control",
        "contraceptive",
        "contracep",
        "used",
        "use",
        "last time",
        "first time",
        "vaginal",
        "pregnant",
        "diagnosed",
        "chlamydia",
        "gonorrhea"
      ),
      collapse = "|"
    )
  )

  behavior_terms
}

classify_isx_component <- function(variable, variable_label) {

  text_l <- lower_clean(paste(variable, variable_label, sep = " | "))

  dplyr::case_when(
    stringr::str_detect(
      text_l,
      "ever had sex|ever have sex|ever had sexual intercourse|ever have sexual intercourse"
    ) ~
      "isx_support_ever_sex",

    stringr::str_detect(
      text_l,
      "first time sex|first.*time.*sex|first.*intercourse|age.*first.*sex|first.*sex.*age|first.*sex.*year|first.*sex.*month"
    ) ~
      "isx1_sexual_initiation_timing",

    stringr::str_detect(
      text_l,
      "number.*partner|how many.*partner|sex.*partner|sexual.*partner|partners"
    ) ~
      "isx2_partner_exposure",

    stringr::str_detect(
      text_l,
      "condom|birth control|contraceptive|contracep|used.*method|use.*method"
    ) ~
      "isx3_protective_method_use",

    stringr::str_detect(
      text_l,
      "how often.*sex|how oft.*sex|times.*sex|sex.*times|intercourse.*times|frequency.*sex|sexual activity"
    ) ~
      "isx4_sexual_frequency_exposure",

    stringr::str_detect(
      text_l,
      "pregnant|diagnosed|chlamydia|gonorrhea|std|sti|hiv|aids"
    ) ~
      "sexual_health_outcome_not_isx",

    TRUE ~
      "behavioral_variable_unclassified"
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
      "Behavioral variable requiring manual classification"
  )
}

candidate_role_clean <- function(component, valid_n, distinct_valid_n) {

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

    TRUE ~
      "manual_review_behavioral_variable"
  )
}

role_rank <- function(role) {
  dplyr::case_when(
    role == "candidate_main_or_proxy_isx_item" ~ 1L,
    role == "candidate_low_valid_n_isx_item" ~ 2L,
    role == "isx_support_variable" ~ 3L,
    role == "manual_review_behavioral_variable" ~ 4L,
    role == "outcome_not_isx_item" ~ 5L,
    TRUE ~ 9L
  )
}

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
# 6. Build clean behavior inventory
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

      h1co_flag <- is_h1co(var)
      behavioral_label_flag <- is_behavioral_label(var, var_label)
      psychosocial_flag <- is_probably_psychosocial(var, var_label)

      # Primary inclusion rule:
      # - Include all H1CO variables.
      # - Include other variables only when behavior is explicit and not psychosocial.
      include_in_clean_behavior_audit <-
        h1co_flag |
        (behavioral_label_flag & !psychosocial_flag)

      if (!include_in_clean_behavior_audit) {
        next
      }

      component <- classify_isx_component(var, var_label)

      x_num <- valid_numeric_vector(x, val_labels)

      valid_values <- sort(unique(x_num[!is.na(x_num)]))
      valid_n <- sum(!is.na(x_num))
      missing_n <- sum(is.na(x_num))
      distinct_valid_n <- length(valid_values)

      role <- candidate_role_clean(
        component = component,
        valid_n = valid_n,
        distinct_valid_n = distinct_valid_n
      )

      inventory_list[[length(inventory_list) + 1]] <- tibble(
        source_file = fp,
        file_name = basename(fp),
        object_name = obj_name,
        variable = var,
        variable_label = var_label,
        value_labels = val_labels,
        h1co_variable = h1co_flag,
        behavioral_label_flag = behavioral_label_flag,
        psychosocial_flag = psychosocial_flag,
        isx_component = component,
        isx_component_label = component_label(component),
        clean_candidate_role = role,
        clean_candidate_role_rank = role_rank(role),
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

behavior_inventory <- bind_rows(inventory_list)

if (nrow(behavior_inventory) == 0) {
  stop("No clean sexual behavior candidate variables were detected.")
}

behavior_inventory <- behavior_inventory %>%
  arrange(
    clean_candidate_role_rank,
    isx_component,
    desc(h1co_variable),
    variable,
    file_name,
    object_name
  ) %>%
  group_by(variable) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(
    clean_candidate_role_rank,
    isx_component,
    variable
  )

readr::write_csv(
  behavior_inventory,
  file.path(audit_dir, "script18a_v3_clean_behavior_variable_inventory.csv")
)

# ------------------------------------------------------------
# 7. H1CO codebook-style audit
# ------------------------------------------------------------

h1co_behavior_codebook_audit <- behavior_inventory %>%
  filter(h1co_variable) %>%
  select(
    variable,
    variable_label,
    isx_component,
    isx_component_label,
    clean_candidate_role,
    valid_n,
    distinct_valid_n,
    valid_values,
    value_distribution,
    value_labels,
    file_name,
    object_name,
    source_file
  ) %>%
  arrange(variable)

readr::write_csv(
  h1co_behavior_codebook_audit,
  file.path(audit_dir, "script18a_v3_h1co_behavior_codebook_audit.csv")
)

# ------------------------------------------------------------
# 8. Clean ISX candidate shortlist
# ------------------------------------------------------------

clean_isx_candidate_shortlist <- behavior_inventory %>%
  filter(
    clean_candidate_role %in% c(
      "candidate_main_or_proxy_isx_item",
      "candidate_low_valid_n_isx_item",
      "isx_support_variable",
      "manual_review_behavioral_variable"
    )
  ) %>%
  arrange(
    clean_candidate_role_rank,
    isx_component,
    desc(valid_n),
    variable
  )

readr::write_csv(
  clean_isx_candidate_shortlist,
  file.path(audit_dir, "script18a_v3_clean_isx_candidate_shortlist.csv")
)

# ------------------------------------------------------------
# 9. Manual selection template
# ------------------------------------------------------------

manual_selection_template <- clean_isx_candidate_shortlist %>%
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
    variable,
    variable_label,
    isx_component,
    isx_component_label,
    clean_candidate_role,
    valid_n,
    missing_n,
    distinct_valid_n,
    valid_values,
    value_distribution,
    value_labels,
    h1co_variable,
    behavioral_label_flag,
    psychosocial_flag,
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
  file.path(audit_dir, "script18a_v3_clean_isx_manual_selection_TEMPLATE.csv")
)

# ------------------------------------------------------------
# 10. Summaries
# ------------------------------------------------------------

component_summary <- behavior_inventory %>%
  group_by(isx_component, isx_component_label) %>%
  summarise(
    variables_detected = n(),
    h1co_variables = sum(h1co_variable),
    candidate_main_or_proxy = sum(clean_candidate_role == "candidate_main_or_proxy_isx_item"),
    candidate_low_valid_n = sum(clean_candidate_role == "candidate_low_valid_n_isx_item"),
    support_variables = sum(clean_candidate_role == "isx_support_variable"),
    outcome_not_isx = sum(clean_candidate_role == "outcome_not_isx_item"),
    manual_review_variables = sum(clean_candidate_role == "manual_review_behavioral_variable"),
    best_variable = first(variable),
    best_label = first(variable_label),
    best_valid_n = first(valid_n),
    best_values = first(valid_values),
    .groups = "drop"
  ) %>%
  arrange(isx_component)

candidate_role_summary <- behavior_inventory %>%
  count(clean_candidate_role, name = "variables") %>%
  arrange(role_rank(clean_candidate_role), clean_candidate_role)

h1co_summary <- h1co_behavior_codebook_audit %>%
  count(isx_component, isx_component_label, clean_candidate_role, name = "h1co_variables") %>%
  arrange(isx_component, role_rank(clean_candidate_role))

readr::write_csv(
  component_summary,
  file.path(audit_dir, "script18a_v3_clean_isx_component_summary.csv")
)

readr::write_csv(
  candidate_role_summary,
  file.path(audit_dir, "script18a_v3_clean_isx_candidate_role_summary.csv")
)

readr::write_csv(
  h1co_summary,
  file.path(audit_dir, "script18a_v3_h1co_summary.csv")
)

# ------------------------------------------------------------
# 11. Scoring guide and methodological decisions
# ------------------------------------------------------------

scoring_guide <- tibble::tribble(
  ~isx_component, ~component_label, ~score_direction, ~score_4_meaning, ~score_3_meaning, ~score_2_meaning, ~score_1_meaning,
  "isx1_sexual_initiation_timing", "Sexual initiation / timing of first sex", "Higher score = later or no sexual initiation", "Never had sexual intercourse", "Late initiation", "Intermediate initiation", "Early initiation",
  "isx2_partner_exposure", "Number of partners / partner exposure", "Higher score = fewer partners", "Never had sexual intercourse", "One partner or low exposure", "Moderate exposure", "Multiple partners or high exposure",
  "isx3_protective_method_use", "Condom / contraceptive / birth-control use", "Higher score = greater protective method use", "Never had sexual intercourse", "Consistent use", "Inconsistent use", "No use",
  "isx4_sexual_frequency_exposure", "Sexual frequency / exposure intensity", "Higher score = lower sexual exposure", "Never had sexual intercourse", "Low frequency", "Moderate frequency", "High frequency"
)

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Dependent variable", "The intended dependent variable is a behavioral ISX-equivalent sexual protection index.",
  "Primary audit focus", "Script 18a v3 prioritizes H1CO variables and variables with explicit sexual behavior labels.",
  "Psychosocial exclusion", "Knowledge, perceptions, friends, parents, school, attitudes, moral norms and self-efficacy are excluded from the dependent ISX index and retained for later independent-variable blocks.",
  "Support variable", "Ever-sex variables are treated as support variables because never having had sex receives the highest protection score in the thesis-style logic.",
  "Outcome variables", "Diagnoses, pregnancy and STI/HIV outcomes are not ISX behavior items; they may be used later as outcomes or external validation variables.",
  "Manual review", "The script produces a manual template. Script 18b should only construct the index after variables and scoring rules are reviewed.",
  "Data protection", "Only aggregate variable summaries are written; respondent-level data are not exported."
)

readr::write_csv(
  scoring_guide,
  file.path(audit_dir, "script18a_v3_clean_isx_scoring_guide.csv")
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18a_v3_clean_isx_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 12. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_clean_sexual_behavior_isx_audit_script18a_v3.docx"
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

  report_h1co <- h1co_behavior_codebook_audit %>%
    select(
      variable,
      variable_label,
      isx_component_label,
      clean_candidate_role,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution
    ) %>%
    slice_head(n = 80)

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — Clean Sexual Behavior ISX Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18a v3 provides a cleaner audit of behavioral sexual variables for constructing an ISX-equivalent sexual protection index. The audit focuses on H1CO variables and excludes psychosocial variables from the dependent index.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Component summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(component_summary)) %>%
    officer::body_add_par("Candidate role summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(candidate_role_summary)) %>%
    officer::body_add_par("H1CO behavior codebook audit", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(report_h1co)) %>%
    officer::body_add_par("Scoring guide", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(scoring_guide)) %>%
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
    "clean_behavior_inventory_created",
    "h1co_codebook_audit_created",
    "clean_isx_shortlist_created",
    "manual_selection_template_created",
    "component_summary_created",
    "candidate_role_summary_created",
    "scoring_guide_created",
    "methodological_decisions_created",
    "word_report_created",
    "manual_review_still_required"
  ),
  status = c(
    length(candidate_data_files) > 0,
    file.exists(file.path(audit_dir, "script18a_v3_clean_behavior_variable_inventory.csv")),
    file.exists(file.path(audit_dir, "script18a_v3_h1co_behavior_codebook_audit.csv")),
    file.exists(file.path(audit_dir, "script18a_v3_clean_isx_candidate_shortlist.csv")),
    file.exists(file.path(audit_dir, "script18a_v3_clean_isx_manual_selection_TEMPLATE.csv")),
    file.exists(file.path(audit_dir, "script18a_v3_clean_isx_component_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_v3_clean_isx_candidate_role_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_v3_clean_isx_scoring_guide.csv")),
    file.exists(file.path(audit_dir, "script18a_v3_clean_isx_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18a_v3_clean_isx_final_status.csv")
)

# ------------------------------------------------------------
# 14. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18a v3 completed: Clean Sexual Behavior ISX Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nComponent summary:\n")
print(component_summary)

cat("\nCandidate role summary:\n")
print(candidate_role_summary)

cat("\nH1CO summary:\n")
print(h1co_summary)

cat("\nClean ISX candidate shortlist:\n")
print(
  clean_isx_candidate_shortlist %>%
    select(
      variable,
      variable_label,
      isx_component,
      isx_component_label,
      clean_candidate_role,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution
    ),
  n = 100
)

cat("\nH1CO behavior codebook audit preview:\n")
print(
  h1co_behavior_codebook_audit %>%
    select(
      variable,
      variable_label,
      isx_component,
      clean_candidate_role,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution
    ),
  n = 100
)

cat("\nOutputs created:\n")
cat("- ", file.path(audit_dir, "script18a_v3_clean_behavior_variable_inventory.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v3_clean_isx_candidate_shortlist.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v3_clean_isx_manual_selection_TEMPLATE.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v3_h1co_behavior_codebook_audit.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v3_clean_isx_component_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v3_clean_isx_candidate_role_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v3_h1co_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v3_clean_isx_scoring_guide.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v3_clean_isx_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v3_clean_isx_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Review script18a_v3_h1co_behavior_codebook_audit.csv and script18a_v3_clean_isx_manual_selection_TEMPLATE.csv before Script 18b.\n")
cat("Do not commit until the behavioral candidates and scoring rules are reviewed.\n")