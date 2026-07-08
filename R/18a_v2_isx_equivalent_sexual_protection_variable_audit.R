# ============================================================
# Script 18a v2 — ISX-Equivalent Sexual Protection Variable Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Search for Add Health Wave I variables that are functionally
#   equivalent to the user's thesis ISX sexual protection index.
#
# Core methodological correction:
#   The dependent variable should be an ISX-equivalent sexual
#   protection behavior index, not the psychosocial protection index.
#
# ISX-equivalent dependent variable:
#   sexual_protection_index
#
# Target dimensions:
#   isx1 — sexual initiation / timing of first sex
#   isx2 — number of sexual partners / sexual partner exposure
#   isx3 — condom / contraceptive / birth-control use
#   isx4 — sexual frequency / sexual exposure intensity
#
# Candidate roles:
#   - isx_exact_candidate
#   - isx_functional_proxy
#   - isx_support_variable
#   - psychosocial_predictor_not_isx
#   - exclude_or_low_relevance
#
# Main outputs:
#   outputs/audits/script18a_v2_isx_equivalent_candidate_inventory.csv
#   outputs/audits/script18a_v2_isx_equivalent_shortlist.csv
#   outputs/audits/script18a_v2_isx_manual_selection_TEMPLATE.csv
#   outputs/audits/script18a_v2_isx_component_summary.csv
#   outputs/audits/script18a_v2_isx_candidate_role_summary.csv
#   docs/add_health_wave01_isx_equivalent_variable_audit_script18a_v2.docx
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
cat("Script 18a v2 started: ISX-Equivalent Sexual Protection Audit\n")
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

summarise_value_distribution <- function(x, value_labels, max_values = 25) {

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
# 3. ISX-equivalent dictionary
# ------------------------------------------------------------

isx_equivalent_dictionary <- tibble::tribble(
  ~isx_item, ~isx_component, ~isx_component_label, ~thesis_logic, ~exact_behavior_regex, ~functional_proxy_regex, ~support_regex, ~psychosocial_predictor_regex, ~low_relevance_regex,

  "isx1_sexual_initiation_timing",
  "sexual_initiation_timing",
  "Sexual initiation / timing of first sex",
  "Higher protection means never had sex or later sexual initiation.",
  "age.*first.*sex|age.*first.*intercourse|first.*sex.*age|first.*intercourse.*age",
  "first time.*sex|first.*time.*sex|first time.*intercourse|first.*intercourse|first.*vaginal|first.*sex.*year|first.*sex.*month|ever have sex|ever had sex|sexual intercourse",
  "ever have sex|ever had sex|never had sex|had sexual intercourse|have sexual intercourse",
  "risk of|chance of|likely|knowledge|know|learned|heard|friend|parent|mother|father|moral|wrong|attitude|feel|resist|refuse",
  "birth control|contracep|condom|partner|partners|diagnos|std|sti|hiv|aids",

  "isx2_partner_exposure",
  "sexual_partner_exposure",
  "Number of partners / sexual partner exposure",
  "Higher protection means no sex or fewer sexual partners.",
  "number.*sexual.*partner|number.*sex.*partner|how many.*sexual.*partner|how many.*sex.*partner|partners.*last.*year|partners.*past.*year|sex partners.*last|sexual partners.*last",
  "sex.*partner|sexual.*partner|partner.*sex|partner.*intercourse|how many.*people.*sex|many.*partners|number.*partners|lifetime.*partners|partners",
  "ever have sex|ever had sex|never had sex",
  "risk of|chance of|likely|knowledge|know|learned|heard|friend.*think|parent|mother|father|moral|wrong|attitude|feel",
  "birth control|contracep|condom|first time|age at first|diagnos|std|sti|hiv|aids",

  "isx3_protective_method_use",
  "condom_contraceptive_use",
  "Condom / contraceptive / birth-control use",
  "Higher protection means no sex or more consistent use of condom/contraceptive protection.",
  "how often.*condom|how oft.*condom|how often.*birth.*control|how oft.*birth.*control|always.*condom|always.*birth.*control|used.*condom|used.*birth.*control|use.*condom|use.*birth.*control",
  "condom|contracep|birth control|protection|prevent.*pregnan|used.*method|use.*method|first time.*birth.*control|last time.*birth.*control|last sex.*condom|first sex.*condom",
  "ever have sex|ever had sex|never had sex",
  "know|knowledge|learned|heard|friend|friends|think|approve|disapprove|risk of|chance of|likely|easy to get|expensive|bothersome|morally|wrong|looking for sex|resist|refuse|attitude|feel",
  "diagnos|std|sti|hiv|aids",

  "isx4_sexual_frequency_exposure",
  "sexual_frequency_exposure",
  "Sexual frequency / sexual exposure intensity",
  "Higher protection means no sex or lower frequency of sexual intercourse.",
  "how often.*sex|how oft.*sex|frequency.*sex|times.*sex|sexual intercourse.*times|intercourse.*times|how many.*times.*sex|past year.*sex|last year.*sex",
  "had sex.*times|sex.*past.*year|sex.*last.*year|recent.*sex|sexual activity|sexually active|sexual intercourse",
  "ever have sex|ever had sex|never had sex",
  "risk of|chance of|likely|knowledge|know|learned|heard|friend|parent|mother|father|moral|wrong|attitude|feel",
  "birth control|contracep|condom|partner|partners|first time|age at first|diagnos|std|sti|hiv|aids"
)

role_rank <- function(role) {
  dplyr::case_when(
    role == "isx_exact_candidate" ~ 1L,
    role == "isx_functional_proxy" ~ 2L,
    role == "isx_support_variable" ~ 3L,
    role == "psychosocial_predictor_not_isx" ~ 4L,
    TRUE ~ 5L
  )
}

classify_candidate_role <- function(exact_match,
                                    proxy_match,
                                    support_match,
                                    psychosocial_match,
                                    low_relevance_match,
                                    valid_n,
                                    distinct_valid_n,
                                    variable,
                                    text_l) {

  likert_like_possible_predictor <-
    distinct_valid_n %in% 4:6 &&
    valid_n >= 2500 &&
    stringr::str_detect(
      text_l,
      "birth control|condom|contracep|risk|resist|friend|know|moral|wrong|likely|chance|easy|expensive|bothersome"
    )

  section_prefix_predictor <- stringr::str_detect(
    lower_clean(variable),
    "^h1pf|^h1se"
  ) &&
    !stringr::str_detect(
      text_l,
      "how often|how oft|first time|last time|number|times|diagnos"
    )

  dplyr::case_when(
    psychosocial_match | likert_like_possible_predictor | section_prefix_predictor ~
      "psychosocial_predictor_not_isx",

    exact_match & !low_relevance_match & distinct_valid_n >= 2 ~
      "isx_exact_candidate",

    proxy_match & !low_relevance_match & distinct_valid_n >= 2 ~
      "isx_functional_proxy",

    support_match & distinct_valid_n >= 1 ~
      "isx_support_variable",

    TRUE ~
      "exclude_or_low_relevance"
  )
}

functional_equivalence_note <- function(isx_component, role) {

  dplyr::case_when(
    role == "isx_exact_candidate" &
      isx_component == "sexual_initiation_timing" ~
      "Directly supports the timing of sexual initiation component.",

    role == "isx_functional_proxy" &
      isx_component == "sexual_initiation_timing" ~
      "Can proxy sexual initiation timing or help derive timing when combined with age/birth-year information.",

    role == "isx_support_variable" &
      isx_component == "sexual_initiation_timing" ~
      "Useful for assigning the highest protection score to respondents who never had sex.",

    role == "isx_exact_candidate" &
      isx_component == "sexual_partner_exposure" ~
      "Directly supports the partner exposure component.",

    role == "isx_functional_proxy" &
      isx_component == "sexual_partner_exposure" ~
      "Can proxy number of partners if the recall period differs from the thesis item.",

    role == "isx_exact_candidate" &
      isx_component == "condom_contraceptive_use" ~
      "Directly supports protective method use.",

    role == "isx_functional_proxy" &
      isx_component == "condom_contraceptive_use" ~
      "Can proxy protective method use if it measures first sex, last sex, usual use, or a narrower recall period.",

    role == "isx_exact_candidate" &
      isx_component == "sexual_frequency_exposure" ~
      "Directly supports sexual frequency or sexual exposure intensity.",

    role == "isx_functional_proxy" &
      isx_component == "sexual_frequency_exposure" ~
      "Can proxy sexual exposure intensity if it captures recent or general sexual activity.",

    role == "psychosocial_predictor_not_isx" ~
      "Important psychosocial predictor candidate, but not part of the behavioral ISX dependent variable.",

    TRUE ~
      "Low relevance or requires manual review."
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
# 6. Build full candidate inventory
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

      for (i in seq_len(nrow(isx_equivalent_dictionary))) {

        item <- isx_equivalent_dictionary[i, ]

        exact_match <- stringr::str_detect(text_l, item$exact_behavior_regex)
        proxy_match <- stringr::str_detect(text_l, item$functional_proxy_regex)
        support_match <- stringr::str_detect(text_l, item$support_regex)
        psychosocial_match <- stringr::str_detect(text_l, item$psychosocial_predictor_regex)
        low_relevance_match <- stringr::str_detect(text_l, item$low_relevance_regex)

        if (!(exact_match | proxy_match | support_match | psychosocial_match)) {
          next
        }

        candidate_role <- classify_candidate_role(
          exact_match = exact_match,
          proxy_match = proxy_match,
          support_match = support_match,
          psychosocial_match = psychosocial_match,
          low_relevance_match = low_relevance_match,
          valid_n = valid_n,
          distinct_valid_n = distinct_valid_n,
          variable = var,
          text_l = text_l
        )

        candidate_priority_score <-
          ifelse(candidate_role == "isx_exact_candidate", 20, 0) +
          ifelse(candidate_role == "isx_functional_proxy", 15, 0) +
          ifelse(candidate_role == "isx_support_variable", 10, 0) +
          ifelse(candidate_role == "psychosocial_predictor_not_isx", 5, 0) +
          ifelse(valid_n >= 500, 3, 0) +
          ifelse(valid_n >= 2000, 2, 0) +
          ifelse(distinct_valid_n >= 2, 2, 0) -
          ifelse(low_relevance_match, 4, 0)

        candidate_inventory_list[[length(candidate_inventory_list) + 1]] <- tibble(
          isx_item = item$isx_item,
          isx_component = item$isx_component,
          isx_component_label = item$isx_component_label,
          thesis_logic = item$thesis_logic,
          source_file = fp,
          file_name = basename(fp),
          object_name = obj_name,
          variable = var,
          variable_label = var_label,
          value_labels = val_labels,
          candidate_text = text_all,
          exact_match = exact_match,
          proxy_match = proxy_match,
          support_match = support_match,
          psychosocial_match = psychosocial_match,
          low_relevance_match = low_relevance_match,
          candidate_role = candidate_role,
          candidate_role_rank = role_rank(candidate_role),
          candidate_priority_score = candidate_priority_score,
          functional_equivalence_note = functional_equivalence_note(
            item$isx_component,
            candidate_role
          ),
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
    "No ISX-equivalent candidate variables were detected. ",
    "Search terms may need to be expanded after inspecting the codebook."
  )
}

# Remove duplicate rows for the same candidate/component/variable.
candidate_inventory <- candidate_inventory %>%
  arrange(
    isx_item,
    candidate_role_rank,
    desc(candidate_priority_score),
    desc(valid_n),
    variable,
    file_name,
    object_name
  ) %>%
  group_by(isx_item, variable) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(
    isx_item,
    candidate_role_rank,
    desc(candidate_priority_score),
    desc(valid_n),
    variable
  )

readr::write_csv(
  candidate_inventory,
  file.path(audit_dir, "script18a_v2_isx_equivalent_candidate_inventory.csv")
)

# ------------------------------------------------------------
# 7. Shortlist candidates
# ------------------------------------------------------------

shortlisted_candidates <- candidate_inventory %>%
  filter(candidate_role != "exclude_or_low_relevance") %>%
  group_by(isx_item) %>%
  arrange(
    candidate_role_rank,
    desc(candidate_priority_score),
    desc(valid_n),
    variable,
    .by_group = TRUE
  ) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  mutate(
    audit_recommendation = case_when(
      candidate_role == "isx_exact_candidate" &
        valid_n >= 500 &
        distinct_valid_n >= 2 ~
        "strong_candidate_for_manual_isx_selection",

      candidate_role == "isx_functional_proxy" &
        valid_n >= 500 &
        distinct_valid_n >= 2 ~
        "functional_proxy_review_for_manual_isx_selection",

      candidate_role == "isx_support_variable" ~
        "support_variable_review_needed",

      candidate_role == "psychosocial_predictor_not_isx" ~
        "retain_for_later_independent_variable_blocks_not_for_isx_dependent_index",

      valid_n < 500 ~
        "review_low_valid_n",

      distinct_valid_n < 2 ~
        "review_no_variation",

      TRUE ~
        "manual_review_required"
    )
  )

readr::write_csv(
  shortlisted_candidates,
  file.path(audit_dir, "script18a_v2_isx_equivalent_shortlist.csv")
)

# ------------------------------------------------------------
# 8. Manual selection template
# ------------------------------------------------------------

manual_selection_template <- shortlisted_candidates %>%
  mutate(
    manual_select_for_isx = case_when(
      candidate_role %in% c("isx_exact_candidate", "isx_functional_proxy", "isx_support_variable") ~ "review",
      TRUE ~ "no"
    ),
    manual_isx_item = isx_item,
    manual_candidate_role = candidate_role,
    manual_final_variable_role = case_when(
      candidate_role == "isx_exact_candidate" ~ "candidate_main_isx_item",
      candidate_role == "isx_functional_proxy" ~ "candidate_functional_proxy",
      candidate_role == "isx_support_variable" ~ "candidate_support_variable",
      candidate_role == "psychosocial_predictor_not_isx" ~ "psychosocial_predictor_not_dependent_index",
      TRUE ~ "exclude"
    ),
    manual_score_4_rule = "",
    manual_score_3_rule = "",
    manual_score_2_rule = "",
    manual_score_1_rule = "",
    manual_never_had_sex_handling = "Use ever-sex support variable to assign score 4 if respondent never had sex.",
    manual_proxy_justification = functional_equivalence_note,
    manual_missing_codes = "",
    manual_reverse_score_needed = "",
    manual_use_in_script18b = "no",
    manual_decision_rationale = "",
    manual_reviewer = "",
    manual_review_date = ""
  ) %>%
  select(
    manual_select_for_isx,
    manual_use_in_script18b,
    manual_isx_item,
    manual_candidate_role,
    manual_final_variable_role,
    manual_score_4_rule,
    manual_score_3_rule,
    manual_score_2_rule,
    manual_score_1_rule,
    manual_never_had_sex_handling,
    manual_proxy_justification,
    manual_missing_codes,
    manual_reverse_score_needed,
    manual_decision_rationale,
    manual_reviewer,
    manual_review_date,
    isx_item,
    isx_component,
    isx_component_label,
    thesis_logic,
    candidate_role,
    functional_equivalence_note,
    audit_recommendation,
    variable,
    variable_label,
    value_labels,
    valid_n,
    missing_n,
    distinct_valid_n,
    valid_values,
    value_distribution,
    candidate_priority_score,
    exact_match,
    proxy_match,
    support_match,
    psychosocial_match,
    low_relevance_match,
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
  file.path(audit_dir, "script18a_v2_isx_manual_selection_TEMPLATE.csv")
)

# ------------------------------------------------------------
# 9. Summaries
# ------------------------------------------------------------

component_summary <- candidate_inventory %>%
  group_by(isx_item, isx_component_label) %>%
  summarise(
    unique_candidate_variables = n_distinct(variable),
    exact_candidates = sum(candidate_role == "isx_exact_candidate"),
    functional_proxies = sum(candidate_role == "isx_functional_proxy"),
    support_variables = sum(candidate_role == "isx_support_variable"),
    psychosocial_predictors_flagged = sum(candidate_role == "psychosocial_predictor_not_isx"),
    candidates_valid_n_500 = sum(valid_n >= 500),
    candidates_with_variation = sum(distinct_valid_n >= 2),
    best_candidate_role = first(candidate_role),
    best_candidate = first(variable),
    best_candidate_label = first(variable_label),
    best_candidate_valid_n = first(valid_n),
    best_candidate_values = first(valid_values),
    .groups = "drop"
  ) %>%
  arrange(isx_item)

candidate_role_summary <- candidate_inventory %>%
  count(isx_item, isx_component_label, candidate_role, name = "variables") %>%
  arrange(isx_item, role_rank(candidate_role), candidate_role)

shortlist_summary <- shortlisted_candidates %>%
  group_by(isx_item, isx_component_label, candidate_role) %>%
  summarise(
    shortlisted_variables = n_distinct(variable),
    best_variable = first(variable),
    best_label = first(variable_label),
    best_valid_n = first(valid_n),
    best_values = first(valid_values),
    .groups = "drop"
  ) %>%
  arrange(isx_item, role_rank(candidate_role))

readr::write_csv(
  component_summary,
  file.path(audit_dir, "script18a_v2_isx_component_summary.csv")
)

readr::write_csv(
  candidate_role_summary,
  file.path(audit_dir, "script18a_v2_isx_candidate_role_summary.csv")
)

readr::write_csv(
  shortlist_summary,
  file.path(audit_dir, "script18a_v2_isx_shortlist_summary.csv")
)

# ------------------------------------------------------------
# 10. Scoring and methodological guide
# ------------------------------------------------------------

isx_equivalent_scoring_guide <- tibble::tribble(
  ~isx_item, ~target_dimension, ~score_4_meaning, ~score_3_meaning, ~score_2_meaning, ~score_1_meaning, ~allowed_proxy_logic,
  "isx1_sexual_initiation_timing", "Sexual initiation / timing", "Never had sex.", "Late initiation or lower-risk timing.", "Intermediate timing.", "Early initiation or higher-risk timing.", "If exact age is unavailable, derive timing from first-sex year/month combined with respondent age/birth-year, or use ever-sex status as partial support.",
  "isx2_partner_exposure", "Partner exposure", "Never had sex.", "One partner or low partner exposure.", "Moderate partner exposure.", "Three or more partners or high partner exposure.", "If last-year partner count is unavailable, use closest available partner exposure measure and document period differences.",
  "isx3_protective_method_use", "Condom / contraceptive use", "Never had sex.", "Consistent or high protective method use.", "Partial or inconsistent use.", "No protective method use.", "If last-year use is unavailable, use closest behavioral measure such as first-sex use, last-sex use, or usual use; do not use knowledge or attitudes as ISX items.",
  "isx4_sexual_frequency_exposure", "Sexual frequency / exposure", "Never had sex.", "Low sexual frequency or low exposure.", "Moderate sexual frequency.", "High sexual frequency or high exposure.", "If exact last-year frequency is unavailable, use closest behavioral sexual activity intensity measure and document recall-period differences."
)

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Dependent variable", "The intended dependent variable is an ISX-equivalent sexual protection behavior index.",
  "Functional equivalence", "Variables do not need to be identical to the user's thesis items if they preserve the same behavioral interpretation: higher values must mean higher sexual protection.",
  "Candidate roles", "Candidate variables are classified as exact ISX candidates, functional proxies, support variables, psychosocial predictors not used in the ISX dependent index, or low-relevance exclusions.",
  "Psychosocial separation", "Knowledge, self-efficacy, attitudes, peer influence and risk perceptions are preserved for later predictor blocks, not for the behavioral ISX dependent index.",
  "Never had sex", "Ever-sex variables are support variables because the thesis-style index assigns the highest protection score to respondents who never had sexual intercourse.",
  "Manual review", "Script 18a v2 does not construct the index. Manual review is required before Script 18b creates the ISX-equivalent index.",
  "Score direction", "All final ISX-equivalent items must be recoded so that higher scores represent higher sexual protection.",
  "Next step", "Script 18b should construct the ISX-equivalent sexual protection index using the manually approved candidate variables and documented proxy rules."
)

readr::write_csv(
  isx_equivalent_scoring_guide,
  file.path(audit_dir, "script18a_v2_isx_equivalent_scoring_guide.csv")
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18a_v2_isx_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 11. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_isx_equivalent_variable_audit_script18a_v2.docx"
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
      candidate_role,
      variable,
      variable_label,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution,
      candidate_priority_score,
      audit_recommendation
    ) %>%
    slice_head(n = 80)

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — ISX-Equivalent Sexual Protection Variable Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18a v2 audits variables that may support an ISX-equivalent sexual protection behavior index. The goal is functional equivalence with the user's thesis index: higher values should represent higher sexual protection. Psychosocial variables are flagged for later use as independent variables and are not treated as ISX dependent-index items.",
      style = "Normal"
    ) %>%
    officer::body_add_par("ISX-equivalent scoring guide", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(isx_equivalent_scoring_guide)) %>%
    officer::body_add_par("Component summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(component_summary)) %>%
    officer::body_add_par("Candidate role summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(candidate_role_summary)) %>%
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
# 12. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "candidate_data_files_found",
    "candidate_inventory_created",
    "shortlist_created",
    "manual_selection_template_created",
    "component_summary_created",
    "candidate_role_summary_created",
    "scoring_guide_created",
    "methodological_decisions_created",
    "word_report_created",
    "manual_review_still_required",
    "ready_for_script18b_after_review"
  ),
  status = c(
    length(candidate_data_files) > 0,
    file.exists(file.path(audit_dir, "script18a_v2_isx_equivalent_candidate_inventory.csv")),
    file.exists(file.path(audit_dir, "script18a_v2_isx_equivalent_shortlist.csv")),
    file.exists(file.path(audit_dir, "script18a_v2_isx_manual_selection_TEMPLATE.csv")),
    file.exists(file.path(audit_dir, "script18a_v2_isx_component_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_v2_isx_candidate_role_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_v2_isx_equivalent_scoring_guide.csv")),
    file.exists(file.path(audit_dir, "script18a_v2_isx_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE,
    nrow(shortlisted_candidates) > 0
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18a_v2_isx_final_status.csv")
)

# ------------------------------------------------------------
# 13. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18a v2 completed: ISX-Equivalent Sexual Protection Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nComponent summary:\n")
print(component_summary)

cat("\nCandidate role summary:\n")
print(candidate_role_summary)

cat("\nShortlist summary:\n")
print(shortlist_summary)

cat("\nShortlisted candidate variables preview:\n")
print(
  shortlisted_candidates %>%
    select(
      isx_item,
      candidate_role,
      variable,
      variable_label,
      valid_n,
      distinct_valid_n,
      valid_values,
      value_distribution,
      candidate_priority_score,
      audit_recommendation
    ) %>%
    print(n = 80)
)

cat("\nOutputs created:\n")
cat("- ", file.path(audit_dir, "script18a_v2_isx_equivalent_candidate_inventory.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v2_isx_equivalent_shortlist.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v2_isx_manual_selection_TEMPLATE.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v2_isx_component_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v2_isx_candidate_role_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v2_isx_shortlist_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v2_isx_equivalent_scoring_guide.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v2_isx_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_v2_isx_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Review script18a_v2_isx_manual_selection_TEMPLATE.csv before constructing the ISX-equivalent index in Script 18b.\n")
cat("Do not commit until the candidate roles and proxy decisions are reviewed.\n")