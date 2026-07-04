# ============================================================
# Project: add-health-adolescent-risk-models
# Script 08: Missing Data and Final Recode Review
# Author: Gelo Picol
#
# Purpose:
#   Review missing data, recode readiness, outcome availability,
#   and construct-level usability before any regression modeling.
#
# Inputs:
#   - Weighted local-only Wave I analytical file from Script 07.
#   - Script 05 import check.
#   - Script 06 construct diagnostics.
#   - Script 07 outcome availability.
#
# Important:
#   - This script does not export individual-level microdata.
#   - AID is used only internally and excluded from public outputs.
#   - Public outputs are aggregate diagnostics only.
# ============================================================


# ============================================================
# 0. Project root and options
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"

options(na.print = "NA")


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "dplyr",
  "tibble",
  "readr",
  "stringr",
  "tidyr",
  "purrr",
  "openxlsx",
  "haven"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(openxlsx)
library(haven)


# ============================================================
# 2. Paths
# ============================================================

data_processed_dir <- file.path(project_root, "data/processed")
outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir   <- file.path(project_root, "outputs/diagnostics")
docs_dir           <- file.path(project_root, "docs")

dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

weighted_analytical_rds_path <- file.path(
  data_processed_dir,
  "add_health_wave01_analytical_weighted_local_only.rds"
)

script05_import_check_path <- file.path(
  outputs_tables_dir,
  "script05_wave01_import_variable_check.csv"
)

script06_construct_availability_path <- file.path(
  outputs_tables_dir,
  "script06_wave01_construct_availability_summary.csv"
)

script06_recode_quality_path <- file.path(
  outputs_tables_dir,
  "script06_wave01_recode_quality_check.csv"
)

script07_outcome_availability_path <- file.path(
  outputs_tables_dir,
  "script07_wave01_outcome_availability.csv"
)

script07_weight_design_path <- file.path(
  outputs_tables_dir,
  "script07_wave01_weight_design_diagnostics.csv"
)


# ============================================================
# 3. Helper functions
# ============================================================

get_variable_label <- function(x) {
  lab <- attr(x, "label")

  if (is.null(lab)) {
    return(NA_character_)
  }

  as.character(lab)
}

safe_count_public <- function(n, threshold = 10) {
  if (is.na(n)) {
    return(NA_character_)
  }

  if (n < threshold) {
    return(paste0("<", threshold))
  }

  as.character(n)
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(haven::zap_labels(x)))
}

weighted_share <- function(flag, weight) {
  ok <- !is.na(flag) & !is.na(weight) & weight > 0

  if (sum(ok) == 0) {
    return(NA_real_)
  }

  100 * sum(flag[ok] * weight[ok], na.rm = TRUE) / sum(weight[ok], na.rm = TRUE)
}

weighted_mean_safe <- function(x, weight) {
  x_num <- safe_numeric(x)

  ok <- !is.na(x_num) & !is.na(weight) & weight > 0

  if (sum(ok) == 0) {
    return(NA_real_)
  }

  sum(x_num[ok] * weight[ok], na.rm = TRUE) / sum(weight[ok], na.rm = TRUE)
}

classify_variable_role <- function(v) {
  dplyr::case_when(
    v %in% c("GSWGT1", "CLUSTER2", "SCHWT1",
             "valid_gswgt1", "valid_cluster2", "valid_schwt1") ~ "weight_or_design_variable",
    v %in% c("a_grade_wave1", "a_age_wave1", "a_female") ~ "core_covariate",
    v %in% c("a_main_sample_grade_10_12", "a_strict_sample_grade_age",
             "a_sample_grade_10_12", "a_sample_age_15_19") ~ "sample_definition_variable",
    v == "a_sex_ever" ~ "candidate_outcome",
    stringr::str_detect(v, "^a_.*_yesno$") ~ "candidate_binary_recode",
    stringr::str_detect(v, "^a_") ~ "analytical_recode",
    stringr::str_detect(v, "^num_") ~ "numeric_cleaned_item",
    stringr::str_detect(v, "^derived_") ~ "script05_derived_variable",
    TRUE ~ "original_imported_variable"
  )
}

availability_class <- function(pct_nonmissing, n_nonmissing) {
  dplyr::case_when(
    is.na(pct_nonmissing) | is.na(n_nonmissing) ~ "not_assessed",
    n_nonmissing < 50 ~ "not_recommended_too_few_cases",
    pct_nonmissing >= 80 ~ "high_availability",
    pct_nonmissing >= 60 ~ "moderate_availability",
    pct_nonmissing >= 40 ~ "limited_availability_review_required",
    TRUE ~ "not_recommended_high_missingness"
  )
}

modeling_decision_rule <- function(variable_role, pct_nonmissing_main, n_nonmissing_main) {
  dplyr::case_when(
    variable_role %in% c("weight_or_design_variable", "sample_definition_variable") ~ "support_variable_not_model_predictor",
    n_nonmissing_main < 50 ~ "exclude_too_few_cases",
    pct_nonmissing_main >= 70 ~ "candidate_for_modeling",
    pct_nonmissing_main >= 40 ~ "review_before_modeling",
    TRUE ~ "exclude_high_missingness"
  )
}

is_binary_01 <- function(x) {
  x_num <- safe_numeric(x)
  vals <- sort(unique(x_num[!is.na(x_num)]))

  length(vals) > 0 && all(vals %in% c(0, 1))
}

binary_distribution <- function(data, variable, sample_name) {
  if (!variable %in% names(data)) {
    return(tibble())
  }

  x <- safe_numeric(data[[variable]])
  w <- data$GSWGT1

  ok <- !is.na(x) & !is.na(w) & w > 0

  if (sum(ok) == 0) {
    return(
      tibble(
        sample_name = sample_name,
        variable = variable,
        n_nonmissing = 0,
        n_yes = 0,
        n_no = 0,
        weighted_pct_yes = NA_real_,
        weighted_pct_no = NA_real_
      )
    )
  }

  n_yes <- sum(x[ok] == 1, na.rm = TRUE)
  n_no  <- sum(x[ok] == 0, na.rm = TRUE)

  w_yes <- sum((x[ok] == 1) * w[ok], na.rm = TRUE)
  w_no  <- sum((x[ok] == 0) * w[ok], na.rm = TRUE)
  w_sum <- sum(w[ok], na.rm = TRUE)

  tibble(
    sample_name = sample_name,
    variable = variable,
    n_nonmissing = sum(ok),
    n_yes = n_yes,
    n_no = n_no,
    weighted_pct_yes = ifelse(w_sum > 0, 100 * w_yes / w_sum, NA_real_),
    weighted_pct_no = ifelse(w_sum > 0, 100 * w_no / w_sum, NA_real_)
  )
}

summarise_missingness_for_variable <- function(data, variable, sample_name) {
  x <- data[[variable]]
  w <- data$GSWGT1

  n_total <- nrow(data)
  n_missing <- sum(is.na(x))
  n_nonmissing <- sum(!is.na(x))

  pct_missing <- ifelse(n_total > 0, 100 * n_missing / n_total, NA_real_)
  pct_nonmissing <- ifelse(n_total > 0, 100 * n_nonmissing / n_total, NA_real_)

  weighted_pct_nonmissing <- weighted_share(!is.na(x), w)
  weighted_pct_missing <- ifelse(
    is.na(weighted_pct_nonmissing),
    NA_real_,
    100 - weighted_pct_nonmissing
  )

  tibble(
    sample_name = sample_name,
    variable = variable,
    variable_label = get_variable_label(x),
    variable_class = paste(class(x), collapse = "; "),
    variable_role = classify_variable_role(variable),
    n_total = n_total,
    n_missing = n_missing,
    n_nonmissing = n_nonmissing,
    pct_missing = round(pct_missing, 2),
    pct_nonmissing = round(pct_nonmissing, 2),
    weighted_pct_missing = round(weighted_pct_missing, 2),
    weighted_pct_nonmissing = round(weighted_pct_nonmissing, 2),
    n_unique_nonmissing = dplyr::n_distinct(x, na.rm = TRUE),
    is_binary_01 = is_binary_01(x),
    public_n_nonmissing = safe_count_public(n_nonmissing),
    availability_class = availability_class(pct_nonmissing, n_nonmissing)
  )
}


# ============================================================
# 4. Check required inputs
# ============================================================

required_inputs <- c(
  weighted_analytical_rds_path,
  script05_import_check_path,
  script06_construct_availability_path,
  script06_recode_quality_path,
  script07_outcome_availability_path,
  script07_weight_design_path
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    paste0(
      "Missing required input files:\n",
      paste(missing_inputs, collapse = "\n")
    )
  )
}


# ============================================================
# 5. Load data and previous diagnostics
# ============================================================

analysis_data <- readRDS(weighted_analytical_rds_path)

script05_import_check <- read_csv(
  script05_import_check_path,
  show_col_types = FALSE
)

script06_construct_availability <- read_csv(
  script06_construct_availability_path,
  show_col_types = FALSE
)

script06_recode_quality <- read_csv(
  script06_recode_quality_path,
  show_col_types = FALSE
)

script07_outcome_availability <- read_csv(
  script07_outcome_availability_path,
  show_col_types = FALSE
)

script07_weight_design <- read_csv(
  script07_weight_design_path,
  show_col_types = FALSE
)

if (!"GSWGT1" %in% names(analysis_data)) {
  stop("GSWGT1 not found in weighted analytical data from Script 07.")
}

if (!"AID" %in% names(analysis_data)) {
  stop("AID not found in weighted analytical data. This should have been recovered in Script 07.")
}


# ============================================================
# 6. Define analysis samples
# ============================================================

analysis_data_valid <- analysis_data %>%
  filter(!is.na(GSWGT1) & GSWGT1 > 0)

sample_full <- analysis_data_valid

sample_main <- analysis_data_valid %>%
  filter(a_main_sample_grade_10_12 == TRUE)

sample_strict <- analysis_data_valid %>%
  filter(a_strict_sample_grade_age == TRUE)

sample_list <- list(
  full_weighted_wave1 = sample_full,
  main_grade_10_12 = sample_main,
  strict_grade_10_12_age_15_19 = sample_strict
)


# ============================================================
# 7. Variables to review
# ============================================================

exclude_from_public_variable_review <- c(
  "AID"
)

variables_to_review <- setdiff(
  names(analysis_data_valid),
  exclude_from_public_variable_review
)

# Avoid exporting raw internal identifiers while still reviewing
# weights and design variables as aggregate diagnostics.
variables_to_review <- variables_to_review[
  !stringr::str_detect(variables_to_review, "^stat_")
]


# ============================================================
# 8. Variable-level missingness summary
# ============================================================

variable_missingness_summary <- purrr::imap_dfr(
  sample_list,
  function(data_sample, sample_name) {
    purrr::map_dfr(
      variables_to_review,
      function(v) summarise_missingness_for_variable(data_sample, v, sample_name)
    )
  }
)


# ============================================================
# 9. Wide missingness decision table for modeling
# ============================================================

missingness_main <- variable_missingness_summary %>%
  filter(sample_name == "main_grade_10_12") %>%
  select(
    variable,
    variable_label,
    variable_class,
    variable_role,
    main_n_total = n_total,
    main_n_missing = n_missing,
    main_n_nonmissing = n_nonmissing,
    main_pct_missing = pct_missing,
    main_pct_nonmissing = pct_nonmissing,
    main_weighted_pct_nonmissing = weighted_pct_nonmissing,
    main_n_unique_nonmissing = n_unique_nonmissing,
    main_is_binary_01 = is_binary_01,
    main_availability_class = availability_class
  )

missingness_strict <- variable_missingness_summary %>%
  filter(sample_name == "strict_grade_10_12_age_15_19") %>%
  select(
    variable,
    strict_n_total = n_total,
    strict_n_missing = n_missing,
    strict_n_nonmissing = n_nonmissing,
    strict_pct_missing = pct_missing,
    strict_pct_nonmissing = pct_nonmissing,
    strict_weighted_pct_nonmissing = weighted_pct_nonmissing,
    strict_availability_class = availability_class
  )

model_variable_decision_template <- missingness_main %>%
  left_join(
    missingness_strict,
    by = "variable"
  ) %>%
  mutate(
    preliminary_modeling_decision = modeling_decision_rule(
      variable_role,
      main_pct_nonmissing,
      main_n_nonmissing
    ),
    final_decision = "",
    final_role = "",
    reviewer_note = ""
  ) %>%
  arrange(
    variable_role,
    preliminary_modeling_decision,
    variable
  )


# ============================================================
# 10. Recode readiness review
# ============================================================

analytical_recode_vars <- names(analysis_data_valid)[
  stringr::str_detect(names(analysis_data_valid), "^a_")
]

recode_readiness_review <- purrr::map_dfr(
  analytical_recode_vars,
  function(v) {

    x_main <- sample_main[[v]]
    x_strict <- sample_strict[[v]]

    main_n_nonmissing <- sum(!is.na(x_main))
    strict_n_nonmissing <- sum(!is.na(x_strict))

    main_pct_nonmissing <- ifelse(
      nrow(sample_main) > 0,
      100 * main_n_nonmissing / nrow(sample_main),
      NA_real_
    )

    strict_pct_nonmissing <- ifelse(
      nrow(sample_strict) > 0,
      100 * strict_n_nonmissing / nrow(sample_strict),
      NA_real_
    )

    tibble(
      variable = v,
      variable_role = classify_variable_role(v),
      is_binary_01 = is_binary_01(sample_main[[v]]),
      main_n_nonmissing = main_n_nonmissing,
      main_pct_nonmissing = round(main_pct_nonmissing, 2),
      strict_n_nonmissing = strict_n_nonmissing,
      strict_pct_nonmissing = round(strict_pct_nonmissing, 2),
      n_unique_main = dplyr::n_distinct(x_main, na.rm = TRUE),
      preliminary_recode_decision = case_when(
        main_n_nonmissing < 50 ~ "exclude_or_review_too_few_cases",
        main_pct_nonmissing < 40 ~ "review_high_missingness",
        classify_variable_role(v) %in% c("candidate_outcome", "candidate_binary_recode") &
          is_binary_01(sample_main[[v]]) ~ "binary_recode_usable_after_codebook_check",
        classify_variable_role(v) %in% c("core_covariate", "sample_definition_variable") ~ "core_variable_usable",
        TRUE ~ "usable_after_directionality_review"
      ),
      final_decision = "",
      reviewer_note = ""
    )
  }
) %>%
  arrange(variable_role, variable)


# ============================================================
# 11. Outcome readiness review
# ============================================================

candidate_outcome_vars <- c(
  "a_sex_ever",
  names(analysis_data_valid)[stringr::str_detect(names(analysis_data_valid), "^a_.*_yesno$")]
)

candidate_outcome_vars <- candidate_outcome_vars[
  candidate_outcome_vars %in% names(analysis_data_valid)
]

outcome_distribution_review <- purrr::map_dfr(
  candidate_outcome_vars,
  function(v) {
    bind_rows(
      binary_distribution(sample_full, v, "full_weighted_wave1"),
      binary_distribution(sample_main, v, "main_grade_10_12"),
      binary_distribution(sample_strict, v, "strict_grade_10_12_age_15_19")
    )
  }
) %>%
  mutate(
    public_n_nonmissing = vapply(n_nonmissing, safe_count_public, character(1)),
    public_n_yes = vapply(n_yes, safe_count_public, character(1)),
    public_n_no = vapply(n_no, safe_count_public, character(1)),
    outcome_cell_status = case_when(
      n_nonmissing < 50 ~ "not_recommended_too_few_cases",
      n_yes < 10 | n_no < 10 ~ "review_small_outcome_cell",
      TRUE ~ "usable"
    )
  )

outcome_readiness_review <- outcome_distribution_review %>%
  filter(sample_name == "main_grade_10_12") %>%
  mutate(
    outcome_group = case_when(
      variable == "a_sex_ever" ~ "sexual_initiation",
      stringr::str_detect(variable, "H1CO8|H1CO9") ~ "condom_use",
      stringr::str_detect(variable, "H1CO3|H1CO6|H1CO13") ~ "contraceptive_use",
      stringr::str_detect(variable, "H1FP7|H1FP8") ~ "pregnancy_outcome",
      stringr::str_detect(variable, "H1CO16|H1HS9") ~ "hiv_sti_outcome",
      TRUE ~ "other_binary_recode"
    ),
    preliminary_outcome_decision = case_when(
      n_nonmissing < 50 ~ "exclude_too_few_nonmissing_cases",
      n_yes < 10 | n_no < 10 ~ "review_small_cell_or_rare_outcome",
      weighted_pct_yes < 1 | weighted_pct_yes > 99 ~ "review_extreme_weighted_distribution",
      TRUE ~ "candidate_outcome_for_bivariate_analysis"
    ),
    final_decision = "",
    reviewer_note = ""
  ) %>%
  arrange(
    outcome_group,
    preliminary_outcome_decision,
    variable
  )


# ============================================================
# 12. Construct-level missingness review
# ============================================================

construct_map <- script05_import_check %>%
  filter(import_decision == "import") %>%
  select(
    thesis_construct_block,
    theoretical_model,
    expected_analytic_use,
    analysis_role,
    variable_level,
    mapping_quality,
    sav_variable_name
  ) %>%
  distinct() %>%
  filter(!is.na(sav_variable_name), sav_variable_name != "")

construct_missingness_review <- construct_map %>%
  mutate(
    original_variable = sav_variable_name,
    numeric_clean_variable = paste0("num_", sav_variable_name),
    original_variable_available = original_variable %in% names(analysis_data_valid),
    numeric_clean_variable_available = numeric_clean_variable %in% names(analysis_data_valid)
  ) %>%
  rowwise() %>%
  mutate(
    original_main_n_nonmissing = ifelse(
      original_variable_available,
      sum(!is.na(sample_main[[original_variable]])),
      NA_integer_
    ),
    numeric_main_n_nonmissing = ifelse(
      numeric_clean_variable_available,
      sum(!is.na(sample_main[[numeric_clean_variable]])),
      NA_integer_
    ),
    preferred_review_variable = case_when(
      numeric_clean_variable_available ~ numeric_clean_variable,
      original_variable_available ~ original_variable,
      TRUE ~ NA_character_
    ),
    preferred_main_n_nonmissing = case_when(
      !is.na(preferred_review_variable) ~ sum(!is.na(sample_main[[preferred_review_variable]])),
      TRUE ~ NA_integer_
    ),
    preferred_main_pct_nonmissing = case_when(
      !is.na(preferred_review_variable) & nrow(sample_main) > 0 ~
        round(100 * preferred_main_n_nonmissing / nrow(sample_main), 2),
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  group_by(
    thesis_construct_block,
    theoretical_model,
    expected_analytic_use,
    analysis_role,
    variable_level,
    mapping_quality
  ) %>%
  summarise(
    n_variables_mapped = n(),
    n_variables_available = sum(!is.na(preferred_review_variable)),
    n_variables_high_availability = sum(preferred_main_pct_nonmissing >= 80, na.rm = TRUE),
    n_variables_moderate_or_high_availability = sum(preferred_main_pct_nonmissing >= 60, na.rm = TRUE),
    mean_main_pct_nonmissing = round(mean(preferred_main_pct_nonmissing, na.rm = TRUE), 2),
    min_main_pct_nonmissing = round(min(preferred_main_pct_nonmissing, na.rm = TRUE), 2),
    max_main_pct_nonmissing = round(max(preferred_main_pct_nonmissing, na.rm = TRUE), 2),
    preferred_variables = paste(
      sort(unique(preferred_review_variable[!is.na(preferred_review_variable)])),
      collapse = "; "
    ),
    construct_readiness = case_when(
      n_variables_available == 0 ~ "not_available",
      n_variables_moderate_or_high_availability >= 2 ~ "candidate_for_scale_or_index_review",
      n_variables_available >= 1 ~ "single_item_or_limited_construct_review",
      TRUE ~ "not_assessed"
    ),
    .groups = "drop"
  ) %>%
  arrange(
    construct_readiness,
    thesis_construct_block
  )


# ============================================================
# 13. Sample-level missingness and design summary
# ============================================================

sample_missingness_summary <- tibble(
  sample_name = c(
    "full_weighted_wave1",
    "main_grade_10_12",
    "strict_grade_10_12_age_15_19"
  ),
  n_observations = c(
    nrow(sample_full),
    nrow(sample_main),
    nrow(sample_strict)
  ),
  weighted_population_total = c(
    sum(sample_full$GSWGT1, na.rm = TRUE),
    sum(sample_main$GSWGT1, na.rm = TRUE),
    sum(sample_strict$GSWGT1, na.rm = TRUE)
  ),
  public_n_observations = vapply(
    c(nrow(sample_full), nrow(sample_main), nrow(sample_strict)),
    safe_count_public,
    character(1)
  ),
  has_gswgt1 = c(
    "GSWGT1" %in% names(sample_full),
    "GSWGT1" %in% names(sample_main),
    "GSWGT1" %in% names(sample_strict)
  ),
  has_cluster2 = c(
    "CLUSTER2" %in% names(sample_full),
    "CLUSTER2" %in% names(sample_main),
    "CLUSTER2" %in% names(sample_strict)
  ),
  has_schwt1 = c(
    "SCHWT1" %in% names(sample_full),
    "SCHWT1" %in% names(sample_main),
    "SCHWT1" %in% names(sample_strict)
  ),
  note = c(
    "All Wave I records with valid GSWGT1.",
    "Primary analytical sample: grades 10 to 12.",
    "Sensitivity sample: grades 10 to 12 and ages 15 to 19."
  )
)


# ============================================================
# 14. Methodological notes
# ============================================================

script08_methodological_notes <- tibble(
  note_id = 1:12,
  note = c(
    "Script 08 reviews missing data and final recode readiness before modeling.",
    "The input is the local-only weighted analytical Wave I RDS produced by Script 07.",
    "GSWGT1 is retained as the Wave I population-average weight.",
    "AID is excluded from all public outputs.",
    "The main analytical sample is students in grades 10 to 12 at Wave I.",
    "The strict sensitivity sample is students in grades 10 to 12 and ages 15 to 19.",
    "Variable availability is classified using nonmissing percentages and nonmissing counts.",
    "Outcome readiness is assessed using nonmissing cases and binary cell sizes.",
    "Construct readiness is diagnostic and does not yet validate scales.",
    "Final scale construction requires directionality checks and reliability analysis.",
    "No individual-level data are exported by this script.",
    "Script 09 should perform bivariate association analysis only with variables approved after this review."
  )
)


# ============================================================
# 15. Execution checklist
# ============================================================

script08_checklist <- tibble(
  check_id = 1:18,
  check_item = c(
    "Project root exists",
    "Weighted analytical local-only RDS exists",
    "Script 05 import check exists",
    "Script 06 construct availability exists",
    "Script 06 recode quality exists",
    "Script 07 outcome availability exists",
    "Script 07 weight design diagnostics exists",
    "Weighted analytical data loaded",
    "GSWGT1 available",
    "AID present internally and excluded from public outputs",
    "Sample subsets created",
    "Variable missingness summary created",
    "Model variable decision template created",
    "Recode readiness review created",
    "Outcome readiness review created",
    "Construct missingness review created",
    "Excel diagnostic workbook exported",
    "Markdown documentation exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(weighted_analytical_rds_path), "OK", "FAIL"),
    ifelse(file.exists(script05_import_check_path), "OK", "FAIL"),
    ifelse(file.exists(script06_construct_availability_path), "OK", "FAIL"),
    ifelse(file.exists(script06_recode_quality_path), "OK", "FAIL"),
    ifelse(file.exists(script07_outcome_availability_path), "OK", "FAIL"),
    ifelse(file.exists(script07_weight_design_path), "OK", "FAIL"),
    "OK",
    ifelse("GSWGT1" %in% names(analysis_data), "OK", "FAIL"),
    ifelse("AID" %in% names(analysis_data) && !"AID" %in% variables_to_review, "OK", "FAIL"),
    "OK",
    ifelse(nrow(variable_missingness_summary) > 0, "OK", "FAIL"),
    ifelse(nrow(model_variable_decision_template) > 0, "OK", "FAIL"),
    ifelse(nrow(recode_readiness_review) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(outcome_readiness_review) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(construct_missingness_review) > 0, "OK", "WARNING_EMPTY"),
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 16. Export public CSV outputs
# ============================================================

write_csv(
  variable_missingness_summary,
  file.path(outputs_tables_dir, "script08_wave01_variable_missingness_summary.csv")
)

write_csv(
  model_variable_decision_template,
  file.path(outputs_tables_dir, "script08_wave01_model_variable_decision_template.csv")
)

write_csv(
  recode_readiness_review,
  file.path(outputs_tables_dir, "script08_wave01_recode_readiness_review.csv")
)

write_csv(
  outcome_distribution_review,
  file.path(outputs_tables_dir, "script08_wave01_outcome_distribution_review.csv")
)

write_csv(
  outcome_readiness_review,
  file.path(outputs_tables_dir, "script08_wave01_outcome_readiness_review.csv")
)

write_csv(
  construct_missingness_review,
  file.path(outputs_tables_dir, "script08_wave01_construct_missingness_review.csv")
)

write_csv(
  sample_missingness_summary,
  file.path(outputs_tables_dir, "script08_wave01_sample_missingness_summary.csv")
)

write_csv(
  script08_methodological_notes,
  file.path(outputs_tables_dir, "script08_wave01_methodological_notes.csv")
)


# ============================================================
# 17. Excel diagnostic workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script08_wave01_missing_data_recode_review.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "variable_missingness")
writeData(wb, "variable_missingness", variable_missingness_summary)

addWorksheet(wb, "model_decisions")
writeData(wb, "model_decisions", model_variable_decision_template)

addWorksheet(wb, "recode_readiness")
writeData(wb, "recode_readiness", recode_readiness_review)

addWorksheet(wb, "outcome_distribution")
writeData(wb, "outcome_distribution", outcome_distribution_review)

addWorksheet(wb, "outcome_readiness")
writeData(wb, "outcome_readiness", outcome_readiness_review)

addWorksheet(wb, "construct_missingness")
writeData(wb, "construct_missingness", construct_missingness_review)

addWorksheet(wb, "sample_summary")
writeData(wb, "sample_summary", sample_missingness_summary)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script08_methodological_notes)

addWorksheet(wb, "checklist")
writeData(wb, "checklist", script08_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:60, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script08_checklist$status[
  script08_checklist$check_item == "Excel diagnostic workbook exported"
] <- "OK"


# ============================================================
# 18. Markdown documentation
# ============================================================

script08_doc <- c(
  "# Missing Data and Final Recode Review",
  "",
  "Script 08 reviews missingness, recode readiness, outcome availability and construct usability before modeling.",
  "",
  "## Input",
  "",
  "`data/processed/add_health_wave01_analytical_weighted_local_only.rds`",
  "",
  "This file is local-only and must not be committed to GitHub.",
  "",
  "## Main sample",
  "",
  "The main analytical sample is students in grades 10 to 12 at Wave I.",
  "",
  "## Weight",
  "",
  "`GSWGT1` is retained as the Wave I population-average sampling weight.",
  "",
  "## Public outputs",
  "",
  "The script exports aggregate diagnostics only:",
  "",
  "- variable missingness summary;",
  "- model variable decision template;",
  "- recode readiness review;",
  "- outcome distribution review;",
  "- outcome readiness review;",
  "- construct missingness review;",
  "- sample missingness summary.",
  "",
  "## Privacy protection",
  "",
  "`AID` is used only internally and is excluded from public outputs.",
  "",
  "## Next step",
  "",
  "Script 09 should perform bivariate association analysis using only variables approved after this review."
)

writeLines(
  script08_doc,
  con = file.path(docs_dir, "missing_data_final_recode_review_script08.md")
)

script08_checklist$status[
  script08_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 19. Save final checklist
# ============================================================

write_csv(
  script08_checklist,
  file.path(outputs_diag_dir, "script08_execution_checklist.csv")
)


# ============================================================
# 20. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 08 completed: Missing Data and Final Recode Review\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Input local-only weighted analytical file:\n")
cat(weighted_analytical_rds_path, "\n\n")

cat("Sample summary:\n")
cat("- Full weighted Wave I observations: ", nrow(sample_full), "\n", sep = "")
cat("- Main grade 10-12 observations: ", nrow(sample_main), "\n", sep = "")
cat("- Strict grade 10-12 and age 15-19 observations: ", nrow(sample_strict), "\n\n", sep = "")

cat("Variable review:\n")
cat("- Variables reviewed publicly: ", length(variables_to_review), "\n", sep = "")
cat("- Analytical recodes reviewed: ", length(analytical_recode_vars), "\n", sep = "")
cat("- Candidate outcome variables reviewed: ", length(candidate_outcome_vars), "\n\n", sep = "")

cat("Main sample preliminary modeling decisions:\n")
print(
  model_variable_decision_template %>%
    count(preliminary_modeling_decision, name = "n_variables")
)

cat("\nOutcome readiness decisions:\n")
print(
  outcome_readiness_review %>%
    count(preliminary_outcome_decision, name = "n_outcomes")
)

cat("\nConstruct readiness decisions:\n")
print(
  construct_missingness_review %>%
    count(construct_readiness, name = "n_constructs")
)

cat("\nPublic outputs created:\n")
cat("- outputs/tables/script08_wave01_variable_missingness_summary.csv\n")
cat("- outputs/tables/script08_wave01_model_variable_decision_template.csv\n")
cat("- outputs/tables/script08_wave01_recode_readiness_review.csv\n")
cat("- outputs/tables/script08_wave01_outcome_distribution_review.csv\n")
cat("- outputs/tables/script08_wave01_outcome_readiness_review.csv\n")
cat("- outputs/tables/script08_wave01_construct_missingness_review.csv\n")
cat("- outputs/tables/script08_wave01_sample_missingness_summary.csv\n")
cat("- outputs/tables/script08_wave01_methodological_notes.csv\n")
cat("- outputs/tables/script08_wave01_missing_data_recode_review.xlsx\n")
cat("- outputs/diagnostics/script08_execution_checklist.csv\n")
cat("- docs/missing_data_final_recode_review_script08.md\n\n")

cat("Execution checklist:\n")
print(script08_checklist)

cat("\nImportant note:\n")
cat("Do not commit data/raw/, data/processed/, AID-level files or any individual-level dataset to GitHub.\n")
cat("Script 09 should use only variables approved after reviewing Script 08 outputs.\n\n")