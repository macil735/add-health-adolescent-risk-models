# ============================================================
# Project: add-health-adolescent-risk-models
# Script 09b: Data Review and Modeling Framework
# Author: Gelo Picol
#
# Purpose:
#   Review available data, classify variables into theoretical
#   modeling blocks, define outcomes, identify core controls,
#   review eligible predictors, and prepare the modeling framework
#   for Script 10.
#
# Important:
#   - This script does not estimate regression models.
#   - It does not export individual-level microdata.
#   - AID is used only internally and excluded from public outputs.
#   - Outputs are aggregate/review tables only.
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

script08_variable_missingness_path <- file.path(
  outputs_tables_dir,
  "script08_wave01_variable_missingness_summary.csv"
)

script08_model_decision_path <- file.path(
  outputs_tables_dir,
  "script08_wave01_model_variable_decision_template.csv"
)

script08_outcome_readiness_path <- file.path(
  outputs_tables_dir,
  "script08_wave01_outcome_readiness_review.csv"
)

script08_construct_missingness_path <- file.path(
  outputs_tables_dir,
  "script08_wave01_construct_missingness_review.csv"
)

script09_bivariate_summary_path <- file.path(
  outputs_tables_dir,
  "script09_wave01_bivariate_association_summary.csv"
)

script09_predictor_metadata_path <- file.path(
  outputs_tables_dir,
  "script09_wave01_predictor_metadata.csv"
)

script09_outcome_metadata_path <- file.path(
  outputs_tables_dir,
  "script09_wave01_outcome_metadata.csv"
)

script09_top_candidates_path <- file.path(
  outputs_tables_dir,
  "script09_wave01_top_bivariate_candidates.csv"
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

is_binary_01 <- function(x) {
  x_num <- safe_numeric(x)
  vals <- sort(unique(x_num[!is.na(x_num)]))

  length(vals) > 0 && all(vals %in% c(0, 1))
}

extract_add_health_token <- function(x) {
  token <- stringr::str_extract(
    stringr::str_to_upper(x),
    "H[0-9][A-Z]+[0-9A-Z]*"
  )

  ifelse(is.na(token), NA_character_, token)
}

infer_original_item <- function(variable) {
  variable_clean <- variable

  variable_clean <- stringr::str_remove(variable_clean, "^num_")
  variable_clean <- stringr::str_remove(variable_clean, "^a_")
  variable_clean <- stringr::str_remove(variable_clean, "_yesno$")

  token <- extract_add_health_token(variable_clean)

  ifelse(is.na(token), variable_clean, token)
}

classify_variable_role_basic <- function(variable) {
  dplyr::case_when(
    variable %in% c(
      "GSWGT1", "CLUSTER2", "SCHWT1",
      "valid_gswgt1", "valid_cluster2", "valid_schwt1"
    ) ~ "weight_or_design_variable",

    variable %in% c(
      "a_female", "a_age_wave1", "a_grade_wave1"
    ) ~ "core_control",

    variable %in% c(
      "a_main_sample_grade_10_12",
      "a_strict_sample_grade_age",
      "a_sample_grade_10_12",
      "a_sample_age_15_19"
    ) ~ "sample_definition_variable",

    variable == "a_sex_ever" ~ "candidate_outcome",

    stringr::str_detect(variable, "^a_.*_yesno$") ~ "candidate_binary_recode",

    stringr::str_detect(variable, "^a_") ~ "analytical_recode",

    stringr::str_detect(variable, "^num_") ~ "numeric_cleaned_item",

    TRUE ~ "original_imported_variable"
  )
}

classify_outcome_family <- function(variable) {
  dplyr::case_when(
    variable == "a_sex_ever" ~ "sexual_initiation",
    stringr::str_detect(variable, "H1CO8|H1CO9") ~ "condom_use",
    stringr::str_detect(variable, "H1CO3|H1CO6|H1CO13") ~ "contraceptive_use",
    stringr::str_detect(variable, "H1FP7|H1FP8|H1FP") ~ "pregnancy_or_reproductive_experience",
    stringr::str_detect(variable, "H1CO16|H1HS9|H1HS") ~ "hiv_sti_or_sexual_health",
    TRUE ~ "other_sexual_reproductive_outcome"
  )
}

classify_theoretical_block <- function(
    variable,
    variable_label = NA_character_,
    thesis_construct_block = NA_character_,
    theoretical_model = NA_character_,
    expected_analytic_use = NA_character_,
    analysis_role = NA_character_
) {

  text <- paste(
    variable,
    variable_label,
    thesis_construct_block,
    theoretical_model,
    expected_analytic_use,
    analysis_role,
    collapse = " "
  )

  text <- stringr::str_to_lower(text)

  dplyr::case_when(
    variable %in% c(
      "GSWGT1", "CLUSTER2", "SCHWT1",
      "valid_gswgt1", "valid_cluster2", "valid_schwt1"
    ) ~ "weight_design",

    variable %in% c(
      "a_main_sample_grade_10_12",
      "a_strict_sample_grade_age",
      "a_sample_grade_10_12",
      "a_sample_age_15_19"
    ) ~ "sample_definition",

    variable %in% c(
      "a_female", "a_age_wave1", "a_grade_wave1"
    ) ~ "core_sociodemographic_controls",

    stringr::str_detect(
      text,
      "race|racial|ethnic|ethnicity|hispanic|white|black|asian|native|sex|gender|age|grade|demograph"
    ) ~ "sociodemographic_controls",

    stringr::str_detect(
      text,
      "family|parent|mother|father|home|household|guardian|fam|parental"
    ) ~ "family_household_context",

    stringr::str_detect(
      text,
      "school|education|academic|teacher|student|grade|class|college|performance"
    ) ~ "school_educational_context",

    stringr::str_detect(
      text,
      "hiv|aids|knowledge|attitude|belief|perception|perceived|condom|contracept|pregnan|prevention"
    ) ~ "knowledge_attitudes_perceptions",

    stringr::str_detect(
      text,
      "peer|friend|romantic|relationship|partner|date|dating|boyfriend|girlfriend"
    ) ~ "peers_relationship_context",

    stringr::str_detect(
      text,
      "alcohol|smok|cigarette|drug|substance|delinquen|violence|risk behavior|problem behavior"
    ) ~ "general_risk_behaviors",

    stringr::str_detect(
      text,
      "sexual|sex |sex_|intercourse|pregnancy|sti|std|reproductive|contraception|condom"
    ) ~ "sexual_reproductive_behavior_near_outcome",

    TRUE ~ "other_review_required"
  )
}

classify_predictor_use_group <- function(theoretical_block, variable_role) {
  dplyr::case_when(
    theoretical_block == "core_sociodemographic_controls" ~ "mandatory_core_control",
    theoretical_block == "sociodemographic_controls" ~ "optional_sociodemographic_control",
    theoretical_block == "family_household_context" ~ "family_block_predictor",
    theoretical_block == "school_educational_context" ~ "school_block_predictor",
    theoretical_block == "knowledge_attitudes_perceptions" ~ "knowledge_attitude_block_predictor",
    theoretical_block == "peers_relationship_context" ~ "peer_relationship_block_predictor",
    theoretical_block == "general_risk_behaviors" ~ "general_risk_behavior_block_predictor",
    theoretical_block == "sexual_reproductive_behavior_near_outcome" ~ "near_outcome_predictor_review",
    variable_role %in% c("weight_or_design_variable", "sample_definition_variable") ~ "support_variable_not_predictor",
    TRUE ~ "other_predictor_review"
  )
}

priority_from_outcome_family <- function(outcome_family) {
  dplyr::case_when(
    outcome_family == "sexual_initiation" ~ "primary",
    outcome_family == "condom_use" ~ "primary_or_secondary",
    outcome_family == "contraceptive_use" ~ "primary_or_secondary",
    outcome_family == "pregnancy_or_reproductive_experience" ~ "secondary_if_cases_sufficient",
    outcome_family == "hiv_sti_or_sexual_health" ~ "secondary_if_cases_sufficient",
    TRUE ~ "review"
  )
}

recommend_predictor_for_outcome <- function(
    predictor_use_group,
    bivariate_decision,
    p_value_bh,
    p_value
) {
  dplyr::case_when(
    predictor_use_group == "mandatory_core_control" ~ "force_include_as_core_control",
    predictor_use_group == "optional_sociodemographic_control" ~ "include_or_review_as_control",
    predictor_use_group == "support_variable_not_predictor" ~ "exclude_support_variable",
    predictor_use_group == "near_outcome_predictor_review" &
      bivariate_decision == "candidate_association_after_bh_adjustment" ~
      "review_carefully_due_to_near_outcome_risk",
    bivariate_decision == "candidate_association_after_bh_adjustment" ~
      "candidate_key_predictor",
    bivariate_decision == "candidate_association_unadjusted_only" ~
      "secondary_candidate_review",
    bivariate_decision %in% c(
      "insufficient_complete_cases",
      "insufficient_outcome_cell_size",
      "test_not_available"
    ) ~ "exclude_or_review_insufficient_data",
    TRUE ~ "not_prioritized_for_script10"
  )
}

lookup_top_predictors <- function(tbl, outcome_value, block_value, max_items = 5) {
  out <- tbl %>%
    filter(
      outcome == outcome_value,
      theoretical_block == block_value
    ) %>%
    arrange(
      p_value_bh,
      p_value,
      desc(abs(effect_value))
    ) %>%
    slice_head(n = max_items) %>%
    pull(predictor)

  if (length(out) == 0) {
    return("")
  }

  paste(unique(out), collapse = "; ")
}


# ============================================================
# 4. Check required inputs
# ============================================================

required_inputs <- c(
  weighted_analytical_rds_path,
  script05_import_check_path,
  script08_variable_missingness_path,
  script08_model_decision_path,
  script08_outcome_readiness_path,
  script08_construct_missingness_path,
  script09_bivariate_summary_path,
  script09_predictor_metadata_path,
  script09_outcome_metadata_path,
  script09_top_candidates_path
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
# 5. Load data and prior diagnostics
# ============================================================

analysis_data <- readRDS(weighted_analytical_rds_path)

script05_import_check <- read_csv(
  script05_import_check_path,
  show_col_types = FALSE
)

variable_missingness_08 <- read_csv(
  script08_variable_missingness_path,
  show_col_types = FALSE
)

model_decision_08 <- read_csv(
  script08_model_decision_path,
  show_col_types = FALSE
)

outcome_readiness_08 <- read_csv(
  script08_outcome_readiness_path,
  show_col_types = FALSE
)

construct_missingness_08 <- read_csv(
  script08_construct_missingness_path,
  show_col_types = FALSE
)

bivariate_summary_09 <- read_csv(
  script09_bivariate_summary_path,
  show_col_types = FALSE
)

predictor_metadata_09 <- read_csv(
  script09_predictor_metadata_path,
  show_col_types = FALSE
)

outcome_metadata_09 <- read_csv(
  script09_outcome_metadata_path,
  show_col_types = FALSE
)

top_candidates_09 <- read_csv(
  script09_top_candidates_path,
  show_col_types = FALSE
)

if (!"AID" %in% names(analysis_data)) {
  stop("AID should exist internally in the local-only weighted analytical file.")
}

if (!"GSWGT1" %in% names(analysis_data)) {
  stop("GSWGT1 not found in the local-only weighted analytical file.")
}


# ============================================================
# 6. Prepare metadata from Script 05
# ============================================================

import_metadata <- script05_import_check %>%
  mutate(
    original_item = sav_variable_name
  ) %>%
  select(
    original_item,
    thesis_construct_block,
    theoretical_model,
    expected_analytic_use,
    analysis_role,
    variable_level,
    mapping_quality,
    everything()
  ) %>%
  distinct(original_item, .keep_all = TRUE)


# ============================================================
# 7. Core data review inventory
# ============================================================

public_variables <- setdiff(names(analysis_data), "AID")

public_variables <- public_variables[
  !stringr::str_detect(public_variables, "^stat_")
]

data_review_inventory <- tibble(
  variable = public_variables,
  original_item = vapply(public_variables, infer_original_item, character(1)),
  variable_label = vapply(
    public_variables,
    function(v) get_variable_label(analysis_data[[v]]),
    character(1)
  ),
  variable_class = vapply(
    public_variables,
    function(v) paste(class(analysis_data[[v]]), collapse = "; "),
    character(1)
  ),
  variable_role_basic = vapply(
    public_variables,
    classify_variable_role_basic,
    character(1)
  ),
  is_binary_01 = vapply(
    public_variables,
    function(v) is_binary_01(analysis_data[[v]]),
    logical(1)
  ),
  n_unique_nonmissing = vapply(
    public_variables,
    function(v) dplyr::n_distinct(analysis_data[[v]], na.rm = TRUE),
    integer(1)
  )
) %>%
  left_join(
    import_metadata,
    by = "original_item"
  ) %>%
  rowwise() %>%
  mutate(
    theoretical_block = classify_theoretical_block(
      variable = variable,
      variable_label = variable_label,
      thesis_construct_block = thesis_construct_block,
      theoretical_model = theoretical_model,
      expected_analytic_use = expected_analytic_use,
      analysis_role = analysis_role
    ),
    predictor_use_group = classify_predictor_use_group(
      theoretical_block,
      variable_role_basic
    )
  ) %>%
  ungroup()


# ============================================================
# 8. Add Script 08 availability and modeling decision
# ============================================================

modeling_variable_blocks <- data_review_inventory %>%
  left_join(
    model_decision_08 %>%
      select(
        variable,
        main_n_total,
        main_n_nonmissing,
        main_pct_nonmissing,
        main_weighted_pct_nonmissing,
        main_n_unique_nonmissing,
        main_is_binary_01,
        main_availability_class,
        preliminary_modeling_decision
      ),
    by = "variable"
  ) %>%
  mutate(
    recommended_general_role = case_when(
      predictor_use_group == "mandatory_core_control" ~ "mandatory_control_in_all_models",
      predictor_use_group == "optional_sociodemographic_control" ~ "optional_control_review",
      preliminary_modeling_decision == "candidate_for_modeling" &
        predictor_use_group %in% c(
          "family_block_predictor",
          "school_block_predictor",
          "knowledge_attitude_block_predictor",
          "peer_relationship_block_predictor",
          "general_risk_behavior_block_predictor"
        ) ~ "eligible_predictor_block",
      predictor_use_group == "near_outcome_predictor_review" ~ "eligible_only_after_outcome_proximity_review",
      variable_role_basic %in% c(
        "weight_or_design_variable",
        "sample_definition_variable"
      ) ~ "support_variable_not_predictor",
      preliminary_modeling_decision %in% c(
        "exclude_high_missingness",
        "exclude_too_few_cases"
      ) ~ "exclude_from_modeling",
      TRUE ~ "review"
    ),
    final_decision = "",
    reviewer_note = ""
  ) %>%
  arrange(
    recommended_general_role,
    theoretical_block,
    variable
  )


# ============================================================
# 9. Sample and design review
# ============================================================

analysis_data_valid <- analysis_data %>%
  filter(!is.na(GSWGT1) & GSWGT1 > 0)

sample_full <- analysis_data_valid

sample_main <- analysis_data_valid %>%
  filter(a_main_sample_grade_10_12 == TRUE)

sample_strict <- analysis_data_valid %>%
  filter(a_strict_sample_grade_age == TRUE)

data_sample_review <- tibble(
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
  public_n_observations = vapply(
    c(nrow(sample_full), nrow(sample_main), nrow(sample_strict)),
    safe_count_public,
    character(1)
  ),
  weighted_population_total = c(
    sum(sample_full$GSWGT1, na.rm = TRUE),
    sum(sample_main$GSWGT1, na.rm = TRUE),
    sum(sample_strict$GSWGT1, na.rm = TRUE)
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
  region_available = FALSE,
  w1_wc_available = FALSE,
  modeling_use = c(
    "descriptive_reference_only",
    "primary_modeling_sample",
    "sensitivity_modeling_sample"
  ),
  note = c(
    "All Wave I observations with valid GSWGT1.",
    "Primary analytical sample: students in grades 10 to 12.",
    "Sensitivity sample: students in grades 10 to 12 and ages 15 to 19."
  )
)

# ============================================================
# 10. Outcome modeling plan
# ============================================================

outcome_metadata_clean <- outcome_metadata_09 %>%
  rename(variable = outcome) %>%
  select(
    variable,
    outcome_label
  ) %>%
  distinct(variable, .keep_all = TRUE)

outcome_modeling_plan <- outcome_readiness_08 %>%
  filter(
    preliminary_outcome_decision %in% c(
      "candidate_outcome_for_bivariate_analysis",
      "review_small_cell_or_rare_outcome"
    )
  ) %>%
  mutate(
    outcome_family = vapply(variable, classify_outcome_family, character(1)),
    modeling_priority = vapply(outcome_family, priority_from_outcome_family, character(1))
  ) %>%
  left_join(
    outcome_metadata_clean,
    by = "variable"
  ) %>%
  mutate(
    recommended_modeling_status = case_when(
      preliminary_outcome_decision == "candidate_outcome_for_bivariate_analysis" &
        modeling_priority %in% c("primary", "primary_or_secondary") ~
        "recommended_for_script10_primary_or_secondary_model",

      preliminary_outcome_decision == "candidate_outcome_for_bivariate_analysis" &
        modeling_priority == "secondary_if_cases_sufficient" ~
        "recommended_for_script10_secondary_model",

      preliminary_outcome_decision == "review_small_cell_or_rare_outcome" ~
        "review_before_modeling",

      TRUE ~ "not_prioritized"
    ),
    final_decision = "",
    reviewer_note = ""
  ) %>%
  arrange(
    modeling_priority,
    outcome_family,
    variable
  )

# ============================================================
# 11. Core controls review
# ============================================================

race_ethnicity_pattern <- paste(
  c(
    "race",
    "racial",
    "ethnic",
    "ethnicity",
    "hispanic",
    "latino",
    "white",
    "black",
    "asian",
    "native",
    "american indian",
    "pacific islander"
  ),
  collapse = "|"
)

mandatory_core_controls <- c(
  "a_age_wave1",
  "a_female",
  "a_grade_wave1"
)

duplicate_core_sources <- c(
  "BIO_SEX",
  "H1GI20"
)

core_control_candidates <- modeling_variable_blocks %>%
  mutate(
    control_review_text = stringr::str_to_lower(
      paste(
        variable,
        variable_label,
        thesis_construct_block,
        theoretical_model,
        expected_analytic_use,
        analysis_role,
        sep = " "
      )
    ),
    is_mandatory_core_control = variable %in% mandatory_core_controls,
    is_duplicate_core_source = variable %in% duplicate_core_sources,
    is_race_ethnicity_candidate = stringr::str_detect(
      control_review_text,
      race_ethnicity_pattern
    )
  ) %>%
  filter(
    is_mandatory_core_control |
      (
        is_race_ethnicity_candidate &
          preliminary_modeling_decision == "candidate_for_modeling" &
          !is_duplicate_core_source
      )
  ) %>%
  select(
    variable,
    original_item,
    variable_label,
    theoretical_block,
    predictor_use_group,
    main_n_nonmissing,
    main_pct_nonmissing,
    main_weighted_pct_nonmissing,
    preliminary_modeling_decision,
    recommended_general_role,
    is_mandatory_core_control,
    is_race_ethnicity_candidate,
    is_duplicate_core_source
  ) %>%
  mutate(
    core_control_decision = case_when(
      variable %in% mandatory_core_controls ~ "include_in_all_models",
      is_race_ethnicity_candidate ~ "review_for_inclusion_as_race_ethnicity_control",
      TRUE ~ "review_or_exclude"
    ),
    final_decision = "",
    reviewer_note = ""
  ) %>%
  arrange(
    core_control_decision,
    variable
  )

# ============================================================
# 12. Predictor-outcome eligibility review
# ============================================================

predictor_block_lookup <- modeling_variable_blocks %>%
  select(
    predictor = variable,
    original_item,
    variable_label,
    theoretical_block,
    predictor_use_group,
    preliminary_modeling_decision,
    recommended_general_role
  )

outcome_family_lookup <- outcome_modeling_plan %>%
  select(
    outcome = variable,
    outcome_family,
    modeling_priority,
    recommended_modeling_status
  )

predictor_outcome_eligibility <- bivariate_summary_09 %>%
  filter(sample_name == "main_grade_10_12") %>%
  left_join(
    predictor_block_lookup,
    by = "predictor"
  ) %>%
  left_join(
    outcome_family_lookup,
    by = "outcome"
  ) %>%
  mutate(
    script10_recommendation = recommend_predictor_for_outcome(
      predictor_use_group = predictor_use_group,
      bivariate_decision = preliminary_bivariate_decision,
      p_value_bh = p_value_bh,
      p_value = p_value
    ),
    outcome_proximity_warning = case_when(
      predictor_use_group == "near_outcome_predictor_review" ~ TRUE,
      theoretical_block == "sexual_reproductive_behavior_near_outcome" ~ TRUE,
      TRUE ~ FALSE
    ),
    final_decision = "",
    reviewer_note = ""
  ) %>%
  arrange(
    outcome,
    script10_recommendation,
    p_value_bh,
    p_value,
    desc(abs(effect_value))
  )


# ============================================================
# 13. Predictor exclusion and review list
# ============================================================

predictor_exclusion_review <- modeling_variable_blocks %>%
  filter(
    recommended_general_role %in% c(
      "exclude_from_modeling",
      "eligible_only_after_outcome_proximity_review",
      "review",
      "support_variable_not_predictor"
    )
  ) %>%
  select(
    variable,
    original_item,
    variable_label,
    variable_role_basic,
    theoretical_block,
    predictor_use_group,
    main_n_nonmissing,
    main_pct_nonmissing,
    preliminary_modeling_decision,
    recommended_general_role,
    final_decision,
    reviewer_note
  ) %>%
  arrange(
    recommended_general_role,
    theoretical_block,
    variable
  )


# ============================================================
# 14. Block summary
# ============================================================

theoretical_block_summary <- modeling_variable_blocks %>%
  group_by(theoretical_block, predictor_use_group, recommended_general_role) %>%
  summarise(
    n_variables = n(),
    n_candidate_for_modeling = sum(
      preliminary_modeling_decision == "candidate_for_modeling",
      na.rm = TRUE
    ),
    mean_main_pct_nonmissing = round(
      mean(main_pct_nonmissing, na.rm = TRUE),
      2
    ),
    .groups = "drop"
  ) %>%
  arrange(
    theoretical_block,
    predictor_use_group,
    recommended_general_role
  )


# ============================================================
# 15. Recommended model sequence for Script 10
# ============================================================

candidate_predictor_lookup <- predictor_outcome_eligibility %>%
  filter(
    script10_recommendation %in% c(
      "candidate_key_predictor",
      "secondary_candidate_review",
      "review_carefully_due_to_near_outcome_risk"
    )
  )

model_stages <- tibble(
  model_stage = c(
    "M0_core_controls",
    "M1_family_context",
    "M2_school_context",
    "M3_knowledge_attitudes",
    "M4_peers_relationships",
    "M5_general_risk_behaviors",
    "M6_final_parsimonious_model"
  ),
  model_purpose = c(
    "Baseline model with mandatory controls.",
    "Assess family and household context net of core controls.",
    "Assess school and educational context net of core controls.",
    "Assess knowledge, attitudes and perceptions net of core controls.",
    "Assess peer and relationship context net of core controls.",
    "Assess general risk behaviors net of core controls.",
    "Combine theoretically defensible and empirically supported predictors."
  ),
  theoretical_block_for_stage = c(
    "core_sociodemographic_controls",
    "family_household_context",
    "school_educational_context",
    "knowledge_attitudes_perceptions",
    "peers_relationship_context",
    "general_risk_behaviors",
    "final_combined"
  )
)

recommended_model_sequence <- tidyr::crossing(
  outcome = outcome_modeling_plan$variable,
  model_stages
) %>%
  left_join(
    outcome_modeling_plan %>%
      select(
        outcome = variable,
        outcome_family,
        modeling_priority,
        recommended_modeling_status
      ),
    by = "outcome"
  ) %>%
  rowwise() %>%
  mutate(
    mandatory_controls = paste(
      core_control_candidates %>%
        filter(core_control_decision == "include_in_all_models") %>%
        pull(variable),
      collapse = "; "
    ),
    optional_controls_for_review = paste(
  core_control_candidates %>%
    filter(core_control_decision == "review_for_inclusion_as_race_ethnicity_control") %>%
    pull(variable),
  collapse = "; "
),
    suggested_predictors = case_when(
      theoretical_block_for_stage == "final_combined" ~ paste(
        candidate_predictor_lookup %>%
          filter(outcome == .env$outcome) %>%
          arrange(p_value_bh, p_value, desc(abs(effect_value))) %>%
          slice_head(n = 12) %>%
          pull(predictor) %>%
          unique(),
        collapse = "; "
      ),
      theoretical_block_for_stage == "core_sociodemographic_controls" ~ "",
      TRUE ~ lookup_top_predictors(
        candidate_predictor_lookup,
        outcome_value = outcome,
        block_value = theoretical_block_for_stage,
        max_items = 5
      )
    ),
    model_status = case_when(
      recommended_modeling_status %in% c(
        "recommended_for_script10_primary_or_secondary_model",
        "recommended_for_script10_secondary_model"
      ) ~ "ready_for_script10_specification",
      TRUE ~ "review_before_script10"
    ),
    reviewer_note = ""
  ) %>%
  ungroup() %>%
  arrange(
    modeling_priority,
    outcome_family,
    outcome,
    model_stage
  )


# ============================================================
# 16. Methodological notes
# ============================================================

script09b_methodological_notes <- tibble(
  note_id = 1:16,
  note = c(
    "Script 09b reviews the analytical data and prepares the modeling framework for Script 10.",
    "No regression model is estimated in this script.",
    "The main analytical sample is students in grades 10 to 12 at Wave I.",
    "The strict sensitivity sample is students in grades 10 to 12 and ages 15 to 19.",
    "GSWGT1 remains the main Wave I population-average sampling weight.",
    "REGION was not available in the current local files, so regional modeling is not planned.",
    "CLUSTER2 and SCHWT1 are documented but not used as substantive region variables.",
    "Core controls are sex, age and grade when available.",
    "Race or ethnicity variables, if detected, are treated as optional sociodemographic controls for review.",
    "Predictors are grouped into sociodemographic, family, school, knowledge/attitudes, peers/relationships and general risk behavior blocks.",
    "Sexual and reproductive behavior variables close to an outcome are flagged for careful review.",
    "Bivariate associations from Script 09 are used for screening, not for causal interpretation.",
    "The final model should be parsimonious and theoretically defensible.",
    "Variables excluded by missingness or small cell size should not enter Script 10 unless manually justified.",
    "AID is used only internally and is excluded from public outputs.",
    "Script 10 should estimate weighted logistic models using this framework."
  )
)


# ============================================================
# 17. Execution checklist
# ============================================================

script09b_checklist <- tibble(
  check_id = 1:22,
  check_item = c(
    "Project root exists",
    "Weighted analytical local-only RDS exists",
    "Script 05 import check exists",
    "Script 08 variable missingness exists",
    "Script 08 model decision template exists",
    "Script 08 outcome readiness exists",
    "Script 08 construct missingness exists",
    "Script 09 bivariate summary exists",
    "Script 09 predictor metadata exists",
    "Script 09 outcome metadata exists",
    "Script 09 top candidates exists",
    "Weighted analytical data loaded",
    "AID present internally and excluded from public outputs",
    "GSWGT1 available",
    "Data review inventory created",
    "Modeling variable blocks created",
    "Outcome modeling plan created",
    "Core controls review created",
    "Predictor-outcome eligibility review created",
    "Recommended model sequence created",
    "Excel workbook exported",
    "Markdown documentation exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(weighted_analytical_rds_path), "OK", "FAIL"),
    ifelse(file.exists(script05_import_check_path), "OK", "FAIL"),
    ifelse(file.exists(script08_variable_missingness_path), "OK", "FAIL"),
    ifelse(file.exists(script08_model_decision_path), "OK", "FAIL"),
    ifelse(file.exists(script08_outcome_readiness_path), "OK", "FAIL"),
    ifelse(file.exists(script08_construct_missingness_path), "OK", "FAIL"),
    ifelse(file.exists(script09_bivariate_summary_path), "OK", "FAIL"),
    ifelse(file.exists(script09_predictor_metadata_path), "OK", "FAIL"),
    ifelse(file.exists(script09_outcome_metadata_path), "OK", "FAIL"),
    ifelse(file.exists(script09_top_candidates_path), "OK", "FAIL"),
    "OK",
    ifelse("AID" %in% names(analysis_data) && !"AID" %in% public_variables, "OK", "FAIL"),
    ifelse("GSWGT1" %in% names(analysis_data), "OK", "FAIL"),
    ifelse(nrow(data_review_inventory) > 0, "OK", "FAIL"),
    ifelse(nrow(modeling_variable_blocks) > 0, "OK", "FAIL"),
    ifelse(nrow(outcome_modeling_plan) > 0, "OK", "FAIL"),
    ifelse(nrow(core_control_candidates) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(predictor_outcome_eligibility) > 0, "OK", "FAIL"),
    ifelse(nrow(recommended_model_sequence) > 0, "OK", "FAIL"),
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 18. Export public CSV outputs
# ============================================================

write_csv(
  data_sample_review,
  file.path(outputs_tables_dir, "script09b_wave01_data_sample_review.csv")
)

write_csv(
  data_review_inventory,
  file.path(outputs_tables_dir, "script09b_wave01_data_review_inventory.csv")
)

write_csv(
  theoretical_block_summary,
  file.path(outputs_tables_dir, "script09b_wave01_theoretical_block_summary.csv")
)

write_csv(
  modeling_variable_blocks,
  file.path(outputs_tables_dir, "script09b_wave01_modeling_variable_blocks.csv")
)

write_csv(
  outcome_modeling_plan,
  file.path(outputs_tables_dir, "script09b_wave01_outcome_modeling_plan.csv")
)

write_csv(
  core_control_candidates,
  file.path(outputs_tables_dir, "script09b_wave01_core_controls_review.csv")
)

write_csv(
  predictor_outcome_eligibility,
  file.path(outputs_tables_dir, "script09b_wave01_predictor_outcome_eligibility.csv")
)

write_csv(
  predictor_exclusion_review,
  file.path(outputs_tables_dir, "script09b_wave01_predictor_exclusion_review.csv")
)

write_csv(
  recommended_model_sequence,
  file.path(outputs_tables_dir, "script09b_wave01_recommended_model_sequence.csv")
)

write_csv(
  script09b_methodological_notes,
  file.path(outputs_tables_dir, "script09b_wave01_methodological_notes.csv")
)


# ============================================================
# 19. Markdown documentation
# ============================================================

script09b_doc <- c(
  "# Data Review and Modeling Framework",
  "",
  "Script 09b reviews the analytical data and prepares the modeling framework for Script 10.",
  "",
  "## Purpose",
  "",
  "This script does not estimate regression models. It organizes the data, outcomes, controls and predictors before logistic modeling.",
  "",
  "## Samples",
  "",
  "- Main sample: students in grades 10 to 12 at Wave I.",
  "- Strict sensitivity sample: students in grades 10 to 12 and ages 15 to 19.",
  "",
  "## Weight",
  "",
  "`GSWGT1` is retained as the Wave I population-average sampling weight.",
  "",
  "## Regional information",
  "",
  "`REGION` was not located in the currently available local files. Therefore, regional modeling is not planned at this stage.",
  "",
  "## Core controls",
  "",
  "The mandatory controls are sex, age and grade when available.",
  "",
  "## Variable blocks",
  "",
  "Predictors are organized into:",
  "",
  "- sociodemographic controls;",
  "- family and household context;",
  "- school and educational context;",
  "- knowledge, attitudes and perceptions;",
  "- peers and relationship context;",
  "- general risk behaviors;",
  "- sexual/reproductive variables requiring outcome-proximity review.",
  "",
  "## Outcomes",
  "",
  "Outcomes are grouped into sexual initiation, condom use, contraceptive use, pregnancy/reproductive experience and HIV/STI or sexual health.",
  "",
  "## Interpretation",
  "",
  "This framework supports associational modeling only. It does not imply causality.",
  "",
  "## Privacy protection",
  "",
  "`AID` is used only internally and is excluded from public outputs.",
  "",
  "## Next step",
  "",
  "Script 10 should estimate weighted logistic regression models using the recommended model sequence."
)

writeLines(
  script09b_doc,
  con = file.path(docs_dir, "data_review_modeling_framework_script09b.md")
)

script09b_checklist$status[
  script09b_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 20. Excel workbook
# ============================================================

script09b_checklist$status[
  script09b_checklist$check_item == "Excel workbook exported"
] <- "OK"

xlsx_path <- file.path(
  outputs_tables_dir,
  "script09b_wave01_data_review_modeling_framework.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "sample_review")
writeData(wb, "sample_review", data_sample_review)

addWorksheet(wb, "data_inventory")
writeData(wb, "data_inventory", data_review_inventory)

addWorksheet(wb, "block_summary")
writeData(wb, "block_summary", theoretical_block_summary)

addWorksheet(wb, "variable_blocks")
writeData(wb, "variable_blocks", modeling_variable_blocks)

addWorksheet(wb, "outcome_plan")
writeData(wb, "outcome_plan", outcome_modeling_plan)

addWorksheet(wb, "core_controls")
writeData(wb, "core_controls", core_control_candidates)

addWorksheet(wb, "predictor_eligibility")
writeData(wb, "predictor_eligibility", predictor_outcome_eligibility)

addWorksheet(wb, "exclusion_review")
writeData(wb, "exclusion_review", predictor_exclusion_review)

addWorksheet(wb, "model_sequence")
writeData(wb, "model_sequence", recommended_model_sequence)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script09b_methodological_notes)

addWorksheet(wb, "checklist")
writeData(wb, "checklist", script09b_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:80, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)


# ============================================================
# 21. Save final checklist
# ============================================================

write_csv(
  script09b_checklist,
  file.path(outputs_diag_dir, "script09b_execution_checklist.csv")
)


# ============================================================
# 22. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 09b completed: Data Review and Modeling Framework\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Input local-only weighted analytical file:\n")
cat(weighted_analytical_rds_path, "\n\n")

cat("Sample review:\n")
print(data_sample_review)

cat("\nVariable inventory:\n")
cat("- Public variables reviewed: ", nrow(data_review_inventory), "\n", sep = "")
cat("- Variables assigned to theoretical blocks: ", sum(!is.na(data_review_inventory$theoretical_block)), "\n", sep = "")
cat("- Variables excluded from public inventory: AID\n\n")

cat("Theoretical block summary:\n")
print(theoretical_block_summary)

cat("\nOutcome modeling plan:\n")
print(
  outcome_modeling_plan %>%
    select(
      variable,
      outcome_family,
      modeling_priority,
      recommended_modeling_status,
      n_nonmissing,
      n_yes,
      n_no,
      weighted_pct_yes
    )
)

cat("\nCore controls review:\n")
print(
  core_control_candidates %>%
    select(
      variable,
      theoretical_block,
      predictor_use_group,
      main_n_nonmissing,
      main_pct_nonmissing,
      core_control_decision
    )
)

cat("\nScript 10 predictor-outcome recommendations:\n")
print(
  predictor_outcome_eligibility %>%
    count(script10_recommendation, name = "n_pairs") %>%
    arrange(script10_recommendation)
)

cat("\nRecommended model sequence preview:\n")
print(
  recommended_model_sequence %>%
    select(
      outcome,
      model_stage,
      model_status,
      mandatory_controls,
      optional_controls_for_review,
      suggested_predictors
    ) %>%
    head(30)
)

cat("\nPublic outputs created:\n")
cat("- outputs/tables/script09b_wave01_data_sample_review.csv\n")
cat("- outputs/tables/script09b_wave01_data_review_inventory.csv\n")
cat("- outputs/tables/script09b_wave01_theoretical_block_summary.csv\n")
cat("- outputs/tables/script09b_wave01_modeling_variable_blocks.csv\n")
cat("- outputs/tables/script09b_wave01_outcome_modeling_plan.csv\n")
cat("- outputs/tables/script09b_wave01_core_controls_review.csv\n")
cat("- outputs/tables/script09b_wave01_predictor_outcome_eligibility.csv\n")
cat("- outputs/tables/script09b_wave01_predictor_exclusion_review.csv\n")
cat("- outputs/tables/script09b_wave01_recommended_model_sequence.csv\n")
cat("- outputs/tables/script09b_wave01_methodological_notes.csv\n")
cat("- outputs/tables/script09b_wave01_data_review_modeling_framework.xlsx\n")
cat("- outputs/diagnostics/script09b_execution_checklist.csv\n")
cat("- docs/data_review_modeling_framework_script09b.md\n\n")

cat("Execution checklist:\n")
print(script09b_checklist)

cat("\nImportant note:\n")
cat("Do not commit data/raw/, data/processed/, AID-level files or individual-level data to GitHub.\n")
cat("Script 10 should be based on this framework, not on an automatic inclusion of all predictors.\n\n")