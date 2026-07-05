# ============================================================
# Project: add-health-adolescent-risk-models
# Script 11: Model Results Review and Final Model Selection
# Author: Gelo Picol
#
# Purpose:
#   Review Script 10 weighted logistic regression results,
#   flag unstable estimates, compare main and strict samples,
#   and select final reportable models.
#
# Important:
#   - This script reads only public aggregate outputs from Script 10.
#   - It does not read individual-level data.
#   - It does not export microdata.
#   - Results remain associational, not causal.
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
  "openxlsx"
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


# ============================================================
# 2. Paths
# ============================================================

outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir   <- file.path(project_root, "outputs/diagnostics")
docs_dir           <- file.path(project_root, "docs")

dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

script10_model_specifications_path <- file.path(
  outputs_tables_dir,
  "script10_wave01_model_specifications.csv"
)

script10_model_fit_summary_path <- file.path(
  outputs_tables_dir,
  "script10_wave01_model_fit_summary.csv"
)

script10_model_coefficients_path <- file.path(
  outputs_tables_dir,
  "script10_wave01_model_coefficients_odds_ratios.csv"
)

script10_odds_ratio_review_path <- file.path(
  outputs_tables_dir,
  "script10_wave01_odds_ratio_review.csv"
)

script10_skipped_failed_path <- file.path(
  outputs_tables_dir,
  "script10_wave01_skipped_or_failed_models.csv"
)

script10_outcome_summary_path <- file.path(
  outputs_tables_dir,
  "script10_wave01_outcome_model_summary.csv"
)

script09b_outcome_modeling_plan_path <- file.path(
  outputs_tables_dir,
  "script09b_wave01_outcome_modeling_plan.csv"
)


# ============================================================
# 3. Helper functions
# ============================================================

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_integer <- function(x) {
  suppressWarnings(as.integer(round(as.numeric(x))))
}

safe_character <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

clean_ratio <- function(high, low) {
  high <- safe_numeric(high)
  low <- safe_numeric(low)

  dplyr::case_when(
    !is.na(high) & !is.na(low) & low > 0 ~ high / low,
    TRUE ~ NA_real_
  )
}

stage_preference_rank <- function(stage) {
  dplyr::case_when(
    stage == "M6_final_parsimonious_model" ~ 1L,
    stage == "M0_core_controls" ~ 2L,
    stage == "M1_family_context" ~ 3L,
    stage == "M2_school_context" ~ 4L,
    stage == "M3_knowledge_attitudes" ~ 5L,
    stage == "M4_peers_relationships" ~ 6L,
    stage == "M5_general_risk_behaviors" ~ 7L,
    TRUE ~ 99L
  )
}

classify_outcome_domain <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    stringr::str_detect(x, "H1CO16|H1HS") ~ "hiv_sti_or_sexual_health",
    stringr::str_detect(x, "H1FP") ~ "pregnancy_or_reproductive_experience",
    stringr::str_detect(x, "H1CO") ~ "condom_or_contraceptive_use",
    stringr::str_detect(x, "sex_ever") ~ "sexual_initiation",
    TRUE ~ "other"
  )
}

classify_term_domain <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    stringr::str_detect(x, "H1CO16|H1HS") ~ "hiv_sti_or_sexual_health_related",
    stringr::str_detect(x, "H1FP") ~ "pregnancy_or_reproductive_related",
    stringr::str_detect(x, "H1CO") ~ "condom_or_contraceptive_related",
    stringr::str_detect(x, "sex_ever|H1SE|H1SX") ~ "sexual_behavior_related",
    stringr::str_detect(x, "age|grade|female|sex") ~ "core_sociodemographic",
    TRUE ~ "other"
  )
}

flag_possible_conceptual_overlap <- function(outcome_domain, term_domain) {
  dplyr::case_when(
    outcome_domain == "sexual_initiation" &
      term_domain %in% c(
        "sexual_behavior_related",
        "condom_or_contraceptive_related",
        "pregnancy_or_reproductive_related",
        "hiv_sti_or_sexual_health_related"
      ) ~ "possible_post_outcome_or_same_domain_predictor_review",

    outcome_domain == "condom_or_contraceptive_use" &
      term_domain %in% c(
        "condom_or_contraceptive_related",
        "pregnancy_or_reproductive_related",
        "hiv_sti_or_sexual_health_related"
      ) ~ "possible_same_domain_or_downstream_predictor_review",

    outcome_domain == "pregnancy_or_reproductive_experience" &
      term_domain %in% c(
        "pregnancy_or_reproductive_related",
        "condom_or_contraceptive_related",
        "sexual_behavior_related"
      ) ~ "possible_same_domain_or_upstream_behavior_predictor_review",

    outcome_domain == "hiv_sti_or_sexual_health" &
      term_domain %in% c(
        "hiv_sti_or_sexual_health_related",
        "condom_or_contraceptive_related",
        "sexual_behavior_related"
      ) ~ "possible_same_domain_or_behavioral_predictor_review",

    TRUE ~ "no_automatic_overlap_flag"
  )
}

classify_p_value <- function(p_value) {
  p_value <- safe_numeric(p_value)

  dplyr::case_when(
    is.na(p_value) ~ "p_missing",
    p_value < 0.001 ~ "p_lt_0_001",
    p_value < 0.01 ~ "p_lt_0_01",
    p_value < 0.05 ~ "p_lt_0_05",
    p_value < 0.10 ~ "p_lt_0_10",
    TRUE ~ "not_statistically_significant"
  )
}

classify_or_direction <- function(or) {
  or <- safe_numeric(or)

  dplyr::case_when(
    is.na(or) ~ "or_missing",
    or > 1 ~ "higher_odds",
    or < 1 ~ "lower_odds",
    or == 1 ~ "no_difference",
    TRUE ~ "or_missing"
  )
}

safe_mean <- function(x) {
  x <- safe_numeric(x)

  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}


# ============================================================
# 4. Check required inputs
# ============================================================

required_inputs <- c(
  script10_model_specifications_path,
  script10_model_fit_summary_path,
  script10_model_coefficients_path,
  script10_odds_ratio_review_path,
  script10_skipped_failed_path,
  script10_outcome_summary_path,
  script09b_outcome_modeling_plan_path
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
# 5. Load Script 10 and Script 09b outputs
# ============================================================

model_specifications <- read_csv(
  script10_model_specifications_path,
  show_col_types = FALSE
)

model_fit_summary <- read_csv(
  script10_model_fit_summary_path,
  show_col_types = FALSE
)

model_coefficients <- read_csv(
  script10_model_coefficients_path,
  show_col_types = FALSE
)

odds_ratio_review_script10 <- read_csv(
  script10_odds_ratio_review_path,
  show_col_types = FALSE
)

skipped_or_failed_script10 <- read_csv(
  script10_skipped_failed_path,
  show_col_types = FALSE
)

outcome_summary_script10 <- read_csv(
  script10_outcome_summary_path,
  show_col_types = FALSE
)

outcome_modeling_plan <- read_csv(
  script09b_outcome_modeling_plan_path,
  show_col_types = FALSE
)


# ============================================================
# 6. Coefficient-level review
# ============================================================

coefficient_review <- model_coefficients %>%
  mutate(
    estimate_log_odds = safe_numeric(estimate_log_odds),
    std_error = safe_numeric(std_error),
    statistic = safe_numeric(statistic),
    p_value = safe_numeric(p_value),
    odds_ratio = safe_numeric(odds_ratio),
    conf_low_or = safe_numeric(conf_low_or),
    conf_high_or = safe_numeric(conf_high_or),
    term = safe_character(term),
    term_variable = safe_character(term_variable),
    coefficient_status = safe_character(coefficient_status),
    outcome_domain = classify_outcome_domain(outcome),
    term_domain = classify_term_domain(term_variable),
    ci_width_ratio = clean_ratio(conf_high_or, conf_low_or),
    absolute_log_or = dplyr::case_when(
      !is.na(odds_ratio) & odds_ratio > 0 ~ abs(log(odds_ratio)),
      TRUE ~ NA_real_
    ),
    p_value_band = classify_p_value(p_value),
    or_direction = classify_or_direction(odds_ratio),
    extreme_or_flag = case_when(
      coefficient_status != "estimated" ~ "not_estimable",
      is.na(odds_ratio) ~ "or_missing",
      odds_ratio >= 100 | odds_ratio <= 0.01 ~ "very_extreme_or_review",
      odds_ratio >= 10 | odds_ratio <= 0.10 ~ "very_extreme_or_review",
      odds_ratio >= 5 | odds_ratio <= 0.20 ~ "extreme_or_review",
      TRUE ~ "not_extreme"
    ),
    ci_width_flag = case_when(
      is.na(ci_width_ratio) ~ "ci_width_missing",
      ci_width_ratio >= 100 ~ "very_wide_ci_review",
      ci_width_ratio >= 20 ~ "wide_ci_review",
      TRUE ~ "ci_width_not_extreme"
    ),
    conceptual_overlap_flag = flag_possible_conceptual_overlap(
      outcome_domain,
      term_domain
    ),
    coefficient_reporting_decision = case_when(
      term == "(Intercept)" ~ "exclude_intercept",
      coefficient_status != "estimated" ~ "exclude_not_estimable",
      extreme_or_flag == "very_extreme_or_review" ~
        "exclude_or_review_due_to_very_extreme_or",
      ci_width_flag == "very_wide_ci_review" ~
        "review_due_to_very_wide_ci",
      conceptual_overlap_flag != "no_automatic_overlap_flag" &
        p_value < 0.05 ~
        "review_due_to_possible_conceptual_overlap",
      extreme_or_flag == "extreme_or_review" ~
        "review_due_to_extreme_or",
      ci_width_flag == "wide_ci_review" ~
        "review_due_to_wide_ci",
      !is.na(p_value) & p_value < 0.05 ~
        "candidate_for_interpretation",
      TRUE ~ "not_statistically_prioritized"
    )
  ) %>%
  arrange(
    sample_name,
    outcome,
    model_stage,
    p_value
  )


# ============================================================
# 7. Model-level quality review
# ============================================================

coefficient_model_flags <- coefficient_review %>%
  filter(term != "(Intercept)") %>%
  group_by(
    model_id,
    sample_name,
    outcome,
    model_stage
  ) %>%
  summarise(
    n_coefficients_reviewed = n(),
    n_significant_05 = sum(!is.na(p_value) & p_value < 0.05, na.rm = TRUE),
    n_significant_10 = sum(!is.na(p_value) & p_value < 0.10, na.rm = TRUE),
    n_extreme_or = sum(extreme_or_flag == "extreme_or_review", na.rm = TRUE),
    n_very_extreme_or = sum(extreme_or_flag == "very_extreme_or_review", na.rm = TRUE),
    n_or_missing = sum(extreme_or_flag == "or_missing", na.rm = TRUE),
    n_not_estimable_terms = sum(extreme_or_flag == "not_estimable", na.rm = TRUE),
    n_wide_ci = sum(ci_width_flag == "wide_ci_review", na.rm = TRUE),
    n_very_wide_ci = sum(ci_width_flag == "very_wide_ci_review", na.rm = TRUE),
    n_possible_conceptual_overlap = sum(
      conceptual_overlap_flag != "no_automatic_overlap_flag",
      na.rm = TRUE
    ),
    n_candidate_for_interpretation = sum(
      coefficient_reporting_decision == "candidate_for_interpretation",
      na.rm = TRUE
    ),
    n_coefficients_requiring_review = sum(
      coefficient_reporting_decision %in% c(
        "exclude_or_review_due_to_very_extreme_or",
        "review_due_to_very_wide_ci",
        "review_due_to_possible_conceptual_overlap",
        "review_due_to_extreme_or",
        "review_due_to_wide_ci"
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

model_quality_review <- model_fit_summary %>%
  mutate(
    n_complete = safe_integer(n_complete),
    n_outcome_yes = safe_integer(n_outcome_yes),
    n_outcome_no = safe_integer(n_outcome_no),
    weighted_total = safe_numeric(weighted_total),
    weighted_outcome_yes = safe_numeric(weighted_outcome_yes),
    weighted_pct_yes = safe_numeric(weighted_pct_yes),
    n_parameters = safe_integer(n_parameters),
    residual_df = safe_numeric(residual_df),
    null_deviance = safe_numeric(null_deviance),
    residual_deviance = safe_numeric(residual_deviance),
    dispersion = safe_numeric(dispersion),
    model_stage_rank = stage_preference_rank(model_stage),
    min_outcome_cell = case_when(
      !is.na(n_outcome_yes) & !is.na(n_outcome_no) ~
        pmin(n_outcome_yes, n_outcome_no),
      TRUE ~ NA_integer_
    )
  ) %>%
  left_join(
    coefficient_model_flags,
    by = c("model_id", "sample_name", "outcome", "model_stage")
  ) %>%
  mutate(
    across(
      c(
        n_coefficients_reviewed,
        n_significant_05,
        n_significant_10,
        n_extreme_or,
        n_very_extreme_or,
        n_or_missing,
        n_not_estimable_terms,
        n_wide_ci,
        n_very_wide_ci,
        n_possible_conceptual_overlap,
        n_candidate_for_interpretation,
        n_coefficients_requiring_review
      ),
      ~ tidyr::replace_na(.x, 0)
    ),
    model_sample_flag = case_when(
      fit_status != "fitted" ~ "not_fitted",
      is.na(n_complete) ~ "n_complete_missing",
      n_complete < 100 ~ "insufficient_n",
      n_complete < 300 ~ "small_model_sample_review",
      TRUE ~ "adequate_model_sample"
    ),
    outcome_cell_flag = case_when(
      fit_status != "fitted" ~ "not_fitted",
      is.na(min_outcome_cell) ~ "outcome_cell_missing",
      min_outcome_cell < 10 ~ "insufficient_outcome_cell",
      min_outcome_cell < 30 ~ "small_outcome_cell_review",
      TRUE ~ "adequate_outcome_cell"
    ),
    parameter_pressure_flag = case_when(
      fit_status != "fitted" ~ "not_fitted",
      is.na(n_parameters) | is.na(n_complete) ~ "parameter_pressure_missing",
      n_parameters == 0 ~ "no_parameters",
      n_complete / n_parameters < 10 ~ "high_parameter_pressure_review",
      n_complete / n_parameters < 20 ~ "moderate_parameter_pressure_review",
      TRUE ~ "parameter_pressure_acceptable"
    ),
    instability_flag = case_when(
      fit_status != "fitted" ~ "not_fitted",
      n_not_estimable_terms > 0 ~ "not_estimable_terms_review",
      n_very_extreme_or > 0 ~ "very_extreme_or_review",
      n_very_wide_ci > 0 ~ "very_wide_ci_review",
      n_extreme_or > 0 ~ "extreme_or_review",
      n_wide_ci > 0 ~ "wide_ci_review",
      TRUE ~ "no_major_numerical_instability_flag"
    ),
    conceptual_review_flag = case_when(
      fit_status != "fitted" ~ "not_fitted",
      n_possible_conceptual_overlap > 0 ~ "possible_conceptual_overlap_review",
      TRUE ~ "no_automatic_conceptual_overlap_flag"
    ),
    final_selection_class = case_when(
      fit_status != "fitted" ~ "not_selectable_not_fitted",
      model_sample_flag %in% c("insufficient_n") ~
        "not_selectable_insufficient_sample",
      outcome_cell_flag %in% c("insufficient_outcome_cell") ~
        "not_selectable_insufficient_outcome_cell",
      n_not_estimable_terms > 0 | n_very_extreme_or > 0 | n_very_wide_ci > 0 ~
        "review_before_reporting",
      n_extreme_or > 0 |
        n_wide_ci > 0 |
        n_possible_conceptual_overlap > 0 |
        model_sample_flag == "small_model_sample_review" |
        outcome_cell_flag == "small_outcome_cell_review" |
        parameter_pressure_flag != "parameter_pressure_acceptable" ~
        "usable_with_caution",
      TRUE ~ "technically_stable_candidate"
    ),
    score_penalty_not_fitted = case_when(
      fit_status != "fitted" ~ 100,
      TRUE ~ 0
    ),
    score_penalty_small_sample = case_when(
      model_sample_flag == "small_model_sample_review" ~ 10,
      model_sample_flag == "insufficient_n" ~ 50,
      TRUE ~ 0
    ),
    score_penalty_small_cell = case_when(
      outcome_cell_flag == "small_outcome_cell_review" ~ 15,
      outcome_cell_flag == "insufficient_outcome_cell" ~ 50,
      TRUE ~ 0
    ),
    score_penalty_extreme_or = 20 * n_extreme_or + 35 * n_very_extreme_or,
    score_penalty_wide_ci = 10 * n_wide_ci + 25 * n_very_wide_ci,
    score_penalty_not_estimable = 25 * n_not_estimable_terms,
    score_penalty_overlap = case_when(
      n_possible_conceptual_overlap > 0 ~ 10,
      TRUE ~ 0
    ),
    score_penalty_parameter_pressure = case_when(
      parameter_pressure_flag == "moderate_parameter_pressure_review" ~ 5,
      parameter_pressure_flag == "high_parameter_pressure_review" ~ 15,
      TRUE ~ 0
    ),
    suitability_score_raw = 100 -
      score_penalty_not_fitted -
      score_penalty_small_sample -
      score_penalty_small_cell -
      score_penalty_extreme_or -
      score_penalty_wide_ci -
      score_penalty_not_estimable -
      score_penalty_overlap -
      score_penalty_parameter_pressure,
    suitability_score = pmax(0, pmin(100, suitability_score_raw)),
    selection_priority = case_when(
      final_selection_class == "technically_stable_candidate" ~ 1L,
      final_selection_class == "usable_with_caution" ~ 2L,
      final_selection_class == "review_before_reporting" ~ 3L,
      TRUE ~ 9L
    )
  ) %>%
  select(
    -suitability_score_raw,
    -starts_with("score_penalty_")
  ) %>%
  arrange(
    sample_name,
    outcome,
    model_stage_rank
  )


# ============================================================
# 8. Final model selection
# ============================================================

final_model_selection <- model_quality_review %>%
  group_by(sample_name, outcome) %>%
  arrange(
    selection_priority,
    model_stage_rank,
    desc(suitability_score),
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    final_model_decision = case_when(
      fit_status != "fitted" ~ "no_final_model_selected",
      final_selection_class == "technically_stable_candidate" &
        model_stage == "M6_final_parsimonious_model" ~
        "preferred_final_parsimonious_model",
      final_selection_class == "technically_stable_candidate" ~
        "alternative_stable_final_model",
      final_selection_class == "usable_with_caution" &
        model_stage == "M6_final_parsimonious_model" ~
        "preferred_final_model_with_caution",
      final_selection_class == "usable_with_caution" ~
        "alternative_final_model_with_caution",
      final_selection_class == "review_before_reporting" ~
        "no_clean_final_model_review_required",
      TRUE ~ "no_final_model_selected"
    ),
    final_model_reporting_status = case_when(
      final_model_decision %in% c(
        "preferred_final_parsimonious_model",
        "alternative_stable_final_model"
      ) ~ "ready_for_cautious_reporting",
      final_model_decision %in% c(
        "preferred_final_model_with_caution",
        "alternative_final_model_with_caution"
      ) ~ "report_only_after_manual_review",
      TRUE ~ "not_ready_for_reporting"
    )
  ) %>%
  arrange(
    sample_name,
    outcome
  )

conservative_reference_models <- model_quality_review %>%
  filter(
    model_stage == "M0_core_controls",
    fit_status == "fitted"
  ) %>%
  select(
    sample_name,
    outcome,
    conservative_model_id = model_id,
    conservative_model_stage = model_stage,
    conservative_suitability_score = suitability_score,
    conservative_selection_class = final_selection_class
  )

final_model_selection <- final_model_selection %>%
  left_join(
    conservative_reference_models,
    by = c("sample_name", "outcome")
  )


# ============================================================
# 9. Final selected coefficients
# ============================================================

selected_model_ids <- final_model_selection %>%
  filter(final_model_reporting_status != "not_ready_for_reporting") %>%
  pull(model_id) %>%
  unique()

final_model_coefficients <- coefficient_review %>%
  filter(model_id %in% selected_model_ids) %>%
  left_join(
    final_model_selection %>%
      select(
        model_id,
        final_model_decision,
        final_model_reporting_status,
        suitability_score,
        final_selection_class
      ),
    by = "model_id"
  ) %>%
  arrange(
    sample_name,
    outcome,
    model_stage,
    p_value
  )

interpretation_candidates <- final_model_coefficients %>%
  filter(
    term != "(Intercept)",
    coefficient_reporting_decision %in% c(
      "candidate_for_interpretation",
      "review_due_to_extreme_or",
      "review_due_to_wide_ci",
      "review_due_to_possible_conceptual_overlap"
    )
  ) %>%
  mutate(
    interpretation_direction = case_when(
      odds_ratio > 1 ~ "associated_with_higher_odds",
      odds_ratio < 1 ~ "associated_with_lower_odds",
      TRUE ~ "no_direction"
    ),
    interpretation_strength_flag = case_when(
      odds_ratio >= 5 | odds_ratio <= 0.20 ~ "large_association_review",
      odds_ratio >= 2 | odds_ratio <= 0.50 ~ "moderate_association",
      TRUE ~ "small_or_modest_association"
    ),
    reporting_caution = case_when(
      coefficient_reporting_decision == "candidate_for_interpretation" ~
        "can_be_interpreted_cautiously",
      TRUE ~ "manual_review_required_before_interpretation"
    )
  ) %>%
  arrange(
    sample_name,
    outcome,
    model_stage,
    p_value
  )


# ============================================================
# 10. Main versus strict sample robustness
# ============================================================

main_coefficients <- coefficient_review %>%
  filter(
    sample_name == "main_grade_10_12",
    term != "(Intercept)",
    coefficient_status == "estimated"
  ) %>%
  select(
    outcome,
    model_stage,
    term,
    term_variable,
    main_odds_ratio = odds_ratio,
    main_p_value = p_value,
    main_conf_low_or = conf_low_or,
    main_conf_high_or = conf_high_or
  )

strict_coefficients <- coefficient_review %>%
  filter(
    sample_name == "strict_grade_10_12_age_15_19",
    term != "(Intercept)",
    coefficient_status == "estimated"
  ) %>%
  select(
    outcome,
    model_stage,
    term,
    term_variable,
    strict_odds_ratio = odds_ratio,
    strict_p_value = p_value,
    strict_conf_low_or = conf_low_or,
    strict_conf_high_or = conf_high_or
  )

main_strict_coefficient_robustness <- main_coefficients %>%
  inner_join(
    strict_coefficients,
    by = c("outcome", "model_stage", "term", "term_variable")
  ) %>%
  mutate(
    main_direction = classify_or_direction(main_odds_ratio),
    strict_direction = classify_or_direction(strict_odds_ratio),
    same_direction = main_direction == strict_direction &
      main_direction %in% c("higher_odds", "lower_odds"),
    main_significant_05 = !is.na(main_p_value) & main_p_value < 0.05,
    strict_significant_05 = !is.na(strict_p_value) & strict_p_value < 0.05,
    both_significant_05 = main_significant_05 & strict_significant_05,
    either_significant_05 = main_significant_05 | strict_significant_05,
    absolute_log_or_difference = case_when(
      !is.na(main_odds_ratio) &
        !is.na(strict_odds_ratio) &
        main_odds_ratio > 0 &
        strict_odds_ratio > 0 ~
        abs(log(main_odds_ratio) - log(strict_odds_ratio)),
      TRUE ~ NA_real_
    ),
    robustness_status = case_when(
      both_significant_05 &
        same_direction &
        !is.na(absolute_log_or_difference) &
        absolute_log_or_difference <= log(2) ~
        "robust_significant_same_direction",
      either_significant_05 &
        same_direction ~
        "partially_robust_same_direction",
      same_direction ~
        "same_direction_not_statistically_prioritized",
      TRUE ~ "direction_changes_or_unstable_review"
    )
  ) %>%
  arrange(
    outcome,
    model_stage,
    term_variable
  )

main_strict_model_robustness <- main_strict_coefficient_robustness %>%
  group_by(outcome, model_stage) %>%
  summarise(
    n_coefficients_compared = n(),
    n_same_direction = sum(same_direction, na.rm = TRUE),
    share_same_direction = round(100 * safe_mean(same_direction), 2),
    n_robust_significant_same_direction = sum(
      robustness_status == "robust_significant_same_direction",
      na.rm = TRUE
    ),
    n_partially_robust_same_direction = sum(
      robustness_status == "partially_robust_same_direction",
      na.rm = TRUE
    ),
    n_direction_changes_or_unstable = sum(
      robustness_status == "direction_changes_or_unstable_review",
      na.rm = TRUE
    ),
    robustness_summary = case_when(
      n_coefficients_compared == 0 ~ "no_coefficients_compared",
      share_same_direction >= 80 &
        n_direction_changes_or_unstable == 0 ~
        "high_directional_robustness",
      share_same_direction >= 60 ~
        "moderate_directional_robustness",
      TRUE ~ "low_or_unstable_directional_robustness"
    ),
    .groups = "drop"
  ) %>%
  arrange(
    outcome,
    model_stage
  )


# ============================================================
# 11. Outcome reporting readiness
# ============================================================

outcome_reporting_readiness <- final_model_selection %>%
  left_join(
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
      ),
    by = c("outcome" = "variable")
  ) %>%
  left_join(
    main_strict_model_robustness,
    by = c("outcome", "model_stage")
  ) %>%
  mutate(
    outcome_reporting_readiness = case_when(
      final_model_reporting_status == "not_ready_for_reporting" ~
        "not_ready_for_reporting",
      final_model_reporting_status == "report_only_after_manual_review" ~
        "manual_review_required_before_reporting",
      robustness_summary %in% c(
        "high_directional_robustness",
        "moderate_directional_robustness"
      ) ~
        "ready_for_cautious_reporting_with_sensitivity_support",
      sample_name == "strict_grade_10_12_age_15_19" ~
        "sensitivity_sample_result_review",
      TRUE ~
        "ready_for_cautious_reporting_pending_narrative_review"
    )
  ) %>%
  arrange(
    sample_name,
    outcome
  )


# ============================================================
# 12. Models and coefficients requiring review
# ============================================================

extreme_odds_ratio_review <- coefficient_review %>%
  filter(
    term != "(Intercept)",
    extreme_or_flag != "not_extreme" |
      ci_width_flag %in% c("wide_ci_review", "very_wide_ci_review") |
      conceptual_overlap_flag != "no_automatic_overlap_flag"
  ) %>%
  arrange(
    sample_name,
    outcome,
    model_stage,
    p_value
  )

models_excluded_from_reporting <- model_quality_review %>%
  filter(
    final_selection_class %in% c(
      "not_selectable_not_fitted",
      "not_selectable_insufficient_sample",
      "not_selectable_insufficient_outcome_cell",
      "review_before_reporting"
    ) |
      fit_status != "fitted"
  ) %>%
  arrange(
    sample_name,
    outcome,
    model_stage_rank
  )

model_stage_review_summary <- model_quality_review %>%
  group_by(sample_name, model_stage) %>%
  summarise(
    n_models = n(),
    n_fitted = sum(fit_status == "fitted", na.rm = TRUE),
    n_not_fitted = sum(fit_status != "fitted", na.rm = TRUE),
    n_stable_candidates = sum(
      final_selection_class == "technically_stable_candidate",
      na.rm = TRUE
    ),
    n_usable_with_caution = sum(
      final_selection_class == "usable_with_caution",
      na.rm = TRUE
    ),
    n_review_before_reporting = sum(
      final_selection_class == "review_before_reporting",
      na.rm = TRUE
    ),
    mean_suitability_score = round(mean(suitability_score, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(
    sample_name,
    model_stage
  )

final_model_selection_summary <- final_model_selection %>%
  count(
    sample_name,
    final_model_decision,
    final_model_reporting_status,
    name = "n_outcomes"
  ) %>%
  arrange(
    sample_name,
    final_model_decision
  )


# ============================================================
# 13. Methodological notes
# ============================================================

script11_methodological_notes <- tibble(
  note_id = 1:20,
  note = c(
    "Script 11 reviews Script 10 weighted logistic regression outputs.",
    "This script uses only public aggregate outputs and does not read individual-level data.",
    "The review focuses on model stability, sample adequacy, outcome-cell size, odds-ratio magnitude, confidence-interval width and conceptual overlap.",
    "Models are classified as technically stable candidates, usable with caution, review-before-reporting, or not selectable.",
    "Odds ratios equal to or above 5, or equal to or below 0.20, are flagged for review.",
    "Odds ratios equal to or above 10, or equal to or below 0.10, are treated as very extreme and require stronger review.",
    "Confidence-interval width ratios equal to or above 20 are flagged as wide.",
    "Confidence-interval width ratios equal to or above 100 are treated as very wide.",
    "Predictors from the same outcome domain are flagged for conceptual review where automatic detection is possible.",
    "The final parsimonious model is preferred when it is technically defensible.",
    "If the final parsimonious model is unstable, the script selects the best available stable or cautious alternative.",
    "The core-control model is retained as a conservative reference model.",
    "The main sample is students in grades 10 to 12.",
    "The strict sensitivity sample is students in grades 10 to 12 and ages 15 to 19.",
    "Main-versus-strict robustness is assessed by coefficient direction and significance concordance.",
    "Selected coefficients are candidates for interpretation only after substantive review.",
    "Results are associational and should not be interpreted as causal effects.",
    "Rare outcomes and small complete-case samples require careful reporting.",
    "No individual-level file, AID-level output, or microdata is exported.",
    "Script 12 should produce final reporting tables and narrative interpretation."
  )
)


# ============================================================
# 14. Export public CSV outputs
# ============================================================

write_csv(
  coefficient_review,
  file.path(outputs_tables_dir, "script11_wave01_coefficient_review.csv")
)

write_csv(
  model_quality_review,
  file.path(outputs_tables_dir, "script11_wave01_model_quality_review.csv")
)

write_csv(
  final_model_selection,
  file.path(outputs_tables_dir, "script11_wave01_final_model_selection.csv")
)

write_csv(
  final_model_coefficients,
  file.path(outputs_tables_dir, "script11_wave01_final_model_coefficients.csv")
)

write_csv(
  interpretation_candidates,
  file.path(outputs_tables_dir, "script11_wave01_interpretation_candidates.csv")
)

write_csv(
  main_strict_coefficient_robustness,
  file.path(outputs_tables_dir, "script11_wave01_main_strict_coefficient_robustness.csv")
)

write_csv(
  main_strict_model_robustness,
  file.path(outputs_tables_dir, "script11_wave01_main_strict_model_robustness.csv")
)

write_csv(
  outcome_reporting_readiness,
  file.path(outputs_tables_dir, "script11_wave01_outcome_reporting_readiness.csv")
)

write_csv(
  extreme_odds_ratio_review,
  file.path(outputs_tables_dir, "script11_wave01_extreme_odds_ratio_review.csv")
)

write_csv(
  models_excluded_from_reporting,
  file.path(outputs_tables_dir, "script11_wave01_models_excluded_from_reporting.csv")
)

write_csv(
  model_stage_review_summary,
  file.path(outputs_tables_dir, "script11_wave01_model_stage_review_summary.csv")
)

write_csv(
  final_model_selection_summary,
  file.path(outputs_tables_dir, "script11_wave01_final_model_selection_summary.csv")
)

write_csv(
  script11_methodological_notes,
  file.path(outputs_tables_dir, "script11_wave01_methodological_notes.csv")
)


# ============================================================
# 15. Markdown documentation
# ============================================================

script11_doc <- c(
  "# Model Results Review and Final Model Selection",
  "",
  "Script 11 reviews the weighted logistic regression outputs produced by Script 10.",
  "",
  "## Purpose",
  "",
  "The script identifies stable, unstable and review-required models before any substantive interpretation is written.",
  "",
  "## Inputs",
  "",
  "- Script 10 model specifications.",
  "- Script 10 model fit summary.",
  "- Script 10 coefficient and odds-ratio outputs.",
  "- Script 10 skipped or failed model diagnostics.",
  "- Script 09b outcome modeling plan.",
  "",
  "## Review criteria",
  "",
  "- Model fit status.",
  "- Complete-case sample size.",
  "- Outcome-cell size.",
  "- Number of estimable coefficients.",
  "- Extreme odds ratios.",
  "- Wide confidence intervals.",
  "- Possible conceptual overlap between predictor and outcome.",
  "- Robustness between main and strict samples.",
  "",
  "## Model selection logic",
  "",
  "The final parsimonious model is preferred when it is stable. If it is not stable, the script selects the best available alternative model based on technical suitability.",
  "",
  "## Reporting rule",
  "",
  "Selected models are not automatically treated as final substantive conclusions. They are candidates for careful interpretation.",
  "",
  "## Privacy protection",
  "",
  "The script reads only aggregate outputs and exports no individual-level data.",
  "",
  "## Next step",
  "",
  "Script 12 should build final reporting tables and narrative interpretation."
)

writeLines(
  script11_doc,
  con = file.path(docs_dir, "model_results_review_final_selection_script11.md")
)


# ============================================================
# 16. Execution checklist
# ============================================================

script11_checklist <- tibble(
  check_id = 1:24,
  check_item = c(
    "Project root exists",
    "Outputs tables directory exists",
    "Outputs diagnostics directory exists",
    "Docs directory exists",
    "Script 10 model specifications input exists",
    "Script 10 model fit summary input exists",
    "Script 10 coefficient input exists",
    "Script 10 odds-ratio review input exists",
    "Script 10 skipped or failed models input exists",
    "Script 10 outcome summary input exists",
    "Script 09b outcome modeling plan input exists",
    "Model specifications loaded",
    "Model fit summary loaded",
    "Model coefficients loaded",
    "Coefficient-level review created",
    "Model-level quality review created",
    "Final model selection created",
    "Final model coefficients created",
    "Main-strict robustness review created",
    "Outcome reporting readiness created",
    "Extreme odds-ratio review created",
    "Markdown documentation exported",
    "Excel workbook exported",
    "No individual-level output exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(dir.exists(outputs_tables_dir), "OK", "FAIL"),
    ifelse(dir.exists(outputs_diag_dir), "OK", "FAIL"),
    ifelse(dir.exists(docs_dir), "OK", "FAIL"),
    ifelse(file.exists(script10_model_specifications_path), "OK", "FAIL"),
    ifelse(file.exists(script10_model_fit_summary_path), "OK", "FAIL"),
    ifelse(file.exists(script10_model_coefficients_path), "OK", "FAIL"),
    ifelse(file.exists(script10_odds_ratio_review_path), "OK", "FAIL"),
    ifelse(file.exists(script10_skipped_failed_path), "OK", "FAIL"),
    ifelse(file.exists(script10_outcome_summary_path), "OK", "FAIL"),
    ifelse(file.exists(script09b_outcome_modeling_plan_path), "OK", "FAIL"),
    ifelse(nrow(model_specifications) > 0, "OK", "FAIL"),
    ifelse(nrow(model_fit_summary) > 0, "OK", "FAIL"),
    ifelse(nrow(model_coefficients) > 0, "OK", "FAIL"),
    ifelse(nrow(coefficient_review) > 0, "OK", "FAIL"),
    ifelse(nrow(model_quality_review) > 0, "OK", "FAIL"),
    ifelse(nrow(final_model_selection) > 0, "OK", "FAIL"),
    ifelse(nrow(final_model_coefficients) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(main_strict_model_robustness) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(outcome_reporting_readiness) > 0, "OK", "FAIL"),
    ifelse(nrow(extreme_odds_ratio_review) > 0, "OK", "WARNING_NONE"),
    ifelse(file.exists(file.path(docs_dir, "model_results_review_final_selection_script11.md")), "OK", "FAIL"),
    "PENDING",
    "OK"
  )
)


# ============================================================
# 17. Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_model_results_review_final_selection.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "coefficient_review")
writeData(wb, "coefficient_review", coefficient_review)

addWorksheet(wb, "model_quality")
writeData(wb, "model_quality", model_quality_review)

addWorksheet(wb, "final_selection")
writeData(wb, "final_selection", final_model_selection)

addWorksheet(wb, "final_coefficients")
writeData(wb, "final_coefficients", final_model_coefficients)

addWorksheet(wb, "interpretation_candidates")
writeData(wb, "interpretation_candidates", interpretation_candidates)

addWorksheet(wb, "coef_robustness")
writeData(wb, "coef_robustness", main_strict_coefficient_robustness)

addWorksheet(wb, "model_robustness")
writeData(wb, "model_robustness", main_strict_model_robustness)

addWorksheet(wb, "reporting_readiness")
writeData(wb, "reporting_readiness", outcome_reporting_readiness)

addWorksheet(wb, "extreme_or_review")
writeData(wb, "extreme_or_review", extreme_odds_ratio_review)

addWorksheet(wb, "excluded_models")
writeData(wb, "excluded_models", models_excluded_from_reporting)

addWorksheet(wb, "stage_summary")
writeData(wb, "stage_summary", model_stage_review_summary)

addWorksheet(wb, "selection_summary")
writeData(wb, "selection_summary", final_model_selection_summary)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script11_methodological_notes)

script11_checklist$status[
  script11_checklist$check_item == "Excel workbook exported"
] <- "OK"

addWorksheet(wb, "checklist")
writeData(wb, "checklist", script11_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:100, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)


# ============================================================
# 18. Save final checklist
# ============================================================

write_csv(
  script11_checklist,
  file.path(outputs_diag_dir, "script11_execution_checklist.csv")
)


# ============================================================
# 19. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 11 completed: Model Results Review and Final Selection\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Input files reviewed:\n")
cat("- Script 10 model fit summary\n")
cat("- Script 10 coefficient and odds-ratio outputs\n")
cat("- Script 10 skipped or failed model diagnostics\n")
cat("- Script 09b outcome modeling plan\n\n")

cat("Model quality summary:\n")
print(
  model_quality_review %>%
    count(
      sample_name,
      final_selection_class,
      name = "n_models"
    ) %>%
    arrange(sample_name, final_selection_class)
)

cat("\nFinal model selection summary:\n")
print(final_model_selection_summary)

cat("\nOutcome reporting readiness:\n")
print(
  outcome_reporting_readiness %>%
    select(
      sample_name,
      outcome,
      model_stage,
      final_model_decision,
      final_model_reporting_status,
      outcome_reporting_readiness,
      suitability_score
    )
)

cat("\nModels requiring review before reporting:\n")
print(
  model_quality_review %>%
    filter(final_selection_class == "review_before_reporting") %>%
    select(
      sample_name,
      outcome,
      model_stage,
      instability_flag,
      n_very_extreme_or,
      n_very_wide_ci,
      n_not_estimable_terms,
      suitability_score
    ) %>%
    head(50)
)

cat("\nExtreme odds-ratio or wide-CI coefficients preview:\n")
print(
  extreme_odds_ratio_review %>%
    select(
      sample_name,
      outcome,
      model_stage,
      term,
      odds_ratio,
      conf_low_or,
      conf_high_or,
      p_value,
      extreme_or_flag,
      ci_width_flag,
      coefficient_reporting_decision
    ) %>%
    head(50)
)

cat("\nMain-strict robustness summary:\n")
print(main_strict_model_robustness)

cat("\nPublic outputs created:\n")
cat("- outputs/tables/script11_wave01_coefficient_review.csv\n")
cat("- outputs/tables/script11_wave01_model_quality_review.csv\n")
cat("- outputs/tables/script11_wave01_final_model_selection.csv\n")
cat("- outputs/tables/script11_wave01_final_model_coefficients.csv\n")
cat("- outputs/tables/script11_wave01_interpretation_candidates.csv\n")
cat("- outputs/tables/script11_wave01_main_strict_coefficient_robustness.csv\n")
cat("- outputs/tables/script11_wave01_main_strict_model_robustness.csv\n")
cat("- outputs/tables/script11_wave01_outcome_reporting_readiness.csv\n")
cat("- outputs/tables/script11_wave01_extreme_odds_ratio_review.csv\n")
cat("- outputs/tables/script11_wave01_models_excluded_from_reporting.csv\n")
cat("- outputs/tables/script11_wave01_model_stage_review_summary.csv\n")
cat("- outputs/tables/script11_wave01_final_model_selection_summary.csv\n")
cat("- outputs/tables/script11_wave01_methodological_notes.csv\n")
cat("- outputs/tables/script11_wave01_model_results_review_final_selection.xlsx\n")
cat("- outputs/diagnostics/script11_execution_checklist.csv\n")
cat("- docs/model_results_review_final_selection_script11.md\n\n")

cat("Execution checklist:\n")
print(script11_checklist)

cat("\nImportant note:\n")
cat("Script 11 selects candidate final models, but substantive interpretation must still be reviewed manually.\n")
cat("No individual-level data were used or exported by this script.\n\n")