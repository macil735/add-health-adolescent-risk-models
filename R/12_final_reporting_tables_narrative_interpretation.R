# ============================================================
# Project: add-health-adolescent-risk-models
# Script 12: Final Reporting Tables and Narrative Interpretation
# Author: Gelo Picol
#
# Purpose:
#   Build final reporting tables and cautious narrative
#   interpretation from Script 11 model review outputs.
#
# Important:
#   - This script reads only public aggregate outputs.
#   - It does not read individual-level data.
#   - It does not export microdata.
#   - Results are associational and not causal.
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

script11_final_selection_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_final_model_selection.csv"
)

script11_final_coefficients_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_final_model_coefficients.csv"
)

script11_interpretation_candidates_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_interpretation_candidates.csv"
)

script11_reporting_readiness_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_outcome_reporting_readiness.csv"
)

script11_model_quality_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_model_quality_review.csv"
)

script11_extreme_or_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_extreme_odds_ratio_review.csv"
)

script11_main_strict_model_robustness_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_main_strict_model_robustness.csv"
)

script11_final_selection_summary_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_final_model_selection_summary.csv"
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

format_or <- function(x) {
  x <- safe_numeric(x)

  dplyr::case_when(
    is.na(x) ~ "",
    x >= 1000 ~ formatC(x, format = "e", digits = 2),
    x < 0.001 ~ formatC(x, format = "e", digits = 2),
    TRUE ~ formatC(x, format = "f", digits = 2)
  )
}

format_p <- function(x) {
  x <- safe_numeric(x)

  dplyr::case_when(
    is.na(x) ~ "",
    x < 0.001 ~ "<0.001",
    TRUE ~ formatC(x, format = "f", digits = 3)
  )
}

format_ci <- function(low, high) {
  low_fmt <- format_or(low)
  high_fmt <- format_or(high)

  dplyr::case_when(
    low_fmt == "" | high_fmt == "" ~ "",
    TRUE ~ paste0("[", low_fmt, "; ", high_fmt, "]")
  )
}

clean_label <- function(x) {
  x <- safe_character(x)

  x %>%
    stringr::str_replace_all("^a_", "") %>%
    stringr::str_replace_all("^num_", "") %>%
    stringr::str_replace_all("_yesno$", "") %>%
    stringr::str_replace_all("_wave1$", "") %>%
    stringr::str_replace_all("_", " ")
}

model_stage_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "M0_core_controls" ~ "M0: Core controls",
    x == "M1_family_context" ~ "M1: Family context",
    x == "M2_school_context" ~ "M2: School context",
    x == "M3_knowledge_attitudes" ~ "M3: Knowledge and attitudes",
    x == "M4_peers_relationships" ~ "M4: Peers and relationships",
    x == "M5_general_risk_behaviors" ~ "M5: General risk behaviors",
    x == "M6_final_parsimonious_model" ~ "M6: Final parsimonious model",
    TRUE ~ x
  )
}

reporting_status_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "ready_for_cautious_reporting" ~ "Ready for cautious reporting",
    x == "report_only_after_manual_review" ~ "Report only after manual review",
    x == "not_ready_for_reporting" ~ "Not ready for reporting",
    TRUE ~ x
  )
}

decision_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "preferred_final_parsimonious_model" ~
      "Preferred parsimonious model",
    x == "alternative_stable_final_model" ~
      "Alternative stable model",
    x == "preferred_final_model_with_caution" ~
      "Preferred model with caution",
    x == "alternative_final_model_with_caution" ~
      "Alternative model with caution",
    x == "no_clean_final_model_review_required" ~
      "No clean model; review required",
    x == "no_final_model_selected" ~
      "No final model selected",
    TRUE ~ x
  )
}

coefficient_decision_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "candidate_for_interpretation" ~
      "Candidate for cautious interpretation",
    x == "review_due_to_extreme_or" ~
      "Review: extreme odds ratio",
    x == "review_due_to_wide_ci" ~
      "Review: wide confidence interval",
    x == "review_due_to_possible_conceptual_overlap" ~
      "Review: possible conceptual overlap",
    x == "exclude_or_review_due_to_very_extreme_or" ~
      "Exclude or review: very extreme odds ratio",
    x == "review_due_to_very_wide_ci" ~
      "Review: very wide confidence interval",
    x == "not_statistically_prioritized" ~
      "Not statistically prioritized",
    x == "exclude_intercept" ~
      "Intercept excluded",
    x == "exclude_not_estimable" ~
      "Not estimable",
    TRUE ~ x
  )
}

association_sentence <- function(term, outcome, or, low, high, p, decision) {
  term_label <- clean_label(term)
  outcome_label <- clean_label(outcome)
  or_num <- safe_numeric(or)

  direction <- dplyr::case_when(
    is.na(or_num) ~ "has an unclear association with",
    or_num > 1 ~ "is associated with higher odds of",
    or_num < 1 ~ "is associated with lower odds of",
    TRUE ~ "shows no difference in odds for"
  )

  paste0(
    "In the selected model, ", term_label, " ", direction, " ",
    outcome_label, " (OR = ", format_or(or), ", 95% CI ",
    format_ci(low, high), ", p = ", format_p(p), "). ",
    "Interpretation status: ", coefficient_decision_label(decision), "."
  )
}

outcome_sentence <- function(outcome, model_stage, decision, status, score) {
  paste0(
    "For outcome ", clean_label(outcome), ", the selected reporting model is ",
    model_stage_label(model_stage), ". The model decision is ",
    decision_label(decision), ", with reporting status: ",
    reporting_status_label(status), ". Suitability score: ",
    safe_character(score), "/100."
  )
}


# ============================================================
# 4. Check required inputs
# ============================================================

required_inputs <- c(
  script11_final_selection_path,
  script11_final_coefficients_path,
  script11_interpretation_candidates_path,
  script11_reporting_readiness_path,
  script11_model_quality_path,
  script11_extreme_or_path,
  script11_main_strict_model_robustness_path,
  script11_final_selection_summary_path,
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
# 5. Load inputs
# ============================================================

final_model_selection <- read_csv(
  script11_final_selection_path,
  show_col_types = FALSE
)

final_model_coefficients <- read_csv(
  script11_final_coefficients_path,
  show_col_types = FALSE
)

interpretation_candidates <- read_csv(
  script11_interpretation_candidates_path,
  show_col_types = FALSE
)

outcome_reporting_readiness <- read_csv(
  script11_reporting_readiness_path,
  show_col_types = FALSE
)

model_quality_review <- read_csv(
  script11_model_quality_path,
  show_col_types = FALSE
)

extreme_odds_ratio_review <- read_csv(
  script11_extreme_or_path,
  show_col_types = FALSE
)

main_strict_model_robustness <- read_csv(
  script11_main_strict_model_robustness_path,
  show_col_types = FALSE
)

final_model_selection_summary <- read_csv(
  script11_final_selection_summary_path,
  show_col_types = FALSE
)

outcome_modeling_plan <- read_csv(
  script09b_outcome_modeling_plan_path,
  show_col_types = FALSE
)


# ============================================================
# 6. Final reporting model table
# ============================================================

final_reporting_model_table <- final_model_selection %>%
  mutate(
    n_complete = safe_integer(n_complete),
    n_outcome_yes = safe_integer(n_outcome_yes),
    n_outcome_no = safe_integer(n_outcome_no),
    weighted_pct_yes = safe_numeric(weighted_pct_yes),
    suitability_score = safe_numeric(suitability_score),
    outcome_label = clean_label(outcome),
    selected_model_label = model_stage_label(model_stage),
    final_model_decision_label = decision_label(final_model_decision),
    final_model_reporting_status_label =
      reporting_status_label(final_model_reporting_status),
    reporting_note = case_when(
      final_model_reporting_status == "ready_for_cautious_reporting" ~
        "Model can be reported cautiously as an associational result.",
      final_model_reporting_status == "report_only_after_manual_review" ~
        "Model requires manual review before substantive reporting.",
      TRUE ~
        "Model is not ready for substantive reporting."
    )
  ) %>%
  select(
    sample_name,
    outcome,
    outcome_label,
    model_stage,
    selected_model_label,
    final_model_decision,
    final_model_decision_label,
    final_model_reporting_status,
    final_model_reporting_status_label,
    final_selection_class,
    suitability_score,
    n_complete,
    n_outcome_yes,
    n_outcome_no,
    weighted_pct_yes,
    n_parameters,
    n_significant_05,
    n_extreme_or,
    n_very_extreme_or,
    n_wide_ci,
    n_very_wide_ci,
    n_possible_conceptual_overlap,
    reporting_note
  ) %>%
  arrange(
    sample_name,
    outcome
  )


# ============================================================
# 7. Final reporting coefficient table
# ============================================================

final_reporting_coefficient_table <- final_model_coefficients %>%
  filter(term != "(Intercept)") %>%
  mutate(
    odds_ratio = safe_numeric(odds_ratio),
    conf_low_or = safe_numeric(conf_low_or),
    conf_high_or = safe_numeric(conf_high_or),
    p_value = safe_numeric(p_value),
    outcome_label = clean_label(outcome),
    predictor_label = clean_label(term_variable),
    selected_model_label = model_stage_label(model_stage),
    odds_ratio_formatted = format_or(odds_ratio),
    confidence_interval_formatted = format_ci(conf_low_or, conf_high_or),
    p_value_formatted = format_p(p_value),
    coefficient_decision_label =
      coefficient_decision_label(coefficient_reporting_decision),
    table_inclusion_status = case_when(
      coefficient_reporting_decision == "candidate_for_interpretation" ~
        "include_in_main_reporting_table",
      coefficient_reporting_decision %in% c(
        "review_due_to_extreme_or",
        "review_due_to_wide_ci",
        "review_due_to_possible_conceptual_overlap"
      ) ~
        "include_in_review_or_appendix_table",
      TRUE ~
        "exclude_from_main_reporting_table"
    )
  ) %>%
  select(
    sample_name,
    outcome,
    outcome_label,
    model_stage,
    selected_model_label,
    term,
    term_variable,
    predictor_label,
    odds_ratio,
    conf_low_or,
    conf_high_or,
    p_value,
    odds_ratio_formatted,
    confidence_interval_formatted,
    p_value_formatted,
    significance,
    coefficient_reporting_decision,
    coefficient_decision_label,
    final_model_reporting_status,
    suitability_score,
    table_inclusion_status
  ) %>%
  arrange(
    sample_name,
    outcome,
    model_stage,
    p_value
  )


main_reporting_coefficients <- final_reporting_coefficient_table %>%
  filter(table_inclusion_status == "include_in_main_reporting_table")

appendix_review_coefficients <- final_reporting_coefficient_table %>%
  filter(table_inclusion_status == "include_in_review_or_appendix_table")


# ============================================================
# 8. Narrative summaries by outcome
# ============================================================

coefficient_sentences <- final_reporting_coefficient_table %>%
  filter(
    table_inclusion_status %in% c(
      "include_in_main_reporting_table",
      "include_in_review_or_appendix_table"
    )
  ) %>%
  mutate(
    coefficient_sentence = purrr::pmap_chr(
      list(
        term_variable,
        outcome,
        odds_ratio,
        conf_low_or,
        conf_high_or,
        p_value,
        coefficient_reporting_decision
      ),
      association_sentence
    )
  ) %>%
  group_by(sample_name, outcome) %>%
  summarise(
    n_interpretable_or_review_coefficients = n(),
    coefficient_narrative = paste(coefficient_sentence, collapse = " "),
    .groups = "drop"
  )

outcome_narrative_summary <- final_reporting_model_table %>%
  mutate(
    model_narrative = purrr::pmap_chr(
      list(
        outcome,
        model_stage,
        final_model_decision,
        final_model_reporting_status,
        suitability_score
      ),
      outcome_sentence
    )
  ) %>%
  left_join(
    coefficient_sentences,
    by = c("sample_name", "outcome")
  ) %>%
  mutate(
    n_interpretable_or_review_coefficients =
      tidyr::replace_na(n_interpretable_or_review_coefficients, 0L),
    coefficient_narrative = tidyr::replace_na(
      coefficient_narrative,
      "No coefficient from the selected model was prioritized for substantive interpretation."
    ),
    final_narrative = paste(
      model_narrative,
      coefficient_narrative,
      "All results are associational and should not be interpreted as causal effects."
    ),
    narrative_reporting_status = case_when(
      final_model_reporting_status == "ready_for_cautious_reporting" &
        n_interpretable_or_review_coefficients > 0 ~
        "narrative_ready_for_cautious_reporting",
      final_model_reporting_status == "ready_for_cautious_reporting" ~
        "model_ready_but_no_prioritized_coefficient",
      final_model_reporting_status == "report_only_after_manual_review" ~
        "narrative_requires_manual_review",
      TRUE ~
        "not_ready_for_narrative_reporting"
    )
  ) %>%
  select(
    sample_name,
    outcome,
    outcome_label,
    selected_model_label,
    final_model_decision_label,
    final_model_reporting_status_label,
    suitability_score,
    n_interpretable_or_review_coefficients,
    narrative_reporting_status,
    final_narrative
  ) %>%
  arrange(
    sample_name,
    outcome
  )


# ============================================================
# 9. Robustness reporting table
# ============================================================

robustness_reporting_table <- main_strict_model_robustness %>%
  mutate(
    outcome_label = clean_label(outcome),
    model_stage_label = model_stage_label(model_stage),
    robustness_reporting_note = case_when(
      robustness_summary == "high_directional_robustness" ~
        "High directional consistency between main and strict samples.",
      robustness_summary == "moderate_directional_robustness" ~
        "Moderate directional consistency between main and strict samples.",
      robustness_summary == "low_or_unstable_directional_robustness" ~
        "Low or unstable directional consistency; interpret cautiously.",
      TRUE ~
        "Robustness could not be clearly established."
    )
  ) %>%
  select(
    outcome,
    outcome_label,
    model_stage,
    model_stage_label,
    n_coefficients_compared,
    n_same_direction,
    share_same_direction,
    n_robust_significant_same_direction,
    n_partially_robust_same_direction,
    n_direction_changes_or_unstable,
    robustness_summary,
    robustness_reporting_note
  ) %>%
  arrange(
    outcome,
    model_stage
  )


# ============================================================
# 10. Caution register
# ============================================================

model_cautions <- final_reporting_model_table %>%
  filter(
    final_model_reporting_status != "ready_for_cautious_reporting" |
      n_extreme_or > 0 |
      n_very_extreme_or > 0 |
      n_wide_ci > 0 |
      n_very_wide_ci > 0 |
      n_possible_conceptual_overlap > 0
  ) %>%
  transmute(
    caution_level = case_when(
      final_model_reporting_status != "ready_for_cautious_reporting" ~ "high",
      n_very_extreme_or > 0 | n_very_wide_ci > 0 ~ "high",
      n_extreme_or > 0 | n_wide_ci > 0 ~ "moderate",
      n_possible_conceptual_overlap > 0 ~ "moderate",
      TRUE ~ "low"
    ),
    sample_name,
    outcome,
    outcome_label,
    model_stage,
    caution_type = "model_level_caution",
    caution_detail = paste0(
      "Reporting status: ",
      final_model_reporting_status,
      "; extreme OR: ",
      n_extreme_or,
      "; very extreme OR: ",
      n_very_extreme_or,
      "; wide CI: ",
      n_wide_ci,
      "; very wide CI: ",
      n_very_wide_ci,
      "; conceptual overlap flags: ",
      n_possible_conceptual_overlap,
      "."
    )
  )

coefficient_cautions <- appendix_review_coefficients %>%
  transmute(
    caution_level = case_when(
      coefficient_reporting_decision %in% c(
        "exclude_or_review_due_to_very_extreme_or",
        "review_due_to_very_wide_ci"
      ) ~ "high",
      TRUE ~ "moderate"
    ),
    sample_name,
    outcome,
    outcome_label,
    model_stage,
    caution_type = "coefficient_level_caution",
    caution_detail = paste0(
      "Predictor: ",
      predictor_label,
      "; OR = ",
      odds_ratio_formatted,
      "; CI = ",
      confidence_interval_formatted,
      "; p = ",
      p_value_formatted,
      "; decision: ",
      coefficient_decision_label,
      "."
    )
  )

reporting_caution_register <- bind_rows(
  model_cautions,
  coefficient_cautions
) %>%
  arrange(
    sample_name,
    outcome,
    caution_level,
    caution_type
  )


# ============================================================
# 11. Publication readiness summary
# ============================================================

publication_readiness_summary <- tibble(
  item = c(
    "Final reporting models created",
    "Main coefficient table created",
    "Appendix review coefficient table created",
    "Outcome narrative summary created",
    "Main-strict robustness table created",
    "Caution register created",
    "Number of outcomes ready for cautious reporting",
    "Number of outcomes requiring manual review",
    "Number of main reporting coefficients",
    "Number of appendix/review coefficients",
    "Microdata used by Script 12",
    "Causal interpretation permitted"
  ),
  value = c(
    as.character(nrow(final_reporting_model_table)),
    as.character(nrow(main_reporting_coefficients)),
    as.character(nrow(appendix_review_coefficients)),
    as.character(nrow(outcome_narrative_summary)),
    as.character(nrow(robustness_reporting_table)),
    as.character(nrow(reporting_caution_register)),
    as.character(sum(
      final_reporting_model_table$final_model_reporting_status ==
        "ready_for_cautious_reporting",
      na.rm = TRUE
    )),
    as.character(sum(
      final_reporting_model_table$final_model_reporting_status ==
        "report_only_after_manual_review",
      na.rm = TRUE
    )),
    as.character(nrow(main_reporting_coefficients)),
    as.character(nrow(appendix_review_coefficients)),
    "No",
    "No"
  )
)


# ============================================================
# 12. Methodological notes
# ============================================================

script12_methodological_notes <- tibble(
  note_id = 1:18,
  note = c(
    "Script 12 creates final reporting tables and cautious narrative interpretation from Script 11 outputs.",
    "The script reads only aggregate public outputs and does not read individual-level Add Health data.",
    "The selected models are based on Script 11 technical review, not on manual substantive judgment alone.",
    "The final reporting model table identifies selected models, reporting status and main technical caution flags.",
    "The coefficient reporting table separates main reportable coefficients from coefficients requiring review or appendix placement.",
    "Odds ratios are reported with Wald confidence intervals from Script 10 model outputs.",
    "Narrative interpretation is deliberately cautious and associational.",
    "The script does not interpret results as causal effects.",
    "Models requiring manual review are not treated as final substantive conclusions.",
    "M0 core-control models are interpreted as conservative baseline models.",
    "Richer models are reported only when technically defensible.",
    "Extreme odds ratios and wide confidence intervals are flagged before reporting.",
    "Possible conceptual overlap between predictors and outcomes is treated as a reporting caution.",
    "Main-versus-strict sample robustness is summarized separately.",
    "The caution register should be reviewed before writing the final report.",
    "No AID-level or individual-level output is exported.",
    "The outputs are suitable for report drafting, not for automatic publication without review.",
    "Script 13 should create the final technical report or manuscript-style document."
  )
)


# ============================================================
# 13. Export CSV outputs
# ============================================================

write_csv(
  final_reporting_model_table,
  file.path(outputs_tables_dir, "script12_wave01_final_reporting_model_table.csv")
)

write_csv(
  final_reporting_coefficient_table,
  file.path(outputs_tables_dir, "script12_wave01_final_reporting_coefficient_table.csv")
)

write_csv(
  main_reporting_coefficients,
  file.path(outputs_tables_dir, "script12_wave01_main_reporting_coefficients.csv")
)

write_csv(
  appendix_review_coefficients,
  file.path(outputs_tables_dir, "script12_wave01_appendix_review_coefficients.csv")
)

write_csv(
  outcome_narrative_summary,
  file.path(outputs_tables_dir, "script12_wave01_outcome_narrative_summary.csv")
)

write_csv(
  robustness_reporting_table,
  file.path(outputs_tables_dir, "script12_wave01_main_strict_robustness_reporting.csv")
)

write_csv(
  reporting_caution_register,
  file.path(outputs_tables_dir, "script12_wave01_reporting_caution_register.csv")
)

write_csv(
  publication_readiness_summary,
  file.path(outputs_tables_dir, "script12_wave01_publication_readiness_summary.csv")
)

write_csv(
  script12_methodological_notes,
  file.path(outputs_tables_dir, "script12_wave01_methodological_notes.csv")
)


# ============================================================
# 14. Markdown narrative report
# ============================================================

main_sample_narratives <- outcome_narrative_summary %>%
  filter(sample_name == "main_grade_10_12") %>%
  mutate(
    md_line = paste0(
      "### ", outcome_label, "\n\n",
      final_narrative, "\n"
    )
  ) %>%
  pull(md_line)

strict_sample_narratives <- outcome_narrative_summary %>%
  filter(sample_name == "strict_grade_10_12_age_15_19") %>%
  mutate(
    md_line = paste0(
      "### ", outcome_label, "\n\n",
      final_narrative, "\n"
    )
  ) %>%
  pull(md_line)

script12_doc <- c(
  "# Final Reporting Tables and Narrative Interpretation",
  "",
  "Script 12 converts the model review outputs from Script 11 into reporting-ready tables and cautious narrative summaries.",
  "",
  "## Scope",
  "",
  "The script uses only aggregate public outputs. It does not read or export individual-level data.",
  "",
  "## Interpretation rule",
  "",
  "All estimates are associational. They should not be interpreted as causal effects.",
  "",
  "## Main sample narratives",
  "",
  main_sample_narratives,
  "",
  "## Strict sensitivity sample narratives",
  "",
  strict_sample_narratives,
  "",
  "## Reporting caution",
  "",
  "Models or coefficients flagged for extreme odds ratios, wide confidence intervals, or possible conceptual overlap require manual review before substantive reporting.",
  "",
  "## Next step",
  "",
  "Script 13 should build the final report or manuscript-style document."
)

writeLines(
  script12_doc,
  con = file.path(
    docs_dir,
    "final_reporting_tables_narrative_interpretation_script12.md"
  )
)


# ============================================================
# 15. Execution checklist
# ============================================================

script12_checklist <- tibble(
  check_id = 1:24,
  check_item = c(
    "Project root exists",
    "Outputs tables directory exists",
    "Outputs diagnostics directory exists",
    "Docs directory exists",
    "Script 11 final model selection input exists",
    "Script 11 final coefficients input exists",
    "Script 11 interpretation candidates input exists",
    "Script 11 reporting readiness input exists",
    "Script 11 model quality input exists",
    "Script 11 extreme odds-ratio input exists",
    "Script 11 main-strict robustness input exists",
    "Script 11 final selection summary input exists",
    "Script 09b outcome modeling plan input exists",
    "Final reporting model table created",
    "Final reporting coefficient table created",
    "Main reporting coefficients table created",
    "Appendix review coefficients table created",
    "Outcome narrative summary created",
    "Robustness reporting table created",
    "Caution register created",
    "Publication readiness summary created",
    "Markdown narrative report exported",
    "Excel workbook exported",
    "No individual-level output exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(dir.exists(outputs_tables_dir), "OK", "FAIL"),
    ifelse(dir.exists(outputs_diag_dir), "OK", "FAIL"),
    ifelse(dir.exists(docs_dir), "OK", "FAIL"),
    ifelse(file.exists(script11_final_selection_path), "OK", "FAIL"),
    ifelse(file.exists(script11_final_coefficients_path), "OK", "FAIL"),
    ifelse(file.exists(script11_interpretation_candidates_path), "OK", "FAIL"),
    ifelse(file.exists(script11_reporting_readiness_path), "OK", "FAIL"),
    ifelse(file.exists(script11_model_quality_path), "OK", "FAIL"),
    ifelse(file.exists(script11_extreme_or_path), "OK", "FAIL"),
    ifelse(file.exists(script11_main_strict_model_robustness_path), "OK", "FAIL"),
    ifelse(file.exists(script11_final_selection_summary_path), "OK", "FAIL"),
    ifelse(file.exists(script09b_outcome_modeling_plan_path), "OK", "FAIL"),
    ifelse(nrow(final_reporting_model_table) > 0, "OK", "FAIL"),
    ifelse(nrow(final_reporting_coefficient_table) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(main_reporting_coefficients) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(appendix_review_coefficients) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(outcome_narrative_summary) > 0, "OK", "FAIL"),
    ifelse(nrow(robustness_reporting_table) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(reporting_caution_register) > 0, "OK", "WARNING_NONE"),
    ifelse(nrow(publication_readiness_summary) > 0, "OK", "FAIL"),
    ifelse(
      file.exists(
        file.path(
          docs_dir,
          "final_reporting_tables_narrative_interpretation_script12.md"
        )
      ),
      "OK",
      "FAIL"
    ),
    "PENDING",
    "OK"
  )
)


# ============================================================
# 16. Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_final_reporting_tables_narrative.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "final_models")
writeData(wb, "final_models", final_reporting_model_table)

addWorksheet(wb, "all_coefficients")
writeData(wb, "all_coefficients", final_reporting_coefficient_table)

addWorksheet(wb, "main_coefficients")
writeData(wb, "main_coefficients", main_reporting_coefficients)

addWorksheet(wb, "appendix_review")
writeData(wb, "appendix_review", appendix_review_coefficients)

addWorksheet(wb, "narrative_summary")
writeData(wb, "narrative_summary", outcome_narrative_summary)

addWorksheet(wb, "robustness")
writeData(wb, "robustness", robustness_reporting_table)

addWorksheet(wb, "caution_register")
writeData(wb, "caution_register", reporting_caution_register)

addWorksheet(wb, "publication_readiness")
writeData(wb, "publication_readiness", publication_readiness_summary)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script12_methodological_notes)

script12_checklist$status[
  script12_checklist$check_item == "Excel workbook exported"
] <- "OK"

addWorksheet(wb, "checklist")
writeData(wb, "checklist", script12_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:100, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)


# ============================================================
# 17. Save final checklist
# ============================================================

write_csv(
  script12_checklist,
  file.path(outputs_diag_dir, "script12_execution_checklist.csv")
)


# ============================================================
# 18. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 12 completed: Final Reporting Tables and Narrative Interpretation\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Reporting model table summary:\n")
print(
  final_reporting_model_table %>%
    count(
      sample_name,
      final_model_reporting_status,
      name = "n_models"
    ) %>%
    arrange(sample_name, final_model_reporting_status)
)

cat("\nPublication readiness summary:\n")
print(publication_readiness_summary)

cat("\nMain reporting coefficients preview:\n")
print(
  main_reporting_coefficients %>%
    select(
      sample_name,
      outcome,
      selected_model_label,
      predictor_label,
      odds_ratio_formatted,
      confidence_interval_formatted,
      p_value_formatted,
      coefficient_decision_label
    ) %>%
    head(50)
)

cat("\nAppendix or review coefficients preview:\n")
print(
  appendix_review_coefficients %>%
    select(
      sample_name,
      outcome,
      selected_model_label,
      predictor_label,
      odds_ratio_formatted,
      confidence_interval_formatted,
      p_value_formatted,
      coefficient_decision_label
    ) %>%
    head(50)
)

cat("\nOutcome narrative summary preview:\n")
print(
  outcome_narrative_summary %>%
    select(
      sample_name,
      outcome,
      selected_model_label,
      final_model_reporting_status_label,
      narrative_reporting_status
    )
)

cat("\nCaution register summary:\n")
print(
  reporting_caution_register %>%
    count(
      sample_name,
      caution_level,
      caution_type,
      name = "n_cautions"
    ) %>%
    arrange(sample_name, caution_level, caution_type)
)

cat("\nPublic outputs created:\n")
cat("- outputs/tables/script12_wave01_final_reporting_model_table.csv\n")
cat("- outputs/tables/script12_wave01_final_reporting_coefficient_table.csv\n")
cat("- outputs/tables/script12_wave01_main_reporting_coefficients.csv\n")
cat("- outputs/tables/script12_wave01_appendix_review_coefficients.csv\n")
cat("- outputs/tables/script12_wave01_outcome_narrative_summary.csv\n")
cat("- outputs/tables/script12_wave01_main_strict_robustness_reporting.csv\n")
cat("- outputs/tables/script12_wave01_reporting_caution_register.csv\n")
cat("- outputs/tables/script12_wave01_publication_readiness_summary.csv\n")
cat("- outputs/tables/script12_wave01_methodological_notes.csv\n")
cat("- outputs/tables/script12_wave01_final_reporting_tables_narrative.xlsx\n")
cat("- outputs/diagnostics/script12_execution_checklist.csv\n")
cat("- docs/final_reporting_tables_narrative_interpretation_script12.md\n\n")

cat("Execution checklist:\n")
print(script12_checklist)

cat("\nImportant note:\n")
cat("Script 12 prepares reporting-ready tables and cautious narrative summaries.\n")
cat("Substantive conclusions still require final manual review.\n")
cat("No individual-level data were used or exported by this script.\n\n")