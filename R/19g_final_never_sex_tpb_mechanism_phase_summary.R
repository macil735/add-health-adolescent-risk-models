# ============================================================
# Script 19g
# Final Never-Sex TPB Mechanism Interpretation and Phase Summary
# Project: Add Health Adolescent Risk Models
# ============================================================
#
# Purpose:
#   Consolidate Scripts 19a_v3, 19a_v3_check, 19d, 19e and 19f
#   into a formal phase summary report.
#
# Main conclusion:
#   The phase supports exploratory TPB-compatible psychosocial
#   mechanism models among adolescents aged 15-19 who had not yet
#   had sexual intercourse.
#
#   It does not support a full TPB mediation model because:
#     1. no direct intention-to-delay outcome was confirmed;
#     2. perceived behavioral control / self-efficacy was not
#        operationally confirmed;
#     3. using a delay-orientation proxy as outcome would create
#        item overlap and circularity.
#
# Outputs:
#   - formal Word report
#   - markdown methodological note
#   - final results table
#   - final weighted main effects table
#   - phase output inventory
#   - commit readiness checklist
#
# No Git action is performed.
#
# ============================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  warn = 1
)

script_id <- "19g"
script_title <- "Final Never-Sex TPB Mechanism Interpretation and Phase Summary"
start_time <- Sys.time()

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

core_pkgs <- c(
  "dplyr",
  "readr",
  "stringr",
  "purrr",
  "tibble",
  "tidyr"
)

missing_core <- core_pkgs[
  !vapply(core_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_core) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_core, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(tidyr)
})

has_docx <- all(vapply(
  c("officer", "flextable"),
  requireNamespace,
  logical(1),
  quietly = TRUE
))

# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

if (!stringr::str_detect(basename(project_root), "add-health-adolescent-risk-models")) {
  warning(
    "Current working directory does not look like the project root: ",
    project_root
  )
}

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/audits", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/analysis", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reports", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)

# Inputs from 19a_v3 and validation
input_19a_v3_sample_audit <- "outputs/audits/script19a_v3_never_sex_sample_audit.csv"
input_19a_v3_construct_summary <- "outputs/tables/script19a_v3_construct_summary.csv"
input_19a_v3_item_summary <- "outputs/audits/script19a_v3_item_summary.csv"
input_19a_v3_check_validation <- "outputs/audits/script19a_v3_check_validation_checks.csv"
input_19a_v3_check_internal <- "outputs/tables/script19a_v3_check_internal_consistency.csv"
input_19a_v3_check_correlations <- "outputs/tables/script19a_v3_check_construct_correlations.csv"

# Inputs from 19d
input_19d_pilot_summary <- "outputs/tables/script19d_tpb_pilot_construct_summary.csv"
input_19d_internal <- "outputs/tables/script19d_tpb_pilot_internal_consistency.csv"
input_19d_correlations <- "outputs/tables/script19d_tpb_pilot_construct_correlations.csv"

# Inputs from 19e
input_19e_readiness <- "outputs/audits/script19e_mediation_readiness_assessment.csv"
input_19e_decision_matrix <- "outputs/audits/script19e_tpb_operational_decision_matrix.csv"
input_19e_final_summary <- "outputs/tables/script19e_final_tpb_construct_summary.csv"
input_19e_recommended_models <- "outputs/tables/script19e_recommended_model_sequence.csv"

# Inputs from 19f
input_19f_covariate_audit <- "outputs/audits/script19f_covariate_detection_audit.csv"
input_19f_sample_audit <- "outputs/audits/script19f_model_sample_audit.csv"
input_19f_model_specs <- "outputs/audits/script19f_model_specifications.csv"
input_19f_coefs <- "outputs/tables/script19f_regression_coefficients.csv"
input_19f_fit <- "outputs/tables/script19f_model_fit_statistics.csv"
input_19f_std_coefs <- "outputs/tables/script19f_standardized_regression_coefficients.csv"

# Outputs
final_results_path <- "outputs/tables/script19g_final_never_sex_tpb_mechanism_key_results.csv"
weighted_main_effects_path <- "outputs/tables/script19g_weighted_adjusted_main_effects.csv"
model_fit_summary_path <- "outputs/tables/script19g_final_model_fit_summary.csv"
phase_inventory_path <- "outputs/tables/script19g_phase_output_inventory.csv"
commit_checklist_path <- "outputs/audits/script19g_commit_readiness_checklist.csv"
phase_decision_path <- "outputs/audits/script19g_final_phase_decision.csv"

markdown_report_path <- "outputs/reports/script19g_final_never_sex_tpb_mechanism_phase_summary.md"
docx_report_path <- "outputs/reports/script19g_final_never_sex_tpb_mechanism_phase_summary.docx"
log_path <- "outputs/logs/script19g_run_log.txt"

cat("", file = log_path)

log_line <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(txt, "\n", file = log_path, append = TRUE)
  message(txt)
}

log_line("Started ", script_id, ": ", script_title)
log_line("Project root: ", project_root)

# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------

read_csv_if_exists <- function(path, required = FALSE) {
  if (!file.exists(path)) {
    if (required) {
      stop("Required input not found: ", path, call. = FALSE)
    }
    
    warning("Optional input not found: ", path)
    return(tibble())
  }
  
  readr::read_csv(path, show_col_types = FALSE)
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

model_label <- function(model_id) {
  dplyr::case_when(
    stringr::str_detect(model_id, "^M1_") ~ "M1: delay-supportive attitudes",
    stringr::str_detect(model_id, "^M2_") ~ "M2: peer norm",
    stringr::str_detect(model_id, "^M3_") ~ "M3: partner norm",
    stringr::str_detect(model_id, "^M4_") ~ "M4: maternal norm",
    TRUE ~ model_id
  )
}

variant_label <- function(model_variant) {
  dplyr::case_when(
    model_variant == "A_unadjusted" ~ "Unadjusted",
    model_variant == "B_interpersonal" ~ "Interpersonal",
    model_variant == "C_adjusted" ~ "Adjusted",
    model_variant == "D_adjusted_weighted" ~ "Adjusted weighted",
    TRUE ~ model_variant
  )
}

term_label <- function(term) {
  dplyr::case_when(
    term == "family_connectedness_mean_1_5" ~ "Family connectedness",
    term == "friend_support_mean_1_5" ~ "Friend support",
    term == "(Intercept)" ~ "Intercept",
    TRUE ~ term
  )
}

interpret_effect <- function(estimate, p_value, term, model_id) {
  sig <- !is.na(p_value) && p_value < 0.05
  marginal <- !is.na(p_value) && p_value >= 0.05 && p_value < 0.10
  
  direction <- dplyr::case_when(
    is.na(estimate) ~ "not estimated",
    estimate > 0 ~ "positive",
    estimate < 0 ~ "negative",
    TRUE ~ "zero"
  )
  
  if (sig) {
    paste0("Statistically significant ", direction, " association.")
  } else if (marginal) {
    paste0("Marginal ", direction, " association; interpret cautiously.")
  } else {
    "No statistically significant association."
  }
}

safe_select <- function(data, cols) {
  data |>
    dplyr::select(dplyr::any_of(cols))
}

# ------------------------------------------------------------
# 4. Load inputs
# ------------------------------------------------------------

sample_19a <- read_csv_if_exists(input_19a_v3_sample_audit, required = TRUE)
construct_summary_19a <- read_csv_if_exists(input_19a_v3_construct_summary, required = TRUE)
item_summary_19a <- read_csv_if_exists(input_19a_v3_item_summary, required = FALSE)
validation_19a <- read_csv_if_exists(input_19a_v3_check_validation, required = TRUE)
internal_19a <- read_csv_if_exists(input_19a_v3_check_internal, required = TRUE)
correlations_19a <- read_csv_if_exists(input_19a_v3_check_correlations, required = FALSE)

pilot_summary_19d <- read_csv_if_exists(input_19d_pilot_summary, required = TRUE)
internal_19d <- read_csv_if_exists(input_19d_internal, required = TRUE)
correlations_19d <- read_csv_if_exists(input_19d_correlations, required = FALSE)

readiness_19e <- read_csv_if_exists(input_19e_readiness, required = TRUE)
decision_19e <- read_csv_if_exists(input_19e_decision_matrix, required = TRUE)
final_summary_19e <- read_csv_if_exists(input_19e_final_summary, required = TRUE)
recommended_19e <- read_csv_if_exists(input_19e_recommended_models, required = TRUE)

covariate_19f <- read_csv_if_exists(input_19f_covariate_audit, required = TRUE)
sample_19f <- read_csv_if_exists(input_19f_sample_audit, required = TRUE)
specs_19f <- read_csv_if_exists(input_19f_model_specs, required = TRUE)
coefs_19f <- read_csv_if_exists(input_19f_coefs, required = TRUE)
fit_19f <- read_csv_if_exists(input_19f_fit, required = TRUE)
std_coefs_19f <- read_csv_if_exists(input_19f_std_coefs, required = TRUE)

log_line("All required phase inputs loaded.")

# ------------------------------------------------------------
# 5. Extract key phase facts
# ------------------------------------------------------------

final_n <- sample_19a |>
  dplyr::pull(final_never_sex_age_15_19_n)

if (length(final_n) == 0 || is.na(final_n[1])) {
  final_n <- NA_integer_
} else {
  final_n <- final_n[1]
}

validation_pass_n <- validation_19a |>
  dplyr::filter(status == "PASS") |>
  nrow()

validation_total_n <- nrow(validation_19a)

full_mediation_ready <- readiness_19e |>
  dplyr::filter(criterion == "Full TPB mediation ready") |>
  dplyr::pull(assessment)

if (length(full_mediation_ready) == 0) {
  full_mediation_ready <- "UNKNOWN"
}

exploratory_ready <- readiness_19e |>
  dplyr::filter(criterion == "Exploratory TPB-compatible mechanism ready") |>
  dplyr::pull(assessment)

if (length(exploratory_ready) == 0) {
  exploratory_ready <- "UNKNOWN"
}

weight_source <- covariate_19f |>
  dplyr::filter(covariate == "survey_weight") |>
  dplyr::pull(detected_variable)

if (length(weight_source) == 0) {
  weight_source <- NA_character_
}

weight_valid_n <- covariate_19f |>
  dplyr::filter(covariate == "survey_weight") |>
  dplyr::pull(n_valid)

if (length(weight_valid_n) == 0) {
  weight_valid_n <- NA_integer_
}

# ------------------------------------------------------------
# 6. Final weighted adjusted main effects
# ------------------------------------------------------------

main_predictors <- c(
  "family_connectedness_mean_1_5",
  "friend_support_mean_1_5"
)

weighted_main_effects <- coefs_19f |>
  dplyr::filter(
    model_variant == "D_adjusted_weighted",
    term %in% main_predictors
  ) |>
  dplyr::mutate(
    model_label = model_label(model_id),
    predictor_label = term_label(term),
    interpretation = purrr::pmap_chr(
      list(estimate, p_value, term, model_id),
      interpret_effect
    ),
    estimate_display = fmt_num(estimate, 3),
    ci_display = paste0("[", fmt_num(conf_low, 3), ", ", fmt_num(conf_high, 3), "]"),
    p_display = fmt_p(p_value)
  ) |>
  dplyr::select(
    model_id,
    model_label,
    dependent_variable,
    predictor_label,
    estimate,
    std_error,
    conf_low,
    conf_high,
    p_value,
    p_display,
    interpretation
  ) |>
  dplyr::arrange(model_id, predictor_label)

readr::write_csv(weighted_main_effects, weighted_main_effects_path)

# ------------------------------------------------------------
# 7. Final key results table
# ------------------------------------------------------------

std_weighted <- std_coefs_19f |>
  dplyr::filter(
    model_variant == "D_adjusted_weighted",
    term %in% main_predictors
  ) |>
  dplyr::select(
    model_id,
    term,
    standardized_estimate = estimate,
    standardized_se = std_error,
    standardized_p_value = p_value
  )

final_key_results <- weighted_main_effects |>
  dplyr::left_join(
    std_weighted,
    by = c("model_id", "predictor_label" = "term")
  )

# The join above uses predictor_label values, not original term values.
# Rebuild with explicit term mapping to avoid mismatch.

final_key_results <- coefs_19f |>
  dplyr::filter(
    model_variant == "D_adjusted_weighted",
    term %in% main_predictors
  ) |>
  dplyr::select(
    model_id,
    dependent_variable,
    term,
    estimate,
    std_error,
    conf_low,
    conf_high,
    p_value
  ) |>
  dplyr::left_join(
    std_coefs_19f |>
      dplyr::filter(
        model_variant == "D_adjusted_weighted",
        term %in% main_predictors
      ) |>
      dplyr::select(
        model_id,
        term,
        standardized_estimate = estimate,
        standardized_std_error = std_error,
        standardized_p_value = p_value
      ),
    by = c("model_id", "term")
  ) |>
  dplyr::mutate(
    model_label = model_label(model_id),
    predictor_label = term_label(term),
    coefficient = fmt_num(estimate, 3),
    confidence_interval = paste0("[", fmt_num(conf_low, 3), ", ", fmt_num(conf_high, 3), "]"),
    p_value_display = fmt_p(p_value),
    standardized_beta = fmt_num(standardized_estimate, 3),
    substantive_reading = purrr::pmap_chr(
      list(estimate, p_value, term, model_id),
      interpret_effect
    )
  ) |>
  dplyr::select(
    model_id,
    model_label,
    dependent_variable,
    predictor = predictor_label,
    coefficient,
    confidence_interval,
    p_value_display,
    standardized_beta,
    substantive_reading
  ) |>
  dplyr::arrange(model_id, predictor)

readr::write_csv(final_key_results, final_results_path)

# ------------------------------------------------------------
# 8. Final model fit summary
# ------------------------------------------------------------

model_fit_summary <- fit_19f |>
  dplyr::mutate(
    model_label = model_label(model_id),
    model_variant_label = variant_label(
      stringr::str_remove(
        model_id,
        "^[A-Z0-9]+_"
      )
    )
  ) |>
  dplyr::select(
    model_id,
    model_label,
    dependent_variable,
    weighted,
    n_used,
    r_squared,
    adj_r_squared,
    residual_se,
    f_statistic,
    f_p_value_display
  ) |>
  dplyr::arrange(model_id)

readr::write_csv(model_fit_summary, model_fit_summary_path)

# ------------------------------------------------------------
# 9. Final phase decision
# ------------------------------------------------------------

phase_decision <- tibble::tibble(
  decision_area = c(
    "analytic_sample",
    "construct_validation",
    "tpb_mapping",
    "mediation_readiness",
    "regression_phase",
    "reporting_decision",
    "git_decision"
  ),
  decision = c(
    paste0("Use never-sex adolescents aged 15-19; final n = ", final_n, "."),
    paste0(validation_pass_n, " of ", validation_total_n, " validation checks passed for corrected constructs."),
    "Use TPB-compatible attitudes and separate normative items; do not use combined subjective norms as a homogeneous scale.",
    "Do not estimate full TPB mediation because direct intention and confirmed perceived behavioral control were not available.",
    "Estimate exploratory TPB-compatible mechanism regressions with adjusted and weighted variants.",
    "Produce formal phase report, final key results table, and methodological caution note.",
    "Commit only after Script 19g report and outputs are reviewed."
  ),
  status = c(
    "approved",
    "approved",
    "approved_with_caution",
    "full_mediation_blocked",
    "completed",
    "completed",
    "pending"
  )
)

readr::write_csv(phase_decision, phase_decision_path)

# ------------------------------------------------------------
# 10. Phase output inventory
# ------------------------------------------------------------

phase_files <- c(
  input_19a_v3_sample_audit,
  input_19a_v3_construct_summary,
  input_19a_v3_check_validation,
  input_19a_v3_check_internal,
  input_19d_pilot_summary,
  input_19d_internal,
  input_19e_readiness,
  input_19e_decision_matrix,
  input_19e_final_summary,
  input_19e_recommended_models,
  input_19f_covariate_audit,
  input_19f_sample_audit,
  input_19f_model_specs,
  input_19f_coefs,
  input_19f_fit,
  input_19f_std_coefs,
  final_results_path,
  weighted_main_effects_path,
  model_fit_summary_path,
  phase_decision_path,
  markdown_report_path,
  docx_report_path,
  log_path
)

phase_inventory <- tibble::tibble(
  phase_component = c(
    "19a_v3 sample audit",
    "19a_v3 construct summary",
    "19a_v3 validation checks",
    "19a_v3 internal consistency",
    "19d TPB pilot summary",
    "19d TPB pilot internal consistency",
    "19e mediation readiness",
    "19e decision matrix",
    "19e final construct summary",
    "19e recommended models",
    "19f covariate audit",
    "19f model sample audit",
    "19f model specifications",
    "19f regression coefficients",
    "19f model fit",
    "19f standardized coefficients",
    "19g final key results",
    "19g weighted main effects",
    "19g model fit summary",
    "19g phase decision",
    "19g markdown report",
    "19g Word report",
    "19g run log"
  ),
  path = phase_files,
  exists = file.exists(phase_files)
)

readr::write_csv(phase_inventory, phase_inventory_path)

# ------------------------------------------------------------
# 11. Commit readiness checklist
# ------------------------------------------------------------

commit_checklist <- tibble::tibble(
  check_id = c(
    "sample_defined",
    "constructs_validated",
    "tpb_mapping_completed",
    "mediation_block_documented",
    "regression_models_completed",
    "survey_weight_recovered",
    "formal_report_created",
    "tables_created",
    "git_commit_ready"
  ),
  check_description = c(
    "Never-sex analytic sample aged 15-19 is defined.",
    "Family, friend and deterrence constructs were validated.",
    "TPB-compatible constructs were mapped and operationalized.",
    "Full TPB mediation was explicitly blocked and justified.",
    "Exploratory TPB-compatible regression models were estimated.",
    "GSWGT1 was recovered from 21600-0004-Data.rda.",
    "Formal Word report was produced.",
    "Final results, model fit and decision tables were produced.",
    "Ready for Git commit after manual review."
  ),
  status = c(
    ifelse(!is.na(final_n) && final_n > 0, "PASS", "FAIL"),
    ifelse(validation_pass_n == validation_total_n && validation_total_n > 0, "PASS", "CHECK"),
    ifelse(nrow(decision_19e) > 0, "PASS", "FAIL"),
    ifelse(full_mediation_ready == "NO", "PASS", "CHECK"),
    ifelse(nrow(coefs_19f) > 0 && nrow(fit_19f) > 0, "PASS", "FAIL"),
    ifelse(!is.na(weight_valid_n) && weight_valid_n > 0, "PASS", "CHECK"),
    ifelse(has_docx, "PASS_AFTER_SCRIPT_COMPLETES", "NO_DOCX_PACKAGE"),
    "PASS",
    "PENDING_MANUAL_REVIEW"
  ),
  note = c(
    paste0("Final sample n = ", final_n),
    paste0("Validation PASS count = ", validation_pass_n, "/", validation_total_n),
    "Script 19e decision matrix is available.",
    paste0("Full TPB mediation ready assessment = ", full_mediation_ready),
    paste0("Regression coefficient rows = ", nrow(coefs_19f)),
    paste0("Weight source: ", weight_source, "; valid n = ", weight_valid_n),
    "Word report status is confirmed after report creation step.",
    "Final CSV tables are created by Script 19g.",
    "Do not commit until reviewing the 19g report."
  )
)

readr::write_csv(commit_checklist, commit_checklist_path)

# ------------------------------------------------------------
# 12. Narrative text for final report
# ------------------------------------------------------------

family_attitude <- final_key_results |>
  dplyr::filter(model_id == "M1_D_adjusted_weighted", predictor == "Family connectedness")

friend_attitude <- final_key_results |>
  dplyr::filter(model_id == "M1_D_adjusted_weighted", predictor == "Friend support")

family_peer <- final_key_results |>
  dplyr::filter(model_id == "M2_D_adjusted_weighted", predictor == "Family connectedness")

friend_peer <- final_key_results |>
  dplyr::filter(model_id == "M2_D_adjusted_weighted", predictor == "Friend support")

family_partner <- final_key_results |>
  dplyr::filter(model_id == "M3_D_adjusted_weighted", predictor == "Family connectedness")

friend_partner <- final_key_results |>
  dplyr::filter(model_id == "M3_D_adjusted_weighted", predictor == "Friend support")

family_maternal <- final_key_results |>
  dplyr::filter(model_id == "M4_D_adjusted_weighted", predictor == "Family connectedness")

friend_maternal <- final_key_results |>
  dplyr::filter(model_id == "M4_D_adjusted_weighted", predictor == "Friend support")

result_sentence <- function(row, predictor, outcome) {
  if (nrow(row) == 0) {
    return(paste0("No adjusted weighted estimate was available for ", predictor, " and ", outcome, "."))
  }
  
  paste0(
    predictor,
    " was associated with ",
    outcome,
    " with b = ",
    row$coefficient[1],
    ", 95% CI ",
    row$confidence_interval[1],
    ", p = ",
    row$p_value_display[1],
    ", standardized beta = ",
    row$standardized_beta[1],
    "."
  )
}

markdown_lines <- c(
  "# Script 19g — Final Never-Sex TPB Mechanism Interpretation and Phase Summary",
  "",
  paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
  "",
  "## 1. Purpose",
  "",
  "This report consolidates the corrected never-sex phase of the Add Health adolescent risk models. The phase focuses on adolescents aged 15-19 who had not yet had sexual intercourse.",
  "",
  "The objective was to examine whether family connectedness and friend support are associated with psychosocial constructs compatible with the Theory of Planned Behavior.",
  "",
  "## 2. Analytic sample",
  "",
  paste0("The final analytic sample contains ", final_n, " adolescents aged 15-19 who had not yet had sexual intercourse."),
  "",
  "## 3. Construct validation",
  "",
  paste0("The corrected family, friend and sexual-delay deterrence constructs passed ", validation_pass_n, " of ", validation_total_n, " validation checks."),
  "",
  "The validated family connectedness measure, friend support measure and deterrence-related measures were used as the foundation for subsequent TPB-compatible operationalization.",
  "",
  "## 4. TPB operationalization decision",
  "",
  "A full TPB mediation model was not estimated. Script 19e classified full mediation as not ready because no direct intention-to-delay outcome was confirmed and perceived behavioral control/self-efficacy was not operationally confirmed.",
  "",
  "Accordingly, the final empirical phase estimates exploratory TPB-compatible mechanism regressions rather than formal mediation models.",
  "",
  "## 5. Regression specification",
  "",
  "The dependent variables were delay-supportive attitudes, peer norm, partner norm and maternal norm. The main predictors were family connectedness and friend support.",
  "",
  "Adjusted models controlled for age, sex/gender, grade and residence. Weighted adjusted models used GSWGT1 recovered from 21600-0004-Data.rda.",
  "",
  "## 6. Main weighted adjusted findings",
  "",
  result_sentence(family_attitude, "Family connectedness", "delay-supportive attitudes"),
  result_sentence(friend_attitude, "Friend support", "delay-supportive attitudes"),
  "",
  result_sentence(family_peer, "Family connectedness", "peer norm"),
  result_sentence(friend_peer, "Friend support", "peer norm"),
  "",
  result_sentence(family_partner, "Family connectedness", "partner norm"),
  result_sentence(friend_partner, "Friend support", "partner norm"),
  "",
  result_sentence(family_maternal, "Family connectedness", "maternal norm"),
  result_sentence(friend_maternal, "Friend support", "maternal norm"),
  "",
  "## 7. Interpretation",
  "",
  "The strongest and most consistent evidence concerns peer norms. Both family connectedness and friend support were positively associated with more delay-supportive peer norms.",
  "",
  "Family connectedness also remained positively associated with delay-supportive attitudes in the weighted adjusted model. Friend support did not remain independently associated with attitudes after adjustment.",
  "",
  "Partner norm results were weak. Friend support showed a small negative association with partner norm in the adjusted unweighted model and a marginal association in the weighted model; this should be interpreted cautiously.",
  "",
  "Maternal norm results showed that friend support was positively associated with perceived maternal disapproval, while family connectedness was not independently associated with this outcome after adjustment.",
  "",
  "## 8. Methodological caution",
  "",
  "These results are cross-sectional associations. They should not be interpreted as causal effects or as formal TPB mediation.",
  "",
  "The appropriate wording is exploratory TPB-compatible psychosocial mechanism analysis.",
  "",
  "## 9. Commit decision",
  "",
  "The phase is technically ready for manual review. Git commit should be performed only after reviewing the Word report and final tables.",
  "",
  "No Git action was performed by this script."
)

writeLines(markdown_lines, markdown_report_path)

# ------------------------------------------------------------
# 13. Formal Word report
# ------------------------------------------------------------

if (has_docx) {
  doc <- officer::read_docx()
  
  doc <- officer::body_add_par(doc, "Final Never-Sex TPB Mechanism Phase Summary", style = "heading 1")
  
  doc <- officer::body_add_par(
    doc,
    paste0("Script 19g | Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "1. Purpose", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste(
      "This report consolidates the corrected never-sex TPB-compatible mechanism phase.",
      "The analysis focuses on adolescents aged 15-19 who had not yet had sexual intercourse.",
      "The purpose is to summarise construct validation, TPB operationalization decisions, regression results and final reporting decisions."
    ),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "2. Analytic sample and construct validation", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste0(
      "The final analytic sample contains ",
      final_n,
      " adolescents aged 15-19 who had not yet had sexual intercourse. ",
      "The corrected constructs passed ",
      validation_pass_n,
      " of ",
      validation_total_n,
      " validation checks."
    ),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Construct summary from Script 19a_v3", style = "heading 3")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(construct_summary_19a)
    )
  )
  
  doc <- officer::body_add_par(doc, "Internal consistency from Script 19a_v3_check", style = "heading 3")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(internal_19a)
    )
  )
  
  doc <- officer::body_add_par(doc, "3. TPB operationalization decision", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste(
      "A full TPB mediation model was not estimated.",
      "Script 19e classified full mediation as not ready because no direct intention-to-delay outcome was confirmed and perceived behavioral control/self-efficacy was not operationally confirmed.",
      "The empirical phase is therefore reported as exploratory TPB-compatible mechanism regression models."
    ),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Mediation readiness assessment", style = "heading 3")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(readiness_19e)
    )
  )
  
  doc <- officer::body_add_par(doc, "Operational decision matrix", style = "heading 3")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(decision_19e)
    )
  )
  
  doc <- officer::body_add_par(doc, "4. Regression design", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste(
      "The regression models estimate associations between family connectedness, friend support and four TPB-compatible psychosocial outcomes:",
      "delay-supportive attitudes, peer norm, partner norm and maternal norm.",
      "Adjusted models control for age, sex/gender, grade and residence.",
      "Weighted adjusted models use GSWGT1 recovered from 21600-0004-Data.rda."
    ),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Covariate and weight audit", style = "heading 3")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(covariate_19f)
    )
  )
  
  doc <- officer::body_add_par(doc, "Model sample audit", style = "heading 3")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(sample_19f)
    )
  )
  
  doc <- officer::body_add_par(doc, "5. Final weighted adjusted results", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste(
      "The table below reports the final weighted adjusted estimates for the two main predictors.",
      "These estimates are the preferred summary for the phase because they include covariate adjustment and survey weights."
    ),
    style = "Normal"
  )
  
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(final_key_results)
    )
  )
  
  doc <- officer::body_add_par(doc, "6. Model fit summary", style = "heading 2")
  
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(model_fit_summary)
    )
  )
  
  doc <- officer::body_add_par(doc, "7. Interpretation", style = "heading 2")
  
  interpretation_paragraphs <- c(
    "The strongest and most consistent finding concerns peer norms. Both family connectedness and friend support were positively associated with more delay-supportive peer norms.",
    "Family connectedness also remained positively associated with delay-supportive attitudes in the weighted adjusted model. Friend support did not remain independently associated with delay-supportive attitudes after adjustment.",
    "Partner norm results were weak. Friend support showed a small negative association with partner norm in the adjusted unweighted model and a marginal association in the weighted model; this should be interpreted cautiously.",
    "Maternal norm results showed that friend support was positively associated with perceived maternal disapproval, while family connectedness was not independently associated with this outcome after adjustment."
  )
  
  for (p in interpretation_paragraphs) {
    doc <- officer::body_add_par(doc, p, style = "Normal")
  }
  
  doc <- officer::body_add_par(doc, "8. Methodological caution", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste(
      "These models are cross-sectional associations.",
      "They should not be interpreted as causal effects or as formal TPB mediation.",
      "The correct label is exploratory TPB-compatible psychosocial mechanism analysis."
    ),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "9. Phase decision and commit readiness", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    "The phase is technically ready for manual review. Git commit should be performed only after the Word report and final tables are reviewed.",
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Final phase decision", style = "heading 3")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(phase_decision)
    )
  )
  
  doc <- officer::body_add_par(doc, "Commit readiness checklist", style = "heading 3")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(
      flextable::flextable(commit_checklist)
    )
  )
  
  print(doc, target = docx_report_path)
}

# ------------------------------------------------------------
# 14. Refresh inventory and checklist after report creation
# ------------------------------------------------------------

phase_inventory <- phase_inventory |>
  dplyr::mutate(exists = file.exists(path))

readr::write_csv(phase_inventory, phase_inventory_path)

commit_checklist <- commit_checklist |>
  dplyr::mutate(
    status = dplyr::case_when(
      check_id == "formal_report_created" & file.exists(docx_report_path) ~ "PASS",
      TRUE ~ status
    ),
    note = dplyr::case_when(
      check_id == "formal_report_created" & file.exists(docx_report_path) ~
        paste0("Word report created: ", docx_report_path),
      TRUE ~ note
    )
  )

readr::write_csv(commit_checklist, commit_checklist_path)

# ------------------------------------------------------------
# 15. Console output
# ------------------------------------------------------------

end_time <- Sys.time()

log_line("Saved final key results: ", final_results_path)
log_line("Saved weighted main effects: ", weighted_main_effects_path)
log_line("Saved model fit summary: ", model_fit_summary_path)
log_line("Saved phase decision: ", phase_decision_path)
log_line("Saved phase inventory: ", phase_inventory_path)
log_line("Saved commit readiness checklist: ", commit_checklist_path)
log_line("Saved markdown report: ", markdown_report_path)

if (has_docx) {
  log_line("Saved Word report: ", docx_report_path)
} else {
  log_line("Word report not created because officer/flextable were unavailable.")
}

log_line("Completed ", script_id, " in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
log_line("No Git action was performed.")

cat("\n============================================================\n")
cat("Script 19g completed: Final Never-Sex TPB Mechanism Phase Summary\n")
cat("============================================================\n\n")

cat("Final phase decision:\n")
phase_decision |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nWeighted adjusted main effects:\n")
weighted_main_effects |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nFinal key results table:\n")
final_key_results |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nModel fit summary:\n")
model_fit_summary |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nCommit readiness checklist:\n")
commit_checklist |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nMain outputs:\n")

main_outputs <- tibble::tibble(
  output = c(
    "Final key results",
    "Weighted adjusted main effects",
    "Model fit summary",
    "Phase output inventory",
    "Commit readiness checklist",
    "Final phase decision",
    "Markdown report",
    "Word report",
    "Run log"
  ),
  path = c(
    final_results_path,
    weighted_main_effects_path,
    model_fit_summary_path,
    phase_inventory_path,
    commit_checklist_path,
    phase_decision_path,
    markdown_report_path,
    ifelse(has_docx, docx_report_path, NA_character_),
    log_path
  ),
  exists = file.exists(c(
    final_results_path,
    weighted_main_effects_path,
    model_fit_summary_path,
    phase_inventory_path,
    commit_checklist_path,
    phase_decision_path,
    markdown_report_path,
    ifelse(has_docx, docx_report_path, ""),
    log_path
  ))
)

main_outputs |>
  tibble::as_tibble() |>
  print(n = Inf)