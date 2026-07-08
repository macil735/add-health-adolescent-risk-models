# ============================================================
# Script 19e
# Manual Review and Final TPB Operationalization
# Project: Add Health Adolescent Risk Models
# ============================================================
#
# Purpose:
#   Review Script 19d outputs and define the final operational
#   status of TPB constructs before regression or mediation.
#
# Main decision:
#   Determine whether the project has:
#   1. a full TPB mediation-ready specification; or
#   2. only an exploratory TPB-compatible mechanism specification.
#
# Important:
#   This script does not estimate regressions or mediation models.
#   No Git action is performed.
#
# ============================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  warn = 1
)

script_id <- "19e"
script_title <- "Manual Review and Final TPB Operationalization"
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

if (!str_detect(basename(project_root), "add-health-adolescent-risk-models")) {
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

input_19d_dictionary <- "outputs/audits/script19d_tpb_candidate_dictionary.csv"
input_19d_coverage <- "outputs/audits/script19d_tpb_candidate_coverage_audit.csv"
input_19d_manual_template <- "outputs/audits/script19d_tpb_manual_review_TEMPLATE.csv"
input_19d_pilot_rds <- "outputs/analysis/script19d_tpb_pilot_construct_dataset.rds"
input_19d_pilot_summary <- "outputs/tables/script19d_tpb_pilot_construct_summary.csv"
input_19d_internal_consistency <- "outputs/tables/script19d_tpb_pilot_internal_consistency.csv"
input_19d_correlations <- "outputs/tables/script19d_tpb_pilot_construct_correlations.csv"
input_19a_v3_constructs <- "outputs/analysis/script19a_v3_corrected_never_sex_family_friend_deterrence_constructs.rds"
input_19b_candidates <- "outputs/audits/script19b_sexual_delay_raw_variable_candidates.csv"

decision_matrix_path <- "outputs/audits/script19e_tpb_operational_decision_matrix.csv"
final_item_decisions_path <- "outputs/audits/script19e_final_tpb_item_decisions.csv"
intention_candidate_audit_path <- "outputs/audits/script19e_intention_candidate_audit.csv"
mediation_readiness_path <- "outputs/audits/script19e_mediation_readiness_assessment.csv"
final_dataset_path <- "outputs/analysis/script19e_final_tpb_operationalization_dataset.rds"

final_construct_summary_path <- "outputs/tables/script19e_final_tpb_construct_summary.csv"
final_construct_correlations_path <- "outputs/tables/script19e_final_tpb_construct_correlations.csv"
recommended_models_path <- "outputs/tables/script19e_recommended_model_sequence.csv"

method_note_path <- "outputs/reports/script19e_final_tpb_operationalization_note.md"
docx_path <- "outputs/reports/script19e_manual_review_final_tpb_operationalization.docx"
log_path <- "outputs/logs/script19e_run_log.txt"

cat("", file = log_path)

log_line <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(txt, "\n", file = log_path, append = TRUE)
  message(txt)
}

log_line("Started ", script_id, ": ", script_title)
log_line("Project root: ", project_root)

# ------------------------------------------------------------
# 3. Input checks
# ------------------------------------------------------------

required_inputs <- c(
  input_19d_dictionary,
  input_19d_coverage,
  input_19d_manual_template,
  input_19d_pilot_rds,
  input_19d_pilot_summary,
  input_19d_internal_consistency,
  input_19d_correlations
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing required input file(s). Run Script 19d first. Missing: ",
    paste(missing_inputs, collapse = ", "),
    call. = FALSE
  )
}

tpb_dictionary <- read_csv(input_19d_dictionary, show_col_types = FALSE)
coverage_audit <- read_csv(input_19d_coverage, show_col_types = FALSE)
manual_template <- read_csv(input_19d_manual_template, show_col_types = FALSE)
pilot_df <- readRDS(input_19d_pilot_rds)
pilot_summary <- read_csv(input_19d_pilot_summary, show_col_types = FALSE)
pilot_internal_consistency <- read_csv(input_19d_internal_consistency, show_col_types = FALSE)
pilot_correlations <- read_csv(input_19d_correlations, show_col_types = FALSE)

if (file.exists(input_19b_candidates)) {
  candidates_19b <- read_csv(input_19b_candidates, show_col_types = FALSE)
} else {
  candidates_19b <- tibble()
}

log_line("Loaded 19d TPB dictionary: ", input_19d_dictionary)
log_line("Loaded 19d pilot dataset: ", input_19d_pilot_rds)
log_line("Rows in pilot dataset: ", nrow(pilot_df))
log_line("Columns in pilot dataset: ", ncol(pilot_df))

# ------------------------------------------------------------
# 4. Helper functions
# ------------------------------------------------------------

find_col <- function(data, patterns) {
  nms <- names(data)
  
  hit <- nms[
    purrr::map_lgl(
      nms,
      ~ all(stringr::str_detect(.x, patterns))
    )
  ]
  
  if (length(hit) == 0) {
    return(NA_character_)
  }
  
  hit[1]
}

find_col_any <- function(data, pattern) {
  hit <- names(data)[stringr::str_detect(names(data), pattern)]
  
  if (length(hit) == 0) {
    return(NA_character_)
  }
  
  hit[1]
}

summarise_var <- function(data, var, label) {
  if (is.na(var) || !(var %in% names(data))) {
    return(tibble(
      construct = label,
      variable = var,
      n_valid = NA_integer_,
      n_missing = NA_integer_,
      pct_valid = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      q25 = NA_real_,
      median = NA_real_,
      q75 = NA_real_,
      max = NA_real_
    ))
  }
  
  x <- data[[var]]
  n_valid <- sum(!is.na(x))
  n_missing <- sum(is.na(x))
  
  if (n_valid == 0) {
    return(tibble(
      construct = label,
      variable = var,
      n_valid = 0L,
      n_missing = n_missing,
      pct_valid = 0,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      q25 = NA_real_,
      median = NA_real_,
      q75 = NA_real_,
      max = NA_real_
    ))
  }
  
  tibble(
    construct = label,
    variable = var,
    n_valid = n_valid,
    n_missing = n_missing,
    pct_valid = round(100 * n_valid / nrow(data), 2),
    mean = round(mean(x, na.rm = TRUE), 4),
    sd = round(sd(x, na.rm = TRUE), 4),
    min = round(min(x, na.rm = TRUE), 4),
    q25 = round(as.numeric(quantile(x, 0.25, na.rm = TRUE)), 4),
    median = round(median(x, na.rm = TRUE), 4),
    q75 = round(as.numeric(quantile(x, 0.75, na.rm = TRUE)), 4),
    max = round(max(x, na.rm = TRUE), 4)
  )
}

safe_cor_long <- function(data, vars) {
  vars <- vars[vars %in% names(data)]
  
  if (length(vars) < 2) {
    return(tibble(
      var1 = character(),
      var2 = character(),
      correlation = numeric()
    ))
  }
  
  cor_mat <- suppressWarnings(
    cor(
      data[, vars, drop = FALSE],
      use = "pairwise.complete.obs"
    )
  )
  
  out <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  
  names(out) <- c("var1", "var2", "correlation")
  
  out |>
    mutate(
      correlation = round(as.numeric(correlation), 4)
    ) |>
    filter(var1 < var2) |>
    arrange(var1, var2)
}

# ------------------------------------------------------------
# 5. Locate key pilot variables
# ------------------------------------------------------------

family_var <- "family_connectedness_mean_1_5"
friend_var <- "friend_support_mean_1_5"
deterrence_var <- "sexual_delay_deterrence_mean_1_5"

attitude_var <- "tpb_attitudes_delay_mean_1_5"
subjective_norms_var <- "tpb_subjective_norms_delay_mean_1_5"
delay_orientation_proxy_var <- "tpb_delay_orientation_proxy_mean_1_5"

peer_norm_var <- find_col_any(
  pilot_df,
  "^tpb_subjective_norms_peer_approval_of_sex_H1MO1_score_1_5$"
)

partner_norm_var <- find_col_any(
  pilot_df,
  "^tpb_subjective_norms_partner_disapproval_H1MO2_score_1_5$"
)

maternal_norm_var <- find_col_any(
  pilot_df,
  "^tpb_subjective_norms_maternal_disapproval_H1MO4_score_1_5$"
)

key_vars <- c(
  family_var,
  friend_var,
  deterrence_var,
  attitude_var,
  subjective_norms_var,
  delay_orientation_proxy_var,
  peer_norm_var,
  partner_norm_var,
  maternal_norm_var
)

key_vars_present <- key_vars[!is.na(key_vars) & key_vars %in% names(pilot_df)]

# ------------------------------------------------------------
# 6. Intention candidate audit
# ------------------------------------------------------------

intention_terms <- c(
  "intend",
  "intention",
  "plan",
  "planning",
  "expect",
  "expected",
  "likely",
  "likelihood",
  "want",
  "ready",
  "wait",
  "delay",
  "postpone",
  "abstain",
  "abstin"
)

if (nrow(tpb_dictionary) > 0) {
  intention_from_19d <- tpb_dictionary |>
    mutate(
      search_text = paste(
        variable,
        source_section,
        tpb_domain,
        subdomain,
        item_text,
        direction_rule,
        matched_terms_metadata,
        sep = " | "
      ),
      possible_intention_signal = str_detect(
        tolower(search_text),
        paste(intention_terms, collapse = "|")
      ),
      source = "script19d_dictionary"
    ) |>
    filter(possible_intention_signal) |>
    select(
      source,
      variable,
      source_section,
      tpb_domain,
      subdomain,
      item_text,
      direction_rule,
      candidate_status,
      review_priority,
      candidate_score,
      priority_19b,
      likely_role_19b,
      matched_terms_metadata
    )
} else {
  intention_from_19d <- tibble()
}

if (nrow(candidates_19b) > 0) {
  intention_from_19b <- candidates_19b |>
    mutate(
      search_text = paste(
        variable,
        variable_label,
        levels_or_value_labels,
        matched_terms_metadata,
        matched_domains_metadata,
        likely_role,
        recommended_review,
        sep = " | "
      ),
      possible_intention_signal = str_detect(
        tolower(search_text),
        paste(intention_terms, collapse = "|")
      ),
      source = "script19b_candidates"
    ) |>
    filter(possible_intention_signal) |>
    transmute(
      source,
      variable,
      source_section = NA_character_,
      tpb_domain = NA_character_,
      subdomain = NA_character_,
      item_text = variable_label,
      direction_rule = NA_character_,
      candidate_status = priority,
      review_priority = priority,
      candidate_score,
      priority_19b = priority,
      likely_role_19b = likely_role,
      matched_terms_metadata
    )
} else {
  intention_from_19b <- tibble()
}

intention_candidate_audit <- bind_rows(
  intention_from_19d,
  intention_from_19b
) |>
  distinct(source, variable, .keep_all = TRUE) |>
  mutate(
    direct_intention_confirmed = FALSE,
    review_decision = case_when(
      str_detect(tolower(item_text), "intend|intention|plan|planning|expect|likely|wait|delay|postpone|abstain|abstin") ~
        "Needs manual codebook confirmation as possible intention item.",
      TRUE ~
        "Not a confirmed direct intention item."
    )
  ) |>
  arrange(source, variable)

if (nrow(intention_candidate_audit) == 0) {
  intention_candidate_audit <- tibble(
    source = "none_detected",
    variable = NA_character_,
    source_section = NA_character_,
    tpb_domain = NA_character_,
    subdomain = NA_character_,
    item_text = NA_character_,
    direction_rule = NA_character_,
    candidate_status = NA_character_,
    review_priority = NA_character_,
    candidate_score = NA_real_,
    priority_19b = NA_character_,
    likely_role_19b = NA_character_,
    matched_terms_metadata = NA_character_,
    direct_intention_confirmed = FALSE,
    review_decision = "No direct intention-to-delay item confirmed by automated review."
  )
}

write_csv(intention_candidate_audit, intention_candidate_audit_path)

direct_intention_confirmed <- any(
  intention_candidate_audit$direct_intention_confirmed == TRUE,
  na.rm = TRUE
)

# ------------------------------------------------------------
# 7. Final item-level TPB decisions
# ------------------------------------------------------------

final_item_decisions <- tibble::tribble(
  ~variable, ~final_construct, ~tpb_domain, ~final_role, ~include_in_confirmatory_mediation, ~include_in_exploratory_mechanism, ~recommended_use, ~reason,
  
  "H1MO3", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Anticipated guilt is an affective attitude toward sexual initiation; it is not a direct intention item.",
  
  "H1MO5", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Perceived physical pleasure is an attitude/benefit item; disagreement supports delay-oriented attitude.",
  
  "H1MO6", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Perceived relaxation is an attitude/benefit item; disagreement supports delay-oriented attitude.",
  
  "H1MO7", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Perceived attractiveness benefit is an attitude item; disagreement supports delay-oriented attitude.",
  
  "H1MO8", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Perceived loneliness reduction is an attitude/benefit item; disagreement supports delay-oriented attitude.",
  
  "H1MO9", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Pregnancy-related family embarrassment is a perceived negative consequence.",
  
  "H1MO10", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Pregnancy-related personal embarrassment is a perceived negative consequence.",
  
  "H1MO11", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Pregnancy-related school discontinuation is a perceived negative consequence.",
  
  "H1MO12", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Pregnancy-related wrong marriage risk is a perceived negative consequence.",
  
  "H1MO13", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Pregnancy-related forced maturity is a perceived negative consequence.",
  
  "H1MO14", "tpb_attitudes_delay", "attitudes", "mediator_candidate", FALSE, TRUE,
  "Use in exploratory TPB-compatible mechanism analysis.",
  "Pregnancy/baby decision stress is a perceived negative consequence.",
  
  "H1MO1", "peer_norm_delay", "subjective_norms", "separate_normative_mediator_candidate", FALSE, TRUE,
  "Use as a separate peer-norm item, not as part of a homogeneous subjective-norm scale.",
  "Subjective norms block had low internal consistency; peer, partner and maternal norms should be treated separately.",
  
  "H1MO2", "partner_norm_delay", "subjective_norms", "separate_normative_mediator_candidate", FALSE, TRUE,
  "Use as a separate partner-norm item, not as part of a homogeneous subjective-norm scale.",
  "Subjective norms block had low internal consistency; this item also overlaps with the earlier deterrence index.",
  
  "H1MO4", "maternal_norm_delay", "subjective_norms", "separate_normative_mediator_candidate", FALSE, TRUE,
  "Use as a separate maternal-norm item, not as part of a homogeneous subjective-norm scale.",
  "Subjective norms block had low internal consistency; H1MO4 has additional missingness because it depends on mother availability.",
  
  "H1SE1-H1SE4", "pbc_or_self_efficacy", "perceived_behavioral_control", "requires_manual_review", FALSE, FALSE,
  "Do not use until exact codebook wording is confirmed.",
  "Automated mapping flagged these items, but wording and direction were not confirmed in 19d.",
  
  "H1BC1-H1BC8", "pbc_or_contraceptive_control", "perceived_behavioral_control", "requires_manual_review", FALSE, FALSE,
  "Do not use until exact codebook wording is confirmed.",
  "These may concern contraception/birth control rather than perceived control over delaying sexual initiation.",
  
  "H1PA3/H1PA6/H1PA7", "norms_or_approval", "subjective_norms", "requires_manual_review", FALSE, FALSE,
  "Do not use until exact codebook wording is confirmed.",
  "Automated mapping flagged these items, but final role cannot be assigned without wording review."
)

write_csv(final_item_decisions, final_item_decisions_path)

# ------------------------------------------------------------
# 8. Operational decision matrix
# ------------------------------------------------------------

attitude_alpha <- pilot_internal_consistency |>
  filter(construct == "tpb_attitudes_delay") |>
  pull(alpha)

norm_alpha <- pilot_internal_consistency |>
  filter(construct == "tpb_subjective_norms_delay") |>
  pull(alpha)

if (length(attitude_alpha) == 0) attitude_alpha <- NA_real_
if (length(norm_alpha) == 0) norm_alpha <- NA_real_

decision_matrix <- tibble(
  component = c(
    "family_connectedness",
    "friend_support",
    "tpb_attitudes_delay",
    "tpb_subjective_norms_delay",
    "peer_partner_maternal_norm_items",
    "perceived_behavioral_control",
    "direct_intention_to_delay",
    "tpb_delay_orientation_proxy",
    "full_tpb_mediation",
    "exploratory_tpb_mechanism_analysis"
  ),
  operational_status = c(
    "ready",
    "ready",
    "usable_exploratory",
    "not_recommended_as_single_scale",
    "usable_exploratory_as_separate_items",
    "not_ready_requires_manual_review",
    "not_confirmed",
    "diagnostic_only_not_outcome_for_mediation",
    "not_ready",
    "ready_with_cautious_language"
  ),
  evidence = c(
    "Validated in 19a_v3_check; high coverage and acceptable internal consistency.",
    "Validated in 19a_v3_check; single-item support measure, high coverage.",
    paste0("Pilot alpha = ", round(attitude_alpha, 3), "; moderate exploratory consistency."),
    paste0("Pilot alpha = ", round(norm_alpha, 3), "; too low for homogeneous scale."),
    "H1MO1, H1MO2 and H1MO4 represent different normative referents.",
    "H1SE and H1BC candidates require exact wording confirmation before use.",
    "No direct intention-to-delay item confirmed by automated 19d/19e review.",
    "Highly correlated with its own component blocks; would create circularity if used as outcome.",
    "Blocked by absence of confirmed direct intention outcome and unconfirmed PBC.",
    "Can model family/friend connectedness associations with attitudes and separate normative items."
  ),
  decision = c(
    "include_as_predictor",
    "include_as_predictor_or_control",
    "include_as_mediator_candidate",
    "do_not_use_as_combined_scale",
    "include_as_separate_mediator_candidates",
    "exclude_until_manual_confirmation",
    "do_not_estimate_full_mediation",
    "do_not_use_as_dependent_variable_for_mediation",
    "do_not_estimate_yet",
    "proceed_to_exploratory_regression_models"
  )
)

write_csv(decision_matrix, decision_matrix_path)

# ------------------------------------------------------------
# 9. Mediation readiness assessment
# ------------------------------------------------------------

mediation_readiness <- tibble(
  criterion = c(
    "X variable available",
    "Atitude mediator available",
    "Norma subjetiva mediator available",
    "Controlo percebido / autoeficácia mediator available",
    "Direct intention-to-delay outcome available",
    "No mediator-outcome item overlap",
    "Full TPB mediation ready",
    "Exploratory TPB-compatible mechanism ready"
  ),
  assessment = c(
    "PASS",
    "PASS_WITH_CAUTION",
    "PASS_AS_SEPARATE_ITEMS_ONLY",
    "BLOCKED",
    "BLOCKED",
    "PASS_IF_PROXY_OUTCOME_IS_NOT_USED",
    "NO",
    "YES"
  ),
  explanation = c(
    "family_connectedness_mean_1_5 is available from 19a_v3.",
    "tpb_attitudes_delay_mean_1_5 is available but should be treated as exploratory.",
    "Combined subjective norms had low alpha; use peer, partner and maternal items separately.",
    "H1SE/H1BC candidates require manual codebook confirmation before use.",
    "No direct intention-to-delay item was confirmed.",
    "Avoid using tpb_delay_orientation_proxy or deterrence index as outcome if their items are used as mediators.",
    "Full TPB mediation requires confirmed PBC and direct intention outcome.",
    "Exploratory mechanism regressions can proceed using family/friend connectedness as predictors and TPB-compatible attitudes/norms as outcomes."
  )
)

write_csv(mediation_readiness, mediation_readiness_path)

# ------------------------------------------------------------
# 10. Final operationalization dataset
# ------------------------------------------------------------

final_dataset <- pilot_df |>
  transmute(
    row_id_19e = dplyr::row_number(),
    age_19d = if ("age_19d" %in% names(pilot_df)) age_19d else NA_real_,
    ever_sex_19d = if ("ever_sex_19d" %in% names(pilot_df)) ever_sex_19d else NA_real_,
    never_sex_19d = if ("never_sex_19d" %in% names(pilot_df)) never_sex_19d else NA,
    
    family_connectedness_mean_1_5 = if (family_var %in% names(pilot_df)) .data[[family_var]] else NA_real_,
    friend_support_mean_1_5 = if (friend_var %in% names(pilot_df)) .data[[friend_var]] else NA_real_,
    
    tpb_attitudes_delay_mean_1_5 = if (attitude_var %in% names(pilot_df)) .data[[attitude_var]] else NA_real_,
    
    peer_norm_delay_H1MO1 = if (!is.na(peer_norm_var) && peer_norm_var %in% names(pilot_df)) .data[[peer_norm_var]] else NA_real_,
    partner_norm_delay_H1MO2 = if (!is.na(partner_norm_var) && partner_norm_var %in% names(pilot_df)) .data[[partner_norm_var]] else NA_real_,
    maternal_norm_delay_H1MO4 = if (!is.na(maternal_norm_var) && maternal_norm_var %in% names(pilot_df)) .data[[maternal_norm_var]] else NA_real_,
    
    sexual_delay_deterrence_mean_1_5 = if (deterrence_var %in% names(pilot_df)) .data[[deterrence_var]] else NA_real_,
    tpb_subjective_norms_delay_mean_1_5 = if (subjective_norms_var %in% names(pilot_df)) .data[[subjective_norms_var]] else NA_real_,
    tpb_delay_orientation_proxy_mean_1_5 = if (delay_orientation_proxy_var %in% names(pilot_df)) .data[[delay_orientation_proxy_var]] else NA_real_
  )

saveRDS(final_dataset, final_dataset_path)

# ------------------------------------------------------------
# 11. Final summaries and correlations
# ------------------------------------------------------------

summary_vars <- c(
  family_connectedness_mean_1_5 = "family_connectedness",
  friend_support_mean_1_5 = "friend_support",
  tpb_attitudes_delay_mean_1_5 = "tpb_attitudes_delay",
  peer_norm_delay_H1MO1 = "peer_norm_delay",
  partner_norm_delay_H1MO2 = "partner_norm_delay",
  maternal_norm_delay_H1MO4 = "maternal_norm_delay",
  sexual_delay_deterrence_mean_1_5 = "sexual_delay_deterrence_diagnostic",
  tpb_subjective_norms_delay_mean_1_5 = "combined_subjective_norms_not_recommended",
  tpb_delay_orientation_proxy_mean_1_5 = "delay_orientation_proxy_diagnostic_only"
)

final_construct_summary <- bind_rows(
  purrr::map2(
    names(summary_vars),
    unname(summary_vars),
    ~ summarise_var(final_dataset, .x, .y)
  )
)

write_csv(final_construct_summary, final_construct_summary_path)

correlation_vars <- names(summary_vars)[
  names(summary_vars) %in% names(final_dataset)
]

final_construct_correlations <- safe_cor_long(
  final_dataset,
  correlation_vars
)

write_csv(final_construct_correlations, final_construct_correlations_path)

# ------------------------------------------------------------
# 12. Recommended model sequence
# ------------------------------------------------------------

recommended_models <- tibble::tribble(
  ~model_id, ~model_type, ~dependent_variable, ~main_predictors, ~interpretation, ~status,
  
  "19f_M1", "linear_regression",
  "tpb_attitudes_delay_mean_1_5",
  "family_connectedness_mean_1_5 + friend_support_mean_1_5 + covariates",
  "Tests whether family/friend connectedness is associated with delay-supportive attitudes.",
  "recommended_next",
  
  "19f_M2", "linear_regression",
  "peer_norm_delay_H1MO1",
  "family_connectedness_mean_1_5 + friend_support_mean_1_5 + covariates",
  "Tests association with perceived peer norm against/for sexual initiation.",
  "recommended_next",
  
  "19f_M3", "linear_regression",
  "partner_norm_delay_H1MO2",
  "family_connectedness_mean_1_5 + friend_support_mean_1_5 + covariates",
  "Tests association with perceived partner respect/sanction.",
  "recommended_next",
  
  "19f_M4", "linear_regression",
  "maternal_norm_delay_H1MO4",
  "family_connectedness_mean_1_5 + friend_support_mean_1_5 + covariates",
  "Tests association with perceived maternal disapproval.",
  "recommended_next",
  
  "19g_Mediation", "mediation",
  "direct intention-to-delay outcome",
  "family_connectedness -> TPB mediators -> intention",
  "Do not estimate until direct intention outcome and PBC are confirmed.",
  "blocked"
)

write_csv(recommended_models, recommended_models_path)

# ------------------------------------------------------------
# 13. Methodological note
# ------------------------------------------------------------

method_note <- c(
  "# Script 19e — Final TPB Operationalization Note",
  "",
  paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
  "",
  "## Main conclusion",
  "",
  "A full Theory of Planned Behavior mediation model is not ready at this stage.",
  "",
  "The project has validated family connectedness and friend support measures and has exploratory TPB-compatible attitude and normative constructs. However, no direct intention-to-delay-sexual-initiation item has been confirmed, and perceived behavioral control/self-efficacy candidates require manual codebook confirmation.",
  "",
  "## Operational decision",
  "",
  "- family_connectedness: ready as main predictor;",
  "- friend_support: ready as predictor or secondary interpersonal support variable;",
  "- tpb_attitudes_delay: usable as exploratory mediator/outcome in mechanism regressions;",
  "- subjective norms: do not use as one combined scale; use peer, partner and maternal norm items separately;",
  "- perceived behavioral control/self-efficacy: not ready;",
  "- direct intention to delay sexual initiation: not confirmed;",
  "- tpb_delay_orientation_proxy: diagnostic only; do not use as mediation outcome because it recombines the mediator blocks.",
  "",
  "## Recommended next step",
  "",
  "Proceed to Script 19f: Exploratory TPB-Compatible Mechanism Regression Models.",
  "",
  "Do not call the next phase a full TPB mediation analysis unless a direct intention outcome and a confirmed perceived behavioral control construct are found.",
  "",
  "No Git action was performed."
)

writeLines(method_note, method_note_path)

# ------------------------------------------------------------
# 14. Optional Word report
# ------------------------------------------------------------

if (has_docx) {
  doc <- officer::read_docx()
  
  doc <- officer::body_add_par(doc, script_title, style = "heading 1")
  
  doc <- officer::body_add_par(
    doc,
    paste0("Script: ", script_id),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(
    doc,
    paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Mediation readiness", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(mediation_readiness))
  )
  
  doc <- officer::body_add_par(doc, "Operational decision matrix", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(decision_matrix))
  )
  
  doc <- officer::body_add_par(doc, "Final item decisions", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(final_item_decisions))
  )
  
  doc <- officer::body_add_par(doc, "Final construct summary", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(final_construct_summary))
  )
  
  doc <- officer::body_add_par(doc, "Recommended model sequence", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(recommended_models))
  )
  
  print(doc, target = docx_path)
}

# ------------------------------------------------------------
# 15. Console output
# ------------------------------------------------------------

end_time <- Sys.time()

log_line("Saved operational decision matrix: ", decision_matrix_path)
log_line("Saved final item decisions: ", final_item_decisions_path)
log_line("Saved intention candidate audit: ", intention_candidate_audit_path)
log_line("Saved mediation readiness assessment: ", mediation_readiness_path)
log_line("Saved final operationalization dataset: ", final_dataset_path)
log_line("Saved final construct summary: ", final_construct_summary_path)
log_line("Saved final construct correlations: ", final_construct_correlations_path)
log_line("Saved recommended model sequence: ", recommended_models_path)
log_line("Saved methodological note: ", method_note_path)

if (has_docx) {
  log_line("Saved Word report: ", docx_path)
} else {
  log_line("Word report not created because officer/flextable were unavailable.")
}

log_line("Completed ", script_id, " in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
log_line("No Git action was performed.")

cat("\n============================================================\n")
cat("Script 19e completed: Final TPB Operationalization\n")
cat("============================================================\n\n")

cat("Mediation readiness assessment:\n")
print(mediation_readiness, n = Inf)

cat("\nOperational decision matrix:\n")
print(decision_matrix, n = Inf)

cat("\nFinal construct summary:\n")
print(final_construct_summary, n = Inf)

cat("\nRecommended model sequence:\n")
print(recommended_models, n = Inf)

cat("\nIntention candidate audit summary:\n")
print(
  intention_candidate_audit |>
    count(source, direct_intention_confirmed, review_decision, name = "n_rows"),
  n = Inf
)

cat("\nMain outputs:\n")
print(tibble(
  output = c(
    "Operational decision matrix",
    "Final item decisions",
    "Intention candidate audit",
    "Mediation readiness assessment",
    "Final operationalization dataset",
    "Final construct summary",
    "Final construct correlations",
    "Recommended model sequence",
    "Methodological note",
    "Word report",
    "Run log"
  ),
  path = c(
    decision_matrix_path,
    final_item_decisions_path,
    intention_candidate_audit_path,
    mediation_readiness_path,
    final_dataset_path,
    final_construct_summary_path,
    final_construct_correlations_path,
    recommended_models_path,
    method_note_path,
    ifelse(has_docx, docx_path, NA_character_),
    log_path
  ),
  exists = file.exists(c(
    decision_matrix_path,
    final_item_decisions_path,
    intention_candidate_audit_path,
    mediation_readiness_path,
    final_dataset_path,
    final_construct_summary_path,
    final_construct_correlations_path,
    recommended_models_path,
    method_note_path,
    ifelse(has_docx, docx_path, ""),
    log_path
  ))
))