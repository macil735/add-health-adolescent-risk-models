# ============================================================
# Project: add-health-adolescent-risk-models
# Script 10: Weighted Logistic Regression Models
# Author: Gelo Picol
#
# Purpose:
#   Estimate weighted logistic regression models for selected
#   Add Health Wave I binary outcomes using the modeling framework
#   created by Script 09b.
#
# Main sample:
#   - Students in grades 10 to 12 at Wave I.
#
# Sensitivity sample:
#   - Students in grades 10 to 12 and ages 15 to 19.
#
# Important:
#   - Models are associational, not causal.
#   - GSWGT1 is used as the Wave I population-average weight.
#   - AID is used only internally and is never exported.
#   - Individual-level data are not exported.
# ============================================================


# ============================================================
# 0. Project root and options
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"

options(na.print = "NA")
options(survey.lonely.psu = "adjust")


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
  "survey",
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
library(survey)
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

script09b_outcome_modeling_plan_path <- file.path(
  outputs_tables_dir,
  "script09b_wave01_outcome_modeling_plan.csv"
)

script09b_core_controls_path <- file.path(
  outputs_tables_dir,
  "script09b_wave01_core_controls_review.csv"
)

script09b_predictor_eligibility_path <- file.path(
  outputs_tables_dir,
  "script09b_wave01_predictor_outcome_eligibility.csv"
)

script09b_recommended_model_sequence_path <- file.path(
  outputs_tables_dir,
  "script09b_wave01_recommended_model_sequence.csv"
)

script09b_data_sample_review_path <- file.path(
  outputs_tables_dir,
  "script09b_wave01_data_sample_review.csv"
)


# ============================================================
# 3. Helper functions
# ============================================================

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(haven::zap_labels(x)))
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

parse_semicolon_list <- function(x) {
  if (length(x) == 0 || is.na(x) || stringr::str_trim(x) == "") {
    return(character(0))
  }

  stringr::str_split(x, ";")[[1]] %>%
    stringr::str_trim() %>%
    purrr::discard(~ .x == "")
}

is_binary_01 <- function(x) {
  x_num <- safe_numeric(x)
  vals <- sort(unique(x_num[!is.na(x_num)]))

  length(vals) > 0 && all(vals %in% c(0, 1))
}

should_be_factor <- function(variable, x) {
  x_num <- safe_numeric(x)
  n_unique <- dplyr::n_distinct(x_num, na.rm = TRUE)

  if (variable == "a_grade_wave1") {
    return(TRUE)
  }

  if (variable %in% c("a_age_wave1", "a_female")) {
    return(FALSE)
  }

  if (is_binary_01(x)) {
    return(FALSE)
  }

  if (n_unique <= 10) {
    return(TRUE)
  }

  FALSE
}

prepare_predictor_vector <- function(variable, x) {
  x_num <- safe_numeric(x)

  if (should_be_factor(variable, x)) {
    return(factor(x_num))
  }

  x_num
}

make_design <- function(data) {
  survey::svydesign(
    ids = ~1,
    weights = ~GSWGT1,
    data = data
  )
}

assign_term_variable <- function(term, predictors) {
  if (term == "(Intercept)") {
    return("(Intercept)")
  }

  predictors_ordered <- predictors[order(nchar(predictors), decreasing = TRUE)]

  for (p in predictors_ordered) {
    if (term == p || startsWith(term, p)) {
      return(p)
    }
  }

  term
}

collapse_unique <- function(x) {
  x <- unique(x[!is.na(x) & x != ""])

  if (length(x) == 0) {
    return("")
  }

  paste(x, collapse = " | ")
}

create_empty_coefficients <- function() {
  tibble(
    model_id = character(),
    sample_name = character(),
    outcome = character(),
    model_stage = character(),
    term = character(),
    term_variable = character(),
    estimate_log_odds = numeric(),
    std_error = numeric(),
    statistic = numeric(),
    p_value = numeric(),
    odds_ratio = numeric(),
    conf_low_or = numeric(),
    conf_high_or = numeric(),
    significance = character(),
    coefficient_status = character()
  )
}

create_model_summary_row <- function(
    model_id,
    sample_name,
    outcome,
    model_stage,
    model_purpose,
    requested_model_status,
    mandatory_controls,
    suggested_predictors,
    predictors_used,
    n_complete,
    n_yes,
    n_no,
    weighted_total,
    weighted_yes,
    fit_status,
    skip_reason,
    warning_messages,
    n_parameters,
    residual_df,
    null_deviance,
    residual_deviance,
    dispersion
) {

  # Force all scalar diagnostics to simple base R types.
  # This avoids bind_rows() errors when survey objects return
  # classed values such as svystat.
  n_complete_num <- suppressWarnings(as.integer(n_complete)[1])
  n_yes_num <- suppressWarnings(as.integer(n_yes)[1])
  n_no_num <- suppressWarnings(as.integer(n_no)[1])

  weighted_total_num <- suppressWarnings(as.numeric(weighted_total)[1])
  weighted_yes_num <- suppressWarnings(as.numeric(weighted_yes)[1])

  n_parameters_num <- suppressWarnings(as.integer(n_parameters)[1])
  residual_df_num <- suppressWarnings(as.numeric(residual_df)[1])
  null_deviance_num <- suppressWarnings(as.numeric(null_deviance)[1])
  residual_deviance_num <- suppressWarnings(as.numeric(residual_deviance)[1])
  dispersion_num <- suppressWarnings(as.numeric(dispersion)[1])

  if (length(predictors_used) == 0) {
    predictors_used_text <- ""
  } else {
    predictors_used_text <- paste(predictors_used, collapse = "; ")
  }

  if (length(mandatory_controls) == 0) {
    mandatory_controls_text <- ""
  } else {
    mandatory_controls_text <- paste(mandatory_controls, collapse = "; ")
  }

  if (length(suggested_predictors) == 0) {
    suggested_predictors_text <- ""
  } else {
    suggested_predictors_text <- paste(suggested_predictors, collapse = "; ")
  }

  weighted_pct_yes_num <- ifelse(
    !is.na(weighted_total_num) && weighted_total_num > 0,
    100 * weighted_yes_num / weighted_total_num,
    NA_real_
  )

  tibble(
    model_id = as.character(model_id),
    sample_name = as.character(sample_name),
    outcome = as.character(outcome),
    model_stage = as.character(model_stage),
    model_purpose = as.character(model_purpose),
    requested_model_status = as.character(requested_model_status),
    mandatory_controls = mandatory_controls_text,
    suggested_predictors = suggested_predictors_text,
    predictors_used = predictors_used_text,
    n_predictors_used = as.integer(length(predictors_used)),
    n_complete = n_complete_num,
    public_n_complete = safe_count_public(n_complete_num),
    n_outcome_yes = n_yes_num,
    n_outcome_no = n_no_num,
    public_n_outcome_yes = safe_count_public(n_yes_num),
    public_n_outcome_no = safe_count_public(n_no_num),
    weighted_total = weighted_total_num,
    weighted_outcome_yes = weighted_yes_num,
    weighted_pct_yes = weighted_pct_yes_num,
    fit_status = as.character(fit_status),
    skip_reason = as.character(skip_reason),
    warning_messages = as.character(warning_messages),
    n_parameters = n_parameters_num,
    residual_df = residual_df_num,
    null_deviance = null_deviance_num,
    residual_deviance = residual_deviance_num,
    dispersion = dispersion_num
  )
}


create_predictor_inventory <- function(
    model_id,
    sample_name,
    outcome,
    model_stage,
    mandatory_controls,
    suggested_predictors,
    predictors_used,
    missing_predictors,
    removed_no_variation
) {

  all_predictors <- unique(c(mandatory_controls, suggested_predictors))

  if (length(all_predictors) == 0) {
    return(
      tibble(
        model_id = model_id,
        sample_name = sample_name,
        outcome = outcome,
        model_stage = model_stage,
        predictor = character(),
        predictor_source = character(),
        predictor_status = character()
      )
    )
  }

  tibble(
    model_id = model_id,
    sample_name = sample_name,
    outcome = outcome,
    model_stage = model_stage,
    predictor = all_predictors,
    predictor_source = case_when(
      predictor %in% mandatory_controls ~ "mandatory_control",
      predictor %in% suggested_predictors ~ "suggested_block_predictor",
      TRUE ~ "other"
    ),
    predictor_status = case_when(
      predictor %in% predictors_used ~ "used_in_model",
      predictor %in% missing_predictors ~ "not_found_in_data",
      predictor %in% removed_no_variation ~ "removed_no_variation",
      TRUE ~ "not_used"
    )
  )
}

fit_one_model <- function(data_sample, sample_name, spec_row) {

  outcome <- spec_row$outcome[[1]]
  model_stage <- spec_row$model_stage[[1]]
  model_purpose <- spec_row$model_purpose[[1]]
  requested_model_status <- spec_row$model_status[[1]]

  model_id <- paste(sample_name, outcome, model_stage, sep = "__")

  # Initialize objects used across different model-fitting branches.
  # This prevents failures when a model is skipped before predictors,
  # sample size, and weighted totals are fully computed.
  valid_predictors <- character(0)
  available_predictors <- character(0)
  missing_predictors <- character(0)
  removed_no_variation <- character(0)

  n_complete <- NA_integer_
  n_yes <- NA_integer_
  n_no <- NA_integer_
  weighted_total <- NA_real_
  weighted_yes <- NA_real_

  mandatory_controls <- parse_semicolon_list(
    spec_row$mandatory_controls[[1]]
  )

  suggested_predictors <- parse_semicolon_list(
    spec_row$suggested_predictors[[1]]
  )

  if (model_stage != "M0_core_controls" && length(suggested_predictors) == 0) {

    summary_row <- create_model_summary_row(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      model_purpose = model_purpose,
      requested_model_status = requested_model_status,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      n_complete = NA_integer_,
      n_yes = NA_integer_,
      n_no = NA_integer_,
      weighted_total = NA_real_,
      weighted_yes = NA_real_,
      fit_status = "skipped",
      skip_reason = "no_suggested_predictors_for_this_block",
      warning_messages = "",
      n_parameters = NA_integer_,
      residual_df = NA_real_,
      null_deviance = NA_real_,
      residual_deviance = NA_real_,
      dispersion = NA_real_
    )

    inventory <- create_predictor_inventory(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      missing_predictors = character(0),
      removed_no_variation = character(0)
    )

    return(
      list(
        summary = summary_row,
        coefficients = create_empty_coefficients(),
        predictor_inventory = inventory
      )
    )
  }

  planned_predictors <- unique(c(mandatory_controls, suggested_predictors))
  planned_predictors <- setdiff(planned_predictors, outcome)

  required_variables <- unique(c(outcome, planned_predictors, "GSWGT1"))

  missing_predictors <- setdiff(planned_predictors, names(data_sample))
  missing_required <- setdiff(required_variables, names(data_sample))

  if (outcome %in% missing_required || "GSWGT1" %in% missing_required) {

    summary_row <- create_model_summary_row(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      model_purpose = model_purpose,
      requested_model_status = requested_model_status,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      n_complete = NA_integer_,
      n_yes = NA_integer_,
      n_no = NA_integer_,
      weighted_total = NA_real_,
      weighted_yes = NA_real_,
      fit_status = "skipped",
      skip_reason = paste("missing_required_variables:", paste(missing_required, collapse = "; ")),
      warning_messages = "",
      n_parameters = NA_integer_,
      residual_df = NA_real_,
      null_deviance = NA_real_,
      residual_deviance = NA_real_,
      dispersion = NA_real_
    )

    inventory <- create_predictor_inventory(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      missing_predictors = missing_predictors,
      removed_no_variation = character(0)
    )

    return(
      list(
        summary = summary_row,
        coefficients = create_empty_coefficients(),
        predictor_inventory = inventory
      )
    )
  }

  available_predictors <- planned_predictors[
    planned_predictors %in% names(data_sample)
  ]

  if (length(available_predictors) == 0) {

    summary_row <- create_model_summary_row(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      model_purpose = model_purpose,
      requested_model_status = requested_model_status,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      n_complete = NA_integer_,
      n_yes = NA_integer_,
      n_no = NA_integer_,
      weighted_total = NA_real_,
      weighted_yes = NA_real_,
      fit_status = "skipped",
      skip_reason = "no_available_predictors",
      warning_messages = "",
      n_parameters = NA_integer_,
      residual_df = NA_real_,
      null_deviance = NA_real_,
      residual_deviance = NA_real_,
      dispersion = NA_real_
    )

    inventory <- create_predictor_inventory(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      missing_predictors = missing_predictors,
      removed_no_variation = character(0)
    )

    return(
      list(
        summary = summary_row,
        coefficients = create_empty_coefficients(),
        predictor_inventory = inventory
      )
    )
  }

  model_data <- data_sample %>%
    select(all_of(unique(c(outcome, available_predictors, "GSWGT1")))) %>%
    mutate(
      .outcome_model = safe_numeric(.data[[outcome]])
    ) %>%
    filter(
      !is.na(.outcome_model),
      .outcome_model %in% c(0, 1),
      !is.na(GSWGT1),
      GSWGT1 > 0
    )

  for (p in available_predictors) {
    model_data[[p]] <- prepare_predictor_vector(p, model_data[[p]])
  }

  complete_vars <- unique(c(".outcome_model", available_predictors, "GSWGT1"))

  model_data_complete <- model_data[
    stats::complete.cases(model_data[, complete_vars]),
  ]

  if (nrow(model_data_complete) == 0) {

    summary_row <- create_model_summary_row(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      model_purpose = model_purpose,
      requested_model_status = requested_model_status,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      n_complete = 0,
      n_yes = 0,
      n_no = 0,
      weighted_total = NA_real_,
      weighted_yes = NA_real_,
      fit_status = "skipped",
      skip_reason = "no_complete_cases",
      warning_messages = "",
      n_parameters = NA_integer_,
      residual_df = NA_real_,
      null_deviance = NA_real_,
      residual_deviance = NA_real_,
      dispersion = NA_real_
    )

    inventory <- create_predictor_inventory(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      missing_predictors = missing_predictors,
      removed_no_variation = character(0)
    )

    return(
      list(
        summary = summary_row,
        coefficients = create_empty_coefficients(),
        predictor_inventory = inventory
      )
    )
  }

  valid_predictors <- available_predictors[
    vapply(
      available_predictors,
      function(p) dplyr::n_distinct(model_data_complete[[p]], na.rm = TRUE) >= 2,
      logical(1)
    )
  ]

  removed_no_variation <- setdiff(available_predictors, valid_predictors)

  complete_vars_final <- unique(c(".outcome_model", valid_predictors, "GSWGT1"))

  model_data_complete <- model_data[
    stats::complete.cases(model_data[, complete_vars_final]),
  ]

  n_complete <- nrow(model_data_complete)
  n_yes <- sum(model_data_complete$.outcome_model == 1, na.rm = TRUE)
  n_no  <- sum(model_data_complete$.outcome_model == 0, na.rm = TRUE)

  weighted_total <- sum(model_data_complete$GSWGT1, na.rm = TRUE)
  weighted_yes <- sum(
    (model_data_complete$.outcome_model == 1) * model_data_complete$GSWGT1,
    na.rm = TRUE
  )

  if (length(valid_predictors) == 0) {

    summary_row <- create_model_summary_row(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      model_purpose = model_purpose,
      requested_model_status = requested_model_status,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      n_complete = n_complete,
      n_yes = n_yes,
      n_no = n_no,
      weighted_total = weighted_total,
      weighted_yes = weighted_yes,
      fit_status = "skipped",
      skip_reason = "no_predictors_with_variation",
      warning_messages = "",
      n_parameters = NA_integer_,
      residual_df = NA_real_,
      null_deviance = NA_real_,
      residual_deviance = NA_real_,
      dispersion = NA_real_
    )

    inventory <- create_predictor_inventory(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = character(0),
      missing_predictors = missing_predictors,
      removed_no_variation = removed_no_variation
    )

    return(
      list(
        summary = summary_row,
        coefficients = create_empty_coefficients(),
        predictor_inventory = inventory
      )
    )
  }

  if (n_complete < 100 || n_yes < 10 || n_no < 10) {

    summary_row <- create_model_summary_row(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      model_purpose = model_purpose,
      requested_model_status = requested_model_status,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = valid_predictors,
      n_complete = n_complete,
      n_yes = n_yes,
      n_no = n_no,
      weighted_total = weighted_total,
      weighted_yes = weighted_yes,
      fit_status = "skipped",
      skip_reason = "insufficient_complete_cases_or_outcome_cells",
      warning_messages = "",
      n_parameters = NA_integer_,
      residual_df = NA_real_,
      null_deviance = NA_real_,
      residual_deviance = NA_real_,
      dispersion = NA_real_
    )

    inventory <- create_predictor_inventory(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = valid_predictors,
      missing_predictors = missing_predictors,
      removed_no_variation = removed_no_variation
    )

    return(
      list(
        summary = summary_row,
        coefficients = create_empty_coefficients(),
        predictor_inventory = inventory
      )
    )
  }

  formula_model <- stats::reformulate(
    termlabels = valid_predictors,
    response = ".outcome_model"
  )

  design_model <- make_design(model_data_complete)

  warning_messages <- character(0)

  fit <- tryCatch(
    withCallingHandlers(
      survey::svyglm(
        formula_model,
        design = design_model,
        family = quasibinomial()
      ),
      warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {

    summary_row <- create_model_summary_row(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      model_purpose = model_purpose,
      requested_model_status = requested_model_status,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = valid_predictors,
      n_complete = n_complete,
      n_yes = n_yes,
      n_no = n_no,
      weighted_total = weighted_total,
      weighted_yes = weighted_yes,
      fit_status = "failed",
      skip_reason = conditionMessage(fit),
      warning_messages = collapse_unique(warning_messages),
      n_parameters = NA_integer_,
      residual_df = NA_real_,
      null_deviance = NA_real_,
      residual_deviance = NA_real_,
      dispersion = NA_real_
    )

    inventory <- create_predictor_inventory(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      mandatory_controls = mandatory_controls,
      suggested_predictors = suggested_predictors,
      predictors_used = valid_predictors,
      missing_predictors = missing_predictors,
      removed_no_variation = removed_no_variation
    )

    return(
      list(
        summary = summary_row,
        coefficients = create_empty_coefficients(),
        predictor_inventory = inventory
      )
    )
  }

  fit_summary <- summary(fit)
  coef_matrix <- as.data.frame(fit_summary$coefficients)
  coef_matrix$term <- rownames(coef_matrix)

  names(coef_matrix)[1:4] <- c(
    "estimate_log_odds",
    "std_error",
    "statistic",
    "p_value"
  )

  coefficients <- coef_matrix %>%
    as_tibble() %>%
    mutate(
      model_id = model_id,
      sample_name = sample_name,
      outcome = outcome,
      model_stage = model_stage,
      term_variable = vapply(
        term,
        assign_term_variable,
        character(1),
        predictors = valid_predictors
      ),
      odds_ratio = exp(estimate_log_odds),
      conf_low_or = exp(estimate_log_odds - 1.96 * std_error),
      conf_high_or = exp(estimate_log_odds + 1.96 * std_error),
      significance = case_when(
        is.na(p_value) ~ "",
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        p_value < 0.1 ~ ".",
        TRUE ~ ""
      ),
      coefficient_status = case_when(
        is.na(estimate_log_odds) | is.na(std_error) ~ "not_estimable",
        TRUE ~ "estimated"
      )
    ) %>%
    select(
      model_id,
      sample_name,
      outcome,
      model_stage,
      term,
      term_variable,
      estimate_log_odds,
      std_error,
      statistic,
      p_value,
      odds_ratio,
      conf_low_or,
      conf_high_or,
      significance,
      coefficient_status
    )

  summary_row <- create_model_summary_row(
    model_id = model_id,
    sample_name = sample_name,
    outcome = outcome,
    model_stage = model_stage,
    model_purpose = model_purpose,
    requested_model_status = requested_model_status,
    mandatory_controls = mandatory_controls,
    suggested_predictors = suggested_predictors,
    predictors_used = valid_predictors,
    n_complete = n_complete,
    n_yes = n_yes,
    n_no = n_no,
    weighted_total = weighted_total,
    weighted_yes = weighted_yes,
    fit_status = "fitted",
    skip_reason = "",
    warning_messages = collapse_unique(warning_messages),
    n_parameters = length(stats::coef(fit)),
    residual_df = fit$df.residual,
    null_deviance = fit$null.deviance,
    residual_deviance = fit$deviance,
    dispersion = as.numeric(fit_summary$dispersion)
  )

  inventory <- create_predictor_inventory(
    model_id = model_id,
    sample_name = sample_name,
    outcome = outcome,
    model_stage = model_stage,
    mandatory_controls = mandatory_controls,
    suggested_predictors = suggested_predictors,
    predictors_used = valid_predictors,
    missing_predictors = missing_predictors,
    removed_no_variation = removed_no_variation
  )

  list(
    summary = summary_row,
    coefficients = coefficients,
    predictor_inventory = inventory
  )
}


# ============================================================
# 4. Check required inputs
# ============================================================

required_inputs <- c(
  weighted_analytical_rds_path,
  script09b_outcome_modeling_plan_path,
  script09b_core_controls_path,
  script09b_predictor_eligibility_path,
  script09b_recommended_model_sequence_path,
  script09b_data_sample_review_path
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
# 5. Load data and Script 09b framework
# ============================================================

analysis_data <- readRDS(weighted_analytical_rds_path)

outcome_modeling_plan <- read_csv(
  script09b_outcome_modeling_plan_path,
  show_col_types = FALSE
)

core_controls_review <- read_csv(
  script09b_core_controls_path,
  show_col_types = FALSE
)

predictor_outcome_eligibility <- read_csv(
  script09b_predictor_eligibility_path,
  show_col_types = FALSE
)

recommended_model_sequence <- read_csv(
  script09b_recommended_model_sequence_path,
  show_col_types = FALSE
)

data_sample_review <- read_csv(
  script09b_data_sample_review_path,
  show_col_types = FALSE
)

if (!"AID" %in% names(analysis_data)) {
  stop("AID should exist internally in the local-only weighted analytical file.")
}

if (!"GSWGT1" %in% names(analysis_data)) {
  stop("GSWGT1 not found in weighted analytical data.")
}


# ============================================================
# 6. Define modeling samples
# ============================================================

analysis_data_valid <- analysis_data %>%
  filter(!is.na(GSWGT1) & GSWGT1 > 0)

sample_main <- analysis_data_valid %>%
  filter(a_main_sample_grade_10_12 == TRUE)

sample_strict <- analysis_data_valid %>%
  filter(a_strict_sample_grade_age == TRUE)

sample_list <- list(
  main_grade_10_12 = sample_main,
  strict_grade_10_12_age_15_19 = sample_strict
)


# ============================================================
# 7. Select eligible outcomes and model specifications
# ============================================================

eligible_outcomes <- outcome_modeling_plan %>%
  filter(
    recommended_modeling_status %in% c(
      "recommended_for_script10_primary_or_secondary_model",
      "recommended_for_script10_secondary_model"
    )
  ) %>%
  pull(variable) %>%
  unique()

eligible_outcomes <- eligible_outcomes[
  eligible_outcomes %in% names(analysis_data_valid)
]

excluded_outcomes <- outcome_modeling_plan %>%
  filter(!variable %in% eligible_outcomes) %>%
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

model_specifications <- recommended_model_sequence %>%
  filter(
    outcome %in% eligible_outcomes,
    model_status == "ready_for_script10_specification"
  ) %>%
  mutate(
    model_specification_id = paste(outcome, model_stage, sep = "__"),
    script10_status = case_when(
      model_stage == "M0_core_controls" ~ "estimate_core_model",
      suggested_predictors == "" | is.na(suggested_predictors) ~ "skip_no_block_predictors",
      TRUE ~ "estimate_block_model"
    )
  ) %>%
  arrange(
    outcome,
    model_stage
  )


# ============================================================
# 8. Estimate models
# ============================================================

model_results <- list()
result_index <- 1

for (sample_name in names(sample_list)) {

  data_sample <- sample_list[[sample_name]]

  for (i in seq_len(nrow(model_specifications))) {

    spec_row <- model_specifications[i, ]

    model_results[[result_index]] <- fit_one_model(
      data_sample = data_sample,
      sample_name = sample_name,
      spec_row = spec_row
    )

    result_index <- result_index + 1
  }
}

model_fit_summary <- purrr::map_dfr(
  model_results,
  "summary"
) %>%
  arrange(
    sample_name,
    outcome,
    model_stage
  )

model_coefficients <- purrr::map_dfr(
  model_results,
  "coefficients"
) %>%
  arrange(
    sample_name,
    outcome,
    model_stage,
    term
  )

model_predictor_inventory <- purrr::map_dfr(
  model_results,
  "predictor_inventory"
) %>%
  arrange(
    sample_name,
    outcome,
    model_stage,
    predictor
  )


# ============================================================
# 9. Summaries and review tables
# ============================================================

model_execution_summary <- model_fit_summary %>%
  count(
    sample_name,
    fit_status,
    skip_reason,
    name = "n_models"
  ) %>%
  arrange(
    sample_name,
    fit_status,
    skip_reason
  )

outcome_model_summary <- model_fit_summary %>%
  group_by(sample_name, outcome) %>%
  summarise(
    n_models_requested = n(),
    n_models_fitted = sum(fit_status == "fitted", na.rm = TRUE),
    n_models_skipped = sum(fit_status == "skipped", na.rm = TRUE),
    n_models_failed = sum(fit_status == "failed", na.rm = TRUE),
    min_n_complete = suppressWarnings(min(n_complete, na.rm = TRUE)),
    max_n_complete = suppressWarnings(max(n_complete, na.rm = TRUE)),
    min_n_yes = suppressWarnings(min(n_outcome_yes, na.rm = TRUE)),
    max_n_yes = suppressWarnings(max(n_outcome_yes, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    min_n_complete = ifelse(is.infinite(min_n_complete), NA_real_, min_n_complete),
    max_n_complete = ifelse(is.infinite(max_n_complete), NA_real_, max_n_complete),
    min_n_yes = ifelse(is.infinite(min_n_yes), NA_real_, min_n_yes),
    max_n_yes = ifelse(is.infinite(max_n_yes), NA_real_, max_n_yes)
  ) %>%
  arrange(sample_name, outcome)

significant_coefficients <- model_coefficients %>%
  filter(
    term != "(Intercept)",
    coefficient_status == "estimated",
    !is.na(p_value),
    p_value < 0.05
  ) %>%
  arrange(
    sample_name,
    outcome,
    model_stage,
    p_value
  )

odds_ratio_review <- model_coefficients %>%
  filter(
    term != "(Intercept)",
    coefficient_status == "estimated"
  ) %>%
  mutate(
    or_interpretation_flag = case_when(
      is.na(odds_ratio) ~ "not_available",
      odds_ratio >= 5 | odds_ratio <= 0.2 ~ "large_or_extreme_or_review",
      conf_low_or > 1 | conf_high_or < 1 ~ "confidence_interval_excludes_one",
      TRUE ~ "confidence_interval_includes_one"
    )
  ) %>%
  arrange(
    sample_name,
    outcome,
    model_stage,
    p_value
  )

skipped_or_failed_models <- model_fit_summary %>%
  filter(fit_status != "fitted") %>%
  arrange(
    sample_name,
    outcome,
    model_stage
  )

model_stage_summary <- model_fit_summary %>%
  group_by(sample_name, model_stage) %>%
  summarise(
    n_models = n(),
    n_fitted = sum(fit_status == "fitted", na.rm = TRUE),
    n_skipped = sum(fit_status == "skipped", na.rm = TRUE),
    n_failed = sum(fit_status == "failed", na.rm = TRUE),
    mean_n_complete = round(mean(n_complete, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(sample_name, model_stage)

predictor_frequency_summary <- model_predictor_inventory %>%
  filter(predictor_status == "used_in_model") %>%
  count(
    sample_name,
    predictor,
    predictor_source,
    name = "n_models_used"
  ) %>%
  arrange(
    sample_name,
    desc(n_models_used),
    predictor
  )


# ============================================================
# 10. Methodological notes
# ============================================================

script10_methodological_notes <- tibble(
  note_id = 1:16,
  note = c(
    "Script 10 estimates weighted logistic regression models for Add Health Wave I.",
    "The models use GSWGT1 as the Wave I population-average sampling weight.",
    "The main analytical sample is students in grades 10 to 12.",
    "The strict sensitivity sample is students in grades 10 to 12 and ages 15 to 19.",
    "The mandatory controls are a_age_wave1, a_female and a_grade_wave1.",
    "Outcomes marked as review_before_modeling in Script 09b are not estimated as main models.",
    "Models are organized into core, family, school, knowledge/attitudes, peers/relationships, general risk behavior and final parsimonious stages.",
    "Non-core stages with no suggested predictors are skipped rather than estimated redundantly.",
    "Models are estimated using survey-weighted logistic regression with quasibinomial family.",
    "Complete-case analysis is applied within each model specification.",
    "Models with fewer than 100 complete cases or fewer than 10 observations in either outcome cell are skipped.",
    "Predictors with no variation in the model sample are removed from that model.",
    "Odds ratios and Wald confidence intervals are reported for fitted models.",
    "Results are associational and should not be interpreted as causal effects.",
    "AID is used only internally and is excluded from all public outputs.",
    "No individual-level dataset is exported by this script."
  )
)


# ============================================================
# 11. Execution checklist
# ============================================================

script10_checklist <- tibble(
  check_id = 1:22,
  check_item = c(
    "Project root exists",
    "Weighted analytical local-only RDS exists",
    "Script 09b outcome modeling plan exists",
    "Script 09b core controls review exists",
    "Script 09b predictor-outcome eligibility exists",
    "Script 09b recommended model sequence exists",
    "Script 09b sample review exists",
    "Weighted analytical data loaded",
    "AID present internally and excluded from public outputs",
    "GSWGT1 available",
    "Main sample created",
    "Strict sensitivity sample created",
    "Eligible outcomes selected",
    "Excluded outcomes documented",
    "Model specifications created",
    "Weighted logistic models attempted",
    "Model fit summary created",
    "Model coefficients created",
    "Skipped or failed models documented",
    "Excel workbook exported",
    "Markdown documentation exported",
    "No individual-level output exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(weighted_analytical_rds_path), "OK", "FAIL"),
    ifelse(file.exists(script09b_outcome_modeling_plan_path), "OK", "FAIL"),
    ifelse(file.exists(script09b_core_controls_path), "OK", "FAIL"),
    ifelse(file.exists(script09b_predictor_eligibility_path), "OK", "FAIL"),
    ifelse(file.exists(script09b_recommended_model_sequence_path), "OK", "FAIL"),
    ifelse(file.exists(script09b_data_sample_review_path), "OK", "FAIL"),
    "OK",
    ifelse("AID" %in% names(analysis_data), "OK", "FAIL"),
    ifelse("GSWGT1" %in% names(analysis_data), "OK", "FAIL"),
    ifelse(nrow(sample_main) > 0, "OK", "FAIL"),
    ifelse(nrow(sample_strict) > 0, "OK", "FAIL"),
    ifelse(length(eligible_outcomes) > 0, "OK", "FAIL"),
    ifelse(nrow(excluded_outcomes) > 0, "OK", "WARNING_NONE"),
    ifelse(nrow(model_specifications) > 0, "OK", "FAIL"),
    ifelse(length(model_results) > 0, "OK", "FAIL"),
    ifelse(nrow(model_fit_summary) > 0, "OK", "FAIL"),
    ifelse(nrow(model_coefficients) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(skipped_or_failed_models) >= 0, "OK", "FAIL"),
    "PENDING",
    "PENDING",
    "OK"
  )
)


# ============================================================
# 12. Export public CSV outputs
# ============================================================

write_csv(
  model_specifications,
  file.path(outputs_tables_dir, "script10_wave01_model_specifications.csv")
)

write_csv(
  excluded_outcomes,
  file.path(outputs_tables_dir, "script10_wave01_excluded_outcomes.csv")
)

write_csv(
  model_fit_summary,
  file.path(outputs_tables_dir, "script10_wave01_model_fit_summary.csv")
)

write_csv(
  model_coefficients,
  file.path(outputs_tables_dir, "script10_wave01_model_coefficients_odds_ratios.csv")
)

write_csv(
  model_predictor_inventory,
  file.path(outputs_tables_dir, "script10_wave01_model_predictor_inventory.csv")
)

write_csv(
  model_execution_summary,
  file.path(outputs_tables_dir, "script10_wave01_model_execution_summary.csv")
)

write_csv(
  outcome_model_summary,
  file.path(outputs_tables_dir, "script10_wave01_outcome_model_summary.csv")
)

write_csv(
  model_stage_summary,
  file.path(outputs_tables_dir, "script10_wave01_model_stage_summary.csv")
)

write_csv(
  significant_coefficients,
  file.path(outputs_tables_dir, "script10_wave01_significant_coefficients.csv")
)

write_csv(
  odds_ratio_review,
  file.path(outputs_tables_dir, "script10_wave01_odds_ratio_review.csv")
)

write_csv(
  skipped_or_failed_models,
  file.path(outputs_tables_dir, "script10_wave01_skipped_or_failed_models.csv")
)

write_csv(
  predictor_frequency_summary,
  file.path(outputs_tables_dir, "script10_wave01_predictor_frequency_summary.csv")
)

write_csv(
  script10_methodological_notes,
  file.path(outputs_tables_dir, "script10_wave01_methodological_notes.csv")
)


# ============================================================
# 13. Markdown documentation
# ============================================================

script10_doc <- c(
  "# Weighted Logistic Regression Models",
  "",
  "Script 10 estimates weighted logistic regression models for selected Add Health Wave I outcomes.",
  "",
  "## Samples",
  "",
  "- Main sample: students in grades 10 to 12.",
  "- Strict sensitivity sample: students in grades 10 to 12 and ages 15 to 19.",
  "",
  "## Weight",
  "",
  "`GSWGT1` is used as the Wave I population-average sampling weight.",
  "",
  "## Mandatory controls",
  "",
  "- `a_age_wave1`",
  "- `a_female`",
  "- `a_grade_wave1`",
  "",
  "## Model sequence",
  "",
  "- M0: core controls",
  "- M1: family context",
  "- M2: school context",
  "- M3: knowledge, attitudes and perceptions",
  "- M4: peers and relationships",
  "- M5: general risk behaviors",
  "- M6: final parsimonious model",
  "",
  "## Estimator",
  "",
  "Models are estimated with `survey::svyglm()` using a quasibinomial logit specification.",
  "",
  "## Interpretation",
  "",
  "The estimated odds ratios are associational. They should not be interpreted as causal effects.",
  "",
  "## Privacy protection",
  "",
  "`AID` is used only internally and is excluded from all public outputs.",
  "",
  "## Public outputs",
  "",
  "The script exports model specifications, fit diagnostics, coefficients, odds ratios, skipped-model diagnostics and methodological notes.",
  "",
  "## Next step",
  "",
  "Script 11 should review, select and interpret final model results for reporting."
)

writeLines(
  script10_doc,
  con = file.path(docs_dir, "weighted_logistic_regression_models_script10.md")
)

script10_checklist$status[
  script10_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 14. Excel workbook
# ============================================================

script10_checklist$status[
  script10_checklist$check_item == "Excel workbook exported"
] <- "OK"

xlsx_path <- file.path(
  outputs_tables_dir,
  "script10_wave01_weighted_logistic_regression_models.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "model_specifications")
writeData(wb, "model_specifications", model_specifications)

addWorksheet(wb, "excluded_outcomes")
writeData(wb, "excluded_outcomes", excluded_outcomes)

addWorksheet(wb, "fit_summary")
writeData(wb, "fit_summary", model_fit_summary)

addWorksheet(wb, "coefficients_or")
writeData(wb, "coefficients_or", model_coefficients)

addWorksheet(wb, "predictor_inventory")
writeData(wb, "predictor_inventory", model_predictor_inventory)

addWorksheet(wb, "execution_summary")
writeData(wb, "execution_summary", model_execution_summary)

addWorksheet(wb, "outcome_summary")
writeData(wb, "outcome_summary", outcome_model_summary)

addWorksheet(wb, "stage_summary")
writeData(wb, "stage_summary", model_stage_summary)

addWorksheet(wb, "significant_coefficients")
writeData(wb, "significant_coefficients", significant_coefficients)

addWorksheet(wb, "odds_ratio_review")
writeData(wb, "odds_ratio_review", odds_ratio_review)

addWorksheet(wb, "skipped_failed")
writeData(wb, "skipped_failed", skipped_or_failed_models)

addWorksheet(wb, "predictor_frequency")
writeData(wb, "predictor_frequency", predictor_frequency_summary)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script10_methodological_notes)

addWorksheet(wb, "checklist")
writeData(wb, "checklist", script10_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:100, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)


# ============================================================
# 15. Save final checklist
# ============================================================

write_csv(
  script10_checklist,
  file.path(outputs_diag_dir, "script10_execution_checklist.csv")
)


# ============================================================
# 16. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 10 completed: Weighted Logistic Regression Models\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Input local-only weighted analytical file:\n")
cat(weighted_analytical_rds_path, "\n\n")

cat("Sample summary:\n")
cat("- Main grade 10-12 observations: ", nrow(sample_main), "\n", sep = "")
cat("- Strict grade 10-12 and age 15-19 observations: ", nrow(sample_strict), "\n\n", sep = "")

cat("Outcome selection:\n")
cat("- Eligible outcomes modeled: ", length(eligible_outcomes), "\n", sep = "")
cat("- Outcomes excluded or held for review: ", nrow(excluded_outcomes), "\n\n", sep = "")

cat("Model execution summary:\n")
print(model_execution_summary)

cat("\nOutcome model summary:\n")
print(outcome_model_summary)

cat("\nModel stage summary:\n")
print(model_stage_summary)

cat("\nSkipped or failed models:\n")
print(
  skipped_or_failed_models %>%
    select(
      sample_name,
      outcome,
      model_stage,
      fit_status,
      skip_reason,
      n_complete,
      n_outcome_yes,
      n_outcome_no
    ) %>%
    head(50)
)

cat("\nSignificant coefficients preview:\n")
print(
  significant_coefficients %>%
    select(
      sample_name,
      outcome,
      model_stage,
      term,
      odds_ratio,
      conf_low_or,
      conf_high_or,
      p_value,
      significance
    ) %>%
    head(50)
)

cat("\nPublic outputs created:\n")
cat("- outputs/tables/script10_wave01_model_specifications.csv\n")
cat("- outputs/tables/script10_wave01_excluded_outcomes.csv\n")
cat("- outputs/tables/script10_wave01_model_fit_summary.csv\n")
cat("- outputs/tables/script10_wave01_model_coefficients_odds_ratios.csv\n")
cat("- outputs/tables/script10_wave01_model_predictor_inventory.csv\n")
cat("- outputs/tables/script10_wave01_model_execution_summary.csv\n")
cat("- outputs/tables/script10_wave01_outcome_model_summary.csv\n")
cat("- outputs/tables/script10_wave01_model_stage_summary.csv\n")
cat("- outputs/tables/script10_wave01_significant_coefficients.csv\n")
cat("- outputs/tables/script10_wave01_odds_ratio_review.csv\n")
cat("- outputs/tables/script10_wave01_skipped_or_failed_models.csv\n")
cat("- outputs/tables/script10_wave01_predictor_frequency_summary.csv\n")
cat("- outputs/tables/script10_wave01_methodological_notes.csv\n")
cat("- outputs/tables/script10_wave01_weighted_logistic_regression_models.xlsx\n")
cat("- outputs/diagnostics/script10_execution_checklist.csv\n")
cat("- docs/weighted_logistic_regression_models_script10.md\n\n")

cat("Execution checklist:\n")
print(script10_checklist)

cat("\nImportant note:\n")
cat("Do not commit data/raw/, data/processed/, AID-level files or individual-level data to GitHub.\n")
cat("Script 10 results are associational and should be reviewed before reporting.\n\n")