# ============================================================
# Script 18h — Dependent Variable Decomposition, Continuous Models
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Decompose the restricted ISX dependent variable into:
#     1. total restricted ISX index
#     2. timing / abstinence component
#     3. method-use / contraception-condom component
#
# Rationale:
#   Script 18g found an unexpected negative coefficient for factual
#   knowledge in the total ISX model. This script checks whether that
#   association is driven by the timing/abstinence component or by the
#   method-use component.
#
# Outcomes:
#   dv_isx_index_1_4
#   dv_isx_timing_1_4
#   dv_isx_method_1_4
#
# Model sequence:
#   M1: covariates
#   M2: covariates + perceptions/barriers
#   M3: M2 + knowledge
#   M4: M3 + self-efficacy
#   M4_z: standardized continuous predictors
#
# Data protection:
#   The row-level decomposition dataset is LOCAL_ONLY and should not
#   be committed to GitHub.
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
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

has_officer <- requireNamespace("officer", quietly = TRUE)
has_flextable <- requireNamespace("flextable", quietly = TRUE)

if (has_officer) suppressPackageStartupMessages(library(officer))
if (has_flextable) suppressPackageStartupMessages(library(flextable))

# ------------------------------------------------------------
# 1. Project root and folders
# ------------------------------------------------------------

project_root <- "C:/Users/LENOVO/GitHub/add-health-adolescent-risk-models"

if (!dir.exists(project_root)) {
  stop("Project root not found: ", project_root)
}

setwd(project_root)

indices_dir <- file.path(project_root, "outputs", "indices")
models_dir <- file.path(project_root, "outputs", "models")
docs_dir <- file.path(project_root, "docs")

dir.create(indices_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 18h started: Dependent Variable Decomposition\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

extract_numeric_code <- function(x) {
  suppressWarnings(
    as.numeric(stringr::str_extract(as.character(x), "-?\\d+(\\.\\d+)?"))
  )
}

safe_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NULL)
  hit[1]
}

z_score <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)

  if (is.na(s) || s == 0) {
    return(rep(NA_real_, length(x)))
  }

  (x - m) / s
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  sd(x)
}

fit_lm_checked <- function(formula, data, model_name) {

  fit <- stats::lm(formula, data = data)

  if (!is.list(fit) || !inherits(fit, "lm")) {
    stop(
      "Model was not estimated as lm. Model: ",
      model_name,
      ". Class: ",
      paste(class(fit), collapse = ", ")
    )
  }

  fit
}

glance_lm_manual <- function(fit, model_name, outcome) {

  if (!is.list(fit) || !inherits(fit, "lm")) {
    stop(
      "Non-lm object received by glance_lm_manual. Model: ",
      model_name,
      ". Class: ",
      paste(class(fit), collapse = ", ")
    )
  }

  sm <- summary(fit)
  resid <- fit[["residuals"]]

  tibble(
    outcome = outcome,
    model = model_name,
    n = length(resid),
    r_squared = unname(sm$r.squared),
    adj_r_squared = unname(sm$adj.r.squared),
    sigma = unname(sm$sigma),
    rmse = sqrt(mean(resid^2, na.rm = TRUE)),
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    df_model = length(stats::coef(fit)) - 1,
    df_residual = stats::df.residual(fit)
  )
}

tidy_lm_manual <- function(fit, model_name, outcome) {

  if (!is.list(fit) || !inherits(fit, "lm")) {
    stop(
      "Non-lm object received by tidy_lm_manual. Model: ",
      model_name,
      ". Class: ",
      paste(class(fit), collapse = ", ")
    )
  }

  sm <- summary(fit)
  coef_df <- as.data.frame(sm$coefficients)

  coef_df %>%
    rownames_to_column("term") %>%
    as_tibble() %>%
    rename(
      estimate = Estimate,
      std_error = `Std. Error`,
      statistic = `t value`,
      p_value = `Pr(>|t|)`
    ) %>%
    mutate(
      outcome = outcome,
      model = model_name,
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error,
      stars = case_when(
        is.na(p_value) ~ "",
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        p_value < 0.10 ~ ".",
        TRUE ~ ""
      )
    ) %>%
    select(
      outcome,
      model,
      term,
      estimate,
      std_error,
      statistic,
      p_value,
      conf_low,
      conf_high,
      stars
    )
}

compute_vif_manual <- function(fit) {

  X <- model.matrix(fit)

  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }

  if (ncol(X) < 2) {
    return(tibble(term = colnames(X), vif = NA_real_))
  }

  bind_rows(lapply(colnames(X), function(v) {

    y <- X[, v]
    others <- X[, setdiff(colnames(X), v), drop = FALSE]

    if (sd(y, na.rm = TRUE) == 0) {
      return(tibble(term = v, vif = NA_real_))
    }

    aux <- stats::lm(y ~ others)
    r2 <- summary(aux)$r.squared

    vif <- ifelse(
      is.na(r2) || r2 >= 1,
      NA_real_,
      1 / (1 - r2)
    )

    tibble(term = v, vif = vif)
  })) %>%
    arrange(desc(vif))
}

# ------------------------------------------------------------
# 3. Input files
# ------------------------------------------------------------

isx_path <- file.path(
  indices_dir,
  "script18b_restricted_isx_sexual_protection_index_LOCAL_ONLY.csv"
)

s08_path <- file.path(
  indices_dir,
  "script18d_s08_perceived_risk_predictors_LOCAL_ONLY.csv"
)

s09_path <- file.path(
  indices_dir,
  "script18e_s09_self_efficacy_predictors_LOCAL_ONLY.csv"
)

s19_path <- file.path(
  indices_dir,
  "script18f_s19_knowledge_predictors_LOCAL_ONLY.csv"
)

required_files <- c(isx_path, s08_path, s09_path, s19_path)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing input files:\n", paste(missing_files, collapse = "\n"))
}

isx_df <- read_csv(isx_path, show_col_types = FALSE) %>%
  mutate(respondent_id = as.character(respondent_id))

s08_df <- read_csv(s08_path, show_col_types = FALSE) %>%
  mutate(respondent_id = as.character(respondent_id))

s09_df <- read_csv(s09_path, show_col_types = FALSE) %>%
  mutate(respondent_id = as.character(respondent_id))

s19_df <- read_csv(s19_path, show_col_types = FALSE) %>%
  mutate(respondent_id = as.character(respondent_id))

# ------------------------------------------------------------
# 4. Detect ISX outcome and component columns
# ------------------------------------------------------------

index_candidates <- c(
  "restricted_isx_index_primary_1_4",
  "restricted_isx_primary_index_1_4",
  "primary_index_1_4",
  "isx_index_primary_1_4",
  "restricted_isx_sexual_protection_index_primary_1_4"
)

index_0_1_candidates <- c(
  "restricted_isx_index_primary_0_1",
  "restricted_isx_primary_index_0_1",
  "primary_index_0_1",
  "isx_index_primary_0_1",
  "restricted_isx_sexual_protection_index_primary_0_1"
)

timing_candidates <- c(
  "isx1_timing_score",
  "isx1_timing_score_1_4",
  "isx1_timing_protection_score_1_4",
  "isx_timing_score_1_4",
  "timing_score_1_4",
  "isx1_timing"
)

method_candidates <- c(
  "isx3_method_score",
  "isx3_method_score_1_4",
  "isx3_method_protection_score_1_4",
  "isx_method_score_1_4",
  "method_score_1_4",
  "isx3_method"
)

index_col <- safe_col(isx_df, index_candidates)
index_0_1_col <- safe_col(isx_df, index_0_1_candidates)
timing_col <- safe_col(isx_df, timing_candidates)
method_col <- safe_col(isx_df, method_candidates)

if (is.null(index_col) && is.null(index_0_1_col)) {
  possible_cols <- names(isx_df)[
    str_detect(
      names(isx_df),
      regex("isx|primary|protection|index", ignore_case = TRUE)
    )
  ]

  stop(
    "Could not detect total ISX index column.\nPossible columns:\n",
    paste(possible_cols, collapse = ", ")
  )
}

if (is.null(timing_col) || is.null(method_col)) {
  possible_component_cols <- names(isx_df)[
    str_detect(
      names(isx_df),
      regex("isx|timing|method|score|component", ignore_case = TRUE)
    )
  ]

  stop(
    "Could not detect timing or method-use component columns.\n",
    "Detected timing column: ", ifelse(is.null(timing_col), "NULL", timing_col), "\n",
    "Detected method column: ", ifelse(is.null(method_col), "NULL", method_col), "\n",
    "Possible columns:\n",
    paste(possible_component_cols, collapse = ", ")
  )
}

if (!is.null(index_col)) {
  isx_outcomes <- isx_df %>%
    transmute(
      respondent_id,
      dv_isx_index_1_4 = as.numeric(.data[[index_col]]),
      dv_isx_timing_1_4 = as.numeric(.data[[timing_col]]),
      dv_isx_method_1_4 = as.numeric(.data[[method_col]])
    )

  index_source <- index_col

} else {
  isx_outcomes <- isx_df %>%
    transmute(
      respondent_id,
      dv_isx_index_1_4 = 1 + 3 * as.numeric(.data[[index_0_1_col]]),
      dv_isx_timing_1_4 = as.numeric(.data[[timing_col]]),
      dv_isx_method_1_4 = as.numeric(.data[[method_col]])
    )

  index_source <- paste0(index_0_1_col, " converted to 1–4")
}

component_sources <- tibble(
  component = c(
    "total_restricted_isx_index",
    "timing_abstinence_component",
    "method_use_component"
  ),
  detected_column = c(
    index_source,
    timing_col,
    method_col
  )
)

# ------------------------------------------------------------
# 5. Covariates
# ------------------------------------------------------------

processed_path <- file.path(
  project_root,
  "data/processed/add_health_wave01_analytical_weighted_local_only.rds"
)

raw_file <- file.path(
  project_root,
  "data/raw/21600-0001-Data.rda"
)

processed_cov <- tibble(respondent_id = character())
raw_cov <- tibble(respondent_id = character())

if (file.exists(processed_path)) {

  processed_df <- as_tibble(readRDS(processed_path))

  id_col <- safe_col(processed_df, c("respondent_id", "AID", "aid"))

  if (!is.null(id_col)) {

    sex_col <- safe_col(processed_df, c("BIO_SEX", "bio_sex", "sex", "gender"))
    age_col <- safe_col(processed_df, c("age_wave1", "a_age_wave1", "AGE", "age"))
    grade_col <- safe_col(processed_df, c("grade_wave1", "a_grade_wave1", "H1GI20", "grade"))
    weight_col <- safe_col(processed_df, c("survey_weight", "GSWGT1", "weight", "weight_wave1"))

    processed_cov <- tibble(
      respondent_id = as.character(processed_df[[id_col]]),
      sex_num_processed = if (!is.null(sex_col)) extract_numeric_code(processed_df[[sex_col]]) else NA_real_,
      age_processed = if (!is.null(age_col)) extract_numeric_code(processed_df[[age_col]]) else NA_real_,
      grade_processed = if (!is.null(grade_col)) extract_numeric_code(processed_df[[grade_col]]) else NA_real_,
      weight_processed = if (!is.null(weight_col)) extract_numeric_code(processed_df[[weight_col]]) else NA_real_
    )
  }
}

if (file.exists(raw_file)) {

  env <- new.env(parent = emptyenv())
  loaded_objects <- load(raw_file, envir = env)

  raw_object_name <- loaded_objects[
    vapply(
      loaded_objects,
      function(nm) is.data.frame(get(nm, envir = env)),
      logical(1)
    )
  ][1]

  raw_df <- as_tibble(get(raw_object_name, envir = env))

  raw_cov <- tibble(
    respondent_id = as.character(raw_df$AID),
    sex_num_raw = if ("BIO_SEX" %in% names(raw_df)) extract_numeric_code(raw_df$BIO_SEX) else NA_real_,
    grade_raw = if ("H1GI20" %in% names(raw_df)) extract_numeric_code(raw_df$H1GI20) else NA_real_,
    residence_raw = if ("H1IR12" %in% names(raw_df)) extract_numeric_code(raw_df$H1IR12) else NA_real_
  ) %>%
    mutate(
      residence_zone = case_when(
        residence_raw == 1 ~ "Rural",
        residence_raw == 2 ~ "Suburban",
        residence_raw %in% c(3, 4, 5) ~ "Urban",
        TRUE ~ NA_character_
      )
    )
}

isx_context <- tibble(
  respondent_id = isx_df$respondent_id,
  age_from_isx = if ("age_wave1" %in% names(isx_df)) as.numeric(isx_df$age_wave1) else NA_real_,
  weight_from_isx = if ("survey_weight" %in% names(isx_df)) as.numeric(isx_df$survey_weight) else NA_real_
)

covariates <- isx_context %>%
  left_join(processed_cov, by = "respondent_id") %>%
  left_join(raw_cov, by = "respondent_id") %>%
  mutate(
    age_wave1_model = coalesce(age_from_isx, age_processed),
    survey_weight_model = coalesce(weight_from_isx, weight_processed),
    sex_num = coalesce(sex_num_processed, sex_num_raw),
    sex_gender = case_when(
      sex_num == 1 ~ "Male",
      sex_num == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    grade_wave1_model = coalesce(grade_processed, grade_raw),
    grade_wave1_model = ifelse(
      grade_wave1_model %in% c(96, 97, 98, 99),
      NA_real_,
      grade_wave1_model
    )
  ) %>%
  select(
    respondent_id,
    age_wave1_model,
    grade_wave1_model,
    sex_gender,
    residence_zone,
    survey_weight_model
  )

# ------------------------------------------------------------
# 6. Predictor blocks
# ------------------------------------------------------------

s08_predictors <- c(
  "s08_pregnancy_consequence_severity_index_1_5",
  "s08_aids_consequence_severity_item_1_5",
  "s08_std_protection_barrier_item_1_5",
  "s08_pregnancy_susceptibility_item_1_5",
  "s08_aids_susceptibility_item_1_5"
)

s19_predictors <- c(
  "s19_knowledge_correct_count_0_10"
)

s09_predictors <- c(
  "s09_contraceptive_self_efficacy_index_1_5"
)

missing_predictors <- c(
  setdiff(s08_predictors, names(s08_df)),
  setdiff(s19_predictors, names(s19_df)),
  setdiff(s09_predictors, names(s09_df))
)

if (length(missing_predictors) > 0) {
  stop("Missing predictor columns:\n", paste(missing_predictors, collapse = "\n"))
}

# ------------------------------------------------------------
# 7. Build common decomposition dataset
# ------------------------------------------------------------

model_data_raw <- isx_outcomes %>%
  left_join(covariates, by = "respondent_id") %>%
  left_join(
    s08_df %>% select(respondent_id, all_of(s08_predictors)),
    by = "respondent_id"
  ) %>%
  left_join(
    s19_df %>% select(respondent_id, all_of(s19_predictors)),
    by = "respondent_id"
  ) %>%
  left_join(
    s09_df %>% select(respondent_id, all_of(s09_predictors)),
    by = "respondent_id"
  ) %>%
  mutate(
    age_15_19_flag = !is.na(age_wave1_model) &
      age_wave1_model >= 15 &
      age_wave1_model <= 19,
    sex_gender = factor(sex_gender, levels = c("Female", "Male")),
    residence_zone = factor(
      residence_zone,
      levels = c("Rural", "Suburban", "Urban")
    )
  )

outcome_vars <- c(
  "dv_isx_index_1_4",
  "dv_isx_timing_1_4",
  "dv_isx_method_1_4"
)

analysis_vars <- c(
  outcome_vars,
  "sex_gender",
  "age_wave1_model",
  "grade_wave1_model",
  "residence_zone",
  "survey_weight_model",
  s08_predictors,
  s19_predictors,
  s09_predictors
)

model_data_15_19 <- model_data_raw %>%
  filter(age_15_19_flag == TRUE)

variable_availability <- tibble(
  variable = analysis_vars,
  valid_n_15_19 = map_int(
    analysis_vars,
    ~ sum(!is.na(model_data_15_19[[.x]]))
  ),
  missing_n_15_19 = map_int(
    analysis_vars,
    ~ sum(is.na(model_data_15_19[[.x]]))
  ),
  missing_rate_15_19 = round(
    missing_n_15_19 / nrow(model_data_15_19),
    4
  )
)

complete_case_vars <- c(
  outcome_vars,
  "sex_gender",
  "age_wave1_model",
  "grade_wave1_model",
  "residence_zone",
  s08_predictors,
  s19_predictors,
  s09_predictors
)

model_data <- model_data_15_19 %>%
  filter(if_all(all_of(complete_case_vars), ~ !is.na(.x)))

if (nrow(model_data) == 0) {
  stop("Complete-case decomposition dataset has zero rows.")
}

continuous_predictors <- c(
  "age_wave1_model",
  "grade_wave1_model",
  s08_predictors,
  s19_predictors,
  s09_predictors
)

model_data <- model_data %>%
  mutate(
    across(
      all_of(continuous_predictors),
      z_score,
      .names = "{.col}_z"
    )
  )

row_level_path <- file.path(
  models_dir,
  "script18h_decomposition_model_dataset_LOCAL_ONLY.csv"
)

write_csv(model_data, row_level_path)

# ------------------------------------------------------------
# 8. Fit models for each outcome
# ------------------------------------------------------------

fit_outcome_models <- function(outcome_var, outcome_label, data) {

  formula_m1 <- as.formula(paste(
    outcome_var,
    "~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone"
  ))

  formula_m2 <- as.formula(paste(
    outcome_var,
    "~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
    paste(s08_predictors, collapse = " + ")
  ))

  formula_m3 <- as.formula(paste(
    outcome_var,
    "~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
    paste(s08_predictors, collapse = " + "),
    "+",
    paste(s19_predictors, collapse = " + ")
  ))

  formula_m4 <- as.formula(paste(
    outcome_var,
    "~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
    paste(s08_predictors, collapse = " + "),
    "+",
    paste(s19_predictors, collapse = " + "),
    "+",
    paste(s09_predictors, collapse = " + ")
  ))

  formula_m4_z <- as.formula(paste(
    outcome_var,
    "~ sex_gender + age_wave1_model_z + grade_wave1_model_z + residence_zone +",
    paste(paste0(s08_predictors, "_z"), collapse = " + "),
    "+",
    paste(paste0(s19_predictors, "_z"), collapse = " + "),
    "+",
    paste(paste0(s09_predictors, "_z"), collapse = " + ")
  ))

  m1 <- fit_lm_checked(formula_m1, data, paste0(outcome_label, "_M1"))
  m2 <- fit_lm_checked(formula_m2, data, paste0(outcome_label, "_M2"))
  m3 <- fit_lm_checked(formula_m3, data, paste0(outcome_label, "_M3"))
  m4 <- fit_lm_checked(formula_m4, data, paste0(outcome_label, "_M4"))
  m4_z <- fit_lm_checked(formula_m4_z, data, paste0(outcome_label, "_M4_z"))

  model_list <- setNames(
    list(m1, m2, m3, m4, m4_z),
    c(
      "M1_covariates",
      "M2_covariates_perceptions",
      "M3_add_knowledge",
      "M4_add_self_efficacy",
      "M4_standardized_continuous"
    )
  )

  model_fit <- bind_rows(lapply(names(model_list), function(nm) {
    glance_lm_manual(model_list[[nm]], nm, outcome_label)
  }))

  model_coefficients <- bind_rows(lapply(names(model_list), function(nm) {
    tidy_lm_manual(model_list[[nm]], nm, outcome_label)
  }))

  incremental_r2 <- model_fit %>%
    filter(model != "M4_standardized_continuous") %>%
    arrange(match(
      model,
      c(
        "M1_covariates",
        "M2_covariates_perceptions",
        "M3_add_knowledge",
        "M4_add_self_efficacy"
      )
    )) %>%
    mutate(
      delta_r_squared = r_squared - lag(r_squared),
      delta_adj_r_squared = adj_r_squared - lag(adj_r_squared)
    )

  list(
    fit = model_fit,
    coefficients = model_coefficients,
    incremental_r2 = incremental_r2,
    vif = compute_vif_manual(m4)
  )
}

outcome_map <- tibble::tribble(
  ~outcome_var, ~outcome_label,
  "dv_isx_index_1_4", "total_restricted_isx_index",
  "dv_isx_timing_1_4", "timing_abstinence_component",
  "dv_isx_method_1_4", "method_use_component"
)

outcome_results <- purrr::pmap(
  outcome_map,
  function(outcome_var, outcome_label) {
    fit_outcome_models(outcome_var, outcome_label, model_data)
  }
)

model_fit <- bind_rows(lapply(outcome_results, `[[`, "fit"))

model_coefficients <- bind_rows(lapply(outcome_results, `[[`, "coefficients"))

incremental_r2 <- bind_rows(lapply(outcome_results, `[[`, "incremental_r2"))

vif_output <- bind_rows(
  lapply(seq_along(outcome_results), function(i) {
    outcome_results[[i]][["vif"]] %>%
      mutate(outcome = outcome_map$outcome_label[i])
  })
) %>%
  select(outcome, term, vif)

# ------------------------------------------------------------
# 9. Key coefficient comparison
# ------------------------------------------------------------

key_terms <- c(
  "age_wave1_model",
  "grade_wave1_model",
  "sex_genderMale",
  "residence_zoneSuburban",
  "residence_zoneUrban",
  s08_predictors,
  s19_predictors,
  s09_predictors
)

key_coefficients <- model_coefficients %>%
  filter(
    model == "M4_add_self_efficacy",
    term %in% key_terms
  ) %>%
  arrange(term, outcome)

key_standardized_coefficients <- model_coefficients %>%
  filter(
    model == "M4_standardized_continuous",
    term != "(Intercept)"
  ) %>%
  arrange(term, outcome)

knowledge_comparison <- model_coefficients %>%
  filter(
    model == "M4_add_self_efficacy",
    term == "s19_knowledge_correct_count_0_10"
  ) %>%
  select(
    outcome,
    model,
    term,
    estimate,
    std_error,
    statistic,
    p_value,
    conf_low,
    conf_high,
    stars
  )

# ------------------------------------------------------------
# 10. Descriptive summaries
# ------------------------------------------------------------

outcome_summary <- bind_rows(lapply(outcome_vars, function(v) {
  tibble(
    outcome = v,
    valid_n = sum(!is.na(model_data[[v]])),
    mean = safe_mean(model_data[[v]]),
    sd = safe_sd(model_data[[v]]),
    min = min(model_data[[v]], na.rm = TRUE),
    max = max(model_data[[v]], na.rm = TRUE)
  )
}))

component_correlation_matrix <- model_data %>%
  select(all_of(c(outcome_vars, continuous_predictors))) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable")

model_sample_summary <- tibble(
  metric = c(
    "respondents_in_isx_file",
    "respondents_aged_15_19",
    "common_complete_case_n",
    "index_source",
    "timing_component_source",
    "method_component_source"
  ),
  value = c(
    as.character(nrow(isx_df)),
    as.character(nrow(model_data_15_19)),
    as.character(nrow(model_data)),
    index_source,
    timing_col,
    method_col
  )
)

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Purpose", "The script decomposes the total restricted ISX index into timing/abstinence and method-use components.",
  "Main reason", "The decomposition checks whether the unexpected negative knowledge coefficient is driven by the timing/abstinence component or by the method-use component.",
  "Scale", "All dependent variables are modelled on the original 1–4 scale.",
  "Estimator", "The models use unweighted OLS.",
  "Sample", "A common complete-case sample is used across the total index, timing component and method-use component to improve comparability.",
  "Interpretation warning", "Because never-sex respondents receive high protection scores on both components, component models may still partly reflect abstinence/timing.",
  "Next possible step", "If needed, a sexually active-only method-use model can be estimated later.",
  "Data protection", "The row-level decomposition dataset is LOCAL_ONLY and should not be committed to GitHub."
)

# ------------------------------------------------------------
# 11. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  docs_dir,
  "add_health_wave01_dependent_variable_decomposition_script18h.docx"
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

  doc <- officer::read_docx()

  doc <- doc %>%
    officer::body_add_par(
      "Add Health Wave I — Dependent Variable Decomposition",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18h decomposes the restricted ISX sexual protection outcome into total index, timing/abstinence component and method-use component.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Model sample summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(model_sample_summary)) %>%
    officer::body_add_par("Component sources", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(component_sources)) %>%
    officer::body_add_par("Outcome summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(outcome_summary)) %>%
    officer::body_add_par("Model fit", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(model_fit)) %>%
    officer::body_add_par("Incremental R-squared", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(incremental_r2)) %>%
    officer::body_add_par("Knowledge coefficient comparison", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(knowledge_comparison)) %>%
    officer::body_add_par("Key full-model coefficients", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(key_coefficients)) %>%
    officer::body_add_par("VIF", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(vif_output)) %>%
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
    "isx_file_loaded",
    "section8_file_loaded",
    "section9_file_loaded",
    "section19_file_loaded",
    "total_index_detected",
    "timing_component_detected",
    "method_component_detected",
    "covariates_constructed",
    "common_complete_case_n_positive",
    "models_estimated_for_total_index",
    "models_estimated_for_timing_component",
    "models_estimated_for_method_component",
    "model_fit_created",
    "model_coefficients_created",
    "incremental_r2_created",
    "knowledge_comparison_created",
    "vif_created",
    "word_report_created",
    "ready_for_decomposition_review"
  ),
  status = c(
    file.exists(isx_path),
    file.exists(s08_path),
    file.exists(s09_path),
    file.exists(s19_path),
    !is.null(index_col) || !is.null(index_0_1_col),
    !is.null(timing_col),
    !is.null(method_col),
    nrow(covariates) > 0,
    nrow(model_data) > 0,
    any(model_fit$outcome == "total_restricted_isx_index"),
    any(model_fit$outcome == "timing_abstinence_component"),
    any(model_fit$outcome == "method_use_component"),
    nrow(model_fit) > 0,
    nrow(model_coefficients) > 0,
    nrow(incremental_r2) > 0,
    nrow(knowledge_comparison) > 0,
    nrow(vif_output) > 0,
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

# ------------------------------------------------------------
# 13. Write outputs
# ------------------------------------------------------------

write_csv(
  final_status,
  file.path(models_dir, "script18h_final_status.csv")
)

write_csv(
  component_sources,
  file.path(models_dir, "script18h_component_sources.csv")
)

write_csv(
  variable_availability,
  file.path(models_dir, "script18h_variable_availability.csv")
)

write_csv(
  model_sample_summary,
  file.path(models_dir, "script18h_model_sample_summary.csv")
)

write_csv(
  outcome_summary,
  file.path(models_dir, "script18h_outcome_summary.csv")
)

write_csv(
  model_fit,
  file.path(models_dir, "script18h_model_fit.csv")
)

write_csv(
  model_coefficients,
  file.path(models_dir, "script18h_model_coefficients.csv")
)

write_csv(
  incremental_r2,
  file.path(models_dir, "script18h_incremental_r2.csv")
)

write_csv(
  key_coefficients,
  file.path(models_dir, "script18h_key_coefficients_m4.csv")
)

write_csv(
  key_standardized_coefficients,
  file.path(models_dir, "script18h_key_standardized_coefficients_m4z.csv")
)

write_csv(
  knowledge_comparison,
  file.path(models_dir, "script18h_knowledge_coefficient_comparison.csv")
)

write_csv(
  vif_output,
  file.path(models_dir, "script18h_vif.csv")
)

write_csv(
  component_correlation_matrix,
  file.path(models_dir, "script18h_component_correlation_matrix.csv")
)

write_csv(
  methodological_decisions,
  file.path(models_dir, "script18h_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 14. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18h completed: Dependent Variable Decomposition\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status, n = Inf)

cat("\nComponent sources:\n")
print(component_sources, n = Inf)

cat("\nModel sample summary:\n")
print(model_sample_summary, n = Inf)

cat("\nOutcome summary:\n")
print(outcome_summary, n = Inf)

cat("\nVariable availability, ages 15–19:\n")
print(variable_availability, n = Inf)

cat("\nModel fit:\n")
print(model_fit, n = Inf)

cat("\nIncremental R-squared:\n")
print(incremental_r2, n = Inf)

cat("\nKnowledge coefficient comparison:\n")
print(knowledge_comparison, n = Inf)

cat("\nKey full-model coefficients:\n")
print(key_coefficients, n = Inf)

cat("\nStandardized full-model coefficients:\n")
print(key_standardized_coefficients, n = Inf)

cat("\nVIF:\n")
print(vif_output, n = Inf)

cat("\nComponent correlation matrix:\n")
print(as.data.frame(component_correlation_matrix))

cat("\nMethodological decisions:\n")
print(methodological_decisions, n = Inf)

cat("\nOutputs created in:\n")
cat(models_dir, "\n")

if (!is.na(word_report_path)) {
  cat("\nWord report:\n")
  cat(word_report_path, "\n")
}

cat("\nImportant Git note:\n")
cat("Do not commit script18h_decomposition_model_dataset_LOCAL_ONLY.csv\n")
cat("Do not commit yet. This is still an exploratory modelling stage.\n")