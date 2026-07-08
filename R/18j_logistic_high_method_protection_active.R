# ============================================================
# Script 18j — Logistic High Method Protection Among Sexually Active Adolescents
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Estimate logistic regression models for high method-use protection
#   among adolescents classified as sexually active.
#
# Background:
#   Script 18i estimated a continuous method-use model among sexually
#   active adolescents. It showed that factual knowledge was not
#   significant, while contraceptive/protective self-efficacy was
#   positive and significant.
#
# Dependent variable:
#   high_method_protection = 1 if method_score == 3
#   high_method_protection = 0 if method_score == 1 or 2
#
# Sample:
#   Respondents aged 15–19 classified as sexually active, with valid
#   method-use score and complete covariate/predictor data.
#
# Data protection:
#   The row-level logistic model dataset is LOCAL_ONLY and should not
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
models_dir  <- file.path(project_root, "outputs", "models")
docs_dir    <- file.path(project_root, "docs")

dir.create(indices_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 18j started: Logistic high method protection\n")
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

is_valid_substantive <- function(x) {
  x_num <- extract_numeric_code(x)

  !is.na(x_num) &
    !(x_num %in% c(
      6, 7, 8, 9,
      96, 97, 98, 99,
      996, 997, 998, 999
    ))
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

fit_glm_checked <- function(formula, data, model_name) {

  fit <- stats::glm(
    formula = formula,
    data = data,
    family = stats::binomial(link = "logit")
  )

  if (!is.list(fit) || !inherits(fit, "glm")) {
    stop(
      "Model was not estimated as glm. Model: ",
      model_name,
      ". Class: ",
      paste(class(fit), collapse = ", ")
    )
  }

  if (!isTRUE(fit$converged)) {
    warning("Model did not converge: ", model_name)
  }

  fit
}

glance_glm_manual <- function(fit, model_name) {

  sm <- summary(fit)
  y <- model.response(model.frame(fit))
  p_hat <- stats::fitted(fit)

  brier <- mean((y - p_hat)^2, na.rm = TRUE)

  null_deviance <- fit$null.deviance
  residual_deviance <- fit$deviance

  mcfadden_r2 <- ifelse(
    is.na(null_deviance) || null_deviance == 0,
    NA_real_,
    1 - residual_deviance / null_deviance
  )

  tibble(
    model = model_name,
    n = length(y),
    events = sum(y == 1, na.rm = TRUE),
    non_events = sum(y == 0, na.rm = TRUE),
    event_rate = mean(y == 1, na.rm = TRUE),
    null_deviance = null_deviance,
    residual_deviance = residual_deviance,
    mcfadden_r2 = mcfadden_r2,
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    brier_score = brier,
    df_model = length(stats::coef(fit)) - 1,
    df_residual = stats::df.residual(fit),
    converged = isTRUE(fit$converged)
  )
}

tidy_glm_manual <- function(fit, model_name) {

  sm <- summary(fit)
  coef_df <- as.data.frame(sm$coefficients)

  coef_df %>%
    rownames_to_column("term") %>%
    as_tibble() %>%
    rename(
      log_odds = Estimate,
      std_error = `Std. Error`,
      statistic = `z value`,
      p_value = `Pr(>|z|)`
    ) %>%
    mutate(
      model = model_name,
      conf_low_log_odds = log_odds - 1.96 * std_error,
      conf_high_log_odds = log_odds + 1.96 * std_error,
      odds_ratio = exp(log_odds),
      or_conf_low = exp(conf_low_log_odds),
      or_conf_high = exp(conf_high_log_odds),
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
      model,
      term,
      log_odds,
      std_error,
      statistic,
      p_value,
      odds_ratio,
      or_conf_low,
      or_conf_high,
      conf_low_log_odds,
      conf_high_log_odds,
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

raw_file <- file.path(
  project_root,
  "data/raw/21600-0001-Data.rda"
)

required_files <- c(isx_path, s08_path, s09_path, s19_path, raw_file)

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
# 4. Load raw data and construct sexually active flag
# ------------------------------------------------------------

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

required_sexual_activity_vars <- c(
  "AID",
  "H1CO1",
  "H1CO2Y",
  "H1CO2M",
  "H1CO3",
  "H1CO6",
  "H1CO8",
  "H1CO9",
  "H1CO13"
)

missing_activity_vars <- setdiff(required_sexual_activity_vars, names(raw_df))

if (length(missing_activity_vars) > 0) {
  stop(
    "Missing sexual activity variables in raw file:\n",
    paste(missing_activity_vars, collapse = "\n")
  )
}

sexual_activity <- raw_df %>%
  transmute(
    respondent_id = as.character(AID),

    h1co1_ever_sex_raw = extract_numeric_code(H1CO1),

    downstream_sex_evidence =
      is_valid_substantive(H1CO2Y) |
      is_valid_substantive(H1CO2M) |
      is_valid_substantive(H1CO3) |
      is_valid_substantive(H1CO6) |
      is_valid_substantive(H1CO8) |
      is_valid_substantive(H1CO9) |
      is_valid_substantive(H1CO13),

    ever_had_sex_operational =
      h1co1_ever_sex_raw == 1 |
      downstream_sex_evidence
  )

# ------------------------------------------------------------
# 5. Detect method-use component
# ------------------------------------------------------------

method_candidates <- c(
  "isx3_method_score",
  "isx3_method_score_1_4",
  "isx3_method_protection_score_1_4",
  "isx_method_score_1_4",
  "method_score_1_4",
  "isx3_method"
)

method_col <- safe_col(isx_df, method_candidates)

if (is.null(method_col)) {

  possible_method_cols <- names(isx_df)[
    str_detect(
      names(isx_df),
      regex("isx|method|score|condom|birth", ignore_case = TRUE)
    )
  ]

  stop(
    "Could not detect method-use component column.\nPossible columns:\n",
    paste(possible_method_cols, collapse = ", ")
  )
}

method_outcome <- isx_df %>%
  transmute(
    respondent_id,
    dv_isx_method_score_raw = as.numeric(.data[[method_col]])
  )

# ------------------------------------------------------------
# 6. Covariates
# ------------------------------------------------------------

processed_path <- file.path(
  project_root,
  "data/processed/add_health_wave01_analytical_weighted_local_only.rds"
)

processed_cov <- tibble(respondent_id = character())

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

raw_cov <- raw_df %>%
  transmute(
    respondent_id = as.character(AID),
    sex_num_raw = if ("BIO_SEX" %in% names(raw_df)) extract_numeric_code(BIO_SEX) else NA_real_,
    grade_raw = if ("H1GI20" %in% names(raw_df)) extract_numeric_code(H1GI20) else NA_real_,
    residence_raw = if ("H1IR12" %in% names(raw_df)) extract_numeric_code(H1IR12) else NA_real_
  ) %>%
  mutate(
    residence_zone = case_when(
      residence_raw == 1 ~ "Rural",
      residence_raw == 2 ~ "Suburban",
      residence_raw %in% c(3, 4, 5) ~ "Urban",
      TRUE ~ NA_character_
    )
  )

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
# 7. Predictor blocks
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
# 8. Build sexually active logistic dataset
# ------------------------------------------------------------

model_data_raw <- method_outcome %>%
  left_join(sexual_activity, by = "respondent_id") %>%
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
    age_15_19_flag =
      !is.na(age_wave1_model) &
      age_wave1_model >= 15 &
      age_wave1_model <= 19,

    sex_gender = factor(sex_gender, levels = c("Female", "Male")),

    residence_zone = factor(
      residence_zone,
      levels = c("Rural", "Suburban", "Urban")
    ),

    valid_active_method_score =
      ever_had_sex_operational == TRUE &
      !is.na(dv_isx_method_score_raw) &
      dv_isx_method_score_raw %in% c(1, 2, 3),

    high_method_protection = case_when(
      valid_active_method_score == TRUE & dv_isx_method_score_raw == 3 ~ 1,
      valid_active_method_score == TRUE & dv_isx_method_score_raw %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    )
  )

analysis_vars <- c(
  "high_method_protection",
  "valid_active_method_score",
  "ever_had_sex_operational",
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

sample_flow <- tibble(
  step = c(
    "all_respondents_in_method_file",
    "aged_15_19",
    "aged_15_19_and_ever_sex_operational",
    "aged_15_19_ever_sex_with_method_score_raw",
    "aged_15_19_valid_binary_method_outcome",
    "complete_case_model_n"
  ),
  n = c(
    nrow(model_data_raw),
    nrow(model_data_15_19),
    sum(model_data_15_19$ever_had_sex_operational == TRUE, na.rm = TRUE),
    sum(
      model_data_15_19$ever_had_sex_operational == TRUE &
        !is.na(model_data_15_19$dv_isx_method_score_raw),
      na.rm = TRUE
    ),
    sum(!is.na(model_data_15_19$high_method_protection)),
    NA_integer_
  )
)

variable_availability <- tibble(
  variable = analysis_vars,
  valid_n_15_19_active_context = map_int(
    analysis_vars,
    ~ sum(!is.na(model_data_15_19[[.x]]))
  ),
  missing_n_15_19_active_context = map_int(
    analysis_vars,
    ~ sum(is.na(model_data_15_19[[.x]]))
  ),
  missing_rate_15_19_active_context = round(
    missing_n_15_19_active_context / nrow(model_data_15_19),
    4
  )
)

complete_case_vars <- c(
  "high_method_protection",
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
  stop("Complete-case logistic high method protection dataset has zero rows.")
}

if (length(unique(model_data$high_method_protection)) < 2) {
  stop("The binary outcome has only one observed class in the model dataset.")
}

sample_flow$n[sample_flow$step == "complete_case_model_n"] <- nrow(model_data)

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
  "script18j_logistic_high_method_protection_dataset_LOCAL_ONLY.csv"
)

write_csv(model_data, row_level_path)

# ------------------------------------------------------------
# 9. Outcome distribution
# ------------------------------------------------------------

outcome_distribution_15_19_active <- model_data_15_19 %>%
  filter(!is.na(high_method_protection)) %>%
  count(high_method_protection, name = "n") %>%
  mutate(
    label = ifelse(
      high_method_protection == 1,
      "high_method_protection_score_3",
      "low_or_medium_method_protection_score_1_2"
    ),
    percent = round(100 * n / sum(n), 2)
  ) %>%
  select(label, high_method_protection, n, percent)

outcome_distribution_model_sample <- model_data %>%
  count(high_method_protection, name = "n") %>%
  mutate(
    label = ifelse(
      high_method_protection == 1,
      "high_method_protection_score_3",
      "low_or_medium_method_protection_score_1_2"
    ),
    percent = round(100 * n / sum(n), 2)
  ) %>%
  select(label, high_method_protection, n, percent)

events_n <- sum(model_data$high_method_protection == 1)
non_events_n <- sum(model_data$high_method_protection == 0)

outcome_summary <- tibble(
  metric = c(
    "complete_case_model_n",
    "events_high_method_protection",
    "non_events_low_medium_protection",
    "event_rate",
    "dependent_variable_definition"
  ),
  value = c(
    as.character(nrow(model_data)),
    as.character(events_n),
    as.character(non_events_n),
    as.character(round(events_n / nrow(model_data), 4)),
    "1 = method_score 3; 0 = method_score 1 or 2"
  )
)

# ------------------------------------------------------------
# 10. Logistic model formulas
# ------------------------------------------------------------

formula_m1 <- high_method_protection ~
  sex_gender +
  age_wave1_model +
  grade_wave1_model +
  residence_zone

formula_m2 <- as.formula(paste(
  "high_method_protection ~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
  paste(s08_predictors, collapse = " + ")
))

formula_m3 <- as.formula(paste(
  "high_method_protection ~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
  paste(s08_predictors, collapse = " + "),
  "+",
  paste(s19_predictors, collapse = " + ")
))

formula_m4 <- as.formula(paste(
  "high_method_protection ~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
  paste(s08_predictors, collapse = " + "),
  "+",
  paste(s19_predictors, collapse = " + "),
  "+",
  paste(s09_predictors, collapse = " + ")
))

formula_m4_z <- as.formula(paste(
  "high_method_protection ~ sex_gender + age_wave1_model_z + grade_wave1_model_z + residence_zone +",
  paste(paste0(s08_predictors, "_z"), collapse = " + "),
  "+",
  paste(paste0(s19_predictors, "_z"), collapse = " + "),
  "+",
  paste(paste0(s09_predictors, "_z"), collapse = " + ")
))

# ------------------------------------------------------------
# 11. Estimate logistic models
# ------------------------------------------------------------

m1 <- fit_glm_checked(formula_m1, model_data, "M1_covariates")
m2 <- fit_glm_checked(formula_m2, model_data, "M2_covariates_perceptions")
m3 <- fit_glm_checked(formula_m3, model_data, "M3_add_knowledge")
m4 <- fit_glm_checked(formula_m4, model_data, "M4_add_self_efficacy")
m4_z <- fit_glm_checked(formula_m4_z, model_data, "M4_standardized_continuous")

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

model_classes <- tibble(
  model = names(model_list),
  object_class = map_chr(model_list, ~ paste(class(.x), collapse = ", ")),
  is_glm = map_lgl(model_list, ~ is.list(.x) && inherits(.x, "glm")),
  converged = map_lgl(model_list, ~ isTRUE(.x$converged)),
  n = map_int(model_list, ~ length(.x[["residuals"]]))
)

if (any(!model_classes$is_glm)) {
  stop("At least one model object is not a glm object.")
}

model_fit <- bind_rows(lapply(names(model_list), function(nm) {
  glance_glm_manual(model_list[[nm]], nm)
}))

model_coefficients <- bind_rows(lapply(names(model_list), function(nm) {
  tidy_glm_manual(model_list[[nm]], nm)
}))

incremental_fit <- model_fit %>%
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
    delta_mcfadden_r2 = mcfadden_r2 - lag(mcfadden_r2),
    delta_residual_deviance = residual_deviance - lag(residual_deviance),
    delta_aic = aic - lag(aic),
    delta_bic = bic - lag(bic)
  )

final_model_vif <- compute_vif_manual(m4)

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
  arrange(term)

key_standardized_coefficients <- model_coefficients %>%
  filter(
    model == "M4_standardized_continuous",
    term != "(Intercept)"
  ) %>%
  arrange(term)

knowledge_selfeff_summary <- model_coefficients %>%
  filter(
    model == "M4_add_self_efficacy",
    term %in% c(
      "s19_knowledge_correct_count_0_10",
      "s09_contraceptive_self_efficacy_index_1_5"
    )
  )

# ------------------------------------------------------------
# 12. Predicted probabilities from final model
# ------------------------------------------------------------

model_data <- model_data %>%
  mutate(
    predicted_probability_m4 = stats::predict(
      m4,
      newdata = model_data,
      type = "response"
    )
  )

prediction_summary <- tibble(
  metric = c(
    "mean_predicted_probability",
    "sd_predicted_probability",
    "min_predicted_probability",
    "max_predicted_probability",
    "brier_score_m4"
  ),
  value = c(
    round(mean(model_data$predicted_probability_m4, na.rm = TRUE), 4),
    round(sd(model_data$predicted_probability_m4, na.rm = TRUE), 4),
    round(min(model_data$predicted_probability_m4, na.rm = TRUE), 4),
    round(max(model_data$predicted_probability_m4, na.rm = TRUE), 4),
    round(model_fit$brier_score[model_fit$model == "M4_add_self_efficacy"], 4)
  )
)

predictor_correlation_matrix <- model_data %>%
  select(
    high_method_protection,
    all_of(continuous_predictors)
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable")

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Purpose", "This script estimates logistic models for high method-use protection among sexually active adolescents.",
  "Dependent variable", "The dependent variable equals 1 when method_score is 3 and 0 when method_score is 1 or 2.",
  "Sample restriction", "The sample is restricted to respondents aged 15–19 classified as sexually active with valid method-use scores.",
  "Model sequence", "Models are estimated incrementally: covariates, perceptions/barriers, knowledge and self-efficacy.",
  "Estimator", "The estimator is logistic regression using glm with binomial logit link.",
  "Interpretation", "Exponentiated coefficients are odds ratios for high method-use protection.",
  "Main diagnostic purpose", "The model tests whether self-efficacy remains important and whether knowledge remains non-significant in a binary high-protection outcome.",
  "Data protection", "The row-level logistic model dataset is LOCAL_ONLY and should not be committed to GitHub."
)

# ------------------------------------------------------------
# 13. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  docs_dir,
  "add_health_wave01_logistic_high_method_protection_script18j.docx"
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
      "Add Health Wave I — Logistic High Method Protection",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18j estimates logistic regression models for high method-use protection among adolescents classified as sexually active.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Sample flow", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(sample_flow)) %>%
    officer::body_add_par("Outcome distribution, model sample", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(outcome_distribution_model_sample)) %>%
    officer::body_add_par("Outcome summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(outcome_summary)) %>%
    officer::body_add_par("Model fit", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(model_fit)) %>%
    officer::body_add_par("Incremental fit", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(incremental_fit)) %>%
    officer::body_add_par("Knowledge and self-efficacy odds ratios", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(knowledge_selfeff_summary)) %>%
    officer::body_add_par("Key full-model odds ratios", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(key_coefficients)) %>%
    officer::body_add_par("Prediction summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(prediction_summary)) %>%
    officer::body_add_par("VIF", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(final_model_vif)) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(methodological_decisions))

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 14. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "isx_file_loaded",
    "raw_file_loaded",
    "section8_file_loaded",
    "section9_file_loaded",
    "section19_file_loaded",
    "method_component_detected",
    "sexually_active_flag_created",
    "binary_outcome_created",
    "covariates_constructed",
    "complete_case_n_positive",
    "binary_outcome_has_two_classes",
    "m1_estimated",
    "m2_estimated",
    "m3_estimated",
    "m4_estimated",
    "all_models_converged",
    "model_fit_created",
    "model_coefficients_created",
    "incremental_fit_created",
    "knowledge_selfeff_summary_created",
    "vif_created",
    "prediction_summary_created",
    "word_report_created",
    "ready_for_logistic_high_method_review"
  ),
  status = c(
    file.exists(isx_path),
    file.exists(raw_file),
    file.exists(s08_path),
    file.exists(s09_path),
    file.exists(s19_path),
    !is.null(method_col),
    nrow(sexual_activity) > 0,
    sum(!is.na(model_data$high_method_protection)) > 0,
    nrow(covariates) > 0,
    nrow(model_data) > 0,
    length(unique(model_data$high_method_protection)) == 2,
    inherits(m1, "glm"),
    inherits(m2, "glm"),
    inherits(m3, "glm"),
    inherits(m4, "glm"),
    all(model_classes$converged),
    nrow(model_fit) > 0,
    nrow(model_coefficients) > 0,
    nrow(incremental_fit) > 0,
    nrow(knowledge_selfeff_summary) > 0,
    nrow(final_model_vif) > 0,
    nrow(prediction_summary) > 0,
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

# ------------------------------------------------------------
# 15. Write outputs
# ------------------------------------------------------------

write_csv(
  final_status,
  file.path(models_dir, "script18j_final_status.csv")
)

write_csv(
  sample_flow,
  file.path(models_dir, "script18j_sample_flow.csv")
)

write_csv(
  variable_availability,
  file.path(models_dir, "script18j_variable_availability.csv")
)

write_csv(
  outcome_distribution_15_19_active,
  file.path(models_dir, "script18j_outcome_distribution_15_19_active.csv")
)

write_csv(
  outcome_distribution_model_sample,
  file.path(models_dir, "script18j_outcome_distribution_model_sample.csv")
)

write_csv(
  outcome_summary,
  file.path(models_dir, "script18j_outcome_summary.csv")
)

write_csv(
  model_classes,
  file.path(models_dir, "script18j_model_object_classes.csv")
)

write_csv(
  model_fit,
  file.path(models_dir, "script18j_model_fit.csv")
)

write_csv(
  model_coefficients,
  file.path(models_dir, "script18j_model_coefficients_or.csv")
)

write_csv(
  incremental_fit,
  file.path(models_dir, "script18j_incremental_fit.csv")
)

write_csv(
  key_coefficients,
  file.path(models_dir, "script18j_key_coefficients_m4_or.csv")
)

write_csv(
  key_standardized_coefficients,
  file.path(models_dir, "script18j_key_standardized_coefficients_m4z_or.csv")
)

write_csv(
  knowledge_selfeff_summary,
  file.path(models_dir, "script18j_knowledge_selfeff_summary_or.csv")
)

write_csv(
  prediction_summary,
  file.path(models_dir, "script18j_prediction_summary.csv")
)

write_csv(
  final_model_vif,
  file.path(models_dir, "script18j_vif.csv")
)

write_csv(
  predictor_correlation_matrix,
  file.path(models_dir, "script18j_predictor_correlation_matrix.csv")
)

write_csv(
  methodological_decisions,
  file.path(models_dir, "script18j_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 16. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18j completed: Logistic high method protection\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status, n = Inf)

cat("\nSample flow:\n")
print(sample_flow, n = Inf)

cat("\nVariable availability, ages 15–19:\n")
print(variable_availability, n = Inf)

cat("\nOutcome distribution, aged 15–19 active valid outcome:\n")
print(outcome_distribution_15_19_active, n = Inf)

cat("\nOutcome distribution, model sample:\n")
print(outcome_distribution_model_sample, n = Inf)

cat("\nOutcome summary:\n")
print(outcome_summary, n = Inf)

cat("\nModel classes:\n")
print(model_classes, n = Inf)

cat("\nModel fit:\n")
print(model_fit, n = Inf)

cat("\nIncremental fit:\n")
print(incremental_fit, n = Inf)

cat("\nKnowledge and self-efficacy odds ratios:\n")
print(knowledge_selfeff_summary, n = Inf)

cat("\nKey full-model odds ratios:\n")
print(key_coefficients, n = Inf)

cat("\nStandardized full-model odds ratios:\n")
print(key_standardized_coefficients, n = Inf)

cat("\nPrediction summary:\n")
print(prediction_summary, n = Inf)

cat("\nVIF:\n")
print(final_model_vif, n = Inf)

cat("\nPredictor correlation matrix:\n")
print(as.data.frame(predictor_correlation_matrix))

cat("\nMethodological decisions:\n")
print(methodological_decisions, n = Inf)

cat("\nOutputs created in:\n")
cat(models_dir, "\n")

if (!is.na(word_report_path)) {
  cat("\nWord report:\n")
  cat(word_report_path, "\n")
}

cat("\nImportant Git note:\n")
cat("Do not commit script18j_logistic_high_method_protection_dataset_LOCAL_ONLY.csv\n")
cat("Do not commit yet. This remains an exploratory modelling stage.\n")