# ============================================================
# Script 18g v2 — Continuous Incremental Regression Main Model
# Dependent variable: restricted ISX protection index, 1–4 scale
# ============================================================

rm(list = ls())

required_packages <- c("dplyr", "tibble", "readr", "stringr", "tidyr", "purrr")

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

project_root <- "C:/Users/LENOVO/GitHub/add-health-adolescent-risk-models"
setwd(project_root)

indices_dir <- file.path(project_root, "outputs", "indices")
models_dir  <- file.path(project_root, "outputs", "models")
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 18g v2 started: Continuous Incremental Regression\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

extract_numeric_code <- function(x) {
  suppressWarnings(as.numeric(stringr::str_extract(as.character(x), "-?\\d+(\\.\\d+)?")))
}

safe_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NULL)
  hit[1]
}

z_score <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
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

glance_lm_manual <- function(fit, model_name) {
  
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
  
  aic_value <- stats::AIC(fit)
  bic_value <- stats::BIC(fit)
  
  tibble(
    model = model_name,
    n = length(resid),
    r_squared = unname(sm$r.squared),
    adj_r_squared = unname(sm$adj.r.squared),
    sigma = unname(sm$sigma),
    rmse = sqrt(mean(resid^2, na.rm = TRUE)),
    aic = aic_value,
    bic = bic_value,
    df_model = length(stats::coef(fit)) - 1,
    df_residual = stats::df.residual(fit)
  )
}

tidy_lm_manual <- function(model, model_name) {
  if (!is.list(model) || !inherits(model, "lm")) {
    stop(
      "Non-lm object received by tidy_lm_manual. Model: ",
      model_name,
      ". Class: ",
      paste(class(model), collapse = ", ")
    )
  }

  sm <- summary(model)
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
    select(model, term, estimate, std_error, statistic, p_value, conf_low, conf_high, stars)
}

compute_vif_manual <- function(model) {
  X <- model.matrix(model)

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
    vif <- ifelse(is.na(r2) || r2 >= 1, NA_real_, 1 / (1 - r2))

    tibble(term = v, vif = vif)
  })) %>%
    arrange(desc(vif))
}

# ------------------------------------------------------------
# Input files
# ------------------------------------------------------------

isx_path <- file.path(indices_dir, "script18b_restricted_isx_sexual_protection_index_LOCAL_ONLY.csv")
s08_path <- file.path(indices_dir, "script18d_s08_perceived_risk_predictors_LOCAL_ONLY.csv")
s09_path <- file.path(indices_dir, "script18e_s09_self_efficacy_predictors_LOCAL_ONLY.csv")
s19_path <- file.path(indices_dir, "script18f_s19_knowledge_predictors_LOCAL_ONLY.csv")

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
# Detect dependent variable: main continuous 1–4
# ------------------------------------------------------------

dv_1_4_candidates <- c(
  "restricted_isx_index_primary_1_4",
  "restricted_isx_primary_index_1_4",
  "primary_index_1_4",
  "isx_index_primary_1_4",
  "restricted_isx_sexual_protection_index_primary_1_4"
)

dv_0_1_candidates <- c(
  "restricted_isx_index_primary_0_1",
  "restricted_isx_primary_index_0_1",
  "primary_index_0_1",
  "isx_index_primary_0_1",
  "restricted_isx_sexual_protection_index_primary_0_1"
)

dv_1_4_col <- safe_col(isx_df, dv_1_4_candidates)
dv_0_1_col <- safe_col(isx_df, dv_0_1_candidates)

if (is.null(dv_1_4_col) && is.null(dv_0_1_col)) {
  possible_dv <- names(isx_df)[
    str_detect(names(isx_df), regex("isx|primary|protection|index", ignore_case = TRUE))
  ]

  stop(
    "Dependent variable not detected. Possible columns:\n",
    paste(possible_dv, collapse = ", ")
  )
}

if (!is.null(dv_1_4_col)) {
  isx_model <- isx_df %>%
    mutate(dv_isx_protection_1_4 = as.numeric(.data[[dv_1_4_col]]))

  dv_source <- dv_1_4_col
} else {
  isx_model <- isx_df %>%
    mutate(dv_isx_protection_1_4 = 1 + 3 * as.numeric(.data[[dv_0_1_col]]))

  dv_source <- paste0(dv_0_1_col, " converted to 1–4")
}

# ------------------------------------------------------------
# Covariates: age, sex, grade, residence, weight
# ------------------------------------------------------------

processed_path <- file.path(project_root, "data/processed/add_health_wave01_analytical_weighted_local_only.rds")
raw_file <- file.path(project_root, "data/raw/21600-0001-Data.rda")

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
    vapply(loaded_objects, function(nm) is.data.frame(get(nm, envir = env)), logical(1))
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
  respondent_id = isx_model$respondent_id,
  age_from_isx = if ("age_wave1" %in% names(isx_model)) as.numeric(isx_model$age_wave1) else NA_real_,
  weight_from_isx = if ("survey_weight" %in% names(isx_model)) as.numeric(isx_model$survey_weight) else NA_real_
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
    grade_wave1_model = ifelse(grade_wave1_model %in% c(96, 97, 98, 99), NA_real_, grade_wave1_model)
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
# Predictors
# ------------------------------------------------------------

s08_predictors <- c(
  "s08_pregnancy_consequence_severity_index_1_5",
  "s08_aids_consequence_severity_item_1_5",
  "s08_std_protection_barrier_item_1_5",
  "s08_pregnancy_susceptibility_item_1_5",
  "s08_aids_susceptibility_item_1_5"
)

s09_predictors <- c(
  "s09_contraceptive_self_efficacy_index_1_5"
)

s19_predictors <- c(
  "s19_knowledge_correct_count_0_10"
)

missing_predictors <- c(
  setdiff(s08_predictors, names(s08_df)),
  setdiff(s09_predictors, names(s09_df)),
  setdiff(s19_predictors, names(s19_df))
)

if (length(missing_predictors) > 0) {
  stop("Missing predictor columns:\n", paste(missing_predictors, collapse = "\n"))
}

# ------------------------------------------------------------
# Build model dataset
# ------------------------------------------------------------

model_data_raw <- isx_model %>%
  select(respondent_id, dv_isx_protection_1_4) %>%
  left_join(covariates, by = "respondent_id") %>%
  left_join(s08_df %>% select(respondent_id, all_of(s08_predictors)), by = "respondent_id") %>%
  left_join(s09_df %>% select(respondent_id, all_of(s09_predictors)), by = "respondent_id") %>%
  left_join(s19_df %>% select(respondent_id, all_of(s19_predictors)), by = "respondent_id") %>%
  mutate(
    age_15_19_flag = !is.na(age_wave1_model) & age_wave1_model >= 15 & age_wave1_model <= 19,
    sex_gender = factor(sex_gender, levels = c("Female", "Male")),
    residence_zone = factor(residence_zone, levels = c("Rural", "Suburban", "Urban"))
  )

analysis_vars <- c(
  "dv_isx_protection_1_4",
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
  valid_n_15_19 = map_int(analysis_vars, ~ sum(!is.na(model_data_15_19[[.x]]))),
  missing_n_15_19 = map_int(analysis_vars, ~ sum(is.na(model_data_15_19[[.x]]))),
  missing_rate_15_19 = round(missing_n_15_19 / nrow(model_data_15_19), 4)
)

complete_case_vars <- c(
  "dv_isx_protection_1_4",
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
  stop("Complete-case model dataset has zero rows.")
}

continuous_predictors <- c(
  "age_wave1_model",
  "grade_wave1_model",
  s08_predictors,
  s19_predictors,
  s09_predictors
)

model_data <- model_data %>%
  mutate(across(all_of(continuous_predictors), z_score, .names = "{.col}_z"))

row_level_path <- file.path(models_dir, "script18g_v2_continuous_model_dataset_LOCAL_ONLY.csv")
write_csv(model_data, row_level_path)

# ------------------------------------------------------------
# Formulas
# ------------------------------------------------------------

formula_m1 <- dv_isx_protection_1_4 ~
  sex_gender +
  age_wave1_model +
  grade_wave1_model +
  residence_zone

formula_m2 <- as.formula(paste(
  "dv_isx_protection_1_4 ~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
  paste(s08_predictors, collapse = " + ")
))

formula_m3 <- as.formula(paste(
  "dv_isx_protection_1_4 ~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
  paste(s08_predictors, collapse = " + "),
  "+",
  paste(s19_predictors, collapse = " + ")
))

formula_m4 <- as.formula(paste(
  "dv_isx_protection_1_4 ~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
  paste(s08_predictors, collapse = " + "),
  "+",
  paste(s19_predictors, collapse = " + "),
  "+",
  paste(s09_predictors, collapse = " + ")
))

formula_m4_z <- as.formula(paste(
  "dv_isx_protection_1_4 ~ sex_gender + age_wave1_model_z + grade_wave1_model_z + residence_zone +",
  paste(paste0(s08_predictors, "_z"), collapse = " + "),
  "+",
  paste(paste0(s19_predictors, "_z"), collapse = " + "),
  "+",
  paste(paste0(s09_predictors, "_z"), collapse = " + ")
))

# ------------------------------------------------------------
# Estimate models
# ------------------------------------------------------------

m1 <- fit_lm_checked(formula_m1, model_data, "M1_covariates")
m2 <- fit_lm_checked(formula_m2, model_data, "M2_covariates_perceptions")
m3 <- fit_lm_checked(formula_m3, model_data, "M3_add_knowledge")
m4 <- fit_lm_checked(formula_m4, model_data, "M4_add_self_efficacy")
m4_z <- fit_lm_checked(formula_m4_z, model_data, "M4_standardized_continuous")

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
  is_lm = map_lgl(model_list, ~ is.list(.x) && inherits(.x, "lm")),
  n = map_int(model_list, ~ length(.x[["residuals"]]))
)

print(model_classes, n = Inf)

if (any(!model_classes$is_lm)) {
  stop("At least one model object is not an lm object.")
}

model_fit <- bind_rows(lapply(names(model_list), function(nm) {
  glance_lm_manual(model_list[[nm]], nm)
}))

model_coefficients <- bind_rows(lapply(names(model_list), function(nm) {
  tidy_lm_manual(model_list[[nm]], nm)
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

final_model_vif <- compute_vif_manual(m4)

predictor_correlation_matrix <- model_data %>%
  select(dv_isx_protection_1_4, all_of(continuous_predictors)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable")

model_sample_summary <- tibble(
  metric = c(
    "dv_source",
    "respondents_in_isx_file",
    "respondents_aged_15_19",
    "complete_case_model_n",
    "dv_mean_1_4",
    "dv_sd_1_4",
    "dv_min_1_4",
    "dv_max_1_4"
  ),
  value = c(
    dv_source,
    as.character(nrow(isx_model)),
    as.character(nrow(model_data_15_19)),
    as.character(nrow(model_data)),
    as.character(round(safe_mean(model_data$dv_isx_protection_1_4), 4)),
    as.character(round(safe_sd(model_data$dv_isx_protection_1_4), 4)),
    as.character(round(min(model_data$dv_isx_protection_1_4, na.rm = TRUE), 4)),
    as.character(round(max(model_data$dv_isx_protection_1_4, na.rm = TRUE), 4))
  )
)

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Dependent variable", "The main dependent variable is the restricted ISX sexual protection index on the original 1–4 continuous scale.",
  "Estimator", "The main estimator is unweighted OLS.",
  "Incremental modelling", "Models are estimated incrementally: covariates, perceptions/barriers, knowledge and self-efficacy.",
  "Sample", "The analytic sample is restricted to respondents aged 15–19 with complete data on the final model variables.",
  "Expected signs", "Severity, susceptibility, knowledge and self-efficacy are expected to be positive. Perceived barriers are expected to be negative.",
  "Data protection", "The row-level model dataset is LOCAL_ONLY and should not be committed to GitHub."
)

final_status <- tibble(
  check = c(
    "isx_file_loaded",
    "section8_file_loaded",
    "section9_file_loaded",
    "section19_file_loaded",
    "dependent_variable_1_4_detected_or_converted",
    "covariates_constructed",
    "complete_case_n_positive",
    "m1_estimated",
    "m2_estimated",
    "m3_estimated",
    "m4_estimated",
    "model_fit_created",
    "model_coefficients_created",
    "incremental_r2_created",
    "vif_created",
    "ready_for_review"
  ),
  status = c(
    file.exists(isx_path),
    file.exists(s08_path),
    file.exists(s09_path),
    file.exists(s19_path),
    !is.na(dv_source),
    nrow(covariates) > 0,
    nrow(model_data) > 0,
    inherits(m1, "lm"),
    inherits(m2, "lm"),
    inherits(m3, "lm"),
    inherits(m4, "lm"),
    nrow(model_fit) > 0,
    nrow(model_coefficients) > 0,
    nrow(incremental_r2) > 0,
    nrow(final_model_vif) > 0,
    TRUE
  )
)

# ------------------------------------------------------------
# Write outputs
# ------------------------------------------------------------

write_csv(variable_availability, file.path(models_dir, "script18g_v2_continuous_variable_availability.csv"))
write_csv(model_sample_summary, file.path(models_dir, "script18g_v2_continuous_model_sample_summary.csv"))
write_csv(model_classes, file.path(models_dir, "script18g_v2_model_object_classes.csv"))
write_csv(model_fit, file.path(models_dir, "script18g_v2_continuous_model_fit.csv"))
write_csv(model_coefficients, file.path(models_dir, "script18g_v2_continuous_model_coefficients.csv"))
write_csv(incremental_r2, file.path(models_dir, "script18g_v2_continuous_model_incremental_r2.csv"))
write_csv(final_model_vif, file.path(models_dir, "script18g_v2_continuous_final_model_vif.csv"))
write_csv(predictor_correlation_matrix, file.path(models_dir, "script18g_v2_continuous_predictor_correlation_matrix.csv"))
write_csv(methodological_decisions, file.path(models_dir, "script18g_v2_methodological_decisions.csv"))
write_csv(final_status, file.path(models_dir, "script18g_v2_final_status.csv"))

# ------------------------------------------------------------
# Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18g v2 completed: Continuous Incremental Regression\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status, n = Inf)

cat("\nModel object classes:\n")
print(model_classes, n = Inf)

cat("\nModel sample summary:\n")
print(model_sample_summary, n = Inf)

cat("\nVariable availability, ages 15–19:\n")
print(variable_availability, n = Inf)

cat("\nModel fit:\n")
print(model_fit, n = Inf)

cat("\nIncremental R-squared:\n")
print(incremental_r2, n = Inf)

cat("\nModel coefficients:\n")
print(model_coefficients, n = Inf)

cat("\nFinal model VIF:\n")
print(final_model_vif, n = Inf)

cat("\nPredictor correlation matrix:\n")
print(as.data.frame(predictor_correlation_matrix))

cat("\nMethodological decisions:\n")
print(methodological_decisions, n = Inf)

cat("\nOutputs created in:\n")
cat(models_dir, "\n")

cat("\nImportant Git note:\n")
cat("Do not commit script18g_v2_continuous_model_dataset_LOCAL_ONLY.csv\n")