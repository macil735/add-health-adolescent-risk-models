# ============================================================
# Script 18i — Method-Use Model Among Sexually Active Adolescents
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Estimate continuous method-use protection models restricted to
#   adolescents classified as sexually active.
#
# Rationale:
#   Scripts 18g and 18h found a negative coefficient for factual
#   knowledge in the total ISX protection index. Script 18h showed
#   that this pattern was stronger for timing/abstinence but also
#   present for method-use. This script removes respondents classified
#   as never sexually active and models method-use protection only.
#
# Dependent variable:
#   isx3_method_score among sexually active adolescents only.
#
# Interpretation:
#   Higher values = greater contraceptive/condom method protection.
#
# Data protection:
#   The row-level dataset is LOCAL_ONLY and should not be committed.
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
cat("Script 18i started: Method-use among sexually active adolescents\n")
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

  sm <- summary(fit)
  resid <- fit[["residuals"]]

  tibble(
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

tidy_lm_manual <- function(fit, model_name) {

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
# 8. Build sexually active method-use dataset
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

    # In Script 18b, method score 4 represents the protection score
    # assigned to respondents classified as not sexually active.
    # Here the model is restricted to sexually active adolescents,
    # so method score 4 is excluded from the active method-use outcome.
    dv_method_active_1_3 = ifelse(
      ever_had_sex_operational == TRUE &
        !is.na(dv_isx_method_score_raw) &
        dv_isx_method_score_raw %in% c(1, 2, 3),
      dv_isx_method_score_raw,
      NA_real_
    )
  )

analysis_vars <- c(
  "dv_method_active_1_3",
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
    "aged_15_19_ever_sex_with_method_score_1_to_3",
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
    sum(!is.na(model_data_15_19$dv_method_active_1_3)),
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
  "dv_method_active_1_3",
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
  stop("Complete-case sexually active method-use dataset has zero rows.")
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
  "script18i_method_use_active_model_dataset_LOCAL_ONLY.csv"
)

write_csv(model_data, row_level_path)

# ------------------------------------------------------------
# 9. Descriptive summaries
# ------------------------------------------------------------

method_score_distribution <- model_data_15_19 %>%
  filter(ever_had_sex_operational == TRUE) %>%
  count(dv_isx_method_score_raw, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  arrange(dv_isx_method_score_raw)

outcome_summary <- tibble(
  outcome = "dv_method_active_1_3",
  valid_n = sum(!is.na(model_data$dv_method_active_1_3)),
  mean = safe_mean(model_data$dv_method_active_1_3),
  sd = safe_sd(model_data$dv_method_active_1_3),
  min = min(model_data$dv_method_active_1_3, na.rm = TRUE),
  max = max(model_data$dv_method_active_1_3, na.rm = TRUE)
)

# ------------------------------------------------------------
# 10. Model formulas
# ------------------------------------------------------------

formula_m1 <- dv_method_active_1_3 ~
  sex_gender +
  age_wave1_model +
  grade_wave1_model +
  residence_zone

formula_m2 <- as.formula(paste(
  "dv_method_active_1_3 ~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
  paste(s08_predictors, collapse = " + ")
))

formula_m3 <- as.formula(paste(
  "dv_method_active_1_3 ~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
  paste(s08_predictors, collapse = " + "),
  "+",
  paste(s19_predictors, collapse = " + ")
))

formula_m4 <- as.formula(paste(
  "dv_method_active_1_3 ~ sex_gender + age_wave1_model + grade_wave1_model + residence_zone +",
  paste(s08_predictors, collapse = " + "),
  "+",
  paste(s19_predictors, collapse = " + "),
  "+",
  paste(s09_predictors, collapse = " + ")
))

formula_m4_z <- as.formula(paste(
  "dv_method_active_1_3 ~ sex_gender + age_wave1_model_z + grade_wave1_model_z + residence_zone +",
  paste(paste0(s08_predictors, "_z"), collapse = " + "),
  "+",
  paste(paste0(s19_predictors, "_z"), collapse = " + "),
  "+",
  paste(paste0(s09_predictors, "_z"), collapse = " + ")
))

# ------------------------------------------------------------
# 11. Estimate models
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

predictor_correlation_matrix <- model_data %>%
  select(
    dv_method_active_1_3,
    all_of(continuous_predictors)
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable")

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Purpose", "This script models method-use protection among adolescents classified as sexually active.",
  "Sexually active definition", "Sexually active status is defined using H1CO1 == 1 or downstream sexual behavior evidence from H1CO2Y, H1CO2M, H1CO3, H1CO6, H1CO8, H1CO9 or H1CO13.",
  "Dependent variable", "The dependent variable is isx3_method_score restricted to sexually active respondents and values 1–3.",
  "Estimator", "The models use unweighted OLS as a continuous exploratory model.",
  "Interpretation", "Higher dependent-variable values indicate greater method-use protection among sexually active adolescents.",
  "Main diagnostic purpose", "The model tests whether factual knowledge remains negatively associated with method-use after excluding respondents classified as never sexually active.",
  "Data protection", "The row-level active method-use model dataset is LOCAL_ONLY and should not be committed to GitHub."
)

# ------------------------------------------------------------
# 12. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  docs_dir,
  "add_health_wave01_method_use_sexually_active_script18i.docx"
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
      "Add Health Wave I — Method-Use Model Among Sexually Active Adolescents",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18i restricts the analytic sample to respondents classified as sexually active and models the method-use protection component.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Sample flow", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(sample_flow)) %>%
    officer::body_add_par("Method score distribution among sexually active respondents", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(method_score_distribution)) %>%
    officer::body_add_par("Outcome summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(outcome_summary)) %>%
    officer::body_add_par("Model fit", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(model_fit)) %>%
    officer::body_add_par("Incremental R-squared", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(incremental_r2)) %>%
    officer::body_add_par("Knowledge and self-efficacy coefficients", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(knowledge_selfeff_summary)) %>%
    officer::body_add_par("Key full-model coefficients", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(key_coefficients)) %>%
    officer::body_add_par("VIF", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(final_model_vif)) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(methodological_decisions))

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 13. Final status
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
    "covariates_constructed",
    "complete_case_n_positive",
    "m1_estimated",
    "m2_estimated",
    "m3_estimated",
    "m4_estimated",
    "model_fit_created",
    "model_coefficients_created",
    "incremental_r2_created",
    "knowledge_selfeff_summary_created",
    "vif_created",
    "word_report_created",
    "ready_for_active_method_use_review"
  ),
  status = c(
    file.exists(isx_path),
    file.exists(raw_file),
    file.exists(s08_path),
    file.exists(s09_path),
    file.exists(s19_path),
    !is.null(method_col),
    nrow(sexual_activity) > 0,
    nrow(covariates) > 0,
    nrow(model_data) > 0,
    inherits(m1, "lm"),
    inherits(m2, "lm"),
    inherits(m3, "lm"),
    inherits(m4, "lm"),
    nrow(model_fit) > 0,
    nrow(model_coefficients) > 0,
    nrow(incremental_r2) > 0,
    nrow(knowledge_selfeff_summary) > 0,
    nrow(final_model_vif) > 0,
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

# ------------------------------------------------------------
# 14. Write outputs
# ------------------------------------------------------------

write_csv(
  final_status,
  file.path(models_dir, "script18i_final_status.csv")
)

write_csv(
  sample_flow,
  file.path(models_dir, "script18i_sample_flow.csv")
)

write_csv(
  variable_availability,
  file.path(models_dir, "script18i_variable_availability.csv")
)

write_csv(
  method_score_distribution,
  file.path(models_dir, "script18i_method_score_distribution_active.csv")
)

write_csv(
  outcome_summary,
  file.path(models_dir, "script18i_outcome_summary.csv")
)

write_csv(
  model_classes,
  file.path(models_dir, "script18i_model_object_classes.csv")
)

write_csv(
  model_fit,
  file.path(models_dir, "script18i_model_fit.csv")
)

write_csv(
  model_coefficients,
  file.path(models_dir, "script18i_model_coefficients.csv")
)

write_csv(
  incremental_r2,
  file.path(models_dir, "script18i_incremental_r2.csv")
)

write_csv(
  key_coefficients,
  file.path(models_dir, "script18i_key_coefficients_m4.csv")
)

write_csv(
  key_standardized_coefficients,
  file.path(models_dir, "script18i_key_standardized_coefficients_m4z.csv")
)

write_csv(
  knowledge_selfeff_summary,
  file.path(models_dir, "script18i_knowledge_selfeff_summary.csv")
)

write_csv(
  final_model_vif,
  file.path(models_dir, "script18i_vif.csv")
)

write_csv(
  predictor_correlation_matrix,
  file.path(models_dir, "script18i_predictor_correlation_matrix.csv")
)

write_csv(
  methodological_decisions,
  file.path(models_dir, "script18i_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 15. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18i completed: Method-use among sexually active adolescents\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status, n = Inf)

cat("\nSample flow:\n")
print(sample_flow, n = Inf)

cat("\nVariable availability, ages 15–19:\n")
print(variable_availability, n = Inf)

cat("\nMethod score distribution among sexually active respondents:\n")
print(method_score_distribution, n = Inf)

cat("\nOutcome summary:\n")
print(outcome_summary, n = Inf)

cat("\nModel classes:\n")
print(model_classes, n = Inf)

cat("\nModel fit:\n")
print(model_fit, n = Inf)

cat("\nIncremental R-squared:\n")
print(incremental_r2, n = Inf)

cat("\nKnowledge and self-efficacy summary:\n")
print(knowledge_selfeff_summary, n = Inf)

cat("\nKey full-model coefficients:\n")
print(key_coefficients, n = Inf)

cat("\nStandardized full-model coefficients:\n")
print(key_standardized_coefficients, n = Inf)

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
cat("Do not commit script18i_method_use_active_model_dataset_LOCAL_ONLY.csv\n")
cat("Do not commit yet. This remains an exploratory modelling stage.\n")