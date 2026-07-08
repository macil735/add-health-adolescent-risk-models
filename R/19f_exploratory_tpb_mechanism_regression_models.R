# ============================================================
# Script 19f
# Exploratory TPB-Compatible Mechanism Regression Models
# Project: Add Health Adolescent Risk Models
# ============================================================
#
# Purpose:
#   Estimate exploratory regression models linking family and
#   friend connectedness to TPB-compatible psychosocial outcomes
#   among adolescents aged 15–19 who have not yet had sexual
#   intercourse.
#
# Important:
#   These are NOT full TPB mediation models.
#
#   Full TPB mediation was classified as not ready in Script 19e
#   because:
#     1. no direct intention-to-delay outcome was confirmed;
#     2. perceived behavioral control / self-efficacy was not
#        operationally confirmed;
#     3. the delay-orientation proxy would create circularity if
#        used as a mediation outcome.
#
# Models:
#   M1: tpb_attitudes_delay_mean_1_5
#   M2: peer_norm_delay_H1MO1
#   M3: partner_norm_delay_H1MO2
#   M4: maternal_norm_delay_H1MO4
#
# Predictors:
#   family_connectedness_mean_1_5
#   friend_support_mean_1_5
#
# Covariates, if detected:
#   age
#   sex/gender
#   school grade
#   urban/rural residence
#   survey weight
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

script_id <- "19f"
script_title <- "Exploratory TPB-Compatible Mechanism Regression Models"
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

input_19e_dataset <- "outputs/analysis/script19e_final_tpb_operationalization_dataset.rds"
input_19e_readiness <- "outputs/audits/script19e_mediation_readiness_assessment.csv"
input_19e_decision_matrix <- "outputs/audits/script19e_tpb_operational_decision_matrix.csv"
input_19e_recommended_models <- "outputs/tables/script19e_recommended_model_sequence.csv"
raw_path <- "data/raw/21600-0001-Data.rda"
weight_path <- "data/raw/21600-0004-Data.rda"

analysis_dataset_path <- "outputs/analysis/script19f_exploratory_tpb_mechanism_regression_dataset.rds"
covariate_audit_path <- "outputs/audits/script19f_covariate_detection_audit.csv"
model_sample_audit_path <- "outputs/audits/script19f_model_sample_audit.csv"
model_spec_path <- "outputs/audits/script19f_model_specifications.csv"
coefs_path <- "outputs/tables/script19f_regression_coefficients.csv"
fit_path <- "outputs/tables/script19f_model_fit_statistics.csv"
std_coefs_path <- "outputs/tables/script19f_standardized_regression_coefficients.csv"
interpretation_path <- "outputs/reports/script19f_interpretation_notes.md"
docx_path <- "outputs/reports/script19f_exploratory_tpb_mechanism_regression_models.docx"
log_path <- "outputs/logs/script19f_run_log.txt"

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
  input_19e_dataset,
  input_19e_readiness,
  input_19e_decision_matrix,
  input_19e_recommended_models
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing required input file(s). Run Script 19e first. Missing: ",
    paste(missing_inputs, collapse = ", "),
    call. = FALSE
  )
}

if (!file.exists(raw_path)) {
  stop(
    "Raw Add Health file not found: ",
    raw_path,
    call. = FALSE
  )
}

tpb_df <- readRDS(input_19e_dataset)
readiness <- read_csv(input_19e_readiness, show_col_types = FALSE)
decision_matrix <- read_csv(input_19e_decision_matrix, show_col_types = FALSE)
recommended_models <- read_csv(input_19e_recommended_models, show_col_types = FALSE)

log_line("Loaded 19e final operationalization dataset: ", input_19e_dataset)
log_line("Rows in 19e dataset: ", nrow(tpb_df))
log_line("Columns in 19e dataset: ", ncol(tpb_df))

# ------------------------------------------------------------
# 4. Helper functions
# ------------------------------------------------------------

std_key <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", x))
}

find_var <- function(df, aliases, patterns = NULL) {
  nms <- names(df)
  nms_key <- std_key(nms)
  alias_key <- std_key(aliases)
  
  exact_idx <- match(alias_key, nms_key)
  exact_idx <- exact_idx[!is.na(exact_idx)]
  
  if (length(exact_idx) > 0) {
    return(nms[exact_idx[1]])
  }
  
  if (!is.null(patterns)) {
    pattern_hit <- nms[
      str_detect(tolower(nms), paste(patterns, collapse = "|"))
    ]
    
    if (length(pattern_hit) > 0) {
      return(pattern_hit[1])
    }
  }
  
  NA_character_
}

extract_first_number <- function(x) {
  if (inherits(x, "haven_labelled")) {
    raw <- suppressWarnings(as.numeric(haven::zap_labels(x)))
    
    if (sum(!is.na(raw)) > 0) {
      return(raw)
    }
    
    x <- as.character(haven::as_factor(x, levels = "labels"))
  } else {
    x <- as.character(x)
  }
  
  out <- stringr::str_extract(x, "\\d+")
  suppressWarnings(as.numeric(out))
}

to_numeric_survey <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  
  if (is.factor(x)) {
    return(extract_first_number(x))
  }
  
  suppressWarnings(as.numeric(x))
}

recode_ever_sex <- function(x) {
  out <- rep(NA_real_, length(x))
  
  if (inherits(x, "haven_labelled")) {
    labels_chr <- tolower(as.character(haven::as_factor(x, levels = "labels")))
    
    out[str_detect(labels_chr, "\\byes\\b|sim")] <- 1
    out[str_detect(labels_chr, "\\bno\\b|nao|não|never")] <- 0
  }
  
  if (is.factor(x) || is.character(x)) {
    ch <- tolower(as.character(x))
    
    out[str_detect(ch, "\\byes\\b|sim")] <- 1
    out[str_detect(ch, "\\bno\\b|nao|não|never")] <- 0
  }
  
  num <- to_numeric_survey(x)
  valid_vals <- sort(unique(num[!is.na(num)]))
  
  mapped <- rep(NA_real_, length(num))
  
  if (length(valid_vals) > 0 && all(valid_vals %in% c(0, 1))) {
    mapped[num == 1] <- 1
    mapped[num == 0] <- 0
  }
  
  if (length(valid_vals) > 0 && all(valid_vals %in% c(1, 2))) {
    mapped[num == 1] <- 1
    mapped[num == 2] <- 0
  }
  
  out[is.na(out)] <- mapped[is.na(out)]
  out
}

compute_wave1_age_from_dates <- function(df) {
  bmonth_var <- find_var(df, c("H1GI1M", "h1gi1m", "H1GILM", "h1gilm"))
  byear_var  <- find_var(df, c("H1GI1Y", "h1gi1y", "H1GILY", "h1gily"))
  imonth_var <- find_var(df, c("IMONTH", "imonth"))
  iday_var   <- find_var(df, c("IDAY", "iday"))
  iyear_var  <- find_var(df, c("IYEAR", "iyear"))
  
  required_vars <- c(bmonth_var, byear_var, imonth_var, iday_var, iyear_var)
  
  if (any(is.na(required_vars))) {
    return(list(
      age = rep(NA_real_, nrow(df)),
      source = "age_not_constructed_missing_date_variables",
      variables = paste(required_vars, collapse = ", ")
    ))
  }
  
  clean_month <- function(x) {
    x <- extract_first_number(x)
    x[x %in% c(96, 97, 98, 99)] <- NA_real_
    x[x < 1 | x > 12] <- NA_real_
    x
  }
  
  clean_day <- function(x) {
    x <- extract_first_number(x)
    x[x %in% c(96, 97, 98, 99)] <- NA_real_
    x[x < 1 | x > 31] <- NA_real_
    x
  }
  
  clean_birth_year <- function(x) {
    x <- extract_first_number(x)
    x[x %in% c(96, 97, 98, 99, 9996, 9997, 9998, 9999)] <- NA_real_
    
    x <- dplyr::case_when(
      is.na(x) ~ NA_real_,
      x >= 1900 & x <= 2026 ~ x,
      x >= 50 & x <= 99 ~ 1900 + x,
      x >= 1 & x <= 30 ~ 1973 + x,
      TRUE ~ NA_real_
    )
    
    x
  }
  
  clean_interview_year <- function(x) {
    x <- extract_first_number(x)
    x[x %in% c(96, 97, 98, 99, 9996, 9997, 9998, 9999)] <- NA_real_
    
    x <- dplyr::case_when(
      is.na(x) ~ NA_real_,
      x >= 1900 & x <= 2026 ~ x,
      x >= 90 & x <= 99 ~ 1900 + x,
      x >= 0 & x <= 9 ~ 1990 + x,
      TRUE ~ NA_real_
    )
    
    x
  }
  
  safe_make_date <- function(year, month, day) {
    out <- rep(as.Date(NA), length(year))
    
    valid <- !is.na(year) &
      !is.na(month) &
      !is.na(day) &
      year >= 1900 &
      year <= 2026 &
      month >= 1 &
      month <= 12 &
      day >= 1 &
      day <= 31
    
    date_chr <- rep(NA_character_, length(year))
    
    date_chr[valid] <- sprintf(
      "%04d-%02d-%02d",
      as.integer(year[valid]),
      as.integer(month[valid]),
      as.integer(day[valid])
    )
    
    out[valid] <- suppressWarnings(
      as.Date(date_chr[valid], format = "%Y-%m-%d")
    )
    
    out
  }
  
  bmonth <- clean_month(df[[bmonth_var]])
  byear  <- clean_birth_year(df[[byear_var]])
  imonth <- clean_month(df[[imonth_var]])
  iday   <- clean_day(df[[iday_var]])
  iyear  <- clean_interview_year(df[[iyear_var]])
  
  birth_date <- safe_make_date(
    year = byear,
    month = bmonth,
    day = rep(15, length(byear))
  )
  
  interview_date <- safe_make_date(
    year = iyear,
    month = imonth,
    day = iday
  )
  
  age <- floor(as.numeric(interview_date - birth_date) / 365.25)
  age[age < 10 | age > 25] <- NA_real_
  
  list(
    age = age,
    source = paste0(
      "constructed_from_",
      paste(c(imonth_var, iday_var, iyear_var, bmonth_var, byear_var), collapse = "_")
    ),
    variables = paste(c(imonth_var, iday_var, iyear_var, bmonth_var, byear_var), collapse = ", ")
  )
}

clean_weight <- function(x) {
  out <- to_numeric_survey(x)
  out[out <= 0] <- NA_real_
  out
}

recover_gswgt1_from_0004 <- function(raw_analysis, id_var, weight_path) {
  empty_result <- list(
    weight = rep(NA_real_, nrow(raw_analysis)),
    weight_variable = NA_character_,
    weight_source = "not_found",
    merge_method = "not_merged",
    n_valid_weight = 0L
  )
  
  if (!file.exists(weight_path)) {
    return(empty_result)
  }
  
  env_w <- new.env(parent = emptyenv())
  loaded_w <- load(weight_path, envir = env_w)
  
  data_objects_w <- loaded_w[
    vapply(
      loaded_w,
      function(x) is.data.frame(get(x, envir = env_w)),
      logical(1)
    )
  ]
  
  if (length(data_objects_w) == 0) {
    return(empty_result)
  }
  
  for (obj_name in data_objects_w) {
    wdf <- get(obj_name, envir = env_w) |>
      tibble::as_tibble()
    
    w_var <- find_var(
      wdf,
      c("GSWGT1", "gswgt1", "weight", "wave1_weight", "sample_weight")
    )
    
    if (is.na(w_var)) {
      next
    }
    
    id_w <- find_var(
      wdf,
      c("AID", "aid", "respondent_id", "id", "caseid", "case_id")
    )
    
    # Preferred method: merge by respondent ID
    if (!is.na(id_var) && id_var %in% names(raw_analysis) && !is.na(id_w)) {
      lookup <- wdf |>
        transmute(
          .merge_id = as.character(.data[[id_w]]),
          survey_weight_19f = clean_weight(.data[[w_var]])
        ) |>
        filter(!is.na(.merge_id)) |>
        distinct(.merge_id, .keep_all = TRUE)
      
      merged <- raw_analysis |>
        mutate(.merge_id = as.character(.data[[id_var]])) |>
        left_join(lookup, by = ".merge_id")
      
      return(list(
        weight = merged$survey_weight_19f,
        weight_variable = w_var,
        weight_source = paste0(weight_path, ":", obj_name, ":", w_var),
        merge_method = paste0("merged_by_", id_var, "_and_", id_w),
        n_valid_weight = sum(!is.na(merged$survey_weight_19f))
      ))
    }
    
    # Fallback method: row-order merge only if row counts match
    if (nrow(wdf) == nrow(raw_analysis)) {
      wt <- clean_weight(wdf[[w_var]])
      
      return(list(
        weight = wt,
        weight_variable = w_var,
        weight_source = paste0(weight_path, ":", obj_name, ":", w_var),
        merge_method = "merged_by_row_order_same_n",
        n_valid_weight = sum(!is.na(wt))
      ))
    }
  }
  
  empty_result
}


fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

tidy_lm_base <- function(model, model_id, model_type) {
  sm <- summary(model)
  coefs <- as.data.frame(sm$coefficients)
  
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL
  
  names(coefs) <- c(
    "estimate",
    "std_error",
    "statistic",
    "p_value",
    "term"
  )
  
  ci <- suppressWarnings(confint(model))
  ci <- as.data.frame(ci)
  ci$term <- rownames(ci)
  rownames(ci) <- NULL
  names(ci) <- c("conf_low", "conf_high", "term")
  
  coefs |>
    left_join(ci, by = "term") |>
    mutate(
      model_id = model_id,
      model_type = model_type,
      estimate = round(estimate, 5),
      std_error = round(std_error, 5),
      statistic = round(statistic, 4),
      p_value = round(p_value, 5),
      p_value_display = fmt_p(p_value),
      conf_low = round(conf_low, 5),
      conf_high = round(conf_high, 5)
    ) |>
    select(
      model_id,
      model_type,
      term,
      estimate,
      std_error,
      statistic,
      p_value,
      p_value_display,
      conf_low,
      conf_high
    )
}

fit_stats_base <- function(model, model_id, model_type, dv, predictors, covariates, weighted) {
  sm <- summary(model)
  
  tibble(
    model_id = model_id,
    model_type = model_type,
    dependent_variable = dv,
    predictors = paste(predictors, collapse = " + "),
    covariates = ifelse(length(covariates) == 0, "none", paste(covariates, collapse = " + ")),
    weighted = weighted,
    n_used = length(stats::model.response(stats::model.frame(model))),
    r_squared = round(sm$r.squared, 5),
    adj_r_squared = round(sm$adj.r.squared, 5),
    residual_se = round(sm$sigma, 5),
    f_statistic = ifelse(length(sm$fstatistic) > 0, round(unname(sm$fstatistic[1]), 4), NA_real_),
    f_num_df = ifelse(length(sm$fstatistic) > 0, unname(sm$fstatistic[2]), NA_real_),
    f_den_df = ifelse(length(sm$fstatistic) > 0, unname(sm$fstatistic[3]), NA_real_),
    f_p_value = ifelse(
      length(sm$fstatistic) > 0,
      pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3], lower.tail = FALSE),
      NA_real_
    )
  ) |>
    mutate(
      f_p_value = round(f_p_value, 5),
      f_p_value_display = fmt_p(f_p_value)
    )
}

standardize_for_model <- function(data, vars_to_scale) {
  out <- data
  
  for (v in vars_to_scale) {
    if (v %in% names(out)) {
      x <- out[[v]]
      s <- sd(x, na.rm = TRUE)
      m <- mean(x, na.rm = TRUE)
      
      if (!is.na(s) && s > 0) {
        out[[v]] <- as.numeric((x - m) / s)
      }
    }
  }
  
  out
}

# ------------------------------------------------------------
# 5. Load raw file and reconstruct analytic covariate frame
# ------------------------------------------------------------

env_raw <- new.env(parent = emptyenv())
loaded_objects <- load(raw_path, envir = env_raw)

data_objects <- loaded_objects[
  vapply(
    loaded_objects,
    function(x) is.data.frame(get(x, envir = env_raw)),
    logical(1)
  )
]

if (length(data_objects) == 0) {
  stop("No data.frame object found in raw file.", call. = FALSE)
}

raw_df <- get(data_objects[1], envir = env_raw) |>
  tibble::as_tibble()

log_line("Loaded raw file: ", raw_path)
log_line("Selected object: ", data_objects[1])
log_line("Raw rows: ", nrow(raw_df), " | Raw columns: ", ncol(raw_df))

id_var <- find_var(
  raw_df,
  c("AID", "aid", "respondent_id", "id", "caseid", "case_id")
)


sexever_var <- find_var(
  raw_df,
  c(
    "sexual_initiation",
    "sex_initiation",
    "ever_had_sex",
    "ever_sex",
    "had_sex",
    "had_sex_ever",
    "vaginal_intercourse_ever",
    "H1CO1",
    "h1co1"
  )
)

age_var <- find_var(
  raw_df,
  c(
    "age",
    "AGE",
    "age_w1",
    "age_wave1",
    "respondent_age",
    "respondent_age_w1",
    "age_years",
    "calculated_age",
    "H1AGE"
  )
)

if (!is.na(age_var)) {
  age_vector <- to_numeric_survey(raw_df[[age_var]])
  age_source <- age_var
} else {
  age_calc <- compute_wave1_age_from_dates(raw_df)
  age_vector <- age_calc$age
  age_source <- age_calc$source
}

if (is.na(sexever_var)) {
  stop("Could not identify ever-sex variable.", call. = FALSE)
}

raw_work <- raw_df |>
  mutate(
    .age_19f = age_vector,
    .ever_sex_19f = recode_ever_sex(.data[[sexever_var]]),
    .never_sex_19f = .ever_sex_19f == 0,
    .age_15_19_19f = .age_19f >= 15 & .age_19f <= 19
  )

raw_analysis <- raw_work |>
  filter(.age_15_19_19f, .never_sex_19f)

if (nrow(raw_analysis) != nrow(tpb_df)) {
  warning(
    "Row count mismatch between reconstructed raw analytic sample and 19e dataset. ",
    "raw_analysis n = ", nrow(raw_analysis),
    "; tpb_df n = ", nrow(tpb_df),
    ". Covariates will still be attached by row order, but review carefully."
  )
}

weight_recovery <- recover_gswgt1_from_0004(
  raw_analysis = raw_analysis,
  id_var = id_var,
  weight_path = weight_path
)

raw_analysis$survey_weight_19f <- weight_recovery$weight

log_line("Weight recovery source: ", weight_recovery$weight_source)
log_line("Weight merge method: ", weight_recovery$merge_method)
log_line("Valid weights recovered: ", weight_recovery$n_valid_weight)

# ------------------------------------------------------------
# 6. Detect and recode covariates
# ------------------------------------------------------------

sex_var <- find_var(
  raw_df,
  c(
    "sex",
    "SEX",
    "gender",
    "GENDER",
    "bio_sex",
    "BIO_SEX",
    "respondent_sex",
    "respondent_gender",
    "H1GI4",
    "h1gi4"
  )
)

grade_var <- find_var(
  raw_df,
  c(
    "grade",
    "GRADE",
    "school_grade",
    "current_grade",
    "respondent_grade",
    "H1GI20",
    "h1gi20",
    "H1GI21",
    "h1gi21",
    "H1ED1",
    "h1ed1"
  )
)

residence_var <- find_var(
  raw_df,
  c(
    "H1IR12",
    "h1ir12",
    "urban_rural",
    "residence",
    "rural_urban",
    "urbanicity"
  )
)

weight_var <- if (weight_recovery$n_valid_weight > 0) {
  "survey_weight_19f"
} else {
  find_var(
    raw_df,
    c(
      "GSWGT1",
      "gswgt1",
      "weight",
      "wave1_weight",
      "sample_weight"
    )
  )
}

recode_sex <- function(x) {
  raw <- extract_first_number(x)
  label <- tolower(as.character(x))
  
  out <- rep(NA_character_, length(raw))
  
  out[str_detect(label, "male|masculino|boy")] <- "Male"
  out[str_detect(label, "female|feminino|girl")] <- "Female"
  
  out[is.na(out) & raw == 1] <- "Male"
  out[is.na(out) & raw == 2] <- "Female"
  
  factor(out)
}

recode_grade <- function(x) {
  raw <- extract_first_number(x)
  raw[raw %in% c(96, 97, 98, 99)] <- NA_real_
  raw[raw < 1 | raw > 20] <- NA_real_
  raw
}

recode_residence <- function(x) {
  raw <- extract_first_number(x)
  label <- tolower(as.character(x))
  
  out <- rep(NA_character_, length(raw))
  
  out[str_detect(label, "rural")] <- "Rural"
  out[str_detect(label, "suburban")] <- "Suburban"
  out[str_detect(label, "urban")] <- "Urban"
  
  # Fallback for common ordinal location codes.
  out[is.na(out) & raw == 1] <- "Urban"
  out[is.na(out) & raw == 2] <- "Suburban"
  out[is.na(out) & raw == 3] <- "Rural"
  
  factor(out)
}

covariate_frame <- tibble(
  row_id_19f = seq_len(nrow(raw_analysis)),
  age_covariate = raw_analysis$.age_19f
)

if (!is.na(sex_var) && sex_var %in% names(raw_analysis)) {
  covariate_frame$sex_covariate <- recode_sex(raw_analysis[[sex_var]])
}

if (!is.na(grade_var) && grade_var %in% names(raw_analysis)) {
  covariate_frame$grade_covariate <- recode_grade(raw_analysis[[grade_var]])
}

if (!is.na(residence_var) && residence_var %in% names(raw_analysis)) {
  covariate_frame$residence_covariate <- recode_residence(raw_analysis[[residence_var]])
}

if (!is.na(weight_var) && weight_var %in% names(raw_analysis)) {
  covariate_frame$survey_weight_19f <- clean_weight(raw_analysis[[weight_var]])

}

covariate_audit <- tibble(
  covariate = c("age", "sex_gender", "grade", "residence", "survey_weight"),
  detected_variable = c(
    age_source,
    ifelse(is.na(sex_var), NA_character_, sex_var),
    ifelse(is.na(grade_var), NA_character_, grade_var),
    ifelse(is.na(residence_var), NA_character_, residence_var),
    ifelse(
      weight_recovery$n_valid_weight > 0,
      weight_recovery$weight_source,
      ifelse(is.na(weight_var), NA_character_, weight_var)
    )
  ),
  included_in_adjusted_models = c(
    TRUE,
    "sex_covariate" %in% names(covariate_frame),
    "grade_covariate" %in% names(covariate_frame),
    "residence_covariate" %in% names(covariate_frame),
    FALSE
  ),
  used_as_weight = c(
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    "survey_weight_19f" %in% names(covariate_frame)
  ),
  n_valid = c(
    sum(!is.na(covariate_frame$age_covariate)),
    ifelse("sex_covariate" %in% names(covariate_frame), sum(!is.na(covariate_frame$sex_covariate)), NA_integer_),
    ifelse("grade_covariate" %in% names(covariate_frame), sum(!is.na(covariate_frame$grade_covariate)), NA_integer_),
    ifelse("residence_covariate" %in% names(covariate_frame), sum(!is.na(covariate_frame$residence_covariate)), NA_integer_),
    ifelse("survey_weight_19f" %in% names(covariate_frame), sum(!is.na(covariate_frame$survey_weight_19f)), NA_integer_)
  ),
  note = c(
    "Age reconstructed consistently with previous scripts.",
    "Included if detected; recoding should be reviewed if unexpected levels appear.",
    "Included if detected; if not detected, adjusted models omit grade.",
    "H1IR12 preferred when available.",
    "Used as analytic weight in weighted model variants only."
  )
)

write_csv(covariate_audit, covariate_audit_path)

log_line("Covariate audit saved: ", covariate_audit_path)

# ------------------------------------------------------------
# 7. Build regression dataset
# ------------------------------------------------------------

analysis_df <- tpb_df |>
  mutate(row_id_19f = dplyr::row_number()) |>
  left_join(covariate_frame, by = "row_id_19f")

required_model_vars <- c(
  "family_connectedness_mean_1_5",
  "friend_support_mean_1_5",
  "tpb_attitudes_delay_mean_1_5",
  "peer_norm_delay_H1MO1",
  "partner_norm_delay_H1MO2",
  "maternal_norm_delay_H1MO4"
)

missing_required_model_vars <- setdiff(required_model_vars, names(analysis_df))

if (length(missing_required_model_vars) > 0) {
  stop(
    "Missing required model variables from 19e dataset: ",
    paste(missing_required_model_vars, collapse = ", "),
    call. = FALSE
  )
}

saveRDS(analysis_df, analysis_dataset_path)

# ------------------------------------------------------------
# 8. Model specifications
# ------------------------------------------------------------

main_predictors <- c(
  "family_connectedness_mean_1_5",
  "friend_support_mean_1_5"
)

covariates <- c(
  "age_covariate",
  "sex_covariate",
  "grade_covariate",
  "residence_covariate"
)

covariates <- covariates[covariates %in% names(analysis_df)]

has_weight <- "survey_weight_19f" %in% names(analysis_df) &&
  sum(!is.na(analysis_df$survey_weight_19f)) > 0

model_outcomes <- tibble::tribble(
  ~model_block, ~dependent_variable, ~outcome_label,
  "M1", "tpb_attitudes_delay_mean_1_5", "TPB-compatible delay-supportive attitudes",
  "M2", "peer_norm_delay_H1MO1", "Peer norm item",
  "M3", "partner_norm_delay_H1MO2", "Partner norm item",
  "M4", "maternal_norm_delay_H1MO4", "Maternal norm item"
)

model_specs <- model_outcomes |>
  tidyr::crossing(
    model_variant = c("A_unadjusted", "B_interpersonal", "C_adjusted", "D_adjusted_weighted")
  ) |>
  mutate(
    predictors = case_when(
      model_variant == "A_unadjusted" ~ "family_connectedness_mean_1_5",
      model_variant == "B_interpersonal" ~ paste(main_predictors, collapse = " + "),
      model_variant == "C_adjusted" ~ paste(c(main_predictors, covariates), collapse = " + "),
      model_variant == "D_adjusted_weighted" ~ paste(c(main_predictors, covariates), collapse = " + "),
      TRUE ~ NA_character_
    ),
    weighted = model_variant == "D_adjusted_weighted",
    model_id = paste(model_block, model_variant, sep = "_"),
    run_model = case_when(
      model_variant == "D_adjusted_weighted" & !has_weight ~ FALSE,
      model_variant == "C_adjusted" & length(covariates) == 0 ~ FALSE,
      model_variant == "D_adjusted_weighted" & length(covariates) == 0 ~ FALSE,
      TRUE ~ TRUE
    ),
    reason_if_not_run = case_when(
      model_variant == "D_adjusted_weighted" & !has_weight ~ "Survey weight not detected or unavailable.",
      model_variant == "C_adjusted" & length(covariates) == 0 ~ "No covariates detected.",
      model_variant == "D_adjusted_weighted" & length(covariates) == 0 ~ "No covariates detected.",
      TRUE ~ NA_character_
    )
  )

write_csv(model_specs, model_spec_path)

# ------------------------------------------------------------
# 9. Fit models
# ------------------------------------------------------------

fit_one_model <- function(spec_row, data) {
  spec_row <- as.list(spec_row)
  
  if (!isTRUE(spec_row$run_model)) {
    return(list(
      model = NULL,
      coefficient_table = tibble(),
      fit_table = tibble(),
      sample_table = tibble(
        model_id = spec_row$model_id,
        dependent_variable = spec_row$dependent_variable,
        model_variant = spec_row$model_variant,
        run_model = FALSE,
        n_available = NA_integer_,
        n_complete = NA_integer_,
        weighted = spec_row$weighted,
        note = spec_row$reason_if_not_run
      ),
      std_coefficient_table = tibble()
    ))
  }
  
  rhs_vars <- unlist(strsplit(spec_row$predictors, " \\+ "))
  rhs_vars <- rhs_vars[rhs_vars %in% names(data)]
  
  model_vars <- c(spec_row$dependent_variable, rhs_vars)
  
  if (isTRUE(spec_row$weighted)) {
    model_vars <- c(model_vars, "survey_weight_19f")
  }
  
  model_data <- data |>
    dplyr::select(dplyr::all_of(model_vars))
  
  model_data <- model_data[stats::complete.cases(model_data), , drop = FALSE]
  
  if (isTRUE(spec_row$weighted)) {
    model_data <- model_data |>
      dplyr::filter(!is.na(survey_weight_19f), survey_weight_19f > 0)
  }
  
  n_available <- sum(!is.na(data[[spec_row$dependent_variable]]))
  n_complete <- nrow(model_data)
  
  if (n_complete < 30) {
    return(list(
      model = NULL,
      coefficient_table = tibble(),
      fit_table = tibble(),
      sample_table = tibble(
        model_id = spec_row$model_id,
        dependent_variable = spec_row$dependent_variable,
        model_variant = spec_row$model_variant,
        run_model = FALSE,
        n_available = n_available,
        n_complete = n_complete,
        weighted = spec_row$weighted,
        note = "Model not run: fewer than 30 complete cases."
      ),
      std_coefficient_table = tibble()
    ))
  }
  
  fml <- as.formula(
    paste(spec_row$dependent_variable, "~", paste(rhs_vars, collapse = " + "))
  )
  
  if (isTRUE(spec_row$weighted)) {
    model <- lm(fml, data = model_data, weights = survey_weight_19f)
  } else {
    model <- lm(fml, data = model_data)
  }
  
  coef_tbl <- tidy_lm_base(
    model = model,
    model_id = spec_row$model_id,
    model_type = "linear_regression"
  ) |>
    mutate(
      dependent_variable = spec_row$dependent_variable,
      outcome_label = spec_row$outcome_label,
      model_variant = spec_row$model_variant,
      weighted = spec_row$weighted
    )
  
  fit_tbl <- fit_stats_base(
    model = model,
    model_id = spec_row$model_id,
    model_type = "linear_regression",
    dv = spec_row$dependent_variable,
    predictors = rhs_vars,
    covariates = covariates[covariates %in% rhs_vars],
    weighted = spec_row$weighted
  )
  
  sample_tbl <- tibble(
    model_id = spec_row$model_id,
    dependent_variable = spec_row$dependent_variable,
    model_variant = spec_row$model_variant,
    run_model = TRUE,
    n_available = n_available,
    n_complete = n_complete,
    weighted = spec_row$weighted,
    note = "Model estimated."
  )
  
  # Standardized model: scale continuous variables only.
  # Factor covariates remain unchanged.
  std_data <- model_data
  
  continuous_vars <- rhs_vars[
    vapply(std_data[rhs_vars], is.numeric, logical(1))
  ]
  
  vars_to_scale <- c(spec_row$dependent_variable, continuous_vars)
  
  if (isTRUE(spec_row$weighted)) {
    vars_to_scale <- setdiff(vars_to_scale, "survey_weight_19f")
  }
  
  std_data <- standardize_for_model(std_data, vars_to_scale)
  
  if (isTRUE(spec_row$weighted)) {
    std_model <- lm(fml, data = std_data, weights = survey_weight_19f)
  } else {
    std_model <- lm(fml, data = std_data)
  }
  
  std_coef_tbl <- tidy_lm_base(
    model = std_model,
    model_id = spec_row$model_id,
    model_type = "standardized_linear_regression"
  ) |>
    mutate(
      dependent_variable = spec_row$dependent_variable,
      outcome_label = spec_row$outcome_label,
      model_variant = spec_row$model_variant,
      weighted = spec_row$weighted
    )
  
  list(
    model = model,
    coefficient_table = coef_tbl,
    fit_table = fit_tbl,
    sample_table = sample_tbl,
    std_coefficient_table = std_coef_tbl
  )
}

model_results <- purrr::map(
  seq_len(nrow(model_specs)),
  ~ fit_one_model(model_specs[.x, ], analysis_df)
)

coefs <- bind_rows(
  purrr::map(model_results, "coefficient_table")
)

fits <- bind_rows(
  purrr::map(model_results, "fit_table")
)

model_sample_audit <- bind_rows(
  purrr::map(model_results, "sample_table")
)

std_coefs <- bind_rows(
  purrr::map(model_results, "std_coefficient_table")
)

write_csv(coefs, coefs_path)
write_csv(fits, fit_path)
write_csv(model_sample_audit, model_sample_audit_path)
write_csv(std_coefs, std_coefs_path)

# ------------------------------------------------------------
# 10. Interpretation notes
# ------------------------------------------------------------

extract_main_effects <- coefs |>
  filter(
    term %in% main_predictors,
    model_variant %in% c("B_interpersonal", "C_adjusted", "D_adjusted_weighted")
  ) |>
  mutate(
    direction = case_when(
      is.na(estimate) ~ "not estimated",
      estimate > 0 ~ "positive",
      estimate < 0 ~ "negative",
      TRUE ~ "zero"
    ),
    statistical_flag = case_when(
      is.na(p_value) ~ "not estimated",
      p_value < 0.05 ~ "p < 0.05",
      p_value < 0.10 ~ "p < 0.10",
      TRUE ~ "not statistically significant at 10%"
    )
  ) |>
  select(
    model_id,
    dependent_variable,
    model_variant,
    term,
    estimate,
    conf_low,
    conf_high,
    p_value_display,
    direction,
    statistical_flag
  )

interpretation_lines <- c(
  "# Script 19f — Interpretation Notes",
  "",
  paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
  "",
  "## Scope",
  "",
  "These models are exploratory TPB-compatible mechanism regressions. They are not full TPB mediation models.",
  "",
  "Script 19e classified full mediation as not ready because no direct intention-to-delay outcome was confirmed and perceived behavioral control/self-efficacy was not operationally confirmed.",
  "",
  "## Outcomes",
  "",
  "- M1: delay-supportive attitudes;",
  "- M2: peer norm item;",
  "- M3: partner norm item;",
  "- M4: maternal norm item.",
  "",
  "## Predictors",
  "",
  "- family_connectedness_mean_1_5;",
  "- friend_support_mean_1_5.",
  "",
  "## Interpretation rule",
  "",
  "A positive coefficient means that higher family connectedness or friend support is associated with a higher score on the corresponding TPB-compatible outcome.",
  "",
  "Because all outcomes are coded from 1 to 5 with higher values indicating more delay-supportive orientation, positive coefficients indicate more protective psychosocial orientation.",
  "",
  "## Caution",
  "",
  "The results should be interpreted as associations in a cross-sectional public-use dataset. They do not establish causality, temporal ordering, or formal mediation."
)

writeLines(interpretation_lines, interpretation_path)

# ------------------------------------------------------------
# 11. Optional Word report
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
  
  doc <- officer::body_add_par(doc, "Scope", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste(
      "These models are exploratory TPB-compatible mechanism regressions.",
      "They are not full TPB mediation models because Script 19e did not confirm a direct intention-to-delay outcome or a perceived behavioral control construct."
    ),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Covariate detection audit", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(covariate_audit))
  )
  
  doc <- officer::body_add_par(doc, "Model sample audit", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(model_sample_audit))
  )
  
  doc <- officer::body_add_par(doc, "Model fit statistics", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(fits))
  )
  
  doc <- officer::body_add_par(doc, "Main regression coefficients", style = "heading 2")
  
  main_coef_report <- coefs |>
    filter(term %in% c("(Intercept)", main_predictors)) |>
    select(
      model_id,
      dependent_variable,
      model_variant,
      term,
      estimate,
      std_error,
      conf_low,
      conf_high,
      p_value_display,
      weighted
    )
  
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(main_coef_report))
  )
  
  doc <- officer::body_add_par(doc, "Standardized coefficients for main predictors", style = "heading 2")
  
  std_main_report <- std_coefs |>
    filter(term %in% main_predictors) |>
    select(
      model_id,
      dependent_variable,
      model_variant,
      term,
      estimate,
      std_error,
      conf_low,
      conf_high,
      p_value_display,
      weighted
    )
  
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(std_main_report))
  )
  
  doc <- officer::body_add_par(doc, "Interpretation note", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste(
      "Positive coefficients indicate that higher family connectedness or friend support is associated with more delay-supportive TPB-compatible psychosocial orientation.",
      "Because this is an exploratory cross-sectional analysis, results should not be interpreted as causal mediation."
    ),
    style = "Normal"
  )
  
  print(doc, target = docx_path)
}

# ------------------------------------------------------------
# 12. Console output
# ------------------------------------------------------------

end_time <- Sys.time()

log_line("Saved regression dataset: ", analysis_dataset_path)
log_line("Saved covariate audit: ", covariate_audit_path)
log_line("Saved model sample audit: ", model_sample_audit_path)
log_line("Saved model specifications: ", model_spec_path)
log_line("Saved coefficients: ", coefs_path)
log_line("Saved model fit statistics: ", fit_path)
log_line("Saved standardized coefficients: ", std_coefs_path)
log_line("Saved interpretation notes: ", interpretation_path)

if (has_docx) {
  log_line("Saved Word report: ", docx_path)
} else {
  log_line("Word report not created because officer/flextable were unavailable.")
}

log_line("Completed ", script_id, " in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
log_line("No Git action was performed.")

cat("\n============================================================\n")
cat("Script 19f completed: Exploratory TPB-Compatible Mechanism Regression Models\n")
cat("============================================================\n\n")

cat("Covariate detection audit:\n")
print(covariate_audit, n = Inf)

cat("\nModel specifications:\n")
print(model_specs, n = Inf)

cat("\nModel sample audit:\n")
print(model_sample_audit, n = Inf)

cat("\nModel fit statistics:\n")
print(fits, n = Inf)

cat("\nMain regression coefficients:\n")
coefs |>
  dplyr::filter(term %in% c("(Intercept)", main_predictors)) |>
  dplyr::arrange(model_id, term) |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nStandardized coefficients for main predictors:\n")
std_coefs |>
  dplyr::filter(term %in% main_predictors) |>
  dplyr::arrange(model_id, term) |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nExtracted main-effect interpretation table:\n")
extract_main_effects |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nMain outputs:\n")
print(tibble(
  output = c(
    "Regression dataset",
    "Covariate detection audit",
    "Model sample audit",
    "Model specifications",
    "Regression coefficients",
    "Model fit statistics",
    "Standardized coefficients",
    "Interpretation notes",
    "Word report",
    "Run log"
  ),
  path = c(
    analysis_dataset_path,
    covariate_audit_path,
    model_sample_audit_path,
    model_spec_path,
    coefs_path,
    fit_path,
    std_coefs_path,
    interpretation_path,
    ifelse(has_docx, docx_path, NA_character_),
    log_path
  ),
  exists = file.exists(c(
    analysis_dataset_path,
    covariate_audit_path,
    model_sample_audit_path,
    model_spec_path,
    coefs_path,
    fit_path,
    std_coefs_path,
    interpretation_path,
    ifelse(has_docx, docx_path, ""),
    log_path
  ))
))