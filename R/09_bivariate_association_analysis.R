# ============================================================
# Project: add-health-adolescent-risk-models
# Script 09: Bivariate Association Analysis
# Author: Gelo Picol
#
# Purpose:
#   Conduct weighted bivariate association analysis for Add Health
#   Wave I using variables approved by Script 08.
#
# Main sample:
#   - Students in grades 10 to 12 at Wave I.
#
# Sensitivity sample:
#   - Students in grades 10 to 12 and ages 15 to 19.
#
# Important:
#   - This script does not estimate multivariable regressions.
#   - It uses GSWGT1 for weighted bivariate estimates.
#   - AID is used only internally and is never exported.
#   - Outputs are aggregate only.
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

script08_model_decision_path <- file.path(
  outputs_tables_dir,
  "script08_wave01_model_variable_decision_template.csv"
)

script08_outcome_readiness_path <- file.path(
  outputs_tables_dir,
  "script08_wave01_outcome_readiness_review.csv"
)

script08_recode_readiness_path <- file.path(
  outputs_tables_dir,
  "script08_wave01_recode_readiness_review.csv"
)

script08_sample_summary_path <- file.path(
  outputs_tables_dir,
  "script08_wave01_sample_missingness_summary.csv"
)


# ============================================================
# 3. Helper functions
# ============================================================

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(haven::zap_labels(x)))
}

get_variable_label <- function(x) {
  lab <- attr(x, "label")

  if (is.null(lab)) {
    return(NA_character_)
  }

  as.character(lab)
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

safe_pct_public <- function(pct, n, threshold = 10) {
  if (is.na(pct) | is.na(n)) {
    return(NA_character_)
  }

  if (n < threshold) {
    return("suppressed")
  }

  sprintf("%.2f", pct)
}

is_binary_01 <- function(x) {
  x_num <- safe_numeric(x)
  vals <- sort(unique(x_num[!is.na(x_num)]))

  length(vals) > 0 && all(vals %in% c(0, 1))
}

is_numeric_like <- function(x) {
  is.numeric(safe_numeric(x))
}

to_category <- function(x) {
  if (inherits(x, "haven_labelled")) {
    return(as.character(haven::as_factor(x, levels = "values")))
  }

  as.character(x)
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0

  if (sum(ok) == 0) {
    return(NA_real_)
  }

  sum(x[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)
}

weighted_pct_yes <- function(y, w) {
  ok <- !is.na(y) & !is.na(w) & w > 0

  if (sum(ok) == 0) {
    return(NA_real_)
  }

  100 * sum((y[ok] == 1) * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)
}

make_design <- function(data) {
  survey::svydesign(
    ids = ~1,
    weights = ~GSWGT1,
    data = data
  )
}

classify_predictor_type <- function(data, variable) {
  x <- data[[variable]]
  x_num <- safe_numeric(x)
  n_unique <- dplyr::n_distinct(x_num, na.rm = TRUE)

  if (is_binary_01(x)) {
    return("binary")
  }

  if (variable %in% c("a_age_wave1")) {
    return("continuous")
  }

  if (is.character(x) || is.factor(x) || is.logical(x)) {
    return("categorical")
  }

  if (n_unique <= 10) {
    return("categorical")
  }

  "continuous"
}

extract_add_health_tokens <- function(x) {
  tokens <- stringr::str_extract_all(
    stringr::str_to_upper(x),
    "H[0-9][A-Z]+[0-9A-Z]*"
  )

  unique(unlist(tokens))
}

test_categorical_association <- function(data, outcome, predictor, sample_name) {

  if (!all(c(outcome, predictor, "GSWGT1") %in% names(data))) {
    return(tibble())
  }

  y <- safe_numeric(data[[outcome]])
  x <- to_category(data[[predictor]])
  w <- data$GSWGT1

  valid <- !is.na(y) &
    y %in% c(0, 1) &
    !is.na(x) &
    x != "" &
    !is.na(w) &
    w > 0

  data_sub <- tibble(
    .outcome = factor(y[valid], levels = c(0, 1), labels = c("No", "Yes")),
    .predictor = factor(x[valid]),
    GSWGT1 = w[valid]
  )

  n_complete <- nrow(data_sub)

  if (n_complete == 0) {
    return(tibble())
  }

  n_yes <- sum(data_sub$.outcome == "Yes", na.rm = TRUE)
  n_no  <- sum(data_sub$.outcome == "No", na.rm = TRUE)

  n_predictor_levels <- nlevels(data_sub$.predictor)

  p_value <- NA_real_
  test_status <- "not_tested"

  if (n_complete >= 50 && n_yes >= 10 && n_no >= 10 && n_predictor_levels >= 2) {
    design_sub <- make_design(data_sub)

    test_obj <- tryCatch(
      survey::svychisq(
        ~.outcome + .predictor,
        design_sub,
        statistic = "F"
      ),
      error = function(e) NULL
    )

    if (!is.null(test_obj)) {
      p_value <- as.numeric(test_obj$p.value)
      test_status <- "rao_scott_chisq_completed"
    } else {
      test_status <- "rao_scott_chisq_failed"
    }
  } else {
    test_status <- "insufficient_complete_cases_or_cells"
  }

  detail <- data_sub %>%
    group_by(.predictor) %>%
    summarise(
      category_unweighted_n = n(),
      category_unweighted_yes = sum(.outcome == "Yes", na.rm = TRUE),
      category_unweighted_no = sum(.outcome == "No", na.rm = TRUE),
      category_weighted_total = sum(GSWGT1, na.rm = TRUE),
      category_weighted_yes = sum((.outcome == "Yes") * GSWGT1, na.rm = TRUE),
      category_weighted_pct_yes = ifelse(
        category_weighted_total > 0,
        100 * category_weighted_yes / category_weighted_total,
        NA_real_
      ),
      .groups = "drop"
    )

  max_pct <- max(detail$category_weighted_pct_yes, na.rm = TRUE)
  min_pct <- min(detail$category_weighted_pct_yes, na.rm = TRUE)

  if (!is.finite(max_pct) || !is.finite(min_pct)) {
    pct_range <- NA_real_
  } else {
    pct_range <- max_pct - min_pct
  }

  tibble(
    sample_name = sample_name,
    outcome = outcome,
    predictor = predictor,
    predictor_type = classify_predictor_type(data, predictor),
    test_family = "binary_outcome_by_categorical_predictor",
    n_complete = n_complete,
    public_n_complete = safe_count_public(n_complete),
    n_outcome_yes = n_yes,
    n_outcome_no = n_no,
    n_predictor_levels = n_predictor_levels,
    weighted_pct_outcome_yes_overall = weighted_pct_yes(
      as.numeric(data_sub$.outcome == "Yes"),
      data_sub$GSWGT1
    ),
    effect_measure = "range_weighted_pct_yes_across_predictor_categories",
    effect_value = pct_range,
    p_value = p_value,
    test_status = test_status
  )
}

categorical_detail_table <- function(data, outcome, predictor, sample_name) {

  if (!all(c(outcome, predictor, "GSWGT1") %in% names(data))) {
    return(tibble())
  }

  y <- safe_numeric(data[[outcome]])
  x <- to_category(data[[predictor]])
  w <- data$GSWGT1

  valid <- !is.na(y) &
    y %in% c(0, 1) &
    !is.na(x) &
    x != "" &
    !is.na(w) &
    w > 0

  data_sub <- tibble(
    outcome_value = ifelse(y[valid] == 1, "Yes", "No"),
    predictor_category = x[valid],
    GSWGT1 = w[valid]
  )

  if (nrow(data_sub) == 0) {
    return(tibble())
  }

  data_sub %>%
    group_by(predictor_category) %>%
    summarise(
      unweighted_n = n(),
      unweighted_yes = sum(outcome_value == "Yes", na.rm = TRUE),
      unweighted_no = sum(outcome_value == "No", na.rm = TRUE),
      weighted_total = sum(GSWGT1, na.rm = TRUE),
      weighted_yes = sum((outcome_value == "Yes") * GSWGT1, na.rm = TRUE),
      weighted_pct_yes = ifelse(
        weighted_total > 0,
        100 * weighted_yes / weighted_total,
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    mutate(
      sample_name = sample_name,
      outcome = outcome,
      predictor = predictor,
      public_unweighted_n = vapply(unweighted_n, safe_count_public, character(1)),
      public_weighted_pct_yes = mapply(
        safe_pct_public,
        weighted_pct_yes,
        unweighted_n,
        USE.NAMES = FALSE
      ),
      disclosure_status = case_when(
        unweighted_n < 10 ~ "suppressed_small_category",
        unweighted_yes < 10 | unweighted_no < 10 ~ "review_small_outcome_cell",
        TRUE ~ "reported"
      )
    ) %>%
    select(
      sample_name,
      outcome,
      predictor,
      predictor_category,
      unweighted_n,
      public_unweighted_n,
      unweighted_yes,
      unweighted_no,
      weighted_total,
      weighted_pct_yes,
      public_weighted_pct_yes,
      disclosure_status
    )
}

test_continuous_association <- function(data, outcome, predictor, sample_name) {

  if (!all(c(outcome, predictor, "GSWGT1") %in% names(data))) {
    return(tibble())
  }

  y <- safe_numeric(data[[outcome]])
  x <- safe_numeric(data[[predictor]])
  w <- data$GSWGT1

  valid <- !is.na(y) &
    y %in% c(0, 1) &
    !is.na(x) &
    !is.na(w) &
    w > 0

  data_sub <- tibble(
    .outcome = factor(y[valid], levels = c(0, 1), labels = c("No", "Yes")),
    .predictor = x[valid],
    GSWGT1 = w[valid]
  )

  n_complete <- nrow(data_sub)

  if (n_complete == 0) {
    return(tibble())
  }

  n_yes <- sum(data_sub$.outcome == "Yes", na.rm = TRUE)
  n_no  <- sum(data_sub$.outcome == "No", na.rm = TRUE)

  mean_yes <- weighted_mean_safe(
    data_sub$.predictor[data_sub$.outcome == "Yes"],
    data_sub$GSWGT1[data_sub$.outcome == "Yes"]
  )

  mean_no <- weighted_mean_safe(
    data_sub$.predictor[data_sub$.outcome == "No"],
    data_sub$GSWGT1[data_sub$.outcome == "No"]
  )

  mean_diff <- mean_yes - mean_no

  p_value <- NA_real_
  test_status <- "not_tested"

  if (
    n_complete >= 50 &&
      n_yes >= 10 &&
      n_no >= 10 &&
      dplyr::n_distinct(data_sub$.predictor, na.rm = TRUE) >= 2
  ) {

    design_sub <- make_design(data_sub)

    test_obj <- tryCatch(
      survey::svyttest(
        .predictor ~ .outcome,
        design_sub
      ),
      error = function(e) NULL
    )

    if (!is.null(test_obj)) {
      p_value <- as.numeric(test_obj$p.value)
      test_status <- "weighted_t_test_completed"
    } else {
      test_status <- "weighted_t_test_failed"
    }

  } else {
    test_status <- "insufficient_complete_cases_or_cells"
  }

  tibble(
    sample_name = sample_name,
    outcome = outcome,
    predictor = predictor,
    predictor_type = "continuous",
    test_family = "binary_outcome_by_continuous_predictor",
    n_complete = n_complete,
    public_n_complete = safe_count_public(n_complete),
    n_outcome_yes = n_yes,
    n_outcome_no = n_no,
    n_predictor_levels = NA_integer_,
    weighted_pct_outcome_yes_overall = weighted_pct_yes(
      as.numeric(data_sub$.outcome == "Yes"),
      data_sub$GSWGT1
    ),
    effect_measure = "weighted_mean_difference_yes_minus_no",
    effect_value = mean_diff,
    weighted_mean_predictor_yes = mean_yes,
    weighted_mean_predictor_no = mean_no,
    p_value = p_value,
    test_status = test_status
  )
}


add_bh_adjustment <- function(df) {
  df %>%
    group_by(sample_name, outcome) %>%
    mutate(
      p_value_bh = {
        p <- p_value
        out <- rep(NA_real_, length(p))
        idx <- !is.na(p)
        out[idx] <- p.adjust(p[idx], method = "BH")
        out
      }
    ) %>%
    ungroup()
}

classify_bivariate_decision <- function(n_complete, n_yes, n_no, p_value, p_value_bh) {
  case_when(
    is.na(n_complete) | n_complete < 50 ~ "insufficient_complete_cases",
    is.na(n_yes) | is.na(n_no) | n_yes < 10 | n_no < 10 ~ "insufficient_outcome_cell_size",
    is.na(p_value) ~ "test_not_available",
    !is.na(p_value_bh) & p_value_bh < 0.05 ~ "candidate_association_after_bh_adjustment",
    p_value < 0.05 ~ "candidate_association_unadjusted_only",
    TRUE ~ "no_bivariate_evidence"
  )
}


# ============================================================
# 4. Check required inputs
# ============================================================

required_inputs <- c(
  weighted_analytical_rds_path,
  script08_model_decision_path,
  script08_outcome_readiness_path,
  script08_recode_readiness_path,
  script08_sample_summary_path
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
# 5. Load data and Script 08 decisions
# ============================================================

analysis_data <- readRDS(weighted_analytical_rds_path)

model_decision <- read_csv(
  script08_model_decision_path,
  show_col_types = FALSE
)

outcome_readiness <- read_csv(
  script08_outcome_readiness_path,
  show_col_types = FALSE
)

recode_readiness <- read_csv(
  script08_recode_readiness_path,
  show_col_types = FALSE
)

sample_summary_08 <- read_csv(
  script08_sample_summary_path,
  show_col_types = FALSE
)

if (!"GSWGT1" %in% names(analysis_data)) {
  stop("GSWGT1 not found in weighted analytical data.")
}

if (!"AID" %in% names(analysis_data)) {
  stop("AID should be present internally but was not found.")
}


# ============================================================
# 6. Define samples
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
# 7. Select outcomes and predictors
# ============================================================

candidate_outcomes <- outcome_readiness %>%
  filter(
    preliminary_outcome_decision ==
      "candidate_outcome_for_bivariate_analysis"
  ) %>%
  pull(variable) %>%
  unique()

candidate_outcomes <- candidate_outcomes[
  candidate_outcomes %in% names(analysis_data_valid)
]

candidate_outcomes <- candidate_outcomes[
  vapply(candidate_outcomes, function(v) is_binary_01(analysis_data_valid[[v]]), logical(1))
]

outcome_tokens <- extract_add_health_tokens(candidate_outcomes)

candidate_predictors <- model_decision %>%
  filter(
    preliminary_modeling_decision == "candidate_for_modeling",
    variable %in% names(analysis_data_valid),
    !variable %in% candidate_outcomes,
    !variable %in% c(
      "AID",
      "GSWGT1",
      "CLUSTER2",
      "SCHWT1",
      "valid_gswgt1",
      "valid_cluster2",
      "valid_schwt1"
    ),
    !stringr::str_detect(variable, "^stat_"),
    !variable_role %in% c(
      "support_variable_not_model_predictor",
      "sample_definition_variable",
      "weight_or_design_variable"
    )
  ) %>%
  mutate(
    variable_upper = stringr::str_to_upper(variable),
    overlaps_with_outcome_item = ifelse(
      length(outcome_tokens) == 0,
      FALSE,
      purrr::map_lgl(
        variable_upper,
        function(vu) any(stringr::str_detect(vu, outcome_tokens))
      )
    )
  ) %>%
  filter(!overlaps_with_outcome_item) %>%
  pull(variable) %>%
  unique()

candidate_predictors <- candidate_predictors[
  candidate_predictors %in% names(analysis_data_valid)
]

candidate_predictors <- candidate_predictors[
  !candidate_predictors %in% c("AID")
]

# ------------------------------------------------------------
# Remove duplicate raw/clean predictor pairs.
#
# If both an original item, for example H1SE2, and its cleaned
# numeric version, num_H1SE2, are available, keep only num_H1SE2.
# This prevents the same Add Health item from being tested twice.
# ------------------------------------------------------------

num_predictors <- candidate_predictors[
  stringr::str_detect(candidate_predictors, "^num_")
]

raw_equivalents_to_drop <- stringr::str_remove(num_predictors, "^num_")

candidate_predictors <- candidate_predictors[
  !candidate_predictors %in% raw_equivalents_to_drop
]

candidate_predictors <- unique(candidate_predictors)

predictor_metadata <- tibble(
  predictor = candidate_predictors,
  predictor_label = vapply(
    candidate_predictors,
    function(v) get_variable_label(analysis_data_valid[[v]]),
    character(1)
  ),
  predictor_type_main = vapply(
    candidate_predictors,
    function(v) classify_predictor_type(sample_main, v),
    character(1)
  ),
  predictor_role = model_decision$variable_role[
    match(candidate_predictors, model_decision$variable)
  ],
  preliminary_modeling_decision = model_decision$preliminary_modeling_decision[
    match(candidate_predictors, model_decision$variable)
  ]
)

outcome_metadata <- tibble(
  outcome = candidate_outcomes,
  outcome_label = vapply(
    candidate_outcomes,
    function(v) get_variable_label(analysis_data_valid[[v]]),
    character(1)
  ),
  preliminary_outcome_decision = outcome_readiness$preliminary_outcome_decision[
    match(candidate_outcomes, outcome_readiness$variable)
  ]
)


# ============================================================
# 8. Build bivariate analysis plan
# ============================================================

bivariate_analysis_plan <- tidyr::crossing(
  outcome = candidate_outcomes,
  predictor = candidate_predictors
) %>%
  filter(outcome != predictor) %>%
  left_join(outcome_metadata, by = "outcome") %>%
  left_join(predictor_metadata, by = "predictor") %>%
  mutate(
    analysis_status = "planned",
    note = "Weighted bivariate screening only; not a multivariable model."
  )


# ============================================================
# 9. Run bivariate associations
# ============================================================

bivariate_summary_raw <- purrr::imap_dfr(
  sample_list,
  function(data_sample, sample_name) {

    purrr::pmap_dfr(
      bivariate_analysis_plan %>%
        select(outcome, predictor),
      function(outcome, predictor) {

        predictor_type <- classify_predictor_type(data_sample, predictor)

        if (predictor_type %in% c("binary", "categorical")) {
          return(
            test_categorical_association(
              data = data_sample,
              outcome = outcome,
              predictor = predictor,
              sample_name = sample_name
            )
          )
        }

        if (predictor_type == "continuous") {
          return(
            test_continuous_association(
              data = data_sample,
              outcome = outcome,
              predictor = predictor,
              sample_name = sample_name
            )
          )
        }

        tibble()
      }
    )
  }
)

bivariate_summary <- bivariate_summary_raw %>%
  add_bh_adjustment() %>%
  mutate(
    preliminary_bivariate_decision = classify_bivariate_decision(
      n_complete,
      n_outcome_yes,
      n_outcome_no,
      p_value,
      p_value_bh
    ),
    effect_abs = abs(effect_value)
  ) %>%
  left_join(outcome_metadata, by = "outcome") %>%
  left_join(predictor_metadata, by = "predictor") %>%
  arrange(
    sample_name,
    outcome,
    preliminary_bivariate_decision,
    p_value_bh,
    p_value,
    desc(effect_abs)
  )


# ============================================================
# 10. Detailed categorical tables
# ============================================================

categorical_predictors <- predictor_metadata %>%
  filter(predictor_type_main %in% c("binary", "categorical")) %>%
  pull(predictor)

categorical_detail <- purrr::imap_dfr(
  sample_list,
  function(data_sample, sample_name) {

    pairs <- bivariate_analysis_plan %>%
      filter(predictor %in% categorical_predictors) %>%
      select(outcome, predictor)

    purrr::pmap_dfr(
      pairs,
      function(outcome, predictor) {
        categorical_detail_table(
          data = data_sample,
          outcome = outcome,
          predictor = predictor,
          sample_name = sample_name
        )
      }
    )
  }
)


# ============================================================
# 11. Screening summaries
# ============================================================

bivariate_decision_summary <- bivariate_summary %>%
  count(
    sample_name,
    preliminary_bivariate_decision,
    test_family,
    name = "n_tests"
  ) %>%
  arrange(sample_name, preliminary_bivariate_decision, test_family)

top_bivariate_candidates <- bivariate_summary %>%
  filter(
    preliminary_bivariate_decision %in% c(
      "candidate_association_after_bh_adjustment",
      "candidate_association_unadjusted_only"
    )
  ) %>%
  group_by(sample_name, outcome) %>%
  arrange(p_value_bh, p_value, desc(effect_abs), .by_group = TRUE) %>%
  slice_head(n = 20) %>%
  ungroup()

outcome_predictor_screening_summary <- bivariate_summary %>%
  group_by(sample_name, outcome) %>%
  summarise(
    n_predictors_screened = n(),
    n_bh_adjusted_candidates = sum(
      preliminary_bivariate_decision ==
        "candidate_association_after_bh_adjustment",
      na.rm = TRUE
    ),
    n_unadjusted_candidates = sum(
      preliminary_bivariate_decision ==
        "candidate_association_unadjusted_only",
      na.rm = TRUE
    ),
    n_no_bivariate_evidence = sum(
      preliminary_bivariate_decision == "no_bivariate_evidence",
      na.rm = TRUE
    ),
    n_not_tested_or_insufficient = sum(
      preliminary_bivariate_decision %in% c(
        "insufficient_complete_cases",
        "insufficient_outcome_cell_size",
        "test_not_available"
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(sample_name, outcome)


# ============================================================
# 12. Methodological notes
# ============================================================

script09_methodological_notes <- tibble(
  note_id = 1:14,
  note = c(
    "Script 09 conducts weighted bivariate association analysis only.",
    "The script uses GSWGT1 as the Wave I population-average weight.",
    "The main analytical sample is students in grades 10 to 12.",
    "The strict sensitivity sample is students in grades 10 to 12 and ages 15 to 19.",
    "Candidate outcomes are selected from Script 08 outcome readiness results.",
    "Candidate predictors are selected from Script 08 model variable decisions.",
    "AID is used only internally and is not exported.",
    "Candidate outcome variables are excluded from the predictor set.",
    "Predictors overlapping with direct outcome item codes are excluded where detectable.",
    "Categorical predictors are screened using weighted cross-tabulations and Rao-Scott chi-square tests.",
    "Continuous predictors are screened using weighted mean differences and weighted t-tests.",
    "Benjamini-Hochberg adjustment is applied within each sample and outcome.",
    "Bivariate screening does not imply causality and does not replace multivariable modeling.",
    "Script 10 should estimate logistic regression models using variables supported by Scripts 08 and 09."
  )
)


# ============================================================
# 13. Execution checklist
# ============================================================

script09_checklist <- tibble(
  check_id = 1:20,
  check_item = c(
    "Project root exists",
    "Weighted analytical local-only RDS exists",
    "Script 08 model decision template exists",
    "Script 08 outcome readiness exists",
    "Script 08 recode readiness exists",
    "Script 08 sample summary exists",
    "Weighted analytical data loaded",
    "GSWGT1 available",
    "AID present internally and excluded from public outputs",
    "Main sample created",
    "Strict sensitivity sample created",
    "Candidate outcomes selected",
    "Candidate predictors selected",
    "Bivariate analysis plan created",
    "Bivariate summary created",
    "Categorical detail table created",
    "Decision summary created",
    "Excel diagnostic workbook exported",
    "Markdown documentation exported",
    "No individual-level output exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(weighted_analytical_rds_path), "OK", "FAIL"),
    ifelse(file.exists(script08_model_decision_path), "OK", "FAIL"),
    ifelse(file.exists(script08_outcome_readiness_path), "OK", "FAIL"),
    ifelse(file.exists(script08_recode_readiness_path), "OK", "FAIL"),
    ifelse(file.exists(script08_sample_summary_path), "OK", "FAIL"),
    "OK",
    ifelse("GSWGT1" %in% names(analysis_data), "OK", "FAIL"),
    ifelse("AID" %in% names(analysis_data), "OK", "FAIL"),
    ifelse(nrow(sample_main) > 0, "OK", "FAIL"),
    ifelse(nrow(sample_strict) > 0, "OK", "FAIL"),
    ifelse(length(candidate_outcomes) > 0, "OK", "FAIL"),
    ifelse(length(candidate_predictors) > 0, "OK", "FAIL"),
    ifelse(nrow(bivariate_analysis_plan) > 0, "OK", "FAIL"),
    ifelse(nrow(bivariate_summary) > 0, "OK", "FAIL"),
    ifelse(nrow(categorical_detail) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(bivariate_decision_summary) > 0, "OK", "FAIL"),
    "PENDING",
    "PENDING",
    "OK"
  )
)


# ============================================================
# 14. Export public CSV outputs
# ============================================================

write_csv(
  bivariate_analysis_plan,
  file.path(outputs_tables_dir, "script09_wave01_bivariate_analysis_plan.csv")
)

write_csv(
  bivariate_summary,
  file.path(outputs_tables_dir, "script09_wave01_bivariate_association_summary.csv")
)

write_csv(
  categorical_detail,
  file.path(outputs_tables_dir, "script09_wave01_bivariate_categorical_detail.csv")
)

write_csv(
  bivariate_decision_summary,
  file.path(outputs_tables_dir, "script09_wave01_bivariate_decision_summary.csv")
)

write_csv(
  top_bivariate_candidates,
  file.path(outputs_tables_dir, "script09_wave01_top_bivariate_candidates.csv")
)

write_csv(
  outcome_predictor_screening_summary,
  file.path(outputs_tables_dir, "script09_wave01_outcome_predictor_screening_summary.csv")
)

write_csv(
  predictor_metadata,
  file.path(outputs_tables_dir, "script09_wave01_predictor_metadata.csv")
)

write_csv(
  outcome_metadata,
  file.path(outputs_tables_dir, "script09_wave01_outcome_metadata.csv")
)

write_csv(
  script09_methodological_notes,
  file.path(outputs_tables_dir, "script09_wave01_methodological_notes.csv")
)


# ============================================================
# 15. Excel diagnostic workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script09_wave01_bivariate_association_analysis.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "analysis_plan")
writeData(wb, "analysis_plan", bivariate_analysis_plan)

addWorksheet(wb, "bivariate_summary")
writeData(wb, "bivariate_summary", bivariate_summary)

addWorksheet(wb, "categorical_detail")
writeData(wb, "categorical_detail", categorical_detail)

addWorksheet(wb, "decision_summary")
writeData(wb, "decision_summary", bivariate_decision_summary)

addWorksheet(wb, "top_candidates")
writeData(wb, "top_candidates", top_bivariate_candidates)

addWorksheet(wb, "outcome_screening")
writeData(wb, "outcome_screening", outcome_predictor_screening_summary)

addWorksheet(wb, "predictor_metadata")
writeData(wb, "predictor_metadata", predictor_metadata)

addWorksheet(wb, "outcome_metadata")
writeData(wb, "outcome_metadata", outcome_metadata)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script09_methodological_notes)

addWorksheet(wb, "checklist")
writeData(wb, "checklist", script09_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:80, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script09_checklist$status[
  script09_checklist$check_item == "Excel diagnostic workbook exported"
] <- "OK"


# ============================================================
# 16. Markdown documentation
# ============================================================

script09_doc <- c(
  "# Bivariate Association Analysis",
  "",
  "Script 09 conducts weighted bivariate screening for Add Health Wave I.",
  "",
  "## Input",
  "",
  "`data/processed/add_health_wave01_analytical_weighted_local_only.rds`",
  "",
  "This file is local-only and must not be committed to GitHub.",
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
  "## Outcomes",
  "",
  "Candidate outcomes are selected from Script 08 outcome readiness diagnostics.",
  "",
  "## Predictors",
  "",
  "Candidate predictors are selected from Script 08 model variable decisions.",
  "",
  "## Tests",
  "",
  "- Categorical predictors: weighted cross-tabulations and Rao-Scott chi-square tests.",
  "- Continuous predictors: weighted mean differences and weighted t-tests.",
  "",
  "## Multiple testing",
  "",
  "Benjamini-Hochberg adjustment is applied within each sample and outcome.",
  "",
  "## Interpretation",
  "",
  "The results are exploratory bivariate associations. They do not imply causality and do not replace multivariable regression.",
  "",
  "## Privacy protection",
  "",
  "`AID` is used only internally and is excluded from all public outputs.",
  "",
  "## Next step",
  "",
  "Script 10 should estimate weighted logistic regression models for selected outcomes."
)

writeLines(
  script09_doc,
  con = file.path(docs_dir, "bivariate_association_analysis_script09.md")
)

script09_checklist$status[
  script09_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 17. Save final checklist
# ============================================================

write_csv(
  script09_checklist,
  file.path(outputs_diag_dir, "script09_execution_checklist.csv")
)


# ============================================================
# 18. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 09 completed: Bivariate Association Analysis\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Input local-only weighted analytical file:\n")
cat(weighted_analytical_rds_path, "\n\n")

cat("Sample summary:\n")
cat("- Main grade 10-12 observations: ", nrow(sample_main), "\n", sep = "")
cat("- Strict grade 10-12 and age 15-19 observations: ", nrow(sample_strict), "\n\n", sep = "")

cat("Variable selection:\n")
cat("- Candidate outcomes selected: ", length(candidate_outcomes), "\n", sep = "")
cat("- Candidate predictors selected: ", length(candidate_predictors), "\n", sep = "")
cat("- Planned bivariate tests per sample: ", nrow(bivariate_analysis_plan), "\n", sep = "")
cat("- Total bivariate tests across samples: ", nrow(bivariate_summary), "\n\n", sep = "")

cat("Bivariate decision summary:\n")
print(bivariate_decision_summary)

cat("\nOutcome-predictor screening summary:\n")
print(outcome_predictor_screening_summary)

cat("\nTop bivariate candidates preview:\n")
print(
  top_bivariate_candidates %>%
    select(
      sample_name,
      outcome,
      predictor,
      predictor_type,
      effect_measure,
      effect_value,
      p_value,
      p_value_bh,
      preliminary_bivariate_decision
    ) %>%
    head(30)
)

cat("\nPublic outputs created:\n")
cat("- outputs/tables/script09_wave01_bivariate_analysis_plan.csv\n")
cat("- outputs/tables/script09_wave01_bivariate_association_summary.csv\n")
cat("- outputs/tables/script09_wave01_bivariate_categorical_detail.csv\n")
cat("- outputs/tables/script09_wave01_bivariate_decision_summary.csv\n")
cat("- outputs/tables/script09_wave01_top_bivariate_candidates.csv\n")
cat("- outputs/tables/script09_wave01_outcome_predictor_screening_summary.csv\n")
cat("- outputs/tables/script09_wave01_predictor_metadata.csv\n")
cat("- outputs/tables/script09_wave01_outcome_metadata.csv\n")
cat("- outputs/tables/script09_wave01_methodological_notes.csv\n")
cat("- outputs/tables/script09_wave01_bivariate_association_analysis.xlsx\n")
cat("- outputs/diagnostics/script09_execution_checklist.csv\n")
cat("- docs/bivariate_association_analysis_script09.md\n\n")

cat("Execution checklist:\n")
print(script09_checklist)

cat("\nImportant note:\n")
cat("Do not commit data/raw/, data/processed/, AID-level files or individual-level data to GitHub.\n")
cat("Bivariate associations are screening evidence only. Script 10 should estimate multivariable logistic models.\n\n")
