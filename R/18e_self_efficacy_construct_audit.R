# ============================================================
# Script 18e — Section 9 Self-Efficacy Construct Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Construct the Section 9 self-efficacy predictor block.
#
# Section 9: Self Efficacy
# Relevant items:
#   H1SE1 — confidence in stopping oneself and using birth control
#           once highly aroused or turned on
#   H1SE2 — confidence in planning ahead to have birth control available
#   H1SE3 — confidence in resisting sexual intercourse if partner does
#           not want to use birth control
#
# Excluded item:
#   H1SE4 — perceived intelligence relative to peers
#           excluded from sexual/protective self-efficacy construct
#
# Coding:
#   H1SE1–H1SE3:
#     1 = very sure
#     2 = moderately sure
#     3 = neither sure nor unsure
#     4 = moderately unsure
#     5 = very unsure
#     6 = I never want to use birth control
#     96/97/98/99 = non-substantive missing codes
#
# Recoding:
#   For H1SE1–H1SE3, valid values are 1–5.
#   Code 6 is treated as missing for self-efficacy.
#   Items are reverse-coded so higher values mean greater self-efficacy:
#     H1SE*_rev = 6 - H1SE*
#
# Main construct:
#   s09_contraceptive_self_efficacy_index_1_5
#
# Main outputs:
#   outputs/indices/script18e_s09_self_efficacy_predictors_LOCAL_ONLY.csv
#   outputs/audits/script18e_s09_item_recoding_audit.csv
#   outputs/audits/script18e_s09_item_distributions_after_cleaning.csv
#   outputs/audits/script18e_s09_construct_reliability_summary.csv
#   outputs/audits/script18e_s09_alpha_if_deleted.csv
#   outputs/audits/script18e_s09_item_total_correlations.csv
#   outputs/audits/script18e_s09_construct_score_summary.csv
#   outputs/audits/script18e_s09_construct_score_summary_15_19.csv
#   outputs/audits/script18e_s09_correlation_matrix.csv
#   outputs/audits/script18e_s09_construct_decision_table.csv
#   outputs/audits/script18e_methodological_decisions.csv
#   outputs/audits/script18e_final_status.csv
#   docs/add_health_wave01_section9_self_efficacy_construct_audit_script18e.docx
#
# Data protection:
#   The row-level predictor file is LOCAL_ONLY and should not be
#   committed to GitHub.
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
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script."
  )
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

audit_dir <- file.path(project_root, "outputs", "audits")
indices_dir <- file.path(project_root, "outputs", "indices")
doc_dir <- file.path(project_root, "docs")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(indices_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 18e started: Section 9 Self-Efficacy Construct Audit\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

extract_numeric_code <- function(x) {
  x_chr <- as.character(x)

  suppressWarnings(
    as.numeric(stringr::str_extract(x_chr, "-?\\d+(\\.\\d+)?"))
  )
}

clean_h1se_selfeff_numeric <- function(x) {

  x_num <- extract_numeric_code(x)

  # For H1SE1–H1SE3:
  # 1–5 are valid self-efficacy responses.
  # 6 = "I never want to use birth control" is not a confidence level.
  # 96/97/98/99 are non-substantive missing codes.
  missing_codes <- c(
    6,
    7, 8, 9,
    96, 97, 98, 99,
    996, 997, 998, 999
  )

  x_num[x_num %in% missing_codes] <- NA_real_

  x_num
}

clean_h1se4_intelligence_numeric <- function(x) {

  x_num <- extract_numeric_code(x)

  # For H1SE4, 1–6 are valid intelligence self-rating responses.
  missing_codes <- c(
    96, 97, 98, 99,
    996, 997, 998, 999
  )

  x_num[x_num %in% missing_codes] <- NA_real_

  x_num
}

reverse_1_5 <- function(x) {
  ifelse(is.na(x), NA_real_, 6 - x)
}

rescale_1_5_to_0_1 <- function(x) {
  ifelse(is.na(x), NA_real_, (x - 1) / 4)
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

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

mean_if_min_valid <- function(df, vars, min_valid = 1) {

  valid_count <- rowSums(!is.na(df[, vars, drop = FALSE]))

  out <- rowMeans(df[, vars, drop = FALSE], na.rm = TRUE)
  out[valid_count < min_valid] <- NA_real_

  out
}

cronbach_alpha <- function(df_items) {

  df_complete <- df_items %>%
    filter(if_all(everything(), ~ !is.na(.x)))

  k <- ncol(df_complete)

  if (k < 2 || nrow(df_complete) < 10) {
    return(NA_real_)
  }

  item_vars <- apply(df_complete, 2, var)
  total_score <- rowSums(df_complete)
  total_var <- var(total_score)

  if (is.na(total_var) || total_var == 0) {
    return(NA_real_)
  }

  (k / (k - 1)) * (1 - sum(item_vars, na.rm = TRUE) / total_var)
}

standardized_alpha <- function(df_items) {

  df_complete <- df_items %>%
    filter(if_all(everything(), ~ !is.na(.x)))

  k <- ncol(df_complete)

  if (k < 2 || nrow(df_complete) < 10) {
    return(NA_real_)
  }

  cor_mat <- suppressWarnings(cor(df_complete, use = "complete.obs"))
  upper_vals <- cor_mat[upper.tri(cor_mat)]

  r_bar <- mean(upper_vals, na.rm = TRUE)

  if (is.na(r_bar)) return(NA_real_)

  (k * r_bar) / (1 + (k - 1) * r_bar)
}

item_total_correlation <- function(df_items, item_name) {

  other_items <- setdiff(names(df_items), item_name)

  if (length(other_items) == 0) return(NA_real_)

  item_vec <- df_items[[item_name]]
  other_score <- rowMeans(df_items[, other_items, drop = FALSE], na.rm = TRUE)

  ok <- !is.na(item_vec) & !is.na(other_score)

  if (sum(ok) < 10) return(NA_real_)

  suppressWarnings(cor(item_vec[ok], other_score[ok]))
}

score_summary <- function(df, variable, label) {

  x <- df[[variable]]

  tibble(
    variable = variable,
    label = label,
    valid_n = sum(!is.na(x)),
    missing_n = sum(is.na(x)),
    missing_rate = round(mean(is.na(x)), 4),
    mean = safe_mean(x),
    sd = safe_sd(x),
    min = safe_min(x),
    max = safe_max(x)
  )
}

# ------------------------------------------------------------
# 3. Load raw Add Health file
# ------------------------------------------------------------

raw_file <- file.path(
  project_root,
  "data/raw/21600-0001-Data.rda"
)

if (!file.exists(raw_file)) {
  stop("Raw Add Health file not found: ", raw_file)
}

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

required_vars <- c("AID", "H1SE1", "H1SE2", "H1SE3", "H1SE4")

missing_vars <- setdiff(required_vars, names(raw_df))

if (length(missing_vars) > 0) {
  stop(
    "Missing required variables in raw file: ",
    paste(missing_vars, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 4. Optional merge with age/weight from Script 18b
# ------------------------------------------------------------

isx_path <- file.path(
  indices_dir,
  "script18b_restricted_isx_sexual_protection_index_LOCAL_ONLY.csv"
)

if (file.exists(isx_path)) {
  isx_context <- readr::read_csv(isx_path, show_col_types = FALSE) %>%
    mutate(respondent_id = as.character(respondent_id)) %>%
    select(
      respondent_id,
      age_wave1,
      survey_weight
    )
} else {
  isx_context <- tibble(
    respondent_id = character(),
    age_wave1 = numeric(),
    survey_weight = numeric()
  )
}

# ------------------------------------------------------------
# 5. Codebook mapping
# ------------------------------------------------------------

s09_codebook <- tibble::tribble(
  ~variable, ~item_number, ~item_text, ~construct_domain, ~response_scale, ~raw_direction, ~recoding_rule, ~final_direction, ~model_decision,
  "H1SE1", 1, "If you wanted to use birth control, how sure are you that you could stop yourself and use birth control once you were highly aroused or turned on?", "Contraceptive self-efficacy", "1=very sure to 5=very unsure; 6=never want to use birth control", "Lower raw value means greater self-efficacy.", "Treat 6 and 96/97/98/99 as missing; reverse 1–5.", "Higher recoded value means greater contraceptive/protective self-efficacy.", "retain",
  "H1SE2", 2, "How sure are you that you could plan ahead to have some form of birth control available?", "Contraceptive self-efficacy", "1=very sure to 5=very unsure; 6=never want to use birth control", "Lower raw value means greater self-efficacy.", "Treat 6 and 96/97/98/99 as missing; reverse 1–5.", "Higher recoded value means greater contraceptive/protective self-efficacy.", "retain",
  "H1SE3", 3, "How sure are you that you could resist sexual intercourse if your partner did not want to use some form of birth control?", "Contraceptive self-efficacy", "1=very sure to 5=very unsure; 6=never want to use birth control", "Lower raw value means greater self-efficacy.", "Treat 6 and 96/97/98/99 as missing; reverse 1–5.", "Higher recoded value means greater contraceptive/protective self-efficacy.", "retain",
  "H1SE4", 4, "Compared with other people your age, how intelligent are you?", "Perceived intelligence", "1=moderately below average to 6=extremely above average", "Higher raw value means higher perceived intelligence.", "No self-efficacy recoding.", "Not applicable for sexual/protective self-efficacy.", "exclude_from_self_efficacy_construct"
)

# ------------------------------------------------------------
# 6. Clean and recode items
# ------------------------------------------------------------

s09_row <- raw_df %>%
  transmute(
    respondent_id = as.character(AID),

    H1SE1_raw = clean_h1se_selfeff_numeric(H1SE1),
    H1SE2_raw = clean_h1se_selfeff_numeric(H1SE2),
    H1SE3_raw = clean_h1se_selfeff_numeric(H1SE3),

    H1SE4_intelligence_raw = clean_h1se4_intelligence_numeric(H1SE4),

    H1SE1_selfeff_rev = reverse_1_5(H1SE1_raw),
    H1SE2_selfeff_rev = reverse_1_5(H1SE2_raw),
    H1SE3_selfeff_rev = reverse_1_5(H1SE3_raw)
  ) %>%
  left_join(isx_context, by = "respondent_id") %>%
  mutate(
    age_15_19_flag = ifelse(
      !is.na(age_wave1) & age_wave1 >= 15 & age_wave1 <= 19,
      TRUE,
      FALSE
    )
  )

selfeff_item_vars <- c(
  "H1SE1_selfeff_rev",
  "H1SE2_selfeff_rev",
  "H1SE3_selfeff_rev"
)

# ------------------------------------------------------------
# 7. Construct self-efficacy scores
# ------------------------------------------------------------

s09_predictors <- s09_row %>%
  mutate(
    s09_contraceptive_self_efficacy_valid_items =
      rowSums(!is.na(across(all_of(selfeff_item_vars)))),

    # Main score: at least 2 of 3 valid items
    s09_contraceptive_self_efficacy_index_1_5 =
      mean_if_min_valid(
        .,
        selfeff_item_vars,
        min_valid = 2
      ),

    # Strict score: all 3 items valid
    s09_contraceptive_self_efficacy_index_strict_1_5 =
      mean_if_min_valid(
        .,
        selfeff_item_vars,
        min_valid = 3
      ),

    s09_contraceptive_self_efficacy_index_0_1 =
      rescale_1_5_to_0_1(
        s09_contraceptive_self_efficacy_index_1_5
      ),

    s09_contraceptive_self_efficacy_index_z =
      z_score(
        s09_contraceptive_self_efficacy_index_1_5
      )
  )

# ------------------------------------------------------------
# 8. Reliability analysis
# ------------------------------------------------------------

selfeff_items_df <- s09_predictors %>%
  select(all_of(selfeff_item_vars))

complete_case_n <- selfeff_items_df %>%
  filter(if_all(everything(), ~ !is.na(.x))) %>%
  nrow()

alpha_all <- cronbach_alpha(selfeff_items_df)
std_alpha_all <- standardized_alpha(selfeff_items_df)

item_total_correlations <- tibble(
  variable = selfeff_item_vars,
  corrected_item_total_correlation = purrr::map_dbl(
    selfeff_item_vars,
    ~ item_total_correlation(selfeff_items_df, .x)
  )
)

alpha_if_deleted <- purrr::map_dfr(
  selfeff_item_vars,
  function(v) {

    remaining <- setdiff(selfeff_item_vars, v)

    tibble(
      deleted_item = v,
      remaining_items = paste(remaining, collapse = ", "),
      alpha_if_deleted = cronbach_alpha(
        selfeff_items_df[, remaining, drop = FALSE]
      ),
      standardized_alpha_if_deleted = standardized_alpha(
        selfeff_items_df[, remaining, drop = FALSE]
      )
    )
  }
)

correlation_matrix <- suppressWarnings(
  cor(selfeff_items_df, use = "pairwise.complete.obs")
)

correlation_matrix_df <- as.data.frame(correlation_matrix) %>%
  rownames_to_column("variable")

construct_reliability_summary <- tibble(
  construct = "s09_contraceptive_self_efficacy",
  items = paste(selfeff_item_vars, collapse = ", "),
  item_count = length(selfeff_item_vars),
  complete_case_n = complete_case_n,
  cronbach_alpha = alpha_all,
  standardized_alpha = std_alpha_all,
  reliability_note = case_when(
    is.na(alpha_all) ~ "Cronbach alpha could not be computed.",
    alpha_all >= 0.70 ~ "Acceptable internal consistency.",
    alpha_all >= 0.60 & alpha_all < 0.70 ~ "Exploratory or marginal internal consistency.",
    alpha_all < 0.60 ~ "Low internal consistency; review before use.",
    TRUE ~ "Review."
  )
)

# ------------------------------------------------------------
# 9. Item recoding audit and distributions
# ------------------------------------------------------------

item_recoding_audit <- s09_codebook %>%
  mutate(
    raw_valid_n = case_when(
      variable %in% c("H1SE1", "H1SE2", "H1SE3") ~
        purrr::map_int(
          variable,
          ~ sum(!is.na(clean_h1se_selfeff_numeric(raw_df[[.x]])))
        ),
      variable == "H1SE4" ~
        purrr::map_int(
          variable,
          ~ sum(!is.na(clean_h1se4_intelligence_numeric(raw_df[[.x]])))
        ),
      TRUE ~ NA_integer_
    ),
    raw_missing_n = case_when(
      variable %in% c("H1SE1", "H1SE2", "H1SE3") ~
        purrr::map_int(
          variable,
          ~ sum(is.na(clean_h1se_selfeff_numeric(raw_df[[.x]])))
        ),
      variable == "H1SE4" ~
        purrr::map_int(
          variable,
          ~ sum(is.na(clean_h1se4_intelligence_numeric(raw_df[[.x]])))
        ),
      TRUE ~ NA_integer_
    ),
    raw_missing_rate = round(raw_missing_n / nrow(raw_df), 4)
  )

item_distribution_after_cleaning <- purrr::map_dfr(
  c("H1SE1", "H1SE2", "H1SE3", "H1SE4"),
  function(v) {

    cleaned_value <- if (v %in% c("H1SE1", "H1SE2", "H1SE3")) {
      clean_h1se_selfeff_numeric(raw_df[[v]])
    } else {
      clean_h1se4_intelligence_numeric(raw_df[[v]])
    }

    tibble(
      variable = v,
      raw_value = as.character(raw_df[[v]]),
      cleaned_numeric_value = cleaned_value
    ) %>%
      count(variable, raw_value, cleaned_numeric_value, name = "n") %>%
      group_by(variable) %>%
      mutate(percent = round(100 * n / sum(n), 2)) %>%
      ungroup()
  }
)

# ------------------------------------------------------------
# 10. Score summaries
# ------------------------------------------------------------

construct_score_summary <- bind_rows(
  score_summary(
    s09_predictors,
    "s09_contraceptive_self_efficacy_index_1_5",
    "Contraceptive/protective self-efficacy index, at least 2 valid items"
  ),
  score_summary(
    s09_predictors,
    "s09_contraceptive_self_efficacy_index_strict_1_5",
    "Contraceptive/protective self-efficacy index, strict 3-item score"
  ),
  score_summary(
    s09_predictors,
    "s09_contraceptive_self_efficacy_index_0_1",
    "Contraceptive/protective self-efficacy index, 0–1 scale"
  ),
  score_summary(
    s09_predictors,
    "s09_contraceptive_self_efficacy_index_z",
    "Contraceptive/protective self-efficacy index, z-score"
  ),
  score_summary(
    s09_predictors,
    "H1SE1_selfeff_rev",
    "Item 1 reversed: self-efficacy under arousal"
  ),
  score_summary(
    s09_predictors,
    "H1SE2_selfeff_rev",
    "Item 2 reversed: planning birth control availability"
  ),
  score_summary(
    s09_predictors,
    "H1SE3_selfeff_rev",
    "Item 3 reversed: resisting sex without birth control"
  )
)

construct_score_summary_15_19 <- s09_predictors %>%
  filter(age_15_19_flag == TRUE) %>%
  {
    bind_rows(
      score_summary(
        .,
        "s09_contraceptive_self_efficacy_index_1_5",
        "Contraceptive/protective self-efficacy index, ages 15–19"
      ),
      score_summary(
        .,
        "s09_contraceptive_self_efficacy_index_strict_1_5",
        "Contraceptive/protective self-efficacy index, strict, ages 15–19"
      ),
      score_summary(
        .,
        "H1SE1_selfeff_rev",
        "Item 1 reversed, ages 15–19"
      ),
      score_summary(
        .,
        "H1SE2_selfeff_rev",
        "Item 2 reversed, ages 15–19"
      ),
      score_summary(
        .,
        "H1SE3_selfeff_rev",
        "Item 3 reversed, ages 15–19"
      )
    )
  }

# ------------------------------------------------------------
# 11. Construct decision table
# ------------------------------------------------------------

construct_decision_table <- tibble::tribble(
  ~construct, ~final_variable, ~items_used, ~alpha_applicable, ~default_model_role, ~expected_sign, ~decision,
  "Contraceptive/protective self-efficacy",
  "s09_contraceptive_self_efficacy_index_1_5",
  "H1SE1 reversed + H1SE2 reversed + H1SE3 reversed",
  TRUE,
  "main_self_efficacy_predictor",
  "positive",
  "Retain if internal consistency is acceptable. Higher values mean greater self-efficacy; expected association with sexual protection is positive.",

  "Perceived intelligence",
  "H1SE4_intelligence_raw",
  "H1SE4",
  FALSE,
  "excluded_from_main_self_efficacy_block",
  "not_applicable",
  "Exclude from sexual/protective self-efficacy construct because it measures perceived intelligence rather than health-protective self-efficacy."
) %>%
  left_join(
    construct_reliability_summary %>%
      select(
        cronbach_alpha,
        standardized_alpha,
        complete_case_n,
        reliability_note
      ) %>%
      mutate(
        final_variable = "s09_contraceptive_self_efficacy_index_1_5"
      ),
    by = "final_variable"
  )

# ------------------------------------------------------------
# 12. Methodological decisions
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Section 9 scope", "Section 9 is used to construct a self-efficacy predictor block separate from perceived risk and barriers.",
  "Included items", "H1SE1, H1SE2 and H1SE3 are retained because they measure perceived ability to use, plan for, or insist on birth control/protection.",
  "Excluded item", "H1SE4 is excluded from the self-efficacy construct because it measures perceived intelligence relative to peers.",
  "Treatment of code 6", "Code 6, 'I never want to use birth control', is treated as missing for self-efficacy because it is not a confidence response on the 1–5 certainty scale.",
  "Reverse coding", "H1SE1–H1SE3 are reverse-coded so that higher values indicate greater contraceptive/protective self-efficacy.",
  "Main score rule", "The main self-efficacy index uses the mean of the three reversed items and requires at least two valid items.",
  "Strict score rule", "A strict three-item score is also created for sensitivity analysis.",
  "Expected regression sign", "The expected coefficient sign is positive when the dependent variable measures greater sexual protection.",
  "Data protection", "The row-level self-efficacy score file is LOCAL_ONLY and should not be committed to GitHub."
)

# ------------------------------------------------------------
# 13. Write row-level LOCAL_ONLY output
# ------------------------------------------------------------

s09_predictor_output <- s09_predictors %>%
  select(
    respondent_id,
    age_wave1,
    age_15_19_flag,
    survey_weight,

    H1SE1_raw,
    H1SE2_raw,
    H1SE3_raw,
    H1SE4_intelligence_raw,

    H1SE1_selfeff_rev,
    H1SE2_selfeff_rev,
    H1SE3_selfeff_rev,

    s09_contraceptive_self_efficacy_valid_items,
    s09_contraceptive_self_efficacy_index_1_5,
    s09_contraceptive_self_efficacy_index_strict_1_5,
    s09_contraceptive_self_efficacy_index_0_1,
    s09_contraceptive_self_efficacy_index_z
  )

row_level_path <- file.path(
  indices_dir,
  "script18e_s09_self_efficacy_predictors_LOCAL_ONLY.csv"
)

readr::write_csv(
  s09_predictor_output,
  row_level_path
)

# ------------------------------------------------------------
# 14. Write audit outputs
# ------------------------------------------------------------

readr::write_csv(
  s09_codebook,
  file.path(audit_dir, "script18e_s09_codebook_mapping.csv")
)

readr::write_csv(
  item_recoding_audit,
  file.path(audit_dir, "script18e_s09_item_recoding_audit.csv")
)

readr::write_csv(
  item_distribution_after_cleaning,
  file.path(audit_dir, "script18e_s09_item_distributions_after_cleaning.csv")
)

readr::write_csv(
  construct_reliability_summary,
  file.path(audit_dir, "script18e_s09_construct_reliability_summary.csv")
)

readr::write_csv(
  alpha_if_deleted,
  file.path(audit_dir, "script18e_s09_alpha_if_deleted.csv")
)

readr::write_csv(
  item_total_correlations,
  file.path(audit_dir, "script18e_s09_item_total_correlations.csv")
)

readr::write_csv(
  correlation_matrix_df,
  file.path(audit_dir, "script18e_s09_correlation_matrix.csv")
)

readr::write_csv(
  construct_score_summary,
  file.path(audit_dir, "script18e_s09_construct_score_summary.csv")
)

readr::write_csv(
  construct_score_summary_15_19,
  file.path(audit_dir, "script18e_s09_construct_score_summary_15_19.csv")
)

readr::write_csv(
  construct_decision_table,
  file.path(audit_dir, "script18e_s09_construct_decision_table.csv")
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18e_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 15. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_section9_self_efficacy_construct_audit_script18e.docx"
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
      "Add Health Wave I — Section 9 Self-Efficacy Construct Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18e constructs the Section 9 contraceptive/protective self-efficacy predictor using H1SE1, H1SE2 and H1SE3. H1SE4 is excluded because it measures perceived intelligence rather than health-protective self-efficacy.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Codebook mapping", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(s09_codebook)) %>%
    officer::body_add_par("Item recoding audit", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(item_recoding_audit)) %>%
    officer::body_add_par("Reliability summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(construct_reliability_summary)) %>%
    officer::body_add_par("Alpha if item deleted", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(alpha_if_deleted)) %>%
    officer::body_add_par("Item-total correlations", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(item_total_correlations)) %>%
    officer::body_add_par("Construct score summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(construct_score_summary)) %>%
    officer::body_add_par("Construct score summary, ages 15–19", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(construct_score_summary_15_19)) %>%
    officer::body_add_par("Construct decision table", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(construct_decision_table)) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(methodological_decisions))

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 16. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "raw_file_loaded",
    "all_section9_items_available",
    "h1se4_excluded_from_self_efficacy",
    "self_efficacy_predictor_file_created",
    "codebook_mapping_created",
    "item_recoding_audit_created",
    "item_distributions_created",
    "construct_reliability_summary_created",
    "alpha_if_deleted_created",
    "item_total_correlations_created",
    "construct_score_summary_created",
    "construct_score_summary_15_19_created",
    "construct_decision_table_created",
    "methodological_decisions_created",
    "word_report_created",
    "ready_for_section9_review"
  ),
  status = c(
    exists("raw_df") && nrow(raw_df) > 0,
    all(c("H1SE1", "H1SE2", "H1SE3", "H1SE4") %in% names(raw_df)),
    TRUE,
    file.exists(row_level_path),
    file.exists(file.path(audit_dir, "script18e_s09_codebook_mapping.csv")),
    file.exists(file.path(audit_dir, "script18e_s09_item_recoding_audit.csv")),
    file.exists(file.path(audit_dir, "script18e_s09_item_distributions_after_cleaning.csv")),
    file.exists(file.path(audit_dir, "script18e_s09_construct_reliability_summary.csv")),
    file.exists(file.path(audit_dir, "script18e_s09_alpha_if_deleted.csv")),
    file.exists(file.path(audit_dir, "script18e_s09_item_total_correlations.csv")),
    file.exists(file.path(audit_dir, "script18e_s09_construct_score_summary.csv")),
    file.exists(file.path(audit_dir, "script18e_s09_construct_score_summary_15_19.csv")),
    file.exists(file.path(audit_dir, "script18e_s09_construct_decision_table.csv")),
    file.exists(file.path(audit_dir, "script18e_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18e_final_status.csv")
)

# ------------------------------------------------------------
# 17. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18e completed: Section 9 Self-Efficacy Construct Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nItem recoding audit:\n")
print(item_recoding_audit, n = Inf)

cat("\nConstruct reliability summary:\n")
print(construct_reliability_summary, n = Inf)

cat("\nAlpha if item deleted:\n")
print(alpha_if_deleted, n = Inf)

cat("\nItem-total correlations:\n")
print(item_total_correlations, n = Inf)

cat("\nConstruct score summary:\n")
print(construct_score_summary, n = Inf)

cat("\nConstruct score summary, ages 15–19:\n")
print(construct_score_summary_15_19, n = Inf)

cat("\nConstruct decision table:\n")
print(construct_decision_table, n = Inf)

cat("\nCorrelation matrix:\n")
print(as.data.frame(correlation_matrix_df))

cat("\nMethodological decisions:\n")
print(methodological_decisions, n = Inf)

cat("\nOutputs created:\n")
cat("- ", row_level_path, "\n")
cat("- ", file.path(audit_dir, "script18e_s09_codebook_mapping.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_s09_item_recoding_audit.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_s09_item_distributions_after_cleaning.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_s09_construct_reliability_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_s09_alpha_if_deleted.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_s09_item_total_correlations.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_s09_correlation_matrix.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_s09_construct_score_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_s09_construct_score_summary_15_19.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_s09_construct_decision_table.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18e_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nImportant Git note:\n")
cat("Do not commit the LOCAL_ONLY row-level predictor file.\n")
cat("Review reliability before moving to knowledge block.\n")