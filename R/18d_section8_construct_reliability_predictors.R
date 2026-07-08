# ============================================================
# Script 18d — Section 8 Construct Reliability and Predictors
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Construct Section 8 perceived-risk predictors as separate
#   independent constructs for later models.
#
# Section 8:
#   Pregnancy, AIDS, and STD Risk Perceptions
#
# Variables:
#   H1RP1 — Pregnancy would be one of the worst things
#   H1RP2 — Pregnancy would not be all that bad
#   H1RP3 — AIDS virus would cause great suffering
#   H1RP4 — It would be a big hassle to protect from STD
#   H1RP5 — Chance of pregnancy after sex once without birth control
#   H1RP6 — Chance of AIDS after one month of unprotected sex
#
# Main construct logic:
#   1. Pregnancy consequence severity:
#        H1RP1 reversed + H1RP2
#
#   2. AIDS consequence severity:
#        H1RP3 reversed
#
#   3. STD protection feasibility:
#        H1RP4 original
#
#   4. Pregnancy susceptibility:
#        H1RP5 original
#
#   5. AIDS susceptibility:
#        H1RP6 original
#
#   6. Optional/exploratory unprotected-sex susceptibility:
#        H1RP5 + H1RP6
#
# Main outputs:
#   outputs/indices/script18d_s08_perceived_risk_predictors_LOCAL_ONLY.csv
#   outputs/audits/script18d_s08_construct_reliability_summary.csv
#   outputs/audits/script18d_s08_construct_score_summary.csv
#   outputs/audits/script18d_s08_item_recoding_audit.csv
#   outputs/audits/script18d_s08_construct_decision_table.csv
#   outputs/audits/script18d_final_status.csv
#   docs/add_health_wave01_section8_construct_reliability_predictors_script18d.docx
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
cat("Script 18d started: Section 8 Construct Reliability and Predictors\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_squish(x)
}

extract_numeric_code <- function(x) {
  x_chr <- as.character(x)

  suppressWarnings(
    as.numeric(stringr::str_extract(x_chr, "-?\\d+(\\.\\d+)?"))
  )
}

clean_h1rp_numeric <- function(x) {

  x_num <- extract_numeric_code(x)

  missing_codes <- c(
    6, 7, 8, 9,
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

mean_if_min_valid <- function(df, vars, min_valid = 1) {

  valid_count <- rowSums(!is.na(df[, vars, drop = FALSE]))

  out <- rowMeans(df[, vars, drop = FALSE], na.rm = TRUE)
  out[valid_count < min_valid] <- NA_real_

  out
}

construct_reliability <- function(df, vars, construct_name) {

  available_vars <- intersect(vars, names(df))

  if (length(available_vars) == 0) {
    return(tibble(
      construct = construct_name,
      items = "",
      item_count = 0,
      complete_case_n = 0,
      cronbach_alpha = NA_real_,
      item_correlation = NA_real_,
      reliability_note = "No items available."
    ))
  }

  complete_df <- df %>%
    select(all_of(available_vars)) %>%
    filter(if_all(everything(), ~ !is.na(.x)))

  item_corr <- NA_real_

  if (length(available_vars) == 2 && nrow(complete_df) >= 10) {
    item_corr <- suppressWarnings(
      cor(
        complete_df[[available_vars[1]]],
        complete_df[[available_vars[2]]],
        use = "complete.obs"
      )
    )
  }

  alpha <- if (length(available_vars) >= 2) {
    cronbach_alpha(df[, available_vars, drop = FALSE])
  } else {
    NA_real_
  }

  note <- case_when(
    length(available_vars) == 1 ~
      "Cronbach alpha not applicable because this is a single-item construct.",
    is.na(alpha) ~
      "Cronbach alpha could not be computed.",
    alpha >= 0.70 ~
      "Acceptable internal consistency.",
    alpha >= 0.60 & alpha < 0.70 ~
      "Exploratory or marginal internal consistency.",
    alpha < 0.60 ~
      "Low internal consistency; retain only if theoretically justified.",
    TRUE ~
      "Review."
  )

  tibble(
    construct = construct_name,
    items = paste(available_vars, collapse = ", "),
    item_count = length(available_vars),
    complete_case_n = nrow(complete_df),
    cronbach_alpha = alpha,
    item_correlation = item_corr,
    reliability_note = note
  )
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

s08_items <- paste0("H1RP", 1:6)

missing_items <- setdiff(s08_items, names(raw_df))

if (length(missing_items) > 0) {
  stop(
    "Missing Section 8 items in raw file: ",
    paste(missing_items, collapse = ", ")
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
# 5. Codebook mapping for Section 8
# ------------------------------------------------------------

s08_codebook <- tibble::tribble(
  ~variable, ~item_text, ~domain, ~response_scale, ~raw_direction, ~recoding_rule, ~final_direction,
  "H1RP1", "Getting pregnant at this time in your life is one of the worst things that could happen to you.", "Pregnancy consequence severity", "1=strongly agree to 5=strongly disagree", "Lower raw value means higher perceived severity.", "reverse_1_5", "Higher recoded value means higher perceived pregnancy consequence severity.",
  "H1RP2", "It would not be all that bad if you got pregnant at this time in your life.", "Pregnancy consequence severity", "1=strongly agree to 5=strongly disagree", "Higher raw value means higher perceived severity because disagreement rejects the statement.", "keep_original", "Higher value means higher perceived pregnancy consequence severity.",
  "H1RP3", "If you got the AIDS virus, you would suffer a great deal.", "AIDS consequence severity", "1=strongly agree to 5=strongly disagree", "Lower raw value means higher perceived severity.", "reverse_1_5", "Higher recoded value means higher perceived AIDS consequence severity.",
  "H1RP4", "It would be a big hassle to do the things necessary to completely protect yourself from getting a sexually transmitted disease.", "STD protection feasibility", "1=strongly agree to 5=strongly disagree", "Higher raw value means lower perceived barrier and greater feasibility of protection.", "keep_original", "Higher value means greater perceived feasibility of STD protection.",
  "H1RP5", "Chance of pregnancy after sexual intercourse once without birth control.", "Pregnancy susceptibility", "1=almost no chance to 5=almost certain", "Higher raw value means higher perceived pregnancy susceptibility.", "keep_original", "Higher value means higher perceived pregnancy susceptibility.",
  "H1RP6", "Chance of AIDS after one month of unprotected sexual intercourse.", "AIDS susceptibility", "1=almost no chance to 5=almost certain", "Higher raw value means higher perceived AIDS susceptibility.", "keep_original", "Higher value means higher perceived AIDS susceptibility."
)

# ------------------------------------------------------------
# 6. Clean and recode items
# ------------------------------------------------------------

s08_row <- raw_df %>%
  transmute(
    respondent_id = as.character(AID),
    H1RP1_raw = clean_h1rp_numeric(H1RP1),
    H1RP2_raw = clean_h1rp_numeric(H1RP2),
    H1RP3_raw = clean_h1rp_numeric(H1RP3),
    H1RP4_raw = clean_h1rp_numeric(H1RP4),
    H1RP5_raw = clean_h1rp_numeric(H1RP5),
    H1RP6_raw = clean_h1rp_numeric(H1RP6),

    H1RP1_pregnancy_worst_rev = reverse_1_5(H1RP1_raw),
    H1RP2_pregnancy_not_bad = H1RP2_raw,
    H1RP3_aids_suffer_rev = reverse_1_5(H1RP3_raw),
    H1RP4_std_protection_feasibility = H1RP4_raw,
    H1RP4_std_protection_barrier = reverse_1_5(H1RP4_raw),
    H1RP5_pregnancy_susceptibility = H1RP5_raw,
    H1RP6_aids_susceptibility = H1RP6_raw
  ) %>%
  left_join(isx_context, by = "respondent_id") %>%
  mutate(
    age_15_19_flag = ifelse(
      !is.na(age_wave1) & age_wave1 >= 15 & age_wave1 <= 19,
      TRUE,
      FALSE
    )
  )

# ------------------------------------------------------------
# 7. Construct Section 8 predictor scores
# ------------------------------------------------------------

s08_predictors <- s08_row %>%
  mutate(
    s08_pregnancy_consequence_valid_items =
      rowSums(
        !is.na(
          cbind(
            H1RP1_pregnancy_worst_rev,
            H1RP2_pregnancy_not_bad
          )
        )
      ),

    s08_pregnancy_consequence_severity_index_1_5 =
  mean_if_min_valid(
    pick(everything()),
    c(
      "H1RP1_pregnancy_worst_rev",
      "H1RP2_pregnancy_not_bad"
    ),
    min_valid = 1
  ),

    s08_pregnancy_consequence_severity_index_strict_1_5 =
  mean_if_min_valid(
    pick(everything()),
    c(
      "H1RP1_pregnancy_worst_rev",
      "H1RP2_pregnancy_not_bad"
    ),
    min_valid = 2
  ),

    s08_aids_consequence_severity_item_1_5 =
      H1RP3_aids_suffer_rev,

    s08_std_protection_feasibility_item_1_5 =
      H1RP4_std_protection_feasibility,

    s08_std_protection_barrier_item_1_5 =
      H1RP4_std_protection_barrier,

    s08_pregnancy_susceptibility_item_1_5 =
      H1RP5_pregnancy_susceptibility,

    s08_aids_susceptibility_item_1_5 =
      H1RP6_aids_susceptibility,

    s08_unprotected_sex_susceptibility_valid_items =
      rowSums(
        !is.na(
          cbind(
            H1RP5_pregnancy_susceptibility,
            H1RP6_aids_susceptibility
          )
        )
      ),

    s08_unprotected_sex_susceptibility_index_1_5 =
  mean_if_min_valid(
    pick(everything()),
    c(
      "H1RP5_pregnancy_susceptibility",
      "H1RP6_aids_susceptibility"
    ),
    min_valid = 1
  ),

    s08_unprotected_sex_susceptibility_index_strict_1_5 =
  mean_if_min_valid(
    pick(everything()),
    c(
      "H1RP5_pregnancy_susceptibility",
      "H1RP6_aids_susceptibility"
    ),
    min_valid = 2
  )
  ) %>%
  mutate(
    across(
      ends_with("_1_5"),
      rescale_1_5_to_0_1,
      .names = "{.col}_0_1"
    ),
    across(
      ends_with("_1_5"),
      z_score,
      .names = "{.col}_z"
    )
  )

# ------------------------------------------------------------
# 8. Reliability by construct
# ------------------------------------------------------------

reliability_pregnancy_consequence <- construct_reliability(
  s08_predictors,
  c(
    "H1RP1_pregnancy_worst_rev",
    "H1RP2_pregnancy_not_bad"
  ),
  "s08_pregnancy_consequence_severity"
)

reliability_aids_consequence <- construct_reliability(
  s08_predictors,
  c("H1RP3_aids_suffer_rev"),
  "s08_aids_consequence_severity"
)

reliability_std_feasibility <- construct_reliability(
  s08_predictors,
  c("H1RP4_std_protection_feasibility"),
  "s08_std_protection_feasibility"
)

reliability_pregnancy_susceptibility <- construct_reliability(
  s08_predictors,
  c("H1RP5_pregnancy_susceptibility"),
  "s08_pregnancy_susceptibility"
)

reliability_aids_susceptibility <- construct_reliability(
  s08_predictors,
  c("H1RP6_aids_susceptibility"),
  "s08_aids_susceptibility"
)

reliability_unprotected_susceptibility <- construct_reliability(
  s08_predictors,
  c(
    "H1RP5_pregnancy_susceptibility",
    "H1RP6_aids_susceptibility"
  ),
  "s08_unprotected_sex_susceptibility_exploratory"
)

construct_reliability_summary <- bind_rows(
  reliability_pregnancy_consequence,
  reliability_aids_consequence,
  reliability_std_feasibility,
  reliability_pregnancy_susceptibility,
  reliability_aids_susceptibility,
  reliability_unprotected_susceptibility
)

# ------------------------------------------------------------
# 9. Score summaries
# ------------------------------------------------------------

construct_score_summary <- bind_rows(
  score_summary(
    s08_predictors,
    "s08_pregnancy_consequence_severity_index_1_5",
    "Pregnancy consequence severity index"
  ),
  score_summary(
    s08_predictors,
    "s08_pregnancy_consequence_severity_index_strict_1_5",
    "Pregnancy consequence severity index, strict two-item score"
  ),
  score_summary(
    s08_predictors,
    "s08_aids_consequence_severity_item_1_5",
    "AIDS consequence severity item"
  ),
  score_summary(
    s08_predictors,
    "s08_std_protection_feasibility_item_1_5",
    "STD protection feasibility item"
  ),
  score_summary(
    s08_predictors,
    "s08_std_protection_barrier_item_1_5",
    "STD protection barrier item"
  ),
  score_summary(
    s08_predictors,
    "s08_pregnancy_susceptibility_item_1_5",
    "Pregnancy susceptibility item"
  ),
  score_summary(
    s08_predictors,
    "s08_aids_susceptibility_item_1_5",
    "AIDS susceptibility item"
  ),
  score_summary(
    s08_predictors,
    "s08_unprotected_sex_susceptibility_index_1_5",
    "Unprotected-sex susceptibility index, exploratory"
  ),
  score_summary(
    s08_predictors,
    "s08_unprotected_sex_susceptibility_index_strict_1_5",
    "Unprotected-sex susceptibility index, strict exploratory"
  )
)

construct_score_summary_15_19 <- s08_predictors %>%
  filter(age_15_19_flag == TRUE) %>%
  {
    bind_rows(
      score_summary(
        .,
        "s08_pregnancy_consequence_severity_index_1_5",
        "Pregnancy consequence severity index, ages 15–19"
      ),
      score_summary(
        .,
        "s08_aids_consequence_severity_item_1_5",
        "AIDS consequence severity item, ages 15–19"
      ),
      score_summary(
        .,
        "s08_std_protection_feasibility_item_1_5",
        "STD protection feasibility item, ages 15–19"
      ),
      score_summary(
        .,
        "s08_pregnancy_susceptibility_item_1_5",
        "Pregnancy susceptibility item, ages 15–19"
      ),
      score_summary(
        .,
        "s08_aids_susceptibility_item_1_5",
        "AIDS susceptibility item, ages 15–19"
      ),
      score_summary(
        .,
        "s08_unprotected_sex_susceptibility_index_1_5",
        "Unprotected-sex susceptibility index, exploratory, ages 15–19"
      )
    )
  }

# ------------------------------------------------------------
# 10. Item recoding audit
# ------------------------------------------------------------

item_recoding_audit <- s08_codebook %>%
  mutate(
    raw_valid_n = purrr::map_int(
      variable,
      ~ sum(!is.na(clean_h1rp_numeric(raw_df[[.x]])))
    ),
    raw_missing_n = purrr::map_int(
      variable,
      ~ sum(is.na(clean_h1rp_numeric(raw_df[[.x]])))
    ),
    raw_missing_rate = round(raw_missing_n / nrow(raw_df), 4)
  )

item_distribution_after_cleaning <- purrr::map_dfr(
  s08_items,
  function(v) {

    raw_num <- clean_h1rp_numeric(raw_df[[v]])

    tibble(
      variable = v,
      value = raw_num
    ) %>%
      count(variable, value, name = "n") %>%
      group_by(variable) %>%
      mutate(percent = round(100 * n / sum(n), 2)) %>%
      ungroup()
  }
)

construct_correlation_matrix <- s08_predictors %>%
  select(
    s08_pregnancy_consequence_severity_index_1_5,
    s08_aids_consequence_severity_item_1_5,
    s08_std_protection_feasibility_item_1_5,
    s08_pregnancy_susceptibility_item_1_5,
    s08_aids_susceptibility_item_1_5,
    s08_unprotected_sex_susceptibility_index_1_5
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable")

# ------------------------------------------------------------
# 11. Construct decision table
# ------------------------------------------------------------

construct_decision_table <- tibble::tribble(
  ~construct, ~final_variable, ~items_used, ~alpha_applicable, ~default_model_role, ~decision,
  "Pregnancy consequence severity", "s08_pregnancy_consequence_severity_index_1_5", "H1RP1 reversed + H1RP2", TRUE, "main_section8_predictor", "Retain if alpha and item correlation are acceptable or if theoretically essential.",
  "AIDS consequence severity", "s08_aids_consequence_severity_item_1_5", "H1RP3 reversed", FALSE, "main_section8_predictor", "Retain as single-item predictor; alpha is not applicable.",
  "STD protection feasibility", "s08_std_protection_feasibility_item_1_5", "H1RP4 original", FALSE, "main_section8_predictor", "Retain as single-item predictor; higher value means greater perceived feasibility of protection.",
  "Pregnancy susceptibility", "s08_pregnancy_susceptibility_item_1_5", "H1RP5 original", FALSE, "main_section8_predictor", "Retain as single-item predictor.",
  "AIDS susceptibility", "s08_aids_susceptibility_item_1_5", "H1RP6 original", FALSE, "main_section8_predictor", "Retain as single-item predictor.",
  "Unprotected-sex susceptibility", "s08_unprotected_sex_susceptibility_index_1_5", "H1RP5 + H1RP6", TRUE, "exploratory_sensitivity_predictor", "Use only as exploratory or sensitivity predictor if reliability is acceptable."
) %>%
  left_join(
    construct_reliability_summary %>%
      select(
        construct_reliability_name = construct,
        cronbach_alpha,
        item_correlation,
        reliability_note
      ),
    by = c("final_variable" = "construct_reliability_name")
  )

# The join above will not match because reliability construct names
# differ from final variable names. Build a cleaner manual link.
construct_decision_table <- tibble::tribble(
  ~construct, ~reliability_construct, ~final_variable, ~items_used, ~alpha_applicable, ~default_model_role, ~decision,
  "Pregnancy consequence severity", "s08_pregnancy_consequence_severity", "s08_pregnancy_consequence_severity_index_1_5", "H1RP1 reversed + H1RP2", TRUE, "main_section8_predictor", "Retain if alpha and item correlation are acceptable or if theoretically essential.",
  "AIDS consequence severity", "s08_aids_consequence_severity", "s08_aids_consequence_severity_item_1_5", "H1RP3 reversed", FALSE, "main_section8_predictor", "Retain as single-item predictor; alpha is not applicable.",
  "STD protection feasibility", "s08_std_protection_feasibility", "s08_std_protection_feasibility_item_1_5", "H1RP4 original", FALSE, "main_section8_predictor", "Retain as single-item predictor; higher value means greater perceived feasibility of protection.",
  "Pregnancy susceptibility", "s08_pregnancy_susceptibility", "s08_pregnancy_susceptibility_item_1_5", "H1RP5 original", FALSE, "main_section8_predictor", "Retain as single-item predictor.",
  "AIDS susceptibility", "s08_aids_susceptibility", "s08_aids_susceptibility_item_1_5", "H1RP6 original", FALSE, "main_section8_predictor", "Retain as single-item predictor.",
  "Unprotected-sex susceptibility", "s08_unprotected_sex_susceptibility_exploratory", "s08_unprotected_sex_susceptibility_index_1_5", "H1RP5 + H1RP6", TRUE, "exploratory_sensitivity_predictor", "Use only as exploratory or sensitivity predictor if reliability is acceptable."
) %>%
  left_join(
    construct_reliability_summary %>%
      select(
        reliability_construct = construct,
        item_count,
        complete_case_n,
        cronbach_alpha,
        item_correlation,
        reliability_note
      ),
    by = "reliability_construct"
  ) %>%
  mutate(
    retain_recommendation = case_when(
      alpha_applicable == FALSE ~ "retain_single_item_construct",
      alpha_applicable == TRUE &
        !is.na(cronbach_alpha) &
        cronbach_alpha >= 0.60 ~ "retain_index_construct",
      alpha_applicable == TRUE &
        !is.na(item_correlation) &
        item_correlation >= 0.30 ~ "retain_index_construct_based_on_two_item_correlation",
      alpha_applicable == TRUE ~ "manual_review_or_use_items_separately",
      TRUE ~ "review"
    )
  )

# ------------------------------------------------------------
# 12. Methodological decisions
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Section 8 scope", "Section 8 is treated as a set of perceived-risk constructs rather than one general index.",
  "Predictor logic", "Each Section 8 construct is intended to enter later models as an independent predictor of the restricted ISX sexual protection index.",
  "Pregnancy consequence severity", "H1RP1 is reverse-coded and averaged with H1RP2 to capture perceived severity of pregnancy consequences.",
  "AIDS consequence severity", "H1RP3 is reverse-coded and retained as a single-item perceived AIDS consequence severity predictor.",
  "STD protection feasibility", "H1RP4 is retained in the original direction so that higher values mean greater perceived feasibility of protection.",
  "Pregnancy susceptibility", "H1RP5 is retained in the original direction so that higher values mean greater perceived pregnancy risk after unprotected sex.",
  "AIDS susceptibility", "H1RP6 is retained in the original direction so that higher values mean greater perceived AIDS risk after unprotected sex.",
  "Susceptibility index", "H1RP5 and H1RP6 are also tested as an exploratory two-item susceptibility index, but the default modelling approach retains pregnancy and AIDS susceptibility as separate predictors unless reliability supports aggregation.",
  "Reliability rule", "Cronbach alpha is computed only for constructs with two or more items. Single-item constructs are evaluated by content validity, missingness and later model behavior.",
  "Data protection", "The row-level Section 8 predictor file is LOCAL_ONLY and should not be committed to GitHub."
)

# ------------------------------------------------------------
# 13. Write row-level LOCAL_ONLY output
# ------------------------------------------------------------

s08_predictor_output <- s08_predictors %>%
  select(
    respondent_id,
    age_wave1,
    age_15_19_flag,
    survey_weight,

    H1RP1_raw,
    H1RP2_raw,
    H1RP3_raw,
    H1RP4_raw,
    H1RP5_raw,
    H1RP6_raw,

    H1RP1_pregnancy_worst_rev,
    H1RP2_pregnancy_not_bad,
    H1RP3_aids_suffer_rev,
    H1RP4_std_protection_feasibility,
    H1RP4_std_protection_barrier,
    H1RP5_pregnancy_susceptibility,
    H1RP6_aids_susceptibility,

    s08_pregnancy_consequence_valid_items,
    s08_pregnancy_consequence_severity_index_1_5,
    s08_pregnancy_consequence_severity_index_strict_1_5,
    s08_aids_consequence_severity_item_1_5,
    s08_std_protection_feasibility_item_1_5,
    s08_std_protection_barrier_item_1_5,
    s08_pregnancy_susceptibility_item_1_5,
    s08_aids_susceptibility_item_1_5,
    s08_unprotected_sex_susceptibility_valid_items,
    s08_unprotected_sex_susceptibility_index_1_5,
    s08_unprotected_sex_susceptibility_index_strict_1_5,

    ends_with("_0_1"),
    ends_with("_z")
  )

row_level_path <- file.path(
  indices_dir,
  "script18d_s08_perceived_risk_predictors_LOCAL_ONLY.csv"
)

readr::write_csv(
  s08_predictor_output,
  row_level_path
)

# ------------------------------------------------------------
# 14. Write audit outputs
# ------------------------------------------------------------

readr::write_csv(
  s08_codebook,
  file.path(audit_dir, "script18d_s08_codebook_mapping.csv")
)

readr::write_csv(
  item_recoding_audit,
  file.path(audit_dir, "script18d_s08_item_recoding_audit.csv")
)

readr::write_csv(
  item_distribution_after_cleaning,
  file.path(audit_dir, "script18d_s08_item_distributions_after_cleaning.csv")
)

readr::write_csv(
  construct_reliability_summary,
  file.path(audit_dir, "script18d_s08_construct_reliability_summary.csv")
)

readr::write_csv(
  construct_score_summary,
  file.path(audit_dir, "script18d_s08_construct_score_summary.csv")
)

readr::write_csv(
  construct_score_summary_15_19,
  file.path(audit_dir, "script18d_s08_construct_score_summary_15_19.csv")
)

readr::write_csv(
  construct_correlation_matrix,
  file.path(audit_dir, "script18d_s08_construct_correlation_matrix.csv")
)

readr::write_csv(
  construct_decision_table,
  file.path(audit_dir, "script18d_s08_construct_decision_table.csv")
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18d_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 15. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_section8_construct_reliability_predictors_script18d.docx"
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
      "Add Health Wave I — Section 8 Construct Reliability and Predictors",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18d constructs separate Section 8 perceived-risk predictors rather than a single general index. These predictors are intended for later models of the restricted ISX sexual protection index.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Codebook mapping and recoding", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(s08_codebook)) %>%
    officer::body_add_par("Item recoding audit", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(item_recoding_audit)) %>%
    officer::body_add_par("Construct reliability summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(construct_reliability_summary)) %>%
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
    "all_section8_items_available",
    "section8_predictor_file_created",
    "codebook_mapping_created",
    "item_recoding_audit_created",
    "construct_reliability_summary_created",
    "construct_score_summary_created",
    "construct_score_summary_15_19_created",
    "construct_decision_table_created",
    "methodological_decisions_created",
    "word_report_created",
    "ready_for_section8_review"
  ),
  status = c(
    exists("raw_df") && nrow(raw_df) > 0,
    all(s08_items %in% names(raw_df)),
    file.exists(row_level_path),
    file.exists(file.path(audit_dir, "script18d_s08_codebook_mapping.csv")),
    file.exists(file.path(audit_dir, "script18d_s08_item_recoding_audit.csv")),
    file.exists(file.path(audit_dir, "script18d_s08_construct_reliability_summary.csv")),
    file.exists(file.path(audit_dir, "script18d_s08_construct_score_summary.csv")),
    file.exists(file.path(audit_dir, "script18d_s08_construct_score_summary_15_19.csv")),
    file.exists(file.path(audit_dir, "script18d_s08_construct_decision_table.csv")),
    file.exists(file.path(audit_dir, "script18d_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18d_final_status.csv")
)

# ------------------------------------------------------------
# 17. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18d completed: Section 8 Construct Reliability and Predictors\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nItem recoding audit:\n")
print(item_recoding_audit, n = Inf)

cat("\nConstruct reliability summary:\n")
print(construct_reliability_summary, n = Inf)

cat("\nConstruct score summary:\n")
print(construct_score_summary, n = Inf)

cat("\nConstruct score summary, ages 15–19:\n")
print(construct_score_summary_15_19, n = Inf)

cat("\nConstruct decision table:\n")
print(construct_decision_table, n = Inf)

cat("\nConstruct correlation matrix:\n")
print(as.data.frame(construct_correlation_matrix))

cat("\nMethodological decisions:\n")
print(methodological_decisions, n = Inf)

cat("\nOutputs created:\n")
cat("- ", row_level_path, "\n")
cat("- ", file.path(audit_dir, "script18d_s08_codebook_mapping.csv"), "\n")
cat("- ", file.path(audit_dir, "script18d_s08_item_recoding_audit.csv"), "\n")
cat("- ", file.path(audit_dir, "script18d_s08_item_distributions_after_cleaning.csv"), "\n")
cat("- ", file.path(audit_dir, "script18d_s08_construct_reliability_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18d_s08_construct_score_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18d_s08_construct_score_summary_15_19.csv"), "\n")
cat("- ", file.path(audit_dir, "script18d_s08_construct_correlation_matrix.csv"), "\n")
cat("- ", file.path(audit_dir, "script18d_s08_construct_decision_table.csv"), "\n")
cat("- ", file.path(audit_dir, "script18d_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18d_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nImportant Git note:\n")
cat("Do not commit the LOCAL_ONLY row-level predictor file.\n")
cat("Review reliability and construct decisions before moving to knowledge and self-efficacy blocks.\n")