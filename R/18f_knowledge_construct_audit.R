# ============================================================
# Script 18f — Section 19 Knowledge Construct Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Construct the Section 19 contraception/birth-control knowledge
#   predictor block for later regression models.
#
# Section 19: Knowledge Quiz
#   Administered only to respondents aged 15 or older.
#
# Main knowledge items:
#   H1KQ1A–H1KQ10A
#
# Confidence items:
#   H1KQ1B–H1KQ10B
#
# Main scoring:
#   Correct answer = 1
#   Incorrect answer = 0
#   Don't know = 0 in the main conservative score
#   Refused / legitimate skip / not applicable = missing
#
# Sensitivity scoring:
#   Don't know = missing in answered-only score
#
# Main outputs:
#   outputs/indices/script18f_s19_knowledge_predictors_LOCAL_ONLY.csv
#   outputs/audits/script18f_s19_codebook_mapping.csv
#   outputs/audits/script18f_s19_item_scoring_audit.csv
#   outputs/audits/script18f_s19_item_difficulty.csv
#   outputs/audits/script18f_s19_construct_reliability_summary.csv
#   outputs/audits/script18f_s19_alpha_if_deleted.csv
#   outputs/audits/script18f_s19_item_total_correlations.csv
#   outputs/audits/script18f_s19_construct_score_summary.csv
#   outputs/audits/script18f_s19_construct_score_summary_15_19.csv
#   outputs/audits/script18f_s19_construct_decision_table.csv
#   outputs/audits/script18f_methodological_decisions.csv
#   outputs/audits/script18f_final_status.csv
#   docs/add_health_wave01_section19_knowledge_construct_audit_script18f.docx
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
cat("Script 18f started: Section 19 Knowledge Construct Audit\n")
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

score_knowledge_main <- function(x, correct_code) {

  x_num <- extract_numeric_code(x)

  dplyr::case_when(
    x_num == correct_code ~ 1,
    x_num %in% c(1, 2) & x_num != correct_code ~ 0,
    x_num == 8 ~ 0,                 # don't know = lack of knowledge in main score
    x_num %in% c(6, 7, 9, 96, 97, 99) ~ NA_real_,
    is.na(x_num) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

score_knowledge_answered_only <- function(x, correct_code) {

  x_num <- extract_numeric_code(x)

  dplyr::case_when(
    x_num == correct_code ~ 1,
    x_num %in% c(1, 2) & x_num != correct_code ~ 0,
    x_num == 8 ~ NA_real_,          # don't know = missing in sensitivity score
    x_num %in% c(6, 7, 9, 96, 97, 99) ~ NA_real_,
    is.na(x_num) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

clean_confidence_numeric <- function(x) {

  x_num <- extract_numeric_code(x)

  # Confidence scale:
  # 1 = very
  # 2 = moderately
  # 3 = slightly
  # 4 = not at all
  # 6/7/8/9 = non-substantive for confidence score
  x_num[!(x_num %in% 1:4)] <- NA_real_

  x_num
}

reverse_confidence_1_4 <- function(x) {
  ifelse(is.na(x), NA_real_, 5 - x)
}

rescale_0_10_to_0_1 <- function(x) {
  ifelse(is.na(x), NA_real_, x / 10)
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

sum_if_min_valid <- function(df, vars, min_valid = 1) {

  valid_count <- rowSums(!is.na(df[, vars, drop = FALSE]))

  out <- rowSums(df[, vars, drop = FALSE], na.rm = TRUE)
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
  other_score <- rowSums(df_items[, other_items, drop = FALSE], na.rm = TRUE)

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

answer_vars <- paste0("H1KQ", 1:10, "A")
confidence_vars <- paste0("H1KQ", 1:10, "B")

required_vars <- c("AID", answer_vars, confidence_vars)

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
# 5. Codebook mapping and correct answers
# ------------------------------------------------------------

knowledge_key <- tibble::tribble(
  ~item_number, ~answer_var, ~confidence_var, ~item_text, ~correct_code, ~correct_answer,
  1, "H1KQ1A",  "H1KQ1B",  "Almost all sperm die inside the woman's body after about six hours.", 2, "false",
  2, "H1KQ2A",  "H1KQ2B",  "When using a condom, the man should pull out right after ejaculation.", 1, "true",
  3, "H1KQ3A",  "H1KQ3B",  "Most women ovulate fourteen days after their periods begin.", 2, "false",
  4, "H1KQ4A",  "H1KQ4B",  "Natural skin condoms provide better AIDS protection than latex condoms.", 2, "false",
  5, "H1KQ5A",  "H1KQ5B",  "A condom should fit tightly, leaving no space at the tip.", 2, "false",
  6, "H1KQ6A",  "H1KQ6B",  "Vaseline can be used with condoms and they will work just as well.", 2, "false",
  7, "H1KQ7A",  "H1KQ7B",  "The most likely time for pregnancy is right before the period starts.", 2, "false",
  8, "H1KQ8A",  "H1KQ8B",  "Pregnancy is still possible even if the man pulls out before ejaculation.", 1, "true",
  9, "H1KQ9A",  "H1KQ9B",  "If the condom fits over the tip of the penis, unrolling distance does not matter.", 2, "false",
  10, "H1KQ10A", "H1KQ10B", "A woman is most likely to get pregnant during her period.", 2, "false"
) %>%
  mutate(
    section = "Section 19 — Knowledge Quiz",
    construct_domain = "Contraception and birth-control knowledge",
    main_scoring = "Correct=1; incorrect=0; don't know=0; refused/skip/not applicable=missing.",
    sensitivity_scoring = "Correct=1; incorrect=0; don't know=missing; refused/skip/not applicable=missing."
  )

# ------------------------------------------------------------
# 6. Score knowledge and confidence items
# ------------------------------------------------------------

s19_base <- raw_df %>%
  transmute(respondent_id = as.character(AID))

for (i in seq_len(nrow(knowledge_key))) {

  avar <- knowledge_key$answer_var[i]
  bvar <- knowledge_key$confidence_var[i]
  correct_code <- knowledge_key$correct_code[i]

  raw_answer <- extract_numeric_code(raw_df[[avar]])
  raw_conf <- clean_confidence_numeric(raw_df[[bvar]])

  s19_base[[paste0(avar, "_raw")]] <- raw_answer
  s19_base[[paste0(avar, "_correct_main")]] <- score_knowledge_main(raw_df[[avar]], correct_code)
  s19_base[[paste0(avar, "_correct_answered_only")]] <- score_knowledge_answered_only(raw_df[[avar]], correct_code)

  s19_base[[paste0(bvar, "_confidence_raw")]] <- raw_conf
  s19_base[[paste0(bvar, "_confidence_rev")]] <- reverse_confidence_1_4(raw_conf)
}

main_correct_vars <- paste0(answer_vars, "_correct_main")
answered_only_correct_vars <- paste0(answer_vars, "_correct_answered_only")
confidence_rev_vars <- paste0(confidence_vars, "_confidence_rev")

s19_predictors <- s19_base %>%
  left_join(isx_context, by = "respondent_id") %>%
  mutate(
    age_15_19_flag = ifelse(
      !is.na(age_wave1) & age_wave1 >= 15 & age_wave1 <= 19,
      TRUE,
      FALSE
    ),

    s19_knowledge_valid_items_main =
      rowSums(!is.na(across(all_of(main_correct_vars)))),

    s19_knowledge_correct_count_0_10 =
      sum_if_min_valid(
        .,
        main_correct_vars,
        min_valid = 7
      ),

    s19_knowledge_proportion_correct_0_1 =
      s19_knowledge_correct_count_0_10 /
        s19_knowledge_valid_items_main,

    s19_knowledge_score_z =
      z_score(s19_knowledge_correct_count_0_10),

    s19_knowledge_valid_items_answered_only =
      rowSums(!is.na(across(all_of(answered_only_correct_vars)))),

    s19_knowledge_correct_count_answered_only_0_10 =
      sum_if_min_valid(
        .,
        answered_only_correct_vars,
        min_valid = 7
      ),

    s19_knowledge_proportion_answered_only_0_1 =
      s19_knowledge_correct_count_answered_only_0_10 /
        s19_knowledge_valid_items_answered_only,

    s19_knowledge_confidence_valid_items =
      rowSums(!is.na(across(all_of(confidence_rev_vars)))),

    s19_knowledge_confidence_index_1_4 =
      mean_if_min_valid(
        .,
        confidence_rev_vars,
        min_valid = 7
      ),

    s19_knowledge_confidence_index_0_1 =
      ifelse(
        is.na(s19_knowledge_confidence_index_1_4),
        NA_real_,
        (s19_knowledge_confidence_index_1_4 - 1) / 3
      ),

    s19_knowledge_confidence_index_z =
      z_score(s19_knowledge_confidence_index_1_4)
  )

# ------------------------------------------------------------
# 7. Reliability analysis: main knowledge score
# ------------------------------------------------------------

knowledge_items_df <- s19_predictors %>%
  select(all_of(main_correct_vars))

complete_case_n <- knowledge_items_df %>%
  filter(if_all(everything(), ~ !is.na(.x))) %>%
  nrow()

alpha_all <- cronbach_alpha(knowledge_items_df)
std_alpha_all <- standardized_alpha(knowledge_items_df)

item_total_correlations <- tibble(
  variable = main_correct_vars,
  corrected_item_total_correlation = purrr::map_dbl(
    main_correct_vars,
    ~ item_total_correlation(knowledge_items_df, .x)
  )
)

alpha_if_deleted <- purrr::map_dfr(
  main_correct_vars,
  function(v) {

    remaining <- setdiff(main_correct_vars, v)

    tibble(
      deleted_item = v,
      remaining_items = paste(remaining, collapse = ", "),
      alpha_if_deleted = cronbach_alpha(
        knowledge_items_df[, remaining, drop = FALSE]
      ),
      standardized_alpha_if_deleted = standardized_alpha(
        knowledge_items_df[, remaining, drop = FALSE]
      )
    )
  }
)

correlation_matrix_df <- suppressWarnings(
  cor(knowledge_items_df, use = "pairwise.complete.obs")
) %>%
  as.data.frame() %>%
  rownames_to_column("variable")

construct_reliability_summary <- tibble(
  construct = "s19_contraception_birth_control_knowledge",
  items = paste(main_correct_vars, collapse = ", "),
  item_count = length(main_correct_vars),
  complete_case_n = complete_case_n,
  cronbach_alpha = alpha_all,
  standardized_alpha = std_alpha_all,
  reliability_note = case_when(
    is.na(alpha_all) ~ "Cronbach alpha could not be computed.",
    alpha_all >= 0.70 ~ "Acceptable internal consistency.",
    alpha_all >= 0.60 & alpha_all < 0.70 ~ "Exploratory or marginal internal consistency.",
    alpha_all < 0.60 ~ "Low internal consistency; acceptable only as a factual quiz score if theoretically justified.",
    TRUE ~ "Review."
  )
)

# ------------------------------------------------------------
# 8. Item scoring audit and item difficulty
# ------------------------------------------------------------

item_scoring_audit <- knowledge_key %>%
  mutate(
    raw_valid_true_false_or_dk_n = purrr::map_int(
      answer_var,
      ~ sum(extract_numeric_code(raw_df[[.x]]) %in% c(1, 2, 8), na.rm = TRUE)
    ),
    raw_answered_true_false_n = purrr::map_int(
      answer_var,
      ~ sum(extract_numeric_code(raw_df[[.x]]) %in% c(1, 2), na.rm = TRUE)
    ),
    raw_missing_or_skip_n = nrow(raw_df) - raw_valid_true_false_or_dk_n,
    raw_missing_or_skip_rate = round(raw_missing_or_skip_n / nrow(raw_df), 4)
  )

item_difficulty <- knowledge_key %>%
  mutate(
    score_var = paste0(answer_var, "_correct_main"),
    answered_only_score_var = paste0(answer_var, "_correct_answered_only"),
    valid_n_main = purrr::map_int(
      score_var,
      ~ sum(!is.na(s19_predictors[[.x]]))
    ),
    correct_n_main = purrr::map_int(
      score_var,
      ~ sum(s19_predictors[[.x]] == 1, na.rm = TRUE)
    ),
    proportion_correct_main = round(correct_n_main / valid_n_main, 4),
    valid_n_answered_only = purrr::map_int(
      answered_only_score_var,
      ~ sum(!is.na(s19_predictors[[.x]]))
    ),
    correct_n_answered_only = purrr::map_int(
      answered_only_score_var,
      ~ sum(s19_predictors[[.x]] == 1, na.rm = TRUE)
    ),
    proportion_correct_answered_only =
      round(correct_n_answered_only / valid_n_answered_only, 4)
  ) %>%
  select(
    item_number,
    answer_var,
    item_text,
    correct_answer,
    valid_n_main,
    correct_n_main,
    proportion_correct_main,
    valid_n_answered_only,
    correct_n_answered_only,
    proportion_correct_answered_only
  )

answer_distribution_after_scoring <- purrr::map_dfr(
  seq_len(nrow(knowledge_key)),
  function(i) {

    avar <- knowledge_key$answer_var[i]
    correct_code <- knowledge_key$correct_code[i]

    tibble(
      variable = avar,
      raw_value = as.character(raw_df[[avar]]),
      raw_numeric = extract_numeric_code(raw_df[[avar]]),
      correct_code = correct_code,
      correct_main = score_knowledge_main(raw_df[[avar]], correct_code),
      correct_answered_only = score_knowledge_answered_only(raw_df[[avar]], correct_code)
    ) %>%
      count(
        variable,
        raw_value,
        raw_numeric,
        correct_code,
        correct_main,
        correct_answered_only,
        name = "n"
      ) %>%
      group_by(variable) %>%
      mutate(percent = round(100 * n / sum(n), 2)) %>%
      ungroup()
  }
)

# ------------------------------------------------------------
# 9. Score summaries
# ------------------------------------------------------------

construct_score_summary <- bind_rows(
  score_summary(
    s19_predictors,
    "s19_knowledge_correct_count_0_10",
    "Contraception and birth-control knowledge, correct count 0–10"
  ),
  score_summary(
    s19_predictors,
    "s19_knowledge_proportion_correct_0_1",
    "Contraception and birth-control knowledge, proportion correct 0–1"
  ),
  score_summary(
    s19_predictors,
    "s19_knowledge_score_z",
    "Contraception and birth-control knowledge, z-score"
  ),
  score_summary(
    s19_predictors,
    "s19_knowledge_correct_count_answered_only_0_10",
    "Knowledge score, answered-only sensitivity count 0–10"
  ),
  score_summary(
    s19_predictors,
    "s19_knowledge_proportion_answered_only_0_1",
    "Knowledge score, answered-only sensitivity proportion 0–1"
  ),
  score_summary(
    s19_predictors,
    "s19_knowledge_confidence_index_1_4",
    "Knowledge confidence index 1–4"
  ),
  score_summary(
    s19_predictors,
    "s19_knowledge_confidence_index_0_1",
    "Knowledge confidence index 0–1"
  )
)

construct_score_summary_15_19 <- s19_predictors %>%
  filter(age_15_19_flag == TRUE) %>%
  {
    bind_rows(
      score_summary(
        .,
        "s19_knowledge_correct_count_0_10",
        "Knowledge correct count 0–10, ages 15–19"
      ),
      score_summary(
        .,
        "s19_knowledge_proportion_correct_0_1",
        "Knowledge proportion correct 0–1, ages 15–19"
      ),
      score_summary(
        .,
        "s19_knowledge_correct_count_answered_only_0_10",
        "Knowledge answered-only count 0–10, ages 15–19"
      ),
      score_summary(
        .,
        "s19_knowledge_proportion_answered_only_0_1",
        "Knowledge answered-only proportion 0–1, ages 15–19"
      ),
      score_summary(
        .,
        "s19_knowledge_confidence_index_1_4",
        "Knowledge confidence index 1–4, ages 15–19"
      )
    )
  }

# ------------------------------------------------------------
# 10. Construct decision table
# ------------------------------------------------------------

construct_decision_table <- tibble::tribble(
  ~construct, ~final_variable, ~items_used, ~alpha_applicable, ~default_model_role, ~expected_sign, ~decision,
  "Contraception and birth-control knowledge",
  "s19_knowledge_correct_count_0_10",
  "H1KQ1A–H1KQ10A scored as correct/incorrect; don't know scored as incorrect in main score",
  TRUE,
  "main_knowledge_predictor",
  "positive",
  "Retain as main factual knowledge predictor. Higher score means more correct contraception and birth-control knowledge.",

  "Contraception and birth-control knowledge proportion",
  "s19_knowledge_proportion_correct_0_1",
  "Same items as count score, rescaled by valid items",
  TRUE,
  "alternative_main_knowledge_predictor",
  "positive",
  "Use instead of count score if a 0–1 metric is preferred.",

  "Answered-only knowledge sensitivity score",
  "s19_knowledge_correct_count_answered_only_0_10",
  "H1KQ1A–H1KQ10A, with don't know treated as missing",
  TRUE,
  "sensitivity_predictor",
  "positive",
  "Use for sensitivity analysis to test whether treating don't know as missing changes results.",

  "Knowledge confidence",
  "s19_knowledge_confidence_index_1_4",
  "H1KQ1B–H1KQ10B reverse-coded confidence items",
  TRUE,
  "optional_confidence_predictor",
  "positive_or_review",
  "Do not treat as factual knowledge. Use only as a separate confidence construct if theoretically justified."
) %>%
  mutate(
    cronbach_alpha_main_knowledge = alpha_all,
    standardized_alpha_main_knowledge = std_alpha_all,
    complete_case_n_main_knowledge = complete_case_n
  )

# ------------------------------------------------------------
# 11. Methodological decisions
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Section 19 scope", "Section 19 is treated as a factual contraception and birth-control knowledge quiz.",
  "Included knowledge items", "H1KQ1A through H1KQ10A are used as the main factual knowledge items.",
  "Correct answer coding", "Each item is scored 1 if correct and 0 if incorrect.",
  "Don't know treatment", "In the main score, don't know is scored as incorrect because it reflects lack of factual knowledge. A sensitivity score treats don't know as missing.",
  "Non-substantive missing", "Refused, legitimate skip and not applicable are treated as missing.",
  "Main score", "The main score is the number of correct answers from 0 to 10, requiring at least seven valid items.",
  "Confidence items", "H1KQ1B through H1KQ10B are not factual knowledge items. They are retained only as an optional confidence construct.",
  "Expected regression sign", "The expected coefficient sign is positive when the dependent variable measures greater sexual protection.",
  "Data protection", "The row-level knowledge predictor file is LOCAL_ONLY and should not be committed to GitHub."
)

# ------------------------------------------------------------
# 12. Write row-level LOCAL_ONLY output
# ------------------------------------------------------------

s19_predictor_output <- s19_predictors %>%
  select(
    respondent_id,
    age_wave1,
    age_15_19_flag,
    survey_weight,

    ends_with("_raw"),
    all_of(main_correct_vars),
    all_of(answered_only_correct_vars),
    all_of(confidence_rev_vars),

    s19_knowledge_valid_items_main,
    s19_knowledge_correct_count_0_10,
    s19_knowledge_proportion_correct_0_1,
    s19_knowledge_score_z,

    s19_knowledge_valid_items_answered_only,
    s19_knowledge_correct_count_answered_only_0_10,
    s19_knowledge_proportion_answered_only_0_1,

    s19_knowledge_confidence_valid_items,
    s19_knowledge_confidence_index_1_4,
    s19_knowledge_confidence_index_0_1,
    s19_knowledge_confidence_index_z
  )

row_level_path <- file.path(
  indices_dir,
  "script18f_s19_knowledge_predictors_LOCAL_ONLY.csv"
)

readr::write_csv(
  s19_predictor_output,
  row_level_path
)

# ------------------------------------------------------------
# 13. Write audit outputs
# ------------------------------------------------------------

readr::write_csv(
  knowledge_key,
  file.path(audit_dir, "script18f_s19_codebook_mapping.csv")
)

readr::write_csv(
  item_scoring_audit,
  file.path(audit_dir, "script18f_s19_item_scoring_audit.csv")
)

readr::write_csv(
  item_difficulty,
  file.path(audit_dir, "script18f_s19_item_difficulty.csv")
)

readr::write_csv(
  answer_distribution_after_scoring,
  file.path(audit_dir, "script18f_s19_answer_distribution_after_scoring.csv")
)

readr::write_csv(
  construct_reliability_summary,
  file.path(audit_dir, "script18f_s19_construct_reliability_summary.csv")
)

readr::write_csv(
  alpha_if_deleted,
  file.path(audit_dir, "script18f_s19_alpha_if_deleted.csv")
)

readr::write_csv(
  item_total_correlations,
  file.path(audit_dir, "script18f_s19_item_total_correlations.csv")
)

readr::write_csv(
  correlation_matrix_df,
  file.path(audit_dir, "script18f_s19_correlation_matrix.csv")
)

readr::write_csv(
  construct_score_summary,
  file.path(audit_dir, "script18f_s19_construct_score_summary.csv")
)

readr::write_csv(
  construct_score_summary_15_19,
  file.path(audit_dir, "script18f_s19_construct_score_summary_15_19.csv")
)

readr::write_csv(
  construct_decision_table,
  file.path(audit_dir, "script18f_s19_construct_decision_table.csv")
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18f_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 14. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_section19_knowledge_construct_audit_script18f.docx"
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
      "Add Health Wave I — Section 19 Knowledge Construct Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18f constructs a factual contraception and birth-control knowledge score from H1KQ1A–H1KQ10A. Confidence items H1KQ1B–H1KQ10B are retained only as an optional confidence construct.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Codebook mapping and correct answers", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(knowledge_key)) %>%
    officer::body_add_par("Item difficulty", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(item_difficulty)) %>%
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
# 15. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "raw_file_loaded",
    "all_section19_answer_items_available",
    "all_section19_confidence_items_available",
    "knowledge_predictor_file_created",
    "codebook_mapping_created",
    "item_scoring_audit_created",
    "item_difficulty_created",
    "construct_reliability_summary_created",
    "alpha_if_deleted_created",
    "item_total_correlations_created",
    "construct_score_summary_created",
    "construct_score_summary_15_19_created",
    "construct_decision_table_created",
    "methodological_decisions_created",
    "word_report_created",
    "ready_for_section19_review"
  ),
  status = c(
    exists("raw_df") && nrow(raw_df) > 0,
    all(answer_vars %in% names(raw_df)),
    all(confidence_vars %in% names(raw_df)),
    file.exists(row_level_path),
    file.exists(file.path(audit_dir, "script18f_s19_codebook_mapping.csv")),
    file.exists(file.path(audit_dir, "script18f_s19_item_scoring_audit.csv")),
    file.exists(file.path(audit_dir, "script18f_s19_item_difficulty.csv")),
    file.exists(file.path(audit_dir, "script18f_s19_construct_reliability_summary.csv")),
    file.exists(file.path(audit_dir, "script18f_s19_alpha_if_deleted.csv")),
    file.exists(file.path(audit_dir, "script18f_s19_item_total_correlations.csv")),
    file.exists(file.path(audit_dir, "script18f_s19_construct_score_summary.csv")),
    file.exists(file.path(audit_dir, "script18f_s19_construct_score_summary_15_19.csv")),
    file.exists(file.path(audit_dir, "script18f_s19_construct_decision_table.csv")),
    file.exists(file.path(audit_dir, "script18f_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18f_final_status.csv")
)

# ------------------------------------------------------------
# 16. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18f completed: Section 19 Knowledge Construct Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nKnowledge key:\n")
print(knowledge_key, n = Inf)

cat("\nItem difficulty:\n")
print(item_difficulty, n = Inf)

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

cat("\nMethodological decisions:\n")
print(methodological_decisions, n = Inf)

cat("\nOutputs created:\n")
cat("- ", row_level_path, "\n")
cat("- ", file.path(audit_dir, "script18f_s19_codebook_mapping.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_item_scoring_audit.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_item_difficulty.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_answer_distribution_after_scoring.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_construct_reliability_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_alpha_if_deleted.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_item_total_correlations.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_correlation_matrix.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_construct_score_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_construct_score_summary_15_19.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_s19_construct_decision_table.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18f_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nImportant Git note:\n")
cat("Do not commit the LOCAL_ONLY row-level predictor file.\n")
cat("Review alpha, item difficulty and score summaries before moving to cues to action.\n")