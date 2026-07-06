# ============================================================
# Project: add-health-adolescent-risk-models
# Script 13b: Results Interpretation Report
# Author: Gelo Picol
#
# Purpose:
#   Create the public-facing interpretation report, aligned with
#   the socioecological logic of the original thesis.
#
# Difference from Script 13:
#   - Script 13 = technical reproducibility/manuscript file.
#   - Script 13b = public-facing interpretation report.
#
# Important:
#   - Reads only public aggregate outputs.
#   - Does not read individual-level data.
#   - Does not export microdata.
#   - Results are associational and not causal.
# ============================================================


# ============================================================
# 0. Project root and options
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"

options(na.print = "NA")


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
  "officer",
  "flextable"
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
library(officer)
library(flextable)


# ============================================================
# 2. Paths
# ============================================================

outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir   <- file.path(project_root, "outputs/diagnostics")
docs_dir           <- file.path(project_root, "docs")

dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

script12_final_model_table_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_final_reporting_model_table.csv"
)

script12_final_coefficient_table_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_final_reporting_coefficient_table.csv"
)

script12_main_coefficients_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_main_reporting_coefficients.csv"
)

script12_appendix_coefficients_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_appendix_review_coefficients.csv"
)

script12_caution_register_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_reporting_caution_register.csv"
)

script12_publication_readiness_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_publication_readiness_summary.csv"
)

script11_model_quality_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_model_quality_review.csv"
)

script11_coefficient_review_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_coefficient_review.csv"
)

script11_model_stage_review_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_model_stage_review_summary.csv"
)

script11_main_strict_robustness_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_main_strict_model_robustness.csv"
)


# ============================================================
# 3. Helper functions
# ============================================================

safe_character <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_integer <- function(x) {
  suppressWarnings(as.integer(round(as.numeric(x))))
}

sample_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "main_grade_10_12" ~ "Main sample: grades 10-12",
    x == "strict_grade_10_12_age_15_19" ~
      "Sensitivity sample: grades 10-12 and ages 15-19",
    TRUE ~ x
  )
}

short_sample_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "main_grade_10_12" ~ "Main",
    x == "strict_grade_10_12_age_15_19" ~ "Sensitivity",
    TRUE ~ x
  )
}

stage_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "M0_core_controls" ~ "M0: age, sex and grade",
    x == "M1_family_context" ~ "M1: family and household context",
    x == "M2_school_context" ~ "M2: school context",
    x == "M3_knowledge_attitudes" ~ "M3: knowledge, attitudes and perceptions",
    x == "M4_peers_relationships" ~ "M4: peers and relationships",
    x == "M5_general_risk_behaviors" ~ "M5: general risk behaviors",
    x == "M6_final_parsimonious_model" ~ "M6: integrated parsimonious model",
    TRUE ~ x
  )
}

ecological_level <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "M0_core_controls" ~ "Individual demographic level",
    x == "M1_family_context" ~ "Family and household level",
    x == "M2_school_context" ~ "School level",
    x == "M3_knowledge_attitudes" ~ "Psychosocial level",
    x == "M4_peers_relationships" ~ "Peer and relationship level",
    x == "M5_general_risk_behaviors" ~ "Individual risk-behavior level",
    x == "M6_final_parsimonious_model" ~ "Integrated multilevel model",
    TRUE ~ "Other"
  )
}

report_status_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "ready_for_cautious_reporting" ~ "Ready for cautious reporting",
    x == "report_only_after_manual_review" ~ "Manual review required",
    x == "not_ready_for_reporting" ~ "Not ready for reporting",
    TRUE ~ x
  )
}

outcome_public_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "a_sex_ever" ~ "Sexual initiation",
    x == "a_H1CO3_yesno" ~ "Birth control use at first sex",
    x == "a_H1CO6_yesno" ~ "Birth control use at most recent sex",
    x == "a_H1CO8_yesno" ~ "Ever used a condom during sex",
    x == "a_H1CO13_yesno" ~ "Ever used birth-control pills",
    x == "a_H1FP7_yesno" ~ "Ever been pregnant",
    x == "a_H1HS9_yesno" ~ "STD testing or treatment",
    x == "a_H1CO16A_yesno" ~ "Self-reported chlamydia diagnosis",
    x == "a_H1CO16C_yesno" ~ "Self-reported gonorrhea diagnosis",
    TRUE ~ x
  )
}

outcome_domain <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "a_sex_ever" ~ "Sexual initiation",
    x %in% c(
      "a_H1CO3_yesno",
      "a_H1CO6_yesno",
      "a_H1CO8_yesno",
      "a_H1CO13_yesno"
    ) ~ "Contraception and protection",
    x == "a_H1FP7_yesno" ~ "Pregnancy and reproductive experience",
    x %in% c(
      "a_H1HS9_yesno",
      "a_H1CO16A_yesno",
      "a_H1CO16C_yesno"
    ) ~ "STI-related outcomes",
    TRUE ~ "Other"
  )
}

predictor_public_label <- function(x, term = "") {
  x <- safe_character(x)
  term <- safe_character(term)
  
  dplyr::case_when(
    stringr::str_detect(term, "a_grade_wave111") ~
      "11th grade vs 10th grade",
    stringr::str_detect(term, "a_grade_wave112") ~
      "12th grade vs 10th grade",
    x == "a_age_wave1" | x == "age" ~ "Age",
    x == "a_female" | x == "female" ~ "Female sex",
    x == "a_grade_wave1" | x == "grade" ~ "School grade",
    stringr::str_detect(x, "family|parent|mother|father|household") ~
      "Family/household indicator",
    stringr::str_detect(x, "school|teacher|grade") ~
      "School-related indicator",
    stringr::str_detect(x, "peer|friend|relationship") ~
      "Peer/relationship indicator",
    stringr::str_detect(x, "attitude|knowledge|risk|efficacy|perception") ~
      "Psychosocial indicator",
    TRUE ~ x
  )
}

format_or <- function(x) {
  x <- safe_numeric(x)

  dplyr::case_when(
    is.na(x) ~ "",
    x >= 1000 ~ formatC(x, format = "e", digits = 2),
    x < 0.001 ~ formatC(x, format = "e", digits = 2),
    TRUE ~ formatC(x, format = "f", digits = 2)
  )
}

format_p <- function(x) {
  x <- safe_numeric(x)

  dplyr::case_when(
    is.na(x) ~ "",
    x < 0.001 ~ "<0.001",
    TRUE ~ formatC(x, format = "f", digits = 3)
  )

}

format_p_sentence_en <- function(x) {
  x <- safe_numeric(x)

  dplyr::case_when(
    is.na(x) ~ "",
    x < 0.001 ~ "p < 0.001",
    TRUE ~ paste0("p = ", formatC(x, format = "f", digits = 3))
  )
}

format_p_sentence_pt <- function(x) {
  x <- safe_numeric(x)

  dplyr::case_when(
    is.na(x) ~ "",
    x < 0.001 ~ "p < 0,001",
    TRUE ~ paste0("p = ", stringr::str_replace(
      formatC(x, format = "f", digits = 3),
      "\\.",
      ","
    ))
  )
}

format_ci <- function(low, high) {
  low_fmt <- format_or(low)
  high_fmt <- format_or(high)

  dplyr::case_when(
    low_fmt == "" | high_fmt == "" ~ "",
    TRUE ~ paste0("[", low_fmt, "; ", high_fmt, "]")
  )
}

direction_phrase <- function(or) {
  or <- safe_numeric(or)

  dplyr::case_when(
    is.na(or) ~ "unclear direction",
    or > 1 ~ "higher odds",
    or < 1 ~ "lower odds",
    TRUE ~ "no difference in odds"
  )
}

text_join <- function(x) {
  x <- safe_character(x)
  x <- x[x != ""]

  if (length(x) == 0) {
    return("")
  }

  paste(x, collapse = " ")
}

make_word_table <- function(data, font_size = 8) {
  data <- data %>%
    mutate(across(everything(), safe_character))

  ft <- flextable::flextable(data)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::autofit(ft)

  ft
}

add_paragraphs <- function(doc, paragraphs) {
  paragraphs <- safe_character(paragraphs)
  paragraphs <- paragraphs[paragraphs != ""]

  for (p in paragraphs) {
    doc <- officer::body_add_par(doc, p, style = "Normal")
  }

  doc
}

add_bullets <- function(doc, items) {
  items <- safe_character(items)
  items <- items[items != ""]

  for (item in items) {
    doc <- officer::body_add_par(doc, paste0("- ", item), style = "Normal")
  }

  doc
}

add_table_or_note <- function(doc, data, note = "No rows available.", font_size = 8) {
  if (nrow(data) == 0) {
    doc <- officer::body_add_par(doc, note, style = "Normal")
  } else {
    doc <- flextable::body_add_flextable(
      doc,
      make_word_table(data, font_size = font_size)
    )
  }

  doc
}

markdown_table <- function(df, max_rows = 30) {
  if (nrow(df) == 0) {
    return("_No rows available._")
  }

  df <- df %>%
    head(max_rows) %>%
    mutate(across(everything(), safe_character))

  header <- paste(names(df), collapse = " | ")
  divider <- paste(rep("---", ncol(df)), collapse = " | ")
  rows <- apply(df, 1, function(row) paste(row, collapse = " | "))

  paste(
    c(
      paste0("| ", header, " |"),
      paste0("| ", divider, " |"),
      paste0("| ", rows, " |")
    ),
    collapse = "\n"
  )
}


# ============================================================
# 4. Check required inputs
# ============================================================

required_inputs <- c(
  script12_final_model_table_path,
  script12_final_coefficient_table_path,
  script12_main_coefficients_path,
  script12_appendix_coefficients_path,
  script12_caution_register_path,
  script12_publication_readiness_path,
  script11_model_quality_path,
  script11_coefficient_review_path,
  script11_model_stage_review_path,
  script11_main_strict_robustness_path
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
# 5. Load inputs
# ============================================================

final_model_table <- readr::read_csv(
  script12_final_model_table_path,
  show_col_types = FALSE
)

final_coefficient_table <- readr::read_csv(
  script12_final_coefficient_table_path,
  show_col_types = FALSE
)

main_coefficients <- readr::read_csv(
  script12_main_coefficients_path,
  show_col_types = FALSE
)

appendix_coefficients <- readr::read_csv(
  script12_appendix_coefficients_path,
  show_col_types = FALSE
)

caution_register <- readr::read_csv(
  script12_caution_register_path,
  show_col_types = FALSE
)

publication_readiness <- readr::read_csv(
  script12_publication_readiness_path,
  show_col_types = FALSE
)

model_quality_review <- readr::read_csv(
  script11_model_quality_path,
  show_col_types = FALSE
)

coefficient_review_all <- readr::read_csv(
  script11_coefficient_review_path,
  show_col_types = FALSE
)

model_stage_review <- readr::read_csv(
  script11_model_stage_review_path,
  show_col_types = FALSE
)

main_strict_robustness <- readr::read_csv(
  script11_main_strict_robustness_path,
  show_col_types = FALSE
)


# ============================================================
# 6. Conceptual dictionaries
# ============================================================

socioecological_framework <- tibble::tibble(
  level = c(
    "Individual demographic level",
    "Psychosocial level",
    "Family and household level",
    "Peer and relationship level",
    "School level",
    "Health outcome level"
  ),
  meaning_in_report = c(
    "Age, sex and grade are treated as developmental and demographic markers.",
    "Knowledge, attitudes, perceived risk, subjective norms and self-efficacy are interpreted as psychosocial mechanisms.",
    "Family support, monitoring and household context are interpreted as interpersonal protective or vulnerability factors.",
    "Peers and relationships are interpreted as sources of social pressure, norms and support.",
    "School connection and climate are interpreted as contextual protective or vulnerability factors.",
    "Sexual initiation, contraception/protection, pregnancy and STI-related indicators are interpreted as outcomes."
  ),
  reporting_rule = c(
    "Final public models can report this level directly when stable.",
    "Reported as exploratory unless model stability is sufficient.",
    "Reported as exploratory when richer models are unstable.",
    "Reported as exploratory when richer models are unstable.",
    "Reported as exploratory when richer models are unstable.",
    "Reported by outcome domain, with rare outcomes treated cautiously."
  )
)

outcome_dictionary <- tibble::tibble(
  outcome = c(
    "a_sex_ever",
    "a_H1CO3_yesno",
    "a_H1CO6_yesno",
    "a_H1CO8_yesno",
    "a_H1CO13_yesno",
    "a_H1FP7_yesno",
    "a_H1HS9_yesno",
    "a_H1CO16A_yesno",
    "a_H1CO16C_yesno"
  ),
  public_label = outcome_public_label(outcome),
  domain = outcome_domain(outcome),
  interpretation_note = c(
    "Ever had sexual intercourse.",
    "Birth control use at first sexual intercourse.",
    "Birth control use at the most recent sexual intercourse.",
    "Ever used a condom during sexual intercourse.",
    "Ever used birth-control pills.",
    "Ever been pregnant; denominator depends on eligibility and skip patterns.",
    "STD testing or treatment; interpreted as service contact or sexual-health exposure, not confirmed infection.",
    "Self-reported chlamydia diagnosis.",
    "Self-reported gonorrhea diagnosis; rare and imprecise in this analysis."
  )
)

predictor_dictionary <- tibble::tibble(
  predictor = c(
    "Age",
    "Female sex",
    "School grade",
    "Family and household context",
    "School context",
    "Peer and relationship context",
    "Knowledge, attitudes and perceptions",
    "General risk behaviors"
  ),
  interpretation_note = c(
    "Developmental marker used in all final models.",
    "Core demographic control.",
    "School progression marker used as categorical control.",
    "Socioecological level theoretically central but treated cautiously when models are unstable.",
    "Socioecological level theoretically central but treated cautiously when models are unstable.",
    "Socioecological level theoretically central but treated cautiously when models are unstable.",
    "Psychosocial mechanisms aligned with the Health Belief Model and Theory of Planned Behavior.",
    "Exploratory behavioral risk block; not treated as causal or final when unstable."
  )
)


# ============================================================
# 7. Clean final reporting tables
# ============================================================

clean_final_models <- final_model_table %>%
  mutate(
    sample = sample_label(sample_name),
    short_sample = short_sample_label(sample_name),
    outcome_readable = outcome_public_label(outcome),
    domain = outcome_domain(outcome),
    selected_model_readable = stage_label(model_stage),
    ecological_level = ecological_level(model_stage),
    reporting_status_readable =
      report_status_label(final_model_reporting_status),
    n_complete = safe_integer(n_complete),
    n_outcome_yes = safe_integer(n_outcome_yes),
    n_outcome_no = safe_integer(n_outcome_no),
    weighted_pct_yes = safe_numeric(weighted_pct_yes),
    suitability_score = safe_numeric(suitability_score)
  )

clean_coefficients <- final_coefficient_table %>%
  filter(term != "(Intercept)") %>%
  mutate(
    sample = sample_label(sample_name),
    short_sample = short_sample_label(sample_name),
    outcome_readable = outcome_public_label(outcome),
    domain = outcome_domain(outcome),
    predictor_readable = predictor_public_label(term_variable, term),
    selected_model_readable = stage_label(model_stage),
    ecological_level = ecological_level(model_stage),
    odds_ratio = safe_numeric(odds_ratio),
    conf_low_or = safe_numeric(conf_low_or),
    conf_high_or = safe_numeric(conf_high_or),
    p_value = safe_numeric(p_value),
    or_text = format_or(odds_ratio),
    ci_text = format_ci(conf_low_or, conf_high_or),
    p_text = format_p(p_value),
    direction = direction_phrase(odds_ratio)
  )

main_clean_coefficients <- clean_coefficients %>%
  filter(coefficient_reporting_decision == "candidate_for_interpretation")

appendix_clean_coefficients <- clean_coefficients %>%
  filter(
    coefficient_reporting_decision != "candidate_for_interpretation",
    coefficient_reporting_decision != "exclude_intercept"
  )

public_model_summary <- clean_final_models %>%
  select(
    Sample = sample,
    Domain = domain,
    Outcome = outcome_readable,
    `Selected model` = selected_model_readable,
    `Reporting status` = reporting_status_readable,
    `Complete cases` = n_complete,
    `Weighted yes percent` = weighted_pct_yes,
    `Suitability score` = suitability_score
  ) %>%
  arrange(Sample, Domain, Outcome)

public_key_results <- main_clean_coefficients %>%
  select(
    Sample = sample,
    Domain = domain,
    Outcome = outcome_readable,
    Predictor = predictor_readable,
    OR = or_text,
    `95 percent CI` = ci_text,
    p = p_text,
    Direction = direction
  ) %>%
  arrange(Sample, Domain, Outcome, Predictor)

public_caution_results <- bind_rows(
  clean_final_models %>%
    filter(final_model_reporting_status != "ready_for_cautious_reporting") %>%
    transmute(
      Sample = sample,
      Domain = domain,
      Outcome = outcome_readable,
      Issue = "Outcome requires manual review before public interpretation.",
      Detail = paste0(
        "Reporting status: ",
        reporting_status_readable,
        "; suitability score: ",
        suitability_score,
        "."
      )
    ),
  appendix_clean_coefficients %>%
    transmute(
      Sample = sample,
      Domain = domain,
      Outcome = outcome_readable,
      Issue = "Coefficient requires review or appendix placement.",
      Detail = paste0(
        predictor_readable,
        ": OR ",
        or_text,
        ", 95 percent CI ",
        ci_text,
        ", p = ",
        p_text,
        "."
      )
    )
) %>%
  arrange(Sample, Domain, Outcome)


# ============================================================
# 8. Exploratory socioecological block summary
# ============================================================

stage_dictionary <- tibble::tibble(
  model_stage = c(
    "M0_core_controls",
    "M1_family_context",
    "M2_school_context",
    "M3_knowledge_attitudes",
    "M4_peers_relationships",
    "M5_general_risk_behaviors",
    "M6_final_parsimonious_model"
  ),
  stage_readable = stage_label(model_stage),
  ecological_level = ecological_level(model_stage),
  public_role = c(
    "Final conservative model",
    "Exploratory socioecological block",
    "Exploratory socioecological block",
    "Exploratory psychosocial block",
    "Exploratory socioecological block",
    "Exploratory individual-risk block",
    "Integrated exploratory model"
  )
)

coefficient_stage_flags <- coefficient_review_all %>%
  filter(term != "(Intercept)") %>%
  mutate(
    p_value = safe_numeric(p_value),
    odds_ratio = safe_numeric(odds_ratio),
    coefficient_reporting_decision = safe_character(coefficient_reporting_decision),
    extreme_or_flag = safe_character(extreme_or_flag),
    ci_width_flag = safe_character(ci_width_flag)
  ) %>%
  group_by(sample_name, model_stage) %>%
  summarise(
    n_coefficients = n(),
    n_p_lt_05 = sum(!is.na(p_value) & p_value < 0.05, na.rm = TRUE),
    n_candidate_interpretation = sum(
      coefficient_reporting_decision == "candidate_for_interpretation",
      na.rm = TRUE
    ),
    n_review_or_exclude_flags = sum(
      stringr::str_detect(coefficient_reporting_decision, "review|exclude") |
        extreme_or_flag != "not_extreme" |
        ci_width_flag %in% c("wide_ci_review", "very_wide_ci_review"),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

exploratory_block_summary <- model_quality_review %>%
  group_by(sample_name, model_stage) %>%
  summarise(
    n_models = n(),
    n_fitted = sum(fit_status == "fitted", na.rm = TRUE),
    n_technically_stable = sum(
      final_selection_class == "technically_stable_candidate",
      na.rm = TRUE
    ),
    n_usable_with_caution = sum(
      final_selection_class == "usable_with_caution",
      na.rm = TRUE
    ),
    n_review_before_reporting = sum(
      final_selection_class == "review_before_reporting",
      na.rm = TRUE
    ),
    n_not_selectable = sum(
      stringr::str_detect(final_selection_class, "not_selectable"),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(stage_dictionary, by = "model_stage") %>%
  left_join(
    coefficient_stage_flags,
    by = c("sample_name", "model_stage")
  ) %>%
  mutate(
    across(
      c(
        n_coefficients,
        n_p_lt_05,
        n_candidate_interpretation,
        n_review_or_exclude_flags
      ),
      ~ tidyr::replace_na(.x, 0)
    ),
    Sample = sample_label(sample_name),
    `Socioecological level` = ecological_level,
    `Model block` = stage_readable,
    `Public role` = public_role,
    `Interpretive decision` = case_when(
      model_stage == "M0_core_controls" ~
        "Used as the main conservative public model when selected.",
      n_fitted == 0 ~
        "Not interpreted because no model was fitted.",
      n_review_before_reporting > n_technically_stable ~
        "Exploratory only; not promoted to public conclusion because review flags dominate.",
      n_usable_with_caution > 0 | n_review_or_exclude_flags > 0 ~
        "Exploratory; may inform hypotheses but requires caution.",
      n_technically_stable > 0 ~
        "Technically promising, but not automatically promoted to final public result.",
      TRUE ~
        "Exploratory only."
    )
  ) %>%
  select(
    Sample,
    `Socioecological level`,
    `Model block`,
    `Public role`,
    `Models fitted` = n_fitted,
    `Technically stable models` = n_technically_stable,
    `Usable with caution` = n_usable_with_caution,
    `Review before reporting` = n_review_before_reporting,
    `Significant coefficients p<0.05` = n_p_lt_05,
    `Review/exclusion flags` = n_review_or_exclude_flags,
    `Interpretive decision`
  ) %>%
  arrange(Sample, `Model block`)

main_exploratory_blocks <- exploratory_block_summary %>%
  filter(
    Sample == "Main sample: grades 10-12",
    !`Model block` %in% c(
      "M0: age, sex and grade",
      "M6: integrated parsimonious model"
    )
  )

sensitivity_exploratory_blocks <- exploratory_block_summary %>%
  filter(
    Sample == "Sensitivity sample: grades 10-12 and ages 15-19",
    !`Model block` %in% c(
      "M0: age, sex and grade",
      "M6: integrated parsimonious model"
    )
  )

robustness_m0_summary <- main_strict_robustness %>%
  filter(model_stage == "M0_core_controls") %>%
  mutate(
    Domain = outcome_domain(outcome),
    Outcome = outcome_public_label(outcome)
  ) %>%
  select(
    Domain,
    Outcome,
    `Coefficients compared` = n_coefficients_compared,
    `Same direction` = n_same_direction,
    `Same direction percent` = share_same_direction,
    `Robustness summary` = robustness_summary
  ) %>%
  arrange(Domain, Outcome)

# ============================================================
# 8b. Public-report compact tables
# ============================================================

public_key_results_compact <- public_key_results %>%
  group_by(Sample, Domain, Outcome, Predictor) %>%
  mutate(
    predictor_count = dplyr::n(),
    predictor_index = dplyr::row_number(),
    Predictor = dplyr::case_when(
      Predictor == "School grade" & predictor_count > 1 ~
        paste0("School grade contrast ", predictor_index),
      TRUE ~ Predictor
    )
  ) %>%
  ungroup() %>%
  select(
    Sample,
    Domain,
    Outcome,
    Predictor,
    OR,
    `95 percent CI`,
    p
  )

public_key_results_main_report <- public_key_results_compact %>%
  filter(Sample == "Main sample: grades 10-12") %>%
  select(
    Domain,
    Outcome,
    Predictor,
    OR,
    `95 percent CI`,
    p
  )

public_key_results_sensitivity_report <- public_key_results_compact %>%
  filter(Sample == "Sensitivity sample: grades 10-12 and ages 15-19") %>%
  select(
    Domain,
    Outcome,
    Predictor,
    OR,
    `95 percent CI`,
    p
  )

socioecological_framework_report <- socioecological_framework %>%
  select(
    Level = level,
    `Interpretation in this report` = meaning_in_report
  )

exploratory_blocks_report <- main_exploratory_blocks %>%
  select(
    `Socioecological level`,
    `Model block`,
    `Models fitted`,
    `Technically stable models`,
    `Usable with caution`,
    `Review before reporting`,
    `Interpretive decision`
  )

caution_summary_report <- public_caution_results %>%
  count(
    Sample,
    Domain,
    Outcome,
    Issue,
    name = "Number of caution flags"
  ) %>%
  arrange(
    Sample,
    Domain,
    Outcome,
    Issue
  )

caution_summary_main_report <- caution_summary_report %>%
  filter(Sample == "Main sample: grades 10-12") %>%
  select(
    Domain,
    Outcome,
    Issue,
    `Number of caution flags`
  )

caution_summary_sensitivity_report <- caution_summary_report %>%
  filter(Sample == "Sensitivity sample: grades 10-12 and ages 15-19") %>%
  select(
    Domain,
    Outcome,
    Issue,
    `Number of caution flags`
  )

robustness_report <- robustness_m0_summary %>%
  select(
    Domain,
    Outcome,
    `Same direction percent`,
    `Robustness summary`
  ) %>%
  mutate(
    `Robustness summary` = case_when(
      `Robustness summary` == "high_directional_robustness" ~
        "High directional robustness",
      `Robustness summary` == "moderate_directional_robustness" ~
        "Moderate directional robustness",
      `Robustness summary` == "low_or_unstable_directional_robustness" ~
        "Low or unstable directional robustness",
      TRUE ~ `Robustness summary`
    )
  )
# ============================================================
# 9. Interpretive findings
# ============================================================

get_result <- function(sample_id, outcome_id, predictor_pattern) {
  res <- main_clean_coefficients %>%
    filter(
      sample_name == sample_id,
      outcome == outcome_id,
      stringr::str_detect(predictor_readable, predictor_pattern)
    ) %>%
    arrange(p_value)

  if (nrow(res) == 0) {
    return("")
  }

  paste0(
    res$predictor_readable,
    " was associated with ",
    res$direction,
    " of ",
    res$outcome_readable,
    " (OR = ",
    res$or_text,
    ", 95 percent CI ",
    res$ci_text,
    ", ",
    format_p_sentence_en(res$p_value),
    ").",
    collapse = " "
  )
}
sexual_initiation_main <- text_join(c(
  get_result("main_grade_10_12", "a_sex_ever", "Age"),
  get_result("main_grade_10_12", "a_sex_ever", "School grade")
))

sexual_initiation_sensitivity <- text_join(c(
  get_result("strict_grade_10_12_age_15_19", "a_sex_ever", "Age"),
  get_result("strict_grade_10_12_age_15_19", "a_sex_ever", "School grade")
))

contraception_main <- text_join(c(
  get_result("main_grade_10_12", "a_H1CO6_yesno", "Age"),
  get_result("main_grade_10_12", "a_H1CO8_yesno", "Age")
))

contraception_sensitivity <- text_join(c(
  get_result("strict_grade_10_12_age_15_19", "a_H1CO8_yesno", "Age")
))

pregnancy_main <- get_result(
  "main_grade_10_12",
  "a_H1FP7_yesno",
  "Age"
)

pregnancy_sensitivity <- get_result(
  "strict_grade_10_12_age_15_19",
  "a_H1FP7_yesno",
  "Age"
)

sti_main <- text_join(c(
  get_result("main_grade_10_12", "a_H1CO16A_yesno", "Age"),
  get_result("main_grade_10_12", "a_H1HS9_yesno", "Age")
))

sti_sensitivity <- text_join(c(
  get_result("strict_grade_10_12_age_15_19", "a_H1HS9_yesno", "Age")
))

interpretive_summary <- tibble::tibble(
  section = c(
    "Overall public message",
    "Theoretical interpretation",
    "Final statistical result",
    "Socioecological caution",
    "Family, school and peer levels",
    "Main implication"
  ),
  interpretation = c(
    "The public-facing results support a cautious socioecological reading of adolescent sexual and reproductive-health outcomes.",
    "The original thesis logic is preserved: adolescent behavior is interpreted through individual, family, peer, school and psychosocial levels.",
    "The most defensible public models are conservative M0 models adjusted for age, sex and grade.",
    "The dominance of M0 does not mean that family, school and peers are irrelevant; it means that richer models were not sufficiently stable for headline public conclusions.",
    "Family, school, peer and psychosocial blocks are reported as exploratory evidence and hypothesis-generating domains.",
    "Public communication should emphasize stable adjusted associations while preserving the socioecological interpretation."
  )
)

key_findings <- tibble::tibble(
  domain = c(
    "Sexual initiation",
    "Contraception and protection",
    "Pregnancy and reproductive experience",
    "STI-related outcomes",
    "Socioecological interpretation"
  ),
  main_sample_finding = c(
    ifelse(sexual_initiation_main != "", sexual_initiation_main, "No prioritized coefficient."),
    ifelse(contraception_main != "", contraception_main, "No uniform prioritized pattern."),
    ifelse(pregnancy_main != "", pregnancy_main, "No prioritized coefficient."),
    ifelse(sti_main != "", sti_main, "No uniform prioritized pattern."),
    "Family, school, peer and psychosocial levels remain theoretically central, but are treated as exploratory in this public report."
  ),
  sensitivity_sample_finding = c(
    ifelse(sexual_initiation_sensitivity != "", sexual_initiation_sensitivity, "No prioritized coefficient."),
    ifelse(contraception_sensitivity != "", contraception_sensitivity, "No prioritized coefficient."),
    ifelse(pregnancy_sensitivity != "", pregnancy_sensitivity, "No prioritized coefficient."),
    ifelse(sti_sensitivity != "", sti_sensitivity, "No prioritized coefficient."),
    "The same conservative interpretation rule is applied to the sensitivity sample."
  ),
  public_interpretation = c(
    "Age and grade behave as developmental markers associated with sexual initiation.",
    "Protection-related outcomes should be interpreted separately; they do not form one uniform empirical pattern.",
    "Pregnancy history is strongly age-graded among eligible respondents, but skip patterns matter.",
    "STI-related outcomes are sensitive to rarity, diagnosis type and service-use interpretation.",
    "The report does not reject the socioecological model; it distinguishes final stable results from exploratory multilevel evidence."
  )
)

policy_research_implications <- tibble::tibble(
  implication_area = c(
    "Public interpretation",
    "Research design",
    "Family and school interventions",
    "Peer and psychosocial mechanisms",
    "Future modeling"
  ),
  implication = c(
    "Present the final models as conservative adjusted associations, not as causal evidence.",
    "Maintain the socioecological framework as the organizing theory of the project.",
    "Do not infer that family or school have no role; instead, report that their empirical evidence remains exploratory in this public-use replication.",
    "Peer norms, attitudes, self-efficacy and perceived control should remain central to future model refinement.",
    "Future work can refine constructs, pool rare outcomes or test alternative models to better estimate multilevel pathways."
  )
)

public_limitations <- tibble::tibble(
  limitation = c(
    "The public-facing final models are conservative and mainly demographic.",
    "The Add Health public-use replication cannot reproduce the full structure of the original thesis dataset.",
    "Family, school, peer and psychosocial blocks were analyzed but not promoted to headline findings when model diagnostics were unstable.",
    "Some outcomes depend on skip patterns and eligibility rules.",
    "Some STI-related outcomes are rare and require cautious interpretation.",
    "All results are associational and should not be interpreted as causal effects."
  )
)


# ============================================================
# 10. Public report text
# ============================================================

title <- "Results Interpretation Report"
subtitle <- "Add Health Wave I Public-Use Adolescent Risk Behavior Analysis"

executive_summary_paragraphs <- c(
  paste0(
    "This report presents the public-facing interpretation of the Add Health Wave I ",
    "adolescent risk behavior analysis. It is designed for readers who need the substantive ",
    "message of the project, not the full technical pipeline."
  ),
  paste0(
    "The report is explicitly guided by a socioecological interpretation. The original thesis ",
    "was not limited to individual factors; it considered individual, family, peer, school and ",
    "psychosocial mechanisms. The Add Health public-use replication preserves this logic while ",
    "separating stable final results from exploratory evidence."
  ),
  paste0(
    "The central empirical result is conservative: the final public models are M0 models adjusted ",
    "for age, sex and grade. This does not imply that family, school and peers are irrelevant. ",
    "It means that the richer socioecological blocks were not sufficiently stable to serve as ",
    "headline public conclusions in this version."
  )
)

theory_paragraphs <- c(
  paste0(
    "The interpretation follows a multilevel framework. At the individual level, adolescent ",
    "development is represented by age, sex and grade. At the family level, support, monitoring ",
    "and household context are interpreted as potential protective or vulnerability factors. ",
    "At the peer level, social pressure and relationships are interpreted as mechanisms shaping ",
    "behavior. At the school level, connection to school and support from adults and peers are ",
    "treated as contextual resources. At the psychosocial level, attitudes, perceived norms, ",
    "risk perceptions and self-efficacy are interpreted as mechanisms linking context to intention ",
    "and behavior."
  ),
  paste0(
    "This structure is consistent with the original thesis logic. The public-use replication, ",
    "however, is more limited than the thesis dataset. Therefore, the report distinguishes between ",
    "theory-guided interpretation and statistically stable public reporting."
  )
)

methods_paragraphs <- c(
  paste0(
    "The analysis uses Add Health Wave I public-use data. The main analytical sample includes ",
    "students in grades 10 to 12. A sensitivity sample restricts the analysis further to students ",
    "in grades 10 to 12 and ages 15 to 19."
  ),
  paste0(
    "Weighted logistic regression models were reviewed before public interpretation. The review ",
    "considered sample size, outcome-cell size, extreme odds ratios, wide confidence intervals, ",
    "possible conceptual overlap and main-versus-sensitivity consistency."
  ),
  paste0(
    "The public reporting rule is conservative. Stable final results are reported directly. ",
    "Family, school, peer and psychosocial blocks are described as exploratory when diagnostics ",
    "do not support strong public claims."
  )
)

how_to_read_paragraphs <- c(
  paste0(
    "Odds ratios above 1 indicate higher odds of the outcome. Odds ratios below 1 indicate lower ",
    "odds of the outcome. Confidence intervals describe statistical uncertainty."
  ),
  paste0(
    "Because the analysis is observational and based on public-use data, the report does not use ",
    "causal language. It does not claim that age, grade, family, peers or school cause the outcomes."
  ),
  paste0(
    "When the report says that a domain is exploratory, this means that the domain is theoretically ",
    "important but was not sufficiently stable in this public-use modeling exercise to support ",
    "a headline empirical conclusion."
  )
)

sexual_initiation_paragraphs <- c(
  paste0(
    "Sexual initiation is the clearest final domain. Age and grade appear as developmental markers ",
    "associated with higher odds of having ever had sexual intercourse."
  ),
  ifelse(
    sexual_initiation_main != "",
    paste0("Main sample evidence: ", sexual_initiation_main),
    "No main-sample coefficient was prioritized for sexual initiation."
  ),
  ifelse(
    sexual_initiation_sensitivity != "",
    paste0("Sensitivity sample evidence: ", sexual_initiation_sensitivity),
    "No sensitivity-sample coefficient was prioritized for sexual initiation."
  ),
  paste0(
    "This result should be interpreted as developmental patterning, not as causal evidence. ",
    "It is consistent with the broader socioecological view that individual development interacts ",
    "with family, school, peers and norms."
  )
)

contraception_paragraphs <- c(
  paste0(
    "The contraception and protection domain is more heterogeneous. Condom use, birth control at ",
    "first sex, birth control at most recent sex and birth-control pill use are related but not identical outcomes."
  ),
  ifelse(
    contraception_main != "",
    paste0("Main sample evidence: ", contraception_main),
    "No uniform main-sample coefficient was prioritized across contraception and protection outcomes."
  ),
  ifelse(
    contraception_sensitivity != "",
    paste0("Sensitivity sample evidence: ", contraception_sensitivity),
    "No uniform sensitivity-sample coefficient was prioritized across contraception and protection outcomes."
  ),
  paste0(
    "The correct public message is therefore not that protection behavior has one simple determinant. ",
    "The evidence supports outcome-specific interpretation."
  )
)

pregnancy_paragraphs <- c(
  paste0(
    "Pregnancy history shows a strong age pattern among eligible respondents. This domain must be ",
    "interpreted with particular attention to eligibility and skip patterns."
  ),
  ifelse(
    pregnancy_main != "",
    paste0("Main sample evidence: ", pregnancy_main),
    "No main-sample coefficient was prioritized for pregnancy history."
  ),
  ifelse(
    pregnancy_sensitivity != "",
    paste0("Sensitivity sample evidence: ", pregnancy_sensitivity),
    "No sensitivity-sample coefficient was prioritized for pregnancy history."
  ),
  paste0(
    "The result should be interpreted as descriptive age-patterning among respondents for whom the item ",
    "is applicable, not as a general causal estimate for all adolescents."
  )
)

sti_paragraphs <- c(
  paste0(
    "The STI-related domain requires the most caution. Some outcomes refer to self-reported diagnosis, ",
    "whereas STD testing or treatment may reflect contact with sexual-health services rather than confirmed infection."
  ),
  ifelse(
    sti_main != "",
    paste0("Main sample evidence: ", sti_main),
    "No uniform main-sample coefficient was prioritized in the STI-related domain."
  ),
  ifelse(
    sti_sensitivity != "",
    paste0("Sensitivity sample evidence: ", sti_sensitivity),
    "No uniform sensitivity-sample coefficient was prioritized in the STI-related domain."
  ),
  paste0(
    "Self-reported gonorrhea should not be used as a headline finding because the outcome is rare and imprecise."
  )
)

exploratory_paragraphs <- c(
  paste0(
    "The exploratory socioecological blocks are essential for interpretation. They include family and household context, ",
    "school context, knowledge and attitudes, peers and relationships, and general risk behaviors."
  ),
  paste0(
    "These blocks were analyzed because they correspond to the thesis framework. However, several richer models showed ",
    "instability, review flags, wide intervals or extreme odds ratios. For that reason, they are not presented as final ",
    "public conclusions."
  ),
  paste0(
    "This should not be read as evidence that family, school or peers have no influence. It means that the present public-use ",
    "replication does not provide sufficiently stable estimates to make strong claims about those levels."
  )
)

conclusion_paragraphs <- c(
  paste0(
    "The public conclusion is socioecological and conservative. The most stable empirical results are concentrated in ",
    "individual developmental markers, especially age and grade. At the same time, the theoretical interpretation remains ",
    "multilevel: family, school, peers and psychosocial mechanisms remain central to understanding adolescent sexual and ",
    "reproductive-health behavior."
  ),
  paste0(
    "The Add Health public-use replication therefore provides a transparent and ethically safer way to reproduce the broad ",
    "analytical logic of the thesis, while avoiding publication of sensitive individual-level thesis data."
  ),
  paste0(
    "Future work should refine the family, school, peer and psychosocial constructs, examine alternative model specifications, ",
    "and treat the exploratory findings as hypotheses rather than final causal conclusions."
  )
)


# ============================================================
# 11. Export CSV outputs
# ============================================================

write_csv(
  socioecological_framework,
  file.path(outputs_tables_dir, "script13b_wave01_socioecological_framework.csv")
)

write_csv(
  outcome_dictionary,
  file.path(outputs_tables_dir, "script13b_wave01_outcome_dictionary.csv")
)

write_csv(
  predictor_dictionary,
  file.path(outputs_tables_dir, "script13b_wave01_predictor_dictionary.csv")
)

write_csv(
  interpretive_summary,
  file.path(outputs_tables_dir, "script13b_wave01_interpretive_summary.csv")
)

write_csv(
  key_findings,
  file.path(outputs_tables_dir, "script13b_wave01_key_findings.csv")
)

write_csv(
  policy_research_implications,
  file.path(outputs_tables_dir, "script13b_wave01_policy_research_implications.csv")
)

write_csv(
  public_limitations,
  file.path(outputs_tables_dir, "script13b_wave01_public_limitations.csv")
)

write_csv(
  public_model_summary,
  file.path(outputs_tables_dir, "script13b_wave01_public_model_summary.csv")
)

write_csv(
  public_key_results,
  file.path(outputs_tables_dir, "script13b_wave01_public_key_results.csv")
)

write_csv(
  public_caution_results,
  file.path(outputs_tables_dir, "script13b_wave01_public_caution_results.csv")
)

write_csv(
  exploratory_block_summary,
  file.path(outputs_tables_dir, "script13b_wave01_exploratory_block_summary.csv")
)

write_csv(
  main_exploratory_blocks,
  file.path(outputs_tables_dir, "script13b_wave01_main_exploratory_blocks.csv")
)

write_csv(
  robustness_m0_summary,
  file.path(outputs_tables_dir, "script13b_wave01_public_robustness_summary.csv")
)


# ============================================================
# 12. Markdown public report
# ============================================================

markdown_path <- file.path(
  docs_dir,
  "add_health_wave01_results_interpretation_report_script13b.md"
)

markdown_report <- c(
  paste0("# ", title),
  "",
  paste0("## ", subtitle),
  "",
  "### Executive summary",
  "",
  executive_summary_paragraphs,
  "",
  "### Key messages",
  "",
  paste0("- ", interpretive_summary$interpretation),
  "",
  "### Socioecological framework",
  "",
  theory_paragraphs,
  "",
  markdown_table(socioecological_framework, max_rows = 20),
  "",
  "### Data and analytical approach",
  "",
  methods_paragraphs,
  "",
  "### How to read the results",
  "",
  how_to_read_paragraphs,
  "",
  "### Final public results",
  "",
  markdown_table(public_key_results, max_rows = 30),
  "",
  "### Interpretation by outcome domain",
  "",
  "#### Sexual initiation",
  "",
  sexual_initiation_paragraphs,
  "",
  "#### Contraception and protection",
  "",
  contraception_paragraphs,
  "",
  "#### Pregnancy and reproductive experience",
  "",
  pregnancy_paragraphs,
  "",
  "#### STI-related outcomes",
  "",
  sti_paragraphs,
  "",
  "### Exploratory socioecological evidence",
  "",
  exploratory_paragraphs,
  "",
  markdown_table(main_exploratory_blocks, max_rows = 20),
  "",
  "### Results requiring caution",
  "",
  markdown_table(public_caution_results, max_rows = 20),
  "",
  "### Robustness of conservative models",
  "",
  markdown_table(robustness_m0_summary, max_rows = 20),
  "",
  "### Implications",
  "",
  paste0("- ", policy_research_implications$implication),
  "",
  "### Limitations",
  "",
  paste0("- ", public_limitations$limitation),
  "",
  "### Conclusion",
  "",
  conclusion_paragraphs
)

writeLines(markdown_report, con = markdown_path)


# ============================================================
# 13. Word public report
# ============================================================

docx_path <- file.path(
  docs_dir,
  "add_health_wave01_results_interpretation_report_script13b.docx"
)

doc <- officer::read_docx()

doc <- officer::body_add_par(doc, title, style = "heading 1")
doc <- officer::body_add_par(doc, subtitle, style = "heading 2")
doc <- officer::body_add_par(
  doc,
  "Public-facing interpretation report generated from aggregate public outputs only.",
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Executive summary", style = "heading 1")
doc <- add_paragraphs(doc, executive_summary_paragraphs)

doc <- officer::body_add_par(doc, "Key messages", style = "heading 1")
doc <- add_bullets(doc, interpretive_summary$interpretation)

doc <- officer::body_add_par(doc, "Socioecological framework", style = "heading 1")
doc <- add_paragraphs(doc, theory_paragraphs)

doc <- officer::body_add_par(
  doc,
  "Table 1 summarizes how the report uses the socioecological framework.",
  style = "Normal"
)

doc <- add_table_or_note(
  doc,
  socioecological_framework_report,
  font_size = 8
)

doc <- officer::body_add_par(doc, "Data and analytical approach", style = "heading 1")
doc <- add_paragraphs(doc, methods_paragraphs)

doc <- officer::body_add_par(doc, "How to read the results", style = "heading 1")
doc <- add_paragraphs(doc, how_to_read_paragraphs)

doc <- officer::body_add_par(doc, "Final public results", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "The final public results are presented separately for the main sample and the sensitivity sample. ",
    "Only coefficients prioritized for cautious public interpretation are shown. ",
    "The results should be read as adjusted associations, not as causal effects."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Main sample", style = "heading 2")
doc <- add_table_or_note(
  doc,
  public_key_results_main_report,
  font_size = 8
)

doc <- officer::body_add_par(doc, "Sensitivity sample", style = "heading 2")
doc <- add_table_or_note(
  doc,
  public_key_results_sensitivity_report,
  font_size = 8
)
doc <- officer::body_add_par(doc, "Interpretation by outcome domain", style = "heading 1")

doc <- officer::body_add_par(doc, "Sexual initiation", style = "heading 2")
doc <- add_paragraphs(doc, sexual_initiation_paragraphs)

doc <- officer::body_add_par(doc, "Contraception and protection", style = "heading 2")
doc <- add_paragraphs(doc, contraception_paragraphs)

doc <- officer::body_add_par(doc, "Pregnancy and reproductive experience", style = "heading 2")
doc <- add_paragraphs(doc, pregnancy_paragraphs)

doc <- officer::body_add_par(doc, "STI-related outcomes", style = "heading 2")
doc <- add_paragraphs(doc, sti_paragraphs)

doc <- officer::body_add_par(doc, "Exploratory socioecological evidence", style = "heading 1")
doc <- add_paragraphs(doc, exploratory_paragraphs)

doc <- officer::body_add_par(
  doc,
  paste0(
    "Table 4 summarizes the exploratory socioecological blocks in the main sample. ",
    "These results are used to preserve the theoretical interpretation of the thesis, ",
    "but they are not treated as final public conclusions."
  ),
  style = "Normal"
)

doc <- add_table_or_note(
  doc,
  exploratory_blocks_report,
  font_size = 7
)
doc <- officer::body_add_par(doc, "Results requiring caution", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "Detailed caution results are retained in the Excel workbook and CSV outputs. ",
    "The public report presents only a summary, because the full caution table is technical and too long for the main text."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Main sample caution summary", style = "heading 2")
doc <- add_table_or_note(
  doc,
  caution_summary_main_report,
  note = "No caution results were flagged in the main sample.",
  font_size = 8
)

doc <- officer::body_add_par(doc, "Sensitivity sample caution summary", style = "heading 2")
doc <- add_table_or_note(
  doc,
  caution_summary_sensitivity_report,
  note = "No caution results were flagged in the sensitivity sample.",
  font_size = 8
)
doc <- officer::body_add_par(doc, "Robustness of conservative models", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "Robustness was assessed by comparing the main grades 10-12 sample with the stricter ",
    "grades 10-12 and ages 15-19 sensitivity sample. The report gives more weight to ",
    "patterns that remain directionally consistent across samples."
  ),
  style = "Normal"
)
doc <- add_table_or_note(
  doc,
  robustness_report,
  font_size = 8
)
doc <- officer::body_add_par(doc, "Implications", style = "heading 1")
doc <- add_bullets(doc, policy_research_implications$implication)

doc <- officer::body_add_par(doc, "Limitations", style = "heading 1")
doc <- add_bullets(doc, public_limitations$limitation)

doc <- officer::body_add_par(doc, "Conclusion", style = "heading 1")
doc <- add_paragraphs(doc, conclusion_paragraphs)

# The complete outcome dictionary is kept in CSV and Excel outputs.
# It is not inserted into the public Word report to avoid wide tables.

print(doc, target = docx_path)


# ============================================================
# 14. Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script13b_wave01_results_interpretation_report_tables.xlsx"
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "interpretive_summary")
openxlsx::writeData(wb, "interpretive_summary", interpretive_summary)

openxlsx::addWorksheet(wb, "socioecological_framework")
openxlsx::writeData(wb, "socioecological_framework", socioecological_framework)

openxlsx::addWorksheet(wb, "key_findings")
openxlsx::writeData(wb, "key_findings", key_findings)

openxlsx::addWorksheet(wb, "public_key_results")
openxlsx::writeData(wb, "public_key_results", public_key_results)

openxlsx::addWorksheet(wb, "exploratory_blocks")
openxlsx::writeData(wb, "exploratory_blocks", exploratory_block_summary)

openxlsx::addWorksheet(wb, "public_cautions")
openxlsx::writeData(wb, "public_cautions", public_caution_results)

openxlsx::addWorksheet(wb, "robustness_m0")
openxlsx::writeData(wb, "robustness_m0", robustness_m0_summary)

openxlsx::addWorksheet(wb, "outcome_dictionary")
openxlsx::writeData(wb, "outcome_dictionary", outcome_dictionary)

openxlsx::addWorksheet(wb, "implications")
openxlsx::writeData(wb, "implications", policy_research_implications)

openxlsx::addWorksheet(wb, "limitations")
openxlsx::writeData(wb, "limitations", public_limitations)

for (sheet in names(wb)) {
  openxlsx::setColWidths(wb, sheet = sheet, cols = 1:100, widths = "auto")
  openxlsx::freezePane(wb, sheet = sheet, firstRow = TRUE)
}

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)


# ============================================================
# 15. Methodological notes and checklist
# ============================================================

script13b_methodological_notes <- tibble::tibble(
  note_id = 1:18,
  note = c(
    "Script 13b creates the public-facing results interpretation report.",
    "The report is aligned with the socioecological logic of the original thesis.",
    "The report distinguishes final stable results from exploratory socioecological evidence.",
    "The report uses only public aggregate outputs from Scripts 11 and 12.",
    "No individual-level data are read or exported.",
    "Outcome codes are translated into public-facing labels.",
    "The dominant final model is M0, adjusted for age, sex and grade.",
    "The dominance of M0 is not interpreted as absence of family, school or peer influence.",
    "Family, school, peer and psychosocial blocks are treated as exploratory when model diagnostics are unstable.",
    "Only coefficients prioritized for cautious interpretation are included in the final public results table.",
    "Results requiring manual review are separated into a caution section.",
    "The STI-related interpretation distinguishes testing/treatment from diagnosis.",
    "Pregnancy results are interpreted with attention to eligibility and skip patterns.",
    "The report states clearly that results are associational and not causal.",
    "The report is suitable for public reading after manual review.",
    "The report preserves ethical separation between public outputs and microdata.",
    "The script produces DOCX, Markdown, CSV and Excel outputs.",
    "Script 14 should be used for repository publication audit and release preparation."
  )
)

write_csv(
  script13b_methodological_notes,
  file.path(outputs_tables_dir, "script13b_wave01_methodological_notes.csv")
)

script13b_checklist <- tibble::tibble(
  check_id = 1:28,
  check_item = c(
    "Project root exists",
    "Outputs tables directory exists",
    "Outputs diagnostics directory exists",
    "Docs directory exists",
    "Script 12 final model table exists",
    "Script 12 final coefficient table exists",
    "Script 12 main coefficients table exists",
    "Script 12 appendix coefficients table exists",
    "Script 12 caution register exists",
    "Script 12 publication readiness table exists",
    "Script 11 model quality table exists",
    "Script 11 coefficient review table exists",
    "Script 11 model stage review table exists",
    "Script 11 main-strict robustness table exists",
    "Socioecological framework created",
    "Outcome dictionary created",
    "Interpretive summary created",
    "Key findings created",
    "Public key results table created",
    "Exploratory block summary created",
    "Public caution table created",
    "M0 robustness summary created",
    "Markdown report exported",
    "Word report exported",
    "Excel workbook exported",
    "Methodological notes exported",
    "No individual-level output exported",
    "Report avoids causal interpretation"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(dir.exists(outputs_tables_dir), "OK", "FAIL"),
    ifelse(dir.exists(outputs_diag_dir), "OK", "FAIL"),
    ifelse(dir.exists(docs_dir), "OK", "FAIL"),
    ifelse(file.exists(script12_final_model_table_path), "OK", "FAIL"),
    ifelse(file.exists(script12_final_coefficient_table_path), "OK", "FAIL"),
    ifelse(file.exists(script12_main_coefficients_path), "OK", "FAIL"),
    ifelse(file.exists(script12_appendix_coefficients_path), "OK", "FAIL"),
    ifelse(file.exists(script12_caution_register_path), "OK", "FAIL"),
    ifelse(file.exists(script12_publication_readiness_path), "OK", "FAIL"),
    ifelse(file.exists(script11_model_quality_path), "OK", "FAIL"),
    ifelse(file.exists(script11_coefficient_review_path), "OK", "FAIL"),
    ifelse(file.exists(script11_model_stage_review_path), "OK", "FAIL"),
    ifelse(file.exists(script11_main_strict_robustness_path), "OK", "FAIL"),
    ifelse(nrow(socioecological_framework) > 0, "OK", "FAIL"),
    ifelse(nrow(outcome_dictionary) > 0, "OK", "FAIL"),
    ifelse(nrow(interpretive_summary) > 0, "OK", "FAIL"),
    ifelse(nrow(key_findings) > 0, "OK", "FAIL"),
    ifelse(nrow(public_key_results) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(exploratory_block_summary) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(public_caution_results) >= 0, "OK", "FAIL"),
    ifelse(nrow(robustness_m0_summary) > 0, "OK", "WARNING_EMPTY"),
    ifelse(file.exists(markdown_path), "OK", "FAIL"),
    ifelse(file.exists(docx_path), "OK", "FAIL"),
    ifelse(file.exists(xlsx_path), "OK", "FAIL"),
    ifelse(file.exists(file.path(outputs_tables_dir, "script13b_wave01_methodological_notes.csv")), "OK", "FAIL"),
    "OK",
    "OK"
  )
)

write_csv(
  script13b_checklist,
  file.path(outputs_diag_dir, "script13b_execution_checklist.csv")
)
# ============================================================
# 15b. Portuguese public-facing report
# ============================================================

translate_sample_pt <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "Main sample: grades 10-12" ~ "Amostra principal: 10.ª a 12.ª classe",
    x == "Sensitivity sample: grades 10-12 and ages 15-19" ~
      "Amostra de sensibilidade: 10.ª a 12.ª classe e 15 a 19 anos",
    TRUE ~ x
  )
}

translate_domain_pt <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "Sexual initiation" ~ "Iniciação sexual",
    x == "Contraception and protection" ~ "Contraceção e proteção",
    x == "Pregnancy and reproductive experience" ~ "Gravidez e experiência reprodutiva",
    x == "STI-related outcomes" ~ "Indicadores relacionados com IST",
    TRUE ~ x
  )
}

translate_outcome_pt <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "Sexual initiation" ~ "Iniciação sexual",
    x == "Birth control use at first sex" ~
      "Uso de contraceção na primeira relação sexual",
    x == "Birth control use at most recent sex" ~
      "Uso de contraceção na relação sexual mais recente",
    x == "Ever used a condom during sex" ~
      "Uso de preservativo alguma vez",
    x == "Ever used birth-control pills" ~
      "Uso de pílula anticoncecional alguma vez",
    x == "Ever been pregnant" ~
      "Gravidez alguma vez",
    x == "STD testing or treatment" ~
      "Teste ou tratamento de IST",
    x == "Self-reported chlamydia diagnosis" ~
      "Diagnóstico autorreportado de clamídia",
    x == "Self-reported gonorrhea diagnosis" ~
      "Diagnóstico autorreportado de gonorreia",
    TRUE ~ x
  )
}

translate_predictor_pt <- function(x) {
  x <- safe_character(x)
  
  dplyr::case_when(
    x == "Age" ~ "Idade",
    x == "Female sex" ~ "Sexo feminino",
    x == "School grade" ~ "Classe escolar",
    x == "11th grade vs 10th grade" ~
      "11.ª classe vs 10.ª classe",
    x == "12th grade vs 10th grade" ~
      "12.ª classe vs 10.ª classe",
    TRUE ~ x
  )
}

translate_issue_pt <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "Coefficient requires review or appendix placement." ~
      "Coeficiente exige revisão ou apresentação em anexo.",
    x == "Outcome requires manual review before public interpretation." ~
      "Desfecho exige revisão manual antes de interpretação pública.",
    TRUE ~ x
  )
}

translate_robustness_pt <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "high_directional_robustness" ~ "Robustez direcional elevada",
    x == "moderate_directional_robustness" ~ "Robustez direcional moderada",
    x == "low_or_unstable_directional_robustness" ~
      "Robustez direcional baixa ou instável",
    x == "High directional robustness" ~ "Robustez direcional elevada",
    x == "Moderate directional robustness" ~ "Robustez direcional moderada",
    TRUE ~ x
  )
}

translate_model_block_pt <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "M1: family and household context" ~
      "M1: contexto familiar e doméstico",
    x == "M2: school context" ~
      "M2: contexto escolar",
    x == "M3: knowledge, attitudes and perceptions" ~
      "M3: conhecimentos, atitudes e perceções",
    x == "M4: peers and relationships" ~
      "M4: pares e relações",
    x == "M5: general risk behaviors" ~
      "M5: comportamentos gerais de risco",
    TRUE ~ x
  )
}

translate_level_pt <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "Individual demographic level" ~ "Nível individual demográfico",
    x == "Psychosocial level" ~ "Nível psicossocial",
    x == "Family and household level" ~ "Nível familiar e doméstico",
    x == "Peer and relationship level" ~ "Nível dos pares e relações",
    x == "School level" ~ "Nível escolar",
    x == "Health outcome level" ~ "Nível dos desfechos de saúde",
    x == "Family and household level" ~ "Nível familiar e doméstico",
    x == "Peer and relationship level" ~ "Nível dos pares e relações",
    x == "Individual risk-behavior level" ~
      "Nível individual de comportamentos de risco",
    TRUE ~ x
  )
}

translate_interpretive_decision_pt <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    stringr::str_detect(x, "not promoted to public conclusion") ~
      "Exploratório; não promovido a conclusão pública porque predominam sinais de revisão.",
    stringr::str_detect(x, "may inform hypotheses") ~
      "Exploratório; pode informar hipóteses, mas exige cautela.",
    stringr::str_detect(x, "no model was fitted") ~
      "Não interpretado porque nenhum modelo foi ajustado.",
    TRUE ~ x
  )
}

public_key_results_pt <- public_key_results_compact %>%
  mutate(
    Amostra = translate_sample_pt(Sample),
    Domínio = translate_domain_pt(Domain),
    Desfecho = translate_outcome_pt(Outcome),
    Preditor = translate_predictor_pt(Predictor),
    p = dplyr::case_when(
      p == "<0.001" ~ "<0,001",
      TRUE ~ stringr::str_replace(p, "\\.", ",")
    )
  ) %>%
  select(
    Amostra,
    Domínio,
    Desfecho,
    Preditor,
    OR,
    `IC 95%` = `95 percent CI`,
    p
  )

public_key_results_main_pt <- public_key_results_pt %>%
  filter(Amostra == "Amostra principal: 10.ª a 12.ª classe") %>%
  select(
    Domínio,
    Desfecho,
    Preditor,
    OR,
    `IC 95%`,
    p
  )

public_key_results_sensitivity_pt <- public_key_results_pt %>%
  filter(Amostra == "Amostra de sensibilidade: 10.ª a 12.ª classe e 15 a 19 anos") %>%
  select(
    Domínio,
    Desfecho,
    Preditor,
    OR,
    `IC 95%`,
    p
  )

socioecological_framework_pt <- tibble::tibble(
  Nível = c(
    "Nível individual demográfico",
    "Nível psicossocial",
    "Nível familiar e doméstico",
    "Nível dos pares e relações",
    "Nível escolar",
    "Nível dos desfechos de saúde"
  ),
  `Interpretação no relatório` = c(
    "Idade, sexo e classe escolar são tratados como marcadores demográficos e de desenvolvimento.",
    "Conhecimentos, atitudes, perceções de risco, normas subjetivas e autoeficácia são interpretados como mecanismos psicossociais.",
    "Apoio familiar, monitoria parental e contexto doméstico são interpretados como fatores interpessoais de proteção ou vulnerabilidade.",
    "Pares e relações são interpretados como fontes de pressão social, normas e suporte.",
    "Ligação à escola e clima escolar são interpretados como recursos contextuais de proteção ou vulnerabilidade.",
    "Iniciação sexual, contraceção/proteção, gravidez e indicadores de IST são tratados como desfechos."
  )
)

exploratory_blocks_pt <- main_exploratory_blocks %>%
  transmute(
    `Nível socioecológico` = translate_level_pt(`Socioecological level`),
    `Bloco do modelo` = translate_model_block_pt(`Model block`),
    `Modelos ajustados` = `Models fitted`,
    `Modelos tecnicamente estáveis` = `Technically stable models`,
    `Usáveis com cautela` = `Usable with caution`,
    `Revisão antes do reporte` = `Review before reporting`,
    `Decisão interpretativa` =
      translate_interpretive_decision_pt(`Interpretive decision`)
  )

caution_summary_main_pt <- caution_summary_main_report %>%
  transmute(
    Domínio = translate_domain_pt(Domain),
    Desfecho = translate_outcome_pt(Outcome),
    Questão = translate_issue_pt(Issue),
    `Número de sinais de cautela` = `Number of caution flags`
  )

caution_summary_sensitivity_pt <- caution_summary_sensitivity_report %>%
  transmute(
    Domínio = translate_domain_pt(Domain),
    Desfecho = translate_outcome_pt(Outcome),
    Questão = translate_issue_pt(Issue),
    `Número de sinais de cautela` = `Number of caution flags`
  )

robustness_report_pt <- robustness_m0_summary %>%
  transmute(
    Domínio = translate_domain_pt(Domain),
    Desfecho = translate_outcome_pt(Outcome),
    `Consistência direcional (%)` = `Same direction percent`,
    `Resumo da robustez` = translate_robustness_pt(`Robustness summary`)
  )

outcome_dictionary_pt <- outcome_dictionary %>%
  transmute(
    Desfecho = translate_outcome_pt(public_label),
    Domínio = translate_domain_pt(domain),
    `Nota de interpretação` = c(
      "Indicador de iniciação sexual.",
      "Indicador de uso de contraceção na primeira relação sexual.",
      "Indicador de uso de contraceção na relação sexual mais recente.",
      "Indicador de uso de preservativo alguma vez.",
      "Indicador de uso de pílula anticoncecional alguma vez.",
      "Indicador de gravidez alguma vez; o denominador depende de elegibilidade e padrões de salto.",
      "Indicador de teste ou tratamento de IST; deve ser interpretado como contacto com serviços de saúde sexual, não como diagnóstico confirmado.",
      "Indicador de diagnóstico autorreportado de clamídia.",
      "Indicador de diagnóstico autorreportado de gonorreia; raro e impreciso nesta análise."
    )
  )

make_pt_evidence <- function(df, outcome_name) {
  rows <- df %>%
    filter(Desfecho == outcome_name)

  if (nrow(rows) == 0) {
    return("Nenhum coeficiente foi priorizado para este desfecho.")
  }

  outcome_text <- dplyr::case_when(
    rows$Desfecho == "Teste ou tratamento de IST" ~
      "teste ou tratamento de IST",
    rows$Desfecho == "Diagnóstico autorreportado de clamídia" ~
      "diagnóstico autorreportado de clamídia",
    rows$Desfecho == "Diagnóstico autorreportado de gonorreia" ~
      "diagnóstico autorreportado de gonorreia",
    TRUE ~ stringr::str_to_lower(rows$Desfecho)
  )

  p_text <- dplyr::case_when(
    rows$p %in% c("<0.001", "<0,001") ~ "p < 0,001",
    stringr::str_detect(rows$p, "^<") ~
      paste0("p ", stringr::str_replace(rows$p, "\\.", ",")),
    rows$p == "" | is.na(rows$p) ~ "",
    TRUE ~ paste0(
      "p = ",
      stringr::str_replace(rows$p, "\\.", ",")
    )
  )

  paste0(
    rows$Preditor,
    " apresentou ",
    ifelse(safe_numeric(rows$OR) > 1, "maiores chances de ", "menores chances de "),
    outcome_text,
    " (OR = ",
    rows$OR,
    ", IC 95% ",
    rows$`IC 95%`,
    ", ",
    p_text,
    ").",
    collapse = " "
  )
}
sexual_main_pt <- make_pt_evidence(
  public_key_results_main_pt,
  "Iniciação sexual"
)

sexual_sens_pt <- make_pt_evidence(
  public_key_results_sensitivity_pt,
  "Iniciação sexual"
)

contraception_main_pt <- text_join(c(
  make_pt_evidence(
    public_key_results_main_pt,
    "Uso de contraceção na relação sexual mais recente"
  ),
  make_pt_evidence(
    public_key_results_main_pt,
    "Uso de preservativo alguma vez"
  )
))

contraception_sens_pt <- make_pt_evidence(
  public_key_results_sensitivity_pt,
  "Uso de preservativo alguma vez"
)

pregnancy_main_pt <- make_pt_evidence(
  public_key_results_main_pt,
  "Gravidez alguma vez"
)

pregnancy_sens_pt <- make_pt_evidence(
  public_key_results_sensitivity_pt,
  "Gravidez alguma vez"
)

sti_main_pt <- text_join(c(
  make_pt_evidence(
    public_key_results_main_pt,
    "Diagnóstico autorreportado de clamídia"
  ),
  make_pt_evidence(
    public_key_results_main_pt,
    "Teste ou tratamento de IST"
  )
))

sti_sens_pt <- make_pt_evidence(
  public_key_results_sensitivity_pt,
  "Teste ou tratamento de IST"
)

interpretive_summary_pt <- tibble::tibble(
  Secção = c(
    "Mensagem pública geral",
    "Interpretação teórica",
    "Resultado estatístico final",
    "Cautela socioecológica",
    "Família, escola e pares",
    "Implicação principal"
  ),
  Interpretação = c(
    "Os resultados públicos sustentam uma leitura socioecológica cautelosa dos comportamentos de saúde sexual e reprodutiva na adolescência.",
    "A lógica da tese original é preservada: o comportamento adolescente é interpretado através de níveis individuais, familiares, escolares, de pares e psicossociais.",
    "Os modelos públicos mais defensáveis são modelos conservadores M0, ajustados por idade, sexo e classe escolar.",
    "A predominância do M0 não significa que família, escola e pares sejam irrelevantes; significa que os modelos mais ricos não foram suficientemente estáveis para conclusões públicas principais.",
    "Os blocos familiar, escolar, de pares e psicossociais são apresentados como evidência exploratória e geradora de hipóteses.",
    "A comunicação pública deve enfatizar associações ajustadas estáveis, preservando a interpretação socioecológica."
  )
)

key_findings_pt <- tibble::tibble(
  Domínio = c(
    "Iniciação sexual",
    "Contraceção e proteção",
    "Gravidez e experiência reprodutiva",
    "Indicadores relacionados com IST",
    "Interpretação socioecológica"
  ),
  `Resultado na amostra principal` = c(
    sexual_main_pt,
    contraception_main_pt,
    pregnancy_main_pt,
    sti_main_pt,
    "Família, escola, pares e fatores psicossociais permanecem teoricamente centrais, mas são tratados como exploratórios neste relatório público."
  ),
  `Resultado na amostra de sensibilidade` = c(
    sexual_sens_pt,
    contraception_sens_pt,
    pregnancy_sens_pt,
    sti_sens_pt,
    "A mesma regra conservadora de interpretação foi aplicada à amostra de sensibilidade."
  ),
  `Interpretação pública` = c(
    "Idade e classe escolar funcionam como marcadores de desenvolvimento associados à iniciação sexual.",
    "Os comportamentos de contraceção e proteção devem ser interpretados desfecho por desfecho, pois não formam um único padrão empírico.",
    "A gravidez apresenta forte padrão etário entre respondentes elegíveis, mas os padrões de salto e elegibilidade devem ser considerados.",
    "Os indicadores de IST exigem cautela, porque combinam autorrelato, raridade de alguns desfechos e contacto com serviços de saúde.",
    "O relatório não rejeita o modelo socioecológico; distingue resultados finais estáveis de evidência exploratória multinível."
  )
)

implications_pt <- tibble::tibble(
  Área = c(
    "Interpretação pública",
    "Desenho de investigação",
    "Família e escola",
    "Pares e mecanismos psicossociais",
    "Modelação futura"
  ),
  Implicação = c(
    "Apresentar os modelos finais como associações ajustadas conservadoras, não como evidência causal.",
    "Manter o modelo socioecológico como estrutura teórica organizadora do projeto.",
    "Não concluir que família ou escola não têm papel; afirmar que a evidência empírica desses níveis permanece exploratória nesta réplica pública.",
    "Normas dos pares, atitudes, autoeficácia e controlo percebido devem continuar centrais em refinamentos futuros.",
    "Trabalhos futuros podem refinar construtos, agrupar desfechos raros ou testar modelos alternativos para estimar melhor vias multiníveis."
  )
)

limitations_pt <- tibble::tibble(
  Limitação = c(
    "Os modelos públicos finais são conservadores e principalmente demográficos.",
    "A réplica com dados públicos do Add Health não reproduz integralmente a estrutura da base original da tese.",
    "Os blocos familiar, escolar, de pares e psicossociais foram analisados, mas não promovidos a conclusões principais quando os diagnósticos dos modelos foram instáveis.",
    "Alguns desfechos dependem de padrões de salto e regras de elegibilidade.",
    "Alguns indicadores relacionados com IST são raros e exigem interpretação cautelosa.",
    "Todos os resultados são associacionais e não devem ser interpretados como efeitos causais."
  )
)

title_pt <- "Relatório Interpretativo dos Resultados"
subtitle_pt <- "Análise de Comportamentos de Risco na Adolescência com Dados Públicos do Add Health Wave I"

executive_summary_pt <- c(
  paste0(
    "Este relatório apresenta a interpretação pública dos resultados da análise de comportamentos ",
    "de risco na adolescência com dados públicos do Add Health Wave I. O documento foi preparado ",
    "para leitores interessados na mensagem substantiva do projeto, e não apenas no pipeline técnico."
  ),
  paste0(
    "A interpretação é explicitamente orientada pelo modelo socioecológico. A lógica da tese original ",
    "não se limitava a fatores individuais; incluía mecanismos individuais, familiares, escolares, ",
    "de pares e psicossociais. A réplica com dados públicos preserva essa lógica, separando resultados ",
    "finais estáveis de evidência exploratória."
  ),
  paste0(
    "O resultado empírico central é conservador: os modelos públicos finais são modelos M0 ajustados ",
    "por idade, sexo e classe escolar. Isso não significa que família, escola e pares sejam irrelevantes. ",
    "Significa que os blocos socioecológicos mais ricos não tiveram estabilidade suficiente para serem ",
    "apresentados como conclusões públicas principais nesta versão."
  )
)

theory_pt <- c(
  paste0(
    "A interpretação segue uma estrutura multinível. No nível individual, o desenvolvimento adolescente ",
    "é representado por idade, sexo e classe escolar. No nível familiar, apoio, monitoria e contexto doméstico ",
    "são interpretados como potenciais fatores de proteção ou vulnerabilidade. No nível dos pares, relações ",
    "e pressão social são entendidas como mecanismos que moldam normas e comportamentos. No nível escolar, ",
    "a ligação à escola e o apoio institucional são tratados como recursos contextuais. No nível psicossocial, ",
    "atitudes, normas subjetivas, perceções de risco e autoeficácia são interpretadas como mecanismos que ligam ",
    "contexto, intenção e comportamento."
  ),
  paste0(
    "Esta estrutura é coerente com a lógica da tese original. Contudo, a réplica com dados públicos é mais limitada ",
    "do que a base da tese. Por isso, o relatório distingue interpretação orientada pela teoria de resultados públicos ",
    "estatisticamente estáveis."
  )
)

methods_pt <- c(
  paste0(
    "A análise utiliza dados públicos do Add Health Wave I. A amostra principal inclui estudantes da 10.ª à 12.ª classe. ",
    "A amostra de sensibilidade restringe adicionalmente a análise a estudantes da 10.ª à 12.ª classe com idades entre ",
    "15 e 19 anos."
  ),
  paste0(
    "Os modelos logísticos ponderados foram revistos antes da interpretação pública. A revisão considerou dimensão da ",
    "amostra, tamanho das células do desfecho, odds ratios extremos, intervalos de confiança largos, possível sobreposição ",
    "conceptual e consistência entre a amostra principal e a amostra de sensibilidade."
  ),
  paste0(
    "A regra de reporte público é conservadora. Resultados finais estáveis são reportados diretamente. Blocos familiares, ",
    "escolares, de pares e psicossociais são descritos como exploratórios quando os diagnósticos não sustentam conclusões ",
    "públicas fortes."
  )
)

how_to_read_pt <- c(
  "Odds ratios acima de 1 indicam maiores chances do desfecho. Odds ratios abaixo de 1 indicam menores chances do desfecho. Os intervalos de confiança representam a incerteza estatística.",
  "Como a análise é observacional e baseada em dados públicos, o relatório evita linguagem causal. O relatório não afirma que idade, classe, família, pares ou escola causam os desfechos.",
  "Quando um domínio é descrito como exploratório, isso significa que ele é teoricamente importante, mas que os modelos desta réplica pública não foram suficientemente estáveis para sustentar uma conclusão empírica principal."
)

sexual_pt <- c(
  "A iniciação sexual é o domínio final mais claro. Idade e classe escolar aparecem como marcadores de desenvolvimento associados a maiores chances de iniciação sexual.",
  paste0("Evidência na amostra principal: ", sexual_main_pt),
  paste0("Evidência na amostra de sensibilidade: ", sexual_sens_pt),
  "Este resultado deve ser interpretado como padrão de desenvolvimento, não como evidência causal. A leitura é compatível com a visão socioecológica de que o desenvolvimento individual interage com família, escola, pares e normas."
)

contraception_pt <- c(
  "O domínio da contraceção e proteção é mais heterogéneo. Uso de preservativo, uso de contraceção na primeira relação, uso de contraceção na relação mais recente e uso de pílula anticoncecional são comportamentos relacionados, mas distintos.",
  paste0("Evidência na amostra principal: ", contraception_main_pt),
  paste0("Evidência na amostra de sensibilidade: ", contraception_sens_pt),
  "A mensagem pública correta é que os comportamentos de proteção não têm um único determinante simples. A evidência deve ser interpretada desfecho por desfecho."
)

pregnancy_pt <- c(
  "A gravidez alguma vez apresenta um forte padrão etário entre respondentes elegíveis. Este domínio exige atenção particular às regras de elegibilidade e aos padrões de salto do questionário.",
  paste0("Evidência na amostra principal: ", pregnancy_main_pt),
  paste0("Evidência na amostra de sensibilidade: ", pregnancy_sens_pt),
  "O resultado deve ser interpretado como padrão etário descritivo entre respondentes para os quais o item é aplicável, e não como estimativa causal geral para todos os adolescentes."
)

sti_pt <- c(
  "Os indicadores relacionados com IST exigem maior cautela. Alguns desfechos referem diagnóstico autorreportado, enquanto teste ou tratamento de IST pode refletir contacto com serviços de saúde sexual e não necessariamente infeção confirmada.",
  paste0("Evidência na amostra principal: ", sti_main_pt),
  paste0("Evidência na amostra de sensibilidade: ", sti_sens_pt),
  "O diagnóstico autorreportado de gonorreia não deve ser usado como resultado principal, porque o desfecho é raro e impreciso nesta análise."
)

exploratory_pt <- c(
  "Os blocos socioecológicos exploratórios são essenciais para a interpretação. Incluem contexto familiar e doméstico, contexto escolar, conhecimentos e atitudes, pares e relações, e comportamentos gerais de risco.",
  "Esses blocos foram analisados porque correspondem à estrutura teórica da tese. No entanto, vários modelos mais ricos apresentaram instabilidade, sinais de revisão, intervalos largos ou odds ratios extremos. Por isso, não são apresentados como conclusões públicas finais.",
  "Isto não deve ser lido como evidência de que família, escola ou pares não têm influência. Significa apenas que a presente réplica com dados públicos não fornece estimativas suficientemente estáveis para afirmações fortes sobre esses níveis."
)

conclusion_pt <- c(
  "A conclusão pública é socioecológica e conservadora. Os resultados empíricos mais estáveis concentram-se em marcadores individuais de desenvolvimento, sobretudo idade e classe escolar. Ao mesmo tempo, a interpretação teórica permanece multinível: família, escola, pares e mecanismos psicossociais continuam centrais para compreender comportamentos de saúde sexual e reprodutiva na adolescência.",
  "A réplica com dados públicos do Add Health oferece uma forma transparente e eticamente mais segura de reproduzir a lógica analítica geral da tese, evitando a publicação de microdados sensíveis da tese original.",
  "Trabalhos futuros devem refinar os construtos familiares, escolares, de pares e psicossociais, testar especificações alternativas e tratar os resultados exploratórios como hipóteses, não como conclusões causais finais."
)

# Export Portuguese CSV outputs

write_csv(
  socioecological_framework_pt,
  file.path(outputs_tables_dir, "script13b_wave01_socioecological_framework_pt.csv")
)

write_csv(
  public_key_results_pt,
  file.path(outputs_tables_dir, "script13b_wave01_public_key_results_pt.csv")
)

write_csv(
  exploratory_blocks_pt,
  file.path(outputs_tables_dir, "script13b_wave01_exploratory_blocks_pt.csv")
)

write_csv(
  caution_summary_main_pt,
  file.path(outputs_tables_dir, "script13b_wave01_caution_summary_main_pt.csv")
)

write_csv(
  caution_summary_sensitivity_pt,
  file.path(outputs_tables_dir, "script13b_wave01_caution_summary_sensitivity_pt.csv")
)

write_csv(
  robustness_report_pt,
  file.path(outputs_tables_dir, "script13b_wave01_robustness_summary_pt.csv")
)

write_csv(
  outcome_dictionary_pt,
  file.path(outputs_tables_dir, "script13b_wave01_outcome_dictionary_pt.csv")
)

write_csv(
  interpretive_summary_pt,
  file.path(outputs_tables_dir, "script13b_wave01_interpretive_summary_pt.csv")
)

write_csv(
  key_findings_pt,
  file.path(outputs_tables_dir, "script13b_wave01_key_findings_pt.csv")
)

write_csv(
  implications_pt,
  file.path(outputs_tables_dir, "script13b_wave01_implications_pt.csv")
)

write_csv(
  limitations_pt,
  file.path(outputs_tables_dir, "script13b_wave01_limitations_pt.csv")
)

# Portuguese Markdown report

markdown_pt_path <- file.path(
  docs_dir,
  "add_health_wave01_results_interpretation_report_script13b_pt.md"
)

markdown_pt <- c(
  paste0("# ", title_pt),
  "",
  paste0("## ", subtitle_pt),
  "",
  "### Sumário executivo",
  "",
  executive_summary_pt,
  "",
  "### Mensagens-chave",
  "",
  paste0("- ", interpretive_summary_pt$Interpretação),
  "",
  "### Enquadramento socioecológico",
  "",
  theory_pt,
  "",
  markdown_table(socioecological_framework_pt, max_rows = 20),
  "",
  "### Dados e abordagem analítica",
  "",
  methods_pt,
  "",
  "### Como ler os resultados",
  "",
  how_to_read_pt,
  "",
  "### Resultados públicos finais",
  "",
  "#### Amostra principal",
  "",
  markdown_table(public_key_results_main_pt, max_rows = 20),
  "",
  "#### Amostra de sensibilidade",
  "",
  markdown_table(public_key_results_sensitivity_pt, max_rows = 20),
  "",
  "### Interpretação por domínio",
  "",
  "#### Iniciação sexual",
  "",
  sexual_pt,
  "",
  "#### Contraceção e proteção",
  "",
  contraception_pt,
  "",
  "#### Gravidez e experiência reprodutiva",
  "",
  pregnancy_pt,
  "",
  "#### Indicadores relacionados com IST",
  "",
  sti_pt,
  "",
  "### Evidência socioecológica exploratória",
  "",
  exploratory_pt,
  "",
  markdown_table(exploratory_blocks_pt, max_rows = 20),
  "",
  "### Resultados que exigem cautela",
  "",
  "#### Amostra principal",
  "",
  markdown_table(caution_summary_main_pt, max_rows = 20),
  "",
  "#### Amostra de sensibilidade",
  "",
  markdown_table(caution_summary_sensitivity_pt, max_rows = 20),
  "",
  "### Robustez dos modelos conservadores",
  "",
  markdown_table(robustness_report_pt, max_rows = 20),
  "",
  "### Implicações",
  "",
  paste0("- ", implications_pt$Implicação),
  "",
  "### Limitações",
  "",
  paste0("- ", limitations_pt$Limitação),
  "",
  "### Conclusão",
  "",
  conclusion_pt
)

writeLines(markdown_pt, con = markdown_pt_path)

# Portuguese Word report

docx_pt_path <- file.path(
  docs_dir,
  "add_health_wave01_results_interpretation_report_script13b_pt.docx"
)

doc_pt <- officer::read_docx()

doc_pt <- officer::body_add_par(doc_pt, title_pt, style = "heading 1")
doc_pt <- officer::body_add_par(doc_pt, subtitle_pt, style = "heading 2")
doc_pt <- officer::body_add_par(
  doc_pt,
  "Relatório público interpretativo gerado apenas a partir de outputs agregados.",
  style = "Normal"
)

doc_pt <- officer::body_add_par(doc_pt, "Sumário executivo", style = "heading 1")
doc_pt <- add_paragraphs(doc_pt, executive_summary_pt)

doc_pt <- officer::body_add_par(doc_pt, "Mensagens-chave", style = "heading 1")
doc_pt <- add_bullets(doc_pt, interpretive_summary_pt$Interpretação)

doc_pt <- officer::body_add_par(doc_pt, "Enquadramento socioecológico", style = "heading 1")
doc_pt <- add_paragraphs(doc_pt, theory_pt)
doc_pt <- add_table_or_note(doc_pt, socioecological_framework_pt, font_size = 8)

doc_pt <- officer::body_add_par(doc_pt, "Dados e abordagem analítica", style = "heading 1")
doc_pt <- add_paragraphs(doc_pt, methods_pt)

doc_pt <- officer::body_add_par(doc_pt, "Como ler os resultados", style = "heading 1")
doc_pt <- add_paragraphs(doc_pt, how_to_read_pt)

doc_pt <- officer::body_add_par(doc_pt, "Resultados públicos finais", style = "heading 1")
doc_pt <- officer::body_add_par(
  doc_pt,
  paste0(
    "Os resultados são apresentados separadamente para a amostra principal e a amostra de sensibilidade. ",
    "Apenas coeficientes priorizados para interpretação pública cautelosa são apresentados. ",
    "Os resultados devem ser lidos como associações ajustadas, não como efeitos causais."
  ),
  style = "Normal"
)

doc_pt <- officer::body_add_par(doc_pt, "Amostra principal", style = "heading 2")
doc_pt <- add_table_or_note(doc_pt, public_key_results_main_pt, font_size = 8)

doc_pt <- officer::body_add_par(doc_pt, "Amostra de sensibilidade", style = "heading 2")
doc_pt <- add_table_or_note(doc_pt, public_key_results_sensitivity_pt, font_size = 8)

doc_pt <- officer::body_add_par(doc_pt, "Interpretação por domínio", style = "heading 1")

doc_pt <- officer::body_add_par(doc_pt, "Iniciação sexual", style = "heading 2")
doc_pt <- add_paragraphs(doc_pt, sexual_pt)

doc_pt <- officer::body_add_par(doc_pt, "Contraceção e proteção", style = "heading 2")
doc_pt <- add_paragraphs(doc_pt, contraception_pt)

doc_pt <- officer::body_add_par(doc_pt, "Gravidez e experiência reprodutiva", style = "heading 2")
doc_pt <- add_paragraphs(doc_pt, pregnancy_pt)

doc_pt <- officer::body_add_par(doc_pt, "Indicadores relacionados com IST", style = "heading 2")
doc_pt <- add_paragraphs(doc_pt, sti_pt)

doc_pt <- officer::body_add_par(doc_pt, "Evidência socioecológica exploratória", style = "heading 1")
doc_pt <- add_paragraphs(doc_pt, exploratory_pt)
doc_pt <- add_table_or_note(doc_pt, exploratory_blocks_pt, font_size = 7)

doc_pt <- officer::body_add_par(doc_pt, "Resultados que exigem cautela", style = "heading 1")
doc_pt <- officer::body_add_par(
  doc_pt,
  paste0(
    "Os resultados detalhados de cautela permanecem no Excel e nos CSVs. ",
    "O relatório público apresenta apenas uma síntese, porque a tabela completa é técnica e extensa."
  ),
  style = "Normal"
)

doc_pt <- officer::body_add_par(doc_pt, "Síntese de cautela: amostra principal", style = "heading 2")
doc_pt <- add_table_or_note(doc_pt, caution_summary_main_pt, font_size = 8)

doc_pt <- officer::body_add_par(doc_pt, "Síntese de cautela: amostra de sensibilidade", style = "heading 2")
doc_pt <- add_table_or_note(doc_pt, caution_summary_sensitivity_pt, font_size = 8)

doc_pt <- officer::body_add_par(doc_pt, "Robustez dos modelos conservadores", style = "heading 1")
doc_pt <- officer::body_add_par(
  doc_pt,
  paste0(
    "A robustez foi avaliada comparando a amostra principal com a amostra de sensibilidade. ",
    "O relatório dá maior peso a padrões que mantêm a mesma direção nas duas amostras."
  ),
  style = "Normal"
)
doc_pt <- add_table_or_note(doc_pt, robustness_report_pt, font_size = 8)

doc_pt <- officer::body_add_par(doc_pt, "Implicações", style = "heading 1")
doc_pt <- add_bullets(doc_pt, implications_pt$Implicação)

doc_pt <- officer::body_add_par(doc_pt, "Limitações", style = "heading 1")
doc_pt <- add_bullets(doc_pt, limitations_pt$Limitação)

doc_pt <- officer::body_add_par(doc_pt, "Conclusão", style = "heading 1")
doc_pt <- add_paragraphs(doc_pt, conclusion_pt)

# O dicionário completo dos desfechos permanece nos outputs CSV e Excel.
# Não é inserido no relatório Word público para evitar tabelas largas.

print(doc_pt, target = docx_pt_path)

# Portuguese Excel workbook

xlsx_pt_path <- file.path(
  outputs_tables_dir,
  "script13b_wave01_results_interpretation_report_tables_pt.xlsx"
)

wb_pt <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_pt, "sumario_interpretativo")
openxlsx::writeData(wb_pt, "sumario_interpretativo", interpretive_summary_pt)

openxlsx::addWorksheet(wb_pt, "estrutura_socioecologica")
openxlsx::writeData(wb_pt, "estrutura_socioecologica", socioecological_framework_pt)

openxlsx::addWorksheet(wb_pt, "resultados_principais")
openxlsx::writeData(wb_pt, "resultados_principais", public_key_results_pt)

openxlsx::addWorksheet(wb_pt, "blocos_exploratorios")
openxlsx::writeData(wb_pt, "blocos_exploratorios", exploratory_blocks_pt)

openxlsx::addWorksheet(wb_pt, "cautelas_principal")
openxlsx::writeData(wb_pt, "cautelas_principal", caution_summary_main_pt)

openxlsx::addWorksheet(wb_pt, "cautelas_sensibilidade")
openxlsx::writeData(wb_pt, "cautelas_sensibilidade", caution_summary_sensitivity_pt)

openxlsx::addWorksheet(wb_pt, "robustez")
openxlsx::writeData(wb_pt, "robustez", robustness_report_pt)

openxlsx::addWorksheet(wb_pt, "dicionario_desfechos")
openxlsx::writeData(wb_pt, "dicionario_desfechos", outcome_dictionary_pt)

openxlsx::addWorksheet(wb_pt, "implicacoes")
openxlsx::writeData(wb_pt, "implicacoes", implications_pt)

openxlsx::addWorksheet(wb_pt, "limitacoes")
openxlsx::writeData(wb_pt, "limitacoes", limitations_pt)

for (sheet in names(wb_pt)) {
  openxlsx::setColWidths(wb_pt, sheet = sheet, cols = 1:100, widths = "auto")
  openxlsx::freezePane(wb_pt, sheet = sheet, firstRow = TRUE)
}

openxlsx::saveWorkbook(wb_pt, xlsx_pt_path, overwrite = TRUE)

cat("\nPortuguese public report outputs created:\n")
cat("- docs/add_health_wave01_results_interpretation_report_script13b_pt.docx\n")
cat("- docs/add_health_wave01_results_interpretation_report_script13b_pt.md\n")
cat("- outputs/tables/script13b_wave01_results_interpretation_report_tables_pt.xlsx\n\n")

# ============================================================
# 16. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 13b completed: Public Results Interpretation Report\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Public-facing report outputs:\n")
cat("- docs/add_health_wave01_results_interpretation_report_script13b.docx\n")
cat("- docs/add_health_wave01_results_interpretation_report_script13b.md\n")
cat("- outputs/tables/script13b_wave01_results_interpretation_report_tables.xlsx\n\n")

cat("Interpretive summary:\n")
print(interpretive_summary)

cat("\nKey findings:\n")
print(key_findings)

cat("\nPublic key results:\n")
print(public_key_results)

cat("\nExploratory socioecological block summary - main sample:\n")
print(main_exploratory_blocks)

cat("\nCaution results:\n")
print(public_caution_results)

cat("\nM0 robustness summary:\n")
print(robustness_m0_summary)

cat("\nPublic outputs created:\n")
cat("- outputs/tables/script13b_wave01_socioecological_framework.csv\n")
cat("- outputs/tables/script13b_wave01_outcome_dictionary.csv\n")
cat("- outputs/tables/script13b_wave01_predictor_dictionary.csv\n")
cat("- outputs/tables/script13b_wave01_interpretive_summary.csv\n")
cat("- outputs/tables/script13b_wave01_key_findings.csv\n")
cat("- outputs/tables/script13b_wave01_policy_research_implications.csv\n")
cat("- outputs/tables/script13b_wave01_public_limitations.csv\n")
cat("- outputs/tables/script13b_wave01_public_model_summary.csv\n")
cat("- outputs/tables/script13b_wave01_public_key_results.csv\n")
cat("- outputs/tables/script13b_wave01_public_caution_results.csv\n")
cat("- outputs/tables/script13b_wave01_exploratory_block_summary.csv\n")
cat("- outputs/tables/script13b_wave01_main_exploratory_blocks.csv\n")
cat("- outputs/tables/script13b_wave01_public_robustness_summary.csv\n")
cat("- outputs/tables/script13b_wave01_results_interpretation_report_tables.xlsx\n")
cat("- outputs/tables/script13b_wave01_methodological_notes.csv\n")
cat("- outputs/diagnostics/script13b_execution_checklist.csv\n\n")

cat("Execution checklist:\n")
print(script13b_checklist)

cat("\nImportant note:\n")
cat("Script 13b generated the public-facing interpretation report from aggregate outputs only.\n")
cat("The report keeps the socioecological interpretation while treating richer models as exploratory.\n")
cat("Open the DOCX and review wording before committing.\n")
cat("No individual-level data were used or exported by this script.\n\n")