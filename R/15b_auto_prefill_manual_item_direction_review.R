# ============================================================
# Script 15b — Automatic Prefill of Manual Item Direction Review
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Create a semi-automatic prefilled version of the Script 15
#   manual item direction review table.
#
# Important:
#   This script does NOT create the final completed review.
#   It creates an AUTO_PREFILL file that must still be reviewed
#   before final index construction.
#
# Inputs:
#   outputs/audits/script15_manual_item_direction_review_WORKING_COPY.csv
#
# Outputs:
#   outputs/audits/script15_manual_item_direction_review_AUTO_PREFILL.csv
#   outputs/audits/script15_manual_item_direction_review_REVIEW_REQUIRED.csv
#   outputs/audits/script15b_auto_prefill_summary.csv
#   docs/add_health_wave01_auto_prefill_manual_item_direction_review_script15b.docx
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

if (has_officer) {
  suppressPackageStartupMessages(library(officer))
}

if (has_flextable) {
  suppressPackageStartupMessages(library(flextable))
}

# ------------------------------------------------------------
# 1. Project root
# ------------------------------------------------------------

project_root <- "C:/Users/LENOVO/GitHub/add-health-adolescent-risk-models"

if (!dir.exists(project_root)) {
  stop("Project root not found: ", project_root)
}

setwd(project_root)

output_dir <- file.path(project_root, "outputs", "audits")
doc_dir <- file.path(project_root, "docs")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 15b started: Automatic Prefill of Manual Item Review\n")
cat("============================================================\n\n")
cat("Project root:\n", project_root, "\n\n")

# ------------------------------------------------------------
# 2. Input files
# ------------------------------------------------------------

working_copy_path <- file.path(
  output_dir,
  "script15_manual_item_direction_review_WORKING_COPY.csv"
)

template_path <- file.path(
  output_dir,
  "script15_manual_item_direction_review_template.csv"
)

if (!file.exists(working_copy_path) && !file.exists(template_path)) {
  stop(
    "Neither WORKING_COPY nor template file was found.\n",
    "Run Script 15 before Script 15b."
  )
}

input_path <- if (file.exists(working_copy_path)) {
  working_copy_path
} else {
  template_path
}

review_data <- readr::read_csv(
  input_path,
  show_col_types = FALSE
)

cat("Input review file loaded:\n")
cat(input_path, "\n\n")

# ------------------------------------------------------------
# 3. Required columns
# ------------------------------------------------------------

required_columns <- c(
  "section_id",
  "section_name",
  "variable",
  "label",
  "value_labels",
  "direction_class",
  "suggested_final_role",
  "suggested_score_direction",
  "manual_final_role",
  "manual_score_direction",
  "manual_reverse_score",
  "manual_include_in_index",
  "manual_construct_label",
  "manual_decision_rationale",
  "manual_reviewer",
  "manual_review_date"
)

missing_required_columns <- setdiff(required_columns, names(review_data))

if (length(missing_required_columns) > 0) {
  stop(
    "The review file is missing required columns: ",
    paste(missing_required_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 4. Helper functions
# ------------------------------------------------------------

clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_squish(x)
}

has_pattern <- function(x, pattern) {
  stringr::str_detect(
    stringr::str_to_lower(clean_chr(x)),
    pattern
  )
}

first_nonempty <- function(x, y) {
  x <- as.character(x)
  y <- as.character(y)
  ifelse(is.na(x) | x == "", y, x)
}

today_chr <- as.character(Sys.Date())

# ------------------------------------------------------------
# 5. Conceptual rule dictionary
# ------------------------------------------------------------

school_protection_regex <- paste(
  c(
    "feel safe",
    "safe in your school",
    "part of your school",
    "feel close to people at your school",
    "teachers treat students fairly",
    "happy to be at school",
    "connected",
    "belong",
    "school safety",
    "school connectedness",
    "teacher"
  ),
  collapse = "|"
)

family_protection_regex <- paste(
  c(
    "mother",
    "father",
    "parent",
    "parents",
    "family",
    "household",
    "care",
    "cares",
    "understand",
    "understands",
    "close",
    "warm",
    "talk",
    "communication",
    "supervision",
    "monitor",
    "dinner",
    "relationship"
  ),
  collapse = "|"
)

future_protection_regex <- paste(
  c(
    "future",
    "expect",
    "expectation",
    "college",
    "university",
    "graduate",
    "graduat",
    "aspire",
    "aspiration",
    "plan",
    "job",
    "work",
    "school performance",
    "motivation"
  ),
  collapse = "|"
)

religion_protection_regex <- paste(
  c(
    "relig",
    "church",
    "pray",
    "prayer",
    "god",
    "faith",
    "service",
    "worship",
    "moral"
  ),
  collapse = "|"
)

emotional_risk_regex <- paste(
  c(
    "depress",
    "sad",
    "lonely",
    "cry",
    "trouble",
    "bothered",
    "could not shake",
    "blue",
    "fearful",
    "unhappy",
    "emotional",
    "negative affect"
  ),
  collapse = "|"
)

behavioral_risk_regex <- paste(
  c(
    "impulsive",
    "temper",
    "anger",
    "fight",
    "weapon",
    "gang",
    "suspend",
    "skip school",
    "trouble",
    "problem",
    "smoke",
    "drink",
    "drunk",
    "drug",
    "marijuana",
    "delinquen"
  ),
  collapse = "|"
)

sexual_risk_perception_regex <- paste(
  c(
    "risk of pregnancy",
    "risk.*pregnancy",
    "pregnancy.*risk",
    "risk of aids",
    "aids.*risk",
    "hiv.*risk",
    "risk.*hiv",
    "without protection",
    "without birth control",
    "without condom",
    "unprotected sex"
  ),
  collapse = "|"
)

control_or_exclude_regex <- paste(
  c(
    "grade",
    "school grade",
    "age",
    "sex",
    "gender",
    "bio_sex",
    "weight",
    "gswgt",
    "residence",
    "urban",
    "rural",
    "suburban",
    "respondent id",
    "caseid",
    "sample",
    "flag"
  ),
  collapse = "|"
)

ambiguous_normative_regex <- paste(
  c(
    "shame",
    "ashamed",
    "guilt",
    "guilty",
    "friends would",
    "parents would",
    "people would",
    "approve",
    "approval",
    "disapprove",
    "moral",
    "wrong",
    "embarrass",
    "embarrassed"
  ),
  collapse = "|"
)

contraception_or_peer_ambiguous_regex <- paste(
  c(
    "birth control",
    "contracept",
    "condom",
    "method",
    "friends",
    "peer",
    "partner",
    "boyfriend",
    "girlfriend",
    "decision"
  ),
  collapse = "|"
)

agreement_scale_regex <- "strongly agree|agree|disagree|strongly disagree"
higher_positive_scale_regex <- "not at all|very much|quite a bit|somewhat|a lot"
frequency_scale_regex <- "never|rarely|sometimes|often|always|most of the time|all of the time"

# ------------------------------------------------------------
# 6. Automatic decision function
# ------------------------------------------------------------

prefill_decision <- function(section_id,
                             variable,
                             label,
                             value_labels,
                             direction_class,
                             suggested_final_role) {

  text <- paste(variable, label, value_labels, sep = " | ")
  text_l <- stringr::str_to_lower(clean_chr(text))
  var_l <- stringr::str_to_lower(clean_chr(variable))
  values_l <- stringr::str_to_lower(clean_chr(value_labels))

  # Default result
  result <- list(
    auto_final_role = "ambiguous_review",
    auto_score_direction = "not_applicable",
    auto_reverse_score = "not_applicable",
    auto_include_in_index = "no",
    auto_construct_label = "requires manual classification",
    auto_decision_rationale = "Automatic rules could not assign a defensible final role.",
    auto_review_status = "needs_manual_review",
    auto_confidence = "low"
  )

  # Exclude clear covariates and administrative variables.
  # Exclude only clear covariates and administrative variables.
# This rule is intentionally narrow because some substantive
# aspiration items may contain words such as "school" or "grade".
if (
  var_l %in% c(
    "h1gi20",
    "bio_sex",
    "gswgt1",
    "h1ir12",
    "caseid",
    "aid"
  ) ||
    stringr::str_detect(var_l, "^a_") ||
    stringr::str_detect(var_l, "sample|flag|weight|wgt|caseid|id$")
) {
  result$auto_final_role <- "exclude"
  result$auto_score_direction <- "not_applicable"
  result$auto_reverse_score <- "not_applicable"
  result$auto_include_in_index <- "no"
  result$auto_construct_label <- "covariate or administrative variable"
  result$auto_decision_rationale <- "The item is treated as a covariate, context variable, weight or administrative field rather than a protection/risk construct."
  result$auto_review_status <- "exclude_recommended"
  result$auto_confidence <- "high"
      return(result)
  }

  # Sexual health risk perception: treated as protective awareness, not direct risk.
  # Sexual health risk perception: treated as protective awareness, not direct risk.
  if (has_pattern(text_l, sexual_risk_perception_regex)) {
    result$auto_final_role <- "protection"
    result$auto_score_direction <- "higher_is_more_protective"
    result$auto_reverse_score <- "no"
    result$auto_include_in_index <- "yes"
    result$auto_construct_label <- "perceived sexual health risk"
    result$auto_decision_rationale <- "Higher perceived risk of pregnancy, HIV or AIDS from unprotected sex is treated as protective awareness rather than direct risk exposure."
    result$auto_review_status <- "check_theoretical_direction"
    result$auto_confidence <- "medium"
    return(result)
  }

  # Ambiguous normative or moral pressure items.
  if (has_pattern(text_l, ambiguous_normative_regex)) {
    result$auto_final_role <- "ambiguous_review"
    result$auto_score_direction <- "not_applicable"
    result$auto_reverse_score <- "not_applicable"
    result$auto_include_in_index <- "no"
    result$auto_construct_label <- "normative or moral orientation"
    result$auto_decision_rationale <- "The item may reflect protective norms, stigma, moral pressure or social desirability; it requires theoretical review before inclusion."
    result$auto_review_status <- "needs_manual_review"
    result$auto_confidence <- "medium"
    return(result)
  }

  # Ambiguous contraception or peer decision items.
  if (has_pattern(text_l, contraception_or_peer_ambiguous_regex) &&
      !has_pattern(text_l, sexual_risk_perception_regex)) {
    result$auto_final_role <- "ambiguous_review"
    result$auto_score_direction <- "not_applicable"
    result$auto_reverse_score <- "not_applicable"
    result$auto_include_in_index <- "no"
    result$auto_construct_label <- "peer or contraceptive context"
    result$auto_decision_rationale <- "The item may reflect knowledge, access, norms or behavior rather than a clear protection/risk construct; it requires manual review."
    result$auto_review_status <- "needs_manual_review"
    result$auto_confidence <- "medium"
    return(result)
  }

  # School protection.
  if (has_pattern(text_l, school_protection_regex)) {
    result$auto_final_role <- "protection"
    result$auto_construct_label <- "school connectedness and safety"
    result$auto_include_in_index <- "yes"
    result$auto_decision_rationale <- "The item measures school connectedness, school safety or supportive school climate."

    if (has_pattern(values_l, agreement_scale_regex)) {
      result$auto_score_direction <- "lower_is_more_protective"
      result$auto_reverse_score <- "yes"
    } else {
      result$auto_score_direction <- "higher_is_more_protective"
      result$auto_reverse_score <- "no"
    }

    result$auto_review_status <- "ready_after_light_review"
    result$auto_confidence <- "medium"
    return(result)
  }

  # Family protection.
  if (has_pattern(text_l, family_protection_regex)) {
    result$auto_final_role <- "protection"
    result$auto_construct_label <- "family support and parental connectedness"
    result$auto_include_in_index <- "yes"
    result$auto_decision_rationale <- "The item measures family support, parental closeness, communication or supervision."

    if (has_pattern(values_l, agreement_scale_regex)) {
      result$auto_score_direction <- "lower_is_more_protective"
      result$auto_reverse_score <- "yes"
    } else if (has_pattern(values_l, higher_positive_scale_regex)) {
      result$auto_score_direction <- "higher_is_more_protective"
      result$auto_reverse_score <- "no"
    } else {
      result$auto_score_direction <- "higher_is_more_protective"
      result$auto_reverse_score <- "no"
    }

    result$auto_review_status <- "ready_after_light_review"
    result$auto_confidence <- "medium"
    return(result)
  }

  # Future orientation.
  if (has_pattern(text_l, future_protection_regex)) {
    result$auto_final_role <- "protection"
    result$auto_construct_label <- "future orientation and educational aspirations"
    result$auto_include_in_index <- "yes"
    result$auto_decision_rationale <- "The item measures future orientation, educational aspiration or expectation."

    if (has_pattern(values_l, agreement_scale_regex)) {
      result$auto_score_direction <- "lower_is_more_protective"
      result$auto_reverse_score <- "yes"
    } else {
      result$auto_score_direction <- "higher_is_more_protective"
      result$auto_reverse_score <- "no"
    }

    result$auto_review_status <- "ready_after_light_review"
    result$auto_confidence <- "medium"
    return(result)
  }

  # Religion as potential protection, but not always final.
  if (has_pattern(text_l, religion_protection_regex)) {
    result$auto_final_role <- "protection"
    result$auto_construct_label <- "religiosity and moral orientation"
    result$auto_include_in_index <- "yes"
    result$auto_decision_rationale <- "The item measures religiosity or moral orientation, treated as a candidate protective factor pending theoretical review."

    if (has_pattern(values_l, agreement_scale_regex)) {
      result$auto_score_direction <- "lower_is_more_protective"
      result$auto_reverse_score <- "yes"
    } else {
      result$auto_score_direction <- "higher_is_more_protective"
      result$auto_reverse_score <- "no"
    }

    result$auto_review_status <- "check_theoretical_direction"
    result$auto_confidence <- "medium"
    return(result)
  }

  # Emotional risk.
  if (has_pattern(text_l, emotional_risk_regex)) {
    result$auto_final_role <- "risk"
    result$auto_construct_label <- "negative emotional affect"
    result$auto_include_in_index <- "yes"
    result$auto_decision_rationale <- "The item measures negative emotional affect or depressive symptoms."

    if (has_pattern(values_l, frequency_scale_regex)) {
      result$auto_score_direction <- "higher_is_more_risky"
      result$auto_reverse_score <- "no"
    } else if (has_pattern(values_l, agreement_scale_regex)) {
      result$auto_score_direction <- "lower_is_more_risky"
      result$auto_reverse_score <- "yes"
    } else {
      result$auto_score_direction <- "higher_is_more_risky"
      result$auto_reverse_score <- "no"
    }

    result$auto_review_status <- "ready_after_light_review"
    result$auto_confidence <- "medium"
    return(result)
  }

  # Behavioral risk.
  if (has_pattern(text_l, behavioral_risk_regex)) {
    result$auto_final_role <- "risk"
    result$auto_construct_label <- "behavioral risk propensity"
    result$auto_include_in_index <- "yes"
    result$auto_decision_rationale <- "The item measures behavioral risk propensity, impulsivity, conflict or problem behavior."

    if (has_pattern(values_l, agreement_scale_regex)) {
      result$auto_score_direction <- "lower_is_more_risky"
      result$auto_reverse_score <- "yes"
    } else {
      result$auto_score_direction <- "higher_is_more_risky"
      result$auto_reverse_score <- "no"
    }

    result$auto_review_status <- "ready_after_light_review"
    result$auto_confidence <- "medium"
    return(result)
  }

  # Fallback from Script 14 classification.
  if (suggested_final_role == "protection" || direction_class == "potential_protection") {
    result$auto_final_role <- "protection"
    result$auto_construct_label <- "candidate protection construct"
    result$auto_include_in_index <- "yes"
    result$auto_decision_rationale <- "Script 14 classified this item as a potential protective construct; coding direction still requires review."

    if (has_pattern(values_l, agreement_scale_regex)) {
      result$auto_score_direction <- "lower_is_more_protective"
      result$auto_reverse_score <- "yes"
    } else {
      result$auto_score_direction <- "higher_is_more_protective"
      result$auto_reverse_score <- "no"
    }

    result$auto_review_status <- "check_coding_direction"
    result$auto_confidence <- "low"
    return(result)
  }

  if (suggested_final_role == "risk" || direction_class == "potential_risk") {
    result$auto_final_role <- "risk"
    result$auto_construct_label <- "candidate risk construct"
    result$auto_include_in_index <- "yes"
    result$auto_decision_rationale <- "Script 14 classified this item as a potential risk construct; coding direction still requires review."

    if (has_pattern(values_l, agreement_scale_regex)) {
      result$auto_score_direction <- "lower_is_more_risky"
      result$auto_reverse_score <- "yes"
    } else {
      result$auto_score_direction <- "higher_is_more_risky"
      result$auto_reverse_score <- "no"
    }

    result$auto_review_status <- "check_coding_direction"
    result$auto_confidence <- "low"
    return(result)
  }

  result
}

# ------------------------------------------------------------
# 7. Apply automatic prefill
# ------------------------------------------------------------

auto_decisions <- purrr::pmap_dfr(
  review_data %>%
    select(
      section_id,
      variable,
      label,
      value_labels,
      direction_class,
      suggested_final_role
    ),
  function(section_id, variable, label, value_labels, direction_class, suggested_final_role) {

    out <- prefill_decision(
      section_id = section_id,
      variable = variable,
      label = label,
      value_labels = value_labels,
      direction_class = direction_class,
      suggested_final_role = suggested_final_role
    )

    tibble(
      auto_final_role = out$auto_final_role,
      auto_score_direction = out$auto_score_direction,
      auto_reverse_score = out$auto_reverse_score,
      auto_include_in_index = out$auto_include_in_index,
      auto_construct_label = out$auto_construct_label,
      auto_decision_rationale = out$auto_decision_rationale,
      auto_review_status = out$auto_review_status,
      auto_confidence = out$auto_confidence
    )
  }
)

auto_prefill <- bind_cols(
  review_data,
  auto_decisions
) %>%
  mutate(
    manual_final_role = auto_final_role,
    manual_score_direction = auto_score_direction,
    manual_reverse_score = auto_reverse_score,
    manual_include_in_index = auto_include_in_index,
    manual_construct_label = auto_construct_label,
    manual_decision_rationale = auto_decision_rationale,
    manual_reviewer = "AUTO_PREFILL_REQUIRES_CONFIRMATION",
    manual_review_date = today_chr,

    final_review_required = case_when(
      auto_review_status %in% c(
        "needs_manual_review",
        "check_theoretical_direction",
        "check_coding_direction"
      ) ~ "yes",
      auto_confidence == "low" ~ "yes",
      TRUE ~ "light_review"
    ),

    auto_prefill_warning = case_when(
      auto_final_role == "ambiguous_review" ~
        "Do not include in final index until manually reviewed.",
      auto_review_status == "check_theoretical_direction" ~
        "Review theoretical role before final inclusion.",
      auto_review_status == "check_coding_direction" ~
        "Review item coding and reverse scoring before final inclusion.",
      auto_review_status == "exclude_recommended" ~
        "Exclusion recommended by automatic rules.",
      TRUE ~
        "Light review recommended."
    )
  ) %>%
  select(
    section_id,
    section_name,
    variable,
    label,
    value_labels,
    numeric_min,
    numeric_max,
    numeric_mean,
    numeric_sd,
    non_missing_n,
    distinct_n,
    direction_class,
    classification_status,
    suggested_final_role,
    suggested_score_direction,
    suggested_include_in_index,
    auto_final_role,
    auto_score_direction,
    auto_reverse_score,
    auto_include_in_index,
    auto_construct_label,
    auto_decision_rationale,
    auto_review_status,
    auto_confidence,
    final_review_required,
    auto_prefill_warning,
    manual_final_role,
    manual_score_direction,
    manual_reverse_score,
    manual_include_in_index,
    manual_construct_label,
    manual_decision_rationale,
    manual_reviewer,
    manual_review_date,
    review_priority,
    review_instruction,
    detection_source,
    file_name,
    object_name
  )

# ------------------------------------------------------------
# 8. Save outputs
# ------------------------------------------------------------

auto_prefill_path <- file.path(
  output_dir,
  "script15_manual_item_direction_review_AUTO_PREFILL.csv"
)

review_required_path <- file.path(
  output_dir,
  "script15_manual_item_direction_review_REVIEW_REQUIRED.csv"
)

completed_candidate_path <- file.path(
  output_dir,
  "script15_manual_item_direction_review_COMPLETED_CANDIDATE_FROM_AUTO_PREFILL.csv"
)

readr::write_csv(
  auto_prefill,
  auto_prefill_path
)

review_required <- auto_prefill %>%
  filter(final_review_required == "yes") %>%
  arrange(section_id, auto_review_status, variable)

readr::write_csv(
  review_required,
  review_required_path
)

# This is deliberately named COMPLETED_CANDIDATE, not COMPLETED.
# It should be manually checked before being renamed as COMPLETED.
readr::write_csv(
  auto_prefill,
  completed_candidate_path
)

# ------------------------------------------------------------
# 9. Summary tables
# ------------------------------------------------------------

auto_prefill_summary <- auto_prefill %>%
  count(
    section_id,
    section_name,
    auto_final_role,
    auto_review_status,
    auto_confidence,
    final_review_required,
    name = "items"
  ) %>%
  arrange(section_id, auto_final_role, auto_review_status)

role_summary <- auto_prefill %>%
  count(auto_final_role, auto_include_in_index, final_review_required, name = "items") %>%
  arrange(auto_final_role, auto_include_in_index, final_review_required)

construct_summary <- auto_prefill %>%
  count(auto_construct_label, auto_final_role, auto_include_in_index, name = "items") %>%
  arrange(auto_final_role, desc(items), auto_construct_label)

review_burden_summary <- tibble(
  total_items = nrow(auto_prefill),
  items_prefilled_as_protection = sum(auto_prefill$auto_final_role == "protection"),
  items_prefilled_as_risk = sum(auto_prefill$auto_final_role == "risk"),
  items_prefilled_as_ambiguous = sum(auto_prefill$auto_final_role == "ambiguous_review"),
  items_prefilled_as_exclude = sum(auto_prefill$auto_final_role == "exclude"),
  items_requiring_manual_review = sum(auto_prefill$final_review_required == "yes"),
  items_requiring_light_review = sum(auto_prefill$final_review_required == "light_review")
)

readr::write_csv(
  auto_prefill_summary,
  file.path(output_dir, "script15b_auto_prefill_summary.csv")
)

readr::write_csv(
  role_summary,
  file.path(output_dir, "script15b_auto_prefill_role_summary.csv")
)

readr::write_csv(
  construct_summary,
  file.path(output_dir, "script15b_auto_prefill_construct_summary.csv")
)

readr::write_csv(
  review_burden_summary,
  file.path(output_dir, "script15b_review_burden_summary.csv")
)

# ------------------------------------------------------------
# 10. Methodological decisions
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Automation scope", "Script 15b performs semi-automatic prefill only. It does not create the final completed manual review.",
  "Protection assignment", "Items with clear school, family, future-orientation, religiosity or sexual-health-risk-perception content are prefilled as candidate protection.",
  "Risk assignment", "Items with clear negative emotional affect or behavioral risk content are prefilled as candidate risk.",
  "Sexual risk perception", "Items measuring perceived pregnancy, HIV or AIDS risk from unprotected sex are treated as protective awareness, not as direct risk exposure.",
  "Ambiguous assignment", "Normative, moral, peer, contraceptive or unclear items are retained as ambiguous_review unless the wording clearly supports protection or risk.",
  "Exclusion", "Administrative variables, covariates and contextual controls are prefilled as exclude.",
  "Manual review", "All low-confidence, ambiguous, theoretical-direction and coding-direction cases require manual confirmation before final index construction.",
  "Next step", "Review AUTO_PREFILL and REVIEW_REQUIRED files. After confirmation, save the final file as script15_manual_item_direction_review_COMPLETED.csv."
)

readr::write_csv(
  methodological_decisions,
  file.path(output_dir, "script15b_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 11. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_auto_prefill_manual_item_direction_review_script15b.docx"
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
      "Add Health Wave I — Automatic Prefill of Manual Item Direction Review",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 15b creates a semi-automatic prefilled version of the manual item direction review table. The output is not a final completed review. It must be checked before final index construction.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Review burden summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(review_burden_summary)
    ) %>%
    officer::body_add_par("Role summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(role_summary)
    ) %>%
    officer::body_add_par("Automatic prefill by section", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(auto_prefill_summary)
    ) %>%
    officer::body_add_par("Construct summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(construct_summary)
    ) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(methodological_decisions)
    ) %>%
    officer::body_add_par("Required action", style = "heading 2") %>%
    officer::body_add_par(
      paste0(
        "Open outputs/audits/script15_manual_item_direction_review_AUTO_PREFILL.csv ",
        "and outputs/audits/script15_manual_item_direction_review_REVIEW_REQUIRED.csv. ",
        "After checking the decisions, save the approved file as ",
        "outputs/audits/script15_manual_item_direction_review_COMPLETED.csv."
      ),
      style = "Normal"
    )

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 12. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "input_review_file_loaded",
    "auto_prefill_file_created",
    "review_required_file_created",
    "completed_candidate_file_created",
    "auto_prefill_summary_created",
    "role_summary_created",
    "construct_summary_created",
    "review_burden_summary_created",
    "word_report_created",
    "manual_confirmation_still_required"
  ),
  status = c(
    nrow(review_data) > 0,
    file.exists(auto_prefill_path),
    file.exists(review_required_path),
    file.exists(completed_candidate_path),
    file.exists(file.path(output_dir, "script15b_auto_prefill_summary.csv")),
    file.exists(file.path(output_dir, "script15b_auto_prefill_role_summary.csv")),
    file.exists(file.path(output_dir, "script15b_auto_prefill_construct_summary.csv")),
    file.exists(file.path(output_dir, "script15b_review_burden_summary.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

readr::write_csv(
  final_status,
  file.path(output_dir, "script15b_final_status.csv")
)

cat("\n============================================================\n")
cat("Script 15b completed: Automatic Prefill of Manual Item Review\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nReview burden summary:\n")
print(review_burden_summary)

cat("\nRole summary:\n")
print(role_summary)

cat("\nAutomatic prefill summary:\n")
print(auto_prefill_summary)

cat("\nConstruct summary:\n")
print(construct_summary)

cat("\nOutputs created:\n")
cat("- ", auto_prefill_path, "\n")
cat("- ", review_required_path, "\n")
cat("- ", completed_candidate_path, "\n")
cat("- ", file.path(output_dir, "script15b_auto_prefill_summary.csv"), "\n")
cat("- ", file.path(output_dir, "script15b_auto_prefill_role_summary.csv"), "\n")
cat("- ", file.path(output_dir, "script15b_auto_prefill_construct_summary.csv"), "\n")
cat("- ", file.path(output_dir, "script15b_review_burden_summary.csv"), "\n")
cat("- ", file.path(output_dir, "script15b_methodological_decisions.csv"), "\n")
cat("- ", file.path(output_dir, "script15b_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Review AUTO_PREFILL and REVIEW_REQUIRED files.\n")
cat("Only after confirmation, save the approved file as:\n")
cat("outputs/audits/script15_manual_item_direction_review_COMPLETED.csv\n")
cat("Then run Script 15 again to validate the completed review.\n")