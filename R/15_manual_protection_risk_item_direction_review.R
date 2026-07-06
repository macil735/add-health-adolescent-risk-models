# ============================================================
# Script 15 — Manual Review of Protection/Risk Item Direction
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Prepare a manual review table for candidate protection/risk
#   items detected in Script 14.
#
#   This script does not finalize indices. It creates a structured
#   review file to decide item by item whether each variable should
#   be classified as:
#     - protection
#     - risk
#     - ambiguous_review
#     - exclude
#
#   It also prepares fields for coding direction and reverse scoring.
#
# Required prior output:
#   outputs/audits/script14_index_item_classification.csv
#
# Main outputs:
#   outputs/audits/script15_manual_item_direction_review_template.csv
#   outputs/audits/script15_manual_review_summary.csv
#   docs/add_health_wave01_manual_item_direction_review_script15.docx
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
cat("Script 15 started: Manual Protection/Risk Item Direction Review\n")
cat("============================================================\n\n")
cat("Project root:\n", project_root, "\n\n")

# ------------------------------------------------------------
# 2. Input files from Script 14
# ------------------------------------------------------------

script14_classification_path <- file.path(
  output_dir,
  "script14_index_item_classification.csv"
)

script14_section_candidates_path <- file.path(
  output_dir,
  "script14_section_variable_candidates.csv"
)

script14_final_status_path <- file.path(
  output_dir,
  "script14_final_status.csv"
)

if (!file.exists(script14_classification_path)) {
  stop(
    "Required file not found: ",
    script14_classification_path,
    "\nRun Script 14 before Script 15."
  )
}

item_classification <- readr::read_csv(
  script14_classification_path,
  show_col_types = FALSE
)

section_candidates <- if (file.exists(script14_section_candidates_path)) {
  readr::read_csv(script14_section_candidates_path, show_col_types = FALSE)
} else {
  tibble()
}

script14_status <- if (file.exists(script14_final_status_path)) {
  readr::read_csv(script14_final_status_path, show_col_types = FALSE)
} else {
  tibble()
}

cat("Script 14 classification file loaded:\n")
cat(script14_classification_path, "\n\n")

# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_squish(x)
  x
}

empty_if_missing <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

detect_keyword <- function(text, pattern) {
  stringr::str_detect(
    stringr::str_to_lower(empty_if_missing(text)),
    pattern
  )
}

# ------------------------------------------------------------
# 4. Conceptual keyword rules for review support
# ------------------------------------------------------------

protective_concepts <- paste(
  c(
    "close",
    "care",
    "support",
    "warm",
    "talk",
    "connected",
    "belong",
    "safe",
    "happy",
    "confidence",
    "esteem",
    "future",
    "expect",
    "college",
    "graduate",
    "relig",
    "church",
    "pray",
    "parent",
    "teacher",
    "supervision",
    "monitor",
    "aspiration",
    "plan"
  ),
  collapse = "|"
)

risk_concepts <- paste(
  c(
    "risk",
    "trouble",
    "fight",
    "weapon",
    "unsafe",
    "sad",
    "depress",
    "lonely",
    "impulsive",
    "temper",
    "anger",
    "problem",
    "smoke",
    "drink",
    "drunk",
    "drug",
    "marijuana",
    "gang",
    "suspend",
    "skip school",
    "delinquen"
  ),
  collapse = "|"
)

ambiguous_concepts <- paste(
  c(
    "other",
    "not applicable",
    "refused",
    "don't know",
    "dont know",
    "skip",
    "unknown",
    "missing"
  ),
  collapse = "|"
)

# ------------------------------------------------------------
# 5. Build manual review template
# ------------------------------------------------------------

required_columns <- c(
  "section_id",
  "section_name",
  "variable",
  "label",
  "value_labels",
  "direction_class",
  "classification_status"
)

missing_required_columns <- setdiff(required_columns, names(item_classification))

if (length(missing_required_columns) > 0) {
  stop(
    "The Script 14 classification file is missing required columns: ",
    paste(missing_required_columns, collapse = ", ")
  )
}

manual_review_template <- item_classification %>%
  mutate(
    item_text = clean_text(
      paste(variable, label, value_labels, sep = " | ")
    ),

    protective_keyword_support = detect_keyword(item_text, protective_concepts),
    risk_keyword_support = detect_keyword(item_text, risk_concepts),
    ambiguous_keyword_support = detect_keyword(item_text, ambiguous_concepts),

    suggested_final_role = case_when(
      direction_class == "potential_protection" ~ "protection",
      direction_class == "potential_risk" ~ "risk",
      direction_class == "ambiguous_review" ~ "ambiguous_review",
      direction_class == "unclassified_review" ~ "manual_review",
      TRUE ~ "manual_review"
    ),

    suggested_score_direction = case_when(
      suggested_final_role == "protection" ~ "higher_is_more_protective",
      suggested_final_role == "risk" ~ "higher_is_more_risky",
      TRUE ~ "requires_manual_review"
    ),

    suggested_include_in_index = case_when(
      suggested_final_role %in% c("protection", "risk") ~ "yes_after_review",
      TRUE ~ "no_until_reviewed"
    ),

    manual_final_role = "",
    manual_score_direction = "",
    manual_reverse_score = "",
    manual_include_in_index = "",
    manual_construct_label = "",
    manual_decision_rationale = "",
    manual_reviewer = "",
    manual_review_date = "",

    review_priority = case_when(
      direction_class %in% c("unclassified_review", "ambiguous_review") ~ "high",
      suggested_final_role == "risk" ~ "high",
      suggested_final_role == "protection" ~ "medium",
      TRUE ~ "high"
    ),

    review_instruction = case_when(
      direction_class == "potential_protection" ~
        "Check whether the item wording and coding confirm that higher values represent stronger protection. If not, set reverse_score = yes or exclude.",
      direction_class == "potential_risk" ~
        "Check whether the item wording and coding confirm that higher values represent higher risk. If not, set reverse_score = yes or exclude.",
      direction_class == "ambiguous_review" ~
        "Review manually. The item contains mixed protection/risk signals or unclear wording.",
      direction_class == "unclassified_review" ~
        "Review manually. The keyword classifier could not assign a protection/risk direction.",
      TRUE ~
        "Review manually."
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
    protective_keyword_support,
    risk_keyword_support,
    ambiguous_keyword_support,
    suggested_final_role,
    suggested_score_direction,
    suggested_include_in_index,
    review_priority,
    review_instruction,
    manual_final_role,
    manual_score_direction,
    manual_reverse_score,
    manual_include_in_index,
    manual_construct_label,
    manual_decision_rationale,
    manual_reviewer,
    manual_review_date,
    detection_source,
    file_name,
    object_name
  ) %>%
  arrange(
    desc(review_priority == "high"),
    section_id,
    direction_class,
    variable
  )

template_path <- file.path(
  output_dir,
  "script15_manual_item_direction_review_template.csv"
)

readr::write_csv(
  manual_review_template,
  template_path
)

# ------------------------------------------------------------
# 6. Review summary
# ------------------------------------------------------------

manual_review_summary <- manual_review_template %>%
  count(section_id, section_name, direction_class, suggested_final_role, review_priority, name = "items") %>%
  arrange(section_id, direction_class, suggested_final_role)

section_summary <- manual_review_template %>%
  count(section_id, section_name, name = "items_total") %>%
  left_join(
    manual_review_template %>%
      filter(review_priority == "high") %>%
      count(section_id, name = "high_priority_items"),
    by = "section_id"
  ) %>%
  mutate(
    high_priority_items = if_else(is.na(high_priority_items), 0L, high_priority_items)
  ) %>%
  arrange(section_id)

decision_fields <- tibble::tribble(
  ~field, ~allowed_values, ~meaning,
  "manual_final_role", "protection; risk; ambiguous_review; exclude", "Final theoretical classification of the item.",
  "manual_score_direction", "higher_is_more_protective; higher_is_more_risky; lower_is_more_protective; lower_is_more_risky; not_applicable", "Direction of the raw coding after reviewing item wording and value labels.",
  "manual_reverse_score", "yes; no; not_applicable", "Whether the item must be reversed before entering the index.",
  "manual_include_in_index", "yes; no", "Whether the item should enter the final protection/risk index.",
  "manual_construct_label", "short text", "Substantive construct name, for example school connectedness, parental warmth or depressive affect.",
  "manual_decision_rationale", "short text", "Brief justification for the manual decision.",
  "manual_reviewer", "name or initials", "Person responsible for the decision.",
  "manual_review_date", "YYYY-MM-DD", "Date of manual review."
)

readr::write_csv(
  manual_review_summary,
  file.path(output_dir, "script15_manual_review_summary.csv")
)

readr::write_csv(
  section_summary,
  file.path(output_dir, "script15_section_review_summary.csv")
)

readr::write_csv(
  decision_fields,
  file.path(output_dir, "script15_manual_decision_field_guide.csv")
)

# ------------------------------------------------------------
# 7. Create a working copy for manual editing
# ------------------------------------------------------------

manual_working_copy_path <- file.path(
  output_dir,
  "script15_manual_item_direction_review_WORKING_COPY.csv"
)

if (!file.exists(manual_working_copy_path)) {
  readr::write_csv(
    manual_review_template,
    manual_working_copy_path
  )
}

# ------------------------------------------------------------
# 8. Optional validation of completed manual review file
# ------------------------------------------------------------

completed_review_path <- file.path(
  output_dir,
  "script15_manual_item_direction_review_COMPLETED.csv"
)

if (file.exists(completed_review_path)) {

  completed_review <- readr::read_csv(
    completed_review_path,
    show_col_types = FALSE
  )

  required_manual_fields <- c(
    "manual_final_role",
    "manual_score_direction",
    "manual_reverse_score",
    "manual_include_in_index"
  )

  missing_manual_fields <- setdiff(required_manual_fields, names(completed_review))

  if (length(missing_manual_fields) > 0) {
    completed_validation <- tibble(
      validation_check = "completed_review_has_required_fields",
      status = FALSE,
      issue = paste(
        "Missing fields:",
        paste(missing_manual_fields, collapse = ", ")
      )
    )
  } else {

    completed_validation <- tibble(
      validation_check = c(
        "completed_review_file_exists",
        "all_items_have_manual_final_role",
        "all_included_items_have_score_direction",
        "all_included_items_have_reverse_score_decision",
        "all_items_have_include_decision"
      ),
      status = c(
        TRUE,
        all(completed_review$manual_final_role != "" & !is.na(completed_review$manual_final_role)),
        all(
          completed_review$manual_include_in_index != "yes" |
            (
              completed_review$manual_score_direction != "" &
                !is.na(completed_review$manual_score_direction)
            )
        ),
        all(
          completed_review$manual_include_in_index != "yes" |
            (
              completed_review$manual_reverse_score != "" &
                !is.na(completed_review$manual_reverse_score)
            )
        ),
        all(completed_review$manual_include_in_index != "" & !is.na(completed_review$manual_include_in_index))
      ),
      issue = c(
        "",
        "Every item must have manual_final_role.",
        "Included items must have manual_score_direction.",
        "Included items must have manual_reverse_score decision.",
        "Every item must have manual_include_in_index."
      )
    )
  }

} else {

  completed_validation <- tibble(
    validation_check = c(
      "completed_review_file_exists",
      "manual_review_required_before_final_indices"
    ),
    status = c(FALSE, TRUE),
    issue = c(
      "No completed manual review file found yet.",
      "Use the WORKING_COPY file, fill the manual fields, then save a completed version as script15_manual_item_direction_review_COMPLETED.csv."
    )
  )
}

readr::write_csv(
  completed_validation,
  file.path(output_dir, "script15_completed_review_validation.csv")
)

# ------------------------------------------------------------
# 9. Methodological decisions
# ------------------------------------------------------------

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Purpose", "Script 15 does not estimate final indices; it prepares the item-level manual review needed before final index construction.",
  "Manual review", "Every candidate item from Sections 8, 9, 17, 18, 19 and 20 must be reviewed for theoretical role, coding direction, reverse scoring and inclusion.",
  "Protection role", "Items should be classified as protection only if their wording and coding support interpretation as a protective factor.",
  "Risk role", "Items should be classified as risk only if their wording and coding support interpretation as a risk factor.",
  "Ambiguous items", "Ambiguous items should remain outside the final index until theoretical justification is clear.",
  "Exclusion", "Items with unclear meaning, poor coding support, weak construct fit or limited coverage should be excluded from final indices.",
  "Next step", "After manual review is completed, Script 16 should construct final reviewed protection and risk indices."
)

readr::write_csv(
  methodological_decisions,
  file.path(output_dir, "script15_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 10. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_manual_item_direction_review_script15.docx"
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
      "Add Health Wave I — Manual Protection/Risk Item Direction Review",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 15 prepares the manual item-level review required before constructing final protection and risk indices. The review table covers candidate items from Sections 8, 9, 17, 18, 19 and 20.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Section review summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(section_summary)
    ) %>%
    officer::body_add_par("Manual review summary", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(manual_review_summary)
    ) %>%
    officer::body_add_par("Manual decision field guide", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(decision_fields)
    ) %>%
    officer::body_add_par("Completed review validation", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(completed_validation)
    ) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(methodological_decisions)
    ) %>%
    officer::body_add_par("Required manual action", style = "heading 2") %>%
    officer::body_add_par(
      paste0(
        "Open the working copy file, complete the manual fields and save the completed review as: ",
        "outputs/audits/script15_manual_item_direction_review_COMPLETED.csv. ",
        "Final protection and risk indices should not be constructed until this completed review file passes validation."
      ),
      style = "Normal"
    )

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 11. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "script14_classification_loaded",
    "manual_review_template_created",
    "working_copy_created",
    "manual_review_summary_created",
    "decision_field_guide_created",
    "completed_review_validation_created",
    "word_report_created",
    "manual_review_still_required"
  ),
  status = c(
    nrow(item_classification) > 0,
    file.exists(template_path),
    file.exists(manual_working_copy_path),
    file.exists(file.path(output_dir, "script15_manual_review_summary.csv")),
    file.exists(file.path(output_dir, "script15_manual_decision_field_guide.csv")),
    file.exists(file.path(output_dir, "script15_completed_review_validation.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    !file.exists(completed_review_path)
  )
)

readr::write_csv(
  final_status,
  file.path(output_dir, "script15_final_status.csv")
)

cat("\n============================================================\n")
cat("Script 15 completed: Manual Protection/Risk Item Direction Review\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nSection review summary:\n")
print(section_summary)

cat("\nManual review summary:\n")
print(manual_review_summary)

cat("\nCompleted review validation:\n")
print(completed_validation)

cat("\nOutputs created:\n")
cat("- ", template_path, "\n")
cat("- ", manual_working_copy_path, "\n")
cat("- ", file.path(output_dir, "script15_manual_review_summary.csv"), "\n")
cat("- ", file.path(output_dir, "script15_section_review_summary.csv"), "\n")
cat("- ", file.path(output_dir, "script15_manual_decision_field_guide.csv"), "\n")
cat("- ", file.path(output_dir, "script15_completed_review_validation.csv"), "\n")
cat("- ", file.path(output_dir, "script15_methodological_decisions.csv"), "\n")
cat("- ", file.path(output_dir, "script15_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Open outputs/audits/script15_manual_item_direction_review_WORKING_COPY.csv,\n")
cat("complete the manual review fields, and save it as:\n")
cat("outputs/audits/script15_manual_item_direction_review_COMPLETED.csv\n")
cat("before constructing final indices in Script 16.\n")