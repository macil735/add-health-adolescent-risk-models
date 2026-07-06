# ============================================================
# Script 17b — Create Completed Manual Outcome Selection
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Create the completed manual outcome selection file required
#   before re-running Script 17.
#
# Methodological decision:
#   Select only true binary sexual health diagnosis outcomes
#   identified by Script 17a:
#
#   - H1CO16A: Ever diagnosed with chlamydia
#   - H1CO16C: Ever diagnosed with gonorrhea
#
# Event coding:
#   1 = diagnosed / event
#   0 = not diagnosed / non-event
#
# Main input:
#   outputs/audits/script17a_manual_outcome_selection_TEMPLATE.csv
#
# Main outputs:
#   outputs/audits/script17a_manual_outcome_selection_COMPLETED.csv
#   outputs/audits/script17b_selected_outcomes.csv
#   outputs/audits/script17b_manual_outcome_selection_validation.csv
#   docs/add_health_wave01_completed_manual_outcome_selection_script17b.docx
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "readr",
  "stringr"
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
# 1. Project root and folders
# ------------------------------------------------------------

project_root <- "C:/Users/LENOVO/GitHub/add-health-adolescent-risk-models"

if (!dir.exists(project_root)) {
  stop("Project root not found: ", project_root)
}

setwd(project_root)

audit_dir <- file.path(project_root, "outputs", "audits")
doc_dir <- file.path(project_root, "docs")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("Script 17b started: Completed Manual Outcome Selection\n")
cat("============================================================\n\n")
cat("Project root:\n", project_root, "\n\n")

# ------------------------------------------------------------
# 2. Input template
# ------------------------------------------------------------

template_path <- file.path(
  audit_dir,
  "script17a_manual_outcome_selection_TEMPLATE.csv"
)

if (!file.exists(template_path)) {
  stop(
    "Manual outcome selection template not found:\n",
    template_path,
    "\nRun Script 17a before Script 17b."
  )
}

template_data <- readr::read_csv(
  template_path,
  show_col_types = FALSE
)

cat("Manual outcome selection template loaded:\n")
cat(template_path, "\n\n")

required_columns <- c(
  "manual_select_outcome",
  "manual_use_in_script17",
  "manual_outcome_domain",
  "manual_event_code",
  "manual_non_event_code",
  "manual_outcome_label",
  "manual_decision_rationale",
  "manual_reviewer",
  "manual_review_date",
  "variable",
  "variable_label",
  "value_labels",
  "inferred_domain",
  "candidate_status",
  "valid_n",
  "distinct_valid_n",
  "valid_values"
)

missing_columns <- setdiff(required_columns, names(template_data))

if (length(missing_columns) > 0) {
  stop(
    "The template is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 3. Define manually selected outcomes
# ------------------------------------------------------------

selected_outcome_rules <- tibble::tribble(
  ~variable,  ~manual_outcome_domain,       ~manual_event_code, ~manual_non_event_code, ~manual_outcome_label,               ~manual_decision_rationale,
  "H1CO16A",  "sexual health diagnosis",    "1",                "0",                    "Ever diagnosed with chlamydia",    "Selected as a binary self-reported sexual health diagnosis outcome. Event code 1 indicates reported diagnosis; code 0 indicates no reported diagnosis.",
  "H1CO16C",  "sexual health diagnosis",    "1",                "0",                    "Ever diagnosed with gonorrhea",    "Selected as a binary self-reported sexual health diagnosis outcome. Event code 1 indicates reported diagnosis; code 0 indicates no reported diagnosis."
)

selected_variables <- selected_outcome_rules$variable

missing_selected_variables <- setdiff(selected_variables, template_data$variable)

if (length(missing_selected_variables) > 0) {
  stop(
    "The following selected outcome variables were not found in the Script 17a template: ",
    paste(missing_selected_variables, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 4. Create completed manual selection
# ------------------------------------------------------------

today_chr <- as.character(Sys.Date())

completed_selection <- template_data %>%
  mutate(
    variable = as.character(variable),
    manual_select_outcome = "no",
    manual_use_in_script17 = "no",
    manual_event_code = "",
    manual_non_event_code = "",
    manual_outcome_label = "",
    manual_reviewer = "AUTO_ASSISTED_MANUAL_SELECTION",
    manual_review_date = today_chr,
    manual_decision_rationale = case_when(
      variable %in% selected_variables ~ manual_decision_rationale,
      TRUE ~ "Not selected for Script 17. The variable is not retained as one of the final binary behavioral/event outcomes for this modelling step."
    )
  ) %>%
  left_join(
    selected_outcome_rules %>%
      rename(
        selected_manual_outcome_domain = manual_outcome_domain,
        selected_manual_event_code = manual_event_code,
        selected_manual_non_event_code = manual_non_event_code,
        selected_manual_outcome_label = manual_outcome_label,
        selected_manual_decision_rationale = manual_decision_rationale
      ),
    by = "variable"
  ) %>%
  mutate(
    manual_select_outcome = ifelse(variable %in% selected_variables, "yes", manual_select_outcome),
    manual_use_in_script17 = ifelse(variable %in% selected_variables, "yes", manual_use_in_script17),
    manual_outcome_domain = ifelse(
      variable %in% selected_variables,
      selected_manual_outcome_domain,
      manual_outcome_domain
    ),
    manual_event_code = ifelse(
      variable %in% selected_variables,
      selected_manual_event_code,
      manual_event_code
    ),
    manual_non_event_code = ifelse(
      variable %in% selected_variables,
      selected_manual_non_event_code,
      manual_non_event_code
    ),
    manual_outcome_label = ifelse(
      variable %in% selected_variables,
      selected_manual_outcome_label,
      manual_outcome_label
    ),
    manual_decision_rationale = ifelse(
      variable %in% selected_variables,
      selected_manual_decision_rationale,
      manual_decision_rationale
    )
  ) %>%
  select(
    -starts_with("selected_")
  )

# ------------------------------------------------------------
# 5. Validation
# ------------------------------------------------------------

selected_completed <- completed_selection %>%
  filter(manual_use_in_script17 == "yes")

validation <- tibble(
  validation_check = c(
    "completed_selection_has_rows",
    "selected_outcomes_present",
    "exactly_two_outcomes_selected",
    "all_selected_have_event_code",
    "all_selected_have_non_event_code",
    "all_selected_have_outcome_label",
    "selected_outcomes_are_binary",
    "selected_outcomes_have_valid_values_0_1"
  ),
  status = c(
    nrow(completed_selection) > 0,
    all(selected_variables %in% selected_completed$variable),
    nrow(selected_completed) == 2,
    all(!is.na(selected_completed$manual_event_code) & selected_completed$manual_event_code != ""),
    all(!is.na(selected_completed$manual_non_event_code) & selected_completed$manual_non_event_code != ""),
    all(!is.na(selected_completed$manual_outcome_label) & selected_completed$manual_outcome_label != ""),
    all(selected_completed$distinct_valid_n == 2),
    all(stringr::str_detect(selected_completed$valid_values, "0") &
          stringr::str_detect(selected_completed$valid_values, "1"))
  ),
  issue_if_false = c(
    "The completed selection file is empty.",
    "One or more intended outcome variables are missing from the completed selection.",
    "The completed selection should select exactly two outcomes in this step.",
    "Every selected outcome must have manual_event_code.",
    "Every selected outcome must have manual_non_event_code.",
    "Every selected outcome must have manual_outcome_label.",
    "Every selected outcome should be binary.",
    "Every selected outcome should have valid values 0 and 1."
  )
)

if (any(!validation$status)) {
  print(validation)
  stop("Manual outcome selection validation failed. Inspect validation table above.")
}

# ------------------------------------------------------------
# 6. Save outputs
# ------------------------------------------------------------

completed_path <- file.path(
  audit_dir,
  "script17a_manual_outcome_selection_COMPLETED.csv"
)

selected_outcomes_path <- file.path(
  audit_dir,
  "script17b_selected_outcomes.csv"
)

validation_path <- file.path(
  audit_dir,
  "script17b_manual_outcome_selection_validation.csv"
)

methodological_decisions_path <- file.path(
  audit_dir,
  "script17b_methodological_decisions.csv"
)

readr::write_csv(
  completed_selection,
  completed_path
)

readr::write_csv(
  selected_completed,
  selected_outcomes_path
)

readr::write_csv(
  validation,
  validation_path
)

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Outcome selection", "Script 17b selects H1CO16A and H1CO16C as final manually approved binary outcomes for Script 17.",
  "Outcome type", "Both selected variables are treated as self-reported sexual health diagnosis outcomes.",
  "Event coding", "Code 1 is treated as the event, indicating reported diagnosis. Code 0 is treated as the non-event.",
  "Excluded variables", "All other candidate variables from Script 17a remain unselected for this modelling step.",
  "Sexual initiation", "H1CO1 is not selected because the audited file showed only one valid value in the available analytic extract.",
  "Pregnancy variables", "Pregnancy and birth-control-related variables are not selected in this step because they were non-binary, low-valid-n, attitude/access measures, or not clean behavioral/event outcomes for the planned model.",
  "Interpretation", "Script 17 results should be interpreted as adjusted associations with self-reported diagnosis outcomes, not as causal estimates.",
  "Next step", "Revise and re-run Script 17 so that it uses script17a_manual_outcome_selection_COMPLETED.csv rather than automatic outcome detection."
)

readr::write_csv(
  methodological_decisions,
  methodological_decisions_path
)

# ------------------------------------------------------------
# 7. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_completed_manual_outcome_selection_script17b.docx"
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
      "Add Health Wave I — Completed Manual Outcome Selection",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 17b creates the completed manual outcome selection file used by Script 17. The selection is intentionally narrow and includes only binary self-reported sexual health diagnosis outcomes identified in Script 17a.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Selected outcomes", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(
        selected_completed %>%
          select(
            variable,
            variable_label,
            manual_outcome_label,
            manual_outcome_domain,
            manual_event_code,
            manual_non_event_code,
            valid_n,
            valid_values,
            value_distribution
          )
      )
    ) %>%
    officer::body_add_par("Validation", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(validation)
    ) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(
      value = make_ft(methodological_decisions)
    ) %>%
    officer::body_add_par("Required next action", style = "heading 2") %>%
    officer::body_add_par(
      "Re-run Script 17 after revising it to read outputs/audits/script17a_manual_outcome_selection_COMPLETED.csv. Do not rely on automatic outcome detection for final modelling.",
      style = "Normal"
    )

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 8. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "template_loaded",
    "selected_variables_found_in_template",
    "completed_manual_selection_created",
    "selected_outcomes_file_created",
    "validation_file_created",
    "all_validation_checks_passed",
    "word_report_created"
  ),
  status = c(
    file.exists(template_path),
    length(missing_selected_variables) == 0,
    file.exists(completed_path),
    file.exists(selected_outcomes_path),
    file.exists(validation_path),
    all(validation$status),
    !is.na(word_report_path) && file.exists(word_report_path)
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script17b_final_status.csv")
)

# ------------------------------------------------------------
# 9. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 17b completed: Completed Manual Outcome Selection\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nSelected outcomes:\n")
print(
  selected_completed %>%
    select(
      variable,
      variable_label,
      manual_outcome_label,
      manual_outcome_domain,
      manual_event_code,
      manual_non_event_code,
      valid_n,
      event_distribution = value_distribution
    )
)

cat("\nValidation:\n")
print(validation)

cat("\nOutputs created:\n")
cat("- ", completed_path, "\n")
cat("- ", selected_outcomes_path, "\n")
cat("- ", validation_path, "\n")
cat("- ", methodological_decisions_path, "\n")
cat("- ", file.path(audit_dir, "script17b_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Revise Script 17 to use the completed manual outcome selection file.\n")
cat("Do not commit respondent-level data or audit CSV outputs.\n")