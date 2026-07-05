# ============================================================
# Project: add-health-adolescent-risk-models
# Script 13: Final Technical Report or Manuscript-Style Document
# Author: Gelo Picol
#
# Purpose:
#   Create a final technical report / manuscript-style document
#   from Script 12 reporting outputs.
#
# Important:
#   - This script reads only public aggregate outputs.
#   - It does not read individual-level data.
#   - It does not export microdata.
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

script12_narrative_summary_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_outcome_narrative_summary.csv"
)

script12_robustness_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_main_strict_robustness_reporting.csv"
)

script12_caution_register_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_reporting_caution_register.csv"
)

script12_publication_readiness_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_publication_readiness_summary.csv"
)

script12_methodological_notes_path <- file.path(
  outputs_tables_dir,
  "script12_wave01_methodological_notes.csv"
)

script11_model_quality_path <- file.path(
  outputs_tables_dir,
  "script11_wave01_model_quality_review.csv"
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

collapse_text <- function(x) {
  x <- safe_character(x)
  x <- x[x != ""]
  if (length(x) == 0) {
    return("")
  }
  paste(x, collapse = " ")
}

sample_label <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "main_grade_10_12" ~ "Main sample: grades 10-12",
    x == "strict_grade_10_12_age_15_19" ~
      "Strict sensitivity sample: grades 10-12 and ages 15-19",
    TRUE ~ x
  )
}

clean_status <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "ready_for_cautious_reporting" ~ "Ready for cautious reporting",
    x == "report_only_after_manual_review" ~ "Manual review required",
    x == "not_ready_for_reporting" ~ "Not ready for reporting",
    TRUE ~ x
  )
}

clean_model_stage <- function(x) {
  x <- safe_character(x)

  dplyr::case_when(
    x == "M0_core_controls" ~ "M0: Core controls",
    x == "M1_family_context" ~ "M1: Family context",
    x == "M2_school_context" ~ "M2: School context",
    x == "M3_knowledge_attitudes" ~ "M3: Knowledge and attitudes",
    x == "M4_peers_relationships" ~ "M4: Peers and relationships",
    x == "M5_general_risk_behaviors" ~ "M5: General risk behaviors",
    x == "M6_final_parsimonious_model" ~ "M6: Final parsimonious model",
    TRUE ~ x
  )
}

safe_pct <- function(x) {
  x <- safe_numeric(x)

  dplyr::case_when(
    is.na(x) ~ "",
    TRUE ~ paste0(formatC(x, format = "f", digits = 2), "%")
  )
}

make_markdown_table <- function(df, max_rows = 30) {
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

add_docx_table <- function(doc, data, title, max_rows = 30) {
  doc <- officer::body_add_par(doc, title, style = "heading 3")

  if (nrow(data) == 0) {
    doc <- officer::body_add_par(doc, "No rows available.", style = "Normal")
    return(doc)
  }

  data_show <- data %>%
    head(max_rows) %>%
    mutate(across(everything(), safe_character))

  ft <- flextable::flextable(data_show)
  ft <- flextable::autofit(ft)
  ft <- flextable::fontsize(ft, size = 8, part = "all")
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")

  doc <- flextable::body_add_flextable(doc, value = ft)
  doc <- officer::body_add_par(doc, "", style = "Normal")

  doc
}

add_bullets <- function(doc, items) {
  items <- safe_character(items)
  items <- items[items != ""]

  for (item in items) {
    doc <- officer::body_add_par(
      doc,
      paste0("- ", item),
      style = "Normal"
    )
  }

  doc
}


# ============================================================
# 4. Check required inputs
# ============================================================

required_inputs <- c(
  script12_final_model_table_path,
  script12_final_coefficient_table_path,
  script12_main_coefficients_path,
  script12_appendix_coefficients_path,
  script12_narrative_summary_path,
  script12_robustness_path,
  script12_caution_register_path,
  script12_publication_readiness_path,
  script12_methodological_notes_path,
  script11_model_quality_path
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

final_model_table <- read_csv(
  script12_final_model_table_path,
  show_col_types = FALSE
)

final_coefficient_table <- read_csv(
  script12_final_coefficient_table_path,
  show_col_types = FALSE
)

main_coefficients <- read_csv(
  script12_main_coefficients_path,
  show_col_types = FALSE
)

appendix_coefficients <- read_csv(
  script12_appendix_coefficients_path,
  show_col_types = FALSE
)

narrative_summary <- read_csv(
  script12_narrative_summary_path,
  show_col_types = FALSE
)

robustness_table <- read_csv(
  script12_robustness_path,
  show_col_types = FALSE
)

caution_register <- read_csv(
  script12_caution_register_path,
  show_col_types = FALSE
)

publication_readiness <- read_csv(
  script12_publication_readiness_path,
  show_col_types = FALSE
)

script12_methodological_notes <- read_csv(
  script12_methodological_notes_path,
  show_col_types = FALSE
)

model_quality_review <- read_csv(
  script11_model_quality_path,
  show_col_types = FALSE
)


# ============================================================
# 6. Report-ready summary tables
# ============================================================

report_model_summary <- final_model_table %>%
  mutate(
    sample_label = sample_label(sample_name),
    model_label = clean_model_stage(model_stage),
    status_label = clean_status(final_model_reporting_status),
    weighted_pct_yes_label = safe_pct(weighted_pct_yes),
    suitability_score = safe_numeric(suitability_score)
  ) %>%
  select(
    sample = sample_label,
    outcome,
    outcome_label,
    selected_model = model_label,
    reporting_status = status_label,
    suitability_score,
    n_complete,
    n_outcome_yes,
    n_outcome_no,
    weighted_pct_yes = weighted_pct_yes_label,
    n_extreme_or,
    n_very_extreme_or,
    n_wide_ci,
    n_very_wide_ci,
    n_possible_conceptual_overlap
  ) %>%
  arrange(sample, outcome)

report_main_results <- main_coefficients %>%
  mutate(
    sample_label = sample_label(sample_name),
    selected_model_label = clean_model_stage(model_stage)
  ) %>%
  select(
    sample = sample_label,
    outcome,
    outcome_label,
    selected_model = selected_model_label,
    predictor = predictor_label,
    odds_ratio = odds_ratio_formatted,
    confidence_interval = confidence_interval_formatted,
    p_value = p_value_formatted,
    interpretation_status = coefficient_decision_label
  ) %>%
  arrange(sample, outcome, predictor)

report_appendix_results <- appendix_coefficients %>%
  mutate(
    sample_label = sample_label(sample_name),
    selected_model_label = clean_model_stage(model_stage)
  ) %>%
  select(
    sample = sample_label,
    outcome,
    outcome_label,
    selected_model = selected_model_label,
    predictor = predictor_label,
    odds_ratio = odds_ratio_formatted,
    confidence_interval = confidence_interval_formatted,
    p_value = p_value_formatted,
    review_status = coefficient_decision_label
  ) %>%
  arrange(sample, outcome, predictor)

report_robustness_summary <- robustness_table %>%
  select(
    outcome,
    outcome_label,
    model_stage_label,
    n_coefficients_compared,
    n_same_direction,
    share_same_direction,
    n_robust_significant_same_direction,
    n_partially_robust_same_direction,
    n_direction_changes_or_unstable,
    robustness_summary,
    robustness_reporting_note
  ) %>%
  arrange(outcome, model_stage_label)

report_caution_summary <- caution_register %>%
  count(
    sample_name,
    caution_level,
    caution_type,
    name = "n_cautions"
  ) %>%
  mutate(sample = sample_label(sample_name)) %>%
  select(
    sample,
    caution_level,
    caution_type,
    n_cautions
  ) %>%
  arrange(sample, caution_level, caution_type)

report_model_quality_summary <- model_quality_review %>%
  count(
    sample_name,
    final_selection_class,
    name = "n_models"
  ) %>%
  mutate(sample = sample_label(sample_name)) %>%
  select(
    sample,
    final_selection_class,
    n_models
  ) %>%
  arrange(sample, final_selection_class)

report_readiness_summary <- publication_readiness %>%
  select(
    item,
    value
  )

report_output_inventory <- tibble(
  output_type = c(
    "Markdown report",
    "Word report",
    "Excel workbook",
    "Model summary table",
    "Main results table",
    "Appendix review table",
    "Robustness summary table",
    "Caution summary table",
    "Execution checklist"
  ),
  path = c(
    "docs/add_health_wave01_final_technical_report_script13.md",
    "docs/add_health_wave01_final_technical_report_script13.docx",
    "outputs/tables/script13_wave01_final_report_tables.xlsx",
    "outputs/tables/script13_wave01_report_model_summary.csv",
    "outputs/tables/script13_wave01_report_main_results.csv",
    "outputs/tables/script13_wave01_report_appendix_results.csv",
    "outputs/tables/script13_wave01_report_robustness_summary.csv",
    "outputs/tables/script13_wave01_report_caution_summary.csv",
    "outputs/diagnostics/script13_execution_checklist.csv"
  ),
  public_safe = c(
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes"
  )
)


# ============================================================
# 7. Executive summary values
# ============================================================

n_final_models <- nrow(report_model_summary)

n_ready <- final_model_table %>%
  filter(final_model_reporting_status == "ready_for_cautious_reporting") %>%
  nrow()

n_manual_review <- final_model_table %>%
  filter(final_model_reporting_status == "report_only_after_manual_review") %>%
  nrow()

n_main_coefficients <- nrow(report_main_results)
n_appendix_coefficients <- nrow(report_appendix_results)
n_cautions <- nrow(caution_register)

dominant_model <- report_model_summary %>%
  count(selected_model, name = "n") %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(selected_model)

if (length(dominant_model) == 0) {
  dominant_model <- "Not available"
}

main_sample_narratives <- narrative_summary %>%
  filter(sample_name == "main_grade_10_12") %>%
  arrange(outcome) %>%
  pull(final_narrative)

strict_sample_narratives <- narrative_summary %>%
  filter(sample_name == "strict_grade_10_12_age_15_19") %>%
  arrange(outcome) %>%
  pull(final_narrative)

method_notes <- script12_methodological_notes %>%
  pull(note)


# ============================================================
# 8. Export CSV report tables
# ============================================================

write_csv(
  report_model_summary,
  file.path(outputs_tables_dir, "script13_wave01_report_model_summary.csv")
)

write_csv(
  report_main_results,
  file.path(outputs_tables_dir, "script13_wave01_report_main_results.csv")
)

write_csv(
  report_appendix_results,
  file.path(outputs_tables_dir, "script13_wave01_report_appendix_results.csv")
)

write_csv(
  report_robustness_summary,
  file.path(outputs_tables_dir, "script13_wave01_report_robustness_summary.csv")
)

write_csv(
  report_caution_summary,
  file.path(outputs_tables_dir, "script13_wave01_report_caution_summary.csv")
)

write_csv(
  report_model_quality_summary,
  file.path(outputs_tables_dir, "script13_wave01_report_model_quality_summary.csv")
)

write_csv(
  report_readiness_summary,
  file.path(outputs_tables_dir, "script13_wave01_report_readiness_summary.csv")
)

write_csv(
  report_output_inventory,
  file.path(outputs_tables_dir, "script13_wave01_output_inventory.csv")
)


# ============================================================
# 9. Markdown final technical report
# ============================================================

markdown_report_path <- file.path(
  docs_dir,
  "add_health_wave01_final_technical_report_script13.md"
)

markdown_report <- c(
  "# Final Technical Report",
  "",
  "## Add Health Wave I Public-Use Adolescent Risk Behavior Analysis",
  "",
  "### Executive summary",
  "",
  paste0(
    "This report summarizes the final public-use analytical outputs from the ",
    "Add Health Wave I adolescent risk behavior project. The report is based only ",
    "on aggregate public outputs produced by the previous scripts. No individual-level ",
    "data are read or exported by this script."
  ),
  "",
  paste0("- Final reporting models created: ", n_final_models, "."),
  paste0("- Outcomes ready for cautious reporting: ", n_ready, "."),
  paste0("- Outcomes requiring manual review: ", n_manual_review, "."),
  paste0("- Main reportable coefficients: ", n_main_coefficients, "."),
  paste0("- Appendix or review coefficients: ", n_appendix_coefficients, "."),
  paste0("- Reporting cautions recorded: ", n_cautions, "."),
  paste0("- Dominant selected model: ", dominant_model, "."),
  "",
  "The report should be interpreted as an associational analysis. It does not provide causal evidence.",
  "",
  "## 1. Purpose and scope",
  "",
  paste0(
    "The purpose of this report is to document the final analytical workflow, ",
    "selected models, reporting tables, robustness checks and limitations of the ",
    "Add Health Wave I public-use adolescent risk behavior analysis."
  ),
  "",
  "The analysis is designed as a reproducible public-use replication framework inspired by the structure of the doctoral research project, without publishing original thesis microdata.",
  "",
  "## 2. Data protection and reproducibility",
  "",
  "The project separates public reproducible code and aggregate outputs from restricted or individual-level data. Raw data, processed individual-level files and AID-level outputs are not exported to the public repository.",
  "",
  "## 3. Analytical workflow",
  "",
  "The project proceeded through documentation review, variable mapping, public-use data import, analytical recoding, weighted descriptive statistics, missing-data review, bivariate screening, modeling framework construction, weighted logistic regression, model review, final model selection and reporting table preparation.",
  "",
  "## 4. Modeling strategy",
  "",
  "The weighted logistic regression models were estimated using the Wave I population-average sampling weight. The main analytical sample includes students in grades 10 to 12. A strict sensitivity sample additionally restricts the analysis to ages 15 to 19.",
  "",
  "The final reporting stage prioritizes technically stable and interpretable models. Results are reported cautiously, with explicit separation between main reporting coefficients and coefficients requiring manual review.",
  "",
  "## 5. Final model summary",
  "",
  make_markdown_table(report_model_summary, max_rows = 40),
  "",
  "## 6. Main reportable coefficients",
  "",
  make_markdown_table(report_main_results, max_rows = 40),
  "",
  "## 7. Appendix or review coefficients",
  "",
  make_markdown_table(report_appendix_results, max_rows = 40),
  "",
  "## 8. Main sample narrative",
  "",
  paste0(
    "### Outcome narrative ",
    seq_along(main_sample_narratives),
    "\n\n",
    main_sample_narratives,
    "\n"
  ),
  "",
  "## 9. Strict sensitivity sample narrative",
  "",
  paste0(
    "### Sensitivity narrative ",
    seq_along(strict_sample_narratives),
    "\n\n",
    strict_sample_narratives,
    "\n"
  ),
  "",
  "## 10. Robustness summary",
  "",
  make_markdown_table(report_robustness_summary, max_rows = 40),
  "",
  "## 11. Reporting cautions",
  "",
  make_markdown_table(report_caution_summary, max_rows = 40),
  "",
  "## 12. Methodological limitations",
  "",
  "- The analysis is associational and not causal.",
  "- The public-use data structure limits the level of detail available for some constructs.",
  "- Some richer models were not selected for final reporting because of numerical instability, extreme odds ratios or wide confidence intervals.",
  "- Models based on rare outcomes require careful interpretation.",
  "- The final report prioritizes conservative and technically defensible reporting over exhaustive model complexity.",
  "",
  "## 13. Conclusion",
  "",
  paste0(
    "The final reporting results support a cautious descriptive interpretation. ",
    "The most defensible final models are conservative core-control models. ",
    "They provide adjusted associations for key demographic controls, while richer ",
    "substantive models remain exploratory or require additional manual review."
  ),
  "",
  "## 14. Output inventory",
  "",
  make_markdown_table(report_output_inventory, max_rows = 50)
)

writeLines(markdown_report, con = markdown_report_path)


# ============================================================
# 10. Word final technical report
# ============================================================

docx_report_path <- file.path(
  docs_dir,
  "add_health_wave01_final_technical_report_script13.docx"
)

doc <- officer::read_docx()

doc <- officer::body_add_par(
  doc,
  "Final Technical Report",
  style = "heading 1"
)

doc <- officer::body_add_par(
  doc,
  "Add Health Wave I Public-Use Adolescent Risk Behavior Analysis",
  style = "heading 2"
)

doc <- officer::body_add_par(
  doc,
  paste0(
    "Prepared as part of a reproducible public-use analytical project. ",
    "This document is generated from aggregate public outputs only."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Executive summary", style = "heading 1")

executive_items <- c(
  paste0("Final reporting models created: ", n_final_models, "."),
  paste0("Outcomes ready for cautious reporting: ", n_ready, "."),
  paste0("Outcomes requiring manual review: ", n_manual_review, "."),
  paste0("Main reportable coefficients: ", n_main_coefficients, "."),
  paste0("Appendix or review coefficients: ", n_appendix_coefficients, "."),
  paste0("Reporting cautions recorded: ", n_cautions, "."),
  paste0("Dominant selected model: ", dominant_model, "."),
  "The estimates are associational and should not be interpreted as causal effects.",
  "No individual-level data are read or exported by Script 13."
)

doc <- add_bullets(doc, executive_items)

doc <- officer::body_add_par(doc, "1. Purpose and scope", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "This report documents the final analytical workflow, selected models, ",
    "reporting tables, robustness checks and limitations of the Add Health Wave I ",
    "public-use adolescent risk behavior analysis."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(
  doc,
  paste0(
    "The analysis is intended as a reproducible public-use replication framework. ",
    "It does not publish original thesis microdata or individual-level Add Health data."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "2. Data protection and reproducibility", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "The project separates public reproducible scripts and aggregate outputs ",
    "from restricted, raw or individual-level data. Files in data/raw, data/processed ",
    "and other private review locations are not part of the public reporting output."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "3. Analytical workflow", style = "heading 1")
workflow_items <- c(
  "Documentation and feasibility audit.",
  "Thesis questionnaire construct inventory.",
  "Add Health documentation and codebook review.",
  "Variable mapping and import planning.",
  "Public-use Wave I variable import.",
  "Analytical recoding and sample definition.",
  "Weighted descriptive statistics.",
  "Missing-data and recode review.",
  "Weighted bivariate association analysis.",
  "Modeling framework construction.",
  "Weighted logistic regression modeling.",
  "Model review and final model selection.",
  "Final reporting tables and narrative interpretation.",
  "Final technical report generation."
)

doc <- add_bullets(doc, workflow_items)

doc <- officer::body_add_par(doc, "4. Modeling strategy", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "The final models are selected from weighted logistic regression outputs. ",
    "The main sample includes students in grades 10 to 12. The strict sensitivity ",
    "sample additionally restricts observations to ages 15 to 19. The reporting ",
    "stage favors technically stable and conservative models."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "5. Final model summary", style = "heading 1")
doc <- add_docx_table(
  doc,
  report_model_summary,
  "Table 1. Final reporting model summary",
  max_rows = 40
)

doc <- officer::body_add_par(doc, "6. Main reportable coefficients", style = "heading 1")
doc <- add_docx_table(
  doc,
  report_main_results,
  "Table 2. Main reportable odds ratios",
  max_rows = 40
)

doc <- officer::body_add_par(doc, "7. Appendix or review coefficients", style = "heading 1")
doc <- add_docx_table(
  doc,
  report_appendix_results,
  "Table 3. Coefficients requiring appendix placement or manual review",
  max_rows = 40
)

doc <- officer::body_add_par(doc, "8. Narrative results", style = "heading 1")
doc <- officer::body_add_par(doc, "8.1 Main sample", style = "heading 2")

for (i in seq_along(main_sample_narratives)) {
  doc <- officer::body_add_par(
    doc,
    paste0("Outcome narrative ", i),
    style = "heading 3"
  )
  doc <- officer::body_add_par(
    doc,
    main_sample_narratives[[i]],
    style = "Normal"
  )
}

doc <- officer::body_add_par(doc, "8.2 Strict sensitivity sample", style = "heading 2")

for (i in seq_along(strict_sample_narratives)) {
  doc <- officer::body_add_par(
    doc,
    paste0("Sensitivity narrative ", i),
    style = "heading 3"
  )
  doc <- officer::body_add_par(
    doc,
    strict_sample_narratives[[i]],
    style = "Normal"
  )
}

doc <- officer::body_add_par(doc, "9. Robustness summary", style = "heading 1")
doc <- add_docx_table(
  doc,
  report_robustness_summary,
  "Table 4. Main-versus-strict sample robustness",
  max_rows = 40
)

doc <- officer::body_add_par(doc, "10. Reporting cautions", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "The caution register records model-level and coefficient-level issues ",
    "that require attention before substantive interpretation."
  ),
  style = "Normal"
)

doc <- add_docx_table(
  doc,
  report_caution_summary,
  "Table 5. Reporting caution summary",
  max_rows = 40
)

doc <- officer::body_add_par(doc, "11. Methodological limitations", style = "heading 1")
limitation_items <- c(
  "The estimates are associational and not causal.",
  "Some constructs are limited by the public-use variable structure.",
  "Richer models were not selected when they showed instability, extreme odds ratios or wide confidence intervals.",
  "Rare outcomes require cautious interpretation.",
  "The final report prioritizes conservative and technically defensible reporting."
)

doc <- add_bullets(doc, limitation_items)

doc <- officer::body_add_par(doc, "12. Conclusion", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "The final reporting results support cautious descriptive interpretation. ",
    "The most defensible final models are conservative core-control models. ",
    "They provide adjusted associations for key demographic controls, while richer ",
    "substantive models remain exploratory or require additional manual review."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "13. Output inventory", style = "heading 1")
doc <- add_docx_table(
  doc,
  report_output_inventory,
  "Table 6. Script 13 public output inventory",
  max_rows = 50
)

doc <- officer::body_add_par(doc, "14. Methodological notes", style = "heading 1")
doc <- add_bullets(doc, method_notes)

print(doc, target = docx_report_path)


# ============================================================
# 11. Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script13_wave01_final_report_tables.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "model_summary")
writeData(wb, "model_summary", report_model_summary)

addWorksheet(wb, "main_results")
writeData(wb, "main_results", report_main_results)

addWorksheet(wb, "appendix_results")
writeData(wb, "appendix_results", report_appendix_results)

addWorksheet(wb, "robustness")
writeData(wb, "robustness", report_robustness_summary)

addWorksheet(wb, "caution_summary")
writeData(wb, "caution_summary", report_caution_summary)

addWorksheet(wb, "model_quality")
writeData(wb, "model_quality", report_model_quality_summary)

addWorksheet(wb, "readiness")
writeData(wb, "readiness", report_readiness_summary)

addWorksheet(wb, "output_inventory")
writeData(wb, "output_inventory", report_output_inventory)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:100, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)


# ============================================================
# 12. Methodological notes and execution checklist
# ============================================================

script13_methodological_notes <- tibble(
  note_id = 1:14,
  note = c(
    "Script 13 creates a final technical report from aggregate public outputs.",
    "The script reads only Script 12 and Script 11 public outputs.",
    "No individual-level data are read by this script.",
    "No individual-level data are exported by this script.",
    "The final report is produced in Markdown and Word formats.",
    "The Word report is generated using officer and flextable.",
    "The report prioritizes conservative and technically defensible interpretation.",
    "M0 core-control models dominate the final reporting selection.",
    "Richer models are retained only where technically defensible.",
    "The narrative remains associational and not causal.",
    "Caution flags are retained for manual review.",
    "The outputs are suitable for internal review and manuscript drafting.",
    "The report should be reviewed manually before public release.",
    "Script 14 may be used for final repository publication audit."
  )
)

write_csv(
  script13_methodological_notes,
  file.path(outputs_tables_dir, "script13_wave01_methodological_notes.csv")
)

script13_checklist <- tibble(
  check_id = 1:24,
  check_item = c(
    "Project root exists",
    "Outputs tables directory exists",
    "Outputs diagnostics directory exists",
    "Docs directory exists",
    "Script 12 final model table input exists",
    "Script 12 final coefficient table input exists",
    "Script 12 main coefficients input exists",
    "Script 12 appendix coefficients input exists",
    "Script 12 narrative summary input exists",
    "Script 12 robustness input exists",
    "Script 12 caution register input exists",
    "Script 12 publication readiness input exists",
    "Script 12 methodological notes input exists",
    "Script 11 model quality input exists",
    "Report model summary created",
    "Report main results created",
    "Report appendix results created",
    "Report robustness summary created",
    "Report caution summary created",
    "Markdown report exported",
    "Word report exported",
    "Excel workbook exported",
    "Output inventory exported",
    "No individual-level output exported"
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
    ifelse(file.exists(script12_narrative_summary_path), "OK", "FAIL"),
    ifelse(file.exists(script12_robustness_path), "OK", "FAIL"),
    ifelse(file.exists(script12_caution_register_path), "OK", "FAIL"),
    ifelse(file.exists(script12_publication_readiness_path), "OK", "FAIL"),
    ifelse(file.exists(script12_methodological_notes_path), "OK", "FAIL"),
    ifelse(file.exists(script11_model_quality_path), "OK", "FAIL"),
    ifelse(nrow(report_model_summary) > 0, "OK", "FAIL"),
    ifelse(nrow(report_main_results) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(report_appendix_results) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(report_robustness_summary) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(report_caution_summary) > 0, "OK", "WARNING_NONE"),
    ifelse(file.exists(markdown_report_path), "OK", "FAIL"),
    ifelse(file.exists(docx_report_path), "OK", "FAIL"),
    ifelse(file.exists(xlsx_path), "OK", "FAIL"),
    ifelse(nrow(report_output_inventory) > 0, "OK", "FAIL"),
    "OK"
  )
)

write_csv(
  script13_checklist,
  file.path(outputs_diag_dir, "script13_execution_checklist.csv")
)


# ============================================================
# 13. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 13 completed: Final Technical Report / Manuscript-Style Document\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Final report summary:\n")
cat("- Final reporting models: ", n_final_models, "\n", sep = "")
cat("- Outcomes ready for cautious reporting: ", n_ready, "\n", sep = "")
cat("- Outcomes requiring manual review: ", n_manual_review, "\n", sep = "")
cat("- Main reportable coefficients: ", n_main_coefficients, "\n", sep = "")
cat("- Appendix/review coefficients: ", n_appendix_coefficients, "\n", sep = "")
cat("- Reporting cautions: ", n_cautions, "\n", sep = "")
cat("- Dominant selected model: ", dominant_model, "\n\n", sep = "")

cat("Report model quality summary:\n")
print(report_model_quality_summary)

cat("\nReport readiness summary:\n")
print(report_readiness_summary)

cat("\nMain results preview:\n")
print(
  report_main_results %>%
    head(30)
)

cat("\nCaution summary:\n")
print(report_caution_summary)

cat("\nPublic outputs created:\n")
cat("- docs/add_health_wave01_final_technical_report_script13.md\n")
cat("- docs/add_health_wave01_final_technical_report_script13.docx\n")
cat("- outputs/tables/script13_wave01_final_report_tables.xlsx\n")
cat("- outputs/tables/script13_wave01_report_model_summary.csv\n")
cat("- outputs/tables/script13_wave01_report_main_results.csv\n")
cat("- outputs/tables/script13_wave01_report_appendix_results.csv\n")
cat("- outputs/tables/script13_wave01_report_robustness_summary.csv\n")
cat("- outputs/tables/script13_wave01_report_caution_summary.csv\n")
cat("- outputs/tables/script13_wave01_report_model_quality_summary.csv\n")
cat("- outputs/tables/script13_wave01_report_readiness_summary.csv\n")
cat("- outputs/tables/script13_wave01_output_inventory.csv\n")
cat("- outputs/tables/script13_wave01_methodological_notes.csv\n")
cat("- outputs/diagnostics/script13_execution_checklist.csv\n\n")

cat("Execution checklist:\n")
print(script13_checklist)

cat("\nImportant note:\n")
cat("Script 13 generated final report files from aggregate public outputs only.\n")
cat("Manual review is still required before any public release or manuscript submission.\n")
cat("No individual-level data were used or exported by this script.\n\n")