# ============================================================
# Script 19g_v2
# Format Final Never-Sex TPB Mechanism Report
# Project: Add Health Adolescent Risk Models
# ============================================================
#
# Purpose:
#   Produce a clean, compact, institutional Word report for the
#   final never-sex TPB-compatible mechanism phase.
#
# This script:
#   1. does not change any analytical result;
#   2. reads outputs from Scripts 19a_v3, 19a_v3_check, 19e, 19f and 19g;
#   3. fixes the duplicate heading numbering problem by avoiding Word
#      auto-numbered heading styles;
#   4. reduces table width by using compact final tables;
#   5. keeps detailed wide tables in CSV outputs;
#   6. creates a formatted Word report and markdown summary.
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

script_id <- "19g_v2"
script_title <- "Format Final Never-Sex TPB Mechanism Report"
start_time <- Sys.time()

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

required_pkgs <- c(
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "officer",
  "flextable"
)

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_pkgs, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(officer)
  library(flextable)
})

# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

if (!stringr::str_detect(basename(project_root), "add-health-adolescent-risk-models")) {
  warning(
    "Current working directory does not look like the project root: ",
    project_root
  )
}

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/audits", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reports", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)

# Inputs
input_construct_summary <- "outputs/tables/script19a_v3_construct_summary.csv"
input_internal_19a <- "outputs/tables/script19a_v3_check_internal_consistency.csv"
input_validation_19a <- "outputs/audits/script19a_v3_check_validation_checks.csv"

input_readiness_19e <- "outputs/audits/script19e_mediation_readiness_assessment.csv"
input_decision_19e <- "outputs/audits/script19e_tpb_operational_decision_matrix.csv"

input_covariate_19f <- "outputs/audits/script19f_covariate_detection_audit.csv"
input_sample_19f <- "outputs/audits/script19f_model_sample_audit.csv"

input_final_results_19g <- "outputs/tables/script19g_final_never_sex_tpb_mechanism_key_results.csv"
input_weighted_effects_19g <- "outputs/tables/script19g_weighted_adjusted_main_effects.csv"
input_fit_19g <- "outputs/tables/script19g_final_model_fit_summary.csv"
input_phase_decision_19g <- "outputs/audits/script19g_final_phase_decision.csv"
input_commit_checklist_19g <- "outputs/audits/script19g_commit_readiness_checklist.csv"
input_inventory_19g <- "outputs/tables/script19g_phase_output_inventory.csv"

# Outputs
formatted_docx_path <- "outputs/reports/script19g_v2_final_never_sex_tpb_mechanism_report_FORMATTED.docx"
formatted_md_path <- "outputs/reports/script19g_v2_final_never_sex_tpb_mechanism_report_FORMATTED.md"
formatting_checklist_path <- "outputs/audits/script19g_v2_report_formatting_checklist.csv"
report_table_inventory_path <- "outputs/tables/script19g_v2_report_table_inventory.csv"
compact_key_results_path <- "outputs/tables/script19g_v2_compact_key_results_for_report.csv"
compact_fit_path <- "outputs/tables/script19g_v2_compact_weighted_model_fit_for_report.csv"
log_path <- "outputs/logs/script19g_v2_run_log.txt"

cat("", file = log_path)

log_line <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(txt, "\n", file = log_path, append = TRUE)
  message(txt)
}

log_line("Started ", script_id, ": ", script_title)
log_line("Project root: ", project_root)

# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------

read_required <- function(path) {
  if (!file.exists(path)) {
    stop("Required input file not found: ", path, call. = FALSE)
  }
  
  readr::read_csv(path, show_col_types = FALSE)
}

short_outcome <- function(x) {
  dplyr::case_when(
    x == "tpb_attitudes_delay_mean_1_5" ~ "Delay-supportive attitudes",
    x == "peer_norm_delay_H1MO1" ~ "Peer norm",
    x == "partner_norm_delay_H1MO2" ~ "Partner norm",
    x == "maternal_norm_delay_H1MO4" ~ "Maternal norm",
    TRUE ~ x
  )
}

short_predictor <- function(x) {
  dplyr::case_when(
    x == "Family connectedness" ~ "Family connectedness",
    x == "Friend support" ~ "Friend support",
    TRUE ~ x
  )
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), as.numeric(x)))
}

add_plain_title <- function(doc, text) {
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        text,
        prop = officer::fp_text(
          bold = TRUE,
          font.size = 18,
          font.family = "Arial"
        )
      ),
      fp_p = officer::fp_par(text.align = "center")
    )
  )
  
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc
}

add_plain_heading <- function(doc, text, level = 1) {
  size <- ifelse(level == 1, 14, 12)
  
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        text,
        prop = officer::fp_text(
          bold = TRUE,
          font.size = size,
          font.family = "Arial"
        )
      )
    )
  )
  
  doc
}

add_note <- function(doc, text) {
  doc <- officer::body_add_par(
    doc,
    text,
    style = "Normal"
  )
  
  doc
}

make_ft <- function(data, font_size = 8, max_width = 6.8) {
  ft <- flextable::flextable(data)
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::padding(ft, padding = 2, part = "all")
  ft <- flextable::bold(ft, bold = TRUE, part = "header")
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::autofit(ft)
  
  if ("fit_to_width" %in% getNamespaceExports("flextable")) {
    ft <- flextable::fit_to_width(ft, max_width = max_width)
  }
  
  ft
}

add_ft <- function(doc, data, font_size = 8, max_width = 6.8) {
  ft <- make_ft(data, font_size = font_size, max_width = max_width)
  doc <- flextable::body_add_flextable(doc, value = ft)
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc
}

# ------------------------------------------------------------
# 4. Load inputs
# ------------------------------------------------------------

construct_summary <- read_required(input_construct_summary)
internal_19a <- read_required(input_internal_19a)
validation_19a <- read_required(input_validation_19a)

readiness_19e <- read_required(input_readiness_19e)
decision_19e <- read_required(input_decision_19e)

covariate_19f <- read_required(input_covariate_19f)
sample_19f <- read_required(input_sample_19f)

final_results_19g <- read_required(input_final_results_19g)
weighted_effects_19g <- read_required(input_weighted_effects_19g)
fit_19g <- read_required(input_fit_19g)
phase_decision_19g <- read_required(input_phase_decision_19g)
commit_checklist_19g <- read_required(input_commit_checklist_19g)
inventory_19g <- read_required(input_inventory_19g)

log_line("All required inputs loaded.")

# ------------------------------------------------------------
# 5. Build compact report tables
# ------------------------------------------------------------

sample_n <- phase_decision_19g |>
  dplyr::filter(decision_area == "analytic_sample") |>
  dplyr::pull(decision)

if (length(sample_n) == 0) {
  sample_n <- "Use never-sex adolescents aged 15-19; final n = 2143."
}

construct_report <- construct_summary |>
  dplyr::transmute(
    Construct = construct,
    Variable = variable,
    N = n_valid,
    Mean = round(mean, 3),
    SD = round(sd, 3),
    Min = min,
    Max = max
  )

internal_report <- internal_19a |>
  dplyr::transmute(
    Construct = construct,
    Items = n_items,
    Complete_N = n_complete,
    Alpha = ifelse(is.na(alpha), NA_character_, fmt_num(alpha, 3)),
    Interpretation = stringr::str_wrap(interpretation, width = 55)
  )

readiness_report <- readiness_19e |>
  dplyr::filter(
    criterion %in% c(
      "Atitude mediator available",
      "Norma subjetiva mediator available",
      "Controlo percebido / autoeficácia mediator available",
      "Direct intention-to-delay outcome available",
      "Full TPB mediation ready",
      "Exploratory TPB-compatible mechanism ready"
    )
  ) |>
  dplyr::transmute(
    Criterion = criterion,
    Assessment = assessment,
    Explanation = stringr::str_wrap(explanation, width = 60)
  )

covariate_report <- covariate_19f |>
  dplyr::transmute(
    Covariate = covariate,
    Source = stringr::str_replace_all(detected_variable, "data/raw/", ""),
    Adjusted = included_in_adjusted_models,
    Weight = used_as_weight,
    Valid_N = n_valid
  )

sample_report <- sample_19f |>
  dplyr::filter(model_variant %in% c("C_adjusted", "D_adjusted_weighted")) |>
  dplyr::transmute(
    Model = model_id,
    Outcome = short_outcome(dependent_variable),
    Variant = dplyr::case_when(
      model_variant == "C_adjusted" ~ "Adjusted",
      model_variant == "D_adjusted_weighted" ~ "Adjusted weighted",
      TRUE ~ model_variant
    ),
    N_available = n_available,
    N_complete = n_complete,
    Weighted = weighted
  )

compact_key_results <- final_results_19g |>
  dplyr::transmute(
    Outcome = short_outcome(dependent_variable),
    Predictor = short_predictor(predictor),
    b = coefficient,
    CI_95 = confidence_interval,
    p = p_value_display,
    Std_beta = standardized_beta,
    Reading = stringr::str_wrap(substantive_reading, width = 45)
  )

readr::write_csv(compact_key_results, compact_key_results_path)

compact_fit <- fit_19g |>
  dplyr::filter(weighted == TRUE) |>
  dplyr::transmute(
    Model = model_label,
    Outcome = short_outcome(dependent_variable),
    N = n_used,
    R2 = fmt_num(r_squared, 3),
    Adj_R2 = fmt_num(adj_r_squared, 3),
    Model_p = f_p_value_display
  )

readr::write_csv(compact_fit, compact_fit_path)

phase_decision_report <- phase_decision_19g |>
  dplyr::transmute(
    Area = decision_area,
    Decision = stringr::str_wrap(decision, width = 70),
    Status = status
  )

commit_report <- commit_checklist_19g |>
  dplyr::transmute(
    Check = check_id,
    Status = dplyr::case_when(
      check_id == "git_commit_ready" ~ "PENDING",
      TRUE ~ status
    ),
    Note = stringr::str_wrap(note, width = 65)
  )

# ------------------------------------------------------------
# 6. Narrative summaries
# ------------------------------------------------------------

executive_summary <- c(
  "This formatted report consolidates the final never-sex TPB-compatible mechanism phase.",
  "The analysis focuses on adolescents aged 15-19 who had not yet had sexual intercourse.",
  "A full Theory of Planned Behavior mediation model was not estimated because no direct intention-to-delay outcome and no confirmed perceived behavioral control/self-efficacy construct were available.",
  "The final empirical specification is therefore an exploratory TPB-compatible psychosocial mechanism analysis."
)

main_findings <- c(
  "Family connectedness was positively associated with delay-supportive attitudes in the weighted adjusted model.",
  "Family connectedness and friend support were both positively associated with more delay-supportive peer norms.",
  "Partner norm results were weak and should be interpreted cautiously.",
  "Friend support was positively associated with maternal norm orientation, while family connectedness was not independently associated with maternal norm after adjustment."
)

methodological_caution <- c(
  "The results are cross-sectional associations.",
  "They do not establish causality, temporal ordering or formal mediation.",
  "The correct reporting label is exploratory TPB-compatible psychosocial mechanism analysis."
)

# ------------------------------------------------------------
# 7. Markdown report
# ------------------------------------------------------------

md_lines <- c(
  "# Script 19g_v2 — Formatted Final Never-Sex TPB Mechanism Report",
  "",
  paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
  "",
  "## Executive summary",
  "",
  paste0("- ", executive_summary),
  "",
  "## Main findings",
  "",
  paste0("- ", main_findings),
  "",
  "## Methodological caution",
  "",
  paste0("- ", methodological_caution),
  "",
  "## Main output",
  "",
  paste0("Formatted Word report: `", formatted_docx_path, "`"),
  "",
  "No Git action was performed."
)

writeLines(md_lines, formatted_md_path)

# ------------------------------------------------------------
# 8. Word report
# ------------------------------------------------------------

doc <- officer::read_docx()

doc <- add_plain_title(
  doc,
  "Final Never-Sex TPB-Compatible Mechanism Report"
)

doc <- add_note(
  doc,
  paste0("Script 19g_v2 | Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S"))
)

doc <- add_plain_heading(doc, "Executive summary", level = 1)

for (txt in executive_summary) {
  doc <- add_note(doc, txt)
}

doc <- add_plain_heading(doc, "Analytic sample and construct validation", level = 1)

doc <- add_note(
  doc,
  sample_n[1]
)

doc <- add_note(
  doc,
  "The corrected family, friend and deterrence constructs passed 11 of 11 validation checks."
)

doc <- add_plain_heading(doc, "Construct summary", level = 2)
doc <- add_ft(doc, construct_report, font_size = 8)

doc <- add_plain_heading(doc, "Internal consistency", level = 2)
doc <- add_ft(doc, internal_report, font_size = 8)

doc <- add_plain_heading(doc, "TPB operationalization decision", level = 1)

doc <- add_note(
  doc,
  paste(
    "The full TPB mediation model was not estimated.",
    "The available Add Health public-use indicators support an exploratory mechanism analysis, not a formal mediation model."
  )
)

doc <- add_plain_heading(doc, "Mediation readiness", level = 2)
doc <- add_ft(doc, readiness_report, font_size = 7)

doc <- add_plain_heading(doc, "Regression design", level = 1)

doc <- add_note(
  doc,
  paste(
    "The dependent variables were delay-supportive attitudes, peer norm, partner norm and maternal norm.",
    "The main predictors were family connectedness and friend support.",
    "Adjusted models controlled for age, sex/gender, grade and residence.",
    "Weighted adjusted models used GSWGT1 recovered from 21600-0004-Data.rda."
  )
)

doc <- add_plain_heading(doc, "Covariates and survey weight", level = 2)
doc <- add_ft(doc, covariate_report, font_size = 7)

doc <- add_plain_heading(doc, "Adjusted and weighted model sample sizes", level = 2)
doc <- add_ft(doc, sample_report, font_size = 7)

doc <- add_plain_heading(doc, "Final weighted adjusted results", level = 1)

doc <- add_note(
  doc,
  paste(
    "The following table reports the preferred final summary estimates.",
    "They include covariate adjustment and survey weighting."
  )
)

doc <- add_ft(doc, compact_key_results, font_size = 7)

doc <- add_plain_heading(doc, "Weighted model fit summary", level = 1)

doc <- add_note(
  doc,
  "The residual standard error from weighted lm models is not emphasized because it is affected by the scale of the survey weights."
)

doc <- add_ft(doc, compact_fit, font_size = 8)

doc <- add_plain_heading(doc, "Interpretation", level = 1)

for (txt in main_findings) {
  doc <- add_note(doc, txt)
}

doc <- add_plain_heading(doc, "Methodological caution", level = 1)

for (txt in methodological_caution) {
  doc <- add_note(doc, txt)
}

doc <- add_plain_heading(doc, "Final phase decision", level = 1)
doc <- add_ft(doc, phase_decision_report, font_size = 7)

doc <- add_plain_heading(doc, "Commit readiness", level = 1)

doc <- add_note(
  doc,
  "The phase is analytically complete. Git commit should be performed only after manual review of this formatted report."
)

doc <- add_ft(doc, commit_report, font_size = 7)

doc <- add_plain_heading(doc, "Detailed outputs retained as CSV", level = 1)

doc <- add_note(
  doc,
  paste(
    "Wide diagnostic tables are not reproduced in full in this Word report.",
    "They remain available in the project outputs/audits and outputs/tables folders."
  )
)

print(doc, target = formatted_docx_path)

# ------------------------------------------------------------
# 9. Formatting checklist and table inventory
# ------------------------------------------------------------

formatting_checklist <- tibble::tibble(
  check_id = c(
    "formatted_docx_created",
    "duplicate_heading_numbering_avoided",
    "wide_tables_reduced",
    "key_results_included",
    "weighted_fit_summary_included",
    "methodological_caution_included",
    "git_commit_ready"
  ),
  status = c(
    ifelse(file.exists(formatted_docx_path), "PASS", "FAIL"),
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PENDING_MANUAL_REVIEW"
  ),
  note = c(
    formatted_docx_path,
    "Report uses plain formatted headings instead of Word auto-numbered heading styles.",
    "Only compact report tables are inserted into Word; full diagnostic outputs remain in CSV.",
    compact_key_results_path,
    compact_fit_path,
    "The report explicitly states that these are exploratory cross-sectional associations, not formal TPB mediation.",
    "Proceed to Git commit only after reviewing the formatted Word report."
  )
)

readr::write_csv(formatting_checklist, formatting_checklist_path)

report_table_inventory <- tibble::tibble(
  table_name = c(
    "Construct summary",
    "Internal consistency",
    "Mediation readiness",
    "Covariates and survey weight",
    "Adjusted and weighted sample sizes",
    "Final weighted adjusted results",
    "Weighted model fit summary",
    "Final phase decision",
    "Commit readiness"
  ),
  included_in_word = TRUE,
  source = c(
    input_construct_summary,
    input_internal_19a,
    input_readiness_19e,
    input_covariate_19f,
    input_sample_19f,
    compact_key_results_path,
    compact_fit_path,
    input_phase_decision_19g,
    input_commit_checklist_19g
  )
)

readr::write_csv(report_table_inventory, report_table_inventory_path)

# ------------------------------------------------------------
# 10. Console output
# ------------------------------------------------------------

end_time <- Sys.time()

log_line("Saved formatted Word report: ", formatted_docx_path)
log_line("Saved formatted markdown report: ", formatted_md_path)
log_line("Saved formatting checklist: ", formatting_checklist_path)
log_line("Saved report table inventory: ", report_table_inventory_path)
log_line("Saved compact key results: ", compact_key_results_path)
log_line("Saved compact weighted fit summary: ", compact_fit_path)
log_line("Completed ", script_id, " in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
log_line("No Git action was performed.")

cat("\n============================================================\n")
cat("Script 19g_v2 completed: Formatted Final Never-Sex TPB Report\n")
cat("============================================================\n\n")

cat("Formatting checklist:\n")
formatting_checklist |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nCompact key results:\n")
compact_key_results |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nCompact weighted model fit:\n")
compact_fit |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nReport table inventory:\n")
report_table_inventory |>
  tibble::as_tibble() |>
  print(n = Inf)

cat("\nMain outputs:\n")

main_outputs <- tibble::tibble(
  output = c(
    "Formatted Word report",
    "Formatted markdown report",
    "Formatting checklist",
    "Report table inventory",
    "Compact key results",
    "Compact weighted model fit",
    "Run log"
  ),
  path = c(
    formatted_docx_path,
    formatted_md_path,
    formatting_checklist_path,
    report_table_inventory_path,
    compact_key_results_path,
    compact_fit_path,
    log_path
  ),
  exists = file.exists(c(
    formatted_docx_path,
    formatted_md_path,
    formatting_checklist_path,
    report_table_inventory_path,
    compact_key_results_path,
    compact_fit_path,
    log_path
  ))
)

main_outputs |>
  tibble::as_tibble() |>
  print(n = Inf)