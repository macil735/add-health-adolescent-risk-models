# ============================================================
# Script 19a_v3_check
# Validation of Corrected Never-Sex Constructs
# Project: Add Health Adolescent Risk Models
# ============================================================
#
# Purpose:
#   Validate the corrected constructs produced by Script 19a_v3:
#   1. family_connectedness
#   2. friend_support
#   3. sexual_delay_deterrence
#
# This script checks:
#   - analytic sample consistency
#   - construct missingness
#   - item missingness
#   - value ranges
#   - inter-item correlations
#   - internal consistency
#   - construct-level correlations
#   - methodological note for the change from 19a_v2 to 19a_v3
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

script_id <- "19a_v3_check"
script_title <- "Validation of Corrected Never-Sex Constructs"
start_time <- Sys.time()

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

core_pkgs <- c(
  "dplyr",
  "readr",
  "stringr",
  "purrr",
  "tibble",
  "tidyr"
)

missing_core <- core_pkgs[
  !vapply(core_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_core) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_core, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(tidyr)
})

has_docx <- all(vapply(
  c("officer", "flextable"),
  requireNamespace,
  logical(1),
  quietly = TRUE
))

# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

if (!str_detect(basename(project_root), "add-health-adolescent-risk-models")) {
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

input_rds <- "outputs/analysis/script19a_v3_corrected_never_sex_family_friend_deterrence_constructs.rds"
input_sample_audit <- "outputs/audits/script19a_v3_never_sex_sample_audit.csv"
input_coding_audit <- "outputs/audits/script19a_v3_construct_coding_audit.csv"
input_item_summary <- "outputs/audits/script19a_v3_item_summary.csv"
input_construct_summary <- "outputs/tables/script19a_v3_construct_summary.csv"

validation_checks_path <- "outputs/audits/script19a_v3_check_validation_checks.csv"
construct_missingness_path <- "outputs/audits/script19a_v3_check_construct_missingness.csv"
item_missingness_path <- "outputs/audits/script19a_v3_check_item_missingness.csv"
range_checks_path <- "outputs/audits/script19a_v3_check_value_range_checks.csv"
inter_item_correlations_path <- "outputs/tables/script19a_v3_check_inter_item_correlations.csv"
internal_consistency_path <- "outputs/tables/script19a_v3_check_internal_consistency.csv"
construct_correlations_path <- "outputs/tables/script19a_v3_check_construct_correlations.csv"
distribution_summary_path <- "outputs/tables/script19a_v3_check_distribution_summary.csv"
method_note_path <- "outputs/reports/script19a_v3_check_methodological_note.md"
docx_path <- "outputs/reports/script19a_v3_check_validation_report.docx"
log_path <- "outputs/logs/script19a_v3_check_run_log.txt"

cat("", file = log_path)

log_line <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(txt, "\n", file = log_path, append = TRUE)
  message(txt)
}

log_line("Started ", script_id, ": ", script_title)
log_line("Project root: ", project_root)

# ------------------------------------------------------------
# 3. Load inputs
# ------------------------------------------------------------

required_inputs <- c(
  input_rds,
  input_sample_audit,
  input_coding_audit,
  input_item_summary,
  input_construct_summary
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing input file(s). Run Script 19a_v3 first. Missing: ",
    paste(missing_inputs, collapse = ", "),
    call. = FALSE
  )
}

construct_df <- readRDS(input_rds)

sample_audit <- read_csv(input_sample_audit, show_col_types = FALSE)
coding_audit <- read_csv(input_coding_audit, show_col_types = FALSE)
item_summary <- read_csv(input_item_summary, show_col_types = FALSE)
construct_summary <- read_csv(input_construct_summary, show_col_types = FALSE)

log_line("Loaded construct dataset: ", input_rds)
log_line("Rows in construct dataset: ", nrow(construct_df))
log_line("Columns in construct dataset: ", ncol(construct_df))

# ------------------------------------------------------------
# 4. Expected variables
# ------------------------------------------------------------

construct_mean_vars <- c(
  family_connectedness = "family_connectedness_mean_1_5",
  friend_support = "friend_support_mean_1_5",
  sexual_delay_deterrence = "sexual_delay_deterrence_mean_1_5"
)

construct_z_vars <- c(
  family_connectedness = "family_connectedness_z",
  friend_support = "friend_support_z",
  sexual_delay_deterrence = "sexual_delay_deterrence_z"
)

construct_item_n_vars <- c(
  family_connectedness = "family_connectedness_item_n",
  friend_support = "friend_support_item_n",
  sexual_delay_deterrence = "sexual_delay_deterrence_item_n"
)

family_item_vars <- item_summary |>
  filter(construct == "family_connectedness") |>
  pull(transformed_variable)

friend_item_vars <- item_summary |>
  filter(construct == "friend_support") |>
  pull(transformed_variable)

deterrence_item_vars <- item_summary |>
  filter(construct == "sexual_delay_deterrence") |>
  pull(transformed_variable)

all_item_vars <- c(family_item_vars, friend_item_vars, deterrence_item_vars)

missing_construct_columns <- setdiff(
  c(construct_mean_vars, construct_z_vars, construct_item_n_vars, all_item_vars),
  names(construct_df)
)

if (length(missing_construct_columns) > 0) {
  stop(
    "Missing expected construct columns: ",
    paste(missing_construct_columns, collapse = ", "),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 5. Helper functions
# ------------------------------------------------------------

pct <- function(x, denom) {
  round(100 * x / denom, 2)
}

safe_cor <- function(data) {
  if (ncol(data) < 2) {
    return(matrix(NA_real_, nrow = ncol(data), ncol = ncol(data)))
  }
  
  suppressWarnings(
    cor(data, use = "pairwise.complete.obs")
  )
}

cor_to_long <- function(cor_mat, label) {
  out <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  
  names(out) <- c("var1", "var2", "correlation")
  
  out |>
    mutate(
      block = label,
      correlation = round(as.numeric(correlation), 4)
    ) |>
    filter(var1 < var2) |>
    arrange(block, var1, var2)
}

cronbach_alpha <- function(data, construct_label) {
  data <- as.data.frame(data)
  
  k <- ncol(data)
  
  if (k < 2) {
    return(tibble(
      construct = construct_label,
      n_items = k,
      n_complete = NA_integer_,
      alpha = NA_real_,
      interpretation = "Not applicable: fewer than two items."
    ))
  }
  
  complete_data <- data[stats::complete.cases(data), , drop = FALSE]
  n_complete <- nrow(complete_data)
  
  if (n_complete < 5) {
    return(tibble(
      construct = construct_label,
      n_items = k,
      n_complete = n_complete,
      alpha = NA_real_,
      interpretation = "Not computed: too few complete cases."
    ))
  }
  
  item_vars <- apply(complete_data, 2, stats::var, na.rm = TRUE)
  total_score <- rowSums(complete_data)
  total_var <- stats::var(total_score, na.rm = TRUE)
  
  if (is.na(total_var) || total_var == 0) {
    alpha <- NA_real_
  } else {
    alpha <- (k / (k - 1)) * (1 - sum(item_vars, na.rm = TRUE) / total_var)
  }
  
  interpretation <- dplyr::case_when(
    is.na(alpha) ~ "Not interpretable.",
    alpha >= 0.80 ~ "High internal consistency.",
    alpha >= 0.70 ~ "Acceptable internal consistency.",
    alpha >= 0.60 ~ "Moderate internal consistency; acceptable for exploratory analysis with few items.",
    alpha >= 0.50 ~ "Low-to-moderate consistency; interpret cautiously.",
    TRUE ~ "Low internal consistency; avoid treating as a homogeneous scale without justification."
  )
  
  tibble(
    construct = construct_label,
    n_items = k,
    n_complete = n_complete,
    alpha = round(alpha, 4),
    interpretation = interpretation
  )
}

distribution_one <- function(data, var, construct_label) {
  x <- data[[var]]
  
  tibble(
    construct = construct_label,
    variable = var,
    n_valid = sum(!is.na(x)),
    n_missing = sum(is.na(x)),
    pct_valid = pct(sum(!is.na(x)), nrow(data)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = suppressWarnings(min(x, na.rm = TRUE)),
    q25 = suppressWarnings(as.numeric(stats::quantile(x, 0.25, na.rm = TRUE))),
    median = suppressWarnings(stats::median(x, na.rm = TRUE)),
    q75 = suppressWarnings(as.numeric(stats::quantile(x, 0.75, na.rm = TRUE))),
    max = suppressWarnings(max(x, na.rm = TRUE))
  )
}

range_check_one <- function(data, var, expected_min = 1, expected_max = 5) {
  x <- data[[var]]
  
  outside <- !is.na(x) & (x < expected_min | x > expected_max)
  
  tibble(
    variable = var,
    n_valid = sum(!is.na(x)),
    observed_min = suppressWarnings(ifelse(all(is.na(x)), NA_real_, min(x, na.rm = TRUE))),
    observed_max = suppressWarnings(ifelse(all(is.na(x)), NA_real_, max(x, na.rm = TRUE))),
    expected_min = expected_min,
    expected_max = expected_max,
    n_outside_expected_range = sum(outside),
    status = ifelse(sum(outside) == 0, "PASS", "FAIL")
  )
}

# ------------------------------------------------------------
# 6. Construct missingness
# ------------------------------------------------------------

construct_missingness <- tibble(
  construct = names(construct_mean_vars),
  variable = unname(construct_mean_vars)
) |>
  mutate(
    n_total = nrow(construct_df),
    n_valid = purrr::map_int(variable, ~ sum(!is.na(construct_df[[.x]]))),
    n_missing = n_total - n_valid,
    pct_valid = round(100 * n_valid / n_total, 2),
    pct_missing = round(100 * n_missing / n_total, 2)
  )

write_csv(construct_missingness, construct_missingness_path)

# ------------------------------------------------------------
# 7. Item missingness
# ------------------------------------------------------------

item_missingness <- item_summary |>
  transmute(
    construct,
    item_name,
    source_variable,
    transformed_variable,
    n_total = nrow(construct_df),
    n_valid = n_valid_transformed,
    n_missing = n_total - n_valid_transformed,
    pct_valid = round(100 * n_valid_transformed / n_total, 2),
    pct_missing = round(100 * (n_total - n_valid_transformed) / n_total, 2),
    transformed_mean,
    transformed_sd,
    transformed_min,
    transformed_max
  )

write_csv(item_missingness, item_missingness_path)

# ------------------------------------------------------------
# 8. Value range checks
# ------------------------------------------------------------

range_checks <- bind_rows(
  purrr::map_dfr(
    c(all_item_vars, unname(construct_mean_vars)),
    ~ range_check_one(construct_df, .x, expected_min = 1, expected_max = 5)
  ),
  purrr::map_dfr(
    unname(construct_item_n_vars),
    ~ range_check_one(
      construct_df,
      .x,
      expected_min = 0,
      expected_max = max(construct_df[[.x]], na.rm = TRUE)
    )
  )
)

write_csv(range_checks, range_checks_path)

# ------------------------------------------------------------
# 9. Internal consistency
# ------------------------------------------------------------

internal_consistency <- bind_rows(
  cronbach_alpha(
    construct_df |>
      select(all_of(family_item_vars)),
    "family_connectedness"
  ),
  cronbach_alpha(
    construct_df |>
      select(all_of(friend_item_vars)),
    "friend_support"
  ),
  cronbach_alpha(
    construct_df |>
      select(all_of(deterrence_item_vars)),
    "sexual_delay_deterrence"
  )
)

write_csv(internal_consistency, internal_consistency_path)

# ------------------------------------------------------------
# 10. Inter-item correlations
# ------------------------------------------------------------

inter_item_correlations <- bind_rows(
  cor_to_long(
    safe_cor(construct_df |> select(all_of(family_item_vars))),
    "family_connectedness"
  ),
  cor_to_long(
    safe_cor(construct_df |> select(all_of(deterrence_item_vars))),
    "sexual_delay_deterrence"
  )
)

write_csv(inter_item_correlations, inter_item_correlations_path)

# ------------------------------------------------------------
# 11. Construct-level correlations
# ------------------------------------------------------------

construct_cor_mat <- safe_cor(
  construct_df |>
    select(all_of(unname(construct_mean_vars)))
)

construct_correlations <- cor_to_long(
  construct_cor_mat,
  "construct_level"
)

write_csv(construct_correlations, construct_correlations_path)

# ------------------------------------------------------------
# 12. Distribution summary
# ------------------------------------------------------------

distribution_summary <- bind_rows(
  distribution_one(
    construct_df,
    "family_connectedness_mean_1_5",
    "family_connectedness"
  ),
  distribution_one(
    construct_df,
    "friend_support_mean_1_5",
    "friend_support"
  ),
  distribution_one(
    construct_df,
    "sexual_delay_deterrence_mean_1_5",
    "sexual_delay_deterrence"
  )
) |>
  mutate(
    across(
      c(mean, sd, min, q25, median, q75, max),
      ~ round(.x, 4)
    )
  )

write_csv(distribution_summary, distribution_summary_path)

# ------------------------------------------------------------
# 13. Validation checks
# ------------------------------------------------------------

sample_n <- sample_audit$final_never_sex_age_15_19_n[1]

expected_sample_n <- 2143

validation_checks <- tibble(
  check_id = c(
    "sample_n",
    "family_items_detected",
    "friend_items_detected",
    "deterrence_items_detected",
    "family_valid_pct",
    "friend_valid_pct",
    "deterrence_valid_pct",
    "range_checks",
    "family_reverse_coding",
    "friend_reverse_coding",
    "deterrence_reverse_coding"
  ),
  check_description = c(
    "Analytic sample should match adolescents aged 15–19 who never had sexual intercourse.",
    "Family connectedness should use four H1PR items.",
    "Friend support should use one H1PR item.",
    "Sexual delay deterrence should use three H1MO items.",
    "Family connectedness should have high construct-level coverage.",
    "Friend support should have high construct-level coverage.",
    "Sexual delay deterrence should have high construct-level coverage.",
    "All item and construct mean scores should be within expected ranges.",
    "Family connectedness should not be reverse-coded.",
    "Friend support should not be reverse-coded.",
    "Sexual delay deterrence should be reverse-coded using 6 - raw value."
  ),
  expected = c(
    as.character(expected_sample_n),
    "4",
    "1",
    "3",
    ">= 90%",
    ">= 90%",
    ">= 90%",
    "0 values outside expected ranges",
    "FALSE for all family items",
    "FALSE for friend item",
    "TRUE for all deterrence items"
  ),
  observed = c(
    as.character(sample_n),
    as.character(sample_audit$family_items_detected[1]),
    as.character(sample_audit$friend_items_detected[1]),
    as.character(sample_audit$deterrence_items_detected[1]),
    paste0(construct_missingness$pct_valid[construct_missingness$construct == "family_connectedness"], "%"),
    paste0(construct_missingness$pct_valid[construct_missingness$construct == "friend_support"], "%"),
    paste0(construct_missingness$pct_valid[construct_missingness$construct == "sexual_delay_deterrence"], "%"),
    as.character(sum(range_checks$n_outside_expected_range, na.rm = TRUE)),
    paste(unique(coding_audit$reverse_coded[coding_audit$construct == "family_connectedness"]), collapse = "; "),
    paste(unique(coding_audit$reverse_coded[coding_audit$construct == "friend_support"]), collapse = "; "),
    paste(unique(coding_audit$reverse_coded[coding_audit$construct == "sexual_delay_deterrence"]), collapse = "; ")
  ),
  status = c(
    ifelse(sample_n == expected_sample_n, "PASS", "REVIEW"),
    ifelse(sample_audit$family_items_detected[1] == 4, "PASS", "FAIL"),
    ifelse(sample_audit$friend_items_detected[1] == 1, "PASS", "FAIL"),
    ifelse(sample_audit$deterrence_items_detected[1] == 3, "PASS", "FAIL"),
    ifelse(construct_missingness$pct_valid[construct_missingness$construct == "family_connectedness"] >= 90, "PASS", "REVIEW"),
    ifelse(construct_missingness$pct_valid[construct_missingness$construct == "friend_support"] >= 90, "PASS", "REVIEW"),
    ifelse(construct_missingness$pct_valid[construct_missingness$construct == "sexual_delay_deterrence"] >= 90, "PASS", "REVIEW"),
    ifelse(sum(range_checks$n_outside_expected_range, na.rm = TRUE) == 0, "PASS", "FAIL"),
    ifelse(all(coding_audit$reverse_coded[coding_audit$construct == "family_connectedness"] == FALSE), "PASS", "FAIL"),
    ifelse(all(coding_audit$reverse_coded[coding_audit$construct == "friend_support"] == FALSE), "PASS", "FAIL"),
    ifelse(all(coding_audit$reverse_coded[coding_audit$construct == "sexual_delay_deterrence"] == TRUE), "PASS", "FAIL")
  )
)

write_csv(validation_checks, validation_checks_path)

# ------------------------------------------------------------
# 14. Methodological note
# ------------------------------------------------------------

method_note <- c(
  "# Script 19a_v3_check — Methodological Validation Note",
  "",
  paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
  "",
  "## Analytic sample",
  "",
  paste0(
    "The validation uses the corrected never-sex analytic sample created by Script 19a_v3. ",
    "The sample includes adolescents aged 15–19 who had not yet had sexual intercourse. ",
    "Final analytic sample size: ", sample_n, "."
  ),
  "",
  "## Construct source correction",
  "",
  "Script 19a_v3 replaced the earlier source alignment used in 19a_v2. The corrected construct definitions are:",
  "",
  "- family_connectedness: H1PR3, H1PR5, H1PR7, H1PR8;",
  "- friend_support: H1PR4;",
  "- sexual_delay_deterrence: H1MO2, H1MO3, H1MO4.",
  "",
  "The deterrence construct should not be interpreted as a full general sexual delay scale. It is a narrower measure of perceived social, moral and maternal deterrents to sexual initiation.",
  "",
  "## Coding direction",
  "",
  "Family connectedness and friend support are coded directly because higher valid values indicate stronger perceived support or connection.",
  "",
  "H1MO2, H1MO3 and H1MO4 are reverse-coded using 6 - raw value because agreement indicates stronger perceived deterrence. After transformation, higher scores indicate stronger perceived deterrence.",
  "",
  "## Validation outputs",
  "",
  paste0("- Validation checks: ", validation_checks_path),
  paste0("- Construct missingness: ", construct_missingness_path),
  paste0("- Item missingness: ", item_missingness_path),
  paste0("- Range checks: ", range_checks_path),
  paste0("- Inter-item correlations: ", inter_item_correlations_path),
  paste0("- Internal consistency: ", internal_consistency_path),
  paste0("- Construct correlations: ", construct_correlations_path),
  paste0("- Distribution summary: ", distribution_summary_path),
  "",
  "No Git action was performed."
)

writeLines(method_note, method_note_path)

# ------------------------------------------------------------
# 15. Optional Word report
# ------------------------------------------------------------

if (has_docx) {
  doc <- officer::read_docx()
  
  doc <- officer::body_add_par(doc, script_title, style = "heading 1")
  
  doc <- officer::body_add_par(
    doc,
    paste0("Script: ", script_id),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(
    doc,
    paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Validation checks", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(validation_checks))
  )
  
  doc <- officer::body_add_par(doc, "Construct missingness", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(construct_missingness))
  )
  
  doc <- officer::body_add_par(doc, "Item missingness", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(item_missingness))
  )
  
  doc <- officer::body_add_par(doc, "Distribution summary", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(distribution_summary))
  )
  
  doc <- officer::body_add_par(doc, "Internal consistency", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(internal_consistency))
  )
  
  doc <- officer::body_add_par(doc, "Construct correlations", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(construct_correlations))
  )
  
  doc <- officer::body_add_par(doc, "Inter-item correlations", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(inter_item_correlations))
  )
  
  print(doc, target = docx_path)
}

# ------------------------------------------------------------
# 16. Console output
# ------------------------------------------------------------

end_time <- Sys.time()

log_line("Saved validation checks: ", validation_checks_path)
log_line("Saved construct missingness: ", construct_missingness_path)
log_line("Saved item missingness: ", item_missingness_path)
log_line("Saved range checks: ", range_checks_path)
log_line("Saved inter-item correlations: ", inter_item_correlations_path)
log_line("Saved internal consistency: ", internal_consistency_path)
log_line("Saved construct correlations: ", construct_correlations_path)
log_line("Saved distribution summary: ", distribution_summary_path)
log_line("Saved methodological note: ", method_note_path)

if (has_docx) {
  log_line("Saved Word report: ", docx_path)
} else {
  log_line("Word report not created because officer/flextable were unavailable.")
}

log_line("Completed ", script_id, " in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
log_line("No Git action was performed.")

cat("\n============================================================\n")
cat("Script 19a_v3_check completed\n")
cat("============================================================\n\n")

cat("Validation checks:\n")
print(validation_checks, n = Inf)

cat("\nConstruct missingness:\n")
print(construct_missingness, n = Inf)

cat("\nInternal consistency:\n")
print(internal_consistency, n = Inf)

cat("\nConstruct correlations:\n")
tibble::as_tibble(construct_correlations) |>
  print(n = Inf)

cat("\nInter-item correlations:\n")
tibble::as_tibble(inter_item_correlations) |>
  print(n = Inf)

cat("\nDistribution summary:\n")
tibble::as_tibble(distribution_summary) |>
  print(n = Inf)

cat("\nMain outputs:\n")
print(tibble(
  output = c(
    "Validation checks",
    "Construct missingness",
    "Item missingness",
    "Range checks",
    "Inter-item correlations",
    "Internal consistency",
    "Construct correlations",
    "Distribution summary",
    "Methodological note",
    "Word report",
    "Run log"
  ),
  path = c(
    validation_checks_path,
    construct_missingness_path,
    item_missingness_path,
    range_checks_path,
    inter_item_correlations_path,
    internal_consistency_path,
    construct_correlations_path,
    distribution_summary_path,
    method_note_path,
    ifelse(has_docx, docx_path, NA_character_),
    log_path
  ),
  exists = file.exists(c(
    validation_checks_path,
    construct_missingness_path,
    item_missingness_path,
    range_checks_path,
    inter_item_correlations_path,
    internal_consistency_path,
    construct_correlations_path,
    distribution_summary_path,
    method_note_path,
    ifelse(has_docx, docx_path, ""),
    log_path
  ))
))