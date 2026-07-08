# ============================================================
# Script 18c — Section 8 Item Meaning and Reliability Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Audit H1RP1–H1RP6 before deciding whether they should form:
#     1. one Section 8 index,
#     2. subindices,
#     3. or separate item-level predictors.
#
# Section 8 variables:
#   H1RP1, H1RP2, H1RP3, H1RP4, H1RP5, H1RP6
#
# Main outputs:
#   outputs/audits/script18c_s08_item_metadata.csv
#   outputs/audits/script18c_s08_item_distributions.csv
#   outputs/audits/script18c_s08_correlation_matrix.csv
#   outputs/audits/script18c_s08_reliability_summary.csv
#   outputs/audits/script18c_s08_alpha_if_deleted.csv
#   outputs/audits/script18c_s08_item_total_correlations.csv
#   outputs/audits/script18c_s08_preliminary_decision_table.csv
#   outputs/audits/script18c_s08_manual_construct_review_TEMPLATE.csv
#   outputs/audits/script18c_final_status.csv
#   docs/add_health_wave01_section8_item_meaning_reliability_script18c.docx
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
    "Missing packages: ",
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
# 1. Project root
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
cat("Script 18c started: Section 8 Item Meaning and Reliability Audit\n")
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

to_numeric_safe <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- as.numeric(x)
  }
  suppressWarnings(as.numeric(as.character(x)))
}

variable_label_string <- function(x) {
  lbl <- attr(x, "label", exact = TRUE)
  if (is.null(lbl)) return("")
  clean_chr(lbl)
}

value_label_string <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels)) return("")
  paste0(as.numeric(labels), "=", names(labels), collapse = "; ")
}

# Specific cleaning for Section 8 Likert-type items.
# For H1RP variables, 1–5 are usually the plausible response scale.
# Values 6, 8, 9, 96–99 and high missing codes are treated as missing.
clean_s08_numeric <- function(x) {
  
  x_chr <- as.character(x)
  
  # Extract the first numeric code from strings such as:
  # "(1) (1) Strongly agree"
  # "(5) (5) Almost certain"
  x_num <- suppressWarnings(
    as.numeric(stringr::str_extract(x_chr, "-?\\d+(\\.\\d+)?"))
  )
  
  missing_codes <- c(
    6, 7, 8, 9,
    96, 97, 98, 99,
    996, 997, 998, 999
  )
  
  x_num[x_num %in% missing_codes] <- NA_real_
  
  x_num
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

  alpha <- (k / (k - 1)) * (1 - sum(item_vars, na.rm = TRUE) / total_var)

  alpha
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

  if (!item_name %in% names(df_items)) return(NA_real_)

  other_items <- setdiff(names(df_items), item_name)

  if (length(other_items) == 0) return(NA_real_)

  item_vec <- df_items[[item_name]]
  other_score <- rowMeans(df_items[, other_items, drop = FALSE], na.rm = TRUE)

  ok <- !is.na(item_vec) & !is.na(other_score)

  if (sum(ok) < 10) return(NA_real_)

  suppressWarnings(cor(item_vec[ok], other_score[ok]))
}

# ------------------------------------------------------------
# 3. Locate source file with H1RP1–H1RP6
# ------------------------------------------------------------

s08_items <- paste0("H1RP", 1:6)

candidate_files <- list.files(
  project_root,
  pattern = "\\.(rda|RData|rds)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

candidate_files <- normalizePath(candidate_files, winslash = "/", mustWork = FALSE)

candidate_files <- candidate_files[
  !stringr::str_detect(candidate_files, "/\\.git/") &
    !stringr::str_detect(candidate_files, "/outputs/") &
    !stringr::str_detect(candidate_files, "/docs/")
]

source_candidates <- list()

for (fp in candidate_files) {

  ext <- tolower(tools::file_ext(fp))

  if (ext %in% c("rda", "rdata")) {

    env <- new.env(parent = emptyenv())
    loaded <- load(fp, envir = env)

    for (nm in loaded) {
      obj <- get(nm, envir = env)

      if (is.data.frame(obj)) {
        df <- as_tibble(obj)
        found_items <- intersect(s08_items, names(df))

        if (length(found_items) > 0) {
          source_candidates[[length(source_candidates) + 1]] <- tibble(
            source_file = fp,
            object_name = nm,
            rows = nrow(df),
            columns = ncol(df),
            items_found = length(found_items),
            found_items = paste(found_items, collapse = ", "),
            has_AID = "AID" %in% names(df)
          )
        }
      }
    }
  }

  if (ext == "rds") {

    obj <- readRDS(fp)

    if (is.data.frame(obj)) {
      df <- as_tibble(obj)
      found_items <- intersect(s08_items, names(df))

      if (length(found_items) > 0) {
        source_candidates[[length(source_candidates) + 1]] <- tibble(
          source_file = fp,
          object_name = basename(fp),
          rows = nrow(df),
          columns = ncol(df),
          items_found = length(found_items),
          found_items = paste(found_items, collapse = ", "),
          has_AID = "AID" %in% names(df)
        )
      }
    }
  }
}

source_candidates <- bind_rows(source_candidates)

if (nrow(source_candidates) == 0) {
  stop("No H1RP1–H1RP6 variables were found in local files.")
}

best_source <- source_candidates %>%
  arrange(desc(items_found), desc(has_AID), desc(rows), desc(columns)) %>%
  slice(1)

cat("Best Section 8 source:\n")
print(best_source)

# Load best source
fp <- best_source$source_file[1]
obj_name <- best_source$object_name[1]
ext <- tolower(tools::file_ext(fp))

if (ext %in% c("rda", "rdata")) {
  env <- new.env(parent = emptyenv())
  load(fp, envir = env)
  s08_df_raw <- as_tibble(get(obj_name, envir = env))
}

if (ext == "rds") {
  s08_df_raw <- as_tibble(readRDS(fp))
}

available_items <- intersect(s08_items, names(s08_df_raw))

if (length(available_items) < 2) {
  stop("Fewer than two Section 8 items were recovered. Reliability analysis cannot proceed.")
}

# ------------------------------------------------------------
# 4. Metadata and distributions
# ------------------------------------------------------------

item_metadata <- tibble(
  variable = available_items,
  variable_label = purrr::map_chr(
    available_items,
    ~ variable_label_string(s08_df_raw[[.x]])
  ),
  value_labels = purrr::map_chr(
    available_items,
    ~ value_label_string(s08_df_raw[[.x]])
  ),
  source_file = fp,
  object_name = obj_name
)

s08_clean_items <- s08_df_raw %>%
  select(all_of(available_items)) %>%
  mutate(across(everything(), clean_s08_numeric))

item_distributions <- purrr::map_dfr(
  available_items,
  function(v) {
    x <- s08_clean_items[[v]]

    tibble(value = x) %>%
      count(value, name = "n") %>%
      mutate(
        variable = v,
        percent = round(100 * n / sum(n), 2)
      ) %>%
      select(variable, value, n, percent)
  }
)

item_summary <- tibble(
  variable = available_items,
  valid_n = purrr::map_int(available_items, ~ sum(!is.na(s08_clean_items[[.x]]))),
  missing_n = purrr::map_int(available_items, ~ sum(is.na(s08_clean_items[[.x]]))),
  missing_rate = round(missing_n / nrow(s08_clean_items), 4),
  mean = purrr::map_dbl(available_items, ~ safe_mean(s08_clean_items[[.x]])),
  sd = purrr::map_dbl(available_items, ~ safe_sd(s08_clean_items[[.x]])),
  min = purrr::map_dbl(available_items, ~ safe_min(s08_clean_items[[.x]])),
  max = purrr::map_dbl(available_items, ~ safe_max(s08_clean_items[[.x]]))
) %>%
  left_join(item_metadata, by = "variable") %>%
  select(
    variable,
    variable_label,
    value_labels,
    valid_n,
    missing_n,
    missing_rate,
    mean,
    sd,
    min,
    max,
    source_file,
    object_name
  )

# ------------------------------------------------------------
# 5. Correlation and reliability diagnostics
# ------------------------------------------------------------

complete_case_n <- s08_clean_items %>%
  filter(if_all(everything(), ~ !is.na(.x))) %>%
  nrow()

correlation_matrix <- suppressWarnings(
  cor(s08_clean_items, use = "pairwise.complete.obs")
)

correlation_matrix_df <- as.data.frame(correlation_matrix) %>%
  rownames_to_column("variable")

alpha_all <- cronbach_alpha(s08_clean_items)
std_alpha_all <- standardized_alpha(s08_clean_items)

alpha_if_deleted <- purrr::map_dfr(
  available_items,
  function(v) {

    remaining <- setdiff(available_items, v)

    tibble(
      deleted_item = v,
      remaining_items = paste(remaining, collapse = ", "),
      alpha_if_deleted = cronbach_alpha(s08_clean_items[, remaining, drop = FALSE]),
      standardized_alpha_if_deleted = standardized_alpha(s08_clean_items[, remaining, drop = FALSE])
    )
  }
)

item_total_correlations <- tibble(
  variable = available_items,
  corrected_item_total_correlation = purrr::map_dbl(
    available_items,
    ~ item_total_correlation(s08_clean_items, .x)
  )
) %>%
  left_join(
    item_metadata %>% select(variable, variable_label),
    by = "variable"
  ) %>%
  select(variable, variable_label, corrected_item_total_correlation)

reliability_summary <- tibble(
  metric = c(
    "items_available",
    "respondents_total",
    "complete_case_n",
    "cronbach_alpha_complete_cases",
    "standardized_alpha_complete_cases"
  ),
  value = c(
    length(available_items),
    nrow(s08_clean_items),
    complete_case_n,
    alpha_all,
    std_alpha_all
  )
)

# ------------------------------------------------------------
# 6. Preliminary decision table
# ------------------------------------------------------------

preliminary_decision_table <- item_total_correlations %>%
  left_join(
    alpha_if_deleted,
    by = c("variable" = "deleted_item")
  ) %>%
  left_join(
    item_summary %>%
      select(variable, valid_n, missing_rate, mean, sd, min, max),
    by = "variable"
  ) %>%
  mutate(
    preliminary_statistical_flag = case_when(
      is.na(corrected_item_total_correlation) ~ "review_no_item_total_correlation",
      corrected_item_total_correlation < 0.20 ~ "review_low_item_total_correlation",
      corrected_item_total_correlation >= 0.20 &
        corrected_item_total_correlation < 0.30 ~ "acceptable_but_weak",
      corrected_item_total_correlation >= 0.30 ~ "acceptable",
      TRUE ~ "review"
    ),
    alpha_deleted_flag = case_when(
      is.na(alpha_if_deleted) | is.na(alpha_all) ~ "review",
      alpha_if_deleted > alpha_all + 0.03 ~ "item_may_reduce_alpha",
      TRUE ~ "no_major_alpha_problem"
    ),
    preliminary_decision = case_when(
      preliminary_statistical_flag == "acceptable" &
        alpha_deleted_flag == "no_major_alpha_problem" ~ "retain_if_theoretically_coherent",
      preliminary_statistical_flag == "acceptable_but_weak" ~ "retain_or_review",
      preliminary_statistical_flag == "review_low_item_total_correlation" ~ "manual_review_required",
      TRUE ~ "manual_review_required"
    )
  )

manual_construct_review_template <- item_summary %>%
  mutate(
    proposed_section = "Section 8",
    proposed_general_domain = "Perceived consequences of sexual activity",
    manual_item_meaning = "",
    manual_construct_label_en = "",
    manual_construct_label_pt = "",
    manual_same_construct_as_others = "",
    manual_group_or_subindex = "",
    manual_reverse_score_needed = "",
    manual_include_in_index = "",
    manual_include_as_separate_predictor = "",
    manual_exclusion_reason = "",
    manual_reviewer = "",
    manual_review_date = ""
  ) %>%
  select(
    variable,
    variable_label,
    value_labels,
    proposed_section,
    proposed_general_domain,
    manual_item_meaning,
    manual_construct_label_en,
    manual_construct_label_pt,
    manual_same_construct_as_others,
    manual_group_or_subindex,
    manual_reverse_score_needed,
    manual_include_in_index,
    manual_include_as_separate_predictor,
    manual_exclusion_reason,
    manual_reviewer,
    manual_review_date,
    valid_n,
    missing_rate,
    mean,
    sd,
    min,
    max
  )

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Purpose of Script 18c", "Audit H1RP1–H1RP6 item meaning and reliability before constructing any Section 8 index.",
  "Item-level caution", "The six H1RP items are not automatically treated as one construct because they may refer to different perceived consequences of sexual activity.",
  "Cronbach alpha use", "Cronbach's alpha is used as an exploratory diagnostic only. It does not override substantive item meaning.",
  "Index construction", "No final Section 8 index is constructed in this script. Index construction requires manual review of item meaning and direction.",
  "Possible outcomes", "The items may be retained as one index, grouped into subindices, or used as separate predictors.",
  "Data protection", "Only aggregate item-level summaries are exported."
)

# ------------------------------------------------------------
# 7. Write outputs
# ------------------------------------------------------------

write_csv(
  source_candidates,
  file.path(audit_dir, "script18c_s08_source_candidates.csv")
)

write_csv(
  item_metadata,
  file.path(audit_dir, "script18c_s08_item_metadata.csv")
)

write_csv(
  item_distributions,
  file.path(audit_dir, "script18c_s08_item_distributions.csv")
)

write_csv(
  item_summary,
  file.path(audit_dir, "script18c_s08_item_summary.csv")
)

write_csv(
  correlation_matrix_df,
  file.path(audit_dir, "script18c_s08_correlation_matrix.csv")
)

write_csv(
  reliability_summary,
  file.path(audit_dir, "script18c_s08_reliability_summary.csv")
)

write_csv(
  alpha_if_deleted,
  file.path(audit_dir, "script18c_s08_alpha_if_deleted.csv")
)

write_csv(
  item_total_correlations,
  file.path(audit_dir, "script18c_s08_item_total_correlations.csv")
)

write_csv(
  preliminary_decision_table,
  file.path(audit_dir, "script18c_s08_preliminary_decision_table.csv")
)

write_csv(
  manual_construct_review_template,
  file.path(audit_dir, "script18c_s08_manual_construct_review_TEMPLATE.csv")
)

write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18c_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 8. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_section8_item_meaning_reliability_script18c.docx"
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
      "Add Health Wave I — Section 8 Item Meaning and Reliability Audit",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "This report audits H1RP1–H1RP6 before deciding whether they should form a single index, subindices, or separate predictors.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Source candidates", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(source_candidates)) %>%
    officer::body_add_par("Item summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(item_summary)) %>%
    officer::body_add_par("Reliability summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(reliability_summary)) %>%
    officer::body_add_par("Alpha if item deleted", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(alpha_if_deleted)) %>%
    officer::body_add_par("Item-total correlations", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(item_total_correlations)) %>%
    officer::body_add_par("Preliminary decision table", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(preliminary_decision_table)) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(methodological_decisions))

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 9. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "section8_source_found",
    "all_six_items_available",
    "item_metadata_created",
    "item_distributions_created",
    "correlation_matrix_created",
    "reliability_summary_created",
    "alpha_if_deleted_created",
    "item_total_correlations_created",
    "manual_review_template_created",
    "word_report_created",
    "ready_for_manual_section8_decision"
  ),
  status = c(
    nrow(source_candidates) > 0,
    all(s08_items %in% available_items),
    file.exists(file.path(audit_dir, "script18c_s08_item_metadata.csv")),
    file.exists(file.path(audit_dir, "script18c_s08_item_distributions.csv")),
    file.exists(file.path(audit_dir, "script18c_s08_correlation_matrix.csv")),
    file.exists(file.path(audit_dir, "script18c_s08_reliability_summary.csv")),
    file.exists(file.path(audit_dir, "script18c_s08_alpha_if_deleted.csv")),
    file.exists(file.path(audit_dir, "script18c_s08_item_total_correlations.csv")),
    file.exists(file.path(audit_dir, "script18c_s08_manual_construct_review_TEMPLATE.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    TRUE
  )
)

write_csv(
  final_status,
  file.path(audit_dir, "script18c_final_status.csv")
)

# ------------------------------------------------------------
# 10. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18c completed: Section 8 Item Meaning and Reliability Audit\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nItem summary:\n")
print(item_summary, n = Inf)

cat("\nReliability summary:\n")
print(reliability_summary)

cat("\nAlpha if item deleted:\n")
print(alpha_if_deleted, n = Inf)

cat("\nItem-total correlations:\n")
print(item_total_correlations, n = Inf)

cat("\nCorrelation matrix:\n")
print(correlation_matrix_df, n = Inf)

cat("\nPreliminary decision table:\n")
print(preliminary_decision_table, n = Inf)

cat("\nManual review template created:\n")
cat(file.path(audit_dir, "script18c_s08_manual_construct_review_TEMPLATE.csv"), "\n")

cat("\nWord report:\n")
cat(word_report_path, "\n")

cat("\nRequired next action:\n")
cat("Review item labels and reliability diagnostics before constructing any Section 8 index.\n")
cat("Do not commit yet.\n")