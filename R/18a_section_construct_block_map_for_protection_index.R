# ============================================================
# Script 18a — Section and Construct Block Map for Protection Index
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   Map reviewed protection/risk/ambiguous items by section and
#   construct block before constructing block-specific indices.
#
# Methodological position:
#   - Script 18a does not estimate regressions.
#   - Script 18a does not create new respondent-level data.
#   - Script 18a maps items from the completed manual review file.
#   - The goal is to prepare block-level analysis where construct
#     blocks can later be used as independent variables and the
#     protection index can be used as the dependent variable.
#
# Main input:
#   outputs/audits/script15_manual_item_direction_review_COMPLETED.csv
#
# Main outputs:
#   outputs/audits/script18a_section_construct_block_map.csv
#   outputs/audits/script18a_items_by_section.csv
#   outputs/audits/script18a_construct_block_summary.csv
#   outputs/audits/script18a_block_readiness_for_18b.csv
#   outputs/audits/script18a_protection_index_item_map.csv
#   docs/add_health_wave01_section_construct_block_map_script18a.docx
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
cat("Script 18a started: Section and Construct Block Map\n")
cat("============================================================\n\n")
cat("Project root:\n", project_root, "\n\n")

# ------------------------------------------------------------
# 2. Input files
# ------------------------------------------------------------

completed_review_path <- file.path(
  audit_dir,
  "script15_manual_item_direction_review_COMPLETED.csv"
)

if (!file.exists(completed_review_path)) {
  stop(
    "Completed manual item review file not found:\n",
    completed_review_path,
    "\nRun Scripts 15 and 15b before Script 18a."
  )
}

review_raw <- readr::read_csv(
  completed_review_path,
  show_col_types = FALSE
)

if (nrow(review_raw) == 0) {
  stop("The completed manual item review file is empty.")
}

cat("Completed manual item review loaded:\n")
cat(completed_review_path, "\n\n")
cat("Rows in completed review:", nrow(review_raw), "\n\n")

# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------

clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_squish(x)
}

lower_clean <- function(x) {
  stringr::str_to_lower(clean_chr(x))
}

first_existing_col <- function(data, candidates) {
  found <- intersect(candidates, names(data))
  if (length(found) == 0) {
    return(NA_character_)
  }
  found[1]
}

get_col_or_blank <- function(data, col) {
  if (is.na(col) || !col %in% names(data)) {
    return(rep("", nrow(data)))
  }
  as.character(data[[col]])
}

get_col_or_na <- function(data, col) {
  if (is.na(col) || !col %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }
  as.character(data[[col]])
}

standard_yes_no <- function(x) {
  x <- lower_clean(x)
  dplyr::case_when(
    x %in% c("yes", "y", "1", "true", "include", "included") ~ "yes",
    x %in% c("no", "n", "0", "false", "exclude", "excluded") ~ "no",
    x == "" ~ "missing",
    TRUE ~ x
  )
}

standard_role <- function(x) {
  x <- lower_clean(x)
  dplyr::case_when(
    stringr::str_detect(x, "protect") ~ "protection",
    stringr::str_detect(x, "risk") ~ "risk",
    stringr::str_detect(x, "ambig|review|unclear") ~ "ambiguous_review",
    stringr::str_detect(x, "exclude") ~ "exclude",
    x == "" ~ "missing",
    TRUE ~ x
  )
}

section_working_domain <- function(section_id, section_name) {

  text_l <- lower_clean(paste(section_id, section_name))

  dplyr::case_when(
    stringr::str_detect(text_l, "s08|section 8") ~
      "perceptions_attitudes_or_perceived_risk",
    stringr::str_detect(text_l, "s09|section 9") ~
      "school_connectedness_and_safety",
    stringr::str_detect(text_l, "s17|section 17") ~
      "motivations_future_orientation_or_sexual_health_context",
    stringr::str_detect(text_l, "s18|section 18") ~
      "self_efficacy_personality_or_personal_agency",
    stringr::str_detect(text_l, "s19|section 19") ~
      "norms_religiosity_or_moral_orientation",
    stringr::str_detect(text_l, "s20|section 20") ~
      "peer_context_or_social_influence",
    TRUE ~ "unclassified_section_domain"
  )
}

assign_conceptual_block <- function(section_id,
                                    section_name,
                                    construct_block,
                                    variable,
                                    variable_label,
                                    candidate_text) {

  text_l <- lower_clean(
    paste(
      section_id,
      section_name,
      construct_block,
      variable,
      variable_label,
      candidate_text,
      sep = " | "
    )
  )

  dplyr::case_when(
    stringr::str_detect(text_l, "mother|father|parent|family|famil") ~
      "family_support_parental_connectedness",

    stringr::str_detect(text_l, "school|teacher|student|classmate|safe at school|school safety|connectedness") ~
      "school_connectedness_and_safety",

    stringr::str_detect(text_l, "future|college|university|education|graduate|expect|aspiration|motivation") ~
      "future_orientation_educational_aspirations",

    stringr::str_detect(text_l, "self|efficacy|control|agency|confidence|decide|decision|refuse|resist|personality|autonomy") ~
      "self_efficacy_personal_agency",

    stringr::str_detect(text_l, "peer|friend|friends|best friend|crowd|social influence") ~
      "peer_context_social_influence",

    stringr::str_detect(text_l, "risk|chance|likely|pregnan|hiv|aids|std|sti|sexually transmitted|diagnos|condom|birthcontrol|birth control") ~
      "perceived_sexual_health_risk_or_contraceptive_context",

    stringr::str_detect(text_l, "relig|church|pray|moral|wrong|values|norm") ~
      "norms_religiosity_moral_orientation",

    TRUE ~
      "unclassified_construct_review"
  )
}

block_label_for_report <- function(block) {
  dplyr::case_when(
    block == "family_support_parental_connectedness" ~
      "Family support / parental connectedness",
    block == "school_connectedness_and_safety" ~
      "School connectedness / safety",
    block == "future_orientation_educational_aspirations" ~
      "Future orientation / educational aspirations",
    block == "self_efficacy_personal_agency" ~
      "Self-efficacy / personal agency",
    block == "peer_context_social_influence" ~
      "Peer context / social influence",
    block == "perceived_sexual_health_risk_or_contraceptive_context" ~
      "Perceived sexual health risk / contraceptive context",
    block == "norms_religiosity_moral_orientation" ~
      "Norms / religiosity / moral orientation",
    TRUE ~
      "Unclassified construct review"
  )
}

# ------------------------------------------------------------
# 4. Detect relevant columns in completed review file
# ------------------------------------------------------------

section_id_col <- first_existing_col(
  review_raw,
  c("section_id", "section", "section_code")
)

section_name_col <- first_existing_col(
  review_raw,
  c("section_name", "section_label", "section_title")
)

variable_col <- first_existing_col(
  review_raw,
  c("variable", "variable_name", "item_variable")
)

variable_label_col <- first_existing_col(
  review_raw,
  c("variable_label", "label", "item_label", "question_label")
)

candidate_text_col <- first_existing_col(
  review_raw,
  c("candidate_text", "item_text", "question_text", "text")
)

construct_block_col <- first_existing_col(
  review_raw,
  c(
    "manual_construct_block",
    "construct_block",
    "auto_construct_block",
    "construct",
    "block",
    "construct_name"
  )
)

final_role_col <- first_existing_col(
  review_raw,
  c("manual_final_role", "final_role", "auto_final_role", "role")
)

include_col <- first_existing_col(
  review_raw,
  c("manual_include_in_index", "include_in_index", "manual_include", "include")
)

score_direction_col <- first_existing_col(
  review_raw,
  c(
    "manual_score_direction",
    "score_direction",
    "manual_item_score_direction",
    "auto_score_direction"
  )
)

reverse_score_col <- first_existing_col(
  review_raw,
  c(
    "manual_reverse_score_decision",
    "manual_reverse_score",
    "reverse_score",
    "reverse_score_decision",
    "auto_reverse_score"
  )
)

rationale_col <- first_existing_col(
  review_raw,
  c(
    "manual_decision_rationale",
    "decision_rationale",
    "manual_rationale",
    "rationale",
    "auto_decision_rationale"
  )
)

if (is.na(variable_col)) {
  stop(
    "Could not detect the variable column in the completed review file. ",
    "Expected a column such as variable, variable_name, or item_variable."
  )
}

detected_columns <- tibble(
  field = c(
    "section_id",
    "section_name",
    "variable",
    "variable_label",
    "candidate_text",
    "construct_block",
    "final_role",
    "include_in_index",
    "score_direction",
    "reverse_score",
    "decision_rationale"
  ),
  detected_column = c(
    section_id_col,
    section_name_col,
    variable_col,
    variable_label_col,
    candidate_text_col,
    construct_block_col,
    final_role_col,
    include_col,
    score_direction_col,
    reverse_score_col,
    rationale_col
  )
)

readr::write_csv(
  detected_columns,
  file.path(audit_dir, "script18a_detected_review_columns.csv")
)

# ------------------------------------------------------------
# 5. Build section and construct map
# ------------------------------------------------------------

section_construct_map <- review_raw %>%
  mutate(.review_row_id = row_number()) %>%
  transmute(
    review_row_id = .review_row_id,
    section_id = get_col_or_blank(review_raw, section_id_col),
    section_name = get_col_or_blank(review_raw, section_name_col),
    variable = get_col_or_blank(review_raw, variable_col),
    variable_label = get_col_or_blank(review_raw, variable_label_col),
    candidate_text = get_col_or_blank(review_raw, candidate_text_col),
    original_construct_block = get_col_or_blank(review_raw, construct_block_col),
    manual_final_role_raw = get_col_or_blank(review_raw, final_role_col),
    manual_include_in_index_raw = get_col_or_blank(review_raw, include_col),
    manual_score_direction = get_col_or_blank(review_raw, score_direction_col),
    manual_reverse_score_decision = get_col_or_blank(review_raw, reverse_score_col),
    manual_decision_rationale = get_col_or_blank(review_raw, rationale_col)
  ) %>%
  mutate(
    section_id = ifelse(section_id == "", "unclassified_section", section_id),
    section_name = ifelse(section_name == "", "Unclassified section", section_name),
    section_working_domain = section_working_domain(section_id, section_name),
    manual_final_role = standard_role(manual_final_role_raw),
    manual_include_in_index = standard_yes_no(manual_include_in_index_raw),
    conceptual_block = purrr::pmap_chr(
      list(
        section_id,
        section_name,
        original_construct_block,
        variable,
        variable_label,
        candidate_text
      ),
      assign_conceptual_block
    ),
    conceptual_block_label = block_label_for_report(conceptual_block),
    included_in_reviewed_protection_index_raw =
      manual_final_role == "protection" &
      manual_include_in_index == "yes"
  )

# Duplicate handling: Script 16 retained unique variables for the final index.
included_protection_unique <- section_construct_map %>%
  filter(included_in_reviewed_protection_index_raw) %>%
  arrange(section_id, conceptual_block, variable, review_row_id) %>%
  group_by(variable) %>%
  mutate(
    protection_review_rows_for_variable = n(),
    retained_unique_protection_item = row_number() == 1
  ) %>%
  ungroup() %>%
  select(
    review_row_id,
    protection_review_rows_for_variable,
    retained_unique_protection_item
  )

section_construct_map <- section_construct_map %>%
  left_join(
    included_protection_unique,
    by = "review_row_id"
  ) %>%
  mutate(
    protection_review_rows_for_variable = ifelse(
      is.na(protection_review_rows_for_variable),
      0L,
      protection_review_rows_for_variable
    ),
    retained_unique_protection_item = ifelse(
      is.na(retained_unique_protection_item),
      FALSE,
      retained_unique_protection_item
    ),
    final_index_inclusion_status = case_when(
      retained_unique_protection_item ~ "included_unique_in_reviewed_protection_index",
      included_in_reviewed_protection_index_raw & !retained_unique_protection_item ~
        "duplicate_review_row_not_counted_as_unique_item",
      manual_final_role == "protection" & manual_include_in_index != "yes" ~
        "protection_role_not_included_in_index",
      manual_final_role == "risk" ~
        "risk_role_not_included_in_protection_index",
      manual_final_role == "ambiguous_review" ~
        "ambiguous_not_included",
      manual_final_role == "exclude" ~
        "excluded",
      TRUE ~ "not_included_or_missing_decision"
    )
  ) %>%
  arrange(section_id, conceptual_block, variable, review_row_id)

readr::write_csv(
  section_construct_map,
  file.path(audit_dir, "script18a_section_construct_block_map.csv")
)

# ------------------------------------------------------------
# 6. Summaries by section and construct block
# ------------------------------------------------------------

items_by_section <- section_construct_map %>%
  group_by(section_id, section_name, section_working_domain) %>%
  summarise(
    review_rows = n(),
    unique_variables = n_distinct(variable),
    unique_protection_items_in_index = sum(retained_unique_protection_item),
    protection_rows = sum(manual_final_role == "protection"),
    risk_rows = sum(manual_final_role == "risk"),
    ambiguous_rows = sum(manual_final_role == "ambiguous_review"),
    excluded_rows = sum(manual_final_role == "exclude"),
    .groups = "drop"
  ) %>%
  arrange(section_id)

construct_block_summary <- section_construct_map %>%
  group_by(conceptual_block, conceptual_block_label) %>%
  summarise(
    review_rows = n(),
    unique_variables = n_distinct(variable),
    unique_protection_items_in_index = sum(retained_unique_protection_item),
    protection_rows = sum(manual_final_role == "protection"),
    risk_rows = sum(manual_final_role == "risk"),
    ambiguous_rows = sum(manual_final_role == "ambiguous_review"),
    excluded_rows = sum(manual_final_role == "exclude"),
    sections_present = paste(sort(unique(section_id)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(unique_protection_items_in_index), conceptual_block)

section_construct_crosswalk <- section_construct_map %>%
  group_by(
    section_id,
    section_working_domain,
    conceptual_block,
    conceptual_block_label
  ) %>%
  summarise(
    review_rows = n(),
    unique_variables = n_distinct(variable),
    unique_protection_items_in_index = sum(retained_unique_protection_item),
    .groups = "drop"
  ) %>%
  arrange(section_id, conceptual_block)

readr::write_csv(
  items_by_section,
  file.path(audit_dir, "script18a_items_by_section.csv")
)

readr::write_csv(
  construct_block_summary,
  file.path(audit_dir, "script18a_construct_block_summary.csv")
)

readr::write_csv(
  section_construct_crosswalk,
  file.path(audit_dir, "script18a_section_construct_crosswalk.csv")
)

# ------------------------------------------------------------
# 7. Protection index item map
# ------------------------------------------------------------

protection_index_item_map <- section_construct_map %>%
  filter(retained_unique_protection_item) %>%
  select(
    variable,
    variable_label,
    section_id,
    section_name,
    section_working_domain,
    conceptual_block,
    conceptual_block_label,
    manual_score_direction,
    manual_reverse_score_decision,
    manual_decision_rationale,
    final_index_inclusion_status
  ) %>%
  arrange(section_id, conceptual_block, variable)

readr::write_csv(
  protection_index_item_map,
  file.path(audit_dir, "script18a_protection_index_item_map.csv")
)

# ------------------------------------------------------------
# 8. Block readiness for Script 18b
# ------------------------------------------------------------

block_readiness_for_18b <- construct_block_summary %>%
  mutate(
    block_index_readiness = case_when(
      unique_protection_items_in_index >= 3 ~ "ready_for_multi_item_block_index",
      unique_protection_items_in_index == 2 ~ "possible_short_block_index",
      unique_protection_items_in_index == 1 ~ "single_item_only_use_with_caution",
      TRUE ~ "not_ready_no_unique_protection_items"
    ),
    recommended_18b_action = case_when(
      unique_protection_items_in_index >= 3 ~
        "Construct block index and assess reliability.",
      unique_protection_items_in_index == 2 ~
        "Construct short block index and report item count limitation.",
      unique_protection_items_in_index == 1 ~
        "Do not call this a scale; retain as single-item block if substantively important.",
      TRUE ~
        "Do not construct block index from retained protection items."
    )
  ) %>%
  arrange(desc(unique_protection_items_in_index), conceptual_block)

readr::write_csv(
  block_readiness_for_18b,
  file.path(audit_dir, "script18a_block_readiness_for_18b.csv")
)

# ------------------------------------------------------------
# 9. Field guide and methodological decisions
# ------------------------------------------------------------

construct_block_field_guide <- tibble::tribble(
  ~conceptual_block, ~conceptual_block_label, ~working_interpretation, ~caution_for_18b,
  "family_support_parental_connectedness", "Family support / parental connectedness", "Measures family or parental connectedness, support, communication or monitoring.", "Can be treated as a protection determinant if not mechanically overlapping with the dependent protection index.",
  "school_connectedness_and_safety", "School connectedness / safety", "Measures school attachment, school safety or perceived school environment.", "Likely protection block, but should be tested separately from the aggregate index.",
  "future_orientation_educational_aspirations", "Future orientation / educational aspirations", "Measures future expectations, educational aspirations or long-term orientation.", "Potentially protective; should be evaluated as a determinant of the protection index.",
  "self_efficacy_personal_agency", "Self-efficacy / personal agency", "Measures self-efficacy, personal control, agency, autonomy or refusal capacity.", "Important block for thesis-style analysis; avoid circularity if included in the dependent index.",
  "peer_context_social_influence", "Peer context / social influence", "Measures peer context, friend influence or social network orientation.", "May be protective or risk-enhancing depending on item direction; should not be assumed protective.",
  "perceived_sexual_health_risk_or_contraceptive_context", "Perceived sexual health risk / contraceptive context", "Measures perceived sexual health risk, contraception-related context or perceived consequences.", "Can represent protection or prior exposure; interpret carefully.",
  "norms_religiosity_moral_orientation", "Norms / religiosity / moral orientation", "Measures moral norms, religiosity or normative orientation.", "May reflect protection, restriction or reporting norms; interpret cautiously.",
  "unclassified_construct_review", "Unclassified construct review", "Items that could not be confidently assigned to a conceptual block.", "Requires manual review before use in Script 18b."
)

methodological_decisions <- tibble::tribble(
  ~decision_area, ~decision,
  "Purpose", "Script 18a maps reviewed items by section and construct block before constructing block-level indices.",
  "Dependent variable logic", "The intended next-stage analysis treats the protection index as the dependent variable and construct blocks as independent variables.",
  "Circularity caution", "If a construct block is part of the protection index, the next-stage model should use a thesis-style separated index or leave-one-block-out strategy.",
  "Section interpretation", "Sections are treated as working domains, not final theoretical constructs. Construct blocks are assigned using section information, item labels and available construct labels.",
  "Protection index item map", "Only unique retained protection items are counted as part of the final reviewed_protection_index map.",
  "Risk index", "No risk index is constructed here. Risk, ambiguous and excluded items are documented only for audit and future review.",
  "Next step", "Script 18b should construct block-specific indices where item counts allow it and identify blocks that require single-item or manual handling."
)

readr::write_csv(
  construct_block_field_guide,
  file.path(audit_dir, "script18a_construct_block_field_guide.csv")
)

readr::write_csv(
  methodological_decisions,
  file.path(audit_dir, "script18a_methodological_decisions.csv")
)

# ------------------------------------------------------------
# 10. Optional Word report
# ------------------------------------------------------------

word_report_path <- file.path(
  doc_dir,
  "add_health_wave01_section_construct_block_map_script18a.docx"
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
      "Add Health Wave I — Section and Construct Block Map",
      style = "heading 1"
    ) %>%
    officer::body_add_par(
      "Script 18a maps reviewed psychosocial items by section and construct block. The aim is to prepare block-level analysis in which construct blocks may be used as independent variables and the protection index as the dependent variable, while avoiding circularity.",
      style = "Normal"
    ) %>%
    officer::body_add_par("Detected review columns", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(detected_columns)) %>%
    officer::body_add_par("Items by section", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(items_by_section)) %>%
    officer::body_add_par("Construct block summary", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(construct_block_summary)) %>%
    officer::body_add_par("Block readiness for Script 18b", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(block_readiness_for_18b)) %>%
    officer::body_add_par("Protection index item map", style = "heading 2") %>%
    flextable::body_add_flextable(
      make_ft(
        protection_index_item_map %>%
          select(
            variable,
            variable_label,
            section_id,
            conceptual_block_label,
            manual_score_direction,
            manual_reverse_score_decision
          )
      )
    ) %>%
    officer::body_add_par("Methodological decisions", style = "heading 2") %>%
    flextable::body_add_flextable(make_ft(methodological_decisions))

  print(doc, target = word_report_path)

} else {
  word_report_path <- NA_character_
}

# ------------------------------------------------------------
# 11. Final status
# ------------------------------------------------------------

final_status <- tibble(
  check = c(
    "completed_review_loaded",
    "section_construct_map_created",
    "items_by_section_created",
    "construct_block_summary_created",
    "protection_index_item_map_created",
    "block_readiness_created",
    "methodological_decisions_created",
    "word_report_created",
    "ready_for_script18b"
  ),
  status = c(
    file.exists(completed_review_path),
    file.exists(file.path(audit_dir, "script18a_section_construct_block_map.csv")),
    file.exists(file.path(audit_dir, "script18a_items_by_section.csv")),
    file.exists(file.path(audit_dir, "script18a_construct_block_summary.csv")),
    file.exists(file.path(audit_dir, "script18a_protection_index_item_map.csv")),
    file.exists(file.path(audit_dir, "script18a_block_readiness_for_18b.csv")),
    file.exists(file.path(audit_dir, "script18a_methodological_decisions.csv")),
    !is.na(word_report_path) && file.exists(word_report_path),
    nrow(protection_index_item_map) > 0
  )
)

readr::write_csv(
  final_status,
  file.path(audit_dir, "script18a_final_status.csv")
)

# ------------------------------------------------------------
# 12. Console output
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("Script 18a completed: Section and Construct Block Map\n")
cat("============================================================\n\n")

cat("Final status:\n")
print(final_status)

cat("\nItems by section:\n")
print(items_by_section)

cat("\nConstruct block summary:\n")
print(construct_block_summary)

cat("\nBlock readiness for Script 18b:\n")
print(block_readiness_for_18b)

cat("\nProtection index item map preview:\n")
print(
  protection_index_item_map %>%
    select(
      variable,
      variable_label,
      section_id,
      conceptual_block_label,
      manual_score_direction,
      manual_reverse_score_decision
    ) %>%
    slice_head(n = 30)
)

cat("\nOutputs created:\n")
cat("- ", file.path(audit_dir, "script18a_detected_review_columns.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_section_construct_block_map.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_items_by_section.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_construct_block_summary.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_section_construct_crosswalk.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_protection_index_item_map.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_block_readiness_for_18b.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_construct_block_field_guide.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_methodological_decisions.csv"), "\n")
cat("- ", file.path(audit_dir, "script18a_final_status.csv"), "\n")

if (!is.na(word_report_path)) {
  cat("- ", word_report_path, "\n")
} else {
  cat("- Word report not created because officer/flextable is not available.\n")
}

cat("\nRequired next action:\n")
cat("Review the section and construct block map before building Script 18b.\n")
cat("Do not commit until the block classification is reviewed.\n")