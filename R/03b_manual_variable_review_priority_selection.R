# ============================================================
# Project: add-health-adolescent-risk-models
# Script 03b: Manual Variable Review and Priority Candidate Selection
# Author: Gelo Picol
#
# Purpose:
#   Reduce the broad documentation search results from Script 03
#   into a cleaner priority review table by thesis construct.
#
#   The script creates:
#     1. a construct-level priority decision table;
#     2. a top document/page review plan;
#     3. a variable candidate review template;
#     4. a manual review workbook for filling exact Add Health
#        variable names, labels and coding after codebook review.
#
# Important:
#   This script does not import Add Health microdata.
#   It does not import thesis data.
#   It does not publish long PDF text extracts.
# ============================================================


# ============================================================
# 0. Project root
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"


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
  "openxlsx"
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


# ============================================================
# 2. Define paths
# ============================================================

outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir   <- file.path(project_root, "outputs/diagnostics")
docs_dir           <- file.path(project_root, "docs")

dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 3. Input files from Script 03
# ============================================================

priority_constructs_path <- file.path(
  outputs_tables_dir,
  "priority_constructs_script03.csv"
)

construct_status_path <- file.path(
  outputs_tables_dir,
  "construct_mapping_status_script03.csv"
)

manual_review_path <- file.path(
  outputs_tables_dir,
  "manual_variable_review_worksheet_script03.csv"
)

keyword_hits_path <- file.path(
  outputs_tables_dir,
  "add_health_codebook_keyword_hits_script03.csv"
)

required_inputs <- c(
  priority_constructs_path,
  construct_status_path,
  manual_review_path
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

priority_constructs <- read_csv(
  priority_constructs_path,
  show_col_types = FALSE
)

construct_mapping_status <- read_csv(
  construct_status_path,
  show_col_types = FALSE
)

manual_review_worksheet <- read_csv(
  manual_review_path,
  show_col_types = FALSE
)

has_keyword_hits_file <- file.exists(keyword_hits_path)

if (has_keyword_hits_file) {
  keyword_hits <- read_csv(
    keyword_hits_path,
    show_col_types = FALSE
  )
} else {
  keyword_hits <- tibble()
}


# ============================================================
# 4. Helper functions
# ============================================================

safe_as_integer <- function(x) {
  suppressWarnings(as.integer(x))
}

count_pages <- function(pages_string) {
  if (is.na(pages_string) || pages_string == "") {
    return(0L)
  }

  pages <- str_split(pages_string, ",\\s*")[[1]]
  pages <- pages[pages != ""]
  length(unique(pages))
}

truncate_text <- function(x, max_chars = 160) {
  if (is.na(x) || x == "") {
    return(NA_character_)
  }

  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)

  if (nchar(x) <= max_chars) {
    return(x)
  }

  paste0(substr(x, 1, max_chars), "...")
}

classify_equivalence_strategy <- function(block) {
  block_lower <- str_to_lower(block)

  case_when(
    str_detect(block_lower, "sample alignment") ~ "Direct sample restriction",
    str_detect(block_lower, "demographics") ~ "Direct control variable",
    str_detect(block_lower, "socioeconomic") ~ "Proxy or index construction",
    str_detect(block_lower, "sexual initiation") ~ "Direct or near-direct behavioural outcome",
    str_detect(block_lower, "multiple partners") ~ "Direct or near-direct behavioural outcome",
    str_detect(block_lower, "condom") ~ "Direct or near-direct protective behaviour",
    str_detect(block_lower, "contraceptive") ~ "Direct or near-direct protective behaviour",
    str_detect(block_lower, "pregnancy") ~ "Direct or near-direct reproductive health outcome",
    str_detect(block_lower, "hiv|sti") ~ "Direct, proxy or related health outcome depending on public-use availability",
    str_detect(block_lower, "knowledge") ~ "Construct-level index or item subset",
    str_detect(block_lower, "susceptibility|severity|benefits|barriers") ~ "Partial construct equivalence likely",
    str_detect(block_lower, "self-efficacy") ~ "Construct-level equivalence likely",
    str_detect(block_lower, "attitudes") ~ "Construct-level equivalence likely",
    str_detect(block_lower, "peer") ~ "Construct-level equivalence likely; check public-use restrictions",
    str_detect(block_lower, "family") ~ "Construct-level equivalence likely",
    str_detect(block_lower, "school") ~ "Construct-level equivalence likely",
    TRUE ~ "To be reviewed"
  )
}

classify_expected_use <- function(block) {
  block_lower <- str_to_lower(block)

  case_when(
    str_detect(block_lower, "sample alignment") ~ "Sample restriction",
    str_detect(block_lower, "demographics|socioeconomic") ~ "Control or stratification",
    str_detect(block_lower, "sexual initiation|multiple partners|condom|contraceptive|pregnancy|hiv|sti") ~ "Outcome or key behavioural variable",
    str_detect(block_lower, "susceptibility|severity|benefits|barriers|knowledge") ~ "Health Belief Model construct",
    str_detect(block_lower, "self-efficacy|attitudes|peer") ~ "Theory of Planned Behaviour construct",
    str_detect(block_lower, "family|school") ~ "Socioecological predictor or mediator",
    TRUE ~ "To be reviewed"
  )
}

classify_manual_priority <- function(mapping_priority, status, total_hits) {
  case_when(
    mapping_priority == "Critical" & str_detect(status, "strong") ~ "1 - Immediate review",
    mapping_priority == "Critical" ~ "1 - Immediate review",
    mapping_priority == "High" & total_hits >= 3 ~ "2 - High priority",
    mapping_priority == "High" ~ "2 - High priority",
    TRUE ~ "3 - Secondary review"
  )
}

classify_decision_rule <- function(block) {
  block_lower <- str_to_lower(block)

  case_when(
    str_detect(block_lower, "sample alignment") ~
      "Accept only if variable directly identifies grade or age at Wave I.",
    str_detect(block_lower, "pregnancy") ~
      "Accept only if pregnancy timing, pregnancy history or pregnancy outcome can be linked to eligible respondents.",
    str_detect(block_lower, "hiv|sti") ~
      "Accept if public-use files include diagnosis, testing, reported infection or related STI/HIV indicator; otherwise use proxy cautiously.",
    str_detect(block_lower, "condom") ~
      "Accept if variable measures actual condom use, consistent use, intention, attitude or self-efficacy.",
    str_detect(block_lower, "contraceptive") ~
      "Accept if variable measures contraceptive use, method use, intention or access.",
    str_detect(block_lower, "family") ~
      "Accept if variable measures parental monitoring, support, communication or family connectedness.",
    str_detect(block_lower, "school") ~
      "Accept if variable measures school belonging, attachment, teacher support or school climate.",
    str_detect(block_lower, "peer") ~
      "Accept if variable measures peer norms or perceived peer behaviour, without restricted network identifiers.",
    TRUE ~
      "Accept only after confirming construct meaning, wave, coding and public-use status."
  )
}


# ============================================================
# 5. Construct-level priority decision table
# ============================================================

construct_priority_decision <- construct_mapping_status %>%
  left_join(
    priority_constructs %>%
      select(
        priority_id,
        core_keywords,
        mapping_rationale
      ),
    by = "priority_id"
  ) %>%
  mutate(
    equivalence_strategy = vapply(
      thesis_construct_block,
      classify_equivalence_strategy,
      character(1)
    ),
    expected_analytic_use = vapply(
      thesis_construct_block,
      classify_expected_use,
      character(1)
    ),
    manual_review_priority = mapply(
      classify_manual_priority,
      mapping_priority,
      status,
      total_keyword_hits
    ),
    decision_rule = vapply(
      thesis_construct_block,
      classify_decision_rule,
      character(1)
    ),
    recommended_number_of_variables = case_when(
      str_detect(str_to_lower(thesis_construct_block), "sample alignment") ~ "1-2",
      str_detect(str_to_lower(thesis_construct_block), "demographics") ~ "2-4",
      str_detect(str_to_lower(thesis_construct_block), "socioeconomic") ~ "3-6, possibly index",
      str_detect(str_to_lower(thesis_construct_block), "knowledge") ~ "3-10, possibly index",
      str_detect(str_to_lower(thesis_construct_block), "susceptibility|severity|benefits|barriers|self-efficacy|attitudes|peer|family|school") ~ "2-6, construct score if items are consistent",
      TRUE ~ "1-4"
    ),
    final_status_after_script03b = "pending_manual_codebook_review"
  ) %>%
  select(
    priority_id,
    thesis_construct_block,
    thesis_item_codes,
    theoretical_model,
    add_health_target_domain,
    expected_wave_priority,
    mapping_priority,
    total_keyword_hits,
    n_documents_with_hits,
    status,
    manual_review_priority,
    equivalence_strategy,
    expected_analytic_use,
    recommended_number_of_variables,
    core_keywords,
    mapping_rationale,
    decision_rule,
    recommended_action,
    final_status_after_script03b
  ) %>%
  arrange(
    manual_review_priority,
    priority_id
  )


# ============================================================
# 6. Candidate document/page review plan
# ============================================================

manual_review_clean <- manual_review_worksheet %>%
  mutate(
    n_pages = vapply(matched_pages, count_pages, integer(1)),
    has_document = !is.na(file_name) & file_name != "",
    review_priority_score = case_when(
      mapping_priority == "Critical" & has_document ~ 100,
      mapping_priority == "High" & has_document ~ 80,
      has_document ~ 60,
      TRUE ~ 20
    ) + pmin(n_pages, 10),
    review_priority_class = case_when(
      review_priority_score >= 100 ~ "1 - Immediate page review",
      review_priority_score >= 80 ~ "2 - High page review",
      review_priority_score >= 60 ~ "3 - Secondary page review",
      TRUE ~ "4 - Manual external search"
    )
  )

candidate_page_review_plan <- manual_review_clean %>%
  filter(has_document) %>%
  group_by(
    priority_id,
    thesis_construct_block,
    thesis_item_codes,
    theoretical_model,
    add_health_target_domain,
    expected_wave_priority,
    mapping_priority,
    file_name
  ) %>%
  summarise(
    matched_pages = paste(sort(unique(na.omit(matched_pages))), collapse = " | "),
    matched_keywords = paste(sort(unique(na.omit(matched_keywords))), collapse = " | "),
    max_review_priority_score = max(review_priority_score, na.rm = TRUE),
    review_priority_class = first(review_priority_class[order(-review_priority_score)]),
    .groups = "drop"
  ) %>%
  mutate(
    manual_task = "Open the listed document and pages; confirm exact Add Health variable name, label, response categories and wave.",
    public_output_rule = "Do not publish PDF extracts; publish only variable names, labels and aggregate methodological notes after confirmation."
  ) %>%
  arrange(
    priority_id,
    desc(max_review_priority_score),
    file_name
  )


# ============================================================
# 7. Top candidate pages by construct
# ============================================================
# Keep at most two documents per construct for practical manual review.

top_candidate_pages_by_construct <- candidate_page_review_plan %>%
  group_by(priority_id) %>%
  slice_max(
    order_by = max_review_priority_score,
    n = 2,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  arrange(priority_id, desc(max_review_priority_score))


# ============================================================
# 8. Use keyword hits if available to identify concentrated pages
# ============================================================
# This section uses the raw keyword hit file only locally. It does
# not export context snippets.

if (has_keyword_hits_file && nrow(keyword_hits) > 0) {

  keyword_page_density <- keyword_hits %>%
    mutate(
      page_number = safe_as_integer(page_number)
    ) %>%
    filter(!is.na(page_number)) %>%
    group_by(
      priority_id,
      thesis_construct_block,
      thesis_item_codes,
      theoretical_model,
      add_health_target_domain,
      expected_wave_priority,
      mapping_priority,
      file_name,
      page_number
    ) %>%
    summarise(
      n_hits_on_page = n(),
      matched_keywords = paste(sort(unique(keyword)), collapse = "; "),
      .groups = "drop"
    ) %>%
    group_by(priority_id, file_name) %>%
    arrange(desc(n_hits_on_page), page_number, .by_group = TRUE) %>%
    mutate(
      page_rank_within_document = row_number()
    ) %>%
    ungroup() %>%
    filter(page_rank_within_document <= 5) %>%
    mutate(
      manual_review_action = "Review this page first; keyword density suggests possible relevant variable entries.",
      context_exported = "No"
    ) %>%
    arrange(priority_id, file_name, page_rank_within_document)

} else {

  keyword_page_density <- tibble(
    priority_id = integer(),
    thesis_construct_block = character(),
    thesis_item_codes = character(),
    theoretical_model = character(),
    add_health_target_domain = character(),
    expected_wave_priority = character(),
    mapping_priority = character(),
    file_name = character(),
    page_number = integer(),
    n_hits_on_page = integer(),
    matched_keywords = character(),
    page_rank_within_document = integer(),
    manual_review_action = character(),
    context_exported = character()
  )
}


# ============================================================
# 9. Priority candidate variable template
# ============================================================
# This is the key table to be filled manually.

priority_candidate_variables_template <- construct_priority_decision %>%
  transmute(
    priority_id,
    thesis_construct_block,
    thesis_item_codes,
    theoretical_model,
    add_health_target_domain,
    expected_analytic_use,
    equivalence_strategy,
    expected_wave_priority,
    mapping_priority,
    manual_review_priority,
    recommended_number_of_variables,
    suggested_documents_to_review = NA_character_,
    suggested_pages_to_review = NA_character_,
    add_health_wave = NA_character_,
    add_health_document = NA_character_,
    add_health_variable_name = NA_character_,
    add_health_variable_label = NA_character_,
    add_health_response_categories = NA_character_,
    proposed_variable_type = NA_character_,
    proposed_coding_direction = NA_character_,
    public_use_status = "to_be_verified",
    mapping_quality = "pending_manual_review",
    final_decision = "pending_manual_review",
    decision_rule,
    reviewer_notes = NA_character_
  )

# Fill suggested documents/pages from top candidate table
suggestions <- top_candidate_pages_by_construct %>%
  group_by(priority_id) %>%
  summarise(
    suggested_documents_to_review = paste(unique(file_name), collapse = " | "),
    suggested_pages_to_review = paste(unique(matched_pages), collapse = " | "),
    .groups = "drop"
  )

priority_candidate_variables_template <- priority_candidate_variables_template %>%
  select(-suggested_documents_to_review, -suggested_pages_to_review) %>%
  left_join(suggestions, by = "priority_id") %>%
  relocate(
    suggested_documents_to_review,
    suggested_pages_to_review,
    .after = recommended_number_of_variables
  ) %>%
  mutate(
    suggested_documents_to_review = ifelse(
      is.na(suggested_documents_to_review),
      "Use ICPSR Variables and Add Health Codebook Explorer",
      suggested_documents_to_review
    ),
    suggested_pages_to_review = ifelse(
      is.na(suggested_pages_to_review),
      "No local page signal; search manually",
      suggested_pages_to_review
    )
  )


# ============================================================
# 10. Initial feasibility classification
# ============================================================

initial_feasibility_assessment <- construct_priority_decision %>%
  mutate(
    feasibility_rating = case_when(
      str_detect(str_to_lower(thesis_construct_block), "sample alignment|demographics|sexual initiation|condom|contraceptive|pregnancy|family monitoring|school") &
        total_keyword_hits >= 3 ~ "High feasibility",
      total_keyword_hits >= 10 ~ "High feasibility",
      total_keyword_hits >= 3 ~ "Moderate feasibility",
      total_keyword_hits >= 1 ~ "Low to moderate feasibility",
      TRUE ~ "Uncertain"
    ),
    feasibility_comment = case_when(
      feasibility_rating == "High feasibility" ~
        "Strong documentation signal. Manual codebook review should focus on exact variable names and coding.",
      feasibility_rating == "Moderate feasibility" ~
        "Some documentation signal. Construct equivalence should be checked carefully.",
      feasibility_rating == "Low to moderate feasibility" ~
        "Limited documentation signal. Manual search in ICPSR Variables and ACE is required.",
      TRUE ~
        "No clear local documentation signal. External manual search is required."
    )
  ) %>%
  select(
    priority_id,
    thesis_construct_block,
    thesis_item_codes,
    add_health_target_domain,
    expected_wave_priority,
    total_keyword_hits,
    n_documents_with_hits,
    status,
    feasibility_rating,
    feasibility_comment,
    decision_rule
  ) %>%
  arrange(priority_id)


# ============================================================
# 11. Script 03b methodological notes
# ============================================================

script03b_methodological_notes <- tibble(
  note_id = 1:10,
  note = c(
    "Script 03b reduces broad keyword hits into a manual review plan.",
    "The script does not import Add Health microdata or thesis data.",
    "The script does not export long PDF text snippets.",
    "The main output is a candidate variable template to be filled manually after reading the codebook pages.",
    "Construct equivalence is classified as direct, near-direct, proxy, index-based or partial.",
    "Grade 10-12 remains the primary sample alignment criterion.",
    "Age 15-19 remains a complementary alignment or sensitivity criterion.",
    "Variables involving peers, schools, partners or networks require careful public-use verification.",
    "Sensitive outcomes must be reported only in aggregate form in later scripts.",
    "The next stage should fill exact Add Health variable names, labels and response categories for the highest-priority constructs."
  )
)


# ============================================================
# 12. Execution checklist
# ============================================================

script03b_checklist <- tibble(
  check_id = 1:15,
  check_item = c(
    "Project root exists",
    "Priority constructs input loaded",
    "Construct mapping status input loaded",
    "Manual review worksheet input loaded",
    "Optional keyword hits file checked",
    "Construct priority decision table created",
    "Candidate page review plan created",
    "Top candidate pages by construct created",
    "Keyword page density table created",
    "Priority candidate variable template created",
    "Initial feasibility assessment created",
    "Methodological notes created",
    "CSV outputs exported",
    "Excel workbook exported",
    "Markdown documentation exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(priority_constructs_path), "OK", "FAIL"),
    ifelse(file.exists(construct_status_path), "OK", "FAIL"),
    ifelse(file.exists(manual_review_path), "OK", "FAIL"),
    ifelse(has_keyword_hits_file, "OK", "WARNING_KEYWORD_HITS_FILE_NOT_FOUND"),
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "PENDING",
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 13. Export CSV outputs
# ============================================================

write_csv(
  construct_priority_decision,
  file.path(outputs_tables_dir, "construct_priority_decision_script03b.csv")
)

write_csv(
  candidate_page_review_plan,
  file.path(outputs_tables_dir, "candidate_page_review_plan_script03b.csv")
)

write_csv(
  top_candidate_pages_by_construct,
  file.path(outputs_tables_dir, "top_candidate_pages_by_construct_script03b.csv")
)

write_csv(
  keyword_page_density,
  file.path(outputs_tables_dir, "keyword_page_density_script03b.csv")
)

write_csv(
  priority_candidate_variables_template,
  file.path(outputs_tables_dir, "add_health_priority_candidate_variables_template_script03b.csv")
)

write_csv(
  initial_feasibility_assessment,
  file.path(outputs_tables_dir, "initial_feasibility_assessment_script03b.csv")
)

write_csv(
  script03b_methodological_notes,
  file.path(outputs_tables_dir, "script03b_methodological_notes.csv")
)

script03b_checklist$status[
  script03b_checklist$check_item == "CSV outputs exported"
] <- "OK"


# ============================================================
# 14. Export Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script03b_priority_candidate_variables.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "construct_priority")
writeData(wb, "construct_priority", construct_priority_decision)

addWorksheet(wb, "candidate_pages")
writeData(wb, "candidate_pages", candidate_page_review_plan)

addWorksheet(wb, "top_pages")
writeData(wb, "top_pages", top_candidate_pages_by_construct)

addWorksheet(wb, "page_density")
writeData(wb, "page_density", keyword_page_density)

addWorksheet(wb, "candidate_variables")
writeData(wb, "candidate_variables", priority_candidate_variables_template)

addWorksheet(wb, "feasibility")
writeData(wb, "feasibility", initial_feasibility_assessment)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script03b_methodological_notes)

addWorksheet(wb, "script03b_checklist")
writeData(wb, "script03b_checklist", script03b_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:40, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script03b_checklist$status[
  script03b_checklist$check_item == "Excel workbook exported"
] <- "OK"


# ============================================================
# 15. Export Markdown documentation
# ============================================================

script03b_note <- c(
  "# Manual Variable Review and Priority Candidate Selection",
  "",
  "Script 03b reduces the broad keyword search from Script 03 into a structured manual review plan.",
  "",
  "This script does not import Add Health microdata and does not use individual-level thesis data.",
  "",
  "## Main outputs",
  "",
  "- construct-level priority decision table;",
  "- candidate document/page review plan;",
  "- top candidate pages by construct;",
  "- priority candidate variable template;",
  "- initial feasibility assessment.",
  "",
  "## Interpretation",
  "",
  "The outputs do not yet provide final Add Health variable names.",
  "",
  "They identify which documents and pages should be reviewed first to confirm exact variable names, labels, response categories and public-use status.",
  "",
  "## Next step",
  "",
  "Open the workbook `outputs/tables/script03b_priority_candidate_variables.xlsx` and complete the `candidate_variables` sheet manually for the highest-priority constructs.",
  "",
  "Priority constructs include sample alignment, condom use, contraception, pregnancy, HIV/STI indicators, sexual initiation, family monitoring, peer norms and school connectedness."
)

writeLines(
  script03b_note,
  con = file.path(docs_dir, "manual_variable_review_priority_selection_script03b.md")
)

script03b_checklist$status[
  script03b_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 16. Save final checklist
# ============================================================

write_csv(
  script03b_checklist,
  file.path(outputs_diag_dir, "script03b_execution_checklist.csv")
)


# ============================================================
# 17. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 03b completed: Manual Variable Review and Priority Candidate Selection\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Input status:\n")
cat("- Priority constructs loaded: ", nrow(priority_constructs), "\n", sep = "")
cat("- Construct mapping status rows: ", nrow(construct_mapping_status), "\n", sep = "")
cat("- Manual review worksheet rows: ", nrow(manual_review_worksheet), "\n", sep = "")
cat("- Raw keyword hits file available locally: ", ifelse(has_keyword_hits_file, "Yes", "No"), "\n\n", sep = "")

cat("Main outputs created:\n")
cat("- outputs/tables/construct_priority_decision_script03b.csv\n")
cat("- outputs/tables/candidate_page_review_plan_script03b.csv\n")
cat("- outputs/tables/top_candidate_pages_by_construct_script03b.csv\n")
cat("- outputs/tables/keyword_page_density_script03b.csv\n")
cat("- outputs/tables/add_health_priority_candidate_variables_template_script03b.csv\n")
cat("- outputs/tables/initial_feasibility_assessment_script03b.csv\n")
cat("- outputs/tables/script03b_methodological_notes.csv\n")
cat("- outputs/tables/script03b_priority_candidate_variables.xlsx\n")
cat("- outputs/diagnostics/script03b_execution_checklist.csv\n")
cat("- docs/manual_variable_review_priority_selection_script03b.md\n\n")

cat("Feasibility summary:\n")
print(
  initial_feasibility_assessment %>%
    count(feasibility_rating, name = "n_constructs") %>%
    arrange(desc(n_constructs))
)

cat("\nManual review priorities:\n")
print(
  construct_priority_decision %>%
    count(manual_review_priority, name = "n_constructs") %>%
    arrange(manual_review_priority)
)

cat("\nExecution checklist:\n")
print(script03b_checklist)

cat("\nImportant note:\n")
cat("The candidate variable template must be manually completed before importing Add Health public-use microdata.\n\n")