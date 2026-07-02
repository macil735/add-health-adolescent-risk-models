# ============================================================
# Project: add-health-adolescent-risk-models
# Script 03: Add Health Codebook Variable Candidate Mapping
# Author: Gelo Picol
#
# Purpose:
#   Search locally stored Add Health public-use codebooks and
#   documentation for keywords derived from the doctoral thesis
#   questionnaire construct inventory.
#
#   Produce a structured candidate mapping table:
#     thesis construct
#     -> search keywords
#     -> Add Health document
#     -> wave/document source
#     -> page hits
#     -> review priority
#     -> candidate mapping status
#
# Important:
#   This script does not import, process, or export individual-level
#   Add Health microdata or thesis data.
#
#   It works only with documentation files stored in data/metadata/.
# ============================================================


# ============================================================
# 0. Project root
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"


# ============================================================
# 1. Required and optional packages
# ============================================================

required_packages <- c(
  "dplyr",
  "tibble",
  "readr",
  "stringr",
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
library(purrr)
library(openxlsx)

has_pdftools <- requireNamespace("pdftools", quietly = TRUE)

if (!has_pdftools) {
  message(
    "Optional package 'pdftools' is not installed. ",
    "The script will still run, but PDF text search will be skipped. ",
    "To enable PDF search, install it with install.packages('pdftools')."
  )
}


# ============================================================
# 2. Define paths
# ============================================================

metadata_dir <- file.path(project_root, "data/metadata")
codebook_dir <- file.path(metadata_dir, "add_health_codebooks")

outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir <- file.path(project_root, "outputs/diagnostics")
docs_dir <- file.path(project_root, "docs")

dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 3. Load previous outputs
# ============================================================

search_terms_path <- file.path(
  outputs_tables_dir,
  "add_health_search_terms_script01b.csv"
)

documentation_inventory_path <- file.path(
  outputs_tables_dir,
  "add_health_documentation_inventory_script02.csv"
)

mapping_template_path <- file.path(
  outputs_tables_dir,
  "add_health_mapping_template_updated_script02.csv"
)

if (file.exists(search_terms_path)) {
  add_health_search_terms <- read_csv(search_terms_path, show_col_types = FALSE)
} else {
  stop("Missing file: outputs/tables/add_health_search_terms_script01b.csv")
}

if (file.exists(documentation_inventory_path)) {
  documentation_inventory <- read_csv(documentation_inventory_path, show_col_types = FALSE)
} else {
  stop("Missing file: outputs/tables/add_health_documentation_inventory_script02.csv")
}

if (file.exists(mapping_template_path)) {
  add_health_mapping_template <- read_csv(mapping_template_path, show_col_types = FALSE)
} else {
  add_health_mapping_template <- tibble(
    status = "Mapping template from Script 02 not found"
  )
}


# ============================================================
# 4. Identify local Add Health codebooks
# ============================================================

local_codebooks <- documentation_inventory %>%
  filter(source_group == "add_health_codebooks") %>%
  mutate(
    full_path = file.path(project_root, relative_path),
    file_exists = file.exists(full_path)
  ) %>%
  select(
    file_name,
    relative_path,
    full_path,
    file_extension,
    file_size_kb,
    document_type,
    wave_coverage,
    period_coverage,
    access_status,
    project_relevance,
    github_policy,
    review_status,
    file_exists
  )

has_local_codebooks <- nrow(local_codebooks) > 0 &&
  any(local_codebooks$file_exists)


# ============================================================
# 5. Manual high-priority construct map
# ============================================================
# This table is used to prioritise the keyword search.
# It does not contain Add Health variable names yet.
# Variable names will be filled only after verifying codebook pages.

priority_constructs <- tribble(
  ~priority_id, ~thesis_construct_block, ~thesis_item_codes, ~theoretical_model, ~add_health_target_domain, ~core_keywords, ~expected_wave_priority, ~mapping_priority, ~mapping_rationale,

  1, "Sample alignment", "GRADE", "Socioecological model", "School grade at Wave I", "grade; school grade; current grade; 10th grade; 11th grade; 12th grade", "Wave I", "Critical", "Required to restrict the Add Health sample to grades 10-12.",
  2, "Sample alignment", "AGE", "Socioecological model", "Age at Wave I", "age; age at interview; date of birth", "Wave I", "Critical", "Required for complementary 15-19 age alignment.",
  3, "Demographics", "GEN", "Socioecological model", "Sex or gender", "sex; gender; male; female", "Wave I", "High", "Core control and heterogeneity variable.",
  4, "Household socioeconomic status", "AFL1-AFL4", "Socioecological model", "Household assets and family socioeconomic status", "car; vehicle; own room; computer; household; parent education; income", "Wave I", "High", "Required to approximate socioeconomic position.",
  5, "Sexual initiation", "SEX_EVER; ISX1", "Behavioural risk model", "Ever had sex and age at first sex", "ever had sex; sexual intercourse; first sex; age first sex; age at first intercourse", "Wave I to Wave III", "Critical", "Central behavioural outcome and risk indicator.",
  6, "Multiple partners", "ISX2", "Behavioural risk model", "Number of sexual partners", "number of partners; sexual partners; partners in past year", "Wave I to Wave III", "High", "Main sexual risk indicator.",
  7, "Condom use", "ISX3; APC; AEF; IPC", "Health Belief Model / Theory of Planned Behaviour", "Condom use and intention to use condom", "condom; condom use; used condom; use condoms; always use condom", "Wave I to Wave III", "Critical", "Central protective behaviour in the thesis.",
  8, "Contraceptive use", "ISX3; APC; EPC; IPC", "Theory of Planned Behaviour", "Contraception and birth control", "contraception; contraceptive; birth control; pill; method", "Wave I to Wave III", "Critical", "Central pregnancy-prevention construct.",
  9, "Pregnancy outcome", "Pregnancy-related outcome", "Behavioural risk / reproductive health", "Pregnancy history or pregnancy outcome", "pregnant; pregnancy; got pregnant; pregnancy outcome; birth", "Wave I to Wave IV", "Critical", "Key thesis outcome related to early pregnancy.",
  10, "HIV/STI outcome", "HIV-related outcome", "Health Belief Model / reproductive health", "HIV, AIDS, STI, STD testing or diagnosis", "HIV; AIDS; STI; STD; sexually transmitted; test; diagnosis", "Wave I to Wave III", "Critical", "Key thesis outcome or related health indicator.",
  11, "HIV/AIDS knowledge", "INA1-INA4; CVS1-CVS9", "Health Belief Model", "HIV/AIDS knowledge and information exposure", "AIDS knowledge; HIV knowledge; transmission; prevention; sex education; AIDS education", "Wave I to Wave III", "High", "Knowledge was a core construct in the thesis.",
  12, "Perceived susceptibility", "SUP1-SUP4", "Health Belief Model", "Perceived HIV/STI risk", "risk of AIDS; risk of HIV; worried; worry; chance of getting AIDS; chance of getting HIV", "Wave I to Wave III", "High", "Core MCS construct.",
  13, "Perceived severity", "SEP1-SEP4", "Health Belief Model", "Perceived seriousness of HIV/AIDS", "serious; seriousness; AIDS is serious; HIV is serious; death", "Wave I to Wave III", "Medium", "Core MCS construct, but direct equivalence may be limited.",
  14, "Perceived benefits", "BEP1-BEP4", "Health Belief Model", "Benefits of condom use or prevention", "condom prevents; prevent AIDS; prevent HIV; safer sex; abstain", "Wave I to Wave III", "High", "Core MCS construct.",
  15, "Perceived barriers", "BAP1-BAP3", "Health Belief Model", "Barriers to condom use or prevention", "embarrassed; embarrassment; difficult to buy condom; partner refuses; less pleasure; access", "Wave I to Wave III", "High", "Core MCS construct.",
  16, "Self-efficacy", "AEF1-AEF7; EPC1-EPC3; EIS1-EIS3", "Theory of Planned Behaviour / Health Belief Model", "Condom, contraception and refusal self-efficacy", "self efficacy; sure could; could use condom; refuse sex; say no; convince partner", "Wave I to Wave III", "Critical", "Central bridge construct in the thesis.",
  17, "Attitudes", "AIS1-AIS3; APC1-APC3", "Theory of Planned Behaviour", "Attitudes toward sex, condom and contraception", "attitude; believe; wrong to have sex; should use condom; should use birth control", "Wave I to Wave III", "High", "Core TCP construct.",
  18, "Peer norms", "NPP1-NPP3; NP4-NP6", "Theory of Planned Behaviour / Socioecological model", "Peer norms and peer sexual behaviour", "friends; peers; peer pressure; friends had sex; friends use condoms; friends think", "Wave I to Wave III", "Critical", "Central TCP and socioecological construct.",
  19, "Family monitoring", "LFA1-LFA2", "Socioecological model", "Parental monitoring", "parents know; know where; know who; whereabouts; parents know friends", "Wave I", "Critical", "Strong expected equivalence with Add Health family items.",
  20, "Family support", "LFA3", "Socioecological model", "Family support or connectedness", "parents care; family support; talk to parents; close to mother; close to father; help with problems", "Wave I", "High", "Important contextual predictor and mediation source.",
  21, "School connectedness", "Q01-Q07", "Socioecological model", "School connectedness and school climate", "school connectedness; feel part of school; teachers care; school belonging; school climate", "Wave I to Wave II", "Critical", "Important contextual construct from the qualitative component.",
  22, "Sex education", "INA4; Q07", "Socioecological model / Health Belief Model", "School-based sex education and health information", "sex education; AIDS education; HIV education; taught about AIDS; school taught", "Wave I to Wave III", "High", "Links school context to HIV and pregnancy prevention."
)


# ============================================================
# 6. Helper functions for keyword search
# ============================================================

split_keywords <- function(keyword_string) {
  keyword_string %>%
    str_split(";") %>%
    unlist() %>%
    str_trim() %>%
    discard(~ .x == "")
}

safe_regex <- function(keyword) {
  keyword %>%
    str_replace_all("([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}\\\\])", "\\\\\\1")
}

extract_context <- function(text, keyword, width = 180) {
  text_lower <- str_to_lower(text)
  keyword_lower <- str_to_lower(keyword)

  pos <- str_locate(text_lower, fixed(keyword_lower))[1, ]

  if (any(is.na(pos))) {
    return(NA_character_)
  }

  start <- max(1, pos[1] - width)
  end <- min(nchar(text), pos[2] + width)

  context <- substr(text, start, end)
  context <- str_replace_all(context, "\\s+", " ")
  context <- str_trim(context)

  context
}

search_pdf_for_keywords <- function(pdf_path, construct_row) {

  if (!has_pdftools) {
    return(tibble())
  }

  if (!file.exists(pdf_path)) {
    return(tibble())
  }

  pdf_text <- tryCatch(
    pdftools::pdf_text(pdf_path),
    error = function(e) character()
  )

  if (length(pdf_text) == 0) {
    return(tibble())
  }

  keywords <- split_keywords(construct_row$core_keywords)

  results <- map_dfr(keywords, function(keyword) {

    pattern <- regex(safe_regex(keyword), ignore_case = TRUE)

    page_hits <- which(str_detect(pdf_text, pattern))

    if (length(page_hits) == 0) {
      return(tibble())
    }

    tibble(
      priority_id = construct_row$priority_id,
      thesis_construct_block = construct_row$thesis_construct_block,
      thesis_item_codes = construct_row$thesis_item_codes,
      theoretical_model = construct_row$theoretical_model,
      add_health_target_domain = construct_row$add_health_target_domain,
      expected_wave_priority = construct_row$expected_wave_priority,
      mapping_priority = construct_row$mapping_priority,
      keyword = keyword,
      page_number = page_hits,
      context_snippet = map_chr(
        page_hits,
        ~ extract_context(pdf_text[.x], keyword)
      )
    )
  })

  results
}


# ============================================================
# 7. Run keyword search across local codebooks
# ============================================================

if (has_local_codebooks && has_pdftools) {

  keyword_hits <- map_dfr(seq_len(nrow(local_codebooks)), function(i) {

    doc <- local_codebooks[i, ]

    map_dfr(seq_len(nrow(priority_constructs)), function(j) {

      construct_row <- priority_constructs[j, ]

      search_pdf_for_keywords(
        pdf_path = doc$full_path,
        construct_row = construct_row
      ) %>%
        mutate(
          file_name = doc$file_name,
          relative_path = doc$relative_path,
          document_wave_coverage = doc$wave_coverage,
          document_type = doc$document_type,
          access_status = doc$access_status
        )
    })
  })

} else {

  keyword_hits <- tibble(
    priority_id = integer(),
    thesis_construct_block = character(),
    thesis_item_codes = character(),
    theoretical_model = character(),
    add_health_target_domain = character(),
    expected_wave_priority = character(),
    mapping_priority = character(),
    keyword = character(),
    page_number = integer(),
    context_snippet = character(),
    file_name = character(),
    relative_path = character(),
    document_wave_coverage = character(),
    document_type = character(),
    access_status = character()
  )
}


# ============================================================
# 8. Summarise keyword hits into candidate mapping table
# ============================================================

if (nrow(keyword_hits) > 0) {

  candidate_mapping_summary <- keyword_hits %>%
    group_by(
      priority_id,
      thesis_construct_block,
      thesis_item_codes,
      theoretical_model,
      add_health_target_domain,
      expected_wave_priority,
      mapping_priority,
      file_name,
      relative_path,
      document_wave_coverage,
      access_status
    ) %>%
    summarise(
      n_keyword_hits = n(),
      matched_keywords = paste(sort(unique(keyword)), collapse = "; "),
      matched_pages = paste(sort(unique(page_number)), collapse = ", "),
      example_context = first(na.omit(context_snippet)),
      .groups = "drop"
    ) %>%
    mutate(
      candidate_variable_name = "to_be_confirmed_from_codebook_page",
      candidate_variable_label = "to_be_confirmed_from_codebook_page",
      public_use_status = case_when(
        str_detect(str_to_lower(access_status), "public") ~ "likely_public_use_documentation",
        TRUE ~ "to_be_verified"
      ),
      mapping_quality_initial = case_when(
        mapping_priority == "Critical" & n_keyword_hits >= 5 ~ "high_priority_strong_keyword_evidence",
        mapping_priority == "Critical" & n_keyword_hits >= 1 ~ "high_priority_some_keyword_evidence",
        mapping_priority == "High" & n_keyword_hits >= 3 ~ "medium_to_high_keyword_evidence",
        n_keyword_hits >= 1 ~ "weak_to_moderate_keyword_evidence",
        TRUE ~ "no_keyword_evidence"
      ),
      manual_review_required = "Yes",
      next_action = "Open the cited codebook pages and confirm exact Add Health variable name, label, coding and public-use status."
    ) %>%
    arrange(priority_id, file_name)

} else {

  candidate_mapping_summary <- priority_constructs %>%
    mutate(
      file_name = NA_character_,
      relative_path = NA_character_,
      document_wave_coverage = NA_character_,
      access_status = NA_character_,
      n_keyword_hits = 0L,
      matched_keywords = NA_character_,
      matched_pages = NA_character_,
      example_context = NA_character_,
      candidate_variable_name = "to_be_confirmed_from_codebook_page",
      candidate_variable_label = "to_be_confirmed_from_codebook_page",
      public_use_status = "to_be_verified",
      mapping_quality_initial = ifelse(
        has_pdftools,
        "no_keyword_evidence",
        "pdf_text_search_not_available"
      ),
      manual_review_required = "Yes",
      next_action = ifelse(
        has_pdftools,
        "No keyword hit found. Search manually in ICPSR Variables and Add Health Codebook Explorer.",
        "Install pdftools or search manually in ICPSR Variables and Add Health Codebook Explorer."
      )
    )
}


# ============================================================
# 9. Construct-level mapping status
# ============================================================

construct_mapping_status <- candidate_mapping_summary %>%
  group_by(
    priority_id,
    thesis_construct_block,
    thesis_item_codes,
    theoretical_model,
    add_health_target_domain,
    expected_wave_priority,
    mapping_priority
  ) %>%
  summarise(
    total_keyword_hits = sum(n_keyword_hits, na.rm = TRUE),
    n_documents_with_hits = n_distinct(file_name[!is.na(file_name)]),
    documents_with_hits = paste(sort(unique(file_name[!is.na(file_name)])), collapse = " | "),
    pages_with_hits = paste(sort(unique(unlist(str_split(na.omit(matched_pages), ",\\s*")))), collapse = ", "),
    status = case_when(
      total_keyword_hits >= 10 ~ "strong_documentary_signal",
      total_keyword_hits >= 3 ~ "moderate_documentary_signal",
      total_keyword_hits >= 1 ~ "weak_documentary_signal",
      !has_pdftools ~ "pdf_text_search_not_available",
      TRUE ~ "no_documentary_signal"
    ),
    recommended_action = case_when(
      total_keyword_hits >= 1 ~ "Review pages and manually confirm variable names and coding.",
      !has_pdftools ~ "Install pdftools or use ICPSR Variables/ACE manual search.",
      TRUE ~ "Search manually in ICPSR Variables and Add Health Codebook Explorer."
    ),
    .groups = "drop"
  ) %>%
  arrange(priority_id)


# ============================================================
# 10. Manual review worksheet
# ============================================================

manual_review_worksheet <- candidate_mapping_summary %>%
  transmute(
    priority_id,
    thesis_construct_block,
    thesis_item_codes,
    theoretical_model,
    add_health_target_domain,
    expected_wave_priority,
    mapping_priority,
    file_name,
    matched_pages,
    matched_keywords,
    candidate_variable_name,
    candidate_variable_label,
    add_health_response_categories = NA_character_,
    analytic_use = NA_character_,
    public_use_status,
    mapping_quality_initial,
    final_mapping_decision = "pending_manual_review",
    reviewer_notes = NA_character_
  ) %>%
  arrange(priority_id, file_name)


# ============================================================
# 11. Script 03 methodological notes
# ============================================================

script03_methodological_notes <- tibble(
  note_id = 1:10,
  note = c(
    "Script 03 performs documentation-based keyword search only.",
    "No Add Health microdata are imported, processed or exported.",
    "Keyword hits are not final variable matches. They identify pages requiring manual review.",
    "Exact Add Health variable names must be confirmed from codebook pages, ICPSR Variables or ACE.",
    "Construct equivalence is more important than literal wording equivalence.",
    "The strongest candidates are expected for grade, age, sexual initiation, condom use, contraception, pregnancy, family monitoring and school connectedness.",
    "Perceived susceptibility, severity, benefits and barriers may have partial or indirect Add Health equivalents.",
    "Variables related to friends, partners, schools or networks must be checked carefully because some Add Health identifiers are restricted.",
    "Final public outputs must never include raw identifiers or individual-level responses.",
    "The next stage should manually review high-priority hits and fill candidate variable names and labels."
  )
)


# ============================================================
# 12. Execution checklist
# ============================================================

script03_checklist <- tibble(
  check_id = 1:14,
  check_item = c(
    "Project root exists",
    "Metadata directory exists",
    "Codebook directory exists",
    "Local codebooks identified",
    "pdftools availability checked",
    "Search terms from Script 01b loaded",
    "Documentation inventory from Script 02 loaded",
    "Priority construct table created",
    "Keyword hits table created",
    "Candidate mapping summary created",
    "Construct mapping status created",
    "Manual review worksheet created",
    "CSV outputs exported",
    "Excel workbook exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(dir.exists(metadata_dir), "OK", "FAIL"),
    ifelse(dir.exists(codebook_dir), "OK", "FAIL"),
    ifelse(has_local_codebooks, "OK", "WARNING_NO_LOCAL_CODEBOOKS"),
    ifelse(has_pdftools, "OK", "WARNING_PDFTOOLS_NOT_INSTALLED"),
    ifelse(file.exists(search_terms_path), "OK", "FAIL"),
    ifelse(file.exists(documentation_inventory_path), "OK", "FAIL"),
    "OK",
    ifelse(nrow(keyword_hits) > 0, "OK", "WARNING_NO_KEYWORD_HITS"),
    "OK",
    "OK",
    "OK",
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 13. Export CSV outputs
# ============================================================

write_csv(
  local_codebooks,
  file.path(outputs_tables_dir, "add_health_local_codebooks_script03.csv")
)

write_csv(
  priority_constructs,
  file.path(outputs_tables_dir, "priority_constructs_script03.csv")
)

write_csv(
  keyword_hits,
  file.path(outputs_tables_dir, "add_health_codebook_keyword_hits_script03.csv")
)

write_csv(
  candidate_mapping_summary,
  file.path(outputs_tables_dir, "add_health_candidate_mapping_summary_script03.csv")
)

write_csv(
  construct_mapping_status,
  file.path(outputs_tables_dir, "construct_mapping_status_script03.csv")
)

write_csv(
  manual_review_worksheet,
  file.path(outputs_tables_dir, "manual_variable_review_worksheet_script03.csv")
)

write_csv(
  script03_methodological_notes,
  file.path(outputs_tables_dir, "script03_methodological_notes.csv")
)

script03_checklist$status[
  script03_checklist$check_item == "CSV outputs exported"
] <- "OK"


# ============================================================
# 14. Export Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script03_add_health_codebook_variable_candidate_mapping.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "local_codebooks")
writeData(wb, "local_codebooks", local_codebooks)

addWorksheet(wb, "priority_constructs")
writeData(wb, "priority_constructs", priority_constructs)

addWorksheet(wb, "keyword_hits")
writeData(wb, "keyword_hits", keyword_hits)

addWorksheet(wb, "candidate_mapping")
writeData(wb, "candidate_mapping", candidate_mapping_summary)

addWorksheet(wb, "construct_status")
writeData(wb, "construct_status", construct_mapping_status)

addWorksheet(wb, "manual_review")
writeData(wb, "manual_review", manual_review_worksheet)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script03_methodological_notes)

addWorksheet(wb, "script03_checklist")
writeData(wb, "script03_checklist", script03_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:40, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script03_checklist$status[
  script03_checklist$check_item == "Excel workbook exported"
] <- "OK"


# ============================================================
# 15. Export Markdown documentation
# ============================================================

script03_note <- c(
  "# Add Health Codebook Variable Candidate Mapping",
  "",
  "Script 03 searches locally stored Add Health codebooks for keywords derived from the doctoral thesis questionnaire construct inventory.",
  "",
  "This script does not use individual-level thesis data or Add Health microdata.",
  "",
  "## Main purpose",
  "",
  "The purpose is to identify candidate pages and documents that may contain Add Health variables equivalent to the thesis constructs.",
  "",
  "## Important warning",
  "",
  "Keyword hits are not final variable matches.",
  "",
  "All candidate variables must be manually confirmed in the codebook, ICPSR Variables or Add Health Codebook Explorer.",
  "",
  "## Current PDF text search status",
  "",
  paste0("- Local codebooks available: ", ifelse(has_local_codebooks, "yes", "no")),
  paste0("- pdftools available: ", ifelse(has_pdftools, "yes", "no")),
  paste0("- Keyword hits found: ", nrow(keyword_hits)),
  "",
  "## Next action",
  "",
  "Review the `manual_review` worksheet in the Script 03 Excel workbook and fill the exact Add Health variable names, labels, response categories and final mapping decision."
)

writeLines(
  script03_note,
  con = file.path(docs_dir, "add_health_codebook_variable_candidate_mapping.md")
)


# ============================================================
# 16. Save final checklist
# ============================================================

write_csv(
  script03_checklist,
  file.path(outputs_diag_dir, "script03_execution_checklist.csv")
)


# ============================================================
# 17. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 03 completed: Add Health Codebook Variable Candidate Mapping\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Local codebooks identified: ", nrow(local_codebooks), "\n", sep = "")
cat("pdftools available: ", ifelse(has_pdftools, "Yes", "No"), "\n", sep = "")
cat("Keyword hits found: ", nrow(keyword_hits), "\n", sep = "")
cat("Constructs reviewed: ", nrow(priority_constructs), "\n\n", sep = "")

cat("Main outputs created:\n")
cat("- outputs/tables/add_health_local_codebooks_script03.csv\n")
cat("- outputs/tables/priority_constructs_script03.csv\n")
cat("- outputs/tables/add_health_codebook_keyword_hits_script03.csv\n")
cat("- outputs/tables/add_health_candidate_mapping_summary_script03.csv\n")
cat("- outputs/tables/construct_mapping_status_script03.csv\n")
cat("- outputs/tables/manual_variable_review_worksheet_script03.csv\n")
cat("- outputs/tables/script03_methodological_notes.csv\n")
cat("- outputs/tables/script03_add_health_codebook_variable_candidate_mapping.xlsx\n")
cat("- outputs/diagnostics/script03_execution_checklist.csv\n")
cat("- docs/add_health_codebook_variable_candidate_mapping.md\n\n")

cat("Construct mapping status summary:\n")
print(construct_mapping_status %>% select(
  priority_id,
  thesis_construct_block,
  total_keyword_hits,
  n_documents_with_hits,
  status
))

cat("\nExecution checklist:\n")
print(script03_checklist)

cat("\nImportant note:\n")
cat("Keyword hits identify candidate pages only. Exact Add Health variable names must be manually confirmed.\n\n")