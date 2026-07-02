# ============================================================
# Project: add-health-adolescent-risk-models
# Script 02: Add Health Documentation Inventory and Variable Search
# Author: Gelo Picol
#
# Purpose:
#   Inventory all documentation files stored in data/metadata/,
#   classify each document by source, type, wave coverage, access
#   status, ethical sensitivity and project relevance.
#
#   Create a structured Add Health variable search plan using the
#   construct inventory produced by Script 01b.
#
# Important:
#   This script does not import, process, or export individual-level
#   thesis data or Add Health microdata.
#
#   It only inventories documentation files and prepares a variable
#   search plan for future mapping.
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
library(openxlsx)


# ============================================================
# 2. Define paths
# ============================================================

metadata_dir <- file.path(project_root, "data/metadata")
outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir <- file.path(project_root, "outputs/diagnostics")
docs_dir <- file.path(project_root, "docs")

dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 3. Helper functions
# ============================================================

classify_source_group <- function(path) {
  path_lower <- tolower(gsub("\\\\", "/", path))

  dplyr::case_when(
    str_detect(path_lower, "thesis_reference") ~ "thesis_reference",
    str_detect(path_lower, "add_health_icpsr") ~ "add_health_icpsr",
    str_detect(path_lower, "add_health_official") ~ "add_health_official",
    str_detect(path_lower, "add_health_codebooks") ~ "add_health_codebooks",
    str_detect(path_lower, "add_health_questionnaires") ~ "add_health_questionnaires",
    str_detect(path_lower, "add_health_user_guides") ~ "add_health_user_guides",
    TRUE ~ "unclassified_metadata"
  )
}

classify_document_type <- function(file_name, source_group) {
  x <- tolower(file_name)

  dplyr::case_when(
    source_group == "thesis_reference" & str_detect(x, "questionnaire|questionario") ~ "thesis_questionnaire",
    source_group == "thesis_reference" & str_detect(x, "presentation|apresentacao|defense") ~ "thesis_defense_presentation",

    str_detect(x, "codebook") ~ "codebook",
    str_detect(x, "questionnaire") ~ "questionnaire",
    str_detect(x, "data_guide|data guide") ~ "data_guide",
    str_detect(x, "analysis_guidelines|guidelines_for_analyzing|analyzing") ~ "analysis_guidelines",
    str_detect(x, "restricted") ~ "restricted_use_guidelines",
    str_detect(x, "weight|weighting|weights") ~ "sampling_weights_documentation",
    str_detect(x, "nonresponse|non-response") ~ "nonresponse_report",
    str_detect(x, "cohort_profile") ~ "cohort_profile",
    str_detect(x, "design_accomplishments|study_design") ~ "study_design_report",
    str_detect(x, "web_transition|panel_survey") ~ "survey_mode_transition_report",
    str_detect(x, "genetic_linkages|social_behavioral_genetic") ~ "methodological_overview",
    str_detect(x, "depressive") ~ "topic_specific_documentation",
    str_detect(x, "icpsr21600|public_use_overview") ~ "icpsr_public_use_overview",
    TRUE ~ "unclassified_document"
  )
}

classify_wave_coverage <- function(file_name) {
  x <- tolower(file_name)

  dplyr::case_when(
    str_detect(x, "waves01_06|wave01_06|waves_01_06|waves i-vi|waves i–vi") ~ "Waves I-VI",
    str_detect(x, "wave06|wave_06|wave vi|wave 6") ~ "Wave VI",
    str_detect(x, "wave05|wave_05|wave v|wave 5") ~ "Wave V",
    str_detect(x, "wave04|wave_04|wave iv|wave 4") ~ "Wave IV",
    str_detect(x, "wave03|wave_03|wave iii|wave 3") ~ "Wave III",
    str_detect(x, "wave02|wave_02|wave ii|wave 2") ~ "Wave II",
    str_detect(x, "wave01|wave_01|wave i|wave 1") ~ "Wave I",
    str_detect(x, "1994_2008|1994-2008") ~ "Waves I-IV, 1994-2008 public-use period",
    str_detect(x, "1994_2018|1994-2018") ~ "Waves I-V, 1994-2018 public-use overview",
    TRUE ~ "General or multi-wave"
  )
}

classify_access_status <- function(file_name, source_group) {
  x <- tolower(file_name)

  dplyr::case_when(
    source_group == "thesis_reference" ~ "local_thesis_reference_not_for_public_commit",
    str_detect(x, "restricted") ~ "restricted_use_documentation",
    str_detect(x, "public_use|public-use|public use|icpsr21600") ~ "public_use_documentation",
    source_group %in% c("add_health_official", "add_health_user_guides", "add_health_icpsr") ~ "public_or_methodological_documentation",
    TRUE ~ "to_be_verified"
  )
}

classify_project_relevance <- function(file_name, document_type, source_group) {
  x <- tolower(file_name)

  dplyr::case_when(
    source_group == "thesis_reference" ~ "High: defines thesis constructs and sample alignment",
    document_type %in% c("data_guide", "analysis_guidelines") ~ "High: required for data structure and analytical decisions",
    document_type == "sampling_weights_documentation" ~ "High: required for weighting and sample interpretation",
    document_type == "icpsr_public_use_overview" ~ "High: identifies public-use study and documentation source",
    document_type == "restricted_use_guidelines" ~ "Medium: useful for understanding restrictions, but not a public-use variable source",
    document_type == "nonresponse_report" ~ "Medium: useful for attrition and non-response discussion",
    document_type %in% c("cohort_profile", "study_design_report", "methodological_overview") ~ "Medium: useful for study background and design",
    document_type == "topic_specific_documentation" ~ "Low to medium: useful only if the topic becomes analytically relevant",
    document_type %in% c("codebook", "questionnaire") ~ "High: required for variable mapping",
    str_detect(x, "depressive") ~ "Low to medium: not central to the initial sexual risk model",
    TRUE ~ "To be reviewed"
  )
}

classify_github_policy <- function(source_group, access_status, document_type) {
  dplyr::case_when(
    source_group == "thesis_reference" ~ "Do not commit PDF; keep local. Commit only metadata inventory.",
    access_status == "restricted_use_documentation" ~ "Do not commit PDF; keep local. Use only to document restrictions.",
    document_type %in% c("codebook", "questionnaire", "data_guide", "analysis_guidelines") ~ "Prefer not to commit PDF; cite official source and commit inventory.",
    TRUE ~ "Prefer not to commit PDF; commit inventory only."
  )
}

standardise_period <- function(wave_coverage) {
  dplyr::case_when(
    str_detect(wave_coverage, "1994-2008") ~ "1994-2008",
    str_detect(wave_coverage, "1994-2018") ~ "1994-2018",
    str_detect(wave_coverage, "Waves I-VI|Wave VI") ~ "1994-2020s, depending on official documentation",
    str_detect(wave_coverage, "Wave V") ~ "Wave V period, verify in documentation",
    str_detect(wave_coverage, "Wave IV") ~ "Wave IV period, verify in documentation",
    str_detect(wave_coverage, "Wave III") ~ "Wave III period, verify in documentation",
    str_detect(wave_coverage, "Wave II") ~ "Wave II period, verify in documentation",
    str_detect(wave_coverage, "Wave I") ~ "Wave I baseline period, verify in documentation",
    TRUE ~ "General or not wave-specific"
  )
}


# ============================================================
# 4. Inventory files in data/metadata
# ============================================================

metadata_files <- list.files(
  path = metadata_dir,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)

if (length(metadata_files) == 0) {
  documentation_inventory <- tibble(
    file_name = character(),
    relative_path = character(),
    source_group = character(),
    file_extension = character(),
    file_size_kb = numeric(),
    last_modified = as.POSIXct(character()),
    document_type = character(),
    wave_coverage = character(),
    period_coverage = character(),
    access_status = character(),
    project_relevance = character(),
    github_policy = character(),
    review_status = character()
  )
} else {

  file_info <- file.info(metadata_files)

  documentation_inventory <- tibble(
    full_path = metadata_files,
    file_name = basename(metadata_files),
    relative_path = str_replace(
      gsub("\\\\", "/", metadata_files),
      fixed(gsub("\\\\", "/", paste0(project_root, "/"))),
      ""
    ),
    source_group = vapply(metadata_files, classify_source_group, character(1)),
    file_extension = tolower(tools::file_ext(metadata_files)),
    file_size_kb = round(file_info$size / 1024, 1),
    last_modified = file_info$mtime
  ) %>%
    mutate(
      document_type = mapply(classify_document_type, file_name, source_group),
      wave_coverage = vapply(file_name, classify_wave_coverage, character(1)),
      period_coverage = vapply(wave_coverage, standardise_period, character(1)),
      access_status = mapply(classify_access_status, file_name, source_group),
      project_relevance = mapply(
        classify_project_relevance,
        file_name,
        document_type,
        source_group
      ),
      github_policy = mapply(
        classify_github_policy,
        source_group,
        access_status,
        document_type
      ),
      review_status = case_when(
        document_type == "unclassified_document" ~ "needs_manual_review",
        source_group == "add_health_codebooks" ~ "priority_for_variable_mapping",
        source_group == "add_health_questionnaires" ~ "priority_for_variable_mapping",
        source_group == "add_health_user_guides" ~ "reviewed_for_design_and_weights",
        source_group == "add_health_icpsr" ~ "reviewed_for_public_use_overview",
        source_group == "add_health_official" ~ "reviewed_for_background",
        source_group == "thesis_reference" ~ "reviewed_for_construct_alignment",
        TRUE ~ "needs_manual_review"
      )
    ) %>%
    select(
      file_name,
      relative_path,
      source_group,
      file_extension,
      file_size_kb,
      last_modified,
      document_type,
      wave_coverage,
      period_coverage,
      access_status,
      project_relevance,
      github_policy,
      review_status
    )
}


# ============================================================
# 5. Documentation summaries
# ============================================================

documentation_source_summary <- documentation_inventory %>%
  count(source_group, document_type, access_status, review_status, name = "n_files") %>%
  arrange(source_group, document_type)

documentation_wave_summary <- documentation_inventory %>%
  count(wave_coverage, period_coverage, name = "n_files") %>%
  arrange(wave_coverage)

documentation_policy_summary <- documentation_inventory %>%
  count(github_policy, name = "n_files") %>%
  arrange(desc(n_files))


# ============================================================
# 6. Empty or pending metadata folders
# ============================================================

expected_metadata_folders <- tibble(
  expected_folder = c(
    "data/metadata/thesis_reference",
    "data/metadata/add_health_official",
    "data/metadata/add_health_icpsr",
    "data/metadata/add_health_codebooks",
    "data/metadata/add_health_questionnaires",
    "data/metadata/add_health_user_guides"
  ),
  expected_role = c(
    "Local thesis defense and questionnaire references",
    "Official Add Health background and methodological documents",
    "ICPSR public-use overview and documentation",
    "Public-use codebooks for variable mapping",
    "Questionnaires for question wording and construct mapping",
    "Data guides, analysis guidelines and weights documentation"
  )
)

folder_status <- expected_metadata_folders %>%
  rowwise() %>%
  mutate(
    absolute_path = file.path(project_root, expected_folder),
    folder_exists = dir.exists(absolute_path),
    n_files = ifelse(
      folder_exists,
      length(list.files(absolute_path, recursive = TRUE, full.names = TRUE)),
      NA_integer_
    ),
    status = case_when(
      !folder_exists ~ "missing_folder",
      n_files == 0 ~ "empty_pending",
      n_files > 0 ~ "available",
      TRUE ~ "to_be_verified"
    )
  ) %>%
  ungroup() %>%
  select(expected_folder, expected_role, folder_exists, n_files, status)


# ============================================================
# 7. Load Script 01b search terms
# ============================================================

search_terms_path <- file.path(
  project_root,
  "outputs/tables/add_health_search_terms_script01b.csv"
)

if (file.exists(search_terms_path)) {
  add_health_search_terms <- read_csv(
    search_terms_path,
    show_col_types = FALSE
  )
} else {
  add_health_search_terms <- tibble(
    search_block = c(
      "Sample alignment",
      "Condom use",
      "Contraceptive use",
      "Pregnancy",
      "HIV and STI outcomes",
      "Family monitoring",
      "School connectedness",
      "Peer norms"
    ),
    thesis_constructs = c(
      "Grade 10-12 and age 15-19",
      "ISX3, APC, AEF, IPC",
      "ISX3, APC, EPC, IPC",
      "Pregnancy-related thesis outcome",
      "HIV/STI-related thesis outcome",
      "LFA1, LFA2",
      "Q01, Q03, Q04, Q06",
      "NPP, NP"
    ),
    english_keywords_for_codebook_search = c(
      "grade school grade current grade age at interview",
      "condom use condom frequency",
      "contraception birth control contraceptive method",
      "pregnancy ever pregnant got pregnant pregnancy outcome",
      "HIV STI sexually transmitted infection test diagnosis",
      "parents know whereabouts parents know friends parental monitoring",
      "school connectedness school belonging teachers care school attachment",
      "friends peers norms peer pressure friends had sex"
    ),
    expected_wave_priority = c(
      "Wave I",
      "Wave I to Wave III",
      "Wave I to Wave III",
      "Wave I to Wave IV",
      "Wave III onward, but verify public-use availability",
      "Wave I",
      "Wave I to Wave II",
      "Wave I to Wave III"
    ),
    search_priority = c(
      "High", "High", "High", "High",
      "High", "High", "High", "High"
    )
  )
}


# ============================================================
# 8. Variable search plan
# ============================================================

available_codebook_files <- documentation_inventory %>%
  filter(source_group == "add_health_codebooks")

available_questionnaire_files <- documentation_inventory %>%
  filter(source_group == "add_health_questionnaires")

has_codebooks <- nrow(available_codebook_files) > 0
has_questionnaires <- nrow(available_questionnaire_files) > 0

variable_search_plan <- add_health_search_terms %>%
  mutate(
    primary_search_resource = case_when(
      has_codebooks & has_questionnaires ~ "Local codebooks, local questionnaires, ICPSR Variables and Add Health Codebook Explorer",
      has_codebooks & !has_questionnaires ~ "Local codebooks, ICPSR Variables and Add Health Codebook Explorer",
      !has_codebooks & has_questionnaires ~ "Local questionnaires, ICPSR Variables and Add Health Codebook Explorer",
      TRUE ~ "ICPSR Variables and Add Health Codebook Explorer; local codebooks still pending"
    ),
    local_documentation_status = case_when(
      has_codebooks & has_questionnaires ~ "codebooks_and_questionnaires_available",
      has_codebooks & !has_questionnaires ~ "codebooks_available_questionnaires_pending",
      !has_codebooks & has_questionnaires ~ "questionnaires_available_codebooks_pending",
      TRUE ~ "codebooks_and_questionnaires_pending"
    ),
    candidate_variable_name = NA_character_,
    candidate_variable_label = NA_character_,
    candidate_wave = NA_character_,
    public_use_status = "to_be_verified",
    mapping_quality = "to_be_verified",
    search_status = "pending_manual_search"
  ) %>%
  select(
    search_block,
    thesis_constructs,
    english_keywords_for_codebook_search,
    expected_wave_priority,
    search_priority,
    primary_search_resource,
    local_documentation_status,
    candidate_variable_name,
    candidate_variable_label,
    candidate_wave,
    public_use_status,
    mapping_quality,
    search_status
  )


# ============================================================
# 9. Update Add Health mapping template from Script 01b
# ============================================================

mapping_template_path <- file.path(
  project_root,
  "outputs/tables/add_health_mapping_template_script01b.csv"
)

if (file.exists(mapping_template_path)) {
  add_health_mapping_template_script02 <- read_csv(
    mapping_template_path,
    show_col_types = FALSE
  ) %>%
    mutate(
      script02_documentation_status = case_when(
        has_codebooks & has_questionnaires ~ "ready_for_local_codebook_and_questionnaire_review",
        has_codebooks & !has_questionnaires ~ "ready_for_codebook_review_questionnaires_pending",
        !has_codebooks & has_questionnaires ~ "ready_for_questionnaire_review_codebooks_pending",
        TRUE ~ "local_codebooks_and_questionnaires_pending"
      ),
      script02_next_action = "Search ICPSR Variables and Add Health Codebook Explorer using Script 01b keywords"
    )
} else {
  add_health_mapping_template_script02 <- tibble(
    status = "Script 01b mapping template not found"
  )
}


# ============================================================
# 10. Script 02 methodological notes
# ============================================================

script02_methodological_notes <- tibble(
  note_id = 1:10,
  note = c(
    "Script 02 inventories documentation only; it does not import microdata.",
    "Source PDFs in data/metadata should remain local and should not be committed to GitHub unless explicitly reviewed.",
    "The safest public GitHub output is the documentation inventory table, not the source PDFs.",
    "Codebooks and questionnaires are still required for detailed variable mapping.",
    "ICPSR Variables and Add Health Codebook Explorer should be used to identify candidate public-use variables.",
    "The thesis questionnaire and defense presentation are local methodological references, not data sources.",
    "Restricted-use documentation may be useful to understand limitations, but the public project should not depend on restricted variables.",
    "The initial analytical focus remains Wave I grades 10-12, with age 15-19 as complementary criterion.",
    "Sensitive topics such as sexual behaviour, pregnancy, HIV and STI outcomes must be reported only in aggregate form.",
    "The next stage should identify candidate Add Health variables for each thesis construct."
  )
)


# ============================================================
# 11. Execution checklist
# ============================================================

script02_checklist <- tibble(
  check_id = 1:13,
  check_item = c(
    "Project root exists",
    "Metadata directory exists",
    "Metadata files inventoried",
    "Documentation source summary created",
    "Documentation wave summary created",
    "Documentation policy summary created",
    "Expected metadata folder status created",
    "Script 01b search terms loaded or fallback created",
    "Variable search plan created",
    "Mapping template updated",
    "CSV outputs exported",
    "Excel workbook exported",
    "Markdown documentation exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(dir.exists(metadata_dir), "OK", "FAIL"),
    ifelse(nrow(documentation_inventory) > 0, "OK", "WARNING_NO_FILES"),
    "OK",
    "OK",
    "OK",
    "OK",
    ifelse(file.exists(search_terms_path), "OK", "WARNING_FALLBACK_USED"),
    "OK",
    ifelse(file.exists(mapping_template_path), "OK", "WARNING_TEMPLATE_NOT_FOUND"),
    "PENDING",
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 12. Export CSV outputs
# ============================================================

write_csv(
  documentation_inventory,
  file.path(outputs_tables_dir, "add_health_documentation_inventory_script02.csv")
)

write_csv(
  documentation_source_summary,
  file.path(outputs_tables_dir, "add_health_documentation_source_summary_script02.csv")
)

write_csv(
  documentation_wave_summary,
  file.path(outputs_tables_dir, "add_health_documentation_wave_summary_script02.csv")
)

write_csv(
  documentation_policy_summary,
  file.path(outputs_tables_dir, "add_health_documentation_policy_summary_script02.csv")
)

write_csv(
  folder_status,
  file.path(outputs_tables_dir, "metadata_folder_status_script02.csv")
)

write_csv(
  variable_search_plan,
  file.path(outputs_tables_dir, "add_health_variable_search_plan_script02.csv")
)

write_csv(
  add_health_mapping_template_script02,
  file.path(outputs_tables_dir, "add_health_mapping_template_updated_script02.csv")
)

write_csv(
  script02_methodological_notes,
  file.path(outputs_tables_dir, "script02_methodological_notes.csv")
)

script02_checklist$status[
  script02_checklist$check_item == "CSV outputs exported"
] <- "OK"


# ============================================================
# 13. Export Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script02_add_health_documentation_inventory.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "documentation_inventory")
writeData(wb, "documentation_inventory", documentation_inventory)

addWorksheet(wb, "source_summary")
writeData(wb, "source_summary", documentation_source_summary)

addWorksheet(wb, "wave_summary")
writeData(wb, "wave_summary", documentation_wave_summary)

addWorksheet(wb, "policy_summary")
writeData(wb, "policy_summary", documentation_policy_summary)

addWorksheet(wb, "folder_status")
writeData(wb, "folder_status", folder_status)

addWorksheet(wb, "variable_search_plan")
writeData(wb, "variable_search_plan", variable_search_plan)

addWorksheet(wb, "mapping_template_update")
writeData(wb, "mapping_template_update", add_health_mapping_template_script02)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script02_methodological_notes)

addWorksheet(wb, "script02_checklist")
writeData(wb, "script02_checklist", script02_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:35, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script02_checklist$status[
  script02_checklist$check_item == "Excel workbook exported"
] <- "OK"


# ============================================================
# 14. Export Markdown documentation
# ============================================================

inventory_note <- c(
  "# Add Health Documentation Inventory",
  "",
  "Script 02 inventories documentation stored locally in `data/metadata/`.",
  "",
  "This script does not import or process individual-level thesis data or Add Health microdata.",
  "",
  "## Current documentation status",
  "",
  paste0("- Number of documentation files inventoried: ", nrow(documentation_inventory)),
  paste0("- Number of metadata folders reviewed: ", nrow(folder_status)),
  paste0("- Codebooks available locally: ", ifelse(has_codebooks, "yes", "no")),
  paste0("- Questionnaires available locally: ", ifelse(has_questionnaires, "yes", "no")),
  "",
  "## GitHub policy",
  "",
  "Source PDFs should remain local unless they are explicitly reviewed for public redistribution.",
  "",
  "The preferred GitHub output is the documentation inventory table, not the source PDFs.",
  "",
  "## Next action",
  "",
  "Use the variable search plan to identify candidate Add Health public-use variables through ICPSR Variables and the Add Health Codebook Explorer."
)

writeLines(
  inventory_note,
  con = file.path(docs_dir, "add_health_documentation_inventory.md")
)

variable_search_note <- c(
  "# Add Health Variable Search Plan",
  "",
  "Script 02 converts the thesis questionnaire construct inventory into a practical variable search plan.",
  "",
  "The search plan is based on the construct inventory created in Script 01b.",
  "",
  "## Priority search areas",
  "",
  "- sample alignment: grade and age;",
  "- demographic controls;",
  "- socioeconomic status;",
  "- sexual initiation and sexual behaviour;",
  "- condom and contraceptive use;",
  "- pregnancy;",
  "- HIV/STI outcomes;",
  "- HIV/AIDS knowledge;",
  "- perceived risk, severity, benefits and barriers;",
  "- self-efficacy and perceived behavioural control;",
  "- attitudes and intentions;",
  "- peer norms;",
  "- family monitoring and support;",
  "- school connectedness and school climate;",
  "- sex education.",
  "",
  "The next analytical stage is to manually search ICPSR Variables and the Add Health Codebook Explorer and then update the mapping template."
)

writeLines(
  variable_search_note,
  con = file.path(docs_dir, "add_health_variable_search_plan_script02.md")
)

script02_checklist$status[
  script02_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 15. Save final checklist
# ============================================================

write_csv(
  script02_checklist,
  file.path(outputs_diag_dir, "script02_execution_checklist.csv")
)


# ============================================================
# 16. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 02 completed: Add Health Documentation Inventory\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Metadata files inventoried: ", nrow(documentation_inventory), "\n", sep = "")
cat("Codebooks available locally: ", ifelse(has_codebooks, "Yes", "No"), "\n", sep = "")
cat("Questionnaires available locally: ", ifelse(has_questionnaires, "Yes", "No"), "\n\n", sep = "")

cat("Main outputs created:\n")
cat("- outputs/tables/add_health_documentation_inventory_script02.csv\n")
cat("- outputs/tables/add_health_documentation_source_summary_script02.csv\n")
cat("- outputs/tables/add_health_documentation_wave_summary_script02.csv\n")
cat("- outputs/tables/add_health_documentation_policy_summary_script02.csv\n")
cat("- outputs/tables/metadata_folder_status_script02.csv\n")
cat("- outputs/tables/add_health_variable_search_plan_script02.csv\n")
cat("- outputs/tables/add_health_mapping_template_updated_script02.csv\n")
cat("- outputs/tables/script02_methodological_notes.csv\n")
cat("- outputs/tables/script02_add_health_documentation_inventory.xlsx\n")
cat("- outputs/diagnostics/script02_execution_checklist.csv\n")
cat("- docs/add_health_documentation_inventory.md\n")
cat("- docs/add_health_variable_search_plan_script02.md\n\n")

cat("Metadata folder status:\n")
print(folder_status)

cat("\nExecution checklist:\n")
print(script02_checklist)

cat("\nImportant note:\n")
cat("Do not commit source PDFs unless explicitly reviewed. Commit the inventory tables instead.\n\n")