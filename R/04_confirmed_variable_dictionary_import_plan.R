# ============================================================
# Project: add-health-adolescent-risk-models
# Script 04: Confirmed Variable Dictionary and Import Plan
# Author: Gelo Picol
#
# Purpose:
#   Create the confirmed variable dictionary and import plan
#   before any Add Health public-use microdata are imported.
#
#   This script reads the candidate variable template from Script 03b
#   and checks whether variables have already been manually reviewed
#   and confirmed.
#
# Important:
#   This script does not import Add Health microdata.
#   This script does not import thesis data.
#   This script prepares the structure required before data import.
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
library(openxlsx)


# ============================================================
# 2. Define paths
# ============================================================

outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir   <- file.path(project_root, "outputs/diagnostics")
docs_dir           <- file.path(project_root, "docs")
data_raw_dir       <- file.path(project_root, "data/raw")
data_processed_dir <- file.path(project_root, "data/processed")
metadata_dir       <- file.path(project_root, "data/metadata")

dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 3. Input files from Script 03b
# ============================================================

candidate_template_path <- file.path(
  outputs_tables_dir,
  "add_health_priority_candidate_variables_template_script03b.csv"
)

construct_priority_path <- file.path(
  outputs_tables_dir,
  "construct_priority_decision_script03b.csv"
)

feasibility_path <- file.path(
  outputs_tables_dir,
  "initial_feasibility_assessment_script03b.csv"
)

script03b_workbook_path <- file.path(
  outputs_tables_dir,
  "script03b_priority_candidate_variables.xlsx"
)

required_inputs <- c(
  candidate_template_path,
  construct_priority_path,
  feasibility_path
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

candidate_template <- read_csv(
  candidate_template_path,
  show_col_types = FALSE
)

construct_priority_decision <- read_csv(
  construct_priority_path,
  show_col_types = FALSE
)

initial_feasibility_assessment <- read_csv(
  feasibility_path,
  show_col_types = FALSE
)


# ============================================================
# 4. Helper functions
# ============================================================

clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  str_trim(x)
}

is_filled <- function(x) {
  x_clean <- str_to_lower(clean_chr(x))

  !x_clean %in% c(
    "",
    "na",
    "n/a",
    "to_be_verified",
    "pending_manual_review",
    "to_be_confirmed",
    "not_available",
    "not applicable"
  )
}

is_accept_decision <- function(x) {
  x_clean <- str_to_lower(clean_chr(x))

  x_clean %in% c(
    "accept",
    "accepted",
    "include",
    "included",
    "selected",
    "use",
    "use_in_analysis",
    "confirmed",
    "confirmed_for_import",
    "accepted_for_import",
    "accepted_for_analysis"
  )
}

is_public_confirmed <- function(x) {
  x_clean <- str_to_lower(clean_chr(x))

  str_detect(x_clean, "public") &
    !str_detect(x_clean, "to_be|verify|pending|restricted")
}

classify_analysis_role <- function(block, expected_use) {
  block_lower <- str_to_lower(clean_chr(block))
  use_lower   <- str_to_lower(clean_chr(expected_use))

  case_when(
    str_detect(block_lower, "sample alignment") ~ "sample_filter",
    str_detect(block_lower, "demographics") ~ "control_variable",
    str_detect(block_lower, "socioeconomic") ~ "control_or_index_variable",
    str_detect(block_lower, "sexual initiation") ~ "outcome_or_key_behaviour",
    str_detect(block_lower, "multiple partners") ~ "outcome_or_key_behaviour",
    str_detect(block_lower, "condom") ~ "protective_behaviour_or_intention",
    str_detect(block_lower, "contraceptive") ~ "protective_behaviour_or_intention",
    str_detect(block_lower, "pregnancy") ~ "reproductive_health_outcome",
    str_detect(block_lower, "hiv|sti") ~ "health_outcome_or_proxy",
    str_detect(block_lower, "knowledge") ~ "health_knowledge_construct",
    str_detect(block_lower, "susceptibility|severity|benefits|barriers") ~ "health_belief_model_construct",
    str_detect(block_lower, "self-efficacy|attitudes|peer") ~ "planned_behaviour_construct",
    str_detect(block_lower, "family") ~ "socioecological_family_construct",
    str_detect(block_lower, "school") ~ "socioecological_school_construct",
    str_detect(use_lower, "control") ~ "control_variable",
    TRUE ~ "to_be_reviewed"
  )
}

classify_variable_level <- function(block) {
  block_lower <- str_to_lower(clean_chr(block))

  case_when(
    str_detect(block_lower, "sample alignment|demographics|socioeconomic") ~ "baseline_covariate",
    str_detect(block_lower, "sexual initiation|condom|contraceptive|pregnancy|hiv|sti|multiple partners") ~ "behavioural_or_health_outcome",
    str_detect(block_lower, "family") ~ "family_context",
    str_detect(block_lower, "school") ~ "school_context",
    str_detect(block_lower, "peer") ~ "peer_context",
    str_detect(block_lower, "knowledge|susceptibility|severity|benefits|barriers|self-efficacy|attitudes") ~ "psychosocial_construct",
    TRUE ~ "to_be_reviewed"
  )
}

classify_import_priority <- function(mapping_priority, manual_priority) {
  mp <- str_to_lower(clean_chr(mapping_priority))
  mr <- str_to_lower(clean_chr(manual_priority))

  case_when(
    str_detect(mp, "critical") | str_detect(mr, "immediate") ~ "1 - import first",
    str_detect(mp, "high") | str_detect(mr, "high") ~ "2 - import second",
    TRUE ~ "3 - import after review"
  )
}


# ============================================================
# 5. Identify manually confirmed candidate variables
# ============================================================

candidate_review_status <- candidate_template %>%
  mutate(
    variable_name_filled = is_filled(add_health_variable_name),
    variable_label_filled = is_filled(add_health_variable_label),
    wave_filled = is_filled(add_health_wave),
    document_filled = is_filled(add_health_document),
    response_categories_filled = is_filled(add_health_response_categories),
    final_decision_accept = is_accept_decision(final_decision),
    public_use_confirmed = is_public_confirmed(public_use_status),
    ready_for_confirmed_dictionary = variable_name_filled &
      variable_label_filled &
      wave_filled &
      public_use_confirmed &
      final_decision_accept,
    review_gap = case_when(
      !variable_name_filled ~ "missing_variable_name",
      !variable_label_filled ~ "missing_variable_label",
      !wave_filled ~ "missing_wave",
      !public_use_confirmed ~ "public_use_status_not_confirmed",
      !final_decision_accept ~ "final_decision_not_accepted",
      TRUE ~ "ready"
    )
  )

confirmed_variable_dictionary <- candidate_review_status %>%
  filter(ready_for_confirmed_dictionary) %>%
  transmute(
    dictionary_id = row_number(),
    priority_id,
    thesis_construct_block,
    thesis_item_codes,
    theoretical_model,
    add_health_target_domain,
    expected_analytic_use,
    analysis_role = mapply(
      classify_analysis_role,
      thesis_construct_block,
      expected_analytic_use
    ),
    variable_level = vapply(
      thesis_construct_block,
      classify_variable_level,
      character(1)
    ),
    equivalence_strategy,
    expected_wave_priority,
    add_health_wave,
    add_health_document,
    add_health_variable_name,
    add_health_variable_label,
    add_health_response_categories,
    proposed_variable_type,
    proposed_coding_direction,
    public_use_status,
    mapping_quality,
    final_decision,
    import_priority = mapply(
      classify_import_priority,
      mapping_priority,
      manual_review_priority
    ),
    reviewer_notes
  )

if (nrow(confirmed_variable_dictionary) == 0) {
  confirmed_variable_dictionary <- tibble(
    dictionary_id = integer(),
    priority_id = numeric(),
    thesis_construct_block = character(),
    thesis_item_codes = character(),
    theoretical_model = character(),
    add_health_target_domain = character(),
    expected_analytic_use = character(),
    analysis_role = character(),
    variable_level = character(),
    equivalence_strategy = character(),
    expected_wave_priority = character(),
    add_health_wave = character(),
    add_health_document = character(),
    add_health_variable_name = character(),
    add_health_variable_label = character(),
    add_health_response_categories = character(),
    proposed_variable_type = character(),
    proposed_coding_direction = character(),
    public_use_status = character(),
    mapping_quality = character(),
    final_decision = character(),
    import_priority = character(),
    reviewer_notes = character()
  )
}


# ============================================================
# 6. Manual review completion summary
# ============================================================

manual_review_completion_summary <- candidate_review_status %>%
  summarise(
    total_candidate_construct_rows = n(),
    rows_with_variable_name = sum(variable_name_filled, na.rm = TRUE),
    rows_with_variable_label = sum(variable_label_filled, na.rm = TRUE),
    rows_with_wave = sum(wave_filled, na.rm = TRUE),
    rows_with_document = sum(document_filled, na.rm = TRUE),
    rows_with_response_categories = sum(response_categories_filled, na.rm = TRUE),
    rows_with_public_use_confirmed = sum(public_use_confirmed, na.rm = TRUE),
    rows_with_final_accept_decision = sum(final_decision_accept, na.rm = TRUE),
    rows_ready_for_confirmed_dictionary = sum(ready_for_confirmed_dictionary, na.rm = TRUE)
  ) %>%
  mutate(
    import_readiness = ifelse(
      rows_ready_for_confirmed_dictionary > 0,
      "partial_or_ready_after_microdata_file_check",
      "not_ready_manual_variable_review_required"
    )
  )

review_gap_summary <- candidate_review_status %>%
  count(review_gap, name = "n_rows") %>%
  arrange(desc(n_rows))


# ============================================================
# 7. Data file inventory template
# ============================================================

existing_raw_data_files <- list.files(
  data_raw_dir,
  recursive = TRUE,
  full.names = FALSE,
  pattern = "\\.(dta|sav|sas7bdat|xpt|csv|tsv|rds|RData)$",
  ignore.case = TRUE
)

data_file_inventory_template <- tibble(
  add_health_wave = c(
    "Wave I",
    "Wave II",
    "Wave III",
    "Wave IV",
    "Wave V",
    "Wave VI"
  ),
  expected_project_role = c(
    "Baseline sample, grades 10-12, age and core adolescent constructs",
    "Short-run adolescent follow-up",
    "Young adult follow-up, sexual behaviour, pregnancy and health outcomes",
    "Adult follow-up, possible longer-run outcomes",
    "Later adult follow-up, optional for extended analysis",
    "Latest follow-up, optional after documentation review"
  ),
  expected_local_raw_file = c(
    "data/raw/add_health_wave01_public_use.*",
    "data/raw/add_health_wave02_public_use.*",
    "data/raw/add_health_wave03_public_use.*",
    "data/raw/add_health_wave04_public_use.*",
    "data/raw/add_health_wave05_public_use.*",
    "data/raw/add_health_wave06_public_use.*"
  ),
  expected_format = c(
    "dta, sav, sas7bdat, xpt or csv",
    "dta, sav, sas7bdat, xpt or csv",
    "dta, sav, sas7bdat, xpt or csv",
    "dta, sav, sas7bdat, xpt or csv",
    "dta, sav, sas7bdat, xpt or csv",
    "dta, sav, sas7bdat, xpt or csv"
  ),
  local_file_status = ifelse(
    length(existing_raw_data_files) > 0,
    "raw_data_folder_contains_files_verify_manually",
    "not_available_locally"
  ),
  git_policy = "Never commit raw or processed individual-level microdata",
  notes = c(
    "Required before Script 05 import.",
    "Required if longitudinal adolescent follow-up is used.",
    "Required for many reproductive and young adult outcomes.",
    "Optional for extended outcomes.",
    "Optional for extended adult outcomes.",
    "Optional; use only after documentation and public-use verification."
  )
)


# ============================================================
# 8. Import plan by wave
# ============================================================

wave_reference <- tibble(
  add_health_wave = c(
    "Wave I",
    "Wave II",
    "Wave III",
    "Wave IV",
    "Wave V",
    "Wave VI"
  ),
  wave_order = 1:6,
  import_sequence = c(
    "1 - first import",
    "2 - second import if needed",
    "3 - third import if needed",
    "4 - optional import",
    "5 - optional import",
    "6 - optional import"
  )
)

if (nrow(confirmed_variable_dictionary) > 0) {
  variables_by_wave <- confirmed_variable_dictionary %>%
    group_by(add_health_wave) %>%
    summarise(
      n_confirmed_variables = n(),
      confirmed_variables = paste(
        sort(unique(add_health_variable_name)),
        collapse = "; "
      ),
      construct_blocks = paste(
        sort(unique(thesis_construct_block)),
        collapse = "; "
      ),
      .groups = "drop"
    )
} else {
  variables_by_wave <- tibble(
    add_health_wave = character(),
    n_confirmed_variables = integer(),
    confirmed_variables = character(),
    construct_blocks = character()
  )
}

import_plan_by_wave <- wave_reference %>%
  left_join(variables_by_wave, by = "add_health_wave") %>%
  mutate(
    n_confirmed_variables = replace_na(n_confirmed_variables, 0L),
    confirmed_variables = replace_na(confirmed_variables, ""),
    construct_blocks = replace_na(construct_blocks, ""),
    import_readiness = case_when(
      n_confirmed_variables > 0 ~ "ready_after_raw_file_availability_check",
      TRUE ~ "not_ready_no_confirmed_variables"
    ),
    import_action = case_when(
      n_confirmed_variables > 0 ~ "Prepare local import script after verifying raw file availability and format.",
      TRUE ~ "Complete manual variable review before importing this wave."
    ),
    git_policy = "Raw data remain local and ignored by Git."
  ) %>%
  arrange(wave_order)


# ============================================================
# 9. Sample definition and analysis design rules
# ============================================================

sample_definition_final <- tibble(
  sample_rule_id = 1:8,
  rule_area = c(
    "Main sample",
    "Complementary age criterion",
    "Sensitivity sample",
    "Baseline wave",
    "Longitudinal follow-up",
    "Microdata publication",
    "Small-cell protection",
    "Original thesis data"
  ),
  rule = c(
    "Restrict main analytical sample to students in grades 10 to 12 at Wave I.",
    "Use age 15 to 19 at Wave I as complementary alignment or sensitivity criterion.",
    "Estimate stricter sensitivity models using both grades 10-12 and age 15-19 when sample size permits.",
    "Use Wave I as baseline for demographic, family, school, peer and initial behavioural constructs.",
    "Use later waves only after variables and longitudinal linkage are confirmed in public-use files.",
    "Do not publish or commit individual-level Add Health microdata.",
    "Review all aggregate outputs for small cells before publication.",
    "Do not publish, reconstruct or approximate the confidential thesis dataset."
  ),
  implementation_stage = c(
    "Script 05 onward",
    "Script 05 onward",
    "Modeling scripts",
    "Import and cleaning scripts",
    "Longitudinal modeling scripts",
    "All scripts",
    "Descriptive and modeling outputs",
    "All scripts"
  )
)


# ============================================================
# 10. Modeling variable role template
# ============================================================

modeling_variable_role_template <- tibble(
  role_id = 1:14,
  variable_role = c(
    "sample_filter",
    "primary_outcome",
    "secondary_outcome",
    "key_behaviour",
    "protective_behaviour",
    "health_belief_construct",
    "planned_behaviour_construct",
    "family_context",
    "school_context",
    "peer_context",
    "socioeconomic_control",
    "demographic_control",
    "survey_design_variable",
    "longitudinal_identifier"
  ),
  expected_constructs = c(
    "grade, age, wave",
    "pregnancy, HIV/STI or equivalent health outcome",
    "sexual initiation, multiple partners or other behavioural outcomes",
    "sexual initiation, frequency, number of partners",
    "condom use, contraceptive use, intention to use protection",
    "susceptibility, severity, benefits, barriers, HIV knowledge",
    "attitudes, subjective norms, perceived control, self-efficacy, intentions",
    "parental monitoring, family support, family connectedness",
    "school connectedness, teacher support, school climate, school attachment",
    "peer norms, peer pressure, perceived peer behaviour",
    "household assets, parental education, income proxies",
    "sex/gender, race/ethnicity, age, grade",
    "weights, strata, clusters if publicly available and analytically required",
    "public-use respondent identifier for internal merging only"
  ),
  import_required = c(
    "Yes",
    "Yes, if available",
    "Yes, if available",
    "Yes, if available",
    "Yes, if available",
    "Yes, if construct equivalents exist",
    "Yes, if construct equivalents exist",
    "Yes",
    "Yes",
    "Yes, with public-use restrictions checked",
    "Yes",
    "Yes",
    "Yes, if available",
    "Yes, internal only"
  ),
  public_output_rule = c(
    "Report only aggregate sample counts.",
    "Report aggregate results and avoid small-cell disclosure.",
    "Report aggregate results and avoid small-cell disclosure.",
    "Report aggregate results only.",
    "Report aggregate results only.",
    "Report scale/index construction, not individual responses.",
    "Report scale/index construction, not individual responses.",
    "Report aggregate or model coefficients only.",
    "Report aggregate or model coefficients only.",
    "Do not publish network identifiers.",
    "Report aggregate or model coefficients only.",
    "Report aggregate or model coefficients only.",
    "Report design choices, not raw design identifiers.",
    "Never publish raw IDs."
  )
)


# ============================================================
# 11. Safe import policy
# ============================================================

safe_import_policy <- tibble(
  policy_id = 1:9,
  policy_area = c(
    "Raw data",
    "Processed data",
    "GitHub",
    "File naming",
    "Variable dictionary",
    "Survey design",
    "Sensitive outcomes",
    "Identifiers",
    "Reproducibility"
  ),
  policy_rule = c(
    "Store raw Add Health public-use files only in data/raw/.",
    "Store processed analytical files only in data/processed/.",
    "Do not commit raw or processed individual-level files.",
    "Use clear local names such as add_health_wave01_public_use.dta.",
    "Import only variables listed in the confirmed variable dictionary.",
    "Use weights and survey design variables only after checking official guidance.",
    "Report pregnancy, HIV/STI and sexual behaviour outputs only in aggregate form.",
    "Use identifiers only for internal merges and never publish them.",
    "Provide code and instructions, but require users to obtain data from official sources."
  ),
  implementation = c(
    ".gitignore blocks data/raw/",
    ".gitignore blocks data/processed/",
    "Check git status before every commit",
    "Document file names in the import plan",
    "Script 05 should read the confirmed dictionary before import",
    "Use Add Health analysis guidelines",
    "Apply disclosure review before publication",
    "Suppress identifiers from all public outputs",
    "README and data access note"
  )
)


# ============================================================
# 12. Import readiness checklist
# ============================================================

n_confirmed_variables <- nrow(confirmed_variable_dictionary)
n_raw_files <- length(existing_raw_data_files)

import_readiness_checklist <- tibble(
  check_id = 1:12,
  readiness_item = c(
    "Manual variable review completed",
    "At least one Add Health variable confirmed",
    "Public-use status confirmed",
    "Wave information completed",
    "Variable labels completed",
    "Response categories completed",
    "Final decision marked as accepted",
    "Raw data files available locally",
    "Raw data excluded from Git",
    "Processed data excluded from Git",
    "Sample definition documented",
    "Import script can be started"
  ),
  status = c(
    ifelse(sum(candidate_review_status$ready_for_confirmed_dictionary) > 0, "OK", "PENDING"),
    ifelse(n_confirmed_variables > 0, "OK", "PENDING"),
    ifelse(sum(candidate_review_status$public_use_confirmed) > 0, "OK", "PENDING"),
    ifelse(sum(candidate_review_status$wave_filled) > 0, "OK", "PENDING"),
    ifelse(sum(candidate_review_status$variable_label_filled) > 0, "OK", "PENDING"),
    ifelse(sum(candidate_review_status$response_categories_filled) > 0, "OK", "PENDING"),
    ifelse(sum(candidate_review_status$final_decision_accept) > 0, "OK", "PENDING"),
    ifelse(n_raw_files > 0, "OK", "PENDING"),
    "OK",
    "OK",
    "OK",
    ifelse(n_confirmed_variables > 0 & n_raw_files > 0, "OK", "NOT_READY")
  ),
  notes = c(
    "Complete candidate_variables sheet from Script 03b.",
    "At least one variable must be confirmed before import.",
    "Use only public-use variables unless formal restricted-use approval exists.",
    "Each confirmed variable must have a wave.",
    "Each confirmed variable must have a label.",
    "Response categories are required for correct recoding.",
    "Only accepted variables enter the import plan.",
    "Place official public-use data locally in data/raw/ only.",
    "Protected by .gitignore.",
    "Protected by .gitignore.",
    "Main sample is grades 10-12; age 15-19 is complementary.",
    "Script 05 should start only after confirmed variables and raw files exist."
  )
)


# ============================================================
# 13. Script 04 methodological notes
# ============================================================

script04_methodological_notes <- tibble(
  note_id = 1:10,
  note = c(
    "Script 04 prepares the confirmed variable dictionary and import plan.",
    "This script does not import Add Health public-use microdata.",
    "This script does not import or use the confidential thesis dataset.",
    "If the Script 03b candidate variable template has not been manually completed, the confirmed dictionary will be empty.",
    "An empty confirmed dictionary is not an error; it means manual codebook review is still required.",
    "The import plan should remain pending until exact variable names, labels, response categories, wave and public-use status are confirmed.",
    "Script 05 should import only variables listed in the confirmed variable dictionary.",
    "Raw and processed microdata must remain local and excluded from GitHub.",
    "The main sample definition remains grades 10-12 at Wave I.",
    "Age 15-19 remains a complementary alignment or sensitivity criterion."
  )
)


# ============================================================
# 14. Execution checklist
# ============================================================

script04_checklist <- tibble(
  check_id = 1:16,
  check_item = c(
    "Project root exists",
    "Candidate template from Script 03b loaded",
    "Construct priority decision loaded",
    "Feasibility assessment loaded",
    "Candidate review status created",
    "Confirmed variable dictionary created",
    "Manual review completion summary created",
    "Review gap summary created",
    "Data file inventory template created",
    "Import plan by wave created",
    "Sample definition documented",
    "Modeling variable role template created",
    "Safe import policy created",
    "Import readiness checklist created",
    "CSV outputs exported",
    "Excel workbook exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(candidate_template_path), "OK", "FAIL"),
    ifelse(file.exists(construct_priority_path), "OK", "FAIL"),
    ifelse(file.exists(feasibility_path), "OK", "FAIL"),
    "OK",
    ifelse(n_confirmed_variables > 0, "OK", "WARNING_EMPTY_CONFIRMED_DICTIONARY"),
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 15. Export CSV outputs
# ============================================================

write_csv(
  candidate_review_status,
  file.path(outputs_tables_dir, "candidate_review_status_script04.csv")
)

write_csv(
  confirmed_variable_dictionary,
  file.path(outputs_tables_dir, "confirmed_variable_dictionary_script04.csv")
)

write_csv(
  manual_review_completion_summary,
  file.path(outputs_tables_dir, "manual_review_completion_summary_script04.csv")
)

write_csv(
  review_gap_summary,
  file.path(outputs_tables_dir, "review_gap_summary_script04.csv")
)

write_csv(
  data_file_inventory_template,
  file.path(outputs_tables_dir, "data_file_inventory_template_script04.csv")
)

write_csv(
  import_plan_by_wave,
  file.path(outputs_tables_dir, "import_plan_by_wave_script04.csv")
)

write_csv(
  sample_definition_final,
  file.path(outputs_tables_dir, "sample_definition_final_script04.csv")
)

write_csv(
  modeling_variable_role_template,
  file.path(outputs_tables_dir, "modeling_variable_role_template_script04.csv")
)

write_csv(
  safe_import_policy,
  file.path(outputs_tables_dir, "safe_import_policy_script04.csv")
)

write_csv(
  import_readiness_checklist,
  file.path(outputs_tables_dir, "import_readiness_checklist_script04.csv")
)

write_csv(
  script04_methodological_notes,
  file.path(outputs_tables_dir, "script04_methodological_notes.csv")
)

script04_checklist$status[
  script04_checklist$check_item == "CSV outputs exported"
] <- "OK"


# ============================================================
# 16. Export Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script04_confirmed_variable_dictionary_import_plan.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "candidate_review_status")
writeData(wb, "candidate_review_status", candidate_review_status)

addWorksheet(wb, "confirmed_dictionary")
writeData(wb, "confirmed_dictionary", confirmed_variable_dictionary)

addWorksheet(wb, "completion_summary")
writeData(wb, "completion_summary", manual_review_completion_summary)

addWorksheet(wb, "review_gaps")
writeData(wb, "review_gaps", review_gap_summary)

addWorksheet(wb, "data_file_inventory")
writeData(wb, "data_file_inventory", data_file_inventory_template)

addWorksheet(wb, "import_plan_by_wave")
writeData(wb, "import_plan_by_wave", import_plan_by_wave)

addWorksheet(wb, "sample_definition")
writeData(wb, "sample_definition", sample_definition_final)

addWorksheet(wb, "variable_roles")
writeData(wb, "variable_roles", modeling_variable_role_template)

addWorksheet(wb, "safe_import_policy")
writeData(wb, "safe_import_policy", safe_import_policy)

addWorksheet(wb, "import_readiness")
writeData(wb, "import_readiness", import_readiness_checklist)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script04_methodological_notes)

addWorksheet(wb, "script04_checklist")
writeData(wb, "script04_checklist", script04_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:40, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script04_checklist$status[
  script04_checklist$check_item == "Excel workbook exported"
] <- "OK"


# ============================================================
# 17. Export Markdown documentation
# ============================================================

script04_note <- c(
  "# Confirmed Variable Dictionary and Import Plan",
  "",
  "Script 04 prepares the confirmed variable dictionary and import plan for the Add Health public-use replication project.",
  "",
  "This script does not import individual-level Add Health microdata.",
  "",
  "## Main purpose",
  "",
  "The script checks whether the candidate variable template from Script 03b has been manually completed and whether variables are ready for inclusion in the confirmed dictionary.",
  "",
  "## Import readiness",
  "",
  paste0("- Confirmed variables: ", n_confirmed_variables),
  paste0("- Raw data files detected locally: ", n_raw_files),
  paste0("- Overall readiness: ", manual_review_completion_summary$import_readiness[1]),
  "",
  "## Rule",
  "",
  "Script 05 should not import any Add Health public-use microdata until exact variable names, labels, waves, response categories, public-use status and final inclusion decisions have been confirmed.",
  "",
  "## Current sample rule",
  "",
  "The main analytical sample remains students in grades 10 to 12 at Wave I. Age 15 to 19 remains a complementary alignment or sensitivity criterion.",
  "",
  "## GitHub rule",
  "",
  "Raw and processed individual-level microdata must remain local and must not be committed to GitHub."
)

writeLines(
  script04_note,
  con = file.path(docs_dir, "confirmed_variable_dictionary_import_plan_script04.md")
)

script04_checklist <- script04_checklist %>%
  add_row(
    check_id = 17,
    check_item = "Markdown documentation exported",
    status = "OK"
  )


# ============================================================
# 18. Save final checklist
# ============================================================

write_csv(
  script04_checklist,
  file.path(outputs_diag_dir, "script04_execution_checklist.csv")
)


# ============================================================
# 19. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 04 completed: Confirmed Variable Dictionary and Import Plan\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Input status:\n")
cat("- Candidate template rows: ", nrow(candidate_template), "\n", sep = "")
cat("- Construct priority rows: ", nrow(construct_priority_decision), "\n", sep = "")
cat("- Feasibility rows: ", nrow(initial_feasibility_assessment), "\n\n", sep = "")

cat("Manual review status:\n")
cat("- Confirmed variables ready for dictionary: ", n_confirmed_variables, "\n", sep = "")
cat("- Raw data files detected locally: ", n_raw_files, "\n", sep = "")
cat("- Import readiness: ", manual_review_completion_summary$import_readiness[1], "\n\n", sep = "")

cat("Main outputs created:\n")
cat("- outputs/tables/candidate_review_status_script04.csv\n")
cat("- outputs/tables/confirmed_variable_dictionary_script04.csv\n")
cat("- outputs/tables/manual_review_completion_summary_script04.csv\n")
cat("- outputs/tables/review_gap_summary_script04.csv\n")
cat("- outputs/tables/data_file_inventory_template_script04.csv\n")
cat("- outputs/tables/import_plan_by_wave_script04.csv\n")
cat("- outputs/tables/sample_definition_final_script04.csv\n")
cat("- outputs/tables/modeling_variable_role_template_script04.csv\n")
cat("- outputs/tables/safe_import_policy_script04.csv\n")
cat("- outputs/tables/import_readiness_checklist_script04.csv\n")
cat("- outputs/tables/script04_methodological_notes.csv\n")
cat("- outputs/tables/script04_confirmed_variable_dictionary_import_plan.xlsx\n")
cat("- outputs/diagnostics/script04_execution_checklist.csv\n")
cat("- docs/confirmed_variable_dictionary_import_plan_script04.md\n\n")

cat("Review gap summary:\n")
print(review_gap_summary)

cat("\nImport readiness checklist:\n")
print(import_readiness_checklist)

cat("\nExecution checklist:\n")
print(script04_checklist)

cat("\nImportant note:\n")
cat("Do not start microdata import until the confirmed variable dictionary contains reviewed Add Health variables.\n\n")