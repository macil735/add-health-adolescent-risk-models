# ============================================================
# Project: add-health-adolescent-risk-models
# Script 01: Data Availability and Variable Mapping Audit
# Author: Gelo Picol
#
# Purpose:
#   Create the initial R/GitHub project structure and produce
#   the first metadata audit for a public, ethical and reproducible
#   application inspired by the doctoral thesis on adolescent
#   pregnancy, HIV infection and adolescent health risk behaviour.
#
# Thesis alignment:
#   The original thesis focused on students from 10th to 12th grade,
#   approximately aged 15 to 19.
#
# Add Health alignment:
#   Add Health Wave I includes students from grades 7 to 12.
#   Therefore, this project will define the main analytical sample
#   as students in grades 10 to 12 at Wave I.
#   Age 15 to 19 will be used as a complementary criterion,
#   mainly for validation or sensitivity analysis.
#
# Ethical rule:
#   This script does not import, process, or export individual-level
#   microdata. It only creates folders, metadata tables, audit files,
#   and documentation.
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
# 2. Create project folder structure
# ============================================================

folders <- c(
  "R",
  "data",
  "data/raw",
  "data/processed",
  "data/metadata",
  "outputs",
  "outputs/tables",
  "outputs/figures",
  "outputs/diagnostics",
  "outputs/logs",
  "docs"
)

for (folder in folders) {
  dir.create(
    file.path(project_root, folder),
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ============================================================
# 3. Project metadata
# ============================================================

project_metadata <- tibble(
  field = c(
    "project_name",
    "repository_name",
    "main_data_source",
    "data_type",
    "doctoral_thesis_population",
    "add_health_initial_population",
    "main_project_sample",
    "complementary_sample_criterion",
    "core_topic",
    "ethical_position",
    "script",
    "script_purpose",
    "microdata_imported"
  ),
  value = c(
    "Add Health Adolescent Risk Models",
    "add-health-adolescent-risk-models",
    "National Longitudinal Study of Adolescent to Adult Health (Add Health)",
    "Public-use documentation and, later, public-use data obtained directly by the user from official sources",
    "Students from 10th to 12th grade, approximately aged 15 to 19",
    "Students from grades 7 to 12 in the United States during the 1994-1995 school year",
    "Students from grades 10 to 12 at Wave I",
    "Age 15 to 19 at Wave I, used as validation or sensitivity criterion",
    "Adolescent pregnancy, HIV-related outcomes and adolescent health risk behaviour",
    "No original thesis data and no individual-level Add Health microdata will be published on GitHub",
    "01_data_availability_variable_mapping_audit.R",
    "Initial audit of data availability, sample alignment and conceptual variable mapping",
    "No"
  )
)


# ============================================================
# 4. Sample definition and thesis alignment
# ============================================================

sample_definition <- tibble(
  sample_level = c(
    "Add Health full baseline school range",
    "Main analytical sample",
    "Complementary age-aligned sample",
    "Sensitivity sample 1",
    "Sensitivity sample 2"
  ),
  definition = c(
    "Students from grades 7 to 12 at Wave I",
    "Students from grades 10 to 12 at Wave I",
    "Students aged 15 to 19 at Wave I",
    "Students from grades 10 to 12 and aged 15 to 19 at Wave I",
    "All Wave I public-use respondents with valid data for the selected outcome and covariates"
  ),
  project_role = c(
    "Description of the Add Health baseline design",
    "Primary sample for thesis-inspired analysis",
    "Age-based validation of comparability with the thesis",
    "Stricter comparability check",
    "Broader robustness check"
  ),
  inclusion_in_main_analysis = c(
    "No",
    "Yes",
    "No, unless grade is unavailable",
    "No, used for sensitivity analysis",
    "No, used only for robustness comparison"
  ),
  notes = c(
    "This range is broader than the thesis population.",
    "This is the closest school-grade equivalent to the original thesis design.",
    "Age is important, but grade is the main alignment criterion because the thesis was based on school years.",
    "This may reduce sample size, but improves comparability.",
    "Useful to check whether results depend strongly on grade restriction."
  )
)


# ============================================================
# 5. Add Health wave overview
# ============================================================
# This table is a metadata guide.
# It does not contain microdata.

add_health_wave_overview <- tibble(
  wave = c(
    "Wave I",
    "Wave II",
    "Wave III",
    "Wave IV",
    "Wave V",
    "Wave VI"
  ),
  broad_period = c(
    "Adolescence",
    "Adolescence",
    "Transition to young adulthood",
    "Young adulthood",
    "Adulthood",
    "Adulthood"
  ),
  baseline_or_followup_role = c(
    "Baseline",
    "Short-run follow-up",
    "Young adult follow-up",
    "Adult follow-up",
    "Later adult follow-up",
    "Latest adult follow-up"
  ),
  relevance_for_project = c(
    "High: baseline characteristics, grade, age, family, school and early risk indicators",
    "High: short-run adolescent transitions and persistence of risk indicators",
    "High: transition outcomes, early adulthood behaviour and possible health outcomes",
    "Medium: adult health and socioeconomic outcomes",
    "Medium: longer-run health and behavioural outcomes",
    "Low at this stage: use only after documentation and access verification"
  ),
  main_sample_note = c(
    "Restrict main analysis to students from grades 10 to 12, with age 15-19 used as complementary criterion.",
    "Use for follow-up of the Wave I analytical sample where longitudinal linkage is available.",
    "Use for longer-run outcomes where variables are available in public-use files.",
    "Use only if relevant to the selected outcome and public-use documentation.",
    "Use only after reviewing public-use documentation and outcome availability.",
    "Use only after confirming public-use documentation, variable availability and comparability."
  ),
  access_status_initial = c(
    "Public-use documentation required",
    "Public-use documentation required",
    "Public-use documentation required",
    "Public-use documentation required",
    "Public-use documentation required",
    "Public-use documentation required"
  )
)


# ============================================================
# 6. Conceptual architecture from the thesis
# ============================================================
# This table transfers the methodological logic of the thesis.
# It does not transfer the original confidential data.

conceptual_blocks <- tibble(
  block_id = c(
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I"
  ),
  conceptual_block = c(
    "Sample alignment variables",
    "Main outcomes",
    "Behavioural risk indicators",
    "Individual characteristics",
    "Family and household context",
    "School and peer context",
    "Knowledge, attitudes and expectations",
    "Health service and prevention indicators",
    "Longitudinal design variables"
  ),
  methodological_role = c(
    "Define comparability between the thesis and Add Health",
    "Dependent variables",
    "Main explanatory variables",
    "Control variables",
    "Control variables and heterogeneity factors",
    "Contextual controls and possible mechanisms",
    "Mechanism and interpretation variables",
    "Prevention and access variables",
    "Panel, transition and temporal ordering variables"
  ),
  examples = c(
    "Grade 10-12 at Wave I; age 15-19 at Wave I",
    "Pregnancy-related outcome; HIV-related outcome; other health risk outcomes",
    "Sexual risk behaviour; early risk exposure; substance-related controls if available",
    "Age; sex; race/ethnicity; school grade; baseline health",
    "Parental education; household structure; socioeconomic status; parental supervision",
    "School attachment; school environment; peer behaviour where publicly available",
    "Risk perception; expectations; health knowledge variables",
    "Testing, counselling or prevention-related indicators if publicly available",
    "Respondent ID for internal merge, wave indicator, timing and survey weights if available"
  )
)


# ============================================================
# 7. Initial variable mapping audit
# ============================================================
# access_status definitions:
#   public_expected:
#     likely available in public-use files, but must be verified
#
#   documentation_required:
#     cannot be classified before official codebook review
#
#   restricted_expected:
#     likely restricted, masked or unavailable in public-use files
#
#   derived_possible:
#     can be constructed if source variables exist
#
# mapping_status definitions:
#   candidate
#   pending_codebook_review
#   not_available_public_use
#   derived_after_import

variable_mapping_audit <- tibble(
  variable_group = c(
    "Sample alignment",
    "Sample alignment",
    "Sample alignment",
    "Main outcome",
    "Main outcome",
    "Main outcome",
    "Behavioural risk",
    "Behavioural risk",
    "Individual control",
    "Individual control",
    "Individual control",
    "Individual control",
    "Family context",
    "Family context",
    "Family context",
    "Family context",
    "School context",
    "School context",
    "Peer context",
    "Knowledge and attitudes",
    "Health service and prevention",
    "Longitudinal design",
    "Longitudinal design",
    "Longitudinal design",
    "Restricted identifiers",
    "Restricted identifiers"
  ),
  thesis_construct = c(
    "Grade restriction: 10th to 12th grade at Wave I",
    "Age restriction: 15 to 19 years at Wave I",
    "Combined grade-age alignment",
    "Early pregnancy or pregnancy-related outcome",
    "HIV infection or HIV-related outcome",
    "General adolescent health risk outcome",
    "Sexual risk behaviour",
    "Other adolescent risk behaviour",
    "Age at baseline",
    "Sex",
    "Race or ethnicity",
    "School grade at baseline",
    "Household socioeconomic status",
    "Parental education",
    "Household structure",
    "Parental supervision or monitoring",
    "School attachment",
    "School environment",
    "Peer risk environment",
    "Risk perception or health knowledge",
    "Health service use or prevention contact",
    "Survey wave",
    "Respondent longitudinal identifier",
    "Sampling weights",
    "School, cluster or contextual identifiers",
    "Friend, sibling or romantic partner identifiers"
  ),
  conceptual_block = c(
    "Sample alignment variables",
    "Sample alignment variables",
    "Sample alignment variables",
    "Main outcomes",
    "Main outcomes",
    "Main outcomes",
    "Behavioural risk indicators",
    "Behavioural risk indicators",
    "Individual characteristics",
    "Individual characteristics",
    "Individual characteristics",
    "Individual characteristics",
    "Family and household context",
    "Family and household context",
    "Family and household context",
    "Family and household context",
    "School and peer context",
    "School and peer context",
    "School and peer context",
    "Knowledge, attitudes and expectations",
    "Health service and prevention indicators",
    "Longitudinal design variables",
    "Longitudinal design variables",
    "Longitudinal design variables",
    "Longitudinal design variables",
    "Longitudinal design variables"
  ),
  expected_wave_relevance = c(
    "Wave I",
    "Wave I",
    "Wave I",
    "Wave I to Wave III; exact wave depends on public codebook",
    "Wave III onward more likely; must verify public availability",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I",
    "Wave I",
    "Wave I",
    "Wave I",
    "Wave I",
    "Wave I",
    "Wave I",
    "Wave I to Wave II",
    "Wave I to Wave II",
    "Wave I to Wave II",
    "Wave I to Wave II; public-use limits expected",
    "Wave I to Wave III",
    "Wave I to Wave IV",
    "All waves",
    "All waves, if public-use longitudinal linkage permits",
    "Wave-specific; must verify documentation",
    "Likely restricted, masked or unavailable in public-use files",
    "Not available in public-use files according to official access restrictions"
  ),
  access_status = c(
    "public_expected",
    "public_expected",
    "derived_possible",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "public_expected",
    "public_expected",
    "public_expected",
    "public_expected",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "documentation_required",
    "public_expected",
    "documentation_required",
    "documentation_required",
    "restricted_expected",
    "restricted_expected"
  ),
  mapping_status = c(
    "candidate",
    "candidate",
    "derived_after_import",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "candidate",
    "candidate",
    "candidate",
    "candidate",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "pending_codebook_review",
    "candidate",
    "pending_codebook_review",
    "pending_codebook_review",
    "not_available_public_use",
    "not_available_public_use"
  ),
  add_health_candidate_variable = c(
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "wave",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_
  ),
  use_in_main_analysis = c(
    "Yes",
    "Control or sensitivity",
    "Sensitivity",
    "Yes, if available in public-use documentation",
    "Yes, if available in public-use documentation",
    "Alternative or complementary outcome",
    "Yes, if available and ethically appropriate",
    "Control or complementary risk indicator",
    "Yes",
    "Yes",
    "Yes",
    "Yes",
    "Yes, if available",
    "Yes, if available",
    "Yes, if available",
    "Yes, if available",
    "Yes, if available",
    "Yes, if available",
    "Only if public-use documentation permits",
    "Yes, if available",
    "Yes, if available",
    "Yes",
    "Internal use only; never publish raw IDs",
    "Yes, if available and appropriate for the model",
    "No",
    "No"
  ),
  notes = c(
    "Primary comparability criterion with the thesis because the thesis was based on students from 10th to 12th grade.",
    "Complementary comparability criterion because the thesis population was approximately aged 15 to 19.",
    "Useful for a stricter sensitivity sample.",
    "Do not assume exact equivalence with the thesis outcome before codebook review.",
    "Treat as sensitive. Use only if public documentation confirms availability and valid measurement.",
    "Define carefully to avoid vague or normative classification.",
    "Use neutral, technical language. Avoid stigmatizing categories.",
    "Use only as scientifically justified control or mechanism.",
    "Required for baseline demographic control.",
    "Required for baseline demographic control.",
    "Use official Add Health categories; avoid over-disaggregation in public outputs.",
    "Central variable for restricting the analytical sample.",
    "May require constructed index depending on available variables.",
    "May be available from adolescent or parent questionnaire; verify source.",
    "Potential confounder or heterogeneity variable.",
    "Potential mechanism; verify exact wording and public availability.",
    "Possible protective factor or mechanism.",
    "Use only if public-use documentation contains relevant variables.",
    "Public-use files exclude direct identifiers of friends, siblings and romantic partners.",
    "Useful for interpretation of behavioural outcomes.",
    "Availability must be verified by wave and public-use status.",
    "Required for longitudinal structure.",
    "Do not publish raw IDs. Use internally only for permitted merges.",
    "Required for representative estimates if available.",
    "Do not use unless explicitly available in public-use files.",
    "Official public-use restrictions exclude these identifiers."
  )
)


# ============================================================
# 8. Ethical risk register
# ============================================================

ethical_risk_register <- tibble(
  risk_area = c(
    "Original thesis data",
    "Add Health microdata",
    "Sensitive adolescent outcomes",
    "Grade and age restrictions",
    "Small-cell disclosure",
    "Restricted-use variables",
    "Identifiers",
    "GitHub publication",
    "Language and interpretation"
  ),
  risk_description = c(
    "The original doctoral database contains sensitive adolescent information.",
    "Public-use Add Health files still contain individual-level survey records.",
    "Outcomes related to pregnancy, HIV and sexual risk behaviour require careful handling.",
    "Restricting to grades 10-12 and possibly ages 15-19 may reduce cell sizes.",
    "Small cells may increase disclosure risk in tables or figures.",
    "Some Add Health files and identifiers require restricted-use contracts.",
    "Friend, sibling, romantic partner, school or cluster identifiers may be restricted, masked or unavailable.",
    "Accidental upload of raw data would create an ethical and legal problem.",
    "Poor wording may stigmatise adolescents or imply moral judgement."
  ),
  mitigation = c(
    "Do not store, copy, describe or publish the original data.",
    "Keep all microdata local and exclude them through .gitignore.",
    "Use neutral academic language and publish only aggregate outputs.",
    "Review sample size after every restriction and report attrition transparently.",
    "Suppress or aggregate small cells before publication.",
    "Classify such variables as restricted and do not use without approval.",
    "Do not publish identifiers or outputs that allow re-identification.",
    "Commit only scripts, documentation and reviewed aggregate outputs.",
    "Use precise epidemiological and econometric language."
  ),
  project_rule = c(
    "Never publish thesis data",
    "Never commit microdata",
    "Publish aggregate analysis only",
    "Check sample size after grade and age filters",
    "Review all tables before release",
    "Use public data only unless formal access exists",
    "No identifiers in public outputs",
    "Check git status before every commit",
    "Avoid stigmatizing labels"
  )
)


# ============================================================
# 9. Planned script sequence
# ============================================================

planned_scripts <- tibble(
  script_number = sprintf("%02d", 1:10),
  script_file = c(
    "01_data_availability_variable_mapping_audit.R",
    "02_documentation_inventory.R",
    "03_public_use_data_import.R",
    "04_data_cleaning_and_harmonisation.R",
    "05_variable_construction.R",
    "06_descriptive_analysis.R",
    "07_binary_response_models.R",
    "08_longitudinal_models.R",
    "09_robustness_checks.R",
    "10_final_report_outputs.R"
  ),
  main_task = c(
    "Create project structure and initial variable availability audit",
    "Inventory official Add Health documentation and codebooks",
    "Import public-use data locally, without committing microdata to GitHub",
    "Clean and harmonise variables, missing values and wave structure",
    "Construct outcomes, exposures, controls and sample restrictions",
    "Produce aggregate descriptive statistics and sample flow tables",
    "Estimate logit/probit models and marginal effects",
    "Estimate longitudinal and transition models where feasible",
    "Run alternative specifications and sensitivity samples",
    "Produce final tables, figures and technical report outputs"
  ),
  microdata_required = c(
    "No",
    "No",
    "Yes, local only",
    "Yes, local only",
    "Yes, local only",
    "Yes, local only",
    "Yes, local only",
    "Yes, local only",
    "Yes, local only",
    "No individual-level output"
  ),
  expected_public_output = c(
    "Metadata audit tables and documentation",
    "Documentation inventory",
    "Import log only, no data",
    "Cleaning log and metadata",
    "Variable construction dictionary",
    "Reviewed aggregate tables and figures",
    "Model summary tables",
    "Longitudinal model summary tables",
    "Robustness tables",
    "Final report, README updates and reviewed outputs"
  )
)


# ============================================================
# 10. Script 01 execution checklist
# ============================================================

script01_checklist <- tibble(
  check_id = 1:12,
  check_item = c(
    "Project root defined",
    "Folder structure created",
    "Project metadata created",
    "Sample definition created",
    "Add Health wave overview created",
    "Conceptual blocks defined",
    "Initial variable mapping audit created",
    "Ethical risk register created",
    "Planned script sequence created",
    "CSV outputs exported",
    "Excel workbook exported",
    "Markdown documentation exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(all(dir.exists(file.path(project_root, folders))), "OK", "FAIL"),
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
# 11. Export CSV outputs
# ============================================================

write_csv(
  project_metadata,
  file.path(project_root, "outputs/tables/project_metadata_script01.csv")
)

write_csv(
  sample_definition,
  file.path(project_root, "outputs/tables/sample_definition_script01.csv")
)

write_csv(
  add_health_wave_overview,
  file.path(project_root, "outputs/tables/add_health_wave_overview_script01.csv")
)

write_csv(
  conceptual_blocks,
  file.path(project_root, "outputs/tables/conceptual_blocks_script01.csv")
)

write_csv(
  variable_mapping_audit,
  file.path(project_root, "outputs/tables/variable_mapping_audit_script01.csv")
)

write_csv(
  ethical_risk_register,
  file.path(project_root, "outputs/tables/ethical_risk_register_script01.csv")
)

write_csv(
  planned_scripts,
  file.path(project_root, "outputs/tables/planned_scripts_script01.csv")
)

script01_checklist$status[
  script01_checklist$check_item == "CSV outputs exported"
] <- "OK"


# ============================================================
# 12. Export Excel workbook
# ============================================================

xlsx_path <- file.path(
  project_root,
  "outputs/tables/script01_data_availability_variable_mapping_audit.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "project_metadata")
writeData(wb, "project_metadata", project_metadata)

addWorksheet(wb, "sample_definition")
writeData(wb, "sample_definition", sample_definition)

addWorksheet(wb, "wave_overview")
writeData(wb, "wave_overview", add_health_wave_overview)

addWorksheet(wb, "conceptual_blocks")
writeData(wb, "conceptual_blocks", conceptual_blocks)

addWorksheet(wb, "variable_mapping")
writeData(wb, "variable_mapping", variable_mapping_audit)

addWorksheet(wb, "ethical_risk_register")
writeData(wb, "ethical_risk_register", ethical_risk_register)

addWorksheet(wb, "planned_scripts")
writeData(wb, "planned_scripts", planned_scripts)

addWorksheet(wb, "script01_checklist")
writeData(wb, "script01_checklist", script01_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:25, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script01_checklist$status[
  script01_checklist$check_item == "Excel workbook exported"
] <- "OK"


# ============================================================
# 13. Export Markdown documentation
# ============================================================

ethics_note <- c(
  "# Ethics Note",
  "",
  "This project does not publish or reconstruct the original doctoral thesis dataset.",
  "",
  "The original dataset contains sensitive information on adolescents and must remain confidential.",
  "",
  "The repository is designed to contain only scripts, documentation, metadata templates and reviewed aggregate outputs.",
  "",
  "Individual-level Add Health microdata must not be committed to GitHub.",
  "",
  "Restricted-use variables must not be used unless formal access approval exists.",
  "",
  "All public outputs must be reviewed for small-cell disclosure risk.",
  "",
  "The main analytical sample will be students from grades 10 to 12 at Wave I.",
  "",
  "Age 15 to 19 will be used as a complementary alignment criterion or sensitivity restriction.",
  "",
  "The project uses neutral academic language and avoids stigmatizing descriptions."
)

writeLines(
  ethics_note,
  con = file.path(project_root, "docs/ethics_note.md")
)

methodology_note <- c(
  "# Methodology Note",
  "",
  "The project is a methodological public application inspired by a doctoral thesis on adolescent pregnancy, HIV infection and adolescent health risk behaviour.",
  "",
  "It does not replicate the confidential thesis database.",
  "",
  "The thesis population was based on students from 10th to 12th grade, approximately aged 15 to 19.",
  "",
  "Add Health Wave I includes students from grades 7 to 12. Therefore, the main analytical sample in this project will be restricted to students from grades 10 to 12 at Wave I.",
  "",
  "Age 15 to 19 will be used as a complementary criterion, either as a control, validation check or sensitivity restriction.",
  "",
  "The initial econometric architecture will focus on binary response models, marginal effects, longitudinal transitions and robustness checks."
)

writeLines(
  methodology_note,
  con = file.path(project_root, "docs/methodology_note.md")
)

variable_mapping_note <- c(
  "# Variable Mapping Note",
  "",
  "Script 01 creates the initial conceptual mapping between the doctoral thesis architecture and Add Health public-use documentation.",
  "",
  "The mapping is preliminary.",
  "",
  "Variables classified as `documentation_required` must be verified against official Add Health codebooks before being used.",
  "",
  "Variables classified as `restricted_expected` must not be used in the public version of the project unless proper restricted-use access has been approved.",
  "",
  "The grade restriction, grades 10 to 12 at Wave I, is the main comparability criterion with the thesis.",
  "",
  "The age restriction, ages 15 to 19 at Wave I, is a complementary criterion.",
  "",
  "The next script will inventory the official documentation and update this mapping."
)

writeLines(
  variable_mapping_note,
  con = file.path(project_root, "docs/variable_mapping.md")
)

data_access_note <- c(
  "# Data Access Note",
  "",
  "This repository does not distribute Add Health microdata.",
  "",
  "Users must obtain public-use Add Health data directly from official sources and comply with the applicable terms of use.",
  "",
  "The `data/raw/` and `data/processed/` folders are local working directories and should remain excluded from GitHub.",
  "",
  "Only metadata templates, scripts and reviewed aggregate outputs should be committed.",
  "",
  "Before any GitHub commit, run `git status` and verify that no raw or processed microdata files are staged."
)

writeLines(
  data_access_note,
  con = file.path(project_root, "docs/data_access_note.md")
)

script01_checklist$status[
  script01_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 14. Create initial README.md
# ============================================================

readme_text <- c(
  "# add-health-adolescent-risk-models",
  "",
  "This repository develops a public, ethical and reproducible R-based application inspired by the econometric architecture of a doctoral thesis on adolescent pregnancy, HIV infection and adolescent health risk behaviour.",
  "",
  "The original thesis dataset cannot be published because it contains sensitive information on adolescents. Therefore, this project does not publish, reconstruct or approximate the original dataset. Instead, it transfers the methodological logic of the thesis to a recognised public longitudinal data source: the National Longitudinal Study of Adolescent to Adult Health, known as Add Health.",
  "",
  "## Scientific objective",
  "",
  "The main objective is to study adolescent and young adult health risk outcomes using public-use Add Health data and reproducible econometric methods.",
  "",
  "## Methodological objective",
  "",
  "The project aims to reproduce the analytical architecture of the original doctoral research in a public and ethically defensible setting. The emphasis is on methods, not on reproducing confidential empirical results from the original thesis.",
  "",
  "## Sample alignment",
  "",
  "The doctoral thesis focused on students from 10th to 12th grade, approximately aged 15 to 19.",
  "",
  "Add Health Wave I includes students from grades 7 to 12. Therefore, the main analytical sample in this project will be restricted to students from grades 10 to 12 at Wave I.",
  "",
  "Age 15 to 19 will be used as a complementary criterion, mainly for validation or sensitivity analysis.",
  "",
  "## Data source",
  "",
  "The project is designed for use with public-use Add Health data and public documentation.",
  "",
  "The repository does not store or redistribute individual-level Add Health data. Users must obtain the data directly from official sources and must comply with all applicable terms of use.",
  "",
  "## Ethical position",
  "",
  "This repository follows four principles:",
  "",
  "1. no publication of the original doctoral dataset;",
  "2. no upload of individual-level Add Health microdata to GitHub;",
  "3. no attempt to identify individuals, schools, friends, siblings, partners or communities;",
  "4. publication only of code, metadata templates, aggregated outputs and reproducible documentation.",
  "",
  "## Project structure",
  "",
  "```text",
  "R/",
  "data/",
  "outputs/",
  "docs/",
  "README.md",
  "LICENSE",
  ".gitignore",
  "```",
  "",
  "The `data/` folder is included only as a local working structure. Raw and processed data files must not be committed to GitHub.",
  "",
  "## Initial script",
  "",
  "The project begins with:",
  "",
  "```text",
  "R/01_data_availability_variable_mapping_audit.R",
  "```",
  "",
  "This script creates the initial project folders, defines the conceptual variable map, classifies expected data availability and exports the first audit tables.",
  "",
  "## Status",
  "",
  "Project under initial development."
)

writeLines(
  readme_text,
  con = file.path(project_root, "README.md")
)


# ============================================================
# 15. Create initial .gitignore
# ============================================================

gitignore_text <- c(
  "# R",
  ".Rhistory",
  ".RData",
  ".Ruserdata",
  ".Rproj.user/",
  "",
  "# Raw and processed data",
  "data/raw/*",
  "data/processed/*",
  "*.dta",
  "*.sav",
  "*.sas7bdat",
  "*.xpt",
  "*.csv",
  "*.tsv",
  "*.rds",
  "*.RData",
  "",
  "# Keep metadata templates",
  "!data/metadata/",
  "!data/metadata/*.csv",
  "!data/metadata/*.xlsx",
  "!data/metadata/*.md",
  "",
  "# Keep selected aggregate output folders",
  "!outputs/",
  "!outputs/tables/",
  "!outputs/figures/",
  "!outputs/diagnostics/",
  "!outputs/logs/",
  "",
  "# Review outputs before committing",
  "outputs/diagnostics/*",
  "outputs/logs/*",
  "",
  "# System files",
  ".DS_Store",
  "Thumbs.db"
)

writeLines(
  gitignore_text,
  con = file.path(project_root, ".gitignore")
)


# ============================================================
# 16. Re-save final checklist
# ============================================================

write_csv(
  script01_checklist,
  file.path(project_root, "outputs/diagnostics/script01_execution_checklist.csv")
)


# ============================================================
# 17. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 01 completed: Data Availability and Variable Mapping Audit\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Main methodological decision:\n")
cat("- Main sample: students from grades 10 to 12 at Wave I.\n")
cat("- Complementary criterion: age 15 to 19 at Wave I.\n")
cat("- No individual-level data were imported or exported.\n\n")

cat("Main outputs created:\n")
cat("- README.md\n")
cat("- .gitignore\n")
cat("- docs/ethics_note.md\n")
cat("- docs/methodology_note.md\n")
cat("- docs/variable_mapping.md\n")
cat("- docs/data_access_note.md\n")
cat("- outputs/tables/project_metadata_script01.csv\n")
cat("- outputs/tables/sample_definition_script01.csv\n")
cat("- outputs/tables/add_health_wave_overview_script01.csv\n")
cat("- outputs/tables/conceptual_blocks_script01.csv\n")
cat("- outputs/tables/variable_mapping_audit_script01.csv\n")
cat("- outputs/tables/ethical_risk_register_script01.csv\n")
cat("- outputs/tables/planned_scripts_script01.csv\n")
cat("- outputs/tables/script01_data_availability_variable_mapping_audit.xlsx\n")
cat("- outputs/diagnostics/script01_execution_checklist.csv\n\n")

cat("Execution checklist:\n")
print(script01_checklist)

cat("\nImportant note:\n")
cat("Before any GitHub commit, verify that no raw or processed microdata files are staged.\n")
cat("Use git status before every commit.\n\n")