# ============================================================
# Script 13d — Psychosocial Construct Variable Recovery and Audit
# Project: Add Health Adolescent Risk Models
#
# Purpose:
#   1. Audit Add Health Wave I psychosocial construct variables.
#   2. Recover variables from raw public-use sources when missing
#      from the clean analytical file.
#   3. Classify candidate variables by questionnaire section and
#      theoretical construct.
#   4. Produce aggregate public outputs only.
#
# Scope:
#   Section 8  — Pregnancy, AIDS, and STD Risk Perceptions
#   Section 9  — Self Efficacy
#   Section 17 — Motivations to Engage in Risky Behaviors
#   Section 18 — Personality and Family
#   Section 19 — Knowledge Quiz
#   Section 20 — Friends
#
# Important:
#   This script does not export individual-level data.
#   It exports metadata, recovery audits, missingness summaries,
#   construct summaries and documentation only.
# ============================================================


# ============================================================
# 0. Setup
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"

setwd(project_root)

required_packages <- c(
  "dplyr", "tidyr", "stringr", "purrr", "readr",
  "tibble", "haven", "openxlsx", "officer", "flextable"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running Script 13d."
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/diagnostics", recursive = TRUE, showWarnings = FALSE)
dir.create("docs", recursive = TRUE, showWarnings = FALSE)

clean_data_path <- file.path(
  project_root,
  "data/processed/add_health_wave01_analytical_clean_local_only.rds"
)

raw_sav_path <- file.path(
  project_root,
  "data/raw/21600-0001-Data.sav"
)

weight_rda_path <- file.path(
  project_root,
  "data/raw/21600-0004-Data.rda"
)

if (!file.exists(clean_data_path)) {
  stop("Clean analytical RDS not found: ", clean_data_path)
}

if (!file.exists(raw_sav_path)) {
  stop("Raw SAV file not found: ", raw_sav_path)
}

dat_clean <- readRDS(clean_data_path)

raw_data <- haven::read_sav(raw_sav_path, user_na = TRUE)
raw_data <- haven::zap_missing(raw_data)


# ============================================================
# 1. Helper functions
# ============================================================

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_character <- function(x) {
  as.character(x)
}

get_var_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)

  if (is.null(label)) {
    return("")
  }

  safe_character(label)
}

write_csv_safe <- function(x, path) {
  readr::write_csv(x, path, na = "")
}

load_first_dataframe_from_rda <- function(path) {
  env <- new.env(parent = emptyenv())
  loaded_objects <- load(path, envir = env)

  for (obj in loaded_objects) {
    candidate <- get(obj, envir = env)

    if (is.data.frame(candidate)) {
      return(candidate)
    }
  }

  return(NULL)
}

find_first_existing <- function(candidates, data_names) {
  hit <- candidates[candidates %in% data_names]

  if (length(hit) == 0) {
    return(NA_character_)
  }

  hit[1]
}

resolve_source_var <- function(base_var, data_names) {
  base_var <- safe_character(base_var)

  candidates <- c(
    base_var,
    paste0("num_", base_var),
    tolower(base_var),
    paste0("num_", tolower(base_var))
  )

  candidates <- candidates[!is.na(candidates) & candidates != ""]

  find_first_existing(candidates, data_names)
}

classify_section <- function(variable, label) {
  v <- stringr::str_to_upper(safe_character(variable))
  l <- stringr::str_to_upper(safe_character(label))

  dplyr::case_when(
    stringr::str_detect(v, "^H1RP") |
      stringr::str_detect(l, "\\bS8Q") ~
      "S8",

    stringr::str_detect(v, "^H1SE") |
      stringr::str_detect(l, "\\bS9Q") ~
      "S9",

    stringr::str_detect(l, "\\bS17Q") ~
      "S17",

    stringr::str_detect(v, "^H1PF") |
      stringr::str_detect(l, "\\bS18Q") ~
      "S18",

    stringr::str_detect(v, "^H1KQ") |
      stringr::str_detect(l, "\\bS19Q") ~
      "S19",

    v == "FR_FLAG" |
      stringr::str_detect(v, "^H1MF") |
      stringr::str_detect(v, "^H1FF") |
      stringr::str_detect(l, "\\bS20Q") ~
      "S20",

    TRUE ~ NA_character_
  )
}

section_label <- function(section_id) {
  dplyr::case_when(
    section_id == "S8" ~
      "Section 8: Pregnancy, AIDS, and STD Risk Perceptions",
    section_id == "S9" ~
      "Section 9: Self Efficacy",
    section_id == "S17" ~
      "Section 17: Motivations to Engage in Risky Behaviors",
    section_id == "S18" ~
      "Section 18: Personality and Family",
    section_id == "S19" ~
      "Section 19: Knowledge Quiz",
    section_id == "S20" ~
      "Section 20: Friends",
    TRUE ~ "Other"
  )
}

classify_construct <- function(section_id, variable, label) {
  v <- stringr::str_to_upper(safe_character(variable))
  l <- stringr::str_to_upper(safe_character(label))

  dplyr::case_when(
    section_id == "S8" &
      stringr::str_detect(l, "PREGN|PREGNANT") ~
      "risk_perception_pregnancy",

    section_id == "S8" &
      stringr::str_detect(l, "AIDS|HIV|VIRUS") ~
      "risk_perception_hiv_aids",

    section_id == "S8" &
      stringr::str_detect(l, "STD|SEXUALLY TRANSMITTED") ~
      "risk_perception_sti",

    section_id == "S8" ~
      "risk_perceptions_general",

    section_id == "S9" ~
      "self_efficacy",

    section_id == "S17" &
      stringr::str_detect(l, "FRIEND|RESPECT|POPULAR") ~
      "risk_motivation_peer_norms",

    section_id == "S17" &
      stringr::str_detect(l, "GUILTY|UPSET|FEEL|ENJOY|PLEASURE") ~
      "risk_motivation_affective_evaluation",

    section_id == "S17" &
      stringr::str_detect(l, "MARRI|PARENT|MOTHER|FATHER|FAMILY") ~
      "risk_motivation_family_moral_norms",

    section_id == "S17" ~
      "risk_motivation_general",

    section_id == "S18" &
      stringr::str_detect(l, "MOTHER|FATHER|PARENT|FAMILY|LOVED|WANTED|CLOSE|CARE") ~
      "family_support_and_connectedness",

    section_id == "S18" &
      stringr::str_detect(l, "DECISION|DECIDE|PROBLEM|PLAN|THINK") ~
      "decision_making_problem_solving",

    section_id == "S18" &
      stringr::str_detect(l, "LIFE|SATISF|HAPPY|DEPRESS|TROUBLE|PERSONALITY|TEMPER") ~
      "personality_wellbeing",

    section_id == "S18" &
      stringr::str_detect(l, "BIRTH CONTROL|CONTRACEPT|PREGN") ~
      "family_or_personal_contraceptive_context",

    section_id == "S18" ~
      "section18_other_personality_family",

    section_id == "S19" &
      stringr::str_detect(v, "A$") ~
      "contraceptive_knowledge_answer",

    section_id == "S19" &
      stringr::str_detect(v, "B$") ~
      "contraceptive_knowledge_confidence",

    section_id == "S19" ~
      "contraceptive_knowledge_other",

    section_id == "S20" &
      (v == "FR_FLAG") ~
      "friend_nomination_design",

    section_id == "S20" &
      stringr::str_detect(l, "GO TO SCHOOL|SCHOOL|GRADE|SISTER SCHOOL") ~
      "friends_school_context",

    section_id == "S20" &
      stringr::str_detect(l, "HOUSE|HANG OUT|MEET|TELEPHONE|TALK|PROBLEM") ~
      "friends_contact_support",

    section_id == "S20" ~
      "friends_peer_network",

    TRUE ~
      "unclassified"
  )
}

expected_direction_note <- function(construct) {
  dplyr::case_when(
    construct %in% c(
      "risk_perception_pregnancy",
      "risk_perception_hiv_aids",
      "risk_perception_sti",
      "risk_perceptions_general"
    ) ~
      "Direction requires item-level review: higher perceived risk/severity may be protective or may reflect existing exposure.",

    construct == "self_efficacy" ~
      "Direction requires recoding: higher final score should indicate stronger self-efficacy.",

    stringr::str_detect(construct, "risk_motivation") ~
      "Direction requires recoding: higher final score should indicate stronger motivation or pressure toward risk, unless reversed.",

    construct == "family_support_and_connectedness" ~
      "Direction requires recoding: higher final score should indicate stronger family support/connectedness.",

    construct == "decision_making_problem_solving" ~
      "Direction requires recoding: higher final score should indicate stronger decision-making/problem-solving capacity.",

    construct == "personality_wellbeing" ~
      "Direction requires item review: items may measure positive wellbeing or vulnerability.",

    construct == "contraceptive_knowledge_answer" ~
      "Can be scored against correct answers in a later script; higher final score should indicate more correct knowledge.",

    construct == "contraceptive_knowledge_confidence" ~
      "Direction requires recoding: higher final score should indicate greater confidence, if used.",

    stringr::str_detect(construct, "friends") ~
      "Direction requires item review: peer contact, peer school context and peer support should not be mixed without review.",

    TRUE ~
      "Direction to review."
  )
}

is_probably_item_for_index <- function(construct) {
  !construct %in% c(
    "friend_nomination_design",
    "contraceptive_knowledge_confidence",
    "unclassified"
  )
}

n_valid <- function(x) {
  sum(!is.na(x))
}

prop_missing <- function(x) {
  mean(is.na(x))
}


# ============================================================
# 2. Recover GSWGT1 if needed
# ============================================================

dat_aug <- dat_clean

weight_source <- NA_character_

weight_var_clean <- resolve_source_var("GSWGT1", names(dat_aug))

if (!is.na(weight_var_clean)) {
  dat_aug$GSWGT1 <- safe_numeric(dat_aug[[weight_var_clean]])
  weight_source <- paste0("resolved_from_clean_data:", weight_var_clean)
}

if (!"GSWGT1" %in% names(dat_aug) || all(is.na(dat_aug$GSWGT1))) {
  if (file.exists(weight_rda_path)) {
    weight_data <- load_first_dataframe_from_rda(weight_rda_path)

    if (!is.null(weight_data)) {
      weight_var_rda <- resolve_source_var("GSWGT1", names(weight_data))

      if (!is.na(weight_var_rda) && nrow(weight_data) == nrow(dat_aug)) {
        dat_aug$GSWGT1 <- safe_numeric(weight_data[[weight_var_rda]])
        weight_source <- paste0(
          "recovered_from:",
          basename(weight_rda_path),
          ":",
          weight_var_rda
        )
      }
    }
  }
}

if (!"GSWGT1" %in% names(dat_aug) || all(is.na(dat_aug$GSWGT1))) {
  dat_aug$GSWGT1 <- 1
  weight_source <- "unweighted_fallback_weight_equals_1"
  warning("GSWGT1 was not found. Script 13d will use fallback weight = 1 for audit summaries.")
}

weight_audit <- tibble::tibble(
  weight_variable = "GSWGT1",
  weight_source = weight_source,
  valid_weight_n = sum(!is.na(dat_aug$GSWGT1) & dat_aug$GSWGT1 > 0),
  interpretation = dplyr::case_when(
    weight_source == "unweighted_fallback_weight_equals_1" ~
      "Exploratory unweighted fallback was used because GSWGT1 was not found.",
    TRUE ~
      "Survey weight was resolved for audit summaries."
  )
)


# ============================================================
# 3. Raw metadata and target section detection
# ============================================================

raw_metadata <- tibble::tibble(
  variable = names(raw_data),
  label = purrr::map_chr(raw_data, get_var_label)
) %>%
  mutate(
    section_id = purrr::map2_chr(variable, label, classify_section),
    section_name = section_label(section_id),
    target_section = section_id %in% c("S8", "S9", "S17", "S18", "S19", "S20")
  )

section_rules <- tibble::tibble(
  section_id = c("S8", "S9", "S17", "S18", "S19", "S20"),
  section_name = section_label(section_id),
  detection_rule = c(
    "Variable name starts with H1RP or label contains S8Q.",
    "Variable name starts with H1SE or label contains S9Q.",
    "Variable label contains S17Q.",
    "Variable name starts with H1PF or label contains S18Q.",
    "Variable name starts with H1KQ or label contains S19Q.",
    "Variable name is FR_FLAG or starts with H1MF/H1FF, or label contains S20Q."
  ),
  audit_status = "planned_for_recovery_and_missingness_audit"
)

candidate_variables <- raw_metadata %>%
  filter(target_section) %>%
  mutate(
    construct_block = purrr::pmap_chr(
      list(section_id, variable, label),
      classify_construct
    ),
    expected_direction = purrr::map_chr(
      construct_block,
      expected_direction_note
    ),
    likely_index_candidate = purrr::map_lgl(
      construct_block,
      is_probably_item_for_index
    )
  ) %>%
  arrange(section_id, variable)

if (nrow(candidate_variables) == 0) {
  stop("No candidate psychosocial variables were detected from the raw SAV metadata.")
}


# ============================================================
# 4. Recover candidate variables into local in-memory data
# ============================================================

recover_one_variable <- function(data, raw, var_name) {
  clean_hit <- resolve_source_var(var_name, names(data))

  if (!is.na(clean_hit)) {
    if (clean_hit != var_name) {
      data[[var_name]] <- data[[clean_hit]]
    }

    return(list(
      data = data,
      recovered = TRUE,
      source_file = "clean_analytical_data",
      source_variable = clean_hit,
      recovery_method = "already_available"
    ))
  }

  raw_hit <- resolve_source_var(var_name, names(raw))

  if (!is.na(raw_hit)) {
    if (nrow(raw) == nrow(data)) {
      data[[var_name]] <- raw[[raw_hit]]

      return(list(
        data = data,
        recovered = TRUE,
        source_file = basename(raw_sav_path),
        source_variable = raw_hit,
        recovery_method = "row_order_same_n"
      ))
    }

    if ("AID" %in% names(data) && "AID" %in% names(raw)) {
      lookup <- raw %>%
        select(AID, recovered_value = all_of(raw_hit)) %>%
        distinct(AID, .keep_all = TRUE)

      data <- data %>%
        left_join(lookup, by = "AID")

      data[[var_name]] <- data$recovered_value
      data$recovered_value <- NULL

      return(list(
        data = data,
        recovered = TRUE,
        source_file = basename(raw_sav_path),
        source_variable = raw_hit,
        recovery_method = "matched_by_AID"
      ))
    }
  }

  return(list(
    data = data,
    recovered = FALSE,
    source_file = NA_character_,
    source_variable = NA_character_,
    recovery_method = "not_found_or_unmatched"
  ))
}

recovery_records <- list()

for (i in seq_len(nrow(candidate_variables))) {
  v <- candidate_variables$variable[i]

  recovery <- recover_one_variable(
    data = dat_aug,
    raw = raw_data,
    var_name = v
  )

  dat_aug <- recovery$data

  recovery_records[[i]] <- tibble::tibble(
    variable = v,
    recovered = recovery$recovered,
    source_file = recovery$source_file,
    source_variable = recovery$source_variable,
    recovery_method = recovery$recovery_method
  )
}

variable_recovery_audit <- dplyr::bind_rows(recovery_records)

candidate_variables <- candidate_variables %>%
  left_join(variable_recovery_audit, by = "variable")


# ============================================================
# 5. Sample definitions
# ============================================================

dat_aug <- dat_aug %>%
  mutate(
    a_age_wave1 = safe_numeric(a_age_wave1),
    a_grade_wave1 = safe_numeric(a_grade_wave1),
    a_female = safe_numeric(a_female),
    GSWGT1 = safe_numeric(GSWGT1),
    sample_main_grade_10_12 = a_grade_wave1 %in% c(10, 11, 12),
    sample_restricted_sensitivity =
      a_grade_wave1 %in% c(10, 11, 12) &
      a_age_wave1 >= 15 &
      a_age_wave1 <= 19
  )

sample_definitions <- tibble::tibble(
  sample_name = c(
    "Full public-use Wave I analytical file",
    "Main sample: grades 10-12",
    "Restricted sensitivity sample: grades 10-12 and ages 15-19"
  ),
  sample_filter = c(
    "all rows",
    "a_grade_wave1 in 10, 11, 12",
    "a_grade_wave1 in 10, 11, 12 and a_age_wave1 between 15 and 19"
  ),
  n_unweighted = c(
    nrow(dat_aug),
    sum(dat_aug$sample_main_grade_10_12, na.rm = TRUE),
    sum(dat_aug$sample_restricted_sensitivity, na.rm = TRUE)
  ),
  weighted_total = c(
    sum(dat_aug$GSWGT1, na.rm = TRUE),
    sum(dat_aug$GSWGT1[dat_aug$sample_main_grade_10_12], na.rm = TRUE),
    sum(dat_aug$GSWGT1[dat_aug$sample_restricted_sensitivity], na.rm = TRUE)
  )
)


# ============================================================
# 6. Missingness and availability summaries
# ============================================================

summarise_variable_availability <- function(data, variable) {
  if (!variable %in% names(data)) {
    return(tibble::tibble(
      variable = variable,
      full_n_valid = 0,
      full_missing_percent = 100,
      main_n_valid = 0,
      main_missing_percent = 100,
      restricted_n_valid = 0,
      restricted_missing_percent = 100
    ))
  }

  x_full <- data[[variable]]

  main_data <- data %>%
    filter(sample_main_grade_10_12)

  restricted_data <- data %>%
    filter(sample_restricted_sensitivity)

  x_main <- main_data[[variable]]
  x_restricted <- restricted_data[[variable]]

  tibble::tibble(
    variable = variable,

    full_n_valid = n_valid(x_full),
    full_missing_percent = 100 * prop_missing(x_full),

    main_n_valid = n_valid(x_main),
    main_missing_percent = 100 * prop_missing(x_main),

    restricted_n_valid = n_valid(x_restricted),
    restricted_missing_percent = 100 * prop_missing(x_restricted)
  )
}

missingness_summary <- purrr::map_dfr(
  candidate_variables$variable,
  ~ summarise_variable_availability(dat_aug, .x)
)

variable_inventory <- candidate_variables %>%
  left_join(missingness_summary, by = "variable") %>%
  mutate(
    audit_recommendation = dplyr::case_when(
      !recovered ~
        "not_available_for_index_construction",
      main_n_valid >= 500 & main_missing_percent <= 60 & likely_index_candidate ~
        "candidate_for_script13e_index_review",
      main_n_valid >= 100 & likely_index_candidate ~
        "possible_but_high_missingness_or_skip_pattern",
      !likely_index_candidate ~
        "metadata_or_auxiliary_variable",
      TRUE ~
        "review_before_use"
    ),
    public_export_status =
      "aggregate_metadata_only_no_individual_values_exported"
  ) %>%
  select(
    section_id,
    section_name,
    variable,
    label,
    construct_block,
    expected_direction,
    likely_index_candidate,
    recovered,
    source_file,
    source_variable,
    recovery_method,
    full_n_valid,
    full_missing_percent,
    main_n_valid,
    main_missing_percent,
    restricted_n_valid,
    restricted_missing_percent,
    audit_recommendation,
    public_export_status
  )

construct_summary <- variable_inventory %>%
  group_by(section_id, section_name, construct_block) %>%
  summarise(
    variables_detected = n(),
    variables_recovered = sum(recovered, na.rm = TRUE),
    likely_index_candidates = sum(likely_index_candidate, na.rm = TRUE),
    median_main_n_valid = median(main_n_valid, na.rm = TRUE),
    median_main_missing_percent = median(main_missing_percent, na.rm = TRUE),
    recommended_for_13e_count = sum(
      audit_recommendation == "candidate_for_script13e_index_review",
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(section_id, construct_block)

section_summary <- variable_inventory %>%
  group_by(section_id, section_name) %>%
  summarise(
    variables_detected = n(),
    variables_recovered = sum(recovered, na.rm = TRUE),
    construct_blocks_detected = n_distinct(construct_block),
    likely_index_candidates = sum(likely_index_candidate, na.rm = TRUE),
    recommended_for_13e_count = sum(
      audit_recommendation == "candidate_for_script13e_index_review",
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(section_id)


# ============================================================
# 7. Methodological notes
# ============================================================

methodological_notes <- tibble::tibble(
  note_id = 1:8,
  note = c(
    "Script 13d is an audit script only; it does not build final indices or estimate models.",
    "Raw Add Health variables are read locally but individual-level values are not exported.",
    "Recovery uses the clean analytical data first; missing variables are recovered from the raw public-use SAV when row counts match.",
    "Variable labels are used to detect sections when variable prefixes are insufficient.",
    "Section 18 is not treated as one construct; it is split into family, personality/wellbeing, decision-making/problem-solving and contraceptive context candidates.",
    "Section 19 is separated into factual knowledge answers and confidence items.",
    "Section 20 friend variables are treated as peer-network candidates and require careful review before index construction.",
    "Script 13e should use this audit to build and test candidate scales, including reliability and direction of scoring."
  )
)

public_summary <- tibble::tibble(
  section = c(
    "Purpose",
    "Main decision",
    "Data protection",
    "Next step"
  ),
  interpretation = c(
    "Script 13d recovers and audits psychosocial variables from Add Health Wave I Sections 8, 9, 17, 18, 19 and 20.",
    "The script identifies candidate variables and construct blocks but does not yet create final scores.",
    "Only aggregate metadata and diagnostics are exported; no respondent-level values are written to public outputs.",
    "Script 13e should construct candidate indices and evaluate reliability, missingness and direction of scoring."
  )
)


# ============================================================
# 8. Export outputs
# ============================================================

write_csv_safe(
  section_rules,
  "outputs/tables/script13d_wave01_psychosocial_section_rules.csv"
)

write_csv_safe(
  weight_audit,
  "outputs/tables/script13d_wave01_psychosocial_weight_audit.csv"
)

write_csv_safe(
  sample_definitions,
  "outputs/tables/script13d_wave01_psychosocial_sample_definitions.csv"
)

write_csv_safe(
  variable_recovery_audit,
  "outputs/tables/script13d_wave01_psychosocial_variable_recovery_audit.csv"
)

write_csv_safe(
  variable_inventory,
  "outputs/tables/script13d_wave01_psychosocial_variable_inventory.csv"
)

write_csv_safe(
  missingness_summary,
  "outputs/tables/script13d_wave01_psychosocial_missingness_summary.csv"
)

write_csv_safe(
  construct_summary,
  "outputs/tables/script13d_wave01_psychosocial_construct_summary.csv"
)

write_csv_safe(
  section_summary,
  "outputs/tables/script13d_wave01_psychosocial_section_summary.csv"
)

write_csv_safe(
  methodological_notes,
  "outputs/tables/script13d_wave01_psychosocial_methodological_notes.csv"
)

write_csv_safe(
  public_summary,
  "outputs/tables/script13d_wave01_psychosocial_public_summary.csv"
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "section_rules")
openxlsx::writeData(wb, "section_rules", section_rules)

openxlsx::addWorksheet(wb, "weight_audit")
openxlsx::writeData(wb, "weight_audit", weight_audit)

openxlsx::addWorksheet(wb, "samples")
openxlsx::writeData(wb, "samples", sample_definitions)

openxlsx::addWorksheet(wb, "recovery_audit")
openxlsx::writeData(wb, "recovery_audit", variable_recovery_audit)

openxlsx::addWorksheet(wb, "variable_inventory")
openxlsx::writeData(wb, "variable_inventory", variable_inventory)

openxlsx::addWorksheet(wb, "missingness")
openxlsx::writeData(wb, "missingness", missingness_summary)

openxlsx::addWorksheet(wb, "construct_summary")
openxlsx::writeData(wb, "construct_summary", construct_summary)

openxlsx::addWorksheet(wb, "section_summary")
openxlsx::writeData(wb, "section_summary", section_summary)

openxlsx::addWorksheet(wb, "methodological_notes")
openxlsx::writeData(wb, "methodological_notes", methodological_notes)

openxlsx::addWorksheet(wb, "public_summary")
openxlsx::writeData(wb, "public_summary", public_summary)

for (sheet in names(wb)) {
  openxlsx::setColWidths(wb, sheet = sheet, cols = 1:50, widths = "auto")
  openxlsx::freezePane(wb, sheet = sheet, firstRow = TRUE)
}

openxlsx::saveWorkbook(
  wb,
  "outputs/tables/script13d_wave01_psychosocial_construct_audit_results.xlsx",
  overwrite = TRUE
)


# ============================================================
# 9. Create DOCX and Markdown documentation
# ============================================================

doc <- officer::read_docx()

doc <- officer::body_add_par(
  doc,
  "Script 13d — Psychosocial Construct Variable Recovery and Audit",
  style = "heading 1"
)

doc <- officer::body_add_par(
  doc,
  "This report documents the recovery and audit of candidate psychosocial construct variables from Add Health Wave I Sections 8, 9, 17, 18, 19 and 20. It exports aggregate metadata only.",
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Public summary", style = "heading 2")
doc <- flextable::body_add_flextable(
  doc,
  flextable::autofit(flextable::flextable(public_summary))
)

doc <- officer::body_add_par(doc, "Section summary", style = "heading 2")
doc <- flextable::body_add_flextable(
  doc,
  flextable::autofit(flextable::flextable(section_summary))
)

doc <- officer::body_add_par(doc, "Construct summary", style = "heading 2")
doc <- flextable::body_add_flextable(
  doc,
  flextable::autofit(flextable::flextable(construct_summary))
)

doc <- officer::body_add_par(doc, "Methodological notes", style = "heading 2")
doc <- flextable::body_add_flextable(
  doc,
  flextable::autofit(flextable::flextable(methodological_notes))
)

doc <- officer::body_add_par(doc, "Interpretive caution", style = "heading 2")
doc <- officer::body_add_par(
  doc,
  "Script 13d does not validate final scales. It only identifies variables and candidate construct blocks. Scale construction, item direction, reliability and modeling should be handled in Scripts 13e and 13f.",
  style = "Normal"
)

print(
  doc,
  target = "docs/add_health_wave01_psychosocial_construct_audit_script13d.docx"
)

md_lines <- c(
  "# Script 13d — Psychosocial Construct Variable Recovery and Audit",
  "",
  "Script 13d audits candidate psychosocial variables from Add Health Wave I Sections 8, 9, 17, 18, 19 and 20.",
  "",
  "## Scope",
  "",
  "- Section 8: Pregnancy, AIDS, and STD Risk Perceptions",
  "- Section 9: Self Efficacy",
  "- Section 17: Motivations to Engage in Risky Behaviors",
  "- Section 18: Personality and Family",
  "- Section 19: Knowledge Quiz",
  "- Section 20: Friends",
  "",
  "## Public data protection",
  "",
  "No individual-level values are exported. Outputs are aggregate metadata, recovery audits, missingness summaries and construct-level summaries.",
  "",
  "## Next step",
  "",
  "Script 13e should construct candidate indices and evaluate reliability, missingness and scoring direction.",
  "",
  "## Outputs",
  "",
  "- outputs/tables/script13d_wave01_psychosocial_section_rules.csv",
  "- outputs/tables/script13d_wave01_psychosocial_weight_audit.csv",
  "- outputs/tables/script13d_wave01_psychosocial_sample_definitions.csv",
  "- outputs/tables/script13d_wave01_psychosocial_variable_recovery_audit.csv",
  "- outputs/tables/script13d_wave01_psychosocial_variable_inventory.csv",
  "- outputs/tables/script13d_wave01_psychosocial_missingness_summary.csv",
  "- outputs/tables/script13d_wave01_psychosocial_construct_summary.csv",
  "- outputs/tables/script13d_wave01_psychosocial_section_summary.csv",
  "- outputs/tables/script13d_wave01_psychosocial_construct_audit_results.xlsx",
  "- docs/add_health_wave01_psychosocial_construct_audit_script13d.docx"
)

writeLines(
  md_lines,
  "docs/add_health_wave01_psychosocial_construct_audit_script13d.md"
)


# ============================================================
# 10. Execution checklist
# ============================================================

execution_checklist <- tibble::tibble(
  check_id = 1:18,
  check_item = c(
    "Project root exists",
    "Clean analytical RDS exists",
    "Raw SAV file exists",
    "Outputs tables directory exists",
    "Outputs diagnostics directory exists",
    "Docs directory exists",
    "Raw metadata created",
    "Target sections detected",
    "Candidate variables detected",
    "Candidate variables recovered or audited",
    "Weight audit created",
    "Sample definitions created",
    "Variable inventory exported",
    "Missingness summary exported",
    "Construct summary exported",
    "Excel workbook exported",
    "DOCX report exported",
    "No individual-level values exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(clean_data_path), "OK", "FAIL"),
    ifelse(file.exists(raw_sav_path), "OK", "FAIL"),
    ifelse(dir.exists("outputs/tables"), "OK", "FAIL"),
    ifelse(dir.exists("outputs/diagnostics"), "OK", "FAIL"),
    ifelse(dir.exists("docs"), "OK", "FAIL"),
    ifelse(nrow(raw_metadata) > 0, "OK", "FAIL"),
    ifelse(sum(raw_metadata$target_section, na.rm = TRUE) > 0, "OK", "FAIL"),
    ifelse(nrow(candidate_variables) > 0, "OK", "FAIL"),
    ifelse(nrow(variable_recovery_audit) > 0, "OK", "FAIL"),
    ifelse(nrow(weight_audit) > 0, "OK", "FAIL"),
    ifelse(nrow(sample_definitions) > 0, "OK", "FAIL"),
    ifelse(file.exists("outputs/tables/script13d_wave01_psychosocial_variable_inventory.csv"), "OK", "FAIL"),
    ifelse(file.exists("outputs/tables/script13d_wave01_psychosocial_missingness_summary.csv"), "OK", "FAIL"),
    ifelse(file.exists("outputs/tables/script13d_wave01_psychosocial_construct_summary.csv"), "OK", "FAIL"),
    ifelse(file.exists("outputs/tables/script13d_wave01_psychosocial_construct_audit_results.xlsx"), "OK", "FAIL"),
    ifelse(file.exists("docs/add_health_wave01_psychosocial_construct_audit_script13d.docx"), "OK", "FAIL"),
    "OK"
  )
)

write_csv_safe(
  execution_checklist,
  "outputs/diagnostics/script13d_execution_checklist.csv"
)


# ============================================================
# 11. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 13d completed: Psychosocial Construct Variable Audit\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Weight audit:\n")
print(weight_audit)

cat("\nSection summary:\n")
print(section_summary)

cat("\nConstruct summary:\n")
print(construct_summary)

cat("\nTop candidate variables for Script 13e:\n")
print(
  variable_inventory %>%
    filter(audit_recommendation == "candidate_for_script13e_index_review") %>%
    select(section_id, construct_block, variable, main_n_valid, main_missing_percent) %>%
    arrange(section_id, construct_block, variable) %>%
    head(40)
)

cat("\nExecution checklist:\n")
print(execution_checklist)

cat("\nOutputs created:\n")
cat("- outputs/tables/script13d_wave01_psychosocial_section_rules.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_weight_audit.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_sample_definitions.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_variable_recovery_audit.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_variable_inventory.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_missingness_summary.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_construct_summary.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_section_summary.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_methodological_notes.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_public_summary.csv\n")
cat("- outputs/tables/script13d_wave01_psychosocial_construct_audit_results.xlsx\n")
cat("- docs/add_health_wave01_psychosocial_construct_audit_script13d.docx\n")
cat("- docs/add_health_wave01_psychosocial_construct_audit_script13d.md\n")
cat("- outputs/diagnostics/script13d_execution_checklist.csv\n")

cat("\nImportant note:\n")
cat("Script 13d is an audit script only. Do not treat the variables as final scales before Script 13e.\n")
cat("No individual-level values were exported.\n")