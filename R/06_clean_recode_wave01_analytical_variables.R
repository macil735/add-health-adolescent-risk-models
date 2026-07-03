# ============================================================
# Project: add-health-adolescent-risk-models
# Script 06: Clean and Recode Wave I Analytical Variables
# Author: Gelo Picol
#
# Purpose:
#   Clean and recode the imported Add Health Wave I variables
#   produced by Script 05.
#
# Important:
#   - This script does not import raw .sav files.
#   - This script reads only the processed local-only Wave I RDS
#     created by Script 05.
#   - It creates a cleaned analytical local-only RDS.
#   - It does not export individual-level microdata publicly.
#   - Public outputs are limited to metadata, diagnostics and
#     aggregate summaries.
# ============================================================


# ============================================================
# 0. Project root and display options
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"

options(na.print = "NA")


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "haven",
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

library(haven)
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

data_processed_dir <- file.path(project_root, "data/processed")
outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir   <- file.path(project_root, "outputs/diagnostics")
docs_dir           <- file.path(project_root, "docs")

dir.create(data_processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

input_rds_path <- file.path(
  data_processed_dir,
  "add_health_wave01_confirmed_variables_local_only.rds"
)

output_clean_rds_path <- file.path(
  data_processed_dir,
  "add_health_wave01_analytical_clean_local_only.rds"
)

confirmed_dictionary_path <- file.path(
  outputs_tables_dir,
  "confirmed_variable_dictionary_script04.csv"
)

script05_metadata_path <- file.path(
  outputs_tables_dir,
  "script05_wave01_variable_metadata.csv"
)

script05_import_check_path <- file.path(
  outputs_tables_dir,
  "script05_wave01_import_variable_check.csv"
)


# ============================================================
# 3. Check required inputs
# ============================================================

required_inputs <- c(
  input_rds_path,
  confirmed_dictionary_path,
  script05_metadata_path,
  script05_import_check_path
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


# ============================================================
# 4. Helper functions
# ============================================================

clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  stringr::str_trim(x)
}

to_numeric_clean <- function(x) {
  suppressWarnings(as.numeric(haven::zap_labels(x)))
}

get_variable_label <- function(x) {
  lab <- attr(x, "label")

  if (is.null(lab)) {
    return(NA_character_)
  }

  as.character(lab)
}

safe_count_public <- function(n, threshold = 10) {
  if (is.na(n)) {
    return(NA_character_)
  }

  if (n < threshold) {
    return(paste0("<", threshold))
  }

  as.character(n)
}

count_true <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_integer_)
  }

  sum(x %in% TRUE, na.rm = TRUE)
}

count_nonmissing <- function(x) {
  if (length(x) == 0) {
    return(NA_integer_)
  }

  sum(!is.na(x))
}

find_var <- function(data, candidates) {
  available_names_upper <- stringr::str_to_upper(names(data))
  candidates_upper <- stringr::str_to_upper(candidates)

  hit <- candidates_upper[candidates_upper %in% available_names_upper]

  if (length(hit) == 0) {
    return(NA_character_)
  }

  names(data)[match(hit[1], available_names_upper)]
}

recode_yes_no_from_labels <- function(x) {
  x_factor <- suppressWarnings(haven::as_factor(x, levels = "labels"))
  x_label <- stringr::str_to_lower(as.character(x_factor))

  out <- rep(NA_integer_, length(x_label))

  out[stringr::str_detect(x_label, "\\byes\\b|^yes$|sim|verdadeiro|true")] <- 1L
  out[stringr::str_detect(x_label, "\\bno\\b|^no$|não|nao|falso|false")] <- 0L

  if (all(is.na(out))) {
    x_num <- to_numeric_clean(x)
    nonmiss <- unique(x_num[!is.na(x_num)])

    if (all(nonmiss %in% c(0, 1))) {
      out <- ifelse(is.na(x_num), NA_integer_, as.integer(x_num == 1))
    }
  }

  out
}

recode_female_from_biosex <- function(x) {
  x_factor <- suppressWarnings(haven::as_factor(x, levels = "labels"))
  x_label <- stringr::str_to_lower(as.character(x_factor))

  out <- rep(NA_integer_, length(x_label))

  out[stringr::str_detect(x_label, "female|woman|girl|feminino|rapariga|mulher")] <- 1L
  out[stringr::str_detect(x_label, "male|man|boy|masculino|rapaz|homem")] <- 0L

  if (all(is.na(out))) {
    x_num <- to_numeric_clean(x)

    # Common Add Health coding is usually 1 = male, 2 = female.
    out <- dplyr::case_when(
      x_num == 2 ~ 1L,
      x_num == 1 ~ 0L,
      TRUE ~ NA_integer_
    )
  }

  out
}

standardize_z <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))

  if (all(is.na(x_num))) {
    return(rep(NA_real_, length(x_num)))
  }

  s <- sd(x_num, na.rm = TRUE)

  if (is.na(s) || s == 0) {
    return(rep(NA_real_, length(x_num)))
  }

  as.numeric((x_num - mean(x_num, na.rm = TRUE)) / s)
}

reverse_minmax <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))

  if (all(is.na(x_num))) {
    return(rep(NA_real_, length(x_num)))
  }

  min_x <- min(x_num, na.rm = TRUE)
  max_x <- max(x_num, na.rm = TRUE)

  if (is.na(min_x) || is.na(max_x) || min_x == max_x) {
    return(rep(NA_real_, length(x_num)))
  }

  max_x + min_x - x_num
}


# ============================================================
# 5. Load data and metadata
# ============================================================

wave01_data <- readRDS(input_rds_path)

confirmed_dictionary <- read_csv(
  confirmed_dictionary_path,
  show_col_types = FALSE
)

script05_metadata <- read_csv(
  script05_metadata_path,
  show_col_types = FALSE
)

script05_import_check <- read_csv(
  script05_import_check_path,
  show_col_types = FALSE
)


# ============================================================
# 6. Identify key variables
# ============================================================

grade_var <- find_var(wave01_data, c("H1GI20", "GRADE"))
sex_var   <- find_var(wave01_data, c("BIO_SEX"))

age_var <- find_var(
  wave01_data,
  c("derived_age_wave1", "AGE")
)

main_sample_flag_var <- find_var(
  wave01_data,
  c("derived_main_sample_flag")
)

strict_sample_flag_var <- find_var(
  wave01_data,
  c("derived_strict_sample_flag")
)

grade_10_12_flag_var <- find_var(
  wave01_data,
  c("derived_grade_10_12_flag")
)

age_15_19_flag_var <- find_var(
  wave01_data,
  c("derived_age_15_19_flag")
)


# ============================================================
# 7. Build base analytical dataset
# ============================================================

analytical_data <- wave01_data

if (!is.na(grade_var)) {
  analytical_data <- analytical_data %>%
    mutate(
      a_grade_wave1 = to_numeric_clean(.data[[grade_var]]),
      a_sample_grade_10_12 = a_grade_wave1 %in% 10:12
    )
} else {
  analytical_data <- analytical_data %>%
    mutate(
      a_grade_wave1 = NA_real_,
      a_sample_grade_10_12 = NA
    )
}

if (!is.na(age_var)) {
  analytical_data <- analytical_data %>%
    mutate(
      a_age_wave1 = to_numeric_clean(.data[[age_var]]),
      a_sample_age_15_19 = a_age_wave1 %in% 15:19
    )
} else {
  analytical_data <- analytical_data %>%
    mutate(
      a_age_wave1 = NA_real_,
      a_sample_age_15_19 = NA
    )
}

if (!is.na(sex_var)) {
  analytical_data <- analytical_data %>%
    mutate(
      a_female = recode_female_from_biosex(.data[[sex_var]])
    )
} else {
  analytical_data <- analytical_data %>%
    mutate(
      a_female = NA_integer_
    )
}

analytical_data <- analytical_data %>%
  mutate(
    a_main_sample_grade_10_12 = a_sample_grade_10_12,
    a_strict_sample_grade_age = a_sample_grade_10_12 & a_sample_age_15_19
  )


# ============================================================
# 8. Create generic cleaned numeric versions of imported variables
# ============================================================

derived_variables_existing <- names(analytical_data)[
  stringr::str_detect(names(analytical_data), "^derived_|^a_")
]

original_imported_variables <- setdiff(
  names(wave01_data),
  names(wave01_data)[stringr::str_detect(names(wave01_data), "^derived_")]
)

clean_numeric_data <- purrr::map_dfc(
  original_imported_variables,
  function(v) {
    tibble(
      !!paste0("num_", v) := to_numeric_clean(wave01_data[[v]])
    )
  }
)

analytical_data <- bind_cols(
  analytical_data,
  clean_numeric_data
)


# ============================================================
# 9. Create specific behavioural recodes where possible
# ============================================================

sex_ever_var <- find_var(analytical_data, c("H1CO1"))

condom_vars <- c("H1CO8", "H1CO9")
condom_vars <- condom_vars[
  stringr::str_to_upper(condom_vars) %in%
    stringr::str_to_upper(names(analytical_data))
]

contraceptive_vars <- c("H1CO3", "H1CO6", "H1CO13")
contraceptive_vars <- contraceptive_vars[
  stringr::str_to_upper(contraceptive_vars) %in%
    stringr::str_to_upper(names(analytical_data))
]

pregnancy_vars <- c("H1FP7", "H1FP8")
pregnancy_vars <- pregnancy_vars[
  stringr::str_to_upper(pregnancy_vars) %in%
    stringr::str_to_upper(names(analytical_data))
]

hiv_sti_vars <- c("H1CO16D", "H1HS9", "H1CO16A", "H1CO16C")
hiv_sti_vars <- hiv_sti_vars[
  stringr::str_to_upper(hiv_sti_vars) %in%
    stringr::str_to_upper(names(analytical_data))
]

if (!is.na(sex_ever_var)) {
  analytical_data <- analytical_data %>%
    mutate(
      a_sex_ever = recode_yes_no_from_labels(.data[[sex_ever_var]])
    )
} else {
  analytical_data <- analytical_data %>%
    mutate(
      a_sex_ever = NA_integer_
    )
}

if (length(condom_vars) > 0) {
  condom_recoded <- purrr::map_dfc(
    condom_vars,
    function(v) {
      tibble(
        !!paste0("a_", v, "_yesno") := recode_yes_no_from_labels(analytical_data[[v]])
      )
    }
  )

  analytical_data <- bind_cols(analytical_data, condom_recoded)
}

if (length(contraceptive_vars) > 0) {
  contraceptive_recoded <- purrr::map_dfc(
    contraceptive_vars,
    function(v) {
      tibble(
        !!paste0("a_", v, "_yesno") := recode_yes_no_from_labels(analytical_data[[v]])
      )
    }
  )

  analytical_data <- bind_cols(analytical_data, contraceptive_recoded)
}

if (length(pregnancy_vars) > 0) {
  pregnancy_recoded <- purrr::map_dfc(
    pregnancy_vars,
    function(v) {
      tibble(
        !!paste0("a_", v, "_yesno") := recode_yes_no_from_labels(analytical_data[[v]])
      )
    }
  )

  analytical_data <- bind_cols(analytical_data, pregnancy_recoded)
}

if (length(hiv_sti_vars) > 0) {
  hiv_sti_recoded <- purrr::map_dfc(
    hiv_sti_vars,
    function(v) {
      tibble(
        !!paste0("a_", v, "_yesno") := recode_yes_no_from_labels(analytical_data[[v]])
      )
    }
  )

  analytical_data <- bind_cols(analytical_data, hiv_sti_recoded)
}


# ============================================================
# 10. Build construct availability groups
# ============================================================

construct_variable_map <- script05_import_check %>%
  filter(import_decision == "import") %>%
  select(
    thesis_construct_block,
    theoretical_model,
    expected_analytic_use,
    analysis_role,
    variable_level,
    mapping_quality,
    sav_variable_name
  ) %>%
  distinct()

construct_availability_summary <- construct_variable_map %>%
  group_by(
    thesis_construct_block,
    theoretical_model,
    expected_analytic_use,
    analysis_role,
    variable_level,
    mapping_quality
  ) %>%
  summarise(
    n_imported_variables = n_distinct(sav_variable_name),
    imported_variables = paste(sort(unique(sav_variable_name)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(
    thesis_construct_block,
    sav_variable_name = imported_variables
  )


# ============================================================
# 11. Create preliminary construct score diagnostics
# ============================================================
# These are diagnostic scores only. They are not final theoretical
# indices. Final scale construction requires a later reliability
# and directionality review.

numeric_clean_names <- names(analytical_data)[
  stringr::str_detect(names(analytical_data), "^num_")
]

construct_score_diagnostics <- construct_variable_map %>%
  mutate(
    numeric_clean_name = paste0("num_", sav_variable_name),
    available_numeric_clean = numeric_clean_name %in% numeric_clean_names
  ) %>%
  group_by(thesis_construct_block) %>%
  summarise(
    n_numeric_items_available = sum(available_numeric_clean, na.rm = TRUE),
    numeric_items_available = paste(
      sort(unique(numeric_clean_name[available_numeric_clean])),
      collapse = "; "
    ),
    preliminary_score_possible = n_numeric_items_available >= 2,
    score_status = ifelse(
      preliminary_score_possible,
      "possible_after_directionality_review",
      "not_recommended_less_than_two_numeric_items"
    ),
    .groups = "drop"
  ) %>%
  arrange(thesis_construct_block)


# ============================================================
# 12. Save clean local-only analytical data
# ============================================================

saveRDS(
  analytical_data,
  output_clean_rds_path
)


# ============================================================
# 13. Public missingness and recode diagnostics
# ============================================================

analytic_variable_inventory <- tibble(
  variable_name = names(analytical_data),
  variable_label = sapply(analytical_data, get_variable_label),
  variable_class = sapply(analytical_data, function(x) paste(class(x), collapse = "; ")),
  n_unique_values = sapply(analytical_data, function(x) dplyr::n_distinct(x, na.rm = FALSE)),
  is_original_imported = names(analytical_data) %in% names(wave01_data),
  is_clean_numeric = stringr::str_detect(names(analytical_data), "^num_"),
  is_analytical_recode = stringr::str_detect(names(analytical_data), "^a_"),
  is_derived_script05 = stringr::str_detect(names(analytical_data), "^derived_")
)

analytic_missingness_summary <- tibble(
  variable_name = names(analytical_data),
  n_observations = nrow(analytical_data),
  n_missing = sapply(analytical_data, function(x) sum(is.na(x))),
  n_nonmissing = sapply(analytical_data, function(x) sum(!is.na(x)))
) %>%
  mutate(
    pct_missing = round(100 * n_missing / n_observations, 2),
    pct_nonmissing = round(100 * n_nonmissing / n_observations, 2)
  ) %>%
  left_join(
    analytic_variable_inventory,
    by = "variable_name"
  ) %>%
  select(
    variable_name,
    variable_label,
    variable_class,
    n_observations,
    n_missing,
    n_nonmissing,
    pct_missing,
    pct_nonmissing,
    n_unique_values,
    is_original_imported,
    is_clean_numeric,
    is_analytical_recode,
    is_derived_script05
  )

sample_definition_summary <- tibble(
  summary_id = 1:9,
  item = c(
    "Total Wave I observations in local analytical file",
    "Grade variable used",
    "Age variable used",
    "Sex variable used",
    "Grade 10-12 observations",
    "Age 15-19 observations",
    "Strict grade 10-12 and age 15-19 observations",
    "Female observations",
    "Male observations"
  ),
  value = c(
    nrow(analytical_data),
    ifelse(is.na(grade_var), "not_found", grade_var),
    ifelse(is.na(age_var), "not_found", age_var),
    ifelse(is.na(sex_var), "not_found", sex_var),
    count_true(analytical_data$a_sample_grade_10_12),
    count_true(analytical_data$a_sample_age_15_19),
    count_true(analytical_data$a_strict_sample_grade_age),
    sum(analytical_data$a_female == 1, na.rm = TRUE),
    sum(analytical_data$a_female == 0, na.rm = TRUE)
  ),
  public_value = c(
    safe_count_public(nrow(analytical_data)),
    ifelse(is.na(grade_var), "not_found", grade_var),
    ifelse(is.na(age_var), "not_found", age_var),
    ifelse(is.na(sex_var), "not_found", sex_var),
    safe_count_public(count_true(analytical_data$a_sample_grade_10_12)),
    safe_count_public(count_true(analytical_data$a_sample_age_15_19)),
    safe_count_public(count_true(analytical_data$a_strict_sample_grade_age)),
    safe_count_public(sum(analytical_data$a_female == 1, na.rm = TRUE)),
    safe_count_public(sum(analytical_data$a_female == 0, na.rm = TRUE))
  ),
  note = c(
    "Aggregate count only.",
    "Main sample criterion.",
    "Complementary alignment criterion.",
    "Control and stratification variable.",
    "Primary analytical sample.",
    "Sensitivity alignment criterion.",
    "Strict sensitivity sample.",
    "Aggregate count only.",
    "Aggregate count only."
  )
)

recode_quality_check <- tibble(
  check_id = 1:12,
  recode_area = c(
    "Grade recode",
    "Age recode",
    "Sex recode",
    "Main sample flag",
    "Strict sample flag",
    "Sexual initiation recode",
    "Condom recode candidates",
    "Contraceptive recode candidates",
    "Pregnancy recode candidates",
    "HIV/STI recode candidates",
    "Generic numeric cleaned variables",
    "Construct score diagnostics"
  ),
  status = c(
    ifelse(sum(!is.na(analytical_data$a_grade_wave1)) > 0, "OK", "WARNING_NO_GRADE"),
    ifelse(sum(!is.na(analytical_data$a_age_wave1)) > 0, "OK", "WARNING_NO_AGE"),
    ifelse(sum(!is.na(analytical_data$a_female)) > 0, "OK", "WARNING_NO_SEX_RECODE"),
    ifelse(sum(!is.na(analytical_data$a_main_sample_grade_10_12)) > 0, "OK", "WARNING_NO_MAIN_SAMPLE"),
    ifelse(sum(!is.na(analytical_data$a_strict_sample_grade_age)) > 0, "OK", "WARNING_NO_STRICT_SAMPLE"),
    ifelse(sum(!is.na(analytical_data$a_sex_ever)) > 0, "OK", "CHECK_SEX_EVER_CODING"),
    ifelse(length(condom_vars) > 0, "OK", "NO_CONDOM_VARS_FOUND"),
    ifelse(length(contraceptive_vars) > 0, "OK", "NO_CONTRACEPTIVE_VARS_FOUND"),
    ifelse(length(pregnancy_vars) > 0, "OK", "NO_PREGNANCY_VARS_FOUND"),
    ifelse(length(hiv_sti_vars) > 0, "OK", "NO_HIV_STI_VARS_FOUND"),
    ifelse(length(numeric_clean_names) > 0, "OK", "NO_NUMERIC_CLEAN_VARS"),
    "DIAGNOSTIC_ONLY"
  ),
  note = c(
    "Grade should support restriction to grades 10-12.",
    "Age should support 15-19 sensitivity restriction.",
    "Female indicator derived from BIO_SEX when possible.",
    "Main sample based on grade 10-12.",
    "Strict sample combines grade 10-12 and age 15-19.",
    "Check labels before final interpretation.",
    "Candidate variables only; final behavioural coding may require codebook confirmation.",
    "Candidate variables only; final behavioural coding may require codebook confirmation.",
    "Candidate variables only; final outcome coding may require codebook confirmation.",
    "Candidate variables only; final outcome coding may require codebook confirmation.",
    "Numeric versions are for diagnostics and later recoding.",
    "Construct scores should not be interpreted until directionality and reliability are reviewed."
  )
)


# ============================================================
# 14. Safe public output policy
# ============================================================

safe_public_output_policy_script06 <- tibble(
  policy_id = 1:8,
  policy_area = c(
    "Input microdata",
    "Clean analytical microdata",
    "Public outputs",
    "Sensitive recodes",
    "Construct scores",
    "Small cells",
    "GitHub",
    "Next step"
  ),
  rule = c(
    "Script 06 reads only local processed Wave I data from data/processed/.",
    "The cleaned analytical RDS remains local in data/processed/.",
    "Only metadata, diagnostics and aggregate summaries are exported.",
    "Sexual behaviour, pregnancy and HIV/STI recodes require cautious interpretation.",
    "Preliminary construct diagnostics are not final validated scales.",
    "Cells below disclosure threshold should be suppressed in later descriptive tables.",
    "Do not commit data/raw/, data/processed/ or outputs/private_review/.",
    "Script 07 should produce descriptive tables only after reviewing recode quality."
  )
)


# ============================================================
# 15. Methodological notes
# ============================================================

script06_methodological_notes <- tibble(
  note_id = 1:10,
  note = c(
    "Script 06 cleans and recodes imported Add Health Wave I variables.",
    "The script does not read raw .sav files.",
    "The script reads the local-only processed Wave I RDS created by Script 05.",
    "The cleaned analytical dataset is saved locally in data/processed/.",
    "The public outputs contain no individual-level microdata.",
    "Grade 10-12 remains the main sample definition.",
    "Age 15-19 remains a complementary or sensitivity alignment criterion.",
    "Generic numeric cleaned variables are created for diagnostic and later modeling use.",
    "Behavioural and sensitive recodes should be checked against the official codebook before final interpretation.",
    "Construct scores are not finalized in this script; they require directionality and reliability review."
  )
)


# ============================================================
# 16. Execution checklist
# ============================================================

script06_checklist <- tibble(
  check_id = 1:18,
  check_item = c(
    "Project root exists",
    "Input Wave I processed RDS exists",
    "Confirmed dictionary exists",
    "Script 05 metadata exists",
    "Script 05 import check exists",
    "Wave I processed data loaded",
    "Key variables identified",
    "Sample flags created",
    "Generic numeric cleaned variables created",
    "Specific behavioural recodes attempted",
    "Construct availability summary created",
    "Construct score diagnostics created",
    "Clean analytical local-only RDS saved",
    "Analytic variable inventory exported",
    "Missingness summary exported",
    "Sample definition summary exported",
    "Excel diagnostic workbook exported",
    "Markdown documentation exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(input_rds_path), "OK", "FAIL"),
    ifelse(file.exists(confirmed_dictionary_path), "OK", "FAIL"),
    ifelse(file.exists(script05_metadata_path), "OK", "FAIL"),
    ifelse(file.exists(script05_import_check_path), "OK", "FAIL"),
    "OK",
    ifelse(!is.na(grade_var) & !is.na(age_var) & !is.na(sex_var), "OK", "WARNING_KEY_VARIABLES_PARTIAL"),
    "OK",
    ifelse(length(numeric_clean_names) > 0, "OK", "FAIL"),
    "OK",
    "OK",
    "OK",
    ifelse(file.exists(output_clean_rds_path), "OK", "FAIL"),
    "PENDING",
    "PENDING",
    "PENDING",
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 17. Export public CSV outputs
# ============================================================

write_csv(
  analytic_variable_inventory,
  file.path(outputs_tables_dir, "script06_wave01_analytic_variable_inventory.csv")
)

write_csv(
  analytic_missingness_summary,
  file.path(outputs_tables_dir, "script06_wave01_missingness_summary.csv")
)

write_csv(
  sample_definition_summary,
  file.path(outputs_tables_dir, "script06_wave01_sample_definition_summary.csv")
)

write_csv(
  construct_availability_summary,
  file.path(outputs_tables_dir, "script06_wave01_construct_availability_summary.csv")
)

write_csv(
  construct_score_diagnostics,
  file.path(outputs_tables_dir, "script06_wave01_construct_score_diagnostics.csv")
)

write_csv(
  recode_quality_check,
  file.path(outputs_tables_dir, "script06_wave01_recode_quality_check.csv")
)

write_csv(
  safe_public_output_policy_script06,
  file.path(outputs_tables_dir, "script06_wave01_safe_public_output_policy.csv")
)

write_csv(
  script06_methodological_notes,
  file.path(outputs_tables_dir, "script06_wave01_methodological_notes.csv")
)

script06_checklist$status[
  script06_checklist$check_item %in% c(
    "Analytic variable inventory exported",
    "Missingness summary exported",
    "Sample definition summary exported"
  )
] <- "OK"


# ============================================================
# 18. Export diagnostic Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script06_wave01_clean_recode_diagnostics.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "variable_inventory")
writeData(wb, "variable_inventory", analytic_variable_inventory)

addWorksheet(wb, "missingness")
writeData(wb, "missingness", analytic_missingness_summary)

addWorksheet(wb, "sample_definition")
writeData(wb, "sample_definition", sample_definition_summary)

addWorksheet(wb, "construct_availability")
writeData(wb, "construct_availability", construct_availability_summary)

addWorksheet(wb, "construct_score_diag")
writeData(wb, "construct_score_diag", construct_score_diagnostics)

addWorksheet(wb, "recode_quality")
writeData(wb, "recode_quality", recode_quality_check)

addWorksheet(wb, "safe_policy")
writeData(wb, "safe_policy", safe_public_output_policy_script06)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script06_methodological_notes)

addWorksheet(wb, "script06_checklist")
writeData(wb, "script06_checklist", script06_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:60, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script06_checklist$status[
  script06_checklist$check_item == "Excel diagnostic workbook exported"
] <- "OK"


# ============================================================
# 19. Export Markdown documentation
# ============================================================

script06_doc <- c(
  "# Clean and Recode Wave I Analytical Variables",
  "",
  "Script 06 cleans and recodes the Add Health Wave I variables imported by Script 05.",
  "",
  "## Input",
  "",
  "`data/processed/add_health_wave01_confirmed_variables_local_only.rds`",
  "",
  "## Output",
  "",
  "`data/processed/add_health_wave01_analytical_clean_local_only.rds`",
  "",
  "The output is a local-only individual-level analytical dataset and must not be committed to GitHub.",
  "",
  "## Public outputs",
  "",
  "- analytical variable inventory;",
  "- missingness summary;",
  "- sample definition summary;",
  "- construct availability summary;",
  "- recode quality checklist;",
  "- safe public output policy.",
  "",
  "## Sample definition",
  "",
  "The main analytical sample remains students in grades 10 to 12 at Wave I.",
  "",
  "Age 15 to 19 remains a complementary alignment and sensitivity criterion.",
  "",
  "## Important limitation",
  "",
  "This script creates recode diagnostics and preliminary analytical variables. Final construct scores require directionality and reliability review in a later script.",
  "",
  "## Next step",
  "",
  "Script 07 should produce descriptive aggregate tables for the Wave I analytical sample after recode quality has been reviewed."
)

writeLines(
  script06_doc,
  con = file.path(docs_dir, "clean_recode_wave01_analytical_variables_script06.md")
)

script06_checklist$status[
  script06_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 20. Save final checklist
# ============================================================

write_csv(
  script06_checklist,
  file.path(outputs_diag_dir, "script06_execution_checklist.csv")
)


# ============================================================
# 21. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 06 completed: Clean and Recode Wave I Analytical Variables\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Input local processed file:\n")
cat(input_rds_path, "\n\n")

cat("Output clean analytical local-only file:\n")
cat(output_clean_rds_path, "\n\n")

cat("Analytical data summary:\n")
cat("- Observations: ", nrow(analytical_data), "\n", sep = "")
cat("- Variables after cleaning and recoding: ", ncol(analytical_data), "\n", sep = "")
cat("- Original imported variables: ", length(original_imported_variables), "\n", sep = "")
cat("- Generic numeric cleaned variables: ", length(numeric_clean_names), "\n\n", sep = "")

cat("Sample definition:\n")
cat("- Grade variable used: ", ifelse(is.na(grade_var), "not_found", grade_var), "\n", sep = "")
cat("- Age variable used: ", ifelse(is.na(age_var), "not_found", age_var), "\n", sep = "")
cat("- Sex variable used: ", ifelse(is.na(sex_var), "not_found", sex_var), "\n", sep = "")
cat("- Grade 10-12 observations: ", count_true(analytical_data$a_sample_grade_10_12), "\n", sep = "")
cat("- Age 15-19 observations: ", count_true(analytical_data$a_sample_age_15_19), "\n", sep = "")
cat("- Strict grade 10-12 and age 15-19 observations: ", count_true(analytical_data$a_strict_sample_grade_age), "\n\n", sep = "")

cat("Public outputs created:\n")
cat("- outputs/tables/script06_wave01_analytic_variable_inventory.csv\n")
cat("- outputs/tables/script06_wave01_missingness_summary.csv\n")
cat("- outputs/tables/script06_wave01_sample_definition_summary.csv\n")
cat("- outputs/tables/script06_wave01_construct_availability_summary.csv\n")
cat("- outputs/tables/script06_wave01_construct_score_diagnostics.csv\n")
cat("- outputs/tables/script06_wave01_recode_quality_check.csv\n")
cat("- outputs/tables/script06_wave01_safe_public_output_policy.csv\n")
cat("- outputs/tables/script06_wave01_methodological_notes.csv\n")
cat("- outputs/tables/script06_wave01_clean_recode_diagnostics.xlsx\n")
cat("- outputs/diagnostics/script06_execution_checklist.csv\n")
cat("- docs/clean_recode_wave01_analytical_variables_script06.md\n\n")

cat("Recode quality check:\n")
print(recode_quality_check)

cat("\nConstruct score diagnostics:\n")
print(construct_score_diagnostics)

cat("\nExecution checklist:\n")
print(script06_checklist)

cat("\nImportant note:\n")
cat("Do not commit data/raw/, data/processed/ or outputs/private_review/ to GitHub.\n")
cat("Final scale/index construction should be handled in a later script after directionality and reliability checks.\n\n")