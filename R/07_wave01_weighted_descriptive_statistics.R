# ============================================================
# Project: add-health-adolescent-risk-models
# Script 07: Wave I Weighted Descriptive Statistics
# Author: Gelo Picol
#
# Purpose:
#   Produce aggregate descriptive statistics for the Add Health
#   Wave I analytical sample using the Wave I grand sample weight
#   GSWGT1.
#
# Key methodological decision:
#   - GSWGT1 is the correct Wave I cross-sectional population-
#     average sampling weight.
#   - AID is needed only internally to merge weights.
#   - AID is not exported in public outputs.
#   - If AID is missing from the analytical RDS, it is recovered
#     from the raw Wave I file only when row counts match exactly.
#   - CLUSTER2 is optional and is used only if available.
# ============================================================


# ============================================================
# 0. Project root and options
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"

options(na.print = "NA")
options(survey.lonely.psu = "adjust")


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
  "openxlsx",
  "survey",
  "haven"
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
library(survey)
library(haven)


# ============================================================
# 2. Paths
# ============================================================

data_processed_dir <- file.path(project_root, "data/processed")
data_raw_dir       <- file.path(project_root, "data/raw")
outputs_tables_dir <- file.path(project_root, "outputs/tables")
outputs_diag_dir   <- file.path(project_root, "outputs/diagnostics")
docs_dir           <- file.path(project_root, "docs")

dir.create(outputs_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(outputs_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

analytical_rds_path <- file.path(
  data_processed_dir,
  "add_health_wave01_analytical_clean_local_only.rds"
)

wave1_raw_rda_path <- file.path(
  data_raw_dir,
  "21600-0001-Data.rda"
)

wave1_raw_sav_path <- file.path(
  data_raw_dir,
  "21600-0001-Data.sav"
)

weights_wave1_path <- file.path(
  data_raw_dir,
  "21600-0004-Data.rda"
)

cluster_wave1_path <- file.path(
  data_raw_dir,
  "21600-0021-Data.rda"
)

school_weight_path <- file.path(
  data_raw_dir,
  "21600-0019-Data.rda"
)

output_weighted_rds_path <- file.path(
  data_processed_dir,
  "add_health_wave01_analytical_weighted_local_only.rds"
)


# ============================================================
# 3. Helper functions
# ============================================================

load_rda_first_dataframe <- function(path) {
  env <- new.env()
  loaded_objects <- load(path, envir = env)
  
  data_objects <- loaded_objects[
    vapply(
      loaded_objects,
      function(obj) is.data.frame(env[[obj]]),
      logical(1)
    )
  ]
  
  if (length(data_objects) == 0) {
    stop(paste0("No data.frame object found in: ", path))
  }
  
  env[[data_objects[1]]]
}

load_wave1_raw_for_aid <- function(rda_path, sav_path) {
  if (file.exists(rda_path)) {
    return(load_rda_first_dataframe(rda_path))
  }
  
  if (file.exists(sav_path)) {
    return(haven::read_sav(sav_path, user_na = TRUE))
  }
  
  stop("Neither 21600-0001-Data.rda nor 21600-0001-Data.sav was found.")
}

find_col <- function(data, candidates) {
  data_names_upper <- stringr::str_to_upper(names(data))
  candidates_upper <- stringr::str_to_upper(candidates)
  
  hit <- candidates_upper[candidates_upper %in% data_names_upper]
  
  if (length(hit) == 0) {
    return(NA_character_)
  }
  
  names(data)[match(hit[1], data_names_upper)]
}

to_num <- function(x) {
  suppressWarnings(as.numeric(haven::zap_labels(x)))
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

format_pct <- function(x) {
  ifelse(is.na(x), NA_character_, sprintf("%.2f", x))
}

safe_pct_public <- function(pct, n, threshold = 10) {
  ifelse(
    is.na(n) | n < threshold,
    "suppressed",
    format_pct(pct)
  )
}

weighted_mean_manual <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  sum(x[ok] * w[ok]) / sum(w[ok])
}

weighted_pct_manual <- function(indicator, w) {
  ok <- !is.na(indicator) & !is.na(w) & w > 0
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  100 * sum((indicator[ok] == TRUE) * w[ok]) / sum(w[ok])
}

make_survey_design <- function(data) {
  
  # Script 07 focuses on weighted point estimates using GSWGT1.
  # CLUSTER2 is retained as a diagnostic variable, but not used as
  # the survey id here because it has missing values in the current
  # public-use merge. Full design-based standard errors require a
  # complete cluster/strata specification.
  
  survey::svydesign(
    ids = ~1,
    weights = ~GSWGT1,
    data = data
  )
}

weighted_category_table <- function(design, variable, sample_name) {
  if (!variable %in% names(design$variables)) {
    return(tibble())
  }
  
  x <- design$variables[[variable]]
  nonmiss_index <- !is.na(x)
  
  if (sum(nonmiss_index) == 0) {
    return(tibble())
  }
  
  design_nonmiss <- design[nonmiss_index, ]
  
  unweighted_tab <- design_nonmiss$variables %>%
    count(category = as.character(.data[[variable]]), name = "unweighted_n")
  
  weighted_tab <- tryCatch(
    {
      tmp <- as.data.frame(
        survey::svytable(
          as.formula(paste0("~", variable)),
          design_nonmiss
        )
      )
      
      names(tmp)[1] <- "category"
      names(tmp)[2] <- "weighted_n"
      
      tmp %>%
        mutate(
          category = as.character(category),
          weighted_pct = 100 * weighted_n / sum(weighted_n, na.rm = TRUE)
        )
    },
    error = function(e) {
      tibble(
        category = character(),
        weighted_n = numeric(),
        weighted_pct = numeric()
      )
    }
  )
  
  full_join(
    unweighted_tab,
    weighted_tab,
    by = "category"
  ) %>%
    mutate(
      sample_name = sample_name,
      variable = variable,
      public_unweighted_n = vapply(unweighted_n, safe_count_public, character(1)),
      public_weighted_pct = safe_pct_public(weighted_pct, unweighted_n),
      disclosure_rule = ifelse(
        is.na(unweighted_n) | unweighted_n < 10,
        "suppressed_small_cell",
        "reported"
      )
    ) %>%
    arrange(sample_name, variable, category) %>%
    select(
      sample_name,
      variable,
      category,
      unweighted_n,
      public_unweighted_n,
      weighted_n,
      weighted_pct,
      public_weighted_pct,
      disclosure_rule
    )
}

weighted_continuous_table <- function(design, variable, sample_name) {
  if (!variable %in% names(design$variables)) {
    return(tibble())
  }
  
  x <- design$variables[[variable]]
  nonmiss_index <- !is.na(x)
  
  if (sum(nonmiss_index) == 0) {
    return(tibble())
  }
  
  design_nonmiss <- design[nonmiss_index, ]
  
  mean_obj <- tryCatch(
    survey::svymean(
      as.formula(paste0("~", variable)),
      design_nonmiss,
      na.rm = TRUE
    ),
    error = function(e) NULL
  )
  
  q_obj <- tryCatch(
    survey::svyquantile(
      as.formula(paste0("~", variable)),
      design_nonmiss,
      quantiles = c(0.25, 0.50, 0.75),
      na.rm = TRUE
    ),
    error = function(e) NULL
  )
  
  q_values <- if (!is.null(q_obj)) {
    as.numeric(q_obj[[1]])
  } else {
    c(NA_real_, NA_real_, NA_real_)
  }
  
  tibble(
    sample_name = sample_name,
    variable = variable,
    unweighted_n = sum(nonmiss_index),
    public_unweighted_n = safe_count_public(sum(nonmiss_index)),
    unweighted_mean = mean(design_nonmiss$variables[[variable]], na.rm = TRUE),
    weighted_mean = if (!is.null(mean_obj)) as.numeric(coef(mean_obj)[1]) else NA_real_,
    weighted_se = if (!is.null(mean_obj)) as.numeric(SE(mean_obj)[1]) else NA_real_,
    weighted_q25 = q_values[1],
    weighted_median = q_values[2],
    weighted_q75 = q_values[3]
  )
}


# ============================================================
# 4. Check required inputs
# ============================================================

required_inputs <- c(
  analytical_rds_path,
  weights_wave1_path
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
# 5. Load analytical data
# ============================================================

analytical_data <- readRDS(analytical_rds_path)

initial_has_aid <- "AID" %in% names(analytical_data)


# ============================================================
# 6. Recover AID if missing
# ============================================================

aid_recovery_status <- "not_needed_aid_already_present"

if (!initial_has_aid) {
  
  wave1_raw_for_aid <- load_wave1_raw_for_aid(
    rda_path = wave1_raw_rda_path,
    sav_path = wave1_raw_sav_path
  )
  
  aid_col_raw <- find_col(wave1_raw_for_aid, "AID")
  
  if (is.na(aid_col_raw)) {
    stop("AID not found in raw Wave I DS0001 file.")
  }
  
  if (nrow(wave1_raw_for_aid) != nrow(analytical_data)) {
    stop(
      paste0(
        "Cannot recover AID by row order because row counts differ. ",
        "Raw Wave I rows: ", nrow(wave1_raw_for_aid),
        "; analytical rows: ", nrow(analytical_data)
      )
    )
  }
  
  analytical_data <- analytical_data %>%
    mutate(
      AID = as.character(wave1_raw_for_aid[[aid_col_raw]])
    )
  
  aid_recovery_status <- "recovered_from_ds0001_by_row_order"
}

analytical_data <- analytical_data %>%
  mutate(AID = as.character(AID))


# ============================================================
# 7. Load Wave I population-average weight: DS0004
# ============================================================

weights_wave1_raw <- load_rda_first_dataframe(weights_wave1_path)

aid_col_w <- find_col(weights_wave1_raw, "AID")
gswgt1_col <- find_col(weights_wave1_raw, "GSWGT1")

if (is.na(aid_col_w) || is.na(gswgt1_col)) {
  stop("AID or GSWGT1 not found in 21600-0004-Data.rda.")
}

weights_wave1 <- weights_wave1_raw %>%
  transmute(
    AID = as.character(.data[[aid_col_w]]),
    GSWGT1 = to_num(.data[[gswgt1_col]])
  ) %>%
  distinct(AID, .keep_all = TRUE)


# ============================================================
# 8. Optional cluster variable: DS0021
# ============================================================

cluster_available <- file.exists(cluster_wave1_path)

if (cluster_available) {
  
  cluster_wave1_raw <- load_rda_first_dataframe(cluster_wave1_path)
  
  aid_col_c <- find_col(cluster_wave1_raw, "AID")
  cluster_col <- find_col(
    cluster_wave1_raw,
    c("CLUSTER2", "PSUSCID", "SCHOOL", "SCID")
  )
  
  if (!is.na(aid_col_c) && !is.na(cluster_col)) {
    cluster_wave1 <- cluster_wave1_raw %>%
      transmute(
        AID = as.character(.data[[aid_col_c]]),
        CLUSTER2 = as.character(.data[[cluster_col]])
      ) %>%
      distinct(AID, .keep_all = TRUE)
  } else {
    cluster_available <- FALSE
    cluster_wave1 <- tibble(AID = character(), CLUSTER2 = character())
  }
  
} else {
  cluster_wave1 <- tibble(AID = character(), CLUSTER2 = character())
}


# ============================================================
# 9. Optional school-level weight: DS0019
# ============================================================

school_weight_available <- file.exists(school_weight_path)

if (school_weight_available) {
  
  school_weight_raw <- load_rda_first_dataframe(school_weight_path)
  
  cluster_col_s <- find_col(
    school_weight_raw,
    c("CLUSTER2", "PSUSCID", "SCHOOL", "SCID")
  )
  
  schwt1_col <- find_col(school_weight_raw, "SCHWT1")
  
  if (!is.na(cluster_col_s) && !is.na(schwt1_col)) {
    school_weight <- school_weight_raw %>%
      transmute(
        CLUSTER2 = as.character(.data[[cluster_col_s]]),
        SCHWT1 = to_num(.data[[schwt1_col]])
      ) %>%
      distinct(CLUSTER2, .keep_all = TRUE)
  } else {
    school_weight_available <- FALSE
    school_weight <- tibble(CLUSTER2 = character(), SCHWT1 = numeric())
  }
  
} else {
  school_weight <- tibble(CLUSTER2 = character(), SCHWT1 = numeric())
}


# ============================================================
# 10. Merge weights and optional design variables
# ============================================================

analysis_data <- analytical_data %>%
  left_join(weights_wave1, by = "AID")

if (nrow(cluster_wave1) > 0) {
  analysis_data <- analysis_data %>%
    left_join(cluster_wave1, by = "AID")
} else {
  analysis_data <- analysis_data %>%
    mutate(CLUSTER2 = NA_character_)
}

if (nrow(school_weight) > 0) {
  analysis_data <- analysis_data %>%
    left_join(school_weight, by = "CLUSTER2")
} else {
  analysis_data <- analysis_data %>%
    mutate(SCHWT1 = NA_real_)
}

analysis_data <- analysis_data %>%
  mutate(
    valid_gswgt1 = !is.na(GSWGT1) & GSWGT1 > 0,
    valid_cluster2 = !is.na(CLUSTER2),
    valid_schwt1 = !is.na(SCHWT1) & SCHWT1 > 0
  )

analysis_data_valid <- analysis_data %>%
  filter(valid_gswgt1)

if (nrow(analysis_data_valid) == 0) {
  stop("No observations with valid GSWGT1 after merge.")
}


# ============================================================
# 11. Save local-only weighted analytical file
# ============================================================

saveRDS(
  analysis_data,
  output_weighted_rds_path
)


# ============================================================
# 12. Create statistical variables
# ============================================================

analysis_data_valid <- analysis_data_valid %>%
  mutate(
    stat_grade = case_when(
      !is.na(a_grade_wave1) ~ paste0("Grade ", as.integer(a_grade_wave1)),
      TRUE ~ NA_character_
    ),
    stat_age_group = case_when(
      is.na(a_age_wave1) ~ NA_character_,
      a_age_wave1 < 15 ~ "<15",
      a_age_wave1 >= 15 & a_age_wave1 <= 19 ~ "15-19",
      a_age_wave1 > 19 ~ ">19",
      TRUE ~ NA_character_
    ),
    stat_sex = case_when(
      a_female == 1 ~ "Female",
      a_female == 0 ~ "Male",
      TRUE ~ NA_character_
    ),
    stat_main_sample = case_when(
      a_main_sample_grade_10_12 == TRUE ~ "Grade 10-12",
      a_main_sample_grade_10_12 == FALSE ~ "Other grades",
      TRUE ~ NA_character_
    ),
    stat_strict_sample = case_when(
      a_strict_sample_grade_age == TRUE ~ "Grade 10-12 and age 15-19",
      a_strict_sample_grade_age == FALSE ~ "Other",
      TRUE ~ NA_character_
    ),
    stat_sex_ever = case_when(
      a_sex_ever == 1 ~ "Yes",
      a_sex_ever == 0 ~ "No",
      TRUE ~ NA_character_
    )
  )

binary_recode_vars <- names(analysis_data_valid)[
  stringr::str_detect(names(analysis_data_valid), "^a_.*_yesno$")
]

for (v in binary_recode_vars) {
  new_v <- paste0("stat_", v)
  
  analysis_data_valid[[new_v]] <- dplyr::case_when(
    analysis_data_valid[[v]] == 1 ~ "Yes",
    analysis_data_valid[[v]] == 0 ~ "No",
    TRUE ~ NA_character_
  )
}

categorical_vars <- c(
  "stat_grade",
  "stat_age_group",
  "stat_sex",
  "stat_main_sample",
  "stat_strict_sample",
  "stat_sex_ever",
  paste0("stat_", binary_recode_vars)
)

categorical_vars <- categorical_vars[
  categorical_vars %in% names(analysis_data_valid)
]

continuous_vars <- c(
  "a_age_wave1"
)

continuous_vars <- continuous_vars[
  continuous_vars %in% names(analysis_data_valid)
]


# ============================================================
# 13. Create survey designs
# ============================================================

wave1_design <- make_survey_design(
  analysis_data_valid
)

wave1_design_main <- make_survey_design(
  analysis_data_valid %>%
    filter(a_main_sample_grade_10_12 == TRUE)
)

wave1_design_strict <- make_survey_design(
  analysis_data_valid %>%
    filter(a_strict_sample_grade_age == TRUE)
)

sample_designs <- list(
  full_weighted_wave1 = wave1_design,
  main_grade_10_12 = wave1_design_main,
  strict_grade_10_12_age_15_19 = wave1_design_strict
)

# ============================================================
# 14. Diagnostics
# ============================================================

weight_design_diagnostics <- tibble(
  diagnostic_id = 1:18,
  item = c(
    "Total observations in analytical file",
    "AID initially present in Script 06 analytical RDS",
    "AID recovery status",
    "Observations with valid GSWGT1",
    "Observations without valid GSWGT1",
    "Main grade 10-12 sample with valid GSWGT1",
    "Strict grade 10-12 and age 15-19 sample with valid GSWGT1",
    "GSWGT1 minimum",
    "GSWGT1 maximum",
    "GSWGT1 mean",
    "GSWGT1 sum",
    "CLUSTER2 file DS0021 available",
    "CLUSTER2 available after merge",
    "Number of non-missing CLUSTER2 values",
    "Number of unique CLUSTER2 values",
    "SCHWT1 available after merge",
    "REGION available",
    "W1_WC available"
  ),
  value = c(
    nrow(analysis_data),
    initial_has_aid,
    aid_recovery_status,
    sum(analysis_data$valid_gswgt1, na.rm = TRUE),
    sum(!analysis_data$valid_gswgt1, na.rm = TRUE),
    sum(analysis_data_valid$a_main_sample_grade_10_12 == TRUE, na.rm = TRUE),
    sum(analysis_data_valid$a_strict_sample_grade_age == TRUE, na.rm = TRUE),
    min(analysis_data_valid$GSWGT1, na.rm = TRUE),
    max(analysis_data_valid$GSWGT1, na.rm = TRUE),
    mean(analysis_data_valid$GSWGT1, na.rm = TRUE),
    sum(analysis_data_valid$GSWGT1, na.rm = TRUE),
    cluster_available,
    any(analysis_data$valid_cluster2, na.rm = TRUE),
    sum(analysis_data$valid_cluster2, na.rm = TRUE),
    dplyr::n_distinct(analysis_data$CLUSTER2[analysis_data$valid_cluster2]),
    any(analysis_data$valid_schwt1, na.rm = TRUE),
    "not_found_in_available_local_files",
    "not_found_in_available_local_files"
  ),
  note = c(
    "Local-only analytical file produced by Script 06.",
    "AID is required internally for merging survey weights.",
    "If recovered, recovery was done from DS0001 by row order after exact row-count match.",
    "Cases usable for weighted Wave I descriptive statistics.",
    "Cases excluded from weighted estimates.",
    "Primary analytical sample.",
    "Sensitivity sample aligned with thesis age range.",
    "Aggregate diagnostic only.",
    "Aggregate diagnostic only.",
    "Aggregate diagnostic only.",
    "Aggregate diagnostic only.",
    "DS0021 is an auxiliary file number, not Wave II.",
    "Cluster variable merged from DS0021 when available.",
    "Aggregate diagnostic only.",
    "Aggregate diagnostic only.",
    "School-level weight merged from DS0019 when available.",
    "REGION was not found in the available local files inspected so far.",
    "W1_WC was not found in the available local files inspected so far."
  )
)


# ============================================================
# 15. Weighted categorical descriptives
# ============================================================

categorical_descriptives <- purrr::imap_dfr(
  sample_designs,
  function(design_obj, sample_name) {
    purrr::map_dfr(
      categorical_vars,
      function(v) weighted_category_table(design_obj, v, sample_name)
    )
  }
)


# ============================================================
# 16. Weighted continuous descriptives
# ============================================================

continuous_descriptives <- purrr::imap_dfr(
  sample_designs,
  function(design_obj, sample_name) {
    purrr::map_dfr(
      continuous_vars,
      function(v) weighted_continuous_table(design_obj, v, sample_name)
    )
  }
)


# ============================================================
# 17. Outcome availability summary
# ============================================================

outcome_vars <- c("a_sex_ever", binary_recode_vars)
outcome_vars <- outcome_vars[outcome_vars %in% names(analysis_data_valid)]

outcome_availability <- tibble(
  variable = outcome_vars,
  variable_group = case_when(
    variable == "a_sex_ever" ~ "sexual_initiation",
    stringr::str_detect(variable, "H1CO8|H1CO9") ~ "condom_use",
    stringr::str_detect(variable, "H1CO3|H1CO6|H1CO13") ~ "contraceptive_use",
    stringr::str_detect(variable, "H1FP7|H1FP8") ~ "pregnancy_outcome",
    stringr::str_detect(variable, "H1CO16|H1HS9") ~ "hiv_sti_outcome",
    TRUE ~ "other_binary_recode"
  )
) %>%
  rowwise() %>%
  mutate(
    full_unweighted_nonmissing = sum(!is.na(analysis_data_valid[[variable]])),
    main_unweighted_nonmissing = sum(
      !is.na(analysis_data_valid[[variable]]) &
        analysis_data_valid$a_main_sample_grade_10_12 == TRUE,
      na.rm = TRUE
    ),
    strict_unweighted_nonmissing = sum(
      !is.na(analysis_data_valid[[variable]]) &
        analysis_data_valid$a_strict_sample_grade_age == TRUE,
      na.rm = TRUE
    ),
    public_full_n = safe_count_public(full_unweighted_nonmissing),
    public_main_n = safe_count_public(main_unweighted_nonmissing),
    public_strict_n = safe_count_public(strict_unweighted_nonmissing)
  ) %>%
  ungroup()


# ============================================================
# 18. Methodological notes
# ============================================================

script07_methodological_notes <- tibble(
  note_id = 1:12,
  note = c(
    "Script 07 produces aggregate descriptive statistics for Add Health Wave I.",
    "The main analytical sample is students in grades 10 to 12 at Wave I.",
    "The strict sensitivity sample is students in grades 10 to 12 and ages 15 to 19.",
    "GSWGT1 is used as the Wave I population-average sampling weight.",
    "AID is used only internally to merge GSWGT1 and is not exported in public outputs.",
    "If AID was absent from the analytical RDS, it was recovered from DS0001 only after exact row-count validation.",
    "DS0021 is an auxiliary ICPSR dataset number, not Wave II.",
    "CLUSTER2 is used as the available cluster variable when DS0021 is present.",
    "REGION was not found in the currently available local files and is documented as a limitation.",
    "W1_WC was not found in the currently available local files and is not used in this script.",
    "SCHWT1 was located but is not needed for the main population-average descriptive tables.",
    "No individual-level data are exported by this script."
  )
)


# ============================================================
# 19. Execution checklist
# ============================================================

script07_checklist <- tibble(
  check_id = 1:18,
  check_item = c(
    "Project root exists",
    "Analytical clean local-only RDS exists",
    "Wave I raw DS0001 exists for AID recovery",
    "Wave I weight file DS0004 exists",
    "Analytical data loaded",
    "AID available or recovered",
    "GSWGT1 loaded",
    "GSWGT1 merged by AID",
    "CLUSTER2 checked",
    "SCHWT1 checked",
    "Survey design created",
    "Weighted categorical descriptives created",
    "Weighted continuous descriptives created",
    "Outcome availability summary created",
    "Weighted analytical local-only RDS saved",
    "CSV outputs exported",
    "Excel diagnostic workbook exported",
    "Markdown documentation exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(analytical_rds_path), "OK", "FAIL"),
    ifelse(file.exists(wave1_raw_rda_path) | file.exists(wave1_raw_sav_path), "OK", "FAIL"),
    ifelse(file.exists(weights_wave1_path), "OK", "FAIL"),
    "OK",
    ifelse("AID" %in% names(analytical_data), "OK", "FAIL"),
    ifelse("GSWGT1" %in% names(analysis_data), "OK", "FAIL"),
    "OK",
    ifelse("CLUSTER2" %in% names(analysis_data), "OK", "WARNING_NOT_AVAILABLE"),
    ifelse("SCHWT1" %in% names(analysis_data), "OK", "WARNING_NOT_AVAILABLE"),
    "OK",
    ifelse(nrow(categorical_descriptives) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(continuous_descriptives) > 0, "OK", "WARNING_EMPTY"),
    ifelse(nrow(outcome_availability) > 0, "OK", "WARNING_EMPTY"),
    ifelse(file.exists(output_weighted_rds_path), "OK", "FAIL"),
    "PENDING",
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 20. Export public CSV outputs
# ============================================================

write_csv(
  weight_design_diagnostics,
  file.path(outputs_tables_dir, "script07_wave01_weight_design_diagnostics.csv")
)

write_csv(
  categorical_descriptives,
  file.path(outputs_tables_dir, "script07_wave01_weighted_categorical_descriptives.csv")
)

write_csv(
  continuous_descriptives,
  file.path(outputs_tables_dir, "script07_wave01_weighted_continuous_descriptives.csv")
)

write_csv(
  outcome_availability,
  file.path(outputs_tables_dir, "script07_wave01_outcome_availability.csv")
)

write_csv(
  script07_methodological_notes,
  file.path(outputs_tables_dir, "script07_wave01_methodological_notes.csv")
)

script07_checklist$status[
  script07_checklist$check_item == "CSV outputs exported"
] <- "OK"


# ============================================================
# 21. Excel workbook
# ============================================================

xlsx_path <- file.path(
  outputs_tables_dir,
  "script07_wave01_weighted_descriptive_statistics.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "weight_design")
writeData(wb, "weight_design", weight_design_diagnostics)

addWorksheet(wb, "categorical")
writeData(wb, "categorical", categorical_descriptives)

addWorksheet(wb, "continuous")
writeData(wb, "continuous", continuous_descriptives)

addWorksheet(wb, "outcome_availability")
writeData(wb, "outcome_availability", outcome_availability)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script07_methodological_notes)

addWorksheet(wb, "checklist")
writeData(wb, "checklist", script07_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:60, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script07_checklist$status[
  script07_checklist$check_item == "Excel diagnostic workbook exported"
] <- "OK"


# ============================================================
# 22. Markdown documentation
# ============================================================

script07_doc <- c(
  "# Wave I Weighted Descriptive Statistics",
  "",
  "Script 07 produces aggregate descriptive statistics for Add Health Wave I.",
  "",
  "## Weighting",
  "",
  "The script uses `GSWGT1` as the Wave I population-average sampling weight.",
  "",
  "## AID",
  "",
  "`AID` is used only internally for merging survey weights. It is not exported in public outputs.",
  "",
  "If `AID` is missing from the analytical RDS, it is recovered from DS0001 only when the row count matches exactly.",
  "",
  "## DS0021",
  "",
  "`21600-0021-Data.rda` is an auxiliary ICPSR dataset number. It is not Wave II. It is used only if it provides `AID` and `CLUSTER2`.",
  "",
  "## Main sample",
  "",
  "The main analytical sample is students in grades 10 to 12 at Wave I.",
  "",
  "## Strict sensitivity sample",
  "",
  "The strict sensitivity sample is students in grades 10 to 12 and ages 15 to 19.",
  "",
  "## Design limitation",
  "",
  "`REGION` and `W1_WC` were not found in the currently available local files inspected so far.",
  "",
  "## Public outputs",
  "",
  "Only aggregate diagnostics and descriptive tables are exported. No individual-level microdata are exported.",
  "",
  "## Next step",
  "",
  "Script 08 should review missing-data patterns and final recoding decisions before regression modeling."
)

writeLines(
  script07_doc,
  con = file.path(docs_dir, "wave01_weighted_descriptive_statistics_script07.md")
)

script07_checklist$status[
  script07_checklist$check_item == "Markdown documentation exported"
] <- "OK"


# ============================================================
# 23. Save final checklist
# ============================================================

write_csv(
  script07_checklist,
  file.path(outputs_diag_dir, "script07_execution_checklist.csv")
)


# ============================================================
# 24. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 07 completed: Wave I Weighted Descriptive Statistics\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Main input:\n")
cat(analytical_rds_path, "\n\n")

cat("AID status:\n")
cat("- AID initially present: ", initial_has_aid, "\n", sep = "")
cat("- AID recovery status: ", aid_recovery_status, "\n\n", sep = "")

cat("Weight inputs:\n")
cat("- DS0004 / GSWGT1: ", weights_wave1_path, "\n", sep = "")
cat("- DS0021 / CLUSTER2 optional: ", cluster_wave1_path, "\n", sep = "")
cat("- DS0019 / SCHWT1 optional: ", school_weight_path, "\n\n", sep = "")

cat("Weighted analysis file:\n")
cat("- Total observations: ", nrow(analysis_data), "\n", sep = "")
cat("- Observations with valid GSWGT1: ", nrow(analysis_data_valid), "\n", sep = "")
cat("- Main grade 10-12 sample with valid GSWGT1: ",
    sum(analysis_data_valid$a_main_sample_grade_10_12 == TRUE, na.rm = TRUE), "\n", sep = "")
cat("- Strict grade 10-12 and age 15-19 sample with valid GSWGT1: ",
    sum(analysis_data_valid$a_strict_sample_grade_age == TRUE, na.rm = TRUE), "\n\n", sep = "")

cat("Design variables:\n")
cat("- GSWGT1 available: ", "GSWGT1" %in% names(analysis_data), "\n", sep = "")
cat("- CLUSTER2 available: ", "CLUSTER2" %in% names(analysis_data), "\n", sep = "")
cat("- SCHWT1 available: ", "SCHWT1" %in% names(analysis_data), "\n", sep = "")
cat("- REGION available: FALSE\n")
cat("- W1_WC available: FALSE\n\n")

cat("Public outputs created:\n")
cat("- outputs/tables/script07_wave01_weight_design_diagnostics.csv\n")
cat("- outputs/tables/script07_wave01_weighted_categorical_descriptives.csv\n")
cat("- outputs/tables/script07_wave01_weighted_continuous_descriptives.csv\n")
cat("- outputs/tables/script07_wave01_outcome_availability.csv\n")
cat("- outputs/tables/script07_wave01_methodological_notes.csv\n")
cat("- outputs/tables/script07_wave01_weighted_descriptive_statistics.xlsx\n")
cat("- outputs/diagnostics/script07_execution_checklist.csv\n")
cat("- docs/wave01_weighted_descriptive_statistics_script07.md\n\n")

cat("Weight and design diagnostics:\n")
print(weight_design_diagnostics)

cat("\nExecution checklist:\n")
print(script07_checklist)

cat("\nImportant note:\n")
cat("Do not commit data/raw/, data/processed/ or any individual-level file to GitHub.\n")
cat("The descriptive outputs are aggregate and public-facing.\n\n")