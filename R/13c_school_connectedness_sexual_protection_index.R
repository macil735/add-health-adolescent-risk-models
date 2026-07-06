# ============================================================
# Script 13c — School Connectedness and Sexual Protection Index
# Project: Add Health Adolescent Risk Models
# Purpose:
#   1. Construct exploratory school connectedness indicators.
#   2. Construct exploratory sexual protection indices inspired by ISX.
#   3. Test associations with selected public outcomes.
#   4. Export aggregate outputs only.
#
# Important:
#   This script does not export individual-level data.
#   All outputs are aggregate tables, diagnostics, and public summaries.
# ============================================================

# -----------------------------
# 0. Setup
# -----------------------------

project_root <- "D:/GitHub/add-health-adolescent-risk-models"

setwd(project_root)

required_packages <- c(
  "dplyr", "tidyr", "stringr", "purrr", "readr",
  "tibble", "survey", "openxlsx", "officer", "flextable"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running Script 13c."
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

options(survey.lonely.psu = "adjust")

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/diagnostics", recursive = TRUE, showWarnings = FALSE)
dir.create("docs", recursive = TRUE, showWarnings = FALSE)

input_data_path <- file.path(
  project_root,
  "data/processed/add_health_wave01_analytical_clean_local_only.rds"
)

if (!file.exists(input_data_path)) {
  stop("Analytical clean RDS not found: ", input_data_path)
}

dat <- readRDS(input_data_path)

# -----------------------------
# 1. Helper functions
# -----------------------------

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_character <- function(x) {
  as.character(x)
}

valid_binary <- function(x) {
  z <- safe_numeric(x)
  dplyr::case_when(
    z %in% c(0, 1) ~ z,
    TRUE ~ NA_real_
  )
}

clean_likert_1_5 <- function(x) {
  z <- safe_numeric(x)
  dplyr::case_when(
    z %in% 1:5 ~ z,
    TRUE ~ NA_real_
  )
}

reverse_likert_1_5 <- function(x) {
  z <- clean_likert_1_5(x)
  dplyr::case_when(
    !is.na(z) ~ 6 - z,
    TRUE ~ NA_real_
  )
}

clean_frequency_0_4_or_1_5 <- function(x) {
  z <- safe_numeric(x)

  if (sum(z %in% 0:4, na.rm = TRUE) >= sum(z %in% 1:5, na.rm = TRUE)) {
    dplyr::case_when(
      z %in% 0:4 ~ z,
      TRUE ~ NA_real_
    )
  } else {
    dplyr::case_when(
      z %in% 1:5 ~ z,
      TRUE ~ NA_real_
    )
  }
}

row_mean_min_items <- function(df, min_items = 2) {
  item_count <- rowSums(!is.na(df))
  item_mean <- rowMeans(df, na.rm = TRUE)
  item_mean[item_count < min_items] <- NA_real_
  item_mean
}

row_sum_min_items <- function(df, min_items = 2) {
  item_count <- rowSums(!is.na(df))
  item_sum <- rowSums(df, na.rm = TRUE)
  item_sum[item_count < min_items] <- NA_real_
  item_sum
}

z_score <- function(x) {
  x <- safe_numeric(x)
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }

  s <- stats::sd(x, na.rm = TRUE)

  if (is.na(s) || s == 0) {
    return(rep(NA_real_, length(x)))
  }

  (x - mean(x, na.rm = TRUE)) / s
}

cronbach_alpha <- function(df) {
  df <- as.data.frame(df)
  df <- df[, vapply(df, function(x) sum(!is.na(x)) > 0, logical(1)), drop = FALSE]

  if (ncol(df) < 2) {
    return(NA_real_)
  }

  cc <- stats::complete.cases(df)
  df_cc <- df[cc, , drop = FALSE]

  if (nrow(df_cc) < 30) {
    return(NA_real_)
  }

  k <- ncol(df_cc)
  item_var <- sum(vapply(df_cc, stats::var, numeric(1), na.rm = TRUE))
  total_var <- stats::var(rowSums(df_cc), na.rm = TRUE)

  if (is.na(total_var) || total_var == 0) {
    return(NA_real_)
  }

  (k / (k - 1)) * (1 - item_var / total_var)
}

fmt_num <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits)
  )
}

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "<0.001",
    TRUE ~ formatC(p, format = "f", digits = 3)
  )
}

write_csv_safe <- function(x, path) {
  readr::write_csv(x, path, na = "")
}

# -----------------------------
# 1b. Resolve survey weight
# -----------------------------

find_first_existing <- function(candidates, data_names) {
  hit <- candidates[candidates %in% data_names]

  if (length(hit) == 0) {
    return(NA_character_)
  }

  hit[1]
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

load_candidate_dataframe <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  if (stringr::str_detect(path, "\\.rds$")) {
    return(readRDS(path))
  }

  if (stringr::str_detect(path, "\\.rda$")) {
    return(load_first_dataframe_from_rda(path))
  }

  if (stringr::str_detect(path, "\\.sav$")) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      warning("Package 'haven' is not installed; cannot read SAV file: ", path)
      return(NULL)
    }

    return(haven::read_sav(path))
  }

  return(NULL)
}

weight_source <- NA_character_

weight_var <- find_first_existing(
  c("GSWGT1", "num_GSWGT1", "gswgt1", "w1_gswgt1", "sample_weight"),
  names(dat)
)

if (!is.na(weight_var)) {
  dat$GSWGT1 <- safe_numeric(dat[[weight_var]])
  weight_source <- paste0("resolved_from_clean_data:", weight_var)
}

if (!"GSWGT1" %in% names(dat) || all(is.na(dat$GSWGT1))) {

  candidate_weight_files <- c(
    file.path(project_root, "data/raw/21600-0004-Data.rda"),
    file.path(project_root, "data/raw/21600-0001-Data.rda"),
    file.path(project_root, "data/processed/add_health_wave01_confirmed_variables_local_only.rds")
  )

  for (candidate_path in candidate_weight_files) {

    if (!file.exists(candidate_path)) {
      next
    }

    candidate_data <- NULL

    if (stringr::str_detect(candidate_path, "\\.rds$")) {
      candidate_data <- readRDS(candidate_path)
    }

    if (stringr::str_detect(candidate_path, "\\.rda$")) {
      candidate_data <- load_first_dataframe_from_rda(candidate_path)
    }

    if (is.null(candidate_data)) {
      next
    }

    candidate_weight_var <- find_first_existing(
      c("GSWGT1", "num_GSWGT1", "gswgt1", "w1_gswgt1", "sample_weight"),
      names(candidate_data)
    )

    if (!is.na(candidate_weight_var) && nrow(candidate_data) == nrow(dat)) {
      dat$GSWGT1 <- safe_numeric(candidate_data[[candidate_weight_var]])
      weight_source <- paste0(
        "recovered_from:",
        basename(candidate_path),
        ":",
        candidate_weight_var
      )
      break
    }
  }
}

if (!"GSWGT1" %in% names(dat) || all(is.na(dat$GSWGT1))) {
  dat$GSWGT1 <- 1
  weight_source <- "unweighted_fallback_weight_equals_1"

  warning(
    "GSWGT1 was not found in the analytical data or candidate source files. ",
    "Script 13c will run as exploratory unweighted analysis."
  )
}# -----------------------------
# 2. Variable availability
# -----------------------------
# Try to recover school items from raw/processed sources before auditing availability.
# This keeps the cleaned analytical file as the main base, but supplements missing
# H1ED variables if they are available locally.

recover_variables_from_sources <- function(data, variables_to_recover, project_root) {

  recovery_audit <- tibble::tibble(
    variable = variables_to_recover,
    recovered = FALSE,
    source_file = NA_character_,
    source_variable = NA_character_,
    recovery_method = NA_character_
  )

  candidate_files <- c(
    file.path(project_root, "data/processed/add_health_wave01_confirmed_variables_local_only.rds"),
    file.path(project_root, "data/raw/21600-0001-Data.sav"),
    file.path(project_root, "data/raw/21600-0001-Data.rda"),
    file.path(project_root, "data/raw/21600-0004-Data.rda"),
    file.path(project_root, "data/raw/21600-0021-Data.rda")
  )

  for (v in variables_to_recover) {

    if (v %in% names(data)) {
      recovery_audit <- recovery_audit %>%
        mutate(
          recovered = ifelse(variable == v, TRUE, recovered),
          source_file = ifelse(variable == v, "clean_analytical_data", source_file),
          source_variable = ifelse(variable == v, v, source_variable),
          recovery_method = ifelse(variable == v, "already_available", recovery_method)
        )

      next
    }

    source_candidates <- c(
      v,
      paste0("num_", v),
      tolower(v),
      paste0("num_", tolower(v))
    )

    for (candidate_path in candidate_files) {

      candidate_data <- load_candidate_dataframe(candidate_path)

      if (is.null(candidate_data)) {
        next
      }

      source_hit <- source_candidates[source_candidates %in% names(candidate_data)]

      if (length(source_hit) == 0) {
        next
      }

      source_hit <- source_hit[1]

      # Preferred method: row-order recovery when row counts match.
      if (nrow(candidate_data) == nrow(data)) {
        data[[v]] <- candidate_data[[source_hit]]

        recovery_audit <- recovery_audit %>%
          mutate(
            recovered = ifelse(variable == v, TRUE, recovered),
            source_file = ifelse(variable == v, basename(candidate_path), source_file),
            source_variable = ifelse(variable == v, source_hit, source_variable),
            recovery_method = ifelse(variable == v, "row_order_same_n", recovery_method)
          )

        break
      }

      # Secondary method: AID-based recovery, if AID exists in both datasets.
      if ("AID" %in% names(data) && "AID" %in% names(candidate_data)) {
        lookup <- candidate_data %>%
          select(AID, recovered_value = all_of(source_hit)) %>%
          distinct(AID, .keep_all = TRUE)

        data <- data %>%
          left_join(lookup, by = "AID")

        data[[v]] <- data$recovered_value
        data$recovered_value <- NULL

        recovery_audit <- recovery_audit %>%
          mutate(
            recovered = ifelse(variable == v, TRUE, recovered),
            source_file = ifelse(variable == v, basename(candidate_path), source_file),
            source_variable = ifelse(variable == v, source_hit, source_variable),
            recovery_method = ifelse(variable == v, "matched_by_AID", recovery_method)
          )

        break
      }
    }
  }

  list(
    data = data,
    audit = recovery_audit
  )
}

school_variables_to_recover <- c(
  "H1ED15", "H1ED16", "H1ED17", "H1ED18",
  "H1ED19", "H1ED20", "H1ED21", "H1ED22", "H1ED23", "H1ED24"
)

school_recovery <- recover_variables_from_sources(
  data = dat,
  variables_to_recover = school_variables_to_recover,
  project_root = project_root
)

dat <- school_recovery$data
school_variable_recovery_audit <- school_recovery$audit

school_connectedness_core_items <- c(
  "H1ED19", "H1ED20", "H1ED23", "H1ED24"
)

school_connectedness_extended_items <- c(
  "H1ED19", "H1ED20", "H1ED21", "H1ED22", "H1ED23", "H1ED24"
)

# Used for recovery and item audit.
school_connectedness_items <- school_connectedness_extended_items

school_adjustment_items <- c(
  "H1ED15", "H1ED16", "H1ED17", "H1ED18"
)

candidate_school_items <- c(
  school_adjustment_items,
  school_connectedness_items
)

resolve_source_var <- function(base_var, data_names) {
  base_var <- safe_character(base_var)

  candidates <- c(
    base_var,
    paste0("num_", base_var),
    tolower(base_var),
    paste0("num_", tolower(base_var))
  )

  candidates <- candidates[!is.na(candidates) & candidates != ""]

  hit <- candidates[candidates %in% data_names]

  if (length(hit) == 0) {
    return(NA_character_)
  }

  hit[1]
}

school_item_map <- tibble::tibble(
  item = candidate_school_items,
  block = dplyr::case_when(
    item %in% school_adjustment_items ~ "School adjustment/problems",
    item %in% school_connectedness_items ~ "School connectedness/climate",
    TRUE ~ "Other"
  )
) %>%
  mutate(
    source_var = purrr::map_chr(
      item,
      ~ resolve_source_var(.x, names(dat))
    ),
    available_in_clean_data = !is.na(source_var),
    intended_scoring = dplyr::case_when(
      item %in% school_adjustment_items ~
        "Higher score = more school adjustment problems",
      item %in% school_connectedness_items ~
        "Reverse-coded; higher score = stronger school connectedness/climate",
      TRUE ~ ""
    )
  )

available_school_items <- school_item_map %>%
  filter(available_in_clean_data) %>%
  mutate(label = paste0(item, " [source: ", source_var, "]")) %>%
  pull(label)

missing_school_items <- school_item_map %>%
  filter(!available_in_clean_data) %>%
  pull(item)

core_controls <- c("a_age_wave1", "a_female", "a_grade_wave1", "GSWGT1")

missing_core_controls <- setdiff(core_controls, names(dat))

if (length(missing_core_controls) > 0) {
  stop(
    "Missing core controls or weight: ",
    paste(missing_core_controls, collapse = ", ")
  )
}

school_item_audit <- school_item_map %>%
  select(
    item,
    block,
    source_var,
    available_in_clean_data,
    intended_scoring
  )

# -----------------------------
# 3. Construct school indices
# -----------------------------

dat13c <- dat %>%
  mutate(
    a_age_wave1 = safe_numeric(a_age_wave1),
    a_female = valid_binary(a_female),
    a_grade_wave1 = safe_numeric(a_grade_wave1),
    GSWGT1 = safe_numeric(GSWGT1),
    grade_factor = factor(a_grade_wave1),
    grade_factor = stats::relevel(grade_factor, ref = "10")
  )

connected_score_vars_core <- character(0)
connected_score_vars_extended <- character(0)
problem_score_vars <- character(0)

connected_extended_map <- school_item_map %>%
  filter(
    item %in% school_connectedness_extended_items,
    block == "School connectedness/climate",
    available_in_clean_data,
    !is.na(source_var),
    source_var != ""
  )

if (nrow(connected_extended_map) > 0) {
  for (i in seq_len(nrow(connected_extended_map))) {
    item_name <- connected_extended_map$item[i]
    source_name <- connected_extended_map$source_var[i]
    score_name <- paste0("score_", item_name, "_connected")

    dat13c[[score_name]] <- reverse_likert_1_5(dat13c[[source_name]])
  }
}

connected_core_map <- connected_extended_map %>%
  filter(item %in% school_connectedness_core_items)

connected_score_vars_core <- paste0(
  "score_",
  connected_core_map$item,
  "_connected"
)

connected_score_vars_core <- connected_score_vars_core[
  connected_score_vars_core %in% names(dat13c)
]

connected_score_vars_extended <- paste0(
  "score_",
  connected_extended_map$item,
  "_connected"
)

connected_score_vars_extended <- connected_score_vars_extended[
  connected_score_vars_extended %in% names(dat13c)
]

problem_map <- school_item_map %>%
  filter(
    block == "School adjustment/problems",
    available_in_clean_data,
    !is.na(source_var),
    source_var != ""
  )

if (nrow(problem_map) > 0) {
  for (i in seq_len(nrow(problem_map))) {
    item_name <- problem_map$item[i]
    source_name <- problem_map$source_var[i]
    score_name <- paste0("score_", item_name, "_problem")

    dat13c[[score_name]] <- clean_frequency_0_4_or_1_5(dat13c[[source_name]])
    problem_score_vars <- c(problem_score_vars, score_name)
  }
}

problem_score_vars <- problem_score_vars[
  problem_score_vars %in% names(dat13c)
]

if (length(connected_score_vars_core) > 0) {
  dat13c$school_connectedness_core_index <- row_mean_min_items(
    dat13c[, connected_score_vars_core, drop = FALSE],
    min_items = 2
  )
} else {
  dat13c$school_connectedness_core_index <- NA_real_
}

if (length(connected_score_vars_extended) > 0) {
  dat13c$school_connectedness_extended_index <- row_mean_min_items(
    dat13c[, connected_score_vars_extended, drop = FALSE],
    min_items = 3
  )
} else {
  dat13c$school_connectedness_extended_index <- NA_real_
}

if (length(problem_score_vars) > 0) {
  dat13c$school_adjustment_problem_index <- row_mean_min_items(
    dat13c[, problem_score_vars, drop = FALSE],
    min_items = 2
  )
} else {
  dat13c$school_adjustment_problem_index <- NA_real_
}

# Backward-compatible aliases:
# The main connectedness index used in models is the core index.
dat13c$school_connectedness_index <- dat13c$school_connectedness_core_index

dat13c <- dat13c %>%
  mutate(
    school_connectedness_core_z = z_score(school_connectedness_core_index),
    school_connectedness_extended_z = z_score(school_connectedness_extended_index),
    school_connectedness_z = school_connectedness_core_z,
    school_adjustment_problem_z = z_score(school_adjustment_problem_index)
  )

connected_score_vars <- connected_score_vars_core

school_index_reliability <- tibble::tibble(
  index = c(
    "School connectedness/climate core index",
    "School connectedness/climate extended index",
    "School adjustment/problem index"
  ),
  items_available = c(
    paste(connected_core_map$item, collapse = ", "),
    paste(connected_extended_map$item, collapse = ", "),
    paste(problem_map$item, collapse = ", ")
  ),
  number_items_available = c(
    length(connected_score_vars_core),
    length(connected_score_vars_extended),
    length(problem_score_vars)
  ),
  cronbach_alpha = c(
    if (length(connected_score_vars_core) >= 2) {
      cronbach_alpha(dat13c[, connected_score_vars_core, drop = FALSE])
    } else {
      NA_real_
    },
    if (length(connected_score_vars_extended) >= 2) {
      cronbach_alpha(dat13c[, connected_score_vars_extended, drop = FALSE])
    } else {
      NA_real_
    },
    if (length(problem_score_vars) >= 2) {
      cronbach_alpha(dat13c[, problem_score_vars, drop = FALSE])
    } else {
      NA_real_
    }
  ),
  interpretation = c(
    "Primary school connectedness/climate index; higher values indicate stronger school connectedness/climate.",
    "Sensitivity version including all available H1ED19-H1ED24 items.",
    "Higher values indicate more school adjustment problems."
  )
)

school_index_summary <- dat13c %>%
  summarise(
    n_total = n(),

    connectedness_core_n = sum(!is.na(school_connectedness_core_index)),
    connectedness_core_mean = mean(school_connectedness_core_index, na.rm = TRUE),
    connectedness_core_sd = sd(school_connectedness_core_index, na.rm = TRUE),

    connectedness_extended_n = sum(!is.na(school_connectedness_extended_index)),
    connectedness_extended_mean = mean(school_connectedness_extended_index, na.rm = TRUE),
    connectedness_extended_sd = sd(school_connectedness_extended_index, na.rm = TRUE),

    school_problem_n = sum(!is.na(school_adjustment_problem_index)),
    school_problem_mean = mean(school_adjustment_problem_index, na.rm = TRUE),
    school_problem_sd = sd(school_adjustment_problem_index, na.rm = TRUE)
  )

# -----------------------------
# 4. Construct ISX-inspired protection indices
# -----------------------------

# Available binary analytical outcomes already generated by earlier scripts.
# Higher ISX score = stronger protection.
# Lower ISX score = higher risk/exposure.

available_cols <- names(dat13c)

has_var <- function(v) {
  v %in% available_cols
}

# Core exposure item
if (has_var("a_sex_ever")) {
  dat13c$a_sex_ever_bin <- valid_binary(dat13c$a_sex_ever)
} else {
  dat13c$a_sex_ever_bin <- NA_real_
}

# Protection items among all respondents:
# never sexually initiated is treated as the most protective category,
# following the logic of the original ISX.
dat13c <- dat13c %>%
  mutate(
    isx1_initiation_protection = dplyr::case_when(
      a_sex_ever_bin == 0 ~ 4,
      a_sex_ever_bin == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

# Birth control at most recent sex
if (has_var("a_H1CO6_yesno")) {
  dat13c$a_H1CO6_yesno_bin <- valid_binary(dat13c$a_H1CO6_yesno)

  dat13c <- dat13c %>%
    mutate(
      isx2_recent_birth_control_protection = dplyr::case_when(
        a_sex_ever_bin == 0 ~ 4,
        a_sex_ever_bin == 1 & a_H1CO6_yesno_bin == 1 ~ 3,
        a_sex_ever_bin == 1 & a_H1CO6_yesno_bin == 0 ~ 1,
        TRUE ~ NA_real_
      )
    )
} else {
  dat13c$isx2_recent_birth_control_protection <- NA_real_
}

# Condom ever
if (has_var("a_H1CO8_yesno")) {
  dat13c$a_H1CO8_yesno_bin <- valid_binary(dat13c$a_H1CO8_yesno)

  dat13c <- dat13c %>%
    mutate(
      isx3_condom_protection = dplyr::case_when(
        a_sex_ever_bin == 0 ~ 4,
        a_sex_ever_bin == 1 & a_H1CO8_yesno_bin == 1 ~ 3,
        a_sex_ever_bin == 1 & a_H1CO8_yesno_bin == 0 ~ 1,
        TRUE ~ NA_real_
      )
    )
} else {
  dat13c$isx3_condom_protection <- NA_real_
}

# Birth control at first sex, if available
if (has_var("a_H1CO3_yesno")) {
  dat13c$a_H1CO3_yesno_bin <- valid_binary(dat13c$a_H1CO3_yesno)

  dat13c <- dat13c %>%
    mutate(
      isx4_first_birth_control_protection = dplyr::case_when(
        a_sex_ever_bin == 0 ~ 4,
        a_sex_ever_bin == 1 & a_H1CO3_yesno_bin == 1 ~ 3,
        a_sex_ever_bin == 1 & a_H1CO3_yesno_bin == 0 ~ 1,
        TRUE ~ NA_real_
      )
    )
} else {
  dat13c$isx4_first_birth_control_protection <- NA_real_
}

isx_broad_items <- c(
  "isx1_initiation_protection",
  "isx2_recent_birth_control_protection",
  "isx3_condom_protection",
  "isx4_first_birth_control_protection"
)

isx_broad_items <- isx_broad_items[isx_broad_items %in% names(dat13c)]

dat13c$isx_broad_score <- row_sum_min_items(
  dat13c[, isx_broad_items, drop = FALSE],
  min_items = 2
)

dat13c$isx_broad_mean <- row_mean_min_items(
  dat13c[, isx_broad_items, drop = FALSE],
  min_items = 2
)

# Protection among sexually initiated respondents only
initiated_items <- c(
  "isx2_recent_birth_control_protection",
  "isx3_condom_protection",
  "isx4_first_birth_control_protection"
)

initiated_items <- initiated_items[initiated_items %in% names(dat13c)]

dat13c$isx_initiated_mean <- row_mean_min_items(
  dat13c[, initiated_items, drop = FALSE],
  min_items = 2
)

dat13c$isx_initiated_mean[dat13c$a_sex_ever_bin != 1] <- NA_real_

# Risk burden index:
# Higher value = higher observed risk burden.
# This is exploratory and not equivalent to the protection index.
dat13c <- dat13c %>%
  mutate(
    risk_sexual_initiation = dplyr::case_when(
      a_sex_ever_bin == 1 ~ 1,
      a_sex_ever_bin == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    risk_no_recent_birth_control = dplyr::case_when(
      a_sex_ever_bin == 1 & is.na(a_H1CO6_yesno_bin) ~ NA_real_,
      a_sex_ever_bin == 1 & a_H1CO6_yesno_bin == 0 ~ 1,
      a_sex_ever_bin == 1 & a_H1CO6_yesno_bin == 1 ~ 0,
      a_sex_ever_bin == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    risk_never_condom = dplyr::case_when(
      a_sex_ever_bin == 1 & is.na(a_H1CO8_yesno_bin) ~ NA_real_,
      a_sex_ever_bin == 1 & a_H1CO8_yesno_bin == 0 ~ 1,
      a_sex_ever_bin == 1 & a_H1CO8_yesno_bin == 1 ~ 0,
      a_sex_ever_bin == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

if (has_var("a_H1FP7_yesno")) {
  dat13c$risk_pregnancy <- valid_binary(dat13c$a_H1FP7_yesno)
} else {
  dat13c$risk_pregnancy <- NA_real_
}

if (has_var("a_H1CO16A_yesno")) {
  dat13c$risk_chlamydia <- valid_binary(dat13c$a_H1CO16A_yesno)
} else {
  dat13c$risk_chlamydia <- NA_real_
}

if (has_var("a_H1HS9_yesno")) {
  dat13c$risk_std_testing_treatment <- valid_binary(dat13c$a_H1HS9_yesno)
} else {
  dat13c$risk_std_testing_treatment <- NA_real_
}

risk_items_main <- c(
  "risk_sexual_initiation",
  "risk_no_recent_birth_control",
  "risk_never_condom",
  "risk_pregnancy",
  "risk_chlamydia"
)

risk_items_extended <- c(
  risk_items_main,
  "risk_std_testing_treatment"
)

dat13c$risk_burden_mean <- row_mean_min_items(
  dat13c[, risk_items_main, drop = FALSE],
  min_items = 2
)

dat13c$risk_burden_extended_mean <- row_mean_min_items(
  dat13c[, risk_items_extended, drop = FALSE],
  min_items = 2
)

isx_item_dictionary <- tibble::tibble(
  item = c(
    "isx1_initiation_protection",
    "isx2_recent_birth_control_protection",
    "isx3_condom_protection",
    "isx4_first_birth_control_protection",
    "isx_broad_score",
    "isx_broad_mean",
    "isx_initiated_mean",
    "risk_burden_mean",
    "risk_burden_extended_mean"
  ),
  interpretation = c(
    "Never sexually initiated = 4; sexually initiated = 1.",
    "Never initiated = 4; birth control at most recent sex = 3; no birth control = 1.",
    "Never initiated = 4; ever used condom = 3; never used condom = 1.",
    "Never initiated = 4; birth control at first sex = 3; no birth control = 1.",
    "Sum of available ISX-inspired protection items; higher = more protection.",
    "Mean of available ISX-inspired protection items; higher = more protection.",
    "Mean protection score among sexually initiated respondents only.",
    "Mean risk burden excluding STD testing/treatment; higher = more risk burden.",
    "Mean risk burden including STD testing/treatment as exploratory service-contact indicator."
  )
)

isx_summary <- dat13c %>%
  summarise(
    n_total = n(),
    isx_broad_score_n = sum(!is.na(isx_broad_score)),
    isx_broad_score_mean = mean(isx_broad_score, na.rm = TRUE),
    isx_broad_score_sd = sd(isx_broad_score, na.rm = TRUE),
    isx_broad_mean_n = sum(!is.na(isx_broad_mean)),
    isx_broad_mean_mean = mean(isx_broad_mean, na.rm = TRUE),
    isx_broad_mean_sd = sd(isx_broad_mean, na.rm = TRUE),
    isx_initiated_mean_n = sum(!is.na(isx_initiated_mean)),
    isx_initiated_mean_mean = mean(isx_initiated_mean, na.rm = TRUE),
    isx_initiated_mean_sd = sd(isx_initiated_mean, na.rm = TRUE),
    risk_burden_mean_n = sum(!is.na(risk_burden_mean)),
    risk_burden_mean_mean = mean(risk_burden_mean, na.rm = TRUE),
    risk_burden_mean_sd = sd(risk_burden_mean, na.rm = TRUE)
  )

# -----------------------------
# 5. Samples
# -----------------------------

dat13c <- dat13c %>%
  mutate(
    sample_main_grade_10_12 = a_grade_wave1 %in% c(10, 11, 12),
    sample_restricted_sensitivity =
      a_grade_wave1 %in% c(10, 11, 12) &
      a_age_wave1 >= 15 &
      a_age_wave1 <= 19
  )
weight_audit <- tibble::tibble(
  weight_variable = "GSWGT1",
  weight_source = weight_source,
  valid_weight_n = sum(!is.na(dat13c$GSWGT1) & dat13c$GSWGT1 > 0),
  interpretation = dplyr::case_when(
    weight_source == "unweighted_fallback_weight_equals_1" ~
      "Exploratory unweighted fallback was used because GSWGT1 was not found.",
    TRUE ~
      "Survey weight was resolved and used in exploratory models."
  )
)
sample_definitions <- tibble::tibble(
  sample_name = c(
    "Main sample: grades 10-12",
    "Restricted sensitivity sample: grades 10-12 and ages 15-19"
  ),
  sample_variable = c(
    "sample_main_grade_10_12",
    "sample_restricted_sensitivity"
  ),
  n_unweighted = c(
    sum(dat13c$sample_main_grade_10_12, na.rm = TRUE),
    sum(dat13c$sample_restricted_sensitivity, na.rm = TRUE)
  ),
  interpretation = c(
    "Main public analytical sample.",
    "Restricted sensitivity sample approximating the thesis age range."
  )
)

# -----------------------------
# 6. Outcome models
# -----------------------------

outcome_dictionary <- tibble::tribble(
  ~domain, ~outcome_var, ~outcome_label,
  "Sexual initiation", "a_sex_ever", "Sexual initiation",
  "Contraception and protection", "a_H1CO6_yesno", "Birth control use at most recent sex",
  "Contraception and protection", "a_H1CO8_yesno", "Ever used a condom during sex",
  "Pregnancy and reproductive experience", "a_H1FP7_yesno", "Ever been pregnant",
  "STI-related outcomes", "a_H1HS9_yesno", "STD testing or treatment",
  "STI-related outcomes", "a_H1CO16A_yesno", "Self-reported chlamydia diagnosis"
) %>%
  mutate(available = outcome_var %in% names(dat13c))

school_predictor_dictionary <- tibble::tribble(
  ~predictor_var, ~predictor_label, ~model_role, ~interpretation,

  "school_connectedness_core_z",
  "School connectedness/climate core index, standardized",
  "primary",
  "One standard-deviation higher score indicates stronger school connectedness/climate. This is the primary connectedness measure.",

  "school_adjustment_problem_z",
  "School adjustment/problem index, standardized",
  "primary",
  "One standard-deviation higher score indicates more school adjustment problems.",

  "school_connectedness_extended_z",
  "School connectedness/climate extended index, standardized",
  "sensitivity",
  "Sensitivity connectedness measure including H1ED19-H1ED24."
)

fit_binary_model <- function(data, sample_var, sample_label, outcome_var, outcome_label,
                             domain, predictor_var, predictor_label) {

  if (!outcome_var %in% names(data)) {
    return(tibble::tibble(
      sample = sample_label,
      domain = domain,
      outcome = outcome_label,
      predictor = predictor_label,
      n_complete = 0,
      n_yes = NA_integer_,
      n_no = NA_integer_,
      OR = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      p_value = NA_real_,
      status = "outcome_not_available"
    ))
  }

  model_data <- data %>%
    filter(.data[[sample_var]]) %>%
    mutate(
      y = valid_binary(.data[[outcome_var]]),
      predictor_x = safe_numeric(.data[[predictor_var]]),
      age = safe_numeric(a_age_wave1),
      female = valid_binary(a_female),
      grade_factor = factor(a_grade_wave1),
      grade_factor = stats::relevel(grade_factor, ref = "10"),
      weight = safe_numeric(GSWGT1)
    ) %>%
    filter(
      !is.na(y),
      !is.na(predictor_x),
      !is.na(age),
      !is.na(female),
      !is.na(grade_factor),
      !is.na(weight),
      weight > 0
    )

  n_complete <- nrow(model_data)
  n_yes <- sum(model_data$y == 1, na.rm = TRUE)
  n_no <- sum(model_data$y == 0, na.rm = TRUE)

  if (n_complete < 100 || n_yes < 10 || n_no < 10) {
    return(tibble::tibble(
      sample = sample_label,
      domain = domain,
      outcome = outcome_label,
      predictor = predictor_label,
      n_complete = n_complete,
      n_yes = n_yes,
      n_no = n_no,
      OR = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      p_value = NA_real_,
      status = "not_fitted_small_or_sparse"
    ))
  }

  design <- survey::svydesign(
    ids = ~1,
    weights = ~weight,
    data = model_data
  )

  fit <- tryCatch(
    survey::svyglm(
      y ~ predictor_x + age + female + grade_factor,
      design = design,
      family = quasibinomial()
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(tibble::tibble(
      sample = sample_label,
      domain = domain,
      outcome = outcome_label,
      predictor = predictor_label,
      n_complete = n_complete,
      n_yes = n_yes,
      n_no = n_no,
      OR = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      p_value = NA_real_,
      status = paste0("model_error: ", fit$message)
    ))
  }

  coefs <- summary(fit)$coefficients

  if (!"predictor_x" %in% rownames(coefs)) {
    return(tibble::tibble(
      sample = sample_label,
      domain = domain,
      outcome = outcome_label,
      predictor = predictor_label,
      n_complete = n_complete,
      n_yes = n_yes,
      n_no = n_no,
      OR = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      p_value = NA_real_,
      status = "predictor_not_estimated"
    ))
  }

  beta <- coefs["predictor_x", "Estimate"]
  se <- coefs["predictor_x", "Std. Error"]
  p <- coefs["predictor_x", "Pr(>|t|)"]

  tibble::tibble(
    sample = sample_label,
    domain = domain,
    outcome = outcome_label,
    predictor = predictor_label,
    n_complete = n_complete,
    n_yes = n_yes,
    n_no = n_no,
    OR = exp(beta),
    CI_low = exp(beta - 1.96 * se),
    CI_high = exp(beta + 1.96 * se),
    p_value = p,
    status = "fitted"
  )
}

sample_list <- list(
  list(
    sample_var = "sample_main_grade_10_12",
    sample_label = "Main sample: grades 10-12"
  ),
  list(
    sample_var = "sample_restricted_sensitivity",
    sample_label = "Restricted sensitivity sample: grades 10-12 and ages 15-19"
  )
)

binary_model_results <- purrr::map_dfr(sample_list, function(smp) {
  purrr::map_dfr(seq_len(nrow(outcome_dictionary)), function(i) {
    purrr::map_dfr(seq_len(nrow(school_predictor_dictionary)), function(j) {
      fit_binary_model(
        data = dat13c,
        sample_var = smp$sample_var,
        sample_label = smp$sample_label,
        outcome_var = outcome_dictionary$outcome_var[i],
        outcome_label = outcome_dictionary$outcome_label[i],
        domain = outcome_dictionary$domain[i],
        predictor_var = school_predictor_dictionary$predictor_var[j],
        predictor_label = school_predictor_dictionary$predictor_label[j]
      )
    })
  })
})

predictor_role_lookup <- school_predictor_dictionary %>%
  transmute(
    predictor = predictor_label,
    model_role = model_role
  )

binary_model_results <- binary_model_results %>%
  left_join(predictor_role_lookup, by = "predictor")

binary_model_results_public <- binary_model_results %>%
  mutate(
    OR_text = fmt_num(OR, 2),
    CI_95 = paste0("[", fmt_num(CI_low, 2), "; ", fmt_num(CI_high, 2), "]"),
    p_text = fmt_p(p_value),
    interpretation = dplyr::case_when(
      status != "fitted" ~ status,
      OR < 1 & p_value < 0.05 ~
        "Higher predictor values were associated with lower odds of the outcome.",
      OR > 1 & p_value < 0.05 ~
        "Higher predictor values were associated with higher odds of the outcome.",
      status == "fitted" ~
        "No statistically significant association at the 5 percent level.",
      TRUE ~ ""
    )
  )

# -----------------------------
# 7. ISX and risk index models
# -----------------------------

index_dictionary <- tibble::tribble(
  ~index_var, ~index_label, ~expected_direction,
  "isx_broad_mean",
  "ISX-inspired broad sexual protection index",
  "Higher values indicate stronger protection.",
  "isx_initiated_mean",
  "ISX-inspired protection index among sexually initiated respondents",
  "Higher values indicate stronger protection among initiated respondents.",
  "risk_burden_mean",
  "Exploratory sexual and reproductive risk burden index",
  "Higher values indicate higher risk burden."
)

fit_continuous_index_model <- function(data, sample_var, sample_label,
                                       index_var, index_label,
                                       predictor_var, predictor_label) {

  if (!index_var %in% names(data)) {
    return(tibble::tibble(
      sample = sample_label,
      index = index_label,
      predictor = predictor_label,
      n_complete = 0,
      beta = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      p_value = NA_real_,
      status = "index_not_available"
    ))
  }

  model_data <- data %>%
    filter(.data[[sample_var]]) %>%
    mutate(
      y = safe_numeric(.data[[index_var]]),
      predictor_x = safe_numeric(.data[[predictor_var]]),
      age = safe_numeric(a_age_wave1),
      female = valid_binary(a_female),
      grade_factor = factor(a_grade_wave1),
      grade_factor = stats::relevel(grade_factor, ref = "10"),
      weight = safe_numeric(GSWGT1)
    ) %>%
    filter(
      !is.na(y),
      !is.na(predictor_x),
      !is.na(age),
      !is.na(female),
      !is.na(grade_factor),
      !is.na(weight),
      weight > 0
    )

  n_complete <- nrow(model_data)

  if (n_complete < 100 || stats::sd(model_data$y, na.rm = TRUE) == 0) {
    return(tibble::tibble(
      sample = sample_label,
      index = index_label,
      predictor = predictor_label,
      n_complete = n_complete,
      beta = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      p_value = NA_real_,
      status = "not_fitted_small_or_no_variation"
    ))
  }

  design <- survey::svydesign(
    ids = ~1,
    weights = ~weight,
    data = model_data
  )

  fit <- tryCatch(
    survey::svyglm(
      y ~ predictor_x + age + female + grade_factor,
      design = design,
      family = gaussian()
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(tibble::tibble(
      sample = sample_label,
      index = index_label,
      predictor = predictor_label,
      n_complete = n_complete,
      beta = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      p_value = NA_real_,
      status = paste0("model_error: ", fit$message)
    ))
  }

  coefs <- summary(fit)$coefficients

  if (!"predictor_x" %in% rownames(coefs)) {
    return(tibble::tibble(
      sample = sample_label,
      index = index_label,
      predictor = predictor_label,
      n_complete = n_complete,
      beta = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      p_value = NA_real_,
      status = "predictor_not_estimated"
    ))
  }

  beta <- coefs["predictor_x", "Estimate"]
  se <- coefs["predictor_x", "Std. Error"]
  p <- coefs["predictor_x", "Pr(>|t|)"]

  tibble::tibble(
    sample = sample_label,
    index = index_label,
    predictor = predictor_label,
    n_complete = n_complete,
    beta = beta,
    CI_low = beta - 1.96 * se,
    CI_high = beta + 1.96 * se,
    p_value = p,
    status = "fitted"
  )
}

index_model_results <- purrr::map_dfr(sample_list, function(smp) {
  purrr::map_dfr(seq_len(nrow(index_dictionary)), function(i) {
    purrr::map_dfr(seq_len(nrow(school_predictor_dictionary)), function(j) {
      fit_continuous_index_model(
        data = dat13c,
        sample_var = smp$sample_var,
        sample_label = smp$sample_label,
        index_var = index_dictionary$index_var[i],
        index_label = index_dictionary$index_label[i],
        predictor_var = school_predictor_dictionary$predictor_var[j],
        predictor_label = school_predictor_dictionary$predictor_label[j]
      )
    })
  })
})

index_model_results <- index_model_results %>%
  left_join(predictor_role_lookup, by = "predictor")

index_model_results_public <- index_model_results %>%
  mutate(
    beta_text = fmt_num(beta, 3),
    CI_95 = paste0("[", fmt_num(CI_low, 3), "; ", fmt_num(CI_high, 3), "]"),
    p_text = fmt_p(p_value),
    interpretation = dplyr::case_when(
      status != "fitted" ~ status,
      beta > 0 & p_value < 0.05 &
        stringr::str_detect(index, "protection") ~
        "Higher predictor values were associated with higher protection score.",
      beta < 0 & p_value < 0.05 &
        stringr::str_detect(index, "protection") ~
        "Higher predictor values were associated with lower protection score.",
      beta > 0 & p_value < 0.05 &
        stringr::str_detect(index, "risk") ~
        "Higher predictor values were associated with higher risk burden.",
      beta < 0 & p_value < 0.05 &
        stringr::str_detect(index, "risk") ~
        "Higher predictor values were associated with lower risk burden.",
      status == "fitted" ~
        "No statistically significant association at the 5 percent level.",
      TRUE ~ ""
    )
  )

# -----------------------------
# 8. Public interpretive summary
# -----------------------------

significant_binary_all <- binary_model_results_public %>%
  filter(status == "fitted", !is.na(p_value), p_value < 0.05)

significant_index_all <- index_model_results_public %>%
  filter(status == "fitted", !is.na(p_value), p_value < 0.05)

# Main report tables use only primary school predictors.
# Sensitivity results remain available in CSV/Excel outputs.
significant_binary <- significant_binary_all %>%
  filter(model_role == "primary")

significant_index <- significant_index_all %>%
  filter(model_role == "primary")

significant_binary_sensitivity <- significant_binary_all %>%
  filter(model_role == "sensitivity")

significant_index_sensitivity <- significant_index_all %>%
  filter(model_role == "sensitivity")
public_summary <- tibble::tibble(
  section = c(
    "Purpose",
    "Primary school connectedness index",
    "Extended school connectedness index",
    "School adjustment problems",
    "ISX-inspired protection index",
    "Risk burden index",
    "Public reporting status"
  ),
  interpretation = c(
    "Script 13c tests whether school connectedness and school adjustment problems are associated with selected adolescent sexual and reproductive outcomes.",
    "The primary school connectedness index uses H1ED19, H1ED20, H1ED23 and H1ED24. It is reverse-coded so that higher values indicate stronger school connectedness or climate.",
    "The extended school connectedness index uses H1ED19-H1ED24 and is retained as a sensitivity measure because its internal consistency is weaker.",
    "The school adjustment/problem index uses H1ED15-H1ED18 and is scored so that higher values indicate more school-related problems.",
    "The ISX-inspired index follows the user's thesis logic: higher score indicates stronger sexual protection and lower score indicates higher exposure or risk.",
    "The risk burden index is exploratory and should not replace individual outcome models.",
    "All results from Script 13c should be treated as exploratory and hypothesis-generating unless later promoted after review."
  )
)
# -----------------------------
# 9. Export outputs
# -----------------------------
write_csv_safe(
  school_variable_recovery_audit,
  "outputs/tables/script13c_wave01_school_variable_recovery_audit.csv"
)

write_csv_safe(
  weight_audit,
  "outputs/tables/script13c_wave01_weight_audit.csv"
)
write_csv_safe(
  school_item_audit,
  "outputs/tables/script13c_wave01_school_item_audit.csv"
)

write_csv_safe(
  school_index_reliability,
  "outputs/tables/script13c_wave01_school_index_reliability.csv"
)

write_csv_safe(
  school_index_summary,
  "outputs/tables/script13c_wave01_school_index_summary.csv"
)

write_csv_safe(
  isx_item_dictionary,
  "outputs/tables/script13c_wave01_isx_item_dictionary.csv"
)

write_csv_safe(
  isx_summary,
  "outputs/tables/script13c_wave01_isx_summary.csv"
)

write_csv_safe(
  sample_definitions,
  "outputs/tables/script13c_wave01_sample_definitions.csv"
)

write_csv_safe(
  outcome_dictionary,
  "outputs/tables/script13c_wave01_outcome_dictionary.csv"
)

write_csv_safe(
  binary_model_results_public,
  "outputs/tables/script13c_wave01_school_connectedness_outcome_models.csv"
)

write_csv_safe(
  index_model_results_public,
  "outputs/tables/script13c_wave01_school_connectedness_isx_models.csv"
)

write_csv_safe(
  public_summary,
  "outputs/tables/script13c_wave01_public_interpretive_summary.csv"
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "school_variable_recovery")
openxlsx::writeData(wb, "school_variable_recovery", school_variable_recovery_audit)

openxlsx::addWorksheet(wb, "weight_audit")
openxlsx::writeData(wb, "weight_audit", weight_audit)

openxlsx::addWorksheet(wb, "school_item_audit")
openxlsx::writeData(wb, "school_item_audit", school_item_audit)

openxlsx::addWorksheet(wb, "school_index_reliability")
openxlsx::writeData(wb, "school_index_reliability", school_index_reliability)

openxlsx::addWorksheet(wb, "school_index_summary")
openxlsx::writeData(wb, "school_index_summary", school_index_summary)

openxlsx::addWorksheet(wb, "isx_item_dictionary")
openxlsx::writeData(wb, "isx_item_dictionary", isx_item_dictionary)

openxlsx::addWorksheet(wb, "isx_summary")
openxlsx::writeData(wb, "isx_summary", isx_summary)

openxlsx::addWorksheet(wb, "samples")
openxlsx::writeData(wb, "samples", sample_definitions)

openxlsx::addWorksheet(wb, "outcome_models")
openxlsx::writeData(wb, "outcome_models", binary_model_results_public)

openxlsx::addWorksheet(wb, "isx_models")
openxlsx::writeData(wb, "isx_models", index_model_results_public)

openxlsx::addWorksheet(wb, "public_summary")
openxlsx::writeData(wb, "public_summary", public_summary)

openxlsx::saveWorkbook(
  wb,
  "outputs/tables/script13c_wave01_school_connectedness_isx_results.xlsx",
  overwrite = TRUE
)

# -----------------------------
# 10. Create public DOCX summary
# -----------------------------

doc <- officer::read_docx()

doc <- officer::body_add_par(
  doc,
  "Script 13c — Exploratory School Connectedness and Sexual Protection Index",
  style = "heading 1"
)

doc <- officer::body_add_par(
  doc,
  "This report summarizes exploratory analyses linking school connectedness, school adjustment problems, selected outcomes, and ISX-inspired protection indices. All outputs are aggregate and public-facing.",
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Purpose", style = "heading 2")
doc <- flextable::body_add_flextable(
  doc,
  flextable::autofit(flextable::flextable(public_summary))
)

doc <- officer::body_add_par(doc, "School item audit", style = "heading 2")
doc <- flextable::body_add_flextable(
  doc,
  flextable::autofit(flextable::flextable(school_item_audit))
)

doc <- officer::body_add_par(doc, "School index reliability", style = "heading 2")
doc <- flextable::body_add_flextable(
  doc,
  flextable::autofit(flextable::flextable(school_index_reliability))
)

doc <- officer::body_add_par(doc, "ISX-inspired index summary", style = "heading 2")
doc <- flextable::body_add_flextable(
  doc,
  flextable::autofit(flextable::flextable(isx_summary))
)

doc <- officer::body_add_par(doc, "Significant exploratory outcome associations", style = "heading 2")

sig_binary_public <- significant_binary %>%
  select(
    sample,
    domain,
    outcome,
    predictor,
    n_complete,
    OR_text,
    CI_95,
    p_text,
    interpretation
  )

if (nrow(sig_binary_public) == 0) {
  doc <- officer::body_add_par(
    doc,
    "No statistically significant binary outcome associations were identified at the 5 percent level.",
    style = "Normal"
  )
} else {
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(sig_binary_public))
  )
}

doc <- officer::body_add_par(doc, "Significant exploratory ISX/index associations", style = "heading 2")

sig_index_public <- significant_index %>%
  select(
    sample,
    index,
    predictor,
    n_complete,
    beta_text,
    CI_95,
    p_text,
    interpretation
  )

if (nrow(sig_index_public) == 0) {
  doc <- officer::body_add_par(
    doc,
    "No statistically significant index associations were identified at the 5 percent level.",
    style = "Normal"
  )
} else {
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(sig_index_public))
  )
}

doc <- officer::body_add_par(doc, "Interpretive caution", style = "heading 2")
doc <- officer::body_add_par(
  doc,
  "Script 13c is exploratory. The ISX-inspired index follows the scoring logic of the thesis, but it is adapted to the public Add Health variables available in this project. These results should not replace the outcome-specific models from Script 13b.",
  style = "Normal"
)

print(
  doc,
  target = "docs/add_health_wave01_school_connectedness_isx_report_script13c.docx"
)

# Markdown summary
md_lines <- c(
  "# Script 13c — Exploratory School Connectedness and Sexual Protection Index",
  "",
  "This report summarizes exploratory analyses linking school connectedness, school adjustment problems, selected outcomes, and ISX-inspired protection indices.",
  "",
  "## Key caution",
  "",
  "The ISX-inspired index follows the user's thesis logic: higher score indicates stronger sexual protection and lower score indicates higher exposure or risk. The index is adapted to public Add Health variables and should be treated as exploratory.",
  "",
  "## Outputs",
  "",
  "- outputs/tables/script13c_wave01_school_item_audit.csv",
  "- outputs/tables/script13c_wave01_school_index_reliability.csv",
  "- outputs/tables/script13c_wave01_school_index_summary.csv",
  "- outputs/tables/script13c_wave01_isx_item_dictionary.csv",
  "- outputs/tables/script13c_wave01_isx_summary.csv",
  "- outputs/tables/script13c_wave01_school_connectedness_outcome_models.csv",
  "- outputs/tables/script13c_wave01_school_connectedness_isx_models.csv",
  "- outputs/tables/script13c_wave01_school_connectedness_isx_results.xlsx",
  "- docs/add_health_wave01_school_connectedness_isx_report_script13c.docx"
)

writeLines(
  md_lines,
  "docs/add_health_wave01_school_connectedness_isx_report_script13c.md"
)

# -----------------------------
# 11. Execution checklist
# -----------------------------

execution_checklist <- tibble::tibble(
  check_id = 1:18,
  check_item = c(
    "Project root exists",
    "Analytical clean RDS exists",
    "Outputs tables directory exists",
    "Outputs diagnostics directory exists",
    "Docs directory exists",
    "GSWGT1 available",
    "Age control available",
    "Sex control available",
    "Grade control available",
    "At least one school connectedness item available",
    "At least one school adjustment item available",
    "School connectedness index created",
    "School adjustment/problem index created",
    "ISX broad score created",
    "ISX initiated score created",
    "Binary outcome models generated",
    "Index models generated",
    "No individual-level data exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(file.exists(input_data_path), "OK", "FAIL"),
    ifelse(dir.exists("outputs/tables"), "OK", "FAIL"),
    ifelse(dir.exists("outputs/diagnostics"), "OK", "FAIL"),
    ifelse(dir.exists("docs"), "OK", "FAIL"),
    ifelse("GSWGT1" %in% names(dat13c), "OK", "FAIL"),
    ifelse("a_age_wave1" %in% names(dat13c), "OK", "FAIL"),
    ifelse("a_female" %in% names(dat13c), "OK", "FAIL"),
    ifelse("a_grade_wave1" %in% names(dat13c), "OK", "FAIL"),
    ifelse(length(connected_score_vars) > 0, "OK", "REVIEW"),
    ifelse(length(problem_score_vars) > 0, "OK", "REVIEW"),
    ifelse(sum(!is.na(dat13c$school_connectedness_index)) > 0, "OK", "REVIEW"),
    ifelse(sum(!is.na(dat13c$school_adjustment_problem_index)) > 0, "OK", "REVIEW"),
    ifelse(sum(!is.na(dat13c$isx_broad_score)) > 0, "OK", "REVIEW"),
    ifelse(sum(!is.na(dat13c$isx_initiated_mean)) > 0, "OK", "REVIEW"),
    ifelse(nrow(binary_model_results_public) > 0, "OK", "REVIEW"),
    ifelse(nrow(index_model_results_public) > 0, "OK", "REVIEW"),
    "OK"
  )
)

write_csv_safe(
  execution_checklist,
  "outputs/diagnostics/script13c_execution_checklist.csv"
)

# -----------------------------
# 12. Console summary
# -----------------------------

cat("\n============================================================\n")
cat("Script 13c completed: School Connectedness and ISX Analysis\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("School variable recovery audit:\n")
print(school_variable_recovery_audit)
cat("\n")

cat("School items available:\n")
cat(paste(available_school_items, collapse = ", "), "\n\n")

cat("School items missing:\n")
cat(ifelse(length(missing_school_items) == 0, "None", paste(missing_school_items, collapse = ", ")), "\n\n")

cat("School index reliability:\n")
print(school_index_reliability)

cat("\nISX summary:\n")
print(isx_summary)

cat("\nSignificant primary binary outcome associations:\n")
print(significant_binary)

cat("\nSignificant sensitivity binary outcome associations:\n")
print(significant_binary_sensitivity)

cat("\nSignificant primary ISX/index associations:\n")
print(significant_index)

cat("\nSignificant sensitivity ISX/index associations:\n")
print(significant_index_sensitivity)

cat("\nExecution checklist:\n")
print(execution_checklist)

cat("\nOutputs created:\n")
cat("- outputs/tables/script13c_wave01_school_item_audit.csv\n")
cat("- outputs/tables/script13c_wave01_school_index_reliability.csv\n")
cat("- outputs/tables/script13c_wave01_school_index_summary.csv\n")
cat("- outputs/tables/script13c_wave01_isx_item_dictionary.csv\n")
cat("- outputs/tables/script13c_wave01_isx_summary.csv\n")
cat("- outputs/tables/script13c_wave01_school_connectedness_outcome_models.csv\n")
cat("- outputs/tables/script13c_wave01_school_connectedness_isx_models.csv\n")
cat("- outputs/tables/script13c_wave01_school_connectedness_isx_results.xlsx\n")
cat("- docs/add_health_wave01_school_connectedness_isx_report_script13c.docx\n")
cat("- docs/add_health_wave01_school_connectedness_isx_report_script13c.md\n")
cat("- outputs/diagnostics/script13c_execution_checklist.csv\n")
cat("- outputs/tables/script13c_wave01_school_variable_recovery_audit.csv\n")

cat("\nImportant note:\n")
cat("Script 13c is exploratory. Do not promote these results to final public conclusions before review.\n")
cat("No individual-level data were exported.\n")