# ============================================================
# Script 19a_v3
# Corrected Never-Sex Family, Friend and Deterrence Constructs
# Project: Add Health Adolescent Risk Models
# ============================================================
#
# Purpose:
#   Build corrected constructs for adolescents aged 15–19 who
#   have not yet had sexual intercourse.
#
# Constructs:
#   1. family_connectedness:
#      H1PR3, H1PR5, H1PR7, H1PR8
#
#   2. friend_support:
#      H1PR4
#
#   3. sexual_delay_deterrence:
#      H1MO2, H1MO3, H1MO4
#
# Interpretation:
#   Higher scores indicate higher protection / stronger deterrence.
#
# Important:
#   This script does not perform any Git action.
#
# ============================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  warn = 1
)

script_id <- "19a_v3"
script_title <- "Corrected Never-Sex Family, Friend and Deterrence Constructs"
start_time <- Sys.time()

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

core_pkgs <- c(
  "dplyr",
  "readr",
  "stringr",
  "purrr",
  "tibble",
  "tidyr",
  "haven"
)

missing_core <- core_pkgs[
  !vapply(core_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_core) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_core, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(tidyr)
  library(haven)
})

has_docx <- all(vapply(
  c("officer", "flextable"),
  requireNamespace,
  logical(1),
  quietly = TRUE
))

# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

if (!str_detect(basename(project_root), "add-health-adolescent-risk-models")) {
  warning(
    "Current working directory does not look like the project root: ",
    project_root
  )
}

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/audits", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/analysis", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reports", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)

rds_path <- "outputs/analysis/script19a_v3_corrected_never_sex_family_friend_deterrence_constructs.rds"
sample_audit_path <- "outputs/audits/script19a_v3_never_sex_sample_audit.csv"
coding_audit_path <- "outputs/audits/script19a_v3_construct_coding_audit.csv"
item_summary_path <- "outputs/audits/script19a_v3_item_summary.csv"
construct_summary_path <- "outputs/tables/script19a_v3_construct_summary.csv"
docx_path <- "outputs/reports/script19a_v3_corrected_never_sex_family_friend_deterrence_constructs.docx"
log_path <- "outputs/logs/script19a_v3_run_log.txt"

cat("", file = log_path)

log_line <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(txt, "\n", file = log_path, append = TRUE)
  message(txt)
}

log_line("Started ", script_id, ": ", script_title)
log_line("Project root: ", project_root)

# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------

std_key <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", x))
}

find_var <- function(df, aliases, patterns = NULL) {
  nms <- names(df)
  nms_key <- std_key(nms)
  alias_key <- std_key(aliases)
  
  exact_idx <- match(alias_key, nms_key)
  exact_idx <- exact_idx[!is.na(exact_idx)]
  
  if (length(exact_idx) > 0) {
    return(nms[exact_idx[1]])
  }
  
  if (!is.null(patterns)) {
    pattern_hit <- nms[
      str_detect(tolower(nms), paste(patterns, collapse = "|"))
    ]
    
    if (length(pattern_hit) > 0) {
      return(pattern_hit[1])
    }
  }
  
  NA_character_
}

extract_first_number <- function(x) {
  if (inherits(x, "haven_labelled")) {
    raw <- suppressWarnings(as.numeric(haven::zap_labels(x)))
    
    if (sum(!is.na(raw)) > 0) {
      return(raw)
    }
    
    x <- as.character(haven::as_factor(x, levels = "labels"))
  } else {
    x <- as.character(x)
  }
  
  out <- stringr::str_extract(x, "\\d+")
  suppressWarnings(as.numeric(out))
}

to_numeric_survey <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  
  if (is.factor(x)) {
    return(extract_first_number(x))
  }
  
  suppressWarnings(as.numeric(x))
}

clean_valid_1_5 <- function(x) {
  out <- extract_first_number(x)
  out[!(out %in% 1:5)] <- NA_real_
  out
}

z_score <- function(x) {
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  
  if (is.na(s) || s == 0) {
    return(rep(NA_real_, length(x)))
  }
  
  as.numeric((x - m) / s)
}

row_mean_min <- function(data, min_items = 1) {
  if (is.null(data) || ncol(data) == 0) {
    return(rep(NA_real_, nrow(data)))
  }
  
  valid_n <- rowSums(!is.na(data))
  out <- rowMeans(data, na.rm = TRUE)
  out[valid_n < min_items] <- NA_real_
  out
}

get_levels_text <- function(x, max_levels = 30) {
  if (is.factor(x)) {
    lev <- levels(x)
    lev <- lev[seq_len(min(length(lev), max_levels))]
    return(paste(lev, collapse = " | "))
  }
  
  if (inherits(x, "haven_labelled")) {
    labs <- attr(x, "labels")
    
    if (!is.null(labs)) {
      txt <- paste0(names(labs), "=", as.numeric(labs))
      txt <- txt[seq_len(min(length(txt), max_levels))]
      return(paste(txt, collapse = " | "))
    }
  }
  
  NA_character_
}

recode_ever_sex <- function(x) {
  out <- rep(NA_real_, length(x))
  
  if (inherits(x, "haven_labelled")) {
    labels_chr <- tolower(as.character(haven::as_factor(x, levels = "labels")))
    
    out[str_detect(labels_chr, "\\byes\\b|sim")] <- 1
    out[str_detect(labels_chr, "\\bno\\b|nao|não|never")] <- 0
  }
  
  if (is.factor(x) || is.character(x)) {
    ch <- tolower(as.character(x))
    
    out[str_detect(ch, "\\byes\\b|sim")] <- 1
    out[str_detect(ch, "\\bno\\b|nao|não|never")] <- 0
  }
  
  num <- to_numeric_survey(x)
  valid_vals <- sort(unique(num[!is.na(num)]))
  
  mapped <- rep(NA_real_, length(num))
  
  if (length(valid_vals) > 0 && all(valid_vals %in% c(0, 1))) {
    mapped[num == 1] <- 1
    mapped[num == 0] <- 0
  }
  
  if (length(valid_vals) > 0 && all(valid_vals %in% c(1, 2))) {
    mapped[num == 1] <- 1
    mapped[num == 2] <- 0
  }
  
  out[is.na(out)] <- mapped[is.na(out)]
  
  out
}

compute_wave1_age_from_dates <- function(df) {
  bmonth_var <- find_var(df, c("H1GI1M", "h1gi1m", "H1GILM", "h1gilm"))
  byear_var  <- find_var(df, c("H1GI1Y", "h1gi1y", "H1GILY", "h1gily"))
  imonth_var <- find_var(df, c("IMONTH", "imonth"))
  iday_var   <- find_var(df, c("IDAY", "iday"))
  iyear_var  <- find_var(df, c("IYEAR", "iyear"))
  
  required_vars <- c(bmonth_var, byear_var, imonth_var, iday_var, iyear_var)
  
  if (any(is.na(required_vars))) {
    return(list(
      age = rep(NA_real_, nrow(df)),
      source = "age_not_constructed_missing_date_variables",
      variables = paste(required_vars, collapse = ", ")
    ))
  }
  
  clean_month <- function(x) {
    x <- extract_first_number(x)
    x[x %in% c(96, 97, 98, 99)] <- NA_real_
    x[x < 1 | x > 12] <- NA_real_
    x
  }
  
  clean_day <- function(x) {
    x <- extract_first_number(x)
    x[x %in% c(96, 97, 98, 99)] <- NA_real_
    x[x < 1 | x > 31] <- NA_real_
    x
  }
  
  clean_birth_year <- function(x) {
    x <- extract_first_number(x)
    
    x[x %in% c(96, 97, 98, 99, 9996, 9997, 9998, 9999)] <- NA_real_
    
    x <- dplyr::case_when(
      is.na(x) ~ NA_real_,
      x >= 1900 & x <= 2026 ~ x,
      x >= 50 & x <= 99 ~ 1900 + x,
      x >= 1 & x <= 30 ~ 1973 + x,
      TRUE ~ NA_real_
    )
    
    x
  }
  
  clean_interview_year <- function(x) {
    x <- extract_first_number(x)
    
    x[x %in% c(96, 97, 98, 99, 9996, 9997, 9998, 9999)] <- NA_real_
    
    x <- dplyr::case_when(
      is.na(x) ~ NA_real_,
      x >= 1900 & x <= 2026 ~ x,
      x >= 90 & x <= 99 ~ 1900 + x,
      x >= 0 & x <= 9 ~ 1990 + x,
      TRUE ~ NA_real_
    )
    
    x
  }
  
  safe_make_date <- function(year, month, day) {
    out <- rep(as.Date(NA), length(year))
    
    valid <- !is.na(year) &
      !is.na(month) &
      !is.na(day) &
      year >= 1900 &
      year <= 2026 &
      month >= 1 &
      month <= 12 &
      day >= 1 &
      day <= 31
    
    date_chr <- rep(NA_character_, length(year))
    
    date_chr[valid] <- sprintf(
      "%04d-%02d-%02d",
      as.integer(year[valid]),
      as.integer(month[valid]),
      as.integer(day[valid])
    )
    
    out[valid] <- suppressWarnings(
      as.Date(date_chr[valid], format = "%Y-%m-%d")
    )
    
    out
  }
  
  bmonth <- clean_month(df[[bmonth_var]])
  byear  <- clean_birth_year(df[[byear_var]])
  imonth <- clean_month(df[[imonth_var]])
  iday   <- clean_day(df[[iday_var]])
  iyear  <- clean_interview_year(df[[iyear_var]])
  
  birth_date <- safe_make_date(
    year = byear,
    month = bmonth,
    day = rep(15, length(byear))
  )
  
  interview_date <- safe_make_date(
    year = iyear,
    month = imonth,
    day = iday
  )
  
  age <- floor(as.numeric(interview_date - birth_date) / 365.25)
  
  age[age < 10 | age > 25] <- NA_real_
  
  list(
    age = age,
    source = paste0(
      "constructed_from_",
      paste(c(imonth_var, iday_var, iyear_var, bmonth_var, byear_var), collapse = "_")
    ),
    variables = paste(c(imonth_var, iday_var, iyear_var, bmonth_var, byear_var), collapse = ", ")
  )
}

summarise_construct <- function(data, var, construct_label) {
  if (!(var %in% names(data))) {
    return(tibble(
      construct = construct_label,
      variable = var,
      n_valid = NA_integer_,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_
    ))
  }
  
  x <- data[[var]]
  n_valid <- sum(!is.na(x))
  
  if (n_valid == 0) {
    return(tibble(
      construct = construct_label,
      variable = var,
      n_valid = 0L,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_
    ))
  }
  
  tibble(
    construct = construct_label,
    variable = var,
    n_valid = n_valid,
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

# ------------------------------------------------------------
# 4. Load raw Wave I file
# ------------------------------------------------------------

raw_path <- "data/raw/21600-0001-Data.rda"

if (!file.exists(raw_path)) {
  stop(
    "Raw Add Health file not found: ",
    raw_path,
    call. = FALSE
  )
}

env_raw <- new.env(parent = emptyenv())
loaded_objects <- load(raw_path, envir = env_raw)

data_objects <- loaded_objects[
  vapply(
    loaded_objects,
    function(x) is.data.frame(get(x, envir = env_raw)),
    logical(1)
  )
]

if (length(data_objects) == 0) {
  stop("No data.frame object found in raw file.", call. = FALSE)
}

df <- get(data_objects[1], envir = env_raw) |>
  tibble::as_tibble()

log_line("Loaded raw file: ", raw_path)
log_line("Selected object: ", data_objects[1])
log_line("Rows: ", nrow(df), " | Columns: ", ncol(df))

# ------------------------------------------------------------
# 5. Identify sample variables
# ------------------------------------------------------------

id_aliases <- c("AID", "aid", "respondent_id", "id", "caseid", "case_id")

age_aliases <- c(
  "age",
  "AGE",
  "age_w1",
  "age_wave1",
  "respondent_age",
  "respondent_age_w1",
  "age_years",
  "calculated_age",
  "H1AGE"
)

sexever_aliases <- c(
  "sexual_initiation",
  "sex_initiation",
  "ever_had_sex",
  "ever_sex",
  "had_sex",
  "had_sex_ever",
  "vaginal_intercourse_ever",
  "H1CO1",
  "h1co1"
)

weight_aliases <- c("GSWGT1", "gswgt1", "weight", "wave1_weight", "sample_weight")

id_var <- find_var(df, id_aliases)
age_var <- find_var(df, age_aliases)
sexever_var <- find_var(df, sexever_aliases)
weight_var <- find_var(df, weight_aliases)

if (is.na(sexever_var)) {
  stop(
    "Ever-sex / sexual initiation variable not found.",
    call. = FALSE
  )
}

if (!is.na(age_var)) {
  age_vector <- to_numeric_survey(df[[age_var]])
  age_source <- age_var
} else {
  age_calc <- compute_wave1_age_from_dates(df)
  age_vector <- age_calc$age
  age_source <- age_calc$source
}

if (all(is.na(age_vector))) {
  stop("Age could not be constructed.", call. = FALSE)
}

df_work <- df |>
  mutate(
    .age_19a_v3 = age_vector,
    .ever_sex_19a_v3 = recode_ever_sex(.data[[sexever_var]]),
    .never_sex_19a_v3 = .ever_sex_19a_v3 == 0,
    .age_15_19_19a_v3 = .age_19a_v3 >= 15 & .age_19a_v3 <= 19
  )

analysis_df <- df_work |>
  filter(.age_15_19_19a_v3, .never_sex_19a_v3)

if (nrow(analysis_df) == 0) {
  stop("Filtered sample has zero observations.", call. = FALSE)
}

log_line("Age source: ", age_source)
log_line("Ever-sex variable: ", sexever_var)
log_line("Final analytic sample n: ", nrow(analysis_df))

# ------------------------------------------------------------
# 6. Define corrected source variables
# ------------------------------------------------------------

family_vars <- c(
  parents_care = "H1PR3",
  family_understand = "H1PR5",
  family_fun = "H1PR7",
  family_attention = "H1PR8"
)

friend_vars <- c(
  friends_care = "H1PR4"
)

deterrence_vars <- c(
  partner_loses_respect = "H1MO2",
  guilt_after_sex = "H1MO3",
  mother_upset = "H1MO4"
)

required_construct_vars <- c(
  family_vars,
  friend_vars,
  deterrence_vars
)

missing_construct_vars <- required_construct_vars[
  !(std_key(required_construct_vars) %in% std_key(names(df)))
]

if (length(missing_construct_vars) > 0) {
  warning(
    "Some construct variables were not found in the raw file: ",
    paste(missing_construct_vars, collapse = ", ")
  )
}

resolve_vars <- function(var_vector, df) {
  out <- var_vector[
    std_key(var_vector) %in% std_key(names(df))
  ]
  
  if (length(out) == 0) {
    return(character())
  }
  
  names(df)[match(std_key(out), std_key(names(df)))]
}

family_detected <- resolve_vars(family_vars, df)
names(family_detected) <- names(family_vars)[
  std_key(family_vars) %in% std_key(names(df))
]

friend_detected <- resolve_vars(friend_vars, df)
names(friend_detected) <- names(friend_vars)[
  std_key(friend_vars) %in% std_key(names(df))
]

deterrence_detected <- resolve_vars(deterrence_vars, df)
names(deterrence_detected) <- names(deterrence_vars)[
  std_key(deterrence_vars) %in% std_key(names(df))
]

# ------------------------------------------------------------
# 7. Build construct dataset
# ------------------------------------------------------------

base_cols <- unique(na.omit(c(id_var, sexever_var, weight_var)))

construct_df <- analysis_df |>
  select(any_of(base_cols)) |>
  mutate(
    age_19a_v3 = analysis_df$.age_19a_v3,
    ever_sex_19a_v3 = analysis_df$.ever_sex_19a_v3,
    never_sex_19a_v3 = analysis_df$.never_sex_19a_v3
  )

# Family connectedness:
# H1PR items use higher values as stronger connection/support.
# Valid values: 1–5. Special codes are set to missing.
# No reverse coding.

family_score_cols <- character()

if (length(family_detected) > 0) {
  for (item_name in names(family_detected)) {
    source_var <- family_detected[[item_name]]
    new_var <- paste0("family_", item_name, "_prot_1_5")
    
    construct_df[[new_var]] <- clean_valid_1_5(analysis_df[[source_var]])
    family_score_cols <- c(family_score_cols, new_var)
  }
  
  family_min_items <- ifelse(length(family_score_cols) >= 2, 2, 1)
  
  family_matrix <- construct_df |>
    dplyr::select(dplyr::all_of(family_score_cols))
  
  construct_df$family_connectedness_item_n <- rowSums(!is.na(family_matrix))
  
  construct_df$family_connectedness_mean_1_5 <- row_mean_min(
    family_matrix,
    min_items = family_min_items
  )
  
  construct_df$family_connectedness_z <- z_score(
    construct_df$family_connectedness_mean_1_5
  )
}

# Friend support:
# H1PR4 uses higher values as stronger friend support.
# Valid values: 1–5. Special codes are set to missing.
# No reverse coding.

friend_score_cols <- character()

if (length(friend_detected) > 0) {
  for (item_name in names(friend_detected)) {
    source_var <- friend_detected[[item_name]]
    new_var <- paste0("friend_", item_name, "_prot_1_5")
    
    construct_df[[new_var]] <- clean_valid_1_5(analysis_df[[source_var]])
    friend_score_cols <- c(friend_score_cols, new_var)
  }
  
  friend_matrix <- construct_df |>
    dplyr::select(dplyr::all_of(friend_score_cols))
  
  construct_df$friend_support_item_n <- rowSums(!is.na(friend_matrix))
  
  construct_df$friend_support_mean_1_5 <- row_mean_min(
    friend_matrix,
    min_items = 1
  )
  
  construct_df$friend_support_z <- z_score(
    construct_df$friend_support_mean_1_5
  )
}

# Sexual delay deterrence:
# H1MO2-H1MO4 use:
#   1 = strongly agree
#   2 = agree
#   3 = neither agree nor disagree
#   4 = disagree
#   5 = strongly disagree
# For these items, agreement means stronger deterrence.
# Therefore: protective/deterrence score = 6 - raw value.

deterrence_score_cols <- character()

if (length(deterrence_detected) > 0) {
  for (item_name in names(deterrence_detected)) {
    source_var <- deterrence_detected[[item_name]]
    new_var <- paste0("sexual_delay_deterrence_", item_name, "_prot_1_5")
    
    raw_value <- clean_valid_1_5(analysis_df[[source_var]])
    
    construct_df[[new_var]] <- ifelse(
      is.na(raw_value),
      NA_real_,
      6 - raw_value
    )
    
    deterrence_score_cols <- c(deterrence_score_cols, new_var)
  }
  
  deterrence_min_items <- ifelse(length(deterrence_score_cols) >= 2, 2, 1)
  
  deterrence_matrix <- construct_df |>
    dplyr::select(dplyr::all_of(deterrence_score_cols))
  
  construct_df$sexual_delay_deterrence_item_n <- rowSums(!is.na(deterrence_matrix))
  
  construct_df$sexual_delay_deterrence_mean_1_5 <- row_mean_min(
    deterrence_matrix,
    min_items = deterrence_min_items
  )
  
  construct_df$sexual_delay_deterrence_z <- z_score(
    construct_df$sexual_delay_deterrence_mean_1_5
  )
}

# ------------------------------------------------------------
# 8. Audits
# ------------------------------------------------------------

sample_audit <- tibble(
  script_id = script_id,
  selected_file = raw_path,
  selected_object = data_objects[1],
  full_n = nrow(df_work),
  age_15_19_n = sum(df_work$.age_15_19_19a_v3, na.rm = TRUE),
  ever_sex_known_n = sum(!is.na(df_work$.ever_sex_19a_v3)),
  age_15_19_ever_sex_known_n = sum(
    df_work$.age_15_19_19a_v3 & !is.na(df_work$.ever_sex_19a_v3),
    na.rm = TRUE
  ),
  final_never_sex_age_15_19_n = nrow(analysis_df),
  age_variable = age_source,
  ever_sex_variable = sexever_var,
  id_variable = ifelse(is.na(id_var), NA_character_, id_var),
  weight_variable = ifelse(is.na(weight_var), NA_character_, weight_var),
  family_items_detected = length(family_detected),
  friend_items_detected = length(friend_detected),
  deterrence_items_detected = length(deterrence_detected),
  run_time = as.character(start_time)
)

coding_audit <- bind_rows(
  tibble(
    construct = "family_connectedness",
    item_name = names(family_detected),
    source_variable = as.character(family_detected),
    source_section = "Section 18 / personal relationships",
    substantive_content = c(
      "Parents care about respondent",
      "Family understands respondent",
      "Family has fun together",
      "Family pays attention to respondent"
    )[seq_along(family_detected)],
    valid_values = "1, 2, 3, 4, 5",
    missing_values = "All values outside 1-5 set to missing",
    direction_rule = "Higher raw values indicate higher family connectedness/protection",
    transformation = "direct",
    reverse_coded = FALSE,
    construct_role = "protective family connectedness"
  ),
  tibble(
    construct = "friend_support",
    item_name = names(friend_detected),
    source_variable = as.character(friend_detected),
    source_section = "Section 18 / personal relationships",
    substantive_content = c(
      "Friends care about respondent"
    )[seq_along(friend_detected)],
    valid_values = "1, 2, 3, 4, 5",
    missing_values = "All values outside 1-5 set to missing",
    direction_rule = "Higher raw values indicate higher friend support/protection",
    transformation = "direct",
    reverse_coded = FALSE,
    construct_role = "protective friend support"
  ),
  tibble(
    construct = "sexual_delay_deterrence",
    item_name = names(deterrence_detected),
    source_variable = as.character(deterrence_detected),
    source_section = "Section 17 / motivations to engage in or refrain from sexual intercourse before marriage",
    substantive_content = c(
      "Partner would lose respect",
      "Respondent would feel guilty afterward",
      "Mother would be upset"
    )[seq_along(deterrence_detected)],
    valid_values = "1, 2, 3, 4, 5",
    missing_values = "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable; all outside 1-5 set to missing",
    direction_rule = "Agreement indicates stronger perceived deterrence; scores reversed so higher values indicate stronger deterrence",
    transformation = "6 - raw value",
    reverse_coded = TRUE,
    construct_role = "perceived social, moral and maternal deterrents to sexual initiation"
  )
)

item_summary_one <- function(source_var, transformed_var, construct_name, item_name) {
  raw_x <- analysis_df[[source_var]]
  transformed_x <- construct_df[[transformed_var]]
  
  raw_num <- extract_first_number(raw_x)
  
  tibble(
    construct = construct_name,
    item_name = item_name,
    source_variable = source_var,
    transformed_variable = transformed_var,
    raw_levels = get_levels_text(df[[source_var]]),
    n_valid_raw_1_5 = sum(raw_num %in% 1:5, na.rm = TRUE),
    n_missing_or_special_raw = sum(!(raw_num %in% 1:5) | is.na(raw_num)),
    n_valid_transformed = sum(!is.na(transformed_x)),
    transformed_mean = mean(transformed_x, na.rm = TRUE),
    transformed_sd = sd(transformed_x, na.rm = TRUE),
    transformed_min = suppressWarnings(min(transformed_x, na.rm = TRUE)),
    transformed_max = suppressWarnings(max(transformed_x, na.rm = TRUE))
  )
}

item_summary <- bind_rows(
  purrr::map2_dfr(
    family_detected,
    names(family_detected),
    ~ item_summary_one(
      source_var = .x,
      transformed_var = paste0("family_", .y, "_prot_1_5"),
      construct_name = "family_connectedness",
      item_name = .y
    )
  ),
  purrr::map2_dfr(
    friend_detected,
    names(friend_detected),
    ~ item_summary_one(
      source_var = .x,
      transformed_var = paste0("friend_", .y, "_prot_1_5"),
      construct_name = "friend_support",
      item_name = .y
    )
  ),
  purrr::map2_dfr(
    deterrence_detected,
    names(deterrence_detected),
    ~ item_summary_one(
      source_var = .x,
      transformed_var = paste0("sexual_delay_deterrence_", .y, "_prot_1_5"),
      construct_name = "sexual_delay_deterrence",
      item_name = .y
    )
  )
) |>
  mutate(
    transformed_mean = round(transformed_mean, 4),
    transformed_sd = round(transformed_sd, 4),
    transformed_min = ifelse(is.infinite(transformed_min), NA_real_, transformed_min),
    transformed_max = ifelse(is.infinite(transformed_max), NA_real_, transformed_max)
  )

construct_summary <- bind_rows(
  summarise_construct(
    construct_df,
    "family_connectedness_mean_1_5",
    "family_connectedness"
  ),
  summarise_construct(
    construct_df,
    "friend_support_mean_1_5",
    "friend_support"
  ),
  summarise_construct(
    construct_df,
    "sexual_delay_deterrence_mean_1_5",
    "sexual_delay_deterrence"
  )
) |>
  mutate(
    mean = round(mean, 4),
    sd = round(sd, 4),
    min = round(min, 4),
    max = round(max, 4)
  )

# ------------------------------------------------------------
# 9. Save outputs
# ------------------------------------------------------------

saveRDS(construct_df, rds_path)

write_csv(sample_audit, sample_audit_path)
write_csv(coding_audit, coding_audit_path)
write_csv(item_summary, item_summary_path)
write_csv(construct_summary, construct_summary_path)

# ------------------------------------------------------------
# 10. Optional Word report
# ------------------------------------------------------------

if (has_docx) {
  doc <- officer::read_docx()
  
  doc <- officer::body_add_par(doc, script_title, style = "heading 1")
  
  doc <- officer::body_add_par(
    doc,
    paste0("Script: ", script_id),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(
    doc,
    paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Methodological decision", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste(
      "This version corrects the source alignment for family, friend and sexual deterrence constructs.",
      "Family connectedness is built from H1PR3, H1PR5, H1PR7 and H1PR8.",
      "Friend support is built from H1PR4.",
      "Sexual delay deterrence is built from H1MO2, H1MO3 and H1MO4.",
      "The deterrence construct is not treated as a full sexual delay index; it measures perceived social, moral and maternal deterrents to sexual initiation."
    ),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Sample audit", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(sample_audit))
  )
  
  doc <- officer::body_add_par(doc, "Construct summary", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(construct_summary))
  )
  
  doc <- officer::body_add_par(doc, "Coding audit", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(coding_audit))
  )
  
  doc <- officer::body_add_par(doc, "Item summary", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(item_summary))
  )
  
  print(doc, target = docx_path)
}

# ------------------------------------------------------------
# 11. Console output
# ------------------------------------------------------------

end_time <- Sys.time()

log_line("Saved construct dataset: ", rds_path)
log_line("Saved sample audit: ", sample_audit_path)
log_line("Saved coding audit: ", coding_audit_path)
log_line("Saved item summary: ", item_summary_path)
log_line("Saved construct summary: ", construct_summary_path)

if (has_docx) {
  log_line("Saved Word report: ", docx_path)
} else {
  log_line("Word report not created because officer/flextable were unavailable.")
}

log_line("Completed ", script_id, " in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
log_line("No Git action was performed.")

cat("\n============================================================\n")
cat("Script 19a_v3 completed\n")
cat("============================================================\n\n")

cat("Sample audit:\n")
print(sample_audit)

cat("\nConstruct summary:\n")
print(construct_summary)

cat("\nCoding audit:\n")
print(
  coding_audit |>
    select(
      construct,
      source_variable,
      substantive_content,
      transformation,
      reverse_coded,
      construct_role
    ),
  n = Inf
)

cat("\nItem summary:\n")
print(
  item_summary |>
    select(
      construct,
      item_name,
      source_variable,
      n_valid_transformed,
      transformed_mean,
      transformed_sd,
      transformed_min,
      transformed_max
    ),
  n = Inf
)

cat("\nMain outputs:\n")
print(tibble(
  output = c(
    "Construct dataset",
    "Sample audit",
    "Coding audit",
    "Item summary",
    "Construct summary",
    "Word report",
    "Run log"
  ),
  path = c(
    rds_path,
    sample_audit_path,
    coding_audit_path,
    item_summary_path,
    construct_summary_path,
    ifelse(has_docx, docx_path, NA_character_),
    log_path
  ),
  exists = file.exists(c(
    rds_path,
    sample_audit_path,
    coding_audit_path,
    item_summary_path,
    construct_summary_path,
    ifelse(has_docx, docx_path, ""),
    log_path
  ))
))