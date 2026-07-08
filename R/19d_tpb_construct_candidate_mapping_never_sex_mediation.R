# ============================================================
# Script 19d
# TPB Construct Candidate Mapping for Never-Sex Mediation
# Project: Add Health Adolescent Risk Models
# ============================================================
#
# Purpose:
#   Map Add Health Wave I candidate variables to constructs from
#   the Theory of Planned Behavior (TPB) for a future mediation
#   analysis among adolescents aged 15–19 who have not yet had
#   sexual intercourse.
#
# Conceptual frame:
#   family_connectedness
#        -> TPB mediators
#        -> intention/proxy intention to delay sexual initiation
#
# TPB constructs reviewed:
#   1. Attitudes toward delaying sexual initiation
#   2. Subjective norms regarding sexual initiation
#   3. Perceived behavioral control / self-efficacy
#   4. Intention to delay sexual initiation
#
# Important:
#   This script does not estimate regressions or mediation models.
#   It creates a candidate mapping, coverage audit, pilot constructs,
#   and manual review template.
#
# No Git action is performed.
#
# ============================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  warn = 1
)

script_id <- "19d"
script_title <- "TPB Construct Candidate Mapping for Never-Sex Mediation"
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
  "tidyr"
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

raw_path <- "data/raw/21600-0001-Data.rda"

input_construct_rds <- "outputs/analysis/script19a_v3_corrected_never_sex_family_friend_deterrence_constructs.rds"
input_sample_audit <- "outputs/audits/script19a_v3_never_sex_sample_audit.csv"
input_19b_candidates <- "outputs/audits/script19b_sexual_delay_raw_variable_candidates.csv"

dictionary_path <- "outputs/audits/script19d_tpb_candidate_dictionary.csv"
coverage_path <- "outputs/audits/script19d_tpb_candidate_coverage_audit.csv"
pilot_item_summary_path <- "outputs/audits/script19d_tpb_pilot_item_summary.csv"
manual_review_path <- "outputs/audits/script19d_tpb_manual_review_TEMPLATE.csv"
pilot_rds_path <- "outputs/analysis/script19d_tpb_pilot_construct_dataset.rds"

pilot_construct_summary_path <- "outputs/tables/script19d_tpb_pilot_construct_summary.csv"
pilot_internal_consistency_path <- "outputs/tables/script19d_tpb_pilot_internal_consistency.csv"
pilot_construct_correlations_path <- "outputs/tables/script19d_tpb_pilot_construct_correlations.csv"

method_note_path <- "outputs/reports/script19d_tpb_mapping_methodological_note.md"
docx_path <- "outputs/reports/script19d_tpb_construct_candidate_mapping_never_sex_mediation.docx"
log_path <- "outputs/logs/script19d_run_log.txt"

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

clean_valid_range <- function(x, valid_min, valid_max) {
  out <- extract_first_number(x)
  out[!(out >= valid_min & out <= valid_max)] <- NA_real_
  out
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

z_score <- function(x) {
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  
  if (is.na(s) || s == 0) {
    return(rep(NA_real_, length(x)))
  }
  
  as.numeric((x - m) / s)
}

get_levels_text <- function(x, max_levels = 40) {
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

cronbach_alpha <- function(data, construct_label) {
  data <- as.data.frame(data)
  k <- ncol(data)
  
  if (k < 2) {
    return(tibble(
      construct = construct_label,
      n_items = k,
      n_complete = NA_integer_,
      alpha = NA_real_,
      interpretation = "Not applicable: fewer than two items."
    ))
  }
  
  complete_data <- data[stats::complete.cases(data), , drop = FALSE]
  n_complete <- nrow(complete_data)
  
  if (n_complete < 5) {
    return(tibble(
      construct = construct_label,
      n_items = k,
      n_complete = n_complete,
      alpha = NA_real_,
      interpretation = "Not computed: too few complete cases."
    ))
  }
  
  item_vars <- apply(complete_data, 2, stats::var, na.rm = TRUE)
  total_score <- rowSums(complete_data)
  total_var <- stats::var(total_score, na.rm = TRUE)
  
  if (is.na(total_var) || total_var == 0) {
    alpha <- NA_real_
  } else {
    alpha <- (k / (k - 1)) * (1 - sum(item_vars, na.rm = TRUE) / total_var)
  }
  
  interpretation <- dplyr::case_when(
    is.na(alpha) ~ "Not interpretable.",
    alpha >= 0.80 ~ "High internal consistency.",
    alpha >= 0.70 ~ "Acceptable internal consistency.",
    alpha >= 0.60 ~ "Moderate internal consistency; acceptable for exploratory work with few items.",
    alpha >= 0.50 ~ "Low-to-moderate consistency; interpret cautiously.",
    TRUE ~ "Low internal consistency; do not treat as homogeneous without justification."
  )
  
  tibble(
    construct = construct_label,
    n_items = k,
    n_complete = n_complete,
    alpha = round(alpha, 4),
    interpretation = interpretation
  )
}

safe_cor <- function(data) {
  data <- as.data.frame(data)
  
  if (ncol(data) < 2) {
    return(matrix(NA_real_, nrow = ncol(data), ncol = ncol(data)))
  }
  
  suppressWarnings(
    cor(data, use = "pairwise.complete.obs")
  )
}

cor_to_long <- function(cor_mat, label) {
  out <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  
  names(out) <- c("var1", "var2", "correlation")
  
  out |>
    mutate(
      block = label,
      correlation = round(as.numeric(correlation), 4)
    ) |>
    filter(var1 < var2) |>
    arrange(block, var1, var2)
}

summarise_score <- function(data, var, construct_label) {
  if (!(var %in% names(data))) {
    return(tibble(
      construct = construct_label,
      variable = var,
      n_valid = NA_integer_,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      q25 = NA_real_,
      median = NA_real_,
      q75 = NA_real_,
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
      q25 = NA_real_,
      median = NA_real_,
      q75 = NA_real_,
      max = NA_real_
    ))
  }
  
  tibble(
    construct = construct_label,
    variable = var,
    n_valid = n_valid,
    mean = round(mean(x, na.rm = TRUE), 4),
    sd = round(sd(x, na.rm = TRUE), 4),
    min = round(min(x, na.rm = TRUE), 4),
    q25 = round(as.numeric(quantile(x, 0.25, na.rm = TRUE)), 4),
    median = round(median(x, na.rm = TRUE), 4),
    q75 = round(as.numeric(quantile(x, 0.75, na.rm = TRUE)), 4),
    max = round(max(x, na.rm = TRUE), 4)
  )
}

# ------------------------------------------------------------
# 4. Load raw Wave I file
# ------------------------------------------------------------

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
# 5. Reconstruct analytic sample: age 15–19 and never had sex
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
    .age_19d = age_vector,
    .ever_sex_19d = recode_ever_sex(.data[[sexever_var]]),
    .never_sex_19d = .ever_sex_19d == 0,
    .age_15_19_19d = .age_19d >= 15 & .age_19d <= 19
  )

analysis_df <- df_work |>
  filter(.age_15_19_19d, .never_sex_19d)

if (nrow(analysis_df) == 0) {
  stop("Filtered sample has zero observations.", call. = FALSE)
}

sample_audit <- tibble(
  script_id = script_id,
  selected_file = raw_path,
  selected_object = data_objects[1],
  full_n = nrow(df_work),
  age_15_19_n = sum(df_work$.age_15_19_19d, na.rm = TRUE),
  ever_sex_known_n = sum(!is.na(df_work$.ever_sex_19d)),
  age_15_19_ever_sex_known_n = sum(
    df_work$.age_15_19_19d & !is.na(df_work$.ever_sex_19d),
    na.rm = TRUE
  ),
  final_never_sex_age_15_19_n = nrow(analysis_df),
  age_variable = age_source,
  ever_sex_variable = sexever_var,
  id_variable = ifelse(is.na(id_var), NA_character_, id_var),
  weight_variable = ifelse(is.na(weight_var), NA_character_, weight_var),
  run_time = as.character(start_time)
)

log_line("Age source: ", age_source)
log_line("Ever-sex variable: ", sexever_var)
log_line("Final analytic sample n: ", nrow(analysis_df))

# ------------------------------------------------------------
# 6. Load 19a_v3 constructs, if available
# ------------------------------------------------------------

if (file.exists(input_construct_rds)) {
  construct_19a_v3 <- readRDS(input_construct_rds)
  
  log_line("Loaded 19a_v3 construct dataset: ", input_construct_rds)
} else {
  construct_19a_v3 <- NULL
  
  warning(
    "19a_v3 construct dataset not found. TPB mapping will continue without family/friend construct merge."
  )
}

if (file.exists(input_sample_audit)) {
  sample_audit_19a_v3 <- read_csv(input_sample_audit, show_col_types = FALSE)
} else {
  sample_audit_19a_v3 <- NULL
}

# ------------------------------------------------------------
# 7. TPB candidate dictionary
# ------------------------------------------------------------

# Notes:
# - H1MO variables are from Section 17 and are the strongest source
#   for attitudes and subjective norms about sexual initiation.
# - H1SE, H1BC and H1PA variables are flagged as candidate sources
#   for TPB-related self-efficacy/control, knowledge, attitudes or
#   approval constructs, but need manual review before final use.
# - Intention to delay sexual initiation is not automatically assumed.
#   It must be confirmed by variable wording.

tpb_dictionary <- tibble::tribble(
  ~variable, ~source_section, ~tpb_domain, ~subdomain, ~item_text, ~valid_min, ~valid_max, ~special_missing_codes, ~direction_rule, ~transformation, ~pilot_use, ~candidate_status, ~review_priority,
  
  "H1MO1",  "Section 17", "subjective_norms", "peer_approval_of_sex", "If you had sexual intercourse, your friends would respect you more.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Disagreement indicates weaker perceived peer approval of sex and stronger delay-supportive norm.", "direct", TRUE, "candidate", "High",
  
  "H1MO2",  "Section 17", "subjective_norms", "partner_disapproval", "If you had sexual intercourse, your partner would lose respect for you.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Agreement indicates stronger perceived social sanction and stronger delay-supportive norm.", "6 - raw value", TRUE, "candidate", "High",
  
  "H1MO3",  "Section 17", "attitudes", "anticipated_guilt", "If you had sexual intercourse, afterward, you would feel guilty.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Agreement indicates stronger negative affective attitude toward sex at this time.", "6 - raw value", TRUE, "candidate", "High",
  
  "H1MO4",  "Section 17", "subjective_norms", "maternal_disapproval", "If you had sexual intercourse, it would upset mother.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Agreement indicates stronger perceived maternal disapproval.", "6 - raw value", TRUE, "candidate", "High",
  
  "H1MO5",  "Section 17", "attitudes", "physical_pleasure", "If you had sexual intercourse, it would give you a great deal of physical pleasure.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Disagreement indicates weaker perceived benefit of sex and stronger delay-supportive attitude.", "direct", TRUE, "candidate", "Medium",
  
  "H1MO6",  "Section 17", "attitudes", "relaxation", "If you had sexual intercourse, it would relax you.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Disagreement indicates weaker perceived benefit of sex and stronger delay-supportive attitude.", "direct", TRUE, "candidate", "Medium",
  
  "H1MO7",  "Section 17", "attitudes", "attractiveness", "If you had sexual intercourse, it would make you more attractive.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Disagreement indicates weaker perceived status/attractiveness benefit of sex.", "direct", TRUE, "candidate", "Medium",
  
  "H1MO8",  "Section 17", "attitudes", "less_lonely", "If you had sexual intercourse, you would feel less lonely.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Disagreement indicates weaker perceived emotional benefit of sex.", "direct", TRUE, "candidate", "Medium",
  
  "H1MO9",  "Section 17", "attitudes", "pregnancy_embarrass_family", "If pregnancy occurred, it would be embarrassing for your family.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Agreement indicates stronger perceived negative consequence.", "6 - raw value", TRUE, "candidate", "Medium",
  
  "H1MO10", "Section 17", "attitudes", "pregnancy_embarrass_self", "If pregnancy occurred, it would be embarrassing for you.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Agreement indicates stronger perceived negative consequence.", "6 - raw value", TRUE, "candidate", "Medium",
  
  "H1MO11", "Section 17", "attitudes", "pregnancy_quit_school", "If pregnancy occurred, you would have to quit school.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Agreement indicates stronger perceived educational cost.", "6 - raw value", TRUE, "candidate", "Medium",
  
  "H1MO12", "Section 17", "attitudes", "pregnancy_wrong_marriage", "If pregnancy occurred, you might marry the wrong person just to get married.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Agreement indicates stronger perceived future cost.", "6 - raw value", TRUE, "candidate", "Medium",
  
  "H1MO13", "Section 17", "attitudes", "pregnancy_grow_up_fast", "If pregnancy occurred, you would be forced to grow up too fast.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Agreement indicates stronger perceived developmental cost.", "6 - raw value", TRUE, "candidate", "Medium",
  
  "H1MO14", "Section 17", "attitudes", "pregnancy_decision_stress", "If pregnancy occurred, decision about the baby would be stressful and difficult.", 1, 5, "6 refused; 7 legitimate skip; 8 don't know; 9 not applicable", "Agreement indicates stronger perceived stress/cost.", "6 - raw value", TRUE, "candidate", "Medium",
  
  "H1SE1",  "Candidate from 19b", "perceived_behavioral_control", "manual_review_required", "Candidate self-efficacy/control-related item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "High",
  
  "H1SE2",  "Candidate from 19b", "perceived_behavioral_control", "manual_review_required", "Candidate self-efficacy/control-related item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "High",
  
  "H1SE3",  "Candidate from 19b", "perceived_behavioral_control", "manual_review_required", "Candidate self-efficacy/control-related item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "High",
  
  "H1SE4",  "Candidate from 19b", "perceived_behavioral_control", "manual_review_required", "Candidate self-efficacy/control-related item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "High",
  
  "H1BC1",  "Candidate from 19b", "perceived_behavioral_control", "birth_control_or_contraception", "Candidate birth-control/contraception item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1BC2",  "Candidate from 19b", "perceived_behavioral_control", "birth_control_or_contraception", "Candidate birth-control/contraception item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1BC3",  "Candidate from 19b", "perceived_behavioral_control", "birth_control_or_contraception", "Candidate birth-control/contraception item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1BC4",  "Candidate from 19b", "perceived_behavioral_control", "birth_control_or_contraception", "Candidate birth-control/contraception item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1BC5",  "Candidate from 19b", "perceived_behavioral_control", "birth_control_or_contraception", "Candidate birth-control/contraception item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1BC6",  "Candidate from 19b", "perceived_behavioral_control", "birth_control_or_contraception", "Candidate birth-control/contraception item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1BC7",  "Candidate from 19b", "perceived_behavioral_control", "birth_control_or_contraception", "Candidate birth-control/contraception item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1BC8",  "Candidate from 19b", "perceived_behavioral_control", "birth_control_or_contraception", "Candidate birth-control/contraception item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1PA3",  "Candidate from 19b", "subjective_norms", "approval_or_parental_attitude", "Candidate approval/norm-related item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1PA6",  "Candidate from 19b", "subjective_norms", "approval_or_parental_attitude", "Candidate approval/norm-related item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium",
  
  "H1PA7",  "Candidate from 19b", "subjective_norms", "approval_or_parental_attitude", "Candidate approval/norm-related item identified by 19b. Review exact wording before final use.", 1, 5, "To be confirmed", "To be confirmed from codebook wording.", "manual_review", FALSE, "candidate_requires_review", "Medium"
)

# Add 19b evidence if the file exists.

if (file.exists(input_19b_candidates)) {
  candidates_19b <- read_csv(input_19b_candidates, show_col_types = FALSE) |>
    select(
      variable,
      candidate_score,
      priority_19b = priority,
      likely_role_19b = likely_role,
      n_valid_never_sex_sample_19b = n_valid_never_sex_sample,
      pct_valid_never_sex_sample_19b = pct_valid_never_sex_sample,
      matched_terms_metadata,
      codebook_hit_n
    )
  
  tpb_dictionary <- tpb_dictionary |>
    left_join(candidates_19b, by = "variable")
} else {
  tpb_dictionary <- tpb_dictionary |>
    mutate(
      candidate_score = NA_real_,
      priority_19b = NA_character_,
      likely_role_19b = NA_character_,
      n_valid_never_sex_sample_19b = NA_real_,
      pct_valid_never_sex_sample_19b = NA_real_,
      matched_terms_metadata = NA_character_,
      codebook_hit_n = NA_real_
    )
}

write_csv(tpb_dictionary, dictionary_path)

# ------------------------------------------------------------
# 8. Candidate coverage audit
# ------------------------------------------------------------

coverage_one <- function(row) {
  v <- row$variable
  
  if (!(v %in% names(df))) {
    return(tibble(
      variable = v,
      present_in_raw = FALSE,
      tpb_domain = row$tpb_domain,
      subdomain = row$subdomain,
      review_priority = row$review_priority,
      valid_min = row$valid_min,
      valid_max = row$valid_max,
      n_total_never_sex_sample = nrow(analysis_df),
      n_valid_declared_range = NA_integer_,
      pct_valid_declared_range = NA_real_,
      n_missing_or_outside_declared_range = NA_integer_,
      raw_observed_min = NA_real_,
      raw_observed_max = NA_real_,
      raw_levels = NA_character_,
      transformation = row$transformation,
      candidate_status = row$candidate_status
    ))
  }
  
  raw_x <- analysis_df[[v]]
  raw_num <- extract_first_number(raw_x)
  
  valid <- !is.na(raw_num) &
    raw_num >= row$valid_min &
    raw_num <= row$valid_max
  
  tibble(
    variable = v,
    present_in_raw = TRUE,
    tpb_domain = row$tpb_domain,
    subdomain = row$subdomain,
    review_priority = row$review_priority,
    valid_min = row$valid_min,
    valid_max = row$valid_max,
    n_total_never_sex_sample = nrow(analysis_df),
    n_valid_declared_range = sum(valid),
    pct_valid_declared_range = round(100 * sum(valid) / nrow(analysis_df), 2),
    n_missing_or_outside_declared_range = sum(!valid),
    raw_observed_min = suppressWarnings(ifelse(all(is.na(raw_num)), NA_real_, min(raw_num, na.rm = TRUE))),
    raw_observed_max = suppressWarnings(ifelse(all(is.na(raw_num)), NA_real_, max(raw_num, na.rm = TRUE))),
    raw_levels = get_levels_text(df[[v]]),
    transformation = row$transformation,
    candidate_status = row$candidate_status
  )
}

coverage_audit <- purrr::map_dfr(
  seq_len(nrow(tpb_dictionary)),
  ~ coverage_one(tpb_dictionary[.x, ])
)

write_csv(coverage_audit, coverage_path)

# ------------------------------------------------------------
# 9. Build pilot TPB constructs from H1MO items only
# ------------------------------------------------------------

pilot_df <- tibble(
  row_id_19d = seq_len(nrow(analysis_df)),
  age_19d = analysis_df$.age_19d,
  ever_sex_19d = analysis_df$.ever_sex_19d,
  never_sex_19d = analysis_df$.never_sex_19d
)

if (!is.na(id_var) && id_var %in% names(analysis_df)) {
  pilot_df[[id_var]] <- analysis_df[[id_var]]
}

if (!is.na(weight_var) && weight_var %in% names(analysis_df)) {
  pilot_df[[weight_var]] <- analysis_df[[weight_var]]
}

# Bring validated 19a_v3 constructs when row counts match.

if (!is.null(construct_19a_v3) && nrow(construct_19a_v3) == nrow(pilot_df)) {
  add_cols <- intersect(
    c(
      "family_connectedness_mean_1_5",
      "family_connectedness_z",
      "friend_support_mean_1_5",
      "friend_support_z",
      "sexual_delay_deterrence_mean_1_5",
      "sexual_delay_deterrence_z"
    ),
    names(construct_19a_v3)
  )
  
  pilot_df <- bind_cols(
    pilot_df,
    construct_19a_v3 |>
      select(all_of(add_cols))
  )
  
  log_line("Merged selected 19a_v3 construct columns into pilot dataset.")
} else {
  log_line("19a_v3 construct merge skipped because file is missing or row count differs.")
}

pilot_items <- tpb_dictionary |>
  filter(pilot_use == TRUE)

pilot_item_names <- character()

for (i in seq_len(nrow(pilot_items))) {
  v <- pilot_items$variable[i]
  
  if (!(v %in% names(analysis_df))) {
    next
  }
  
  raw_value <- clean_valid_range(
    analysis_df[[v]],
    valid_min = pilot_items$valid_min[i],
    valid_max = pilot_items$valid_max[i]
  )
  
  score <- dplyr::case_when(
    pilot_items$transformation[i] == "direct" ~ raw_value,
    pilot_items$transformation[i] == "6 - raw value" ~ 6 - raw_value,
    TRUE ~ NA_real_
  )
  
  new_var <- paste0(
    "tpb_",
    pilot_items$tpb_domain[i],
    "_",
    pilot_items$subdomain[i],
    "_",
    v,
    "_score_1_5"
  )
  
  pilot_df[[new_var]] <- score
  pilot_item_names <- c(pilot_item_names, new_var)
}

# Define pilot constructs.
# These are not final scales. They are diagnostic summaries for review.

subjective_norm_item_vars <- names(pilot_df)[
  str_detect(names(pilot_df), "^tpb_subjective_norms_")
]

attitude_item_vars <- names(pilot_df)[
  str_detect(names(pilot_df), "^tpb_attitudes_")
]

# A narrower deterrence proxy already validated in 19a_v3 can be compared,
# but here we build TPB pilot blocks.

if (length(subjective_norm_item_vars) > 0) {
  subjective_norm_matrix <- pilot_df |>
    select(all_of(subjective_norm_item_vars))
  
  pilot_df$tpb_subjective_norms_delay_mean_1_5 <- row_mean_min(
    subjective_norm_matrix,
    min_items = ifelse(length(subjective_norm_item_vars) >= 2, 2, 1)
  )
  
  pilot_df$tpb_subjective_norms_delay_z <- z_score(
    pilot_df$tpb_subjective_norms_delay_mean_1_5
  )
}

if (length(attitude_item_vars) > 0) {
  attitude_matrix <- pilot_df |>
    select(all_of(attitude_item_vars))
  
  pilot_df$tpb_attitudes_delay_mean_1_5 <- row_mean_min(
    attitude_matrix,
    min_items = ceiling(length(attitude_item_vars) / 2)
  )
  
  pilot_df$tpb_attitudes_delay_z <- z_score(
    pilot_df$tpb_attitudes_delay_mean_1_5
  )
}

# A provisional combined TPB delay orientation proxy combines attitude and
# subjective norm blocks. This is NOT a final intention measure.

combined_tpb_vars <- intersect(
  c(
    "tpb_attitudes_delay_mean_1_5",
    "tpb_subjective_norms_delay_mean_1_5"
  ),
  names(pilot_df)
)

if (length(combined_tpb_vars) > 0) {
  combined_matrix <- pilot_df |>
    select(all_of(combined_tpb_vars))
  
  pilot_df$tpb_delay_orientation_proxy_mean_1_5 <- row_mean_min(
    combined_matrix,
    min_items = 1
  )
  
  pilot_df$tpb_delay_orientation_proxy_z <- z_score(
    pilot_df$tpb_delay_orientation_proxy_mean_1_5
  )
}

# ------------------------------------------------------------
# 10. Pilot item and construct summaries
# ------------------------------------------------------------

pilot_item_summary <- pilot_items |>
  mutate(
    transformed_variable = paste0(
      "tpb_",
      tpb_domain,
      "_",
      subdomain,
      "_",
      variable,
      "_score_1_5"
    )
  ) |>
  rowwise() |>
  mutate(
    present_in_pilot = transformed_variable %in% names(pilot_df),
    n_valid_transformed = ifelse(
      present_in_pilot,
      sum(!is.na(pilot_df[[transformed_variable]])),
      NA_integer_
    ),
    pct_valid_transformed = ifelse(
      present_in_pilot,
      round(100 * n_valid_transformed / nrow(pilot_df), 2),
      NA_real_
    ),
    transformed_mean = ifelse(
      present_in_pilot,
      mean(pilot_df[[transformed_variable]], na.rm = TRUE),
      NA_real_
    ),
    transformed_sd = ifelse(
      present_in_pilot,
      sd(pilot_df[[transformed_variable]], na.rm = TRUE),
      NA_real_
    ),
    transformed_min = ifelse(
      present_in_pilot,
      min(pilot_df[[transformed_variable]], na.rm = TRUE),
      NA_real_
    ),
    transformed_max = ifelse(
      present_in_pilot,
      max(pilot_df[[transformed_variable]], na.rm = TRUE),
      NA_real_
    )
  ) |>
  ungroup() |>
  mutate(
    transformed_mean = round(transformed_mean, 4),
    transformed_sd = round(transformed_sd, 4),
    transformed_min = round(transformed_min, 4),
    transformed_max = round(transformed_max, 4)
  )

write_csv(pilot_item_summary, pilot_item_summary_path)

pilot_construct_vars <- intersect(
  c(
    "tpb_subjective_norms_delay_mean_1_5",
    "tpb_attitudes_delay_mean_1_5",
    "tpb_delay_orientation_proxy_mean_1_5",
    "sexual_delay_deterrence_mean_1_5",
    "family_connectedness_mean_1_5",
    "friend_support_mean_1_5"
  ),
  names(pilot_df)
)

pilot_construct_summary <- bind_rows(
  purrr::map_dfr(
    pilot_construct_vars,
    ~ summarise_score(
      pilot_df,
      .x,
      dplyr::case_when(
        .x == "tpb_subjective_norms_delay_mean_1_5" ~ "tpb_subjective_norms_delay",
        .x == "tpb_attitudes_delay_mean_1_5" ~ "tpb_attitudes_delay",
        .x == "tpb_delay_orientation_proxy_mean_1_5" ~ "tpb_delay_orientation_proxy",
        .x == "sexual_delay_deterrence_mean_1_5" ~ "sexual_delay_deterrence_19a_v3",
        .x == "family_connectedness_mean_1_5" ~ "family_connectedness_19a_v3",
        .x == "friend_support_mean_1_5" ~ "friend_support_19a_v3",
        TRUE ~ .x
      )
    )
  )
)

write_csv(pilot_construct_summary, pilot_construct_summary_path)

# ------------------------------------------------------------
# 11. Internal consistency and correlations
# ------------------------------------------------------------

pilot_internal_consistency <- bind_rows(
  cronbach_alpha(
    pilot_df |>
      select(any_of(subjective_norm_item_vars)),
    "tpb_subjective_norms_delay"
  ),
  cronbach_alpha(
    pilot_df |>
      select(any_of(attitude_item_vars)),
    "tpb_attitudes_delay"
  )
)

write_csv(pilot_internal_consistency, pilot_internal_consistency_path)

cor_vars <- intersect(
  c(
    "family_connectedness_mean_1_5",
    "friend_support_mean_1_5",
    "sexual_delay_deterrence_mean_1_5",
    "tpb_subjective_norms_delay_mean_1_5",
    "tpb_attitudes_delay_mean_1_5",
    "tpb_delay_orientation_proxy_mean_1_5"
  ),
  names(pilot_df)
)

if (length(cor_vars) >= 2) {
  pilot_correlations <- cor_to_long(
    safe_cor(
      pilot_df |>
        select(all_of(cor_vars))
    ),
    "tpb_pilot_construct_level"
  )
} else {
  pilot_correlations <- tibble(
    var1 = character(),
    var2 = character(),
    correlation = numeric(),
    block = character()
  )
}

write_csv(pilot_correlations, pilot_construct_correlations_path)

# ------------------------------------------------------------
# 12. Manual review template
# ------------------------------------------------------------

manual_review_template <- tpb_dictionary |>
  left_join(
    coverage_audit |>
      select(
        variable,
        present_in_raw,
        n_valid_declared_range,
        pct_valid_declared_range,
        raw_observed_min,
        raw_observed_max,
        raw_levels
      ),
    by = "variable"
  ) |>
  transmute(
    script_id = script_id,
    variable,
    present_in_raw,
    source_section,
    tpb_domain,
    subdomain,
    item_text,
    valid_min,
    valid_max,
    special_missing_codes,
    direction_rule,
    proposed_transformation = transformation,
    pilot_use,
    candidate_status,
    review_priority,
    n_valid_never_sex_sample = n_valid_declared_range,
    pct_valid_never_sex_sample = pct_valid_declared_range,
    raw_observed_min,
    raw_observed_max,
    raw_levels,
    candidate_score_19b = candidate_score,
    priority_19b,
    likely_role_19b,
    matched_terms_metadata,
    codebook_hit_n,
    reviewer_keep = "",
    reviewer_final_tpb_domain = "",
    reviewer_final_construct_name = "",
    reviewer_final_transformation = "",
    reviewer_notes = ""
  ) |>
  arrange(
    factor(review_priority, levels = c("High", "Medium", "Low")),
    tpb_domain,
    variable
  )

write_csv(manual_review_template, manual_review_path)

# ------------------------------------------------------------
# 13. Save pilot dataset
# ------------------------------------------------------------

saveRDS(pilot_df, pilot_rds_path)

# ------------------------------------------------------------
# 14. Methodological note
# ------------------------------------------------------------

method_note <- c(
  "# Script 19d — TPB Construct Candidate Mapping for Never-Sex Mediation",
  "",
  paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
  "",
  "## Purpose",
  "",
  "This script maps Add Health Wave I candidate variables to Theory of Planned Behavior constructs for future mediation analysis.",
  "",
  "The intended conceptual model is:",
  "",
  "`family_connectedness -> TPB mediators -> intention/proxy intention to delay sexual initiation`",
  "",
  "## Analytic sample",
  "",
  paste0(
    "The analytic sample includes adolescents aged 15–19 who had not yet had sexual intercourse. ",
    "Final analytic sample size: ", nrow(analysis_df), "."
  ),
  "",
  "## Theoretical decision",
  "",
  "The mediation model should not use sexual initiation as the dependent variable within the never-sex sample because sexual initiation is constant by design.",
  "",
  "A future mediation model should first establish whether Add Health contains an adequate measure of intention to delay sexual initiation. If no direct intention item is available, the analysis should be described as an exploratory TPB-compatible mechanism analysis, not as a full TPB mediation model.",
  "",
  "## TPB candidate domains",
  "",
  "- Attitudes: mainly H1MO3, H1MO5-H1MO14;",
  "- Subjective norms: mainly H1MO1, H1MO2, H1MO4;",
  "- Perceived behavioral control / self-efficacy: H1SE and H1BC candidates require manual review;",
  "- Intention to delay sexual initiation: no direct item is confirmed by this script; manual review is required.",
  "",
  "## Outputs",
  "",
  paste0("- Candidate dictionary: ", dictionary_path),
  paste0("- Coverage audit: ", coverage_path),
  paste0("- Pilot item summary: ", pilot_item_summary_path),
  paste0("- Manual review template: ", manual_review_path),
  paste0("- Pilot construct dataset: ", pilot_rds_path),
  paste0("- Pilot construct summary: ", pilot_construct_summary_path),
  paste0("- Pilot internal consistency: ", pilot_internal_consistency_path),
  paste0("- Pilot construct correlations: ", pilot_construct_correlations_path),
  "",
  "No Git action was performed."
)

writeLines(method_note, method_note_path)

# ------------------------------------------------------------
# 15. Optional Word report
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
  
  doc <- officer::body_add_par(doc, "Purpose", style = "heading 2")
  
  doc <- officer::body_add_par(
    doc,
    paste(
      "This script maps candidate Add Health variables to Theory of Planned Behavior constructs.",
      "It does not estimate regression or mediation models.",
      "The final mediation specification requires manual confirmation of TPB mediators and the dependent variable."
    ),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Sample audit", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(sample_audit))
  )
  
  doc <- officer::body_add_par(doc, "Candidate coverage audit", style = "heading 2")
  
  coverage_report <- coverage_audit |>
    select(
      variable,
      tpb_domain,
      subdomain,
      review_priority,
      present_in_raw,
      n_valid_declared_range,
      pct_valid_declared_range,
      transformation,
      candidate_status
    ) |>
    arrange(
      factor(review_priority, levels = c("High", "Medium", "Low")),
      tpb_domain,
      variable
    )
  
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(coverage_report))
  )
  
  doc <- officer::body_add_par(doc, "Pilot construct summary", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(pilot_construct_summary))
  )
  
  doc <- officer::body_add_par(doc, "Pilot internal consistency", style = "heading 2")
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(pilot_internal_consistency))
  )
  
  doc <- officer::body_add_par(doc, "Pilot construct correlations", style = "heading 2")
  
  if (nrow(pilot_correlations) > 0) {
    doc <- flextable::body_add_flextable(
      doc,
      flextable::autofit(flextable::flextable(pilot_correlations))
    )
  } else {
    doc <- officer::body_add_par(
      doc,
      "No construct-level correlations were computed.",
      style = "Normal"
    )
  }
  
  print(doc, target = docx_path)
}

# ------------------------------------------------------------
# 16. Console output
# ------------------------------------------------------------

end_time <- Sys.time()

log_line("Saved TPB candidate dictionary: ", dictionary_path)
log_line("Saved TPB coverage audit: ", coverage_path)
log_line("Saved pilot item summary: ", pilot_item_summary_path)
log_line("Saved manual review template: ", manual_review_path)
log_line("Saved pilot dataset: ", pilot_rds_path)
log_line("Saved pilot construct summary: ", pilot_construct_summary_path)
log_line("Saved pilot internal consistency: ", pilot_internal_consistency_path)
log_line("Saved pilot construct correlations: ", pilot_construct_correlations_path)
log_line("Saved methodological note: ", method_note_path)

if (has_docx) {
  log_line("Saved Word report: ", docx_path)
} else {
  log_line("Word report not created because officer/flextable were unavailable.")
}

log_line("Completed ", script_id, " in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
log_line("No Git action was performed.")

cat("\n============================================================\n")
cat("Script 19d completed: TPB Construct Candidate Mapping\n")
cat("============================================================\n\n")

cat("Sample audit:\n")
print(sample_audit)

cat("\nTPB candidate coverage summary:\n")
print(
  coverage_audit |>
    count(tpb_domain, review_priority, present_in_raw, name = "n_variables") |>
    arrange(tpb_domain, review_priority, desc(present_in_raw)),
  n = Inf
)

cat("\nPilot construct summary:\n")
print(pilot_construct_summary, n = Inf)

cat("\nPilot internal consistency:\n")
print(pilot_internal_consistency, n = Inf)

cat("\nPilot construct correlations:\n")
tibble::as_tibble(pilot_correlations) |>
  print(n = Inf)

cat("\nManual review rows by TPB domain:\n")
print(
  manual_review_template |>
    count(tpb_domain, review_priority, candidate_status, name = "n_rows") |>
    arrange(tpb_domain, review_priority),
  n = Inf
)

cat("\nMain outputs:\n")
print(tibble(
  output = c(
    "TPB candidate dictionary",
    "TPB coverage audit",
    "Pilot item summary",
    "Manual review template",
    "Pilot construct dataset",
    "Pilot construct summary",
    "Pilot internal consistency",
    "Pilot construct correlations",
    "Methodological note",
    "Word report",
    "Run log"
  ),
  path = c(
    dictionary_path,
    coverage_path,
    pilot_item_summary_path,
    manual_review_path,
    pilot_rds_path,
    pilot_construct_summary_path,
    pilot_internal_consistency_path,
    pilot_construct_correlations_path,
    method_note_path,
    ifelse(has_docx, docx_path, NA_character_),
    log_path
  ),
  exists = file.exists(c(
    dictionary_path,
    coverage_path,
    pilot_item_summary_path,
    manual_review_path,
    pilot_rds_path,
    pilot_construct_summary_path,
    pilot_internal_consistency_path,
    pilot_construct_correlations_path,
    method_note_path,
    ifelse(has_docx, docx_path, ""),
    log_path
  ))
))