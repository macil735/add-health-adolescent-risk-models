# ============================================================
# Script 19b
# Sexual Delay Variable Discovery and Candidate Audit
# Project: Add Health Adolescent Risk Models
# ============================================================
#
# Purpose:
#   Discover candidate variables for a sexual delay construct
#   among adolescents aged 15–19 who have not yet had sexual
#   intercourse.
#
# Important:
#   This script does NOT construct the final sexual delay index.
#   It only creates an audit table and a manual review template.
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

script_id <- "19b"
script_title <- "Sexual Delay Variable Discovery and Candidate Audit"
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
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reports", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)

raw_candidate_path <- "outputs/audits/script19b_sexual_delay_raw_variable_candidates.csv"
codebook_hits_path <- "outputs/audits/script19b_sexual_delay_codebook_text_hits.csv"
manual_review_path <- "outputs/audits/script19b_sexual_delay_manual_review_TEMPLATE.csv"
sample_audit_path <- "outputs/audits/script19b_never_sex_sample_audit.csv"
summary_path <- "outputs/tables/script19b_sexual_delay_candidate_summary.csv"
docx_path <- "outputs/reports/script19b_sexual_delay_variable_discovery_candidate_audit.docx"
log_path <- "outputs/logs/script19b_run_log.txt"

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
    # Common coding: 1 = Yes, 2 = No
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

get_var_label <- function(x) {
  lab <- attr(x, "label")
  if (is.null(lab)) lab <- attr(x, "var_label")
  if (is.null(lab)) lab <- attr(x, "description")
  if (is.null(lab)) return(NA_character_)
  as.character(lab[1])
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
  
  if (is.character(x)) {
    vals <- unique(x[!is.na(x)])
    vals <- vals[seq_len(min(length(vals), max_levels))]
    return(paste(vals, collapse = " | "))
  }
  
  NA_character_
}

truncate_text <- function(x, n = 500) {
  x <- as.character(x)
  x <- ifelse(is.na(x), "", x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n), "..."), x)
}

# ------------------------------------------------------------
# 4. Search terms
# ------------------------------------------------------------

candidate_terms <- tibble::tribble(
  ~domain, ~term, ~pattern, ~weight,
  "core_delay", "abstinence", "\\babstinen|abstain", 6,
  "core_delay", "virginity", "\\bvirgin", 6,
  "core_delay", "wait_to_have_sex", "\\bwait|waiting|waited", 6,
  "core_delay", "delay_or_postpone", "\\bdelay|delaying|postpone|postponing", 6,
  "core_delay", "until_marriage", "until marriage|before marriage|after marriage|marry|marriage", 6,
  "intention", "intend_plan_expect", "intend|intention|plan|planning|expect|likely|likelihood", 5,
  "intention", "want_ready", "\\bwant|ready|choose|decision|decide", 4,
  "sexual_behavior", "sexual_intercourse", "sexual intercourse|intercourse|had sex|have sex|having sex|sexually active|vaginal sex|first sex|first intercourse", 4,
  "sexual_behavior", "sex_general", "\\bsex\\b|sexual", 2,
  "pressure", "sexual_pressure", "pressure|forced|coerce|coerced|partner pressure|peer pressure", 4,
  "peer_partner", "partner_relationship", "boyfriend|girlfriend|partner|relationship|dating|date", 3,
  "consequence", "pregnancy", "pregnan|teen pregnan|early pregnan", 3,
  "consequence", "contraception", "condom|birth control|contracep", 2,
  "consequence", "sti_hiv", "\\bsti\\b|\\bstd\\b|hiv|aids|disease", 2,
  "norms", "approval_norms", "approve|disapprove|wrong|right|belief|attitude|opinion|should", 3,
  "motivation", "reason", "reason|because|why", 3
)

combined_pattern <- paste(candidate_terms$pattern, collapse = "|")

score_text <- function(text) {
  text <- paste(text, collapse = " ")
  text <- tolower(text)
  
  if (is.na(text) || nchar(text) == 0) {
    return(0)
  }
  
  hits <- stringr::str_detect(
    text,
    stringr::regex(candidate_terms$pattern, ignore_case = TRUE)
  )
  
  sum(candidate_terms$weight[hits])
}

hit_terms <- function(text) {
  text <- paste(text, collapse = " ")
  text <- tolower(text)
  
  if (is.na(text) || nchar(text) == 0) {
    return(NA_character_)
  }
  
  hits <- stringr::str_detect(
    text,
    stringr::regex(candidate_terms$pattern, ignore_case = TRUE)
  )
  
  out <- candidate_terms$term[hits]
  
  if (length(out) == 0) {
    return(NA_character_)
  }
  
  paste(unique(out), collapse = "; ")
}

hit_domains <- function(text) {
  text <- paste(text, collapse = " ")
  text <- tolower(text)
  
  if (is.na(text) || nchar(text) == 0) {
    return(NA_character_)
  }
  
  hits <- stringr::str_detect(
    text,
    stringr::regex(candidate_terms$pattern, ignore_case = TRUE)
  )
  
  out <- candidate_terms$domain[hits]
  
  if (length(out) == 0) {
    return(NA_character_)
  }
  
  paste(unique(out), collapse = "; ")
}

# ------------------------------------------------------------
# 5. Load raw Wave I file
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
# 6. Reconstruct analytic sample: age 15–19 and never had sex
# ------------------------------------------------------------

id_aliases <- c(
  "AID",
  "aid",
  "respondent_id",
  "id",
  "caseid",
  "case_id"
)

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

weight_aliases <- c(
  "GSWGT1",
  "gswgt1",
  "weight",
  "wave1_weight",
  "sample_weight"
)

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
  stop(
    "Age could not be constructed.",
    call. = FALSE
  )
}

df_work <- df |>
  mutate(
    .age_19b = age_vector,
    .ever_sex_19b = recode_ever_sex(.data[[sexever_var]]),
    .never_sex_19b = .ever_sex_19b == 0,
    .age_15_19_19b = .age_19b >= 15 & .age_19b <= 19
  )

analysis_df <- df_work |>
  filter(.age_15_19_19b, .never_sex_19b)

if (nrow(analysis_df) == 0) {
  stop(
    "Filtered sample has zero observations.",
    call. = FALSE
  )
}

sample_audit <- tibble(
  script_id = script_id,
  selected_file = raw_path,
  selected_object = data_objects[1],
  full_n = nrow(df_work),
  age_15_19_n = sum(df_work$.age_15_19_19b, na.rm = TRUE),
  ever_sex_known_n = sum(!is.na(df_work$.ever_sex_19b)),
  age_15_19_ever_sex_known_n = sum(
    df_work$.age_15_19_19b & !is.na(df_work$.ever_sex_19b),
    na.rm = TRUE
  ),
  final_never_sex_age_15_19_n = nrow(analysis_df),
  age_variable = age_source,
  ever_sex_variable = sexever_var,
  id_variable = ifelse(is.na(id_var), NA_character_, id_var),
  weight_variable = ifelse(is.na(weight_var), NA_character_, weight_var),
  run_time = as.character(start_time)
)

write_csv(sample_audit, sample_audit_path)

log_line("Age source: ", age_source)
log_line("Ever-sex variable: ", sexever_var)
log_line("Final analytic sample n: ", nrow(analysis_df))

# ------------------------------------------------------------
# 7. Raw data variable metadata search
# ------------------------------------------------------------

summarise_raw_variable <- function(v) {
  x_full <- df[[v]]
  x_sample <- analysis_df[[v]]
  
  var_label <- get_var_label(x_full)
  levels_text <- get_levels_text(x_full)
  
  name_score <- score_text(v)
  label_score <- score_text(var_label)
  levels_score <- score_text(levels_text)
  
  combined_text <- paste(
    v,
    var_label,
    levels_text,
    sep = " | "
  )
  
  n_valid_sample <- sum(!is.na(x_sample))
  unique_valid_n <- length(unique(x_sample[!is.na(x_sample)]))
  
  num_sample <- suppressWarnings(to_numeric_survey(x_sample))
  
  tibble(
    variable = v,
    variable_key = std_key(v),
    variable_class = paste(class(x_full), collapse = "|"),
    variable_label = var_label,
    levels_or_value_labels = levels_text,
    n_valid_never_sex_sample = n_valid_sample,
    pct_valid_never_sex_sample = round(100 * n_valid_sample / nrow(analysis_df), 2),
    unique_valid_n_never_sex_sample = unique_valid_n,
    numeric_min_never_sex_sample = suppressWarnings(
      ifelse(all(is.na(num_sample)), NA_real_, min(num_sample, na.rm = TRUE))
    ),
    numeric_max_never_sex_sample = suppressWarnings(
      ifelse(all(is.na(num_sample)), NA_real_, max(num_sample, na.rm = TRUE))
    ),
    name_score = name_score,
    label_score = label_score,
    levels_score = levels_score,
    raw_metadata_score = name_score + label_score + pmin(levels_score, 5),
    matched_terms_metadata = hit_terms(combined_text),
    matched_domains_metadata = hit_domains(combined_text),
    searchable_metadata = truncate_text(combined_text, 1000)
  )
}

metadata_tbl <- purrr::map_dfr(names(df), summarise_raw_variable)

log_line("Raw metadata variables scanned: ", nrow(metadata_tbl))

# ------------------------------------------------------------
# 8. Search existing text/CSV/Markdown outputs and codebook-like files
# ------------------------------------------------------------

search_dirs <- c("data", "docs", "outputs")

search_files <- unlist(lapply(
  search_dirs[dir.exists(search_dirs)],
  function(d) {
    list.files(
      d,
      pattern = "\\.(txt|csv|md|qmd|log)$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  }
))

search_files <- unique(search_files)

if (length(search_files) > 0) {
  file_sizes <- file.info(search_files)$size
  search_files <- search_files[!is.na(file_sizes) & file_sizes <= 25 * 1024^2]
}

extract_var_tokens <- function(text) {
  toks <- stringr::str_extract_all(
    text,
    "\\bH1[A-Z0-9]{2,8}\\b|\\bIMONTH\\b|\\bIDAY\\b|\\bIYEAR\\b|\\bGSWGT1\\b|\\bAID\\b"
  )[[1]]
  
  toks <- unique(toks)
  
  if (length(toks) == 0) {
    return(NA_character_)
  }
  
  paste(toks, collapse = ";")
}

scan_text_file <- function(path) {
  out_empty <- tibble(
    source_file = character(),
    line_number = integer(),
    matched_text = character(),
    matched_terms = character(),
    matched_domains = character(),
    extracted_variables = character()
  )
  
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )
  
  if (length(lines) == 0) {
    return(out_empty)
  }
  
  hit <- stringr::str_detect(
    tolower(lines),
    stringr::regex(combined_pattern, ignore_case = TRUE)
  )
  
  if (!any(hit, na.rm = TRUE)) {
    return(out_empty)
  }
  
  hit_lines <- lines[hit]
  hit_idx <- which(hit)
  
  tibble(
    source_file = normalizePath(path, winslash = "/", mustWork = FALSE),
    line_number = hit_idx,
    matched_text = truncate_text(hit_lines, 1000),
    matched_terms = purrr::map_chr(hit_lines, hit_terms),
    matched_domains = purrr::map_chr(hit_lines, hit_domains),
    extracted_variables = purrr::map_chr(hit_lines, extract_var_tokens)
  )
}

codebook_hits <- purrr::map_dfr(search_files, scan_text_file)

if (nrow(codebook_hits) == 0) {
  codebook_hits <- tibble(
    source_file = character(),
    line_number = integer(),
    matched_text = character(),
    matched_terms = character(),
    matched_domains = character(),
    extracted_variables = character()
  )
}

write_csv(codebook_hits, codebook_hits_path)

log_line("Codebook/text files scanned: ", length(search_files))
log_line("Codebook/text hit lines found: ", nrow(codebook_hits))

# ------------------------------------------------------------
# 9. Link text hits back to raw variables
# ------------------------------------------------------------

if (nrow(codebook_hits) > 0) {
  codebook_var_summary <- codebook_hits |>
    filter(!is.na(extracted_variables), extracted_variables != "") |>
    tidyr::separate_rows(extracted_variables, sep = ";") |>
    mutate(
      variable_key = std_key(extracted_variables)
    ) |>
    group_by(variable_key) |>
    summarise(
      codebook_hit_n = n(),
      codebook_matched_terms = paste(unique(na.omit(matched_terms)), collapse = "; "),
      codebook_matched_domains = paste(unique(na.omit(matched_domains)), collapse = "; "),
      codebook_evidence = paste(unique(na.omit(matched_text))[1:min(n(), 5)], collapse = " || "),
      .groups = "drop"
    )
} else {
  codebook_var_summary <- tibble(
    variable_key = character(),
    codebook_hit_n = integer(),
    codebook_matched_terms = character(),
    codebook_matched_domains = character(),
    codebook_evidence = character()
  )
}

raw_candidates <- metadata_tbl |>
  left_join(codebook_var_summary, by = "variable_key") |>
  mutate(
    codebook_hit_n = ifelse(is.na(codebook_hit_n), 0L, codebook_hit_n),
    codebook_score = pmin(codebook_hit_n * 2, 12),
    candidate_score = raw_metadata_score + codebook_score,
    combined_terms = paste(
      matched_terms_metadata,
      codebook_matched_terms,
      sep = "; "
    ),
    combined_domains = paste(
      matched_domains_metadata,
      codebook_matched_domains,
      sep = "; "
    ),
    priority = dplyr::case_when(
      candidate_score >= 12 ~ "High",
      candidate_score >= 6 ~ "Medium",
      candidate_score > 0 ~ "Low",
      TRUE ~ "Not selected"
    ),
    likely_role = dplyr::case_when(
      variable == sexever_var ~ "sample_filter_or_outcome_not_index_item",
      str_detect(tolower(combined_terms), "abstinence|virginity|wait_to_have_sex|delay_or_postpone|until_marriage") ~ "core_sexual_delay_candidate",
      str_detect(tolower(combined_domains), "intention|norms") ~ "attitude_or_intention_candidate",
      str_detect(tolower(combined_domains), "pressure|peer_partner") ~ "peer_partner_pressure_context",
      str_detect(tolower(combined_domains), "consequence") ~ "consequence_or_prevention_context",
      str_detect(tolower(combined_domains), "sexual_behavior") ~ "sexual_behavior_related_review_needed",
      TRUE ~ "review_needed"
    ),
    recommended_review = dplyr::case_when(
      variable == sexever_var ~ "Exclude from sexual delay index; already used to define never-sex sample.",
      priority == "High" ~ "Manual review required; likely relevant candidate.",
      priority == "Medium" ~ "Manual review required; possible candidate.",
      priority == "Low" ~ "Low-priority candidate; review only if high/medium candidates are insufficient.",
      TRUE ~ "Not selected."
    )
  ) |>
  filter(priority != "Not selected") |>
  arrange(desc(candidate_score), variable)

write_csv(raw_candidates, raw_candidate_path)

log_line("Candidate raw variables selected: ", nrow(raw_candidates))

# ------------------------------------------------------------
# 10. Manual review template
# ------------------------------------------------------------

manual_review_template <- raw_candidates |>
  filter(priority %in% c("High", "Medium")) |>
  transmute(
    script_id = script_id,
    variable,
    variable_label,
    levels_or_value_labels,
    n_valid_never_sex_sample,
    pct_valid_never_sex_sample,
    unique_valid_n_never_sex_sample,
    candidate_score,
    priority,
    likely_role,
    combined_terms,
    combined_domains,
    codebook_evidence = truncate_text(codebook_evidence, 800),
    reviewer_keep = "",
    reviewer_construct = "",
    reviewer_direction = "",
    reviewer_notes = ""
  )

write_csv(manual_review_template, manual_review_path)

# ------------------------------------------------------------
# 11. Summary table
# ------------------------------------------------------------

candidate_summary <- raw_candidates |>
  count(priority, likely_role, name = "n_variables") |>
  arrange(priority, desc(n_variables))

summary_overall <- tibble(
  metric = c(
    "raw_variables_scanned",
    "candidate_variables_selected",
    "high_priority_candidates",
    "medium_priority_candidates",
    "low_priority_candidates",
    "manual_review_rows",
    "codebook_text_hit_lines",
    "final_never_sex_age_15_19_n"
  ),
  value = c(
    nrow(metadata_tbl),
    nrow(raw_candidates),
    sum(raw_candidates$priority == "High"),
    sum(raw_candidates$priority == "Medium"),
    sum(raw_candidates$priority == "Low"),
    nrow(manual_review_template),
    nrow(codebook_hits),
    nrow(analysis_df)
  )
)

write_csv(
  bind_rows(
    summary_overall |> mutate(table = "overall") |> rename(category = metric, n = value),
    candidate_summary |> mutate(table = "priority_by_role") |> rename(category = likely_role, n = n_variables) |> select(table, category, n, priority)
  ),
  summary_path
)

# ------------------------------------------------------------
# 12. Optional Word report
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
      "This audit searches for candidate variables that may support a sexual delay construct.",
      "It does not construct a final index.",
      "Candidate variables require manual review before use in Script 19c."
    ),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "Sample audit", style = "heading 2")
  
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(sample_audit))
  )
  
  doc <- officer::body_add_par(doc, "Overall candidate summary", style = "heading 2")
  
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(summary_overall))
  )
  
  doc <- officer::body_add_par(doc, "Candidate variables by priority and role", style = "heading 2")
  
  if (nrow(candidate_summary) > 0) {
    doc <- flextable::body_add_flextable(
      doc,
      flextable::autofit(flextable::flextable(candidate_summary))
    )
  } else {
    doc <- officer::body_add_par(
      doc,
      "No candidate variables were selected.",
      style = "Normal"
    )
  }
  
  doc <- officer::body_add_par(doc, "Top candidate variables", style = "heading 2")
  
  top_candidates_report <- raw_candidates |>
    select(
      variable,
      candidate_score,
      priority,
      likely_role,
      n_valid_never_sex_sample,
      pct_valid_never_sex_sample,
      variable_label,
      levels_or_value_labels,
      recommended_review
    ) |>
    slice_head(n = 50) |>
    mutate(
      variable_label = truncate_text(variable_label, 120),
      levels_or_value_labels = truncate_text(levels_or_value_labels, 180),
      recommended_review = truncate_text(recommended_review, 120)
    )
  
  if (nrow(top_candidates_report) > 0) {
    doc <- flextable::body_add_flextable(
      doc,
      flextable::autofit(flextable::flextable(top_candidates_report))
    )
  } else {
    doc <- officer::body_add_par(
      doc,
      "No top candidates available.",
      style = "Normal"
    )
  }
  
  doc <- officer::body_add_par(doc, "Outputs", style = "heading 2")
  
  output_tbl <- tibble(
    output = c(
      "Raw variable candidate audit",
      "Codebook/text hit audit",
      "Manual review template",
      "Sample audit",
      "Summary table",
      "Run log"
    ),
    path = c(
      raw_candidate_path,
      codebook_hits_path,
      manual_review_path,
      sample_audit_path,
      summary_path,
      log_path
    )
  )
  
  doc <- flextable::body_add_flextable(
    doc,
    flextable::autofit(flextable::flextable(output_tbl))
  )
  
  print(doc, target = docx_path)
}

# ------------------------------------------------------------
# 13. Console output
# ------------------------------------------------------------

end_time <- Sys.time()

log_line("Saved raw candidate audit: ", raw_candidate_path)
log_line("Saved codebook/text hits: ", codebook_hits_path)
log_line("Saved manual review template: ", manual_review_path)
log_line("Saved sample audit: ", sample_audit_path)
log_line("Saved summary: ", summary_path)

if (has_docx) {
  log_line("Saved Word report: ", docx_path)
} else {
  log_line("Word report not created because officer/flextable were unavailable.")
}

log_line("Completed ", script_id, " in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
log_line("No Git action was performed.")

cat("\n============================================================\n")
cat("Script 19b completed: Sexual Delay Variable Discovery\n")
cat("============================================================\n\n")

cat("Sample audit:\n")
print(sample_audit)

cat("\nOverall summary:\n")
print(summary_overall)

cat("\nCandidate summary:\n")
print(candidate_summary)

cat("\nTop candidate variables:\n")
print(
  raw_candidates |>
    select(
      variable,
      candidate_score,
      priority,
      likely_role,
      n_valid_never_sex_sample,
      pct_valid_never_sex_sample,
      matched_terms_metadata,
      codebook_hit_n
    ) |>
    slice_head(n = 30),
  n = 30
)

cat("\nMain outputs:\n")
print(tibble(
  output = c(
    "Raw variable candidate audit",
    "Codebook/text hit audit",
    "Manual review template",
    "Sample audit",
    "Summary table",
    "Word report",
    "Run log"
  ),
  path = c(
    raw_candidate_path,
    codebook_hits_path,
    manual_review_path,
    sample_audit_path,
    summary_path,
    ifelse(has_docx, docx_path, NA_character_),
    log_path
  ),
  exists = file.exists(c(
    raw_candidate_path,
    codebook_hits_path,
    manual_review_path,
    sample_audit_path,
    summary_path,
    ifelse(has_docx, docx_path, ""),
    log_path
  ))
))