# ============================================================
# Script 20
# Update Repository Metadata Files
# Project: Add Health Adolescent Risk Models
# ============================================================
#
# Purpose:
#   Update README.md, CITATION.cff, LICENSE and .gitignore
#   after completion of the main protection modelling and
#   never-sex TPB-compatible mechanism phases.
#
# This script:
#   1. backs up existing metadata files;
#   2. writes updated repository metadata;
#   3. reinforces the ethical/data-use boundary;
#   4. avoids committing raw Add Health microdata;
#   5. produces an audit checklist.
#
# No Git action is performed.
#
# ============================================================

rm(list = ls())

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

script_id <- "20"
script_title <- "Update Repository Metadata Files"
start_time <- Sys.time()

required_pkgs <- c("readr", "tibble")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_pkgs, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(tibble)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

dir.create("docs", showWarnings = FALSE)
dir.create("docs/metadata_backups", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/audits", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)

backup_dir <- file.path(
  "docs",
  "metadata_backups",
  paste0("script20_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)

dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

log_path <- "outputs/logs/script20_update_repository_metadata_files.log"
cat("", file = log_path)

log_line <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(txt, "\n", file = log_path, append = TRUE)
  message(txt)
}

backup_file <- function(path) {
  if (file.exists(path)) {
    file.copy(path, file.path(backup_dir, basename(path)), overwrite = TRUE)
    return(TRUE)
  }
  FALSE
}

metadata_files <- c("README.md", "CITATION.cff", "LICENSE", ".gitignore")
backup_status <- vapply(metadata_files, backup_file, logical(1))

log_line("Started ", script_id, ": ", script_title)
log_line("Project root: ", project_root)
log_line("Backup directory: ", backup_dir)

author_name <- "macil735"
repo_name <- "add-health-adolescent-risk-models"
repo_url <- "https://github.com/macil735/add-health-adolescent-risk-models"
release_version <- "0.2.0"
release_date <- as.character(Sys.Date())
copyright_year <- format(Sys.Date(), "%Y")

# ------------------------------------------------------------
# README.md
# ------------------------------------------------------------

readme_text <- paste(c(
  "# add-health-adolescent-risk-models",
  "",
  "Public, ethical and reproducible R-based analysis of adolescent health risk behaviour using Add Health public-use data.",
  "",
  "## Overview",
  "",
  "This repository develops a public and reproducible R-based application inspired by the econometric architecture of a doctoral thesis on adolescent pregnancy, HIV infection and adolescent health risk behaviour.",
  "",
  "The original doctoral dataset cannot be published because it contains sensitive information on adolescents. This project therefore does not publish, reconstruct or approximate the original confidential dataset. Instead, it transfers the methodological logic of the thesis to a recognised public longitudinal data source: the National Longitudinal Study of Adolescent to Adult Health, known as Add Health.",
  "",
  "This repository is not an official Add Health product. It is an independent academic replication and methodological transfer exercise based on public-use documentation and locally obtained public-use data.",
  "",
  "## Scientific objective",
  "",
  "The main scientific objective is to study adolescent and young adult health risk outcomes using public-use Add Health data and reproducible econometric methods.",
  "",
  "The project focuses on protective and risk-related psychosocial, family, school and behavioural mechanisms during adolescence.",
  "",
  "## Methodological objective",
  "",
  "The methodological objective is to reproduce the analytical architecture of the original doctoral research in a public and ethically defensible setting.",
  "",
  "The emphasis is on methods, construct mapping, reproducible data preparation, transparent modelling decisions, weighted descriptive analysis, regression modelling, model diagnostics and institutional reporting. The project does not attempt to reproduce confidential empirical results from the original thesis.",
  "",
  "## Data source",
  "",
  "The repository is designed for use with public-use Add Health data and public documentation.",
  "",
  "Add Health is a longitudinal study of adolescents in the United States who were in grades 7 to 12 during the 1994-1995 school year and have been followed across multiple waves.",
  "",
  "Public-use Add Health data must be obtained directly from official sources. This repository does not store, redistribute or expose individual-level Add Health microdata.",
  "",
  "Official resources:",
  "",
  "- Add Health website: https://addhealth.cpc.unc.edu/",
  "- ICPSR Add Health series: https://www.icpsr.umich.edu/web/NAHDAP/series/1006",
  "- Add Health acknowledgement guidance: https://addhealth.cpc.unc.edu/data/ancillary-studies/acknowledgement/",
  "",
  "## Ethical position",
  "",
  "This repository follows four principles:",
  "",
  "1. no publication of the original doctoral dataset;",
  "2. no upload of individual-level Add Health microdata to GitHub;",
  "3. no attempt to identify individuals, schools, friends, siblings, partners or communities;",
  "4. publication only of code, metadata templates, aggregate outputs and reproducible documentation.",
  "",
  "All raw and processed microdata must remain local and must not be committed to the repository.",
  "",
  "## Sample alignment",
  "",
  "The doctoral thesis focused on students from 10th to 12th grade, approximately aged 15 to 19.",
  "",
  "Add Health Wave I includes students from grades 7 to 12. For thesis-aligned analyses, the project prioritises grade-based and age-based restrictions where appropriate.",
  "",
  "The project currently includes two main empirical tracks:",
  "",
  "1. protective behaviour and risk modelling among analytically eligible adolescents;",
  "2. exploratory Theory of Planned Behavior compatible mechanism analysis among adolescents aged 15 to 19 who had not yet had sexual intercourse.",
  "",
  "The second track is explicitly exploratory. It does not estimate a formal Theory of Planned Behavior mediation model because no direct intention-to-delay outcome and no confirmed perceived behavioural control or self-efficacy construct were available in the public-use operationalisation used here.",
  "",
  "## Current analytical status",
  "",
  "The repository has moved beyond initial development. The following major phases have been completed and committed:",
  "",
  "- project setup, data availability and variable mapping;",
  "- confirmed variable dictionary and import planning;",
  "- psychosocial construct recovery and covariate audit;",
  "- protection and risk item direction review;",
  "- final reviewed protection index construction;",
  "- selected outcome models and rare outcome diagnostics;",
  "- sexual protection modelling and final interpretation reports;",
  "- never-sex TPB-compatible mechanism operationalisation and regression phase.",
  "",
  "The latest completed phase estimates exploratory TPB-compatible regression models linking family connectedness and friend support to delay-supportive attitudes and separate normative items.",
  "",
  "## Repository structure",
  "",
  "```text",
  "R/                 R scripts for the reproducible analytical pipeline",
  "data/              Local-only data structure; raw and processed microdata are ignored",
  "docs/              Technical reports, documentation and selected Word outputs",
  "outputs/audits/    Audit outputs and validation tables",
  "outputs/tables/    Aggregate tables suitable for public reporting",
  "outputs/reports/   Public-facing or technical reports",
  "outputs/analysis/  Local analytical objects; not intended for GitHub",
  "outputs/logs/      Local run logs; not intended for GitHub",
  "```",
  "",
  "## Reproducibility",
  "",
  "The scripts are organised as a numbered pipeline. Users should run scripts in sequence after placing authorised Add Health public-use data files in the local `data/raw/` folder.",
  "",
  "Raw data are intentionally excluded from version control. Some scripts will not run unless the user has obtained the required public-use files from official sources and placed them locally using the expected file names.",
  "",
  "## Main outputs",
  "",
  "The repository includes code, aggregate tables, audit summaries and selected reports. It excludes individual-level microdata and local analytical binary files.",
  "",
  "Key public outputs include:",
  "",
  "- construct validation tables;",
  "- weighted descriptive and regression summaries;",
  "- model diagnostics;",
  "- final interpretation reports;",
  "- methodological notes;",
  "- reproducibility-oriented documentation.",
  "",
  "## Citation and acknowledgement",
  "",
  "If this repository is used, cite the software repository using the information in `CITATION.cff`.",
  "",
  "Any report, manuscript or presentation based on Add Health data should also follow the current official Add Health acknowledgement requirements. Users are responsible for verifying and applying the current wording from the official Add Health website.",
  "",
  "## License",
  "",
  "The code in this repository is released under the MIT License.",
  "",
  "The license applies only to the repository code and documentation produced here. It does not apply to Add Health data, Add Health documentation, the original doctoral dataset or any third-party materials.",
  "",
  "## Data-use disclaimer",
  "",
  "This repository does not grant access to Add Health data. Users must obtain data from official sources and comply with all applicable data-use terms, institutional requirements and ethical obligations.",
  "",
  "## Project status",
  "",
  "Stable research pipeline under active development. Recent commits completed the sexual protection modelling phase and the never-sex TPB-compatible mechanism phase.",
  ""
), collapse = "\n")

cat(readme_text, file = "README.md")

# ------------------------------------------------------------
# CITATION.cff
# ------------------------------------------------------------

citation_text <- paste(c(
  "cff-version: 1.2.0",
  "message: \"If you use this repository, please cite it as below.\"",
  "type: software",
  "authors:",
  paste0("  - name: \"", author_name, "\""),
  paste0("title: \"", repo_name, "\""),
  paste0("version: \"", release_version, "\""),
  paste0("date-released: \"", release_date, "\""),
  paste0("repository-code: \"", repo_url, "\""),
  "license: \"MIT\"",
  "abstract: >-",
  "  Public, ethical and reproducible R-based application inspired by the",
  "  econometric architecture of doctoral research on adolescent pregnancy,",
  "  HIV infection and adolescent health risk behaviour. The repository",
  "  transfers the methodological logic to Add Health public-use data without",
  "  publishing, reconstructing or approximating confidential adolescent microdata.",
  "keywords:",
  "  - Add Health",
  "  - adolescent health",
  "  - risk behaviour",
  "  - protective behaviour",
  "  - public-use data",
  "  - reproducible research",
  "  - R",
  "  - econometrics",
  "  - Theory of Planned Behavior",
  "  - research ethics"
), collapse = "\n")

cat(citation_text, file = "CITATION.cff")

# ------------------------------------------------------------
# LICENSE
# ------------------------------------------------------------

license_text <- paste(c(
  "MIT License",
  "",
  paste0("Copyright (c) ", copyright_year, " ", author_name),
  "",
  "Permission is hereby granted, free of charge, to any person obtaining a copy",
  "of this software and associated documentation files (the \"Software\"), to deal",
  "in the Software without restriction, including without limitation the rights",
  "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell",
  "copies of the Software, and to permit persons to whom the Software is",
  "furnished to do so, subject to the following conditions:",
  "",
  "The above copyright notice and this permission notice shall be included in all",
  "copies or substantial portions of the Software.",
  "",
  "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR",
  "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,",
  "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE",
  "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER",
  "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
  "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE",
  "SOFTWARE.",
  "",
  "Data-use note:",
  "",
  "This license applies only to the code and documentation produced in this",
  "repository. It does not apply to Add Health data, Add Health documentation,",
  "the original doctoral dataset, or any third-party data or documentation.",
  "Users must obtain Add Health data directly from official sources and comply",
  "with all applicable data-use requirements."
), collapse = "\n")

cat(license_text, file = "LICENSE")

# ------------------------------------------------------------
# .gitignore
# ------------------------------------------------------------

gitignore_text <- paste(c(
  "# ============================================================",
  "# R / RStudio",
  "# ============================================================",
  ".Rhistory",
  ".RData",
  ".Ruserdata",
  ".Rproj.user/",
  "*.Rproj.user",
  "",
  "# ============================================================",
  "# Operating system files",
  "# ============================================================",
  ".DS_Store",
  "Thumbs.db",
  "desktop.ini",
  "",
  "# ============================================================",
  "# Temporary Office files",
  "# ============================================================",
  "~$*.docx",
  "~$*.xlsx",
  "~$*.pptx",
  "",
  "# ============================================================",
  "# Local credentials and environment files",
  "# ============================================================",
  ".Renviron",
  ".env",
  "*.key",
  "*.pem",
  "*.p12",
  "",
  "# ============================================================",
  "# Data files: never commit Add Health microdata",
  "# ============================================================",
  "data/raw/**",
  "data/processed/**",
  "data/interim/**",
  "data/derived/**",
  "",
  "!data/raw/.gitkeep",
  "!data/processed/.gitkeep",
  "!data/interim/.gitkeep",
  "!data/derived/.gitkeep",
  "",
  "data/**/*.rda",
  "data/**/*.RData",
  "data/**/*.rds",
  "data/**/*.csv",
  "data/**/*.tsv",
  "data/**/*.xlsx",
  "data/**/*.xls",
  "data/**/*.sav",
  "data/**/*.dta",
  "data/**/*.sas7bdat",
  "data/**/*.por",
  "",
  "# ============================================================",
  "# Local analytical objects and logs",
  "# ============================================================",
  "outputs/analysis/**",
  "outputs/logs/**",
  "",
  "!outputs/analysis/.gitkeep",
  "!outputs/logs/.gitkeep",
  "",
  "# ============================================================",
  "# Large or local-only binary artefacts",
  "# ============================================================",
  "*.zip",
  "*.7z",
  "*.tar",
  "*.tar.gz",
  "*.gz",
  "",
  "# ============================================================",
  "# Keep public aggregate outputs and reports trackable",
  "# ============================================================",
  "# Do not ignore outputs/tables, outputs/audits, outputs/reports or docs.",
  "# These folders contain aggregate, public-facing or reproducibility outputs.",
  "",
  "# ============================================================",
  "# Repository maintenance backups",
  "# ============================================================",
  "docs/metadata_backups/**",
  "",
  "# ============================================================",
  "# Local derived analytical outputs",
  "# ============================================================",
  "# These folders may contain individual-level derived objects, indices,",
  "# model objects, or local-only intermediate outputs.",
  "outputs/constructs/**",
  "outputs/indices/**",
  "outputs/models/**",
  ""
), collapse = "\n")

cat(gitignore_text, file = ".gitignore")

# ------------------------------------------------------------
# Metadata note
# ------------------------------------------------------------

metadata_note_path <- "docs/repository_metadata_update_note_script20.md"

metadata_note <- paste(c(
  "# Repository Metadata Update — Script 20",
  "",
  paste0("Run time: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
  "",
  "## Files updated",
  "",
  "- README.md",
  "- CITATION.cff",
  "- LICENSE",
  "- .gitignore",
  "",
  "## Main changes",
  "",
  "- Updated README from initial-development status to current stable pipeline status.",
  "- Added explicit separation between code license and Add Health data-use requirements.",
  "- Added repository-level data-use disclaimer.",
  "- Updated CITATION.cff for repository citation.",
  "- Replaced or reinforced .gitignore rules to prevent committing Add Health microdata.",
  "- Preserved aggregate outputs, reports and documentation as trackable files.",
  "",
  "## Git decision",
  "",
  "No Git action was performed by this script. Review files before staging and committing.",
  "",
  "## Backup",
  "",
  paste0("Previous metadata files were backed up to: `", backup_dir, "`")
), collapse = "\n")

cat(metadata_note, file = metadata_note_path)

# ------------------------------------------------------------
# Audit checklist
# ------------------------------------------------------------

updated_files <- c(
  "README.md",
  "CITATION.cff",
  "LICENSE",
  ".gitignore",
  metadata_note_path,
  log_path
)

checklist <- tibble::tibble(
  file = updated_files,
  exists = file.exists(updated_files),
  size_bytes = as.numeric(file.info(updated_files)$size),
  note = c(
    "Updated public repository description.",
    "Updated software citation metadata.",
    "Updated MIT code license with data-use note.",
    "Updated data protection and local-output ignore rules.",
    "Created metadata update note.",
    "Created Script 20 run log."
  )
)

checklist_path <- "outputs/audits/script20_repository_metadata_update_checklist.csv"
readr::write_csv(checklist, checklist_path)

end_time <- Sys.time()

log_line("Saved README.md")
log_line("Saved CITATION.cff")
log_line("Saved LICENSE")
log_line("Saved .gitignore")
log_line("Saved metadata note: ", metadata_note_path)
log_line("Saved checklist: ", checklist_path)
log_line("Completed ", script_id, " in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
log_line("No Git action was performed.")

cat("\n============================================================\n")
cat("Script 20 completed: Repository Metadata Update\n")
cat("============================================================\n\n")

cat("Metadata update checklist:\n")
print(checklist, n = Inf)

cat("\nBackup status:\n")
print(
  tibble::tibble(
    file = metadata_files,
    backed_up = as.logical(backup_status)
  ),
  n = Inf
)

cat("\nMain outputs:\n")
print(
  tibble::tibble(
    output = c(
      "README",
      "Citation file",
      "License",
      "Gitignore",
      "Metadata note",
      "Checklist",
      "Run log"
    ),
    path = c(
      "README.md",
      "CITATION.cff",
      "LICENSE",
      ".gitignore",
      metadata_note_path,
      checklist_path,
      log_path
    ),
    exists = file.exists(c(
      "README.md",
      "CITATION.cff",
      "LICENSE",
      ".gitignore",
      metadata_note_path,
      checklist_path,
      log_path
    ))
  ),
  n = Inf
)