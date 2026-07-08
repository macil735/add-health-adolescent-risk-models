# add-health-adolescent-risk-models

Public, ethical and reproducible R-based analysis of adolescent health risk behaviour using Add Health public-use data.

## Overview

This repository develops a public and reproducible R-based application inspired by the econometric architecture of a doctoral thesis on adolescent pregnancy, HIV infection and adolescent health risk behaviour.

The original doctoral dataset cannot be published because it contains sensitive information on adolescents. This project therefore does not publish, reconstruct or approximate the original confidential dataset. Instead, it transfers the methodological logic of the thesis to a recognised public longitudinal data source: the National Longitudinal Study of Adolescent to Adult Health, known as Add Health.

This repository is not an official Add Health product. It is an independent academic replication and methodological transfer exercise based on public-use documentation and locally obtained public-use data.

## Scientific objective

The main scientific objective is to study adolescent and young adult health risk outcomes using public-use Add Health data and reproducible econometric methods.

The project focuses on protective and risk-related psychosocial, family, school and behavioural mechanisms during adolescence.

## Methodological objective

The methodological objective is to reproduce the analytical architecture of the original doctoral research in a public and ethically defensible setting.

The emphasis is on methods, construct mapping, reproducible data preparation, transparent modelling decisions, weighted descriptive analysis, regression modelling, model diagnostics and institutional reporting. The project does not attempt to reproduce confidential empirical results from the original thesis.

## Data source

The repository is designed for use with public-use Add Health data and public documentation.

Add Health is a longitudinal study of adolescents in the United States who were in grades 7 to 12 during the 1994-1995 school year and have been followed across multiple waves.

Public-use Add Health data must be obtained directly from official sources. This repository does not store, redistribute or expose individual-level Add Health microdata.

Official resources:

- Add Health website: https://addhealth.cpc.unc.edu/
- ICPSR Add Health series: https://www.icpsr.umich.edu/web/NAHDAP/series/1006
- Add Health acknowledgement guidance: https://addhealth.cpc.unc.edu/data/ancillary-studies/acknowledgement/

## Ethical position

This repository follows four principles:

1. no publication of the original doctoral dataset;
2. no upload of individual-level Add Health microdata to GitHub;
3. no attempt to identify individuals, schools, friends, siblings, partners or communities;
4. publication only of code, metadata templates, aggregate outputs and reproducible documentation.

All raw and processed microdata must remain local and must not be committed to the repository.

## Sample alignment

The doctoral thesis focused on students from 10th to 12th grade, approximately aged 15 to 19.

Add Health Wave I includes students from grades 7 to 12. For thesis-aligned analyses, the project prioritises grade-based and age-based restrictions where appropriate.

The project currently includes two main empirical tracks:

1. protective behaviour and risk modelling among analytically eligible adolescents;
2. exploratory Theory of Planned Behavior compatible mechanism analysis among adolescents aged 15 to 19 who had not yet had sexual intercourse.

The second track is explicitly exploratory. It does not estimate a formal Theory of Planned Behavior mediation model because no direct intention-to-delay outcome and no confirmed perceived behavioural control or self-efficacy construct were available in the public-use operationalisation used here.

## Current analytical status

The repository has moved beyond initial development. The following major phases have been completed and committed:

- project setup, data availability and variable mapping;
- confirmed variable dictionary and import planning;
- psychosocial construct recovery and covariate audit;
- protection and risk item direction review;
- final reviewed protection index construction;
- selected outcome models and rare outcome diagnostics;
- sexual protection modelling and final interpretation reports;
- never-sex TPB-compatible mechanism operationalisation and regression phase.

The latest completed phase estimates exploratory TPB-compatible regression models linking family connectedness and friend support to delay-supportive attitudes and separate normative items.

## Repository structure

```text
R/                 R scripts for the reproducible analytical pipeline
data/              Local-only data structure; raw and processed microdata are ignored
docs/              Technical reports, documentation and selected Word outputs
outputs/audits/    Audit outputs and validation tables
outputs/tables/    Aggregate tables suitable for public reporting
outputs/reports/   Public-facing or technical reports
outputs/analysis/  Local analytical objects; not intended for GitHub
outputs/logs/      Local run logs; not intended for GitHub
```

## Reproducibility

The scripts are organised as a numbered pipeline. Users should run scripts in sequence after placing authorised Add Health public-use data files in the local `data/raw/` folder.

Raw data are intentionally excluded from version control. Some scripts will not run unless the user has obtained the required public-use files from official sources and placed them locally using the expected file names.

## Main outputs

The repository includes code, aggregate tables, audit summaries and selected reports. It excludes individual-level microdata and local analytical binary files.

Key public outputs include:

- construct validation tables;
- weighted descriptive and regression summaries;
- model diagnostics;
- final interpretation reports;
- methodological notes;
- reproducibility-oriented documentation.

## Citation and acknowledgement

If this repository is used, cite the software repository using the information in `CITATION.cff`.

Any report, manuscript or presentation based on Add Health data should also follow the current official Add Health acknowledgement requirements. Users are responsible for verifying and applying the current wording from the official Add Health website.

## License

The code in this repository is released under the MIT License.

The license applies only to the repository code and documentation produced here. It does not apply to Add Health data, Add Health documentation, the original doctoral dataset or any third-party materials.

## Data-use disclaimer

This repository does not grant access to Add Health data. Users must obtain data from official sources and comply with all applicable data-use terms, institutional requirements and ethical obligations.

## Project status

Stable research pipeline under active development. Recent commits completed the sexual protection modelling phase and the never-sex TPB-compatible mechanism phase.
