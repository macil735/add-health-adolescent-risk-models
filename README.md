# add-health-adolescent-risk-models

This repository develops a public, ethical and reproducible R-based application inspired by the econometric architecture of a doctoral thesis on adolescent pregnancy, HIV infection and adolescent health risk behaviour.

The original thesis dataset cannot be published because it contains sensitive information on adolescents. Therefore, this project does not publish, reconstruct or approximate the original dataset. Instead, it transfers the methodological logic of the thesis to a recognised public longitudinal data source: the National Longitudinal Study of Adolescent to Adult Health, known as Add Health.

## Scientific objective

The main objective is to study adolescent and young adult health risk outcomes using public-use Add Health data and reproducible econometric methods.

## Methodological objective

The project aims to reproduce the analytical architecture of the original doctoral research in a public and ethically defensible setting. The emphasis is on methods, not on reproducing confidential empirical results from the original thesis.

## Sample alignment

The doctoral thesis focused on students from 10th to 12th grade, approximately aged 15 to 19.

Add Health Wave I includes students from grades 7 to 12. Therefore, the main analytical sample in this project will be restricted to students from grades 10 to 12 at Wave I.

Age 15 to 19 will be used as a complementary criterion, mainly for validation or sensitivity analysis.

## Data source

The project is designed for use with public-use Add Health data and public documentation.

The repository does not store or redistribute individual-level Add Health data. Users must obtain the data directly from official sources and must comply with all applicable terms of use.

## Ethical position

This repository follows four principles:

1. no publication of the original doctoral dataset;
2. no upload of individual-level Add Health microdata to GitHub;
3. no attempt to identify individuals, schools, friends, siblings, partners or communities;
4. publication only of code, metadata templates, aggregated outputs and reproducible documentation.

## Project structure

```text
R/
data/
outputs/
docs/
README.md
LICENSE
.gitignore
```

The `data/` folder is included only as a local working structure. Raw and processed data files must not be committed to GitHub.

## Initial script

The project begins with:

```text
R/01_data_availability_variable_mapping_audit.R
```

This script creates the initial project folders, defines the conceptual variable map, classifies expected data availability and exports the first audit tables.

## Status

Project under initial development.
