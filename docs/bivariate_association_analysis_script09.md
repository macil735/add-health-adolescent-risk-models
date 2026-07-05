# Bivariate Association Analysis

Script 09 conducts weighted bivariate screening for Add Health Wave I.

## Input

`data/processed/add_health_wave01_analytical_weighted_local_only.rds`

This file is local-only and must not be committed to GitHub.

## Samples

- Main sample: students in grades 10 to 12.
- Strict sensitivity sample: students in grades 10 to 12 and ages 15 to 19.

## Weight

`GSWGT1` is used as the Wave I population-average sampling weight.

## Outcomes

Candidate outcomes are selected from Script 08 outcome readiness diagnostics.

## Predictors

Candidate predictors are selected from Script 08 model variable decisions.

## Tests

- Categorical predictors: weighted cross-tabulations and Rao-Scott chi-square tests.
- Continuous predictors: weighted mean differences and weighted t-tests.

## Multiple testing

Benjamini-Hochberg adjustment is applied within each sample and outcome.

## Interpretation

The results are exploratory bivariate associations. They do not imply causality and do not replace multivariable regression.

## Privacy protection

`AID` is used only internally and is excluded from all public outputs.

## Next step

Script 10 should estimate weighted logistic regression models for selected outcomes.
