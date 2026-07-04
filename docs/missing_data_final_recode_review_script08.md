# Missing Data and Final Recode Review

Script 08 reviews missingness, recode readiness, outcome availability and construct usability before modeling.

## Input

`data/processed/add_health_wave01_analytical_weighted_local_only.rds`

This file is local-only and must not be committed to GitHub.

## Main sample

The main analytical sample is students in grades 10 to 12 at Wave I.

## Weight

`GSWGT1` is retained as the Wave I population-average sampling weight.

## Public outputs

The script exports aggregate diagnostics only:

- variable missingness summary;
- model variable decision template;
- recode readiness review;
- outcome distribution review;
- outcome readiness review;
- construct missingness review;
- sample missingness summary.

## Privacy protection

`AID` is used only internally and is excluded from public outputs.

## Next step

Script 09 should perform bivariate association analysis using only variables approved after this review.
