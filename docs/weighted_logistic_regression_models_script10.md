# Weighted Logistic Regression Models

Script 10 estimates weighted logistic regression models for selected Add Health Wave I outcomes.

## Samples

- Main sample: students in grades 10 to 12.
- Strict sensitivity sample: students in grades 10 to 12 and ages 15 to 19.

## Weight

`GSWGT1` is used as the Wave I population-average sampling weight.

## Mandatory controls

- `a_age_wave1`
- `a_female`
- `a_grade_wave1`

## Model sequence

- M0: core controls
- M1: family context
- M2: school context
- M3: knowledge, attitudes and perceptions
- M4: peers and relationships
- M5: general risk behaviors
- M6: final parsimonious model

## Estimator

Models are estimated with `survey::svyglm()` using a quasibinomial logit specification.

## Interpretation

The estimated odds ratios are associational. They should not be interpreted as causal effects.

## Privacy protection

`AID` is used only internally and is excluded from all public outputs.

## Public outputs

The script exports model specifications, fit diagnostics, coefficients, odds ratios, skipped-model diagnostics and methodological notes.

## Next step

Script 11 should review, select and interpret final model results for reporting.
