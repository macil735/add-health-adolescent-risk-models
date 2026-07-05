# Final Technical Report

## Add Health Wave I Public-Use Adolescent Risk Behavior Analysis

### Executive summary

This report summarizes the final public-use analytical outputs from the Add Health Wave I adolescent risk behavior project. The report is based only on aggregate public outputs produced by the previous scripts. No individual-level data are read or exported by this script.

- Final reporting models created: 18.
- Outcomes ready for cautious reporting: 16.
- Outcomes requiring manual review: 2.
- Main reportable coefficients: 14.
- Appendix or review coefficients: 1.
- Reporting cautions recorded: 3.
- Dominant selected model: M0: Core controls.

The report should be interpreted as an associational analysis. It does not provide causal evidence.

## 1. Purpose and scope

The purpose of this report is to document the final analytical workflow, selected models, reporting tables, robustness checks and limitations of the Add Health Wave I public-use adolescent risk behavior analysis.

The analysis is designed as a reproducible public-use replication framework inspired by the structure of the doctoral research project, without publishing original thesis microdata.

## 2. Data protection and reproducibility

The project separates public reproducible code and aggregate outputs from restricted or individual-level data. Raw data, processed individual-level files and AID-level outputs are not exported to the public repository.

## 3. Analytical workflow

The project proceeded through documentation review, variable mapping, public-use data import, analytical recoding, weighted descriptive statistics, missing-data review, bivariate screening, modeling framework construction, weighted logistic regression, model review, final model selection and reporting table preparation.

## 4. Modeling strategy

The weighted logistic regression models were estimated using the Wave I population-average sampling weight. The main analytical sample includes students in grades 10 to 12. A strict sensitivity sample additionally restricts the analysis to ages 15 to 19.

The final reporting stage prioritizes technically stable and interpretable models. Results are reported cautiously, with explicit separation between main reporting coefficients and coefficients requiring manual review.

## 5. Final model summary

| sample | outcome | outcome_label | selected_model | reporting_status | suitability_score | n_complete | n_outcome_yes | n_outcome_no | weighted_pct_yes | n_extreme_or | n_very_extreme_or | n_wide_ci | n_very_wide_ci | n_possible_conceptual_overlap |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Main sample: grades 10-12 | a_H1CO13_yesno | H1CO13 | M0: Core controls | Ready for cautious reporting | 100 | 481 | 363 | 118 | 77.16% | 0 | 0 | 0 | 0 | 0 |
| Main sample: grades 10-12 | a_H1CO16A_yesno | H1CO16A | M0: Core controls | Ready for cautious reporting | 100 | 1763 | 59 | 1704 | 3.09% | 0 | 0 | 0 | 0 | 0 |
| Main sample: grades 10-12 | a_H1CO16C_yesno | H1CO16C | M0: Core controls | Manual review required | 75 | 1764 | 25 | 1739 | 1.18% | 0 | 0 | 1 | 0 | 0 |
| Main sample: grades 10-12 | a_H1CO3_yesno | H1CO3 | M0: Core controls | Ready for cautious reporting | 100 | 1756 | 1200 | 556 | 68.57% | 0 | 0 | 0 | 0 | 0 |
| Main sample: grades 10-12 | a_H1CO6_yesno | H1CO6 | M0: Core controls | Ready for cautious reporting | 100 | 1749 | 1203 | 546 | 69.03% | 0 | 0 | 0 | 0 | 0 |
| Main sample: grades 10-12 | a_H1CO8_yesno | H1CO8 | M0: Core controls | Ready for cautious reporting | 100 | 402 | 247 | 155 | 63.39% | 0 | 0 | 0 | 0 | 0 |
| Main sample: grades 10-12 | a_H1FP7_yesno | H1FP7 | M0: Core controls | Ready for cautious reporting | 100 | 896 | 163 | 733 | 17.88% | 0 | 0 | 0 | 0 | 0 |
| Main sample: grades 10-12 | a_H1HS9_yesno | H1HS9 | M0: Core controls | Ready for cautious reporting | 100 | 3255 | 232 | 3023 | 6.70% | 0 | 0 | 0 | 0 | 0 |
| Main sample: grades 10-12 | a_sex_ever | sex ever | M0: Core controls | Ready for cautious reporting | 100 | 3226 | 1766 | 1460 | 54.59% | 0 | 0 | 0 | 0 | 0 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1CO13_yesno | H1CO13 | M0: Core controls | Ready for cautious reporting | 100 | 478 | 360 | 118 | 77.02% | 0 | 0 | 0 | 0 | 0 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1CO16A_yesno | H1CO16A | M0: Core controls | Ready for cautious reporting | 100 | 1753 | 58 | 1695 | 3.02% | 0 | 0 | 0 | 0 | 0 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1CO16C_yesno | H1CO16C | M0: Core controls | Manual review required | 85 | 1753 | 25 | 1728 | 1.19% | 0 | 0 | 0 | 0 | 0 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1CO3_yesno | H1CO3 | M0: Core controls | Ready for cautious reporting | 100 | 1746 | 1196 | 550 | 68.78% | 0 | 0 | 0 | 0 | 0 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1CO6_yesno | H1CO6 | M0: Core controls | Ready for cautious reporting | 100 | 1738 | 1197 | 541 | 69.14% | 0 | 0 | 0 | 0 | 0 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1CO8_yesno | H1CO8 | M0: Core controls | Ready for cautious reporting | 100 | 398 | 244 | 154 | 63.37% | 0 | 0 | 0 | 0 | 0 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1FP7_yesno | H1FP7 | M0: Core controls | Ready for cautious reporting | 100 | 892 | 161 | 731 | 17.71% | 0 | 0 | 0 | 0 | 0 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1HS9_yesno | H1HS9 | M0: Core controls | Ready for cautious reporting | 100 | 3237 | 229 | 3008 | 6.64% | 0 | 0 | 0 | 0 | 0 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_sex_ever | sex ever | M0: Core controls | Ready for cautious reporting | 100 | 3208 | 1755 | 1453 | 54.61% | 0 | 0 | 0 | 0 | 0 |

## 6. Main reportable coefficients

| sample | outcome | outcome_label | selected_model | predictor | odds_ratio | confidence_interval | p_value | interpretation_status |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Main sample: grades 10-12 | a_H1CO16A_yesno | H1CO16A | M0: Core controls | age | 1.58 | [1.06; 2.35] | 0.024 | Candidate for cautious interpretation |
| Main sample: grades 10-12 | a_H1CO6_yesno | H1CO6 | M0: Core controls | age | 0.84 | [0.72; 0.98] | 0.031 | Candidate for cautious interpretation |
| Main sample: grades 10-12 | a_H1CO8_yesno | H1CO8 | M0: Core controls | age | 1.36 | [1.00; 1.85] | 0.050 | Candidate for cautious interpretation |
| Main sample: grades 10-12 | a_H1FP7_yesno | H1FP7 | M0: Core controls | age | 1.95 | [1.45; 2.62] | <0.001 | Candidate for cautious interpretation |
| Main sample: grades 10-12 | a_H1HS9_yesno | H1HS9 | M0: Core controls | age | 1.61 | [1.30; 1.98] | <0.001 | Candidate for cautious interpretation |
| Main sample: grades 10-12 | a_sex_ever | sex ever | M0: Core controls | age | 1.33 | [1.19; 1.50] | <0.001 | Candidate for cautious interpretation |
| Main sample: grades 10-12 | a_sex_ever | sex ever | M0: Core controls | grade | 1.49 | [1.12; 1.99] | 0.007 | Candidate for cautious interpretation |
| Main sample: grades 10-12 | a_sex_ever | sex ever | M0: Core controls | grade | 1.32 | [1.06; 1.63] | 0.012 | Candidate for cautious interpretation |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1CO8_yesno | H1CO8 | M0: Core controls | age | 1.4 | [1.02; 1.94] | 0.041 | Candidate for cautious interpretation |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1FP7_yesno | H1FP7 | M0: Core controls | age | 1.85 | [1.36; 2.52] | <0.001 | Candidate for cautious interpretation |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_H1HS9_yesno | H1HS9 | M0: Core controls | age | 1.63 | [1.29; 2.05] | <0.001 | Candidate for cautious interpretation |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_sex_ever | sex ever | M0: Core controls | age | 1.35 | [1.20; 1.53] | <0.001 | Candidate for cautious interpretation |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_sex_ever | sex ever | M0: Core controls | grade | 1.48 | [1.10; 1.98] | 0.009 | Candidate for cautious interpretation |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | a_sex_ever | sex ever | M0: Core controls | grade | 1.31 | [1.05; 1.63] | 0.015 | Candidate for cautious interpretation |

## 7. Appendix or review coefficients

| sample | outcome | outcome_label | selected_model | predictor | odds_ratio | confidence_interval | p_value | review_status |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Main sample: grades 10-12 | a_H1CO16C_yesno | H1CO16C | M0: Core controls | grade | 1.34 | [0.30; 6.05] | 0.704 | Review: wide confidence interval |

## 8. Main sample narrative

### Outcome narrative 1

For outcome H1CO13, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. No coefficient from the selected model was prioritized for substantive interpretation. All results are associational and should not be interpreted as causal effects.

### Outcome narrative 2

For outcome H1CO16A, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with higher odds of H1CO16A (OR = 1.58, 95% CI [1.06; 2.35], p = 0.024). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.

### Outcome narrative 3

For outcome H1CO16C, the selected reporting model is M0: Core controls. The model decision is Alternative model with caution, with reporting status: Report only after manual review. Suitability score: 75/100. In the selected model, grade is associated with higher odds of H1CO16C (OR = 1.34, 95% CI [0.30; 6.05], p = 0.704). Interpretation status: Review: wide confidence interval. All results are associational and should not be interpreted as causal effects.

### Outcome narrative 4

For outcome H1CO3, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. No coefficient from the selected model was prioritized for substantive interpretation. All results are associational and should not be interpreted as causal effects.

### Outcome narrative 5

For outcome H1CO6, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with lower odds of H1CO6 (OR = 0.84, 95% CI [0.72; 0.98], p = 0.031). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.

### Outcome narrative 6

For outcome H1CO8, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with higher odds of H1CO8 (OR = 1.36, 95% CI [1.00; 1.85], p = 0.050). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.

### Outcome narrative 7

For outcome H1FP7, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with higher odds of H1FP7 (OR = 1.95, 95% CI [1.45; 2.62], p = <0.001). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.

### Outcome narrative 8

For outcome H1HS9, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with higher odds of H1HS9 (OR = 1.61, 95% CI [1.30; 1.98], p = <0.001). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.

### Outcome narrative 9

For outcome sex ever, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with higher odds of sex ever (OR = 1.33, 95% CI [1.19; 1.50], p = <0.001). Interpretation status: Candidate for cautious interpretation. In the selected model, grade is associated with higher odds of sex ever (OR = 1.49, 95% CI [1.12; 1.99], p = 0.007). Interpretation status: Candidate for cautious interpretation. In the selected model, grade is associated with higher odds of sex ever (OR = 1.32, 95% CI [1.06; 1.63], p = 0.012). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.


## 9. Strict sensitivity sample narrative

### Sensitivity narrative 1

For outcome H1CO13, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. No coefficient from the selected model was prioritized for substantive interpretation. All results are associational and should not be interpreted as causal effects.

### Sensitivity narrative 2

For outcome H1CO16A, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. No coefficient from the selected model was prioritized for substantive interpretation. All results are associational and should not be interpreted as causal effects.

### Sensitivity narrative 3

For outcome H1CO16C, the selected reporting model is M0: Core controls. The model decision is Alternative model with caution, with reporting status: Report only after manual review. Suitability score: 85/100. No coefficient from the selected model was prioritized for substantive interpretation. All results are associational and should not be interpreted as causal effects.

### Sensitivity narrative 4

For outcome H1CO3, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. No coefficient from the selected model was prioritized for substantive interpretation. All results are associational and should not be interpreted as causal effects.

### Sensitivity narrative 5

For outcome H1CO6, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. No coefficient from the selected model was prioritized for substantive interpretation. All results are associational and should not be interpreted as causal effects.

### Sensitivity narrative 6

For outcome H1CO8, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with higher odds of H1CO8 (OR = 1.40, 95% CI [1.02; 1.94], p = 0.041). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.

### Sensitivity narrative 7

For outcome H1FP7, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with higher odds of H1FP7 (OR = 1.85, 95% CI [1.36; 2.52], p = <0.001). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.

### Sensitivity narrative 8

For outcome H1HS9, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with higher odds of H1HS9 (OR = 1.63, 95% CI [1.29; 2.05], p = <0.001). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.

### Sensitivity narrative 9

For outcome sex ever, the selected reporting model is M0: Core controls. The model decision is Alternative stable model, with reporting status: Ready for cautious reporting. Suitability score: 100/100. In the selected model, age is associated with higher odds of sex ever (OR = 1.35, 95% CI [1.20; 1.53], p = <0.001). Interpretation status: Candidate for cautious interpretation. In the selected model, grade is associated with higher odds of sex ever (OR = 1.48, 95% CI [1.10; 1.98], p = 0.009). Interpretation status: Candidate for cautious interpretation. In the selected model, grade is associated with higher odds of sex ever (OR = 1.31, 95% CI [1.05; 1.63], p = 0.015). Interpretation status: Candidate for cautious interpretation. All results are associational and should not be interpreted as causal effects.


## 10. Robustness summary

| outcome | outcome_label | model_stage_label | n_coefficients_compared | n_same_direction | share_same_direction | n_robust_significant_same_direction | n_partially_robust_same_direction | n_direction_changes_or_unstable | robustness_summary | robustness_reporting_note |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| a_H1CO13_yesno | H1CO13 | M0: Core controls | 3 | 3 | 100 | 0 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO13_yesno | H1CO13 | M1: Family context | 7 | 7 | 100 | 1 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO13_yesno | H1CO13 | M3: Knowledge and attitudes | 25 | 25 | 100 | 7 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO13_yesno | H1CO13 | M4: Peers and relationships | 7 | 7 | 100 | 1 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO13_yesno | H1CO13 | M6: Final parsimonious model | 37 | 37 | 100 | 6 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO16A_yesno | H1CO16A | M0: Core controls | 3 | 2 | 66.67 | 0 | 1 | 1 | moderate_directional_robustness | Moderate directional consistency between main and strict samples. |
| a_H1CO16A_yesno | H1CO16A | M1: Family context | 8 | 8 | 100 | 5 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO16A_yesno | H1CO16A | M3: Knowledge and attitudes | 17 | 16 | 94.12 | 8 | 1 | 1 | moderate_directional_robustness | Moderate directional consistency between main and strict samples. |
| a_H1CO16A_yesno | H1CO16A | M6: Final parsimonious model | 30 | 30 | 100 | 14 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO16C_yesno | H1CO16C | M0: Core controls | 3 | 3 | 100 | 0 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO16C_yesno | H1CO16C | M1: Family context | 4 | 4 | 100 | 0 | 1 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO16C_yesno | H1CO16C | M3: Knowledge and attitudes | 17 | 17 | 100 | 4 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO16C_yesno | H1CO16C | M6: Final parsimonious model | 30 | 30 | 100 | 3 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO3_yesno | H1CO3 | M0: Core controls | 3 | 3 | 100 | 0 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO3_yesno | H1CO3 | M1: Family context | 21 | 21 | 100 | 6 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO3_yesno | H1CO3 | M2: School context | 12 | 12 | 100 | 0 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO3_yesno | H1CO3 | M3: Knowledge and attitudes | 25 | 24 | 96 | 9 | 1 | 1 | moderate_directional_robustness | Moderate directional consistency between main and strict samples. |
| a_H1CO3_yesno | H1CO3 | M4: Peers and relationships | 7 | 7 | 100 | 2 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO3_yesno | H1CO3 | M6: Final parsimonious model | 55 | 53 | 96.36 | 10 | 0 | 2 | moderate_directional_robustness | Moderate directional consistency between main and strict samples. |
| a_H1CO6_yesno | H1CO6 | M0: Core controls | 3 | 3 | 100 | 0 | 1 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO6_yesno | H1CO6 | M1: Family context | 25 | 25 | 100 | 9 | 1 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO6_yesno | H1CO6 | M2: School context | 8 | 7 | 87.5 | 1 | 0 | 1 | moderate_directional_robustness | Moderate directional consistency between main and strict samples. |
| a_H1CO6_yesno | H1CO6 | M3: Knowledge and attitudes | 24 | 24 | 100 | 4 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO6_yesno | H1CO6 | M6: Final parsimonious model | 55 | 55 | 100 | 7 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO8_yesno | H1CO8 | M0: Core controls | 3 | 3 | 100 | 1 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO8_yesno | H1CO8 | M1: Family context | 14 | 14 | 100 | 2 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1CO8_yesno | H1CO8 | M3: Knowledge and attitudes | 23 | 22 | 95.65 | 1 | 1 | 1 | moderate_directional_robustness | Moderate directional consistency between main and strict samples. |
| a_H1CO8_yesno | H1CO8 | M6: Final parsimonious model | 42 | 42 | 100 | 4 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1FP7_yesno | H1FP7 | M0: Core controls | 3 | 3 | 100 | 1 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1FP7_yesno | H1FP7 | M1: Family context | 14 | 14 | 100 | 6 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1FP7_yesno | H1FP7 | M3: Knowledge and attitudes | 17 | 17 | 100 | 6 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1FP7_yesno | H1FP7 | M4: Peers and relationships | 7 | 7 | 100 | 2 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1FP7_yesno | H1FP7 | M6: Final parsimonious model | 44 | 44 | 100 | 9 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1HS9_yesno | H1HS9 | M0: Core controls | 3 | 3 | 100 | 1 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1HS9_yesno | H1HS9 | M1: Family context | 27 | 27 | 100 | 9 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1HS9_yesno | H1HS9 | M2: School context | 24 | 24 | 100 | 4 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1HS9_yesno | H1HS9 | M3: Knowledge and attitudes | 17 | 17 | 100 | 1 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |
| a_H1HS9_yesno | H1HS9 | M4: Peers and relationships | 7 | 6 | 85.71 | 1 | 1 | 1 | moderate_directional_robustness | Moderate directional consistency between main and strict samples. |
| a_H1HS9_yesno | H1HS9 | M6: Final parsimonious model | 45 | 40 | 88.89 | 1 | 0 | 5 | moderate_directional_robustness | Moderate directional consistency between main and strict samples. |
| a_sex_ever | sex ever | M0: Core controls | 3 | 3 | 100 | 3 | 0 | 0 | high_directional_robustness | High directional consistency between main and strict samples. |

## 11. Reporting cautions

| sample | caution_level | caution_type | n_cautions |
| --- | --- | --- | --- |
| Main sample: grades 10-12 | high | model_level_caution | 1 |
| Main sample: grades 10-12 | moderate | coefficient_level_caution | 1 |
| Strict sensitivity sample: grades 10-12 and ages 15-19 | high | model_level_caution | 1 |

## 12. Methodological limitations

- The analysis is associational and not causal.
- The public-use data structure limits the level of detail available for some constructs.
- Some richer models were not selected for final reporting because of numerical instability, extreme odds ratios or wide confidence intervals.
- Models based on rare outcomes require careful interpretation.
- The final report prioritizes conservative and technically defensible reporting over exhaustive model complexity.

## 13. Conclusion

The final reporting results support a cautious descriptive interpretation. The most defensible final models are conservative core-control models. They provide adjusted associations for key demographic controls, while richer substantive models remain exploratory or require additional manual review.

## 14. Output inventory

| output_type | path | public_safe |
| --- | --- | --- |
| Markdown report | docs/add_health_wave01_final_technical_report_script13.md | yes |
| Word report | docs/add_health_wave01_final_technical_report_script13.docx | yes |
| Excel workbook | outputs/tables/script13_wave01_final_report_tables.xlsx | yes |
| Model summary table | outputs/tables/script13_wave01_report_model_summary.csv | yes |
| Main results table | outputs/tables/script13_wave01_report_main_results.csv | yes |
| Appendix review table | outputs/tables/script13_wave01_report_appendix_results.csv | yes |
| Robustness summary table | outputs/tables/script13_wave01_report_robustness_summary.csv | yes |
| Caution summary table | outputs/tables/script13_wave01_report_caution_summary.csv | yes |
| Execution checklist | outputs/diagnostics/script13_execution_checklist.csv | yes |
