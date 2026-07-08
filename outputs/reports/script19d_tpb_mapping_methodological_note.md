# Script 19d — TPB Construct Candidate Mapping for Never-Sex Mediation

Run time: 2026-07-08 08:32:25

## Purpose

This script maps Add Health Wave I candidate variables to Theory of Planned Behavior constructs for future mediation analysis.

The intended conceptual model is:

`family_connectedness -> TPB mediators -> intention/proxy intention to delay sexual initiation`

## Analytic sample

The analytic sample includes adolescents aged 15–19 who had not yet had sexual intercourse. Final analytic sample size: 2143.

## Theoretical decision

The mediation model should not use sexual initiation as the dependent variable within the never-sex sample because sexual initiation is constant by design.

A future mediation model should first establish whether Add Health contains an adequate measure of intention to delay sexual initiation. If no direct intention item is available, the analysis should be described as an exploratory TPB-compatible mechanism analysis, not as a full TPB mediation model.

## TPB candidate domains

- Attitudes: mainly H1MO3, H1MO5-H1MO14;
- Subjective norms: mainly H1MO1, H1MO2, H1MO4;
- Perceived behavioral control / self-efficacy: H1SE and H1BC candidates require manual review;
- Intention to delay sexual initiation: no direct item is confirmed by this script; manual review is required.

## Outputs

- Candidate dictionary: outputs/audits/script19d_tpb_candidate_dictionary.csv
- Coverage audit: outputs/audits/script19d_tpb_candidate_coverage_audit.csv
- Pilot item summary: outputs/audits/script19d_tpb_pilot_item_summary.csv
- Manual review template: outputs/audits/script19d_tpb_manual_review_TEMPLATE.csv
- Pilot construct dataset: outputs/analysis/script19d_tpb_pilot_construct_dataset.rds
- Pilot construct summary: outputs/tables/script19d_tpb_pilot_construct_summary.csv
- Pilot internal consistency: outputs/tables/script19d_tpb_pilot_internal_consistency.csv
- Pilot construct correlations: outputs/tables/script19d_tpb_pilot_construct_correlations.csv

No Git action was performed.
