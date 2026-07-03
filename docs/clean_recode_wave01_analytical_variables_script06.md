# Clean and Recode Wave I Analytical Variables

Script 06 cleans and recodes the Add Health Wave I variables imported by Script 05.

## Input

`data/processed/add_health_wave01_confirmed_variables_local_only.rds`

## Output

`data/processed/add_health_wave01_analytical_clean_local_only.rds`

The output is a local-only individual-level analytical dataset and must not be committed to GitHub.

## Public outputs

- analytical variable inventory;
- missingness summary;
- sample definition summary;
- construct availability summary;
- recode quality checklist;
- safe public output policy.

## Sample definition

The main analytical sample remains students in grades 10 to 12 at Wave I.

Age 15 to 19 remains a complementary alignment and sensitivity criterion.

## Important limitation

This script creates recode diagnostics and preliminary analytical variables. Final construct scores require directionality and reliability review in a later script.

## Next step

Script 07 should produce descriptive aggregate tables for the Wave I analytical sample after recode quality has been reviewed.
