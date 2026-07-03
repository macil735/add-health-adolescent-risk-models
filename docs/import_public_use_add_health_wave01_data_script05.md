# Import Public-Use Add Health Wave I Data

Script 05 imports only confirmed public-use Add Health Wave I variables.

## Input file

`data/raw/21600-0001-Data.sav`

This file is treated as the Add Health Wave I public-use data file.

## Wave restriction

The confirmed variable dictionary is filtered to Wave I only before import.

Confirmed variables from other waves are excluded from this script and should be handled in separate wave-specific import scripts if those data files are available.

## Microdata policy

Raw microdata remain local in `data/raw/`.

Processed individual-level data are saved locally in `data/processed/`.

Neither raw nor processed microdata should be committed to GitHub.

## Processed local-only file

`data/processed/add_health_wave01_confirmed_variables_local_only.rds`

## Public outputs

- Wave I variable availability diagnostics;
- construct import summary;
- missingness summary;
- sample restriction summary;
- safe public output policy.

## Sample alignment

The main analytical sample is restricted to students in grades 10 to 12 at Wave I.

Age 15 to 19 is retained as a complementary alignment and sensitivity criterion.

## Next step

Script 06 should clean, recode and document the imported Wave I analytical variables before descriptive analysis.
