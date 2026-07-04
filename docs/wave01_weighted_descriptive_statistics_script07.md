# Wave I Weighted Descriptive Statistics

Script 07 produces aggregate descriptive statistics for Add Health Wave I.

## Weighting

The script uses `GSWGT1` as the Wave I population-average sampling weight.

## AID

`AID` is used only internally for merging survey weights. It is not exported in public outputs.

If `AID` is missing from the analytical RDS, it is recovered from DS0001 only when the row count matches exactly.

## DS0021

`21600-0021-Data.rda` is an auxiliary ICPSR dataset number. It is not Wave II. It is used only if it provides `AID` and `CLUSTER2`.

## Main sample

The main analytical sample is students in grades 10 to 12 at Wave I.

## Strict sensitivity sample

The strict sensitivity sample is students in grades 10 to 12 and ages 15 to 19.

## Design limitation

`REGION` and `W1_WC` were not found in the currently available local files inspected so far.

## Public outputs

Only aggregate diagnostics and descriptive tables are exported. No individual-level microdata are exported.

## Next step

Script 08 should review missing-data patterns and final recoding decisions before regression modeling.
