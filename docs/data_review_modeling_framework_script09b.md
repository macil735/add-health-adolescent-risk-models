# Data Review and Modeling Framework

Script 09b reviews the analytical data and prepares the modeling framework for Script 10.

## Purpose

This script does not estimate regression models. It organizes the data, outcomes, controls and predictors before logistic modeling.

## Samples

- Main sample: students in grades 10 to 12 at Wave I.
- Strict sensitivity sample: students in grades 10 to 12 and ages 15 to 19.

## Weight

`GSWGT1` is retained as the Wave I population-average sampling weight.

## Regional information

`REGION` was not located in the currently available local files. Therefore, regional modeling is not planned at this stage.

## Core controls

The mandatory controls are sex, age and grade when available.

## Variable blocks

Predictors are organized into:

- sociodemographic controls;
- family and household context;
- school and educational context;
- knowledge, attitudes and perceptions;
- peers and relationship context;
- general risk behaviors;
- sexual/reproductive variables requiring outcome-proximity review.

## Outcomes

Outcomes are grouped into sexual initiation, condom use, contraceptive use, pregnancy/reproductive experience and HIV/STI or sexual health.

## Interpretation

This framework supports associational modeling only. It does not imply causality.

## Privacy protection

`AID` is used only internally and is excluded from public outputs.

## Next step

Script 10 should estimate weighted logistic regression models using the recommended model sequence.
