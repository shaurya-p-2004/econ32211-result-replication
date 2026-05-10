# Infrastructure Without Adoption: The Causal Effect of Public EV Chargers on Battery Electric Vehicle Uptake and Its Distribution Across English Local Authorities

**ECON32212 Applied Economics B Dissertation | Student ID: 11020506**

## Overview
This repository contains the data and R code to replicate all 
results reported in the dissertation.

## Requirements
- R version 4.5.3
- Packages: fixest, ivreg, dplyr, tidyr, ggplot2

Install all packages by running:
install.packages(c("fixest", "ivreg", "dplyr", "tidyr", "ggplot2"))

## How to Replicate
1. Clone or download this repository
2. Open Panel_Clean.R in RStudio
3. Set your working directory to the root of this folder
4. Run the script in full — all tables and figures are 
   produced in the order they appear in the dissertation

## Data Sources
All raw data is publicly available:
- BEV stock: DfT Vehicle Licensing Statistics (VEH0142)
- Charging infrastructure: DfT EV Charging Statistics
- Income: ONS Regional GDHI
- Deprivation: IMD 2025 (GOV.UK)
- Rural/urban classification: ONS
