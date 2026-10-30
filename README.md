# Determinants of Innovation and Innovation Spillovers in European Regions

What drives patenting in European regions, and how far does one region's R&D spending reach into its neighbours? This repository contains the R code of a group seminar paper written at TU Dresden, "Friedrich List" Faculty of Transport and Traffic Sciences, Chair of Economics, esp. Transport Policy and Spatial Economics (course *Empirical Methods in Spatial and Environmental Economics*, submitted 25 August 2025).

## Authors

- Ali Mirza
- Bhavay Singhal
- Javidan Guliyev

## Questions

1. **Determinants of innovation.** Which regional characteristics explain PCT patent applications (public and business R&D expenditure, digital skills, SME collaboration, employment in knowledge-intensive activities)?
2. **Spillovers.** Does business R&D in surrounding regions raise a region's patenting, and over what distance? The distance-band design follows Bottazzi and Peri.

## Data

Regional indicators from the OECD regional database, 2017 to 2021 (about 240 European regions, mixed NUTS levels). After cleaning, the panel has 235 regions and 5 years; Croatia is excluded because of implausibly high R&D values. Region boundaries come from Eurostat GISCO (NUTS 2021, 1:20 million).

| File | Content |
|---|---|
| `data/processed_panel_data_final.rds` | Cleaned panel with square-root transformed variables (output of script 01) |
| `data/OECD_Complete_Dataset_csv.csv` | OECD regional indicators used for the spillover analysis |

Not included: `OECD_Final_Variables.csv` (input of script 01, about 10 MB) and the NUTS shapefile (download from Eurostat GISCO and place it in `data/`). Please check the OECD and Eurostat terms of use before redistributing data.

## Scripts

Run from the repository root, in this order:

| Script | What it does |
|---|---|
| `R/01_exploratory_analysis.R` | Cleaning, outlier check, square-root transformation, scatterplot matrices and yearly correlations; writes the processed panel |
| `R/02_pooled_ols.R` | Pooled OLS with R&D only, then with human-capital and structural variables; VIF, Breusch-Pagan, robust standard errors |
| `R/03_panel_models.R` | First-difference, within (one-way and two-way fixed effects) and between estimators with `plm` |
| `R/04_spillovers_distance_bands.R` | Boundary-to-boundary distance matrix, neighbour R&D summed in five distance bands (0-300, 300-600, 600-900, 900-1300, 1300-2000 km), cross-sectional regression and diagnostics (RESET, VIF, Breusch-Pagan, residual plots) |

## Main finding

In the distance-band regression, only the 0-300 km band has a significantly positive effect on patenting; the 600-900 km band is significantly negative. Spillovers are therefore local. Details and interpretation are in the seminar paper, which is not part of this repository.

## Requirements

R with `tidyverse`, `GGally`, `plm`, `car`, `lmtest`, `sandwich`, `sf`, `geosphere`, `tmap`, `leaflet`, `readxl`, `writexl`, `base64enc`.

## Status

Course project, kept as submitted apart from replacing local file paths with relative ones. The scripts have not been re-run after this change.
