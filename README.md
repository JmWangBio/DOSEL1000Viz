![Maturity level-0](https://img.shields.io/badge/Maturity%20Level-ML--0-red)

DOSE-L1000-Viz
================
Junmin Wang
05/26/2025

This repo contains code and instructions to deploy the DOSE-L1000-Viz shiny app and 
construct the backend database.

## Introduction

DOSE-L1000-Viz is a Shiny-based application for visualizing compound-induced gene expression 
changes and dose-response data from the LINCS L1000 database. DOSE-L1000-Viz provides volcano 
plots, dose-response curves, and comparative efficacy-potency analysis across different 
compounds, genes, cell lines, and time points. The app allows users to easily query, visualize, 
and download the data, simplifying the analysis of dose-dependent effects and drug mechanisms 
of action. DOSE-L1000-Viz is publicly accessible at https://www.dosel1000.com

## Prerequisites

The following R libraries are required:
- DBI
- RSQLite
- dplyr
- tidyr
- shiny
- ggplot2
- plotly
- mgcv
- shinycssloaders
- shinyjs
- stringr
- DT
- jsonlite

## Database Generation

Considering the large size of the SQLite database, the individual tables are provided as 
`.rds` files for easier download. You can download each `.rds` file from https://www.dosel1000.com 
under the **Download Data** tab.

> **Note**: `test.rds` is approximately 5GB, so downloading may take some time.

To set up the database:

1. Run `01_convert_rds_to_sqlite.R` to import the downloaded `.rds` files into a single SQLite database file.
2. Then, execute `02_add_index_to_sqlite.R` to index the tables in the SQLite database.

## Shiny App

To run the Shiny app:

1. Use the `app.R` file as the main entry point.
2. The user interface and server functions for each tab are organized in separate files for modularity.

## References

Wang J, Novick S. DOSE-L1000: unveiling the intricate landscape of compound-induced transcriptional changes. Bioinformatics. 2023;39(11):btad683.
