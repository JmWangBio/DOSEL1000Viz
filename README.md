![Maturity level-0](https://img.shields.io/badge/Maturity%20Level-ML--0-red)

DOSE-L1000-Viz
================
Junmin Wang
05/26/2025

This repo contains code and instructions to deploy the DOSE-L1000-Viz shiny app, 
construct the backend database, and benchmarking.

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
3. The scripts inside the `optional` folder show how to generate the `moa_info` and `cond_gene_sets` tables. You don't need 
to run these scripts if your goal is just to generate the database.

## Shiny App

To run the Shiny app:

1. Use the `app.R` file as the main entry point.
2. The user interface and server functions for each tab are organized in separate files for modularity.

## Benchmarking

To demonstrate the robustness of GAM-derived signatures in the DOSE-L1000 database, we compared GAM-based signatures with those derived using 
the characteristic direction (CD) method across replicated perturbation conditions. To establish a baseline, we also generated 2000 random pairs 
of perturbation conditions across the dataset. For each pair, Jaccard indices were calculated for both methods.

To reproduce the analysis:

1. Download CD-based signatures and GAM-based signatures from the L1000CDS2 portal and DOSE-L1000-Viz (https://www.dosel1000.com).
2. Follow steps outlined in `00_export_cds2_bson_to_csv_steps.txt` to format CD-based signatures.
3. Run `01_get_inter_batch_and_rand_pairs.R` to generate pairs of conditions replicated across batches and random pairs of conditions. 
4. Run `02_calc_jaccard_inter_batch_and_rand_chdir.R` and `03_calc_jaccard_inter_batch_and_rand_gam.R` to calculate the distributions of Jaccard indices 
for each method.

## References

Wang J, Novick S. DOSE-L1000: unveiling the intricate landscape of compound-induced transcriptional changes. Bioinformatics. 2023;39(11):btad683.

Duan Q, Reid S, Clark N et al. L1000CDS2: LINCS L1000 characteristic direction signatures search engine. npj Syst Biol Appl. 2016;2:16015.
