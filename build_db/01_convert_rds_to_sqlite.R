
## Author: Junmin Wang
## Date: 10/27/2024
## Note: To build the DOSE-L1000 sqlite database, one needs to provide the correct path 
## to the rds files (lines 17 - 22 and lines 87 - 93) and the path to write the database file to (line 14). 
## The rds files can be downloaded from https://www.dosel1000.com under the "Download Data" tab.

## load libraries
library(DBI)
library(RSQLite)
library(dplyr)

## connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "/path/to/DOSE_L1000.db")

## load DOSE-L1000 data
pert <- readRDS(file = "/path/to/pert.rds")
combination <- readRDS(file = "/path/to/combination.rds")
combination_gene <- readRDS(file = "/path/to/combination_gene.rds")
test <- readRDS(file = "/path/to/test.rds")
interaction <- readRDS(file = "/path/to/interaction.rds")
gene_info <- readRDS(file = "/path/to/gene_info.rds")

## create the tables with SQL and define the primary key
create_table_query_1 <- "CREATE TABLE pert (
pert_id INTEGER PRIMARY KEY,
pert_name TEXT
);"

create_table_query_2 <- "CREATE TABLE combination (
comb_id INTEGER PRIMARY KEY,
pert_id INTEGER,
cell_id TEXT,
pert_time REAL,
model TEXT
);"

create_table_query_3 <- "CREATE TABLE combination_gene (
comb_gene_id INTEGER PRIMARY KEY,
comb_id INTEGER,
gene INTEGER
);"

create_table_query_4 <- "CREATE TABLE test (
comb_gene_id INTEGER,
pert_dose REAL,
Diff REAL,
pval REAL
);"

create_table_query_5 <- "CREATE TABLE interaction (
comb_gene_id INTEGER PRIMARY KEY,
lpotency REAL,
se_lpotency REAL,
lefficacy REAL,
se_lefficacy REAL,
pseudo_conc REAL
);"

create_table_query_6 <- "CREATE TABLE gene_info (
gene INTEGER PRIMARY KEY,
symbol TEXT
);"

dbExecute(con, create_table_query_1)
dbExecute(con, create_table_query_2)
dbExecute(con, create_table_query_3)
dbExecute(con, create_table_query_4)
dbExecute(con, create_table_query_5)
dbExecute(con, create_table_query_6)

## write to the database
dbWriteTable(con, name = "pert", value = get("pert"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "combination", value = get("combination"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "combination_gene", value = get("combination_gene"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "test", value = get("test"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "interaction", value = get("interaction"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "gene_info", value = get("gene_info"),
             append = TRUE, row.names = FALSE)

## load L1000 data
drc_GSE70138 <- readRDS(file = "/path/to/drc_GSE70138.rds")
dmso_GSE70138 <- readRDS(file = "/path/to/dmso_GSE70138.rds")
plate_GSE70138 <- readRDS(file = "/path/to/plate_GSE70138.rds")

drc_GSE92742 <- readRDS(file = "/path/to/drc_GSE92742.rds")
dmso_GSE92742 <- readRDS(file = "/path/to/dmso_GSE92742.rds")
plate_GSE92742 <- readRDS(file = "/path/to/plate_GSE92742.rds")

## create the tables with SQL and define the primary key
create_table_query_7 <- "CREATE TABLE drc (
comb_gene_id INTEGER,
pert_dose REAL,
plate_id INTEGER,
abundance REAL
);"

create_table_query_8 <- "CREATE TABLE dmso (
gene INTEGER,
plate_id INTEGER,
abundance REAL
);"

create_table_query_9 <- "CREATE TABLE plate (
plate_id INTEGER PRIMARY KEY,
plate_name TEXT
);"

dbExecute(con, create_table_query_7)
dbExecute(con, create_table_query_8)
dbExecute(con, create_table_query_9)

## write to the database
dbWriteTable(con, name = "drc", value = get("drc_GSE70138"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "drc", value = get("drc_GSE92742"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "dmso", value = get("dmso_GSE70138"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "dmso", value = get("dmso_GSE92742"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "plate", value = get("plate_GSE70138"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "plate", value = get("plate_GSE92742"),
             append = TRUE, row.names = FALSE)

## disconnect from the database
dbDisconnect(con)
