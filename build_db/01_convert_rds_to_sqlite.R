
## Author: Junmin Wang
## Date: 05/23/25
## Note: To build the DOSE-L1000 sqlite database, one needs to provide the correct path to the rds files and the path to write the database file to.
## The rds files can be downloaded from https://www.dosel1000.com under the "Download Data" tab.

## load libraries
library(DBI)
library(RSQLite)
library(dplyr)

## connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "/path/to/DOSE_L1000.db")

## load DOSE-L1000 data
pert <- readRDS(file = "/path/to/pert.rds")
condition <- readRDS(file = "/path/to/condition.rds")
condition_gene <- readRDS(file = "/path/to/condition_gene.rds")
test <- readRDS(file = "/path/to/test.rds")
interaction <- readRDS(file = "/path/to/interaction.rds")
gene_info <- readRDS(file = "/path/to/gene_info.rds")

## create the tables with SQL and define the primary key
create_table_query_1 <- "CREATE TABLE pert (
broad_id TEXT PRIMARY KEY,
cpd_name TEXT
);"

create_table_query_2 <- "CREATE TABLE condition (
cond_id INTEGER PRIMARY KEY,
broad_id TEXT,
cell_id TEXT,
pert_time REAL,
batch_id TEXT,
model TEXT,
broad_batch_id TEXT
);"

create_table_query_3 <- "CREATE TABLE condition_gene (
cond_gene_id INTEGER PRIMARY KEY,
cond_id INTEGER,
gene INTEGER
);"

create_table_query_4 <- "CREATE TABLE test (
cond_gene_id INTEGER,
pert_dose REAL,
Diff REAL,
pval REAL
);"

create_table_query_5 <- "CREATE TABLE interaction (
cond_gene_id INTEGER PRIMARY KEY,
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
dbWriteTable(con, name = "condition", value = get("condition"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "condition_gene", value = get("condition_gene"),
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
cond_gene_id INTEGER,
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

## load remaining data
cond_gene_sets <- readRDS(file = "/path/to/cond_gene_sets.rds")
moa_info <- readRDS(file = "/path/to/moa_info.rds")
cell_info <- readRDS(file = "/path/to/cell_info.rds")

## create the tables with SQL and define the primary key
create_table_query_10 <- "CREATE TABLE cond_gene_sets (
cond_id INTEGER,
pert_dose REAL,
up_genes TEXT,
down_genes TEXT
);"

create_table_query_11 <- "CREATE TABLE moa_info (
cpd_name TEXT PRIMARY KEY,
moa TEXT
);"

create_table_query_12 <- "CREATE TABLE cell_info (
cell_id TEXT PRIMARY KEY,
primary_site TEXT
);"

dbExecute(con, create_table_query_10)
dbExecute(con, create_table_query_11)
dbExecute(con, create_table_query_12)

## write to the database
dbWriteTable(con, name = "cond_gene_sets", value = get("cond_gene_sets"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "moa_info", value = get("moa_info"),
             append = TRUE, row.names = FALSE)
dbWriteTable(con, name = "cell_info", value = get("cell_info"),
             append = TRUE, row.names = FALSE)

## disconnect from the database
dbDisconnect(con)
