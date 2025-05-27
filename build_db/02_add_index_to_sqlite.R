
## Author: Junmin Wang
## Date: 05/23/2025
## Note: To index the individual tables, one needs to provide the correct path to the DOSE-L1000 sqlite database.

## Load libraries
library(DBI)
library(RSQLite)

## Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "/path/to/DOSE_L1000.db")

## Add indices
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_c_broadiXcelli ON condition(broad_id, cell_id);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_c_celli ON condition(cell_id);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_c_broadbatchiXcelli ON condition(broad_batch_id, cell_id);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_gi_s ON gene_info(symbol);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_p_cn ON pert(cpd_name);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cg_condiXg ON condition_gene(cond_id, gene);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cg_g ON condition_gene(gene);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_t_cgiXpd ON test(cond_gene_id, pert_dose);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_drc_cgi ON drc(cond_gene_id);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_dmso_pliXg ON dmso(plate_id, gene);")

## Disconnect
dbDisconnect(con)

