
## Author: Junmin Wang
## Date: 10/27/2024
## Note: To index the individual tables, one needs to provide the correct path 
## to the DOSE-L1000 sqlite database (line 12).

## Load libraries
library(DBI)
library(RSQLite)

## Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "/path/to/DOSE_L1000.db")

## Add indices
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_p_pn ON pert(pert_name);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_i_cgi ON interaction(comb_gene_id);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_t_cgiXpd ON test(comb_gene_id, pert_dose);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_gi_s ON gene_info(symbol);")

dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_c_celli ON combination(cell_id);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_c_piXcelli ON combination(pert_id, cell_id);")

dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cg_combiXg ON combination_gene(comb_id, gene);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cg_g ON combination_gene(gene);")

dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_drc_cgi ON drc(comb_gene_id);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_dmso_pliXg ON dmso(plate_id, gene);")

# Disconnect
dbDisconnect(con)
