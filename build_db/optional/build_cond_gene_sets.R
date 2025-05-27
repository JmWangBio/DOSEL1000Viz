
## Author: Junmin Wang
## Date: 05/23/25
## Note: The purpose of this script is to build the gene signatures for all perturbation conditions in the DOSE-L1000 database, To build cond_gene_sets, one needs to provide the correct path to the rds files and the output file.

## Load libraries
library(dplyr)
library(jsonlite)
library(stringr)

## Load L1000 data
condition <- readRDS(file = "/path/to/condition.rds")
condition_gene <- readRDS(file = "/path/to/condition_gene.rds")
test <- readRDS(file = "/path/to/test.rds")
gene_info <- readRDS(file = "/path/to/gene_info.rds")

## Keep only data for GAMs
test_GAM <- condition %>%
  filter(model == "gam") %>%
  inner_join(condition_gene, by = "cond_id") %>%
  inner_join(test, by = "cond_gene_id")

## Convert gene to character
gene_info <- gene_info %>% mutate(gene = as.character(gene))

## Convert test_GAM to new format
cond_gene_sets <- test_GAM %>%
  inner_join(gene_info, by = "gene") %>%
  group_by(broad_id, batch_id, cell_id, pert_time, pert_dose) %>%
  mutate(adj.pval = p.adjust(pval, method = "BH")) %>%
  summarise(up_genes = list(symbol[Diff > 1 & adj.pval < 0.05]),
            down_genes = list(symbol[Diff < -1 & adj.pval < 0.05])) %>%
  ungroup() %>%
  filter(lengths(up_genes) > 0 | lengths(down_genes) > 0) %>%
  mutate(up_genes = sapply(up_genes, toJSON),
         down_genes = sapply(down_genes, toJSON)) %>%
  dplyr::select(broad_id, batch_id, cell_id, pert_time, pert_dose, 
                up_genes, down_genes) %>%
  inner_join(condition, by = c("broad_id",
                               "batch_id",
                               "cell_id",
                               "pert_time")) %>%
  dplyr::select(cond_id, pert_dose, up_genes, down_genes)

## Save output
saveRDS(object = cond_gene_sets, file = "/path/to/cond_gene_sets.rds")
