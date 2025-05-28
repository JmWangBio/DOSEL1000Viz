
## Author: Junmin Wang
## Date: 05/23/25
## This script finds repeated perturbation conditions in both DOSE-L1000 (GAM) and L1000CDS2, generating inter-batch and random pairs.
## Make sure to update the paths to the input and output files.
## "cpcd-gse70138-cp.csv" can be created following the instructions provided in "00_export_cds2_bson_to_csv_steps.txt".
## "test.rds", "condition.rds", and "condition_gene.rds" can be downloaded from https://www.dosel1000.com under the "Download Data" tab.

## load libraries
library(dplyr)
library(tidyr)

#####################################
######## inter-batch pairs ##########
#####################################
## read CDS2 data
all_res_chdir <- read.delim("/path/to/cpcd-gse70138-cp.csv", sep = ",")

## find unique perturbation conditions in CDS2
uniq_cond_chdir <- all_res_chdir %>%
  mutate(batch_id = sub("_.*", "", batch),
         pert_dose = round(pert_dose, 2)) %>%
  dplyr::select(pert_id, batch_id, cell_id, pert_time, pert_dose) %>%
  distinct() %>%
  rename(broad_id = pert_id)

## read DOSE-L1000 (GAM) data
all_res_dosel1000 <- readRDS("/path/to/test.rds")
condition <- readRDS("/path/to/condition.rds")
condition_gene <- readRDS("/path/to/condition_gene.rds")

## find unique perturbation conditions in DOSE-L1000 fitted by GAM
uniq_cond_gam <- all_res_dosel1000 %>%
  inner_join(condition_gene, by = "cond_gene_id") %>%
  dplyr::select(cond_id, pert_dose) %>%
  distinct() %>%
  inner_join(condition, by = "cond_id") %>%
  dplyr::filter(model == "gam") %>%
  mutate(pert_dose = round(pert_dose, 2)) %>%
  dplyr::select(broad_id, batch_id, cell_id, pert_time, pert_dose) %>%
  distinct()

## find perturbation conditions shared by CDS2 and GAM
uniq_cond_common <- inner_join(uniq_cond_chdir, uniq_cond_gam)

## find common perturbation conditions repeated across batches; get inter-batch pairs
inter_batch_pairs <- uniq_cond_common  %>%
  group_by(broad_id, cell_id, pert_time, pert_dose) %>%
  filter(n() > 1) %>%
  summarise(batch_combos = list(as.data.frame(t(combn(batch_id, 2)))), .groups = "drop") %>%
  unnest(batch_combos) %>%
  rename(batch1 = V1, batch2 = V2)

################################
######## random pairs ##########
################################
## get random pairs
set.seed(123)
N <- nrow(uniq_cond_common)
rand_pairs <- tibble(
  idx1 = sample(1:N, 2000, replace = TRUE),
  idx2 = sample(1:N, 2000, replace = TRUE)
)

rand_pairs <- cbind(uniq_cond_common[rand_pairs$idx1, ] %>%
                      `colnames<-`(paste0(colnames(.), "1")), 
                    uniq_cond_common[rand_pairs$idx2, ] %>%
                      `colnames<-`(paste0(colnames(.), "2")))

## save uniq_cond_common, inter_batch_pairs, and rand_pairs
save(uniq_cond_common, inter_batch_pairs, rand_pairs, file = "/path/to/uniq_cond_and_pairs.RData")
