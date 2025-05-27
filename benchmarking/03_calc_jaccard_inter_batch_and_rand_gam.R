
## Author: Junmin Wang
## Date: 05/23/25
## This script assumes the dose-l1000 data has been downloaded.
## It calculates Jaccard indices for all pairs of perturbation conditions.
## Make sure to update the paths to the input files.

## load library
library(dplyr)
library(purrr)
library(stringr)
library(jsonlite)

## load results from "get_inter_batch_and_rand_pairs.R"
load("/path/to/uniq_cond_and_pairs.RData")

## load GAM data
all_res_gam <- readRDS("/path/to/cond_gene_sets.rds")
condition <- readRDS("/path/to/condition.rds")

all_res_gam <- all_res_gam %>%
  mutate(pert_dose = round(pert_dose, 2),
         upGenes_parsed = map(up_genes, safely(jsonlite::fromJSON)),
         upGenes_parsed = map(upGenes_parsed, ~ .x$result),
         dnGenes_parsed = map(down_genes, safely(jsonlite::fromJSON)),
         dnGenes_parsed = map(dnGenes_parsed, ~ .x$result))

## keep perturbation conditions present in both CDS2 and DOSE-L1000
common_res_gam <- all_res_gam %>%
  inner_join(condition, by = "cond_id") %>%
  inner_join(uniq_cond_common, 
             by = c("broad_id", "batch_id", "cell_id", "pert_time", "pert_dose"))

## split into up and down
common_res_gam_up <- common_res_gam %>%
  dplyr::select(broad_id, batch_id, cell_id, pert_time, pert_dose, upGenes_parsed)
common_res_gam_dn <- common_res_gam %>%
  dplyr::select(broad_id, batch_id, cell_id, pert_time, pert_dose, dnGenes_parsed)


##############################
##### inter-batch pairs ######
##############################
## join data for each batch pair
up_inter_batch_jaccard_df <- inter_batch_pairs %>%
  left_join(common_res_gam_up, by = c("broad_id", "cell_id", "pert_time", "pert_dose", "batch1" = "batch_id")) %>%
  rename(sig1 = upGenes_parsed) %>%
  left_join(common_res_gam_up, by = c("broad_id", "cell_id", "pert_time", "pert_dose", "batch2" = "batch_id")) %>%
  rename(sig2 = upGenes_parsed) %>%
  mutate(jaccard = map2_dbl(sig1, sig2, ~ length(intersect(.x, .y)) / length(union(.x, .y)))) %>%
  dplyr::select(broad_id, cell_id, pert_time, pert_dose, batch1, batch2, jaccard)

dn_inter_batch_jaccard_df <- inter_batch_pairs %>%
  left_join(common_res_gam_dn, by = c("broad_id", "cell_id", "pert_time", "pert_dose", "batch1" = "batch_id")) %>%
  rename(sig1 = dnGenes_parsed) %>%
  left_join(common_res_gam_dn, by = c("broad_id", "cell_id", "pert_time", "pert_dose", "batch2" = "batch_id")) %>%
  rename(sig2 = dnGenes_parsed) %>%
  mutate(jaccard = map2_dbl(sig1, sig2, ~ length(intersect(.x, .y)) / length(union(.x, .y)))) %>%
  dplyr::select(broad_id, cell_id, pert_time, pert_dose, batch1, batch2, jaccard)

## combine up and dn inter-batch jaccard
inter_batch_jaccard_df <- rbind(up_inter_batch_jaccard_df, dn_inter_batch_jaccard_df)


#########################
##### random pairs ######
#########################
## join data for each batch pair
up_rand_jaccard_df <- rand_pairs %>%
  left_join(common_res_gam_up, by = c("broad_id1" = "broad_id", "cell_id1" = "cell_id", "pert_time1" = "pert_time", "pert_dose1" = "pert_dose", "batch_id1" = "batch_id")) %>%
  rename(sig1 = upGenes_parsed) %>%
  left_join(common_res_gam_up, by = c("broad_id2" = "broad_id", "cell_id2" = "cell_id", "pert_time2" = "pert_time", "pert_dose2" = "pert_dose", "batch_id2" = "batch_id")) %>%
  rename(sig2 = upGenes_parsed) %>%
  mutate(jaccard = map2_dbl(sig1, sig2, ~ length(intersect(.x, .y)) / length(union(.x, .y)))) %>%
  dplyr::select(broad_id1, cell_id1, pert_time1, pert_dose1, batch_id1, broad_id2, cell_id2, pert_time2, pert_dose2, batch_id2, jaccard)

dn_rand_jaccard_df <- rand_pairs %>%
  left_join(common_res_gam_dn, by = c("broad_id1" = "broad_id", "cell_id1" = "cell_id", "pert_time1" = "pert_time", "pert_dose1" = "pert_dose", "batch_id1" = "batch_id")) %>%
  rename(sig1 = dnGenes_parsed) %>%
  left_join(common_res_gam_dn, by = c("broad_id2" = "broad_id", "cell_id2" = "cell_id", "pert_time2" = "pert_time", "pert_dose2" = "pert_dose", "batch_id2" = "batch_id")) %>%
  rename(sig2 = dnGenes_parsed) %>%
  mutate(jaccard = map2_dbl(sig1, sig2, ~ length(intersect(.x, .y)) / length(union(.x, .y)))) %>%
  dplyr::select(broad_id1, cell_id1, pert_time1, pert_dose1, batch_id1, broad_id2, cell_id2, pert_time2, pert_dose2, batch_id2, jaccard)

## combine up and dn rand jaccard
rand_jaccard_df <- rbind(up_rand_jaccard_df, dn_rand_jaccard_df)
