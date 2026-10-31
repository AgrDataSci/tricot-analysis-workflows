library("ClimMobTools")
library("gosset")
library("PlackettLuce")
library("ggplot2")
library("tidyverse")
library("magrittr")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

# climate variable cleaning helper
clean_clim_vars = function(dat, response = "G", cor_cutoff = 0.90) {
  
  # separate response and predictors
  y = dat[, response, drop = FALSE]
  x = dat[, setdiff(names(dat), response), drop = FALSE]
  
  # keep only numeric predictors
  x = x[, sapply(x, is.numeric), drop = FALSE]
  
  # remove variables with non-finite values
  non_finite = names(x)[sapply(x, function(z) any(!is.finite(z)))]
  x = x[, !names(x) %in% non_finite, drop = FALSE]
  
  # remove all-zero variables
  all_zero = names(x)[sapply(x, function(z) all(z == 0, na.rm = TRUE))]
  x = x[, !names(x) %in% all_zero, drop = FALSE]
  
  # remove zero-variance variables
  zero_var = names(x)[sapply(x, function(z) {
    length(unique(na.omit(z))) <= 1
  })]
  x = x[, !names(x) %in% zero_var, drop = FALSE]
  
  # identify highly correlated variables
  cor_mat = cor(x, use = "pairwise.complete.obs")
  cor_mat[lower.tri(cor_mat, diag = TRUE)] = NA
  
  high_cor_pairs = which(abs(cor_mat) > cor_cutoff, arr.ind = TRUE)
  
  high_cor_table = data.frame(
    var1 = rownames(cor_mat)[high_cor_pairs[, 1]],
    var2 = colnames(cor_mat)[high_cor_pairs[, 2]],
    cor  = cor_mat[high_cor_pairs]
  )
  
  # simple rule: remove second variable in each high-correlation pair
  remove_cor = unique(high_cor_table$var2)
  
  x_clean = x[, !names(x) %in% remove_cor, drop = FALSE]
  
  list(
    data = cbind(y, x_clean),
    removed = list(
      non_finite = non_finite,
      all_zero = all_zero,
      zero_variance = zero_var,
      high_correlation = remove_cor
    ),
    high_cor_table = high_cor_table
  )
}

dir.create("output", showWarnings = FALSE)

load("data/climate-data-matrices2.rda")

dat = read.csv("data/ranking-data.csv")

covar = read.csv("data/block-data.csv")

# select only one crop
keep = covar$crop_name == "amaranth"

covar = covar[keep, ]

rain = rain_indices[keep, ]

temp = temp_indices[keep, ]

# now in the rankings
keep = dat$block_id %in% covar$block_id

table(keep)

dat = dat[keep, ]

# ids into factor in both datasets 
ids = covar$block_id

items = unique(dat$genotype_name)

covar$block_id = factor(covar$block_id, levels = ids)

dat$block_id = factor(dat$block_id, levels = ids)

# names of traits
traits = unique(dat$trait)

# # split the ranking into traits and collection moments
# dat$id = paste(dat$collection_moment, dat$trait, sep = "-")
# 
# dat = split(dat, dat$trait)


#..........................................................
#..........................................................
#..........................................................
# PlackettLuce rankings ####
# traits to combine
target_traits = c("overall", "yield", "grownext")

dat_agro = dat[dat$trait %in% target_traits, ]

dat_agro$id = paste(dat_agro$block_id, dat_agro$trait, sep = "_")

R_agro = rank_tricot2(
  data  = dat_agro,
  items = "genotype_name",
  input = "value",
  id    = "id"
)

block_index = sub("_(overall|yield|grownext)$", "", rownames(R_agro))

idx = match(block_index, as.character(ids))

# empty ranking matrix for all blocks
M = matrix(
  0,
  nrow = length(ids),
  ncol = length(items),
  dimnames = list(as.character(ids), items)
)

M = as.rankings(M)

# bind empty rankings with real rankings
Z = rbind(M, R_agro[!is.na(idx), ])

# first rows are empty placeholders, then real rankings
index = c(
  seq_along(ids),
  idx[!is.na(idx)]
)

G_agro = group(Z, index = index)

pld = cbind(G = G_agro, rain, temp)

keep = !is.na(pld$G)

pld = pld[keep, ]

head(pld)

out = clean_clim_vars(pld, response = "G", cor_cutoff = 0.90)

pld = out$data

out$removed

pltree(G ~ ., data = pld, npseudo = 5, alpha = 0.1)

