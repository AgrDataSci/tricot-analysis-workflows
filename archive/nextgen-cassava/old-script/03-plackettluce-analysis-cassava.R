# .......................................
# .......................................
# PlackettLuce tree with NextGen cassava data 
library("PlackettLuce")
library("tidyverse")
library("ClimMobTools")
library("caret")
library("gosset")
library("ggplot2")
library("patchwork")
library("ggparty")
library("mapview")
library("leaflet")
library("patchwork")
library("magrittr")
library("tidyverse")
library("reshape2")
library("heatmap3")
library("corpcor")
library("GeneNet")
library("readxl")
library("janitor")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

source("script/helper-01-function.R")

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/03_analysis_pltree.txt")

dir.create("output", 
           recursive = TRUE,
           showWarnings = FALSE)

# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
list.files("data", full.names = TRUE)

dat = read.csv("data/tricot-data-nextgen-cassava.csv")

genotype = read_excel("data/genotype-information.xlsx")

names(genotype) = make_clean_names(names(genotype))

pack_index = paste0("variety", letters[1:3])

itemnames = sort(unique(unlist(dat[, pack_index])))

list.files("output", full.names = TRUE)

trait_cor = read.csv("output/kendall-partial-correlation-backward-selection.csv")

keep = !grepl("Overall", trait_cor$trait)

trait_cor = trait_cor[keep, ]

# most important trait in each data collection moment
trait_cor = split(trait_cor, trait_cor$cropstage)

trait_cor = lapply(trait_cor, function(x){
  i = which.max(x$cor)
  x[i, ]
})

trait_cor = do.call("rbind",  trait_cor)

# add traits highly correlated to overall preference at harvest
trait_cor2 = read.csv("output/kendall-partial-correlation-backward-selection-overall-at-harvest.csv")

trait_cor = rbind(trait_cor, trait_cor2)

trait_cor = trait_cor[!duplicated(trait_cor$trait), ]

trait_cor = trait_cor[trait_cor$cor > 0.3, ]

trait_sel = c(trait_cor$trait, 
              "Final Evaluation - Choice For Replant",
              "Harvesting - Overall")

# select the top 10 traits, but remove overall performance 
# harvesting traits don't figure in the top 10, but 
# I will keep them and select the top 7 from the other 
# crop stages
# trait_top10 = trait_cor[!grepl("Overall", trait_cor$trait), ]
# 
# top10 = which(rev(order(trait_top10$cor) <= 7))
# 
# trait_top10 = trait_top10[top10, ]
# 
# trait_top10$trait_weight = trait_top10$cor / sum(trait_top10$cor)

# ......................................
# ......................................
# Trait list #####
# get the list of traits
# and fix names in data collection moments
trait_list = getTraitList(dat, pattern = c("_best", "_worst"))

traits = lapply(trait_list, function(x){
  x$trait_label
})

traits = unlist(traits)

traits = unlist(traits)

traitlabels = fix_trait_labels(traits)

# keep traits selected in the previous script 
# from backward selection

trait_list = getTraitList(dat,
                          pattern = c("_best", "_worst"),
                          trait.labels = traitlabels)


keep = which(traitlabels %in% trait_sel)

trait_list = trait_list[keep]

traitlabels = traitlabels[keep]

# ..............................
# ..............................
# Covariates ####
# select covariates based on this paper
# https://doi.org/10.3389/fsufs.2021.740926
# ..............................
# ..............................
names(dat)

covars = c("sex", "processor", "other_processor_gender", 
           "crop_div", "score_hdds_bad_season", "ppi_likelihood", 
           "gender_male_control", "h_hsize_mae", "land_owned",
           "household_type", "year")

newnamescovars = c("GenderFarmer", "Processor", "GenderProcessor", 
                   "CropDiv", "HDDSBadSeason","PPI", "GenderMaleControl",
                   "HHSizeMAE", "LandOwned", "HouseholdType", "TrialYear")

covar = dat[, covars]

names(covar) = newnamescovars 

covar$TrialYear = as.factor(covar$TrialYear)

table(covar$TrialYear)

# update gender of processor, using information from the trial 
covar$GenderProcessor = ifelse(covar$Processor  == "Self",
                               covar$GenderFarmer, 
                               covar$GenderProcessor)


table(covar$GenderFarmer)

covar$GenderFarmer[is.na(covar$GenderFarmer)] = "Woman"

table(covar$GenderProcessor)

covar$GenderProcessor[covar$GenderProcessor == "M"] = "Man"

covar$GenderProcessor[covar$GenderProcessor == "F"] = "Woman"

covar$GenderProcessor[is.na(covar$GenderProcessor)] = "Woman"

table(covar$GenderProcessor)

covar[1:ncol(covar)] = lapply(covar[1:ncol(covar)], function(x){
  
  if (class(x) != "character") {
    x[is.na(x)] = mean(x, na.rm = TRUE)
  }
  
  if (class(x) == "numeric") {
    x = round(x, 2)
  }
  
  if (class(x) == "character") {
    x = ifelse(is.na(x), "Not specified", x)
    x = as.factor(x)
  }
  
  x
  
})

# .....................................................
# .....................................................
# Check covariates ####
names(covar)

table(covar$GenderFarmer)
table(covar$GenderProcessor)
table(covar$GenderFarmer, covar$Processor)

plot(density(covar$CropDiv))

hist(covar$CropDiv)

plot(density(covar$HDDSBadSeason))

plot(density(covar$GenderMaleControl))

table(covar$Processor, covar$HouseholdType)

table(covar$PPI)

plot(density(covar$LandOwned))

boxplot(covar$LandOwned)

#.....................................
#.....................................
# List of rankings #####

R = lapply(trait_list, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})

traits

traitlabels

#.....................................
#.....................................
# Overall performance at harvest ####
# This is one of the few traits (and the strongest)
# from the agronomic part of cassava testing that
# correlates with the choice for replant, see results from 
# previous script. Here I will assess the influence of 
# socio-economic variables on the decision for different varieties
trait_harvest = c("Harvesting - Fresh Roots Yield",
                  "Harvesting - Overall")

for (j in seq_along(trait_harvest)) {
  
  base = grep(trait_harvest[j], traitlabels)
  
  R_base = R[[base]]
  
  keep = trait_list[[base]]$keep
  
  R_base = R_base[keep, ]
  
  covar_base = covar[keep, ]
  
  # make the tree
  G_base = group(R_base, index = 1:length(R_base))
  
  pld = cbind(G = G_base, covar_base)
  
  tree = pltree(G ~ ., 
                data = pld,
                minsize = 50,
                npseudo = 30,
                gamma = TRUE,
                alpha = 0.05)
  
  nodes_tree = predict(tree, type = "node")
  node_id_tree = sort(unique(nodes_tree))
  
  length(nodes_tree) == length(G_base)
  
  tree_mod = list()
  nobs_tree = integer()
  
  for (i in seq_along(node_id_tree)) {
    
    Gi = G_base[nodes_tree == node_id_tree[i]]
    
    tree_mod[[i]] = PlackettLuce(Gi)
    
    nobs_tree = c(nobs_tree, length(Gi))
    
    rm(Gi)
  }
  
  tree_branch = gosset:::build_tree_branches(tree)
  
  tree_nodes = gosset:::build_tree_nodes(tree_mod, 
                                         log = TRUE,
                                         node.ids = node_id_tree,
                                         n.obs = nobs_tree,
                                         ci.level = 0.05)
  
  ptree = tree_branch / tree_nodes + plot_layout(heights =  c(1, 1))
  
  t_i = gsub(" |-", "", tolower(trait_harvest[j]))
  
  ggsave(filename = paste0("output/pltree-", t_i, ".pdf"),
         plot = ptree,
         units = "cm",
         height = 40,
         width = 40,
         dpi = 600)
  
  
  # add node info to the main data
  model_nodes = predict(tree, newdata = covar_base, type = "node")
  ids = dat$tricot_pkg[keep]
  
  model_nodes = data.frame(node = model_nodes,
                           tricot_pkg = ids)
  
  rules = node_rules(tree)
  
  model_nodes = merge(model_nodes, rules, by = "node", all.x = TRUE)
  
  names(model_nodes)[c(1,3)] = paste0(names(model_nodes)[c(1,3)], "_", t_i)
  
  dat = merge(dat, model_nodes, by = "tricot_pkg", all.x = TRUE)
  
  
}

names(dat)

#.....................................
#.....................................
# Choice for replant based on processor and farmer gender ####
base = grep("Choice For Replant", traitlabels)

R_sel = R[which(traitlabels %in% trait_sel)]

R_base = R[[base]]

# ranking network
plot(network(R_base))

pdf("output/cassava-ranking-network-choice-for-replant.pdf",
    width = 20,
    height = 20)
plot(network(R_base))
dev.off()

keep = trait_list[[base]]$keep

R_base = R_base[keep, ]

covar_base = covar[keep, ]

# make the tree
G_base = group(R_base, index = 1:length(R_base))

pld = cbind(G = G_base, covar_base)

nrow(pld)

tree_replant = pltree(G ~ Processor + GenderFarmer, 
                      data = pld,
                      minsize = 50,
                      npseudo = 30,
                      gamma = TRUE,
                      alpha = 0.05)

tree = tree_replant

tree

node_rules(tree)

# run this for the condensended rankings using predicted nodes
nodes_tree = predict(tree, type = "node")

table(nodes_tree)

sum(table(nodes_tree))

node_id_tree = sort(unique(nodes_tree))

length(nodes_tree) == length(G_base)

tree_mod = list()
nobs_tree = integer()

for (i in seq_along(node_id_tree)) {
  
  print(node_id_tree[i])
  
  Gi = G_base[nodes_tree == node_id_tree[i]]
  
  tree_mod[[i]] = PlackettLuce(Gi)
  
  nobs_tree = c(nobs_tree, length(Gi))
  
  rm(Gi)
}

nobs_tree

sum(nobs_tree)

tree_branch = gosset:::build_tree_branches(tree)

tree_nodes = gosset:::build_tree_nodes(tree_mod, 
                                       log = TRUE,
                                       node.ids = node_id_tree,
                                       n.obs = nobs_tree,
                                       ci.level = 0.05)

ptree = tree_branch / tree_nodes + plot_layout(heights =  c(0.5, 1))

ptree

ggsave(filename = "output/pltree-choice-for-replant.pdf",
       plot = ptree,
       units = "cm",
       height = 25,
       width = 25,
       dpi = 600)

# add node info to the main data
dat = cbind(dat, covar)

model_nodes = predict(tree, newdata = dat[keep, ], type = "node")

ids = dat$tricot_pkg[keep]

model_nodes = data.frame(node = model_nodes,
                         tricot_pkg = ids)

rules = node_rules(tree)

model_nodes = merge(model_nodes, rules, by = "node", all.x = TRUE)

names(model_nodes)[c(1,3)] = paste0(names(model_nodes)[c(1,3)], "_choice_replant")

head(model_nodes)

# create worth map based on the nodes from the tree
nodes = as.integer(predict(tree, 
                           newdata = dat[keep, names(covar)], 
                           type = "node"))

nodes_ids = sort(unique(nodes))

table(nodes)

dat = merge(dat, model_nodes, by = "tricot_pkg", all.x = TRUE)

write.csv(dat, "output/tricot-data-with-node-info.csv", row.names = FALSE)


# .........................................
# .........................................
# .........................................
# Worth map with three of choice for replanting ####
# retain only the entries used in the tree
R_worth_map = lapply(R_sel, function(x){
  x[keep, ]
})

# first get the list of genotypes, released and the best in the 10th percentile
# of candidate varieties
# I will use the regret analysis as it will give the best approximation
# including all the nodes in the choice for replanting
reference = "TMEB2"

st_regret = regret(tree, n1 = 100)

best_threshold1 = quantile(best_regret$worth, probs = 0.80)
best_threshold2 = quantile(best_regret$regret, probs = 0.20)
best_threshold3 = quantile(best_regret$worst_regret, probs = 0.20)

keep1 = which(best_regret$worth >= best_threshold1)
keep2 = which(best_regret$regret <= best_threshold2)
keep3 = which(best_regret$worst_regret <= best_threshold3)

keep = union(keep1, keep2)
keep = union(keep, keep3)

best_genotypes = best_regret$items[keep]

keep = genotype$genotype %in% best_genotypes

keep = genotype$year_of_release == "NR" & keep

sum(keep)


genotype$genotype[keep]

# add a pseudo year of release to the candidates
genotype$year_of_release[keep] = "2023"

# select the released varieties
released = genotype$year_of_release != "NR"

released = genotype[released, ]

released = released[order(released$year_of_release), ]

released$final_name = factor(released$genotype,
                             levels = union(reference, released$genotype))

tree_worth_map = list()

for(i in seq_along(nodes_ids)) {
  
  k_i = nodes == nodes_ids[i]
  
  print(sum(k_i))
  
  R_i = lapply(R_worth_map, function(x){
    x = x[k_i, ]
    PlackettLuce(x)
  })
  
  winprobs = gosset:::.combine_coeffs(R_i, 
                                      log = TRUE,
                                      vcov = FALSE,
                                      ref = reference)
  
  # add name of features
  names(winprobs) = traitlabels
  
  sel = rownames(winprobs) %in% released$final_name
  
  winprobs = winprobs[sel, ]
  
  winprobs = data.frame(items = rep(dimnames(winprobs)[[1]], 
                                    times = ncol(winprobs)),
                        labels = rep(dimnames(winprobs)[[2]], 
                                     each = nrow(winprobs)),
                        winprob = as.numeric(unlist(winprobs)))
  
  winprobs$labels = factor(winprobs$labels, levels = traitlabels)
  
  winprobs$items = factor(winprobs$items, levels = levels(released$final_name))
  
  lims = max(abs(winprobs$winprob)) * c(-1, 1)
  
  wp = ggplot(winprobs, 
         aes(x = items, 
             y = labels,
             fill = winprob,
             label = as.character(round(winprob, 2)))) +
    geom_tile() + 
    scale_fill_distiller(palette = "BrBG", 
                         limit = lims,
                         direction = 1,
                         na.value = "white",
                         name = "") +
    theme_minimal() +
    theme(axis.text = element_text(color = "grey20"),
          strip.text.x = element_text(color = "grey20"),
          axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1),
          axis.text.y = element_text(angle = 1, vjust = 1, hjust = 1),
          panel.grid = element_blank()) +
    labs(x = "", 
         y = "",
         fill = "")
  
  tree_worth_map[[i]] = wp
  
}

node_maps = 
  tree_worth_map[[1]] +
  tree_worth_map[[2]] +
  tree_worth_map[[3]] 

node_maps = tree_branch / node_maps

node_maps 

ggsave(filename = "output/pltree-choice-replant-worth-map.pdf",
       plot = node_maps,
       units = "cm",
       height = 30,
       width = 40,
       dpi = 600)


# ..........................................
# ..........................................
# Put the selected rankings together  ####
# to see the "genetic gain" over the years
# attempt to create selection index (indices)
# ....................................
# ....................................
basetrait = "Final Evaluation - Choice For Replant"

trait_group = union(trait_top10$trait, basetrait)

basetrait = grep(basetrait, traitlabels)

# this part will group the rankings by each trait 
# using the index argument in group() to assess genotype 
# performance across traits
# set the indices for the basetrait
indicesbase1 <- as.vector(which(trait_list[[basetrait]]$keep))
resetindices1 <- 1:length(indicesbase1)

R1 <- list()
index1 <- c()

for(i in seq_along(trait_group)) {
  
  trait_i = grep(trait_group[i], traitlabels)
  
  # this should be combined with the baseline trait
  index_i = as.vector(which(trait_list[[trait_i]]$keep))
  
  keep_i = index_i %in% indicesbase1
  
  index_i = index_i[keep_i]
  
  r_i <- rankTricot(dat[index_i, ],
                    pack_index,
                    trait_list[[trait_i]]$string,
                    group = FALSE)
  
  # reset indices to match with grouped_rankings later
  index_i <- resetindices1[indicesbase1 %in% index_i]
  
  index1 <- c(index1, index_i)
  
  R1[[i]] <- r_i
  
}

R1 <- do.call("rbind", R1)

G1 <- group(R1, index = index1)

# model with all the data
mod_grouped = PlackettLuce(G1)

# get the check varieties
checks = genotype$genotype[genotype$check_variety == "Y"]

# calculate reliability over the the check varieties
reliabilities = data.frame()

for(i in seq_along(checks)) {
  
  r_i = reliability(mod_grouped, ref = checks[i])
  
  r_i$check = checks[i]
  
  reliabilities = rbind(reliabilities, r_i)
  
}

# take the average and max min for each of 
# the reliabilities
reliabilities = 
  reliabilities %>% 
  group_by(item) %>% 
  summarise(min_rel = min(reliability), 
            max_rel = max(reliability),
            reliability = mean(reliability),
            worth = mean(worth)) %>% 
  rename(genotype = item)

# add crossing year and order by this
names(genotype)

reliabilities = merge(reliabilities, 
                      genotype[,c("genotype", "crossing_year")],
                      by = "genotype")

# for landrace I'll assume that crossing year is 1965, 2 years 
# before IITA 
reliabilities$crossing_year[reliabilities$crossing_year == "Landrace"] = 1965

reliabilities$crossing_year = as.integer(reliabilities$crossing_year)

reliabilities = reliabilities[order(reliabilities$crossing_year), ]

crono_order = reliabilities$genotype

reliabilities$genotype = factor(reliabilities$genotype, levels = rev(crono_order))

reliability_plot = ggplot(data = reliabilities,
       aes(x = genotype, 
           y = reliability,
           ymax = max_rel,
           ymin = min_rel)) +
  geom_hline(yintercept = 0.5, 
             colour = "#3182bd",
             linewidth = 0.8) +
  geom_point() +
  facet_grid(rows = vars(crossing_year),
             scales = "free",
             space = "free") +
  geom_errorbar(width = 0.1) +
  theme_minimal() +
  coord_flip() +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(vjust = 1, hjust=1,
                                   size = 10, color = "grey20"),
        axis.text.y = element_text(size = 10, color = "grey20"),
        strip.background.x = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank()) +
  labs(x = "", y = "Reliability")

reliability_plot

ggsave("output/reliability-grouped-rankings-all-data.pdf",
       reliability_plot,
       width = 15,
       height = 20,
       units = "cm")

#.....................................
#.....................................
# Now the same but for the tree nodes 
# model with all the data
length(nodes)
length(G1)

node_id_tree

reliability_plot = list()

for (nn in seq_along(node_id_tree)) {
  
  node_select = nodes_tree == nodes[nn]
  
  mod_grouped = PlackettLuce(G1[node_select, ])
  
  # calculate reliability over the the check varieties
  reliabilities = data.frame()
  
  for(i in seq_along(checks)) {
    
    r_i = reliability(mod_grouped, ref = checks[i])
    
    r_i$check = checks[i]
    
    reliabilities = rbind(reliabilities, r_i)
    
  }
  
  # do crazy stuff here !!!
  # take the average and max min for each of 
  # the reliabilities
  reliabilities = 
    reliabilities %>% 
    group_by(item) %>% 
    summarise(min_rel = min(reliability), 
              max_rel = max(reliability),
              reliability = mean(reliability),
              worth = mean(worth)) %>% 
    rename(genotype = item)
  
  reliabilities = merge(reliabilities, 
                        genotype[,c("genotype", "crossing_year")],
                        by = "genotype")
  
  # for landrace I'll assume that crossing year is 1965, 2 years 
  # before IITA 
  reliabilities$crossing_year[reliabilities$crossing_year == "Landrace"] = 1965
  
  reliabilities$crossing_year = as.integer(reliabilities$crossing_year)
  
  reliabilities$genotype = factor(reliabilities$genotype, levels = rev(crono_order))
  
  rel_plot = ggplot(data = reliabilities,
                            aes(x = genotype, 
                                y = reliability,
                                ymax = max_rel,
                                ymin = min_rel)) +
    geom_hline(yintercept = 0.5, 
               colour = "#3182bd",
               linewidth = 0.8) +
    ylim(0.2, 0.8) +
    geom_point() +
    facet_grid(rows = vars(crossing_year),
               scales = "free",
               space = "free") +
    geom_errorbar(width = 0.1) +
    theme_minimal() +
    coord_flip() +
    theme(panel.grid.major = element_blank(),
          axis.text.y = if(nn == 1) element_text(size = 10, color = "grey20")
          else element_blank(),
          axis.text.x = element_text(vjust = 1, hjust=1,
                                     size = 10, color = "grey20"),
          strip.background.x = element_blank(),
          strip.placement = "outside",
          strip.text = if(nn != 3) element_blank(),
          strip.background = element_blank()) +
    labs(x = "",
         y = if(nn == 2) "Reliability" else "")
  
  
  reliability_plot[[nn]] = rel_plot
  
  
  
}

node_reliability = 
  reliability_plot[[1]] +
  reliability_plot[[2]] +
  reliability_plot[[3]] 

node_reliability = tree_branch / node_reliability + plot_layout(heights =  c(0.5, 1))

node_reliability

ggsave("output/reliability-grouped-rankings-farmer-processor-tree.pdf",
       node_reliability,
       width = 25,
       height = 33,
       units = "cm")

# # ............................................
# # create worth map based on the nodes from the tree
# nodes = as.integer(predict(tree, newdata = dat[keep, names(covar)], type = "node"))
# 
# nodes_ids = sort(unique(nodes))
# 
# table(nodes)
# 
# # retain only the entries used in the tree
# R_worth_map = lapply(R_sel, function(x){
#   x[keep, ]
# })
# 
# tree_worth_map = list()
# 
# for(i in seq_along(nodes_ids)) {
#   
#   k_i = nodes == nodes_ids[i]
#   
#   print(sum(k_i))
#   
#   R_i = lapply(R_worth_map, function(x){
#     x = x[k_i, ]
#     PlackettLuce(x)
#   })
#   
#   wp = 
#     worth_map(R_i,
#               labels = trait_sel,
#               na.replace = FALSE, 
#               ref = "TMEB7") +
#     
#     labs(x = if(i == 2) "Genotype" else "", 
#          y = if(i == 1) "Trait" else "") +
#     scale_fill_distiller(palette = "BrBG", 
#                          direction = 1, 
#                          na.value = "white", 
#                          name = "") +
#     theme(axis.text.y = 
#             if(i == 1) element_text(angle = 1) 
#           else element_blank(),
#           axis.text.x = element_text(angle = 65), 
#           legend.position = "none")
#   
#   tree_worth_map[[i]] = wp
#   
# }
# 
# 
# node_maps = 
#   tree_worth_map[[1]] +
#   tree_worth_map[[2]] +
#   tree_worth_map[[3]] +
#   tree_worth_map[[4]] +
#   tree_worth_map[[5]]
# 
# node_maps = tree_branch / node_maps
# 
# node_maps 
# 
# ggsave(filename = "output/pltree-overall-at-harvest-worth-map.pdf",
#        plot = node_maps,
#        units = "cm",
#        height = 30,
#        width = 40,
#        dpi = 600)
# 
# 
# 
# # ..........................................
# # ..........................................
# # Compare overall at harvest vs choice for replant ####
# base1 = which(grepl("Harvesting Fresh Roots - Overall", traitlabels))
# base2 = which(grepl("Choice For Replant", traitlabels))
# 
# R_harvest = R[[base1]]
# 
# R_replant = R[[base2]]
# 
# keep = trait_list[[base1]]$keep & trait_list[[base2]]$keep
# 
# R_harvest = R_harvest[keep, ]
# 
# R_replant = R_replant[keep,]
# 
# covar_base = covar[keep, ]
# 
# nodes = predict(tree_replant, newdata = covar_base, type = "node")
# 
# table(nodes)
# 
# node_ids = unique(nodes)
# 
# compare_plots = data.frame()
# 
# for(i in seq_along(node_ids)) {
#   
#   k = nodes == node_ids[i]
#   
#   mod_harvest = PlackettLuce(R_harvest[k, ])
#   
#   mod_replant = PlackettLuce(R_replant[k, ])
#   
#   y = coef(mod_replant, log = T)
#   x = coef(mod_harvest, log = T)
#   
#   c_i = data.frame(x, y, node = node_ids[i])
#   
#   compare_plots = rbind(compare_plots, c_i)
#   
# }
# 
# compare_plots
# 
# node_cor = split(compare_plots, compare_plots$node)
# 
# lapply(node_cor, function(x) cor(x$x, x$y))
# 
# lapply(node_cor, function(x) summary(lm(y ~ x, data = x)))
# 
# p = ggplot(data = compare_plots, 
#            mapping = aes(x = x, y = y)) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ x) +
#   facet_wrap(~ node, scales = "free") +
#   labs(y = "Choice for Replant (log-worth)",
#        x = "Overall Preference at Harvest (log-worth)") +
#   theme_bw() +
#   theme(strip.background = ggplot2::element_blank(),
#         plot.background = ggplot2::element_blank(),
#         panel.grid.major = ggplot2::element_blank(),
#         panel.grid.minor = ggplot2::element_blank(),
#         panel.border = ggplot2::element_rect(colour = "black"),
#         axis.ticks = ggplot2::element_line(colour = "black"),
#         axis.ticks.length = grid::unit(0.3, "cm")) 
# 
# p
# 
# tree_branch = gosset:::build_tree_branches(tree_replant)
# 
# ptree = tree_branch / p + plot_layout(heights =  c(1, 1))
# 
# ptree
# 
# ggsave(filename = "output/pltree-cor-overall-harvest-vs-choice-replant.pdf",
#        plot = ptree,
#        units = "cm",
#        height = 25,
#        width = 25,
#        dpi = 600)


# ..........................................
# ..........................................
# Assess the progress of overall evaluation across crop stages ####
# ....................................
# ....................................
# overall = grep("Overall", traitlabels)
# 
# traitlabels[overall]
# 
# sex = na.omit(unique(dat$sex))
# 
# overall_stages = data.frame()
# 
# # number of points to calculate
# n = 100
# # strength of distance decay for case weights in LOESS function
# d = 3
# interval0to1 = seq(from = 0 + 1/n, to = 1 - 1/n, length.out = n) 
# 
# scales = matrix(c(1, 3, 
#                   3, 6,
#                   6, 9,
#                   9, 12,
#                   12, 13,
#                   13, 14,
#                   14, 15, 
#                   15, 16,
#                   16, 17,
#                   17, 18),
#                 nrow = 10, ncol = 2, byrow = TRUE)
# 
# # scales = c(0, seq(1:length(overall)))
# 
# result = data.frame()
# 
# # i=j=k=1
# 
# # run over crop stages
# for (j in seq_along(overall)) {
#   
#   Rj = R[[overall[j]]]
#   
#   # run over genders
#   for (i in seq_along(sex)) {
#     
#     sex_i = dat$sex == sex[i] & !is.na(dat$sex)
#     
#     # subset for the gender i
#     Rji = Rj[sex_i, ]
#     
#     # get number of rows and columns (varieties)
#     n_r = dim(Rji)[1]
#     
#     n_v = dim(Rji)[2]
#     
#     # run an uniform sample over number of rows and scales 
#     # for the given crop stage
#     # this will create a smooth line to connect 
#     # points from the beginning to the end of the crop stage
#     covar = runif(n_r, scales[j], scales[j] + 1)
#     covarrescaled = (covar - min(covar)) / (max(covar) - min(covar))
#     cc = (max(covar) - min(covar)) / n
#     intervalMintoMax =  seq(from = min(covar) + cc, 
#                             to = max(covar) - cc, 
#                             length.out = n)
#     
#     # Prepare the matrix for the results
#     worth = matrix(NA, nrow = n, ncol = n_v)
#     
#     # run over the number of points to calculate
#     # using the intervals and covar scaled as weights
#     for (k in 1:n) { 
#       weights = (1 - (abs(interval0to1[k] - covarrescaled)^d))^d
#       weights = (weights / sum(weights)) * length(weights)
#       model_k = PlackettLuce(Rji, weights = weights)
#       pval = anova.PL(model_k)[2, 5]
#       worth[k, ] = log(coef(model_k, log = FALSE))
#     }
#     
#     # Create data frame 
#     dt1 = data.frame(crop_stage = traitlabels[overall[j]],
#                      covar = rep(intervalMintoMax, n_v),
#                      worth = as.vector(worth),
#                      variety = rep(names(coef(model_k)), each = n),
#                      sex = sex[i])
#     
#     result = rbind(result, dt1)
#     
#     
#   }
#   
# }
# 
# reference
# 
# abline_value = mean(result[result$crop_stage == "Final Evaluation Overall" & 
#                              result$variety == reference, "worth"])
# 
# result$variety = factor(result$variety, 
#                         levels = union(reference, unique(result$variety)))
# 
# # Draw line plot with ggplot2
# 
# table(dat$sex)
# 
# crop_progress =
#   ggplot(data = result, aes(x = covar, y = worth, color = sex)) +  
#   geom_hline(yintercept = abline_value, col = "black") +         
#   #geom_line(show.legend = T, alpha = 0.6) +
#   geom_smooth(show.legend = TRUE, se = FALSE) +
#   scale_color_manual(values = c("#4575b4", "#d73027"), name = "") +
#   scale_x_continuous(breaks = c(1, 3, 6, 9, 12, 15, 18)) +
#   facet_wrap(~ variety) +
#   labs(x = "Crop stage (months after planting)",
#        y = "log(Worth)") +
#   theme_classic() +
#   theme(legend.position = "bottom")
# 
# crop_progress
# 
# ggsave("output/crop-stages-overall.pdf",
#        plot = crop_progress,
#        width = 25,
#        height = 23, 
#        units = "cm")
# 
# 
# 
# 
# 
