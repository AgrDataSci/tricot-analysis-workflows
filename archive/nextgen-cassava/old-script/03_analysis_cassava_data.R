# .......................................
# .......................................
# PlackettLuce tree with NexGen cassava data 
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
# add extra functions
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/03_analysis_pltree.txt")

dir.create("output", recursive = TRUE, showWarnings = FALSE)
# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
list.files("data")
dat = read.csv("data/nextgen_cassava_tricot_with_covar.csv")

# get the list of traits
trait_list = getTraitList(dat, pattern = c("_best", "_worst"))

traits = lapply(trait_list, function(x){
  x$trait_label
})

traits = unlist(traits)

traits

traitlabels = traits
traitlabels
traitlabels = gsub("_qst_|_|tricot", " ", traitlabels)
traitlabels = ClimMobTools:::.title_case(traitlabels)
traitlabels = gsub("Finalevaluationoverallimpression", "Overall", traitlabels)
traitlabels = gsub("Overallimpression", "Overall", traitlabels)
traitlabels = gsub("overallimpression", "Overall", traitlabels)
traitlabels = gsub("Storage", "Storage ", traitlabels)
traitlabels = gsub("Preparation", "Preparation ", traitlabels)
traitlabels = gsub("resistance", "Resistance", traitlabels)
traitlabels = gsub("tolerance", "Tolerance", traitlabels)
traitlabels = gsub("colour", "Colour", traitlabels)
traitlabels = gsub("yield", "Yield", traitlabels)
traitlabels = gsub("content", "Content", traitlabels)
traitlabels = gsub("Weedcompetitivenesscanopyformation", "WeedCompetitiveness", traitlabels)
traitlabels = gsub("Survivalsproutingnrofplantsleft", "SurvivalSprouting", traitlabels)
traitlabels = gsub("Survivalsprouting", "SurvivalSprouting", traitlabels)
traitlabels = gsub("Branchinghabitarchitectureoftheplant", "BranchingHabit", traitlabels)
traitlabels = gsub("ebaqualitymouldability", "Mouldability", traitlabels)
traitlabels = gsub("notstickinesstohand Mouth", "NotStickiness", traitlabels)
traitlabels = gsub("swellingofgari", "Swelling", traitlabels)
traitlabels = gsub("notstickiness", "NotStickiness", traitlabels)
traitlabels = gsub("Woodyfilament", "WoodyFilament", traitlabels)
traitlabels = gsub("Gariqualitydryness", "GariDryness", traitlabels)
traitlabels = gsub("Waterreleaseduringpressing", "WaterRelease", traitlabels)
traitlabels = gsub("Mashpulpdiscolor", "MashPulpDiscolor", traitlabels)
traitlabels = gsub("Gariprocessingeaseofpeeling", "EasePeeling", traitlabels)
traitlabels = gsub("Suitabilitytosoilandenvironment", "SuitabilitySoil", traitlabels)
traitlabels = gsub("Germinationsprouting", "Sprouting", traitlabels)
traitlabels = gsub("DensitywaterContent", "WaterContent", traitlabels)
traitlabels = gsub("Swellingofmash", "SwellingMash", traitlabels)
traitlabels = gsub("Choiceforreplant", "ChoiceReplant", traitlabels)
traitlabels = ClimMobTools:::.title_case(traitlabels)
traitlabels = gsub("  ", " ", traitlabels)

traitlabels

trait_list = getTraitList(dat,
                          pattern = c("_best", "_worst"),
                          trait.labels = traitlabels)

# ..............................
# ..............................
# Covariates ####
# ..............................
# ..............................
names(dat)
covars = c("sex", "processor", "other_processor_gender", "region", 
           "crop_div", "score_hdds_bad_season", "ppi_likelihood", 
           "gender_male_control", "h_hsize_mae", "land_owned",
           "household_type")

newnamescovars = c("GenderFarmer", "Processor", "GenderProcessor", "Region",
                   "CropDiv", "HDDSBadSeason","PPI", "GenderMaleControl",
                   "HHSizeMAE", "LandOwned", "HouseholdType")

covar = dat[, covars]

names(covar) = newnamescovars 

# update gender of processor, using information from the trial 
covar$GenderProcessor = ifelse(covar$Processor  == "Self",
                               covar$GenderFarmer, 
                               covar$GenderProcessor)

table(covar$GenderProcessor)

covar$GenderProcessor[is.na(covar$GenderProcessor)] = "Woman"

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
# Check covariates #
names(covar)

table(covar$GenderFarmer)
table(covar$GenderProcessor)
table(covar$GenderFarmer, covar$Processor)

plot(density(covar$CropDiv))

plot(density(covar$HDDSBadSeason))

plot(density(covar$GenderMaleControl))

table(covar$Processor, covar$HouseholdType)

table(covar$PPI)

plot(density(covar$LandOwned))

boxplot(covar$LandOwned)

#.....................................
#.....................................
# Kendall cor ####
# an index for the packages 
pack_index = paste0("variety", letters[1:3])

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames

reference = "TMEB693"

# PlackettLuce rankings
R = lapply(trait_list, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})


traitlabels

# the first approach will use the whole rankings
base = which(grepl("ChoiceReplant", traitlabels))

# choice for replant as baseline
kendall = lapply(R[-base], function(x){
  kendallTau(R[[base]], x)
})

kendall = do.call("rbind", kendall)

kendall$trait = traitlabels[-base]

keep = !grepl(" Overall", kendall$trait)

kendall = kendall[keep, ]

kendall = kendall[rev(order(kendall$kendallTau)), ]

kendall

# the other approach uses the PL coefficients
mod = lapply(R, PlackettLuce)

# compute kendall correlation
kendall2 = lapply(mod[-base], function(x){
  xx = coefficients(mod[[base]], log = FALSE)
  yy = coefficients(x, log = FALSE)
  kendallTau(xx, yy)
})

kendall2 = do.call("rbind", kendall2)

kendall2$trait = traitlabels[-base]

keep = !grepl(" Overall", kendall2$trait)

kendall2 = kendall2[keep, ]

kendall2 = kendall2[rev(order(kendall2$kendallTau)), ]

kendall2

write.csv(kendall, "output/kendall-cor-from-rankings.csv",
          row.names = FALSE)

write.csv(kendall2, "output/kendall-cor-from-PL-coefficients.csv",
          row.names = FALSE)


#.....................................
#.....................................
# Choice for replant based on processor and farmer gender ####
# an index for the packages 
traitlabels

base = which(grepl("ChoiceReplant", traitlabels))

R_base = R[[base]]

R_base = R_base[trait_list[[base]]$keep, ]

covar_base = covar[trait_list[[base]]$keep, ]

split_rule = paste(covar_base$GenderFarmer, 
                   covar_base$Processor, 
                   #covar_base$GenderProcessor, 
                   sep = " - ")

table(split_rule)

split_rule = as.factor(split_rule)

split_levels = levels(split_rule)

split_rule = as.integer(split_rule)

mod_base = PlackettLuce(R_base)

anova(mod_base)

llratio = likelihood_ratio(R_base, split_rule)

capture.output(print(llratio),
               file = "output/ll-ratio-choice-replant-farmer-processor.txt")


# make the tree
G_base = group(R_base, index = 1:length(R_base))

pld = cbind(G = G_base, covar_base)

tree_replant = pltree(G ~ Processor + GenderFarmer, 
                      data = pld,
                      minsize = 20,
                      npseudo = 30,
                      gamma = TRUE,
                      alpha = 0.05)

tree = tree_replant

node_rules(tree)

# run this for the condensended rankings using predicted nodes
gosset::node_rules(tree)

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
                                       ci.level = 0.1)

ptree = tree_branch / tree_nodes + plot_layout(heights =  c(0.5, 1))

ptree

ggsave(filename = "output/pltree-choice-replant-farmer-processor.pdf",
       plot = ptree,
       units = "cm",
       height = 25,
       width = 25,
       dpi = 600)


#.....................................
#.....................................
# Overall performance at harvest ####
# an index for the packages 
traitlabels

base = which(grepl("Harvesting Fresh Roots Overall", traitlabels))

R_base = R[[base]]

R_base = R_base[trait_list[[base]]$keep, ]

covar_base = covar[trait_list[[base]]$keep, ]

plot(density(covar_base$GenderMaleControl))

split_rule = paste(covar_base$GenderMaleControl > 0.9)

table(split_rule)

split_rule = as.factor(split_rule)

split_levels = levels(split_rule)

split_rule = as.integer(split_rule)

mod_base = PlackettLuce(R_base)

anova(mod_base)

llratio = likelihood_ratio(R_base, split_rule)

capture.output(print(llratio),
               file = "output/ll-ratio-overall-at-harvest.txt")

# make the tree
G_base = group(R_base, index = 1:length(R_base))

pld = cbind(G = G_base, covar_base)

head(pld)

tree_harvest = pltree(G ~ ., 
                      data = pld,
                      minsize = 60,
                      npseudo = 20,
                      gamma = TRUE,
                      alpha = 0.05)


tree = tree_harvest

deviance(tree)

plot(tree, log = TRUE, ci.level = 0.05)

node_rules(tree)

gosset::node_rules(tree)

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

ptree = tree_branch / tree_nodes + plot_layout(heights =  c(0.5, 1))

ptree

ggsave(filename = "output/pltree-overall-preference-at-harvest.pdf",
       plot = ptree,
       units = "cm",
       height = 25,
       width = 25,
       dpi = 600)

# ..........................................
# ..........................................
# Compare overall at harvest vs choice for replant ####
base1 = which(grepl("Harvesting Fresh Roots Overall", traitlabels))
base2 = which(grepl("ChoiceReplant", traitlabels))

R_harvest = R[[base1]]

R_replant = R[[base2]]

keep = trait_list[[base1]]$keep & trait_list[[base2]]$keep


R_harvest = R_harvest[keep, ]

R_replant = R_replant[keep,]

covar_base = covar[keep, ]

nodes = predict(tree_replant, newdata = covar_base, type = "node")

table(nodes)

node_ids = unique(nodes)

compare_plots = data.frame()

for(i in seq_along(node_ids)) {
  
  k = nodes == node_ids[i]

  mod_harvest = PlackettLuce(R_harvest[k, ])
  
  mod_replant = PlackettLuce(R_replant[k, ])
  
  y = coef(mod_replant, log = T)
  x = coef(mod_harvest, log = T)
  
  c_i = data.frame(x, y, node = node_ids[i])
  
  compare_plots = rbind(compare_plots, c_i)
  
}

compare_plots

node_cor = split(compare_plots, compare_plots$node)

lapply(node_cor, function(x) cor(x$x, x$y))

lapply(node_cor, function(x) summary(lm(y ~ x, data = x)))

p = ggplot(data = compare_plots, 
           mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ node, scales = "free") +
  labs(y = "Choice for Replant (log-worth)",
       x = "Overall Preference at Harvest (log-worth)") +
  theme_bw() +
  theme(strip.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "black"),
        axis.ticks = ggplot2::element_line(colour = "black"),
        axis.ticks.length = grid::unit(0.3, "cm")) 

p

tree_branch = gosset:::build_tree_branches(tree_replant)

ptree = tree_branch / p + plot_layout(heights =  c(1, 1))

ptree

ggsave(filename = "output/pltree-cor-overall-harvest-vs-choice-replant.pdf",
       plot = ptree,
       units = "cm",
       height = 25,
       width = 25,
       dpi = 600)


# ..........................................
# ..........................................
# Assess the progress of overall evaluation across crop stages ####
base = which(grepl("Overall", traitlabels))

traitlabels[base]








