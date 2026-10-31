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
library("patchwork")
library("magrittr")
library("tidyverse")
library("reshape2")
library("heatmap3")
library("corpcor")
library("readxl")
library("janitor")
library("ggrepel")
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

names(dat)


genotype = read_excel("data/genotype-information.xlsx")

names(genotype) = make_clean_names(names(genotype))

pack_index = paste0("variety", letters[1:3])

itemnames = sort(unique(unlist(dat[, pack_index])))

list.files("output", full.names = TRUE)

trait_cor = read.csv("output/kendall-partial-correlation-backward-selection.csv" )

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

trait_sel = trait_cor$trait

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

traitlabels = fix_trait_labels(traits)

# keep traits selected in the previous script 
# from backward selection
trait_list = getTraitList(dat,
                          pattern = c("_best", "_worst"),
                          trait.labels = traitlabels)

keep = traitlabels %in% c("Final Evaluation - Choice For Replant",
                          "Harvesting - Overall",
                          trait_sel)

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


# ...................................
# ...................................
# PlackettLuce tree ####

base = grep("Choice For Replant", traitlabels)

R = lapply(trait_list, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})


G_base = R[[base]]

keep = trait_list[[base]]$keep

G_base = G_base[keep, ]

covar_base = covar[keep, ]

# make the tree
G_base = group(G_base, index = 1:length(G_base))

pld = cbind(G = G_base, covar_base)

nrow(pld)

tree_replant = pltree(G ~ Processor + GenderFarmer, 
                      data = pld,
                      minsize = 50,
                      npseudo = 30,
                      gamma = TRUE,
                      alpha = 0.05)

covar$GenderFarmer

covar$Processor[covar$Processor == "Not specified"] = "Other Person"

covar$node = as.integer(predict(tree_replant,
                                type = "node", newdata = covar))

node_rules(tree_replant)

labs = c("Men farmers, outside processor", 
         "Men farmers, self or family member processing",
         "Women farming and processing")

covar$Cluster = factor(covar$node, labels = labs)

# ..............................
# ..............................
# PCA plot  ####
# get list of released varieties
genotype

reference = "TMEB2"

released = genotype$year_of_release != "NR"

released = genotype[released, ]

released = released[order(released$year_of_release), ]

released$final_name = factor(released$genotype,
                             levels = union(reference, released$genotype))

# resample 
labels = traitlabels

# test likelihood ratio
# this will check whether the ranks are significantly distinct 
# from each group
llr = lapply(R, function(x){
  likelihood_ratio(x, split = covar$Cluster)
})

llr = do.call("rbind", llr)

llr$trait = labels

llr

write.csv(llr, "output/likelihood-ratio-clusters.csv")

# keep only traits with distinct rankings
#keep_traits = which(llr[,3] < 0.05)
keep_traits = 1:length(trait_list)

labels = traitlabels[keep_traits]

mod = lapply(R[keep_traits], PlackettLuce)

coeffs = lapply(mod, function(x){
  resample(x, bootstrap = TRUE, seed = 1621, n1 = 50)
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = labels

# add name of features
#names(winprobs) = labels

pc = princomp(coeffs[-1], cor = FALSE)

pcplot = plot_pca(pc, scale = 1)

ggsave("output/biplot-performance-all-traits.pdf",
       plot = pcplot,
       height = 25,
       width = 25,
       units = "cm")


# ...........................................
# ...........................................
# PCA segmented by gender and regions ####
# first fit a PL tree to see if gender influences 
# variety performance
gender_class = table(covar$Cluster)
gender_class
plots_gender = list()

for (i in seq_along(gender_class)) {
  
  R_subset = lapply(R[keep_traits], function(x){
    x = x[covar$Cluster == names(gender_class[i])]
    na.omit(x)
  })
  
  mod = lapply(R_subset, PlackettLuce)
  
  # get probabilities
  coeffs = lapply(mod, function(x){
    resample(x, bootstrap = TRUE, seed = 1622, n1 = 50) 
  })
  
  coeffs = do.call(cbind, coeffs)
  
  rmv = which(names(coeffs) == "item")[-1]
  
  coeffs = coeffs[-rmv]
  
  names(coeffs)[-1] = labels
  
  pc = princomp(coeffs[, -1], cor = TRUE)
  
  pcplot = plot_pca(pc, scale = 3) + 
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(gender_class[i]),
                        ", n = ",
                        as.integer(gender_class[i])))
  
  plots_gender[[i]] = pcplot
  
}

p = plots_gender[[1]] + plots_gender[[2]] + plots_gender[[3]]

ggsave("output/biplot-trait-performance-gender.pdf",
       plot = p,
       height = 17,
       width = 50,
       units = "cm")



