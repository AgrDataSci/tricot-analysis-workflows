# .......................................
# .......................................
# This script will compare overall performance 
# when cassava roots were harvested and after processing 
library("PlackettLuce")
library("gosset")
library("tidyverse")
library("ggplot2")
library("ggrepel")
# add extra functions
source("script/helper_01_functions.R")

sessioninfo::session_info()

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/04_compare_overall_performance_harvest_vs_postharvest.txt")


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




trait1 = "harvesting_fresh_roots_overallimpression"

trait2 = "final_evaluation_choiceforreplant"

keep = trait_list[[trait1]]$keep & trait_list[[trait2]]$keep

#keep = as.vector(keep & dat$processor == "Other Person")

# make rankings
OAH = rank_tricot(dat[keep, ],
                   items = itemnames,
                   input = c(trait_list[[trait1]]$index_pos, 
                             trait_list[[trait1]]$index_neg))


OAP = rank_tricot(dat[keep, ],
                   items = itemnames,
                   input = c(trait_list[[trait2]]$index_pos,
                             trait_list[[trait2]]$index_neg))


modOAH = PlackettLuce(OAH)

modOAP = PlackettLuce(OAP)

wholedata = compare(modOAP, 
                     modOAH) +
  labs(title = "Whole dataset",
       x = "Average log-worth",
       y = "Difference (food product - harvest)")

wholedata


# ..................
# now sub-setting of using those that did not process 
keep = trait_list[[trait1]]$keep & trait_list[[trait2]]$keep
keep = as.vector(keep & dat$processor == "Other Person")

table(keep)

# make rankings
OAH = rank_tricot(dat[keep, ],
                   items = itemnames,
                   input = c(trait_list[[trait1]]$index_pos, 
                             trait_list[[trait1]]$index_neg))


OAP = rank_tricot(dat[keep, ],
                   items = itemnames,
                   input = c(trait_list[[trait2]]$index_pos,
                             trait_list[[trait2]]$index_neg))


modOAH = PlackettLuce(OAH)

modOAP = PlackettLuce(OAP)

subsetdata = compare(modOAP, 
                     modOAH) +
  labs(title = "Different farmer and processor",
       x = "Average log-worth",
       y = "Difference (food product - harvest)")


ggsave("output/selection_agreement_plot_whole_data.png",
       plot = wholedata,
       width = 20,
       height = 20,
       dpi = 400,
       units = "cm")

ggsave("output/selection_agreement_plot_subset_data.png",
       plot = subsetdata,
       width = 20,
       height = 20,
       dpi = 400,
       units = "cm")

