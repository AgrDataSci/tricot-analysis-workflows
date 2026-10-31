# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")
library("magrittr")
library("tidyverse")
library("reshape2")
library("heatmap3")
library("corpcor")
library("GeneNet")
library("readxl")
library(ggrepel)
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

genotype_info = read_xlsx("data/groundnut-genotype-release-information.xlsx")

dat = read.csv("data/groundnut-tanzania-tricot.csv")

# ..................................
# get summaries ####
traits = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(traits, function(x){
  x$trait_label
}))

traits

traits = gsub("_qst_|_|tricot", " ", traits)
traits = ClimMobTools:::.title_case(traits)
traits = gsub("Podyield", "Pod yield", traits)
traits = gsub("Haulmyield", "Haulm yield", traits)
traits = gsub("yield", "Yield", traits)
traits = gsub("Grainsize", "Grain Size", traits)
traits = gsub("Graincolour", "Grain Color", traits)
traits = gsub("content", " Content", traits)
traits = gsub("resistance", " Resistance", traits)
traits = gsub("tolerance", " Tolerance", traits)
traits = gsub("labour", " Labor", traits)
traits = gsub("Physiologicalmaturity ", "", traits)
traits = gsub("Agronomicperformance ", "", traits)
traits = gsub("Reproductive Diseases Resistance", "Diseases Resistance [Reproductive]", traits)
traits = gsub("Reproductive Vigor", "Vigor [Reproductive]", traits)
traits = gsub(" Overall", "", traits)
traits = gsub("Vegetative ", "", traits)
traits = gsub("Reproductive ", "", traits)
traits

out = !traits %in% c("Vegetative", "Reproductive", "Flowering")

traitlist = getTraitList(dat, c("_pos", "_neg"), trait.labels = traits)

traits = traits[out]

traitlist = traitlist[out]

pack_index = paste0("package_item_", letters[1:3])

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames

# PlackettLuce rankings
traits

reference = which(traits == "Overall")

dat$gender = ifelse(dat$registration_gender == "Prefer Not To Respond", NA,
                    dat$registration_gender)

dat = dat[!is.na(dat$gender), ]

dat = dat[!is.na(dat$ecoregion), ]

cor_backward = read.csv("output/kendall-partial-correlation-backward-selection.csv")

cor_backward = cor_backward[order(cor_backward$cor), ]

cor_backward$trait = factor(cor_backward$trait, 
                            levels = unique(cor_backward$trait))

head(cor_backward)

# remove two traits with least Kendall correlation
keep = cor_backward$cor > 0.3

cor_backward = cor_backward[keep, ]

traits = c("Overall", traits[traits %in% cor_backward$trait])

traits = unlist(lapply(traitlist, function(x){
 x$trait_label
}))

keep = traits %in% c(as.character(cor_backward$trait), "Overall")

traits = traits[keep]

traitlist = traitlist[keep]

do.call("rbind", lapply(traitlist, function(x) cbind(x$trait_label, sum(x$keep))))

# ..............................
# ..............................
# Worth map  ####
reference = "Naliendele 09"

# get list of released varieties
released = genotype_info$release_year != "Not released"

released = genotype_info[released, ]

released = released[!duplicated(released$final_name), ]

released = released[order(released$release_year), ]

# released = released[released$genotype %in% sel, ] 

released$final_name = factor(released$final_name,
                             levels = union(reference, released$final_name))

R = lapply(traitlist, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})

mod = lapply(R, PlackettLuce)

# PCA with log-worth  ####
itemnames

labels = traitlabels =  traits

# PL model
mod = lapply(R, PlackettLuce)

coeffs = lapply(mod, function(x){
  resample(x, bootstrap = TRUE, seed = 1008, n1 = 200)
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = labels

# select released materials 
sel = coeffs$item %in% released$final_name

coeffs = coeffs[sel, ]

pc = princomp(coeffs[-1], cor = FALSE)

pcplot = plot_pca(pc)

pcplot

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
gender_class = table(dat$ecoregion)
gender_class
plots_gender = list()

for (i in seq_along(gender_class)) {
  
  R_subset = lapply(R, function(x){
    x = x[dat$ecoregion == names(gender_class[i])]
    na.omit(x)
  })
  
  R_subset
  
  mod = lapply(R_subset, PlackettLuce)
  
  # get probabilities
  coeffs = lapply(mod, function(x){
    resample(x, bootstrap = TRUE, seed = 1424, n1 = 200) 
  })
  
  coeffs = do.call(cbind, coeffs)
  
  rmv = which(names(coeffs) == "item")[-1]
  
  coeffs = coeffs[-rmv]
  
  names(coeffs)[-1] = labels
  
  sel = coeffs$item %in% released$final_name
  
  coeffs = coeffs[sel, ]
  
  pc = princomp(coeffs[, -1], cor = TRUE)
  
  pcplot = plot_pca(pc, scale = 6) + 
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(gender_class[i]),
                        ", n = ",
                        as.integer(gender_class[i])))
  
  pcplot
  
  plots_gender[[i]] = pcplot
  
}

p = plots_gender[[1]] + plots_gender[[2]] + 
  plots_gender[[3]] + plots_gender[[4]] +
  plots_gender[[5]] + plots_gender[[6]]

p

ggsave("output/biplot-trait-performance-gender.pdf",
       plot = p,
       height = 30,
       width = 40,
       units = "cm")





# winprobs = winprobs[sel, ]
# 
# winprobs = data.frame(items = rep(dimnames(winprobs)[[1]], 
#                                   times = ncol(winprobs)),
#                       labels = rep(dimnames(winprobs)[[2]], 
#                                    each = nrow(winprobs)),
#                       winprob = as.numeric(unlist(winprobs)))
# 
# winprobs$labels = factor(winprobs$labels, levels = traits)
# 
# winprobs$items = factor(winprobs$items, levels = levels(released$final_name))
# 
# lims = max(abs(winprobs$winprob)) * c(-1, 1)
# 
# worth_m = ggplot(winprobs, 
#            aes(x = items, 
#                y = labels,
#                fill = winprob,
#                label = as.character(round(winprob, 2)))) +
#   geom_tile() + 
#   scale_fill_distiller(palette = "BrBG", 
#                        limit = lims,
#                        direction = 1,
#                        na.value = "white",
#                        name = "") +
#   theme_minimal() +
#   theme(axis.text = element_text(color = "grey20"),
#         strip.text.x = element_text(color = "grey20"),
#         axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1),
#         axis.text.y = element_text(angle = 1, vjust = 1, hjust = 1),
#         panel.grid = element_blank()) +
#   labs(x = "", 
#        y = "",
#        fill = "")
# 
# 
# ggsave("output/worth-map-groundnut.pdf",
#        plot = worth_m,
#        height = 12,
#        width = 16,
#        units = "cm")
# 
# # get winprobs again 
# winprobs = gosset:::.combine_coeffs(mod, 
#                                     log = TRUE,
#                                     vcov = FALSE)
# 
# unique(rownames(winprobs))
# 
# labels = c("Harvest labor", "Pod yield", "Grain yield" ,
#            "Oil content", "Grain size", "Grain color",
#            "Marketability", "Overall preference" ) 
# 
# # add name of features
# names(winprobs) = labels
# 
# sel = rownames(winprobs) %in% released$final_name
# 
# winprobs = winprobs[sel, ]
# 
# pc = princomp(winprobs, cor = FALSE)
# 
# pcplot = factoextra::fviz_pca_biplot(pc, 
#                                      repel = TRUE,
#                                      title = "")
# 
# pcplot
# 
# ggsave("output/biplot-groundnut-performance-all-traits.pdf",
#        plot = pcplot,
#        height = 18,
#        width = 18,
#        units = "cm")
# 
# # ...........................................
# # ...........................................
# # PCA segmented by gender and regions ####
# # first fit a PL tree to see if gender influences 
# # variety performance
# reference_trait_index = grep("Overall", traits)
# 
# #G = rank_tricot traitlist[[reference_trait_index]]
# 
# G = R[[reference_trait_index]]
# 
# G = group(G, index = 1:length(G))
# 
# pld = cbind(G, dat[, c("gender", "ecoregion")])
# 
# head(pld)
# 
# pltree(G ~ ecoregion, minsize = 50, data = pld, alpha = 1, gamma = TRUE)
# 
# gender = table(dat$gender)
# region = table(dat$ecoregion)
# 
# for (i in seq_along(region)) {
#   
#   R_subset = lapply(R, function(x){
#     x[dat$ecoregion == names(region[i])]
#   })
#   
#   mod = lapply(R_subset, PlackettLuce)
#   
#   # get winprobs again 
#   winprobs = gosset:::.combine_coeffs(mod, 
#                                       log = TRUE,
#                                       vcov = FALSE)
#   
#   unique(rownames(winprobs))
#   
#   labels = c("Harvest labor", "Pod yield", "Grain yield" ,
#              "Oil content", "Grain size", "Grain color",
#              "Marketability", "Overall preference" ) 
#   
#   # add name of features
#   names(winprobs) = labels
#   
#   sel = rownames(winprobs) %in% released$final_name
#   
#   winprobs = winprobs[sel, ]
#   
#   pc = princomp(winprobs, cor = FALSE)
#   
#   pcplot = factoextra::fviz_pca_biplot(pc, 
#                                        repel = TRUE,
#                                        title = paste0(names(region[i]),
#                                                       " Region, n = (",
#                                                       as.integer(region[i]), ")"))
#   
#   ggsave(paste0("output/biplot-groundnut-performance-all-traits-", 
#                 tolower(names(region[i])), ".pdf"),
#          plot = pcplot,
#          height = 18,
#          width = 18,
#          units = "cm")
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
