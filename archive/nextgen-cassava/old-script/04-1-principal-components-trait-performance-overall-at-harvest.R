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

trait_cor = read.csv("output/kendall-partial-correlation-backward-selection-overall-at-harvest.csv")

keep = !grepl("Overall", trait_cor$trait)

trait_cor = trait_cor[keep, ]

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

keep = traitlabels %in% c("Harvesting - Overall", 
                          "Final Evaluation - Choice For Replant",
                          trait_sel)
 
trait_list = trait_list[keep]
 
traitlabels = traitlabels[keep]

# ..............................
# ..............................
# Worth map  ####
# get list of released varieties
genotype

reference = "TMEB2"

released = genotype$year_of_release != "NR"

released = genotype[released, ]

released = released[order(released$year_of_release), ]

released$final_name = factor(released$genotype,
                             levels = union(reference, released$genotype))

R = lapply(trait_list, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})

mod = lapply(R, PlackettLuce)

winprobs = gosset:::.combine_coeffs(mod, 
                                    log = TRUE,
                                    vcov = FALSE,
                                    ref = reference)

unique(rownames(winprobs))

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

ggplot(winprobs, 
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


ggsave("output/worth-map-cassava-overall-at-harvest.pdf",
       plot = last_plot(),
       height = 12,
       width = 20,
       units = "cm")

# get winprobs again 
winprobs = gosset:::.combine_coeffs(mod, 
                                    log = TRUE,
                                    vcov = FALSE)

unique(rownames(winprobs))

traitlabels

labels = c("Fresh root yield", "Water content",
           "Root size", "Root shape",
           "Woody filament", "Overall preference",
           "Preference for replanting")

# add name of features
names(winprobs) = labels

sel = rownames(winprobs) %in% released$final_name

winprobs = winprobs[sel, ]

pc = princomp(winprobs, cor = FALSE)

pcplot = factoextra::fviz_pca_biplot(pc, 
                                     repel = TRUE,
                                     title = "")

pcplot

ggsave("output/biplot-cassava-performance-overall-at-harvest.pdf",
       plot = pcplot,
       height = 20,
       width = 20,
       units = "cm")






