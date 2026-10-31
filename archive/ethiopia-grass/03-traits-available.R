library("tidyverse")
library("janitor")
library("ggrepel")
library("tidyverse")
library("patchwork")
library("magrittr")
library("ggfortify")
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("readxl")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

dat = read.csv("data/tricot-data-ethiopiagrass.csv")

pack_index = paste0("package_item_", LETTERS[1:3])

table(unlist(dat[pack_index]))

# check availability of traits
trait_list = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(trait_list, function(x){
  x$trait_label
}))

sort(traits)

keep = lapply(trait_list, function(x) sum(x$keep))

keep = unlist(keep) > 100

trait_list = trait_list[keep]

traits = traits[keep]

trait_available = lapply(trait_list, function(x){
  z = data.frame(N = x$keep, region = dat$region)
  table(z$N, z$region)[2,]
})

trait_available = do.call("rbind", trait_available)

trait_available = cbind(trait_available, N = rowSums(trait_available))

trait_available = as.data.frame(trait_available)

trait_available$trait = traits

trait_available

traits

traits = gsub("secondharvest_", "2nd harvest ", traits)
traits = gsub("firstharvest_", "1st harvest ", traits)
traits = gsub("germination_", "germination ", traits)
traits = gsub("tricot", "", traits)
traits = gsub("colour", " colour", traits)
traits = gsub("height", " height", traits)
traits = gsub("population", " population", traits)
traits = gsub("requirement", " requirement", traits)
traits = gsub("resistance", " resistance", traits)
traits = gsub("feeding", " feeding", traits)
traits = gsub("tolerance", " tolerance", traits)
traits = gsub("stem", " stem ratio", traits)
traits = gsub("cover", " cover", traits)
traits = gsub("envcobenefits", "environmental co-benefits", traits)
traits = gsub("harvesting", "easy to harvest", traits)
sort(traits)

traitlabels = ClimMobTools:::.title_case(traits)

trait_available$trait_name = traitlabels

trait_available

# write.csv(trait_available, "data/traits-available.csv")