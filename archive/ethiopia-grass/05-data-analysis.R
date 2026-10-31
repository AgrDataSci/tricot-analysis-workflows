#..........................................................
#..........................................................
#..........................................................
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

#..........................................................
#..........................................................
#..........................................................
# Data ####
list.files("data")

dat = read.csv("data/tricot-data-ethiopiagrass.csv")

clust = read.csv("data/farmers-clusters.csv")

weights = read.csv("data/traits-available-tf-idf.csv")

pack_index = paste0("package_item_", LETTERS[1:3])

table(unlist(dat[pack_index]))

weights = na.omit(weights)

weights = weights[!duplicated(weights$trait), ] 

weights$w = weights$tf / sum(weights$tf)

weights$string = gsub("germination_|firstharvest_|secondharvest_", "", weights$string)

clust = clust[!duplicated(clust$block_id), ]

clust = clust[!is.na(clust$block_id), ]

dat = merge(dat, clust, by = "block_id", all.x = TRUE)

#..........................................................
#..........................................................
#..........................................................
# Organize traits ####
# traits will be grouped as a single trait
# check availability of traits
trait_list = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(trait_list, function(x){
  x$trait_label
}))

keep = grep(paste(weights$string, collapse = "|"), traits)

trait_list = trait_list[keep]

traits = traits[keep]

traits

traitlabels = gsub("secondharvest_", "2nd harvest ", traits)
traitlabels = gsub("firstharvest_", "1st harvest ", traitlabels)
traitlabels = gsub("fourthharvest_", "4th harvest ", traitlabels)
traitlabels = gsub("thirdharvest_", "3rd harvest ", traitlabels)
traitlabels = gsub("germination_", "germination ", traitlabels)
traitlabels = gsub("tricot", "", traitlabels)
traitlabels = gsub("colour", " colour", traitlabels)
traitlabels = gsub("height", " height", traitlabels)
traitlabels = gsub("population", " population", traitlabels)
traitlabels = gsub("requirement", " requirement", traitlabels)
traitlabels = gsub("resistance", " resistance", traitlabels)
traitlabels = gsub("feeding", " feeding", traitlabels)
traitlabels = gsub("tolerance", " tolerance", traitlabels)
traitlabels = gsub("stem", " stem ratio", traitlabels)
traitlabels = gsub("cover", " cover", traitlabels)
traitlabels = gsub("envcobenefits", "environmental co-benefits", traitlabels)
traitlabels = gsub("harvesting", "easy to harvest", traitlabels)
traitlabels = gsub("  ", " ", traitlabels)

traitlabels = ClimMobTools:::.title_case(traitlabels)

traitlabels

itemnames = unique(sort(unlist(dat[pack_index])))

#..........................................................
#..........................................................
#..........................................................
# PlackettLuce with aggregated rankings ####
R = list()

for(i in seq_along(weights$string)) {
  
  print(weights$string[i])
  
  index = grep(weights$string[i], traits)
  
  indicesbase = 1:nrow(dat)
  
  RG = lapply(trait_list[index], function(x){
    rank_tricot(items = pack_index,
                input = x$string,
                data = dat,
                validate.rankings = TRUE)
  })
  
  RG = do.call("rbind", RG)
  
  RG = group(RG, index = rep(1:nrow(dat), length(index)))
  
  R[[i]] = RG
  
}

traits = weights$string
traitlabels =  weights$trait

# ........................................
# .......................................
# ANOVA  ####
# this will be used to test whether the varieties are different 
# from each other and we will retain the ones with alpha > 0.05
# get the reference trait
ov = grep("overall", weights$string)

pdf(file = "output/trial-network.pdf",
    width = 9,
    height = 9)
plot(network(R[[ov]]))
dev.off()

anov = lapply(R, function(x){
  a = anova(PlackettLuce(x))
  a[2, ]
})

anov = do.call("rbind", anov)

anov[,1] = traitlabels

anov

write.csv(anov, 
          "output/anova.csv")

# ........................................
# ........................................
# Calculate PL model  ####
ref = "Desho"

mod = lapply(R, PlackettLuce)

worth_map(mod, labels = traitlabels, labels.order = rev(traitlabels)) 

worth = 
  worth_map(mod, labels = traitlabels) +
  labs(x = "", y = "") +
  theme(axis.text.y = element_text(vjust = 0.5, 
                                   hjust = 1, 
                                   angle = 0)) 

ggsave("output/worth-map-trait-performance.pdf",
       plot = worth,
       height = 15,
       width = 30,
       units = "cm")

# ........................................
# ........................................
# Apply selection index  ####
coeffs = lapply(mod, function(x){
  resample(x, bootstrap = TRUE, seed = 1424, n1 = 10, log = FALSE) 
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = traitlabels

for (w in seq_along(weights$trait)) {
  coeffs[, weights$trait[w]] = coeffs[,weights$trait[w]] * weights$w[w]
}

coeffs$index = rowSums(coeffs[2:ncol(coeffs)])

coeffs$subset = "All"

lvls = unique(coeffs$item[order(coeffs$index)])

coeffs$item = factor(coeffs$item, levels = lvls)

ggplot(coeffs, aes(y = item, x = index,
                        fill = subset, color = subset)) +
    geom_boxplot(alpha = 0.2) +
    theme_minimal() +
    #scale_fill_manual(values = c("grey50","#225ea8", "#e31a1c")) +
    #scale_color_manual(values = c("grey50","#225ea8", "#e31a1c")) +
    labs(y = "", x = "Selection index") +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          text = element_text(colour = "grey20"))


# ........................................
# ........................................
# Split the data in subsets  ####
# do it by region
dat$region[dat$region == "Sidama"] = "SNNP"

split = dat$region

group_split = table(split)

group_split

# Likelihood ratio will check whether the ranks are significantly distinct 
# from each gender
llr = lapply(R, function(x){
  likelihood_ratio(x, split = split)
})

llr = do.call("rbind", llr)

llr$trait = traitlabels

llr

write.csv(llr, 
          "output/likelihood-ratio-region.csv")

# PCA and worth map
n_points = 10
worth_plot = list()

for (i in seq_along(group_split)) {
  
  # run the model 
  R_subset = lapply(R, function(x){
    x = x[split == names(group_split[i])]
    x = na.omit(x)
    x
  })
  
  mod_i = lapply(R_subset, PlackettLuce)
  
  # selection index
  coeffs_i = lapply(mod_i, function(x){
    resample(x, bootstrap = TRUE, seed = 1424, n1 = n_points, log = FALSE) 
  })
  
  coeffs_i = do.call(cbind, coeffs_i)
  
  rmv = which(names(coeffs_i) == "item")[-1]
  
  coeffs_i = coeffs_i[-rmv]
  
  names(coeffs_i)[-1] = traitlabels
  
  for (w in seq_along(weights$trait)) {
    coeffs_i[, weights$trait[w]] = coeffs_i[,weights$trait[w]] * weights$w[w]
  }
  
  coeffs_i$index = rowSums(coeffs_i[2:ncol(coeffs_i)])
  
  coeffs_i$subset = names(group_split[i])
  
  coeffs_i$item = factor(coeffs_i$item, levels = lvls)
  
  coeffs = rbind(coeffs, coeffs_i)
  
  worth_plot[[i]] = 
    worth_map(mod_i, labels = traitlabels) +
    labs(x = "", y = "") +
    theme(axis.text.y = element_text(vjust = 0.5, 
                                     hjust = 1, 
                                     angle = 0)) +
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i])))
  
  if(i > 1) {
    worth_plot[[i]] =
      worth_plot[[i]] +
      theme(axis.text.y = element_blank())
    
  }
  
  if(i == 1) {
    worth_plot[[i]] =
      worth_plot[[i]] +
      theme(legend.position = "none")
  }
  
}

p = worth_plot[[1]] + worth_plot[[2]] 

p

ggsave("output/worth-map-trait-performance-region.pdf",
       plot = p,
       height = 15,
       width = 35,
       units = "cm")


sel = 
  ggplot(coeffs, aes(y = item, x = index,
                   fill = subset, color = subset)) +
  geom_vline(xintercept = mean(coeffs$index),
             colour = "grey40",
             linewidth = 0.5) +
  geom_boxplot(alpha = 0.2) +
  theme_minimal() +
  scale_fill_manual(values = c("grey50","#225ea8", "#e31a1c")) +
  scale_color_manual(values = c("grey50","#225ea8", "#e31a1c")) +
  labs(y = "", x = "Selection index") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(colour = "grey20", size = 14))

sel


ggsave("output/selection-index-mixtures-region.pdf",
       plot = sel,
       height = 20,
       width = 25,
       units = "cm")



# ........................................
# ........................................
# Split the data in subsets  ####
# do it by clusters
coeffs = lapply(mod, function(x){
  resample(x, bootstrap = TRUE, seed = 1424, n1 = 10, log = FALSE) 
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = traitlabels

for (w in seq_along(weights$trait)) {
  coeffs[, weights$trait[w]] = coeffs[,weights$trait[w]] * weights$w[w]
}

coeffs$index = rowSums(coeffs[2:ncol(coeffs)])

coeffs$subset = "All"

lvls = unique(coeffs$item[order(coeffs$index)])

coeffs$item = factor(coeffs$item, levels = lvls)


split = dat$cluster_desc

group_split = table(split)

group_split

# Likelihood ratio will check whether the ranks are significantly distinct 
# from each gender
llr = lapply(R, function(x){
  likelihood_ratio(x, split = split)
})

llr = do.call("rbind", llr)

llr$trait = traitlabels

llr

write.csv(llr, 
          "output/likelihood-ratio-clusters.csv")


coeffs = coeffs[coeffs$subset == "All", ]

# PCA and worth map
n_points = 10
worth_plot = list()

for (i in seq_along(group_split)) {
  
  # run the model 
  R_subset = lapply(R, function(x){
    x = x[split == names(group_split[i])]
    x = na.omit(x)
    x
  })
  
  mod_i = lapply(R_subset, PlackettLuce)
  
  # selection index
  coeffs_i = lapply(mod_i, function(x){
    resample(x, bootstrap = TRUE, seed = 1424, n1 = n_points, log = FALSE) 
  })
  
  coeffs_i = do.call(cbind, coeffs_i)
  
  rmv = which(names(coeffs_i) == "item")[-1]
  
  coeffs_i = coeffs_i[-rmv]
  
  names(coeffs_i)[-1] = traitlabels
  
  for (w in seq_along(weights$trait)) {
    coeffs_i[, weights$trait[w]] = coeffs_i[,weights$trait[w]] * weights$w[w]
  }
  
  coeffs_i$index = rowSums(coeffs_i[2:ncol(coeffs_i)])
  
  coeffs_i$subset = names(group_split[i])
  
  coeffs_i$item = factor(coeffs_i$item, levels = lvls)
  
  coeffs = rbind(coeffs, coeffs_i)
  
  worth_plot[[i]] = 
    worth_map(mod_i, labels = traitlabels) +
    labs(x = "", y = "") +
    theme(axis.text.y = element_text(vjust = 0.5, 
                                     hjust = 1, 
                                     angle = 0)) +
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i])))
  
  if(i > 1) {
    worth_plot[[i]] =
      worth_plot[[i]] +
      theme(axis.text.y = element_blank())
    
  }
  
  if(i < length(group_split)) {
    worth_plot[[i]] =
      worth_plot[[i]] +
      theme(legend.position = "none")
  }
  
}

p = worth_plot[[1]] + worth_plot[[2]] + worth_plot[[3]]

p

ggsave("output/worth-map-trait-performance-clusters.pdf",
       plot = p,
       height = 15,
       width = 35,
       units = "cm")


sel = 
  ggplot(coeffs, aes(y = item, x = index,
                     fill = subset, color = subset)) +
  geom_vline(xintercept = mean(coeffs$index),
             colour = "grey40",
             linewidth = 0.5) +
  geom_boxplot(alpha = 0.1) +
  theme_minimal() +
  scale_fill_manual(values = c("grey50","#225ea8", "#e31a1c", "#fdae61")) +
  scale_color_manual(values = c("grey50","#225ea8", "#e31a1c", "#fdae61")) +
  labs(y = "", x = "Selection index") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(colour = "grey20", size = 14))

sel


ggsave("output/selection-index-mixtures-clusters.pdf",
       plot = sel,
       height = 20,
       width = 25,
       units = "cm")









