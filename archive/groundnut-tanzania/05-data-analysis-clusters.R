library("cluster")
library("factoextra")
library("caret")
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
library("geodata")
library("sf")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
source("script/helper-01-function.R")
# Load data
dat = read.csv("data/groundnut-tricot-tanzania.csv")

table(dat$package_project_name)

unique(as.Date(dat$vegetative_survey_start[dat$package_project_name == "gnuttz2025"]))

trait_cor = read.csv("output/kendall-partial-correlation-backward-selection.csv")

trait_cor[11, c(1)] = c("Overall")

adm = gadm("TZ", 3, "data")

adm = st_as_sf(adm)
crs(adm)

coords = st_as_sf(dat[,c("longitude", "latitude")], coords = c("longitude", "latitude"), crs = 4326)

adm_info = st_join(coords, adm, join = st_intersects)

dat$adm1 = adm_info$NAME_1

dat$adm2 = adm_info$NAME_2

table(dat$adm1)
table(dat$adm2)

#............................................
#............................................
# Select relevant socio-economic variables
dat$xy = dat$longitude + dat$latitude

sel = c("age", "experiencecrop", "gender", 
        #"whocontrolexchange", "whocontrolsell", "whocontrolprod",
        "xy")

covar = dat[sel]

covar[1:ncol(covar)] = lapply(covar[1:ncol(covar)], function(x){
  
  if (class(x) != "character") {
    x[is.na(x)] = median(x, na.rm = TRUE)
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

str(covar)

# one-hot encoding for categorical variables
encoded = dummyVars("~.", data = covar, fullRank = TRUE)
transformed = predict(encoded, covar)
scaled = scale(transformed) # Scale numerical values

# Determine optimal number of clusters using the Elbow method
wss = sapply(2:10, function(k) {
  kmeans(scaled, centers = k, nstart = 10)$tot.withinss
})

# Plot Elbow curve
plot(2:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "N clusters (k)", 
     ylab = "Total within sum of squares")

# apply K-Means clustering with optimal k (e.g., 4)
k_optimal = 3
set.seed(1947)
kmeans_result = kmeans(scaled, centers = k_optimal, nstart = 10)
covar$cluster = as.factor(kmeans_result$cluster)

table(covar$cluster)

pca = prcomp(scaled)
pca_data = as.data.frame(pca$x[, 1:2])
pca_data$cluster = covar$cluster

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Clusters Visualization with PCA", 
       x = "PC1", 
       y = "PC2") +
  theme_minimal() + 
  theme(text = element_text(size = 15))


ggsave("output/pca-clusters-farms.pdf",
       plot = last_plot(),
       width = 15,
       height = 15,
       units = "cm")


dat$cluster = covar$cluster

table(dat$gender, covar$cluster)

write.csv(dat, "data/groundnut-tricot-tanzania-with-clusters.csv", row.names = FALSE)

dat = dat[dat$package_project_name != "gnuttz2025", ]

table(dat$gender, dat$cluster)

#............................................
#............................................
#  Trial map ####
adm = gadm("TZ", 1, "data")

adm = st_as_sf(adm)

adm$region = ifelse(adm$NAME_1 == "Kagera" | adm$NAME_1 == "Mwanza" | adm$NAME_1 == "Mara" , "Lake",
                    ifelse(adm$NAME_1 == "Kigoma" | adm$NAME_1 ==  "Shinyanga" | adm$NAME_1 == "Tabora" | adm$NAME_1 == "Geita"  | adm$NAME_1 == "Simiyu", "Western",
                           ifelse(adm$NAME_1 == "Arusha" | adm$NAME_1 == "Kilimanjaro" | adm$NAME_1 == "Manyara" | adm$NAME_1 == "Tanga", "Northern",
                                  ifelse(adm$NAME_1 == "Singida" | adm$NAME_1 =="Dodoma", "Central",
                                         ifelse(adm$NAME_1 == "Rukwa" | adm$NAME_1 == "Mbeya" | adm$NAME_1 == "Iringa" | adm$NAME_1 == "Katavi" | adm$NAME_1 == "Songwe" | adm$NAME_1 == "Njombe", "Southern Highlands",
                                                ifelse(adm$NAME_1 == "Lindi" | adm$NAME_1 == "Mtwara" | adm$NAME_1 == "Ruvuma", "Southern",
                                                       "Eastern"))))))

coords = dat[,c("longitude", "latitude")]

coords[coords == 0] = NA

coords = na.omit(coords)

# to ensure the privacy of participants location
# we can put the lonlat info into clusters of 0.5 resolution
h = dist(coords)

h = hclust(h)

h = cutree(h, h = 0.25)

# split the d by each defined cluster
d = split(coords[, 1:2], h)

# and take the mean 
d = lapply(d, function(x) {
  colMeans(x)
})

# back to data frame
d = do.call("rbind", d)

d = as.data.frame(d)

names(d) = c("longitude", "latitude")

d$trial = "tricot"

coords = st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326)

rcbd = matrix(c(-6.5167,	38.9167,
                -10.3539,	40.1682,
                -4.8356,	29.6777,
                -5.5866,	35.4609,
                -8.9094,	34.4560,
                -10.22,	38.4545,
                -5.3456,	32.4156,
                -2.4300,	33.1000,
                -11.0400,	37.2100), 
              ncol = 2, byrow = TRUE)

dimnames(rcbd)[[2]] = c("latitude", "longitude")

rcbd = as.data.frame(rcbd)

rcbd$trial = "RCBD"

rcbd = st_as_sf(rcbd, coords = c("longitude", "latitude"), crs = 4326)

coords = rbind(coords, rcbd)

map = 
  ggplot() +
  geom_sf(data = adm, mapping = aes(fill = region), alpha = 0.5) +
  geom_sf(data = coords, mapping = aes(color = trial, shape = trial), size = 2) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.title = element_blank(),
        legend.position = "right")
map

ggsave("output/trial-location-map.pdf",
       plot = map,
       width = 20,
       height = 20,
       units = "cm")

#............................................
#............................................
# PlackettLuce ####
names(dat)
pack_index = paste0("package_item_", letters[1:3])

table(unlist(dat[pack_index]))

#..........................................................
#..........................................................
# Organize traits ####
# traits will be grouped as a single trait
# check availability of traits
trait_list = getTraitList(dat, pattern = c("_pos", "_neg"))

traits = lapply(trait_list, function(x){
  x$trait_label
})

traits = unlist(traits)

traitlabels = fix_trait_labels(traits)

index = which(traitlabels %in% trait_cor$trait)

# keep traits selected in the previous script 
# from backward selection
trait_list = getTraitList(dat,
                          pattern = c("_pos", "_neg"),
                          trait.labels = traitlabels)

traitlabels[index]

traitlabels = traitlabels[index]

trait_list = trait_list[index]

traits = traits[index]

table(unlist(dat[pack_index]))

itemnames = unique(sort(unlist(dat[pack_index])))


# dat[pack_index] = lapply(dat[pack_index], function(x){
#   x[x == "Tarika 1"] = NA
#   x[x == "Tarika 2"] = NA
# })

#..........................................................
#..........................................................
#..........................................................
# PlackettLuce with aggregated rankings ####
R = list()

unitrait = unique(trait_cor$trait)

indicesbase = 1:nrow(dat)

for(i in seq_along(unitrait)) {
  
  print(unitrait[i])
  
  index = grep(unitrait[i], traitlabels)
  
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

R

# ........................................
# .......................................
# ANOVA  ####
# this will be used to test whether the varieties are different 
# from each other and we will retain the ones with alpha > 0.05
# get the reference trait
traitlabels = unitrait

ov = grep("Overall", traitlabels)

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
ref = "Mnanje 09"

mod = lapply(R, PlackettLuce)

worth_map(mod, 
          labels = traitlabels,
          labels.order = union(c("Overall"), rev(traitlabels))) 

worth = 
  worth_map(mod, 
            labels = traitlabels,
            labels.order = union(c("Overall"), rev(traitlabels))) +
  labs(x = "", y = "") +
  theme(axis.text.y = element_text(vjust = 0.5, 
                                   hjust = 1, 
                                   angle = 0),
        text = element_text(size = 15)) 


worth

ggsave("output/worth-map-trait-performance.pdf",
       plot = worth,
       height = 15,
       width = 35,
       units = "cm")

# ........................................
# ........................................
# Split the data by clusters
# dat$cluster_desc = ifelse(dat$cluster == 1, "Market-oriented", 
#                           ifelse(dat$cluster == 2, "Self-sufficient",
#                                  ifelse(dat$cluster == 3, "Resilient", NA)))
# 
# dat$cluster_desc = factor(dat$cluster_desc, levels = c("Market-oriented", "Self-sufficient", "Resilient"))

dat$cluster_desc = as.factor(dat$cluster)

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

# PCA and worth map
traits = traitlabels
worth_plot = list()
pca_plot = list()

for (i in seq_along(group_split)) {
  
  # run the model 
  R_subset = lapply(R, function(x){
    x = x[split == names(group_split[i])]
    x = na.omit(x)
    x
  })
  
  mod_i = lapply(R_subset, PlackettLuce)
  
  if(i == 1) {
    itemsorder = names(rev(sort(coefficients(mod_i[[ov]], log = FALSE))))
  }
  
  # selection index
  coeffs = lapply(mod_i, function(x){
    gosset::resample(x, bootstrap = TRUE, 
             seed = 1424, 
             n1 = 10, 
             log = FALSE) 
  })
  
  coeffs = do.call(cbind, coeffs)
  
  rmv = which(names(coeffs) == "item")[-1]
  
  coeffs = coeffs[-rmv]
  
  names(coeffs)[-1] = traitlabels
  
  # PCA
  pc = princomp(coeffs[, -1], cor = TRUE)
  
  pcplot = plot_pca(pc, scale = 6) + 
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i]))) +
    theme(text = element_text(size = 15))
  
  pca_plot[[i]] = pcplot
  
  worth_plot[[i]] = 
    worth_map(mod_i,
              labels = traitlabels,
              #items.order = rev(itemsorder),
              labels.order = union(c("Overall"), rev(traitlabels))) +
    labs(x = "", y = "") +
    theme(axis.text.y = element_text(vjust = 0.5, 
                                     hjust = 1, 
                                     angle = 0),
          text = element_text(size = 14)) +
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i])))
  
  # write individual plots
  ggsave(paste0("output/worth-map-trait-performance-", i, "-", tolower(names(group_split)[i]), ".pdf"),
         plot = worth_plot[[i]],
         height = 15,
         width = 35,
         units = "cm")
  
  ggsave(paste0("output/pca-trait-performance-", i, "-", tolower(names(group_split)[i]), ".pdf"),
         plot = pca_plot[[i]],
         height = 20,
         width = 20,
         units = "cm")
  
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
       width = 50,
       units = "cm")

p = pca_plot[[1]] + pca_plot[[2]] + pca_plot[[3]]



ggsave("output/pca-trait-performance-clusters.pdf",
       plot = p,
       height = 15,
       width = 45,
       units = "cm")


# ....................................
# Kendall tau inside the nodes ####
kendall = data.frame()
k_plot = list()
t_labels = unlist(lapply(trait_list, function(x) x$trait_label))
k = 10
reference_trait = grep("Overall", t_labels)

for (i in seq_along(group_split)) {
  
  # run kendall correlation among traits for this group
  R_traits = lapply(trait_list, function(x){
    rank_tricot(dat[split == names(group_split[i]), ], 
                pack_index, 
                x$string, 
                validate.rankings = TRUE)
  })
  
  
  n = nrow(R_traits[[ov]])
  set.seed(1111)
  folds = sample(rep(1:k, times = ceiling(n / k), length.out = n))
  
  # run over folds
  for (j in seq_len(k)) {
    
    kt = lapply(R_traits[-ov], function(x){
      kendallTau(x[folds != j, ],
                 R_traits[[ov]][folds != j, ])
    })
    
    kt = do.call("rbind", kt)
    
    kt$trait = t_labels[-ov]
    
    kt$group = names(group_split)[i]
    
    kendall = rbind(kendall, kt)
    
  }
  
  if(i == 1) lvls = unique(kendall$trait[order(kendall$kendallTau)])
  
  kendall$trait = factor(kendall$trait, levels = lvls)
  
  kendall_plot = 
    ggplot(kendall[kendall$group == names(group_split[i]), ],
                        aes(y = trait, x = kendallTau, 
                      fill = group, color = group)) +
    geom_boxplot(alpha = 0.8, show.legend = FALSE) +
    theme_minimal() +
    scale_fill_manual(values = c("#225ea8", "#e31a1c", "#fdae61")) +
    scale_color_manual(values = c("#225ea8", "#e31a1c", "#fdae61")) +
    xlim(0, 0.6) +
    labs(y = "", 
         x = "Kendall correlation with Overall Preference",
         title = paste0("(", LETTERS[i], ") ", 
                names(group_split[i]),
                ", n = ",
                as.integer(group_split[i]))) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          text = element_text(colour = "grey10", size = 16),
          title = element_text(size = 0))
  
  # write individual plots
  ggsave(paste0("output/kendall-tau-", i, "-", tolower(names(group_split)[i]), ".pdf"),
         plot = kendall_plot,
         height = 15,
         width = 15,
         units = "cm")
  
  k_plot[[i]] =
    kendall_plot + 
    labs(y = "", 
         x = "",
         title = paste0("(", LETTERS[i], ") ", 
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i])))
  
  if(i > 1) {
    k_plot[[i]] =
      k_plot[[i]] +
      theme(axis.text.y = element_blank())
    
  }

}

kendall_plot = 
  ggplot(kendall, aes(y = trait, x = kendallTau, 
                      fill = group, color = group)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual(values = c("grey20","#225ea8", "#e31a1c")) +
  scale_color_manual(values = c("grey20","#225ea8", "#e31a1c")) +
  labs(y = "", x = "Kendall correlation with Choice for Replanting") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(colour = "grey20", size = 15))

kendall_plot

ggsave("output/kendall-correlation.pdf",
       plot = kendall_plot,
       height = 30,
       width = 25,
       units = "cm")


p = k_plot[[1]] + k_plot[[2]] + k_plot[[3]]

p

ggsave("output/kendall-correlation-panels.pdf",
       plot = p,
       height = 15,
       width = 30,
       units = "cm")



coords$cluster = covar$cluster


plot(coords)


