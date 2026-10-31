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
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
source("script/helper/helper-01-function.R")
# Load data
dat = read.csv("data/tricot-data-nextgen-cassava.csv")

trait_cor = read.csv("output/kendall-partial-correlation-backward-selection.csv")

trait_cor[37, c(1,3)] = c("Final Evaluation - Choice For Replant", "Choice For Replant")

#............................................
#............................................
# Select relevant socio-economic variables
sel = c("sex", "processor", "region", "crop_div", "maritalstatus",
        "score_hdds_bad_season", "ppi_likelihood", "gender_male_control",
        "fa_market_orientation",  "h_hsize_mae", "livestock_div", 
        "longitude", "latitude")

covar = dat[sel]

# impute missing values
covar$sex[is.na(covar$sex)] = "Woman"
covar$maritalstatus[covar$maritalstatus == "Window"] = "Widow"
covar$maritalstatus[covar$maritalstatus == "Widower"] = "Widow"
covar$maritalstatus[covar$maritalstatus == "married"] = "Married"
str(covar)

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

table(covar$maritalstatus)
table(covar$region)
table(covar$processor)
table(covar$sex)

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
  scale_color_brewer(palette = "Set1")

ggsave("output/pca-clusters.pdf",
       plot = last_plot(),
       width = 12,
       height = 12,
       units = "cm")

boxplot(covar$gender_male_control ~ covar$cluster)

table(covar$sex, covar$cluster)
table(covar$processor, covar$cluster)

dat$cluster = covar$cluster

#............................................
#............................................
# PlackettLuce ####
names(dat)
pack_index = paste0("variety", letters[1:3])

table(unlist(dat[pack_index]))

#..........................................................
#..........................................................
# Organize traits ####
# traits will be grouped as a single trait
# check availability of traits
trait_list = getTraitList(dat, pattern = c("_best", "_worst"))

traits = lapply(trait_list, function(x){
  x$trait_label
})

traits = unlist(traits)

traitlabels = fix_trait_labels(traits)

index = which(traitlabels %in% trait_cor$trait)

# keep traits selected in the previous script 
# from backward selection
trait_list = getTraitList(dat,
                          pattern = c("_best", "_worst"),
                          trait.labels = traitlabels)

traitlabels[index]

traitlabels = traitlabels[index]

trait_list = trait_list[index]

traits = traits[index]

dat[pack_index] = lapply(dat[pack_index], function(x){
  x[x == "TMS-13F1160P0004"] = "Game Changer"
  x[x == "TMS-13F1343P0022"] = "Obasanjo 2"
  x[x == "IITA-TMS-IBA000070"] = "Baba 70"
  x
})

itemnames = unique(sort(unlist(dat[pack_index])))

#..........................................................
#..........................................................
#..........................................................
# PlackettLuce with aggregated rankings ####
R = list()

# this blocked code here will group the rankings by unique traits
#unitrait = unique(trait_cor$croptrait)

# this uses ranking individually
unitrait = trait_cor$trait

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

traits

# ........................................
# .......................................
# ANOVA  ####
# this will be used to test whether the varieties are different 
# from each other and we will retain the ones with alpha > 0.05
# get the reference trait
traitlabels = unitrait

ov = grep("Choice For Replant", traitlabels)

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



keep = union(which(anov$`Pr(>Chisq)` < 0.1), 37)

traitlabels = traitlabels[keep]

trait_list = trait_list[keep]

R = R[keep]


ov = grep("Choice For Replant", traitlabels)


pld = cbind(G = R[[ov]], covar)

pld$roles = ifelse(pld$sex == "Woman" & pld$processor == "Self", "Group 1", 
                   ifelse(pld$sex == "Man" & pld$processor == "Self", "Group 2",
                          "Group 3"))

pld$roles = as.factor(pld$roles)

dat$roles = pld$roles


table(pld$roles)

# choice for replanting
likelihood_ratio(R[[ov]], pld$cluster)

likelihood_ratio(R[[ov]], pld$roles)

tree1 = pltree(G ~ cluster, data = pld, alpha = 0.1, gamma = TRUE, 
               minsize = 10)

tree2 = pltree(G ~ roles, data = pld, alpha = 0.1, gamma = TRUE, 
               minsize = 10, npseudo = 5)

deviance(tree1)
deviance(tree2)

plot(tree1)
plot(tree2)

# # both variables are able to identify different rankings,
# # with a small improvement from role-based groups
# 
# ov_harvest = grep("Harvesting - Overall", traitlabels)
# # overall impression at harvest
# likelihood_ratio(R[[ov_harvest]], pld$cluster)
# 
# likelihood_ratio(R[[ov_harvest]], pld$roles)
# 
# # overall impression at 9 months
# ov_9m = grep("Ninth Month - Overall", traitlabels)
# 
# likelihood_ratio(R[[ov_9m]], pld$cluster)
# 
# likelihood_ratio(R[[ov_9m]], pld$roles)
# 
# # no difference from clusters, but the role-based groups 
# # present difference in preference
# 
# pld$G9m = R[[ov_9m]]
# 
# tree1 = pltree(G9m ~ cluster, data = pld, alpha = 0.1, gamma = TRUE, 
#                minsize = 10)
# 
# tree2 = pltree(G9m ~ roles, data = pld, alpha = 0.1, gamma = TRUE, 
#                minsize = 10, npseudo = 5)
# 
# deviance(tree1)
# deviance(tree2)
# 
# plot(tree1)
# plot(tree2)

# ........................................
# ........................................
# Calculate PL model  ####
ref = "TMEB693"

mod = lapply(R, PlackettLuce)

worth_map(mod, 
          labels = traitlabels,
          labels.order = rev(traitlabels))

worth = 
  worth_map(mod, 
            labels = traitlabels,
            labels.order = rev(traitlabels)) +#union(c("Choice For Replant", "Overall"), rev(traitlabels))) +
  labs(x = "", y = "") +
  theme(axis.text.y = element_text(vjust = 0.5, 
                                   hjust = 1, 
                                   angle = 0),
        text = element_text(size = 15)) 

ggsave("output/worth-map-trait-performance.pdf",
       plot = worth,
       height = 18,
       width = 30,
       units = "cm")

# ........................................
# ........................................
# Split the data by clusters
dat$cluster_desc = ifelse(dat$cluster == 1, "Market-oriented fresh root sellers", 
                          ifelse(dat$cluster == 2, "Woman dominated processors",
                                 ifelse(dat$cluster == 3, "Vulnerable resilient farmers", NA)))

dat$cluster_desc = factor(dat$cluster_desc, levels = c("Market-oriented fresh root sellers",
                                                       "Woman dominated processors", 
                                                       "Vulnerable resilient farmers"))

split = dat$cluster_desc

#split = dat$roles


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

# drop traits with no difference in likelihood ratio
keep = which(llr$`Pr(>Chisq)` < 0.1)

traitlabels = traitlabels[keep]

trait_list = trait_list[keep]

R = R[keep]


# reset indices
ov = grep("Choice For Replant", traitlabels)
#ov_harvest = grep("Harvesting - Overall", traitlabels)
# overall impression at 9 months
ov_9m = grep("Ninth Month - Overall", traitlabels)

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
             n1 = length(R_subset[[1]]), 
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
    worth_map2(object = mod_i,
              labels = traitlabels,
              labels.order = rev(traitlabels)) +
    labs(x = "", y = "") +
    theme(axis.text.y = element_text(vjust = 0.5, 
                                     hjust = 1, 
                                     angle = 0),
          text = element_text(size = 12),
          title = element_text(size = 10)) +
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i])))
  
  # write individual plots
  ggsave(paste0("output/worth-map-trait-performance-", i, "-", tolower(names(group_split)[i]), ".pdf"),
         plot = worth_plot[[i]],
         height = 10,
         width = 26,
         units = "cm")
  
  ggsave(paste0("output/pca-trait-performance-", i, "-", tolower(names(group_split)[i]), ".pdf"),
         plot = pca_plot[[i]],
         height = 10,
         width = 10,
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
       height = 10,
       width = 55,
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
t_labels = traitlabels
k = 10
reference_trait = grep("Choice For Replant", t_labels)

for (i in seq_along(group_split)) {
  
  # run kendall correlation among traits for this group
  R_traits = lapply(trait_list, function(x){
    rank_tricot(dat[split == names(group_split[i]), ],
                pack_index,
                x$string,
                validate.rankings = TRUE)
  })
  
  
  n = length(R_traits[[ov]])
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
  
  kt = kendall[kendall$group == names(group_split)[i], ]
  
  lvls = unique(kendall$trait[order(kt$kendallTau)])
  
  kt$trait = factor(kt$trait, levels = lvls)
  
  kendall_plot = 
    ggplot(kt,
           aes(y = trait, x = kendallTau, 
               fill = group, color = group)) +
    geom_boxplot(alpha = 0.8, show.legend = FALSE) +
    theme_minimal() +
    scale_fill_manual(values = c("#225ea8", "#e31a1c", "#fdae61")) +
    scale_color_manual(values = c("#225ea8", "#e31a1c", "#fdae61")) +
    xlim(0, 1) +
    labs(y = "", 
         x = expression("Kendall's " * tau),
         title = paste0("(", LETTERS[i], ") ", 
                names(group_split[i]),
                ", n = ",
                as.integer(group_split[i]))) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          text = element_text(colour = "grey10", size = 13),
          title = element_text(size = 10))
  
  # write individual plots
  ggsave(paste0("output/kendall-tau-", i, "-", tolower(names(group_split)[i]), ".pdf"),
         plot = kendall_plot,
         height = 15,
         width = 20,
         units = "cm")
  
  k_plot[[i]] =
    kendall_plot + 
    labs(y = "", 
         x = "",
         title = paste0("(", LETTERS[i], ") ", 
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i])))
  
  # if(i > 1) {
  #   k_plot[[i]] =
  #     k_plot[[i]] +
  #     theme(axis.text.y = element_blank())
  #   
  # }

}

kendall_plot = 
  ggplot(kendall, aes(y = trait, x = kendallTau, 
                      fill = group, color = group)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual(values = c("grey20","#225ea8", "#e31a1c")) +
  scale_color_manual(values = c("grey20","#225ea8", "#e31a1c")) +
  labs(y = "", x = expression("Kendall's " * tau)) +
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
       width = 40,
       units = "cm")

