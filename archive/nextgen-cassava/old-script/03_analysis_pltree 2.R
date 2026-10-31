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
source("script/helper_01_functions.R")

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/03_analysis_pltree.txt")

dir.create("output", recursive = TRUE, showWarnings = FALSE)
# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
list.files("data")
dat <- read.csv("data/nextgen_cassava_tricot_with_covar.csv")

# this script runs over the data and select traits 
# validating the data for the analysis
source("script/helper_02_organize_traits_and_assessments.R")

# station <- read.csv("data/cassava_onstation_traits.csv")
# 
# # organize and match names of genotypes
# station$accession_name <- gsub("TMS13", "TMS-13", station$accession_name)
# station$accession_name <- gsub("TMS14", "TMS-14", station$accession_name)
# station$accession_name <- gsub("IITA-TMS-IBA961632", "TMS-IBA961632", station$accession_name)
# station$accession_name <- gsub("TMS-13F1053P0010", "TMS-13F0153P0001", station$accession_name)
# 
# oft_geno <- sort(unique(unlist(dat[,c("varietya", "varietyb", "varietyc")]))) 
# sta_geno <- sort(unique(station$accession_name))
# 
# oft_geno[!oft_geno %in% sta_geno]
# 
# sta_geno[!sta_geno %in% oft_geno]
# 
# # keep only qualitative data from station
# paste(names(station), collapse = "','")
# 
# sel <- c('accession_name', 'Year', 'location', 'sprouting','mcmds','fyld','dyld','dm','A_value','B_value')
# 
# station <- station[, sel]
# 
# names(station)[1:2] <- c("genotype", "year")
# 
# mod <- glm(dm ~ location + genotype, data = station)
# summary(mod)
# 
# mod2 <- glm(fyld ~ location + genotype, data = station)
# summary(mod2)
# 
# mod3 <- glm(dyld ~ location + genotype, data = station)
# summary(mod3)
# 
# # aggregate data 
# station <- split(station, paste(station$genotype, 
#                                 sep = ","))
# 
# station <- lapply(station, function(x){
#   y <- data.frame(genotype = x[1,1],
#                   #location = x[1,3],
#                   sprouting = max(x[,4], na.rm = TRUE),
#                   mcmds = min(x[,5], na.rm = TRUE),
#                   fyld = max(x[,6], na.rm = TRUE),
#                   dyld = max(x[,7], na.rm = TRUE),
#                   dm = min(x[,8], na.rm = TRUE),
#                   A_value = mean(x[,9], na.rm = TRUE),
#                   B_value = mean(x[,10], na.rm = TRUE))
# 
# })
# 
# station <- as.data.frame(do.call("rbind", station))
# 
# # input empty data for the missing genotype
# station[nrow(station) + 1, "genotype"] <- "K195"
# station[nrow(station), -c(1)] <- colMeans(station[,-c(1)], na.rm = TRUE)
# 
# features <- station

# ..............................
# ..............................
# Split the analysis into two parts ####
# one for the overall preference at harvest to capture the agronomic 
# performance and the other for the overall preference at the end of the 
# project, which reflects the preference of post-harvest
# ..............................
# ..............................
# PlackettLuce tree ####
covars <- c("sex", "processor", "other_processor_gender", "region", 
            "crop_div", "score_hdds_bad_season", "ppi_likelihood", "gender_male_control", 
            "gender_female_control", "h_hsize_mae", "land_owned", "household_type")

newnamescovars <- c("GenderFarmer", "Processor", "GenderProcessor", "Region",
                    "CropDiv", "HDDSBadSeason","PPI", "GenderMaleControl",
                    "GenderFemaleControl", "HHSizeMAE", "LandOwned", "HouseholdType")

covar <- dat[, covars]

names(covar) <- newnamescovars 

# update gender of processor, using information from the trial 
covar$GenderProcessor <- ifelse(covar$Processor  == "Self",
                                covar$GenderFarmer, 
                                covar$GenderProcessor)

table(covar$GenderProcessor)

covar$GenderProcessor[is.na(covar$GenderProcessor)] <- "Woman"

covar[1:ncol(covar)] <- lapply(covar[1:ncol(covar)], function(x){
  
  if (class(x) != "character") {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  
  if (class(x) == "numeric") {
    x <- round(x, 2)
  }
  
  if (class(x) == "character") {
    x <- ifelse(is.na(x), "Unknown", x)
    x <- as.factor(x)
  }
  
  x
  
})


# .........................................
# Correlation between overall and other traits
# select the baseline trait 
overall1 <- "harvesting_fresh_roots_overallimpression"

# and the select the other traits from agronomic performance
traits1 <- names(trait_list)[1:29]

# overall as first to make easy to handle later
traits1 <- traits1[!grepl("overall", traits1)]

traits1 <- union(overall1, traits1)

R1 <- list()
tname1 <- c()
ttag1 <- c()
traitcov1 <- list()

# run over the traits to compute kendall correlation
for(i in seq_along(traits1)) {
  
  trait_i <- which(names(trait_list) %in% traits1[i])
  
  # this should be combined with the overall at harvest which will be the baseline
  keep_i <- trait_list[[trait_i]]$keep
  
  r_i <- rankTricot(dat[keep_i, ],
                    itemnames,
                    c(trait_list[[trait_i]]$index_pos, trait_list[[trait_i]]$index_neg),
                    group = FALSE)
  
  tname1 <- c(tname1, paste(trait_list[[trait_i]]$trait, trait_list[[trait_i]]$moment, sep = "-"))
  
  ttag1 <- c(ttag1, names(trait_list[trait_i]))
  
  traitcov1[[i]] <- rep(paste(trait_list[[trait_i]]$trait, trait_list[[trait_i]]$moment, sep = "-"), 
                        times = length(r_i))
  
  R1[[i]] <- r_i
  
}

# compute kendall correlation
kendall1 <- lapply(R1[-1], function(x){
  xx <- coefficients(PlackettLuce(R1[[1]]), log = FALSE)
  yy <- coefficients(PlackettLuce(x), log = FALSE)
  kendallTau(xx, yy)
})

kendall1 <- do.call("rbind", kendall1)

kendall1$trait <- tname1[-1]
kendall1$ttag <- ttag1[-1]

kendall1 <- kendall1[kendall1$kendallTau > 0, ]

kendall1 <- kendall1[rev(order(kendall1$kendallTau)), ]

write.csv(kendall1, "output/agronomic_performance_kendall_cor_harvesting_fresh_roots_overallimpression.csv", 
          row.names = FALSE)

# Run a PlackettLuce() to assess trait performance by genotype
mod1 <- lapply(R1[-1], function(x){
  PlackettLuce(x)
})

wmap1 <- worth_map(mod1, labels = tname1[-1]) + 
  theme_bw() +
  theme(axis.text = element_text(color = "grey20"),
        strip.text.x = element_text(color = "grey20"),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust=1),
        axis.text.y = element_text(angle = 40, vjust = 1, hjust=1))

ggsave("output/agronomic_performance_worth_map.png",
       plot = wmap1,
       width = 23,
       height = 15,
       units = "cm",
       dpi = 600)


# get coefficients from each genotype
best_genos1 <- lapply(mod1, function(x){
  c_x <- rev(sort(coef(x, log = FALSE)))
  data.frame(items = names(c_x)[1:5],
             rank = 1:5)
})

best_genos1 <- do.call(rbind, best_genos1)

best_genos1$trait <- rep(tname1[-1], each = 5)

best_genos1

write.csv(best_genos1, "output/agronomic_performance_best_geno_by_trait.csv", row.names = FALSE)

# ..........................................
# ..........................................
# PlackettLuce tree with OA at harvest ####
# this will look for into socio-economic
# aspects influencing the choice/preference of genotypes
names(trait_list)

basetrait1 <- "harvesting_fresh_roots_overallimpression"
maintrait1 <- kendall1$ttag   
maintrait1 <- maintrait1[!grepl("overall", maintrait1)]
maintrait1 <- union(basetrait1, maintrait1)

# this part will group the rankings by each trait 
# using the index argument in group() to assess genotype 
# performance across traits
# set the indices for the basetrait
indicesbase1 <- as.vector(which(trait_list[[basetrait1]]$keep))
resetindices1 <- 1:length(indicesbase1)

R1 <- list()
index1 <- c()

for(i in seq_along(maintrait1)) {
  
  trait_i <- which(names(trait_list) %in% maintrait1[i])
  
  # this should be combined with the overall at harvest which will be the baseline
  index_i <- as.vector(which(trait_list[[trait_i]]$keep))
  
  keep_i <- index_i %in% indicesbase1
  
  index_i <- index_i[keep_i]
  
  r_i <- rankTricot(dat[index_i, ],
                    itemnames,
                    c(trait_list[[trait_i]]$index_pos, trait_list[[trait_i]]$index_neg),
                    group = FALSE)
  
  
  # reset indices to match with grouped_rankings later
  index_i <- resetindices1[indicesbase1 %in% index_i]
  
  index1 <- c(index1, index_i)
  
  R1[[i]] <- r_i
  
}

Ragro <- do.call("rbind", R1)

Gagro <- group(Ragro, index = index1)

Gagro

# Gagro is dense group of rankings provided by participants to each trait
reference_geno <- "K195"

mod_agro <- PlackettLuce(Gagro)

reliable1 <- reliability(mod_agro, ref = reference_geno)

reliable1

# now select covariates
keep <- trait_list[[basetrait1]]$keep

sum(keep)

# check covars
covar1 <- covar[keep, ]

# refresh factor levels
covar1[1:ncol(covar1)] <- lapply(covar1[1:ncol(covar1)], function(x){
  if(class(x)=="factor"){
    x <- as.factor(as.character(x))
  }
  x
})

str(covar1)

# ..............................
# ..............................
# # Fit PL tree
# PL tree with condensed grouped rankings 
# up to harvest agronomic performance
G1 <- group(R1[[1]], 1:length(R1[[1]]))

pld1 <- cbind(G = G1, covar1)

names(pld1)

tree1 <- pltree(G ~ HDDSBadSeason + GenderMaleControl + HHSizeMAE + 
                  GenderFarmer + Region + PPI + CropDiv +
                  LandOwned + HouseholdType,
                alpha = 0.1,
                minsize = 40,
                gamma = TRUE,
                data = pld1,
                verbose = TRUE,
                npseudo = 10)

# this tree is to check the effect of GenderFemaleControl
# it produces a model with lower deviance, but the nodes are
# more complex to explain, since the improvement is not significant
# we keep with tree 1, with GenderMaleControl 
# tree2 <- pltree(G ~ HDDSBadSeason + GenderFemaleControl + HHSizeMAE + 
#                   GenderFarmer + Region + PPI + CropDiv +
#                   LandOwned + HouseholdType,
#                 alpha = 0.1,
#                 minsize = 40,
#                 gamma = TRUE,
#                 data = pld1,
#                 verbose = TRUE,
#                 npseudo = 10)

node_rules(tree1)

# run this for the condensed rankings using predicted nodes
nodes_tree1 <- predict(tree1, type = "node")
node_id_tree1 <- sort(unique(nodes_tree1))

length(nodes_tree1) == length(Gagro)

tree1_mod <- list()
nobs_tree1 <- integer()
for (i in seq_along(node_id_tree1)) {
  
  Gi <- Gagro[nodes_tree1 == node_id_tree1[i]]
  
  tree1_mod[[i]] <- PlackettLuce(Gi)
  
  nobs_tree1 <- c(nobs_tree1, length(Gi))
  
}

tree1_branch <- gosset:::build_tree_branches(tree1)

# tree1_nodes <- gosset:::build_tree_nodes(tree1_mod, 
#                                          log = TRUE,
#                                          node.ids = node_id_tree1,
#                                          n.obs = nobs_tree1)


ptree1 <- tree1_branch / tree1_nodes + plot_layout(heights =  c(1, 2))

ptree1

wr1 <- regret(tree1_mod, n1 = 1000)

reliable_tree1 <- lapply(tree1_mod, function(x){
  reliability(x, ref = reference_geno)
})

reliable_tree1

tree1_nodes <- plot_nodes(reliable_tree1)

ptree1 <- tree1_branch / tree1_nodes + plot_layout(heights =  c(1, 2))


reliable_tree1 <- colMeans(do.call(rbind, reliable_tree1))
 
reliable_tree1 <- data.frame(items = names(rev(sort(reliable_tree1))),
                      reliability_tree = rev(sort(reliable_tree1)))

wr1 <- merge(wr1, reliable1, by = "items")
wr1 <- merge(wr1, reliable_tree1, by = "items")
wr1

wr1 <- wr1[order(wr1$worst_regret), ]

ggsave(filename = "output/agronomic_performance_pltree.png", 
       plot = ptree1,
       units = "cm",
       height = 30,
       width = 50, 
       dpi = 600)

write.csv(wr1,
          "output/agronomic_performance_risk_assessment.csv", 
          row.names = FALSE)

# predict nodes of each farmer
keep_agronomic <- keep

nodes1 <- predict(tree1, newdata = covar, type = "node")

nodes1 <- data.frame(tricot_pkg = dat[, "tricot_pkg"], 
                     node = nodes1)

rules1 <- node_rules(tree1)

nodes_agro1 <- merge(nodes1, rules1, by = "node", all.x = TRUE)

# now for the other tree with female control
nodes2 <- predict(tree2, newdata = covar, type = "node")

nodes2 <- data.frame(tricot_pkg = dat[,"tricot_pkg"], 
                     node = nodes2)

rules2 <- node_rules(tree2)

nodes_agro2 <- merge(nodes2, rules2, by = "node", all.x = TRUE)

head(nodes_agro1)
head(nodes_agro2)

names(nodes_agro1)[c(1,3)] <- paste0(names(nodes_agro1)[c(1,3)], "_tree1")
names(nodes_agro2)[c(1,3)] <- paste0(names(nodes_agro2)[c(1,3)], "_tree2")

head(nodes_agro1)
head(nodes_agro2)

nodes_agro <- merge(nodes_agro1, nodes_agro2, by = "tricot_pkg", all.x = TRUE)

head(nodes_agro)

write.csv(nodes_agro, "output/agronomic_performance_nodes.csv", row.names = FALSE)

# get traits that each variety is best
top1 <- lapply(tree1_mod, function(x){
  names(rev(sort(coef(x, log = F))))[1:3]
})

top1 <- data.frame(items = unlist(top1), node = rep(node_id_tree1, each = 3))

top1$traits <- NA

best_i <- best_genos1[best_genos1$rank <= 5, ]

for (i in seq_along(top1$items)){
  item_i <- top1$items[i]
  
  top1$traits[i] <- paste(best_i$trait[best_i$items %in% item_i],
                          collapse = ", ")
}

write.csv(top1, "output/agronomic_performance_genotype_trait_performance_from_tree_nodes.csv", row.names = FALSE)

#..........
# write worth maps for each node
for (j in seq_along(node_id_tree1)) {
  
  modj <- list()
  nodesij <- predict(tree1, newdata = covar, type = "node")
  
  for(i in seq_along(traits1)) {
    
    trait_i <- which(names(trait_list) %in% traits1[i])
    
    # this should be combined with the overall at harvest which will be the baseline
    Rij <- dat[trait_list[[trait_i]]$keep & 
                 keep_agronomic &
                 nodesij == node_id_tree1[j], ]
    
    Rij <- rankTricot(Rij,
                     itemnames,
                     c(trait_list[[trait_i]]$index_pos, trait_list[[trait_i]]$index_neg),
                     group = FALSE)
    
    
    Rij <- pseudo_rank(Rij)
    
    Rij <- PlackettLuce(Rij)
    
    modj[[i]] <- Rij
    
    rm(Rij)
    
  }
  
  wmj <- worth_map(modj, labels = tname1) + 
    theme_bw() +
    theme(axis.text = element_text(color = "grey20"),
          strip.text.x = element_text(color = "grey20"),
          axis.text.x = element_text(angle = 40, vjust = 1, hjust=1),
          axis.text.y = element_text(angle = 40, vjust = 1, hjust=1))
  
  ggsave(paste0("output/agronomic_performance_worth_map_node", 
                node_id_tree1[j], ".png"),
         plot = wmj,
         width = 23,
         height = 15,
         units = "cm",
         dpi = 600)
  
  rm(modj, wmj, nodesij)
}



# ..............................
# ..............................
# PlackettLuce tree with OA post-harvest ####
# .........................................
# Correlation between overall and other traits
# select the baseline trait 
overall3 <- "final_evaluation_choiceforreplant"

# and the select the other traits from agronomic performance
traits3 <- names(trait_list)[31:60]

# overall as first to make easy to handle later
traits3 <- traits3[!grepl("overall", traits3)]

traits3 <- union(overall3, traits3)

R3 <- list()
tname3 <- c()
ttag3 <- c()
traitcov3 <- list()

# run over the traits to compute kendall correlation
for(i in seq_along(traits3)) {
  
  trait_i <- which(names(trait_list) %in% traits3[i])
  
  # this should be combined with the overall at harvest which will be the baseline
  keep_i <- trait_list[[trait_i]]$keep
  
  r_i <- rankTricot(dat[keep_i, ],
                    itemnames,
                    c(trait_list[[trait_i]]$index_pos, trait_list[[trait_i]]$index_neg),
                    group = FALSE)
  
  tname3 <- c(tname3, paste(trait_list[[trait_i]]$trait, trait_list[[trait_i]]$moment, sep = "-"))
  
  ttag3 <- c(ttag3, names(trait_list[trait_i]))
  
  traitcov3[[i]] <- rep(paste(trait_list[[trait_i]]$trait, trait_list[[trait_i]]$moment, sep = "-"), 
                        times = length(r_i))
  
  R3[[i]] <- r_i
  
}

# compute kendall correlation
kendall3 <- lapply(R3[-1], function(x){
  xx <- coefficients(PlackettLuce(R3[[1]]), log = FALSE)
  yy <- coefficients(PlackettLuce(x), log = FALSE)
  kendallTau(xx, yy)
})

kendall3 <- do.call("rbind", kendall3)

kendall3$trait <- tname3[-1]
kendall3$ttag <- ttag3[-1]

kendall3 <- kendall3[kendall3$kendallTau > 0, ]

kendall3 <- kendall3[rev(order(kendall3$kendallTau)), ]

write.csv(kendall3, "output/postharvest_kendall_cor_final_choice.csv", 
          row.names = FALSE)

# Run a PlackettLuce() to assess trait performance by genotype
mod3 <- lapply(R3[-1], function(x){
  PlackettLuce(x)
})

wmap3 <- worth_map(mod3, labels = tname3[-1]) + 
  theme_bw() +
  theme(axis.text = element_text(color = "grey20"),
        strip.text.x = element_text(color = "grey20"),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust=1),
        axis.text.y = element_text(angle = 40, vjust = 1, hjust=1))

ggsave("output/postharvest_performance_worth_map.png",
       plot = wmap3,
       width = 23,
       height = 15,
       units = "cm",
       dpi = 600)

# get coefficients from each genotype
best_genos3 <- lapply(mod3, function(x){
  c_x <- rev(sort(coef(x, log = FALSE)))
  data.frame(items = names(c_x)[1:5],
             rank = 1:5)
})

best_genos3 <- do.call(rbind, best_genos3)

best_genos3$trait <- rep(tname3[-1], each = 5)

write.csv(best_genos3, "output/postharvest_best_geno_by_trait.csv", row.names = FALSE)

# ..........................................
# ..........................................
# PlackettLuce tree with OA post harvest ####
# this will look for into socio-economic
# aspects influencing the choice/preference of genotypes
names(trait_list)

basetrait3 <- "final_evaluation_choiceforreplant"

maintrait3 <- kendall3$ttag   

maintrait3 <- maintrait3[!grepl("overall", maintrait3)]

maintrait3 <- union(basetrait3, maintrait3)

# this part will group the rankings by each trait 
# using the index argument in group() to assess genotype 
# performance across traits
# set the indices for the basetrait
indicesbase3 <- as.vector(which(trait_list[[basetrait3]]$keep))
resetindices3 <- 1:length(indicesbase3)

R3 <- list()
index3 <- c()

for(i in seq_along(maintrait3)) {
  
  trait_i <- which(names(trait_list) %in% maintrait3[i])
  
  # this should be combined with the overall at harvest which will be the baseline
  index_i <- as.vector(which(trait_list[[trait_i]]$keep))
  
  keep_i <- index_i %in% indicesbase3
  
  index_i <- index_i[keep_i]
  
  r_i <- rankTricot(dat[index_i, ],
                    itemnames,
                    c(trait_list[[trait_i]]$index_pos, trait_list[[trait_i]]$index_neg),
                    group = FALSE)
  
  
  # reset indices to match with grouped_rankings later
  index_i <- resetindices3[indicesbase3 %in% index_i]
  
  index3 <- c(index3, index_i)
  
  R3[[i]] <- r_i
  
}

Rphar <- do.call("rbind", R3)

Gphar <- group(Rphar, index = index3)

Gphar

mod_phar <- PlackettLuce(Gphar)

reliable3 <- reliability(mod_phar, ref = reference_geno)

reliable3 <- data.frame(items = names(rev(sort(reliable3))),
                        reliability = rev(sort(reliable3)))

# now select covariates
keep <- trait_list[[basetrait3]]$keep

sum(keep)

# check covars
covar3 <- covar[keep, ]

# refresh factor levels
covar3[1:ncol(covar3)] <- lapply(covar3[1:ncol(covar3)], function(x){
  if(class(x)=="factor"){
    x <- as.factor(as.character(x))
  }
  x
})

str(covar3)

# ..............................
# ..............................
# PlackettLuce with covariates
G3 <- group(R3[[1]], 1:length(R3[[1]]))

pld3 <- cbind(G = G3, covar3)

tree3 <- pltree(G ~ HDDSBadSeason + GenderMaleControl + HHSizeMAE + 
                  GenderFarmer + Region + PPI + CropDiv +
                  LandOwned + HouseholdType,
                alpha = 0.1,
                minsize = 40,
                gamma = TRUE,
                data = pld3,
                verbose = TRUE,
                npseudo = 21)

deviance(tree3)

node_rules(tree3)

# run this for the condensended rankings using predicted nodes
nodes_tree3 <- predict(tree3, type = "node")
node_id_tree3 <- sort(unique(nodes_tree3))

length(nodes_tree3) == length(Gphar)

tree3_mod <- list()
nobs_tree3 <- integer()

for (i in seq_along(node_id_tree3)) {
  
  Gi <- Gagro[nodes_tree3 == node_id_tree3[i]]
  
  tree3_mod[[i]] <- PlackettLuce(Gi)
  
  nobs_tree3 <- c(nobs_tree3, length(Gi))
  
  rm(Gi)
}

tree3_branch <- gosset:::build_tree_branches(tree3)
tree3_nodes <- gosset:::build_tree_nodes(tree3_mod, 
                                         log = TRUE,
                                         ci.level = 0.1,
                                         node.ids = node_id_tree3,
                                         n.obs = nobs_tree3)

ptree3 <- tree3_branch / tree3_nodes + plot_layout(heights =  c(1, 2))

ptree3

wr3 <- regret(tree3_mod, n1 = 1000)

reliable_tree3 <- lapply(tree3_mod, function(x){
  reliability(x, ref = reference_geno)
})

reliable_tree3 <- colMeans(do.call(rbind, reliable_tree3))

reliable_tree3 <- data.frame(items = names(rev(sort(reliable_tree3))),
                             reliability_tree = rev(sort(reliable_tree3)))

wr3 <- merge(wr3, reliable3, by = "items")
wr3 <- merge(wr3, reliable_tree3, by = "items")
wr3

wr3 <- wr3[order(wr3$worst_regret), ]

ggsave(filename = "output/postharvest_pltree.png",
       plot = ptree3,
       units = "cm",
       height = 30,
       width = 50,
       dpi = 600)

write.csv(wr3,
          "output/postharvest_risk_assessment.csv", row.names = FALSE)

# predict nodes of each farmer
keep_postharvest <- keep

nodes3 <- predict(tree3, newdata = covar, type = "node")

nodes3 <- data.frame(tricot_pkg = dat[,"tricot_pkg"], 
                     node = nodes3)

rules3 <- node_rules(tree3)

nodes_harv <- merge(nodes3, rules3, by = "node", all.x = TRUE)

head(nodes_harv)

names(nodes_harv)[c(1,3)] <- paste0(names(nodes_harv)[c(1,3)], "_tree3")

write.csv(nodes_harv, "output/postharvest_nodes.csv")

# get traits that each variety is best
top3 <- lapply(tree3_mod, function(x){
  names(rev(sort(coef(x, log = F))))[1:3]
})

top3 <- data.frame(items = unlist(top3), node = rep(node_id_tree3, each = 3))

top3$traits <- NA

best_i <- best_genos3[best_genos3$rank <= 3, ]

for (i in seq_along(top3$items)){
  item_i <- top3$items[i]

  top3$traits[i] <- paste(best_i$trait[best_i$items %in% item_i], collapse = ", ")
}

write.csv(top3, "output/postharvest_genotype_trait_performance_from_tree_nodes.csv", row.names = FALSE)

#..........
# write worth maps for each node
for (j in seq_along(node_id_tree3)) {
  
  modj <- list()
  nodesij <- predict(tree3, newdata = covar, type = "node")
  
  for(i in seq_along(traits3)) {
    
    trait_i <- which(names(trait_list) %in% traits3[i])
    
    # this should be combined with the overall at harvest which will be the baseline
    Rij <- dat[trait_list[[trait_i]]$keep & 
                 keep_agronomic &
                 nodesij == node_id_tree3[j], ]
    
    Rij <- rankTricot(Rij,
                      itemnames,
                      c(trait_list[[trait_i]]$index_pos, trait_list[[trait_i]]$index_neg),
                      group = FALSE)
    
    
    Rij <- pseudo_rank(Rij)
    
    Rij <- PlackettLuce(Rij)
    
    modj[[i]] <- Rij
    
    rm(Rij)
    
  }
  
  wmj <- worth_map(modj, labels = tname3) + 
    theme_bw() +
    theme(axis.text = element_text(color = "grey20"),
          strip.text.x = element_text(color = "grey20"),
          axis.text.x = element_text(angle = 40, vjust = 1, hjust=1),
          axis.text.y = element_text(angle = 40, vjust = 1, hjust=1))
  
  ggsave(paste0("output/postharvest_worth_map_node", 
                node_id_tree1[j], ".png"),
         plot = wmj,
         width = 23,
         height = 15,
         units = "cm",
         dpi = 600)
  
  rm(modj, wmj, nodesij)
}

#...............................
#...............................
# PlackettLuce with role as processor or grower
G4 <- group(R3[[1]], 1:length(R3[[1]]))

pld4 <- cbind(G = G4, covar3)

tree4 <- pltree(G ~ GenderFarmer + Processor + GenderProcessor,
                alpha = 0.1,
                minsize = 10,
                gamma = TRUE,
                data = pld4,
                verbose = TRUE,
                npseudo = 21)

node_rules(tree4)

# run this for the condensended rankings using predicted nodes
nodes_tree4 <- predict(tree4, type = "node")
node_id_tree4 <- sort(unique(nodes_tree4))

length(nodes_tree4) == length(Gphar)

tree4_mod <- list()
nobs_tree4 <- integer()

for (i in seq_along(node_id_tree4)) {
  
  Gi <- Gphar[nodes_tree4 == node_id_tree4[i]]
  
  tree4_mod[[i]] <- PlackettLuce(Gi)
  
  nobs_tree4 <- c(nobs_tree4, length(Gi))
  
  rm(Gi)
}

tree4_branch <- gosset:::build_tree_branches(tree4)
tree4_nodes <- gosset:::build_tree_nodes(tree4_mod, 
                                         log = TRUE,
                                         ci.level = 0.1,
                                         node.ids = node_id_tree4,
                                         n.obs = nobs_tree4)

ptree4 <- tree4_branch / tree4_nodes + plot_layout(heights =  c(1, 2))

ptree4

wr3 <- regret(tree3_mod, n1 = 1000)

reliable_tree3 <- lapply(tree3_mod, function(x){
  reliability(x, ref = reference_geno)
})

reliable_tree3 <- colMeans(do.call(rbind, reliable_tree3))

reliable_tree3 <- data.frame(items = names(rev(sort(reliable_tree3))),
                             reliability_tree = rev(sort(reliable_tree3)))

wr3 <- merge(wr3, reliable3, by = "items")
wr3 <- merge(wr3, reliable_tree3, by = "items")
wr3

wr3 <- wr3[order(wr3$worst_regret), ]

ggsave(filename = "output/postharvest_pltree.png",
       plot = ptree3,
       units = "cm",
       height = 30,
       width = 50,
       dpi = 600)

write.csv(wr3,
          "output/postharvest_risk_assessment.csv", row.names = FALSE)

# predict nodes of each farmer
keep_postharvest <- keep

nodes3 <- predict(tree3, newdata = covar, type = "node")

nodes3 <- data.frame(tricot_pkg = dat[,"tricot_pkg"], 
                     node = nodes3)

rules3 <- node_rules(tree3)

nodes_harv <- merge(nodes3, rules3, by = "node", all.x = TRUE)

head(nodes_harv)

names(nodes_harv)[c(1,3)] <- paste0(names(nodes_harv)[c(1,3)], "_tree3")

write.csv(nodes_harv, "output/postharvest_nodes.csv")

# get traits that each variety is best
top3 <- lapply(tree3_mod, function(x){
  names(rev(sort(coef(x, log = F))))[1:3]
})

top3 <- data.frame(items = unlist(top3), node = rep(node_id_tree3, each = 3))

top3$traits <- NA

best_i <- best_genos3[best_genos3$rank <= 3, ]

for (i in seq_along(top3$items)){
  item_i <- top3$items[i]
  
  top3$traits[i] <- paste(best_i$trait[best_i$items %in% item_i], collapse = ", ")
}

write.csv(top3, "output/postharvest_genotype_trait_performance_from_tree_nodes.csv", row.names = FALSE)

#..........
# write worth maps for each node
for (j in seq_along(node_id_tree3)) {
  
  modj <- list()
  nodesij <- predict(tree3, newdata = covar, type = "node")
  
  for(i in seq_along(traits3)) {
    
    trait_i <- which(names(trait_list) %in% traits3[i])
    
    # this should be combined with the overall at harvest which will be the baseline
    Rij <- dat[trait_list[[trait_i]]$keep & 
                 keep_agronomic &
                 nodesij == node_id_tree3[j], ]
    
    Rij <- rankTricot(Rij,
                      itemnames,
                      c(trait_list[[trait_i]]$index_pos, trait_list[[trait_i]]$index_neg),
                      group = FALSE)
    
    
    Rij <- pseudo_rank(Rij)
    
    Rij <- PlackettLuce(Rij)
    
    modj[[i]] <- Rij
    
    rm(Rij)
    
  }
  
  wmj <- worth_map(modj, labels = tname3) + 
    theme_bw() +
    theme(axis.text = element_text(color = "grey20"),
          strip.text.x = element_text(color = "grey20"),
          axis.text.x = element_text(angle = 40, vjust = 1, hjust=1),
          axis.text.y = element_text(angle = 40, vjust = 1, hjust=1))
  
  ggsave(paste0("output/postharvest_worth_map_node", 
                node_id_tree1[j], ".png"),
         plot = wmj,
         width = 23,
         height = 15,
         units = "cm",
         dpi = 600)
  
  rm(modj, wmj, nodesij)
}



# G4 <- rbind(R1[[1]], R3[[1]])
# G4 <- group(G4, index = 1:length(G4))
# 
# covar4 <- rbind(covar1, covar3)
# covar4$CropStage <- as.factor(c(rep("AgronomicPerformance", length(R1[[1]])), 
#                                 rep("PostHarvest", length(R3[[1]]))))
# 
# pld4 <- cbind(G = G4, covar4)
# 
# tree4 <- pltree(G ~ CropStage + GenderFarmer + Processor,
#                 alpha = 0.1,
#                 minsize = 40,
#                 gamma = TRUE,
#                 data = pld4,
#                 verbose = TRUE,
#                 npseudo = 21)
# 
# nodes_tree4 <- predict(tree4, type = "node")
# node_id_tree4 <- sort(unique(nodes_tree4))
# 
# length(nodes_tree4) == length(G4)
# 
# tree4_mod <- list()
# nobs_tree4 <- integer()
# for (i in seq_along(node_id_tree4)) {
#   
#   Gi <- G4[nodes_tree4 == node_id_tree4[i]]
#   
#   tree4_mod[[i]] <- PlackettLuce(Gi)
#   
#   nobs_tree4 <- c(nobs_tree4, length(Gi))
#   
#   rm(Gi)
# }
# 
# tree4_branch <- gosset:::build_tree_branches(tree4)
# 
# tree4_nodes <- gosset:::build_tree_nodes(tree4_mod, 
#                                          log = TRUE,
#                                          ci.level = 0.1,
#                                          node.ids = node_id_tree4,
#                                          n.obs = nobs_tree4)
# 
# ptree4 <- tree4_branch / tree4_nodes + plot_layout(heights =  c(1, 2))
# 
# ptree4
# 
# # predict nodes of each farmer
# keep_cropstage <- c(keep_agronomic, keep_postharvest)
# 
# nodes4 <- predict(tree4, type = "node")
# 
# nodes4 <- data.frame(tricot_pkg = c(dat$tricot_pkg, dat$tricot_pkg)[keep_cropstage], 
#                      node = nodes4)
# 
# rules4 <- node_rules(tree4)
# 
# nodes_cps <- merge(nodes4, rules4, by = "node", all.x = TRUE)
# 
# head(nodes_cps)
# 
# names(nodes_cps)[c(1,3)] <- paste0(names(nodes_cps)[c(1,3)], "_tree4")
# 
# nodes_cps <- nodes_cps[nodes_cps$node_tree4 != "5", ]
# 
# sum(duplicated(nodes_cps$tricot_pkg))
# 
# # merge two nodes with the main data
# dat2 <- merge(dat, nodes_agro, by = "tricot_pkg", all.x = TRUE)
# dat2 <- merge(dat2, nodes_harv, by = "tricot_pkg", all.x = TRUE)
# 
# write.csv(dat2, "output/tricot_data_with_pl_nodes.csv", row.names = FALSE)
# 
# write.csv(nodes_cps, "output/cropstages_tree_nodes_rules.csv", row.names = FALSE)



