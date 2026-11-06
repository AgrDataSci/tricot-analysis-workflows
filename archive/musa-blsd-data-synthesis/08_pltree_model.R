library(gosset)
library(PlackettLuce)
library(ggplot2)
library(ggparty)
library(patchwork)
library(caret)
library(stablelearner)

source(file = "scripts/utils/plot_tree_musa_trials.R")

#load trial and climate data
source("scripts/utils/load_data.R")

genotypes_info <- read.csv("data/processed/genotypes_info.csv")

genotypes_info <- janitor::clean_names(genotypes_info)

head(musa_trials_data)

head(musa_trials_climate)

total_genotypes <- ncol(musa_trials_data)

total_genotypes

sort(colnames(musa_trials_data))

#remove variables with near zero variability
vars_to_remove <- caret::nearZeroVar(musa_trials_climate, names = TRUE)

vars_to_remove

musa_trials_climate <- musa_trials_climate[!colnames(musa_trials_climate) %in% vars_to_remove]

caret::findCorrelation(cor(musa_trials_climate),names = TRUE)

png(filename = "output/figures/covariates_correlation.png",
    width = 16, height = 10, res = 300,units = "in")

pheatmap::pheatmap(cor(musa_trials_climate),
                   display_numbers = TRUE,
                   number_color = "gray10",
                   angle_col = 0,
                   cluster_rows = FALSE,
                   cluster_cols = FALSE)

dev.off()

#Check network connectivity

musa_ranks <- as.rankings(musa_trials_data)

musa_trials_adj <- adjacency(musa_ranks)

pl_conn <- PlackettLuce::connectivity(musa_trials_adj)

pl_conn

unconnected <- which(pl_conn$membership != 1)

unconnected

#Remove unconnecte genotypes
musa_trials_data <- musa_trials_data[,!colnames(musa_trials_data) %in% names(unconnected)]
nrow(musa_trials_data)
ncol(musa_trials_data)

#musa_trials_data <- musa_trials_data[,!colnames(musa_trials_data) %in% genotypes_remove]

#### Check and remove ties ####
best_genotype <- vector(mode = "list", length = nrow(musa_trials_data)) 
worst_genotype <- vector(mode = "list", length = nrow(musa_trials_data))

tied_best <- vector(mode = "numeric", length = nrow(musa_trials_data))

tied_worst <- vector(mode = "numeric", length = nrow(musa_trials_data))

for(i in 1:nrow(musa_trials_data)){
  
  best_genotype[[i]]<-  which(musa_trials_data[i,] == 1)
  tied_best[[i]] <- ifelse(length(best_genotype[[i]]) > 1, i, NA) 
  worst_genotype[[i]] <-  which(musa_trials_data[i,] == max(musa_trials_data[i,]))
  tied_worst[[i]] <- ifelse(length(worst_genotype[[i]]) > 1, i, NA) 
  
}

best_genotype
worst_genotype

tied_best <- which(!is.na(tied_best))
tied_worst <- which(!is.na(tied_worst))

tied_best

tied_worst

set.seed(123)
#Remove genotypes tied in last position
for(i in seq_len(nrow(musa_trials_data))){
  
  #option 1 - Break ties assigning new rnd values to worst tied genotypes
  # wrst_genotypes <- names(which(musa_trials_data[i,] == max(musa_trials_data[i,])))
  # 
  # if(length(wrst_genotypes) > 1){
  #   max_value <- max(musa_trials_data[i, ])
  #   wrst_n <- length(wrst_genotypes)
  # 
  #   new_worst_rank <- sample(x = max_value:(max_value + wrst_n), size = wrst_n, replace = FALSE)
  # 
  #   musa_trials_data[i, wrst_genotypes] <- new_worst_rank
  # }
  # 
  # 
  # bst_genotypes <- names(which(musa_trials_data[i,] == 1))
  # 
  # if(length(bst_genotypes) > 1){
  #   
  #   #from the tied at top, keep one and remove the rest
  #   keep_ind <- sample(x = length(bst_genotypes), size = 1)
  #   
  #   musa_trials_data[i, bst_genotypes][-keep_ind] <- 0
  #   
  # }

  print(i)
  
  #Option 2, simple set all ties to 0 to drop them
  musa_trials_data[i, which(duplicated(musa_trials_data[i, ]) &
                              musa_trials_data[i, ] > 0)] <- 0


}


ncol(musa_trials_data)
nrow(musa_trials_data)
length(musa_trials_data[i,])

#### Check and remove underrepresented genotypes

sort(round(colSums(musa_trials_data == 0)/nrow(musa_trials_data) * 100))

absence_perc <- round(colSums(musa_trials_data == 0)/nrow(musa_trials_data) * 100)

sort(absence_perc)

#Keep only genotypes present in at least 95% of the rankings
to_keep <- names(which(absence_perc <= 95))

musa_trials_data <- musa_trials_data[, colnames(musa_trials_data) %in% to_keep]


#### Fit Plackett-Luce tree ####

colnames(musa_trials_data)
ncol(musa_trials_data)

musa_ranks <- as.rankings(musa_trials_data)

musa_ranks <- group(musa_ranks, seq_along(musa_ranks))

musa_data <- cbind(musa_ranks, musa_trials_climate)

musa_plt_01 <- pltree(musa_ranks ~  .,
                      data = musa_data,
                      npseudo = 1.5,
                      alpha = 0.05,
                      bonferroni = T,
                      verbose = TRUE,
                      ncores = 7,
                      minsize = floor(nrow(musa_data)/3)
                      )

#summary(musa_plt_01)

#plot(musa_plt_01, worth = F, ref = 1)

plot_tree_musa(musa_plt_01, log = T , qve = T)


ggsave("output/figures/musa_trial_plt_01.png", dpi = 600, width = 22, height = 20)

#sampling with 80%
set.seed(123)
stb_01 <- stablelearner::stabletree(musa_plt_01,
                                    sampler = stablelearner::subsampling, 
                                    savetrees = TRUE, 
                                    B = 1000, 
                                    v = 0.80)

#save(stb_01, file = "output/models/stb_01.rda")

#load(file = "output/models/stb_01.rda")

summary(stb_01)

png(filename = "output/figures/stb_01_vsf.png", width = 16, height = 10, res = 600,units = "in")
barplot(stb_01)
dev.off()

png("output/figures/stb_01_vs.png",  width = 16, height = 10, res = 600,units = "in")
image(stb_01)
dev.off()

#plot(stb_01, select = c("MLDS", "SDII", "rhum_09", "CSDI", "DTR"))

png("output/figures/stb_01_cop.png",  width = 16, height = 10, res = 600,units = "in")
plot(stb_01, select = c("MLDS"))
dev.off()


stablelearner:::breaks_barplot()
stb_01$brclasses

cutpoins_MLDS <- stb_01$br$MLDS

cutpoins_MLDS <- data.frame(table(cutpoins_MLDS))

names(cutpoins_MLDS)[1] <- "MLDS"

cutpoins_MLDS$initial <- ifelse(cutpoins_MLDS$MLDS == 11, "yes", "no")

cutpoins_MLDS

ggplot() + 
  geom_col(data = cutpoins_MLDS, aes(x = MLDS,
                                     y = Freq,
                                     fill = initial)) + 
  labs(x = "Cutpoint",
       y = "Counts",
       title = "MLDS",
       ) + 
  scale_fill_manual(values = c("yes" = "#b10026", "no" = "gray45")) + 
  theme(legend.position = "none")

ggsave("output/figures/cop_stb_01.png", dpi = 300, width = 8, height = 4)



rel_freqs <- colSums(stb_01$vs) / 1000 * 100

rel_freqs_df <- data.frame("variable" = names(rel_freqs), "rel_freq" = rel_freqs)

rel_freqs_df$initial <- ifelse(rel_freqs_df$variable == "MLDS", "yes", "no") 


ggplot() + 
  geom_col(data = rel_freqs_df, aes(x = reorder(variable, -rel_freq),
                                    y = rel_freqs,
                                    fill = initial)) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 35, hjust = 1)) + 
  labs(x = "Variables",
       y = "Frequency (%)") + 
  scale_fill_manual(values = c("yes" = "#b10026", "no" = "gray35"))


ggsave("output/figures/rel_freq_stb_01.png", dpi = 300, width = 8, height = 4)


### t-tests

genotypes_info

#Node 2 - Humid conditions

node_2 <- itempar(musa_plt_01[2], vcov = F, log = T)

node_2 <- data.frame("genotypes" = colnames(node_2),
                     "worth" = as.numeric(node_2))

n2_hyb <- node_2[node_2$genotypes %in% genotypes_info[genotypes_info$biological_a_status != "Landrace",
                                                   "i_name"], ]

n2_lnd <- node_2[!node_2$genotypes %in% genotypes_info[genotypes_info$biological_a_status != "Landrace",
                                                      "i_name"], ]

boxplot(n2_hyb[, 2], n2_lnd[, 2], names = c("Hybrids", "Landraces"))

t.test(n2_hyb[, 2], n2_lnd[, 2])



#Node 3 - Dry Conditions

node_3 <- itempar(musa_plt_01[3], vcov = F, log = T)

node_3 <- data.frame("genotypes" = colnames(node_3),
                     "worth" = as.numeric(node_3))

n3_hyb <- node_3[node_3$genotypes %in% genotypes_info[genotypes_info$biological_a_status != "Landrace",
                                                      "i_name"], ]

n3_lnd <- node_3[!node_3$genotypes %in% genotypes_info[genotypes_info$biological_a_status != "Landrace",
                                                       "i_name"], ]

boxplot(n3_hyb[, 2], n3_lnd[, 2], names = c("Hybrids", "Landraces"))

t.test(n3_hyb[, 2], n3_lnd[, 2]) 

library(multcomp)

multcomp::
  
node_3$status <- ifelse(node_3$genotypes %in% genotypes_info[genotypes_info$biological_a_status
                                                             == "Landrace", "i_name"],
                        0,
                        1)

node_3$status <- as.factor(node_3$status)

aov_node3 <- lm(worth ~  genotypes_f * status, node_3)
summary(aov_node3)
a <-  anova(aov_node3)
summary.aov(aov_node3)
TukeyHSD(aov_node3), "Calcutta 4")
library(emmeans)
emmeans(noise.lm, pairwise ~ size)
emmeans(aov_node3,  ~ status)

emmeans::pwpp(emmeans(aov_node3,  ~ status))

node_3 <- qvcalc::qvcalc(musa_plt_01[[2]])$qvframecoef(node_2)

coef(musa_plt_01[[2]])

# get estimates from item parameters using qvcalc


node_2 <- data.frame("name" = names(node_2), "estimate" = node_2)

node_3 <- coef(musa_plt_01[[3]])

node_3 <- data.frame("name" = names(node_3), "estimate" = node_3)

node_2 <- merge(genotypes_info, node_2, by.x = "i_name", by.y = "name")





nrow(coeffs)

genotype_coeffs <- merge(genotypes_info, coeffs, by.x = "i_name", by.y = "items")

nrow(genotype_coeffs)

node_2 <- genotype_coeffs[genotype_coeffs$id == 2, ]

node_3 <- genotype_coeffs[genotype_coeffs$id == 3, ]

node_2_means <- aggregate(estimate ~ biological_a_status, FUN = "mean", data = node_2)

node_2_means <- data.frame("type" = node_2_means$biological_a_status[1:2],
                           "est" = node_2_means$estimate[1:2])

aggregate(estimate ~ biological_a_status, FUN = "mean", data = node_3)

t.test(node_2[node_2$biological_a_status == "Hybrid",]$estimate,
       node_2[node_2$biological_a_status == "Landrace",]$estimate)


t.test(node_3[node_3$biological_a_status == "Hybrid",]$estimate,
       node_3[node_3$biological_a_status == "Landrace",]$estimate)


plot(node_3[node_3$biological_a_status == "Landrace",]$estimate ,
            node_3[node_3$biological_a_status == "Landrace",]$i_name)


p1 <- ggplot(node_2[node_2$biological_a_status == "Landrace",])+ 
  aes(x = i_name, y = estimate) + geom_point()+
  theme(axis.text.x = element_text(angle = 35, hjust = 1))+
  labs(title = "Landraces performance with dry spell <= 11 days",
       x =  "Genotypes",
       y = "(log) worth")


p2 <- ggplot(node_3[node_3$biological_a_status == "Landrace",])+ 
aes(x = i_name, y = estimate) + geom_point()+
  theme(axis.text.x = element_text(angle = 35, hjust = 1))+
  labs(title = "Landraces performance with dry spell > 11 days",
       x =  "Genotypes",
       y = "")

p3 <- ggplot(node_2[node_2$biological_a_status == "Hybrid" |
                      node_2$biological_a_status == "Somaclonal variant", ])+ 
  aes(x = i_name, y = estimate) + geom_point()+
  theme(axis.text.x = element_text(angle = 35, hjust = 1))+
  labs(title = "Hybrids performance with dry spell <= 11 days",
       x =  "Genotypes",
       y = "(log) worth")


p4 <- ggplot(node_3[node_3$biological_a_status == "Hybrid" |
                      node_3$biological_a_status == "Somaclonal variant", ])+ 
  aes(x = i_name, y = estimate) + geom_point()+
  theme(axis.text.x = element_text(angle = 35, hjust = 1))+
  labs(title = "Hybrids performance with dry spell > 11 days",
       x =  "Genotypes",
       y = "") 
  
  
library(ggforce)

c4$ <- kmeans(node_3[node_3$biological_a_status == "Hybrid" |
                node_3$biological_a_status == "Somaclonal variant", ]$estimate, 3)


(p1 + p2) / (p3 + p4)

ggsave("output/figures/worth_musa.png", dpi = 300, width = 16, height = 8)   

library(cluster)

node_3

autoplot(pam(node_3$estimate, 3)), frame = TRUE, frame.type = 'norm')


cluster::clara(node_3$estimate, 3)


###################################
gosset::reliability(musa_plt_01, ref = "Calcutta 4")

worst_regret()
worth_map(musa_plt_01)


#bootstrapping
stb_02 <- stablelearner::stabletree(musa_plt_01,
                                    sampler = stablelearner::bootstrap, 
                                    savetrees = TRUE, 
                                    B = 1000, 
                                    v = 1)

save(stb_02, file = "output/models/stb_02.rda")

summary(stb_02)



png(filename = "output/figures/stb_02_vsf.png", width = 16, height = 10, res = 300,units = "in")
barplot(stb_02)
dev.off()

png("output/figures/stb_02_vs.png",  width = 16, height = 10, res = 300,units = "in")
image(stb_02)
dev.off()

png("output/figures/stb_02_cop.png",  width = 16, height = 10, res = 300,units = "in")
plot(stb_02, select = c("MLDS")), "SDII"), "rhum_09", "CSDI", "DTR"))
dev.off()


stablelearner::accuracy(stb_02)


bootstrap(B = 500, v = 1)



plt_models

lapply(plt_models, gosset::node_rules)

# sampleweights <- rexp(n, 1)/100
# sampleweights <- sampleweights / sum(sampleweights


################# OLD code #######################

a <- samplesplitting(100)
a$sampler(94)

b <- bootstrap(B = 1000, v = .8)
b$method
musa_boot <- b$sampler(94)
musa_boot

length(musa_boot[1,])
musa_data[musa_boot[1,],]

boot_list <- vector(mode = "list", length = 500)

boot_list <- lapply(boot_list, function(X) sample(x = 94, replace = TRUE))

mtry <- round(sqrt(ncol(musa_data) - 1))

custom_plt <- function(data){
  
  
  res <- pltree(musa_ranks ~ .,
         data = data,
         npseudo = 0.5,
         alpha = .9,
         bonferroni = TRUE,
         verbose = TRUE,
         ncores = 7,
         dfsplit = FALSE,
         prune = "AIC",
         breakties = TRUE,
         mtry = mtry,
         minsize = floor(nrow(musa_data)/3)
         )
  
  return(res)
  
}

pltree_forest <- lapply(boot_list, function(X) custom_plt(musa_data[X, ]))


gosset:::node_labels(pltree_forest[[1]])
lapply(pltree_forest, gosset::node_rules)

labels <- lapply(pltree_forest, gosset::node_labels)


w_splits <- lapply(labels, function(X) length(X) > 0)
w_splits
w_splits <- which(unlist(w_splits))
length(w_splits)

pltree_forest[w_splits] 

library(gosset, lib.loc = "E:/gosset_paper/")

cv_10k <- crossvalidation(musa_ranks ~ .,
                      data = musa_data,
                      npseudo = 0.5,
                      alpha = .9,
                      bonferroni = TRUE,
                      verbose = TRUE,
                      ncores = 7,
                      dfsplit = FALSE,
                      prune = "AIC",
                      mean.method = "Z-Tex",
                      minsize = floor(nrow(musa_data)/3),
                      k = 10,#nrow(musa_data),
                      #folds = seq_len(nrow(musa_data))
                      seed = 123
                      )

cv_10k




cv_loo <- crossvalidation(musa_ranks ~ .,
                          data = musa_data,
                          npseudo = 0.5,
                          alpha = .9,
                          bonferroni = TRUE,
                          verbose = TRUE,
                          ncores = 7,
                          dfsplit = FALSE,
                          prune = "AIC",
                          minsize = floor(nrow(musa_data)/3),
                          k = nrow(musa_data),
                          folds = seq_len(nrow(musa_data))
                          method)

cv_loo
library(gosset, lib.loc = "E:/gosset_paper/")



# forced tree with second important variable
musa_plt_02 <- pltree(musa_ranks ~ SDII,
                      data = musa_data,
                      npseudo = 1.5,
                      alpha = .9,
                      bonferroni = T,
                      verbose = TRUE,
                      ncores = 7,
                      dfsplit = FALSE,
                      prune = "AIC",
                      # weights = weights,
                      # caseweights = TRUE,
                      minsize = floor(nrow(musa_data)/3))


plot_tree_musa(musa_plt_02, log = T , qve = T)

ggsave("output/figures/musa_trial_plt_02.png", dpi = 300, width = 16, height = 12)




# forced tree with third important variable
musa_plt_03 <- pltree(musa_ranks ~ rhum_09,
                      data = musa_data,
                      npseudo = 1.5,
                      alpha = .9,
                      bonferroni = T,
                      verbose = TRUE,
                      ncores = 7,
                      dfsplit = FALSE,
                      prune = "AIC",
                      # weights = weights,
                      # caseweights = TRUE,
                      minsize = floor(nrow(musa_data)/3))


plot_tree_musa(musa_plt_03, log = T , qve = T)

ggsave("output/figures/musa_trial_plt_03.png", dpi = 300, width = 16, height = 12)


pseudoR2(update(musa_plt_01, ~1))

pseudoR2(musa_plt_01)

p1 <- predict(musa_plt_01, newdata = musa_data, type = "rank")

o1 <- musa_trials_data

kw <- mean(unlist(lapply(X = seq_len(nrow(p1)),
                         FUN = function(X){
                           #print(X)
                           a <- o1[X, ]
                           b <- p1[X,]
                           #b[b==0] <-NA
                           a <- a[a > 0]
                           #print(a)
                           b <- b[names(b) %in% names(a)]
                           
                           #print(b)
                           
                           DescTools::KendallW(data.frame("obs" = a,
                                                          "pred" = b),
                                               correct = TRUE,
                                               test = TRUE)$estimate}
)
)
)
round(kw, 3)





