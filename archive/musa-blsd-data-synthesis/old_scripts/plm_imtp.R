library(readr)
library(PlackettLuce)
library(janitor)
#library(magrittr)
library(gosset)
library(caret)
library(igraph)
library(GGally)
library(dplyr)

source(file = "scripts/correct_musa_names.R")


#load imtp data
load("data/processed_trial_data_imtp.rda")

#load climate indices for IMTP trials
load("data/climate/imtp_agera5_prec_ind.rda")
load("data/climate/imtp_agera5_temp_ind.rda")

#load Narvaez Thesis data
load("data/processed_narvaez.rda")

#load climate data for Narvaez thesis
load("data/climate/nvz_agera5_prec_ind.rda")
load("data/climate/nvz_agera5_temp_indices.rda")

#load mgis_6
#mgis_data <- read_csv("data/mgis/mgis_6/mgis_6_c1.csv")


#add column mean_yls to imtp1 data to match column in imtp3 data
yls_data_imtp1_all$mean_yls <- yls_data_imtp1_all$youngest_leaf_spotted

#add column mean_yls to imtp2 data to match column in imtp3 data
yls_data_imtp2_all$mean_yls <- yls_data_imtp2_all$youngest_leaf_spotted

#add additional id to imtp1 data to match imtp3 data
yls_data_imtp1_all$trial_r <- yls_data_imtp1_all$trial_id

#add additional id to imtp2 data to match imtp3 data
yls_data_imtp2_all$trial_r <- yls_data_imtp2_all$trial_id

#add additional id to imtp3 for each block
yls_data_imtp3_blocks$trial_r <- yls_data_imtp3_blocks$block_id


#
data_nvz$trial_r <- data_nvz$trial

data_nvz$mean_yls <- data_nvz$youngest_leaf_spotted_at_harvest


#Bind rows of the three phases
imtp_all <- dplyr::bind_rows(yls_data_imtp1_all,
                             yls_data_imtp2_all,
                             yls_data_imtp3_blocks,
                             data_nvz
                             )

#unique(imtp_all$trial_id)
#unique(imtp_all$trial_r[imtp_all$phase == 1])
unique(imtp_all$trial_r)

#Fix genotype names
imtp_all <- correct_names(imtp_all)

#check fixed genotype names
sort(unique(imtp_all$genotypes))

#
# imtp_all <- imtp_all[c(imtp_all$genotypes != "Local"), ]
# imtp_all <- imtp_all[c(imtp_all$genotypes != "Agbagba"), ]
# imtp_all <- imtp_all[c(imtp_all$genotypes != "BITA2"), ]
# imtp_all <- imtp_all[c(imtp_all$genotypes != "B7925"), ]
# imtp_all <- imtp_all[c(imtp_all$genotypes != "Burro Cemsa"), ]
# imtp_all <- imtp_all[c(imtp_all$genotypes != "Calcutta 4"), ]
# imtp_all <- imtp_all[c(imtp_all$genotypes != "EMB 402"), ]
# imtp_all <- imtp_all[c(imtp_all$genotypes != "EMB 403"), ]
# imtp_all <- imtp_all[c(imtp_all$genotypes != "SF 215 / NBA 14"), ]


imtp_all <- as.data.frame(imtp_all)

#imtp_all$trial_r <- as.integer(as.factor(imtp_all$trial_r))

#make rankings object with function rank_numeric
imtp_all_ranks <- gosset::rank_numeric(data = imtp_all,
                                       items = "genotypes", 
                                       input = "mean_yls", 
                                       id = "trial_r", 
                                       group = FALSE)

imtp_all_ranks

#Check network connectivity
imtp_adj <- PlackettLuce::adjacency(imtp_all_ranks)
imtp_adj
PlackettLuce::connectivity(imtp_adj)
PlackettLuce::connectivity(imtp_adj)[[1]]
imtp_net <- igraph::graph_from_adjacency_matrix(imtp_adj)


#____Check local variety in the original data, change to the known name!!!!!_______#
imtp_all[imtp_all$genotypes == "Local", ]
#remove genotype labeled as Local as it is only present in trial 16166 from IMTP-3

imtp_all_ranks <- imtp_all_ranks[, dimnames(imtp_all_ranks)[[2]] != "Local"]

#check the network again
imtp_adj <- adjacency(imtp_all_ranks)
imtp_adj
connectivity(imtp_adj)
connectivity(imtp_adj)[[1]]
imtp_net <- graph_from_adjacency_matrix(imtp_adj)

net <- network::network(imtp_adj)

ggnet2(net, label = T, node.color = "grey")


GGally::ggnet2(net, 
               label = TRUE,
               arrow.size = 8, 
               edge.color = "darkgrey",
               mode = "kamadakawai",
               arrow.gap = 0.03)


# remove Ingagara who belongs to a different network
#imtp_all_ranks <- imtp_all_ranks[, dimnames(imtp_all_ranks)[[2]] != "Ingagara"]
#imtp_all_ranks <- imtp_all_ranks[, dimnames(imtp_all_ranks)[[2]] != "Agbagba"]

# rank_matrix <- unclass(imtp_all_ranks)
# 
# # rank_matrix <- rank_matrix[, -c(colnames(rank_matrix) == "Local")]
# # rank_matrix <- rank_matrix[, -c(colnames(rank_matrix) == "BITA2")]
# # rank_matrix <- rank_matrix[, -c(colnames(rank_matrix) == "Burro Cemsa")]
# ncol(rank_matrix)
# 
# #remove varieties that cause Cholmod error
# rank_matrix <- unclass(imtp_all_ranks)
# for (i in 2:ncol(rank_matrix)){
#   matx <- rank_matrix[, c(1:i)]
#   print(i)
#   print(matx)
#   rank_test <- as.rankings(matx)
#   plm1 <- PlackettLuce(rank_test)
#   print(summary(plm1))
# }

imtp_all <- correct_names(imtp_all)

trial_x_gent <- data.frame(rbind(table(imtp_all$genotypes, imtp_all$trial_r)))

trial_x_gent

sort(rowSums(trial_x_gent))

unique(imtp_all$genotypes)

rowSums(trial_x_gent)[rowSums(trial_x_gent) <= 2]

gent_to_remove <- names(rowSums(trial_x_gent)[rowSums(trial_x_gent) <= 10])

imtp_all <- imtp_all[!imtp_all$genotypes %in% gent_to_remove, ]


imtp_all_ranks <- gosset::rank_numeric(data = imtp_all,
                                       items = "genotypes", 
                                       input = "mean_yls", 
                                       id = "trial_r", 
                                       group = FALSE)

imtp_all_ranks

rowSums(trial_x_gent)

hist(rowSums(trial_x_gent))

sum(rowSums(trial_x_gent))

ncol(trial_x_gent)

rownames(trial_x_gent)
sum(trial_x_gent[1,] < 1)

pheatmap::pheatmap(as.matrix(table(imtp_all$genotypes, imtp_all$trial_r)),
                        display_numbers = T)




##########################
#Plackett-Luce Model
# 
imtp_all_ranks <- gosset::rank_numeric(data = imtp_all, 
                               items = "genotypes", 
                               input = "mean_yls", 
                               id = "trial_r", 
                               group = F)

imtp_all_plm1 <- PlackettLuce(imtp_all_ranks, 
                              npseudo = 0.5,
                              ncores = 7,
                              verbose = F)
 
summary(imtp_all_plm1, ref = 1)
 
imtp_all_qv <- qvcalc(imtp_all_plm1)
#summary(imtp_all_qv)

est <- imtp_all_qv$qvframe$estimate
qvs2 <- imtp_all_qv
qvs2$qvframe <- imtp_all_qv$qvframe[order(est, decreasing = TRUE), , drop = FALSE]
plot(qvs2, las = 2)

par(mar = c(10, 5, 5, 5))

#custom plot
plot(qvs2, 
     las = 2, 
     cex = 1.5, 
     pch = 18, 
     xlab = "",
     col = "bisque4",
     #main = "Ranking of common bean genotypes",
     main = "",
     ylab = "log(worth) estimate")
 
plot(imtp_all_qv, 
     cex = 1.5, 
     pch = 18, 
     xlab = "",
     
     las = 2, 
     main = "Reaction to Black Leaf Streak")
 
nrow(imtp_all_ranks)
 
imtp_loc_dates <- (bind_rows(imtp1_dates,
                             imtp2_dates,
                             imtp3_dates))

prec_data <- bind_rows(imtp_agera5_prec_ind, nvz_agera5_prec_ind)

temp_data <- bind_rows(imtp_agera5_temp_indices, nvz_agera5_temp_indices)

env_data <- cbind(prec_data, temp_data)

#imtp_data <- cbind(imtp_all_ranks, imtp_agera5_prec_ind, imtp_agera5_temp_indices)

imtp_data <- cbind(imtp_all_ranks, env_data)

#imtp_data <- cbind(imtp_all_ranks, env_data[imtp_all_ranks,])


country <- imtp_all %>%
        group_by(trial_r)%>%
        select(country) %>%
        unique()

#imtp_data <- cbind(imtp_all_ranks, imtp_agera5_prec_ind, imtp_agera5_temp_indices, "country" = as.factor(country$country))


#calculate minsize for PLT
ceiling((nrow(imtp_data) * 0.05))

loo_folds <- 1:nrow(imtp_data)

#Forward selection of covariates
imtp_f <- forward(imtp_all_ranks ~ .,
                  data = imtp_data,
                  #k = 10,
                  folds = loo_folds,
                  select.by = "deviance",
                  ncores = 2,
                  packages = "PlackettLuce",
                  seed = 123,
                  alpha = 0.9,
                  verbose = T,
                  breakties = T,
                  #npseudo = 50,
                  #minsize = 24
                  )


covars <- imtp_data[,-1]
var_to_remove <- caret::nearZeroVar(covars, freqCut = 70/30)

names(covars[var_to_remove])

imtp_all_ranks <- gosset::rank_numeric(data = imtp_all, 
                                       items = "genotypes", 
                                       input = "mean_yls", 
                                       id = "trial_r", 
                                       group = T) 
        
        

#Fit the PlackettLuce Treee
imtp_plt_1 <- pltree(imtp_all_ranks ~ .,
                     data = imtp_data,
                     alpha = 1,
                     ncores = 7,
                     npseudo = 1,
                     #minsize = 1,
                     verbose = T,
                     bonferroni = F)



plot(imtp_plt_1)

imtp_plt_1 <- pltree(imtp_all_ranks ~ Rtotal,
                     data = imtp_data,
                     #gamma = TRUE,
                     alpha = 1,
                     npseudo = 10,
                     verbose = T,
                     cores = 7)

plot(imtp_plt_1)


imtp_plt_1 <- pltree(imtp_all_ranks ~  maxDT +   minDT +   maxNT  +   minNT,
                     data = imtp_data,
                     npseudo = 1050,
                     breakties = T,
                     alpha = 0.9,
                     ref = 2,
                     verbose = T,
                     minsize = 10
                     )

plot(imtp_plt_1)



imtp_f <- forward(imtp_all_ranks ~ ., data = cbind(imtp_all_ranks, rain_ind_imtp),
                  k = 10,
                  select.by = "MaxLik",
                  ncores = 2,
                  packages = "PlackettLuce",
                  seed = 123)







imtp_2_ranks <- gosset::rank_numeric(data = yls_data_imtp2_all,
                                     items = "genotypes", 
                                     input = "mean_yls", 
                                     id = "trial_r", 
                                     group = T)

imtp_2_ranks

imtp_2_pl <- PlackettLuce::PlackettLuce(imtp_2_ranks)

imtp_2_pl

plot(qvcalc::qvcalc(imtp_2_pl))


imtp_2_dat <- cbind(imtp_2_ranks, imtp_agera5_prec_ind[imtp_2_ranks,])

imtp_2_plt <- PlackettLuce::pltree(imtp_2_ranks ~ .,
                                   data = imtp_2_dat,
                                   alpha = 1,
                                   npseudo = 1.5,
                                   minsize = 1)

imtp_2_plt



