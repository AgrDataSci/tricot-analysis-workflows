library(gosset)
library(psychotree)
library(ggparty)

#Fit a Bradley-Terry Tree model


#load trial data
source("scripts/utils/load_data.R")

#load climate data
musa_trials_climate <- read.csv("data/climate/musa_trials_climate.csv")

#remove variables with near zero variability
vars_to_remove <- caret::nearZeroVar(musa_trials_climate, names = TRUE)

musa_trials_climate <- musa_trials_climate[!colnames(musa_trials_climate) %in% vars_to_remove]


musa_trials_data <- musa_trials_data[,!colnames(musa_trials_data) %in% c("GCTCV-215",
                                                                         "Foconah",
                                                                         "Pisang Lemak Manis",
                                                                         "Selangor",
                                                                         "TMBx91283",
                                                                         "Formosana")]

musa_ranks <- as.rankings(musa_trials_data)

musa_ranks


musa_pcomp <- gosset::rank_paircomp(musa_ranks)

musa_mod_data <- cbind(musa_pcomp, musa_trials_climate)

musa_bttree <- bttree(musa_pcomp ~ Rtotal + maxDT + minDT + maxNT + minNT,
                      data = musa_mod_data,
                      alpha = 0.05,
                      bonferroni = TRUE,
                      verbose = TRUE,
                      cores = 6)


boxplot(worth(imtp_tree), las = 2)



png(filename = "plots/imtp_bttree_hr_001.png", width = 1920, height = 1020, res = 105)

plot(imtp_tree,
     names = F,
     worth = T)

dev.off()

Summary(imtp_tree)
sum(worth(imtp_tree)[2,])


write.csv(as.data.frame(table(imtp_all$genotypes)), file = "data/genotypes_imtp_blsd_freq.csv")

  