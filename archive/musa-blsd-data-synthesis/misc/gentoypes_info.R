

source(file = "scripts/utils/plot_tree_musa_trials.R")

#load trial and climate data
source("scripts/utils/load_data.R")

head(musa_trials_data)

head(musa_trials_climate)


nrow(musa_trials_data)

genotypes <- data.frame("name" = sort(colnames(musa_trials_data)),
                        "type" = rep("", ncol(musa_trials_data)),
                        "blsd" = rep("", ncol(musa_trials_data)))


genotypes

#write.csv(genotypes, file = "data/processed/genotypes_info_1.csv", row.names = FALSE)

genotypes$type <- rep("", nrow(genotypes))


genotypes



genotypes <- read.csv("data/processed/musa_genotypes_info_v003.csv")

genotypes$blsd_react <- as.numeric(as.factor(genotypes$BLSD.reaction))


genotypes

cor(genotypes$blsd_react)












