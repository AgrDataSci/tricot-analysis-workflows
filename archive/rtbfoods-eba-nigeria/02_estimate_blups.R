library("lme4")
library("car")

feat1 <- read.csv("data/biophysical-features.csv")

feat2 <- read.csv("data/eba-texture-features.csv")

str(feat1)
str(feat2)

# coerce to factors and make the check variety to be the first
lvl <- union("TMEB3", sort(unique(feat2$genotype)))

feat1$genotype <- factor(feat1$genotype,
                         levels = lvl)

feat2$genotype <- factor(feat2$genotype,
                         levels = lvl)

str(feat1)
str(feat2)

# # check correlation
# cor1 <- cor(feat1[-1])
# 
# cor1[cor1 < 0.7 & cor1 > -0.7 | cor1 == 1] <- NA
# 
# rmv <- names(which(rowSums(cor1, na.rm = TRUE) != 0))
# 
# rmv <- !names(feat1) %in% rmv
# 
# feat1 <- feat1[rmv]
# 
# head(feat1)
# 
# # the same for the other 
# cor2 <- cor(feat2[-1])
# 
# cor2[cor2 < 0.7 & cor2 > -0.7 | cor2 == 1] <- NA
# 
# rmv <- names(which(rowSums(cor2, na.rm = TRUE) != 0))
# 
# rmv <- !names(feat2) %in% rmv
# 
# feat2 <- feat2[rmv]
# 
# head(feat2)

# compute the linear estimates
features <- data.frame(genotype = lvl)

features

metrics <- names(feat1)[-1]

for(i in seq_along(metrics)){
  form <- paste0(metrics[i], "~ 1 + (1 | genotype)")
  
  form <- as.formula(form)
  
  mod_i <- lmer(form, data = feat1)
  
  blups <- ranef(mod_i)$genotype
  
  features <- cbind(features, e = blups[,1])
  
}

names(features)[-1] <- metrics

head(features)

# now the same for the other data.frame
features2 <- data.frame(genotype = lvl)

features2

metrics2 <- names(feat2)[-1]

for(i in seq_along(metrics2)){
  form <- paste0(metrics2[i], "~ 1 + (1 | genotype)")
  
  form <- as.formula(form)
  
  mod_i <- lmer(form, data = feat2)
  
  blups <- ranef(mod_i)$genotype
  
  features2 <- cbind(features2, e = blups[,1])
  
}

names(features2)[-1] <- metrics2

# merge data 
features <- merge(features, features2, by = "genotype")

sel <- c("genotype", "percent_soluble", 'color_l', 'color_a', 'color_b',
  'cohesiveness', 'gumminess', 'chewiness', 'hardness_g', 'adhesiveness_g_sec_1', 
  'springiness_percent')

features <- features[,sel]

write.csv(features, "data/blups-cassava.csv", row.names = FALSE)

