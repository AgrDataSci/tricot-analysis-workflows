library(lme4)
library(nlme)
library(KenSyn)
library(metafor)
library(emmeans)
library(car) # version 2.1-6
library(outliers) # Version: 0.14
library(dplyr)


library(gamlss)


gamlss1 <- gamlss(mean_yls ~ genotypes + 
                    MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day + R95p  + R99p  + Rtotal +  SDII + 
                    maxDT + minDT + maxNT + minNT +  DTR  +  WSDI + CSDI + T10p + T90p, 
                  data = na.omit(yls_imtp_all))


summary(gamlss1)
# Model adjustment
res.lmer <- lmer(rendement ~ variete + (1|experimentation) + 
                   (1|experimentation:variete) + (1|experimentation:bloc), data=DF,
                 na.action=na.exclude)
res.lmer

yls_data_imtp3_blocks$env_ID <- as.integer(as.factor(yls_data_imtp3_blocks$trial_r))+19


yls_data_imtp3_blocks <- inner_join(yls_data_imtp3_blocks, imtp_agera5_prec_ind, by = c("envID" = "id"))

yls_data_imtp3_blocks <- inner_join(yls_data_imtp3_blocks, imtp_agera5_temp_indices, by = c("envID" = "id"))


yls_data_imtp3_blocks$genotypes <- as.factor(yls_data_imtp3_blocks$genotypes)

lm3 <- lmer(mean_yls ~ genotypes + (1|trial_id) + (1|trial_id:genotypes) + (1|block:genotypes) +
              MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day + R95p  + R99p  + Rtotal +  SDII + 
              maxDT + minDT + maxNT + minNT +  DTR  +  WSDI + CSDI + T10p + T90p, 
            data = yls_data_imtp3_blocks)

summary(lm3)



nlme

vcov(lm3)
imtp1_yls <- yls_data_imtp1_all

yls_data_imtp1_all$envID <- as.integer(as.factor(yls_data_imtp1_all$trial_r))

imtp_agera5_temp_indices$id <- 1:nrow(imtp_agera5_temp_indices)

imtp_agera5_prec_ind$id <- 1:nrow(imtp_agera5_prec_ind)


yls_data_imtp1_all <- inner_join(yls_data_imtp1_all, imtp_agera5_temp_indices, by = c("envID"="id"))

yls_data_imtp1_all <- inner_join(yls_data_imtp1_all, imtp_agera5_prec_ind, by = c("envID"="id"))

yls_data_imtp1_all



colnames(yls_data_imtp1_all)
lm1 <- lmer(youngest_leaf_spotted ~ genotypes + (1|trial_r) + maxDT + minDT, data = yls_data_imtp1_all)

summary(lm1)
lm1

#example code from the Makowski book------
# ANOVA table
Anova(lm1, test.statistic="F")
# Adjusted means
moy_var <- lsmeans(lm1, ~ genotypes)
moy_var
# Pairwise comparisons
pairs(moy_var, adjust = "tukey")
cld(moy_var, Letters = c(LETTERS))
# Comparisons with the general mean
contrast(moy_var, method="eff", adjust="sidak")
confint(contrast(moy_var, method="eff", adjust="sidak"))
# Comparison with a control
contrast(moy_var, method="trt.vs.ctrl", ref=2)
confint(contrast(moy_var, method="trt.vs.ctrl", ref=2))
# Random effects
ranef(lm1)
# Variance of the means and variance of the interaction
Vmoy <- 0.5*mean(VMCj) # VMCj = VMC vector per experiment
Vmoy
var_intera <- summary(res.lmer)$sigma^2 – Vmoy
var_intera
#---------------------------------


imtp1_hybrids <- c("FHIA-01", "FHIA-02", "FHIA-03", "FHIA-04", "FHIA-05", "FHIA-06", "FHIA-07")


imtp1_yls$genotype_class <- "type"


imtp1_yls[imtp1_yls$genotypes %in% imtp1_hybrids, ]$genotype_class <- "hybrid"
imtp1_yls[!imtp1_yls$genotypes %in% imtp1_hybrids, ]$genotype_class <- "reference"
imtp1_yls <- bind_cols(imtp1_yls, imtp1_rain_lm_ind, imtp1_temp_ind)

post(imtp1_rp)

library(glmertree)

imtp1_yls <- as.data.frame(imtp1_yls)
str(imtp1_yls)

imtp1_yls$genotypes <- as.factor(imtp1_yls$genotypes)
str(imtp1_yls)


glmt <- lmer(formula = youngest_leaf_spotted ~ genotypes |trial| maxDT,
                data = imtp1_yls)

str(DepressionDemo)
fit <- rpart(Mileage~Price + Country + Reliability + Type,    method="anova", data=cu.summary)
rpart.plot(fit)
printcp(fit)

plot(glmt)


imtp1_rfor <- randomForest(youngest_leaf_spotted ~  genotypes + MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day  +
                             R95p  + R99p  + Rtotal +  SDII + 
                             maxDT + minDT + maxNT + minNT +  DTR  +  SU  +  TR +  CFD +  WSDI + CSDI + T10p + T90p,
                       proximity = T,
                       importance = T,
                     data = imtp1_yls)

imtp1_rfor

plot(imtp1_rfor)

library(glmertree)

imtp1_lt <- lmertree(youngest_leaf_spotted ~ genotypes | trial_id | MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day  +
                       R95p  + R99p  + Rtotal +  SDII, cluster = trial_id,
                     data = imtp1_yls)


plot(imtp1_lt)

cart


imtp1_yls$trial <- as.factor(imtp1_yls$trial)
imtp1_yls$location_name <- as.factor(imtp1_yls$location_name)

lmm1_imtp1 <- lmer(youngest_leaf_spotted ~ 1 + genotypes + (1|trial_id), data = imtp1_yls)

lmm1_imtp1

summary(lmm1_imtp1)
# validation of the model
plot(fitted(lmm1_imtp1), residuals(lmm1_imtp1), abline(h = 0))
hist(residuals(lmm1_imtp1))
print(lmm1_imtp1, correlation=TRUE)
lmm1_imtp1$residus <- residuals(lmm1_imtp1)

grubbs.test(residuals(lmm1_imtp1))

DFmoy[which.max(DFmoy$residus),]
# ANOVA table
Anova(lmm1_imtp1, test.statistic = "F")

# Adjusted means
yls1_var <- lsmeans(lmm1_imtp1, ~ genotypes)
plot(yls1_var)
# Pairwise comparisons
pairs(yls1_var, adjust = "tukey")

#cld is deprecated - see help
#cld(yls1_var, Letters = c(LETTERS))

pwpp(yls1_var)


# Comparisons with the general mean
contrast(yls1_var, method = "eff", adjust = "sidak")

confint(contrast(yls1_var, method="eff", adjust="sidak"))

# Comparison with a control
contrast(yls1_var, method="trt.vs.ctrl", ref=2)
confint(contrast(yls1_var, method="trt.vs.ctrl", ref=2))
# Random effects
ranef(lmm1_imtp1)
# Variance of the means and variance of the interaction
Vmoy <- 0.5*mean(VMCj) # VMCj = VMC vector per experiment
Vmoy
var_intera <- summary(res.lmer)$sigma^2 – Vmoy
var_intera

hist(imtp1_yls$youngest_leaf_spotted)
boxplot(imtp1_yls$youngest_leaf_spotted ~ imtp1_yls$genotypes, las = 2) 

lmm2_imtp1 <- lmer(youngest_leaf_spotted ~ 1 + genotypes + (1|trial_r) +
                     MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day  + R95p  + R99p  + Rtotal +  SDII +
                     maxDT + minDT + maxNT + minNT +  DTR  +  SU  +  TR +  CFD +  WSDI + CSDI + T10p + T90p,
                   data = yls_data_imtp1_all)

lmm2_imtp1
summary(lmm2_imtp1)
# validation of the model
plot(fitted(lmm2_imtp1), residuals(lmm2_imtp1), abline(h = 0))
hist(residuals(lmm2_imtp1))
print(lmm2_imtp1, correlation=TRUE)
lmm2_imtp1.residus <- residuals(lmm2_imtp1)

grubbs.test(lmm2_imtp1.residus)

lmm2_imtp1.residus[which.max(lmm2_imtp1.residus),]
# ANOVA table
Anova(lmm2_imtp1, test.statistic = "F")

# Adjusted means
yls2_var <- lsmeans(lmm1_imtp2, ~ genotypes)
plot(yls2_var)
# Pairwise comparisons
pairs(yls2_var, adjust = "tukey")

#cld is deprecated - see help
#cld(yls1_var, Letters = c(LETTERS))

pwpp(yls2_var)


# Comparisons with the general mean
contrast(yls2_var, method = "eff", adjust = "sidak")

confint(contrast(yls2_var, method="eff", adjust="sidak"))

# Comparison with a control
contrast(moy_var, method="trt.vs.ctrl", ref=2)
confint(contrast(moy_var, method="trt.vs.ctrl", ref=2))
# Random effects
ranef(res.lmer)
# Variance of the means and variance of the interaction
Vmoy <- 0.5*mean(VMCj) # VMCj = VMC vector per experiment
Vmoy
var_intera <- summary(res.lmer)$sigma^2 – Vmoy
var_intera

#IMTP-2

imtp2_yls <- yls_data_imtp2_all

lmm1_imtp2 <- lmer(youngest_leaf_spotted ~ 1 + genotypes + (1|trial) + (1|genotypes:trial), data = imtp2_yls)

lmm1_imtp2

summary(lmm1_imtp2)


#IMTP 1 and 2
imtp12_yls <- bind_rows(imtp1_yls, imtp2_yls)

imtp12_yls <- correct_names(imtp12_yls)

lmm1_imtp12 <- lmer(youngest_leaf_spotted ~ genotypes + (1|trial) + (1|genotypes:trial), data = imtp12_yls)

lmm1_imtp12

summary(lmm1_imtp12)

plot(fitted(lmm1_imtp12), residuals(lmm1_imtp12), abline(h = 0))
hist(residuals(lmm1_imtp12))
print(lmm1_imtp12, correlation = TRUE)
lmm1_imtp12.residus <- residuals(lmm1_imtp12)

grubbs.test(lmm1_imtp12.residus)

lmm2_imtp1.residus[which.max(lmm2_imtp1.residus),]
# ANOVA table
Anova(lmm1_imtp12, test.statistic = "F")

# Adjusted means
yls_var_imtp12 <- lsmeans(lmm1_imtp12, ~ genotypes)
plot(yls_var_imtp12)
# Pairwise comparisons
pairs(yls_var_imtp12, adjust = "tukey")

#cld is deprecated - see help
#cld(yls1_var, Letters = c(LETTERS))

pwpp(yls_var_imtp12)


# Comparisons with the general mean
contrast(yls_var_imtp12, method = "eff", adjust = "sidak")

confint(contrast(yls_var_imtp12, method = "eff", adjust = "sidak"))

# Comparison with a control
contrast(yls_var_imtp12, method = "trt.vs.ctrl", ref = 2)
confint(contrast(yls_var_imtp12, method = "trt.vs.ctrl", ref = 2))

# Random effects
ranef(lmm1_imtp12)
# Variance of the means and variance of the interaction
Vmoy <- 0.5*mean(VMCj) # VMCj = VMC vector per experiment
Vmoy
var_intera <- summary(lmm1_imtp12)$sigma^2 – Vmoy
var_intera

#--IMTP 2 with covariates--#

#get covariates for IMTP-2

imtp2_rain_lm <- chirps_extract_dataset(.trial_dataset = imtp2_yls)

imtp2_rain_lm_ind <- get_ct_indices(imtp2_rain_lm)

imtp2_rain_lm_ind <- bind_rows(imtp2_rain_lm_ind)

imtp2_rain_lm_ind


imtp2_min_temp <- extract_dataset(.trial_dataset = imtp2_yls, .temp_ss = "Tmin")

imtp2_max_temp <- extract_dataset(.trial_dataset = imtp2_yls, .temp_ss = "Tmax")

imtp2_min_temp_md <- lapply(imtp2_min_temp, median)

imtp2_max_temp_md <- lapply(imtp2_max_temp, median)

imtp2_temp_ind <- NULL
for(i in 1:length(imtp2_max_temp_md)){
  imtp2_temp_ind[[i]] <- temperature(imtp2_max_temp_md[[i]], imtp2_min_temp_md[[i]])
}

imtp2_temp_ind
imtp2_temp_ind <- bind_rows(imtp2_temp_ind)

imtp2_yls <- cbind(imtp2_yls, imtp2_rain_lm_ind, imtp2_temp_ind)

lmm2_imtp2 <- lmer(youngest_leaf_spotted ~  genotypes + (1|trial_id) +
                     MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day  + R95p  + R99p  + Rtotal +  SDII +
                     maxDT + minDT + maxNT + minNT +  DTR  +  SU  +  TR +  CFD +  WSDI + CSDI + T10p + T90p,
                   data = imtp2_yls)

imtp2_yls <- correct_names(imtp2_yls)

imtp2_rpart <- rpart(youngest_leaf_spotted ~  genotypes +
                       MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day  + R95p  + R99p  + Rtotal +  SDII +
                       maxDT + minDT + maxNT + minNT +  DTR  +  SU  +  TR +  CFD +  WSDI + CSDI + T10p + T90p,
                     data = imtp2_yls)


rpart.plot(imtp2_rpart)

imtp2_yls$pdate
imtp2_yls$hdate
imtp2_yls$latitude
imtp2_yls$longitude

#--IMTP 1 and 2 with covariates--#








#-------------------------------------#


unique(yls_data_imtp3_all$trial_id)

imtp3_yls_block_m <- lmer(youngest_leaf_spotted ~ 1 + cultivar + (1|trial_id) + (1|block), data = yls_data_imtp3_all)

imtp3_yls_block_m


imtp3

summary(imtp3_yls_block_m)

10.6622-(-2.9929)

yls_var_imtp3_b <- lsmeans(imtp3_yls_block_m, ~ cultivar)
yls_var_imtp3_b

imtp3_locations <- read_csv("data/trials/processed/imtp_blsd_trial_block_locations.csv")

imtp3_locations <- imtp3_locations[imtp3_locations$phase == 3, ]

view(imtp3_locations)
group_by(yls_data_imtp3_all, trial_id, block)

unique(imtp3_locations$trial)
unique(imtp3_locations$trial_ID2)

unique(yls_data_imtp3_all$trial_id)

merge(imtp3_locations, yls_data_imtp3_all, by = intersect(imtp3_locations$trial_ID2, yls_data_imtp3_all$trial_id), all.x = T)

colnames(yls_data_imtp3_all)

yls_data_imtp3_all$trial_id <- as.factor(yls_data_imtp3_all$trial_id)
yls_data_imtp3_all$block <- as.factor(yls_data_imtp3_all$block)
yls_data_imtp3_all$block <- as.factor(yls_data_imtp3_all$plant_cycle)

view(locations_imtp[locations_imtp$phase == 3, ])

lmm1_imtp <- lmer(youngest_leaf_spotted ~ 1 + cultivar + (1|trial_id) + (1|trial_id:cultivar) + (1|trial_id:block) + 
                    (1|trial_id:plant_cycle) + (1|cultivar:trial_id) + (1|cultivar:block) + (1|cultivar:plant_cycle),
                  data = yls_data_imtp3_all) 

summary(lmm1_imtp)

print(lmm1_imtp, correlation = T)

emmeans(lmm1_imtp, ~ cultivar)

plot(emmeans(lmm1_imtp, ~ cultivar))

lmm2_imtp <- lmm1_imtp <- lmer(youngest_leaf_spotted ~ 1 + (1|trial_id) + (1|trial_id:cultivar) + (1|trial_id:block), data = yls_data_imtp3_all) 

summary(lmm2_imtp)



lm1_imtp <-lm(youngest_leaf_spotted ~ 1, data = yls_data_imtp3_all)

mean(yls_data_imtp3_all$youngest_leaf_spotted)

summary(lm1_imtp)


 <- data.frame()

wheatyield
wheat_var
organic
wheat_data <- data.frame("E1", c(72.3, 68.5, 77.5, 77.6, 78.6, 74.6, 72.4, 80.7, 79.2, 71.9),
                         "E2", c(89.7, 87.4, 89.7, 93.7, 86.3, 90.9, 95.7, 94.0, 85.8, 86.6),
                         "E3", c(72.2, 66.4, 72.9, 73.4, 76.2, 70.7, 72.8, 70.4, 69.7, 66.9),
                         "E4", c(71.8, 67.5, 77.4, 77.7, 78.7, 74.3, 72.1, 80.7, 80.4, 71.2),
                         "E5", c(88.9, 87.1, 88.9, 93.7, 87.1, 91.0, 95.7, 95.2, 84.7, 86.3))

dim(wheat_data) <- c(11, 5)

wheat_data

colnames(wheat_data) <- c("experiment", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")

wheat_data


#Reading of the external file including the data
TAB <- wheatyield
#Loading of the nlme library
library(nlme)
#Adjustment of the model with the function lme
Mod <- lme(Rdt ~ 1, random = ~ 1 | Site, data = TAB)
#Display of the results
summary(Mod)








