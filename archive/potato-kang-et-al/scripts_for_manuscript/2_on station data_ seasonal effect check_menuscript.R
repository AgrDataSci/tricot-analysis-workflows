#recall package 
library("ggplot2")
library("Matrix")
library("lme4")
library("ggpubr")
library("readxl")
library("tibble")
library("dplyr")

#recall csv data
df_on_station<-read.csv("data/Fixed version On station data_ver2.csv", stringsAsFactors = F)

 
str(df_on_station)
head(df_on_station)
tail(df_on_station)
table(df_on_station$site)
table(df_on_station$clone)
factor(df_on_station)



hist(df_on_station$flowering) 
hist(df_on_station$plant_uniformity)	
hist(df_on_station$plant_vigor)	
hist(df_on_station$lb_incidence)	
hist(df_on_station$lb_severity)	
hist(df_on_station$lmf_incidence)	
hist(df_on_station$lmf_severity)	
hist(df_on_station$viruses_incidence)	
hist(df_on_station$viruses_severity)	
hist(df_on_station$bw_incidence)	
hist(df_on_station$bw_severity)	
hist(df_on_station$average_nber_of_tubers_per_plant)	
hist(df_on_station$number_marketable_yield_ton_ha)	
hist(df_on_station$marketable_yield_ton_ha)	
hist(df_on_station$totat_wight_of_tubers_ton_ha)






#Preleminary test to check the test assumptions: do this on model results
# after run mode. do! check normal distribution residual and factors.
ggscatter(df_on_station, x = "totat_wight_of_tubers_ton_ha", y = "marketable_yield_ton_ha", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ton/ha", ylab = "market ton/ha")


cor.test(df_on_station$totat_wight_of_tubers_ton_ha, df_on_station$marketable_yield_ton_ha,  method = "pearson")

#Visual inspection of the data normality using Q-Q plots (quantile-quantile plots). Q-Q plot draws the correlation between a given sample and the normal distribution.
ggqqplot(df_on_station$number_marketable_yield_ton_ha,   ylab = "total_yield_ton_ha")	
ggqqplot(df_on_station$marketable_yield_ton_ha,  ylab = "total_yield_ton_ha")	
ggqqplot(df_on_station$totat_wight_of_tubers_ton_ha, ylab = "total_yield_ton_ha")

#Use Shapiro-Wilk normality test > R function: shapiro.test() Null hypothesis: the data are normally distributed; Alternative hypothesis: the data are not normally distributed 
shapiro.test(df_on_station$marketable_yield_ton_ha)	#p-value < 2.2e-16 It shows marketfacble_yield _ton_ha does not show normal distribution
shapiro.test(df_on_station$totat_wight_of_tubers_ton_ha) #p-value = 6.013e-16  It shows marketfacble_yield _ton_ha does not show normal distribution

########################MODEL ANALYSIS####################
# need to set it as factor!

site <-factor(df_on_station$site)
no <-factor(df_on_station$no)
season	<-factor(df_on_station$season)
rep	<-factor(df_on_station$rep)
clone<-factor(df_on_station$clone)
###################################devide data to check season effect
keep <- df_on_station[]
which(keep$season == 2) #change this part to check seasons
df_on_station1 <-keep[c(which(keep$season == 1)),] #extract season 1 Season B 
df_on_station2 <-keep[c(which(keep$season == 2)),] #extract season 2 Season A

###################################################################
###ONLY clone RANDOM and OTHER FIXED EFFECTS MODEL
## run model with random effects 
#flowering trait ## run model with random effects 

#thisblow, model is old 
#flowering_all_lmer<-lmer(df_on_station$flowering ~ site + season + (1|clone)+(season/site/rep), data = df_on_station)

#From lme4 documentation you can learn that
#coef: Computes the sum of the random and fixed effects coefficients for each explanatory variable for each level of each grouping factor
#ranef is A generic function to extract the conditional modes of the random effects from a fitted model object  .
# For linear mixed models the conditional modes of the random effects are also the conditional means.
#ranef(flowering_all_lmer) #BLUPs about flowering 
#str(ranef(flowering_all_lmer))
#ranef(flowering_all_lmer)$clone
#VarCorr(flowering_all_lmer, comp = "vcov")
#fixef(flowering_all_lmer)

#vcov(flowering_all_lmer)
#coef(flowering_all_lmer)$clone[1]
#anova(flowering_all_lmer)
#fixef(flowering_all_lmer)[[1]]

#previous one 
#flowering is gategorical value. 
#flowering_all_lmer<-lmer(df_on_station1$flowering ~ site + (1|clone)+(site/rep), data = df_on_station1)

#make GLMM model
flowering_all_glmer<-glmer(df_on_station1$flowering ~ site + (1|clone)+(site/rep), data = df_on_station1, family = poisson )


#below model for checking correlationship 
flowering_all_glmer1<-glmer(df_on_station1$flowering ~ site + (1|clone)+(site/rep), data = df_on_station1,family = poisson)
flowering_all_glmer2<-glmer(df_on_station2$flowering ~ site + (1|clone)+(site/rep), data = df_on_station2,family = poisson)

summary(flowering_all_glmer)

flower_GLMM_1<- as.data.frame(coef(flowering_all_glmer1)$clone[[1]])
flower_GLMM_2<- as.data.frame(coef(flowering_all_glmer2)$clone[[1]])
flower_GLMM_1 <- flower_GLMM_1[c(3, 5 ,6,9,10,13,14),]

t1<- cor.test(flower_GLMM_1[,"condval"],flower_GLMM_2[,"condval"])
t1
#make data frame
flowering_GLMM <- ranef(flowering_all_glmer)
flowering_GLMM <- as.data.frame(flowering_GLMM)
names(flowering_GLMM) <- c("grpvar" ,"term", "clone"  ,"flowering_condval","flowering_condsd")
#condval:value of the conditional mean
#condsd: conditional standard deviation
flowering_GLMM <- add_column(flowering_GLMM,"flowering_fixed_effects_estimates"= fixef(flowering_all_glmer)[[1]], .after = "clone")
flowering_GLMM <- add_column(flowering_GLMM,"sum_random_and_fixed_effects_coefficients"= coef(flowering_all_glmer)$clone[[1]], .after = "flowering_condval")
flowering_GLMM <- flowering_GLMM[,-7] 
flowering_GLMM
ifelse(flowering_GLMM[,4] + flowering_GLMM[,5] ==  flowering_GLMM[,6], T, F)

#plant_uniformity trait 
#old model
#plant_uniformity_glmer<-lmer(df_on_station$plant_uniformity ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)


plant_uniformity_glmer<-glmer(df_on_station1$plant_uniformity ~  (1|clone), data = df_on_station1, family = poisson )
summary(plant_uniformity_glmer)
ranef(plant_uniformity_glmer) #BLUPs about plant_uniformity 
vcov(plant_uniformity_glmer)
plant_uniformity_GLMM <- ranef(plant_uniformity_glmer)
coef(plant_uniformity_glmer)$clone[[1]]
fixef(plant_uniformity_glmer)
#make data frame
plant_uniformity_GLMM <- as.data.frame(plant_uniformity_GLMM)
plant_uniformity_GLMM <- plant_uniformity_GLMM[,-c(1:3)]
names(plant_uniformity_GLMM) <- c("plant_uniformity_condval","plant_uniformity_condsd")
plant_uniformity_GLMM
plant_uniformity_GLMM <- add_column(plant_uniformity_GLMM,"plant_uniformity_fixed_effects_estimates"= fixef(plant_uniformity_glmer)[[1]], .before = "plant_uniformity_condval")
plant_uniformity_GLMM <- add_column(plant_uniformity_GLMM,"sum_random_and_fixed_effects_coefficients"= coef(plant_uniformity_glmer)$clone[[1]], .after = "plant_uniformity_condval")
plant_uniformity_GLMM <- plant_uniformity_GLMM[,-4]

plant_uniformity_GLMM
##
plant_uniformity_glmer1<-glmer(df_on_station1$plant_uniformity ~  (1|clone), data = df_on_station1, family = poisson )
plant_uniformity_glmer2<-glmer(df_on_station2$plant_uniformity ~ (1|clone), data = df_on_station2, family = poisson )

plant_uniformity_GLMM1<- as.data.frame(coef(plant_uniformity_glmer1)$clone[[1]])
plant_uniformity_GLMM2<- as.data.frame(coef(plant_uniformity_glmer2)$clone[[1]])
plant_uniformity_GLMM1 <- plant_uniformity_GLMM1[c(3, 5 ,6,9,10,13,14),]

t2<- cor.test(plant_uniformity_GLMM1[,"condval"],plant_uniformity_GLMM2[,"condval"])
t2

##

#plant_vigor trait
#old model
#plant_vigor_glmer<-lmer(df_on_station$plant_vigor ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)
plant_vigor_glmer<-glmer(df_on_station1$plant_vigor ~ site + (1|clone)+(site/rep), data = df_on_station1,  family = poisson)
summary(plant_vigor_glmer)
ranef(plant_vigor_glmer) #BLUPs about plant_vigor 
vcov(plant_vigor_glmer)
plant_vigor_GLMM <- ranef(plant_vigor_glmer)
coef(plant_vigor_glmer)
#make data frame
plant_vigor_GLMM <- as.data.frame(plant_vigor_GLMM)
plant_vigor_GLMM <- plant_vigor_GLMM[,-c(1:3)]
names(plant_vigor_GLMM) <- c("plant_vigor_condval","plant_vigor_condsd")
plant_vigor_GLMM 
plant_vigor_GLMM <- add_column(plant_vigor_GLMM,"plant_vigor_fixed_effects_estimates"= fixef(plant_vigor_glmer)[[1]], .before = "plant_vigor_condval")
plant_vigor_GLMM <- add_column(plant_vigor_GLMM,"sum_random_and_fixed_effects_coefficients"= coef(plant_vigor_glmer)$clone[[1]], .after = "plant_vigor_condval")
plant_vigor_GLMM <- plant_vigor_GLMM[,-4]
plant_vigor_GLMM
ifelse(plant_vigor_GLMM[,1] + plant_vigor_GLMM[,2] ==  plant_vigor_GLMM[,3], T, F)

##
plant_vigor_glmer1<-glmer(df_on_station1$plant_vigor ~ site + (1|clone)+(site/rep), data = df_on_station1, family = poisson)
plant_vigor_glmer2<-glmer(df_on_station2$plant_vigor ~ site + (1|clone)+(site/rep), data = df_on_station2, family = poisson)

plant_vigor_GLMM1<- as.data.frame(coef(plant_vigor_glmer1)$clone[[1]])
plant_vigor_GLMM2<- as.data.frame(coef(plant_vigor_glmer2)$clone[[1]])
plant_vigor_GLMM1 <- plant_vigor_GLMM1[c(3, 5 ,6,9,10,13,14),]

t3<- cor.test(plant_vigor_GLMM1[,"condval"],plant_vigor_GLMM2[,"condval"])
t3



#lb_incidence trait
#lb_incidence_lmer<-lmer(df_on_station$lb_incidence ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)

lb_incidence_lmer<-lmer(df_on_station1$lb_incidence ~ site +  +  (1|clone)+(site/rep), data = df_on_station1)

summary(lb_incidence_lmer)
ranef(lb_incidence_lmer) #BLUPs about lb_incidence 
vcov(lb_incidence_lmer)

lb_incidence_blup <- ranef(lb_incidence_lmer)

#make data frame
lb_incidence_blup <- as.data.frame(lb_incidence_blup)
lb_incidence_blup <- lb_incidence_blup[,-c(1:3)]
names(lb_incidence_blup) <- c("lb_incidence_condval","lb_incidence_condsd")
lb_incidence_blup 
lb_incidence_blup <- add_column(lb_incidence_blup,"lb_incidence_fixed_effects_estimates"= fixef(lb_incidence_lmer)[[1]], .before = "lb_incidence_condval")
lb_incidence_blup <- add_column(lb_incidence_blup,"sum_random_and_fixed_effects_coefficients"= coef(lb_incidence_lmer)$clone[[1]], .after = "lb_incidence_condval")
lb_incidence_blup <- lb_incidence_blup[,-4]
lb_incidence_blup
ifelse(lb_incidence_blup[,1] + lb_incidence_blup[,2] ==  lb_incidence_blup[,3], T, F)



#lb_severity trait
#old model
#lb_severity_lmer<-lmer(df_on_station$lb_severity ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)
lb_severity_lmer<-lmer(df_on_station1$lb_severity ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station1)


summary(lb_severity_lmer)
ranef(lb_severity_lmer) #BLUPs about lb_severity 
vcov(lb_severity_lmer)
lb_severity_blup <- ranef(lb_severity_lmer)

#make data frame
lb_severity_blup <- as.data.frame(lb_severity_blup)
lb_severity_blup <- lb_severity_blup[,-c(1:3)]
names(lb_severity_blup) <- c("lb_severity_condval","lb_severity_condsd")
lb_severity_blup 
lb_severity_blup <- add_column(lb_severity_blup,"lb_severity_fixed_effects_estimates"= fixef(lb_severity_lmer)[[1]], .before = "lb_severity_condval")
lb_severity_blup <- add_column(lb_severity_blup,"sum_random_and_fixed_effects_coefficients"= coef(lb_severity_lmer)$clone[[1]], .after = "lb_severity_condval")
lb_severity_blup <- lb_severity_blup[,-4]
lb_severity_blup
ifelse(lb_severity_blup[,1] + lb_severity_blup[,2] ==  lb_severity_blup[,3], T, F)



#lmf_incidence trait
#lmf_incidence_lmer<-lmer(df_on_station$lmf_incidence ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)
lmf_incidence_lmer<-lmer(df_on_station1$lmf_incidence ~ site +   (1|clone)+(site/rep), data = df_on_station1)


summary(lmf_incidence_lmer)
ranef(lmf_incidence_lmer) #BLUPs about lmf_incidence 
vcov(lmf_incidence_lmer)
lmf_incidence_blup <- ranef(lmf_incidence_lmer)

#make data frame
lmf_incidence_blup <- as.data.frame(lmf_incidence_blup)
lmf_incidence_blup <- lmf_incidence_blup[,-c(1:3)]
names(lmf_incidence_blup) <- c("lmf_incidence_condval","lmf_incidence_condsd")
lmf_incidence_blup 
lmf_incidence_blup <- add_column(lmf_incidence_blup,"lmf_incidence_fixed_effects_estimates"= fixef(lmf_incidence_lmer)[[1]], .before = "lmf_incidence_condval")
lmf_incidence_blup <- add_column(lmf_incidence_blup,"sum_random_and_fixed_effects_coefficients"= coef(lmf_incidence_lmer)$clone[[1]], .after = "lmf_incidence_condval")
lmf_incidence_blup <- lmf_incidence_blup[,-4]
lmf_incidence_blup
ifelse(lmf_incidence_blup[,1] + lmf_incidence_blup[,2] ==  lmf_incidence_blup[,3], T, F)



#lmf_severity trait
#lmf_severity_lmer<-lmer(df_on_station$lmf_severity  ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)
lmf_severity_lmer<-lmer(df_on_station1$lmf_severity  ~ site  +  (1|clone)+(site/rep), data = df_on_station1)

summary(lmf_severity_lmer)
ranef(lmf_severity_lmer) #BLUPs about lmf_severity  
vcov(lmf_severity_lmer)

lmf_severity_blup <- ranef(lmf_severity_lmer)
#make data frame
lmf_severity_blup <- as.data.frame(lmf_severity_blup)
lmf_severity_blup <- lmf_severity_blup[,-c(1:3)]
names(lmf_severity_blup) <- c("lmf_severity_condval","lmf_severity_condsd")
lmf_severity_blup 

lmf_severity_blup <- add_column(lmf_severity_blup,"lmf_severity_fixed_effects_estimates"= fixef(lmf_severity_lmer)[[1]], .before = "lmf_severity_condval")
lmf_severity_blup <- add_column(lmf_severity_blup,"sum_random_and_fixed_effects_coefficients"= coef(lmf_severity_lmer)$clone[[1]], .after = "lmf_severity_condval")
lmf_severity_blup <- lmf_severity_blup[,-4]
lmf_severity_blup
ifelse(lmf_severity_blup[,1] + lmf_severity_blup[,2] ==  lmf_severity_blup[,3], T, F)




#viruses_incidence trait
#viruses_incidence_lmer<-lmer(df_on_station$viruses_incidence ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)

viruses_incidence_lmer<-lmer(df_on_station1$viruses_incidence ~ site +   (1|clone)+(site/rep), data = df_on_station1)

summary(viruses_incidence_lmer)
ranef(viruses_incidence_lmer) #BLUPs about viruses_incidence 
vcov(viruses_incidence_lmer)

viruses_incidence_blup <- ranef(viruses_incidence_lmer)


#make data frame
viruses_incidence_blup <- as.data.frame(viruses_incidence_blup)
viruses_incidence_blup <- viruses_incidence_blup[,-c(1:3)]
names(viruses_incidence_blup) <- c("viruses_incidence_condval","viruses_incidence_condsd")
viruses_incidence_blup 
viruses_incidence_blup <- add_column(viruses_incidence_blup,"viruses_incidence_fixed_effects_estimates"= fixef(viruses_incidence_lmer)[[1]], .before = "viruses_incidence_condval")
viruses_incidence_blup <- add_column(viruses_incidence_blup,"sum_random_and_fixed_effects_coefficients"= coef(viruses_incidence_lmer)$clone[[1]], .after = "viruses_incidence_condval")
viruses_incidence_blup <- viruses_incidence_blup[,-4]
viruses_incidence_blup
ifelse(viruses_incidence_blup[,1] + viruses_incidence_blup[,2] ==  viruses_incidence_blup[,3], T, F)



#viruses_severity trait
#viruses_severity_lmer<-lmer(df_on_station$viruses_severity ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)
viruses_severity_lmer<-lmer(df_on_station1$viruses_severity ~ site +   (1|clone)+(site/rep), data = df_on_station1)

summary(viruses_severity_lmer)
ranef(viruses_severity_lmer) #BLUPs about viruses_severity 
vcov(viruses_severity_lmer)

viruses_severity_blup <- ranef(viruses_severity_lmer)


#make data frame
viruses_severity_blup <- as.data.frame(viruses_severity_blup)
viruses_severity_blup <- viruses_severity_blup[,-c(1:3)]
names(viruses_severity_blup) <- c("viruses_severity_condval","viruses_severity_condsd")
viruses_severity_blup 

viruses_severity_blup 
viruses_severity_blup <- add_column(viruses_severity_blup,"viruses_severity_fixed_effects_estimates"= fixef(viruses_severity_lmer)[[1]], .before = "viruses_severity_condval")
viruses_severity_blup <- add_column(viruses_severity_blup,"sum_random_and_fixed_effects_coefficients"= coef(viruses_severity_lmer)$clone[[1]], .after = "viruses_severity_condval")
viruses_severity_blup <- viruses_severity_blup[,-4]
viruses_severity_blup
ifelse(viruses_severity_blup[,1] + viruses_severity_blup[,2] ==  viruses_severity_blup[,3], T, F)




#bw_incidence trait
#bw_incidence_lmer<-lmer(df_on_station$bw_incidence ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)

bw_incidence_lmer<-lmer(df_on_station1$bw_incidence ~ site +   (1|clone)+(site/rep), data = df_on_station1)

summary(bw_incidence_lmer)
ranef(bw_incidence_lmer) #BLUPs about bw_incidence 
vcov(bw_incidence_lmer)
anova(bw_incidence_lmer)

bw_incidence_blup <- ranef(bw_incidence_lmer)
#make data frame
bw_incidence_blup <- as.data.frame(bw_incidence_blup)
bw_incidence_blup <- bw_incidence_blup[,-c(1:3)]
names(bw_incidence_blup) <- c("bw_incidence_condval","bw_incidence_condsd")
bw_incidence_blup 
bw_incidence_blup <- add_column(bw_incidence_blup,"bw_incidence_fixed_effects_estimates"= fixef(bw_incidence_lmer)[[1]], .before = "bw_incidence_condval")
bw_incidence_blup <- add_column(bw_incidence_blup,"sum_random_and_fixed_effects_coefficients"= coef(bw_incidence_lmer)$clone[[1]], .after = "bw_incidence_condval")
bw_incidence_blup <- bw_incidence_blup[,-4]
bw_incidence_blup
ifelse(bw_incidence_blup[,1] + bw_incidence_blup[,2] ==  bw_incidence_blup[,3], T, F)




#bw_severity trait
#bw_severity_lmer <- lmer(df_on_station$bw_severity ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)

bw_severity_lmer <- lmer(df_on_station1$bw_severity ~ site + (1|clone)+(site/rep), data = df_on_station1)

summary(bw_severity_lmer)
ranef(bw_severity_lmer) #BLUPs about bw_severity  
vcov(bw_severity_lmer)

bw_severity_blup <- ranef(bw_severity_lmer)
#make data frame
bw_severity_blup <- as.data.frame(bw_severity_blup)
bw_severity_blup <- bw_severity_blup[,-c(1:3)]
names(bw_severity_blup) <- c("bw_severity_condval","bw_severity_condsd")
bw_severity_blup 

bw_severity_blup <- add_column(bw_severity_blup,"bw_severity_fixed_effects_estimates"= fixef(bw_severity_lmer)[[1]], .before = "bw_severity_condval")
bw_severity_blup <- add_column(bw_severity_blup,"sum_random_and_fixed_effects_coefficients"= coef(bw_severity_lmer)$clone[[1]], .after = "bw_severity_condval")
bw_severity_blup <- bw_severity_blup[,-4]
bw_severity_blup
ifelse(bw_severity_blup[,1] + bw_severity_blup[,2] ==  bw_severity_blup[,3], T, F)



#average_nber_of_tubers_per_plant trait
#average_nber_of_tubers_per_plant_lmer<-lmer(df_on_station$average_nber_of_tubers_per_plant ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)
average_nber_of_tubers_per_plant_lmer<-lmer(df_on_station1$average_nber_of_tubers_per_plant ~ site + (1|clone)+(site/rep), data = df_on_station1)

summary(average_nber_of_tubers_per_plant_lmer)
ranef(average_nber_of_tubers_per_plant_lmer) #BLUPs about average_nber_of_tubers_per_plant 
vcov(average_nber_of_tubers_per_plant_lmer)

average_nber_of_tubers_per_plant_blup <- ranef(average_nber_of_tubers_per_plant_lmer)

#make data frame
average_nber_of_tubers_per_plant_blup <- as.data.frame(average_nber_of_tubers_per_plant_blup)
average_nber_of_tubers_per_plant_blup <- average_nber_of_tubers_per_plant_blup[,-c(1:3)]
names(average_nber_of_tubers_per_plant_blup) <- c("average_nber_of_tubers_per_plant_condval","average_nber_of_tubers_per_plant_condsd")
average_nber_of_tubers_per_plant_blup 

average_nber_of_tubers_per_plant_blup <- add_column(average_nber_of_tubers_per_plant_blup,"average_nber_of_tubers_per_plant_fixed_effects_estimates"= fixef(average_nber_of_tubers_per_plant_lmer)[[1]], .before = "average_nber_of_tubers_per_plant_condval")
average_nber_of_tubers_per_plant_blup <- add_column(average_nber_of_tubers_per_plant_blup,"sum_random_and_fixed_effects_coefficients"= coef(average_nber_of_tubers_per_plant_lmer)$clone[[1]], .after = "average_nber_of_tubers_per_plant_condval")
average_nber_of_tubers_per_plant_blup <- average_nber_of_tubers_per_plant_blup[,-4]
average_nber_of_tubers_per_plant_blup
ifelse(average_nber_of_tubers_per_plant_blup[,1] + average_nber_of_tubers_per_plant_blup[,2] ==  average_nber_of_tubers_per_plant_blup[,3], T, F)


##
average_nber_of_tubers_per_plant_lmer1<-lmer(df_on_station1$average_nber_of_tubers_per_plant ~ site + (1|clone)+(site/rep), data = df_on_station1)
average_nber_of_tubers_per_plant_lmer2<-lmer(df_on_station2$average_nber_of_tubers_per_plant ~ site + (1|clone)+(site/rep), data = df_on_station2)

average_nber_of_tubers_per_plant_blup1<- as.data.frame(coef(average_nber_of_tubers_per_plant_lmer1)$clone[[1]])
average_nber_of_tubers_per_plant_blup2<- as.data.frame(coef(average_nber_of_tubers_per_plant_lmer2)$clone[[1]])
average_nber_of_tubers_per_plant_blup1 <- average_nber_of_tubers_per_plant_blup1[c(3, 5 ,6,9,10,13,14),]

t12<- cor.test(average_nber_of_tubers_per_plant_blup1[,"condval"],average_nber_of_tubers_per_plant_blup2[,"condval"])
t12






#number_marketable_yield_ton_ha trait
#number_marketable_yield_ton_ha_lmer<-lmer(df_on_station$number_marketable_yield_ton_ha ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)

number_marketable_yield_ton_ha_lmer<-lmer(df_on_station1$number_marketable_yield_ton_ha ~ site + (1|clone)+(site/rep), data = df_on_station1)

summary(number_marketable_yield_ton_ha_lmer)
ranef(number_marketable_yield_ton_ha_lmer) #BLUPs about number_marketable_yield_ton_ha 
vcov(number_marketable_yield_ton_ha_lmer)

number_marketable_yield_ton_ha_blup <- ranef(number_marketable_yield_ton_ha_lmer)
#make data frame
number_marketable_yield_ton_ha_blup <- as.data.frame(number_marketable_yield_ton_ha_blup)
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup[,-c(1:2)] #here some clones are NA
names(number_marketable_yield_ton_ha_blup) <- c("clone", "number_marketable_yield_ton_ha_condval","number_marketable_yield_ton_ha_condsd")
number_marketable_yield_ton_ha_blup 


number_marketable_yield_ton_ha_blup <- add_column(number_marketable_yield_ton_ha_blup,"number_marketable_fixed_effects_estimates"= fixef(number_marketable_yield_ton_ha_lmer)[[1]], .before = "number_marketable_yield_ton_ha_condval")
number_marketable_yield_ton_ha_blup <- add_column(number_marketable_yield_ton_ha_blup,"sum_random_and_fixed_effects_coefficients"= coef(number_marketable_yield_ton_ha_lmer)$clone[[1]], .after = "number_marketable_yield_ton_ha_condval")
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup[,-5]
number_marketable_yield_ton_ha_blup
ifelse(number_marketable_yield_ton_ha_blup[,2] + number_marketable_yield_ton_ha_blup[,3] ==  number_marketable_yield_ton_ha_blup[,4], T, F)




#marketable_yield_ton_ha trait
#marketable_yield_ton_ha_lmer<-lmer(df_on_station$marketable_yield_ton_ha ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)
marketable_yield_ton_ha_lmer<-lmer(df_on_station1$marketable_yield_ton_ha ~ site +   (1|clone)+(site/rep), data = df_on_station1)

summary(marketable_yield_ton_ha_lmer)
ranef(marketable_yield_ton_ha_lmer) #BLUPs about marketable_yield_ton_ha 
vcov(marketable_yield_ton_ha_lmer)


marketable_yield_ton_ha_blup <- ranef(marketable_yield_ton_ha_lmer)
#make data frame
marketable_yield_ton_ha_blup <- as.data.frame(marketable_yield_ton_ha_blup)
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup[,-c(1:2)] #here some clones are NA
names(marketable_yield_ton_ha_blup) <- c("clone", "marketable_yield_ton_ha_condval","marketable_yield_ton_ha_condsd")
marketable_yield_ton_ha_blup 

marketable_yield_ton_ha_blup <- add_column(marketable_yield_ton_ha_blup,"marketable_yield_ton_ha_fixed_effects_estimates"= fixef(marketable_yield_ton_ha_lmer)[[1]], .before = "marketable_yield_ton_ha_condval")
marketable_yield_ton_ha_blup <- add_column(marketable_yield_ton_ha_blup,"sum_random_and_fixed_effects_coefficients"= coef(marketable_yield_ton_ha_lmer)$clone[[1]], .after = "marketable_yield_ton_ha_condval")
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup[,-5]
marketable_yield_ton_ha_blup
ifelse(marketable_yield_ton_ha_blup[,2] + marketable_yield_ton_ha_blup[,3] ==  marketable_yield_ton_ha_blup[,4], T, F)




#totat_wight_of_tubers_ton_ha trait
#totat_wight_of_tubers_ton_ha_lmer<-lmer(df_on_station$totat_wight_of_tubers_ton_ha ~ site + season +  (1|clone)+(season/site/rep), data = df_on_station)
totat_wight_of_tubers_ton_ha_lmer<-lmer(df_on_station1$totat_wight_of_tubers_ton_ha ~ site + (1|clone)+(site/rep), data = df_on_station1)

summary(totat_wight_of_tubers_ton_ha_lmer)
ranef(totat_wight_of_tubers_ton_ha_lmer) #BLUPs about totat_wight_of_tubers_ton_ha 
vcov(totat_wight_of_tubers_ton_ha_lmer)

#make data frame

totat_wight_of_tubers_ton_ha_blup <- ranef(totat_wight_of_tubers_ton_ha_lmer)

totat_wight_of_tubers_ton_ha_blup <- as.data.frame(totat_wight_of_tubers_ton_ha_blup)
totat_wight_of_tubers_ton_ha_blup <- totat_wight_of_tubers_ton_ha_blup[,-c(1:3)]
names(totat_wight_of_tubers_ton_ha_blup) <- c("totat_wight_of_tubers_ton_ha_condval","totat_wight_of_tubers_ton_ha_condsd")
totat_wight_of_tubers_ton_ha_blup 
totat_wight_of_tubers_ton_ha_blup <- add_column(totat_wight_of_tubers_ton_ha_blup,"totat_wight_of_tubers_ton_ha_fixed_effects_estimates"= fixef(totat_wight_of_tubers_ton_ha_lmer)[[1]], .before = "totat_wight_of_tubers_ton_ha_condval")
totat_wight_of_tubers_ton_ha_blup <- add_column(totat_wight_of_tubers_ton_ha_blup,"sum_random_and_fixed_effects_coefficients"= coef(totat_wight_of_tubers_ton_ha_lmer)$clone[[1]], .after = "totat_wight_of_tubers_ton_ha_condval")
totat_wight_of_tubers_ton_ha_blup <- totat_wight_of_tubers_ton_ha_blup[,-4]
totat_wight_of_tubers_ton_ha_blup
ifelse(totat_wight_of_tubers_ton_ha_blup[,1] + totat_wight_of_tubers_ton_ha_blup[,2] ==  totat_wight_of_tubers_ton_ha_blup[,3], T, F)

##
##
totat_wight_of_tubers_ton_ha_lmer1<-lmer(df_on_station1$totat_wight_of_tubers_ton_ha ~ site + (1|clone)+(site/rep), data = df_on_station1)
totat_wight_of_tubers_ton_ha_lmer2<-lmer(df_on_station2$totat_wight_of_tubers_ton_ha ~ site + (1|clone)+(site/rep), data = df_on_station2)

totat_wight_of_tubers_ton_ha_blup1<- as.data.frame(coef(totat_wight_of_tubers_ton_ha_lmer1)$clone[[1]])
totat_wight_of_tubers_ton_ha_blup2<- as.data.frame(coef(totat_wight_of_tubers_ton_ha_lmer2)$clone[[1]])
totat_wight_of_tubers_ton_ha_blup1 <- totat_wight_of_tubers_ton_ha_blup1[c(3, 5 ,6,9,10,13,14),]

t15<- cor.test(totat_wight_of_tubers_ton_ha_blup1[,"condval"],totat_wight_of_tubers_ton_ha_blup2[,"condval"])
t15




#check how different
number_marketable_yield_ton_ha_blup #7
marketable_yield_ton_ha_blup #7




# put missing clones data as NA 
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup %>% add_row(clone ="CIP388676.1", number_marketable_fixed_effects_estimates =NA, number_marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup %>% add_row(clone ="CIP392797.22", number_marketable_fixed_effects_estimates =NA, number_marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup %>% add_row(clone ="CIP393079.4", number_marketable_fixed_effects_estimates =NA, number_marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup %>% add_row(clone ="CIP394611.112", number_marketable_fixed_effects_estimates =NA, number_marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup %>% add_row(clone ="CIP395112.6", number_marketable_fixed_effects_estimates =NA, number_marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup %>% add_row(clone ="CIP398190.89", number_marketable_fixed_effects_estimates =NA, number_marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup %>% add_row(clone ="CIP398193.511", number_marketable_fixed_effects_estimates =NA, number_marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup %>% add_row(clone ="Shangi", number_marketable_fixed_effects_estimates =NA, number_marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)

number_marketable_yield_ton_ha_blup <- number_marketable_yield_ton_ha_blup[c(8 ,  9, 1, 10, 2, 3, 11, 12, 4, 5 , 13 , 14, 6, 7, 15),]

ifelse(number_marketable_yield_ton_ha_blup[,1] == flowering_GLMM[,3], T, F)

# put missing clones data as NA 
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup %>% add_row(clone ="CIP388676.1", marketable_yield_ton_ha_fixed_effects_estimates =NA, marketable_yield_ton_ha_condval =NA , sum_random_and_fixed_effects_coefficients = NA)
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup %>% add_row(clone ="CIP392797.22", marketable_yield_ton_ha_fixed_effects_estimates =NA, marketable_yield_ton_ha_condval =NA , sum_random_and_fixed_effects_coefficients = NA)
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup %>% add_row(clone ="CIP393079.4", marketable_yield_ton_ha_fixed_effects_estimates =NA, marketable_yield_ton_ha_condval =NA , sum_random_and_fixed_effects_coefficients = NA)
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup %>% add_row(clone ="CIP394611.112", marketable_yield_ton_ha_fixed_effects_estimates =NA, marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup %>% add_row(clone ="CIP395112.6", marketable_yield_ton_ha_fixed_effects_estimates =NA, marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup %>% add_row(clone ="CIP398190.89", marketable_yield_ton_ha_fixed_effects_estimates =NA, marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup %>% add_row(clone ="CIP398193.511", marketable_yield_ton_ha_fixed_effects_estimates =NA, marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)
marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup %>% add_row(clone ="Shangi", marketable_yield_ton_ha_fixed_effects_estimates =NA, marketable_yield_ton_ha_condval =NA, sum_random_and_fixed_effects_coefficients = NA)

marketable_yield_ton_ha_blup <- marketable_yield_ton_ha_blup[c(8 ,  9, 1, 10, 2, 3, 11, 12, 4, 5 , 13 , 14, 6, 7, 15),]
ifelse(marketable_yield_ton_ha_blup[,1] == flowering_GLMM[,3], T, F)


#combine blups of traits 
tuber_blup <- cbind(flowering_GLMM, plant_uniformity_GLMM)
tuber_blup <- cbind(tuber_blup, plant_vigor_GLMM)
tuber_blup <- cbind(tuber_blup, lb_incidence_blup)
tuber_blup <- cbind(tuber_blup, lb_severity_blup)
tuber_blup <- cbind(tuber_blup, lmf_incidence_blup)
tuber_blup <- cbind(tuber_blup, lmf_severity_blup)
tuber_blup <- cbind(tuber_blup, viruses_incidence_blup)
tuber_blup <- cbind(tuber_blup, viruses_severity_blup)
tuber_blup <- cbind(tuber_blup, bw_incidence_blup)
tuber_blup <- cbind(tuber_blup, bw_severity_blup)
tuber_blup <- cbind(tuber_blup, average_nber_of_tubers_per_plant_blup)
tuber_blup <- cbind(tuber_blup, number_marketable_yield_ton_ha_blup)
tuber_blup <- cbind(tuber_blup, marketable_yield_ton_ha_blup)
tuber_blup <- cbind(tuber_blup, totat_wight_of_tubers_ton_ha_blup)
tuber_blup <- tuber_blup[,-c(40, 44)]

tuber_blup
#change dataframe columns name
names(tuber_blup) <- c( "grpvar","term" ,                                                   
                        "clone","flowering_Grand_mean_intercept +/- std err"  ,                     
                        "flowering_BLUPS_genotypic_value","flowering_Predicted_mean_(Sum_of_fixef_and_BLUP)",  
                        "plant_uniformity_Grand_mean_intercept +/- std err","plant_uniformity_GLMMS_genotypic_value" 
                        ,"plant_uniformity_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "plant_vigor_Grand_mean_intercept +/- std err"  ,                   
                        "plant_vigor_GLMMS_genotypic_value","plant_vigor_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "lb_incidence_Grand_mean_intercept +/- std err","lb_incidence_BLUPS_genotypic_value"    ,                                
                        "lb_incidence_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "lb_severity_Grand_mean_intercept +/- std err","lb_severity_BLUPS_genotypic_value",
                        "lb_severity_Predicted_mean_(Sum_of_fixef_and_BLUP)"  ,
                        "lmf_incidence_Grand_mean_intercept +/- std err","lmf_incidence_BLUPS_genotypic_value"  ,                                 
                        "lmf_incidence_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "lmf_severity_Grand_mean_intercept +/- std err" ,                   
                        "lmf_severity_BLUPS_genotypic_value","lmf_severity_Predicted_mean_(Sum_of_fixef_and_BLUP)",    
                        "viruses_incidence_Grand_mean_intercept +/- std err","viruses_incidence_BLUPS_genotypic_value"  ,                             
                        "viruses_incidence_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "viruses_severity_Grand_mean_intercept +/- std err",
                        "viruses_severity_BLUPS_genotypic_value","viruses_severity_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "bw_incidencey_Grand_mean_intercept +/- std err","bw_incidence_BLUPS_genotypic_value",
                        "bw_incidencey_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "bw_severity_Grand_mean_intercept +/- std err","bw_severity_BLUPS_genotypic_value",
                        "bw_severity_Predicted_mean_(Sum_of_fixef_and_BLUP)" ,           
                        "average_nber_of_tubers_per_plant_Grand_mean_intercept +/- std err" ,"average_nber_of_tubers_per_plant_BLUPS_genotypic_value" ,               
                        "average_nber_of_tubers_per_plant_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "number_marketable_Grand_mean_intercept +/- std err",
                        "number_marketable_yield_ton_ha_BLUPS_genotypic_value","number_marketable_yield_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP)" ,   
                        "marketable_yield_ton_ha_Grand_mean_intercept +/- std err","marketable_yield_ton_ha_BLUPS_genotypic_value"                         
                        ,"marketable_yield_ton_ha__Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "totat_wight_of_tubers_ton_ha_Grand_mean_intercept +/- std err"    
                        ,"totat_wight_of_tubers_ton_ha_BLUPS_genotypic_value","totat_wight_of_tubers_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP")

#calculate traits real value.

#traits <- df_on_station[,12:26] 

traits <- df_on_station1[,12:26] 
traits_mean<- data.frame(apply(traits,2, mean, na.rm=TRUE))



tuber_blup <- add_column(tuber_blup,"flowering_average_performance"= traits_mean[1,], .after = "flowering_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"plant_uniformity_average_performance"= traits_mean[2,], .after = "plant_uniformity_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"plant_vigor_average_performance"= traits_mean[3,], .after = "plant_vigor_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"lb_incidence_average_performance"= traits_mean[4,], .after = "lb_incidence_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"lb_severity_average_performance"= traits_mean[5,], .after = "lb_severity_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"lmf_incidence_average_performance"= traits_mean[6,], .after = "lmf_incidence_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"lmf_severity_average_performance"= traits_mean[7,], .after = "lmf_severity_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"viruses_incidence_average_performance"= traits_mean[8,], .after = "viruses_incidence_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"viruses_severity_average_performance"= traits_mean[9,], .after = "viruses_severity_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"bw_incidencey_average_performance"= traits_mean[10,], .after = "bw_incidencey_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"bw_severity_average_performance"= traits_mean[11,], .after = "bw_severity_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"average_nber_of_tubers_per_plant_average_performance"= traits_mean[12,], .after = "average_nber_of_tubers_per_plant_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"number_marketable_average_performance"= traits_mean[13,], .after = "number_marketable_yield_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"marketable_yield_ton_ha_average_performance"= traits_mean[14,], .after = "marketable_yield_ton_ha__Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"totat_wight_of_tubers_ton_ha_average_performance"= traits_mean[15,], .after = "totat_wight_of_tubers_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP")
################################## finished calculating BLUP

###### calculate binary value


tem_dt <- df_on_station[]

tem_dt <- add_column(tem_dt, "late_blight" = ifelse(tem_dt$lb_incidence == 0 & 
                                                      tem_dt$lb_severity == 0, 0, 1), .after= "lb_severity")

tem_dt <- add_column(tem_dt, "Leaf_miner_flies" = ifelse(tem_dt$lmf_incidence == 0 & 
                                                           tem_dt$lmf_severity == 0, 0, 1), .after= "lmf_severity")


tem_dt <- add_column(tem_dt, "viruses" = ifelse(tem_dt$viruses_incidence == 0 &  
                                                  tem_dt$viruses_severity == 0, 0, 1), .after= "viruses_severity")


tem_dt <- add_column(tem_dt, "bacterial_wilt" = ifelse(tem_dt$bw_incidence == 0 &  
                                                         tem_dt$bw_severity == 0, 0, 1), .after= "bw_severity")

clone_names <- unique(tem_dt$clone)




rrr <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt, tem_dt$clone== clone_names[[i]])
  rrr[i]<- sum(sucal$late_blight)/count(subset(tem_dt, tem_dt$clone== clone_names[[i]]))}
rrr  

rrr<- as.data.frame(rrr)  
rrr<- t(rrr) # change form 
rownames(rrr) <-c(1:15)
rrr <- cbind(clone_names, rrr)
rrr<- as.data.frame(rrr[])
#



aaa <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt, tem_dt$clone== clone_names[[i]])
  aaa[i]<- sum(sucal$Leaf_miner_flies)/count(subset(tem_dt, tem_dt$clone== clone_names[[i]]))}
aaa  

aaa<- as.data.frame(aaa)  
aaa<- t(aaa) # change form 
rownames(aaa) <-c(1:15)
aaa <- cbind(clone_names, aaa)
aaa<- as.data.frame(aaa[])
#

bbb <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt, tem_dt$clone== clone_names[[i]])
  bbb[i]<- sum(sucal$viruses)/count(subset(tem_dt, tem_dt$clone== clone_names[[i]]))}
bbb  

bbb<- as.data.frame(bbb)  
bbb<- t(bbb) # change form 
rownames(bbb) <-c(1:15)
bbb <- cbind(clone_names, bbb)
bbb<- as.data.frame(bbb[])
bbb




#
fff <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt, tem_dt$clone== clone_names[[i]])
  fff[i]<- sum(sucal$bacterial_wilt)/count(subset(tem_dt, tem_dt$clone== clone_names[[i]]))}
fff  

fff<- as.data.frame(fff)  
fff<- t(fff) # change form 
rownames(fff) <-c(1:15)
fff <- cbind(clone_names, fff)
fff<- as.data.frame(fff[])

rrr<- cbind(rrr, aaa)
rrr<- cbind(rrr, bbb)
rrr<- cbind(rrr, fff)

names(rrr) <- c("clone_name1","late_blight","clone_name2", "Leaf_miner_flies","clone_name3","viruses","clone_name4", "bacterial_wilt")
rrr


##check season effect. 
tem_dt1 <- tem_dt[c(which(tem_dt$season == 1)),]
tem_dt2 <- tem_dt[c(which(tem_dt$season == 2)),]




rrr <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt1, tem_dt1$clone== clone_names[[i]])
  rrr[i]<- sum(sucal$late_blight)/count(subset(tem_dt1, tem_dt1$clone== clone_names[[i]]))}
rrr  

rrr<- as.data.frame(rrr)  
rrr<- t(rrr) # change form 
rownames(rrr) <-c(1:15)
rrr <- cbind(clone_names, rrr)
rrr<- as.data.frame(rrr[])
#



aaa <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt1, tem_dt1$clone== clone_names[[i]])
  aaa[i]<- sum(sucal$Leaf_miner_flies)/count(subset(tem_dt1, tem_dt1$clone== clone_names[[i]]))}
aaa  

aaa<- as.data.frame(aaa)  
aaa<- t(aaa) # change form 
rownames(aaa) <-c(1:15)
aaa <- cbind(clone_names, aaa)
aaa<- as.data.frame(aaa[])
#

bbb <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt1, tem_dt1$clone== clone_names[[i]])
  bbb[i]<- sum(sucal$viruses)/count(subset(tem_dt1, tem_dt1$clone== clone_names[[i]]))}
bbb  

bbb<- as.data.frame(bbb)  
bbb<- t(bbb) # change form 
rownames(bbb) <-c(1:15)
bbb <- cbind(clone_names, bbb)
bbb<- as.data.frame(bbb[])
bbb




#
fff <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt1, tem_dt1$clone== clone_names[[i]])
  fff[i]<- sum(sucal$bacterial_wilt)/count(subset(tem_dt1, tem_dt1$clone== clone_names[[i]]))}
fff  

fff<- as.data.frame(fff)  
fff<- t(fff) # change form 
rownames(fff) <-c(1:15)
fff <- cbind(clone_names, fff)
fff<- as.data.frame(fff[])

rrr<- cbind(rrr, aaa)
rrr<- cbind(rrr, bbb)
rrr<- cbind(rrr, fff)

names(rrr) <- c("clone_name1","late_blight","clone_name2", "Leaf_miner_flies","clone_name3","viruses","clone_name4", "bacterial_wilt")
rrr1<- rrr[]
rrr_A<- rrr[]


#########################################

rrr <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt2, tem_dt2$clone== clone_names[[i]])
  rrr[i]<- sum(sucal$late_blight)/count(subset(tem_dt2, tem_dt2$clone== clone_names[[i]]))}
rrr  

rrr<- as.data.frame(rrr)  
rrr<- t(rrr) # change form 
rownames(rrr) <-c(1:15)
rrr <- cbind(clone_names, rrr)
rrr<- as.data.frame(rrr[])
#



aaa <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt2, tem_dt2$clone== clone_names[[i]])
  aaa[i]<- sum(sucal$Leaf_miner_flies)/count(subset(tem_dt2, tem_dt2$clone== clone_names[[i]]))}
aaa  

aaa<- as.data.frame(aaa)  
aaa<- t(aaa) # change form 
rownames(aaa) <-c(1:15)
aaa <- cbind(clone_names, aaa)
aaa<- as.data.frame(aaa[])
#

bbb <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt2, tem_dt2$clone== clone_names[[i]])
  bbb[i]<- sum(sucal$viruses)/count(subset(tem_dt2, tem_dt2$clone== clone_names[[i]]))}
bbb  

bbb<- as.data.frame(bbb)  
bbb<- t(bbb) # change form 
rownames(bbb) <-c(1:15)
bbb <- cbind(clone_names, bbb)
bbb<- as.data.frame(bbb[])
bbb




#
fff <-list()
for(i in seq_along(clone_names)){
  sucal <- subset(tem_dt2, tem_dt2$clone== clone_names[[i]])
  fff[i]<- sum(sucal$bacterial_wilt)/count(subset(tem_dt2, tem_dt2$clone== clone_names[[i]]))}
fff  

fff<- as.data.frame(fff)  
fff<- t(fff) # change form 
rownames(fff) <-c(1:15)
fff <- cbind(clone_names, fff)
fff<- as.data.frame(fff[])

rrr<- cbind(rrr, aaa)
rrr<- cbind(rrr, bbb)
rrr<- cbind(rrr, fff)

names(rrr) <- c("clone_name1","late_blight","clone_name2", "Leaf_miner_flies","clone_name3","viruses","clone_name4", "bacterial_wilt")
rrr2<- rrr[]

rrr1 <- rrr1[1:7,]
rrr2 <- rrr2[1:7,]
#correlation
rrr1[,2] <- as.numeric(rrr1[,2])
rrr1[,4] <- as.numeric(rrr1[,4])
rrr1[,6] <- as.numeric(rrr1[,6])
rrr1[,8] <- as.numeric(rrr1[,8])


rrr2[,2] <- as.numeric(rrr2[,2])
rrr2[,4] <- as.numeric(rrr2[,4])
rrr2[,6] <- as.numeric(rrr2[,6])
rrr2[,8] <- as.numeric(rrr2[,8])


t4 <- cor.test(rrr1[,2], rrr2[,2])
t5 <- cor.test(rrr1[,4], rrr2[,4])
t6 <- cor.test(rrr1[,6], rrr2[,6])
t7 <- cor.test(rrr1[,8], rrr2[,8])

##
cor_data <- c(t1$estimate, t2$estimate, t3$estimate, t4$estimate, t5$estimate, t6$estimate, t7$estimate, t12$estimate, t15$estimate)
names(cor_data) <- c( "flowering" ,"plant_uniformity" ,"plant_vigor"    ,     "late_blight"  ,   
                      "Leaf_miner_flies" ,"viruses" ,   "bacterial_wilt"   ,     "average_nber_of_tubers_per_plant",
                      "totat_wight_of_tubers_ton_ha" )

cor_data

capture.output(cor_data, 
               print("correlation_between_seasons"),
               cor_data,
               file ="output_breeders/correlation_between_seasons.txt")
######################checking disease trait 
########################finishing calculate binary value
tuber_blup$clone
rrr_A$clone_names 

rrr <- rrr_A[c(11, 8, 1, 15,
               3, 4, 14, 13,
               6, 7, 10, 9,
               5, 2,12),]

ifelse(rrr$clone_names == tuber_blup$clone, T,F)
tuber_blup <- cbind(tuber_blup, rrr)

ifelse(tuber_blup[,3] == tuber_blup[,64], T,F)
ifelse(tuber_blup[,64] == tuber_blup[,66], T,F)
ifelse(tuber_blup[,66] == tuber_blup[,68], T,F)
ifelse(tuber_blup[,68] == tuber_blup[,70], T,F)

tuber_blup <- tuber_blup[,-c(64,66, 68, 70)]


tuber_blup_1 <- tuber_blup[,-2] #delete un-necessary columns 

tuber_genentic_blup <- tuber_blup_1[]

#change cip number to real varieties name

tuber_genentic_blup$clone<- as.character(tuber_genentic_blup$clone)

tuber_genentic_blup$clone[tuber_genentic_blup$clon == "CIP39601.241"] <- "Izihirwe"
tuber_genentic_blup[tuber_genentic_blup == "CIP393371.58"] <- "Twihaze"
tuber_genentic_blup[tuber_genentic_blup == "CIP393077.159"] <- "Kazeneza"
tuber_genentic_blup[tuber_genentic_blup == "CIP398190.615"] <- "Ndeze"
tuber_genentic_blup[tuber_genentic_blup == "CIP393280.64"] <- "Nkunganire"
tuber_genentic_blup[tuber_genentic_blup == "CIP392797.22"] <- "Cyerekezo"
tuber_genentic_blup[tuber_genentic_blup == "CIP394611.112"] <- "Jyambere"
tuber_genentic_blup[tuber_genentic_blup == "CIP398190.89"] <- "Seka"
tuber_genentic_blup[tuber_genentic_blup == "Shangi"] <- "Ndamira"

rownames(tuber_genentic_blup) <-c(1:15)
#delete unnecessary tuber 
tuber_genentic_blup <- tuber_genentic_blup[-c(1,4,8,12),]

#delete "Cyerekezo" and "Kinigi because they do not exist in the tricot. 
tuber_genentic_blup <- tuber_genentic_blup[-c(1,9),]


#delete disease and insect blup data because its not clear and we have binary data 
tuber_genentic_blup <- tuber_genentic_blup[,-c(15:46)]




#tuber_genentic_blup <- tuber_genentic_blup[,c("clone" , "flowering_Predicted_mean_(Sum_of_fixef_and_BLUP)", "plant_uniformity_Predicted_mean_(Sum_of_fixef_and_BLUP)",
#                                              "plant_vigor_Predicted_mean_(Sum_of_fixef_and_BLUP)", "average_nber_of_tubers_per_plant_Predicted_mean_(Sum_of_fixef_and_BLUP)", 
#                                              "number_marketable_yield_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP)", "marketable_yield_ton_ha__Predicted_mean_(Sum_of_fixef_and_BLUP)", "totat_wight_of_tubers_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP"
#                                              ,"late_blight", "Leaf_miner_flies", "viruses", "bacterial_wilt")]

#bring predict value by pluus blup effect + average.
tuber_genentic_blup <- add_column(tuber_genentic_blup, "flowering_prediction" = tuber_genentic_blup[,"flowering_Predicted_mean_(Sum_of_fixef_and_BLUP)"] +  tuber_genentic_blup[,"flowering_average_performance"]
                                  ,.after ="flowering_average_performance"   )

tuber_genentic_blup <- add_column(tuber_genentic_blup, "plant_uniformity_prediction" = tuber_genentic_blup[,"plant_uniformity_Predicted_mean_(Sum_of_fixef_and_BLUP)"] +  tuber_genentic_blup[,"plant_uniformity_average_performance"]
                                  ,.after ="plant_uniformity_average_performance"   )


tuber_genentic_blup <- add_column(tuber_genentic_blup, "plant_vigor_prediction" = tuber_genentic_blup[,"plant_vigor_Predicted_mean_(Sum_of_fixef_and_BLUP)"] +  tuber_genentic_blup[,"plant_vigor_average_performance"]
                                  ,.after ="plant_vigor_average_performance"   )


tuber_genentic_blup <- add_column(tuber_genentic_blup, "average_nber_of_tubers_per_plant_prediction" = tuber_genentic_blup[,"average_nber_of_tubers_per_plant_Predicted_mean_(Sum_of_fixef_and_BLUP)"] +  tuber_genentic_blup[,"average_nber_of_tubers_per_plant_average_performance"]
                                  ,.after ="average_nber_of_tubers_per_plant_average_performance"   )


tuber_genentic_blup <- add_column(tuber_genentic_blup, "totat_wight_of_tubers_ton_ha_prediction" = tuber_genentic_blup[,"totat_wight_of_tubers_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP"] +  tuber_genentic_blup[,"totat_wight_of_tubers_ton_ha_average_performance"]
                                  ,.after ="totat_wight_of_tubers_ton_ha_average_performance"   )

#delete unnecessary columns 
tuber_genentic_blup <- tuber_genentic_blup[,c("clone" , "flowering_prediction", "plant_uniformity_prediction",
                                              "plant_vigor_prediction", "average_nber_of_tubers_per_plant_prediction", 
                                              "totat_wight_of_tubers_ton_ha_prediction"
                                              ,"late_blight", "Leaf_miner_flies", "viruses", "bacterial_wilt")]

str(tuber_genentic_blup)

#remove columns that have NA 


tuber_genentic_blup$late_blight <-as.numeric(tuber_genentic_blup$late_blight)
tuber_genentic_blup$Leaf_miner_flies <-as.numeric(tuber_genentic_blup$Leaf_miner_flies)
tuber_genentic_blup$viruses <-as.numeric(tuber_genentic_blup$viruses)
tuber_genentic_blup$bacterial_wilt <-as.numeric(tuber_genentic_blup$bacterial_wilt)
m_1<- mean(tuber_genentic_blup$late_blight )
m_2<- mean(tuber_genentic_blup$Leaf_miner_flies )
m_3<- mean(tuber_genentic_blup$viruses )
m_4<- mean(tuber_genentic_blup$bacterial_wilt )



#Cruza and Gisubizo do not exist in on station data. So, lets put fake data in data.frame(Not real if NA. item covariate does not)
tuber_genentic_blup <- tuber_genentic_blup %>%  add_row(clone ="Cruza",  flowering_prediction = traits_mean[1,], plant_uniformity_prediction = traits_mean[2,],
                                                        plant_vigor_prediction = traits_mean[3,], average_nber_of_tubers_per_plant_prediction = traits_mean[12,]
                                                        ,totat_wight_of_tubers_ton_ha_prediction = traits_mean[15,], late_blight = m_1, Leaf_miner_flies =m_2, viruses =m_3, bacterial_wilt = m_4)


tuber_genentic_blup <- tuber_genentic_blup %>%  add_row(clone ="Gisubizo", flowering_prediction = traits_mean[1,] , plant_uniformity_prediction = traits_mean[2,],
                                                        plant_vigor_prediction  = traits_mean[3,], average_nber_of_tubers_per_plant_prediction = traits_mean[12,]
                                                        ,totat_wight_of_tubers_ton_ha_prediction = traits_mean[15,], late_blight = m_1, Leaf_miner_flies =m_2, viruses =m_3, bacterial_wilt = m_4)



#create a csv File
write.table(tuber_genentic_blup,file="data/on_station-covariate_season 1.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
#write.table(tuber_blup,file="data/on_station-covariate_season 2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

####Done
###############################################################################
###############converted distribution to standard districbution################

test_dt <- read.csv("data/on_station-covariate_season 1.csv")
str(test_dt)
test_dt[,2]
mean(test_dt[,2])
summary(test_dt[,2])


potato_names<- test_dt[,1]
column_names<- names(test_dt)
tt_dt<- test_dt[,2:10]

x<- c(1:9)
st_dt <- list() 
for (i in x) {keep<- (tt_dt[,i] - mean(tt_dt[,i]))/sd((tt_dt[,i]))
st_dt[[i]] <- keep}

st_dt

st_dt_c <- as.data.frame(st_dt)

bb <- cbind(potato_names, st_dt_c)
names(bb) <- column_names
histogram(bb[,7])

write.table(bb,file="data/on_station-covariate_season 1_converted.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

