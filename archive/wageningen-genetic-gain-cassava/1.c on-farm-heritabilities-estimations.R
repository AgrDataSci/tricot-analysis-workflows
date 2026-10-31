
# Data analysis metric-rankings - cassava
# December 2023
# Estimation of genetic parameters

###############################################################################
############################# No processed ####################################
###############################################################################

rm(list=ls())

library(nlme)
library(ggplot2)
library(tidyr)
library(predictmeans)
library(sp)
library(geodist)
library(leaflet)
library(mapview)
library(webshot)
library(lubridate)
library(tidyverse)
library(here)
library(gosset)
library(PlackettLuce)
library(insight)
library(ggpmisc)
library(emmeans)

setwd(here())

setwd('Data_analysis_tricots/Cassava/Cassava_feb_2024/')

cassava_data <- read.csv('Processed_data/cassava_metric_dataset_post_AE.csv')

cassava_data <- cassava_data %>% mutate(Zone=as.factor(Zone),genotype=as.factor(genotype),farm=as.factor(farm))

mixmodel0 <- lmer(yieldton_ha~(1|genotype)+Zone+(1|Zone:farm)+(1|genotype:Zone),data=cassava_data,na.action = 'na.omit' )

vc <- VarCorr(mixmodel0)

vc <- data.frame(vc)

ve   <- vc[vc$grp=="Residual","vcov"]
vg   <- vc[vc$grp=="genotype","vcov"]
vgl  <- vc[vc$grp=="genotype:Zone",'vcov']

r <- round(mean(table(paste(cassava_data$genotype,cassava_data$Zone,sep='-'))))
l <- length(unique(cassava_data$state))

vg/(vg+vgl/l+ve/(l*r))

# Complete effect

mixmodel1 <- lmer(yieldton_ha~(1|genotype)+(1|Zone)+(1|Zone:farm)+(1|genotype:Zone),data=cassava_data,na.action = 'na.omit' )

vc <- VarCorr(mixmodel1)

vc <- data.frame(vc)


#----------

H2_cullis.calc<-function(mixmodel){
  
  BLUPs=ranef(mixmodel)$genotype
  V_BLUP<-var(c(outer(unlist(BLUPs),unlist(BLUPs),"-")))
  VC=as.data.frame(VarCorr(mixmodel))
  Vg<-VC$vcov[VC$grp=="genotype"] 
  H2_cullis=1-V_BLUP/(2* Vg)
  return(H2_cullis)	
  
}


H2_cullis.calc(mixmodel=mixmodel0)

#---------

library(lme4)
# Genotype as random effect

g.ran <- mixmodel0
  
# Genotype as fixed effect
g.fix <- lmer(yield ~ gen + rep + (1|rep:block), 
              data=dat)

##########################
# Handle model estimates #
##########################
# Genetic variance component
vc.g <- summary(g.ran)$varcor$genotype[1] #
vc.g #0.142902
vc.e <- summary(g.ran)$sigma^2
vc.e #0.08162035
reps <- with(dat, table(gen))
n.rep <- mean(reps); n.rep # 3 eplicates each
n.inds <- length(reps); n.inds

# Mean variance of a difference of two genotypic BLUEs
vdBLUE.mat <- diag(vcov(g.fix))[1:n.inds] # obtain squared s.e.d. matrix 
vdBLUE.avg <- mean(vdBLUE.mat) # take mean of upper triangle
vdBLUE.avg #0.07010875

# Mean variance of a difference of two genotypic BLUPs
vdBLUP.mat <- (unlist(abs(ranef(g.ran,condVar=TRUE)$genotype)))# obtain squared s.e.d. matrix 
vdBLUP.avg <- mean(vdBLUP.mat)^2 # take mean of upper triangle
vdBLUP.avg #0.05455038

#############
# H2 Cullis #
#############
H2Cullis <- 1 - (vdBLUP.avg/(vc.g*2))



#--------

# heratabilities

casava_metric_complete <- cassava_data[c('yieldton_ha','genotype','farm','Zone')]

casava_metric_complete <- casava_metric_complete[complete.cases(casava_metric_complete),]

casava_metric_complete$zone_year <- paste(casava_metric_complete$Zone,sep='_')

tab_zone <- table(casava_metric_complete$zone_year)

ls_cassava_data_her <- split(casava_metric_complete,casava_metric_complete$zone_year)

ls_cassava_data_her <- ls_cassava_data_her

#csd1 <- cassava_data[c('genotype','zone_year','yieldton_ha')]

#ggplot(droplevels(csd1[complete.cases(csd1),]),aes(x=genotype,y=yieldton_ha))+ geom_boxplot() + theme_bw() +facet_wrap(~zone_year)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

h2 <- lapply(ls_cassava_data_her,function(w){
  print(unique(w$zone_year))
  w <- w[c('yieldton_ha','genotype','farm')]
  w <- w[complete.cases(w),]
  w <- droplevels(w)
  
  
  if(is.null(w) | nrow(w) < 20){return(NULL)}
  
  mixMod <- lmer(yieldton_ha~(1|genotype)+(1|farm),data=w) 
  ggplot(w,aes(x=genotype,y=yieldton_ha))+ geom_boxplot() + theme_bw() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(summary(mixMod))
  df <- data.frame(VarCorr(mixMod))
  print(table(w$genotype))
  r = round(mean(table(w$genotype)))
  print(r)
  se = df[df$grp=="Residual",5]
  sg = df[df$grp=="genotype",5]
  sf = df[df$grp=="farm",5]
  #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
  h2_1 <- sg^2/(sg^2+se^2/r)
  h2_2 <- sg^2/(sg^2+se^2)
  data.frame(sg,sf,se,round(h2_1,2),round(h2_2,2),r=r,n=nrow(w))
})

do.call(rbind,h2)
