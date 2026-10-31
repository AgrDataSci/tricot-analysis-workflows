
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
library(ggpmisc) 
library(ggpubr)
library(insight)
library(emmeans)
library(asreml)

setwd('Data_analysis_tricots/Groundnut/Groundnut_Feb_2024')

dataset <- read.csv('Processed_data/groundnut-tanzania_metric_dataset_post_AE.csv')

dataset <- dataset %>% mutate(genotype=factor(genotype),farm=factor(farm),ecoregion=factor(ecoregion),year=factor(year))

mixmodel0 <- lmer(yieldton_ha~(1|genotype)+year+(1|genotype:year)+(1|genotype:ecoregion)+(1|genotype:ecoregion:year)+(1|ecoregion:farm)+ecoregion,data=dataset)



dataset$yield_kg_ha <- dataset$yieldton_ha*1000

# Model happy paper

mixmodel0 <- lmer(yield_kg_ha~(1|genotype)+year+(1|genotype:year)+(1|genotype:ecoregion)+(1|genotype:ecoregion:year)+(1|ecoregion:farm)+ecoregion,data=dataset)

oats.asr <- asreml(yield_kg_ha ~ year+ecoregion, random = ~ genotype +genotype:year+genotype:ecoregion+genotype:ecoregion:year+ecoregion:farm , data = dataset)

summary(oats.asr)

# Estimation of population mena

mixmodel1 <- lmer(yield_kg_ha~(1|genotype)+(1|genotype:year)+(1|genotype:ecoregion)+(1|genotype:ecoregion:year)+(1|ecoregion:farm),data=dataset)


emmeans(mixmodel0)

summary(mixmodel0)

vc_tot <- VarCorr(mixmodel0)

vc_tot <- data.frame(vc_tot)

ve   <- vc_tot[vc_tot$grp=="Residual",4]
vg   <- vc_tot[vc_tot$grp=="genotype",4]
vgl  <- vc_tot[vc_tot$grp=="genotype:ecoregion",4]
vgy  <- vc_tot[vc_tot$grp=="genotype:year",4]
vgyl <- vc_tot[vc_tot$grp=="genotype:ecoregion:year",4]

r <- round(mean(table(paste(dataset$ecoregion,dataset$year,dataset$genotype,sep='-'))))
l <- length(unique(dataset$ecoregion))
y <- length(unique(dataset$year))


vg/(vg+vgl/l+vgy/y+vgyl/(y*l)+ve/(y*l*r))


# heratabilities

groundnut_data_full <- dataset[complete.cases(dataset[c('ecoregion','year')]),]

dataset$location <- paste(dataset$ecoregion,dataset$year,sep='_')

tab_zone <- table(dataset$location)

ls_groundnut_her <- split(dataset,dataset$location)

h2 <- lapply(ls_groundnut_her,function(w){
  w <- w[c('yield_kg_ha','genotype','farm', "year", "location")]
  w <- w[complete.cases(w),]
  w <- droplevels(w)
  
  
  if(nrow(w)<20){return(NULL)}

    
    mixMod <- lmer(yield_kg_ha~(1|genotype)+(1|farm),data=w) 
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    print(table(w$genotype))
    r = round(mean(table(w$genotype)))
    y = length(unique(w$year))
    
    print(r)
    
    se  = df[df$grp=="Residual",5]
    sg  = df[df$grp=="genotype",5]
    sf  = df[df$grp=="farm",5]
    
    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+se^2/(r))
    h2_2 <- sg^2/(sg^2+se^2)
    data.frame(mu=fixed.effects(mixMod)[1],cv=sd(w$yield_kg_ha)/mean(w$yield_kg_ha),varg=sg^2,varf=sf^2,vare=se^2,location=unique(w$location),h1=round(h2_1,6),h2=round(h2_2,6),n=nrow(w),r=r,years=paste(sort(unique(w$year)),collapse = "-"))
  
  
})

h2 <- do.call(rbind)

h2.mod <- h2 %>% separate(location,c('ecozone',NA),sep="_")

rbind(
h2.mod[c('mu','ecozone','years')] %>% pivot_wider(names_from = 'ecozone',values_from = 'mu'),

h2.mod[c('cv','ecozone','years')] %>% pivot_wider(names_from = 'ecozone',values_from = 'cv'),

h2.mod[c('h1','ecozone','years')] %>% pivot_wider(names_from = 'ecozone',values_from = 'h1')
)

names(h2) <- c("varg","vare","location",'h1','h2','n','year')

# write.csv(x = h2,file = "Results/heritabilities_loc_year.csv",row.names = T)


# Means estimation

groundnut_data_full <- dataset[complete.cases(dataset[c('ecoregion','year')]),]

dataset$location <- paste(dataset$ecoregion,dataset$year,sep='_')

tab_zone <- table(dataset$location)

ls_groundnut_her <- split(dataset,dataset$location)

h2 <- lapply(ls_groundnut_her,function(w){
  w <- w[c('yield_kg_ha','genotype','farm', "year", "location",'ecoregion')]
  w <- w[complete.cases(w),]
  w <- droplevels(w)
  
  
  if(nrow(w)<20){return(NULL)}

    
    mixMod <- lmer(yield_kg_ha~(genotype)+(1|farm),data=w) 
    
    em <- data.frame(emmeans(mixMod,'genotype'))
    
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    print(table(w$genotype))
    r = round(mean(table(w$genotype)))
    y = length(unique(w$year))
    
    print(r)
    
    se  = df[df$grp=="Residual",5]
    sg  = df[df$grp=="genotype",5]
    sf  = df[df$grp=="farm",5]
    
    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+se^2/(r))
    h2_2 <- sg^2/(sg^2+se^2)
    data.frame(location=unique(w$ecoregion),em[,c(1,2)],years=paste(sort(unique(w$year)),collapse = "-"))
  
  
})

ds <- do.call(rbind,h2)

ds2 <- pivot_wider(ds,names_from = location,values_from = emmean)

ds2  <- arrange(ds2,genotype)

write.csv(ds2,'Results/groundnut_means.csv')

