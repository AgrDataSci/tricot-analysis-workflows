
# Data analysis metric-rankings - cassava
# December 2023
# Estimation of genetic parameters

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

setwd(here())

#setwd("Parameters_estimations_tricots/")


# Variance component estimations (no/time/spatial effects)

groundnut_data <- read.csv('Processed_data/groundnut-tanzania_metric_dataset_post_AE.csv')


groundnut_data$year <- factor(groundnut_data$year)


# Heritabilities estimation

groundnut_data <-groundnut_data %>% mutate(ecoregion =factor(ecoregion ),year=factor(year),genotype=factor(genotype),ecoregion=factor(ecoregion),farm=factor(farm))

mixmodel1 <- lmer(yieldton_ha~ecoregion+year+genotype+(1|ecoregion:farm)+
                    (1|genotype:ecoregion)+(1|genotype:year)+(1|genotype:year:ecoregion),
                  data=groundnut_data,na.action = 'na.omit' )



on_farm_means <- predictmeans(mixmodel1,'genotype', mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)

write.csv(on_farm_means$mean_table,'Results/Datasets/on_farm_yield_genotypic_means.csv',row.names = F)

#----------------------------

groundnut_data$location <- paste(groundnut_data$ecoregion,sep='_')

tab_zone <- table(groundnut_data$location)

ls_groundnut_her <- split(groundnut_data,groundnut_data$location)

ls_state <- split(groundnut_data,groundnut_data$ecoregion)

gg <- lapply(ls_state, function(x){
  if(nrow(x[complete.cases(x),])==0){return(NULL)}
  
  if(length(unique(x$year))==1){
    mixmodel0 <- lmer(yieldton_ha~ genotype+  
                        (1|farm),data=x,na.action = 'na.omit')# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'genotype', mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table)
    
    data.frame(genotype.means,state=unique(x$ecoregion))
    
  }else{
    
    mixmodel0 <- lmer(yieldton_ha~ genotype+ year + (1|genotype:year) +
                        (1|farm),data=x,na.action = 'na.omit')# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'genotype', mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table)
    
    data.frame(genotype.means,state=unique(x$ecoregion))
  }

}

)

complete_gg <- do.call(rbind,gg)

write.csv(complete_gg,'Results/Datasets/on_farm_yield_location_genotypic_means.csv',row.names = F)


