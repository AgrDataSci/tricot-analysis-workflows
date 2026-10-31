

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

setwd('Data_analysis_tricots/Cassava/Cassava_feb_2024')

source('Scripts/0.anova.PL.R')

on_station_data <- read.csv("Processed_data/on-station-data-cassava-nigeria.csv")

on_station_data <- on_station_data %>%  mutate(genotype=factor(genotype),Zone=factor(Zone),Year=factor(Year),rep_number=factor(rep_number))

on_station_data1 <- on_station_data[on_station_data$on_farm_zone == 'Within of on-farm zone' ,]

on_station_data2 <- on_station_data[on_station_data$on_farm_zone == 'Within of on-farm zone' & on_station_data$Year==2020,] # paired with on-farm only one location

# complete  model

var_comp_gen <- lmer(fyld~genotype+Zone+Year+(1|Zone:rep_number) +
                      (1|genotype:Zone)+(1|genotype:Year)+
                      (1|genotype:Year:Zone),data=on_station_data) # I removed year:zone effect after cause problems

means <- predictmeans(var_comp_gen,"genotype",barplot=FALSE, pplot=F, bkplot=F, plot=F)

write.csv(means$mean_table,"Results/Datasets/on_station_fresh_yield_complete_genotypic_means.csv",row.names = F)

# Within the region

var_comp_gen <- lmer(fyld~genotype+Zone+Year+(1|Zone:rep_number) +
                       (1|genotype:Zone)+(1|genotype:Year)+
                       (1|genotype:Year:Zone),data=on_station_data1) # I removed year:zone effect after cause problems

means <- predictmeans(var_comp_gen,"genotype",barplot=FALSE, pplot=F, bkplot=F, plot=F)

write.csv(means$mean_table,"Results/Datasets/on_station_fresh_yield_overlapregioun_genotypic_means.csv",row.names = F)


# Within the region almost same year

var_comp_gen <- lmer(fyld~genotype+(1|rep_number) ,data=on_station_data2) # I removed year:zone effect after cause problems

means <- predictmeans(var_comp_gen,"genotype",barplot=FALSE, pplot=F, bkplot=F, plot=F)

write.csv(data.frame(means$mean_table,Zone=unique(on_station_data2$Zone)),"Results/Datasets/on_station_fresh_yield_overlapregioun2020_genotypic_means.csv",row.names = F)

# ---------------------------------------------------------------------

# by state

ls_state <- split(on_station_data,paste0(on_station_data$Zone))

gg <- lapply(ls_state, function(x){
  
  print(unique(x$Zone))
  if(length(unique(x$Year)==1)){
    mixmodel0 <- lmer(fyld~ genotype+(1|rep_number),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    print(mixmodel0)
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'genotype',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table)

    data.frame(genotype.means,zone=unique(x$Zone))
    
  }else{
    
    mixmodel0 <- lmer(fyld~ genotype+ (1|rep_number) + 
                        (1|Year)+(1|genotype:Year),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    print(mixmodel0)
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'genotype',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table)
   
    data.frame(genotype.means,zone=unique(x$Zone))
  }
 
}

)

library(ggpmisc) 

complete_gg <- do.call(rbind,gg)

write.csv(complete_gg,"Results/Datasets/on_station_fresh_yield_locations_genotypic_means.csv")

#-----------------------------------------

# by state

ls_state <- split(on_station_data1,paste0(on_station_data1$Zone))

gg <- lapply(ls_state, function(x){
  
  print(unique(x$Zone))
  if(length(unique(x$Year)==1)){
    mixmodel0 <- lmer(fyld~ genotype+(1|rep_number),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    print(mixmodel0)
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'genotype',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table)
    
    data.frame(genotype.means,zone=unique(x$Zone))
    
  }else{
    
    mixmodel0 <- lmer(fyld~ genotype+ (1|rep_number) + 
                        (1|Year)+(1|genotype:Year),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    print(mixmodel0)
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'genotype',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table)
    
    data.frame(genotype.means,zone=unique(x$Zone))
  }
  
}

)

library(ggpmisc) 

complete_gg <- do.call(rbind,gg)

write.csv(complete_gg,"Results/Datasets/on_station_fresh_yield_overlapregion_locations_genotypic_means.csv")

# On-station year location


ls_state <- split(on_station_data1,paste0(on_station_data1$Zone,'-',on_station_data1$Year))

gg <- lapply(ls_state, function(x){
  
  print(unique(x$Zone))
  if(length(unique(x$Year)==1)){
    mixmodel0 <- lmer(fyld~ genotype+(1|rep_number),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    print(mixmodel0)
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'genotype',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table)
    
    data.frame(genotype.means,zone=unique(x$Zone),year=unique(x$Year))
    
  }else{
    
    mixmodel0 <- lmer(fyld~ genotype+ (1|rep_number) + 
                        (1|Year)+(1|genotype:Year),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    print(mixmodel0)
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'genotype',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table)
    
    data.frame(genotype.means,zone=unique(x$Zone))
  }
  
}

)

library(ggpmisc) 

complete_gg <- do.call(rbind,gg)

write.csv(complete_gg,"Results/Datasets/on_station_fresh_yield_overlapregion_locations_genotypic_means_qv.csv")


