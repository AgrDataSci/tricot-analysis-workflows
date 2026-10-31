# Analysis of on-station data

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
library(readxl)
library(sf)
library(terra)
library(stringr)
library(ggpmisc)
library(emmeans)

long_farm_data <- read.csv('Processed_data/on-station-data-groundnut-tanzania.csv')

table(long_farm_data$REP,long_farm_data$Location,long_farm_data$Year)

long_farm_data <- long_farm_data %>% mutate(genotype=factor(genotype),
                                            Year =factor(Year), 
                                            Location= factor(Location),
                                            NEW_SITE=factor(NEW_SITE),
                                            REP=factor(REP),ADM1_NAME=factor(ADM1_NAME))


mixmodel1 <- lmer(PY_ton_ha~ Location + Year+genotype+(1|genotype:Year)+(1|genotype:Location)+
                   (1|genotype:Year:Location)+(1|Location:ADM1_NAME:Year:REP),data=long_farm_data)

mixmodel2 <- lmer(PY_ton_ha~ Location + Year+genotype+
                    (1|genotype:Year)+(1|genotype:Location)+
                    (1|genotype:Year:Location)+
                    (1|Location:ADM1_NAME:Year:REP),data=long_farm_data)


on_farm_means <- predictmeans(mixmodel1,'genotype', mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)

write.csv(on_farm_means$mean_table,'Results/Datasets/on_station_yield_genotypic_means.csv',row.names = F)

# On station by location

long_farm_data$zone_year <- paste(long_farm_data$Location,long_farm_data$Year)

ls_on_station_data_her <- split(long_farm_data,long_farm_data$zone_year)

location_means <- lapply(ls_on_station_data_her,function(w){
  if(nrow(w)<20){return(NULL)}
  w <- w[c('PY_ton_ha','Location','REP',"genotype",'ADM1_NAME2','Year')]
  w <- w[complete.cases(w),]
  w <- droplevels(w)
  
  y = length(unique(w$Year))
  r = round(mean(table(w$genotype)))
  if(y==1){
    mixMod <- lmer(PY_ton_ha~genotype+(1|ADM1_NAME2:REP),data=w) 

    means <-  predictmeans(mixMod,'genotype', mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    means <-  means$mean_table
    
  }else{
    mixMod <- lmer(PY_ton_ha~genotype+Year+(1|ADM1_NAME2:REP),data=w) 
    
    
    means <-  predictmeans(mixMod,'genotype', mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    means <-  means$mean_table
  }
  

  data.frame(means,Location=unique(w$Location),year=unique(w$Year))
  
})

location_means <- do.call(rbind,location_means)

write.csv(location_means,'Results/Datasets/on_station_location_yield_genotypic_means.csv',row.names = F)





