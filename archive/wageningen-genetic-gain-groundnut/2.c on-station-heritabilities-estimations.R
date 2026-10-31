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

setwd('Data_analysis_tricots/Groundnut/Groundnut_Feb_2024')

long_farm_data <- read.csv('Processed_data/on-station-data-groundnut-tanzania.csv')

table(long_farm_data$REP,long_farm_data$Location,long_farm_data$Year)

long_farm_data <- long_farm_data %>% mutate(genotype=factor(genotype),
                                            Year =factor(Year), 
                                            Location= factor(Location),
                                            NEW_SITE=factor(NEW_SITE),
                                            REP=factor(REP),ADM1_NAME=factor(ADM1_NAME))


groudnut <- lmer(PY_ton_ha~ Location + Year+(1|genotype)+(1|genotype:Year)+(1|genotype:Location)+
                   (1|genotype:Year:Location)+(1|Location:ADM1_NAME:Year:REP),data=long_farm_data)



varCorrM1 <- VarCorr(groudnut)

varCorrM0 <- as.data.frame(varCorrM1)


# cov_estim <- varcomp(var_comp)

vgly <- varCorrM0[varCorrM0$grp=="genotype:Year:Location",4]
vgl <- varCorrM0[varCorrM0$grp=="genotype:Location",4]
vgy <- varCorrM0[varCorrM0$grp=="genotype:Year",4]
vg <- varCorrM0[varCorrM0$grp=="genotype",4]
ve <- varCorrM0[varCorrM0$grp=="Residual",4]

l <- length(unique(long_farm_data$Location))
y <- length(unique(long_farm_data$Year))
r <- round(mean(table(paste(long_farm_data$genotype,long_farm_data$Location,long_farm_data$Year))))

vg/((vg+vgy/y+vgl/l+vgly/(y*l)+ve/(l*y*r)))


long_farm_data$zone_year <- paste(long_farm_data$Location,long_farm_data$Year,sep='_')

tab_zone <- table(long_farm_data$zone_year)

ls_on_station_data_her <- split(long_farm_data,long_farm_data$zone_year)

h2 <- lapply(ls_on_station_data_her,function(w){
  if(nrow(w)<20){return(NULL)}
  w <- w[c('PY_ton_ha','Location','REP',"genotype",'ADM1_NAME2','Year')]
  w <- w[complete.cases(w),]
  w <- droplevels(w)
  
  y = length(unique(w$Year))
  r = round(mean(table(w$genotype)))
  if(y==1){
    mixMod <- lmer(PY_ton_ha~(1|genotype)+(1|ADM1_NAME2:REP),data=w) 
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    se = df[df$grp=="Residual",5]
    sg = df[df$grp=="genotype",5]
    sr = df[df$grp=="ADM1_NAME2:REP",5]
    
    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+se^2/r)
    h2_2 <- sg^2/(sg^2+se^2)
    
  }else{
    mixMod <- lmer(PY_ton_ha~(1|GENOTYPE)+Year+(1|GENOTYPE:Year),data=w) 
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    se = df[3,5]
    sg = df[2,5]
    sy = df[1,5]
    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+sy/y+se^2/(y*r))
    h2_2 <- sg^2/(sg^2+sy^2+se^2)
  }
  
  
  
  
  
  data.frame(varg = sg^2,sr=sr^2, vare = se^2,location=unique(w$Location),h2_1=round(h2_1,2),h2_2=round(h2_2,2),r=r,
             n=nrow(w),years=paste(sort(unique(w$Year)),collapse = "-"))
  
})

do.call(rbind,h2)

