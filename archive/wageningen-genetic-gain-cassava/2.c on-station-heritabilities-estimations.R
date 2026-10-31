
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

on_station_data <- read.csv("Processed_data/on-station-data-cassava-nigeria.csv")

on_station_data <- on_station_data %>%  mutate(genotype=factor(genotype),Zone=factor(Zone),Year=factor(Year),rep_number=factor(rep_number))

on_station_data1 <- on_station_data[on_station_data$on_farm_zone == 'Within of on-farm zone' ,]

on_station_data2 <- on_station_data[on_station_data$on_farm_zone == 'Within of on-farm zone' & on_station_data$Year==2020,] # paired with on-farm only one location

# complete  model

mixmodel1 <- lmer(fyld~Zone+Year+(1|genotype)+(1|Zone:rep_number) +
                       (1|genotype:Zone)+(1|genotype:Year)+
                       (1|genotype:Year:Zone),data=on_station_data1) # I removed year:zone effect after cause problems




vc <- VarCorr(mixmodel1)

vc <- data.frame(vc)

ve   <- vc[vc$grp=="Residual","vcov"]
vg   <- vc[vc$grp=="genotype","vcov"]
vgl  <- vc[vc$grp=="genotype:Zone",'vcov']
vgy  <- vc[vc$grp=="genotype:Year",'vcov']
vgyl  <- vc[vc$grp=="genotype:Year:Zone",'vcov']

r <- round(mean(table(paste(on_station_data$genotype,on_station_data$Zone,on_station_data$Year,sep='-'))))
l <- length(unique(on_station_data1$Zone))
y <- length(unique(on_station_data1$Year))

vg/(vg+vgl/l+vgy/y+vgyl/(l*y)+ve/(l*r*y))



# Within the region

var_comp_gen <- lmer(fyld~Zone+Year+(1|genotype)+(1|Zone:rep_number) +
                       (1|genotype:Zone)+(1|genotype:Year)+
                       (1|genotype:Year:Zone),data=on_station_data1) # I removed year:zone effect after cause problems



H2_cullis.calc<-function(mixmodel){
  
  BLUPs=ranef(mixmodel)$genotype
  V_BLUP<-var(c(outer(unlist(BLUPs),unlist(BLUPs),"-")))
  VC=as.data.frame(VarCorr(mixmodel))
  Vg<-VC$vcov[VC$grp=="genotype"] 
  H2_cullis=1-V_BLUP/(2* Vg)
  return(H2_cullis)	
  
}


H2_cullis.calc(mixmodel=var_comp_gen)

# estimation by regions

unique(on_station_data1[c('Zone','zone_year')])

tab_zone <- table(on_station_data1$zone_year)

ls_on_station_data_her <- split(on_station_data1,paste(on_station_data1$Zone,on_station_data1$Year,sep="_"))

h2 <- lapply(ls_on_station_data_her,function(w){
  w <- w[c('fyld','rep_number','genotype','Year',"Zone")]
  w <- w[complete.cases(w),]
  w <- w[complete.cases(w),]
  w <- droplevels(w)
  
  y = length(unique(w$Year))
  r = round(mean(table(w$genotype)))
  if(y==1){
    mixMod <- lmer(fyld~(1|rep_number)+(1|genotype),data=w) 
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    se = df[df$grp=='Residual',5]
    sr = df[df$grp=='rep_number',5]
    sg = df[df$grp=='genotype',5]
    
    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+se^2/r)
    h2_2 <- sg^2/(sg^2+se^2)
    
  }else{
    mixMod <- lmer(fyld~(1|accession_name)+(1|Year)+(1|rep_number)+(1|accession_name:Year),data=w) 
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    se = df[4,5]
    sg = df[2,5]
    sy = df[3,5]
    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+sy/y+se^2/(y*r))
    h2_2 <- sg^2/(sg^2+sy^2+se^2)
  }
  
  
  
  print(r)
  
  data.frame(zone=unique(w$Zone),sg,sr,se,h2_1=round(h2_1,2),h2_2=round(h2_2,2),r=r,n=nrow(w),years=paste(sort(unique(w$Year)),collapse = "-"))
})

do.call(rbind,h2)
  
