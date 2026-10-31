
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
library(ggrepel)
library(ggpmisc)
library(quantreg)

rm(list=ls())

setwd('Data_analysis_tricots/Cassava/Cassava_feb_2024/')

## On farm

# Metric 

on_farm_means     <- read.csv("Results/Datasets/on_farm_fresh_yield_genotypic_means.csv")

on_farm_means_loc <- read.csv("Results/Datasets/on_farm_fresh_yield_location_genotypic_means.csv")

# Ranks

on_farm_ranks    <-   read.csv("Results/Datasets/on_farm_ranks_fresh_yield_log_worth.csv")

on_farm_over_ranks <-   read.csv("Results/Datasets/on_farm_ranks_harvestoverallimpression_log_worth.csv")

on_farm_replan_ranks <-   read.csv("Results/Datasets/on_farm_ranks_finalchoicereplant_log_worth.csv")

on_farm_ranks_loc <- read.csv("Results/Datasets/on_farm_ranks_fresh_yield_location_log_worth.csv")

on_farm_ranks_over_loc <- read.csv("Results/Datasets/on_farm_ranks_harvestoverallimpression_location_log_worth.csv")

on_farm_ranks_replan_loc <-  read.csv("Results/Datasets/on_farm_marketable_yield_location_genotypic_means.csv")

## On station

on_station_means <- read.csv("Results/Datasets/on_station_fresh_yield_complete_genotypic_means.csv")

on_station_means_over <- read.csv("Results/Datasets/on_station_fresh_yield_overlapregioun_genotypic_means.csv")

on_station_means_loc <- read.csv("Results/Datasets/on_station_fresh_yield_locations_genotypic_means.csv")

on_station_means_over_loc <- read.csv("Results/Datasets/on_station_fresh_yield_overlapregion_locations_genotypic_means.csv")

on_station_means_over_loc2020 <- read.csv("Results/Datasets/on_station_fresh_yield_overlapregioun2020_genotypic_means.csv")


##-----------------------------------------------------------------------------
#                             Comparing means

# all data available

on_farm_on_station_yld <- on_farm_means[c('genotype','emmean')] %>% inner_join(on_station_means[c('genotype','Mean')],by='genotype')

g0 <- ggplot(on_farm_on_station_yld,aes(y=emmean,x=Mean))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+xlab("Genotypic means of on-farm yield (t/ha)")+ylab("PL Log(worth) of farmer rankings of yield")+
  theme_bw()


ggsave("Results/graphics/on_farm_rank_on_farm_yield.png",g0,height = 4,width = 5)


on_farm_on_station_loc_yld <- on_farm_means_loc[c('genotype','Mean','Zone')] %>% inner_join(on_station_means_loc[c('genotype','Mean','zone')],by=c('Zone'='zone','genotype'='genotype'))

on_farm_on_station_loc_yld <- droplevels(on_farm_on_station_loc_yld)

g1 <- ggplot(on_farm_on_station_loc_yld,aes(y=Mean.x,x=Mean.y))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+facet_grid(.~Zone)+xlab("Genotypic means of on-station yield (t/ha)")+ylab("Genotypic means of on-farm yield (t/ha)")+
  theme_bw()

ggsave("Results/Correlation_data_analysis/on_farm_on_station_metric_loc_full_datasets.png",g1,height = 4,width = 12)

# Whithin regions

on_farm_on_station_yld_over <- on_farm_means[c('genotype','emmean')] %>% inner_join(on_station_means_over[c('genotype','Mean')],by='genotype')

cor.test(on_farm_on_station_yld_over$emmean,on_farm_on_station_yld_over$Mean)

g2 <- ggplot(on_farm_on_station_yld_over,aes(y=emmean,x=Mean))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+xlab("Genotypic means of on-station yield (t/ha)")+ylab("Genotypic means of on-farm yield (t/ha)")+
  theme_bw()

cov(on_farm_on_station_yld_over$emmean,on_farm_on_station_yld_over$Mean)/

ggsave("Results/Correlation_data_analysis/on_farm_rank_on_farm_yield_whitin_locations.png",g2,height = 4,width = 5)

on_farm_on_station_yld_over_loc <- on_farm_means_loc[c('genotype','Mean','Zone')] %>% inner_join(on_station_means_over_loc[c('genotype','Mean','zone')],by=c('genotype'='genotype','Zone'='zone'))

g3 <- ggplot(on_farm_on_station_yld_over_loc,aes(y=Mean.x,x=Mean.y))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+facet_grid(.~Zone)+geom_smooth(method = "lm",se=F,alpha=0.2)+xlab("Genotypic means of on-station yield (t/ha)")+ylab("Genotypic means of on-farm yield (t/ha)")+
  theme_bw()


ggsave("Results/graphics/on_farm_rank_on_farm_yield.png",g3,height = 4,width = 5)

on_farm_station_mean_over2020 <- on_farm_means_loc[on_farm_means_loc$Zone=='South West',][c('genotype','Mean')] %>% inner_join(on_station_means_over_loc2020[c('genotype','Mean')],by='genotype')

g4 <- ggplot(on_farm_station_mean_over2020,aes(y=Mean.x,x=Mean.y))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+xlab("Genotypic means of on-station yield (t/ha)")+ylab("Genotypic means of on-farm yield (t/ha)")+
  theme_bw()


ggsave("Results/graphics/on_farm_rank_on_farm_yield.png",g4,height = 4,width = 5)


