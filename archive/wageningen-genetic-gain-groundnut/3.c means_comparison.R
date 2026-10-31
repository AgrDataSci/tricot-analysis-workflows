

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


on_farm_means     <- read.csv("Results/Datasets/on_farm_yield_genotypic_means.csv")

on_station_means <- read.csv("Results/Datasets/on_station_yield_genotypic_means.csv")

on_farm_log_worth <- read.csv("Results/Datasets/log_worths_rank_yield.csv")

cor(daset_cor$Mean.x,daset_cor$Mean.y,use="complete.obs")


# all data available

on_farm_on_station_yld <-  on_farm_means[c('genotype','Mean')] %>% inner_join(on_station_means[c('genotype','Mean')],by="genotype")

g0 <- ggplot(on_farm_on_station_yld,aes(x=Mean.y,y=Mean.x))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+ylim(c(0.5,1.2))+
  ylab("Genotypic means of on-farm yield (t/ha)")+xlab("Genotypic means of on-station yield (t/ha)")+
  theme_bw()


ggsave("Results/Correlation_data_analysis/Correlation_data_analysis.png",g0,height = 5,width = 6)


on_farm_on_station_yld <-  on_farm_means[c('genotype','Mean')] %>% inner_join(on_station_means[c('genotype','Mean')],by="genotype")

g1 <- ggplot(on_farm_on_station_yld,aes(x=Mean.y,y=Mean.x))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+ylim(c(0.5,1.2))+
  ylab("Genotypic means of on-farm yield (t/ha)")+xlab("Genotypic means of on-station yield (t/ha)")+
  theme_bw()


ggsave("Results/Correlation_data_analysis/Correlation_data_analysis.png",g0,height = 5,width = 6)


on_farm_on_station_rnk <-  on_station_means[c('genotype','Mean')] %>% inner_join(on_farm_log_worth[c('genotype','coef_estimate')],by="genotype")

g2 <- ggplot(on_farm_on_station_rnk,aes(x=Mean,y=coef_estimate))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+#ylim(c(0.5,1.2))+
  ylab("PL Log(worth) of farmer rankings of yield")+xlab("Genotypic means of on-station yield (t/ha)")+
  theme_bw()

cor.test(on_farm_on_station_rnk$Mean,on_farm_on_station_rnk$coef_estimate)

ggsave("Results/Correlation_data_analysis/Correlation_data_analysis_farmer_ranks_onstation_means.png",g2,height = 5,width = 6)



on_farm_on_farm_rnk <-  on_farm_means[c('genotype','Mean')] %>% inner_join(on_farm_log_worth[c('genotype','coef_estimate')],by="genotype")

g3 <- ggplot(on_farm_on_farm_rnk,aes(x=Mean,y=coef_estimate))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+#ylim(c(0.5,1.2))+
  ylab("PL Log(worth) of farmer rankings of yield")+xlab("Genotypic means of on-farm yield (t/ha)")+
  theme_bw()

cor.test(on_farm_on_farm_rnk$Mean,on_farm_on_farm_rnk$coef_estimate)

ggsave("Results/Correlation_data_analysis/Correlation_data_analysis_farmer_ranks_famer_means.png",g3,height = 5,width = 6)


