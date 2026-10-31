
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
library(asreml)

onstation_means <- read.csv("Results/Datasets/on_station_fresh_yield_overlapregion_locations_genotypic_means_qv.csv")

onfarm_means <- read.csv("Results/Datasets/on_farm_ranks_finalchoicereplant_location_log_worth_qv.csv")

on_farm_means <- read.csv('Results/Datasets/on_farm_fresh_yield_location_genotypic_means.csv')

consolidated0  <- onstation_means %>% inner_join(onfarm_means,by=c('genotype'='genotype','year'='year','zone'='Zone' ))

row.names(consolidated0) <- NULL

consolidated <- consolidated0[consolidated0$zone=='South West' & consolidated0$year==2019,]

consolidated$weight_pl <-   mean(1/consolidated$quasiVar)

consolidated$genotype <- factor(consolidated$genotype)

var_error_log_worth <- mean(consolidated$quasiVar)

RankTwoStagesPLWd  <- asreml(fixed = estimate ~ 1,
                             random = ~  genotype,
                             weights = weight_pl,
                             family = (asr_gaussian(dispersion = 1)),
                             data = consolidated)

vc <- summary(RankTwoStagesPLWd)

vg <- vc$varcomp['genotype','component']

rank_yield_h2 <- vg/(vg+var_error_log_worth)

phenotipic_cor <- with(consolidated,cor(Mean,estimate))

h2_onstation <- 0.59

phenotipic_cor/sqrt(rank_yield_h2*h2_onstation)


g2 <- ggplot(consolidated,aes(x=Mean,y=estimate))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+#ylim(c(0.5,1.2))+
  ylab("PL Log(worth) of farmer rankings of yield")+xlab("Genotypic means of on-station yield (t/ha)")+
  theme_bw()


#

ggsave("Results/Correlation_data_analysis/Correlation_choice_replant_genotypic_means.png",g2,height = 5,width = 6)

consolidated2 <- on_farm_means[on_farm_means$Zone=='South West',] %>%  inner_join(onstation_means[onstation_means$zone== 'South West' & onstation_means$year==2019,],by=c('genotype'='genotype' ))

g3 <- ggplot(consolidated2,aes(x=Mean.y,y=Mean.x))+geom_point()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
    
  )+  geom_smooth(method = 'lm') + 
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+#ylim(c(0.5,1.2))+
  xlab("Genotypic means of on-station yield (t/ha)")+ylab("Genotypic means of on-farm yield (t/ha)")+
  theme_bw()

0.33/sqrt(0.48*0.48)


ggsave("Results/Correlation_data_analysis/Correlation_on_station_farm_means.png",g3,height = 5,width = 6)
