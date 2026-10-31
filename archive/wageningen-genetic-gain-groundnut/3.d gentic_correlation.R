
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

setwd("C:/Users/dorad002/R_Projects/Data_analysis_tricots/Groundnut/Groundnut_Feb_2024")

on_farm_means     <- read.csv("Results/Datasets/on_farm_yield_genotypic_means.csv")

on_station_means <- read.csv("Results/Datasets/on_station_yield_genotypic_means.csv")

on_farm_log_worth <- read.csv("Results/Datasets/log_worths_rank_yield_qv.csv")

on_farm_log_worth_yield <- read.csv("Results/Datasets/log_worths_rank_yield_qv.csv")

on_farm_log_worth_ovrll <- read.csv("Results/Datasets/log_worths_rank_overall_qv.csv")

on_farm_log_worth_mark <-read.csv("Results/Datasets/log_worths_rank_marketabilty_qv.csv")

on_farm_means <- read.csv('Results/Datasets/on_farm_fresh_yield_location_genotypic_means.csv')

h2_onstation <- 0.09

location_year_overall <- read.csv("Results/Datasets/log_worth_overall_location_qv.csv")

location_year_overall <- read.csv("Results/Datasets/log_worth_overall_location_qv.csv")

location_year_onstation <- read.csv('Results/Datasets/on_station_location_yield_genotypic_means.csv')

table(location_year_overall$Zone,location_year_overall$year)

central_2021_ranks <- location_year_overall[location_year_overall$Zone=="Southern" & location_year_overall$year==2023,]

central_2021_onstation <- location_year_onstation[location_year_onstation$Location=="Southern"&location_year_onstation$year==2023,]

interceps_gent <- central_2021_ranks$genotype[central_2021_ranks$genotype %in% central_2021_onstation$genotype]

central_2021_ranks <- central_2021_ranks[central_2021_ranks$genotype %in% interceps_gent,]

central_2021_onstation <- central_2021_onstation[central_2021_onstation$genotype %in% interceps_gent,]

# overall

central_2021_ranks$weight_pl <-   1/central_2021_ranks$quasiVar

central_2021_ranks$genotype <- factor(central_2021_ranks$genotype)

var_error_log_worth <- mean(central_2021_ranks$quasiVar)

RankTwoStagesPLWd  <- asreml(fixed = estimate ~ 1,
                             random = ~  genotype,
                             weights = weight_pl,
                             family = (asr_gaussian(dispersion = 1)),
                             data = central_2021_ranks)

vc <- summary(RankTwoStagesPLWd)

vg <- vc$varcomp['genotype','component']

rank_yield_h2 <- vg/(vg+var_error_log_worth)

station_ranks <- inner_join(central_2021_onstation[c('genotype','Mean')],central_2021_ranks[c('genotype','estimate')],)

phenotipic_cor <- with(station_ranks,cor(Mean,estimate))

phenotipic_cor/sqrt(rank_yield_h2*h2_onstation)

# overall

on_farm_log_worth_ovrll$weight_pl <-   1/on_farm_log_worth_ovrll$quasiVar

on_farm_log_worth_ovrll$genotype <- factor(on_farm_log_worth_ovrll$genotype)

sigma_error_log_worth <- mean(sqrt(on_farm_log_worth_ovrll$quasiVar))*sqrt(unique(on_farm_log_worth_ovrll$nrep))

RankTwoStagesPLWd  <- asreml(fixed = estimate ~ 1,
                             random = ~  genotype,
                             weights = weight_pl,
                             family = (asr_gaussian(dispersion = 1)),
                             data = on_farm_log_worth_ovrll)

vc <- summary(RankTwoStagesPLWd)

vg <- vc$varcomp['genotype','component']

rank_yield_h2 <- vg/(vg+sigma_error_log_worth^2/unique(on_farm_log_worth_ovrll$nrep))

station_ranks <- inner_join(on_station_means[c('genotype','Mean')],on_farm_log_worth_ovrll[c('genotype','estimate')],)

phenotipic_cor <- with(station_ranks,cor(Mean,estimate))

phenotipic_cor/sqrt(rank_yield_h2*h2_onstation)


# marketability

on_farm_log_worth_mark$weight_pl <-   1/on_farm_log_worth_mark$quasiVar

on_farm_log_worth_mark$genotype <- factor(on_farm_log_worth_mark$genotype)

sigma_error_log_worth <- mean(sqrt(on_farm_log_worth_mark$quasiVar))*sqrt(unique(on_farm_log_worth_mark$nrep))

RankTwoStagesPLWd  <- asreml(fixed = estimate ~ 1,
                             random = ~  genotype,
                             weights = weight_pl,
                             family = (asr_gaussian(dispersion = 1)),
                             data = on_farm_log_worth_mark)

vc <- summary(RankTwoStagesPLWd)

vg <- vc$varcomp['genotype','component']

rank_yield_h2 <- vg/(vg+sigma_error_log_worth^2/unique(on_farm_log_worth_mark$nrep))

station_ranks <- inner_join(on_station_means[c('genotype','Mean')],on_farm_log_worth_mark[c('genotype','estimate')],)

phenotipic_cor <- with(station_ranks,cor(Mean,estimate))

phenotipic_cor/sqrt(rank_yield_h2*h2_onstation)

g2 <- ggplot(consolidated,aes(x=Mean,y=estimate))+geom_point()+
     geom_text_repel(
         aes(label=genotype),
         box.padding = unit(0.2, "lines"),size=2,max.overlaps = 25
         
         )+  geom_smooth(method = 'lm') + 
     stat_poly_eq(
         aes(label =  paste(..eq.label.., ..rr.label.., 'rho','`=`',round(sqrt(r.squared),2),sep = "~~~~")),formula = y ~ x,
         parse = TRUE)+geom_smooth(method = "lm",se=F,alpha=0.2)+ylim(c(-4,1))+#ylim(c(0.5,1.2))+
     ylab("PL Log(worth) of farmer rankings of choice to replant")+xlab("Genotypic means of on-station yield (t/ha)")+
     theme_bw()


ggsave("Results/Correlation_data_analysis/Correlation_choice_replant_genotypic_means.png",g2,height = 5,width = 6)



