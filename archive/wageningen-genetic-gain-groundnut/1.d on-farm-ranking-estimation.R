
# Data analysis metric-rankings - cassava
# December 2023
# Ranking data analysis

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


################################################################################
##########################            TRICOT  ##################################

setwd('Data_analysis_tricots/Groundnut/Groundnut_Feb_2024')

groundnut_data <- read.csv("Processed_data/on-farm-data-groundnut-tanzania.csv")


## Rankings analysis

#  yield------------------------------------------------------------------------

groundnut_data_yld <- groundnut_data[which(!is.na(groundnut_data$rank_yield) & groundnut_data$rank_yield %in% c(1,2,3)),]

length(unique(groundnut_data_yld$year))

length(unique(groundnut_data_yld$genotype))

length(unique(groundnut_data_yld$state))

length(unique(groundnut_data_yld$farm))

length(groundnut_data_yld$farm)

##remove farms with missing plots

groundnut_data_yld$farm <- as.character(groundnut_data_yld$farm)

tap<-with(groundnut_data_yld,tapply(plot, farm,function(x) length(unique(x))))

tap<-tap[tap==3]

groundnut_data_yld <- groundnut_data_yld[groundnut_data_yld$farm %in% names(tap),]

groundnut_data_yld$genotype <- as.character(groundnut_data_yld$genotype)

groundnut_data_yld_tricot <- groundnut_data_yld %>% mutate(ID=as.integer(as.numeric(factor(farm))))

groundnut_data_yld_tricot2 <- groundnut_data_yld_tricot[c("farm","genotype","rank_yield")]

length(unique(groundnut_data_yld_tricot2$farm))

tricot.rank.data <-rank_numeric(data = groundnut_data_yld_tricot2,
                                items = "genotype",
                                input = "rank_yield", 
                                id = "farm",ascending = FALSE)

PL <- PlackettLuce(tricot.rank.data)

coef.pl <- summary(PL)$coefficients

coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)

names(coef.pl) <- c('genotype','coef_estimate','coef_std_error','coef_zvalue','coef_pl')

write.csv(coef.pl,"Results/Datasets/log_worths_rank_yield.csv",row.names = F)

qv <- qvcalc(PL)$qvframe

qv <- cbind(genotype=row.names(qv),qv,nrep=round(mean(table(groundnut_data_yld_tricot2$genotype))))

write.csv(qv,"Results/Datasets/log_worths_rank_yield_qv.csv",row.names = F)

#  overall ---------------------------------------------------------------------

groundnut_data_ovrll <- groundnut_data[which(!is.na(groundnut_data$rank_overall) & groundnut_data$rank_overall %in% c(1,2,3)),]

##remove farms with missing plots

groundnut_data_ovrll$farm <- as.character(groundnut_data_ovrll$farm)

tap<-with(groundnut_data_ovrll,tapply(plot, farm,function(x) length(unique(x))))

tap<-tap[tap==3]

groundnut_data_ovrll <- groundnut_data_ovrll[groundnut_data_ovrll$farm %in% names(tap),]

groundnut_data_ovrll$genotype <- as.character(groundnut_data_ovrll$genotype)

groundnut_data_ovrll_tricot <- groundnut_data_ovrll %>% mutate(ID=as.integer(as.numeric(factor(farm))))

groundnut_data_ovrll_tricot2 <- groundnut_data_ovrll_tricot[c("farm","genotype","rank_overall")]

length(unique(groundnut_data_ovrll_tricot2$farm))

tricot.rank.data <-rank_numeric(data = groundnut_data_ovrll_tricot2,
                                items = "genotype",
                                input = "rank_overall", 
                                id = "farm",ascending = FALSE)

PL <- PlackettLuce(tricot.rank.data)

coef.pl <- summary(PL)$coefficients

coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)

names(coef.pl) <- c('genotype','coef_estimate','coef_std_error','coef_zvalue','coef_pl')

write.csv(coef.pl,"Results/Datasets/log_worths_rank_overall.csv",row.names = F)

qv <- qvcalc(PL)$qvframe

qv <- cbind(genotype=row.names(qv),qv,nrep=round(mean(table(groundnut_data_ovrll_tricot2$genotype))))

write.csv(qv,"Results/Datasets/log_worths_rank_overall_qv.csv",row.names = F)


##remove farms with missing plots

#  marketable-------------------------------------------------------------------

groundnut_data_markt <- groundnut_data[which(!is.na(groundnut_data$rank_marketability) & groundnut_data$rank_marketability %in% c(1,2,3)),]

##remove farms with missing plots

groundnut_data_markt$farm <- as.character(groundnut_data_markt$farm)

tap<-with(groundnut_data_markt,tapply(plot, farm,function(x) length(unique(x))))

tap<-tap[tap==3]

groundnut_data_markt <- groundnut_data_markt[groundnut_data_markt$farm %in% names(tap),]

groundnut_data_markt$genotype <- as.character(groundnut_data_markt$genotype)

groundnut_data_markt_tricot <- groundnut_data_markt %>% mutate(ID=as.integer(as.numeric(factor(farm))))

groundnut_data_markt_tricot2 <- groundnut_data_markt[c("farm","genotype","rank_marketability")]

length(unique(groundnut_data_markt_tricot2$farm))

tricot.rank.data <-rank_numeric(data = groundnut_data_markt_tricot2,
                                items = "genotype",
                                input = "rank_marketability", 
                                id = "farm",ascending = FALSE)

PL <- PlackettLuce(tricot.rank.data)

coef.pl <- summary(PL)$coefficients

coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)

names(coef.pl) <- c('genotype','coef_estimate','coef_std_error','coef_zvalue','coef_pl')

write.csv(coef.pl,"Results/Datasets/log_worths_rank_marketabilty.csv",row.names = F)

qv <- qvcalc(PL)$qvframe

qv <- cbind(genotype=row.names(qv),qv,nrep=round(mean(table(groundnut_data_markt_tricot2$genotype))))

write.csv(qv,"Results/Datasets/log_worths_rank_marketabilty_qv.csv",row.names = F)

#-----------------------------------------------------------------------------
###                                  Location

groundnut_data_yld <- groundnut_data[which(!is.na(groundnut_data$rank_yield) & groundnut_data$rank_yield %in% c(1,2,3)),]

groundnut_data_yld <- groundnut_data_yld[!is.na(groundnut_data_yld$ecoregion),]

groundnut_data_yld$farm <- as.character(groundnut_data_yld$farm)

tap<-with(groundnut_data_yld,tapply(plot, farm,function(x) length(unique(x))))

tap<-tap[tap==3]

groundnut_data_yld <- groundnut_data_yld[groundnut_data_yld$farm %in% names(tap),]

groundnut_data_yld$genotype <- as.character(groundnut_data_yld$genotype)

groundnut_data_yld_tricot <- groundnut_data_yld %>% mutate(ID=as.integer(as.numeric(factor(farm))))

ls_groundnut_data_yld_tricot <- split(groundnut_data_yld_tricot,groundnut_data_yld_tricot$ecoregion)

#ls_groundnut_data_yld_tricot <- ls_groundnut_data_yld_tricot[-2]

log_worth_location <- lapply(ls_groundnut_data_yld_tricot,function(x){
  
  
  groundnut_data_yld_tricot2 <- x[c("farm","genotype","rank_yield")]
  
  
  tricot.rank.data <-rank_numeric(data = groundnut_data_yld_tricot2,
                                  items = "genotype",
                                  input = "rank_yield", 
                                  id = "farm",ascending = FALSE)
  
  PL <- PlackettLuce(tricot.rank.data)
  
  coef.pl <- coef(PL)
  
  #coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)
  
  #names(coef.pl) <- c('genotype','coef_estimate','coef_std_error','coef_zvalue','coef_pl')
  
  coef.pl <- data.frame( genotype=names(coef.pl),coef_estimate = coef(PL),Zone=unique(x$ecoregion))
  coef.pl
}
)

log_worth_loc <- do.call(rbind,log_worth_location)

write.csv(log_worth_loc,"Results/Datasets/log_worth_yield_location.csv",row.names = F)

#-----


groundnut_data_yld <- groundnut_data[which(!is.na(groundnut_data$rank_marketability) & groundnut_data$rank_marketability %in% c(1,2,3)),]

groundnut_data_yld <- groundnut_data_yld[!is.na(groundnut_data_yld$ecoregion),]

groundnut_data_yld$farm <- as.character(groundnut_data_yld$farm)

tap<-with(groundnut_data_yld,tapply(plot, farm,function(x) length(unique(x))))

tap<-tap[tap==3]

groundnut_data_yld <- groundnut_data_yld[groundnut_data_yld$farm %in% names(tap),]

groundnut_data_yld$genotype <- as.character(groundnut_data_yld$genotype)

groundnut_data_yld_tricot <- groundnut_data_yld %>% mutate(ID=as.integer(as.numeric(factor(farm))))

ls_groundnut_data_yld_tricot <- split(groundnut_data_yld_tricot,groundnut_data_yld_tricot$ecoregion)

#ls_groundnut_data_yld_tricot <- ls_groundnut_data_yld_tricot[-2]

log_worth_location <- lapply(ls_groundnut_data_yld_tricot,function(x){
  
  
  groundnut_data_yld_tricot2 <- x[c("farm","genotype","rank_marketability")]
  
  
  tricot.rank.data <-rank_numeric(data = groundnut_data_yld_tricot2,
                                  items = "genotype",
                                  input = "rank_marketability", 
                                  id = "farm",ascending = FALSE)
  
  PL <- PlackettLuce(tricot.rank.data)
  
  coef.pl <- coef(PL)
  
  #coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)
  
  #names(coef.pl) <- c('genotype','coef_estimate','coef_std_error','coef_zvalue','coef_pl')
  
  coef.pl <- data.frame( genotype=names(coef.pl),coef_estimate = coef(PL),Zone=unique(x$ecoregion))
  coef.pl
}
)

log_worth_loc <- do.call(rbind,log_worth_location)

write.csv(log_worth_loc,"Results/Datasets/log_worth_marketability_location.csv",row.names = F)

#-------------------------------------------------------------------------------
# overall

groundnut_data_yld <- groundnut_data[which(!is.na(groundnut_data$rank_overall) & groundnut_data$rank_overall %in% c(1,2,3)),]

groundnut_data_yld <- groundnut_data_yld[!is.na(groundnut_data_yld$ecoregion),]

groundnut_data_yld$farm <- as.character(groundnut_data_yld$farm)

tap<-with(groundnut_data_yld,tapply(plot, farm,function(x) length(unique(x))))

tap<-tap[tap==3]

groundnut_data_yld <- groundnut_data_yld[groundnut_data_yld$farm %in% names(tap),]

groundnut_data_yld$genotype <- as.character(groundnut_data_yld$genotype)

groundnut_data_yld_tricot <- groundnut_data_yld %>% mutate(ID=as.integer(as.numeric(factor(farm))))

ls_groundnut_data_yld_tricot <- split(groundnut_data_yld_tricot,groundnut_data_yld_tricot$ecoregion)

#ls_groundnut_data_yld_tricot <- ls_groundnut_data_yld_tricot[-c(2,3,6)]


log_worth_location <- lapply(ls_groundnut_data_yld_tricot,function(x){
  
  
  groundnut_data_yld_tricot2 <- x[c("farm","genotype","rank_overall")]
  
  
  tricot.rank.data <-rank_numeric(data = groundnut_data_yld_tricot2,
                                  items = "genotype",
                                  input = "rank_overall", 
                                  id = "farm",ascending = FALSE)
  
  PL <- PlackettLuce(tricot.rank.data)
  
  
  coef.pl <- coef(PL)
  
  #coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)
  
  #names(coef.pl) <- c('genotype','coef_estimate','coef_std_error','coef_zvalue','coef_pl')
  
  coef.pl <- data.frame( genotype=names(coef.pl),coef_estimate = coef(PL),Zone=unique(x$ecoregion))
  coef.pl
}
)

log_worth_loc <- do.call(rbind,log_worth_location)

write.csv(log_worth_loc,"Results/Datasets/log_worth_overall_location.csv",row.names = F)

# Location vs year-------------------------------------------------------------

# overall

groundnut_data_yld <- groundnut_data[which(!is.na(groundnut_data$rank_overall) & groundnut_data$rank_overall %in% c(1,2,3)),]

groundnut_data_yld <- groundnut_data_yld[!is.na(groundnut_data_yld$ecoregion),]

groundnut_data_yld$farm <- as.character(groundnut_data_yld$farm)

tap<-with(groundnut_data_yld,tapply(plot, farm,function(x) length(unique(x))))

tap<-tap[tap==3]

groundnut_data_yld <- groundnut_data_yld[groundnut_data_yld$farm %in% names(tap),]

groundnut_data_yld$genotype <- as.character(groundnut_data_yld$genotype)

groundnut_data_yld_tricot <- groundnut_data_yld %>% mutate(ID=as.integer(as.numeric(factor(farm))))

ls_groundnut_data_yld_tricot <- split(groundnut_data_yld_tricot,paste0(groundnut_data_yld_tricot$ecoregion,"-",groundnut_data_yld_tricot$year))

#ls_groundnut_data_yld_tricot <- ls_groundnut_data_yld_tricot[-c(2,3,6)]


log_worth_location <- lapply(ls_groundnut_data_yld_tricot[c(1,2,3,4,8,11,12,14,15,16,17,20)],function(x){
  
  
  groundnut_data_yld_tricot2 <- x[c("farm","genotype","rank_overall")]
  
  
  tricot.rank.data <-rank_numeric(data = groundnut_data_yld_tricot2,
                                  items = "genotype",
                                  input = "rank_overall", 
                                  id = "farm",ascending = FALSE)
  
  PL <- PlackettLuce(tricot.rank.data)
  
  
  qv <- qvcalc(PL)$qvframe
  
  #coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)
  
  #names(coef.pl) <- c('genotype','coef_estimate','coef_std_error','coef_zvalue','coef_pl')
  
  coef.pl <- cbind( genotype=row.names(qv), qv,Zone=unique(x$ecoregion),year=unique(x$year),n=nrow(groundnut_data_yld_tricot2),nrep=round(mean(table(groundnut_data_yld_tricot2$genotype))))
  coef.pl
}
)

log_worth_loc <- do.call(rbind,log_worth_location)

write.csv(log_worth_loc,"Results/Datasets/log_worth_overall_location_qv.csv",row.names = F)





