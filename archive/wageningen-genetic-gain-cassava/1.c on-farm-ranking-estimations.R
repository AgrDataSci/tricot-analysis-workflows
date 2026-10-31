
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
library(ggpmisc)

setwd(here())

rm(list=ls())

source('Scripts/0.anova.PL.R')

################################################################################
##########################            TRICOT  ##################################

setwd('Data_analysis_tricots/Cassava/Cassava_feb_2024')

cassava_data <- read.csv('Processed_data/on-farm-data-cassava-nigeria.csv')

zone_state <- unique(read.csv('Processed_data/cassava_metric_dataset_post_AE.csv')[c("state","Zone")])

cassava_data <- cassava_data %>% left_join(zone_state,by=c('state'))

## Rankings analysis

# full dataset fresh yield------------------------------------------------------

cassava_data_tricot <- cassava_data[which(!is.na(cassava_data$rank_freshrootsyield) & cassava_data$rank_freshrootsyield %in% c(1,2,3)),]

##remove farms with missing plots

cassava_data_tricot$farm <- as.character(cassava_data_tricot$farm)

tap<-with(cassava_data_tricot,tapply(plot, farm,function(x) length(unique(x))))
tap<-tap[tap==3]
cassava_data_tricot <- cassava_data_tricot[cassava_data_tricot$farm %in% names(tap),]


cassava_data_tricot$genotype <- as.character(cassava_data_tricot$genotype)

cassava_data_tricot <- cassava_data_tricot %>% mutate(ID=as.integer(as.numeric(factor(farm))))

cassava_data_tricot2 <- cassava_data_tricot[c("farm","genotype","rank_freshrootsyield")]

length(unique(cassava_data_tricot2$farm))

tricot.rank.data <-rank_numeric(data = cassava_data_tricot2,
                                items = "genotype",
                                input = "rank_freshrootsyield", 
                                id = "farm",ascending = FALSE)

PL <- PlackettLuce(tricot.rank.data)

coef.pl <- summary(PL)$coefficients

coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)

names(coef.pl) <- c("genotype","logworth", "SE","z.value","P_value")

write.csv(coef.pl,"Results/Datasets/on_farm_ranks_fresh_yield_log_worth.csv",row.names = F)

#replanting ---------------------------------------------------------------

cassava_data_tricot <- cassava_data[which(!is.na(cassava_data$rank_finalchoicereplant) & cassava_data$rank_finalchoicereplant %in% c(1,2,3)),]

##remove farms with missing plots

cassava_data_tricot$farm <- as.character(cassava_data_tricot$farm)

tap<-with(cassava_data_tricot,tapply(plot, farm,function(x) length(unique(x))))
tap<-tap[tap==3]
cassava_data_tricot <- cassava_data_tricot[cassava_data_tricot$farm %in% names(tap),]


cassava_data_tricot$genotype <- as.character(cassava_data_tricot$genotype)

cassava_data_tricot <- cassava_data_tricot %>% mutate(ID=as.integer(as.numeric(factor(farm))))

cassava_data_tricot2 <- cassava_data_tricot[c("farm","genotype","rank_finalchoicereplant")]

length(unique(cassava_data_tricot2$farm))

tricot.rank.data <-rank_numeric(data = cassava_data_tricot2,
                                items = "genotype",
                                input = "rank_finalchoicereplant", 
                                id = "farm",ascending = FALSE)

PL <- PlackettLuce(tricot.rank.data)

coef.pl <- summary(PL)$coefficients

coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)

names(coef.pl) <- c("genotype","logworth", "SE","z.value","P_value")

write.csv(coef.pl,"Results/Datasets/on_farm_ranks_finalchoicereplant_log_worth.csv",row.names = F)

#rank_harvestoverallimpression---------------------------------------------------------------

cassava_data_tricot <- cassava_data[which(!is.na(cassava_data$rank_harvestoverallimpression) & cassava_data$rank_harvestoverallimpression %in% c(1,2,3)),]

##remove farms with missing plots

cassava_data_tricot$farm <- as.character(cassava_data_tricot$farm)

tap<-with(cassava_data_tricot,tapply(plot, farm,function(x) length(unique(x))))
tap<-tap[tap==3]
cassava_data_tricot <- cassava_data_tricot[cassava_data_tricot$farm %in% names(tap),]


cassava_data_tricot$genotype <- as.character(cassava_data_tricot$genotype)

cassava_data_tricot <- cassava_data_tricot %>% mutate(ID=as.integer(as.numeric(factor(farm))))


cassava_data_tricot2 <- cassava_data_tricot[c("farm","genotype","rank_harvestoverallimpression")]

length(unique(cassava_data_tricot2$farm))

tricot.rank.data <-rank_numeric(data = cassava_data_tricot2,
                                items = "genotype",
                                input = "rank_harvestoverallimpression", 
                                id = "farm",ascending = FALSE)

PL <- PlackettLuce(tricot.rank.data)

coef.pl <- summary(PL)$coefficients

coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)

names(coef.pl) <- c("genotype","logworth", "SE","z.value","P_value")

write.csv(coef.pl,"Results/Datasets/on_farm_ranks_harvestoverallimpression_log_worth.csv",row.names = F)

#-------------------------------------------------------------------------------
# by location
#-------------------------------------------------------------------------------


# The model trained contain the same values in ranking and PL, its no comparable with script b
##----------------------------------------------------------------------------
## Fresh yield by Location

cassava_data_tricot <- cassava_data[which(!is.na(cassava_data$rank_freshrootsyield) & cassava_data$rank_freshrootsyield %in% c(1,2,3)),]

##remove farms with missing plots

cassava_data_tricot$farm <- as.character(cassava_data_tricot$farm)

tap<-with(cassava_data_tricot,tapply(plot, farm,function(x) length(unique(x))))
tap<-tap[tap==3]
cassava_data_tricot <- cassava_data_tricot[cassava_data_tricot$farm %in% names(tap),]


cassava_data_tricot$genotype <- as.character(cassava_data_tricot$genotype)

cassava_data_tricot <- cassava_data_tricot %>% mutate(ID=as.integer(as.numeric(factor(farm))))

ls_cassava_data_tricot <- split(cassava_data_tricot,paste(cassava_data_tricot$Zone,sep="_"))

log_worth_location <- lapply(ls_cassava_data_tricot,function(x){
  cassava_data_tricot2 <- x[c("farm","genotype","rank_freshrootsyield")]
  
  length(unique(cassava_data_tricot2$farm))
  
  tricot.rank.data <-rank_numeric(data = cassava_data_tricot2,
                                  items = "genotype",
                                  input = "rank_freshrootsyield", 
                                  id = "farm",ascending = FALSE)
  
  PL <- PlackettLuce(tricot.rank.data)
  
  #anv <- anova.PL(PL)
  
  coef.pl <- summary(PL)$coefficients
  
  coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)
  
  names(coef.pl) <- c("genotype","logworth", "SE","z.value","P_value")
  
  coef.pl <- data.frame(Zone=unique(x$Zone),genotype=row.names(coef.pl), coef.pl = coef.pl)
  coef.pl
}
)

log_worth_loc <- do.call(rbind,log_worth_location)

write.csv(log_worth_loc,"Results/Datasets/on_farm_ranks_fresh_yield_location_log_worth.csv",row.names = F)

#------------------------------------------------------------------------------
## Choice for replating ranks by LOCATION-------------------------------------

cassava_data_tricot <- cassava_data[which(!is.na(cassava_data$rank_finalchoicereplant) & cassava_data$rank_finalchoicereplant %in% c(1,2,3)),]

##remove farms with missing plots

cassava_data_tricot$farm <- as.character(cassava_data_tricot$farm)

tap<-with(cassava_data_tricot,tapply(plot, farm,function(x) length(unique(x))))
tap<-tap[tap==3]
cassava_data_tricot <- cassava_data_tricot[cassava_data_tricot$farm %in% names(tap),]


cassava_data_tricot$genotype <- as.character(cassava_data_tricot$genotype)

cassava_data_tricot <- cassava_data_tricot %>% mutate(ID=as.integer(as.numeric(factor(farm))))

ls_cassava_data_tricot <- split(cassava_data_tricot,cassava_data_tricot$Zone)

log_worth_location <- lapply(ls_cassava_data_tricot,function(x){
  cassava_data_tricot2 <- x[c("farm","genotype","rank_finalchoicereplant")]
  
  length(unique(cassava_data_tricot2$farm))
  
  tricot.rank.data <-rank_numeric(data = cassava_data_tricot2,
                                  items = "genotype",
                                  input = "rank_finalchoicereplant", 
                                  id = "farm",ascending = FALSE)
  
  PL <- PlackettLuce(tricot.rank.data)
  
  coef.pl <- summary(PL)$coefficients
  
  coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)
  
  names(coef.pl) <- c("genotype","logworth", "SE","z.value","P_value")
  
  coef.pl <- data.frame(Zone=unique(x$Zone),genotype=row.names(coef.pl), coef.pl = coef.pl)
  coef.pl
}
)

log_worth_loc <- do.call(rbind,log_worth_location)

write.csv(log_worth_loc,"Results/Datasets/on_farm_ranks_finalchoicereplant_location_log_worth.csv",row.names = F)

#------------------------------------------------------------------------------
## overall performance ranks by LOCATION-------------------------------------

cassava_data_tricot <- cassava_data[which(!is.na(cassava_data$rank_harvestoverallimpression) & cassava_data$rank_harvestoverallimpression %in% c(1,2,3)),]

##remove farms with missing plots

cassava_data_tricot$farm <- as.character(cassava_data_tricot$farm)

tap<-with(cassava_data_tricot,tapply(plot, farm,function(x) length(unique(x))))
tap<-tap[tap==3]
cassava_data_tricot <- cassava_data_tricot[cassava_data_tricot$farm %in% names(tap),]


cassava_data_tricot$genotype <- as.character(cassava_data_tricot$genotype)

cassava_data_tricot <- cassava_data_tricot %>% mutate(ID=as.integer(as.numeric(factor(farm))))

ls_cassava_data_tricot <- split(cassava_data_tricot,cassava_data_tricot$Zone)

log_worth_location <- lapply(ls_cassava_data_tricot,function(x){
  cassava_data_tricot2 <- x[c("farm","genotype","rank_harvestoverallimpression")]
  
  length(unique(cassava_data_tricot2$farm))
  
  tricot.rank.data <-rank_numeric(data = cassava_data_tricot2,
                                  items = "genotype",
                                  input = "rank_harvestoverallimpression", 
                                  id = "farm",ascending = FALSE)
  
  PL <- PlackettLuce(tricot.rank.data)
  
  coef.pl <- summary(PL)$coefficients
  names(coef.pl) <- c("genotype","logworth", "SE","z.value","P_value")
  coef.pl <- data.frame(Zone=unique(x$Zone),genotype=row.names(coef.pl), coef.pl = coef.pl)
  coef.pl
}
)

log_worth_loc <- do.call(rbind,log_worth_location)

write.csv(log_worth_loc,"Results/Datasets/on_farm_ranks_harvestoverallimpression_location_log_worth.csv",row.names = F)

###-----------------------------------------------------------------------------
# Fresh yield  reported---------------------------------------------------------

# Simulataneusly ranking + yield

cassava_data_tricot <- cassava_data[which(!is.na(cassava_data$rank_freshrootsyield) & cassava_data$rank_freshrootsyield %in% c(1,2,3)& !is.na(cassava_data$yieldton_ha)),]

##remove farms with missing plots

cassava_data_tricot$farm <- as.character(cassava_data_tricot$farm)

tap<-with(cassava_data_tricot,tapply(plot, farm,function(x) length(unique(x))))
tap<-tap[tap==3]
cassava_data_tricot <- cassava_data_tricot[cassava_data_tricot$farm %in% names(tap),]


cassava_data_tricot$genotype <- as.character(cassava_data_tricot$genotype)

cassava_data_tricot <- cassava_data_tricot %>% mutate(ID=as.integer(as.numeric(factor(farm))))

cassava_data_tricot2 <- cassava_data_tricot[c("farm","genotype","rank_freshrootsyield")]

length(unique(cassava_data_tricot2$farm))

tricot.rank.data <-rank_numeric(data = cassava_data_tricot2,
                                items = "genotype",
                                input = "rank_freshrootsyield", 
                                id = "farm",ascending = FALSE)

PL <- PlackettLuce(tricot.rank.data)

coef.pl <- summary(PL)$coefficients

coef.pl <- data.frame(genotype=row.names(coef.pl), coef.pl = coef.pl)

write.csv(coef.pl,"Results/cassava_nigeria/datasets/log_worth_rank_freshyield_(rank_yield_same).csv")

#------------------------------------------------------------------------------

## Choice for replating ranks by LOCATION year-------------------------------------

cassava_data_tricot <- cassava_data[which(!is.na(cassava_data$rank_finalchoicereplant) & cassava_data$rank_finalchoicereplant %in% c(1,2,3)),]

##remove farms with missing plots

cassava_data_tricot$farm <- as.character(cassava_data_tricot$farm)

tap<-with(cassava_data_tricot,tapply(plot, farm,function(x) length(unique(x))))
tap<-tap[tap==3]
cassava_data_tricot <- cassava_data_tricot[cassava_data_tricot$farm %in% names(tap),]


cassava_data_tricot$genotype <- as.character(cassava_data_tricot$genotype)

cassava_data_tricot <- cassava_data_tricot %>% mutate(ID=as.integer(as.numeric(factor(farm))))

ls_cassava_data_tricot <- split(cassava_data_tricot,paste(cassava_data_tricot$Zone,cassava_data_tricot$year,sep='-'))

log_worth_location <- lapply(ls_cassava_data_tricot,function(x){
  cassava_data_tricot2 <- x[c("farm","genotype","rank_finalchoicereplant")]
  
  length(unique(cassava_data_tricot2$farm))
  
  tricot.rank.data <-rank_numeric(data = cassava_data_tricot2,
                                  items = "genotype",
                                  input = "rank_finalchoicereplant", 
                                  id = "farm",ascending = FALSE)
  
  PL <- PlackettLuce(tricot.rank.data)
  
  print(paste(unique(x$year),'-',Zone=unique(x$Zone)))
  print(anova.PL(PL))
  
  coef.pl <- data.frame(qvcalc(PL)$qvframe,year=unique(x$year),Zone=unique(x$Zone))
  
  coef.pl <- cbind(genotype=row.names(coef.pl), coef.pl)
  
  #names(coef.pl) <- c("genotype","logworth", "SE","z.value","P_value")
  
  #coef.pl <- data.frame(Zone=unique(x$Zone),genotype=row.names(coef.pl), coef.pl = coef.pl,year=unique(x$year))
  coef.pl
}
)

log_worth_loc <- do.call(rbind,log_worth_location)

write.csv(log_worth_loc,"Results/Datasets/on_farm_ranks_finalchoicereplant_location_log_worth_qv.csv",row.names = F)

# On - farm rankings



