
library(nlme)
library(ggplot2)
library(tidyr)
library(predictmeans)
library(tidyverse)
library(leaflet)

rm(list = ls())

setwd("Data_analysis_tricots/Cassava/Cassava_feb_2024")

# Read data

# longitude,latitude,

farms_data0 <- read.csv("Data/tricot-data-nextgen-cassava.csv",stringsAsFactors = T)

names(farms_data0)[1] <- "id"

#--Question for kaue: where can I find the planting dates?, which is the correct parameter for yield?

metric_vars <- c('id','varietya','varietyb','varietyc','rootweight_plot_a',
'rootweight_plot_b','rootweight_plot_c','rootweight_plot_local','rootweight_marketable_roots_a',
'rootweight_marketable_roots_b','rootweight_marketable_roots_c')

ranking_vars <- c('id',
'harvesting_fresh_roots_harvesting_freshrootsyield_best',
'harvesting_fresh_roots_harvesting_freshrootsyield_worst',
'final_evaluation_finalevaluationoverallimpression_best',
'final_evaluation_finalevaluationoverallimpression_worst',
'final_evaluation_choiceforreplant_best',
'final_evaluation_choiceforreplant_worst',
'harvesting_fresh_roots_overallimpression_best',
'harvesting_fresh_roots_overallimpression_worst')

across_vars <- c("id","longitude","latitude","state","year.y","year.x")

# Extract and process variables

metric_data <- farms_data0[metric_vars]

names(metric_data) <- gsub("rootweight_plot","rootweight",names(metric_data))

names(metric_data) <- gsub("rootweight_marketable_roots","rootweightmarketable",names(metric_data))

names(metric_data) <- gsub("variety","genotype_",names(metric_data))

long_farm_data <-metric_data %>% 
  pivot_longer(
    cols = !id, 
    names_to = c(".value", "plot"), 
    names_sep = "_", 
    values_drop_na = F
  )

# Is it the local always the same or should be changed by location?

# Rank data processing

ranks_farm_data <- farms_data0[ranking_vars]

names(ranks_farm_data) <- gsub("harvesting_fresh_roots_harvesting_freshrootsyield","freshrootsyield",names(ranks_farm_data))

names(ranks_farm_data) <- gsub("final_evaluation_finalevaluationoverallimpression","finaloverallimpression",names(ranks_farm_data))

names(ranks_farm_data) <- gsub("final_evaluation_choiceforreplant","finalchoicereplant",names(ranks_farm_data))

names(ranks_farm_data) <- gsub("harvesting_fresh_roots_overallimpression","harvestoverallimpression",names(ranks_farm_data))


index <- grep("best|worst",names(ranks_farm_data))

options <- c("A","B","C")

for(i in index){
  newvar <- ranks_farm_data[,i] 
  newvar <- as.character(ranks_farm_data[,i])
  newvar[is.na(newvar) | !(newvar %in% options) ] <- NA
  ranks_farm_data[,i] <- newvar
}


ls_class_freshrootsyield <- with(ranks_farm_data,split(data.frame(freshrootsyield_best,freshrootsyield_worst),seq(length(freshrootsyield_best))))

ls_class_finaloverallimpression <- with(ranks_farm_data,split(data.frame(finaloverallimpression_best,finaloverallimpression_worst),seq(length(finaloverallimpression_best))))

ls_class_finalchoicereplant <- with(ranks_farm_data,split(data.frame(finalchoicereplant_best,finalchoicereplant_worst),seq(length(finalchoicereplant_best))))

ls_class_harvestoverallimpression <- with(ranks_farm_data,split(data.frame(harvestoverallimpression_best,harvestoverallimpression_worst),seq(length(harvestoverallimpression_best))))


ranks_farm_data$freshrootsyield_med <- sapply(ls_class_freshrootsyield,function(w){if(sum(unique(w)%in% options)==2){options[!(options %in% w)][1]}else{ NA }}) # Duplicated cases 

ranks_farm_data$finaloverallimpression_med <- sapply(ls_class_finaloverallimpression,function(w){if(sum(unique(w)%in% options)==2){options[!(options %in% w)][1]}else{ NA }})

ranks_farm_data$finalchoicereplant_med <- sapply(ls_class_finalchoicereplant,function(w){if(sum(unique(w)%in% options)==2){options[!(options %in% w)][1]}else{ NA }})

ranks_farm_data$harvestoverallimpression_med <- sapply(ls_class_harvestoverallimpression,function(w){if(sum(unique(w)%in% options)==2){options[!(options %in% w)][1]}else{ NA }})


# Rankings to orders

ranks_farm_data <- ranks_farm_data %>% 
  pivot_longer(-id,names_to = c(".value","calification"),names_sep = "_",
               values_drop_na = F ) %>% 
  mutate(rank = str_replace_all(calification,c("best"="1","worst"="3",
                                       "med"="2"))) 

ranks_farm_data <- ranks_farm_data %>% pivot_longer(-c(id,calification,rank),names_to = "variable",values_to = "plot")

ranks_farm_data <- ranks_farm_data[!is.na(ranks_farm_data$plot),] %>% select(!calification )# check cases but I think plot never has NA

# Remove duplicated cases of ranks

ranks_farm_data <- ranks_farm_data[!duplicated(paste(ranks_farm_data$id,ranks_farm_data$variable,ranks_farm_data$plot,sep = '_')),]

table(table(paste(ranks_farm_data$id,ranks_farm_data$variable,ranks_farm_data$plot,sep = '_'))) # Should be unique

ranks_farm_data <- ranks_farm_data %>% pivot_wider(names_from =variable ,values_from = rank)

names(ranks_farm_data)[3:6] <- paste0('rank_',names(ranks_farm_data)[3:6] )

long_farm_data$plot <- toupper(long_farm_data$plot)

# Merge dataset (put all dataset together)---------

long_farm_data <- long_farm_data %>% 
  left_join(ranks_farm_data,by=c("id", "plot")) %>%
  left_join(farms_data0[across_vars],by=c("id"))# in this case, metric dataset is the same length that ranking dataset

long_farm_data$genotype <- as.character(long_farm_data$genotype )

long_farm_data$genotype[long_farm_data$plot == "LOCAL"] <- "Local"

#long_farm_data <-  long_farm_data[complete.cases(long_farm_data),]

long_farm_data <- long_farm_data[order(long_farm_data$id),]

# names(long_farm_data)[7] <- "date"

# names(long_farm_data)[6] <- "yield"


# Data cleaning---------

long_farm_data$rank_freshrootsyield <- as.numeric(long_farm_data$rank_freshrootsyield )

long_farm_data$rootweightmarketable <- as.numeric(long_farm_data$rootweightmarketable )

long_farm_data$longitude[!is.na(long_farm_data$longitude) & !is.na(long_farm_data$latitude) & long_farm_data$longitude  == 0 & long_farm_data$latitude  == 0] <- NA

long_farm_data$latitude[is.na(long_farm_data$longitude) & long_farm_data$latitude  == 0] <- NA


# %>%
  # mutate(date=as.Date(date)) %>% filter(!is.na(date))

summary(long_farm_data)


##clean data

boxplot(long_farm_data$rootweight)

long_farm_data$rootweight[which(long_farm_data$rootweight<2)] <- NA
long_farm_data$rootweight[which(long_farm_data$rootweight>200)] <- NA

boxplot(long_farm_data$rootweightmarketable)


long_farm_data$rootweightmarketable[which(long_farm_data$rootweightmarketable<2)] <- NA
long_farm_data$rootweightmarketable[which(long_farm_data$rootweightmarketable>200)] <- NA

boxplot(long_farm_data$rootweightmarketable)


long_farm_data %>% ggplot(aes(x=rootweight,y=rootweightmarketable))+geom_point()

names(long_farm_data)[1] <- "farm"

long_farm_data$yieldton_ha <- long_farm_data$rootweight*10000/(30*1000) # 1kg/30mts2*1000mts2/1ha*1ton/1000kg

long_farm_data$yieldmarkton_ha <- long_farm_data$rootweightmarketable*10000/(30*1000) # 1kg/30mts2*1000mts2/1ha*1ton/1000kg

long_farm_data <- long_farm_data[,-which(names(long_farm_data)=="year.y")]

data.frame(long_farm_data)

long_farm_data$year <- 2019

long_farm_data$year[long_farm_data$year.x==2] <- 2021 

long_farm_data <- long_farm_data[,-which(names(long_farm_data)=="year.x")]

planting_date <- NULL

# Planting dates were randomly defined by request of Kaue. Those were not used in the model estimations

for(i in 1:nrow(long_farm_data)){
  obs.v <- long_farm_data[i,]
  planting_date[i] <-
  if(obs.v$state =="Akwa Ibom"){
    if(obs.v$year == 2019){
      as.Date(sample(as.Date("2019-05-29"):as.Date("2019-06-04"),1), origin = "1970-01-01")
    }else{as.Date(sample(as.Date("2021-04-20"):as.Date("2021-06-28"),1), origin = "1970-01-01")}
  }else if(obs.v$state == "Benue"){
    if(obs.v$year == 2019){
      as.Date(sample(as.Date("2019-06-20"):as.Date("2019-06-27"),1), origin = "1970-01-01")
    }else{as.Date(sample(as.Date("2021-06-10"):as.Date("2021-06-17"),1), origin = "1970-01-01")}
  }else if(obs.v$state == "Imo"){
    if(obs.v$year == 2019){
      as.Date(sample(as.Date("2019-06-18"):as.Date("2019-06-28"),1), origin = "1970-01-01")
    }else{as.Date(sample(as.Date("2021-05-08"):as.Date("2021-06-01"),1), origin = "1970-01-01")}
  }else if(obs.v$state == "Osun"){
    if(obs.v$year == 2019){
      as.Date(sample(as.Date("2019-05-23"):as.Date("2019-06-10"),1), origin = "1970-01-01")
    }else{as.Date(sample(as.Date("2021-05-28"):as.Date("2021-06-21"),1), origin = "1970-01-01")}
  }
}

long_farm_data$planting_date <- as.Date(planting_date,origin = "1970-01-01")

long_farm_data$planting_date_ref_1970_01_01 <- as.numeric(long_farm_data$planting_date )

long_farm_data$genotype[long_farm_data$genotype=="TMS-13F0153P0001"] <- "TMS-13F1053P0010"

write.csv(long_farm_data,"Processed_data/on-farm-data-cassava-nigeria.csv",row.names = F)





