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

setwd('Parameters_estimations_tricots/')

# On station data analysis  cassava 

on_station_data <- read.csv('Data/cassava-onstation-traits.csv')

on_farm_data <- read.csv('Processed_data/on-farm-data-cassava-nigeria.csv')

on_station_coords <- read.csv("Data/station-coordinates_est.csv")

nga_zones <- read.csv("Data/nga-zones.csv")

# release year

# recover farms location

on_farm_data <- on_farm_data %>% left_join(nga_zones,by=c('state'='NAME_1'))


state_zone <- 
  data.frame(state= c("Onne","Otobi","Ubiaja","Zaria","Kano","Mokwa","Ikenne",'Ibadan',
                      'Ago-Owu',
                      'Abuja',
                      'Umudike'), 
             Zone = c("South Central","South Central","South Central",
                      "North Central","North Central","West Central",
                      'South West','South West',
                                           'South West',
                                           'North Central',
                                           'South South'),
             on_farm_zone = c("Out of on-farm zone","Out of on-farm zone",
                              "Out of on-farm zone","Out of on-farm zone",
                              "Out of on-farm zone","Out of on-farm zone",
                              "Out of on-farm zone","Within of on-farm zone",
                              "Within of on-farm zone","Within of on-farm zone",
                              "Within of on-farm zone")
  )

on_station_data <- on_station_data %>% left_join(state_zone,c("location"="state")) 


data.frame(unique(on_farm_data$genotype))

on_station_data$accession_name <- gsub("TMS","TMS-",on_station_data$accession_name)

on_station_data$accession_name <- gsub("--","-",on_station_data$accession_name)

on_station_data$accession_name <- 

# No all the varieties tested on-farm were tested on-station

unique(on_station_data$accession_name) %in% unique(on_farm_data$genotype)

unique(on_station_data$accession_name)[!(unique(on_station_data$accession_name) %in% unique(on_farm_data$genotype))]

data.frame(unique(on_station_data$accession_name))

data.frame(unique(on_farm_data$genotype))

on_station_data$Year <- factor(on_station_data$Year)

on_station_data$accession_name <- factor(on_station_data$accession_name)

on_station_data$location <- factor(on_station_data$location)

dim(on_station_data)

table(on_station_data$accession_name,on_station_data$location,on_station_data$Year)

table(on_station_data$accession_name,on_station_data$location)

table(on_station_data$accession_name,on_station_data$Year)

names(on_station_data)[which(names(on_station_data) == 'accession_name')] <- "genotype"

on_station_data$genotype[on_station_data$genotype=="TMS-13F0153P0001"] <- "TMS-13F1053P0010"

write.csv(on_station_data,"Processed_data/on-station-data-cassava-nigeria.csv",row.names = F)

