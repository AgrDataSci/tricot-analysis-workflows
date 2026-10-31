
# Data analysis metric-rankings - cassava
# December 2023
# Estimation of genetic parameters

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

# Spatail cluster Groups (just repeting cluster analysis)

on_station_processed <- read.csv("Processed_data/on-station-data-cassava-nigeria.csv")

on_farm_data <- read.csv('Processed_data/on-farm-data-cassava-nigeria.csv')

on_station_coords <- read.csv("Data/station-coordinates_est.csv")

pal <- colorFactor(
  palette = 'Dark2',
  domain = on_farm_data$state        
)

cl1 <- leaflet(on_farm_data) %>% addTiles() %>% 
  addCircleMarkers(lng = ~longitude,label = ~state,
                   lat = ~latitude,fillOpacity = 1,
                   color = ~pal(state),
                   fillColor = ~pal(state), radius = 5,
                   weight = 1)  %>%
  
  addLegend("topleft", pal = pal, values = ~state,
            title = "(e.g.) % voting UKIP at GE2015",
            opacity = 1) %>%
  
  addMarkers(data = on_station_coords,lng = ~longitude,label = ~location ,
             lat = ~latitude,labelOptions = ~location) %>%
  addLabelOnlyMarkers(data = on_station_coords,lng = ~longitude-0.3,lat =  ~latitude, 
                      label =  ~as.character(location), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = F))

cl1


mapshot(cl1,file='Results/Exploratory_data_analysis/spatial_cluster_cassava.png')


# on_station_data <- on_station_data %>% left_join(on_station_coords[c('location','state')],c("location"="location")) 

on_station_data %>% ggplot(aes(x=location,y=fyld))+
  geom_boxplot(aes(fill=Zone))+ scale_fill_discrete(name = "Zone")+facet_wrap(~on_farm_zone,scales="free_x") +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Location") + ylab("On-station fresh yield (t/ha)") 

on_station_data_gg <- on_station_data %>% ggplot(aes(x=location,y=fyld))+
  geom_boxplot(fill="#009999") +facet_wrap(~on_farm_zone,scales="free_x") +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Location") + ylab("On-station fresh yield (t/ha)") 


ggsave("Results/Exploratory_data_analysis/on_station_yield_location.png",on_station_data_gg,height = 4,width = 6)


on_station_data_gg_var <- on_station_data %>% ggplot(aes(x=genotype,y=fyld))+
  geom_boxplot(fill="#009999")  +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Genotype") + ylab("On-station fresh yield (t/ha)") + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Results/Exploratory_data_analysis/on_station_yield_var.png",on_station_data_gg_var,height = 4,width = 7)


on_station_data_gg_year <- on_station_data %>% ggplot(aes(x=factor(Year),y=fyld))+
  geom_boxplot(fill="#009999")  +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Planting year") + ylab("On-station fresh yield (t/ha)") + theme_bw()


ggsave("Results/Exploratory_data_analysis/on_station_yield_location.png",on_station_data_gg_year,height = 4,width = 4.5)

on_station_data_gg_location_geno1 <- on_station_data %>% filter(location %in% c("Abuja"  , "Ago-Owu", "Ibadan" , "Ikenne",  "Kano" ,   "Mokwa")) %>% ggplot(aes(x=genotype,y=fyld))+
  geom_boxplot(fill="#009999") +facet_grid(location~.) +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Genotype") + ylab("On-station fresh yield (t/ha)") + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("Results/Exploratory_data_analysis/on_station_yield_location_geno_1.png",on_station_data_gg_location_geno1,height =7,width = 5)

on_station_data_gg_location_geno2 <- on_station_data%>% filter(location %in% c("Onne" ,   "Otobi"  , "Ubiaja" , "Umudike", "Zaria" )) %>% ggplot(aes(x=genotype,y=fyld))+
  geom_boxplot(fill="#009999") +facet_grid(location~.) +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Genotype") + ylab("On-station fresh yield (t/ha)") + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("Results/Exploratory_data_analysis/on_station_yield_location_geno_2.png",on_station_data_gg_location_geno2,height =6,width = 5)

