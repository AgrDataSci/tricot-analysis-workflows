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
library(readxl)
library(sf)
library(terra)
library(stringr)
library(ggpmisc)

setwd('Data_analysis_tricots/Groundnut/Groundnut_Feb_2024')

# On station data analysis  cassava 

names_ds <- c("Preliminary_yieldtrial_2020","Preliminary_yieldtrial_2021","Preliminary_yieldtrial_2022","Advance_yieldtrial_2019","Advance_yieldtrial_2020","Advance_yieldtrial_2021","Advance_yieldtrial_2022")

on_station_data1 <- lapply(1:7,function(w){read_xlsx('Data/Data genetic gain_updated_names.xlsx',sheet = w)})

on_station_data1 <- on_station_data1[-4]

on_station_data_ls <- lapply(1:6,function(i){
  ds=on_station_data1[[i]][c("SITE","REP","GENOTYPE","KY")]
  data.frame(id=1:nrow(ds),ds,datasetnam=names_ds[i])
})

on_station_data2 <- read_xlsx('Data/Tanzania Groundnut-PYT.xlsx')[c("SITE","REP","GENOTYPE","PDY (kg/ha)")]

Advance_yieldtrial_2023 <- data.frame(id=1:nrow(on_station_data2),on_station_data2,datasetnam='Advance_yieldtrial_2023')

names(Advance_yieldtrial_2023)[5] <- "KY"

complete_dataset <- rbind(do.call(rbind,on_station_data_ls),Advance_yieldtrial_2023)

complete_dataset <- complete_dataset %>% separate(datasetnam,c("DS",NA,"Year"),sep="_")

complete_dataset$Year <- as.numeric(complete_dataset$Year)

table(complete_dataset$SITE)

complete_dataset$NEW_SITE <- str_replace_all(complete_dataset$SITE,c('CHAMBEZI'='Bagamoyo', 'KIHINGA'='Kigoma', 'MAKUTOPORA'='Dodoma Urban', 'MAKUTUPORA' ='Dodoma Urban', 'NACHINGWEA'= 'Nachingwea' , 'NAKAYAYA'= 'Tunduru',
                                                                     'NALIENDELE'='Mtwara', 'TUMBI'  ='Tabora Urban', 'UKIRIGURU'= 'Misungwi', 'UKIRUGURU'='Misungwi','UYOLE'='Mbeya'))


africa_shp <- st_read("C:/Users/dorad002/R_Projects/Map_select_counties/afr_g2014_2013_2/afr_g2014_2013_2.shp")

Tanzania <- africa_shp[africa_shp$ADM0_NAME=='United Republic of Tanzania',]

long_farm_data <- complete_dataset %>% left_join(unique(st_drop_geometry((Tanzania[c("ADM2_NAME","ADM1_NAME")]))),by=c("NEW_SITE"="ADM2_NAME"))

long_farm_data$PY_ton_ha <- long_farm_data$KY/1000

long_farm_data$ADM1_NAME <-  factor(long_farm_data$ADM1_NAME)

long_farm_data$PY_ton_ha[long_farm_data$PY_ton_ha==0] <- NA

long_farm_data$GENOTYPE[long_farm_data$GENOTYPE=="Mnanje"] <- "Mnanje 2009"

long_farm_data$GENOTYPE[long_farm_data$GENOTYPE=="Naliendele"] <- "Naliendele 2009"

long_farm_data$GENOTYPE <- str_replace_all(long_farm_data$GENOTYPE,c('SM165'='SM 165',
                                                                     "MNANJE 09"="Mnanje 2009",'Mnanje 09'="Mnanje 2009","Naliende 09"='Naliendele 2009',
                                                                     'Naliendele 09' = "Naliendele 2009","ICGV-SM 01514"="Tanzanut 2016",
                                                                     "ICGV-SM 08503"="Naliendele 2016","MANGAKA 09"="Mangaka 2009","ICGV-SM16"='ICGV-SM 16',
                                                                     "ICGV-SM 01514"="Tanzanut 2016"  ))




sort(unique(long_farm_data$GENOTYPE))


namVar <- names(sort(tapply(long_farm_data$PY_ton_ha,long_farm_data$ADM1_NAME,mean,na.rm=T),decreasing = T))

long_farm_data$ADM1_NAME2 <- factor(long_farm_data$ADM1_NAME,levels = namVar)

location_table <- data.frame(


  state2 = c('Dodoma',
             'Geita',
             'Iringa',
             'Katavi',
             'Kigoma',
             'Lindi',
             'Mbeya',
             'Mtwara',
             'Mwanza',
             'Ruvuma',
             'Shinyanga',
             'Tabora',
             'Pwani'),
  Location = c("Central","Western","Southern highlands","Southern highlands",
               "Western", "Southern","Southern highlands","Southern","Lake","Southern",
               "Western","Western","Eastern")

)

# Mwanza Lindi Kigoma Dodoma Mtwara Tabora Ruvuma Mbeya Pwani

long_farm_data <- long_farm_data %>% left_join(location_table,by=c('ADM1_NAME2'='state2'))

names(long_farm_data)[which(names(long_farm_data)=="GENOTYPE")] <- 'genotype'

write.csv(long_farm_data,'Processed_data/on-station-data-groundnut-tanzania.csv',row.names = F)
