

library(nlme)
library(ggplot2)
library(tidyr)
library(predictmeans)
library(tidyverse)
library(leaflet)
library(sf)
library(terra)
library(lubridate)


setwd("Data_analysis_tricots/Groundnut/Groundnut_Feb_2024")

# Read data

rm(list = ls())

farms_data0 <- read.csv("Data/groundnut-tanzania-tricot_eco_regions_added.csv")

farms_data0$id <- paste0("ID",1:nrow(farms_data0),"_",farms_data0$id ) # The ID is restarted per each cropcycle

# Select variables

farms_data <-  farms_data0 %>% select(id,
                                      vegetative_plantingdate,
                                      package_item_a,
                                      package_item_b,
                                      package_item_c,
                                      physiologicalmaturity_yieldperse_a,
                                      physiologicalmaturity_yieldperse_b,
                                      physiologicalmaturity_yieldperse_c,
                                      physiologicalmaturity_yield_pos,
                                      physiologicalmaturity_yield_neg,
                                      physiologicalmaturity_overall_pos,
                                      physiologicalmaturity_overall_neg,
                                      physiologicalmaturity_marketability_pos,
                                      physiologicalmaturity_marketability_neg
)



# Extract and process variables

farms_data$id <- as.character(farms_data$id)

names(farms_data)[1] <- "id"

names(farms_data) <- gsub("vegetative_plantingdate","plantingdate",names(farms_data))

names(farms_data) <- gsub("physiologicalmaturity_","",names(farms_data))

names(farms_data) <- gsub("gari_weight_from_plot","gariweight",names(farms_data))

names(farms_data) <- gsub("package_item","genotype",names(farms_data))

# Metric data processing

long_farm_data <-farms_data %>% select(id:yieldperse_c) %>% 
  pivot_longer(
    cols = !(id:plantingdate), 
    names_to = c(".value", "plot"), 
    names_sep = "_", 
    values_drop_na = F
  )

# Ranking data procesing

ranks_farm_data <- farms_data %>% dplyr::select(id,yield_pos:marketability_neg) 

index <- grep("pos|neg",names(ranks_farm_data))

options <- c("A","B","C")

# Remove anything different from the three options for tricot

for(i in index){
  newvar <- ranks_farm_data[,i] 
  newvar <- as.character(ranks_farm_data[,i])
  newvar[is.na(newvar) | !(newvar %in% options) ] <- NA
  ranks_farm_data[,i] <- newvar
}

# Split ranks

ls_class_yield <- with(ranks_farm_data,split(data.frame(yield_pos,yield_neg),seq(length(yield_pos))))

ls_class_overall <- with(ranks_farm_data,split(data.frame(overall_pos,overall_neg),seq(length(overall_pos))))

ls_class_marketability <- with(ranks_farm_data,split(data.frame(marketability_pos,marketability_neg),seq(length(marketability_pos))))

ranks_farm_data$yield_med <- sapply(ls_class_yield,function(w){if(sum(unique(w)%in% options)==2){options[!(options %in% w)][1]}else{ NA }}) # Duplicated cases 

ranks_farm_data$overall_med <- sapply(ls_class_overall,function(w){if(sum(unique(w)%in% options)==2){options[!(options %in% w)][1]}else{ NA }}) # Duplicated cases 

ranks_farm_data$marketability_med <- sapply(ls_class_marketability,function(w){if(sum(unique(w)%in% options)==2){options[!(options %in% w)][1]}else{ NA }}) #


ranks_farm_data <- ranks_farm_data %>% 
  pivot_longer(-id,names_to = c(".value","calification"),names_sep = "_",
               values_drop_na = F ) %>% 
  mutate(rank = str_replace_all(calification,c("pos"="1","neg"="3",
                                               "med"="2"))) 

ranks_farm_data <- ranks_farm_data %>% pivot_longer(-c(id,calification,rank),names_to = "variable",values_to = "plot")


ranks_farm_data <- ranks_farm_data[!is.na(ranks_farm_data$plot),] %>% select(!calification )

# Remove duplicated cases of ranks

table(!duplicated(paste(ranks_farm_data$id,ranks_farm_data$variable,ranks_farm_data$plot,sep = '_')))

ranks_farm_data <- ranks_farm_data[!duplicated(paste(ranks_farm_data$id,ranks_farm_data$variable,ranks_farm_data$plot,sep = '_')),] # No duplicates for groundnut (duplicate in marketability)

table(table(paste(ranks_farm_data$id,ranks_farm_data$variable,ranks_farm_data$plot,sep = '_'))) # Should be unique

ranks_farm_data <- ranks_farm_data %>% pivot_wider(names_from =variable ,values_from = rank)

names(ranks_farm_data)[3:5] <- paste0('rank_',names(ranks_farm_data)[3:5] )

long_farm_data$plot <- toupper(long_farm_data$plot)

# Merge datasets

long_farm_data <- long_farm_data %>% left_join(ranks_farm_data,by=c("id", "plot")) # complete ranking and complete metric (long_farm_data was not filtered, rows are completed)

long_farm_data <- long_farm_data %>% mutate(rank_yield=as.numeric(rank_yield),rank_overall=as.numeric(rank_overall),rank_marketability=as.numeric(rank_marketability) )

long_farm_data$genotype <- as.character(long_farm_data$genotype )

long_farm_data <- long_farm_data[order(long_farm_data$id),]

# names(long_farm_data)[7] <- "date"

# names(long_farm_data)[6] <- "yield"

# Data cleaning

#long_farm_data <- long_farm_data %>% filter(longitude  != 0 & latitude  != 0)# %>%
# mutate(date=as.Date(date)) %>% filter(!is.na(date))

#summary(long_farm_data)

##clean data

boxplot(long_farm_data$yieldperse)

long_farm_data$yieldperse[which(long_farm_data$yieldperse<=0)] <- NA
#long_farm_data$yieldperse[which(long_farm_data$yieldperse>1500)] <- NA

boxplot(long_farm_data$yieldperse)

hist(long_farm_data$yieldperse)



names(long_farm_data)[1] <- "farm"

# Cleaning district




long_farm_data$genotype <- str_replace_all(long_farm_data$genotype,c("Mangaka 2009 \\(check\\)"="Mangaka 2009","MANGAKA 2009"="Mangaka 2009","Naliendele 09"="Naliendele 2009","Naliendele 2016"="Naliendele 2016",
                                                                     "ICGV-SM 08503"="Naliendele 2016","NALX -22-18"="NALX-22-18",'Mnanje 2009 \\(check\\)'='Mnanje 2009',
                                                                     "MNANJE 2009"="Mnanje 2009","Mnanje 09"="Mnanje 2009",'Tanzanite 2016'='Tanzanut 2016',
                                                                     'ICGV-SM 01514'='Tanzanut 2016'))


table(long_farm_data$genotype)

long_farm_data$yieldton_ha <- long_farm_data$yieldperse*1/4*10000/(1000000)

long_farm_data$plantingdate <- as.Date(long_farm_data$plantingdate,"%m/%d/%Y")

long_farm_data$year <- year(long_farm_data$plantingdate)

farms_data0 <- farms_data0 %>% separate('registration_submitted_date',c('reg_date',NA),sep= " ")

farms_data0$reg_date_year <- year(as.Date(farms_data0$reg_date,format = "%m/%d/%Y"))

long_farm_data <- long_farm_data %>% left_join(farms_data0[c("id","reg_date_year")],by=c('farm'="id"))

long_farm_data$year[is.na(long_farm_data$year)] <- long_farm_data$reg_date_year[is.na(long_farm_data$year)]

long_farm_data$plantingdate_1970_01_01 <- as.numeric(long_farm_data$plantingdate)

long_farm_data <- long_farm_data %>% left_join(farms_data0[c('id',"ecoregion")],by=c("farm"='id'))

long_farm_data <- long_farm_data[ long_farm_data$genotype != "Local",]

write.csv(long_farm_data,"Processed_data/on-farm-data-groundnut-tanzania.csv",row.names = F)

