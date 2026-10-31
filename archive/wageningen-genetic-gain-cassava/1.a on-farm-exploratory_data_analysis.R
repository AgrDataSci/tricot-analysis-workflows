
# Data analysis metric-rankings - cassava
# December 2023
# Only descriptive data analysis

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

#setwd(here())

setwd("Data_analysis_tricots/Cassava/Cassava_feb_2024")

cassava_data <- read.csv("Processed_data/on-farm-data-cassava-nigeria.csv")

nga_zones <- read.csv("Data/nga-zones.csv")

cassava_data <- cassava_data %>% left_join(nga_zones,by=c('state'='NAME_1'))

cassava_data <- cassava_data[cassava_data$plot!="LOCAL",]

cassava_data <- droplevels(cassava_data)

# Remove genotypes with less than 10 repetitions

cassava_data$genotype <- as.character(cassava_data$genotype)

table_genotype <- data.frame(table(cassava_data$genotype))

table_genotype <- table_genotype[order(table_genotype$Freq,decreasing = T),]

selecet_var <- table_genotype$Var1[table_genotype$Freq>=10]

cassava_data <- cassava_data[cassava_data$genotype %in% selecet_var,]

cassava_data <- droplevels(cassava_data) 

# descriptive data analysis for the yield

sum1 <- data.frame(cbind(summary(cassava_data$yieldton_ha)))

cassava_data_formm <- cassava_data[c('yieldton_ha','genotype','farm','state')]

cassava_data_formm <- cassava_data_formm[complete.cases(cassava_data_formm),]

dim(cassava_data_formm)

table(cassava_data_formm$genotype,cassava_data_formm$state)

names(sum1) <- "Yield"

yield_summ <- rbind(sum1,Sd=sd(cassava_data$yieldton_ha,na.rm = T),Cv=sd(cassava_data$yieldton_ha,na.rm = T)/mean(cassava_data$yieldton_ha,na.rm=T))

yieldbx <- ggplot(cassava_data,aes(x='On-farm fresh yield (t/ha)',y=yieldton_ha))+geom_boxplot()+xlab("")+
  ylab('On-farm fresh yield (t/ha)')+theme_bw()
yieldbx

ggsave("Results/Exploratory_data_analysis/cassava_yield_boxplot.png",yieldbx,width = 3,height = 4)

# Sort varieties according to the median before plot them

cassava_data$genotype <- as.character(cassava_data$genotype)

cassava_data$genotype2 <- factor(cassava_data$genotype,
                                levels = names(sort(tapply(cassava_data$yieldton_ha,
                                                           cassava_data$genotype, median,na.rm=T),decreasing = T)))

yieldbxgen <- ggplot(cassava_data[!is.na(cassava_data$yieldton_ha),],aes(x=genotype2,y=yieldton_ha))+geom_boxplot(fill='#009999')+xlab("")+
  ylab('On-farm fresh yield (t/ha)')+theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Results/Exploratory_data_analysis/varieties_cassava.png",yieldbxgen,height = 4,width =7)

yieldbxgen <- ggplot(cassava_data[!is.na(cassava_data$yieldton_ha),],aes(x=genotype2,y=yieldton_ha))+geom_boxplot(aes(fill=state))+xlab("")+
  ylab('On-farm fresh yield (t/ha)')+theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Results/Exploratory_data_analysis/varieties_cassava_state.png",yieldbxgen,height = 6,width =12.5 )

yieldbxgen <- ggplot(cassava_data[!is.na(cassava_data$yieldton_ha),],aes(x=state,y=yieldton_ha))+geom_boxplot(aes(fill=state))+xlab("")+
  ylab('On-farm fresh yield (t/ha)')+theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Results/Exploratory_data_analysis/cassava_state.png",yieldbxgen,height = 4,width =5 )

write.csv(cassava_data,'Processed_data/cassava_metric_dataset_post_AE.csv',row.names = F) # Post analysis explolatory
