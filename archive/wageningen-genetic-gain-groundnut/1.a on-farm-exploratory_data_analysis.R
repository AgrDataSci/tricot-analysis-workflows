
# Data analysis metric-rankings - groundnuts
# December 2023

library(PlackettLuce)
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
library(lubridate)

#setwd(here())

# Read data

# setwd("C:/Users/dorad002/R_Projects/Data_analysis_tricots/Groundnut/Groundnut_Feb_2024")

crop_country <- 'groundnut_tanzania'

groundnut_data <- read.csv("Processed_data/on-farm-data-groundnut-tanzania.csv",stringsAsFactors = T)

#groundnut_data$yieldton_ha <- groundnut_data$yieldperse*1/4*10000/(1000000) # 1kg/30mts2*1000mts2/1ha*1ton/1000kg

summary(groundnut_data$yieldton_ha*1000)

hist(groundnut_data$yieldton_ha)

# Number of plots

groundnut_data_formm <- groundnut_data[c('yieldton_ha','genotype','farm','ecoregion',"year","plot")]

dim(groundnut_data_formm)

groundnut_data_formm <- groundnut_data_formm[complete.cases(groundnut_data_formm[c('yieldton_ha')]),]

dim(groundnut_data_formm)

groundnut_data_formm <- groundnut_data_formm[groundnut_data_formm$yieldton_ha<=5,]

dim(groundnut_data_formm)

groundnut_data_formm <- groundnut_data_formm[complete.cases(groundnut_data_formm[c('yieldton_ha','genotype','farm','ecoregion',"year","plot")]),]

dim(groundnut_data_formm)

table_location <- data.frame(table(groundnut_data_formm$ecoregion))

table_location  <- table_location[order(table_location$Freq,decreasing = T),]

selecet_var <- table_location$Var1[table_location$Freq>=20] # Freq>=20

groundnut_data_formm <- groundnut_data_formm[groundnut_data_formm$ecoregion %in% selecet_var,]

table_genotype <- data.frame(table(groundnut_data_formm$genotype))

table_genotype <- table_genotype[order(table_genotype$Freq,decreasing = T),]

selecet_var <- table_genotype$Var1[table_genotype$Freq>=10] # Freq>=20

dim(groundnut_data_formm)

groundnut_data_formm <- groundnut_data_formm[groundnut_data_formm$genotype %in% selecet_var,]

dim(groundnut_data_formm)

groundnut_data_formm <- droplevels(groundnut_data_formm) 

table(groundnut_data_formm$ecoregion)

table(groundnut_data_formm$genotype)



# Filter location

nrow(groundnut_data_formm)

length(unique(groundnut_data_formm$farm))

length(unique(groundnut_data_formm$genotype))

length(unique(groundnut_data_formm$ecoregion))

unique(groundnut_data_formm$year)

length(unique(groundnut_data_formm$year))

table(groundnut_data_formm$genotype,groundnut_data_formm$year)

table(groundnut_data_formm$genotype,groundnut_data_formm$ecoregion)

unique(groundnut_data_formm$ecoregion)



# names(sum1) <- "Yield"
# 
# yield_summ <- rbind(sum1,Sd=sd(groundnut_data$yieldton_ha,na.rm = T),Cv=sd(groundnut_data$yieldton_ha,na.rm = T)/mean(groundnut_data$yieldton_ha,na.rm=T))

yieldbx <- ggplot(groundnut_data,aes(x='On-farm yield (t/ha)',y=yieldton_ha))+geom_boxplot()+xlab("")+
  ylab('Yield (ton/ha)')+theme_bw()
yieldbx

ggsave("Results/groundnut_tanzania/groundnut_yield_boxplot.png",yieldbx,width = 3,height = 4)


# Sort varieties according to the median before plot them

groundnut_data_formm$genotype <- as.character(groundnut_data_formm$genotype)

groundnut_data_formm$genotype2 <- factor(groundnut_data_formm$genotype,
                                         levels = names(sort(tapply(groundnut_data_formm$yieldton_ha,
                                                                    groundnut_data_formm$genotype, median,na.rm=T),decreasing = T)))

yieldbxgen <- ggplot(groundnut_data_formm,aes(x=genotype2,y=yieldton_ha))+geom_boxplot(fill='#009999')+xlab("")+
  ylab('On-farm yield (t/ha)')+theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Results/groundnut_tanzania/groundnut_yield_boxplot.png",yieldbxgen,height = 4,width =7)

groundnut_data_formm$state2 <- factor(groundnut_data_formm$ecoregion,
                                      levels = names(sort(tapply(groundnut_data_formm$yieldton_ha,
                                                                 groundnut_data_formm$ecoregion, median,na.rm=T),decreasing = T)))


yieldbxgen <- ggplot(groundnut_data_formm,aes(x=state2,y=yieldton_ha))+geom_boxplot(fill='#009999')+xlab("")+
  ylab('On-farm yield (t/ha)')+theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Results/groundnut_tanzania/groundnut_state.png",yieldbxgen,height = 4,width =5 )


groundnut_data_formm$year <- factor(groundnut_data_formm$year)

yieldbxgen <- ggplot(groundnut_data_formm,aes(x=year,y=yieldton_ha))+geom_boxplot(fill='#009999')+xlab("")+
  ylab('On-farm yield (t/ha)')+theme_bw()

ggsave("Results/groundnut_tanzania/groundnut_year.png",yieldbxgen,height = 4,width =5 )


datePlot <- ggplot(groundnut_data_formm,aes(x=date))+geom_histogram()+theme_bw()

groundnut_data_formm %>% select('farm',"plot",'ecoregion', 'year', 'genotype', 'yieldton_ha')

groundnut_data_formm %>% ggplot(aes(x=genotype,yieldton_ha)) + geom_boxplot() +theme_bw() + ylab('Yield (t/ha)') + xlab('Genotype') 

write.csv(groundnut_data_formm,'Processed_data/groundnut-tanzania_metric_dataset_post_AE.csv',row.names = F)

ecoregion <- groundnut_data_formm %>% group_by(ecoregion) %>% summarise(n=n())

ecoregion <- data.frame(ecoregion,ecoregion3=paste0(ecoregion$ecoregion," (",ecoregion$n,")"))

genotype <- groundnut_data_formm %>% group_by(genotype) %>% summarise(n=n())

genotype <- data.frame(genotype,genotype3=paste0(genotype$genotype," (",genotype$n,")"))

year <- groundnut_data_formm %>% group_by(year) %>% summarise(n=n())

year <- data.frame(year,year3=paste0(year$year," (",year$n,")"))

groundnut_data_formm <- groundnut_data_formm %>% 
  left_join(genotype[c('genotype','genotype3')],by='genotype')

groundnut_data_formm <- groundnut_data_formm %>% 
  left_join(ecoregion[c('ecoregion','ecoregion3')],by='ecoregion')

groundnut_data_formm <- groundnut_data_formm %>% 
  left_join(year[c('year','year3')],by='year')

g1 <- groundnut_data_formm %>% ggplot(aes(x=genotype3,yieldton_ha)) + geom_boxplot()+ theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ylab('Yield (t/ha)') + xlab('')# xlab('Genotype')


g2 <- groundnut_data_formm %>% ggplot(aes(x=ecoregion3,y=yieldton_ha))+geom_boxplot()+theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ylab('Yield (t/ha)') +xlab('')# xlab('Eco region') 

g3 <- groundnut_data_formm %>% ggplot(aes(x=factor(year3) ,y=yieldton_ha))+geom_boxplot()+ theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ylab('Yield (t/ha)') +xlab('') #xlab('Harvest year') 

library(ggpubr)

g <- ggarrange(ggarrange(g2,g3,ncol=2,nrow=1,labels=c("A","B")),nrow=2,ggarrange(g1,labels = 'C'))

g

summary(groundnut_data_formm$yieldton_ha*1000)

ggsave('Exploratory_data_analysis.png',g,height = 6.5,width = 7)
