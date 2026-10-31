
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

cassava_data <- read.csv('Processed_data/cassava_metric_dataset_post_AE.csv')

#-------------------------------------------------------------------------------
#------------------------------Fresh yield--------------------------------------


cassava_data <- cassava_data %>% mutate(Zone=as.factor(Zone),genotype=as.factor(genotype),farm=as.factor(farm))

mixmodel1 <- lmer(yieldton_ha~Zone+genotype+(1|Zone:farm)+(1|genotype:Zone),data=cassava_data,na.action = 'na.omit' )

genotypeMeans0 <- predictmeans(mixmodel1,'genotype',barplot=FALSE, pplot=F, bkplot=F, plot=F)


# Genotypic means of the whole experiment

genotype_means1 <- data.frame(emmeans::emmeans(mixmodel1,"genotype"))

write.csv(genotype_means1,"Results/Datasets/on_farm_fresh_yield_genotypic_means.csv",row.names = F)

# Assumptions validation

plot(mixmodel1)

mixmodel0resid = recordPlot() 

qqnorm(residuals(mixmodel1))
qqline(residuals(mixmodel1))

# Genotypic means by location

ls_state <- split(cassava_data,cassava_data$Zone)

gg <- lapply(ls_state, function(x){
  mixmodel0 <- lmer(yieldton_ha~ genotype+  
                      (1|farm),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
  
  summary(mixmodel0)
  
  genotypeMeans <- predictmeans(mixmodel0,'genotype', pplot=F, bkplot=F, plot=F)
  
  genotypeMeans
  
  genotype.means <- data.frame(genotypeMeans$mean_table)
  
  data.frame(genotype.means,Zone=unique(x$Zone))

}
  
)

library(ggpmisc) 

complete_gg <- do.call(rbind,gg)

write.csv(complete_gg,"Results/Datasets/on_farm_fresh_yield_location_genotypic_means.csv",row.names = F)

# Genotypic means by location-year 

ls_state <- split(cassava_data,paste0(cassava_data$Zone,"-",cassava_data$Y))

gg <- lapply(ls_state, function(x){
  mixmodel0 <- lmer(yieldton_ha~ genotype+  
                      (1|farm),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
  
  summary(mixmodel0)
  
  genotypeMeans <- predictmeans(mixmodel0,'genotype', pplot=F, bkplot=F, plot=F)
  
  genotypeMeans
  
  genotype.means <- data.frame(genotypeMeans$mean_table)
  
  data.frame(genotype.means,Zone=unique(x$Zone))
  
}

)

library(ggpmisc) 

complete_gg <- do.call(rbind,gg)

write.csv(complete_gg,"Results/Datasets/on_farm_fresh_yield_location_genotypic_means.csv",row.names = F)



#-------------------------------------------------------------------------------
#----------------------------Marketable yield-----------------------------------


# cassava_data <- cassava_data[-which(row.names(cassava_data) %in% c(1031,1309,1311)),]


mixmodel0 <- lmer(yieldmarkton_ha~ genotype+Zone+  
                    (1|Zone:farm)+ # Genotype by year interacton zero and was removed
                    (1|genotype:Zone ),data=cassava_data,na.action = 'na.omit' )


genotypeMeans <- predictmeans(mixmodel0,'genotype', mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)

write.csv(genotypeMeans$mean_table,"Results/Datasets/on_farm_marketable_yield_genotypic_means.csv",row.names = F)


# Marketable yield location

ls_state <- split(cassava_data,cassava_data$Zone)

gg <- lapply(ls_state, function(x){
  print(unique(x$Zone))
  x <- x[,c('yieldmarkton_ha','farm','genotype', 'Zone')]
  
  if(nrow(x[complete.cases(x),])==0){return(NULL)}
    
  mixmodel0 <- lmer(yieldmarkton_ha~ genotype+  
                      (1|farm),data=x,na.action = 'na.omit')# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
  
  summary(mixmodel0)
  
  genotypeMeans <- predictmeans(mixmodel0,'genotype', mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
  
  genotypeMeans
  
  genotype.means <- data.frame(genotypeMeans$mean_table[,1:2])
  

  data.frame(genotype.means,Zone=unique(x$Zone))

}

)

library(ggpmisc) 

complete_gg <- do.call(rbind,gg)

write.csv(complete_gg,"Results/Datasets/on_farm_marketable_yield_location_genotypic_means.csv")




