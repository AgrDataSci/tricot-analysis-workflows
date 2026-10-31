#


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
library(ggrepel)
library(ggpmisc)
library(quantreg)

setwd("Data_analysis_tricots/Groundnut/Groundnut_Feb_2024")

genotype_info <- readxl::read_xlsx("Data/groundnut-genotype-release-information (003).xlsx")

release_year <- genotype_info[genotype_info$release_year!="Not released",]

release_year$genotype <- gsub("Naliendele 09",'Naliendele 2009',release_year$genotype)

release_year$genotype <- gsub("Mnanje 09","Mnanje 2009",release_year$genotype)


release_year <- genotype_info[genotype_info$release_year!="Not released",]

release_year$release_year <- as.numeric(release_year$release_year)

release_year$genotype <- gsub("Naliendele 09",'Naliendele 2009',release_year$genotype)

release_year$genotype <- gsub("Mnanje 09","Mnanje 2009",release_year$genotype)

on_station_yield <- read.csv('Results/Datasets/on_station_yield_genotypic_means.csv')

on_station_yield_gg <- inner_join(on_station_yield,release_year,by='genotype')

g1 <- ggplot(on_station_yield_gg,aes(y=Mean,x=release_year))+geom_point()+theme_bw()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  )+# geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F) +geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),colour='gray')+#geom_smooth(method = "lm",se=F, aes(colour='Simple')) +geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "blue")+
  #stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D") +
  ylab("Genotypic means of on-station yield (t/ha)") + xlab("Release year") #+ labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")

summary(lm(Mean~release_year,data=on_station_yield_gg))

summary(rq(Mean~release_year,data=on_station_yield_gg,tau = 0.75),se='iid')

ggsave("Results/genetic_gain/genetic_gain_on_station.png",g1,height = 4,width = 5)

on_station_yield_loc <- read.csv('Results/Datasets/on_station_location_yield_genotypic_means.csv')

on_station_yield_loc_gg <- inner_join(on_station_yield_loc,release_year,by='genotype')

g2 <- ggplot(on_station_yield_loc_gg,aes(y=Mean,x=release_year))+geom_point()+theme_bw()+ geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F, aes(colour='Simple')) +geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "#00BFC4")+
  stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D")+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  ) +facet_wrap(Location~.)+
  ylab("Genotypic means of on-station yield (t/ha)") + xlab("Release year") + labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")


lapply( split(on_station_yield_loc_gg,on_station_yield_loc_gg$Location) ,function(wa){
  
  
  print(summary(lm(Mean~release_year,data=wa)))
  
  #print(summary(rq(Mean~release_year,data=wa,tau = 0.75),se='iid'))
}
)


ggsave("Results/genetic_gain/genetic_gain_on_station_location.png",g2,height = 6,width = 8)

on_farm_yield <- read.csv('Results/Datasets/on_farm_yield_genotypic_means.csv')

on_farm_yield_gg <- inner_join(on_farm_yield,release_year,by='genotype')

g3 <- ggplot(on_farm_yield_gg,aes(y=Mean,x=release_year))+geom_point()+theme_bw()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  )+ #geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F) +geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),colour='gray')+#geom_smooth(method = "lm",se=F, aes(colour='Simple')) +geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "blue")+
  #stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D") +
  ylab("Genotypic means of on-farm yield (t/ha)")+ xlab("Release year")  #+ labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")

summary(lm(Mean~release_year,data=on_farm_yield_gg))

summary(rq(Mean~release_year,data=on_station_yield_gg,tau = 0.75),se='iid')

ggsave("Results/genetic_gain/genetic_gain_on_farm.png",g3,height = 4,width = 5)

on_farm_yield_loc <- read.csv('Results/Datasets/on_farm_yield_location_genotypic_means.csv')

on_farm_yield_loc_gg <- inner_join(on_farm_yield_loc,release_year,by='genotype')


g4 <- ggplot(on_farm_yield_loc_gg,aes(y=Mean,x=release_year))+geom_point()+theme_bw()+
 geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F, aes(colour='Simple')) +geom_errorbar(aes(ymin=Mean+SE, ymax=Mean-SE),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "#00BFC4")+
  stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D") +facet_wrap(state~.)+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  )+
  ylab("Genotypic means of on-farm yield (t/ha)") + xlab("Release year") + labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")

lapply( split(on_farm_yield_loc_gg,on_farm_yield_loc_gg$state) ,function(wa){
  
  
  print(summary(lm(Mean~release_year,data=wa)))
  
  #print(summary(rq(Mean~release_year,data=wa,tau = 0.75),se='iid'))
}
)

ggsave("Results/genetic_gain/genetic_gain_on_farm_location.png",g4,height = 6,width = 8)

on_farm_yield_ranks <- read.csv('Results/Datasets/log_worths_rank_yield.csv')

on_farm_yield_gg <- inner_join(on_farm_yield_ranks,release_year,by='genotype')

g5 <- ggplot(on_farm_yield_gg,aes(y=coef_estimate,x=release_year))+geom_point()+theme_bw()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  )+ #geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F) +geom_errorbar(aes(ymin=coef_estimate-coef_std_error, ymax=coef_estimate+coef_std_error),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "blue")+#stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "#00BFC4")+
  #stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D") +#facet_wrap(state~.)+
  ylab("PL Log(worth) of farmer rankings of yield") + xlab("Release year") + labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")

summary(lm(coef_estimate~release_year,data=on_farm_yield_gg))

summary(rq(coef_estimate~release_year,data=on_farm_yield_gg,tau = 0.75),se='iid')

ggsave("Results/genetic_gain/genetic_gain_on_ranking_yield.png",g5,height = 4,width = 5)

on_farm_yield_ranks_loc <- read.csv('Results/Datasets/log_worth_yield_location.csv')

names(on_farm_yield_ranks_loc)[1] <- 'genotype'

on_farm_yield_loc_gg <- inner_join(on_farm_yield_ranks_loc,release_year,by=c('genotype'))

g6 <- ggplot(on_farm_yield_loc_gg,aes(y=coef_estimate,x=release_year))+geom_point()+theme_bw()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  )+ geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F, aes(colour='Simple')) +#geom_errorbar(aes(ymin=coef.pl.coef_estimate-1.96*coef.pl.coef_std_error, ymax=coef.pl.coef_estimate+1.96*coef.pl.coef_std_error),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "#00BFC4")+
  stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D") +facet_wrap(Zone~.)+
  ylab("PL Log(worth) of farmer rankings of yield") + xlab("Release year") + labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")

lapply( split(on_farm_yield_loc_gg,on_farm_yield_loc_gg$Zone) ,function(wa){
  
  
  print(summary(lm(coef_estimate~release_year,data=wa)))
  
  print(summary(rq(coef_estimate~release_year,data=wa,tau = 0.75),se='iid'))
}
)



ggsave("Results/genetic_gain/genetic_gain_on_farm_ranking_yield.png",g6,height = 6,width = 8)

on_farm_market_ranks <- read.csv('Results/Datasets/log_worths_rank_marketabilty.csv')

on_farm_market_gg <- inner_join(on_farm_market_ranks,release_year,by='genotype')

g7 <- ggplot(on_farm_market_gg,aes(y=coef_estimate,x=release_year))+geom_point()+theme_bw()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  )+ geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F, aes(colour='Simple')) +geom_errorbar(aes(ymin=coef_estimate-coef_std_error, ymax=coef_estimate+coef_std_error),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "#00BFC4")+
  stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D") +#facet_wrap(state~.)+
  ylab("PL Log(worth) of farmer rankings of marketability") + xlab("Release year") + labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")

summary(lm(coef_estimate~release_year,data=on_farm_market_gg))

summary(rq(coef_estimate~release_year,data=on_farm_market_gg,tau = 0.75),se='iid')

ggsave("Results/genetic_gain/genetic_gain_on_ranking_marketable.png",g7,height = 4,width = 5)

on_farm_market_ranks_loc <- read.csv('Results/Datasets/log_worth_marketability_location.csv')

on_farm_market_loc_gg <- inner_join(on_farm_market_ranks_loc,release_year,by=c('genotype'))

g8 <- ggplot(on_farm_market_loc_gg,aes(y=coef_estimate,x=release_year))+geom_point()+theme_bw()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  )+ geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F, aes(colour='Simple')) +#geom_errorbar(aes(ymin=coef.pl.coef_estimate-1.96*coef.pl.coef_std_error, ymax=coef.pl.coef_estimate+1.96*coef.pl.coef_std_error),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "#00BFC4")+
  stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D") +facet_wrap(Zone~.)+
  ylab("PL Log(worth) of farmer rankings of marketability") + xlab("Release year") + labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")

ggsave("Results/genetic_gain/genetic_gain_on_farm_ranking_market.png",g8,height = 6,width = 8)

lapply( split(on_farm_market_loc_gg,on_farm_market_loc_gg$Zone) ,function(wa){
  
  
  print(summary(lm(coef_estimate~release_year,data=wa)))
  
  print(summary(rq(coef_estimate~release_year,data=wa,tau = 0.75),se='iid'))
}
)

on_farm_over_ranks <- read.csv('Results/Datasets/log_worths_rank_overall.csv')

on_farm_over_gg <- inner_join(on_farm_over_ranks,release_year,by='genotype')

g9 <- ggplot(on_farm_over_gg,aes(y=coef_estimate,x=release_year))+geom_point()+theme_bw()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  )+# geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F) +geom_errorbar(aes(ymin=coef_estimate-coef_std_error, ymax=coef_estimate+coef_std_error),colour='gray')+#geom_smooth(method = "lm",se=F, aes(colour='Simple')) +geom_errorbar(aes(ymin=coef_estimate-coef_std_error, ymax=coef_estimate+coef_std_error),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "blue")+
  #stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D") +#facet_wrap(state~.)+
  ylab("PL Log(worth) of farmer rankings of overall appreciation") + xlab("Release year") + labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")

ggsave("Results/genetic_gain/genetic_gain_on_ranking_overall.png",g9,height = 4,width = 5)

summary(lm(coef_estimate~release_year,data=on_farm_over_gg))

summary(rq(coef_estimate~release_year,data=on_farm_over_gg,tau = 0.75),se='iid')


on_farm_over_ranks_loc <- read.csv('Results/Datasets/log_worth_overall_location.csv')

on_farm_over_loc_gg <- inner_join(on_farm_over_ranks_loc,release_year,by=c('genotype'))

g10 <- ggplot(on_farm_over_loc_gg,aes(y=coef_estimate,x=release_year))+geom_point()+theme_bw()+
  geom_text_repel(
    aes(label=genotype),
    box.padding = unit(0.2, "lines"),size=2
  )+ geom_quantile(quantiles = 0.75,lwd=1, aes(colour='Quantile 0.75')) +
  #geom_quantile(quantiles = 0.75,lwd=1,method = "rqss",alpha=5, aes(colour='Qx(0.75) rqss Linear'))+
  geom_smooth(method = "lm",se=F, aes(colour='Simple')) +#geom_errorbar(aes(ymin=coef.pl.coef_estimate-1.96*coef.pl.coef_std_error, ymax=coef.pl.coef_estimate+1.96*coef.pl.coef_std_error),colour='gray')+
  stat_poly_eq(use_label('eq'),formula = y ~ x,parse = TRUE,label.x = "right",label.y = "bottom",color = "#00BFC4")+
  stat_quant_eq(formula = y ~ x,parse = TRUE,quantiles =0.75,label.x = "left",label.y = "top",color = "#F8766D") +facet_wrap(Zone~.)+
  ylab("PL Log(worth) of farmer rankings of overall appreciation") + xlab("Release year") + labs(colour='Linear regression')  #+facet_wrap(~state)+ xlab("Genotypic means of on-station yield")+ylab("PL Log (worth) of farmer rankings of overall appreciation")

lapply( split(on_farm_over_loc_gg,on_farm_over_loc_gg$Zone) ,function(wa){
  
  
  print(summary(lm(coef_estimate~release_year,data=wa)))
  
  #print(summary(rq(coef_estimate~release_year,data=wa,tau = 0.75),se='iid'))
}
)


ggsave("Results/genetic_gain/genetic_gain_on_farm_ranking_overall.png",g10,height = 6,width = 8)


