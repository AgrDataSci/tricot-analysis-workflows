
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

H2_cullis.calc<-function(mixmodel,var="genotype"){
  
  BLUPs=ranef(mixmodel)[,var]
  V_BLUP<-var(c(outer(unlist(BLUPs),unlist(BLUPs),"-")))
  VC=as.data.frame(VarCorr(mixmodel1))
  Vg<-VC$vcov[VC$grp=="genotype"] 
  H2_cullis=1-V_BLUP/(2* Vg)
  return(H2_cullis)	
  
}



on_station_data <- read.csv('Data/cassava-nigeria/cassava-onstation-traits.csv')

on_station_data


# on_farm_data_0 <- read.csv("Data/cassava-nigeria/tricot-data-nextgen-cassava.csv")[c("tricot_pkg","state")]

on_farm_data <- read.csv('Proccesed_data/cassava-nigeria.csv')

on_station_coords <- read.csv("Data/cassava-nigeria/station-coordinates_est.csv")

# on_farm_data <- unique(on_farm_data[c('farm','longitude','latitude')])

nga_zones <- read.csv("Data/cassava-nigeria/nga-zones.csv")

# release year

genotype_info <- readxl::read_xlsx("Data/cassava-nigeria/genotype-information.xlsx")

genotype_info <- genotype_info[,c(1,2,11)]

genotype_info <- data.frame(genotype_info)

names(genotype_info) <- c("genotype","check_vairiety","Release.year")

release_data <- genotype_info[genotype_info$Release.year!="NR",]

# recover farms location

on_farm_data <- on_farm_data %>% left_join(nga_zones,by=c('state'='NAME_1'))


# Spatail cluster Groups (just repeting cluster analysis)


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


mapshot(cl1,file='Results/spatial_cluster_cassava.png')


state_zone <- 
  data.frame(state= c("Onne","Otobi","Ubiaja","Zaria","Kano","Mokwa","Ikenne",'Ibadan',
             'Ago-Owu',
             'Abuja',
             'Umudike'), Zone = c("South Central","South Central","South Central","North Central","North Central","West Central",'South West','South West',
             'South West',
             'North Central',
             'South South'), on_farm_zone = c("Out of on-farm zone","Out of on-farm zone","Out of on-farm zone","Out of on-farm zone","Out of on-farm zone","Out of on-farm zone",
                                              "Out of on-farm zone","Within of on-farm zone","Within of on-farm zone","Within of on-farm zone","Within of on-farm zone")
  )

on_station_data <- on_station_data %>% left_join(state_zone,c("location"="state")) 

# on_station_data <- on_station_data %>% left_join(on_station_coords[c('location','state')],c("location"="location")) 

on_station_data %>% ggplot(aes(x=location,y=fyld))+
  geom_boxplot(aes(fill=Zone))+ scale_fill_discrete(name = "Zone")+facet_wrap(~on_farm_zone,scales="free_x") +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Location") + ylab("On-station fresh yield (t/ha)") 

on_station_data_gg <- on_station_data %>% ggplot(aes(x=location,y=fyld))+
  geom_boxplot(fill="#009999") +facet_wrap(~on_farm_zone,scales="free_x") +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Location") + ylab("On-station fresh yield (t/ha)") 


ggsave("Results/cassava_nigeria/on_station_yield_location.png",on_station_data_gg,height = 4,width = 6)


on_station_data_gg_var <- on_station_data %>% ggplot(aes(x=accession_name,y=fyld))+
  geom_boxplot(fill="#009999")  +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Genotype") + ylab("On-station fresh yield (t/ha)") + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Results/cassava_nigeria/on_station_yield_var.png",on_station_data_gg_var,height = 4,width = 7)


on_station_data_gg_year <- on_station_data %>% ggplot(aes(x=factor(Year),y=fyld))+
  geom_boxplot(fill="#009999")  +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Planting year") + ylab("On-station fresh yield (t/ha)") + theme_bw()


ggsave("Results/cassava_nigeria/on_station_yield_location.png",on_station_data_gg_year,height = 4,width = 4.5)

on_station_data_gg_location_geno1 <- on_station_data %>% filter(location %in% c("Abuja"  , "Ago-Owu", "Ibadan" , "Ikenne",  "Kano" ,   "Mokwa")) %>% ggplot(aes(x=accession_name,y=fyld))+
  geom_boxplot(fill="#009999") +facet_grid(location~.) +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Genotype") + ylab("On-station fresh yield (t/ha)") + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("Results/cassava_nigeria/on_station_yield_location_geno_1.png",on_station_data_gg_location_geno1,height =7,width = 5)

on_station_data_gg_location_geno2 <- on_station_data%>% filter(location %in% c("Onne" ,   "Otobi"  , "Ubiaja" , "Umudike", "Zaria" )) %>% ggplot(aes(x=accession_name,y=fyld))+
  geom_boxplot(fill="#009999") +facet_grid(location~.) +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Genotype") + ylab("On-station fresh yield (t/ha)") + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("Results/cassava_nigeria/on_station_yield_location_geno_2.png",on_station_data_gg_location_geno2,height =6,width = 5)


data.frame(unique(on_farm_data$genotype))

on_station_data$accession_name <- gsub("TMS","TMS-",on_station_data$accession_name)

on_station_data$accession_name <- gsub("--","-",on_station_data$accession_name)

# No all the varieties tested on-farm were tested on-station

unique(on_station_data$accession_name) %in% unique(on_farm_data$genotype)

data.frame(unique(on_station_data$accession_name))

data.frame(unique(on_farm_data$genotype))


on_station_data$Year <- factor(on_station_data$Year)

on_station_data$accession_name <- factor(on_station_data$accession_name)

on_station_data$location <- factor(on_station_data$location)

dim(on_station_data)

table(on_station_data$accession_name,on_station_data$location,on_station_data$Year)

table(on_station_data$accession_name,on_station_data$location)

table(on_station_data$accession_name,on_station_data$Year)

write.csv(on_station_data,"Proccesed_data/cassava-nigeria/onstation_trial_casava.csv",row.names = F)

# Across all the experimental stations - zone

on_station_data <- on_station_data %>% mutate(accession_name=as.factor(accession_name),Year=as.factor(Year),location=as.factor(location))

on_station_data$rep_number <- factor(on_station_data$rep_number)

on_station_data1 <- on_station_data[on_station_data$on_farm_zone=="Within of on-farm zone",]

var_comp <- lmer(fyld~Year+Zone+(1|accession_name)+(1|accession_name:Zone:Year)+(1|Zone:rep_number)+(1|accession_name:Zone)+(1|Year:accession_name),data=on_station_data1) # 

lmer(fyld~Year+Zone+(1|accession_name)+(1|accession_name:Zone:Year)+(1|Zone:rep_number)+(1|accession_name:Zone)+(1|Year:accession_name),data=on_station_data1)



H2_cullis.calc<-function(mixmodel,var="genotype"){
  
  BLUPs=ranef(mixmodel)[,var]
  V_BLUP<-var(c(outer(unlist(BLUPs),unlist(BLUPs),"-")))
  VC=as.data.frame(VarCorr(mixmodel1))
  Vg<-VC$vcov[VC$grp=="genotype"] 
  H2_cullis=1-V_BLUP/(2* Vg)
  return(H2_cullis)	
  
}



summary(var_comp)

varCorrM0 <- data.frame(VarCorr(var_comp))

cov_estim <- varCorrM0[,c(1,4,5)]

#cov_estim <- varcomp(var_comp)

library(insight)

get_variance_fixed(var_comp)

vgly <- cov_estim[cov_estim$grp=="accession_name:Zone:Year","vcov"]
vgl <- cov_estim[cov_estim$grp=="accession_name:Zone","vcov"]
vgy <- cov_estim[cov_estim$grp=="Year:accession_name","vcov"]
vg <- cov_estim[cov_estim$grp=="accession_name","vcov"]
ve <- cov_estim[cov_estim$grp=="Residual","vcov"]

l <- length(unique(on_station_data$location))
y <- length(unique(on_station_data$Year))
r <- mean(table(on_station_data$accession_name))

vg/((vg+vgy/y+vgl/l+vgly/(y*l)+ve/(l*y*r)))

write.csv(round(cov_estim,2),"Results/variance_estimations_cassava_onstation.csv")

##-----


on_station_data$zone_year <- paste(on_station_data$location,sep='_')

tab_zone <- table(on_station_data$zone_year)

ls_on_station_data_her <- split(on_station_data,paste(on_station_data$zone_year,on_station_data$Year,sep="_"))


h2 <- lapply(ls_on_station_data_her,function(w){
  w <- w[c('fyld','rep_number','accession_name','Year',"location")]
  w <- w[complete.cases(w),]
  w <- w[complete.cases(w),]
  w <- droplevels(w)
    
  y = length(unique(w$Year))
  r = round(mean(table(w$accession_name)))
  if(y==1){
    mixMod <- lmer(fyld~(1|rep_number)+(1|accession_name),data=w) 
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    se = df[3,5]
    sr = df[2,5]
    sg = df[1,5]

    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+se^2/r)
    h2_2 <- sg^2/(sg^2+se^2)
    
  }else{
    mixMod <- lmer(fyld~(1|accession_name)+(1|Year)+(1|accession_name:Year),data=w) 
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    se = df[4,5]
    sg = df[2,5]
    sy = df[3,5]
    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+sy/y+se^2/(y*r))
    h2_2 <- sg^2/(sg^2+sy^2+se^2)
  }
  

  
  print(r)

  data.frame(location=unique(w$location),sg,sr,se,h2_1=round(h2_1,2),h2_2=round(h2_2,2),n=nrow(w),years=paste(sort(unique(w$Year)),collapse = "-"))
})

do.call(rbind,h2)

#----------------------------------------------------------------------------
#  

on_station_data1 <- on_station_data[on_station_data$on_farm_zone=="Within of on-farm zone",]

unique(on_station_data1[c('Zone','zone_year')])

on_station_data1$zone_year <- paste(on_station_data1$location,sep='_')

tab_zone <- table(on_station_data1$zone_year)

ls_on_station_data_her <- split(on_station_data1,paste(on_station_data1$Zone,on_station_data1$Year,sep="_"))


h2 <- lapply(ls_on_station_data_her,function(w){
  w <- w[c('fyld','rep_number','accession_name','Year',"location")]
  w <- w[complete.cases(w),]
  w <- w[complete.cases(w),]
  w <- droplevels(w)
  
  y = length(unique(w$Year))
  r = round(mean(table(w$accession_name)))
  if(y==1){
    mixMod <- lmer(fyld~(1|rep_number)+(1|accession_name),data=w) 
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    se = df[3,5]
    sr = df[2,5]
    sg = df[1,5]
    
    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+se^2/r)
    h2_2 <- sg^2/(sg^2+se^2)
    
  }else{
    mixMod <- lmer(fyld~(1|accession_name)+(1|Year)+(1|rep_number)+(1|accession_name:Year),data=w) 
    print(summary(mixMod))
    df <- data.frame(VarCorr(mixMod))
    se = df[4,5]
    sg = df[2,5]
    sy = df[3,5]
    #print(if(se!=0){sg^2/(sg^2+se^2/r)}else{NULL})
    h2_1 <- sg^2/(sg^2+sy/y+se^2/(y*r))
    h2_2 <- sg^2/(sg^2+sy^2+se^2)
  }
  
  
  
  print(r)
  
  data.frame(location=unique(w$location),sg,sr,se,h2_1=round(h2_1,2),h2_2=round(h2_2,2),n=nrow(w),years=paste(sort(unique(w$Year)),collapse = "-"))
})

do.call(rbind,h2)


# Genetic gain 

on_station_data1$Zone <- factor(on_station_data1$Zone )

#var_comp <- lmer(fyld~Year+Zone+(1|accession_name)+(1|accession_name:Zone:Year)+(1|Zone:rep_number)+(1|accession_name:Zone)+(1|Year:accession_name),data=on_station_data1) # 

var_comp_gen <- lmer(fyld~accession_name+Zone+Year+ +
                       (1|accession_name:Zone)+(1|accession_name:Year)+
                       (1|accession_name:Year:Zone),data=on_station_data1) # I removed year:zone effect after cause problems

means <- predictmeans(var_comp_gen,"accession_name",barplot=FALSE, pplot=F, bkplot=F, plot=F)

write.csv(means$mean_table,"Results/cassava_nigeria/datasets/genotypic_means_onstation.csv")

#

# ---------------------------------------------------------------------

# Genetic gain 

var_comp_gen <- lmer(fyld~accession_name+location+Year+(1|location:rep_number) +
                       (1|accession_name:location)+(1|accession_name:Year)+
                       (1|accession_name:Year:location),data=on_station_data) # I removed year:zone effect after cause problems

means <- predictmeans(var_comp_gen,"accession_name",barplot=FALSE, pplot=F, bkplot=F, plot=F)

write.csv(means$mean_table,"Results/cassava_nigeria/genotypic_means_onstation.csv")

# Model restringed

var_comp_gen2 <- lmer(fyld~accession_name+location+Year+ +
                       (1|accession_name:location)+(1|accession_name:Year)+
                       (1|accession_name:Year:location),
                     data=on_station_data[on_station_data$on_farm_zone == "Within of on-farm zone" & on_station_data$Year!=2019,] ) # I removed year:zone effect after cause problems

means <- predictmeans(var_comp_gen2,"accession_name",barplot=FALSE, pplot=F, bkplot=F, plot=F)

write.csv(means$mean_table,"Results/cassava_nigeria/genotypic_means_onstation_whithin_on-farm.csv")




means <- means$mean_table[c('accession_name','Mean')]

names(means)[1] <- "genotype" 

mean_release <- means %>% inner_join(release_data,by=c('genotype'='genotype'))

mean_release$Release.year <- as.numeric(mean_release$Release.year)

coefgg <- lm(Mean~Release.year,data=mean_release)

summary(coefgg)$coefficients

release_data <- release_data[order(release_data$Release.year),]

mean_release$Genotype <- factor(mean_release$genotype,levels = release_data$genotype)


geneticg <- ggplot(mean_release,aes(x=Release.year,y=Mean ))+geom_point(aes(colour=Genotype),size=3)+
  geom_smooth(method = 'lm') +
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = y~x,parse=T
  )+theme_bw()+ylab("Genotypic means of on-station fresh yield (t/ha)")+xlab("Release year")

geneticg

ggsave('Results/cassava_nigeria/genetic_gain_cassava_fresh_onstation_yield.png',geneticg,width = 5,height = 5)

# by state

ls_state <- split(on_station_data1,paste0(on_station_data1$Zone))

gg <- lapply(ls_state, function(x){
  
  print(unique(x$Zone))
  if(length(unique(x$Year)==1)){
    mixmodel0 <- lmer(fyld~ accession_name+(1|rep_number),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    print(mixmodel0)
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'accession_name',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table)
    
    #genotype.means <- genotype.means %>% inner_join(release_data,by=c('accession_name'="genotype"))
    
    #genotype.means$Release.year <- as.numeric(genotype.means$Release.year)
    
    #lm.gg <- lm(Mean~Release.year,data=genotype.means)
    #print(unique(x$location))
    #print(summary(lm.gg)$coefficients)
    
    data.frame(genotype.means,zone=unique(x$Zone))
  }else{
  
  mixmodel0 <- lmer(fyld~ accession_name+ (1|rep_number) + 
                      (1|Year)+(1|accession_name:Year),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
  
  print(mixmodel0)
  
  summary(mixmodel0)
  
  genotypeMeans <- predictmeans(mixmodel0,'accession_name',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
  
  genotypeMeans
  
  genotype.means <- data.frame(genotypeMeans$mean_table)
  
  #genotype.means <- genotype.means %>% inner_join(release_data,by=c('accession_name'="genotype"))
  
  #genotype.means$Release.year <- as.numeric(genotype.means$Release.year)
  
  #lm.gg <- lm(Mean~Release.year,data=genotype.means)
  #print(unique(x$location))
  #print(summary(lm.gg)$coefficients)
  
  data.frame(genotype.means,zone=unique(x$Zone))
  }
  # library(ggpubr)
  # 
  # 
  # lm.gg <- lm(Mean~Release.year,data=genotype.means)
  # slgg <- summary(lm.gg)
  # print(summary(lm.gg))
  # 
  # data.frame(t(round(lm.gg$coefficients,4)),R2=round(slgg$r.squared,4))
  
}

)

library(ggpmisc) 

complete_gg <- do.call(rbind,gg)

write.csv(complete_gg,"Results/cassava_nigeria/datasets/locations_onstation_means.csv")

## ggg 222



names(complete_gg)[1] <- "genotype"

complete_gg$Genotype <- factor(complete_gg$genotype,levels = release_data$genotype)

geneticg2 <- ggplot(complete_gg,aes(x=Release.year,y=Mean ))+geom_point(aes(colour=genotype))+
  geom_smooth(method = 'lm') +
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),formula = y ~ x,
    parse = TRUE)+
  facet_wrap(~state,nrow = 4,ncol=3)+theme_bw()+ylab("Genotypic means of on-station fresh yield (t/ha)")+xlab("Release year")

geneticg2

ggsave('Results/cassava_nigeria/genetic_gain_cassava_fresh_yield_loc_onstation_loc.png',geneticg2,width = 9.5,height = 7)


# gggge2222

on_station_data2 <- on_station_data[on_station_data$on_farm_zone!="Within of on-farm zone",]

ls_state <- split(on_station_data,on_station_data$Zone)

gg2 <- lapply(ls_state, function(x){
  
  if(length(unique(x$Year)==1)){
    mixmodel0 <- lm(fyld~ accession_name,data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    print(mixmodel0)
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'accession_name',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table[,1:2])
    
    #genotype.means <- genotype.means %>% inner_join(release_data,by=c('accession_name'="genotype"))
    
    #genotype.means$Release.year <- as.numeric(genotype.means$Release.year)
    
    #lm.gg <- lm(Mean~Release.year,data=genotype.means)
    #print(unique(x$location))
    #print(summary(lm.gg)$coefficients)
    
    data.frame(genotype.means,state=unique(x$Zone))
  }else{
    
    mixmodel0 <- lmer(fyld~ accession_name+  
                        (1|Year)+(1|accession_name:Year),data=x)# Genotype by year interacton zero and was removed ,data=x,na.action = 'na.omit' )
    
    print(mixmodel0)
    
    summary(mixmodel0)
    
    genotypeMeans <- predictmeans(mixmodel0,'accession_name',mplot=F, barplot=FALSE, pplot=F, bkplot=F, plot=F)
    
    genotypeMeans
    
    genotype.means <- data.frame(genotypeMeans$mean_table[,1:2])
    
    #genotype.means <- genotype.means %>% inner_join(release_data,by=c('accession_name'="genotype"))
    
    #genotype.means$Release.year <- as.numeric(genotype.means$Release.year)
    
    #lm.gg <- lm(Mean~Release.year,data=genotype.means)
    #print(unique(x$location))
    #print(summary(lm.gg)$coefficients)
    
    data.frame(genotype.means,state=unique(x$Zone))
  }
  # library(ggpubr)
  # 
  # 
  # lm.gg <- lm(Mean~Release.year,data=genotype.means)
  # slgg <- summary(lm.gg)
  # print(summary(lm.gg))
  # 
  # data.frame(t(round(lm.gg$coefficients,4)),R2=round(slgg$r.squared,4))
  
}

)

library(ggpmisc) 

complete_gg <- do.call(rbind,gg2)

write.csv(complete_gg,"Results/cassava_nigeria/on-station_means_localities.csv")

# Across all the experimental stations - location

var_comp_loc <- lmer(fyld~(1|accession_name)+(1|Year)+(1|location)+(1|accession_name:location)+(1|Year:location),data=on_station_data) # 

var_comp_gen_loc <- lmer(fyld~accession_name+(1|Year)+(1|location)+(1|accession_name:location)+(1|Year:location),data=on_station_data) # I removed year:zone effect after cause problems

means_loc <- predictmeans(var_comp_gen_loc,"accession_name")

means_loc <- means_loc$mean_table[c('accession_name',  'Mean')]

names(means_loc)[1] <- "genotype" 

mean_release_loc <- means_loc %>% inner_join(release_data,by=c('genotype'='genotype'))

mean_release_loc$Release.year <- as.numeric(mean_release_loc$Release.year)

ggplot(mean_release_loc,aes(y=Mean,x=Release.year))+geom_point()+geom_smooth(method = 'lm')+theme_bw()+xlab("Release year")

summary(lm(Mean~Release.year,data=mean_release_loc))

# Across experimental station with location shared with on farm trials

on_station_data_onfarm <- on_station_data[on_station_data$on_farm_zone=="Within of on-farm zone",]

var_comp_onfarm <- lmer(fyld~(1|accession_name)+(1|Year)+(1|Zone)+(1|accession_name:Zone)+(1|Year:Zone),data=on_station_data_onfarm) # 

var_comp_gen_onfarm<- lmer(fyld~accession_name+(1|Year)+(1|Zone)+(1|accession_name:Zone)+(1|Year:Zone),data=on_station_data_onfarm) # I removed year:zone effect after cause problems

means_loc_onfarm <- predictmeans(var_comp_gen_onfarm,"accession_name")

means_loc_onfarm <- means_loc_onfarm$mean_table[c('accession_name',  'Mean')]

names(means_loc_onfarm)[1] <- "genotype" 

mean_release_loc_onfarm <- means_loc_onfarm %>% inner_join(release_data,by=c('genotype'='genotype'))

mean_release_loc_onfarm$Release.year <- as.numeric(mean_release_loc_onfarm$Release.year)

ggplot(mean_release_loc_onfarm,aes(y=Mean,x=Release.year))+geom_point()+geom_smooth(method = 'lm')+theme_bw()+xlab("Release year")

summary(lm(Mean~Release.year,data=mean_release_loc_onfarm))

# Across experimental station with location shared with on farm trials 2021

#----------------- No possible


