
# 

on_station_data_gg <- long_farm_data %>% ggplot(aes(x=ADM1_NAME2,y=PY_ton_ha))+
  geom_boxplot(fill="#009999")+theme_bw()   +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  
  #+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  xlab("Location") + ylab("On-station yield (t/ha)") 

ggsave("Results/groundnut_tanzania/on_station_yield_groundnut_location_1.png",on_station_data_gg,height = 4,width = 6)


namVar <- names(sort(tapply(long_farm_data$PY_kg_ha,long_farm_data$GENOTYPE,mean,na.rm=T),decreasing = T))

long_farm_data$GENOTYPE2 <- factor(long_farm_data$GENOTYPE,levels = namVar)

on_station_data_gg_var <- long_farm_data[!is.na(long_farm_data$GENOTYPE2),] %>% ggplot(aes(x=GENOTYPE2,y=PY_ton_ha))+
  geom_boxplot(fill="#009999")  +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Genotype") + ylab("On-station yield (t/ha)") + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Results/groundnut_tanzania/on_station_yield_var.png",on_station_data_gg_var,height = 4,width = 8)

long_farm_data <- long_farm_data %>% separate(datasetnam,c("DS",NA,"Year"),sep="_")

long_farm_data$Year <- as.numeric(long_farm_data$Year)

long_farm_data_complete <- long_farm_data[complete.cases(long_farm_data[c('ADM1_NAME','PY_ton_ha','Year')]),]

table(long_farm_data_complete$GENOTYPE,long_farm_data_complete$Year)

table(long_farm_data_complete$GENOTYPE,long_farm_data_complete$ADM1_NAME)

nrow(long_farm_data_complete)

length(unique(long_farm_data_complete$Year))

length(unique(long_farm_data_complete$ADM1_NAME))

length(unique(long_farm_data_complete$GENOTYPE))

lg_loc <- unique(long_farm_data_complete[c("SITE","ADM1_NAME")])

lg_loc <- lg_loc[order(lg_loc$ADM1_NAME,decreasing = T),]

tab_loc<- sort(tapply(long_farm_data$PY_ton_ha, long_farm_data$ADM1_NAME, mean,na.rm=T),decreasing = T)

long_farm_data$ADM1_NAME2 <- factor(long_farm_data$ADM1_NAME,levels = names(tab_loc))


on_station_data_gg_loc <- long_farm_data %>% ggplot(aes(x=factor(ADM1_NAME2),y=PY_ton_ha))+
  geom_boxplot(fill="#009999")  +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Location") + ylab("On-station yield (t/ha)") + theme_bw()


ggsave("Results/groundnut_tanzania/on_station_state_yield.png",on_station_data_gg_loc,height = 4,width = 4.5)



on_station_data_gg_year <- long_farm_data %>% ggplot(aes(x=factor(Year),y=PY_ton_ha))+
  geom_boxplot(fill="#009999")  +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Planting year") + ylab("On-station yield (t/ha)") + theme_bw()


ggsave("Results/groundnut_tanzania/on_station_yield_year.png",on_station_data_gg_year,height = 4,width = 4.5)

on_station_data_gg_location_geno1 <- long_farm_data[!is.na(long_farm_data$GENOTYPE2),] %>% filter(ADM1_NAME2 %in% c('Mwanza', 'Kigoma',  'Lindi', 'Dodoma', 'Mtwara')) %>% ggplot(aes(x=GENOTYPE,y=PY_ton_ha))+
  geom_boxplot(fill="#009999") +facet_grid(ADM1_NAME2~.) +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Genotype") + ylab("On-station yield (t/ha)") + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("Results/groundnut_tanzania/on_station_yield_location_geno_1.png",on_station_data_gg_location_geno1,height =7,width = 9)

on_station_data_gg_location_geno2 <- long_farm_data[!is.na(long_farm_data$GENOTYPE2),] %>% filter(ADM1_NAME2 %in% c('Tabora', 'Ruvuma',  'Pwani',  'Mbeya' )) %>% ggplot(aes(x=GENOTYPE,y=PY_ton_ha))+
  geom_boxplot(fill="#009999") +facet_grid(ADM1_NAME2~.) +#+scale_colour_manual(values = c("N"="black", "Y"="blue"))
  theme_bw() + xlab("Genotype") + ylab("On-station yield (t/ha)") + theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("Results/groundnut_tanzania/on_station_yield_location_geno_2.png",on_station_data_gg_location_geno2,height =6,width = 9)


data.frame(unique(long_farm_data$GENOTYPE))


# No all the varieties tested on-farm were tested on-station

# unique(on_station_data$accession_name) %in% unique(on_farm_data$genotype)




table(long_farm_data$GENOTYPE,long_farm_data$ADM1_NAME2,long_farm_data$Year)

table(long_farm_data$GENOTYPE,long_farm_data$ADM1_NAME2)

table(long_farm_data$GENOTYPE,long_farm_data$Year)