####################################################
###### SCRIPT TO DEBUG PRUEBA3 BASELINE
# Kaue de Sousa
# Updated 10Oct2018
####################################################


# This script still needs some updates on the R project structure


#....................................................
#....................................................
# Packages ####
library("tidyverse")
library("magrittr")
library("rjson")
library("jsonlite")
library("sp")
library("maptools")
library("rgdal")
library("reshape2")

#....................................................
#....................................................
# Read survey data ####
mydata <- fromJSON("data/raw/Prueba3_Baseline_Clean_JSON.json")

# change name of id variable
names(mydata)[4] <- "id"

# Subset variables of baseline into groups
geo <- mydata[,c("id","store_gps:Latitude","store_gps:Longitude","store_gps:Altitude","store_gps:Accuracy")]

social <- mydata[,c("id","local_currency","pueblo","educacion_entrevistado",
                      "sexo_entrevistado","posicion_en_hogar","jefe_hogar","nombre_jh","edad_jhh","edad_jhm",
                      "educacion_jhm","fiabilidad","inscripcion_escolar","edad_entrevistado",
                      "posicion_hogar_alt","nombre_entrevistado",
                      "hpersons","children","malesunder7","femalesunder7","males7to13","females7to13",
                      "youth","males14to19","females14to19","adults","malesover20","femalesover20",
                      "material_casa","refri","estufa","plancha","piedra","celular",
                      "telefono_cercano")]

bean_know <- mydata[,c("id","frijoles_conocimiento_nombres",
                         "frijoles_conocimiento_conteo","frijoles_conocimiento_otro1", "frijoles_conocimiento_otro2",
                         "frijoles_conocimiento_otro3","frijoles_conocimiento_otro4",
                         "comprado_semillas","comprar_semillas_numero","comprar_semillas_nombres","compra_anual",
                         "comprar_semillas_otro1","comprar_semillas_otro2","comprar_semillas_otro3",
                         "recibido_semillas","recibido_semillas_numero","recibido_semillas_nombres","recibido_semillas_otro1",
                         "recibido_semillas_fuente","recibido_otra_fuente_vecino","recibido_semillas_persona",
                         "vender_semillas","vender_semillas_numero","venta_anual","vender_semillas_nombres",
                         "vender_semillas_otro1","vender_semillas_donde","vender_semillas_donde_otro",
                         "reglar_semilla_genero","regalado_semillas_numero",
                         "regalado_semillas","regalado_semillas_otro1",
                         "donacion_anual","rechazar_semillas","rechazar_semillas_opt",
                         "rechazar_semillas_nombres","rechazar_semilla_gen","rechazar_otros","rechazar_semillas_otro1",
                         "rechazar_semillas_otro2","rechazar_semillas_otro3","intercambio_semillas",
                         "informacion_opt","pruebas_opt","vecinos_opt")]

bean_prod <- mydata[,c("id","cultivo_frijol","bean_use","bean_who_works","bean_who_control_revenue",
                         "periodos_cultivo","area_primera_2014","cosecha_primera_2014","area_segunda_2014",
                         "cosecha_segunda_2014","area_apante_2015","cosecha_apante_2015",
                         "area_primera_2015","cosecha_primera_2015_SN","fertilise_beans","fertiliser_type",
                         "fertiliser_type_other", "fertiliser_amount", "fertiliser_units",
                         "fertiliser_type","fertiliser_type_other","tierra_unidades","tierra_unidades2",
                         "cuantos_metros","unidad_metrica","tierra_produccion","tierra_posesion",
                         "tierra_alquiler1","tierra_alquiler1_area","tierra_alquiler2","tierra_alquiler2_area",
                         "tarea_riego","tierra_posesion_area","bean_residue_use","bean_residue_sell",
                         "bean_residue_burn","bean_residue_feed","bean_residue_soil","bean_residue_compost",
                         "bean_residue_fuel","manure_beans","consumo_semanal","bean_consume_control")]

other_crop <- mydata[,c("id","crops","crop_count","crops_other1",
                          "crops_other2","crops_other3","crop_repeat_count",
                          "crop_repeat")]

livestock <- mydata[,c("id","livestock","livestock_owners","livestock_count",
                         "livestock_repeat_count","livestock_other1","livestock_other2",
                         "livestock_otherpoultry","livestock_repeat")]

food <- mydata[,c("id","foodshortagetime","foodshortagetime_months_amount",
                    "foodshortagetime_months_which","food_worry","food_notpreferredfood",
                    "food_limitvariety","food_eatnastyfood","food_smallermeals","food_skipmeals",
                    "food_nofoodinhouse","food_sleephungry","food_24hrsnofood","gather_wildfoods",
                    "wildfoods","wildfood_collect_when","wildfood_amount","wildfood_amount",
                    "Legumes","Legumes_goodseason","Legumes_source","Grains","Grains_goodseason",
                    "Grain_source","Roots_tubers","Roots_tubers_goodseason","Roots_tubers_source",
                    "Vegetables","Vegetables_goodseason","Vegetables_source","Fruits","Fruits_goodseason",
                    "Fruit_source","Meat","Meat_goodseason","Meat_source","Fish","Fish_goodseason","Fish_source",
                    "Eggs","Eggs_goodseason","Eggs_source","Milk_Dairy","Milk_Dairy_goodseason","Milk_Dairy_source",
                    "Fats","Fats_goodseason","Fats_source","Sweets","Sweets_goodseason","Sweets_source","Other",
                    "Other_goodseason","Other_source")]

aid <- mydata[,c("id","aidreceived","aidtypes", "aid_food_what","aid_food_quantity","aid_food_units",
                   "animal_aid_received","animal_aid_1_outcome","animal_aid_1","animal_aid_1_amount",
                   "animal_aid_2_outcome","animal_aid_2","animal_aid_2_amount","animal_aid_3","animal_aid_3_amount",
                   "animal_aid_3_outcome","aid_other")]

income <- mydata[,c("id","offfarm_incomes_any",
                      "offfarm_incomes","offfarm_income_repeat","offfarm_incomes_other",
                      "offfarm_incomes_count","offfarm_income_repeat_count",
                      "offfarm_total_income_proportion","debts_have", "debts_proportionofincome",
                      "debts_worry", "debts_to_who","debt_owner_other")]


#....................................................
#....................................................
# Clean geo info ####

# rename the variables
names(geo) <- c("id", "lat","lon","altitude","gps_accuracy")

# add comunity info
df <- read_csv("data/raw/AleatorizacionFams9-9-2015ConSemillas.csv")

names(df)[names(df)=="UniqueCode"] <- "id"

geo <- merge(geo, 
             df[,c("id","Comunidad","Organizacion","CodigoEntrevista")], 
             by="id", 
             all.x = TRUE)

geo$Comunidad <- as.factor(geo$Comunidad)

#replace missing lat lon by the mean of each comunity
for (i in unique(geo$Comunidad)){
  geo$lat <- ifelse(geo$Comunidad == i & is.na(geo$lat),
                       mean(geo$lat[geo$Comunidad == i ], na.rm=TRUE), 
                       geo$lat)
  geo$lon <- ifelse(geo$Comunidad == i & is.na(geo$lon),
                       mean(geo$lon[geo$Comunidad == i ], na.rm=TRUE), 
                       geo$lon)
}


#following lines had no previus geographic information (I included information from web)
for (i in c("Cumbre de La Arada", "La Ceiba, El Tesoro", "Ticanlu")){
  geo$lat <- ifelse(geo$Comunidad== i & is.na(geo$lat),
                       14.819444, geo$lat) 
  geo$lon <- ifelse(geo$Comunidad== i & is.na(geo$lon),
                       -89.390278, geo$lon)
}

sum(is.na(geo[,c("lon","lat")]))

# add global adminstrative unit information
#source http://gadm.org/ 
myproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

dsn <- "data/shapefile"

adm <- readOGR(dsn = dsn, layer = "latin_america_adm3")
xy <- sp::SpatialPoints(geo[,c("lon","lat")], proj4string=CRS(myproj))
adm <- sp::over(xy, adm)
adm <- adm[,c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME")]



#bind coluns
geo <- cbind(geo, adm)

#remove unsued factors 
geo[9:11] <- lapply(geo[9:11], as.character)
geo[9:11] <- lapply(geo[9:11], as.factor)


# #add SRTM (altitude) info
# #http://srtm.csi.cgiar.org/
# file <- paste0(ras,  "/srtm")
# if(dir.exists(file)){
#   srtm <- raster::stack(paste0(file, "./srtm_19_10.tif"))
#   geo$altitude <-raster::extract(srtm, geo[,c("lon", "lat")])
# } else {
#   geo$altitude <- NA
#   warning("Raster with elevation data not found")
# }

#remove temporary values data and functions
rm(xy, adm, srtm, file)

# Social subset

### replace currency name into standarized ISO currency code
social$local_currency <- as.factor(as.character(ifelse(social$local_currency=="quetzales", "GTQ",
                                                       ifelse(social$local_currency=="lempiras", "HNL",
                                                              "USD"))))

## indentify and replace/remove outliers in household information ##

#head of the household 
var <- "edad_jhh"
kl <- length(unique(boxplot.stats(social[,var])$out))>0
while(kl){
  out <- unique(boxplot.stats(social[,var])$out)
  
  social[,var] <- ifelse(social[,var] %in% out, NA, social[,var])
  
  kl <- length(unique(boxplot.stats(social[,var])$out))>0
}


#household women age
var <- "edad_jhm"
kl <- length(unique(boxplot.stats(social[,var])$out))>0
while(kl){
  out <- unique(boxplot.stats(social[,var])$out)
  
  social[,var] <- ifelse(social[,var] %in% out, NA, social[,var])
  
  kl <- length(unique(boxplot.stats(social[,var])$out))>0
}

#gender
social$sexo_entrevistado <- ifelse(is.na(social$sexo_entrevistado) &
                                    social$nombre_entrevistado=="Juana Domitila Hern?ndez Morales de Vides" |
                                     social$nombre_entrevistado=="Benedicta Jacome Osorio" |
                                     social$nombre_entrevistado=="Ventura Rosales Quincin" |
                                     social$nombre_entrevistado=="Romilia de Jesus Crisostomo Guerra de Campos" |
                                     social$nombre_entrevistado=="Berta Leiva L?pez" |
                                     social$nombre_entrevistado=="Lucrecia Elizabeth Lemus Espino de G?mez",
                                   "Female", social$sexo_entrevistado)
social$sexo_entrevistado <- ifelse(is.na(social$sexo_entrevistado), "Male", social$sexo_entrevistado)
social$sexo_entrevistado[social$sexo_entrevistado=="HOMBRE"] <- "Male"
social$sexo_entrevistado[social$sexo_entrevistado=="MUJER"] <- "Female"
social$sexo_entrevistado <- as.factor(social$sexo_entrevistado)

#education
social$educacion_entrevistado <- as.character(social$educacion_entrevistado)
social$educacion_entrevistado <- as.factor(ifelse(social$educacion_entrevistado=="edu1","Illiterate", #cannot read and write
                                         ifelse(social$educacion_entrevistado=="edu2","Literate", #can read and write
                                                ifelse(social$educacion_entrevistado=="edu3","Elementary", #have gone and concluded primary school
                                                       ifelse(social$educacion_entrevistado=="edu4","Secondary", #have gone and concluded secondary school
                                                              ifelse(social$educacion_entrevistado=="edu5","Undergraduate", #have gone and concluded college
                                                                     social$educacion_entrevistado))))))
summary(social$educacion_entrevistado)

#household position
social$posicion_en_hogar <- as.character(social$posicion_en_hogar)

hh_positon <- data.frame(x=c("Abuela","Hija","Hijo", "hombrejh","mujerjh","Nuera","otro","Yerno"),
                         y=c("Grandmother", "Daughter","Son","Head of household - man","Head of household - woman","Daughter-in-law","Other","Son-in-law"))

for (i in 1:nrow(hh_positon))  {
  social$posicion_en_hogar <- ifelse(social$posicion_en_hogar==as.character(hh_positon[i,1]),
                                     as.character(hh_positon[i,2]),
                                     social$posicion_en_hogar)
}

social$posicion_en_hogar <- as.factor(social$posicion_en_hogar)
summary(social$posicion_en_hogar)

#head of household status
social$jefe_hogar <- as.character(social$jefe_hogar)
social$jefe_hogar <- as.factor(ifelse(social$jefe_hogar=="jefe1","Married couple",
                                      ifelse(social$jefe_hogar=="jefe2","Only woman",
                                             ifelse(social$jefe_hogar=="jefe3","Only man",
                                                    ifelse(social$jefe_hogar=="jefe4","Other",
                                                           social$jefe_hogar)))))
summary(social$jefe_hogar)
#household woman education
social$educacion_jhm <- as.character(social$educacion_jhm)
social$educacion_jhm <- as.factor(ifelse(social$educacion_jhm=="edu1","Illiterate", #cannot read and write
                                         ifelse(social$educacion_jhm=="edu2","Literate", #can read and write
                                                ifelse(social$educacion_jhm=="edu3","Elementary", #have gone and concluded primary school
                                                       ifelse(social$educacion_jhm=="edu4","Secondary", #have gone and concluded secondary school
                                                              ifelse(social$educacion_jhm=="edu5","Undergraduate", #have gone and concluded college
                                                                     social$educacion_jhm))))))
summary(social$educacion_jhm)

#reliability
#into likert scale 
social$fiabilidad <- as.character(social$fiabilidad)
social$fiabilidad <- as.integer(ifelse(social$fiabilidad=="fiable1",1,#"Not reliable", 
                                         ifelse(social$fiabilidad=="fiable2",2,#"Not very reliable", 
                                                ifelse(social$fiabilidad=="fiable3",4,#"Very reliable",
                                                       ifelse(social$fiabilidad=="fiable4",3,#"Reliable", 
                                                              ifelse(social$fiabilidad=="fiable5",5,#"High reliable", 
                                                                     social$fiabilidad))))))
summary(as.factor(social$fiabilidad))

#school registration
social$inscripcion_escolar <- as.factor(as.character(ifelse(social$inscripcion_escolar=="NO","No",
                                                            ifelse(social$inscripcion_escolar=="SI", "Yes",
                                                                   social$inscripcion_escolar))))
summary(social$inscripcion_escolar)

#interviewed age
var <- "edad_entrevistado"
kl <- length(unique(boxplot.stats(social[,var])$out))>0
while(kl){
  out <- unique(boxplot.stats(social[,var])$out)
  
  social[,var] <- ifelse(social[,var] %in% out, NA, social[,var])
  
  kl <- length(unique(boxplot.stats(social[,var])$out))>0
}

#### verify if given information about TOTALhpersons is true
boxplot.stats(social$hpersons)$out
social$malesover20 <- ifelse(social$malesover20==31, 1, social$malesover20)
#replace NA's to 0
social[18:28][is.na(social[18:28])] <- 0
str(social[,18:28])

social$hperson <- rowSums(social[,c(19:22,24:25,27:28)])


social$children <- as.factor(ifelse(social$children=="Y", "Yes", "No"))
social$youth <- as.factor(ifelse(social$youth =="Y", "Yes", "No"))
social$adults <- as.factor(ifelse(social$adults =="Y", "Yes", "No"))

#what is the main material of the floors of the house?
summary(social$material_casa)
social$refri <- as.factor(ifelse(social$refri=="SI", "Yes", "No"))
summary(social$refri)
social$estufa <- as.factor(ifelse(social$estufa=="SI", "Yes", "No"))
summary(social$estufa)
social$plancha <- as.factor(ifelse(social$plancha=="SI","Yes","No"))
summary(social$plancha)
social$piedra <- as.factor(ifelse(social$piedra=="SI","Yes","No"))
summary(social$piedra)
social$celular <- as.factor(ifelse(social$celular=="SI","Yes","No"))
summary(social$celular)
social$telefono_cercano <- as.factor(ifelse(social$telefono_cercano=="SI","Yes","No"))
summary(social$telefono_cercano)


#####  add indicators HHdat info ####
load(here(wd, "./input/PruebaCorta_IndicatorResults.RData"))
names(HHdat) #### R working space from Mark van Vijk
colnames(HHdat)[colnames(HHdat)=="codigo_hogar"] <- "id"

##### add indicators from HHdat 
## As the indicators has no unique id, I merged the information 
#considering that the rows in HHdat are the same in the indicator vector
indicators <- as.data.frame(cbind(score_HDDS_bad, score_HDDS_good, score_HFIAS, score_PPI))
names(indicators) <- c("HDDS_bad","HDDS_good", "HFIAS","PPI")
social <- cbind(social, indicators)
names(social)
#remove temporary datasets, values and functions
rm(indicators)
rm(score_PPI)
rm(score_HDDS_bad)
rm(score_HDDS_good)
rm(score_HFIAS)
rm(HFIAS_grouping)
rm(HHdat)

##### TREATMENTS maintainence in project
#information provided by Jose Gabriel Suchini through e-mail in 24may2016
#datasets were previous debuged manually 
reg_TREAT <- social[,c(1:10)]
evaluacion_ECA <- read.csv(here(wd, "input/EVALUACIONES_SPV_ECAS_FRIJOL_TRIFINIO_15jun2016.csv"))
names(evaluacion_ECA)[names(evaluacion_ECA)=="id_household"] <- "id"

reg_EPM <- read.csv(here(wd, "./input/CONTROL_DE_LLAMADAS_POR_FAMILIA_EPM_25may2016.csv"))
names(reg_EPM)[names(reg_EPM)=="id_household"] <- "id"
#get some information from randomization 
rand_treat <- read.csv(here(wd, "./input/AleatorizacionFams7-9-2015ConSemillasTOTAL2.csv"))
names(rand_treat)[2] <- "id"
rand_treat <- rand_treat[,c("id","TRAT")]
#check EPM info with ClimMob data
#some of EPM farms where not shown in the first list "reg_EPM", so I completed the list 
#with Climmob dataset 
climmob <- read.csv(here(wd, "./input/CLIMmob_14jun2016.csv"))
climmob$TREAT_clim <- as.factor("EPM")
names(climmob)[names(climmob)=="id_household"] <- "id"

#add information
reg_TREAT <- merge(rand_treat, reg_TREAT, by="id", all.y=TRUE)
reg_TREAT <- merge(climmob[,c("id","TREAT_clim")], reg_TREAT, by="id", all.y=T)

#create variable for maintainance in the project
evaluacion_ECA$TRAT_ECA <- as.factor("ECA")
evaluacion_ECA$farmer_participation <- as.factor(paste(evaluacion_ECA$farmer_participation_ECA3,
                                      evaluacion_ECA$farmer_participation_ECA4,
                                      evaluacion_ECA$farmer_participation_ECA5,
                                      sep="-"))
#if farmers participated in at least 1 section 
#they count as "In" the project, the contrary they count as "Out"
evaluacion_ECA$farmer_participation <- as.factor(ifelse(evaluacion_ECA$farmer_participation=="No-No-No", 
                                                   "Out",
                                                 "In"))
#join information from PVS (ECA) to TREAT dataset
reg_TREAT <- merge(evaluacion_ECA[,c("id", "TRAT_ECA", "farmer_participation")], 
                   reg_TREAT, all.y=TRUE)

#join information from CCI (EPM) to TREAT dataset
names(reg_EPM)
reg_TREAT <- merge(reg_EPM[,c("id","TRAT","confidence")],
                   reg_TREAT, by="id", all.y=T)
### standarise treatment info
names(reg_TREAT)
reg_TREAT$TREAT <- as.factor(paste(reg_TREAT$TRAT,reg_TREAT$TRAT.x,
                         reg_TREAT$TRAT.y, reg_TREAT$TREAT_clim, sep="-"))

summary(reg_TREAT$TREAT)

reg_TREAT$TREAT <- as.factor(ifelse(reg_TREAT$TREAT=="-NA-ECA-NA", "PVS",
                                    ifelse(reg_TREAT$TREAT=="-NA-NA-NA", "Control", 
                                           "CCI")))
summary(reg_TREAT$TREAT)

#debug information of maintainance in project 
#only farms from Climmob with confidence 3 or 2 are considered as "In" for CCI treatment 
#the other CCI farms are classified as "Out"
#criteria for PVS was mentioned above
#merge, updated colunm of maintainance in EPM with state of maintainance from ECA 
reg_TREAT$TREAT_status <- as.factor(paste(reg_TREAT$farmer_participation,
                                         reg_TREAT$confidence,
                                         reg_TREAT$TREAT, sep="-"))

summary(reg_TREAT$TREAT_status)
reg_TREAT$TREAT_status <- as.factor(ifelse(reg_TREAT$TREAT_status=="In-NA-PVS" |
                                            reg_TREAT$TREAT_status=="NA-2-CCI" |
                                            reg_TREAT$TREAT_status=="NA-3-CCI",
                                          "In",
                                          ifelse(reg_TREAT$TREAT_status=="NA-1-CCI"|
                                                   reg_TREAT$TREAT_status=="NA-NA-CCI" |
                                                   reg_TREAT$TREAT_status=="NA-NA-PVS" |
                                                   reg_TREAT$TREAT_status=="Out-NA-PVS",
                                                 "Out",
                                                 "Control")))
summary(reg_TREAT$TREAT_status)
reg_TREAT <- subset(reg_TREAT, !duplicated(reg_TREAT$id))
#merge with dataset social2_0
names(social)
social2_0 <- merge(reg_TREAT[,c("id","TREAT", "TREAT_status")],
              social, by="id", all.y=TRUE)
summary(social2_0$TREAT)
### remove temporary datasets
rm(reg_TREAT)
rm(reg_EPM)
rm(climmob)
rm(evaluacion_ECA)
rm(rand_treat)

# BEAN PRODUCTION data set
######################################################################################
names(bean_prod)
str(bean_prod$cultivo_frijol)
bean_prod$cultivo_frijol <- as.factor(ifelse(bean_prod$cultivo_frijol=="SI", "Yes", "No"))
summary(bean_prod$cultivo_frijol)

#get list from bean_prod$periodos_cultivo
#define the list you want extract the information
my_list <- bean_prod$periodos_cultivo
#take the max of collected information within the list
n_option <- max(unlist(lapply(my_list,FUN=length)))
#create a empty matrix to add the data from my_list
take_from_list <- matrix(NA,length(my_list), n_option)
#run over the length of my_list
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}

take_from_list <- as.data.frame(take_from_list)
#reclassify variables into Yes/No variables
#paste all columns of dataframe take_from_list
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
#set the name of the variable 
name_x <- "season"
#get the names of answer options existing in the dataframe take_from_list
options <- sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
#run over the options and create a dummy variable
for (i in options){
  #look for the pattern i
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  #change colunm name
  names(take_from_list)[names(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

names(take_from_list)[5:7] <- paste0("season_", c("apante","primera","postrera"))

#join data set into bean_prod dataframe
names(bean_prod)
bean_prod <- cbind(bean_prod[,c(1:5,7:43)],
                   take_from_list[,c(5:7)])

#get list bean residue use
#define the list you want extract the information
my_list <- bean_prod$bean_residue_use
#take the max of collected information within the list
n_option <- max(unlist(lapply(my_list,FUN=length)))
#create a empty matrix to add the data from my_list
take_from_list <- matrix(NA,length(my_list), n_option)
#run over the length of my_list
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}

take_from_list <- as.data.frame(take_from_list)
#reclassify variables into binary variables
#paste all columns of dataframe take_from_list
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
#set the name of the variable 
name_x <- "bean_residue_use"
#get the names of answer options existing in the dataframe take_from_list
options <- sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
#run over the options and create a dummy variable
for (i in options){
  #look for the pattern i
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  #change colunm name
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#join data set into bean_prod table
names(bean_prod)
names(take_from_list)
bean_prod <- cbind(bean_prod[,c(1:32,34:45)], take_from_list[,c(5:10)])
names(bean_prod)

#get list who works in bean production
#define the list you want extract the information
my_list <- bean_prod$bean_who_works
#take the max of collected information within the list
n_option <- max(unlist(lapply(my_list,FUN=length)))
#create a empty matrix to add the data from my_list
take_from_list <- matrix(NA,length(my_list), n_option)
#run over the length of my_list
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}

take_from_list <- as.data.frame(take_from_list)
#reclassify variables into binary variables
#paste all columns of dataframe take_from_list
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
#set the name of the variable 
name_x <- "bean_who_works"
#get the names of answer options existing in the dataframe take_from_list
options <- sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
#run over the options and create a dummy variable
for (i in options){
  #look for the pattern i
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  #change colunm name
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#join data set into bean_prod table
names(bean_prod)
names(take_from_list)
bean_prod <- cbind(bean_prod[,c(1:3,5:50)],
                   take_from_list[,c(5:8)])
names(bean_prod)

#get list of who control the revenue
my_list <- bean_prod$bean_who_control_revenue
#take the max of collected information within the list
n_option <- max(unlist(lapply(my_list,FUN=length)))
#create a empty matrix to add the data from my_list
take_from_list <- matrix(NA,length(my_list), n_option)
#run over the length of my_list
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}

take_from_list <- as.data.frame(take_from_list)
#reclassify variables into binary variables
#paste all columns of dataframe take_from_list
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
#set the name of the variable 
name_x <- "bean_who_control_revenue"
#get the names of answer options existing in the dataframe take_from_list
options <- sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
#run over the options and create a dummy variable
for (i in options){
  #look for the pattern i
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  #change colunm name
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#join data set into bean_prod table
bean_prod <- cbind(bean_prod[,c(1:3,5:53)], 
                   take_from_list[,c(5:7)])

###
#get list of who control the consumption of beans 
my_list <- bean_prod$bean_consume_control
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "bean_consume_control"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#join data set into bean_prod table
bean_prod <- cbind(bean_prod[,c(1:16,19:38,40:55)], 
                   take_from_list[,c(7:9)])
###
#get list of bean uses
my_list <- bean_prod$bean_use
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "bean_use"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

##############################################
#join data set into bean_prod dataframe
bean_prod <- cbind(bean_prod[,c(1:2,4:55)], 
                   take_from_list[,c(5:7)])
#####  Debug fertilisation information ##
######################################################################################
names(bean_prod)
####extract information in list of fertilisation type
my_list <- bean_prod$fertiliser_type
n_option <- max(unlist(lapply(my_list,FUN=length)))
#n_option <- sort(unique(as.vector(unlist(var_df[,c("fertiliser_type_1", "fertiliser_type_2","fertiliser_type_3","fertiliser_type_4")]))))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "fertiliser_type"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

##############################################
#join data set into bean_prod dataframe
bean_prod <- cbind(bean_prod[,c(1:10,16:57,11,14:15)], 
                   take_from_list[,c(6:13)])
#as fertilise_beans is not complete, get yes/no info for fertilise beans from type of fertilisation
#the current variable will be removed and only farms where the type of fertiliser was reported will be 
#considered as "Y", those who not crop beans will be NA's and the others "N"
bean_prod$fertilise_beans <- NA
bean_prod$fertilise_beans <- as.factor(ifelse(take_from_list$paste=="NA-NA-NA-NA" &
                                                bean_prod$cultivo_frijol == "Yes", "No",
                                              ifelse(take_from_list$paste!="NA-NA-NA-NA", "Yes",
                                                     bean_prod$fertilise_beans)))
summary(bean_prod$fertilise_beans)

##standardise areas of production and remove outliers
names(bean_prod)[c(22,15,18,20,3,5,7,9)]
bean_prod_areas <- NULL
bean_prod$unidad_metrica <- as.numeric(bean_prod$unidad_metrica)
for (i in c(22,15,18,20,3,5,7,9)){
  x <- as.numeric(ifelse(bean_prod$tierra_unidades=="tareas", bean_prod[,i]*((1.672*bean_prod$unidad_metrica)^2)/10000,
                         ifelse(bean_prod$tierra_unidades=="manzanas", bean_prod[,i]*0.7,
                                NA)))
  bean_prod_areas <- data.frame(cbind(bean_prod_areas,x))
  colnames(bean_prod_areas)[colnames(bean_prod_areas)=="x"] <- paste(names(bean_prod[i]), "ha", sep="_")
}

bean_prod_areas[bean_prod_areas==0] <- NA

#remove outliers
summary(bean_prod_areas)


for (j in 1:ncol(bean_prod_areas)){
for (i in unique(boxplot.stats(bean_prod_areas[,j])$out)){
  bean_prod_areas[,j] <- ifelse(bean_prod_areas[,j] == i , NA,bean_prod_areas[,j])
}
}

names(bean_prod_areas) <- gsub("segunda","postrera",names(bean_prod_areas))


#add into bean prod dataframe
bean_prod <- cbind(bean_prod, bean_prod_areas)
rm(bean_prod_areas)

#tarea riego the extension of land that have irrigation
summary(bean_prod$tarea_riego)
bean_prod$riego_ha <- ifelse(bean_prod$tierra_unidades=="tareas",
                             bean_prod$tarea_riego*((1.672*bean_prod$unidad_metrica)^2)/10000,
                             ifelse(bean_prod$tierra_unidades=="manzanas",
                                    bean_prod$tarea_riego*0.7,
                                    NA))
summary(bean_prod$riego_ha)

bean_prod$bean_riego <- as.factor(ifelse(bean_prod$riego_ha > 0, "Y", "N"))
bean_prod$riego_ha[bean_prod$riego_ha==0] <- NA
boxplot(bean_prod$riego_ha)
hist(bean_prod$riego_ha)
#remove outliers
var <- "riego_ha"
kl <- length(unique(boxplot.stats(bean_prod[,var])$out))>0
while(kl){
  out <- unique(boxplot.stats(bean_prod[,var])$out)
  
  bean_prod[,var] <- ifelse(bean_prod[,var] %in% out, NA, bean_prod[,var])
  
  kl <- length(unique(boxplot.stats(bean_prod[,var])$out))>0
}

### production in quintales to kg 
names(bean_prod)[c(4,6,8)]
bean_prod_amount <- NULL
for (i in c(4,6,8)){
  x <- as.numeric(bean_prod[,i]*45.3592)
  bean_prod_amount <- data.frame(cbind(bean_prod_amount,x))
  colnames(bean_prod_amount)[colnames(bean_prod_amount)=="x"] <- names(bean_prod[i])
}

bean_prod_amount[bean_prod_amount==0] <- NA

#remove outliers
for (j in 1:ncol(bean_prod_amount)){
  kl <- length(unique(boxplot.stats(bean_prod_amount[,j])$out))>0
  while(kl){
    out <- unique(boxplot.stats(bean_prod_amount[,j])$out)
    
    bean_prod_amount[,j] <- ifelse(bean_prod_amount[,j] %in% out, NA, bean_prod_amount[,j])
    
    kl <- length(unique(boxplot.stats(bean_prod_amount[,j])$out))>0
  }
}

#add into bean prod dataframe
bean_prod <- cbind(bean_prod,bean_prod_amount)
rm(bean_prod_amount)

#if have harvested in primera 2015
bean_prod$cosecha_primera_2015_SN <- as.factor(ifelse(bean_prod$cosecha_primera_2015_SN=="SI","Yes","No"))
summary(bean_prod$cosecha_primera_2015_SN)

#standarise fertiliser units into kg
#### This data has several errors which I could not debug 
bean_prod$fertiliser_amount[bean_prod$fertiliser_amount==0] <- NA

bean_prod$fertiliser_amount_kg <- as.numeric(ifelse(bean_prod$fertiliser_units=="libras" & bean_prod$fertilise_beans=="Yes",
                                                    bean_prod$fertiliser_amount*0.453592,
                                                    ifelse(bean_prod$fertiliser_units=="qq" & bean_prod$fertilise_beans=="Yes",
                                                           bean_prod$fertiliser_amount*45.3592,
                                                           NA)))

boxplot(bean_prod$fertiliser_amount_kg)
#remove outliers
  var <- "fertiliser_amount_kg"
  kl <- length(unique(boxplot.stats(bean_prod[,var])$out))>0
  while(kl){
    out <- unique(boxplot.stats(bean_prod[,var])$out)
    
    bean_prod[,var] <- ifelse(bean_prod[,var] %in% out, NA, bean_prod[,var])
    
    kl <- length(unique(boxplot.stats(bean_prod[,var])$out))>0
  }

#weekly bean consumption into kg
summary(bean_prod$consumo_semanal)
bean_prod$consumo_semanal_kg <- bean_prod$consumo_semanal*0.453592
boxplot(bean_prod$consumo_semanal_kg)
#remove outliers
var <- "consumo_semanal_kg"
kl <- length(unique(boxplot.stats(bean_prod[,var])$out))>0
while(kl){
  out <- unique(boxplot.stats(bean_prod[,var])$out)
  
  bean_prod[,var] <- ifelse(bean_prod[,var] %in% out, NA, bean_prod[,var])
  
  kl <- length(unique(boxplot.stats(bean_prod[,var])$out))>0
}
############################################################
#manure beans
bean_prod$manure_beans <- as.factor(bean_prod$manure_beans)
summary(bean_prod$manure_beans)
############################################################
#select standarised variables
names(bean_prod)
bean_prod2_0 <- bean_prod[,-c(3:9,11:15,18,20:22,30)] 
names(bean_prod2_0)

##### DETAILED information on BEAN PRODUCTION within bean variables
### in order to do not eliminate entries the table were previus handled in excel
bean_prod_detailed <- read.csv(here(wd, "input/Prueba3_Baseline_BeanProduction.csv"))
colnames(bean_prod_detailed)[1] <- "id"
str(bean_prod_detailed)
bean_prod_detailed$Value <- as.character(bean_prod_detailed$Value)

### read list of bean production I reclassified in order to reshape beanprod table
beanvar <- read.csv(here(wd, "input/Prueba3_bean_varieties_classified_25may2016.csv"))
str(beanvar)
beanvar$bean_id <- paste(beanvar$codigo_hogar, beanvar$Variety, sep="-")

## read list of beans types (criollo or mejorado) according Rosa Nila
var_type <- read.csv(here(wd, "input/Prueba3_beanclasses_24may2016.csv"))
str(var_type)
colnames(var_type)[1] <- "bean_id" 
var_type <- merge(beanvar[,c("bean_id","Variedad")],
                  var_type, by="bean_id", all.y=TRUE)
var_type$Variable <- ifelse(is.na(var_type$bean_id), "", "class")
var_type$Variable <- paste(var_type$Variable, var_type$Variedad, sep="_")
colnames(var_type)[7] <- "Value"
colnames(var_type)[3] <- "id"
names(var_type)
#set with class criollo and mejorado
var_type1 <- var_type[,c("id", "Variety", "Variable", "Value")]
#set with name of beans varieties
var_type2 <- var_type[,c("id", "Variety", "Variable", "frijoles_label")]
colnames(var_type2)[colnames(var_type2)=="frijoles_label"] <- "Value"
var_type2$Variable <- gsub("class_","class_name_",var_type2$Variable)

# create merged _id in order to merge objects
bean_prod_detailed$bean_id <- paste(bean_prod_detailed$id, 
                                    bean_prod_detailed$Variety, sep="-")
str(bean_prod_detailed)

###merge beanvar with bean_prod_detailed
names(beanvar)
names(bean_prod_detailed)
bean_prod_detailed <- merge(beanvar[,c("bean_id", "Variedad")], 
                  bean_prod_detailed, by="bean_id", all.y=TRUE)
names(bean_prod_detailed)
bean_prod_detailed$Variable <- paste(bean_prod_detailed$Variable,
                                     bean_prod_detailed$Variedad, sep="_")
bean_prod_detailed <- bean_prod_detailed[,c("id", "Variety", "Variable", "Value")]

#join rows
bean_prod_detailed <- rbind(bean_prod_detailed, var_type1)
bean_prod_detailed <- rbind(bean_prod_detailed, var_type2)

bean_prod_detailed$Variable <- gsub("frijoles_","",bean_prod_detailed$Variable)
bean_prod_detailed$bean_id <- paste(bean_prod_detailed$id,
                                    bean_prod_detailed$Variety, sep="-")

bean_prod_detailed <- bean_prod_detailed[,c("bean_id", "Variable", "Value")]
bean_prod_detailed$Variable <- gsub(c("_var1"), "",bean_prod_detailed$Variable)
bean_prod_detailed$Variable <- gsub(c("_var2"), "",bean_prod_detailed$Variable)
bean_prod_detailed$Variable <- gsub(c("_var3"), "",bean_prod_detailed$Variable)
bean_prod_detailed$Variable <- gsub(c("_var4"), "",bean_prod_detailed$Variable)
bean_prod_detailed$Variable <- gsub(c("_var5"), "",bean_prod_detailed$Variable)

bean_prod_detailed$Variable <- as.factor(bean_prod_detailed$Variable)
summary(bean_prod_detailed$Variable)

bean_prod_detailed <- subset(bean_prod_detailed, Variable!="periodos")
bean_prod_detailed <- subset(bean_prod_detailed, Variable!="periodos.2")
#bean_prod_detailed <- subset(bean_prod_detailed, Variable!="apante_areas")
#bean_prod_detailed <- subset(bean_prod_detailed, Variable!="segunda_consumo")
bean_prod_detailed <- subset(bean_prod_detailed, Variable!="segunda_consumo.2")
#bean_prod_detailed <- subset(bean_prod_detailed, Variable!="apante_produccion")
bean_prod_detailed <- subset(bean_prod_detailed, Variable!="periodos.2.2")
bean_prod_detailed <- subset(bean_prod_detailed, Variable!="segunda_usos")
bean_prod_detailed <- subset(bean_prod_detailed, Variable!="segunda_usos.2")
bean_prod_detailed <- subset(bean_prod_detailed, Variable!="segunda_usos.2.2")
bean_prod_detailed <- subset(bean_prod_detailed, Variable!="segunda_usos.2.2.2")
#bean_prod_detailed <- subset(bean_prod_detailed, Variable!="segunda_venta")
#bean_prod_detailed <- subset(bean_prod_detailed, Variable!="primera_areas_2015")
bean_prod_detailed <- subset(bean_prod_detailed, Variable!="primera_produccion_2015_SN")
bean_prod_detailed <- subset(bean_prod_detailed, Variable!="primera_produccion_2015")

names(bean_prod_detailed)
bean_prod_detailed <- reshape(bean_prod_detailed,
                    v.names ="Value", #observed/collected values 
                    timevar="Variable", #variables/questions 
                    idvar= "bean_id", #unique id of reshaped entry
                    direction="wide")[,-3]
colnames(bean_prod_detailed) <- gsub("Value.", "", colnames(bean_prod_detailed))

#split bean_id
bean_prod_detailed <- cbind(bean_prod_detailed,
                            colsplit(bean_prod_detailed$bean_id, "-", names=c("id","label")))

## remove temporary data
rm(beanvar)
rm(var_type)
rm(var_type2)
rm(var_type1)

#####  debug information ##
######################################################################################
#join information of metric and currencyunits from bean_prod dataset 
names(bean_prod)
bean_prod_detailed <- merge(bean_prod_detailed,
                            bean_prod[,c("id","tierra_unidades","unidad_metrica","cuantos_metros")],
                            by="id", all.y=TRUE)
bean_prod_detailed <- merge(bean_prod_detailed,
                            social[,c("id","local_currency")],
                                   by="id", all.y=TRUE)

#years of working in beans
bean_prod_detailed$segunda_anyos <- as.numeric(bean_prod_detailed$segunda_anyos)
boxplot(bean_prod_detailed$segunda_anyos)
bean_prod_detailed$segunda_anyos[bean_prod_detailed$segunda_anyos==250] <- 25
bean_prod_detailed$segunda_anyos[bean_prod_detailed$segunda_anyos==70] <- 7

#bean price per kg 
bean_prod_detailed$segunda_precio <- as.numeric(bean_prod_detailed$segunda_precio)
bean_prod_detailed$bean_price_USD_kg <- with(bean_prod_detailed, ifelse(local_currency=="GTQ",
                                                                        segunda_precio/7.8,
                                                                        ifelse(local_currency=="HNL",
                                                                               segunda_precio/20,
                                                                               segunda_precio)))
boxplot.stats(bean_prod_detailed$bean_price_USD_kg)$out
#remove some outliers
bean_prod_detailed$bean_price_USD_kg <- ifelse(bean_prod_detailed$bean_price_USD_kg<10 |
                                                 bean_prod_detailed$bean_price_USD_kg>99, NA,
                                               bean_prod_detailed$bean_price_USD_kg)
#prices into USD per kg
bean_prod_detailed$bean_price_USD_kg <- bean_prod_detailed$bean_price_USD_kg / 45.35
boxplot(bean_prod_detailed$bean_price_USD_kg )

##standarise bean area production into hectare
names(bean_prod_detailed)[c(7,9,11,13)]
bean_prod_areas <- NULL
for (i in c(7,9,11,13)){
  bean_prod_detailed[,i] <- as.numeric(bean_prod_detailed[,i])
  x <- as.numeric(ifelse(bean_prod_detailed$tierra_unidades=="tareas", bean_prod_detailed[,i]*((1.672*bean_prod_detailed$unidad_metrica)^2)/10000,
                         ifelse(bean_prod_detailed$tierra_unidades=="manzanas", bean_prod_detailed[,i]*0.7,
                                NA)))
  bean_prod_areas <- data.frame(cbind(bean_prod_areas,x))
  colnames(bean_prod_areas)[colnames(bean_prod_areas)=="x"] <- paste(names(bean_prod_detailed[i]), "ha", sep="_")
}

bean_prod_areas[bean_prod_areas==0] <- NA

#remove outliers
for (j in 1:ncol(bean_prod_areas)){
  for (i in unique(boxplot.stats(bean_prod_areas[,j])$out)){
    bean_prod_areas[,j] <- ifelse(bean_prod_areas[,j] == i , NA,bean_prod_areas[,j])
  }
  boxplot(bean_prod_areas[j])
}

#add into bean prod dataframe
bean_prod_detailed <- cbind(bean_prod_detailed, bean_prod_areas)
rm(bean_prod_areas)

###########################
### production in quintales to kg 
names(bean_prod_detailed)[c(8,10,12)]
bean_prod_amount <- NULL
for (i in c(8,10,12)){
  bean_prod_detailed[,i] <- as.numeric(bean_prod_detailed[,i])
  x <- as.numeric(bean_prod_detailed[,i]*45.3592)
  bean_prod_amount <- data.frame(cbind(bean_prod_amount,x))
  colnames(bean_prod_amount)[colnames(bean_prod_amount)=="x"] <- paste(names(bean_prod_detailed[i]), "kg", sep="_")
}

bean_prod_amount[bean_prod_amount==0] <- NA

#remove outliers
for (j in 1:ncol(bean_prod_amount)){
  for (i in unique(boxplot.stats(bean_prod_amount[,j])$out)){
    bean_prod_amount[,j] <- ifelse(bean_prod_amount[,j] == i , NA,bean_prod_amount[,j])
  }
  boxplot(bean_prod_amount[j])
}

#add into bean prod dataframe
bean_prod_detailed <- cbind(bean_prod_detailed, bean_prod_amount)
rm(bean_prod_amount)

###create final dataset for prod_detailed
names(bean_prod_detailed)
bean_prod_detailed2_0 <- bean_prod_detailed[,-c(3:13,18:21)]

bean_prod_detailed2_0 <- bean_prod_detailed2_0[!is.na(bean_prod_detailed2_0$bean_id),]

##### BEAN KNOWLEDGE data set ####
######################################################################################
######################################################################################
names(bean_know)
#degub variable "comprado semilla numero"
for (i in 1:nrow(bean_know)){
  bean_know[i,9] <- as.integer(ifelse(length(bean_know$comprar_semillas_nombres[[i]])==0, 0,
                                      length(bean_know$comprar_semillas_nombres[[i]])))
}  
###if farms have bought bean seeds 
bean_know$comprado_semillas <- as.factor(ifelse(bean_know$comprar_semillas_numero==0,"No","Yes"))
summary(bean_know$comprado_semillas)

#degub variable "compra anual"
#the amount of seed bought in the last 12 months in quintales
summary(bean_know$compra_anual)
bean_know$comprar_anual_kg <- as.numeric(bean_know$compra_anual*45.36)
bean_know$comprar_anual_kg <- as.numeric(ifelse(bean_know$comprar_anual_kg==0, NA,
                                                ifelse(bean_know$comprado_semillas=="No", NA,
                                                bean_know$comprar_anual_kg)))
for (i in unique(boxplot.stats(bean_know$comprar_anual_kg)$out)){
  bean_know$comprar_anual_kg <- ifelse(bean_know$comprar_anual_kg== i , NA, bean_know$comprar_anual_kg)
}
summary(bean_know$comprar_anual_kg)
boxplot(bean_know$comprar_anual_kg, main="annual purchase")

###if farms have received bean seeds in last 12 months 
#recibido semillas numero
for (i in 1:nrow(bean_know)){
  bean_know[i,16] <- as.integer(ifelse(length(bean_know$recibido_semillas_nombres[[i]])==0, 0,
                                      length(bean_know$recibido_semillas_nombres[[i]])))
} 

bean_know$recibido_semillas <- as.factor(ifelse(bean_know$recibido_semillas_numero==0,"No","Yes"))
summary(bean_know$recibido_semillas)

#get list of names of source of bean seeds
my_list <- bean_know$recibido_semillas_fuente
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "recibido_semillas_fuente"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#join to bean_know dataset
bean_know <- cbind(bean_know, take_from_list[,c(4:8)])

#recibido semilla otra fuente vecino
#if have received seeds from neighbours, was man or woman 
summary(as.factor(bean_know$recibido_otra_fuente_vecino))
bean_know$recibido_otra_fuente_vecino <- as.factor(ifelse(bean_know$recibido_otra_fuente_vecino=="HOMBRE","Male",
                                                          ifelse(bean_know$recibido_otra_fuente_vecino=="MUJER","Female",
                                                                 NA)))
##recibido semilla persona
##if was a man or woman who received the seed from neighbour
summary(as.factor(bean_know$recibido_semillas_persona))
bean_know$recibido_semillas_persona <- as.factor(ifelse(bean_know$recibido_semillas_persona=="HOMBRE","Male",
                                                        ifelse(bean_know$recibido_semillas_persona=="MUJER","Female",
                                                               NA)))
##vender semilla
#how many times have sold seeds
for (i in 1:nrow(bean_know)){
  bean_know[i,23] <- as.integer(ifelse(length(bean_know$vender_semillas_nombres[[i]])==0, 0,
                                       length(bean_know$vender_semillas_nombres[[i]])))
}
##if have sold seeds
summary(as.factor(bean_know$vender_semillas))
bean_know$vender_semillas <- as.factor(ifelse(bean_know$vender_semillas_numero==0, "No","Yes"))
summary(bean_know$vender_semillas)
##venta anual
##the ammount of seeds sold in last 12 months
str(bean_know$venta_anual)
#values into kg
bean_know$venta_anual_kg <- as.numeric(ifelse(bean_know$vender_semillas=="Yes" &
                                       bean_know$venta_anual==0, NA,
                                     ifelse(bean_know$vender_semillas=="No",0,
                                            ifelse(bean_know$venta_anual>90, 90*45.36,
                                                   bean_know$venta_anual*45.36))))
#degub variable "compra anual"
#the amount of seed bought in the last 12 months in quintales
summary(bean_know$venta_anual)
bean_know$venta_anual_kg <- as.numeric(bean_know$venta_anual*45.36)
bean_know$venta_anual_kg <- as.numeric(ifelse(bean_know$venta_anual_kg==0, NA,
                                                ifelse(bean_know$vender_semillas=="No", NA,
                                                       bean_know$venta_anual_kg)))
for (i in unique(boxplot.stats(bean_know$venta_anual_kg)$out)){
  bean_know$venta_anual_kg <- ifelse(bean_know$venta_anual_kg== i , NA, bean_know$venta_anual_kg)
}
summary(bean_know$venta_anual_kg)
boxplot(bean_know$venta_anual_kg, main="annual sell")

#get list of names of where beans were sold
my_list <- bean_know$vender_semillas_donde
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "vender_semillas_donde"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

##join colunms to bean_know
bean_know <- cbind(bean_know, take_from_list[,c(4:7)])

##regalar semilla
##if farmer gave beans to someone
summary(as.factor(bean_know$regalado_semillas))
bean_know$regalado_semillas <- as.factor(ifelse(bean_know$regalado_semillas=="SI","Yes","No"))
summary(bean_know$regalado_semillas)
##regalar semilla genero
##if was a man or woman who received the seed from neighbour
summary(as.factor(bean_know$reglar_semilla_genero))
bean_know$reglar_semilla_genero <- as.factor(ifelse(bean_know$reglar_semilla_genero=="HOMBRE","Male",
                                                    ifelse(bean_know$reglar_semilla_genero=="MUJER","Female",
                                                           NA)))
#regalar semilla numero
#how many times have gave seeds 
hist(bean_know$regalado_semillas_numero)
bean_know$regalado_semillas_numero <- as.integer(ifelse(bean_know$regalado_semillas=="No", NA,
                                                        bean_know$regalado_semillas_numero))

#donacion anual
#how many beans have received for consumption in last 12 months
str(bean_know$donacion_anual)
bean_know$donacion_anual_kg <- as.numeric(bean_know$donacion_anual*45.36)
bean_know$donacion_anual_kg[bean_know$donacion_anual_kg==0] <- NA
for (i in unique(boxplot.stats(bean_know$donacion_anual_kg)$out)){
  bean_know$donacion_anual_kg <- ifelse(bean_know$donacion_anual_kg== i , NA, bean_know$donacion_anual_kg)
}
summary(bean_know$donacion_anual_kg)
boxplot(bean_know$donacion_anual_kg, main="annual donation")

#rechazar semillas
#if have quit some variety of beans 
summary(as.factor(bean_know$rechazar_semillas))
bean_know$rechazar_semillas <- as.factor(ifelse(bean_know$rechazar_semillas=="NO","No","Yes"))

#get list of names of options to refused seeds
my_list <- bean_know$rechazar_semillas_opt
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "rechazar_semillas"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}
#join to bean_know dataset
bean_know <- cbind(bean_know, take_from_list[,c(12:25)])

#####
#refused seed gender
#if was the man, woman or both who had refused the seeds
summary(as.factor(bean_know$rechazar_semilla_gen))
bean_know$rechazar_semilla_gen<-as.factor(ifelse(bean_know$rechazar_semilla_gen=="rechazar_H","Male",
                                                 ifelse(bean_know$rechazar_semilla_gen=="rechazar_M","Female",
                                                        ifelse(bean_know$rechazar_semilla_gen=="rechazar_Dos","Both",
                                                               ifelse(bean_know$rechazar_semilla_gen=="rechazar_Alt", "Male",
                                                                      ifelse(bean_know$rechazar_semilla_gen=="rechazar_otro","Male",
                                                                             as.character(bean_know$rechazar_semilla_gen)))))))
summary(bean_know$rechazar_semilla_gen)

#get list of received information
#if farmers have received information from neighbours 
my_list <- bean_know$informacion_opt
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "informacion"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}
###join vectors of information into bean_know
bean_know <- cbind(bean_know, take_from_list[,c(13:26)])

#get list of field tests lead by farmers
#if farmers have lead some test in one of these options 
my_list <- bean_know$pruebas_opt
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "prueba"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#join to bean_know dataset
names(take_from_list)
bean_know <- cbind(bean_know, take_from_list[,c(11:24)])

#get list of vecinos 
#if farmers have gave some information to neighbours 
my_list <- bean_know$vecinos_opt
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "vecino"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

bean_know <- cbind(bean_know, take_from_list[,c(9:22)])
names(bean_know)
#reorganise colunms and remove non focal colunms
bean_know2_0 <- bean_know[,-c(2,4:7,10:14,17:19,24:28,32:33,35:36,38:41,43:45)]
names(bean_know2_0)

##### INCOME information ####
######################################################################################
names(income)
###standarise other variables 
income$offfarm_incomes_any <- as.factor(income$offfarm_incomes_any)
summary(income$offfarm_incomes_any)

#off farm incomes
my_list <- income$offfarm_incomes
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "offfarm_incomes"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

###########################
#reorganise colunms
income <- cbind(income[,c(1:2,5:13)], take_from_list[,c(7:18)])
##################################
summary(income$offfarm_incomes_other)
income$offfarm_incomes_other <- as.factor(income$offfarm_incomes_other)
##################################
income$offfarm_incomes_count <- as.numeric(income$offfarm_incomes_count)
income$offfarm_incomes_count[income$offfarm_incomes_count==0] <- NA
#if farmer has no income then count is zero
income$offfarm_incomes_count <- ifelse(is.na(income$offfarm_incomes_count) &
                                         income$offfarm_incomes_any=="No",0,
                                       income$offfarm_incomes_count)
summary(income$offfarm_incomes_count)
hist(income$offfarm_incomes_count)


#off farm income proportion
summary(as.factor(income$offfarm_total_income_proportion))
income$offfarm_total_income_proportion <- as.factor(income$offfarm_total_income_proportion)
#count of off farm incomes

#debts
income$debts_have <- as.character(income$debts_have)
income$debts_have <- as.factor(ifelse(income$debts_have=="Y","Yes", ifelse(income$debts_have=="N","No", income$debts_have)))
summary(income$debts_have)

#debts proportion of income
income$debts_proportionofincome <- as.factor(income$debts_proportionofincome)
summary(income$debts_proportionofincome)
#debts worry
income$debts_worry <- as.character(income$debts_worry)
income$debts_worry  <- as.factor(ifelse(income$debts_worry =="Y","Yes", ifelse(income$debts_worry =="N","No", income$debts_worry )))
summary(income$debts_worry)

#debts to who
my_list <- income$debts_to_who
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "debts_to"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#merge colunms
income <- cbind(income, take_from_list[,c(5:9)])
#select only focal variables
names(income)
income2_0 <- income[,-c(5,10,11)]

##### LIVESTOCK information ######
######################################################################################
names(livestock)
#livestock animals list
my_list <- livestock$livestock
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "livestock"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#merge set_livestock info with livestock dataset
livestock <- cbind(livestock[,c(1,3:4)], take_from_list[,c(9:25)])
names(livestock)


#fill information about livestock using animals options in set_livestock
livestock$livestock_owners <- as.factor(ifelse(take_from_list$paste=="NA-NA-NA-NA-NA-NA-NA" , "No", "Yes"))
summary(livestock$livestock_owners)

#livestock count
livestock$livestock_count <- as.integer(ifelse(livestock$livestock_owners=="No",0,livestock$livestock_count))

#final dataset
livestock2_0 <- livestock
  
##### OTHER CROP information ######
names(other_crop)

#get list of other crops
my_list <- other_crop$crops
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "crop"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}


#merge with other crops dataset
other_crop <- cbind(other_crop, take_from_list[,c(13:29)] )
#create final dataset
names(other_crop)
other_crop2_0 <- other_crop[,c(1,3,9:17,21:25,18:20,4:6)]

##### OTHER CROP detailed ######
#get detailed list of other crops 
setwd(baseline_dir)
other_crop_detailed <- read.csv("input/Prueba3_Baseline_crops.csv")
colnames(other_crop_detailed)[1] <- "id"

other_crop_detailed$crop_id <- with(other_crop_detailed,
                                    paste(id, Crop, sep="-"))

other_crop_detailed <- reshape(other_crop_detailed,
                               v.names ="Value", #observed/collected values 
                               timevar="Variable", #variables/questions 
                               idvar= "crop_id",#unique id of reshaped entry
                               direction="wide")[,-3]
colnames(other_crop_detailed) <- gsub("Value.", "", colnames(other_crop_detailed))

str(other_crop_detailed$crop_area)
other_crop_detailed$crop_area <- as.numeric(other_crop_detailed$crop_area)
summary(other_crop_detailed$crop_area)

str(other_crop_detailed$crop_area_units)
summary(other_crop_detailed$crop_area_units)
other_crop_detailed$crop_area_units <- as.character(other_crop_detailed$crop_area_units)
other_crop_detailed$crop_area_units <- as.factor(other_crop_detailed$crop_area_units)
summary(other_crop_detailed$crop_area_units)

other_crop_detailed <- merge(other_crop_detailed, mydata[,c("id", "unidad_metrica")], 
                             by="id", all.y=FALSE)

#standarise information 
#land area of crop
str(other_crop_detailed$unidad_metrica)
other_crop_detailed$unidad_metrica <- as.numeric(other_crop_detailed$unidad_metrica)
summary(other_crop_detailed$unidad_metrica)


other_crop_detailed$unidad_metrica <- ifelse(is.na(other_crop_detailed$unidad_metrica) &
                                               other_crop_detailed$crop_area_units=="tarea", 12,
                                             other_crop_detailed$unidad_metrica)

other_crop_detailed$crop_area_ha <- ifelse(other_crop_detailed$crop_area_units=="tarea",
                                           other_crop_detailed$crop_area*((1.672*other_crop_detailed$unidad_metrica)^2)/10000,
                                         ifelse(other_crop_detailed$crop_area_units=="manzana",
                                                other_crop_detailed$crop_area*0.7,
                                                ifelse(other_crop_detailed$crop_area_units=="metersq",
                                                       other_crop_detailed$crop_area*0.0001,
                                                       ifelse(is.na(other_crop_detailed$crop_area_units),
                                                              other_crop_detailed$crop_area*((1.672*other_crop_detailed$unidad_metrica)^2)/10000,
                                                       other_crop_detailed$crop_area))))
#crop yield 
str(other_crop_detailed$crop_yield_units)
other_crop_detailed$crop_yield_units <- as.character(other_crop_detailed$crop_yield_units)
other_crop_detailed$crop_yield_units <- as.factor(other_crop_detailed$crop_yield_units)
summary(other_crop_detailed$crop_yield_units)
other_crop_detailed$crop_yield_units <- as.character(other_crop_detailed$crop_yield_units)
summary(other_crop_detailed$crop_yield_units)

str(other_crop_detailed$crop_yield)
other_crop_detailed$crop_yield <- as.numeric(other_crop_detailed$crop_yield)

other_crop_detailed$crop_yield_kg <- as.numeric(ifelse(other_crop_detailed$crop_yield_units=="buckets",
                                            other_crop_detailed$crop_yield*18.9271,
                                            ifelse(other_crop_detailed$crop_yield_units=="bunches", #a banana bunche has ~30-50 kg, I used the medium value
                                                   other_crop_detailed$crop_yield*40,
                                                   ifelse(other_crop_detailed$crop_yield_units=="libras",
                                                          other_crop_detailed$crop_yield*0.4536,
                                                          ifelse(other_crop_detailed$crop_yield_units=="qq",
                                                                 other_crop_detailed$crop_yield*45.36,
                                                                 ifelse(other_crop_detailed$crop_yield_units=="tons",
                                                                        other_crop_detailed$crop_yield*1000,
                                                                        ifelse(other_crop_detailed$crop_yield_units=="kg",
                                                                               other_crop_detailed$crop_yield,      
                                                                        NA)))))))


#fertilise crop 
other_crop_detailed$fertilise_crop <- as.character(other_crop_detailed$fertilise_crop)
other_crop_detailed$fertilise_crop[other_crop_detailed$fertilise_crop=="Y"] <- "Yes"
other_crop_detailed$fertilise_crop[other_crop_detailed$fertilise_crop=="N"] <- "No"
other_crop_detailed$fertilise_crop <- as.factor(other_crop_detailed$fertilise_crop)
summary(other_crop_detailed$fertilise_crop)

#manure crop 
other_crop_detailed$manure_crop <- as.character(other_crop_detailed$manure_crop)
other_crop_detailed$manure_crop[other_crop_detailed$manure_crop=="Y"] <- "Yes"
other_crop_detailed$manure_crop[other_crop_detailed$manure_crop=="N"] <- "No"
other_crop_detailed$manure_crop <- as.factor(other_crop_detailed$manure_crop)
summary(other_crop_detailed$manure_crop)

#crop use
str(other_crop_detailed$crop_use)
other_crop_detailed$crop_use <- as.character(other_crop_detailed$crop_use)
other_crop_detailed$crop_use <- as.factor(other_crop_detailed$crop_use)
summary(other_crop_detailed$crop_use)

#crop residue use
other_crop_detailed$residue_soil <- as.factor(as.character(other_crop_detailed$residue_soil))
summary(other_crop_detailed$residue_soil)

other_crop_detailed$residue_fuel <- as.factor(as.character(other_crop_detailed$residue_fuel))
summary(other_crop_detailed$residue_fuel)

other_crop_detailed$residue_feed <- as.factor(as.character(other_crop_detailed$residue_feed))
summary(other_crop_detailed$residue_feed)

other_crop_detailed$residue_sell <- as.factor(as.character(other_crop_detailed$residue_sell))
summary(other_crop_detailed$residue_sell)

other_crop_detailed$residue_burn <- as.factor(as.character(other_crop_detailed$residue_burn))
summary(other_crop_detailed$residue_burn)

other_crop_detailed$residue_compost <- as.factor(as.character(other_crop_detailed$residue_compost))
summary(other_crop_detailed$residue_compost)

#sold quantity
str(other_crop_detailed$crop_sold_quantity)
other_crop_detailed$crop_sold_quantity <- as.numeric(other_crop_detailed$crop_sold_quantity)
summary(other_crop_detailed$crop_sold_quantity)

str(other_crop_detailed$crop_sold_quantity_units)
other_crop_detailed$crop_sold_quantity_units <- as.factor(as.character(other_crop_detailed$crop_sold_quantity_units ))
summary(other_crop_detailed$crop_sold_quantity_units)

other_crop_detailed$crop_sold_quantity_kg <- as.numeric(ifelse(other_crop_detailed$crop_sold_quantity_units=="buckets",
                                                       other_crop_detailed$crop_sold_quantity*18.9271,
                                                       ifelse(other_crop_detailed$crop_sold_quantity_units=="bunches", #a banana bunche has ~30-50 kg, I used the medium value
                                                              other_crop_detailed$crop_sold_quantity*40,
                                                              ifelse(other_crop_detailed$crop_sold_quantity_units=="libras",
                                                                     other_crop_detailed$crop_sold_quantity*0.4536,
                                                                     ifelse(other_crop_detailed$crop_sold_quantity_units=="qq",
                                                                            other_crop_detailed$crop_sold_quantity*45.36,
                                                                            ifelse(other_crop_detailed$crop_sold_quantity_units=="tons",
                                                                                   other_crop_detailed$crop_sold_quantity*1000,
                                                                                   ifelse(other_crop_detailed$crop_sold_quantity_units=="kg",
                                                                                          other_crop_detailed$crop_sold_quantity,      
                                                                                          NA)))))))

#crop sold income
str(other_crop_detailed$crop_sold_income)
other_crop_detailed$crop_sold_income <- as.numeric(other_crop_detailed$crop_sold_income)
summary(other_crop_detailed$crop_sold_income)

#join currency information
other_crop_detailed <- merge(other_crop_detailed, social2_0[,c("id", "local_currency")],
                             by="id", all.y=FALSE)

other_crop_detailed$crop_sold_income_USD <- as.numeric(ifelse(other_crop_detailed$local_currency=="GTQ",
                                                              other_crop_detailed$crop_sold_income/7.7, #average value of GTQ in 2014
                                                              ifelse(other_crop_detailed$local_currency=="HNL",
                                                                     other_crop_detailed$crop_sold_income/20.3,
                                                                     other_crop_detailed$crop_sold_income)))


str(other_crop_detailed$crop_sold_price_quantityunits)
other_crop_detailed$crop_sold_price_quantityunits <- as.factor(as.character(other_crop_detailed$crop_sold_price_quantityunits))
summary(other_crop_detailed$crop_sold_price_quantityunits)

#crop sold how
str(other_crop_detailed$crop_sold_how)
other_crop_detailed$crop_sold_how <- as.factor(as.character(other_crop_detailed$crop_sold_how))
summary(other_crop_detailed$crop_sold_how)

#crop consume quantity
str(other_crop_detailed$crop_consumed_quantity)
other_crop_detailed$crop_consumed_quantity <- as.numeric(other_crop_detailed$crop_consumed_quantity)
summary(other_crop_detailed$crop_consumed_quantity)

str(other_crop_detailed$crop_consumed_quantity_units)
other_crop_detailed$crop_consumed_quantity_units <- as.factor(as.character(other_crop_detailed$crop_consumed_quantity_units))
summary(other_crop_detailed$crop_consumed_quantity_units)

other_crop_detailed$crop_consumed_quantity_kg <- as.numeric(ifelse(other_crop_detailed$crop_consumed_quantity_units=="buckets",
                                                               other_crop_detailed$crop_consumed_quantity*18.9271,
                                                               ifelse(other_crop_detailed$crop_consumed_quantity_units=="bunches", #a banana bunche has ~30-50 kg, I used the medium value
                                                                      other_crop_detailed$crop_consumed_quantity*40,
                                                                      ifelse(other_crop_detailed$crop_consumed_quantity_units=="libras",
                                                                             other_crop_detailed$crop_consumed_quantity*0.4536,
                                                                             ifelse(other_crop_detailed$crop_consumed_quantity_units=="qq",
                                                                                    other_crop_detailed$crop_consumed_quantity*45.36,
                                                                                    ifelse(other_crop_detailed$crop_consumed_quantity_units=="tons",
                                                                                           other_crop_detailed$crop_consumed_quantity*1000,
                                                                                           ifelse(other_crop_detailed$crop_consumed_quantity_units=="kg",
                                                                                                  other_crop_detailed$crop_consumed_quantity,      
                                                                                                  NA)))))))

#other
other_crop_detailed$crop_traded_quantity <- as.factor(as.character(other_crop_detailed$crop_traded_quantity))
other_crop_detailed$crop_traded_quantity_units <- as.factor(as.character(other_crop_detailed$crop_traded_quantity_units))
other_crop_detailed$crop_consumed_units_other <- as.factor(as.character(other_crop_detailed$crop_consumed_units_other))
other_crop_detailed$crop_who_works <- as.factor(as.character(other_crop_detailed$crop_who_works))
summary(other_crop_detailed$crop_who_works)

other_crop_detailed$crop_who_control_revenue <-as.factor(as.character(other_crop_detailed$crop_who_control_revenue))
other_crop_detailed$crop_consume_control <- as.factor(as.character(other_crop_detailed$crop_consume_control))

#create crop_detailed2_0
names(other_crop_detailed)


#remove temporary datasets 
rm(set_crops)
other_crop_detailed2_0 <- other_crop_detailed[,c(1:2,5:44)]
rm(other_crop_detailed)


##### AID information ######
names(aid)
#get list of types of aid received
my_list <- aid$aidtypes
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "aidtypes"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#merge with aid dataset
names(take_from_list)
aid <- cbind(aid, take_from_list[,(6:10)])

#get list of what type of aid food were given
my_list <- aid$aid_food_what
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "aidfood"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}
#merge with aid dataset
names(take_from_list)
aid <- cbind(aid, take_from_list[,c(7:11)])

#aid food quantity into kg
str(aid$aid_food_quantity)
str(aid$aid_food_units)
aid$aid_food_units <- as.factor(aid$aid_food_units)
summary(aid$aid_food_units)

aid$aid_food_quantity_kg <- as.numeric(ifelse(aid$aid_food_units=="quintales" &
                                                aid$aid_food_quantity>0, aid$aid_food_quantity*45.36,
                                              ifelse(aid$aid_food_units=="libras" &
                                                       aid$aid_food_quantity>0.1, aid$aid_food_quantity*0.4536,
                                                     ifelse(aid$aidreceived=="N",0,
                                                            NA))))

#get list of what type of animal aid received
my_list <- aid$animal_aid_received
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "animal_aid_received"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#insert in aid dataset
aid$animal_aid_received <- as.factor(take_from_list$animal_aid_received_chicken)


#get list of what type of animal aid received outcome
my_list <- aid$animal_aid_1_outcome
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "animal_aid_1_outcome"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

aid$animal_aid_1_outcome <- take_from_list$V1

#standarise yes/no varibles for aid received
aid$aid_Food <- as.factor(ifelse(!is.na(aid$aid_other), "Yes", as.character(aid$aid_Food)))

aid$aidreceived <- as.factor(aid$aidreceived)
summary(aid$aidreceived)
aid$aidreceived <- as.factor(ifelse(do.call(paste, c(aid[c(18:27)], sep="-"))!="No-No-No-No-No-No-No-No-No-No",
                                    "Yes","No"))

#create final dataset for AID
names(aid)
aid2_0 <- aid[,-c(3:6,9,11:16)]

##### FOOD information ######
names(food)
food$foodshortagetime <- as.factor(ifelse(food$foodshortagetime=="Y","Yes","No"))
summary(food$foodshortagetime)
#food worry
food$food_worry <- as.factor(food$food_worry)
summary(food$food_worry)
#amount of months with low food
str(food$foodshortagetime_months_amount)
food$foodshortagetime_months_amount <- as.integer(ifelse(food$foodshortagetime_months_amount>13,3,
                                                         ifelse(food$foodshortagetime=="No" &
                                                                  is.na(food$foodshortagetime_months_amount),0,
                                              food$foodshortagetime_months_amount)))
food$foodshortagetime_months_amount[is.na(food$foodshortagetime_months_amount)] <- 0


#get list of which months they felt food worry
my_list <- food$foodshortagetime_months_which
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "foodshortagetime"
#options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
options = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

##join to food dataset
food2_0 <- cbind(food[,c(1:3)], take_from_list[,c(14:25)])

#gather wildfoods
food$gather_wildfoods <- as.factor(food$gather_wildfoods)
summary(food$gather_wildfoods)
#######
#get list of collected wild food 
my_list <- food$wildfoods
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "gather_wildfoods"
options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
#options = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#complete information Y/N about gather_wildfoods
food$gather_wildfoods <- as.factor(ifelse(take_from_list$paste=="NA-NA-NA-NA","No","Yes"))
summary(food$gather_wildfoods)
#join to food2_0 dataset
food2_0 <- cbind(food2_0, food[,c(5:14)], take_from_list[,c(6:11)])

##which month they collect wild food
my_list <- food$wildfood_collect_when
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
take_from_list$paste <- do.call(paste, c(take_from_list[1:ncol(take_from_list)], sep="-"))
name_x = "wild_food_collected"
#options = sort(unique(as.vector(unlist(take_from_list[,c(1:(ncol(take_from_list)-1))])))) 
options = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for (i in options){
  take_from_list$y <- as.factor(ifelse(grepl(i,take_from_list$paste),"Yes","No"))
  colnames(take_from_list)[colnames(take_from_list)=="y"] <- paste(name_x, i , sep="_")  
}

#add to food2_0 dataset
food2_0 <- cbind(food2_0, take_from_list[,c(14:25)])

#the amount of wild food collected
my_list <- food$wildfood_amount
n_option <- max(unlist(lapply(my_list,FUN=length)))
take_from_list <- matrix(NA,length(my_list), n_option)
for (i in 1:length(my_list)) {
  data <- my_list[[i]]
  if (length(data)>0) {
    subset <- my_list[[i]]
    take_from_list[i,1:length(subset)] <- subset
  }
}
take_from_list <- as.data.frame(take_from_list)
#add to food2_0 dataset
food2_0$wildfood_amount <- take_from_list$V1
names(food)
food2_0 <- cbind(food2_0, food[,c(19:54)])

#remome temporary dataset
rm(x)

##### Prepare data to export #####
#clean working space
rm(aid)
rm(bean_know)
rm(bean_prod_detailed)
rm(bean_prod)
rm(food)
rm(income)
rm(livestock)
rm(other_crop)
rm(social)
rm(other_crop_detailed)

#create mean of lat lon to anonymize household location
for (i in unique(as.character(geo$Comunidad))){
  geo$lat_mean[geo$Comunidad == i] <- mean(geo$lat[geo$Comunidad == i], na.rm=TRUE)
  geo$lon_mean[geo$Comunidad == i] <- mean(geo$lon[geo$Comunidad == i], na.rm=TRUE)
}

for (i in unique(geo$ADM2_NAME)){
  geo$lat_mean[geo$ADM2_NAME == i & is.na(geo$Comunidad)] <- mean(geo$lat[geo$ADM2_NAME == i], na.rm=TRUE)
  geo$lon_mean[geo$ADM2_NAME == i & is.na(geo$Comunidad)] <- mean(geo$lon[geo$ADM2_NAME == i], na.rm=TRUE)
}

# Join all data and export csv file #
all_data <- merge(geo,
                  social2_0,by="id", all.y=TRUE)

all_data <- merge(all_data,
           bean_prod2_0,by="id", all.y=TRUE)

all_data <- merge(all_data,
           bean_know2_0,by="id", all.y=TRUE)

all_data <- merge(all_data,
           income2_0,by="id", all.y=TRUE)

all_data <- merge(all_data,
           livestock2_0,by="id", all.y=TRUE)

all_data <- merge(all_data,
           other_crop2_0,by="id", all.y=TRUE)

all_data <- merge(all_data,
           aid2_0,by="id", all.y=TRUE)

all_data <- merge(all_data,
                  food2_0,by="id", all.y=TRUE)


#### to dataverse #####
#read file with standarised var names ####
var_names <- read.csv("./input/var_names.csv")
names(all_data)
names(all_data) <- as.character(var_names$var_standar)
#reorder variables
var_names %<>% 
  arrange(. , order) %>%
  filter(., Dataverse==1) %>%
  dplyr::select(. , var_standar) %>%
  as.matrix() %>%
  as.vector(.)

#keep only variables sellected for dataverse
data_verse <- all_data[,var_names]

#write outputs
write.csv(data_verse, "./prueba3_baseline_4dataverse/Prueba3_baseline_4dataverse.csv", row.names = F)

write.csv(other_crop_detailed2_0, "./prueba3_baseline_4dataverse/other_crops.csv", row.names = F)

write.csv(bean_prod_detailed2_0, "./prueba3_baseline_4dataverse/bean_prod.csv", row.names = F)


 save(data_verse, aid, bean_prod, bean_know, food,income,livestock,other_crop, social,
     file = "./prueba3_baseline_4dataverse/Prueba3_baseline_4dataverse.RData" ) 





rm(j)
rm(name_x)
rm(i)
rm(climmob_dir)
rm(baseline_dir)
rm(data)
rm(informacion_names)
rm(layer_dir)
rm(rechazar_names)
rm(subset)
rm(time_var)


