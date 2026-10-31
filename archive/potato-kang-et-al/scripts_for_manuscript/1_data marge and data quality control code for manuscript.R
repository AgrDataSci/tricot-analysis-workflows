###############################################################################
##############################tricot data cleaning#############################
dt <- read.csv("data/potato-data-tricot_fixed.csv") #bring potato tricotdat CSV file. BTW, it is the fixed version (merged PH2 and PH2_update as one)

 

#check data structure (458obs of 160 variables)
str(dt)
head(dt)
tail(dt)
summary(dt)


#Orignal tricot data has mistakes to put dormancy answer oppositely. 
#Let's fix this one. Create temperated data frame. 
temp<- dt[,c( "postharvest2_dormancy_pos" ,"postharvest2_dormancy_neg", "postharvest3_dormancy_pos", "postharvest3_dormancy_neg")]
temp2<- dt[,c( "postharvest2_dormancy_neg" ,"postharvest2_dormancy_pos", "postharvest3_dormancy_neg", "postharvest3_dormancy_pos")] #change position 
colnames(temp2) <-c("postharvest2_dormancy_pos" ,"postharvest2_dormancy_neg", "postharvest3_dormancy_pos", "postharvest3_dormancy_neg") # change name
colnames(temp2)
#apply changed dormancy data fate into the main data frame.  
dt[,c( "postharvest2_dormancy_pos" ,"postharvest2_dormancy_neg", "postharvest3_dormancy_pos", "postharvest3_dormancy_neg")] <- temp2[]
# check whether it changed well or not.  
dt[,"postharvest2_dormancy_pos"] == temp2[,"postharvest2_dormancy_pos"]
dt[,"postharvest2_dormancy_pos"] == temp[,"postharvest2_dormancy_pos"]
####################################################################################
####################################################################################
# add external envrionment covariate data to tricot data   

# recall packages
library("PlackettLuce")
library("leaflet")
library("nasapower")
library("tibble")
library("climatrends")
library("chirps")
library("sp")
library("Rcpp")
library("sf")


###########
#calculate planting day
dt$vegetative1_survey_start<- as.Date(dt$vegetative1_survey_start, format = "%d/%m/%Y")
dt$vegetative1_survey_start 

#calculate harvesting day 
dt$postharvest1_survey_start<- as.Date(dt$postharvest1_survey_start, format = "%d/%m/%Y")
dt$postharvest1_survey_start

#devide data frame to each projects for easy to get data 
AIP <- dt[which(dt$package_project_name == "21AIP"),]
BIP <- dt[which(dt$package_project_name == "21BIP"),]
Pot21A <- dt[which(dt$package_project_name == "Pot21A"),]
POT21B <- dt[which(dt$package_project_name == "POT21B"),]



# put result data about seasons in data frame but first create new columns 
dt <- add_column(dt, "season" = NA, .after = "Total_sum_QS_point.with.BW")

#put season data in season column
dt[1:80,"season"] <- "Season_A"
dt[81:160,"season"] <- "Season_B"
dt[161:310,"season"] <- "Season_A"
dt[311:463,"season"] <- "Season_B"
#checking result 
ifelse( dt[1:80,"season"] == "Season_A" & dt[1:80,"package_project_name"] == "21AIP", T, F)
ifelse( dt[81:160,"season"] == "Season_B" & dt[81:160,"package_project_name"] == "21BIP", T, F)
ifelse( dt[161:310,"season"] == "Season_A" & dt[161:310,"package_project_name"] == "Pot21A", T, F)
ifelse( dt[311:463,"season"] == "Season_B" & dt[311:463,"package_project_name"] == "POT21B", T, F)

################################################################################
########################Temperature########################################################
####get temperature data from Climatrends 
#before staring, lets transform factors 
dt$longitude <- as.numeric(dt$longitude)
dt$latitude <- as.numeric(dt$latitude)

#check data structure (458 obs. of 160 variables) becuase 5 data sets did not have gps and so I deducted nod gps having data. 
dt<- dt[-c(which(is.na(dt[,c("longitude","latitude")]))),] #delete none GPS tricot data  



####get temperature data from NASA_POWER via "Climatrends_package" 


dt <-add_column(dt, "t_planting_day" = (dt[,"vegetative1_survey_start"]- 45) )
dt <-add_column(dt, "t_harvesting_day" = (dt[,"postharvest1_survey_start"]- 5) )

#calculate planting_day, especially NA plot by checking other stage_survey_start
dt[78,"t_planting_day"]

dt[78,"vegetative2_survey_start"] <- as.character("2020-12-27")
as.Date(dt[78,"vegetative2_survey_start"]) - 75
dt[78,"t_planting_day"] <- (as.Date(dt[78,"vegetative2_survey_start"]) - 75)
which(is.na(dt$t_planting_day))

#calculate harvest_day, especially NA plot by checking other stage_survey_start
which(is.na(dt$t_harvesting_day))

mean(dt$t_harvesting_day - dt$t_planting_day, na.rm =T)
#average range day from planting to harvest is 110 

dt[which(is.na(dt$t_harvesting_day)),"t_harvesting_day"] <- dt[which(is.na(dt$t_harvesting_day)),"t_planting_day"] + 110



temp<- temperature(dt[,c("longitude","latitude")], day.one = dt$t_planting_day,
                   last.day =  dt$t_harvesting_day) 



#######getting GDD

#gdd_potato <- GDD(temp$maxDT , temp$minNT, tbase = 7, equation = "default") # tbase and equation based on (Worthington CM, Hutchinson CM 2005)
#file:///C:/Users/reret/Downloads/perrycollins-098-101%20(2).pdf
#however, this code measures heat accumulation based on the growing degree-days that saved in the data frame .
# I need to return the number of days which is a given organism required to reach a certain number of accumulated degree-days. Therfore,I need degree-days


potato_gdd <- GDD(dt[,c("longitude","latitude")], 
                  day.one = dt[,"t_planting_day"],
                  last.day =  dt[,"t_harvesting_day"],
                  degree.days =  1281.985, return.as = "ndays")
table(potato_gdd)

##########################
#########crop_sensitive 

potato_SI<- crop_sensitive(dt[,c("longitude","latitude")], 
                           day.one =dt[,"t_planting_day"],
                           last.day = dt[,"t_harvesting_day"], 
                           as.sf = FALSE)
table(potato_SI)
#####################combine all climatrends result.
envi_phenology <- cbind(temp, potato_gdd)
envi_phenology <- cbind(envi_phenology, potato_SI)
envi_phenology
table(envi_phenology[,14])
table(envi_phenology[,15])
table(envi_phenology[,16])
table(envi_phenology[,17])
table(envi_phenology[,18])
table(envi_phenology[,19])
table(envi_phenology[,20])
table(envi_phenology[,21])
envi_phenology <- envi_phenology[,-c(14:21)]

table(envi_phenology[,17])
table(envi_phenology[,18])
table(envi_phenology[,19])
table(envi_phenology[,20])
table(envi_phenology[,21])
table(envi_phenology[,22])
envi_phenology <- envi_phenology[,-c(17:22)]

dt <- cbind(dt, envi_phenology)
view(dt[])


################################################################################
########################Rain########################################################
####get temperature data from Climatrends

row.names(dt)<-c(1:458)
######################################
#get precipitation data from chirps 
# get  precipitation data from 1 row 
rownames(dt) <- c(seq_along(dt$id))
lonlat <- dt[,c("longitude","latitude")]



#total days through the entire experiment. because Chirps download only one time... I don't know why.
days <- c("2020-09-26", "2021-08-15")


rain_data <- get_chirps(lonlat, days, server = "CHC")
#so huge data frame!!!! 
#it works 

rain<- rain_data[]

#season A range is from planting day to harvesting day
#season B range is from planting day to harvesting day
min(dt$t_planting_day)
max(dt$t_harvesting_day)




test <- subset(rain, rain$id == 1)
test <- test[1:(dt[1,"t_harvesting_day"] + 1 - dt[1,"t_planting_day"]),]

y <-list()
for (i in 1:458) {output <- dt[i,"t_harvesting_day"] + 1 - dt[i,"t_planting_day"]
y[[i]]<- output}
y <- as.data.frame(y)
y <-t(y)
rownames(y) <- c(1:458)
y<- gsub(" days", "", y)
y<-as.numeric(y)

#create loop to extract certain days and make data.frame that has sum and mean
precipi_list = list()
for(i in 1:458){x<- subset(rain, rain$id == i)
x <- x[1:y[i],]
precipi_list[[i]]<- data.frame(i,sum(x$chirps), mean(x$chirps), sum(x$chirps ==0))} 
precipitation_data <- do.call(rbind, precipi_list)


names(precipitation_data) <- c("i", "totalsum_rainfall", "mean_rainfall", "sum_no_rain")


covariate_rainfall <- precipitation_data[]



dt <-cbind(dt,covariate_rainfall) 

#############################################################################3
##########precipitation done!!


###############################################################################
###################download SOIL data##########################################

#run packages!!
library("raster")
library("rgdal")
library("ggplot2")

# Load raster into R
raster_soc <- raster("raster/CroppedSoilData.grd", band = 1)
raster_cec <- raster("raster/CroppedSoilData.grd", band = 2)
raster_nitrogen <- raster("raster/CroppedSoilData.grd", band = 3)
raster_pH  <- raster("raster/CroppedSoilData.grd", band = 4)
raster_soil_group  <- raster("raster/soil_group.tif")

# View raster structure
raster_soc
raster_cec
raster_nitrogen
raster_pH
raster_soil_group
#layer 1= Soil organic carbon, layer 2=Cation exchange capacity, layer 3=nitrogen, layer 4=pH)

# convert to a df for plotting in two steps,
# First, to a SpatialPointsDataFrame
raster_soc_pts <- rasterToPoints(raster_soc, spatial = TRUE)
raster_cec_pts <- rasterToPoints(raster_cec, spatial = TRUE)  
raster_nitrogen_pts <- rasterToPoints(raster_nitrogen, spatial = TRUE)
raster_pH_pts <- rasterToPoints(raster_pH, spatial = TRUE)
raster_soil_group_pts <-rasterToPoints(raster_soil_group, spatial = TRUE)

# Then to a 'conventional' dataframe
raster_soc_df  <- data.frame(raster_soc_pts)
raster_cec_df <- data.frame(raster_cec_pts)
raster_nitrogen_df <- data.frame(raster_nitrogen_pts)
raster_pH_df <- data.frame(raster_pH_pts)
raster_soil_group_df <-data.frame(raster_soil_group_pts)


##get point data from raster
######https://gisday.wordpress.com/2014/03/24/extract-raster-values-from-points-using-r/comment-page-1/
gps_keep <- dt[,1:3]
coordinates(gps_keep)= ~ longitude + latitude

#stack layers 
rasStack = stack(raster_soc,
                 raster_cec,
                 raster_nitrogen,
                 raster_pH)

rasValue=extract(rasStack, gps_keep)
combinePointValue=cbind(gps_keep,rasValue)

write.table(combinePointValue,file="combinedPointValue.csv", append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

#soil group
rasValue=extract(raster_soil_group, gps_keep)
combinePointValue=cbind(combinePointValue,rasValue)

write.table(combinePointValue,file="data/combinedPointValue_soil_all.csv", append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
#find index here :https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.rat.xml

##recall combinedPointValue_soil_all.csv to change column names
df_soil <- read.csv("data/combinedPointValue_soil_all.csv")
str(df_soil) 
colnames(df_soil)[1]<-"id_soil"
colnames(df_soil)[2]<-"Soil organic carbon"
colnames(df_soil)[3]<-"Cation exchange capacity(at ph7)"
colnames(df_soil)[4]<-"nitrogen"
colnames(df_soil)[5]<-"pH_water"
colnames(df_soil)[6]<-"Soil_classification"

# lets change soil_classification index to real name by referencing :https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.rat.xml
table(df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("0", "Acrisols", df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("3", "Andosols", df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("6", "Cambisols", df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("1Acrisols", "Ferralsols", df_soil$Soil_classification)
#layer 1= Soil organic carbon, layer 2=Cation exchange capacity, layer 3=nitrogen, layer 4=pH)

df_soil <- df_soil[,-c(7:9)]
## combind soil data fram with tricot

dt <- cbind(dt,df_soil)
table(ifelse(dt$id == dt$id_soil, T, F)) #all data saved well.
table(dt$Soil_classification)
###############################################################################
###############################################################################
# add external envrionment covariate data to tricot data   
# recall packages
library("tibble")
library("dplyr")   
library("stringr")


datacleaning <- read.csv("data/RHoMIS_RAB_21A_21B.csv", header=T, na.strings=c("","NA"))

str(datacleaning)
datacleaning[,"X_xform_id_string"]<- gsub("RHoMIS_Minimal_v1_6", "Pot21A", datacleaning$X_xform_id_string)
datacleaning[,"X_xform_id_string"]<- gsub("RHoMIS_21BPot", "POT21B", datacleaning$X_xform_id_string)

datacleaning[,"X_xform_id_string"]<- paste(datacleaning$X_xform_id_string, datacleaning$householdid,sep="-")

names(datacleaning)[5] <- c("RHoMIS_id")


###################assign agricultural intensification#
#agricultural intensification can be measured by quantifying input resource and farmers land size

RHoMis<- datacleaning[]
RHoMis_add <- RHoMis[]

RHoMis_add <- add_column(RHoMis_add, "AI_landcultivated_points"= NA, .after = "landcultivated")
RHoMis_add$AI_landcultivated_points<- ifelse(RHoMis_add$landcultivated > 2 , 1, 0)

RHoMis_add <- add_column(RHoMis_add, "AI_farm_labour_points"= NA, .after = "farm_labour")
RHoMis_add$AI_farm_labour_points <-ifelse(grepl("Hire_labour", RHoMis_add$farm_labour), 1, 0)

#
RHoMis_add <- add_column(RHoMis_add, "AI_ag_inputs_points"= NA, .after = "ag_inputs_1")
RHoMis_add$AI_ag_inputs_points <-ifelse(grepl("Fertilisers", RHoMis_add$ag_inputs_1) |grepl("Hybrid_Seeds", RHoMis_add$ag_inputs_1) | grepl("Pesticides", RHoMis_add$ag_inputs_1),1,0)


RHoMis_add <- add_column(RHoMis_add, "AI_ag_inputs2_points"= NA, .after = "ag_inputs_2")
RHoMis_add$AI_ag_inputs2_points <-ifelse(grepl("Fertilisers", RHoMis_add$ag_inputs_2) |grepl("Hybrid_Seeds", RHoMis_add$ag_inputs_2) | grepl("Pesticides", RHoMis_add$ag_inputs_2),1,0)


RHoMis_add <- add_column(RHoMis_add, "AI_ag_inputs3_points"= NA, .after = "ag_inputs_3")
RHoMis_add$AI_ag_inputs3_points <-ifelse(grepl("Fertilisers", RHoMis_add$ag_inputs_3) |grepl("Hybrid_Seeds", RHoMis_add$ag_inputs_3) | grepl("Pesticides", RHoMis_add$ag_inputs_3),1,0)


RHoMis_add <- add_column(RHoMis_add, "AI_ag_inputs4_points"= NA, .after = "ag_inputs_4")
RHoMis_add$AI_ag_inputs4_points <-ifelse(grepl("Fertilisers", RHoMis_add$ag_inputs_4) |grepl("Hybrid_Seeds", RHoMis_add$ag_inputs_4) | grepl("Pesticides", RHoMis_add$ag_inputs_4),1,0)


RHoMis_add <- add_column(RHoMis_add, "AI_ag_inputs5_points"= NA, .after = "ag_inputs_5")
RHoMis_add$AI_ag_inputs5_points <-ifelse(grepl("Fertilisers", RHoMis_add$ag_inputs_5) |grepl("Hybrid_Seeds", RHoMis_add$ag_inputs_5) | grepl("Pesticides", RHoMis_add$ag_inputs_5),1,0)

RHoMis_add <- add_column(RHoMis_add, "AI_ag_inputs_points_sum"= 
                           rowSums(RHoMis_add[,c("AI_ag_inputs_points","AI_ag_inputs2_points","AI_ag_inputs3_points", "AI_ag_inputs4_points","AI_ag_inputs5_points" )]), .after = "ag_inputs_6")
#


RHoMis_add <- add_column(RHoMis_add, "AI_farm_irrigation_points"= NA, .after = "land_irrigated")
RHoMis_add$AI_farm_irrigation_points <-ifelse(grepl("Y", RHoMis_add$land_irrigated), 1, 0)




RHoMis_add <- add_column(RHoMis_add, "Agriculture_intensification_level"= 
                           rowSums(RHoMis_add[,c("AI_landcultivated_points","AI_farm_labour_points","AI_ag_inputs_points_sum", "AI_farm_irrigation_points")]), .after = "Farm_Income_USD")


write.csv(RHoMis_add,"MyRprojectRHoMis_edited.csv", row.names = FALSE) #export CSV file. 

#############################################
##Descritive statistic


table(RHoMis_add[,"region"])
#  Gicumbi               Nya6            Nyabihu          Nyamabage          Nyamagabe Nyamagabe District 
#97                  1                 98                  1                 83                  2 
prop.table(table(RHoMis_add[,"respondentsex"]))
#F         M 
#0.4539007 0.5460993 

prop.table(table(RHoMis_add[,"respondent_is_head"]))
#N         Y 
#0.2588652 0.7411348 
prop.table(table(RHoMis_add[,"household_type"]))
#couple   man_single woman_single 
#0.8865248    0.0106383    0.1028369
prop.table(table(RHoMis_add[,"work_away"]))
#both_work_away   man_works_away       none     woman_works_away 
#0.12056738       0.07801418       0.77304965       0.02836879 
prop.table(table(RHoMis_add[,"education_level"]))
#    No_school postsecondary       primary     secondary 
#0.09929078    0.03546099    0.53191489    0.33333333 

crop_rhomis<- RHoMis_add[,c("crop_1","crop_2","crop_3","crop_4","crop_5","crop_6","crop_7","crop_8")]
crop_rhomis <- add_column(crop_rhomis, "crop_all"= NA, .after = "crop_8")

crop_rhomis <- add_column(crop_rhomis, "crop_all1"= NA, .after = "crop_all")
crop_rhomis <- add_column(crop_rhomis, "crop_all2"= NA, .after = "crop_all1")
crop_rhomis <- add_column(crop_rhomis, "crop_all3"= NA, .after = "crop_all2")


crop_rhomis$crop_all<- paste(crop_rhomis$crop_1, crop_rhomis$crop_2, sep =", ")
crop_rhomis$crop_all1<- paste(crop_rhomis$crop_3, crop_rhomis$crop_4, sep =", ")
crop_rhomis$crop_all2<- paste(crop_rhomis$crop_5, crop_rhomis$crop_6, sep =", ")
crop_rhomis$crop_all3<- paste(crop_rhomis$crop_7, crop_rhomis$crop_8, sep =", ")

crop_rhomis <- add_column(crop_rhomis, "crop_all_a"= NA, .after = "crop_all3")
crop_rhomis <- add_column(crop_rhomis, "crop_all3_b"= NA, .after = "crop_all4")


crop_rhomis$crop_all_a<- paste(crop_rhomis$crop_all, crop_rhomis$crop_all1, sep =", ")
crop_rhomis$crop_all3_b<- paste(crop_rhomis$crop_all2, crop_rhomis$crop_all3, sep =", ")

crop_rhomis$crop_all<- paste(crop_rhomis$crop_all_a, crop_rhomis$crop_all3_b, sep =", ")

crop_rhomis <-crop_rhomis[,-c(10,11,12,13,14)] 
crop_rhomis[]<- crop_rhomis[complete.cases(crop_rhomis[,"crop_all"]),]


crop_rhomis <- add_column(crop_rhomis, "Maize"= NA, .after = "crop_all")
crop_rhomis$Maize<- ifelse(grepl("Maize", crop_rhomis$crop_all), 1,0 )
prop.table(table(crop_rhomis[,"Maize"]))


crop_rhomis <- add_column(crop_rhomis, "Potato"= NA, .after = "crop_all")
crop_rhomis$Potato<- ifelse(grepl("Potato", crop_rhomis$crop_all), 1,0 )
prop.table(table(crop_rhomis[,"Potato"]))


crop_rhomis <- add_column(crop_rhomis, "field_beans"= NA, .after = "crop_all")
crop_rhomis$field_beans<- ifelse(grepl("field_beans", crop_rhomis$crop_all), 1,0 )
prop.table(table(crop_rhomis[,"field_beans"]))


crop_rhomis <- add_column(crop_rhomis, "Irish_Potatoes"= NA, .after = "crop_all")
crop_rhomis$Irish_Potatoes<- ifelse(grepl("Irish Potatoes", crop_rhomis$crop_all), 1,0 )
prop.table(table(crop_rhomis[,"Irish_Potatoes"]))





#####################################
#progressing create RHoMIS covariate 
RHoMIs_covariate <- RHoMis_add[]


###########Testtable
#potato important yes = 1 , no = 0

strsplit(RHoMIs_covariate$crops, " " )
RHoMIs_covariate<- add_column(RHoMIs_covariate, "potato_important" = NA, .after = "Agriculture_intensification_level" )

RHoMIs_covariate[which(grepl("potato", RHoMIs_covariate$crops)), "potato_important"] <- 1
RHoMIs_covariate[which(is.na(RHoMIs_covariate$potato_important)), "potato_important"] <- 0

table(RHoMIs_covariate[,"potato_important"])

vv<- subset(RHoMIs_covariate,RHoMIs_covariate$respondent_is_head == "Y")
vvn<- subset(RHoMIs_covariate,RHoMIs_covariate$respondent_is_head == "N")


table(vv$respondentsex)
table(vvn$respondentsex)

RHoMIs_covariate<- add_column(RHoMIs_covariate, "true_householdheader" = NA, .after = "respondent_is_head" )
RHoMIs_covariate$true_householdheader<- ifelse(RHoMIs_covariate$respondent_is_head == "Y",RHoMIs_covariate$true_householdheader <- RHoMIs_covariate$respondentsex, paste0( RHoMIs_covariate$respondentsex , "_o"))
keep  <- RHoMIs_covariate$true_householdheader[]
keep <- as.data.frame(keep)
keep[keep== "F_o",]<- "M"
keep[keep == "M_o",] <- "F"
RHoMIs_covariate$true_householdheader <-keep
table(RHoMIs_covariate$crops)

table(RHoMIs_covariate[which(RHoMIs_covariate$potato_important == 1),"true_householdheader"])
#F  M 
#31 80
#man tought potato is important 

table(RHoMIs_covariate[which(RHoMIs_covariate$potato_important == 0),"true_householdheader"])
#F   M 
#34 137

table(RHoMIs_covariate[which(RHoMIs_covariate$potato_important == 1),"respondentsex"])
#F  M 
#49 62
#man tought potato is important 
table(RHoMIs_covariate[which(RHoMIs_covariate$potato_important == 0),"respondentsex"])

#F   M 
#79 92 

###########
selected_factors <- c("RHoMIS_id","respondentsex","respondent_is_head","true_householdheader", "education_level","household_type", "count_people", "landcultivated",
                      "landowned", "land_ownership", "farm_labour","potato_important",
                      "Agriculture_intensification_level", "Asset_Count",
                      "Crop_Diversity", "Ag_Input_Diversity", "Livestock_Diversity",
                      "Livestock_TLU_Score", "Hunger_Months_Count", "Crop_Income_USD",
                      "Livestock_Income_USD", "Farm_Income_USD",
                      "X_longitude", "X_latitude", "householdid", "region",
                      "sublocation", "village", "household_position", "crops", "crop_count", "land_irrigated",
                      "agric_inputs_other", "livestock", "livestock_count", "offfarm_incomes_any", 
                      "offfarm_income_proportion", "offfarm_who_control_revenue") 

RHoMIs_covariate1 <-list()
for(i in seq_along(selected_factors)){
  output  <- RHoMIs_covariate[,c(selected_factors[[i]])]
  RHoMIs_covariate1[[i]] <-output}
RHoMIs_covariate1 <- as.data.frame(RHoMIs_covariate1)
RHoMIs_covariate <- RHoMIs_covariate1[]


names(RHoMIs_covariate) <- c("RHoMIS_id","respondentsex","respondent_is_head","true_householdheader", "education_level","household_type", "count_people", "landcultivated",
                             "landowned", "land_ownership", "farm_labour","potato_important",
                             "Agriculture_intensification_level", "Asset_Count",
                             "Crop_Diversity", "Ag_Input_Diversity", "Livestock_Diversity",
                             "Livestock_TLU_Score", "Hunger_Months_Count", "Crop_Income_USD",
                             "Livestock_Income_USD", "Farm_Income_USD",
                             "X_longitude", "X_latitude", "householdid", "region",
                             "sublocation", "village", "household_position", "crops", "crop_count", "land_irrigated",
                             "agric_inputs_other", "livestock", "livestock_count", "offfarm_incomes_any", 
                             "offfarm_income_proportion", "offfarm_who_control_revenue") 

#check empty cells in data frame. 
for(i in seq_along(RHoMIs_covariate)){print(i) & print(table(RHoMIs_covariate[,i], exclude = NULL))}

RHoMIs_covariate[is.na(RHoMIs_covariate)] <- "None_respond"
###########################################3
#cleaning none useful factors
table(RHoMIs_covariate$offfarm_who_control_revenue)
#no_respond 186
table(RHoMIs_covariate$offfarm_income_proportion)
#no_respond 186 
table(RHoMIs_covariate$offfarm_incomes_any)

table(RHoMIs_covariate$livestock_count)
table(RHoMIs_covariate$livestock)
table(RHoMIs_covariate$agric_inputs_other)
table(RHoMIs_covariate$land_irrigated)
table(RHoMIs_covariate$crop_count)
table(RHoMIs_covariate$crops)
table(RHoMIs_covariate$household_position)

RHoMIs_covariate <- RHoMIs_covariate[,c("RHoMIS_id","respondentsex","respondent_is_head","true_householdheader", "education_level","household_type", "count_people", "landcultivated",
                                        "landowned", "land_ownership", "farm_labour","potato_important",
                                        "Agriculture_intensification_level", "Asset_Count",
                                        "Crop_Diversity", "Ag_Input_Diversity", "Livestock_Diversity",
                                        "Livestock_TLU_Score", "Hunger_Months_Count", "Crop_Income_USD",
                                        "Livestock_Income_USD", "Farm_Income_USD")]
#########################
# change farm_labour component 
#hire_labout = 1 , none =0 
table(RHoMIs_covariate$farm_labour)
RHoMIs_covariate[,"farm_labour"]
RHoMIs_covariate_t <- RHoMIs_covariate[]
RHoMIs_covariate_t[RHoMIs_covariate_t == "Hire_labour|Household|Reciprocal"] <-  "Hire_labour"
RHoMIs_covariate_t[RHoMIs_covariate_t == "Hire_labour|Household"] <-  "Hire_labour"
RHoMIs_covariate_t[RHoMIs_covariate_t == "Hire_labour|Reciprocal"] <-  "Hire_labour"
RHoMIs_covariate_t[RHoMIs_covariate_t == "Household|Reciprocal"] <-  "Household"



RHoMIs_covariate_t$farm_labour <- gsub("Hire_labour", "1", RHoMIs_covariate_t$farm_labour)
RHoMIs_covariate_t$farm_labour <- gsub("Household", "0", RHoMIs_covariate_t$farm_labour)
RHoMIs_covariate_t$farm_labour <- gsub("Reciprocal", "0", RHoMIs_covariate_t$farm_labour)
RHoMIs_covariate_t$farm_labour <- gsub("None_respond", "0", RHoMIs_covariate_t$farm_labour)

RHoMIs_covariate <- add_column(RHoMIs_covariate, "farm_labour_b" = RHoMIs_covariate_t$farm_labour, .after = "farm_labour")

#########################
#re-order dataframe as same order to tricot data frame 

#reorder data fram based on dt.
RHoMIs_covariate <- RHoMIs_covariate[match(dt$id, RHoMIs_covariate$RHoMIS_id),]
dt<- cbind(dt, RHoMIs_covariate)

dt_Keep<- dt[]

#backup
dt<- dt_Keep


write.csv(dt,"data/manuscript_temp.csv", row.names = FALSE) #export CSV file. 
###############################################################################
###############################################################################
#however, there are new convariate data for analyzing vegetative 2 stage(from planting to 75days later) 
dt <- read.csv("data/manuscript_temp.csv") 

 
# recall packages
library("PlackettLuce")
library("leaflet")
library("nasapower")
library("tibble")
library("climatrends")
library("chirps")
library("sp")
library("Rcpp")
library("sf")

#
temp_d78<- dt[78,"vegetative2_survey_start"]
#convert dt$vegetative2_survey_start date form 
dt$vegetative2_survey_start<- as.Date(dt$vegetative2_survey_start, format = "%d/%m/%Y")
#I do not know when I change format why data of row 78 disappear.... 
#just but data diractly
dt[78,"vegetative2_survey_start"] <- temp_d78
#check NA at planting and harvesting days from previous dataframe
which(is.na(dt[,"t_planting_day"]))
which(is.na(dt[,"t_harvesting_day"]))
which(is.na(dt[,"vegetative2_survey_start"]))



plnt_day_temp<- as.Date(dt[which(is.na(dt$vegetative2_survey_start)),"t_planting_day"]) + 74

plnt_day_temp <-as.character(plnt_day_temp)
dt[which(is.na(dt$vegetative2_survey_start)),"vegetative2_survey_start"] 
lis_n_t<- which(is.na(dt$vegetative2_survey_start))
for(i in 1:3){dt[lis_n_t[i],"vegetative2_survey_start"]  <- plnt_day_temp[[i]]}


which(is.na(dt[,"vegetative2_survey_start"]))
dt[lis_n_t,"vegetative2_survey_start"]



##need to convert dt$vegetative2_survey_start

temp_vg_2 <- temperature(dt[,c("longitude","latitude")], day.one = dt$t_planting_day,
                         last.day =  dt$vegetative2_survey_start) 



#######getting GDD

gdd_potato <- GDD(temp$maxDT , temp$minNT, tbase = 7, equation = "default")
#however, this code measures heat accumulation based on the growing degree-days that saved in the data frame .
# I need to return the number of days which is a given organism required to reach a certain number of accumulated degree-days. Therfore,I need degree-days



potato_gdd_vg2 <- GDD(dt[,c("longitude","latitude")], 
                      day.one = dt[,"t_planting_day"],
                      last.day =  dt[,"vegetative2_survey_start"],
                      degree.days =  1007.350, return.as = "ndays")


##########################
#########crop_sensitive 



potato_SI_vg2<- crop_sensitive(dt[,c("longitude","latitude")], 
                               day.one =dt[,"t_planting_day"],
                               last.day = dt[,"vegetative2_survey_start"], 
                               as.sf = FALSE)


#####################combine all climatrends result.
envi_phenology <- cbind(temp_vg_2, potato_gdd_vg2)
envi_phenology <- cbind(envi_phenology, potato_SI_vg2)
envi_phenology
table(envi_phenology[,14])
table(envi_phenology[,15])
table(envi_phenology[,16])
table(envi_phenology[,17])
table(envi_phenology[,18])
table(envi_phenology[,19])
table(envi_phenology[,20])
table(envi_phenology[,21])
envi_phenology <- envi_phenology[,-c(14:21)]

table(envi_phenology[,17])
table(envi_phenology[,18])
table(envi_phenology[,19])
table(envi_phenology[,20])
table(envi_phenology[,21])
table(envi_phenology[,22])
envi_phenology <- envi_phenology[,-c(17:22)]

dt <- cbind(dt, envi_phenology)
view(dt[])

which(is.na(dt[,"vegetative2_survey_start"]))


# done
#save
dt_keep2<-dt[]
dt<- dt_keep2[]

################################################################################
########################Rain########################################################
####get temperature data from Climatrends


######################################
#get precipitation data from chirps 
# get  precipitation data from 1 row 
rownames(dt) <- c(seq_along(dt$id))
lonlat <- dt[,c("longitude","latitude")]



#total days through the entire experiment. because Chirps download only one time... I don't know why.
days <- c("2020-09-26", "2021-08-15")


rain_data <- get_chirps(lonlat, days, server = "CHC")
#so huge data frame!!!! 
#it works 


#write.csv(rain_data, "rain_data_v1.csv") #<---- for save!!  I dont want to recall it again.. ;)
rain_data<- read.csv("rain_data_v1.csv")
rain<- rain_data[]

#season A range is from planting day to harvesting day
#season B range is from planting day to harvesting day
min(dt$t_planting_day)
max(dt$t_harvesting_day)

dt[,"vegetative2_survey_start"] <- as.Date(dt[,"vegetative2_survey_start"])
dt[,"t_planting_day"] <- as.Date(dt[,"t_planting_day"])


test <- subset(rain, rain$id == 1)
test <- test[1:(dt[1,"vegetative2_survey_start"] + 1 - dt[1,"t_planting_day"]),]

y <-list()
for (i in 1:458) {output <- dt[i,"vegetative2_survey_start"] + 1  - dt[i,"t_planting_day"]
y[[i]]<- output}
y <- as.data.frame(y)
y <-t(y)
rownames(y) <- c(1:458)
y<- gsub(" days", "", y)
y<-as.numeric(y)

#create loop to extract certain days and make data.frame that has sum and mean
precipi_list = list()
for(i in 1:458){x<- subset(rain, rain$id == i)
x <- x[1:y[i],]
precipi_list[[i]]<- data.frame(i,sum(x$chirps), mean(x$chirps), sum(x$chirps ==0))} 
precipitation_data <- do.call(rbind, precipi_list)


names(precipitation_data) <- c("i", "totalsum_rainfall", "mean_rainfall", "sum_no_rain")


covariate_rainfall <- precipitation_data[]



tricot <-cbind(dt,covariate_rainfall) 
dt<- tricot
#############################################################################3
##########precipitation done!!


###############################################################################
###################download SOIL data##########################################

#run packages!!
library("raster")
library("rgdal")
library("ggplot2")

# Load raster into R
raster_soc <- raster("raster/CroppedSoilData.grd", band = 1)
raster_cec <- raster("raster/CroppedSoilData.grd", band = 2)
raster_nitrogen <- raster("raster/CroppedSoilData.grd", band = 3)
raster_pH  <- raster("raster/CroppedSoilData.grd", band = 4)
raster_soil_group  <- raster("raster/soil_group.tif")

# View raster structure
raster_soc
raster_cec
raster_nitrogen
raster_pH
raster_soil_group
#layer 1= Soil organic carbon, layer 2=Cation exchange capacity, layer 3=nitrogen, layer 4=pH)

# convert to a df for plotting in two steps,
# First, to a SpatialPointsDataFrame
raster_soc_pts <- rasterToPoints(raster_soc, spatial = TRUE)
raster_cec_pts <- rasterToPoints(raster_cec, spatial = TRUE)  
raster_nitrogen_pts <- rasterToPoints(raster_nitrogen, spatial = TRUE)
raster_pH_pts <- rasterToPoints(raster_pH, spatial = TRUE)
raster_soil_group_pts <-rasterToPoints(raster_soil_group, spatial = TRUE)

# Then to a 'conventional' dataframe
raster_soc_df  <- data.frame(raster_soc_pts)
raster_cec_df <- data.frame(raster_cec_pts)
raster_nitrogen_df <- data.frame(raster_nitrogen_pts)
raster_pH_df <- data.frame(raster_pH_pts)
raster_soil_group_df <-data.frame(raster_soil_group_pts)

##get point data from raster
######https://gisday.wordpress.com/2014/03/24/extract-raster-values-from-points-using-r/comment-page-1/
gps_keep <- dt[,1:3]
coordinates(gps_keep)= ~ longitude + latitude

#stack layers 
rasStack = stack(raster_soc,
                 raster_cec,
                 raster_nitrogen,
                 raster_pH)

rasValue=extract(rasStack, gps_keep)
combinePointValue=cbind(gps_keep,rasValue)

write.table(combinePointValue,file="combinedPointValue.csv", append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

#soil group
rasValue=extract(raster_soil_group, gps_keep)
combinePointValue=cbind(combinePointValue,rasValue)

write.table(combinePointValue,file="data/combinedPointValue_soil_all.csv", append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
#find index here :https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.rat.xml

##recall combinedPointValue_soil_all.csv to change column names
df_soil <- read.csv("data/combinedPointValue_soil_all.csv")
str(df_soil) 
colnames(df_soil)[1]<-"id_soil"
colnames(df_soil)[2]<-"Soil organic carbon"
colnames(df_soil)[3]<-"Cation exchange capacity(at ph7)"
colnames(df_soil)[4]<-"nitrogen"
colnames(df_soil)[5]<-"pH_water"
colnames(df_soil)[6]<-"Soil_classification"

# lets change soil_classification index to real name by referencing :https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.rat.xml
table(df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("0", "Acrisols", df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("3", "Andosols", df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("6", "Cambisols", df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("1Acrisols", "Ferralsols", df_soil$Soil_classification)
#layer 1= Soil organic carbon, layer 2=Cation exchange capacity, layer 3=nitrogen, layer 4=pH)

df_soil <- df_soil[,-c(7:9)]
## combind soil data fram with tricot

dt <- cbind(dt,df_soil)
table(ifelse(dt$id == dt$id_soil, T, F)) #all data saved well.
table(dt$Soil_classification)

#save 
write.table(dt,file="data/potato-tricot-data-with-covariate_for_manuscript.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
##DONE!!!
################################################################################

################################################################################
############################Tricot_Quality control####################################
###############However, this quality control progress was conducted by using Excel
#Step 1. Check all data (460 farmers’ data) to know what data frame structure looks 
#like and how many participant data exist in each stage.


#Step 2. Exclude data that have no GPS (already done in the above code) and of which the computed quality score is 
#greater than 98 point (a total of 23 data were excluded).


#Step 3. Exclude data for which there is no response for both the best and 
#the worst comparisons because Plackett-Luce model cannot calculate all tied cases A= B = C. 
#So, the model cannot distinguish any observed cases or farmers missed to fill data in. 
#Therefore, deleting the case of NA responses about all tricot questions may be a good option.


#Step 4. Run Plackett-Luce model with each trait data, which had been evaluated 
#over a specific period of stages by farmers.


#Step 5. When evaluating the final assessment (overall preference, overall performance, and WPF), 
#the model runs using all farmer data that responded to the overall preference among them, 
#including farmer data that even failed to assess potato tubers at a specific stage. 
#The reason is that it is possible to make a conclusion based on their experience and knowledge, 
#even though farmers did not involve some stages.
#saved as potato-tricot-data-with-covariate_for_manuscript_QS
############################################################
##############################add  vg2 to vegetative2  covariate name#########################
#run package
library(dplyr)

#read potato-tricot-data-with-covariate_for_manuscript_QS
dt <- read.csv("data/potato-tricot-data-with-covariate_for_manuscript_QS.csv") 



#change covariate name  
names(dt)[136:161] <-  c("MaxDT"      ,                      "MinDT"           ,                 "MaxNT"  ,                         
                          "MinNT"       ,                     "DTR"              ,                "SU"      ,                        
                          "TR"           ,                    "CFD"               ,               "WSDI"     ,                       
                          "CSDI"          ,                   "T10p"               ,              "T90p"      ,                      
                          "GDD"            ,                  "CDI_mean_22"         ,             "CDI_mean_23"  ,                   
                          "CDI_mean_24"     ,                 "i"                    ,            "Totalsum_rainfall",               
                          "Average_rainfall"    ,                "sum_no_rain"           ,           "id_soil"           ,              
                          "Soil.organic.carbon",              "Cation.exchange.capacity.at.ph7." ,"Nitrogen"           ,             
                          "PH_water"            ,             "Soil_classification")   



names(dt)[162:184] <- c("RHoMIS_id"               ,          "Respondent_gender"           ,          "Respondent_is_head",               
                        "True_householdheader"      ,        "Education_level"         ,          "Household_type"      ,             
                         "Count_people"               ,       "Landcultivated"           ,         "Landowned"            ,            
                       "Land_ownership"               ,     "Farm_labour"             ,          "Farm_labour_b"         ,           
                        "Potato_important"             ,     "Agriculture_intensification_level", "Asset_Count"            ,          
                          "Crop_Diversity"                ,    "Ag_Input_Diversity"       ,         "Livestock_Diversity"     ,         
                          "Livestock_TLU_Score"            ,   "Hunger_months"     ,          "Crop_Income_USD"          ,        
                          "Livestock_Income_USD"            ,  "Farm_Income_USD")    



##change covariate name of  vegetative stage 2 name 

tt_dt_name<- colnames(dt[185:210])


tt_name<- c("MaxDT_vg2",
            "MinDT_vg2",
            "MaxNT_vg2",
            "MinNT_vg2",
            "DTR_vg2",
            "SU_vg2",
            "TR_vg2",
            "CFD_vg2",
            "WSDI_vg2",
            "CSDI_vg2",
            "T10p_vg2",
            "T90p_vg2",
            "GDD_vg2",
            "CDI_mean_22_vg2",
            "CDI_mean_23_vg2",
            "CDI_mean_24_vg2",
            "i_vg2",
            "Totalsum_rainfall_vg2",
            "Average_rainfall_vg2",
            "Sum_no_rain_vg2",
            "id_soil_vg2",
            "Soil.organic.carbon_vg2",
            "Cation.exchange.capacity.at.ph7._vg2",
            "Nitrogen_vg2",
            "PH_water_vg2",
            "Soil_classification_vg2")

names(dt)[185:210] <- tt_name
names(dt)[185:210]
 
#save 
write.table(dt,file="data/potato-tricot-data-with-covariate_for_manuscript_QS.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
##DONE!!!
