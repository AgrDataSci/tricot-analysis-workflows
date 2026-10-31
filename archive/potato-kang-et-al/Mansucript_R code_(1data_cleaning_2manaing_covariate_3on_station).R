###############################################################################
###################Tricot data cleaning and data control#######################
#Bring potato tricotdat CSV file. BTW, it is the fixed version (merged PH2 and PH2_update as one)
df <- read.csv("data/potato-data-tricot_fixed.csv") 

#Check data structure (458obs of 160 variables)
str(df)
head(df)
tail(df)
summary(df)


#Orignal tricot data has mistakes to put dormancy answer oppositely. 
#Let's fix this one. Create temperated data frame. 
temp<- df[,c( "postharvest2_dormancy_pos" ,"postharvest2_dormancy_neg", "postharvest3_dormancy_pos", "postharvest3_dormancy_neg")]
temp2<- df[,c( "postharvest2_dormancy_neg" ,"postharvest2_dormancy_pos", "postharvest3_dormancy_neg", "postharvest3_dormancy_pos")] #change position 
colnames(temp2) <-c("postharvest2_dormancy_pos" ,"postharvest2_dormancy_neg", "postharvest3_dormancy_pos", "postharvest3_dormancy_neg") # change name
colnames(temp2)
#apply changed dormancy data fate into the main data frame.  
df[,c( "postharvest2_dormancy_pos" ,"postharvest2_dormancy_neg", "postharvest3_dormancy_pos", "postharvest3_dormancy_neg")] <- temp2[]
# check if the changes were applied correctly
identical(df[, "postharvest2_dormancy_pos"], temp2[, "postharvest2_dormancy_pos"])
identical(df[, "postharvest2_dormancy_pos"], temp[, "postharvest2_dormancy_pos"])

#save dormancy answer corrected version
write.table(df,file="data/potato-data-tricot_fixed_dormancy_answer_corrected_ver.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


####################################################################################
#############################Data cleaning by using quality score###################

#We decided to use a data quality score to detect inconsistencies in the data(potato-data-tricot_fixed_dormancy_answer_corrected_ver.csv) by using Excel. 
#These scores were assigned based on farmers' responses, comparing each individual evaluation stage to whether the data were consistent.
# For example, if a farmer responded differently from the previous response, the data were assigned 0, 1, or 99 depending on the intensity of the discrepancy. 
#“0” was consistent as the perfect state. “1” was assigned if consistency was not severely compromised based on answers to questions related to data quality. 
#In case of little ambiguity, “2” was assigned. 99 points were assigned to the case of no consistency.
# The case of quality score allocating tuber preservation or not is the same way as trial success data quality scoring.
#Additionally, in the case of BW as the same approach, if the farmer answered the tricot question about BW resistance while acknowledging the presence of the bacteria, it was assigned as 0 because of consistency. 
#On the other hand, if the farmer answered the tricot question about resistance while not answering about the presence of bacteria in the field, 1 was assigned because it was considered inconsistent. 
#It was repeated twice because farmers responded to BW presence and tricot during two stages (VG1 and VG2).
#save the csv file name as potato-data-tricot_fixed_dormancy_answer_corrected_ver_QS.csv


####################################################################################
# add external envrionment covariate data to tricot data   


# Load required packages
library("nasapower")
library("tibble")
library("climatrends")
library("chirps")

# Read data 
df <- read.csv("data/potato-data-tricot_fixed_dormancy_answer_corrected_ver_QS.csv")

# Convert date columns to Date format
date_cols <- c("vegetative1_survey_start", "postharvest1_survey_start")
df[date_cols] <- lapply(df[date_cols], as.Date, format = "%d/%m/%Y")

# Divide data frame into different projects
AIP <- subset(df, package_project_name == "21AIP")
BIP <- subset(df, package_project_name == "21BIP")
Pot21A <- subset(df, package_project_name == "Pot21A")
POT21B <- subset(df, package_project_name == "POT21B")

# Put result data about seasons in data frame but first create new columns 
df <- add_column(df, "season" = NA, .after = "Total_sum_QS_point.with.BW")


# Create a new column for seasons and fill it with appropriate seasons 
df$season[1:80] <- "Season_A"
df$season[81:160] <- "Season_B"
df$season[161:310] <- "Season_A"
df$season[311:463] <- "Season_B"

# Check the results
ifelse( df[1:80,"season"] == "Season_A" & df[1:80,"package_project_name"] == "21AIP", T, F)
ifelse( df[81:160,"season"] == "Season_B" & df[81:160,"package_project_name"] == "21BIP", T, F)
ifelse( df[161:310,"season"] == "Season_A" & df[161:310,"package_project_name"] == "Pot21A", T, F)
ifelse( df[311:463,"season"] == "Season_B" & df[311:463,"package_project_name"] == "POT21B", T, F)
df[,c("package_project_name", "season")]


################################################################################
############################Add temperature covariate###########################
# Convert longitude and latitude to numeric
df$longitude <- as.numeric(df$longitude)
df$latitude <- as.numeric(df$latitude)

# Remove rows with missing GPS data
df <- df[complete.cases(df[,c("longitude", "latitude")]),]

# Create column to calculate planting and harvesting days
df <-add_column(df, "t_planting_day" = (df[,"vegetative1_survey_start"]- 45) )
df <-add_column(df, "t_harvesting_day" = (df[,"postharvest1_survey_start"]- 5) )

#calculate planting_day, especially NA plot by checking other stage_survey_start
which(is.na(df[,"t_planting_day"]))
df[which(is.na(df[,"t_planting_day"])),"vegetative2_survey_start"] <- as.character("2020-12-27")
df[78,"t_planting_day"] <- (as.Date(df[78,"vegetative2_survey_start"]) - 75)

# Calculate harvest_day that has NA 
which(is.na(df[,"postharvest1_survey_start"]))
#average range day from planting to harvest is 109.3287. let's ert 110 
mean(df$t_harvesting_day - df$t_planting_day, na.rm =T)

# Replace missing harvesting days with planting day + 110 days
df[which(is.na(df$t_harvesting_day)),"t_harvesting_day"] <- df[which(is.na(df$t_harvesting_day)),"t_planting_day"] + 110


####### Download temperature 
# Get temperature data from Climatrends using NASA_POWER
temp <- temperature(df[,c("longitude","latitude")], day.one = df$t_planting_day, last.day = df$t_harvesting_day)

####### Getting GDD

# gdd_potato <- GDD(temp$maxdf , temp$minNT, tbase = 7, equation = "default") # tbase and equation based on (Worthington CM, Hutchinson CM 2005)
# Worthington, C. M., & Hutchinson, C. M. (2005, December). Accumulated growing degree days as a model to determine key developmental stages and evaluate yield and quality of potato in Northeast Florida. In Proceedings of the Florida state horticultural society (Vol. 118, pp. 98-101).
# However, this code measures heat accumulation based on the growing degree-days that saved in the data frame.
# I need to return the number of days which is a given organism required to reach a certain number of accumulated degree-days. Therfore,I need degree-days


potato_gdd <- GDD(df[,c("longitude","latitude")], 
                  day.one = df[,"t_planting_day"],
                  last.day =  df[,"t_harvesting_day"],
                  degree.days =  1281.985, return.as = "ndays")


####### Crop_sensitive 
# Calculate potato sensitive index
potato_SI <- crop_sensitive(df[, c("longitude", "latitude")], 
                            day.one = df[,"t_planting_day"],
                            last.day = df[,"t_harvesting_day"], 
                            as.sf = FALSE)

# Combine all climatrends results
envi_phenology <- cbind(temp, potato_gdd, potato_SI)

# Remove unnecessary columns
envi_phenology <- envi_phenology[, -c(14:22)]

# Add envi_phenology to df and view it
df <- cbind(df, envi_phenology)

###########################Add Percipitation covariate##########################
# Assign row names
row.names(df) <- c(1:458)

# Get precipitation data from chirps
rownames(df) <- seq_along(df$id)
lonlat <- df[, c("longitude", "latitude")]
days <- c("2020-09-26", "2021-08-15")
rain_data <- get_chirps(lonlat, days, server = "CHC")
rain <- rain_data[]

# Create data frame with sum and mean precipitation data
precipi_list <- lapply(1:458, function(i) {
  output <- df[i, "t_harvesting_day"] + 1 - df[i, "t_planting_day"]
  x <- subset(rain, rain$id == i)
  x <- x[1:output, ]
  data.frame(i, sum(x$chirps), mean(x$chirps), sum(x$chirps == 0))
})
precipitation_data <- do.call(rbind, precipi_list)
names(precipitation_data) <- c("i", "totalsum_rainfall", "mean_rainfall", "sum_no_rain")

# Merge precipitation data with original data frame
df <- cbind(df, precipitation_data)

############################Download soil data##################################
# Load required packages
library("raster")
library("rgdal")
library("ggplot2")


# Load raster into R
raster_soc <- raster("raster/CroppedSoilData.grd", band = 1)
raster_cec <- raster("raster/CroppedSoilData.grd", band = 2)
raster_nitrogen <- raster("raster/CroppedSoilData.grd", band = 3)
raster_pH  <- raster("raster/CroppedSoilData.grd", band = 4)
raster_soil_group  <- raster("raster/soil_group.tif")

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


## Get point data from raster
######https://gisday.wordpress.com/2014/03/24/extract-raster-values-from-points-using-r/comment-page-1/
gps_keep <- df[,1:3]
coordinates(gps_keep)= ~ longitude + latitude

# Stack layers 
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
#Find index here :https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.rat.xml

## Recall combinedPointValue_soil_all.csv to change column names
df_soil <- read.csv("data/combinedPointValue_soil_all.csv")
str(df_soil) 
colnames(df_soil)[1]<-"id_soil"
colnames(df_soil)[2]<-"Soil organic carbon"
colnames(df_soil)[3]<-"Cation exchange capacity(at ph7)"
colnames(df_soil)[4]<-"nitrogen"
colnames(df_soil)[5]<-"pH_water"
colnames(df_soil)[6]<-"Soil_classification"

# Lets change soil_classification index to real name by referencing :https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.rat.xml
table(df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("0", "Acrisols", df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("3", "Andosols", df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("6", "Cambisols", df_soil$Soil_classification)
df_soil$Soil_classification<- gsub("1Acrisols", "Ferralsols", df_soil$Soil_classification)
# Layer 1= Soil organic carbon, layer 2=Cation exchange capacity, layer 3=nitrogen, layer 4=pH)

df_soil <- df_soil[,-c(7:9)]
## Combind soil data fram with tricot
df <- cbind(df,df_soil)


################## Finished download environment covariate ##################### 
################################################################################

#######################Get socio-economic covariate############################# 
library(dplyr)

datacleaning <- read.csv("data/RHoMIS_RAB_21A_21B.csv", header=T, na.strings=c("","NA"))

# Update column names
datacleaning$X_xform_id_string <- str_replace_all(datacleaning$X_xform_id_string, 
                                                  c("RHoMIS_Minimal_v1_6" = "Pot21A", "RHoMIS_21BPot" = "POT21B"))
names(datacleaning)[5] <- "RHoMIS_id"


# create a new data frame with the same columns as datacleaning
RHoMis_add <- datacleaning[, ]

# add a column for AI_landcultivated_points
RHoMis_add$AI_landcultivated_points <- as.numeric(RHoMis_add$landcultivated > 2)

# add a column for AI_farm_labour_points
RHoMis_add$AI_farm_labour_points <- as.numeric(grepl("Hire_labour", RHoMis_add$farm_labour))

# add columns for AI_ag_inputs_points (1-5) and calculate their sum
RHoMis_add[, paste0("AI_ag_inputs", 1:5, "_points")] <- 
  apply(RHoMis_add[, paste0("ag_inputs_", 1:5)], 2, 
        function(x) as.numeric(grepl("Fertilisers|Hybrid_Seeds|Pesticides", x)))
RHoMis_add$AI_ag_inputs_points_sum <- rowSums(RHoMis_add[, paste0("AI_ag_inputs", 1:5, "_points")])

# add a column for AI_farm_irrigation_points
RHoMis_add$AI_farm_irrigation_points <- as.numeric(grepl("Y", RHoMis_add$land_irrigated))

# add a column for Agriculture_intensification_level
RHoMis_add$Agriculture_intensification_level <- rowSums(RHoMis_add[, c("AI_landcultivated_points", 
                                                                       "AI_farm_labour_points", 
                                                                       "AI_ag_inputs_points_sum", 
                                                                       "AI_farm_irrigation_points")])

#write.csv(RHoMis_add,"MyRprojectRHoMis_edited.csv", row.names = FALSE) #export CSV file. 


# Creating RHoMIS covariate
RHoMIs_covariate <- RHoMis_add[]

# Adding potato_important column
RHoMIs_covariate$potato_important <- ifelse(grepl("potato", RHoMIs_covariate$crops), 1, 0)
table(RHoMIs_covariate$potato_important)

# Creating true_householdheader column
RHoMIs_covariate$true_householdheader <- ifelse(RHoMIs_covariate$respondent_is_head == "Y", RHoMIs_covariate$respondentsex, paste0(RHoMIs_covariate$respondentsex, "_o"))
RHoMIs_covariate$true_householdheader <- ifelse(RHoMIs_covariate$true_householdheader == "F_o", "M", ifelse(RHoMIs_covariate$true_householdheader == "M_o", "F", RHoMIs_covariate$true_householdheader))


######
# Define selected factors
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

# Subset RHoMIs_covariate and rename columns
RHoMIs_covariate <- as.data.frame(lapply(selected_factors, function(x) unlist(RHoMIs_covariate[, x])))

colnames(RHoMIs_covariate) <- selected_factors

for(i in seq_along(RHoMIs_covariate)){
  print(i)
  print(table(RHoMIs_covariate[,i], exclude = NULL))
}


RHoMIs_covariate <- RHoMIs_covariate[,c("RHoMIS_id","respondentsex","respondent_is_head","true_householdheader", "education_level","household_type", "count_people", "landcultivated",
                                        "landowned", "land_ownership", "farm_labour","potato_important",
                                        "Agriculture_intensification_level", "Asset_Count",
                                        "Crop_Diversity", "Ag_Input_Diversity", "Livestock_Diversity",
                                        "Livestock_TLU_Score", "Hunger_Months_Count", "Crop_Income_USD",
                                        "Livestock_Income_USD", "Farm_Income_USD")]

# Change farm_labour component
# Hire_labout = 1, none = 0
RHoMIs_covariate_t <- RHoMIs_covariate[]
RHoMIs_covariate_t[RHoMIs_covariate_t == "Hire_labour|Household|Reciprocal"] <-  "Hire_labour"
RHoMIs_covariate_t[RHoMIs_covariate_t == "Hire_labour|Household"] <-  "Hire_labour"
RHoMIs_covariate_t[RHoMIs_covariate_t == "Hire_labour|Reciprocal"] <-  "Hire_labour"
RHoMIs_covariate_t[RHoMIs_covariate_t == "Household|Reciprocal"] <-  "Household"
#
RHoMIs_covariate_t$farm_labour <- gsub("Hire_labour", "1", RHoMIs_covariate_t$farm_labour)
RHoMIs_covariate_t$farm_labour <- gsub("Household", "0", RHoMIs_covariate_t$farm_labour)
RHoMIs_covariate_t$farm_labour <- gsub("Reciprocal", "0", RHoMIs_covariate_t$farm_labour)
RHoMIs_covariate_t$farm_labour <- gsub("None_respond", "0", RHoMIs_covariate_t$farm_labour)

RHoMIs_covariate <- add_column(RHoMIs_covariate, "farm_labour_b" = RHoMIs_covariate_t$farm_labour, .after = "farm_labour")

######################
#re-order dataframe as same order to tricot data frame 

#reorder data fram based on df.

RHoMIs_covariate <- RHoMIs_covariate[match(df$id, RHoMIs_covariate$RHoMIS_id),]
df <- cbind(df, RHoMIs_covariate)

# export CSV file
#write.csv(df, "data/manuscript_temp.csv", row.names = FALSE)

##################
################################################################################
###########################Covariate vegetative 2 stage#########################
#however, there are new convariate data for analyzing vegetative 2 stage(from planting to 75days later) 


# Convert date format
df$vegetative2_survey_start <- as.Date(df$vegetative2_survey_start, format = "%d/%m/%Y")

which(is.na(df[,  "vegetative2_survey_start"])) 


df[78,"vegetative2_survey_start"]


#
temp_d78<- df[78,"vegetative2_survey_start"]
#convert df$vegetative2_survey_start date form 
df$vegetative2_survey_start<- as.Date(df$vegetative2_survey_start, format = "%d/%m/%Y")

# Fill in missing values for vegetative2_survey_start
df[is.na(df$vegetative2_survey_start), "vegetative2_survey_start"] <- as.Date(df[is.na(df$vegetative2_survey_start), "t_planting_day"]) + 74

# Convert vegetative2_survey_start to temperature
temp_vg_2 <- temperature(df[, c("longitude", "latitude")], day.one = df$t_planting_day, last.day = df$vegetative2_survey_start)



#######getting GDD
#gdd_potato <- GDD(temp_vg_2$maxdf , temp_vg_2$minNT, tbase = 7, equation = "default")
#however, this code measures heat accumulation based on the growing degree-days that saved in the data frame .
# I need to return the number of days which is a given organism required to reach a certain number of accumulated degree-days. Therfore,I need degree-days


potato_gdd_vg2 <- GDD(df[,c("longitude","latitude")], 
                      day.one = df[,"t_planting_day"],
                      last.day =  df[,"vegetative2_survey_start"],
                      degree.days =  1007.350, return.as = "ndays")


##########################
#########crop_sensitive 

potato_SI_vg2<- crop_sensitive(df[,c("longitude","latitude")], 
                               day.one =df[,"t_planting_day"],
                               last.day = df[,"vegetative2_survey_start"], 
                               as.sf = FALSE)

###################
# Combine columns
envi_phenology <- cbind(temp_vg_2, potato_gdd_vg2, potato_SI_vg2)

# Display tables
for(i in 14:21){
  table(envi_phenology[,i])
}
envi_phenology <- envi_phenology[,-c(14:21)]
for(i in 17:22){
  table(envi_phenology[,i])
}
envi_phenology <- envi_phenology[,-c(17:22)]

# Add to data frame
df <- cbind(df, envi_phenology)
view(df[])

# Check for NA values
which(is.na(df[,"vegetative2_survey_start"]))

###########################Add Percipitation covariate##########################

# Set row names
rownames(df) <- seq_len(nrow(df))

# Define lonlat
lonlat <- df[, c("longitude", "latitude")]

# Define days
days <- c("2020-09-26", "2021-08-15")

# Get precipitation data
rain_data <- get_chirps(lonlat, days, server = "CHC")

# Save the data to a file
#write.csv(rain_data, "rain_data_v1.csv")
# Read the data from the file
#rain <- read.csv("rain_data_v1.csv")

# Set up a data frame for the precipitation data
y <- sapply(df[, c("vegetative2_survey_start", "t_planting_day")], as.Date)
y <- y[, 1] + 1 - y[, 2]

# Extract certain days and create a data frame
precipi_list <- lapply(seq_len(nrow(df)), function(i) {
  x <- subset(rain, id == i)
  x <- x[1:y[i], ]
  data.frame(i, sum = sum(x$chirps), mean = mean(x$chirps), sum_no_rain = sum(x$chirps == 0))
})
precipitation_data <- do.call(rbind, precipi_list)

# Rename columns of the precipitation data
colnames(precipitation_data) <- c("i", "totalsum_rainfall", "mean_rainfall", "sum_no_rain")

# Combine the data frames
df <- cbind(df, precipitation_data)

############################Download soil data##################################
# Actually, we do not need download soil data for vg2 
temp_df<-df[,162:166]
df<- cbind(df, temp_df)
df<-df[,-c(151:156)]
names(df[,136:160]) 

#change covariate name  (start with capital letter)

names(df)[136:160]  <-  c("MaxDT"      ,                      "MinDT"           ,                 "MaxNT"  ,                         
                         "MinNT"       ,                     "DTR"              ,                "SU"      ,                        
                         "TR"           ,                    "CFD"               ,               "WSDI"     ,                       
                         "CSDI"          ,                   "T10p"               ,              "T90p"      ,                      
                         "GDD"            ,                  "CDI_mean_23"  ,                   
                         "CDI_mean_24"     ,                 "i"                    ,            "Totalsum_rainfall",               
                         "Average_rainfall"    ,                "sum_no_rain"           ,           "id_soil"           ,              
                         "Soil.organic.carbon",              "Cation.exchange.capacity.at.ph7." ,"Nitrogen"           ,             
                         "PH_water"            ,             "Soil_classification")   


names(df)[161:183] <- c("RHoMIS_id"               ,          "Respondentsex"           ,          "Respondent_is_head",               
                        "True_householdheader"      ,        "Education_level"         ,          "Household_type"      ,             
                        "Count_people"               ,       "Landcultivated"           ,         "Landowned"            ,            
                        "Land_ownership"               ,     "Farm_labour"             ,          "Farm_labour_b"         ,           
                        "Potato_important"             ,     "Agriculture_intensification_level", "Asset_Count"            ,          
                        "Crop_Diversity"                ,    "Ag_Input_Diversity"       ,         "Livestock_Diversity"     ,         
                        "Livestock_TLU_Score"            ,   "Hunger_months"     ,          "Crop_Income_USD"          ,        
                        "Livestock_Income_USD"            ,  "Farm_Income_USD")    

#change vg2 covarate name to easily recognize. 

names(df)[184:208] <- c("MaxDT_vg2",
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
                          "Totalsum_rainfall_vg2",
                          "Average_rainfall_vg2",
                          "Sum_no_rain_vg2",
                          "id_soil_vg2",
                          "Soil.organic.carbon_vg2",
                          "Cation.exchange.capacity.at.ph7._vg2",
                          "Nitrogen_vg2",
                          "PH_water_vg2",
                          "Soil_classification_vg2")

#save 
write.table(df,file="data/potato-tricot-data-with-covariate_for_manuscript_12_april.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



################################################################################
################################################################################
################################################################################
#####################Calculate BLUP from on station data########################
################################################################################
# Load required packages
library("lme4")

#recall csv data
df_on_station<-read.csv("data/Fixed version On station data_ver2.csv", stringsAsFactors = F)

########################MODEL ANALYSIS####################
# need to set it as factor!

# Define factor variables
site <- factor(df_on_station$site)
no <- factor(df_on_station$no)
season <- factor(df_on_station$season)
rep <- factor(df_on_station$rep)
clone <- factor(df_on_station$clone)

# Divide data into seasons
df_on_station1 <- subset(df_on_station, season == 1) #extract season 1 Season B 
df_on_station2 <- subset(df_on_station, season == 2)  #extract season 2 Season A

###################################################################
###ONLY clone RANDOM and OTHER FIXED EFFECTS MODEL
## run model with random effects 
#flowering trait ## run model with random effects 

#From lme4 documentation you can learn that
#coef: Computes the sum of the random and fixed effects coefficients for each explanatory variable for each level of each grouping factor
#ranef is a generic function to extract the conditional modes of the random effects from a fitted model object  .
# For linear mixed models the conditional modes of the random effects are also the conditional means.

#make GLMM model
flowering_all_glmer<-glmer(df_on_station1$flowering ~ site + (1|clone)+(site/rep), data = df_on_station1, family = poisson )
#make data frame
flowering_GLMM <- ranef(flowering_all_glmer)
flowering_GLMM <- as.data.frame(flowering_GLMM)
names(flowering_GLMM) <- c("grpvar" ,"term", "clone"  ,"flowering_condval","flowering_condsd")
#condval:value of the conditional mean
#condsd: conditional standard deviation
flowering_GLMM <- add_column(flowering_GLMM,"flowering_fixed_effects_estimates"= fixef(flowering_all_glmer)[[1]], .after = "clone")
flowering_GLMM <- add_column(flowering_GLMM,"sum_random_and_fixed_effects_coefficients"= coef(flowering_all_glmer)$clone[[1]], .after = "flowering_condval")
flowering_GLMM <- flowering_GLMM[,-7] 

#plant_vigor trait
#old model
plant_vigor_glmer<-glmer(df_on_station1$plant_vigor ~ site + (1|clone)+(site/rep), data = df_on_station1,  family = poisson)
 #BLUPs about plant_vigor 
plant_vigor_GLMM <- ranef(plant_vigor_glmer)
#make data frame
plant_vigor_GLMM <- as.data.frame(plant_vigor_GLMM)
plant_vigor_GLMM <- plant_vigor_GLMM[,-c(1:3)]
names(plant_vigor_GLMM) <- c("plant_vigor_condval","plant_vigor_condsd")
plant_vigor_GLMM <- add_column(plant_vigor_GLMM,"plant_vigor_fixed_effects_estimates"= fixef(plant_vigor_glmer)[[1]], .before = "plant_vigor_condval")
plant_vigor_GLMM <- add_column(plant_vigor_GLMM,"sum_random_and_fixed_effects_coefficients"= coef(plant_vigor_glmer)$clone[[1]], .after = "plant_vigor_condval")
plant_vigor_GLMM <- plant_vigor_GLMM[,-4]


#average_nber_of_tubers_per_plant trait
average_nber_of_tubers_per_plant_lmer<-lmer(df_on_station1$average_nber_of_tubers_per_plant ~ site + (1|clone)+(site/rep), data = df_on_station1)
 #BLUPs about average_nber_of_tubers_per_plant 
average_nber_of_tubers_per_plant_blup <- ranef(average_nber_of_tubers_per_plant_lmer)

#make data frame
average_nber_of_tubers_per_plant_blup <- as.data.frame(average_nber_of_tubers_per_plant_blup)
average_nber_of_tubers_per_plant_blup <- average_nber_of_tubers_per_plant_blup[,-c(1:3)]
names(average_nber_of_tubers_per_plant_blup) <- c("average_nber_of_tubers_per_plant_condval","average_nber_of_tubers_per_plant_condsd")

average_nber_of_tubers_per_plant_blup <- add_column(average_nber_of_tubers_per_plant_blup,"average_nber_of_tubers_per_plant_fixed_effects_estimates"= fixef(average_nber_of_tubers_per_plant_lmer)[[1]], .before = "average_nber_of_tubers_per_plant_condval")
average_nber_of_tubers_per_plant_blup <- add_column(average_nber_of_tubers_per_plant_blup,"sum_random_and_fixed_effects_coefficients"= coef(average_nber_of_tubers_per_plant_lmer)$clone[[1]], .after = "average_nber_of_tubers_per_plant_condval")
average_nber_of_tubers_per_plant_blup <- average_nber_of_tubers_per_plant_blup[,-4]




#totat_wight_of_tubers_ton_ha trait
totat_wight_of_tubers_ton_ha_lmer<-lmer(df_on_station1$totat_wight_of_tubers_ton_ha ~ site + (1|clone)+(site/rep), data = df_on_station1)
# BLUPs about totat_wight_of_tubers_ton_ha 
# Make data frame
totat_wight_of_tubers_ton_ha_blup <- ranef(totat_wight_of_tubers_ton_ha_lmer)

totat_wight_of_tubers_ton_ha_blup <- as.data.frame(totat_wight_of_tubers_ton_ha_blup)
totat_wight_of_tubers_ton_ha_blup <- totat_wight_of_tubers_ton_ha_blup[,-c(1:3)]
names(totat_wight_of_tubers_ton_ha_blup) <- c("totat_wight_of_tubers_ton_ha_condval","totat_wight_of_tubers_ton_ha_condsd")

totat_wight_of_tubers_ton_ha_blup <- add_column(totat_wight_of_tubers_ton_ha_blup,"totat_wight_of_tubers_ton_ha_fixed_effects_estimates"= fixef(totat_wight_of_tubers_ton_ha_lmer)[[1]], .before = "totat_wight_of_tubers_ton_ha_condval")
totat_wight_of_tubers_ton_ha_blup <- add_column(totat_wight_of_tubers_ton_ha_blup,"sum_random_and_fixed_effects_coefficients"= coef(totat_wight_of_tubers_ton_ha_lmer)$clone[[1]], .after = "totat_wight_of_tubers_ton_ha_condval")
totat_wight_of_tubers_ton_ha_blup <- totat_wight_of_tubers_ton_ha_blup[,-4]


#combine blups of traits 
tuber_blup <- cbind(flowering_GLMM, plant_vigor_GLMM)
tuber_blup <- cbind(tuber_blup, average_nber_of_tubers_per_plant_blup)
tuber_blup <- cbind(tuber_blup, totat_wight_of_tubers_ton_ha_blup)
tuber_blup_kep<- tuber_blup


names(tuber_blup) <- c( "grpvar","term" ,                                                   
                        "clone","flowering_Grand_mean_intercept +/- std err"  ,                     
                        "flowering_BLUPS_genotypic_value","flowering_Predicted_mean_(Sum_of_fixef_and_BLUP)",  
                        "plant_vigor_Grand_mean_intercept +/- std err"  ,                   
                        "plant_vigor_GLMMS_genotypic_value","plant_vigor_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "average_nber_of_tubers_per_plant_Grand_mean_intercept +/- std err" ,"average_nber_of_tubers_per_plant_BLUPS_genotypic_value" ,               
                        "average_nber_of_tubers_per_plant_Predicted_mean_(Sum_of_fixef_and_BLUP)",
                        "totat_wight_of_tubers_ton_ha_Grand_mean_intercept +/- std err"    
                        ,"totat_wight_of_tubers_ton_ha_BLUPS_genotypic_value","totat_wight_of_tubers_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP")


traits <- df_on_station1[,12:26] 
traits_mean<- data.frame(apply(traits,2, mean, na.rm=TRUE))

tuber_blup <- add_column(tuber_blup,"flowering_average_performance"= traits_mean[1,], .after = "flowering_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"plant_vigor_average_performance"= traits_mean[3,], .after = "plant_vigor_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"average_nber_of_tubers_per_plant_average_performance"= traits_mean[12,], .after = "average_nber_of_tubers_per_plant_Predicted_mean_(Sum_of_fixef_and_BLUP)")
tuber_blup <- add_column(tuber_blup,"totat_wight_of_tubers_ton_ha_average_performance"= traits_mean[15,], .after = "totat_wight_of_tubers_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP")
################################## finished calculating BLUP

tem_dt <- df_on_station[]
tem_dt <- add_column(tem_dt, "late_blight" = ifelse(tem_dt$lb_incidence == 0 & 
                                                      tem_dt$lb_severity == 0, 0, 1), .after= "lb_severity")
tem_dt <- add_column(tem_dt, "Leaf_miner_flies" = ifelse(tem_dt$lmf_incidence == 0 & 
                                                           tem_dt$lmf_severity == 0, 0, 1), .after= "lmf_severity")
tem_dt <- add_column(tem_dt, "viruses" = ifelse(tem_dt$viruses_incidence == 0 &  
                                                  tem_dt$viruses_severity == 0, 0, 1), .after= "viruses_severity")
tem_dt <- add_column(tem_dt, "bacterial_wilt" = ifelse(tem_dt$bw_incidence == 0 &  
                                                         tem_dt$bw_severity == 0, 0, 1), .after= "bw_severity")
tem_dt1 <- subset(tem_dt, season == 1)


clone_names <- unique(tem_dt$clone)
## Hold only season 1 == season B
tem_dt1 <- tem_dt[c(which(tem_dt$season == 1)),]
#tem_dt2 <- tem_dt[c(which(tem_dt$season == 2)),]


rrr <- sapply(clone_names, function(x) mean(tem_dt1$late_blight[tem_dt1$clone == x]))
bbb <- sapply(clone_names, function(x) mean(tem_dt1$viruses[tem_dt1$clone == x]))
fff <- sapply(clone_names, function(x) mean(tem_dt1$bacterial_wilt[tem_dt1$clone == x]))

rrr<- cbind(rrr, bbb)
rrr<- cbind(rrr, fff)

colnames(rrr) <- c("late_blight", "viruses", "bacterial_wilt")


########################finished calculate binary value

# Arrange data frames to combine as same clone names sequence
rrr <- rrr[c(11, 8, 1, 15,
             3, 4, 14, 13,
             6, 7, 10, 9,
             5, 2,12),]
rrr

tuber_blup <- cbind(tuber_blup, rrr)

# Delete un-necessary columns
tuber_genentic_blup <- tuber_blup[,-c(1:2)]  

tuber_genentic_blup$clone<- as.character(tuber_genentic_blup$clone)

tuber_genentic_blup$clone[tuber_genentic_blup$clon == "CIP39601.241"] <- "Izihirwe"
tuber_genentic_blup[tuber_genentic_blup == "CIP393371.58"] <- "Twihaze"
tuber_genentic_blup[tuber_genentic_blup == "CIP393077.159"] <- "Kazeneza"
tuber_genentic_blup[tuber_genentic_blup == "CIP398190.615"] <- "Ndeze"
tuber_genentic_blup[tuber_genentic_blup == "CIP393280.64"] <- "Nkunganire"
tuber_genentic_blup[tuber_genentic_blup == "CIP392797.22"] <- "Cyerekezo"
tuber_genentic_blup[tuber_genentic_blup == "CIP394611.112"] <- "Jyambere"
tuber_genentic_blup[tuber_genentic_blup == "CIP398190.89"] <- "Seka"
tuber_genentic_blup[tuber_genentic_blup == "Shangi"] <- "Ndamira"


#delete untest tubers in Tricot 
tuber_genentic_blup <- tuber_genentic_blup[-c(1,4,8,12),]

#delete "Cyerekezo" and "Kinigi because they do not exist in the tricot. 
tuber_genentic_blup <- tuber_genentic_blup[-c(1,9),]
tuber_genentic_blup


#bring predict value by pluus blup effect + average.
tuber_genentic_blup <- add_column(tuber_genentic_blup, "flowering_prediction" = tuber_genentic_blup[,"flowering_Predicted_mean_(Sum_of_fixef_and_BLUP)"] +  tuber_genentic_blup[,"flowering_average_performance"]
                                  ,.after ="flowering_average_performance"   )

tuber_genentic_blup <- add_column(tuber_genentic_blup, "plant_vigor_prediction" = tuber_genentic_blup[,"plant_vigor_Predicted_mean_(Sum_of_fixef_and_BLUP)"] +  tuber_genentic_blup[,"plant_vigor_average_performance"]
                                  ,.after ="plant_vigor_average_performance"   )

tuber_genentic_blup <- add_column(tuber_genentic_blup, "average_nber_of_tubers_per_plant_prediction" = tuber_genentic_blup[,"average_nber_of_tubers_per_plant_Predicted_mean_(Sum_of_fixef_and_BLUP)"] +  tuber_genentic_blup[,"average_nber_of_tubers_per_plant_average_performance"]
                                  ,.after ="average_nber_of_tubers_per_plant_average_performance"   )

tuber_genentic_blup <- add_column(tuber_genentic_blup, "totat_wight_of_tubers_ton_ha_prediction" = tuber_genentic_blup[,"totat_wight_of_tubers_ton_ha_Predicted_mean_(Sum_of_fixef_and_BLUP"] +  tuber_genentic_blup[,"totat_wight_of_tubers_ton_ha_average_performance"]
                                  ,.after ="totat_wight_of_tubers_ton_ha_average_performance"   )
#delete unnecessary columns 
tuber_genentic_blup <- tuber_genentic_blup[,c("clone" , "flowering_prediction", 
                                              "plant_vigor_prediction", "average_nber_of_tubers_per_plant_prediction", 
                                              "totat_wight_of_tubers_ton_ha_prediction"
                                              ,"late_blight",  "viruses", "bacterial_wilt")]


# Calculate mean for untested tubers Cruza and Gisubizo 

tuber_genentic_blup$late_blight <-as.numeric(tuber_genentic_blup$late_blight)
tuber_genentic_blup$viruses <-as.numeric(tuber_genentic_blup$viruses)
tuber_genentic_blup$bacterial_wilt <-as.numeric(tuber_genentic_blup$bacterial_wilt)
m_1<- mean(tuber_genentic_blup$late_blight )
m_2<- mean(tuber_genentic_blup$viruses )
m_3<- mean(tuber_genentic_blup$bacterial_wilt )
#Cruza and Gisubizo do not exist in on station data. So, lets put fake data in data.frame(Not real if NA. item covariate does not)
tuber_genentic_blup <- tuber_genentic_blup %>%  add_row(clone ="Cruza",  flowering_prediction = traits_mean[1,], 
                                                        plant_vigor_prediction = traits_mean[3,], average_nber_of_tubers_per_plant_prediction = traits_mean[12,]
                                                        ,totat_wight_of_tubers_ton_ha_prediction = traits_mean[15,], late_blight = m_1, viruses =m_2, bacterial_wilt = m_3)


tuber_genentic_blup <- tuber_genentic_blup %>%  add_row(clone ="Gisubizo", flowering_prediction = traits_mean[1,] , 
                                                        plant_vigor_prediction  = traits_mean[3,], average_nber_of_tubers_per_plant_prediction = traits_mean[12,]
                                                        ,totat_wight_of_tubers_ton_ha_prediction = traits_mean[15,], late_blight = m_1,  viruses =m_2, bacterial_wilt = m_3)


####Done
###############################################################################
###############converted distribution to standard distribution#################

test_dt <- tuber_genentic_blup
potato_names<- test_dt[,1]
column_names<- names(test_dt)
tt_dt<- test_dt[,2:8]

st_dt <- list() 
for (i in 1:7) {keep<- (tt_dt[,i] - mean(tt_dt[,i]))/sd((tt_dt[,i]))
st_dt[[i]] <- keep}
st_dt_c <- as.data.frame(st_dt)

bb <- cbind(potato_names, st_dt_c)
names(bb) <- column_names

#create a csv File
write.table(bb,file="data/on_station-covariate_season 1 _12-april.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

################################################################################
################################################################################
