#######################################################
#Extract data from chirps tiff files
#Customized for IMTP data
#Using the package raster and climatrends
#David Brown Fuentes
#Wageningen University and Research
#
#Created on:2019
#
#Last update:2020-04-28
#######################################################

#Load libraries
library(raster)
library(R.utils)
library(tidyverse)
library(rgdal)
library(ncdf4)
library(magrittr)
library(climatrends)


#######################################
#Load data
#chirps_folder_path <- "d:/environmental_data/chirps_0.05"

chirps_files_all <- list.files(path = chirps_folder_path, 
                               pattern = "*.tif", 
                               recursive = T)
chirps_files_all
head(chirps_files_all)
tail(chirps_files_all)

#Get the list of files to be read
get_days_list <- function(.planting_date, .harvest_date){
selected_files <- NULL
days_to_search <- seq.Date(.planting_date, .harvest_date, by = "days")
days_to_search %<>% gsub(pattern = "-",replacement = ".")
(days_to_search)
# for(i in seq_along(days_to_search)){
#   for(j in seq_along(chirps_files_all)){
#     if(str_detect(chirps_files_all[j],days_to_search[i]))  
#       selected_files[i]<-chirps_files_all[j]
#   }
#}
#return(selected_files)

}

get_days_list(imtp_loc_dates$pdate[2],
              imtp_loc_dates$hdate[2])



#Function to get rainfall values from raster files, for each location
get_rainfall <- function(planting_date, span, location_df){
  
rainfall_current_location <- array(dim = c(1, span))

files_to_read <- get_days_list(planting_date, span)

days <- seq.Date(from = planting_date, to = planting_date + span - 1, by = "days")
#for(i in 1:span){
for(i in 1:10){
 # rainfall_current_location[,i]<-raster::extract(raster(paste0(chirps_folder_path,files_to_read[i])),location_df)
 print(paste0(chirps_folder_path, files_to_read[i]))
  print(location_df)
  print(paste(files_to_read[i], "Element:", i, sep = " "))

}
colnames(rainfall_current_location) <- as.character(days)
return(rainfall_current_location)
}

#files_to_read <- get_days_list(locations_imtp$planting_date[2], locations_imtp$max_time_planting_harvest[2])
#chirps_data_test <- raster::extract(raster(paste(chirps_folder_path, files_to_read[1], sep = "")),
#data.frame(locations_imtp$longitude[1], locations_imtp$latitude[1]))

#For loop to call get_rainfall function
chirps_data <- NULL
for (i in 1:nrow(locations_imtp)){
chirps_data[[i]] <- get_rainfall(locations_imtp$planting_date[i], 
                                 locations_imtp$max_time_planting_harvest[i],
                                 data.frame(locations_imtp$longitude[i],
                                            locations_imtp$latitude[i]))
print(paste("Element Chirps data:", i, sep = " "))
}


#write extracted values from raster to csv files
# for(i in seq_along(chirps_data)){
#   
#   write_csv(as.data.frame(chirps_data[[i]]),paste0("extracted_chirps/",locations_imtp$trial[i],".csv"))
#   
# }

rainfall_indices <- bind_rows(rainfall_index)
rainfall_indices
write_csv(rainfall_indices,"rainfall_indices/rainfall_indices_bw.csv")

#°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#

# #In case the chirps data were already stored in CSV files
# #read it with the following code
# chirps_csv_files<-list.files("extracted_chirps/")
# chirps_csv_files
# chirps_csv<-map(chirps_csv_files,read_csv)
# chirps_csv[1]
# for (i in seq_along(chirps_csv)){
#   rainfall_index[[i]]<-rainfall(chirps_csv[[i]],day.one=locations_imtp$planting_date[i+47],
#                                 span=locations_imtp$max_time_planting_harvest[i+47]-1)
# }
# 
# locations_imtp$planting_date[48]
# rainfall_index
# rainfall_indices<-NULL
# rainfall_indices<-bind_rows(rainfall_index)
# rainfall_indices
# write_csv(rainfall_indices,"rainfall_indices/trials_1_47.csv")


#########----End of Script------#######################


#CRU#
#cru_ncdf <- ncdf4::nc_open("data/environmental_data/CRU/temp/HadCRUT.4.6.0.0.median.nc")
#cru_data<-ncdf4::ncvar_get(cru_ncdf,varid = "temperature_anomaly")

#library(getCRUCLdata)
#temp_cru <- getCRUCLdata::get_CRU_df(tmp = T)

