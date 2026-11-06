#---------------------------------------------
#Extract temperature values from CHIRTS TIF files
#
#David Brown - WUR
#
#Key packages dependencies:
# - raster 
# - climatrends
#---------------------------------------------

#Load libraries
library(raster)
library(R.utils)
library(tidyverse)
library(rgdal)
library(ncdf4)
library(magrittr)
library(climatrends)

#set chirts folder path
#chirts_folder_path <- NULL

get_filepath <- function(.date_ts, .temp_ss, .chirts_folder_path){
  chirts_folder_path <- paste(.chirts_folder_path, .temp_ss, sep = "/")
  chirts_prefix <- .temp_ss
  year_ts <- as.numeric(format(.date_ts, "%Y"))
  month_ts <- as.character(format(.date_ts, "%m"))
  day_ts <- as.character(format(.date_ts, "%d"))
  #if(day_ts<10) day_ts <-paste0("")
  chirts_filename <- paste(chirts_prefix, year_ts, month_ts, day_ts, "tif", sep = ".")
  print(chirts_filename)
  chirts_filepath <- paste(chirts_folder_path, year_ts, chirts_filename, sep = "/")
  #print(chirts_filepath)
  
  return(chirts_filepath)
}

#.temp_ss is used to indicate max or min temperature, should be either "Tmax" or "Tmin"
chirts_extract <- function(.date, .location_xy, .temp_ss, .chirts_folder_path){
  file_path <- get_filepath(.date, .temp_ss, .chirts_folder_path)
  data_out <- raster::extract(raster(file_path), .location_xy)
  return(data_out)
}


extract_period <- function(.start_date,
                           .end_date,
                           .location_xy,
                           .temp_ss,
                           .chirts_folder_path){
  .start_date <- as.Date(.start_date, format = "%m/%d/%Y")
  .end_date <- as.Date(.end_date, format = "%m/%d/%Y")
  time_span <- seq.Date(from = .start_date, to = .end_date, by = "days")
  data_out_period <- array(dim=c(1, length(time_span)))
  for(i in 1:length(time_span)){
    data_out_period[, i] <- chirts_extract(time_span[i], .location_xy, .temp_ss, .chirts_folder_path)
  }
  colnames(data_out_period) <- as.character(time_span)
  
  return(data_out_period)
}

extract_dataset <- function(.trial_dataset, .temp_ss, .chirts_folder_path){
  extracted_dataset <- NULL
  for(i in 1:nrow(.trial_dataset)){
    extracted_dataset[[i]] <- extract_period(.trial_dataset[i, ]$pdate,
                                             .trial_dataset[i, ]$hdate,
                                             data.frame(lon = .trial_dataset[i, ]$lon,
                                                        lat = .trial_dataset[i, ]$lat),
                                             .temp_ss,
                                             .chirts_folder_path)
    
  }
  return(extracted_dataset)

}

##################################

#input dataset for extact_dataset() should be a dataframe and not a tibble,
#it will fail with a tibble
# imtp1_dates <- as.data.frame(imtp1_dates)
#
# imtp1_dates
#
# chirts_imtp1_tmin <- extract_dataset(imtp1_dates, "Tmin")
#
# chirts_imtp1_tmax <- extract_dataset(imtp1_dates, "Tmax")
#
# plot(chirts_imtp1_tmin)
#
# library(timeSeries)
# timeSeries(charvec = colnames(chirts_imtp1_tmax[[1]]),
#            data = chirts_imtp1_tmax[[1]])
#
#
#
# chirts_mdmin <- lapply(chirts_imtp1_tmin, median)
# imtp_mdt[1:6]
# chirts_mdmax <- lapply(chirts_imtp1_tmax, median)
#
# eric(imtp_mdt[1:6])
#
# save(imtp_temp_ind_chirts, file = "data/climate/imtp_temp_ind_chirts.rda")




