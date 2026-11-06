library(climatrends)
library(ag5Tools)


#load data
musa_trials_data <- read.csv("data/processed/musa_trials_data.csv", row.names = 1, check.names = FALSE)

musa_trials_time_loc <- read.csv("data/processed/musa_trials_time_loc.csv", row.names = 1)

head(musa_trials_data)

head(musa_trials_time_loc)

min(musa_trials_time_loc$pdate)

max(musa_trials_time_loc$end_date)

#for some odd reason this should be done before getting the whole climatic dataset
#to avoid crashing RStudio
# ag5Tools::ag5_extract(coords = musa_trials_time_loc)
#                       .date = musa_trials_time_loc$pdate[1],
#                             .lon = musa_trials_time_loc$lon[1],
#                             .lat = musa_trials_time_loc$lat[1],
#                             .agera5_folder = "D:/Dropbox (Bioversity CR)/env_data/agera5/")



# agera5::get_temp.data_point(.date = musa_trials_time_loc$pdate[1],
#                             .lon = musa_trials_time_loc$lon[1],
#                             .lat = musa_trials_time_loc$lat[1],
#                             .agera5_folder = "D:/Dropbox (Bioversity CR)/env_data/agera5/")


musa_trials_prec <- ag5Tools::ag5_extract(coords =  musa_trials_time_loc,
                                          start_date = "pdate",
                                          end_date = "end_date",
                                          lon = "lon",
                                          lat = "lat",
                                          variable = "Precipitation-Flux",
                                          path = "D:/Dropbox (Bioversity CR)/env_data/agera5/")

#save(musa_trials_prec, file = "data/climate/musa_trials_prec.rda")
load(file = "data/climate/musa_trials_prec.rda")

musa_trials_prec_ind <- vector(mode = "list", length = length(musa_trials_prec))

for(i in 1:length(musa_trials_prec)){
  
  musa_trials_prec_ind[[i]] <- climatrends::rainfall(object  = data.matrix(musa_trials_prec[[i]]),
                                                     day.one = colnames(musa_trials_prec[[i]])[1],
                                                     span    = length(musa_trials_prec[[i]]))
  
  
}


musa_trials_prec_ind <- do.call(rbind, musa_trials_prec_ind)



#temperature data
musa_trials_temp_daytime_max <- ag5Tools::ag5_extract(coords =  musa_trials_time_loc,
                                                      start_date = "pdate",
                                                      end_date = "end_date",
                                                      lon = "lon",
                                                      lat = "lat",
                                                      variable = "Temperature-Air-2m",
                                                      statistic = "Max-Day-Time",
                                                      path = "D:/Dropbox (Bioversity CR)/env_data/agera5/")


#save(musa_trials_temp_daytime_max, file = "data/climate/musa_trials_temp_daytime_max.rda")

musa_trials_temp_nighttime_min <- ag5Tools::ag5_extract(coords =  musa_trials_time_loc,
                                                        start_date = "pdate",
                                                        end_date = "end_date",
                                                        lon = "lon",
                                                        lat = "lat",
                                                        variable = "Temperature-Air-2m",
                                                        statistic = "Min-Night-Time",
                                                        path = "D:/Dropbox (Bioversity CR)/env_data/agera5/")

#save(musa_trials_temp_nighttime_min, file = "data/climate/musa_trials_temp_nighttime_min.rda")


musa_trials_temp_ind <- vector(mode = "list", length = length(musa_trials_temp_daytime_max))

for(i in 1:length(musa_trials_temp_daytime_max)){
  
  musa_trials_temp_ind[[i]] <- temperature(as.numeric(musa_trials_temp_daytime_max[[i]][1, ]), 
                                           as.numeric(musa_trials_temp_nighttime_min[[i]][1, ]),
                                           span = length((musa_trials_temp_daytime_max[[i]][1,])))
}
musa_trials_temp_ind
musa_trials_temp_ind <- do.call(rbind, musa_trials_temp_ind)
musa_trials_temp_ind


musa_trials_climate <- cbind(musa_trials_prec_ind, musa_trials_temp_ind)

#write.csv(musa_trials_climate, file = "data/climate/musa_trials_climate.csv", row.names = FALSE)

#Relative humidity

musa_trials_rhum_09h <- ag5_extract(coords = musa_trials_time_loc,
                                   start_date = "pdate",
                                   variable = "Relative-Humidity-2m",
                                   time = "09h",
                                   path = "F:/env/AgERA5/rhum/")


save(musa_trials_rhum_09h, file = "data/climate/musa_trials_rhum_09h.rda")


musa_trials_rhum_09h_mean <- lapply(musa_trials_rhum_09h, function(X) mean(as.numeric(X)))

musa_trials_rhum_09h_mean <- data.frame("rhum_09" = unlist(musa_trials_rhum_09h_mean))

write.csv(musa_trials_rhum_09h_mean, file = "data/climate/musa_trials_rhum_09h_mean.csv")

as.numeric(musa_trials_rhum_09h[[1]])

#####
musa_trials_rhum_15h <- ag5_extract(coords = musa_trials_time_loc,
                                   start_date = "pdate",
                                   variable = "Relative-Humidity-2m",
                                   time = "15h",
                                   path = "F:/env/AgERA5/rhum/")


save(musa_trials_rhum_15h, file = "data/climate/musa_trials_rhum_15h.rda")


musa_trials_rhum_15h_mean <- lapply(musa_trials_rhum_15h, function(X) mean(as.numeric(X)))

musa_trials_rhum_15h_mean <- data.frame("rhum_15" = unlist(musa_trials_rhum_15h_mean))

write.csv(musa_trials_rhum_15h_mean, file = "data/climate/musa_trials_rhum_15h_mean.csv")


#add rhum to previous climate data
musa_climate_data <- read.csv("data/climate/musa_trials_climate.csv")

musa_climate_data <- cbind(musa_climate_data,
                           musa_trials_rhum_09h_mean,
                           musa_trials_rhum_15h_mean)

#write.csv(musa_climate_data, file = "data/climate/musa_trials_climate.csv", row.names = FALSE)




