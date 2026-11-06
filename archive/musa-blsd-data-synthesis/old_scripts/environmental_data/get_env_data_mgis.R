#Get environmental data for IMTP trials

imtp1_loc_dates <- yls_data_imtp1_all %>% 
  group_by(trial_id) %>% 
  summarise(pdate = mean(pdate), hdate = mean(hdate), lon = mean(lon), lat = mean(lat))


imtp2_loc_dates <- yls_data_imtp2_all %>% 
  group_by(trial_id) %>% 
  summarise(pdate = mean(pdate), hdate = mean(hdate), lon = mean(lon), lat = mean(lat))


imtp3_loc_dates <- yls_data_imtp3_blocks %>% 
  group_by(block_id) %>% 
  summarise(pdate = mean(pdate), hdate = mean(hdate), lon = mean(lon), lat = mean(lat))


save(yls_data_imtp1_all,
     yls_data_imtp2_all,
     yls_data_imtp3_blocks,
     file = "data/processed_trial_data_imtp.rda")

save(imtp1_loc_dates,
     imtp2_loc_dates,
     imtp3_loc_dates,
     file = "data/imtp_loc_dates.rda")




imtp_loc_dates <- bind_rows(imtp1_loc_dates, imtp2_loc_dates, imtp3_loc_dates)

agera5_folder <- "G:/env/AgERA5/"

source("D:/GitHub/red_beans_CA/scripts/agera5_temp_extract.R")

imtp_agera5_maxdt <- extract_dataset(trial_dataset = imtp_loc_dates, .stat = 2)

imtp_agera5_minnt <- extract_dataset(trial_dataset = imtp_loc_dates, .stat = 7)

save(imtp_agera5_maxdt,imtp_agera5_minnt, file = "data/climate/agera5_temp_imtp.rda")

temp_indices <- NULL
for(i in 1:length(imtp_agera5_maxdt)){
  temp_indices[[i]] <- temperature(imtp_agera5_maxdt[[i]][1, ], imtp_agera5_minnt[[i]][1, ])
}

temp_indices <- bind_rows(temp_indices)
temp_indices
imtp_agera5_temp_indices <- temp_indices
imtp_agera5_temp_indices

save(imtp_agera5_temp_indices, file = "data/climate/imtp_agera5_temp_indices.rda")


#add growth period lenght as covariate planting_to_harvest = (hdate - pdate)

agera5_folder <- "G:/env/AgERA5/"

source("D:/GitHub/red_beans_CA/scripts/agera5_rainfall_extract.R")

imtp_agera5_prec <- extract_prec_dataset(imtp_loc_dates)

save(imtp_agera5_prec, file = "data/climate/agera5_prec_imtp.rda")

imtp_agera5_prec_ind <- NULL

get_ct_indices <- function(rainfall_object){
  rainfall_indices <- NULL
  for(i in 1:length(rainfall_object)){
    rainfall_indices[[i]] <- rainfall(object  = rainfall_object[[i]],
                                      day.one = colnames(rainfall_object[[i]])[1],
                                      span    = length(rainfall_object[[i]]))
  }
  return(rainfall_indices)
}

imtp_agera5_prec_ind <- get_ct_indices(imtp_agera5_prec)

imtp_agera5_prec_ind <- bind_rows(imtp_agera5_prec_ind)

save(imtp_agera5_prec_ind, file = "data/climate/imtp_agera5_prec_ind.rda")


imtp_agera5_env_data <- cbind(imtp_agera5_temp_indices, imtp_agera5_prec_ind)

#------------------------CHIRTS data------------------------#

folder_path <- "I:/environmental_data/CHIRTS_0.05/"


chirts_imtp_max <- extract_dataset(imtp_loc_dates, "Tmax", folder_path)


chirts_imtp_min <- extract_dataset(imtp_loc_dates, "Tmin", folder_path)


save(chirts_imtp_max, chirts_imtp_min, file = "data/climate/chirts_imtp.rda")



