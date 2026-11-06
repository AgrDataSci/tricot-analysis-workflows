#Get environmental data for trials from Narvaez
library(readxl)
library(climatrends)

agera5_folder <- "F:/env/AgERA5/"

source("D:/GitHub/red_beans_CA/scripts/agera5_temp_extract.R")

locations_nvz <- readxl::read_xlsx("data/agtrials_banana/clean/narvaez_thesis/narvaez_locations.xlsx")
locations_nvz

#Narvaez data
load("data/processed_narvaez.rda")

data_nvz

data_nvz$pdate

nvz_agera5_maxdt <- extract_dataset(trial_dataset = locations_nvz, .stat = 2, .agera5_folder = agera5_folder)

nvz_agera5_minnt <- extract_dataset(trial_dataset = locations_nvz, .stat = 7, .agera5_folder = agera5_folder)

save(nvz_agera5_maxdt, nvz_agera5_minnt, file = "data/climate/agera5_temp_nvz.rda")

temp_indices <- NULL
for(i in 1:length(nvz_agera5_maxdt)){
  temp_indices[[i]] <- temperature(nvz_agera5_maxdt[[i]][1, ], nvz_agera5_minnt[[i]][1, ])
}

temp_indices <- bind_rows(temp_indices)
temp_indices
nvz_agera5_temp_indices <- temp_indices
nvz_agera5_temp_indices

save(nvz_agera5_temp_indices, file = "data/climate/nvz_agera5_temp_indices.rda")


#add growth period lenght as covariate planting_to_harvest = (hdate - pdate)

agera5_folder <- "G:/env/AgERA5/"

source("D:/GitHub/red_beans_CA/scripts/agera5_rainfall_extract.R")

nvz_agera5_prec <- extract_prec_dataset(locations_nvz, .agera5_folder = agera5_folder)

save(nvz_agera5_prec, file = "data/climate/agera5_prec_nvz.rda")

nvz_agera5_prec_ind <- NULL

get_ct_indices <- function(rainfall_object){
  rainfall_indices <- NULL
  for(i in 1:length(rainfall_object)){
    rainfall_indices[[i]] <- rainfall(object  = rainfall_object[[i]],
                                      day.one = colnames(rainfall_object[[i]])[1],
                                      span    = length(rainfall_object[[i]]))
  }
  return(rainfall_indices)
}

nvz_agera5_prec_ind <- get_ct_indices(nvz_agera5_prec)

nvz_agera5_prec_ind <- bind_rows(nvz_agera5_prec_ind)

save(nvz_agera5_prec_ind, file = "data/climate/nvz_agera5_prec_ind.rda")


