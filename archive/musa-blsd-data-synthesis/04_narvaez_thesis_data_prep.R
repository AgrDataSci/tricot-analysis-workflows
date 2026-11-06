#extract data for Narvaez thesis
library(readr)
library(janitor)
library(PlackettLuce)
library(gosset)
library(readxl)


source("scripts/utils/correct_musa_names.R")

narvaez_folder <- ("data/agtrials_banana/clean/narvaez_thesis/")

narvaez_files <- list.files(path = narvaez_folder, 
                            pattern = "bls.csv", 
                            recursive = TRUE,
                            full.names = TRUE)

narvaez_files

#locations_narvaez <- read_xlsx(path = "data/agtrials_banana/clean/narvaez_thesis/narvaez_locations.xlsx")

locations_narvaez <- read_csv(file = "data/agtrials_banana/clean/narvaez_thesis/narvaez_locations.csv")

locations_narvaez$trial <- as.character(locations_narvaez$trial)

narvaez_data <- lapply(narvaez_files, read_csv)

narvaez_data

narvaez_data <- lapply(narvaez_data, janitor::clean_names)

lapply(narvaez_data, colnames)

narvaez_data[[1]]


narvaez_data <- lapply(narvaez_data, correct_names)

narvaez_data

narvaez_data <- lapply(narvaez_data, as.data.frame)

narvaez_data


narvaez_data[[1]]$trial <- as.character(3191)
narvaez_data[[2]]$trial <- as.character(3193)
narvaez_data[[3]]$trial <- as.character(3194)

lapply(narvaez_data, colnames)

narvaez_data[[1]]$index_of_youngest_leaf_spotted_at_shooting

narvaez_data[[1]]$youngest_leaf_spotted_at_shooting

colnames(narvaez_data[[1]])[4] <- "youngest_leaf_spotted_at_shooting"

#Select "index_of_youngest_leaf_spotted_at_shooting" instead of "youngest_leaf_spotted_at_shooting"to avoid ties
narvaez_cols_sel <- c("genotypes", "index_of_youngest_leaf_spotted_at_shooting", "time_from_planting_to_shooting_days", "trial")

narvaez_data <- lapply(narvaez_data, function(X) X[narvaez_cols_sel])

narvaez_data

mean_time_trial_3193 <- mean(narvaez_data[[2]][!is.na(narvaez_data[[2]]$time_from_planting_to_shooting_days), 
                                               "time_from_planting_to_shooting_days"])

narvaez_data[[2]][is.na(narvaez_data[[2]]$time_from_planting_to_shooting_days),
                  ]$time_from_planting_to_shooting_days <- mean_time_trial_3193

narvaez_data <- do.call(rbind, narvaez_data)


narvaez_rank <- gosset::rank_numeric(data = narvaez_data,
                                     items = 1, 
                                     input = 2, 
                                     id = 4,
                                     ascending = FALSE)


narvaez_rank_mtx <- unclass(narvaez_rank)

row.names(narvaez_rank_mtx) <- unique(narvaez_data$trial)

narvaez_rank_mtx

narvaez_rank_mtx <- cbind(narvaez_rank_mtx, 
                          locations_narvaez[locations_narvaez$trial %in% row.names(narvaez_rank_mtx),
                                            c("lat", 
                                              "lon",
                                              "pdate",
                                              "hdate")])



names(narvaez_rank_mtx)[colnames(narvaez_rank_mtx) %in% "hdate"] <- "end_date"

narvaez_rank_mtx$pdate <- as.Date(narvaez_rank_mtx$pdate, format = "%m/%d/%Y")

narvaez_rank_mtx$end_date <- as.Date(narvaez_rank_mtx$end_date, format = "%m/%d/%Y")

narvaez_rank_mtx


