library(readr)
library(janitor)
library(PlackettLuce)
library(gosset)

source("scripts/utils/correct_musa_names.R")

#read trial files
imtp_1_folder <- "data/agtrials_banana/clean/imtp/imtp_1/from_AgTrials/trial_data_csv/"

imtp_1_files <- list.files(path = imtp_1_folder, pattern = "\\.csv", full.names = TRUE)

imtp_1_data <- lapply(imtp_1_files, read_csv)

imtp_1_data <- lapply(imtp_1_data, function(X) janitor::clean_names(X))

imtp_1_data <- lapply(imtp_1_data, function(X) as.data.frame(X))

imtp_1_data <- lapply(imtp_1_data, function(X) correct_names(X))

#use trial file name to creat trial id
imtp_1_trial_names <- gsub(pattern = "data/agtrials_banana/clean/imtp/imtp_1/from_AgTrials/trial_data_csv/imtp-1_",
                                  replacement = "imtp_",
                                  x = imtp_1_files)

imtp_1_trial_names <- gsub(pattern = ".csv",
                           replacement = "",
                           x = imtp_1_trial_names)

imtp_1_trial_names <- gsub(pattern = "_trial_data",
                           replacement = "",
                           x = imtp_1_trial_names)

imtp_1_trial_names

trial_id <- imtp_1_trial_names

names(imtp_1_data) <- trial_id

#Check available data and select only the required variables

imtp_1_data[[1]]

imtp_1_data[[1]] <- imtp_1_data[[1]][, c("genotypes",
                                         "time_from_planting_to_shooting_days",
                                         "development_time_days")]

#genotypes Tuu Gia, Pahang and Calcutta 4 did not develop symptoms 
#We cannot use these genotypes 
#remove
imtp_1_data[[1]] <- imtp_1_data[[1]][!is.na(imtp_1_data[[1]]$development_time_days), ]


imtp_1_data[[2]]

imtp_1_data[[2]] <- imtp_1_data[[2]][,c("genotypes",
                                        "time_from_shooting_to_shooting_days",
                                        "disease_index_at_shooting_modified_scale_of_stover")]

imtp_1_data[[3]]

imtp_1_data[[3]] <- imtp_1_data[[3]][, c("genotypes",
                     "time_from_planting_to_shooting_days",
                     "disease_development_time_days")]

#this trial does not have time from planting to shooting and should be removed
imtp_1_data[[4]]


imtp_1_data[[5]]

imtp_1_data[[5]] <- imtp_1_data[[5]][,c("genotypes",
                    "time_from_planting_to_shooting_days",
                    "average_disease_development_time_days")]

imtp_1_data[[5]]$average_disease_development_time_days <- as.numeric(imtp_1_data[[5]]$average_disease_development_time_days)

#remove genotypes without disease development time
imtp_1_data[[5]] <- imtp_1_data[[5]][!is.na(imtp_1_data[[5]]$average_disease_development_time_days), ]


#to avoid discarding data points, input the time from planting to shooting average to 
#genotypes without data for this variable
imtp_1_data[[5]][is.na(imtp_1_data[[5]]$time_from_planting_to_shooting_days),
                 "time_from_planting_to_shooting_days"] <- mean(imtp_1_data[[5]][!is.na(imtp_1_data[[5]]$time_from_planting_to_shooting_days),
                                                                         "time_from_planting_to_shooting_days"])
#this trial does not have time from planting to shooting
#remove
imtp_1_data[[6]]

imtp_1_data <- imtp_1_data[c(1,2,3,5)]

#set the bls rating with available measurements
imtp_1_data$imtp_1566_c1$bls_rating <- as.numeric(imtp_1_data$imtp_1566_c1$development_time_days)

imtp_1_data$imtp_1566_c2$bls_rating <- as.numeric(imtp_1_data$imtp_1566_c2$disease_index_at_shooting_modified_scale_of_stover)

imtp_1_data$imtp_1683$bls_rating <- as.numeric(imtp_1_data$imtp_1683$disease_development_time_days)

imtp_1_data$imtp_1924$bls_rating <- as.numeric(imtp_1_data$imtp_1924$average_disease_development_time_days)


names(imtp_1_data[[1]])[2] <- "obs_time"

names(imtp_1_data[[2]])[2] <- "obs_time"

names(imtp_1_data[[3]])[2] <- "obs_time"

names(imtp_1_data[[4]])[2] <- "obs_time"

imtp_1_data <- lapply(imtp_1_data, function(X) X[, c("genotypes", "obs_time", "bls_rating")])

imtp_1_data


#set trial Id
for(i in 1:length(imtp_1_data)){
  
  imtp_1_data[[i]]$trial_id <- names(imtp_1_data[i])
}

#trial imtp_1566_c2 has a different rating scale, where lower values are better
#set ascending = TRUE
imtp_1_rank_t2 <- gosset::rank_numeric(data = imtp_1_data[[2]], 
                                       items = 1,
                                       input = 3,
                                       id = 4,
                                       ascending = TRUE,
                                       group = FALSE)

imtp_1_rank_t2 <- as.data.frame(unclass(imtp_1_rank_t2))

row.names(imtp_1_rank_t2) <- names(imtp_1_data[2])

#the rest of trials have rating scales where higher is better
imtp_1_data_t1_3_4 <- do.call(rbind, c(imtp_1_data[c(1,3,4)], make.row.names = FALSE))

imtp_1_rank_t1_3_4 <- gosset::rank_numeric(data = imtp_1_data_t1_3_4, 
                                         items = 1,
                                         input = 3,
                                         id = 4,
                                         ascending = FALSE,
                                         group = FALSE)

imtp_1_rank_t1_3_4 <- as.data.frame(unclass(imtp_1_rank_t1_3_4))

row.names(imtp_1_rank_t1_3_4) <- names(imtp_1_data[c(1,3,4)])

imtp_1_rank_mtx <- dplyr::bind_rows(imtp_1_rank_t1_3_4, imtp_1_rank_t2)

imtp_1_rank_mtx$id <- row.names(imtp_1_rank_mtx)

imtp_loc_dates <- readr::read_csv("data/agtrials_banana/clean/imtp/imtp_dates.csv")

imtp_loc_dates$trial_id

imtp_loc_dates$trial_id3 <- gsub(pattern = "-1|-2|-3|_trial_data", 
                                 replacement = "",
                                 x = imtp_loc_dates$trial_id)

imtp_1_loc_dates <- imtp_loc_dates[imtp_loc_dates$trial_id3 %in% row.names(imtp_1_rank_mtx),
                                   c("trial_id3",
                                     "latitude", 
                                     "longitude",
                                     "planting_date")]


imtp_1_rank_mtx <- merge(x = imtp_1_rank_mtx,
                         y = imtp_1_loc_dates,
                         by.x = "id",
                         by.y = "trial_id3")

imtp_1_rank_mtx$planting_date <- as.Date(imtp_1_rank_mtx$planting_date, format = "%d/%m/%Y")

imtp_1_time_1_3_4 <- aggregate(obs_time ~ trial_id, data = imtp_1_data_t1_3_4, FUN = mean)

#data for second cycle seems to be registered correctly, where time of the first cycle
#was already substracted
imtp_1_time_2 <- data.frame(trial_id  = "imtp_1566_c2",
                            obs_time = mean(imtp_1_data[[2]]$obs_time))

imtp_1_time <- rbind(imtp_1_time_1_3_4, imtp_1_time_2)

imtp_1_time <- merge(x = imtp_1_rank_mtx,
                     y = imtp_1_time,
                     by.x = "id",
                     by.y = "trial_id")

#starting data of second cycle of imtp-1566 should be adjusted to the shooting day of first cycle
imtp_1_time[imtp_1_time$id == "imtp_1566_c2", ]$planting_date <- imtp_1_time[imtp_1_time$id == "imtp_1566_c1",
                                                                             ]$planting_date +  imtp_1_time[imtp_1_time$id == "imtp_1566_c1",
                                                                                                            ]$obs_time

imtp_1_time

imtp_1_rank_mtx$planting_date <- imtp_1_time$planting_date

#Calculate end date
imtp_1_rank_mtx$end_date <- imtp_1_rank_mtx$planting_date + imtp_1_time$obs_time

names(imtp_1_rank_mtx)

imtp1_loc_dates_cols <- which(colnames(imtp_1_rank_mtx) %in% c("latitude",
                                                               "longitude",
                                                               "planting_date",
                                                               "end_date"))

names(imtp_1_rank_mtx)[imtp1_loc_dates_cols] <-  c("lat","lon","pdate","end_date")

row.names(imtp_1_rank_mtx) <- imtp_1_rank_mtx$id

imtp_1_rank_mtx <- imtp_1_rank_mtx[-1]

imtp_1_rank_mtx
