library(readr)
library(janitor)
library(PlackettLuce)
library(gosset)

source("scripts/utils/correct_musa_names.R")

#read trial files
imtp_2_folder <- "data/agtrials_banana/clean/imtp/imtp_2/BLS/trial_data_csv/"

imtp_2_files <- list.files(path = imtp_2_folder, pattern = "\\.csv", full.names = TRUE)

imtp_2_data <- lapply(imtp_2_files, read_csv)

imtp_2_data

imtp_2_data <- lapply(imtp_2_data, janitor::clean_names)

#Use file names to identify each trial
trial_names_imtp2 <- gsub(pattern = ".csv", replacement = "", x = imtp_2_files)
trial_names_imtp2 <- gsub(pattern = "_trial_data", replacement = "", x = trial_names_imtp2)
trial_names_imtp2 <- gsub(pattern = "data/agtrials_banana/clean/imtp/imtp_2/BLS/trial_data/", replacement = "", x = trial_names_imtp2)
trial_names_imtp2 <- gsub(pattern = '-2', replacement = "", x = trial_names_imtp2)

trial_names_imtp2

names(imtp_2_data) <- trial_names_imtp2

imtp_2_data

length(imtp_2_data)

#set trial Id
for(i in 1:length(imtp_2_data)){
  
  imtp_2_data[[i]]$trial_id <- trial_names_imtp2[i]
  
}

lapply(imtp_2_data, colnames)

length(imtp_2_data)


cols_to_search <- c("genotypes", 
                    "time_from_planting_to_shooting_days",
                    "youngest_leaf_spotted",
                    "trial_id")

                    # "average_disease_development_time_days",
                    # "disease_development_time_days",
                    # "disease_index_at_shooting")

imtp_2_data <- lapply(imtp_2_data, function(X) X[, names(X) %in% cols_to_search])


#trial second cycle of imtp_2937 does not have time variables, should be removed
imtp_2_data <- imtp_2_data[-3]

#replace NAs by the mean of time from planting to shooting
tp2s_2938 <- imtp_2_data$imtp_2938[!is.na(imtp_2_data$imtp_2938$time_from_planting_to_shooting_days),
                           "time_from_planting_to_shooting_days"]

imtp_2_data$imtp_2938[is.na(imtp_2_data$imtp_2938$time_from_planting_to_shooting_days),
                      "time_from_planting_to_shooting_days"] <- mean(as.numeric(unlist(tp2s_2938)))


imtp_2_data <- do.call(rbind, imtp_2_data)

imtp_2_data <- correct_names(imtp_2_data)

imtp_2_data <- imtp_2_data[!is.na(imtp_2_data$youngest_leaf_spotted), ]

imtp_2_rank <- gosset::rank_numeric(data = imtp_2_data,
                                    items = 1,
                                    input = 3,
                                    id = 4,
                                    group = FALSE,
                                    ascending = FALSE)


imtp_2_rank_mtx <- as.data.frame(unclass(imtp_2_rank))

#imtp_2_rank_mtx$trial_id <- unique(imtp_2_data$trial_id)

row.names(imtp_2_rank_mtx) <- unique(imtp_2_data$trial_id)

imtp_2_rank_mtx$id <- row.names(imtp_2_rank_mtx)

imtp_2_rank_mtx

nrow(imtp_2_rank_mtx)

imtp_loc_dates <- readr::read_csv("data/agtrials_banana/clean/imtp/imtp_dates.csv")

imtp_loc_dates$trial_id

imtp_loc_dates$trial_id3 <- gsub(pattern = "-1|-2|-3|_trial_data", 
                                 replacement = "",
                                 x = imtp_loc_dates$trial_id)

imtp_2_loc_dates <- imtp_loc_dates[imtp_loc_dates$trial_id3 %in% row.names(imtp_2_rank_mtx),
                                   c("trial_id3",
                                     "latitude", 
                                     "longitude",
                                     "planting_date")]

imtp_2_rank_mtx <- merge(x = imtp_2_rank_mtx,
                         y = imtp_2_loc_dates,
                         by.x = "id",
                         by.y = "trial_id3")


imtp_2_rank_mtx$planting_date <- as.Date(imtp_2_rank_mtx$planting_date, format = "%m/%d/%Y")

imtp_2_time <- aggregate(time_from_planting_to_shooting_days ~ trial_id,
                         data = imtp_2_data,
                         FUN = mean)


imtp_2_rank_mtx$end_date <- imtp_2_rank_mtx$planting_date + imtp_2_time$time_from_planting_to_shooting_days

imtp_2_rank_mtx[c("id", "planting_date", "end_date")]

cycle_2_ids <- imtp_2_rank_mtx[grepl("_c2",imtp_2_rank_mtx$id),c("id")]

imtp_2_rank_mtx[grepl("_c2",imtp_2_rank_mtx$id),]$planting_date <- imtp_2_rank_mtx[imtp_2_rank_mtx$id %in% gsub("_c2","_c1", cycle_2_ids), "end_date"]

ncol(imtp_2_rank_mtx)

row.names(imtp_2_rank_mtx) <- imtp_2_rank_mtx$id

imtp_2_rank_mtx <- imtp_2_rank_mtx[-1]

names(imtp_1_rank_mtx)

imtp2_loc_dates_cols <- which(colnames(imtp_2_rank_mtx) %in% c("latitude",
                                                               "longitude",
                                                               "planting_date",
                                                               "end_date"))

names(imtp_2_rank_mtx)[imtp2_loc_dates_cols] <-  c("lat","lon","pdate","end_date")

imtp_2_rank_mtx

