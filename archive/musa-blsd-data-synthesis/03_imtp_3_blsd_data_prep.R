library(readr)
library(janitor)
library(PlackettLuce)
library(gosset)

source("scripts/utils/correct_musa_names.R")

imtp_3_folder <- "data/agtrials_banana/clean/imtp/imtp_3/black_sigatoka/trial_data_csv/"

imtp_3_files <- list.files(path = imtp_3_folder, pattern = "\\.csv", full.names = TRUE) 

imtp_3_data <- lapply(imtp_3_files, read_csv)

imtp_3_data

imtp_3_data <- lapply(imtp_3_data, janitor::clean_names)

lapply(imtp_3_data, colnames)

#Use file names to identify each trial
trial_names_imtp3 <- gsub(pattern = "data/agtrials_banana/clean/imtp/imtp_3/black_sigatoka/trial_data_csv/", replacement = "", x = imtp_3_files)
trial_names_imtp3 <- gsub(pattern = "_trial_data|\\.csv", replacement = "", x = trial_names_imtp3)
trial_names_imtp3 <- gsub(pattern = "-3", replacement = "", x = trial_names_imtp3)
trial_names_imtp3

names(imtp_3_data) <- trial_names_imtp3
imtp_3_data
length(imtp_3_data)

cols_to_search_imtp_3 <- c("cultivar",
                           "block", 
                           "plant",
                           "mat_id",
                           "plant_cycle",
                           "date_planted",
                           "date_shooting",
                           "youngest_leaf_spotted",
                           "disease_development_time_days_growing_phase")

imtp_3_data <- lapply(imtp_3_data, function(X) X[, names(X) %in% cols_to_search_imtp_3])

for(i in 1:length(imtp_3_data)){
  
 names(imtp_3_data[[i]])[1] <- "genotypes"
  
}

imtp_3_data <- lapply(imtp_3_data, correct_names)

imtp_3_data <- lapply(imtp_3_data, as.data.frame)

lapply(imtp_3_data, function(X) unique(X$genotypes))

#There is no information about what is "Local" variety in trial 16166, then it should be removed
imtp_3_data$imtp_16166 <- imtp_3_data$imtp_16166[!imtp_3_data$imtp_16166$genotypes == "Local", ]

lapply(imtp_3_data, head)

lapply(imtp_3_data, nrow)

lapply(imtp_3_data, function(X) nrow(X[!is.na(X$youngest_leaf_spotted),]))

lapply(imtp_3_data, function(X) nrow(X[!is.na(X$disease_development_time_days_growing_phase),]))

imtp_3_data[c(1,2,5)]

#in trials 3 and 4 is better to use disease development time
names(imtp_3_data[[3]])[7] <- "bls_rating"

names(imtp_3_data[[4]])[8] <- "bls_rating"

#in trials 1, 2, 5 only youngest leaf spotted is available

names(imtp_3_data[[1]])[8] <- "bls_rating"

names(imtp_3_data[[2]])[8] <- "bls_rating"

names(imtp_3_data[[5]])[8] <- "bls_rating"

#Select required columns
imtp_3_data <- lapply(imtp_3_data, function(X) X[, colnames(X) %in% c("genotypes",
                                                                      "block", 
                                                                      "plant",
                                                                      "plant_cycle",
                                                                      "date_planted",
                                                                      "date_shooting",
                                                                      "bls_rating")])

imtp_3_data <- lapply(imtp_3_data, function(X) X[!is.na(X$bls_rating), ])

lapply(imtp_3_data, nrow)

lapply(imtp_3_data, function(X) X[X$plant_cycle == "ratoon",])

#ratoon plant cycle should be removed, as it doesn't have time
imtp_3_data <- lapply(imtp_3_data, function(X) X[X$plant_cycle == "mother", ])

#trial 16166 has some data points with date_shooting with problematic format
imtp_3_data$imtp_16166$date_shooting <- gsub(pattern = "/03", 
                                             x = imtp_3_data$imtp_16166$date_shooting,
                                             replacement = "/2003")

imtp_3_data$imtp_16166$date_shooting <- gsub(pattern = "/02", 
                                             x = imtp_3_data$imtp_16166$date_shooting,
                                             replacement = "/2002")

#now format the dates of the rest of the data

for(i in seq_along(imtp_3_data)){
  
  imtp_3_data[[i]]$date_planted <- as.Date(imtp_3_data[[i]]$date_planted,
                                           tryFormats = c("%d/%m/%Y",
                                                          "%m/%d/%Y"))
  
  imtp_3_data[[i]]$date_shooting <- as.Date(imtp_3_data[[i]]$date_shooting, 
                                            tryFormats = c("%d/%m/%Y", 
                                                           "%m/%d/%Y"))
  
  mean_date_planted <- mean(imtp_3_data[[i]]$date_planted, 
                            na.rm = T)

  mean_date_shooting <- mean(imtp_3_data[[i]]$date_shooting, 
                             na.rm = T)

  no_date_planted <- length(imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_planted), 
                                             "date_planted"])

  no_date_shooting <- length(imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_shooting), 
                                              "date_shooting"])
  
  #Replace NA's with mean values
  
  imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_planted), "date_planted"] <- rep(mean_date_planted,
                                                                                no_date_planted)
  
  imtp_3_data[[i]][is.na(imtp_3_data[[i]]$date_shooting), "date_shooting"] <- rep(mean_date_shooting,
                                                                                  no_date_shooting)
  
}

imtp_3_data$imtp_16166$date_shooting

group_by(imtp_3_data$imtp_16166, genotypes, block)%>%
  summarise(bls_rating = mean(bls_rating), pdate = mean(date_planted), sdate = mean(date_shooting))

imtp_3_data_means <- lapply(imtp_3_data, function(X) group_by(X, genotypes, block)%>%
                              summarise(bls_rating = mean(bls_rating)))


imtp_3_data_dates <- lapply(imtp_3_data, function(X) group_by(X,  block)%>%
                              summarise(pdate = mean(date_planted),
                                        sdate = mean(date_shooting)))

imtp_3_data_dates <- lapply(imtp_3_data_dates, as.data.frame)


imtp_3_data_dates <- do.call(rbind, imtp_3_data_dates)

dates_id <- gsub(pattern = "[.][1-5]{1}", replacement = "", x = rownames(imtp_3_data_dates))

imtp_3_data_dates$id <- dates_id

imtp_3_data_dates$id2 <- paste(dates_id, imtp_3_data_dates$block, sep = "_")

row.names(imtp_3_data_dates) <- NULL

lapply(imtp_3_data_means, function(X) split(X, f = list(X$block)))


#split trials by block
imtp_3_data_ext <- lapply(imtp_3_data_means, function(X) split(X, f = list(X$block)))

length(imtp_3_data_ext)


for(i in seq_along(imtp_3_data_ext)){
  
  names(imtp_3_data_ext[[i]]) <- paste(names(imtp_3_data_ext)[i], 
                                       names(imtp_3_data_ext[[i]]),
                                       sep = "_")
  
}

names(imtp_3_data_ext[[1]][1])

names(imtp_3_data_ext[1])

for(i in seq_along(imtp_3_data_ext)){
  
  length_j <- length(imtp_3_data_ext[[i]])
  print(length_j)
  for(j in 1:length_j){

    imtp_3_data_ext[[i]][[j]]$id <- rep(names(imtp_3_data_ext[[i]][j]), nrow(imtp_3_data_ext[[i]][[j]]))
    
    imtp_3_data_ext[[i]][[j]] <- as.data.frame(imtp_3_data_ext[[i]][[j]])
  }
  
}



for(i in seq_along(imtp_3_data_ext)){
  imtp_3_data_ext[[i]] <- do.call(what = rbind, 
                                        args = c(imtp_3_data_ext[[i]], 
                                                 make.row.names = FALSE))  
  
}

imtp_3_data_ext <- do.call(what = rbind,
                           args = c(imtp_3_data_ext, 
                                    make.row.names = FALSE))

imtp_3_data_ext

head(imtp_3_data_ext)

imtp_3_rank <- gosset::rank_numeric(data = imtp_3_data_ext,
                                    items = 1,
                                    input = 3,
                                    id = 4,
                                    group = FALSE,
                                    ascending = FALSE)

imtp_3_rank_mtx <- as.data.frame(unclass(imtp_3_rank))

unique(imtp_3_data_ext$id)

row.names(imtp_3_rank_mtx) <- unique(imtp_3_data_ext$id)

imtp_3_rank_mtx$trial_id <- gsub(pattern = "_|[A-Z]|[a-z]",
                                 replacement = "",
                                 row.names(imtp_3_rank_mtx))

imtp_3_rank_mtx

nrow(imtp_3_rank_mtx)

imtp_loc_dates <- readr::read_csv("data/agtrials_banana/clean/imtp/imtp_dates.csv")

imtp_loc_dates$trial_id2

imtp_3_loc_dates <- imtp_loc_dates[imtp_loc_dates$trial_id2 %in% imtp_3_rank_mtx$trial_id , ]

imtp_3_loc_dates <- imtp_3_loc_dates[imtp_3_loc_dates$cycle == "mother", ]

imtp_3_loc_dates <- imtp_3_loc_dates[c("trial_id2",
                                       "latitude", 
                                       "longitude",
                                       "planting_date")]

names(imtp_3_loc_dates) <- c("trial_id2","lat","lon","pdate")

imtp_3_rank_mtx <- merge(x = imtp_3_rank_mtx,
                         y = imtp_3_loc_dates,
                         by.x = "trial_id",
                         by.y = "trial_id2"
                         )

imtp_3_rank_mtx$pdate <- as.Date(imtp_3_rank_mtx$pdate, format = "%m/%d/%Y")

colnames(imtp_3_rank_mtx)

#imtp_3_rank_mtx <- imtp_3_rank_mtx[, c(1:37, 43:45)]

colnames(imtp_3_data_dates)[3] <- "end_date"

imtp_3_rank_mtx <- cbind(imtp_3_rank_mtx, "end_date" = imtp_3_data_dates$end_date)    

#imtp_3_rank_mtx <- imtp_3_rank_mtx[, c(1:39, 42:43)]

ncol(imtp_3_rank_mtx)

#names(imtp_3_rank_mtx)[38:41] <- c("lat","lon","pdate","end_date")

imtp_3_rank_mtx <- imtp_3_rank_mtx[!colnames(imtp_3_rank_mtx) %in% c("trial_id")]

row.names(imtp_3_rank_mtx) <- unique(imtp_3_data_ext$id)

#imtp_3_rank_time_loc <- imtp_3_rank_mtx[38:41]


#### trial 16181 is in a separate file/dataset ####

trial_16181_agr<-read_csv("data/agtrials_banana/clean/imtp/imtp_3/black_sigatoka/TrialData16181/imtp-3_16181_trial_data_agronomic.csv")

trial_16181_di<-read_csv("data/agtrials_banana/clean/imtp/imtp_3/black_sigatoka/TrialData16181/imtp-3_16181_trial_data_disease.csv")

trial_16181_agr <- janitor::clean_names(trial_16181_agr)
trial_16181_di  <- janitor::clean_names(trial_16181_di)

head(trial_16181_agr)
head(trial_16181_di)

trial_16181_agr <- as.data.frame(trial_16181_agr)
trial_16181_di <- as.data.frame(trial_16181_di)

nrow(trial_16181_agr)

nrow(trial_16181_di)

#merge disease and agronomic datasets

trial_16181_data <- merge(x = trial_16181_agr,
                          y = trial_16181_di,
                          by.x = c("cultivar", "plant", "mat_id", "plant_cycle"),
                          by.y = c("cultivar", "plant", "mat_id", "plant_cycle"))

colnames(trial_16181_data)
#check which variable has the higher number of NA's
sum(is.na(trial_16181_data$number_of_functional_leaves_shooting))
sum(is.na(trial_16181_data$infection_index))

#check variance of each variable
var(trial_16181_data$number_of_functional_leaves_shooting[!is.na(trial_16181_data$number_of_functional_leaves_shooting)])
var(trial_16181_data$infection_index[!is.na(trial_16181_data$infection_index)])

hist(trial_16181_data$number_of_functional_leaves_shooting[!is.na(trial_16181_data$number_of_functional_leaves_shooting)])
hist(trial_16181_data$infection_index[!is.na(trial_16181_data$infection_index)])

#infection_index has higher number of NA's but is more continuous and has higher variability

trial_16181_data <- trial_16181_data[!is.na(trial_16181_data$infection_index), ]

#ratoon data points does not have date_planted and date_shooting
trial_16181_data <- trial_16181_data[trial_16181_data$plant_cycle == "mother", ]

head(trial_16181_data)
names(trial_16181_data)
nrow(trial_16181_data)


trial_16181_cols_sel <- c("cultivar",
                          "plant",
                          "mat_id",
                          "date_planted",
                          "date_shooting",
                          "infection_index")

#select only required variables
trial_16181_data <- trial_16181_data[, trial_16181_cols_sel]

names(trial_16181_data)[1] <- "genotypes"

trial_16181_data <- correct_names(trial_16181_data)


trial_16181_data_agg <- aggregate(infection_index ~ genotypes, data = trial_16181_data, FUN = mean)

trial_16181_data_agg$id <- rep("imtp_16181", nrow(trial_16181_data_agg))

trial_16181_rank <- gosset::rank_numeric(data = trial_16181_data_agg,
                                         items = 1,
                                         input = 2,
                                         id = 3,
                                         ascending = TRUE)

trial_16181_rank_mtx <- unclass(trial_16181_rank)

row.names(trial_16181_rank_mtx) <- trial_16181_data_agg$id[1]

trial_16181_rank_mtx <- as.data.frame(trial_16181_rank_mtx)

imtp_loc_dates <- readr::read_csv("data/agtrials_banana/clean/imtp/imtp_dates.csv")

imtp_loc_dates$trial_id

imtp_loc_dates$trial_id3 <- gsub(pattern = "-1|-2|-3|_trial_data", 
                                 replacement = "",
                                 x = imtp_loc_dates$trial_id)

trial_16181_loc <- imtp_loc_dates[imtp_loc_dates$trial_id2 == 16181 & imtp_loc_dates$cycle == "mother",
                                  c("latitude", "longitude", "planting_date")]

trial_16181_loc$end_date <- mean(as.Date(trial_16181_data[!is.na(trial_16181_data$date_shooting),
                                                          "date_shooting"], format = "%d/%m/%Y"))


trial_16181_loc$planting_date <- as.Date(trial_16181_loc$planting_date, format = "%m/%d/%Y")

trial_16181_loc <- as.data.frame(trial_16181_loc)

names(trial_16181_loc) <- c("lat","lon","pdate","end_date")

row.names(trial_16181_loc) <- "imtp_16181"

trial_16181_rank_mtx <- cbind(trial_16181_rank_mtx, trial_16181_loc)

#### End of trial 16181 ####

colnames(imtp_3_rank_mtx)
colnames(trial_16181_rank_mtx)

#imtp_3_rank_mtx <- imtp_3_rank_mtx[!colnames(imtp_3_rank_mtx) %in% "trial_id"]

trial_16181_rank_mtx

imtp_3_rank_mtx <- as.data.frame(dplyr::bind_rows(imtp_3_rank_mtx, trial_16181_rank_mtx))

imtp_3_rank_time_loc <- imtp_3_rank_mtx[colnames(imtp_3_rank_mtx) %in% c("lat",
                                                                         "lon", 
                                                                         "pdate", 
                                                                         "end_date")]


imtp_3_rank_mtx <- imtp_3_rank_mtx[!colnames(imtp_3_rank_mtx) %in% c("lat",
                                                                     "lon", 
                                                                     "pdate", 
                                                                    "end_date")]

imtp_3_rank_mtx <- cbind(imtp_3_rank_mtx, imtp_3_rank_time_loc)

nrow(imtp_3_rank_mtx)


