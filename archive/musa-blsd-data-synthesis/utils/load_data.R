
#ranking matrices
musa_trials_data <- data.matrix(read.csv("data/processed/musa_trials_data.csv", row.names = 1, check.names = FALSE))


musa_trials_data

#trial locations
#musa_trials_time_loc <- read.csv("data/processed/musa_trials_time_loc.csv", row.names = 1, check.names = FALSE)

#climate data
musa_trials_climate <- read.csv("data/climate/musa_trials_climate.csv")
