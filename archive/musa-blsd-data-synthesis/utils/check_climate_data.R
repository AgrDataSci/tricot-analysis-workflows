load("data/climate/musa_trials_prec.rda")
load("data/climate/musa_trials_temp_daytime_max.rda")
load("data/climate/musa_trials_temp_nighttime_min.rda")
# 
# prec_tot <- lapply(musa_trials_prec, function(X) sum(as.numeric(X)))
# 
# prec_tot <- data.frame("total_prec" = unlist(musa_trials_prec_mean))
# 
# cbind(prec_tot, musa_trials_climate$Rtotal)
# 
# 
# temp_daytime_max_mean <- lapply(musa_trials_temp_daytime_max, function(X) mean(as.numeric(X)))
# 
# temp_daytime_max_mean <- data.frame("max_dt" = unlist(temp_daytime_max_mean))
# 
# temp_daytime_max_mean <- temp_daytime_max_mean - 273.15
# 
# cbind(temp_daytime_max_mean, musa_trials_climate$maxDT - 273.15)


r1 <- data.frame("date" = names(musa_trials_prec[[91]]), "rainfall" = unlist(musa_trials_prec[91]))

r1$site <- 

r3 <- data.frame("date" = names(musa_trials_prec[[93]]), "rainfall" = unlist(musa_trials_prec[93]))


r3

ggplot() + 
  geom_line(data = r3, aes(x = date, y = rainfall),
            group = 1,
            color = "#00AFBB",
            size = 2) +
  geom_line(data = r1, aes(x = date, y = rainfall),
            group = 1,
            color = "#FC4E07",
            size = 2)


mean(r3$rainfall)

  
