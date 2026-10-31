library("climatrends")
library("nasapower")
library("chirps")

dat = read.csv("data/planting-dates-longlat.csv")

dat$plantingdate = as.Date(dat$plantingdate)

dat = split(dat, dat$trial_id)

ndays = 70

# CHIRPS 
chirps_dat = lapply(dat, function(x){
  
  dates = c(min(x$plantingdate), (max(x$plantingdate) + ndays))
  
  dates = as.character(dates)
  
  r1 = get_chirps(x[,c("longitude",  "latitude")], 
                  dates, 
                  server = "CHC",
                  as.matrix = TRUE)
  
})

chirps_dat = lapply(chirps_dat, function(x){
  newnames = dimnames(x)[[2]]
  newnames = gsub("chirps-v2.0.", "", newnames)
  newnames = gsub("[.]", "-", newnames)
  names(x) = newnames
  x
})

rain = list()

for(i in seq_along(dat)){
  r = rainfall(as.matrix(chirps_dat[[i]]),
               day.one = dat[[i]]$plantingdate, 
               span = 65)
  r$block_id = dat[[i]]$block_id
  rain[[i]] = r
}

rain

rain = do.call("rbind", rain)

dat = do.call("rbind", dat)

rain$block_id = dat$block_id

rain = na.omit(rain)

write.csv(rain, "data/rain-covariates.csv", row.names = FALSE)


# temperature
make_tiles = function(df, tile_size = 4) {
  df$tile_lon = floor(df$longitude / tile_size) * tile_size
  df$tile_lat = floor(df$latitude  / tile_size) * tile_size
  df$tile_id = paste(df$tile_lon, df$tile_lat, sep = "_")
  as.integer(as.factor(df$tile_id))
}

dat$tiles = make_tiles(dat, tile_size = 4)

dat = split(dat, paste0(dat$tiles, dat$trial_id))

temp = lapply(dat, function(x){
  temperature(x[,c("longitude", "latitude")],
              day.one = x$plantingdate,
              span = ndays)
})

temp = do.call("rbind", temp)

dat = do.call("rbind", dat)

temp$block_id = dat$block_id

temp = na.omit(temp)

write.csv(temp, "data/temp-covariates.csv", row.names = FALSE)



