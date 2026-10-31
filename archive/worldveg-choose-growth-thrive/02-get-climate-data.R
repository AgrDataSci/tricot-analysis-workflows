library("chirps")
library("terra")
library("climatrends")

dat = read.csv("data/block-data.csv")

dat$plantingdate = as.Date(dat$plantingdate)

n = nrow(dat)

ndays = 4

dates =  c(min(dat$plantingdate), (min(dat$plantingdate) + ndays))

dates = as.character(dates)

longlat = vect(dat[,c("longitude", "latitude")],
               geom = c("longitude", "latitude"),
               crs = "EPSG:4326")

# cat("download CHIRPS data \n")
# chirpsv3 = get_chirps_raw(dates, version = "3.0", type = "rnl")

#cat("extract CHIRPS data \n")
#rain = extract(chirpsv3, longlat)

# names(rain)
# rain = rain[, -1]
# newnames = dimnames(rain)[[2]]
# newnames = gsub("chirps-v3.0.rnl.", "", newnames)
# newnames = gsub("[.]", "-", newnames)
# names(rain) = newnames
# names(rain)

# cat("download CHIRTS Tmax data \n")
# tmax = get_chirts_era5_raw(dates, var = "Tmax")
# 
# cat("download CHIRTS Tmin data \n")
# tmin = get_chirts_era5_raw(dates, var = "Tmin")

# cat("extract CHIRTS Tmax data \n")
# tempmax = extract(tmax, longlat)

# tempmax = tempmax[, -1]
# names(tempmax)
# newnames = dimnames(tempmax)[[2]]
# newnames = gsub("CHIRTS-ERA5.daily_Tmax.", "", newnames)
# newnames = gsub("[.]", "-", newnames)
# names(tempmax) = newnames
# names(tempmax)
 
# cat("extract CHIRTS Tmin data \n")
# tempmin = extract(tmin, longlat)

# tempmin = tempmin[, -1]
# names(tempmin)
# newnames = dimnames(tempmin)[[2]]
# newnames = gsub("CHIRTS-ERA5.daily_Tmin.", "", newnames)
# newnames = gsub("[.]", "-", newnames)
# names(tempmin) = newnames
# names(tempmin)

# temperature
make_tiles = function(df, tile_size = 4) {
  df$tile_lon = floor(df$longitude / tile_size) * tile_size
  df$tile_lat = floor(df$latitude  / tile_size) * tile_size
  df$tile_id = paste(df$tile_lon, df$tile_lat, sep = "_")
  as.integer(as.factor(df$tile_id))
}

dat$tiles = make_tiles(dat, tile_size = 4)

dat$row_id = seq_len(nrow(dat))

dat_split = split(dat, paste0(dat$tiles, dat$trial_id))

temp = lapply(dat_split, function(x) {
  temperature(x[, c("longitude", "latitude")],
              day.one = x$plantingdate,
              span = ndays)
})

temp_indices = do.call("rbind", temp)

# temp_max = lapply(dat_split, function(x) {
#   get_timeseries(
#     x[, c("longitude", "latitude")],
#     day.one = x$plantingdate,
#     pars = "T2M_MAX",
#     span = ndays,
#     as.matrix = TRUE
#   )[[1]]
# })
# 
# temp_min = lapply(dat_split, function(x) {
#   get_timeseries(
#     x[, c("longitude", "latitude")],
#     day.one = x$plantingdate,
#     pars = "T2M_MIN",
#     span = ndays,
#     as.matrix = TRUE
#   )[[1]]
# })
# 
# nms = sort(unique(unlist(lapply(temp_max, colnames))))
# 
# temp = array(
#   NA_real_,
#   dim = c(2, nrow(dat), length(nms)),
#   dimnames = list(c("tmax", "tmin"), dat$row_id, nms)
# )
# 
# for (i in seq_along(temp_max)) {
#   x = temp_max[[i]]
#   rows = dat_split[[i]]$row_id
#   cols = match(colnames(x), nms)
#   
#   temp["tmax", as.character(rows), cols] = x
#   
#   
#   x = temp_min[[i]]
#   rows = dat_split[[i]]$row_id
#   cols = match(colnames(x), nms)
#   
#   temp["tmin", as.character(rows), cols] = x
# }


rain = as.matrix(rain)

rain_indices = rainfall(rain, 
                        day.one = dat$plantingdate,
                        span = ndays)

save(rain, temp, rain_indices, temp_indices,
     file = "data/climate-data-matrices2.rda")

