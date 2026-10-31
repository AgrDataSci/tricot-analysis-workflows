

library(RCurl)

#Load functions ClimMobTools from GitHub
#repository https://github.com/kauedesousa/ClimMobTools/
tools <- getURL("https://raw.githubusercontent.com/kauedesousa/ClimMobTools/pilot/ClimMobCovariates.R", ssl.verifypeer = FALSE)
eval(parse(text = tools))

# #https://ndawn.ndsu.nodak.edu/help-wheat-growing-degree-days.html #https://link.springer.com/article/10.1007/BF03030690
cycle <- rbind(cbind(cycle = c("VEG","FLO","GRA"), days = c(0,0,0), span = c(0,0,0), kc = c(0.7, 1.1, 0.3), gdd = c(275,340,285),  crop = "commonbean"), 
               cbind(cycle = c("VEG","FLO","GRA"), days = c(0,0,0), span = c(0,0,0), kc = c(0.7, 1.1, 0.3), gdd = c(1255,570,250), crop = "rice"),
               cbind(cycle = c("VEG","FLO","GRA"), days = c(0,0,0), span = c(0,0,0), kc = c(0.7, 1.1, 0.3), gdd = c(1680,200,280), crop = "wheat"),
               cbind(cycle = c("VEG","FLO","GRA"), days = c(0,0,0), span = c(0,0,0), kc = c(0.7, 1.1, 0.3), gdd = c(1600,250,280), crop = "durumwheat"))




modis_factor <- 0.02
k_to_c <- 273.15

#Subset cycle information for crop[m]
cycle_m <- cycle[cycle[,6]==crop[m] , ]

if(crop[m]=="commonbean"){
  #add modis data 
  #read modis
  dates <- NULL
  for(i in c(2003:2016))
    dates <- c(dates, c(seq(as.Date(paste0(i, "-01-01"), "%Y-%m-%d"), as.Date(paste0(i, "-12-31"), "%Y-%m-%d"), 8)))
  dates <- as.Date(c(dates, seq(as.Date("2017-01-01", "%Y-%m-%d"), as.Date("2017-09-06", "%Y-%m-%d"), 8)), origin = "1970-01-01")

  full_dates <- seq(dates[1], dates[length(dates)], 1)
  #read modis day and night
  modis <- array(NA, dim = c(n, length(dates), 2), dimnames = list(NULL, as.character(dates), c(1:2)))
  for(i in 1:2){
    if(i == 1 ) d="day" else d="night"
    file <- raster::stack(list.files("E:/rasters/modis/smooth_Nicaragua/", pattern = d, full.names = T))
    x <- raster::extract(file, coord[,c("x","y")])
    x <- x * modis_factor - k_to_c #convert to Celsius
    x[x<1] <- NA #remove wrong data
    modis[ , , i] <- x
  }
  sum(is.na(modis))
  #apply linear interpolation to the 8-day temperature
  modis_approx <- array(NA, c(n, length(full_dates), 2), dimnames = list(NULL,  as.character(full_dates), c(1:2) ))
  for(i in 1:dim(modis)[1]){
    modis_approx[i,,1] <- approx(modis[i,,1], n = length(full_dates))[[2]]
    modis_approx[i,,2] <- approx(modis[i,,2], n = length(full_dates))[[2]]
  }
}

if(crop[m]=="durumwheat"){
  tiles <- c("h21v07","h21v08")
  dates <- NULL
  for(i in c(2003:2015)){
    dates <- c(dates, c(seq(as.Date(paste0(i, "-01-01"), "%Y-%m-%d"), as.Date(paste0(i, "-12-31"), "%Y-%m-%d"), 8)))}
  dates <- as.Date(c(dates, seq(as.Date("2016-01-01", "%Y-%m-%d"), as.Date("2016-12-26", "%Y-%m-%d"), 8)), origin = "1970-01-01")
  #vector with full dates
  full_dates <- seq(dates[1], dates[length(dates)], 1)
  #read modis day and night
  modis <- array(NA, dim = c(n, length(dates), 2), dimnames = list(NULL, as.character(dates), c(1:2)))
  #files in this directory has different extensions, so each tile is loaded individually
  for (i in 1:2){
    if(i == 1 ) d <- "day" else d <- "night"
    for(j in 1:length(tiles)){
      print(paste(tiles[j],sep=" "))
      file <- raster::stack(list.files(paste0("E:/rasters/modis/smooth_Ethiopia/", tiles[j]), pattern = d, full.names = T))
      x <- raster::extract(file, coord[,c("x","y")])
      x <- x * modis_factor - k_to_c #convert to Celsius
      x[x<1] <- NA #remove wrong data
      dimnames(x)[[1]] <- 1:n

      x <- x[!is.na(x[,3]), ]
      modis[ as.integer(dimnames(x)[[1]]) , , i] <- x
    }
  }

  rm(x)
  sum(is.na(modis))

  #replace NA's in temp using the median per day
  for(i in 1:dim(modis)[2]){
    modis[,i,1] <- ifelse(is.na(modis[,i,1]), median(modis[,i,1], na.rm = T), modis[,i,1])
    modis[,i,2] <- ifelse(is.na(modis[,i,2]), median(modis[,i,2], na.rm = T), modis[,i,2])
  }
  #apply linear interpolation to the 8-day temperature
  modis_approx <- array(NA, c(n, length(full_dates), 2), dimnames = list(1:n,  as.character(full_dates), c(1:2) ))
  for(i in 1:dim(modis)[1]){
    modis_approx[i,,1] <- approx(modis[i,,1], n = length(full_dates))[[2]]
    modis_approx[i,,2] <- approx(modis[i,,2], n = length(full_dates))[[2]]
  }
}

if(crop[m]=="wheat"){
  tiles <- c("h24v06","h25v06")
  #vector with 8-day dates
  dates <- NULL
  for(i in c(2003:2016))
    dates <- c(dates, c(seq(as.Date(paste0(i, "-01-01"), "%Y-%m-%d"), as.Date(paste0(i, "-12-31"), "%Y-%m-%d"), 8)))
  dates <- as.Date(c(dates, seq(as.Date("2017-01-01", "%Y-%m-%d"), as.Date("2017-09-22", "%Y-%m-%d"), 8)), origin = "1970-01-01")
  #sequence of dates by day
  full_dates <- seq(dates[1], dates[length(dates)], 1)
  #read modis day and night
  modis <- array(NA, dim = c(n, length(dates), 2), dimnames = list(c(1:n), as.character(dates), c(1:2)))

  #files in this directory has different extensions, so each tile is loaded individually
  for (i in 1:2){
    if(i == 1 ) d <- "day" else d <- "night"
    for(j in 1:length(tiles)){
      print(paste(d,tiles[j],sep=" "))
      file <- raster::stack(list.files(paste0("E:/rasters/modis/smooth_India/", tiles[j]), pattern = d, full.names = T))
      x <- raster::extract(file, coord[,c("x","y")])
      x <- x * modis_factor - k_to_c #convert to Celsius
      x[x<1] <- NA #remove wrong data
      dimnames(x)[[1]] <- 1:n

      x <- x[!is.na(x[,3]), ]
      modis[ as.integer(dimnames(x)[[1]]) , , i] <- x
    }
  }
  rm(x)
  sum(is.na(modis))

  #replace NA's in temp using the median per day
  for(i in 1:dim(modis)[2]){
    modis[,i,1] <- ifelse(is.na(modis[,i,1]), median(modis[,i,1], na.rm = T), modis[,i,1])
    modis[,i,2] <- ifelse(is.na(modis[,i,2]), median(modis[,i,2], na.rm = T), modis[,i,2])
  }
  #apply linear interpolation to the 8-day temperature
  modis_approx <- array(NA, c(n, length(full_dates), 2), dimnames = list(1:n,  as.character(full_dates), c(1:2) ))
  for(i in 1:dim(modis)[1]){
    modis_approx[i,,1] <- approx(modis[i,,1], n = length(full_dates))[[2]]
    modis_approx[i,,2] <- approx(modis[i,,2], n = length(full_dates))[[2]]
  }

}

#get the median of planting dates
pd <- NULL

for(i in 1:length(pdates)){
  
  f <- pdates[[i]]
  
  f <- as.integer(as.vector(apply(f, 2, median)))
  
  if(crop[m]=="commonbean") {
    
    v <- as.integer(format(as.Date(f, origin="1970-01-01"),'%m'))
    
    f <- f[v<2 | v>10]
    
  }
  
  pd <- c(pd, f)

}

avgclim <- list()

#run over quantiles dates
for(k in seq_along(pd)){
  
  #take planting dates
  p <- as.Date(rep(pd[k], times = n), formart="%Y-%m-%d", origin="1970-01-01")
  
  #calculate growing degree-days
  ts <- 180
  degree <- sum(as.integer(cycle_m[,5]))
  
  #temperature
  temp <- array(NA, c(n, ts, 2), dimnames = list(NULL,  NULL, c(1:2) ))
  temp[,,1] <- get.timespan(modis_approx[,,1], p, ts)
  temp[,,2] <- get.timespan(modis_approx[,,2], p, ts)

  #add gdd to the main dataset
  GDD <- get.GDD(temp, degree)
  
  #degree days in the vegetative period
  GDD_veg <- get.GDD(temp, as.integer(cycle_m[1,5]))
  
  #degree days in the reproductive period
  temp <- array(NA, c(n, ts, 2), dimnames = list(NULL,  NULL, c(1:2) ))
  temp[,,1] <- get.timespan(modis_approx[,,1], (p + GDD_veg), ts)
  temp[,,2] <- get.timespan(modis_approx[,,2], (p + GDD_veg), ts)
  GDD_flo <- get.GDD(temp, as.integer(cycle_m[2,5]))
  
  #degree days in the grain filling period
  temp <- array(NA, c(n, ts, 2), dimnames = list(NULL,  NULL, c(1:2) ))
  temp[,,1] <- get.timespan(modis_approx[,,1], (p + GDD_veg + GDD_flo), 60)
  temp[,,2] <- get.timespan(modis_approx[,,2], (p + GDD_veg + GDD_flo), 60)
  GDD_gra <- get.GDD(temp, as.integer(cycle_m[3,5]))
  
  # Calculate indices per growig period based on GDD 
  per_gdd <- round(colMeans(cbind(GDD_veg,GDD_flo,GDD_gra)),0) 
  
  #the day after planting date each period began (per plot)
  per_cuts <- cbind(0, per_gdd[1], per_gdd[1] + per_gdd[2]) 
  
  tf <- bind_cols(plating_date = p, GDD = GDD, GDD_veg = GDD_veg, GDD_flo = GDD_flo, GDD_gra = GDD_gra)
  
  for(j in 1:nrow(cycle_m)){
    
    ts  <- per_gdd[j]
    dat <- as.Date(p + per_cuts[j], format = "%Y-%m-%d")
    per <- cycle_m[j,1]
    
    #calculate temperature
    #temperature for the timespan of each site
    temp <- array(NA, c(n, max(ts), 2), dimnames = list(NULL,  NULL, c(1:2) ))
    #day temperature
    temp[,,1] <- get.timespan(modis_approx[,,1], dat , ts)
    #night temperature
    temp[,,2] <- get.timespan(modis_approx[,,2], dat , ts)
    
    #get indices
    temp %<>% temp.index(. , max(ts))
    #add period within growth cycle in indices names
    names(temp) <- paste0(names(temp),"_",per)
    
    tf <- bind_cols(tf, temp)
    
  }
  
  
  #Calculate indices in cumulative way, from planting date to the end o veg period, from planting day to the end of flo period...
  for(j in 2:nrow(cycle_m)){
    ts <- sum(per_gdd[1:j])
    per <- cycle_m[j,1]
    kc  <- as.numeric(cycle_m[j,4])
    
    #calculate temperature
    #temperature for the timespan of each site
    temp <- array(NA, c(n, max(ts), 2), dimnames = list(NULL,  NULL, c(1:2) ))
    #day temperature
    temp[,,1] <- get.timespan(modis_approx[,,1], p , ts)
    #night temperature
    temp[,,2] <- get.timespan(modis_approx[,,2], p , ts)
    
    #get indices
    x <- temp.index(temp, max(ts))
    #add period within growth cycle in indices names
    names(x) <- paste0(names(x),"_",per,"_acc")
    
    tf <- bind_cols(tf, x)
    
  }
  
  
  avgclim[[k]] <- tf
  
}




#Use historical data to get predictions using fitted PLT-clim model
avgpred <- array(NA, dim = c(n, length(items), length(pd)), dimnames = list(1:n, as.character(items), seq_along(pd)) )

for(i in seq_along(pd)) {
  
  clim_i <- avgclim[[i]]
  
  avgpred[ , , i] <- predict(tree, newdata = clim_i)
  
}



