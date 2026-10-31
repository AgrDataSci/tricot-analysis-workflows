####################################################
###### Transform PLT winning probabilites into recommendation maps
#Kauê de Sousa
#<k.desousa(at)cgiar.org>
#Bioversity International - Costa Rica
#First run 01Sep2017
#Updated 10July2018
####################################################
############################################################################
#Load packages ##
library("here")
library("readr")
library("tidyverse")
library("magrittr")
library("PlackettLuce")
library("gstat")
library("raster")
library("rgeos")
library("dismo")
library("alphahull")



#read data
df <- read_csv(here("input/tricot_data.csv"), na = c("NA",""))

#Crop that will be analysed 
crop <-  sort(unique(df$crop))

#Set parameters for geoinfo 
myproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

for(m in seq_along(crop)){
  
  #Create folder for outputs
  output <- "./output/"
  dir.create(output, showWarnings = FALSE)
  
  #Output for the crop
  output <- paste0(output,crop[m],"_",unique(df$country[df$crop==crop[m]]),"/")
  dir.create(output, showWarnings = FALSE)
  
  #Load model input
  load(here(output,"pltree_fit.RData"))
  
  #Load winning probabilities
  load(here(output,"average_predictions.RData"))
  
  #Sum winning probabilities cols and rows
  probs <- apply(avgpred, c(1,2), sum)
  
  #Get item names
  items <- dimnames(probs)[[2]]
  
  #Number of observations 
  n <- nrow(probs)
  
  #Get coordinates in a separated object
  coord <- mydata[,c("lon","lat")]
  names(coord) <- c("x","y")
  
  #Make a convex hull using trials coordinates
  #If dataset is larger than 5000 observations
  #take a sample to avoid problems in RAM memory
  if(nrow(coord) > 5000){
    #calculate largest distance between coordinates
    largedist <- coord[ sample(row.names(coord), 4000) ,] %>%
      raster::pointDistance(., longlat = FALSE) %>%
      max(., na.rm = TRUE)
    
  }else{
    #calculate largest distance between coordinates
    largedist <- coord %>%
      raster::pointDistance(., longlat = FALSE) %>%
      max(., na.rm = TRUE)
    
  }
  
  
  hull <- convHull(coord, lonlat=TRUE)
  
  hull <- gBuffer(hull@polygons, width=0.1*largedist)
  
  #set projection
  crs(hull) <- myproj
  #define extention for interpolation
  myext <- extent(hull)
  
  #Get the best two varieties per observation
  best <- 
    probs %>% 
    multiply_by(. , -1) %>%
    split( . , as.integer(row.names(.))) %>%
    lapply( . , rank) %>% 
    lapply( . , function(X) X < 3) %>%
    lapply( . , function(X) items[X]) %>%
    lapply( . , function(X) paste(X, collapse = ", ")) %>%
    bind_rows(.) %>%
    t(.)
  
  #Add best to the main dataset
  coord$best <- best
  
  #Define raster
  r <- raster(hull)
  #Set the resolution of the cells to 2-5 minutes
  if(crop[m]=="durumwheat"){
    res(r) <- c(0.04166667, 0.04166667)
  }else{
    res(r) <- c(0.004166667, 0.004166667)

  }
  #Remove points within the same grid
  frq <- as.vector(summary(factor(coord$best)))/n
  frq <- frq > 0.10
  frq <- as.vector(names(summary(factor(coord$best))))[frq]

  s <- coord[ coord$best %in% frq , ]

  #Sample
  s %<>%
    dplyr::select(., x,y) %>%
    as.matrix() %>%
    as.data.frame() %>%
    dismo::gridSample(., r, n=1) %>%
    row.names(.) %>%
    as.integer(.)

  coord <- rbind(coord[s,], coord[ !coord$best %in% frq , ])

  #summary(as.factor(coord$best))
  noptions <- as.vector(summary(factor(coord$best)))
  options <- as.vector(names(summary(factor(coord$best))))
  
  #Replace factors in options per numeric values for interpolation
  for(i in seq_along(options)){
    coord$best <-  ifelse(coord$best==options[i], 
                          round(runif(noptions[i], i*10, (i*10)+9 ),0) , coord$best)
  }
  
  coord$best <- as.integer(coord$best)
  
  sp::coordinates(coord) <- ~x+y
  sp::proj4string(coord) <- CRS(myproj)
  
  #Define the grid extent 
  x_range <- c(myext[1],myext[2])
  y_range <- c(myext[3],myext[4])
  
  #Create an empty grid where n is the total number of cells
  grd <- expand.grid(x = seq(x_range[1], x_range[2], by = 0.01),
                     y = seq(y_range[1], y_range[2], by = 0.01))
  
  coordinates(grd) <- ~x+y
  gridded(grd)     <- TRUE
  proj4string(grd) <- proj4string(coord)
  
  #interpolate the grid cells
  p_idw <- gstat::idw(best ~ 1, locations = coord, newdata = grd)
  #convert to raster object then clip
  r <- raster(p_idw)
  
  #Crop raster to fit the convexhull
  r %<>%
    mask(., hull) %>%
    crop(., myext)
  
  #Convert into varieties codes
  r <- as.data.frame(r, xy=TRUE)
  r <- r[!is.na(r[,3]), ]
  
  #new variable for each change code
  for(i in seq_along(options)){
    r[,3] <- ifelse(r[,3] > (i*10) & r[,3] < ((i*10) + 10) , options[i], r[,3])
  }
  
  #Create spatial points data frame
  coordinates(r) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(r) <- TRUE
  sp::proj4string(r) <- CRS(myproj)
  # coerce to raster
  r <- raster(r)
  plot(r)
  
  writeRaster(r, filename=paste(output,"./best_var_map.tif",sep=""), format="GTiff", overwrite=TRUE)

}
