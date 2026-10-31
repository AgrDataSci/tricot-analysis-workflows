####################################################
###### SCRIPT TO CLEAN DATA FROM CLIMMOB AND ADD COVARIATES
#Kaue de Sousa <k.desousa(at)cgiar.org>
#Bioversity International - Costa Rica
#First run 01Sep2016
#Updated 24May2018
####################################################

#Load packages#
library(Matrix)
library(tidyverse)
library(magrittr)
library(broom)
library(readr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(ggmap)
library(ggplot2)
library(ggfortify)
library(survival)
library(RCurl)
library(corrplot)
library(caret)

#Load functions ClimMobTools from GitHub
#repository https://github.com/kauedesousa/ClimMobTools/
tools <- getURL("https://raw.githubusercontent.com/kauedesousa/ClimMobTools/pilot/ClimMobCovariates.R", ssl.verifypeer = FALSE)
eval(parse(text = tools))

#Matrix with estimated days within growth periods for focal crops (common bean, wheat, rice, and durum)
## cycle, in this order: vegetative, floration, grain filling c("V0","R5","R8")
## day = first day of this period after planting date
## span = duration in days of this period 
## kc = crop coefficient
## gdd = growing degree days 
# #https://www.hindawi.com/journals/ija/2016/8065985/
# #https://ndawn.ndsu.nodak.edu/help-wheat-growing-degree-days.html #https://link.springer.com/article/10.1007/BF03030690
cycle <- rbind(cbind(cycle = c("VEG","FLO","GRA"), days = c(0,0,0), span = c(0,0,0), kc = c(0.7, 1.1, 0.3), gdd = c(275,340,285),  crop = "commonbean"), 
               cbind(cycle = c("VEG","FLO","GRA"), days = c(0,0,0), span = c(0,0,0), kc = c(0.7, 1.1, 0.3), gdd = c(1255,570,250), crop = "rice"),
               cbind(cycle = c("VEG","FLO","GRA"), days = c(0,0,0), span = c(0,0,0), kc = c(0.7, 1.1, 0.3), gdd = c(1680,200,280), crop = "wheat"),
               cbind(cycle = c("VEG","FLO","GRA"), days = c(0,0,0), span = c(0,0,0), kc = c(0.7, 1.1, 0.3), gdd = c(1600,250,280), crop = "durumwheat"))


#get names of crops
crop <- sort(unique(cycle[,6]))

#drop rice
crop <- crop[-match("rice",crop)]

for(m in 2:3){#seq_along(crop)){

  cat("#######################\n#######################\n Reading:", toupper(crop[m]), " \n Time:", date(), "\n")
  
  if(crop[m] == "commonbean") source("./script/getbeansnicaragua.R")
  
  if(crop[m] == "rice") source("./script/getriceindia.R")
  
  if(crop[m] == "wheat") source("./script/getwheatindia.R")
  
  if(crop[m] == "durumwheat") source("./script/getdurumwheatethiopia.R")
  
  #Subset cycle information for crop[m]
  cycle_m <- cycle[cycle[,6]==crop[m] , ]
  
  #calculate growing degree-days
  ts <- 180
  degree <- sum(as.integer(cycle_m[,5]))
  
  #temperature
  temp <- array(NA, c(n, ts, 2), dimnames = list(NULL,  NULL, c(1:2) ))
  temp[,,1] <- get.timespan(modis_approx[,,1], mydata$planting_date, ts)
  temp[,,2] <- get.timespan(modis_approx[,,2], mydata$planting_date, ts)
  #rainfall
  precip <- get.timespan(chirps, mydata$planting_date , ts)
  
  #add gdd to the main dataset
  mydata$GDD <- get.GDD(temp, degree)
  
  #degree days in the vegetative period
  mydata$GDD_veg <- get.GDD(temp, as.integer(cycle_m[1,5]))
   
  #degree days in the reproductive period
  temp <- array(NA, c(n, ts, 2), dimnames = list(NULL,  NULL, c(1:2) ))
  temp[,,1] <- get.timespan(modis_approx[,,1], (mydata$planting_date + mydata$GDD_veg), ts)
  temp[,,2] <- get.timespan(modis_approx[,,2], (mydata$planting_date + mydata$GDD_veg), ts)
  mydata$GDD_flo <- get.GDD(temp, as.integer(cycle_m[2,5]))
  
  #degree days in the grain filling period
  temp <- array(NA, c(n, ts, 2), dimnames = list(NULL,  NULL, c(1:2) ))
  temp[,,1] <- get.timespan(modis_approx[,,1], (mydata$planting_date + mydata$GDD_veg + mydata$GDD_flo), 60)
  temp[,,2] <- get.timespan(modis_approx[,,2], (mydata$planting_date + mydata$GDD_veg + mydata$GDD_flo), 60)
  mydata$GDD_gra <- get.GDD(temp, as.integer(cycle_m[3,5]))
  
  # Calculate indices per growig period based on GDD 
  per_gdd <- as.matrix(mydata[c("GDD_veg","GDD_flo","GDD_gra")]) 
  
  #periods averaged by season 
  for(j in unique(mydata$season)){
    
    per_gdd[,1] <-  ifelse( mydata$season == j, round(mean(per_gdd[,1][mydata$season == j]),0) ,per_gdd[,1])
    per_gdd[,2] <-  ifelse( mydata$season == j, round(mean(per_gdd[,2][mydata$season == j]),0) ,per_gdd[,2])
    per_gdd[,3] <-  ifelse( mydata$season == j, round(mean(per_gdd[,3][mydata$season == j]),0) ,per_gdd[,3])
    
  }
  
  if(crop[m] != "commonbean") per_gdd <- as.matrix(mydata[c("GDD_veg","GDD_flo","GDD_gra")])
  
  #the day after planting date each period began (per plot)
  per_cuts <- cbind(0, per_gdd[,1], per_gdd[,1] + per_gdd[,2]) 
  
  for(j in 1:nrow(cycle_m)){
    cat("Calculating indices, based on GDD", cycle_m[j,1], "\n")
    ts  <- per_gdd[,j]
    dat <- as.Date(mydata$planting_date + per_cuts[,j], format = "%Y-%m-%d")
    per <- cycle_m[j,1]
    kc  <- as.numeric(cycle_m[j,4])
    
    #calculate temperature
    #temperature for the timespan of each site
    temp <- array(NA, c(n, max(ts), 2), dimnames = list(NULL,  NULL, c(1:2) ))
    #day temperature
    temp[,,1] <- get.timespan(modis_approx[,,1], dat , ts)
    #night temperature
    temp[,,2] <- get.timespan(modis_approx[,,2], dat , ts)
    
    #get indices
    x <- temp.index(temp, max(ts))
    #add period within growth cycle in indices names
    names(x) <- paste0(names(x),"_",per)
    
    #get precipitation for the timespan
    precip <- get.timespan(chirps, dat , ts)
    
    #get indices
    y <- rainfall.index(precip, max(ts) )
    
    #add period in indices names
    names(y) <- paste0(names(y),"_",per)
    
    mydata <- bind_cols(mydata, x, y)
    
  }
  
  #Calculate indices in cumulative way, from planting date to the end o veg period, from planting day to the end of flo period...
  for(j in 2:nrow(cycle_m)){
    cat("Calculating indices, estimated period cumulative", cycle_m[j,1], "\n")
    ts <- rowSums(per_gdd[,1:j]) #sum(per_gdd[,1:j]) 
    per <- cycle_m[j,1]
    kc  <- as.numeric(cycle_m[j,4])

    #calculate temperature
    #temperature for the timespan of each site
    temp <- array(NA, c(n, max(ts), 2), dimnames = list(NULL,  NULL, c(1:2) ))
    #day temperature
    temp[,,1] <- get.timespan(modis_approx[,,1], mydata$planting_date , ts)
    #night temperature
    temp[,,2] <- get.timespan(modis_approx[,,2], mydata$planting_date , ts)

    #get indices
    x <- temp.index(temp, max(ts))
    #add period within growth cycle in indices names
    names(x) <- paste0(names(x),"_",per,"_acc")

    #get precipitation for the timespan
    precip <- get.timespan(chirps, mydata$planting_date , ts)

    #get indices
    y <- rainfall.index(precip, max(ts) )

    #add quarter in indices names
    names(y) <- paste0(names(y),"_",per,"_acc")

    mydata <- bind_cols(mydata, x, y)

  }

  #Read soil database
  #source Harmonized World Soil Database v 1.2
  #http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HTML/
  hwsd <- raster::stack("E:/rasters/HWSD/hwsd.bil")
  hwsd_data <- read.csv("E:/rasters/HWSD/HWSD_DATA.csv")
  #extract soil info 
  hwsd %<>% 
    raster::extract(., mydata[,c("lon", "lat")]) %>%
    as.data.frame() %>%
    mutate(MU_GLOBAL = ifelse(is.na(.$hwsd) | .$hwsd==0, hwsd[1,1], .$hwsd))
  
  mydata <- bind_cols(mydata, MU_GLOBAL = hwsd$MU_GLOBAL)
  
  mydata <- inner_join(mydata, hwsd_data[,c("MU_GLOBAL","SOIL_TYPE")], by="MU_GLOBAL")
  
  colnames(mydata)[colnames(mydata)=="SOIL_TYPE"] <- "soil"
  
  mydata$soil <- as.factor(as.character(mydata$soil))
  
  mydata %<>% 
    dplyr::select(. , -starts_with("MU_GLOBAL"))
  
  
  #Predict past plating dates 
  
  source("./script/PlantingDates.R")
  
  #Get historical indices based on these predictions
  climatology <- list()
  
  cat("Get indices over historical data\n")

  pb <- txtProgressBar(min = 1, max = length(years), style = 3)
  
  for(i in seq_along(years)){

    pd <- pdates[[i]]
    
    indices <- list()
    
    #run over quantiles dates
    for(k in seq_along(pd[1,])){

      #take planting dates
      p <- as.Date(as.vector(pd[ , k]), formart="%Y-%m-%d", origin="1970-01-01")
      
      #calculate growing degree-days
      ts <- 180
      degree <- sum(as.integer(cycle_m[,5]))
      
      #temperature
      temp <- array(NA, c(n, ts, 2), dimnames = list(NULL,  NULL, c(1:2) ))
      temp[,,1] <- get.timespan(modis_approx[,,1], p, ts)
      temp[,,2] <- get.timespan(modis_approx[,,2], p, ts)
      #rainfall
      precip <- get.timespan(chirps, p , ts)
      
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

        #get precipitation for the timespan
        precip <- get.timespan(chirps, dat , ts)

        #get indices
        precip %<>% rainfall.index(. , max(ts) )

        #add period in indices names
        names(precip) <- paste0(names(precip),"_",per)

        tf <- bind_cols(tf, temp, precip)

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

        #get precipitation for the timespan
        precip <- get.timespan(chirps, p , ts)

        #get indices
        y <- rainfall.index(precip, max(ts) )

        #calculate evapotranspiration and water balance
        y$ETo <- get.ETo(temp, Kc = kc)
        y$WB <- y$Rtotal - (y$ETo * ts)

        #add quarter in indices names
        names(y) <- paste0(names(y),"_",per,"_acc")

        tf <- bind_cols(tf, x, y)

      }

      
      indices[[k]] <- tf

    }

    climatology[[i]] <- indices
    
    setTxtProgressBar(pb, i)

  }

  close(pb)
  
  #Add new spatial explanatory variables 
  mydata %<>%
    mutate(xy = .$lon + .$lat,
           yx = .$lon - .$lat,
           planting_day = as.integer(format(.$planting_date,'%j')),
           planting_date = as.integer(.$planting_date))
  
  #Export data 
  rdata <- paste0("./input/",unique(mydata$country),"/",crop[m],"_climatology.RData")
  save(pdates, climatology, fit, file = rdata)
  csv <- paste0("./input/",unique(mydata$country),"/",crop[m],"_data.csv")
  write_csv(mydata, csv)

}

