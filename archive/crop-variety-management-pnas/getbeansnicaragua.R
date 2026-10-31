####################################################
###### Read and clean red beans data in Nicaragua
#Kaue de Sousa
#<k.desousa(at)cgiar.org>
#Bioversity International - Costa Rica
#First run 01Sep2016
#Updated 03May2018

library(Matrix)
library(tidyverse)
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

#set paraters for spatial analysis
my_proj <- "+proj=longlat +datum=WGS84"
my_ext <- raster::extent(-87, -83, 12, 15)
modis_factor <- 0.02
k_to_c <- 273.15

# Debug data and get environmental data #####
cat("## Readind raw data. Time:", date(), "\n")
#get the names of climmob projects
input_projects <- "./input/NIC/raw/projects/"
projects_names <- gsub(paste(input_projects,"/",sep=""), "", list.dirs(input_projects)[2:length(list.dirs(input_projects))])
projects_names <- projects_names[grepl("nic",projects_names)]
#run over the projects to create the paircomp matrices for bt analysis
mydata <- NULL
for (i in 1:length(projects_names)){
  #define some parameters for the loop
  index_lat <- "localizacion_latitud.Â" #pattern in the column name with lat info
  index_lon <- "localizacion_longitud.Â" #pattern in the column name with lon info
  index_date <- "fecha_de_siembra" ##pattern in the column name with planting date info
  index_char <- "Desempeno_general-(A/B/C)"
  index_char2 <- "Desempeno_general_VS_Local-(Mejor/Peor)"
  index_yield <- "Rendimiento-(A/B/C)"
  index_village <- "pueblo"
  index_district <- "Distrito"
  index_gender <- "genero.M.F."
  index_age <- "edad"
  
  #read and prepare files #
  #the file with farmers evalutation
  file <- read.csv(paste(input_projects, projects_names[ i ], "/", projects_names[ i ], ".csv",sep=""), 
                      na.strings = c("ERROR"," ","", "NA"))
  #the file with packages and varieties names 
  pack <- read.csv(paste(input_projects,  projects_names[ i ], "/", "paquete_", projects_names[ i ], ".csv",sep=""), 
                           na.strings = c("ERROR",""," ", "NA"))
  names(pack) <- c("codigo_paquetes","variety_a","variety_b","variety_c")
  
  #merge dataframes by Packages
  file <- merge(file, pack,  by="codigo_paquetes", all.x=T)
  file <- file[,c(1, ncol(file)-2,ncol(file)-1,ncol(file),2:(ncol(file)-3))]
  #remove NA's in package info
  file <- file[!is.na(file[,2]) & !is.na(file[,3]) & !is.na(file[,4]), ]
  #identify the position of each index
  #index_char
  index_char <- match( index_char, as.matrix(file[1,]) )
  #index_char2
  index_char2 <- match( index_char2, as.matrix(file[1,]) )
  #index yield
  index_yield <- match( index_yield, as.matrix(file[1,]) )
  
  #index_lon
  index_lon <- which(grepl(index_lon, as.vector(names(file))))[1] #in case that are multiple vectors for this index, take first
  #index_lat
  index_lat <- which(grepl(index_lat, as.vector(names(file))))[1] #in case that are multiple vectors for this index, take first
  #index_date
  index_date <- which(grepl(index_date, as.vector(names(file))))
  #index_village
  index_village <- which(grepl(index_village, as.vector(names(file))))
  #index_district
  index_district <- which(grepl(index_district, as.vector(names(file))))
  #index_gender
  index_gender <- match( index_gender, as.vector(names(file)) )
  #index_district
  index_age <- match( index_age, as.vector(names(file)) )
  
  #remove NA's in evalutation of index_char
  file <- file[!is.na(file[, index_char+1 ]) & !is.na(file[, index_char+2 ]),]
  file <- subset(file, file[, index_char+1]!="undefined" & file[, index_char+2]!="undefined")
  #get selected variables
  file <- file[,c(1:5, index_district,index_village,index_age, index_gender, index_date,index_lon,index_lat,
                     index_char, index_char+1, index_char+2, index_char2,index_char2+1,index_char2+2,index_char2+3, index_yield, index_yield+1, index_yield+2 )]
  names(file) <- c("package","variety_a","variety_b","variety_c","observers",
                   "district","village","age","gender","planting_date","lon","lat",
                   "characteristic","best","worst","overall_vs_local","var_a","var_b","var_c","yield","best_yield","worst_yield")
  file$project <- projects_names[i]
  #add to the large dataset
  mydata <- rbind(mydata, file)
}

n <- nrow(mydata)

#debug name of varieties
itemnames <- sort(unique(as.vector(unlist(mydata[,c("variety_a","variety_b","variety_c")]))))

var <- c("variety_a","variety_b","variety_c")
for(i in var) {
  mydata[,i] <- as.character(mydata[,i])
  mydata[,i] <- as.character(ifelse(mydata[,i]=="BRT 103- 182","BRT 103-182", 
                                              ifelse(mydata[,i]=="INTA Rojo (testigo)","INTA Rojo",
                                                     ifelse(mydata[,i]=="PM2- Don Rey", "PM2 Don Rey",
                                                            ifelse(mydata[,i]=="PM2-Don Rey","PM2 Don Rey",
                                                                   mydata[,i] )))))
}

mydata <- mutate(mydata,
                 variety_a = ifelse(variety_a == "INTA Sequia", "INTA Sequia Fuerte", variety_a),
                 variety_b = ifelse(variety_b == "INTA Sequia", "INTA Sequia Fuerte", variety_b),
                 variety_c = ifelse(variety_c == "INTA Sequia", "INTA Sequia Fuerte", variety_c))

itemnames <- sort(unique(as.vector(unlist(mydata[,c("variety_a","variety_b","variety_c")]))))



#convert values as character them as factor again
mydata[c(1,5:6)] <- lapply(mydata[c(1,5:6)] , as.character)
mydata[c(1,5:6)] <- lapply(mydata[c(1,5:6)] , as.factor)
mydata[c(11:12)] <- lapply(mydata[c(11:12)] , as.character)
mydata[c(11:12)] <- lapply(mydata[c(11:12)] , as.numeric)

#debug lat and lon using mean per village 
mydata$region <- ifelse(grepl("nic",mydata$project),"Nicaragua","Trifinio")
#some points in Nicaragua appears to be in wrong places, so I defined a threshold 
mydata$lat <- ifelse(mydata$region=="Nicaragua" & mydata$lat < 12.65| mydata$region=="Nicaragua" & mydata$lat > 13.6, NA, mydata$lat)
mydata$lon <- ifelse(mydata$region=="Nicaragua" & mydata$lon < -86.1, NA, mydata$lon)

#debug by village
sum(is.na(mydata[,c("lon","lat")]))

for(i in unique(mydata[,"region"])){
  for (j in unique(mydata[,"village"])){
    mydata[,"lon"] <- ifelse(mydata[,"region"] == i & mydata[,"village"] == j & is.na(mydata[,"lon"]),
                                       mean(mydata[,"lon"][mydata[,"region"] == i & mydata[,"village"] == j ], na.rm=T),
                                       mydata[,"lon"])
    mydata[,"lat"] <- ifelse(mydata[,"region"] == i & mydata[,"village"] == j & is.na(mydata[,"lat"]),
                                       mean(mydata[,"lat"][mydata[,"region"] == i & mydata[,"village"] == j ], na.rm=T),
                                       mydata[,"lat"])
  }
}
sum(is.na(mydata[,c("lon","lat")]))
#debug lat and lon using mean per district
summary(mydata$district)
for(i in unique(mydata[,"region"])){
  for (j in unique(mydata[,"district"])){
    mydata[,"lon"] <- ifelse(mydata[,"region"] == i & mydata[,"district"] == j & is.na(mydata[,"lon"]),
                                       mean(mydata[,"lon"][mydata[,"region"] == i & mydata[,"district"] == j ], na.rm=T),
                                       mydata[,"lon"])
    mydata[,"lat"] <- ifelse(mydata[,"region"] == i & mydata[,"district"] == j & is.na(mydata[,"lat"]),
                                       mean(mydata[,"lat"][mydata[,"region"] == i & mydata[,"district"] == j ], na.rm=T),
                                       mydata[,"lat"])
  }
}
#finally by region
for(i in unique(mydata[,"region"])){
  mydata[,"lon"] <- ifelse(mydata[,"region"]==i & is.na(mydata[,"lon"]) | 
                                       mydata[,"region"]==i & is.nan(mydata[,"lon"]),
                                     mean(mydata[,"lon"][mydata[,"region"] == i], na.rm=T),
                                     mydata[,"lon"])
  mydata[,"lat"] <- ifelse(mydata[,"region"]==i & is.na(mydata[,"lat"]) | 
                                       mydata[,"region"]==i & is.nan(mydata[,"lat"]),
                                     mean(mydata[,"lat"][mydata[,"region"] == i], na.rm=T),
                                     mydata[,"lat"])
}
sum(is.na(mydata[,c("lon","lat")]))

cat("Geographical information debugged", sum(is.na(mydata[,c("lon","lat")])), "missing information \n")

#debug planting date
sum(is.na(mydata[,"planting_date"]))
mydata$planting_date <- as.Date(mydata$planting_date, "%Y-%m-%d")
summary(factor(mydata$project))
#add variable for crop season
mydata$season <- with(mydata, as.factor(ifelse(grepl("apante_2015", project), "Ap15",
                                               ifelse(grepl("apante_2016", project), "Ap16",
                                                      ifelse(grepl("postrera_2015", project), "Po15",
                                                             ifelse(grepl("postrera_2016", project), "Po16",
                                                                    ifelse(grepl("16_primera", project), "Pr15", 
                                                                           NA)))))))
summary(factor(mydata$season))
#export distribution of original planting dates
#p <- ggplot(mydata, aes(x = project, y = planting_date) ) + geom_boxplot()
#ggsave("./processing/planting_dates_boxplot.png", plot=p, width = 32, height = 15, units = "cm", dpi = 400 )

#remove outliers in planting date
#some points are out the median by project
#so I defined a threshold of -35 and +35 from the median of each project
#return the median per project for values outside this threshold
for(i in unique(mydata$project)){
  mydata$planting_date <- ifelse(mydata$project==i & mydata$planting_date > (median(mydata$planting_date[mydata$project==i])+35) |
                                             mydata$project==i & mydata$planting_date < (median(mydata$planting_date[mydata$project==i])-35),
                                           median(mydata$planting_date[mydata$project==i]),
                                           mydata$planting_date)
}
sum(is.na(mydata$planting_date))
mydata$planting_date <-  as.Date(mydata$planting_date, origin = "1970-01-01")

#p <- ggplot(mydata, aes(x= project, y= planting_date) ) + geom_boxplot()
#ggsave("./processing/planting_dates_boxplot_cleaned.png", plot=p, width = 32, height = 15, units = "cm", dpi = 400 )
#add the year for each project
mydata$year <- as.integer(strftime(mydata$planting_date, "%Y"))

cat("Planting dates debugged", sum(is.na(mydata[,c("planting_date")])), "missing information \n")


#add modis data 
cat("Readind MODIS data. Time:", date(), "\n")
# #read modis
# dates <- NULL
# for(i in c(2003:2016))
#   dates <- c(dates, c(seq(as.Date(paste0(i, "-01-01"), "%Y-%m-%d"), as.Date(paste0(i, "-12-31"), "%Y-%m-%d"), 8)))
# dates <- as.Date(c(dates, seq(as.Date("2017-01-01", "%Y-%m-%d"), as.Date("2017-09-06", "%Y-%m-%d"), 8)), origin = "1970-01-01")
# 
# full_dates <- seq(dates[1], dates[length(dates)], 1)
# #read modis day and night
# modis <- array(NA, dim = c(n, length(dates), 2), dimnames = list(NULL, as.character(dates), c(1:2)))
# for(i in 1:2){
#   if(i == 1 ) m="day" else m="night"
#   file <- raster::stack(list.files("E:/rasters/modis/smooth_Nicaragua/", pattern = m, full.names = T))
#   raster::crs(file) <- my_proj
#   x <- raster::extract(file, mydata[,c("lon","lat")])
#   x <- x * modis_factor - k_to_c #convert to Celsius
#   x[x<1] <- NA #remove wrong data
#   modis[ , , i] <- x
# }
# sum(is.na(modis))
# #apply linear interpolation to the 8-day temperature
# modis_approx <- array(NA, c(n, length(full_dates), 2), dimnames = list(NULL,  as.character(full_dates), c(1:2) ))
# for(i in 1:dim(modis)[1]){
#   modis_approx[i,,1] <- approx(modis[i,,1], n = length(full_dates))[[2]]
#   modis_approx[i,,2] <- approx(modis[i,,2], n = length(full_dates))[[2]]
# }
# save(modis, modis_approx, file = "./processing/modis_nicaragua.RData")
load("./processing/modis_nicaragua.RData")

# # #take CHIRPS info
# cat("Readind CHIRPS data. Time:", date(), "\n")
# #source CHIRPS (precipitation data)
# #http://chg.geog.ucsb.edu/data/chirps/
# years <- seq(2003 , 2017, 1)
# chirps <- list()
# #run CHIRPS over the chosen years
# for (i in years){
#   print(i)
#   info <- raster::stack(list.files(paste0("E:/rasters/CHIRPS/", i), pattern = ".tif", full.names = T))
#   #change name and number order in dates of chirps info
#   names(info) <- gsub("chirps.v2.0.", "d", names(info))
#   chirps[paste("y", i ,sep="")] <- info
#   rm(info)
# }
# #convert list into one large raster stack
# chirps <- raster::stack(chirps)
# #chirps <- raster::crop(chirps, my_ext)
# #extract precipitation info using lat and lon of each farm
# chirps <- raster::extract(chirps, mydata[,c("lon","lat")])
# sum(is.na(chirps))
# dimnames(chirps)[[2]] <- gsub("d", "", dimnames(chirps)[[2]])
# dimnames(chirps)[[2]] <- gsub("[.]", "-", dimnames(chirps)[[2]])
# save(chirps, file = "./processing/chirps_nicaragua.RData")
load("./processing/chirps_nicaragua.RData")


#organise data
names(mydata)

#translate rankings to English
mydata <- mutate(mydata,
                 var_a = ifelse(var_a == "Peor", "Worse", "Better"),
                 var_b = ifelse(var_b == "Peor", "Worse", "Better"),
                 var_c = ifelse(var_c == "Peor", "Worse", "Better"))

#same variables in all data sets, NIC, IND, ETH
mydata$crop <- "commonbean"
mydata$characteristic <- "Overall_performance-(A/B/C)"
mydata$overall_vs_local <- "Overall_performance_VS_Local-(Best/Worse)"
mydata$yield <- "Yield-(A/B/C)"
mydata$country <- "NIC"
mydata$variety_d <- NA
mydata$rank_variety_a <- NA
mydata$rank_variety_b <- NA
mydata$rank_variety_c <- NA
mydata$rank_variety_d <- NA

#plot(mydata$lon, mydata$lat)

# #standardise lat lon for open datasets (Dataverse, Github)
# for (j in unique(mydata[,"village"])){
#     mydata[,"lon"] <- ifelse(mydata[,"village"] == j,
#                              mean(mydata[,"lon"][mydata[,"village"] == j ], na.rm=T),
#                              mydata[,"lon"])
#     mydata[,"lat"] <- ifelse(mydata[,"village"] == j,
#                              mean(mydata[,"lat"][mydata[,"village"] == j ], na.rm=T),
#                              mydata[,"lat"])
# }
# 
# mydata[,c("lon","lat")] <- lapply(mydata[,c("lon","lat")], function(X) round(X, 4))

#plot(mydata$lon, mydata$lat)

#remove some variables
mydata <- mydata[,-c(5,8,9,24)]#"observers","age", "gender","region"

mydata <- as_tibble(mydata)

rm(file, my_ext, pack, var, projects_names, my_proj, modis_factor, 
   k_to_c, i, j, itemnames, input_projects, index_age, index_char, index_char2,
   index_date, index_district, index_gender, index_lat, index_lon, index_village, index_yield)

cat("## Ready to add covariates. Time:", date(), "\n")
