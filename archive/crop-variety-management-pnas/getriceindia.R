####################################################
###### Read and clean wheat in India
# INDIA
#Kaue de Sousa
#<k.desousa(at)cgiar.org>
#Bioversity International - Costa Rica
#First run 01Sep2016
#Updated 03May2018
####################################################

#Load packages#
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

#define some parameters
my_proj <- "+proj=longlat +datum=WGS84"
ext <- raster::extent(68, 86, 9, 33)
modis_factor <- 0.02
k_to_c <- 273.15

# Debug data and get some environmental information ####
cat("Readind raw data. Time:", date(), "\n")
#the folder whith raster info
#get names of projects for this region
input_projects <- "./input/projects/"
projects_names <- gsub(paste(input_projects,"/",sep=""), "", list.dirs(input_projects)[2:length(list.dirs(input_projects))])

#run over the projects to get variables
mydata <- NULL
for (i in 1:length(projects_names)){
  #define some parameters for the loop
  index_lat <- "lat" #colname with latitude info
  index_lon <- "lon" #colname with longitude info
  index_date <- "planting_date" #colname with sowing date info
  index_char <- "Overall_performance-(A/B/C)" #pattern in colunm indication the evaluatated characteristic
  index_village <- "illage" #colname with village info
  index_district <- "istrict" #colname with district info
  index_yield <- "Yield-(A/B/C)"
  
  #read and prepare files #
  #the file with reported results from Tricot trials
  file <- read.csv(paste(input_projects, projects_names[ i ], "/", projects_names[ i ], ".csv",sep=""), 
                      na.strings = c("ERROR"," ","", "NA","#REF!"))
  
  #the file with item names per package
  pack <- read.csv(paste(input_projects,  projects_names[ i ], "/", "package_", projects_names[ i ], ".csv",sep=""), 
                           na.strings = c("ERROR",""," ", "NA"))
  names(pack) <- c("code_packages","variety_a","variety_b","variety_c")
  #convert item names into upper case 
  pack[1:4] <- lapply(pack[1:4], toupper)
  pack[1:4] <- lapply(pack[1:4], as.character)
  #also remove space between names
  for (j in 1:4){
    pack[,j] <- gsub("  ", "",pack[,j])
    pack[,j] <- gsub(" ", "",pack[,j])
  }
  #same in dataframe with results
  file$code_packages <- toupper(file$code_packages)
  file$code_packages <- as.character(gsub(" ","",file$code_packages))
  
  #merge dataframes by packages codes
  file <- merge(file, pack,  by="code_packages", all.x=T)
  #reorder columns
  file <- file[,c(1, ncol(file)-2, ncol(file)-1, ncol(file), 2:(ncol(file)-3))]
  #remove NA's in item names
  file <- subset(file, !is.na(file$variety_a) & !is.na(file$variety_b) & !is.na(file$variety_c))
  
  #identify the position of each index defined in parameters
  #identify the position of each index defined in parameters
  #index_char
  index_char <- match( index_char, as.matrix(file[1,]) )
  #index_yield 
  index_yield <- match( index_yield, as.matrix(file[1,]) )
  #index_lon
  index_lon <- which(grepl(index_lon, as.vector(names(file))))[1] #in case that are multiple vectors for this index, take first
  #index_lat
  index_lat = which(grepl(index_lat, as.vector(names(file))))[1] #in case that are multiple vectors for this index, take first
  #index_date
  index_date = which(grepl(index_date, as.vector(names(file))))[1]
  #index_village
  index_village = which(grepl(index_village, as.vector(names(file))))
  #index_district
  index_district = which(grepl(index_district, as.vector(names(file))))
  #remove NA's in evalutation of index_char
  file <- subset(file, !is.na(file[, index_char+1 ]) & !is.na(file[, index_char+2 ]))
  file <- subset(file, file[, index_char+1]!="undefined" & file[, index_char+2]!="undefined")
  #get selected variables
  file <- file[,c(1:4, index_district,index_village, index_date,index_lon,index_lat,
                  index_char,index_char+1, index_char+2, index_yield, index_yield+1, index_yield+2 )]
  names(file) <- c("package","variety_a","variety_b","variety_c","district","village","planting_date","lon","lat",
                   "characteristic","best","worst","yield","best_yield","worst_yield")
  file$project <- projects_names[i]
  file$crop <- ifelse(grepl("rice", projects_names[ i ]), "rice","wheat")
  #all elements into character
  file[1:ncol(file)] <- lapply(file[1:ncol(file)], as.character)
  #add to the large dataset
  mydata <- rbind(mydata, file)
  rm(file, pack,index_lat, index_lon,index_date,index_district,index_village,index_char,index_yield)

}
#generate an ID for each entry
dim(mydata)

length(unique(paste(mydata$package, mydata$project, sep="-")))
#there are duplicated entries so I'll take the 1st
mydata$id <- paste(mydata$package, mydata$project, sep="-")
mydata <- mydata[!duplicated(mydata$id),]
n <- nrow(mydata)
#debug latitude and longitude
#lat lon as numeric 
mydata$lat <- as.numeric(mydata$lat)
mydata$lon <- as.numeric(mydata$lon)
#village and district as uppercase
mydata$village <- toupper(mydata$village)
mydata$district <- toupper(mydata$district)
# #export maps of original points
# for(i in unique(mydata$crop)) {
#   file <- subset(mydata, mydata$crop==i & !is.na(mydata$lon))
#   m <- ggmap(get_googlemap(center = c(lon= mean(file$lon) , lat= mean(file$lat)),
#                            zoom = if(i=="wheat") 2 else 3, format="png8",maptype="terrain")) +
#     geom_point(data=file, aes(x=lon,y=lat, color=crop), size=2.5, shape=4) +
#     labs(x=NULL,y=NULL) +
#     scale_color_manual(values= "red", name=NULL, labels= NULL) +
#     theme(legend.position="none")
#   
#   ggsave(paste("./processing/location_points", toupper(i), "_original.png", sep=""),
#          plot = m, dpi = 400, 
#          width = 20, height = 20 , units = "cm")
# }
#remove wrong lat lon info
mydata[,"lon"] <- ifelse(mydata[,"lon"] < 68, NA, ifelse(mydata[,"lon"] > 86, NA,mydata[,"lon"]))
mydata[,"lat"] <- ifelse(mydata[,"lat"] < 9, NA, ifelse(mydata[,"lat"] > 33, NA, mydata[,"lat"]))
sum(is.na(mydata[,c("lon","lat")]))

#debug lat and lon using mean per village 
summary(as.factor(mydata$village))
for (j in unique(mydata[,"village"])){
  mydata[,"lon"] <- ifelse(mydata[,"village"] == j & is.na(mydata[,"lon"]),
                                mean(mydata[,"lon"][mydata[,"village"] == j ], na.rm=T),
                                mydata[,"lon"])
  mydata[,"lat"] <- ifelse(mydata[,"village"] == j & is.na(mydata[,"lat"]),
                                mean(mydata[,"lat"][mydata[,"village"] == j ], na.rm=T),
                                mydata[,"lat"])
}
sum(is.na(mydata[,c("lon","lat")]))
#debug lat and lon using mean per district
summary(as.factor(mydata$district))
for (j in unique(mydata[,"district"])){
  mydata[,"lon"] <- ifelse(mydata[,"district"] == j & is.na(mydata[,"lon"]),
                                     mean(mydata[,"lon"][mydata[,"district"] == j ], na.rm=T),
                                     mydata[,"lon"])
  mydata[,"lat"] <- ifelse(mydata[,"district"] == j & is.na(mydata[,"lat"]),
                                     mean(mydata[,"lat"][mydata[,"district"] == j ], na.rm=T),
                                     mydata[,"lat"])
}

cat("Geographical information debugged", sum(is.na(mydata[,c("lon","lat")])), "missing information \n")

# #export maps of cleaned points
# for(i in unique(mydata$crop)) {
#   file <- subset(mydata, mydata$crop==i & !is.na(mydata$lon))
#   m <- ggmap(get_googlemap(center = c(lon= mean(file$lon) , lat= mean(file$lat)),
#                            zoom = 6 , format="png8",maptype="terrain")) +
#     geom_point(data=file, aes(x=lon,y=lat, color=crop), size=2.5, shape=4) +
#     labs(x=NULL,y=NULL) +
#     scale_color_manual(values= "red", name=NULL, labels= NULL) +
#     theme(legend.position="none")
#   
#   ggsave(paste("./processing/location_points", toupper(i), "_cleaned.png", sep=""),
#          plot = m, dpi = 400, 
#          width = 20, height = 20 , units = "cm")
# }

#debug planting date
sum(is.na(mydata[,"planting_date"]))
#create new vector for not compromise the original data
x <- mydata[,"planting_date"]
x <- reshape2::colsplit(x, "-", names=c("v1","v2","v3"))
x[1:ncol(x)] <- lapply(x[1:ncol(x)], as.integer)
x$date <- ifelse(x$v1 >= 2012, paste(x$v1, x$v2, x$v3, sep="-"),
                 ifelse(x$v3 >= 2012, paste(x$v3, x$v2, x$v1, sep="-"),
                        ifelse(x$v1 < 17 & x$v3 < 17, paste(as.numeric(paste0(20,x$v3)), x$v2, x$v1, sep="-"), 
                               NA)))
x$date <- as.Date(x$date, "%Y-%m-%d")
x$date <- ifelse(x$date > as.Date("2017-01-01","%Y-%m-%d"), NA, x$date)
#add to mydata
mydata$planting_date <- x$date
sum(is.na(mydata$planting_date))
mydata$planting_date <- as.Date(mydata$planting_date, origin = "1970-01-01")
#export distribution of original planting dates
# p <- ggplot(mydata, aes(x= project, y= planting_date) ) +
#   geom_boxplot() + theme(axis.text.x = element_text(angle = 75, hjust=0, vjust=0, face="plain"))
# ggsave("./processing/planting_dates_boxplot.png", plot=p, width = 30, height = 15, units = "cm", dpi = 400 )

mydata$planting_date <- as.integer(mydata$planting_date)
#remove outliers and NA's in planting date
#some points are out the median by project
#so I defined a threshold of -35 and +35 from the median of each project
#return the median per project for values outside this threshold
for(i in unique(mydata$project)){
  mydata$planting_date <- ifelse(mydata$project==i & mydata$planting_date > (median(mydata$planting_date[mydata$project==i], na.rm=T)+35) |
                                             mydata$project==i & mydata$planting_date < (median(mydata$planting_date[mydata$project==i], na.rm=T)-35),
                                           median(mydata$planting_date[mydata$project==i], na.rm=TRUE),
                                           mydata$planting_date)
  mydata$planting_date <- ifelse(mydata$project==i & is.na(mydata$planting_date), median(mydata$planting_date[mydata$project==i], na.rm=TRUE), 
                                 mydata$planting_date)
}
sum(is.na(mydata$planting_date))

#planting dates into as.Date
mydata$planting_date <- as.Date(mydata$planting_date, origin = "1970-01-01")

cat("Planting dates debugged", sum(is.na(mydata[,c("planting_date")])), "missing information \n")

#export chart with cleaned planting dates per crop
# for(i in unique(mydata$crop)){
#   png(paste0("./processing/planting_dates_",i,".png"), units = "cm", width=40, height = 25, res=200)
#   plot(as.factor(mydata$planting_date[mydata$crop== i ]), main= paste0("Planting dates ",i))
#   dev.off()
# }
#export distribution of original planting dates
# p <- ggplot(mydata, aes(x= project, y= planting_date) ) + 
#   geom_boxplot() + theme(axis.text.x = element_text(angle = 75, hjust=0, vjust=0, face="plain"))
# ggsave("./processing/planting_dates_boxplot_cleaned.png", plot=p, width = 30, height = 15, units = "cm", dpi = 400 )

#add year
mydata$year <- as.integer(strftime(mydata$planting_date, "%Y"))
#add season
mydata$season <- as.factor(ifelse(mydata$crop=="wheat", paste0("Rabi",mydata$year),  paste0("Kharif",mydata$year)))
summary(mydata$season)

# #add MODIS data
cat("Readind MODIS data. Time:", date(), "\n")
# tiles <- c("h24v06","h25v06")
# #vector with 8-day dates 
# dates <- NULL
# for(i in c(2003:2016))
#   dates <- c(dates, c(seq(as.Date(paste0(i, "-01-01"), "%Y-%m-%d"), as.Date(paste0(i, "-12-31"), "%Y-%m-%d"), 8)))
# dates <- as.Date(c(dates, seq(as.Date("2017-01-01", "%Y-%m-%d"), as.Date("2017-09-22", "%Y-%m-%d"), 8)), origin = "1970-01-01")
# #sequence of dates by day 
# full_dates <- seq(dates[1], dates[length(dates)], 1)
# #read modis day and night
# modis <- array(NA, dim = c(n, length(dates), 2), dimnames = list(c(1:n), as.character(dates), c(1:2)))
# 
# #files in this directory has different extensions, so each tile is loaded individually
# for (i in 1:2){
#   if(i == 1 ) m <- "day" else m <- "night"
#   for(j in 1:length(tiles)){
#     print(paste(m,tiles[j],sep=" "))
#     file <- raster::stack(list.files(paste0("E:/rasters/modis/smooth_India/", tiles[j]), pattern = m, full.names = T))
#     raster::crs(file) <- my_proj
#     x <- raster::extract(file, mydata[,c("lon","lat")])
#     x <- x * modis_factor - k_to_c #convert to Celsius
#     x[x<1] <- NA #remove wrong data
#     dimnames(x)[[1]] <- 1:n
#     
#     x <- x[!is.na(x[,3]), ]
#     modis[ as.integer(dimnames(x)[[1]]) , , i] <- x
#   }
# }
# rm(x)
# sum(is.na(modis))
# 
# #replace NA's in temp using the median per day
# for(i in 1:dim(modis)[2]){
#   modis[,i,1] <- ifelse(is.na(modis[,i,1]), median(modis[,i,1], na.rm = T), modis[,i,1])
#   modis[,i,2] <- ifelse(is.na(modis[,i,2]), median(modis[,i,2], na.rm = T), modis[,i,2])
# }
# #apply linear interpolation to the 8-day temperature
# modis_approx <- array(NA, c(n, length(full_dates), 2), dimnames = list(mydata$id,  as.character(full_dates), c(1:2) ))
# for(i in 1:dim(modis)[1]){
#   modis_approx[i,,1] <- approx(modis[i,,1], n = length(full_dates))[[2]]
#   modis_approx[i,,2] <- approx(modis[i,,2], n = length(full_dates))[[2]]
# }
# save(modis, modis_approx, file = "./processing/modis_india.RData")
load("./processing/modis_india.RData")

# #add CHIRPS data
cat("Readind CHIRPS data. Time:", date(), "\n")
# #source CHIRPS (precipitation data)
# #http://chg.geog.ucsb.edu/data/chirps/
# years <- years <- seq(from=2003 , to=2017, by=1)
# chirps <- list()
# #run CHIRPS over the chosen years
# for (i in years){
#   print(i)
#   info <- raster::stack(list.files(paste0("E:/rasters/CHIRPS/", i), pattern = ".tif", full.names = T))
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
# save(chirps, file = "./processing/chirps_india.RData")
load("./processing/chirps_india.RData")
#dimnames(chirps)[[1]] <- as.integer(row.names(mydata))

#Subset data and keep only wheat
keep <- mydata$crop=="rice"

chirps <- chirps[keep, ]
modis <- modis[keep,,]
modis_approx <- modis_approx[keep,,]
mydata <- mydata[keep,]

#check rankings, some of them are coded in different ways 
summary(as.factor(mydata$best))
summary(as.factor(mydata$worst))
x <- mydata[,c("best","worst")]
x[x=="v1"] <- "A"
x[x=="V1"] <- "A"
x[x=="1"] <- "A"

x[x==" v2"] <- "B"
x[x=="v2"] <- "B"
x[x=="V2"] <- "B"
x[x=="2"] <- "B"

x[x=="v3"] <- "C"
x[x=="V3"] <- "C"
x[x=="3"] <- "C"

mydata$best <- x$best
mydata$worst <- x$worst

mydata <- mydata[mydata$best!="#REF!",]

#same for yield 
summary(as.factor(mydata$best_yield))
summary(as.factor(mydata$worst_yield))
x <- mydata[,c("best_yield","worst_yield")]
x[x=="v1"] <- "A"
x[x=="V1"] <- "A"
x[x=="1"] <- "A"

x[x==" v2"] <- "B"
x[x=="v2"] <- "B"
x[x=="V2"] <- "B"
x[x=="2"] <- "B"

x[x=="v3"] <- "C"
x[x=="V3"] <- "C"
x[x=="3"] <- "C"

summary(as.factor(x$best_yield))
summary(as.factor(x$worst_yield))
#add to mydata 
mydata$best_yield <- x$best_yield
mydata$worst_yield <- x$worst_yield

n <- nrow(mydata)


#same variables in all data sets, NIC, IND, ETH
mydata$overall_vs_local <- "Overall_performance_VS_Local-(Best/Worse)"
mydata$var_a <- NA
mydata$var_b <- NA
mydata$var_c <- NA
mydata$country <- "IND"


mydata$variety_d <- NA
mydata$rank_variety_a <- NA
mydata$rank_variety_b <- NA
mydata$rank_variety_c <- NA
mydata$rank_variety_d <- NA


#standardise lat lon for open datasets (Dataverse, Github)

g <- mydata[,c("lon","lat")]

for (j in sort(unique(mydata[,"village"]))){
  mydata[,"lon"] <- ifelse(mydata[,"village"] == j ,
                           mean(mydata[,"lon"][mydata[,"village"] == j ], na.rm = TRUE),
                           mydata[,"lon"])
  mydata[,"lat"] <- ifelse(mydata[,"village"] == j ,
                           mean(mydata[,"lat"][mydata[,"village"] == j ], na.rm = TRUE),
                           mydata[,"lat"])
}

mydata$lon <- ifelse(is.na(mydata$lon), g$lon, mydata$lon)
mydata$lat <- ifelse(is.na(mydata$lat), g$lon, mydata$lat)

mydata[,c("lon","lat")] <- lapply(mydata[,c("lon","lat")], function(X) round(X, 4))


mydata <- mydata[,-18]

rm(ext, x, i, input_projects, j, k_to_c, keep, modis_factor, my_proj, projects_names, g)