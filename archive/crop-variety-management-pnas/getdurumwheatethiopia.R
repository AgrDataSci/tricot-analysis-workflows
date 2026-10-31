####################################################
###### Read and clean durum wheat in Ethiopia
#Kaue de Sousa
#<k.desousa(at)cgiar.org>
#Bioversity International - Costa Rica
#First run 01Sep2016
#Updated 03
####################################################

#set work directory
cat("Readind raw data. Time:", date(), "\n")

#read csv file
ethiopia <- read_csv("vanEtten_etal2018_replication_data/input/ETH/raw/climmob_ethiopia_2017.csv", 
                     na = c("#VALUE!", "NA", "<NA>", ""),
                     col_types = cols(GY_gm = col_number()))

# drop all missing rankings or rankings with missing Accession (item id)
ethiopia <- ethiopia %>%
  filter(!is.na(Farmer_Rank) & !is.na(Accession))

# only keep strict rankings of four distinct items
keep <- ethiopia %>%
  group_by(Farmer_No) %>%
  summarise(keep = setequal(Farmer_Rank, 1:4) & n_distinct(Accession) == 4) %>%
  filter(keep)

id <- ethiopia$Farmer_No %in% keep$Farmer_No

#take rankings
n <- nrow(keep)
itemnames <- sort(unique(ethiopia$Accession[id]))
R <- sparseMatrix(i = rep(1:n, each = 4), 
                  j = match(ethiopia$Accession[id], itemnames),
                  x = ethiopia$Farmer_Rank[id],
                  dimnames = list(keep$Farmer_No, itemnames))

#add environmental data
mydata <- as.data.frame(ethiopia[id,])

head(mydata)

#reshape in a wide format 
mydata <- reshape(mydata,                              
                  v.names = c("Accession","Farmer_Rank") , 
                  timevar=  "PlotNo", 
                  idvar= "Farmer_No",
                  direction="wide",
                  drop = c("No","Code_Region", "Code_Year","Code_Zone","Code_District","Code_Kebelle","Altitude","Code_Farmer","Sex",
                           "Code_Accession","Days2Maturity","GY_gm","Biomass_gm") )

#order columns
names(mydata)
mydata <- mydata[,c(1:5,8:9,6:7,10,11,13,15,17,12,14,16,18)]
names(mydata)

names(mydata)[8:18] <- c("lon","lat", "genotype","var_a","var_b","var_c","var_d",
                         "rank_var_a","rank_var_b","rank_var_c","rank_var_d")

#R <- cbind(Farmer_No = mydata$Farmer_No, as.matrix(R))

#planting dates 
pdate <- read_csv("./input/ETH/raw/ethiopia_planting_dates.csv")
mydata <- merge(mydata, pdate[,c("Farmer","planting_date")], by="Farmer", all.x=TRUE)
summary(as.factor(mydata$planting_date))
mydata$planting_date <- as.Date(mydata$planting_date, "%d/%m/%Y")
#plot(as.factor(mydata$planting_date))
mydata$planting_date <- as.integer(mydata$planting_date)

#fill missing planting dates with average per year
sum(is.na(mydata$planting_date))
for(i in seq_along(unique(mydata$Year))){
  y_i <- unique(mydata$Year)[i]
  mydata$planting_date <- ifelse(is.na(mydata$planting_date) & mydata$Year == y_i, 
                                 mean(mydata$planting_date[mydata$Year == y_i], na.rm=T), mydata$planting_date)
}
mydata$planting_date <- as.Date(mydata$planting_date, origin = "1970-01-01")
#plot(as.factor(mydata$planting_date))
mydata$Year <- as.integer(strftime(mydata$planting_date, "%Y"))

#lon lat 
sum(is.na(mydata[,c("lon","lat")]))
# ggmap(get_googlemap(center = c(lon= mean(mydata$lon,na.rm=T) , lat= mean(mydata$lat,na.rm=T)),
#                     zoom = 8 , format="png8",maptype="terrain")) +
#   geom_point(data=mydata, aes(x=lon,y=lat, color= District), size=2.5, shape=4) +
#   labs(x=NULL,y=NULL)

#debug lat and lon using mean per village 
summary(as.factor(mydata$Kebele))
for (j in unique(mydata[,"Kebele"])){
  mydata[,"lon"] <- ifelse(mydata[,"Kebele"] == j & is.na(mydata[,"lon"]),
                           mean(mydata[,"lon"][mydata[,"Kebele"] == j ], na.rm=T),
                           mydata[,"lon"])
  mydata[,"lat"] <- ifelse(mydata[,"Kebele"] == j & is.na(mydata[,"lat"]),
                           mean(mydata[,"lat"][mydata[,"Kebele"] == j ], na.rm=T),
                           mydata[,"lat"])
}
sum(is.na(mydata[,c("lon","lat")]))

#debug lat and lon using mean per district
summary(mydata$District)
for (j in unique(mydata[,"District"])){
  mydata[,"lon"] <- ifelse(mydata[,"District"] == j & is.na(mydata[,"lon"]),
                           mean(mydata[,"lon"][mydata[,"District"] == j ], na.rm=T),
                           mydata[,"lon"])
  mydata[,"lat"] <- ifelse(mydata[,"District"] == j & is.na(mydata[,"lat"]),
                           mean(mydata[,"lat"][mydata[,"District"] == j ], na.rm=T),
                           mydata[,"lat"])
}
sum(is.na(mydata[,c("lon","lat")]))

# ggmap(get_googlemap(center = c(lon= mean(mydata$lon) , lat= mean(mydata$lat)),
#                     zoom = 8 , format="png8",maptype="terrain")) +
#   geom_point(data=mydata, aes(x=lon,y=lat), size=2.5, shape=4) +
#   labs(x=NULL,y=NULL)


#Take number of observations
n <- nrow(mydata)
row.names(mydata) <- 1:n

#add MODIS data
#define some parameters
my_proj <- "+proj=longlat +datum=WGS84"
#ext = raster::extent(68, 86, 9, 33)
#gdd <- 1300 
#base <- 10

## add modis data
cat("Readind MODIS data. Time:", date(), "\n")
# ## read modis
# modis_factor <- 0.02
# k_to_c <- 273.15
# tiles <- c("h21v07","h21v08")
# dates <- NULL
# for(i in c(2003:2015)){
#   dates <- c(dates, c(seq(as.Date(paste0(i, "-01-01"), "%Y-%m-%d"), as.Date(paste0(i, "-12-31"), "%Y-%m-%d"), 8)))}
# dates <- as.Date(c(dates, seq(as.Date("2016-01-01", "%Y-%m-%d"), as.Date("2016-12-26", "%Y-%m-%d"), 8)), origin = "1970-01-01")
# #vector with full dates 
# full_dates <- seq(dates[1], dates[length(dates)], 1)
# #read modis day and night
# modis <- array(NA, dim = c(n, length(dates), 2), dimnames = list(NULL, as.character(dates), c(1:2)))
# #files in this directory has different extensions, so each tile is loaded individually
# for (i in 1:2){
#   if(i == 1 ) m <- "day" else m <- "night"
#   for(j in 1:length(tiles)){
#     print(paste(m,tiles[j],sep=" "))
#     file <- raster::stack(list.files(paste0("E:/rasters/modis/smooth_Ethiopia/", tiles[j]), pattern = m, full.names = T))
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
# 
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
# #save(modis, modis_approx, file = "./processing/modis_ethiopia.RData")
load("./processing/modis_ethiopia.RData")


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
# save(chirps, file = "./processing/chirps_ethiopia.RData")
load("./processing/chirps_ethiopia.RData")


n <- nrow(mydata)

#add season 
mydata$season <- paste("Meher", mydata$Year, sep="-")
mydata$season <- gsub("20","",mydata$season)
#add name of the crop 
mydata$crop <- "durumwheat"
#change name in year
names(mydata)[names(mydata)=="Year"] <- "year"

#same variables in all data sets, NIC, IND, ETH
mydata$country <- "ETH"
names(mydata)[7] <- "package"
names(mydata)[11:14] <- paste0("variety_",letters[1:4])
names(mydata)[15:18] <- paste0("rank_variety_",letters[1:4])
names(mydata)[5:6] <- c("district","village")

mydata$characteristic <- "Overall_performance-(A/B/C)"
mydata$best <- NA
mydata$worst <- NA
mydata$overall_vs_local <- "Overall_performance_VS_Local-(Best/Worse)"
mydata$var_a <- NA
mydata$var_b <- NA
mydata$var_c <- NA
mydata$yield <- "Yield-(A/B/C)"
mydata$best_yield <- NA
mydata$worst_yield <- NA
mydata$project <- paste0("durumwheat_eth_",mydata$year,"_",mydata$Region)

names(mydata)

#plot(mydata$lon, mydata$lat)

# #standardise lat lon for open datasets (Dataverse, Github)
# for (j in unique(mydata[,"village"])){
#   mydata[,"lon"] <- ifelse(mydata[,"village"] == j ,
#                            mean(mydata[,"lon"][mydata[,"village"] == j ], na.rm=T),
#                            mydata[,"lon"])
#   mydata[,"lat"] <- ifelse(mydata[,"village"] == j ,
#                            mean(mydata[,"lat"][mydata[,"village"] == j ], na.rm=T),
#                            mydata[,"lat"])
# }
# 
# mydata[,c("lon","lat")] <- lapply(mydata[,c("lon","lat")], function(X) round(X, 4))

#plot(mydata$lon, mydata$lat)

mydata <- mydata[,-c(1,3,4,10)] #Farmer, Region, Zone, genotype


rm(ethiopia, keep, R, i, id, itemnames, j, my_proj, y_i)




