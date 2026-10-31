# Script to combine data from the potato on-farm trials 
# in Rwanda
library("ClimMobTools")
library("gosset")
library("PlackettLuce")
library("jsonlite")
library("janitor")

here <- "data/raw"

ids <- list.dirs(here, full.names = TRUE)[-1]

ids <- ids[which(!grepl("station", ids))]

# read the json files with project data
lt <- list()
for(i in seq_along(ids)) {
  x <- list.files(ids[i], pattern = ".json", full.names = TRUE)[1]
  x <- fromJSON(x)
  lt[[i]] <- x
  rm(x)
}

# read the json files with project data
lonlat <- list()
for(i in seq_along(ids)) {
  x <- list.files(ids[i], pattern = ".csv", full.names = TRUE)[1]
  if (is.na(x)) next
  x <- read.csv(x)
  lonlat[[i]] <- x
  rm(x)
}


# try to fix names in data collection moments
assessments <- lapply(lt, function(x){
  data.frame(proj =  x$project$project_cod, 
             desc = x$assessments$desc, 
             code = x$assessments$code)
})

assessments <- do.call(rbind, assessments)

# make clean names which will match with those in data
assessments$cleannames <- make_clean_names(assessments$desc)

# write unified names by hand

assessments$newnames <- c("postharvest3", "vegetative2", "vegetative1",
                          "postharvest1", "postharvest2", "postharvest2_update",
                          "postharvest3", "vegetative2", "postharvest1",
                          "vegetative1", "yield_at_harvest", "vegetative1",
                          "vegetative2", "postharvest2", "postharvest1",
                          "postharvest3", "postharvest2", "vegetative2",
                          "yield_at_harvest", "vegetative1", "postharvest1",
                          "postharvest2")

assessments$names <- tolower(gsub(" ", "", assessments$desc))

assessments$names <- gsub("[)]|[(]", "_", assessments$names)

# run over data
quest <- lapply(lt, function(x){
  x$specialfields
})

quest <- do.call(rbind, quest)

for(i in seq_along(ids)) {

  x <- lt[[i]] 
  
  class(x) <- union("CM_list", class(x))
  
  x <- as.data.frame(x, pivot.wider = TRUE, tidynames = TRUE)
  
  names(x) <- gsub("[)]|[(]", "_", names(x))
  
  # run over the data collection moments to fix names
  for (j in seq_along(assessments$names)) {
    names(x) <- gsub(assessments$names[j], assessments$newnames[j], names(x))
  }
  
  names(x) <- gsub("perf_overallchar_1", "comparetolocal_a", names(x))
  names(x) <- gsub("perf_overallchar_2", "comparetolocal_b", names(x))
  names(x) <- gsub("perf_overallchar_3", "comparetolocal_c", names(x))
  names(x) <- gsub("perf_comparetolocal_1", "comparetolocal_a", names(x))
  names(x) <- gsub("perf_comparetolocal_2", "comparetolocal_b", names(x))
  names(x) <- gsub("perf_comparetolocal_3", "comparetolocal_c", names(x))
  names(x) <- gsub("perf_overal1", "comparetolocal_a", names(x))
  names(x) <- gsub("perf_overal2", "comparetolocal_b", names(x))
  names(x) <- gsub("perf_overal3", "comparetolocal_c", names(x))
  
  names(x) <- gsub("item_A_vs_local", "comparetolocal_a", names(x))
  names(x) <- gsub("item_B_vs_local", "comparetolocal_b", names(x))
  names(x) <- gsub("item_C_vs_local", "comparetolocal_c", names(x))
  
  names(x) <- gsub("bacterialwilt21b", "bacterialwilt", names(x))
  names(x) <- gsub("diseaseinsect21b", "diseaseinsect", names(x))
  names(x) <- gsub("overallperf", "overallper", names(x))
  names(x) <- gsub("potato21b_", "", names(x))
  
  lt[[i]] <- x
  
  rm(x)
  
}

lapply(lt, names)

quest$code = unlist(lapply(strsplit(quest$name, "_"), function(x) x[1]))
quest$code = gsub("ASS", "", quest$code)

assessments = merge(quest, assessments, by = "code", all.x = TRUE)

write.csv(assessments, "output/assessments.csv", row.names = FALSE)

cmdata <- rowbind(lt)

names(cmdata)

# check if any longitude colunm has data 
lon <- which(grepl("longitude", names(cmdata)))

all(unlist(lapply(cmdata[lon], function(x) sum(is.na(x)))) == nrow(cmdata))

# remove some columns from the data set
keep <- which(!grepl("participant_name|deviceimei|form_id_string|
                     longitude|longitude|latitude|elevation|_precision|
                     _active|_active|_clc_after|_instancename|_surname|_consent|
                     registration_name|registration_name|_enumerator|_surveyid|_submitted_by|
                     _alttelno|telno",
                     names(cmdata)))

cmdata <- cmdata[, keep]

names(cmdata)

# organize by order of events from registration to last assessment
unique(assessments$newnames)
index <- which(grepl("registration", names(cmdata)))
index <- c(index, which(grepl("vegetative1", names(cmdata))))
index <- c(index, which(grepl("vegetative2", names(cmdata))))
index <- c(index, which(grepl("yield_at_harvest", names(cmdata))))
index <- c(index, which(grepl("postharvest1", names(cmdata))))
index <- c(index, which(grepl("postharvest2", names(cmdata))))
index <- c(index, which(grepl("postharvest2_update", names(cmdata))))
index <- c(index, which(grepl("postharvest3", names(cmdata))))
index <- c(1:5, index)
index <- unique(index)

cmdata <- cmdata[,index]

names(cmdata)

cmdata$id <- paste(cmdata$package_project_name, cmdata$id, sep = "-")


# ........................
# add the longitude and latitude from Rhomis survey
names(lonlat[[1]])
coords1 <- lonlat[[1]][,c("location.geopoint_longitude", "location.geopoint_latitude", "Package_no")]
names(coords1) <- c("longitude", "latitude", "id")
ids[[1]]
coords1$id
i <- strsplit(coords1$id, ":")
i <- do.call(rbind, i)
i <- strsplit(i[,2], "-")
i <- do.call(rbind, i)
coords1$id <- paste("21AIP", i[,1],sep = "-")

names(lonlat[[2]])
names(lonlat[[2]]) <- make_clean_names(names(lonlat[[2]]))
names(lonlat[[2]])
coords2 <- lonlat[[2]][, c("farm_geolocation", "package_code")]
coords2 <- na.omit(coords2)
i <- strsplit(coords2$farm_geolocation, " ")
i <- do.call(rbind, i)
coords2 <- data.frame(longitude = i[,2], latitude = i[,1], id = coords2$package_code)
ids
coords2$id <- paste("21BIP", coords2$id,sep = "-")

coords3 <- lonlat[[3]][,c("X_longitude", "X_latitude", "householdid")]
names(coords3) <- c("longitude", "latitude", "id")
ids[[3]]
coords3$id <- paste("Pot21A", coords3$id,sep = "-")

coords4 <- lonlat[[4]][,c("X_longitude", "X_latitude", "SECTION_META.householdID")]
names(coords4) <- c("longitude", "latitude", "id")
ids[[4]]
coords4$id <- paste("POT21B", coords4$id,sep = "-")

coords <- rbind(coords1, coords2,coords3, coords4)

str(coords)

coords[c(1,2)] <- lapply(coords[c(1:2)], as.numeric)

# remove geo identity but keep agroecological structure
?rmGeoIndentity
coords[,c(1,2)] <- rmGeoIndentity(coords[,c(1,2)])

# add to the main data set
cmdata <- merge(coords, cmdata, by = "id", all.y = TRUE)

sum(is.na(cmdata$longitude))
sum(is.na(cmdata$longitude))

# input lat and lon using closest value to the median of z within the adm3 
cmdata$registration_sector <- ClimMobTools:::.title_case(cmdata$registration_sector)

cmdata$registration_sector <-  gsub(" |-","", cmdata$registration_sector)

cmdata$registration_sector

p <- na.omit(unique(cmdata$registration_sector))

cmdata$z <- rowSums(cmdata[, c("longitude","latitude")])

for (i in seq_along(p)){
  # data frame with coordinates and z 
  xy <- cmdata[cmdata$registration_sector == p[i] & !is.na(cmdata$registration_sector),
               c("longitude","latitude","z")]
  
  #vector with z minus the mean of z
  z <- xy$z - mean(xy$z, na.rm = TRUE)
  
  #which of these values are closest to 0 (closest to the mean)
  z <- xy[which.min(abs(z)), c("longitude","latitude")]
  
  #replace NA using the closest value
  cmdata[,"longitude"] <- ifelse(cmdata[,"registration_sector"] == p[i] & is.na(cmdata[,"longitude"]),
                       z[,1], cmdata[,"longitude"])
  cmdata[,"latitude"] <- ifelse(cmdata[,"registration_sector"] == p[i] & is.na(cmdata[,"latitude"]),
                       z[,2], cmdata[,"latitude"])
  
}

sum(is.na(cmdata[,"longitude"]))

cmdata <- cmdata[,-match(c("z"), names(cmdata))]

rm(x, y, xy, z, i)

cmdata[is.na(cmdata$longitude), 1:10]


# write files
write.csv(cmdata, "data/potato-data-tricot.csv", row.names = FALSE)

# # remove duplicates in quest
questions <- quest[!duplicated(quest$desc), ]

questions

write.csv(questions, "data/questions-asked-tricot.csv", row.names = FALSE)


assessments <- assessments[!duplicated(assessments$newnames), c("desc", "newnames") ]

write.csv(assessments, "data/assessments-tricot.csv", row.names = FALSE)






