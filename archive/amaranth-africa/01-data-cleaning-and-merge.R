# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")
library("dplyr")
library("readr")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")


# read file with API key,
key = "."
server = "1000farms"

replace_codes = read.csv("data/replacement-pattern-names.csv")

# get the list of projects from the user indicated in the API key
projects = getProjectsCM(key,
                         server = server)

# select by userowner
userowner = c("sognigbendanikou", "AbdulShango")

projects = projects[projects$user_owner %in% userowner, ]

# make a data.frame with project name, userowner, API key and server
# but not save it, to keep the API key confidential
serverdata = projects[,c("project_id", "user_owner", "server")]

# fetch the data from climmob
cmdata = list()

for(i in seq_along(projects$project_id)) {

  d_i = getDataCM(key,
                  project = serverdata$project_id[i],
                  userowner = serverdata$user_owner[i],
                  server = serverdata$server[i],
                  as.data.frame = FALSE)

  cmdata[[i]] = d_i

}


# put the data into data.frame format
dat = lapply(cmdata, function(x){
  x = as.data.frame(x, 
                    tidynames = TRUE,
                    pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  
  x
  
})

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

uniquenames

# fix some codes that diverged over time
# write more code to fix if needed
for (i in seq_along(replace_codes$name)) {
  dat = lapply(dat, function(x){
    names(x) = gsub(replace_codes$name[i], replace_codes$newname[i], names(x))
    x
  })
}



# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

sort(uniquenames)

# put the data together 
dat = rowbind(dat)

dat

pack_index = paste0("package_item_", letters[1:3])

dat[pack_index] = lapply(dat[pack_index], function(x){
  x = gsub("Local Benin", "Local", x)
  x = gsub("Local check [(]AC-NL[)]", "AC-NL", x)
  x = gsub("Local Mali", "Local", x)
  x
})


# check list of varieties tested
items_available = data.frame(items = unlist(dat[pack_index]),
                             country = rep(dat$package_country, times = 3))


items_available = table(items_available$items, items_available$country)

dir.create("data/summaries/", showWarnings = FALSE)

write.csv(items_available, "data/summaries/varieties-tested-country.csv")

# check availability of traits
trait_list = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(trait_list, function(x){
  x$trait_label
}))

sort(traits)

table(dat$package_country)

trait_available = lapply(trait_list, function(x){
  z = data.frame(N = x$keep, country = dat$package_country)
  table(z$N, z$country)[2,]
})

trait_available = do.call("rbind", trait_available)

trait_available = cbind(trait_available, N = rowSums(trait_available))

trait_available = as.data.frame(trait_available)

trait_available$trait = traits

#write.csv(trait_available, "data/summaries/traits-available.csv")

# Coordinates ####
# get the coordinates as an independent data.frame
keep = grepl("_longitude|_latitude", names(dat))

lonlat = dat[, keep]

dat = dat[, !keep]

lon = grep("_longitude", names(lonlat))
lon = lonlat[, lon]

lon = as.vector(apply(lon, 1, function(x){
  # I'll take the reverse as this increases the likelihood of
  # getting the coordinates from the trial, not the point of
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

lon[is.na(lon)] = "registration_longitude"

lat = gsub("_longitude", "_latitude", lon)

table(lon)
table(lat)

# keep only the selected columns, one per plot
lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)],
                    latitude = lonlat[cbind(1:nrow(lonlat), lat)])


lonlat[1:2] = lapply(lonlat[1:2], as.numeric)

plot(lonlat)

plot_map(lonlat, c("longitude", "latitude"),
         map_provider = "OpenStreetMap.Mapnik")


dat = cbind(lonlat, dat)


keep = !grepl("gps_precision|elevation", names(dat))

dat = dat[keep]

# check availability of farmers covariates
covar = grep("registration", names(dat))

covar = lapply(dat[covar], function(x){
  z = data.frame(N = is.na(x),
                 country = dat$package_country)
  table(z$N, z$country)[1,]
}) 

covar = do.call("rbind", covar)

write.csv(covar, "data/summaries/covar-available.csv")

# save dat as csv file. 
#write_excel_csv(dat, file = "data/amaranth-tricot-data.csv")

