# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required

# load packages
library("ClimMobTools")
library("sf")
library("gosset")
library("janitor")
library("magrittr")
library("tidyverse")
library("reshape2")
library("readxl")
library("geodata")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

genotype_info = read_xlsx("data/groundnut-genotype-release-information.xlsx")

ecoregions = read.csv("data/TZ-regions-ecoregions.csv")

# # read file with API key, this is a txt file 
# # indicating the key(s) and the server from which the data should 
# # be retrieved. Its a matrix with two coluns, the first being the key and the second 
# # being the server
# key = read.table("token/api-key.txt", sep = ",")
# key = data.frame(key = key[,1], server = key[, 2])
# 
# # get the list of projects from the user indicated in the API key
# projects = data.frame()
# 
# for(i in seq_along(key$key)) {
#   projects = rbind(projects, 
#                    getProjectsCM(key$key[i], server = key$server[i]))
# }
# 
# 
# # select by userowner
# userowner = c('Happy')
# 
# 
# projects = projects[projects$user_owner %in% userowner, ]
# 
# projects = projects[grepl("groundnut", tolower(projects$project_name)), ]
# 
# # make a data.frame with project name, userowner, API key and server
# # but not save it, to keep the API key confidential
# serverdata = projects[,c("project_id", "user_owner", "server")]
# serverdata = merge(serverdata, key, by = "server")
# 
# # fetch the data from climmob
# cmdata = list()
# 
# for(i in seq_along(projects$project_id)) {
#   
#   d_i = getDataCM(serverdata$key[i], 
#                   project = serverdata$project_id[i],
#                   userowner = serverdata$user_owner[i],
#                   server = serverdata$server[i],
#                   as.data.frame = FALSE)
#   
#   cmdata[[i]] = d_i
#   
# }
# 
# 
# save(cmdata, file = "data/groundnut-data-tanzania.rda")

load("data/groundnut-data-tanzania.rda")

# put the data into data.frame format
dat = lapply(cmdata, function(x){
  x = as.data.frame(x, 
                    tidynames = TRUE,
                    pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  
  x
  
})

keep = unlist(lapply(dat, nrow)) > 0

dat = dat[keep]

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

# for the summaries here, I only use data from package distribution and participant registration
uniquenames = uniquenames[grep("id$|package_|registration_|_longitude|_latitude", uniquenames)]

uniquenames

# fix some codes that diverged over time
dat = lapply(dat, function(x){
  names(x)[names(x) == "registration_gender1"] = "registration_gender"
  names(x)[names(x) == "registration_biogender"] = "registration_gender"
  names(x)[names(x) == "registration_bioage"] = "registration_age"
  names(x)[names(x) == "registration_kebele"] = "registration_village"
  names(x)[names(x) == "registration_location"] = "registration_village"
  names(x) = gsub("overallchar2", "overall", names(x))
  x
})

# put the data together 
dat = rowbind(dat)

# fix trial coordinator name
sort(unique(dat$package_coordinator))

dat$package_coordinator[dat$package_coordinator == "HAPPY DAUDI"] = "Happy Daudi"

# countries
unique(dat$package_country)

#write.csv(dat, "data/groundnut-data.csv", row.names = F)

# ..................................
# get summaries ####
traits = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(traits, function(x){
  x$trait_label
}))

traits

traits = gsub("_qst_|_|tricot", " ", traits)
traits = ClimMobTools:::.title_case(traits)
traits = gsub("Podyield", "Pod yield", traits)
traits = gsub("Haulmyield", "Haulm yield", traits)
traits = gsub("yield", "Yield", traits)
traits = gsub("Grainsize", "Grain Size", traits)
traits = gsub("Graincolour", "Grain Color", traits)
traits = gsub("content", " Content", traits)
traits = gsub("resistance", " Resistance", traits)
traits = gsub("tolerance", " Tolerance", traits)
traits = gsub("labour", " Labor", traits)
traits = gsub("Physiologicalmaturity ", "", traits)
traits = gsub("Agronomicperformance ", "", traits)
traits = gsub("Reproductive Diseases Resistance", "Diseases Resistance [Reproductive]", traits)
traits = gsub("Reproductive Vigor", "Vigor [Reproductive]", traits)
traits = gsub(" Overall", "", traits)
traits = gsub("Vegetative ", "", traits)
traits = gsub("Reproductive ", "", traits)
traits

out = !traits %in% c("Vegetative", "Reproductive", "Flowering")

traitlist = getTraitList(dat, c("_pos", "_neg"), trait.labels = traits)

traits = traits[out]

traitlist = traitlist[out]

pack_index = paste0("package_item_", letters[1:3])

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames

dat[pack_index] = lapply(dat[pack_index], function(x){
  x[x=="Mnanje 2009 (check)"] = "Mnanje 09"
  x[x=="Mangaka 2009 (check)"] = "Mangaka 2009"
  x[x=="NALX -22-18"] = "NALX-22-18"
  x
})

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames

for (i in seq_along(genotype_info$genotype)) {
  
  dat[pack_index] = lapply(dat[pack_index], function(x){
    x[ x == genotype_info$genotype[i] ] = genotype_info$final_name[i]
    x
  })
  
  
}

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames


# PlackettLuce rankings
traits

reference = which(traits == "Overall")

dat$gender = ifelse(dat$registration_gender == "Prefer Not To Respond", NA, dat$registration_gender)

dat = dat[!is.na(dat$gender), ]

table(dat$gender)


# Coordinates ####
# get the coordinates as an independent data.frame
rownames(dat) = 1:nrow(dat)

keep = grepl("_longitude|_latitude", names(dat))

lonlat = dat[, keep]

names(lonlat)

dat = dat[, !keep]

lon = grep("_longitude", names(lonlat))
lon = lonlat[, lon]

lon = as.vector(apply(lon, 1, function(x){
  # I'll take the reverse as this increases the likelihood of
  # getting the coordinates from the trial, not the point of
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

lon[is.na(lon)] = "registration_pointofdelivery_longitude"

lat = gsub("_longitude", "_latitude", lon)

table(lon)
table(lat)

# keep only the selected columns, one per plot
lonlat = data.frame(longitude = lonlat[cbind(1:length(lon), lon)],
                    latitude = lonlat[cbind(1:length(lat), lat)])


lonlat[1:2] = lapply(lonlat[1:2], as.numeric)

plot(lonlat)

# latitude higher than 23 is wrong
lonlat$longitude[lonlat$latitude > 23] = NA
lonlat$latitude[lonlat$latitude > 23] = NA

# also lonlat with 0, 0
lonlat$longitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA
lonlat$latitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA

plot(lonlat)

trial_map = plot_map(lonlat, c("longitude", "latitude"),
                     map_provider = "OpenStreetMap.Mapnik")

trial_map

mapview::mapshot(trial_map, 
                 file = "output/trial-map.pdf")


adm = gadm("Tanzania", level = 1, path = "data/")

plot(adm)

adm_info = extract(adm, lonlat)
adm_info$ecoregion = NA

adm_info = adm_info[,c("NAME_1", "ecoregion")]

for (i in seq_along(ecoregions$Region)) {
  adm_info$ecoregion = ifelse(adm_info$NAME_1 == ecoregions$Region[i],
                              ecoregions$Zone[i],
                              adm_info$ecoregion)
}

names(adm_info)[1] = "adm1_name"

dat = cbind(adm_info, dat)

dat$id = paste(dat$package_project_name, dat$id, sep = "-")

dat = dat[union("id", names(dat))]

write.csv(dat, "data/groundnut-tanzania-tricot.csv", row.names = FALSE)

