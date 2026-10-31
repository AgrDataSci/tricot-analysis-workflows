
# ..................................
# This script organize the data and produce some summaries

# load packages
library("ClimMobTools")
library("PlackettLuce")
library("ggrepel")
library("gosset")
library("janitor")
library("magrittr")
library("tidyverse")
library("raster")
library("readr")
library("sf")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

key = "28abd92b-406a-4260-9614-601cdf243afe"
server = "1000FARMS"

p = getProjectsCM(key, server = server)

p = p[p$user_owner == "sognigbendanikou", ]

p

project = list()

for(i in seq_along(p$project_id)) {
  print(i)
  pg_i = getDataCM(key, 
                   project = p$project_id[i],
                   userowner = p$user_owner[i],
                   server = server,
                   pivot.wider = TRUE, 
                   tidynames = TRUE)
  
  project[[i]] = pg_i
  
}

# keep only the projects with data on it
keep = lapply(project, function(x){
  nrow(x) > 1
})

keep = unlist(keep)

sum(keep)

# add the logic vector to the data with project info
project = project[keep]

coords_trial = list()

for(i in seq_along(project)) {
  
  x = project[[i]]
  
  names(x) = make_clean_names(names(x))
  
  # Coordinates
  # get the coordinates as an independent xa.frame
  keep = grepl("_longitude|_latitude", names(x))
  
  if(length(keep) < 1) next
  
  lonlat = x[, keep]
  
  x = x[, !keep]
  
  lon = grep("_longitude", names(lonlat))
  lon = lonlat[, lon]
  
  lon = as.vector(apply(lon, 1, function(x){
    # I'll take the reverse as this increases the likelihood of 
    # getting the coordinates from the trial, not the point of 
    # delivery
    names(x)[rev(which(!is.na(x)))[1]]
  }))
  
  lon[!is.na(lon)] = lon[!is.na(lon)][1]
  
  lat = gsub("_longitude", "_latitude", lon)
  
  # keep only the selected columns, one per plot
  lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)], 
                      latitude = lonlat[cbind(1:nrow(lonlat), lat)])
  
  
  lonlat[1:2] = lapply(lonlat[1:2], as.numeric)
  
  # latitude higher than 23 is wrong
  lonlat$longitude[lonlat$latitude > 23] = NA
  lonlat$latitude[lonlat$latitude > 23] = NA
  
  # also lonlat with 0, 0
  lonlat$longitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA
  lonlat$latitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA
  
  lonlat$crop = x$package_technology
  
  coords_trial[[i]] = lonlat
  
}

coords = do.call("rbind", coords_trial)

nplots = data.frame(crop = names(table(coords$crop)),
                    n = as.vector(table(coords$crop)))

coords = merge(coords, nplots, by = "crop", all.x = TRUE)

coords$label = paste0(coords$crop, " (n = ", coords$n, ")")

table(coords$label)

# plot a map

# ADM data
# shape with adm units
adm = st_read("data/gadm/africa/africa_adm0.shp")
adm = st_as_sf(adm)
adm = adm[-c(2:3)]
adm

bbox(adm)

adm = st_crop(adm, st_bbox(c(xmin = -9.5, ymin = 4.4, 
                             xmax = 4,  ymax = 14.5)))

plot(adm)

coord = na.omit(coords)

names(coords)

ggplot() +
  geom_sf(adm$geometry,
          mapping = aes(), 
          colour = "#4d4d4d", 
          fill = "#ffffd9") +
  geom_jitter(data = coords, aes(x = longitude,
                                y = latitude, 
                                color = label,
                                shape = label),
              size = 1) +
  theme_void() +
  scale_color_manual(values = c('#e41a1c','#9970ab','#2b83ba')) +
  theme(legend.position = "right",
        #legend.direction = "horizontal",
        legend.text = element_text(size = 12,
                                   colour = "grey20"),
        panel.background = element_blank(),
        plot.margin = unit(rep(1, 4), "mm"),
        legend.title = element_blank())

ggsave(filename = "output/map-tricot-plots.pdf",
       width = 25,
       height = 25,
       units = "cm")


# ..............................
# get some Okra data ####
keep = unlist(lapply(project, function(x) unique(x$package_technology))) == "Okra"

dat = project[keep]

dat = rowbind(dat)

keep = dat$package_technology == "Okra"

dat = dat[keep,]

traits = getTrdattraits = getTraitList(dat, c("_pos", "_neg"))

keep = unlist(lapply(traits, function(x) sum(x$keep)))

keep

keep = keep > 100

traits = traits[keep]

trait_labs = unlist(lapply(traits, function(x) x$trait_label))

names(dat)

ov = which(trait_labs == "troisièmerécolte_overallcharfr")

packindex = paste0("package_item_", LETTERS[1:3])

R = lapply(traits, function(x){
  rank_tricot(items = packindex,
              input = x$string,
              data = dat,
              validate.rankings = TRUE)
})


kt = lapply(R[-ov], function(x){
  kendallTau(x,
             R[[ov]])
})

kt = do.call("rbind", kt)

kt

kt$trait = trait_labs[-ov]

keep = kt$`Pr(>|z|)`  < 0.05

keep = kt$kendallTau > 0.3 & keep

kt = kt[keep, ]

# filter traits
keep = trait_labs %in% kt$trait

trait_labs = trait_labs[keep]

traits = traits[keep]

R = R[keep]

# make trait labels
labels = trait_labs

labels = gsub("_|qst|fr$|tricot", " ", labels)

labels = gsub("récolte", " récolte", labels)

labels = gsub("  ", " ", labels)

labels = ClimMobTools:::.title_case(labels)

labels

# run PL model

mod = lapply(R, PlackettLuce)

wm = worth_map(mod, labels = labels) + 
  theme(axis.text.y = element_text(angle = 0))

wm

ggsave("output/worth-all-traits-okra.pdf",
       plot = wm,
       height = 10,
       width = 15,
       units = "cm")


# PCA
coeffs = lapply(mod, function(x){
  gosset::resample(x, bootstrap = TRUE, seed = 1424, n1 = 200)
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = labels

# add name of features
#names(winprobs) = labels

pc = princomp(coeffs[-1], cor = FALSE)

pcplot = plot_pca(pc)

pcplot

ggsave("output/biplot-performance-all-traits-okra.pdf",
       plot = pcplot,
       height = 15,
       width = 15,
       units = "cm")
  
# ..............................
# get some Jute data ####
keep = unlist(lapply(project, function(x) unique(x$package_technology))) == "Jute mallow"

dat = project[keep]

dat = rowbind(dat)

table( dat$package_technology)

keep = dat$package_technology == "Jute mallow"

dat = dat[keep,]

traits = getTrdattraits = getTraitList(dat, c("_pos", "_neg"))

keep = unlist(lapply(traits, function(x) sum(x$keep)))

keep

keep = keep > 100

traits = traits[keep]

trait_labs = unlist(lapply(traits, function(x) x$trait_label))

trait_labs

ov = which(trait_labs == "quatrièmerécolte_overallcharfr")

packindex = paste0("package_item_", LETTERS[1:3])

R = lapply(traits, function(x){
  rank_tricot(items = packindex,
              input = x$string,
              data = dat,
              validate.rankings = TRUE)
})


kt = lapply(R[-ov], function(x){
  kendallTau(x,
             R[[ov]])
})

kt = do.call("rbind", kt)

kt

kt$trait = trait_labs[-ov]

keep = kt$`Pr(>|z|)`  < 0.05

#keep = kt$kendallTau > 0.3 & keep

kt = kt[keep, ]

# filter traits
keep = trait_labs %in% kt$trait

trait_labs = trait_labs[keep]

traits = traits[keep]

R = R[keep]

# make trait labels
labels = trait_labs

labels = gsub("_|qst|fr$|tricot", " ", labels)

labels = gsub("récolte", " récolte", labels)

labels = gsub("  ", " ", labels)

labels = ClimMobTools:::.title_case(labels)

labels

# run PL model

mod = lapply(R, PlackettLuce)

wm = worth_map(mod, labels = labels) + 
  theme(axis.text.y = element_text(angle = 0))

wm

ggsave("output/worth-all-traits-jute.pdf",
       plot = wm,
       height = 10,
       width = 20,
       units = "cm")


# PCA
coeffs = lapply(mod, function(x){
  gosset::resample(x, bootstrap = TRUE, seed = 1424, n1 = 200)
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = labels

# add name of features
#names(winprobs) = labels

pc = princomp(coeffs[-1], cor = FALSE)

pcplot = plot_pca(pc)

pcplot

ggsave("output/biplot-performance-all-traits-jute.pdf",
       plot = pcplot,
       height = 15,
       width = 15,
       units = "cm")

