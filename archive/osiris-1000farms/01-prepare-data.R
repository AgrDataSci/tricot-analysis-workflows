library("ClimMobTools")
library("gosset")
library("PlackettLuce")
library("sf")
library("geodata")
library("ggplot2")
library("ggrepel")
library("ggspatial")
library("jsonlite")
library("tidyverse")
library("magrittr")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

dir.create("output", showWarnings = FALSE)

files = as.list(list.files("data", pattern = ".json", full.names = TRUE))

dat = lapply(files, fromJSON)

#..........................................................
#..........................................................
#..........................................................
# Organize traits ####
# Assess trait rankings 
R = lapply(dat, function(x){
  r = x$plot_data
  r = r[which(r$value_type == "rank"), ]
  r = r[grep("yield|overall", r$trait), ]
  r$trait = paste0(r$collection_moment, "-", r$trait)
  r
})

lapply(R, function(x) unique(x$trait))

# retain only last traits in crop growing, yield and overall at harvest
sel = c("agronomicperformance-yield", "agronomicperformance-overall",
        "physiologicalmaturity-yield", "physiologicalmaturity-overall",
        "harvest-yield", "harvest-overall", 
        "physiologicalmaturity-overallchar2")

R = lapply(R, function(x){
  k = x$trait %in% sel 
  x[k, ]
})

lapply(R, function(x) unique(x$trait))

# standardize names of traits and crop stages
R = lapply(R, function(x){
  x$trait = ifelse(grepl("-overall", x$trait), "overall", "yield")
  x[,c(1, 2, 3, 5, 6)]
})

lapply(R, function(x) unique(x$trait))

# combine data and merge with the covariates
R = do.call("rbind", R)

D = lapply(dat, function(x){
  x$block_data$crop_name = x$metadata$crop$name
  x$block_data$trial_year = format(as.Date(x$metadata$date$start), "%Y")
  x$block_data$trial_id = x$metadata$study$id
  x$block_data$country = x$metadata$study$country
  k = names(x$block_data)[grep("crop_name|block_id|plantingdate|longitude|latitude|trial_year|trial_id|country", 
                               names(x$block_data))]
  x$block_data[k]
})

D = do.call("rbind", D)

D = merge(R, D, all.x = TRUE, by = "block_id")

names(D) = gsub("vegetative_", "", names(D))

keep = !is.na(D$longitude) & !is.na(D$latitude) & !is.na(D$plantingdate)

D = D[keep, ]

head(D)

D = 
  D %>%
  group_by(block_id, trait) %>%
  filter(
    n() %in% c(2, 3),                 
    n_distinct(value) == n(),          
    setequal(sort(value), seq_len(n()))
  ) %>%
  ungroup()


# check planting dates
D$plantingdate = as.Date(D$plantingdate)

boxplot(D$plantingdate ~ paste(D$crop_name, D$trial_id, sep = "-"))

table(D$crop_name, format(D$plantingdate, "%m"))

# Trial map ######
# make the trial map
# extract country ISO2 code
ctry = unique(D$country)

# download GADM map using geodata
adm = geodata::gadm(country = ctry, level = 1, path = "data/")

adm = st_as_sf(adm)

xy = st_as_sf(D, coords = c("longitude", "latitude"), crs = 4326)

trial_map = 
  ggplot() +
  geom_sf(data = adm, fill = "#f7fcfd", color = "black", alpha = 1) +
  geom_sf(data = xy, aes(color = crop_name), size = 1.5, stroke = 1) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 15)) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.3)

ggsave(paste0("output/trial-map.pdf"),
       plot = trial_map,
       width = 25,
       height = 25,
       units = "cm")

head(D)

# write file with unique block_ids and planting dates
pdates = D[!duplicated(D$block_id), c("block_id", "longitude", 
                                      "latitude", "plantingdate", 
                                      "crop_name", "trial_id",
                                      "country") ]

write.csv(pdates, "data/planting-dates-longlat.csv", row.names = FALSE)

# Split the data and treat each crop trait independently
D = split(D, f = paste(D$crop_name, D$trait, sep = "-"))

croptrait = names(D)

# PlackettLuce rankings 
R = lapply(D, function(x) {
  rank_tricot2(
    data  = x,
    items = "genotype_name",
    input = "value",
    id    = "block_id"
  )
})

R

for(i in seq_along(croptrait)){
  # create a representation of the trial network
  pdf(file = paste0("output/trial-network-", croptrait[i], ".pdf"),
      width = 10,
      height = 10)
  plot(network(R[[i]]))
  dev.off()
}

#..........................................................
#..........................................................
#..........................................................
# PlackettLuce rankings ####

# ANOVA  ####
# this will be used to test whether the varieties are different 
# from each other and we will retain the ones with alpha > 0.05
# get the reference trait
anov = lapply(R, function(x){
  a = anova(PlackettLuce(x))
  a[2, ]
})

anov = do.call("rbind", anov)

anov[,1] = croptrait

anov

write.csv(anov, 
          paste0("output/anova.csv"))

