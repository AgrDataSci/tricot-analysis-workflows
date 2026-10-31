library("ClimMobTools")
library("gosset")
library("PlackettLuce")
library("sf")
library("geodata")
library("ggplot2")
library("jsonlite")
library("tidyverse")
library("magrittr")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

dir.create("output", showWarnings = FALSE)

files = as.list(list.files("data", pattern = ".json", full.names = TRUE))

dat = lapply(files, fromJSON)

covar = read.csv("data/clim-covariates.csv")

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

D = merge(D, covar[,c("block_id", "cluster")], by = 'block_id', all.x = TRUE)

D = D[!is.na(D$cluster), ]

table(D$cluster, D$crop_name)

# covar = merge(covar, D[,c("block_id", "country", "longitude", "latitude")], by = "block_id", all.x = TRUE)
# 
# covar = covar[!duplicated(covar$block_id), ]
# 
# write.csv(covar, "data/covariates.csv", row.names = FALSE)

D$TPE = ifelse(D$cluster == 1, "Dry-Hot with Prolonged Dry Spells",
               ifelse(D$cluster == 2, "Moderately Wet with Irregular Rainfall",
                      ifelse(D$cluster == 3, "Wet and Well-Distributed",
                             ifelse(D$cluster == 4, "Wet with High Rainfall Intensity",
                                    "Hot with Elevated Night Temperatures"))))

# Split the data and treat each crop trait independently
D = split(D, f = paste(D$crop_name, D$trait, D$TPE, sep = "-"))
#..........................................................
#..........................................................
#..........................................................
# PlackettLuce rankings ####
R = lapply(D, function(x) {
  rank_tricot2(
    data  = x,
    items = "genotype_name",
    input = "value",
    id    = "block_id")
})

# ........................................
# ........................................
# Calculate PL model  ####
ref = 1

mod = lapply(R, function(x){
  PlackettLuce(x)
})

top3 = lapply(mod, function(x){
  paste(names(rev(sort(coef(x, log = FALSE))))[1:3], collapse = ", ")
})

top3 = as.data.frame(do.call("rbind", top3))

names(top3) = "recommended_varieties"

top3$id = rownames(top3)

dat = do.call(rbind, D)

dat$id = paste(dat$crop_name, dat$trait, dat$TPE, sep = "-")

dat = merge(dat, top3, by = "id", all.x = TRUE)

dat = dat[!duplicated(dat$block_id), ]

table(dat$recommended_varieties, dat$TPE)

write.csv(dat, 'output/crop-recommendation.csv', row.names = FALSE)



# Trial map ######
# make the trial map
# extract country ISO2 code
ctry = unique(dat$country)

# download GADM map using geodata
adm = geodata::gadm(country = ctry, level = 1, path = "data/")

adm = st_as_sf(adm)

xy = st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326)

xy = xy[xy$trait == "yield", ]

trial_map = 
  ggplot() +
  geom_sf(data = adm, fill = "#ffffe5", color = "black", alpha = 1) +
  geom_sf(data = xy, aes(color = recommended_varieties), size = 1.5, stroke = 1) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.3)

ggsave(paste0("output/map.pdf"),
       plot = trial_map,
       width = 25,
       height = 25,
       units = "cm")

