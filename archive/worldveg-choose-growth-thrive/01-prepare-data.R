library("sf")
library("geodata")
library("ggplot2")
library("ggrepel")
library("ggspatial")
library("jsonlite")
library("tidyverse")
library("magrittr")
library("gosset")

dir.create("output", showWarnings = FALSE)

files = as.list(list.files("data/raw/", pattern = ".json", full.names = TRUE))

dat = lapply(files, fromJSON)

#..........................................................
#..........................................................
#..........................................................
# Organize traits ####
# Assess trait rankings 
R = lapply(dat, function(x){
  r = x$plot_data
  r = r[which(r$value_type == "rank"), ]
  r
})

unique(unlist(lapply(R, function(x) unique(x$trait))))

# combine data and merge with the covariates
R = do.call("rbind", R)
names(R)

# remove ties
R = 
  R %>%
  group_by(block_id, trait) %>%
  filter(n() %in% c(2, 3),                 
         n_distinct(value) == n(),          
         setequal(sort(value), seq_len(n()))) %>%
  ungroup()

# get unique ids
ids = unique(R$block_id)

sel = c("crop_name", "block_id", "longitude", "latitude",
        "trial_year", "trial_id", "country", "registration_survey_end",
        "socioeconomic_gender", "socioeconomic_age", "_plantingdate")

sel = paste(sel, collapse = "|")
        
# get block data
D = lapply(dat, function(x){
  x$block_data$crop_name = x$metadata$crop$name
  x$block_data$trial_year = format(as.Date(x$metadata$date$start), "%Y")
  x$block_data$trial_id = x$metadata$study$id
  x$block_data$country = x$metadata$study$country
  k = names(x$block_data)[grep(sel, names(x$block_data))]
  x$block_data[k]
})

lapply(D, names)

D = lapply(D, function(x){
  names(x) =  gsub("nursery_|transplantation_|vegetative_|socioeconomic_|ppinire_", 
                   "", 
                   names(x))
  x
})

lapply(D, names)

D = rowbind(D)

# remove entries with no ids in R
keep = D$block_id %in% ids

D = D[keep, ]

# remove entries with longitude and latitude 
keep = !is.na(D$longitude) & !is.na(D$latitude) 

D = D[keep, ]

head(D)

# update ids
ids = unique(D$block_id)

keep = R$block_id %in% ids

R = R[keep, ]

# check planting dates
sum(is.na(D$plantingdate))

boxplot(as.Date(D$plantingdate) ~ paste(D$crop_name, D$trial_id, sep = "-"))

# fix outliers and replace then with the mean
D$plantingdate = as.integer(as.Date(D$plantingdate))
D$registration_survey_end = as.integer(as.Date(D$registration_survey_end))

# split the data
D = split(D, D$trial_id)

D = lapply(D, function(x){

  #if the is outlier, replace with the average
  out = boxplot.stats(x$plantingdate)$out
  rm = x$plantingdate %in% out
  x$plantingdate[rm] = NA
  x$plantingdate = ifelse(is.na(x$plantingdate), 
                          mean(x$plantingdate, na.rm = TRUE), 
                          x$plantingdate)
  
  x
})

D = do.call("rbind", D)

D$plantingdate = as.Date(D$plantingdate, origin = "1970-01-01")

boxplot(D$plantingdate ~ paste(D$crop_name, D$trial_id, sep = "-"))

# make a nice boxplot
pdates = 
  ggplot(data = D, aes(x = plantingdate, y = paste(crop_name, trial_id, country, sep = "-"))) +
  geom_boxplot() +
  labs(x = "Planting date", y = "Trial") +
  theme_bw() +
  theme(text = element_text(size = 12))

ggsave(paste0("output/planting-dates.pdf"),
       plot = pdates,
       width = 15,
       height = 10,
       units = "cm")

# fix gender
unique(D$gender)

D$gender[D$gender == "Femme"] = "Woman"
D$gender[D$gender == "Homme"] = "Man"
D$gender[D$gender == "Je Préfère Ne Pas Le Dire"] = NA

table(D$gender)

# fix age
D$age = ifelse(D$age > 90, NA, 
               ifelse(D$age < 20, NA, D$age))

boxplot(D$age)

pgender = 
  D %>% filter(is.na(gender) == FALSE) %>% 
ggplot(aes(x = age, y = gender)) +
  labs(x = "Gender", y = "Age") +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size = 12))

ggsave(paste0("output/gender-age.pdf"),
       plot = pgender,
       width = 10,
       height = 10,
       units = "cm")

# Trial map ######
# make the trial map
# extract country ISO2 code
ctry = unique(D$country)

# download GADM map using geodata
#adm = geodata::gadm(country = ctry, level = 1, path = "data/")
#adm = st_as_sf(adm)
adm = st_read("data/gadm/africa/africa_adm0.shp")
adm = st_as_sf(adm)
adm = adm[-c(2:3)]

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
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.3)

ggsave(paste0("output/trial-map.pdf"),
       plot = trial_map,
       width = 25,
       height = 25,
       units = "cm")

unique(R$trait)

R$trait[R$trait == "overallchar"] = "overall"

# write cleaned data 
write.csv(D, "data/block-data.csv", row.names = FALSE)

write.csv(R, "data/ranking-data.csv", row.names = FALSE)

