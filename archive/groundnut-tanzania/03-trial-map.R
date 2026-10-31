#............................................
#............................................
#  Trial map 
library("geodata")
library("sf")
library("ggplot2")

# Load data
dat = read.csv("data/groundnut-tricot-tanzania.csv")

dat = dat[dat$package_project_name != "gnuttz2025", ]

dat = dat[dat$package_project_name != "gnuttZ2324", ]

adm = gadm("TZ", 1, "data")

adm = st_as_sf(adm)

adm$region = ifelse(adm$NAME_1 == "Kagera" | adm$NAME_1 == "Mwanza" | adm$NAME_1 == "Mara" , "Lake",
                    ifelse(adm$NAME_1 == "Kigoma" | adm$NAME_1 ==  "Shinyanga" | adm$NAME_1 == "Tabora" | adm$NAME_1 == "Geita"  | adm$NAME_1 == "Simiyu", "Western",
                           ifelse(adm$NAME_1 == "Arusha" | adm$NAME_1 == "Kilimanjaro" | adm$NAME_1 == "Manyara" | adm$NAME_1 == "Tanga", "Northern",
                                  ifelse(adm$NAME_1 == "Singida" | adm$NAME_1 =="Dodoma", "Central",
                                         ifelse(adm$NAME_1 == "Rukwa" | adm$NAME_1 == "Mbeya" | adm$NAME_1 == "Iringa" | adm$NAME_1 == "Katavi" | adm$NAME_1 == "Songwe" | adm$NAME_1 == "Njombe", "Southern Highlands",
                                                ifelse(adm$NAME_1 == "Lindi" | adm$NAME_1 == "Mtwara" | adm$NAME_1 == "Ruvuma", "Southern",
                                                       "Eastern"))))))

coords = dat[,c("longitude", "latitude")]

coords[coords == 0] = NA

coords = na.omit(coords)

# to ensure the privacy of participants location
# we can put the lonlat info into clusters of 0.5 resolution
h = dist(coords)

h = hclust(h)

h = cutree(h, h = 0.25)

# split the d by each defined cluster
d = split(coords[, 1:2], h)

# and take the mean 
d = lapply(d, function(x) {
  colMeans(x)
})

# back to data frame
d = do.call("rbind", d)

d = as.data.frame(d)

names(d) = c("longitude", "latitude")

d$trial = "tricot"

coords = st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326)

rcbd = matrix(c(-6.5167,	38.9167,
                -10.3539,	40.1682,
                -4.8356,	29.6777,
                -5.5866,	35.4609,
                -8.9094,	34.4560,
                -10.22,	38.4545,
                -5.3456,	32.4156,
                -2.4300,	33.1000,
                -11.0400,	37.2100), 
              ncol = 2, byrow = TRUE)

dimnames(rcbd)[[2]] = c("latitude", "longitude")

rcbd = as.data.frame(rcbd)

rcbd$trial = "RCBD"

rcbd = st_as_sf(rcbd, coords = c("longitude", "latitude"), crs = 4326)

coords = rbind(coords, rcbd)

map = 
  ggplot() +
  geom_sf(data = adm, mapping = aes(fill = region), alpha = 0.5) +
  geom_sf(data = coords, mapping = aes(color = trial, shape = trial), size = 2) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.title = element_blank(),
        legend.position = "right")

map

ggsave("output/trial-location-map.pdf",
       plot = map,
       width = 20,
       height = 20,
       units = "cm")
