library("geodata")
library("sf")

d = read.csv("data/kenya-districts.csv")
names(d) = tolower(names(d))
head(d)

ke = gadm("KEN", 3, "data")

ke = st_as_sf(ke)

xy = st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326)

dat = st_join(xy, ke, join = st_intersects)


dat

d$NAME_2 = dat$NAME_2


