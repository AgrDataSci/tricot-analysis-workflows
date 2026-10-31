library("readxl")
library("janitor")
library("gosset")

list.files("data/raw/stations", full.names = TRUE)

coords <- read_excel("data/raw/stations/GPS data of trial sites.xlsx",
                   sheet = 1)


dat1 <- read_excel("data/raw/stations/NPT data for Mendes.xlsx",
                   sheet = 1)


dat2 <- read_excel("data/raw/stations/NPT data for set 2.xlsx",
                   sheet = 1)


dat3 <- read_excel("data/raw/stations/NPT data for set 2.xlsx",
                   sheet = 2)



names(dat1) <- make_clean_names(names(dat1))

names(dat2) <- make_clean_names(names(dat2))

names(dat3) <- make_clean_names(names(dat3))


names(dat1)
names(dat2)
names(dat3)

names(dat1) <- gsub("sever$", "severity", names(dat1))
names(dat1) <- gsub("incid$", "incidence", names(dat1))
names(dat1) <- gsub("plant_unif$", "plant_uniformity", names(dat1))
names(dat1) <- gsub("no_", "number_", names(dat1))
names(dat1) <- gsub("clone_name", "clone", names(dat1))


names(dat2) <- gsub("airp", "site", names(dat2))
names(dat2) <- gsub("sever$", "severity", names(dat2))
names(dat2) <- gsub("sever2", "severity2", names(dat2))
names(dat2) <- gsub("sver_2", "severity2", names(dat2))
names(dat2) <- gsub("incid$", "incidence", names(dat2))
names(dat2) <- gsub("incid2", "incidence2", names(dat2))
names(dat2) <- gsub("incid_2", "incidence2", names(dat2))
names(dat2) <- gsub("p_", "plant_", names(dat2))
names(dat2) <- gsub("clone_id", "clone", names(dat2))
names(dat2) <- gsub("n_|nbr_", "number_", names(dat2))

names(dat3) <- gsub("clone_id", "clone", names(dat3))
names(dat3) <- gsub("n_|nbr_", "number_", names(dat3))



# merge dat2 and dat3
dat1$id <- paste0(dat1$site, dat1$no, dat1$clone, dat1$rep)
dat2$id <- paste0(dat2$site, dat2$no, dat2$clone, dat2$rep)
dat3$id <- paste0(dat3$site, dat3$no, dat3$clone, dat3$rep)

length(unique(dat1$id)) == nrow(dat1)
length(unique(dat2$id)) == nrow(dat2)
length(unique(dat3$id)) == nrow(dat3)

dat3 <- dat3[, !names(dat3) %in% c("site", "no", "clone",
                                   "rep", "number_emerged_plants", "number_of_tubers_planted")]

dat2 <- merge(dat2, dat3, by = "id")

names(dat2)


dat <- rowbind(dat1, dat2)

unique(length(as.integer(as.factor(dat$id))))

dat$id <- as.integer(as.factor(dat$id))

# add coordinates 
names(coords) <- make_clean_names(names(coords))

names(coords)

coords <- coords[!is.na(coords$latitude), ]

coords <- coords[!duplicated(coords$sector), ]

coords <- coords[,c("sector", "longitude", "latitude", "planting_date", "category")]

names(coords) <- gsub("sector", "site", names(coords))
names(coords) <- gsub("category", "station_category", names(coords))


coords$site %in% dat$site

sort(unique(coords$site))

sort(unique(dat$site))

coords$site[!coords$site %in% dat$site]


dat$site <- gsub("MUKO", "Muko", dat$site)
dat$site <- gsub("MT Kinigi", "Kinigi", dat$site)
dat$site <- gsub("Karongi", "Karenge", dat$site)


dat <- merge(coords, dat, by = "site", all.y = TRUE)

dat <- dat[, union("id", names(dat))]

write.csv(dat, "data/station-data.csv", row.names = FALSE)
