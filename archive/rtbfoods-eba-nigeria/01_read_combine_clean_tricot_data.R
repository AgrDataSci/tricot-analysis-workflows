# .............................
# .............................
# Packages ####
library("readxl")
library("ClimMobTools")
library("gosset")
library("janitor")



list.files("data")

# the file has two sheets
excel_sheets("data/raw/WP5 Complete Dataset For Nigeria Osun and Benue.xlsx")

# benue
dat1 <- read_excel("data/raw/WP5 Complete Dataset For Nigeria Osun and Benue.xlsx",
                 sheet = 1, 
                 na = c(".", " "))

# osun
dat2 <- read_excel("data/raw/WP5 Complete Dataset For Nigeria Osun and Benue.xlsx",
                   sheet = 2, 
                   na = c(".", " "))


names(dat1) <- make_clean_names(names(dat1))
names(dat2) <- make_clean_names(names(dat2))

# add region
dat1$region <- "Osun"
dat2$region <- "Benue"

# add package info from benue
list.files("data/raw")

excel_sheets("data/raw/WP5 Consumer Testing_Osun and Benue Varieties Randomization.xlsx")

pkg1 <- read_excel("data/raw/WP5 Consumer Testing_Osun and Benue Varieties Randomization.xlsx",
                   sheet = 2, 
                   na = c(".", " "))

names(pkg1)

names(pkg1) <- make_clean_names(names(pkg1))

names(pkg1)

pkg1$package_code <- gsub("Package #", "", pkg1$package_code)


# merge the data sets 

dat1 <- merge(pkg1, dat1, by = "package_code", all.x = TRUE)

# merge dataset from benue and osun
dat <- rowbind(dat1, dat2)

names(dat) <- gsub("which_of_these_eba_samples_abc_is_your_",
                   "",
                   names(dat))

names(dat) <- gsub("considering_only_",
                   "",
                   names(dat))

# remove farmer name and phone
rmv <- !grepl("farmer_name|phone", names(dat))

dat <- dat[rmv]

dat$package_code <- as.integer(dat$package_code)

#dat <- dat[order(dat$package_code), ]

dat$id <- paste0(dat$region, "-pkg", dat$package_code)

dat <- dat[union("id", names(dat))]

write.csv(dat, "data/tricot-consumer-cassava-data.csv", row.names = FALSE)


# now do the same for the genotype features 
excel_sheets("data/raw/RTBFoods-WP5_Lab gari samples_Benue and Osun-updated files.xlsx")

# first the biophysical
feat1  <- read_excel("data/raw/RTBFoods-WP5_Lab gari samples_Benue and Osun-updated files.xlsx", 
                  sheet = 1, 
                  na = c(".", " "))
names(feat1) <- make_clean_names(names(feat1))

# the other region
feat2  <- read_excel("data/raw/RTBFoods-WP5_Lab gari samples_Benue and Osun-updated files.xlsx", 
                     sheet = 3, 
                     na = c(".", " "))
names(feat2) <- make_clean_names(names(feat2))

feat <- rowbind(feat1, feat2)

rmv <- c("processor", "sample", "batch")
rmv <- !names(feat) %in% rmv

feat <- feat[rmv]

# now for the othe traits
excel_sheets("data/raw/RTBFoods-WP5_Lab gari samples_Benue and Osun-updated files.xlsx")

ebatex1  <- read_excel("data/raw/RTBFoods-WP5_Lab gari samples_Benue and Osun-updated files.xlsx", 
                     sheet = 2, 
                     na = c(".", " "))
names(ebatex1) <- make_clean_names(names(ebatex1))


ebatex2  <- read_excel("data/raw/RTBFoods-WP5_Lab gari samples_Benue and Osun-updated files.xlsx", 
                       sheet = 4, 
                       na = c(".", " "))
names(ebatex2) <- make_clean_names(names(ebatex2))


head(ebatex1)

ebatex <- rowbind(ebatex1, ebatex2)

rmv <- c("processor", "sample", "batch")
rmv <- !names(ebatex) %in% rmv

ebatex <- ebatex[rmv]


names(feat)[names(feat) == "variety_name"] <- "genotype"

names(ebatex)[names(ebatex) == "variety_name"] <- "genotype"

# check whether the varieties names have the same names as the lab data
pack <- paste0("option_", letters[1:3])

unique(unlist(dat[pack]))
unique(feat$genotype)

all(unique(unlist(dat[pack])) %in% as.character(feat$genotype))
all(as.character(feat$genotype) %in% unique(unlist(dat[pack])))

feat <- feat[as.character(feat$genotype) %in% unique(unlist(dat[pack])), ]

all(unique(unlist(dat[pack])) %in% as.character(feat$genotype))
all(as.character(feat$genotype) %in% unique(unlist(dat[pack])))

# check texture data
all(unique(unlist(dat[pack])) %in% as.character(ebatex$genotype))
all(as.character(ebatex$genotype) %in% unique(unlist(dat[pack])))

ebatex <- ebatex[as.character(ebatex$genotype) %in% unique(unlist(dat[pack])), ]

all(unique(unlist(dat[pack])) %in% as.character(ebatex$genotype))
all(as.character(ebatex$genotype) %in% unique(unlist(dat[pack])))


# write files
write.csv(feat, "data/biophysical-features.csv", row.names = FALSE)

write.csv(ebatex, "data/eba-texture-features.csv", row.names = FALSE)

