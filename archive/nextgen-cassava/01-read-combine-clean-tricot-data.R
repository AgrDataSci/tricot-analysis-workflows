# .............................
# .............................
# This script puts the data together in a single 
# data frame
# Packages ####
library("readxl")
library("ClimMobTools")
library("janitor")

# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/01_read_combine_clean_tricot_data.txt")

list.files("data/raw")

# the file has two sheets, 1st with the postharvest data and 
# the 2nd with the agronomic performance data
dat = read_excel("data/raw/TRICOT_Combined_raw_data.xlsx",
                 sheet = 1, 
                 na = c(".", " "))

dat = as.data.frame(dat)

# now do the same for the other sheet
dat2 = read_excel("data/raw/TRICOT_Combined_raw_data.xlsx",
                  sheet = 2, 
                  na = c(".", " "))

dat2 = as.data.frame(dat2)

# the yield data collected in the 2nd year
list.files("data/raw/year2", full.names = TRUE)
yield_dat = read_excel("data/raw/year2/TRICOT Yr2_Combined NRCRI  IITA_METRIC_HARVEST Data_bt_Sam_2605_la_Anonymous.xlsx", 
                       sheet = 1,
                       na = c(".", " "))

# table with list of correct names
fixednames = read.csv("data/raw/item_names.csv")

# open the file with rhomis data
rh1 = read_excel("data/raw/indicator_sheet_PLUS TRICOT PACKAGE NRS.xlsx",
                 sheet = 1, 
                 na = c(".", " "))

# open the file with information about cassava processing
process = read_excel("data/raw/TRICOT_Combined_raw_data _PROCESSOR IDENTIFICATION_FINAL.xlsx", 
                     sheet = 1)

names(process) = make_clean_names(process[1,])

names(process)[c(2:3)]

process = process[-c(2:3), ]

# select only the colums with information about processing
names(process)[c(1, 11:14)]

process[,c(1, 11:14)]

process = process[,c(1, 11:14)]

process = process[-1, ]

process

names(process) = c("tricot_pkg", "processor", "other_processor_gender", 
                   "other_processor_from", "other_processor_relation")

# fix package names
process$tricot_pkg = ifelse(grepl("Package #", process$tricot_pkg), 
                            paste0(process$tricot_pkg, "-1"), 
                            paste0(process$tricot_pkg, "-2"))

table(process$tricot_pkg)

process$tricot_pkg = gsub("Package #", "", process$tricot_pkg)

process$tricot_pkg = paste0("package-", process$tricot_pkg)

table(process$tricot_pkg)

table(process$processor)
table(process$other_processor_gender)
table(process$other_processor_from)
table(process$other_processor_relation)

process[2:5] = lapply(process[2:5], function(x) {
  ClimMobTools:::.title_case(x)
})

process$other_processor_gender =  ifelse(process$other_processor_gender == "Male", "Man",
                                         ifelse(process$other_processor_gender == "Female", "Woman",
                                                process$other_processor_gender))

table(process$processor)
table(process$other_processor_gender)
table(process$other_processor_from)
table(process$other_processor_relation)

process$processor[process$processor == "."] = NA

process$processor = ifelse(grepl("Wife|Mother|Sister", process$processor),
                           "Other Family Member",
                           process$processor)

process$processor = ifelse(grepl("Hired", process$processor),
                           "Other Person",
                           process$processor)

table(process$processor)

# .............................
# .............................
# fix col names in dat ####
# the colnames contains the name of farmers characterization variables
# and the data collection moments
n1 = as.vector(names(dat))
# the first line contains the trait names
n2 = as.vector(t(dat[1,]))
# the second line contains the indices for best and worst 
# response for each trait
n3 = as.vector(t(dat[2,]))
# put all three together as a single element
n = paste(n1, n2, "_", n3, collapse = ",")
# remove special characters
n = gsub("NA|[...]|[/]| |[0-9]+", "", n)
n = gsub("\\s*\\([^\\)]+\\)", "", n) # [^[:alnum:]]

# split it
n = strsplit(tolower(n), ",")[[1]]

# look for the indices of worst side of
# response in each trait
index_w = which(grepl("_worst", n))

# now take the name of traits
chars = n[index_w-1]

chars = gsub("_best","",chars)

n[index_w] = paste0(chars, n[index_w])

n = gsub("_$", "", n)
n = gsub("[^[:alnum:]]", "_", n) # 
n
# now look for the data collection moments
n1 = gsub("[^[:alnum:]]", "_", n1)
n1 = gsub("__|___", "_", n1)
n1 = tolower(n1)

gsub("[0:9]", "", n1)

n1 = n1[c(14, 31, 44, 59, 78, 95)]

n1 = gsub("_$", "", n1)

n1

n[14:30] = paste(n1[1], n[14:30], sep = "_")
n[31:43] = paste(n1[2], n[31:43], sep = "_")
n[44:58] = paste(n1[3], n[44:58], sep = "_")
n[59:77] = paste(n1[4], n[59:77], sep = "")
n[78:94] = paste(n1[5], n[78:94], sep = "")
n[95:99] = paste(n1[6], n[95:99], sep = "_")

n

names(dat) = n

# remove first two rows since it is the col names
dat = dat[-c(1:2), ]

names(dat)

# .............................
# .............................
# fix col names in dat2 ####
# the colnames contains the name of farmers characterization variables
# and the data collection moments
n1 = as.vector(names(dat2))
# the first line contains the trait names
n2 = as.vector(t(dat2[1,]))
# the second line contains the indices for best and worst 
# response for each trait
n3 = as.vector(t(dat2[2,]))
# put all three together as a single element
n = paste(n1, n2, "_", n3, collapse = ",")
# remove special characters
n = gsub("NA|[...]|[/]| |[0-9]+", "", n)
n = gsub("\\s*\\([^\\)]+\\)", "", n) # [^[:alnum:]]

# split it
n = strsplit(tolower(n), ",")[[1]]

# look for the indices of worst side of
# response in each trait
index_w = which(grepl("_worstoff", n))

# now take the name of traits
chars = n[index_w-1]

chars = gsub("_best","",chars)

n[index_w] = paste0(chars, n[index_w])

n = gsub("_$", "", n)
n = gsub("[^[:alnum:]]", "_", n) # 
n = gsub("_worstoff", "_worst", n)

# now look for the data collection moments
n1 = gsub("[^[:alnum:]]", "_", n1)
n1 = gsub("__|___", "_", n1)
n1 = tolower(n1)

n1

n1 = n1[c(13, 23, 35, 47)]

n[13:22] = paste(n1[1], n[13:22], sep = "_")
n[23:34] = paste(n1[2], n[23:34], sep = "_")
n[35:46] = paste(n1[3], n[35:46], sep = "_")
n[47:56] = paste(n1[4], n[47:56], sep = "_")

n = gsub("imonth", "", n)
n = gsub("_months", "", n)
n = gsub("i_month", "first_month_", n)
n = gsub("3months", "third_month_", n)
n = gsub("6months", "sixth_month_", n)
n = gsub("9months", "ninth_month_", n)
n = gsub("__", "_", n)

names(dat2) = n 

n

names(dat)
names(dat2)

# fix tricot package id by adding year
dat$tricot_pkg = paste0("package-", dat$farmerpackage, "-", dat$year)

dat2$tricot_pkg = paste0("package-", dat2$farmerpackage, "-", dat2$year)

# remove first two rows since it is the col names
names(dat2)[c(1:3, 57:59)]

dat2 = dat2[, -c(1:3, 57:59)]

dat2 = dat2[-c(1:2), ]

sum(dat2$tricot_pkg %in% dat$tricot_pkg)

sum(is.na(dat$sex))
sum(is.na(dat2$sex))

# merge both data by package, but remove the first lines in dat so it 
# will not repeat in the final dataset
names(dat)

names(dat2)

names(dat2)[names(dat2) %in% names(dat)]

names(dat)[names(dat) %in% names(dat2)]

rmv = names(dat)[names(dat) %in% names(dat2)]

rmv = rmv[rmv != "tricot_pkg"]

rmv = !names(dat2) %in% rmv

names(dat2)[rmv]

dat2 = dat2[, rmv]

# merge
dat = merge(dat2, dat, by = "tricot_pkg", all.x = TRUE)

rm(dat2)

names(dat)

organize = union(names(dat[c(1, 46:58, 145:149)]), names(dat))

organize

dat = dat[organize]

# .............................
# .............................
# check coordinates ####
dat$longitude
dat$latitude

trial_map = plot_map(data = dat, 
                     make.clusters = FALSE,
                     xy = c("longitude", "latitude"),
                     minimap = TRUE, 
                     map_provider = "OpenStreetMap.Mapnik")

trial_map

# mapview::mapshot(trial_map, 
#                  url = "output/trial_map.html",
#                  file = "output/trial_map.png")


# .............................
# .............................
# fix variety names ####
vars = paste0("variety", letters[1:3])
itemnames = unique(unlist(dat[, vars]))

dat[, vars] = lapply(dat[,vars], function(x) {
  x = gsub("[Cassava varieties: ]", "",x)
  x = gsub("[\r\n]", "", x)
  x = gsub("V", "", x)
  x = gsub(" ", "", x)
  x = gsub("ITA-", "IITA-", x)
  x = gsub("IIITA-", "IITA-", x)
  x = gsub("_[0-9]+$","", x)
  x = gsub("-[0-9]+$","", x)
  x = gsub("IITA", "IITA-", x)
  x = gsub("TMS", "TMS-", x)
  x = gsub("_", "", x)
  x = gsub("--", "-", x)
  #x = gsub("TMEB69$", "TMEB693", x)
})

# use the table with fixed item names
unique(unlist(dat[, vars])) %in% fixednames$genotype_original_name

for (i in seq_along(fixednames$genotype_original_name)) {
  dat$varietya[dat$varietya == fixednames$genotype_original_name[i]] = fixednames$correct_name[i]
  dat$varietyb[dat$varietyb == fixednames$genotype_original_name[i]] = fixednames$correct_name[i]
  dat$varietyc[dat$varietyc == fixednames$genotype_original_name[i]] = fixednames$correct_name[i]
}

dat = dat[!is.na(dat$varietya), ]

itemnames = sort(unique(unlist(dat[, vars])))

table(unlist(dat[, vars]))

# .............................
# .............................
# check explanatory variables
unique(dat$sex)

table(dat$sex)

sum(is.na(dat$sex))

dat[, "sex"] = ifelse(dat[, "sex"] == "Male", "M",
                      ifelse(dat[, "sex"] == "Female", "F",
                             dat[, "sex"]))

dat$sex[dat$sex == "M"] = "Man"
dat$sex[dat$sex == "F"] = "Woman"
dat$sex[dat$sex == "0"] = NA

table(dat$sex)

names(dat)

names(dat)[names(dat) == "eba_quality_after_preparation_worstsmoothness_worst"] = "eba_quality_after_preparation_smoothness_worst"
names(dat)[names(dat) == "eba_quality_after_preparation_best"] = "eba_quality_after_preparation_smoothness_best"
names(dat)[names(dat) == "eba_quality_after_preparation_best"] = "eba_quality_after_preparation_smoothness_best"

names(dat)

# check duplicated packages
duplicated(dat$tricot_pkg)

sum(duplicated(dat$tricot_pkg))

# ..........................................
# .........................................
# RHoMIS files ####
names(rh1) = make_clean_names(names(rh1))

names(rh1)

names(rh1)[names(rh1) == "section_meta_tricot_pack_no"] = "tricot_pkg"

rh1$tricot_pkg

rh1 = rh1[!is.na(rh1$tricot_pkg), ]

sum(duplicated(rh1$tricot_pkg))

# remove duplicated packages
rmv = rh1$tricot_pkg[duplicated(rh1$tricot_pkg)]

keep = !rh1$tricot_pkg %in% rmv

rh1 = rh1[keep,]

sum(dat$tricot_pkg %in% rh1$tricot_pkg)

rh1$tricot_pkg

rh1$tricot_pkg = paste0("package-", rh1$tricot_pkg, "-1")

# fix tricot packages for the same farmers in year2 to 
# merge with the rhomis data
dat$packageyear = ifelse(!is.na(dat$packageyear),
                         paste0("package-", dat$packageyear, "-1"),
                         dat$tricot_pkg)

dat$rhomis = dat$packageyear

rh1$rhomis = rh1$tricot_pkg

sum(dat$rhomis %in% rh1$rhomis)

names(rh1)[7:18]

names(dat)

rh1 = rh1[,-c(7:18)]

names(rh1)
names(dat)

dat = merge(dat, rh1, by = "rhomis", all.x = TRUE)

# add region 
table(dat$state)

dat$region = ifelse(dat$state == "Akwa Ibom", "South South", 
                    ifelse(dat$state == "Imo", "South East", 
                           ifelse(dat$state == "Benue", "North Central", 
                                  ifelse(dat$state == "Osun", "South West", NA))))

table(dat$region)

dat$agroregion = ifelse(dat$state == "Akwa Ibom", "Humid Forest", 
                        ifelse(dat$state == "Imo", "Humid Forest", 
                               ifelse(dat$state == "Benue", "Forest-Savanna Transition", 
                                      ifelse(dat$state == "Osun", "Forest-Savanna Transition", NA))))

table(dat$agroregion)

# add information about processing
dat = merge(dat, process, by = "tricot_pkg", all.x = TRUE)

# ..............................
# ..............................
# add yield data ####
yield_dat

names(yield_dat) = make_clean_names(names(yield_dat))

yield_dat = yield_dat[-1, ]

dat$tricot_pkg

yield_dat$tricot_pkg = paste0("package-", yield_dat$package_number, "-2")

yield_dat$tricot_pkg %in% dat$tricot_pkg

names(yield_dat)

yield_dat = yield_dat[,-c(1:6)]

# merge the data
dat = merge(dat, yield_dat, by = "tricot_pkg", all.x = TRUE)


write.csv(dat, "data/tricot-data-nextgen-cassava.csv", row.names = FALSE)


