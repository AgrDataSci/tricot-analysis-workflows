# .......................................
# .......................................
# This script runs the PlackettLuce model 
# to assess farmers preferences 
# Load packages
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("tidyverse")
library("patchwork")
library("readxl")
#source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

#.................................
# Data ####
vars = read_excel("data/names-to-standardize-selected.xlsx")
keep = vars$RELEVANCE == "REL"
vars = vars[keep, ]

# read the data from both countries. 
# this is a csv file previously treated from the original 
# .json file with the farmers' responses on the preferred concepts
maize = list(read.csv('data/maize/maize-kenya.csv'),
             read.csv('data/maize/maize-uganda.csv'))
maize[[1]]$country = "Kenya"
maize[[2]]$country = "Uganda"

cols = unique(names(maize[[1]]), names(maize[[2]]))

common_columns = Reduce(intersect, lapply(maize, names))

cols[!cols %in% common_columns]

maize = rowbind(maize)

names(maize)

maize[paste0("video", 1:3)] = lapply(maize[paste0("video", 1:3)], ClimMobTools:::.title_case)

sort(unique(unlist(maize[paste0("video", 1:3)])))

maize[paste0("video", 1:3)] = lapply(maize[paste0("video", 1:3)], function(x){
  x = gsub(" Variety", "", x)
  x[x=="Drought Avoidance"] = "Drought avoidance"
  x[x=="Green Maize"] = "Consumer-oriented"
  x[x=="Home Use"] = "Home use"
  x[x=="Home Use & Family Consumption"] = "Home use"
  x[x=="Intercropping"] = "Intercropping"
  x[x=="Nutritious"] = "High nutritional"
  x[x=="Resilience"] = "Resilient"
  x[x=="Resilient"] = "Resilient"
  x[x=="Feed"] = "Livestock feed"
  x[x=="Food & Fodder"] = "Food and fodder"
  x[x=="Feed & Fodder"] = "Food and fodder"
  x[x=="Drought avoidance"] = "Drought escape"
  x
})

table(unlist(maize[paste0("video", 1:3)]))

maize$crop = "maize"

maize$best = maize$variety_preferred
maize$worst = maize$variety_lesspreferred

# .........................................
# .........................................
##############################
# sorghum
sorghum = read.csv("data/sorghum/maintable-Sorghum.csv")

# put responses back to A, B, C
unique(unlist(sorghum[paste0("video", 1:3)]))
unique(sorghum$variety_preferred)

sorghum$best = apply(sorghum[, c(paste0("video", 1:3), "variety_preferred")], 1, function(x){
  LETTERS[which(as.vector(x[4]) == as.vector(unlist(x[1:3])))]
})

sorghum$least_preferred

sorghum$worst = apply(sorghum[, c(paste0("video", 1:3), "least_preferred")], 1, function(x){
  LETTERS[which(as.vector(x[4]) == as.vector(unlist(x[1:3])))]
})

sum(sorghum$best == sorghum$worst)

# put concept names as title case
head(sorghum[paste0("video", 1:3)])

sorghum[paste0("video", 1:3)] = lapply(sorghum[paste0("video", 1:3)], ClimMobTools:::.title_case)

sort(unique(unlist(sorghum[paste0("video", 1:3)])))

sorghum[paste0("video", 1:3)] = lapply(sorghum[paste0("video", 1:3)], function(x){
  x = gsub(" Variety", "", x)
  x = gsub("Farage For Animals", "Livestock feed", x)
  x = gsub("Food And Fodder", "Food and fodder", x)
  x = gsub("Feed And Food", "Feed and food", x)
  x = gsub("Food For Family", "Home use", x)
  x = gsub("Grain For Beer", "Industry-oriented", x)
  x = gsub("High Yielding Hybrid", "High-yielding hybrid", x)
  x = gsub("Sorghum And Peas", "Intercropping", x)
  x
})

sort(unique(unlist(sorghum[paste0("video", 1:3)])))

table(unlist(sorghum[paste0("video", 1:3)]))

sorghum$crop = "sorghum"

sorghum$country = "Tanzania"

# .........................................
# .........................................
##############################
# groundnut

# read the data  
# this is a csv file previously treated from the original 
# .json file with the farmers' responses on the preferred concepts
gnut = read.csv("data/groundnut/gnut_tanzania_phase1_and_2_merged.csv")

gnut = gnut[!is.na(gnut$resp_gender), ]

table(gnut$resp_gender)

# put responses back to A, B, C
gnut$variety_preferred

gnut$best = apply(gnut[, c(paste0("video", 1:3), "variety_preferred")], 1, function(x){
  LETTERS[which(as.vector(x[4]) == as.vector(unlist(x[1:3])))]
})

gnut$variety_lesspreferred

gnut$worst = apply(gnut[, c(paste0("video", 1:3), "variety_lesspreferred")], 1, function(x){
  LETTERS[which(as.vector(x[4]) == as.vector(unlist(x[1:3])))]
})

sum(gnut$best == gnut$worst)

# put concept names as title case
head(gnut[paste0("video", 1:3)])

gnut[paste0("video", 1:3)] = lapply(gnut[paste0("video", 1:3)], ClimMobTools:::.title_case)

sort(unique(unlist(gnut[paste0("video", 1:3)])))
sort(unique(unlist(maize[paste0("video", 1:3)])))
sort(unique(unlist(sorghum[paste0("video", 1:3)])))

gnut[paste0("video", 1:3)] = lapply(gnut[paste0("video", 1:3)], function(x){
  x = gsub("Drought resistant variety", "Resilient", x)
  x = gsub("Dual-purpose variety", "Food and fodder", x)
  x = gsub("Early Maturity variety", "Drought escape", x)
  x = gsub("High nutritional variety", "High nutritional", x)
  x = gsub("Home cooking variety", "Home use", x)
  x = gsub("Snacking variety", "Consumer-oriented", x)
  x = gsub("Industrial processor variety", "Industry-oriented", x)
  x = gsub(" variety", "", x)
  x
})

table(unlist(gnut[paste0("video", 1:3)]))
table(unlist(maize[paste0("video", 1:3)]))
table(unlist(sorghum[paste0("video", 1:3)]))

gnut$crop = "groundnut"

gnut$country = "Tanzania"

maize$resp_gender
sorghum$resp_gender

# .........................................
# .........................................
##############################
# get ODK surveys #####
odk1 = read_excel('data/groundnut/Round 1_hh_groundnut_Formshare_0612a.xlsx', sheet = 1)

odk2 = read_excel('data/maize/raw/Kenya/odk_kenya.xlsx', sheet = 1)

odk3 = read_excel('data/sorghum/Formshare excel-1489_cimmyt_sorghum.xlsx', sheet = 1)
names(odk3)[3] = "label"

odk = list(odk1, 
           odk2,
           odk3)

odk = lapply(odk, function(x){
  x = x[,c("name", "label")]
  x = x[!duplicated(x$name),]
  x$name = gsub("X_", "", x$name)
  x$name = gsub("maize_", "", x$name)
  x$name = gsub("groundnut_", "", x$name)
  x$name = gsub("sorghum_", "", x$name)
  x$name = gsub("maize", "", x$name)
  x$name = gsub("groundnut", "", x$name)
  x$name = gsub("sorghum", "", x$name)
  x$name = gsub("crops", "crop", x$name)
  x$name = gsub("grndnut", "", x$name)
  x$name = gsub("_$", "", x$name)
  x$name = gsub("gps_", "", x$name)
  x
})


dat = list(gnut,
           maize,
           sorghum)

dat = lapply(dat, function(x){
  names(x) = gsub("X_", "", names(x))
  names(x) = gsub("maize_", "", names(x))
  names(x) = gsub("groundnut_", "", names(x))
  names(x) = gsub("sorghum_", "", names(x))
  names(x) = gsub("maize", "", names(x))
  names(x) = gsub("groundnut", "", names(x))
  names(x) = gsub("sorghum", "", names(x))
  names(x) = gsub("crops", "crop", names(x))
  names(x) = gsub("grndnut", "", names(x))
  names(x) = gsub("_$", "", names(x))
  names(x) = gsub("gps_", "", names(x))
  x
})


d1 = data.frame(name = names(dat[[1]]), 
                crop1 = "gnut")
d1 = merge(d1, odk[[1]], by = "name", all = TRUE)
names(d1)[3] = "label1"

d2 = data.frame(name = names(dat[[2]]), 
                crop2 = "maize")
d2 = merge(d2, odk[[2]], by = "name", all = TRUE)
names(d2)[3] = "label2"

d3 = data.frame(name = names(dat[[3]]), 
                crop3 = "sorghum")
d3 = merge(d3, odk[[3]], by = "name", all = TRUE)
names(d3)[3] = "label3"

d = merge(d1, d2, all = TRUE, by = "name")

d = merge(d, d3, all = TRUE, by = "name")

d$n_dataset = (!is.na(d$crop1)) + (!is.na(d$crop2)) + (!is.na(d$crop3))

d$in_all_crops = (is.na(d$crop1) + is.na(d$crop2) + is.na(d$crop3)) == 0

d$final_name = ifelse(d$n_dataset == 3, d$name, "")
d[is.na(d)] = ""

# write.csv(d, "data/names-to-standardize.csv", row.names = F)
for(i in seq_along(vars$name)){
  dat = lapply(dat, function(x){
    names(x)[names(x) == vars$name[i]] = vars$final_name[i]
    x
  })
}

dat = rowbind(dat)

table(dat$farm_income)

sel = unique(vars$final_name)

sel = sel[sel %in% names(dat)]

dat = dat[, sel]

vars = vars[!duplicated(vars$final_name), ]

vars = vars[vars$final_name %in% names(dat), ]

names(dat) = gsub("resp_", "", names(dat))

vars$name = gsub("resp_", "", vars$name)

table(dat$curr)
dat$curr[dat$curr == "1"] = "Yes"
dat$curr[dat$curr == "0"] = "No"
table(dat$curr)

table(dat$purchase)
dat$purchase[dat$purchase == "1"] = "Yes"
dat$purchase[dat$purchase == "0"] = "No"
table(dat$purchase)

boxplot.stats(dat$age)$out
dat$age[dat$age > 85] = NA
boxplot(dat$age)

boxplot(dat$bags)
boxplot(log(dat$bags))

boxplot(dat$consump_hh)
boxplot.stats(dat$consump_hh)$out
boxplot(ifelse(dat$consump_hh > 100, dat$consump_hh / 100, dat$consump_hh))
dat$consump_hh = ifelse(dat$consump_hh > 100, dat$consump_hh / 100, dat$consump_hh)

boxplot(ifelse(dat$pyment_knd > 100, dat$pyment_knd / 100, dat$pyment_knd))
dat$pyment_knd = ifelse(dat$pyment_knd > 100, dat$pyment_knd / 100, dat$pyment_knd)

boxplot(dat$sold_direct)
boxplot(ifelse(dat$sold_direct > 100, dat$sold_direct / 100, dat$sold_direct))
dat$sold_direct = ifelse(dat$sold_direct > 100, dat$sold_direct / 100, dat$sold_direct)

boxplot(dat$sold_later)
boxplot(ifelse(dat$sold_later > 100, dat$sold_later / 100, dat$sold_later))
dat$sold_later = ifelse(dat$sold_later > 100, dat$sold_later / 100, dat$sold_later)

# ............................
# get labels from ODK
odk1 = read_excel('data/groundnut/Round 1_hh_groundnut_Formshare_0612a.xlsx', sheet = 2)
odk2 = read_excel('data/maize/raw/Kenya/odk_kenya.xlsx', sheet = 2)
odk3 = read_excel('data/sorghum/Formshare excel-1489_cimmyt_sorghum.xlsx', sheet = 2)
names(odk3)[3] = "label"

odk = rbind(odk1[c("list_name", "value", "label")],
            odk2[c("list_name", "value", "label")],
            odk3[c("list_name", "value", "label")])


odk = odk[!is.na(odk$list_name), ]

odk$id = paste0(odk$list_name, "-", odk$value)

odk = odk[!duplicated(odk$id), ]

odk$label = gsub("[\u00A0]+$", "", odk$label)
odk$value = gsub("[\u00A0]+$", "", odk$value)

odk$list_name[odk$list_name == "marital"] = "marstat"

sel = c("education", "farm_income", "marstat")

table(dat$education)
table(dat$farm_income)
table(dat$marstat)

for(j in seq_along(sel)) {
  
  x = odk[odk$list_name == sel[j], ]
  
  for(i in seq_along(x$value)){
    #dat[sel[j]] = gsub("[\u00A0]+$", "", dat[sel[j]])
    dat[, sel[j]] = ifelse(dat[, sel[j]] == x$value[i], x$label[i], dat[, sel[j]])
    
  }
  
}

table(dat$education)
table(dat$farm_income)
table(dat$marstat)

# get the variables to characters and count how many options participants respond
sel = c("crop_apply_fertlizer", "hh_crop","hh_crop_fertilizer",
        "hh_crop_input", "livestock_name","traits")

dat[sel] = lapply(dat[sel], function(x) {
  x = gsub(" ", ",", x)
  x = gsub("[|]", ",", x)
  x
})

# add the count
for(i in seq_along(sel)) {
  x = unlist(dat[sel[i]])
  x[x == ""] = NA
  NAs = is.na(x)
  x = strsplit(x, ",")
  x = unlist(lapply(x, length))
  x[NAs] = 0
  #dat$x = x
  #names(dat)[names(dat) == "x"] = paste0(sel[i], "_count")
  dat[sel[i]] = x
}

str(dat)

names(dat)

sel = union(c("video1", "video2", "video3", "best", "worst",
              "reason_preferred", "reason_lesspreferred", "crop", "country"),
            names(dat))

dat = dat[sel]

dat = as_tibble(dat)

dat = as.data.frame(dat)

str(dat)

dat[dat<0] = NA

dat[1:ncol(dat)] = lapply(dat[1:ncol(dat)], function(x){
  
  if (class(x) != "character") {
    x[is.na(x)] = median(x, na.rm = TRUE)
  }
  
  if (class(x) == "numeric") {
    x = round(x, 2)
  }
  
  if (class(x) == "character") {
    x = ifelse(is.na(x), "Not specified", x)
    x = as.factor(x)
  }
  
  x
  
})

str(dat)

dat = dat[dat$gender != "Not specified", ]

dat$best[dat$best == "Not specified"] = NA
dat$worst[dat$worst == "Not specified"] = NA

keep = !is.na(dat$worst) & !is.na(dat$best)

table(keep)

dat = dat[keep, ]


write.csv(dat, 'data/mip-tricot-data.csv', row.names = FALSE)

vars = data.frame(vars[c("final_name", "label1")])
vars$label1 = gsub("groundnut|sorghum|maize", "the crop", vars$label1)

write.csv(vars, 'data/variables-description.csv', row.names = FALSE)
