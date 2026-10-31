library("tidyverse")
library("janitor")
library("ggrepel")
library("tidyverse")
library("patchwork")
library("magrittr")
library("ggfortify")
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("readxl")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")


key = read.table("token/key.txt")[1,]

p = getProjectsCM(key = key, server = "1000FARMS")

keep = p$coordinator == "An Notenbaert"

p = p[keep, ]

p

cmdata = list()

for(i in seq_along(p$project_id)) {
  
  x = getDataCM(key = key,
                project = p$project_id[i],
                server = "1000FARMS",
                userowner = p$user_owner[i],
                as.data.frame = TRUE, 
                tidynames = TRUE,
                pivot.wider = TRUE)
  
  cmdata[[i]] = x
  
}


# get unique col names across the data.frames
uniquenames = lapply(cmdata, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

uniquenames

dat = rowbind(cmdata)

dat$block_id = paste(dat$package_project_name, dat$id, sep = "-")

dat = dat[union(c("block_id", "registration_qst_region", 
                "registration_kebele", "registration_district"),
                names(dat))]

names(dat)

names(dat) = gsub("registration_|qst_", "", names(dat))

# second harvest has two collection moments, we need to put it together
keep = grep("secondharvest", names(dat))

sec = dat[, keep]

dat = dat[, -keep]

secnames = unique(gsub("secondharvestpartii_|secondharvest_", "", names(sec)))

newsec = as.data.frame(matrix(NA, 
                              nrow = nrow(dat), 
                              ncol = length(secnames),
                              dimnames = list(1:nrow(dat), 
                                              paste0("secondharvest_", secnames))))


for (i in seq_along(secnames)) {
  
  x = sec[, grep(secnames[i], names(sec))]
  
  if (is.null(ncol(x))) {
    newsec[, grep(secnames[i], names(newsec))] = x
    next
  }
  
  z = apply(x, 1, function(y){
    which(!is.na(y))[1]
  })
  
  z[is.na(z)] = 1
  
  
  newsec[, grep(secnames[i], names(newsec))] = x[cbind(1:nrow(x), z)]
  
}

dat = cbind(dat, newsec)


# merge farmers response on best and worst why
keep = grep("bestwhy", names(dat))

x = apply(dat[, keep], 1, function(y) paste(na.omit(y), collapse = ", "))

dat = dat[, -keep]

dat$bestwhy = x

# worst
keep = grep("worstwhy", names(dat))

x = apply(dat[, keep], 1, function(y) paste(na.omit(y), collapse = ", "))

dat = dat[, -keep]

dat$worstwhy = x

names(dat)

write.csv(dat, "data/tricot-data-ethiopiagrass.csv", row.names = FALSE)
