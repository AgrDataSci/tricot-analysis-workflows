library("PlackettLuce")
library("gosset")
library("ClimMobTools")
library("ggplot2")
source("script/helper_01_functions.R")


dat <- read.csv("data/potato-data-tricot.csv")

names(dat)

traits <- c("vegetative2_bacterialwilt", 
            "postharvest1_yield", 
            "postharvest1_marketableatharv",
            "postharvest3_overallper")

base_trait <- 4

items <- paste0("package_item_", LETTERS[1:3])

itemnames <- sort(unique(unlist(dat[items])))

# I may ignore some pre-ranking process
# for the sake of this example
R <- list()

for(i in seq_along(traits)){
  
  # select the item names and rankings for the trait in the iteration i
  d_i <- dat[, c(items, paste0(traits[i], c("_pos", "_neg")))]
  # not observed as NA
  d_i[d_i == "Not observed"] <- NA
  # check for ties in the response pos == neg
  keep <- d_i[,4] != d_i[,5] & !is.na(d_i[,4]) & !is.na(d_i[,5])
  # keep only the TRUE values out of the later validation
  d_i <- d_i[keep, ]
  
  R[[i]] <- rankTricot(d_i, items = 1:3, input = 4:5)


}

# PlackettLuce model
mod <- lapply(R, function(x){
  
  PlackettLuce(x)
  
})

# Now get the coefficients from the models
coeffs <- lapply(mod, function(x){
  
  x <- rank(coefficients(x, log = FALSE))
  
  x <- x[itemnames]
  
})


# base coeff (trait) as a new object
coeffs_base <- coeffs[[base_trait]]

# other coeffs in a new object
coeffs_trait <- coeffs[-base_trait]

# kendall tau
kendall <- lapply(coeffs_trait, function(x){
  kendallTau(x, coeffs_base)
})

kendall <- do.call(rbind, kendall)

kendall$trait <- traits[-base_trait]

kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall

# another approach is to make a heatmap to check the performance of 
# varieties
# this map plots the log-worth from PL models
# and helps in visualizing in which trait each
# variety is doing well or bad
winprob_map(mod, traits)











