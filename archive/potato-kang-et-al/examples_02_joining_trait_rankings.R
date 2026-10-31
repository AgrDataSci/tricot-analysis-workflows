library("PlackettLuce")
library("gosset")
library("ClimMobTools")
library("ggplot2")
library("ggparty")
library("patchwork")
source("script/helper_01_functions.R")


dat <- read.csv("data/potato-data-tricot.csv")

names(dat)

# Put the traits but exclude Overall and Plant Future
traits <- c("vegetative2_bacterialwilt", 
            "postharvest1_yield", 
            "postharvest1_tubersize",
            "postharvest1_marketableatharv",
            "postharvest2_marketableatstor",
            "postharvest2_taste")

items <- paste0("package_item_", LETTERS[1:3])

itemnames <- sort(unique(unlist(dat[items])))

# I may ignore some pre-ranking process
# for the sake of this example
pldat <- data.frame()

for(i in seq_along(traits)){
  
  # select the item names and rankings for the trait in the iteration i
  d_i <- dat[, c(items, paste0(traits[i], c("_pos", "_neg")))]
  # not observed as NA
  d_i[d_i == "Not observed"] <- NA
  # check for ties in the response pos == neg
  keep <- d_i[,4] != d_i[,5] & !is.na(d_i[,4]) & !is.na(d_i[,5])
  # keep only the TRUE values out of the later validation
  d_i <- d_i[keep, ]
  
  names(d_i)[4:5] <- c("pos", "neg")
  
  d_i$trait <- traits[i]
  
  pldat <- rbind(pldat, d_i)

}

pldat$trait[pldat$trait == "vegetative2_bacterialwilt"] <- "BacWilt"
pldat$trait[pldat$trait == "postharvest1_yield"] <- "Yield"
pldat$trait[pldat$trait == "postharvest1_tubersize"] <- "TuberSize"
pldat$trait[pldat$trait == "postharvest1_marketableatharv"] <- "MarkHarv"
pldat$trait[pldat$trait == "postharvest2_marketableatstor"] <- "MarkStorage"
pldat$trait[pldat$trait == "postharvest2_taste"] <- "Taste"

table(pldat$trait)


pldat$trait <- as.factor(pldat$trait)

# make the grouped rankings
pldat$G <- rankTricot(pldat, items = 1:3, input = 4:5, group = TRUE)

head(pldat)

str(pldat)

tree <- pltree(G ~ trait, data = pldat, alpha = 0.1, minsize = 50)

gosset:::plot_tree(tree)

top_items(tree, 3)

node_rules(tree)

# so then you can compute the worst regret (look at regret theory)
# and Table 3 here https://www.pnas.org/content/116/10/4194
# to assess the genotypes with best performance across traits
?worst_regret

wr <- worst_regret(tree)

wr






