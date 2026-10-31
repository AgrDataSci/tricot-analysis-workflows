# run packages
library("PlackettLuce")
library("gosset")
library("ClimMobTools")
library("ggplot2")
library("tibble")
source("script/helper_01_functions.R")
library("reshape2")
library("heatmap3")
library("ComplexHeatmap")
 
#recall the data
dat <- read.csv("data/potato-tricot-data-with-covariate_for_manuscript_QS.csv")


#check data structure
str(dat)
head(dat)
tail(dat)
summary(dat)
table(dat$id) # there are some Id duplicated 
which(table(dat$id) >1) #find duplicate!! 
#POT21B-113 POT21B-133  POT21B-91  These Id were duplicated. So, let's delete it!
del1<- which(dat$id=="POT21B-113")
del1 <-del1[[-1]] 
del2<- which(dat$id=="POT21B-133") 
del2 <-del2[[-1]] 
del3<- which(dat$id=="POT21B-91")  
del3 <-del3[[-1]] 



dat <- dat[-c(del1, del2, del3),] #data structure (460 obs. of 126 variables)
which(table(dat$id) > 1) #check data structure again. yeap! well applied.

#####################################################################################################
#######################################based on scenario2############################################
#######################################Quality scoring base##########################################

hist(dat$Total_sum_QS_point.without.BW, labels = TRUE)
hist(dat$Total_sum_QS_point.with.BW, labels = TRUE)
#based on "dat$Total_sum_QS_point.with.BW" I will deducted data that has bigger than 99.(there are 23 data set bigger equal or than 99. 99 means data might be biased.)


QS <- which(dat$Total_sum_QS_point.with.BW >98)
QS
dat <- dat[-c(QS),]
#437 farmers data left
#########################################################################
#checking how many data left in each trait 
a1<- sum(!is.na(dat$vegetative1_plantvigour_pos) | !is.na(dat$vegetative1_plantvigour_neg)) 
a2<-sum(!is.na(dat$vegetative1_bacterialwilt_pos) | !is.na(dat$vegetative1_bacterialwilt_neg)) 
a3<-sum(!is.na(dat$vegetative1_diseaseinsect_pos) | !is.na(dat$vegetative1_diseaseinsect_neg)) 
a4<-sum(!is.na(dat$vegetative2_bacterialwilt_pos) | !is.na(dat$vegetative2_bacterialwilt_neg)) 
a5<-sum(!is.na(dat$vegetative2_diseaseinsect_pos) | !is.na(dat$vegetative2_diseaseinsect_neg)) 
a6<-sum(!is.na(dat$postharvest1_maturity_pos) | !is.na(dat$postharvest1_maturity_neg)) 
a7<-sum(!is.na(dat$postharvest1_yield_pos) | !is.na(dat$postharvest1_yield_neg)) 
a8<-sum(!is.na(dat$postharvest1_tubersize_pos) | !is.na(dat$postharvest1_tubersize_neg)) 
a9<-sum(!is.na(dat$postharvest1_tuberappearance_pos) | !is.na(dat$postharvest1_tuberappearance_neg)) 
a10<-sum(!is.na(dat$postharvest1_marketableatharv_pos) | !is.na(dat$postharvest1_marketableatharv_neg))

a11<-sum(!is.na(dat$postharvest1_taste_pos) | !is.na(dat$postharvest1_taste_neg)) 
a12<-sum(!is.na(dat$postharvest1_marketableatstor_pos) | !is.na(dat$postharvest1_marketableatstor_neg)) 
a13<-sum(!is.na(dat$postharvest2_marketableatstor_pos) | !is.na(dat$postharvest2_marketableatstor_neg)) 
a14<-sum(!is.na(dat$postharvest2_dormancy_pos) | !is.na(dat$postharvest2_dormancy_neg))
a15<-sum(!is.na(dat$postharvest2_taste_pos) | !is.na(dat$postharvest2_taste_neg))
a16<-sum(!is.na(dat$postharvest3_dormancy_pos) | !is.na(dat$postharvest3_dormancy_neg))
a17<-sum(!is.na(dat$postharvest3_tuberquality_pos) | !is.na(dat$postharvest3_tuberquality_neg))
a18<-sum(!is.na(dat$postharvest3_overallper_pos) | !is.na(dat$postharvest3_overallper_neg))
a19<-sum(!is.na(dat$postharvest3_plantfuture_pos) | !is.na(dat$postharvest3_plantfuture_neg))
a20<-sum(!is.na(dat$postharvest3_preference_pos) | !is.na(dat$postharvest3_preference_neg))
tricot <- c(a1, a2, a3, a4, a5, a6, a7, a8,a9, a10, a11, a12, a13, a14, a15, a16,a17, a18, a19, a20)
tricot_name <- c("Plant vigor 1", "Bacterial wilt 1", "Diseases or insect resistance","Bacterial wilt 2 (75 days)", "Disease and  insect resistance 2 (75 days)", 
                 "maturity post-harvest 1", "post harvest yield","post harvest tuber size" , "post harvest tuber appearance " ," post harvest market able ","post harvest taste","post harvest_ marketable store","easiest to sell after storage (Day 45)","dromancy(long or short)  (Day 45)","taste (Day 45)","dormancy (Day 60)",
                 "tuber quality (Day 60)","overall performance  (Day 60)","plant for future  (Day 60)", "best overall  (Day 60)")
tricot <-cbind(tricot_name, tricot)
tricot<- as.data.frame(tricot)
tricot[,2] <- as.numeric(tricot[,2])

gt1<- ggplot(tricot, aes(x=tricot_name, y = tricot)) +  geom_bar(stat='identity')  + coord_cartesian(ylim = c(0, 500)) + theme(axis.text.x=element_text(angle=45, hjust=1)) + geom_text(aes(label=tricot), vjust=-1, size=3)
gt2 <- gt1 +scale_x_discrete(limits=c("Plant vigor 1", "Bacterial wilt 1", "Diseases or insect resistance","Bacterial wilt 2 (75 days)", "Disease and  insect resistance 2 (75 days)", 
                                      "maturity post-harvest 1", "post harvest yield","post harvest tuber size" , "post harvest tuber appearance " ," post harvest market able ","post harvest taste","post harvest_ marketable store","easiest to sell after storage (Day 45)","dromancy(long or short)  (Day 45)","taste (Day 45)","dormancy (Day 60)",
                                      "tuber quality (Day 60)","overall performance  (Day 60)","plant for future  (Day 60)", "best overall  (Day 60)"))

gt2

##############
#first check correlation between overall performance, overall preference and plan for future.  



traits <- c(
  "postharvest3_overallper",
  "postharvest3_plantfuture",
  "postharvest3_preference")

base_trait <- 3

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

Kendall_t<- kendall

kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall



##extract P-value
z <- list()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value}

p_value <- c(z[[1]],z[[2]])

kendall_add <- cbind(Kendall_t,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[1]], method="kendall")


#capture of potato plant vigour(45) tricot result 
capture.output(kendall_add,
               file ="output_thesis/kendall_overall_preference_performance_&_future.txt")


#####################
#1.5 check performance vs plant on future. 
############
#first check correlation between overall performance, overall preference and plan for future.  



traits <- c(  "postharvest3_preference",
              "postharvest3_plantfuture",
              "postharvest3_overallper")

base_trait <- 3

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

Kendall_t<- kendall

kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall

#capture of potato plant vigour(45) tricot result 
capture.output(kendall,
               file ="output_thesis/kendall_overall_performance_&_future.txt")



##extract P-value
z <- list()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(Kendall_t,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[1]], method="kendall")




############################################
#Second, check correlation between overall preference and other traits.  



traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest3_preference")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

Kendall_t<- kendall

kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall

k_overallpreference <- kendall






##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(Kendall_t,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[16]], method="kendall")


#capture of potato plant vigour(45) tricot result 
capture.output(kendall_add,
               file ="output_thesis/kendall_overall_preference vs others.txt")

#########################
#third, check correlation between overall performance and other traits.  



traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_plantfuture",
            "postharvest3_preference",
            "postharvest3_overallper")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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


Kendall_t<- kendall

kendall


kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall

k_overallperformance <- kendall


##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(Kendall_t,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[1]], method="kendall")



#capture of potato plant vigour(45) tricot result 
capture.output(kendall_add,
               file ="output_thesis/kendall_overall_performance vs others.txt")
###############################################################################
#forth, check correlation between plant for future and other traits.  




traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture")

base_trait <- 20


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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

Kendall_t<- kendall

kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall

k_future <- kendall


##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(Kendall_t,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[1]], method="kendall")



#capture of potato plant vigour(45) tricot result 
capture.output(kendall_add,
               file ="output_thesis/kendall_plant for future vs others.txt")

########################################################################
#######################################################################3
#creating heat map 
#postharvest3_tuberquality



traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest3_tuberquality")

base_trait <- 20
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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

Kendall_t<- kendall

kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall

k_tubarqual_3 <- kendall

##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(Kendall_t,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[1]], method="kendall")




#capture of potato tuber quality 
capture.output(kendall_add,
               file ="output_thesis/kendall_postharvest3_tuberquality vs others.txt")


#######################################################################3
#creating heat map 
#postharvest3_dormancy


traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest3_dormancy")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

Kendall_t<- kendall

kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall

k_dormancy_3 <- kendall


##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(kendall,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[1]], method="kendall")


#capture of potato tuber dormancy 
capture.output(kendall_add,
               file ="output_thesis/kendall_postharvest3_dormancy vs others.txt")

##############################################
#######################################################################3
#creating heat map 
#postharvest2_taste


traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest2_taste")

base_trait <- 20
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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

k_postharvest2_taste <- kendall

#################################################
######################################################################3
#creating heat map 
#postharvest2_dormancy


traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest2_dormancy")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_dormancy2 <- kendall


##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(kendall,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[1]], method="kendall")

######################################################################3
#creating heat map 
#postharvest2_marketableatstor


traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest2_marketableatstor")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

kendall_add <- kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall

K_marketableatstor2<- kendall

##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(kendall_add,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[1]], method="kendall")


######################################################################3
#creating heat map 
#postharvest1_marketableatstor


traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest1_marketableatstor")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_marketableatstor1<- kendall

######################################################################3
#creating heat map 
#postharvest1_taste


traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest1_taste")

base_trait <- 20


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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_taste1<- kendall

##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(kendall,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[1]], method="kendall")


##############################################
######################################################################3
#creating heat map 
#postharvest1_marketableatharv


traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest1_marketableatharv")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_marketableatharv<- kendall

######################################################################3
#creating heat map 
#postharvest1_tuberappearance


traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest1_tuberappearance")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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
kendall_add <- kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]

kendall

K_tuberappearance1<- kendall




##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(kendall_add,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[11]], method="kendall")

######################################################################3
#creating heat map 
#postharvest1_tubersize



traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest1_tubersize")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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
Kendall_t<- kendall

kendall <- kendall[rev(order(kendall$kendallTau)), ]


kendall


K_tubersize1<- kendall


##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(Kendall_t,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[11]], method="kendall")


######################################################################3
#creating heat map 
#postharvest1_yield



traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest1_yield")

base_trait <- 20


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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_yield1<- kendall


##extract P-value
z <- list()
p_value<- vector()
for( i in seq_along(coeffs_trait)){outcome <- cor.test(coeffs_base, coeffs_trait[[i]], method="kendall")
z[[i]]<- outcome$p.value
p_value[i] <- z[[i]]}

kendall_add <- cbind(kendall,p_value)

kendall_add

cor.test(coeffs_base, coeffs_trait[[2]], method="kendall")




######################################################################3
#creating heat map 
#postharvest1_maturity



traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "postharvest1_maturity")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_maturity1<- kendall

######################################################################3
#creating heat map 
#vegetative2_diseaseinsect


traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_bacterialwilt",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "vegetative2_diseaseinsect")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_diseaseinsect2<- kendall
######################################################################3
#creating heat map 
#vegetative2_bacterialwilt



traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "vegetative2_bacterialwilt")

base_trait <- 20

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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_bacterialwilt2<- kendall
######################################################################3
#creating heat map 
#vegetative1_diseaseinsect



traits <- c("vegetative1_plantvigour", 
            "vegetative1_bacterialwilt", 
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "vegetative1_diseaseinsect")

base_trait <- 20


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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_diseaseinsect1<- kendall
######################################################################3
#creating heat map 
#vegetative1_bacterialwilt

traits <- c("vegetative1_plantvigour", 
            "vegetative1_diseaseinsect", 
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "vegetative1_bacterialwilt")

base_trait <- 20
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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_bacterialwilt1<- kendall
##############################################################
######################################################################3
#creating heat map 
#vegetative1_plantvigour

traits <- c("vegetative1_bacterialwilt", 
            "vegetative1_diseaseinsect", 
            "vegetative2_bacterialwilt",
            "vegetative2_diseaseinsect",
            "postharvest1_maturity",
            "postharvest1_yield",
            "postharvest1_tubersize",
            "postharvest1_tuberappearance",
            "postharvest1_marketableatharv",
            "postharvest1_taste",
            "postharvest1_marketableatstor",
            "postharvest2_marketableatstor",
            "postharvest2_dormancy",
            "postharvest2_taste",
            "postharvest3_dormancy",
            "postharvest3_tuberquality",
            "postharvest3_preference",
            "postharvest3_overallper",
            "postharvest3_plantfuture",
            "vegetative1_plantvigour")

base_trait <- 20
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
  keep <- !is.na(d_i[,4]) | !is.na(d_i[,5]) 
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

K_plantvigor<- kendall

###########################
##############################################################3
#short and making data frame

K_plantvigor <- K_plantvigor[,c(3,1)]
K_bacterialwilt1 <- K_bacterialwilt1[,c(3,1)]
K_diseaseinsect1 <- K_diseaseinsect1[,c(3,1)]
K_bacterialwilt2 <- K_bacterialwilt2[,c(3,1)]
K_diseaseinsect2 <- K_diseaseinsect2[,c(3,1)]
K_maturity1 <- K_maturity1[,c(3,1)]
K_yield1 <- K_yield1[,c(3,1)]
K_tubersize1 <- K_tubersize1[,c(3,1)]
K_marketableatharv <- K_marketableatharv[,c(3,1)]
K_tuberappearance1 <- K_tuberappearance1[,c(3,1)]
K_marketableatstor1 <- K_marketableatstor1[,c(3,1)]
K_taste1 <- K_taste1[,c(3,1)]
K_marketableatstor2 <- K_marketableatstor2[,c(3,1)]
K_dormancy2 <- K_dormancy2[,c(3,1)]
k_postharvest2_taste <- k_postharvest2_taste[,c(3,1)]
k_dormancy_3 <- k_dormancy_3[,c(3,1)]
k_tubarqual_3 <- k_tubarqual_3[,c(3,1)]
k_overallperformance <- k_overallperformance[,c(3,1)]
k_future <-k_future[,c(3,1)]
k_overallpreference<- k_overallpreference[,c(3,1)]


#make data frame 

k_overallpreference
#create oneself value
k_overallpreference <- add_row(k_overallpreference, trait =  "postharvest3_preference", kendallTau = 1)

m_trait <- merge(k_overallpreference, k_future,  by="trait", all = T )
m_trait <- merge(m_trait, k_overallperformance,  by="trait", all = T )
m_trait <- merge(m_trait, k_tubarqual_3,  by="trait", all = T )
m_trait <- merge(m_trait, k_dormancy_3,  by="trait", all = T )
m_trait <- merge(m_trait, k_postharvest2_taste,  by="trait", all = T )
m_trait <- merge(m_trait, K_dormancy2,  by="trait", all = T )
m_trait <- merge(m_trait, K_marketableatstor2,  by="trait", all = T )
m_trait <- merge(m_trait, K_marketableatstor1,  by="trait", all = T )
m_trait <- merge(m_trait, K_taste1,  by="trait", all = T )
m_trait <- merge(m_trait, K_marketableatharv,  by="trait", all = T )
m_trait <- merge(m_trait, K_tuberappearance1,  by="trait", all = T )
m_trait <- merge(m_trait, K_tubersize1,  by="trait", all = T )
m_trait <- merge(m_trait, K_yield1,  by="trait", all = T )
m_trait <- merge(m_trait, K_maturity1,  by="trait", all = T )
m_trait <- merge(m_trait, K_diseaseinsect2,  by="trait", all = T )
m_trait <- merge(m_trait, K_bacterialwilt2,  by="trait", all = T )
m_trait <- merge(m_trait, K_diseaseinsect1,  by="trait", all = T )
m_trait <- merge(m_trait, K_bacterialwilt1,  by="trait", all = T )
m_trait <- merge(m_trait, K_plantvigor,  by="trait", all = T )


m_trait


names(m_trait) <- c("trait","postharvest3_preference",  "postharvest3_plantfuture", "postharvest3_overallper", "postharvest3_tuberquality","postharvest3_dormancy","postharvest2_taste",
                    "postharvest2_dormancy","postharvest2_marketableatstor", "postharvest1_marketableatstor", "postharvest1_taste",
                    "postharvest1_marketableatharv", "postharvest1_tuberappearance","postharvest1_tubersize","postharvest1_yield",
                    "postharvest1_maturity","vegetative2_diseaseinsect", "vegetative2_bacterialwilt","vegetative1_diseaseinsect",
                    "vegetative1_bacterialwilt", "vegetative1_plantvigour") 

tt<- m_trait



tt[which(is.na(tt[,3])), 3]  <- 1
tt[which(is.na(tt[,4])), 4] <- 1
tt[which(is.na(tt[,5])), 5] <- 1
tt[which(is.na(tt[,6])), 6] <- 1
tt[which(is.na(tt[,7])), 7] <- 1
tt[which(is.na(tt[,8])), 8] <- 1
tt[which(is.na(tt[,9])), 9] <- 1
tt[which(is.na(tt[,10])), 10] <- 1
tt[which(is.na(tt[,11])), 11] <- 1
tt[which(is.na(tt[,12])), 12] <- 1
tt[which(is.na(tt[,13])), 13] <- 1
tt[which(is.na(tt[,14])), 14] <- 1
tt[which(is.na(tt[,15])), 15] <- 1
tt[which(is.na(tt[,16])), 16] <- 1
tt[which(is.na(tt[,17])), 17] <- 1
tt[which(is.na(tt[,18])), 18] <- 1
tt[which(is.na(tt[,19])), 19] <- 1
tt[which(is.na(tt[,20])), 20] <- 1
tt[which(is.na(tt[,21])), 21] <- 1




tt <-tt[c(14, 13, 12, 15,11, 10,8,9,2,4,1,5,6,7,3,20,19,17,16,18),]


#rname<-c("Plant vigour VG1"      ,        "BW resistance VG1",    
#         "Disease & insect VG1","BW resistance VG2",    
#         "Disease & insect VG2",         "Murity PH1",        
#         "Yield PH1",        "Tuber sizea PH1",       
#         "Tuber appearance PH1",        "MAH PH1",
#         "Taste PH1",       "MAS PH1",
#         "MAS PH2"                    ,      "Dormancy PH2",        
#         "Taste PH2",     "Dormancy PH3",        
#         "Tuber quality PH3","Overall perfromance",  "WPF",    "Overall preference")


rname<-c("Plant vigour VG1",        "Bacterial wilt (BW) resistance VG1",    
         "Disease & insect VG1","Bacterial wilt (BW) resistance VG2",    
         "Disease & insect VG2",         "Murity PH1",        
         "Yield PH1",        "Tuber sizea PH1",       
         "Tuber appearance PH1",        "Marketable after harvest (MAH) PH1",
         "Taste PH1",       "Marketable after store (MAS) PH1",
         "Marketable after store (MAS) PH2"                    ,      "Dormancy PH2",        
         "Taste PH2",     "Dormancy PH3",        
         "Tuber quality PH3","Overall perfromance",  "Willing to plants in the future (WPF)",    "Overall preference")

rname <- rev(rname)
rownames(tt)<- rname
tt <-tt[,-1]
colnames(tt) <- rname

km_tttt<- data.matrix(tt)

col<- colorRampPalette(c("red", "white", "blue"))(256)
Heatmap(km_tttt, name = "Kendall",col =  col, column_names_gp = grid::gpar(fontsize = 15),
        row_names_gp = grid::gpar(fontsize = 15)) 


png(filename = "output_thesis/heatmap_full_dial_about_kendall_tau_and_dendorgram_threeoassessment_ver2.png", width = 40, height = 40, units = "cm", res = 400)
Heatmap(km_tttt, name = "Kendall",col =  col,column_names_gp = grid::gpar(fontsize = 17),
        row_names_gp = grid::gpar(fontsize = 17)) 

dev.off()



png(filename = "output_thesis/heatmap_full_dial_about_kendall_tau_and_dendorgram_threeoassessment.png", width = 40, height = 40, units = "cm", res = 400)
Heatmap(km_tttt, name = "Kendall",col =  col)
dev.off()