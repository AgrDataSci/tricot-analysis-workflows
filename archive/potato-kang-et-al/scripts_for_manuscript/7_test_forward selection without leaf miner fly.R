### Call packages
library("PlackettLuce")
library("qvcalc")
library("gosset")
library("patchwork")
library("ggparty")
library("ggplot2")
library("igraph")
library("leaflet")
#source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/R/functions.R")
library("ClimMobTools")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/b842039ab35583df9961025dde9ec1dcc83d5095/R/functions.R")
library("caret")
library("abind")
library("foreach")
library("parallel")
library("partykit")
library("ggpubr")
###



#### part of function commend for selecting best covariates for less deviance of plackett luce model.  

#' Combine parallel outputs
#' @param ... arguments passed to methods
#' @noRd
.comb <- function(...) {
  abind::abind(..., along = 1, force.array = TRUE)
}

#' Model call for parallel
#' @param formula a symbolic description of the model, if set as   
#'   \eqn{ y ~ . } all variables in \code{data} are used 
#' @param args a list with model call arguments 
#' @param estimator the name of estimators to select
#' @noRd
.forward_dopar <- function(formula, args){
  
  args <- c(formula, args)
  
  m <- do.call(gosset::crossvalidation, args)
  
  result <- m$raw$estimators
  
  nfold <- m$raw$k
  
  result  <- array(unlist(result), c(1, nfold, ncol(result)))
  
  return(result)
}
#############
####


#call csv data to make data.frame

dt <- read.csv("data/potato-tricot-data-with-covariate_for_manuscript_QS.csv")
tuber_blup <- read.csv("data/on_station-covariate_season 1_converted.csv")
tuber_blup<- tuber_blup[,-3] #remove plant-uniformity because data quality issue. 
##remove leaf miner fly from tuber_blup
tuber_blup <- tuber_blup[,-7]
str(tuber_blup)

#change disease variable names to disease occurrence
#names(tuber_blup)[6:9] <- c("late_blight_occurrence","Leaf_miner_flies_occurrence" ,"viruses_occurrence",  "bacterial_wilt_occurrence")  
names(tuber_blup)[6:8] <- c("late_blight_occurrence" ,"viruses_occurrence",  "bacterial_wilt_occurrence")  

names(tuber_blup)

#check data structure (458 obs. of 160 variables) becuase 5 data sets did not have gps and so I deducted these data. 
#35 variables
#check data structure (458obs of 160 variables)
str(dt)
head(dt)
tail(dt)
summary(dt)
table(dt$id) # check duplicated Ids 
which(table(dt$id) >1) #find duplicate!! 
#POT21B-113 POT21B-133  POT21B-91  These Id were duplicated. So, let's delete it!
del1<- which(dt$id=="POT21B-113")
del1 <-del1[[-1]] 
del2<- which(dt$id=="POT21B-133") 
del2 <-del2[[-1]] 
del3<- which(dt$id=="POT21B-91")  
del3 <-del3[[-1]] 


dt <- dt[-c(del1, del2, del3),] #data structure (455 obs. of 160 variables)
which(table(dt$id) > 1) #check data structure again. yeap! well applied.
#####################################################################################################
#######################################Quality scoring base##########################################

hist(dt$Total_sum_QS_point.without.BW, labels = TRUE)
hist(dt$Total_sum_QS_point.with.BW, labels = TRUE)
#based on "dt$Total_sum_QS_point.with.BW" I will deducted data that has bigger than 99.(there are 23 data set bigger equal or than 99. 99 means data might be biased.)


QS <- which(dt$Total_sum_QS_point.with.BW >98)
QS
dt <- dt[-c(QS),]

################################setting before starting plackett luce ##############################################33
#set item varieties as list for an easy way!!  
variety <- c("package_item_A", "package_item_B", "package_item_C")
# using reference variety "Kirundo" because it has been released since 1989.
ref = "Kirundo"
#except BW resistance because cruza showed the best bacterial resistance and so its more easy to compare the others 
# environment covariate 
env_cov <- c("MaxDT"      ,                      "MinDT"           ,                 "MaxNT"  ,                         
             "MinNT"       ,                     "DTR"              ,                "SU"      ,                        
             "TR"           ,                    "CFD"               ,               "WSDI"     ,                       
             "CSDI"          ,                   "T10p"               ,              "T90p"      ,                      
             "GDD"            ,                  "CDI_mean_22"         ,             "CDI_mean_23"  ,                   
             "CDI_mean_24"     ,                "Totalsum_rainfall",               
             "Average_rainfall"    ,                "sum_no_rain"           ,                      
             "Soil.organic.carbon",              "Cation.exchange.capacity.at.ph7." ,"Nitrogen"           ,             
             "PH_water"            ,             "Soil_classification")   



rhomis_cov <- c("RHoMIS_id"               ,          "Respondent_gender"           ,                
                "True_householdheader"      , 
                "Education_level"         ,          "Household_type"      ,             
                "Count_people"               ,       "Landcultivated"           ,         
                "Landowned"            ,              "Land_ownership"               ,     
                "Farm_labour_b"         ,           
                "Potato_important"             ,     "Agriculture_intensification_level", "Asset_Count"            ,          
                "Crop_Diversity"                ,    "Ag_Input_Diversity"       ,         "Livestock_Diversity"     ,         
                "Livestock_TLU_Score"            ,   "Hunger_months"     ,          "Crop_Income_USD"          ,        
                "Livestock_Income_USD"            ,  "Farm_Income_USD")    



#######################################change_character_to_factors#########################################################
dt$season<- as.factor(dt$season)
dt$Soil_classification<- as.factor(dt$Soil_classification)
dt$RHoMIS_id<- as.factor(dt$RHoMIS_id)
dt$Respondent_gender <- as.factor(dt$Respondent_gender)
dt$Education_level <- as.factor(dt$Education_level)
dt$True_householdheader<- as.factor(dt$True_householdheader)
dt$Household_type<- as.factor(dt$Household_type)
dt$Landowned <- as.factor(dt$Landowned)
dt$Land_ownership <- as.factor(dt$Land_ownership)
dt$Farm_labour <- as.factor(dt$Farm_labour)
dt$Soil_classification_vg2<- as.factor(dt$Soil_classification_vg2)


###test
dt$Potato_important <- ifelse(dt$Potato_important == 0, "No", "Yes")
dt$Potato_important <-  as.factor(dt$Potato_important)




###########################Vegetative 1 (Day 45) stage###############################################
#####################################################################################
###########################Vegetative 1 (Day 45) stage################################

which(is.na(dt[,"vegetative1_survey_start"]))
dt1 <- dt[] #"21AIP-8" house hold did not participate in vegetative 1 stage. However, in here, this case will be automatically deducted 
#because the case that farmers answered none both: bast and worst will be diminished before calculating the Plackett luce model. 
########################################################################################33
#1###tricot into a PlackettLuce rankings about plant vigor during Vegetative 1 (Day 45)

trait_vigour_v1 <- c("vegetative1_plantvigour_pos","vegetative1_plantvigour_neg")

## checking NA in Columes 


is.na(dt1[,c("vegetative1_plantvigour_pos","vegetative1_plantvigour_neg")])
sum(is.na(dt1[,c("vegetative1_plantvigour_pos","vegetative1_plantvigour_neg")])) # number of NA 2.

#delete some farmers who did not respond all qeustions:positive & negative 

is.na(dt1[,"vegetative1_plantvigour_pos"]) & is.na(dt1[,"vegetative1_plantvigour_neg"])

sum(is.na(dt1[,"vegetative1_plantvigour_pos"]) & is.na(dt1[,"vegetative1_plantvigour_neg"])) # 1 farmers didn't respond both questions

which(is.na(dt1[,"vegetative1_plantvigour_pos"]) & is.na(dt1[,"vegetative1_plantvigour_neg"]))
keep1 <- dt1[]


# vector to kepp only the ones with at least one reply
keep_vigour_v1 <- apply(keep1[trait_vigour_v1], 1, function(x){
  sum(is.na(x))})

keep_vigour_v1 <- keep_vigour_v1 <= 1

R1 <-  rankTricot(keep1[keep_vigour_v1,], 
                  items = variety,
                  input = trait_vigour_v1)

mod1 <- PlackettLuce(R1)

# using reference variety "Kirundo" because it has been released since 1989.
ref = "Kirundo"

summary(mod1, ref) # Here we can see all Coefficient results

coef(mod1, ref)
coef(mod1, log = FALSE)
plot(qvcalc(mod1, ref = ref))
plot(qvcalc(mod1, ref = ref), main = "Plant vigour(Day 45) trait quasi standard ") # result of plant vigour trait 


# capture PNG "output_thesis/plant vigour(Day 45) trait Quasi standard.png"
png(filename = "output_thesis/quasi standard_error plant vigour(Day 45) trait.png", width = 30, height = 15, units = "cm", res = 400)
plot(qvcalc(mod1, ref = ref), main = "Plant vigour(Day 45) trait quasi standard error") 
dev.off()


#capture of potato plant vigour(45) tricot result 
capture.output(summary(mod1, ref), 
               print("model coefficients(plant.vigour45days)"),
               coef(mod1),
               file ="output_thesis/summary_mod_Coef_potato_vigour45days.txt")




# Let see the network (how these varieties are connected) 
# Here command does not work
#a <- adjacency(R1)
#a <- network(a)
#plot(a, main = "varieties network_plant vigour(Day 45) trait network")



# worth plot about plant vigor 

plot_worth(mod1, main = "varieties plant vigour(Day 45) trait worth")


png(filename = "output_thesis/worth plot about plant vigour (Day 45).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod1) + theme_bw() +theme(panel.grid = element_blank())
dev.off()



####################################################################
#2.#######tricot into a PlackettLuce rankings about bacterial wilt (Day 45)

trait_bw_v1 <- c("vegetative1_bacterialwilt_pos","vegetative1_bacterialwilt_neg")

#checking NA in columns 
na_check <- is.na(dt1[trait_bw_v1])
summary(na_check) #however, there are "Not observed" data. lets change "Not observed" to NA

keep2_NA<- dt1[]
keep2_NA[keep2_NA=="Not observed"] <- NA # change Not observed  to NA.
keep2_NA

keep2_NA[trait_bw_v1]
is.na(keep2_NA[trait_bw_v1])
summary(is.na(keep2_NA[trait_bw_v1]))


is.na(keep2_NA[,"vegetative1_bacterialwilt_pos"]) & is.na(keep2_NA[,"vegetative1_bacterialwilt_neg"])
sum(is.na(keep2_NA[,"vegetative1_bacterialwilt_pos"]) & is.na(keep2_NA[,"vegetative1_bacterialwilt_neg"])) 
# 244 farmers didn't respond both questions


keep2_NA<- keep2_NA[!is.na(keep2_NA$vegetative1_bacterialwilt_pos) | !is.na(keep2_NA$vegetative1_bacterialwilt_neg),] # ramaining 188 farmers data from farmers data
keep2_NA[trait_bw_v1]
summary(keep2_NA[trait_bw_v1]) 

keep2_1 <- keep2_NA[]

# vector to keep only the ones with at least one reply
keep_bw1  <- apply(keep2_1[trait_bw_v1], 1, function(x){
  sum(is.na(x))
})

# PlackettLuce rankings about bacterial wilt (Day 45)
keep_bw1 <- keep_bw1 <= 1

R_bw1 <- rankTricot(keep2_1[keep_bw1,], 
                    items = variety,
                    input = trait_bw_v1)

mod_bw1 <- PlackettLuce(R_bw1)

summary(mod_bw1)


ref = "Kirundo" #using reference "Kirundo" for comparing 

summary(mod_bw1, ref = ref) # summary  coefficients about bacterial wilt resistance ranking by using refence "Kirundo"  
coef(mod_bw1, ref = ref)


capture.output(summary(mod_bw1, ref = ref), 
               print("model coefficients( bacterial wilt resistance 45days)"),
               coef(mod_bw1, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_ bacterial wilt resistance(45days).txt")




# the network checking (how these varieties are connected) 
#a_bw1 <- adjacency(R_bw1)
#a_bw1 <- network(a_bw1)
#plot(a_bw1)




coef(mod_bw1, ref = ref)
coef(mod_bw1, log = FALSE)
plot(qvcalc(mod_bw1, ref = ref))


png(filename = "output_thesis/quasi_standard_error bacterial wilt resistance(Day 45).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_bw1, ref = ref), main = "bacterial wilt resistance(Day 45)quasi_standard_error")
dev.off()

#worth plot about bacterial wilt
plot_worth(mod_bw1)

png(filename = "output_thesis/worth plot _bacterial_wilt_resistance_(Day 45).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_bw1)+ theme_bw() +theme(panel.grid = element_blank())
dev.off()


###
#3.#################################################################################
###tricot into a PlackettLuce rankings about diseases or insect resistance  (Day 45) 

trait_DI_v1 <- c("vegetative1_diseaseinsect_pos","vegetative1_diseaseinsect_neg")

#checking NA in columne 

na_check2 <- is.na(dt1[,trait_DI_v1])
summary(na_check2) #however, there are "Not observed" data. lets change "Not observed" to NA
dt1[,trait_DI_v1]
keep3dt_NA<- dt1[]
keep3dt_NA[keep3dt_NA=="Not observed"] <- NA # change Not observed  to NA.
keep3dt_NA[,trait_DI_v1]
#delete some farmers who did not respond all questions:positive & negative 

is.na(keep3dt_NA[,"vegetative1_diseaseinsect_pos"]) & is.na(keep3dt_NA[,"vegetative1_diseaseinsect_neg"])
sum(is.na(keep3dt_NA[,"vegetative1_diseaseinsect_pos"]) & is.na(keep3dt_NA[,"vegetative1_diseaseinsect_neg"]))
# 316 farmers did not respond both questions:positive & negative 


subset(keep3dt_NA[,trait_DI_v1],is.na(keep3dt_NA[,"vegetative1_diseaseinsect_pos"]) & is.na(keep3dt_NA[,"vegetative1_diseaseinsect_neg"]))
summary(subset(keep3dt_NA[,trait_DI_v1],is.na(keep3dt_NA[,"vegetative1_diseaseinsect_pos"]) & is.na(keep3dt_NA[,"vegetative1_diseaseinsect_neg"])))

keep3dt_NA<- keep3dt_NA[!is.na(keep3dt_NA$vegetative1_diseaseinsect_pos) | !is.na(keep3dt_NA$vegetative1_diseaseinsect_neg),] # delete 341 farmers data from 462 farmers data

keep3dt_NA[trait_DI_v1]
summary(keep3dt_NA[trait_DI_v1]) #116 farmers' data remained  


keep_DI_v1<- apply(keep3dt_NA[,trait_DI_v1], 1, function(x){
  sum(is.na(x))})

keep_DI_v1<- keep_DI_v1 <= 1

R3 <-  rankTricot(keep3dt_NA[keep_DI_v1,], 
                  items = variety,
                  input = trait_DI_v1)

R3

mod3 <- PlackettLuce(R3)

summary(mod3, ref = ref)



capture.output(summary(mod3, ref = ref), 
               print("model coefficients(diseases or insect resistance(Day 45))"),
               coef(mod3, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_diseases or insect resistance(Day 45)_45days.txt")



coef(mod3, ref = ref)
coef(mod3, log = FALSE)
plot(qvcalc(mod3, ref = ref))


png(filename = "output_thesis/quasi_standard_error potato_diseases or insect resistance(Day 45).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod3, ref = ref), main = "potato_diseases or insect resistance(Day 45)quasi_standard_error")
dev.off()


# the network checking (how these varieties are connected) 
#a_di1 <- adjacency(R3)
#a_di1 <- network(a_di1)
#plot(a_di1)


#worth plot about diseases or insect resistance
plot_worth(mod3)


png(filename = "output_thesis/worth plot about diseases or insect resistance(Day 45).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()



###########################Vegetative 2 (Day 75) stage################################
#during Vegetative 2 (Day 75) stage, farmers respond their tricot field condition whether they succeeded or not to keep continue tricot trial.   

which(is.na(dt[,"vegetative2_survey_start"]))
dt2 <- dt[]

############################################################################

#4.###################################################################################
#### tricot into a PlackettLuce rankings about bacterial wilt 2 (75 days)
trait_bw_v2 <- c("vegetative2_bacterialwilt_pos","vegetative2_bacterialwilt_neg")


#checking NA in columne 
na_check <- is.na(dt2[trait_bw_v2])
summary(na_check) #however, there are "Not observed" data. lets change "Not observed" to NA

keep4dt_NA<- dt2[]
keep4dt_NA[keep4dt_NA=="Not observed"] <- NA # change Not observed  to NA.
keep4dt_NA

keep4dt_NA[trait_bw_v2]
is.na(keep4dt_NA[trait_bw_v2])
summary(is.na(keep4dt_NA[trait_bw_v2]))



#delete some farmers who did not respond all questions:positive & negative 

is.na(keep4dt_NA[,"vegetative2_bacterialwilt_pos"]) & is.na(keep4dt_NA[,"vegetative2_bacterialwilt_neg"])
sum(is.na(keep4dt_NA[,"vegetative2_bacterialwilt_pos"]) & is.na(keep4dt_NA[,"vegetative2_bacterialwilt_neg"]))
# 83 farmers did not respond both questions:positive & negative 


subset(keep4dt_NA[,trait_bw_v2],is.na(keep4dt_NA[,"vegetative2_bacterialwilt_pos"]) & is.na(keep4dt_NA[,"vegetative2_bacterialwilt_neg"]))
summary(subset(keep4dt_NA[,trait_bw_v2],is.na(keep4dt_NA[,"vegetative2_bacterialwilt_pos"]) & is.na(keep4dt_NA[,"vegetative2_bacterialwilt_neg"])))

keep4dt_NA<- keep4dt_NA[!is.na(keep4dt_NA$vegetative2_bacterialwilt_pos) | !is.na(keep4dt_NA$vegetative2_bacterialwilt_neg),] # delete 83 farmers data 
summary(is.na(keep4dt_NA[trait_bw_v2]))#349 farmers remained 



# vector to keep only the ones with at least one reply
keep4dt_bw2  <- apply(keep4dt_NA[trait_bw_v2], 1, function(x){
  sum(is.na(x))
})


# PlackettLuce rankings about bacterial wilt 2 (Day 75)
keep4dt_bw2 <- keep4dt_bw2 <= 1

R_bw2 <- rankTricot(keep4dt_NA[keep4dt_bw2,], 
                    items = variety,
                    input = trait_bw_v2)

mod_bw2 <- PlackettLuce(R_bw2)

summary(mod_bw2)

coef(mod_bw2, log = F)


summary(mod_bw2, ref = "Cruza") # summary  coefficients about bacterial wilt resistance ranking by using refence "Kirundo"  


capture.output(summary(mod_bw2,  ref = "Cruza"), 
               print("model coefficients( bacterial wilt resistance 75 days)"),
               coef(mod_bw2,  ref = "Cruza"),
               file ="output_thesis/summary_mod_Coef_potato_ bacterial wilt resistance 2 (75days).txt")





coef(mod_bw2,  ref = "Cruza")
coef(mod_bw2, log = FALSE)
plot(qvcalc(mod_bw2,  ref = "Cruza"))


png(filename = "output_thesis/quasi_standard_error_bacterial wilt resistance 2 (Day 75).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_bw2,  ref = "Cruza"), main = "bacterial wilt resistance 2 (Day 75)quasi_standard_error")
dev.off()


#worth plot about bacterial wilt during vegtative stage 2 

plot_worth(mod_bw2) 

plot_worth(mod_bw2) + theme_bw() +theme(panel.grid = element_blank()) +  theme(text = element_text(size = 16))  + labs(y = "Varieties", x = "The probability of winning (worth)")

png(filename = "output_thesis/worth plot_bacterial_wilt_resistance 2_(Day 75)_ver2.png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_bw2) + theme_bw() +theme(panel.grid = element_blank()) +  theme(text = element_text(size = 16))  + labs(y = "Varieties", x = "The probability of winning (worth)")
dev.off()



png(filename = "output_thesis/worth plot_bacterial_wilt_resistance 2_(Day 75).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_bw2) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

####4-1plackett luce tree with environment and soil covariate data 


#almost same above code but here use grop = true.
G <-  rankTricot(keep4dt_NA[keep4dt_bw2,], 
                 items = variety,
                 input = trait_bw_v2,
                 group = TRUE)

# covariates

covar <- keep4dt_NA[keep4dt_bw2,c("MaxDT_vg2",
                                  "MinDT_vg2",
                                  "MaxNT_vg2",
                                  "MinNT_vg2",
                                  "DTR_vg2",
                                  "SU_vg2",
                                  "TR_vg2",
                                  "CFD_vg2",
                                  "WSDI_vg2",
                                  "CSDI_vg2",
                                  "T10p_vg2",
                                  "T90p_vg2",
                                  "GDD_vg2",
                                  "CDI_mean_22_vg2",
                                  "CDI_mean_23_vg2",
                                  "CDI_mean_24_vg2",
                                  "Totalsum_rainfall_vg2",
                                  "Average_rainfall_vg2",
                                  "Sum_no_rain_vg2",
                                  "Soil.organic.carbon_vg2",
                                  "Cation.exchange.capacity.at.ph7._vg2",
                                  "Nitrogen_vg2",
                                  "PH_water_vg2",
                                  "Soil_classification_vg2")]


# rankings 
out_r <- caret::nearZeroVar(covar)

names(covar)[out_r]

covar <- covar[, -out_r]

colnames(covar)

pld <- cbind(G, covar)

head(pld)

pld$Soil_classification <- as.factor(pld$Soil_classification)
# run a forward selection with cross-validation
# to select the best model
# Define initial parameters 

# set the baseline as a deviance from a Plackett-Luce model without covariates
baseline <- deviance(PlackettLuce(G))
# vector to keep best explanatory variables
covar_keep <- character(0L)
# keep running if TRUE
best <- TRUE
# number of runs
counter <- 1
# a list to keep the goodness-of-fit coefficients from each step
modelcoeffs <- list()
# get the names of explanatory and response variables
covarnames <- names(covar)
siglevel <- 0.1
minsize <- 25
npseudo <- 0.8
bonferroni <- TRUE
gamma <- TRUE

# keep running until the model get its best performance
while(best){
  
  message("Run " , counter, "\n")
  
  fs <- length(covarnames)
  
  args <- list(formula = NULL,
               data = pld,
               alpha = siglevel,
               npseudo = npseudo,
               bonferroni = bonferroni,
               gamma = gamma,
               minsize = minsize)
  
  deviances <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste0("G ~ ", paste(c(covar_keep, covarnames[i]), collapse = " + ")))
    args$formula <- f_i
    pl_i <- do.call("pltree", args)
    deviances <- c(deviances, deviance(pl_i))
  }
  
  best_dv <- which.min(deviances)
  
  value_best <- min(deviances)
  
  # take the name of best variable
  best_model <- covarnames[best_dv]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("G ~ ", paste(paste(covar_keep, collapse = " "), covarnames))
  call_m <- data.frame(cbind(call = call_m, deviance = deviances))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  if(best){
    
    # remove best variable for the next run
    covarnames <- covarnames[!covarnames %in% best_model]
    
    # keep this model for the next run
    covar_keep <- c(covar_keep, best_model)
    
    message("Best model found: ",
            paste0("G ~ ", paste(covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(covarnames) == 0) {
    best <- FALSE
  }
  
}


covar_keep

f <- formula(paste0("G ~ ", paste(covar_keep, collapse = " + ")))

f

tree <- pltree(f, 
               data = pld, 
               minsize = minsize, 
               alpha = 0.07, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

tree

plot(tree)


gosset:::plot_tree(tree)

png(filename = "output_thesis/Plackett-Luce tree bcaterial_wilt (Day 75)with enivornment covariates.png", width = 40, height = 38, units = "cm", res = 400)
plot(tree)
dev.off()

gosset:::plot_tree(tree)

png(filename = "output_thesis/Plackett-Luce tree bcaterial_wilt (Day 75)with enivornment_gosset.png", width = 90, height = 100, units = "cm", res = 400)
gosset:::plot_tree(tree)
dev.off()



#4-1-1 item covariate 




tree <- pltree(G ~ T10p_vg2 + CDI_mean_23_vg2 + Sum_no_rain_vg2 + GDD_vg2, 
               data = keep4dt_NA[keep4dt_bw2,],
               minsize = minsize, 
               alpha = 0.07, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

#increase pont
plot(tree) + theme(axis.text.y = element_text(size = 16, face = "bold"),axis.text.x = element_text(size = 16, face = "bold"))

predict(tree, type = "node")
table(predict(tree, type = "node"))

#here I just named 1, 2, 3, 4... for easy to memorize
partition_nod1<- subset(keep4dt_NA[keep4dt_bw2,], predict(tree, type = "node") == 2)
partition_nod2<- subset(keep4dt_NA[keep4dt_bw2,], predict(tree, type = "node") == 5)
partition_nod3<- subset(keep4dt_NA[keep4dt_bw2,], predict(tree, type = "node") == 6)
partition_nod4<- subset(keep4dt_NA[keep4dt_bw2,], predict(tree, type = "node") == 7)



write.table(partition_nod1,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod2,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node5.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod3,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node6.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod4,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node7.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

#extract   each season date set from each node  

partition_nod1_SA<- partition_nod1[c(which(partition_nod1[,"season"] == "Season_A")),]
partition_nod1_SB<- partition_nod1[c(which(partition_nod1[,"season"] == "Season_B")),]

partition_nod2_SA<- partition_nod2[c(which(partition_nod2[,"season"] == "Season_A")),]
partition_nod2_SB<- partition_nod2[c(which(partition_nod2[,"season"] == "Season_B")),]

partition_nod3_SA<- partition_nod3[c(which(partition_nod3[,"season"] == "Season_A")),]
partition_nod3_SB<- partition_nod3[c(which(partition_nod3[,"season"] == "Season_B")),]

partition_nod4_SA<- partition_nod4[c(which(partition_nod4[,"season"] == "Season_A")),]
partition_nod4_SB<- partition_nod4[c(which(partition_nod4[,"season"] == "Season_B")),]

write.table(partition_nod1_SA,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node2_seasonA.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod1_SB,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node2_seasonB.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod2_SA,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node5_seasonA.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod2_SB,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node5_seasonB.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod3_SA,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node6_seasonA.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod3_SB,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node6_seasonB.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod4_SA,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node7_seasonA.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod4_SB,file="data subtract/potato_tricot_plant_bacterial_wilt_vg2_node7_seasonB.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


##analysis nodes
#node 2

# Run packettluce modle about node2(partition_nod1) 
ig<- rankTricot(partition_nod1[], 
                items = variety,
                input = trait_bw_v2)

#prepare it to run forward selection
item_cov<- tuber_blup[]
item_covariance_name<- names(tuber_blup[-1])
counter <- 1
modelcoeffs <- list()
item_covar_keep<- character(0L)

#put all variable because to set highest AIC value 
baseline <- AIC(pladmm(ig, ~ flowering_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                       + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence  + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8))

#however, sometime pladmm model that has all variable has lower AIC than having less variable version... so make value big!!
baseline <- baseline + 300

# Be aware it! if you run it once as auto, it cannot worwk... Then, please manually run it the code and after second arround run it as auto. then it will work
# running until to find the best model 
while(best){
  message("Run " , counter, "\n")
  fs <- length(item_covariance_name)
  args <- list(formula = NULL,
               data = tuber_blup[],
               rho= 8 )
  AIC_value <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste( paste("~", c(item_covar_keep, item_covariance_name[i]), collapse = " + ")))
    args$formula <-f_i
    pl_i<- pladmm(ig,args$formula, data = tuber_blup,rho= 8  )
    AIC_value <- c(AIC_value, AIC(pl_i))
  }
  
  best_aic <- which.min(AIC_value)
  
  value_best <- min(AIC_value)
  
  # take the name of best variable
  best_model <- item_covariance_name[best_aic]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("~ ", paste(paste(item_covar_keep, collapse = " "), item_covariance_name))
  call_m <- data.frame(cbind(call = call_m, AIC = AIC_value))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  
  if(best){
    
    # remove best variable for the next run
    item_covariance_name <- item_covariance_name[!item_covariance_name %in% best_model]
    
    # keep this model for the next run
    item_covar_keep <- c(item_covar_keep, best_model)
    
    message("Best model found: ",
            paste0(" ~ ", paste(item_covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(item_covariance_name) == 0) {
    best <- FALSE}
}

item_covar_keep


f <- formula(paste0("~ ", paste(item_covar_keep, collapse = " + ")))

f
regressionPL_test<- pladmm(ig, f,data = tuber_blup[], rho = 8)
summary(regressionPL_test)
AIC(regressionPL_test)


#save the result. you do not want to play above colde... its toooo long to run 
capture.output(summary(regressionPL_test), 
               print("item_covariate_plant_BW_resistance (VG2)_node1_environment"),
               file ="output_thesis/item_covariate_plant_BW_resistance (VG2)_node1_environment_new.txt")



#node 5 (node2)
# Run packettluce modle about node5(partition_nod2) 
ig<- rankTricot(partition_nod2[], 
                items = variety,
                input = trait_bw_v2)

#prepare it to run forward selection
item_cov<- tuber_blup[]
item_covariance_name<- names(tuber_blup[-1])
counter <- 1
modelcoeffs <- list()
item_covar_keep<- character(0L)
#put all variable because to set highest AIC value 
baseline <- AIC(pladmm(ig, ~ flowering_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                       + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence  + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8))

#however, sometime pladmm model that has all variable has lower AIC than having less variable version... so make value big!!
baseline <- baseline + 300


# Be aware it! if you run it once as auto, it cannot worwk... Then, please manually run it the code and after second arround run it as auto. then it will work
# running until to find the best model 
while(best){
  message("Run " , counter, "\n")
  fs <- length(item_covariance_name)
  args <- list(formula = NULL,
               data = tuber_blup[],
               rho= 8 )
  AIC_value <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste( paste("~", c(item_covar_keep, item_covariance_name[i]), collapse = " + ")))
    args$formula <-f_i
    pl_i<- pladmm(ig,args$formula, data = tuber_blup,rho= 8  )
    AIC_value <- c(AIC_value, AIC(pl_i))
  }
  
  best_aic <- which.min(AIC_value)
  
  value_best <- min(AIC_value)
  
  # take the name of best variable
  best_model <- item_covariance_name[best_aic]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("~ ", paste(paste(item_covar_keep, collapse = " "), item_covariance_name))
  call_m <- data.frame(cbind(call = call_m, AIC = AIC_value))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  
  if(best){
    
    # remove best variable for the next run
    item_covariance_name <- item_covariance_name[!item_covariance_name %in% best_model]
    
    # keep this model for the next run
    item_covar_keep <- c(item_covar_keep, best_model)
    
    message("Best model found: ",
            paste0(" ~ ", paste(item_covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(item_covariance_name) == 0) {
    best <- FALSE}
}

item_covar_keep


f <- formula(paste0("~ ", paste(item_covar_keep, collapse = " + ")))

f
regressionPL_test<- pladmm(ig, f,data = tuber_blup[], rho = 8)
summary(regressionPL_test)
AIC(regressionPL_test)



#save the result. you do not want to play above colde... its toooo long to run 
capture.output(summary(regressionPL_test), 
               print("item_covariate_plant_BW_resistance (VG2)_node2_environment"),
               file ="output_thesis/item_covariate_plant_BW_resistance (VG2)_node2_environment_new.txt")



#node 6 (node3)
# Run packettluce modle about node6 (partition_nod3)
ig<- rankTricot(partition_nod3[], 
                items = variety,
                input = trait_bw_v2)

#prepare it to run forward selection
item_cov<- tuber_blup[]
item_covariance_name<- names(tuber_blup[-1])
counter <- 1
modelcoeffs <- list()
item_covar_keep<- character(0L)

#put all variable because to set highest AIC value 
baseline <- AIC(pladmm(ig, ~ flowering_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                       + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence  + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8))

#however, sometime pladmm model that has all variable has lower AIC than having less variable version... so make value big!!
baseline <- baseline + 300



# Be aware it! if you run it once as auto, it cannot worwk... Then, please manually run it the code and after second arround run it as auto. then it will work
# running until to find the best model 

while(best){
  message("Run " , counter, "\n")
  fs <- length(item_covariance_name)
  args <- list(formula = NULL,
               data = tuber_blup[],
               rho= 8 )
  AIC_value <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste( paste("~", c(item_covar_keep, item_covariance_name[i]), collapse = " + ")))
    args$formula <-f_i
    pl_i<- pladmm(ig,args$formula, data = tuber_blup,rho= 8  )
    AIC_value <- c(AIC_value, AIC(pl_i))
  }
  
  best_aic <- which.min(AIC_value)
  
  value_best <- min(AIC_value)
  
  # take the name of best variable
  best_model <- item_covariance_name[best_aic]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("~ ", paste(paste(item_covar_keep, collapse = " "), item_covariance_name))
  call_m <- data.frame(cbind(call = call_m, AIC = AIC_value))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  
  if(best){
    
    # remove best variable for the next run
    item_covariance_name <- item_covariance_name[!item_covariance_name %in% best_model]
    
    # keep this model for the next run
    item_covar_keep <- c(item_covar_keep, best_model)
    
    message("Best model found: ",
            paste0(" ~ ", paste(item_covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(item_covariance_name) == 0) {
    best <- FALSE}
}

item_covar_keep


f <- formula(paste0("~ ", paste(item_covar_keep, collapse = " + ")))

f
regressionPL_test<- pladmm(ig, f,data = tuber_blup[], rho = 8)
summary(regressionPL_test)
AIC(regressionPL_test)

# Be aware it! if you run it once as auto, it cannot worwk... Then, please manually run it the code and after second arround run it as auto. then it will work
# running until to find the best model 
capture.output(summary(regressionPL_test), 
               print("item_covariate_plant_BW_resistance (VG2)_node3_environment"),
               file ="output_thesis/item_covariate_plant_BW_resistance (VG2)_node3_environment_new.txt")


#node 7- node4
# Run packettluce modle about node7(partition_nod4)
ig<- rankTricot(partition_nod4[], 
                items = variety,
                input = trait_bw_v2)

#prepare it to run forward selection
item_cov<- tuber_blup[]
item_covariance_name<- names(tuber_blup[-1])
counter <- 1
modelcoeffs <- list()
item_covar_keep<- character(0L)

#put all variable because to set highest AIC value
baseline <- AIC(pladmm(ig, ~ flowering_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                       + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence  + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8))

#however, sometime pladmm model that has all variable has lower AIC than having less variable version... so make value big!!
baseline <- baseline + 300



# Be aware it! if you run it once as auto, it cannot worwk... Then, please manually run it the code and after second arround run it as auto. then it will work
# running until to find the best model 
while(best){
  message("Run " , counter, "\n")
  fs <- length(item_covariance_name)
  args <- list(formula = NULL,
               data = tuber_blup[],
               rho= 8 )
  AIC_value <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste( paste("~", c(item_covar_keep, item_covariance_name[i]), collapse = " + ")))
    args$formula <-f_i
    pl_i<- pladmm(ig,args$formula, data = tuber_blup,rho= 8  )
    AIC_value <- c(AIC_value, AIC(pl_i))
  }
  
  best_aic <- which.min(AIC_value)
  
  value_best <- min(AIC_value)
  
  # take the name of best variable
  best_model <- item_covariance_name[best_aic]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("~ ", paste(paste(item_covar_keep, collapse = " "), item_covariance_name))
  call_m <- data.frame(cbind(call = call_m, AIC = AIC_value))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  
  if(best){
    
    # remove best variable for the next run
    item_covariance_name <- item_covariance_name[!item_covariance_name %in% best_model]
    
    # keep this model for the next run
    item_covar_keep <- c(item_covar_keep, best_model)
    
    message("Best model found: ",
            paste0(" ~ ", paste(item_covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(item_covariance_name) == 0) {
    best <- FALSE}
}

item_covar_keep


f <- formula(paste0("~ ", paste(item_covar_keep, collapse = " + ")))

f
regressionPL_test<- pladmm(ig, f,data = tuber_blup[], rho = 8)
summary(regressionPL_test)
AIC(regressionPL_test)



#save the result. you do not want to play above colde... its toooo long to run 
capture.output(summary(regressionPL_test), 
               print("item_covariate_plant_BW_resistance (VG2)_node4_environment"),
               file ="output_thesis/item_covariate_plant_BW_resistance (VG2)_node4_environment_new.txt")




####node elevation
mean(partition_nod1[,220])
mean(partition_nod2[,220])
mean(partition_nod3[,220])
mean(partition_nod4[,220])

el_nod1 <- rep("node1", 47)
el_nod2 <- rep("node2", 110)
el_nod3 <- rep("node3", 48)
el_nod4 <- rep("node4", 144)

el_nod1 <- cbind(el_nod1, partition_nod1[,220])
el_nod2 <- cbind(el_nod2, partition_nod2[,220])
el_nod3 <- cbind(el_nod3, partition_nod3[,220])
el_nod4 <- cbind(el_nod4, partition_nod4[,220])


el_test <- rbind(el_nod1, el_nod2)
el_test<- rbind(el_test, el_nod3)
el_test<- rbind(el_test, el_nod4)

el_test <- as.data.frame(el_test)
kruskal.test(el_nod1 ~ V2, data = el_test)







#rankTricot(keep4dt_NA[keep4dt_bw2,], 
#           items = variety,
#           input = trait_bw_v2
#node 1 
#unc<- rankTricot(partition_nod1[], 
#                 items = variety,
#                 input = trait_bw_v2)
#
#regressionPL1 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
#                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)
#
#summary(regressionPL1)

#capture.output(summary(regressionPL1), 
#               print("item_covariate_plant_bacterial wilt_vg2_node1_environment&Rhomis"),
#               file ="output_thesis/item_covariate_plant_bacterial wilt_vg2_node1_environment_con.txt")

#node 2 
#unc<- rankTricot(partition_nod2[], 
#                 items = variety,
#                 input = trait_bw_v2)
#
#regressionPL2 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
#                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)

#summary(regressionPL2)

#capture.output(summary(regressionPL2), 
#               print("item_covariate_plant_bacterial wilt_vg2_node2_environment&Rhomis"),
#               file ="output_thesis/item_covariate_plant_bacterial wilt_vg2_node2_environment_con.txt")


#node 3 
#unc<- rankTricot(partition_nod3[], 
#                 items = variety,
#                 input = trait_bw_v2)

#regressionPL3 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
#                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)
#
#summary(regressionPL3)

#capture.output(summary(regressionPL3), 
#               print("item_covariate_plant_bacterial wilt_vg2_node3_environment&Rhomis"),
#               file ="output_thesis/item_covariate_plant_bacterial wilt_vg2_node3_environment_con.txt")


#node 4 
#unc<- rankTricot(partition_nod4[], 
#                 items = variety,
#                 input = trait_bw_v2)

#regressionPL4 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
#                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)

#summary(regressionPL4)

#capture.output(summary(regressionPL4), 
#               print("item_covariate_plant_bacterial wilt_vg2_node4_environment&Rhomis"),
#               file ="output_thesis/item_covariate_plant_bacterial wilt_vg2_node4_environment_contxt")





####4-2 plackett luce tree with Rhomis (just for checking but not available to use the manuscirpt this thime  )


#almost same above code but here use grop = true.
G <-  rankTricot(keep4dt_NA[keep4dt_bw2,], 
                 items = variety,
                 input = trait_bw_v2,
                 group = TRUE)



# covariates

covar <- keep4dt_NA[keep4dt_bw2,rhomis_cov]






# rankings 
out_r <- caret::nearZeroVar(covar)

names(covar)[out_r]

covar <- covar[]# there is no  zero covariate...

colnames(covar)

pld <- cbind(G, covar)
#cut data set that have NA
which(!is.na(pld$RHoMIS_id))
pld <- pld[which(!is.na(pld$RHoMIS_id)),]



# run a forward selection with cross-validation
# to select the best model
# Define initial parameters 

# set the baseline as a deviance from a Plackett-Luce model without covariates
baseline <- deviance(PlackettLuce(G))
# vector to keep best explanatory variables
covar_keep <- character(0L)
# keep running if TRUE
best <- TRUE
# number of runs
counter <- 1
# a list to keep the goodness-of-fit coefficients from each step
modelcoeffs <- list()
# get the names of explanatory and response variables
covarnames <- names(covar)
siglevel <- 0.1
minsize <- 25
npseudo <- 0.8
bonferroni <- TRUE
gamma <- TRUE

# keep running until the model get its best performance
while(best){
  
  message("Run " , counter, "\n")
  
  fs <- length(covarnames)
  
  args <- list(formula = NULL,
               data = pld,
               alpha = siglevel,
               npseudo = npseudo,
               bonferroni = bonferroni,
               gamma = gamma,
               minsize = minsize)
  
  deviances <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste0("G ~ ", paste(c(covar_keep, covarnames[i]), collapse = " + ")))
    args$formula <- f_i
    pl_i <- do.call("pltree", args)
    deviances <- c(deviances, deviance(pl_i))
  }
  
  best_dv <- which.min(deviances)
  
  value_best <- min(deviances)
  
  # take the name of best variable
  best_model <- covarnames[best_dv]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("G ~ ", paste(paste(covar_keep, collapse = " "), covarnames))
  call_m <- data.frame(cbind(call = call_m, deviance = deviances))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  if(best){
    
    # remove best variable for the next run
    covarnames <- covarnames[!covarnames %in% best_model]
    
    # keep this model for the next run
    covar_keep <- c(covar_keep, best_model)
    
    message("Best model found: ",
            paste0("G ~ ", paste(covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(covarnames) == 0) {
    best <- FALSE
  }
  
}


covar_keep

f <- formula(paste0("G ~ ", paste(covar_keep, collapse = " + ")))

f

tree <- pltree(f, 
               data = pld, 
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

tree

plot(tree)

png(filename = "output_thesis/Plackett-Luce tree bacterial_wilt (Day 75)with Rhomis covariates.png", width = 40, height = 30, units = "cm", res = 400)
plot(tree)
dev.off()

gosset:::plot_tree(tree)

png(filename = "output_thesis/Plackett-Luce tree bacterial_wilt (Day 75)with Rhomis_gosset.png", width = 40, height = 30, units = "cm", res = 400)
gosset:::plot_tree(tree)
dev.off()
#
#####
#### 4- 2- 2 item_covariate_Rhomis (let's check how it work!!). 


rhomis_t<- keep4dt_NA[keep4dt_bw2,]
which(!is.na(rhomis_t$RHoMIS_id))
rhomis_t <- rhomis_t[which(!is.na(rhomis_t$RHoMIS_id)),]

#rhomis_t == pld 

tree <- pltree(G ~ Potato_important + Farm_Income_USD, 
               data = pld,
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

plot(tree)
predict(tree, type = "node")
table(predict(tree, type = "node"))
partition_nod1<- subset(rhomis_t, predict(tree, type = "node") ==  2)
partition_nod2<- subset(rhomis_t, predict(tree, type = "node") ==  4)
partition_nod3<- subset(rhomis_t, predict(tree, type = "node") ==  5)

write.table(partition_nod1,file="data subtract/potato_tricot_plant_bacterial_wilt_with_Rhomis_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod2,file="data subtract/potato_tricot_plant_bacterial_wilt_with_Rhomis_node4.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod3,file="data subtract/potato_tricot_plant_bacterial_wilt_with_Rhomis_node5.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


#node 1
unc<- rankTricot(partition_nod1[], 
                 items = variety,
                 input = trait_bw_v2)

mod_R_nod2 <- PlackettLuce(unc)

summary(mod_R_nod2)
summary(mod_R_nod2, ref = "Jyambere")
plot_worth(mod_R_nod2)
coef(mod_R_nod2, log = F)
qv

#node 4
unc<- rankTricot(partition_nod2[], 
                 items = variety,
                 input = trait_bw_v2)

mod_R_nod4 <- PlackettLuce(unc)

summary(mod_R_nod4)
summary(mod_R_nod4, ref = "Cruza")
plot_worth(mod_R_nod4)
coef(mod_R_nod4, log = F)

#node 5
unc<- rankTricot(partition_nod3[], 
                 items = variety,
                 input = trait_bw_v2)

mod_R_nod2 <- PlackettLuce(unc)

summary(mod_R_nod2)
summary(mod_R_nod2, ref = "Kazeneza")
plot_worth(mod_R_nod2)
coef(mod_R_nod2, log = F)
###
#gender
prop.table(table(partition_nod1[,166]))
prop.table(table(partition_nod2[,166]))
prop.table(table(partition_nod3[,166]))

#count people 
prop.table(table(partition_nod1[,171]))
prop.table(table(partition_nod2[,171]))
prop.table(table(partition_nod3[,171]))

mean(partition_nod1[,171])
mean(partition_nod2[,171])
mean(partition_nod3[,171])


#node demograpic
prop.table(table(partition_nod1[,"education_level"]))
prop.table(table(partition_nod2[,"education_level"]))
prop.table(table(partition_nod3[,"education_level"]))

#asset count 
mean(partition_nod1[,179])
mean(partition_nod2[,179])
mean(partition_nod3[,179])


# potato important
prop.table(table(partition_nod1[,177]))
prop.table(table(partition_nod2[,177]))
prop.table(table(partition_nod3[,177]))

r_pot<-rhomis_t[,c("respondentsex", "potato_important")]
prop.table(table(r_pot))


#land cultivate
mean(partition_nod1[,172])
mean(partition_nod2[,172])
mean(partition_nod3[,172])

#land ownership
prop.table(table(partition_nod1[,173]))
prop.table(table(partition_nod2[,173]))
prop.table(table(partition_nod3[,173]))



prop.table(table(partition_nod1[,174]))
prop.table(table(partition_nod2[,174]))
prop.table(table(partition_nod3[,174]))

#farm labour
prop.table(table(partition_nod1[,175]))
prop.table(table(partition_nod2[,175]))
prop.table(table(partition_nod3[,175]))
#agriculture intensification 
mean(partition_nod1[,178])
mean(partition_nod2[,178])
mean(partition_nod3[,178])

#input diversity
mean(partition_nod1[,181])
mean(partition_nod2[,181])
mean(partition_nod3[,181])


#agriculture diversity
mean(partition_nod1[,180])
mean(partition_nod2[,180])
mean(partition_nod3[,180])

#TLU 
mean(partition_nod1[,183])
mean(partition_nod2[,183])
mean(partition_nod3[,183])


#node crop income
mean(partition_nod1[,185])
mean(partition_nod2[,185])
mean(partition_nod3[,185])

#livestock income
mean(partition_nod1[,186])
mean(partition_nod2[,186])
mean(partition_nod3[,186])

#node crop income
mean(partition_nod1[,187])
mean(partition_nod2[,187])
mean(partition_nod3[,187])

#hunder month
prop.table(table(partition_nod1$Hunger_Months_Count))
prop.table(table(partition_nod2$Hunger_Months_Count))
prop.table(table(partition_nod3$Hunger_Months_Count))

mean(partition_nod1$Hunger_Months_Count)
mean(partition_nod2$Hunger_Months_Count)
mean(partition_nod3$Hunger_Months_Count)


#item covariate
#node 1 
unc<- rankTricot(partition_nod1[], 
                 items = variety,
                 input = trait_bw_v2)

regressionPL1 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)

summary(regressionPL1)

capture.output(summary(regressionPL1), 
               print("item_covariate_plant_bacterial wilt_vg2_node1_Rhomis"),
               file ="output_thesis/item_covariate_plant_bacterial wilt_vg2_node1_Rhomis_con.txt")

#node 2 
unc<- rankTricot(partition_nod2[], 
                 items = variety,
                 input = trait_bw_v2)

regressionPL2 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)

summary(regressionPL2)

capture.output(summary(regressionPL2), 
               print("item_covariate_plant_bacterial wilt_vg2_node1_Rhomis"),
               file ="output_thesis/item_covariate_plant_bacterial wilt_vg2_node2_Rhomis_con.txt")


#node 3 
unc<- rankTricot(partition_nod3[], 
                 items = variety,
                 input = trait_bw_v2)

regressionPL3 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)

summary(regressionPL3)

capture.output(summary(regressionPL3), 
               print("item_covariate_plant_bacterial wilt_vg2_node1_Rhomis"),
               file ="output_thesis/item_covariate_plant_bacterial wilt_vg2_node3_Rhomis_con.txt")


#5.###########################################################################
#####tricot into a PlackettLuce model about disease and  insect resistance 2 (75 days)

trait_DI2 <-c("vegetative2_diseaseinsect_pos","vegetative2_diseaseinsect_neg") 

na_check <- is.na(dt2[,trait_DI2])
summary(na_check) #however, there are "Not observed" data. lets change "Not observed" to NA



keep_DI2<- dt2[]
keep_DI2[,trait_DI2]

keep_DI2[keep_DI2=="Not observed"] <- NA # change Not observed  to NA.
keep_DI2
keep_DI2[,trait_DI2]

#delete some farmers who did not respond both questions:positive & negative 

is.na(keep_DI2[,"vegetative2_diseaseinsect_pos"]) & is.na(keep_DI2[,"vegetative2_diseaseinsect_neg"])
sum(is.na(keep_DI2[,"vegetative2_diseaseinsect_pos"]) & is.na(keep_DI2[,"vegetative2_diseaseinsect_neg"]))
# 215 farmers didn't respond both questions:positive & negative 
#delete them
subset(keep_DI2[trait_DI2],is.na(keep_DI2[,"vegetative2_diseaseinsect_pos"]) & is.na(keep_DI2[,"vegetative2_diseaseinsect_neg"]))


keep_DI2<- keep_DI2[!is.na(keep_DI2$vegetative2_diseaseinsect_pos) | !is.na(keep_DI2$vegetative2_diseaseinsect_neg),] 

summary(keep_DI2[trait_DI2]) #217 farmers' data left
keep_DI2[trait_DI2]



keep_DI2_keep <- apply(keep_DI2[trait_DI2], 1, function(x){
  sum(is.na(x))})

keep_DI2_keep <- keep_DI2_keep <= 1

R_DI2 <-  rankTricot(keep_DI2[keep_DI2_keep,], 
                     items = variety,
                     input = trait_DI2)
R_DI2

mod_DI2<- PlackettLuce(R_DI2)

mod_DI2

summary(mod_DI2, ref = ref)

capture.output(summary(mod_DI2, ref = ref), 
               print("model coefficients(diseases or insect resistance 2 (Day 75))"),
               coef(mod_DI2, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_diseases or insect resistance2 (Day 75).txt")


plot(qvcalc(mod_DI2, ref = ref))


png(filename = "output_thesis/quasi_standard_error_potato_diseases or insect resistance 2 (Day 75).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_DI2, ref = ref), main = "potato_diseases or insect resistance2 (Day 75)quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_di2 <- adjacency(R_DI2)

a_di2 <- network(a_di2)
plot(a_di2)


png(filename = "output_thesis/network_potato_diseases or insect resistance2 (Day 75).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_di2)
dev.off()

# worth plot about diseases or insect resistance(Day 75)

plot_worth(mod_DI2)+ scale_x_continuous(limits=c(0,0.14))


png(filename = "output_thesis/worth plot about diseases or insect resistance 2 (Day 75).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_DI2) + theme_bw() +theme(panel.grid = element_blank()) + scale_x_continuous(limits=c(0,0.14))
dev.off()




# victories
vic <- summarise_victories(R_DI2)

plot(vic) +
  theme_bw() +
  theme(panel.grid = element_blank())

png(filename = "output_thesis/victories about diseases or insect resistance(Day 75).png", width = 30, height = 15, units = "cm", res = 400)
plot(vic) +
  theme_bw() +
  theme(panel.grid = element_blank())
dev.off()

############################################################################
#######################################
######################################
###################################
################################
#############################
#check farmers statement during vegetative 2 (Day 75) stage
dt3 <- dt[]
#######################################
#########################New stage:  post-harvest 1 (5 days after harvest) ###########################
#6.############################################################
#####tricot into a PlackettLuce model about maturity post-harvest 1 (5 days after harvest)) 

trait_Mph1 <- c("postharvest1_maturity_pos","postharvest1_maturity_neg")


na_check <- is.na(dt3[,c("postharvest1_maturity_pos","postharvest1_maturity_neg")])
summary(na_check) #however, there are "Not observed" data. lets change "Not observed" to NA


keep6<- dt3[]
keep6[,trait_Mph1]


#delete some farmers who did not respond both questions:positive & negative 


is.na(keep6[,"postharvest1_maturity_pos"]) & is.na(keep6[,"postharvest1_maturity_neg"])
sum(is.na(keep6[,"postharvest1_maturity_pos"]) & is.na(keep6[,"postharvest1_maturity_neg"]))

# 39 farmers didn't respond both questions:positive & negative 

subset(keep6[trait_Mph1],is.na(keep6[,"postharvest1_maturity_pos"]) & is.na(keep6[,"postharvest1_maturity_neg"]))
summary(subset(keep6[trait_Mph1],is.na(keep6[,"postharvest1_maturity_pos"]) & is.na(keep6[,"postharvest1_maturity_neg"])))


keep6_Mph1<- keep6[!is.na(keep6$postharvest1_maturity_pos) | !is.na(keep6$postharvest1_maturity_neg),] # delete 39 farmers data 

summary(keep6_Mph1[trait_Mph1])
keep6_Mph1[trait_Mph1] # after deleting farmers who didn't respond both questions. The result shows there are no more NA. so lets skip blow procedure. 
is.na(keep6_Mph1[trait_Mph1])
summary(is.na(keep6_Mph1[trait_Mph1])) #  393farmers' data left

################################skip this part
#keep_Mph1_keep <- apply(keep6_Mph1[trait_Mph1], 1, function(x){sum(is.na(x))}
#keep_Mph1_keep <- keep_Mph1_keep <= 1
##########################################
R_Mph1 <-  rankTricot(keep6_Mph1[], 
                      items = variety,
                      input = trait_Mph1)




mod_Post_mat <- PlackettLuce(R_Mph1)

summary(mod_Post_mat)


summary(mod_Post_mat, ref = ref)


capture.output(summary(mod_Post_mat, ref = ref), 
               print("model coefficients(maturity post-harvest 1 (5 days after harvest))"),
               coef(mod_Post_mat, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_maturity post-harvest 1 (5 days after harvest).txt")


plot(qvcalc(mod_Post_mat, ref = ref))


png(filename = "output_thesis/quasi_standard_error_potato_maturity post-harvest 1 (5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_Post_mat, ref = ref), main = "maturity post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_Post_mat <- adjacency(R_Mph1)

a_Post_mat  <- network(a_Post_mat )
plot(a_Post_mat )

png(filename = "output_thesis/network_potato_maturity post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_Post_mat )
dev.off()

# worth plot about potato_maturity post-harvest 1 (5 days after harvest)

plot_worth(mod_Post_mat)

png(filename = "output_thesis/worth plot about potato_maturity post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_Post_mat) + theme_bw() +theme(panel.grid = element_blank())
dev.off()



# victories
vic <- summarise_victories(R_Mph1)

plot(vic) +
  theme_bw() +
  theme(panel.grid = element_blank())

png(filename = "output_thesis/victories about potato_maturity post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(vic) +
  theme_bw() +
  theme(panel.grid = element_blank())
dev.off()
###############################################################################
###########################################################################
#7.############################################################
###########################################################################
#7.############################################################
#####tricot into a PlackettLuce model about post harvest yield (5 days after harvest)) 

trait_vigour_yieldpos1 <- c("postharvest1_yield_pos","postharvest1_yield_neg")
na_check <- is.na(dt3[,c("postharvest1_yield_pos","postharvest1_yield_neg")])
summary(na_check) #however, there are "Not observed" data. lets change "Not observed" to NA


keep7<- dt3[]
keep7[trait_vigour_yieldpos1 ]

# lets check some farmers who did not respond both questions:positive & negative 

is.na(keep7[,"postharvest1_yield_pos"]) & is.na(keep7[,"postharvest1_yield_neg"])
sum(is.na(keep7[,"postharvest1_yield_pos"]) & is.na(keep7[,"postharvest1_yield_neg"]))
subset(keep7[trait_vigour_yieldpos1],is.na(keep7[,"postharvest1_yield_pos"]) & is.na(keep7[,"postharvest1_yield_neg"]))
summary(subset(keep7[trait_vigour_yieldpos1],is.na(keep7[,"postharvest1_yield_pos"]) & is.na(keep7[,"postharvest1_yield_neg"])))
# 39 farmers didnit respond both questions:positive & negative 

keep7_yieldpos1<- keep7[!is.na(keep7$postharvest1_yield_pos) | !is.na(keep7$postharvest1_yield_neg),] # delete 13 farmers data from 430 farmers data
keep7_yieldpos1[trait_vigour_yieldpos1]

summary(keep7_yieldpos1[trait_vigour_yieldpos1]) #  393 farmers data remained.  

table(keep7_yieldpos1[trait_vigour_yieldpos1])
#the result shows there are no more NA. so lets skip blow procesure. 
summary(is.na(keep7_yieldpos1[trait_vigour_yieldpos1])) 


################################skip this part
#keep7_yieldpos1_keep <- apply(keep7_yieldpos1[trait_vigour_yieldpos1], 1, function(x){sum(is.na(x))}
#keep7_yieldpos1_keep <- keep7_yieldpos1_keep <= 1
##########################################



#run packetlucce model 
R_yieldpos1<-  rankTricot(keep7_yieldpos1[], 
                          items = variety,
                          input = trait_vigour_yieldpos1)

mod_Post_yield <- PlackettLuce(R_yieldpos1)

summary(mod_Post_yield )
summary(mod_Post_yield, ref = ref)



plot(qvcalc(mod_Post_yield, ref = ref))


a_di4 <- adjacency(R_yieldpos1)
a_di4 <- network(R_yieldpos1)
plot(a_di4)

#worth plot about harvest yield (5 days after harvest) 

plot_worth(mod_Post_yield)



#7-1##plackett luce tree with environment and soil covariate data

#almost same above code but here use grop = true.
G <-  rankTricot(keep7_yieldpos1[], 
                 items = variety,
                 input = trait_vigour_yieldpos1,
                 group = TRUE)



# covariates



covar <- keep7_yieldpos1[,env_cov]

# rankings 
out_r <- caret::nearZeroVar(covar)

names(covar)[out_r]

covar <- covar[, -out_r]

colnames(covar)

pld <- cbind(G, covar)

head(pld)

# run a forward selection with cross-validation
# to select the best model
# Define initial parameters 

# set the baseline as a deviance from a Plackett-Luce model without covariates
baseline <- deviance(PlackettLuce(G))
# vector to keep best explanatory variables
covar_keep <- character(0L)
# keep running if TRUE
best <- TRUE
# number of runs
counter <- 1
# a list to keep the goodness-of-fit coefficients from each step
modelcoeffs <- list()
# get the names of explanatory and response variables
covarnames <- names(covar)
siglevel <- 0.1
minsize <- 25
npseudo <- 0.8
bonferroni <- TRUE
gamma <- TRUE

# keep running until the model get its best performance
while(best){
  
  message("Run " , counter, "\n")
  
  fs <- length(covarnames)
  
  args <- list(formula = NULL,
               data = pld,
               alpha = siglevel,
               npseudo = npseudo,
               bonferroni = bonferroni,
               gamma = gamma,
               minsize = minsize)
  
  deviances <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste0("G ~ ", paste(c(covar_keep, covarnames[i]), collapse = " + ")))
    args$formula <- f_i
    pl_i <- do.call("pltree", args)
    deviances <- c(deviances, deviance(pl_i))
  }
  
  best_dv <- which.min(deviances)
  
  value_best <- min(deviances)
  
  # take the name of best variable
  best_model <- covarnames[best_dv]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("G ~ ", paste(paste(covar_keep, collapse = " "), covarnames))
  call_m <- data.frame(cbind(call = call_m, deviance = deviances))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  if(best){
    
    # remove best variable for the next run
    covarnames <- covarnames[!covarnames %in% best_model]
    
    # keep this model for the next run
    covar_keep <- c(covar_keep, best_model)
    
    message("Best model found: ",
            paste0("G ~ ", paste(covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(covarnames) == 0) {
    best <- FALSE
  }
  
}


covar_keep

f <- formula(paste0("G ~ ", paste(covar_keep, collapse = " + ")))

f

tree <- pltree(f, 
               data = pld, 
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

tree
plot(tree)

p<- plot(tree) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#7-1 -item covariate environment 

tree <- pltree(G ~ T90p, 
               data = keep7_yieldpos1[],
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)


plot(tree)
predict(tree, type = "node")
table(predict(tree, type = "node"))

# extract each node farmers' data 
partition_nod1<- subset(keep7_yieldpos1[], predict(tree, type = "node") == 2)
partition_nod2<- subset(keep7_yieldpos1[], predict(tree, type = "node") == 3)



##analysis nodes
#node 2
R_yieldpos1_node2<-  rankTricot(partition_nod1[], 
                                items = variety,
                                input = trait_vigour_yieldpos1)

mod_Post_yield_node2 <- PlackettLuce(R_yieldpos1_node2)

summary(mod_Post_yield_node2 )
summary(mod_Post_yield_node2, ref = "Twihaze")
plot_worth(mod_Post_yield_node2)
coef(mod_Post_yield_node2, log= F)


#node 3
R_yieldpos1_node3<-  rankTricot(partition_nod2[], 
                                items = variety,
                                input = trait_vigour_yieldpos1)

mod_Post_yield_node3 <- PlackettLuce(R_yieldpos1_node3)

summary(mod_Post_yield_node3 )
summary(mod_Post_yield_node3, ref = "Kirundo")
plot_worth(mod_Post_yield_node3)
coef(mod_Post_yield_node3, log= F)



###### 7-1-1 analyzing pladmm to know item covariate effect
###
#node2 (partition_nod1) 
# Run packettluce modle about node2(partition_nod1)
#prepare it to run forward selection
ig<- rankTricot(partition_nod1[], 
                items = variety,
                input = trait_vigour_yieldpos1)
item_cov<- tuber_blup[]
item_covariance_name<- names(tuber_blup[-1])
counter <- 1
modelcoeffs <- list()
item_covar_keep<- character(0L)
#put all variable because to set highest AIC value 
baseline <- AIC(pladmm(ig, ~ flowering_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                       + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence  + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8))
baseline<- baseline + 100 # to increase baseline number to compare forward selection results 

#remove leaf miner files

# Be aware it! if you run it once as auto, it cannot worwk... Then, please manually run it the code and after second around run it as auto. then it will work
# running until to find the best model 


while(best){
  message("Run " , counter, "\n")
  fs <- length(item_covariance_name)
  args <- list(formula = NULL,
               data = tuber_blup[],
               rho= 8 )
  AIC_value <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste( paste("~", c(item_covar_keep, item_covariance_name[i]), collapse = " + ")))
    args$formula <-f_i
    pl_i<- pladmm(ig,args$formula, data = tuber_blup,rho= 8  )
    AIC_value <- c(AIC_value, AIC(pl_i))
  }
  
  best_aic <- which.min(AIC_value)
  
  value_best <- min(AIC_value)
  
  # take the name of best variable
  best_model <- item_covariance_name[best_aic]
  
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("~ ", paste(paste(item_covar_keep, collapse = " "), item_covariance_name))
  call_m <- data.frame(cbind(call = call_m, AIC = AIC_value))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  
  if(best){
    
    # remove best variable for the next run
    item_covariance_name <- item_covariance_name[!item_covariance_name %in% best_model]
    
    # keep this model for the next run
    item_covar_keep <- c(item_covar_keep, best_model)
    
    message("Best model found: ",
            paste0(" ~ ", paste(item_covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(item_covariance_name) == 0) {
    best <- FALSE}
}


item_covar_keep


f <- formula(paste0("~", paste(item_covar_keep, collapse = " + ")))

f
regressionPL1_test<- pladmm(ig, f,data = tuber_blup[], rho = 8)
summary(regressionPL1_test)

AIC(regressionPL1_test)


#save the result. you do not want to play above colde... its toooo long to run 
capture.output(summary(regressionPL1_test), 
               print("item_covariate_plant_yield(post_harvest 1)_node1_environment"),
               file ="output_thesis/item_covariate_plant_yield(post_harvest 1)_node1_environment_con_fowardsel_test.txt")






#####
#node3 (node2 ) 
# Run packettluce modle about node3(partition_nod2)
#prepare it to run forward selection
ig<- rankTricot(partition_nod2[], 
                items = variety,
                input = trait_vigour_yieldpos1)
item_cov<- tuber_blup[]
item_covariance_name<- names(tuber_blup[-1])
counter <- 1
modelcoeffs <- list()
item_covar_keep<- character(0L)
#put all variable because to set highest AIC value 
baseline <-  AIC(pladmm(ig, ~ flowering_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence  + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8))

#however, sometime pladmm model that has all variable has lower AIC than having less variable version... so make value big!!
baseline<- baseline + 300


# Be aware it! if you run it once as auto, it cannot work... Then, please manually run it the code and after second arround run it as auto. then it will work
# running until to find the best model 

while(best){
  message("Run " , counter, "\n")
  fs <- length(item_covariance_name)
  args <- list(formula = NULL,
               data = tuber_blup[],
               rho= 8 )
  AIC_value <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste( paste("~", c(item_covar_keep, item_covariance_name[i]), collapse = " + ")))
    args$formula <-f_i
    pl_i<- pladmm(ig,args$formula, data = tuber_blup,rho= 8  )
    AIC_value <- c(AIC_value, AIC(pl_i))
  }
  
  best_aic <- which.min(AIC_value)
  
  value_best <- min(AIC_value)
  
  # take the name of best variable
  best_model <- item_covariance_name[best_aic]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("~ ", paste(paste(item_covar_keep, collapse = " "), item_covariance_name))
  call_m <- data.frame(cbind(call = call_m, AIC = AIC_value))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  
  if(best){
    
    # remove best variable for the next run
    item_covariance_name <- item_covariance_name[!item_covariance_name %in% best_model]
    
    # keep this model for the next run
    item_covar_keep <- c(item_covar_keep, best_model)
    
    message("Best model found: ",
            paste0(" ~ ", paste(item_covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(item_covariance_name) == 0) {
    best <- FALSE}
}

item_covar_keep


f <- formula(paste0("~ ", paste(item_covar_keep, collapse = " + ")))

f
regressionPL_test<- pladmm(ig, f,data = tuber_blup[], rho = 8)
summary(regressionPL_test)
AIC(regressionPL_test)


#save the result. you do not want to play above colde... its toooo long to run 

capture.output(summary(regressionPL_test), 
               print("item_covariate_plant_yield(post_harvest 1)_node2_environment"),
               file ="output_thesis/item_covariate_plant_yield(post_harvest 1)_node2_environment_con_fowardsel_test.txt")









###7-2 plackett luce tree with Rhomis(just for checking )
#almost same above code but here use grop = true.


keep7_yieldpos1_C<- keep7_yieldpos1[which(!is.na(keep7_yieldpos1[,"RHoMIS_id"])),]


#worth_plot check about data with Rhomis
G <- rankTricot(keep7_yieldpos1_C[], 
                items = variety,
                input = trait_vigour_yieldpos1)
m  <- PlackettLuce(G)
summary(m, ref = ref)
plot_worth(m) + theme_bw() +theme(panel.grid = element_blank()) 




png(filename = "output_thesis/worth plot about potato yield post-harvest 1 (5 days after harvest)with Rhomis.png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(m) + theme_bw() +theme(panel.grid = element_blank())
dev.off()




G <-  rankTricot(keep7_yieldpos1_C[], 
                 items = variety,
                 input = trait_vigour_yieldpos1,
                 group = TRUE)




# covariates

covar <- keep7_yieldpos1_C[,rhomis_cov]


# rankings 
out_r <- caret::nearZeroVar(covar)

names(covar)[out_r]

covar <- covar[]# there is no  zero covariate...

colnames(covar)

pld <- cbind(G, covar)
#cut data set that have NA
which(!is.na(pld$RHoMIS_id))
pld 


str(pld)



# run a forward selection with cross-validation
# to select the best model
# Define initial parameters 

# set the baseline as a deviance from a Plackett-Luce model without covariates
baseline <- deviance(PlackettLuce(G))
# vector to keep best explanatory variables
covar_keep <- character(0L)
# keep running if TRUE
best <- TRUE
# number of runs
counter <- 1
# a list to keep the goodness-of-fit coefficients from each step
modelcoeffs <- list()
# get the names of explanatory and response variables
covarnames <- names(covar)
siglevel <- 0.1
minsize <- 25
npseudo <- 0.8
bonferroni <- TRUE
gamma <- TRUE

# keep running until the model get its best performance
while(best){
  
  message("Run " , counter, "\n")
  
  fs <- length(covarnames)
  
  args <- list(formula = NULL,
               data = pld,
               alpha = siglevel,
               npseudo = npseudo,
               bonferroni = bonferroni,
               gamma = gamma,
               minsize = minsize)
  
  deviances <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste0("G ~ ", paste(c(covar_keep, covarnames[i]), collapse = " + ")))
    args$formula <- f_i
    pl_i <- do.call("pltree", args)
    deviances <- c(deviances, deviance(pl_i))
  }
  
  best_dv <- which.min(deviances)
  
  value_best <- min(deviances)
  
  # take the name of best variable
  best_model <- covarnames[best_dv]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("G ~ ", paste(paste(covar_keep, collapse = " "), covarnames))
  call_m <- data.frame(cbind(call = call_m, deviance = deviances))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  if(best){
    
    # remove best variable for the next run
    covarnames <- covarnames[!covarnames %in% best_model]
    
    # keep this model for the next run
    covar_keep <- c(covar_keep, best_model)
    
    message("Best model found: ",
            paste0("G ~ ", paste(covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(covarnames) == 0) {
    best <- FALSE
  }
  
}


covar_keep

f <- formula(paste0("G ~ ", paste(covar_keep, collapse = " + ")))

f

tree <- pltree(f, 
               data = pld, 
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

tree
plot(tree)
png(filename = "output_thesis/Plackett-Luce tree  yield post-harvest 1 (5 days after harvest)with RhoMIS covariates.png", width = 37, height = 28, units = "cm", res = 400)
plot(tree)
dev.off()


gosset:::plot_tree(tree)

png(filename = "output_thesis/Plackett-Luce tree  yield post-harvest 1 (5 days after harvest)with RhoMIS covariates_gosset.png", width = 30, height = 30, units = "cm", res = 400)
gosset:::plot_tree(tree)
dev.off()


#7-2 -item covariate environment 
rhomis_t<- keep7_yieldpos1[]
which(!is.na(rhomis_t$RHoMIS_id))
rhomis_t <- rhomis_t[which(!is.na(rhomis_t$RHoMIS_id)),]


#pld = rhomis_t

tree <- pltree(G ~ Hunger_months, 
               data = pld,
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

plot(tree)
predict(tree, type = "node")
table(predict(tree, type = "node"))

partition_nod1<- subset(rhomis_t[], predict(tree, type = "node") == 3)
partition_nod2<- subset(rhomis_t[], predict(tree, type = "node") == 5)
partition_nod3<- subset(rhomis_t[], predict(tree, type = "node") == 6)
partition_nod4<- subset(rhomis_t[], predict(tree, type = "node") == 7)


write.table(partition_nod1,file="data subtract/potato_tricot_yield_with_Rhomis_node3.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod2,file="data subtract/potato_tricot_yield_with_Rhomis_node5.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod3,file="data subtract/potato_tricot_yield_with_Rhomis_node6.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod4,file="data subtract/potato_tricot_yield_with_Rhomis_node7.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


##analysis nodes
#node 3
R_yieldpos1_node3_rhomois<-  rankTricot(partition_nod1[], 
                                        items = variety,
                                        input = trait_vigour_yieldpos1)

mod_Post_yield_node3_rhomois <- PlackettLuce(R_yieldpos1_node3_rhomois)

summary(mod_Post_yield_node3_rhomois )
summary(mod_Post_yield_node3_rhomois, ref = "Kirundo")
plot_worth(mod_Post_yield_node3_rhomois)

coef(mod_Post_yield_node3_rhomois, log = F)


#node 5
R_yieldpos1_node5_rhomois<-  rankTricot(partition_nod2[], 
                                        items = variety,
                                        input = trait_vigour_yieldpos1)

mod_Post_yield_node5_rhomois <- PlackettLuce(R_yieldpos1_node5_rhomois)

summary(mod_Post_yield_node5_rhomois )
summary(mod_Post_yield_node5_rhomois, ref = "Kirundo")
plot_worth(mod_Post_yield_node5_rhomois)

coef(mod_Post_yield_node5_rhomois, log = F)

#node 6
R_yieldpos1_node6_rhomois<-  rankTricot(partition_nod3[], 
                                        items = variety,
                                        input = trait_vigour_yieldpos1)

mod_Post_yield_node6_rhomois <- PlackettLuce(R_yieldpos1_node6_rhomois)

summary(mod_Post_yield_node6_rhomois )
summary(mod_Post_yield_node6_rhomois, ref = "Twihaze")
plot_worth(mod_Post_yield_node6_rhomois)
coef(mod_Post_yield_node6_rhomois, log = F)

#node 7
R_yieldpos1_node7_rhomois<-  rankTricot(partition_nod4[], 
                                        items = variety,
                                        input = trait_vigour_yieldpos1)

mod_Post_yield_node7_rhomois <- PlackettLuce(R_yieldpos1_node7_rhomois)

summary(mod_Post_yield_node7_rhomois )
summary(mod_Post_yield_node7_rhomois, ref = "Kazeneza")
plot_worth(mod_Post_yield_node7_rhomois)

coef(mod_Post_yield_node7_rhomois, log = F)

#####
# gender
prop.table(table(partition_nod1[,166]))
prop.table(table(partition_nod2[,166]))
prop.table(table(partition_nod3[,166]))
prop.table(table(partition_nod4[,166]))


#node demograpic
prop.table(table(partition_nod1[,"education_level"]))
prop.table(table(partition_nod2[,"education_level"]))
prop.table(table(partition_nod3[,"education_level"]))
prop.table(table(partition_nod4[,"education_level"]))

#asset count 
mean(partition_nod1[,179])
mean(partition_nod2[,179])
mean(partition_nod3[,179])
mean(partition_nod4[,179])


# potato important
prop.table(table(partition_nod1[,177]))
prop.table(table(partition_nod2[,177]))
prop.table(table(partition_nod3[,177]))
prop.table(table(partition_nod4[,177]))

#land cultivate
mean(partition_nod1[,172])
mean(partition_nod2[,172])
mean(partition_nod3[,172])
mean(partition_nod4[,172])

#land ownership
prop.table(table(partition_nod1[,173]))
prop.table(table(partition_nod2[,173]))
prop.table(table(partition_nod3[,173]))
prop.table(table(partition_nod4[,173]))


prop.table(table(partition_nod1[,174]))
prop.table(table(partition_nod2[,174]))
prop.table(table(partition_nod3[,174]))
prop.table(table(partition_nod4[,174]))

#farm labour
prop.table(table(partition_nod1[,175]))
prop.table(table(partition_nod2[,175]))
prop.table(table(partition_nod3[,175]))
prop.table(table(partition_nod4[,175]))
#agriculture intensification 
mean(partition_nod1[,178])
mean(partition_nod2[,178])
mean(partition_nod3[,178])
mean(partition_nod4[,178])

#agriculture diversity
mean(partition_nod1[,180])
mean(partition_nod2[,180])
mean(partition_nod3[,180])
mean(partition_nod4[,180])

#TLU 
mean(partition_nod1[,183])
mean(partition_nod2[,183])
mean(partition_nod3[,183])
mean(partition_nod4[,183])


#node crop income
mean(partition_nod1[,185])
mean(partition_nod2[,185])
mean(partition_nod3[,185])
mean(partition_nod4[,185])

#livestock income
mean(partition_nod1[,186])
mean(partition_nod2[,186])
mean(partition_nod3[,186])
mean(partition_nod4[,186])

#node crop income
mean(partition_nod1[,187])
mean(partition_nod2[,187])
mean(partition_nod3[,187])
mean(partition_nod4[,187])



##### Checking itemcovariate effect in each node of RHOMIS plt 
##
#node 1 
unc<- rankTricot(partition_nod1[], 
                 items = variety,
                 input = trait_vigour_yieldpos1)

regressionPL1 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)

summary(regressionPL1)

capture.output(summary(regressionPL1), 
               print("item_covariate_plant_yield(post_harvest 1)_node1_Rhomis"),
               file ="output_thesis/item_covariate_plant_yield(post_harvest 1)_node1_Rhomis_con.txt")

#node 2 
unc<- rankTricot(partition_nod2[], 
                 items = variety,
                 input = trait_vigour_yieldpos1)

regressionPL2 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)

summary(regressionPL2)

capture.output(summary(regressionPL2), 
               print("item_covariate_plant_yield(post_harvest 1)_node2_Rhomis"),
               file ="output_thesis/item_covariate_plant_yield(post_harvest 1)_node2_Rhomis_con.txt")



#node 3 
unc<- rankTricot(partition_nod3[], 
                 items = variety,
                 input = trait_vigour_yieldpos1)

regressionPL3 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)

summary(regressionPL3)

capture.output(summary(regressionPL3), 
               print("item_covariate_plant_yield(post_harvest 1)_node3_environment"),
               file ="output_thesis/item_covariate_plant_yield(post_harvest 1)_node3_Rhomis_con.txt")



#node 4 
unc<- rankTricot(partition_nod4[], 
                 items = variety,
                 input = trait_vigour_yieldpos1)

regressionPL4 <- pladmm(unc, ~ flowering_prediction + plant_uniformity_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                        + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8)

summary(regressionPL4)

capture.output(summary(regressionPL4), 
               print("item_covariate_plant_yield(post_harvest 1)_node4_environment"),
               file ="output_thesis/item_covariate_plant_yield(post_harvest 1)_node4_Rhomis_con.txt")

#
#####
########################################
#8.#########################################################
# tricot into a PlackettLuce rankings about post harvest tuber size (5 days after harvest)) 

trait_tubersize_p1 <- c("postharvest1_tubersize_pos","postharvest1_tubersize_neg")

na_check <- is.na(dt3[,c("postharvest1_tubersize_pos","postharvest1_tubersize_neg")])
summary(na_check) 


keep8<- dt3[]
keep8[trait_tubersize_p1]
#ckeck NA how many are there

is.na(keep8[,"postharvest1_tubersize_pos"]) & is.na(keep8[,"postharvest1_tubersize_neg"])
sum(is.na(keep8[,"postharvest1_tubersize_pos"]) & is.na(keep8[,"postharvest1_tubersize_neg"]))

# 39 farmers didnit respond both questions:positive & negative 

subset(keep8[trait_tubersize_p1],is.na(keep6[,"postharvest1_tubersize_pos"]) & is.na(keep8[,"postharvest1_tubersize_neg"]))


keep8_tubersize_p1<- keep8[!is.na(keep8$postharvest1_tubersize_pos) | !is.na(keep8$postharvest1_tubersize_neg),] # delete 13 farmers data from 430 farmers data

summary(keep8_tubersize_p1[trait_tubersize_p1]) #393 farmers left
keep8_tubersize_p1[trait_tubersize_p1] # after deleting farmers who didn't respond both questions. 1 Farmers respond only neg. 
is.na(keep8_tubersize_p1[trait_tubersize_p1])
summary(is.na(keep8_tubersize_p1[trait_tubersize_p1]))


keep_keep8_tubersize_p1_keep <- apply(keep8_tubersize_p1[trait_tubersize_p1], 1, function(x){sum(is.na(x))})
keep_keep8_tubersize_p1_keep <- keep_keep8_tubersize_p1_keep <= 1 



R_tuber_size <-  rankTricot(keep8_tubersize_p1[keep_keep8_tubersize_p1_keep,], 
                            items = variety,
                            input = trait_tubersize_p1)

mod_tuber_size <- PlackettLuce(R_tuber_size )

summary(mod_tuber_size)


summary(mod_tuber_size, ref = "Twihaze")

coef(mod_tuber_size, log = F)


capture.output(summary(mod_tuber_size, ref = "Twihaze"), 
               print("model coefficients(maturity post harvest tuber size (5 days after harvest))"),
               coef(mod_tuber_size, ref = "Twihaze"),
               file ="output_thesis/summary_mod_Coef_potato harvest tuber size post-harvest 1 (5 days after harvest).txt")


plot(qvcalc(mod_tuber_size, ref = "Twihaze"))

png(filename = "output_thesis/quasi_standard_error_potato_ tuber size post-harvest 1 (5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_tuber_size, ref = "Twihaze"), main = "tuber size post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_tuber_size <- adjacency(R_tuber_size)

#a_tuber_size  <- network(a_tuber_size )
#plot(a_tuber_size )

# worth plot about potato_tuber size post-harvest 1 (5 days after harvest)


plot_worth(mod_tuber_size) 

png(filename = "output_thesis/worth plot about potato_tuber size post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_tuber_size)  + theme_bw() +theme(panel.grid = element_blank())
dev.off()



png(filename = "output_thesis/worth plot about potato_tuber size post-harvest 1 (5 days after harvest)_ver2.png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_tuber_size)  + theme_bw() +theme(panel.grid = element_blank()) +  theme(text = element_text(size = 16))  + labs(y = "Varieties", x = "The probability of winning (worth)")
dev.off()



#8-1####plackett luce tree with environment and soil covariate data
###plackett luce tree with environment and soil covariate data

#almost same above code but here use grop = true.
G <-  rankTricot(keep8_tubersize_p1[keep_keep8_tubersize_p1_keep,], 
                 items = variety,
                 input = trait_tubersize_p1,
                 group = TRUE)



# covariates



covar <- keep8_tubersize_p1[keep_keep8_tubersize_p1_keep,env_cov]

# rankings 
out_r <- caret::nearZeroVar(covar)

names(covar)[out_r]

covar <- covar[, -out_r]

colnames(covar)

pld <- cbind(G, covar)

head(pld)


# run a forward selection with cross-validation
# to select the best model
# Define initial parameters 

# set the baseline as a deviance from a Plackett-Luce model without covariates
baseline <- deviance(PlackettLuce(G))
# vector to keep best explanatory variables
covar_keep <- character(0L)
# keep running if TRUE
best <- TRUE
# number of runs
counter <- 1
# a list to keep the goodness-of-fit coefficients from each step
modelcoeffs <- list()
# get the names of explanatory and response variables
covarnames <- names(covar)
siglevel <- 0.1
minsize <- 25
npseudo <- 0.8
bonferroni <- TRUE
gamma <- TRUE

# keep running until the model get its best performance
while(best){
  
  message("Run " , counter, "\n")
  
  fs <- length(covarnames)
  
  args <- list(formula = NULL,
               data = pld,
               alpha = siglevel,
               npseudo = npseudo,
               bonferroni = bonferroni,
               gamma = gamma,
               minsize = minsize)
  
  deviances <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste0("G ~ ", paste(c(covar_keep, covarnames[i]), collapse = " + ")))
    args$formula <- f_i
    pl_i <- do.call("pltree", args)
    deviances <- c(deviances, deviance(pl_i))
  }
  
  best_dv <- which.min(deviances)
  
  value_best <- min(deviances)
  
  # take the name of best variable
  best_model <- covarnames[best_dv]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("G ~ ", paste(paste(covar_keep, collapse = " "), covarnames))
  call_m <- data.frame(cbind(call = call_m, deviance = deviances))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  if(best){
    
    # remove best variable for the next run
    covarnames <- covarnames[!covarnames %in% best_model]
    
    # keep this model for the next run
    covar_keep <- c(covar_keep, best_model)
    
    message("Best model found: ",
            paste0("G ~ ", paste(covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(covarnames) == 0) {
    best <- FALSE
  }
  
}


covar_keep

f <- formula(paste0("G ~ ", paste(covar_keep, collapse = " + ")))

f

tree <- pltree(f, 
               data = pld, 
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

tree

plot(tree)

plot(tree)

png(filename = "output_thesis/Plackett-Luce tree tuber size post-harvest 1 (5 days after harvest)with environment covariates.png", width = 30, height = 30, units = "cm", res = 400)
plot(tree)
dev.off()


gosset:::plot_tree(tree)

png(filename = "output_thesis/Plackett-Luce tree  tuber size post-harvest 1 (5 days after harvest)with environment covariates_gosset.png", width = 30, height = 30, units = "cm", res = 400)
gosset:::plot_tree(tree)
dev.off()



######

#9.############################################################ 
# tricot into a PlackettLuce model about post harvest tuber appearance (5 days after harvest))  
trait_appearance_d5 <- c("postharvest1_tuberappearance_pos","postharvest1_tuberappearance_neg")

na_check <- is.na(dt[,c("postharvest1_tuberappearance_pos","postharvest1_tuberappearance_neg")])
summary(na_check) #Lets check how many NA in farmers data 

keep9<- dt3[]
keep9[trait_appearance_d5]
is.na(keep9[trait_appearance_d5])
sum(is.na(keep9[trait_appearance_d5])) 
subset(keep9[trait_appearance_d5],is.na(keep9[,"postharvest1_tuberappearance_pos"]) & is.na(keep9[,"postharvest1_tuberappearance_neg"]))
summary(subset(keep9[trait_appearance_d5],is.na(keep9[,"postharvest1_tuberappearance_pos"]) & is.na(keep9[,"postharvest1_tuberappearance_neg"])))
#39 farmers did not respond.

keep9_appearance_p1<- keep9[!is.na(keep9$postharvest1_tuberappearance_pos) | !is.na(keep9$postharvest1_tuberappearance_neg),] # delete 21 farmers data from 430 farmers data


summary(keep9_appearance_p1[trait_appearance_d5])
keep9_appearance_p1[trait_appearance_d5]  
sum(is.na(keep9_appearance_p1[trait_appearance_d5] ))
# after deleting farmers who didn't respond both questions. 3 Farmers respond only neg.


keep9_tubersize_keep <- apply(keep9_appearance_p1[trait_appearance_d5], 1, function(x){sum(is.na(x))})
keep9_tubersize_keep <- keep9_tubersize_keep <= 1 



R_tuberapp <-  rankTricot(keep9_appearance_p1[keep9_tubersize_keep,], 
                          items = variety,
                          input = trait_appearance_d5)

mod_tuber_app <- PlackettLuce(R_tuberapp)

summary(mod_tuber_app)


summary(mod_tuber_app, ref = ref)
capture.output(summary(mod_tuber_app, ref = ref), 
               print("model coefficients(maturity post harvest tuber appearance (5 days after harvest))"),
               coef(mod_tuber_app, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato harvest tuber appearance post-harvest 1 (5 days after harvest).txt")


plot(qvcalc(mod_tuber_app, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ tuber appearance post-harvest 1 (5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_tuber_app, ref = ref), main = "tuber appearance post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_tuber_app <- adjacency(R_tuberapp)

#a_tuber_app  <- network(R_tuberapp )
#plot(a_tuber_app)

#png(filename = "output_thesis/network_potato_tuber appearance post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
#plot(a_tuber_app)
#dev.off()

# worth plot about potato_tuber size post-harvest 1 (5 days after harvest)
plot_worth(mod_tuber_app)


png(filename = "output_thesis/worth plot about potato_tuber appearance post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_tuber_app) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

#10.###########################################
# tricot into a PlackettLuce model about post harvest market able at harvest (5 days after harvest)) 

trait_marketable_5d <- c("postharvest1_marketableatharv_pos","postharvest1_marketableatharv_neg")


is.na(dt3[,c("postharvest1_marketableatharv_pos","postharvest1_marketableatharv_neg")])
na_check <- is.na(dt3[,c("postharvest1_marketableatharv_pos","postharvest1_marketableatharv_neg")])
summary(na_check) #check NA. 


keep10<- dt3[]
keep10[trait_marketable_5d]

is.na(keep10[trait_marketable_5d])
sum(is.na(keep10[trait_marketable_5d])) 
subset(keep10[trait_marketable_5d],is.na(keep10[,"postharvest1_marketableatharv_pos"]) & is.na(keep10[,"postharvest1_marketableatharv_neg"]))
summary(subset(keep10[trait_appearance_d5],is.na(keep10[,"postharvest1_marketableatharv_pos"]) & is.na(keep10[,"postharvest1_marketableatharv_neg"])))
#173 farmers did not respond both of questions: pos & neg 

keep10_marketable_5d<- keep10[!is.na(keep10$postharvest1_marketableatharv_pos) | !is.na(keep10$postharvest1_marketableatharv_neg),] # delete 152 farmers data from 430 farmers data
summary(keep10_marketable_5d[trait_marketable_5d]) #259 farmers left
keep10_marketable_5d[trait_marketable_5d]


sum(is.na(keep10_marketable_5d[trait_marketable_5d]))
# after deleting farmers who didn't respond both questions. 2 Farmers respond only one answer

keep10_marketable_keep <- apply(keep10_marketable_5d[trait_marketable_5d], 1, function(x){sum(is.na(x))})
keep10_marketable_keep <- keep10_marketable_keep <= 1 




R_marketableatharv <-  rankTricot(keep10_marketable_5d[keep10_marketable_keep,], 
                                  items = variety,
                                  input = trait_marketable_5d)

mod_marketableatharv <- PlackettLuce(R_marketableatharv)

summary(mod_marketableatharv)



summary(mod_marketableatharv, ref = ref)

capture.output(summary(mod_marketableatharv, ref = ref), 
               print("model coefficients(maturity post harvest marketableatharv (5 days after harvest))"),
               coef(mod_marketableatharv, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato marketableatharv post harvest(5 days after harvest).txt")


plot(qvcalc(mod_marketableatharv, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato marketableatharv post harvest(5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_marketableatharv, ref = ref), main = "potato marketableatharv post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_marketableatharv <- adjacency(R_marketableatharv)

#a_marketableatharv  <- network(a_marketableatharv )
#plot(a_marketableatharv )

#worth plot about potato market able at harvest post-harvest 1 (5 days after harvest)

plot_worth(mod_marketableatharv)

png(filename = "output_thesis/worth plot about potato marketableatharv post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_marketableatharv) + theme_bw() +theme(panel.grid = element_blank())
dev.off()




#11.###############################################
# tricot into a PlackettLuce rankings about post harvest taste (5 days after harvest)) 
# tricot into a PlackettLuce rankings about post harvest taste (5 days after harvest)) 
trait_taste_d5<- c("postharvest1_taste_pos","postharvest1_taste_neg")

na_check <- is.na(dt3[,c("postharvest1_taste_pos","postharvest1_taste_neg")])
summary(na_check)


keep11<- dt3[]

keep11[trait_taste_d5]

is.na(keep11[trait_taste_d5])
sum(is.na(keep11[trait_taste_d5]))

subset(keep11[trait_taste_d5],is.na(keep11[,"postharvest1_taste_pos"]) & is.na(keep11[,"postharvest1_taste_neg"]))
summary(subset(keep11[trait_taste_d5],is.na(keep11[,"postharvest1_taste_pos"]) & is.na(keep11[,"postharvest1_taste_neg"])))
#41farmers did not respond both of questions: pos & neg 

keep11_taste_d5 <- keep11[!is.na(keep11$postharvest1_taste_pos) | !is.na(keep11$postharvest1_taste_neg),] # delete 13 farmers data from 430 farmers data
summary(keep11_taste_d5[trait_taste_d5]) #417 farmers remained
keep11_taste_d5[trait_taste_d5]

sum(is.na(keep11_taste_d5[trait_taste_d5]))
# after deleting farmers who didn't respond both questions. No NA 
#seek blow procedure
#keep11_taste_d5keep <- apply(keep11_taste_d5[trait_taste_d5], 1, function(x){sum(is.na(x))})
#keep11_taste_d5keep <- keep11_taste_d5keep <= 1 



R_taste <-  rankTricot(keep11_taste_d5[], 
                       items = variety,
                       input = trait_taste_d5)
mod_taste<- PlackettLuce(R_taste)

summary(mod_taste)

summary(mod_taste, ref = ref)
capture.output(summary(mod_taste, ref = ref), 
               print("model coefficients(maturity post harvest taste (5 days after harvest))"),
               coef(mod_taste, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato taste post harvest(5 days after harvest).txt")


plot(qvcalc(mod_taste, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato taste post harvest(5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_taste, ref = ref), main = "potato taste post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_taste <- adjacency(R_taste)

#a_taste  <- network(a_taste)
#plot(a_taste)

#png(filename = "output_thesis/network_potato taste post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
#plot(a_taste)
#dev.off()

# worth plot about  potato taste post-harvest 1 (5 days after harvest)
plot_worth(mod_taste)

png(filename = "output_thesis/worth plot about potato taste post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_taste) + theme_bw() +theme(panel.grid = element_blank())
dev.off()




#12.##################################################
###tricot into a PlackettLuce rankings about post harvest_ marketable store (5 days after harvest)
trait_store <- c("postharvest1_marketableatstor_pos","postharvest1_marketableatstor_neg")
na_check <- is.na(dt3[,c("postharvest1_marketableatstor_pos","postharvest1_marketableatstor_neg")])
summary(na_check) #however, there are "Not observed" data. lets change "Not observed" to NA

keep12<- dt3[]
keep12[trait_store]


is.na(keep12[trait_store])
subset(keep12[trait_store],is.na(keep12[,"postharvest1_marketableatstor_pos"]) & is.na(keep12[,"postharvest1_marketableatstor_neg"]))
summary(subset(keep12[trait_store],is.na(keep12[,"postharvest1_marketableatstor_pos"]) & is.na(keep12[,"postharvest1_marketableatstor_neg"])))
#298 farmers did not respond both of questions: pos & neg 

keep12_store <- keep12[!is.na(keep12$postharvest1_marketableatstor_pos) | !is.na(keep12$postharvest1_marketableatstor_neg),] # delete 298 farmers data from 430 farmers data
summary(keep12_store[trait_store]) #134 farmers left
keep12_store[trait_store]

sum(is.na(keep12_store[trait_store]))
# after deleting farmers who didn't respond both questions. No NA 
#seek blow procedure
#keep12_store_k <- apply(keep12_store[trait_store], 1, function(x){sum(is.na(x))})
#keep12_store_k <- keep12_store_k <= 1 

R_tuberstore <-  rankTricot(keep12_store[], 
                            items = variety,
                            input = trait_store)


mod_tuberstore <- PlackettLuce(R_tuberstore)

summary(mod_tuberstore)
summary(mod_tuberstore, ref = ref)
capture.output(summary(mod_tuberstore, ref = ref), 
               print("model coefficients(post harvest tubarstore (5 days after harvest))"),
               coef(mod_tuberstore, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato tubarstore post harvest(5 days after harvest).txt")


plot(qvcalc(mod_tuberstore, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ tubarstore post-harvest 1 (5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_tuberstore, ref = ref), main = "tubarstore post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_tuberstore <- adjacency(R_tuberstore)

#a_tuberstore  <- network(a_tuberstore)
#plot(a_tuberstore)

# worth plot about potato_tuber store post-harvest 1 (5 days after harvest)

plot_worth(mod_tuberstore) 

png(filename = "output_thesis/worth plot about potato_tuber store post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_tuberstore)+ theme_bw() +theme(panel.grid = element_blank())
dev.off()


###################################
################################
#############################
#check farmers statement during post-harvest 2 (5 days after harvest) stage
# however, in this stage, some farmers' data did not have survey_start and survey_end data. It might be code error when it download
dt4 <- dt[]
#326 farmers left


###############################Post-harvest 2 (45 days after harvest)################################
# Post-harvest 2 (45 days after harvest) stage means that farmers respond tricot questions after 45 days

########################################################################
#13.#######tricot into a PlackettLuce model  about easiest to sell after storage (Day 45)


trait_marketable_45d <- c("postharvest2_marketableatstor_pos","postharvest2_marketableatstor_neg")

#checking NA in columns 
na_check <- is.na(dt4[trait_marketable_45d])
summary(na_check)


keep13_marketable<- dt4[]

keep13_marketable[trait_marketable_45d]
is.na(keep13_marketable[trait_marketable_45d])
summary(is.na(keep13_marketable[trait_marketable_45d])) #NA check
#156 farmer did not respond 

subset(keep13_marketable[trait_marketable_45d],is.na(keep13_marketable[,"postharvest2_marketableatstor_pos"]) & is.na(keep13_marketable[,"postharvest2_marketableatstor_neg"]))
summary(subset(keep13_marketable[trait_marketable_45d],is.na(keep13_marketable[,"postharvest2_marketableatstor_pos"]) & is.na(keep13_marketable[,"postharvest2_marketableatstor_neg"])))
#156 farmers did not respond both of questions: pos & neg 
#let's delete 156 farmers  who did not answer both qeustions


subset(keep13_marketable[,trait_marketable_45d],is.na(keep13_marketable[,"postharvest2_marketableatstor_pos"]) & is.na(keep13_marketable[,"postharvest2_marketableatstor_neg"]))
summary(subset(keep13_marketable[,trait_marketable_45d],is.na(keep13_marketable[,"postharvest2_marketableatstor_pos"]) & is.na(keep13_marketable[,"postharvest2_marketableatstor_neg"])))

keep13dt_NA<- keep13_marketable[!is.na(keep13_marketable$postharvest2_marketableatstor_pos) | !is.na(keep13_marketable$postharvest2_marketableatstor_neg),]
# delete 156farmers data 

keep13dt_NA[trait_marketable_45d]
summary(keep13dt_NA[trait_marketable_45d])
#276 farmers data remained 


R_tuberstore2 <-  rankTricot(keep13dt_NA[], 
                             items = variety,
                             input = trait_marketable_45d)


mod_tuberstore2 <- PlackettLuce(R_tuberstore2)

summary(mod_tuberstore2)

summary(mod_tuberstore2, ref = ref)
capture.output(summary(mod_tuberstore2, ref = ref), 
               print("model coefficients( post harvest 2 tubarstore (45 days after harvest)"),
               coef(mod_tuberstore2, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_post_harvest2_tubarstore_(45 days after harvest).txt")


plot(qvcalc(mod_tuberstore2, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ tubarstore post-harvest 2 (45 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_tuberstore2, ref = ref), main = "tubarstore post-harvest 2 (45 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_tuberstore2 <- adjacency(R_tuberstore2)

#a_tuberstore2  <- network(a_tuberstore2)
#plot(a_tuberstore2)

# worth plot about potato_tuber store post-harvest 2 (45 days after harvest)

plot_worth(mod_tuberstore2) 


#13-2####plackett luce tree with Rhomis
#no available environment covariate 


keep13dt_NA_c<- keep13dt_NA[which(!is.na(keep13dt_NA[,"RHoMIS_id"])),]

#almost same above code but here use grop = true.
G <-  rankTricot(keep13dt_NA_c[], 
                 items = variety,
                 input = trait_marketable_45d,
                 group = TRUE)


# covariates



covar <- keep13dt_NA_c[,c(162:184)]


# rankings 

pld <- cbind(G, covar)

head(pld)




# run a forward selection with cross-validation
# to select the best model
# Define initial parameters 

# set the baseline as a deviance from a Plackett-Luce model without covariates
baseline <- deviance(PlackettLuce(G))
# vector to keep best explanatory variables
covar_keep <- character(0L)
# keep running if TRUE
best <- TRUE
# number of runs
counter <- 1
# a list to keep the goodness-of-fit coefficients from each step
modelcoeffs <- list()
# get the names of explanatory and response variables
covarnames <- names(covar)
siglevel <- 0.1
minsize <- 25
npseudo <- 0.8
bonferroni <- TRUE
gamma <- TRUE

# keep running until the model get its best performance
while(best){
  
  message("Run " , counter, "\n")
  
  fs <- length(covarnames)
  
  args <- list(formula = NULL,
               data = pld,
               alpha = siglevel,
               npseudo = npseudo,
               bonferroni = bonferroni,
               gamma = gamma,
               minsize = minsize)
  
  deviances <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste0("G ~ ", paste(c(covar_keep, covarnames[i]), collapse = " + ")))
    args$formula <- f_i
    pl_i <- do.call("pltree", args)
    deviances <- c(deviances, deviance(pl_i))
  }
  
  best_dv <- which.min(deviances)
  
  value_best <- min(deviances)
  
  # take the name of best variable
  best_model <- covarnames[best_dv]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("G ~ ", paste(paste(covar_keep, collapse = " "), covarnames))
  call_m <- data.frame(cbind(call = call_m, deviance = deviances))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  if(best){
    
    # remove best variable for the next run
    covarnames <- covarnames[!covarnames %in% best_model]
    
    # keep this model for the next run
    covar_keep <- c(covar_keep, best_model)
    
    message("Best model found: ",
            paste0("G ~ ", paste(covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(covarnames) == 0) {
    best <- FALSE
  }
  
}


covar_keep

f <- formula(paste0("G ~ ", paste(covar_keep, collapse = " + ")))

f

tree <- pltree(f, 
               data = pld, 
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

tree
plot(tree)

png(filename = "output_main_cov/Plackett-Luce tree  marketableatstor post-harvest 2 (45 days after harvest)with RhoMis covariates.png", width = 40, height = 30, units = "cm", res = 400)
plot(tree)
dev.off()


gosset:::plot_tree(tree)

png(filename = "output_main_cov/Plackett-Luce tree  marketableatstor post-harvest 2 (45 days after harvest)with RhoMis covariates_gosset.png", width = 40, height = 30, units = "cm", res = 400)
gosset:::plot_tree(tree)
dev.off()

# Pladmm model
#13-2-analyze item_ covariate 
rhomis_t <- keep13dt_NA_c[]
which(!is.na(rhomis_t$RHoMIS_id))
rhomis_t <- rhomis_t[which(!is.na(rhomis_t$RHoMIS_id)),]


#pld = rhomis_t

tree <- pltree(G ~ Crop_Income_USD,
               data = pld,
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

plot(tree)
predict(tree, type = "node")
table(predict(tree, type = "node"))


partition_nod1<- subset(rhomis_t[], predict(tree, type = "node") == 2)
partition_nod2<- subset(rhomis_t[], predict(tree, type = "node") == 4)
partition_nod3<- subset(rhomis_t[], predict(tree, type = "node") == 5)

#save each node data set to analyze correlation ship!!! -> move to the script "Partial correlations JvR.R" 
write.table(partition_nod1,file="output_thesis/marketafterstore_ph2_node1.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod2,file="output_thesis/marketafterstore_ph2_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(partition_nod3,file="output_thesis/marketafterstore_ph2_node3.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

##
##node 2(node1)
# Run packettluce modle about node2(partition_nod1)
#prepare it to run forward selection
ig<- rankTricot(partition_nod1[], 
                items = variety,
                input = trait_marketable_45d)

item_cov<- tuber_blup[]
item_covariance_name<- names(tuber_blup[-1])
counter <- 1
modelcoeffs <- list()
item_covar_keep<- character(0L)

#put all variable because to set highest AIC value 
baseline <- AIC(pladmm(ig, ~ flowering_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                       + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8))
#however, sometime pladmm model that has all variable has lower AIC than having less variable version... so make value big!!
baseline_keep <- baseline
baseline<- baseline + 100


# Be aware it! if you run it once as auto, it cannot worwk... Then, please manually run it the code and after second arround run it as auto. then it will work
# running until to find the best model 

while(best){
  message("Run " , counter, "\n")
  fs <- length(item_covariance_name)
  args <- list(formula = NULL,
               data = tuber_blup[],
               rho= 8 )
  AIC_value <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste( paste("~", c(item_covar_keep, item_covariance_name[i]), collapse = " + ")))
    args$formula <-f_i
    pl_i<- pladmm(ig,args$formula, data = tuber_blup,rho= 8  )
    AIC_value <- c(AIC_value, AIC(pl_i))
  }
  
  best_aic <- which.min(AIC_value)
  
  value_best <- min(AIC_value)
  
  # take the name of best variable
  best_model <- item_covariance_name[best_aic]
  
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("~ ", paste(paste(item_covar_keep, collapse = " "), item_covariance_name))
  call_m <- data.frame(cbind(call = call_m, AIC = AIC_value))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  
  if(best){
    
    # remove best variable for the next run
    item_covariance_name <- item_covariance_name[!item_covariance_name %in% best_model]
    
    # keep this model for the next run
    item_covar_keep <- c(item_covar_keep, best_model)
    
    message("Best model found: ",
            paste0(" ~ ", paste(item_covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(item_covariance_name) == 0) {
    best <- FALSE}
}


item_covar_keep


f <- formula(paste0("~", paste(item_covar_keep, collapse = " + ")))

f
regressionPL1_test<- pladmm(ig, f,data = tuber_blup[], rho = 8)
summary(regressionPL1_test)

AIC(regressionPL1_test)

#save the result. you do not want to play above colde... its toooo long to run 

capture.output(summary(regressionPL1_test), 
               print("item_covariate_marketabilityafterstore_BW(ph2)_node2_rhomis"),
               file ="output_thesis/item_covariate_marketabilityafterstore_BW(ph2)_node2_rhomis_con_fowardsel.txt")


##node 4(node2)
# Run packettluce modle about node4(partition_nod2)
ig<- rankTricot(partition_nod2[], 
                items = variety,
                input = trait_marketable_45d)

#prepare it to run forward selection
item_cov<- tuber_blup[]
item_covariance_name<- names(tuber_blup[-1])
counter <- 1
modelcoeffs <- list()
item_covar_keep<- character(0L)
#put all variable because to set highest AIC value 
baseline <- AIC(pladmm(ig, ~ flowering_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                       + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8))

#however, sometime pladmm model that has all variable has lower AIC than having less variable version... so make value big!!
baseline_keep <- baseline
baseline<- baseline + 100


# Be aware it! if you run it once as auto, it cannot worwk... Then, please manually run it the code and after second arround run it as auto. then it will work
# running until to find the best model 


while(best){
  message("Run " , counter, "\n")
  fs <- length(item_covariance_name)
  args <- list(formula = NULL,
               data = tuber_blup[],
               rho= 8 )
  AIC_value <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste( paste("~", c(item_covar_keep, item_covariance_name[i]), collapse = " + ")))
    args$formula <-f_i
    pl_i<- pladmm(ig,args$formula, data = tuber_blup,rho= 8  )
    AIC_value <- c(AIC_value, AIC(pl_i))
  }
  
  best_aic <- which.min(AIC_value)
  
  value_best <- min(AIC_value)
  
  # take the name of best variable
  best_model <- item_covariance_name[best_aic]
  
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("~ ", paste(paste(item_covar_keep, collapse = " "), item_covariance_name))
  call_m <- data.frame(cbind(call = call_m, AIC = AIC_value))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  
  if(best){
    
    # remove best variable for the next run
    item_covariance_name <- item_covariance_name[!item_covariance_name %in% best_model]
    
    # keep this model for the next run
    item_covar_keep <- c(item_covar_keep, best_model)
    
    message("Best model found: ",
            paste0(" ~ ", paste(item_covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(item_covariance_name) == 0) {
    best <- FALSE}
}


item_covar_keep


f <- formula(paste0("~", paste(item_covar_keep, collapse = " + ")))

f
regressionPL1_test<- pladmm(ig, f,data = tuber_blup[], rho = 8)
summary(regressionPL1_test)

AIC(regressionPL1_test)

#save the result. you do not want to play above colde... its toooo long to run 

capture.output(summary(regressionPL1_test), 
               print("item_covariate_marketabilityafterstore_BW(postharvest2)_node2_rhomis"),
               file ="output_thesis/item_covariate_marketabilityafterstore_BW(postharvest2)_node2_rhomis_con_fowardsel.txt")


#node 5 (node3)
# Run packettluce modle about node5(partition_nod3)

ig<- rankTricot(partition_nod3[], 
                items = variety,
                input = trait_marketable_45d)



#prepare it to run forward selection
item_cov<- tuber_blup[]
item_covariance_name<- names(tuber_blup[-1])
counter <- 1
modelcoeffs <- list()
item_covar_keep<- character(0L)

#put all variable because to set highest AIC value 
baseline <- AIC(pladmm(ig, ~ flowering_prediction +plant_vigor_prediction+average_nber_of_tubers_per_plant_prediction
                       + totat_wight_of_tubers_ton_ha_prediction + late_blight_occurrence + Leaf_miner_flies_occurrence + viruses_occurrence + bacterial_wilt_occurrence, data = tuber_blup, rho = 8))
baseline_keep <- baseline
#however, sometime pladmm model that has all variable has lower AIC than having less variable version... so make value big!!

baseline<- baseline + 100

# Be aware it! if you run it once as auto, it cannot worwk... Then, please manually run it the code and after second arround run it as auto. then it will work
# running until to find the best model 


while(best){
  message("Run " , counter, "\n")
  fs <- length(item_covariance_name)
  args <- list(formula = NULL,
               data = tuber_blup[],
               rho= 8 )
  AIC_value <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste( paste("~", c(item_covar_keep, item_covariance_name[i]), collapse = " + ")))
    args$formula <-f_i
    pl_i<- pladmm(ig,args$formula, data = tuber_blup,rho= 8  )
    AIC_value <- c(AIC_value, AIC(pl_i))
  }
  
  best_aic <- which.min(AIC_value)
  
  value_best <- min(AIC_value)
  
  # take the name of best variable
  best_model <- item_covariance_name[best_aic]
  
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("~ ", paste(paste(item_covar_keep, collapse = " "), item_covariance_name))
  call_m <- data.frame(cbind(call = call_m, AIC = AIC_value))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  
  if(best){
    
    # remove best variable for the next run
    item_covariance_name <- item_covariance_name[!item_covariance_name %in% best_model]
    
    # keep this model for the next run
    item_covar_keep <- c(item_covar_keep, best_model)
    
    message("Best model found: ",
            paste0(" ~ ", paste(item_covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(item_covariance_name) == 0) {
    best <- FALSE}
}


item_covar_keep


f <- formula(paste0("~", paste(item_covar_keep, collapse = " + ")))

f
regressionPL1_test<- pladmm(ig, f,data = tuber_blup[], rho = 8)
summary(regressionPL1_test)

AIC(regressionPL1_test)


#save the result. you do not want to play above colde... its toooo long to run 


capture.output(summary(regressionPL1_test), 
               print("item_covariate_marketabilityafterstore_BW(postharvest2)_node3_rhomis"),
               file ="output_thesis/item_covariate_marketabilityafterstore_BW(postharvest2)_node3_rhomis_con_fowardsel.txt")






#14.##################################################################################################
########tricot into a PlackettLuce model  about dromancy(long or short)  (Day 45)

trait_dormancy2 <- c("postharvest2_dormancy_pos","postharvest2_dormancy_neg")

#checking NA in columns 
na_check <- is.na(dt4[trait_dormancy2])
summary(na_check)


keep14_dor<- dt4[]

keep14_dor[trait_dormancy2]
is.na(keep14_dor[trait_dormancy2])
summary(is.na(keep14_dor[trait_dormancy2])) #NA check

subset(keep14_dor[trait_dormancy2],is.na(keep14_dor[,"postharvest2_dormancy_pos"]) & is.na(keep14_dor[,"postharvest2_dormancy_pos"]))
summary(subset(keep14_dor[trait_dormancy2],is.na(keep14_dor[,"postharvest2_dormancy_pos"]) & is.na(keep14_dor[,"postharvest2_dormancy_pos"])))
#156 farmers did not respond both of questions: pos & neg 
#let's delete 156 farmers  who did not answer both questions 

keep14_NA<- keep14_dor[!is.na(keep14_dor$postharvest2_dormancy_pos) | !is.na(keep14_dor$postharvest2_dormancy_pos),]
# delet 156 farmers data from 242 farmers data 

keep14_NA[trait_dormancy2]
summary(keep14_NA[trait_dormancy2])
#276 farmers left


R_dor2 <-  rankTricot(keep14_NA[], 
                      items = variety,
                      input = trait_dormancy2)


mod_dor2 <- PlackettLuce(R_dor2)

summary(mod_dor2)

summary(mod_dor2, ref = ref)
capture.output(summary(mod_dor2, ref = ref), 
               print("model coefficients( post harvest 2 dormancy (45 days after harvest)"),
               coef(mod_dor2, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_post_harvest2_dormancy_(45 days after harvest).txt")


plot(qvcalc(mod_dor2, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ dormancy post-harvest 2 (45 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_dor2, ref = ref), main = "dormancy post-harvest 2 (45 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_dor2 <- adjacency(R_dor2)

#a_dor2  <- network(a_dor2)
#plot(a_dor2)

# worth plot about potato_tuber dromancy post-harvest 2 (45 days after harvest)

plot_worth(mod_dor2)

png(filename = "output_thesis/worth plot about dormancy post-harvest 2 (45 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_dor2) + theme_bw() +theme(panel.grid = element_blank())
dev.off()




#15.###################################################################################################################
########tricot into a PlackettLuce model  about taste (Day 45)

trait_taste2 <- c("postharvest2_taste_pos","postharvest2_taste_neg")

#checking NA in columns 
na_check <- is.na(dt4[trait_taste2])
summary(na_check)


keep15_taste<- dt4[]

keep15_taste[trait_taste2]
is.na(keep15_taste[trait_taste2])
summary(is.na(keep15_taste[trait_taste2])) #NA check

subset(keep15_taste[trait_taste2],is.na(keep15_taste[,"postharvest2_taste_pos"]) & is.na(keep15_taste[,"postharvest2_taste_neg"]))

summary(subset(keep15_taste[trait_taste2],is.na(keep15_taste[,"postharvest2_taste_pos"]) & is.na(keep15_taste[,"postharvest2_taste_neg"])))
#38 farmers did not respond both of questions: pos & neg 
#let's delete 38 farmers  who did not answer both qeustions

keep15_taste<- keep15_taste[!is.na(keep15_taste$postharvest2_taste_pos) | !is.na(keep15_taste$postharvest2_taste_neg),]
# delet 38 farmers data from 264 farmers data 

keep15_taste[trait_taste2]
summary(keep15_taste[trait_taste2])
#288 farmers left


R_taste2 <-  rankTricot(keep15_taste[], 
                        items = variety,
                        input = trait_taste2)


mod_taste2 <- PlackettLuce(R_taste2)

summary(mod_taste2)

summary(mod_taste2, ref = ref)
capture.output(summary(mod_taste2, ref = ref), 
               print("model coefficients( post harvest 2 taste (45 days after harvest)"),
               coef(mod_taste2, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_post_harvest2_taste_(45 days after harvest).txt")


plot(qvcalc(mod_taste2, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ taste post-harvest 2 (45 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_taste2, ref = ref), main = "taste post-harvest 2 (45 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_taste2 <- adjacency(R_taste2)

#a_taste2  <- network(a_taste2)
#plot(a_taste2)

# worth plot about potato_tuber taste post-harvest 2 (45 days after harvest)
plot_worth(mod_taste2)

png(filename = "output_thesis/worth plot about taste post-harvest 2 (45 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_taste2) + theme_bw() +theme(panel.grid = element_blank())
dev.off()



##########################################################################
############################### Post-harvest 3 (60 days after harvest)################################
# Post-harvest 3 (60 days after harvest) stage means that farmers respond tricot questions after 60 days from harvest.

dt5 <- dt[]

#16.###############################################################3
#####tricot into a PlackettLuce model  about dormancy (Day 60)

trait_dor3 <- c("postharvest3_dormancy_pos","postharvest3_dormancy_neg")

#checking NA in columns 
na_check <- is.na(dt5[trait_dor3])
summary(na_check)


keep16_dor3<- dt5[]

keep16_dor3[trait_dor3]
is.na(keep16_dor3[trait_dor3])
summary(is.na(keep16_dor3[trait_dor3])) #NA check 


subset(keep16_dor3[trait_dor3],is.na(keep16_dor3[,"postharvest3_dormancy_pos"]) & is.na(keep16_dor3[,"postharvest3_dormancy_neg"]))

summary(subset(keep16_dor3[trait_dor3],is.na(keep16_dor3[,"postharvest3_dormancy_pos"]) & is.na(keep16_dor3[,"postharvest3_dormancy_neg"])))

# delete 123 farmers data from farmers data set

keep16_dor3<- keep16_dor3[!is.na(keep16_dor3$postharvest3_dormancy_pos) | !is.na(keep16_dor3$postharvest3_dormancy_neg),]
keep16_dor3[trait_dor3]
summary(keep16_dor3[trait_dor3])
#309 farmers left

keep16_dor3[trait_dor3]

R_dor3 <-  rankTricot(keep16_dor3[], 
                      items = variety,
                      input = trait_dor3)


mod_dor3  <- PlackettLuce(R_dor3)

summary(mod_dor3)

summary(mod_dor3, ref = ref)
capture.output(summary(mod_dor3, ref = ref), 
               print("model coefficients( post harvest 3 dorrmancy (60 days after harvest)"),
               coef(mod_dor3, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_post_harvest3_dormancy_(60 days after harvest).txt")


plot(qvcalc(mod_dor3, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ dormancy post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_dor3, ref = ref), main = "dormancy post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_dor3 <- adjacency(R_dor3)

#a_dor3  <- network(a_dor3)
#plot(a_dor3)

# worth plot about potato_tuber dormancy post-harvest 3 (60 days after harvest)
plot_worth(mod_dor3)

png(filename = "output_thesis/worth plot about dormancy post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_dor3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()




#17.###################################################################################################################
########tricot into a PlackettLuce model  about tuber quality (Day 60)

trait_q3 <- c("postharvest3_tuberquality_pos","postharvest3_tuberquality_neg")

#checking NA in columns 
na_check <- is.na(dt5[trait_q3])
summary(na_check)


keep17_q3<- dt5[]

keep17_q3[trait_q3]
is.na(keep17_q3[trait_q3])
summary(is.na(keep17_q3[trait_q3])) #NA check 


subset(keep17_q3[trait_q3],is.na(keep17_q3[,"postharvest3_tuberquality_pos"]) & is.na(keep17_q3[,"postharvest3_tuberquality_pos"]))

summary(subset(keep17_q3[trait_q3],is.na(keep17_q3[,"postharvest3_tuberquality_pos"]) & is.na(keep17_q3[,"postharvest3_tuberquality_pos"])))
#351 farmers did not respond both of questions: pos & neg 
#let's delete 351 farmers  who did not answer both qeustions

keep17_q3<- keep17_q3[!is.na(keep17_q3$postharvest3_tuberquality_pos) | !is.na(keep17_q3$postharvest3_tuberquality_pos),]
# delete 112 farmers data from farmers data 

keep17_q3[trait_q3]
summary(keep17_q3[trait_q3])
#81 farmers left



R_q3 <-  rankTricot(keep17_q3[], 
                    items = variety,
                    input = trait_q3)


mod_q3  <- PlackettLuce(R_q3)

summary(mod_q3)

coef(mod_q3, ref)
coef(mod_q3, log = FALSE)

summary(mod_q3, ref = ref)
capture.output(summary(mod_q3, ref = ref), 
               print("model coefficients( post harvest 3 tuber quality (60 days after harvest)"),
               coef(mod_q3, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_post_harvest3_tuber quality_(60 days after harvest).txt")


plot(qvcalc(mod_q3, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ tuber quality post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_q3, ref = ref), main = "tuber quality post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_q3 <- adjacency(R_q3)

#a_q3  <- network(a_q3)
#plot(a_q3)

# worth plot about potato_tuber quality post-harvest 3 (60 days after harvest)

plot_worth(mod_q3) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_thesis/worth plot about tuber quality post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_q3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()


#####
####################################################################
#18.###########################################################
##tricot into a PlackettLuce model  about overall performance  (Day 60)
#Here assessed varieties overall performance during entilre stages.
#therefore, get all farmers data, no matter farmers failed to preserve tuber or maintain trial field on some stages.

trait_overallper3 <- c("postharvest3_overallper_pos","postharvest3_overallper_neg")

#checking NA in columns 
na_check <- is.na(dt[trait_overallper3])
summary(na_check)


keep18_q3<- dt[]
keep18_q3[trait_overallper3]
is.na(keep18_q3[trait_overallper3])
summary(is.na(keep18_q3[trait_overallper3])) #NA check 


subset(keep18_q3[trait_overallper3],is.na(keep18_q3[,"postharvest3_overallper_pos"]) & is.na(keep18_q3[,"postharvest3_overallper_neg"]))
summary(subset(keep18_q3[trait_overallper3],is.na(keep18_q3[,"postharvest3_overallper_pos"]) & is.na(keep18_q3[,"postharvest3_overallper_neg"])))
#123 farmers did not respond both of questions: pos & neg 
#let's delete 123farmers  who did not answer both questions

keep18_q3<- keep18_q3[!is.na(keep18_q3$postharvest3_overallper_pos) | !is.na(keep18_q3$postharvest3_overallper_pos),]
# delet 123 farmers data from 463 farmers data 

keep18_q3[trait_overallper3]
summary(keep18_q3[trait_overallper3])
#309 farmers left



R_o3 <-  rankTricot(keep18_q3[], 
                    items = variety,
                    input = trait_overallper3)


mod_o3  <- PlackettLuce(R_o3)

summary(mod_o3)

summary(mod_o3, ref = ref)
capture.output(summary(mod_o3, ref = ref), 
               print("model coefficients( post harvest 3 overall performance (60 days after harvest)"),
               coef(mod_o3, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_post_harvest3_overall performance_(60 days after harvest).txt")


plot(qvcalc(mod_o3, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ tuber overall performance post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_o3, ref = ref), main = "tuber overall performance post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_o3 <- adjacency(R_o3)

#a_o3  <- network(a_o3)
#plot(a_o3)

# worth plot about overall performance  (Day 60)

plot_worth(mod_o3) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_thesis/worth plot about tuber  overall performance post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_o3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

png(filename = "output_thesis/worth plot about tuber  overall performance post-harvest 3 (60 days after harvest)_ver2.png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_o3) + theme_bw() +theme(panel.grid = element_blank()) +  theme(text = element_text(size = 16))  + labs(y = "Varieties", x = "The probability of winning (worth)")
dev.off()




#19.########################################################################
##tricot into a PlackettLuce model about plant for future  (Day 60)

trait_f3 <- c("postharvest3_plantfuture_pos","postharvest3_plantfuture_neg")

#checking NA in columns 
na_check <- is.na(dt[trait_f3])
summary(na_check)


keep19_f3<- dt[]

keep19_f3[trait_f3]
is.na(keep19_f3[trait_f3])
summary(is.na(keep19_f3[trait_f3])) #NA check 


subset(keep19_f3[trait_f3],is.na(keep19_f3[,"postharvest3_plantfuture_pos"]) & is.na(keep19_f3[,"postharvest3_plantfuture_neg"]))
summary(subset(keep19_f3[trait_f3],is.na(keep19_f3[,"postharvest3_plantfuture_pos"]) & is.na(keep19_f3[,"postharvest3_plantfuture_neg"])))
#123 farmers did not respond both of questions: pos & neg 
#let's delete 123 farmers  who did not answer both questions

keep19_f3<- keep19_f3[!is.na(keep19_f3$postharvest3_plantfuture_pos) | !is.na(keep19_f3$postharvest3_plantfuture_neg),]
# delet 123 farmers data from 248 farmers data 

keep19_f3[trait_f3]
summary(keep19_f3[trait_f3])
#309 farmers left



R_f3 <-  rankTricot(keep19_f3[], 
                    items = variety,
                    input = trait_f3)


mod_f3  <- PlackettLuce(R_f3)

summary(mod_f3)

summary(mod_f3, ref = ref)
capture.output(summary(mod_f3, ref = ref), 
               print("model coefficients( post harvest 3 plant for future (60 days after harvest)"),
               coef(mod_f3, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_post_harvest3_plant for future_(60 days after harvest).txt")


plot(qvcalc(mod_f3, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ tuber plant for future post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_f3, ref = ref), main = "plant for future post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_f3 <- adjacency(R_f3)

#a_f3  <- network(a_f3)
#plot(a_f3)

# worth plot about potato_tuber plant for future post-harvest 3 (60 days after harvest)

plot_worth(mod_f3) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_thesis/worth plot about tuber  plant for future post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_f3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()





#20.#####################################################################################
###tricot into a PlackettLuce model about best overall (Day 60)



trait_best3 <- c("postharvest3_preference_pos","postharvest3_preference_neg")

#checking NA in columns 
na_check <- is.na(dt[trait_best3])
summary(na_check)


keep20_b3<- dt[]

keep20_b3[trait_best3]
is.na(keep20_b3[trait_best3])
summary(is.na(keep20_b3[trait_best3])) #NA check 


subset(keep20_b3[trait_best3],is.na(keep20_b3[,"postharvest3_preference_pos"]) & is.na(keep20_b3[,"postharvest3_preference_neg"]))

summary(subset(keep20_b3[trait_best3],is.na(keep20_b3[,"postharvest3_preference_pos"]) & is.na(keep20_b3[,"postharvest3_preference_neg"])))
#341 farmers did not respond both of questions: pos & neg 
#let's delete 341 farmers  who did not answer both questions

keep20_b3<- keep20_b3[!is.na(keep20_b3$postharvest3_preference_pos) | !is.na(keep20_b3$postharvest3_preference_neg),]


keep20_b3[trait_best3]
summary(keep20_b3[trait_best3])
#122 farmers left

R_b3 <-  rankTricot(keep20_b3[], 
                    items = variety,
                    input = trait_best3)


mod_b3  <- PlackettLuce(R_b3)

summary(mod_b3)

summary(mod_b3, ref = ref)
capture.output(summary(mod_b3, ref = ref), 
               print("model coefficients( post harvest 3 best overall (60 days after harvest)"),
               coef(mod_b3, ref = ref),
               file ="output_thesis/summary_mod_Coef_potato_post_harvest3_best overall_(60 days after harvest).txt")


plot(qvcalc(mod_b3, ref = ref))

png(filename = "output_thesis/quasi_standard_error_potato_ tuber best overall post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_b3, ref = ref), main = "best overall post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_b3 <- adjacency(R_b3)

#a_b3  <- network(a_b3)
#plot(a_b3)

# worth plot about best overall post-harvest 3 (60 days after harvest)


plot_worth(mod_b3) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_thesis/worth plot about tuber  best overall post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_b3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()


png(filename = "output_thesis/worth plot about tuber  best overall post-harvest 3 (60 days after harvest)_ver2.png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_b3) + theme_bw() +theme(panel.grid = element_blank()) +  theme(text = element_text(size = 16))  + labs(y = "Varieties", x = "The probability of winning (worth)")
dev.off()


#######################################################


###################################################################
#Explanation why farmer like
#reason why they prefer or not 
#reasom why they prefer 
reason_p<- dt$postharvest3_reasonmostpref  
reason_N<- dt$postharvest3_reasonleastpref

reason <-cbind(reason_p,reason_N)

sum(is.na(reason[,1]))
sum(is.na(reason[,2]))

View(table(reason[,1]))
############################compared  with local variety ##############################################
############################Overall preference, which one is best or what you usually use?##########################
#Here assessed varieties overall preference
#local check and best preference 
trait_best3 <- c("postharvest3_preference_pos","postharvest3_preference_neg")

#checking NA in columns 
na_check <- is.na(dt[trait_best3])
summary(na_check)


keep20_b3<- dt[]

keep20_b3[trait_best3]
is.na(keep20_b3[trait_best3])
summary(is.na(keep20_b3[trait_best3])) #NA check 


subset(keep20_b3[trait_best3],is.na(keep20_b3[,"postharvest3_preference_pos"]) & is.na(keep20_b3[,"postharvest3_preference_neg"]))

summary(subset(keep20_b3[trait_best3],is.na(keep20_b3[,"postharvest3_preference_pos"]) & is.na(keep20_b3[,"postharvest3_preference_neg"])))
#341 farmers did not respond both of questions: pos & neg 
#let's delete 341 farmers  who did not answer both questions

keep20_b3<- keep20_b3[!is.na(keep20_b3$postharvest3_preference_pos) | !is.na(keep20_b3$postharvest3_preference_neg),]


keep20_b3[trait_best3]
summary(keep20_b3[trait_best3])






#check local answer
keep20_b3[c("postharvest3_comparetolocal_a", "postharvest3_comparetolocal_b", "postharvest3_comparetolocal_c")]
summary(keep20_b3[is.na(keep20_b3$postharvest3_comparetolocal_a) & is.na(keep20_b3$postharvest3_comparetolocal_b) & is.na(keep20_b3$postharvest3_comparetolocal_c),])
length(keep20_b3[is.na(keep20_b3$postharvest3_comparetolocal_a) & is.na(keep20_b3$postharvest3_comparetolocal_b) & is.na(keep20_b3$postharvest3_comparetolocal_c),])
#none answer 105
summary(keep20_b3[!is.na(keep20_b3$postharvest3_comparetolocal_a) & !is.na(keep20_b3$postharvest3_comparetolocal_b) & !is.na(keep20_b3$postharvest3_comparetolocal_c),])
#207 farmers respond
#
keep_loc <- keep20_b3[!is.na(keep20_b3$postharvest3_comparetolocal_a) | !is.na(keep20_b3$postharvest3_comparetolocal_b)| !is.na(keep20_b3$postharvest3_comparetolocal_c),]
summary(keep_loc[c("postharvest3_comparetolocal_a", "postharvest3_comparetolocal_b", "postharvest3_comparetolocal_c")])
#207 left!!! 

R_b3_withlocal <-  rankTricot(keep_loc[], 
                              items = variety,
                              input = trait_best3,
                              additional.rank = keep_loc[c("postharvest3_comparetolocal_a", "postharvest3_comparetolocal_b", "postharvest3_comparetolocal_c")])


#add local 

R_b3_withlocal

mod_R_b3_withlocal  <- PlackettLuce(R_b3_withlocal)

summary(mod_R_b3_withlocal)




summary(mod_R_b3_withlocal ,ref = "Local Check")

summary(mod_R_b3_withlocal, ref = "Local Check")
capture.output(summary(mod_R_b3_withlocal, ref = "Local Check"), 
               print("model coefficients( post harvest 3 overall preference with local (60 days after harvest)"),
               coef(mod_R_b3_withlocal, ref = "Local Check"),
               file ="output_thesis/summary_mod_Coef_potato_post_harvest3_overall preference_with_local(60 days after harvest).txt")


plot(qvcalc(mod_R_b3_withlocal, ref = "Local Check"))

png(filename = "output_thesis/quasi_standard_error_potato_ tuber overall preference post-harvest 3 with Local (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_R_b3_withlocal, ref = "Local Check"), main = "tuber overall preference post-harvest 3 with Local (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
#a_local <- adjacency(R_b3_withlocal)

#a_local  <- network(a_local)
#plot(a_local)

# worth plot about overall preference  (Day 60)

plot_worth(mod_R_b3_withlocal) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_thesis/worth plot about tuber  overall preference with local post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_R_b3_withlocal) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

png(filename = "output_thesis/worth plot about tuber  overall preference with local post-harvest 3 (60 days after harvest)_ver2.png", width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod_R_b3_withlocal)+ theme_bw() +theme(panel.grid = element_blank()) +  theme(text = element_text(size = 16))  + labs(y = "Varieties", x = "The probability of winning (worth)")
dev.off()


###############Done plackett luce model########
######################################### 
#how many farmer think potato important 


###########Testtable
#potato important yes = 1 , no = 0

strsplit(dt$crops, " " )
dt<- add_column(dt, "potato_important" = NA, .after = "Agriculture_intensification_level" )

dt[which(grepl("potato", dt$crops)), "potato_important"] <- 1
dt[which(is.na(dt$potato_important)), "potato_important"] <- 0



vv<- subset(dt,dt$respondent_is_head == "Y")
vvn<- subset(dt,dt$respondent_is_head == "N")


table(vv$respondentsex)
table(vvn$respondentsex)

dt<- add_column(dt, "true_householdheader" = NA, .after = "respondent_is_head" )
dt$true_householdheader<- ifelse(dt$respondent_is_head == "Y",dt$true_householdheader <- dt$respondentsex, paste0( dt$respondentsex , "_o"))
keep  <- dt$true_householdheader[]
keep <- as.data.frame(keep)
keep[keep== "F_o",]<- "M"
keep[keep == "M_o",] <- "F"
dt$true_householdheader <-keep
table(dt$crops)

table(dt[which(dt$potato_important == 1),"true_householdheader"])
#F  M 
#31 80
#man tought potato is important 
table(dt[which(dt$potato_important == 0),"true_householdheader"])
#F   M 
#34 137

table(dt[which(dt$potato_important == 1),"respondentsex"])
#F  M 
#49 62
#man tought potato is important 
table(dt[which(dt$potato_important == 0),"respondentsex"])

#F   M 
#79 92 
kind <-c("male_potato_important", "female_potato_important", "male_no_potato_important", "female_no_potato_important")
value <-c(62, 49, 79, 92)

t_potato_value <- cbind(kind, value)
t_potato_value <- as.data.frame(t_potato_value)
t_potato_value$value <- as.numeric(t_potato_value$value)
pie(t_potato_value$value, t_potato_value$kind)

piepercent<- round(100*t_potato_value$value/sum(t_potato_value$value), 1)


pie(t_potato_value$value, labels  = paste(piepercent, "%"), main = " Respondent gender potato important pie chart",col = rainbow(length(t_potato_value$value)))
legend("topright", c("female_no_potato_important","female_potato_important","male_no_potato_important","male_potato_important"), cex = 0.8, fill = rainbow(length(t_potato_value$value)))

png(filename = "output_thesis/ Respondent gender potato important pie chart.png", width = 30, height = 15, units = "cm", res = 400)
pie(t_potato_value$value, labels  = paste(piepercent, "%"), main = " Respondent gender potato important pie chart",col = rainbow(length(t_potato_value$value)))
legend("topright", c("female_no_potato_important","female_potato_important","male_no_potato_important","male_potato_important"), cex = 0.8, fill = rainbow(length(t_potato_value$value)))
dev.off()

#true hoseholdhead 
kind <-c("male_potato_important", "female_potato_important", "male_no_potato_important", "female_no_potato_important")
value <-c(62, 49, 79, 92)

t_potato_value <- cbind(kind, value)
t_potato_value <- as.data.frame(t_potato_value)
t_potato_value$value <- as.numeric(t_potato_value$value)
pie(t_potato_value$value, t_potato_value$kind)

piepercent<- round(100*t_potato_value$value/sum(t_potato_value$value), 1)

#F   M 



###############################calculate Agreement between rankings:Kendall correlation

#Line graph about Response rate of farmers according to each trait


length(keep1[,1])
length(keep2_1[,1])
length(keep3dt_NA[,1])
length(keep4dt_NA[,1] )
length(keep_DI2[,1])
length(keep6_Mph1[,1])
length(keep7_yieldpos1[,1])
length(keep8_tubersize_p1[,1])
length(keep9_appearance_p1[,1])
length(keep10_marketable_5d[,1])
length(keep11_taste_d5[,1])
length(keep12_store[,1])
length(keep13dt_NA[,1])
length(keep14_NA[,1])
length(keep15_taste[,1])
length(keep16_dor3[,1])
length(keep17_q3[,1])
length(keep18_q3[,1])
length(keep19_f3[,1])
length(keep20_b3[,1])

Tricot_traits <- c("Plant vigor 1", "Bacterial wilt 1", "Diseases or insect resistance","Bacterial wilt 2 (75 days)", "Disease and  insect resistance 2 (75 days)", 
                   "maturity post-harvest 1", "post harvest yield","post harvest tuber size" , "post harvest tuber appearance " ," post harvest market able ","post harvest taste","post harvest_ marketable store","easiest to sell after storage (Day 45)","dromancy(long or short)  (Day 45)","taste (Day 45)","dormancy (Day 60)","tuber quality (Day 60)","overall performance  (Day 60)","plant for future  (Day 60)", "best overall  (Day 60)")
Tricot_traits_coun <- c(length(keep1[,1]), length(keep2_1[,1]), length(keep3dt_NA[,1]), length(keep4dt_NA[,1]), length(keep_DI2[,1]), length(keep6_Mph1[,1]),
                        length(keep7_yieldpos1[,1]), length(keep8_tubersize_p1[,1]), length(keep9_appearance_p1[,1]), length(keep10_marketable_5d[,1]), length(keep11_taste_d5[,1]),
                        length(keep12_store[,1]), length(keep13dt_NA[,1]), length(keep14_NA[,1]), length(keep15_taste[,1]), length(keep16_dor3[,1]), length(keep17_q3[,1]),
                        length(keep18_q3[,1]), length(keep19_f3[,1]), length(keep20_b3[,1]))

traits_linegraph <-data.frame(Tricot_traits, Tricot_traits_coun)
traits_linegraph
gt1<- ggplot(traits_linegraph, aes(x=Tricot_traits, y = Tricot_traits_coun)) +  geom_bar(stat='identity')+ geom_text(aes(label =Tricot_traits_coun), vjust =-0.3)  + coord_cartesian(ylim = c(0, 500)) + theme(axis.text.x=element_text(angle=45, hjust=1))
gt2 <- gt1 +scale_x_discrete(limits=c("Plant vigor 1", "Bacterial wilt 1", "Diseases or insect resistance","Bacterial wilt 2 (75 days)", "Disease and  insect resistance 2 (75 days)", 
                                      "maturity post-harvest 1", "post harvest yield","post harvest tuber size" , "post harvest tuber appearance " ," post harvest market able ","post harvest taste","post harvest_ marketable store","easiest to sell after storage (Day 45)","dromancy(long or short)  (Day 45)","taste (Day 45)","dormancy (Day 60)",
                                      "tuber quality (Day 60)","overall performance  (Day 60)","plant for future  (Day 60)", "best overall  (Day 60)"))

plot(gt2)


png(filename = "output_thesis/tricot_trait_data_circumstance.png", width = 30, height = 30, units = "cm", res = 400)
plot(gt2)
dev.off()



png(filename = "output_thesis/tricot_trait_data_circumstance.png", width = 30, height = 30, units = "cm", res = 400)
plot(gt2)
dev.off()
#####################done#########################
#####################################traits heatmap######
#
#traits <- c("Plant vigour at VG1", 
#            "BW resistance at VG1", 
#            "Disease & insect at VG1",
#            "BW resistance at VG2",
#            "Disease & insect at VG2",
#            "Maturity at PH1",
#            "Yield at PH1",
#            "Tuber size at PH1",
#            "Tuber appearance at PH1",
#            "MAH at PH1",
#            "Taste at PH1",
#            "MAS at PH1",
#            "MAS at PH2",
#            "Dormancy at PH2",
#            "Taste at PH2",
#            "Dormancy at PH3",
#            "Tuber quality at PH3",
#            "Overall performance",
#            "Willing to plant in the future",
#            "Overall preference")
#
#

traits <- c("Plant vigour at VG1", 
            "BW resistance at VG1", 
            "Disease & insect at VG1",
            "BW resistance at VG2",
            "Disease & insect at VG2",
            "Maturity at PH1",
            "Yield at PH1",
            "Tuber size at PH1",
            "Tuber appearance at PH1",
            "Marketable after harvest (MAH) at PH1",
            "Taste at PH1",
            "Marketable after store (MAS) at PH1",
            "Marketable after store (MAS) at PH2",
            "Dormancy at PH2",
            "Taste at PH2",
            "Dormancy at PH3",
            "Tuber quality at PH3",
            "Overall performance",
            "Willing to plants in the future (WPF)",
            "Overall preference")


pl_traits_list <- list(mod1,
                       mod_bw1,
                       mod3,
                       mod_bw2,
                       mod_DI2,
                       mod_Post_mat, 
                       mod_Post_yield,
                       mod_tuber_app,
                       mod_tuber_size ,
                       mod_marketableatharv,
                       mod_taste,
                       mod_tuberstore,
                       mod_tuberstore2,
                       mod_dor2,
                       mod_taste2,
                       mod_dor3,
                       mod_q3,
                       mod_o3,
                       mod_f3,
                       mod_b3)


# another approach is to make a heatmap to check the performance of 
# varieties
# this map plots the log-worth from PL models
# and helps in visualizing in which trait each
# variety is doing well or bad
winprob_map(pl_traits_list, traits) 

winprob_map(pl_traits_list, traits) +  theme(text = element_text(size = 20))  + labs(y = "Traits", x = "Varieties") 

gosset::worth_map(pl_traits_list, traits, ref = "Kirundo")  +  theme(text = element_text(size = 20))  + labs(y = "Traits", x = "Varieties") 


# capture PNG "output_main_s2/plant vigour(Day 45) trait Quasi standard.png"
png(filename = "output_thesis/heatmap_about_kendall_ver2.png", width = 30, height = 30, units = "cm", res = 400)
winprob_map(pl_traits_list, traits) +  theme(text = element_text(size = 20))  + labs(y = "Traits", x = "Varieties") 
dev.off()

png(filename = "output_thesis/heatmap_about_kendall_ver3.png", width = 30, height = 30, units = "cm", res = 400)
gosset::worth_map(pl_traits_list, traits, ref = "Kirundo")  +  theme(text = element_text(size = 20))  + labs(y = "Traits", x = "Varieties") 
dev.off()



# capture PNG "output_main_s2/plant vigour(Day 45) trait Quasi standard.png"
png(filename = "output_thesis/heatmap_about_kendall.png", width = 30, height = 30, units = "cm", res = 400)
winprob_map(pl_traits_list, traits)
dev.off()