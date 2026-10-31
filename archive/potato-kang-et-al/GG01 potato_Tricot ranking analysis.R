# recall packages
library("PlackettLuce")
library("qvcalc")
library("gosset")
library("patchwork")
library("ggparty")
library("ggplot2")
library("igraph")
library("leaflet")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/R/functions.R")
library("ClimMobTools")

dt <- read.csv("data/Tricot Potato_data + RHoMIS(also, combining update Post harvest 2  and post harvest 2).csv")


#check data structure: edit_ver(463 obs. of 199 variables), orig_ver(463 obs. of 172 variables)
str(dt)
head(dt)
tail(dt)
summary(dt)



###########################Vegetative 1 (Day 45) stage################################
#Before testing tricot data anaylysis, check farmers' trial field condition. 
#during Vegetative 1 (Day 45) stage, farmers respond their tricot field condition whether they succeeded or not until assesment days.   
# Here, farmers said about the qeustion: "Did the trial continue until this time?" 
#1. Yes and farmer managed the plots well
#2. Yes but farmer did not manage the plots well
#3. No Because Farmer Did Not Implement The Trial
#4. No Because Crop Did Not Survive
#5. No Due To Other Reason
#6. NA
#For next stages data analysis, we need to remove farmers data which are no answers: 
#3. No Because Farmer did not implement the trial
#4. No Because Crop Did Not Survive
#5. No Due To Other Reason   
dt[,"vegetative1_trialsuccess"]

#print row that farmers badly managed farmland. 
which(grepl("No Because Farmer Did Not Implement The Trial", dt$vegetative1_trialsuccess))
which(grepl("No Because Crop Did Not Survive", dt$vegetative1_trialsuccess))
which(grepl("No Due To Other Reason", dt$vegetative1_trialsuccess))

dt1 <- dt[-c(143,174),]

subset(dt1[,"vegetative1_trialsuccess"], dt1$vegetative1_trialsuccess == "No Because Farmer Did Not Implement The Trial") 
subset(dt1[,"vegetative1_trialsuccess"], dt1$vegetative1_trialsuccess == "No Because Crop Did Not Survive") 
subset(dt1[,"vegetative1_trialsuccess"], dt1$vegetative1_trialsuccess == "No Due To Other Reason")

varit <-unlist(dt[,"vegetative1_trialsuccess"])
table(varit)

varit <-unlist(dt1[,"vegetative1_trialsuccess"])
table(varit)


#lets check this code well applied by checking  vegetative1_plantvigour columns 
dt1[,c("vegetative1_plantvigour_pos","vegetative1_plantvigour_neg")]
summary(dt1[,c("id")]) #2 farmers failed managing field. 

#########################################################
####tricot into a PlackettLuce rankings about plant vigor during Vegetative 1 (Day 45)


trait_vigour_v1 <- c("vegetative1_plantvigour_pos","vegetative1_plantvigour_neg")
variety <- c("package_item_A", "package_item_B", "package_item_C")

## checking NA in Columes 

is.na(dt1[,c("vegetative1_plantvigour_pos","vegetative1_plantvigour_neg")])
sum(is.na(dt1[,c("vegetative1_plantvigour_pos","vegetative1_plantvigour_neg")])) # number of NA 4.

#delete some farmers who did not respond all qeustions:positive & negative 

is.na(dt1[,"vegetative1_plantvigour_pos"]) & is.na(dt1[,"vegetative1_plantvigour_neg"])

sum(is.na(dt1[,"vegetative1_plantvigour_pos"]) & is.na(dt1[,"vegetative1_plantvigour_neg"])) # 1 farmers didnit respond both questions

#find none respond farmers.. check row 78.. farmer 21AIP-8.. 
subset(dt1[,trait_vigour_v1],is.na(dt1[,"vegetative1_plantvigour_pos"]) & is.na(dt1[,"vegetative1_plantvigour_neg"]))

keep1 <- subset(dt[],id!="21AIP-8")
View(keep1)

keep1[,trait_vigour_v1]

# vector to kepp only the ones with at least one reply
keep_vigour_v1 <- apply(keep1[trait_vigour_v1], 1, function(x){
  sum(is.na(x))})

keep_vigour_v1 <- keep_vigour_v1 <= 1
View(keep_vigour_v1)

R1 <-  rankTricot(keep1[keep_vigour_v1,], 
                items = variety,
                input = trait_vigour_v1)

mod1 <- PlackettLuce(R1)

mod1

# using reference variety "Kirundo" because it has been released since 1989.
ref = "Kirundo"

summary(mod1, ref) # Here we can see all Coefficient results
 

coef(mod1, ref)
coef(mod1, log = FALSE)
plot(qvcalc(mod1, ref = ref))
plot(qvcalc(mod1, ref = ref), main = "plant vigour(Day 45) trait Quasi standard ") # result of plant vigour trait 

# capture PNG "output_v1/plant vigour(Day 45) trait Quasi standard.png"
png(filename = "output_v1/plant vigour(Day 45) trait Quasi standard.png", width = 30, height = 15, units = "cm", res = 400)
plot(qvcalc(mod1, ref = ref), main = "plant vigour(Day 45) trait Quasi standard ") 
dev.off()


#capture of potato plant vigour(45) tricot result 
capture.output(summary(mod1, ref), 
               print("model coefficients(plant.vigour45days)"),
               coef(mod1),
               file ="output_v1/summary_mod_Coef_potato_vigour45days.txt")



# Let see the network (how these varieties are connected) 

a <- adjacency(R1)

a <- network(a)
plot(a, main = "varieties network_plant vigour(Day 45) trait network")

png(filename = "output_v1/varieties network_plant vigour(Day 45) trait network.png", width = 30, height = 15, units = "cm", res = 400)
plot(a, main = "varieties network_plant vigour(Day 45) trait network")
dev.off()

# Favourability score about plant vigour 
fav <- summarise_favourite(R1)

fav #Kirundo show best score about plant vigour

plot(fav) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about plant vigour (Day 45).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

########################################################################
########tricot into a PlackettLuce rankings about bacterial wilt (Day 45)

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
# we changed "Not observed" to NA but according to Tricot_survey some farmers mentioned appearance of bacteria wilt on their farm field.
# Therefore, based on their responses, I only used tricot data that farmers respond answers about the question about appearancing bacterial wilt.
# First, let's check the response about the question about appearancing bacterial wilt

keep2_NA[,"vegetative1_wiltpresence"] # some of answers are "None". Let's change "None" to NA
keep2_NA[keep2_NA=="None"] <- NA # some of answers are "None". Let's change "None" to NA
keep2_NA
keep2_NA[,"vegetative1_wiltpresence"]

#based on bacteria wilt presence question, Remove farms with missing values in question in wilting presence
subset(keep2_NA[,"vegetative1_wiltpresence"], is.na(keep2_NA[,"vegetative1_wiltpresence"]))


complete.cases(keep2_NA[,"vegetative1_wiltpresence"]) #here part shows using 'complete.cases'.

keep2_1 <- keep2_NA[complete.cases(keep2_NA[,"vegetative1_wiltpresence"]),] # make new data frame. only exclude NA in columns "vegetative1_wiltpresence","vegetative1_plantvigour_neg"

keep2_1[,trait_bw_v1] # following code One Acre Fund projects:21AIP and 21BIP. Disappear. 


# vector to kepp only the ones with at least one reply
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

capture.output(summary(mod_bw1, ref = ref), 
               print("model coefficients( bacterial wilt resistance 45days)"),
               coef(mod_bw1, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_ bacterial wilt resistance(45days).txt")


# the network checking (how these varieties are connected) 
a_bw1 <- adjacency(R_bw1)

a_bw1 <- network(a_bw1)
plot(a_bw1)

png(filename = "output_v1/bacterial wilt resistance(Day 45)_network.png", width = 35, height = 15, units = "cm", res = 400)
plot(a_bw1)
dev.off()


coef(mod_bw1, ref = ref)
coef(mod_bw1, log = FALSE)
plot(qvcalc(mod_bw1, ref = ref))

png(filename = "output_v1/quasi_standard_error_bacterial wilt resistance(Day 45).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_bw1, ref = ref), main = "bacterial wilt resistance(Day 45)quasi_standard_error")
dev.off()

fav_bw <- summarise_favourite(R_bw1)   # favorite bacterial wilt resistance score among varieties  

fav_bw 

plot(fav_bw) + theme_bw() +theme(panel.grid = element_blank())


png(filename = "output_v1/Favourability_score_bacterial_wilt_resistance_(Day 45).png", width = 30, height = 15, units = "cm", res = 400)
fav_bw <- summarise_favourite(R_bw1)   # favorite bacterial wilt resistance score among varieties  
fav_bw 
plot(fav_bw) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

##################################################################################
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
# 340 farmers didnit respond both questions:positive & negative 


subset(keep3dt_NA[,trait_DI_v1],is.na(keep3dt_NA[,"vegetative1_diseaseinsect_pos"]) & is.na(keep3dt_NA[,"vegetative1_diseaseinsect_neg"]))
summary(subset(keep3dt_NA[,trait_DI_v1],is.na(keep3dt_NA[,"vegetative1_diseaseinsect_pos"]) & is.na(keep3dt_NA[,"vegetative1_diseaseinsect_neg"])))

keep3dt_NA<- keep3dt_NA[!is.na(keep3dt_NA$vegetative1_diseaseinsect_pos) | !is.na(keep3dt_NA$vegetative1_diseaseinsect_neg),] # delet 340 farmers data from 461 farmers data

keep3dt_NA[trait_DI_v1]
summary(keep3dt_NA[trait_DI_v1])


 

keep_DI_v1<- apply(keep3dt_NA[,trait_DI_v1], 1, function(x){
  sum(is.na(x))})

keep_DI_v1<- keep_DI_v1 <= 1
View(keep_DI_v1)

R3 <-  rankTricot(keep3dt_NA[keep_DI_v1,], 
                  items = variety,
                  input = trait_DI_v1)

R3

mod3 <- PlackettLuce(R3)

summary(mod3, ref = ref)



capture.output(summary(mod3, ref = ref), 
               print("model coefficients(diseases or insect resistance(Day 45))"),
               coef(mod3, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_diseases or insect resistance(Day 45)_45days.txt")

coef(mod3, ref = ref)
coef(mod3, log = FALSE)
plot(qvcalc(mod3, ref = ref))


png(filename = "output_v1/quasi_standard_error_potato_diseases or insect resistance(Day 45).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod3, ref = ref), main = "potato_diseases or insect resistance(Day 45)quasi_standard_error")
dev.off()


# the network checking (how these varieties are connected) 
a_di1 <- adjacency(R3)

a_di1 <- network(a_di1)
plot(a_di1)

png(filename = "output_v1/network_potato_diseases or insect resistance(Day 45).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_di1)
dev.off()

# Favourability score about diseases or insect resistance(Day 45)
fav_di1 <- summarise_favourite(R3)

fav_di1 

plot(fav_di1) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about diseases or insect resistance(Day 45).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_di1 ) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

###########################Vegetative 2 (Day 75) stage################################
#during Vegetative 2 (Day 75) stage, farmers respond their tricot field condition whether they succeeded or not to keep continue tricot trial.   
# Here, farmers said about the qeustion: "Did the trial continue until this time?" 
#1. Yes and farmer managed the plots well
#2. Yes but farmer did not manage the plots well
#3. No because farmer did not implement the trial
#4. No because crop did not survive
#5. No due to other reason 
#6. NA
#For next stages data analysis, we need to remove farmers data which are no answer in Vegetative 2 (Day 75) stage: 
#3. No because farmer did not implement the trial
#4. No because crop did not survive
#5. No due to other reason 

dt1[,"vegetative2_trialsuccess"]
summary(dt1[,"vegetative2_trialsuccess"])
table(dt1[,"vegetative2_trialsuccess"]) #36 farmers failed to maintain in Vegetative 2 (Day 75) stage

#print row that farmers badly managed farmland and delet those farmers data  
which(grepl("No Because Farmer Did Not Implement The Trial", dt1$vegetative2_trialsuccess))

dt2 <- dt1[-c(182, 215),]

which(grepl("No Because Crop Did Not Survive", dt2$vegetative2_trialsuccess))
dt2 <- dt2[-c( 6,21,22,176,180,188,190,193,208,213,246,260,274,297,318,323,324,336,344,351),]

which(grepl("No Due To Other Reason", dt2$vegetative2_trialsuccess))

dt2 <- dt2[-c(145,177,194,198,203,238,239,302,303,314,320,323,384,395),]


#check again data frame2
which(grepl("No Because Farmer Did Not Implement The Trial", dt2$vegetative2_trialsuccess))
which(grepl("No Because Crop Did Not Survive", dt2$vegetative2_trialsuccess))
which(grepl("No Due To Other Reason", dt2$vegetative2_trialsuccess))

#check the code applied well. 

subset(dt2[,"vegetative2_trialsuccess"], dt1$vegetative1_trialsuccess == "No Because Farmer Did Not Implement The Trial") 
subset(dt2[,"vegetative2_trialsuccess"], dt1$vegetative1_trialsuccess == "No Because Crop Did Not Survive") 
subset(dt2[,"vegetative2_trialsuccess"], dt1$vegetative1_trialsuccess == "No Due To Other Reason")

table(dt2[,"vegetative2_trialsuccess"])

varit <-unlist(dt[,"vegetative1_trialsuccess"])
table(varit)

varit <-unlist(dt1[,"vegetative1_trialsuccess"])
table(varit)


#lets check this code well applied by checking  vegetative2_bacterialwilt_neg columns 
dt2[,c("vegetative2_bacterialwilt_pos","vegetative2_bacterialwilt_neg")]
summary(dt2[,c("id")]) #36 farmers failed managing field. 


#####################################################################################
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
# we changed "Not observed" to NA but according to Tricot_survey some farmers mentioned appearance of bacteria wilt (Vegetative 2 (75 days) stages) on their farm field.
# Therefore, based on their responses, I only used tricot data that farmers respond answers about the question about bacterial wilt appearance at Vegetative 2 (75 days) stages .
# First, let's check the response about the question about bacterial wilt appearance 2 

keep4dt_NA[,"vegetative2_wiltpresence"] # some of answers are "None". Let's change "None" to NA
keep4dt_NA[keep4dt_NA=="None"] <- NA # some of answers are "None". Let's change "None" to NA
keep4dt_NA
keep4dt_NA[,"vegetative2_wiltpresence"]

#based on bacteria wilt presence 2 question, Remove farms with missing values in question in wilting presence
subset(keep4dt_NA[,"vegetative2_wiltpresence"], is.na(keep4dt_NA[,"vegetative2_wiltpresence"]))


complete.cases(keep4dt_NA[,"vegetative2_wiltpresence"]) #here part shows using 'complete.cases'.

keep4dt_NA <- keep4dt_NA[complete.cases(keep4dt_NA[,"vegetative2_wiltpresence"]),] # make new data frame. only exclude NA in columns "vegetative1_wiltpresence","vegetative1_plantvigour_neg"
keep4dt_NA[,"vegetative2_wiltpresence"]

keep4dt_NA[,trait_bw_v2] # following code One Acre Fund projects:21AIP and 21BI disappeared. 


# vector to kepp only the ones with at least one reply
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


ref = "Kirundo" #using reference "Kirundo" for comparing 

summary(mod_bw2, ref = ref) # summary  coefficients about bacterial wilt resistance ranking by using refence "Kirundo"  

capture.output(summary(mod_bw2, ref = ref), 
               print("model coefficients( bacterial wilt resistance 75 days)"),
               coef(mod_bw1, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_ bacterial wilt resistance 2 (75days).txt")


# the network checking (how these varieties are connected) 
a_bw2 <- adjacency(R_bw2)

a_bw2 <- network(a_bw2)
plot(a_bw2)

png(filename = "output_v1/bacterial wilt resistance 2 (Day 75)_network.png", width = 35, height = 15, units = "cm", res = 400)
plot(a_bw2)
dev.off()


coef(mod_bw2, ref = ref)
coef(mod_bw2, log = FALSE)
plot(qvcalc(mod_bw2, ref = ref))

png(filename = "output_v1/quasi_standard_error_bacterial wilt resistance 2 (Day 75).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_bw1, ref = ref), main = "bacterial wilt resistance 2 (Day 75)quasi_standard_error")
dev.off()

fav_bw2 <- summarise_favourite(R_bw2)   # favorite bacterial wilt resistance score among varieties  

fav_bw2 

plot(fav_bw2) + theme_bw() +theme(panel.grid = element_blank())


png(filename = "output_v1/Favourability_score_bacterial_wilt_resistance 2_(Day 75).png", width = 30, height = 15, units = "cm", res = 400)
fav_bw2 <- summarise_favourite(R_bw2)   # favorite bacterial wilt resistance score among varieties  
fav_bw2
plot(fav_bw2) + theme_bw() +theme(panel.grid = element_blank())
dev.off()


############################################################################
#tricot into a PlackettLuce model about disease and  insect resistance 2 (75 days)

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
# 207 farmers didnit respond both questions:positive & negative 

subset(keep_DI2[trait_DI2],is.na(keep_DI2[,"vegetative2_diseaseinsect_pos"]) & is.na(keep_DI2[,"vegetative2_diseaseinsect_neg"]))


keep_DI2<- keep_DI2[!is.na(keep_DI2$vegetative2_diseaseinsect_pos) | !is.na(keep_DI2$vegetative2_diseaseinsect_neg),] # delete 207 farmers data from 286 farmers data

summary(keep_DI2[trait_DI2])
keep_DI2[trait_DI2] #delete farmers who didn't respond both questions.



keep_DI2_keep <- apply(keep_DI2[trait_DI2], 1, function(x){
  sum(is.na(x))})

keep_DI2_keep <- keep_DI2_keep <= 1

R_DI2 <-  rankTricot(keep_DI2[keep_DI2_keep,], 
                  items = variety,
                  input = trait_DI2)
R_DI2
View(R_DI2)

mod_DI2<- PlackettLuce(R_DI2)

mod_DI2

summary(mod_DI2, ref = ref)

capture.output(summary(mod_DI2, ref = ref), 
               print("model coefficients(diseases or insect resistance 2 (Day 75))"),
               coef(mod_DI2, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_diseases or insect resistance2 (Day 75).txt")


plot(qvcalc(mod_DI2, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_diseases or insect resistance 2 (Day 75).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_DI2, ref = ref), main = "potato_diseases or insect resistance2 (Day 75)quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_di2 <- adjacency(R_DI2)

a_di2 <- network(a_di2)
plot(a_di2)

png(filename = "output_v1/network_potato_diseases or insect resistance2 (Day 75).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_di2)
dev.off()

# Favourability score about diseases or insect resistance(Day 75)
fav_di2 <- summarise_favourite(R_DI2)

fav_di2 

plot(fav_di2) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about diseases or insect resistance 2 (Day 75).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_di2 ) + theme_bw() +theme(panel.grid = element_blank())
dev.off()
########################################

#########################New stage:  post-harvest 1 (5 days after harvest) ###########################
#during  post-harvest 1 (5 days after harvest) stage, farmers respond their tricot field condition whether they succeeded or not to keep continue tricot trial.   
# Here, farmers said about the qeustion: "Did the trial continue until this time?" 
#1. Yes and farmer managed the plots well
#2. Yes but farmer did not manage the plots well
#3. No because farmer did not implement the trial
#4. No because crop did not survive
#5. No due to other reason 
#6. NA
#For data analysis, we need to remove farmers data which are no answer in post-harvest 1 (5 days after harvest)stage: 
#3. No because farmer did not implement the trial
#4. No because crop did not survive
#5. No due to other reason 


dt2[,"postharvest1_trialsuccess"]
summary(dt2[,"postharvest1_trialsuccess"])


table(dt2[,"postharvest1_trialsuccess"]) #103 farmers failed to maintain in post-harvest 1 (5 days after harvest) stage among 286 farmers

#print row that farmers badly managed farmland and delete those farmers data  
which(grepl("No Because Farmer Did Not Implement The Trial", dt2$postharvest1_trialsuccess))

dt3 <- dt2[-c(49,68,98),]

which(grepl("No Because Crop Did Not Survive", dt3$postharvest1_trialsuccess))
dt3 <- dt3[-c( 19,34,36,49,54,55,56,59,60,62,65,66,68,70,72,74,75,91,92,95,100,106,127,153,154,161,165,189,197,198,200,201,202,212,214,215,216,218,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,236,238,239,241,242,244,246,248,273,281),]


which(grepl("No Due To Other Reason", dt3$postharvest1_trialsuccess))

dt3 <- dt3[-c(74,83,85,96,108,139,140,141,142,143,144,145,151,153,163,164,165,167,168,169,181,184,201,232,237,240,241,242,244,245,258,259,260,261,262,266,268),]


#check again data frame2
which(grepl("No Because Farmer Did Not Implement The Trial", dt3$postharvest1_trialsuccess))
which(grepl("No Because Crop Did Not Survive", dt3$postharvest1_trialsuccess))
which(grepl("No Due To Other Reason", dt3$postharvest1_trialsuccess))

#check the code applied well. 

subset(dt3[,"postharvest1_trialsuccess"], dt3$postharvest1_trialsuccess == "No Because Farmer Did Not Implement The Trial") 
subset(dt3[,"postharvest1_trialsuccess"], dt3$postharvest1_trialsuccess == "No Because Crop Did Not Survive") 
subset(dt3[,"postharvest1_trialsuccess"], dt3$postharvest1_trialsuccess == "No Due To Other Reason")

table(dt3[,"postharvest1_trialsuccess"]) #293 farmers said Yes And Farmer Managed The Plots Well  and 6 farmers said Yes But Farmer Did Not Manage The Plots Well. plus, NA
sum(is.na(dt3[,"postharvest1_trialsuccess"])) # number of NA  is 23


#lets check this code well applied 
summary(dt3[,c("id")]) #103 farmers failed managing field.

#In addition  post-harvest 1 (5 days after harvest) is different from previous stages because in here stage, farmers are requested to answer "Does farmer still have enough tubers of the 3 varieties to enable comparison?".
#farmers mentioned 
#Yes, The Farmer Possesses Sufficient Tubers Of All 3 Varieties
#Yes, The Farmer Possesses Sufficient Tubers Of All No, The Farmer Cannot Make Observations Due To Another Reason Varieties
#No, Because The Farmer Does Not Possess Sufficient Quantities Of Tubers Of One Or More Varieties
#No, The Farmer Cannot Make Observations Due To Another Reason
#NA
#
#However, the answers seems like wrong but let's delete negative answers.However, before delete negetitve lets change answer as short  yes and no.
dt3t<-dt3[]
dt3t[dt3t == 'Yes, The Farmer Possesses Sufficient Tubers Of All 3 Varieties'] <- "Yes"
dt3t[dt3t == 'Yes, The Farmer Possesses Sufficient Tubers Of All No, The Farmer Cannot Make Observations Due To Another Reason Varieties'] <- "Yes"
dt3t[dt3t == 'No, Because The Farmer Does Not Possess Sufficient Quantities Of Tubers Of One Or More Varieties'] <- "No"
dt3t[dt3t == 'No, The Farmer Cannot Make Observations Due To Another Reason'] <- "No"

table(dt3t[,"postharvest1_tuberstoobserve"])
dt3t[,"postharvest1_tuberstoobserve"]

which(grepl("No", dt3t$postharvest1_tuberstoobserve))
#delete No answer
dt3<-dt3t[-c(13,16,17,20,21,22,23,24,25,26,28,35,42,51,52,72,74,75,78,79,81,84,102,111,133,141,147,148,178,195,202,205,208,221,222,223),]
summary(dt3[,"postharvest1_tuberstoobserve"])

#There are 286 farmers left, including NA.
#############################################################
#####tricot into a PlackettLuce model about maturity post-harvest 1 (5 days after harvest)) 

trait_Mph1 <- c("postharvest1_maturity_pos","postharvest1_maturity_neg")


na_check <- is.na(dt3[,c("postharvest1_maturity_pos","postharvest1_maturity_neg")])
summary(na_check) #however, there are "Not observed" data. lets change "Not observed" to NA


keep6<- dt3[]
keep6[,trait_Mph1]


#delete some farmers who did not respond both questions:positive & negative 


is.na(keep6[,"postharvest1_maturity_pos"]) & is.na(keep6[,"postharvest1_maturity_neg"])
sum(is.na(keep6[,"postharvest1_maturity_pos"]) & is.na(keep6[,"postharvest1_maturity_neg"]))

# 22 farmers didnit respond both questions:positive & negative 

subset(keep6[trait_Mph1],is.na(keep6[,"postharvest1_maturity_pos"]) & is.na(keep6[,"postharvest1_maturity_neg"]))


keep6_Mph1<- keep6[!is.na(keep6$postharvest1_maturity_pos) | !is.na(keep6$postharvest1_maturity_neg),] # delete 22 farmers data from 286 farmers data

summary(keep6_Mph1[trait_Mph1])
keep6_Mph1[trait_Mph1] # after deleting farmers who didn't respond both questions. The result shows there are no more NA. so lets skip blow procesure. 
is.na(keep6_Mph1[trait_Mph1])


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
               file ="output_v1/summary_mod_Coef_potato_maturity post-harvest 1 (5 days after harvest).txt")


plot(qvcalc(mod_Post_mat, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_maturity post-harvest 1 (5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_Post_mat, ref = ref), main = "maturity post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_Post_mat <- adjacency(R_Mph1)

a_Post_mat  <- network(a_Post_mat )
plot(a_Post_mat )

png(filename = "output_v1/network_potato_maturity post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_Post_mat )
dev.off()

# Favourability score about potato_maturity post-harvest 1 (5 days after harvest)
fav_Post_mat <- summarise_favourite(R_Mph1)

fav_Post_mat

plot(fav_Post_mat) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about potato_maturity post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_Post_mat ) + theme_bw() +theme(panel.grid = element_blank())
dev.off()
#############################################################
#####tricot into a PlackettLuce model about ost harvest yield (5 days after harvest)) 

trait_vigour_yieldpos1 <- c("postharvest1_yield_pos","postharvest1_yield_neg")
na_check <- is.na(dt3[,c("postharvest1_yield_pos","postharvest1_yield_neg")])
summary(na_check) #however, there are "Not observed" data. lets change "Not observed" to NA


keep7<- dt3[]
keep7[trait_vigour_yieldpos1 ]

# lets check some farmers who did not respond both questions:positive & negative 

is.na(keep7[,"postharvest1_yield_pos"]) & is.na(keep7[,"postharvest1_yield_neg"])
sum(is.na(keep7[,"postharvest1_yield_pos"]) & is.na(keep7[,"postharvest1_yield_neg"]))
subset(keep7[trait_Mph1],is.na(keep6[,"postharvest1_yield_pos"]) & is.na(keep7[,"postharvest1_yield_neg"]))
# 22 farmers didnit respond both questions:positive & negative 

keep7_yieldpos1<- keep7[!is.na(keep7$postharvest1_yield_pos) | !is.na(keep7$postharvest1_yield_neg),] # delete 22 farmers data from 286 farmers data
keep7_yieldpos1[trait_vigour_yieldpos1]

summary(keep7_yieldpos1[trait_vigour_yieldpos1]) # 300 farmers data are remained.  

table(keep7_yieldpos1[trait_vigour_yieldpos1])
#the result shows there are no more NA. so lets skip blow procesure. 

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


capture.output(summary(mod_Post_yield, ref = ref), 
               print("model coefficients(post harvest yield (5 days after harvest))"),
               coef(mod_Post_yield, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_post harvest yield (5 days after harvest).txt")



plot(qvcalc(mod_Post_yield, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_post harvest yield (5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_Post_yield, ref = ref), main = "post harvest yield (5 days after harvest) quasi_standard_error")
dev.off()


a_di4 <- adjacency(R_yieldpos1)
a_di4 <- network(R_yieldpos1)
plot(a_di4)

png(filename = "output_v1/network_potato_post harvest yield (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_di4)
dev.off()


fav_Post_yield <- summarise_favourite(R_yieldpos1)

fav_Post_yield

plot(fav_Post_yield) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about potato_post harvest yield (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_Post_yield ) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

##########################################################
# tricot into a PlackettLuce rankings about post harvest tuber size (5 days after harvest)) 

trait_tubersize_p1 <- c("postharvest1_tubersize_pos","postharvest1_tubersize_neg")

na_check <- is.na(dt3[,c("postharvest1_tubersize_pos","postharvest1_tubersize_neg")])
summary(na_check) 


keep8<- dt3[]
keep8[trait_tubersize_p1]
#ckeck NA how many are there

is.na(keep8[,"postharvest1_tubersize_pos"]) & is.na(keep8[,"postharvest1_tubersize_neg"])
sum(is.na(keep8[,"postharvest1_tubersize_pos"]) & is.na(keep8[,"postharvest1_tubersize_neg"]))

# 22 farmers didnit respond both questions:positive & negative 

subset(keep8[trait_tubersize_p1],is.na(keep6[,"postharvest1_tubersize_pos"]) & is.na(keep8[,"postharvest1_tubersize_neg"]))


keep8_tubersize_p1<- keep8[!is.na(keep8$postharvest1_tubersize_pos) | !is.na(keep8$postharvest1_tubersize_neg),] # delete 22 farmers data from 286 farmers data

summary(keep8_tubersize_p1[trait_tubersize_p1])
keep8_tubersize_p1[trait_tubersize_p1] # after deleting farmers who didn't respond both questions. 1 Farmers respond only neg. 
is.na(keep8_tubersize_p1[trait_tubersize_p1])



keep_keep8_tubersize_p1_keep <- apply(keep8_tubersize_p1[trait_tubersize_p1], 1, function(x){sum(is.na(x))})
keep_keep8_tubersize_p1_keep <- keep_keep8_tubersize_p1_keep <= 1 



R_tuber_size <-  rankTricot(keep8_tubersize_p1[keep_keep8_tubersize_p1_keep,], 
                            items = variety,
                            input = trait_tubersize_p1)

mod_tuber_size <- PlackettLuce(R_tuber_size )

summary(mod_tuber_size)


summary(mod_tuber_size, ref = ref)
capture.output(summary(mod_tuber_size, ref = ref), 
               print("model coefficients(maturity post harvest tuber size (5 days after harvest))"),
               coef(mod_tuber_size, ref = ref),
               file ="output_v1/summary_mod_Coef_potato harvest tuber size post-harvest 1 (5 days after harvest).txt")


plot(qvcalc(mod_tuber_size, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ tuber size post-harvest 1 (5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_tuber_size, ref = ref), main = "tuber size post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_tuber_size <- adjacency(R_tuber_size)

a_tuber_size  <- network(a_tuber_size )
plot(a_tuber_size )

png(filename = "output_v1/network_potato_tuber size post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_tuber_size )
dev.off()

# Favourability score about potato_tuber size post-harvest 1 (5 days after harvest)
fav_tuber_size <- summarise_favourite(R_tuber_size)

fav_tuber_size

plot(fav_tuber_size) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about potato_tuber size post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_tuber_size) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

############################################################# 
# tricot into a PlackettLuce model about post harvest tuber appearance (5 days after harvest)) 
trait_appearance_d5 <- c("postharvest1_tuberappearance_pos","postharvest1_tuberappearance_neg")

na_check <- is.na(dt[,c("postharvest1_tuberappearance_pos","postharvest1_tuberappearance_neg")])
summary(na_check) #Lets check how many NA in farmers data 

keep9<- dt3[]
keep9[trait_appearance_d5]
is.na(keep9[trait_appearance_d5])
sum(is.na(keep9[trait_appearance_d5]))  #22 farmers did not respond.
subset(keep9[trait_appearance_d5],is.na(keep9[,"postharvest1_tuberappearance_pos"]) & is.na(keep9[,"postharvest1_tuberappearance_neg"]))
summary(subset(keep9[trait_appearance_d5],is.na(keep9[,"postharvest1_tuberappearance_pos"]) & is.na(keep9[,"postharvest1_tuberappearance_neg"])))
#22 farmers did not respond.

keep9_appearance_p1<- keep9[!is.na(keep9$postharvest1_tuberappearance_pos) | !is.na(keep9$postharvest1_tuberappearance_neg),] # delete 22 farmers data from 286 farmers data


summary(keep9_appearance_p1[trait_appearance_d5])
keep9_appearance_p1[trait_appearance_d5]  
sum(is.na(keep9_appearance_p1[trait_appearance_d5] ))
# after deleting farmers who didn't respond both questions. 2 Farmers respond only neg.


keep9_tubersize_keep <- apply(keep9_appearance_p1[trait_appearance_d5], 1, function(x){sum(is.na(x))})
keep9_tubersize_keep <- keep9_tubersize_keep <= 1 



R_tubersize <-  rankTricot(keep9_appearance_p1[keep9_tubersize_keep,], 
                            items = variety,
                            input = trait_appearance_d5)

mod_tuber_size <- PlackettLuce(R_tubersize)

summary(mod_tuber_size)


summary(mod_tuber_size, ref = ref)
capture.output(summary(mod_tuber_size, ref = ref), 
               print("model coefficients(maturity post harvest tuber size (5 days after harvest))"),
               coef(mod_tuber_size, ref = ref),
               file ="output_v1/summary_mod_Coef_potato harvest tuber size post-harvest 1 (5 days after harvest).txt")


plot(qvcalc(mod_tuber_size, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ tuber size post-harvest 1 (5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_tuber_size, ref = ref), main = "tuber size post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_tuber_size <- adjacency(R_tubersize)

a_tuber_size  <- network(R_tubersize )
plot(a_tuber_size)

png(filename = "output_v1/network_potato_tuber size post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_tuber_size)
dev.off()

# Favourability score about potato_tuber size post-harvest 1 (5 days after harvest)
fav_tuber_size <- summarise_favourite(a_tuber_size)

fav_tuber_size

plot(fav_tuber_size) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about potato_tuber size post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_tuber_size) + theme_bw() +theme(panel.grid = element_blank())
dev.off()



############################################
# tricot into a PlackettLuce model about post harvest market able at harvest (5 days after harvest)) 

trait_marketable_5d <- c("postharvest1_marketableatharv_pos","postharvest1_marketableatharv_neg")


is.na(dt3[,c("postharvest1_marketableatharv_pos","postharvest1_marketableatharv_neg")])
na_check <- is.na(dt3[,c("postharvest1_marketableatharv_pos","postharvest1_marketableatharv_neg")])
summary(na_check) #check NA. 


keep10<- dt3[]
keep10[trait_marketable_5d]

is.na(keep10[trait_marketable_5d])
sum(is.na(keep10[trait_marketable_5d])) #280 farmers did not respond.
subset(keep10[trait_marketable_5d],is.na(keep10[,"postharvest1_marketableatharv_pos"]) & is.na(keep10[,"postharvest1_marketableatharv_neg"]))
summary(subset(keep10[trait_appearance_d5],is.na(keep10[,"postharvest1_marketableatharv_pos"]) & is.na(keep10[,"postharvest1_marketableatharv_neg"])))
#139 farmers did not respond both of questions: pos & neg 

keep10_marketable_5d<- keep10[!is.na(keep10$postharvest1_marketableatharv_pos) | !is.na(keep10$postharvest1_marketableatharv_neg),] # delete 139 farmers data from 286 farmers data
summary(keep10_marketable_5d[trait_marketable_5d]) #183 farmers left
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
               file ="output_v1/summary_mod_Coef_potato marketableatharv post harvest(5 days after harvest).txt")


plot(qvcalc(mod_marketableatharv, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato marketableatharv post harvest(5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_marketableatharv, ref = ref), main = "potato marketableatharv post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_marketableatharv <- adjacency(mod_marketableatharv)

a_marketableatharv  <- network(a_marketableatharv )
plot(a_marketableatharv )

png(filename = "output_v1/network_potato marketableatharv post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_marketableatharv )
dev.off()

fav_marketableatharv <- summarise_favourite(R_marketableatharv)

fav_marketableatharv

plot(fav_marketableatharv) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about potato marketableatharv post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_marketableatharv) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

###############################################
# tricot into a PlackettLuce rankings about post harvest taste (5 days after harvest)) 
trait_taste_d5<- c("postharvest1_taste_pos","postharvest1_taste_neg")

na_check <- is.na(dt3[,c("postharvest1_taste_pos","postharvest1_taste_neg")])
summary(na_check)


keep11<- dt3[]

keep11[trait_taste_d5]

is.na(keep11[trait_taste_d5])
sum(is.na(keep11[trait_taste_d5])) #44 farmers did not respond.

subset(keep11[trait_taste_d5],is.na(keep11[,"postharvest1_taste_pos"]) & is.na(keep11[,"postharvest1_taste_neg"]))
summary(subset(keep11[trait_taste_d5],is.na(keep11[,"postharvest1_taste_pos"]) & is.na(keep11[,"postharvest1_taste_neg"])))
#22farmers did not respond both of questions: pos & neg 

keep11_taste_d5 <- keep11[!is.na(keep11$postharvest1_taste_pos) | !is.na(keep11$postharvest1_taste_neg),] # delete 22 farmers data from 286 farmers data
summary(keep11_taste_d5[trait_taste_d5]) #183 farmers left
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
               file ="output_v1/summary_mod_Coef_potato taste post harvest(5 days after harvest).txt")


plot(qvcalc(mod_taste, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato taste post harvest(5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_taste, ref = ref), main = "potato taste post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_taste <- adjacency(R_taste)

a_taste  <- network(a_taste)
plot(a_taste)

png(filename = "output_v1/network_potato taste post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_taste)
dev.off()

fav_taste <- summarise_favourite(R_taste)

fav_taste 

plot(fav_taste) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about potato taste post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_taste) + theme_bw() +theme(panel.grid = element_blank())
dev.off()
###################################################
#tricot into a PlackettLuce rankings about post harvest_ marketable store (5 days after harvest)
trait_store <- c("postharvest1_marketableatstor_pos","postharvest1_marketableatstor_neg")
na_check <- is.na(dt3[,c("postharvest1_marketableatstor_pos","postharvest1_marketableatstor_neg")])
summary(na_check) #however, there are "Not observed" data. lets change "Not observed" to NA

keep12<- dt3[]
keep12[trait_store]


is.na(keep12[trait_store])
subset(keep12[trait_taste_d5],is.na(keep12[,"postharvest1_marketableatstor_pos"]) & is.na(keep12[,"postharvest1_marketableatstor_neg"]))
summary(subset(keep12[trait_taste_d5],is.na(keep12[,"postharvest1_marketableatstor_pos"]) & is.na(keep12[,"postharvest1_marketableatstor_neg"])))
#205farmers did not respond both of questions: pos & neg 

keep12_store <- keep12[!is.na(keep12$postharvest1_marketableatstor_pos) | !is.na(keep12$postharvest1_marketableatstor_neg),] # delete 205 farmers data from 286 farmers data
summary(keep12_store[trait_store]) #117 farmers left
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
               file ="output_v1/summary_mod_Coef_potato tubarstore post harvest(5 days after harvest).txt")


plot(qvcalc(mod_tuberstore, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ tubarstore post-harvest 1 (5 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_tuberstore, ref = ref), main = "tubarstore post-harvest 1 (5 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_tuberstore <- adjacency(R_tuberstore)

a_tuberstore  <- network(a_tuberstore)
plot(a_tuberstore)

png(filename = "output_v1/network_potato_tuber_store post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(a_tuberstore )
dev.off()

# Favourability score about potato_tuber store post-harvest 1 (5 days after harvest)
fav_tuberstore <- summarise_favourite(R_tuberstore)

fav_tuberstore

plot(fav_tuberstore) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about potato_tuber store post-harvest 1 (5 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_tuberstore) + theme_bw() +theme(panel.grid = element_blank())
dev.off()
###############################Post-harvest 2 (45 days after harvest)################################
# Post-harvest 2 (45 days after harvest) stage means that farmers respond tricot questions after 45 days
#previous tricot data analysis, we request farmer to answer field condition but from this stage, farmers responded tuber conditions question:"Does farmer still have enough tubers of the 3 varieties to enable comparison?"
# instead of farmers' trial field condition. 
# In addition, 52 farmers answers from 21AIP classified as other category stages Post-harvest 2 (Update), although both of stages "Post-harvest 2 (Update)" and "Post-harvest 2 (45 days after harvest)" started and end same day.
#Therefore, I will marged two stages as one Post-harvest 2 (45 days after harvest) through excel.

#farmers are requested to answer the question "Does farmer still have enough tubers of the 3 varieties to enable comparison?".
#farmers mentioned 
#Yes, The Farmer Possesses Sufficient Tubers Of All 3 Varieties
#Yes, The Farmer Possesses Sufficient Tubers Of All No, The Farmer Cannot Make Observations Due To Another Reason Varieties
#No, Because The Farmer Does Not Possess Sufficient Quantities Of Tubers Of One Or More Varieties
#No, The Farmer Cannot Make Observations Due To Another Reason
#NA
#However, the answers seems like wrong. So, in this stage I ignore the question(postharvest1_tuberstoobserve):
#"Does farmer still have enough tubers of the 3 varieties to enable comparison?"
#so lets delete negative answers  


dt4t<-dt3[]
table(dt4t[,"postharvest2_tuberstoobserve"])

which(grepl("No", dt4t$postharvest2_tuberstoobserve))
#delete No answer
dt4<-dt4t[-c(3,8,9,10,11,18,27,35,39,43,101,104,111,116,117,123,131,142,145,150,159,162),]
summary(dt4[,"postharvest2_tuberstoobserve"])





#242 farmers failed to maintain in post-harvest 2 (45 days after harvest) stage among 286 farmers
########################################################################
########tricot into a PlackettLuce model  about easiest to sell after storage (Day 45)

trait_marketable_45d <- c("postharvest2_marketableatstor_pos","postharvest2_marketableatstor_neg")

#checking NA in columns 
na_check <- is.na(dt4[trait_marketable_45d])
summary(na_check)


keep13_marketable<- dt4[]

keep13_marketable[trait_marketable_45d]
is.na(keep13_marketable[trait_marketable_45d])
summary(is.na(keep13_marketable[trait_marketable_45d])) #NA check

subset(keep13_marketable[trait_marketable_45d],is.na(keep13_marketable[,"postharvest2_marketableatstor_pos"]) & is.na(keep13_marketable[,"postharvest2_marketableatstor_neg"]))
summary(subset(keep13_marketable[trait_marketable_45d],is.na(keep13_marketable[,"postharvest2_marketableatstor_pos"]) & is.na(keep13_marketable[,"postharvest2_marketableatstor_neg"])))
#91 farmers did not respond both of questions: pos & neg 
#let's delete 91 farmers  who did not answer both qeustions


subset(keep13_marketable[,trait_marketable_45d],is.na(keep13_marketable[,"postharvest2_marketableatstor_pos"]) & is.na(keep13_marketable[,"postharvest2_marketableatstor_neg"]))
summary(subset(keep13_marketable[,trait_marketable_45d],is.na(keep13_marketable[,"postharvest2_marketableatstor_pos"]) & is.na(keep13_marketable[,"postharvest2_marketableatstor_neg"])))

keep13dt_NA<- keep13_marketable[!is.na(keep13_marketable$postharvest2_marketableatstor_pos) | !is.na(keep13_marketable$postharvest2_marketableatstor_neg),]
# delet 91 farmers data from 242 farmers data

keep13dt_NA[trait_marketable_45d]
summary(keep13dt_NA[trait_marketable_45d])


R_tuberstore2 <-  rankTricot(keep13dt_NA[], 
                             items = variety,
                             input = trait_marketable_45d)


mod_tuberstore2 <- PlackettLuce(R_tuberstore2)

summary(mod_tuberstore2)

summary(mod_tuberstore2, ref = ref)
capture.output(summary(mod_tuberstore2, ref = ref), 
               print("model coefficients( post harvest 2 tubarstore (45 days after harvest)"),
               coef(mod_tuberstore2, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_post_harvest2_tubarstore_(45 days after harvest).txt")


plot(qvcalc(mod_tuberstore2, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ tubarstore post-harvest 2 (45 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_tuberstore2, ref = ref), main = "tubarstore post-harvest 2 (45 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_tuberstore2 <- adjacency(R_tuberstore2)

a_tuberstore2  <- network(a_tuberstore2)
plot(a_tuberstore2)

png(filename = "output_v1/network_potato_tuber_store post-harvest 2.png", width = 30, height = 15, units = "cm", res = 400)
plot(a_tuberstore2 )
dev.off()

# Favourability score about potato_tuber store post-harvest 2 (45 days after harvest)
fav_tuberstore2 <- summarise_favourite(R_tuberstore2)

fav_tuberstore2

plot(fav_tuberstore2) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about potato_tuber store post-harvest 2 (45 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_tuberstore2) + theme_bw() +theme(panel.grid = element_blank())
dev.off()
###################################################################################################
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
#197 farmers did not respond both of questions: pos & neg 
#let's delete 197 farmers  who did not answer both qeustions

keep14_NA<- keep14_dor[!is.na(keep14_dor$postharvest2_dormancy_pos) | !is.na(keep14_dor$postharvest2_dormancy_pos),]
# delet 197 farmers data from 242 farmers data 

keep14_NA[trait_dormancy2]
summary(keep14_NA[trait_dormancy2])
#45 farmers left


R_dor2 <-  rankTricot(keep14_NA[], 
                             items = variety,
                             input = trait_dormancy2)


mod_dor2 <- PlackettLuce(R_dor2)

summary(mod_dor2)

summary(mod_dor2, ref = ref)
capture.output(summary(mod_dor2, ref = ref), 
               print("model coefficients( post harvest 2 dormancy (45 days after harvest)"),
               coef(mod_dor2, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_post_harvest2_dormancy_(45 days after harvest).txt")


plot(qvcalc(mod_dor2, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ dormancy post-harvest 2 (45 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_dor2, ref = ref), main = "dormancy post-harvest 2 (45 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_dor2 <- adjacency(R_dor2)

a_dor2  <- network(a_dor2)
plot(a_dor2)

png(filename = "output_v1/network_dormancy post-harvest 2.png", width = 30, height = 15, units = "cm", res = 400)
plot(a_dor2 )
dev.off()

# Favourability score about potato_tuber store post-harvest 2 (45 days after harvest)
fav_dor2 <- summarise_favourite(R_dor2)

fav_dor2

plot(fav_dor2) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about dormancy post-harvest 2 (45 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_dor2) + theme_bw() +theme(panel.grid = element_blank())
dev.off()
####################################################################################################################
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
#93 farmers did not respond both of questions: pos & neg 
#let's delete 93 farmers  who did not answer both qeustions

keep15_taste<- keep15_taste[!is.na(keep15_taste$postharvest2_taste_pos) | !is.na(keep15_taste$postharvest2_taste_neg),]
# delet 197 farmers data from 242 farmers data 

keep15_taste[trait_taste2]
summary(keep15_taste[trait_taste2])
#149 farmers left


R_taste2 <-  rankTricot(keep15_taste[], 
                      items = variety,
                      input = trait_taste2)


mod_taste2 <- PlackettLuce(R_taste2)

summary(mod_taste2)

summary(mod_taste2, ref = ref)
capture.output(summary(mod_taste2, ref = ref), 
               print("model coefficients( post harvest 2 taste (45 days after harvest)"),
               coef(mod_taste2, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_post_harvest2_taste_(45 days after harvest).txt")


plot(qvcalc(mod_taste2, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ taste post-harvest 2 (45 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_taste2, ref = ref), main = "taste post-harvest 2 (45 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_taste2 <- adjacency(R_taste2)

a_taste2  <- network(a_taste2)
plot(a_taste2)

png(filename = "output_v1/network_taste post-harvest 2.png", width = 30, height = 15, units = "cm", res = 400)
plot(a_taste2 )
dev.off()

# Favourability score about potato_tuber taste post-harvest 2 (45 days after harvest)
fav_taste2 <- summarise_favourite(R_taste2)

fav_taste2

plot(fav_taste2) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about taste post-harvest 2 (45 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_taste2) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

##########################################################################
############################### Post-harvest 3 (60 days after harvest)################################
# Post-harvest 3 (60 days after harvest) stage means that farmers respond tricot questions after 60 days from harvest.
#farmers responded tuber conditions question:"Does farmer still have enough tubers of the 3 varieties to enable comparison?"
#farmers are requested to answer the question "Does farmer still have enough tubers of the 3 varieties to enable comparison?".
#farmers mentioned 
#Yes, The Farmer Possesses Sufficient Tubers Of All 3 Varieties
#Yes, The Farmer Possesses Sufficient Tubers Of All No, The Farmer Cannot Make Observations Due To Another Reason Varieties
#No, Because The Farmer Does Not Possess Sufficient Quantities Of Tubers Of One Or More Varieties
#No, The Farmer Cannot Make Observations Due To Another Reason
#NA
#However, the answers seems like wrong. So, in this stage I ignore the question(postharvest1_tuberstoobserve):
#"Does farmer still have enough tubers of the 3 varieties to enable comparison?"
#so lets delete negative answers  


dt5<-dt4[]
table(dt5[,"postharvest3_tuberstoobserve"])

which(grepl("No", dt5$postharvest3_tuberstoobserve))
#delete No answer

dt5<-dt5[-c(23,26,47,57,65,68,71,77,93,96,97,100,115,117,119,138),]
summary(dt5[,"postharvest3_tuberstoobserve"])
#248 farmers left from 264 farmers
################################################################3
#####tricot into a PlackettLuce model  about dor (Day 60)

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
#161 farmers did not respond both of questions: pos & neg 
#let's delete 161 farmers  who did not answer both qeustions

keep16_dor3<- keep16_dor3[!is.na(keep16_dor3$postharvest3_dormancy_pos) | !is.na(keep16_dor3$postharvest3_dormancy_neg),]
# delet 161 farmers data from 248 farmers data 

keep16_dor3[trait_dor3]
summary(keep16_dor3[trait_dor3])
#87 farmers left

keep16_dor3[trait_dor3]

R_dor3 <-  rankTricot(keep16_dor3[], 
                      items = variety,
                      input = trait_dor3)


mod_dor3  <- PlackettLuce(R_dor3)

summary(mod_dor3)

summary(mod_dor3, ref = ref)
capture.output(summary(mod_dor3, ref = ref), 
               print("model coefficients( post harvest 3 dorrmancy (60 days after harvest)"),
               coef(mod_taste2, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_post_harvest3_dormancy_(60 days after harvest).txt")


plot(qvcalc(mod_dor3, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ dormancy post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_taste2, ref = ref), main = "dormancy post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_dor3 <- adjacency(R_dor3)

a_dor3  <- network(a_dor3)
plot(a_dor3)

png(filename = "output_v1/network_dormancy post-harvest 3.png", width = 30, height = 15, units = "cm", res = 400)
plot(a_dor3 )
dev.off()

# Favourability score about potato_tuber taste post-harvest 2 (45 days after harvest)
fav_dor3 <- summarise_favourite(R_dor3)

fav_dor3

plot(fav_dor3) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about dormancy post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_dor3) + theme_bw() +theme(panel.grid = element_blank())


####################################################################################################################
########tricot into a PlackettLuce model  about tuber quality (Day 60)

trait_q3 <- c("postharvest3_tuberquality_pos","postharvest3_tuberquality_pos")

#checking NA in columns 
na_check <- is.na(dt5[trait_q3])
summary(na_check)


keep17_q3<- dt5[]

keep17_q3[trait_q3]
is.na(keep17_q3[trait_q3])
summary(is.na(keep17_q3[trait_q3])) #NA check 


subset(keep17_q3[trait_q3],is.na(keep17_q3[,"postharvest3_tuberquality_pos"]) & is.na(keep17_q3[,"postharvest3_tuberquality_pos"]))

summary(subset(keep17_q3[trait_q3],is.na(keep17_q3[,"postharvest3_tuberquality_pos"]) & is.na(keep17_q3[,"postharvest3_tuberquality_pos"])))
#197 farmers did not respond both of questions: pos & neg 
#let's delete 197 farmers  who did not answer both qeustions

keep17_q3<- keep17_q3[!is.na(keep17_q3$postharvest3_tuberquality_pos) | !is.na(keep17_q3$postharvest3_tuberquality_pos),]
# delet 197 farmers data from 248 farmers data 

keep17_q3[trait_q3]
summary(keep17_q3[trait_q3])
#51 farmers left



R_q3 <-  rankTricot(keep17_q3[], 
                    items = variety,
                    input = trait_q3)


mod_q3  <- PlackettLuce(R_q3)

summary(mod_q3)

summary(mod_q3, ref = ref)
capture.output(summary(mod_q3, ref = ref), 
               print("model coefficients( post harvest 3 tuber quality (60 days after harvest)"),
               coef(mod_q3, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_post_harvest3_tuber quality_(60 days after harvest).txt")


plot(qvcalc(mod_q3, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ tuber quality post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_q3, ref = ref), main = "tuber quality post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_q3 <- adjacency(R_q3)

a_q3  <- network(a_q3)
plot(a_q3)

png(filename = "output_v1/network_ tuber quality post-harvest 3.png", width = 30, height = 15, units = "cm", res = 400)
plot(a_q3 )
dev.off()

# Favourability score about potato_tuber quality post-harvest 3 (60 days after harvest)
fav_q3 <- summarise_favourite(R_q3)

fav_q3

plot(fav_q3) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about tuber quality post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_q3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()
#################################################################
##tricot into a PlackettLuce model  about overall performance  (Day 60)

trait_overallper3 <- c("postharvest3_overallper_pos","postharvest3_overallper_neg")

#checking NA in columns 
na_check <- is.na(dt5[trait_overallper3])
summary(na_check)


keep18_q3<- dt5[]

keep18_q3[trait_overallper3]
is.na(keep18_q3[trait_overallper3])
summary(is.na(keep18_q3[trait_overallper3])) #NA check 


subset(keep18_q3[trait_overallper3],is.na(keep18_q3[,"postharvest3_overallper_pos"]) & is.na(keep18_q3[,"postharvest3_overallper_neg"]))
summary(subset(keep18_q3[trait_overallper3],is.na(keep18_q3[,"postharvest3_overallper_pos"]) & is.na(keep18_q3[,"postharvest3_overallper_neg"])))
#161 farmers did not respond both of questions: pos & neg 
#let's delete 161 farmers  who did not answer both questions

keep18_q3<- keep18_q3[!is.na(keep18_q3$postharvest3_overallper_pos) | !is.na(keep18_q3$postharvest3_overallper_pos),]
# delet 161 farmers data from 248 farmers data 

keep18_q3[trait_overallper3]
summary(keep18_q3[trait_overallper3])
#87 farmers left



R_o3 <-  rankTricot(keep18_q3[], 
                    items = variety,
                    input = trait_overallper3)


mod_o3  <- PlackettLuce(R_o3)

summary(mod_o3)

summary(mod_o3, ref = ref)
capture.output(summary(mod_o3, ref = ref), 
               print("model coefficients( post harvest 3 overall performance (60 days after harvest)"),
               coef(mod_o3, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_post_harvest3_overall performance_(60 days after harvest).txt")


plot(qvcalc(mod_o3, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ tuber overall performance post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_o3, ref = ref), main = "tuber overall performance post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_o3 <- adjacency(R_o3)

a_o3  <- network(a_o3)
plot(a_o3)

png(filename = "output_v1/network_ tuber overall performance post-harvest 3.png", width = 30, height = 15, units = "cm", res = 400)
plot(a_o3 )
dev.off()

# Favourability score about potato_tuber quality post-harvest 3 (60 days after harvest)
fav_o3 <- summarise_favourite(R_o3)

fav_o3

plot(fav_o3) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about tuber  overall performance post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_o3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()
#########################################################################
##tricot into a PlackettLuce model about plant for future  (Day 60)

trait_f3 <- c("postharvest3_plantfuture_pos","postharvest3_plantfuture_neg")

#checking NA in columns 
na_check <- is.na(dt5[trait_f3])
summary(na_check)


keep19_f3<- dt5[]

keep19_f3[trait_f3]
is.na(keep19_f3[trait_f3])
summary(is.na(keep19_f3[trait_f3])) #NA check 


subset(keep19_f3[trait_f3],is.na(keep19_f3[,"postharvest3_plantfuture_pos"]) & is.na(keep19_f3[,"postharvest3_plantfuture_neg"]))
summary(subset(keep19_f3[trait_f3],is.na(keep19_m3[,"postharvest3_plantfuture_pos"]) & is.na(keep19_f3[,"postharvest3_plantfuture_neg"])))
#161 farmers did not respond both of questions: pos & neg 
#let's delete 161 farmers  who did not answer both questions

keep19_f3<- keep19_f3[!is.na(keep19_f3$postharvest3_plantfuture_pos) | !is.na(keep19_f3$postharvest3_plantfuture_neg),]
# delet 161 farmers data from 248 farmers data 

keep19_f3[trait_f3]
summary(keep19_f3[trait_f3])
#87 farmers left



R_f3 <-  rankTricot(keep19_f3[], 
                    items = variety,
                    input = trait_f3)


mod_f3  <- PlackettLuce(R_f3)

summary(mod_f3)

summary(mod_f3, ref = ref)
capture.output(summary(mod_f3, ref = ref), 
               print("model coefficients( post harvest 3 plant for future (60 days after harvest)"),
               coef(mod_f3, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_post_harvest3_plant for future_(60 days after harvest).txt")


plot(qvcalc(mod_f3, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ tuber plant for future post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_f3, ref = ref), main = "plant for future post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_f3 <- adjacency(R_f3)

a_f3  <- network(a_f3)
plot(a_f3)

png(filename = "output_v1/network_ plant for future  post-harvest 3.png", width = 30, height = 15, units = "cm", res = 400)
plot(a_f3 )
dev.off()

# Favourability score about potato_tuber plant for future post-harvest 3 (60 days after harvest)
fav_f3 <- summarise_favourite(R_f3)

fav_f3

plot(fav_f3) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about tuber  plant for future post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_f3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()
######################################################################################
###tricot into a PlackettLuce model about best overall (Day 60)



trait_best3 <- c("postharvest3_preference_pos","postharvest3_preference_neg")

#checking NA in columns 
na_check <- is.na(dt5[trait_best3])
summary(na_check)


keep20_b3<- dt5[]

keep20_b3[trait_best3]
is.na(keep20_b3[trait_best3])
summary(is.na(keep20_b3[trait_best3])) #NA check 


subset(keep20_b3[trait_best3],is.na(keep20_b3[,"postharvest3_preference_pos"]) & is.na(keep20_b3[,"postharvest3_preference_neg"]))

summary(subset(keep20_b3[trait_best3],is.na(keep20_b3[,"postharvest3_preference_pos"]) & is.na(keep20_b3[,"postharvest3_preference_neg"])))
#212 farmers did not respond both of questions: pos & neg 
#let's delete 161 farmers  who did not answer both questions

keep20_b3<- keep20_b3[!is.na(keep20_b3$postharvest3_preference_pos) | !is.na(keep20_b3$postharvest3_preference_neg),]
# delet 212 farmers data from 248 farmers data 

keep20_b3[trait_best3]
summary(keep20_b3[trait_best3])
#36 farmers left

R_b3 <-  rankTricot(keep20_b3[], 
                    items = variety,
                    input = trait_best3)


mod_b3  <- PlackettLuce(R_b3)

summary(mod_b3)

summary(mod_b3, ref = ref)
capture.output(summary(mod_b3, ref = ref), 
               print("model coefficients( post harvest 3 best overall (60 days after harvest)"),
               coef(mod_b3, ref = ref),
               file ="output_v1/summary_mod_Coef_potato_post_harvest3_best overall_(60 days after harvest).txt")


plot(qvcalc(mod_b3, ref = ref))

png(filename = "output_v1/quasi_standard_error_potato_ tuber best overall post-harvest 3 (60 days after harvest).png", width = 35, height = 15, units = "cm", res = 400)
plot(qvcalc(mod_b3, ref = ref), main = "best overall post-harvest 3 (60 days after harvest) quasi_standard_error")
dev.off()

# the network checking (how these varieties are connected) 
a_b3 <- adjacency(R_b3)

a_b3  <- network(a_b3)
plot(a_b3)

png(filename = "output_v1/network_ best overall  post-harvest 3.png", width = 30, height = 15, units = "cm", res = 400)
plot(a_b3 )
dev.off()

# Favourability score about potato_tuber plant for future post-harvest 3 (60 days after harvest)
fav_b3 <- summarise_favourite(R_b3)

fav_b3

plot(fav_b3) + theme_bw() +theme(panel.grid = element_blank())

png(filename = "output_v1/Favourability score about tuber  best overall post-harvest 3 (60 days after harvest).png", width = 30, height = 15, units = "cm", res = 400)
plot(fav_b3) + theme_bw() +theme(panel.grid = element_blank())
dev.off()

###############Done########