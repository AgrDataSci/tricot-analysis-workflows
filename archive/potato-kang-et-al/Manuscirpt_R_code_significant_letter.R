library("multcompView")
  
#overall performance
a1<- summary(mod_o3, ref = ref)
a2<- summary(mod_o3, ref = "Cruza"      )
a3<- summary(mod_o3, ref = "Ndamira")
a4<- summary(mod_o3, ref = "Kazeneza")
a5<- summary(mod_o3, ref = "Izihirwe")
a6<- summary(mod_o3, ref = "Gisubizo")
a7<- summary(mod_o3, ref = "Seka")
a8<-summary(mod_o3, ref = "Twihaze")
a9<-summary(mod_o3, ref = "Jyambere")
a10<- summary(mod_o3, ref = "Ndeze")
a11 <- summary(mod_o3, ref = "Nkunganire")

a1 <- a1$coefficients[,4]
a2 <- a2$coefficients[,4]
a3 <- a3$coefficients[,4]
a4 <- a4$coefficients[,4]
a5 <- a5$coefficients[,4]
a6 <- a6$coefficients[,4]
a7 <- a7$coefficients[,4]
a8 <- a8$coefficients[,4]
a9 <- a9$coefficients[,4]
a10 <- a10$coefficients[,4]
a11 <- a11$coefficients[,4]


alist<-list(a1, a2 ,a3 ,a4, a5, a6, a7,a8,a9,a10, a11)

amatrix<- do.call(rbind, alist)

amatrix[is.na(amatrix)] <- 0  
amatrix
amatrix
colnames(amatrix)
amatrix<- amatrix[,c("Kirundo",
           "Cruza",
           "Ndamira",
           "Kazeneza",
           "Izihirwe",
           "Gisubizo",
           "Seka",
           "Twihaze",
           "Jyambere",
           "Ndeze",
           "Nkunganire")]
amatrix
rownames(amatrix) <- c("Kirundo",
                       "Cruza",
                       "Ndamira",
                       "Kazeneza",
                       "Izihirwe",
                       "Gisubizo",
                       "Seka",
                       "Twihaze",
                       "Jyambere",
                       "Ndeze",
                       "Nkunganire")

amatrix
cxLtrs <- multcompLetters(amatrix, threshold=0.05)
cxLtrs


##
#plant into the future
a1<- summary(mod_f3, ref = ref)
a2<- summary(mod_f3, ref = "Cruza"      )
a3<- summary(mod_f3, ref = "Ndamira")
a4<- summary(mod_f3, ref = "Kazeneza")
a5<- summary(mod_f3, ref = "Izihirwe")
a6<- summary(mod_f3, ref = "Gisubizo")
a7<- summary(mod_f3, ref = "Seka")
a8<-summary(mod_f3, ref = "Twihaze")
a9<-summary(mod_f3, ref = "Jyambere")
a10<- summary(mod_f3, ref = "Ndeze")
a11 <- summary(mod_f3, ref = "Nkunganire")

a1 <- a1$coefficients[,4]
a2 <- a2$coefficients[,4]
a3 <- a3$coefficients[,4]
a4 <- a4$coefficients[,4]
a5 <- a5$coefficients[,4]
a6 <- a6$coefficients[,4]
a7 <- a7$coefficients[,4]
a8 <- a8$coefficients[,4]
a9 <- a9$coefficients[,4]
a10 <- a10$coefficients[,4]
a11 <- a11$coefficients[,4]


alist<-list(a1, a2 ,a3 ,a4, a5, a6, a7,a8,a9,a10, a11)

amatrix<- do.call(rbind, alist)

amatrix[is.na(amatrix)] <- 0  
amatrix
amatrix
colnames(amatrix)
amatrix<- amatrix[,c("Kirundo",
                     "Cruza",
                     "Ndamira",
                     "Kazeneza",
                     "Izihirwe",
                     "Gisubizo",
                     "Seka",
                     "Twihaze",
                     "Jyambere",
                     "Ndeze",
                     "Nkunganire")]
amatrix
rownames(amatrix) <- c("Kirundo",
                       "Cruza",
                       "Ndamira",
                       "Kazeneza",
                       "Izihirwe",
                       "Gisubizo",
                       "Seka",
                       "Twihaze",
                       "Jyambere",
                       "Ndeze",
                       "Nkunganire")

amatrix
cxLtrs <- multcompLetters(amatrix, threshold=0.05)
cxLtrs
##


#overall preference 
a1<- summary(mod_b3, ref = ref)
a2<- summary(mod_b3, ref = "Cruza"      )
a3<- summary(mod_b3, ref = "Ndamira")
a4<- summary(mod_b3, ref = "Kazeneza")
a5<- summary(mod_b3, ref = "Izihirwe")
a6<- summary(mod_b3, ref = "Gisubizo")
a7<- summary(mod_b3, ref = "Seka")
a8<-summary(mod_b3, ref = "Twihaze")
a9<-summary(mod_b3, ref = "Jyambere")
a10<- summary(mod_b3, ref = "Ndeze")
a11 <- summary(mod_b3, ref = "Nkunganire")

a1 <- a1$coefficients[,4]
a2 <- a2$coefficients[,4]
a3 <- a3$coefficients[,4]
a4 <- a4$coefficients[,4]
a5 <- a5$coefficients[,4]
a6 <- a6$coefficients[,4]
a7 <- a7$coefficients[,4]
a8 <- a8$coefficients[,4]
a9 <- a9$coefficients[,4]
a10 <- a10$coefficients[,4]
a11 <- a11$coefficients[,4]


alist<-list(a1, a2 ,a3 ,a4, a5, a6, a7,a8,a9,a10, a11)

amatrix<- do.call(rbind, alist)

amatrix[is.na(amatrix)] <- 0  
amatrix
amatrix
colnames(amatrix)
amatrix<- amatrix[,c("Kirundo",
                     "Cruza",
                     "Ndamira",
                     "Kazeneza",
                     "Izihirwe",
                     "Gisubizo",
                     "Seka",
                     "Twihaze",
                     "Jyambere",
                     "Ndeze",
                     "Nkunganire")]
amatrix
rownames(amatrix) <- c("Kirundo",
                       "Cruza",
                       "Ndamira",
                       "Kazeneza",
                       "Izihirwe",
                       "Gisubizo",
                       "Seka",
                       "Twihaze",
                       "Jyambere",
                       "Ndeze",
                       "Nkunganire")

amatrix
cxLtrs <- multcompLetters(amatrix, threshold=0.05)
cxLtrs

qvcalc(mod_f3, ref = ref)

####### PLT yield node analyze! 
#PLT yield node  2 and nod 3
plt_yield_node2 <- read.csv("data/plt_yied_node2.csv")
plt_yield_node3 <- read.csv("data/plt_yied_node3.csv")

########node 2 first !!
#
#Check the names of the ranking (positive and negative) traits
trait_list = getTraitList(plt_yield_node2, 
                          pattern = c("_pos", "_neg"))
tricot_trait<- unlist(lapply(trait_list, function(x) x$trait ))
tricot_trait

################################################################################
#Check how many farmer's left
row_number(plt_yield_node2[,1]) #113 number of farmers

#Check how many record left in each trait 
data.frame(trait = tricot_trait,
           records = unlist(lapply(trait_list, function(x) sum(x$keep))))

#Check tricot packages A B C
pack_index = c("package_item_A", "package_item_B", "package_item_C")

# Prepare list of rankings for different traits
R <- lapply(trait_list, function(x){
  rank_tricot(data = plt_yield_node2,
              items = pack_index,
              input = x$string, 
              validate.rankings = TRUE)
})

names(R) <- tricot_trait

mod_list<- lapply(R, function(x){PlackettLuce(x)})

#let's check yield 
mod_list["postharvest1_yield"]

names(mod_list$postharvest1_yield$coefficients)

var_11_list<-list()
varieties_11_yield <- names(mod_list$postharvest1_yield$coefficients)
for (i in 1:11){var_11_list[[i]] <- summary(mod_list$postharvest1_yield, ref= names(mod_list$postharvest1_yield$coefficients)[i])} 

var_11_list[[1]]$coefficients[,4]

var_11_list_temp <- list()
for(i in 1:11){var_11_list_temp[[i]]<- var_11_list[[i]]$coefficients[,4]}
var_matrix_yield<- do.call(rbind, var_11_list_temp)
var_matrix_yield[is.na(var_matrix_yield)] <- 0  
rownames(var_matrix_yield) <- colnames(var_matrix_yield)

names(sort(mod_list$postharvest1_yield$coefficients ,decreasing = T))

# keep_test<- var_matrix_yield[,c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze")]
# keep_test<- keep_test[c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze"),]

keep_test<- var_matrix_yield[,c(names(sort(mod_list$postharvest1_yield$coefficients ,decreasing = T)))]
keep_test<- keep_test[c(names(sort(mod_list$postharvest1_yield$coefficients ,decreasing = T))),]


multcompLetters(keep_test, threshold=0.05)


####node 3
#
#Check the names of the ranking (positive and negative) traits
trait_list = getTraitList(plt_yield_node3, 
                          pattern = c("_pos", "_neg"))
tricot_trait<- unlist(lapply(trait_list, function(x) x$trait ))
tricot_trait

################################################################################
#Check how many farmer's left
row_number(plt_yield_node3[,1]) #113 number of farmers

#Check how many record left in each trait 
data.frame(trait = tricot_trait,
           records = unlist(lapply(trait_list, function(x) sum(x$keep))))

#Check tricot packages A B C
pack_index = c("package_item_A", "package_item_B", "package_item_C")

# Prepare list of rankings for different traits
R <- lapply(trait_list, function(x){
  rank_tricot(data = plt_yield_node3,
              items = pack_index,
              input = x$string, 
              validate.rankings = TRUE)
})

names(R) <- tricot_trait

mod_list<- lapply(R, function(x){PlackettLuce(x)})

#let's check yield 
mod_list["postharvest1_yield"]

names(mod_list$postharvest1_yield$coefficients)

var_11_list<-list()
varieties_11_yield <- names(mod_list$postharvest1_yield$coefficients)
for (i in 1:11){var_11_list[[i]] <- summary(mod_list$postharvest1_yield, ref= names(mod_list$postharvest1_yield$coefficients)[i])} 

var_11_list[[1]]$coefficients[,4]

var_11_list_temp <- list()
for(i in 1:11){var_11_list_temp[[i]]<- var_11_list[[i]]$coefficients[,4]}
var_matrix_yield<- do.call(rbind, var_11_list_temp)
var_matrix_yield[is.na(var_matrix_yield)] <- 0  
rownames(var_matrix_yield) <- colnames(var_matrix_yield)

# 
# keep_test<- var_matrix_yield[,c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze")]
# keep_test<- keep_test[c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze"),]

keep_test<- var_matrix_yield[,c(names(sort(mod_list$postharvest1_yield$coefficients ,decreasing = T)))]
keep_test<- keep_test[c(names(sort(mod_list$postharvest1_yield$coefficients ,decreasing = T))),]


multcompLetters(keep_test, threshold=0.05)



###############################################################################
####################set significant letter for marketability.
####### PLT marketability node analyze! 
#PLT yield node  2,  nod 4, and nod5
partition_node2_market <- read.csv("data/marketafterstore_ph2_node1.csv")
partition_node4_market <- read.csv("data/marketafterstore_ph2_node2.csv")
partition_node5_market <- read.csv("data/marketafterstore_ph2_node3.csv")


#Check the names of the ranking (positive and negative) traits
trait_list = getTraitList(partition_nod2_market, 
                          pattern = c("_pos", "_neg"))
tricot_trait<- unlist(lapply(trait_list, function(x) x$trait ))
tricot_trait


################################################################################
#Check how many farmer's left
length(partition_node2_market[,1]) #60 number of farmers

#Check how many record left in each trait 
data.frame(trait = tricot_trait,
           records = unlist(lapply(trait_list, function(x) sum(x$keep))))


# Prepare list of rankings for different traits
R_market_node2 <- rankTricot(partition_node2_market[], 
                     items = c("package_item_A", "package_item_B", "package_item_C"),
                     input = c("postharvest2_marketableatstor_pos", "postharvest2_marketableatstor_neg"),
                     validate.rankings = TRUE)

mod_mareket_node2<- PlackettLuce(R_market_node2)


varieties_11_list<-list()
varieties_11_marketability <- names(mod_mareket_node2$coefficients)
for (i in 1:11){varieties_11_list[[i]] <- summary(mod_mareket_node2, ref= names(mod_mareket_node2$coefficients)[i])} 

varieties_11_list_temp <- list()
for(i in 1:11){varieties_11_list_temp[[i]]<- varieties_11_list[[i]]$coefficients[,4]}
varieties_matrix_marketability<- do.call(rbind, varieties_11_list_temp)
varieties_matrix_marketability[is.na(varieties_matrix_marketability)] <- 0  
rownames(varieties_matrix_marketability) <- colnames(varieties_matrix_marketability)
varieties_matrix_marketability

keep_test<- varieties_matrix_marketability[,c(names(sort(mod_mareket_node2$coefficients ,decreasing = T)))]
keep_test<- keep_test[c(names(sort(mod_mareket_node2$coefficients ,decreasing = T))),]


# keep_test<- varieties_matrix_marketability[,c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze")]
# keep_test<- keep_test[c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze"),]

multcompLetters(keep_test, threshold=0.05)

# Cruza   Gisubizo   Izihirwe   Jyambere   Kazeneza    Kirundo    Ndamira      Ndeze Nkunganire       Seka    Twihaze 
# "a"       "ab"      "bcd"      "bcd"        "c"      "abd"       "ab"      "bcd"       "cd"       "ab"      "bcd" 

##################################################################### 
###########
#Check how many farmer's left
length(partition_node4_market[,1]) #50 number of farmers

# Prepare list of rankings for different traits
R_market_node4 <- rankTricot(partition_node4_market[], 
                            items = c("package_item_A", "package_item_B", "package_item_C"),
                            input = c("postharvest2_marketableatstor_pos", "postharvest2_marketableatstor_neg"),
                            validate.rankings = TRUE)

mod_mareket_node4<- PlackettLuce(R_market_node4)


varieties_11_list<-list()
varieties_11_marketability <- names(mod_mareket_node4$coefficients)
for (i in 1:11){varieties_11_list[[i]] <- summary(mod_mareket_node4, ref= names(mod_mareket_node4$coefficients)[i])} 

varieties_11_list_temp <- list()
for(i in 1:11){varieties_11_list_temp[[i]]<- varieties_11_list[[i]]$coefficients[,4]}
varieties_matrix_marketability<- do.call(rbind, varieties_11_list_temp)
varieties_matrix_marketability[is.na(varieties_matrix_marketability)] <- 0  
rownames(varieties_matrix_marketability) <- colnames(varieties_matrix_marketability)
varieties_matrix_marketability


keep_test<- varieties_matrix_marketability[,c(names(sort(mod_mareket_node4$coefficients ,decreasing = T)))]
keep_test<- keep_test[c(names(sort(mod_mareket_node4$coefficients ,decreasing = T))),]




# keep_test<- varieties_matrix_marketability[,c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze")]
# keep_test<- keep_test[c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze"),]

multcompLetters(keep_test, threshold=0.05)
# 
# Cruza   Gisubizo   Izihirwe   Jyambere   Kazeneza    Kirundo    Ndamira      Ndeze Nkunganire       Seka    Twihaze 
# "a"        "b"       "ac"      "acd"      "acd"      "acd"        "b"        "b"        "b"       "bd"      "bcd" 
# 

##################################################################### 
###########
#Check how many farmer's left
length(partition_node5_market[,1]) #109 number of farmers

# Prepare list of rankings for different traits
R_market_node5 <- rankTricot(partition_node5_market[], 
                             items = c("package_item_A", "package_item_B", "package_item_C"),
                             input = c("postharvest2_marketableatstor_pos", "postharvest2_marketableatstor_neg"),
                             validate.rankings = TRUE)

mod_mareket_node5<- PlackettLuce(R_market_node5)


varieties_11_list<-list()
varieties_11_marketability <- names(mod_mareket_node5$coefficients)
for (i in 1:11){varieties_11_list[[i]] <- summary(mod_mareket_node5, ref= names(mod_mareket_node5$coefficients)[i])} 

varieties_11_list_temp <- list()
for(i in 1:11){varieties_11_list_temp[[i]]<- varieties_11_list[[i]]$coefficients[,4]}
varieties_matrix_marketability<- do.call(rbind, varieties_11_list_temp)
varieties_matrix_marketability[is.na(varieties_matrix_marketability)] <- 0  
rownames(varieties_matrix_marketability) <- colnames(varieties_matrix_marketability)
varieties_matrix_marketability

# 
# keep_test<- varieties_matrix_marketability[,c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze")]
# keep_test<- keep_test[c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze"),]



keep_test<- varieties_matrix_marketability[,c(names(sort(mod_mareket_node5$coefficients ,decreasing = T)))]
keep_test<- keep_test[c(names(sort(mod_mareket_node5$coefficients ,decreasing = T))),]


multcompLetters(keep_test,  threshold=0.05)
# Cruza   Gisubizo   Izihirwe   Jyambere   Kazeneza    Kirundo    Ndamira      Ndeze Nkunganire       Seka    Twihaze 
# "abcd"     "abce"      "abe"      "acd"     "abce"       "be"        "e"       "cd"        "d"     "abce"      "abe" 


###########################################
################analyze BW resistance.

partition_bW_node2<- read.csv("data subtract/potato_tricot_plant_bacterial_wilt_vg2_node2.csv")
partition_bW_node5<- read.csv("data subtract/potato_tricot_plant_bacterial_wilt_vg2_node5.csv")
partition_bW_node6<- read.csv("data subtract/potato_tricot_plant_bacterial_wilt_vg2_node6.csv")
partition_bW_node7<- read.csv("data subtract/potato_tricot_plant_bacterial_wilt_vg2_node7.csv")



########node 2 first !!
#
#Check the names of the ranking (positive and negative) traits
trait_list = getTraitList(partition_bW_node2, 
                          pattern = c("_pos", "_neg"))
tricot_trait<- unlist(lapply(trait_list, function(x) x$trait ))
tricot_trait

################################################################################
#Check how many farmer's left
row_number(partition_bW_node2[,1]) #113 number of farmers

#Check how many record left in each trait 
data.frame(trait = tricot_trait,
           records = unlist(lapply(trait_list, function(x) sum(x$keep))))

#Check tricot packages A B C
pack_index = c("package_item_A", "package_item_B", "package_item_C")


# Prepare list of rankings for different traits
R_bw_node2 <- rankTricot(partition_bW_node2[], 
                             items = c("package_item_A", "package_item_B", "package_item_C"),
                             input = c("vegetative2_bacterialwilt_pos", "vegetative2_bacterialwilt_neg"),
                             validate.rankings = TRUE)

mod_bw_node2<- PlackettLuce(R_bw_node2)

varieties_11_list<-list()
varieties_11_bw <- names(mod_bw_node2$coefficients)[1:11]
for (i in 1:11){varieties_11_list[[i]] <- summary(mod_bw_node2, ref= names(mod_bw_node2$coefficients)[i])} 

varieties_11_list_temp <- list()
for(i in 1:11){varieties_11_list_temp[[i]]<- varieties_11_list[[i]]$coefficients[,4][1:11]}
varieties_matrix_bw<- do.call(rbind, varieties_11_list_temp)
varieties_matrix_bw[is.na(varieties_matrix_bw)] <- 0  
rownames(varieties_matrix_bw) <- colnames(varieties_matrix_bw)
varieties_matrix_bw

keep_test<- varieties_matrix_bw[,c(names(sort(mod_bw_node2$coefficients[1:11] ,decreasing = T)))]
keep_test<- keep_test[c(names(sort(mod_bw_node2$coefficients[1:11] ,decreasing = T))),]


# keep_test<- varieties_matrix_bw[,c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze")]
# keep_test<- keep_test[c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze"),]

multcompLetters(keep_test, threshold=0.05)

##################
########node 5
#
#Check the names of the ranking (positive and negative) traits
trait_list = getTraitList(partition_bW_node5, 
                          pattern = c("_pos", "_neg"))
tricot_trait<- unlist(lapply(trait_list, function(x) x$trait ))
tricot_trait

################################################################################
#Check how many farmer's left
row_number(partition_bW_node5[,1]) #113 number of farmers

#Check how many record left in each trait 
data.frame(trait = tricot_trait,
           records = unlist(lapply(trait_list, function(x) sum(x$keep))))

#Check tricot packages A B C
pack_index = c("package_item_A", "package_item_B", "package_item_C")


# Prepare list of rankings for different traits
R_bw_node5 <- rankTricot(partition_bW_node5[], 
                         items = c("package_item_A", "package_item_B", "package_item_C"),
                         input = c("vegetative2_bacterialwilt_pos", "vegetative2_bacterialwilt_neg"),
                         validate.rankings = TRUE)

mod_bw_node5<- PlackettLuce(R_bw_node5)

varieties_11_list<-list()
varieties_11_bw <- names(mod_bw_node5$coefficients)[1:11]
for (i in 1:11){varieties_11_list[[i]] <- summary(mod_bw_node5, ref= names(mod_bw_node5$coefficients)[i])} 

varieties_11_list_temp <- list()
for(i in 1:11){varieties_11_list_temp[[i]]<- varieties_11_list[[i]]$coefficients[,4][1:11]}
varieties_matrix_bw<- do.call(rbind, varieties_11_list_temp)
varieties_matrix_bw[is.na(varieties_matrix_bw)] <- 0  
rownames(varieties_matrix_bw) <- colnames(varieties_matrix_bw)
varieties_matrix_bw

keep_test<- varieties_matrix_bw[,c(names(sort(mod_bw_node5$coefficients[1:11] ,decreasing = T)))]
keep_test<- keep_test[c(names(sort(mod_bw_node5$coefficients[1:11] ,decreasing = T))),]


# keep_test<- varieties_matrix_bw[,c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze")]
# keep_test<- keep_test[c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze"),]

multcompLetters(keep_test, threshold=0.05)
# 
# Cruza Nkunganire    Kirundo    Twihaze   Jyambere       Seka   Gisubizo   Izihirwe   Kazeneza      Ndeze    Ndamira 
   # "d"       "cd"      "bcd"      "bcd"      "abc"      "abc"      "abc"      "abc"      "abc"       "ab"        "a"

##################
########node 6
#
#Check the names of the ranking (positive and negative) traits
trait_list = getTraitList(partition_bW_node6, 
                          pattern = c("_pos", "_neg"))
tricot_trait<- unlist(lapply(trait_list, function(x) x$trait ))
tricot_trait

################################################################################
#Check how many farmer's left
row_number(partition_bW_node6[,1]) #113 number of farmers

#Check how many record left in each trait 
data.frame(trait = tricot_trait,
           records = unlist(lapply(trait_list, function(x) sum(x$keep))))

#Check tricot packages A B C
pack_index = c("package_item_A", "package_item_B", "package_item_C")


# Prepare list of rankings for different traits
R_bw_node6 <- rankTricot(partition_bW_node6[], 
                         items = c("package_item_A", "package_item_B", "package_item_C"),
                         input = c("vegetative2_bacterialwilt_pos", "vegetative2_bacterialwilt_neg"),
                         validate.rankings = TRUE)

mod_bw_node6<- PlackettLuce(R_bw_node6)

varieties_11_list<-list()
varieties_11_bw <- names(mod_bw_node6$coefficients)[1:11]
for (i in 1:11){varieties_11_list[[i]] <- summary(mod_bw_node6, ref= names(mod_bw_node6$coefficients)[i])} 

varieties_11_list_temp <- list()
for(i in 1:11){varieties_11_list_temp[[i]]<- varieties_11_list[[i]]$coefficients[,4][1:11]}
varieties_matrix_bw<- do.call(rbind, varieties_11_list_temp)
varieties_matrix_bw[is.na(varieties_matrix_bw)] <- 0  
rownames(varieties_matrix_bw) <- colnames(varieties_matrix_bw)
varieties_matrix_bw

keep_test<- varieties_matrix_bw[,c(names(sort(mod_bw_node6$coefficients[1:11] ,decreasing = T)))]
keep_test<- keep_test[c(names(sort(mod_bw_node6$coefficients[1:11] ,decreasing = T))),]


# keep_test<- varieties_matrix_bw[,c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze")]
# keep_test<- keep_test[c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze"),]

multcompLetters(keep_test, threshold=0.05)
# 


##################
########node 7
#
#Check the names of the ranking (positive and negative) traits
trait_list = getTraitList(partition_bW_node7, 
                          pattern = c("_pos", "_neg"))
tricot_trait<- unlist(lapply(trait_list, function(x) x$trait ))
tricot_trait

################################################################################
#Check how many farmer's left
row_number(partition_bW_node7[,1]) #113 number of farmers

#Check how many record left in each trait 
data.frame(trait = tricot_trait,
           records = unlist(lapply(trait_list, function(x) sum(x$keep))))

#Check tricot packages A B C
pack_index = c("package_item_A", "package_item_B", "package_item_C")


# Prepare list of rankings for different traits
R_bw_node7 <- rankTricot(partition_bW_node7[], 
                         items = c("package_item_A", "package_item_B", "package_item_C"),
                         input = c("vegetative2_bacterialwilt_pos", "vegetative2_bacterialwilt_neg"),
                         validate.rankings = TRUE)

mod_bw_node7<- PlackettLuce(R_bw_node7)

varieties_11_list<-list()
varieties_11_bw <- names(mod_bw_node7$coefficients)[1:11]
for (i in 1:11){varieties_11_list[[i]] <- summary(mod_bw_node7, ref= names(mod_bw_node7$coefficients)[i])} 

varieties_11_list_temp <- list()
for(i in 1:11){varieties_11_list_temp[[i]]<- varieties_11_list[[i]]$coefficients[,4][1:11]}
varieties_matrix_bw<- do.call(rbind, varieties_11_list_temp)
varieties_matrix_bw[is.na(varieties_matrix_bw)] <- 0  
rownames(varieties_matrix_bw) <- colnames(varieties_matrix_bw)
varieties_matrix_bw

keep_test<- varieties_matrix_bw[,c(names(sort(mod_bw_node7$coefficients[1:11] ,decreasing = T)))]
keep_test<- keep_test[c(names(sort(mod_bw_node7$coefficients[1:11] ,decreasing = T))),]


# keep_test<- varieties_matrix_bw[,c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze")]
# keep_test<- keep_test[c("Cruza","Gisubizo", "Izihirwe", "Jyambere", "Kazeneza", "Kirundo", "Ndamira", "Ndeze", "Nkunganire", "Seka", "Twihaze"),]

multcompLetters(keep_test, threshold=0.05)
# 
# Jyambere   Izihirwe   Kazeneza      Cruza      Ndeze    Ndamira Nkunganire       Seka    Kirundo   Gisubizo    Twihaze 
# "a"       "ab"      "abc"     "abcd"     "abcd"     "abcd"      "bcd"       "cd"       "cd"       "cd"        "d" 

