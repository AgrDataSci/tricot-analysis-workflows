############################################
#Second, check correlation between overall preference and other traits.  
# run packages
library("PlackettLuce")
library("gosset")
library("ClimMobTools")
library("ggplot2")
library("tibble")
library("dplyr")
library("corpcor")
library("GeneNet")


#Define new function to obtain a Kendall tau correlation matrix and effective N matrix
kendallTauCorMatrix <- function(L){
  
  n_i <- length(L)
  
  #Check if the list is longer than 3 and check if it contains rankings
  stopifnot(n_i>2, inherits(L[[1]], "rankings"), inherits(L[[2]], "rankings"))
  
  #Prepare the correlation matrix to be filled with values
  r <- matrix(NA, nrow=n_i, ncol=n_i)
  rownames(r) <- names(L)
  colnames(r) <- names(L)
  
  #Prepare the effective N matrix
  N <- matrix(NA, nrow=n_i, ncol=n_i)
  rownames(N) <- names(L)
  colnames(N) <- names(L)
  
  #Fill the matrix
  for(i in 1:n_i){
    for (j in 1:i){ #Fills lower triangle only, to avoid calculating the same value twice
      
      rNij <- kendallTau(L[[i]], L[[j]]) #TODO deal with NA values
      r[i,j] <- as.numeric(rNij[1])
      N[i,j] <- as.numeric(rNij[2]) 
      
    }
    
  }
  
  #Make matrices symmetric    
  r[upper.tri(r)] <- t(r)[upper.tri(r)]
  N[upper.tri(N)] <- t(N)[upper.tri(N)]
  
  #Wrap result
  results <- list(Kendall_tau_corr=r,Effective_N=N)
  return(results)
  
}


##### Function that returns the names of the ranking (positive and negative)  traits
getRankingTraitNames <- function(x, strip=TRUE){
  
  r <- unlist(colnames(x))
  r <- r[grepl("_pos", r) | grepl("_neg", r)]
  if(strip) r <- unlist(unique(strsplit(r, c("_pos", "_neg"))))
  return(r)
  
}


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

########################################################



#read the data
dt <- read.csv("data/potato-tricot-data-with-covariate_for_manuscript_QS.csv")


#read marketablility after store at ph2 node data
partition_node1<- read.csv("data/marketafterstore_ph2_node1.csv")
partition_node2<- read.csv("data/marketafterstore_ph2_node2.csv")
partition_node3<- read.csv("data/marketafterstore_ph2_node3.csv")



#check data structure
str(dt)
head(dt)
tail(dt)
summary(dt)
table_id <- table(dt$id)
duplicated_ID <- names(table_id[table_id>1]) #find duplicate!! 

del1<- which(dt$id==duplicated_ID[1])
del1 <-del1[[-1]] 
del2<- which(dt$id==duplicated_ID[2]) 
del2 <-del2[[-1]] 
del3<- which(dt$id==duplicated_ID[3])  
del3 <-del3[[-1]] 

dt <- dt[-c(del1, del2, del3),] #data structure (455 obs. of 160 variables)
which(table(dt$id) > 1) #check data structure again
#######################################################
#######################################Quality scoring base##########################################

hist(dt$Total_sum_QS_point.without.BW, labels = TRUE)
hist(dt$Total_sum_QS_point.with.BW, labels = TRUE)
#based on "dt$Total_sum_QS_point.with.BW" I will deducted data that has bigger than 99.(there are 23 data set bigger equal or than 99. 99 means data might be biased.)


QS <- which(dt$Total_sum_QS_point.with.BW >98)
dt <- dt[-c(QS),]
#tricot date frame 430obs

################################setting before starting plackett luce ###############################
#set item varieties as list for an easy way!!  
variety <- c("package_item_A", "package_item_B", "package_item_C")
# using reference variety "Kirundo" because it has been released since 1989.
ref = "Kirundo"
# check traits names & tricot results
trait_names_posneg <- getRankingTraitNames(dt, strip=FALSE)
trait_names <- getRankingTraitNames(dt, strip=TRUE)
trait_names_pos <- trait_names_posneg[grepl("_pos", trait_names_posneg)]
trait_names_neg <- trait_names_posneg[grepl("_neg", trait_names_posneg)]


########################################
# checking how many record left after we remove all equal case (e.g A = B = C) plackettluce models cannot calculate all equal cases 
#plus, we do not know NA occur because farmers think all same or trial fails induced results
records <- vector(length=length(trait_names))
names(records) <- trait_names
#change "Not observed" to "NA"
dt[dt == "Not observed"] <- NA

for(i in 1:length(trait_names)){
  test_test<- dt[,grepl(trait_names[i], colnames(dt))]
  records[i] <-  nrow(test_test[-which(is.na(test_test[,1]) & is.na(test_test[,2])),])
}
records

#Inspect results
as.data.frame(records)


# Prepare list of rankings for different traits

d_i<- list()

for(i in 1:length(trait_names)){
  d_i_t <- cbind(dt[, c("package_item_A", "package_item_B", "package_item_C")], dt[, grepl(trait_names[i], colnames(dt))])
  #cbind for climate covariate
  d_i[[i]]  <- d_i_t
}
d_i
names(d_i) <- trait_names

#tranfer list to one dataframe form by using cbind
temp_dt <- cbind(d_i[[1]],d_i[[2]][,4:5])
for(i in 1:18){temp_dt <- cbind(temp_dt,d_i[[i+2]][,4:5])}

temp_dt

list_traitname <- list()
for(i in 1:20){list_traitname[[i]]<- c(trait_names_pos[i], trait_names_neg[i])}
list_traitname
list_traitname[20]
#It is a dataframe that do not deducted NA yet because calculate kendall tau correaltion trait vs trait. 
#It will compare traits one vs. one. 
#Among traits comparison, trait data with low numbers could be mainly based on calculating other traits' correlation because NA hinter the calculation. 
#So, it will match the number of rows in the data frame to the smaller number of data being compared.
#for example, plant vigor data number is 431 but tuber quality is 81. So, 350 data of plant vigor that do not have tuber quality data will be deducted.
#############################################################
###########overall preference vs all:one by one##############
df_list <- list()
var<-list()
var_overalpre <- list()
list_kendall <- list()
for( i in 1:19){df_list[[i]] <- cbind(d_i[[i]],d_i[[20]])
sel_name<- ifelse(records[20] > records[i], list_traitname[i], list_traitname[20])
df_list[[i]]<- df_list[[i]][-which(is.na(df_list[[i]][,which(colnames(df_list[[i]]) == sel_name[[1]][1])]) &
                                     is.na(df_list[[i]][,which(colnames(df_list[[i]]) == sel_name[[1]][2])])),]
var[[i]]<- rankTricot(df_list[[i]], items = 1:3, input = 4:5)
var_overalpre[[i]]<- rankTricot(df_list[[i]], items = 6:8, input = 9:10)
list_kendall[[i]] <-kendallTau(var[[i]], var_overalpre[[i]])
}
df_list
list_kendall
#add oneself correlation analsys. 

df_list[[20]]<- d_i[[20]][-which(is.na(d_i[[20]][,4]) &
                                     is.na(d_i[[20]][,5])),]
list_kendall[[20]] <- kendallTau(rankTricot(df_list[[20]], items = 1:3, input = 4:5), rankTricot(df_list[[20]], items = 1:3, input = 4:5))

#calculate p-value but before do it extract data from list 

list_kendall_k <- list()
list_kendall_n <- list()
for (i in 1:20){list_kendall_k[i]<- list_kendall[[i]]$kendallTau
                list_kendall_n[i]<- list_kendall[[i]]$N_effective}

kdcol_pref<- cbind(unlist(list_kendall_k),unlist(list_kendall_n))
kdcol_pref
#calculate pvalue

zpv<- c()
z_pvalue<- list()
for(i in 1:20){
     au<-kdcol_pref[i,1]/(sqrt(2*(2 *kdcol_pref[,2] +5))/sqrt(9*kdcol_pref[,2]*(kdcol_pref[,2]-1)))
    zpv<-au
    z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
    z_pvalue[i] <- z_value
  }
z_pvalue

z_value_preference<- unlist(z_pvalue)
kendalcorr_preference<-  cbind(kdcol_pref,z_value_preference)
kendalcorr_preference
colnames(kendalcorr_preference) <-c("overall preference_kendallTau", "N_effective", "p_value")
row.names(kendalcorr_preference) <- trait_names
kendalcorr_preference

write.table(kendalcorr_preference,file="output_thesis/kendallcorrlation_alltrai_new_version_preference.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

###############################################################
###########overall performance vs all: one by one##############

df_list <- list()
var<-list()
var_overalpre <- list()
list_kendall <- list()
#change trait sequence to easily calculate(overall performance(18) -> overall preference(20))
#                                         overall preference(20)  -> overall performance(18)   
keep_d_i <- list()
for(i in 1:17){keep_d_i[[i]] <-d_i[[i]] }
keep_d_i[[18]] <- d_i[[20]] 
keep_d_i[[19]] <- d_i[[19]] 
keep_d_i[[20]] <- d_i[[18]] 
#channge recode sequence too
records_t <- records
records_t[18] <- records[20]
records_t[20] <- records[18]
records_t


temp_18<- names(records_t)[18]
temp_20<- names(records_t)[20]
names(records_t)[18] <- temp_20
names(records_t)[20] <- temp_18
records_t


list_traitname_per<- list_traitname
temp_18<- list_traitname[[18]] 
temp_20<- list_traitname[[20]] 
list_traitname_per[[18]] <- temp_20
list_traitname_per[[20]] <- temp_18
list_traitname_per

trait_names_per <- trait_names
temp_name_18<- trait_names[18]
temp_name_20<- trait_names[20]
trait_names_per[18] <- temp_name_20
trait_names_per[20] <- temp_name_18


for( i in 1:19){df_list[[i]] <- cbind(keep_d_i[[i]],keep_d_i[[20]])
sel_name<- ifelse(records_t[20] > records_t[i], list_traitname_per[i], list_traitname_per[20])
df_list[[i]]<- df_list[[i]][-which(is.na(df_list[[i]][,which(colnames(df_list[[i]]) == sel_name[[1]][1])]) &
                                     is.na(df_list[[i]][,which(colnames(df_list[[i]]) == sel_name[[1]][2])])),]
var[[i]]<- rankTricot(df_list[[i]], items = 1:3, input = 4:5)
var_overalpre[[i]]<- rankTricot(df_list[[i]], items = 6:8, input = 9:10)
list_kendall[[i]] <-kendallTau(var[[i]], var_overalpre[[i]])
}
df_list
list_kendall
#add oneself correlation analyze. 

df_list[[20]]<- keep_d_i[[20]][-which(is.na(keep_d_i[[20]][,4]) &
                                   is.na(keep_d_i[[20]][,5])),]
list_kendall[[20]] <- kendallTau(rankTricot(df_list[[20]], items = 1:3, input = 4:5), rankTricot(df_list[[20]], items = 1:3, input = 4:5))

#calculate p-value but before do it extract data from list 

list_kendall_k <- list()
list_kendall_n <- list()
for (i in 1:20){list_kendall_k[i]<- list_kendall[[i]]$kendallTau
list_kendall_n[i]<- list_kendall[[i]]$N_effective}

kdcol_pref<- cbind(unlist(list_kendall_k),unlist(list_kendall_n))
kdcol_pref
#calculate pvalue

zpv<- c()
z_pvalue<- list()
for(i in 1:20){
  au<-kdcol_pref[i,1]/(sqrt(2*(2 *kdcol_pref[,2] +5))/sqrt(9*kdcol_pref[,2]*(kdcol_pref[,2]-1)))
  zpv<-au
  z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
  z_pvalue[i] <- z_value
}
z_pvalue


z_value_performance<- unlist(z_pvalue)
kendalcorr_performance<-  cbind(kdcol_pref,z_value_performance)
kendalcorr_performance
colnames(kendalcorr_performance) <-c("overall performance_kendallTau", "N_effective", "p_value")
row.names(kendalcorr_performance) <- trait_names_per
kendalcorr_performance

write.table(kendalcorr_performance,file="output_thesis/kendallcorrlation_alltrai_new_version_performance.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



###############################################################
###########plant into the future vs all: one by one##############

df_list <- list()
var<-list()
var_overalpre <- list()
list_kendall <- list()
#change trait sequence to easily calculate(overall performance(18) -> overall preference(20))
#                                         overall preference(20)  -> overall performance(18)   
keep_d_i <- list()
for(i in 1:18){keep_d_i[[i]] <-d_i[[i]] }
keep_d_i[[19]] <- d_i[[20]] 
keep_d_i[[20]] <- d_i[[19]] 
#channge recode sequence too
records_t <- records
records_t[19] <- records[20]
records_t[20] <- records[19]
records_t


temp_19<- names(records_t)[19]
temp_20<- names(records_t)[20]
names(records_t)[19] <- temp_20
names(records_t)[20] <- temp_19
records_t

list_traitname_ptf <- list_traitname
temp_19<- list_traitname[[19]] 
temp_20<- list_traitname[[20]] 
list_traitname_ptf[[19]] <- temp_20
list_traitname_ptf[[20]] <- temp_19
list_traitname_ptf


trait_names_ptf <- trait_names
temp_name_ptf_19<- trait_names[19]
temp_name_ptf_20<- trait_names[20]
trait_names_ptf[19] <- temp_name_ptf_20
trait_names_ptf[20] <- temp_name_ptf_19



for( i in 1:19){df_list[[i]] <- cbind(keep_d_i[[i]],keep_d_i[[20]])
sel_name<- ifelse(records_t[20] > records_t[i], list_traitname_ptf[i], list_traitname_ptf[20])
df_list[[i]]<- df_list[[i]][-which(is.na(df_list[[i]][,which(colnames(df_list[[i]]) == sel_name[[1]][1])]) &
                                     is.na(df_list[[i]][,which(colnames(df_list[[i]]) == sel_name[[1]][2])])),]
var[[i]]<- rankTricot(df_list[[i]], items = 1:3, input = 4:5)
var_overalpre[[i]]<- rankTricot(df_list[[i]], items = 6:8, input = 9:10)
list_kendall[[i]] <-kendallTau(var[[i]], var_overalpre[[i]])
}
df_list
list_kendall
#add oneself correlation analyze. 

df_list[[20]]<- keep_d_i[[20]][-which(is.na(keep_d_i[[20]][,4]) &
                                        is.na(keep_d_i[[20]][,5])),]
list_kendall[[20]] <- kendallTau(rankTricot(df_list[[20]], items = 1:3, input = 4:5), rankTricot(df_list[[20]], items = 1:3, input = 4:5))

#calculate p-value but before do it extract data from list 

list_kendall_k <- list()
list_kendall_n <- list()
for (i in 1:20){list_kendall_k[i]<- list_kendall[[i]]$kendallTau
list_kendall_n[i]<- list_kendall[[i]]$N_effective}

kdcol_pref<- cbind(unlist(list_kendall_k),unlist(list_kendall_n))
kdcol_pref
#calculate pvalue

zpv<- c()
z_pvalue<- list()
for(i in 1:20){
  au<-kdcol_pref[i,1]/(sqrt(2*(2 *kdcol_pref[,2] +5))/sqrt(9*kdcol_pref[,2]*(kdcol_pref[,2]-1)))
  zpv<-au
  z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
  z_pvalue[i] <- z_value
}
z_pvalue


z_value_plantintofuture<- unlist(z_pvalue)
kendalcorr_plantintofuture<-  cbind(kdcol_pref,z_value_plantintofuture)
kendalcorr_plantintofuture
colnames(kendalcorr_plantintofuture) <-c("overall plantintofuture_kendallTau", "N_effective", "p_value")
row.names(kendalcorr_plantintofuture) <- trait_names_ptf
kendalcorr_plantintofuture

write.table(kendalcorr_plantintofuture,file="output_thesis/kendallcorrlation_alltrai_new_version_plantintofuture.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#################################################################################
#################################################################################
################Now analyze node######################################
#####################################
#Set non-observed to NA
partition_node1[partition_node1 == "Not observed"] <- NA
partition_node2[partition_node2 == "Not observed"] <- NA
partition_node3[partition_node3 == "Not observed"] <- NA


#####################################
###############################node 1
table(partition_node1$postharvest3_tuberquality_pos)
table(partition_node1$postharvest3_tuberquality_neg)

#because there is no data tuber quality. So, they turn oo logic...it must change to character
partition_node1$postharvest3_tuberquality_pos <-as.character(partition_node1$postharvest3_tuberquality_pos)
partition_node1$postharvest3_tuberquality_neg <- as.character(partition_node1$postharvest3_tuberquality_pos)

#########################################################################
# Check how many record left in each trait 
tnames_posneg <- getRankingTraitNames(partition_node1, strip=FALSE)
tnames <- getRankingTraitNames(partition_node1, strip=TRUE)
records <- vector(length=length(tnames))
names(records) <- tnames
for(i in 1:length(tnames)){
  
  records[i] <- sum(complete.cases(partition_node1[,grepl(tnames[i], colnames(partition_node1))]))
  
}

#Inspect results
as.data.frame(records)

testing<-partition_node1[,c(77:115)]

#check NA
#doramncy 3
which(is.na(testing[,38]) & is.na(testing[,39]))

#postharvest1_maturity            
which(is.na(testing[,1]) & is.na(testing[,2]))

#yield 
which(is.na(testing[3]) & is.na(testing[,4]))

#postharvest1_tuberappearance 
which(is.na(testing[5]) & is.na(testing[,6]))

#node 1
#postharvest1_maturity              59
#postharvest1_yield                 59
#postharvest1_tubersize             59
#postharvest1_tuberappearance       59
#postharvest1_marketableatharv      20 <- we do not use this one 
#postharvest1_taste                 59
#postharvest1_marketableatstor      39 <- we do not use this one
#postharvest2_marketableatstor      60
#postharvest2_dormancy              60
#postharvest2_taste                 60

node1_market_list<- list_traitname[c(6:9, 11:16)]
node1_market_list

#Create the necessary data frame based on node 1
partition_node1_test_df<- partition_node1[, c("package_item_A", "package_item_B", "package_item_C","postharvest1_maturity_pos" ,"postharvest1_maturity_neg", 
                                           "postharvest1_yield_pos" ,"postharvest1_yield_neg",
                                            "postharvest1_tubersize_pos" ,"postharvest1_tubersize_neg",
                                           "postharvest1_tuberappearance_pos" ,"postharvest1_tuberappearance_neg",
                                           "postharvest1_taste_pos" ,"postharvest1_taste_neg",
                                           "postharvest2_dormancy_pos", "postharvest2_dormancy_neg",
                                           "postharvest2_taste_pos", "postharvest2_taste_neg",
                                           "postharvest3_dormancy_pos", "postharvest3_dormancy_neg",
                                           "postharvest2_marketableatstor_pos" ,"postharvest2_marketableatstor_neg")]

partition_node1_test_df[]
#remove NA raw
partition_node1_test_df_t<- na.omit(partition_node1_test_df[])
partition_node1_test_df_t

#extract name
tnames_node1<- names(partition_node1_test_df)
tnames_node1<- tnames_node1[-c(1:3)]
tnames_node1
trait_names_node1 <-   c("postharvest1_maturity", "postharvest1_yield",           "postharvest1_tubersize",        "postharvest1_tuberappearance" ,
                          "postharvest1_taste" ,"postharvest2_dormancy"    ,     "postharvest2_taste",           
                         "postharvest3_dormancy","postharvest2_marketableatstor")  

#put tarit in the list one by one 
ttt<- partition_node1_test_df_t[,c(1:3)]
ttt_tt<- partition_node1_test_df_t[]
ttt_tt[,c(grep(trait_names_node1[1], names(partition_node1_test_df_t)))]

rlist <- list()
for(i in 1:9){
  rlist[[i]] <- cbind(ttt, ttt_tt[,c(grep(trait_names_node1[i], names(partition_node1_test_df_t)))])}
rlist

ttt_tt[,c(grep(trait_names_node1[8], names(partition_node1_test_df_t)))]



rlist[1]
ttt
str(ttt)



# Prepare list of rankings for different traits
R <- list()
for(i in seq_along(trait_names_node1)){
  
  # select the item names and rankings for the trait in the iteration i
  # put in the matrix
  R[[i]] <- rankTricot(rlist[[i]], items = 1:3, input = 4:5)}

names(R) <- trait_names_node1
R


rN <- kendallTauCorMatrix(R)
r <- rN[[1]]
N <- rN[[2]]
r

#kendall 
write.table(r,file="output_thesis/node1_kendallcorrlation_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


zpv<- c()
ztest_pvalue_matrix <- matrix(NA, nrow=9, ncol=9)
#Fill the matrix
for(i in 1:9){
  for (j in 1:9){ #Fills lower triangle only, to avoid calculating the same value twice
    
    au<-r[j,i]/(sqrt(2*(2 *N[[i]] +5))/sqrt(9*N[[i]]*(N[[i]]-1)))
    zpv<-au
    keep_z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
    ztest_pvalue_matrix[j,i] <- keep_z_value
  }}

ztest_pvalue_matrix

keep_ztest_pvalue_matrix <- as.data.frame(ztest_pvalue_matrix)
row.names(keep_ztest_pvalue_matrix) <- trait_names_node1
colnames(keep_ztest_pvalue_matrix) <- trait_names_node1
keep_ztest_pvalue_matrix

#Kandell_pvalue
write.table(keep_ztest_pvalue_matrix,file="output_thesis/node1_kendall_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


# Test with the full matrix, just to see if it works
pr <- cor2pcor(r)

#calculate partiall correlation of Kendall tau
pr

# Simple way to work with these values is to convert to Pearson's r
r_pearson <- sin(3.141592654*r*.5)
r_pearson


#pearson
write.table(r_pearson,file="output_thesis/node1_Pearson_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

#calculate pearson correlation P-value 
ttv<- list()
ttest_pvalue_matrix <- matrix(NA, nrow=9, ncol=9)

#Fill the matrix
for(i in 1:9){
  for (j in 1:9){ #Fills lower triangle only, to avoid calculating the same value twice
    
    keep_tt<- r_pearson[j,i] *sqrt((N[[i]]-2)/ (1-(r_pearson[j,i]^2)))
    ttv <- keep_tt
    keep_tt_value<- 2 * (pt(-abs(ttv), N[[i]]-2))
    ttest_pvalue_matrix[j,i] <- keep_tt_value}}

ttest_pvalue_matrix

keep_ttest_pvalue_matrix <- as.data.frame(ttest_pvalue_matrix)
row.names(keep_ttest_pvalue_matrix) <- trait_names_node1
colnames(keep_ttest_pvalue_matrix) <- trait_names_node1
keep_ttest_pvalue_matrix

#pearson P value 
write.table(keep_ttest_pvalue_matrix,file="output_thesis/node1_pear_corrlation_t-test_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


pcor_pearson <- cor2pcor(r_pearson[])
row.names(pcor_pearson) <- trait_names_node1
colnames(pcor_pearson) <- trait_names_node1
pcor_pearson

#partial correlation
write.table(pcor_pearson,file="output_thesis/node1_partial_corrlation_alltrait_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


#calculate partial correlation P-value
degree_f<-n2kappa(N[[i]])
pr_val0<-cor0.test(pcor_pearson, degree_f)
pr_val0[]

pr_val0<-as.data.frame(pr_val0[])
row.names(pr_val0) <- trait_names_node1
colnames(pr_val0) <- trait_names_node1
pr_val0

#partial p value
write.table(pr_val0,file="output_thesis/node1_partial_corrlation_t-test_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


########################################################################
###############################node 2
table(partition_node2$postharvest3_tuberquality_pos)
table(partition_node2$postharvest3_tuberquality_neg)

#because there is no data tuber quality. So, they turn oo logic...it must change to character
partition_node2$postharvest3_tuberquality_pos <-as.character(partition_node2$postharvest3_tuberquality_pos)
partition_node2$postharvest3_tuberquality_neg <- as.character(partition_node2$postharvest3_tuberquality_pos)

#########################################################################
# Check how many record left in each trait 
tnames_posneg <- getRankingTraitNames(partition_node2, strip=FALSE)
tnames <- getRankingTraitNames(partition_node2, strip=TRUE)
records <- vector(length=length(tnames))
names(records) <- tnames
for(i in 1:length(tnames)){
  
  records[i] <- sum(complete.cases(partition_node2[,grepl(tnames[i], colnames(partition_node2))]))
  
}

#Inspect results
as.data.frame(records)

testing<-partition_node2[,c(77:115)]

#check NA
#doramncy 3
which(is.na(testing[,38]) & is.na(testing[,39]))

#postharvest1_maturity            
which(is.na(testing[,1]) & is.na(testing[,2]))

#yield 
which(is.na(testing[3]) & is.na(testing[,4]))

#postharvest1_tuberappearance 
which(is.na(testing[5]) & is.na(testing[,6]))

#node 2
#postharvest1_maturity              49
#postharvest1_yield                 49
#postharvest1_tubersize             49
#postharvest1_tuberappearance       49
#postharvest1_marketableatharv      19  <- we will not use it 
#postharvest1_taste                 49
#postharvest1_marketableatstor      30  <- we will not use it
#postharvest2_marketableatstor      50
#postharvest2_dormancy              50
#postharvest2_taste                 50
#postharvest3_dormancy              50

node2_market_list<- list_traitname[c(6:9, 11:16)]
node2_market_list

#Create the necessary data frame based on node 1
partition_node2_test_df<- partition_node2[, c("package_item_A", "package_item_B", "package_item_C","postharvest1_maturity_pos" ,"postharvest1_maturity_neg", 
                                            "postharvest1_yield_pos" ,"postharvest1_yield_neg",
                                            "postharvest1_tubersize_pos" ,"postharvest1_tubersize_neg",
                                            "postharvest1_tuberappearance_pos" ,"postharvest1_tuberappearance_neg",
                                            "postharvest1_taste_pos" ,"postharvest1_taste_neg",
                                            "postharvest2_dormancy_pos", "postharvest2_dormancy_neg",
                                            "postharvest2_taste_pos", "postharvest2_taste_neg",
                                            "postharvest3_dormancy_pos", "postharvest3_dormancy_neg",
                                            "postharvest2_marketableatstor_pos" ,"postharvest2_marketableatstor_neg")]

partition_node2_test_df[]
partition_node2[,c(100,101)]
partition_node2_test_df[,c(20,21)]
#check 
ifelse(partition_node2[,c(100,101)] == partition_node2_test_df[,c(20,21)], T, F) 

#remove NA raw and make same size dataframe
partition_node2_test_df_t<- na.omit(partition_node2_test_df[])
partition_node2_test_df_t

str(partition_node2_test_df_t)

#extract name
tnames_node2<- names(partition_node2_test_df)
tnames_node2<- tnames_node2[-c(1:3)]
tnames_node2
trait_names_node2 <-   c("postharvest1_maturity", "postharvest1_yield",           "postharvest1_tubersize",        "postharvest1_tuberappearance" ,
                         "postharvest1_taste" ,"postharvest2_dormancy"    ,     "postharvest2_taste",           
                         "postharvest3_dormancy","postharvest2_marketableatstor")  

#put tarit in the list one by one 
ttt<- partition_node2_test_df_t[,c(1:3)]
ttt_tt<- partition_node2_test_df_t[]
ttt_tt[,c(grep(trait_names_node2[1], names(partition_node2_test_df_t)))]

rlist <- list()
for(i in 1:9){
  rlist[[i]] <- cbind(ttt, ttt_tt[,c(grep(trait_names_node2[i], names(partition_node2_test_df_t)))])}
rlist

ttt_tt[,c(grep(trait_names_node2[8], names(partition_node2_test_df_t)))]

str(ttt)
str(rlist)

# Prepare list of rankings for different traits
R <- list()
for(i in seq_along(trait_names_node2)){
  
  # select the item names and rankings for the trait in the iteration i
  # put in the matrix
  R[[i]] <- rankTricot(rlist[[i]], items = 1:3, input = 4:5)}

names(R) <- trait_names_node2
R


rN <- kendallTauCorMatrix(R)
r <- rN[[1]]
N <- rN[[2]]
r

#kendall 
write.table(r,file="output_thesis/node2_kendallcorrlation_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


zpv<- c()
ztest_pvalue_matrix <- matrix(NA, nrow=9, ncol=9)
#Fill the matrix
for(i in 1:9){
  for (j in 1:9){ #Fills lower triangle only, to avoid calculating the same value twice
    
    au<-r[j,i]/(sqrt(2*(2 *N[[i]] +5))/sqrt(9*N[[i]]*(N[[i]]-1)))
    zpv<-au
    keep_z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
    ztest_pvalue_matrix[j,i] <- keep_z_value
  }}

ztest_pvalue_matrix

keep_ztest_pvalue_matrix <- as.data.frame(ztest_pvalue_matrix)
row.names(keep_ztest_pvalue_matrix) <- trait_names_node2
colnames(keep_ztest_pvalue_matrix) <- trait_names_node2
keep_ztest_pvalue_matrix

#Kandell_pvalue
write.table(keep_ztest_pvalue_matrix,file="output_thesis/node2_kendall_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


# Test with the full matrix, just to see if it works
pr <- cor2pcor(r)

#calculate partiall correlation of Kendall tau
pr

# Simple way to work with these values is to convert to Pearson's r
r_pearson <- sin(3.141592654*r*.5)
r_pearson


#pearson
write.table(r_pearson,file="output_thesis/node2_Pearson_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

#calculate pearson correlation P-value 
ttv<- list()
ttest_pvalue_matrix <- matrix(NA, nrow=9, ncol=9)

#Fill the matrix
for(i in 1:9){
  for (j in 1:9){ #Fills lower triangle only, to avoid calculating the same value twice
    
    keep_tt<- r_pearson[j,i] *sqrt((N[[i]]-2)/ (1-(r_pearson[j,i]^2)))
    ttv <- keep_tt
    keep_tt_value<- 2 * (pt(-abs(ttv), N[[i]]-2))
    ttest_pvalue_matrix[j,i] <- keep_tt_value}}

ttest_pvalue_matrix

keep_ttest_pvalue_matrix <- as.data.frame(ttest_pvalue_matrix)
row.names(keep_ttest_pvalue_matrix) <- trait_names_node2
colnames(keep_ttest_pvalue_matrix) <- trait_names_node2
keep_ttest_pvalue_matrix

#pearson P value 
write.table(keep_ttest_pvalue_matrix,file="output_thesis/node2_pear_corrlation_t-test_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


pcor_pearson <- cor2pcor(r_pearson[])
row.names(pcor_pearson) <- trait_names_node2
colnames(pcor_pearson) <- trait_names_node2
pcor_pearson

#partial correlation
write.table(pcor_pearson,file="output_thesis/node2_partial_corrlation_alltrait_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


#calculate partial correlation P-value
degree_f<-n2kappa(N[[i]])
pr_val0<-cor0.test(pcor_pearson, degree_f)
pr_val0[]

pr_val0<-as.data.frame(pr_val0[])
row.names(pr_val0) <- trait_names_node2
colnames(pr_val0) <- trait_names_node2
pr_val0

#partial p value
write.table(pr_val0,file="output_thesis/node2_partial_corrlation_t-test_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

########################################################################
###############################node 3
table(partition_node3$postharvest3_tuberquality_pos)
table(partition_node3$postharvest3_tuberquality_neg)

#because there is no data tuber quality. So, they turn oo logic...it must change to character
partition_node3$postharvest3_tuberquality_pos <-as.character(partition_node3$postharvest3_tuberquality_pos)
partition_node3$postharvest3_tuberquality_neg <- as.character(partition_node3$postharvest3_tuberquality_pos)

#########################################################################
# Check how many record left in each trait 
tnames_posneg <- getRankingTraitNames(partition_node3, strip=FALSE)
tnames <- getRankingTraitNames(partition_node3, strip=TRUE)
records <- vector(length=length(tnames))
names(records) <- tnames
for(i in 1:length(tnames)){
  
  records[i] <- sum(complete.cases(partition_node3[,grepl(tnames[i], colnames(partition_node3))]))
  
}

#Inspect results
as.data.frame(records)

testing<-partition_node3[,c(77:115)]

#check NA
#doramncy 3
which(is.na(testing[,38]) & is.na(testing[,39]))

#postharvest1_maturity            
which(is.na(testing[,1]) & is.na(testing[,2]))

#yield 
which(is.na(testing[3]) & is.na(testing[,4]))

#postharvest1_tuberappearance 
which(is.na(testing[5]) & is.na(testing[,6]))

#node 3
#postharvest1_maturity             109
#postharvest1_yield                109
#postharvest1_tubersize            109
#postharvest1_tuberappearance      109
#postharvest1_marketableatharv      65 <- we will not use it
#postharvest1_taste                109
#postharvest1_marketableatstor      44 <- we will not use it
#postharvest2_marketableatstor     109
#postharvest2_dormancy             109
#postharvest2_taste                109
#postharvest3_dormancy             107

node3_market_list<- list_traitname[c(6:9, 11:16)]
node3_market_list

#Create the necessary data frame based on node 1
partition_node3_test_df<- partition_node3[, c("package_item_A", "package_item_B", "package_item_C","postharvest1_maturity_pos" ,"postharvest1_maturity_neg", 
                                              "postharvest1_yield_pos" ,"postharvest1_yield_neg",
                                              "postharvest1_tubersize_pos" ,"postharvest1_tubersize_neg",
                                              "postharvest1_tuberappearance_pos" ,"postharvest1_tuberappearance_neg",
                                              "postharvest1_taste_pos" ,"postharvest1_taste_neg",
                                              "postharvest2_dormancy_pos", "postharvest2_dormancy_neg",
                                              "postharvest2_taste_pos", "postharvest2_taste_neg",
                                              "postharvest3_dormancy_pos", "postharvest3_dormancy_neg",
                                              "postharvest2_marketableatstor_pos" ,"postharvest2_marketableatstor_neg")]

partition_node3_test_df[]
partition_node3[,c(100,101)]
partition_node3_test_df[,c(20,21)]
#check 
ifelse(partition_node3[,c(100,101)] == partition_node3_test_df[,c(20,21)], T, F) 

#remove NA raw and make same size dataframe
partition_node3_test_df_t<- na.omit(partition_node3_test_df[])
partition_node3_test_df_t

str(partition_node3_test_df_t)

#extract name
tnames_node3<- names(partition_node3_test_df)
tnames_node3<- tnames_node3[-c(1:3)]
tnames_node3
trait_names_node3 <-   c("postharvest1_maturity", "postharvest1_yield",           "postharvest1_tubersize",        "postharvest1_tuberappearance" ,
                         "postharvest1_taste" ,"postharvest2_dormancy"    ,     "postharvest2_taste",           
                         "postharvest3_dormancy","postharvest2_marketableatstor")  

#put tarit in the list one by one 
ttt<- partition_node3_test_df_t[,c(1:3)]
ttt_tt<- partition_node3_test_df_t[]
ttt_tt[,c(grep(trait_names_node3[1], names(partition_node3_test_df_t)))]

rlist <- list()
for(i in 1:9){
  rlist[[i]] <- cbind(ttt, ttt_tt[,c(grep(trait_names_node3[i], names(partition_node3_test_df_t)))])}
rlist

ttt_tt[,c(grep(trait_names_node3[8], names(partition_node3_test_df_t)))]

str(ttt)
str(rlist)

# Prepare list of rankings for different traits
R <- list()
for(i in seq_along(trait_names_node3)){
  
  # select the item names and rankings for the trait in the iteration i
  # put in the matrix
  R[[i]] <- rankTricot(rlist[[i]], items = 1:3, input = 4:5)}

names(R) <- trait_names_node3
R


rN <- kendallTauCorMatrix(R)
r <- rN[[1]]
N <- rN[[2]]
r

#kendall 
write.table(r,file="output_thesis/node3_kendallcorrlation_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


zpv<- c()
ztest_pvalue_matrix <- matrix(NA, nrow=9, ncol=9)
#Fill the matrix
for(i in 1:9){
  for (j in 1:9){ #Fills lower triangle only, to avoid calculating the same value twice
    
    au<-r[j,i]/(sqrt(2*(2 *N[[i]] +5))/sqrt(9*N[[i]]*(N[[i]]-1)))
    zpv<-au
    keep_z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
    ztest_pvalue_matrix[j,i] <- keep_z_value
  }}

ztest_pvalue_matrix

keep_ztest_pvalue_matrix <- as.data.frame(ztest_pvalue_matrix)
row.names(keep_ztest_pvalue_matrix) <- trait_names_node3
colnames(keep_ztest_pvalue_matrix) <- trait_names_node3
keep_ztest_pvalue_matrix

#Kandell_pvalue
write.table(keep_ztest_pvalue_matrix,file="output_thesis/node3_kendall_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


# Test with the full matrix, just to see if it works
pr <- cor2pcor(r)

#calculate partiall correlation of Kendall tau
pr

# Simple way to work with these values is to convert to Pearson's r
r_pearson <- sin(3.141592654*r*.5)
r_pearson


#pearson
write.table(r_pearson,file="output_thesis/node3_Pearson_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

#calculate pearson correlation P-value 
ttv<- list()
ttest_pvalue_matrix <- matrix(NA, nrow=9, ncol=9)

#Fill the matrix
for(i in 1:9){
  for (j in 1:9){ #Fills lower triangle only, to avoid calculating the same value twice
    
    keep_tt<- r_pearson[j,i] *sqrt((N[[i]]-2)/ (1-(r_pearson[j,i]^2)))
    ttv <- keep_tt
    keep_tt_value<- 2 * (pt(-abs(ttv), N[[i]]-2))
    ttest_pvalue_matrix[j,i] <- keep_tt_value}}

ttest_pvalue_matrix

keep_ttest_pvalue_matrix <- as.data.frame(ttest_pvalue_matrix)
row.names(keep_ttest_pvalue_matrix) <- trait_names_node3
colnames(keep_ttest_pvalue_matrix) <- trait_names_node3
keep_ttest_pvalue_matrix

#pearson P value 
write.table(keep_ttest_pvalue_matrix,file="output_thesis/node3_pear_corrlation_t-test_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


pcor_pearson <- cor2pcor(r_pearson[])
row.names(pcor_pearson) <- trait_names_node3
colnames(pcor_pearson) <- trait_names_node3
pcor_pearson

#partial correlation
write.table(pcor_pearson,file="output_thesis/node3_partial_corrlation_alltrait_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


#calculate partial correlation P-value
degree_f<-n2kappa(N[[i]]) 
pr_val0<-cor0.test(pcor_pearson, degree_f)
pr_val0[]

pr_val0<-as.data.frame(pr_val0[])
row.names(pr_val0) <- trait_names_node3
colnames(pr_val0) <- trait_names_node3
pr_val0

#partial p value
write.table(pr_val0,file="output_thesis/node3_partial_corrlation_t-test_p_value_new_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

############The end############










