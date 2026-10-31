############################################
#Second, check correlation between overall preference and other traits.  
# run packages
library("PlackettLuce")
library("gosset")
library("ClimMobTools")
library("ggplot2")
library("tibble")
#source("helper_01_functions.R")
library("reshape2")
library("heatmap3")
library("corpcor")
library("GeneNet")


#read the data
dat <- read.csv("data/potato-tricot-data-with-covariate_for_manuscript_QS.csv")


#read marketablility after store at ph2 node data
partition_nod1<- read.csv("data/marketafterstore_ph2_node1.csv")
partition_nod2<- read.csv("data/marketafterstore_ph2_node2.csv")
partition_nod3<- read.csv("data/marketafterstore_ph2_node3.csv")


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

#Function that returns the names of the ranking (positive and negative) traits
getRankingTraitNames <- function(x, strip=TRUE){
  
  r <- unlist(colnames(x))
  r <- r[grepl("_pos", r) | grepl("_neg", r)]
  if(strip) r <- unlist(unique(strsplit(r, c("_pos", "_neg"))))
  return(r)
  
}

getRankingTraitNames(dat)

#####################################

#check data structure
str(dat)
head(dat)
tail(dat)
summary(dat)
table_id <- table(dat$id)
duplicated_ID <- names(table_id[table_id>1]) #find duplicate!! 
dat <- dat[!(dat$id %in% duplicated_ID),]
which(table(dat$id) > 1) #check data structure again

#####################################################################################################
#######################################based on scenario2############################################
#######################################Quality scoring base##########################################

# Remove 23 records exceeding QS of 99, due to low data quality.
QS <- which(dat$Total_sum_QS_point.with.BW > 98)
dat <- dat[-c(QS),] #437 farmers data left in the dataset

#Set non-observed to NA
dat[dat == "Not observed"] <- NA

#########################################################################
# Check how many record left in each trait 
tnames_posneg <- getRankingTraitNames(dat, strip=FALSE)
tnames <- getRankingTraitNames(dat, strip=TRUE)
records <- vector(length=length(tnames))
names(records) <- tnames
for(i in 1:length(tnames)){
  
  records[i] <- sum(complete.cases(dat[,grepl(tnames[i], colnames(dat))]))
  
}

#Inspect results
as.data.frame(records)

# Prepare list of rankings for different traits
R <- list()

for(i in seq_along(tnames)){
  
  # select the item names and rankings for the trait in the iteration i
  d_i <- cbind(dat[, c("package_item_A", "package_item_B", "package_item_C")], 
               dat[, grepl(tnames[i], colnames(dat))])

  # put in the matrix
  R[[i]] <- rankTricot(d_i, items = 1:3, input = 4:5)

}

names(R) <- tnames

rN <- kendallTauCorMatrix(R)
r <- rN[[1]]
N <- rN[[2]]
N[1]

#kendall 
write.table(r,file="output_thesis/alltrait_matrix_kendallcorrlation_alltrait.csv",  
            append=FALSE, 
            sep= ",", 
            row.names = FALSE, col.names=TRUE)


#calculate kendall p-value 
#au<-r[1,1]/(sqrt(2*(2 *(N[1]) +5))/sqrt(9*N[1]*(N[1]-1)))
#2*pnorm(abs(au), lower.tail=FALSE)

zpv<- c()
ztest_pvalue_matrix <- matrix(NA, nrow=20, ncol=20)
#Fill the matrix
for(i in 1:20){
  for (j in 1:20){ #Fills lower triangle only, to avoid calculating the same value twice
    
    au<-r[j,i]/(sqrt(2*(2 *(N[i]) +5))/sqrt(9*N[i]*(N[i]-1)))
    zpv<-au
    keep_z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
    ztest_pvalue_matrix[j,i] <- keep_z_value
    }}

ztest_pvalue_matrix

keep_ztest_pvalue_matrix <- as.data.frame(ztest_pvalue_matrix)
row.names(keep_ztest_pvalue_matrix) <- getRankingTraitNames(dat)
colnames(keep_ztest_pvalue_matrix) <- getRankingTraitNames(dat)
keep_ztest_pvalue_matrix

#Kandell_pvalue
write.table(keep_ztest_pvalue_matrix,file="output_thesis/alltrait_matrix_kendallcorrlation_p-test_p_value.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)




# Test with the full matrix, just to see if it works
pr <- cor2pcor(r)
pr
# Simple way to work with these values is to convert to Pearson's r
r_pearson <- sin(3.141592654*r*.5)
r_pearson

#pearson
write.table(r_pearson,file="output_thesis/alltrait_matrix_Pearson_corrlation_alltrait.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

#calculate pearson correlation P-value 
ttv<- list()
ttest_pvalue_matrix <- matrix(NA, nrow=20, ncol=20)
#Fill the matrix
for(i in 1:20){
  for (j in 1:20){ #Fills lower triangle only, to avoid calculating the same value twice
    
    keep_tt<- r_pearson[j,i] *sqrt((51.29616-2)/ (1-(r_pearson[j,i]^2)))
    ttv <- keep_tt
    keep_tt_value<- 2 * (pt(-abs(ttv), 51.29616-2))
    ttest_pvalue_matrix[j,i] <- keep_tt_value}}
    
ttest_pvalue_matrix


keep_ttest_pvalue_matrix <- as.data.frame(ttest_pvalue_matrix)
row.names(keep_ttest_pvalue_matrix) <- getRankingTraitNames(dat)
colnames(keep_ttest_pvalue_matrix) <- getRankingTraitNames(dat)
keep_ttest_pvalue_matrix

#pearson P value 
write.table(keep_ttest_pvalue_matrix,file="output_thesis/alltrait_matrix_pearcorrlation_t-test_p_value.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


# Is overall preference mainly affected by marketability, taste, or yield?
s1<- c("postharvest1_maturity","postharvest1_yield","postharvest1_tubersize",
       "postharvest1_tuberappearance" ,"postharvest1_marketableatharv","postharvest1_taste" , 
       "postharvest2_marketableatstor", "postharvest2_dormancy", "postharvest2_taste")


pcor_pearson <- cor2pcor(r_pearson[])

#pcor_pearson <- cor2pcor(r_pearson[])


r1 <- r[s1, s1]
r1
pcor_pearson

#partial correlation
write.table(pcor_pearson,file="output_thesis/alltrait_matrix_partial_corrlation_alltrait.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#calculate partial correlation P-value 
n2kappa(51.29616)
pr_val0<-cor0.test(pcor_pearson[], 50.29616)
pr_val0[]

pr_val0<-as.data.frame(pr_val0[])
row.names(pr_val0) <- getRankingTraitNames(dat)
colnames(pr_val0) <- getRankingTraitNames(dat)
pr_val0

#partial p value
write.table(pr_val0,file="output_thesis/alltrait_matrix_partial_corrlation_t-test_p_value.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



pr_val0_tt<-as.data.frame(pr_val0[s1, s1])
pr_val0_tt
#####################################
#node1 
#Set non-observed to NA
partition_nod1[partition_nod1 == "Not observed"] <- NA

#because there is no data tuber quality. So, they turn oo logic...it must change to character
partition_nod1$postharvest3_tuberquality_pos <-as.character(partition_nod1$postharvest3_tuberquality_pos)
partition_nod1$postharvest3_tuberquality_neg <- as.character(partition_nod1$postharvest3_tuberquality_pos)

#########################################################################
# Check how many record left in each trait 
tnames_posneg <- getRankingTraitNames(partition_nod1, strip=FALSE)
tnames <- getRankingTraitNames(partition_nod1, strip=TRUE)
records <- vector(length=length(tnames))
names(records) <- tnames
for(i in 1:length(tnames)){
  
  records[i] <- sum(complete.cases(partition_nod1[,grepl(tnames[i], colnames(partition_nod1))]))
  
}

#Inspect results
as.data.frame(records)

# Prepare list of rankings for different traits
R <- list()

for(i in seq_along(tnames)){
  
  # select the item names and rankings for the trait in the iteration i
  d_i <- cbind(partition_nod1[, c("package_item_A", "package_item_B", "package_item_C")], partition_nod1[, grepl(tnames[i], colnames(partition_nod1))])
  
  # put in the matrix
  R[[i]] <- rankTricot(d_i, items = 1:3, input = 4:5)
  
}


names(R) <- tnames

rN <- kendallTauCorMatrix(R)
r <- rN[[1]]
N <- rN[[2]]

#kendall 
write.table(r,file="output_thesis/alltrait_matrix_kendallcorrlation_node1.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#calculate kendall p-value 

zpv<- c()
ztest_pvalue_matrix <- matrix(NA, nrow=20, ncol=20)
#Fill the matrix
for(i in 1:20){
  for (j in 1:20){ #Fills lower triangle only, to avoid calculating the same value twice
    
    au<-r[j,i]/(sqrt(2*(2 *(N[i]) +5))/sqrt(9*N[i]*(N[i]-1)))
    zpv<-au
    keep_z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
    ztest_pvalue_matrix[j,i] <- keep_z_value
  }}

ztest_pvalue_matrix

keep_ztest_pvalue_matrix <- as.data.frame(ztest_pvalue_matrix)
row.names(keep_ztest_pvalue_matrix) <- getRankingTraitNames(dat)
colnames(keep_ztest_pvalue_matrix) <- getRankingTraitNames(dat)
keep_ztest_pvalue_matrix

#Kandell_pvalue
write.table(keep_ztest_pvalue_matrix,file="output_thesis/alltrait_matrix_kendallcorrlation_p-test_p_value_nod1.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



# Test with the full matrix, just to see if it works
pr <- cor2pcor(r)

# Simple way to work with these values is to convert to Pearson's r
r_pearson <- sin(3.141592654*r*.5)

#pearson
write.table(r_pearson,file="output_thesis/alltrait_matrix_Pearson_corrlation_node1.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#calculate pearson correlation P-value 
ttv<- list()
ttest_pvalue_matrix <- matrix(NA, nrow=20, ncol=20)
#Fill the matrix
for(i in 1:20){
  for (j in 1:20){ #Fills lower triangle only, to avoid calculating the same value twice
    
    keep_tt<- r_pearson[j,i] *sqrt((51.29616-2)/ (1-(r_pearson[j,i]^2)))
    ttv <- keep_tt
    keep_tt_value<- 2 * (pt(-abs(ttv), 51.29616-2))
    ttest_pvalue_matrix[j,i] <- keep_tt_value}}

ttest_pvalue_matrix


keep_ttest_pvalue_matrix <- as.data.frame(ttest_pvalue_matrix)
row.names(keep_ttest_pvalue_matrix) <- getRankingTraitNames(dat)
colnames(keep_ttest_pvalue_matrix) <- getRankingTraitNames(dat)
keep_ttest_pvalue_matrix

##pearson P value 
write.table(keep_ttest_pvalue_matrix,file="output_thesis/alltrait_matrix_pearcorrlation_t-test_p_value_node1.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)




# Is overall preference mainly affected by marketability, taste, or yield?
s1<- c("postharvest1_maturity","postharvest1_yield","postharvest1_tubersize",
       "postharvest1_tuberappearance" ,"postharvest1_marketableatharv","postharvest1_taste" , 
       "postharvest2_marketableatstor", "postharvest2_dormancy", "postharvest2_taste")


pcor_pearson <- cor2pcor(r_pearson[s1, s1])

r1 <- r[s1, s1]
r1
pcor_pearson1 <- pcor_pearson[]


#partial correlation
write.table(pcor_pearson1,file="output_thesis/alltrait_matrix_partial_corrlation_node1.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


n2kappa(19.48025)
pr_val1<-cor0.test(pcor_pearson[], 18.48025 )
pr_val1
pr_val1<-as.data.frame(pr_val1[])

row.names(pr_val1) <- getRankingTraitNames(dat)
colnames(pr_val1) <- getRankingTraitNames(dat)
pr_val1

#partial p value
write.table(pr_val1,file="output_thesis/alltrait_matrix_partial_corrlation_t-test_p_value_node1.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


#####################################
#node2 
#Set non-observed to NA
partition_nod2[partition_nod2 == "Not observed"] <- NA


#because there is no data tuber quality. So, they turn oo logic...it must change to character
partition_nod2$postharvest3_tuberquality_pos <-as.character(partition_nod2$postharvest3_tuberquality_pos)
partition_nod2$postharvest3_tuberquality_neg <- as.character(partition_nod2$postharvest3_tuberquality_pos)
#########################################################################
# Check how many record left in each trait 
tnames_posneg <- getRankingTraitNames(partition_nod2, strip=FALSE)
tnames <- getRankingTraitNames(partition_nod2, strip=TRUE)
records <- vector(length=length(tnames))
names(records) <- tnames
for(i in 1:length(tnames)){
  
  records[i] <- sum(complete.cases(partition_nod2[,grepl(tnames[i], colnames(partition_nod2))]))
  
}

#Inspect results
as.data.frame(records)

# Prepare list of rankings for different traits
R <- list()

for(i in seq_along(tnames)){
  
  # select the item names and rankings for the trait in the iteration i
  d_i <- cbind(partition_nod2[, c("package_item_A", "package_item_B", "package_item_C")], partition_nod2[, grepl(tnames[i], colnames(partition_nod2))])
  
  # put in the matrix
  R[[i]] <- rankTricot(d_i, items = 1:3, input = 4:5)
  
}

names(R) <- tnames

rN <- kendallTauCorMatrix(R)
r <- rN[[1]]
N <- rN[[2]]


#kendall 
write.table(r,file="output_thesis/alltrait_matrix_kendallcorrlation_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#calculate kendall p-value 

zpv<- c()
ztest_pvalue_matrix <- matrix(NA, nrow=20, ncol=20)
#Fill the matrix
for(i in 1:20){
  for (j in 1:20){ #Fills lower triangle only, to avoid calculating the same value twice
    
    au<-r[j,i]/(sqrt(2*(2 *(N[i]) +5))/sqrt(9*N[i]*(N[i]-1)))
    zpv<-au
    keep_z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
    ztest_pvalue_matrix[j,i] <- keep_z_value
  }}

ztest_pvalue_matrix

keep_ztest_pvalue_matrix <- as.data.frame(ztest_pvalue_matrix)
row.names(keep_ztest_pvalue_matrix) <- getRankingTraitNames(dat)
colnames(keep_ztest_pvalue_matrix) <- getRankingTraitNames(dat)
keep_ztest_pvalue_matrix


#Kandell_pvalue
write.table(keep_ztest_pvalue_matrix,file="output_thesis/alltrait_matrix_kendallcorrlation_p-test_p_value_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)




# Test with the full matrix, just to see if it works
pr <- cor2pcor(r)

# Simple way to work with these values is to convert to Pearson's r
r_pearson <- sin(3.141592654*r*.5)

#pearson
write.table(r_pearson,file="output_thesis/alltrait_matrix_Pearson_corrlation_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#calculate pearson correlation P-value 
ttv<- list()
ttest_pvalue_matrix <- matrix(NA, nrow=20, ncol=20)
#Fill the matrix
for(i in 1:20){
  for (j in 1:20){ #Fills lower triangle only, to avoid calculating the same value twice
    
    keep_tt<- r_pearson[j,i] *sqrt((51.29616-2)/ (1-(r_pearson[j,i]^2)))
    ttv <- keep_tt
    keep_tt_value<- 2 * (pt(-abs(ttv), 51.29616-2))
    ttest_pvalue_matrix[j,i] <- keep_tt_value}}

ttest_pvalue_matrix


keep_ttest_pvalue_matrix <- as.data.frame(ttest_pvalue_matrix)
row.names(keep_ttest_pvalue_matrix) <- getRankingTraitNames(dat)
colnames(keep_ttest_pvalue_matrix) <- getRankingTraitNames(dat)
keep_ttest_pvalue_matrix


#pearson P value 
write.table(keep_ttest_pvalue_matrix,file="output_thesis/alltrait_matrix_pearcorrlation_t-test_p_value_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



# Is overall preference mainly affected by marketability, taste, or yield?
s1<- c("postharvest1_maturity","postharvest1_yield","postharvest1_tubersize",
       "postharvest1_tuberappearance" ,"postharvest1_marketableatharv","postharvest1_taste" , 
       "postharvest2_marketableatstor", "postharvest2_dormancy", "postharvest2_taste")


pcor_pearson <- cor2pcor(r_pearson[])
pcor_pearson

r1 <- r[s1, s1]
r1
pcor_pearson2 <- pcor_pearson[]

#partial correlation
write.table(pcor_pearson2,file="output_thesis/alltrait_matrix_partial_corrlation_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



n2kappa(17.82772)
pr_val2<-cor0.test(pcor_pearson[], 16.82772)
pr_val2


pr_val2<-as.data.frame(pr_val2[])
pr_val2
row.names(pr_val2) <- getRankingTraitNames(dat)
colnames(pr_val2) <- getRankingTraitNames(dat)
pr_val2

#partial p value
write.table(pr_val2,file="output_thesis/alltrait_matrix_partial_corrlation_t-test_p_value_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#####################################
#node3 
#Set non-observed to NA
partition_nod3[partition_nod3 == "Not observed"] <- NA


#because there is no data tuber quality. So, they turn oo logic...it must change to character
partition_nod3$postharvest3_tuberquality_pos <-as.character(partition_nod3$postharvest3_tuberquality_pos)
partition_nod3$postharvest3_tuberquality_neg <- as.character(partition_nod3$postharvest3_tuberquality_pos)

#########################################################################
# Check how many record left in each trait 
tnames_posneg <- getRankingTraitNames(partition_nod3, strip=FALSE)
tnames <- getRankingTraitNames(partition_nod3, strip=TRUE)
records <- vector(length=length(tnames))
names(records) <- tnames
for(i in 1:length(tnames)){
  
  records[i] <- sum(complete.cases(partition_nod3[,grepl(tnames[i], colnames(partition_nod3))]))
  
}

#Inspect results
as.data.frame(records)

# Prepare list of rankings for different traits
R <- list()

for(i in seq_along(tnames)){
  
  # select the item names and rankings for the trait in the iteration i
  d_i <- cbind(partition_nod3[, c("package_item_A", "package_item_B", "package_item_C")], partition_nod3[, grepl(tnames[i], colnames(partition_nod3))])
  
  # put in the matrix
  R[[i]] <- rankTricot(d_i, items = 1:3, input = 4:5)
  
}

names(R) <- tnames

rN <- kendallTauCorMatrix(R)
r <- rN[[1]]
N <- rN[[2]]


#kendall 
write.table(r,file="output_thesis/alltrait_matrix_kendallcorrlation_node3.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#calculate kendall p-value 

zpv<- c()
ztest_pvalue_matrix <- matrix(NA, nrow=20, ncol=20)
#Fill the matrix
for(i in 1:20){
  for (j in 1:20){ #Fills lower triangle only, to avoid calculating the same value twice
    
    au<-r[j,i]/(sqrt(2*(2 *(N[i]) +5))/sqrt(9*N[i]*(N[i]-1)))
    zpv<-au
    keep_z_value<- 2*pnorm(abs(zpv), lower.tail=FALSE)
    ztest_pvalue_matrix[j,i] <- keep_z_value
  }}

ztest_pvalue_matrix

keep_ztest_pvalue_matrix <- as.data.frame(ztest_pvalue_matrix)
row.names(keep_ztest_pvalue_matrix) <- getRankingTraitNames(dat)
colnames(keep_ztest_pvalue_matrix) <- getRankingTraitNames(dat)
keep_ztest_pvalue_matrix

#Kandell_pvalue
write.table(keep_ztest_pvalue_matrix,file="output_thesis/alltrait_matrix_kendallcorrlation_p-test_p_value_node3.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



# Test with the full matrix, just to see if it works
pr <- cor2pcor(r)

# Simple way to work with these values is to convert to Pearson's r
r_pearson <- sin(3.141592654*r*.5)


#pearson
write.table(r_pearson,file="output_thesis/alltrait_matrix_Pearson_corrlation_node3.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#calculate pearson correlation P-value 
ttv<- list()
ttest_pvalue_matrix <- matrix(NA, nrow=20, ncol=20)
#Fill the matrix
for(i in 1:20){
  for (j in 1:20){ #Fills lower triangle only, to avoid calculating the same value twice
    
    keep_tt<- r_pearson[j,i] *sqrt((51.29616-2)/ (1-(r_pearson[j,i]^2)))
    ttv <- keep_tt
    keep_tt_value<- 2 * (pt(-abs(ttv), 51.29616-2))
    ttest_pvalue_matrix[j,i] <- keep_tt_value}}

ttest_pvalue_matrix


keep_ttest_pvalue_matrix <- as.data.frame(ttest_pvalue_matrix)
row.names(keep_ttest_pvalue_matrix) <- getRankingTraitNames(dat)
colnames(keep_ttest_pvalue_matrix) <- getRankingTraitNames(dat)
keep_ttest_pvalue_matrix

#pearson P value 
write.table(keep_ttest_pvalue_matrix,file="output_thesis/alltrait_matrix_pearcorrlation_t-test_p_value_node3.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



# Is overall preference mainly affected by marketability, taste, or yield?
s1<- c("postharvest1_maturity","postharvest1_yield","postharvest1_tubersize",
       "postharvest1_tuberappearance" ,"postharvest1_marketableatharv","postharvest1_taste" , 
       "postharvest2_marketableatstor", "postharvest2_dormancy", "postharvest2_taste")


pcor_pearson <- cor2pcor(r_pearson[])

#partial correlation
write.table(pcor_pearson,file="output_thesis/alltrait_matrix_partial_corrlation_node3.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



r1 <- r[s1, s1]
r1
pcor_pearson3 <- pcor_pearson[]

n2kappa(26.07831)
pr_val3<-cor0.test(pcor_pearson[], 25.07831)

pr_val3<-as.data.frame(pr_val3[])
row.names(pr_val3) <- getRankingTraitNames(dat)
colnames(pr_val3) <- getRankingTraitNames(dat)

pr_val3

#partial p value
write.table(pr_val3,file="output_thesis/alltrait_matrix_partial_corrlation_t-test_p_value_node3.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
