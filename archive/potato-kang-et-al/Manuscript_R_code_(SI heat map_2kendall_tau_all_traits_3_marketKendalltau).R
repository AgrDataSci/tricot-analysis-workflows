################################################################################
# run packages
library("PlackettLuce")
library("gosset")
library("ClimMobTools")
library("ggplot2")
library("corpcor")
library("GeneNet")
library('DescTools')
library("dplyr")
#read the data
# dt <- read.csv("data/potato-tricot-data-with-covariate_for_manuscript_QS.csv")
dt <- read.csv("data/potato-tricot-data-with-covariate_for_manuscript_12_april.csv")

#####################################################################################################
#######################################based on scenario2############################################
#######################################Quality scoring base##########################################

# Remove 23 records exceeding QS of 99, due to low dta quality.
QS <- which(dt$Total_sum_QS_point.with.BW > 98)
dt <- dt[-c(QS),] #437 farmers dta left in the dtaset

#Set non-observed to NA
dt[dt == "Not observed"] <- NA

#########################################################################


#Check the names of the ranking (positive and negative) traits
trait_list = getTraitList(dt, 
                          pattern = c("_pos", "_neg"))
tricot_trait<- unlist(lapply(trait_list, function(x) x$trait ))
tricot_trait

################################################################################
#Check how many farmer's left
row_number(dt[,1]) #434 number of farmers

#Check how many record left in each trait 
data.frame(trait = tricot_trait,
           records = unlist(lapply(trait_list, function(x) sum(x$keep))))

#Check tricot packages A B C
pack_index = c("package_item_A", "package_item_B", "package_item_C")

# Prepare list of rankings for different traits
R <- lapply(trait_list, function(x){
  rank_tricot(data = dt,
              items = pack_index,
              input = x$string, 
              validate.rankings = TRUE)
})

names(R) <- tricot_trait


#Calculate triats' kendell tau correlations ship
#Prepare the correlation matrix to be filled with values
r <- matrix(NA, nrow=20, ncol=20)

#Prepare the effective N matrix
N <- matrix(NA, nrow=20, ncol=20)

for(i in 1:20){
  for (j in 1:20){ #Fills lower triangle only, to avoid calculating the same value twice
    
    rNij <- kendallTau(R[[i]], R[[j]], average =TRUE) #TODO deal with NA values
    r[i,j] <- as.numeric(rNij[1])
    N[i,j] <- as.numeric(rNij[2])}}

keep_r <- r
r
N

# change number to trait name
rownames(r) <-tricot_trait
colnames(r) <- tricot_trait
rownames(N) <-tricot_trait
colnames(N) <- tricot_trait
# r is all correlation of traits 
# n is effective size 

# calculate z-value using n effective

z <- matrix(NA, nrow=20, ncol=20)
p <- matrix(NA, nrow=20, ncol=20)
for(i in 1:20){
  for( j in 1:20){ z[j,i] <- (3 * r[j,i] * sqrt(N[j,i]*(N[j,i]-1))) / sqrt(2 * (2*N[j,i] + 5))
  p[j,i] = pnorm(z[j,i], lower.tail = F)
  p[j,i] = as.numeric(formatC(p[j,i], format = "e", digits = 2))
  }}
keep_p<- p

rownames(p) <-tricot_trait
colnames(p) <- rep("p_value", times = 20)
p <- as.data.frame(p)

#merge the process....
kt_result <- list()
for(i in 1:20){kt_result[[i]]<- cbind(r[,i], p[,i])
colnames(kt_result[[i]]) <- c(tricot_trait[i], "p_value")}
kt_result<- do.call(cbind, kt_result)
kt_result<- as.data.frame(kt_result)

for(i in 1:40){kt_result[,i] <- round(as.numeric(kt_result[,i], nsmall = 3), 3) }
kt_result

write.table(kt_result,file="data/Kendall_correlation_13_march.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
###############################################################################
##what is different between final appreciation
kt_result[,35:40]
write.table(kt_result[,35:40],file="data/Kendall_correlation_final_appreciation_28_march.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


###############################################################################
#. The heatmap presents Kendall rank correlations between all traits 
library("gplots")




fixed_traits <- c("Plant vigour (VG1)", 
                  "BW resistance (VG1)", 
                  "Disease & insect (VG1)",
                  "BW resistance (VG2)",
                  "Disease & insect resistance (VG2)",
                  "Maturity (PH1)",
                  "Yield (PH1)",
                  "Tuber size (PH1)",
                  "Tuber appearance (PH1)",
                  "Marketability after harvest (PH1)",
                  "Taste (PH1)",
                  "Marketability after store (PH1)",
                  "Marketability after store (PH2)",
                  "Dormancy (PH2)",
                  "Taste (PH2)",
                  "Dormancy (PH3)",
                  "Tuber quality (PH3)",
                  "Overall performance",
                  "Willing to plants in the future",
                  "Overall preference")


rownames(keep_p) <-fixed_traits
colnames(keep_p) <-fixed_traits

rownames(keep_r) <-fixed_traits
colnames(keep_r) <-fixed_traits

keep_p
keep_r
# 
# 
# heatmap.2(keep_r,dendrogram="both",  hclustfun = function(x) hclust(x, method="average"), 
#           distfun = function(x) as.dist(1 - x), 
#           key=TRUE, keysize=1, key.title = "Kendall τ",
#           col=colorRampPalette(c("red", "white", "blue"))(50), trace="none", labRow=fixed_traits, labCol=fixed_traits, margins=c(14,14), cexRow=1.5, cexCol=1.5)
# 
#  heatmap.2(keep_r,dendrogram="both",  hclustfun = function(x) hclust(x, method="single"), 
#            distfun = function(x) as.dist(1 - x), 
#            key=TRUE, keysize=1, key.title = "Kendall τ",
#            col=colorRampPalette(c("red", "white", "blue"))(50), trace="none", labRow=fixed_traits, labCol=fixed_traits, margins=c(14,14), cexRow=1.5, cexCol=1.5)
#  
#  
#  
 heatmap.2(keep_r,dendrogram="both",  hclustfun = function(x) hclust(x, method="complete"), 
           distfun = function(x) as.dist(1 - x), 
           key=TRUE, keysize=1, key.title = "Kendall τ",
           col=colorRampPalette(c("red", "white", "blue"))(50), trace="none", labRow=fixed_traits, labCol=fixed_traits, margins=c(14,14), cexRow=1.1, cexCol=1.1)






# save as png file
png(filename = "output_thesis/heatmap_full_dial_about_kendall_tau_and_dendorgram_threeoassessment_ver3_new.png", width = 50, height = 50, units = "cm", res = 400)
heatmap.2(keep_r,dendrogram="both",  hclustfun = function(x) hclust(x, method="complete"), 
          distfun = function(x) as.dist(1 - x), 
          key=TRUE, keysize=0.7, key.title = "Kendall τ",
          col=colorRampPalette(c("red", "white", "blue"))(50), trace="none", labRow=fixed_traits, labCol=fixed_traits, margins=c(31,31), cexRow=2.3, cexCol=2.3)
dev.off()



################################################################################
#lets check postharvest2_marketableatstor                     


#read marketablility after store at ph2 node data
partition_node1<- read.csv("data/marketafterstore_ph2_node1.csv")
partition_node2<- read.csv("data/marketafterstore_ph2_node2.csv")
partition_node3<- read.csv("data/marketafterstore_ph2_node3.csv")

partition_node1[partition_node1 == "Not observed"] <- NA
partition_node2[partition_node2 == "Not observed"] <- NA
partition_node3[partition_node3 == "Not observed"] <- NA


#####################################
###############################node 1
trait_list_nod1 = getTraitList(partition_node1, 
                          pattern = c("_pos", "_neg"))
tricot_trait_nod1<- unlist(lapply(trait_list_nod1, function(x) x$trait ))
tricot_trait_nod1

#check node 1
data.frame(trait = tricot_trait_nod1,
           records = unlist(lapply(trait_list_nod1, function(x) sum(x$keep))))
#postharvest3_overallper([[17]]) is zero. 
trait_list_nod1 = trait_list_nod1[-17]
tricot_trait_nod1<- unlist(lapply(trait_list_nod1, function(x) x$trait ))
tricot_trait_nod1



# Prepare list of rankings for different traits
R_nod1 <- lapply(trait_list_nod1, function(x){
  rank_tricot(data = partition_node1,
              items = pack_index,
              input = x$string, 
              validate.rankings = TRUE)
})

names(R_nod1) <- tricot_trait_nod1



#Calculate triats' kendell tau correlations ship
#Prepare the correlation matrix to be filled with values
r_n1 <- matrix(NA, nrow=19, ncol=19)

#Prepare the effective N matrix
N_n1 <- matrix(NA, nrow=19, ncol=19)

for(i in 1:19){
  for (j in 1:19){ #Fills lower triangle only, to avoid calculating the same value twice
    
    rNij <- kendallTau(R_nod1[[i]], R_nod1[[j]], average =TRUE) #TODO deal with NA values
    r_n1[i,j] <- as.numeric(rNij[1])
    N_n1[i,j] <- as.numeric(rNij[2])}}


r_n1
N_n1

# change number to trait name
rownames(r_n1) <-tricot_trait_nod1
colnames(r_n1) <- tricot_trait_nod1
rownames(N_n1) <-tricot_trait_nod1
colnames(N_n1) <- tricot_trait_nod1
# r is all correlation of traits 
# n is effective size 

# calculate z-value using n effective

z <- matrix(NA, nrow=19, ncol=19)
p <- matrix(NA, nrow=19, ncol=19)
for(i in 1:19){
  for( j in 1:19){ z[j,i] <- (3 * r_n1[j,i] * sqrt(N_n1[j,i]*(N_n1[j,i]-1))) / sqrt(2 * (2*N_n1[j,i] + 5))
  p[j,i] = pnorm(z[j,i], lower.tail = F)
  p[j,i] = formatC(p[j,i], format = "e", digits = 2)
  }}

rownames(p) <-tricot_trait_nod1
colnames(p) <- rep("p_value", times = 19)
p <- as.data.frame(p)

#merge the process....
kt_result_n1 <- list()
for(i in 1:19){kt_result_n1[[i]]<- cbind(r_n1[,i], p[,i])
colnames(kt_result_n1[[i]]) <- c(tricot_trait_nod1[i], "p_value")}
kt_result_n1<- do.call(cbind, kt_result_n1)
kt_result_n1<- as.data.frame(kt_result_n1)

for(i in 1:38){kt_result_n1[,i] <- round(as.numeric(kt_result_n1[,i], nsmall = 3), 3) }
kt_result_n1_result<- kt_result_n1[c(6:9, 11, 14,15,16),c("postharvest2_marketableatstor", "p_value")]
kt_result_n1_result<-cbind(c(rownames(kt_result_n1_result)), kt_result_n1_result)

write.table(kt_result_n1_result,file="data/marketability_ph2_node2.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#####################################
###############################node 2
trait_list_nod2 = getTraitList(partition_node2, 
                               pattern = c("_pos", "_neg"))
tricot_trait_nod2<- unlist(lapply(trait_list_nod2, function(x) x$trait ))
tricot_trait_nod2

data.frame(trait = tricot_trait_nod2,
           records = unlist(lapply(trait_list_nod2, function(x) sum(x$keep))))

#postharvest3_overallper([[17]]) is zero. 
trait_list_nod2 = trait_list_nod2[-17]
tricot_trait_nod2<- unlist(lapply(trait_list_nod2, function(x) x$trait ))
tricot_trait_nod2



# Prepare list of rankings for different traits
R_nod2 <- lapply(trait_list_nod2, function(x){
  rank_tricot(data = partition_node2,
              items = pack_index,
              input = x$string, 
              validate.rankings = TRUE)
})

names(R_nod2) <- tricot_trait_nod2



#Calculate triats' kendell tau correlations ship
#Prepare the correlation matrix to be filled with values
r_n2 <- matrix(NA, nrow=19, ncol=19)

#Prepare the effective N matrix
N_n2 <- matrix(NA, nrow=19, ncol=19)

for(i in 1:19){
  for (j in 1:19){ #Fills lower triangle only, to avoid calculating the same value twice
    
    rNij <- kendallTau(R_nod2[[i]], R_nod2[[j]], average =TRUE) #TODO deal with NA values
    r_n2[i,j] <- as.numeric(rNij[1])
    N_n2[i,j] <- as.numeric(rNij[2])}}


r_n2
N_n2

# change number to trait name
rownames(r_n2) <-tricot_trait_nod2
colnames(r_n2) <- tricot_trait_nod2
rownames(N_n2) <-tricot_trait_nod2
colnames(N_n2) <- tricot_trait_nod2
# r is all correlation of traits 
# n is effective size 

# calculate z-value using n effective

z <- matrix(NA, nrow=19, ncol=19)
p <- matrix(NA, nrow=19, ncol=19)
for(i in 1:19){
  for( j in 1:19){ z[j,i] <- (3 * r_n2[j,i] * sqrt(N_n2[j,i]*(N_n2[j,i]-1))) / sqrt(2 * (2*N_n2[j,i] + 5))
  p[j,i] = pnorm(z[j,i], lower.tail = F)
  p[j,i] = formatC(p[j,i], format = "e", digits = 2)
  }}

rownames(p) <-tricot_trait_nod2
colnames(p) <- rep("p_value", times = 19)
p <- as.data.frame(p)

#merge the process....
kt_result_n2 <- list()
for(i in 1:19){kt_result_n2[[i]]<- cbind(r_n2[,i], p[,i])
colnames(kt_result_n2[[i]]) <- c(tricot_trait_nod2[i], "p_value")}
kt_result_n2<- do.call(cbind, kt_result_n2)
kt_result_n2<- as.data.frame(kt_result_n2)



for(i in 1:38){kt_result_n2[,i] <- round(as.numeric(kt_result_n2[,i], nsmall = 3), 3) }
kt_result_n2[,c("postharvest2_marketableatstor", "p_value")]
kt_result_n2_result<- kt_result_n2[c(6:9, 11, 14,15,16),c("postharvest2_marketableatstor", "p_value")]
kt_result_n2_result

kt_result_n2_result<-cbind(c(rownames(kt_result_n2_result)), kt_result_n2_result)

write.table(kt_result_n2_result,file="data/marketability_ph2_node4.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



#####################################
###############################node 3
trait_list_nod3 = getTraitList(partition_node3, 
                               pattern = c("_pos", "_neg"))
tricot_trait_nod3<- unlist(lapply(trait_list_nod3, function(x) x$trait ))
tricot_trait_nod3

data.frame(trait = tricot_trait_nod3,
           records = unlist(lapply(trait_list_nod3, function(x) sum(x$keep))))

#postharvest3_overallper([[17]]) is zero. 
trait_list_nod3 = trait_list_nod3[-17]
tricot_trait_nod3<- unlist(lapply(trait_list_nod3, function(x) x$trait ))
tricot_trait_nod3



# Prepare list of rankings for different traits
R_nod3 <- lapply(trait_list_nod3, function(x){
  rank_tricot(data = partition_node3,
              items = pack_index,
              input = x$string, 
              validate.rankings = TRUE)
})

names(R_nod3) <- tricot_trait_nod3



#Calculate triats' kendell tau correlations ship
#Prepare the correlation matrix to be filled with values
r_n3 <- matrix(NA, nrow=19, ncol=19)

#Prepare the effective N matrix
N_n3 <- matrix(NA, nrow=19, ncol=19)

for(i in 1:19){
  for (j in 1:19){ #Fills lower triangle only, to avoid calculating the same value twice
    
    rNij <- kendallTau(R_nod3[[i]], R_nod3[[j]], average =TRUE) #TODO deal with NA values
    r_n3[i,j] <- as.numeric(rNij[1])
    N_n3[i,j] <- as.numeric(rNij[2])}}


r_n3
N_n3

# change number to trait name
rownames(r_n3) <-tricot_trait_nod3
colnames(r_n3) <- tricot_trait_nod3
rownames(N_n3) <-tricot_trait_nod3
colnames(N_n3) <- tricot_trait_nod3
# r is all correlation of traits 
# n is effective size 

# calculate z-value using n effective

z <- matrix(NA, nrow=19, ncol=19)
p <- matrix(NA, nrow=19, ncol=19)
for(i in 1:19){
  for( j in 1:19){ z[j,i] <- (3 * r_n3[j,i] * sqrt(N_n3[j,i]*(N_n3[j,i]-1))) / sqrt(2 * (2*N_n3[j,i] + 5))
  p[j,i] = pnorm(z[j,i], lower.tail = F)
  p[j,i] = formatC(p[j,i], format = "e", digits = 2)
  }}

rownames(p) <-tricot_trait_nod3
colnames(p) <- rep("p_value", times = 19)
p <- as.data.frame(p)

#merge the process....
kt_result_n3 <- list()
for(i in 1:19){kt_result_n3[[i]]<- cbind(r_n3[,i], p[,i])
colnames(kt_result_n3[[i]]) <- c(tricot_trait_nod3[i], "p_value")}
kt_result_n3<- do.call(cbind, kt_result_n3)
kt_result_n3<- as.data.frame(kt_result_n3)

for(i in 1:38){kt_result_n3[,i] <- round(as.numeric(kt_result_n3[,i], nsmall = 3), 3) }
kt_result_n3[,c("postharvest2_marketableatstor", "p_value")]

kt_result_n3_result<- kt_result_n3[c(6:9, 11, 14,15,16),c("postharvest2_marketableatstor", "p_value")]
kt_result_n3_result

kt_result_n3_result<-cbind(c(rownames(kt_result_n3_result)), kt_result_n3_result)

write.table(kt_result_n3_result,file="data/marketability_ph2_node5.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)




###############check between node 1 2 & 3##################
kt_result_n1[,c("postharvest2_marketableatstor", "p_value")]
kt_result_n2[,c("postharvest2_marketableatstor", "p_value")]
kt_result_n3[,c("postharvest2_marketableatstor", "p_value")]
N_n1[,13]
N_n2[,13]
N_n3[,13]

#test_traits<- c("postharvest1_maturity",                   
#                "postharvest1_yield",                      
#                "postharvest1_tubersize",                 
#                "postharvest1_tuberappearance",             
#                "postharvest1_taste"           ,           
#                "postharvest2_marketableatstor",           
#                "postharvest2_dormancy",                
#                "postharvest2_taste")            

tricot_trait_nod1


kt_n1<- kt_result_n1[,c(tricot_trait_nod1)]
# Test with the full matrix, just to see if it works

# Simple way to work with these values is to convert to Pearson's r
r_pearson_n1 <- sin(3.141592654*r_n1*.5)

pcor_pearson <- cor2pcor(r_pearson_n1[])
row.names(pcor_pearson) <- test_traits
colnames(pcor_pearson) <- test_traits
pcor_pearson

#calculate partial correlation P-value
degree_f<-n2kappa(N_n1_part)
pr_val0<-cor0.test(pcor_pearson, degree_f)
pr_val0[]

pr_val0<-as.data.frame(pr_val0[])
pr_val0
#########################################
