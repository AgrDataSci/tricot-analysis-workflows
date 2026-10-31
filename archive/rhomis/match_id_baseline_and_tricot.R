

#read ClimMob files #
#use these files to get the farmers' names  
file1 <- read.csv("./input/guatemala_negro_2015.csv", na.strings = c("", "NA"))
file1[1:ncol(file1)] <- lapply(file1[1:ncol(file1)], as.character)
file2 <- read.csv("./input/honduras_salvador_rojo_2015.csv", na.strings = c("", "NA"))
file2[1:ncol(file2)] <- lapply(file2[1:ncol(file2)], as.character)
#bind files
mydata <- rbind(file1[,c(1:12,46:48)], file2[,c(1:12,46:48)])
dim(mydata)
head(mydata,10)
#remove rows with missing answers for overall performance
mydata <- mydata[!is.na(mydata$mejor_variedad_9), ]
mydata <- mydata[!is.na(mydata$peor_variedad_9), ]
head(mydata)
mydata[is.na(mydata)] <- "missing"
names(mydata)[1] <- "observers"
#read cleaned files of climmob projects
#use this to get administrative info
file3 <- read.csv("./input/climmob_projects_cleaned.csv", na.strings = c("", "NA"))
file3 <- file3[file3$ADM0_NAME!="Nicaragua",]
dim(file3)
names(file3)
names(mydata)
mydata <- merge(file3[,c(1:10)], mydata[,c(1:3)], by="observers", all.x=T)
mydata[1:ncol(mydata)] <- lapply(mydata[1:ncol(mydata)], as.character)
#read baseline
#use this to get the ids 
baseline <- read.csv("./output/Prueba3_baseline12may2017.csv")
baseline <- baseline[baseline$TREAT=="CCI", c("id_household","TREAT","TREAT_state","ADM0_NAME","ADM1_NAME","ADM2_NAME","nombre_jh","nombre_entrevistado")]
baseline[1:ncol(baseline)] <- lapply(baseline[1:ncol(baseline)], as.character)
dim(baseline)
#baseline$observers <- NA
ids <- NULL
for(j in unique(mydata$ADM2_NAME)){
  df <- baseline[baseline$ADM2_NAME==j, ]
  for(i in rownames(mydata)[mydata$ADM2_NAME==j]){
    index_obs <- which( agrepl(mydata[i,11], df[ , "nombre_jh"]), T)
    #if the function don't find a match try with other variables

        if(length(index_obs)==0) index_obs <- which( agrepl(mydata[i,12], df[ ,"nombre_jh"]), T)
    if(length(index_obs)==0) index_obs <- which( agrepl(mydata[i,11], df[ ,"nombre_entrevistado"]), T)
    if(length(index_obs)==0) index_obs <- which( agrepl(mydata[i,12], df[ ,"nombre_entrevistado"]), T)
    #get the id from ClimMob using the index captured before
    if(length(index_obs)!=0) ids <- rbind(ids, cbind(df[ index_obs , "id_household" ], mydata[i,1]))
  }
}
str(ids)
ids <- as.data.frame(ids)
names(ids) <- c("id_household","observers")
ids[1:2] <- lapply(ids[1:2], as.character)
baseline <- merge(baseline, ids, by="id_household",all.x=T)

summary(!is.na(baseline$observers))
length(unique(baseline$observers))
#some ids are duplicated
head(baseline)
baseline$observers <- ifelse(duplicated(baseline$observers), NA, baseline$observers)

#write csv file
write.csv(baseline, "./processing/baseline_with_partial_ids_climmob.csv", row.names = F)
write.csv(mydata, "./processing/ids_climmob.csv", row.names = F)



