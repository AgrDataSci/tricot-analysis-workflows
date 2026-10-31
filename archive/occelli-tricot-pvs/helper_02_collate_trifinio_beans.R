####################################################
###### Read and clean red beans data
#Kaue de Sousa
#Updated 05Jul2019

library("tidyverse")
library("magrittr")
library("janitor")

cat("## Read raw data. Time:", date(), "\n")
#get the names of climmob projects
input_projects <- "data/temporary"
projects_names <- list.dirs(input_projects)[2:length(list.dirs(input_projects))]


mydata <- NULL

for (i in 1:length(projects_names)){
  
  d <- list.files(projects_names[i], full.names = TRUE)
  
  pack <- d[grepl("paquete",d)]
  pack %<>% 
    read_csv() %>% 
    as_tibble(.name_repair = janitor::make_clean_names) %>% 
    rename(pack = codigo_paquetes)
  
  file <- d[!grepl("paquete",d)]
  file %<>% 
    read_csv() %>% 
    as_tibble(.name_repair = janitor::make_clean_names) %>% 
    rename(pack = codigo_paquetes) 
  
  # merge dataframes by Packages
  file <- merge(file, pack,  by="pack", all.x=T)
  
  file <- file[,c(1, ncol(file)-2,ncol(file)-1,ncol(file),2:(ncol(file)-3))]
  
  head(file)
  
  
  # remove NA's in package info
  file <- file[!is.na(file[,2]) & !is.na(file[,3]) & !is.na(file[,4]), ]

  
  # check consistency in char_9 and char_10
  index <- match("caracteristica_9", names(file))
  
  # no NA's
  keep <- !is.na(file[, index+1]) & !is.na(file[, index+2])
  file <- file[keep, ]
  
  # no equal
  keep <- file[, index+1] != file[, index+2]
  file <- file[keep, ]
  
  # only LETTERS A,B,C
  keep <- file[, index+1]  %in%  LETTERS[1:3]
  file <- file[keep, ]

  keep <- file[, index+2]  %in%  LETTERS[1:3]
  file <- file[keep, ]

  # check char10
  index <- match("caracteristica_10", names(file))
  
  for(j in 1:3){
    
    file[, index+j] <- ifelse(file[, index+j] == "Peor", "Worse",
                              ifelse(file[, index+j] == "Mejor", "Better",
                                     NA))
    
    names(file)[index+j] <- paste0("local_vs_var_",letters[j])
  
  }

  mydata <- rbind(mydata, file)
    
}


names(mydata) <- c("pack",paste0("variety_", letters[1:3]), "id", "name",
  "relative_name","adm3","adm1","phone","gender","age",
  "elevation","lon","lat","planting_date","harverst_date",
  "irrigation","fertilization","slope","soil_quality","rain",
  "rain_how","heat",
  paste0(rep(c("char_","best_","worst_"), times = 9), rep(1:9, each = 3)),
  "char_10","local_vs_var_a","local_vs_var_b","local_vs_var_c")

head(mydata)




mydata$irrigation <- ifelse(mydata$irrigation == "No", "No",
                             ifelse(mydata$irrigation == "Si", "Yes", NA))




mydata$fertilization <- ifelse(mydata$fertilization == "No", "No",
                            ifelse(mydata$fertilization == "Si", "Yes", NA))


summary(as.factor(mydata$slope))
mydata$slope <- ifelse(mydata$slope == "Alta", "high",
                       ifelse(mydata$slope == "Media", "medium",
                              "plain"))


out <- !names(mydata) %in% c("soil_quality", "rain", "rain_how", "heat")

mydata <- mydata[,out]

mydata$char_1 <- "vigor"

mydata$char_2 <- "size"

mydata$char_3 <- "pest_resistance"

mydata$char_4 <- "disease_resistance"

mydata$char_5 <- "drought_resistance"

mydata$char_6 <- "yield"

mydata$char_7 <- "market_value"

mydata$char_8 <- "taste"

mydata$char_9 <- "overall"

mydata$char_10 <- "overall_vs_local"


write_csv(mydata, "data/commonbean.csv")

        