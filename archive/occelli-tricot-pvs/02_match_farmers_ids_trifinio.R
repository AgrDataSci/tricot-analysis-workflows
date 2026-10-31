# Find matches between citizen science dataset
# and RHOMIS survey in Trifinio
#
# Kauê de Sousa

library("tidyverse")
library("magrittr")

list.files("data")

# read RHOMis data with ids
hh <- "data/raw/names_rhomis_trifinio.csv"
hh %<>% 
  read_csv()


# read the citizen science data
df <- "data/commonbean.csv"
df %<>% 
  read_csv() %>% 
  select(id, name, relative_name, adm3, adm1) %>% 
  distinct(id, .keep_all = TRUE)

#.....................................
#.....................................
# fix names in community ####

# first in the RHOMIS data
adm3 <- sort(unique(hh$adm3))

# first colunm has the wrong names of pattern and 
# the second has the fixed
repl <- matrix(c("Buena Vista, casco urbano","Buena Vista - Casco Urbano",
                 "Caserio Las Cruces, Aldea Matazano", "Matazano",
                 "Chantiago el Rodeo", "Chantiago El Rodeo",
                 "El Coco,  Aldea Pacren", "El Coco - Aldea Pacren",
                 "El Rodeo","Chantiago El Rodeo",
                 "GUALCHO", "Gualcho",
                 "Las Queseras, Despoblado", "Las Queseras - Despoblado",
                 "Linda Vista, casco urbano", "Linda Vista - Casco Urbano",
                 "LOS SITIOS", "Los Sitios",
                 "SAN RAMON","San Ramon",
                 "Caserio Changuis, El Limon", "Caserio Changuis - El Limon"), 
               nrow = 11, ncol = 2, byrow = TRUE)

for(i in 1:nrow(repl)) {
  hh$adm3 <- ifelse(hh$adm3 == repl[i,1], repl[i,2],hh$adm3)
}

sort(unique(hh$adm3))


# now for the tricot data 
adm3 <- sort(unique(df$adm3))

repl <- matrix(c("Agua Zarca La mina","Agua Zarca La Mina",
                 "Aldea Matazano", "Matazano",
                 "Buena Vista", "Buena Vista - Casco Urbano",
                 "Caserio Changuis El Limon","Caserio Changuis - El Limon",
                 "Caserio pitahaya Nearar", "Pitahaya Nearar",
                 "Caserio Pitahaya Nearar", "Pitahaya Nearar",
                 "Chilin De Argueta", "Chilin de Argueta",
                 "El Coco Pacren","El Coco - Aldea Pacren",
                 "El Rodeo","Chantiago El Rodeo",
                 "las cruces Matazano", "Matazano",
                 "las Cruces Matazano", "Matazano",
                 "Las Cruces Matazano", "Matazano",
                 "Las Queseras Despoblado","Las Queseras - Despoblado",
                 "Linda Vista","Linda Vista - Casco Urbano",
                 "Los Penascos","Penascos",
                 "Majada", "La Majada",
                 "Mojon", "El Mojon",
                 "Plan Del Guineo" , "Plan del Guineo",
                 "Potrero", "Potreros"), 
               nrow = 19, ncol = 2, byrow = TRUE)
                 
for(i in 1:nrow(repl)) {
  df$adm3 <- ifelse(df$adm3 == repl[i,1], repl[i,2],df$adm3)
}

# one adm3 is missing because the tricot project was not applied there after all 
# drop this 
adm3 <- sort(unique(df$adm3))

hh <- hh[hh$adm3 %in% adm3, ]

#.....................................
#.....................................
# find matches ####
# empty vector to match with survey dataset
df$hhid <- NA
df$hh_member <- NA
df$survey_name <- NA
df$headhh_name <- NA

# run over kebeles 
# then over farmers names within kebeles to find possible match
for (j in seq_along(adm3)) {
  # subset datasets
  x <- hh[hh$adm3 == adm3[j], ]
  
  y <- df[df$adm3 == adm3[j], ]
  
  for(i in seq_along(y$id)) {
    # find index for possible matches
    # we can play arround these arguments and try
    # to increase number of matches
    index <- agrep(y[[i, "name"]], x$name, max = 3, ignore.case = TRUE)
    
    # if no match, go to the next farmer
    if(length(index) == 0){
      cat("look at the next pattern\n")
      index <- agrep(y[[i, "name"]], x$survey_name, max = 3, ignore.case = TRUE)
    }
    
    if(length(index) == 0){
      cat("look at the next pattern\n")
      index <- agrep(y[[i, "name"]], x$headhh_name, max = 3, ignore.case = TRUE)
    }
    
    if(length(index) == 0) next
    
    # if more than one match, take the first
    if(length(index) > 1) {
      cat("two pattern found for this entry\n")
      index <- index[1]
    } 
    
    # get the hh id
    index_id <- x$id[index]
    
    # and the farmer name
    index_name <- x$name[index]
    
    index_survey_name <- x$survey_name[index]
    
    index_headhh_name <- x$headhh_name[index]
    
    # put it into the citizen science dataset
    df[[df$id == y$id[i], "hhid"]] <- index_id
    
    df[[df$id == y$id[i], "hh_member"]] <- index_name
    
    df[[df$id == y$id[i], "survey_name"]] <- index_survey_name
    
    df[[df$id == y$id[i], "headhh_name"]] <- index_headhh_name
    
  }
}



sum(!is.na(df$hhid))
length(unique(df$hhid))

df$dups <- duplicated(df$hhid)

write_csv(df, "data/raw/hhsurvey_matches_tricot.csv")


