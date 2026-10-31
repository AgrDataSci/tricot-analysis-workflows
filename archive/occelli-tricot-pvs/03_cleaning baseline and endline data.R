# DATA: TRIFINIO
# SCRIPT: treatment check, diversity data

library(readr)
library(tidyverse)
library(naniar)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

setwd("~/Documents/GitHub/socioeco-tricot/data/Trifinio_data/Matched_survey_end_base")

# Importing baseline data (2015)
base_data <- read_csv("trifinio_baseline_survey_matched.csv")

# Importing endline data (2018)
end_data <- read_csv("trifinio_endline_survey_matched.csv")

# Check if the HHID are exactly matching
check1 <- ifelse(base_data$hhid==end_data$hhid, "Yes", "No")
table(check1)

####### TREATMENT CHECK #######

# Check if they report the same treatment (presence of spillover effects)
treat_df <- data.frame(base_data$treatment, end_data$tricot_group)
View(treat_df)
str(treat_df)

treat_df[treat_df=="n/a"] = NA

colnames(treat_df)
names(treat_df)[names(treat_df) == "base_data.treatment"] <- "base_treat"
names(treat_df)[names(treat_df) == "end_data.tricot_group"] <- "end_treat"

table(treat_df$base_treat)

treat_df[,1] <- ifelse(treat_df[,1] == "CCI", 2, ifelse(treat_df[,1] == "PVS", 1, 0))
treat_df[,2] <- ifelse(treat_df[,2] == "tricot", 2, ifelse(treat_df[,2] == "group", 1, 0))

same_treat <- ifelse(treat_df[,2] == treat_df[,1], "Yes", "No")
table(same_treat)

# 409 households declare at endline of being included in a treatment different from baseline

high_treat <- ifelse(treat_df[,2] > treat_df[,1], "Yes", "No")
table(high_treat)

# 279 (68%) households declare at endline that their treatment is more engaging than the one of baseline


####### DIVERSITY MEASURE ####### 

#Baseline - bean data

data_base_allid <- read_excel("trifinio_baseline_survey_data_all_master.xlsx", 5)
names(data_base_allid)[names(data_base_allid) == "id"] <- "hhid"

#Leave behind hhid which are not inside the matched baseline/endline analysis (panel structure needed)
merge_var <- merge(data_base_allid, base_data, by="hhid")

# Count and list how many bean varieties are grown by every household at baseline (by counting how many times a hhid appears in the variety list)
var_count_base <- tally(group_by(merge_var, hhid))
View(var_count_base)

# Area in which the bean varieties are grown
area_base <- merge_var[, c("hhid", "variety", "type", "area_season1.x", "area_season2.x", "area_season1_2015.x", "max_area") ]

#We are interested into the max area in which each variety is grown between 2014 and 2015
area_max <- data.frame(area_base$hhid, area_base$max_area)

area_max_wide <- area_max %>%
  pivot_wider(names_from = area_base.max_area, values_from = area_base.max_area)

unite.df <- area_max_wide %>%
unite("final", 2:82, remove = F)

for (i in c(1:3)){
  write.xlsx(unite.df[i], file="test.xlsx", sheetName=paste(i), append=T)
}

# If we would be interested in all areas, then we would need to use the formula below
joint_var_area_base <- area_base %>%
  pivot_wider(names_from = variety, values_from = c(area_season1.x, area_season2.x, area_season1_2015.x, max_area))

View(joint_var_area_base)

names(joint_var_area_base)


# Check if var count and area count collide (they do, are both 1866)
area_count <- tally(group_by(area_base, hhid))

#Extract hhid which are not contained into the base data (and therefore do not match also the endline data)
diff <- as.data.frame(setdiff(base_data$hhid, var_base$hhid))
str(diff)
write_xlsx(diff,"diff_hhid_var_base.xlsx")

#Merge variety count and area (Extra)

var_base <- merge(var_count_base, joint_var_area_base, by="hhid")
class(var_base)
var_base[var_base=="NULL"] = 0
var_base[var_base=="NA"] = NA

# Computation of the diversity index

# Diversity index 1 - count
div_base <- data_frame(var_base$hhid, var_base$n)
names(div_base)[names(div_base) == "var_base$hhid"] <- "hhid"
names(div_base)[names(div_base) == "var_base$n"] <- "count"

write_xlsx(div_base,"div_base.xlsx")

# Call joint excel file from count and area data for baseline

var_base <- read_excel("Baseline_varieties and areas/var_base.xlsx")
area_base_var <- read_excel("Baseline_varieties and areas/area_base_var.xlsx", 
                            col_types = c("numeric", "numeric", "numeric", 
                                          "numeric"))

div_base_final <- merge(var_base, area_base_var, by="hhid", sort = T)
div_base_final$count_index_base <- div_base_final$n
div_base_final$diversification_index_base <- (div_base_final$n/div_base_final$max_joint)

# As Inf value appears since we divide by NA values, we replace Inf with NA
div_base_final[div_base_final=="Inf"] = NA

write_xlsx(div_base_final,"div_base_final.xlsx")

#Calculate diversity data for both baseline and endline

#Recall datasets

div_base_data <- read_excel("Variety_area_data/!div_base_final.xlsx")

div_end_data <- read_excel("Variety_area_data/!div_end_final.xlsx", 
                              col_types = c("numeric", "text", "text", 
                                            "text", "text", "text", "text", "numeric", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "numeric", "numeric"))

div_merge <- merge(div_base_data, div_end_data, by="hhid")

div_merge$delta_count <- ((div_merge$count_index_end - div_merge$count_index_base) / div_merge$count_index_base)
div_merge$diff_count <- (div_merge$count_index_end - div_merge$count_index_base)

g <- div_merge %>%
  group_by(treatment) %>%
  summarise_each(funs(mean(., na.rm=T)))

data_base_allid <- read_excel("trifinio_baseline_survey_data_all_master.xlsx", 5)
names(data_base_allid)[names(data_base_allid) == "id"] <- "hhid"

d <- merge(data_base_allid, div_base_data, by="hhid")
d <- subset(d, select = c("hhid", "production_season1", "production_season2", "production_season3", "variety" ))

joint_d_base <- d %>%
  pivot_wider(names_from = variety, values_from = c(production_season1, production_season2, production_season3))

unit_1 <- joint_d_base %>%
  unite("season1_prod", 2:99, remove=F)

unit_2 <- joint_d_base %>%
  unite("season2_prod", 100:197, remove=F)

unit_3 <- joint_d_base %>%
  unite("season3_prod", 198: 195, remove = F)

new_prod <- data_frame(unit_1$hhid, unit_1$season1_prod, unit_2$season2_prod, unit_3$season3_prod)
names(new_prod)[names(new_prod) == "unit_1$hhid"] <- "hhid"

m <- merge(new_prod, div_base_data, by="hhid")

write_xlsx(m, "m.xlsx")
write_csv(m, "m.csv")

