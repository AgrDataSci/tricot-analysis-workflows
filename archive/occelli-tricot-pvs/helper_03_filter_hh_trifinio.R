# Filter RHOMIS data using matched ids from tricot
#
# Kauê de Sousa

# most of the ids (485) were matched using the script 
# script/02_match_farmers_ids_trifinio.R
# the others were checked manually 

library("tidyverse")
library("readxl")
library("magrittr")

list.files("data")
list.files("data/raw", full.names = TRUE)


ids <- "data/raw/hhsurvey_matches_tricot_R-and-hand.csv"
ids %<>% 
  read_csv() %>% 
  filter(!is.na(hhid)) %>% 
  select(id, hhid) %>% 
  rename(package_id = id)

# read the baseline survey
excel_sheets("data/raw/trifinio_baseline_survey_data_all.xlsx")

hh <- read_xlsx("data/raw/trifinio_baseline_survey_data_all.xlsx",
                sheet = "Household Data")

names(ids)

hh %<>% 
  rename(hhid = id)


hh %<>% 
  merge(ids, ., by = "hhid", all.y = TRUE) %>% 
  as_tibble()

names(hh)

treat <- hh[!is.na(hh$package_id),c("package_id","treatment","treatment_status")]

table(treat$treatment_status, treat$treatment)

hh$treatment <- ifelse(!is.na(hh$package_id), "CCI", hh$treatment)

hh$treatment_status <- ifelse(!is.na(hh$package_id) & hh$treatment == "CCI", "In", hh$treatment_status)


write_csv(hh, "data/trifinio_baseline_survey.csv")



