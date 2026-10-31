# DATA: TRIFINIO dataset 2015 - 2018
# SCRIPT: econometric analysis after Reviewers' comments

# Libraries
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(readr)
library(miceadds)
library(tidyverse)

yourwd <-"~/Documents/GitHub/socioeco-tricot/data/Trifinio_data/!Final data cleaned for analysis"
setwd(yourwd)

### DiD - IV analysis _ following Ogutu et al. 2020 ###

#Open dataset
baseline_trifinio <- read_excel("DiD-IV dataset/baseline_trifinio.xlsx")
endline_trifinio <- read_excel("DiD-IV dataset/endline_trifinio.xlsx")

#Baseline
base_hh_data <- read_excel("trifinio_baseline_survey_matched.xlsx")
base_var_data <- read_excel("!div_base_final.xlsx")

#Endline
end_hh_data <- read_excel("trifinio_endline_survey_matched_cleaned.xlsx")
end_var_data <- read_excel("!div_end_final.xlsx")

#Combine into a panel dataset
panel_base <- merge(base_var_data, baseline_trifinio, by="hhid")
panel_end <- merge(end_var_data, endline_trifinio, by="hhid")

panel_base_2 <- panel_base[c(1,6:9, 13, 14, 16, 17, 18, 19, 20, 21, 22:26, 27, 28, 29)]
colnames(panel_base_2) <- c("hhid", "area_bean_max", "count_index", "diversification_index",
                            "treatment", "prod_bean_tot", "year", "treatment_status", "HHS_index",
                            "ethnic_group", "education", "gender", "age",
                            "hh_size", "bean_who_works_female", "offfarm_incomes",
                            "offfarm_incomes_count", "bean_know", "adm3", "week_consumption", "land_owner")
panel_base_2<-panel_base_2[,order(colnames(panel_base_2),decreasing=TRUE)]

panel_end_2 <- panel_end[c(1, 23, 24, 25, 28:44)]
colnames(panel_end_2) <- c("hhid", "area_bean_max", "count_index", "diversification_index",
                           "prod_bean_tot", "year", "treatment","treatment_status", "HHS_index",
                           "ethnic_group", "education", "gender", "age",
                           "hh_size", "bean_who_works_female", "offfarm_incomes",
                           "offfarm_incomes_count", "bean_know", "adm3", "week_consumption", "land_owner")
panel_end_2<-panel_end_2[,order(colnames(panel_end_2),decreasing=TRUE)]

v <- panel_end_2[panel_end_2$hhid %in% panel_base_2$hhid,] #elements which are contained at endline but not baseline
m <- panel_base_2[panel_base_2$hhid %in% panel_end_2$hhid,]
panel <- rbind(m, v) #create the panel
panel <- panel[order(panel$hhid),]

writexl::write_xlsx(panel, "panel.xlsx")

# In Excel, we cleaned the panel as some observations in 2015 (at baseline) were copied two times
# Final number of observations in the panel equals 3732 (1866 X 2)

panel <- read_excel("panel.xlsx")

# To take care of spill-overs and look at the effect in control villages and treated villages we proceed as follows:
# Creation of a new treatment combined for the two levels of control

co_village <- base_hh_data %>% 
  filter(!is.na(treatment)) %>% 
  group_by(treatment, adm3) %>% 
  count()

co_village[!duplicated(co_village$adm3),]
dim(co_village)
c <- which(table(co_village$adm3) == 1)
result <- co_village[co_village$adm3 %in% names(c),]

# Rename Control_treated those control farmers in treated villages

panel$new_treatment <- 0

obj <- subset(result, result$treatment == "Control")
obj

hits <- which(panel$adm3 %in% obj$adm3)
hits

panel$new_treatment[hits] <- 1

panel$treatment_combined <- paste0(panel$treatment, "_", panel$new_treatment)
panel$treatment_combined

panel$treatment_combined <- as_factor(panel$treatment_combined)
levels(panel$treatment_combined)

levels(panel$treatment_combined)[levels(panel$treatment_combined) == "Control_0"] <- "C_T"
levels(panel$treatment_combined)[levels(panel$treatment_combined) == "Control_1"] <- "C_C"
levels(panel$treatment_combined)[levels(panel$treatment_combined) == "PVS_0"] <- "PVS"
levels(panel$treatment_combined)[levels(panel$treatment_combined) == "CCI_0"] <- "CCI"
levels(panel$treatment_combined)

#Reshape levels of the categorical treatment
str(panel$treatment_combined)
panel$treatment_combined <- as.factor(panel$treatment_combined)
panel <- within(panel, treatment_combined <- relevel(treatment_combined, ref = 3))

#Create dummy time
panel$post <- ifelse(panel$year == "2018",1,0)
table(panel$post)

# # We address Rev. #2 comments regarding the control function

library(nnet) # First stage of the control function

panel$treatment_first_stage <- panel$treatment_combined #we are not interested in the spillovers matter now
levels(panel$treatment_first_stage)[levels(panel$treatment_first_stage) == "C_T"] <- "Control"
levels(panel$treatment_first_stage)[levels(panel$treatment_first_stage) == "C_C"] <- "Control"
table(panel$treatment_first_stage)
str(panel$treatment_first_stage)
panel <- within(panel, treatment_first_stage <- relevel(treatment_first_stage, ref = 3))

#First stage - Control Function

panel$treatment_status <- as.factor(panel$treatment_status)
levels(panel$treatment_status)
panel <- within(panel, treatment_status <- relevel(treatment_status, ref = 1))

FirstStage <- multinom(treatment_first_stage ~ treatment_status + offfarm_incomes + 
                         hh_size + bean_who_works_female + 
                         as.numeric(area_bean_max) +gender + ethnic_group, data = panel)
summary(FirstStage)

ZG_hat <- predict(FirstStage, type = "probs")
treatment_pred <- pnorm(ZG_hat)

InvMills_D1 <-  ifelse(panel$treatment_first_stage=="PVS",  dnorm(-ZG_hat)/(1-pnorm(-ZG_hat)), 0)
InvMills_D0 <-  ifelse(panel$treatment_first_stage=="Control", -dnorm(-ZG_hat)/(pnorm(-ZG_hat)), 0)
InvMills_D2 <-  ifelse(panel$treatment_first_stage=="CCI",  dnorm(-ZG_hat)/(1-pnorm(-ZG_hat)), 0)
ExpectedError <- ifelse(panel$treatment_first_stage=="PVS", InvMills_D1, 
                        ifelse(panel$treatment_first_stage=="CCI", InvMills_D2, InvMills_D0)) 
ExpectedError_PVS <- ifelse(panel$treatment_first_stage=="PVS", InvMills_D1, InvMills_D0)
ExpectedError_CCI <-  ifelse(panel$treatment_first_stage=="CCI", InvMills_D2, InvMills_D0)

panel$ExpectedError <- ExpectedError
panel$ExpectedError_PVS <- ExpectedError_PVS
panel$ExpectedError_CCI <- ExpectedError_CCI

# Second stage - Control Function

str(panel$treatment_combined)

did_CF_count_index <- lm(as.numeric(count_index) ~ treatment_first_stage + post + treatment_first_stage*post +
                     offfarm_incomes + hh_size + bean_who_works_female + 
                     as.numeric(area_bean_max) + gender + ethnic_group + ExpectedError,
                    data = panel)
summary(did_CF_count_index)
coeff_3 <- coeftest(did_CF_count_index, vcov = NULL, cluster = ~adm3)
coeff_3

did_CF_diversification <- lm(as.numeric(diversification_index) ~ treatment_first_stage + post + treatment_first_stage*post + 
                     offfarm_incomes + hh_size + bean_who_works_female + 
                     as.numeric(area_bean_max) +gender + ethnic_group + ExpectedError, 
                   data = panel)

summary(did_CF_diversification)
coeff_2 <- coeftest(did_CF_diversification, vcov = NULL, cluster = ~adm3)
coeff_2

str(panel$HHS_index)
panel$HHS_index <- as_factor(panel$HHS_index)
table(panel$HHS_index)

did_CF_HHS<- polr(HHS_index ~ treatment_first_stage + post + treatment_first_stage*post, 
                     na.action = na.omit,
                     data = panel, Hess=TRUE)
summary(did_CF_HHS)
library(sandwich)
library(lmtest)
coeff_1 <- coeftest(did_CF_HHS, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))

## Below, we address the Rev. #2 comments regarding IPW method
library(ipw)

# We calculate the weight using the ipwpoint function 
panel_ipw <- panel[complete.cases(panel[,c("gender", "ethnic_group", "education")]),]
weight <- ipwpoint(exposure = treatment_combined, family = "multinomial",
                   numerator = ~1,
                   denominator = ~ gender + education + ethnic_group, data = as.data.frame(panel_ipw))
summary(weight)
weight$ipw.weights

panel_ipw$ipw2015 = weight$ipw.weights

did_weight_count_index <- lm(as.numeric(count_index) ~ treatment_combined + post + treatment_combined*post +
                           offfarm_incomes + hh_size + bean_who_works_female + 
                           as.numeric(area_bean_max) + gender + ethnic_group, weights = weight$ipw.weights,
                         data = panel_ipw)
summary(did_weight_count_index)
coeff_3 <- coeftest(did_weight_count_index, vcov = NULL, cluster = ~adm3)
coeff_3

# We re-run the IV-DID with the IPW, both ITT and TOT
# Let's start with the ITT

# PVS - C_C only

panel_pvs_cc <- panel_ipw[panel_ipw$treatment_combined %in% c("PVS", "C_C"), ]
levels(panel_pvs_cc$treatment_combined)
panel_pvs_cc$treatment_combined <- droplevels(panel_pvs_cc$treatment_combined)
table(panel_pvs_cc$treatment_combined)

did_pvs_cc <- lm(as.numeric(count_index) ~ treatment_combined + post + treatment_combined*post + 
                     offfarm_incomes + hh_size + bean_who_works_female + 
                     as.numeric(area_bean_max) + gender + ethnic_group, weights = panel_pvs_cc$ipw2015,
                   data = panel_pvs_cc)
summary(did_pvs_cc)
coeff <- coeftest(did_pvs_cc, vcov = NULL, cluster = ~adm3)
coeff


did_pvs_cc_2 <- lm(as.numeric(diversification_index) ~ treatment_combined + post + treatment_combined*post +
                     offfarm_incomes + hh_size + bean_who_works_female + 
                     as.numeric(area_bean_max) + gender + ethnic_group, 
                     weights = panel_pvs_cc$ipw2015,
                   data = panel_pvs_cc)

summary(did_pvs_cc_2)
coeff <- coeftest(did_pvs_cc_2, vcov = NULL, cluster = ~adm3)
coeff

panel_pvs_cc$HHS_index <- as_factor(panel_pvs_cc$HHS_index)
table(panel_pvs_cc$HHS_index)

did_pvs_cc_3 <- polr(HHS_index ~ treatment_combined + post + treatment_combined*post, weights = panel_pvs_cc$ipw2015,
                     na.action = na.omit,
                     data = panel_pvs_cc, Hess=TRUE)
summary(did_pvs_cc_3)
library(sandwich)
library(lmtest)
coeff_1 <- coeftest(did_pvs_cc_3, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))

# PVS - C_T only
panel_pvs_ct <- panel_ipw[panel_ipw$treatment_combined %in% c("PVS", "C_T"),]
table(panel_pvs_ct$treatment_combined)
panel_pvs_ct$treatment_combined <- droplevels(panel_pvs_ct$treatment_combined)


did_pvs_ct1 <- lm(as.numeric(count_index) ~ treatment_combined + post + treatment_combined*post + 
                   offfarm_incomes + hh_size + bean_who_works_female + 
                   as.numeric(area_bean_max) + gender + ethnic_group, weights = panel_pvs_ct$ipw2015,
                 data = panel_pvs_ct)
summary(did_pvs_ct1)
coeff <- coeftest(did_pvs_ct1, vcov = NULL, cluster = ~adm3)
coeff


did_pvs_ct2 <- lm(as.numeric(diversification_index) ~ treatment_combined + post + treatment_combined*post +
                     offfarm_incomes + hh_size + bean_who_works_female + 
                     as.numeric(area_bean_max) + gender + ethnic_group, 
                   weights = panel_pvs_ct$ipw2015,
                   data = panel_pvs_ct)

summary(did_pvs_ct2)
coeff <- coeftest(did_pvs_ct2, vcov = NULL, cluster = ~adm3)
coeff

panel_pvs_ct$HHS_index <- as_factor(panel_pvs_ct$HHS_index)
table(panel_pvs_ct$HHS_index)

did_pvs_ct_3 <- polr(HHS_index ~ treatment_combined + post + treatment_combined*post +
                       offfarm_incomes + hh_size + bean_who_works_female + 
                       as.numeric(area_bean_max) + gender + ethnic_group, weights = panel_pvs_ct$ipw2015,
                     na.action = na.omit,
                     data = panel_pvs_ct, Hess=TRUE)
summary(did_pvs_ct_3)
coeff_1 <- coeftest(did_pvs_ct_3, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))

# CCS - C_C only

panel_ccs_cc <- panel_ipw[panel_ipw$treatment_combined %in% c("CCI", "C_C"),]
table(panel_ccs_cc$treatment_combined)
panel_ccs_cc$treatment_combined <- droplevels(panel_ccs_cc$treatment_combined)


did_ccs_cc1 <- lm(as.numeric(count_index) ~ treatment_combined + post + treatment_combined*post + 
                    offfarm_incomes + hh_size + bean_who_works_female + 
                    as.numeric(area_bean_max) + gender + ethnic_group, weights = panel_ccs_cc$ipw2015,
                  data = panel_ccs_cc)
summary(did_ccs_cc1)
coeff <- coeftest(did_ccs_cc1, vcov = NULL, cluster = ~adm3)
coeff


did_ccs_cc2 <- lm(as.numeric(diversification_index) ~ treatment_combined + post + treatment_combined*post +
                    offfarm_incomes + hh_size + bean_who_works_female + 
                    as.numeric(area_bean_max) + gender + ethnic_group, 
                  weights = panel_ccs_cc$ipw2015,
                  data = panel_ccs_cc)

summary(did_ccs_cc2)
coeff <- coeftest(did_ccs_cc2, vcov = NULL, cluster = ~adm3)
coeff

panel_ccs_cc$HHS_index <- as_factor(panel_ccs_cc$HHS_index)
table(panel_ccs_cc$HHS_index)

did_ccs_cc_3 <- polr(HHS_index ~ treatment_combined + post + treatment_combined*post, weights = panel_ccs_cc$ipw2015,
                     na.action = na.omit,
                     data = panel_ccs_cc, Hess=TRUE)
summary(did_ccs_cc_3)
coeff_1 <- coeftest(did_ccs_cc_3, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))

# CCS - C_T only

panel_ccs_ct <- panel_ipw[panel_ipw$treatment_combined %in% c("CCI", "C_T"),]
table(panel_ccs_ct$treatment_combined)
panel_ccs_ct$treatment_combined <- droplevels(panel_ccs_ct$treatment_combined)
panel_ccs_ct <- within(panel_ccs_ct, treatment_combined <- relevel(treatment_combined, ref = 2))
levels(panel_ccs_ct$treatment_combined)

did_ccs_ct1 <- lm(as.numeric(count_index) ~ treatment_combined + post + treatment_combined*post + 
                    offfarm_incomes + hh_size + bean_who_works_female + 
                    as.numeric(area_bean_max) + gender + ethnic_group, weights = panel_ccs_ct$ipw2015,
                  data = panel_ccs_ct)
summary(did_ccs_ct1)
coeff <- coeftest(did_ccs_ct1, vcov = NULL, cluster = ~adm3)
coeff

did_ccs_ct2 <- lm(as.numeric(diversification_index) ~ treatment_combined + post + treatment_combined*post +
                    offfarm_incomes + hh_size + bean_who_works_female + 
                    as.numeric(area_bean_max) + gender + ethnic_group, 
                  weights = panel_ccs_ct$ipw2015,
                  data = panel_ccs_ct)

summary(did_ccs_ct2)
coeff <- coeftest(did_ccs_ct2, vcov = NULL, cluster = ~adm3)
coeff

panel_ccs_ct$HHS_index <- as_factor(panel_ccs_ct$HHS_index)
table(panel_ccs_ct$HHS_index)

did_ccs_ct_3 <- polr(HHS_index ~ treatment_combined + post + treatment_combined*post, weights = panel_ccs_ct$ipw2015,
                     na.action = na.omit,
                     data = panel_ccs_ct, Hess=TRUE)
summary(did_ccs_ct_3)
coeff_1 <- coeftest(did_ccs_ct_3, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))

# Estimation of the IV TOT effects

library(AER)
library(car)
library(ivtools)


# PVS - C_C only

panel_pvs_cc$treat <- ifelse(panel_pvs_cc$treatment_combined=="PVS", 1, 0)
panel_pvs_cc$status <- ifelse(panel_pvs_cc$treatment_status=="In", 1, 0)
table(panel_pvs_cc$treat)
table(panel_pvs_cc$treatment_combined)
table(panel_pvs_cc$status)
first_stage <- lm(status ~ as.factor(treat), data = panel_pvs_cc)
summary(first_stage)

iv_1 <- ivreg(as.numeric(count_index) ~ treat + post + treat*post + 
                offfarm_incomes + hh_size + bean_who_works_female + 
                as.numeric(area_bean_max) +  gender + ethnic_group  |. -treat + status,weights = panel_pvs_cc$ipw2015,
              data = panel_pvs_cc)
summary(iv_1)

iv_2 <- ivreg(as.numeric(diversification_index) ~ treat + post + treat*post+ 
                offfarm_incomes + hh_size + bean_who_works_female + 
                gender + ethnic_group |. -treat + status, weights = panel_pvs_cc$ipw2015, data = panel_pvs_cc)
summary(iv_2)

str(panel_pvs_cc$HHS_index)

iv_3 <- ivreg(as.numeric(HHS_index) ~ treat + post + treat*post + 
                offfarm_incomes + hh_size + bean_who_works_female + 
                as.numeric(area_bean_max) + gender + ethnic_group |. -treat + status, weights = panel_pvs_cc$ipw2015,
              data = panel_pvs_cc)
summary(iv_3)

# PVS - C_T only

panel_pvs_ct$treat <- ifelse(panel_pvs_ct$treatment_combined=="PVS", 1, 0)
panel_pvs_ct$status <- ifelse(panel_pvs_ct$treatment_status=="In", 1, 0)
table(panel_pvs_ct$treat)
table(panel_pvs_ct$treatment_combined)
table(panel_pvs_ct$status)
first_stage <- lm(status ~ as.factor(treat), data = panel_pvs_ct)
summary(first_stage)

iv_1 <- ivreg(as.numeric(count_index) ~ treat + post + treat*post + 
                offfarm_incomes + hh_size + bean_who_works_female + 
                as.numeric(area_bean_max) +  gender + ethnic_group |. -treat + status, weights = panel_pvs_ct$ipw2015,
              data = panel_pvs_ct)
summary(iv_1)

iv_2 <- ivreg(as.numeric(diversification_index) ~ treat + post + treat*post + 
                offfarm_incomes + hh_size + bean_who_works_female + 
                as.numeric(area_bean_max) +  gender + ethnic_group |. -treat + status, weights = panel_pvs_ct$ipw2015,
              data = panel_pvs_ct)
summary(iv_2)

str(panel_pvs_ct$HHS_index)

iv_3 <- ivreg(as.numeric(HHS_index) ~ treat + post + treat*post + 
                offfarm_incomes + hh_size + bean_who_works_female + 
                as.numeric(area_bean_max) +  gender + ethnic_group |. -treat + status, weights = panel_pvs_ct$ipw2015,
              data = panel_pvs_ct)
summary(iv_3)

# CCS - C_C only

panel_ccs_cc$treat <- ifelse(panel_ccs_cc$treatment_combined=="CCI", 1, 0)
panel_ccs_cc$status <- ifelse(panel_ccs_cc$treatment_status=="In", 1, 0)
table(panel_ccs_cc$treat)
table(panel_ccs_cc$treatment_combined)
table(panel_ccs_cc$status)
first_stage <- lm(status ~ as.factor(treat), data = panel_ccs_cc)
summary(first_stage)


iv_1b <- ivreg(as.numeric(count_index) ~ treat + post + treat*post  + 
                 offfarm_incomes + hh_size + bean_who_works_female + 
                 as.numeric(area_bean_max) +  gender + ethnic_group |. -treat + status, weights = panel_ccs_cc$ipw2015,
               data = panel_ccs_cc)
summary(iv_1b)

iv_2b <- ivreg(as.numeric(diversification_index) ~ treat + post + treat*post  + 
                 offfarm_incomes + hh_size + bean_who_works_female + 
                 as.numeric(area_bean_max) +  gender + ethnic_group |. -treat + status, weights = panel_ccs_cc$ipw2015,
               data = panel_ccs_cc)
summary(iv_2b)

iv_3b <- ivreg(as.numeric(HHS_index) ~ treat + post + treat*post  + 
                 offfarm_incomes + hh_size + bean_who_works_female + 
                 as.numeric(area_bean_max) + gender + ethnic_group |. -treat + status, weights = panel_ccs_cc$ipw2015,
               data = panel_ccs_cc)
summary(iv_3b)

# CCS - C_T only

panel_ccs_ct$treat <- ifelse(panel_ccs_ct$treatment_combined=="CCI", 1, 0)
panel_ccs_ct$status <- ifelse(panel_ccs_ct$treatment_status=="In", 1, 0)
table(panel_ccs_ct$treat)
table(panel_ccs_ct$treatment_combined)
table(panel_ccs_ct$status)
first_stage <- lm(status ~ as.factor(treat), data = panel_ccs_ct)
summary(first_stage)


iv_1b <- ivreg(as.numeric(count_index) ~ treat + post + treat*post   + 
                 offfarm_incomes + hh_size + bean_who_works_female + 
                 as.numeric(area_bean_max) +  gender + ethnic_group |. -treat + status, weights = panel_ccs_ct$ipw2015,
               data = panel_ccs_ct, diagnostic = TRUE)
summary(iv_1b, diagnostics = T)

iv_2b <- ivreg(as.numeric(diversification_index) ~ treat + post + treat*post  + 
                 offfarm_incomes + hh_size + bean_who_works_female + 
                 as.numeric(area_bean_max) +  gender + ethnic_group |. -treat + status, weights = panel_ccs_ct$ipw2015,
               data = panel_ccs_ct)
summary(iv_2b)

iv_3b <- ivreg(as.numeric(HHS_index) ~ treat + post + treat*post |. -treat + status, weights = panel_ccs_ct$ipw2015,
               data = panel_ccs_ct)
summary(iv_3b)
