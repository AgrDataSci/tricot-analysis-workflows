# DATA: TRIFINIO dataset 2015 - 2018
# SCRIPT: socio-economic analysis

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

# Building the dataset

#Baseline
base_hh_data <- read_excel("trifinio_baseline_survey_matched.xlsx")
base_var_data <- read_excel("!div_base_final.xlsx")

base_data <- merge(base_hh_data, base_var_data, by="hhid")

#Endline

end_hh_data <- read_excel("trifinio_endline_survey_matched_cleaned.xlsx")
end_var_data <- read_excel("!div_end_final.xlsx")

end_data <- merge(end_hh_data, end_var_data, by="hhid")

# Number of villages and control groups

str(base_hh_data$adm3) 

co_village <- base_hh_data %>% 
  filter(!is.na(treatment)) %>% 
  group_by(treatment, adm3) %>% 
  count()


co_village[!duplicated(co_village$adm3),]
dim(co_village)
c <- which(table(co_village$adm3) == 1)
result <- co_village[co_village$adm3 %in% names(c),]


# Rename Control_treated those control farmers in treated villages

base_data$new_treatment <- 0

obj <- subset(result, result$treatment == "Control")
obj

hits <- which(base_data$adm3 %in% obj$adm3)
hits

base_data$new_treatment[hits] <- 1

base_data$treatment_combined <- paste0(base_data$treatment.x, "_", base_data$new_treatment)
base_data$treatment_combined

base_data$treatment_combined <- as_factor(base_data$treatment_combined)
levels(base_data$treatment_combined)

# Calculate self - induced attrition rate

final %>%
  group_by(treatment_combined, treatment_status) %>%
  summarise(mean = mean(count_index_end, na.rm = T))

final %>%
  group_by(treatment_combined, treatment_status) %>%
  summarise(mean = mean(as.numeric(diversification_index_end), na.rm = T))

food_merged %>%
  group_by(treatment_combined, treatment_status) %>%
  summarise(mean = mean(as.numeric(HHS_end_cat), na.rm = T))


#Descriptive - variety and agronomic data

joint_var_data <- merge(base_var_data, end_var_data, by="hhid")
joint_var_data$count_diff <- ((as.numeric(joint_var_data$count_index_end) - joint_var_data$count_index_base) / joint_var_data$count_index_base)  

joint_var_data %>%
  ggplot(aes(x=as.numeric(count_index_base), fill=treatment)) +
  geom_histogram(aes(y = (..count..)/1978),position="dodge", size=4, binwidth = 0.7) +
  #geom_vline(aes(xintercept = mean(as.numeric(count_index_base), na.rm = T)))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Control"="#7570b3" , "PVS"="#d95f02", "CCI"="#1b9e77")) +
  facet_wrap(~ as.factor(treatment))+
  #theme_classic() +
  xlab("Diversity count index (2015)") + 
  ylab("Farmers (%)") +
  theme(legend.position="",
        legend.title = element_blank())

joint_var_data %>%
  ggplot(aes(x=as.numeric(count_index_end), fill=treatment)) +
  geom_histogram(aes(y = (..count..)/1978),position="dodge", size=4, binwidth = 0.7) +
  #geom_vline(aes(xintercept = mean(as.numeric(count_index_base), na.rm = T)))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Control"="#7570b3" , "PVS"="#d95f02", "CCI"="#1b9e77")) +
  facet_wrap(~ as.factor(treatment))+
  #theme_classic() +
  xlab("Diversity count index (2018)") + 
  ylab("Farmers (%)") +
  theme(legend.position="",
        legend.title = element_blank())

joint_var_data %>%
  ggplot(aes(x=as.numeric(count_diff), fill=treatment)) +
  geom_histogram(aes(y = (..count..)/1978),position="dodge", size=4, binwidth = 0.7) +
  #geom_vline(aes(xintercept = mean(as.numeric(count_index_base), na.rm = T)))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Control"="#7570b3" , "PVS"="#d95f02", "CCI"="#1b9e77")) +
  facet_wrap(~ as.factor(treatment))+
  #theme_classic() +
  xlab("Diversity count index (growth rate)") + 
  ylab("Farmers (%)") +
  theme(legend.position="",
        legend.title = element_blank())

# Boxplot representation of the delta in the two varietal indices

delta_count <- data_frame(base_data$hhid, base_data$treatment.x, base_data$lat, base_data$lon, base_data$count_index_base, base_data$diversification_index_base)
names(delta_count)[names(delta_count) == "base_data$hhid"] <- "hhid"
merge_count <- merge(delta_count, end_var_data, by="hhid")

merge_count$delta_count <- (as.numeric(merge_count$count_index_end) - as.numeric(merge_count$`base_data$count_index_base`))

merge_count %>%
  ggplot(aes(x=as.numeric(delta_count), fill=merge_count$`base_data$treatment.x`)) +
  geom_histogram(aes(y = (..count..)/1978),position="dodge", size=4, binwidth = 0.7) +
  #geom_vline(aes(xintercept = mean(as.numeric(count_index_base), na.rm = T)))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Control"="#7570b3" , "PVS"="#d95f02", "CCI"="#1b9e77")) +
  facet_wrap(~ as.factor(merge_count$`base_data$treatment.x`))+
  #theme_classic() +
  xlab("Diversity count index (growth rate)") + 
  ylab("Farmers (%)") +
  theme(legend.position="",
        legend.title = element_blank())


#Complete dataset
final <- merge(end_data, base_data, by="hhid")

#Make control the reference factor and rename levels
final$treatment_combined <- as.factor(final$treatment_combined)
levels(final$treatment_combined)
final <- within(final, treatment_combined <- relevel(treatment_combined, ref = 3))

levels(final$treatment_combined)[levels(final$treatment_combined) == "Control_0"] <- "C_T"
levels(final$treatment_combined)[levels(final$treatment_combined) == "Control_1"] <- "C_C"
levels(final$treatment_combined)[levels(final$treatment_combined) == "PVS_0"] <- "PVS"
levels(final$treatment_combined)[levels(final$treatment_combined) == "CCI_0"] <- "CCI"
levels(final$treatment_combined)

# Correlation with diversity index

m1a <- glm(as.numeric(count_index_end)  ~ treatment.x + count_index_base + 
            as.numeric(max_joint_ha) + sum_prod_bean_end + gender +
            ethnic_group + as.numeric(interviewed_age) + 
            as.numeric(land_own) + bean_know + offfarm_incomes + aidreceived + bean_who_works_female,
            final, family="quasipoisson")
summary(m1a)

m1b <- lm(as.numeric(count_index_end)  ~ treatment.x + count_index_base + 
             as.numeric(max_joint_ha) + sum_prod_bean_end + gender +
             ethnic_group + as.numeric(interviewed_age) + 
             as.numeric(land_own) + bean_know + offfarm_incomes + aidreceived + bean_who_works_female,
            final)
summary(m1a)

m2a <- lm(as.numeric(diversification_index_end) ~ treatment.x + count_index_base + 
            as.numeric(max_joint_ha) + sum_prod_bean_end + gender +
            ethnic_group + as.numeric(interviewed_age) + 
            as.numeric(land_own) + bean_know + offfarm_incomes + aidreceived + bean_who_works_female,
          final)

summary(m2a)


##### LINEAR MODEL #####

# Model with DID for PVS
did_pvs_cc <- final[final$treatment_combined %in% c("PVS", "C_C"),]
table(did_pvs_cc$treatment_combined)

did_ccs_cc <- final[final$treatment_combined %in% c("CCI", "C_C"),]
table(did_ccs_cc$treatment_combined)
table(did_pvs_cc$post)

did_ccs$treatment.x <- as.factor(did_ccs$treatment.x)
levels(did_ccs$treatment.x)
did_ccs <- within(did_ccs, treatment.x <- relevel(treatment.x, ref = 2))


did1_ccs <- lm(as.numeric(count_index_end)  ~ treatment.x + post*treatment.x, did_ccs)
summary(did1_ccs)

library(car)

# Check whether the effects from the two treatments are statistically different
linearHypothesis(m1a, "treatment.xPVS = treatment.xCCI")

ttest <- function(m1a, coefnum, val){
  co <- coef(summary(m1a))
  tstat <- (co[coefnum,1]-val)/co[coefnum,2]
  2 * pt(abs(tstat), m1a$df.residual, lower.tail = FALSE)
}
ttest(m1a, 2, 0.1)


m1b <- glm(as.numeric(count_index_end)  ~ treatment.x + count_index_base + 
            as.numeric(max_joint_ha) + sum_prod_bean_end + gender +
            ethnic_group + as.numeric(interviewed_age) + 
            as.numeric(land_own) + bean_know + offfarm_incomes + aidreceived + bean_who_works_female + 
            participation_seed,
            final, family="quasipoisson")
summary(m1b)

# Check whether the effects from the two treatments are statistically different
linearHypothesis(m1b, "treatment.xPVS = treatment.xCCI")


#Robust standard errors for m1a and m1b
coeftest(m1a, vcov=vcovHC(m1, "HC1"))

# Model with diversity index: richness index  

class(final$diversification_index_end)
(as.numeric(final$diversification_index_end))

m2a <- lm(as.numeric(diversification_index_end)  ~ treatment.x + as.numeric(diversification_index_base) +
            as.numeric(max_joint_ha) + sum_prod_bean_end + gender +
            ethnic_group + as.numeric(interviewed_age) + as.numeric(land_own) +
            bean_know + offfarm_incomes + aidreceived,
         final)
summary(m2a)

# Check whether the effects from the two treatments are statistically different
linearHypothesis(m2a, "treatment.xPVS = treatment.xCCI")

m2b <- lm(as.numeric(diversification_index_end)  ~ treatment.x + as.numeric(diversification_index_base) + 
             as.numeric(max_joint_ha) + sum_prod_bean_end + gender +
             ethnic_group + as.numeric(interviewed_age) + 
             as.numeric(land_own) + bean_know + offfarm_incomes + aidreceived + bean_who_works_female + participation_seed,
           final)
summary(m2b)

# Check whether the effects from the two treatments are statistically different
linearHypothesis(m2b, "treatment.xPVS = treatment.xCCI")

#Robust standard errors for m2a and m2b
coeftest(m2a, vcov=vcovHC(m2, "HC1"))
coeftest(m2b, vcov=vcovHC(m2, "HC1"))

# Model with Household Hunger Score

# Build food security dataset
food <- read_excel("trifinio_foodinsecurity.xlsx")

food_merged <- merge(final, food, by="hhid")

food_merged %>%
  group_by(treatment.x, food_merged$HHS_end_cat) %>%
  count()

food_merged$treatment.x <- as.factor(food_merged$treatment.x)
levels(food_merged$treatment.x)
food_merged <- within(food_merged, treatment.x <- relevel(treatment.x, ref = 2))

# Model food security 

food_merged[food_merged=="n/a"] = NA
food_merged[food_merged=="NA"] = NA

mod_foodsec2 <- polr(as.factor(HHS_end_cat) ~ treatment.x + 
                      HHS_base_cat + ethnic_group + 
                      as.numeric(max_joint_ha) + sum_prod_bean_end +
                      as.numeric(interviewed_age) + bean_know +  
                      bean_who_works_female,
                      na.action = na.omit,
                      data = food_merged, Hess=TRUE)
summary(mod_foodsec2)
(ctable <- coef(summary(mod_foodsec2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
exp(coef(mod_foodsec))

# Check whether the effects from the two treatments are statistically different
linearHypothesis(mod_foodsec2, "treatment.xPVS = treatment.xCCI")

# Focus on variety received are the best

mod_foodsec3 <- polr(as.factor(HHS_end_cat) ~ treatment.x + 
                       HHS_base_cat + ethnic_group + 
                       as.numeric(interviewed_age) + as.numeric(max_joint_ha) + sum_prod_bean_end +
                       bean_who_works_female + as.numeric(offfarm_incomes_count) + bean_know +
                       + participation_seed,
                     na.action = na.omit,
                     data = food_merged, Hess=TRUE)
summary(mod_foodsec3)
(ctable <- coef(summary(mod_foodsec3)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
exp(coef(mod_foodsec3))

# Check whether the effects from the two treatments are statistically different
linearHypothesis(mod_foodsec3, "treatment.xPVS = treatment.xCCI")

mod_foodsec2 <- polr(as.factor(HHS_end_cat) ~ treatment.x + treatment.x*participation_seed + 
                       HHS_base_cat,
                     na.action = na.omit,
                     data = food_merged, Hess=TRUE)
summary(mod_foodsec2)
(ctable <- coef(summary(mod_foodsec2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
exp(coef(mod_foodsec3))

##### LINEAR MODEL #####

# Drop out rate

# The variable OUT is the one of interest - the head of the household participated in all the process of selection and returned the results

# Create the attrition variable
head(food_merged$treatment_status)
table(food_merged$treatment_status)
food_merged$treatment_status <- as_factor(food_merged$treatment_status)
head(food_merged$treatment_status)

# Create the summary table for the different attrition groups
install.packages("arsenal")
library(arsenal)

food_merged %>%
  group_by(treatment_status) %>%
  summarise(mean_var = mean_sd(age_malehead, na_rm = TRUE))

m <- paste(extract_fstat(lm(age_malehead ~ treatment_status, data = data_attr)),
           extract_fpvalue(lm(age_malehead ~ treatment_status, data = data_attr)),
           collapse = ",")
m

food_merged$landcultivated <- as.numeric(food_merged$landcultivated)
head(food_merged$landcultivated)

data_attr %>%
  group_by(treatment_status) %>%
  summarise(mean_var = mean_sd(landcultivated, na_rm = TRUE))


m2 <- paste(extract_fstat(lm(landcultivated ~ treatment_status, data = data_attr)),
           extract_fpvalue(lm(landcultivated ~ treatment_status, data = data_attr)),
           collapse = ",")
m2

data_attr$annual_bought<- as.numeric(data_attr$annual_bought)
data_attr %>%
  group_by(treatment_status) %>%
  summarise(mean_var=mean_sd(annual_bought, na_rm = T))

m3 <- paste(extract_fstat(lm(annual_bought ~ treatment_status, data = data_attr)),
            extract_fpvalue(lm(annual_bought ~ treatment_status, data = data_attr)),
            collapse = ",")
m3

data_attr$hh_type <- as.numeric(data_attr$hh_type)
head(data_attr$hh_type)

data_attr %>%
  group_by(treatment_status) %>% 
  sd(hh_type)

m4 <- paste(extract_fstat(lm(hh_type ~ treatment_status, data = data_attr)),
            extract_fpvalue(lm(hh_type ~ treatment_status, data = data_attr)),
            collapse = ",")
m4

data_attr$gender_interviewed <- as.numeric(data_attr$gender_interviewed)
head(data_attr$gender_interviewed)

data_attr %>%
  group_by(treatment_status) %>% 
  count(gender_interviewed)

m5 <- paste(extract_fstat(lm(gender_interviewed ~ treatment_status, data = data_attr)),
            extract_fpvalue(lm(gender_interviewed ~ treatment_status, data = data_attr)),
            collapse = ",")
m5

table(food_merged$debts_have)
food_merged$debts_have <- ifelse(food_merged$debts_have == "No", 1, 0)


str(data_attr$land_owner)
data_attr$land_owner <- as.numeric(data_attr$area_season2)

data_attr$land_owner <- ifelse(data_attr$land_owner == "No", 0, 1)

data_attr %>%
  group_by(treatment_status) %>% 
  summarise(mena_var=mean_sd(land_owner, na_rm = T))

m5 <- paste(extract_fstat(lm(land_owner ~ treatment_status, data = data_attr)),
            extract_fpvalue(lm(land_owner ~ treatment_status, data = data_attr)),
            collapse = ",")
m5


# Sub-select only those farmers who have a treatment status which is not control
data_attr <- food_merged[food_merged$treatment.x != "Control", ]

data_attr %>%
  group_by(treatment.x, treatment_status) %>%
  count()

data_attr$treatment_status <- ifelse(data_attr$treatment_status == "Out", 1, 0)

head(data_attr$treatment_status)

# Poisson regression on the probability to drop out before delivering the final evaluations

attr_mod1 <- glm(treatment_status ~ as.factor(treatment.x) + ethnic_group + gender_interviewed + hh_persons +
                   as.numeric(interviewed_age) + as.numeric(max_joint.x) + sum_prod_tot_base +
                   bean_who_works_female + as.numeric(offfarm_incomes_count) + bean_know + HHS_base_cat + count_index_base,
                  family = "binomial", data = data_attr)
summary(attr_mod1)

data_attr$count_

attr_mod2 <- lm(treatment_status ~ as.factor(treatment.x), data = data_attr)
summary(attr_mod2)

# Adoption and yield results

# Adoption - adoption group = PVS adoption while adoption - tricot = CCS adoption

table(food_merged$adoption_tricot)
head(food_merged$adoption_tricot)
str(food_merged$adoption_tricot)

table(food_merged$adoption_group)
head(food_merged$adoption_group)
str(food_merged$adoption_group)

# Recode to create adoption dummies
food_merged$adoption_group <- recode(food_merged$adoption_group, "N" = 0, "Y" = 1, .default = NA_real_)
table(food_merged$adoption_group)

food_merged$adoption_tricot <- recode(food_merged$adoption_tricot, "N" = 0, "Y" = 1, .default = NA_real_)
table(food_merged$adoption_tricot)

food_merged$tricot_group_end <- as.factor(food_merged$tricot_group_end)
class(food_merged$tricot_group_end)
levels(food_merged$tricot_group_end)

# Brief analysis on adoption

data_attr$adoption_group <- as_factor(data_attr$adoption_group)
levels(data_attr$adoption_group)
table(data_attr$adoption_group)

levels(data_attr$adoption_group)[levels(data_attr$adoption_group)=="N"] <- 0
levels(data_attr$adoption_group)[levels(data_attr$adoption_group)=="Y"] <- 1
levels(data_attr$adoption_group)[levels(data_attr$adoption_group)=="n/a"] <- NA

data_attr$adoption_tricot <- as_factor(data_attr$adoption_tricot)
levels(data_attr$adoption_tricot)
table(data_attr$adoption_tricot)

levels(data_attr$adoption_tricot)[levels(data_attr$adoption_tricot)=="N"] <- 0
levels(data_attr$adoption_tricot)[levels(data_attr$adoption_tricot)=="Y"] <- 1
levels(data_attr$adoption_tricot)[levels(data_attr$adoption_tricot)=="n/a"] <- NA

data_attr$adoption_v2 <- rowSums(cbind(as.numeric(data_attr$adoption_group), as.numeric(data_attr$adoption_tricot)), na.rm=TRUE)


data_attr$adoption_v2 <- as_factor(data_attr$adoption_v2)
levels(data_attr$adoption_v2)
levels(data_attr$adoption_v2)[levels(data_attr$adoption_v2)=="2"] <- 1
table(data_attr$adoption_v2)

levels(data_attr$treatment_combined)
data_attr <- within(data_attr, treatment_combined <- relevel(treatment_combined, ref = 4))

adopt_data <- merge(data_attr, panel, by="hhid")
table(adopt_data$treatment_status.x)

levels(adopt_data$treatment_combined.x)
adopt_data <- within(adopt_data, treatment_combined.x <- relevel(treatment_combined.x, ref = 2))

str(adopt_data$participation_seed.x)

adopt_mod_did <- ivprobit(as.numeric(adoption_v2) ~ treatment_combined.x + treatment_combined.x*post.y + as.numeric(participation_seed.x)
                         |. -treatment_combined.x + treatment_status.x, data = adopt_data)

adopt_mod_glm <- glm(as.numeric(adoption_v2) ~ treatment_combined.x + treatment_combined.x*as.numeric(participation_seed.x), data = adopt_data)

summary(adopt_mod_did)
summary(adopt_mod_glm)

adopt_data %>%
  group_by(adoption_v2, participation_seed.x) %>%
  count(na.rm=T)

yield_mod_did <- ivreg(bean_prod_sum ~ treatment_combined.x + post.y + treatment_combined.x*post.y + as.numeric(participation_seed.x) + 
                         offfarm_incomes.x + hh_size + bean_who_works_female.x + 
                         bean_area_sum +  gender.x + ethnic_group.x|. -treatment_combined.x + treatment_status.x, data = adopt_data)

summary(yield_mod_did)

adopt_data$participation_seed.x <- as_factor(adopt_data$participation_seed.x)
levels(adopt_data$participation_seed.x)

levels(adopt_data$participation_seed.x)[levels(adopt_data$participation_seed.x)=="N"] <- 0
levels(adopt_data$participation_seed.x)[levels(adopt_data$participation_seed.x)=="Y"] <- 1
levels(adopt_data$participation_seed.x)[levels(adopt_data$participation_seed.x)=="n/a"] <- NA

str(adopt_data$participation_seed.x)

seed_mod_did <- ivreg(as.numeric(participation_seed.x) ~ treatment_combined.x + post.y + treatment_combined.x*post.y + 
                         offfarm_incomes.x + hh_size + bean_who_works_female.x + 
                         bean_area_sum +  gender.x + ethnic_group.x
                       |. -treatment_combined.x + treatment_status.x, data = adopt_data)

summary(seed_mod_did)

data_attr %>%
  group_by(treatment_combined) %>%
  count(adoption_v2)

table(data_attr$treatment.x)

table(food_merged$adoption_group)
str(food_merged$adoption_tricot)
table(food_merged$adoption_v2)

adopt_mod_PVS <- glm(adoption_v2 ~ as_factor(treatment.x), family = "binomial", data = food_merged)
summary(adopt_mod_PVS)

adopt_mod_CCS <- glm(adoption_tricot ~ as_factor(treatment.x), family = "binomial", data = food_merged)
summary(adopt_mod_CCS)


# Approach success

str(data_attr$participation_seed)
data_attr$participation_seed <- recode(data_attr$participation_seed, "N" = 0, "Y" = 1, .default = NA_real_)

success_PVS <- glm(participation_seed  ~ as_factor(treatment.x) + as.numeric(max_joint.x) + ethnic_group + hh_persons +
                     gender_interviewed + as.numeric(interviewed_age)  + sum_prod_tot_base +
                     bean_who_works_female + as.numeric(offfarm_incomes_count) + bean_know, family = "binomial", data = data_attr)
summary(success_PVS)

data_attr %>%
  group_by(treatment.x) %>%
  count(participation_seed)

by(food_merged$treatment.x, food_merged$adoption_tricot, table)

# Quantile treatment effect on yield
library(quantreg)
library(qte)

yield_data <- merge(base_var_data, end_var_data, by="hhid")

head(yield_data$treatment)

yield_model <- lm(sum_prod_bean_end ~ as.factor(treatment), data = yield_data)
summary(yield_model)

yield_q_mod <- ci.qtet(sum_prod_bean_end ~ as.factor(treatment), data = yield_data, probs = seq(0.05,0.95,0.05), se=T, iters=10)
summary(yield_q_mod)

by(yield_data$sum_prod_tot_base, yield_data$treatment, quantile)
quant_effect_yieldend <- by(yield_data$sum_prod_bean_end, yield_data$treatment, quantile)

View(quant_effect_yieldbase)
View(quant_effect_yieldend)

##### DiD - IV analysis _ following Ogutu et al. 2020 #####

baseline_trifinio <- read_excel("DiD-IV dataset/baseline_trifinio.xlsx")
endline_trifinio <- read_excel("DiD-IV dataset/endline_trifinio.xlsx")

#Combine into a panel dataset

panel_base <- merge(base_var_data, baseline_trifinio, by="hhid")
extra <- data.frame(final$hhid, final$participation_seed)
colnames(extra) <- c("hhid", "delivery_seed")
panel_base <- merge(panel_base, extra, by="hhid")

panel_end <- merge(end_var_data, endline_trifinio, by="hhid")
extra_end <- data.frame(final$hhid, final$participation_seed)
colnames(extra_end) <- c("hhid", "delivery_seed")
panel_end <- merge(panel_end, extra_end, by="hhid")

panel_base_2 <- panel_base[c(1,6,7,8, 9, 13, 14, 16:29)]
colnames(panel_base_2) <- c("hhid", "area_bean_max", "count_index", "diversification_index",
                            "treatment", "prod_bean_tot", "year", "treatment_status", "HHS_index",
                            "ethnic_group", "education", "gender", "age",
                            "hh_size", "bean_who_works_female", "offfarm_incomes",
                            "offfarm_incomes_count", "bean_know", "adm3", "week_consumption", "land_owner")
panel_base_2$delivery_seed <- panel_end_2$delivery_seed
panel_base_2<-panel_base_2[,order(colnames(panel_base_2),decreasing=TRUE)]

panel_end_2 <- panel_end[c(1,23:25, 28:45 )]
colnames(panel_end_2) <- c("hhid", "area_bean_max", "count_index", "diversification_index",
                            "prod_bean_tot", "year", "treatment","treatment_status", "HHS_index",
                            "ethnic_group", "education", "gender", "age",
                            "hh_size", "bean_who_works_female", "offfarm_incomes",
                            "offfarm_incomes_count", "bean_know", "adm3", "week_consumption", "land_owner", "delivery_seed")
panel_end_2<-panel_end_2[,order(colnames(panel_end_2),decreasing=TRUE)]

panel <- rbind(panel_base_2, panel_end_2)
str(panel)
library(tidyverse)
panel <- panel[order(panel$hhid),]

library(plm)
install.packages("pglm")
library(pglm)

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
panel$treatment_combined <- as.factor(panel$treatment_combined)
panel <- within(panel, treatment_combined <- relevel(treatment_combined, ref = 3))

#Create dummy time
panel$post <- ifelse(panel$year == "2018",1,0)
table(panel$post)

#Create one treatment category only at the time
# PVS - C_C only

#################################################
y <- merge(panel_adopt, data_attr, by="hhid")
table(y$post.x)
table(y$treatment_combined.x)
table(y$adm)


y <- within(y, treatment_combined.x <- relevel(treatment_combined.x, ref = 2))

did_pvs_cc_adoption <- glm(adoption_v2 ~ treatment_combined.x + post.x + treatment_combined.x*post.x, 
                          data = y)
summary(did_pvs_cc_adoption)

did_adoption <- ivreg(bean_prod_sum ~ treatment_combined.x + post.x + treatment_combined.x*post.x  |. -treatment_combined.x + treatment_status.x, data = y)
summary(did_adoption)

#################################################

panel_pvs_cc <- panel[panel$treatment_combined %in% c("PVS", "C_C"),]
table(panel_pvs_cc$treatment_combined)

str(panel_pvs_cc$treatment_combined)
panel_pvs_cc$treatment_combined
levels(panel_pvs_cc$treatment_combined)

hist(y$bean_prod_sum)

table(as.numeric(panel_pvs_cc$count_index))

did_pvs_cc_1 <- lm(as.numeric(count_index) ~ treatment_combined + post + treatment_combined*post + 
                             offfarm_incomes + hh_size + bean_who_works_female + 
                             as.numeric(area_bean_max) +gender + ethnic_group, data = panel_pvs_cc)
summary(did_pvs_cc_1)

did_pvs_1 <- glm(as.numeric(count_index) ~ treatment_combined*post+ 
                   offfarm_incomes + hh_size + bean_who_works_female + 
                   as.numeric(area_bean_max) +gender + ethnic_group, data = panel_pvs_cc, family = "quasipoisson")
summary(did_pvs_1)


did_pvs_cc_2 <- lm(as.numeric(diversification_index) ~ treatment_combined + post + treatment_combined*post,
                             data = panel_pvs_cc)

summary(did_pvs_cc_2)

panel_pvs_cc$HHS_index <- as_factor(panel_pvs_cc$HHS_index)
table(panel_pvs_cc$HHS_index)

did_pvs_cc_3 <- polr(HHS_index ~ treatment_combined + post + treatment_combined*post,
                       na.action = na.omit,
                      data = panel_pvs_cc, Hess=TRUE)
summary(did_pvs_cc_3)
library(sandwich)
library(lmtest)
coeff_1 <- coeftest(did_pvs_cc_3, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))

# PVS - C_T only
panel_pvs_ct <- panel[panel$treatment_combined %in% c("PVS", "C_T"),]
table(panel_pvs_ct$treatment_combined)

str(panel_pvs_ct$treatment_combined)
levels(panel_pvs_cc$treatment_combined)
panel_pvs_ct <- within(panel_pvs_ct, treatment_combined <- relevel(treatment_combined, ref = 3))


did_pvs_ct_1 <- lm(as.numeric(count_index) ~ treatment_combined + post + treatment_combined*post + 
                     offfarm_incomes + hh_size + bean_who_works_female + 
                     as.numeric(area_bean_max) +gender + ethnic_group, data = panel_pvs_ct)
summary(did_pvs_ct_1)


did_pvs_ct_2 <- lm(as.numeric(diversification_index) ~ treatment_combined + post + treatment_combined*post + 
                     offfarm_incomes + hh_size + bean_who_works_female + 
                     as.numeric(area_bean_max) +gender + ethnic_group, data = panel_pvs_ct)

summary(did_pvs_ct_2)

panel_pvs_ct$HHS_index <- as_factor(panel_pvs_ct$HHS_index)
table(panel_pvs_ct$HHS_index)

did_pvs_ct_3 <- polr(as_factor(HHS_index) ~ treatment_combined + post + treatment_combined*post + 
                       offfarm_incomes + hh_size + bean_who_works_female + 
                       as.numeric(area_bean_max) +gender + ethnic_group, 
                     na.action = na.omit,
                     data = panel_pvs_ct, Hess=TRUE)
summary(did_pvs_ct_3)
library(sandwich)
library(lmtest)
coeff_1 <- coeftest(did_pvs_ct_3, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))


# CCS - C_C only

panel_ccs_cc <- panel[panel$treatment_combined %in% c("CCI", "C_C"),]
panel_ccs_cc$treatment_combined

levels(panel_ccs_cc$treatment_combined)
panel_ccs_cc <- within(panel_ccs_cc, treatment_combined <- relevel(treatment_combined, ref = 1))

did_ccs_cc <- lm(as.numeric(count_index) ~ treatment_combined + post + treatment_combined*post, data = panel_ccs_cc)
summary(did_ccs_cc)

did_ccs_2 <- lm(as.numeric(diversification_index) ~ treatment_combined + post + treatment_combined*post + 
                  offfarm_incomes + hh_size + bean_who_works_female + 
                  as.numeric(area_bean_max) +  gender + ethnic_group, data = panel_ccs_cc)

summary(did_ccs_2)

str(panel_ccs_cc$HHS_index)
did_ccs_3 <- polr(as_factor(HHS_index) ~ treatment_combined + post + treatment_combined*post + 
                    offfarm_incomes + hh_size + bean_who_works_female + 
                    as.numeric(area_bean_max) +  gender + ethnic_group,
                  na.action = na.omit,
                  data = panel_ccs_cc, Hess=TRUE)
summary(did_ccs_3)
library(sandwich)
library(lmtest)
coeff_ccs <- coeftest(did_ccs_3, vcov = NULL, cluster = ~adm3)
coeff_ccs
exp(coef(coeff_1))

# CCS - C_T only

panel_ccs_ct <- panel[panel$treatment_combined %in% c("CCI", "C_T"),]
panel_ccs_ct$treatment_combined

levels(panel_ccs_ct$treatment_combined)
panel_ccs_ct <- within(panel_ccs_ct, treatment_combined <- relevel(treatment_combined, ref = 3))

did_ccs_ct <- lm(as.numeric(count_index) ~ treatment_combined + post + treatment_combined*post + 
                   offfarm_incomes + hh_size + bean_who_works_female + 
                   as.numeric(area_bean_max) +  gender + ethnic_group, data = panel_ccs_ct)
summary(did_ccs_ct)

did_ccs_ct_2 <- lm(as.numeric(diversification_index) ~ treatment_combined + post + treatment_combined*post , data = panel_ccs_ct)

summary(did_ccs_ct_2)

str(panel_ccs_ct$HHS_index)
did_ccs_3 <- polr(as_factor(HHS_index) ~ treatment_combined + post + treatment_combined*post + 
                    offfarm_incomes + hh_size + bean_who_works_female + 
                    as.numeric(area_bean_max) +  gender + ethnic_group,
                  na.action = na.omit,
                  data = panel_ccs_ct, Hess=TRUE)
summary(did_ccs_3)
library(sandwich)
library(lmtest)
coeff_ccs <- coeftest(did_ccs_3, vcov = NULL, cluster = ~adm3)
coeff_ccs
exp(coef(coeff_1))

# Estimation of the IV TOT effects

library(AER)
library(car)
library(ivtools)
library(ivpack)

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
                as.numeric(area_bean_max) +  gender + ethnic_group  |. -treat + status, data = panel_pvs_cc)
summary(iv_1)

iv_2 <- ivreg(as.numeric(diversification_index) ~ treat + post + treat*post+ 
                offfarm_incomes + hh_size + bean_who_works_female + 
                gender + ethnic_group |. -treat + status, data = panel_pvs_cc)
summary(iv_2)

str(panel_pvs_cc$HHS_index)

iv_3 <- ivreg(as.numeric(HHS_index) ~ treat + post + treat*post + 
                offfarm_incomes + hh_size + bean_who_works_female + 
                as.numeric(area_bean_max) + gender + ethnic_group |. -treat + status, data = panel_pvs_cc)
summary(iv_3)

# PVS - C_T only

panel_pvs_ct$treat <- ifelse(panel_pvs_ct$treatment_combined=="PVS", 1, 0)
panel_pvs_ct$status <- ifelse(panel_pvs_ct$treatment_status=="In", 1, 0)
table(panel_pvs_ct$treat)
table(panel_pvs_ct$treatment_combined)
table(panel_pvs_ct$status)
first_stage <- lm(status ~ as.factor(treat), data = panel_pvs_ct)
summary(first_stage)

iv_1 <- ivreg(as.numeric(count_index) ~ treat + post + treat*post  |. -treat + status, data = panel_pvs_ct)
summary(iv_1)

iv_2 <- ivreg(as.numeric(diversification_index) ~ treat + post + treat*post  |. -treat + status, data = panel_pvs_ct)
summary(iv_2)

str(panel_pvs_ct$HHS_index)

iv_3 <- ivreg(as.numeric(HHS_index) ~ treat + post + treat*post  |. -treat + status, data = panel_pvs_ct)
summary(iv_3)

# CCS - C_C only

panel_ccs_cc$treat <- ifelse(panel_ccs_cc$treatment_combined=="CCI", 1, 0)
panel_ccs_cc$status <- ifelse(panel_ccs_cc$treatment_status=="In", 1, 0)
table(panel_ccs_cc$treat)
table(panel_ccs_cc$treatment_combined)
table(panel_ccs_cc$status)
first_stage <- lm(status ~ as.factor(treat), data = panel_ccs_cc)
summary(first_stage)


iv_1b <- ivreg(as.numeric(count_index) ~ treat + post + treat*post  |. -treat + status, data = panel_ccs_cc)
summary(iv_1b)

iv_2b <- ivreg(as.numeric(diversification_index) ~ treat + post + treat*post |. -treat + status, data = panel_ccs_cc)
summary(iv_2b)

iv_3b <- ivreg(as.numeric(HHS_index) ~ treat + post + treat*post  + 
                 offfarm_incomes + hh_size + bean_who_works_female + 
                 as.numeric(area_bean_max) + gender + ethnic_group|. -treat + status, data = panel_ccs_cc)
summary(iv_3b)

# CCS - C_T only

panel_ccs_ct$treat <- ifelse(panel_ccs_ct$treatment_combined=="CCI", 1, 0)
panel_ccs_ct$status <- ifelse(panel_ccs_ct$treatment_status=="In", 1, 0)
table(panel_ccs_ct$treat)
table(panel_ccs_ct$treatment_combined)
table(panel_ccs_ct$status)
first_stage <- lm(status ~ as.factor(treat), data = panel_ccs_ct)
summary(first_stage)


iv_1b <- ivreg(as.numeric(count_index) ~ treat + post + treat*post  |. -treat + status, data = panel_ccs_ct)
summary(iv_1b)

iv_2b <- ivreg(as.numeric(diversification_index) ~ treat + post + treat*post |. -treat + status, data = panel_ccs_ct)
summary(iv_2b)

iv_3b <- ivreg(as.numeric(HHS_index) ~ treat + post + treat*post + 
                 offfarm_incomes + hh_size + bean_who_works_female + 
                 as.numeric(area_bean_max) + gender + ethnic_group |. -treat + status, data = panel_ccs_ct)
summary(iv_3b)


# Brief exercise on yield

panel_ccs_pvs <- panel[panel$treatment_combined %in% c("CCI", "PVS"),]
panel_ccs_pvs$treatment_combined

panel_ccs_pvs$treat <- ifelse(panel_ccs_pvs$treatment_combined=="CCI", 1, 0)
panel_ccs_pvs$status <- ifelse(panel_ccs_pvs$treatment_status=="In", 1, 0)

panel_ccs_pvs$delivery_seed <- as_factor(panel_ccs_pvs$delivery_seed)
levels(panel_ccs_pvs$delivery_seed)[levels(panel_ccs_pvs$delivery_seed) == "n/a"] <- NA
levels(panel_ccs_pvs$delivery_seed)[levels(panel_ccs_pvs$delivery_seed) == "Y"] <- 1
levels(panel_ccs_pvs$delivery_seed)[levels(panel_ccs_pvs$delivery_seed) == "N"] <- 0
levels(panel_ccs_pvs$delivery_seed)

str(panel_ccs_pvs$bean_who_works_female)

yield <- ivreg(as.numeric(prod_bean_tot) ~ treat + post + treat*post   +
                 as.numeric(area_bean_max) + delivery_seed|. -treat + status, data = panel_ccs_pvs)
summary(yield)

delivery <- ivreg(as.numeric(delivery_seed) ~ treat + post + treat*post |. -treat + status, data = panel_ccs_pvs)
summary(delivery)

# Finally merge adoption data

adoption <- merge(panel_ccs_pvs, final, by="hhid")

adoption$adoption_group <- as_factor(adoption$adoption_group)
levels(adoption$adoption_group)
table(adoption$adoption_group)

levels(adoption$adoption_group)[levels(adoption$adoption_group)=="N"] <- 0
levels(adoption$adoption_group)[levels(adoption$adoption_group)=="Y"] <- 1
levels(adoption$adoption_group)[levels(adoption$adoption_group)=="n/a"] <- NA

adoption$adoption_tricot <- as_factor(adoption$adoption_tricot)
levels(adoption$adoption_tricot)
table(adoption$adoption_tricot)

levels(adoption$adoption_tricot)[levels(adoption$adoption_tricot)=="N"] <- 0
levels(adoption$adoption_tricot)[levels(adoption$adoption_tricot)=="Y"] <- 1
levels(adoption$adoption_tricot)[levels(adoption$adoption_tricot)=="n/a"] <- NA

adoption$adoption_v2 <- rowSums(cbind(as.numeric(adoption$adoption_group), as.numeric(adoption$adoption_tricot)), na.rm=TRUE)


adoption$adoption_v2 <- as_factor(adoption$adoption_v2)
levels(adoption$adoption_v2)
levels(adoption$adoption_v2)[levels(adoption$adoption_v2)=="2"] <- 1
table(adoption$adoption_v2)

table(adoption$off)

adoption$treat <- ifelse(adoption$treatment_combined.x=="CCI", 1, 0)
adoption$status <- ifelse(adoption$treatment_status.x=="In", 1, 0)

adop <- ivreg(as.numeric(adoption_v2) ~ treat + post.x + treat*post.x +  
                offfarm_incomes.x + hh_size + bean_who_works_female.x + 
                as.numeric(area_bean_max) + gender.x + ethnic_group.x |. -treat + status, data = adoption)
summary(adop)

livelihood <- ivreg(as.numeric(HHS_index) ~ treat + post.x + treat*post.x  + 
                      offfarm_incomes.x + hh_size + bean_who_works_female.x + 
                      as.numeric(area_bean_max) + gender.x + ethnic_group.x + adoption_v2|. -treat + status, data = adoption)
summary(livelihood)

adoption %>%
  group_by(adoption_group, treat) %>%
  summarise(mean(HHS_index))


# Calculations with control groups combined

panel$treatment <- as_factor(panel$treatment)
levels(panel$treatment)
panel <- within(panel, treatment <- relevel(treatment, ref = 2))


all_1 <- ivreg(as.numeric(diversification_index) ~ treatment + post + treatment*post |. - treatment + treatment_status, data = panel)
summary(all_1)


a <- panel %>%
  group_by(adm3, treatment) %>%
  count()


# Further analysis on the mechanisms (PVS only)

# PVS evaluations
PVS_evaluations <- read_csv("Mechanisms analysis/PVS_evaluations.csv")

data_participation <- PVS_evaluations[c(1,3, 5, 18, 58)]

data_participation$farmer_participation_ECA3 <- ifelse(data_participation$farmer_participation_ECA3=="Yes", 1, 0)
table(data_participation$farmer_participation_ECA3)

data_participation$farmer_participation_ECA4 <- ifelse(data_participation$farmer_participation_ECA4=="Yes", 1, 0)
table(data_participation$farmer_participation_ECA4)

data_participation$farmer_participation_ECA5 <- ifelse(data_participation$farmer_participation_ECA5=="Yes", 1, 0)
table(data_participation$farmer_participation_ECA5)

data_participation$PVS_part <- (data_participation$farmer_participation_ECA3 + data_participation$farmer_participation_ECA4 + data_participation$farmer_participation_ECA5)
table(data_participation$PVS_part)  

data_participation <- data_participation %>%
  rename(hhid = id_household)

# Calculate how this influence the household food security only on treated 

baseline_trifinio <- read_excel("DiD-IV dataset/baseline_trifinio.xlsx")
endline_trifinio <- read_excel("DiD-IV dataset/endline_trifinio.xlsx")

panel <- rbind(baseline_trifinio, endline_trifinio)
panel <- panel[order(panel$hhid),]

table(panel$treatment)
panel_ccs_pvs <- panel[panel$treatment %in% c("CCI", "PVS"),]

food_panel <- merge(panel, data_participation, by="hhid")
food_panel$post <- ifelse(food_panel$year == "2018",1,0)

food_panel$PVS_part[food_panel$post == 0] <- 0 

model_degree_part <- polr(as_factor(HHS_index) ~ as_factor(PVS_part) + gender + offfarm_incomes,
                          na.action = na.omit,
                          data = food_panel, Hess=TRUE)
summary(model_degree_part)
coeff_1 <- coeftest(model_degree_part, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))

model <- lm(HHS_index ~ as_factor(PVS_part) + gender + offfarm_incomes, data = food_panel_post)
summary(model)

adop_panel <- merge(food_panel, adoption, by="hhid")

adop_panel$HH

model_degree_DOP <- polr(as_factor(HHS_index.x) ~ as_factor(PVS_part) + adoption_v2 + gender + offfarm_incomes,
                          na.action = na.omit,
                          data = adop_panel, Hess=TRUE)
summary(model_degree_DOP)
coeff_1 <- coeftest(model_degree_DOP, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))

# Delivery of the right chosen variety - look at the registry

PVS_evaluations <- read_csv("Mechanisms analysis/PVS_evaluations.csv")
PVS_variety_delivered <- read_csv("Mechanisms analysis/PVS_variety delivered.csv")


# Varieties as independent variables
baseline_trifinio <- read_excel("DiD-IV dataset/baseline_trifinio.xlsx")
endline_trifinio <- read_excel("DiD-IV dataset/endline_trifinio.xlsx")

panel <- rbind(baseline_trifinio, endline_trifinio)
panel <- panel[order(panel$hhid),]

table(panel$treatment)
panel_ccs_pvs <- panel[panel$treatment %in% c("CCI", "PVS"),]

# Retrieve info on adoption
new <- merge(x=panel_ccs_pvs, y=final[,c("hhid", "adoption_group", "adoption_tricot")], by="hhid", all.x = TRUE)
new$adoption_group <- as_factor(new$adoption_group)
levels(new$adoption_group)
table(new$adoption_group)

levels(new$adoption_group)[levels(new$adoption_group)=="N"] <- 0
levels(new$adoption_group)[levels(new$adoption_group)=="Y"] <- 1
levels(new$adoption_group)[levels(new$adoption_group)=="n/a"] <- NA

new$adoption_tricot <- as_factor(new$adoption_tricot)
levels(new$adoption_tricot)
table(new$adoption_tricot)

levels(new$adoption_tricot)[levels(new$adoption_tricot)=="N"] <- 0
levels(new$adoption_tricot)[levels(new$adoption_tricot)=="Y"] <- 1
levels(new$adoption_tricot)[levels(new$adoption_tricot)=="n/a"] <- NA

new$adoption_v2 <- rowSums(cbind(as.numeric(new$adoption_group), as.numeric(new$adoption_tricot)), na.rm=TRUE)

new$adoption_v2 <- as_factor(new$adoption_v2)
levels(new$adoption_v2)
levels(new$adoption_v2)[levels(new$adoption_v2)=="2"] <- 1
table(new$adoption_v2)

# Retrieve info on yield
panel_base_new <- read_excel("DiD-IV dataset/panel_base_new.xlsx")
panel_end_new <- read_excel("DiD-IV dataset/panel_end_new.xlsx")

yield_base <- data_frame(panel_base_new$hhid, panel_base_new$sum_prod_tot_base, panel_base_new$max_joint)
yield_base$year <- 2015

yield_end <- data_frame(panel_end_new$hhid, panel_end_new$sum_prod_bean_end, panel_end_new$max_joint_ha)
yield_end$year <- 2018

colnames(yield_base) <- c("hhid", "prod_tot", "area_tot", "year")
colnames(yield_end) <- c("hhid", "prod_tot", "area_tot", "year")

yield <- rbind(yield_base, yield_end)
yield <- yield[order(yield$hhid),]

# Dataset with yield, adoption and treatment data
all <- merge(new, yield, by=c("hhid", "year"))

# Include data on varieties delivered
PVS_variety_delivered <- read_csv("Mechanisms analysis/PVS_variety delivered.csv")

library(fastDummies)
PVS_variety_delivered <- dummy_cols(PVS_variety_delivered, select_columns = "variety")
PVS_variety_delivered  <- PVS_variety_delivered [c(1, 5:16)]
PVS_variety_delivered <- PVS_variety_delivered %>%
  rename(adm3 = Community)

all_2 <- merge(all, PVS_variety_delivered, by = "adm3")
data_participation <- data_participation %>%
  rename(adm3 = Community)
all_3 <- merge(all_2, data_participation, by=c("hhid", "adm3"))

all_3 <- all_3 %>% 
  rename(local1 = `variety_Local 1`)

all_3 %>% 
  group_by(adm3, year) %>% 
  summarise(count=table(variety_A))

library("writexl")
write_xlsx(y,"y.xlsx")

model_all1 <- polr(as_factor(HHS_index) ~ as_factor(PVS_part) + adoption_v2 +  variety_A +
                     variety_B + variety_C +variety_D +variety_E +variety_F +variety_G + variety_H + variety_I + variety_J +
                     local1 + local2, na.action = na.omit,
                     data = all_3, Hess=TRUE)
summary(model_all1)
coeff_1 <- coeftest(model_all1, vcov = NULL, cluster = ~adm3)
coeff_1
exp(coef(coeff_1))

exp1 <- lm(HHS_index ~ variety_A, data=all_3)
summary(exp1)

