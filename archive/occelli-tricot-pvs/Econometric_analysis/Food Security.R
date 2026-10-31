# PARTICIPATIVE APPROACH AND FOOD SECURITY

#Packages and Libraries
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

#Get the working directory where diversity data are
setwd("~/GitHub/socioeco-tricot/data/Diversity Index")
getwd()
# Call the file for baseline data
trif_sec <- read_excel("trifinio_foodinsecurity.xlsx")
View(trif_sec)
str(trif_sec$HHS_end_cat)
str(trif_sec$HHS_base_cat)
str(trif_sec$HHS_delta_cat)

trif_sec %>% mutate(
  treat = factor(treat,
                 levels=c("1","2", "3"),
                 labels=c("Control", "PSV", "Tricot") 
  )
) %>%
ggplot(aes(x=M_end, fill=treat))+
  geom_histogram(alpha =.5, position = "dodge")+
  scale_fill_manual(values = c("Control"="#7570b3" , "PSV"="#d95f02", "Tricot"="#1b9e77"))

ggplot(data = trif_sec, aes(x=as.factor(HHS_end_cat))) +
  geom_bar(aes(y = (..count..)/2098),position="dodge", size=4) +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~ as.factor(treat))

trif_sec %>% mutate(
  treat = factor(treat,
                 levels=c("1","2", "3"),
                 labels=c("Control", "PSV", "Tricot") 
  )
) %>%
  ggplot(aes(x=as.numeric(HHS_END), fill=treat)) +
  geom_histogram(aes(y = (..count..)/2098),position="dodge", size=4, binwidth = 0.7) +
  geom_vline(aes(xintercept = median(as.numeric(HHS_END), na.rm = T)))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Control"="#7570b3" , "PSV"="#d95f02", "Tricot"="#1b9e77")) +
  facet_wrap(~ as.factor(treat))+
  #theme_classic() +
  xlab("Mergalef Index (bean)") + 
  ylab("Farmers (%)") +
  theme(legend.position="",
        legend.title = element_blank())

#Econometric exercise
getwd()
setwd("~/GitHub/socioeco-tricot/data/Trifinio _data")
trif_all <- read_excel("Evaluation_FINAL_trifinio.xlsx")
setwd("~/GitHub/socioeco-tricot/data/Ethiopia _data")
eth_all <- read_excel("Evaluation_FINAL_Ethiopia.xlsx")

#Main dependent variable is the food security (HHS)
#So the model is a multinomial ordinal logistic regression
str(trif_all$HHS_end_cat)
str(trif_all$HHS_delta_cat)
str(trif_all$HHS_base_cat)
str(trif_all$age)

age <- as.numeric(trif_all$age) 
View(age)

mod_foodsec <- polr(as.factor(HHS_end_cat) ~ as.factor(treat) + 
                      HHS_base_cat + ethnicity + education + 
                      gender + as.numeric(age) + crop_bean_before + land_owner +
                      as.numeric(SAU_baseline),
                      na.action = na.omit,
                      data = trif_all, Hess=TRUE)
mod_foodsec <- polr(as.factor(HHS_end_cat) ~ as.factor(treat) + HHS_base_cat,
                    na.action = na.omit,
                    data = trif_all, Hess=TRUE)
summary(mod_foodsec)
(ctable <- coef(summary(mod_foodsec)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
exp(coef(mod_foodsec))


mod_foodsec_ETH <- polr(as.factor(HHS_end_cat) ~ as.factor(Treat) + 
                      hh_gender + offfarm_income_dum19 + 
                      as.factor(learn_farming_imitatingvillage),
                    na.action = na.omit,
                    data = eth_all, Hess=TRUE)
summary(mod_foodsec_ETH)
(ctable <- coef(summary(mod_foodsec_ETH)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

#Main dependent variable is Mergalef index, so the model is a generalized linear model
library(miceadds)
mod_diversity <- lm.cluster(as.numeric(M_end) ~ as.factor(treat) + as.numeric(M_base) +
                       ethnicity + education + country + 
                       gender + as.numeric(age) + crop_bean_before + land_owner +
                       as.numeric(SAU_baseline), data=trif_all, cluster="treat")
summary(mod_diversity)

# Relevel for categoric reference to be PVS
str(trif_all$treat)
trif_all$treat = relevel(as.factor(trif_all$treat), ref = 2) 
mod_div_PVSbased <- lm(as.numeric(M_end) ~ as.factor(treat) + as.numeric(M_base) +
                         ethnicity + education + country + 
                         gender + as.numeric(age) + crop_bean_before + land_owner +
                         as.numeric(SAU_baseline), data=trif_all)
summary(mod_div_PVSbased)

foodsec_lm <- lm.cluster(HHS_end_cat ~ as.factor(treat) +
                      HHS_base_cat + ethnicity + education + 
                      gender + as.numeric(age) + crop_bean_before + land_owner +
                      as.numeric(SAU_baseline),data = trif_all, cluster="adm3")

summary(foodsec_lm)

 install.packages("plm")
library(plm)
foodsec.fe <- plm(HHS_end_cat ~ as.factor(treat) + HHS_base_cat + country, data=trif_all, model="between")
summary(foodsec.fe)


#Representation of the results as histograms
#This representation is still not convincing -> CHECK!!!
trif_all %>% mutate(
  hunger = factor(HHS_end_cat,
                 levels=c("1","2", "3"),
                 labels=c("Little hunger", "Moderate hunger", "Severe hunger")) 
  ) %>% mutate(
  treat2 = factor(treat,
                  levels=c("1","2", "3"),
                  labels=c("Control", "PSV", "Tricot"))
) %>%
  ggplot(aes(x=as.numeric(treat), fill=treat2)) +
  geom_histogram(aes(y = (..count..)/2098),position="dodge", size=4, binwidth=1) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Control"="#7570b3" , "PSV"="#d95f02", "Tricot"="#1b9e77")) +
  facet_wrap(~ as.factor(hunger))+
  #theme_classic() +
  xlab("Participatory treatment") + 
  ylab("Farmers (%)") +
  theme(legend.position="",
        legend.title = element_blank())


