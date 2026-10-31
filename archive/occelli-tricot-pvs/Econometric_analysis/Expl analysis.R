# COLLABORATION ON GENETIC TRAITS AND SOCIO-ECONOMIC ANALYSIS
# First descriptive stats on trifinio

# Installed packages
install.packages("stargazer")
# Load packages
library(readr)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)

#Get the working directory
getwd()
setwd("~/GitHub/socioeco-tricot/data/Trifinio_data")
# Call the file
data <- read_csv("hhsurvey_trifinio_data.csv")


# Creating variables of interest

# Socioeconomic characteristics -> x (age, ethnic group, gender, family size, female controlling the revenues, bean gifted, presence of off farm incomes)
#Ethnic group maya 
data$ethnic_group <- as.numeric(data$ethnic_group=="maya")
#Gender of the HH
data$gender_interviewed <- as.numeric(data$gender_interviewed=="Female")
# Male are working in the bean field
data$bean_who_works_male <- as.numeric(data$bean_who_works_male=="Yes")
# Female control revenues
data$who_control_revenue_female <- as.numeric(data$who_control_revenue_female=="Yes")
# Bean gift
data$bean_gift <- as.numeric(data$bean_gift=="Yes")
# Off farm incomes
data$offfarm_incomes <- as.numeric((data$offfarm_incomes=="Yes"))
# Administrative level
data$adm1 <- factor(data$adm1)
data$adm2 <- factor(data$adm2)
data$adm3 <- factor(data$adm3)

data %>% mutate(
  Landrace_preferred = factor(LR_overIV,
                  levels=c("0","1"),
                  labels=c("No", "Yes") 
  )
) %>% ggplot(aes(factor(LR_overIV), fill=Landrace_preferred)) +
  geom_bar(aes(y = (..count..)/512),position="dodge", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("No"="#b2182b" , "Yes"="#2166ac")) +
  theme_classic() +
  xlab("Landrace variety preferred") + 
  ylab("Farmers (%)") +
  theme(legend.position="",
        legend.title = element_blank()) +
  #axis.title.y = element_text(angle = 0)) +
  labs(fill="Region of the study") -> fig1
fig1

ggplot(data=data, aes(x=factor(data$LR_overIV)))+
  geom_bar()

# Descriptive on the main dependent variable
table(data$LR_overIV)
test <- cor.test(data$LR_overIV, data$adm1, method = "pearson", use = "complete.obs")
test

# Poisson logistic descriptive exercise
m1 <- glm(LR_overIV ~ adm2, family = "poisson", data)
summary(m1)

# Visualizing the results in Latex
stargazer(m1)
