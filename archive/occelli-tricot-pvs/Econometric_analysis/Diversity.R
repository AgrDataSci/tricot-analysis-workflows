# PARTICIPATIVE APPROACH AND AGROBIODIVERSITY

#Packages and Libraries
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)

#Get the working directory where diversity data are
setwd("~/GitHub/socioeco-tricot/data/Diversity Index")
getwd()
# Call the file for baseline data
trif_div <- read_excel("trifinio_diversity.xlsx")
View(trif_div)

#Explore treatment and diversity
table(trif_div$treat)
str(trif_div$treat)
str(trif_div$M_end)
#CCI=37% (42%) Control=43% (34%) and PVS=20% (17%)

#Outlier detection - Cook's distance
mod <- lm(as.numeric(M_end) ~ as.factor(treat), data = trif_div)
summary(mod)
cooksd <- cooks.distance(mod)
#Plot Cook Distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
head(trif_div[influential, ])
# Test from the car package
car::outlierTest(mod)

# Boxplot representation of the Mergalef and count index according to treatment (END)
ggplot(trif_div, aes(x=as.factor(treat), y=as.numeric(Count_end)))+
  geom_boxplot()

ggplot(data = trif_div, aes(x=as.numeric(Count_end))) +
  geom_bar(aes(y = (..count..)/1596),position="dodge", size=4) +
  geom_vline(aes(xintercept = median(as.numeric(Count_end), na.rm = T)))+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~ as.factor(treat))

trif_div %>% mutate(
  treat = factor(treat,
                levels=c("1","2", "3"),
                labels=c("Control", "PSV", "Tricot") 
  )
) %>%
ggplot(aes(x=as.numeric(M_end3), fill=treat)) +
  geom_histogram(aes(y = (..count..)/2098),position="dodge", size=4, binwidth = 0.7) +
  geom_vline(aes(xintercept = median(as.numeric(M_end3), na.rm = T)))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Control"="#7570b3" , "PSV"="#d95f02", "Tricot"="#1b9e77")) +
  facet_wrap(~ as.factor(treat))+
  #theme_classic() +
  xlab("Mergalef Index (bean)") + 
  ylab("Farmers (%)") +
  theme(legend.position="",
        legend.title = element_blank())

# Boxplot representation of the Mergalef and count index according to treatment (BASE)
ggplot(trif_div, aes(x=as.factor(treat), y=as.numeric(M_base3)))+
  geom_boxplot()

ggplot(data = trif_div, aes(x=as.numeric(base_cropcount))) +
  geom_histogram(aes(y = (..count..)/2098),position="dodge", size=4, binwidth = 0.3) +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~ as.factor(treat))

trif_div %>% mutate(
  treat = factor(treat,
                 levels=c("1","2", "3"),
                 labels=c("Control", "PSV", "Tricot") 
  )
) %>%
  ggplot(aes(x=as.numeric(M_base3), fill=treat)) +
  geom_histogram(aes(y = (..count..)/1673),position="dodge", size=4, binwidth = 0.7) +
  geom_vline(aes(xintercept = median(as.numeric(M_base3), na.rm = T)))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Control"="#7570b3" , "PSV"="#d95f02", "Tricot"="#1b9e77")) +
  facet_wrap(~ as.factor(treat))+
  #theme_classic() +
  xlab("Mergalef Index (bean)") + 
  ylab("Farmers (%)") +
  theme(legend.position="",
        legend.title = element_blank())


# Call the file for endline data
end <- read_csv("trifinio_endline_survey_1013.csv")
#Explore RCT composition
table(end$`SECTION_META/continuation/beneficiary_consent/Interview/group_regalar_semillas/regalado_semillas`)
