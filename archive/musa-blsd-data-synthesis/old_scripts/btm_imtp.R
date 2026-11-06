library(gosset)
library(caret)
library(BradleyTerry2)
library(PlackettLuce)
# library("gtools")
# library("Kendall")
# library(irr)
# library(psychotree)
# library(dplyr)
# library(jtools)
# library(kableExtra)


#load trial and climate data
source("scripts/utils/load_data.R")

#remove variables with near zero variability
vars_to_remove <- caret::nearZeroVar(musa_trials_climate, names = TRUE)

musa_trials_climate <- musa_trials_climate[!colnames(musa_trials_climate) %in% vars_to_remove]

#First run the data preparation part on the pltree model script, to remove unconnected
#and underrepresented genotypes

musa_ranks <- as.rankings(musa_trials_data)

musa_binom <- rank_binomial(object = musa_ranks, disaggregate  = T)

#Model 0 - no covariates
btm0 <- BTm(cbind(win1, win2),  
            player1 = musa_binom$player1, 
            player2 = musa_binom$player2,
            data = musa_binom)
 
# summary.glm(btm0)
# summary.lm(btm0)
summary(btm0)

class(btm0) <- c("glm")

boxplot(btm0$R ~ names(btm0$coefficients), las = 2)
names(btm0$coefficients)

#From Jeske's Paper
output <- summary.glm(btm0)
# Retrieve the MLEs
alpha.hat=c(0,output$coeff[,1])
print(alpha.hat)
# Retrieve the variance-covariance matrix of MLEs
varcov.0=output$cov.unscaled
varcov.1=rbind(c(0,0,0,0),varcov.0)
varcov=cbind(c(0,0,0,0,0),varcov.1)
print(varcov)
#################################


bta_0 <- BTabilities(btm0)

qvc_0 <- qvcalc(bta_0)

plot(qvc_0, las = 2)

#Model selection 
# btp <- btpermute(contests = musa_binom,
#                  predictors = musa_trials_climate, 
#                  n.iterations = 2,
#                  seed = 123)

summary(btp$model)

btp$model$formula

#save(btp, file = "output/model/btpermute_2022_03_11.rda")



#to fit the model with formula from btpermute, items should be 
#stored in variable items
items <- unique(musa_binom$player1)

musa_btm_data <- vector(mode = "list", length = 3)

#genotypes <- as.data.frame(ifelse(musa_trials_data != 0, 1, 0))

musa_btm_data[[1]] <- musa_binom
musa_btm_data[[2]] <- musa_trials_climate
musa_btm_data[[3]] <- items



btm1 <- BTm(outcome = cbind(win1, win2),
            player1 = player1, 
            player2 = player2,
            formula = ~ .. + minNT[id] * items[..] + ~ .. + rhum_09[id] * items[..] + ~ .. + Rtotal[id] * items[..] , 
            data = musa_btm_data)

summary(btm1)

s <- summary.lm(btm1)

s$cov.unscaled


btm1$R

predict(btm1, type = "terms")

pheatmap::pheatmap(cor(btm1$coefficients))

#save(btm1, file = "output/model/btm1.rda")

btm1_results <- summary(btm1)

AIC(btm1)
AIC(btm0)

em <- qdrg(btm1,
           data = musa_btm_data)

btm1_results$coefficients
btm1_results$cov.unscaled

btm1_results[btm1_results$na.action]
str(btm1_results)

names(btm1_results$na.action)

rm(items2)
items2 <- items
musa_btm_data$items2 <- items2 

Bexp2 <- list(B, clim_dat[,c("SU", "CSDI")], items)

Bexp2[[2]]$minDT <- jitter(Bexp2[[2]]$minDT, 1)
Bexp2[[2]]$CSDI <- jitter(Bexp2[[2]]$CSDI,2)

B$player1
str(Bexp1)
str(flatlizards)

B$player2

Bexp1[[3]]

rownames(clim_dat)

exp <- left_join(B,clim_dat, by = c("id" = "id.1"))

Bexp <- Bexp1[[1]]
Bexp <- list(list(B, exp[,-c(2:4)]))

str(Bexp)




#covariates - no random effect
#OLD CODE
# btm1 <- BTm(outcome = cbind(musa_binom$win1, musa_binom$win2),
#             player1 = musa_binom$player1, 
#             player2 = musa_binom$player2,
#             formula = ~ .. + .. * SU + .. * CSDI,
#             data = musa_trials_climate,
#             refcat = "Calcutta 4",
#             br = T)



clim_vars <- paste0(colnames(musa_trials_climate[1:5]), "[id]")
gen_vars <- paste0(colnames(genotypes), "[..]")
#clim_vars_2 <- 
var_combinations <- expand.grid(clim_vars, gen_vars)
var_combinations_2 <- paste(paste(var_combinations$Var1, var_combinations$Var2, sep = " + "), collapse = " * ")
mus_form <- paste(" ~ ", var_combinations_2)

musa_form <- as.formula(mus_form)

btm1 <- BTm(outcome = cbind(win1, win2),
            player1 = player1, 
            player2 = player2,
            formula = musa_form, 
            data = musa_btm_data
            )

summary.glm(btm1)
summary(btm1)



genotypes


a <- aggregate(player1 ~ id, data = musa_binom, FUN = unique) 

musa_trials_data[unlist(a)]
musa_trials_data$genotype <- 

musa_btm_data 

data()



summary.glm(btm1)
summary(btm1)

plot(btm1)

bta_1 <- BTabilities(btm1), vcov=F ) 

qvc_1 <- qvcalc(bta_1, ref = NULL)

plot(qvc_1, las = 2)

qvs <- qvc_1

est <- qvs$qvframe$estimate
qvs2 <- qvs
qvs2$qvframe <- qvs$qvframe[order(est, decreasing = TRUE), , drop = FALSE]
plot(qvs2 ,las = 2)


pred <- predict(btm1), exp, type = "terms")




help(chameleons)



data("baseball")

baseball$home.team

is.list(baseball)

str(baseball)

chameleon.model <- BTm(player1 = winner, player2 = loser,
                       formula = ~ prev.wins.2 + ch.res[ID] + prop.main[ID] + (1|ID), id = "ID",
                       data = chameleons)

summary(chameleon.model)

chameleons

data("flatlizards")

flatlizards
ftl <- BTm(outcome = 1, player1 = winner, player2 = loser, formula = ~SVL[..] +
      (1 | ..), data = flatlizards)

head(BTabilities(chameleon.model))

data(CEMS)

CEMS



str(chameleons)
chameleons$winner
chameleons$loser
