library("gosset")
library("PlackettLuce")
library("psychotree")

load("data/imtp_trials_exp2.rda")

# check the n for each sampled genotype
table(imtp_all$genotypes)
sort(table(imtp_all$genotypes))

# Niyarma Yik and Nyarma Yik seems to be the same genotype 
# put all with the same same
imtp_all$genotypes[imtp_all$genotypes == "Nyarma Yik"] <- "Niyarma Yik"

table(imtp_all$genotypes)
sort(table(imtp_all$genotypes))

# remove those that were sampled once
rmv <- table(imtp_all$genotypes)
rmv <- rmv[rmv < 2]
rmv <- names(rmv)
rmv

imtp_all <- imtp_all[!imtp_all$genotypes %in% rmv, ]

sort(table(imtp_all$genotypes))

# now into PL rankings
R <- rank_numeric(data = imtp_all, 
                  items = "genotypes", 
                  input = "mean_yls", 
                  id = "trial_r", 
                  group = FALSE)

#check connectivity
connectivity(R)

# into paircomp
P <- rank_paircomp(R)

# model without covariates
mod1 <- bttree(P ~ 1, data = P)

coefficients(mod1)

sort(coefficients(mod1))

boxplot.stats(coefficients(mod1))$out

plot(mod1)

# a very high coefficient for Tuu Gia and GCTCV-215, 
# this may be some kind of bias 
# remove it to check if solves
imtp_all <- imtp_all[imtp_all$genotypes != "Tuu Gia",]

R <- rank_numeric(data = imtp_all, 
                  items = "genotypes", 
                  input = "mean_yls", 
                  id = "trial_r", 
                  group = FALSE)

# into paircomp
P <- rank_paircomp(R)

# model without covariates
mod2 <- bttree(P ~ 1, data = P)

plot(mod2)

sort(coefficients(mod2))

boxplot.stats(coefficients(mod2))$out

summary(mod2)

# again with Pahang
imtp_all <- imtp_all[imtp_all$genotypes != "Pahang",]

# now into PL rankings
R <- rank_numeric(data = imtp_all, 
                  items = "genotypes", 
                  input = "mean_yls", 
                  id = "trial_r", 
                  group = FALSE)
#check connectivity
connectivity(R)

# into paircomp
P <- rank_paircomp(R)

# model without covariates
mod3 <- bttree(P ~ 1, data = P)

plot(mod3)

sort(coefficients(mod3))

summary(mod3)

# again with PV42320
imtp_all <- imtp_all[imtp_all$genotypes != "PV42320",]

# now into PL rankings
R <- rank_numeric(data = imtp_all, 
                  items = "genotypes", 
                  input = "mean_yls", 
                  id = "trial_r", 
                  group = FALSE)
#check connectivity
connectivity(R)

# into paircomp
P <- rank_paircomp(R)

# model without covariates
mod4 <- bttree(P ~ 1, data = P)

mod4

plot(mod4)

dat <- cbind(P, exp2)

bttree(P ~., data = dat, minsize = 5, alpha = 0.5)
