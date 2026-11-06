library(glmertree)
library(party)
library(rpart)
library(rpart.plot)
library(vip)
library(ggplot2)
library(ggparty)

yls_data_imtp1_all$envID <- as.integer(as.factor(yls_data_imtp1_all$trial_r))

imtp_agera5_temp_indices$id <- 1:nrow(imtp_agera5_temp_indices)

imtp_agera5_prec_ind$id <- 1:nrow(imtp_agera5_prec_ind)


yls_data_imtp1_all <- inner_join(yls_data_imtp1_all, imtp_agera5_temp_indices, by = c("envID"="id"))

yls_data_imtp1_all <- inner_join(yls_data_imtp1_all, imtp_agera5_prec_ind, by = c("envID"="id"))

yls_data_imtp1_all

View(yls_data_imtp1_all)

#imtp2

yls_data_imtp2_all$envID <- as.integer(as.factor(yls_data_imtp2_all$trial_r)) + 6

yls_data_imtp2_all <- inner_join(yls_data_imtp2_all, imtp_agera5_temp_indices, by = c("envID"="id"))

yls_data_imtp2_all <- inner_join(yls_data_imtp2_all, imtp_agera5_prec_ind, by = c("envID"="id"))

yls_data_imtp2_all




yls_data_imtp3_blocks$envID <- as.integer(as.factor(yls_data_imtp3_blocks$trial_r)) + 19

yls_data_imtp3_all <- inner_join(yls_data_imtp3_blocks, imtp_agera5_temp_indices, by = c("envID"="id"))

yls_data_imtp3_all <- inner_join(yls_data_imtp3_blocks, imtp_agera5_prec_ind, by = c("envID"="id"))

yls_data_imtp3_all

lmert1 <- lmertree(mean_yls ~ genotypes |block|1  , data = yls_data_imtp3_all,
                   verbose = T
) 
lmert1$lmer

plot(lmert1)


lmt_1 <- partykit::lmtree(youngest_leaf_spotted ~ as.factor(genotypes) + , data = yls_data_imtp1_all)

plot(lmt_1)

yls_imtp_all <- bind_rows(yls_data_imtp1_all,yls_data_imtp2_all,yls_data_imtp3_all)

yls_imtp_all <- correct_names(yls_imtp_all)


yls_imtp_all$genotypes <- as.factor(yls_imtp_all$genotypes)

unique(yls_imtp_all$genotypes)

yls_imtp_all$genotypes <- as.character(yls_imtp_all$genotypes)

yls_imtp_all <- correct_names(yls_imtp_all)

yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "Local", ]

yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "EcuadorDwarf", ]

yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "Ingagara", ]

yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "I-63", ]

yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "Tani", ]
yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "Cardaba", ]
yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "Cachaco", ]
yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "Falso Cuerno", ]
yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "Pahang", ]
yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "Lakatan", ]
yls_imtp_all <- yls_imtp_all[yls_imtp_all$genotypes != "SF 215 / NBA 14", ]
  
yls_imtp_all[yls_imtp_all$genotypes == "SF 215 / NBA 14", ]

unique(yls_imtp_all$genotypes)

yls_imtp_all$genotypes <- as.factor(yls_imtp_all$genotypes)
unique(yls_imtp_all$genotypes)

yls_imtp_all %>% 
  group_by(genotypes)


lmt_1 <- partykit::lmtree(mean_yls ~  genotypes + maxDT +  minDT + maxNT +  minNT + 
                     DTR + SU + TR + CFD + WSDI + CSDI + T10p + T90p + MLDS +
                     MLWS + R10mm + R20mm + Rx1day + Rx5day + R95p + R99p + Rtotal + SDII +
                     altitude,
                     data = yls_imtp_all,
                     verbose = T,
                     alpha = 0.05,
                     bonferroni = T
                     ) 

lmert1

plot(lmert1)
nrow(yls_imtp_all)

## artificial example data
data("DepressionDemo", package = "glmertree")

## fit normal linear regression LMM tree for continuous outcome
lt <- lmertree(depression ~ treatment | cluster | age + anxiety + duration,
               data = DepressionDemo)
print(lt)
plot(lt, which = "all") # default behavior, may also be "tree" or "ranef" 
coef(lt)
ranef(lt)
predict(lt, type = "response") # default behavior, may also be "node"
predict(lt, re.form = NA) # excludes random effects, see ?lme4::predict.merMod
residuals(lt)
VarCorr(lt) # see lme4::VarCorr



ctree_1 <- party::ctree(mean_yls ~ genotypes + maxDT +  minDT + maxNT +  minNT + DTR + SU + TR + CFD + WSDI + CSDI + T10p + T90p + MLDS +
                             MLWS + R10mm + R20mm + Rx1day + Rx5day + R95p + R99p + Rtotal + SDII + altitude + as.factor(location_name), data = yls_imtp_all)

plot(ctree_1, gp = gpar(fontsize = 1))


ctree_2 <- party::ctree(mean_yls ~ genotypes + maxDT +  minDT + maxNT +  minNT + DTR + SU + TR + CFD + WSDI + CSDI + T10p + T90p + MLDS +
                          MLWS + R10mm + R20mm + Rx1day + Rx5day + R95p + R99p + Rtotal + SDII + altitude , data = yls_imtp_all)

plot(ctree_2, gp = gpar(fontsize = 1))






vip(ctree_1)

autoplot(ctree_1)

party(ctree_1)
ggparty::ggparty(ctree_1)

str(ctree_1)
ctree_1[]

plot(ctree_1, gp = gpar(fontsize = 0))

rpart_1 <- rpart(mean_yls ~ genotypes + maxDT +  minDT + maxNT +  minNT + DTR + SU + TR + CFD + WSDI + CSDI + T10p + T90p + MLDS +
                   MLWS + R10mm + R20mm + Rx1day + Rx5day + R95p + R99p + Rtotal + SDII + altitude + as.factor(location_name), data = yls_imtp_all)

rpart.plot::rpart.plot(rpart_1)

autoplot(as.party(rpart_1))

vip(rpart_1, num_features = 13, geom = "point")
