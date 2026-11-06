library(rpart)
library(rpart.plot)
library(rattle)

imtp_all <- bind_rows(yls_data_imtp1_all,
                      yls_data_imtp2_all,
                      yls_data_imtp3_blocks)

mob <- mob(youngest_leaf_spotted ~   genotypes|trial_r + MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day  +
                       R95p  + R99p  + Rtotal +  SDII + 
                       maxDT + minDT + maxNT + minNT +  DTR  +  SU  +  TR +  CFD +  WSDI + CSDI + T10p + T90p,
                      
                     data = imtp_all)



rpart_imtp1 <- rpart(formula = youngest_leaf_spotted ~ genotypes, data = yls_data_imtp1_all)

rpart_imtp1 <- rpart(youngest_leaf_spotted ~  genotypes + MLDS + MLWS + R10mm + R20mm + Rx1day + Rx5day  +
                       R95p  + R99p  + Rtotal +  SDII + 
                       maxDT + minDT + maxNT + minNT +  DTR  +  SU  +  TR +  CFD +  WSDI + CSDI + T10p + T90p,
                     method = "anova",
                     data = imtp_all)

rattle::

rpart_imtp1

rsq.rpart(rpart_imtp1)

rpart.plot::rpart.plot(rpart_imtp1)

rpart_imtp1_pk <- as.party.rpart(rpart_imtp1)
plot(rpart_imtp1_pk)


partykit::plot.modelparty(rpart_imtp1_pk, )
fancyRpartPlot(rpart_imtp1)

colnames(yls_data_imtp1_all)

par(xpd = TRUE)
plot(rpart_imtp1)
text(rpart_imtp1, use.n = TRUE)
par(mar )
rpart.plot(x = rpart_imtp1, type = 3, extra = 100)
printcp(rpart_imtp1)
plotcp(rpart_imtp1)

rpart_imtp2 <- rpart(formula = youngest_leaf_spotted ~ genotypes, data = yls_data_imtp2_all)

rpart_imtp2

rpart.plot::rpart.plot(rpart_imtp2, type = 1,extra = "auto",)



rpart_imtp3 <- 


