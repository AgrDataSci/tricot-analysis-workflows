library(randomForest)


randomForest::randomForest(mean_yls ~ genotypes + maxDT +  minDT + maxNT +  minNT + 
                              DTR + SU + TR + CFD + WSDI + CSDI + T10p + T90p + MLDS +
                              MLWS + R10mm + R20mm + Rx1day + Rx5day + R95p + R99p + Rtotal + SDII,
                            data = yls_imtp_all)



yls_imtp_all <- yls_imtp_all[!is.na(yls_imtp_all$genotypes), ]


yls_imtp_all <- yls_imtp_all[!is.na(yls_imtp_all$genotypes), ]

View(yls_imtp_all)
