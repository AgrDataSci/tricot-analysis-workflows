library(partykit)

narita_data$env_name <- ifelse(grepl(pattern = "Ka", x = narita_data$Envnt),"Kawanda",
                                 ifelse(grepl(pattern = "Mb", x = narita_data$Envnt), "Mbarara",
                                        "Mitalula"))


narita_data_2 <- merge(x = narita_data,
                       y = bbb_loc,
                       by.x = "env_name",
                       by.y = "Site")


narita_data_2

narita_data_2$pdate <- as.Date(narita_data_2$pdate, format = "%m/%d/%Y")

narita_data_2$end_date <- narita_data_2$pdate + narita_data_2$Days_planting_to_harvest_mean

narita_data_2 <- narita_data_2[!is.na(narita_data_2$end_date), ]

nrow(narita_data_2)

narita_data_g2

prec_bbb_ind_2 <- cbind(narita_data_g2, prec_bbb_ind)

prec_bbb_2 <- agera5::get_prec.dataset(.trial_dataset = narita_data_2,
                                       .start_date = "pdate",
                                       .end_date = "end_date",
                                       .lon = "Longitude",
                                       .lat = "Latitude",
                                       .agera5_folder = "D:/Dropbox (Bioversity CR)/env_data/agera5/")



narita_data_3 <- merge(x = narita_data_2,
                       y = prec_bbb_ind_2,
                       by.x = c("Longitude", "Latitude"),
                       by.y = c("V2", "V1"))

narita_data_3$Bunch_Yield_t_per_Ha_per_Yr <- as.numeric(narita_data_3$Bunch_Yield_t_per_Ha_per_Yr)

narita_data_3 <- narita_data_3[!is.na(narita_data_3$Bunch_Yield_t_per_Ha_per_Yr),]

narita_data_3 <- narita_data_3[narita_data_3$genotypes != "NARITA 25",]

unique(narita_data_3$genotypes)

sort(table(narita_data_3$genotypes))

ct_bbb <- partykit::ctree(formula = mean_blsd  ~ MLDS + MLWS + R10mm + R20mm + Rx1day  + Rx5day +  R95p +  R99p + Rtotal +SDII + as.factor(genotypes) , 
                data = narita_data_3)
plot(ct_bbb)

library(ggparty)
autoplot(ct_bbb)
