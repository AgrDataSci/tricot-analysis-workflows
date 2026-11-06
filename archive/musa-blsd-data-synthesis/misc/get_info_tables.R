lapply(list.files("scripts/", pattern = "_prep", full.names = TRUE, recursive = TRUE),
       source)



#number of genotypes in IMTP-1
genotypes_imtp_1 <- names(imtp_1_rank_mtx)
genotypes_imtp_1
nrow(imtp_1_rank_mtx)

#number of genotypes in IMTP-2
genotypes_imtp_2 <- names(imtp_2_rank_mtx)
genotypes_imtp_2
nrow(imtp_2_rank_mtx)


#number of genotypes in IMTP-3
genotypes_imtp_3 <- names(imtp_3_rank_mtx)
genotypes_imtp_3
nrow(imtp_3_rank_mtx)
#number of genotypes in Narvaez thesis
genotypes_narvaez <- names(narvaez_rank_mtx)
genotypes_narvaez
nrow(narvaez_rank_mtx)


#number of genotypes in MGIS 
genotypes_mgis_6 <- names(mgis_6_ranks_mtx)
genotypes_mgis_6
nrow(mgis_6_ranks_mtx)

genotypes_mgis_20 <- names(mgis_20_ranks_mtx)
genotypes_mgis_20
nrow(mgis_20_ranks_mtx)

#number of genotypes in BBB project
genotypes_bbb_1_3 <- names(bbb_ranks_1_3_mtx)

genotypes_bbb_4 <- names(bbb_ranks_4_mtx)

bbb_genotypes <- sort(unique(c(genotypes_bbb_1_3, genotypes_bbb_4)))

bbb_genotypes <- bbb_genotypes[!bbb_genotypes %in% c("lon", "lat", "pdate","end_date")]

bbb_genotypes
