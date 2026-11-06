#load all data
lapply(list.files("scripts/", pattern = "_prep", full.names = TRUE, recursive = TRUE),
       source)

min(imtp_1_rank_mtx$pdate)
max(imtp_1_rank_mtx$end_date)

min(imtp_2_rank_mtx$pdate)
max(imtp_2_rank_mtx$end_date)

min(imtp_3_rank_mtx$pdate)
max(imtp_3_rank_mtx$end_date)

min(narvaez_rank_mtx$pdate)
max(narvaez_rank_mtx$end_date)

mgis_locations


min(bbb_ranks_1_3_mtx$pdate)
min(bbb_ranks_4_mtx$pdate)

max(bbb_ranks_1_3_mtx$end_date)
max(bbb_ranks_4_mtx$end_date)




min([mgis_locations$trial_id == ]$pdate)
max(imtp_1_rank_mtx$end_date)

imtp_2_rank_mtx[time_loc_cols]
imtp_3_rank_mtx[time_loc_cols]
narvaez_rank_mtx[time_loc_cols]
mgis_ranks_mtx[time_loc_cols]
bbb_ranks_1_3_mtx[time_loc_cols]
bbb_ranks_4_mtx[time_loc_cols]


musa_trials_data

metric_x_ranking <- data.frame("trial" = row.names(musa_trials_data),
                               "YLS" = rep(0, nrow(musa_trials_data)),
                               "DDT" = rep(0, nrow(musa_trials_data)),
                               "DI" = rep(0, nrow(musa_trials_data)))


metric_x_ranking

#write.csv(x = metric_x_ranking, file = "data/processed/metric_x_ranking.csv")




