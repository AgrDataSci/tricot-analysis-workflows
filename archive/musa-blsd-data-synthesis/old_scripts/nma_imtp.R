library(readr)
library(metafor)



imtp2 <- read_csv("data/agtrials_banana/clean/imtp/4meta-analysis/imtp_4meta-analysis.csv")


imtp2_mean_trial <- aggregate(x = imtp2$yls_mean, by = list(imtp2$trial_id), FUN = mean)


imtp2[imtp2$trial_id == 2935 & imtp2$, ]$yls_sd <- imtp2[imtp2$trial_id == 2935, ]$yls_cv * imtp2_mean_trial[1, 2]


data_imtp1

escalc(measure = "OR", )
yls_data_imtp2_all

help(dat.bcg)


metafor::di

### copy data into 'dat'
dat <- dat.berkey1998

### construct list of the variance-covariance matrices of the observed outcomes for the studies
V <- lapply(split(dat[c("v1i", "v2i")], dat$trial), as.matrix)
V


res.list <- lmList(distance ~ age, data=Orthodont)

################################


yls_data_imtp3_blocks

res_list <- lme4::lmList(mean_yls ~ 1 | trial, data = yls_data_imtp3_blocks)
res_list

b <- lapply(res_list, coef)
V <- lapply(res_list, vcov)

b
V

estm <- rep(c("intercept","slope"), length(b))
trial <- rep(names(b), each=2)

b
V

b <- unlist(b)
V <- bldiag(V)

b
V

res2 <- rma.mv(b ~ estm - 1, V, random = ~ estm | trial, struct="UN")
res2











