library(stablelearner)

b <- bootstrap(B = 1000, v = 1)

s <- subsampling(B = 1000, v = 0.8)

#number of rankings/data points in the banana dataset
n <- 94

b_1 <- b$sampler(n)

b_1[,1]

duplicated(b_1[, 1])

s_1 <- s$sampler(94)

s_1[, 1]

duplicated(s_1[,1])
