# Data

`simulate-data.R` generates station and on-farm data for the same 15 genotypes. Each genotype has a genetic value with variance 1. Station yield adds location, replicate, genotype-by-location (variance 0.3) and residual (variance 1) effects over 3 locations with 3 replicates, which gives a true entry-mean H2 of about 0.83. On-farm log-worth is `b * g + d`, where `d` (variance 0.25) is what farmers value beyond station yield. With `b = 0` farmer preference is unrelated to station performance. The true values are in `sim$truth`.
