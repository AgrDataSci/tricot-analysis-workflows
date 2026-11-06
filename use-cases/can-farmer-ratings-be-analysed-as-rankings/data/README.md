# Data

`script/helper/simulate-data.R` generates 48 farmers, 24 women and 24 men, at two locations, each scoring 30 of 40 genotypes for overall appreciation on a scale from 1 to 5. Each farmer has their own shift and spread on the scale, so scores are only comparable within farmer. Two latent traits drive the scores. Men order genotypes by trait A, women by A and B together, and location has no effect. "Reference" has the highest value of A. The 39 lines belong to 8 families, used to show aggregation. The truth is in `sim$truth`.

To use your own data, place it in this folder and document it here (scale, question asked, who scored, replicate structure, source and licence), with one table of ratings (rater, item, score, plus replicate and location when they exist) and one of rater covariates. Then replace the simulation call in `script/01-analysis.R` with calls that read them. With replicates, treat each farmer by replicate as a rater, as the original script did.
