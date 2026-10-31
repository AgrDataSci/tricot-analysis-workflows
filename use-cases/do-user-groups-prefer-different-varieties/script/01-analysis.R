# Do different groups of users prefer different varieties?
# Status: tested-on-synthetic-data
# Decision record: decisions/socioeconomic-heterogeneity.md, decisions/ranking.md
# Derived from: archive/groundnut-tanzania/05-data-analysis-clusters.R,
#   archive/mip-concept-testing/02-clusters-identification.R,
#   archive/osiris-1000farms/03-clusters-identification.R
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R

library("PlackettLuce")
library("gosset")
library("cluster")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

dir.create("output", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
dat = simulate_tricot()

items = c("item_A", "item_B", "item_C")
input = c("overall_pos", "overall_neg")
covars = c("farm_size", "share_sold", "market_distance", "household_head")

rk = build_rankings(dat, items, input)
segment = attr(dat, "segment")[rk$valid]
dat = dat[rk$valid, ]
R = rk$rankings

# 2. Clusters from covariates only ----------------------------------------
# Choose the clustering covariates deliberately. Skewed covariates are
# log-transformed so a few large farms do not dominate. Binary factors are
# left out of the distance, because Gower distance gives a binary factor full
# weight and the partition then reproduces it (see the tests). Household
# head is used afterwards to describe the clusters
X = dat[c("farm_size", "share_sold", "market_distance")]
X$farm_size = log(X$farm_size)
X$market_distance = log(X$market_distance)

cs = cluster_search(X, k = 2:5, nboot = 30)

print(cs$criteria, digits = 2)
cat("Chosen k:", cs$k, "\n")

if (is.na(cs$k)) {
  stop("no defensible cluster solution, report this rather than force one")
}

group = cs$labels[[as.character(cs$k)]]

print(cluster_profile(dat[covars], group), digits = 2)

# 3. Does preference differ between clusters? -----------------------------
het = preference_heterogeneity(R, group, nperm = 200)

print(het, digits = 3)

# 4. Worth per cluster ----------------------------------------------------
wg = worth_by_group(R, group, ref = "Check")

print(wg, digits = 2)

# agreement with the simulated segments, only possible with synthetic data
cat("Adjusted Rand index with the true segments:",
    round(adjusted_rand(group, segment), 2), "\n")

# 5. Outputs --------------------------------------------------------------
utils::write.csv(cs$criteria, "output/cluster-criteria.csv", row.names = FALSE)
utils::write.csv(cluster_profile(dat[covars], group), "output/cluster-profile.csv", row.names = FALSE)
utils::write.csv(het, "output/preference-heterogeneity.csv", row.names = FALSE)
utils::write.csv(wg, "output/worth-by-cluster.csv", row.names = FALSE)

sessionInfo()
