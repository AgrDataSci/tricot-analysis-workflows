# Do different groups of users prefer different varieties?
# Status: tested-on-synthetic-data
# Decision record: decisions/socioeconomic-heterogeneity.md, decisions/ranking.md
# Derived from: archive/groundnut-tanzania/05-data-analysis-clusters.R,
#   archive/mip-concept-testing/02-clusters-identification.R,
#   archive/osiris-1000farms/03-clusters-identification.R,
#   AgrDataSci/ClimMob-analysis R/44_group_analysis.R (part A)
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R

library("PlackettLuce")
library("gosset")
library("cluster")
library("VarSelLCM")
library("FactoMineR")
library("qvcalc")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

dir.create("output", showWarnings = FALSE)

items = c("item_A", "item_B", "item_C")
input = c("overall_pos", "overall_neg")

# A. Predefined groups ====================================================
# Women and men, districts, age classes. The groups were designed into the
# study, so nothing is clustered. The questions are whether each group has
# enough answers, whether the group is confounded with site, and whether the
# groups differ once site is held fixed. Replace the simulation with your
# data, with one row per participant and the group and site columns
dat_a = simulate_known_groups(gender_effect = FALSE)

rk_a = build_rankings(dat_a, items, input)
R_a = rk_a$rankings
dat_a = dat_a[rk_a$valid, ]

# A1. Enough answers per group? Groups below 20 valid answers are not
# compared, unlabelled answers never are
elig = group_eligibility(dat_a$gender, min_n = 20)
print(elig, digits = 2)

# A2. Is the group confounded with site?
conf = confounding_check(dat_a$gender, dat_a$site)
print(conf$counts)
cat("Cramer's V between gender and site:", round(conf$cramers_v, 2), "\n")

# A3. Pooled over sites, then within sites. Only the stratified test speaks
# to gender. The pooled test is shown to make the confounding visible
naive = preference_heterogeneity(R_a, dat_a$gender, nperm = 200)
strat = stratified_heterogeneity(R_a, dat_a$gender, dat_a$site, nperm = 200)
print(rbind(pooled_over_sites = naive[c("deviance", "df", "p_chisq", "p_permutation")],
            within_sites = strat[c("deviance", "df", "p_chisq", "p_permutation")]),
      digits = 3)
cat("Strata used:", strat$strata_used, " Strata left out:", strat$strata_dropped, "\n")

# A4. Worth per group with quasi-standard errors and n, agreement between
# groups, and the balanced ranking when one group is a small minority
wi = worth_with_intervals(R_a, dat_a$gender, ref = "Check")
print(wi, digits = 2)
print(agreement_between_groups(R_a, dat_a$gender), digits = 2)
if (min(elig$share, na.rm = TRUE) < 0.25) print(balanced_ranking(R_a, dat_a$gender), digits = 2)

utils::write.csv(elig, "output/group-eligibility.csv", row.names = FALSE)
utils::write.csv(as.data.frame(conf$counts), "output/group-by-site.csv", row.names = FALSE)
utils::write.csv(rbind(cbind(test = "pooled_over_sites", naive[c("deviance", "df", "p_chisq", "p_permutation")]),
                       cbind(test = "within_sites", strat[c("deviance", "df", "p_chisq", "p_permutation")])),
                 "output/group-heterogeneity.csv", row.names = FALSE)
utils::write.csv(wi, "output/worth-by-group.csv", row.names = FALSE)

# B. User types from covariates ===========================================

# 1. Read data ------------------------------------------------------------
dat = simulate_tricot()

covars = c("farm_size", "share_sold", "market_distance", "household_head")

rk = build_rankings(dat, items, input)
segment = attr(dat, "segment")[rk$valid]
dat = dat[rk$valid, ]
R = rk$rankings

# 2. Clusters from covariates only ----------------------------------------
# All covariates enter, including the binary household head. Skewed
# covariates are log-transformed so a few large farms do not dominate
X = dat[covars]
X$farm_size = log(X$farm_size)
X$market_distance = log(X$market_distance)

# 2a. Default, model-based clustering with variable selection. Variables
# unrelated to the clusters are declared irrelevant and do not shape them
vs = cluster_varsel(X, k = 2:5)
print(vs$criteria, digits = 5)
cat("Chosen k by", vs$criterion, ":", vs$k, "\n")
cat("Relevant variables:", paste(vs$relevant, collapse = ", "), "\n")
cat("Irrelevant variables:", paste(vs$irrelevant, collapse = ", "), "\n")

group = vs$labels
cat("Smallest cluster share:", round(min(table(group)) / length(group), 2), "\n")

# 2b. Gower distance and PAM, with the stability search, for comparison
cs = cluster_search(X, k = 2:5, nboot = 30)
print(cs$criteria, digits = 2)
gower = cs$labels[[as.character(if (is.na(cs$k)) 2 else cs$k)]]

# 2c. FAMD components (eigenvalue above 1) and PAM, for comparison
sc = famd_scores(X)
cat("FAMD eigenvalues:", round(attr(sc, "eigenvalues"), 2), "\n")
cs_f = cluster_search(sc, k = 2:5, nboot = 30, metric = "euclidean")
famd = cs_f$labels[[as.character(if (is.na(cs_f$k)) 2 else cs_f$k)]]

# 2d. Which variables drive each partition? A partition that reproduces a
# single variable, or collapses when it is left out, is flagged
infl_gower = suppressWarnings(variable_influence(X, gower, function(x) {
  cluster::pam(cluster::daisy(x, metric = "gower"), k = length(unique(gower)),
               diss = TRUE, cluster.only = TRUE)
}))
print(infl_gower, digits = 2)

# agreement of each method with the simulated segments and with household
# head, only possible with synthetic data
comparison = data.frame(
  method = c("VarSelLCM (default)", "Gower + PAM", "FAMD + PAM"),
  k = c(vs$k, length(unique(gower)), length(unique(famd))),
  ari_segments = c(adjusted_rand(vs$labels, segment),
                   adjusted_rand(gower, segment),
                   adjusted_rand(famd, segment)),
  ari_household_head = c(adjusted_rand(vs$labels, dat$household_head),
                         adjusted_rand(gower, dat$household_head),
                         adjusted_rand(famd, dat$household_head)))
print(comparison, digits = 2)

print(cluster_profile(dat[covars], group), digits = 2)

# The profile, the worth per cluster and the cluster criteria (written to
# output/ below) can be given to an AI assistant to draft user personas, or
# TPE descriptions for environmental clusters. Follow the rules in the README
# section "From clusters to personas or TPE descriptions". Every statement
# must trace to these numbers, and images must be AI-generated and labelled,
# never real farmers' photos

# 3. Does preference differ between clusters? -----------------------------
het = preference_heterogeneity(R, group, nperm = 200)

print(het, digits = 3)

# which pairs of clusters differ? Clusters that do not differ in preference
# need no separate recommendation, even when their profiles differ
print(pairwise_heterogeneity(R, group), digits = 3)

# 4. Worth per cluster ----------------------------------------------------
wg = worth_by_group(R, group, ref = "Check")

print(wg, digits = 2)


# 5. Outputs --------------------------------------------------------------
utils::write.csv(vs$criteria, "output/cluster-criteria.csv", row.names = FALSE)
utils::write.csv(comparison, "output/method-comparison.csv", row.names = FALSE)
utils::write.csv(cluster_profile(dat[covars], group), "output/cluster-profile.csv", row.names = FALSE)
utils::write.csv(het, "output/preference-heterogeneity.csv", row.names = FALSE)
utils::write.csv(wg, "output/worth-by-cluster.csv", row.names = FALSE)

sessionInfo()
