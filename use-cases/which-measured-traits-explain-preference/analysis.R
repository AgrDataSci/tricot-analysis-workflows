# Which measured traits explain preference?
# Status: tested-on-synthetic-data
# Decision record: decisions/trait-associations.md, decisions/genetic-gain.md
# Derived from: archive/rtbfoods-eba-nigeria (02_estimate_blups.R, 03_pladmm.R),
#   archive/teff-elife/01_farmers_oa.R
#
# Run from this folder with  Rscript analysis.R

library("PlackettLuce")
library("gosset")
library("lme4")

source("functions.R")
source("data/simulate-data.R")

dir.create("outputs", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
sim = simulate_trait_study()
lab = sim$lab
consumers = sim$consumers

options = c("option_a", "option_b", "option_c")

R = rank_tricot(consumers, options, c("best_taste", "worst_taste"),
                validate.rankings = TRUE)

# 2. Genotype names must match --------------------------------------------
common = check_genotype_names(unlist(consumers[options]), lab$genotype)
cat("Genotypes in both data sets:", length(common), "\n")

# 3. Genotype estimates per trait -----------------------------------------
traits_all = c("hardness", "cohesiveness", "springiness", "colour_l",
               "soluble", "gumminess", "chewiness")

te = trait_estimates(lab, traits_all, region = "region")

print(te$repeatability, digits = 2)

# 4. Covariates, collinearity and the choice of traits --------------------
cov_all = covariate_table(te, "blue")

cc_all = collinearity_check(cov_all)
print(round(cc_all$vif, 1))
print(cc_all$pairs, digits = 2)

# gumminess and chewiness are products of the other texture traits, so they
# are dropped and the component traits kept. Soluble content is kept, its
# low repeatability is reported with the result
traits = c("hardness", "cohesiveness", "springiness", "colour_l", "soluble")

cov = covariate_table(te, "blue")[c("genotype", traits)]
cc = collinearity_check(cov)
print(round(cc$vif, 1))

# 5. Preference as a function of measured traits --------------------------
mod = fit_trait_model(R, cov, traits)

eff = trait_effects(mod)
print(eff, digits = 2)

print(trait_model_adequacy(R, mod), digits = 3)

# BLUPs against BLUEs. After standardization in a balanced design they give
# the same model. On the raw scale, as when BLUPs enter pladmm without
# standardization, shrinkage inflates the coefficient of poorly measured
# traits such as soluble content
mod_blup = fit_trait_model(R, covariate_table(te, "blup")[c("genotype", traits)], traits)

raw = function(type) {
  stats::coef(fit_trait_model(R, covariate_table(te, type, standardize = FALSE)[c("genotype", traits)], traits))[traits]
}

print(data.frame(trait = traits,
                 standardized_blue = stats::coef(mod)[traits],
                 standardized_blup = stats::coef(mod_blup)[traits],
                 raw_blue = raw("blue"),
                 raw_blup = raw("blup"),
                 row.names = NULL), digits = 3)

# 6. Stability under measurement error ------------------------------------
sens = covariate_sensitivity(R, te, traits, ndraw = 50)
print(sens, digits = 2)

# 7. Outputs --------------------------------------------------------------
utils::write.csv(te$estimates, "outputs/trait-estimates.csv", row.names = FALSE)
utils::write.csv(te$repeatability, "outputs/trait-repeatability.csv", row.names = FALSE)
utils::write.csv(eff, "outputs/trait-effects.csv", row.names = FALSE)
utils::write.csv(sens, "outputs/trait-effects-sensitivity.csv", row.names = FALSE)

sessionInfo()
