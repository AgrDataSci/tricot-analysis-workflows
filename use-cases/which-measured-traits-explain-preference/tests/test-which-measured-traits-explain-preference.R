# Tests for the use case "Which measured traits explain preference?"

library("testthat")

options = c("option_a", "option_b", "option_c")
traits = c("hardness", "cohesiveness", "springiness", "colour_l", "soluble")

sim = simulate_trait_study()
R = gosset::rank_tricot(sim$consumers, options, c("best_taste", "worst_taste"),
                        validate.rankings = TRUE)
te = trait_estimates(sim$lab, c(traits, "gumminess", "chewiness"), region = "region")

test_that("estimates are matched to genotypes by name", {
  # drop one genotype for one trait, the others must stay aligned
  lab = sim$lab
  lab$hardness[lab$genotype == "G03"] = NA
  e = trait_estimates(lab, c("hardness", "colour_l"), region = "region")$estimates
  h = e[e$trait == "hardness", ]
  expect_false("G03" %in% h$genotype)
  truth = sim$truth$genotype
  expect_gt(stats::cor(h$blue, truth$hardness[match(h$genotype, truth$genotype)]), 0.95)
})

test_that("BLUEs recover the simulated genotype values", {
  w = covariate_table(te, "blue")
  truth = sim$truth$genotype[match(w$genotype, sim$truth$genotype$genotype), ]
  expect_false(anyNA(w))
  for (v in c("hardness", "cohesiveness", "colour_l")) {
    expect_gt(stats::cor(w[[v]], truth[[v]]), 0.95)
  }
})

test_that("low repeatability is reported and BLUPs shrink that trait most", {
  r = stats::setNames(te$repeatability$repeatability, te$repeatability$trait)
  expect_lt(r["soluble"], r["hardness"])
  e = te$estimates
  ratio = function(v) stats::sd(e$blup[e$trait == v]) / stats::sd(e$blue[e$trait == v])
  expect_lt(ratio("soluble"), ratio("hardness"))
})

test_that("standardized BLUPs equal BLUEs in a balanced design, raw ones do not", {
  cov_blue = covariate_table(te, "blue")[c("genotype", traits)]
  cov_blup = covariate_table(te, "blup")[c("genotype", traits)]
  m1 = fit_trait_model(R, cov_blue, traits)
  m2 = fit_trait_model(R, cov_blup, traits)
  expect_equal(stats::coef(m1), stats::coef(m2), tolerance = 1e-2)
  raw_blue = covariate_table(te, "blue", standardize = FALSE)[c("genotype", traits)]
  raw_blup = covariate_table(te, "blup", standardize = FALSE)[c("genotype", traits)]
  b1 = stats::coef(fit_trait_model(R, raw_blue, traits))["soluble"]
  b2 = stats::coef(fit_trait_model(R, raw_blup, traits))["soluble"]
  # shrinkage of the low-repeatability trait inflates its raw coefficient
  expect_gt(abs(b2), abs(b1) * 1.5)
})

test_that("region confounded with genotype is reported", {
  s = simulate_trait_study(region_crossed = FALSE)
  expect_warning(trait_estimates(s$lab, "hardness", region = "region"), "confounded")
})

test_that("derived texture traits are flagged as collinear", {
  cov_all = covariate_table(te, "blue")
  expect_warning(cc <- collinearity_check(cov_all), "collinear")
  expect_true(any(c("gumminess", "chewiness") %in% cc$flagged))
  cov = cov_all[c("genotype", traits)]
  expect_silent(collinearity_check(cov))
})

test_that("pladmm recovers the direction of the trait effects", {
  cov = covariate_table(te, "blue")[c("genotype", traits)]
  mod = fit_trait_model(R, cov, traits)
  eff = stats::setNames(trait_effects(mod)$estimate, trait_effects(mod)$trait)
  expect_gt(eff["cohesiveness"], 0.4)
  expect_lt(eff["hardness"], -0.3)
  expect_gt(eff["colour_l"], 0.15)
  expect_lt(abs(eff["springiness"]), 0.2)
  expect_gt(trait_model_adequacy(R, mod)$p_value, 0.01)
})

test_that("shuffled covariate rows give the same model", {
  cov = covariate_table(te, "blue")[c("genotype", traits)]
  m1 = fit_trait_model(R, cov, traits)
  m2 = fit_trait_model(R, cov[sample(nrow(cov)), ], traits)
  expect_equal(stats::coef(m1), stats::coef(m2), tolerance = 1e-3)
})

test_that("too many covariates for the number of genotypes stop the fit", {
  cov = covariate_table(te, "blue")
  many = setdiff(names(cov), "genotype")
  R_small = R[, c("G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08")]
  expect_error(fit_trait_model(R_small, cov, many), "degrees of freedom")
})

test_that("sensitivity to measurement error is summarized", {
  s = covariate_sensitivity(R, te, traits, ndraw = 10)
  expect_equal(nrow(s), length(traits))
  expect_true(all(s$same_sign >= 0 & s$same_sign <= 1))
  expect_gt(s$same_sign[s$trait == "cohesiveness"], 0.9)
})
