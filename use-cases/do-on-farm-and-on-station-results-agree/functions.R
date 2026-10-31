# Functions for the use case "Do on-farm and on-station results agree?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/genetic-gain.md
#
# Scientific intent
#   Check whether genotypes that perform well in on-station trials are the
#   ones farmers prefer on their own farms, and how reliable each source of
#   information is. This informs whether station selection is a good proxy
#   for farmer preference, and where on-farm testing adds information.
#
# Approach
#   1. On-station, fit a linear mixed model with genotype, genotype by
#      location and residual variance components (lme4::lmer), estimate
#      entry-mean broad-sense heritability and genotype BLUPs.
#   2. On-farm, fit a Plackett-Luce model to tricot rankings of overall
#      preference and extract log-worth per genotype.
#   3. Estimate the reliability of the on-farm ranking with a split-half
#      approach, two random halves of participants, correlation of their
#      log-worth, Spearman-Brown correction. This is the reliability of the
#      preference estimate, not a heritability.
#   4. Correlate station BLUPs with on-farm log-worth across the genotypes in
#      both trials, with a bootstrap interval over genotypes.
#   5. Compare the top genotypes selected by each source.
#
# Assumptions
#   - Station trials follow a randomized complete block design within each
#     location, with replicates recorded.
#   - Genotype names are harmonized between station and on-farm data.
#   - Entry-mean heritability uses the number of locations and the mean
#     number of replicates per location from the same factors used in the
#     model.
#
# Alternatives
#   - Generalized (Cullis) heritability from the prediction error variance,
#     for unbalanced station data.
#   - Genetic correlation between station traits and on-farm preference with
#     a multivariate mixed model, when the design supports it.
#   - Treating overall preference as a heritable trait. This is a research
#     proposition. It needs a design that separates genetic, environmental,
#     rater and measurement effects, and is not estimated here.
#
# Limits
#   - With few genotypes the correlation interval is wide. Report it.
#   - A low correlation does not show either source is wrong. Farmers may
#     value traits not measured on station, and on-farm environments differ.
#   - The split-half reliability is not heritability. Do not report it as H2.


#' Station variance components, heritability and BLUPs
#'
#' @param station data.frame with columns yield, genotype, location, rep
#' @return list with variance components, H2 and a table of BLUPs
station_heritability = function(station) {

  station$genotype = factor(station$genotype)
  station$location = factor(station$location)
  station$rep = factor(station$rep)

  mod = lme4::lmer(yield ~ location + (1 | location:rep) +
                     (1 | genotype) + (1 | genotype:location),
                   data = station)

  vc = as.data.frame(lme4::VarCorr(mod))
  vg = vc$vcov[vc$grp == "genotype"]
  vgl = vc$vcov[vc$grp == "genotype:location"]
  ve = vc$vcov[vc$grp == "Residual"]

  # number of locations and mean replicates per location, from the same
  # factors used in the model
  L = nlevels(station$location)
  r = mean(tapply(station$rep, station$location, function(x) length(unique(x))))

  H2 = vg / (vg + vgl / L + ve / (L * r))

  blup = lme4::ranef(mod)$genotype

  list(model = mod,
       components = data.frame(genotype = vg, genotype_location = vgl,
                               residual = ve, locations = L, reps = r),
       H2 = H2,
       blup = data.frame(genotype = rownames(blup), blup = blup[, 1],
                         row.names = NULL))

}


#' On-farm log-worth from tricot rankings
on_farm_worth = function(R) {

  co = stats::coef(PlackettLuce::PlackettLuce(R), log = TRUE)

  # centre on the mean so the scale does not depend on a reference genotype
  data.frame(genotype = names(co), log_worth = unname(co - mean(co)))

}


#' Split-half reliability of on-farm log-worth
#'
#' @param R rankings
#' @param nsplit integer, number of random splits
#' @param seed integer
#' @return mean split-half correlation and its Spearman-Brown correction
on_farm_reliability = function(R, nsplit = 50, seed = 1) {

  n = nrow(R)
  set.seed(seed)

  r = vapply(seq_len(nsplit), function(s) {
    half = sample(n) <= n / 2
    a = stats::coef(PlackettLuce::PlackettLuce(R[half, ]), log = TRUE)
    b = stats::coef(PlackettLuce::PlackettLuce(R[!half, ]), log = TRUE)
    common = intersect(names(a), names(b))
    stats::cor(a[common], b[common])
  }, numeric(1))

  data.frame(split_half = mean(r),
             reliability = 2 * mean(r) / (1 + mean(r)))

}


#' Agreement between station BLUPs and on-farm log-worth
#'
#' @param blup data.frame with genotype and blup
#' @param worth data.frame with genotype and log_worth
#' @param top integer, number of top genotypes to compare
#' @param nboot integer, bootstrap resamples of genotypes
#' @param seed integer
agreement = function(blup, worth, top = 5, nboot = 1000, seed = 1) {

  only_station = setdiff(blup$genotype, worth$genotype)
  only_farm = setdiff(worth$genotype, blup$genotype)

  if (length(only_station) + length(only_farm) > 0) {
    warning("genotypes not in both trials are excluded: ",
            paste(c(only_station, only_farm), collapse = ", "),
            ". Check that names are harmonized", call. = FALSE)
  }

  d = merge(blup, worth, by = "genotype")

  if (nrow(d) < 5) {
    stop("fewer than 5 genotypes in common, agreement cannot be assessed",
         call. = FALSE)
  }

  spearman = function(i) stats::cor(d$blup[i], d$log_worth[i], method = "spearman")

  set.seed(seed)
  boot = vapply(seq_len(nboot), function(b) {
    spearman(sample.int(nrow(d), nrow(d), replace = TRUE))
  }, numeric(1))

  top_station = d$genotype[order(d$blup, decreasing = TRUE)][seq_len(top)]
  top_farm = d$genotype[order(d$log_worth, decreasing = TRUE)][seq_len(top)]

  list(summary = data.frame(genotypes = nrow(d),
                            spearman = spearman(seq_len(nrow(d))),
                            lower = stats::quantile(boot, 0.025, na.rm = TRUE),
                            upper = stats::quantile(boot, 0.975, na.rm = TRUE),
                            top_overlap = length(intersect(top_station, top_farm)),
                            top = top,
                            row.names = NULL),
       table = d[order(d$blup, decreasing = TRUE), ])

}
