# Functions for the use case "Which measured traits explain preference?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/trait-associations.md and decisions/genetic-gain.md
# Domain leads for genetic parameters: Joost van Heerwaarden and Hugo Dorado-Betancourt
#
# Scientific intent
#   Explain participant preference for genotypes with traits measured
#   independently, in the lab or on station (texture, colour, yield
#   components). The genotype estimates of each measured trait become item
#   covariates of a Plackett-Luce model (PlackettLuce::pladmm), so the
#   coefficients say how much each trait moves preference.
#
# Approach
#   1. Check that genotype names match between the rankings and the lab data.
#   2. Estimate each trait per genotype with a mixed model that carries the
#      design available. Region (or location) enters as a fixed effect when
#      genotypes were measured in more than one region. Report the BLUE
#      (adjusted genotype mean), the BLUP and the repeatability of the trait.
#   3. Use BLUEs as covariates by default. BLUPs shrink each trait by a
#      different amount, depending on its repeatability, which rescales its
#      coefficient in pladmm.
#   4. Screen covariates for collinearity, including traits derived from
#      others by formula, and keep enough residual degrees of freedom.
#   5. Standardize the covariates, so coefficients are per standard deviation.
#   6. Fit pladmm, test the attribute model against the free Plackett-Luce
#      model, and check how stable the coefficients are when the covariates
#      are redrawn within their standard errors.
#
# Assumptions
#   - Lab or station samples are replicates of the same genotypes evaluated by
#     participants, under comparable processing.
#   - Effects of the traits on log-worth are additive and linear.
#   - Without processor, batch or sample information, sample variation within
#     genotype is treated as residual.
#
# Alternatives
#   - pltree with the same worth formula, to let coefficients differ between
#     participant groups (see archive/rtbfoods-eba-nigeria/03_pladmm.R).
#   - The trait-association use case, when the traits are ranked by
#     participants rather than measured.
#   - Two-step genetic parameters from the rankings themselves
#     (Dorado-Betancourt et al. 2026), when the question is the genetic signal
#     in preference rather than its drivers.
#
# Limits
#   - Associations between measured traits and preference are not causal. A
#     trait may stand for an unmeasured one correlated with it.
#   - With few genotypes, only a few covariates can be estimated. The model
#     has one data point per genotype for the attribute part.
#   - Covariates carry estimation error, which biases coefficients towards
#     zero. The sensitivity check shows how much this matters.


#' Compare genotype names between rankings and lab data
#'
#' @param ranked character, genotype names in the rankings
#' @param measured character, genotype names in the lab data
#' @return character vector of genotypes in both, with a warning listing the
#'   rest
check_genotype_names = function(ranked, measured) {

  ranked = unique(as.character(ranked))
  measured = unique(as.character(measured))

  only_ranked = setdiff(ranked, measured)
  only_measured = setdiff(measured, ranked)

  if (length(only_ranked) + length(only_measured) > 0) {
    warning("genotypes not in both data sets. Ranked only: ",
            paste(only_ranked, collapse = ", "),
            ". Measured only: ", paste(only_measured, collapse = ", "),
            ". Check that names are harmonized", call. = FALSE)
  }

  intersect(ranked, measured)

}


#' Genotype estimates, BLUE, BLUP and repeatability, for measured traits
#'
#' For each trait, fits a fixed-effect model for the BLUE (adjusted genotype
#' mean, averaged over regions with sum-to-zero contrasts) and a mixed model
#' with genotype as random for the BLUP and the variance components. Region
#' enters both models when genotypes were measured in more than one region.
#' Results are matched to genotypes by name, never by position.
#'
#' @param lab data.frame, one row per lab sample
#' @param traits character, trait columns in lab
#' @param genotype character, the genotype column
#' @param region character or NULL, the region or location column
#' @return list with a long table of estimates and a table of repeatability
trait_estimates = function(lab, traits, genotype = "genotype", region = NULL) {

  lab[[genotype]] = factor(lab[[genotype]])
  geno = levels(lab[[genotype]])

  use_region = FALSE
  if (!is.null(region)) {
    lab[[region]] = factor(lab[[region]])
    spread = tapply(lab[[region]], lab[[genotype]], function(x) length(unique(x)))
    if (nlevels(lab[[region]]) < 2) {
      use_region = FALSE
    } else if (all(spread == 1)) {
      warning("each genotype was measured in one region only, so region and ",
              "genotype effects are confounded. Region is left out and genotype ",
              "estimates include regional differences", call. = FALSE)
    } else {
      use_region = TRUE
    }
  }

  est = list()
  rep = list()

  for (tr in traits) {

    d = lab[!is.na(lab[[tr]]), c(tr, genotype, if (use_region) region), drop = FALSE]
    names(d)[1:2] = c("y", "g")
    if (use_region) names(d)[3] = "region"
    d$g = droplevels(d$g)

    # BLUE, adjusted mean per genotype
    f_fix = if (use_region) y ~ 0 + g + region else y ~ 0 + g
    ctr = if (use_region) list(region = "contr.sum") else NULL
    fit = stats::lm(f_fix, data = d, contrasts = ctr)
    cf = summary(fit)$coefficients
    cf = cf[grepl("^g", rownames(cf)), , drop = FALSE]
    g_names = sub("^g", "", rownames(cf))

    # BLUP and variance components
    f_mix = if (use_region) y ~ region + (1 | g) else y ~ 1 + (1 | g)
    mix = lme4::lmer(f_mix, data = d)
    re = as.data.frame(lme4::ranef(mix, condVar = TRUE))
    vc = as.data.frame(lme4::VarCorr(mix))
    vg = vc$vcov[vc$grp == "g"]
    ve = vc$vcov[vc$grp == "Residual"]
    n_bar = mean(table(d$g))

    blue = cf[, "Estimate"]
    est[[tr]] = data.frame(
      genotype = g_names,
      trait = tr,
      blue = blue - mean(blue),
      blue_se = cf[, "Std. Error"],
      blup = re$condval[match(g_names, as.character(re$grp))],
      blup_se = re$condsd[match(g_names, as.character(re$grp))],
      n = as.vector(table(d$g)[g_names]),
      row.names = NULL)

    rep[[tr]] = data.frame(trait = tr, vg = vg, ve = ve, samples = n_bar,
                           repeatability = vg / (vg + ve / n_bar),
                           region_in_model = use_region)

  }

  list(estimates = do.call("rbind", est),
       repeatability = do.call("rbind", rep),
       genotypes = geno)

}


#' Wide table of covariates, one row per genotype
#'
#' @param te output of trait_estimates
#' @param type "blue" (default) or "blup"
#' @param standardize logical, convert each trait to z-scores
covariate_table = function(te, type = c("blue", "blup"), standardize = TRUE) {

  type = match.arg(type)
  e = te$estimates

  w = stats::reshape(e[c("genotype", "trait", type)],
                     idvar = "genotype", timevar = "trait", direction = "wide")
  names(w) = sub(paste0("^", type, "\\."), "", names(w))

  if (standardize) {
    for (v in setdiff(names(w), "genotype")) {
      w[[v]] = as.vector(scale(w[[v]]))
    }
  }

  rownames(w) = NULL
  w

}


#' Screen covariates for collinearity
#'
#' Traits derived from others by formula (in texture profile analysis,
#' gumminess = hardness x cohesiveness and chewiness = gumminess x
#' springiness) are collinear by construction and should not enter together
#' with their components. This check catches them and any other strong
#' dependence through correlations and variance inflation factors.
#'
#' @param covariates data.frame with a genotype column and trait columns
#' @param max_cor absolute correlation above which a pair is flagged
#' @param max_vif variance inflation factor above which a trait is flagged
collinearity_check = function(covariates, max_cor = 0.85, max_vif = 10) {

  X = covariates[setdiff(names(covariates), "genotype")]
  r = stats::cor(X, use = "pairwise.complete.obs")

  vif = tryCatch(diag(solve(r)), error = function(e) rep(Inf, ncol(r)))
  names(vif) = colnames(r)

  idx = which(abs(r) > max_cor & upper.tri(r), arr.ind = TRUE)
  pairs = data.frame(trait_1 = rownames(r)[idx[, 1]],
                     trait_2 = colnames(r)[idx[, 2]],
                     correlation = r[idx])

  flagged = names(vif)[vif > max_vif]

  if (length(flagged) > 0) {
    warning("collinear covariates (VIF > ", max_vif, "): ",
            paste(flagged, collapse = ", "),
            ". Their coefficients cannot be interpreted separately. Drop ",
            "derived traits or keep one trait per correlated group",
            call. = FALSE)
  }

  list(vif = vif, pairs = pairs, flagged = flagged)

}


#' Match the covariate table to the order of items in the rankings
align_covariates = function(R, covariates) {

  items = colnames(unclass(R))
  miss = setdiff(items, covariates$genotype)
  if (length(miss) > 0) {
    stop("ranked genotypes without measured traits: ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  covariates[match(items, covariates$genotype), , drop = FALSE]

}


#' Fit pladmm with measured traits as item covariates
#'
#' @param R rankings of the genotypes
#' @param covariates data.frame from covariate_table
#' @param traits character, traits to include
#' @param min_df integer, smallest residual degrees of freedom allowed for
#'   the attribute model against the free model
#' @param rho penalty parameter of pladmm
fit_trait_model = function(R, covariates, traits, min_df = 2, rho = 8) {

  n_items = ncol(unclass(R))
  df = (n_items - 1) - length(traits)

  if (df < min_df) {
    stop(length(traits), " covariates for ", n_items, " genotypes leaves ", df,
         " degrees of freedom. Use fewer traits", call. = FALSE)
  }

  f = stats::as.formula(paste("~", paste(traits, collapse = " + ")))
  d = align_covariates(R, covariates)

  PlackettLuce::pladmm(R, f, data = d, rho = rho)

}


#' Coefficients with standard errors
trait_effects = function(mod) {

  s = summary(mod)$coefficients
  s = s[rownames(s) != "(Intercept)", , drop = FALSE]

  data.frame(trait = rownames(s),
             estimate = s[, "Estimate"],
             se = s[, "Std. Error"],
             lower = s[, "Estimate"] - 1.96 * s[, "Std. Error"],
             upper = s[, "Estimate"] + 1.96 * s[, "Std. Error"],
             p_value = s[, "Pr(>|z|)"],
             row.names = NULL)

}


#' Do the measured traits explain the genotype worths?
#'
#' Likelihood-ratio test of the attribute model against the free
#' Plackett-Luce model, both without pseudo-rankings. A small p-value means
#' preference differs between genotypes in ways the traits do not capture.
trait_model_adequacy = function(R, mod) {

  free = PlackettLuce::PlackettLuce(R, npseudo = 0)
  ll_free = free$loglik
  ll_attr = as.numeric(stats::logLik(mod))
  df = (ncol(unclass(R)) - 1) - mod$rank
  dev = 2 * (ll_free - ll_attr)

  data.frame(deviance = dev, df = df,
             p_value = stats::pchisq(dev, df, lower.tail = FALSE))

}


#' Stability of coefficients when covariates are redrawn within their error
#'
#' Each draw replaces every genotype estimate by a value drawn from a normal
#' distribution with its standard error, standardizes again and refits. Large
#' spread or frequent sign changes mean the conclusion depends on how
#' precisely the traits were measured.
#'
#' @param R rankings
#' @param te output of trait_estimates
#' @param traits character
#' @param ndraw integer
#' @param seed integer
covariate_sensitivity = function(R, te, traits, ndraw = 50, seed = 1, rho = 8) {

  base = trait_effects(fit_trait_model(R, covariate_table(te), traits, rho = rho))

  set.seed(seed)
  draws = vapply(seq_len(ndraw), function(b) {
    e = te
    e$estimates$blue = stats::rnorm(nrow(e$estimates), e$estimates$blue,
                                    e$estimates$blue_se)
    m = fit_trait_model(R, covariate_table(e), traits, rho = rho)
    trait_effects(m)$estimate[match(base$trait, trait_effects(m)$trait)]
  }, numeric(nrow(base)))

  draws = matrix(draws, nrow = nrow(base))

  data.frame(trait = base$trait,
             estimate = base$estimate,
             draw_sd = apply(draws, 1, stats::sd),
             same_sign = rowMeans(sign(draws) == sign(base$estimate)))

}
