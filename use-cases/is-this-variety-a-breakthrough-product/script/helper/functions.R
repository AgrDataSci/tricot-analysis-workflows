# Functions for the use case "Is this variety a breakthrough product?"
# Status: tested-on-synthetic-data. The tests pass in the continuous integration of the repository. It is a
#   translation of script/other-language/breakthrough.py, which was run and
#   produced every number in the README.
# Decision record: decisions/breakthrough-product.md
#
# Scientific intent
#   Decide whether a test variety gives at least a set gain over a check
#   (default 25%, the breakthrough product definition in Cairns et al. 2026),
#   and how often it does so across farms, with uncertainty.
#
# Approach
#   Step 1, per trial. Log yield on variety and farm, contrasts to the check
#     with their standard errors. This is the two-step logic of
#     Dorado-Betancourt et al. (2026), applied to a measured trait.
#   Step 2, across trials. Weighted random-effects mean of the contrasts,
#     with a variety-by-trial variance.
#   Decision. Reliability (Eskridge and Mumm 1992), the probability that the
#     variety outperforms the check. Moving the threshold from zero to the
#     required gain is our extension, not part of their paper.
#
# Assumptions
#   - Plots on one farm share the farm effect, so their ratio is free of it.
#   - Yield ratios are stable on the log scale, with one residual variance for
#     all varieties and trials. Eskridge and Mumm let the variance of the
#     differences vary by variety. Tricot data seldom have enough direct pairs
#     with the check for that, so the model-based reliability cannot show that
#     one variety is less stable than another. The direct win share can, when
#     there are enough pairs.
#   - The trials sample the farms where the check is normally grown.
#
# Alternatives
#   - One mixed model on all plots, when trials share most varieties.
#   - A Bayesian model with the full posterior of the gain (ProbBreed).
#   - Arithmetic yield ratios with zeros kept, which answer a different
#     question (total production) and are driven by a few extreme plots.
#
# Limits
#   - On-farm data only. A breakthrough decision combines station and on-farm
#     evidence, and this workflow does not read station data yet.
#   - Yield and overall preference only. Yield is not the single driver of
#     farmer choice, so the verdict is about the yield gain, not the product.
#   - A log model drops zero yields. They are counted and reported, and they
#     stay in the direct win share.
#   - The verdict thresholds are provisional defaults, not a standard.


#' Varieties linked to the check through farms that share varieties
connected_to_check = function(farm, genotype, check) {

  seen = check
  new = check

  while (length(new) > 0) {
    f = unique(farm[genotype %in% new])
    hit = unique(genotype[farm %in% f])
    new = setdiff(hit, seen)
    seen = c(seen, new)
  }

  seen

}


#' Step 1. Per trial, contrast of each variety to the check on log yield
#'
#' @param data data.frame with trial, farm, genotype and yield, one row per plot
#' @param check character, the check variety
#' @return data.frame with one row per trial and test variety. The attributes
#'   "skipped" and "sigma2" hold the trials left out and the residual variance
trial_contrasts = function(data, check) {

  rows = list()
  skipped = list()
  rss = 0
  dfres = 0

  for (tr in sort(unique(data$trial))) {

    d = data[data$trial == tr, ]
    n_in = nrow(d)

    # zero yields cannot enter a log model. They stay in direct_reliability()
    d = d[!is.na(d$yield) & d$yield > 0, ]
    d = d[d$farm %in% names(which(table(d$farm) >= 2)), ]

    if (!check %in% d$genotype) {
      skipped[[length(skipped) + 1]] = data.frame(trial = tr, reason = "check not grown")
      next
    }

    d = d[d$genotype %in% connected_to_check(d$farm, d$genotype, check), ]
    d = d[d$farm %in% names(which(table(d$farm) >= 2)), ]

    items = c(check, sort(setdiff(unique(d$genotype), check)))

    if (length(items) < 2) {
      skipped[[length(skipped) + 1]] = data.frame(trial = tr, reason = "no variety shares a farm network with the check")
      next
    }

    d$g = factor(d$genotype, levels = items)
    d$f = factor(d$farm)
    d$ly = log(d$yield)

    mod = stats::lm(ly ~ g + f, data = d)
    b = stats::coef(mod)[paste0("g", items[-1])]

    if (any(is.na(b))) {
      skipped[[length(skipped) + 1]] = data.frame(trial = tr, reason = "contrasts to the check not estimable")
      next
    }

    v = diag(summary(mod)$cov.unscaled)[paste0("g", items[-1])]

    rss = rss + sum(stats::residuals(mod)^2)
    dfres = dfres + mod$df.residual

    rows[[length(rows) + 1]] = data.frame(trial = tr,
                                          genotype = items[-1],
                                          d = as.numeric(b),
                                          v = as.numeric(v),
                                          n_plots = as.integer(table(d$g)[items[-1]]),
                                          plots_in = n_in,
                                          plots_used = nrow(d))
  }

  out = do.call(rbind, rows)
  if (is.null(out)) out = data.frame()

  # one residual variance for all trials, so that a small trial, which cannot
  # estimate its own, still enters with an honest standard error
  sigma2 = if (dfres > 0) rss / dfres else NA_real_

  if (nrow(out) > 0) {
    out$se = sqrt(sigma2 * out$v)
    out$v = NULL
  }

  if (length(skipped) > 0) {
    skipped = do.call(rbind, skipped)
    message("Trials left out of step 1: ",
            paste0(skipped$trial, " (", skipped$reason, ")", collapse = ", "))
  } else {
    skipped = data.frame(trial = character(0), reason = character(0))
  }

  attr(out, "skipped") = skipped
  attr(out, "sigma2") = sigma2

  out

}


#' Step 2. Weighted random-effects mean of the trial contrasts per variety
#'
#' The between-trial variance (tau2) is the variety-by-trial interaction,
#' estimated with the DerSimonian-Laird moment estimator. With one trial it
#' cannot be estimated and is set to zero. Check n_trials before reading it.
pool_trials = function(contrasts) {

  res = lapply(split(contrasts, contrasts$genotype), function(x) {

    w = 1 / x$se^2
    k = nrow(x)
    mu_f = sum(w * x$d) / sum(w)
    tau2 = 0

    if (k > 1) {
      q = sum(w * (x$d - mu_f)^2)
      tau2 = max(0, (q - (k - 1)) / (sum(w) - sum(w^2) / sum(w)))
    }

    ws = 1 / (x$se^2 + tau2)

    data.frame(genotype = x$genotype[1],
               log_gain = sum(ws * x$d) / sum(ws),
               se = sqrt(1 / sum(ws)),
               tau2 = tau2,
               n_trials = k,
               n_plots = sum(x$n_plots))
  })

  res = do.call(rbind, res)
  rownames(res) = NULL
  res

}


#' Reliability from normally distributed differences
#'
#' Eskridge and Mumm (1992), equation 2, on the log scale. threshold = 0 is
#' their reliability, the probability of outperforming the check.
#' threshold = 0.25 is our extension, the probability that the gain is at
#' least 25% on a farm drawn from the population the trials sampled.
reliability_normal = function(log_gain, sd_diff, threshold = 0) {

  stats::pnorm((log_gain - log1p(threshold)) / sd_diff)

}


#' Wilson interval of a proportion
wilson = function(x, n, level = 0.95) {

  if (n == 0) return(c(NA_real_, NA_real_))

  z = stats::qnorm(1 - (1 - level) / 2)
  p = x / n
  centre = (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half = z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)

  c(centre - half, centre + half)

}


#' Nonparametric reliability from farms that grew the variety and the check
#'
#' The share of such farms where the variety outyields the check, and where it
#' does so by the threshold. Zero yields stay in. A pair where both plots
#' failed counts as no win. Eskridge and Mumm need about 16 environments for a
#' precise estimate, so check n_pairs.
direct_reliability = function(data, check, threshold = 0.25) {

  data$key = paste(data$trial, data$farm, sep = "::")
  chk = data[data$genotype == check, c("key", "yield")]
  chk = stats::aggregate(yield ~ key, data = chk, FUN = mean)
  names(chk)[2] = "check_yield"

  res = lapply(sort(setdiff(unique(data$genotype), check)), function(g) {

    x = data[data$genotype == g, c("key", "yield")]
    x = stats::aggregate(yield ~ key, data = x, FUN = mean)
    x = merge(x, chk, by = "key")
    n = nrow(x)
    win0 = sum(x$yield > x$check_yield)
    wint = sum(x$yield >= (1 + threshold) * x$check_yield & x$yield > 0)
    ci = wilson(wint, n)

    data.frame(genotype = g,
               n_pairs = n,
               wins = win0,
               wins_threshold = wint,
               direct_rel = if (n > 0) win0 / n else NA_real_,
               direct_rel_threshold = if (n > 0) wint / n else NA_real_,
               direct_lower = ci[1],
               direct_upper = ci[2])
  })

  do.call(rbind, res)

}


#' Share of plots with zero yield per variety, the part a log model drops
harvest_failure = function(data) {

  res = lapply(split(data, data$genotype), function(x) {
    data.frame(genotype = x$genotype[1],
               n_plots_all = nrow(x),
               n_failed = sum(x$yield <= 0),
               share_failed = mean(x$yield <= 0))
  })

  res = do.call(rbind, res)
  rownames(res) = NULL
  res

}


#' The breakthrough table, one row per test variety
#'
#' @param data data.frame with trial, farm, genotype and yield
#' @param check character, the check variety
#' @param threshold numeric, the required gain over the check (0.25 is 25%)
#' @param nboot integer, resamples of farms for the reliability interval
#' @param min_plots integer, plots per variety below which no verdict is given.
#'   Dorado-Betancourt et al. (2026) found about 30 replicates per genotype
#'   usually enough for ranking data. We borrow that number as a floor
#' @param min_pairs integer, direct pairs with the check needed to trust the
#'   direct win share (about 16 environments in Eskridge and Mumm 1992)
#' @param p_supported numeric, probability that the mean gain reaches the
#'   threshold above which the verdict is "supported". Provisional
#' @return data.frame ordered from the largest to the smallest gain
breakthrough_table = function(data, check, threshold = 0.25, nboot = 200,
                              min_plots = 30, min_pairs = 16,
                              p_supported = 0.8, seed = 1) {

  data = data[!is.na(data$yield), c("trial", "farm", "genotype", "yield")]

  con = trial_contrasts(data, check)

  if (is.null(con) || nrow(con) == 0) {
    stop("no trial has data on the check ", check, call. = FALSE)
  }

  sigma2 = attr(con, "sigma2")
  res = pool_trials(con)
  z = stats::qnorm(0.975)

  res$gain_ratio = exp(res$log_gain)
  res$lower = exp(res$log_gain - z * res$se)
  res$upper = exp(res$log_gain + z * res$se)

  # confidence that the average gain reaches the threshold
  res$p_mean_gain = stats::pnorm((res$log_gain - log1p(threshold)) / res$se)

  # a difference between two plots on one farm carries two residuals, plus
  # the variety-by-trial interaction
  res$sd_diff = sqrt(2 * sigma2 + res$tau2)
  res$rel = reliability_normal(res$log_gain, res$sd_diff, 0)
  res$rel_threshold = reliability_normal(res$log_gain, res$sd_diff, threshold)

  # interval of the reliability, resampling farms within trials
  set.seed(seed)
  data$key = paste(data$trial, data$farm, sep = "::")
  rows = split(seq_len(nrow(data)), data$key)
  keys = split(data$key[!duplicated(data$key)], data$trial[!duplicated(data$key)])

  boot = vapply(seq_len(nboot), function(i) {
    pick = unlist(lapply(keys, function(k) sample(k, length(k), replace = TRUE)))
    idx = rows[pick]
    bd = data[unlist(idx), ]
    bd$farm = rep(seq_along(idx), lengths(idx))
    bc = suppressMessages(trial_contrasts(bd, check))
    out = rep(NA_real_, nrow(res))
    if (is.null(bc) || nrow(bc) == 0) return(out)
    bp = pool_trials(bc)
    r = reliability_normal(bp$log_gain, sqrt(2 * attr(bc, "sigma2") + bp$tau2), threshold)
    out[match(bp$genotype, res$genotype)] = r
    out
  }, numeric(nrow(res)))

  boot = matrix(boot, nrow = nrow(res))
  res$rel_threshold_lower = apply(boot, 1, stats::quantile, probs = 0.025, na.rm = TRUE)
  res$rel_threshold_upper = apply(boot, 1, stats::quantile, probs = 0.975, na.rm = TRUE)

  res = merge(res, direct_reliability(data, check, threshold), by = "genotype", all.x = TRUE)
  res = merge(res, harvest_failure(data), by = "genotype", all.x = TRUE)
  res$enough_pairs = res$n_pairs >= min_pairs

  res$verdict = ifelse(res$n_plots < min_plots, "insufficient data",
                ifelse(res$p_mean_gain >= p_supported, "supported",
                ifelse(res$gain_ratio >= 1 + threshold, "indicative",
                       "not supported")))

  res = res[order(res$log_gain, decreasing = TRUE), ]
  rownames(res) = NULL

  attr(res, "skipped") = attr(con, "skipped")
  attr(res, "sigma2") = sigma2
  attr(res, "plots_in") = nrow(data)
  attr(res, "plots_zero") = sum(data$yield <= 0)

  res

}


#' Probability that farmers rank each variety above the check
#'
#' The "value" side of the definition. Rankings cannot return a percentage
#' gain, so this is reported beside the yield gain, never converted into it.
#'
#' @param rankings data.frame with farm, genotype and rank (1 is best)
#' @param check character, the check variety
preference_reliability = function(rankings, check) {

  rankings = rankings[!is.na(rankings$rank), ]
  items = sort(unique(rankings$genotype))
  farms = unique(rankings$farm)

  m = matrix(0, nrow = length(farms), ncol = length(items),
             dimnames = list(farms, items))
  m[cbind(match(rankings$farm, farms), match(rankings$genotype, items))] = rankings$rank

  # keep farms with a complete ranking of three plots, without ties
  ok = apply(m, 1, function(x) identical(as.numeric(sort(x[x > 0])), c(1, 2, 3)))
  R = PlackettLuce::as.rankings(m[ok, , drop = FALSE])

  mod = PlackettLuce::PlackettLuce(R)
  rel = gosset::reliability(mod, ref = check)

  out = data.frame(genotype = rel$item,
                   p_preferred = rel$reliability,
                   n_rankings = colSums(m[ok, , drop = FALSE] > 0)[rel$item],
                   row.names = NULL)

  out[out$genotype != check, ]

}
