# Functions for the use case "Are rankings repeatable within segments?"
# Status: reviewed-not-executed. This R code has not been run. It is a
#   translation of script/other-language/repeatability.py, which was run and
#   produced every number in the README. Run tests/run-tests.R before use.
# Decision record: decisions/repeatability.md
#
# Scientific intent
#   Measure how repeatable a ranking of items (varieties or product concepts)
#   is, and test whether it is more repeatable within segments (market
#   segments, envirotypes) than in the unsegmented data.
#
# Approach
#   Step 1. Plackett-Luce log-worth of each item with its variance, per group
#     (Dorado-Betancourt et al. 2026).
#   Step 2. The variance among items against the variance of their estimates
#     gives the repeatability of the item means.
#   Fair test. Segmenting shrinks the sample, and a smaller sample is less
#     repeatable whatever the segments mean. So segments are compared with
#     random segments of the same sizes, and with the pooled data at the
#     same number of participants.
#
# Assumptions
#   - Participants are independent and each ranks three items, with a best
#     and a worst choice and no ties.
#   - Segments are defined without looking at the rankings. Segments found
#     by a Plackett-Luce tree used the rankings, so their repeatability must
#     be judged on held-out data only.
#   - Estimation errors of the log-worths are treated as independent, as the
#     quasi-variances in Dorado-Betancourt et al. (2026) do.
#
# Alternatives
#   - The Cullis heritability of their qH2(), preferable with many items.
#   - Thurstonian models in place of Plackett-Luce, as in their code.
#   - A factor-analytic mixed model across environments (ASReml), when the
#     question is genotype by environment structure.
#
# Limits
#   - Repeatability is about the item means. It says nothing on how well one
#     participant's answer can be predicted (see individual_fit()).
#   - High repeatability is not validity. A stable ranking of stated
#     preferences does not establish adoption or purchase.
#   - A segmentation that fails the test here can still matter elsewhere.


#' Order the three items of each participant from best to worst
#'
#' @param data data.frame, one row per participant
#' @param items character, the three columns with the items shown
#' @param best,worst character, columns with the position (A, B or C) of the
#'   item chosen as best and as worst
#' @return integer matrix with the item index of the first, second and third
#'   choice. The attribute "levels" holds the item names, "rows" the rows of
#'   data kept and "dropped" the number of rows left out
as_ordered = function(data, items = c("item_1", "item_2", "item_3"),
                      best = "best", worst = "worst", levels = NULL) {

  pos = c(A = 1, B = 2, C = 3)
  b = unname(pos[as.character(data[[best]])])
  w = unname(pos[as.character(data[[worst]])])
  it = as.matrix(data[, items])

  ok = !is.na(b) & !is.na(w) & b != w & rowSums(is.na(it)) == 0
  ok[is.na(ok)] = FALSE

  b = b[ok]
  w = w[ok]
  m = 6 - b - w
  it = it[ok, , drop = FALSE]
  r = seq_len(nrow(it))

  if (is.null(levels)) levels = sort(unique(as.vector(it)))

  out = cbind(match(it[cbind(r, b)], levels),
              match(it[cbind(r, m)], levels),
              match(it[cbind(r, w)], levels))

  attr(out, "levels") = levels
  attr(out, "rows") = which(ok)
  attr(out, "dropped") = sum(!ok)

  out

}


#' Step 1. Plackett-Luce log-worth, centred on zero, with its variance
#'
#' @param ord integer matrix from as_ordered(), or a subset of its rows
#' @param k integer, the number of items
#' @return list with est and var. PlackettLuce adds by default half a win and
#'   half a loss of each item against a hypothetical item (npseudo = 0.5), so
#'   an item never chosen as best or worst still has a finite estimate
pl_estimates = function(ord, k) {

  n = nrow(ord)
  m = matrix(0, nrow = n, ncol = k)
  m[cbind(seq_len(n), ord[, 1])] = 1
  m[cbind(seq_len(n), ord[, 2])] = 2
  m[cbind(seq_len(n), ord[, 3])] = 3
  colnames(m) = paste0("i", seq_len(k))

  mod = PlackettLuce::PlackettLuce(PlackettLuce::as.rankings(m))

  # ref = NULL centres the log-worths on their mean
  est = stats::coef(mod, ref = NULL)
  var = diag(stats::vcov(mod, ref = NULL))

  list(est = as.numeric(est[colnames(m)]),
       var = as.numeric(var[colnames(m)]))

}


#' Step 2. Repeatability of the item means
#'
#' The items are a random effect with variance sigma2, observed with known
#' error variances, the weighted second step of Dorado-Betancourt et al.
#' (2026) with the dispersion fixed at one. sigma2 is estimated by REML. The
#' repeatability is the mean of sigma2 / (sigma2 + variance), the share of
#' the variance among estimated item means that is real difference between
#' items and not estimation error.
#'
#' Their qH2() uses the Cullis form, 1 - mean(PEV) / sigma2, where the
#' prediction error also counts the uncertainty of the overall mean. With k
#' items that puts a ceiling of about 1 - 1/k on the result, 0.875 for eight
#' concepts however large the sample. The default here leaves that term out,
#' since the log-worths are already centred. With many items the two agree.
#'
#' @param est,var numeric, estimates and their variances
#' @param cullis logical, TRUE returns the Cullis form
#' @return numeric, with the attribute "sigma2"
repeatability = function(est, var, cullis = FALSE) {

  reml = function(s2) {
    w = 1 / (s2 + var)
    mu = sum(w * est) / sum(w)
    0.5 * (sum(log(s2 + var)) + log(sum(w)) + sum(w * (est - mu)^2))
  }

  hi = max(stats::var(est) * 4, 1e-6)
  s2 = stats::optimize(reml, c(0, hi), tol = 1e-10)$minimum

  if (s2 < 1e-8 || reml(0) <= reml(s2)) {
    out = 0
    attr(out, "sigma2") = 0
    return(out)
  }

  if (cullis) {
    w = 1 / (s2 + var)
    P = diag(w) - outer(w, w) / sum(w)
    out = 1 - mean(s2 - s2^2 * diag(P)) / s2
  } else {
    out = mean(s2 / (s2 + var))
  }

  attr(out, "sigma2") = s2
  out

}


group_repeatability = function(ord, k) {

  e = pl_estimates(ord, k)
  as.numeric(repeatability(e$est, e$var))

}


#' Log-likelihood of ordered triplets under given log-worths
loglik_ordered = function(ord, est) {

  a = est[ord[, 1]]
  b = est[ord[, 2]]
  c = est[ord[, 3]]

  sum(a - log(exp(a) + exp(b) + exp(c)) + b - log(exp(b) + exp(c)))

}


#' Repeatability of the unsegmented data against the number of participants
#'
#' Subsamples without replacement. Read it as sample-size guidance, the
#' number of participants at which the ranking of the items stabilises.
repeatability_curve = function(ord, sizes = c(50, 100, 200, 400, 800, 1600),
                               B = 200, seed = 1) {

  set.seed(seed)
  k = length(attr(ord, "levels"))
  sizes = sizes[sizes <= nrow(ord)]

  res = lapply(sizes, function(m) {
    x = replicate(B, group_repeatability(ord[sample(nrow(ord), m), , drop = FALSE], k))
    data.frame(n = m, repeatability = mean(x),
               lower = unname(stats::quantile(x, 0.025)),
               upper = unname(stats::quantile(x, 0.975)))
  })

  rbind(do.call(rbind, res),
        data.frame(n = nrow(ord), repeatability = group_repeatability(ord, k),
                   lower = NA, upper = NA))

}


#' Repeatability of the pooled data and of each segment at the same n
#'
#' For the pooled data and every segment with at least m participants, draw B
#' subsamples of m and compute the repeatability. Where a group has at least
#' 2m, also draw two disjoint subsamples of m and correlate their log-worths
#' (split-half). Single groups move by chance, so this table describes and
#' segment_test() decides.
equal_n = function(ord, segment, m, B = 200, seed = 1) {

  set.seed(seed)
  k = length(attr(ord, "levels"))
  segment = as.character(segment)
  groups = c(list(pooled = seq_len(nrow(ord))),
             split(seq_len(nrow(ord)), segment))

  res = lapply(names(groups), function(g) {

    idx = groups[[g]]
    row = data.frame(group = g, n = length(idx), m = m, repeatability = NA,
                     lower = NA, upper = NA, split_half_r = NA,
                     split_half_lower = NA, split_half_upper = NA,
                     split_half_tau = NA)

    if (length(idx) >= m) {
      x = replicate(B, group_repeatability(ord[sample(idx, m), , drop = FALSE], k))
      row$repeatability = mean(x)
      row$lower = unname(stats::quantile(x, 0.025))
      row$upper = unname(stats::quantile(x, 0.975))
    }

    if (length(idx) >= 2 * m) {
      x = replicate(B, {
        s = sample(idx, 2 * m)
        e1 = pl_estimates(ord[s[seq_len(m)], , drop = FALSE], k)$est
        e2 = pl_estimates(ord[s[m + seq_len(m)], , drop = FALSE], k)$est
        c(stats::cor(e1, e2), stats::cor(e1, e2, method = "kendall"))
      })
      row$split_half_r = mean(x[1, ])
      row$split_half_lower = unname(stats::quantile(x[1, ], 0.025))
      row$split_half_upper = unname(stats::quantile(x[1, ], 0.975))
      row$split_half_tau = mean(x[2, ])
    }

    row
  })

  do.call(rbind, res)

}


#' Do the segments differ, and does segmenting raise repeatability?
#'
#' Two statistics, each against random segments of the same sizes (labels
#' shuffled among participants).
#'   deviance. Twice the gain in log-likelihood of one model per segment over
#'     one pooled model. A small p says preferences differ between segments.
#'   repeatability. The mean repeatability of the segments, weighted by size.
#'     A small p says the segments are more repeatable than random groups of
#'     the same sizes, which is the claim under test.
#' Random segments carry the same loss of sample size as real ones, so the
#' comparison is fair.
segment_test = function(ord, segment, B = 500, seed = 1) {

  set.seed(seed)
  k = length(attr(ord, "levels"))
  keep = !is.na(segment)
  ord = ord[keep, , drop = FALSE]
  lab = as.character(segment[keep])

  ll0 = loglik_ordered(ord, pl_estimates(ord, k)$est)

  stats_for = function(lab) {
    dev = -2 * ll0
    rep = 0
    for (g in unique(lab)) {
      og = ord[lab == g, , drop = FALSE]
      e = pl_estimates(og, k)
      dev = dev + 2 * loglik_ordered(og, e$est)
      rep = rep + nrow(og) * as.numeric(repeatability(e$est, e$var))
    }
    c(dev, rep / length(lab))
  }

  obs = stats_for(lab)
  null = replicate(B, stats_for(sample(lab)))

  data.frame(n = nrow(ord),
             segments = length(unique(lab)),
             smallest = min(table(lab)),
             deviance = obs[1],
             p_deviance = (1 + sum(null[1, ] >= obs[1])) / (B + 1),
             repeatability_pooled = group_repeatability(ord, k),
             repeatability_segments = obs[2],
             repeatability_random = mean(null[2, ]),
             random_lower = unname(stats::quantile(null[2, ], 0.025)),
             random_upper = unname(stats::quantile(null[2, ], 0.975)),
             p_repeatability = (1 + sum(null[2, ] >= obs[2])) / (B + 1))

}


#' Gain in held-out log-likelihood per ranking from one model per segment
#'
#' Positive when knowing the segment of a participant predicts their ranking
#' better than the pooled model. With blocks (village, district, season) each
#' block is held out in turn, so the test fold holds places the training
#' folds did not see. Without blocks, k random folds, repeated.
cv_gain = function(ord, segment, blocks = NULL, k_folds = 10, reps = 5, seed = 1) {

  set.seed(seed)
  k = length(attr(ord, "levels"))
  keep = !is.na(segment)
  ord = ord[keep, , drop = FALSE]
  lab = as.character(segment[keep])

  if (!is.null(blocks)) {
    folds = list(as.integer(factor(blocks[keep])))
  } else {
    folds = lapply(seq_len(reps), function(i) sample(nrow(ord)) %% k_folds + 1)
  }

  gains = vapply(folds, function(fold) {
    g = 0
    for (f in unique(fold)) {
      tr = fold != f
      te = fold == f
      pooled = pl_estimates(ord[tr, , drop = FALSE], k)$est
      g = g - loglik_ordered(ord[te, , drop = FALSE], pooled)
      for (s in unique(lab)) {
        if (any(te & lab == s)) {
          est = pooled
          if (sum(tr & lab == s) >= 20) {
            est = pl_estimates(ord[tr & lab == s, , drop = FALSE], k)$est
          }
          g = g + loglik_ordered(ord[te & lab == s, , drop = FALSE], est)
        }
      }
    }
    g / nrow(ord)
  }, numeric(1))

  data.frame(gain_per_ranking = mean(gains),
             gain_sd = if (length(gains) > 1) stats::sd(gains) else NA,
             folds = if (!is.null(blocks)) "blocks" else paste(k_folds, "random folds x", reps))

}


#' How well the model predicts one participant's ranking
#'
#' McFadden pseudo R2, one minus the log-likelihood of the model over that of
#' choosing at random among the six orders of three items. It is about single
#' answers. Repeatability is about the item means. A ranking of items can be
#' highly repeatable while single answers stay hard to predict.
individual_fit = function(ord, segment = NULL) {

  k = length(attr(ord, "levels"))
  ll0 = nrow(ord) * log(1 / 6)
  out = data.frame(n = nrow(ord),
                   pseudo_r2_pooled = 1 - loglik_ordered(ord, pl_estimates(ord, k)$est) / ll0,
                   pseudo_r2_segments = NA)

  if (!is.null(segment)) {
    keep = !is.na(segment)
    lab = as.character(segment)
    ll = sum(vapply(unique(lab[keep]), function(g) {
      og = ord[keep & lab == g, , drop = FALSE]
      loglik_ordered(og, pl_estimates(og, k)$est)
    }, numeric(1)))
    out$pseudo_r2_segments = 1 - ll / (sum(keep) * log(1 / 6))
  }

  out

}


#' Probability that each item is ranked above a reference item
#'
#' Reliability in the sense of Eskridge and Mumm (1992), from Plackett-Luce
#' worth as gosset::reliability() computes it. Pooled, and per segment when
#' one is given. Intervals from resampling participants.
benchmark_reliability = function(ord, ref, segment = NULL, B = 300, seed = 1) {

  set.seed(seed)
  lev = attr(ord, "levels")
  k = length(lev)
  r = match(ref, lev)
  groups = list(pooled = seq_len(nrow(ord)))
  if (!is.null(segment)) {
    groups = c(groups, split(seq_len(nrow(ord)), as.character(segment)))
  }

  rel = function(e) 1 / (1 + exp(-(e - e[r])))

  res = lapply(names(groups), function(g) {
    idx = groups[[g]]
    p = rel(pl_estimates(ord[idx, , drop = FALSE], k)$est)
    bt = replicate(B, rel(pl_estimates(ord[sample(idx, length(idx), replace = TRUE), , drop = FALSE], k)$est))
    data.frame(group = g, n = length(idx), item = lev[-r], reliability = p[-r],
               lower = apply(bt, 1, stats::quantile, probs = 0.025)[-r],
               upper = apply(bt, 1, stats::quantile, probs = 0.975)[-r],
               row.names = NULL)
  })

  do.call(rbind, res)

}


#' Adjusted Rand index of two partitions of the same units
#'
#' @param a,b vectors with a label per unit
#' @return numeric, 1 for identical partitions and about 0 for unrelated ones
adjusted_rand = function(a, b) {

  tab = table(a, b)
  c2 = function(x) x * (x - 1) / 2
  s = sum(c2(tab))
  ra = sum(c2(rowSums(tab)))
  rb = sum(c2(colSums(tab)))
  e = ra * rb / c2(sum(tab))

  if ((ra + rb) / 2 == e) return(1)

  (s - e) / ((ra + rb) / 2 - e)

}


#' Build segments from covariates alone, never from the rankings
#'
#' Columns named in log are transformed with log1p, all columns are
#' standardised, missing values take the column median and k-means gives k
#' groups. Stability is the mean adjusted Rand index between the groups of
#' the full data and the groups refitted on random 80% subsamples. A
#' partition below about 0.7 is not stable enough to name.
#'
#' k-means on standardised numeric covariates is the simplest defensible
#' choice. With categorical covariates use model-based clustering or FAMD
#' first, as the record on socioeconomic heterogeneity says.
#'
#' @param x data.frame of numeric covariates, one row per unit (a participant
#'   for market segments, a location for envirotypes)
#' @param k integer, number of segments
#' @param log character, columns to log-transform
#' @param seed integer
#' @param stability_runs integer, number of subsamples
#' @return list with segment (integer, 1 is the largest group), profile (mean
#'   covariates and n per segment) and stability
build_segments = function(x, k, log = character(), seed = 1, stability_runs = 50) {

  set.seed(seed)

  x = as.data.frame(lapply(x, as.numeric))
  z = x
  for (v in log) z[[v]] = log1p(pmax(z[[v]], 0))
  for (v in names(z)) z[[v]][is.na(z[[v]])] = stats::median(z[[v]], na.rm = TRUE)
  z = scale(z)
  z[, attr(z, "scaled:scale") == 0] = 0

  lab = stats::kmeans(z, centers = k, nstart = 20)$cluster

  ari = vapply(seq_len(stability_runs), function(i) {
    idx = sample(nrow(z), floor(0.8 * nrow(z)))
    adjusted_rand(lab[idx], stats::kmeans(z[idx, , drop = FALSE], centers = k, nstart = 5)$cluster)
  }, numeric(1))

  # label 1 is the largest group
  ord_lab = order(-tabulate(lab, k))
  segment = match(lab, ord_lab)

  profile = stats::aggregate(x, list(segment = segment), mean, na.rm = TRUE)
  profile$n = as.vector(table(segment))

  list(segment = segment, profile = profile, stability = mean(ari))

}
