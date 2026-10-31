# Functions for the use case "Do different groups of users prefer different varieties?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/socioeconomic-heterogeneity.md and decisions/ranking.md
#
# Scientific intent
#   Find groups of participants (user personas) from socioeconomic,
#   environmental and management covariates, then ask whether variety
#   preferences differ between those groups. The groups are defined without
#   using the rankings, so the preference test is not circular.
#
# Approach
#   1. Cluster participants on their covariates with Gower distance and
#      partitioning around medoids (cluster::daisy, cluster::pam), which
#      handles mixed numeric and categorical data.
#   2. Search k = 2 to 5 and judge each solution on stability (bootstrap
#      adjusted Rand index), separation (average silhouette width) and
#      representation (size of the smallest cluster).
#   3. Choose the most stable defensible k, or report that none is defensible.
#   4. Test whether one Plackett-Luce model for everyone fits worse than one
#      model per cluster, with the likelihood-ratio test
#      (gosset::likelihood_ratio) and a permutation test that does not rely
#      on the chi-squared approximation.
#   5. Report worth per cluster and the cluster profiles.
#
# Assumptions
#   - Covariates are measured before or independently of the rankings.
#   - Each participant belongs to one group. Mixed membership is not modelled.
#
# Alternatives
#   - Plackett-Luce trees (PlackettLuce::pltree), which split on covariates
#     using the rankings. They find groups that differ in preference but give
#     binary hierarchical partitions that may not be actionable personas.
#   - Latent class ranking models, where groups and preferences are
#     estimated jointly.
#   - Covariate models of worth when a single covariate is the question.
#
# Limits
#   - A significant test means the preference structure differs, not that the
#     difference matters for a decision. Look at the worth per cluster.
#   - Clusters depend on the covariates chosen and their scaling. Report the
#     covariates used.
#   - Statistical stability is necessary but not sufficient. Clusters must be
#     interpretable and usable by the people who will act on them.


#' Build tricot rankings, keeping valid rows only
build_rankings = function(data, items, input) {

  R = gosset::rank_tricot(data = data,
                          items = items,
                          input = input,
                          validate.rankings = TRUE)

  valid = rowSums(unclass(R) > 0) >= 2

  list(rankings = R[valid, ], valid = valid)

}


#' Adjusted Rand index between two partitions
adjusted_rand = function(a, b) {

  tab = table(a, b)
  n = sum(tab)
  comb2 = function(x) x * (x - 1) / 2

  sum_ij = sum(comb2(tab))
  sum_a = sum(comb2(rowSums(tab)))
  sum_b = sum(comb2(colSums(tab)))

  expected = sum_a * sum_b / comb2(n)
  maximum = (sum_a + sum_b) / 2

  if (maximum == expected) return(1)

  (sum_ij - expected) / (maximum - expected)

}


#' Cluster participants on covariates for a range of k
#'
#' @param covariates data.frame of covariates, numeric or factor
#' @param k integer vector of numbers of clusters to try
#' @param nboot integer, bootstrap resamples for stability
#' @param min_share smallest acceptable cluster share
#' @param min_stability smallest acceptable mean adjusted Rand index
#' @param seed integer
#' @return list with a table of criteria per k, the chosen k (or NA) and the
#'   cluster labels for every k
cluster_search = function(covariates,
                          k = 2:5,
                          nboot = 30,
                          min_share = 0.10,
                          min_stability = 0.75,
                          seed = 1) {

  d = cluster::daisy(covariates, metric = "gower")
  n = nrow(covariates)

  set.seed(seed)

  labels = list()
  crit = data.frame()

  for (kk in k) {

    fit = cluster::pam(d, k = kk, diss = TRUE)
    lab = fit$clustering
    labels[[as.character(kk)]] = lab

    # stability, recluster bootstrap samples and compare with the
    # original labels of the same participants
    ari = vapply(seq_len(nboot), function(b) {
      i = sample.int(n, n, replace = TRUE)
      db = cluster::daisy(covariates[i, , drop = FALSE], metric = "gower")
      lb = cluster::pam(db, k = kk, diss = TRUE, cluster.only = TRUE)
      adjusted_rand(lab[i], lb)
    }, numeric(1))

    crit = rbind(crit,
                 data.frame(k = kk,
                            silhouette = fit$silinfo$avg.width,
                            stability = mean(ari),
                            min_share = min(table(lab)) / n))

  }

  crit$defensible = crit$stability >= min_stability & crit$min_share >= min_share

  if (any(crit$defensible)) {
    ok = crit[crit$defensible, ]
    # most stable, then best separated, then fewest clusters
    ok = ok[order(-round(ok$stability, 2), -ok$silhouette, ok$k), ]
    chosen = ok$k[1]
  } else {
    chosen = NA_integer_
    message("no cluster solution meets the stability and representation criteria")
  }

  list(criteria = crit, k = chosen, labels = labels)

}


#' Does preference differ between groups?
#'
#' Combines the likelihood-ratio test from gosset with a permutation test,
#' which shuffles group labels among participants and refits, so it does not
#' depend on the chi-squared approximation.
#'
#' @param R a rankings object
#' @param group vector of group labels, one per ranking
#' @param nperm integer, number of permutations
#' @param seed integer
preference_heterogeneity = function(R, group, nperm = 200, seed = 1) {

  # gosset::likelihood_ratio splits by observation, so pass the rankings as
  # grouped_rankings with one group per participant
  G = PlackettLuce::group(R, seq_len(nrow(R)))
  lr = gosset::likelihood_ratio(G, split = group)

  fit_dev = function(g) {
    ll_all = PlackettLuce::PlackettLuce(R)$loglik
    ll_g = sum(vapply(split(seq_len(nrow(R)), g), function(i) {
      PlackettLuce::PlackettLuce(R[i, ])$loglik
    }, numeric(1)))
    -2 * (ll_all - ll_g)
  }

  obs = fit_dev(group)

  set.seed(seed)
  perm = vapply(seq_len(nperm), function(b) fit_dev(sample(group)), numeric(1))

  data.frame(deviance = obs,
             df = lr$DF_delta,
             p_chisq = lr[["Pr(>Chisq)"]],
             p_permutation = (sum(perm >= obs) + 1) / (nperm + 1))

}


#' Log-worth per group, relative to a reference item
worth_by_group = function(R, group, ref) {

  out = lapply(split(seq_len(nrow(R)), group), function(i) {
    stats::coef(PlackettLuce::PlackettLuce(R[i, ]), ref = ref, log = TRUE)
  })

  out = do.call("cbind", out)
  colnames(out) = paste0("cluster_", colnames(out))

  data.frame(item = rownames(out), out, row.names = NULL, check.names = FALSE)

}


#' Profile of each cluster, mean of numeric and share of categorical covariates
cluster_profile = function(covariates, group) {

  prof = lapply(names(covariates), function(v) {
    x = covariates[[v]]
    if (is.numeric(x)) {
      m = tapply(x, group, mean)
      data.frame(variable = v, t(m), check.names = FALSE)
    } else {
      tab = prop.table(table(x, group), 2)
      data.frame(variable = paste0(v, " = ", rownames(tab)),
                 unclass(tab)[, , drop = FALSE], check.names = FALSE,
                 row.names = NULL)
    }
  })

  out = do.call("rbind", prof)
  names(out)[-1] = paste0("cluster_", names(out)[-1])
  out

}
