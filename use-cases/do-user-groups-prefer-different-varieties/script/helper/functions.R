# Functions for the use case "Do different groups of users prefer different varieties?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/socioeconomic-heterogeneity.md and decisions/ranking.md
#
# Two entry points
#   A. Predefined groups (women and men, districts). Check the answers per
#      group, check confounding with site, test the groups within sites and
#      report worth with quasi-standard errors (functions at the end of
#      this file, from group_eligibility onwards).
#   B. User types from covariates, below.
#
# Scientific intent of part B
#   Find groups of participants (user personas) from socioeconomic,
#   environmental and management covariates, then ask whether variety
#   preferences differ between those groups. The groups are defined without
#   using the rankings, so the preference test is not circular.
#
# Approach
#   1. Cluster participants on numeric and categorical covariates together
#      with model-based clustering and variable selection (cluster_varsel,
#      VarSelLCM), choosing k by ICL. Variables unrelated to the clusters are
#      declared irrelevant and do not shape them.
#   2. Check which variables drive the partition (variable_influence), and
#      the size of the smallest cluster.
#   3. Test whether one Plackett-Luce model for everyone fits worse than one
#      model per cluster, with the likelihood-ratio test
#      (gosset::likelihood_ratio) and a permutation test that does not rely
#      on the chi-squared approximation, then test every pair of clusters.
#   4. Report worth per cluster and the cluster profiles.
#   Gower distance with PAM (cluster_search) and FAMD components
#   (famd_scores) are kept for comparison and for numeric-only data.
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
#     covariates used. With Gower distance a binary factor gets the same
#     weight as a whole continuous covariate and can define the partition on
#     its own, with perfect stability. Check with variable_influence().
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
#' @param metric distance for cluster::daisy, "gower" for mixed data or
#'   "euclidean" for component scores (see famd_scores)
#' @param seed integer
#' @return list with a table of criteria per k, the chosen k (or NA) and the
#'   cluster labels for every k
cluster_search = function(covariates,
                          k = 2:5,
                          nboot = 30,
                          min_share = 0.10,
                          min_stability = 0.75,
                          metric = "gower",
                          seed = 1) {

  d = cluster::daisy(covariates, metric = metric)
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
      db = cluster::daisy(covariates[i, , drop = FALSE], metric = metric)
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


#' Which variables drive the partition?
#'
#' Refits the clustering leaving out one variable at a time and compares each
#' partition with the full one (adjusted Rand index), and compares the full
#' partition with each categorical variable directly. A variable is flagged
#' when leaving it out changes the solution completely, or when the clusters
#' reproduce it almost exactly. This catches a single variable hijacking the
#' partition, whatever clustering method is used.
#'
#' @param covariates data.frame used for clustering
#' @param labels the full partition
#' @param cluster_fun function(covariates) returning a partition with the
#'   same number of clusters
#' @param max_factor_ari flag a categorical variable reproduced above this
#' @param min_loo_ari flag a variable whose removal drops agreement below this
variable_influence = function(covariates, labels, cluster_fun,
                              max_factor_ari = 0.8, min_loo_ari = 0.5) {

  vars = names(covariates)

  loo = vapply(vars, function(v) {
    if (length(vars) < 2) return(NA_real_)
    adjusted_rand(labels, cluster_fun(covariates[setdiff(vars, v)]))
  }, numeric(1))

  reproduced = vapply(vars, function(v) {
    x = covariates[[v]]
    if (is.numeric(x)) return(NA_real_)
    adjusted_rand(labels, x)
  }, numeric(1))

  out = data.frame(variable = vars,
                   ari_without = loo,
                   ari_with_variable = reproduced,
                   row.names = NULL)

  out$drives_partition = (!is.na(out$ari_with_variable) &
                            out$ari_with_variable > max_factor_ari) |
    (!is.na(out$ari_without) & out$ari_without < min_loo_ari)

  if (any(out$drives_partition)) {
    warning("the partition is driven by: ",
            paste(out$variable[out$drives_partition], collapse = ", "),
            ". Check whether this is the structure you want, or use a ",
            "method that weighs variables by relevance (cluster_varsel)",
            call. = FALSE)
  }

  out

}


#' Model-based clustering of mixed data with variable selection
#'
#' Fits latent class models for numeric and categorical variables together
#' (VarSelLCM::VarSelCluster) for each number of clusters and declares each
#' variable relevant or irrelevant to the clusters. An irrelevant variable
#' does not shape the partition, so a binary factor that is unrelated to the
#' segments cannot hijack it.
#'
#' The number of clusters is chosen by ICL by default. ICL penalizes
#' overlapping clusters, while BIC tends to split a segment into similar
#' subgroups. Both criteria are returned.
#'
#' @param covariates data.frame of numeric and factor columns
#' @param k integer vector, numbers of clusters to compare
#' @param criterion "ICL" (default) or "BIC", for choosing k
#' @param seed integer
#' @return list with the partition, the chosen k, the criteria per k and the
#'   relevant and irrelevant variables
cluster_varsel = function(covariates, k = 2:5, criterion = c("ICL", "BIC"), seed = 1) {

  criterion = match.arg(criterion)

  set.seed(seed)

  fits = lapply(k, function(g) {
    VarSelLCM::VarSelCluster(covariates, gvals = g, vbleSelec = TRUE,
                             crit.varsel = "BIC", nbcores = 1)
  })

  crit = data.frame(k = k,
                    BIC = vapply(fits, function(f) f@criteria@BIC, numeric(1)),
                    ICL = vapply(fits, function(f) f@criteria@ICL, numeric(1)))

  best = fits[[which.max(crit[[criterion]])]]

  labels = as.integer(best@partitions@zMAP)
  relevant = best@model@names.relevant

  list(labels = labels,
       k = k[which.max(crit[[criterion]])],
       criteria = crit,
       criterion = criterion,
       relevant = relevant,
       irrelevant = setdiff(names(covariates), relevant))

}


#' Do pairs of clusters differ in preference?
#'
#' Likelihood-ratio test of one Plackett-Luce model against one per cluster,
#' for every pair of clusters. Two clusters that do not differ in preference
#' may still be worth keeping apart for other reasons, but they do not need
#' different variety recommendations.
#'
#' @param R rankings
#' @param group vector of cluster labels, one per ranking
pairwise_heterogeneity = function(R, group) {

  g = sort(unique(group))
  pairs = utils::combn(g, 2)

  do.call("rbind", lapply(seq_len(ncol(pairs)), function(j) {
    keep = group %in% pairs[, j]
    Rj = R[keep, ]
    G = PlackettLuce::group(Rj, seq_len(nrow(Rj)))
    lr = gosset::likelihood_ratio(G, split = group[keep])
    data.frame(cluster_a = pairs[1, j], cluster_b = pairs[2, j],
               deviance = lr$deviance, df = lr$DF_delta,
               p_value = lr[["Pr(>Chisq)"]])
  }))

}


#' Clustering on components of a factor analysis of mixed data
#'
#' FAMD (FactoMineR) puts numeric and categorical variables on a common
#' scale. Clustering then runs on the leading components with Euclidean
#' distance. The number of components retained matters. Components carrying
#' a single binary variable can still dominate when they are kept, so check
#' the result with variable_influence().
#'
#' @param covariates data.frame of numeric and factor columns
#' @param ncp integer or NULL. NULL keeps components with eigenvalue above 1
#' @return data.frame of component scores, with the eigenvalues as attribute
famd_scores = function(covariates, ncp = NULL) {

  f = FactoMineR::FAMD(covariates, ncp = ncol(covariates) * 3, graph = FALSE)
  eig = f$eig[, 1]

  if (is.null(ncp)) ncp = max(1, sum(eig > 1))

  scores = as.data.frame(f$ind$coord[, seq_len(ncp), drop = FALSE])
  attr(scores, "eigenvalues") = eig
  scores

}


# ................................................................
# Predefined groups (women and men, districts, projects, age classes)
#
# When the groups were designed into the study there is nothing to cluster.
# The question is whether the groups differ in preference, and whether that
# difference belongs to the group or to where the group happens to be. The
# functions below follow the rules of the ClimMob report workflow
# (AgrDataSci/ClimMob-analysis, R/44_group_analysis.R)
#   - unlabelled answers are not compared
#   - a group needs at least min_n valid answers (20 by default)
#   - when the smallest group holds less than 25% of the answers the pooled
#     ranking is dominated by the largest group, so a balanced ranking is
#     reported beside it
# and add what the report does not do
#   - check whether the group is confounded with site or district
#   - test the group within strata, so a site effect is not read as a
#     group effect
#   - report worth with quasi-standard errors and the n behind each group
# ................................................................


#' Which groups have enough answers to be compared
#'
#' @param group grouping vector, one value per ranking
#' @param min_n minimum number of valid answers per group
#' @param exclude labels that are never compared
#' @return data.frame with group, n, share, eligible and reason
group_eligibility = function(group, min_n = 20,
                             exclude = c("Not specified", "")) {

  g = as.character(group)
  g = g[!is.na(g)]

  tb = table(g)
  labelled = sum(tb[!names(tb) %in% exclude])

  out = data.frame(group = names(tb),
                   n = as.integer(tb),
                   share = as.integer(tb) / labelled,
                   stringsAsFactors = FALSE)

  out$eligible = out$n >= min_n & !out$group %in% exclude
  out$reason = ifelse(out$group %in% exclude, "no_label",
                      ifelse(out$n < min_n, "too_few", "ok"))
  out$share[out$group %in% exclude] = NA

  out[order(-out$n), , drop = FALSE]

}


#' Is the group confounded with a stratum such as site or district?
#'
#' Cross-tabulates group and stratum and computes Cramer's V. A V above
#' max_v means the group mostly lives in some strata, so a difference in
#' preference between groups can be a difference between strata. Use
#' stratified_heterogeneity() before reading any group difference.
#'
#' @param group grouping vector
#' @param stratum stratum vector, same length
#' @param max_v Cramer's V above which the group is flagged as confounded
#' @return list with the table of counts, Cramer's V and the flag
confounding_check = function(group, stratum, max_v = 0.3) {

  tab = table(group = group, stratum = stratum)
  chi = suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  v = sqrt(as.numeric(chi) / (sum(tab) * (min(dim(tab)) - 1)))

  if (v > max_v) {
    warning("group and stratum are associated (Cramer's V = ", round(v, 2),
            "). Read group differences from stratified_heterogeneity()",
            call. = FALSE)
  }

  list(counts = tab, cramers_v = v, confounded = v > max_v)

}


#' Log-likelihood and degrees of freedom of one Plackett-Luce model per cell
cell_loglik = function(R, cell) {

  fits = lapply(split(seq_len(nrow(R)), cell, drop = TRUE), function(i) {
    stats::logLik(PlackettLuce::PlackettLuce(R[i, ]))
  })

  c(loglik = sum(vapply(fits, as.numeric, numeric(1))),
    df = sum(vapply(fits, function(l) attr(l, "df"), numeric(1))))

}


#' Do groups differ in preference within strata?
#'
#' Compares one model per stratum (site, district) with one model per stratum
#' and group. The difference is the evidence that the group matters once
#' location is held fixed. Strata where any group has fewer than min_cell
#' answers are left out and listed, because they cannot inform the
#' comparison. The permutation test shuffles group labels within each
#' stratum, so it keeps the stratum composition and does not rely on the
#' chi-squared approximation.
#'
#' @param R rankings
#' @param group grouping vector, one per ranking
#' @param stratum stratum vector, one per ranking
#' @param min_cell minimum answers per group within a stratum
#' @param nperm number of permutations, 0 to skip
#' @param seed integer
#' @return data.frame with deviance, df, p values, the strata used and the
#'   strata left out
stratified_heterogeneity = function(R, group, stratum, min_cell = 10,
                                    nperm = 200, seed = 1) {

  group = as.character(group)
  stratum = as.character(stratum)

  cells = table(stratum, group)
  ok = rownames(cells)[apply(cells, 1, function(x) all(x >= min_cell))]
  dropped = setdiff(rownames(cells), ok)

  if (length(ok) == 0) {
    stop("no stratum has at least ", min_cell, " answers in every group",
         call. = FALSE)
  }

  keep = stratum %in% ok
  R = R[keep, ]
  group = group[keep]
  stratum = stratum[keep]

  dev = function(g) {
    l0 = cell_loglik(R, stratum)
    l1 = cell_loglik(R, paste(stratum, g, sep = "::"))
    c(deviance = as.numeric(2 * (l1["loglik"] - l0["loglik"])),
      df = as.numeric(l1["df"] - l0["df"]))
  }

  obs = dev(group)

  p_perm = NA_real_
  if (nperm > 0) {
    set.seed(seed)
    idx = split(seq_along(group), stratum)
    perm = vapply(seq_len(nperm), function(b) {
      g = group
      for (i in idx) g[i] = g[i][sample.int(length(i))]
      dev(g)[["deviance"]]
    }, numeric(1))
    p_perm = (sum(perm >= obs[["deviance"]]) + 1) / (nperm + 1)
  }

  data.frame(deviance = obs[["deviance"]],
             df = obs[["df"]],
             p_chisq = stats::pchisq(obs[["deviance"]], obs[["df"]],
                                     lower.tail = FALSE),
             p_permutation = p_perm,
             n = nrow(R),
             strata_used = paste(ok, collapse = ", "),
             strata_dropped = paste(dropped, collapse = ", "))

}


#' Worth per group with quasi-standard errors and the n behind each estimate
#'
#' Log-worth relative to a reference item, with quasi-standard errors from
#' qvcalc so intervals can be compared between items. When qvcalc fails
#' (sparse data, a non positive definite information matrix) the estimate is
#' kept and the standard error is NA, with a warning. Never read an estimate
#' without a standard error as precise.
#'
#' @param R rankings
#' @param group grouping vector
#' @param ref reference item
#' @param level confidence level of the intervals
#' @return long data.frame with group, n, item, estimate, quasi_se, lower,
#'   upper and the number of answers in which the item was ranked
worth_with_intervals = function(R, group, ref, level = 0.95) {

  z = stats::qnorm(1 - (1 - level) / 2)
  M = unclass(R)

  out = lapply(split(seq_len(nrow(R)), as.character(group)), function(i) {

    m = PlackettLuce::PlackettLuce(R[i, ])
    est = stats::coef(m, ref = ref, log = TRUE)
    est = est[!grepl("^tie", names(est))]

    qse = tryCatch({
      qv = qvcalc::qvcalc(m, ref = match(ref, names(est)))$qvframe
      stats::setNames(qv$quasiSE, rownames(qv))[names(est)]
    }, error = function(e) {
      warning("quasi-standard errors could not be computed for a group, ",
              "estimates are reported without them", call. = FALSE)
      stats::setNames(rep(NA_real_, length(est)), names(est))
    })

    data.frame(n = length(i),
               item = names(est),
               estimate = as.numeric(est),
               quasi_se = as.numeric(qse),
               lower = as.numeric(est - z * qse),
               upper = as.numeric(est + z * qse),
               n_ranked = as.integer(colSums(M[i, names(est), drop = FALSE] > 0)),
               row.names = NULL)

  })

  out = do.call("rbind", Map(function(d, g) cbind(group = g, d), out, names(out)))
  rownames(out) = NULL
  out

}


#' Agreement between groups on the order of the items
#'
#' Kendall tau between the worth of each pair of groups, and how many of each
#' group's top items the other group shares. Groups can agree on the order
#' and still pick different top items, or the reverse. Report both.
#'
#' @param R rankings
#' @param group grouping vector
#' @param top number of top items compared
agreement_between_groups = function(R, group, top = 3) {

  w = lapply(split(seq_len(nrow(R)), as.character(group)), function(i) {
    x = stats::coef(PlackettLuce::PlackettLuce(R[i, ]), log = FALSE)
    x[!grepl("^tie", names(x))]
  })

  g = names(w)
  pairs = utils::combn(g, 2)

  do.call("rbind", lapply(seq_len(ncol(pairs)), function(j) {
    a = w[[pairs[1, j]]]
    b = w[[pairs[2, j]]]
    common = intersect(names(a), names(b))
    top_a = names(sort(a[common], decreasing = TRUE))[seq_len(top)]
    top_b = names(sort(b[common], decreasing = TRUE))[seq_len(top)]
    data.frame(group_a = pairs[1, j], group_b = pairs[2, j],
               kendall_tau = stats::cor(a[common], b[common], method = "kendall"),
               shared_top = length(intersect(top_a, top_b)),
               top = top)
  }))

}


#' Pooled and balanced rankings
#'
#' The pooled ranking weighs every answer equally, so the largest group
#' dominates it. The balanced ranking gives every group the same total
#' weight. Report both when the smallest group holds less than 25% of the
#' answers, as the ClimMob report does.
#'
#' @param R rankings
#' @param group grouping vector
balanced_ranking = function(R, group) {

  n = table(group)
  wt = 1 / as.numeric(n[as.character(group)])
  wt = wt / mean(wt)

  wp = stats::coef(PlackettLuce::PlackettLuce(R), log = FALSE)
  wb = stats::coef(PlackettLuce::PlackettLuce(R, weights = wt), log = FALSE)
  items = names(wp)[!grepl("^tie", names(wp))]

  out = data.frame(item = items,
                   worth_pooled = 100 * wp[items] / sum(wp[items]),
                   worth_balanced = 100 * wb[items] / sum(wb[items]),
                   row.names = NULL)
  out$rank_pooled = rank(-out$worth_pooled, ties.method = "min")
  out$rank_balanced = rank(-out$worth_balanced, ties.method = "min")

  out[order(out$rank_balanced), ]

}
