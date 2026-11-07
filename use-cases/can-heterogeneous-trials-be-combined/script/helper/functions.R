# Functions for the use case "Can trials with different measurements and
# designs be combined into one analysis?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/data-synthesis.md
#
# Scientific intent
#   Combine trials that measured the same response in different ways (scores,
#   counts, days, yield), at different levels (block or trial mean), with
#   different genotype sets and in different years, into one analysis that
#   links genotype performance to the climate each evaluation experienced.
#   The approach follows Brown et al. (2023), Agronomy Journal, where 31
#   heterogeneous Musa trials from 1990 to 2019 were pooled to explain the
#   reaction of 62 genotypes to black leaf streak disease, and the earlier
#   idea in cgiar-csi-2020 (wheat.R), where station yields from 255 sites in
#   India were pooled as rankings.
#
# Approach
#   1. Screen trials against the inclusion criteria (genotype response to the
#      trait, coordinates, start and end dates, genotype overlap).
#   2. Choose one metric per trial, the one with the most data and the one
#      most trials share, and record its direction (higher or lower is better).
#   3. Rank genotypes within each evaluation unit. A block is a unit when
#      block data exist, otherwise the trial mean. Each crop cycle is a unit.
#   4. Aggregate the rankings into one sparse matrix.
#   5. Keep the genotypes that are connected to the rest and evaluated often
#      enough, and resolve ties.
#   6. Compute climate indices over the evaluation period of each unit, and
#      drop covariates with near-zero variance.
#   7. Fit a Plackett-Luce tree in explanatory mode (large nodes, Bonferroni,
#      pseudo-rankings for connectivity) and assess its stability by
#      refitting on subsamples.
#   8. Report worth per node, reliability against a reference genotype, the
#      number of evaluations per genotype per node and the goodness of fit.
#
# Assumptions
#   - The metrics chosen in different trials order the genotypes the same way
#     for the trait of interest. Converting to ranks removes the scale, not a
#     disagreement between metrics.
#   - The evaluation period of a unit is known (planting to shooting, flowering
#     or harvest), so climate can be computed over it.
#   - Genotype names are harmonized across trials before ranking.
#
# Alternatives
#   - Analyse metric data with mixed models when the metrics are the same and
#     the designs are compatible.
#   - Network meta-analysis of pairwise contrasts, which keeps the metric scale.
#   - Plackett-Luce with genotype covariates (pladmm) to explain worth by
#     genotype traits instead of partitioning by environment.
#
# Limits
#   - Ranks lose the magnitude of differences. Two genotypes a hair apart and
#     two genotypes far apart contribute the same information.
#   - A split in the tree shows that the ranking differs between conditions.
#     It does not identify the mechanism, and climate is confounded with site,
#     year, management and the people who scored the trial.
#   - Genotypes seen in few units, or only under one condition, have wide
#     uncertainty and no claim can be made about them under the other.


#' Screen trials against the inclusion criteria
#'
#' Brown et al. (2023) kept a trial only when it had (1) evaluations of the
#' genotype response to the trait, (2) coordinates, (3) start and end dates of
#' the evaluation period and (4) genotypes that overlap with other trials.
#'
#' @param meta data.frame with one row per trial and columns trial, lon, lat,
#'   start, end
#' @param obs data.frame of observations with columns trial, genotype and the
#'   metric columns
#' @param metrics character, names of the metric columns in obs
#' @param min_overlap integer, a trial must share at least this many genotypes
#'   with the other trials
#' @return data.frame with one row per trial and a logical column per
#'   criterion, plus include
screen_trials = function(meta, obs, metrics, min_overlap = 1) {

  trials = meta$trial

  has_response = vapply(trials, function(t) {
    x = obs[obs$trial == t, metrics, drop = FALSE]
    nrow(x) > 0 && any(!is.na(unlist(x)))
  }, logical(1))

  has_coords = !is.na(meta$lon) & !is.na(meta$lat)

  has_dates = !is.na(meta$start) & !is.na(meta$end) & meta$end > meta$start

  overlap = vapply(trials, function(t) {
    g = unique(obs$genotype[obs$trial == t])
    others = unique(obs$genotype[obs$trial != t])
    sum(g %in% others)
  }, integer(1))

  out = data.frame(trial = trials,
                   has_response = has_response,
                   has_coords = has_coords,
                   has_dates = has_dates,
                   overlap = overlap,
                   row.names = NULL)
  out$include = out$has_response & out$has_coords & out$has_dates &
    out$overlap >= min_overlap
  out

}


#' Choose one metric per trial
#'
#' When a trial reports several metrics of the same trait, keep the one with
#' the most non-missing values. Ties go to the metric that comes first in
#' `priority`, so that the pooled data set rests on as few metrics as
#' possible. Brown et al. (2023) used this rule to end with four metrics for
#' 110 rankings.
#'
#' @param obs data.frame of observations with columns trial, genotype and the
#'   metric columns
#' @param priority character, metric names in order of preference
#' @return data.frame with trial, metric and n (non-missing values)
choose_metric = function(obs, priority) {

  do.call("rbind", lapply(split(obs, obs$trial), function(x) {
    n = vapply(priority, function(m) sum(!is.na(x[[m]])), integer(1))
    best = names(n)[n == max(n)][1]
    data.frame(trial = x$trial[1], metric = best, n = n[[best]],
               row.names = NULL)
  }))

}


#' Rank genotypes within each evaluation unit from a numeric metric
#'
#' Each unit (trial by cycle by block, or trial by cycle when only means are
#' reported) becomes one ranking. The direction of the metric decides the
#' order. For a disease index, lower is better. For days to symptoms or for
#' yield, higher is better. Ties keep the same rank (ties.method "min").
#'
#' gosset::rank_numeric() did the same in Brown et al. (2023). It is written
#' out here so the decision stays visible.
#'
#' @param obs data.frame with columns unit, genotype and value
#' @param higher_is_better logical, one value per row of obs or a single value
#' @return list of named integer vectors, one per unit
rank_within_units = function(obs, higher_is_better) {

  if (length(higher_is_better) == 1) {
    higher_is_better = rep(higher_is_better, nrow(obs))
  }
  obs$higher_is_better = higher_is_better

  obs = obs[!is.na(obs$value), ]

  lapply(split(obs, obs$unit), function(x) {
    if (length(unique(x$higher_is_better)) > 1) {
      stop("the metric direction must be the same within a unit", call. = FALSE)
    }
    v = if (x$higher_is_better[1]) -x$value else x$value
    r = rank(v, ties.method = "min")
    stats::setNames(as.integer(r), x$genotype)
  })

}


#' Aggregate rankings into one sparse matrix
#'
#' Columns are the genotypes seen in any unit, rows are the units. A genotype
#' not evaluated in a unit gets 0, which PlackettLuce reads as absent.
#'
#' @param ranks list from rank_within_units
#' @return integer matrix with units as row names
rank_matrix = function(ranks) {

  genotypes = sort(unique(unlist(lapply(ranks, names))))
  M = matrix(0L, nrow = length(ranks), ncol = length(genotypes),
             dimnames = list(names(ranks), genotypes))
  for (i in seq_along(ranks)) {
    M[i, names(ranks[[i]])] = ranks[[i]]
  }
  M

}


#' Resolve ties in a rank matrix
#'
#' PlackettLuce handles ties up to order four, but many ties in the low ranks
#' can stop the model from converging. Brown et al. (2023) kept one genotype
#' at random from each tied set and dropped the others from that unit, and
#' found that which one was kept did not change the results. The project
#' scripts also tried dropping every tied genotype, kept here as "drop_all".
#' With "keep", ties are left for PlackettLuce.
#'
#' Ranks are renumbered so they are consecutive after dropping.
#'
#' @param M rank matrix from rank_matrix
#' @param method "keep_one", "drop_all" or "keep"
#' @param seed integer for the random choice
#' @return list with the matrix and the number of entries dropped
resolve_ties = function(M, method = c("keep_one", "drop_all", "keep"),
                        seed = 123) {

  method = match.arg(method)
  if (method == "keep") return(list(M = M, dropped = 0L))

  set.seed(seed)
  dropped = 0L

  for (i in seq_len(nrow(M))) {
    r = M[i, ]
    present = r > 0
    tied_values = unique(r[present][duplicated(r[present])])
    for (v in tied_values) {
      idx = which(r == v)
      drop = if (method == "keep_one") idx[-sample(length(idx), 1)] else idx
      r[drop] = 0L
      dropped = dropped + length(drop)
    }
    present = r > 0
    r[present] = rank(r[present], ties.method = "min")
    M[i, ] = as.integer(r)
  }

  list(M = M, dropped = dropped)

}


#' Keep genotypes that are connected and evaluated often enough
#'
#' A Plackett-Luce model needs a strongly connected network of wins and
#' losses. Genotypes outside the main component are removed. Genotypes present
#' in fewer than `min_presence` of the units are removed too, because very
#' rare genotypes have unstable worth and weaken the network when the tree
#' partitions the data. Brown et al. (2023) kept genotypes present in at least
#' 5% of the units, a threshold found by searching for the smallest value that
#' let the model converge while losing the least data. The two filters are
#' applied until nothing changes, since removing a genotype can disconnect
#' another.
#'
#' Units left with fewer than two genotypes are removed.
#'
#' @param M rank matrix
#' @param min_presence numeric in [0, 1]
#' @return list with the matrix, the genotypes removed and why
filter_network = function(M, min_presence = 0.05) {

  removed = data.frame(genotype = character(0), reason = character(0))

  repeat {
    changed = FALSE

    presence = colMeans(M > 0)
    rare = names(presence)[presence < min_presence]
    if (length(rare)) {
      removed = rbind(removed, data.frame(genotype = rare, reason = "rare"))
      M = M[, !colnames(M) %in% rare, drop = FALSE]
      changed = TRUE
    }

    M = M[rowSums(M > 0) >= 2, , drop = FALSE]

    R = PlackettLuce::as.rankings(M)
    conn = PlackettLuce::connectivity(PlackettLuce::adjacency(R), verbose = FALSE)
    main = which.max(tabulate(conn$membership))
    out = names(conn$membership)[conn$membership != main]
    if (length(out)) {
      removed = rbind(removed, data.frame(genotype = out, reason = "disconnected"))
      M = M[, !colnames(M) %in% out, drop = FALSE]
      M = M[rowSums(M > 0) >= 2, , drop = FALSE]
      changed = TRUE
    }

    if (!changed) break
  }

  # renumber in case a removal left gaps in a unit
  for (i in seq_len(nrow(M))) {
    present = M[i, ] > 0
    M[i, present] = rank(M[i, present], ties.method = "min")
  }
  storage.mode(M) = "integer"

  list(M = M, removed = removed)

}


#' Evaluation windows per unit, with later cycles starting where the
#' previous one ended
#'
#' The climate that matters is the climate of the evaluation period, from
#' planting to the end of the evaluation (shooting, flowering or harvest). For
#' a ratoon or second cycle the period starts at the end of the previous cycle
#' of the same trial, not at the original planting date.
#'
#' @param units data.frame with columns unit, trial, cycle, start, end (Date)
#' @return the same data.frame with start adjusted and span in days
evaluation_window = function(units) {

  units = units[order(units$trial, units$cycle), ]
  for (t in unique(units$trial)) {
    i = which(units$trial == t)
    if (length(i) > 1) {
      for (k in 2:length(i)) {
        units$start[i[k]] = units$end[i[k - 1]]
      }
    }
  }
  units$span = as.integer(units$end - units$start)
  units

}


#' Climate indices over the evaluation window of each unit
#'
#' Rainfall indices (climatrends::rainfall) and temperature indices
#' (climatrends::temperature) over the window. Brown et al. (2023) also
#' averaged relative humidity at five times of day, which AgERA5 provides.
#' Relative humidity is not simulated here. Add it as extra columns when the
#' data have it.
#'
#' @param rain matrix [unit, day] of daily rainfall, dates as column names
#' @param temp array [unit, day, 2] of daily maximum and minimum temperature
#' @param start Date vector, window start per unit
#' @param span integer vector, window length per unit
#' @return data.frame of indices, one row per unit
climate_indices = function(rain, temp, start, span) {

  rr = climatrends::rainfall(rain, day.one = start, span = span)
  tt = climatrends::temperature(temp, day.one = start, span = span)
  cbind(as.data.frame(rr), as.data.frame(tt))

}


#' Drop covariates with near-zero variance and report strong correlations
#'
#' Covariates that barely vary cannot split the data and only add noise to
#' the stability assessment. Brown et al. (2023) removed them with
#' caret::nearZeroVar. The same rule is written out here. Pairs of covariates
#' with absolute correlation above `cor_cutoff` are reported, not removed,
#' because the tree picks one of them and the report explains which others
#' could have been chosen instead.
#'
#' @param x data.frame of covariates
#' @param freq_cutoff ratio of the most common value to the second most common
#' @param unique_cutoff percent of unique values below which a variable is
#'   near-zero variance
#' @param cor_cutoff absolute correlation to report
#' @return list with the covariates kept, the names removed and the
#'   correlated pairs
screen_covariates = function(x, freq_cutoff = 95 / 5, unique_cutoff = 10,
                             cor_cutoff = 0.9) {

  nzv = vapply(x, function(v) {
    v = v[!is.na(v)]
    tab = sort(table(v), decreasing = TRUE)
    freq_ratio = if (length(tab) > 1) tab[1] / tab[2] else Inf
    pct_unique = 100 * length(tab) / length(v)
    freq_ratio > freq_cutoff && pct_unique < unique_cutoff
  }, logical(1))

  kept = x[, !nzv, drop = FALSE]

  cm = stats::cor(kept, use = "pairwise.complete.obs")
  cm[upper.tri(cm, diag = TRUE)] = NA
  idx = which(abs(cm) > cor_cutoff, arr.ind = TRUE)
  pairs = data.frame(a = rownames(cm)[idx[, 1]], b = colnames(cm)[idx[, 2]],
                     r = cm[idx], row.names = NULL)

  list(covariates = kept, removed = names(x)[nzv], correlated = pairs)

}


#' Fit a Plackett-Luce tree in explanatory mode
#'
#' Brown et al. (2023) used alpha 0.05 with Bonferroni correction, a minimum
#' node size of 35% of the units, and two pseudo-rankings so that the network
#' stays connected after a split. The pseudo-rankings also shrink the worth
#' estimates, so reliability is conservative. Large nodes are deliberate. The
#' model explains, it does not predict, and small nodes with a sparse network
#' give unstable worth.
#'
#' @param M rank matrix after filter_network and resolve_ties
#' @param covariates data.frame, one row per unit, in the same order as M
#' @param alpha significance level for a split
#' @param bonferroni logical
#' @param minsize_prop minimum node size as a proportion of the units
#' @param npseudo number of pseudo-rankings
#' @details PlackettLuce must be attached with library("PlackettLuce"),
#'   because pltree() looks up its fitting functions on the search path
fit_synthesis_tree = function(M, covariates, alpha = 0.05, bonferroni = TRUE,
                              minsize_prop = 0.35, npseudo = 2) {

  R = PlackettLuce::as.rankings(M)
  G = PlackettLuce::group(R, seq_len(nrow(M)))
  d = cbind(G, covariates)
  f = stats::as.formula(paste("G ~", paste(names(covariates), collapse = " + ")))

  # pltree() re-evaluates its control arguments by name, so pass the values
  # themselves through do.call() rather than variables of this function
  do.call(PlackettLuce::pltree,
          list(formula = f, data = d, alpha = alpha, bonferroni = bonferroni,
               minsize = floor(nrow(M) * minsize_prop), npseudo = npseudo))

}


#' Variables used in the splits of a tree, in order of the nodes
split_variables = function(tree) {

  ids = partykit::nodeids(tree)
  inner = ids[!ids %in% partykit::nodeids(tree, terminal = TRUE)]
  if (length(inner) == 0) return(character(0))
  vars = names(tree$data)
  unlist(partykit::nodeapply(tree, ids = inner, function(n) {
    vars[partykit::split_node(n)$varid]
  }))

}


#' Cut point of the first split, NA when the tree has no split
first_cutpoint = function(tree) {

  ids = partykit::nodeids(tree)
  inner = ids[!ids %in% partykit::nodeids(tree, terminal = TRUE)]
  if (length(inner) == 0) return(NA_real_)
  s = partykit::split_node(partykit::node_party(tree))
  b = partykit::breaks_split(s)
  if (is.null(b)) NA_real_ else b[1]

}


#' Stability of the tree under subsampling
#'
#' Recursive partitioning is unstable. A small change in the data can change
#' the split. Following Philipp et al. (2018) and the stablelearner package,
#' as used in Brown et al. (2023), the tree is refitted on B subsamples of a
#' proportion v of the units, and the share of refits in which each variable
#' is used for a split is reported, with the first cut points. The original
#' tree is stable when its split variable is selected in most refits. Brown et
#' al. (2023) used B = 1000 and v = 0.8, and the split variable was selected
#' in 855 of 1000 refits.
#'
#' Subsampling without replacement, rather than bootstrap, keeps the network
#' of each refit as close as possible to the original.
#'
#' @param M rank matrix
#' @param covariates data.frame
#' @param B number of refits
#' @param v proportion of units in each subsample
#' @param seed integer
#' @param ... passed to fit_synthesis_tree
#' @return list with the relative frequency per variable, the first cut points
#'   and the number of refits with no split
split_stability = function(M, covariates, B = 100, v = 0.8, seed = 123, ...) {

  set.seed(seed)
  n = nrow(M)
  size = floor(n * v)

  res = lapply(seq_len(B), function(b) {
    i = sort(sample(n, size))
    Mi = M[i, , drop = FALSE]
    # a genotype absent from the subsample cannot be in the model
    Mi = Mi[, colSums(Mi > 0) > 0, drop = FALSE]
    tree = fit_synthesis_tree(Mi, covariates[i, , drop = FALSE], ...)
    list(vars = split_variables(tree), cut = first_cutpoint(tree))
  })

  vars = unlist(lapply(res, function(x) unique(x$vars)))
  freq = sort(table(factor(vars, levels = names(covariates))), decreasing = TRUE)
  freq = data.frame(variable = names(freq), frequency = as.numeric(freq) / B,
                    row.names = NULL)
  freq = freq[freq$frequency > 0, ]

  first = vapply(res, function(x) if (length(x$vars)) x$vars[1] else NA_character_,
                 character(1))
  cuts = data.frame(variable = first,
                    cutpoint = vapply(res, function(x) x$cut, numeric(1)))
  cuts = cuts[!is.na(cuts$variable), ]

  list(frequency = freq, cutpoints = cuts, no_split = sum(is.na(first)))

}


#' Worth per node relative to a reference genotype
#'
#' @param tree pltree
#' @param ref character, the reference genotype
#' @return data.frame with genotype and one column per terminal node, log worth
worth_by_node = function(tree, ref) {

  nodes = partykit::nodeids(tree, terminal = TRUE)
  w = t(stats::coef(tree, ref = ref, log = TRUE))
  colnames(w) = paste0("node_", nodes)
  data.frame(genotype = rownames(w), w, row.names = NULL)

}


#' Reliability per node, with a fallback when quasi-variances fail
#'
#' gosset::reliability() gives the probability of outperforming the reference
#' with quasi-standard errors from qvcalc. In sparse pooled data a node can
#' have genotypes with so few evaluations, or so clearly ordered, that the
#' information matrix is not positive definite and qvcalc stops. The
#' reliability itself only needs the worth, so it is then computed from the
#' node coefficients without standard errors, with a warning. Report it as
#' such, and do not read a reliability without a standard error as precise.
#'
#' @param tree pltree
#' @param ref character, the reference genotype
#' @return data.frame with node, item, reliability, reliabilitySE (NA in the
#'   fallback) and worth
node_reliability = function(tree, ref) {

  out = tryCatch(gosset::reliability(tree, ref = ref), error = function(e) NULL)

  if (!is.null(out)) {
    return(data.frame(out[, c("node", "item", "reliability", "reliabilitySE", "worth")],
                      row.names = NULL))
  }

  warning("quasi-variances could not be computed, reliability is reported ",
          "from the worth without standard errors", call. = FALSE)

  nodes = partykit::nodeids(tree, terminal = TRUE)
  do.call("rbind", lapply(nodes, function(k) {
    lw = stats::coef(tree, node = k, ref = ref, log = TRUE)
    worth = exp(lw) / sum(exp(lw))
    data.frame(node = k, item = names(worth),
               reliability = as.numeric(worth / (worth[ref] + worth)),
               reliabilitySE = NA_real_, worth = as.numeric(worth),
               row.names = NULL)
  }))

}


#' Number of evaluations per genotype per terminal node
#'
#' The distribution of evaluations across nodes is unbalanced in pooled data,
#' and it explains why some genotypes have wide uncertainty in one node.
#'
#' @param tree pltree
#' @param M rank matrix used to fit the tree
#' @return data.frame with genotype and one column per terminal node
evaluations_by_node = function(tree, M) {

  node = as.integer(stats::predict(tree, type = "node"))
  nodes = sort(unique(node))
  counts = vapply(nodes, function(k) colSums(M[node == k, , drop = FALSE] > 0),
                  numeric(ncol(M)))
  counts = matrix(counts, ncol = length(nodes),
                  dimnames = list(colnames(M), paste0("node_", nodes)))
  data.frame(genotype = rownames(counts), counts, row.names = NULL)

}


#' Goodness of fit of the tree on the data it was fitted to
#'
#' Pseudo R2 (gosset::pseudoR2) and the average Kendall tau between the
#' observed rankings and the rankings predicted from the worth of each node
#' (gosset::kendallTau). Both are optimistic because they use the training
#' data. Use them to compare trees, not to claim predictive skill.
#'
#' @param tree pltree
#' @param M rank matrix used to fit the tree
#' @return data.frame with one row
goodness_of_fit = function(tree, M) {

  pr2 = gosset::pseudoR2(tree)
  pred = stats::predict(tree, type = "rank")
  pred = pred[, colnames(M), drop = FALSE]
  kt = gosset::kendallTau(M, pred)
  data.frame(loglik = pr2$logLik,
             loglik_null = pr2$logLikNull,
             mcfadden = pr2$McFadden,
             cragg_uhler = pr2$CraggUhler,
             kendall_tau = kt$kendallTau,
             row.names = NULL)

}


#' Dot plot of log worth per node, genotypes sorted by the first node
#'
#' One panel per terminal node. Point size follows the number of evaluations
#' of the genotype in that node, so a genotype with a high worth and a single
#' evaluation is visibly weaker evidence.
#'
#' @param worth data.frame from worth_by_node
#' @param evals data.frame from evaluations_by_node
#' @param cutpoint numeric, first cut point for the panel titles
#' @param variable character, first split variable for the panel titles
plot_worth_by_node = function(worth, evals, cutpoint = NA, variable = NA) {

  nodes = grep("^node_", names(worth), value = TRUE)
  o = order(worth[[nodes[1]]])
  worth = worth[o, ]
  evals = evals[match(worth$genotype, evals$genotype), ]
  rng = range(unlist(worth[nodes]), na.rm = TRUE)

  op = graphics::par(mfrow = c(1, length(nodes)),
                     mar = c(4, 8, 3, 1), oma = c(0, 0, 0, 0))
  on.exit(graphics::par(op))

  for (k in seq_along(nodes)) {
    n = evals[[nodes[k]]]
    graphics::plot(worth[[nodes[k]]], seq_len(nrow(worth)), xlim = rng,
                   yaxt = "n", ylab = "", xlab = "log worth against the reference",
                   pch = 19, cex = 0.6 + 1.5 * n / max(evals[nodes], 1),
                   col = ifelse(n == 0, "grey70", "grey20"),
                   main = paste0(nodes[k], if (!is.na(variable))
                     paste0("  (", variable, if (k == 1) " <= " else " > ",
                            round(cutpoint, 1), ")")))
    graphics::abline(v = 0, lty = 2, col = "grey50")
    if (k == 1) graphics::axis(2, at = seq_len(nrow(worth)), labels = worth$genotype,
                               las = 1, cex.axis = 0.8)
  }

}
