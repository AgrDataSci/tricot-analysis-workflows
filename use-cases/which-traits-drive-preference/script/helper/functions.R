# Functions for the use case "Which traits drive overall preference?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/trait-associations.md
#
# Scientific intent
#   Quantify how each farmer-ranked trait is associated with overall
#   preference, both on its own (marginal) and after accounting for the
#   other traits (partial). The partial association is what answers the
#   question "which traits drive preference", because traits are often
#   correlated with each other at variety level.
#
# Approach
#   1. Build one rankings object per trait from tricot best/worst data.
#   2. Compute Kendall tau per participant (row) for every pair of traits.
#      Rows are the independent units, so uncertainty is estimated by
#      resampling rows (participants), not from an effective N.
#   3. Average tau across rows, weighting each row by its number of
#      compared pairs, as gosset::kendallTau() does.
#   4. Convert tau to a Pearson-scale correlation, r = sin(pi * tau / 2),
#      so the matrix can be inverted to obtain partial correlations.
#   5. Backward selection on partial correlations with overall preference,
#      dropping the weakest trait until every remaining trait has a
#      bootstrap interval that excludes zero.
#
# Assumptions
#   - Each row is one participant evaluating the same set of items for all
#     traits. Rankings of different traits in the same row are dependent,
#     which is exactly what the row bootstrap preserves.
#   - The sin transformation is exact for bivariate normal latent scores and
#     approximate otherwise.
#   - All ranked traits are evaluated by farmers. Quantitative traits need
#     observer, instrument, scale, timing and units recorded before use.
#
# Alternatives
#   - Marginal Kendall tau only (gosset::kendallTau), when the aim is
#     description rather than attribution among correlated traits.
#   - Plackett-Luce models of overall preference with trait worths as item
#     covariates (PlackettLuce::pladmm), when variety-level drivers are the
#     question.
#   - Regression of overall rankings on trait rankings within row
#     (rank-ordered logit), when row-level effects are the question.
#
# Limits
#   - Associations are not causal. A trait can be selected because it is a
#     proxy for an unranked characteristic.
#   - Partial correlations depend on which traits are in the set. Results
#     change if a trait is added or removed, so report the full set tested.
#   - Selection by significance is not operational importance. Report effect
#     sizes with intervals, not only the selected list.


#' Build rankings for several tricot traits
#'
#' @param data data.frame in tricot format
#' @param items character, the columns holding the item names (e.g. item_A, item_B, item_C)
#' @param traits named list, each element the two columns with best and worst (e.g. c("yield_pos", "yield_neg"))
#' @return a list with the rankings per trait and the number of valid rows per trait
build_trait_rankings = function(data, items, traits) {

  stopifnot(is.list(traits), !is.null(names(traits)))

  R = lapply(traits, function(input) {
    gosset::rank_tricot(data = data,
                        items = items,
                        input = input,
                        validate.rankings = TRUE)
  })

  # rows with an invalid entry (NA, tie, best equal to worst, letters other
  # than A, B, C) are kept as empty rankings by rank_tricot. Count them so the
  # loss of information is reported, not hidden
  n_valid = vapply(R, function(r) {
    m = unclass(r)
    sum(rowSums(m > 0) >= 2)
  }, numeric(1))

  list(rankings = R, n_valid = n_valid, n_rows = nrow(data))

}


#' Check that the comparison network of a rankings object is connected
#'
#' A disconnected network means some items were never compared, directly or
#' indirectly, with others. Worth estimates and rank correlations across the
#' disconnected parts are then not comparable.
#'
#' @param R a rankings object
#' @return logical, TRUE if connected, with a warning otherwise
check_connectivity = function(R) {

  conn = PlackettLuce::connectivity(R, verbose = FALSE)

  connected = conn$no == 1

  if (!connected) {
    warning("the comparison network has ", conn$no,
            " disconnected clusters of items. ",
            "Associations across clusters are not comparable",
            call. = FALSE)
  }

  connected

}


#' Kendall tau per row between two rankings objects
#'
#' Computes tau-b for each row using only the items ranked in both rankings.
#' Returns the tau and the number of item pairs compared in each row, which
#' is the weight used to average across rows.
#'
#' @param x, y rankings objects with the same rows and items
#' @return data.frame with columns tau and pairs, one row per participant
row_kendall = function(x, y) {

  X = unclass(x)
  Y = unclass(y)

  stopifnot(identical(dim(X), dim(Y)), identical(colnames(X), colnames(Y)))

  J = ncol(X)
  n = nrow(X)

  S = numeric(n)
  nx = numeric(n)
  ny = numeric(n)
  pairs = numeric(n)

  for (a in seq_len(J - 1)) {
    for (b in (a + 1):J) {
      ok = X[, a] > 0 & X[, b] > 0 & Y[, a] > 0 & Y[, b] > 0
      if (!any(ok)) next
      dx = sign(X[, a] - X[, b]) * ok
      dy = sign(Y[, a] - Y[, b]) * ok
      S = S + dx * dy
      nx = nx + (dx != 0)
      ny = ny + (dy != 0)
      pairs = pairs + ok
    }
  }

  tau = ifelse(nx > 0 & ny > 0, S / sqrt(nx * ny), NA_real_)

  data.frame(tau = tau, pairs = ifelse(is.na(tau), 0, pairs))

}


#' Row-level Kendall tau for all pairs of traits
#'
#' @param R list of rankings objects, one per trait, same rows
#' @return list with tau and weights, each an array rows x traits x traits
row_kendall_all = function(R) {

  p = length(R)
  n = nrow(unclass(R[[1]]))
  nm = names(R)

  tau = array(NA_real_, dim = c(n, p, p), dimnames = list(NULL, nm, nm))
  w = array(0, dim = c(n, p, p), dimnames = list(NULL, nm, nm))

  for (i in seq_len(p)) {
    tau[, i, i] = 1
    w[, i, i] = 1
    if (i == p) next
    for (j in (i + 1):p) {
      k = row_kendall(R[[i]], R[[j]])
      tau[, i, j] = tau[, j, i] = k$tau
      w[, i, j] = w[, j, i] = k$pairs
    }
  }

  list(tau = tau, w = w)

}


#' Pair-weighted average of row-level tau
#'
#' @param rk output of row_kendall_all
#' @param rows integer, the rows to use (allows bootstrap resampling)
#' @return a traits x traits matrix of average Kendall tau
average_tau = function(rk, rows = NULL) {

  if (is.null(rows)) rows = seq_len(dim(rk$tau)[1])

  tau = rk$tau[rows, , , drop = FALSE]
  w = rk$w[rows, , , drop = FALSE]

  tau[is.na(tau)] = 0

  num = apply(tau * w, c(2, 3), sum)
  den = apply(w, c(2, 3), sum)

  out = num / den
  out[den == 0] = NA_real_
  diag(out) = 1

  out

}


#' Partial correlations from a Kendall tau matrix
#'
#' Converts tau to the Pearson scale with r = sin(pi * tau / 2), forces the
#' matrix to be positive definite by clipping small eigenvalues, and inverts
#' it to obtain partial correlations.
#'
#' @param tau a square matrix of Kendall tau
#' @param tol smallest eigenvalue allowed
#' @return a square matrix of partial correlations
tau_to_partial = function(tau, tol = 1e-6) {

  if (anyNA(tau)) {
    stop("some trait pairs were never ranked by the same participants, ",
         "partial correlations cannot be computed", call. = FALSE)
  }

  r = sin(pi * tau / 2)

  e = eigen(r, symmetric = TRUE)

  if (min(e$values) < tol) {
    v = pmax(e$values, tol)
    r = e$vectors %*% diag(v) %*% t(e$vectors)
    r = stats::cov2cor(r)
  }

  P = -stats::cov2cor(solve(r))
  diag(P) = 1
  dimnames(P) = dimnames(tau)

  P

}


#' Marginal and partial association of each trait with a reference trait
#'
#' @param rk output of row_kendall_all
#' @param reference character, the reference trait (usually overall preference)
#' @param traits character, traits to include (reference included)
#' @param nboot integer, number of bootstrap resamples of participants
#' @param level numeric, confidence level of the intervals
#' @param seed integer, seed for reproducible resampling
#' @return data.frame with one row per trait
trait_association = function(rk,
                             reference,
                             traits = dimnames(rk$tau)[[2]],
                             nboot = 500,
                             level = 0.95,
                             seed = 1) {

  stopifnot(reference %in% traits, length(traits) >= 2)

  rk = list(tau = rk$tau[, traits, traits, drop = FALSE],
            w = rk$w[, traits, traits, drop = FALSE])

  others = setdiff(traits, reference)

  stat = function(rows) {
    tau = average_tau(rk, rows)
    marg = tau[others, reference]
    part = if (length(traits) > 2) {
      tau_to_partial(tau)[others, reference]
    } else {
      sin(pi * marg / 2)
    }
    c(marg, part)
  }

  est = stat(NULL)

  n = dim(rk$tau)[1]

  set.seed(seed)

  boot = vapply(seq_len(nboot), function(b) {
    stat(sample.int(n, n, replace = TRUE))
  }, numeric(length(est)))

  boot = matrix(boot, nrow = length(est))

  a = (1 - level) / 2
  ci = apply(boot, 1, stats::quantile, probs = c(a, 1 - a), na.rm = TRUE)

  k = length(others)

  data.frame(trait = others,
             tau = est[seq_len(k)],
             tau_lower = ci[1, seq_len(k)],
             tau_upper = ci[2, seq_len(k)],
             partial = est[k + seq_len(k)],
             partial_lower = ci[1, k + seq_len(k)],
             partial_upper = ci[2, k + seq_len(k)],
             row.names = NULL)

}


#' Backward selection of traits on partial correlation with a reference
#'
#' At each step the trait whose bootstrap interval for the partial
#' correlation is closest to including zero (smallest lower bound relative
#' to its sign) is removed, and the partial correlations are recomputed with
#' the remaining traits. Selection stops when every remaining interval
#' excludes zero.
#'
#' @inheritParams trait_association
#' @return list with the final table, the removal order and every step
backward_selection = function(rk,
                              reference,
                              traits = dimnames(rk$tau)[[2]],
                              nboot = 500,
                              level = 0.95,
                              seed = 1) {

  keep = traits
  removed = character(0)
  steps = list()

  repeat {

    tab = trait_association(rk, reference, keep,
                            nboot = nboot, level = level, seed = seed)

    steps[[length(steps) + 1]] = tab

    # distance of each interval from zero, negative when it includes zero
    gap = pmax(tab$partial_lower, -tab$partial_upper)

    if (all(gap > 0) || nrow(tab) == 1) break

    drop = tab$trait[which.min(gap)]
    removed = c(removed, drop)
    keep = setdiff(keep, drop)

  }

  if (!any(gap > 0)) {
    message("no trait has a partial association with ", reference,
            " whose interval excludes zero")
  }

  list(selected = tab[gap > 0, ],
       removed = removed,
       steps = steps)

}
