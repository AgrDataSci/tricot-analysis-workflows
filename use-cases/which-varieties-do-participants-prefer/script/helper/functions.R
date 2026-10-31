# Functions for the use case "Which varieties do participants prefer, and how sure are we?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/ranking.md
#
# Scientific intent
#   Estimate the relative preference (worth) of each variety from tricot
#   rankings, with uncertainty, and translate it into quantities a breeder or
#   extension officer can act on, the probability of outperforming a check
#   and the probability of being among the top varieties.
#
# Approach
#   1. Build rankings with gosset::rank_tricot() and validate them.
#   2. Check the comparison network is connected.
#   3. Fit a Plackett-Luce model (PlackettLuce::PlackettLuce).
#   4. Report log-worth with quasi-standard errors (qvcalc), so intervals can
#      be compared between any two varieties, not only against the reference.
#   5. Report the probability of beating a check (gosset::reliability).
#   6. Resample participants to get the probability that each variety is
#      ranked first, or among the top k, which expresses ranking uncertainty
#      directly.
#   7. Report model adequacy with McFadden's pseudo R2 against a model in
#      which all varieties are equally preferred.
#
# Assumptions
#   - Participants are independent and evaluate their own three items.
#   - Preferences follow the Plackett-Luce model, so the odds of choosing one
#     variety over another do not depend on the third variety in the set.
#   - A single worth per variety describes the whole population. When groups
#     differ, see the use case on user groups.
#
# Alternatives
#   - Mean ranks or win counts, only as a quick description. They ignore which
#     varieties each participant compared, so they are biased under
#     incomplete block designs.
#   - Thurstonian (probit) models, under investigation, not equivalent.
#   - Plackett-Luce trees or covariate models when worth depends on context.
#
# Limits
#   - Worth is relative to the set of varieties tested. It is not yield, and
#     it does not predict adoption or market share.
#   - A variety tested by few participants has wide intervals. Report them.


#' Build tricot rankings and report data loss
build_rankings = function(data, items, input) {

  R = gosset::rank_tricot(data = data,
                          items = items,
                          input = input,
                          validate.rankings = TRUE)

  m = unclass(R)
  valid = rowSums(m > 0) >= 2

  list(rankings = R[valid, ],
       n_valid = sum(valid),
       n_rows = nrow(data),
       n_per_item = colSums(m[valid, , drop = FALSE] > 0))

}


#' Check that the comparison network is connected
check_connectivity = function(R) {

  conn = PlackettLuce::connectivity(R, verbose = FALSE)

  connected = conn$no == 1

  if (!connected) {
    warning("the comparison network has ", conn$no,
            " disconnected clusters of items. ",
            "Worth estimates are not comparable across clusters",
            call. = FALSE)
  }

  connected

}


#' Worth table with quasi-standard errors and probability of beating a check
#'
#' @param mod a PlackettLuce model
#' @param ref character, the check variety
#' @param level confidence level for intervals on the log-worth scale
#' @return data.frame ordered from most to least preferred
worth_table = function(mod, ref, level = 0.95) {

  qv = qvcalc::qvcalc(mod, ref = ref)$qvframe

  z = stats::qnorm(1 - (1 - level) / 2)

  rel = gosset::reliability(mod, ref = ref)

  out = data.frame(item = rownames(qv),
                   log_worth = qv$estimate,
                   quasi_se = qv$quasiSE,
                   lower = qv$estimate - z * qv$quasiSE,
                   upper = qv$estimate + z * qv$quasiSE,
                   worth = exp(qv$estimate) / sum(exp(qv$estimate)),
                   p_beat_check = rel$reliability[match(rownames(qv), rel$item)],
                   row.names = NULL)

  out[order(out$log_worth, decreasing = TRUE), ]

}


#' Probability that each variety ranks first or within the top k
#'
#' Refits the model on bootstrap resamples of participants. This expresses
#' how stable the order of varieties is, which the interval of each single
#' variety does not show.
#'
#' @param R a rankings object
#' @param k integer, size of the top group
#' @param nboot integer, number of resamples
#' @param seed integer
#' @return data.frame with the probability of rank 1 and of top k per item
top_probability = function(R, k = 3, nboot = 200, seed = 1) {

  items = colnames(unclass(R))
  n = nrow(R)

  set.seed(seed)

  ranks = vapply(seq_len(nboot), function(b) {
    i = sample.int(n, n, replace = TRUE)
    co = stats::coef(PlackettLuce::PlackettLuce(R[i, ]), log = TRUE)
    # items absent from a resample get the lowest rank
    co = co[items]
    co[is.na(co)] = -Inf
    rank(-co, ties.method = "random")
  }, numeric(length(items)))

  data.frame(item = items,
             p_first = rowMeans(ranks == 1),
             p_top_k = rowMeans(ranks <= k),
             row.names = NULL)

}


#' McFadden pseudo R2 and likelihood-ratio test against equal preference
#'
#' A low value is common for tricot data and does not mean the ranking of
#' varieties is wrong. It means individual choices are noisy, which is why
#' many participants are needed.
#'
#' @param mod a PlackettLuce model
#' @return data.frame with log-likelihoods, McFadden R2 and the LR test
model_adequacy = function(mod) {

  ll = mod$loglik
  ll0 = mod$null.loglik
  df = mod$df.null - mod$df.residual

  dev = -2 * (ll0 - ll)

  data.frame(loglik = ll,
             null_loglik = ll0,
             mcfadden_r2 = 1 - ll / ll0,
             deviance = dev,
             df = df,
             p_value = stats::pchisq(dev, df, lower.tail = FALSE))

}
