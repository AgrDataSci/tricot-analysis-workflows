# Functions for the use case "How many participants do we need?"
# Status: tested-on-synthetic-data (see tests/)
# Decision record: decisions/ranking.md
#
# Scientific intent
#   Tell a trial team, and the participants themselves, how much each extra
#   answer improves the ranking of the items. The question comes up when
#   response rates are low and the team needs to know whether the current
#   answers are enough, or how many more to aim for.
#
# Approach
#   1. Fit a Plackett-Luce model to the answers available now, or take
#      log-worths from an earlier trial or from expert expectation.
#   2. Treat these log-worths as the truth. Simulate tricot trials of n
#      participants, each testing k items drawn at random and ranking them
#      following the Plackett-Luce model.
#   3. Refit the model to every simulated trial and compare the estimated
#      order with the truth: Kendall tau over all items, whether the best
#      item is found, and how many of the true top items are recovered.
#   4. Repeat for several n and read off the n that reaches a target.
#
# Why simulate fresh participants instead of subsampling the data
#   A tempting shortcut is to draw subsamples of the real answers and compare
#   each fit with the fit on all answers. Subsamples share participants with
#   the full data, so agreement rises towards 1 as n approaches the number of
#   answers, whatever the true accuracy. It measures overlap, not accuracy,
#   and cannot say anything about more answers than the trial has.
#   naive_subsample_agreement() is kept to show this in the tests.
#
# Assumptions
#   - The truth is the fitted model. With few answers its worths are shrunk
#     towards each other (PlackettLuce pseudo-data), so the curve is
#     conservative. Report the curve as expected accuracy if the current
#     estimates were true, not as a property of the trial.
#   - Items are allocated at random. A balanced design (each item tested
#     equally often) does slightly better at small n.
#   - Participants are independent and follow the Plackett-Luce model.
#
# Alternatives
#   - Analytical power for one pairwise comparison with the check, from the
#     standard error of the log-worth difference.
#   - A Bayesian prior on worths from earlier trials of the same crop.
#
# Limits
#   - Accuracy depends on how different the items really are. When they are
#     close, no realistic number of participants separates them.
#   - It says nothing about whether those who do not answer differ from those
#     who do. Non-response can bias results even when n is large.


#' Build tricot rankings from best and worst answers
build_rankings = function(data, items = c("item_A", "item_B", "item_C"),
                          input = c("overall_pos", "overall_neg")) {

  gosset::rank_tricot(data = data, items = items, input = input,
                      validate.rankings = TRUE)

}


#' Simulate tricot rankings from known log-worths
#'
#' Each participant gets k items at random. Adding Gumbel noise to the
#' log-worths and ordering gives rankings that follow the Plackett-Luce model
#' exactly.
#'
#' @param logworth named numeric vector, one value per item
#' @param n number of participants
#' @param k items per participant
#' @return a PlackettLuce rankings object, one ranking per participant
#' @examples
#' R = simulate_tricot_rankings(c(a = 0, b = 1, c = -1, d = 0.5), n = 20)
simulate_tricot_rankings = function(logworth, n, k = 3) {

  items = names(logworth)
  p = length(items)

  if (is.null(items) || p < k) {
    stop("logworth must be a named vector with at least ", k, " items")
  }

  m = matrix(0L, nrow = n, ncol = p, dimnames = list(NULL, items))

  for (r in seq_len(n)) {
    s = sample.int(p, k)
    u = logworth[s] - log(-log(stats::runif(k)))
    m[r, s] = rank(-u)
  }

  PlackettLuce::as.rankings(m)

}


#' Expected accuracy of the ranking by number of participants
#'
#' @param logworth named log-worths taken as the truth
#' @param sizes numbers of participants to simulate
#' @param nsim simulated trials for each size
#' @param top how many of the best items to look at
#' @param k items per participant
#' @param seed random seed
#' @return a data.frame with one row per size: mean Kendall tau with the
#'  true order and its 10th and 90th percentiles, the share of trials that
#'  find the true best item, and the mean share of the true top items found
design_curve = function(logworth, sizes, nsim = 100, top = 3, k = 3, seed = 1) {

  set.seed(seed)

  best = names(logworth)[which.max(logworth)]
  true_top = names(sort(logworth, decreasing = TRUE))[seq_len(top)]

  rows = lapply(sizes, function(n) {

    r = vapply(seq_len(nsim), function(b) {
      R = simulate_tricot_rankings(logworth, n, k)
      est = stats::coef(PlackettLuce::PlackettLuce(R), log = TRUE)[names(logworth)]
      est_top = names(sort(est, decreasing = TRUE))[seq_len(top)]
      c(tau = stats::cor(est, logworth, method = "kendall"),
        winner = as.numeric(names(est)[which.max(est)] == best),
        top = length(intersect(est_top, true_top)) / top)
    }, numeric(3))

    data.frame(n = n,
               tau = mean(r["tau", ]),
               tau_lower = unname(stats::quantile(r["tau", ], 0.10)),
               tau_upper = unname(stats::quantile(r["tau", ], 0.90)),
               p_winner = mean(r["winner", ]),
               top_found = mean(r["top", ]))

  })

  do.call("rbind", rows)

}


#' Smallest number of participants that reaches a target
#'
#' @param curve output of design_curve()
#' @param target value to reach
#' @param what column of the curve, for example "p_winner" or "tau"
#' @return the smallest n with curve[[what]] >= target, or NA
n_for_target = function(curve, target = 0.8, what = "p_winner") {

  ok = curve$n[curve[[what]] >= target]

  if (length(ok) == 0) NA_integer_ else as.integer(min(ok))

}


#' Agreement of subsamples with the full data (biased, for comparison only)
#'
#' Do not use this to judge accuracy. Subsamples share participants with the
#' full data, so agreement rises towards 1 as n approaches the number of
#' answers. See the header of this file.
naive_subsample_agreement = function(R, sizes, nsim = 50, seed = 1) {

  set.seed(seed)

  full = stats::coef(PlackettLuce::PlackettLuce(R), log = TRUE)

  vapply(sizes, function(n) {
    mean(vapply(seq_len(nsim), function(b) {
      i = sample.int(nrow(unclass(R)), n)
      est = stats::coef(PlackettLuce::PlackettLuce(R[i, ]), log = TRUE)[names(full)]
      stats::cor(est, full, method = "kendall")
    }, numeric(1)))
  }, numeric(1))

}
