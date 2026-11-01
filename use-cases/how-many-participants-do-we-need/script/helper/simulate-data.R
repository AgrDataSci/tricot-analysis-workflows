# Synthetic data for the use case "How many participants do we need?"
#
# A trial with 12 items in which 93 participants registered and only 35
# returned their overall ranking, the situation that motivated this use case.
# Items differ moderately: log-worths from 0.8 to -0.8.

#' Simulate tricot answers in the ClimMob format
#'
#' @return a data.frame with item_A, item_B, item_C, overall_pos and
#'  overall_neg (best and worst as letters), NA for participants who did not
#'  answer. The true log-worth is in attr(, "truth")
simulate_tricot = function(n_registered = 93, n_answered = 35, n_items = 12, seed = 7) {

  set.seed(seed)

  items = sprintf("Item%02d", seq_len(n_items))
  truth = stats::setNames(seq(0.8, -0.8, length.out = n_items), items)

  pkg = t(replicate(n_registered, sample(items, 3)))

  best = worst = rep(NA_character_, n_registered)

  answered = sort(sample.int(n_registered, n_answered))

  for (r in answered) {
    u = truth[pkg[r, ]] - log(-log(stats::runif(3)))
    best[r] = LETTERS[which.max(u)]
    worst[r] = LETTERS[which.min(u)]
  }

  dat = data.frame(id = seq_len(n_registered),
                   item_A = pkg[, 1], item_B = pkg[, 2], item_C = pkg[, 3],
                   overall_pos = best, overall_neg = worst,
                   stringsAsFactors = FALSE)

  attr(dat, "truth") = truth

  dat

}
