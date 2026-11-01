# Simulate tricot data from a Plackett-Luce model with known worth
#
# Each participant ranks three of the varieties. Choices follow the
# Plackett-Luce model exactly, through the Gumbel trick, a participant's
# utility is the log-worth plus Gumbel noise, and the item with the highest
# utility is the best.

simulate_tricot = function(n = 600,
                           n_items = 10,
                           spread = 1,
                           seed = 2026) {

  set.seed(seed)

  items = c("Check", paste0("Variety ", sprintf("%02d", seq_len(n_items - 1))))

  log_worth = sort(stats::rnorm(n_items, sd = spread), decreasing = TRUE)
  # the check is a middle-of-the-range variety
  log_worth = c(stats::median(log_worth), log_worth[-ceiling(n_items / 2)])
  names(log_worth) = items

  alloc = t(replicate(n, sample(items, 3)))
  colnames(alloc) = c("item_A", "item_B", "item_C")

  gumbel = function(n) -log(-log(stats::runif(n)))

  bw = t(apply(alloc, 1, function(x) {
    u = log_worth[x] + gumbel(3)
    o = order(u, decreasing = TRUE)
    c(LETTERS[o[1]], LETTERS[o[3]])
  }))
  colnames(bw) = c("overall_pos", "overall_neg")

  dat = data.frame(id = seq_len(n), alloc, bw, stringsAsFactors = FALSE)

  attr(dat, "truth") = log_worth - log_worth["Check"]

  dat

}

if (sys.nframe() == 0L) {
  dat = simulate_tricot()
  utils::write.csv(dat, "data/synthetic-tricot.csv", row.names = FALSE)
}
