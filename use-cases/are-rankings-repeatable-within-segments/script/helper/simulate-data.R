# Simulate tricot rankings of eight concepts from two segments
# Status: tested-on-synthetic-data, translated from script/other-language/repeatability.py
#
# Segment "market" prefers concepts C1 to C3 and segment "home" prefers C6 to
# C8. Their preferences partly cancel in the pooled data, so the pooled worths
# are closer together and less repeatable at a given number of participants.
# "noise" is a label unrelated to preference, a segmentation that should gain
# nothing. contrast scales how far the segments differ. With contrast = 0 the
# two segments are identical and segmenting should gain nothing either.

simulate_segments = function(n = 1200, seed = 2026, share = 0.5, contrast = 1) {

  set.seed(seed)

  items = paste0("C", 1:8)
  common = c(0.4, 0.2, 0, 0, 0, 0, -0.2, -0.4)
  spec = c(0.8, 0.6, 0.4, 0, 0, -0.4, -0.6, -0.8) * contrast
  worth = cbind(market = common + spec, home = common - spec)
  rownames(worth) = items

  gumbel = function(n) -log(-log(stats::runif(n)))
  pos = c("A", "B", "C")

  dat = lapply(seq_len(n), function(i) {
    s = if (stats::runif(1) < share) "market" else "home"
    show = sample(8, 3)
    u = worth[show, s] + gumbel(3)
    o = order(-u)
    data.frame(id = i,
               item_1 = items[show[1]], item_2 = items[show[2]], item_3 = items[show[3]],
               best = pos[o[1]], worst = pos[o[3]],
               segment = s,
               noise = if (stats::runif(1) < 0.5) "x" else "y",
               stringsAsFactors = FALSE)
  })

  dat = do.call(rbind, dat)
  attr(dat, "worth") = worth

  dat

}

if (sys.nframe() == 0L) {
  dat = simulate_segments()
  utils::write.csv(dat, "data/synthetic-concept-rankings.csv", row.names = FALSE)
}
