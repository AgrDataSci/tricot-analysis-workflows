# Simulate tricot data with two user groups that prefer different varieties
#
# The truth built into the simulation
#   - Participants belong to one of two segments, market-oriented (40%) and
#     subsistence-oriented (60%).
#   - Segments differ in covariates (farm size, share of harvest sold, distance
#     to market) with overlap, plus one covariate unrelated to segment.
#   - Each segment has its own log-worth for the varieties. With
#     heterogeneity = FALSE both segments share the same worth.
#   - Rankings follow the Plackett-Luce model of the participant's segment.

simulate_tricot = function(n = 800,
                           n_items = 8,
                           heterogeneity = TRUE,
                           separation = 1,
                           seed = 2026) {

  set.seed(seed)

  items = c("Check", paste0("Variety ", sprintf("%02d", seq_len(n_items - 1))))

  segment = sample(c("market", "subsistence"), n, replace = TRUE,
                   prob = c(0.4, 0.6))
  m = segment == "market"

  covariates = data.frame(
    farm_size = exp(stats::rnorm(n, ifelse(m, 1.0, 0) * separation, 0.5)),
    share_sold = stats::plogis(stats::rnorm(n, ifelse(m, 1.2, -1.2) * separation, 0.8)),
    market_distance = exp(stats::rnorm(n, ifelse(m, 0.5, 1.7) * separation, 0.5)),
    household_head = factor(sample(c("woman", "man"), n, replace = TRUE))
  )

  lw_market = stats::setNames(seq(1.2, -1.2, length.out = n_items), items)
  lw_subsistence = if (heterogeneity) rev(lw_market) else lw_market
  names(lw_subsistence) = items

  alloc = t(replicate(n, sample(items, 3)))
  colnames(alloc) = c("item_A", "item_B", "item_C")

  gumbel = function(n) -log(-log(stats::runif(n)))

  bw = t(vapply(seq_len(n), function(i) {
    lw = if (m[i]) lw_market else lw_subsistence
    u = lw[alloc[i, ]] + gumbel(3)
    o = order(u, decreasing = TRUE)
    c(LETTERS[o[1]], LETTERS[o[3]])
  }, character(2)))
  colnames(bw) = c("overall_pos", "overall_neg")

  dat = data.frame(id = seq_len(n), alloc, bw, covariates,
                   stringsAsFactors = FALSE)

  attr(dat, "segment") = segment

  dat

}

if (sys.nframe() == 0L) {
  dat = simulate_tricot()
  utils::write.csv(dat, "data/synthetic-tricot.csv", row.names = FALSE)
}
