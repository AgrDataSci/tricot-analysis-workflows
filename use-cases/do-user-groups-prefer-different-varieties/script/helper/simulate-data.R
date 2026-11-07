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


# Simulate tricot data with a predefined group confounded with site
#
# The truth built into the simulation
#   - Two sites with opposite preferences, as when the same varieties meet
#     different growing conditions.
#   - Women are concentrated in the south site (share_women_south) and men
#     in the north (share_women_north), as in trials where women were
#     recruited mostly in some districts.
#   - With gender_effect = FALSE gender has no effect on preference within a
#     site, so any difference between women and men pooled over sites is a
#     site effect. With gender_effect = TRUE women give two varieties a
#     higher worth in both sites.
simulate_known_groups = function(n_per_site = 300,
                                 n_items = 8,
                                 share_women_north = 0.15,
                                 share_women_south = 0.75,
                                 gender_effect = FALSE,
                                 shift = 1.5,
                                 seed = 2026) {

  set.seed(seed)

  items = c("Check", paste0("Variety ", sprintf("%02d", seq_len(n_items - 1))))

  site = rep(c("north", "south"), each = n_per_site)
  p_women = ifelse(site == "north", share_women_north, share_women_south)
  gender = ifelse(stats::runif(length(site)) < p_women, "woman", "man")
  n = length(site)

  lw_north = stats::setNames(seq(1.2, -1.2, length.out = n_items), items)
  lw_south = stats::setNames(rev(lw_north), items)
  bonus = stats::setNames(rep(0, n_items), items)
  if (gender_effect) bonus[c("Variety 03", "Variety 05")] = shift

  alloc = t(replicate(n, sample(items, 3)))
  colnames(alloc) = c("item_A", "item_B", "item_C")

  gumbel = function(n) -log(-log(stats::runif(n)))

  bw = t(vapply(seq_len(n), function(i) {
    lw = if (site[i] == "north") lw_north else lw_south
    if (gender[i] == "woman") lw = lw + bonus
    u = lw[alloc[i, ]] + gumbel(3)
    o = order(u, decreasing = TRUE)
    c(LETTERS[o[1]], LETTERS[o[3]])
  }, character(2)))
  colnames(bw) = c("overall_pos", "overall_neg")

  data.frame(id = seq_len(n), alloc, bw, site = site, gender = gender,
             stringsAsFactors = FALSE)

}
