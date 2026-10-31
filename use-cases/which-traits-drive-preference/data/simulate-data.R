# Simulate tricot data with a known trait structure
#
# The truth built into the simulation
#   - Overall preference is driven by yield (strong) and taste (moderate).
#   - Grain colour has no direct effect, but it is correlated with yield at
#     variety level, so its marginal correlation with overall preference is
#     positive. A sound analysis should drop it after accounting for yield.
#   - Drought tolerance has no effect and no correlation with the drivers.
#
# Each participant tests three of the varieties and reports the best and the
# worst for every trait, as letters A, B, C, in the ClimMob tricot format.

simulate_tricot = function(n = 800,
                           n_items = 12,
                           b_yield = 1,
                           b_taste = 0.6,
                           noise = 0.7,
                           seed = 2026) {

  set.seed(seed)

  items = paste0("Variety ", sprintf("%02d", seq_len(n_items)))

  # latent variety values
  yield = stats::rnorm(n_items)
  taste = stats::rnorm(n_items)
  colour = 0.8 * yield + 0.6 * stats::rnorm(n_items)
  drought = stats::rnorm(n_items)

  # balanced random allocation of three varieties per participant
  alloc = t(replicate(n, sample(items, 3)))
  colnames(alloc) = c("item_A", "item_B", "item_C")

  best_worst = function(score) {
    o = order(score, decreasing = TRUE)
    c(LETTERS[o[1]], LETTERS[o[3]])
  }

  out = matrix("", nrow = n, ncol = 10)
  colnames(out) = paste0(rep(c("overall", "yield", "taste", "colour", "drought"),
                             each = 2),
                         c("_pos", "_neg"))

  for (i in seq_len(n)) {

    k = match(alloc[i, ], items)

    # what this participant perceives on each plot
    p_yield = yield[k] + stats::rnorm(3, sd = noise)
    p_taste = taste[k] + stats::rnorm(3, sd = noise)
    p_colour = colour[k] + stats::rnorm(3, sd = noise)
    p_drought = drought[k] + stats::rnorm(3, sd = noise)

    # overall preference is built from the perceived drivers
    p_overall = b_yield * p_yield + b_taste * p_taste +
      stats::rnorm(3, sd = noise)

    out[i, ] = c(best_worst(p_overall),
                 best_worst(p_yield),
                 best_worst(p_taste),
                 best_worst(p_colour),
                 best_worst(p_drought))

  }

  dat = data.frame(id = seq_len(n),
                   alloc,
                   out,
                   gender = sample(c("Man", "Woman"), n, replace = TRUE),
                   stringsAsFactors = FALSE)

  attr(dat, "truth") = data.frame(item = items,
                                  yield = yield,
                                  taste = taste,
                                  colour = colour,
                                  drought = drought)

  dat

}

if (sys.nframe() == 0L) {
  dat = simulate_tricot()
  utils::write.csv(dat, "data/synthetic-tricot.csv", row.names = FALSE)
}
