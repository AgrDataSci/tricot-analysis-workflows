# Simulate on-farm trials with a known breakthrough product
# Status: reviewed-not-executed, translated from script/other-language/breakthrough.py
#
# Variety 01 is a breakthrough product (true gain 40%), Variety 02 sits on the
# threshold (25%), Variety 03 gains 10% and the others are close to the check.
# Each farm grows three varieties. Log yield is the check level plus the
# variety gain, a variety-by-trial interaction, a farm effect and a residual.
# Plots fail (zero yield) with the same probability for every variety. Farmers
# rank their three plots, with a utility that follows the true gain plus
# Gumbel noise, as in the Plackett-Luce model.
#
# The true reliability is the probability that a plot of the variety beats a
# plot of the check on the same farm, by any margin (rel) or by 25%
# (rel_threshold). It shows that a variety can be a breakthrough product on
# average and still miss the 25% gain on many farms.

simulate_breakthrough = function(n_trials = 6,
                                 farms_per_trial = 150,
                                 seed = 2026,
                                 sd_farm = 0.6,
                                 sd_resid = 0.35,
                                 sd_gxt = 0.05,
                                 p_fail = 0.05,
                                 check_yield = 800,
                                 check_in_all = TRUE) {

  set.seed(seed)

  gain = c("Check" = 1, "Variety 01" = 1.40, "Variety 02" = 1.25,
           "Variety 03" = 1.10, "Variety 04" = 1.03, "Variety 05" = 1,
           "Variety 06" = 0.97, "Variety 07" = 0.90)
  items = names(gain)

  gumbel = function(n) -log(-log(stats::runif(n)))

  dat = lapply(seq_len(n_trials), function(t) {

    # the first trial lacks the check when check_in_all is FALSE
    avail = if (check_in_all || t > 1) items else items[-1]
    gxt = stats::rnorm(length(items), sd = sd_gxt)
    names(gxt) = items

    farms = lapply(seq_len(farms_per_trial), function(f) {
      grown = sample(avail, 3)
      ly = log(check_yield) + log(gain[grown]) + gxt[grown] - gxt["Check"] +
        stats::rnorm(1, sd = sd_farm) + stats::rnorm(3, sd = sd_resid)
      u = 2 * log(gain[grown]) + gumbel(3)
      y = ifelse(stats::runif(3) < p_fail, 0, exp(ly))
      data.frame(trial = paste0("T", t),
                 farm = paste0("T", t, "-", f),
                 genotype = grown,
                 yield = as.numeric(y),
                 rank = rank(-u),
                 stringsAsFactors = FALSE)
    })

    do.call(rbind, farms)
  })

  dat = do.call(rbind, dat)
  rownames(dat) = NULL

  sd_d = sqrt(2 * sd_resid^2 + 2 * sd_gxt^2)

  truth = data.frame(genotype = items[-1],
                     gain_ratio = as.numeric(gain[-1]),
                     rel = stats::pnorm(log(gain[-1]) / sd_d),
                     rel_threshold = stats::pnorm((log(gain[-1]) - log(1.25)) / sd_d),
                     row.names = NULL)

  attr(dat, "truth") = truth

  dat

}

if (sys.nframe() == 0L) {
  dat = simulate_breakthrough()
  utils::write.csv(dat, "data/synthetic-on-farm-yield.csv", row.names = FALSE)
}
