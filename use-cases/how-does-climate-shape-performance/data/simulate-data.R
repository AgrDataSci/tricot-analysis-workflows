# Simulate tricot data in which heat during the vegetative stage changes
# which varieties are preferred
#
# The truth built into the simulation
#   - Plots lie along an elevation gradient, so warmer plots are lower.
#   - Daily temperature and rainfall are generated for one year per plot.
#   - Flowering happens when accumulated degree days (base 10 C) reach 900.
#   - Two varieties are heat tolerant. When the highest day temperature
#     between planting and flowering (maxDT in climatrends) is above 35 C,
#     they are preferred, below it they are not. Rainfall has no effect on
#     preference.
#   - Plot rainfall is generated independently of temperature.

simulate_climate_trial = function(n = 900,
                                  n_items = 8,
                                  threshold = 35,
                                  effect = 1.5,
                                  seed = 2026) {

  set.seed(seed)

  dates = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
  nd = length(dates)

  # warmer and cooler sites
  elevation = stats::runif(n, 200, 1800)
  base_tmax = 37 - 0.006 * elevation
  season = 2 * sin(2 * pi * (seq_len(nd) - 30) / 365)

  tmax = t(vapply(seq_len(n), function(i) {
    base_tmax[i] + season + stats::rnorm(nd, sd = 1.5)
  }, numeric(nd)))
  tmin = tmax - 10 - matrix(stats::runif(n * nd, 0, 3), n, nd)

  temp = array(c(tmax, tmin), dim = c(n, nd, 2),
               dimnames = list(NULL, as.character(dates), c("tmax", "tmin")))

  wetness = stats::runif(n, 0.2, 0.6)
  rain = t(vapply(seq_len(n), function(i) {
    stats::rbinom(nd, 1, wetness[i]) * stats::rgamma(nd, shape = 0.8, scale = 12)
  }, numeric(nd)))
  dimnames(rain) = list(NULL, as.character(dates))

  planting = as.Date("2025-02-15") + sample(0:40, n, replace = TRUE)

  # true window and heat in that window
  gdd_daily = pmax((tmax + tmin) / 2 - 10, 0)
  start = match(as.character(planting), as.character(dates))
  flowering = vapply(seq_len(n), function(i) {
    which(cumsum(gdd_daily[i, start[i]:nd]) > 900)[1] - 1
  }, numeric(1))
  heat = vapply(seq_len(n), function(i) {
    max(tmax[i, start[i]:(start[i] + flowering[i] - 1)])
  }, numeric(1))

  items = c("Check", paste0("Variety ", sprintf("%02d", seq_len(n_items - 1))))
  tolerant = items[2:3]

  lw_base = stats::setNames(seq(0.6, -0.6, length.out = n_items), items)

  alloc = t(replicate(n, sample(items, 3)))
  colnames(alloc) = c("item_A", "item_B", "item_C")

  gumbel = function(k) -log(-log(stats::runif(k)))

  hot = heat > threshold

  bw = t(vapply(seq_len(n), function(i) {
    lw = lw_base
    lw[tolerant] = lw[tolerant] + ifelse(hot[i], effect, -effect)
    u = lw[alloc[i, ]] + gumbel(3)
    o = order(u, decreasing = TRUE)
    c(LETTERS[o[1]], LETTERS[o[3]])
  }, character(2)))
  colnames(bw) = c("overall_pos", "overall_neg")

  trial = data.frame(id = seq_len(n), alloc, bw, planting = planting,
                     elevation = elevation, stringsAsFactors = FALSE)

  list(trial = trial,
       temp = temp,
       rain = rain,
       truth = data.frame(flowering = flowering, heat = heat, hot = hot),
       tolerant = tolerant)

}
