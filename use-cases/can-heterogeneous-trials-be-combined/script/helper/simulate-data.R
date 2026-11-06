# Simulate heterogeneous trials of one trait measured in different ways
#
# The design mirrors the Musa data in Brown et al. (2023). Trials differ in
# the metric used to score the trait, in whether block data or only trial
# means are reported, in the genotypes tested, in the number of crop cycles,
# and in the climate of the evaluation period. A few trials fail the inclusion
# criteria on purpose.
#
# The truth built into the simulation
#   - 30 genotypes. "Reference" has the highest resistance, like Calcutta 4
#     in the paper. Seven genotypes are dry-sensitive. Their resistance drops
#     by `effect` when the longest dry spell in the evaluation period (MLDS)
#     exceeds `threshold` days. Nothing else depends on climate.
#   - Three metrics score the same latent resistance. "dii" is a disease
#     index where lower is better. "ddt" is days to symptoms and "yls" is the
#     youngest leaf with symptoms, where higher is better. "yls" is a count on
#     a short scale, so it produces ties.
#   - Block-level trials give one ranking per block and cycle. Trials with
#     means only give one ranking per cycle.
#   - A second cycle starts when the first ends, and experiences its own
#     climate.
#   - Temperature is generated independently of rainfall and has no effect.

simulate_heterogeneous_trials = function(n_trials = 26,
                                         n_items = 30,
                                         threshold = 13,
                                         effect = 2.5,
                                         seed = 2026) {

  set.seed(seed)

  dates = seq(as.Date("2016-01-01"), as.Date("2019-12-31"), by = "day")
  nd = length(dates)

  # genotypes and their latent resistance
  items = c("Reference",
            paste0("Hybrid ", sprintf("%02d", 1:12)),
            paste0("Landrace ", sprintf("%02d", 1:17)))
  items = items[seq_len(n_items)]
  resistance = stats::setNames(c(2, stats::rnorm(n_items - 1, 0, 0.8)), items)
  sensitive = sample(items[-1], 7)

  # one site per trial, with its own tendency to long dry spells
  dry_persistence = stats::runif(n_trials, 0.50, 0.95)
  lon = stats::runif(n_trials, -90, 130)
  lat = stats::runif(n_trials, -25, 25)

  # daily rainfall from a two-state Markov chain, so the maximum dry spell
  # varies between sites. Rain on wet days is scaled so that the expected
  # total is the same at every site, as in the paper, where dry spell length
  # separated the environments while rainfall totals did not. Daily
  # temperature comes from a site mean plus noise
  rain = t(vapply(seq_len(n_trials), function(i) {
    wet = logical(nd)
    wet[1] = TRUE
    for (d in 2:nd) {
      p = if (wet[d - 1]) 0.6 else 1 - dry_persistence[i]
      wet[d] = stats::runif(1) < p
    }
    wet_fraction = (1 - dry_persistence[i]) / (1 - dry_persistence[i] + 0.4)
    wet * stats::rgamma(nd, shape = 0.9, scale = 4 / wet_fraction)
  }, numeric(nd)))
  dimnames(rain) = list(NULL, as.character(dates))

  site_tmax = stats::runif(n_trials, 27, 34)
  tmax = t(vapply(seq_len(n_trials), function(i) {
    site_tmax[i] + 1.5 * sin(2 * pi * seq_len(nd) / 365) + stats::rnorm(nd, sd = 1.2)
  }, numeric(nd)))
  tmin = tmax - 8 - matrix(stats::runif(n_trials * nd, 0, 3), n_trials, nd)
  temp = array(c(tmax, tmin), dim = c(n_trials, nd, 2),
               dimnames = list(NULL, as.character(dates), c("tmax", "tmin")))

  # trial design
  trial = sprintf("trial_%02d", seq_len(n_trials))
  planting = as.Date("2016-02-01") + sample(0:850, n_trials, replace = TRUE)
  n_cycles = ifelse(stats::runif(n_trials) < 0.3, 2L, 1L)
  level = ifelse(stats::runif(n_trials) < 0.6, "block", "mean")
  n_blocks = ifelse(level == "block", sample(3:5, n_trials, replace = TRUE), 1L)
  metric = sample(c("dii", "ddt", "yls"), n_trials, replace = TRUE,
                  prob = c(0.45, 0.25, 0.30))
  # every third trial also reports a second metric, with gaps
  second_metric = ifelse(seq_len(n_trials) %% 3 == 0,
                         c("ddt", "yls", "dii")[match(metric, c("dii", "ddt", "yls"))],
                         NA_character_)

  # the genotypes of a trial are the same in all its cycles. The reference is
  # in most trials, as a resistant check usually is
  trial_genotypes = lapply(seq_len(n_trials), function(i) {
    unique(c(if (stats::runif(1) < 0.9) "Reference",
             sample(items[-1], sample(6:14, 1))))
  })

  meta = data.frame(trial = trial, lon = lon, lat = lat,
                    start = planting, stringsAsFactors = FALSE)

  cycles = do.call("rbind", lapply(seq_len(n_trials), function(i) {
    span = sample(180:330, n_cycles[i], replace = TRUE)
    end = planting[i] + cumsum(span)
    data.frame(trial = trial[i], trial_index = i, cycle = seq_len(n_cycles[i]),
               start = planting[i], end = end, stringsAsFactors = FALSE)
  }))
  meta$end = as.Date(vapply(trial, function(t) {
    as.numeric(max(cycles$end[cycles$trial == t]))
  }, numeric(1)), origin = "1970-01-01")

  # true MLDS of each cycle, computed directly from the simulated rain
  mlds = function(x) {
    r = rle(x < 1)
    if (any(r$values)) max(r$lengths[r$values]) else 0L
  }
  cycles$true_mlds = vapply(seq_len(nrow(cycles)), function(k) {
    i = cycles$trial_index[k]
    s = if (cycles$cycle[k] == 1) cycles$start[k] else cycles$end[k - 1]
    # the window has as many days as the span, starting on its first day,
    # which is how climatrends reads day.one and span
    d = which(dates == s) + seq_len(as.integer(cycles$end[k] - s)) - 1
    mlds(rain[i, d])
  }, numeric(1))
  cycles$dry = cycles$true_mlds > threshold

  # observations
  obs = do.call("rbind", lapply(seq_len(nrow(cycles)), function(k) {
    i = cycles$trial_index[k]
    g = trial_genotypes[[i]]
    do.call("rbind", lapply(seq_len(n_blocks[i]), function(b) {
      latent = resistance[g] -
        effect * cycles$dry[k] * (g %in% sensitive) +
        stats::rnorm(length(g), sd = 0.6)
      data.frame(trial = trial[i], cycle = cycles$cycle[k],
                 block = if (level[i] == "block") b else NA_integer_,
                 genotype = g,
                 dii = pmin(pmax(round(50 - 12 * latent), 0), 100),
                 ddt = round(40 + 8 * latent),
                 yls = pmin(pmax(round(8 + 1.5 * latent), 1), 15),
                 stringsAsFactors = FALSE)
    }))
  }))

  # each trial reports its own metric, and some a second one with gaps
  main = metric[match(obs$trial, trial)]
  second = second_metric[match(obs$trial, trial)]
  for (m in c("dii", "ddt", "yls")) {
    is_main = main == m
    is_second = !is.na(second) & second == m
    obs[[m]][!is_main & !is_second] = NA
    obs[[m]][is_second & stats::runif(nrow(obs)) < 0.4] = NA
  }

  # trials that fail the inclusion criteria
  meta$lon[2] = NA
  meta$end[3] = NA
  obs$genotype[obs$trial == trial[4]] = paste0(obs$genotype[obs$trial == trial[4]],
                                               " (local name)")

  list(obs = obs,
       meta = meta,
       cycles = cycles[, c("trial", "trial_index", "cycle", "start", "end",
                           "true_mlds", "dry")],
       rain = rain,
       temp = temp,
       metrics = data.frame(metric = c("dii", "ddt", "yls"),
                            description = c("disease infection index",
                                            "disease development time, days",
                                            "youngest leaf spotted"),
                            higher_is_better = c(FALSE, TRUE, TRUE)),
       truth = list(resistance = resistance, sensitive = sensitive,
                    threshold = threshold, effect = effect))

}
