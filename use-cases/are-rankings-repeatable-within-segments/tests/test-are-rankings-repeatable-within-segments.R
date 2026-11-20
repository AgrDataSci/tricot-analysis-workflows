# Tests for the use case "Are rankings repeatable within segments?"
# Not yet run in R. The same cases pass in script/other-language/run_checks.py

library("testthat")

dat = simulate_segments()
ord = as_ordered(dat)
k = length(attr(ord, "levels"))

test_that("the model recovers the simulated worth", {
  big = simulate_segments(n = 20000, seed = 3)
  ob = as_ordered(big)
  truth = attr(big, "worth")
  for (s in c("market", "home")) {
    e = pl_estimates(ob[big$segment == s, , drop = FALSE], k)$est
    expect_lt(max(abs(e - (truth[, s] - mean(truth[, s])))), 0.1)
  }
})

test_that("inconsistent and missing answers are dropped and counted", {
  bad = dat
  bad$worst[1] = bad$best[1]
  bad$best[2] = NA
  ob = as_ordered(bad)
  expect_equal(attr(ob, "dropped"), 2)
  expect_equal(nrow(ob), nrow(dat) - 2)
})

test_that("repeatability rises with the number of participants", {
  curve = repeatability_curve(ord, sizes = c(50, 100, 200, 400, 800), B = 100)
  expect_true(all(diff(curve$repeatability) > 0))
})

test_that("repeatability is the share of real variance", {
  expect_equal(as.numeric(repeatability(rep(0, 8), rep(0.05, 8))), 0)
  expect_gt(repeatability(seq(-2, 2, length.out = 8), rep(0.001, 8)), 0.99)
  # the Cullis form has a ceiling near 1 - 1/k with few items
  expect_lt(abs(repeatability(seq(-2, 2, length.out = 8), rep(0.001, 8), cullis = TRUE) - 0.875), 0.01)
  e = pl_estimates(ord, k)
  expect_lt(repeatability(e$est, e$var * 10), repeatability(e$est, e$var))
})

test_that("true segments pass the fair test and a noise label does not", {
  st = segment_test(ord, dat$segment, B = 300)
  sn = segment_test(ord, dat$noise, B = 300)
  expect_lt(st$p_deviance, 0.01)
  expect_lt(st$p_repeatability, 0.01)
  expect_gt(sn$p_deviance, 0.05)
  expect_gt(sn$p_repeatability, 0.05)
  # random segments pay the cost of a smaller sample
  expect_lt(st$repeatability_random, st$repeatability_pooled)
})

test_that("segments with identical preferences are not more repeatable", {
  same = simulate_segments(contrast = 0)
  ss = segment_test(as_ordered(same), same$segment, B = 300)
  expect_gt(ss$p_repeatability, 0.05)
})

test_that("at the same n true segments beat the pooled data", {
  eq = equal_n(ord, dat$segment, m = 200, B = 150)
  pooled = eq$repeatability[eq$group == "pooled"]
  expect_true(all(eq$repeatability[eq$group != "pooled"] > pooled))
  expect_true(all(eq$split_half_r[eq$group != "pooled"] > eq$split_half_r[eq$group == "pooled"]))
  # single groups move by chance, so a noise label is judged on its mean
  en = equal_n(ord, dat$noise, m = 200, B = 150)
  expect_lt(abs(mean(en$repeatability[en$group != "pooled"]) - pooled), 0.1)
})

test_that("true segments predict held-out rankings better", {
  expect_gt(cv_gain(ord, dat$segment)$gain_per_ranking, 0.02)
  expect_lt(cv_gain(ord, dat$noise)$gain_per_ranking, 0.005)
  expect_equal(cv_gain(ord, dat$segment, blocks = seq_len(nrow(ord)) %% 6)$folds, "blocks")
})

test_that("single answers are harder to predict than item means are to repeat", {
  fi = individual_fit(ord, dat$segment)
  expect_lt(fi$pseudo_r2_pooled, 0.2)
  expect_gt(group_repeatability(ord, k), 0.2)
  expect_gt(fi$pseudo_r2_segments, fi$pseudo_r2_pooled + 0.02)
})

test_that("reliability against a reference changes sides between segments", {
  br = benchmark_reliability(ord, "C4", dat$segment, B = 100)
  expect_gt(br$reliability[br$group == "market" & br$item == "C1"], 0.6)
  expect_lt(br$reliability[br$group == "home" & br$item == "C1"], 0.5)
  expect_true(all(br$lower <= br$reliability & br$reliability <= br$upper))
})

test_that("segments built from covariates recover the true segments and pass the fair test", {
  set.seed(5)
  market = dat$segment == "market"
  cov = data.frame(sold = pmin(pmax(ifelse(market, rnorm(nrow(dat), 70, 10), rnorm(nrow(dat), 15, 10)), 0), 100),
                   land = ifelse(market, rlnorm(nrow(dat), 1.2, 0.4), rlnorm(nrow(dat), 0.2, 0.4)))
  seg = build_segments(cov, 2, log = "land")
  expect_gt(adjusted_rand(seg$segment, dat$segment), 0.8)
  expect_gt(seg$stability, 0.9)
  expect_equal(adjusted_rand(seg$segment, seg$segment), 1)
  expect_lt(abs(adjusted_rand(seg$segment, dat$noise)), 0.05)
  expect_lt(segment_test(ord, seg$segment, B = 200)$p_repeatability, 0.01)
  noise_cov = as.data.frame(matrix(rnorm(nrow(dat) * 3), ncol = 3))
  expect_lt(build_segments(noise_cov, 3)$stability, 0.7)
})

test_that("climate indices separate wet and dry points and envirotypes recover the zones", {
  pts = sprintf("P%02d", 0:39)
  zone = rep(c("wet", "dry"), each = 20)
  cl = simulate_climate(pts, zone)
  ind = climate_indices(cl$rain, cl$temp)
  z = zone[match(ind$id, pts)]
  expect_gt(min(ind$rain_total[z == "wet"]), max(ind$rain_total[z == "dry"]))
  expect_lt(max(ind$tmax_mean[z == "wet"]), min(ind$tmax_mean[z == "dry"]))
  expect_lt(mean(ind$max_dry_spell[z == "wet"]), mean(ind$max_dry_spell[z == "dry"]))
  expect_true(all(ind$years_rain == 5) && all(ind$years_temp == 5))
  env = envirotypes(ind, 2)
  expect_equal(adjusted_rand(env$envirotype[pts], zone), 1)
  expect_gt(env$stability, 0.9)
  # an incomplete year is left out
  short = cl$rain[format(cl$rain$date, "%Y") < "2011" | as.integer(format(cl$rain$date, "%m")) <= 6, ]
  expect_true(all(climate_indices(short)$years_rain == 4))
  # an index that does not vary is dropped, correlated indices are reported
  flat = ind
  flat$hot_days = 100
  expect_false("hot_days" %in% screen_indices(flat)$keep)
  expect_gt(nrow(screen_indices(ind)$pairs), 0)
})

test_that("envirotypes pass the fair test only when preference follows climate", {
  set.seed(7)
  pts = sprintf("P%02d", 0:39)
  zone = rep(c("wet", "dry"), each = 20)
  cl = simulate_climate(pts, zone)
  env = envirotypes(climate_indices(cl$rain, cl$temp), 2)$envirotype
  tied = simulate_segments(seed = 11)
  ot = as_ordered(tied)
  tied_pt = ifelse(tied$segment == "market", sample(pts[zone == "wet"], nrow(tied), TRUE),
                   sample(pts[zone == "dry"], nrow(tied), TRUE))
  expect_lt(segment_test(ot, env[tied_pt], B = 200)$p_repeatability, 0.01)
  free_pt = sample(pts, nrow(dat), TRUE)
  expect_gt(segment_test(ord, env[free_pt], B = 200)$p_repeatability, 0.05)
})
