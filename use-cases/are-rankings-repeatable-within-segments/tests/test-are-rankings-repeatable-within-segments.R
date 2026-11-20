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
