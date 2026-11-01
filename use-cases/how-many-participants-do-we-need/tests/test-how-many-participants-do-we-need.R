# Tests for the use case "How many participants do we need?"

library("testthat")

test_that("simulated rankings give each participant three ranked items", {
  set.seed(1)
  R = simulate_tricot_rankings(c(a = 0, b = 1, c = -1, d = 0.5, e = 0.2), n = 40)
  m = unclass(R)
  expect_equal(nrow(m), 40)
  expect_true(all(rowSums(m > 0) == 3))
  expect_true(all(apply(m, 1, function(r) all(sort(r[r > 0]) == 1:3))))
})

test_that("simulated rankings follow the log-worths", {
  set.seed(2)
  lw = c(a = 2, b = 0.5, c = 0, d = -0.5, e = -2)
  est = stats::coef(PlackettLuce::PlackettLuce(simulate_tricot_rankings(lw, n = 500)), log = TRUE)
  expect_equal(names(est)[which.max(est)], "a")
  expect_equal(names(est)[which.min(est)], "e")
  expect_gt(stats::cor(est[names(lw)], lw), 0.95)
})

lw = stats::setNames(seq(1, -1, length.out = 8), paste0("i", 1:8))
curve = design_curve(lw, sizes = c(15, 300), nsim = 30, top = 3, seed = 3)

test_that("more participants give a more accurate ranking", {
  expect_equal(curve$n, c(15, 300))
  expect_gt(curve$tau[2], curve$tau[1])
  expect_gt(curve$p_winner[2], curve$p_winner[1])
  expect_gt(curve$tau[2], 0.6)
  expect_true(all(curve$tau_lower <= curve$tau_upper))
  expect_true(all(curve$top_found >= 0 & curve$top_found <= 1))
})

test_that("the same seed gives the same curve", {
  again = design_curve(lw, sizes = c(15, 300), nsim = 30, top = 3, seed = 3)
  expect_equal(curve, again)
})

test_that("the target is read from the curve, NA when out of reach", {
  expect_equal(n_for_target(curve, 0, "p_winner"), 15L)
  expect_true(is.na(n_for_target(curve, 1.1, "p_winner")))
})

test_that("subsampling the real answers overstates accuracy", {
  dat = simulate_tricot()
  ok = !is.na(dat$overall_pos)
  R = build_rankings(dat[ok, ])
  truth = stats::coef(PlackettLuce::PlackettLuce(R), log = TRUE)
  n = sum(ok)
  naive = naive_subsample_agreement(R, n - 2, nsim = 20)
  simulated = design_curve(truth, sizes = n - 2, nsim = 30)$tau
  # nearly all participants are shared, so agreement is close to 1
  expect_gt(naive, 0.75)
  expect_gt(naive, simulated)
})
