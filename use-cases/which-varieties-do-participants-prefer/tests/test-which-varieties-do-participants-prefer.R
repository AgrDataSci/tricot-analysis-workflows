# Tests for the use case "Which varieties do participants prefer?"

library("testthat")

items = c("item_A", "item_B", "item_C")
input = c("overall_pos", "overall_neg")

dat = simulate_tricot()
rk = build_rankings(dat, items, input)
mod = PlackettLuce::PlackettLuce(rk$rankings)
wt = worth_table(mod, ref = "Check")

test_that("estimated log-worth recovers the simulated truth", {
  truth = attr(dat, "truth")
  est = stats::setNames(wt$log_worth, wt$item)[names(truth)]
  expect_gt(stats::cor(est, truth), 0.9)
  expect_equal(unname(est["Check"]), 0)
})

test_that("the true best variety beats the check and leads the top probability", {
  truth = attr(dat, "truth")
  best = names(which.max(truth))
  expect_gt(wt$p_beat_check[wt$item == best], 0.5)
  tp = top_probability(rk$rankings, k = 3, nboot = 50)
  expect_equal(tp$item[which.max(tp$p_top_k)], best)
  expect_true(all(tp$p_first >= 0 & tp$p_first <= 1))
  expect_equal(sum(tp$p_first), 1)
})

test_that("intervals cover the truth for most varieties", {
  truth = attr(dat, "truth")
  w = wt[wt$item != "Check", ]
  covered = truth[w$item] >= w$lower & truth[w$item] <= w$upper
  expect_gte(mean(covered), 0.7)
})

test_that("model adequacy rejects equal preference", {
  ma = model_adequacy(mod)
  expect_gt(ma$mcfadden_r2, 0)
  expect_lt(ma$p_value, 0.001)
  expect_equal(ma$df, 9)
})

test_that("disconnected rankings are flagged", {
  d = simulate_tricot(n = 200)
  it = unique(unlist(d[items]))
  set.seed(1)
  half = seq_len(nrow(d)) <= nrow(d) / 2
  d[half, items] = t(replicate(sum(half), sample(it[1:5], 3)))
  d[!half, items] = t(replicate(sum(!half), sample(it[6:10], 3)))
  R = build_rankings(d, items, input)$rankings
  expect_warning(ok <- check_connectivity(R), "disconnected")
  expect_false(ok)
})

test_that("invalid best/worst entries are dropped and counted", {
  d = simulate_tricot(n = 100)
  d$overall_neg[1:10] = d$overall_pos[1:10]
  d$overall_pos[11:15] = NA
  expect_warning(x <- build_rankings(d, items, input))
  expect_equal(x$n_valid, 85)
  expect_equal(nrow(x$rankings), 85)
})
