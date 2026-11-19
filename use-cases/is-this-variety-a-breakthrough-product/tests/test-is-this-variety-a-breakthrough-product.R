# Tests for the use case "Is this variety a breakthrough product?"
# Not yet run in R. The same cases pass in script/other-language/run_checks.py

library("testthat")

test_that("the gain is recovered in a large network without interaction", {
  d = simulate_breakthrough(farms_per_trial = 2000, sd_gxt = 0, seed = 1)
  est = pool_trials(suppressMessages(trial_contrasts(d, "Check")))
  truth = attr(d, "truth")
  err = exp(est$log_gain) - truth$gain_ratio[match(est$genotype, truth$genotype)]
  expect_lt(max(abs(err)), 0.03)
})

test_that("the 95% interval covers the simulated gain in most networks", {
  cover = vapply(1:40, function(s) {
    d = simulate_breakthrough(seed = s)
    p = pool_trials(suppressMessages(trial_contrasts(d, "Check")))
    lg = log(attr(d, "truth")$gain_ratio[match(p$genotype, attr(d, "truth")$genotype)])
    mean(p$log_gain - 1.96 * p$se <= lg & lg <= p$log_gain + 1.96 * p$se)
  }, numeric(1))
  expect_gte(mean(cover), 0.85)
})

test_that("verdicts follow the simulated gains", {
  tab = breakthrough_table(simulate_breakthrough(), "Check", nboot = 50)
  v = stats::setNames(tab$verdict, tab$genotype)
  expect_equal(unname(v["Variety 01"]), "supported")
  expect_true(all(v[c("Variety 05", "Variety 06", "Variety 07")] == "not supported"))
  expect_false(v["Variety 02"] == "supported")
})

test_that("reliability behaves as in Eskridge and Mumm", {
  expect_equal(reliability_normal(log(1.25), 0.5, 0.25), 0.5)
  # a less stable variety with the same mean gain is less reliable
  expect_lt(reliability_normal(log(1.4), 0.8), reliability_normal(log(1.4), 0.4))
  d = simulate_breakthrough()
  tab = breakthrough_table(d, "Check", nboot = 50)
  expect_true(all(tab$rel_threshold < tab$rel))
  truth = attr(d, "truth")
  err = tab$rel_threshold - truth$rel_threshold[match(tab$genotype, truth$genotype)]
  expect_lt(max(abs(err)), 0.08)
})

test_that("the direct win share agrees with the simulated reliability", {
  d = simulate_breakthrough(farms_per_trial = 1500, p_fail = 0)
  dr = direct_reliability(d, "Check")
  truth = attr(d, "truth")
  expect_lt(abs(dr$direct_rel[dr$genotype == "Variety 01"] -
                  truth$rel[truth$genotype == "Variety 01"]), 0.05)
})

test_that("a trial without the check is skipped and named", {
  d = simulate_breakthrough(check_in_all = FALSE)
  expect_message(trial_contrasts(d, "Check"), "check not grown")
  con = suppressMessages(trial_contrasts(d, "Check"))
  expect_equal(attr(con, "skipped")$trial, "T1")
  expect_false("T1" %in% con$trial)
})

test_that("zero yields are counted, not silently dropped", {
  d = simulate_breakthrough()
  tab = breakthrough_table(d, "Check", nboot = 20)
  expect_equal(attr(tab, "plots_zero"), sum(d$yield == 0))
  expect_gt(attr(tab, "plots_zero"), 0)
  expect_true(all(tab$share_failed >= 0 & tab$share_failed <= 0.15))
})

test_that("too few plots gives no verdict", {
  d = simulate_breakthrough(n_trials = 1, farms_per_trial = 30)
  tab = breakthrough_table(d, "Check", nboot = 20)
  expect_true(any(tab$n_plots < 30))
  expect_true(all(tab$verdict[tab$n_plots < 30] == "insufficient data"))
})

test_that("the choice of check changes the gain", {
  d = simulate_breakthrough()
  a = breakthrough_table(d, "Check", nboot = 20)
  b = breakthrough_table(d, "Variety 07", nboot = 20)
  expect_gt(b$gain_ratio[b$genotype == "Variety 01"],
            a$gain_ratio[a$genotype == "Variety 01"])
})

test_that("the preferred variety is ranked above the check more often than not", {
  pr = preference_reliability(simulate_breakthrough(), "Check")
  expect_gt(pr$p_preferred[pr$genotype == "Variety 01"], 0.6)
})
