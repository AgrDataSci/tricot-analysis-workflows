# Tests for the use case "Do on-farm and on-station results agree?"

library("testthat")

rank_farm = function(farm) {
  gosset::rank_tricot(farm, c("item_A", "item_B", "item_C"),
                      c("overall_pos", "overall_neg"), validate.rankings = TRUE)
}

test_that("station heritability is close to the simulated value", {
  # average over several simulated trials to avoid judging one draw
  est = vapply(1:5, function(s) {
    station_heritability(simulate_trials(seed = s, n_genotypes = 40)$station)$H2
  }, numeric(1))
  truth = simulate_trials(n_genotypes = 40)$truth$H2
  expect_lt(abs(mean(est) - truth), 0.1)
})

test_that("BLUPs track the simulated genetic values", {
  sim = simulate_trials()
  st = station_heritability(sim$station)
  g = sim$truth$g[st$blup$genotype]
  expect_gt(stats::cor(st$blup$blup, g), 0.85)
})

test_that("agreement is detected when preference follows genetic value", {
  sim = simulate_trials(b = 0.8)
  ag = agreement(station_heritability(sim$station)$blup,
                 on_farm_worth(rank_farm(sim$farm)), nboot = 300)
  expect_gt(ag$summary$lower, 0)
})

test_that("no agreement is claimed when preference is unrelated", {
  sim = simulate_trials(b = 0)
  ag = agreement(station_heritability(sim$station)$blup,
                 on_farm_worth(rank_farm(sim$farm)), nboot = 300)
  expect_lt(ag$summary$lower, 0)
})

test_that("on-farm reliability is high with many participants and lower with few", {
  sim = simulate_trials()
  R = rank_farm(sim$farm)
  many = on_farm_reliability(R, nsplit = 20)$reliability
  few = on_farm_reliability(R[1:80, ], nsplit = 20)$reliability
  expect_gt(many, 0.8)
  expect_lt(few, many)
})

test_that("mismatched genotype names are reported", {
  sim = simulate_trials()
  st = station_heritability(sim$station)
  wf = on_farm_worth(rank_farm(sim$farm))
  wf$genotype[1] = "G01 (check)"
  expect_warning(agreement(st$blup, wf, nboot = 50), "harmonized")
})
