# Tests for the use case "How do climate and environment shape variety performance?"

library("testthat")

sim = simulate_climate_trial()
dat = sim$trial
R = gosset::rank_tricot(dat, c("item_A", "item_B", "item_C"),
                        c("overall_pos", "overall_neg"), validate.rankings = TRUE)

window = phenology_window(sim$temp, dat$planting, tbase = 10, degree_days = 900)
ci = climate_indices(sim$temp, sim$rain, dat$planting, window)

test_that("the phenology window matches the simulated flowering date", {
  expect_lt(max(abs(window - sim$truth$flowering)), 2)
  # cooler plots at higher elevation take longer to reach flowering
  expect_gt(stats::cor(window, dat$elevation), 0)
})

test_that("phenology must be provided, not guessed", {
  expect_error(phenology_window(sim$temp, dat$planting), "phenology is required")
})

test_that("plots that do not reach the stage get no window", {
  expect_warning(w <- phenology_window(sim$temp[1:20, , ], dat$planting[1:20],
                                       tbase = 10, degree_days = 5000,
                                       max_days = 100),
                 "did not reach")
  expect_true(all(is.na(w)))
})

test_that("window indices reproduce the simulated heat", {
  expect_gt(stats::cor(ci$maxDT, sim$truth$heat), 0.99)
})

test_that("the tree splits on heat and not on rainfall", {
  tree = fit_climate_tree(R, ci)
  sv = split_variables(tree)
  expect_true(sv[1] %in% c("maxDT", "SU"))
  expect_false("Rtotal" %in% sv)
})

test_that("window sensitivity returns one row per window", {
  windows = list(gdd = window, fixed = rep(60, nrow(dat)))
  ws = window_sensitivity(R, windows, sim$temp, sim$rain, dat$planting)
  expect_equal(ws$window, c("gdd", "fixed"))
  expect_true(all(ws$plots == nrow(dat)))
})

test_that("unsupported spatial extrapolation is flagged", {
  target = data.frame(maxDT = c(stats::median(ci$maxDT), max(ci$maxDT) + 3),
                      DTR = stats::median(ci$DTR),
                      SU = stats::median(ci$SU),
                      Rtotal = stats::median(ci$Rtotal))
  ex = extrapolation_check(ci, target)
  expect_equal(ex$extrapolation, c(FALSE, TRUE))
})

test_that("a constant or redundant index does not break the extrapolation check", {
  train = ci
  train$DTR = 11
  train$maxDT2 = 2 * train$maxDT
  target = data.frame(maxDT = c(stats::median(ci$maxDT), max(ci$maxDT) + 3),
                      DTR = c(11, 11), SU = stats::median(ci$SU),
                      Rtotal = stats::median(ci$Rtotal))
  target$maxDT2 = 2 * target$maxDT
  expect_warning(ex <- extrapolation_check(train, target), "no variation")
  expect_equal(ex$extrapolation, c(FALSE, TRUE))
})
