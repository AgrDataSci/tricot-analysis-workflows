# Tests for the use case "Which traits drive overall preference?"
# Run from the use-case folder with  Rscript tests/run-tests.R

library("testthat")

items = c("item_A", "item_B", "item_C")

traits = list(overall = c("overall_pos", "overall_neg"),
              yield = c("yield_pos", "yield_neg"),
              taste = c("taste_pos", "taste_neg"),
              colour = c("colour_pos", "colour_neg"),
              drought = c("drought_pos", "drought_neg"))

dat = simulate_tricot()
tr = build_trait_rankings(dat, items, traits)
rk = row_kendall_all(tr$rankings)

test_that("row-level tau averages to the gosset estimate", {
  for (t in c("yield", "colour")) {
    ours = average_tau(rk)[t, "overall"]
    ref = gosset::kendallTau(tr$rankings[[t]], tr$rankings[["overall"]])
    expect_equal(unname(ours), unname(ref[1, "kendallTau"]), tolerance = 1e-8)
  }
})

test_that("partial correlations match the closed form for three variables", {
  r = matrix(c(1, 0.5, 0.4,
               0.5, 1, 0.3,
               0.4, 0.3, 1), 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  tau = 2 * asin(r) / pi
  P = tau_to_partial(tau)
  expected = (0.5 - 0.4 * 0.3) / sqrt((1 - 0.4^2) * (1 - 0.3^2))
  expect_equal(P["a", "b"], expected, tolerance = 1e-8)
})

test_that("the known drivers are recovered and the confounded trait is dropped", {
  assoc = trait_association(rk, "overall", nboot = 200)
  colour = assoc[assoc$trait == "colour", ]
  # colour looks associated on its own
  expect_gt(colour$tau_lower, 0)
  # but not after accounting for yield and taste
  expect_lt(colour$partial_lower, 0)
  expect_gt(colour$partial_upper, 0)

  sel = backward_selection(rk, "overall", nboot = 200)
  expect_setequal(sel$selected$trait, c("yield", "taste"))
})

test_that("disconnected rankings are flagged", {
  d = simulate_tricot(n = 200, n_items = 12)
  groupA = paste0("Variety ", sprintf("%02d", 1:6))
  groupB = paste0("Variety ", sprintf("%02d", 7:12))
  set.seed(1)
  half = seq_len(nrow(d)) <= nrow(d) / 2
  d[half, items] = t(replicate(sum(half), sample(groupA, 3)))
  d[!half, items] = t(replicate(sum(!half), sample(groupB, 3)))
  R = build_trait_rankings(d, items, traits["overall"])$rankings$overall
  expect_warning(ok <- check_connectivity(R), "disconnected")
  expect_false(ok)
})

test_that("invalid best/worst entries are dropped and counted", {
  d = simulate_tricot(n = 100)
  d$yield_neg[1:10] = d$yield_pos[1:10]   # best equal to worst
  d$taste_pos[11:15] = "D"                 # letter outside A, B, C
  d$colour_neg[16:20] = NA                 # missing
  expect_warning(x <- build_trait_rankings(d, items, traits))
  expect_equal(unname(x$n_valid["yield"]), 90)
  expect_equal(unname(x$n_valid["taste"]), 95)
  expect_equal(unname(x$n_valid["colour"]), 95)
  expect_equal(unname(x$n_valid["overall"]), 100)
})

test_that("traits never ranked by the same participants stop the analysis", {
  d = simulate_tricot(n = 100)
  d$yield_pos[1:50] = NA
  d$yield_neg[1:50] = NA
  d$taste_pos[51:100] = NA
  d$taste_neg[51:100] = NA
  x = suppressWarnings(build_trait_rankings(d, items, traits[1:3]))
  rk_x = row_kendall_all(x$rankings)
  expect_error(tau_to_partial(average_tau(rk_x)), "never ranked")
})
