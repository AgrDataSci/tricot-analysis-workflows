# Tests for the use case "Can trials with different measurements and designs
# be combined into one analysis?"

library("testthat")

sim = simulate_heterogeneous_trials()
obs = sim$obs
meta = sim$meta
cycles = sim$cycles
metrics = sim$metrics

test_that("trials without coordinates, dates or overlapping genotypes are excluded", {
  s = screen_trials(meta, obs, metrics = metrics$metric, min_overlap = 2)
  expect_false(s$include[s$trial == "trial_02"])
  expect_false(s$has_coords[s$trial == "trial_02"])
  expect_false(s$include[s$trial == "trial_03"])
  expect_false(s$has_dates[s$trial == "trial_03"])
  expect_false(s$include[s$trial == "trial_04"])
  expect_equal(s$overlap[s$trial == "trial_04"], 0L)
  expect_gt(sum(s$include), nrow(s) - 4)
})

test_that("the metric with the most data is chosen, with the priority as tie-breaker", {
  x = data.frame(trial = "t", genotype = letters[1:4],
                 a = c(1, 2, NA, 4), b = c(1, 2, 3, 4), c = c(1, 2, 3, 4))
  expect_equal(choose_metric(x, priority = c("a", "c", "b"))$metric, "c")
  expect_equal(choose_metric(x, priority = c("b", "c"))$metric, "b")
})

test_that("ranking direction follows the metric and ties share a rank", {
  x = data.frame(unit = "u", genotype = c("a", "b", "c", "d"),
                 value = c(10, 30, 30, 20))
  r_low = rank_within_units(x, higher_is_better = FALSE)$u
  r_high = rank_within_units(x, higher_is_better = TRUE)$u
  expect_equal(unname(r_low), c(1L, 3L, 3L, 2L))
  expect_equal(unname(r_high), c(4L, 1L, 1L, 3L))
  expect_error(rank_within_units(x, c(TRUE, TRUE, FALSE, FALSE)), "direction")
})

test_that("absent genotypes are zero in the rank matrix", {
  r = list(u1 = c(a = 1L, b = 2L), u2 = c(b = 1L, c = 2L))
  M = rank_matrix(r)
  expect_equal(dim(M), c(2L, 3L))
  expect_equal(M["u1", "c"], 0L)
  expect_equal(M["u2", "b"], 1L)
})

test_that("ties are resolved by keeping one genotype and ranks stay consecutive", {
  M = rank_matrix(list(u = c(a = 1L, b = 2L, c = 2L, d = 2L, e = 3L)))
  one = resolve_ties(M, "keep_one")
  expect_equal(one$dropped, 2L)
  kept = one$M["u", ]
  expect_equal(sort(unname(kept[kept > 0])), 1:3)
  all = resolve_ties(M, "drop_all")
  expect_equal(all$dropped, 3L)
  expect_equal(sort(unname(all$M["u", all$M["u", ] > 0])), 1:2)
  expect_equal(resolve_ties(M, "keep")$M, M)
})

test_that("disconnected and rare genotypes are removed", {
  # two groups never compared, one rare genotype
  r = list(u1 = c(a = 1L, b = 2L), u2 = c(b = 1L, a = 2L),
           u3 = c(c = 1L, d = 2L), u4 = c(d = 1L, c = 2L),
           u5 = c(a = 1L, b = 2L, e = 3L))
  M = rank_matrix(r)
  f = filter_network(M, min_presence = 0.3)
  expect_true("e" %in% f$removed$genotype[f$removed$reason == "rare"])
  expect_equal(PlackettLuce::connectivity(
    PlackettLuce::adjacency(PlackettLuce::as.rankings(f$M)), verbose = FALSE)$no, 1L)
  expect_true(all(rowSums(f$M > 0) >= 2))
})

test_that("a later cycle starts where the previous one ended", {
  w = evaluation_window(cycles)
  two = w[w$trial %in% w$trial[w$cycle == 2], ]
  for (t in unique(two$trial)) {
    expect_equal(two$start[two$trial == t & two$cycle == 2],
                 two$end[two$trial == t & two$cycle == 1])
  }
  expect_true(all(w$span > 0))
})

test_that("the climate indices reproduce the simulated dry spell", {
  w = evaluation_window(cycles)
  ci = climate_indices(sim$rain[w$trial_index, , drop = FALSE],
                       sim$temp[w$trial_index, , , drop = FALSE],
                       w$start, w$span)
  expect_equal(ci$MLDS, w$true_mlds)
})

test_that("near-zero variance covariates are removed and correlations reported", {
  x = data.frame(a = c(rep(1, 29), 2), b = stats::rnorm(30))
  x$c = x$b * 2 + 1e-6
  s = screen_covariates(x)
  expect_equal(s$removed, "a")
  expect_true(nrow(s$correlated) >= 1)
  expect_setequal(c(s$correlated$a, s$correlated$b), c("b", "c"))
})

# the full pipeline on the simulated data
screen = screen_trials(meta, obs, metrics = metrics$metric, min_overlap = 2)
obs = obs[obs$trial %in% screen$trial[screen$include], ]
cycles = cycles[cycles$trial %in% screen$trial[screen$include], ]
mpt = choose_metric(obs, priority = c("dii", "yls", "ddt"))
obs$metric = mpt$metric[match(obs$trial, mpt$trial)]
obs$value = vapply(seq_len(nrow(obs)), function(i) obs[[obs$metric[i]]][i], numeric(1))
obs$hib = metrics$higher_is_better[match(obs$metric, metrics$metric)]
obs$unit = paste0(obs$trial, "_c", obs$cycle, "_b", ifelse(is.na(obs$block), "mean", obs$block))
M = rank_matrix(rank_within_units(obs[, c("unit", "genotype", "value")], obs$hib))
M = filter_network(resolve_ties(M)$M, min_presence = 0.05)$M
cycles = evaluation_window(cycles)
ci = climate_indices(sim$rain[cycles$trial_index, , drop = FALSE],
                     sim$temp[cycles$trial_index, , , drop = FALSE],
                     cycles$start, cycles$span)
key = paste0(cycles$trial, "_c", cycles$cycle)
units_key = sub("_b.*$", "", rownames(M))
covariates = screen_covariates(ci[match(units_key, key), ])$covariates
rownames(covariates) = NULL

test_that("block-level trials give one ranking per block and cycle", {
  blocks = obs[!is.na(obs$block), ]
  expect_equal(length(unique(blocks$unit)),
               nrow(unique(blocks[, c("trial", "cycle", "block")])))
})

test_that("the tree separates the dry evaluation periods and ignores temperature", {
  tree = fit_synthesis_tree(M, covariates)
  sv = split_variables(tree)
  expect_true(length(sv) >= 1)
  expect_true(sv[1] %in% c("MLDS", "MLWS", "R10mm", "R20mm", "Rx1day", "Rx5day",
                           "R95p", "R99p", "Rtotal", "SDII"))
  expect_false(any(sv %in% c("maxDT", "minDT", "maxNT", "minNT", "DTR", "SU",
                             "TR", "CFD", "WSDI", "CSDI", "T10p", "T90p")))
  node = as.integer(predict(tree, type = "node"))
  dry = cycles$dry[match(units_key, key)]
  agreement = max(mean((node == min(node)) == dry), mean((node == min(node)) != dry))
  expect_gt(agreement, 0.8)
})

test_that("the dry-sensitive genotypes fall in the ranking under dry conditions", {
  tree = fit_synthesis_tree(M, covariates)
  w = worth_by_node(tree, ref = "Reference")
  node = as.integer(predict(tree, type = "node"))
  dry = cycles$dry[match(units_key, key)]
  dry_node = names(which.max(table(node[dry])))
  wet_node = names(which.max(table(node[!dry])))
  expect_false(dry_node == wet_node)
  sens = w$genotype %in% sim$truth$sensitive
  drop = w[[paste0("node_", wet_node)]] - w[[paste0("node_", dry_node)]]
  expect_gt(mean(drop[sens]), mean(drop[!sens]))
})

test_that("split stability returns frequencies that sum to at most B", {
  st = split_stability(M, covariates, B = 10, v = 0.8)
  expect_true(all(st$frequency$frequency <= 1))
  expect_true(all(st$frequency$variable %in% names(covariates)))
  expect_true(st$no_split + nrow(st$cutpoints) == 10)
})

test_that("reliability, coverage and fit are reported per node", {
  tree = fit_synthesis_tree(M, covariates)
  rel = suppressWarnings(node_reliability(tree, ref = "Reference"))
  expect_true(all(rel$reliability[rel$item == "Reference"] == 0.5))
  ev = evaluations_by_node(tree, M)
  expect_equal(sum(ev[, -1]), sum(M > 0))
  g = goodness_of_fit(tree, M)
  expect_true(g$kendall_tau > 0)
  expect_true(g$mcfadden > 0)
})
