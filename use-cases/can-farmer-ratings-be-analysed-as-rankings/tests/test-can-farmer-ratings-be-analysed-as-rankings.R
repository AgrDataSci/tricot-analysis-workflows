# Tests for the use case "Can farmer ratings be analysed as rankings?"

library("testthat")

toy = data.frame(rater = rep(c("r1", "r2", "r3"), each = 4),
                 item = rep(c("a", "b", "c", "d"), 3),
                 score = c(3, 3, 5, 1,
                           2, 4, 4, 4,
                           3, 3, 3, 3),
                 stringsAsFactors = FALSE)

test_that("tied pairs are counted before conversion", {
  info = rating_information(toy)
  expect_equal(info$pairs, c(6, 6, 6))
  expect_equal(info$tied_share, c(1 / 6, 3 / 6, 1))
  expect_equal(info$untied_pairs, c(5, 3, 0))
  expect_equal(info$distinct_scores, c(3L, 2L, 1L))
})

test_that("paired comparisons follow the score and ties are NA, in paircomp order", {
  M = paired_comparisons(toy)
  expect_equal(colnames(M), c("a vs b", "a vs c", "b vs c", "a vs d", "b vs d", "c vs d"))
  expect_equal(unname(M["r1", ]), c(NA, -1L, -1L, 1L, 1L, 1L))
  expect_true(all(is.na(M["r3", ])))
  expect_equal(unname(M["r2", "a vs b"]), -1L)
})

test_that("a rater with only ties is dropped and the rest become grouped rankings", {
  gp = grouped_from_pairs(paired_comparisons(toy))
  expect_equal(gp$dropped, "r3")
  expect_equal(gp$raters, c("r1", "r2"))
  expect_equal(length(gp$G), 2L)
  # r1 has five comparisons and r2 three
  expect_equal(unname(table(attr(gp$G, "index"))), c(5L, 3L))
})

test_that("rankings with ties report the largest tie set", {
  rt = rankings_with_ties(toy)
  expect_equal(unname(rt$max_tie_order), c(2, 3, 4))
  expect_equal(unname(rt$R["r1", ]), c(2L, 2L, 1L, 4L))
})

test_that("items aggregate to groups by the mean score per rater", {
  groups = c(a = "g1", b = "g1", c = "g2", d = "g2")
  agg = aggregate_items(toy, groups)
  r1 = agg[agg$rater == "r1", ]
  expect_equal(r1$score[r1$item == "g1"], 3)
  expect_equal(r1$score[r1$item == "g2"], 3)
  expect_true(all(agg$n == 2))
  expect_error(aggregate_items(toy, groups[1:3]), "group")
})

test_that("agreement between raters is computed on shared items", {
  ag = rater_agreement(toy, min_shared = 3)
  expect_equal(nrow(ag), 3L)
  expect_true(all(ag$shared == 4))
  expect_true(is.na(ag$rho[ag$rater_a == "r1" & ag$rater_b == "r3"]))
})

# the full pipeline on the simulated data
sim = simulate_farmer_ratings()
ratings = sim$ratings
raters = sim$raters
M = paired_comparisons(ratings)
gp = grouped_from_pairs(M)
covariates = raters[match(gp$raters, raters$rater), c("gender", "location")]
covariates[] = lapply(covariates, factor)
rownames(covariates) = NULL

test_that("a five-point scale on many plots leaves tie sets beyond what rankings can hold", {
  rt = rankings_with_ties(ratings)
  expect_gt(min(rt$max_tie_order), 4)
  info = rating_information(ratings)
  expect_gt(median(info$tied_share), 0.15)
})

test_that("the Plackett-Luce model recovers the order men used", {
  mod = PlackettLuce::PlackettLuce(gp$G)
  w = stats::coef(mod, log = TRUE)
  expect_gt(stats::cor(w, sim$truth$trait_a[names(w)]), 0.5)
  rel = gosset::reliability(mod, ref = "Reference")
  expect_equal(rel$reliability[rel$item == "Reference"], 0.5)
})

test_that("the tree splits on gender and not on location", {
  tree = fit_rater_tree(gp$G, covariates)
  sv = split_variables(tree)
  expect_equal(sv, "gender")
  node = as.integer(predict(tree, type = "node"))
  w = worth_by_node(tree, ref = "Reference")
  for (k in unique(node)) {
    g = names(which.max(table(covariates$gender[node == k])))
    truth = if (g == "woman") sim$truth$women else sim$truth$men
    wk = w[[paste0("node_", k)]]
    expect_gt(stats::cor(wk, truth[w$item]), 0.7)
  }
})

test_that("aggregation to families keeps a usable model", {
  fam = aggregate_items(ratings, sim$families)
  Gf = grouped_from_pairs(paired_comparisons(fam))$G
  mod_f = PlackettLuce::PlackettLuce(Gf)
  expect_equal(length(stats::coef(mod_f)), 9L)
})
