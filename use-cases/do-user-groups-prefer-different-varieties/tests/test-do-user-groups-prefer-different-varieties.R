# Tests for the use case "Do different groups of users prefer different varieties?"

library("testthat")

items = c("item_A", "item_B", "item_C")
input = c("overall_pos", "overall_neg")
covars = c("farm_size", "share_sold", "market_distance", "household_head")

prep = function(d) {
  X = d[c("farm_size", "share_sold", "market_distance")]
  X$farm_size = log(X$farm_size)
  X$market_distance = log(X$market_distance)
  X
}

test_that("adjusted Rand index behaves as expected", {
  a = rep(1:3, each = 10)
  expect_equal(adjusted_rand(a, a), 1)
  expect_equal(adjusted_rand(a, 4 - a), 1)
  set.seed(1)
  expect_lt(abs(adjusted_rand(a, sample(a))), 0.2)
})

test_that("the two simulated segments are recovered and preferences differ", {
  dat = simulate_tricot()
  R = build_rankings(dat, items, input)$rankings
  cs = cluster_search(prep(dat), k = 2:4, nboot = 15)
  expect_equal(cs$k, 2)
  group = cs$labels[["2"]]
  expect_gt(adjusted_rand(group, attr(dat, "segment")), 0.6)
  het = preference_heterogeneity(R, group, nperm = 50)
  expect_lt(het$p_chisq, 0.001)
  expect_lt(het$p_permutation, 0.05)
})

prep_all = function(d) {
  X = prep(d)
  X$household_head = d$household_head
  X
}

test_that("the influence diagnostic flags a variable that hijacks the partition", {
  dat = simulate_tricot()
  X = prep_all(dat)
  pam2 = function(x) cluster::pam(cluster::daisy(x, metric = "gower"), k = 2,
                                  diss = TRUE, cluster.only = TRUE)
  g = pam2(X)
  # Gower distance gives the binary factor full weight and PAM splits on it
  expect_gt(adjusted_rand(g, dat$household_head), 0.9)
  expect_warning(infl <- variable_influence(X, g, pam2), "driven by")
  expect_true(infl$drives_partition[infl$variable == "household_head"])
})

test_that("variable selection recovers the segments with household head included", {
  dat = simulate_tricot()
  vs = cluster_varsel(prep_all(dat), k = 2:4)
  expect_true("household_head" %in% vs$irrelevant)
  expect_gt(adjusted_rand(vs$labels, attr(dat, "segment")), 0.6)
  # every cluster is nested in one true segment, even if a segment is split
  tab = table(vs$labels, attr(dat, "segment"))
  expect_true(all(apply(prop.table(tab, 1), 1, max) > 0.85))
})

test_that("pairwise tests separate clusters with different preferences only", {
  dat = simulate_tricot()
  R = build_rankings(dat, items, input)$rankings
  seg = attr(dat, "segment")
  # split the subsistence segment at random into two clusters
  set.seed(4)
  g = ifelse(seg == "market", 1, sample(2:3, length(seg), replace = TRUE))
  pw = pairwise_heterogeneity(R, g)
  expect_lt(pw$p_value[pw$cluster_a == 1 & pw$cluster_b == 2], 0.001)
  expect_gt(pw$p_value[pw$cluster_a == 2 & pw$cluster_b == 3], 0.01)
})

test_that("FAMD scores cluster on the leading component", {
  dat = simulate_tricot()
  sc = famd_scores(prep_all(dat), ncp = 1)
  g = cluster_search(sc, k = 2, nboot = 5, metric = "euclidean")$labels[["2"]]
  expect_gt(adjusted_rand(g, attr(dat, "segment")), 0.6)
})

test_that("no heterogeneity is detected when segments share preferences", {
  dat = simulate_tricot(heterogeneity = FALSE)
  R = build_rankings(dat, items, input)$rankings
  group = cluster_search(prep(dat), k = 2, nboot = 5)$labels[["2"]]
  het = preference_heterogeneity(R, group, nperm = 50)
  expect_gt(het$p_permutation, 0.05)
})

test_that("unstable cluster solutions are reported, not forced", {
  set.seed(3)
  noise = data.frame(a = stats::rnorm(300), b = stats::rnorm(300),
                     c = stats::rnorm(300))
  expect_message(cs <- cluster_search(noise, k = 2:4, nboot = 15,
                                      min_stability = 0.9),
                 "no cluster solution")
  expect_true(is.na(cs$k))
})

test_that("cluster profiles cover numeric and categorical covariates", {
  dat = simulate_tricot(n = 200)
  g = rep(1:2, length.out = 200)
  p = cluster_profile(dat[covars], g)
  expect_true(all(c("farm_size", "household_head = woman") %in% p$variable))
  expect_equal(ncol(p), 3)
})


# Predefined groups ---------------------------------------------------------

test_that("group eligibility drops unlabelled and small groups", {
  g = c(rep("man", 40), rep("woman", 12), rep("Not specified", 5))
  e = group_eligibility(g, min_n = 20)
  expect_equal(e$eligible[e$group == "man"], TRUE)
  expect_equal(e$reason[e$group == "woman"], "too_few")
  expect_equal(e$reason[e$group == "Not specified"], "no_label")
  expect_true(is.na(e$share[e$group == "Not specified"]))
})

test_that("a site effect is not read as a gender effect once sites are held fixed", {
  dat = simulate_known_groups(gender_effect = FALSE)
  rk = build_rankings(dat, items, input)
  R = rk$rankings
  d = dat[rk$valid, ]
  expect_warning(cc <- confounding_check(d$gender, d$site), "associated")
  expect_true(cc$confounded)
  # pooled over sites, women and men look different
  naive = preference_heterogeneity(R, d$gender, nperm = 20)
  expect_lt(naive$p_chisq, 0.001)
  # within sites they do not
  st = stratified_heterogeneity(R, d$gender, d$site, nperm = 50)
  expect_gt(st$p_chisq, 0.01)
  expect_gt(st$p_permutation, 0.01)
  expect_equal(st$strata_dropped, "")
})

test_that("a real gender effect survives stratification", {
  dat = simulate_known_groups(gender_effect = TRUE)
  rk = build_rankings(dat, items, input)
  d = dat[rk$valid, ]
  st = stratified_heterogeneity(rk$rankings, d$gender, d$site, nperm = 50)
  expect_lt(st$p_chisq, 0.001)
  expect_lt(st$p_permutation, 0.05)
})

test_that("strata without enough answers in every group are left out and listed", {
  dat = simulate_known_groups(share_women_north = 0.01)
  rk = build_rankings(dat, items, input)
  d = dat[rk$valid, ]
  st = stratified_heterogeneity(rk$rankings, d$gender, d$site, nperm = 0)
  expect_equal(st$strata_dropped, "north")
  expect_equal(st$strata_used, "south")
})

test_that("worth comes with quasi-standard errors and the n behind it", {
  dat = simulate_known_groups()
  rk = build_rankings(dat, items, input)
  d = dat[rk$valid, ]
  w = worth_with_intervals(rk$rankings, d$gender, ref = "Check")
  expect_setequal(unique(w$group), c("man", "woman"))
  expect_equal(sum(table(d$gender)), sum(tapply(w$n, w$group, unique)))
  expect_true(all(is.finite(w$quasi_se[w$item != "Check"])))
  expect_true(all(w$lower <= w$estimate & w$estimate <= w$upper, na.rm = TRUE))
})

test_that("balanced ranking gives every group the same total weight", {
  dat = simulate_known_groups()
  rk = build_rankings(dat, items, input)
  d = dat[rk$valid, ]
  b = balanced_ranking(rk$rankings, d$gender)
  expect_equal(sum(b$worth_pooled), 100)
  expect_equal(sum(b$worth_balanced), 100)
  ag = agreement_between_groups(rk$rankings, d$gender)
  expect_equal(nrow(ag), 1)
  expect_true(ag$kendall_tau >= -1 && ag$kendall_tau <= 1)
})
