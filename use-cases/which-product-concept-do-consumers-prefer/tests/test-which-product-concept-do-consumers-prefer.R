# Tests for the use case "Which product concept do consumers prefer?"

library("testthat")

items = c("item_A", "item_B", "item_C")
input = c("overall_pos", "overall_neg")
form = ~ price + cooking + packaging + biofortified

sim = simulate_concept_test(segments = FALSE)
R = build_rankings(sim$data, items, input)$rankings
mod = fit_attribute_model(R, form, sim$features)
ae = attribute_effects(mod)
est = stats::setNames(ae$estimate, ae$term)

test_that("attribute effects recover the simulated values", {
  expect_lt(abs(est["price"] - (-0.6)), 0.15)
  expect_lt(abs(est["cookinglong"] - (-0.8)), 0.2)
  expect_lt(abs(est["packagingbranded"] - 0.4), 0.2)
  expect_lt(abs(est["biofortifiedyes"] - 0.5), 0.2)
})

test_that("the attribute table is matched by name, not by position", {
  shuffled = sim$features[sample(nrow(sim$features)), ]
  mod2 = fit_attribute_model(R, form, shuffled)
  expect_equal(stats::coef(mod2), stats::coef(mod), tolerance = 1e-3)
  expect_error(fit_attribute_model(R, form, sim$features[-1, ]),
               "concepts without attributes")
})

test_that("the additive model is adequate when the truth is additive", {
  expect_gt(attribute_adequacy(R, mod)$p_value, 0.05)
})

test_that("a missing interaction is detected", {
  s2 = simulate_concept_test(segments = FALSE, interaction = TRUE)
  R2 = build_rankings(s2$data, items, input)$rankings
  m2 = fit_attribute_model(R2, form, s2$features)
  expect_lt(attribute_adequacy(R2, m2)$p_value, 0.01)
})

test_that("group differences in price sensitivity are detected", {
  s3 = simulate_concept_test(segments = TRUE)
  rk3 = build_rankings(s3$data, items, input)
  g = s3$data$group[rk3$valid]
  bg = attribute_effects_by_group(rk3$rankings, g, form, s3$features)
  expect_lt(bg$test$p_value, 0.01)
  e = bg$effects[bg$effects$term == "price", ]
  expect_lt(e$estimate[e$group == "urban"], e$estimate[e$group == "rural"])
})

test_that("predictions outside the tested levels are refused", {
  new = data.frame(concept = "New", price = 5,
                   cooking = factor("short", levels = c("short", "long")),
                   packaging = factor("sack", levels = c("sack", "branded")),
                   biofortified = factor("no", levels = c("no", "yes")))
  expect_error(predict_concepts(mod, sim$features, new, "Concept A"),
               "outside the tested levels")
  new$price = 2
  p = predict_concepts(mod, sim$features, new, "Concept A")
  expect_true(p$p_preferred_over_reference > 0 & p$p_preferred_over_reference < 1)
})
