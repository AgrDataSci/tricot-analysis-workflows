# Simulate a concept test where consumers rank three product concepts each
#
# The truth built into the simulation
#   - 12 concepts combine four attributes, price (1 to 3), cooking time
#     (short, long), packaging (sack, branded) and biofortification (no, yes).
#   - Log-worth = -0.6 price - 0.8 long cooking + 0.4 branded + 0.5 biofortified.
#   - Urban consumers are twice as price sensitive as rural consumers when
#     segments = TRUE.
#   - With interaction = TRUE, branded packaging is valued only for short
#     cooking concepts, which the additive model cannot represent.

concept_features = function() {

  data.frame(
    concept = paste0("Concept ", LETTERS[1:12]),
    price = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 1, 3),
    cooking = factor(c("short", "long", "short", "long", "short", "long",
                       "short", "short", "long", "long", "long", "short"),
                     levels = c("short", "long")),
    packaging = factor(c("sack", "branded", "branded", "sack", "sack", "branded",
                         "branded", "sack", "branded", "sack", "sack", "branded"),
                       levels = c("sack", "branded")),
    biofortified = factor(c("no", "no", "yes", "yes", "no", "yes",
                            "no", "yes", "no", "yes", "yes", "no"),
                          levels = c("no", "yes")),
    stringsAsFactors = FALSE
  )

}

simulate_concept_test = function(n = 900,
                                 segments = TRUE,
                                 interaction = FALSE,
                                 seed = 2026) {

  set.seed(seed)

  f = concept_features()

  group = sample(c("urban", "rural"), n, replace = TRUE)

  base = -0.8 * (f$cooking == "long") + 0.5 * (f$biofortified == "yes")

  brand = if (interaction) {
    0.8 * (f$packaging == "branded" & f$cooking == "short") -
      0.8 * (f$packaging == "branded" & f$cooking == "long")
  } else {
    0.4 * (f$packaging == "branded")
  }

  price_coef = c(urban = -0.6, rural = -0.6)
  if (segments) price_coef = c(urban = -0.9, rural = -0.45)

  lw = sapply(price_coef, function(b) base + brand + b * f$price)
  rownames(lw) = f$concept

  alloc = t(replicate(n, sample(f$concept, 3)))
  colnames(alloc) = c("item_A", "item_B", "item_C")

  gumbel = function(k) -log(-log(stats::runif(k)))

  bw = t(vapply(seq_len(n), function(i) {
    u = lw[alloc[i, ], group[i]] + gumbel(3)
    o = order(u, decreasing = TRUE)
    c(LETTERS[o[1]], LETTERS[o[3]])
  }, character(2)))
  colnames(bw) = c("overall_pos", "overall_neg")

  list(data = data.frame(id = seq_len(n), alloc, bw, group = group,
                         stringsAsFactors = FALSE),
       features = f,
       truth = list(price = price_coef, cooking_long = -0.8, branded = 0.4,
                    biofortified = 0.5))

}
