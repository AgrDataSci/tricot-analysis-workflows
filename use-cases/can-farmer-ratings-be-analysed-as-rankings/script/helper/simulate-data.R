# Simulate farmer ratings of genotypes on a five-point scale
#
# The design follows the participatory variety selection in Gesesse et al.
# (2023) and Woldeyohannes et al. (2022). Women and men farmers at two
# locations score plots for overall appreciation from 1 (poor) to 5
# (excellent). Each farmer uses the scale in their own way, with their own
# mean and spread, so scores are not comparable between farmers, only within.
#
# The truth built into the simulation
#   - 40 genotypes in 8 families of 5, except "Reference", which stands alone
#     and has the highest value of trait A.
#   - Two latent traits per genotype, A (yield-like) and B (a quality trait).
#     Men order genotypes by A. Women order them by A and B together. So the
#     order differs by gender, and the location has no effect.
#   - The score is a noisy, coarse reading of the latent value, which creates
#     many ties. Each farmer scores a random subset of the genotypes.

simulate_farmer_ratings = function(n_items = 40,
                                   raters_per_location = 24,
                                   items_per_rater = 30,
                                   weight_b = 1.2,
                                   seed = 2026) {

  set.seed(seed)

  items = c("Reference", sprintf("Line %02d", seq_len(n_items - 1)))
  families = c("Reference", paste0("F", rep(1:8, length.out = n_items - 1)))
  names(families) = items

  trait_a = stats::setNames(c(1.8, stats::rnorm(n_items - 1)), items)
  trait_b = stats::setNames(c(0, stats::rnorm(n_items - 1)), items)

  locations = c("Adet", "Kulumsa")
  raters = data.frame(
    rater = sprintf("r%02d", seq_len(2 * raters_per_location)),
    location = rep(locations, each = raters_per_location),
    gender = rep(rep(c("woman", "man"), each = raters_per_location / 2), 2),
    stringsAsFactors = FALSE
  )
  # how each farmer uses the scale
  raters$shift = stats::rnorm(nrow(raters), 0, 0.4)
  raters$spread = stats::runif(nrow(raters), 0.6, 1.1)

  ratings = do.call("rbind", lapply(seq_len(nrow(raters)), function(i) {
    g = sort(sample(items, items_per_rater))
    wb = if (raters$gender[i] == "woman") weight_b else 0
    latent = trait_a[g] + wb * trait_b[g]
    score = 3 + raters$shift[i] + raters$spread[i] * latent + stats::rnorm(length(g), sd = 0.5)
    data.frame(rater = raters$rater[i], location = raters$location[i],
               gender = raters$gender[i], item = g,
               score = as.integer(pmin(pmax(round(score), 1), 5)),
               stringsAsFactors = FALSE)
  }))

  list(ratings = ratings,
       raters = raters[, c("rater", "location", "gender")],
       families = families,
       truth = list(men = trait_a,
                    women = trait_a + weight_b * trait_b,
                    trait_a = trait_a, trait_b = trait_b))

}
