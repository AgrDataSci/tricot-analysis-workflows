# Simulate lab measurements and consumer rankings for the same genotypes
#
# The truth built into the simulation
#   - 14 genotypes are measured in the lab in two regions, with 3 samples per
#     genotype and region. Region shifts every trait.
#   - Measured traits are hardness, cohesiveness, springiness, colour (L*)
#     and soluble content. Soluble content is measured with much more noise,
#     so its repeatability is low.
#   - Gumminess = hardness x cohesiveness and chewiness = gumminess x
#     springiness, as in texture profile analysis.
#   - Consumer log-worth = 0.8 cohesiveness - 0.6 hardness + 0.4 colour, on
#     standardized genotype values. Springiness and soluble content have no
#     effect.
#   - 900 consumers each rank three genotypes.

simulate_trait_study = function(n_genotypes = 14,
                                samples = 3,
                                n_consumers = 900,
                                region_crossed = TRUE,
                                seed = 2026) {

  set.seed(seed)

  geno = paste0("G", sprintf("%02d", seq_len(n_genotypes)))

  z = function(n) as.vector(scale(stats::rnorm(n)))
  true = data.frame(genotype = geno,
                    hardness = z(n_genotypes),
                    cohesiveness = z(n_genotypes),
                    springiness = z(n_genotypes),
                    colour_l = z(n_genotypes),
                    soluble = z(n_genotypes))

  regions = c("Region A", "Region B")

  lab = if (region_crossed) {
    expand.grid(genotype = geno, region = regions, sample = seq_len(samples),
                stringsAsFactors = FALSE)
  } else {
    data.frame(genotype = rep(geno, each = samples * 2),
               region = rep(rep(regions, length.out = n_genotypes), each = samples * 2),
               sample = rep(seq_len(samples * 2), n_genotypes),
               stringsAsFactors = FALSE)
  }

  k = match(lab$genotype, geno)
  shift = ifelse(lab$region == "Region B", 0.5, 0)

  # measurement scales and noise per trait
  lab$hardness = 900 + 150 * (true$hardness[k] + shift + stats::rnorm(nrow(lab), sd = 0.3))
  lab$cohesiveness = 0.55 + 0.05 * (true$cohesiveness[k] + shift + stats::rnorm(nrow(lab), sd = 0.3))
  lab$springiness = 85 + 4 * (true$springiness[k] + shift + stats::rnorm(nrow(lab), sd = 0.3))
  lab$colour_l = 70 + 3 * (true$colour_l[k] + shift + stats::rnorm(nrow(lab), sd = 0.3))
  lab$soluble = 5 + 0.8 * (true$soluble[k] + shift + stats::rnorm(nrow(lab), sd = 2.5))
  lab$gumminess = lab$hardness * lab$cohesiveness
  lab$chewiness = lab$gumminess * lab$springiness / 100

  # rows in random order, as lab sheets often are
  lab = lab[sample(nrow(lab)), ]
  rownames(lab) = NULL

  lw = stats::setNames(0.8 * true$cohesiveness - 0.6 * true$hardness +
                         0.4 * true$colour_l, geno)

  alloc = t(replicate(n_consumers, sample(geno, 3)))
  colnames(alloc) = c("option_a", "option_b", "option_c")

  gumbel = function(n) -log(-log(stats::runif(n)))

  bw = t(apply(alloc, 1, function(x) {
    o = order(lw[x] + gumbel(3), decreasing = TRUE)
    c(LETTERS[o[1]], LETTERS[o[3]])
  }))
  colnames(bw) = c("best_taste", "worst_taste")

  list(lab = lab,
       consumers = data.frame(id = seq_len(n_consumers), alloc, bw,
                              stringsAsFactors = FALSE),
       truth = list(genotype = true, log_worth = lw,
                    effects = c(cohesiveness = 0.8, hardness = -0.6,
                                colour_l = 0.4, springiness = 0, soluble = 0)))

}
