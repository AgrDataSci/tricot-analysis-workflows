# Simulate on-station and on-farm (tricot) data for the same genotypes
#
# The truth built into the simulation
#   - Each genotype has a genetic value g with variance 1.
#   - On-station yield = location + rep + g + genotype-by-location + error,
#     with genotype-by-location variance 0.3 and residual variance 1, over 3
#     locations with 3 replicates. True entry-mean H2 = 1 / (1 + 0.1 + 0.111),
#     about 0.83.
#   - On-farm log-worth = b * g + d, where d (variance 0.25) is what farmers
#     value beyond station yield. With b = 0 farmer preference is unrelated to
#     station performance.

simulate_trials = function(n_genotypes = 15,
                           locations = 3,
                           reps = 3,
                           vg = 1,
                           vgl = 0.3,
                           ve = 1,
                           b = 0.8,
                           vd = 0.25,
                           n_farmers = 600,
                           seed = 2026) {

  set.seed(seed)

  genotypes = paste0("G", sprintf("%02d", seq_len(n_genotypes)))
  g = stats::setNames(stats::rnorm(n_genotypes, sd = sqrt(vg)), genotypes)

  station = expand.grid(genotype = genotypes,
                        rep = seq_len(reps),
                        location = paste0("L", seq_len(locations)),
                        stringsAsFactors = FALSE)

  loc_eff = stats::setNames(stats::rnorm(locations, 5, 1), unique(station$location))
  rep_eff = stats::rnorm(locations * reps, sd = 0.3)
  gl = stats::rnorm(n_genotypes * locations, sd = sqrt(vgl))

  rep_id = as.integer(factor(paste(station$location, station$rep)))
  gl_id = as.integer(factor(paste(station$genotype, station$location)))

  station$yield = loc_eff[station$location] + rep_eff[rep_id] +
    g[station$genotype] + gl[gl_id] + stats::rnorm(nrow(station), sd = sqrt(ve))

  lw = b * g + stats::rnorm(n_genotypes, sd = sqrt(vd))

  alloc = t(replicate(n_farmers, sample(genotypes, 3)))
  colnames(alloc) = c("item_A", "item_B", "item_C")

  gumbel = function(k) -log(-log(stats::runif(k)))

  bw = t(apply(alloc, 1, function(x) {
    o = order(lw[x] + gumbel(3), decreasing = TRUE)
    c(LETTERS[o[1]], LETTERS[o[3]])
  }))
  colnames(bw) = c("overall_pos", "overall_neg")

  farm = data.frame(id = seq_len(n_farmers), alloc, bw, stringsAsFactors = FALSE)

  H2 = vg / (vg + vgl / locations + ve / (locations * reps))

  list(station = station, farm = farm,
       truth = list(g = g, log_worth = lw, H2 = H2))

}
