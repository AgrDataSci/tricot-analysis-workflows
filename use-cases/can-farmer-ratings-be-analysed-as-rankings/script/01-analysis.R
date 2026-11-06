# Can farmer ratings be analysed as rankings?
# Status: reviewed-not-executed
# Decision record: decisions/data-synthesis.md, section "Ratings to rankings"
# Derived from: archive/etnam-farmer-ratings/01_farmers_oa.R (Gesesse et al.
#   2023, PNAS, https://doi.org/10.1073/pnas.2205774119; the same method in
#   Woldeyohannes et al. 2022, eLife, https://doi.org/10.7554/eLife.80009)
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R
#
# Scientific intent
#   Likert scores given by farmers to many plots are ratings, not rankings,
#   and they are not comparable between farmers. Keeping only the order
#   within each farmer, as paired comparisons, loses the magnitude and every
#   tied pair, but what remains can be modelled with Plackett-Luce and
#   partitioned by who the farmer is. The workflow makes the loss explicit
#   before drawing conclusions from what is left.

library("PlackettLuce")
library("gosset")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

dir.create("output", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
# ratings has one row per farmer and genotype with the score. raters has one
# row per farmer with the covariates
sim = simulate_farmer_ratings()
ratings = sim$ratings
raters = sim$raters

# 2. How much order is there in the ratings? ------------------------------
info = rating_information(ratings)
print(summary(info[, c("items", "distinct_scores", "tied_share", "untied_pairs")]))

agree = rater_agreement(ratings)
agree$same_gender = raters$gender[match(agree$rater_a, raters$rater)] ==
  raters$gender[match(agree$rater_b, raters$rater)]
print(stats::aggregate(rho ~ same_gender, data = agree, FUN = median))

# the alternative, rankings with ties kept, is not an option here. The
# largest tie set per farmer is far above the four that PlackettLuce handles
rt = rankings_with_ties(ratings)
cat("Largest tie set per farmer, range:", range(rt$max_tie_order), "\n")

# 3. Paired comparisons, ties dropped -------------------------------------
M = paired_comparisons(ratings)
cat("Pairs per farmer:", ncol(M),
    " untied and scored:", round(mean(!is.na(M)), 3), "\n")

gp = grouped_from_pairs(M)
G = gp$G
cat("Farmers kept:", length(gp$raters), " dropped:", length(gp$dropped), "\n")

# 4. Plackett-Luce model and reliability ----------------------------------
mod = PlackettLuce(G)
print(summary(mod, ref = "Reference"))

rel = reliability(mod, ref = "Reference")
print(rel, digits = 3)

# 5. Do women and men, or the two locations, order the genotypes differently?
covariates = raters[match(gp$raters, raters$rater), c("gender", "location")]
covariates[] = lapply(covariates, factor)
rownames(covariates) = NULL

tree = fit_rater_tree(G, covariates, alpha = 0.01, minsize = 10)
print(tree)
cat("Split variables:", paste(split_variables(tree), collapse = ", "), "\n")

worth = worth_by_node(tree, ref = "Reference")
print(worth, digits = 2)

# how the nodes relate to the truth of the simulation. Worth per node against
# the latent value each gender ordered by
node = as.integer(predict(tree, type = "node"))
for (k in sort(unique(node))) {
  g = names(which.max(table(covariates$gender[node == k])))
  w = worth[[paste0("node_", k)]]
  names(w) = worth$item
  truth = if (g == "woman") sim$truth$women else sim$truth$men
  cat("node", k, "mostly", g, "farmers, correlation of worth with their truth:",
      round(stats::cor(w, truth[names(w)]), 2), "\n")
}

# 6. When the item set is too large, aggregate to groups ------------------
# the paper aggregated lines to NAM families. The within-family order is lost
fam = aggregate_items(ratings, sim$families)
Mf = paired_comparisons(fam)
Gf = grouped_from_pairs(Mf)$G
mod_f = PlackettLuce(Gf)
cat("Family pairs:", ncol(Mf),
    "untied and scored:", round(mean(!is.na(Mf)), 3), "\n")
print(round(sort(stats::coef(mod_f, ref = "Reference", log = TRUE), decreasing = TRUE), 2))

# 7. Outputs --------------------------------------------------------------
write.csv(info, "output/rating-information.csv", row.names = FALSE)
write.csv(agree, "output/rater-agreement.csv", row.names = FALSE)
write.csv(rel, "output/reliability.csv", row.names = FALSE)
write.csv(worth, "output/worth-by-node.csv", row.names = FALSE)
write.csv(data.frame(rater = gp$raters, node = node, covariates),
          "output/raters-by-node.csv", row.names = FALSE)

png("output/worth-by-node.png", width = 1600, height = 1800, res = 150)
nodes = grep("^node_", names(worth), value = TRUE)
o = order(worth[[nodes[1]]])
op = par(mar = c(4, 7, 2, 1))
plot(worth[[nodes[1]]][o], seq_len(nrow(worth)), xlim = range(unlist(worth[nodes])),
     yaxt = "n", ylab = "", xlab = "log worth against the reference", pch = 19,
     main = "Worth per node of the farmer tree")
if (length(nodes) > 1) {
  for (k in 2:length(nodes)) points(worth[[nodes[k]]][o], seq_len(nrow(worth)), pch = k)
}
axis(2, at = seq_len(nrow(worth)), labels = worth$item[o], las = 1, cex.axis = 0.7)
abline(v = 0, lty = 2, col = "grey50")
legend("bottomright", legend = nodes, pch = seq_along(nodes), bty = "n")
par(op)
dev.off()

sessionInfo()
