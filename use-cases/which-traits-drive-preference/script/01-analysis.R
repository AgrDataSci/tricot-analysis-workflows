# Which traits drive overall preference?
# Status: tested-on-synthetic-data
# Decision record: decisions/trait-associations.md
# Derived from: archive/groundnut-tanzania/02-kendall-partial-correlation-backward-selection.R
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R
# Replace the synthetic data with your own tricot data in the same format.

library("PlackettLuce")
library("gosset")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

dir.create("output", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
# synthetic data with a known answer, yield and taste drive preference,
# colour only looks important because it is correlated with yield
dat = simulate_tricot()

items = c("item_A", "item_B", "item_C")

traits = list(overall = c("overall_pos", "overall_neg"),
              yield = c("yield_pos", "yield_neg"),
              taste = c("taste_pos", "taste_neg"),
              colour = c("colour_pos", "colour_neg"),
              drought = c("drought_pos", "drought_neg"))

# the reference is overall preference. With ClimMob data, use the most
# recent overall-type question, whatever its code. A project allows only
# one "overall" code, so later overall rankings are often coded
# "generalappreciation", "generalpreference" or "grownext". Do not enter
# a later overall-type question as an explanatory trait.
# See decisions/ranking.md
reference = "overall"

# 2. Build rankings and check the design ----------------------------------
tr = build_trait_rankings(dat, items, traits)

# report how many rows each trait keeps after validation
print(data.frame(trait = names(tr$n_valid),
                 valid_rows = tr$n_valid,
                 total_rows = tr$n_rows))

# a disconnected network invalidates comparisons across its parts
check_connectivity(tr$rankings[[reference]])

# 3. Marginal and partial association with overall preference -------------
rk = row_kendall_all(tr$rankings)

assoc = trait_association(rk, reference, nboot = 500)

print(assoc, digits = 2)

# 4. Backward selection on partial correlations ---------------------------
sel = backward_selection(rk, reference, nboot = 500)

cat("\nRemoved, in order:", paste(sel$removed, collapse = ", "), "\n")
cat("Selected:\n")
print(sel$selected, digits = 2)

# 5. Subgroups ------------------------------------------------------------
# run the same analysis within each group and report the group size.
# Overlapping intervals are not a test of difference between groups
groups = split(seq_len(nrow(dat)), dat$gender)

by_group = do.call("rbind", lapply(names(groups), function(g) {
  rows = groups[[g]]
  rk_g = list(tau = rk$tau[rows, , , drop = FALSE],
              w = rk$w[rows, , , drop = FALSE])
  out = trait_association(rk_g, reference, nboot = 500)
  out$group = paste0(g, " (n = ", length(rows), ")")
  out
}))

print(by_group, digits = 2)

# 6. Outputs --------------------------------------------------------------
utils::write.csv(assoc, "output/trait-association.csv", row.names = FALSE)
utils::write.csv(sel$selected, "output/backward-selection.csv", row.names = FALSE)
utils::write.csv(by_group, "output/trait-association-by-group.csv", row.names = FALSE)

# marginal versus partial association, with bootstrap intervals
grDevices::png("output/marginal-vs-partial.png", width = 1600, height = 900, res = 200)
op = graphics::par(mfrow = c(1, 2), mar = c(4, 6, 2, 1))
k = nrow(assoc)
lims = range(0, assoc[, -1], na.rm = TRUE)
for (v in c("tau", "partial")) {
  graphics::plot(assoc[[v]], seq_len(k), xlim = lims, yaxt = "n", pch = 19,
                 xlab = if (v == "tau") "Kendall tau" else "Partial correlation",
                 ylab = "", main = if (v == "tau") "Marginal" else "Partial")
  graphics::segments(assoc[[paste0(v, "_lower")]], seq_len(k),
                     assoc[[paste0(v, "_upper")]], seq_len(k))
  graphics::abline(v = 0, lty = 2)
  graphics::axis(2, at = seq_len(k), labels = assoc$trait, las = 1)
}
graphics::par(op)
grDevices::dev.off()

sessionInfo()
