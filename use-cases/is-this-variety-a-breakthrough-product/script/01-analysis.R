# Is this variety a breakthrough product?
# Status: tested-on-synthetic-data. This script and the tests run in the continuous integration of the repository. The
#   Python reference in script/other-language/ was run and gave the numbers in
#   the README.
# Decision record: decisions/breakthrough-product.md
#
# Scientific intent
#   A breakthrough product gives at least 25% more productivity or value than
#   the check in farmers' fields (Cairns et al. 2026). This script estimates
#   the yield gain of each variety over the check, the confidence that the
#   average gain reaches 25%, and the reliability, the share of farms where
#   the variety beats the check and where it does so by 25%.
# Assumptions
#   Farms represent where the check is grown. One residual variance on the log
#   scale. See script/helper/functions.R.
# Alternatives
#   One mixed model on all plots, or a Bayesian probability model (ProbBreed).
# Limits
#   On-farm data only. The decision combines station and on-farm evidence, and
#   station data are not read. The verdict is about the yield gain only, since
#   yield is not the single driver of farmer choice. Thresholds are provisional.
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R

library("PlackettLuce")
library("gosset")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

dir.create("output", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
# one row per plot, with trial, farm, genotype, yield and, for the preference
# side, the farmer's rank of the plot (1 is best)
dat = simulate_breakthrough()

check = "Check"
threshold = 0.25

cat("Plots:", nrow(dat), "| zero yield:", sum(dat$yield <= 0),
    "| trials:", length(unique(dat$trial)),
    "| farms:", length(unique(dat$farm)), "\n")

# 2. Step 1, contrasts to the check per trial ----------------------------
con = trial_contrasts(dat, check)

print(attr(con, "skipped"))

# 3. Step 2 and the decision table ----------------------------------------
tab = breakthrough_table(dat, check, threshold = threshold, nboot = 200)

# 4. Preference, the value side of the definition -------------------------
pref = preference_reliability(dat, check)

res = merge(tab, pref, by = "genotype", all.x = TRUE)
res = res[order(res$log_gain, decreasing = TRUE), ]

print(res[, c("genotype", "gain_ratio", "lower", "upper", "p_mean_gain",
              "rel", "rel_threshold", "rel_threshold_lower",
              "rel_threshold_upper", "n_plots", "n_trials", "n_pairs",
              "direct_rel_threshold", "share_failed", "p_preferred",
              "verdict")], digits = 2, row.names = FALSE)

# 5. Outputs --------------------------------------------------------------
utils::write.csv(res, "output/breakthrough-table.csv", row.names = FALSE)

grDevices::png("output/gain-over-check.png", width = 1400, height = 1000, res = 200)
op = graphics::par(mar = c(4, 7, 1, 1))
k = nrow(res)
graphics::plot(rev(res$gain_ratio), seq_len(k), pch = 19, yaxt = "n", log = "x",
               xlim = range(res$lower, res$upper, 1 + threshold),
               xlab = paste("Yield relative to", check), ylab = "")
graphics::segments(rev(res$lower), seq_len(k), rev(res$upper), seq_len(k))
graphics::abline(v = 1, lty = 2)
graphics::abline(v = 1 + threshold, lty = 3)
graphics::axis(2, at = seq_len(k), labels = rev(res$genotype), las = 1)
graphics::par(op)
grDevices::dev.off()

sessionInfo()
