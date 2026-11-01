# How many participants do we need?
# Status: tested-on-synthetic-data
# Decision record: decisions/ranking.md
#
# Scientific intent
#   Show how much each extra answer improves the ranking, to set a target
#   for the number of participants and to tell participants why their
#   answer counts. See script/helper/functions.R for the reasoning.
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R

library("PlackettLuce")
library("gosset")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

dir.create("output", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
# replace with your own data in the ClimMob format
dat = simulate_tricot()

n_registered = nrow(dat)
answered = !is.na(dat$overall_pos) & !is.na(dat$overall_neg)
n_now = sum(answered)

cat("Registered:", n_registered, " answered:", n_now, "\n")

# 2. Fit the model to the answers available now ---------------------------
R = build_rankings(dat[answered, ])
mod = PlackettLuce(R)

# the fitted log-worths are the truth of the simulation
truth = stats::coef(mod, log = TRUE)
truth = truth[!grepl("^tie", names(truth))]

print(round(sort(truth, decreasing = TRUE), 2))

# 3. Expected accuracy by number of participants --------------------------
sizes = sort(unique(c(round(n_now / 2), n_now, 50, n_registered, 150, 250)))

curve = design_curve(truth, sizes = sizes, nsim = 100, top = 3)

print(curve, digits = 2, row.names = FALSE)

cat("\nWith", n_now, "answers, chance of finding the best item:",
    round(100 * curve$p_winner[curve$n == n_now]), "%\n")
cat("If all", n_registered, "registered answered:",
    round(100 * curve$p_winner[curve$n == n_registered]), "%\n")
cat("Participants needed for an 80% chance of finding the best item:",
    n_for_target(curve, 0.8, "p_winner"), "\n")

# 4. Why not subsample the real answers -----------------------------------
# agreement with the full data rises towards 1 as n approaches n_now
sub_sizes = c(round(n_now / 2), round(0.8 * n_now), n_now - 1)
naive = naive_subsample_agreement(R, sub_sizes, nsim = 50)

print(data.frame(n = sub_sizes,
                 naive_subsample_tau = round(naive, 2),
                 simulated_tau = round(stats::approx(curve$n, curve$tau, sub_sizes, rule = 2)$y, 2)),
      row.names = FALSE)

# 5. Outputs --------------------------------------------------------------
utils::write.csv(curve, "output/design-curve.csv", row.names = FALSE)

grDevices::png("output/design-curve.png", width = 1600, height = 1000, res = 200)
op = graphics::par(mar = c(4.5, 4.5, 1, 1))
graphics::plot(curve$n, curve$tau, type = "n", ylim = c(0, 1),
               xlab = "Participants who answer", ylab = "Share or correlation")
graphics::polygon(c(curve$n, rev(curve$n)), c(curve$tau_lower, rev(curve$tau_upper)),
                  col = grDevices::adjustcolor("#1E4532", 0.15), border = NA)
graphics::lines(curve$n, curve$tau, lwd = 2, col = "#1E4532", type = "b", pch = 19)
graphics::lines(curve$n, curve$p_winner, lwd = 2, col = "#D9952B", type = "b", pch = 15)
graphics::abline(v = c(n_now, n_registered), lty = 3, col = "grey40")
graphics::legend("bottomright", bty = "n", lwd = 2, pch = c(19, 15),
                 col = c("#1E4532", "#D9952B"),
                 legend = c("Kendall tau with the true order", "Chance of finding the best item"))
graphics::par(op)
grDevices::dev.off()

sessionInfo()
