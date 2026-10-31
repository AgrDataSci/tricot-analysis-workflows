# Which varieties do participants prefer, and how sure are we?
# Status: tested-on-synthetic-data
# Decision record: decisions/ranking.md
# Derived from: archive/groundnut-tanzania, archive/nextgen-cassava and the
#   ranking module of ClimMob-analysis
#
# Run from this folder with  Rscript analysis.R

library("PlackettLuce")
library("gosset")

source("functions.R")
source("data/simulate-data.R")

dir.create("outputs", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
dat = simulate_tricot()

items = c("item_A", "item_B", "item_C")
input = c("overall_pos", "overall_neg")
check = "Check"

# 2. Build rankings and check the design ----------------------------------
rk = build_rankings(dat, items, input)

cat("Valid rankings:", rk$n_valid, "of", rk$n_rows, "\n")
cat("Participants per variety:\n")
print(rk$n_per_item)

R = rk$rankings

check_connectivity(R)

# 3. Fit the Plackett-Luce model ------------------------------------------
mod = PlackettLuce(R)

# model adequacy compared with a model where all varieties are equal
print(model_adequacy(mod), digits = 3)

# 4. Worth, intervals and probability of beating the check ----------------
wt = worth_table(mod, ref = check)

print(wt, digits = 2)

# 5. How stable is the order? ---------------------------------------------
tp = top_probability(R, k = 3, nboot = 200)

res = merge(wt, tp, by = "item")
res = res[order(res$log_worth, decreasing = TRUE), ]

print(res[, c("item", "log_worth", "lower", "upper", "p_beat_check",
              "p_first", "p_top_k")], digits = 2, row.names = FALSE)

# 6. Outputs --------------------------------------------------------------
utils::write.csv(res, "outputs/variety-preference.csv", row.names = FALSE)

grDevices::png("outputs/log-worth.png", width = 1400, height = 1000, res = 200)
op = graphics::par(mar = c(4, 7, 1, 1))
k = nrow(wt)
graphics::plot(rev(wt$log_worth), seq_len(k), pch = 19, yaxt = "n",
               xlim = range(wt$lower, wt$upper),
               xlab = paste("Log-worth relative to", check), ylab = "")
graphics::segments(rev(wt$lower), seq_len(k), rev(wt$upper), seq_len(k))
graphics::abline(v = 0, lty = 2)
graphics::axis(2, at = seq_len(k), labels = rev(wt$item), las = 1)
graphics::par(op)
grDevices::dev.off()

sessionInfo()
