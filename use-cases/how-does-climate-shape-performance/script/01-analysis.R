# How do climate and environment shape variety performance?
# Status: tested-on-synthetic-data
# Decision record: decisions/climate-environment.md
# Derived from: archive/crop-variety-management-pnas, archive/etnam-farmer-ratings,
#   archive/osiris-1000farms/02-get-climate-data.R,
#   archive/worldveg-choose-growth-thrive/03-get-climate-covar.R
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R
#
# With real data, daily climate comes from a documented source (for example
# CHIRPS for rainfall and NASA POWER or AgERA5 for temperature, via the chirps
# and climatrends packages). Record the source, its spatial and temporal
# resolution, and the date it was downloaded.

library("PlackettLuce")
library("gosset")
library("climatrends")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

dir.create("output", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
sim = simulate_climate_trial()
dat = sim$trial

R = rank_tricot(dat, c("item_A", "item_B", "item_C"),
                c("overall_pos", "overall_neg"), validate.rankings = TRUE)

# 2. Climate window from phenology ----------------------------------------
# Phenology source: for real data cite the study or breeder information here.
# This simulation uses base 10 C and 900 degree days to flowering
window = phenology_window(sim$temp, dat$planting,
                          tbase = 10, degree_days = 900)

summary(window)

ci = climate_indices(sim$temp, sim$rain, dat$planting, window)

summary(ci)

# 3. Plackett-Luce tree on the climate indices ----------------------------
keep = stats::complete.cases(ci)

tree = fit_climate_tree(R[keep, ], ci[keep, ])

print(tree)

cat("Split variables:", paste(split_variables(tree), collapse = ", "), "\n")

# worth per node, relative to the check
nodes = partykit::nodeids(tree, terminal = TRUE)
worth = t(stats::coef(tree, ref = "Check", log = TRUE))
colnames(worth) = paste0("node_", nodes)
print(round(worth, 2))

# 4. Sensitivity to the window definition ---------------------------------
windows = list(
  gdd_to_flowering = window,
  fixed_30_days = rep(30, nrow(dat)),
  fixed_90_days = rep(90, nrow(dat))
)

ws = window_sensitivity(R, windows, sim$temp, sim$rain, dat$planting)

print(ws, digits = 4)

# 5. Can the results be applied to new sites? -----------------------------
# four target sites, typical, warm but observed, hotter than any trial plot,
# and much wetter than any trial plot
q = function(p) vapply(ci[keep, ], stats::quantile, numeric(1), probs = p)
target = rbind(typical = q(0.5), warm = q(0.75), q(0.5), q(0.5))
target[3, "maxDT"] = max(ci$maxDT, na.rm = TRUE) + 3
target[4, "Rtotal"] = 3 * max(ci$Rtotal, na.rm = TRUE)
rownames(target) = c("typical", "warm", "hotter", "wetter")
target = as.data.frame(target)

ex = extrapolation_check(ci[keep, ], target)

print(ex, digits = 3)

# 6. Outputs --------------------------------------------------------------
utils::write.csv(cbind(dat[c("id", "planting")], window = window, ci),
                 "output/climate-indices.csv", row.names = FALSE)
utils::write.csv(data.frame(item = rownames(worth), worth),
                 "output/worth-by-node.csv", row.names = FALSE)
utils::write.csv(ws, "output/window-sensitivity.csv", row.names = FALSE)
utils::write.csv(ex, "output/extrapolation-check.csv", row.names = FALSE)

grDevices::png("output/climate-tree.png", width = 2000, height = 1200, res = 150)
plot(tree)
grDevices::dev.off()

sessionInfo()
