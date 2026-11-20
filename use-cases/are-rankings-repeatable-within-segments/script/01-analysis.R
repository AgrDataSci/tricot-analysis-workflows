# Are rankings repeatable within segments?
# Status: tested-on-synthetic-data. This script and the tests run in the
#   continuous integration of the repository. The Python reference in
#   script/other-language/ gave the numbers on open data in the README.
# Decision record: decisions/repeatability.md
#
# Scientific intent
#   Measure how repeatable the ranking of items is, how many participants it
#   takes, and whether segments (market segments, envirotypes) are more
#   repeatable than the unsegmented data.
# Assumptions
#   Independent participants, three items each, best and worst, no ties.
#   Segments defined without the rankings.
# Alternatives
#   Cullis heritability with many items, Thurstonian models, a factor-analytic
#   mixed model across environments.
# Limits
#   Repeatability of item means is not predictability of single answers, and
#   it is not validity.
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R

library("PlackettLuce")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")
source("script/helper/envirotypes.R")

dir.create("output", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
# one row per participant, with the three items shown, the position of the
# best and the worst, and the segment labels
dat = simulate_segments()

ord = as_ordered(dat)
dat = dat[attr(ord, "rows"), ]

cat("Participants:", nrow(dat), "| dropped:", attr(ord, "dropped"),
    "| items:", length(attr(ord, "levels")), "\n")

# 2. How repeatable is the unsegmented ranking, and at what n -------------
curve = repeatability_curve(ord, sizes = c(50, 100, 200, 400, 800))
print(curve, digits = 3)

# single answers are another matter
print(individual_fit(ord, dat$segment), digits = 3)

# 3. The fair test of each segmentation -----------------------------------
# "segment" is a real difference in preference, "noise" is not
tests = rbind(cbind(segmentation = "segment", segment_test(ord, dat$segment)),
              cbind(segmentation = "noise", segment_test(ord, dat$noise)))
print(tests, digits = 3)

cv = rbind(cbind(segmentation = "segment", cv_gain(ord, dat$segment)),
           cbind(segmentation = "noise", cv_gain(ord, dat$noise)))
print(cv, digits = 3)

# 4. Equal n, descriptive -------------------------------------------------
eq = equal_n(ord, dat$segment, m = 200)
print(eq, digits = 3)

# 5. Reliability against a reference item ---------------------------------
rel = benchmark_reliability(ord, ref = "C4", segment = dat$segment)

# 5b. Segments and envirotypes built without the rankings ------------------
# Market segments from several household variables at once. Replace cov with
# the numeric household variables of the data, one row per participant.
cov = data.frame(sold = ifelse(dat$segment == "market", 70, 15) + stats::rnorm(nrow(dat), 0, 10),
                 land = stats::rlnorm(nrow(dat), ifelse(dat$segment == "market", 1.2, 0.2), 0.4))
hh = build_segments(cov, k = 2, log = "land")
print(hh$profile)
cat("stability of the household segments:", round(hh$stability, 2), "\n")
print(as.data.frame(segment_test(ord, hh$segment)))

# Envirotypes from daily climate. With real data read the files written by the
# chirps-rainfall and chirts-temperature skills of environment-data,
#   rain = utils::read.csv("rain.csv.gz"); temp = utils::read.csv("temp.csv.gz")
# and give each participant the id of its point.
points = sprintf("P%02d", 1:40)
zone = rep(c("wet", "dry"), each = 20)
clim = simulate_climate(points, zone)
indices = climate_indices(clim$rain, clim$temp)
print(screen_indices(indices))
env = envirotypes(indices, k = 2)
print(env$profile)
cat("stability of the envirotypes:", round(env$stability, 2), "\n")
# in this simulation the points are assigned at random, so the envirotypes
# should not pass the test
dat$point = sample(points, nrow(dat), replace = TRUE)
print(as.data.frame(segment_test(ord, env$envirotype[dat$point])))

# 6. Outputs --------------------------------------------------------------
utils::write.csv(curve, "output/repeatability-curve.csv", row.names = FALSE)
utils::write.csv(tests, "output/segment-tests.csv", row.names = FALSE)
utils::write.csv(eq, "output/equal-n.csv", row.names = FALSE)
utils::write.csv(rel, "output/reliability-reference.csv", row.names = FALSE)

grDevices::png("output/repeatability-curve.png", width = 1400, height = 1000, res = 200)
graphics::plot(curve$n, curve$repeatability, type = "b", pch = 19, log = "x",
               ylim = c(0, 1), xlab = "Participants",
               ylab = "Repeatability of the item means")
graphics::segments(curve$n, curve$lower, curve$n, curve$upper)
graphics::abline(h = 0.8, lty = 3)
grDevices::dev.off()

sessionInfo()
