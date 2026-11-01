# Do on-farm and on-station results agree?
# Status: tested-on-synthetic-data
# Decision record: decisions/genetic-gain.md
# Derived from: archive/wageningen-genetic-gain-cassava,
#   archive/wageningen-genetic-gain-groundnut, archive/potato-kang-et-al
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R

library("PlackettLuce")
library("gosset")
library("lme4")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

dir.create("output", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
sim = simulate_trials()

R = rank_tricot(sim$farm, c("item_A", "item_B", "item_C"),
                c("overall_pos", "overall_neg"), validate.rankings = TRUE)

# 2. On-station heritability and BLUPs ------------------------------------
st = station_heritability(sim$station)

print(st$components, digits = 3)
cat("Entry-mean H2 on station:", round(st$H2, 2), "\n")

# 3. On-farm preference and its reliability -------------------------------
wf = on_farm_worth(R)

rel = on_farm_reliability(R, nsplit = 50)

print(rel, digits = 3)

# 4. Agreement between the two sources ------------------------------------
ag = agreement(st$blup, wf, top = 5)

print(ag$summary, digits = 2)
print(ag$table, digits = 2, row.names = FALSE)

# 5. Outputs --------------------------------------------------------------
utils::write.csv(st$components, "output/station-variance-components.csv", row.names = FALSE)
utils::write.csv(ag$table, "output/station-vs-farm.csv", row.names = FALSE)
utils::write.csv(cbind(ag$summary, H2_station = st$H2, rel),
                 "output/agreement-summary.csv", row.names = FALSE)

grDevices::png("output/station-vs-farm.png", width = 1200, height = 1200, res = 200)
graphics::plot(ag$table$blup, ag$table$log_worth, pch = 19,
               xlab = "Station yield BLUP", ylab = "On-farm log-worth")
graphics::text(ag$table$blup, ag$table$log_worth, ag$table$genotype,
               pos = 3, cex = 0.6)
grDevices::dev.off()

sessionInfo()
