# Can trials with different measurements and designs be combined into one
# analysis?
# Status: reviewed-not-executed
# Decision record: decisions/data-synthesis.md
# Derived from: archive/musa-blsd-data-synthesis (Brown et al. 2023, Agronomy
#   Journal, https://doi.org/10.1002/agj2.21436) and
#   archive/cgiar-csi-2020/wheat.R (station yields pooled as rankings)
#
# Run from the use-case folder (open the .Rproj) with  Rscript script/01-analysis.R
#
# Scientific intent
#   Trials that scored the same trait with different metrics, at different
#   levels and with different genotype sets cannot be pooled on their original
#   scales. Converting each evaluation to a ranking removes the scale, and a
#   Plackett-Luce tree on climate covariates of the evaluation period shows
#   whether the ranking changes with the environment. The output supports the
#   choice of genotypes for further testing at new locations.
#
# With real data, daily climate comes from a documented source (for example
# AgERA5 through ag5Tools, as in the paper, or CHIRPS and NASA POWER through
# the chirps and climatrends packages). Record the source, its spatial and
# temporal resolution and the download date, and keep the evaluation period
# of every unit with the data.

library("PlackettLuce")
library("gosset")
library("climatrends")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

dir.create("output", showWarnings = FALSE)

# 1. Read data ------------------------------------------------------------
# obs has one row per genotype per block (or per trial when only means are
# reported), with the metric columns. meta has one row per trial. cycles has
# one row per trial and crop cycle with the evaluation period
sim = simulate_heterogeneous_trials()
obs = sim$obs
meta = sim$meta
cycles = sim$cycles
metrics = sim$metrics

# 2. Screen trials against the inclusion criteria -------------------------
screen = screen_trials(meta, obs, metrics = metrics$metric, min_overlap = 2)
print(screen)

included = screen$trial[screen$include]
cat("Trials included:", length(included), "of", nrow(screen), "\n")

obs = obs[obs$trial %in% included, ]
cycles = cycles[cycles$trial %in% included, ]

# 3. One metric per trial, then rankings ----------------------------------
# priority follows the metric most trials share, so the pooled data rest on
# as few metrics as possible
metric_per_trial = choose_metric(obs, priority = c("dii", "yls", "ddt"))
print(metric_per_trial)
print(table(metric_per_trial$metric))

obs$metric = metric_per_trial$metric[match(obs$trial, metric_per_trial$trial)]
obs$value = vapply(seq_len(nrow(obs)), function(i) obs[[obs$metric[i]]][i],
                   numeric(1))
obs$higher_is_better = metrics$higher_is_better[match(obs$metric, metrics$metric)]

# an evaluation unit is a block within a cycle, or the cycle itself when only
# means were reported
obs$unit = paste0(obs$trial, "_c", obs$cycle, "_b",
                  ifelse(is.na(obs$block), "mean", obs$block))

ranks = rank_within_units(obs[, c("unit", "genotype", "value")],
                          obs$higher_is_better)
M = rank_matrix(ranks)

cat("Rankings:", nrow(M), " genotypes:", ncol(M), "\n")

# 4. Ties, connectivity and representation --------------------------------
ties = resolve_ties(M, method = "keep_one")
cat("Entries dropped to resolve ties:", ties$dropped, "\n")

net = filter_network(ties$M, min_presence = 0.05)
M = net$M
print(net$removed)
cat("After filtering, rankings:", nrow(M), " genotypes:", ncol(M), "\n")

R = as.rankings(M)
print(connectivity(adjacency(R), verbose = FALSE)$no)

# 5. Climate of the evaluation period -------------------------------------
# a second cycle starts where the first ended
cycles = evaluation_window(cycles)

ci_cycle = climate_indices(sim$rain[cycles$trial_index, , drop = FALSE],
                           sim$temp[cycles$trial_index, , , drop = FALSE],
                           cycles$start, cycles$span)
ci_cycle$key = paste0(cycles$trial, "_c", cycles$cycle)

# expand to the units kept in M, in the same order as the rows of M
units = data.frame(unit = rownames(M),
                   key = sub("_b.*$", "", rownames(M)),
                   stringsAsFactors = FALSE)
covariates = ci_cycle[match(units$key, ci_cycle$key), names(ci_cycle) != "key"]
rownames(covariates) = NULL

sc = screen_covariates(covariates, cor_cutoff = 0.9)
covariates = sc$covariates
cat("Covariates removed for near-zero variance:",
    paste(sc$removed, collapse = ", "), "\n")
print(sc$correlated)

# 6. Plackett-Luce tree in explanatory mode -------------------------------
tree = fit_synthesis_tree(M, covariates, alpha = 0.05, bonferroni = TRUE,
                          minsize_prop = 0.35, npseudo = 2)
print(tree)

cat("Split variables:", paste(split_variables(tree), collapse = ", "), "\n")
cat("First cut point:", first_cutpoint(tree), "\n")

# 7. Stability of the split under subsampling -----------------------------
# the paper used B = 1000. 100 is enough to see whether the variable holds
stability = split_stability(M, covariates, B = 100, v = 0.8)
print(stability$frequency)
cat("Refits with no split:", stability$no_split, "of 100\n")

# 8. Worth, reliability and coverage per node -----------------------------
worth = worth_by_node(tree, ref = "Reference")
print(worth, digits = 2)

rel = node_reliability(tree, ref = "Reference")
print(rel, digits = 3)

evals = evaluations_by_node(tree, M)
print(evals)

gof = goodness_of_fit(tree, M)
print(gof, digits = 3)

# how the nodes relate to the truth of the simulation
node = as.integer(predict(tree, type = "node"))
truth = cycles$dry[match(units$key, paste0(cycles$trial, "_c", cycles$cycle))]
print(table(node = node, dry = truth))

# 9. Outputs --------------------------------------------------------------
write.csv(screen, "output/trial-screening.csv", row.names = FALSE)
write.csv(metric_per_trial, "output/metric-per-trial.csv", row.names = FALSE)
write.csv(cbind(unit = rownames(M), as.data.frame(M)), "output/rank-matrix.csv",
          row.names = FALSE)
write.csv(net$removed, "output/genotypes-removed.csv", row.names = FALSE)
write.csv(cbind(units, covariates), "output/climate-covariates.csv",
          row.names = FALSE)
write.csv(worth, "output/worth-by-node.csv", row.names = FALSE)
write.csv(rel, "output/reliability-by-node.csv", row.names = FALSE)
write.csv(evals, "output/evaluations-by-node.csv", row.names = FALSE)
write.csv(stability$frequency, "output/split-stability.csv", row.names = FALSE)
write.csv(stability$cutpoints, "output/split-cutpoints.csv", row.names = FALSE)
write.csv(gof, "output/goodness-of-fit.csv", row.names = FALSE)

png("output/synthesis-tree.png", width = 2400, height = 1400, res = 150)
plot(tree)
dev.off()

sessionInfo()
