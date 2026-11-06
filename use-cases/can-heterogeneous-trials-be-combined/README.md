# Can trials with different measurements and designs be combined into one analysis?

**Status** reviewed-not-executed
**Decision record** [data synthesis](../../decisions/data-synthesis.md)
**Derived from** [archive/musa-blsd-data-synthesis](../../archive/musa-blsd-data-synthesis/) (Brown et al. 2023, Agronomy Journal, [10.1002/agj2.21436](https://doi.org/10.1002/agj2.21436)) and [archive/cgiar-csi-2020](../../archive/cgiar-csi-2020/) `wheat.R`, where station yields from 255 sites in India were first pooled as rankings

## Scientific question

Trials scored the same trait with different metrics (a disease index, days to symptoms, leaf counts, yield), at different levels (block data or trial means), with different genotype sets, in different years and places. Can they be pooled into one analysis, and does the environment of each evaluation change which genotypes perform best?

## Intended use

Use it to make sense of trial data scattered across repositories, theses and partner files, which were never designed to be analysed together. The output is a ranking of genotypes per environment, with reliability against a reference, which supports the choice of genotypes for further testing at new locations. The same logic applies to on-station yield trials across sites, the starting point in cgiar-csi-2020, and to the on-farm synthesis in Brown et al. (2022).

It does not replace a multi-environment trial. Ranks lose the magnitude of differences, and the pooled data are unbalanced, so genotypes seen in few evaluations have wide uncertainty.

## Input data

- Observations with trial, cycle, block (NA when only means are reported), genotype and one column per metric. Genotype names harmonized across trials beforehand.
- Trial metadata with coordinates.
- Evaluation periods with start and end dates per trial and cycle. The end is the date the trait was scored (shooting, flowering or harvest).
- For each metric, its direction, whether higher or lower values mean better performance, with the source of the scale.
- Daily rainfall and temperature per trial covering every evaluation period. With real data these come from a documented source, for example AgERA5 through ag5Tools (used in the paper) or CHIRPS and NASA POWER through the chirps and climatrends packages. Relative humidity, which the paper also used, is added as extra columns when available.

Record the climate source, its spatial and temporal resolution and the download date.

## Assumptions

- The metrics chosen in different trials order the genotypes the same way for the trait. Converting to ranks removes the scale, not a disagreement between metrics.
- The evaluation period is known for every unit, so the climate covariates describe the period the trait was exposed to.
- One ranking per block is a fair replicate. Where only means exist, the trial mean is one ranking, which weighs that trial less.

## Approach

1. Screen trials against the inclusion criteria of Brown et al. (2023), a measured response to the trait, coordinates, start and end dates, and genotype overlap with the other trials (`screen_trials`).
2. Choose one metric per trial, the one with the most data, with ties going to the metric most trials share (`choose_metric`), so the pooled data rest on as few metrics as possible.
3. Rank genotypes within each evaluation unit, a block within a cycle or the cycle itself, in the direction of the metric (`rank_within_units`), and aggregate into one sparse matrix with 0 for absent genotypes (`rank_matrix`).
4. Resolve ties by keeping one genotype at random from each tied set (`resolve_ties`), then remove genotypes outside the main strongly connected component and genotypes present in fewer than 5% of the units, repeating until stable (`filter_network`).
5. Set the evaluation window of each cycle, with later cycles starting where the previous one ended (`evaluation_window`), and compute rainfall and temperature indices with climatrends over it (`climate_indices`). Drop covariates with near-zero variance and report strong correlations (`screen_covariates`).
6. Fit a Plackett-Luce tree in explanatory mode (`fit_synthesis_tree`), alpha 0.05 with Bonferroni correction, minimum node size 35% of the units and two pseudo-rankings to keep the network connected after a split.
7. Refit the tree on subsamples of 80% of the units and count how often each variable is selected (`split_stability`), following Philipp et al. (2018) and the stablelearner package used in the paper.
8. Report log worth per node against the reference (`worth_by_node`), reliability, the probability of outperforming the reference (`node_reliability`, which wraps `gosset::reliability`, Eskridge and Mumm 1992, and falls back to worth without standard errors when quasi-variances fail), the number of evaluations per genotype per node (`evaluations_by_node`) and the goodness of fit (`goodness_of_fit`).

## Alternatives

- Mixed models on the metric data when all trials used the same metric and compatible designs.
- Network meta-analysis of pairwise contrasts, which keeps the metric scale but needs a common metric.
- Plackett-Luce with genotype covariates (`PlackettLuce::pladmm`) to explain worth by genotype traits, such as landrace against hybrid or genome group, within each node. The archived scripts hold a first attempt.
- Bootstrap instead of subsampling for the stability assessment. Subsampling keeps the network of each refit closer to the original.

## Interpretation

On the synthetic data, 23 of 26 trials pass the screening and give about 80 rankings of about 25 genotypes on three metrics. The tree splits the units on a rainfall index of the evaluation period, the simulated driver is the maximum length of dry spell, and never on temperature. In the dry node the seven dry-sensitive genotypes lose worth relative to the reference, and in the other node they do not. The split variable is selected in most of the 100 subsample refits. Reliability of the reference is 0.5 by definition, and genotypes evaluated in few units in a node have wide quasi-standard errors. Run the script to see the numbers for the current versions of the packages.

A split shows that the ranking differs between conditions. It is consistent with the simulated effect, and in real data it is a hypothesis about the environment, not a mechanism.

## Limitations

- Information loss. Ranks drop the magnitude of differences. Expert knowledge about the trait helps choose metrics that order genotypes consistently, which limits the loss.
- Unbalanced coverage. Genotypes tested only under one condition say nothing about the other. Report the evaluations per node with the worth.
- Confounding. Climate of the evaluation period goes with site, year, management and the people who scored the trial. Failing to outperform a highly resistant reference is not poor performance.
- Node size. Large nodes keep the network connected and the worth stable, at the cost of finer splits. The 35% rule is a choice for explanation, not prediction.
- `pltree()` needs PlackettLuce attached with `library("PlackettLuce")`, because it looks up its fitting functions on the search path.

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates the trials, the climate and the metrics.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
- `references.bib` lists the publications behind the method.
