# Data synthesis of heterogeneous trials

Status: methodological rationale from Kauê de Sousa (Bioversity International), 2026-11-06, from Brown et al. (2023, Agronomy Journal, [10.1002/agj2.21436](https://doi.org/10.1002/agj2.21436)), Brown et al. (2022, Crop Science), the cgiar-csi-2020 webinar, and for ratings from Gesesse et al. (2023, PNAS) and Woldeyohannes et al. (2022, eLife). This is not independent validation of cited studies or software.

## Default approach

- Convert each evaluation to a ranking of the genotypes it compared, then pool the rankings. Ranks remove the scale, so trials that scored the same trait with different metrics, at block or trial level, in different years and with different genotype sets can enter one Plackett-Luce analysis. Station yields across sites follow the same logic.
- Include a trial only when it has a measured response to the trait, coordinates, start and end dates of the evaluation period, and genotypes that overlap with other trials.
- Use one metric per trial, the one with the most data, and prefer the metric most trials share, so the pooled data rest on as few metrics as possible. State the direction of each metric. For a disease index lower is better, for days to symptoms or yield higher is better.
- One ranking per block when block data exist, otherwise one per trial mean. Each crop cycle is its own ranking, and a later cycle starts where the previous one ended.
- Resolve ties by keeping one genotype at random from each tied set. PlackettLuce handles ties up to order four, but many low-rank ties stop convergence. Check that the choice does not change the result.
- Keep only genotypes in the main strongly connected component and present in a minimum share of the rankings (5% in the paper, found by searching for the smallest value that lets the model converge while losing the least data). Use pseudo-rankings so the network stays connected after a split.
- Compute climate covariates over the evaluation period of each ranking, from a documented daily source (AgERA5 in the paper), with climatrends indices. Drop covariates with near-zero variance. Report strong correlations rather than removing covariates, since the tree picks one of a correlated set and the others are candidate explanations.
- Fit the Plackett-Luce tree in explanatory mode. Alpha 0.05 with Bonferroni correction, large minimum node size (35% of the rankings in the paper), pseudo-rankings. The aim is to explain, so small nodes with a sparse network are avoided.
- Assess stability by refitting on subsamples (80% without replacement, 1000 refits in the paper) and report how often each variable is selected and where the cut points fall. A split that appears in most refits is stable.
- Report log worth per node with quasi-standard errors, reliability against a reference genotype (the probability of outperforming it, Eskridge and Mumm 1992), the number of evaluations per genotype per node, and the goodness of fit. Use ranking probabilities and reliability together when selecting genotypes for further testing.

## Ratings to rankings

Scores that farmers give to plots on a short scale (overall appreciation from 1 to 5) are ratings, not rankings, and are not comparable between farmers. The default keeps only the order within each farmer.

- Count the information first, items scored, distinct scores used, share of tied pairs per farmer, agreement between farmers on shared items. The heritability of the scores (0.81 for overall appreciation in teff, 0.45 in the EtNAM) is the argument that ratings carry signal.
- Convert each farmer's scores to paired comparisons, a higher score wins, a tie is no information. Build the pairs in the order `psychotools::paircomp()` expects and turn them into grouped rankings with `PlackettLuce::as.grouped_rankings()`. Report the share of pairs kept and drop farmers left with none.
- Do not keep ties as tied rankings on a coarse scale. Tie sets on a five-point scale with dozens of plots are far above the order of four that PlackettLuce handles.
- Treat each farmer by replicate as a rater.
- When the item set is too large for the model, aggregate items to groups by the mean score per farmer (lines to NAM families in the paper) and state that the within-group order is lost.
- Then proceed as with any rankings, Plackett-Luce worth, reliability against a reference, a tree on rater covariates (gender, location) and item covariates from station data (pladmm, see `decisions/trait-associations.md`).

## Alternatives

- Mixed models on the metric data, when every trial used the same metric and the designs are compatible.
- Network meta-analysis of pairwise contrasts, which keeps the metric scale but needs a common metric.
- Plackett-Luce with genotype covariates (pladmm) to explain worth within nodes by genotype traits.
- Bootstrap instead of subsampling for stability. Subsampling keeps the network of each refit closer to the original.
- For ratings, mixed models or BLUPs of the scores, which keep the magnitude but treat the scale as interval data, and Bradley-Terry models on the paired comparisons directly.

## What the results cannot support

- A split shows that the ranking differs between conditions. It does not identify a mechanism. Climate of the evaluation period is confounded with site, year, management and the people who scored the trial.
- Failing to outperform a highly resistant reference is not poor performance. Compare genotypes with each other within the node as well.
- Genotypes evaluated only under one condition say nothing about the other. Report evaluations per node with the worth.
- Reliability with pseudo-rankings is conservative, because the pseudo-rankings shrink the worth estimates.
- Ranks lose the magnitude of differences. Expert knowledge of the trait and its metrics limits, but does not remove, the loss.
- With ratings, a coarse scale discards most pairs as ties, and worth is relative to the plots each farmer saw. A split by gender or location shows that the order differs, not why.

## Archive examples

- `archive/musa-blsd-data-synthesis/`
- `archive/cgiar-csi-2020/`
- `archive/etnam-farmer-ratings/`

## Source pointers

- [Brown et al. 2023, Agronomy Journal](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/brown-2023-agronj.md)
- [Brown et al. 2022, Crop Science](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/brown-2022-cropsci.md)
- [Brown et al. 2020, Agronomy for Sustainable Development, review of data synthesis for crop variety evaluation](https://doi.org/10.1007/s13593-020-00630-7)
- [Gesesse et al. 2023, PNAS](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/gesesse-2023-pnas.md)
- [Woldeyohannes et al. 2022, eLife](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/woldeyohannes-2022-elife.md)
- [de Sousa et al. 2023, SoftwareX, gosset](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/desousa-2023-softwarex.md)
