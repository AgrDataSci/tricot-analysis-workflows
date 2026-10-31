# Do different groups of users prefer different varieties?

**Status** tested-on-synthetic-data
**Decision record** [socioeconomic heterogeneity](../../decisions/socioeconomic-heterogeneity.md) and [ranking](../../decisions/ranking.md)
**Derived from** [archive/groundnut-tanzania](../../archive/groundnut-tanzania/) `05-data-analysis-clusters.R`, [archive/mip-concept-testing](../../archive/mip-concept-testing/) `02-clusters-identification.R`, [archive/osiris-1000farms](../../archive/osiris-1000farms/) `03-clusters-identification.R`

## Scientific question

Can participants be grouped into interpretable user types from who they are and how they farm, and do those groups prefer different varieties?

## Intended use

Use it to define breeding target users or to differentiate recommendations. The groups are built from covariates only, without the rankings, and the preference test comes afterwards. That keeps the test from being circular and gives groups that can be recognized in the field without knowing anyone's rankings.

It does not fit when the aim is to find whichever covariate best splits preferences. Use Plackett-Luce trees for that.

## Input data

Tricot rankings as in the ranking use case, plus covariates measured independently of the rankings (socioeconomic, environmental, management). Record when and how each covariate was collected.

## Assumptions

- Covariates are measured before, or independently of, the rankings.
- Each participant belongs to one group.
- The clustering covariates are chosen deliberately and transformed where needed. Skewed variables are log-transformed.

## Approach

1. Compute Gower distance on the clustering covariates and partition around medoids (`cluster::daisy`, `cluster::pam`).
2. Search k = 2 to 5. For each k, record stability (mean adjusted Rand index over 30 bootstrap reclusterings), separation (average silhouette width) and the share of the smallest cluster.
3. A solution is defensible when stability is at least 0.75 and the smallest cluster holds at least 10% of participants. Choose the most stable, then the best separated, then the smallest k. If none is defensible, the workflow stops and says so.
4. Test whether preferences differ between clusters with `gosset::likelihood_ratio()` and with a permutation test that shuffles cluster labels and refits (200 permutations). The permutation test does not rely on the chi-squared approximation.
5. Report worth per cluster and the cluster profiles, including covariates left out of the distance.

## A pitfall the tests document

With Gower distance a binary factor gets the same weight as a whole continuous covariate. In the simulation, adding household head to the distance made PAM split participants into women and men (adjusted Rand index above 0.9 with household head) and lose the real segments completely. Keep such factors out of the distance, or down-weight them with the `weights` argument of `daisy`, and use them to describe the clusters.

## Alternatives

- Plackett-Luce trees (`PlackettLuce::pltree`) split on covariates using the rankings. They find groups that differ in preference but give hierarchical binary partitions that may not be actionable personas.
- Latent class ranking models estimate groups and preferences jointly.
- Covariate models of worth, when one covariate is the question.

## Interpretation

On the synthetic data (800 participants, two segments with opposite preferences) k = 2 is the only defensible solution (stability 1.00, silhouette 0.57, smallest cluster 41%). k = 3 to 5 are unstable (stability 0.58 to 0.70). The two clusters match the true segments (adjusted Rand index 0.93). Cluster 1 has larger farms, sells 73% of the harvest and lives closer to market. Cluster 2 has smaller farms, sells 25% and lives farther away. Household head is balanced in both. Preferences clearly differ (deviance 498 on 7 df, permutation p = 0.005), and the worth per cluster shows the varieties ranked in nearly opposite order.

A significant test means the preference structure differs. Whether the difference justifies separate recommendations depends on the worth per cluster and on whether the groups can be reached in practice.

## Limitations

- Clusters depend on the covariates chosen and on their scaling. Report both.
- Stability is necessary but not sufficient. Clusters must also be interpretable and usable by the people who will act on them.
- With unbalanced cluster sizes, the smaller cluster's worth estimates are less precise.

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates two segments with different covariates and preferences.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
