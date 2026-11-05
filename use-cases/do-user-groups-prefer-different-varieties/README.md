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

tricot rankings as in the ranking use case, plus covariates measured independently of the rankings (socioeconomic, environmental, management). Record when and how each covariate was collected.

## Assumptions

- Covariates are measured before, or independently of, the rankings.
- Each participant belongs to one group.
- The clustering covariates are chosen for the decision at hand, the users you want to target. Skewed variables are log-transformed.

## Approach

1. Cluster participants on all chosen covariates, numeric and categorical together, with model-based clustering and variable selection (`VarSelLCM::VarSelCluster`). Each variable is declared relevant or irrelevant to the clusters, so a variable unrelated to the segments does not shape them.
2. Compare k = 2 to 5 and choose k by ICL, which penalizes overlapping clusters. BIC is reported as well. It tends to split a segment into similar subgroups.
3. Check the smallest cluster share (at least 10% by default) and which variables drive the partition (`variable_influence()`, see below).
4. Test whether preferences differ between clusters with `gosset::likelihood_ratio()` and with a permutation test that shuffles cluster labels and refits (200 permutations). Then test every pair of clusters (`pairwise_heterogeneity()`). Clusters that do not differ in preference need no separate recommendation, even when their profiles differ.
5. Report worth per cluster and the cluster profiles, including the variables declared irrelevant.

## Choosing a clustering method for mixed data

Socioeconomic data mix numeric variables (farm size, distance to market) with categorical ones (gender of the household head, group membership). How a method weighs them decides what the clusters mean. The workflow fits three methods on the same data and compares them. In the simulation, household head is unrelated to the segments and enters every method.

| Method | k | Agreement with true segments (ARI) | Agreement with household head (ARI) |
|---|---|---|---|
| VarSelLCM, k by ICL (default) | 2 | 0.92 | 0.00 |
| Gower distance + PAM | 2 | 0.00 | 1.00 |
| FAMD components + PAM | 4 | 0.45 | 0.52 |

- **VarSelLCM** recovers the segments and declares household head irrelevant.
- **Gower distance with PAM** gives a binary variable the same weight as a whole continuous one, so it splits participants into women and men and misses the segments. Its stability is a perfect 1.00. A hijacked partition can be perfectly stable, so stability alone cannot detect the problem.
- **FAMD followed by PAM** balances variable types, but keeping components with eigenvalue above 1 retains the household-head dimension (eigenvalue 1.00), which pulls the partition. Retaining only the leading component recovers the segments (tested), so the number of components matters.

Whatever method you use, run `variable_influence()`. It refits leaving out one variable at a time and compares the clusters with each categorical variable. It flags household head in the Gower solution (agreement 1.00 with the clusters, 0.00 after removing it). Theory still comes first. A method can show that a variable is irrelevant to the structure, not whether it matters for the decision.

## Alternatives

- Plackett-Luce trees (`PlackettLuce::pltree`) split on covariates using the rankings. They find groups that differ in preference but give hierarchical binary partitions that may not be actionable personas.
- Gower distance with PAM and the stability search (`cluster_search()`), for numeric covariates only or with categorical variables down-weighted.
- FAMD components with PAM (`famd_scores()`, `cluster_search(metric = "euclidean")`), retaining few components.
- Latent class ranking models estimate groups and preferences jointly.
- Covariate models of worth, when one covariate is the question.

## Interpretation

On the synthetic data (800 participants, two segments with opposite preferences, household head unrelated), ICL selects k = 2. BIC would select k = 3, splitting the subsistence segment in two. The two clusters match the true segments (adjusted Rand index 0.92), and household head is declared irrelevant. Cluster 1 (59%) has smaller farms, sells 25% of the harvest and lives farther from market. Cluster 2 (41%) has larger farms, sells 73% and lives closer. Household head is balanced in both. Preferences clearly differ (deviance 486 on 7 df, permutation p = 0.005), and the worth per cluster ranks the varieties in nearly opposite order.

A significant test means the preference structure differs. Whether the difference justifies separate recommendations depends on the worth per cluster and on whether the groups can be reached in practice.

## From clusters to personas or TPE descriptions

Once the clusters are defensible, an AI assistant can draft user personas (socioeconomic clusters) or descriptions of the target population of environments (environmental clusters) from the output files. Give it `output/cluster-profile.csv`, `output/worth-by-cluster.csv`, `output/cluster-criteria.csv` and the cluster sizes. This is also the step where an assistant is most likely to invent details, so follow these rules.

**For every persona or TPE description**

1. Ground every statement in a number from the output files. If the data do not show it, leave it out.
2. Fictional names are fine and help people remember segments, but label each persona as a fictional summary of a cluster.
3. State the cluster size, the values that distinguish it from the other clusters, and its most and least preferred varieties. The preferred varieties make a persona actionable.
4. Flag patterns that look surprising rather than smoothing them into the story.
5. Describe gender and other social variables only as far as the data go. No stereotypes.
6. Mark the result as a draft for the field team and partners to check.

**Images**

Do not pair real farmers' photos with fictional profiles. Readers attach the story to the person, and slides travel without their disclaimers. Suggest generating an illustrative image with an AI image model available to the user instead.

- Label every image as AI-generated on the slide itself.
- Never prompt for a specific real person or use a real photo as a reference.
- Ground the image in the profile (age, gender, setting) and show the crop accurately.
- Use neutral, respectful field scenes, without poverty imagery or caricature.
- Save the image prompt with the outputs so the image can be traced and regenerated.

**For TPE descriptions from environmental clusters**

- Describe each cluster with the observed ranges of its climate and soil variables, not adjectives alone.
- Say whether the clusters describe the trial sites or the target region. They describe a TPE only when the target region was characterized as well.
- Check target sites against the trial conditions with the extrapolation check in [how-does-climate-shape-performance](../how-does-climate-shape-performance/).

**Worked example, amaranth in Tanzania**

Three socioeconomic clusters from amaranth tricot trials in Tanzania, from the [tricot data](https://doi.org/10.5281/zenodo.17112491). Cluster sizes are not shown here, and a real persona must state them.

| | Cluster 1 | Cluster 2 | Cluster 3 |
|---|---|---|---|
| Women (%) | 28.7 | 8.4 | 77.5 |
| Control production (%) | 40.2 | 14.0 | 93.5 |
| Control selling (%) | 38.4 | 21.8 | 84.7 |
| Control seed exchange (%) | 40.9 | 23.2 | 91.6 |
| Age | 41.1 | 37.9 | 46.4 |
| Household size | 4.4 | 3.7 | 4.4 |
| Number of other crops | 2.5 | 2.1 | 3.0 |
| Sell amaranth (%) | 4.1 | 76.9 | 68.5 |
| Distance to market (km) | 17.9 | 38.7 | 42.1 |
| Most preferred varieties | Nguruma, AVAM1941 | AVAM2302, Nguruma | AVAM2305, Akeri, AVAM2302 |
| Least preferred varieties | AVAM2016, Madiira 2 | AVAM2304 | AVAM1941 |

A first draft described cluster 1 as "a younger male farmer for whom amaranth farming is his main activity". The data do not support that. Cluster 1 is the middle age group, not the youngest, and only 4% of its members sell amaranth, so nothing shows amaranth is the main activity. The draft also missed the most striking pattern, since cluster 1 lives closest to market but almost never sells.

A grounded rewrite reads as follows.

> **Abdallah (fictional summary of cluster 1).** Mostly men (71%), average age 41, households of about four people, growing two to three other crops. Abdallah lives closest to market of the three groups, about 18 km on average, yet almost never sells amaranth (4%). He grows it mainly for the household. Decisions on production, selling and seed exchange rest with the respondent in about 40% of households. He prefers Nguruma, then AVAM1941, and ranks AVAM2016 and Madiira 2 lowest. Open question for the field team: why do farmers this close to market not sell?

Every number in the rewrite comes from the table. The interpretive step, "grows it mainly for the household", follows from the low share selling and is worth confirming in the field.

## Limitations

- Clusters depend on the covariates chosen and on their scaling. Report both.
- Stability is necessary but not sufficient. Clusters must also be interpretable and usable by the people who will act on them.
- With unbalanced cluster sizes, the smaller cluster's worth estimates are less precise.

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates two segments with different covariates and preferences.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
