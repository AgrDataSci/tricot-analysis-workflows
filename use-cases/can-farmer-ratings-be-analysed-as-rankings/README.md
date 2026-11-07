# Can farmer ratings be analysed as rankings?

**Status** reviewed-not-executed
**Decision record** [data synthesis](../../decisions/data-synthesis.md), section "Ratings to rankings"
**Derived from** [archive/etnam-farmer-ratings](../../archive/etnam-farmer-ratings/) `01_farmers_oa.R` (Gesesse et al. 2023, PNAS, [10.1073/pnas.2205774119](https://doi.org/10.1073/pnas.2205774119)). The same method was used on teff in Woldeyohannes et al. 2022 (eLife, [10.7554/eLife.80009](https://doi.org/10.7554/eLife.80009)).

## Scientific question

Farmers scored many plots on a short scale, for example overall appreciation from 1 to 5. Can those scores be analysed as rankings, how much is lost on the way, and do groups of farmers order the genotypes differently?

## Intended use

Use it for participatory variety selection data collected as scores rather than as tricot rankings, typically on station with hundreds of plots. It gives a Plackett-Luce worth per genotype, reliability against a reference, and a tree showing whether women and men, or farmers at different locations, order the genotypes differently. The worth can then be explained by station traits in [which-measured-traits-explain-preference](../which-measured-traits-explain-preference/), which is where the item-covariate part of the original script lives.

It is not a way to recover the magnitude of farmer preference. The scores are kept only for their order within farmer.

## Input data

- Ratings with rater, item and score, plus replicate and location where they exist. With replicates, each farmer by replicate is a rater.
- The scale, the question farmers answered and how the scoring was organised (simultaneous scoring to limit within-group bias, as in the papers).
- Rater covariates, at least gender and location.
- Optional, a grouping of items (families, origins) for when the item set is too large.

## Assumptions

- Scores are comparable within a farmer, so their order is meaningful.
- A tie carries no preference. Two plots with the same score are treated as not compared.
- Farmers scored enough plots with enough distinct scores to leave untied pairs. The information summary checks this.

## Approach

1. Count the information in the ratings before converting them, items scored, distinct scores used and the share of tied pairs per farmer (`rating_information`), and the agreement between farmers on shared items (`rater_agreement`). The papers made the same case with the heritability of farmer scores (0.81 for overall appreciation in teff, 0.45 in the EtNAM), which shows that scores carry genetic signal.
2. Show why rankings with ties are not an option here. On a five-point scale with dozens of plots the largest tie set per farmer is far beyond the four that PlackettLuce handles (`rankings_with_ties`).
3. Build all paired comparisons per farmer, a higher score wins and a tie is NA (`paired_comparisons`), in the order `psychotools::paircomp()` expects. Report the share of pairs kept. Drop farmers left with no comparison (`grouped_from_pairs`).
4. Fit the Plackett-Luce model on the grouped rankings and report reliability against the reference (`gosset::reliability`).
5. Fit a Plackett-Luce tree on the rater covariates (`fit_rater_tree`, alpha 0.01, at least 10 farmers per node, as in the script) and report log worth per node (`worth_by_node`).
6. When the item set is too large for the model, aggregate items to groups by the mean score per farmer (`aggregate_items`), as the script did with NAM families, and say that the within-group order is lost.

## Alternatives

- Mixed models on the scores with farmer as a random effect. They keep the magnitude but treat a Likert score as interval data.
- BLUPs of the scores per genotype, the route the papers used for heritability and genomic selection, then rank the BLUPs.
- Rankings with ties when the scale is fine enough that tie sets stay at four or below.
- Bradley-Terry models on the paired comparisons directly (`psychotools::btmodel`), which give the same worth without the grouped-rankings step.

## Interpretation

On the synthetic data, 48 farmers score 30 of 40 genotypes each on a five-point scale. A sizeable share of the pairs is tied and dropped, and the largest tie set per farmer is well above four. The Plackett-Luce model on the untied pairs recovers the latent order, and the tree splits the farmers by gender and not by location, with the worth in each node following the order that gender used in the simulation. Aggregating the 39 lines to 8 families keeps a usable model of 9 items. Run the script for the numbers of the current package versions.

A split by gender shows that women and men ordered the genotypes differently. It does not say which traits drive the difference. That question goes to the measured-traits use case.

## Limitations

- Ties are dropped, so a coarse scale discards most pairs. Report the share kept and the number of distinct scores per farmer.
- Worth is relative to what each farmer saw. Genotypes rarely scored have wide uncertainty, and genotypes never scored together are compared only through the network.
- Aggregating items to groups hides the within-group differences that breeding selects on.
- `pltree()` needs PlackettLuce attached with `library("PlackettLuce")`, because it looks up its fitting functions on the search path.

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates the farmers, the scale use and the scores.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
- `references.bib` lists the publications behind the method.
