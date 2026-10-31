# Which traits drive overall preference?

**Status** tested-on-synthetic-data
**Decision record** [trait associations](../../decisions/trait-associations.md)
**Derived from** [archive/groundnut-tanzania](../../archive/groundnut-tanzania/), script `02-kendall-partial-correlation-backward-selection.R`

## Scientific question

When participants rank varieties for several traits and for overall preference, which traits are associated with overall preference once the other traits are taken into account?

## Intended use

Use this workflow to prioritize traits for breeding or variety recommendation from tricot data, and to explain what participants value. It fits single-stage tricot trials where every participant ranks the same three items for every trait.

It does not fit when traits were ranked by different participants, when the question is about variety-level drivers (use Plackett-Luce models with item covariates), or when the aim is to estimate the causal effect of a trait.

## Input data

One row per participant, in the ClimMob tricot format.

| Column | Content |
|---|---|
| `item_A`, `item_B`, `item_C` | names of the three items tested |
| `<trait>_pos`, `<trait>_neg` | best and worst item for each trait, as A, B or C |
| grouping variables | optional, for example gender |

Rows with ties, missing values, letters other than A, B, C, or best equal to worst are dropped for that trait and counted in the output. All ranked traits are evaluated by participants. Measured traits are out of scope here. If they are added, record observer, instrument, scale, timing and units first.

## Assumptions

- Participants are the independent units. Uncertainty comes from resampling participants, which keeps the dependence between traits ranked by the same person.
- The comparison network is connected. The workflow warns when it is not.
- The conversion r = sin(pi * tau / 2) is exact for bivariate normal latent scores and approximate otherwise.

## Approach

1. Build one rankings object per trait with `gosset::rank_tricot()`.
2. Compute Kendall tau per participant for every pair of traits, and average across participants weighting by the number of compared pairs. This reproduces `gosset::kendallTau()`.
3. Estimate the marginal association of each trait with overall preference.
4. Convert the tau matrix to the Pearson scale and invert it to get partial correlations with overall preference.
5. Get 95% intervals from 500 bootstrap resamples of participants.
6. Run backward selection on the partial correlations. At each step drop the trait whose interval sits deepest across zero, recompute, and stop when every remaining interval excludes zero.
7. Repeat steps 3 to 5 within groups, reporting group sizes.

## What changed from the archived script

The archived script computed partial correlations (`cor2pcor`) but never used them. Its p-values came from the marginal correlations, so the "backward selection" did not actually condition on the other traits. Removing a trait does not change the other marginal correlations, so the loop effectively became a single filter on marginal p-values. The p-values also used the mean effective N of the whole matrix and a t-test on Pearson-converted tau, which [gosset](https://github.com/AgrDataSci/gosset) now treats as too conservative for large tricot trials.

This version selects on partial correlations, derives uncertainty from participants, and reports marginal and partial associations side by side. It also keeps negative associations rather than filtering them out, because a trait that lowers overall preference matters for breeding too.

## Alternatives

- Marginal Kendall tau only (`gosset::kendallTau()`), when the aim is description.
- Plackett-Luce models of overall preference with trait worths as item covariates (`PlackettLuce::pladmm()`), when the question is at variety level.
- Rank-ordered logit of overall rankings on within-row trait rankings, when the question is at participant level.

## Interpretation

On the synthetic data, yield and taste are selected and drought is dropped. Colour has a clear positive marginal tau but a partial correlation near zero, because it is correlated with yield at variety level. This is the pattern the workflow is designed to expose.

A selected trait is associated with overall preference conditional on the traits in the set. It is not proven to cause preference, and it can be a proxy for something that was not ranked.

## Limitations

- Partial correlations depend on the set of traits tested. Always report the full set.
- Selection by interval is not operational importance. Report effect sizes, not only the selected list.
- Overlapping group intervals are not a test of difference between groups. Bootstrap the difference when that is the question.
- With few participants per trait the bootstrap intervals are unstable. Check the valid row counts first.

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates tricot data with a known answer.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
- `references.bib` lists the references.
