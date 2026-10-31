# Which varieties do participants prefer, and how sure are we?

**Status** tested-on-synthetic-data
**Decision record** [ranking](../../decisions/ranking.md)
**Derived from** the ranking steps in [archive/groundnut-tanzania](../../archive/groundnut-tanzania/), [archive/nextgen-cassava](../../archive/nextgen-cassava/) and [ClimMob-analysis](https://github.com/AgrDataSci/ClimMob-analysis)

## Scientific question

Which varieties do participants prefer overall, how large are the differences, and how confident can we be in the order?

## Intended use

This is the core analysis of any tricot trial. Use it to report variety preference to breeders, seed systems and participants, and as the starting point for the other use cases.

It does not fit when preferences clearly differ between groups or environments. Run it first, then see the use cases on user groups and on climate.

## Input data

One row per participant in the ClimMob tricot format.

| Column | Content |
|---|---|
| `item_A`, `item_B`, `item_C` | names of the three varieties tested |
| `overall_pos`, `overall_neg` | best and worst variety, as A, B or C |

Rows with ties, missing values or letters other than A, B, C are dropped and counted.

## Assumptions

- Participants are independent.
- Choices follow the Plackett-Luce model. The odds of preferring one variety over another do not depend on the third variety in the set.
- One worth per variety describes the whole population.
- The comparison network is connected. The workflow warns when it is not.

## Approach

1. Build rankings with `gosset::rank_tricot()` and report how many rows and participants per variety remain.
2. Check network connectivity.
3. Fit `PlackettLuce::PlackettLuce()`.
4. Compare it with a model where all varieties are equally preferred (likelihood-ratio test and McFadden R2).
5. Report log-worth relative to the check with quasi-standard errors from `qvcalc`. Quasi-SEs let any two varieties be compared, not only each variety against the check.
6. Report the probability that each variety beats the check (`gosset::reliability()`).
7. Resample participants 200 times and refit, to get the probability that each variety ranks first and within the top 3.

## Alternatives

- Mean ranks or win counts are fine for a quick look. They ignore which varieties each participant compared and are biased under incomplete block designs.
- Thurstonian (probit) models are under investigation and should not be treated as equivalent.
- Use Plackett-Luce trees or covariate models when worth depends on context.

## Interpretation

On the synthetic data (600 participants, 10 varieties, known worth), the model clearly rejects equal preference (McFadden R2 0.19, p < 0.001). Variety 01 has the highest log-worth (1.14, 95% interval 0.88 to 1.40) and beats the check with probability 0.76. It ranks first in 99.5% of the resamples. Varieties 01 and 03 are in the top 3 in at least 99% of resamples and Variety 02 in 84%. The fourth place is uncertain, so a recommendation should name a group of top varieties, not a strict order.

Worth is relative to the varieties in the trial. It is not yield, and it does not predict adoption or market share.

## Limitations

- A variety tested by few participants has wide intervals. Check the participants-per-variety table.
- A low McFadden R2 is normal for tricot data. It reflects noisy individual choices, not a wrong ranking.
- Ignoring real heterogeneity between groups or environments can produce an average that suits nobody.

## Files

- `functions.R` holds the documented functions.
- `analysis.R` is the workflow, run with `Rscript analysis.R` from this folder.
- `data/simulate-data.R` simulates tricot data from a Plackett-Luce model with known worth.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
