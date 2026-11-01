# How many participants do we need?

**Status** reviewed-not-executed
**Decision record** [ranking](../../decisions/ranking.md)
**Derived from** the analysis of a tricot trial of forage mixtures in Norway (2026), where few registered farms returned their rankings, and the `data_value` step of [ClimMob-analysis](https://github.com/AgrDataSci/ClimMob-analysis)

## Scientific question

How much does each extra answer improve the ranking of the items, and how many participants would a trial need to find the best item with a given confidence?

## Intended use

Use it when the response rate is low and the team must decide whether the current answers are enough, how many more to aim for, or how to explain to participants why their answer counts. It also helps plan the size of a new trial when log-worths from an earlier trial of the same items are available.

It does not fit when the question is whether those who did not answer differ from those who did. More answers from the same kind of participant do not correct non-response bias.

## Input data

One row per registered participant in the ClimMob tricot format.

| Column | Content |
|---|---|
| `item_A`, `item_B`, `item_C` | names of the three items tested |
| `overall_pos`, `overall_neg` | best and worst item as A, B or C, `NA` when the participant did not answer |

Alternatively, a named vector of log-worths from an earlier trial or from expert expectation.

## Assumptions

- The fitted log-worths are the truth of the simulation. With few answers they are shrunk towards each other by the pseudo-data of PlackettLuce, so the curve is conservative.
- Each simulated participant tests three items drawn at random. A balanced design does slightly better at small sample sizes.
- Participants are independent and follow the Plackett-Luce model.

## Approach

1. Fit a Plackett-Luce model to the answers available now.
2. Take its log-worths as the truth. Simulate tricot trials of n new participants, each ranking three random items following the model (log-worth plus Gumbel noise).
3. Refit each simulated trial and compare it with the truth: Kendall tau over all items, whether the best item is found, and the share of the true top three that is recovered.
4. Repeat 100 times for each n, including the current answers and all registered participants, and read off the n that reaches a target.

### Why not subsample the real answers

A tempting shortcut draws subsamples of the real answers and compares each fit with the fit on all of them. The subsamples share participants with the full data, so agreement climbs towards 1 as n approaches the number of answers, whatever the real accuracy. It measures overlap, not accuracy, and cannot say what more answers would bring. The first analysis that motivated this use case made this mistake. `naive_subsample_agreement()` is kept to show the bias, and a test checks that it overstates accuracy.

## Alternatives

- Analytical power for one item against the check, from the standard error of the log-worth difference.
- A Bayesian prior on worths from earlier trials of the same crop.

## Interpretation

On the synthetic data (12 items, 93 registered, 35 answers), the curve gives the chance of finding the best item with the 35 answers and with all 93, and the number of participants needed for an 80% chance. Report it as "if the current estimates were true, this many answers would find the best item this often". It is a planning and communication tool, not a property of the trial.

## Limitations

- Accuracy depends on how different the items really are. When they are close, no realistic number of participants separates them, and the curve shows it.
- The truth is itself estimated. With very few answers, use log-worths from an earlier trial or a range of plausible spreads instead.
- Non-response can bias the results even when n is large.

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates a trial with low response.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
- `references.bib` lists the references.
