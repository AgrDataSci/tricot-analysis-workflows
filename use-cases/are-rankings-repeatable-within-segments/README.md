# Are rankings repeatable within segments?

**Status** reviewed-not-executed. The R scripts are a draft and have not been run. The Python reference in `script/other-language/` was run on synthetic data and on open data, and gave every number below.
**Decision record** [repeatability of rankings](../../decisions/repeatability.md), a draft awaiting review
**Domain leads to consult** Joost van Heerwaarden and Hugo Dorado-Betancourt (Wageningen University)
**Derived from** Dorado-Betancourt et al. (2026) for the two-step estimation and Eskridge and Mumm (1992) for split-half repeatability and reliability against a reference. No archived script.

## Scientific question

How repeatable is a ranking of items, how many participants does it take, and is it more repeatable within market segments or envirotypes than in the unsegmented data?

## Intended use

Use it when someone doubts that ranking data repeat, or proposes that results only make sense within segments. It turns both positions into numbers. It works for variety trials and for concept or consumer tests, wherever each participant ranks three items with a best and a worst.

Do not use it to decide whether a preferred item will be adopted. Repeatability says the ranking is stable, not that it is right.

## Input data

One row per participant.

| Column | Content |
|---|---|
| `item_1`, `item_2`, `item_3` | the three items shown |
| `best`, `worst` | the position (A, B or C) of the item chosen as best and as worst |
| segment columns | any label defined without looking at the rankings, for example country, gender, market orientation, an envirotype |
| a block column | optional, village or district, for cross-validation on held-out places |

## Assumptions

- Participants are independent, and each ranks three items without ties.
- Segments are defined without the rankings.
- Estimation errors of the log-worths are treated as independent.

## Approach

1. **Repeatability of the item means.** Fit a Plackett-Luce model, take the log-worth of each item and its variance, and estimate the variance among items. The repeatability is the share of the variance among estimated item means that is real difference between items (`repeatability()`).
2. **A curve against n.** Repeat on subsamples of 50, 100, 200 and more participants (`repeatability_curve()`). The curve is the sample-size guidance.
3. **Single answers.** Report how well the model predicts one participant's ranking, as a pseudo R2 (`individual_fit()`), so it is not confused with the repeatability of the result.
4. **The fair test of a segmentation.** Compare the segments with random segments of the same sizes (`segment_test()`). `p_deviance` says whether preferences differ between segments. `p_repeatability` says whether the segments are more repeatable than random groups of the same sizes.
5. **Held-out prediction.** Report the gain in held-out log-likelihood per ranking from one model per segment, with whole blocks held out (`cv_gain()`).
6. **Equal n.** Describe the pooled data and each segment at the same number of participants, with split-half correlations (`equal_n()`).
7. **A reference item.** Report the probability that each item is ranked above a benchmark, pooled and per segment (`benchmark_reliability()`).

## Alternatives

- The Cullis heritability of Dorado-Betancourt et al. (2026), with many items. With eight items it cannot exceed about 0.875, which is why this use case departs from it.
- Thurstonian models in place of Plackett-Luce.
- A Plackett-Luce tree to find segments. It uses the rankings, so its nodes must be judged on held-out data.

## Interpretation

On the synthetic data, 1,200 participants rank three of eight concepts. Two true segments prefer opposite concepts, and a second label means nothing.

- The pooled ranking has a repeatability of 0.94 at 1,200 participants, 0.68 at 200 and 0.31 at 50.
- The true segments pass the fair test. Their repeatability is 0.954 against 0.877 for random segments of the same sizes (p = 0.003), and one model per segment gains 0.15 in held-out log-likelihood per ranking.
- The meaningless label fails it, 0.859 against 0.877 (p = 0.96), with no gain in prediction.
- Splitting has a cost. Random segments (0.877) are less repeatable than the pooled data (0.938), because each holds half the participants. A segmentation must earn more than it costs.
- Single answers stay hard to predict, with a pseudo R2 below 0.2, while the item means are highly repeatable.

## Worked example on open data

Video-based concept testing of eight maize seed product concepts with 2,400 farmers in Kenya and Uganda (Donovan et al. 2025). Each farmer watched three concepts and chose the best and the worst. Replication data from [maize-concept-testing](https://github.com/AgrDataSci/maize-concept-testing), https://doi.org/10.5281/zenodo.15063837, CC BY 4.0. All 2,400 interviews gave a valid ranking. Run with the Python reference on 2026-11-20.

**The unsegmented ranking is highly repeatable.**

| Participants | Repeatability of the item means (95% of subsamples) |
|---|---|
| 50 | 0.72 (0.39 to 0.86) |
| 100 | 0.86 (0.72 to 0.92) |
| 200 | 0.93 (0.89 to 0.95) |
| 400 | 0.97 (0.95 to 0.97) |
| 800 | 0.98 (0.98 to 0.99) |
| 2,400 | 0.99 |

- Two disjoint samples of 150 farmers give log-worths that correlate at 0.92 (0.78 to 0.98).
- Halves made of whole sub-counties, about 600 farmers each and no shared village, correlate at 0.98 in each country (lowest of ten splits 0.97 in Kenya and 0.95 in Uganda).
- Kenya and Uganda correlate at 0.88. Green maize and intercropping score differently in the two countries.
- Single answers are another matter. The model explains 12% of the deviance of one farmer's ranking (pseudo R2 0.116).

**Segments, each against random segments of the same sizes (500 permutations).**

| Data | Segmentation | Smallest segment | Preferences differ (p) | Repeatability, segments | Repeatability, random segments (95%) | More repeatable (p) | Held-out gain per ranking |
|---|---|---|---|---|---|---|---|
| Both | country | 1,200 | 0.002 | 0.989 | 0.988 (0.988 to 0.989) | 0.002 | 0.016 |
| Both | gender | 1,193 | 0.20 | 0.988 | 0.988 (0.988 to 0.989) | 0.53 | 0.000 |
| Both | share of maize sold, 3 classes | 397 | 0.002 | 0.984 | 0.983 (0.981 to 0.983) | 0.002 | 0.007 |
| Kenya | gender | 593 | 0.72 | 0.981 | 0.981 (0.981 to 0.981) | 0.89 | -0.002 |
| Kenya | share of maize sold, 3 classes | 235 | 0.82 | 0.970 | 0.971 (0.969 to 0.973) | 0.89 | -0.012 |
| Kenya | elevation tercile | 400 | 0.26 | 0.972 | 0.972 (0.971 to 0.972) | 0.09 | -0.010 |
| Kenya | sub-county | 125 | 0.58 | 0.944 | 0.943 (0.939 to 0.945) | 0.15 | -0.018 |
| Uganda | gender | 600 | 0.11 | 0.976 | 0.976 (0.975 to 0.976) | 0.014 | -0.001 |
| Uganda | share of maize sold, 3 classes | 59 | 0.16 | 0.946 | 0.963 (0.947 to 0.971) | 0.98 | -0.006 |
| Uganda | elevation tercile | 400 | 0.026 | 0.961 | 0.963 (0.962 to 0.964) | 1.00 | -0.009 |
| Uganda | sub-county | 159 | 0.048 | 0.918 | 0.926 (0.919 to 0.930) | 0.98 | -0.010 |

The held-out gain holds out whole sub-counties, except for the sub-county rows, which use random folds. All sixteen tests are in the output of the example script.

What this example shows.

- **Low repeatability is not supported for the result.** The ranking of the eight concepts is stable from about 100 farmers. Below that it is not, which is the part of the small-sample concern that holds.
- **Single answers are noisy, the result is not.** A reader who looks at how well individual choices are predicted sees little signal. That is a different quantity.
- **Country is the one segmentation that passes.** Preferences differ between Kenya and Uganda and prediction improves. The gain in repeatability is real and tiny, 0.989 against 0.988.
- **Market orientation is confounded with country.** It differs in the pooled data and not within either country. In Uganda 87% of farmers sell half or more of their maize, in Kenya 52% sell none.
- **Elevation and sub-county do not raise repeatability.** Preferences differ a little by elevation and sub-county in Uganda, and the segments are still no more repeatable than random ones, with no gain in prediction.
- **There was no room to gain.** The pooled ranking is already near its ceiling. This data set cannot support the claim that segments raise repeatability, and it does not refute it for data where the pooled signal is weak.

What it does not show.

- **Climate envirotypes were not tested.** Elevation from the GPS reading is the only environmental variable used, and it spans a narrow range (1,170 to 1,793 m in Kenya). Climate could not be fetched in the session that built this example. The coordinates are in the data, and [environment-data](https://github.com/AgrDataSci/environment-data) adds rainfall and temperature from them.
- **The segments are crude.** One variable each. The household clusters built elsewhere for these data were not used here.
- **Sixteen tests, no correction.** The small p for gender in Uganda (0.014) comes with no difference in preference and no gain in prediction. Treat it as chance.
- **Stated preference.** These are rankings of described concepts. A stable ranking does not establish what farmers would buy.

**Against the benchmark concept.** No concept is ranked above the resilient benchmark with confidence in the pooled data. Drought escape reaches 0.51 (0.48 to 0.55) and intercropping 0.49 (0.45 to 0.53). By country, intercropping is above the benchmark in Kenya, 0.56 (0.51 to 0.62), and below it in Uganda, 0.42 (0.38 to 0.49). Drought escape is level with it in Uganda, 0.55 (0.50 to 0.60).

## Limitations

- The R scripts are untested. Run `Rscript tests/run-tests.R` first and report what fails.
- With eight items the variance among items rests on eight values. Read small differences in repeatability with care.
- Variances assume independent participants. Clustering in villages makes the model-based repeatability somewhat optimistic. The split-half between whole sub-counties does not share this weakness.
- The few-item form of repeatability departs from the Cullis form in the code of Dorado-Betancourt et al. (2026) and awaits review by the domain leads.
- Equal-n comparisons of single groups move by chance. Decide with the permutation test.
- The Python Plackett-Luce fit handles complete rankings of three items without ties. Use PlackettLuce in R for anything else.

## References

- Donovan J, Rutsaert P, Mawia H, de Sousa K, van Etten J (2025). Farmers' preferences for the next generation of maize seed products: Application of concept testing in Kenya and Uganda. *Experimental Agriculture* 61, e5. https://doi.org/10.1017/S001447972500002X
- Dorado-Betancourt H, van Eeuwijk F, van Etten J, Ramirez-Villegas J, de Sousa K, Daudi H, Ndegwa M, Mendes T, van Heerwaarden J (2026). Estimating on-farm genotypic performance and variability using ranking data. *Theoretical and Applied Genetics* 139, 231. https://doi.org/10.1007/s00122-026-05319-1
- Eskridge KM, Mumm RF (1992). Choosing plant cultivars based on the probability of outperforming a check. *Theoretical and Applied Genetics* 84, 494-500. https://doi.org/10.1007/BF00229512

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates rankings from two segments with a known answer.
- `script/other-language/repeatability.py` is the Python reference that was run. `run_checks.py` holds its checks and `example-maize-concepts.py` the worked example.
- `tests/` holds the R tests, run with `Rscript tests/run-tests.R`.
