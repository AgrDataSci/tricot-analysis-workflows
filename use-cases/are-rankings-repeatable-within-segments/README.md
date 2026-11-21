# Are rankings repeatable within segments?

**Status** tested-on-synthetic-data. The R tests and `script/01-analysis.R` pass in the continuous integration of the repository on synthetic data. The numbers on open data below come from the Python reference in `script/other-language/`, and the two were not compared number by number.
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
7. **Segments from several variables.** Build segments from household covariates with k-means, never from the rankings, and report how stable the partition is (`build_segments()`). Then give them the fair test. A stable partition is one that can be found again. It is not evidence that the groups are real.
8. **Envirotypes.** Compute annual climate indices per interview point from daily rainfall and temperature (`climate_indices()`), drop the indices that do not vary (`screen_indices()`), group the points (`envirotypes()`) and give the groups the fair test. A concept test has no planting date, so the indices describe the climate of the place over many years.
9. **Similarity between data sets.** When two data sets are compared with a similarity index such as Czekanowski, set it against its noise floor, the similarity of two samples of the same size from one data set (`cross_dataset_similarity()`).
10. **A reference item.** Report the probability that each item is ranked above a benchmark, pooled and per segment (`benchmark_reliability()`).

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

Video-based concept testing of eight maize seed product concepts with 2,400 farmers in Kenya and Uganda (Donovan et al. 2025). Each farmer watched three concepts and chose the best and the worst. Data from [tricot-data](https://github.com/AgrDataSci/tricot-data), files `maize-KEN-6834d470d4cv.json` and `maize-UGA-862ed6d6adfv.json`, CC BY 4.0. They hold the rankings, 30 household variables from the survey of the study and the interview point rounded to two decimals. All 2,400 interviews gave a complete ranking. Run with the Python reference on 2026-11-20, on the files that joined `main` of tricot-data that day (commit 90a79c6).

**The unsegmented ranking is highly repeatable.**

| Participants | Repeatability of the item means (95% of subsamples) |
|---|---|
| 50 | 0.71 (0.34 to 0.86) |
| 100 | 0.86 (0.77 to 0.91) |
| 200 | 0.93 (0.89 to 0.96) |
| 400 | 0.97 (0.96 to 0.97) |
| 800 | 0.98 (0.98 to 0.99) |
| 2,400 | 0.99 |

- Two disjoint samples of 150 farmers give log-worths that correlate at 0.92 (0.80 to 0.98).
- Halves made of whole sub-counties, about 600 farmers each and no shared village, correlate at 0.98 in each country (lowest of ten splits 0.97 in Kenya and 0.95 in Uganda).
- Kenya and Uganda correlate at 0.88. The consumer-oriented (green maize) and intercropping concepts score differently in the two countries.
- Single answers are another matter. The model explains 12% of the deviance of one farmer's ranking (pseudo R2 0.116).

**Segments, each against random segments of the same sizes (500 permutations).**

| Data | Segmentation | Smallest segment | Preferences differ (p) | Repeatability, segments | Repeatability, random segments (95%) | More repeatable (p) | Held-out gain per ranking |
|---|---|---|---|---|---|---|---|
| Both | country | 1,200 | 0.002 | 0.989 | 0.988 (0.988 to 0.989) | 0.002 | 0.016 |
| Both | gender | 1,193 | 0.20 | 0.988 | 0.988 (0.988 to 0.989) | 0.58 | 0.000 |
| Both | share of maize sold, 3 classes | 397 | 0.002 | 0.984 | 0.983 (0.981 to 0.983) | 0.004 | 0.007 |
| Kenya | gender | 593 | 0.73 | 0.981 | 0.981 (0.981 to 0.981) | 0.90 | -0.002 |
| Kenya | share of maize sold, 3 classes | 235 | 0.82 | 0.970 | 0.971 (0.969 to 0.973) | 0.90 | -0.012 |
| Kenya | decides on the variety, yes or no | 85 | 0.44 | 0.981 | 0.981 (0.974 to 0.984) | 0.60 | -0.005 |
| Kenya | takes part in selling, yes or no | 288 | 0.43 | 0.981 | 0.981 (0.979 to 0.982) | 0.70 | -0.005 |
| Kenya | income mostly from the farm, yes or no | 314 | 0.07 | 0.981 | 0.981 (0.979 to 0.982) | 0.65 | 0.000 |
| Kenya | buys seed every year, yes or no | 29 | 0.77 | 0.975 | 0.981 (0.969 to 0.986) | 0.89 | -0.003 |
| Kenya | sole crop or intercrop | 147 | 0.15 | 0.983 | 0.981 (0.977 to 0.984) | 0.13 | -0.002 |
| Kenya | farm size, 3 classes | 212 | 0.68 | 0.971 | 0.972 (0.969 to 0.973) | 0.76 | -0.008 |
| Kenya | found the ranking easy, yes or no | 164 | 0.50 | 0.975 | 0.981 (0.977 to 0.983) | 1.00 | -0.007 |
| Kenya | gender of the presenter in the video | 573 | 0.08 | 0.981 | 0.981 (0.981 to 0.981) | 0.09 | -0.005 |
| Kenya | sub-county | 125 | 0.59 | 0.944 | 0.943 (0.939 to 0.945) | 0.15 | -0.017 |
| Uganda | gender | 600 | 0.10 | 0.976 | 0.975 (0.975 to 0.976) | 0.008 | -0.001 |
| Uganda | share of maize sold, 3 classes | 59 | 0.16 | 0.946 | 0.963 (0.950 to 0.971) | 0.99 | -0.006 |
| Uganda | decides on the variety, yes or no | 77 | 0.07 | 0.981 | 0.975 (0.964 to 0.981) | 0.022 | -0.002 |
| Uganda | takes part in selling, yes or no | 104 | 0.14 | 0.979 | 0.975 (0.966 to 0.980) | 0.13 | -0.002 |
| Uganda | income mostly from the farm, yes or no | 224 | 0.046 | 0.977 | 0.975 (0.971 to 0.978) | 0.26 | 0.002 |
| Uganda | buys seed every year, yes or no | 588 | 0.63 | 0.976 | 0.976 (0.975 to 0.976) | 0.41 | -0.005 |
| Uganda | sole crop or intercrop | 452 | 0.23 | 0.976 | 0.975 (0.974 to 0.976) | 0.19 | -0.003 |
| Uganda | farm size, 3 classes | 319 | 0.006 | 0.965 | 0.963 (0.961 to 0.964) | 0.012 | 0.003 |
| Uganda | found the ranking easy, yes or no | 100 | 0.77 | 0.977 | 0.975 (0.966 to 0.980) | 0.41 | -0.003 |
| Uganda | gender of the presenter in the video | 599 | 0.20 | 0.976 | 0.976 (0.975 to 0.976) | 0.31 | -0.005 |
| Uganda | sub-county | 159 | 0.05 | 0.918 | 0.926 (0.920 to 0.930) | 1.00 | -0.012 |

The held-out gain holds out whole sub-counties, except for the sub-county rows, which use random folds. With these 25 tests and the 15 below the Bonferroni threshold at 0.05 is 0.002, which is also the smallest p that 500 permutations can give.

What this example shows.

- **Low repeatability is not supported for the result.** The ranking of the eight concepts is stable from about 100 farmers. Below that it is not, which is the part of the small-sample concern that holds.
- **Single answers are noisy, the result is not.** A reader who looks at how well individual choices are predicted sees little signal. That is a different quantity.
- **Country is the one segmentation that passes.** Preferences differ between Kenya and Uganda and prediction improves. The gain in repeatability is real and tiny, 0.989 against 0.988.
- **Market orientation is confounded with country.** The share of maize sold differs in the pooled data and not within either country. In Uganda 87% of farmers sold half or more of their maize, in Kenya 52% sold none.
- **Who decides and who sells do not define segments here.** Farmers who decide on the variety, or who take part in selling, do not rank the concepts differently from the others within a country. Few farmers are outside these roles, 85 and 77 for the decision on the variety.
- **Farm size in Uganda is the one lead.** Preferences differ by farm size class (p = 0.006), the segments are slightly more repeatable (p = 0.012) and prediction improves a little. It does not pass the corrected threshold and needs another data set.
- **Finding the task difficult does not change the result.** Farmers who found the ranking neutral or difficult order the concepts as the others do.
- **The design variable behaves.** The gender of the presenter in the video, assigned at random, does not define segments.
- **There was no room to gain.** The pooled ranking is already near its ceiling. This data set cannot support the claim that segments raise repeatability, and it does not refute it for data where the pooled signal is weak.

**Household segments from nine variables do not help either.** Segments were built within each country by k-means on household size, age, farm size, maize area, number of crops, number of kinds of livestock, harvest, share of the harvest sold and share kept for the household. The partitions are stable, which means they can be found again and not that the households fall into distinct types. The groups do not rank the concepts differently.

| Scope | Segments | Stability | Smallest | p, preferences differ | Repeatability, segments | Repeatability, random (95%) | p, more repeatable | Held-out gain |
|---|---|---|---|---|---|---|---|---|
| Kenya | 2 | 0.96 | 513 | 0.60 | 0.981 | 0.981 (0.980 to 0.981) | 0.38 | -0.006 |
| Kenya | 3 | 0.94 | 321 | 0.83 | 0.971 | 0.971 (0.970 to 0.972) | 0.65 | -0.010 |
| Kenya | 4 | 0.84 | 233 | 0.66 | 0.963 | 0.962 (0.960 to 0.963) | 0.21 | -0.014 |
| Uganda | 2 | 0.94 | 474 | 0.004 | 0.976 | 0.976 (0.974 to 0.976) | 0.018 | 0.003 |
| Uganda | 3 | 0.84 | 77 | 0.084 | 0.963 | 0.963 (0.952 to 0.968) | 0.57 | -0.007 |
| Uganda | 4 | 0.89 | 51 | 0.52 | 0.945 | 0.951 (0.937 to 0.957) | 0.89 | -0.013 |

In Kenya the three types are households that keep their maize, smaller farms that sell part of it and larger farms with more crops. In Uganda most farmers sell most of their maize and the types separate by farm size, about 3 against 14 units of land for the two segments. That split in Uganda is the farm size lead above seen again, with the same small effect, and it does not pass the corrected threshold.

**Envirotypes from climate.** Daily rainfall (CHIRPS) and temperature (CHIRTS) for 2007 to 2021 at the 388 interview points, with no missing days. Ten annual indices per point, with wet days at 1 mm, heavy rain at 20 mm, hot days at Tmax of 30 °C or more and warm nights at Tmin of 20 °C or more. The thresholds vary across the points in both countries, so none was changed. Mean Tmax is dropped in Uganda, where it hardly varies. The Kenyan points are wetter, 2,130 mm against 1,310 mm a year at the median, and have more hot days, 179 against 59.

| Scope | Envirotypes | Stability | Smallest | p, preferences differ | Repeatability, envirotypes | Repeatability, random (95%) | p, more repeatable | Held-out gain |
|---|---|---|---|---|---|---|---|---|
| Kenya | 2 | 0.99 | 437 | 0.17 | 0.982 | 0.981 (0.980 to 0.982) | 0.088 | 0.000 |
| Kenya | 3 | 0.81 | 324 | 0.19 | 0.972 | 0.972 (0.970 to 0.973) | 0.22 | -0.009 |
| Kenya | 4 | 0.80 | 125 | 0.27 | 0.964 | 0.962 (0.958 to 0.964) | 0.064 | -0.013 |
| Uganda | 2 | 0.99 | 479 | 0.002 | 0.972 | 0.975 (0.974 to 0.976) | 1.00 | 0.007 |
| Uganda | 3 | 0.83 | 243 | 0.028 | 0.963 | 0.963 (0.959 to 0.965) | 0.71 | -0.018 |
| Uganda | 4 | 0.90 | 174 | 0.006 | 0.948 | 0.951 (0.946 to 0.954) | 0.91 | -0.030 |
| Both | 2 | 0.99 | 1,025 | 0.002 | 0.989 | 0.988 (0.988 to 0.989) | 0.002 | 0.014 |
| Both | 3 | 0.95 | 510 | 0.002 | 0.984 | 0.983 (0.982 to 0.983) | 0.002 | 0.013 |
| Both | 4 | 0.95 | 476 | 0.002 | 0.976 | 0.977 (0.976 to 0.978) | 0.97 | 0.016 |

- **In the pooled data the envirotype is the country.** With two envirotypes all 1,200 Ugandan farmers and 175 Kenyan farmers fall in the drier one. It passes because country passes.
- **Kenya shows no envirotype effect.** The two envirotypes are very stable and do not rank the concepts differently.
- **In Uganda preferences differ between envirotypes, and the envirotype is the district.** The wetter envirotype with warmer nights holds 479 farmers, all in Mityana. The other holds all 601 of Mubende and 120 of Mityana. The drier envirotype separates the concepts more, with intercropping, resilience and drought escape further ahead. The split improves prediction on held-out sub-counties a little (0.007 per ranking). It does not make the ranking more repeatable than random groups, because the wetter group ranks the concepts closer together.
- **Climate and place cannot be separated with two districts.** Within Mityana the two envirotypes do not differ in preference (p = 0.09, from a check outside the scripts). A design that separates them needs more districts along the climate gradient.
- **The 175 Kenyan farmers in the climate group of Uganda are in between.** Their similarity to Uganda is 0.87 against 0.85 for random Kenyan groups of that size (p = 0.21), and their similarity to the other Kenyans is 0.88 against 0.92 (p = 0.08). The direction fits a climate effect and the group is too small to tell.

**A low Czekanowski similarity between data sets is mostly sample size.** The Czekanowski similarity of the worth shares of Kenya and Uganda is 0.86 with all farmers. The table gives it for smaller samples, next to the similarity of two samples from the same country.

| Participants per sample | Kenya against Uganda (95% of draws) | Two samples from Kenya | Two samples from Uganda | Share of the dissimilarity that is noise |
|---|---|---|---|---|
| 50 | 0.77 (0.58 to 0.90) | 0.80 | 0.81 | 0.85 |
| 100 | 0.82 (0.69 to 0.90) | 0.86 | 0.86 | 0.75 |
| 200 | 0.83 (0.73 to 0.91) | 0.90 | 0.90 | 0.58 |
| 400 | 0.86 (0.80 to 0.91) | 0.93 | 0.93 | 0.47 |
| 600 | 0.86 (0.82 to 0.89) | 0.94 | 0.95 | 0.39 |

Two samples of 100 farmers from the same country reach only 0.86, so the index has no fixed ceiling to be judged against. At 50 per sample the two countries cannot be told apart from two samples of one country, and 85% of their dissimilarity is sampling noise. At 600 the difference between the countries is clear in 99.7% of draws. A low similarity between data sets therefore separates into a part that any two samples would show and a part that is real difference between the populations. The second part is what the fair test of segments then tries to explain.

What it does not show.

- **A climate effect apart from place.** See above. Interviews of a country lie within about half a degree.

- **Forty tests.** The small p values for gender and for the decision on the variety in Uganda come with no difference in preference at the corrected level and no gain in prediction. Treat them as chance.
- **Stated preference.** These are rankings of described concepts. A stable ranking does not establish what farmers would buy.

**Against the benchmark concept.** No concept is ranked above the resilient benchmark with confidence in the pooled data. Drought escape reaches 0.51 (0.48 to 0.55) and intercropping 0.49 (0.45 to 0.53). By country, intercropping is above the benchmark in Kenya, 0.56 (0.51 to 0.62), and below it in Uganda, 0.42 (0.38 to 0.48). Drought escape is level with it in Uganda, 0.55 (0.50 to 0.60).

## Second worked example on open data

Video-based concept testing of seven cassava product concepts with 398 farmers in the departments of Bolívar, Córdoba and Sucre in the Caribbean region of Colombia (CIAT with CIRAD as partner, November and December 2024). Data from [tricot-data](https://github.com/AgrDataSci/tricot-data), file `cassava-COL-26ad22f40f6v.json`, CC0 1.0, on `main` at commit 32982e7. 396 farmers gave a complete ranking. The file holds the kind of cassava each farmer grows, sweet for the fresh market, industrial for starch, or both. Daily rainfall (CHIRPS) and temperature (CHIRTS) at the 185 farm or interview points, 2007 to 2021 as in the maize example, with the same thresholds. Run with the Python reference on 2026-11-20, 500 permutations.

**The unsegmented ranking is highly repeatable here too.** Repeatability is 0.84 (0.74 to 0.91) with 50 farmers, 0.93 (0.89 to 0.95) with 100, 0.96 with 200 and 0.98 with all 396. The pseudo R2 of single answers is 0.18.

**Market type is a real segment.** The 342 farmers who grow only sweet cassava and the 54 who grow industrial cassava or both order the concepts almost in reverse.

| Concept | Pooled | Sweet only | Industrial or both |
|---|---|---|---|
| La rendidora | 1.13 | 1.39 | 0.21 |
| Rapidita | 0.74 | 0.95 | -0.37 |
| La Duradera | 0.58 | 0.71 | -0.23 |
| Doble Propósito | -0.03 | -0.19 | 0.80 |
| Prematura | -0.23 | -0.39 | 0.48 |
| La Súper industrial | -0.97 | -1.23 | 0.47 |
| La Amarilla | -1.21 | -1.24 | -1.36 |

The Czekanowski similarity of the two markets is 0.52, against 0.87 (0.76 to 0.95) for random splits of the same sizes. This low similarity is real difference, not sample size.

| Scope | Segmentation | Smallest | p, preferences differ | Repeatability, segments | Repeatability, random (95%) | p, more repeatable | Held-out gain |
|---|---|---|---|---|---|---|---|
| All | market type | 54 | 0.002 | 0.958 | 0.961 (0.948 to 0.968) | 0.81 | 0.050 |
| All | department | 78 | 0.002 | 0.933 | 0.943 (0.936 to 0.947) | 0.99 | 0.017 |
| All | gender | 55 | 0.99 | 0.963 | 0.962 (0.948 to 0.968) | 0.54 | -0.019 |
| All | age, 2 classes | 189 | 0.95 | 0.962 | 0.962 (0.960 to 0.963) | 0.54 | -0.012 |
| All | experience, 2 classes | 168 | 0.39 | 0.962 | 0.962 (0.959 to 0.964) | 0.44 | -0.009 |
| All | sweet cassava area, 2 classes | 161 | 0.026 | 0.961 | 0.963 (0.961 to 0.965) | 0.99 | 0.002 |
| All | envirotypes, 2 | 112 | 0.002 | 0.966 | 0.962 (0.956 to 0.965) | 0.002 | 0.016 |
| All | envirotypes, 3 | 75 | 0.002 | 0.948 | 0.943 (0.934 to 0.949) | 0.058 | -0.035 |
| All | envirotypes, 4 | 74 | 0.004 | 0.925 | 0.924 (0.916 to 0.929) | 0.37 | -0.027 |
| Sweet only | envirotypes, 2 | 111 | 0.024 | 0.968 | 0.966 (0.963 to 0.968) | 0.050 | 0.006 |
| Sweet only | envirotypes, 3 | 65 | 0.012 | 0.953 | 0.949 (0.942 to 0.954) | 0.052 | -0.002 |
| Sweet only | envirotypes, 4 | 65 | 0.010 | 0.935 | 0.932 (0.927 to 0.936) | 0.12 | -0.002 |
| Sweet only | department | 50 | 0.34 | 0.941 | 0.949 (0.942 to 0.953) | 0.98 | -0.018 |
| Drier envirotype | market type | 53 | 0.002 | 0.938 | 0.938 (0.920 to 0.947) | 0.59 | 0.051 |

The held-out gain holds out whole municipalities, except for department, which uses random folds. With 14 tests the Bonferroni threshold at 0.05 is 0.004.

- **Market type gives the largest gain in prediction of any segmentation in the two examples**, 0.050 per ranking against 0.016 for country in the maize data. It does not raise repeatability, because the pooled ranking is 86% sweet growers and so is already their ranking, and the industrial group is small.
- **Two envirotypes pass the fair test.** They differ in preference, they are more repeatable than random groups of the same sizes (p = 0.002) and they predict held-out municipalities better. This is the only segmentation in the two examples that is more repeatable than chance, apart from country.
- **The envirotypes are a dry and a wet one.** The drier holds 284 farmers, with a longest dry spell of 42 days, 1,510 mm of rain a year and 220 hot days. The wetter holds 112 farmers, nearly all in Bolívar, with a dry spell of 20 days, 1,780 mm and 109 hot days. In the wetter one La rendidora leads by far (1.76) and La Súper industrial is rejected (-1.87). In the drier one the worths lie closer and the early concept Prematura is near the middle (-0.06 against -0.68).
- **Climate is partly the market.** All but one of the industrial growers are in the drier envirotype. Among sweet growers only, the envirotypes still differ in preference (p = 0.024) and are at the edge of being more repeatable (p = 0.050), with a small gain in prediction. That does not pass the corrected threshold.
- **Climate does better than the department.** Among sweet growers the three departments do not differ in preference (p = 0.34), while the envirotypes, which cut across Bolívar, do.
- **Market type holds within the drier envirotype** (p = 0.002), so the market split is not climate under another name.

What the two examples say together. The pooled ranking of concepts is repeatable from about 100 farmers on. Segments matter where markets differ, and what they change is which product fits which farmer, not how stable the result is. Repeatability is the wrong yardstick for a segmentation. Whether preferences differ and whether prediction on held-out places improves are the right ones. Climate envirotypes show their first sign of a gain in repeatability in Colombia, where the points span a real dry to wet gradient, and none in Kenya and Uganda, where the points of a country lie within half a degree.

Limits of this example. One survey of 396 farmers and one region. The industrial segment has 54 farmers. Temperature indices are strongly correlated with each other, so the envirotypes are close to a single dry and hot against wet and cool axis. The reasons farmers gave are in the file in English translation and were not analysed here.

## Third worked example, on-farm variety trials

tricot trials of 30 cassava varieties with 448 farmers in Akwa Ibom, Benue, Imo and Osun, Nigeria, over two seasons (NextGen Cassava, IITA and NRCRI). Each farmer grew three varieties and followed them from planting through harvest, gari processing and eba. The outcome is the choice of the best and the worst variety to replant, with 480 valid answers. The data are not yet in [tricot-data](https://github.com/AgrDataSci/tricot-data), so this example cannot be rerun from open files. The analysis lives in AgrDataSci/nextgen-oft-nga (`script/06-workflow-case-study`), private until the paper is out. Run with the Python reference on 2026-11-21, 200 permutations.

This is the case the two concept tests could not show. There the pooled ranking was near its ceiling. Here it carries no signal at all, with 30 varieties, each seen by about 48 farmers.

**The pooled ranking does not repeat at any sample size, and one segment does.** Segments are processing roles, defined from who processed the roots of the trial and never from the rankings.

| Participants | All | Women farmer-processors | Rely on other processors |
|---|---|---|---|
| about 50 | 0.02 | 0.03 | 0.02 |
| 100 | 0.03 | 0.11 | 0.04 |
| 150 | | 0.21 | 0.05 |
| 200 | 0.04 | 0.29 (0.18 to 0.42) | 0.02 |
| all | 0.00 (480) | 0.32 (215) | 0.00 (217) |

Men farmer-processors gave 46 answers, with a repeatability of 0. Single answers are barely predictable, with a pseudo R2 of 0.012 pooled and 0.068 with one model per role.

**The roles pass the fair test.** Preferences differ (p = 0.005) and the roles are more repeatable than random segments of the same sizes, 0.14 against 0.02 (p = 0.014 with 500 permutations). For the final overall evaluation, the most recent overall question, both large roles repeat (0.32 and 0.27) while the pooled ranking stays at 0, and the roles reach 0.26 against 0.05 for random segments (p = 0.004). The roles share none of their three preferred varieties.

**Location takes part of it.** The roles cluster by state (Cramér's V 0.38). The fair test shuffles labels among all participants, so its random segments also lose the state structure. With labels shuffled within states the roles are still about twice as repeatable as random groups, 0.14 against 0.07 (p = 0.15) for the choice to replant and 0.26 against 0.11 (p = 0.04) for the final overall evaluation. Imo alone reaches 0.51 with 140 farmers and holds half of the women farmer-processors. Outside Imo they reach 0.34 with 105. No segmentation, state included, predicted the rankings of held-out local government areas better than the pooled model.

**Against the check.** Among women farmer-processors the two varieties released from these trials, Baba 70 and Obasanjo 2, are ranked above the check TMEB419 with probability 0.76 (0.57 to 0.91 and 0.53 to 0.92). Among farmers who rely on other processors no variety beats the check with confidence.

What this example adds.

- **Segments can raise repeatability from nothing.** When segments rank the items in opposite orders, the pooled ranking cancels out and repeatability is the yardstick that shows it. The statement above, that repeatability is the wrong yardstick for a segmentation, holds where the pooled ranking is already stable.
- **The curve gives a number per segment.** About 200 answers from women farmer-processors give a repeatability near 0.3 with 30 varieties. More answers from farmers who rely on other processors do not help.
- **Shuffle within sites when the segment is confounded with site.** The fair test of step 4 does not do this. It was run here outside the functions (`fair_test_within()` in the script of the study) and is a candidate for `segment_test()`.

Limits of this example. The data are not open yet. Roles are confounded with state and the within-state evidence is at the edge. 192 farmers took part in both seasons and the permutations shuffle answers, not people. With 30 items and three per farmer the variances are large, which is why a repeatability of 0.3 is the best seen.

## Limitations

- The R tests pass in the continuous integration of the repository. Every number in this README comes from the Python reference, and the R and Python results were not compared number by number.
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
- `script/other-language/repeatability.py` is the Python reference that was run. `run_checks.py` holds its checks and `example-maize-concepts.py` the worked example, which reads the maize concept tests from tricot-data.
- `script/helper/envirotypes.R` and `script/other-language/envirotypes.py` hold the climate indices and the envirotype step. `example-cassava-concepts.py` is the second worked example and takes the tricot-data folder and the climate file. `example-maize-envirotypes.py` runs the envirotype step on the maize points and takes the tricot-data folder, the rainfall file and the temperature file as arguments. The climate files used here hold both and are not in this repository.
- `tests/` holds the R tests, run with `Rscript tests/run-tests.R`.
