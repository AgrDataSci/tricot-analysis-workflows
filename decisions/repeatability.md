# Repeatability of rankings

Status: draft rationale prepared with Kauê de Sousa (Bioversity International), 2026-11-20. Not yet reviewed by him or by the domain leads. This is not independent validation of cited studies or software.

**Domain leads** Joost van Heerwaarden and Hugo Dorado-Betancourt (Wageningen University) lead genetic parameters from rankings. Ask them to review this record before its use case moves past a draft.

## The question

Two claims meet here. One says that results from ranking-based testing have low repeatability, are sensitive to small samples, and would improve with absolute scores. The other says that unsegmented data should not be expected to repeat, and that repeatability is higher within market segments and envirotypes. Both are claims about data, so both are tested, not argued.

## Default approach

- **Say which repeatability.** Three things carry the name. The repeatability of the item means asks whether a new sample of participants would order the items the same way. The predictability of a single answer asks whether the model can tell what one participant will choose. Agreement across places asks whether two regions order the items the same way. They give very different numbers on the same data, so name the one reported. The first is the one a breeding or product decision rests on.
- **Repeatability of the item means.** Follow the two steps of Dorado-Betancourt et al. (2026). Step 1 gives each item a Plackett-Luce log-worth and a variance. Step 2 treats the items as a random effect observed with those variances and estimates the variance among items. The repeatability is the share of the variance among estimated item means that is real difference between items and not estimation error.
- **Few items.** Their code uses the Cullis form, where the prediction error also counts the uncertainty of the overall mean. With k items that puts a ceiling near 1 - 1/k on the result, 0.875 for eight concepts however large the sample. With few items, as in concept testing, use the mean of sigma2 / (sigma2 + variance) on centred log-worths. With many genotypes the two agree. This departure from their code is ours and awaits their review.
- **Split-half as the plain check.** Draw two disjoint samples of participants, fit each, and correlate the log-worths. Eskridge and Mumm (1992) judged statistics the same way, by the agreement between random halves of the environments. Halves made of whole villages or districts test agreement across places.
- **Repeatability depends on n.** Report it as a curve against the number of participants, never as one number. The curve is also the sample-size guidance for the next study. The use case `how-many-participants-do-we-need` plans a trial from the same idea.
- **Segments are built without the rankings.** Market segments from socioeconomic covariates and envirotypes from the climate, soil and elevation of each location, as [socioeconomic heterogeneity](socioeconomic-heterogeneity.md) and [climate and environment](climate-environment.md) require. Coordinates are enough to add climate from public sources through [environment-data](https://github.com/AgrDataSci/environment-data).
- **The fair test.** A segment holds fewer participants than the pooled data, and a smaller sample is less repeatable whatever the segment means. So compare the segments with random segments of the same sizes, by shuffling the labels. Two statistics, the gain in fit from one model per segment (do preferences differ) and the mean repeatability of the segments (are they more repeatable). The second is the claim about segments. A segmentation can pass the first and fail the second.
- **Similarity between data sets.** An index such as Czekanowski compares the worth shares of two data sets. Two samples from one population do not reach one, and the smaller they are the lower they fall. Report the index with its noise floor at the same n, and read only the shortfall below the floor as difference between populations.
- **Equal n, as description.** Also report the pooled data and each segment at the same number of participants. Single groups move by chance at equal n, in the synthetic data from 0.46 to 0.78 around a pooled 0.66 for a label that means nothing. The table describes and the permutation test decides.
- **Prediction on held-out places.** Report the gain in held-out log-likelihood from one model per segment, with whole villages or districts held out. Keep a segmentation only when it predicts rankings better than the pooled model, as the socioeconomic record already asks.
- **Segments found by a tree.** A Plackett-Luce tree uses the rankings to find its split. Its nodes will look more repeatable than random groups by construction. Judge them on held-out data only.
- **A reference item.** Report the probability that each item is ranked above a benchmark, the reliability of Eskridge and Mumm (1992) as `gosset::reliability()` computes it, pooled and per segment. This gives the stable anchor that a call for benchmarks asks for, without leaving the ranking format.
- **Ratings are not the fix.** Scores on a short scale are not comparable between participants, and the default analysis keeps only their order within a participant (see "Ratings to rankings" in [data synthesis](data-synthesis.md)). Adding scores adds a second instrument to validate. Whether it adds repeatability is an empirical question that needs both formats collected from the same participants.

## Claims and where maniva answers them

| Claim | Answer |
|---|---|
| Rankings have low repeatability | Measure it for the item means, as a curve against n. Use case `are-rankings-repeatable-within-segments` |
| Results are sensitive to small samples | True below a size the curve shows. Use cases `are-rankings-repeatable-within-segments` and `how-many-participants-do-we-need` |
| Rankings are relative and lack context | Context comes from covariates joined to each participant, climate from coordinates and socioeconomic data. Records [climate and environment](climate-environment.md) and [socioeconomic heterogeneity](socioeconomic-heterogeneity.md) |
| Add absolute scoring | Scores are not comparable between participants. Use case `can-farmer-ratings-be-analysed-as-rankings` |
| Use benchmarks or reference concepts | Reliability against a reference item, this record |
| Similarity between data sets is low (Czekanowski or another index) | Set it against the similarity of two samples of the same size from one data set. The shortfall below that floor is the real difference, and segments or envirotypes are then tested as its explanation |
| Repeatability is higher within segments | The fair test in this record. It can fail |
| Use choice experiments where price drives choice | An alternative in [consumer and concept testing](consumer-concept-testing.md) |

## Alternatives

- The Cullis heritability of Dorado-Betancourt et al. (2026), with many items and environments.
- Thurstonian models in place of Plackett-Luce, as in their code. On a latent normal scale they connect directly to the normal estimator of Eskridge and Mumm (1992).
- A factor-analytic mixed model across environments, when the question is the structure of genotype by environment interaction.
- Test and retest of the same participants, when the question is whether a person answers the same way twice. No maniva data set holds it.

## What the results cannot support

- High repeatability is not validity. A stable ranking of stated preferences about a description does not establish adoption or purchase. Mawia et al. (2025) show farmers choosing the market leader in a shop whatever they say they prefer.
- Low predictability of single answers is not low repeatability of the result. Most of what one participant chooses is not explained by any model, and the item means can still be stable.
- A segmentation that fails the test in one data set can matter in another. The test is about the segments and the items at hand.
- A result that is near its ceiling in the pooled data cannot show a gain from segments. Test the claim about segments where the pooled repeatability is low.
- Variances assume independent participants. Participants of one village are not independent, so repeatability from the model is somewhat optimistic. The split-half between whole villages or districts does not share this weakness.
- Many segmentations tested on one data set produce small p-values by chance. Correct for it or treat a lone small p as a lead, not a finding.

## Open questions

- Whether the few-item form of repeatability is acceptable to the domain leads, or whether a different statistic should be the default for concept tests.
- Envirotypes from climate. In the first worked example the envirotype coincides with the country in the pooled data and with the district in Uganda, so climate and place are not separated. A data set with many districts along a climate gradient is needed.
- Whether annual long-term indices are the right description of climate for a concept test, and which thresholds to use for hot days and warm nights in the highlands.
- The on-farm yield data where the unsegmented ranking shows little signal. That is where the claim about segments has room to show a gain, and those data are not open.
- Ratings and rankings from the same participants, to settle the claim about absolute scoring with data.

## Use case

- `use-cases/are-rankings-repeatable-within-segments/`

## Source pointers

- [Dorado-Betancourt et al. 2026, TAG](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/dorado-2026-tag.md)
- [Eskridge and Mumm 1992, TAG](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/eskridge-1992-tag.md), card proposed on the same date as this record
- [Donovan et al. 2025, Experimental Agriculture](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/donovan-2025-expagric.md)
- [Mawia et al. 2025, Frontiers in Sustainable Food Systems](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/mawia-2025-fsufs.md)
- [van Etten et al. 2019, PNAS](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/vanetten-2019-pnas.md)
