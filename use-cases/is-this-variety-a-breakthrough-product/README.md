# Is this variety a breakthrough product?

**Status** reviewed-not-executed. The R scripts are a draft and have not been run. The Python reference in `script/other-language/` was run on synthetic data and on open data, and gave every number below.
**Decision record** [breakthrough products](../../decisions/breakthrough-product.md), a draft awaiting review
**Domain leads to consult** Joost van Heerwaarden and Hugo Dorado-Betancourt (Wageningen University)
**Derived from** Eskridge and Mumm (1992) for the decision rule and Dorado-Betancourt et al. (2026) for the two-step estimation. No archived script.

## Scientific question

Does a variety give at least 25% more yield than the check in farmers' fields, how sure are we, and on what share of farms does it do so?

## Intended use

Use it to screen on-farm trial data for candidate breakthrough products, defined by Cairns et al. (2026) as varieties with at least 25% higher productivity or value in farmers' fields. It gives the on-farm side of the evidence.

It does not make the decision. A breakthrough decision is generally taken with station data, which maniva does not connect yet. In tricot-data only the five groundnut trials from Tanzania have on-station companions, and those files are not in the release. See the use case [do on-farm and on-station results agree](../do-on-farm-and-on-station-results-agree/) for comparing the two sources.

## Input data

One row per plot.

| Column | Content |
|---|---|
| `trial` | trial id |
| `farm` | farm id, the environment |
| `genotype` | variety name, harmonized across trials |
| `yield` | measured yield, in one unit within a farm. Zero for a failed plot |
| `rank` | optional, the farmer's overall ranking of the plot, 1 is best |

Name the check. When the metadata list several checks, run each. From tricot-data, use `trait == "yield"` with `value_type == "numeric"` and `value_standard`, and use the most recent overall-type ranking as `rank`, as the [ranking decision record](../../decisions/ranking.md) asks.

## Assumptions

- Plots on one farm share the farm effect, so their yield ratio is free of it.
- Yield ratios are stable on the log scale, with one residual variance for all varieties and trials.
- The farms sample the environments where the check is normally grown.
- Farms are independent.

## Approach

1. **Step 1, per trial.** Fit log yield on variety and farm, and take the contrast of each variety to the check with its standard error. Trials without the check are left out and named.
2. **Step 2, across trials.** Combine the contrasts with weights from their variances and a variety-by-trial variance (DerSimonian-Laird). The result is the mean gain with a 95% interval.
3. **Confidence in the mean.** Compute the probability that the mean gain reaches the threshold (`p_mean_gain`).
4. **Reliability.** Compute the probability that the variety beats the check on a farm (`rel`), and that it does so by the threshold (`rel_threshold`), from the mean gain and the standard deviation of the differences. Resample farms within trials for the interval.
5. **Direct cross-check.** On farms that grew the variety and the check, count the wins and the wins by the threshold, with zero yields kept (`n_pairs`, `wins`, `wins_threshold`).
6. **Failed plots.** Report the share of zero yields per variety (`share_failed`).
7. **Value.** Fit a Plackett-Luce model on the overall ranking and report the probability that farmers rank the variety above the check (`p_preferred`).
8. **Verdict.** Supported, indicative, not supported or insufficient data, with provisional cut-offs set as arguments.

Steps 1 and 2 follow the two-step logic of Dorado-Betancourt et al. (2026), which they built for rankings. Using it for measured yield is our adaptation. Step 4 is the reliability of Eskridge and Mumm (1992). Moving its threshold from zero to 25% is our extension.

## Alternatives

- One mixed model on all plots, when trials share most varieties.
- A Bayesian probability of outperforming a check or a threshold, as in the ProbBreed R package. It is outside maniva.
- Arithmetic yield ratios with zeros kept, for a question about total production.

## Interpretation

On the synthetic data (6 trials, 900 farms, 2,700 plots of which 122 failed), Variety 01 has a simulated gain of 40%. The workflow estimates 1.37 (95% interval 1.26 to 1.49), with probability 0.98 that the mean gain reaches 25%, and calls it supported. Variety 02 sits on the threshold (simulated 1.25, estimated 1.20, 1.12 to 1.29) and is not supported. The five other varieties are not supported.

The reliability tells a different story from the mean. Variety 01 beats the check on 74% of farms and reaches the 25% gain on 57% (interval 0.53 to 0.63, simulated value 0.59). The direct count agrees, 54% of 97 farms. A breakthrough product on average still misses the threshold on four farms in ten here. Report both numbers and keep them apart.

On preference, farmers rank Variety 01 above the check with probability 0.61 (316 rankings).

## Worked example on open data

CIAT common bean tricot trials in Tanzania that started in 2021 or 2022, from tricot-data. Fifteen trials match, ten hold measured yield (1,583 plots, 269 with zero yield) and eight a post-harvest overall ranking. Run with the Python reference on 2026-11-20, tricot-data commit e69e173.

Check Uyole 03, labelled a historical check in the metadata.

| Variety | Trials started | Trials used | Yield ratio (95% interval) | P(mean gain ≥ 25%) | Reliability at 25% (interval) | Plots | Direct pairs, wins by 25% | Verdict |
|---|---|---|---|---|---|---|---|---|
| TARIBEAN 3 | 2021 | 2 | 1.51 (1.02 to 2.25) | 0.83 | 0.59 (0.43 to 0.79) | 38 | 3, 2 | supported |
| TARIBEAN 3 | 2022 | 6 | 1.87 (1.11 to 3.15) | 0.94 | 0.69 (0.51 to 0.80) | 42 | 10, 7 | supported |
| TARIBEAN 3 | both | 8 | 1.68 (1.22 to 2.30) | 0.97 | 0.65 (0.49 to 0.77) | 80 | 13, 9 | supported |
| TARIBEAN 1 | 2021 | 2 | 1.30 (0.87 to 1.94) | 0.58 | 0.52 (0.31 to 0.70) | 35 | 4, 2 | indicative |
| TARIBEAN 1 | 2022 | 6 | 1.91 (0.96 to 3.81) | 0.89 | 0.67 (0.49 to 0.79) | 55 | 27, 16 | supported |
| TARIBEAN 1 | both | 8 | 1.69 (1.07 to 2.67) | 0.90 | 0.63 (0.50 to 0.72) | 90 | 31, 18 | supported |

Check Calima Uyole, labelled the comparative check, trials of both years (10 trials).

| Variety | Yield ratio (95% interval) | P(mean gain ≥ 25%) | Reliability at 25% | Plots | Direct pairs, wins by 25% | Verdict |
|---|---|---|---|---|---|---|
| TARIBEAN 1 | 1.37 (0.93 to 2.02) | 0.68 | 0.54 | 105 | 13, 9 | indicative |
| TARIBEAN 3 | 1.36 (1.01 to 1.84) | 0.71 | 0.54 | 93 | 10, 6 | indicative |

Farmers rank TARIBEAN 3 above Uyole 03 with probability 0.62 (90 rankings) and TARIBEAN 1 with probability 0.57 (105 rankings), both years together. The Python reference gives no interval for these.

What this example shows, and what it does not.

- **The check decides the verdict.** Both varieties are supported against Uyole 03 and only indicative against Calima Uyole.
- **On-farm data only.** These trials have no station data in maniva. The example cannot stand for a breakthrough decision, which is generally taken with station data.
- **Few direct pairs.** Only TARIBEAN 1 has more than 16 farms beside Uyole 03. The reliabilities rest on the model and on one residual variance for all varieties.
- **Failed plots.** 17% of all plots yielded zero and left the log model. The share is 16% for TARIBEAN 1, 12% for TARIBEAN 3 and 22% for Uyole 03 across both years.
- **Assumed plot areas.** 504 of 1,583 yields rest on an assumed plot area. The ratio within a farm is safe only if its three plots had the same size.
- **Wide 2022 intervals.** The gain differs between the 2022 trials, and the variety-by-trial variance widens the interval.

## Limitations

- The R scripts are untested. Run `Rscript tests/run-tests.R` first and report what fails.
- On-farm data only, as above.
- One residual variance for all varieties. The model-based reliability cannot show that one variety is less stable than another. Eskridge and Mumm estimate a standard deviation per variety, which needs many direct pairs.
- With one trial the variety-by-trial variance is set to zero and the interval is too narrow. Check `n_trials`.
- A trial enters step 1 only if it grew the check. Evidence that links a variety to the check through other varieties in other trials is not used.
- The verdict cut-offs (0.8 and 30 plots) are provisional.
- The Python Plackett-Luce fit handles complete rankings of three plots without ties. Use PlackettLuce in R for anything else.
- Worth and preference are relative to the varieties tested. A yield gain is not a gain in value, adoption or profit.

## References

- Cairns JE, Fisher M, Pinto F, de Sousa K, van Etten J, Zaman-Allah M, Daudi H, Nas TM, Werner CR (2026). Advancing inclusive on-farm testing approaches to facilitate maize breakthrough product development in sub-Saharan Africa. *Journal of Experimental Botany* erag431. https://doi.org/10.1093/jxb/erag431
- Dorado-Betancourt H, van Eeuwijk F, van Etten J, Ramirez-Villegas J, de Sousa K, Daudi H, Ndegwa M, Mendes T, van Heerwaarden J (2026). Estimating on-farm genotypic performance and variability using ranking data. *Theoretical and Applied Genetics* 139, 231. https://doi.org/10.1007/s00122-026-05319-1
- Eskridge KM, Mumm RF (1992). Choosing plant cultivars based on the probability of outperforming a check. *Theoretical and Applied Genetics* 84, 494-500. https://doi.org/10.1007/BF00229512

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates on-farm trials with a known breakthrough product.
- `script/other-language/breakthrough.py` is the Python reference that was run. `run_checks.py` holds its checks and `example-beans-tanzania.py` the worked example.
- `tests/` holds the R tests, run with `Rscript tests/run-tests.R`.
