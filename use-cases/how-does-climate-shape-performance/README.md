# How do climate and environment shape variety performance?

**Status** tested-on-synthetic-data
**Decision record** [climate and environment](../../decisions/climate-environment.md)
**Derived from** [archive/crop-variety-management-pnas](../../archive/crop-variety-management-pnas/), [archive/etnam-farmer-ratings](../../archive/etnam-farmer-ratings/), [archive/osiris-1000farms](../../archive/osiris-1000farms/) `02-get-climate-data.R`, [archive/worldveg-choose-growth-thrive](../../archive/worldveg-choose-growth-thrive/) `03-get-climate-covar.R`

## Scientific question

Does the climate a plot experienced during a key crop stage change which varieties participants prefer, and to which target sites can the result be applied?

## Intended use

Use it to identify climate conditions under which variety recommendations should differ, and to check whether a new target site falls within the conditions the trial actually covered.

It does not establish that climate causes the difference. Climate is confounded with soils, management and who the farmers are.

## Input data

- tricot rankings with a planting date per plot.
- Daily maximum and minimum temperature per plot as an array `[plot, day, 2]`, and daily rainfall as a matrix `[plot, day]`, with dates (YYYY-MM-DD) as names. With real data these come from documented sources, for example CHIRPS through the chirps package and NASA POWER or AgERA5 through climatrends.
- Phenology for the crop and varieties, the base temperature and the degree days to the target stage, with its source.

Record the climate source, its spatial and temporal resolution, and the download date.

## Assumptions

- Phenology parameters come from a documented source. `phenology_window()` has no defaults for them and stops if they are missing, because a guessed window can create or hide an effect.
- Daily climate covers the full window of every plot.

## Approach

1. Estimate the window from planting to flowering per plot with `climatrends::GDD(..., return.as = "ndays")`.
2. Compute maxDT, DTR and SU (`climatrends::temperature`) and Rtotal (`climatrends::rainfall`) over that window.
3. Fit a Plackett-Luce tree on the indices (`PlackettLuce::pltree`, at least 100 rankings per node). The script uses alpha 0.01, the conservative choice. With real data use 0.05 or 0.10 and keep 0.01 for data sets above 5,000 participants or a demanding review ([climate and environment](../../decisions/climate-environment.md)).
4. Refit with fixed 30-day and 90-day windows to show how the conclusion depends on the window.
5. Flag target sites outside the observed range of any index, or with a Mahalanobis distance beyond the 99% chi-squared quantile, which catches combinations never observed even when each index is in range.

## A note for climatrends

When the degree-day threshold is not reached within the span, `GDD(..., return.as = "ndays")` returns the full span length with no warning. A plot that never flowers would then silently get the longest window. `phenology_window()` sets those windows to NA and warns. It may be worth adding the same check to climatrends itself.

## Alternatives

- Fixed windows when phenology is unknown, reported as a sensitivity analysis.
- Plackett-Luce models with continuous covariates on worth, when a smooth response is expected rather than a threshold.
- Environmental analogues to match trial sites to target sites, instead of predicting from a model.

## Interpretation

On the synthetic data (900 plots along an elevation gradient, two heat-tolerant varieties favoured when maxDT exceeds 35 C), the windows run from 40 to 76 days and are longer at higher, cooler sites. The tree makes one split, at maxDT 34.99. Above it, the tolerant Varieties 01 and 02 move from the bottom of the ranking (−1.98 and −2.19 against the check) to the top (+1.30 and +1.04). Rainfall is never used. The window matters. The degree-day window fits best (log-likelihood −1331), a fixed 30-day window splits on SU instead and fits worse (−1374), and a fixed 90-day window fits worse as well (−1363). Of four target sites, the typical and warm ones are within the trial conditions. A site 3 C hotter than any plot and a site three times wetter are both flagged as extrapolation.

## Limitations

- A tree split shows that preference differs between conditions. It does not prove a causal climate effect.
- Thresholds from a tree are estimates. Report them with the sensitivity analysis, not as fixed cut-offs.
- Predictions for flagged sites are extrapolation and must not be used as recommendations.
- Indices that do not vary across plots are left out of the Mahalanobis distance, with a warning, and redundant indices are handled with a generalized inverse. Some climatrends versions return DTR rounded to whole degrees, which makes it constant in this simulation. Check `packageVersion("climatrends")` when results differ from the README.
- `pltree()` needs PlackettLuce attached with `library("PlackettLuce")`, because it looks up its fitting functions on the search path.

## Files

- `script/helper/functions.R` holds the documented functions.
- `script/01-analysis.R` is the workflow. Open the `.Rproj` file and run it, or run `Rscript script/01-analysis.R` from this folder.
- `script/helper/simulate-data.R` simulates the climate, phenology and rankings.
- `tests/` holds the tests, run with `Rscript tests/run-tests.R`.
