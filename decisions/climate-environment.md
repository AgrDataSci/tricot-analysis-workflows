# Climate and environmental data

Status: methodological rationale from Kauê de Sousa (Bioversity International), 2026-10-31, expanded 2026-11-06 from the archived scripts and the published studies listed under sources, and reviewed by Kauê de Sousa on 2026-11-06. Points marked (script) are read from the code. This is not independent validation of cited studies or software.

## Default approach

- **Climate of the plot, not of the site.** Link every plot to daily climate by its coordinates and its planting date. Two plots at the same site planted weeks apart experienced different seasons. Design variables (location, season, soil class) explained rankings less well than climate in van Etten et al. (2019), and adding coordinates to a climate model added nothing.
- **Window from phenology.** Compute indices over the period from planting to the crop stage that matters, the vegetative stage for establishment, flowering for heat and cold stress, grain filling for drought. Estimate the stage from accumulated growing degree days with documented thresholds per crop (`climatrends::GDD`), with the base temperature and the degree days to the stage recorded with their source. The PNAS scripts carried the degree days per stage for bean, wheat, durum wheat and rice in one table (script). When phenology is unknown, use fixed windows and report them as a sensitivity analysis, as the climate use case does with 30 and 90 days, and as OSIRIS and WorldVeg did with 65 to 70 days for short-cycle vegetables.
- **Indices, not raw series.** Summarise each window with the ETCCDI-style indices now in climatrends. Temperature, maxDT, minDT, maxNT, minNT, DTR, SU, TR, CFD, WSDI, CSDI, T10p, T90p. Rainfall, MLDS, MLWS, R10mm, R20mm, Rx1day, Rx5day, R95p, R99p, Rtotal, SDII. Night indices matter. Bean rankings in Nicaragua shifted at 18.7 C maximum night temperature and durum wheat at 8.4 C minimum night temperature, both close to published physiological thresholds. Evapotranspiration (Blaney-Criddle with crop coefficients) was tried in 2018 and dropped (script).
- **Sources.** Record the climate product, its spatial and temporal resolution and the download date. Land-surface temperature (MODIS) shifts thresholds relative to air temperature, which the 2019 paper states. Later work used AgERA5 through ag5Tools, CHIRPS through chirps and NASA POWER through climatrends. Interpolate missing days in satellite series before computing indices (script, `modis_interpolation.R` in the Musa archive).
- **Covariate screening.** Drop indices with near-zero variance. Report strong correlations between indices rather than removing them, because the tree picks one of a correlated set and the others are candidate explanations (see [data synthesis](data-synthesis.md)).
- **Covariate selection by cross-validation.** With many indices, select them by forward stepwise selection on cross-validated fit (`gosset::crossvalidation`; the earlier `forward()` was removed from gosset), comparing models on deviance, pseudo R2 and Kendall tau averaged over folds. Folds follow the design, by season or by site, never random rows, so the test fold holds conditions the training folds did not see. The PNAS scripts averaged fold statistics with Stouffer weights or by fold size (script). The 2019 paper used 10-fold cross-validated pseudo R2 to compare covariate sets.
- **Plackett-Luce trees.** Partition the rankings on the selected indices with `PlackettLuce::pltree`. Keep the pseudo-rankings so the network stays connected after a split. Set the minimum node size as a share of the data large enough for stable worth in every node, 50 plots in the PNAS scripts, 35% of the rankings in the Musa synthesis, more with sparse networks.
- **Alpha is a choice, not a creed.** In practice the team uses alpha 0.05 or 0.10, with Bonferroni, and states the choice with the reason. Alpha 0.01 is not a rule. It is a conservative option for large data sets, above 5,000 participants, or to convince demanding reviewers and statisticians. van Etten et al. (2019) used alpha 0.01 with Bonferroni correction because the data were large and the approach was new, and a conservative threshold made the case easier to defend in review. WorldVeg used 0.10 with five pseudo-rankings on small vegetable trials (script). Report the splits found at the stricter level as the robust ones.
- **Thresholds are estimates.** A split value is a point estimate from a greedy search. Report it with the stability assessment (refits on subsamples, see [data synthesis](data-synthesis.md)) and with the sensitivity to the window, not as a fixed cut-off, and compare it with published physiological thresholds for the crop.
- **Continuous alternatives.** When a smooth response is expected rather than a threshold, fit Plackett-Luce with covariates on worth, or a Bradley-Terry model with the same covariates, and compare the cross-validated fit with the tree.

## From model to recommendation

- **Correct for the trial seasons.** The seasons of a trial are a sample of the climate. Predict over a base period of many seasons (15 years in the 2019 paper) and average, so the recommendation is not tied to the weather of the years the trial ran.
- **Planting dates for unobserved places and seasons.** Prediction over a region needs a planting date per cell and season. Model planting as a survival process on accumulated rainfall and temperature indices before planting, trained on the observed planting dates with pseudo-absences (script, `PlantingDates.R`; Cox models by season in Brown et al. 2022). Report the concordance index per season and distrust seasons where it is low.
- **Three readings of the prediction.** Probability of winning gives the expected best variety. Worst regret gives the risk-averse choice, the variety whose loss relative to the winner is smallest in its worst season (`gosset::regret`). Reliability gives the probability of outperforming a check (`gosset::reliability`). They disagree, and the choice between them is a decision about risk, so report more than one.
- **Portfolios.** Ensembles of trees on bootstrap or subsample draws give a distribution of winners per cell, from which a portfolio of a few complementary varieties is built, and climate envelopes show where each variety is within a share of the optimum (script, `portfolio_construction_from_PLTE.R`, `ClimateEnvelope.R`).
- **Maps.** Draw the predicted top varieties per cell, the normalised entropy of the prediction as the uncertainty in the rankings, and the area of applicability as the uncertainty in the environment (Brown et al. 2022). Cells outside the area of applicability, or beyond the observed range of any index, or far in Mahalanobis distance from the trial conditions, are extrapolation and get no recommendation (climate use case). Maps use the Equal Earth projection, as everywhere in the suite.
- **Analogues.** Environmental similarity between trial and target sites is the alternative to prediction from a model, and the safer one when the target lies at the edge of the trial conditions. Explore both.
- **Forecasts.** Seasonal forecasts raise predictive fit when they are good. The 2019 paper tested perfect forecasts only, so gains from real forecasts are untested.

## What the results cannot support

- A split on a climate index shows that rankings differ between conditions. It is consistent with a physiological mechanism when the threshold matches the literature, and it is still confounded with soils, management and who the farmers are.
- Predictions outside the trial conditions are extrapolation, whatever the model says.
- Fit depends on the diversity of the varieties tested. The 2019 paper found the lowest fit where the set was narrow and modern, so a low pseudo R2 is not evidence that climate does not matter.
- Recommendations from rankings say nothing about adoption, income or food security.

## Sources

- [van Etten et al. 2019, PNAS](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/vanetten-2019-pnas.md)
- [Brown et al. 2022, Crop Science](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/brown-2022-cropsci.md)
- [Brown et al. 2023, Agronomy Journal](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/brown-2023-agronj.md)
- [de Sousa et al. 2023, JOSS, climatrends](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/desousa-2023-joss.md)
- [de Sousa et al. 2023, SoftwareX, gosset](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/desousa-2023-softwarex.md)

## Archive examples

- `archive/crop-variety-management-pnas/` (`AddCovariatesClimMob.R` windows by growing degree days, `ClimMobLearning.R` cross-validation, `PlantingDates.R`, `portfolio_construction_from_PLTE.R`, `ClimateEnvelope.R`, `MapPerformance.R`)
- `archive/musa-blsd-data-synthesis/` (AgERA5 extraction, stability of the tree)
- `archive/osiris-1000farms/` (fixed 65 to 70 day windows for vegetables)
- `archive/etnam-farmer-ratings/` (environmental data for station sites)
- `archive/worldveg-choose-growth-thrive/` (alpha 0.10 with five pseudo-rankings)
