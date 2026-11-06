# Trait associations

Status: methodological rationale from Kauê de Sousa (Bioversity International), 2026-10-31. This is not independent validation of cited studies or software.

- **Primary aim.** Understand how overall farmer preference relates to the other farmer-ranked traits. Do not assume yield is the dominant driver.
- **Provisional trait categories.** Preference or psychological traits (overall preference, taste, marketability), genotype-by-environment traits (maturity, yield, flowering, drought tolerance) and management traits (yield, pests and diseases). The categories overlap and are not an exclusive ontology.
- **Who evaluates.** All ranking traits are evaluated by farmers in this workflow. Quantitative traits may be measured by researchers or farmers. Record observer, instrument, method, scale, timing and units. If unknown, ask the analyst rather than assume.
- **Causality.** Correlation is not causal attribution. Document dependence, uncertainty and alternative explanations.

- **Small samples.** Partial correlations from an inverted tau matrix need many participants who ranked every pair of traits. With about 20 to 30 participants per pair, bootstrap intervals of partial correlations can span almost the whole range from -1 to 1. Then report marginal Kendall tau with bootstrap intervals and say that partial associations cannot be estimated yet.
- **Traits from different moments.** When traits are ranked at different times (establishment, winter, harvest), each pair is computed on the participants who answered both. Report the n of each pair, because attrition changes who is compared.
- **Reference outcome.** Use the most recent overall-type question as the reference, whatever its ClimMob code (`overall`, `generalappreciation`, `generalpreference`). Use `grownext` as reference only when none of these exists, and otherwise report its Kendall tau with the reference as a consistency check. See "Which question is overall preference" in [ranking](ranking.md). Never enter a later overall-type question as an explanatory trait. When the question is about a specific stage, run a separate model with that stage's overall ranking as reference, using only traits recorded at or before it.

- **Measured traits as item covariates.** When traits are measured in the lab or on station (BLUPs of yield components, texture, colour, phenology), model log worth as a linear function of them with `PlackettLuce::pladmm()`, test the covariate model against the free Plackett-Luce model, and read a coefficient as the change in log worth per unit of the trait, holding the others fixed. Fit it per participant group or per tree node to see whether groups weigh the traits differently, as the EtNAM scripts did by gender and location (use case `which-measured-traits-explain-preference`, scripts in `archive/rtbfoods-eba-nigeria/` and `archive/etnam-farmer-ratings/`). Coefficients depend on the scale of each trait, so standardise or report the scale, and do not compare raw coefficients between groups whose covariates were scaled differently.

## Archive examples

- `archive/groundnut-tanzania/` (Kendall partial correlation with backward selection)
- `archive/issd-ethiopia/`
- `archive/potato-kang-et-al/`
- `archive/rtbfoods-eba-nigeria/` (lab trait BLUPs as item covariates, pladmm)
- `archive/etnam-farmer-ratings/` (station BLUPs as item covariates by gender and location)
