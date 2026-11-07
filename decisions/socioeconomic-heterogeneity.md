# Socioeconomic heterogeneity

Status: methodological rationale from Kauê de Sousa (Bioversity International), 2026-10-31, expanded 2026-11-06 from the archived scripts and the published studies listed under sources. Points marked (script) are read from the code and await Kauê's confirmation. This is not independent validation of cited studies or software.

## Three questions, three methods

- **Which covariate best splits preferences.** A Plackett-Luce tree on participant covariates (`PlackettLuce::pltree`). It uses the rankings to find the split, gives hierarchical binary partitions, and is the right tool when the question is exploratory. The partitions are rarely actionable personas on their own.
- **Do predefined groups differ.** Women and men, districts, projects, countries. This is the default when the groups were designed into the study (use case `do-user-groups-prefer-different-varieties`, part A). Compare only groups with at least 20 valid answers and never the unlabelled ones, as the ClimMob report does. Check whether the group is confounded with site or district, and test the group within sites, one model per site against one per site and group, with a permutation of the labels within sites. A test pooled over sites reads a site effect as a group effect whenever the group is unevenly spread. Report worth per group with quasi-standard errors and the n behind it, and the balanced ranking beside the pooled one when the smallest group holds under 25% of the answers. Test trait by trait, not only overall preference.
- **Which user types exist, and do they prefer different things.** Build groups from covariates alone, without the rankings, then test preferences between the groups. This is the default for defining breeding target users and differentiated recommendations, because the groups can be recognised in the field without anyone's rankings, and the test is not circular (use case `do-user-groups-prefer-different-varieties`).
- Do not conflate the three. Explaining heterogeneity, defining target users and differentiating recommendations are different goals, and a result from one does not serve the others.

## Default approach for user types

- **Covariates from the decision.** Choose the covariates for the users to be targeted, household composition, land, livestock, crops and their use (home consumption, sold at harvest, sold later), market access, off-farm income, gender of the decision maker. Record when and how each was collected. Covariates measured after the trial can be shaped by it.
- **Household indicators.** When a household survey exists (RHoMIS, the 2016 baseline in Trifinio), derive the standard indicators before clustering. Food availability in kilocalories per adult equivalent from crops, livestock, off-farm income and wild food, crop and livestock diversity, land per adult equivalent, the share of production controlled by women in energy terms, the Progress out of Poverty Index (script, `rhomis/`). Use the indicators, not the raw questionnaire items, and check their correlations before clustering (script, `Indicator_correlations.R`).
- **Prepare the data.** Impute missing values, log-transform skewed variables, and treat categorical variables as categorical. One-hot encoding before k-means (script, OSIRIS and MIP) weighs a binary variable as much as a whole continuous one, so the use case prefers model-based clustering with variable selection (`VarSelLCM`), or FAMD with few components before PAM, and shows how Gower distance with PAM can be hijacked by one binary variable.
- **Number of groups.** Search k = 2 to 5. Choose on stability, adequacy (ICL or BIC, the elbow in the scripts), interpretability, representation (smallest group at least 10%) and decision feasibility. Larger k is a sensitivity analysis. Report when no solution is defensible.
- **Name the groups from their profiles.** The MIP analysis named four, farming for resilience, for family nutrition, for market access, for family security (script, `04-data-analysis.R`). Ground every name in the covariate means of the group, and label fictional personas as such (use case README, "From clusters to personas").
- **Test preferences between groups.** Likelihood ratio between the group models and the pooled model, a permutation test of the labels, and pairwise tests between groups. Groups that do not differ in preference need no separate recommendation, whatever their profiles.
- **Validate the segmentation as a predictor.** Compare, by blocked cross-validation (`gosset::crossvalidation`), an intercept-only model, a model on gender, a tree on the raw covariates and the segment model (script, MIP `03-model-validation.R`). Keep the segments only when they predict rankings better than the simpler models.
- **Gender.** Analyse women and men separately before pooling, because trait weights differ even when top choices coincide (Gesesse et al. 2023, Woldeyohannes et al. 2022, Voss et al. 2025), and differences often sit in specific traits such as labour-saving ones (Daudi et al. 2026). Gender preferences also shift with food security, household type, control of resources and region (Teeken et al. 2021), and women and men each form several farm types (Cairns et al. 2025). So a test between women and men is the first check, gender is a covariate within user types, not a type, and a gender result holds for those groups in those places.
- **Linking to trait rankings.** Once groups exist, compute Kendall tau between each trait ranking and overall preference within each group (script, `tricot-rhomis/05_kendall_tau.R`), to see which traits drive preference in each group. The rules in [trait associations](trait-associations.md) apply, including the n per pair.
- **Alpha.** As in [climate and environment](climate-environment.md), 0.01 for large data sets or a conservative case, 0.05 or 0.10 otherwise, stated with the reason.

## Impact and adoption

- Rankings measure preference. Adoption, varietal diversification, food security and income need a design for them, a baseline and endline with a comparison group and difference in differences, as in Occelli et al. (2024), where tricot and group PVS gave similar diversification and adoption and tricot cost 28% less than group PVS. The econometric scripts are in `archive/occelli-tricot-pvs/`, and the matching of trial participants to survey households is the first and hardest step (script, `01_` and `02_match_farmers_ids`).
- Participation itself is a covariate. Participants who stayed in the trial through all stages differ from those who left (script, "In" and "Out" of the project in the 2016 baseline), so attrition is reported by group before any claim about preferences by group (see coverage in [ranking](ranking.md)).

## What the results cannot support

- With two or three groups and fewer than about 30 valid answers per group, worth plots often look different when the test is not significant. Write "the groups look different but there are not yet enough answers to tell", never "the groups prefer different items", and do not put such a claim on material for participants.
- Do not cluster participants when only a handful of covariates were recorded (age, gender, region). Compare the predefined groups instead.
- A stable partition can be a wrong one. A binary variable can hijack the clusters with perfect stability. Check which variables drive the partition.
- A group that differs in preference is not yet a market segment or a breeding target. That needs the group to be reachable, large enough and stable over seasons.
- Statistical significance is not operational usefulness.

## Sources

- [Occelli et al. 2024, Agricultural Economics](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/occelli-2024-agecon.md)
- [Voss et al. 2025, Plants People Planet](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/voss-2025-ppp.md)
- [Cairns et al. 2025, Plants People Planet](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/cairns-2025-ppp.md)
- [Gesesse et al. 2023, PNAS](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/gesesse-2023-pnas.md)
- [Woldeyohannes et al. 2022, eLife](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/woldeyohannes-2022-elife.md)
- [Daudi et al. 2026, Food and Energy Security](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/daudi-2026-fes.md)
- [Teeken et al. 2021, Frontiers in Sustainable Food Systems](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/teeken-2021-fsufs.md)
- [Donovan et al. 2025, Experimental Agriculture](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/donovan-2025-expagric.md)

## Archive examples

- [ClimMob-analysis](https://github.com/AgrDataSci/ClimMob-analysis) `R/44_group_analysis.R` (group eligibility, balanced ranking, agreement between and within groups)

- `archive/rhomis/` (household indicators from the RHoMIS baseline, food availability, gender control, PPI)
- `archive/tricot-rhomis/` (linking tricot rankings to household data, Kendall tau by node)
- `archive/occelli-tricot-pvs/` (matching participants to survey households, econometric analysis)
- `archive/mip-concept-testing/` (segments from covariates, validation by cross-validation)
- `archive/osiris-1000farms/` (k-means on one-hot encoded covariates)
- `archive/groundnut-tanzania/` (`05-data-analysis-clusters.R`)
