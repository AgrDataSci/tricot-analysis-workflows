# Ranking

Status: methodological rationale from Kauê de Sousa (Bioversity International), 2026-10-31. This is not independent validation of cited studies or software.

- **Default.** Plackett-Luce for tricot ranking data, with checks for design, network connectivity, model fit and uncertainty.
- **Which question is overall preference.** A ClimMob project can hold only one question with the code `overall`. Trials that asked for overall preference at several moments store the later ones under other codes, such as `generalappreciation`, `generalpreference` or `grownext`, often with an assessment prefix (`postharvest2_generalappreciation`). Older protocols made this worse, so the code `overall` does not mean the final judgement. Treat every overall-type question as a measure of overall preference at its stage, not as a separate trait. The most recent one is the closest to overall preference, because participants have then seen the whole cycle, including harvest, storage and consumption. Use it as the default overall outcome, whatever its code. Order the assessments by collection date, or by their order in the ClimMob project when dates are missing.
- **Two overall-type questions in the same assessment.** Default to the general preference or appreciation question. Run `grownext` as a sensitivity outcome, since it records an intention to grow the variety again, not adoption. Check the questionnaire wording when it is available.
- **Earlier overall rankings.** Keep them as stage-specific outcomes (for example preference at harvest) rather than dropping them or merging them with the final one. Their agreement with the final ranking shows how preference changes over the trial. A trait recorded after an overall ranking cannot explain that ranking.
- **Report the choice.** State which question served as overall preference, its assessment, its code and its number of valid rows. Do not stop to ask when the assessment order is clear. Ask only when the order or the meaning of the questions is unknown.
- **Thurstonian approaches.** An emerging alternative under investigation. Do not imply validated equivalence with Plackett-Luce.
- **Subgroup models, joint models and Plackett-Luce trees.** Each has limitations. Trees give hierarchical binary partitions, which may not yield actionable user personas.
- **Non-hierarchical clusters.** An alternative for identifying user personas, integrating socioeconomic, environmental and management covariates. Clusters are defined without the preference outcome unless explicitly designed otherwise. Then assess whether preference models differ across clusters. Check comparability and the assumptions of the likelihood-ratio test. Statistical significance is not operational usefulness.
- **Cluster search.** Default operational search is k = 2 to 5, judged on stability, adequacy, interpretability, representation and decision feasibility. Report when no solution is defensible. Larger k can be examined as a sensitivity analysis.

- **How many participants.** Judge whether the answers are enough by simulating new trials from the fitted model (use case `how-many-participants-do-we-need`). Do not subsample the real answers and compare with the full fit: subsamples share participants with the full data, so agreement rises towards 1 whatever the real accuracy.
- **Few answers.** Name a group of top items rather than a strict order, give the bootstrap probability of a top-k place, and say that the order will change as more answers arrive.

## Archive examples

- `archive/groundnut-tanzania/`
- `archive/issd-ethiopia/`
- `archive/nextgen-cassava/`
- `archive/crop-variety-management-pnas/`
