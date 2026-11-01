# Ranking

Status: methodological rationale from Kauê de Sousa (Bioversity International), 2026-10-31. This is not independent validation of cited studies or software.

- **Default.** Plackett-Luce for tricot ranking data, with checks for design, network connectivity, model fit and uncertainty.
- **Thurstonian approaches.** An emerging alternative under investigation. Do not imply validated equivalence with Plackett-Luce.
- **Subgroup models, joint models and Plackett-Luce trees.** Each has limitations. Trees give hierarchical binary partitions, which may not yield actionable user personas.
- **Non-hierarchical clusters.** An alternative for identifying user personas, integrating socioeconomic, environmental and management covariates. Clusters are defined without the preference outcome unless explicitly designed otherwise. Then assess whether preference models differ across clusters. Check comparability and the assumptions of the likelihood-ratio test. Statistical significance is not operational usefulness.
- **Cluster search.** Default operational search is k = 2 to 5, judged on stability, adequacy, interpretability, representation and decision feasibility. Report when no solution is defensible. Larger k can be examined as a sensitivity analysis.

## Archive examples

- `archive/groundnut-tanzania/`
- `archive/issd-ethiopia/`
- `archive/nextgen-cassava/`
- `archive/crop-variety-management-pnas/`
