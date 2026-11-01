# Ranking

Status: methodological rationale from Kauê de Sousa (Bioversity International), 2026-10-31. This is not independent validation of cited studies or software.

- **Default.** Plackett-Luce for tricot ranking data, with checks for design, network connectivity, model fit and uncertainty.
- **Which question is overall preference.** Trials that asked for overall preference at several moments store it in one of two ways. Either the same code repeats under assessment prefixes (`map1_overall` to `map12_overall`, months after planting), or a ClimMob project holding only one `overall` code stores the later ones as `generalappreciation` or `generalpreference` (`postharvest2_generalappreciation`). Older protocols made this worse, so a column coded `overall` is not necessarily the final judgement. Treat every overall-type question (`overall`, `generalappreciation`, `generalpreference`) as a measure of overall preference at its stage, not as a separate trait. The most recent one is the closest to overall preference, because participants have then seen the whole cycle, including harvest, storage and consumption. Use it as the default overall outcome, whatever its code. Order the assessments by collection date, or by their order in the ClimMob project when dates are missing.
- **Grow next season.** `grownext` records an intention to grow the variety again, not adoption. Use it as overall preference only when the trial has no other overall-type question. Otherwise use it as a consistency check. Compute Kendall tau between the most recent overall-type question and `grownext`, and report it with its n. A high tau is the expected result and supports the overall outcome. A low tau is a finding to flag, since participants may prefer a variety they would not grow again, or the questions may have been misread. Do not enter `grownext` as an explanatory trait.
- **Earlier overall rankings.** Keep them as stage-specific outcomes (for example preference at harvest) rather than dropping them or merging them with the final one. Their agreement with the final ranking shows how preference changes over the trial. A trait recorded after an overall ranking cannot explain that ranking.
- **Coverage of the final outcome.** The last assessment usually has the highest attrition, and it can lose whole groups. Report the valid rows of the overall outcome by district, gender or any grouping used, and say whom the final outcome represents. For a group with too few final answers, report its latest overall ranking as a stage-specific result rather than dropping the group silently.
- **Report the choice.** State which question served as overall preference, its assessment, its code and its number of valid rows. Do not stop to ask when the assessment order is clear. Ask only when the order or the meaning of the questions is unknown.
- **Thurstonian approaches.** An emerging alternative under investigation. Do not imply validated equivalence with Plackett-Luce.
- **Subgroup models, joint models and Plackett-Luce trees.** Each has limitations. Trees give hierarchical binary partitions, which may not yield actionable user personas.
- **Non-hierarchical clusters.** An alternative for identifying user personas, integrating socioeconomic, environmental and management covariates. Clusters are defined without the preference outcome unless explicitly designed otherwise. Then assess whether preference models differ across clusters. Check comparability and the assumptions of the likelihood-ratio test. Statistical significance is not operational usefulness.
- **Cluster search.** Default operational search is k = 2 to 5, judged on stability, adequacy, interpretability, representation and decision feasibility. Report when no solution is defensible. Larger k can be examined as a sensitivity analysis.

- **How many participants.** Judge whether the answers are enough by simulating new trials from the fitted model (use case `how-many-participants-do-we-need`). Do not subsample the real answers and compare with the full fit: subsamples share participants with the full data, so agreement rises towards 1 whatever the real accuracy.
- **Few answers.** Name a group of top items rather than a strict order, give the bootstrap probability of a top-k place, and say that the order will change as more answers arrive.

## Examples behind the rule

Kauê de Sousa (Bioversity International) set this rule after agents misread multi-stage trials. Within-participant Kendall tau, n is the participants who answered both questions.

- **Cassava, NextGen, Tanzania.** 200 participants in six districts ranked overall preference at 1, 3, 6, 9 and 12 months after planting (`map1_overall` to `map12_overall`). The five overall rankings barely agree with each other (tau between -0.07 and 0.13, n 94 to 111, every 95% interval reaching zero). Each one reflects what farmers could judge at that stage. Root traits ranked at 12 months agree with the 12-month overall ranking (yield 0.44, cooking quality 0.43, root shape 0.42, taste 0.31, n 93 to 98) but not with the 9-month one (all between -0.03 and 0.19). Only 98 participants gave a valid 12-month overall ranking, and Handeni kept 2 of 40.
- **Potato, Nigeria 2023.** `overall` was ranked at harvest and `generalappreciation` at the end of the trial, with tau 0.25 between them (n 296). Taste agrees with the final ranking (0.63, n 208) far more than with the harvest one (0.17). Reading `overall` as the final judgement would have understated taste.

Both trials show the same lesson. Overall preference is bound to its stage, the latest ranking integrates the most information, and a trait explains only the overall rankings recorded at or after it.

## Archive examples

- `archive/groundnut-tanzania/`
- `archive/issd-ethiopia/`
- `archive/nextgen-cassava/`
- `archive/crop-variety-management-pnas/`
