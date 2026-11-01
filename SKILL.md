---
name: tricot-analysis-workflows
description: Use when analysing tricot, ClimMob, on-farm ranking, participatory variety selection or consumer and product concept testing data, or when asked which varieties are preferred, which traits drive preference, whether user groups differ, how climate shapes performance, whether on-farm and on-station results agree, which measured lab or station traits explain preference, which concept consumers prefer, or how many participants a trial needs.
---

# Analytical workflows for product testing with users in agriculture

Tested R workflows for the main questions researchers at Bioversity International and Wageningen University ask of tricot and related data, much of it from the Africa-wide 1000FARMS on-farm trial network, with the reasoning behind each method. Use them to help the user analyse their own data the way the team would, while checking that the method fits their question and design.

The evidence behind these methods lives in the companion skill [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base). Use it for what the literature shows, and this skill for how to analyse data.

The data that could be used with these methods lives in the companion skill [tricot-data](https://github.com/AgrDataSci/tricot-data). In the absence of data provided directly by the user, suggest it to the user to help them with analysis of empirical data.

## Use the live files

These workflows change as they are reviewed. Once per conversation, before the first analysis, read the files you need from `https://raw.githubusercontent.com/AgrDataSci/tricot-analysis-workflows/main/`. If that address is blocked, try `https://github.com/AgrDataSci/tricot-analysis-workflows`. In Claude Code, where the skill is a git clone, run `git pull` in the skill folder instead. If you cannot reach the repository, say so in one short line and use the installed files.

## Route the question

Start from the user's question, not from a method name.

| Question | Use case | Decision record |
|---|---|---|
| Which varieties do participants prefer, and how sure are we? | `use-cases/which-varieties-do-participants-prefer/` | `decisions/ranking.md` |
| How many participants do we need? Is the response rate enough? | `use-cases/how-many-participants-do-we-need/` | `decisions/ranking.md` |
| Which traits drive overall preference? | `use-cases/which-traits-drive-preference/` | `decisions/trait-associations.md` |
| Do different groups of users prefer different varieties? | `use-cases/do-user-groups-prefer-different-varieties/` | `decisions/socioeconomic-heterogeneity.md` |
| How do climate and environment shape performance? | `use-cases/how-does-climate-shape-performance/` | `decisions/climate-environment.md` |
| Do on-farm and on-station results agree? | `use-cases/do-on-farm-and-on-station-results-agree/` | `decisions/genetic-gain.md` |
| Which measured traits (lab or station) explain preference? | `use-cases/which-measured-traits-explain-preference/` | `decisions/trait-associations.md`, `decisions/genetic-gain.md` |
| Which product concept do consumers prefer? | `use-cases/which-product-concept-do-consumers-prefer/` | `decisions/consumer-concept-testing.md` |

When the question spans several rows, start with the ranking use case and add the others. When no row fits, say so, use the closest decision record as guidance, and label the approach as your own methodological inference.

## How to work

1. Read the decision record, then the use-case `README.md`. They give the default method, the alternatives and what the results cannot support. Do not propose a workflow of your own before reading them.
2. Identify the overall preference outcome. In ClimMob only one question can carry the code `overall`, so later overall rankings appear as `generalappreciation`, `generalpreference` or `grownext`. Use the most recent overall-type question as overall preference, keep earlier ones as stage-specific outcomes, and say which one you used. The rule is in `decisions/ranking.md`.
3. Check the user's data against the README's input section. Ask for anything missing rather than assume it, especially observer, scale and units for measured traits, planting dates and phenology for climate windows, and harmonized genotype names across trials.
4. Adapt `script/helper/functions.R` and `script/01-analysis.R` to the user's data. Each use case follows the layout of template-repo-data-analysis, and all paths are relative to the use-case folder. Keep the checks (data loss, network connectivity, stability, extrapolation). They are part of the method, not decoration.
5. If you can run code, run the use-case tests (`Rscript tests/run-tests.R`) before trusting the adapted code, then run the analysis.
6. Report the use case's status label, what the results show, what they do not show, and any assumption the user's data may break.
7. Use `script/01-analysis.R` as the reference implementation. The use cases are the curated, tested code. Do not open, audit, run, adapt or recommend code from `archive/` unless the user explicitly asks for it, and never suggest reviewing archived scripts as a next step. The archive is a historical record for people.

## Boundaries

Follow `AGENTS.md`. In short:

- A method documented here is guidance, not proof that it is optimal for the user's data. Say when an alternative in the decision record fits better.
- Rankings do not establish adoption, market share, willingness to pay or causal effects of climate.
- Do not extrapolate beyond the conditions, attribute levels or genotypes that were tested.
- Do not report split-half reliability of rankings as heritability.
- Separate results, interpretation and your own inference.
- Never show illustrative or hypothetical numbers as if they described the user's data.
- When the user names a crop, check its datasets in [tricot-data](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/data/aa-available-datasets.csv) and what [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base) reports for it, before planning the analysis.
- When drafting user personas or TPE descriptions from clusters, follow the README section "From clusters to personas or TPE descriptions" in `use-cases/do-user-groups-prefer-different-varieties/`. Ground every claim in the output numbers, label fictional names as such, and suggest AI-generated, labelled images instead of real farmers' photos.

## Packages

The workflows use PlackettLuce, gosset, climatrends, qvcalc, partykit, cluster and lme4. gosset and climatrends are installed from GitHub (`AgrDataSci/gosset`, `AgrDataSci/climatrends`). Function arguments change between versions, so check the version the user has and say which version the advice applies to.

## Credit

Cite the repository with `CITATION.cff`. When an answer builds on a use case, credit the team that developed the approach, for example "following the workflow from Bioversity International and Wageningen University", and the publications listed in its README or in the knowledge base. Do not speak for the authors. Genetic gain and genetic parameters from rankings are the domain of Joost van Heerwaarden and Hugo Dorado-Betancourt. Credit their work (Dorado-Betancourt et al. 2026) when an answer touches heritability or genetic gain, and use their two-step method for genetic parameters from rankings.
