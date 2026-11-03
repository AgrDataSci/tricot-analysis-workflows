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
2. Identify the overall preference outcome. Repeated overall rankings appear either as one code under assessment prefixes (`map1_overall` to `map12_overall`) or, since ClimMob allows one `overall` code per project, as `generalappreciation` or `generalpreference`. Use the most recent overall-type question as overall preference, keep earlier ones as stage-specific outcomes, report its valid rows by group, and say which one you used. Use `grownext` as overall preference only when no overall-type question exists. Otherwise report its Kendall tau with the overall outcome as a consistency check. The rule is in `decisions/ranking.md`.
3. Check the user's data against the README's input section. Ask for anything missing rather than assume it, especially observer, scale and units for measured traits, planting dates and phenology for climate windows, and harmonized genotype names across trials.
4. Adapt `script/helper/functions.R` and `script/01-analysis.R` to the user's data. Each use case follows the layout of template-repo-data-analysis, and all paths are relative to the use-case folder. Keep the checks (data loss, network connectivity, stability, extrapolation). They are part of the method, not decoration.
5. If you can run code, run the use-case tests (`Rscript tests/run-tests.R`) before trusting the adapted code, then run the analysis.
6. Report the use case's status label, what the results show, what they do not show, and any assumption the user's data may break.
7. Use `script/01-analysis.R` as the reference implementation. The use cases are the curated, tested code. Do not open, audit, run, adapt or recommend code from `archive/` unless the user explicitly asks for it, and never suggest reviewing archived scripts as a next step. The archive is a historical record for people.

## Deliver a reproducible project

When the user asks for an analysis and supplies data, or asks for the scripts, return the full analysis as a project that follows [template-repo-data-analysis](https://doi.org/10.5281/zenodo.15224388). Show the main results in the chat as usual. The project is an addition, not a replacement.

Skip this for quick exploratory questions, such as checking columns or a single summary, unless the user asks for the files.

1. Name the project folder `<crop>-<use-case>-<YYYYMMDD>`, for example `cassava-which-traits-drive-preference-20261102`.
2. Use the template folders that the analysis needs. Always include `data/`, `script/`, `output/`, `decisions/`, `metadata/` and `README.md`. Add `tests/` and `docs/` when the analysis has them. Add an `.Rproj` file named after the project folder, copied from the template's, so the project opens in RStudio with the root as working directory.
3. Put the user's input in `data/` unchanged. Write cleaned or derived data to `output/` from the scripts, never by hand.
4. Number the scripts (`01-`, `02-`) and use paths relative to the project root, so the project runs on any computer.
5. If the user supplied their own scripts and your scripts differ substantially from them, keep the originals unchanged in `script/user-script/`. Substantial means a different method, model, data filtering or outcome variable, not only renamed objects or updated paths. In the README, list what changed and why, script by script. When the changes are minor, edit the user's scripts in place and note the edits in the README.
6. Generate every file in `output/` by running the scripts. If you cannot run R, run the analysis in Python or another language you can execute, then translate it to R for `script/`. In that case
   - keep the code that produced the outputs in `script/other-language/`, so the outputs remain reproducible
   - mark each R script as untested in its header and in the README
   - name the packages you used in place of the R ones and say where results may differ
   - if no faithful equivalent exists for a method (for example PlackettLuce with ties, qvcalc or Plackett-Luce trees), do not approximate it. Deliver the untested R script without that output and tell the user.
7. Copy the use-case decision record into `decisions/` and record any change you made to the default method and why.
8. Fill `metadata/provenance.json` so each output points to its script, input file and decision record. Never invent a DOI, ORCID, license or funder. Write `null` and tell the user.
9. Record the software of each run in `script/session-info/`, as the template does, and pin package versions in `renv.lock` when you can. Include `.Rprofile` and the `renv/` folder only when you ran `renv::snapshot()`. The template's `.Rprofile` activates renv, so without `renv/` R fails on startup.
10. In `README.md`, state the scientific question, the use case and its status label, the commit or version of this repository you used, how to rerun the analysis, and what the results cannot support. Credit the workflow as described under Credit.
11. Deliver the project as a zip file by default. Deliver it as a plain folder when the user works in a local project or repository.

Draw every map in the Equal Earth projection (EPSG:8857), the equal-area world map the UN General Assembly endorsed in September 2026. Keep coordinates in WGS84 for calculations and project only to draw, for example `coord_sf(crs = sf::st_crs("EPSG:8857"), default_crs = sf::st_crs(4326))`. Interactive tile maps stay in Web Mercator. The rule is in `AGENTS.md`.

Before packaging, check the input for personal data such as names, phone numbers, ID numbers or exact GPS points. Warn the user and ask whether to remove, pseudonymize or keep it. Do not include it by default.

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
