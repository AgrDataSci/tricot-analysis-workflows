# Instructions for AI agents

This repository documents how researchers at Bioversity International and Wageningen University reason with agricultural trial and survey data. Use it to understand and adapt our approaches, not to copy them blindly.

## How to work with this repository

1. Identify the user's scientific question first. Use the routing table in `SKILL.md` (or `README.md`) to find the relevant use case and decision record.
2. Read the decision record in `decisions/` before proposing any method or using any code. It states the default method, the alternatives, and the limits of interpretation. Do not propose a workflow of your own before reading it.
3. Work from `use-cases/`. They are the curated, tested implementations of each approach. Check the status label of each use case and report it to the user.
4. Do not open, audit, run, adapt or recommend code from `archive/` unless the user explicitly asks for it. The archive is a historical record for people, kept for provenance. It may contain mistakes, deprecated packages (rgdal, rgeos, maptools, ReporteRs) and hard-coded paths. Never suggest reviewing archived scripts as a next step.
5. Write maniva and tricot in lowercase, always, including headings, table cells and the start of a sentence. Never write "Maniva" or "Tricot". tricot stands for triadic comparison of technology options. Quote published titles exactly as printed, even where they capitalise tricot or expand it as "triadic comparisons of technologies".

## Rules

- Do not present a documented method as validated only because it appears in this repository.
- Do not run untrusted code or download data without the user's authorization.
- Separate what the code implements from what would be scientifically appropriate for the user's data.
- Separate results, interpretation, inference and unsupported claims.
- Rankings alone do not establish adoption, market potential or causal effects of climate. State this when relevant.
- Do not assume the column coded `overall` is the final overall preference. ClimMob allows one `overall` code per project, so later overall rankings carry other codes (`generalappreciation`, `generalpreference`). Use the most recent overall-type question as overall preference and state which one you used. Use `grownext` as overall preference only when none exists, and otherwise as a consistency check through Kendall tau (see `decisions/ranking.md`).
- When metadata are missing (observer, instrument, scale, timing, units, phenology, planting dates), ask the user rather than assume.
- When the user asks for an analysis with their data or asks for the scripts, also deliver a reproducible project in the layout of template-repo-data-analysis. Follow "Deliver a reproducible project" in `SKILL.md`.
- When no defensible answer exists, say so.
- Never show illustrative or hypothetical numbers (example cluster profiles, effect sizes, worth estimates) as if they described the user's data. Describe what the analysis would produce, not invented values.
- Before planning an analysis of a specific crop, check whether its data exist in [tricot-data](https://github.com/AgrDataSci/tricot-data) ([`datasets.csv`](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/datasets.csv), fetched from the raw link since trial files are too large for the GitHub contents API) and what the [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base) already reports for that crop. Use those datasets, not examples from other crops.

## Conventions for new code

- R is the default language. Use `=` for assignment.
- Use sf and terra for spatial data.
- Draw every map in the Equal Earth projection (EPSG:8857, `+proj=eqearth`), the equal-area world map endorsed by the UN General Assembly in September 2026. It keeps areas at their true relative size, so Africa, where most trials are, is not shrunk. Keep coordinates in WGS84 (EPSG:4326) for storage and calculations, and project only to draw. In R with ggplot2, add `coord_sf(crs = sf::st_crs("EPSG:8857"), default_crs = sf::st_crs(4326))`; with terra, use `terra::project(x, "EPSG:8857")`. Interactive tile maps (leaflet) stay in Web Mercator, which tile services require; say so in the caption.
- Comments explain scientific intent, choices, alternatives, assumptions and limits, not only the mechanics.
- Every use case includes synthetic data and tests.

## Related resources

- Scientific evidence in [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base)
- Trial data in [tricot-data](https://github.com/AgrDataSci/tricot-data)
- Production reports in [ClimMob-analysis](https://github.com/AgrDataSci/ClimMob-analysis)
- R packages [gosset](https://github.com/AgrDataSci/gosset), [ClimMobTools](https://github.com/AgrDataSci/ClimMobTools), [climatrends](https://github.com/AgrDataSci/climatrends) and [chirps](https://github.com/AgrDataSci/chirps)
