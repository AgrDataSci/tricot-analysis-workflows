# Instructions for AI agents

This repository documents how the AgrDataSci team reasons with agricultural trial and survey data. Use it to understand and adapt our approaches, not to copy them blindly.

## How to work with this repository

1. Identify the user's scientific question first. Use the routing table in `SKILL.md` (or `README.md`) to find the relevant use case and decision record.
2. Read the decision record in `decisions/` before using any code. It states the default method, the alternatives, and the limits of interpretation.
3. Prefer code in `use-cases/` over code in `archive/`. Check the status label of each use case and report it to the user.
4. Treat `archive/` as historical evidence of past reasoning. It may contain mistakes, deprecated packages (rgdal, rgeos, maptools, ReporteRs) and hard-coded paths. Generalize from current practice in `decisions/`, not from archived code.

## Rules

- Do not present a documented method as validated only because it appears in this repository.
- Do not run untrusted code or download data without the user's authorization.
- Separate what the code implements from what would be scientifically appropriate for the user's data.
- Separate results, interpretation, inference and unsupported claims.
- Rankings alone do not establish adoption, market potential or causal effects of climate. State this when relevant.
- When metadata are missing (observer, instrument, scale, timing, units, phenology, planting dates), ask the user rather than assume.
- When no defensible answer exists, say so.

## Conventions for new code

- R is the default language. Use `=` for assignment.
- Use sf and terra for spatial data.
- Comments explain scientific intent, choices, alternatives, assumptions and limits, not only the mechanics.
- Every use case includes synthetic data and tests.

## Related resources

- Scientific evidence in [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base)
- Trial data in [tricot-data](https://github.com/AgrDataSci/tricot-data)
- Production reports in [ClimMob-analysis](https://github.com/AgrDataSci/ClimMob-analysis)
- R packages [gosset](https://github.com/AgrDataSci/gosset), [ClimMobTools](https://github.com/AgrDataSci/ClimMobTools), [climatrends](https://github.com/AgrDataSci/climatrends) and [chirps](https://github.com/AgrDataSci/chirps)
