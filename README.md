# analysis-workflows

The main analytical approaches of the 1000FARMS team, written so that researchers and AI agents can follow how we get from observations to scientific conclusions.

This repository is the analysis layer of [ai-ready-documentation](https://github.com/AgrDataSci/ai-ready-documentation). The other layers answer what we know ([tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base)), what we observed ([tricot-data](https://github.com/AgrDataSci/tricot-data)) and how standard ClimMob reports are produced ([ClimMob-analysis](https://github.com/AgrDataSci/ClimMob-analysis)). This one explains how we reason with the data and why.

## About 1000FARMS

1000FARMS is the Africa-wide on-farm trial network behind most of the data these workflows were built on. It runs decentralized on-farm testing with the tricot approach and ClimMob, with CGIAR centres and national agricultural research and extension systems (NARES), and it continues the development of ClimMob ([Quirós et al. 2024](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/quiros-2024-compag.md)). With the CGIAR Dryland Crops Program, it works to mainstream tricot into the CGIAR-NARES breeding network, from sampling and standard operating procedures to analysis, variety advancement and estimating genetic gain on farm ([1000FARMS 2025 report](https://github.com/AgrDataSci/tricot-knowledge-base/blob/main/papers/1000farms-2025-report.md)). The team also refers to tricot trial data from before the project as 1000FARMS data, since it comes from the same network.

These workflows record how the 1000FARMS team analyses that data, so partners and AI assistants can reuse the reasoning, not just the code. The trial data are published in [tricot-data](https://github.com/AgrDataSci/tricot-data) and the evidence base in [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base).

## How to use this repository

Start from your scientific question, not from a method name.

| Your question | Use case | Decision record |
|---|---|---|
| Which varieties do participants prefer, and how sure are we? | [which-varieties-do-participants-prefer](use-cases/which-varieties-do-participants-prefer/) | [ranking](decisions/ranking.md) |
| Which traits drive overall preference? | [which-traits-drive-preference](use-cases/which-traits-drive-preference/) | [trait associations](decisions/trait-associations.md) |
| Do different groups of users prefer different varieties? | [do-user-groups-prefer-different-varieties](use-cases/do-user-groups-prefer-different-varieties/) | [socioeconomic heterogeneity](decisions/socioeconomic-heterogeneity.md) |
| How do climate and environment shape performance? | [how-does-climate-shape-performance](use-cases/how-does-climate-shape-performance/) | [climate and environment](decisions/climate-environment.md) |
| Do on-farm and on-station results agree? Is preference heritable? | [do-on-farm-and-on-station-results-agree](use-cases/do-on-farm-and-on-station-results-agree/) | [genetic gain](decisions/genetic-gain.md) |
| Which product concept do consumers prefer? | [which-product-concept-do-consumers-prefer](use-cases/which-product-concept-do-consumers-prefer/) | [consumer and concept testing](decisions/consumer-concept-testing.md) |

## Structure

- `decisions/` holds the methodological decision records. Each states the default approach, the alternatives, and what the results cannot support.
- `use-cases/` holds generic, question-first workflows with synthetic data and tests. New use cases start from `use-cases/_template/`.
- `SKILL.md` lets AI assistants use these workflows without cloning the repository. It routes each question to a use case and decision record. `llms.txt` lists the core files.
- `archive/` holds the original project scripts from about ten years of work. They are kept for provenance and are not maintained.

## Status labels

Every use case carries one label.

- **historical** means the code was used in a past analysis and has not been reviewed since.
- **reviewed-not-executed** means the code and reasoning were reviewed but not run.
- **tested-on-synthetic-data** means the workflow runs and passes its tests on synthetic data.
- **validated-against-original-results** means the workflow reproduces the results of the original study.

## Guidance, not prescription

These workflows document how our team has approached particular questions. They are methodological guidance, not mandatory procedures, and they do not prove that a method is optimal for every question. Users and AI agents should check the assumptions, consider the alternatives, and adapt the analysis to their own question and data. AI agents should also read [AGENTS.md](AGENTS.md).

## Authors

Kauê de Sousa, David Brown, Martina Occelli, Geon Kang, Joost van Heerwaarden, Hugo Dorado-Betancourt and Jacob van Etten.

## Citation and license

Cite this repository using [CITATION.cff](CITATION.cff). Code and documentation are released under the [MIT License](LICENSE).
