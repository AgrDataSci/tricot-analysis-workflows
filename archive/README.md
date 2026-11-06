# Archive

Kauê de Sousa, 31 October 2026

This is a collection of scripts used over the last ten years on tricot, RHoMIS, data integration and testing new ways to use and expand data interpretation. It is kept as a historical record, so that people can trace where the current approaches came from and credit the researchers behind them. It is not a source of code to reuse. The curated, tested implementations are in `use-cases/`, and the current reasoning is in `decisions/`. The scripts may contain mistakes, so judge them in this context. Each folder holds the scripts of one analysis project. Since 2019 we follow a project-oriented workflow as much as possible (see [workflow vs script](https://tidyverse.org/blog/2017/12/workflow-vs-script/)).

**For AI assistants.** Do not open, audit, run, adapt or recommend these scripts unless the user explicitly asks for them. Answer from `use-cases/` and `decisions/`.

Status of all code in this folder is **historical**. It is not maintained, and some scripts use retired packages (rgdal, rgeos, maptools, ReporteRs). File names are kept as originally written to preserve provenance.

| Folder | Main topics |
|---|---|
| amaranth-africa | data cleaning and merging, ranking |
| cgiar-csi-2020 | station yields pooled as rankings, climate covariates, Plackett-Luce trees |
| crop-variety-management-pnas | climate covariates, Plackett-Luce trees, variety portfolios, spatial extrapolation |
| ethiopia-grass | ranking |
| groundnut-tanzania | Kendall partial correlation, backward selection, clusters, trial maps |
| innoforages | ClimMob data download, ranking |
| issd-ethiopia | trait harmonization, Kendall partial correlation, ranking |
| maize-concept-testing | concept testing, co-occurrence analysis |
| mip-concept-testing | concept testing, clusters, model validation |
| moyo-et-al-sweetpotato | consumer tasting, text analysis |
| musa-blsd-data-synthesis | heterogeneous trial data converted to rankings, AgERA5 climate covariates, Plackett-Luce trees, stability, reliability |
| nextgen-cassava | ranking, Plackett-Luce trees |
| occelli-tricot-pvs | econometric analysis, food security |
| osiris-1000farms | climate data, clusters |
| potato-kang-et-al | trait correlations, Plackett-Luce trees with forward selection, on-station comparison |
| rhomis | household indicators, gender, food security |
| rtbfoods-consumer-testing | consumer testing |
| rtbfoods-eba-nigeria | eba consumer testing in Nigeria, lab trait BLUPs, pladmm and pltree with trait covariates |
| etnam-farmer-ratings | farmer Likert scores to paired comparisons and rankings, Plackett-Luce trees on gender and location, pladmm with station BLUPs, environmental data |
| tricot-rhomis | linking tricot and RHoMIS data |
| wageningen-genetic-gain-cassava | heritability, genetic gain, on-farm vs on-station |
| wageningen-genetic-gain-groundnut | heritability, genetic gain, on-farm vs on-station |
| worldveg-choose-growth-thrive | climate data and covariates |

## Original repositories and publications

Some projects have their own public repository, which is the reference version of their code and data. Cite the publication when you use these approaches.

| Folder | Publication | Original repository |
|---|---|---|
| occelli-tricot-pvs | Occelli et al. (2024), Agricultural Economics, https://doi.org/10.1111/agec.12819 | https://github.com/AgrDataSci/socioeco-tricot |
| potato-kang-et-al | Kang et al. (2025), Experimental Agriculture, https://doi.org/10.1017/S0014479725100100 | not yet public |
| (not archived here) | Brown et al. (2022), Crop Science, https://doi.org/10.1002/csc2.20817 | https://github.com/AgrDataSci/Data_synthesis_bean_tricot_CA |
| musa-blsd-data-synthesis | Brown et al. (2023), Agronomy Journal, https://doi.org/10.1002/agj2.21436 | https://github.com/AgrDataSci/Data_Synthesis_Musa_Trials_BLSD |
| cgiar-csi-2020 | webinar, CGIAR CSI Annual Meeting 2020 | https://github.com/AgrDataSci/cgiar-csi-2020 |
| etnam-farmer-ratings | Gesesse et al. (2023), PNAS, https://doi.org/10.1073/pnas.2205774119 | https://github.com/AgrDataSci/teff-breeding-elife (mislabelled as the teff paper) |
