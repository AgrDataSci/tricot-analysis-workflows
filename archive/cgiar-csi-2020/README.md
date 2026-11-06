# cgiar-csi-2020

Scripts from the webinar "Machine learning with metadata and experimental citizen science in R", given by Kauê de Sousa at the CGIAR CSI Annual Meeting in Amsterdam, March 2020. The original repository, with the data, is https://github.com/AgrDataSci/cgiar-csi-2020.

Status is historical. `wheat.R` holds the first version of the idea that became Brown et al. (2023). Yields of 18 bread wheat varieties from 255 station trials across northern India, from different projects and years, cannot be compared on their original scale, so they are ranked within each trial with `gosset::rank_numeric()`, checked for network connectivity, modelled with Plackett-Luce, then linked to temperature indices from NASA POWER over the period from planting to flowering, and partitioned with a Plackett-Luce tree. `functions.R` holds the plotting helpers it sources. The curated version is `use-cases/can-heterogeneous-trials-be-combined/`.
