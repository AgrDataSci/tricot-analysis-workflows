# Musa black leaf streak disease data synthesis

Scripts of Brown et al. (2023), Rank-based data synthesis of heterogeneous trials to identify the effects of climatic factors on the reaction of Musa genotypes to black leaf streak disease, Agronomy Journal 115, 2990–3009, https://doi.org/10.1002/agj2.21436. The reference version of the code and data is the original repository, https://github.com/AgrDataSci/Data_Synthesis_Musa_Trials_BLSD. Scripts by David Brown with Kauê de Sousa.

Status is historical. The scripts are kept as written, with hard-coded paths and some unfinished exploration, so the decisions behind the use case can be traced. The curated version is `use-cases/can-heterogeneous-trials-be-combined/`.

What each part did

- `01_` to `06_` data preparation, one script per source (IMTP 1 to 3 from AgTrials, the Narváez thesis, MGIS, the BBB project from MusaBase). Each selects the metric with the most data, ranks genotypes within blocks or trial means with `gosset::rank_numeric()` in the direction of the metric, and sets the evaluation period from planting to shooting or harvest, with ratoon cycles starting where the previous cycle ended.
- `utils/aggregate_musa_trials.R` binds the rankings into one sparse matrix and the evaluation periods into one table.
- `07_get_climate_data.R` extracts AgERA5 rainfall, temperature and relative humidity over each evaluation period with ag5Tools and computes climatrends indices.
- `08_pltree_model.R` removes near-zero variance covariates, checks connectivity, resolves ties, keeps genotypes present in at least 5% of the rankings, fits the Plackett-Luce tree, assesses stability with stablelearner and computes reliability against Calcutta 4.
- `08_pltree_pladmm_model.R` is a first attempt to explain worth within nodes by genotype status and breeding programme with `PlackettLuce::pladmm()`.
- `gof_musa_trials.R` goodness of fit under resampling.
- `misc/`, `utils/`, `old_scripts/` and `test_dir/` hold maps, network plots, earlier models (Bradley-Terry trees, mixed models, random forests) and earlier climate extraction (CHIRPS, CHIRTS, MODIS, AgERA5).
