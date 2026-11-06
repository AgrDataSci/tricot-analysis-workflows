# EtNAM farmer ratings

Scripts of Gesesse et al. (2023), Genomics-driven breeding for local adaptation of durum wheat is enhanced by farmers' traditional knowledge, PNAS 120(14), e2205774119, https://doi.org/10.1073/pnas.2205774119. Farmer scripts by Kauê de Sousa. The scripts were kept in the repository https://github.com/AgrDataSci/teff-breeding-elife, whose README cites the teff paper of Woldeyohannes et al. (2022, eLife, https://doi.org/10.7554/eLife.80009) by mistake. The data files and the genotype families (N1 to N51, reference RF, locations Adet, Kulumsa and Geregera) are the Ethiopian durum wheat NAM population of the PNAS paper. The teff paper used the same method on the Ethiopian Teff Diversity Panel, and its scripts have not been located (6 November 2026).

Status is historical. The curated version is `use-cases/can-farmer-ratings-be-analysed-as-rankings/`, and the item-covariate part (station BLUPs explaining worth with pladmm) is in `use-cases/which-measured-traits-explain-preference/`.

What each script did

- `01_farmers_oa.R` reshapes farmers' overall appreciation scores (Likert 1 to 5, one score per plot per farmer) into one record per farmer, replicate and location, aggregates genotypes to their NAM family because PlackettLuce could not handle the full set of lines, builds all pairwise comparisons per farmer with ties set to NA, converts them to grouped rankings through `psychotools::paircomp()`, fits a Plackett-Luce model with reliability against the reference RF, Plackett-Luce trees on location and gender, and a tree with station BLUPs as item covariates (`pltree(..., worth = f)`).
- `02_get_environmental_data.R` retrieves climate data for the trial locations.
- `write_plots.R` draws the figures.
