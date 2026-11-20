# Data

`script/helper/simulate-data.R` generates on-farm trials with a known answer. Six trials of 150 farms each grow three of eight varieties per farm. The true gains over the check are 40% (Variety 01), 25% (Variety 02), 10% (Variety 03), 3%, 0%, -3% and -10%. Log yield adds a farm effect (SD 0.6), a variety-by-trial interaction (SD 0.05) and a residual (SD 0.35). Five percent of plots fail and yield zero, whatever the variety. Farmers rank their three plots with a utility that follows the true gain plus Gumbel noise. The true gains and reliabilities are in `attr(dat, "truth")`.

To use your own data, place it in this folder, document it here (variables, units, source and licence), and replace the simulation call in `script/01-analysis.R` with a call that reads it. The workflow needs one row per plot with `trial`, `farm`, `genotype` and `yield`, and `rank` for the preference side.

The worked example reads open data from [tricot-data](https://github.com/AgrDataSci/tricot-data) and keeps no copy here. Run `python3 script/other-language/example-beans-tanzania.py <path to a clone of tricot-data>`.
