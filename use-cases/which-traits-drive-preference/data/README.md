# Data

`script/helper/simulate-data.R` generates synthetic tricot data with a known answer. There are 800 participants and 12 varieties, and each participant ranks three varieties for overall preference, yield, taste, colour and drought tolerance.

- Overall preference is driven by yield (weight 1) and taste (weight 0.6).
- Colour has no direct effect but is correlated with yield at variety level (0.8).
- Drought tolerance has no effect and is uncorrelated with yield and taste across varieties by construction.

The true variety values are stored in `attr(dat, "truth")`. Run `Rscript script/helper/simulate-data.R` from the use-case folder to write `data/synthetic-tricot.csv`.

To use your own data, place it in this folder, document it here (variables, units, source and licence), and replace the simulation call in `script/01-analysis.R` with a call that reads it.
