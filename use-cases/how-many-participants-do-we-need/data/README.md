# Data

`script/helper/simulate-data.R` generates a trial with 12 items in which 93 participants registered and 35 returned their overall ranking. Answers follow the Plackett-Luce model with log-worths from 0.8 to -0.8, stored in `attr(dat, "truth")`. Participants who did not answer have `NA` in `overall_pos` and `overall_neg`.

To use your own data, place it in this folder in the ClimMob format (`item_A`, `item_B`, `item_C`, best and worst as letters), document it here (variables, source and licence), and replace the simulation call in `script/01-analysis.R`. Keep the rows of participants who did not answer, so the script knows how many registered.
