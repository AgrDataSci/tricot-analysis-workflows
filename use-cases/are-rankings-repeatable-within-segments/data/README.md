# Data

`script/helper/simulate-data.R` generates tricot rankings with a known answer. 1,200 participants each see three of eight concepts and choose the best and the worst. Two segments of equal size hold opposite preferences, "market" for concepts C1 to C3 and "home" for C6 to C8, on top of a common preference. A second label, "noise", is unrelated to preference. The true log-worths are in `attr(dat, "worth")`. Setting `contrast = 0` makes the two segments identical.

To use your own data, place it in this folder, document it here (variables, units, source and licence), and replace the simulation call in `script/01-analysis.R` with a call that reads it. The workflow needs one row per participant with the three items shown, the position of the best and the worst, and the segment labels.

The worked example reads open data from [maize-concept-testing](https://github.com/AgrDataSci/maize-concept-testing) (https://doi.org/10.5281/zenodo.15063837, CC BY 4.0) and keeps no copy here. Run `python3 script/other-language/example-maize-concepts.py <path to a clone of maize-concept-testing>`.
