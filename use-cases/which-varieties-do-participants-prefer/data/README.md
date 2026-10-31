# Data

`simulate-data.R` generates 600 participants ranking three of 10 varieties each. Choices follow the Plackett-Luce model exactly (log-worth plus Gumbel noise). The check is a middle-of-the-range variety. The true log-worth relative to the check is stored in `attr(dat, "truth")`.
