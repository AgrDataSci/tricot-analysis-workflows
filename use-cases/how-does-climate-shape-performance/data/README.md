# Data

`simulate-data.R` generates 900 plots along an elevation gradient, with one year of daily temperature and rainfall per plot and planting dates between 15 February and 27 March. Flowering happens when accumulated degree days (base 10 C) reach 900. Two varieties are heat tolerant. They are preferred when the highest day temperature between planting and flowering exceeds 35 C, and not preferred below it. Rainfall is generated independently and has no effect. The true flowering day and heat per plot are in `sim$truth`.
