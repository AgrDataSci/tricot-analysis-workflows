# Data

`simulate-data.R` generates 800 participants in two segments, market-oriented (40%) and subsistence-oriented (60%). Segments differ, with overlap, in farm size, share of harvest sold and distance to market. Household head is unrelated to segment. Each segment ranks eight varieties with its own Plackett-Luce worth, and the two worth vectors are reversed. With `heterogeneity = FALSE` both segments share the same worth. The true segment is stored in `attr(dat, "segment")`.
