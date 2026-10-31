# Data

`simulate-data.R` generates 900 consumers ranking three of 12 concepts each. Concepts combine price (1 to 3), cooking time (short, long), packaging (sack, branded) and biofortification (no, yes). True log-worth is −0.6 per price step, −0.8 for long cooking, +0.4 for branded packaging and +0.5 for biofortification. With `segments = TRUE` urban consumers have a price effect of −0.9 and rural consumers −0.45. With `interaction = TRUE` branded packaging helps short-cooking concepts and hurts long-cooking ones, which the additive model cannot represent.
