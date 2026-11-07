# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

- Tied pairs and distinct scores are counted per rater before conversion.
- Paired comparisons follow the score, ties are NA, and pairs are in the order `psychotools::paircomp()` expects.
- A rater with only ties is dropped, and the others become grouped rankings with one group per rater.
- Rankings with ties report the largest tie set per rater.
- Items aggregate to groups by the mean score per rater, and every item needs a group.
- Agreement between raters is computed on shared items.
- On the simulated five-point scores, tie sets exceed what Plackett-Luce rankings can hold.
- The Plackett-Luce model recovers the latent order, and the reference has reliability 0.5.
- The tree splits on gender and not on location, and worth per node follows the order each gender used.
- Aggregation to families keeps a usable model.
