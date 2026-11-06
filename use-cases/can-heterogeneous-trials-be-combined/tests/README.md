# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

- Trials without coordinates, dates or overlapping genotypes are excluded.
- The metric with the most data is chosen per trial, with the priority list as tie-breaker.
- Ranking direction follows the metric, and tied values share a rank.
- Absent genotypes are zero in the rank matrix.
- Ties are resolved by keeping one genotype at random, and ranks stay consecutive.
- Disconnected and rare genotypes are removed until the network is strongly connected.
- A later crop cycle starts where the previous one ended.
- The climate indices reproduce the simulated maximum dry spell.
- Near-zero variance covariates are removed and strong correlations reported.
- Block-level trials give one ranking per block and cycle.
- The tree separates the dry evaluation periods on a rainfall index and never on temperature.
- The dry-sensitive genotypes lose worth under dry conditions.
- Split stability reports frequencies per variable and cut points.
- Reliability, evaluations per node and goodness of fit are reported.
