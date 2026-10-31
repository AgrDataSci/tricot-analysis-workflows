# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

- The adjusted Rand index is 1 for identical partitions and near 0 for random ones.
- The two simulated segments are recovered (k = 2, adjusted Rand index above 0.6) and preference heterogeneity is detected.
- A binary factor in the Gower distance hijacks the partition.
- No heterogeneity is detected when segments share preferences.
- Unstable cluster solutions are reported rather than forced.
- Cluster profiles cover numeric and categorical covariates.
