# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`. They also run on GitHub Actions at every push.

- Row-level tau averages to the `gosset::kendallTau()` estimate.
- Partial correlations match the closed form for three variables.
- The known drivers (yield, taste) are recovered and the confounded trait (colour) is dropped.
- Disconnected rankings are flagged.
- Invalid best/worst encoding (best equal to worst, letters outside A, B, C, missing) is dropped and counted.
- Traits never ranked by the same participants stop the analysis with a clear error.
