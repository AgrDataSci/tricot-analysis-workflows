# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

The R tests have not been run yet. The same cases pass in the Python reference, run with `python3 script/other-language/run_checks.py` (24 checks on 2026-11-20).

- The model recovers the simulated log-worths within 0.1 in a large sample.
- Inconsistent and missing answers are dropped and counted.
- Repeatability rises with the number of participants.
- No difference between items gives zero, large differences with small variances give a value near one, and the Cullis form has a ceiling near 1 - 1/k with few items.
- True segments differ in preference and are more repeatable than random segments of the same sizes. A meaningless label is neither.
- Segments with identical preferences are not more repeatable than random ones.
- Random segments are less repeatable than the pooled data, the cost of a smaller sample.
- At the same number of participants, true segments beat the pooled data, and a meaningless label does not on average.
- True segments predict held-out rankings better, a meaningless label does not, and blocked folds are used when blocks are given.
- Single answers are far harder to predict than the item means are to repeat.
- Reliability against a reference item changes sides between segments.
- Segments built from covariates that follow the true segments recover them, are stable and pass the fair test. A partition of pure noise is unstable.
- Climate indices separate simulated wet and dry points, an incomplete year is left out, an index that does not vary is dropped and correlated indices are reported.
- Envirotypes recover the simulated zones. They pass the fair test when preference follows climate and fail it when it does not.
