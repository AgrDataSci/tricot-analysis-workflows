# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

The R tests pass in the continuous integration of the repository. The same cases pass in the Python reference, run with `python3 script/other-language/run_checks.py` (42 checks on 2026-11-20). Tests on labels without meaning use a cut of 0.01 on the p value, since such a p value falls below 0.05 in one run out of twenty.

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
- Segments built from covariates that follow the true segments recover them, are stable and pass the fair test. Segments built from pure noise fail the fair test, although k-means splits noise in a stable way.
- Climate indices separate simulated wet and dry points, an incomplete year is left out, an index that does not vary is dropped and correlated indices are reported.
- The Czekanowski similarity of two samples from one population is below one and rises with n. Data sets from one population are as similar between as within, and data sets from different populations fall below their noise floor.
- Envirotypes recover the simulated zones. They pass the fair test when preference follows climate and fail it when it does not.
