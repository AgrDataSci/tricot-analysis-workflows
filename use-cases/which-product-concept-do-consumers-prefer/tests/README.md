# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

- Attribute effects recover the simulated values.
- The attribute table is matched by name, so shuffling its rows does not change the result, and missing concepts stop the analysis.
- The additive model is not rejected when the truth is additive.
- A missing interaction is detected by the adequacy test.
- Group differences in price sensitivity are detected.
- Predictions outside the tested levels are refused.
