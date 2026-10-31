# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

- Estimates are matched to genotypes by name, also when a genotype is missing for one trait.
- BLUEs recover the simulated genotype values.
- Low repeatability is reported, and BLUPs shrink that trait the most.
- Region confounded with genotype is reported.
- Derived texture traits (gumminess, chewiness) are flagged as collinear, and the component traits pass.
- pladmm recovers the direction of the trait effects, and the trait model is adequate.
- Shuffled covariate rows give the same model.
- Too many covariates for the number of genotypes stop the fit.
- Sensitivity to measurement error is summarized.
