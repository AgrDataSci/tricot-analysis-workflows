# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

- Station heritability is within 0.1 of the simulated value, averaged over five simulated trials.
- BLUPs track the simulated genetic values.
- Agreement is detected when farmer preference follows genetic value.
- No agreement is claimed when preference is unrelated to genetic value.
- On-farm reliability is high with many participants and lower with few.
- Mismatched genotype names are reported.
