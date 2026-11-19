# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

The R tests have not been run yet. The same cases pass in the Python reference, run with `python3 script/other-language/run_checks.py` (16 checks on 2026-11-20).

- The gain is recovered within 0.03 in a large network without variety-by-trial interaction.
- The 95% interval covers the simulated gain in at least 85% of 40 simulated networks (0.94 in the Python run).
- The 40% variety is supported, varieties close to the check are not, and the variety on the 25% threshold is not called supported.
- Reliability is 0.5 when the mean gain equals the threshold, lower at the threshold than at zero, and lower for a less stable variety.
- The model-based reliability is within 0.08 of the simulated value, and the direct win share agrees with it in a large network.
- A trial without the check is skipped and named.
- Zero yields are counted and reported.
- Fewer than 30 plots gives "insufficient data".
- A weaker check raises the gain.
- The preferred variety is ranked above the check more often than not.
