# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

- Estimated log-worth recovers the simulated truth (correlation above 0.9).
- The true best variety beats the check and leads the top-3 probability.
- Intervals cover the true log-worth for most varieties.
- The model rejects equal preference.
- Disconnected rankings are flagged.
- Invalid best/worst entries are dropped and counted.
