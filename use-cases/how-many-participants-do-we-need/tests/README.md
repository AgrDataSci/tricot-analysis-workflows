# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

- Simulated rankings give each participant three ranked items.
- Simulated rankings recover the log-worths they were drawn from.
- More participants give a higher Kendall tau and a higher chance of finding the best item.
- The same seed gives the same curve.
- The target number of participants is read from the curve, and is NA when out of reach.
- Subsampling the real answers overstates accuracy compared with simulating new participants.
