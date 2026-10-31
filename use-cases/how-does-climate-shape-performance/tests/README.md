# Tests

Run from the use-case folder with `Rscript tests/run-tests.R`.

- The phenology window matches the simulated flowering date, and cooler plots have longer windows.
- Phenology must be provided, not guessed.
- Plots that do not reach the stage get no window, with a warning.
- Window indices reproduce the simulated heat.
- The tree splits on heat and not on rainfall.
- Window sensitivity returns one row per window.
- Unsupported spatial extrapolation is flagged.
