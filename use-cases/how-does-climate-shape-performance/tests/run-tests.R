# Run the tests of this use case from the use-case folder
#   Rscript tests/run-tests.R

# pltree() looks up its fitting functions on the search path, so
# PlackettLuce must be attached, as in analysis.R
library("PlackettLuce")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

res = testthat::test_file("tests/test-how-does-climate-shape-performance.R",
                          reporter = "summary", stop_on_failure = TRUE)
