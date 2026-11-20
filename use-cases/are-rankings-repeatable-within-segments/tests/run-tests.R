# Run the tests of this use case from the use-case folder
#   Rscript tests/run-tests.R

library("PlackettLuce")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")
source("script/helper/envirotypes.R")

res = testthat::test_file("tests/test-are-rankings-repeatable-within-segments.R",
                          reporter = "summary", stop_on_failure = TRUE)
