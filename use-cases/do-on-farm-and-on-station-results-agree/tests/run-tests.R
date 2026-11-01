# Run the tests of this use case from the use-case folder
#   Rscript tests/run-tests.R

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

res = testthat::test_file("tests/test-do-on-farm-and-on-station-results-agree.R",
                          reporter = "summary", stop_on_failure = TRUE)
