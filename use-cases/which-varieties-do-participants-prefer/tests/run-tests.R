# Run the tests of this use case from the use-case folder
#   Rscript tests/run-tests.R

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

res = testthat::test_file("tests/test-which-varieties-do-participants-prefer.R",
                          reporter = "summary", stop_on_failure = TRUE)
