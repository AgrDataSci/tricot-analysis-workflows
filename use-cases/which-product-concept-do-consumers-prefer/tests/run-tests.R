# Run the tests of this use case from the use-case folder
#   Rscript tests/run-tests.R

source("functions.R")
source("data/simulate-data.R")

res = testthat::test_file("tests/test-which-product-concept-do-consumers-prefer.R",
                          reporter = "summary", stop_on_failure = TRUE)
