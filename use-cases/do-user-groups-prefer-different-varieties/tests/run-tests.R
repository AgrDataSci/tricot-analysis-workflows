# Run the tests of this use case from the use-case folder
#   Rscript tests/run-tests.R

# VarSelLCM and FactoMineR return S4 objects and FAMD results whose methods
# are only found reliably when the packages are attached, as in 01-analysis.R
library("VarSelLCM")
library("FactoMineR")

source("script/helper/functions.R")
source("script/helper/simulate-data.R")

res = testthat::test_file("tests/test-do-user-groups-prefer-different-varieties.R",
                          reporter = "summary", stop_on_failure = TRUE)
