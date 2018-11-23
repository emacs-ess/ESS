source('../etc/ESSR/R/.load.R')
load.ESSR('../etc/ESSR/R')
testthat::test_dir("ESSR", stop_on_failure=TRUE)
