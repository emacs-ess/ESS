context("completion.R")
test_that(".ess_fn_pkg", {
    library(devtools)
    expect_output(.ess_fn_pkg("eval"), "^base$")
    expect_output(.ess_fn_pkg("sqrt"), NA)
    expect_output(.ess_fn_pkg("adfljsdfl"), NA)
    expect_output(.ess_fn_pkg("use_testthat"), "^devtools$")
})
