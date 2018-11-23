context("completion.R")
test_that(".ess_fn_pkg", {
    expect_output(.ess_fn_pkg("eval"), "^base$")
    expect_output(.ess_fn_pkg("sqrt"), NA)
    expect_output(.ess_fn_pkg("adfljsdfl"), NA)
    expect_output(.ess_fn_pkg("history"), "^utils$")
})
