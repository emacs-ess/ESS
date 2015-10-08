
## inspired by:
## https://github.com/rstudio/rstudio/pull/191

## nested calls should work
df[a][, |]

## matrix, list, environments
l <- list(aaaa = 1111, bbbb = 2222)
l[a|] ## -> "aaaa" (quoted!)

## data table
dt <- data.table(aaaa = 1111, bbbb = 22222)
dt[, a|] # -> aaaa (unquoted!)

## attributes
x <- ""
attr(x, "foo") <- function(alpha, beta) {}
attr(x, "foo")(al| # -> "alpha"

## chains
mtcars %>% dplyr::select(mp| #-> mpg (unquoted))

## models
lm(mpg ~ cy| , data = mtcars) #-> cyl

## "by" keyword in data.table, inner_join,  etc
inner_join(foo, bar, by = c(| # provides completions for variables in foo when
                              # on the left side of an =, and bar when on the
                              # right side of an =.
