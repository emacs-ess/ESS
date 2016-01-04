
### TESTS
.ess_lang2list <- function(expr){
    if(is.symbol(expr))
        expr
    else
        lapply(as.list(expr), .ess_lang2list)
}

.ess_in_fun_call <- function(expr){
    path <- .ess_cursor_path(expr)
    !is.symbol(.ess_get_in(expr, path))
}

.strexpr <- function(text){
    expr <- parse(text = text, keep.source = F)
    str(.ess_lang2list(expr))
    invisible(expr)
}

text <- "df[a][, { (._.) }, data=bla]"
.strexpr("df[a][, { (._.) }, data=bla]"w)
.strexpr("df[a][, dd][,  ._.]")
.strexpr("lm(y ~  ._., data=bla)")
.strexpr("lm(y ~  I(._.), data=bla)")
.strexpr("df %>% select(foo, bar) %>% group_by(._.)")
.strexpr("graphics::plot(._.)")
.strexpr("plot(._.)")
.strexpr("base::aaa$bbb(._.)")
.strexpr("base::cat((._.))")
.strexpr("mtcars[ (._.)]")
.strexpr("mtcars[, (._.)]")
.strexpr("mtcars[aa, (._.)]")
.ess_cursor_path(.strexpr("mtcars[(._.), ]"))
expr <- .strexpr("lm(mpg ~ di(._.), data=mtcars)")
.ess_cursor_path(expr)
.ess_cursor_path(.strexpr("base::cat((._.))"))
.ess_cursor_path(.strexpr("base::cat(._.)"))

## ## exp <- .strexpr("base::strptime$sbs((._.))")
tt <- list(aaaaa = 23, bbbb = 34, ccccc = function(a = 2, b = 3){})

str(.ess_complete("ca(._.)"))
str(.ess_complete("base::ca(._.)"))
str(.ess_complete("base::cat(._.)"))
str(.ess_complete("base::cat((._.))"))
str(.ess_complete("cat((._.))"))
str(.ess_complete("tt$a(._.)"))
str(.ess_complete("tt$z(._.)"))
str(.ess_complete("tt$ccccc((._.))"))
str(.ess_complete("lm(mpg ~ di(._.), data=mtcars)"))
str(.ess_complete("lm(mpg ~ ls((._.)), data=mtcars)"))
str(.ess_complete("mtcars[, (._.)]"))
str(.ess_complete("mtcars[(._.), ]"))

.strexpr("with(mtcars, { (._.) })")
str(.ess_complete("with(mtcars, { (._.) })"))

library(data.table)
dt <- as.data.table(mtcars)
.strexpr(text <- "dt[, { (._.) }]")
str(.ess_complete(text))
