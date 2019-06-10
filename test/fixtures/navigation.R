fn1 <- function(a, b) something(aaa) +
                          aa / bb %>%
                          cc ## end of fn1



fn2 <- function(a, b)
    something(aaa) +
        aa / bb %>%
        cc ## end of fn2

### Some comments
## extra comments

## more comments

fn3 <- function() {
    ## comment

    a <- 1

    b <- 3

    a + b
    
} ## end of fn3

## some paragraph
par1 <- 1
par1 <- par1

setMethod("method1",
          function() {
          }) ## end of setMethod


fn4 <- function() {
    ## comment

    inner_fn5 <- function() {
        fn5_body
    }
    
    b <- 3

    a + b
    
} ## end of fn4


f5 <- function(){} ## end of f5

"after f5"

funcs <-
    list(f6 = function(){
        some_code6 <- here

    }, ## end of f6

    f7 = function(){

        some_code7 -> there
    }) ## end of f7


f8 <-
    function(aaa,
             bb = bbb(),
             cc = "cccc")
{
    Call <- match.call()
    miss.data <- missing(data) || !is.data.frame(data)

    ...
} ## end of f8

f9 <- function (x, ...) some_code


## navigation.R ends here
