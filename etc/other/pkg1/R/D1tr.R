D1tr <- function(y, x = 1)
{
    ## Purpose:  discrete trivial estimate of 1st derivative.
    ## ----------------------------------------------------------------------
    ## Arguments: x is optional; let's say we don't like "'"
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, ~ 1990
    n <- length(y)

    if(length(x) == 1)
        c(y[2] - y[1], 0.5 * (y[-(1:2)] - y[-((n-1):n)]), y[n] - y[n-1])/x
    else {
        ## Here, already using non-matching apostrophes, but developer mode
        ## still seems to work ..
        if(n != length(x)) stop("lengths' of x & 'y' must equal")
        if(is.unsorted(x)) stop("'x' must be sorted !")
        c(y[2] - y[1], 0.5 * (y[-(1:2)] - y[-((n-1):n)]), y[n] - y[n-1]) /
            c(x[2] - x[1], 0.5 * (x[-(1:2)] - x[-((n-1):n)]), x[n] - x[n-1])
    }
}
