## A Silly Example
## 14 October 1999

ajr.x <- rnorm(100)
ajr.y <- (5 * ajr.x) + rchisq(100)

ajr.test <- function(x,y) {
  z <- x-y
  w <- x+y
  corr(z,w)
}


lm(ajr.y ~ ajr.x)


ajr.test(ajr.x,ajr.y)

