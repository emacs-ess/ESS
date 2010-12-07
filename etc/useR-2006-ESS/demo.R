 

## M-x load-library allout
## Set-header-lead to be "###"
## play with exposure.
## Navigation:

###* This is first example

my.test.function <- function(x,y) {
  this <- x
  that <- function(z) {
    print(z)
  }
  print(this)
  print(that(y))
}

###* This is second example

x <- rnorm(10)

y <- 4 * x + runif(10)

lm(y ~ x)

###** There are others

my.lm.obj <- lm(y ~ x)
summary(my.lm        ) ## object completion


###** Example for second level folding

###* Back to the top level.



