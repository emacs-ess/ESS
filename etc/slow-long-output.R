###  Example modified from Paul Johnson's post to ESS-help, Nov.25, 2013
op <- options("max.print")

##' construct (random) character vector of length n,  of about 10 characters each
mkCh <- function(n) {
    if(getOption("max.print") <= n) options(max.print = n + 1) ## <-
    if(n == 10) stop("error's message with apostrophe - work's in developer mode")
    N <- pmin(26,pmax(1, rpois(n, 10)))
    cbind(vapply(N, function(m) paste(sample(letters, m), collapse=""), ""))
}

set.seed(1)
y <- mkCh(9999)
system.time(print(y))
## R CMD BATCH slow-long-output.R && grep -A 1 -B 1 '^ *user *sys' slow-long-output.Rout 
##  user  system elapsed 
## 0.059   0.010   0.070  -- MM@lynne R CMD BATCH
##  user  system elapsed 
## 0.120   0.138   3.225 (+/- 0.2) -- MM@lynne ESS 13.09-1 [svn: 5930]
##---
## 0.124   0.097  52.540  -- MM@lynne ESS svn r 5909; incl font-locking, tracebug
## 0.128   0.080  57.305       ditto
## 0.139   0.088  54.770  -- sfs@lynne ESS 13.09 (emacs 24.3.1 GTK+)
## --
## 0.102   0.075   4.685  -- MM@lynne ESS 5.14 (emacs 24.3.1 incl font-lock!)
## 0.091   0.078   4.683  --     "    ESS 12.04-4 [rev. 4896 (2012-06-07)]
## 0.123   0.063   4.638  --     "    ESS 12.09-2 [rev. 5395 (2013-01-10)]
## 0.117   0.118   3.358  --     "    ESS 13.05   [rev. 5574 (2013-05-14)]
##                 ^^^^^ clearly a bit faster !

set.seed(23456)
y <- mkCh(50000)
system.time(print(y))
##  user   system  elapsed 
## 0.255    0.089    0.345  -- BATCH MM@lynne
## 0.685    0.369 1664.557  -- MM@lynne ESS svn r 5909 ...
## 0.760    0.398 1643.608  -- ..@lynne ESS 13.09
## 0.590    0.510   33.349  --     "    ESS 13.05   [rev. 5574 (2013-05-14)]
## 0.565    0.548   28.251	"	 "	
## 0.598    0.450   27.804 	"	 "


## Don't even think of trying this with ESS, currently:
y <- mkCh(200000)
system.time(print(y))
##  user  system elapsed 
## 1.013   0.284   1.300  -- BATCH MM@lynne

options(op)
