#### File showing off  things that go wrong or *went* wrong in the past
#### -- with R-mode (mostly coded in ../lisp/ess-mode.el )

### -------- 1 ---------  extraneous comment chars :  This seems fixed

## From: Robert Gentleman <rgentlem@fhcrc.org>
## To: Martin Maechler <maechler@stat.math.ethz.ch>
## Subject: ESS buglet
## Date: Sun, 01 Jul 2007 21:41:24 -0700

## Hi Martin,
##   It seems that the following buglet exists (at least in what ever
## version I am using)

##a silly comment
##and a second one
foo <- function(x=a, abc = list("def", a=1,3,3), more.args, and, bla,
                blu, bl,
                another, plus, yet.another, and_mbasd,
                lots = NULL,
                more = NULL,
                args = NULL) {
    x
}

##-   when the line before a function def is a comment, and adding args,
##- then new lines, when generated have a comment char at the beginning of
##- the line. It is slighly annoying as I have to remove the comment char.
##-
##- If I add a blank line after the comment line, then the problem does not
##- occur.
## and another ''anonymous'' function:
function(x=a, abc = list("def", a=c(1,3,3)), more.args, and, bla, blu,
         blo, Abc,
         def,
         another, and_another, and_this) {
    ...; ...
}

## This is a "TRUE" example (from Matrix/tests/ ):
NA.or.True <- function(x) is.na(x) | x

abc <- function(x, y, ...) this.is.just.a.one.liner(x,y, z=TRUE, ...)

## A more-liner function with no "{...}" -- this one even works (but not all!)
mindiff <- function(df) df[which.min(df$diff),
                           which.max(df$daff)]

## Two functions in one line - can I "send" just one of them? {no, not "simply"}
f1 <- function(x) be.friendly(x, force=TRUE); f2 <- function(x,y) x*sin(pi*x)

### --- Suggestion (Jenny Brian): --> Create a (defun ess-eval-multiline .)
### Here is useful valid R "test code":

## From 'example(plot.default)' :

Speed <- cars$speed
Distance <- cars$dist
plot(Speed, Distance, panel.first = grid(8,8),
     pch = 0, cex = 1.2, col = "blue")
pp <- plot(Speed, Distance, panel.first = grid(8,8),
           pch = 0, cex = 1.2, col = "blue")
plot(Speed, Distance,
     panel.first = lines(lowess(Speed, Distance), lty = "dashed"),
     pch = 0, cex = 1.2, col = "blue")

## Note: We now at least C-c C-c {ess-eval-function-or-paragraph-and-step}



### Here, the indentation is wrong ... rather an Emacs buglet ?

a <- function(ch) {
    if(ch == Inf) {
	E.cond <- numeric(nb)
    }
    else {
	indic  <- ifelse(jinf+1 <= 1 & jsup >= 1,1,0)
	E.cond <- ch*(-pbinom(jinf,ni,prb) + 1-pbinom(js.n,ni,prb)) +
	    ifelse(ni == 1, prb*indic,
		   mu*(pbinom(js.n-1,pmax(ni-1,1),prb)-
		       pbinom(jinf-1,pmax(ni-1,1),prb))) / sV -
### why is the following line wrongly indented by Emacs/ESS ?
                           mu/sV*(pbinom(js.n,ni,prb) - pbinom(jinf,ni,prb))

	indic2 <- ifelse(jinf+1 <= 1 & jsup >= 1 & ni == 2,1,0)

    }
}


### Here, the beginning of function is not found correctly, and hence
###       all "ess-*-function" (C-M-a, C-M-e, ...) fail:

setMeneric <-
  ## It is clearly allowed to have comments here.
  ## S version 4, and John Chambers in particular like it.
  ##
  ## BUG: M-C-e or M-C-a fails from ``here'' --
  ## ---  effectively because of ess-beginning-of-function fails
  ## and that really relies on finding  ess-function-pattern;
  ## i.e., ess-R-function-pattern in ~/emacs/ess/lisp/ess-cust.el
  ##
    function(name, def = NULL, group = list(), valueClass = character(),
             where = topenv(parent.frame()), genericFunction = NULL)
{
    ## comments in here are at least kept via "source" attribute
    if(exists(name, "package:base") &&
       typeof(get(name, "package:base")) != "closure") {
        FALSE
    }
    "ABC"
}

## In one-liners without "{ ... }" body, the end-of-function is also
## not correctly found:
## Use C-M-e to see:  In these two, the "end-of-function" is after
## 'class' :
## ---- these all work now (ESS version 5.3.8) :
onelinerFails <- function(x, ...) class(x)

onelinerFailsToo <-
    function(x, ...)
    class(x)

onelinerWorks <- function(x, ...) { class(x) }

onelinerWorksToo <-
    function(x, ...) {
        class(x)
    }


## this has one line more before 'function' than "typically:"
setMethod("[", signature(x = "dgTMatrix", i = "numeric", j = "missing",
			 drop = "logical"),
	  function (x, i, j, ..., drop) { ## select rows
	      storage.mode(i) <- "integer"
              xi <- x@i + 1:1 # 1-indexing
              ## ...................
	      if (drop && any(nd == 1)) drop(as(x,"matrix")) else x
	  })

"dimnames<-.data.frame" <- function(x, value) {
    d <- dim(x)
    if(!is.list(value) || length(value) != 2
       || d[[1]] != length(value[[1]])
       || d[[2]] != length(value[[2]]))
	stop("invalid 'dimnames' given for data frame")
    row.names(x) <- as.character(value[[1]]) # checks validity
    names(x) <- as.character(value[[2]])
    x
}

'[.foo' <- function(x, i, value)
{
###
    y <- x
    y[i] <- value
    y
}

'[[.bar' <- function(x, i, value)
{
    ## bla bla bla
    y <- as.foo(x) ; y[[i]] <- value
    y
}

"[<-.foobar" <- function(x,i,j,value) {
    ## just something
    x
}

"names<-.foobar" <- function(x, value) {
    ## just something else
    x
}

`[<-.data.frame` <- function(x, i, j, value)
{
    nA <- nargs() # value is never missing, so 3 or 4.

###..........

    class(x) <- cl
    x
}

"[[<-.data.frame"<- function(x, i, j, value)
{
    cl <- oldClass(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[<-
    class(x) <- NULL

###...........

    class(x) <- cl
    x
}


"$<-.data.frame"<- function(x, i, value)
{
    cl <- oldClass(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[<-

###...........

    class(x) <- cl
    return(x)
}
