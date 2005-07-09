#### File showing off  things that go wrong -- with R-mode


### Here, the indentation is wrong:

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
			   ## why is the following line wrongly indented by Emacs/ESS ?
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

"[<-.data.frame" <- function(x, i, j, value)
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
