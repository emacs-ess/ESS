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
###       all "ess-*-function" fail:

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
