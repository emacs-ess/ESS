#### File showing off  things that go wrong or *went* wrong in the past
#### -- with R-mode (mostly coded in ../lisp/ess-mode.el )

#### NOTE!!! this file is indented with RRR style !!!!!




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
##- the line. It is slightly annoying as I have to remove the comment char.
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

### --- 2 ----------------------------------------------------------------
### --- Suggestion (Jenny Brian): --> Create a (defun ess-eval-multiline .)
## Here is useful valid R "test code":

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

### --- 3 ----------------------------------------------------------------
###--- This one (from the Matrix package) is for testing ess-roxy...,
## i.e.,  C-c C-o

## not exported but used more than once for "dimnames<-" method :
## -- or do only once for all "Matrix" classes ??
dimnamesGets <- function (x, value) {
    d <- dim(x)
    if (!is.list(value) || length(value) != 2 ||
        !(is.null(v1 <- value[[1]]) || length(v1) == d[1]) ||
        !(is.null(v2 <- value[[2]]) || length(v2) == d[2]))
        stop(gettextf("invalid dimnames given for '%s' object", class(x)))
    x@Dimnames <- list(if(!is.null(v1)) as.character(v1),
                       if(!is.null(v2)) as.character(v2))
    x
}


### --- 4 ----------------------------------------------------------------
##  Here, the indentation is wrong ... rather an Emacs buglet ?

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


### --- 5 ----------------------------------------------------------------
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

### --- 6 ----------------------------------------------------------------
## In one-liners without "{ ... }" body, the end-of-function is also
## not correctly found:
## Use C-M-e to see:  In these two, the "end-of-function" is after
## 'class' :
## ---- these all work now (ESS version 5.3.8) :
## no it doesn't VS[10-03-2012|ESS 12.03]:
onelinerFails <- function(x, ...) class(x)

onelinerFailsToo <-
    function(x, ...)
    class(x)

onelinerWorks <- function(x, ...) { class(x) }

onelinerWorksToo <-
    function(x, ...) {
        class(x)
    }

### --- 7 ----------------------------------------------------------------
## idem:
## this has one line more before 'function' than "typically:"
setMethod("[", signature(x = "dgTMatrix", i = "numeric", j = "missing",
                         drop = "logical"),
	  function (x, i, j, ..., drop) { ## select rows
              storage.mode(i) <- "integer"
              xi <- x@i + 1:1 # 1-indexing
              ## ...................
              if (drop && any(nd == 1)) drop(as(x,"matrix")) else x
	  })

### --- 8 ----------------------------------------------------------------
## idem:
## all bellow are ok VS[10-03-2012|ESS 12.03]:
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


"$<-.data.frame" <- function(x, i, value)
{
    cl <- oldClass(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[<-

###...........

    class(x) <- cl
    return(x)
}

## swanky functions:
`swank:quit-inspector` <- function(slimeConnection, sldbState) {
    resetInspector(slimeConnection)
    FALSE
}

'swank:quit-inspector' <- function(slimeConnection, sldbState) {
    resetInspector(slimeConnection)
    FALSE
}


### --- 9 ----------------------------------------------------------------
## VS[03-2012|12.03]:FIXED:

## From: "Sebastian P. Luque" <spluque@gmail.com>
## To: ess-bugs@stat.math.ethz.ch
## Subject: [ESS-bugs] ess-mode 5.12; `ess-indent-line' error
## Date: Tue, 17 Aug 2010 13:08:25 -0500

## With the following input, and point on the line with "Table 8.3":
## it was the parenthetical expression at the beg of line

if (require(lme4)) {
    ## Model in p. 213
    (fm1 <- lmer(logFEV1 ~ age + log(height) + age0 + log(height0) + (age | id),
                 data=fev1, subset=logFEV1 > -0.5))
    ## Table 8.3
    VarCorr(fm1)$id * 100

    ## Model in p. 216
    (fm2 <- update(fm1, . ~ . - (age | id) + (log(height) | id)))
}

### -----
## hitting TAB (`ess-indent-command'), which calls `ess-indent-line' I get
## the following trace:

## ....: (scan-error "Containing expression ends prematurely" 20 20)
##   scan-sexps(177 -2)
##   forward-sexp(-2)
##   ...
##   ess-continued-statement-p()
## ......

## Interestingly, if the lines 2-4 are absent, then the problem is gone.
## The problem is also there in ESS 5.11.

## I'll try to find out what is going on in `ess-continued-statement-p' but
## given that I'm not very familiar with the stuff in ess-mode.el, I'm
## submitting the report in case somebody can detect the issue sooner.

## another example: hitting Tab at }else line
.essDev_differs <- function(f1, f2){
    if (is.function(f1) && is.function(f2)){
        !(identical(body(f1), body(f2)) && identical(args(f1), args(f2)))
    }else
        !identical(f1, f2)
}



### --- 10 ---------------------------------------------------------------
## indent at 0 after }else:
## VS:[03-2012|12.03]:FIXED:
if (is.function(f1) && is.function(f2)){
    !(identical(body(f1), body(f2)) && identical(args(f1), args(f2)))
}else
    !identical(f1, f2)


### --- 11 ---------------------------------------------------------------
##  --------------- C-c C-c  was finding the wrong "beginning of function"
##				[:FIXED:, 2011-05-28]
foobar <- function(...) {}
rm(list=ls())

##--------> consequence of the above experiments:
## the 2nd form is numerically "uniformly better" than the first
##--------> 2011-05-27:  Change Frank's psiInv() to
## psiInv = function(t,theta)
##     -log1p(exp(-theta)*expm1((1-t)*theta)/expm1(-theta))

### --- 12 ---------------------------------------------------------------
##--- In the following block, in the first line, C-c C-c does *NOT* behave
## VS[10-03-2012|ESS 12.03]: works fine for me:
th <- 48 # now do ls() and see what happened ... the horror !!!
d <- 3
cpF <- list("Frank", list(th, 1:d))
cop <- acF <- cpF$copula

### --- 13 ---------------------------------------------------------------
## VS[05-05-2012|ESS 12.04]: looks like :FIXED:

## From: Aleksandar Blagotic <aca.blagotic@gmail.com>
## To: <ess-help@stat.math.ethz.ch>
## Subject: [ESS] R-mode: forward-sexp: Scan error: "Unbalanced parentheses"
## Date: Tue, 6 Dec 2011 01:24:11 +0100
                                        #
## Let's presuppose that I have a function like this:
                                        #
fn <- function(x, ...){
    re <- "^#{1,6} [[:print:]]+$"
    grepl(re, x, ...)
}
## As soon as I put my cursor at the end of the line with regexp, and
## press RET, I get this error:

## forward-sexp: Scan error: "Unbalanced parentheses"
##
##-------
## Rodney S: I can reproduce it ...
## Martin M: I can NOT reproduce it, neither with 'emacs -Q';
##	tried both ESS 5.14 and ESS from svn
## VS[03-2012|12.03]: Cannot reproduce it either, solved?


### --- 14 ---------------------------------------------------------------
## check the behavior of ess-arg-function-offset-new-line

a <- some.function(
    arg1,
    arg2)
##  ^--- RRR has ess-arg-function-offset-new-line (4)  ==> should indent here

a <- some.function(arg1,
                   arg2)
##                 ^--- indent here


### --- 15 --------------------------------------------------------------
## VS[05-05-2012|ESS 12.04]:FIXED:
## indentation of the 3rd line is wrong
for(s in seq(10, 50, len = 5))
    for(a in seq(.5, 1, len = 5))
        pt_dif_plot(s, a)
##      ^-- here

### --- 16 ----
## VS[05-05-2012|ESS 12.04]:FIXED:
## Gives error unbalanced para at else lines and indentation is wrong
## error: Point is not in a function according to 'ess-function-pattern'.
getOrCreateForm <- function(bindName, whereEnv)
    if(exists(bindName, envir = get(".forms", envir = whereEnv)))
        get(bindName, envir = whereEnv)
##      ^-- here
    else
        new("protoForm")
##      ^-- here

parentContainer <-
    if(is.null(.getPrototype(.Object@host))) emptyenv()
    else sdf

### --- 17 ---
## Indentation -----  "expression" is special
expremmion <- c(1, 3,
                9876)# was always ok
## Had wrong indentation here:
expression <- c(2343,
                23874, 239487)

## or here:
foo <- function(x) {
    expression <- c(2343,
                    23874, 239487)
    10 + expression
}

## Where as here, we *do* want the indentation to
## *NOT* go all the way to the right:

{
    my.long.Expression <- expression(
        x[a[j]] == exp(theta[1] + theta[2]^2),
        x[b[i]] == sin(theta[3] ~~ theta[4])
        )
    ausdruck <- expression
    my.long.Expr...... <- ausdruck(
        x[a[j]] == exp(theta[1] + theta[2]^2),
        )
}

## VS[18-08-2012]: redundant feature. This is a feature for long subexpressions
## imidiately folowing new line. Documented in ess-arg-function-offset-new-line

### --- 18 ---
##  M-C-a (beginning of function)
##  -----   anywhere inside the following function, M-C-a must go to beginning
Ops.x.x <- function(e1, e2)
{
    d <- dimCheck(e1,e2)
    if((dens1 <- extends(c1 <- class(e1), "denseMatrix")))
	gen1 <- extends(c1, "generalMatrix")
    if((dens2 <- extends(c2 <- class(e2), "denseMatrix")))
	gen2 <- extends(c2, "generalMatrix")
    if(dens1 && dens2) { ## both inherit from ddense*
	geM <- TRUE
	if(!gen1) {
	    if(!gen2) { ## consider preserving "triangular" / "symmetric"
		geM <- FALSE
		le <- prod(d)
		isPacked <- function(x) length(x@x) < le
            }
        }
	## now, in all cases @x should be matching & correct {only "uplo" part is used}
	r <- callGeneric(e1@x, e2@x)
	if(geM)
	    new(paste0(.M.kind(r), "geMatrix"), x = r, Dim = d, Dimnames = dimnames(e1))
	else
	    new(paste0(.M.kind(r), Mclass), x = r, Dim = d, .....)
    }
    else {
	r <- ....

	## criterion "2 * nnz(.) < ." as in sparseDefault() in Matrix()	 [./Matrix.R] :
	if(2 * nnzero(r, na.counted = TRUE) < prod(d))
	    as(r, "sparseMatrix") else r
    }
}


### --- 19 ---
## wrong indentation because of the regexp
parse_roc <- function(lines, match = "^\\s*#+\' ?") {
                                                       lines <- lines[str_detect(lines, match)]
                                                       if (length(lines) == 0) return(NULL)
}


### --- 20 ---
## wrong continuation indentation


{
  a <- ggplot(data = overtime.by.month,
              aes(x="",
                  y=Percent,
                  fill = Overtime)) +
                    geom_bar(width = 1) +
                      xlab('') +
                        ylab(sub.txt) +
                          labs(title = title.txt) +
                            facet_wrap(~Year.Month)
}

### --- 21 ---

## From: Marius Hofert <marius.hofert@math.ethz.ch>
##     Date: Fri, 15 Mar 2013 21:00:45 +0100 (32 minutes, 1 second ago)
## Hi,
## The following bug happens in ESS 12.09-2 [rev. 5395 (2013-01-10)]. Put the
## cursor in the line before the function head and hit C-c C-c. 

foo <- function(x)
    x # bar
x <- 1:10

## I'll see 
## > + >  [1]  1  2  3  4  5  6  7  8  9 10

foo <- function(x) x*x 
bar <- function(y) y 
## via C-c C-c leads to "Error: object 'bar' not found". 


### --- 22 ----
## in the beginning of buffer indenting the second line does give an error 
if (!grepl("#", x))
return(res)

