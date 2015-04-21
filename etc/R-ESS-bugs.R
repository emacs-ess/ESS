#### File showing off  things that go wrong or *went* wrong in the past
#### -- with R-mode (mostly coded in ../lisp/ess-mode.el )

### NOTE: this file is indented with RRR style !!!!!
### but do not change indentations anymore of anything in here:
### expressions are written as we *want* them, not as ESS currently puts them

options(keep.source = FALSE) # so we see R's deparse() + print() indentation


### --- 1 ---------  extraneous comment chars :  This seems fixed

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
### continued statements
a <- function(ch) {
    if(ch == Inf) {
        E.cond <- numeric(nb)
    }
    else {
        indic  <- ifelse(jinf+1 <= 1 & jsup >= 1,1,0)
        E.cond <- ch*(-pbinom(jinf,ni,prb) + 1-pbinom(js.n,ni,prb)) +
            ifelse(ni == 1, prb*indic,
                   mu*(pbinom(js.n-1,pmax(ni-1,1),prb) -
                           pbinom(jinf-1,pmax(ni-1,1),prb))) / sV -
###                        ^-- here
                               mu/sV*(pbinom(js.n,ni,prb) - pbinom(jinf,ni,prb))
###                            ^-- here
        indic2 <- ifelse(jinf+1 <= 1 & jsup >= 1 & ni == 2,1,0)
    }
}


### --- 5 ----------------------------------------------------------------
### The beginning of function is not found correctly, and hence
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
##                 ^--- here


### --- 15 --------------------------------------------------------------
## VS[05-05-2012|ESS 12.04]:FIXED:
## indentation of the 3rd line is wrong
for(s in seq(10, 50, len = 5))
    for(a in seq(.5, 1, len = 5))
        pt_dif_plot(s, a)
##      ^-- here

### --- 16 ----
## VS[05-05-2012|ESS 12.04]:FIXED:
## MM[2014-04-28]: added '}' before else (=> "{" after if(.))
## so parse(<file>) works at all!
## Gives error unbalanced para at else lines and indentation is wrong
## error: Point is not in a function according to 'ess-function-pattern'.
getOrCreateForm <- function(bindName, whereEnv)
    if(exists(bindName, envir = get(".forms", envir = whereEnv))) {
        get(bindName, envir = whereEnv)
###      ^-- here
    } else
        new("protoForm")
###     ^-- here



parentContainer <-
    if(is.null(.getPrototype(.Object@host))) { emptyenv()
                                           } else sdf
### ^-- } is here (broken!)

parentContainer <-
    if(is.null(.getPrototype(.Object@host))) emptyenv()
    else sdf
### ^-- here

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
## indentation with regexp (bug in ess-backward-to-noncomment)
parse_roc <- function(lines, match = "^\\s*+\' ?") {
    lines <- lines[str_detect(lines, match)]
    if (length(lines) == 0) return(NULL)
### ^-- here (2014-11: fixed)
}


### --- 20 ---
## continuation indentation must be consistent in/out {}:

{
    a <- ggplot(data = overtime.by.month,
                aes(x="", y=Percent, fill = Overtime)) +
        geom_bar(width = 1) +
            xlab('') +
                ylab(sub.txt) +
                    labs(title = title.txt) +
                        facet_wrap(~Year.Month)
}

a <- ggplot(data = overtime.by.month,
            aes(x="", y=Percent, fill = Overtime)) +
    geom_bar(width = 1) +
        xlab('') +
            ylab(sub.txt) +
                labs(title = title.txt) +
                    facet_wrap(~Year.Month)
###                 ^-- face_wrap must be here


### --- 20b ---
## From https://github.com/emacs-ess/ESS/issues/120

mean(rnorm(100, mean = runif(1, 1, 10)), na.rm =TRUE) +
    2
##  ^--- 2 is here

mean(rnorm(100, mean = runif(1, 1, 10)),
     na.rm =TRUE) +
    2
##  ^--- 2 is here

mean(rnorm(100,
           mean = runif(1, 1, 10)), na.rm=TRUE) +
    2
##  ^--- 2 is here

### --- 21 ---

## From: Marius Hofert <marius.hofert@math.ethz.ch>
##     Date: Fri, 15 Mar 2013 21:00:45 +0100
## Hi,
## The following bug happens in ESS 12.09-2 [rev. 5395 (2013-01-10)]. Put the
## cursor in the line before the function head and hit C-c C-c.

foo <- function(x)
    x # bar
x <- 1:10

## I'll see
## > + >  [1]  1  2  3  4  5  6  7  8  9 10
## ESS 15.03: Error in eval(expr,  ....  : object 'x' not found

foo <- function(x) x*x
bar <- function(y) y
## via C-c C-c leads to "Error: object 'bar' not found". -- fixed


### --- 22 ----
## wrong indentation because of # {was same reason as 19 - but that is fixed!}
if (!grepl("#", x))
return(res)

### --- 23 ----
### three ways to indent closing parent depending on context:
foo <-
    function_call(
        a,
        b,
        c
        )
###     ^-- ) is here

foo <- function_call(
    a,
    b,
    c
    )
### ^-- ) is here

foo <- function_call(a,
                     b,
                     c
                     )
###                  ^-- ) is here

### --- 24 ---
### shift comma in function calls

foo <- function_call(a
                   , b
                   , c
###                  ^-- c is here
                     )
###                  ^-- ) is here

### --- 25 ---
## if/else in function calls and nested

function_call(abc =
                  if (test)
                      do_something
                  else
                      do_something_else)

function_call(
    abc =
        if (test)
            do_something
        else
            do_something_else)


function_call(abc = if (test)
                        do_something
                    else
                        do_something_else)

## real example is smooth.spline() source code [still (2015-04-08) wrong / bug!]
ss <- function (x, all.knots, nknots, ...)
{
    if (all.knots) {
        if (!missing(nknots) && !is.null(nknots))
            warning("'all.knots' is TRUE; 'nknots' specification is disregarded")
        nknots <- nx
    } else if (is.null(nknots))         # <- for back compatibility
          nknots <- .nknots.smspl(nx)
      else {
### ^ want 'else' there
          if (is.function(nknots))
              nknots <- nknots(nx)
          else if (!is.numeric(nknots))
              stop("'nknots' must be numeric (in {1,..,n})")
          if (nknots < 1)
              stop("'nknots' must be at least 1")
          else if (nknots > nx)
              stop("cannot use more inner knots than unique 'x' values")
      }
### ^-- want '}' there
}

## "if" conditional is an exception of the continuation rules:
## Here, we do not want subsequently further indentation of the c1 || c2 || c3
## part:
t2 <- function(x) {
    if(long.expression.of.some.size(x, pi) ||
       another.longish.expression(sin(x)*exp(x)) ||
       a.third.condition.under.which.A.is.chosen)
###    ^-- here
        A
    else
        B
}


r <-
    (some.function (x, 2342)  +
         another.f (x^3) + sdfsdf - sdfsdf  +
             and(x) +  the(x) -  last(x)*part(3))


### --- 26 ----
## This is formally correct R, though help(parse) mentions the line-length limit of
##  4095 __when reading from the console__
## ESS gives syntax errors ("Error: unexpected ','" ...) when evaluating this
## because line length >= 4096 :
##
x <- c(1, 3.075819, 1.515999, 2.156169, 1.480742, 1.765485, 1.460206, 1.603707, 1.427429, 1.504712, 1.334528, 1.48297,  1.355308, 1.383867, 1.319241, 1.36065,  1.307467, 1.365596, 1.255259, 1.352741, 1.239381, 3.15342, 1.799889, 2.258497, 1.688312, 1.906779, 1.548203, 1.724785, 1.500873, 1.573442, 1.417137, 1.540805, 1.395945, 1.472596, 1.394247, 1.377487, 1.337394, 1.369354, 1.333378, 1.3181, 1.313813, 1.315528, 2.12777, 2.718898, 1.993509, 2.220433, 1.820585, 1.97782, 1.672455, 1.770151, 1.587478, 1.685352, 1.539295, 1.584536, 1.499487, 1.50702, 1.41952, 1.449058, 1.393042, 1.432999, 1.369964, 1.400997, 1.333824, 2.950549, 2.145387, 2.382224, 1.927077, 2.032489, 1.8371, 1.877833, 1.710891, 1.756053, 1.620778, 1.657761, 1.558978, 1.56257, 1.508633, 1.534406, 1.46709, 1.468734, 1.432529, 1.455283, 1.386975, 1.417532, 2.229573, 2.494447, 2.016117, 2.190061, 1.877996, 1.978964, 1.767284, 1.836948, 1.677372, 1.743316, 1.616383, 1.655964, 1.55484, 1.594831, 1.502185, 1.543723, 1.467005, 1.491123, 1.44402, 1.446915, 1.401578, 2.580264, 2.109121, 2.240741, 1.944719, 2.043397, 1.821808, 1.89725, 1.748788, 1.786988, 1.659333, 1.697012, 1.610622, 1.616503, 1.538529, 1.562024, 1.499964, 1.529344, 1.474519, 1.483264, 1.441552, 1.434448, 2.165233, 2.320281, 2.007836, 2.086471, 1.884052, 1.950563, 1.76926, 1.843328, 1.708941, 1.741039, 1.627206, 1.644755, 1.580563, 1.593402, 1.527312, 1.568418, 1.501462, 1.502542, 1.464583, 1.467921, 1.431141, 2.340443, 2.048262, 2.161097, 1.926082, 1.995422, 1.81446, 1.853165, 1.738533, 1.784456, 1.679444, 1.696463, 1.612931, 1.629483, 1.548186, 1.580026, 1.52198, 1.531111, 1.482914, 1.484824, 1.442726, 1.447838, 2.093386, 2.185793, 1.948989, 2.02804, 1.867137, 1.907732, 1.771923, 1.800413, 1.691612, 1.720603, 1.642705, 1.649769, 1.589028, 1.598955, 1.539759, 1.55096, 1.503965, 1.50703, 1.471349, 1.469791, 1.436959, 2.218315, 1.997369, 2.041128, 1.887059, 1.928524, 1.79626, 1.827538, 1.716748, 1.735696, 1.658329, 1.664211, 1.599286, 1.611511, 1.553925, 1.562637, 1.516805, 1.529894, 1.476064, 1.482474, 1.453253, 1.458467, 2.0247, 2.07899, 1.921976, 1.949376, 1.824629, 1.851671, 1.744713, 1.765647, 1.683525, 1.685592, 1.625113, 1.624961, 1.571921, 1.581223, 1.535257, 1.537464, 1.497165, 1.504879, 1.468682, 1.469319, 1.448344, 2.092315, 1.941412, 1.969843, 1.844093, 1.866133, 1.766145, 1.783829, 1.703613, 1.709714, 1.646078, 1.654264, 1.594523, 1.598488, 1.545105, 1.555356, 1.514627, 1.521353, 1.483958, 1.487677, 1.449191, 1.459721, 1.958987, 1.985144, 1.87739, 1.879643, 1.786823, 1.799642, 1.720015, 1.724688, 1.663539, 1.662997, 1.609267, 1.615124, 1.56746, 1.562026, 1.520586, 1.52503, 1.493008, 1.502496, 1.471983, 1.468546, 1.435064, 1.994706, 1.880348, 1.894254, 1.805827, 1.815965, 1.744296, 1.743389, 1.665481, 1.681644, 1.624466, 1.626109, 1.584028, 1.5818, 1.54376, 1.547237, 1.504878, 1.515087, 1.479032, 1.47936, 1.450758, 1.45073, 1.892685, 1.91087, 1.825301, 1.827176, 1.745363, 1.746115, 1.693373, 1.701692, 1.648247, 1.637112, 1.594648, 1.592013, 1.554849, 1.55013, 1.522186, 1.520901, 1.492606, 1.493072, 1.460868, 1.46733, 1.440956, 1.92771, 1.835696, 1.841979, 1.775991, 1.766092, 1.703807, 1.708791, 1.654985, 1.655917, 1.602388, 1.611867, 1.570765, 1.573368, 1.53419, 1.529033, 1.506767, 1.503596, 1.481126, 1.471806, 1.444917, 1.451682, 1.850262, 1.855034, 1.778997, 1.789995, 1.718871, 1.717326, 1.667357, 1.666291, 1.619743, 1.631475, 1.582624, 1.58766, 1.546302, 1.545063, 1.512222, 1.517888, 1.489127, 1.487271, 1.466722, 1.463618, 1.444137, 1.8709, 1.794033, 1.80121, 1.736376, 1.740201, 1.673776, 1.682541, 1.638153, 1.642294, 1.604417, 1.597721, 1.559534, 1.559108, 1.533942, 1.529348, 1.499517, 1.501586, 1.473147, 1.473031, 1.457615, 1.452348, 1.805753, 1.812952, 1.746549, 1.747222, 1.696924, 1.694957, 1.652157, 1.650568, 1.607807, 1.613666, 1.577295, 1.570712, 1.543704, 1.538272, 1.515369, 1.517113, 1.487451, 1.491593, 1.464514, 1.464658, 1.439359, 1.823222, 1.758781, 1.767358, 1.70872, 1.712926, 1.666956, 1.667838, 1.62077, 1.621445, 1.592891, 1.58549, 1.55603, 1.559042, 1.521501, 1.523342, 2, 3, 4)

### --- 27 ----
## Indentation after open brace
.a.lst <-
    list(ex1 = function(p) {
             cMah <- qchisq(0.975, p)
             function(d) as.numeric(d < cMah)
###          ^--- here
         },
         ex2 = function(p) {
             cM <- qchisq(0.95, p)
             function(d) as.numeric(d < cM)
###          ^--- here
         })
###      ^--- here


.a.lst <- list(ex1 = function(p) {
                   cMah <- qchisq(0.975, p)
###                ^--- here
               },
               ex2 = function(p) {
                   cM <- qchisq(0.95, p)
                   function(d) as.numeric(d < cM)
###                ^--- here
               })

.a.lst <- list(list(aa = {
                        bbb
###                     ^--- here
                    },
                    aaa = function(p) {
                        qchisq(0.95, p)
###                     ^--- here
                    },
                    aaaa = {
                        cccc
###                     ^--- here
                    }))

list(function(p){
         abc
###      ^-- here
     })
###  ^-- here

(ab) {
    sfdsf
### ^-- here
}

### --- 27b --- [new, 2015-04-09]
print.MethodsFunction <- function(x, byclass = attr(x, "byclass"), ...)
{
    info <- attr(x, "info")
    values <- if (byclass) {
        unique(info$generic)
    } else {
        visible <- ifelse(info$visible, "", "*")
        paste0(rownames(info), visible)
###     ^-- both lines above should start here
    }
### ^-- "}" here

    ## 2nd version:
    val <-
        if (byclass) {
            unique(info$generic)
        } else {
            visible <- ifelse(info$visible, "", "*")
            paste0(rownames(info), visible)
###         ^-- both lines above should start here
        }
###     ^-- "}" here
    invisible(x)
}



### --- 28 --- [still unfixed, 2015-04-08]
## Indentation of end-line comments (to column 40 = 'indent-column')
## {this is part of "real" code in Rmpfr/R/hjk.R}:
hjk <- function(x,n) { # <--- C-M-q  "on {" -- does *no longer* indent the "# .."
    ##-- Setting steps and stepsize -----
    nsteps <- floor(log2(1/tol))	# number of steps
    steps  <- 2^c(-(0:(nsteps-1))) # decreasing step size
    dir <- diag(1, n, n) # orthogonal directions

    x <- par    # start point
    fx <- f(x)    # smallest value so far
    fcount <- 1     # counts number of function calls

    if (info) cat(sprintf("step  nofc %-12s | %20s\n",
                          "fmin", "xpar"))

    ##-- Start the main loop ------------
    ns <- 0
    while (ns < nsteps && fcount < maxfeval && abs(fx) < target) {
        ns <- ns + 1
        hjs    <- .hjsearch(x, f, steps[ns], dir, fcount, maxfeval, target)
    }
    hjs
}

### --- 29 ---
foreach(a = 1:3) %do% {
    a^2
### ^--- here
}

foreach(a = 1:3) %:%
    foreach(b = 10:13) %dopar% {
### ^--- here
        a + b
###     ^---- here
    }
### ^--- here

read.csv('file.csv') %>%
    mutate(X = X+2, Y = Y/2) %>%
### ^--- here
        filter(X < 5)
###     ^-- here



### Local Variables:
### page-delimiter: "^### --- [1-9]"
### End:
