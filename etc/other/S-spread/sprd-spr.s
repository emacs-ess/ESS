#-*-Fundamental-*-


# Spreadsheet written in S

# The spreadsheet may be called anything.
# References to cells in the spreadsheet must be called "x".

# Updating is in column order.

# Version 3 classes and methods technology.


as.spread <- function(x)
{
	if (is.spread(x)) return(x)
	x <- as.array(x)
	attr(x,"expr") <- as.expr(x, length=0)
	attr(x,"macro") <- as.expr(x, length=0)
	attr(x,"before") <- as.expr(x, length=0)
	attr(x,"after") <- as.expr(x, length=0)
	class(x) <- c("spread", class(x))
	x	
}

is.spread <- function(x)
	inherits(x,"spread")


print.spread <- function(x, ..., quote=F)
{
	if (inherits(x, "data.frame")) print.data.frame(x)
        else {
		class(x) <- class(x)[-match("spread",class(x))]	
		print.array(x, ..., quote=quote)
	}
	invisible(x)
}



"[.spread"<-
function(x, ..., drop = F)
{
# Note: We do not retain the spread class!
#       If we did, the subscripts on the expr() and macros() would be wrong
#
	NextMethod("[", drop=drop)
}


"[.expr" <- function(x, ... , drop=F)
{
# Note: We do retain the expr class.
#       The primary use is for printing, so we want the original subscripting.

	z <- NextMethod("[", drop=drop)
	class(z) <- class(x)
	z
}


update.spread <- function(object, ..., force=F)
{
	if (force) object <- eval.spread(object, NULL, force=force)
	if (length(before(object)))
		object <- eval.spread(object, before(object))
	if (length(expr(object)))
		object <- eval.spread(object, force=force)
	if (length(after(object)))
		object <- eval.spread(object, after(object))
	object
}

eval.spread <- function(object, e, force=F)
{
	x <- object
	class(x) <- class(x)[-match("spread",class(x))]
	if (force) {
		.Options$warn <- -1
		tmp <- as.numeric(as.matrix(x))
		if (!any(is.na(tmp))) x <- tmp
	}
	if (missing(e)) {
		if (inherits(x,"data.frame")) {
			e <- expr(object)
			if (force)
			  for (j in 1:ncol(x)) for (i in 1:nrow(x))
				x[[i,j]] <- eval(e[i,j])
			else
			  for (j in 1:ncol(x)) for (i in 1:nrow(x)) {
				eij <- e[i,j]
				if(is.language(eij)) x[[i,j]] <- eval(eij)
			  }
		}
		else {
			i <- 0
			if (force)
				for (ei in expr(object))
					{i <- i+1; x[i] <- eval(ei)}
			else
				for (ei in expr(object))
				{i <- i+1; if(is.language(ei)) x[i] <- eval(ei)}
		}
	}
	else eval(e)
	class(x) <- class(object)
	x
}

#usage: x <- macro.eval(x, i)
macro.eval <- function(object, i)
	eval.spread(object, macro(x)[i])


"[[<-.spread" <- function(...) do.call("[<-.spread", list(...))

"[<-.spread" <- function(object, ..., value)
{
	x <- object
	expr(x) <- expression()
	class(x) <- NULL
	e <- expr(object)
	l.e <- length(e)
	i.a.v <- is.atomic(substitute(value))
	n.cells <-  prod(dim(x[..., drop=F]))

	if (l.e == 0) {
		if (n.cells != 1 || i.a.v )
			x[...] <- eval(substitute(value))
		else {
			e <- as.expr(object)
			l.e <- length(e)
		}
	}
	if (l.e != 0) {
		if (n.cells != 1) {
			e.s.v  <- eval(substitute(value, sys.parent()))
			x[...] <- e.s.v
			e[...] <- e.s.v
		}
		else {
			e[[...]] <- substitute(value)
			x[[...]] <- eval(e[[...]])
		}
	}
	attributes(x) <- attributes(object)
	class(x) <- class(object)
	expr(x) <- e
	update.spread(x)
}


print.expr <- function(e, ..., replace.string=F) {
	replace <- as.logical(replace.string)
	if (length(e) == 0) {
		if (replace) cat(replace.string, "<- ")
		print(expression())
	}
	else if (is.null(dim(e))) {
		ne <- names(e)
		for (i in 1:length(e)) {
			nei <- index.value(ne, i)
			if (replace) cat(replace.string)
			cat(paste("[", nei, "] ", sep=""))
			if (replace) cat("<- expression(")
			cat(e[i])
			if (replace) cat(")")
			cat("\n")
		}
	}
	else {
		dn <- dimnames(e)
		if (is.null(dn)) dn <- list()
		for (i in 1:length(dim(e))) {
			if (is.null(dn[[i]])) dn[[i]] <- 1:dim(e)[i]
		}
		dnn <- outer(dn[[1]], dn[[2]], paste, sep=",")
		if (length(dn) > 2)
			for (i in 3:length(dn))
				dnn <- outer(dnn, dn[[i]], paste, sep=",")
		for (i in seq(length=length(e))) {
			if (replace) cat("x")
			cat(paste("[", dnn[i], "] ", sep=""))
			if (replace) cat("<-")
			cat(paste(" ", e[i], "\n", sep=""))
		}
	}
	invisible(e)
}

as.expr <- function(x, ...) UseMethod("as.expr")

as.expr.default <- function(x, length.x=prod(dim(x))) {
	e <- vector(mode="expression", length=length.x)
	x <- unclass(x)
	if (length.x > 0) {
		e <- array(e, dim(x), dimnames(x))
		e[] <- x[]
#		for (i in 1:length(e)) e[i] <- x[i]
	}
	class(e) <- "expr"
	e
}

as.expr.data.frame <- function(x, length.x=prod(dim(x))) {
	e <- vector(mode="expression", length=length.x)
	if (length.x > 0) {
		e <- array(e, dim(x), dimnames(x))
		u.x <- unclass(x)
		for (j in 1:ncol(x)) {
			uxj <- as.matrix(u.x[[j]])
			for (i in 1:nrow(x))
				e[i,j] <- uxj[i,1]
		}
	}
	class(e) <- "expr"
	e
}


expr <- function(x)
	attr(x,"expr")

# "expr<-" is used only when value is a matrix the size of x, or to update
# a subscripted piece of x.  It is not a user function.
# Currently used only in "[<-.spread".
"expr<-" <- function(x, value)
{
	attr(x,"expr") <- value
	x
}

"before<-" <- function(x, value)
{
	attr(x,"before") <- value
	class(attr(x,"before")) <- "expr"
	x
}

"macro<-" <- function(x, value)
{
	attr(x,"macro") <- value
	class(attr(x,"macro")) <- "expr"
	x
}

"after<-" <- function(x, value)
{
	attr(x,"after") <- value
	class(attr(x,"after")) <- "expr"
	x
}

before <- function(x)
	attr(x,"before")


macro <- function(x)
	attr(x,"macro")


after <- function(x)
	attr(x,"after")


expr.rc <- function(x, ...) UseMethod("expr.rc")

expr.rc.default <- function(x, acpab)
{
	subs <- paste("[", paste(acpab, collapse=","), "]")

	if (length(expr(x))==0) {
		x.expr <- paste("x.value(x",subs,")",sep="")
		value <- eval(parse(text=x.expr))
	}
	else {
		e.expr <- paste("expr.value(expr(x)", subs, ", x", subs, ")")
		value <- eval(parse(text=e.expr))
	}

	paste("x", subs, " <- ", value, sep="")
}


x.value <- function(x) {
	value <-
	if (length(x)==1)
		as.vector(as.matrix(x[[1]]))
	else if (inherits(x,"data.frame"))
		lapply(x, function(x) as.vector(as.matrix(x)))
	else
		as.vector(x)
	deparse(value)
}

expr.value <- function(e, x) {
	if (inherits(x,"data.frame") &&
			(dim(e)[2]>1 || inherits(x[[1]],"factor")))
		value <- deparse(lapply(e, function(x) as.vector(as.matrix(x))))
	else {
		value <- paste(e, collapse=",")
		if (length(e) > 1) value <- paste("c(", value, ")", sep="")
	}
	value
}


index.value <- function(dn, i, deparse.result=T) {
	if (i==0) {i <- 0; mode(i) <- "missing"}
	if (is.numeric(i) && i>0 && length(dn)) i <- dn[i]
	if (deparse.result) deparse(as.vector(i))
	else as.vector(i)
}

as.numeric.spread <- function(x)
{
	.Options$warn <- -1
	tmp <- as.numeric(unclass(x))
	tmp <- ifelse(is.na(tmp), 0, tmp)
	attributes(tmp) <- attributes(x)
	tmp
}

all.numeric <- function(x) {
	.Options$warn <- -1
	!any(is.na(as.numeric(x)))
}

