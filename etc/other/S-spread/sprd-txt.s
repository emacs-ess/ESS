# prspread is based on prmatrix
prspread <-
function(x, rowlab = character(0), collab = character(0), quote = T, right = F,
	spread.name=deparse(match.call()[["x"]]) )
{
	d <- dim(x)
	dnames <- dimnames(x)
	if(is.null(dnames))
		dnames <- list(rowlab, collab)
	else {
		if(!missing(rowlab))
			dnames[[1]] <- as.character(rowlab)
		if(!missing(collab))
			dnames[[2]] <- as.character(collab)
	}
	if(length(dnames[[1]]) == 0)
		dnames[[1]] <- paste("[", 1:d[1], ",]", sep = "")
	else if(length(dnames[[1]]) != d[1])
		stop("rowlab is wrong length")
	if(length(dnames[[2]]) == 0)
		dnames[[2]] <- paste("[,", 1:d[2], "]", sep = "")
	else if(length(dnames[[2]]) != d[2])
		stop("collab is wrong length")
	cbind(c(spread.name,dnames[[1]]), rbind(dnames[[2]], as.matrix(x)))
}



row.ch <- function(x, d=dim(x))
	array(1:d[1], d, dimnames(x))

col.ch <- function(x, d=dim(x))
	array(rep.int(1:d[2], rep.int(d[1], d[2])), d, dimnames(x))


print.text <- function(x, screen.n, cex=1,
	spread.name=deparse(match.call()[["x"]]), clear=T, ...)
{
	x.pr <- prspread(x, spread.name=spread.name)
	if (!missing(screen.n)) screen(screen.n)
	usr <- c(0, ncol(x.pr), 0, nrow(x.pr)) - .5
	par(usr=usr)
	par(plt=c(0,1,0,1))
	if (clear)
	polygon(usr[c(1,2,2,1)],usr[c(3,3,4,4)], den=-1,col=0,xaxt="s",yaxt="s")
	text(x=as.vector(col.ch(x.pr)-1),
		 y=as.vector(nrow(x.pr)-row.ch(x.pr)), x.pr, cex=cex, ...)
	box()
	invisible(x)
}

text.update.spread <- function(xij, row.i, col.j, screen.n, cex=1, x)
{
	if (!missing(screen.n)) {screen(screen.n, new=F); par(plt=c(0,1,0,1))}
	y <- nrow(x)-row.i
	clear.text(x=col.j, y=y)
	text(x=col.j, y=y, xij, cex=cex)
	box()
	invisible(x)
}


cell.rc.text <- function(nrow.x, n=1, type="n")
{
	xy <- locator(n, type)
	c(row=nrow.x-round(xy$y), col=round(xy$x))
}

clear.text <- function(x,y)
   polygon(x-.5+c(0,1,1,0), y-.5+c(0,0,1,1), den=-1, col=0, border=F,
   xaxt="s", yaxt="s")


print.update.text <- function(x, ..., x.old, screen.n, cex=1,
	spread.name=deparse(match.call()[["x"]]))
{
	if(missing(x.old)) return(invisible(print.text(x, screen=screen.n,
		cex=cex, spread.name=spread.name)))
	if (!missing(screen.n)) {screen(screen.n, new=F); par(plt=c(0,1,0,1))}
	diff.x <- as.vector(x != x.old)
	xx <- col(x)[diff.x]
	yy <- nrow(x)-row(x)[diff.x]
	for (i in seq(along=xx)) clear.text(xx[i], yy[i])
	box()
	text(x=xx, y=yy, as.vector(unlist(x))[diff.x], cex=cex)
	invisible(x)
}


control.text <- function(x, screen.n, cex=1,
	spread.name=deparse(match.call()[["x"]]))
{
	#This is a real function that does its own work
	if (!missing(screen.n)) {screen(screen.n, new=F); par(plt=c(0,1,0,1))}
	x.old <- x[,]
	rc <- cell.rc.text(nrow(x))
	command <- expr.rc(x, rc)
	cat("> ", command, "\n", sep="", file="")
	eval(parse(text=readline()))
	if (!missing(screen.n)) {screen(screen.n, new=F); par(plt=c(0,1,0,1))}
	print.update.text(x, x.old=x.old, cex=cex, spread.name=spread.name)
#	print.text(x, cex=cex, spread.name=spread.name)
	invisible(x)
}


#text usage
# device() # for example, x11(), or motif(), or win.graph()
# x <- my.spread                # copy my.spread to x
##loop
# print.text(x)                 # work with x
# x <- control.text(x, screen)  # screen is optional
##end loop
# my.spread <- x                # copy revised x back to my.spread
