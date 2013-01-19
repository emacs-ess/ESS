foobar <- function(...) {}
rm(list=ls())

##--------> consequence of the above experiments:
## the 2nd form is numerically "uniformly better" than the first
##--------> 2011-05-27:  Change Frank's psiInv() to
## psiInv = function(t,theta)
##     -log1p(exp(-theta)*expm1((1-t)*theta)/expm1(-theta))

##--- In the following block, in the first line, C-c C-c did *NOT* behave

th <- 48 # now do ls() and see what happened ... the horror !!!
d <- 3
cpF <- list("Frank", list(th, 1:d))
cop <- acF <- cpF$copula


### Here, the bug (12.09-2, e.g.) has been that
### the function beginning is not found reliably:
### C-M-q -> should go to end; then C-M-a should go back to beginning (here)
mplot4 <- function(x, vList, xvar, cvar, rvar, log = "",
		   verbose=FALSE, show.layout=verbose)
{
    dn <- dimnames(x)
    ## the variable displayed in one plot (with different colors):
    v <- setdiff(names(dn), c(xvar, cvar, rvar))
    stopifnot(length(v) == 1, 1 <= (nv <- length(dn[[v]])), nv <= length(pcol),
	      length(pspc) == 2, length(spc) == 2, length(axlabspc) == 2,
	      length(labspc) == 2, length(auxcol) == 4)
    v.col <- colorRampPalette(pcol, space="Lab")(nv) # colors for v
    ## permute to know the component indices:
    x <- aperm(x, perm=c(rvar, cvar, v, xvar))

    if(is.null(xlab)) # default: the expression from varlist
	xlab <- vList[[xvar]]$expr
    z <- as.numeric(vList[[xvar]]$value) # pick out different x values
    zrange <- range(z) # for forcing the same x axis limits per row

    ## set up the grid layout
    nx <- length(dn[[cvar]]) # number of plot columns
    nx. <- nx+1+(nx-1)+1 # +1: for y axis label; +(nx-1): for gaps; +1: for row labels
    ny <- length(dn[[rvar]]) # number of plot rows
    ny. <- ny+1+(ny-1)+1 # +1: for column labels; +(ny-1): for gaps; +1: for x axis label
    ## plot settings, restored on exit
    opar <- par(no.readonly=TRUE); on.exit(par(opar))
    plot.new() # start (empty) new page with 'graphics'
    gl <- grid.layout(nx., ny.,
	     ## units in npc as for pdf(); no square plotting region otherwise:
	     default.units="npc",
	     widths=c(axlabspc[1], rep(c(pspc[1], spc[1]), nx-1), pspc[1], labspc[1]),
	     heights=c(labspc[2], rep(c(pspc[2], spc[2]), ny-1), pspc[2], axlabspc[2]))
    if(show.layout) grid.show.layout(gl, vp=viewport(width=1.25, height=1.25))
    pushViewport(viewport(layout=gl)) # use this layout in a viewport

    ## --- plot data ---
    for(i in 1:nx) { # rows
	i. <- 2*i # column index in layout (for jumping over gaps)
	if(verbose) cat(sprintf("plot row %d (%d): [columns:] ", i, i.))
	yrange <- range(x[i,,,]) # for forcing the same y axis limits per row
	for(j in 1:ny) { # columns
	    j. <- 2*j # row index in layout (for jumping over gaps)
	    if(verbose) cat(sprintf("%d (%d) ", j, j.))
	    pushViewport(viewport(layout.pos.row=i., layout.pos.col=j.))

	    ## plot
	    grid.rect(gp=gpar(col=NA, fill=auxcol[3])) # background
	    ## start a 'graphics' plot
	    par(plt = gridPLT())
	    ## Hmm, this is not really useful for debugging:
	    ## rp <- tryCatch(par(plt=gridPLT()), error = function(e)e)
	    ## if(inherits(rp, "error")) {
	    ##	   cat("\n *** ERROR in mplot() :\n", rp$message,"\n"); return(gl)
	    ## }
	    par(new=TRUE) # always do this before each new 'graphics' plot
	    ## set up coordinate axes:
	    plot(zrange, yrange, log=log, type="n", ann=FALSE, axes=FALSE)
	    ## background grid:
	    grid(col=auxcol[4], lty="solid", lwd=grid.lwd, equilogs=FALSE)
	    ## plot corresponding points/lines
	    for(k in 1:nv) points(z, x[i,j,k,], type="b", col=v.col[k])
	    ## axes
	    c1 <- auxcol[1]
	    if(i == nx) # x axes
		axis(1, lwd=ax.lwd, col=NA, col.ticks=c1, col.axis=c1)
	    if(j == 1) { # y axes
		if(packageVersion("sfsmisc") >= "1.0-21")
		    ## allow for adjusting colors of small ticks
		    eaxis(2, lwd=ax.lwd, col=NA, col.ticks=c1, col.axis=c1,
			  small.args=list(col=NA, col.ticks=c1, col.axis=c1))
		else
		    eaxis(2, lwd=ax.lwd, col=NA, col.ticks=c1, col.axis=c1)
	    }
	    upViewport()

	    ## column labels
	    if(i == 1) {
		pushViewport(viewport(layout.pos.row=1, layout.pos.col=j.))
		grid.rect(gp=gpar(col=NA, fill=auxcol[2]))
		grid.text(parse(text=dn[[cvar]][j]), x=0.5, y=0.5, gp=gpar(cex=tx.cex))
		upViewport()
	    }

	    ## row labels
	    if(j == 2) {
		pushViewport(viewport(layout.pos.row=i., layout.pos.col=nx.))
		grid.rect(gp=gpar(col=NA, fill=auxcol[2]))
		grid.text(parse(text=dn[[rvar]][i]), x=0.5, y=0.5, gp=gpar(cex=tx.cex), rot=-90)
		upViewport()
	    }
	}## for(j ..)
	if(verbose) cat("\n")
    }## for(i ..)

    ## legend
    pushViewport(viewport(layout.pos.row=ny., layout.pos.col=2:(ny.-1)))
    ll <- 0.01 # line length

    ## [... ... made example smaller ... ESS-bug still shows ....]

    upViewport()
    invisible(gl)
}
