#-*-Fundamental-*-

	
col.spacing <- function(x)
{
	rn.w <- if (length(dimnames(x)[[1]]) > 0) max(nchar(dimnames(x)[[1]]))
		else nchar(as.character(nrow(x)))+3
	col.w <- apply(x, 2, function(x) nchar(format(x))[1])
	dn.w <- if (length(dimnames(x)[[2]]) > 0) nchar(dimnames(x)[[2]])
		else nchar(as.character(ncol(x)))+3
	col.w <- ifelse( col.w > dn.w , col.w, dn.w)
	cumsum(c(rn.w,col.w)+1)
}

emacs.expr <- function(x, i, j=i[2], result.type)
# 1. emacs.rc
# 2. emacs.macro
# 3. emacs.macro.text(deparse.result=T) #default for index.value
# 4. emacs.macro.text(deparse.result=F)
# 1. assign expression to cell or to macro
# 2. evaluate macro expression
# 3. retrieve macro expression
# 4. construct control.text() expression from macro name
# 5. construct print.text() expression from macro name
{
	# i and j are integer scalars

	if (missing(j)) {j <- i[2] ; i <- i[1]}

	if ((.Active == .Active.buffer) && (length(dim(x)) > 2))
		stop("Must use rectangular slice, not 3d buffer")

	if (i <= nrow(x) && result.type==1)
		return(expr.rc(x, c(i, j)))

	if (!inherits(x, "spread")) stop("Not a spread.frame")
	mm <- (nrow(x)+1):(nrow(x)+2+length(macro(x)))
	bb <- mm[length(mm)]+(1:(2+length(before(x))))
	aa <- bb[length(bb)]+(1:(2+length(after(x))))

	find.expr <- function(type.x, kk, type, result.type)
	{
	   if (kk>0) {
	      iv <- index.value(names(type.x), kk,
			!((result.type == 4) || (result.type == 5)))
	      switch(result.type,
		paste(type, "(x)[", iv, "] <- expression(",
		   expr.value(type.x[kk],1), ")"),
		paste("x <- eval.spread(x, ", type, "(x)[", iv, "] )" ),
		deparse(eval(parse(text=paste(type, "(x)[", iv, "]")))[[1]]),
		paste(iv, "<- control.text(", iv, ")"),
		paste(iv, "<- print.text(", iv, ")")
	      )
	}
	   else if (result.type==1) paste(type, "(x)[\"\"] <- expression()")
	      else NULL
	}

	k <- match(i, mm, 0)
	if (k) return(find.expr(macro(x), k-2, "macro", result.type))

	k <- match(i, bb, 0)
	if (k) return(find.expr(before(x), k-2, "before", result.type))

	k <- match(i, aa, 0)
	if (k) return(find.expr(after(x), k-2, "after", result.type))
}

cell.rc.emacs <- function(x, e.r, e.c)
{
	x.r <- ifelse(e.c == 0, e.r, e.r-1)
	x.c <- sum(e.c >= col.spacing(x))
	c(row=x.r, col=x.c)
}

print.update.emacs <- function(x, ...,
	file=paste(.spread.directory, .Active.buffer, sep="/"))
{
	sink(file)
	print(x, ...)

	xs <- get(.Active)
	if (inherits(xs, "spread"))
	{
		print.spread.macro(xs, macro)
		print.spread.macro(xs, before)
		print.spread.macro(xs, after)
	}

	sink()
	invisible(x)
}

print.spread.macro <- function(x, macro)
{
	cat("\n**", as.character(substitute(macro)), "**\n", sep="")
	ne <- names(macro(x))
	if (length(ne))
		for (i in 1:length(ne))
			cat(index.value(ne,i,F),"\n")
}
	

as.two.way.array <- function(x, subs=parse(text=.Active.buffer)[[1]][-(1:2)])
{
	if (length(dim(x))==2) return(x)
# This is designed for 3 way arrays with
# two missing and one specified dimension.
# If the drop parameter exists, it is over-ridden.
	subs$drop <- NULL
	which.subs <- (sapply(subs,length)==0)
	dnx <- dimnames(x)[which.subs]
	dimnames(x) <- NULL
	dim(x) <- dim(x)[which.subs]
	dimnames(x) <- dnx
	x
}


fg <- function( sprdname=.Active )
# sprdname = character name, possibly subscripted
{
   if (is.na(match(sprdname, names(macro(.Registry))))) {
	macro(.Registry)[sprdname] <- sprdname
        assign(".Registry", .Registry, where=1 )
   }	
   assign(".Active.buffer", sprdname, frame=0 )
   assign(".Active", find.names(sprdname), frame=0 )
   assign("x", eval(parse(text=.Active)), where=1 )
   assign("x.buffer", where=1,
	if (.Active.buffer==.Active) x
	else as.two.way.array(eval(parse(text=.Active.buffer))))
   invisible(sprdname)
}

control.emacs <- function(x)
{
#this is a fake function
#emacs does the work

# control.emacs never gets called when emacs is in control.
#	RET in spread window puts old command in minibuffer:
# emacs sends
#    emacs.cell('spreadname', e.r, e.c, result.type)
# emacs reads the file written by the above and
# asks the user to revise it in the minibuffer.
#	RET in minibuffer puts revised command in S buffer,
# and causes the revised command to be executed, updating the spreadsheet.
# emacs issues
#    invisible(assign(.Active, x))
# to place the object in x into the object named in .Active
# emacs issues
#    print.find.emacs('spreadname', update.Registry=F)
# to update all buffers showing views of the object named in .Active
# When S gets control back, the command has been executed and the
# spreadsheet has been updated
}

#emacs usage
#load-file S-spread.el
#In the *S* buffer, type ^Cr to place a spread.frame or 2-way or 3-way array
# into a spread.frame buffer.
#In the spread.frame buffer, type RET to update a cell.
#In the minibuffer, revise the cell and type RET to update the object and
# the display.
#If there is a timing problem and the display is not updated,
# then type ^Cv in the spread buffer.



find.sprds <- function(sprdname, reg.names=names(macro(.Registry)))
{
	reg.names[find.names(reg.names) == find.names(sprdname)]
}

find.names <- function(reg.names)
{
	prn <- parse(text=reg.names)
	for (i in 1:length(prn))
		if (mode(prn[[i]]) != "name") reg.names[i] <- prn[[i]][[2]]
	reg.names
}


print.sprds.emacs <- function(sprdname)
{
	fssn <- find.sprds(sprdname)
	fssn2 <- fssn
	for(i in fssn2) {
		fg(i)
		print.update.emacs(x.buffer)
	}
	cat(paste(fssn, collapse="\n"), "\n", sep="", file=.spread.command.file)
	invisible(fg(sprdname))
}

print.update.emacs.3d <- function(object)
{
	object.name <- as.character(substitute(object))
	dobject <- dim(object)
	if (length(dobject) != 3) stop("3-way array required")
	fg(object.name)

	n3 <- dimnames(object)[[3]]
	if (is.null(n3)) n3 <- seq(length=dobject[3])
	else n3 <- paste("\"", n3, "\"", sep="")
	for (i in n3) {
		fg(paste( object.name, "[,,", i, "]", sep="" ))
		print.update.emacs(x.buffer)
	}
	invisible(object)
}

emacs.start <- function(spread.directory)
{
	assign('.spread.directory', spread.directory, frame=0)
	if (!exists('.Registry', 1))
		assign(".Registry", where=1, as.spread(matrix(".Registry")))
	assign(".spread.command.file", frame=0,
		paste(spread.directory, "*command*", sep="/"))
	fg(".Registry")
	print.update.emacs(.Registry)
	invisible(".Registry")
}


print.find.emacs <- function(spread=.Active, update.Registry=T)
{
	fg(spread)
	if (update.Registry) {
		fg(".Registry")
		print.update.emacs(.Registry)
		fg(spread)
	}
	print.sprds.emacs(spread)
	invisible(spread)
}


emacs.cell <- function(spread, e.r, e.c, result.type)
{
	fg(spread)
	cell.rc <- cell.rc.emacs(x.buffer, e.r, e.c)
	.Options$width <- 1000
	if (result.type==1 && cell.rc[1] <= nrow(x.buffer)) {
		cell.rc <- cell.sub.emacs(x, cell.rc)
		cell.expr <- expr.rc(x, cell.rc)
	}
	else
		cell.expr <- emacs.expr(x, cell.rc, result.type=result.type)
	cat(cell.expr, '\n', sep='', file=.spread.command.file)
}

cell.sub.emacs <- function(x, i, j=i[2])
{
	# i and j are integer scalars

	if (missing(j)) {j <- i[2] ; i <- i[1]}
	if (i==0 && j==0) stop("non-zero row or column required")

	if ((length(dim(x)) == 2)) {
		acpab <- c("","")
		positions <- 1:2
	}
	else if (.Active == .Active.buffer)
		stop("Must use rectangular slice, not 3d buffer")
	else {
		pab <- parse(text=.Active.buffer)
		acpab <- as.character( pab[[1]][-(1:2)] )
		positions <- (1:length(acpab))[sapply(acpab, nchar) == 0]
	}

	di <- index.value(dimnames(x)[[positions[1]]], i)
	dj <- index.value(dimnames(x)[[positions[2]]], j)

	acpab[positions[1]] <- di
	acpab[positions[2]] <- dj

	acpab
}
