as.grade <- function(x)
{
	if (inherits(x,"grade")) return(x)
	if (match("sum",dimnames(x)[[2]],0) == 0) {
		dx  <- dim(x)
		dnx <- dimnames(x)
		if (length(dim(x)) == 2) {
			if (length(dnx) != 2) dimnames(x) <- list(NULL, 1:dx[2])
			tmp <- cbind(x,sum=0)
		}
		if (length(dim(x)) == 3) {
			dimnames(x) <- NULL
			tmp <- aperm(x,c(1,3,2))
			dim(tmp) <- c(dim(tmp)[1]*dim(tmp)[3], dim(tmp)[2])
			tmp <- cbind(tmp, sum=0)
			dim(tmp) <- (dx + c(0,1,0))[c(1,3,2)]
			tmp <- aperm(tmp,c(1,3,2))
			if (length(dnx) != 3) dnx <- list(NULL, 1:dx[2], NULL)
			dnx[[2]] <- c(dnx[[2]], "sum")
			dimnames(tmp) <- dnx
		}
		if (length(dim(x)) > 3) stop("grade requires 2d or 3d")
	}
	x <- as.spread(tmp)
	sum.col <- match("sum",dimnames(x)[[2]],0)
	tmp.expr <- paste(
		"x[,",
		sum.col,
		if (length(dim(x))==3) ",",
		"] <- apply(x[,",
		-sum.col,
		if (length(dim(x))==3) ",",
		 "],",
		deparse(if (length(dim(x))==3) c(1,3) else 1),
		",sum)"
	)
	after(x)["sum"] <- parse(text=tmp.expr)
	class(x) <- c("grade", class(x))
	update.spread(x)
}

	expr.rc.grade <- function(x, acpab)
{
	if (sapply(acpab,nchar)[[2]] == 0) {
		j <- -match("sum", dimnames(x)[[2]], 0)
		acpab[2] <- j
	}
	expr.rc.default(x,acpab)
}
