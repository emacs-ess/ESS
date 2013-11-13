#### Essential functionality needed by ESS
#### -------------------------------------
#### Do not use _ in names, nor :: as they cannot be parsed in old R versions

## run *after* ./1st.R
##		 ~~~~~
.ess.Rversion <- {
    if(exists("getRversion", mode="function"))
        getRversion() else paste(R.version$major, R.version$minor, sep=".")
}
.ess.R.has.utils <- (.ess.Rversion >= "1.9.0")
.ess.utils.name <- paste("package",
                         if(.ess.Rversion >= "1.9.0") "utils" else "base",
                         sep = ":")
## Instead of modern  utils::help use one that works in R 1.0.0:
.ess.findFUN   <- get("find", .ess.utils.name)
.ess.helpFUN   <- get("help", .ess.utils.name)
.ess.sourceFUN <- get("source", pos="package:base")

### HELP
.ess.help <- function(..., help.type = getOption('help_type'))
{
    if (.ess.Rversion > '2.10')# abbreviating 'help_type' on purpose:
	.ess.helpFUN(..., help = help.type)
    else # not using identical(), and working also for NULL:
	.ess.helpFUN(..., htmlhelp = (length(help.type) && help.type=='html'))
}

### SOURCING
.ess.eval <- function(string, echo = TRUE, print.eval = TRUE,
                      max.deparse.length = 300,
                      file = tempfile("ESS"), local = parent.frame()){
    ## create FILE, put string into it. Then source.
    ## arguments are like in source and .ess.source
    cat(string, file = file)
    on.exit(file.remove(file))
    .ess.source(file, echo = echo, print.eval = print.eval,
                max.deparse.length = max.deparse.length, local = local)
}

.ess.source <- function(file, echo = TRUE, print.eval = TRUE,
                        max.deparse.length = 300, local = parent.frame())
{
    ss <- .ess.sourceFUN
    ss <- if(any("keep.source" == names(formals(ss)))) ss
    else ## for R versions < 2.7.x, drop 'keep.source':
	function(..., keep.source) ss(...)
    invisible(ss(file, echo = echo, local = local, print.eval = print.eval,
		 max.deparse.length = max.deparse.length,
		 keep.source = TRUE)$value) ## return value for org-babel
}

if(.ess.Rversion < "1.8")# (works for "1.7.2"): bquote() was new in 1.8.0
    bquote <- function(expr, where=parent.frame())
{
    unquote <- function(e)
        if (is.pairlist(e)) as.pairlist(lapply(e, unquote))
        else if (length(e) <= 1L) e
        else if (e[[1L]] == as.name(".")) eval(e[[2L]], where)
        else as.call(lapply(e, unquote))

    unquote(substitute(expr))
}

