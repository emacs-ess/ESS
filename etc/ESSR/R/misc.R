.ess_help <- function(..., help_type = getOption('help_type')){
    if (getRversion() > '2.10')
        utils::help(..., help_type = help_type)
    else
        utils::help(..., htmlhelp = (help_type=='html'))
}

### SOURCING
.ess_eval <- function(string, echo = TRUE, print.eval = TRUE, max.deparse.length = 300,
                      file = tempfile("ESS"), local = parent.frame()){
    cat(string, file = file)
    .ess_source(file, echo = echo, print.eval = print.eval,
                max.deparse.length = max.deparse.length, local = local)
}

.ess_source <- function(file, echo = TRUE, print.eval = TRUE,
                        max.deparse.length = 300, local = parent.frame()){
    invisible(base::source(file = file,
                           echo = echo, local = local,
                           print.eval = print.eval,
                           max.deparse.length = 300,
                           keep.source = TRUE)$value) ## return value for org-babel
}

### WEAVING
.ess_weave <- function(command, file, encoding = NULL){
    cmd_symb <- substitute(command)
    if(grepl('knit|purl', deparse(cmd_symb))) require(knitr)
    od <- getwd()
    on.exit(setwd(od))
    setwd(dirname(file))
    frame <- parent.frame()
    if(is.null(encoding))
        eval(bquote(.(cmd_symb)(.(file))), envir = frame)
    else
        eval(bquote(.(cmd_symb)(.(file), encoding = .(encoding))), envir = frame)
}

