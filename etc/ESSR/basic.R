## Esential functionality needed by ESS

.essRversion <- function() {
    if(exists("getRversion", mode="function"))
        getRversion() else paste(R.version$major, R.version$minor, sep=".")
}

### HELP
.ess_help <- function(..., help_type = getOption('help_type')){
    if (.essRversion() > '2.10')
        utils::help(..., help_type = help_type)
    else
        utils::help(..., htmlhelp = (help_type=='html'))
}

### SOURCING
.ess_eval <- function(string, echo = TRUE, print.eval = TRUE, max.deparse.length = 300,
                      file = tempfile("ESS"), local = parent.frame()){
    ## create FILE, put string into it. Then source.
    ## arguments are like in source and .ess_source
    cat(string, file = file)
    on.exit(file.remove(file))
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

