#### Essential functionality needed by ESS

## Should work on *all* vesions of R.
## Do not use _ in names, nor :: , nor 1L etc, as they
## cannot be parsed in old R versions


## loading ESSR.rda might fail, so re-assign here:
.ess.Rversion <-
    if( exists("getRversion", mode="function") ){
        getRversion()
    } else {
        paste(R.version$major, R.version$minor, sep=".")
    }

.ess.R.has.utils <- (.ess.Rversion >= "1.9.0")
.ess.utils.name <- paste("package",
                         if(.ess.Rversion >= "1.9.0") "utils" else "base",
                         sep = ":")

## Instead of modern  utils::help use one that works in R 1.0.0:
.ess.findFUN   <- get("find", .ess.utils.name)


### HELP
.ess.help <- function(..., help.type = getOption("help_type")) {
    if (is.null(help.type)) {
        help.type <- "text"
    }

    ## - Searching in global env makes sure it works with devtools
    ## - Redefining to .ess.help this way is necessary because
    ##   `print.help_files_with_topic` (used internally when there's
    ##   more than one a package) uses the quoted call
    .ess.help <- function(...) {
        do.call(get("help", envir = .GlobalEnv), list(...))
    }

    if (.ess.Rversion > "2.10") {
        ## Abbreviating help_type to avoid underscore
        .ess.help(..., help = help.type)
    } else {
        .ess.help(..., htmlhelp = help.type == "html")
    }
}

.ess.getHelpAliases <- function(){
    readrds <-
        if(.ess.Rversion >= '2.13.0') readRDS
        else .readRDS
    rds.files <- paste(searchpaths(), "/help/aliases.rds", sep = "")
    unlist(lapply(rds.files,
                  function(f){
                      if( file.exists(f) )
                          try(names(readrds(f)))
                  }),
           use.names = FALSE)
}

### SOURCING
.ess.eval <- function(string, visibly = TRUE, output = FALSE,
                      max.deparse.length = 300,
                      file = tempfile("ESS"),
                      local = if (.ess.Rversion > '2.13') parent.frame() else FALSE)
{
    ## create FILE, put string into it. Then source.
    ## arguments are like in source and .ess.source
    cat(string, file = file)
    on.exit(file.remove(file))
    .ess.source(file, visibly = visibly, output = output,
                max.deparse.length = max.deparse.length, local = local)
}

.ess.source <- function(file, visibly = TRUE, output = FALSE,
                        max.deparse.length = 300,
                        local = if (.ess.Rversion > '2.13') parent.frame() else FALSE)
{
    ss <- # drop 'keep.source' for older versions
        if(.ess.Rversion >= "2.8") base::source
        else function(..., keep.source) base::source(...)
    invisible(ss(file, echo = visibly, local = local, print.eval = output,
                 max.deparse.length = max.deparse.length,
                 keep.source = TRUE)$value) ## return value for org-babel
}

if(.ess.Rversion < "1.8")
    ## (works for "1.7.2"): bquote() was new in 1.8.0
    bquote <- function(expr, where=parent.frame()){
        unquote <- function(e)
            if (is.pairlist(e)) as.pairlist(lapply(e, unquote))
            else if (length(e) <= 1) e
            else if (e[[1]] == as.name(".")) eval(e[[2]], where)
            else as.call(lapply(e, unquote))

        unquote(substitute(expr))
    }


## Local Variables:
## eval: (ess-set-style 'RRR t)
## End:
