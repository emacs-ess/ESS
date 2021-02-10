## This file is sourced when R starts and `load.ESSR` is called. See
## inferior-ess-r-load-ESSR--local.
## Do not use _ in names, nor :: as they cannot be parsed in old R versions

## load .base.R and all other files into ESSR environment; then attach ESSR
.ess.load.ESSR <- function(dir) {

    Rver <- .ess.load.ESSR.get.rver()
    oldR <- .ess.load.ESSR.check.oldr(Rver)

    ESSR <- .ess.load.ESSR.create.env(Rver, oldR)
    ESSR <- .ess.load.ESSR.source.files(ESSR, dir, oldR)
    .ess.load.ESSR.attach.env(ESSR, Rver, oldR)

    ## BUILDESSR needs this:
    invisible(ESSR)
}


## obtain the R version number as a string
.ess.load.ESSR.get.rver <- function() {
    if(exists("getRversion", mode="function")) getRversion()
    else paste(R.version$major, R.version$minor, sep=".")
}


## check whether R is on or before an old version (certain functions that are
## relevant to the ESSR load process change after this version)
.ess.load.ESSR.check.oldr <- function(Rver) {
    Rver <= "1.3.0"
}


## create the ESSR evironment and add R and ESSR version information to it
.ess.load.ESSR.create.env <- function(Rver, oldR) {

    ESSR <-
        if(oldR) ## really old library() revert order a bit
            attach(NULL, name = "ESSR")
        else if(length(nn <- names(formals(new.env))) && any(nn == "parent"))
            new.env(parent =
                        if(Rver >= "1.9.0") getNamespace("utils")
                        else .BaseNamespaceEnv)
        else
            new.env()

    assign(".ess.Rversion", Rver, envir = ESSR)

    ## updated by make !!
    VERSION <- "1.7"
    assign(".ess.ESSRversion", VERSION, envir = ESSR)

    ESSR
}


## attempt to source the files in the ESSR directory and place the resulting R
## objects into ESSR environment; the updated version of the environment is
## returned
.ess.load.ESSR.source.files <- function(ESSR, dir, oldR) {

    .source <-
        if(any("keep.source" == names(formals(sys.source))))
            sys.source
        else
            function(..., keep.source) sys.source(...)

    ## .basic.R:
    try(.source(paste(dir,'/.basic.R', sep = ""), envir = ESSR, keep.source = FALSE))

    ## all others try(*) as it will fail in old R
    if(!oldR) # no sense if(oldR)
        for( f in dir(dir, pattern='\\.R$', full.names=TRUE) )
            try(.source(f, envir = ESSR, keep.source = FALSE))

    ESSR
}


## attach the ESSR environment to the search path
.ess.load.ESSR.attach.env <- function(ESSR, Rver, oldR) {
    if(Rver >= "2.4.0")
        attach(ESSR)
    else if(!oldR) { ## borrow from older library()
        e <- attach(NULL, name = "ESSR")
        .Internal(lib.fixup(ESSR, e))
    } else { ## if(oldR), use as in that old library():
        .Internal(lib.fixup(ESSR, .GlobalEnv))
    }
}


## copy any ESSR R objects in the global environment into a newly created ESSR
## environment; then remove the original objects and attach ESSR
.ess.collect.ESSR.objects <- function() {

    Rver <- .ess.load.ESSR.get.rver()
    oldR <- .ess.load.ESSR.check.oldr(Rver)

    ESSR <- .ess.load.ESSR.create.env(Rver, oldR)
    essr.nms <- grep('\\.(ess|ESS)', ls(.GlobalEnv, all.names = TRUE), value = TRUE)
    for (nm in essr.nms) {
        assign(nm, get(nm, pos = .GlobalEnv), envir = ESSR)
    }
    .ess.load.ESSR.attach.env(ESSR, Rver, oldR)
    rm(list = essr.nms, pos = .GlobalEnv)
}
