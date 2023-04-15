## This file is sourced when R starts and `load.ESSR` is called. See
## inferior-ess-r-load-ESSR--local.
## Do not use _ in names, nor :: as they cannot be parsed in old R versions

## obtain the R version number as a string
.ess.ESSR.get.rver <- function() {
    if(exists("getRversion", mode="function")) getRversion()
    else paste(R.version$major, R.version$minor, sep=".")
}

## load .base.R and all other files into ESSR environment; then attach ESSR
.ess.ESSR.load <- function(dir) {

    if (nzchar(Sys.getenv("ESSR_TEST_LOAD_ERROR")))
        stop('Loading failed with a nice message.')

    Rver <- .ess.ESSR.get.rver()
    ESSR <- .ess.ESSR.create.env(Rver)
    .ess.ESSR.source.files(ESSR, dir, Rver)
    .ess.ESSR.attach.env(ESSR, Rver)

    ## BUILDESSR needs this:
    invisible(ESSR)
}

.ess.ESSR.create.env <- function(Rver) {

    ESSR <-
        if (Rver <= "1.3.0") ## really old library() revert order a bit
            attach(NULL, name = "ESSR")
        else if(length(nn <- names(formals(new.env))) && any(nn == "parent"))
            new.env(parent =
                        if (Rver >= "1.9.0") getNamespace("utils")
                        else .BaseNamespaceEnv)
        else
            new.env()

    assign(".ess.Rversion", Rver, envir = ESSR)

    ## updated by make !!
    VERSION <- "1.8"
    assign(".ess.ESSRversion", VERSION, envir = ESSR)

    ESSR
}

## attempt to source the files in the ESSR directory and place the resulting R
## objects into ESSR environment; the updated version of the environment is
## returned
.ess.ESSR.source.files <- function(ESSR, dir, Rver) {

    .source <-
        if(any("keep.source" == names(formals(sys.source))))
            sys.source
        else
            function(..., keep.source) sys.source(...)

    ## .basic.R:
    try(.source(paste(dir,'/.basic.R', sep = ""), envir = ESSR, keep.source = FALSE))

    ## all others try(*) as it will fail in old R
    if (Rver > "1.3.0") { # no sense if very old R
        for (f in dir(dir, pattern='\\.R$', full.names=TRUE))
            try(.source(f, envir = ESSR, keep.source = FALSE))
    }

    ESSR
}


## attach the ESSR environment to the search path
.ess.ESSR.attach.env <- function(ESSR, Rver) {
    if (Rver >= "2.4.0")
        attach(ESSR)
    else if (Rver > "1.3.0") { # borrow from older library()
        e <- attach(NULL, name = "ESSR")
        .Internal(lib.fixup(ESSR, e))
    } else { # if old R, use as in that old library():
        .Internal(lib.fixup(ESSR, .GlobalEnv))
    }
}


## copy any ESSR R objects in the global environment into a newly created ESSR
## environment; then remove the original objects and attach ESSR
.ess.collect.ESSR.objects <- function() {

    Rver <- .ess.ESSR.get.rver()
    ESSR <- .ess.ESSR.create.env(Rver)
    essr.nms <- grep('\\.(ess|ESS)', ls(.GlobalEnv, all.names = TRUE), value = TRUE)
    for (nm in essr.nms) {
        assign(nm, get(nm, pos = .GlobalEnv), envir = ESSR)
    }
    .ess.ESSR.attach.env(ESSR, Rver)
    rm(list = essr.nms, pos = .GlobalEnv)
}
