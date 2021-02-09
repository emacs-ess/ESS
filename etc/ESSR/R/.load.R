## This file is sourced when R starts and `load.ESSR` is called. See
## inferior-ess-r-load-ESSR--local.
## Do not use _ in names, nor :: as they cannot be parsed in old R versions

## load .base.R and all other files into ESSR environment; then attach ESSR
.ess.load.ESSR <- function(dir = NULL) {
    .source <-
        if(any("keep.source" == names(formals(sys.source))))
            sys.source
        else
            function(..., keep.source) sys.source(...)

    Rver <- if(exists("getRversion", mode="function")) getRversion()
            else paste(R.version$major, R.version$minor, sep=".")
    oldR <- Rver <= "1.3.0"

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

    ## either source the ESSR files if a directory is provided, or otherwise try
    ## to move any existing R objects into the ESSR environment
    if(!is.null(dir)) {

        ## .basic.R:
        try(.source(paste(dir,'/.basic.R', sep = ""), envir = ESSR, keep.source = FALSE))

        ## all others try(*) as it will fail in old R
        if(!oldR) # no sense if(oldR)
            for( f in dir(dir, pattern='\\.R$', full.names=TRUE) )
                try(.source(f, envir = ESSR, keep.source = FALSE))
    }
    else {
        essr.nms <- grep('\\.(ess|ESS)', ls(.GlobalEnv, all.names = TRUE), value = TRUE)
        for (nm in essr.nms) {
            ESSR[[nm]] <- get(nm, pos = .GlobalEnv)
        }
        rm(list = essr.nms, pos = .GlobalEnv)
    }

    if(Rver >= "2.4.0")
        attach(ESSR)
    else if(!oldR) { ## borrow from older library()
        e <- attach(NULL, name = "ESSR")
        .Internal(lib.fixup(ESSR, e))
    } else { ## if(oldR), use as in that old library():
        .Internal(lib.fixup(ESSR, .GlobalEnv))
    }

    ## BUILDESSR needs this:
    invisible(ESSR)
}
