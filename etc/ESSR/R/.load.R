## Do not use _ in names, nor :: as they cannot be parsed in old R versions

## load .base.R and all other files into ESSR environment; then attach ESSR
load.ESSR <- function(dir){
    .source <-
        if(any("keep.source" == names(formals(sys.source))))
            sys.source
        else
            function(..., keep.source) sys.source(...)

    Rver <-
        if(exists("getRversion", mode="function")) getRversion()
        else paste(R.version$major, R.version$minor, sep=".")

    nn <- names(formals(new.env))

    ESSR <-
        if(length(nn) && any(nn == "parent"))
            new.env(parent =
                    if(Rver >= "1.9.0") getNamespace("utils")
                    else .BaseNamespaceEnv)
        else
            new.env()

    assign(".ess.Rversion", Rver, envir = ESSR)

    ## .basic.R:
    .source(paste(dir,'/.basic.R', sep = ""), envir = ESSR, keep.source = FALSE)

    ## all others:
    for( f in dir(dir, pattern='[A-Za-z].*\\.R$', full.names=TRUE) )
        try(.source(f, envir = ESSR, keep.source = FALSE))

    attach(ESSR)
}
