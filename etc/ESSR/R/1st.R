#### Essential PROTO-functionality needed by ESS (before anything else
#### -------------------------------------------
#### even before ./basic.R
##
## Do not use _ in names, nor :: as they cannot be parsed in old R versions
##
.ess.sys.source <- if(any("keep.source" == names(formals(sys.source))))
		       sys.source else function(..., keep.source) sys.source(...)
ESSR <- if(local({nn <- names(formals(new.env))
                  length(nn) && any(nn == "parent")})) {
    .R.ver <- if(exists("getRversion", mode="function"))
        getRversion() else paste(R.version$major, R.version$minor, sep=".")
    new.env(parent = if(.R.ver >= "1.9.0")
            getNamespace("utils") else .BaseNamespaceEnv)
} else new.env()
##fails: rm(.R.ver)
