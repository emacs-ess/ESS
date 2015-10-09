.ess_funargs <- function(funname) {
    if(.ess.Rversion > '2.14.1') {
        comp <- compiler::enableJIT(0)
        op <- options(error=NULL)
        on.exit({ options(op); compiler::enableJIT(comp) })
    }
    ## don't remove; really need eval(parse(  here!!
    fun <- tryCatch(eval(parse(text=funname)),
                    error=function(e) NULL) ## also works for special objects containing @:$ etc
    if(is.function(fun)) {
        special <- grepl('[:$@[]', funname)
        args <- if(!special){
            fundef <- paste(funname, '.default',sep='')
            do.call('argsAnywhere', list(fundef))
        }

        if(is.null(args))
            args <- args(fun)
        if(is.null(args))
            args <- do.call('argsAnywhere', list(funname))

        fmls <- formals(args)
        fmls_names <- names(fmls)
        fmls <- gsub('\"', '\\\"',
                     gsub("\\", "\\\\", as.character(fmls),fixed = TRUE),
                     fixed=TRUE)
        args_alist <-
            sprintf("'(%s)",
                    paste("(\"", fmls_names, "\" . \"", fmls, "\")",
                          sep = '', collapse = ' '))
        allargs <-
            if(special) fmls_names
            else tryCatch(gsub('=', '', utils:::functionArgs(funname, ''), fixed = TRUE),
                          error=function(e) NULL)
        
        allargs <- sprintf("'(\"%s\")",
                           paste(allargs, collapse = '\" "'))
        envname <- environmentName(environment(fun))
        if(envname == "R_GlobalEnv") envname <- ""
        cat(sprintf('(list \"%s\" %s %s)\n',
                    envname, args_alist, allargs))
    }
}

## if(.ess.Rversion > '2.14.1'){
##     comp <- compiler::enableJIT(0)
##     op <- options(error=NULL)
##     on.exit({ options(op); compiler::enableJIT(comp) })
## }


## args_alist <-
##     sprintf("'(%s)",
##             paste("(\"", fmls_names, "\" . \"", fmls, "\")",
##                   sep = '', collapse = ' '))

## allargs <- sprintf("'(\"%s\")",
##                    paste(allargs, collapse = '\" "'))
