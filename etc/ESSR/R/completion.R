
.ess_funargs <- function(funname) {
    if(.ess.Rversion > '2.14.1') {
        comp <- compiler::enableJIT(0)
        olderr <- getOption('error')
        options(error=NULL)
        on.exit({
            compiler::enableJIT(comp)
            options(error = olderr)
        })
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

.ess_get_completions <- function(string, end){
    if(.ess.Rversion > '2.14.1'){
        comp <- compiler::enableJIT(0)
        olderr <- getOption('error')
        options(error=NULL)
        on.exit({options(error = olderr)
                 compiler::enableJIT(comp)})
    }
    utils:::.assignLinebuffer(string)
    utils:::.assignEnd(end)
    utils:::.guessTokenFromLine()
    utils:::.completeToken()
    c(get('token', envir=utils:::.CompletionEnv),
      utils:::.retrieveCompletions())
}
