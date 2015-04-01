
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

.ess_arg_help <- function(arg, func){
    olderr <- getOption('error')
    options(error=NULL)
    on.exit(options(error=olderr))
    fguess <-
        if(is.null(func)) get('fguess', envir=utils:::.CompletionEnv)
        else func
    findArgHelp <- function(fun, arg){
        file <- help(fun, try.all.packages=FALSE)[[1]]
        hlp <- utils:::.getHelpFile(file)
        id <- grep('arguments', tools:::RdTags(hlp), fixed=TRUE)
        if(length(id)){
            arg_section <- hlp[[id[[1L]]]]
            items <- grep('item', tools:::RdTags(arg_section), fixed=TRUE)
            ## cat('items:', items, fill=TRUE)
            if(length(items)){
                arg_section <- arg_section[items]
                args <- unlist(lapply(arg_section,
                                      function(el) paste(unlist(el[[1]][[1]], TRUE, FALSE), collapse='')))
                fits <- grep(arg, args, fixed=TRUE)
                ## cat('args', args, 'fits', fill=TRUE)
                if(length(fits))
                    paste(unlist(arg_section[[fits[1L]]][[2]], TRUE, FALSE), collapse='')
            }
        }
    }
    funcs <- c(fguess, tryCatch(methods(fguess),
                                warning=function(w) {NULL},
                                error=function(e) {NULL}))
    if(length(funcs) > 1 && length(pos <- grep('default', funcs))){
        funcs <- c(funcs[[pos[[1L]]]], funcs[-pos[[1L]]])
    }
    i <- 1L; found <- FALSE
    out <- 'No help found'
    while(i <= length(funcs) && is.null(out <-
        tryCatch(findArgHelp(funcs[[i]], arg),
                 warning=function(w) {NULL},
                 error=function(e) {NULL})
                                        ))
        i <- i + 1L
    cat('\n\n', as.character(out), '\n')
};
