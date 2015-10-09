.ess_arg_help <- function(arg, func){
    op <- options(error=NULL)
    on.exit(options(op))
    fguess <-
        if(is.null(func)) get('fguess', envir=utils:::.CompletionEnv)
        else func
    findArgHelp <- function(fun, arg){
        file <- help(fun, try.all.packages=FALSE)[[1]]
        hlp <- utils:::.getHelpFile(file)
        id <- grep('arguments', tools:::RdTags(hlp), fixed=TRUE)
        if(length(id)){
            arg_section <- hlp[[id[[1]]]]
            items <- grep('item', tools:::RdTags(arg_section), fixed=TRUE)
            ## cat('items:', items, fill=TRUE)
            if(length(items)){
                arg_section <- arg_section[items]
                args <- unlist(lapply(arg_section,
                                      function(el) paste(unlist(el[[1]][[1]], TRUE, FALSE), collapse='')))
                fits <- grep(arg, args, fixed=TRUE)
                ## cat('args', args, 'fits', fill=TRUE)
                if(length(fits))
                    paste(unlist(arg_section[[fits[1]]][[2]], TRUE, FALSE), collapse='')
            }
        }
    }
    funcs <- c(fguess, tryCatch(methods(fguess),
                                warning=function(w) {NULL},
                                error=function(e) {NULL}))
    if(length(funcs) > 1 && length(pos <- grep('default', funcs))){
        funcs <- c(funcs[[pos[[1]]]], funcs[-pos[[1]]])
    }
    i <- 1; found <- FALSE
    out <- 'No help found'
    while(i <= length(funcs) && is.null(out <-
                                            tryCatch(findArgHelp(funcs[[i]], arg),
                                                     warning=function(w) {NULL},
                                                     error=function(e) {NULL})
                                        ))
        i <- i + 1
    cat('\n\n', as.character(out), '\n')
}
