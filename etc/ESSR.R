{if ("ESSR" %in% search()) detach("ESSR") # good for debugging ESSR
 ESSR <- attach(NULL, pos = 5, name = "ESSR", warn.conflicts = FALSE)
 ESSR$.ESSR_Env <- ESSR
 ESSR$.ess_evalq <- function(expr){
     ## like evalq but change the enclosure
     .Internal(eval(substitute(expr), .ESSR_Env, baseenv()))
     invisible(lapply(.ESSR_Env$.ess_find_funcs(.ESSR_Env),
                      function(fun){
                          environment(.ESSR_Env[[fun]]) <- .BaseNamespaceEnv
                      }))
 }
 remove(ESSR)

 .ess_evalq({

### BASICS
     .ess_help <- function(..., help_type = getOption('help_type')){
         if (getRversion() > '2.10')
             utils::help(..., help_type = help_type)
         else
             utils::help(..., htmlhelp = (help_type=='html'))
     }

     .ess_funargs <- function(funname){
         if(getRversion() > '2.14.1'){
             comp <- compiler::enableJIT(0L)
             olderr <- getOption('error')
             options(error=NULL)
             on.exit({
                 compiler::enableJIT(comp)
                 options(error = olderr)
             })
         }
         ## don't remove; really need eval(parse(  here!!
         fun <- tryCatch(eval(parse(text=funname)),
                         error=function(e) NULL) ## works for special objects also
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
             fmls <- gsub('\"', '\\\"', as.character(fmls), fixed=TRUE)
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
             cat(sprintf('(list \"%s\" %s %s)\n',
                         envname, args_alist, allargs))
         }
     }

     .ess_get_completions <- function(string, end){
         if(getRversion() > '2.14.1'){
             comp <- compiler::enableJIT(0L)
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

### WEAVING
     .ess_weave <- function(command, file, encoding = NULL){
         if(grepl('knit|purl', deparse(substitute(command))))
             require(knitr)
         od <- getwd()
         on.exit(setwd(od))
         setwd(dirname(file))
         if(is.null(encoding))
             command(file)
         else
             command(file, encoding = encoding)
     }


### DEBUG/UNDEBUG
     .ess_find_funcs <- function(env)
     {
         objs <- ls(envir = env, all.names = TRUE)
         objs[sapply(objs, exists, envir = env,
                     mode = 'function', inherits = FALSE)]
     }

     .ess_all_functions <- function(packages = c(), env = NULL)
     {
         if(is.null(env))
             env <- parent.frame()
         empty <- emptyenv()
         coll <- list()
         for(p in packages){
             ## package might not be attached
             try({objNS <- .ess_find_funcs(asNamespace(p))
                  objPKG <- .ess_find_funcs(as.environment(paste0('package:', p)))
                  coll[[length(coll) + 1L]] <-
                      paste0(p, ':::`', setdiff(objNS, objPKG), '`')
              }, silent = TRUE)
         }
         while(!identical(empty, env)){
             coll[[length(coll) + 1L]] <- .ess_find_funcs(env)
             env <- parent.env(env)
         }
         grep('^\\.ess', unlist(coll, use.names = FALSE),
              invert = TRUE, value = TRUE)
     }

     .ess_dbg_getTracedAndDebugged <- function(packages = c())
     {
         tr_state <- tracingState(FALSE)
         on.exit(tracingState(tr_state))
         generics <- methods::getGenerics()
         all_traced <- c()
         for(i in seq_along(generics)){
             genf <- methods::getGeneric(generics[[i]],
                                         package=generics@package[[i]])
             if(!is.null(genf)){ ## might happen !! v.2.13
                 menv <- methods::getMethodsForDispatch(genf)
                 traced <- unlist(eapply(menv, is, 'traceable', all.names=TRUE))
                 if(length(traced) && any(traced))
                     all_traced <- c(paste(generics[[i]],':',
                                           names(traced)[traced],sep=''), all_traced)
                 tfn <- getFunction(generics[[i]], mustFind=FALSE, where = .GlobalEnv)
                 if(!is.null(tfn ) && is(tfn,  'traceable')) # if the default is traced,  it does not appear in the menv :()
                     all_traced <- c(generics[[i]], all_traced)
             }
         }
         debugged_pkg <- unlist(lapply(packages, function(pkgname){
             ns <- asNamespace(pkgname)
             funcs <- .ess_find_funcs(ns)
             dbged <- funcs[unlist(lapply(funcs,
                                          function(f){
                                              isdebugged(get(f, envir = ns, inherits = FALSE))
                                          }))]
             if(length(dbged))
                 paste0(pkgname, ':::`', dbged, '`')
         }))
         env <- parent.frame()
         ## traced function don't appear here. Not realy needed and would affect performance.
         all <- .ess_all_functions(packages = packages, env = env)
         which_deb <- lapply(all, function(nm){
             ## if isdebugged is called with string it doess find
	     tryCatch(isdebugged(get(nm, envir = env)),
		      error = function(e) FALSE)
             ## try(eval(substitute(isdebugged(nm), list(nm = as.name(nm)))), silent = T)
         })
         debugged <- all[which(unlist(which_deb, recursive=FALSE, use.names=FALSE))]
         unique(c(debugged_pkg, debugged, all_traced))
     }

     .ess_dbg_UntraceOrUndebug <- function(name, env = parent.frame())
     {
         tr_state <- tracingState(FALSE)
         on.exit(tracingState(tr_state))
         if( grepl('::', name) ){
             ## foo:::bar name
             eval(parse(text = sprintf('undebug(%s)', name)))
         }else{
             ## name is a name of a function to be undebugged or has a form
             ## name:Class1#Class2#Class3 for traced methods
             name <- strsplit(name, ':', fixed = TRUE)[[1]]
             if( length(name)>1 ){
                 ## a method
                 fun <- name[[1]]
                 sig <- strsplit(paste(name[-1], collapse=''), '#', fixed=TRUE)[[1]]
                 untrace(fun, signature = sig)
             }else{
                 ## function
                 if( is(getFunction(name, where = parent.frame()), 'traceable') )
                     untrace(name)
                 else if(grepl(":", name))
                     undebug(name)
                 else
                     undebug(get(name, envir = env))
             }}}

     .ess_dbg_UndebugALL <- function(funcs)
     {
         tr_state <- tracingState(FALSE)
         on.exit(tracingState(tr_state))
         env <- parent.frame()
         invisible(lapply(funcs, function( nm ){
             ## ugly tryCatch, but there might be several names pointing to the
             ## same function, like foo:::bar and bar. An alternative would be
             ## to call .ess_dbg_getTracedAndDebugged each time but that might
             ## be ery slow
             try(.ess_dbg_UntraceOrUndebug(nm, env = env), TRUE)
         }))
     }

### WATCH
     .ess_watch_expressions <- list()

     .ess_watch_eval <- function()
     {
         if(!exists('.ess_watch_expressions')){
             assign('.ess_watch_expressions', list(), envir = .GlobalEnv)
         }
         if(length(.ess_watch_expressions) == 0L){
             cat('\n# Watch list is empty!\n
# a       append new expression
# i       insert new expression
# k       kill
# e       edit the expression
# r       rename
# n/p     navigate
# u/d,U   move the expression up/down
# q       kill the buffer
')
         }else{
             .parent_frame <- parent.frame()
             .essWEnames <- allNames(.ess_watch_expressions)
             len0p <- !nzchar(.essWEnames)
             .essWEnames[len0p] <- seq_along(len0p)[len0p]
             for(i in seq_along(.ess_watch_expressions)){
                 cat('\n@---- ', .essWEnames[[i]], ' ',
                     rep.int('-', max(0, 35 - nchar(.essWEnames[[i]]))), '-@\n', sep = '')
                 cat(paste('@---:', deparse(.ess_watch_expressions[[i]][[1L]])), ' \n', sep = '')
                 tryCatch(print(eval(.ess_watch_expressions[[i]],
                                     envir = .parent_frame)),
                          error = function(e) cat('Error:', e$message, '\n' ),
                          warning = function(w) cat('warning: ', w$message, '\n' ))
             }}
     }

     .ess_log_eval <- function(log_name)
     {
         if(!exists(log_name, envir = .GlobalEnv, inherits = FALSE))
             assign(log_name, list(), envir = .GlobalEnv)
         log <- get(log_name, envir = .GlobalEnv, inherits = FALSE)
         .essWEnames <- allNames(.ess_watch_expressions)
         cur_log <- list()
         .parent_frame <- parent.frame()
         for(i in seq_along(.ess_watch_expressions)){
             capture.output({
                 cur_log[[i]] <-
                     tryCatch(eval(.ess_watch_expressions[[i]]),
                              envir = .parent_frame,
                              error = function(e) paste('Error:', e$message, '\n'),
                              warning = function(w) paste('warning: ', w$message, '\n'))
                 if(is.null(cur_log[i][[1]]))
                     cur_log[i] <- list(NULL)
             })
         }
         names(cur_log) <- .essWEnames
         assign(log_name, c(log, list(cur_log)), envir = .GlobalEnv)
         invisible(NULL)
     }
 })}

## length(ls(.ESSR_Env, all = TRUE)) # VS[01-05-2013]: 13 functs
