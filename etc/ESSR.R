.ess_find_funcs <- function(env){
    objs <- ls(envir = env, all.names = TRUE)
    objs[sapply(objs, exists, envir = env,
                mode = 'function', inherits = FALSE)]
}

.ess_all_functions <-
    function(packages = c()){
        env = parent.frame()
        empty <- emptyenv()
        coll <- list()
        for(p in packages){
            ## package might not be attached
            try({objNS <- .ess_find_funcs(asNamespace(p))
                 objPKG <- .ess_find_funcs(as.environment(paste0('package:', p)))
                 coll[[length(coll) + 1L]] <- paste0(p, ':::`', setdiff(objNS, objPKG), '`')
             }, silent = TRUE)
        }
        while(!identical(empty, env)){
            coll[[length(coll) + 1L]] <- .ess_find_funcs(env)
            env <- parent.env(env)
        }
        grep('^\\.ess|\\.help.ESS', unlist(coll, use.names = F),
             invert = TRUE, value = TRUE)
    }

.ess_dbg_getTracedAndDebugged <-
    function(packages = c()){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        generics <- methods::getGenerics()
        all_traced <- c()
        for(i in seq_along(generics)){
            genf <- methods::getGeneric(generics[[i]], package=generics@package[[i]])
            if(!is.null(genf)){ ## might happen !! v.2.13
                menv <- methods::getMethodsForDispatch(genf)
                traced <- unlist(eapply(menv, is, 'traceable', all.names=TRUE))
                if(length(traced) && any(traced))
                    all_traced <- c(paste(generics[[i]],':', names(traced)[traced],sep=''), all_traced)
                if(!is.null(tfn<-getFunction(generics[[i]], mustFind=FALSE, where = .GlobalEnv))&&is(tfn,  'traceable')) # if the default is traced,  it does not appear in the menv :()
                    all_traced <- c(generics[[i]], all_traced)
            }
        }
        debugged_pkg <- unlist(lapply(packages, function(pkgname){
            ns <- asNamespace(pkgname)
            funcs <- .ess_find_funcs(ns)
            dbged <- funcs[unlist(lapply(funcs, function(f) isdebugged(get(f, envir = ns, inherits = FALSE))))]
            if(length(dbged))
                paste0(pkgname, ':::`', dbged, '`')
        }))
        all <- .ess_all_functions()
        ## traced function don't appear here. Not realy needed and would affect performance.
        which_deb <- lapply(all, function(nm){
            tryCatch(isdebugged(nm), error = function(e) FALSE)
            ## try(eval(substitute(isdebugged(nm), list(nm = as.name(nm)))), silent = T)
        })
        debugged <- all[which(unlist(which_deb, recursive=FALSE, use.names=FALSE))]
        unique(c(debugged_pkg, debugged, all_traced))
    }

.ess_dbg_UntraceOrUndebug <- function(name){
    tr_state <- tracingState(FALSE)
    on.exit(tracingState(tr_state))
    if(grepl('::', name)){
        ## foo:::bar name
        eval(parse(text = sprintf('undebug(%s)', name)))
    }else{
        ## name is a name of a function to be undebugged or has a form
        ## name:Class1#Class2#Class3 for traced methods
        name <- strsplit(name, ':', fixed = TRUE)[[1]]
        if(length(name)>1){
            ## a method
            fun <- name[[1]]
            sig <- strsplit(paste(name[-1], collapse=''), '#', fixed=TRUE)[[1]]
            untrace(fun, signature = sig)
        }else{
            ## function
            if(is(getFunction(name), 'traceable'))
                untrace(name)
            else
                undebug(name)
        }}}
    

.ess_dbg_UndebugALL <- function(funcs){
    tr_state <- tracingState(FALSE)
    on.exit(tracingState(tr_state))
    invisible(lapply(funcs, .ess_dbg_UntraceOrUndebug))
}

## inject_env <- .GlobalEnv ##.BaseNamespaceEnv
## environment(.ess_dbg_UndebugALL) <-
##     environment(.ess_dbg_UntraceOrUndebug) <-
##     environment(.ess_dbg_getTracedAndDebugged) <- .GlobalEnv  ## to see all the funcs
## assign('.ess_dbg_getTracedAndDebugged', .ess_dbg_getTracedAndDebugged, envir= inject_env)
## assign('.ess_dbg_UntraceOrUndebug', .ess_dbg_UntraceOrUndebug, envir= inject_env)
## assign('.ess_dbg_UndebugALL', .ess_dbg_UndebugALL, envir= inject_env)
assign('.ESSBP.', new.env(parent = emptyenv()), envir= .BaseNamespaceEnv)
