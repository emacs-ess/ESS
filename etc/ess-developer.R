.ess_evalq({

    ## COMMENT ON S3 METHODS: It is not feasible and, quite frankly, a bad practice
    ## to check all the assigned function names for "." separator. Thus, S3 methods
    ## are not automatically registered. You can register them manually after you
    ## have inserted method_name.my_class into your package environment using
    ## ess-developer, like follows:
    ##
    ##    registerS3method("method_name", "my_class", my_package:::method_name.my_class)
    ##
    ## Otherwise R will call the registered (i.e. cached) S3 method instead of the
    ## new method that ess-developer inserted in the package environment.


    .essDev_differs <- function(f1, f2)
    {
        if (is.function(f1) && is.function(f2)){
            !(identical(body(f1), body(f2)) && identical(args(f1), args(f2)))
        }else
            !identical(f1, f2)
    }

    .essDev_source <- function (source, expr, package = "")
    {
        require('methods')
        oldopts <- options(warn = 1)
        on.exit(options(oldopts))
        MPattern <- methods:::.TableMetaPattern()
        CPattern <- methods:::.ClassMetaPattern()
        allPlainObjects <- function() allObjects[!(grepl(MPattern, allObjects) |
                                                   grepl(CPattern, allObjects))]
        allMethodTables <- function() allObjects[grepl(MPattern, allObjects)]
        allClassDefs <- function() allObjects[grepl(CPattern, allObjects)]
        pname <- paste("package:", package, sep = "")
        envpkg <- tryCatch(as.environment(pname), error = function(cond) NULL)
        if(is.null(envpkg)){
            library(package, character.only = TRUE)
            envpkg <- tryCatch(as.environment(pname), error = function(cond) NULL)
        }
        if (is.null(envpkg))
            stop(gettextf("Can't find an environment corresponding to package name '%s'",
                          package), domain = NA)
        envns <- tryCatch(asNamespace(package), error = function(cond) NULL)
        if (is.null(envns))
            stop(gettextf("Can't find a namespace environment corresponding to package name '%s\"",
                          package), domain = NA)
        env <- .essDev_evalSource(source, substitute(expr), package)
        envPackage <- getPackageName(env, FALSE)
        if (nzchar(envPackage) && envPackage != package)
            warning(gettextf("Supplied package, %s, differs from package inferred from source, %s",
                             sQuote(package), sQuote(envPackage)), domain = NA)
        allObjects <- objects(envir = env, all.names = TRUE)
        allObjects <- allObjects[!(allObjects %in% c(".cacheOnAssign", ".packageName"))]

        ## PLAIN OBJECTS and FUNCTIONS:
        funcNs <- funcPkg <- newFunc <- newNs <- newObjects <- newPkg <-
            objectsNs <- objectsPkg <- character()
        for (this in allPlainObjects()) {
            thisEnv <- get(this, envir = env)
            thisNs <- NULL
            ## NS
            if (exists(this, envir = envns, inherits = FALSE)){
                thisNs <- get(this, envir = envns)
                if(is.function(thisNs) || is.function(thisEnv)){
                    if(is.function(thisNs) && is.function(thisEnv)){
                        if(.essDev_differs(thisEnv, thisNs)){
                            environment(thisEnv) <- environment(thisNs)
                            .essDev_assign(this, thisEnv, envns)
                            funcNs <- c(funcNs, this)
                            if(exists(".__S3MethodsTable__.", envir = envns, inherits = FALSE)){
                                S3_table <- get(".__S3MethodsTable__.", envir = envns)
                                if(exists(this, envir = S3_table, inherits = FALSE))
                                    .essDev_assign(this, thisEnv, S3_table)
                            }
                        }
                    }else{
                        newNs <- c(newNs, this)
                    }
                }else{
                    if(!identical(thisEnv, thisNs)){
                        .essDev_assign(this, thisEnv, envns)
                        objectsNs <- c(objectsNs, this)}
                }
            }else{
                newNs <- c(newNs, this)
            }
            ## PKG
            if (exists(this, envir = envpkg, inherits = FALSE)){
                thisPkg <- get(this, envir = envpkg)
                if(is.function(thisPkg) || is.function(thisEnv)){
                    if(is.function(thisPkg) && is.function(thisEnv)){
                        if(.essDev_differs(thisPkg, thisEnv)){
                            environment(thisEnv) <- environment(thisPkg)
                            .essDev_assign(this, thisEnv, envpkg)
                            funcPkg <- c(funcPkg, this)}
                    }else{
                        newPkg <- c(newPkg, this)}
                }else{
                    if(!identical(thisPkg, thisEnv)){
                        .essDev_assign(this, thisEnv, envpkg)
                        objectsPkg <- c(objectsPkg, this)}}
            }else{
                newPkg <- c(newPkg, this)}
        }
        for(this in intersect(newPkg, newNs)){
            thisEnv <- get(this, envir = env, inherits = FALSE)
            if(exists(this, envir = .GlobalEnv, inherits = FALSE)){
                thisGl <- get(this, envir = .GlobalEnv)
                if(.essDev_differs(thisEnv, thisGl)){
                    if(is.function(thisEnv)){
                        environment(thisEnv) <- envns
                        newFunc <- c(newFunc, this)
                    }else{
                        newObjects <- c(newObjects, this)
                    }
                    .essDev_assign(this, thisEnv, .GlobalEnv)
                }
            }else{
                if(is.function(thisEnv)){
                    environment(thisEnv) <- envns
                    newFunc <- c(newFunc, this)
                }else{
                    newObjects <- c(newObjects, this)
                }
                .essDev_assign(this, thisEnv, .GlobalEnv)
            }
        }
        if(length(funcNs))
            objectsNs <- c(objectsNs, sprintf("FUN[%s]", paste(funcNs, collapse = ", ")))
        if(length(funcPkg))
            objectsPkg <- c(objectsPkg, sprintf("FUN[%s]", paste(funcPkg, collapse = ", ")))
        if(length(newFunc))
            newObjects <- c(newObjects, sprintf("FUN[%s]", paste(newFunc, collapse = ", ")))

        ## CLASSES
        classesPkg <- classesNs <- newClasses <- character()
        for(this in allClassDefs()){
            newPkg <- newNs <- FALSE
            thisEnv <- get(this, envir = env)
            if(exists(this, envir = envpkg, inherits = FALSE)){
                if(!.essDev_identicalClass(thisEnv, get(this, envir = envpkg))){
                    .essDev_assign(this, thisEnv, envir = envpkg)
                    classesPkg <- c(classesPkg, this)
                }
            }else{
                newPkg <- TRUE
            }
            if(exists(this, envir = envns, inherits = FALSE)){
                if(!.essDev_identicalClass(thisEnv, get(this, envir = envns))){
                    .essDev_assign(this, thisEnv, envir = envns)
                    classesNs <- c(classesNs, this)
                }
            }else{
                newNs <- TRUE
            }
            if(newNs && newPkg){
                if(exists(this, envir = .GlobalEnv, inherits = FALSE)){
                    if(!.essDev_identicalClass(thisEnv, get(this, envir = .GlobalEnv))){
                        .essDev_assign(this, thisEnv, envir = .GlobalEnv)
                        newClasses <- c(newClasses, this)
                    }
                }else{
                    .essDev_assign(this, thisEnv, envir = .GlobalEnv)
                    newClasses <- c(newClasses, this)
                }
            }
        }
        if(length(classesPkg))
            objectsPkg <- gettextf("CLS[%s]", sub(methods:::.ClassMetaPattern(), "", paste(classesPkg, collapse = ", ")))
        if(length(classesNs))
            objectsNs <- gettextf("CLS[%s]", sub(methods:::.ClassMetaPattern(), "", paste(classesNs, collapse = ", ")))
        if(length(newClasses))
            newObjects <- gettextf("CLS[%s]", sub(methods:::.ClassMetaPattern(), "", paste(newClasses, collapse = ", ")))

        ## METHODS:
        ## Method internals: For efficiency reasons setMethod() caches
        ## method definition into a global table which you can get with
        ## 'getMethodsForDispatch' function, and when a method is dispatched that
        ## table is used. When ess-developer is used to source method definitions the
        ## two copies of the functions are identical up to the environment. The
        ## environment of the cached object has namespace:foo as it's parent but the
        ## environment of the object in local table is precisely namspace:foo. This
        ## does not cause any difference in evaluation.
        methodNames <- allMethodTables()
        methods <- sub(methods:::.TableMetaPrefix(), "", methodNames)
        methods <- sub(":.*", "", methods)
        methodsNs <- newMethods <- character()
        for (i in seq_along(methods)){
            table <- methodNames[[i]]
            tableEnv <- get(table,  envir = env)
            if(exists(table,  envir = envns, inherits = FALSE)){
                inserted <- .essDev_insertMethods(tableEnv, get(table, envir = envns), envns)
                if(length(inserted))
                    methodsNs <- c(methodsNs,  gettextf("%s{%s}", methods[[i]], paste(inserted, collapse = ", ")))
            }else if(exists(table,  envir = .GlobalEnv, inherits = FALSE)){
                inserted <- .essDev_insertMethods(tableEnv, get(table, envir = .GlobalEnv), envns)
                if(length(inserted))
                    newMethods <- c(newMethods,  gettextf("%s{%s}", methods[[i]], paste(inserted, collapse = ", ")))
            }else{
                .essDev_assign(table, tableEnv, envir = .GlobalEnv)
                newMethods <- c(newMethods,  gettextf("%s{%s}", methods[[i]], paste(objects(envir = tableEnv, all = T), collapse = ", ")))
            }
        }
        if(length(methodsNs))
            objectsNs <- c(objectsNs, gettextf("METH[%s]", paste(methodsNs, collapse = ", ")))
        if(length(newMethods))
            newObjects <- c(newObjects, gettextf("METH[%s]", paste(newMethods, collapse = ", ")))

        if(length(objectsPkg))
            cat(sprintf("%s@PKG:\t%s\n", package, paste(objectsPkg, collapse = ", ")))
        if(length(objectsNs))
            cat(sprintf("%s@NS:\t%s\n", package, paste(objectsNs, collapse = ", ")))
        if(length(newObjects))
            cat(sprintf("*GlobalEnv*:\t%s\n", paste(newObjects, collapse = ", ")))
        if(length(c(objectsNs, objectsPkg, newObjects)) == 0L)
            cat(sprintf("*** Nothing explicitly assigned ***\n"))
        invisible(env)
    }

    .essDev_insertMethods <- function(tableEnv,  tablePkg, envns)
    {
        inserted <- character()
        for(m in ls(envir = tableEnv, all = T)){
            if(exists(m, envir = tablePkg, inherits = FALSE)){
                thisEnv <- get(m, envir = tableEnv)
                thisPkg <- get(m, envir = tablePkg)
                if(is(thisEnv, "MethodDefinition") && is(thisPkg, "MethodDefinition") &&
                   .essDev_differs(thisEnv@.Data, thisPkg@.Data)){
                    environment(thisEnv@.Data) <- envns
                    ## environment of cached method in getMethodsForDispatch table is still env
                    ## not a problem as such,  but might confuse users
                    .essDev_assign(m, thisEnv, tablePkg)
                    inserted <- c(inserted, m)
                }}}
        inserted
    }


    .essDev_evalSource <- function (source, expr, package = "")
    {
        envns <- tryCatch(asNamespace(package), error = function(cond) NULL)
        if(is.null(envns))
            stop(gettextf("Package \"%s\" is not attached and no namespace found for it",
                          package), domain = NA)
        env <- new.env(parent = envns)
        env[[".packageName"]] <- package
        methods:::setCacheOnAssign(env, TRUE)
        if (missing(source))
            eval(expr, envir = env)
        else  if (is(source, "character"))
            for (text in source) sys.source(text, envir = env, keep.source = TRUE)
        else stop(gettextf("Invalid source argument:  got an object of class \"%s\"",
                           class(source)[[1]]), domain = NA)
        env
    }


    .essDev_assign <- function (x, value, envir)
    {
        if (exists(x, envir = envir, inherits = FALSE) && bindingIsLocked(x, envir)) {
            unlockBinding(x, envir)
            assign(x, value, envir = envir, inherits = FALSE)
            w <- options("warn")
            on.exit(options(w))
            options(warn = -1)
            lockBinding(x, envir)
        } else {
            assign(x, value, envir = envir, inherits = FALSE)
        }
        invisible(NULL)
    }

    .essDev_identicalClass <- function(cls1, cls2, printInfo = FALSE){
        slots1 <- slotNames(class(cls1))
        slots2 <- slotNames(class(cls2))
        if(identical(slots1, slots2)){
            vK <- grep("versionKey", slots1)
            if(length(vK))
                slots1 <- slots2 <- slots1[-vK]
            out <- sapply(slots1, function(nm) identical(slot(cls1, nm), slot(cls2, nm)))
            if(printInfo) print(out)
            all(out)
        }
    }
})
