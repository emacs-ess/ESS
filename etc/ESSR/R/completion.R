### UTILS
.ess_start_with <- function(strings, str, exact = F){
    if(!exact){
        strings <- tolower(strings)
        str <- tolower(str)
    }
   n <- nchar(string)
   (nchar(strings) >= n) & (substring(strings, 1, n) == str)
}

.ess_get_in <- function(obj, path){
    len <- length(path)
    if(len == 1) obj[[path]]
    else if(len > 0) .ess_get_in(obj[[path[[1]]]], path[-1])
    else NULL
}

.ess_cursor_path <- function(expr, path = c()){
    cursor <- as.name("._.")
    ## reverting, cursor is likely to be in the tail position
    for(i in rev(seq_along(expr))){
        el <- expr[[i]]
        if(!missing(el)){
            if(is.recursive(el)){
                out <- Recall(el, c(path, i))
                if(!is.null(out))
                    return(out)
            } else if (el == cursor){
                if(expr[[1]] == as.name("(")){
                    .ess_comp_env[["IS_CALL"]] <- TRUE
                    return(path)
                } else {
                    .ess_comp_env[["IS_CALL"]] <- FALSE
                    return(c(path, i))
                }
            }
        }
    }
}



### HANDLERS
.ess_comp_fun_handler <- function(my_loc, completions){
    funname <- .ess_get_in(EXPR, c(my_loc, 1))
    if(is.symbol(funname)) funname <- as.character(funname)
    else stop("invalid 'EXPR' or 'my_loc' argument")
    ## avoid completing for {, ( 
    if(nchar(funname) > 1)
        completions[[funname]] <- .ess_comp_args(funname)
    completions
}

.ess_comp_obj_handler <- function(my_loc, completions){
    if(nzchar(TOKEN))
        completions[[TOKEN]] <- .ess_comp_object(funname, force = FALSE)
    completions
}

.ess_comp_make_op_handler <- function(operator){
        function(my_loc, completions){
            e <- .ess_get_in(EXPR, my_loc)
            if (IS_CALL) {
                completions[[operator]] <- .ess_comp_args(e)
            } else {
                ## this is probably good engough
                completions <- .ess_comp_obj_handler(my_loc, completions)
            }
            completions
        }
}

.ess_comp_bracket_handler <- function(my_loc, completions){
    arg1 <- .ess_get_in(EXPR, c(my_loc, 2))
    if(is.symbol(arg1)){
        data <- eval(arg1)

        if(is(data, "data.table")){
            completions[["["]] <- list(colnames = names(data))
        } else if(nchar(TOKEN) > 1) {
            ## Start completing on at least 2 characters. There could be milions
            ## of row names.
            pos <- tail(my_loc, 1)
            names <-
                if(is.null(dim(data))){
                    names(data)
                } else if (pos <= length(dim(dim)) + 1){
                    comps <- dimnames(data)[[pos]]
                } else NULL

            if(!is.null(names)){
                names <- names[.ess_start_with(names, TOKEN)]
                if(length(names))
                    completions[["["]] <- list(meta = list(wrap = T),
                                               names = head(names, 1000))
            }
        }
    }
    
    completions
}

.ess_comp_tilda_handler <- function(my_loc, completions){
    call <- .ess_get_in(EXPR, my_loc[-length(my_loc)])
    if(!is.null(call)){
        data <- match.call(function(data = NULL, ...){}, call, expand.dots = F)$data
        if(!is.null(data)){
            completions[["~"]] <- list(names = names(eval(data)))
        }
    }
    completions
}

.ess_comp_make_indata_handler <- function(call_name, data_name = "data"){
    function(my_loc, completions){
        call <- .ess_get_in(EXPR, my_loc)
        if(!is.null(call)){
            dummy <- function(dname = NULL, ...){}
            names(formals(dummy))[[1]] <- data_name
            data <- match.call(dummy, call, expand.dots = F)[[data_name]]
            if(!is.null(data)){
                if(!is.null(names <- names(eval(data)))){
                    completions[[call_name]] <- list(names = names)
                }
            }
        }
        completions
    }
}


### COMPLETION
.ess_comp_args <- function(funname) {

    if(is.character(funname)){
        fun <- eval(parse(text=funname)) # works for special objects containing @:$ etc
    } else if (is.language(funname)){
        fun <- eval(funname)
        funname <- deparse(funname)
    } else stop("invalid funname object")


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

        formals <- formals(args)
        
        all <-
            if(special) names(formals)
            else tryCatch(gsub('=', '', utils:::functionArgs(funname, ''), fixed = TRUE),
                          error=function(e) NULL)
        
        envname <- environmentName(environment(fun))
        if(envname == "R_GlobalEnv") envname <- ""

        list(cache = envname, args = formals, allargs = all)
    }
}

.ess_comp_object <- function(expr, pos = NULL, force = FALSE){
    if(force || !nzchar(TOKEN))
        .ess_set_token(expr, pos)
    utils:::.completeToken()
    list(objects = utils:::.retrieveCompletions())
}

.ess_set_token <- function(text, pos = NULL, force = FALSE){
    if(is.language(text))
        text <- deparse(text)
    utils:::.assignLinebuffer(text)
    if(is.null(pos))
        pos <- nchar(text)
    utils:::.assignEnd(pos)
    utils:::.guessTokenFromLine()
    token <- get('token', envir=utils:::.CompletionEnv)
    .ess_comp_env[["TOKEN"]] <- token
    token
}

.ess_comp_set_env <- function(text){
    expr <- parse(text = text, keep.source = F)

    ## sets IS_CALL
    path <- .ess_cursor_path(expr)
    
    start <- regexpr("(._.)", text, fixe = TRUE) - 1

    if(length(path) == 0L || start < 0)
        stop("No cursor found")

    list2env(list(TEXT = text,
                  EXPR = expr,
                  PATH = path,
                  PATH_LEN = length(path),
                  TOKEN_START = start, 
                  TOKEN = NULL),
             .ess_comp_env)

    ## sets the TOKEN
    .ess_set_token(text, start)
}

.ess_complete <- function(text){

    .ess_comp_set_env(text)
    path <- PATH[-PATH_LEN]

    completions <- list()
    
    ## run handlers in inner -> outer order
    while(length(path)){
        loc_path <- path
        caller <- .ess_get_in(EXPR, loc_path)[[1]]
        while(!is.symbol(caller)){
            caller <- caller[[1]]
            loc_path <- c(loc_path, 1)
        }
        
        fun <- .ess_comp_handlers[[as.character(caller)]]

        if(is.null(fun)){
            ## default handler
            completions <-
                if(IS_CALL) .ess_comp_fun_handler(loc_path, completions)
                else .ess_comp_obj_handler(loc_path, completions)
        } else {
            completions <- fun(loc_path, completions)
        }
        
        path <- path[-length(path)]
    }
    
    completions
}


.ess_comp_env <- globalenv()
## .ess_comp_env <- new.env(FALSE)
for(nm in ls(all.names = T, pattern = "^\\.ess_comp")){
    ## set environment of all completion functions to .ess_comp_env
    if(is.function(get(nm)))
        eval(bquote(environment(.(nm)) <- .ess_comp_env, list(nm = as.name(nm))))
}

.ess_comp_handlers <- list(
    "::" = .ess_comp_make_op_handler("::"), 
    "$" = .ess_comp_make_op_handler("$"), 
    "@" = .ess_comp_make_op_handler("@"), 
    "[" =  .ess_comp_bracket_handler,    
    "~" =  .ess_comp_tilda_handler,
    "with" = .ess_comp_make_indata_handler("with"), 
    "within" = .ess_comp_make_indata_handler("within")
)
