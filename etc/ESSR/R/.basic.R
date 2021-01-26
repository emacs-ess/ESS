#### Essential functionality needed by ESS

## Should work on *all* vesions of R.
## Do not use _ in names, nor :: , nor 1L etc, as they
## cannot be parsed in old R versions

.ess.getRversion <- function() {
    if(exists("getRversion", mode="function")) getRversion()
    else paste(R.version$major, R.version$minor, sep=".")
}

## loading ESSR.rda might fail, so re-assign here:
.ess.Rversion <- .ess.getRversion()

.ess.R.has.utils <- (.ess.Rversion >= "1.9.0")
.ess.utils.name <- paste("package",
                         if(.ess.Rversion >= "1.9.0") "utils" else "base",
                         sep = ":")

## Instead of modern  utils::help use one that works in R 1.0.0:
.ess.findFUN   <- get("find", .ess.utils.name)


### HELP
.ess.help <- function(..., help.type = getOption("help_type")) {
    if (is.null(help.type)) {
        help.type <- "text"
    }

    ## - get("help", ..) searching in global env works with devtools redefines
    ## - Redefining to .ess.help this way is necessary because
    ##   utils:::print.help_files_with_topic (used internally when there's
    ##   more than one a package) uses the quoted call
    ##   MM: don't understand; more specifically?
    .ess.help <- function(...) {
        do.call(get("help", envir = .GlobalEnv), list(...))
    }

    if (.ess.Rversion > "2.10") {
        ## Abbreviating help_type to avoid underscore
        .ess.help(..., help = help.type)
    } else {
        .ess.help(..., htmlhelp = help.type == "html")
    }
}

.ess.getHelpAliases <- local({
    readrds <- if (.ess.Rversion >= '2.13.0') readRDS else .readRDS
    aliasesCache <- new.env()

    getAliases <- function(file) {
        cached <- aliasesCache[[file]]
        if (!is.null(cached))
            return(cached)

        aliases <- tryCatch(
            error = function(...) NULL,
            if (file.exists(file))
                names(readrds(file))
            else
                NULL
        )

        aliasesCache[[file]] <- aliases
        aliases
    }

    function(reset = FALSE) {
        if (reset)
            aliasesCache <<- new.env()

        rdsFiles <- paste(searchpaths(), "/help/aliases.rds", sep = "")
        unlist(lapply(rdsFiles, getAliases), use.names = FALSE)
    }
})

.ess.helpLinks <- function(topic, package) {
    tryCatch(
        warning = function(...) NULL,
        error = function(...) NULL,
        {
            ast <- .ess.fetchParsedRd(topic, package)
            .ess.findLinks(ast)
        }
    )
}

.ess.fetchParsedRd <- function(topic, package) {
    path <- utils:::index.search(topic, find.package(package))
    utils:::.getHelpFile(path)
}

.ess.findLinks <- function(x) {
    links <- NULL
    findLinksRec <- function(x) {
        if (.ess.isRdLink(x)) {
            ## Ignore `pkg` for now
            links <<- c(links, .ess.getRdLink(x)[[2]])
        } else if (is.list(x)) {
            for (elt in x)
                findLinksRec(elt)
        }
    }

    findLinksRec(x)
    links
}

.ess.isRdLink <- function(x) {
    identical(attr(x, "Rd_tag"), "\\link")
}

## From `tools:::get_link()`
.ess.getRdLink <- function(x) {
    pkg <- ""
    dest <- paste(unlist(x), collapse = "")

    opt <- attr(x, "Rd_option")
    if (!is.null(opt)) {
        if (grepl("^=", opt, perl = TRUE, useBytes = TRUE)) {
            dest <- sub("^=", "", opt)
        } else if (grepl(":", opt, perl = TRUE, useBytes = TRUE)) {
    	    dest <- sub("^[^:]*:", "", opt)
            pkg <- sub(":.*", "", opt)
        } else {
            pkg <- as.character(opt)
        }
    }

    c(pkg, dest)
}

### SOURCING
.ess.eval <- function(string, visibly = TRUE, output = FALSE,
                      max.deparse.length = 300,
                      file = tempfile("ESS"), local = NULL)
{
    if (is.null(local)) {
        local <- if (.ess.Rversion > '2.13') parent.frame() else FALSE
    }

    ## create FILE, put string into it. Then source.
    ## arguments are like in source and .ess.source
    cat(string, file = file)
    ## The following on.exit infloops in R 3.3.0
    ## https://github.com/emacs-ess/ESS/issues/334
    ## https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16971
    ## So we are cleanning it in .ess.source instead.
    ## on.exit(file.remove(file))
    .ess.source(file, visibly = visibly, output = output,
                max.deparse.length = max.deparse.length,
                local = local, fake.source = TRUE)
}

.ess.strip.error <- function(msg, srcfile) {
    pattern <- paste0(srcfile, ":[0-9]+:[0-9]+: ")
    sub(pattern, "", msg)
}

.ess.file.remove <- function(file){
    if (base::file.exists(file)) base::file.remove(file)
    else FALSE
}

.ess.source <- function(file, visibly = TRUE, output = FALSE,
                        max.deparse.length = 300, local = NULL,
                        fake.source = FALSE, keep.source = TRUE,
                        message.prefix = "") {
    if (is.null(local)) {
        local <- if (.ess.Rversion > "2.13")
            parent.frame()
        else FALSE
    }

    ss <-
        if (.ess.Rversion >= "3.4")
            base::source
        else if (.ess.Rversion >= "2.8")
            function(..., spaced) base::source(...)
        else function(..., spaced, keep.source) base::source(...)

    on.exit({
        if (fake.source)
            .ess.file.remove(file)
    })

    out <- ss(file, echo = visibly, local = local, print.eval = output, spaced = FALSE,
              max.deparse.length = max.deparse.length, keep.source = keep.source)

    if(!fake.source)
        cat(sprintf("%sSourced file %s\n", message.prefix, file))

    ## Return value for org-babel
    invisible(out$value)
}

if(.ess.Rversion < "1.8")
    ## (works for "1.7.2"): bquote() was new in 1.8.0
    bquote <- function(expr, where=parent.frame()){
        unquote <- function(e)
            if (is.pairlist(e)) as.pairlist(lapply(e, unquote))
            else if (length(e) <= 1) e
            else if (e[[1]] == as.name(".")) eval(e[[2]], where)
            else as.call(lapply(e, unquote))

        unquote(substitute(expr))
    }

.ess.command <- function(expr, sentinel) {
    ## It is possible that the REPL is marked as non-busy when the
    ## output is sinked because prompts are not sinked. In that case,
    ## redirect the sinked output temporarily to ESS.
    sinked <- sink.number() != 0
    if (sinked)
        sink(.ess.stdout)

    on.exit({
        writeLines(paste0(sentinel, "-END"))
        if (sinked)
            sink(NULL)
    })

    writeLines(paste0(sentinel, "-START"))

    ## Don't interrupt `browser()` sessions (#1081)
    restart <- function(...) {
        if (!is.null(findRestart("browser")))
            invokeRestart("browser")
    }

    out <- withCallingHandlers(
        interrupt = restart,
        withVisible(expr)
    )

    ## Print result manually because we can't rely on auto-print
    ## without changing the last value
    if (out$visible)
        print(out$value)

    ## Keep `.Last.value` stable
    invisible(.Last.value)
}

## stdout() always returns the current file where output is sinked to.
## If there is any sink, it returns that file rather than connection 1.
## Since we can't get the default stdout connection when a sink is
## active we save it here.
.ess.stdout <- stdout()
