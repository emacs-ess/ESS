
### WEAVING
.ess_weave <- function(command, file, encoding = NULL){
    cmd_symb <- substitute(command)
    if(grepl('knit|purl', deparse(cmd_symb))) require(knitr)
    od <- getwd()
    on.exit(setwd(od))
    setwd(dirname(file))
    frame <- parent.frame()
    if(is.null(encoding))
        eval(bquote(.(cmd_symb)(.(file))), envir = frame)
    else
        eval(bquote(.(cmd_symb)(.(file), encoding = .(encoding))), envir = frame)
}

