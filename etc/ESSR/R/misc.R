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


## modified version of headtail from package "psych" by William Revelle
## Users might find it useful. So don't prefix with .ess.
headtail <- function (x, hlength = 4, tlength = 4, digits = 2, ellipsis = TRUE)
{
    if (is.data.frame(x) | is.matrix(x)) {
        if (nrow(x) <= tlength + hlength){
            print(x)
        } else {
            if (is.matrix(x))
                x <- data.frame(unclass(x))
            nvar <- dim(x)[2]
            dots <- rep("...", nvar)
            h <- data.frame(head(x, hlength))
            t <- data.frame(tail(x, tlength))
            for (i in 1:nvar) {
                if (is.numeric(h[1, i])) {
                    h[i] <- round(h[i], digits)
                    t[i] <- round(t[i], digits)
                }
                else {
                    dots[i] <- NA
                }
            }
            if (ellipsis) {
                head.tail <- rbind(h, ... = dots, t)
            }
            else {
                head.tail <- rbind(h, t)
            }
            print(head.tail)
        }
    } else {
        cat("head(", hlength, "):\n", sep = "")
        print(head(x, hlength))
        if(length(x) > tlength + hlength){
            cat("\ntail(", tlength, "):\n", sep = "")
            print(tail(x, tlength))
        }
    }
    invisible(NULL)
}
