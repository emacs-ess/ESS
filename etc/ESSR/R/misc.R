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


## taken literaly from package "psych" by William Revelle
headtail <- function (x, hlength = 4, tlength = 4, digits = 2, ellipsis = TRUE)
{
    if (is.data.frame(x) | is.matrix(x)) {
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
    }
    else {
        h <- head(x, hlength)
        t <- tail(x, tlength)
        if (ellipsis) {
            head.tail <- rbind(h, "...       ...", t)
        }
        else {
            head.tail <- rbind(h, t)
            head.tail <- as.matrix(head.tail)
        }
    }
    return(head.tail)
}
