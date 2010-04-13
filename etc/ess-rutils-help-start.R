## Hacked help.start() to use with ess-rutils.el
.rutils.help.start <- function (update=FALSE, remote=NULL) {
    home <- if (is.null(remote)) {
        if (tools:::httpdPort == 0L)
            tools::startDynamicHelp()
        if (tools:::httpdPort > 0L) {
            if (update)
                make.packages.html()
            paste("http://127.0.0.1:", tools:::httpdPort, sep="")
        }
        else stop(".rutils.help.start() requires the HTTP server to be running",
                  call.=FALSE)
    } else remote
    paste(home, "/doc/html/index.html", sep="")
}
