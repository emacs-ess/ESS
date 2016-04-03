## Hacked help.start() to use with ess-rutils.el
.rutils.help.start <- function (update=FALSE, remote=NULL) {
    home <- if (is.null(remote)) {
                port <- tools::startDynamicHelp(NA)
                if (port > 0L) {
                    if (update)
                        make.packages.html(temp=TRUE)
                    paste0("http://127.0.0.1:", port)
                }
                else stop(".rutils.help.start() requires the HTTP server to be running",
                          call.=FALSE)
            } else remote
    paste0(home, "/doc/html/index.html")
}
