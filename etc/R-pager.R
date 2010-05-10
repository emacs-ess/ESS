mypager <- function(files, header, title, delete.file) {
    tfile <- tempfile(paste(basename(files[1]),"__", sep=""))
    Tf <- file(tfile, open="w+")
    stopifnot(file.append(tfile, files))
    system(paste("emacsclient -n", tfile))
}
file.show(file.path(R.home("doc"), "COPYRIGHTS"),
          pager = mypager)

options(pager = mypager)


## test :
file.show(file.path(R.home("doc"), "COPYRIGHTS"))

## or
RShowDoc('NEWS')
## using a suboptimal file name
