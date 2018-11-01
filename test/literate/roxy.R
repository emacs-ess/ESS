
##### Filling

### 1a ---------------------------------------------------------------

##' Title
##'
##' @param¶ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
##' @param Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
NULL

##> (ess-roxy-mode)
##! (fill-paragraph)

##' Title
##'
##' @param¶ Lorem ipsum dolor sit amet, consectetur adipiscing elit,
##'     sed do eiusmod tempor incididunt ut labore et dolore magna
##'     aliqua. Ut enim ad minim veniam, quis nostrud exercitation
##'     ullamco laboris nisi ut aliquip ex ea commodo consequat.
##' @param Lorem ipsum dolor sit amet, consectetur adipiscing elit,
##'     sed do eiusmod tempor incididunt ut labore et dolore magna
##'     aliqua. Ut enim ad minim veniam, quis nostrud exercitation
##'     ullamco laboris nisi ut aliquip ex ea commodo consequat.
NULL


### 1b ---------------------------------------------------------------

##' Title
##'
¶##' @param Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
##' @param Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
NULL

##! (fill-paragraph)

##' Title
##'
¶##' @param Lorem ipsum dolor sit amet, consectetur adipiscing elit,
##'     sed do eiusmod tempor incididunt ut labore et dolore magna
##'     aliqua. Ut enim ad minim veniam, quis nostrud exercitation
##'     ullamco laboris nisi ut aliquip ex ea commodo consequat.
##' @param Lorem ipsum dolor sit amet, consectetur adipiscing elit,
##'     sed do eiusmod tempor incididunt ut labore et dolore magna
##'     aliqua. Ut enim ad minim veniam, quis nostrud exercitation
##'     ullamco laboris nisi ut aliquip ex ea commodo consequat.
NULL


### 2 ----------------------------------------------------------------

##' ¶

##! "RET"

##'
##' ¶

##> "RET"

##'
##'
##' ¶

##> (setq ess-roxy-insert-prefix-on-newline nil)
##> "RET"

##'
##'
##'
¶

##! "M-j"

##'
##' ¶


### 3 ----------------------------------------------------------------

##' @param¶ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor `incididunt' ut labore et ``dolore'' magna aliqua.
NULL

##! (fill-paragraph)

##' @param¶ Lorem ipsum dolor sit amet, consectetur adipiscing elit,
##'     sed do eiusmod tempor `incididunt' ut labore et ``dolore''
##'     magna aliqua.
NULL



##### Fontification of roxy blocks

### 1 Param keyword --------------------------------------------------

##' ¶@param foo
NULL

##! (should (memq 'font-lock-keyword-face (faces-at-point)))

##' ¶@param foo
NULL

##> (forward-char)
##> (should (memq 'font-lock-keyword-face (faces-at-point)))

##' @¶param foo
NULL

##> (forward-word)
##> (forward-char)
##> (should (not (memq 'font-lock-keyword-face (faces-at-point))))
##> (should (memq 'font-lock-variable-name-face (faces-at-point)))

##' @param ¶foo
NULL


### 2 Comma-separated params -----------------------------------------

##' @param ¶foo,bar baz
NULL

##! (should (memq 'font-lock-variable-name-face (faces-at-point)))

##' @param ¶foo,bar baz
NULL

##> (forward-word)
##> (should (memq 'font-lock-variable-name-face (faces-at-point)))

##' @param foo¶,bar baz
NULL

##> (forward-char)
##> (should (memq 'font-lock-variable-name-face (faces-at-point)))

##' @param foo,¶bar baz
NULL

##> (forward-word)
##> (forward-char)
##> (should (not (memq 'font-lock-variable-name-face (faces-at-point))))

##' @param foo,bar ¶baz
NULL

