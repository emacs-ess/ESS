;;;; Things that go wrong or *went* wrong in the past
;;;; (from list side) see R-ESS-bugs.R for the R's side.


 ;;;; 1 ess-get-words-from-vector stumbles over \"
(ess-get-words-from-vector "c('aaa','bbb\"ccc', 'dddd')\n")
;;-> ("      " "ccc" "dddd"): SOLVED


;;;; 2 ess-get-words-from-vector disregards max.print
;; options(max.print=1000) (warning added to the docs)
(length (ess-get-words-from-vector "as.character(1:10000)\n"))
;;-> 1001 with "max.print" at the end; added a comment in the function doc

;;;; 3 Inferior-ess-primary-prompt does not capture "+ + > "
;;     this hangs emacs; SOLVED
(ess-command    "tf<-function(N){
N}\n")

;;;;  4 ess-command detects the prompt prematurely
;;   this outputs str(iris) in the inferior buffer; SOLVED
(ess-command "
lm_test <- function (formula, data, subset, weights, na.action, method = 'qr',
          model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
          contrasts = NULL, offset, ...)
{
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c('formula', 'data', 'subset', 'weights', 'na.action',
                 'offset'), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name('model.frame')
    mf <- eval(mf, parent.frame())
    if (method == 'model.frame')
        return(mf)
    else if (method != 'qr')
        warning(gettextf('method is not supported. Using',
                         method), domain = NA)
    mt <- attr(mf, 'terms')
    y <- model.response(mf, 'numeric')
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w))
        stop('weights must be a numeric vector')
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
        if (length(offset) != NROW(y))
            stop(gettextf('number of offsets is %d, should equal %d (number of observations)',
                          length(offset), NROW(y)), domain = NA)
    }
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = if (is.matrix(y)) matrix(, 0,
                    3) else numeric(0L), residuals = y, fitted.values = 0 *
                  y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w !=
                                               0) else if (is.matrix(y)) nrow(y) else length(y))
        if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
        x <- model.matrix(mt, mf, contrasts)
        z <- if (is.null(w))
            lm.fit(x, y, offset = offset, singular.ok = singular.ok,
                   ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
                     ...)
    }
    class(z) <- c(if (is.matrix(y)) 'mlm', 'lm')
    z$na.action <- attr(mf, 'na.action')
    z$offset <- offset
    z$contrasts <- attr(x, 'contrasts')
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model)
        z$model <- mf
    if (ret.x)
        z$x <- x
    if (ret.y)
        z$y <- y
    if (!qr)
        z$qr <- NULL
    z
}
str(iris)
")

;;;; 5 double prompt > > used to stall emacs; SOLVED
(ess-command "\n\n\n")
