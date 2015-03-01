;;; ess-r-completion.el --- R completion
;;
;; Copyright (C) 2015 A.J. Rossini, Richard M. Heiberger, Martin Maechler, Kurt
;;      Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.
;;
;; Author: Vitalie Spinu
;; Maintainer: ESS-core <ESS-core@r-project.org>
;;
;; Keywords: languages, statistics
;;
;; This file is part of ESS.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/
;;
;;; Commentary:
;;
;;; Code:


;;; ELDOC

(defun ess-R-eldoc-function ()
  "Return the doc string, or nil.
If an ESS process is not associated with the buffer, do not try
to look up any doc strings."
  (interactive)
  (let ((proc (ess-get-next-available-process)))
    (when proc
     (let ((funname (or (and ess-eldoc-show-on-symbol ;; aggressive completion
                             (thing-at-point 'symbol))
                        (car (ess--funname.start)))))
       (when funname
         (let* ((args (ess-function-arguments funname proc))
                (bargs (cadr args))
                (doc (mapconcat (lambda (el)
                                  (if (equal (car el) "...")
                                      "..."
                                    (concat (car el) "=" (cdr el))))
                                bargs ", "))
                (margs (nth 2 args))
                (W (- (window-width (minibuffer-window)) (+ 4 (length funname))))
                doc1)
           (when doc
             (setq doc (ess-eldoc-docstring-format funname doc))
             (when (and margs (< (length doc1) W))
               (setq doc1 (concat doc (propertize "  || " 'face font-lock-function-name-face)))
               (while (and margs (< (length doc1) W))
                 (let ((head (pop margs)))
                   (unless (assoc head bargs)
                     (setq doc doc1
                           doc1 (concat doc1 head  "=, ")))))
               (when (equal (substring doc -2) ", ")
                 (setq doc (substring doc 0 -2)))
               (when (and margs (< (length doc) W))
                 (setq doc (concat doc " {--}"))))
             doc)))))))

(defun ess-eldoc-docstring-format (funname doc)
  (save-match-data
    (let* (;; (name (symbol-name sym))
           (truncate (or  (not (eq t eldoc-echo-area-use-multiline-p))
                          (eq ess-eldoc-abbreviation-style 'aggressive)))
           ;; Subtract 1 from window width since will cause a wraparound and
           ;; resize of the echo area.
           (W (1- (- (window-width (minibuffer-window))
                     (+ 2 (length funname)))))
           newdoc
           )
      (setq doc
            (if (or (<= (length doc) W)
                    (null ess-eldoc-abbreviation-style)
                    (eq 'none ess-eldoc-abbreviation-style))
                doc
              ;;MILD filter
              (setq doc (replace-regexp-in-string "TRUE" "T" doc))
              (setq doc (replace-regexp-in-string "FALSE" "F" doc))
              (if (or (<= (length doc) W)
                      (eq 'mild ess-eldoc-abbreviation-style))
                  doc
                ;;NORMAL filter (deal with long defaults)
                (setq doc (replace-regexp-in-string
                           ;; function calls inside default docs foo(xxxx{..})
                           "([^)]\\{8\\}\\([^)]\\{4,\\}\\))"
                           "{.}" doc nil nil 1))
                (if (<= (length doc) W)
                    doc
                  (setq doc (replace-regexp-in-string
                             " +[^ \t=,\"\]+=[^ \t]\\{10\\}\\([^ \t]\\{4,\\}\\)\\(,\\|\\'\\)"
                             "{.}," doc nil nil 1))
                  (if (<= (length doc) W)
                      doc
                    (setq doc (replace-regexp-in-string
                               " +[^ \t=,\"]+=\\([^ \t]\\{10,\\}\\)\\(,\\|\\'\\)"
                               "{.}," doc nil nil 1))
                    (if (or (<= (length doc) W)
                            (eq 'normal ess-eldoc-abbreviation-style))
                        doc
                      ;;STRONG filter (replace defaults)
                      (setq doc (replace-regexp-in-string
                                 " *[^ \t=,\"\\]* = \\([^ \t]\\{4,\\}\\)\\(,\\|\\'\\)"
                                 "{.}," doc nil nil 1))
                      (if (<= (length doc) W)
                          doc
                        (setq doc (replace-regexp-in-string
                                   "\\(=[^FT0-9].+?\\)\\(, [^ =,\"\\]+=\\|\\'\\)"
                                   "" doc nil nil 1))
                        (setq doc (replace-regexp-in-string
                                   "\\(=[^FT0-9].+?\\)\\(, [^ =,\"\\]+,\\|\\'\\)"
                                   "" doc nil nil 1))
                        (if (or (<= (length doc) W)
                                (eq 'strong ess-eldoc-abbreviation-style))
                            doc
                          ;;AGGRESSIVE filter (truncate what is left)
                          (concat (substring doc 0 (- W 4)) "{--}")))))))))
      (when (and truncate
                 (> (length doc) W))
        (setq doc (concat (substring doc 0 (- W 4)) "{--}")))
      (format "%s: %s" (propertize funname 'face 'font-lock-function-name-face) doc))))



;;; ARGUMENTS

(defvar ess-R--funargs-pre-cache
  '(("plot"
     (("graphics")
      (("x" . "")    ("y" . "NULL")    ("type" . "p")    ("xlim" . "NULL")    ("ylim" . "NULL")    ("log" . "")    ("main" . "NULL")    ("sub" . "NULL")    ("xlab" . "NULL")    ("ylab" . "NULL")
       ("ann" . "par(\"ann\")")     ("axes" . "TRUE")    ("frame.plot" . "axes")    ("panel.first" . "NULL")    ("panel.last" . "NULL")    ("asp" . "NA")    ("..." . ""))
      ("x" "y" "..." "ci" "type" "xlab" "ylab" "ylim" "main" "ci.col" "ci.type" "max.mfrow" "ask" "mar" "oma" "mgp" "xpd" "cex.main" "verbose" "scale" "xlim" "log" "sub" "ann" "axes" "frame.plot"
       "panel.first" "panel.last" "asp" "center" "edge.root" "nodePar" "edgePar" "leaflab" "dLeaf" "xaxt" "yaxt" "horiz"
       "zero.line" "verticals" "col.01line" "pch" "legend.text" "formula" "data" "subset" "to" "from" "newpage" "vp" "labels"
       "hang" "freq" "density" "angle" "col" "border" "lty" "add" "predicted.values" "intervals" "separator" "col.predicted"
       "col.intervals" "col.separator" "lty.predicted" "lty.intervals" "lty.separator" "plot.type" "main2" "par.fit" "grid"
       "panel" "cex" "dimen" "abbrev" "which" "caption" "sub.caption" "id.n" "labels.id" "cex.id" "qqline" "cook.levels"
       "add.smooth" "label.pos" "cex.caption" "rows" "levels" "conf" "absVal" "ci.lty" "xval" "do.points" "col.points" "cex.points"
       "col.hor" "col.vert" "lwd" "set.pars" "range.bars" "col.range" "xy.labels" "xy.lines" "nc" "yax.flip" "mar.multi" "oma.multi")))
    ("print"
     (("base")
      (("x" . "")    ("digits" . "NULL")    ("quote" . "TRUE")    ("na.print" . "NULL")    ("print.gap" . "NULL")    ("right" . "FALSE")    ("max" . "NULL")    ("useSource" . "TRUE")    ("..." . ""))
      ("x" "..." "digits" "signif.stars" "intercept" "tol" "se" "sort" "verbose" "indent" "style" ".bibstyle" "prefix" "vsep" "minlevel" "quote" "right" "row.names" "max" "na.print" "print.gap"
       "useSource" "diag" "upper" "justify" "title" "max.levels" "width" "steps" "showEnv" "newpage" "vp" "cutoff" "max.level" "give.attr" "units" "abbrCollate" "print.x" "deparse" "locale" "symbolic.cor"
       "loadings" "zero.print" "calendar"))))
  "Alist of cached arguments for very time consuming functions.")

(defun ess-R-object-completion ()
  "Return completions at point in a format required by `completion-at-point-functions'. "
  (if (ess-make-buffer-current)
      (let* ((funstart (cdr (ess--funname.start)))
             (completions (ess-R-get-rcompletions funstart))
             (token (pop completions)))
        (when completions
          (list (- (point) (length token)) (point) completions)))
    (when (string-match "complete" (symbol-name last-command))
      (message "No ESS process associated with current buffer")
      nil)))

(defun ess-complete-object-name ()
  "Perform completion on `ess-language' object preceding point.
Uses \\[ess-R-complete-object-name] when `ess-use-R-completion' is non-nil,
or \\[ess-internal-complete-object-name] otherwise."
  (interactive)
  (if (ess-make-buffer-current)
      (if ess-use-R-completion
          (ess-R-complete-object-name)
        (ess-internal-complete-object-name))
    ;; else give a message on second invocation
    (when (string-match "complete" (symbol-name last-command))
      (message "No ESS process associated with current buffer")
      nil)))

(defun ess-complete-object-name-deprecated ()
  "Gives a deprecated message "
  (interactive)
  (ess-complete-object-name)
  (message "C-c TAB is deprecated, completions has been moved to [M-TAB] (aka C-M-i)")
  (sit-for 2 t))

;; This one is needed for R <= 2.6.x -- hence *not* obsoleting it
(defun ess-internal-complete-object-name ()
  "Perform completion on `ess-language' object preceding point.
The object is compared against those objects known by
`ess-get-object-list' and any additional characters up to ambiguity are
inserted.  Completion only works on globally-known objects (including
elements of attached data frames), and thus is most suitable for
interactive command-line entry, and not so much for function editing
since local objects (e.g. argument names) aren't known.

Use \\[ess-resynch] to re-read the names of the attached directories.
This is done automatically (and transparently) if a directory is
modified (S only!), so the most up-to-date list of object names is always
available.  However attached dataframes are *not* updated, so this
command may be necessary if you modify an attached dataframe."
  (interactive)
  (ess-make-buffer-current)
  (if (memq (char-syntax (preceding-char)) '(?w ?_))
      (let* ((comint-completion-addsuffix nil)
             (end (point))
             (buffer-syntax (syntax-table))
             (beg (unwind-protect
                      (save-excursion
                        (set-syntax-table ess-mode-syntax-table)
                        (backward-sexp 1)
                        (point))
                    (set-syntax-table buffer-syntax)))
             (full-prefix (buffer-substring beg end))
             (pattern full-prefix)
             ;; See if we're indexing a list with `$'
             (listname (if (string-match "\\(.+\\)\\$\\(\\(\\sw\\|\\s_\\)*\\)$"
                                         full-prefix)
                           (progn
                             (setq pattern
                                   (if (not (match-beginning 2)) ""
                                     (substring full-prefix
                                                (match-beginning 2)
                                                (match-end 2))))
                             (substring full-prefix (match-beginning 1)
                                        (match-end 1)))))
             ;; are we trying to get a slot via `@' ?
             (classname (if (string-match "\\(.+\\)@\\(\\(\\sw\\|\\s_\\)*\\)$"
                                          full-prefix)
                            (progn
                              (setq pattern
                                    (if (not (match-beginning 2)) ""
                                      (substring full-prefix
                                                 (match-beginning 2)
                                                 (match-end 2))))
                              (ess-write-to-dribble-buffer
                               (format "(ess-C-O-Name : slots..) : patt=%s"
                                       pattern))
                              (substring full-prefix (match-beginning 1)
                                         (match-end 1)))))
             (components (if listname
                             (ess-object-names listname)
                           (if classname
                               (ess-slot-names classname)
                             ;; Default case: It hangs here when
                             ;;    options(error=recover) :
                             (ess-get-object-list ess-current-process-name)))))
        ;; always return a non-nil value to prevent history expansions
        (or (comint-dynamic-simple-complete  pattern components) 'none))))

(defun ess-R-get-rcompletions (&optional start end)
  "Call R internal completion utilities (rcomp) for possible completions.
Optional START and END delimit the entity to complete, default to
bol and point. First element of a returned list is the completion
token. Needs version of R>2.7.0 "
  (let* ((start (or start
                    (save-excursion (comint-bol nil) (point))))
         (end (or end (point)))
         ;; (opts1 (if no-args "op<-rc.options(args=FALSE)" ""))
         ;; (opts2 (if no-args "rc.options(op)" ""))
         (comm (format ".ess_get_completions(\"%s\", %d)\n"
                (ess-quote-special-chars (buffer-substring start end))
                (- end start))))
    (ess-get-words-from-vector comm)))

(defun ess-R-complete-object-name ()
  "Completion in R via R's completion utilities (formerly 'rcompgen').
To be used instead of ESS' completion engine for R versions >= 2.7.0."
  (interactive)
  (let ((possible-completions (ess-R-get-rcompletions))
        token-string)
    ;; If there are no possible-completions, should return nil, so
    ;; that when this function is called from
    ;; comint-dynamic-complete-functions, other functions can also be
    ;; tried.
    (when possible-completions
      (setq token-string (pop possible-completions))
      (or (comint-dynamic-simple-complete token-string
                                          possible-completions)
          'none))))


;;; AC SOURCES
;;; http://cx4a.org/software/auto-complete/index.html
(defvar ac-source-R
  '((prefix     . ess-ac-start)
    ;; (requires   . 0) ::)
    (candidates . ess-ac-candidates)
    (document   . ess-ac-help)
    ;; (action  . ess-ac-action-args) ;; interfere with ac-fallback mechanism on RET (which is extremely annoing in inferior buffers)
    )
  "Combined ad-completion source for R function arguments and R objects")

(defun ess-ac-start ()
  (when (and ess-local-process-name
             (get-process ess-local-process-name))
    (or (ess-ac-start-args)
        (ess-symbol-start))))

(defun ess-ac-candidates ()
  "OBJECTS + ARGS"
  (let ((args (ess-ac-args)))
    ;; sort of intrusive but right
    (if (and ac-auto-start
             (< (length ac-prefix) ac-auto-start))
        args
      (if args
          (append args (ess-ac-objects t))
        (ess-ac-objects)))))

(defun ess-ac-help (sym)
  (if (string-match-p "= *\\'" sym)
      (ess-ac-help-arg sym)
    (ess-ac-help-object sym)))

;; OBJECTS
(defvar  ac-source-R-objects
  '((prefix     . ess-symbol-start)
    ;; (requires   . 2)
    (candidates . ess-ac-objects)
    (document   . ess-ac-help-object))
  "Auto-completion source for R objects")

(defun ess-ac-objects (&optional no-kill)
  "Get all cached objects"
 (let ((aprf ac-prefix))
   (when (and aprf (ess-process-live-p))
     (unless no-kill ;; workaround
       (kill-local-variable 'ac-use-comphist))
     (if (string-match-p "[]:$@[]" aprf)
         ;; call proc for objects
         (cdr (ess-R-get-rcompletions ac-point))
       ;; else, get the (maybe cached) list of objects
       (with-ess-process-buffer 'no-error ;; use proc buf alist
         (ess-when-new-input last-objlist-update
           (if (and ess-sl-modtime-alist
                    (not  (process-get *proc* 'sp-for-ac-changed?)))
               ;; not changes, re-read .GlobalEnv
               (ess-extract-onames-from-alist ess-sl-modtime-alist 1 'force))
           ;; reread all objects, but not rda, much faster and not needed anyways
           (ess-get-modtime-list)
           (process-put *proc* 'sp-for-ac-changed? nil))
         (apply 'append (mapcar 'cddr ess-sl-modtime-alist)))))))

(defun ess-ac-help-object (sym)
  "Help string for ac."
  (let ((buf (get-buffer-create " *ess-command-output*")))
    (when (string-match ":+\\(.*\\)" sym)
      (setq sym (match-string 1 sym)))
    (ess-with-current-buffer buf
      (ess--flush-help-into-current-buffer sym nil t))
    (with-current-buffer buf
      (ess-help-underline)
      (goto-char (point-min))
      (buffer-string))))

;; ARGS
(defvar  ac-source-R-args
  '((prefix     . ess-ac-start-args)
    ;; (requires   . 0)
    (candidates . ess-ac-args)
    (document   . ess-ac-help-arg)
    ;; (action     . ess-ac-action-args)
    )
  "Auto-completion source for R function arguments")

(defun ess-ac-start-args ()
  "Get initial position for args completion"
  (when (and (ess-process-live-p)
             (not (eq (get-text-property (point) 'face) 'font-lock-string-face)))
    (when (ess--funname.start)
      (if (looking-back "[(,]+[ \t\n]*")
          (point)
        (ess-symbol-start)))))

(defun ess-ac-args ()
  "Get the args of the function when inside parentheses."
  (when  (and ess--funname.start ;; set in a call to ess-ac-start-args
              (ess-process-live-p))
    (let ((args (nth 2 (ess-function-arguments (car ess--funname.start))))
          (len (length ac-prefix)))
      (if args
          (set (make-local-variable 'ac-use-comphist) nil)
        (kill-local-variable 'ac-use-comphist))
      (delete "..." args)
      (mapcar (lambda (a) (concat a ess-ac-R-argument-suffix))
              args))))

(defun ess-ac-help-arg (sym)
  "Help string for ac."
  (setq sym (replace-regexp-in-string " *= *\\'" "" sym))
  (let ((fun (car ess--funname.start)))
    (with-current-buffer (ess-command (format ess--ac-help-arg-command sym fun))
      (goto-char (point-min))
      (forward-line)
      (buffer-substring-no-properties (point) (point-max)))))

(defvar ess--ac-help-arg-command
  "
getArgHelp <- function(arg, func=NULL){
    olderr <- getOption('error')
    options(error=NULL)
    on.exit(options(error=olderr))
    fguess <-
        if(is.null(func)) get('fguess', envir=utils:::.CompletionEnv)
        else func
    findArgHelp <- function(fun, arg){
        file <- help(fun, try.all.packages=FALSE)[[1]]
        hlp <- utils:::.getHelpFile(file)
        id <- grep('arguments', tools:::RdTags(hlp), fixed=TRUE)
        if(length(id)){
            arg_section <- hlp[[id[[1L]]]]
            items <- grep('item', tools:::RdTags(arg_section), fixed=TRUE)
            ## cat('items:', items, fill=TRUE)
            if(length(items)){
                arg_section <- arg_section[items]
                args <- unlist(lapply(arg_section,
                                      function(el) paste(unlist(el[[1]][[1]], TRUE, FALSE), collapse='')))
                fits <- grep(arg, args, fixed=TRUE)
                ## cat('args', args, 'fits', fill=TRUE)
                if(length(fits))
                    paste(unlist(arg_section[[fits[1L]]][[2]], TRUE, FALSE), collapse='')
             }
        }
    }
    funcs <- c(fguess, tryCatch(methods(fguess),
                                warning=function(w) {NULL},
                                error=function(e) {NULL}))
    if(length(funcs) > 1 && length(pos <- grep('default', funcs))){
        funcs <- c(funcs[[pos[[1L]]]], funcs[-pos[[1L]]])
    }
    i <- 1L; found <- FALSE
    out <- 'No help found'
    while(i <= length(funcs) && is.null(out <-
            tryCatch(findArgHelp(funcs[[i]], arg),
                     warning=function(w) {NULL},
                     error=function(e) {NULL})
            ))
        i <- i + 1L
    cat(' \n\n', as.character(out), '\n\n')
}; getArgHelp('%s','%s')
")

(provide 'ess-r-completion)
