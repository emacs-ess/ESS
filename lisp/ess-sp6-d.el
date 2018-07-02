;;; ess-sp6-d.el --- S-Plus 6 & 7 & 8  customization

;; Copyright (C) 2001--2005 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@u.washington.edu>
;; Created: 2001/02/06
;; Maintainer: ESS Core Team <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; AJR copied S+5 to be S+6.
;; AJR copied S4 to be S+5.
;; DB contributed the changes from ess-sp3-d.el to
;; ess-s4-d.el. (removed the old ugly approach).
;; This file defines Sp5 customizations for ess-mode.  Lots of thanks
;; to RMH and JMC for code and suggestions
;; Thanks to MM for making this sensible.

;;; Code:

(require 'ess-mode)
(require 'ess-inf)
(require 'ess-s-lang)
(require 'ess-dde)

;; You now need to make sure you've defined if you are running 5.0 or 5.1.
;; Lots of things are broken between them, GRR...

(defun S+-directory-p (directory)
  "Splus 5++ directories have a .Data directory and a __Meta directory within."
  (and directory
       (file-directory-p (concat directory ".Data"))
       (file-directory-p (concat directory ".Data/__Meta"))))

(defvar S+-directory-function #'inferior-ess-default-directory)

(defvaralias 'S+6-setup-directory-function 'S+-setup-directory-function)
(defvar S+-setup-directory-function
  (lambda (startdir)
    (when (and startdir (S+-directory-p startdir))
      (setenv "S_WORK"
              (if (getenv "S_WORK")
                  (concat startdir ":" (getenv "S_WORK"))
                ;;(message "adding %s to S_WORK" startdir)
                startdir)))))



(defvaralias 'S+6-customize-alist 'S+-customize-alist)
(defvar S+-customize-alist
  (append
   '((ess-local-customize-alist        . 'S+-customize-alist)
     (ess-dialect                      . S+-dialect-name)
     (ess-loop-timeout                 . ess-S-loop-timeout) ;fixme: dialect spec.
     (ess-function-pattern             . ess-r-function-pattern)

     (ess-object-name-db-file          . "ess-sp6-namedb.el")
     (inferior-ess-program             . inferior-S+-program)
     (inferior-ess-help-command        . "help(\"%s\", pager=\"slynx -dump\", window=FALSE)\n")
     (inferior-ess-help-filetype       . nil)
     (inferior-ess-search-list-command . "searchPaths()\n")

     (ess-send-region-function         . #'ess-dde-send-region)
     (ess-load-file-function           . #'ess-dde-load-file)
     (ess-command-function             . #'ess-dde-command)
     (ess-eval-linewise-function       . #'ess-dde-eval-linewise)
     (ess-dump-object-function         . #'ess-dde-dump-object)
     (ess-read-object-name-function    . #'ess-dde-read-object-name)
     (ess-find-help-file-function      . #'ess-dde-find-help-file)
     (ess-display-help-on-object-function . #'ess-dde-display-help-on-object)

     (ess-directory-function           . S+-directory-function)
     (ess-setup-directory-function     . S+-setup-directory-function)
     (inferior-ess-start-args          . inferior-S+-start-args)
     (ess-STERM                        . "iESS")
     )
   S+common-cust-alist)

  "Variables to customize for S+.")

(defvar ess-S+-post-run-hook nil
  "Functions run in process buffer after the initialization of S+
  process.")

(defalias 'S+6 'S+)
(defun S+ (&optional proc-name)
  "Call 'Splus6', based on S version 4, from Bell Labs.
New way to do it."
  (interactive)
  (setq ess-customize-alist S+-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(S+): ess-dialect=%s, buf=%s\n" ess-dialect (current-buffer)))
  (inferior-ess)
  (ess-command ess-S+--injected-code)
  (if inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start))
  (with-ess-process-buffer nil
    (run-mode-hooks 'ess-S+-post-run-hook)))

(defvar ess-S+--injected-code
  ".ess_funargs <- function(funname){
  ## funname <- deparse(substitute(object))
  fun <- try(eval(parse(text=funname)), silent = TRUE)
  if(is.function(fun)) {
    special <- grepl('[:$@[]', funname)
    args <- args(fun)
    fmls <- formals(args)
    fmls.names <- names(fmls)
    fmls <- gsub('\\\"', '\\\\\\\"', as.character(fmls), fixed = TRUE)
    args.alist <- sprintf(\"'(%s)\", paste(\"(\\\"\", fmls.names, \"\\\" . \\\"\", fmls, \"\\\")\", sep = '', collapse = ' '))
    ## envname <- environmentName(environment(fun))
    envname <-  if (special) '' else 'S+'
    cat(sprintf('(list \\\"%s\\\" %s )\\n', envname, args.alist))
  }
}
")


(defalias 'S+6-mode 'S+-mode)
;;;###autoload
(defun S+-mode (&optional proc-name)
  "Major mode for editing S+ source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S+-customize-alist)
  (ess-mode S+-customize-alist proc-name)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  (if ess-imenu-use-S (ess-imenu-S)))

(defalias 'S+6-transcript-mode 'S+-transcript-mode)
(defun S+-transcript-mode ()
  "S-PLUS 6 transcript mode."
  (interactive)
  (ess-transcript-mode S+-customize-alist))

(defvar ess-s-versions '("Splus")
  "List of partial strings for versions of S to access within ESS.
Each string specifies the start of a filename.  If a filename
beginning with one of these strings is found on `exec-path', a M-x
command for that version of S is made available.  For example, if the
file \"Splus7\" is found and this variable includes the string
\"Splus\", a function called `M-x Splus7' will be available to run that
version of S.
If duplicate versions of the same program are found (which happens if
the same path is listed on `exec-path' more than once), they are
ignored by calling `ess-uniq-list'.
Set this variable to nil to disable searching for other versions
of S using this method.
If you set this variable, you need to restart Emacs (and set this variable
before ess-site is loaded) for it to take effect.")

(defvar ess-s-created-runners)
(define-obsolete-variable-alias
  'ess-s-versions-created 'ess-s-created-runners "2018-05-12")
(defun ess-s-define-runners ()
  "Generate functions for starting other versions of S.
See `ess-s-versions' for strings that determine which functions are created.
It assumes these versions of S can be run as a substitute for Splus6.

This function returns the list of functions, if any, that were
created.  The functions will normally be placed on the menubar upon
ESS initialization."
  (when ess-s-versions
    (let ((versions
           (ess-uniq-list
            (mapcar #'file-name-nondirectory
                    (apply #'nconc
                           (mapcar #'ess-find-exec-completions
                                   ess-s-versions))))))
      ;; Iterate over each string in VERSIONS, creating a new defun
      ;; each time.
      (setq ess-s-created-runners
            (mapc (lambda (v) (ess-define-runner v "S")) versions))
      ;; Add to menu
      (when ess-s-created-runners
        ;; new-menu will be a list of 3-vectors, of the form:
        ;; ["R-1.8.1" R-1.8.1 t]
        (let ((new-menu (mapcar (lambda (x) (vector x (intern x) t))
                                ess-s-created-runners)))
          (easy-menu-add-item ess-mode-menu '("Start Process")
                              (cons "Other" new-menu)))))))
;; Define the runners
(if ess-microsoft-p
    (nconc
     (ess-sqpe-versions-create ess-SHOME-versions) ;; 32-bit
     (ess-sqpe-versions-create ess-SHOME-versions-64 "-64-bit")) ;; 64-bit
  (ess-s-define-runners))
(define-obsolete-function-alias
  'ess-s-versions-create 'ess-s-define-runners "2018-05-12")



 ; Provide package

(provide 'ess-sp6-d)

;;; ess-sp6-d.el ends here
