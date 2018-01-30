;;; ess-r-xref.el --- An xref backend for R. -*- lexical-binding: t -*-
;;
;; Author: Aaron Jacobs
;; Created: 21 January 2018
;; Maintainer: ESS-core <ESS-core@r-project.org>
;;
;; Keywords: languages, statistics, xref
;; Package-Requires: ((emacs "25"))
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

;;; Commentary:

;; This file contains an xref backend for `R-mode'.

;;; Code:

(require 'subr-x)
(require 'xref)
(require 'ess)

;; Silence the byte compiler.
(declare-function inferior-ess-r-force "ess-r-mode.el")


;;; Xref API

(defun ess-r-xref-backend ()
  "An `xref-backend-functions' implementation for `R-mode'."
  'ess-r)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ess-r)))
  ;; Symbols are either a string, or a list whose car is the namespace and whose
  ;; cdr is the symbol.
  (let* ((symbol (ess-symbol-at-point))
         (parts (when symbol (split-string (symbol-name symbol) "::"))))
    (if (cdr parts) parts (car parts))))

(cl-defmethod xref-backend-definitions ((_backend (eql ess-r)) symbol)
  (inferior-ess-r-force)
  ;; Ignore non-functions.
  (when (ess-r-xref--fn-exists-p symbol)
    (ess-r-xref--srcfile-xref symbol)))

(cl-defmethod xref-backend-apropos ((_backend (eql ess-r)))
  ;; Not yet supported.
  nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ess-r)))
  (inferior-ess-r-force)
  (let ((raw (ess-get-object-list ess-local-process-name)))
    ;; This is not technically correct, since some of these objects are
    ;; non-functions. But it *is* approximately correct, and expensive to
    ;; filter with existing functions. A better strategy may be desirable in
    ;; the future.
    raw))


;;; Source File Locations

(defun ess-r-xref--get-symbol (symbol)
  "Transform the R object SYMBOL into an explicit get() call."
  (if (listp symbol)
      ;; Handle namespaced symbols.
       (format "base::get(\"%s\", envir = base::asNamespace(\"%s\"), inherits = FALSE)"
               (cadr symbol) (car symbol))
    (format "base::get(\"%s\")" symbol)))

(defun ess-r-xref--fn-exists-p (symbol)
  "Check whether SYMBOL is a known R function."
  (and (if (listp symbol)
           ;; Handle namespaced symbols.
           (ess-boolean-command
            (format "base::exists(\"%s\", envir = base::asNamespace(\"%s\"), inherits = FALSE)\n"
                    (cadr symbol) (car symbol)))
         (ess-boolean-command (format "base::exists(\"%s\")\n" symbol)))
       (ess-boolean-command (format "base::is.function(%s)\n"
                                    (ess-r-xref--get-symbol symbol)))))

(defun ess-r-xref--srcfile-xref (symbol)
  "Creates an xref for the source file reference of R symbol SYMBOL, if it has one.

Most functions will not have source file references, either
because they were sent to the interpreter directly, or because
they are loaded via byte-compiled packages."
  (when (ess-boolean-command (format "!base::is.null(utils::getSrcref(%s))\n"
                                     (ess-r-xref--get-symbol symbol)))
    (let* ((fname (string-trim
                   (ess-string-command
                    (format "base::cat(utils::getSrcFilename(%s, full.names = TRUE)[1], \"\\n\")\n"
                            (ess-r-xref--get-symbol symbol)))))
           ;; Check if the file srcref is cached by ESS.
           (srcref (gethash fname ess--srcrefs))
           (ess-buff (when srcref (ess--dbg-find-buffer (car srcref))))
           ;; R seems to keep track of lines even for non-existent files, too.
           (line (string-to-number
                  (ess-string-command
                   (format "base::cat(utils::getSrcLocation(%s, which = \"line\")[1], \"\\n\")\n"
                           (ess-r-xref--get-symbol symbol))))))
      (cond
       ;; When the function was evaluated linewise into the interpreter, the
       ;; filename will be empty. We can't do anything in this case.
       ((string-equal fname "") nil)
       ;; If the file exists, jump into it.
       ((ess-boolean-command (format "base::file.exists(\"%s\")\n" fname))
        (list (xref-make (if (listp symbol)
                             (concat (car symbol) "::" (cadr symbol))
                           symbol)
                         (xref-make-file-location
                          (expand-file-name fname) line 0))))
       ;; For functions eval'd into ESS, we can use the cached file position.
       (ess-buff (list (xref-make
                        (if (listp symbol)
                            (concat (car symbol) "::" (cadr symbol))
                          symbol)
                        (xref-make-buffer-location ess-buff (caddr srcref)))))
       ;; TODO: Handle other non-existing srcfiles, such as temp install
       ;; directories.
       (t nil))))) ;; We didn't find it.


(provide 'ess-r-xref)

;;; ess-r-xref.el ends here
