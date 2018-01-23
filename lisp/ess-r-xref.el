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


;;; Xref API

(defun ess-r-xref-backend ()
  "An `xref-backend-functions' implementation for `R-mode'."
  'ess-r)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ess-r)))
  (ess-symbol-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql ess-r)) symbol)
  (ess-r-xref--check-for-process
   (when (ess-r-xref--exists symbol)
     (if (ess-r-xref--has-srcfile symbol)
         (ess-r-xref--find-srcfile symbol)
       ;; Ignore non-functions for now.
       (when (ess-r-xref--is-function symbol)
         (ess-r-xref--find-body symbol))))))

(cl-defmethod xref-backend-apropos ((_backend (eql ess-r)))
  ;; Not yet supported.
  nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ess-r)))
  (ess-r-xref--check-for-process
   (let ((raw (ess-get-object-list ess-local-process-name)))
     ;; This is not technically correct, since some of these objects are
     ;; non-functions. But it *is* approximately correct, and expensive to
     ;; filter with existing functions. A better strategy may be desirable in
     ;; the future.
     raw)))

(defmacro ess-r-xref--check-for-process (&rest body)
  "Wraps statements in BODY with `ess-make-buffer-current'."
  `(if (not (ess-make-buffer-current))
       (prog1 nil
         ;; Give a message on the second invocation, as with
         ;; `ess-complete-object-name'.
         (when (string-match "xref" (symbol-name last-command))
           (message "No ESS process associated with current buffer. Definitions \
are not available.")))
     ,@body))


;;; Source File Locations

(defun ess-r-xref--exists (symbol)
  "Check whether SYMBOL is a known R symbol."
  (ess-boolean-command (format "exists(\"%s\")\n" symbol)))

(defun ess-r-xref--has-srcfile (symbol)
  "Checks that the R symbol SYMBOL has a valid source file reference.

Most functions will not have source file references, either
because they were sent to the interpreter directly, or because
they are loaded via byte-compiled packages."
  (ess-boolean-command
   (format
    "!is.null(utils::getSrcref(%s)) && file.exists(utils::getSrcFilename(%s))\n"
    symbol symbol)))

(defun ess-r-xref--find-srcfile (symbol)
  "Creates an xref for the source file reference of R symbol SYMBOL."
  (let ((file (ess-string-command
               (format "cat(utils::getSrcFilename(%s)[1], \"\\n\")\n" symbol)))
        (line (ess-string-command
               (format "cat(utils::getSrcLocation(%s, which = \"line\")[1], \"\\n\")\n" symbol))))
    (list (xref-make (format "%s" symbol)
                     (xref-make-file-location
                      (expand-file-name (string-trim file))
                      (string-to-number line)
                      0)))))


;;; Local/Byte-compiled Locations

(defun ess-r-xref--is-function (symbol)
  "Check whether SYMBOL is an R symbol."
  (ess-boolean-command (format "is.function(%s)\n" symbol)))

(defun ess-r-xref--find-body (fn)
  "Creates an xref to a buffer containing the body of the R function FN."
  (let ((cmd (format "cat(\"%s <- \"); print.function(%s)\n"
                     fn fn))
        (buff (get-buffer-create
               (format "*definition[R]:%s*" fn)))
        (proc ess-local-process-name))
    (with-current-buffer (ess-command cmd buff)
      (ess-r-source-mode)
      (setq-local ess-local-process-name proc))
    (list (xref-make (format "%s" fn) (xref-make-buffer-location buff 0)))))


;;; R Function Source Viewer Mode

(defun ess-r-xref--parse-byte-compiled-p (buff)
  "Determine whether the R function printed in BUFF is byte-compiled.

This also erases the metadata once the compilation status has
been determined."
  (with-current-buffer buff
    (goto-char (point-max))
    (when (re-search-backward "^<bytecode" nil 'noerror)
      ;; Delete the metadata, if it's there.
      (beginning-of-line)
      (kill-whole-line)
      t)))

(defun ess-r-xref--parse-environment (buff)
  "Determine the environment of the R function printed in BUFF.

This also erases the metadata once the environment has been
determined."
  (with-current-buffer buff
    (goto-char (point-max))
    (let* ((found (re-search-backward "^<environment: \\(.*\\)>$" nil 'noerror))
           (env (or (when found (match-string-no-properties 1)) ".GlobalEnv")))
      ;; Delete the metadata, if it's there.
      (when found (beginning-of-line) (kill-whole-line))
      env)))

;; Defined in ess-r-mode.
(defvar ess-r-syntax-table)

(defvar-local ess-r-source-byte-compiled-p nil
  "Indicates whether the R function on display is byte-compiled.")

(defvar-local ess-r-source-environment nil
  "The environment the R function on display belongs to.")

(define-derived-mode ess-r-source-mode special-mode "R Source"
  "Major mode for navigating the source of local/byte-compiled R functions."
  :syntax-table nil
  (setq buffer-read-only nil)
  ;; Use R's syntax table.
  (set-syntax-table ess-r-syntax-table)
  ;; Move metadata to the header line.
  (let ((compiled (ess-r-xref--parse-byte-compiled-p (current-buffer)))
        (env (ess-r-xref--parse-environment (current-buffer))))
    (setq-local ess-r-source-byte-compiled-p compiled)
    (setq-local ess-r-source-environment env)
    (setq header-line-format
          (concat "Environment: " env (when compiled " (byte-compiled)"))))
  (add-hook 'xref-backend-functions #'ess-r-xref-backend nil 'local)
  (setq buffer-read-only t))

(provide 'ess-r-xref)

;;; ess-r-xref.el ends here
