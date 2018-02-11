;;; ess-r-flymake.el --- A ess-r Flymake backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 J. Alexander Branham (alex DOT branham AT gmail DOT com)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.


;;; Commentary:
;; Flymake is the built-in Emacs package that supports on-the-fly
;; syntax checking.  This file adds support for this in R-mode by
;; relying on the lintr package, available on CRAN and currently
;; hosted at https://github.com/jimhester/lintr.

;; It is enabled by default.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'ess-custom)
(require 'flymake)

(defcustom ess-r-flymake-linters "default_linters"
  "Default linters to use.
See \"lintr::with_defaults\" for how to customize this."
  :group 'ess-R
  :type 'string)

(defcustom ess-r-flymake-lintr-cache t
  "If non-nil, cache lintr results."
  :group 'ess-R
  :type 'string)

(defvar-local ess-r--flymake-proc nil)

(defun ess-r-flymake (report-fn &rest _args)
  "A Flymake backend for ESS-R modes.  Relies on the lintr package.
REPORT-FN is flymake's callback function."
  (unless (executable-find inferior-ess-r-program-name)
    (error "Cannot find program '%s'" inferior-ess-r-program-name))
  ;; Kill the process if earlier check was found. The sentinel of the earlier
  ;; check will detect this.
  (when (process-live-p ess-r--flymake-proc)
    (kill-process ess-r--flymake-proc))
  (let ((src-buffer (current-buffer)))
    (save-restriction
      (widen)
      (setq
       ess-r--flymake-proc
       (make-process
        :name "ess-r-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer "*r-flymake*")
        :command (list inferior-R-program-name
                       "--slave" "--restore" "--no-save"
                       "-e" (concat
                             "library(lintr);"
                             ;; commandArgs(TRUE) returns everything after
                             ;; --args as a character vector
                             "lint(commandArgs(TRUE),"
                             "linters = " ess-r-flymake-linters
                             (when ess-r-flymake-lintr-cache
                               ", cache = TRUE")
                             ")")
                       "--args" (buffer-substring-no-properties
                                 (point-min) (point-max)))
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (unwind-protect
                (if (eq proc (buffer-local-value 'ess-r--flymake-proc src-buffer))
                    (ess-r-flymake--parse-output (process-buffer proc) src-buffer report-fn)
                  (flymake-log :warning "Canceling obsolete check %s" proc))
              (kill-buffer (process-buffer proc))))))))))

(defun ess-r-flymake--parse-output (msg-buffer src-buffer report-fn)
  "Parse the content of MSG-BUFFER for lint locations.
SRC-BUFFER is the original source buffer.  Collect all messages
into a list and call REPORT-FN on it."
  (with-current-buffer msg-buffer
    (goto-char (point-min))
    (cl-loop
     while (search-forward-regexp
            ;; Regex to match the output lint() gives us.
            (rx line-start "<text>:"
                ;; row
                (group-n 1 (one-or-more num)) ":"
                ;; column
                (group-n 2 (one-or-more num)) ": "
                ;; type
                (group-n 3 (| "style: " "warning: " "error: "))
                ;; msg
                (group-n 4 (one-or-more not-newline)) line-end)
            nil t)
     for msg = (match-string 4)
     for (beg . end) = (let ((line (string-to-number (match-string 1)))
                             (col (string-to-number (match-string 2))))
                         (flymake-diag-region src-buffer line col))
     for type = (let ((str (match-string 3)))
                  (cond ((string= str "error: ") :error)
                        ((string= str "warning: ") :warning)
                        ((string= str "style: ") :note)))
     collect (flymake-make-diagnostic src-buffer beg end type msg)
     into diags
     finally (funcall report-fn diags))))

(defun ess-r-setup-flymake ()
  "Setup flymake for ESS."
  (when (< emacs-major-version 26)
    (error "ESS-flymake requires Emacs version 26 or later"))
  (when (string= "R" ess-dialect)
    (add-hook 'flymake-diagnostic-functions #'ess-r-flymake nil t)
    ;; Try not to enable flymake if flycheck is already running:
    (unless (bound-and-true-p flycheck-mode)
      (flymake-mode))))

;; Enable flymake in Emacs 26+
(when (<= 26 emacs-major-version)
  (add-hook 'ess-mode-hook #'ess-r-setup-flymake))

(provide 'ess-r-flymake)

;;; ess-r-flymake.el ends here
