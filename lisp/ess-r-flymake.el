;;; ess-r-flymake.el --- A ess-r Flymake backend  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 J. Alexander Branham (alex DOT branham AT gmail DOT com)
;; Copyright (C) 2018 ESS-core team
;; Maintainer: ESS-core <ESS-core@r-project.org>
;;
;; This file is NOT part of GNU Emacs.
;;
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
;;
;;; Commentary:
;;
;; Flymake is the built-in Emacs package that supports on-the-fly
;; syntax checking.  This file adds support for this in R-mode by
;; relying on the lintr package, available on CRAN and currently
;; hosted at https://github.com/jimhester/lintr.
;;
;; It is enabled by default.
;;
;;; Code:

(require 'cl-lib)
(require 'ess-custom)
(require 'flymake)

(defcustom ess-r-flymake-linters
  '("commented_code_linter = NULL"
    "single_quotes_linter = NULL"
    "infix_spaces_linter = NULL"
    "line_length_linter = NULL"
    "object_name_linter = NULL"
    "object_usage_linter = NULL"
    "pipe_continuation_linter = NULL"
    "trailing_whitespace_linter = NULL")
  "Default linters to use.
Can be either a string with R expression to be used as
is (e.g. 'lintr::default_linters'). Or a list of strings where
each element is passed as argument to 'lintr::with_defaults'."
  :group 'ess-R
  :type '(choice string (repeat string)))

(defcustom ess-r-flymake-lintr-cache t
  "If non-nil, cache lintr results."
  :group 'ess-R
  :type 'string)

(defvar-local ess-r--flymake-proc nil)

(defvar ess-r--flymake-def-linter
  (replace-regexp-in-string
   "[\n\t ]+" " "
   "esslint <- function(str, linters, cache) {
    if (!suppressWarnings(require(lintr, quietly=T))) {
        cat('@@error: @@`lintr` package not installed')
    } else {
        if (packageVersion('lintr') <= '1.0.1') {
            cat('@@error: @@Need `lintr` version > v1.0.1')
        } else {
            tryCatch(lintr::lint(commandArgs(TRUE), linters = linters, cache = cache),
                    error = function(e) {
                       cat('@@warning: @@', e)
                    })
        }
    }
};"))

(defun ess-r--flymake-linters ()
  (replace-regexp-in-string
   "[\n\t ]+" " "
   (if (stringp ess-r-flymake-linters)
       ess-r-flymake-linters
     (concat "lintr::with_defaults("
             (mapconcat #'identity
                        ess-r-flymake-linters
                        ", ")
             ")"))))

(defun ess-r--flymake-msg-type (str)
  "Transform STR into log level."
  (cond ((string= str "error: ") :error)
        ((string= str "warning: ") :warning)
        ((string= str "style: ") :note)
        (t (error "Invalid msg type %s" str))))

(defun ess-r--flymake-check-errors ()
  "Check for critical errors and return non-nil if such occurred."
  (goto-char (point-min))
  (when (re-search-forward "@@\\(\\(error\\|warning\\): \\)@@" nil t)
    (let ((type (ess-r--flymake-msg-type (match-string 1)))
          (msg (buffer-substring-no-properties (match-end 0) (point-max))))
      (flymake-log type msg)
      (eq type :error))))

(defun ess-r--flymake-parse-output (msg-buffer src-buffer report-fn)
  "Parse the content of MSG-BUFFER for lint locations.
SRC-BUFFER is the original source buffer.  Collect all messages
into a list and call REPORT-FN on it."
  (with-current-buffer msg-buffer
    (if (ess-r--flymake-check-errors)
        (with-current-buffer src-buffer
          ;; we are in the sentinel here; don't throw but remove our hook instead
          (remove-hook 'flymake-diagnostic-functions 'ess-r-flymake t))
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
       for type = (ess-r--flymake-msg-type (match-string 3))
       collect (flymake-make-diagnostic src-buffer beg end type msg)
       into diags
       finally (funcall report-fn diags)))))

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
    (setq ess-r--flymake-proc
          (make-process
           :name "ess-r-flymake" :noquery t :connection-type 'pipe
           :buffer (generate-new-buffer " *ess-r-flymake*")
           :command (list inferior-R-program-name
                          "--no-save" "--no-restore" "--no-site-file" "--no-init-file" "--slave"
                          "-e" (concat
                                ess-r--flymake-def-linter
                                ;; commandArgs(TRUE) returns everything after
                                ;; --args as a character vector
                                "esslint(commandArgs(TRUE),"
                                "linters = " (ess-r--flymake-linters)
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
                       (ess-r--flymake-parse-output (process-buffer proc) src-buffer report-fn)
                     (flymake-log :warning "Canceling obsolete check %s" proc))
                 (kill-buffer (process-buffer proc)))))))))

(defun ess-r-setup-flymake ()
  "Setup flymake for ESS.
Activate flymake only if `ess-use-flymake' is non-nil."
  (when ess-use-flymake
    (when (< emacs-major-version 26)
      (error "ESS-flymake requires Emacs version 26 or later"))
    (when (string= "R" ess-dialect)
      (add-hook 'flymake-diagnostic-functions #'ess-r-flymake nil t)
      ;; Try not to enable flymake if flycheck is already running:
      (unless (bound-and-true-p flycheck-mode)
        (flymake-mode)))))

;; Enable flymake in Emacs 26+
(when (<= 26 emacs-major-version)
  (add-hook 'ess-mode-hook #'ess-r-setup-flymake))

(provide 'ess-r-flymake)

;;; ess-r-flymake.el ends here
