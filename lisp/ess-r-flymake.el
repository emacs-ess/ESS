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
;; syntax checking. This file adds support for this in R-mode by
;; relying on the lintr package, available on CRAN and currently
;; hosted at https://github.com/jimhester/lintr.

;; It is enabled by default.

;;; Code:

(defcustom ess-r-flymake-linters "default_linters"
  "Default linters to use.

See \"lintr::with_defaults\" for how to customize this."
  :group 'ess-R
  :type 'string)

(defvar-local ess-r--flymake-proc nil)

(defun ess-r-flymake (report-fn &rest _args)
  "A Flymake backend for ESS-R modes. Relies on the lintr package.

REPORT-FN is flymake's callback function."
  (unless (executable-find
           ;; TODO - check whether lintr package is available
           "R") (error "Cannot find a suitable R"))
  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `ess-r-flymake-proc' to a different value
  (when (process-live-p ess-r--flymake-proc)
    (kill-process ess-r--flymake-proc))
  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       ess-r--flymake-proc
       (make-process
        :name "ess-r-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *ess-r-flymake*")
        :command `("R" "--slave" "--restore" "--no-save" "-e"
                   ,(eval (concat
                           "library(lintr);"
                           ;; commandArgs(TRUE) lets us access
                           ;; everything after --args as a string:
                           "try(lint(commandArgs(TRUE)"
                           ", " "linters = " ess-r-flymake-linters
                           "))"))
                   "--args" ,(eval
                              ;; text string of the current buffer:
                              (concat (buffer-substring-no-properties
                                       (point-min) (point-max)))))
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (unwind-protect
                ;; Only proceed if `proc' is the same as
                ;; `ess-r--flymake-proc', which indicates that
                ;; `proc' is not an obsolete process.
                (if (with-current-buffer source (eq proc ess-r--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      ;; Parse the output buffer for diagnostic's
                      ;; messages and locations, collect them in a list
                      ;; of objects, and call `report-fn'.
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
                                  (group-n 4 (one-or-more not-newline)) "\n")
                              nil t)
                       for msg = (match-string 4)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 1))
                                          (string-to-number (match-string 2)))
                       for type = (let ((str (match-string 3)))
                                    (cond ((string-equal str "error: ") :error)
                                          ((string-equal str "warning: ") :warning)
                                          ((string-equal str "style: ") :note)))
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        type
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the
              ;; check's output.
              (kill-buffer (process-buffer proc))))))))))

(defun ess-r-setup-flymake ()
  "Setup flymake for ESS."
  (add-hook 'flymake-diagnostic-functions #'ess-r-flymake nil t)
  ;; Try not to enable flymake if flycheck is already running:
  (unless (bound-and-true-p flycheck-mode)
    (flymake-mode)))

;; Enable flymake in Emacs 26+
(when (<= 26 emacs-major-version)
  (add-hook 'ess-mode-hook #'ess-r-setup-flymake))

(provide 'ess-r-flymake)

;;; ess-r-flymake.el ends here
