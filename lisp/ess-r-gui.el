;;; ess-r-gui.el --- Run Rgui on Windows as an inferior Emacs process

;; Copyright (C) 2008 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author:  Richard M. Heiberger <rmh@temple.edu>
;; Created: 10 Mar 2008
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

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

                                        ;In Rgui:
                                        ;> library(tcltk2)  ## >= 1.0-6
                                        ;> .ess.command <- function() source("c:/temp/ess-tempfile.R", echo=TRUE)
                                        ;> tclFun(.ess.command)
                                        ;[1] "R_call 0203A04C"

;;; Code:

(require 'ess-dde) ;; needed here because we override several definitions

(defun ess-ddeclient-p ()
  "Returns the name of the ddeclient iff `ess-local-process-name'
is associated with an `inferior-ess-ddeclient', and nil if the
ess-process is running as an ordinary inferior process.  Alway
nil on Unix machines."
  (interactive)
  (if ess-microsoft-p
      (let ((ess-ddeclient (ess-get-process-variable 'inferior-ess-ddeclient)))
        (if (not (equal ess-ddeclient (default-value 'inferior-ess-ddeclient)))
            ess-ddeclient))))

(defun ess-eval-region-execdde (start end even-empty)
  "Loop through lines in region and send them to ESS via execdde."
  (setq ;; set the following variables for the current ddeESS process.
   inferior-ess-ddeclient (ess-get-process-variable 'inferior-ess-ddeclient)
   )
  (write-region start end ess-command-file nil nil 'no-message)
  (call-process-shell-command
   (concat inferior-ess-execdde ess-rgui-command))
  )


(if (not (getenv "R_HOME")) (setenv "R_HOME" "c:/progra~1/R/R-2.6.1"))
;;                                                         ^^^^^^^^^ FIXME! do something better
(defvar inferior-Rgui-program-name "cmd" "Rgui program name")
(defvar Rgui-pager "emacsclientw.exe" "Rgui pager program")
(defvar ess-command-file "c:/temp/ess-tempfile.R"
  "file name for communication with Rgui")
(defvar inferior-ess-execdde
  (concat (getenv "R_HOME") "/site-library/tcltk2/bin/execdde.exe")
  "Full pathname to execdde executable")
(defvar ess-rgui-command " -s TclEval -t R -c .ess.command > NUL"
  "command to inferior-ess-execdde that will make Rgui read the command file")
(defvar inferior-ess-language-start-rgui
  "options(chmhelp=FALSE, htmlhelp=FALSE, help_type='text'); require(tcltk2)"
  "additional arguments to rgui")

(defun ess-dde-rgui-send-region (proc start end &optional visibly message)
  "Loop through lines in region and send them to ESS via ddeclient.

PROC, VISIBLY and MESSAGE are ignored."
  (setq ;; set the following variables for the current ddeESS process.
   inferior-ess-ddeclient (ess-get-process-variable 'inferior-ess-ddeclient)
   inferior-ess-client-name (ess-get-process-variable 'inferior-ess-client-name)
   inferior-ess-client-command (ess-get-process-variable 'inferior-ess-client-command))
  (narrow-to-region start end)
  (goto-char (point-min))

  (if (equal inferior-ess-ddeclient "execdde")
      (ess-eval-region-execdde start end 'even-empty)

    (let ((beg))
      (while (or (< (point) (point-max))
                 (= 1 (point-max)))
        (setq beg (point))
        (end-of-line)
        ;; call-process-region won't send over a 0-character line.
        ;; We go outside the loop to create a 1-character line " " in the
        ;; *ESS-temporary* buffer
        (if (= beg (point)) ;; do empty line outside loop
            (ess-eval-linewise-ddeclient " " nil 'eob t)
          (call-process-region
           beg (point)
           inferior-ess-ddeclient nil nil nil
           inferior-ess-client-name inferior-ess-client-command))
        (forward-line 1))))
  (widen))



(defvar Rgui-customize-alist
  (append
   '((ess-local-customize-alist        . 'Rgui-customize-alist)
     (ess-dialect                      . "R")
     (ess-suffix                       . "R")
     (ess-dump-filename-template       . (ess-replace-regexp-in-string
                                          "S$" ess-suffix ; in the one from custom:
                                          ess-dump-filename-template-proto))
     (ess-mode-syntax-table            . ess-r-syntax-table)
     (ess-mode-editing-alist           . R-editing-alist)
     (ess-change-sp-regexp             . ess-R-change-sp-regexp)
     (ess-help-sec-regex               . ess-help-R-sec-regex)
     (ess-help-sec-keys-alist          . ess-help-R-sec-keys-alist)
     (ess-loop-timeout                 . ess-S-loop-timeout);fixme: dialect spec.
     (ess-cmd-delay                    . ess-R-cmd-delay)
     (ess-function-pattern             . ess-R-function-pattern)
     (ess-object-name-db-file          . "ess-r-namedb.el" )
     (ess-send-region-function         . #'ess-dde-rgui-send-region)
     (ess-load-file-function           . #'ess-dde-load-file)
     (ess-command-function             . #'ess-dde-command)
     (ess-eval-linewise-function       . #'ess-dde-eval-linewise)
     (ess-dump-object-function         . #'ess-dde-dump-object)
     (ess-read-object-name-function    . #'ess-dde-read-object-name)
     (ess-find-help-file-function      . #'ess-dde-find-help-file)
     (ess-display-help-on-object-function . #'ess-dde-display-help-on-object)
     (inferior-ess-program             . inferior-Rgui-program-name)
     (inferior-ess-objects-command     . inferior-R-objects-command)
     (inferior-ess-font-lock-keywords  . 'inferior-R-font-lock-keywords)
     (inferior-ess-search-list-command . "search()\n")
     (inferior-ess-help-command        . "help(\"%s\")\n")
     (inferior-ess-help-filetype       . nil) ;; "chm") ;;?
     (inferior-ess-exit-command        . "q()")
     (inferior-ess-exit-prompt         . "Save workspace image? [y/n/c]: ")
     (inferior-ess-primary-prompt      . "\\([A-Z/][][A-Za-z0-9./]*\\)*[>$] ")
     (inferior-ess-secondary-prompt    . "+ ?")
     ;;harmful for shell-mode's C-a: -- but "necessary" for ESS-help?
     (inferior-ess-start-file          . nil) ;; "~/.ess-R"
     (inferior-ess-start-args          . "")
     (inferior-ess-ddeclient           . "execdde")
     (ess-STERM                        . "ddeSS")
     (ess-editor                       . R-editor)
     (ess-pager                        . Rgui-pager)
     )
   S-common-cust-alist)
  "Variables to customize for Rgui")


(defun Rgui (&optional proc-name)
  "Call 'Rgui for Windows'.  Put R in an independent MS-Window (R
persists even if the '(ddeESS [R])' window is killed in emacs).
Do this by creating a comint process that calls cmd.  This is a
placeholder buffer with mode '(ddeESS [R])'.  Commands sent from
an (ESS[S] [R]) buffer to this process will be sourced into the
independent Rgui R Console."
  (interactive)
  (save-excursion
    (setq ess-customize-alist Rgui-customize-alist)
    (ess-write-to-dribble-buffer
     (format "\n(Rgui): ess-dialect=%s, buf=%s\n" ess-dialect
             (current-buffer)))
    (setq ess-customize-alist           ; change inferior-ess-primary-prompt
          (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (let ((default-ddeclient (default-value 'inferior-ess-ddeclient)))
      (cd (w32-short-file-name (directory-file-name default-directory)))
      ;; (setenv "S_PROJ" default-directory)
      (setq-default inferior-ess-ddeclient "execdde")
      (inferior-ess)
      (setq-default inferior-ess-ddeclient default-ddeclient)
      (sleep-for 2) ; need to wait, else working too fast!
      )
    (setq comint-process-echoes nil)

    ;; *R* buffer
    (goto-char (point-min))
    (insert
     "This is a placeholder buffer.  You can't type anything here.\n
You may ignore the 'options' error in this buffer.\n\n")
    (goto-char (point-max))
    (set-buffer-process-coding-system 'raw-text-dos 'raw-text-unix)
    (setq buffer-read-only t) ; force buffer to be read-only
    (setq mode-name "ddeESS")

    ;; initialization
    (set-buffer (find-file-noselect ess-command-file 'nowarn))
    (erase-buffer)
    (setq ;; set the following variables for the current ddeESS process.
     inferior-ess-language-start (ess-get-process-variable 'inferior-ess-language-start))
    (if inferior-ess-language-start
        (insert inferior-ess-language-start))
    (if inferior-ess-language-start-rgui
        (insert (concat "\n" inferior-ess-language-start-rgui)))
    (save-buffer 0)
    (call-process-shell-command
     (concat inferior-ess-execdde ess-rgui-command))
    ))

;;; ess-r-gui.el ends here
