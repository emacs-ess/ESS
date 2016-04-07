;;; ess-sta-d.el --- Stata customization

;; Copyright (C) 1997--1999 A. J. Rossini, Thomas Lumley
;; Copyright (C) 1997--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 9 Sep 1998
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

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

;; This file defines all the Stata customizations for ess-mode. It is somewhat
;; based on Stata-mode by Thomas Lumley <thomas@biostat.washington.edu>.

;;; Code:

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

(require 'ess-utils)
(require 'ess-sta-l)

(defvar STA-dialect-name "stata"
  "Name of 'dialect' for Stata.");easily changeable in a user's .emacs

(defvar STA-customize-alist
  '((ess-local-customize-alist     . 'STA-customize-alist)
    (ess-language                  . "STA")
    (ess-dialect                   . STA-dialect-name)
    (ess-suffix                    . "ado")
    (ess-mode-editing-alist        . STA-editing-alist)
    (ess-mode-syntax-table         . STA-syntax-table)
    (ess-mode-edit                 . 'STA-mode)
    (ess-help-sec-regex            . ess-help-STA-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-STA-sec-keys-alist)
    (ess-loop-timeout              . 500000 )
    (ess-object-name-db-file       . "ess-sta-namedb.el" )
    (ess-help-web-search-command   . "http://www.stata.com/search/?q=%s&restrict=&btnG=Search&client=stata&num=&output=xml_no_dtd&site=stata&ie=&oe=UTF-8&sort=&proxystylesheet=stata")
    (ess-eval-linewise-function    . #'stata-eval-linewise)
    (inferior-ess-font-lock-defaults . ess-STA-mode-font-lock-defaults)
    (inferior-ess-program          . inferior-STA-program-name)
    (inferior-ess-objects-command  . "describe\n")
    (inferior-ess-help-command     . "help %s\n") ;; assumes set more off 
    (inferior-ess-exit-command     . "exit\n")
    ;; --more-- is necessary here (hangs otherwise if startup stata.msg is big)
    (inferior-ess-primary-prompt   . "[.:] \\|--more--") 
    (inferior-ess-secondary-prompt . "--more--")
    (comint-use-prompt-regexp      . t)
    (inferior-ess-start-file       . inferior-STA-start-file) ;"~/.ess-stata")
    (inferior-ess-start-args       . inferior-STA-start-args)
    (ess-get-help-topics-function  . 'ess-get-STA-help-topics)
    (inferior-ess-search-list-command   . "set more off\n search()\n")
    (comment-start                . "/\* ")
    (comment-end                  . " \*/")
    (comment-start-skip           . "/\\*+ *")
    (comment-use-syntax           . t) ;; needed for multiline
    (ess-execute-screen-options-command . "set linesize %s\n")
    )
  "Variables to customize for Stata.")


(defun STA-mode (&optional proc-name)
  "Major mode for editing Stata source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist STA-customize-alist)
  (ess-mode STA-customize-alist proc-name))

(fset 'stata-mode 'STA-mode)
(fset 'Stata-mode 'STA-mode)


(defun ess-sta-remove-comments (string)
  "Remove one-line comments before sending the STRING to process.

This function is placed in `ess-presend-filter-functions'.
"
  (replace-regexp-in-string "/\\*.*\\*/\\|^//.*$" "" string))

;; (ess-sta-remove-comments "aaa /* sdfdsf */ bbb
;; sdfsd
;;  ccc
;; // sdfsf
;; sdf /* sdfdsf */
;; sdfsf
;; " )


(defvar ess-stata-post-run-hook nil
  "Functions run in process buffer after the initialization of
  stata process.")

(defun stata (&optional start-args)
  "Call Stata."
  (interactive "P")
  (setq ess-customize-alist STA-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(STA): ess-dialect=%s , buf=%s \n"
           ess-dialect
           (current-buffer)))
  (let ((sta-start-args 
         (concat inferior-STA-start-args
                 (when start-args (read-string "Starting Args [possibly -k####] ? ")))))
    (inferior-ess sta-start-args)
    (let ((proc (get-process ess-local-process-name)))
      (while (process-get proc 'sec-prompt)
        ;; get read of all --more-- if stata.msg is too long.
        (ess-send-string proc "q")
        (ess-wait-for-process proc t))
      (ess-send-string proc "set more off")
      (goto-char (point-max))
      (with-current-buffer (process-buffer proc)
        (add-hook 'ess-presend-filter-functions 'ess-sta-remove-comments nil 'local)
        (run-mode-hooks 'ess-stata-post-run-hook)))))


(defun STA-transcript-mode ()
  "Stata transcript mode."
  (interactive)
  (ess-transcript-mode STA-customize-alist))

(defun ess--STA-retrive-topics-from-search ()
  (with-current-buffer (ess-command inferior-ess-search-list-command)
    (goto-char (point-min))
    (let (topics)
      (while (re-search-forward "(help \\(.+?\\)\\( if installed\\| for replacement.*\\)?)$" nil t)
        (setq topics
              (nconc (split-string (match-string-no-properties 1) ",\\|; +")
                     topics)))
      (nreverse (delete-dups topics))
      )))

(defun ess-get-STA-help-topics (&optional name)
  "Return a list of current STA help topics associated with process NAME."
  (or (ess-process-get 'help-topics)
      (progn
        (ess-process-put 'help-topics (ess--STA-retrive-topics-from-search))
        (ess-process-get 'help-topics))))

(defun stata-eval-linewise (text &optional invisibly &rest args)
  ;; The following is required to make sure things work!
  (let ((ess-eval-linewise-function nil)
        ;; RAS: mindless replacement of semi-colons
        (text (if ess-sta-delimiter-friendly
                  (ess-replace-in-string text ";" "\n")
                text)))
    (apply #'ess-eval-linewise text t args)))

 ; Provide package

(provide 'ess-sta-d)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:     ^L ;
;;; Sections:    ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:  defuns, defvars, defconsts
;;;              Random code beginning with a ;;;;* comment

;;; Local variables:
;;; mode: emacs-lisp
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-sta-d.el ends here
