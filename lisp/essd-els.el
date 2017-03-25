;;; essd-els.el --- S-PLUS 3.x at another location customization

;; Copyright (C) 1998 Richard M. Heiberger
;; Copyright (C) 1999--2005 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Richard M. Heiberger <rmh@temple.edu>
;; Created: December 1998

;; Maintainer: ESS-core <ESS-core@r-project.org>

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

;; This file defines all the S-PLUS 3.x customizations for ess-mode.

;;; Code:

(require 'ess-s-lang)
(require 'ess-utils)

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")
(defvar S+elsewhere-dialect-name "S+6"
  "Name of 'dialect' for S-PLUS at another location.")
                                        ;easily changeable in a user's .emacs

(defvar S+elsewhere-customize-alist
  (append
   '((ess-local-customize-alist         . 'S+elsewhere-customize-alist)
     (ess-dialect                       . S+elsewhere-dialect-name)
     (ess-loop-timeout                  . ess-S-loop-timeout);fixme: dialect spec.
     (ess-object-name-db-file           . "ess-spelsewhere-namedb.el" )
     (inferior-ess-program              . inferior-S-elsewhere-program-name)
     (inferior-ess-help-command         . "help(\"%s\", pager=\"cat\", window=F)\n")

     (inferior-ess-start-file           . nil) ;"~/.ess-S+3")
     (inferior-ess-start-args           . "-i")
     (ess-STERM  . "iESS")
     )
   S+common-cust-alist)
  "Variables to customize for S+elsewhere")


(defun S+elsewhere (&optional proc-name)
  "Call 'S-PLUS 3.x', the 'Real Thing'  from StatSci.
This command is obsolete; please use `ess-remote' instead."
  (interactive)
  (setq ess-customize-alist S+elsewhere-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(S+elsewhere): ess-dialect=%s, buf=%s\n" ess-dialect
           (current-buffer)))
  (inferior-ess)
  (if inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start)))


(defun S+elsewhere-mode (&optional proc-name)
  "Major mode for editing S+3 source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S+elsewhere-customize-alist)
  (ess-mode S+elsewhere-customize-alist proc-name))

(defun S+elsewhere-transcript-mode ()
  "S-PLUS 3.x transcript mode."
  (interactive)
  (ess-transcript-mode S+elsewhere-customize-alist))

;; This REALLY shouldn't need an editing mode.  Just a transcript and
;; an inferior process handler.

(defun ess-change-alist (item value alist)
  "Modify ALIST to set VALUE to ITEM.
If there is a pair whose car is ITEM, replace its cdr by VALUE.
If there is not such pair, create new pair (ITEM . VALUE) and
return new alist whose car is the new pair and cdr is ALIST.
\[tomo's ELIS like function]"
  (let ((pair (assoc item alist)))
    (if pair
        (progn
          (setcdr pair value)
          alist)
      (cons (cons item value) alist))))


(defun ess-select-alist-dialect (&optional dialect)
  "Query user for an ESS dialect and return the matching customize-alist."
  (interactive)
  (let* ((dialects '("R" "S+" "julia" "arc" "vst" "omg" "s3" "s4" "stata" "sp3" "sp4"
                     "sqpe4" "sp5" "sqpe" "XLS" "SAS"))
         (dialect (or dialect
                      (ess-completing-read "Dialect" dialects nil t))))
    (cond
     ((string= dialect "julia") ess-julia-customize-alist)
     ((string= dialect "arc")   ARC-customize-alist)
     ((string= dialect "vst")   VST-customize-alist)
     ((string= dialect "omg")   OMG-customize-alist)
     ((string= dialect "s3")    S3-customize-alist)
     ((string= dialect "s4")    S4-customize-alist)
     ((string= dialect "stata") STA-customize-alist)
     ((string= dialect "R")     ess-r-customize-alist)
     ((string= dialect "sp3")   S+3-customize-alist)
     ((string= dialect "sp4")   S+4-customize-alist)
     ((string= dialect "sqpe4") Sqpe+4-customize-alist)
     ((string= dialect "sp5")   S+5-customize-alist)
     ((string= dialect "S+")    S+-customize-alist)
     ((string= dialect "sqpe") Sqpe+-customize-alist)
     ((string= dialect "XLS")   XLS-customize-alist)
     ((string= dialect "SAS")   SAS-customize-alist);was S+elsewhere-customize-alist?
     (t                         S+elsewhere-customize-alist)
     )))


;; (defun ESS-elsewhere (&optional proc-name)
;;   "Call an inferior process from ELSEWHERE.
;; This command is obsolete; please use `ess-remote' instead."
;;   (interactive)
;;   ;; Need to select a elsewhere-customize-alist
;;   (let ((elsewhere-customize-alist (ess-select-alist-dialect)))
;;     (ess-change-alist 'inferior-ess-program
;;                       inferior-ESS-elsewhere-program-name
;;                       elsewhere-customize-alist)
;;     (setq ess-customize-alist elsewhere-customize-alist)
;;     (ess-write-to-dribble-buffer
;;      (format "\n(ESS-elsewhere): ess-dialect=%s, buf=%s\n" ess-dialect
;;              (current-buffer)))
;;     (inferior-ess)
;;     (if (equal ess-language "S")
;;         (if inferior-ess-language-start
;;             (ess-eval-linewise inferior-ess-language-start)))))


(defun ess-add-ess-process ()
  "Execute this command from within a buffer running a process to add
the process to `ess-process-name-alist' and to make it the
`ess-current-process-name'.  This command will normally be run in a
telnet buffer connected to another computer or in a shell or comint
buffer on the local computer."
  (interactive)
  (let ((proc (get-buffer-process (buffer-name))))
    (if (not proc)
        (error "No process is associated with this buffer.")
      (set-process-filter proc 'inferior-ess-output-filter)
      (setq ess-current-process-name (process-name proc))
      (add-to-list 'ess-process-name-list
                   (list ess-current-process-name)))))

(defvar ess-remote nil
  "Indicator, t in ess-remote buffers.")

(defun ess-remote (&optional proc-name dialect)
  "Execute this command from within a buffer running a process.  It
runs `ess-add-ess-process' to add the process to
`ess-process-name-alist' and to make it the
`ess-current-process-name'.  It then prompts the user for an ESS
language and sets the editing characteristics appropriately.

To use this command, first start a process on a remote computer by
manual use of telnet, rlogin, ssh, or some other protocol.  Start the
relevant program (\"S\" or \"R\" or \"sas -stdio\") in that buffer.  Once
you are talking to S or R or SAS, then execute `ess-remote' to make
the current buffer an inferior-ess buffer with the right behavior for
the language you are currently working with.  With S and R, use C-c
C-n to send lines over.  With SAS, use C-c i
`ess-eval-line-and-step-invisibly' to send lines over invisibly.

DIALECT is the desired ess-dialect. If nil, ask for dialect"

  (interactive)
  (ess-add-ess-process)
  ;; Need to select a remote-customize-alist
  (let ((ess-customize-alist (ess-select-alist-dialect dialect)))
    (ess-write-to-dribble-buffer
     (format "\n(ESS-remote): ess-dialect=%s, buf=%s\n" ess-dialect
             (current-buffer)))
    (ess-setq-vars-local ess-customize-alist)
    (inferior-ess-mode)
    (set (make-local-variable 'ess-remote) t)
    (setq ess-local-process-name (or proc-name ess-current-process-name))

    (goto-char (point-max))

    (when (equal ess-dialect "R")
      ;; ugly fix for evn variable. What can we do :(
      (ess-eval-linewise (format "options(pager='%s')\n" inferior-ess-pager)
                         nil nil nil 'wait)
      (inferior-ess-r-load-ESSR))

    (when (equal ess-dialect "S+")
      (ess-command ess-S+--injected-code))

    (when (equal ess-language "SAS")
      (font-lock-mode 0)
      (SAS-log-mode)
      (shell-mode)
      (setq buffer-read-only nil)
      (font-lock-mode 1))

    (ess-process-put 'funargs-cache (make-hash-table :test 'equal))
    (ess-process-put 'funargs-pre-cache nil)
    (ess-process-put 'accum-buffer-name (format " *%s:accum*" ess-local-process-name))
    (ess-load-extras)

    (when inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start
                         nil nil nil 'wait-prompt))))


 ; Provide package

(provide 'essd-els)

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

;;; essd-els.el ends here
