;;; essd-s+5.el --- S-plus 5  customization

;; Copyright (C) 1998 A.J. Rossini

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 9 Nov 1998
;; Modified: $Date: 1998/11/11 10:07:51 $
;; Version: $Revision: 1.3 $
;; RCS: $Id: essd-s+5.el,v 1.3 1998/11/11 10:07:51 maechler Exp $
;;
;; Keywords: start up, configuration.

;; This file is part of ESS.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; AJR copied S4 to be S+5.
;;; DB contributed the changes from essd-s+3.el to
;;; essd-s4.el. (removed the old ugly approach).
;;; This file defines S+5 customizations for ess-mode.  Lots of thanks
;;; to RMH and JMC for code and suggestions

;;; Requires and Autoloads:

(require 'essl-s)

(autoload 'inferior-ess "ess-inf" "Run an ESS process")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process")

;;; Code:

(defvar S+5-dialect-name "S+5"
  "Name of 'dialect' for S-PLUS 5.");easily changeable in a user's .emacs

(defvar S+5-customize-alist
  '((ess-local-customize-alist     . 'S+5-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . S+5-dialect-name)
    (ess-suffix                    . "S")
    (ess-dump-filename-template    . (concat (user-login-name)
					     ".%s."
					     ess-suffix))
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-syntax-table         . S-syntax-table)
    (ess-help-sec-regex            . "^[A-Z. ---]+:$")
    (ess-help-sec-keys-alist       . S4-help-sec-keys-alist)
    (ess-function-template         . " <- \n#\nfunction()\n{\n\n}\n")
    (ess-loop-timeout              . 100000 )
    (ess-object-name-db-file       . "ess-s+5-namedb.el")
    (ess-dumped-missing-re  
     . "\\(\\(<-\\|=\\)\nDumped\n\\'\\)\\|\\(\\(<-\\|=\\)\\(\\s \\|\n\\)*\\'\\)")
    (ess-syntax-error-re
     . "\\(Syntax error: .*\\) at line \\([0-9]*\\), file \\(.*\\)$")
    (ess-retr-lastvalue-command
     . ".Last.value <- get(\".ess.lvsave\",frame=0)\n")
    (ess-save-lastvalue-command
     . "assign(\".ess.lvsave\",.Last.value,frame=0)\n")
    (inferior-ess-program          . inferior-S+5-program-name)
    (inferior-ess-objects-command  . ".SmodeObs(%d, pattern=\"%s\")\n")
					; ^ was "objects(%d)")
    (inferior-ess-objects-pattern  . ".*") ; for new s4 stuff
    (inferior-ess-help-command     . "help(\"%s\")\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-primary-prompt   . "[a-zA-Z0-9() ]*> ?")
    (inferior-ess-secondary-prompt . "+ ?")
    (inferior-ess-load-command     . ".SmodeLoad(\"%s\")\n")
    (inferior-ess-dump-command     . ".SmodeDump(\"%s\", \"%s\")\n")
    (inferior-ess-search-list-command . ".SmodePaths()\n"))
  "Variables to customize for S")

;; For loading up the S code required for the above.
;;(add-hook 'ess-post-run-hook
;;	  '(lambda ()
;;	     (ess-command
;;	      (concat
;;	       "if(exists(\"Sversion\")) library(emacs) else source(\""
;;	       ess-mode-run-file
;;	       "\")\n"))
;;	     (if ess-mode-run-file2
;;		 (ess-command
;;		  (concat "source(\"" ess-mode-run-file2 "\")\n")))))


(defun S+5 ()
  "Call 'Splus5', based on S version 4, from Bell Labs
New way to do it."
  (interactive)
  (setq ess-customize-alist S+5-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(S): ess-dialect=%s , buf=%s \n"
	   ess-dialect
	   (current-buffer)))
  (inferior-ess))


(defun S+5-mode (&optional proc-name)
  "Major mode for editing S+5 source.  See ess-mode for more help."
  (interactive)
  (setq ess-customize-alist S+5-customize-alist)
  (ess-mode S+5-customize-alist proc-name))

(defun S+5-transcript-mode ()
  "S-PLUS 5 transcript mode."
  (interactive)
  (ess-transcript-mode S+5-customize-alist))

;; From RMH:    ALL THIS SHOULD BE INCORPORATED BY 5.0!

;;; s4.el startup file
;;; Richard M. Heiberger
;;; rmh@astro.ocis.temple.edu
;;
;;(load "S")
;;(setq inferior-S-program "/disk05/s4/betaJun96/S")
;;(setq S-plus nil)                            ;; needed for non S-plus
;;(add-to-list 'load-path "/disk05/s4/betaJun96") ;; S-namedb.el is here
;;(S)
;;(load-file "/disk05/s4/betaJun96/library/emacs/S-modeadds.el") ;; must come after (S)
;;
;;
;;;;; S4 __Help, no longer S3 .Help
;;(load "S-help")
;;                                             ;; Must follow S-help
;;; S-help.file line 270
;;(defun S-get-help-files-list nil
;;  (mapcar 'list
;;	  (apply 'append
;;		 (mapcar '(lambda (dirname)
;;			    (if (file-directory-p dirname) 
;;				(directory-files dirname)))
;;			 (mapcar '(lambda (str) (concat str "/__Help"))
;;				 (S-search-list))))))
;;
;;
;;;;; additional font-lock-keywords for S4
;;
;;;;*;; based on S-inf.el line 107
;;;;(add-to-list 'S-inf-font-lock-keywords
;;;;	     '("\\<\\(^Problem\\|^Warning\\|^Error\\|Debug ?\\|Browsing in frame of\\|Local Variables\\)\\>" . font-lock-reference-face) ; S-inf problems
;;;;)
;;;;(add-to-list 'S-inf-font-lock-keywords
;;;; '("^R>" . font-lock-keyword-face)	; debug prompt
;;;;)
;;(inferior-S-mode)
;;
;;; S-inf.el line 150
;;(setq inferior-S-search-list-command "searchPaths()\n")
;;
;;;; fontify S-transcript-mode
;;;; overwrites S-trans.el lines 60-69
;;;;(setq S-trans-font-lock-keywords S-inf-font-lock-keywords)
;;
;;(load "S-mode")
;;                                             ;; Must follow S-mode
;;;;*;; based on S-mode.el line 219
;;(add-to-list 'S-mode-font-lock-keywords
;;	     '("\\<\\(setGeneric\\|removeGeneric\\|setMethod\\|unsetMethod\\|setReplaceGeneric\\|setReplaceMethod\\|standardGeneric\\|setIs\\|setClass\\|representation\\)\\>" . font-lock-function-name-face)  ; S4 method functions
;;)
;;
;;
;;
;;;;; fix to S-load-file to make C-c C-l work with S4
;;
;;;When a file sourced into S4 by C-c C-l has a syntax error
;;;without the following changes, the system
;;;freezes until it is released with ^G.  The reason is that the error
;;;messages, including the `Debug ?' request, go to the *S-errors*
;;;buffer.  The *S-errors* buffer is not switched to, and couldn't accept
;;;a response if it were.
;;;
;;;The fix requires three modification to S-inf.el and two to S-mode.el.
;;;The correction to S-check-source noted in smode.cmt is also necessary.
;;;
;;
;;; S-inf.el line 92  NEW variable
;;(defvar inferior-S-debug-prompt "Debug \\? (y|n): "
;; "The expression S uses to offer to initiate debug tracing.")
;;
;;; S-inf.el line 458
;;(defun inferior-S-wait-for-prompt ()
;;  "Wait until the S process is ready for input."
;;  (let* ((cbuffer (current-buffer))
;;         (sprocess (get-S-process S-current-process-name))
;;         (sbuffer (process-buffer sprocess))
;;         r
;;	 (timeout 0))
;;    (set-buffer sbuffer)
;;    (while (progn
;;	     (if (not (eq (process-status sprocess) 'run))
;;		 (S-error "S process has died unexpectedly.")
;;	       (if (> (setq timeout (1+ timeout)) S-loop-timeout)
;;		   (S-error "Timeout waiting for prompt. Check inferior-S-prompt or S-loop-timeout."))
;;	       (accept-process-output)
;;	       (goto-char (point-max))
;;(setq end (point))
;;	       (beginning-of-line)
;;(setq e (buffer-substring (point) end))
;;(if (equal e inferior-S-debug-prompt)
;;    (S-error "Debug prompt"))
;;	       (setq r (looking-at inferior-S-prompt))
;;	       (not (or r (looking-at ".*\\?\\s *"))))))
;;    (goto-char (point-max))
;;    (set-buffer cbuffer)
;;    (symbol-value r)))
;;
;;
;;
;;; S-mode.el line 204
;;(setq S-dump-error-re "Problem")
;;
;;;; S-mode.el line 655
;;(defun S-parse-errors (showerr)
;;  "Jump to error in last loaded S source file.
;;With prefix argument, only shows the errors S reported."
;;  (interactive "P")
;;  (S-make-buffer-current)
;;  (let ((errbuff (get-buffer S-error-buffer-name)))
;;    (if (not errbuff)
;;        (error "You need to do a load first!")
;;      (set-buffer errbuff)
;;      (goto-char (point-max))
;;      (if
;;          (re-search-backward ", file \"" nil t)
;;	  (let* ((beg-pos (progn (re-search-forward "\"" nil t) (point)))
;;		 (end-pos (progn (re-search-forward "\"" nil t) (- (point) 1)))
;;		 (filename (buffer-substring beg-pos end-pos))
;;                 (fbuffer (get-file-buffer filename))
;;                 (linenum (string-to-int
;;			   (progn (re-search-backward "," nil t)
;;				  (current-word))))
;;		 (end-pos (point))
;;                 (beg-pos (progn (goto-char (point-min))
;;				 (re-search-forward ":" nil t)
;;				 (1+ (point))))
;;                 (errmess (buffer-substring beg-pos end-pos))
;;		 )
;;            (if showerr
;;                  (S-display-temp-buffer errbuff)
;;              (if fbuffer nil
;;                (setq fbuffer (find-file-noselect filename))
;;                (save-excursion
;;                  (set-buffer fbuffer)
;;                  (S-mode)))
;;              (pop-to-buffer fbuffer)
;;              (goto-line linenum))
;;            (princ errmess t))
;;        (message "Not a syntax error.")
;;        (S-display-temp-buffer errbuff)))))
;;
;;
;;
;;;; S-inf.el line 584
;;(defun S-prompt-wait (proc &optional start-of-output)
;;  "Wait for a prompt to appear at BOL of current buffer
;;PROC is the S process. Does not change point"
;;  (if start-of-output nil (setq start-of-output (point-min)))
;;  (save-excursion
;;    (while (progn
;;	     ;; get output if there is some ready
;;	     (accept-process-output proc 0 500) 
;;	     (goto-char (marker-position (process-mark proc)))
;;	     (beginning-of-line)
;;
;;	     (if (re-search-forward inferior-S-debug-prompt nil t)
;;		 (if (equal (get-buffer S-error-buffer-name)
;;			    (get-buffer S-error-buffer-name))
;;		     (let* ((sprocess (get-S-process S-current-process-name))
;;			    (sbuffer (process-buffer sprocess)))
;;		       (set-buffer sbuffer)
;;		       (process-send-string sprocess "n\n")
;;		       (accept-process-output sprocess)
;;		       (beginning-of-line); delete inferior-S-debug-prompt
;;		       (kill-line)
;;		       (insert "> ")))
;;
;;	     (if (< (point) start-of-output) (goto-char start-of-output))
;;	     (not (looking-at inferior-S-primary-prompt)))))))
;;



 ; Provide package

(provide 'essd-s+5)

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

;;; essd-s+5.el ends here
