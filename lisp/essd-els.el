;;; essd-els.el --- S-PLUS 3.x at another location customization
;; Copyright (C) 1998 Richard M. Heiberger

;; Author: Richard M. Heiberger <rmh@fisher.stat.temple.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: December 1998
;; Modified: $Date: 1999/11/03 22:46:27 $
;; Version: $Revision: 1.7 $
;; RCS: $Id: essd-els.el,v 1.7 1999/11/03 22:46:27 ess Exp $
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
;;; This file defines all the S-PLUS 3.x customizations for ess-mode.

;;; Requires and Autoloads:

(require 'essl-s)

(autoload 'inferior-ess "ess-inf" "Run an ESS process")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process")

; Code:

(defvar S+elsewhere-dialect-name "S+3"
  "Name of 'dialect' for S-PLUS 3.x at another location.")
					;easily changeable in a user's .emacs

(defvar S+elsewhere-customize-alist
  '((ess-local-customize-alist     . 'S+elsewhere-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . S+elsewhere-dialect-name)
    (ess-suffix                    . "S")
    (ess-dump-filename-template    . (concat (user-login-name)
					     ".%s."
					     ess-suffix))
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-syntax-table         . S-syntax-table)
    (ess-help-sec-regex            . ess-help-S+-sec-regex)
    (ess-help-sec-keys-alist       . S+-help-sec-keys-alist)
    (ess-loop-timeout              . 500000 )
    (ess-object-name-db-file       . "ess-spelsewhere-namedb.el" )
    (ess-retr-lastvalue-command
     . ".Last.value <- get(\".ess.lvsave\",frame=0)\n")
    (ess-save-lastvalue-command
     . "assign(\".ess.lvsave\",.Last.value,frame=0)\n")
    (inferior-ess-program          . inferior-S-elsewhere-program-name)
    (inferior-ess-objects-command  . "objects(%d)\n")
    (inferior-ess-help-command     . "help(\"%s\",pager=\"cat\",window=F)\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-primary-prompt   . "[a-zA-Z0-9() ]*> ?")
    (inferior-ess-secondary-prompt . "+ ?")
    (inferior-ess-start-file       . nil) ;"~/.ess-S+3")
    (inferior-ess-start-args       . "-i"))
 "Variables to customize for S+elsewhere")


(defun S+elsewhere (&optional proc-name)
  "Call 'S-PLUS 3.x', the 'Real Thing'  from StatSci."
  (interactive)
  (setq ess-customize-alist S+elsewhere-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(S+elsewhere): ess-dialect=%s, buf=%s\n" ess-dialect
	   (current-buffer)))
  (inferior-ess))


(defun S+elsewhere-mode (&optional proc-name)
  "Major mode for editing S+3 source.  See ess-mode for more help."
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


(defun ess-select-alist-dialect ()
  "This is UGLY and NEEDS TO BE FIXED."
  (interactive)
  (let ((dialect (read-string "Which Dialect (stata, r, sp3, sp5, xls)?")))
    (if (string= dialect "stata")
	STA-customize-alist
      (if (string= dialect "sp3")
	  S+3-customize-alist
	(if (string= dialect "r")
	    R-customize-alist
	  (if (string= dialect "xls")
	      XLS-customize-alist
	    S+5-customize-alist)))))))

(defun ESS-elsewhere (&optional proc-name)
  "Call an inferior process from ELSEWHERE."
  (interactive)
  ;; Need to select a elsewhere-customize-alist
  (let ((elsewhere-customize-alist (ess-select-alist-dialect)))
    (ess-change-alist 'inferior-ess-program
		      inferior-ESS-elsewhere-program-name
		      elsewhere-customize-alist)
    (setq ess-customize-alist elsewhere-customize-alist)
    (ess-write-to-dribble-buffer
     (format "\n(ESS-elsewhere): ess-dialect=%s, buf=%s\n" ess-dialect
	     (current-buffer)))
    (inferior-ess)))


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
