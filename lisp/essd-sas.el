;;; essd-sas.el --- SAS customization

;; Copyright (C) 1997 Richard M. Heiberger and A. J. Rossini

;; Author: Richard M. Heiberger <rmh@astro.ocis.temple.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 20 Aug 1997
;; Modified: $Date: 1997/12/01 16:01:45 $
;; Version: $Revision: 5.0 $
;; RCS: $Id: essd-sas.el,v 5.0 1997/12/01 16:01:45 rossini Exp $
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
;; This file defines all the SAS customizations for  ESS behaviors.  See
;; essl-sas for the underlying general modifications.


;;; Autoloads:

(require 'comint)
(require 'shell)

(require 'essl-sas)


(autoload 'inferior-ess "ess-inf" no-doc t)
(autoload 'ess-mode "ess-mode" no-doc t)
(autoload 'ess-proc-name "ess-inf" no-doc nil)

(defvar inferior-SAS-args "-stdio -linesize 80 -noovp -nosyntaxcheck"
  "*Arguments to use for starting SAS.")

(defvar inferior-SAS-args-temp nil
  "Hack variable, needed for args preprocessing. 
Better logic needed!  (see 2 uses, in this file).")

;;; Code:

(defun ess-SAS-pre-run-hook (temp-ess-dialect)
  "Set up log and list files for interactive SAS."

  (let* ((ess-shell-buffer-name-flag (get-buffer "*shell*"))
	 ess-shell-buffer-name
	 ;; isn't pretty yet.
	 ;;  ess-local-process-name is defined after this function.
	 ;;  it needs to be defined prior to this function.
	 (tmp-procname (let ((ntry 0)
			     (done nil))
			 ;; find a non-existent process
			 (while (not done)
			   (setq ntry (1+ ntry)
				 done (not
				       (get-process (ess-proc-name
						     ntry
						     temp-ess-dialect)))))
			 (ess-proc-name ntry temp-ess-dialect)))
	 ;; Following was tmp-local-process-name.  Stolen from inferior-ess
	 (ess-sas-lst-bufname (concat "*" tmp-procname ".lst*"))
	 (ess-sas-log-bufname (concat "*" tmp-procname ".log*"))
	 (explicit-shell-file-name "/bin/sh")
	 inferior-SAS-redirect-args
	 ess-sas-lst
	 ess-sas-log)
    
    (ess-write-to-dribble-buffer
     (format "(ess-SAS-pre-run-hook 1): ess-lang=%s, ess-dialect=%s, temp-dialect=%s, buf=%s \n"
	     ess-language
	     ess-dialect
	     temp-ess-dialect
	     (current-buffer)))
    ;; If someone is running a *shell* buffer, rename it to avoid
    ;; inadvertent nuking.
    (if ess-shell-buffer-name-flag
	(save-excursion       
	  (set-buffer "*shell*")
	  (setq ess-shell-buffer-name
		(rename-buffer "*ess-shell-regular*" t))))
    
    ;; Construct the LST buffer for output
    (if (get-buffer ess-sas-lst-bufname)
	nil
      (shell)
      (accept-process-output (get-buffer-process (current-buffer)))
      (sleep-for 2) ; need to wait, else working too fast!
      (setq ess-sas-lst (ess-insert-accept "tty"))
      (SAS-listing-mode)
      (shell-mode)
      (ess-listing-minor-mode t)
      (rename-buffer ess-sas-lst-bufname t))
    
     ;; Construct the LOG buffer for output
    (if (get-buffer  ess-sas-log-bufname)
	nil
      (shell)
      (accept-process-output (get-buffer-process (current-buffer)))
      (sleep-for 2) ; need to wait, else working too fast!
      (setq ess-sas-log (ess-insert-accept "tty"))
      (SAS-log-mode)
      (shell-mode)
      (ess-transcript-minor-mode t)
      (rename-buffer ess-sas-log-bufname t))
    
    (setq inferior-SAS-redirect-args (concat " "
					     ess-sas-lst
					     " "
					     ess-sas-log
					     " ")
	  inferior-SAS-args-temp (concat inferior-SAS-redirect-args
					 inferior-SAS-args))
    
    ;; Restore the *shell* buffer
    (if ess-shell-buffer-name-flag
	(save-excursion       
	  (set-buffer ess-shell-buffer-name)
	  (rename-buffer "*shell*")))
    
    (delete-other-windows)
    (split-window-vertically)
    (split-window-vertically)
    (switch-to-buffer (nth 2 (buffer-list)))
    (other-window 2)
    (switch-to-buffer ess-sas-log-bufname)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer ess-sas-lst-bufname)
    (other-window 2)
    
    ;;workaround
    (setq inferior-SAS-program-name
	  (concat ess-lisp-directory "/" "ess-sas-sh-command"))
    (setq inferior-ess-program inferior-SAS-program-name)))
 
(defun ess-insert-accept (command)
  "Submit command to process, get next line."
  (interactive)
  (goto-char (point-max))
  (insert command)
  (comint-send-input)
  (accept-process-output (get-buffer-process (current-buffer)))
  (forward-line -1)
  (let* ((beg (point))
	 (ess-tty-name (progn (end-of-line) (buffer-substring beg (point)))))
    (goto-char (point-max))
    ess-tty-name))


(defvar SAS-customize-alist
  '((ess-local-customize-alist     . 'SAS-customize-alist)
    (ess-language                  . "SAS")
    (ess-dialect                   . "SAS")
    (ess-mode-editing-alist        . SAS-editing-alist) ; from essl-sas.el
    (ess-mode-syntax-table         . SAS-syntax-table)
    (inferior-ess-program          . inferior-SAS-program-name)
    (ess-help-sec-regex            . "^[A-Z. ---]+:$")
    (ess-help-sec-keys-alist       . " ")
    (ess-object-name-db-file       . "ess-sas-namedb.el")
    (inferior-ess-objects-command  . "objects(%d)")
    (inferior-ess-help-command     . "help(\"%s\",pager=\"cat\",window=F)\n")
    (inferior-ess-exit-command     . "endsas;\n")
    (ess-loop-timeout              .  100000 )
    (inferior-ess-primary-prompt   . "^")
    (inferior-ess-secondary-prompt . "^")
    (inferior-ess-start-file       . nil) ;"~/.ess-SAS")
    (inferior-ess-start-args       . inferior-SAS-args-temp) 
    ;; (ess-pre-run-hook              . 'ess-SAS-pre-run-hook)
    (ess-local-process-name        . nil))
  "Variables to customize for SAS")

;;; The functions of interest (mode, inferior mode)

(defvar sas-mode-local-map nil "contains modified local keymap for SAS")

(defun SAS-mode (&optional proc-name)
  "Major mode for editing SAS source.  See ess-mode for more help."
  (interactive)
  (setq ess-customize-alist SAS-customize-alist)
  (ess-mode SAS-customize-alist proc-name)

  ;; Local map settings, AFTER initialization.
  (setq sas-mode-local-map (copy-keymap (current-local-map)))
  (use-local-map sas-mode-local-map)
  (local-set-key "\t" 'sas-indent-line))

(defun SAS ()
  "Call 'SAS', from SAS Institute."
  (interactive)
  (setq-default ess-customize-alist SAS-customize-alist)
  (let* ((temp-dialect "SAS")) ;(cdr (rassoc ess-dialect SAS-customize-alist))))
    (ess-write-to-dribble-buffer
     (format "(SAS): ess-dial=%s, temp-dial=%s\n"
	     ess-dialect
	     temp-dialect))
    (ess-SAS-pre-run-hook temp-dialect)
    (inferior-ess)))



 ; Provide package

(provide 'essd-sas)

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

;;; essd-sas.el ends here



