;;; essd-sas.el --- SAS customization

;; Copyright (C) 1997 Richard M. Heiberger and A. J. Rossini

;; Author: Richard M. Heiberger <rmh@astro.ocis.temple.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 20 Aug 1997
;; Modified: $Date: 1997/10/20 20:09:26 $
;; Version: $Revision: 1.13 $
;; RCS: $Id: essd-sas.el,v 1.13 1997/10/20 20:09:26 rossini Exp $
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
;;; This file defines all the SAS customizations for  ESS.

;;; Autoloads:

(require 'essl-sas)

(autoload 'inferior-ess "ess-inf" "Run an ESS process")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process")

(defvar inferior-SAS-args "-stdio -linesize 80 -noovp"
  "*Arguments to use for starting SAS.")

;;; Code:

(defun ess-SAS-pre-run-hook () "Set up log and list files for interactive SAS."
  (interactive)
  (if (get-buffer "*shell*")
      (save-excursion       
	(set-buffer "*shell*")
	(setq ess-shell-buffer-name (rename-buffer "*ess-shell-regular*"))
	(setq ess-shell-buffer-name-p t)
	))

  (if (get-buffer "*myfile.lst*")
      nil
    (shell)
    (accept-process-output (get-buffer-process (current-buffer)))
    (setq ess-sas-lst (ess-insert-accept "tty"))
    (rename-buffer "*myfile.lst*"))
  
  (if (get-buffer "*myfile.log*")
      nil
    (shell)
    (accept-process-output (get-buffer-process (current-buffer)))
    (setq ess-sas-log (ess-insert-accept "tty"))
    (rename-buffer "*myfile.log*"))
  (setq additional-inferior-SAS-args (concat " "
					     ess-sas-lst
					     " "
					     ess-sas-log))
  (setq inferior-SAS-args (concat inferior-SAS-args
				  additional-inferior-SAS-args))
  
  (if ess-shell-buffer-name-p
      (save-excursion       
	(set-buffer ess-shell-buffer-name)
	(rename-buffer "*shell*")
	(setq ess-shell-buffer-name-p nil)
	))

  (delete-other-windows)
  (split-window-vertically)
  (split-window-vertically)
  (switch-to-buffer (nth 2 (buffer-list)))
  (other-window 2)
  (switch-to-buffer "*myfile.log*")
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer "*myfile.lst*")
  (other-window 1)

  ;;workaround
  (setq inferior-SAS-program-name (concat ess-lisp-directory "/" "ess-sas-sh-command"))
  (setq inferior-ess-program inferior-SAS-program-name)
  ;; workaround

  )

(defun ess-insert-accept (command) "" (interactive)
  (goto-char (point-max))
  (insert command)
  (comint-send-input)
  (accept-process-output (get-buffer-process (current-buffer)))
  (forward-line -1)
  (let* ((beg (point))
	 (ess-tty-name (progn (end-of-line) (buffer-substring beg (point)))))
    (goto-char (point-max))
    ess-tty-name
    )
  )


(defvar SAS-customize-alist
  '((ess-local-customize-alist     . 'SAS-customize-alist)
    (ess-language                  . "SAS")
    (ess-dialect                   . "SAS")
    (ess-mode-editing-alist        . SAS-editing-alist) ; from essl-sas.el
    (inferior-ess-program          . inferior-SAS-program-name)
    (ess-help-sec-regex            . "^[A-Z. ---]+:$")
    (ess-help-sec-keys-alist       . " ")
    (inferior-ess-objects-command  . "objects(%d)")
    (inferior-ess-help-command     . "help(\"%s\",pager=\"cat\",window=F)\n")
    (inferior-ess-exit-command     . "q()\n")
    (ess-loop-timeout              .  100000 )
    (inferior-ess-primary-prompt   . "^")
    (inferior-ess-secondary-prompt . "^")
    (inferior-ess-start-file       . nil) ;"~/.ess-SAS")
    (inferior-ess-start-args       . inferior-SAS-args) 
    (ess-pre-run-hook              . 'ess-SAS-pre-run-hook)
    (ess-local-process-name        . nil))
  "Variables to customize for SAS")

;;; The functions of interest (mode, inferior mode)

(defun SAS-mode (&optional proc-name)
  "Major mode for editing SAS source.  See ess-mode for more help."
  (interactive)
  (setq ess-customize-alist SAS-customize-alist)
  (ess-mode SAS-customize-alist proc-name))

(defun SAS ()
  "Call 'SAS', from SAS Institute."
  (interactive)
  (setq ess-customize-alist SAS-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(SAS): ess-dialect=%s , buf=%s \n"
	   ess-dialect
	   (current-buffer)))
  (inferior-ess))



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



