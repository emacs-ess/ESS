;;; essd-iw32.el --- ESS customization for ddeclients under Windows 9x/NT
;;; Richard M. Heiberger, December 1998

;; Copyright (C) 1998,  Richard M. Heiberger <rmh@fisher.stat.temple.edu>

;; Author: Richard M. Heiberger  <rmh@fisher.stat.temple.edu>
;; Maintainer: A.J. Rossini <rossinI@stat.sc.edu>
;; Created: 9 Dec 1998
;; Modified: $Date: 1998/12/11 00:57:59 $
;; Version: $Revision: 1.1 $
;; RCS: $Id: ess-iw32.el,v 1.1 1998/12/11 00:57:59 rossini Exp $


;; This file is part of ESS

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

;; Code for dealing with running external processes on Windows 9x/NT
;; through ddeclient.

;;; Code:

 ; Requires and autoloads

(require 'ess-mode)


;; C-c C-r
(defun ess-eval-region-ddeclient (start end toggle &optional message)
  "Send the current region to the inferior ESS process."
  (interactive "r\nP")
;;(call-process-region start end
;;                     "ddeclient" nil nil nil "S-PLUS" "SCommand")
  (call-process-region start end
                       inferior-ess-ddeclient nil nil nil
		       inferior-ess-client-name inferior-ess-client-command)
)

;; C-c C-n
(defun ess-eval-line-and-next-line-ddeclient ()
  "Evaluate the current line visibly and move to the next line."
  ;; From an idea by Rod Ball (rod@marcam.dsir.govt.nz)
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      ;; RDB modified to go to end of S buffer so user can see result
      (ess-eval-region-ddeclient (buffer-substring (point) end) nil t)))
  (next-line 1))




(put 'ess-external-minor-mode 'permanent-local t)
(or (assq 'ess-external-minor-mode minor-mode-alist)
;    (let ((mode-name (concat " [external " inferior-ess-program-name "]")))
	  (setq minor-mode-alist
		(append minor-mode-alist
			(list '(ess-external-minor-mode
;				mode-name
				" [external]"
)))))
;)


(if ess-external-mode-map
    nil
  (progn
    (setq ess-external-mode-map (copy-keymap ess-mode-map))
    (define-key ess-external-mode-map "\C-c\C-n" 'ess-eval-line-and-next-line-ddeclient)
    (define-key ess-external-mode-map "\C-c\C-r" 'ess-eval-region-ddeclient)
))


(defun ess-external-minor-mode (&optional arg)
  "Toggle Ess-external minor mode.
With arg, turn ess-external minor mode on if arg is positive, off otherwise.
ess-external minor mode uses dde to send lines to an ESS process external to emacs."
  (interactive "P")
  (if (setq ess-external-minor-mode
	    (if (null arg) (not ess-external-minor-mode)
	      (> (prefix-numeric-value arg) 0)))
      (use-local-map ess-external-mode-map)  ;; external program, say Sqpe
      (use-local-map ess-mode-map))          ;; iESS[] buffer running, say SPlus

  (force-mode-line-update)
  (setq mode-line-process ess-dialect)
)

(provide 'ess-iw32)

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
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-iw32.el ends here
