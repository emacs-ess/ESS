;;; essd-iw32.el --- ESS customization for ddeclients under Windows 9x/NT
;; Copyright (C) 1998,  Richard M. Heiberger <rmh@fisher.stat.temple.edu>

;; Author: Richard M. Heiberger  <rmh@fisher.stat.temple.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 9 Dec 1998
;; Modified: $Date: 1999/01/11 16:46:48 $
;; Version: $Revision: 1.3 $
;; RCS: $Id: ess-iw32.el,v 1.3 1999/01/11 16:46:48 maechler Exp $


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
(require 'ess-inf)
(require 'ess-help)


;; C-c C-r
(defun ess-eval-region-ddeclient (start end toggle &optional message)
"*Loop through lines in region and send them to ESS via ddeclient."
  (narrow-to-region start end)
  (beginning-of-buffer)
  (let ((beg))
    (while (< (point) (point-max))
      (setq beg (point))
      (end-of-line)
      ;;(call-process-region start end
      ;;                     "ddeclient" nil nil nil "S-PLUS" "SCommand")
      (call-process-region
       beg (point)
       inferior-ess-ddeclient nil nil nil
       inferior-ess-client-name inferior-ess-client-command)
      (forward-line 1))
    (widen)))

(fset 'ess-eval-region-original (symbol-function  'ess-eval-region))

(defun ess-eval-region (start end toggle &optional message)
  (interactive "r\nP")
  (if (equal (ess-get-process-variable
	      ess-current-process-name 'inferior-ess-ddeclient)
	     (default-value 'inferior-ess-ddeclient))
      (ess-eval-region-original start end toggle message)
    (ess-force-buffer-current "Process to load into: ")
    (ess-eval-region-ddeclient start end toggle message))
)


;; (defun ess-eval-visibly-ddeclient (start end toggle &optional message)
;; defun ess-eval-region-visibly-ddeclient (start end toggle &optional message)
;;  "Send the current region to the inferior ESS process."
;;  (interactive "r\nP")
;; ;(call-process-region start end
;; ;                     "ddeclient" nil nil nil "S-PLUS" "SCommand")
;;  (call-process-region start end
;; 			 inferior-ess-ddeclient nil nil nil
;; 			  inferior-ess-client-name inferior-ess-client-command)
;; 

;;; switch between Splus by ddeclient and Splus running in an emacs buffer
(defun ess-eval-visibly-ddeclient (text-withtabs &optional invisibly eob)
    (save-excursion
      (set-buffer (get-buffer-create "*ESS-temporary*"))
      (ess-setq-vars-local ess-customize-alist (current-buffer))
      (erase-buffer)
      (insert text-withtabs)
      (ess-eval-region-ddeclient (point-min) (point-max) t t)))

(fset 'ess-eval-visibly-original (symbol-function  'ess-eval-visibly))

(defun ess-eval-visibly (text-withtabs &optional invisibly eob)
  (if (equal (ess-get-process-variable
	      ess-current-process-name 'inferior-ess-ddeclient)
	     (default-value 'inferior-ess-ddeclient))
      (ess-eval-visibly-original text-withtabs invisibly eob)
      (ess-eval-visibly-ddeclient text-withtabs invisibly eob)))


;; C-c C-v
;;; this works for Sqpe+4 and S+4
(defun ess-display-help-on-object-ddeclient (object)
  "Display the ESS documentation for OBJECT in another window.
If prefix arg is given, forces a query of the ESS process for the help
file.  Otherwise just pops to an existing buffer if it exists."
  (ess-force-buffer-current "Process to load into: ")
  (ess-eval-visibly (concat "help(" object ")")))


(fset 'ess-display-help-on-object-original
      (symbol-function  'ess-display-help-on-object))

(defun ess-display-help-on-object (object)
  (interactive "sHelp on: ")
  (if (equal (ess-get-process-variable
	      ess-current-process-name 'inferior-ess-ddeclient)
	     (default-value 'inferior-ess-ddeclient))
      (ess-display-help-on-object-original object)
    (ess-display-help-on-object-ddeclient object))
  (widen))

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
