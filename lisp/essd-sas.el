;;; essd-sas.el --- sas xustomization

;; Copyright (C) 1997 A. J. Rossini

;; Author: Richard M. Heiberger <rmh@astro.ocis.temple.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 20 Aug 1997
;; Modified: $Date: 1997/08/26 22:54:23 $
;; Version: $Revision: 1.2 $
;; RCS: $Id: essd-sas.el,v 1.2 1997/08/26 22:54:23 rossini Exp $
;;
;; Keywords: start up, configuration.

;; This file is part of ess-mode.

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
;;; This file defines all the SAS customizations for ess-mode.

;;;
;;: $Log: essd-sas.el,v $
;;: Revision 1.2  1997/08/26 22:54:23  rossini
;;: *** empty log message ***
;;:
;;: Revision 1.1  1997/08/25 14:30:49  rossini
;;: Initial revision
;;:
;;: Revision 1.1  1997/08/20 23:12:08  heiberger
;;: Initial set up.
;;:
;;;

;;; Autoloads:

(autoload 'inferior-ess "ess-inf" "Run an ESS process")


;;; to be moved to ess-site.el (1.2)
(setq auto-mode-alist
	(append
	 '(("\\.sas\\'" . SAS-mode)
	   ("\\.SAS\\'" . SAS-mode))
	 auto-mode-alist))

;;; to be uncommented and  moved to ess-site.el (1.3)
;(autoload 'SAS-mode "ess-mode"
;  "Major mode for editing SAS source code." t)
;(autoload 'SAS-transcript-mode
;  "ess-trns" "ESS source eval mode" t)


;;; to be moved to ess-site.el (1.4)
;;(setq-default inferior-SAS-program-name "sas")

;;; to be moved to ess-site.el (1.5)
;;(require 'essd-sas)  ;; rmh   can't be in essd-sas.el

;;; to be moved to ess-vars.el
(defvar inferior-SAS-program-name "mysas"
  "*Program name for invoking an inferior S with SAS().")
;(setq-default inferior-sas-args  "-stdio -linesize 80")

;;; to be moved to ess-mode.el
(defun SAS-mode (&optional proc-name)
  "Major mode for editing SAS source.  See ess-mode for more help."
  (interactive)

  (ess-mode proc-name ess-language))

;;; Code:

(defvar SAS-customize-alist
  '((ess-language                 . "SAS")
    (ess-dialect                  . "SAS")
    (inferior-ess-program         . inferior-SAS-program-name)
    (ess-help-sec-regex           . "^[A-Z. ---]+:$")
    (ess-help-sec-keys-alist      . '((?a . "ARGUMENTS:")
				      (?b . "BACKGROUND:")
				      (?B . "BUGS:")
				      (?d . "DETAILS:")
				      (?D . "DESCRIPTION:")
				      (?e . "EXAMPLES:")
				      (?n . "NOTE:")
				      (?o . "OPTIONAL ARGUMENTS:")
				      (?r . "REQUIRED ARGUMENTS:")
				      (?R . "REFERENCES:")
				      (?s . "SIDE EFFECTS:")
				      (?S . "SEE ALSO:")
				      (?u . "USAGE:")
				      (?v . "VALUE:")))
    (inferior-ess-objects-command  . "objects(%d)")
    (inferior-ess-help-command     . "help(\"%s\",pager=\"cat\",window=F)\n")
    (inferior-ess-exit-command     . "q()\n")
    (ess-loop-timeout              .  100000 )
    (inferior-ess-primary-prompt   .  "^\'")
    (inferior-ess-secondary-prompt . "+ ?"))
 "Variables to customize for SAS")


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

