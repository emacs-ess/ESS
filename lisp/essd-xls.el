;;; essd-xls.el --- XLispStat customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/06/14 23:14:17 $
;; Version: $Revision: 1.6 $
;; RCS: $Id: essd-xls.el,v 1.6 1997/06/14 23:14:17 rossini Exp $
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
;;; This file defines all the Splus 3.x customizations for ess-mode.

;;;
;;: $Log: essd-xls.el,v $
;;: Revision 1.6  1997/06/14 23:14:17  rossini
;;: finally setup.
;;:
;;;

;;; Code:


(defun ess-XLS-shortcut-pre-run-hook ()
  "Initialize variables."
  (setq-default inferior-ess-program          inferior-XLS-program-name
	ess-proc-prefix               "XLS"
	ess-version-running           "XLS"
	inferior-ess-primary-prompt   "> ?" 
	inferior-ess-help-command     "(help '%s)\n"
	inferior-ess-exit-command     "(exit)\n")
)

(defun ess-XLS-shortcut-post-run-hook ()
  "Remove initialization."
  (remove-hook 'ess-pre-run-hook 'ess-XLS-shortcut-pre-run-hook))


(defun XLS () "Call 'XLS', but this is only minimally correct..."
  (interactive)
  (add-hook 'ess-pre-run-hook  'ess-XLS-shortcut-pre-run-hook)
  (add-hook 'ess-post-run-hook 'ess-XLS-shortcut-post-run-hook)
  (setq     ess-proc-prefix    "XLS")
  (inferior-ess))


 ; Provide package

(provide 'essd-xls)

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

;;; ess-site.el ends here
