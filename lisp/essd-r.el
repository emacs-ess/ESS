;;; essd-r.el --- R customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/06/15 08:42:11 $
;; Version: $Revision: 1.8 $
;; RCS: $Id: essd-r.el,v 1.8 1997/06/15 08:42:11 rossini Exp $
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
;;; This file defines all the R customizations for ess-mode.

;;;
;;: $Log: essd-r.el,v $
;;: Revision 1.8  1997/06/15 08:42:11  rossini
;;: setq -> setq-default for initialziation.  I think this is right!
;;:
;;: Revision 1.7  1997/06/15 08:17:37  rossini
;;: added autolaod of inferior-ess
;;:
;;: Revision 1.6  1997/06/14 23:13:27  rossini
;;: finally setup.
;;:
;;:
;;;

;;; Autoloads:

(autoload 'inferior-ess "ess-inf" "Run an ESS process")

;;; Code:

(defun ess-R-shortcut-pre-run-hook ()
  "Initialize variables."
  (setq-default ess-proc-prefix              "R"
	ess-version-running          "R" ;-> using 'ls()' instead of objects..
	inferior-ess-program         inferior-R-program-name
	inferior-ess-objects-command "if(%d == 1) ls() else builtins()"
	ess-help-sec-regex           ess-help-R-sec-regex
	ess-help-sec-keys-alist      ess-help-R-sec-keys-alist
	inferior-ess-help-command    "help(\"%s\")\n"
	inferior-ess-exit-command    "q()\n"
	ess-loop-timeout             100000 ;- default is 50000
	inferior-ess-primary-prompt  "[][a-zA-Z0-9() ]*> ?") ;; [] for browser()
  (remove-hook  'ess-post-run-hook 'ess-execute-screen-options); length fails
  (add-hook 'S-mode-load-hook
	    '(lambda () (setq-default S-proc-prefix "R")))
  ;;(setq inferior-S-search-list-command "search()\n");- is failing in R
  (setenv "PAGER" inferior-ess-pager)   ;-- a MUST for the old-style
					;(nroff) help  above ! 
)

(defun ess-R-shortcut-post-run-hook ()
  "Remove initialization."
  (remove-hook 'ess-pre-run-hook 'ess-R-shortcut-pre-run-hook))

(defun R () "Call 'R', the 'Splus clone' from Robert & Ross (Auckland, NZ.)"
  (interactive)
  (add-hook 'ess-pre-run-hook  'ess-R-shortcut-pre-run-hook)
  (add-hook 'ess-post-run-hook 'ess-R-shortcut-post-run-hook)
  (setq     ess-proc-prefix    "R")
  (inferior-ess))


 ; Provide package

(provide 'essd-r)

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
