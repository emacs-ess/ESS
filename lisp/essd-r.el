;;; essd-r.el --- R customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/07/07 16:25:29 $
;; Version: $Revision: 1.17 $
;; RCS: $Id: essd-r.el,v 1.17 1997/07/07 16:25:29 rossini Exp $
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
;;: Revision 1.17  1997/07/07 16:25:29  rossini
;;: set variables in the "call". (i.e. R2).
;;:
;;: Revision 1.16  1997/07/03 14:38:57  rossini
;;: changed alist -- to not use defs in ess.el!
;;:
;;: Revision 1.15  1997/07/03 14:28:48  rossini
;;: need to use EDEBUG!  yowso...
;;:
;;: Revision 1.14  1997/07/03 14:17:53  rossini
;;: added messages for debugging.
;;:
;;: Revision 1.13  1997/07/03 13:58:20  rossini
;;: stuff
;;:
;;: Revision 1.12  1997/07/03 13:26:35  rossini
;;: added alist's for setting up things properly.
;;:
;;: Revision 1.11  1997/07/03 12:00:45  rossini
;;: added alist for customization...
;;:
;;: Revision 1.10  1997/07/02 16:16:19  rossini
;;: moved vars to R defun.
;;:
;;: Revision 1.9  1997/06/22 23:49:46  rossini
;;: -> ESS.
;;:
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


(defvar R-customize-alist
  '((ess-customize-alist           . R-customize-alist)
    (ess-proc-prefix               . "R")
    (ess-version-running           . "R" )
    (inferior-ess-program          . "R" ) ; inferior-R-program-name)
    (inferior-ess-objects-command  . "if(%d == 1) ls() else builtins()")
    (ess-help-sec-regex            . "^\\s *[A-Z[a-z. ---]+:$") ;ess-help-R-sec-regex)
    (ess-help-sec-keys-alist       . '((?a . "\\s *Arguments:")
					(?d . "\\s *Description:")
					(?n . "\\s *Note:")
					(?r . "\\s *References:")
					(?v . "\\s *Value[s]?")
					(?s . "\\s *See Also:")
					(?e . "\\s *Examples:")))  ; ess-help-R-sec-keys-alist)
    (inferior-ess-help-command     . "help(\"%s\")\n")
    (inferior-ess-exit-command     . "q()\n")
    (ess-loop-timeout              . 100000 )
    (inferior-ess-primary-prompt   . "[][a-zA-Z0-9() ]*> ?"))
  "Variables to customize for R")

(defun ess-R-shortcut-post-run-hook ()
  "Remove initialization."
  (remove-hook 'ess-pre-run-hook 'ess-R-shortcut-pre-run-hook))

(defun R () "Call 'R', the 'Splus clone' from Robert & Ross (Auckland, NZ.)"
  (interactive)
;;  (add-hook 'ess-pre-run-hook  'ess-R-shortcut-pre-run-hook)
;;  (add-hook 'ess-post-run-hook 'ess-R-shortcut-post-run-hook)
 
  (setq-default ess-proc-prefix              "R"
		ess-version-running          "R" ;using 'ls()' instead of objects..
		inferior-ess-program         inferior-R-program-name
		inferior-ess-objects-command "if(%d == 1) ls() else builtins()"
		ess-help-sec-regex           ess-help-R-sec-regex
		ess-help-sec-keys-alist      ess-help-R-sec-keys-alist
		inferior-ess-help-command    "help(\"%s\")\n"
		inferior-ess-exit-command    "q()\n"
		ess-loop-timeout             100000 ; default is 50000
		inferior-ess-primary-prompt  "[][a-zA-Z0-9() ]*> ?") 
					;[] for browser()
  (inferior-ess))

(defun R2 ()
  "Call 'R', the 'Splus clone' from Robert & Ross (Auckland, NZ)."
  (interactive)
  ;; Setup the needed vars
  (setq ess-customize-alist R-customize-alist) ; setq or setq-default?
  (ess-set-vars ess-customize-alist (current-buffer))
  ;; debug, only
  (message "(R2): ess-proc-prefix=%s , buf=%s"
	   ess-proc-prefix (current-buffer))
  ;; now run...
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
