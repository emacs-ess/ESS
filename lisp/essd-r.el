;;; essd-r.el --- R customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/07/31 12:51:50 $
;; Version: $Revision: 1.27 $
;; RCS: $Id: essd-r.el,v 1.27 1997/07/31 12:51:50 rossini Exp $
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
;;: Revision 1.27  1997/07/31 12:51:50  rossini
;;: changed the save call to reflect R.
;;:
;;: Revision 1.26  1997/07/30 13:13:21  rossini
;;: vars back.
;;:
;;: Revision 1.25  1997/07/26 02:12:28  rossini
;;: need primary prompt...
;;:
;;: Revision 1.24  1997/07/26 01:38:56  rossini
;;: changed objects command, as per R-0.50-a1.
;;:
;;: Revision 1.23  1997/07/17 20:41:36  rossini
;;: cleaned up for release.
;;:
;;: Revision 1.22  1997/07/17 18:31:57  rossini
;;: formatting.
;;:
;;: Revision 1.21  1997/07/17 18:20:42  rossini
;;: replaced message with write to dribble buffer.
;;:
;;: Revision 1.20  1997/07/07 21:39:05  rossini
;;: can't set variables in initial call!
;;:
;;: Revision 1.19  1997/07/07 16:51:22  rossini
;;: R2 -> R in debug-message.
;;:
;;: Revision 1.18  1997/07/07 16:45:59  rossini
;;: R is now the "new-way".
;;:
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

(defconst ess-help-R-sec-keys-alist 
  '((?a . "\\s *Arguments:") 
    (?d . "\\s *Description:")
    (?n . "\\s *Note:")
    (?r . "\\s *References:") 
    (?v . "\\s *Value[s]?")	;
    (?s . "\\s *See Also:") 
    (?e . "\\s *Examples:") 
    )) ;; "Alist of (key . string) pairs for use in section searching."

(defconst ess-help-R-sec-regex "^\\s *[A-Z[a-z. ---]+:$")

(defvar R-customize-alist
  '((ess-customize-alist           . R-customize-alist)
    (ess-proc-prefix               . "R")
    (ess-version-running           . "0.50-a1" )
    (inferior-ess-program          . inferior-R-program-name)
    (ess-help-sec-regex            . ess-help-R-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-R-sec-keys-alist)
    (inferior-ess-help-command     . "help(\"%s\")\n")
    (inferior-ess-objects-command  . "objects(pos = %d)\n")
    (inferior-ess-exit-command     . "q()\n")
    (ess-loop-timeout              . 100000 )
    (ess-retr-lastvalue-command .
     ".Last.value <- get(\"smode.lvsave\",envir=1)\n")
    (ess-save-lastvalue-command .
     "assign(\"smode.lvsave\",.Last.value,envir=1)\n")
    (inferior-ess-primary-prompt   . "[][a-zA-Z0-9() ]*> ?"))
  "Variables to customize for R")

(defun R ()
  "Call 'R', the 'Splus clone' from Robert & Ross (Auckland, NZ).
New way to do it."
  (interactive)
  (setq ess-customize-alist R-customize-alist)
  ;; for debugging only
  (ess-write-to-dribble-buffer
   (format "(R): ess-proc-prefix=%s , buf=%s \n"
	   ess-proc-prefix
	   (current-buffer)))
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
