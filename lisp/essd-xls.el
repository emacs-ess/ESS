;;; essd-xls.el --- XLispStat customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/08/25 14:31:04 $
;; Version: $Revision: 1.20 $
;; RCS: $Id: essd-xls.el,v 1.20 1997/08/25 14:31:04 rossini Exp $
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
;;: Revision 1.20  1997/08/25 14:31:04  rossini
;;: *** empty log message ***
;;:
;;: Revision 1.19  1997/07/31 11:52:39  rossini
;;: changed to reflect current.
;;:
;;: Revision 1.18  1997/07/31 11:32:53  rossini
;;: added program name.
;;:
;;: Revision 1.17  1997/07/17 20:40:50  rossini
;;: cleaned up for release.
;;:
;;: Revision 1.16  1997/07/17 18:31:21  rossini
;;: stuff
;;:
;;: Revision 1.15  1997/07/17 18:30:58  rossini
;;: XLS, not R!
;;:
;;: Revision 1.14  1997/07/17 18:20:23  rossini
;;: replaced message with a write-to-dribble-buffer.
;;:
;;: Revision 1.13  1997/07/07 21:39:29  rossini
;;: can't set variables in initial call!!
;;:
;;: Revision 1.12  1997/07/07 16:57:50  rossini
;;: need to use program name, not external variables...
;;:
;;: Revision 1.11  1997/07/07 16:53:46  rossini
;;: setup new-style, corrected alist.
;;:
;;: Revision 1.10  1997/07/07 16:51:45  rossini
;;: added new-style language variable setup.
;;:
;;: Revision 1.9  1997/07/03 13:36:16  rossini
;;: added inferior-ess autoload.
;;:
;;: Revision 1.8  1997/07/02 16:21:10  rossini
;;: removed hooks.
;;:
;;: Revision 1.7  1997/07/02 16:15:41  rossini
;;: moved variables to XLS defun.
;;:
;;: Revision 1.6  1997/06/14 23:14:17  rossini
;;: finally setup.
;;:
;;;

;;; Autoloads:

(autoload 'inferior-ess "ess-inf" "Run an ESS process")

;;; Code:

(defvar XLS-customize-alist
  '((ess-customize-alist           .  XLS-customize-alist )
    (ess-proc-prefix               .  "XLS"               )
    (ess-version-running           .  "3.50"              )
    (ess-loop-timeout              .  10000               )
    (ess-object-name-db-file       .  "ess-xls-namedb.el" )
    (ess-help-sec-regex            .  " ")
    (ess-help-sec-keys-alist       .  " ")
    (inferior-ess-primary-prompt   .  "> ?"               )
    (inferior-ess-program          .  inferior-XLS-program-name)
    (inferior-ess-help-command     .  "(help '%s)\n"      )
    (inferior-ess-objects-command  .  "(variables)\n"     )
    (inferior-ess-exit-command     .  "(exit)\n"          )
    (inferior-ess-start-file       . "~/.ess-XLS")
    (inferior-ess-start-args       . nil)
    )
  "Variables to customize for XLS")


(defun XLS-mode (&optional proc-name)
  "Major mode for editing XLispStat source.  NOT EVEN STARTED."
  (interactive)
  (setq ess-customize-alist XLS-customize-alist)
  (lisp-mode))


(defun XLS ()
  "Call 'XLispStat', the Lisp statistical system from Luke Tierney."

  (interactive)
  (setq ess-customize-alist XLS-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(XLS): ess-proc-prefix=%s , buf=%s\n"
  	   ess-proc-prefix (current-buffer)))
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
