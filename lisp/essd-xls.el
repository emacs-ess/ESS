;;; ess-xls.el --- XLispStat customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/05/21 20:07:29 $
;; Version: $Revision: 1.5 $
;; RCS: $Id: essd-xls.el,v 1.5 1997/05/21 20:07:29 rossini Exp $
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
;;: Revision 1.5  1997/05/21 20:07:29  rossini
;;: conversion to ess complete
;;:
;;: Revision 1.4  1997/05/21 18:46:48  rossini
;;: S -> ess.
;;:
;;: Revision 1.3  1997/05/20 15:01:37  rossini
;;: changed S to ess (emacs speaks statistics).
;;:
;;: Revision 1.2  1997/05/20 14:44:06  rossini
;;: stuff.
;;:
;;: Revision 1.30  1997/04/23 03:11:45  rossini
;;: local buffer mess sorted out.
;;:
;;: Revision 1.29  1997/04/23 01:04:32  rossini
;;: "cat" -> inferior-S-pager (in R()).
;;:
;;: Revision 1.28  1997/04/23 00:52:31  rossini
;;: setq-default -> setq
;;:
;;: Revision 1.27  1997/04/23 00:28:12  rossini
;;: most things are set.
;;:
;;: Revision 1.26  1997/04/22 02:07:00  rossini
;;: fixed XLS help.
;;:
;;: Revision 1.25  1997/04/22 02:00:12  rossini
;;: *** empty log message ***
;;:
;;: Revision 1.24  1997/04/20 16:31:08  rossini
;;: change S-trans autoload to point at S-trans.
;;:
;;: Revision 1.23  1997/04/18 21:50:03  rossini
;;: rearranged.
;;:
;;: Revision 1.22  1997/04/14 11:48:15  rossini
;;: added examples for S-lisp-directory.
;;:
;;: Revision 1.21  1997/04/14 00:33:23  rossini
;;: moved (require 'S) to AFTER setting path.  Needed if not in defaults.
;;:
;;: Revision 1.20  1997/04/09 02:11:15  rossini
;;: changed calling vars.
;;:
;;: Revision 1.19  1997/04/08 01:06:19  rossini
;;: removed (req 'font-lock).
;;:
;;: Revision 1.18  1997/04/07 18:54:54  rossini
;;: moved S,R,XLS here.  Edited comments.
;;:
;;: Revision 1.17  1997/04/07 12:43:14  rossini
;;: redid autoloads.
;;:
;;: Revision 1.16  1997/04/07 12:04:47  rossini
;;: added R-mode autoload.  changed docs to suggest a "require" rather
;;: than "load", if possible.
;;:
;;: Revision 1.15  1997/04/07 11:51:42  rossini
;;: autoload doc strings fixed.
;;:
;;: Revision 1.14  1997/04/07 01:20:14  rossini
;;: added XLS autoload.
;;:
;;: Revision 1.13  1997/04/04 17:20:34  rossini
;;: added R as an autoload.
;;:
;;: Revision 1.12  1997/03/10 15:08:14  rossini
;;: removed all traces of S-xmacs.
;;: added example of sys-dep S calls.
;;:
;;: Revision 1.11  1997/03/10 14:46:20  rossini
;;: inlining the log file, for others use...
;;:
;;;

;;; Code:

;;*;; Requires : can't require, until path is set.
(require 'ess-site)


;;; (1.3) Autoloads.  You should not need to change this bit.

;; Do we need the lower-case versions, or should we just move the
;; (fset) commands here?

(autoload 'ess-S-mode "ess-mode" "Major mode for editing S source code" t)
(autoload 'ess-R-mode "ess-mode" "Major mode for editing R source code" t)

(autoload 'ess-s-transcript-mode "ess-trans" "ESS source eval mode" t)
(autoload 'ess-S-transcript-mode "ess-trans" "ESS source eval mode" t)

(autoload 'inferior-ess "ess-inf"
  "Run [inferior-ess-program], an ess process, in an Emacs buffer" t)

;; Short cut: Splus

(defun ess-S-shortcut-pre-run-hook ()
  "Initialize variables for S."
  (setq ess-proc-prefix              "S"
	ess-version-running          "S+3"
	inferior-ess-program         inferior-S-program-name
	ess-help-sec-regex           ess-help-S-sec-regex
	ess-help-sec-keys-alist      ess-help-S-sec-keys-alist
	inferior-ess-objects-command "objects(%d)"
	                             ;;(if (string= ess-version-running "S3")
				     ;;    "objects(%d)"
				     ;;  "ls()")
	inferior-ess-help-command    "help(\"%s\",pager=\"cat\",window=F)\n"
	                             ;;(if S-plus
				     ;; "help(\"%s\",pager=\"cat\",window=F)\n"
				     ;; "help(\"%s\")\n")
	inferior-ess-exit-command    "q()\n"))


(defun ess-S-shortcut-post-run-hook ()
  "Remove initialization."
  (remove-hook 'ess-pre-run-hook 'ess-S-shortcut-pre-run-hook))

(defun S () "Call 'S', even after calling R."
  (interactive)
  (add-hook 'ess-pre-run-hook  'ess-S-shortcut-pre-run-hook)
  (add-hook 'ess-post-run-hook 'ess-S-shortcut-post-run-hook)
  (setq ess-proc-prefix "S")
  (inferior-ess))


 ; Provide package

(provide 'ess-xls)

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
