;;; ess-site.el --- user customization of ess-mode

;; Copyright (C) 1993 David M. Smith

;; Author: David Smith <D.M.Smith@lancaster.ac.uk>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Nov 1993
;; Modified: $Date: 1997/05/21 20:07:29 $
;; Version: $Revision: 1.5 $
;; RCS: $Id: ess-site.el,v 1.5 1997/05/21 20:07:29 rossini Exp $
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

;;; This file defines all the site-specific customizations for ess-mode.
;;; It should be edited on a per-site basis.  Section 1 *must* be
;;; edited, and the correct pathname for the directory which contains
;;; this file must be supplied in ess-lisp-directory.  The editing of
;;; remaining sections is optional.  It should then be byte-compiled,
;;; and users who wish to use ess-mode should add the line:
;;;    (load "/PATH/TO/THIS/FILE/ess-site")
;;; (where /PATH/TO/THIS/FILE is the path to ess-site.elc: i.e. the
;;; value of ess-lisp-directory, below) to their .emacs file.
;;;
;;; Alternatively, if the file is already in a directory specified by
;;; the load-path variable, a simple:
;;;    (require 'ess-site)
;;; will work.

;;;
;;: $Log: ess-site.el,v $
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

;;;; 1. Load path, autoloads, and major modes
;;;; ========================================
;;;
;;; (1.1) Change the value of ess-lisp-directory to the directory which
;;; is to contain the file ess-site.elc.  This is probably the current
;;; directory, or the value of LISPDIR if it was set in the Makefile.

(defvar ess-lisp-directory
  (directory-file-name "/usr/local/lib/xemacs/site-lisp/ess-mode"))
;; Or replace directly with:
;(directory-file-name "/usr/local/share/emacs/site-lisp/ess-mode"))
;; Not needed in XEmacs (if in the site-lisp directory, but might as
;; well add):
;(directory-file-name "/usr/local/lib/xemacs/site-lisp/ess-mode"))

(add-to-list 'load-path ess-lisp-directory)

;;*;; Requires : can't require, until path is set.
(require 'ess)

;;; (1.2) Files ending in .q and .S are considered to be S source files
;;; Files ending in .St are considered to be S transcript files
;;; NB: in standard Emacs, files ending in .s are assembler files.  If you
;;; want to use such files as S source files, uncomment the appropriate
;;; line below.

(if (assoc "\\.q$" auto-mode-alist) nil
  (setq auto-mode-alist
	(append
	 '(("\\.q$" . ess-S-mode)
	   ("\\.s$"  . ess-S-mode) ;; Comment for default asm-mode
	   ("\\.S$"  . ess-S-mode)
	   ("\\.R$"  . ess-R-mode)
	   ("\\.St$" . ess-S-transcript-mode))
	 auto-mode-alist)))


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

;; Shortcut: R

(defun ess-R-shortcut-pre-run-hook ()
  "Initialize variables."
  (setq ess-proc-prefix              "R"
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

;; Short cut: XLispStat

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

;;TODO:
;; Short cut: ViSta
;; Short cut: S4
;; Short cut: S3





;;; In what follows I have tried to list the main user variables you
;;; might want to modify in this file (at least those that are
;;; necessary for getting S-mode running).  It is not an exhaustive
;;; list, however; check the files S.el, S-inf.el, S-mode.el,
;;; S-trans.el and S-help.el (or the texinfo manual) for others.
;;; Variables you can change are listed with the leading comment ";;"
;;; (two semicolons) which you must delete if you wish to change the
;;; variable.  Default values are listed; if you are not sure do not
;;; change anything.


;;;; 2. Executable location and version for S/S-plus
;;;; ===============================================

;;; Set this to the name of the program you use to run S or Splus.  It
;;; can be an absolute pathname, if you wish.
;;(setq inferior-S-program "Splus")
;;(setq inferior-S-program (concat (getenv "SHOME") "/Splus"))

;;; Set this to nil if you are running vanilla (AT&T) S instead of S-plus
;;(setq S-plus t)

;;; Set this to "2.3" if you are running a pre-3.0 version of S or S-plus
;;; Otherwise do not change it (still use "3.0" for any version after 3.0)
;; (setq ess-version-running "3.0")

;;; You will need to change the following two variables if you use a
;;; non-standard S prompt.
;; (setq inferior-S-primary-prompt "[a-zA-Z0-9() ]*> ?")
;; (setq inferior-S-secondary-prompt "+ ?")


;;;; 3. Optional extras
;;;; ==================

;;; (3.1) Font-lock
;; The following two expressions automatically enable font-lock-mode
;; for S-mode and inferior-S-mode buffers.

;;(require 'font-lock) Moved to S.el

(if window-system
    (progn
      (add-hook 'S-mode-hook 'turn-on-font-lock t)
      (add-hook 'S-transcript-mode-hook 'turn-on-font-lock t)
      (add-hook 'inferior-ess-mode-hook 'turn-on-font-lock t)))

;;; (3.2) Framepop.  Windows produced by S-execute-objects etc. are
;;; often unneccessarily large. The framepop package makes such
;;; windows appear in a separate, shrink-wrapped frame. This will
;;; also affect other "temporary" windows such as those produced by
;;; C-h k, etc.  To enable, uncomment both lines of code below).
;;;
;;; Works only with Emacs at this time.

;; (cond (window-system
;;       (require 'framepop)))

 ; Provide package

(provide 'ess-site)

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
