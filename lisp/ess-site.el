;;; ess-site.el --- user customization of ess-mode

;; Copyright (C) 1993 David M. Smith

;; Author: David Smith <D.M.Smith@lancaster.ac.uk>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Nov 1993
;; Modified: $Date: 1997/06/15 09:43:38 $
;; Version: $Revision: 1.9 $
;; RCS: $Id: ess-site.el,v 1.9 1997/06/15 09:43:38 rossini Exp $
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
;;: Revision 1.9  1997/06/15 09:43:38  rossini
;;: FILES, NOT FILE...
;;:
;;: Revision 1.8  1997/06/15 09:23:36  rossini
;;: fixed docs, (assemb vs S mode) ala RH.
;;:
;;: Revision 1.7  1997/06/15 09:09:30  rossini
;;: added ess-keep-dump-files example. (thanks RH)
;;:
;;: Revision 1.6  1997/06/14 23:17:09  rossini
;;: moved language stuff into essd-* files.
;;:
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

(provide 'ess-site)

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
;;; want to use assembler, comment the appropriate
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

;; Require the needed dialects for your setup.

(require 'essd-s+3)
(require 'essd-r)
(require 'essd-xls)


;;TODO:
;;  ViSta (essd-vst)
;;  S4 (essd-s4)
;;  S3 (essd-s3)
;;  S+4 (essd-s+4)
;;  SAS (essd-sas)


;;; 2. Site Specific setup
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


;;; 3. Customization (and commented out examples) for your site
;;;; ===============================================


;;; (3.1) Font-lock
;; The following two expressions automatically enable font-lock-mode
;; for S-mode and inferior-S-mode buffers.

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

;;; (3.3) S-keep-dump-files.
;;; Documentation:
;;; *Variable controlling whether to delete dump files after a successful load.
;;; If nil: always delete.  If `ask', confirm to delete.  If `check', confirm
;;; to delete, except for files created with S-dump-object-into-edit-buffer.
;;; Anything else, never delete.  This variable only affects the behaviour
;;; of S-load-file.  Dump files are never deleted if an error occurs
;;; during the load. 

;;; RH sez: which I found imperative.  The default was to throw away
;;; files at the wrong time (I think it was something like, if you M-x
;;; S-load a file twice, while you are working on it, the file is
;;; deleted).  I believe source is real and the S object is temporary.
;;; The default behavior is for people who believe the object is real
;;; and the source file temporary.

(setq ess-keep-dump-files "always")

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

;;; ess-site.el ends here
