;;; ess-emcs.el --- simple determination of Emacs/XEmacs and version #.

;; Copyright (C) 2000--2001 A.J. Rossini <rossini@u.washington.edu>,
;; R.M. Heiberger <rmh@surfer.sbm.temple.edu>,
;; Martin Maechler <maechler@stat.math.ethz.ch>,
;; Kurt Hornik <hornik@ci.tuwien.ac.at>, and
;; Rodney Sparapani <rsparapa@mcw.edu>.

;; Author:  A.J. Rossini <rossini@biostat.washington.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 07 June 2000
;; Modified: $Date: 2001/08/10 13:46:07 $
;; Version: $Revision: 5.12 $
;; RCS: $Id: ess-emcs.el,v 5.12 2001/08/10 13:46:07 maechler Exp $
;;
;; Keywords: start up, configuration.

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file contains functions for easily determining features of the
;; version of Emacs that we are using.   In particular, it look for
;; version number, customize support, as well as Emacs/XEmacs, for
;; flaggin support later on.

;;; Code:

;; Older versions of emacs did not have these variables
;; (emacs-major-version and emacs-minor-version.)
;; Let's define them if they're not around, since they make
;; it much easier to conditionalize on the emacs version.

(if (and (not (boundp 'emacs-major-version))
	 (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
	  (string-to-int (substring emacs-version
				    (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
	 (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
	  (string-to-int (substring emacs-version
				    (match-beginning 1) (match-end 1)))))

;;; Define a function to make it easier to check which version we're
;;; running.

(defun ess-running-emacs-version-or-newer (major minor)
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
	   (>= emacs-minor-version minor))))

(defvar ess-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defvar ess-local-custom-available (featurep 'custom)
  "Value is nil if custom.el not available, t if available.
Only a concern with earlier versions of Emacs.")

(defvar ess-microsoft-p (or (equal window-system 'w32)
			    ;; XEmacs only...
;;;			    (equal (console-type) 'pc)
;;;			    (equal (console-type) 'mswindows)
			    (equal window-system 'win32)
			    (equal window-system 'mswindows))
  "Value is t if the OS is one of Microsoft's, nil otherwise.")


;; These definitions are for Emacs versions < 20.4 or XEmacs
;; These are taken verbatim from the file emacs-20.6/lisp/w32-fns.el
;;
;; Note: 20.3 and 19.x NTemacs users are strongly encouraged to upgrade to
;; version 20.4 or higher.  NTemacs 20.2 is not supported by ESS.

;; XEmacs 20.x needs this

(if (not (fboundp 'find-buffer-visiting))
    (fset 'find-buffer-visiting 'get-file-buffer))

;; XEmacs and NTemacs 19.x need these
(if (not (boundp 'w32-system-shells))
      (defvar w32-system-shells '("cmd" "cmd.exe" "command" "command.com"
				  "4nt" "4nt.exe" "4dos" "4dos.exe"
				  "ndos" "ndos.exe")
	"List of strings recognized as Windows NT/9X system shells.")
)

(if (not (fboundp 'w32-system-shell-p))
      (defun w32-system-shell-p (shell-name)
	(and shell-name
	     (member (downcase (file-name-nondirectory shell-name))
		     w32-system-shells)))
)

(if (not (fboundp 'w32-shell-name))
      (defun w32-shell-name ()
	"Return the name of the shell being used."
	(or (and (boundp 'explicit-shell-file-name) explicit-shell-file-name)
	    (getenv "ESHELL")
	    (getenv "SHELL")
	    (and (w32-using-nt) "cmd.exe")
	    "command.com"))
)

;; XEmacs and NTemacs 20.3 need this
(if (not (fboundp 'w32-shell-dos-semantics)) (defun w32-shell-dos-semantics ()
  "Return t if the interactive shell being used expects msdos shell semantics."
  (or (w32-system-shell-p (w32-shell-name))
      (and (member (downcase (file-name-nondirectory (w32-shell-name)))
		   '("cmdproxy" "cmdproxy.exe"))
	   (w32-system-shell-p (getenv "COMSPEC")))))
)

(provide 'ess-emcs)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:	  ^L ;
;;; Sections:	 ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:	 defuns, defvars, defconsts
;;;		 Random code beginning with a ;;;;* comment
;;; Local variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-emcs.el ends here
