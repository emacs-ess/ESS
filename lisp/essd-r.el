;;; essd-r.el --- R customization

;; Copyright (C) 1997--1999 A. J. Rossini, Richard M. Heiberger, Kurt
;; Hornik, and Martin Maechler.


;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1999/09/01 19:19:34 $
;; Version: $Revision: 5.16 $
;; RCS: $Id: essd-r.el,v 5.16 1999/09/01 19:19:34 maechler Exp $
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
;;; This file defines all the R customizations for ESS.  See essl-s.el
;;; for general S language customizations.

;;; Autoloads and Requires

(require 'essl-s)

(autoload 'inferior-ess "ess-inf" "Run an ESS process")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process")

;;; Code:

(defvar R-customize-alist
  '((ess-local-customize-alist     . 'R-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . "R")
    (ess-suffix                    . "R")
    (ess-loop-timeout              . 500000 )
    (ess-dump-filename-template    . (concat (user-login-name)
					     ".%s."
					     ess-suffix))
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-syntax-table         . S-syntax-table)
    (ess-help-sec-regex            . ess-help-R-sec-regex)
    (ess-help-sec-keys-alist       . R-help-sec-keys-alist)
    (ess-object-name-db-file       . "ess-r-namedb.el" )
    (ess-retr-lastvalue-command
     . ".Last.value <- get(\".ess.lvsave\",inherits=T)\n") ; envir=1
    (ess-save-lastvalue-command
     . "assign(\".ess.lvsave\",.Last.value,inherits=T)\n") ;envir=1
    (inferior-ess-program          . inferior-R-program-name)
    (inferior-ess-objects-command  . "objects(pos = %d)\n")
    (inferior-ess-help-command     . "help(\"%s\")\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-primary-prompt   . "[A-Za-z0-9.]*> ")
    (inferior-ess-secondary-prompt . "+ ?")
    (inferior-ess-start-file       . nil) ; "~/.ess-R")
    (inferior-ess-start-args       . ""))
  "Variables to customize for R")


(defun R-mode  (&optional proc-name)
  "Major mode for editing R source.  See ess-mode for more help."
  (interactive)
  (setq ess-customize-alist R-customize-alist)
  (ess-mode R-customize-alist proc-name))

(fset 'r-mode 'R-mode)

;;
;; R for unix-only systems.  This should go away.
;;
;;(defun R-original (&optional start-args)
;;  "Call 'R', the GNU 'S clone' from Robert & Ross (Auckland, NZ)."
;;  (interactive "P")
;;  (setq ess-customize-alist R-customize-alist)
;;  ;; for debugging only
;;  (ess-write-to-dribble-buffer
;;   (format
;;    "\n(R): ess-dialect=%s, buf=%s, start-arg=%s\n\t current-prefix-arg=%s\n"
;;    ess-dialect (current-buffer) start-args
;;    current-prefix-arg))
;;  (let ((r-start-args  (concat "--no-readline "
;;	 (if start-args (read-string
;;			 "Starting Args [other than `--no-readline'] ? ")
;;	   nil))))
;;    (inferior-ess r-start-args)))

;; R that does the right thing irregardless of OS.
(defun R (&optional start-args)
  "Call 'R', the GNU 'S clone' from Robert & Ross (Auckland, NZ).
Optional prefix (C-u) allows to set command line arguments, such as --vsize."
  (interactive "P")
  (setq ess-customize-alist R-customize-alist)
  ;; for debugging only
  (ess-write-to-dribble-buffer
   (format
    "\n(R): ess-dialect=%s, buf=%s, start-arg=%s\n current-prefix-arg=%s\n"
    ess-dialect (current-buffer) start-args current-prefix-arg))
  (let* ((r-always-arg
	  (if (or (equal window-system 'w32) (equal window-system 'win32))
	      "--ess "
	    "--no-readline "))
	 (r-start-args
	  (concat r-always-arg
		  (if start-args
		      (read-string
		       (concat "Starting Args [other than `"
			       r-always-arg
			       "'] ? "))
		    nil))))
    (inferior-ess r-start-args))
  (if (or (equal window-system 'w32) (equal window-system 'win32))
      (progn
	(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
	(comint-strip-ctrl-m)           ; Timing problem in bash.
					; Can't make startup ^M go away.
	(goto-char (point-max))
	(beginning-of-line)
	(insert
"The interaction of ESS 5.1.x and R 0.63.3 pre-Beta is rough:\n
To start the graphics window, you must explicitly use the `x11()' command.\n
You must quit R with `q()' or you take the risk of not being able
to shut down the computer cleanly.\n\n")
	(goto-char (point-max)))))





(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files" t)

(defun r-transcript-mode ()
  "Does the right thing."
  (interactive)
  (ess-transcript-mode R-customize-alist))

(fset 'R-transcript-mode 'r-transcript-mode)

(defun R-fix-T-F (&optional from quietly)
  "Fix T/F into TRUE and FALSE --- CAUTIOUSLY"
  (interactive "d\nP"); point and prefix (C-u)
  (save-excursion
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\) *\\)T\\>" "\\1TRUE"
		    'fixcase nil (not quietly))
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\) *\\)F\\>" "\\1FALSE"
		    'fixcase nil (not quietly))
    ))

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
