;;; essd-r.el --- R customization

;; Copyright (C) 1997--2001 A. J. Rossini, Richard M. Heiberger, Kurt
;; Hornik, Martin Maechler, and Rodney Sparapani.


;; Author: A.J. Rossini <rossini@u.washington.edu>
;; Maintainers: A.J. Rossini <rossini@u.washington.edu>
;;              M. Maechler <maechler@stat.math.ethz.ch>
;; Created: 12 Jun 1997
;; Modified: $Date: 2001/06/21 22:34:14 $
;; Version: $Revision: 5.29 $
;; RCS: $Id: essd-r.el,v 5.29 2001/06/21 22:34:14 rossini Exp $
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

(ess-message "[essd-r:] (require 'essl-s)")
(require 'essl-s)

(ess-message "[essd-r:] (autoload ..) & (def** ..)")

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

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
    (ess-imenu-mode-function       . 'ess-imenu-R)
    (inferior-ess-program          . inferior-R-program-name)
    (inferior-ess-objects-command  . "objects(pos = %d)\n")
    (inferior-ess-search-list-command   . "search()\n")
    (inferior-ess-help-command     . "help(\"%s\", htmlhelp=FALSE)\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-primary-prompt   . "[A-Za-z0-9.]*> ")
    (inferior-ess-secondary-prompt . "+ ?")
    (comint-use-prompt-regexp-instead-of-fields . t) ;; emacs 21 and up
    (inferior-ess-start-file       . nil)            ;; "~/.ess-R"
    (inferior-ess-start-args       . ""))
  "Variables to customize for R")

;;; AJR: Need to condition on this...! 
(require 'ess-menu)

(defun R-mode  (&optional proc-name)
  "Major mode for editing R source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist R-customize-alist)
  ;;(setq imenu-generic-expression R-imenu-generic-expression)
  (ess-mode R-customize-alist proc-name)
  ;;; AJR: Need to condition on this...!
  (ess-imenu-S))

(fset 'r-mode 'R-mode)

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
	  (if (or (equal window-system 'w32)
		  (equal window-system 'win32)
		  (equal window-system 'mswindows))
	      "--ess "
	    "--no-readline "))
	 (r-start-args
	  (concat r-always-arg
		  (if start-args
		      (read-string
		       (concat "Starting Args [other than `"
			       r-always-arg
			       "'] ? "))
		    nil)))
	 default-process-coding-system)
    (if (or (equal window-system 'w32) (equal window-system 'win32))
	(setq default-process-coding-system '(undecided-dos . undecided-dos)))
    (inferior-ess r-start-args)));; (R)


(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files." t)

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
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\|_\\) *\\)T\\>" "\\1TRUE"
		    'fixcase nil (not quietly))
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\|_\\\) *\\)F\\>" "\\1FALSE"
		    'fixcase nil (not quietly))))

;;; R package wizard tools.  See ``R-extensions'' manual for more details as to
;;; the construction of an R package.

(defun R-package-wizard (&optional packages-directory)
  "Create an R project skeleton.
Top-level directory is one below `packages-directory', i.e. package
contents will be placed in packages-directory/package-name."
  (interactive "P")
  (let* ((R-pkg-directory (if packages-directory
				  (read-string
				   (concat "Starting Directory (where you keep packages: ?"))))
	 (R-pkg-name        (read-string (concat "Package Name: ")))
	 (R-pkg-home-dir        (concat R-pkg-directory R-pkg-name))
	 (R-pkg-R-srcdir        (concat R-pkg-home-dir "/R"))
	 (R-pkg-compiled-srcdir (concat R-pkg-home-dir "/src"))
	 (R-pkg-man-srcdir      (concat R-pkg-home-dir "/man"))
	 (R-pkg-test-srcdir     (concat R-pkg-home-dir "/tests"))
	 (R-pkg-exec-srcdir     (concat R-pkg-home-dir "/exec"))
	 (R-pkg-Description-file  (concat R-pkg-home-dir "/Description"))
	 (R-pkg-Index-file        (concat R-pkg-home-dir "/INDEX")))
    ;; Now create and construct everything
    (make-directory R-pkg-home-dir)
    (make-directory R-pkg-R-srcdir)       
    (make-directory R-pkg-compiled-srcdir)
    (make-directory R-pkg-man-srcdir)
    (make-directory R-pkg-test-srcdir)
    (make-directory R-pkg-exec-srcdir)
    (R-create-description-file R-pkg-name R-pkg-Description-file)
    (R-create-index-file R-pkg-home-dir R-pkg-Index-file)))

(defun R-create-description-file (R-pkg-name R-pkg-Description-file)
  "Create a proper description file."
  )

(defun R-create-index-file (R-pkg-home-dir R-pkg-Index-file)
  "Create a proper description file.
This should use R CMD to rebuild the index."
  )

 ; provides

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
