;;; essd-r.el --- R customization

;; Copyright (C) 1997--2004 A. J. Rossini, Richard M. Heiberger, Kurt
;; Hornik, Martin Maechler, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@u.washington.edu>
;; Maintainers: A.J. Rossini <rossini@u.washington.edu>
;;              M. Maechler <maechler@stat.math.ethz.ch>,
;;              Stephen Eglen < >
;; Created: 12 Jun 1997
;; Modified: $Date: 2004/07/02 14:25:13 $
;; Version: $Revision: 5.57 $
;; RCS: $Id: essd-r.el,v 5.57 2004/07/02 14:25:13 rmh Exp $
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

;; modify S Syntax table:
(setq R-syntax-table S-syntax-table)
;; R >= 1.8: back tick `string` -- unfortunately no *pair* checking:
;; breaks when things like `..' are used:
;; (modify-syntax-entry ?` "\"" R-syntax-table)
(modify-syntax-entry ?_  "_"  R-syntax-table) ; foo_bar is symbol in R >=1.9

(ess-message "[essd-r:] (autoload ..) & (def** ..)")

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

;;; Code:

(defvar R-customize-alist
  '((ess-local-customize-alist     . 'R-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . "R")
    (ess-suffix                    . "R")
    (ess-dump-filename-template    . (replace-regexp-in-string
				      "S$" ess-suffix ; in the one from custom:
				      ess-dump-filename-template-proto))
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-syntax-table         . R-syntax-table)
    (ess-help-sec-regex            . ess-help-R-sec-regex)
    (ess-help-sec-keys-alist       . R-help-sec-keys-alist)
    (ess-loop-timeout              . ess-S-loop-timeout)
    (ess-object-name-db-file       . "ess-r-namedb.el" )
    (ess-retr-lastvalue-command
     . "assign(\".Last.value\", .ess.lvsave, envir=NULL)\n") ; package:base
;;    (ess-retr-lastvalue-command
;;     . ".Last.value <- get(\".ess.lvsave\",inherits=TRUE)\n") ; envir=1
    (ess-save-lastvalue-command
     . "assign(\".ess.lvsave\",.Last.value,inherits=TRUE)\n") ;envir=1
    (ess-imenu-mode-function       . 'ess-imenu-R)
    (inferior-ess-program          . inferior-R-program-name)
    (inferior-ess-objects-command  . "objects(pos = %d)\n")
    (inferior-ess-search-list-command   . "search()\n")
    (inferior-ess-help-command     . "help(\"%s\", htmlhelp=FALSE)\n")
    (inferior-ess-exit-command     . "q()")
    (inferior-ess-exit-prompt      . "Save workspace image? [y/n/c]: ")
    (inferior-ess-primary-prompt   . "\\([A-Z][][A-Za-z0-9.]*\\)*> ")
    (inferior-ess-secondary-prompt . "+ ?")
    ;;harmful for shell-mode's C-a: -- but "necessary" for ESS-help?
    (comint-use-prompt-regexp-instead-of-fields . t) ;; emacs 21 and up
    (inferior-ess-start-file       . nil)            ;; "~/.ess-R"
    (inferior-ess-start-args       . "")
    (ess-STERM  . "iESS")
    (ess-editor . R-editor)
    (ess-pager  . R-pager)
    (inferior-ess-language-start . (eval inferior-S-language-start))
    )
  "Variables to customize for R")

;;; AJR: Need to condition on this...!
(require 'ess-menu)

;;;### autoload
(defun R (&optional start-args)
  "Call 'R', the GNU 'S clone' from Robert & Ross (Auckland, NZ).
Optional prefix (C-u) allows to set command line arguments, such as
--vsize.  This should be OS agnostic."
  (interactive "P")
  (setq ess-customize-alist R-customize-alist)
  (ess-write-to-dribble-buffer   ;; for debugging only
   (format
    "\n(R): ess-dialect=%s, buf=%s, start-arg=%s\n current-prefix-arg=%s\n"
    ess-dialect (current-buffer) start-args current-prefix-arg))
  (let* ((r-always-arg
	  (if ess-microsoft-p
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
    (if ess-microsoft-p
	(setq default-process-coding-system '(undecided-dos . undecided-dos)))
    (inferior-ess r-start-args) ;; (R)
    (ess-write-to-dribble-buffer 
     (format "(R): inferior-ess-language-start=%s\n"
	     inferior-ess-language-start))
    (if inferior-ess-language-start
	(ess-eval-linewise inferior-ess-language-start))))

;;;### autoload
(defun R-mode  (&optional proc-name)
  "Major mode for editing R source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist R-customize-alist)
  ;;(setq imenu-generic-expression R-imenu-generic-expression)
  (ess-mode R-customize-alist proc-name)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  ;; ECB needs seminatic stuff.
  ;;  (if (featurep 'semantic)
  ;;      (setq semantic-toplevel-bovine-table r-toplevel-bovine-table))
  ;; AJR: Need to condition on this...!
  ;; MM: and you probably should really use ess-imenu-mode-function from the
  ;;     alist above!
  (if ess-imenu-use-S (ess-imenu-R)))

(fset 'r-mode 'R-mode)

(defvar ess-r-versions-created nil
  "List of strings of the new defuns created by `ess-r-versions-create'.
This is used by the easymenu code to add the new defuns to the menubar
under ESS -> Start Process -> Other.")

(defun ess-r-versions-create ()
  "Generate the `M-x R-X.Y' functions for starting other versions of R.
See `ess-r-versions' for strings that determine which functions are created.

The local variable `ess-r-versions-created' is used to return
list of the new R defuns, if any, that were created.  The result will
normally be bound to the global variable `ess-r-versions-created'.
The defuns will normally be placed on the menubar upon ESS initialisation."

  ;; This works by creating a temp buffer where the template function is
  ;; edited so that X.Y is replaced by the version name
  (let ((template "")
	(beg)
	(versions)
	(version)
	(eval-buf (get-buffer-create "*ess-temp-r-evals*"))
	(ess-r-versions-created)
	)
    ;; 
    ;; This is the template function used for creating M-x R-X.Y.
    (setq template "(defun R-X.Y (&optional start-args)
  \"Call R-X.Y, i.e., the R version 'R-X.Y' using ESS.
This function was generated by `ess-r-versions-create'.\"
  (interactive \"P\")
  (let ((inferior-R-program-name \"R-X.Y\"))
    (R start-args)))

")
    (save-excursion
      (set-buffer eval-buf)
      ;; clear the buffer.
      (delete-region (point-min) (point-max))

      ;; Find which versions of R we want.  Remove the pathname, leaving just
      ;; the name of the executable.
      (setq versions
	    (mapcar 'file-name-nondirectory
		    (ess-uniq-list
		     (apply 'nconc 
			    (mapcar 'ess-find-exec-completions 
				    ess-r-versions)))))
      (ess-write-to-dribble-buffer
       (format "(R): ess-r-versions-create making M-x defuns for %s"
	       (mapconcat 'identity versions " ")))
      (setq ess-r-versions-created versions) ;keep copy for returning at end.
      ;; Iterate over each string in VERSIONS, creating a new defun each time.
      (while versions
	(setq version (car versions)
	      versions (cdr versions))
	(setq beg (point))
	(insert template)
	(goto-char beg)
	(while (search-forward "R-X.Y" nil t)
	  (replace-match version t t))
	(goto-char (point-max))
	)
      ;; buffer has now been created with defuns, so eval them!
      (eval-buffer)
      (kill-buffer eval-buf)
      )
    ess-r-versions-created))


(defun ess-find-rterm (&optional ess-R-root-dir)
  "Find the full path of all occurences of Rterm.exe under the ESS-R-ROOT-DIR.
If ESS-R-ROOT-DIR is nil, construct it by looking for an occurence of Rterm.exe
in the exec-path."
  (let* ((Rpath)
	 (rwxxyy)
	 (rw)
	 (Rterm nil))
    (if (not ess-R-root-dir)
	(progn
	  (setq Rpath (executable-find "Rterm"))
	  (setq ess-R-root-dir
		(if Rpath 
		    (expand-file-name
		     (concat
		      (file-name-directory Rpath)
		      "../../"))
		  ""))))
    (setq rwxxyy (file-name-all-completions "rw" ess-R-root-dir))
    (while rwxxyy
      (setq rw (car rwxxyy))
      (setq rwxxyy (cdr rwxxyy))
      (setq Rterm (cons (concat ess-R-root-dir rw "bin/Rterm.exe") Rterm)))
    Rterm))


(defvar ess-rterm-versions-created nil
  "List of strings of the new defuns created by `ess-rterm-versions-create'.
This is used by the easymenu code to add the new defuns to the menubar
under ESS -> Start Process -> Other.")

(defun ess-rterm-versions-create ()
  "Generate the `M-x rwxxyy' functions for starting other versions of R.
See `ess-rterm-versions' for strings that determine which functions
are created.

The result `ess-rterm-versions-created' will store a
list of the new Rterm defuns, if any, that were created.  The result will
normally be bound to the added to global variable `ess-rterm-versions-created'.
The defuns will normally be placed on the menubar upon ESS initialisation."

  ;; This works by creating a temp buffer where the template function is
  ;; edited so that R-X.Y is replaced by the version name
  (let ((template "")
	(beg)
	(versions)
	(version)
	(eval-buf (get-buffer-create "*ess-temp-r-evals*"))
	(ess-rterm-versions-created)
)
    ;; 
    ;; This is the template function used for creating M-x R-X.Y.
    (setq template "(defun R-X.Y (&optional start-args)
  \"Call R-X.Y, i.e., the R version 'R-X.Y' using ESS.
This function was generated by `ess-rterm-versions-create'.\"
  (interactive \"P\")
  (let ((inferior-R-program-name \"Rterm-X.Y\"))
    (R start-args)))

")
    (save-excursion
      (set-buffer eval-buf)
      ;; clear the buffer.
      (delete-region (point-min) (point-max))

      ;; Find which versions of R we want.  Remove the pathname, leaving just
      ;; the name of the executable.
      (setq versions ess-rterm-versions)
      (ess-write-to-dribble-buffer
       (format "(R): ess-rterm-versions-create making M-x defuns for %s"
	       (mapconcat 'identity versions " ")))

      ;; Iterate over each string in VERSIONS, creating a new defun each time.
      (while versions
	(setq version (car versions)
	      versions (cdr versions)
	      version-root (file-name-nondirectory
			    (substring (file-name-directory
					(substring
					 (file-name-directory version)
					 0 -1)) 
				       0 -1)))
	(setq beg (point))
	(insert template)
	(goto-char beg)
	(while (search-forward "R-X.Y" nil t)
	  (replace-match version-root t t))
	(goto-char beg)
	(while (search-forward "Rterm-X.Y" nil t)
	  (replace-match version t t))
	(goto-char (point-max))
	(setq ess-rterm-versions-created
	      (cons version-root ess-rterm-versions-created))
	)
      ;; buffer has now been created with defuns, so eval them!
      (eval-buffer)
      (kill-buffer eval-buf)
      )
    ess-rterm-versions-created))

;;;### autoload
(defun Rnw-mode ()
  "Major mode for editing Sweave(R) source.
See `noweb-mode' and `R-mode' for more help."
  (interactive)
  (require 'ess-noweb);; << probably someplace else
  (noweb-mode 1); turn it on
  (noweb-set-doc-mode 'latex-mode)
  (noweb-set-code-mode 'R-mode))


(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files." t)

(defun R-transcript-mode ()
  "Does the right thing."
  (interactive)
  (ess-transcript-mode R-customize-alist))

(fset 'r-transcript-mode 'R-transcript-mode)

(defun R-fix-T-F (&optional from quietly)
  "Fix T/F into TRUE and FALSE *cautiously*, i.e. not in comments and strings;
 starting from the current position (point)."
  (interactive "d\nP"); point and prefix (C-u)
  (save-excursion
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\|_\\) *\\)T\\>" "\\1TRUE"
		    'fixcase nil (not quietly))
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\|_\\\) *\\)F\\>" "\\1FALSE"
		    'fixcase nil (not quietly))))

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
