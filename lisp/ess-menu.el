;;; ess-menu.el --- Menu and Speedbar support for statistical
;;; programming and analysis

;; Copyright 2000--2001 (C) A.J. Rossini, Richard M. Heiberger,
;; Kurt Hornik, Martin Maechler and Rodney Sparapani.

;; Author:  A.J. Rossini <rossini@u.washington.edu>
;; Maintainer(s): A.J. Rossini <rossini@u.washington.edu>
;; Created: September 4, 2000
;; Version: $Id: ess-menu.el,v 1.13 2001/08/31 16:33:26 maechler Exp $
;; Keywords: statistical support
;; Summary: general functions for ESS

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:

;;; Code:

;;*;; Requires and autoloads
;;;=====================================================

;;(require 'ess-site)
(require 'ess-cust)
(if (not (require 'imenu "imenu.elc" ; how can I have 'no-error without f.name?
		  'no-error))
    (message "** warning: 'imenu not available")
)
;;possibly below, after checking: (require 'speedbar)

 ;;; Function Menu (func-menu) for XEmacs:

;;(defvar fume-function-name-regexp-smode
;;  " "
;;  "Expression to get function names")
;;
;;(append
;; '((s-mode  . fume-function-name-regexp-smode)
;;   (r-mode  . fume-function-name-regexp-smode))
;; fume-function-name-regexp-alist)

 ;;; Imenu for Emacs/XEmacs...

;;; S imenu support

(defcustom ess-use-imenu t
  "Use imenu if exists."
  :group 'ess
  :type  'boolean)

(defcustom ess-S-use-imenu ess-use-imenu
  "Use imenu if exists."
  :group 'ess
  :type  'boolean)

(defcustom ess-S-imenu-variable-regexp
  (concat "\\("
	  "[a-zA-Z0-9\\.]*"  ;; legal chars in name
	  "\\)"
	  "[ \t]*<-[ \t]*"   ;; whitespace, assignment, whitespace
	  "function")        ;; NOT this.
  "Regexp for R assignments."
  :group 'ess
  :type  'regex)

(defcustom ess-S-imenu-function-regexp
  "^\\(.+\\)\\s-+<-\\s-+function"
  ;; (concat "\\("
  ;;  	     "[a-zA-Z0-9\\.]*"  ;; legal chars in name
  ;;	     "\\)"
  ;;	     "[ \t]*<-[ \t]*"   ;; whitespace, assignment, whitespace
  ;;	     "function"         ;; it's a fcn...
  ;;	     "[ \t]*"           ;; whitespace
  ;;	     "(")               ;; start of arguments
  "Regexp for R functions.
Thanks to Stephen Eglen <stephen@cogsci.ed.ac.uk> for first version."
  :group 'ess
  :type  'regex)

(defcustom ess-S-imenu-generic-expression
  (concat ess-S-imenu-variable-regexp
	  ;; "\\|"
	  ;; ess-S-imenu-function-regexp
	  )
  "Imenu Regexp for S."
  :group 'ess
  :type  'regex)

(setq R-imenu-generic-expression 'ess-S-imenu-generic-expression)
(setq S-imenu-generic-expression 'ess-S-imenu-generic-expression)

(defun ess-imenu-S (&optional arg)
  "S Language Imenu support for ESS.
Initial version from Stephen Eglen <stephen@cogsci.ed.ac.uk>."
  (interactive)
  (setq imenu-generic-expression
	'( (nil ;;ess-S-imenu-generic-expression
	        "^\\(.+\\)\\s-+<-\\s-+function"
		1)))
  (imenu-add-to-menubar "Imenu-S"))

(fset 'ess-imenu-R 'ess-imenu-S)

;;; XLS imenu support

(defun ess-imenu-XLS (&optional arg)
  "XLispStat Language Imenu support for ESS."
  (interactive)
  (setq imenu-generic-expression
	'( (nil "New one needed" 1)))
  (imenu-add-to-menubar "XLS-fcts"))

(defun imenu-example--XLS-extract-index-name ()
  ;; Example of a candidate for `imenu-extract-index-name-function'.
  ;; This will generate a flat index of definitions in a lisp file.
  (save-match-data
    (and (looking-at "(def")
	 (condition-case nil
	     (progn
	       (down-list 1)
	       (forward-sexp 2)
	       (let ((beg (point))
		     (end (progn (forward-sexp -1) (point))))
		 (buffer-substring beg end)))
	   (error nil)))))

(defun imenu-example--create-XLS-index ()
  ;; Example of a candidate for `imenu-create-index-function'.
  ;; It will generate a nested index of definitions.
  (let ((index-alist '())
	(index-var-alist '())
	(index-type-alist '())
	(index-unknown-alist '())
	prev-pos)
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0)
    ;; Search for the function
    (while (beginning-of-defun)
      (imenu-progress-message prev-pos nil t)
      (save-match-data
	(and (looking-at "(def")
	     (save-excursion
	       (down-list 1)
	       (cond
		((looking-at "def\\(var\\|const\\)")
		 (forward-sexp 2)
		 (push (imenu-example--name-and-position)
		       index-var-alist))
		((looking-at "def\\(un\\|subst\\|macro\\|advice\\)")
		 (forward-sexp 2)
		 (push (imenu-example--name-and-position)
		       index-alist))
		((looking-at "def\\(type\\|struct\\|class\\|ine-condition\\)")
 		 (forward-sexp 2)
 		 (if (= (char-after (1- (point))) ?\))
 		     (progn
 		       (forward-sexp -1)
 		       (down-list 1)
 		       (forward-sexp 1)))
 		 (push (imenu-example--name-and-position)
 		       index-type-alist))
		(t
		 (forward-sexp 2)
		 (push (imenu-example--name-and-position)
		       index-unknown-alist)))))))
    (imenu-progress-message prev-pos 100)
    (and index-var-alist
	 (push (cons "Variables" index-var-alist)
	       index-alist))
    (and index-type-alist
 	 (push (cons "Types" index-type-alist)
  	       index-alist))
    (and index-unknown-alist
	 (push (cons "Syntax-unknown" index-unknown-alist)
	       index-alist))
    index-alist))

(defun ess-imenu-STA (&optional arg)
  "Stata Language Imenu support for ESS."
  (interactive)
  (setq imenu-generic-expression
	'( (nil "New one needed" 1)))
  (imenu-add-to-menubar "Stata-fcts"))

(defun ess-imenu-SAS (&optional arg)
  "SAS language Imenu support for ESS."
  (interactive)
  (setq imenu-generic-expression
	'( (nil "[ \t\n=]\\([a-zA-Z_][a-zA-Z_0-9]*[.][a-zA-Z_][a-zA-Z_0-9]*\\)[ \t\n;]" 1)))
  (imenu-add-to-menubar "SAS-fcts"))


;;;; Example for C menus:
;;; Regular expression to find C functions
;(defvar imenu-example--function-name-regexp-c
;  (concat
;   "^[a-zA-Z0-9]+[ \t]?"		; type specs; there can be no
;   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"	; more than 3 tokens, right?
;   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"
;   "\\([*&]+[ \t]*\\)?"			; pointer
;   "\\([a-zA-Z0-9_*]+\\)[ \t]*("	; name
;   ))
;(defun imenu-example--create-c-index (&optional regexp)
;  (let ((index-alist '())
;	prev-pos char)
;    (goto-char (point-min))
;    (imenu-progress-message prev-pos 0)
;    ;; Search for the function
;    (save-match-data
;      (while (re-search-forward
;	      (or regexp imenu-example--function-name-regexp-c)
;	      nil t)
;	(imenu-progress-message prev-pos)
;	(backward-up-list 1)
;	(save-excursion
;	  (goto-char (scan-sexps (point) 1))
;	  (setq char (following-char)))
;	;; Skip this function name if it is a prototype declaration.
;	(if (not (eq char ?\;))
;	    (push (imenu-example--name-and-position) index-alist))))
;    (imenu-progress-message prev-pos 100)
;    (nreverse index-alist)))


 ;;; Speedbar stuff.


(defun S-speedbar-buttons (buffer)
  "attempted hack."

  (speedbar-with-writable)
  ;;(speedbar-make-tag-line)
  ;;(speedbar-insert-button)
  )

(fset 'R-speedbar-buttons 'S-speedbar-buttons)

(defun S-speedbar-menu-items  ( )
  "Attempted hack.")

(defun ess-S-initialize-speedbar ()
  "Extend to all extensions; see initialization, and edit."
  (speedbar-add-supported-extension ".R")
  (speedbar-add-supported-extension ".S")
  (speedbar-add-supported-extension ".s")
  (speedbar-add-supported-extension ".q"))

(if (featurep 'speedbar)
    (progn
      (message "enabling speedbar support")
      (require 'speedbar)
      (ess-S-initialize-speedbar)))

 ; Run load hook and provide package

(provide 'ess-menu)

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

;;; ess-menu.el ends here
