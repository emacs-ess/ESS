;;; ess-menu.el --- Menu and Speedbar support for statistical
;;; programming and analysis

;; Copyright 2000 (C) Rossini, Heiberger, Hornik, Maechler and
;;                          Sparapani.

;; Author:  A.J. Rossini <rossini@biostat.washington.edu>
;; Maintainer(s): A.J. Rossini <rossini@biostat.washington.edu>
;; Created: September 4, 2000
;; Version: $Id: ess-menu.el,v 1.3 2000/10/30 13:31:16 rossini Exp $
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

(require 'ess-site)

 ;;; Function Menu (func-menu) for XEmacs:

;;(defvar fume-function-name-regexp-smode
;;  " "
;;  "Expression to get function names")
;;
;;(append
;; '((s-mode  . fume-function-name-regexp-smode)
;;   (r-mode  . fume-function-name-regexp-smode))
;; fume-function-name-regexp-alist)

 ;;; Imenu for Emacs...

(require 'imenu)

(defun ess-imenu-S (&optional arg)
  "S Language Imenu support for ESS.  
Initial version from Stephen Eglen <stephen@cogsci.ed.ac.uk>."
  (interactive)
  (setq imenu-generic-expression 
	'( (nil "^\\(.+\\)\\s-+<-\\s-+function" 1)))
  (imenu-add-to-menubar "Imenu-R"))

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
	'( (nil "New one needed" 1)))
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

;;; ess.el ends here
