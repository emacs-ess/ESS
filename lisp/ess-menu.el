;;; ess-menu.el --- Menu and Speedbar support for statistical
;;;                 programming and analysis

;; Copyright (C) 2000--2005 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini
;; Created: September 4, 2000
;; Maintainer: ESS Core Team <ESS-core@r-project.org>

;; Keywords: statistics, languages

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
;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/


;;; Commentary:

;;; Code:

;;*;; Requires and autoloads
;;;=====================================================

(require 'ess-custom)
(if (and (featurep 'xemacs); need this, since require in XEmacs has only 2 arg
         (not (require 'imenu "imenu.elc")))
    (message "** warning: 'imenu not available for this version of XEmacs"))

;;(defgroup ess-menu nil
;;  "ESS: object menu systems."
;;  :group 'ess
;;  :prefix "ess-")


 ;;; Function Menu (func-menu) for XEmacs:

;; (if ess-funcmenu-use-p
;;     (defvar fume-function-name-regexp-S
;;       (append
;;        '((s-mode  . fume-function-name-regexp-smode)
;;       (r-mode  . fume-function-name-regexp-smode))
;;        fume-function-name-regexp-alist)
;;       "Expression to get function names"))

 ;;; Imenu for Emacs/XEmacs...

;;; XLS imenu support

;; (defun ess-imenu-XLS (&optional arg)
;;  "XLispStat Language Imenu support for ESS."
;;  (interactive)
;;  (setq imenu-generic-expression
;;       '( (nil "New one needed" 1)))
;;  (imenu-add-to-menubar "XLS-fcts"))

;; (defun imenu-example--XLS-extract-index-name ()
;;  ;; Example of a candidate for `imenu-extract-index-name-function'.
;;  ;; This will generate a flat index of definitions in a lisp file.
;;  (save-match-data
;;    (and (looking-at "(def")
;;        (condition-case nil
;;            (progn
;;              (down-list 1)
;;              (forward-sexp 2)
;;              (let ((beg (point))
;;                    (end (progn (forward-sexp -1) (point))))
;;                (buffer-substring beg end)))
;;          (error nil)))))

;; (defun imenu-example--create-XLS-index ()
;;  ;; Example of a candidate for `imenu-create-index-function'.
;;  ;; It will generate a nested index of definitions.
;;  (let ((index-alist '())
;;       (index-var-alist '())
;;       (index-type-alist '())
;;       (index-unknown-alist '())
;;       prev-pos)
;;    (goto-char (point-max))
;;    (imenu-progress-message prev-pos 0)
;;    ;; Search for the function
;;    (while (beginning-of-defun)
;;      (imenu-progress-message prev-pos nil t)
;;      (save-match-data
;;       (and (looking-at "(def")
;;            (save-excursion
;;              (down-list 1)
;;              (cond
;;               ((looking-at "def\\(var\\|const\\)")
;;                (forward-sexp 2)
;;                (push (imenu-example--name-and-position)
;;                      index-var-alist))
;;               ((looking-at "def\\(un\\|subst\\|macro\\|advice\\)")
;;                (forward-sexp 2)
;;                (push (imenu-example--name-and-position)
;;                      index-alist))
;;               ((looking-at "def\\(type\\|struct\\|class\\|ine-condition\\)")
;;                (forward-sexp 2)
;;                (if (= (char-after (1- (point))) ?\))
;;                    (progn
;;                      (forward-sexp -1)
;;                      (down-list 1)
;;                      (forward-sexp 1)))
;;                (push (imenu-example--name-and-position)
;;                      index-type-alist))
;;               (t
;;                (forward-sexp 2)
;;                (push (imenu-example--name-and-position)
;;                      index-unknown-alist)))))))
;;    (imenu-progress-message prev-pos 100)
;;    (and index-var-alist
;;        (push (cons "Variables" index-var-alist)
;;              index-alist))
;;    (and index-type-alist
;;        (push (cons "Types" index-type-alist)
;;              index-alist))
;;    (and index-unknown-alist
;;        (push (cons "Syntax-unknown" index-unknown-alist)
;;              index-alist))
;;    index-alist))

;; (defun ess-imenu-STA (&optional arg)
;;  "Stata Language Imenu support for ESS."
;;  (interactive)
;;  (setq imenu-generic-expression
;;       '( (nil "New one needed" 1)))
;;  (imenu-add-to-menubar "Stata-fcts"))

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
