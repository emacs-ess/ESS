;;; ess-xls-d.el --- XLispStat customization for ESS.

;; Copyright (C) 1997 A. J. Rossini
;; Copyright (C) 1998--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Maintainers: ESS-core <ESS-core@r-project.org>

;; Keywords: Statistics

;; This file is part of ESS

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
;;; This file defines all the XLispStat customizations for ESS.  See
;;; ess-lsp-l.el for general Lisp modifications.

;;; Requires and Autoloads:

(require 'ess-lsp-l)

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")

;;; Code:

(defvar ess-help-XLS-sec-keys-alist
  '((?a . "Args:"))
  "Sparse online XLS help.")

(defvar XLS-editing-alist Lisp-editing-alist)

(defvar XLS-customize-alist
  '((ess-local-customize-alist     . 'XLS-customize-alist)
    (ess-language                  . "XLS"               )
    (ess-dialect                   . "XLS"               )
    (ess-mode-editing-alist        . XLS-editing-alist   )
    (ess-loop-timeout              . ess-XLS-loop-timeout)
    (ess-object-name-db-file       . "ess-xls-namedb.el" )
    (ess-help-sec-regex            . "^[A-Z. ---]+:$")
    (ess-help-sec-keys-alist       . ess-help-XLS-sec-keys-alist)
    (inferior-ess-primary-prompt   . "> ?"               )
    (inferior-ess-secondary-prompt . "^"                 )
    (comint-use-prompt-regexp-instead-of-fields . t) ;; emacs 21 and up
    (inferior-ess-program          . inferior-XLS-program-name)
    (inferior-ess-help-command     . "(help '%s)\n"      )
    (inferior-ess-objects-command  . "(variables)\n"     )
    (inferior-ess-exit-command     . "(exit)\n"          )
    ;;(inferior-ess-start-args       . ""))
    (inferior-ess-start-file       . nil))
  "Variables to customize for XLS.")

;;; The functions of interest (mode, inferior mode)

(defun XLS-mode (&optional proc-name)
  "Major mode for editing XLispStat source.  NOT EVEN STARTED."
  (interactive)
  (setq ess-customize-alist XLS-customize-alist)
  (ess-mode XLS-customize-alist proc-name)
  (setq major-mode 'XLS-mode))

(fset 'xlispstat-mode 'XLS-mode)

(defun XLS ()
  "Call 'XLispStat', the Lisp statistical system from Luke Tierney."

  (interactive)
  (setq ess-customize-alist XLS-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(XLS): ess-dialect=%s , buf=%s\n"
  	   ess-dialect (current-buffer)))
  (inferior-ess))

(defun xls-transcript-mode ()
  "Does the right thing."
  (interactive)
  (ess-transcript-mode XLS-customize-alist))

(fset 'XLS-transcript-mode 'xls-transcript-mode)

 ; Provide package

(provide 'ess-xls-d)

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

;;; ess-xls-d.el ends here
