;;; essd-xls.el --- XLispStat customization for ESS.

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1999/09/15 05:56:13 $
;; Version: $Revision: 5.4 $
;; RCS: $Id: essd-xls.el,v 5.4 1999/09/15 05:56:13 ess Exp $
;;
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
;;; essl-lsp for general Lisp modifications.

;;; Requires and Autoloads:

(require 'essl-lsp)

(autoload 'inferior-ess "ess-inf" "Run an ESS process")

;;; Code:

(defvar XLS-help-sec-keys-alist
  '((?a . "Args:"))
  "Sparse online XLS help.")

(defvar XLS-editing-alist Lisp-editing-alist)

(defvar XLS-customize-alist
  '((ess-local-customize-alist     . 'XLS-customize-alist)
    (ess-language                  . "XLS"               )
    (ess-dialect                   . "XLS"               )
    (ess-mode-editing-alist        . XLS-editing-alist   )
    (ess-loop-timeout              . 50000               )
    (ess-object-name-db-file       . "ess-xls-namedb.el" )
    (ess-help-sec-regex            . "^[A-Z. ---]+:$")
    (ess-help-sec-keys-alist       . XLS-help-sec-keys-alist)
    (ess-retr-lastvalue-command    . "()\n" )
    (ess-save-lastvalue-command    . "()\n" )
    (inferior-ess-primary-prompt   . "> ?"               )
    (inferior-ess-secondary-prompt . "^"                 )
    (inferior-ess-program          . inferior-XLS-program-name)
    (inferior-ess-help-command     . "(help '%s)\n"      )
    (inferior-ess-objects-command  . "(variables)\n"     )
    (inferior-ess-exit-command     . "(exit)\n"          )
    (inferior-ess-start-file       . nil)
    (inferior-ess-start-args       . ""))
  "Variables to customize for XLS")

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

(provide 'essd-xls)

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

;;; essd-xls.el ends here
