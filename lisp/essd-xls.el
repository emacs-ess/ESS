;;; essd-xls.el --- XLispStat customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/09/01 17:51:11 $
;; Version: $Revision: 1.24 $
;; RCS: $Id: essd-xls.el,v 1.24 1997/09/01 17:51:11 rossini Exp $
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
;;; This file defines all the Splus 3.x customizations for ess-mode.

;;; Requires and Autoloads:

(require 'essl-lsp)

(autoload 'inferior-ess "ess-inf" "Run an ESS process")

;;; Code:

(defvar XLS-editing-alist 'Lisp-editing-alist)

(defvar XLS-customize-alist
  '((ess-language                  .  "XLS"               )
    (ess-dialect                   .  "XLS"               )
    (ess-editing-alist             .  'XLS-editing-alist  )
    (ess-loop-timeout              .  10000               )
    (ess-object-name-db-file       .  "ess-xls-namedb.el" )
    (ess-help-sec-regex            .  " ")
    (ess-help-sec-keys-alist       .  " ")
    (inferior-ess-primary-prompt   .  "> ?"               )
    (inferior-ess-program          .  inferior-XLS-program-name)
    (inferior-ess-help-command     .  "(help '%s)\n"      )
    (inferior-ess-objects-command  .  "(variables)\n"     )
    (inferior-ess-exit-command     .  "(exit)\n"          )
    (inferior-ess-start-file       . nil)  ; "~/.ess-XLS")
    (inferior-ess-start-args       . ""))
  "Variables to customize for XLS")


(defun XLS-mode (&optional proc-name)
  "Major mode for editing XLispStat source.  NOT EVEN STARTED."
  (interactive)
  (setq ess-customize-alist XLS-customize-alist)
  (lisp-mode))


(defun XLS ()
  "Call 'XLispStat', the Lisp statistical system from Luke Tierney."

  (interactive)
  (setq ess-customize-alist XLS-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(XLS): ess-dialect=%s , buf=%s\n"
  	   ess-dialect (current-buffer)))
  (inferior-ess))

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
