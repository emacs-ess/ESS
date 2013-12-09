;;; ess-vst-d.el --- ViSta customization

;; Copyright (C) 1997 A. J. Rossini
;; Copyright (C) 1997--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@u.washington.edu>
;; Created: 26 Aug 1997
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; This file extends the XLispStat configuration for ViSta.

;;; Code:

;;; Requires and Autoloads:

(require 'ess-lsp-l)

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")

(defvar VST-customize-alist
  '((ess-customize-alist           .  VST-customize-alist )
    (ess-language                  .  "XLS"               )
    (ess-dialect                   .  "ViSta"             )
    (ess-loop-timeout              .  ess-XLS-loop-timeout)
    (ess-object-name-db-file       .  "ess-xls-namedb.el" )
    (ess-help-sec-regex            .  " ")
    (ess-help-sec-keys-alist       .  " ")
    (inferior-ess-primary-prompt   .  "> ?"               )
    (comint-use-prompt-regexp      . t)
    (inferior-ess-program          .  inferior-VST-program-name)
    (inferior-ess-help-command     .  "(help '%s)\n"      )
    (inferior-ess-objects-command  .  "(variables)\n"     )
    (inferior-ess-exit-command     .  "(exit)\n"          )
    (inferior-ess-start-file       . nil) ;"~/.ess-VST")
    ;;(inferior-ess-start-args       . nil)
    )
  "Variables to customize for XLS.")


(defun VST-mode (&optional proc-name)
  "Major mode for editing ViSta source.  NOT EVEN STARTED."
  (interactive)
  (setq ess-customize-alist VST-customize-alist)
  (lisp-mode))


(defun ViSta ()
  "Call 'ViSta', the extend XLispStat statistical system, from Forrest Young."

  (interactive)
  (setq ess-customize-alist VST-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(ViSta): ess-dialect=%s , buf=%s\n"
           ess-dialect (current-buffer)))
  (inferior-ess))

 ; Provide package

(provide 'ess-vst-d)

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

;;; ess-vst-d.el ends here
