;;; ess-omg-d.el --- Omega customization

;; Copyright (C) 1999 A. J. Rossini
;; Copyright (C) 2000--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 15 August 1999
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS.

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

;; This file defines all the S-PLUS 3.x customizations for ess-mode.

;;; Code:

;;; Requires and Autoloads:

(require 'ess-omg-l)

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

(defvar OMG-dialect-name "OMG"
  "Name of 'dialect' for Omega.") ;easily changeable in a user's .emacs

(defvar OMG-customize-alist
  '((ess-local-customize-alist     . 'OMG-customize-alist)
    (ess-language                  . "OMG")
    (ess-dialect                   . "omegahat")
    (ess-suffix                    . "omg")
    (ess-loop-timeout              . 5000)
    (ess-dump-filename-template    . (ess-replace-regexp-in-string
                                      "S$" ess-suffix ; in the one from custom:
                                      ess-dump-filename-template-proto))
    (ess-mode-editing-alist        . OMG-editing-alist)
    (ess-mode-syntax-table         . OMG-syntax-table)
    (ess-change-sp-regexp          . "");fixme (if omegahat ever ..)
    (ess-help-sec-regex            . ess-help-S+-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-S+sec-keys-alist)
    (ess-object-name-db-file       . "ess-omg-namedb.el" )
    (inferior-ess-program          . inferior-OMG-program-name)
    (inferior-ess-objects-command  . "objects(%d)\n")
    (inferior-ess-help-command     . "help(\"%s\",pager=\"cat\",window=F)\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-primary-prompt   . "\\[[0-9]*\\]")
    (inferior-ess-secondary-prompt . ".. ?")
    (comint-use-prompt-regexp      . t)
    (inferior-ess-start-file       . nil) ;"~/.ess-omg")
    (inferior-ess-start-args       . ""))
  "Variables to customize for OMG (Omegahat)")


(defun OMG (&optional start-args) ; proc-name)
  "Call Omegahat, from the Omega Group for Statistical Computing."
  (interactive "P")
  (setq ess-customize-alist OMG-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(OMG): ess-dialect=%s, buf=%s\n"
           ess-dialect
           (current-buffer)))
  (let ((omg-start-args
         (concat inferior-ess-start-args
                 (if start-args (read-string
                                 "Starting Args [possibly -CORBA] ? ")
                   nil))))
    (inferior-ess omg-start-args)))



(fset 'omegahat 'OMG)

(defun OMG-mode (&optional proc-name)
  "Major mode for editing Omegahat source.  NOT EVEN STARTED."
  (interactive)
  (setq ess-customize-alist OMG-customize-alist)
  (ess-mode OMG-customize-alist proc-name)
  ;;(java-mode)
  (setq major-mode 'OMG-mode))

(fset 'omegahat-mode 'OMG-mode)

(defun OMG-transcript-mode ()
  "Omegahat transcript mode."
  (interactive)
  (ess-transcript-mode OMG-customize-alist))

 ; Provide package

(provide 'ess-omg-d)

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

;;; ess-omg-d.el ends here
