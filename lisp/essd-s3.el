;;; essd-s3.el ---  S 3 (AT&T version) customization

;; Copyright (C) 1997 A. J. Rossini
;; Copyright (C) 1998--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Maintainers: ESS-core <ESS-core@stat.math.ethz.ch>

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
;;; This file defines all the S 3 customizations for ess-mode.

;;; Requires and Autoloads:

(require 'essl-s)

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")

;;; Code:

(defvar S3-customize-alist
  '((ess-local-customize-alist     . 'S3-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . "S3")
    (ess-suffix                    . "S")
    (ess-loop-timeout              . ess-S-loop-timeout)
    (ess-dump-filename-template    . (ess-replace-regexp-in-string
				      "S$" ess-suffix ; in the one from custom:
				      ess-dump-filename-template-proto))
    (ess-help-sec-regex            . "^[A-Z. ---]+:$")
;;;    (ess-help-sec-regex            . ess-help-S+-sec-regex)
    (ess-change-sp-regexp	   . ess-S-change-sp-regexp)
    (ess-help-sec-keys-alist       . S3-help-sec-keys-alist)
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-syntax-table         . S-syntax-table)
    (ess-object-name-db-file       . "ess-s3-namedb.el" )
    (ess-retr-lastvalue-command
     . ".Last.value <- get(\".ess.lvsave\",frame=0)\n")
    (ess-save-lastvalue-command
     . "assign(\".ess.lvsave\",.Last.value,frame=0)\n")

    (inferior-ess-program          . inferior-S3-program-name) ;        "S")
    (inferior-ess-help-command     . "help(\"%s\")\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-objects-command  . "objects(%d)\n")
    (inferior-ess-primary-prompt   . "[a-zA-Z0-9() ]*> ?")
    (inferior-ess-secondary-prompt . "+ ?")
    (comint-use-prompt-regexp-instead-of-fields . t) ;; emacs 21 and up
    (inferior-ess-start-file       . nil) ;"~/.ess-S3")
    (inferior-ess-start-args       . "")
    (ess-STERM  . "iESS")
    (ess-editor . S-editor)
    (ess-pager  . S-pager)
    (inferior-ess-language-start . (eval inferior-S-language-start))
    )
  "Variables to customize for S3")


(defun S3 (&optional proc-name)
  "Call 'S 3.x', the version from AT&T."
  (interactive)
  (setq ess-customize-alist S3-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(S3): ess-dialect=%s, buf=%s\n" ess-dialect (current-buffer)))
  (inferior-ess)
  (if inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start)))


(defun S3-mode (&optional proc-name)
  "Major mode for editing S3 source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S3-customize-alist)
  (ess-mode S3-customize-alist proc-name)
  (if ess-imenu-use-S (ess-imenu-R)))


 ; Provide package

(provide 'essd-s3)

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

;;; essd-s3.el ends here
