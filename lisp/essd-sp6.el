;;; essd-sp6.el --- S-Plus 6  customization

;; Copyright (C) 2001 A.J. Rossini <rossini@u.washington.edu>

;; Author: A.J. Rossini <rossini@u.washington.edu>
;; Maintainer: ESS Core Team <ESS-core@stat.math.ethz.ch>
;; Created: 2001/02/06
;; Modified: $Date: 2004/05/03 02:17:19 $
;; Version: $Revision: 1.13 $
;; RCS: $Id: essd-sp6.el,v 1.13 2004/05/03 02:17:19 rossini Exp $
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
;;; AJR copied S+5 to be S+6.
;;; AJR copied S4 to be S+5.
;;; DB contributed the changes from essd-sp3.el to
;;; essd-s4.el. (removed the old ugly approach).
;;; This file defines Sp5 customizations for ess-mode.  Lots of thanks
;;; to RMH and JMC for code and suggestions
;;; Thanks to MM for making this sensible.

;;; Requires and Autoloads:

(require 'essl-s)

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

;;; Code:

;; You now need to make sure you've defined if you are running 5.0 or 5.1.
;; Lots of things are broken between them, GRR...

(defvar S+6-dialect-name "S+6"
  "Name of 'dialect' for S-PLUS 6.");easily changeable in a user's .emacs

(defun S+6-directory-p (directory)
  "Splus 5++ directories have a .Data directory and a __Meta directory within."
  (and directory
       (file-directory-p (concat directory ".Data"))
       (file-directory-p (concat directory ".Data/__Meta"))))

(defvar S+6-directory-function
  #'(lambda ()
      (if (S+6-directory-p default-directory)
	  default-directory
	(or ess-directory default-directory))))

(defvar S+6-setup-directory-function
  #'(lambda (startdir)
      (if (and startdir (S+6-directory-p startdir))
          (progn
	    (setenv "S_WORK"
		    (if (getenv "S_WORK")
			(concat startdir ":" (getenv "S_WORK"))
		      ;;(message "adding %s to S_WORK" startdir)
		      startdir))
            ))))

(defvar S+6-customize-alist
  '((ess-local-customize-alist     . 'S+6-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . S+6-dialect-name)
    (ess-suffix                    . "S")
    (ess-directory-function        . S+6-directory-function)
    (ess-setup-directory-function  . S+6-setup-directory-function)
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-syntax-table         . S-syntax-table)
    (ess-help-sec-regex            . ess-help-S+-sec-regex)
					;or just "^[A-Z. ---]+:$"
    (ess-help-sec-keys-alist       . S+-help-sec-keys-alist)

    (ess-function-template         . " <- \n#\nfunction()\n{\n\n}\n")
    (ess-loop-timeout              . ess-S-loop-timeout)
    (ess-dump-filename-template    . (replace-regexp-in-string
				      "S$" ess-suffix ; in the one from custom:
				      ess-dump-filename-template-proto))
    (ess-object-name-db-file       . "ess-sp6-namedb.el")
    (ess-dumped-missing-re
     . "\\(\\(<-\\|=\\)\nDumped\n\\'\\)\\|\\(\\(<-\\|=\\)\\(\\s \\|\n\\)*\\'\\)")
    (ess-syntax-error-re
     . "\\(Syntax error: .*\\) at line \\([0-9]*\\), file \\(.*\\)$")
    (ess-retr-lastvalue-command
     . ".Last.value <- get(\".ess.lvsave\",frame=0)\n")
    (ess-save-lastvalue-command
     . "assign(\".ess.lvsave\",.Last.value,frame=0)\n")
    (inferior-ess-program          . inferior-S+6-program-name)
    (inferior-ess-objects-command  . "objects(where = %d)\n")
    (inferior-ess-objects-pattern  . ".*") ; for new s4 stuff
    (inferior-ess-help-command     . "help(\"%s\",pager=\"slynx -dump\",window=F)\n")
    ;; "paths": get the "/" needed by  (ess-dir-modtime dir)  in ./ess-inf.el:
    ;; (inferior-ess-search-list-command . "search(\"paths\")\n")
    (inferior-ess-search-list-command . "search()\n")
    (inferior-ess-exit-command     . "q()\n")
    (comint-use-prompt-regexp-instead-of-fields . t) ;; emacs 21 and up
    (inferior-ess-primary-prompt   . "[a-zA-Z0-9() ]*> ?")
    (inferior-ess-secondary-prompt . "+ ?")
    (ess-STERM  . "iESS")
    (ess-editor . S-editor)
    (ess-pager  . S-pager)
    (inferior-ess-language-start . (eval inferior-S-language-start))
    )
  "Variables to customize for S+6.")


(defun S+6 (&optional proc-name)
  "Call 'Splus6', based on S version 4, from Bell Labs.
New way to do it."
  (interactive)
  (setq ess-customize-alist S+6-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(S+6): ess-dialect=%s, buf=%s\n" ess-dialect (current-buffer)))
  (inferior-ess)
  (if inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start)))


(defun S+6-mode (&optional proc-name)
  "Major mode for editing S+6 source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S+6-customize-alist)
  (ess-mode S+6-customize-alist proc-name)
  (if ess-imenu-use-S (ess-imenu-S)))

(defun S+6-transcript-mode ()
  "S-PLUS 6 transcript mode."
  (interactive)
  (ess-transcript-mode S+6-customize-alist))

 ; Provide package

(provide 'essd-sp6)

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

;;; essd-sp6.el ends here
