;;; essd-sp5.el --- S-Plus 6  customization

;; Copyright (C) 2001 A.J. Rossini <rossini@u.washington.edu>

;; Author: A.J. Rossini <rossini@u.washington.edu>
;; Maintainer: A.J. Rossini <rossini@u.washington.edu>
;; Created: 9 Nov 1998
;; Modified: $Date: 2001/07/27 23:04:17 $
;; Version: $Revision: 1.2 $
;; RCS: $Id: essd-sp6.el,v 1.2 2001/07/27 23:04:17 rossini Exp $
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

(defvar S+6-customize-alist
  '((ess-local-customize-alist     . 'S+6-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . S+6-dialect-name)
    (ess-suffix                    . "S")
    (ess-dump-filename-template    . (concat (user-login-name)
					     ".%s."
					     ess-suffix))
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-syntax-table         . S-syntax-table)
    (ess-help-sec-regex            . ess-help-S+-sec-regex)
					;or just "^[A-Z. ---]+:$"
    (ess-help-sec-keys-alist       . S+-help-sec-keys-alist)

    (ess-function-template         . " <- \n#\nfunction()\n{\n\n}\n")
    (ess-loop-timeout              . 500000 )
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
    (inferior-ess-search-list-command . "search(\"paths\")\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-primary-prompt   . "[a-zA-Z0-9() ]*> ?")
    (inferior-ess-secondary-prompt . "+ ?"))

  "Variables to customize for S.")


(defun S+6 (&optional proc-name)
  "Call 'Splus6', based on S version 4, from Bell Labs.
New way to do it."
  (interactive)
  (setq ess-customize-alist S+6-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(S+6): ess-dialect=%s, buf=%s\n" ess-dialect (current-buffer)))
  (inferior-ess))


(defun S+6-mode (&optional proc-name)
  "Major mode for editing S+6 source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S+6-customize-alist)
  (ess-mode S+6-customize-alist proc-name))

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

;;; essd-sp5.el ends here
