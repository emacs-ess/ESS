;;; essd-s+4.el --- S-PLUS 4.x customization
;;; Richard M. Heiberger, December 1998

;; Copyright (C) 1997 Richard M. Heiberger <rmh@fisher.stat.temple.edu>

;; Author: Richard M. Heiberger <rmh@fisher.stat.temple.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: December 1998
;; Modified: $Date: 1998/12/11 19:48:23 $
;; Version: $Revision: 1.3 $
;; RCS: $Id: essd-s+4.el,v 1.3 1998/12/11 19:48:23 rossini Exp $
;;
;; Keywords: start up, configuration.

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
;;; This file defines all the S-PLUS 4.x customizations for ess-mode
;;; with ddeclient.  

;;; Requires and Autoloads:

(require 'essl-s)
(require 'ess-iw32)

;;(autoload 'inferior-ess "ess-inf" "Run an ESS process")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process")

; Code:


(defvar S+4-customize-alist
  '((ess-local-customize-alist     . 'S+4-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . "S+4")
    (ess-suffix                    . "S")
    (ess-dump-filename-template    . (concat (user-login-name)
					     ".%s."
					     ess-suffix))
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-syntax-table         . S-syntax-table)
    (ess-mode-edit                 . 'S+4-mode)
    (ess-help-sec-regex            . ess-help-S+3-sec-regex)
    (ess-help-sec-keys-alist       . S+3-help-sec-keys-alist)
    (ess-loop-timeout              . 100000 )
    (ess-object-name-db-file       . "ess-S+4-namedb.el" )
    (ess-retr-lastvalue-command
     . ".Last.value <- get(\".ess.lvsave\",frame=0)\n")
    (ess-save-lastvalue-command
     . "assign(\".ess.lvsave\",.Last.value,frame=0)\n")
    (inferior-ess-program          . inferior-S+4-program-name)
    (inferior-ess-ddeclient        . "ddeclient")
    (inferior-ess-client-name      . "S-PLUS")
    (inferior-ess-client-command   . "SCommand")
    (inferior-ess-objects-command  . "objects(%d)\n")
    (inferior-ess-help-command     . "help(\"%s\")\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-primary-prompt   . "[a-zA-Z0-9() ]*> ?")
    (inferior-ess-secondary-prompt . "+ ?")
    (inferior-ess-start-file       . nil) ;"~/.ess-S+4")
    (inferior-ess-start-args       . ""))
 "Variables to customize for S+4")


(defun S+4-mode (&optional proc-name)
  "Major mode for editing S+4 source.  See ess-mode for more help."
  (interactive)
  (setq ess-customize-alist S+4-customize-alist)
  (ess-mode S+4-customize-alist proc-name)
  (ess-external-minor-mode t)
)


(defun S+4 (&optional proc-name)
  "Tell user to call 'S-PLUS 4.x', the 'Gui Thing'  from StatSci."
  (interactive)
  (setq ess-customize-alist S+4-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(S): ess-dialect=%s , buf=%s \n"
	   ess-dialect
	   (current-buffer)))
(message "Click on Splus.")
)


(defun S+4-transcript-mode ()
  "S-PLUS 4.x transcript mode."
  (interactive)
  (ess-transcript-mode S+4-customize-alist))



 ; Provide package

(provide 'essd-s+4)

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

;;; essd-s+4.el ends here
