;;; essd-s+3.el --- Splus 3.x customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/09/01 21:47:52 $
;; Version: $Revision: 1.23 $
;; RCS: $Id: essd-s+3.el,v 1.23 1997/09/01 21:47:52 rossini Exp $
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

(require 'essl-s)

(autoload 'inferior-ess "ess-inf" "Run an ESS process")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process")

;;; Definitions for Splus

(defconst ess-help-S+3-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?D . "DESCRIPTION:")
    (?d . "DETAILS:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
    (?o . "OPTIONAL ARGUMENTS:")
    (?R . "REFERENCES:") 
    (?r . "REQUIRED ARGUMENTS:")
    (?S . "SEE ALSO:")
    (?s . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Alist of (key . string) pairs for use in section searching.")
;;; `key' indicates the keystroke to use to search for the section heading
;;; `string' in an S help file. `string' is used as part of a
;;; regexp-search, and so specials should be quoted.

(defconst ess-help-S+3-sec-regex "^[A-Z. ---]+:$"
  "Reg(ular) Ex(pression) of section headers in help file")

; Code:

(defvar S+3-customize-alist
  '((ess-customize-alist-symb      . 'S+3-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . "S+3")
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-edit                 . 'S+3-mode)
    (ess-help-sec-regex            . ess-help-S+3-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-S+3-sec-keys-alist)
    (ess-loop-timeout              . 100000 )
    (ess-object-name-db-file       . "ess-s+3-namedb.el" )
    (ess-retr-lastvalue-command    . ".Last.value <- get(\"smode.lvsave\",frame=0)\n")
    (ess-save-lastvalue-command    . "assign(\"smode.lvsave\",.Last.value,frame=0)\n")
    (inferior-ess-program          . inferior-S+3-program-name)
    (inferior-ess-objects-command  . "objects(%d)")
    (inferior-ess-help-command     . "help(\"%s\",pager=\"cat\",window=F)\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-primary-prompt   . "[a-zA-Z0-9() ]*> ?")
    (inferior-ess-secondary-prompt . "+ ?")
    (inferior-ess-start-file       . nil) ;"~/.ess-S+3")
    (inferior-ess-start-args       . ""))
 "Variables to customize for S+3")

(defun S-mode (&optional proc-name)
  "Major mode for editing S+3 source.  See ess-mode for more help."
  (interactive)
  (setq-default ess-customize-alist S+3-customize-alist)
  (ess-mode S+3-customize-alist proc-name))

(defun S+3-mode (&optional proc-name)
  "Major mode for editing S+3 source.  See ess-mode for more help."
  (interactive)
  (setq-default ess-customize-alist S+3-customize-alist)
  (ess-mode S+3-customize-alist proc-name))


(defun S+3 (&optional proc-name)
  "Call 'Splus 3.x', the 'Real Thing'  from StatSci."
  (interactive)
  (setq-default ess-customize-alist S+3-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(S): ess-dialect=%s , buf=%s \n"
	   ess-dialect
	   (current-buffer)))
  (inferior-ess))

(defun S ()
  "Basic, usual, call..."
  (interactive)
  (S+3))


 ; Provide package

(provide 'essd-s+3)

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

;;; essd-s+3.el ends here
