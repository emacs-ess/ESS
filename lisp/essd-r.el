;;; essd-r.el --- R customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/09/09 14:39:33 $
;; Version: $Revision: 1.42 $
;; RCS: $Id: essd-r.el,v 1.42 1997/09/09 14:39:33 rossini Exp $
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
;;; This file defines all the R customizations for ess-mode.


;;; Autoloads: and Requires

(require 'essl-s)

(autoload 'inferior-ess "ess-inf" "Run an ESS process")

(autoload 'ess-mode     "ess-mode" "Edit an ESS process")

;;; Code:

(defvar R-customize-alist
  '((ess-local-customize-alist     . 'R-customize-alist)
    (ess-language                  . "S")
    (ess-dialect                   . "R")
    (ess-suffix                    . "R")
    (ess-dump-filename-template    . (concat (user-login-name)
					     ".%s."
					     ess-suffix))
    (ess-mode-editing-alist        . S-editing-alist)
    (ess-mode-syntax-table         . S-syntax-table)
    (ess-help-sec-regex            . ess-help-R-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-R-sec-keys-alist) 
    (ess-loop-timeout              . 100000 )
    (ess-object-name-db-file       . "ess-r-namedb.el" )
    (ess-retr-lastvalue-command
     . ".Last.value <- get(\".ess.lvsave\",inherits=T)\n") ; envir=1
    (ess-save-lastvalue-command
     . "assign(\".ess.lvsave\",.Last.value,inherits=T)\n") ;envir=1
    (inferior-ess-program          . inferior-R-program-name)
    (inferior-ess-help-command     . "help(\"%s\")\n")
    (inferior-ess-objects-command  . "objects(pos = %d)\n")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-primary-prompt   . "[][a-zA-Z0-9() ]*> ?")
    (inferior-ess-start-file       . nil) ; "~/.ess-R")
    (inferior-ess-start-args       . ""))
  "Variables to customize for R")


(defun R-mode  (&optional proc-name) 
  "Major mode for editing R source.  See ess-mode for more help."
  (interactive)
  (setq-default ess-customize-alist R-customize-alist)
  (ess-mode R-customize-alist proc-name))


(defun R ()
  "Call 'R', the 'Splus clone' from Robert & Ross (Auckland, NZ)."
  (interactive)
  (setq ess-customize-alist R-customize-alist)
  ;; for debugging only
  (ess-write-to-dribble-buffer
   (format "(R): ess-dialect=%s , buf=%s \n"
	   ess-dialect
	   (current-buffer)))
  (inferior-ess))

 ; Provide package

(provide 'essd-r)

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

;;; ess-site.el ends here
