;;; essd-r.el --- R customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/09/01 18:12:12 $
;; Version: $Revision: 1.32 $
;; RCS: $Id: essd-r.el,v 1.32 1997/09/01 18:12:12 rossini Exp $
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


;;; Autoloads:

(autoload 'inferior-ess "ess-inf" "Run an ESS process")

;;; Code:

(defconst ess-help-R-sec-keys-alist 
  '((?a . "\\s *Arguments:") 
    (?d . "\\s *Description:")
    (?e . "\\s *Examples:") 
    (?n . "\\s *Note:")
    (?r . "\\s *References:") 
    (?s . "\\s *See Also:") 
    (?v . "\\s *Value[s]?")	;
    )) ;; "Alist of (key . string) pairs for use in section searching."

(defconst ess-help-R-sec-regex "^\\s *[A-Z[a-z. ---]+:$")

;;(ess-customize-alist           . 'R-customize-alist)
(defvar R-customize-alist
  '((ess-language                  . "S")
    (ess-dialect                   . "R")
    (ess-help-sec-regex            . ess-help-R-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-R-sec-keys-alist) 
    (ess-loop-timeout              . 100000 )
    (ess-object-name-db-file       . "ess-r-namedb.el" )
    (ess-retr-lastvalue-command    . ".Last.value <- get(\"smode.lvsave\",envir=1)\n")
    (ess-save-lastvalue-command    . "assign(\"smode.lvsave\",.Last.value,envir=1)\n")
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
  (ess-mode R-customize-alist proc-name))


(defun R ()
  "Call 'R', the 'Splus clone' from Robert & Ross (Auckland, NZ).
New way to do it."
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
