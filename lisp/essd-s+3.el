;;; essd-s+3.el --- Splus 3.x customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/07/17 20:51:31 $
;; Version: $Revision: 1.13 $
;; RCS: $Id: essd-s+3.el,v 1.13 1997/07/17 20:51:31 rossini Exp $
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

;;;
;;: $Log: essd-s+3.el,v $
;;: Revision 1.13  1997/07/17 20:51:31  rossini
;;: stuff
;;:
;;: Revision 1.12  1997/07/17 20:49:21  rossini
;;: new version, take 1.
;;:
;;: Revision 1.11  1997/06/22 23:13:14  rossini
;;: removed S-inf... variable.  Whoops.
;;:
;;: Revision 1.10  1997/06/19 21:17:02  rossini
;;: added font-lock-keywords, ala RMH.
;;:
;;: Revision 1.9  1997/06/15 08:42:53  rossini
;;: setq-> setq-default.  I think this is correct!@
;;:
;;: Revision 1.8  1997/06/15 08:18:10  rossini
;;: added autoload for inferior-ess
;;:
;;: Revision 1.7  1997/06/15 07:11:10  rossini
;;: need to provide the actual file name.
;;:
;;: Revision 1.6  1997/06/14 23:12:08  rossini
;;: Finally set up properly.
;;:
;;;

;;; Autoloads:

(autoload 'inferior-ess "ess-inf" "Run an ESS process")

;;; Code:

(defvar S+3-customize-alist
  '((ess-proc-prefix      .         "S")
    (ess-version-running  .         "S+3")
    (inferior-ess-program .         "Splus")
    (ess-help-sec-regex   .         "^[A-Z. ---]+:$")
    (ess-help-sec-keys-alist .      '((?a . "ARGUMENTS:")
				      (?b . "BACKGROUND:")
				      (?B . "BUGS:")
				      (?d . "DETAILS:")
				      (?D . "DESCRIPTION:")
				      (?e . "EXAMPLES:")
				      (?n . "NOTE:")
				      (?o . "OPTIONAL ARGUMENTS:")
				      (?r . "REQUIRED ARGUMENTS:")
				      (?R . "REFERENCES:")
				      (?s . "SIDE EFFECTS:")
				      (?S . "SEE ALSO:")
				      (?u . "USAGE:")
				      (?v . "VALUE:")))
    (inferior-ess-objects-command . "objects(%d)")
                                     ;(if (string= ess-version-running "S3")
				     ;    "objects(%d)"
				     ;  "ls()")
    (inferior-ess-help-command .    "help(\"%s\",pager=\"cat\",window=F)\n")
	                             ;(if S-plus
				     ; "help(\"%s\",pager=\"cat\",window=F)\n"
				     ; "help(\"%s\")\n")
    (inferior-ess-exit-command .    "q()\n")
    (ess-loop-timeout              . 100000 )
 "Variables to customize for S")


(defun S ()
  "Call 'Splus 3.x', the 'Real Thing'  from StatSci.
New way to do it."
  (interactive)
  (setq ess-customize-alist S+3-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(S): ess-proc-prefix=%s , buf=%s \n"
	   ess-proc-prefix
	   (current-buffer)))
  (inferior-ess))

;; From RMH:  (for both s+3 and s3) ? 
;;(add-to-list 'S-inf-font-lock-keywords
;;	     '("^Syntax error" . font-lock-reference-face)) ; S-inf problems
;;(add-to-list 'S-inf-font-lock-keywords
;;	     '("^Error:" . font-lock-reference-face)) ; S-inf error
;;(add-to-list 'S-inf-font-lock-keywords
;;	     '("^Error in" . font-lock-reference-face)) ; S-inf error
;;(add-to-list 'S-inf-font-lock-keywords
;;	     '("^Dumped" . font-lock-reference-face)) ; S-inf error


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
