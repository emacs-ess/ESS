;;; ess-emcs.el --- simple determination of Emacs/XEmacs and version #.

;; Copyright (C) 2000 A.J. Rossini, R.M. Heiberger, Martin
;; Maechler, Kurt Hornik.

;; Author:  A.J. Rossini <rossini@biostat.washington.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 07 June 2000
;; Modified: $Date: 2000/06/07 22:20:20 $
;; Version: $Revision: 5.1 $
;; RCS: $Id: ess-emcs.el,v 5.1 2000/06/07 22:20:20 rossini Exp $
;;
;; Keywords: start up, configuration.

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file contains functions for easily determining which version
;; of Emacs we are using.

;;; Code:

;; Older versions of emacs did not have these variables
;; (emacs-major-version and emacs-minor-version.)
;; Let's define them if they're not around, since they make
;; it much easier to conditionalize on the emacs version.

(if (and (not (boundp 'emacs-major-version))
	 (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
	  (string-to-int (substring emacs-version
				    (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
	 (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
	  (string-to-int (substring emacs-version
				    (match-beginning 1) (match-end 1)))))

;;; Define a function to make it easier to check which version we're
;;; running.

(defun running-emacs-version-or-newer (major minor)
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
	   (>= emacs-minor-version minor))))

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(provide 'ess-emcs)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:	  ^L ;
;;; Sections:	 ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:	 defuns, defvars, defconsts
;;;		 Random code beginning with a ;;;;* comment
;;; Local variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-site.el ends here
