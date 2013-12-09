;;; ess-comp.el --- setting for compiling, only.

;; Copyright (C) 1997--2006 A. J. Rossini
;; Copyright (C) 1997--2006 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <blindglobe@gmail.com>
;; Created: 25 July 1997
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS

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

;; This file sets up all compilation needs.

;;; Code:

(provide 'ess-comp)

;; Emacs doesn't include '.' in the emacs lisp load path.
(add-to-list 'load-path nil)

;; defvar'ed to nil in ./ess-site.el
(setq ess-show-load-messages t)

(defun ess-message (format-string &rest args)
  "Shortcut for \\[message] only if `ess-show-load-messages' is non-nil."
  (if ess-show-load-messages (message format-string args)))

;; These are required by every other file.
(ess-message "loading 'ess-compat ..")  (require 'ess-compat) ;set Emacsen diffs
(ess-message "loading 'ess-custom ..")  (require 'ess-custom) ;set variables
(ess-message "loading 'ess ..")       (require 'ess)      ;configure
(ess-message "loading 'ess-site ..")  (require 'ess-site) ;overload defaults

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
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-comp.el ends here
