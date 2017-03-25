;;; ess-compat.el --- simple determination of Emacs and version #.

;; Copyright (C) 2000--2017 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 07 June 2000
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

;; This file contains functions for easily determining features of the
;; version of Emacs that we are using.

;;; Code:

(defun ess-sleep ()
  ;; FIXME: Not a "compatibility" thing any more, so move to ess-utils.el.
  "Put emacs to sleep for `ess-sleep-for-shell' seconds (floats work)."
  (sleep-for ess-sleep-for-shell))

;; for emacs <= 24.2 :
(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
    (declare (debug defvar) (doc-string 3))
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'progn (list 'defvar var val docstring)
          (list 'make-variable-buffer-local (list 'quote var)))))

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'set (list 'make-local-variable (list 'quote var)) val)))

(provide 'ess-compat)

 ; Local variables section

;; This file is automatically placed in Outline minor mode.
;; The file is structured as follows:
;; Chapters:     ^L ;
;; Sections:    ;;*;;
;; Subsections: ;;;*;;;
;; Components:  defuns, defvars, defconsts
;;              Random code beginning with a ;;;;* comment
;; Local variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;; End:

;;; ess-compat.el ends here
