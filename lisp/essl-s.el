;;; essl-s.el --- Support for editing S source code

;; Copyright (C) 1989-1997 Bates, Kademan, Ritter, Smith, Hornik,
;; Heiberger, Maechler, and Rossini.

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossinI@stat.sc.edu>
;; Created: 26 Aug 1997
;; Modified: $Date: 1997/09/01 17:37:35 $
;; Version: $Revision: 1.3 $
;; RCS: $Id: essl-s.el,v 1.3 1997/09/01 17:37:35 rossini Exp $

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

;; Code for general editing S source code (specializes to S, S+, R). 

;;; Code:

 ; Requires and autoloads

 ; Configuration variables


(defvar S-editing-alist
  '((paragraph-start              . (concat "^$\\|" page-delimiter))
    (paragraph-separate           . (concat "^$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (indent-line-function         . 'ess-indent-line)
    (require-final-newline        . t)
    (comment-start                . "#")
    (comment-start-skip           . "#+ *")
    (comment-column               . 40)
    (comment-indent-function      . 'ess-comment-indent)
    (parse-sexp-ignore-comments   . t)
    (ess-set-style                . ess-default-style)
    (ess-local-process-name       . nil)
    (ess-keep-dump-files          . nil)
    (font-lock-defaults           . ess-mode-font-lock-keywords))
  "General options for editing S, S+, and R source files.")

(provide 'essl-s)

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

;;; essl-s.el ends here

