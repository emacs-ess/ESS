;;; ess-lsp-l.el --- Support for editing Lisp source code

;; Copyright (C) 1997 A.J. Rossini.
;; Copyright (C) 1998--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 1 Sept 1997
;; Maintainers: ESS-core <ESS-core@r-project.org>

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

;; Configurations for editing XLispStat source code.  Contains any underlying
;; changes that need to be made.

;;; Code:

 ; Requires and autoloads

;; Contents "translated" from lisp-mode.el
(require 'lisp-mode)

 ; Configuration variables



(defvar Lisp-editing-alist
  '((paragraph-start              . (concat "^$\\|" page-delimiter))
    (paragraph-separate           . (concat "^$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (fill-paragraph-function      . 'lisp-fill-paragraph)
    (adaptive-fill-mode           . nil)
    (indent-line-function         . 'lisp-indent-line)
    (indent-region-function       . 'lisp-indent-region)
    (require-final-newline        . t)
    (comment-start                . ";")
    (comment-start-skip           . "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
    (comment-column               . 40)
    (comment-indent-function      . 'lisp-comment-indent)
    (parse-sexp-ignore-comments   . t)
    (ess-style                . ess-default-style)
    (ess-local-process-name       . nil)
    ;;(ess-keep-dump-files          . 'ask)
    (ess-mode-syntax-table        . lisp-mode-syntax-table)
    (font-lock-defaults           . '(lisp-font-lock-keywords)))
  "General options for editing LispStat, XLispStat, and ViSta source files.")

(provide 'ess-lsp-l)

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

;;; ess-lsp-l.el ends here
