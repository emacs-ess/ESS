;;; essl-lsp.el --- Support for editing Lisp source code

;; Copyright (C) 1997 A.J. Rossini.

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossinI@stat.sc.edu>
;; Created: 1 Sept 1997
;; Modified: $Date: 1997/09/02 17:46:27 $
;; Version: $Revision: 1.3 $
;; RCS: $Id: essl-lsp.el,v 1.3 1997/09/02 17:46:27 rossini Exp $

;; This file is part of ess-mode

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

;; Configurations for editing XLispStat source code.

;;; Code:

 ; Requires and autoloads

(require 'lisp-mode) ;; Contents "translated" from lisp-mode.el

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
    (ess-set-style                . ess-default-style)
    (ess-local-process-name       . nil)
    (ess-keep-dump-files          . nil)
    (ess-mode-syntax-table        . 'lisp-mode-syntax-table)
    (font-lock-defaults           . '(lisp-font-lock-keywords)))
  "General options for editing LispStat, XLispStat, and ViSta source files.")

(provide 'essl-lsp)

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

;;; essl-lsp.el ends here
