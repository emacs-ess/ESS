;;; r-ts-mode.el --- R mode for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.
;; Maintainer: ESS-core <ESS-core@r-project.org>
;; Created: April 2023
;; Keywords: R languages tree-sitter

;; This file is part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>


;;; Customization:

(defcustom r-indent-offset 4
  "Number of spaces for each indentation step in `r-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'R)


;;; Code:

(require 'treesit)
(eval-when-compile
  (require 'rx))

;; Eventually this dependency should be the other way around
(require 'ess-r-mode)

;;; Grammar

(defvar r--defun-type-regexp
  (regexp-opt '("lambda_function"
                "function_definition")))

(defvar r--indent-rules
  '((r
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "consequence") parent-bol r-indent-offset)
     ((parent-is "brace_list") parent-bol r-indent-offset)
     ((parent-is "binary") r--indent-binary-root-bol r-indent-offset)
     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `r-ts-mode'.")

(defun r--indent-binary-root-bol (node parent &rest _)
  (r--ts-node-start (r--ts-binary-root node parent)))

(defun r--ts-binary-root (node parent)
  "Find the root of a tree of binary expressions."
  (let ((is-binary-parent (lambda (n p) (equal (treesit-node-type p) "binary"))))
    (r--ts-climb-while node parent is-binary-parent)))

(defun r--ts-climb-while (node parent predicate)
  "Climb parents while predicate matches NODE and PARENT."
  (while (funcall predicate node parent)
    (setq node parent)
    (setq parent (treesit-node-parent node)))
  node)

(defun r--ts-node-start (node)
  "Like `treesit-node-start' but skips blank lines."
  (let ((start (treesit-node-start node)))
    (save-excursion
      (goto-char start)
      (or (when (re-search-forward "[^\t\n ]" nil t)
            (match-beginning 0))
          start))))


;;; Mode

(define-derived-mode r-ts-mode prog-mode "R"
  "Major mode for editing R, powered by tree-sitter."
  :group 'R
  :syntax-table ess-r-mode-syntax-table

  (if (not (treesit-ready-p 'r))
      (ess-r-mode)
    (treesit-parser-create 'r)

    ;; Text structure
    (setq-local comment-start "#")
    (setq-local comment-start-skip "#+ *")
    (setq-local paragraph-start (concat "\\s-*$\\|" page-delimiter))
    (setq-local paragraph-separate (concat "\\s-*$\\|" page-delimiter))
    (setq-local paragraph-ignore-fill-prefix t)

    ;; Navigation
    (setq-local treesit-defun-type-regexp r--defun-type-regexp)

    ;; Indent
    (setq-local indent-tabs-mode nil)
    (setq-local treesit-simple-indent-rules r--indent-rules)

    (treesit-major-mode-setup)))

(provide 'r-ts-mode)

;;; r-ts-mode.el ends here
