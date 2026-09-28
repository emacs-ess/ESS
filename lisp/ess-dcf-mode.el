;;; ess-dcf-mode.el --- DCF Debian Control Format customization  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 1997-2026 Free Software Foundation, Inc.
;; Maintainer: ESS-core <ESS-core@r-project.org>
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

(require 'generic-x)

(define-generic-mode 'ess-dcf-mode nil nil
  '(("^\\(#.*\\)" 1 'font-lock-comment-face)
    ("^\\([^ \\f\\n\\r\\t\\v#][^:]*\\):" 1 'font-lock-variable-name-face))
  '("DESCRIPTION$")
  (list (lambda () (setq-local mode-name "ESS/DCF")))
  "A major mode for editing Debian Control Format files (e.g., DESCRIPTION).")

;;;###autoload
(add-to-list 'auto-mode-alist '("DESCRIPTION\\'" . ess-dcf-mode))

(provide 'ess-dcf-mode)

;;; ess-dcf-mode.el ends here
