;;; ess-python.el --- ESS support for python         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  J. Alexander Branham

;; Author: J. Alexander Branham <alex.branham@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ESS support for python. Support is experimental, use at your own
;; risk. Please contact the developers before relying on this code,
;; APIs are subject to change.

;;; Code:

(require 'ess-help)
(require 'ess-inf)
(require 'python)

(eval-when-compile
  (require 'subr-x))

(defgroup ess-python nil
  "ESS support for python"
  :group 'languages
  :group 'ess
  :group 'python)

(defcustom ess-python-post-run-hook nil
  "Hook to run after starting python."
  :type 'hook
  :group 'ess-python
  :group 'ess-hooks
  :package-version '(ess . "19.04"))

(defvar ess-python-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ess-mode-map)
    map)
  "Keymap for `ess-python-mode'.")

(cl-defmethod ess-help-get-topics (_proc &context ((string= ess-dialect "python") (eql t)))
  ;; TODO, but return nil for now so that `ess-display-help-on-object'
  ;; doesn't error.
  nil)

(cl-defmethod ess-start-process-specific (_language _dialect &context ((string= ess-dialect "python") (eql t)))
  (save-current-buffer
    (run-ess-python)))

(cl-defmethod ess-quit--override (_arg &context ((string= ess-dialect "python") (eql t)))
  "ARG is ignored for python."
  (if-let ((sprocess (ess-get-process ess-current-process-name)))
      (progn (ess-cleanup)
             (ess-send-string sprocess "exit()\n" t))
    (error "No ESS process running")))

(defun ess-python-input-sender (proc string)
  "Input sender for `ess-inferior-python-mode' buffers.
`comint-input-sender' is set to this, which see for PROC and
STRING."
  (save-current-buffer
    (let ((comint-output-filter-functions nil))
      (cond ((string-match (rx bol (0+ whitespace) (group-n 1 "help(") (group-n 2 (1+ nonl)) ")")
                           string)
             (ess-display-help-on-object (match-string 2 string))
             (process-send-string proc "\n"))
            (t ;; normal command
             (inferior-ess-input-sender proc string))))))

;;;###autoload
(defun run-ess-python (&optional start-args)
  "Start an inferior python process.
START-ARGS (interactively, \\[universal-argument]) allows
modifying the command line arguments."
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Run python with args: " python-shell-interpreter-args))
     (list python-shell-interpreter-args)))
  (let* ((inferior-ess-program python-shell-interpreter)
         (inf-buf (inferior-ess (or start-args python-shell-interpreter-args) '((ess-dialect . "python")))))
    (with-current-buffer inf-buf
      (run-mode-hooks 'ess-python-post-run-hook)
      (comint-goto-process-mark))
    inf-buf))

;;;###autoload
(define-derived-mode ess-python-mode python-mode "ESS[python]"
  "ESS mode for python buffers."
  (setq ess-dialect "python"))

(define-derived-mode inferior-ess-python-mode inferior-ess-mode "iESS[python]"
  "Mode for inferior python buffers."
  (setq ess-dialect "python"
        inferior-ess-help-command "help(%s)\n"
        inferior-ess-primary-prompt python-shell-prompt-regexp
        inferior-ess-secondary-prompt python-shell-prompt-block-regexp
        inferior-ess-search-list-command "import sys; sys.modules.keys()\n"
        inferior-ess-exit-command "exit()\n")
  (setq-local ess-getwd-command "import os; os.getcwd(os.path.expanduser('%s'))")
  (setq-local ess-setwd-command "import os; os.chdir('%s')")
  (setq-local comint-input-sender #'ess-python-input-sender)
  (setq-local compilation-error-regexp-alist python-shell-compilation-regexp-alist)
  (compilation-shell-minor-mode))

(add-to-list 'auto-mode-alist '("\\.py\\'" . ess-python-mode))

(provide 'ess-python)
;;; ess-python.el ends here
