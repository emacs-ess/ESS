;;; ess-sesman.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 13, 2021
;; Modified: December 13, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/ess-sesman
;; Package-Requires: ((emacs 29.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'sesman)
(require 'sesman-browser)

(defun ess-sesman-sessions ()
  "Return a list of all active ESS sessions."
  (sesman-sessions 'ESS))

(cl-defmethod sesman-more-relevant-p ((_system (eql ESS)) session1 session2)
  "Figure out if SESSION1 or SESSION2 is more relevant."
  (sesman-more-recent-p (cdr session1) (cdr session2)))

(cl-defmethod sesman-project ((_system (eql ESS)))
  (ignore-errors (expand-file-name (directory-file-name (ess--project-root (project-current))))))

(cl-defmethod sesman-start-session ((_system (eql ESS)))
  "Start a connection of any type interactively.
Session will be named after the LANG inferior repl."
  (pcase major-mode
    ('ess-r-mode
     (call-interactively #'run-ess-r))
    ('ess-julia-mode
     (call-interactively #'run-ess-julia)))
  (cons (buffer-name) (current-buffer)))

(cl-defmethod sesman-quit-session ((_system (eql ESS)) session)
  "Quit an ESS SESSION."
  (let ((repls (cdr session)))
    (cl-flet* ((quit (repl)
                     (when (buffer-live-p repl)
                       (with-current-buffer repl
                         (let* ((inf-buf (inferior-ess-force))
                                (inf-proc (get-buffer-process inf-buf)))
                           (ess-quit 'no-save)
                           (inferior-ess--wait-for-exit inf-proc)
                           (kill-buffer inf-buf))))))
      (save-selected-window
        (mapc #'quit repls)))))

(cl-defmethod ess-quit--override (arg &context (ess-dialect "R"))
  "With ARG, do not offer to save the workspace.
Additionally, remove sesman object."
  (let ((cmd (format "base::q('%s')\n" (if arg "no" "default")))
        (sprocess (ess-get-process ess-current-process-name)))
    (when (not sprocess) (error "No ESS process running"))
    (sesman-remove-object 'ESS nil (current-buffer) arg t)
    (ess-cleanup)
    (ess-send-string sprocess cmd)))

(cl-defgeneric ess-quit--override (arg &context (ess-dialect "julia"))
  "Stop the inferior process.
Additionally, remove sesman object."
  (let ((proc (ess-get-process)))
    (sesman-remove-object 'ESS nil (current-buffer) arg t)
    (ess-cleanup)
    (when ess-eval-visibly
      (goto-char (marker-position (process-mark proc)))
      (insert inferior-ess-exit-command))
    (process-send-string proc inferior-ess-exit-command)))

(cl-defmethod sesman-restart-session ((_system (eql ESS)) session)
  "Restart an ESS SESSION."
  (let ((ses-name (car session))
        (repls (cdr session)))
    (cl-flet* ((restart (repl)
                        (when (buffer-live-p repl)
                          (with-current-buffer repl
                            (inferior-ess-reload)))))
      (mapc #'restart repls))))

(defun ess--sesman-init-repl (&rest _)
  "Set local variables necessary for a new inf repl."
  (setq-local sesman-system 'ESS)
  (sesman-add-object 'ESS (buffer-name) (current-buffer) 'allow-new))

(defun ess--sesman-ensure-process-name ()
  "Ensure ESS process name and sesman system are registered."
  (setq-local sesman-system 'ESS)
  (when (and (not ess-local-process-name)
             (sesman-current-session 'ESS))
    (setq ess-local-process-name
          (process-name (get-buffer-process (cadr (sesman-current-session 'ESS)))))))

(defun ess--sesman-switch-process-link (orig-fun &rest args)
  "Ensure sesman session switches to new process."
  (let ((buf (buffer-name))
        (new-buf (apply orig-fun args)))
    (sesman-link-with-buffer buf (sesman-session-for-object 'ESS new-buf))))


;; ensure sesman set for script buffers
(add-hook 'ess-r-mode-hook #'ess--sesman-ensure-process-name)
(add-hook 'ess-julia-mode-hook #'ess--sesman-ensure-process-name)

;; ensure sesman set for repl buffers
(advice-add #'inferior-ess--set-major-mode :after #'ess--sesman-init-repl)
(advice-add 'ess-switch-process :around #'ess--sesman-switch-process-link)

(provide 'ess-sesman)
;;; ess-sesman.el ends here
