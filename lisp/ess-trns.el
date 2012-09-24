;;; ess-trns.el --- Support for manipulating S transcript files

;; Copyright (C) 1989--1994 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2012 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Maintainer: ESS-core <ESS-core@r-project.org>

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

;; Code for dealing with ESS transcripts.

;;; Code:

 ; Requires and autoloads

(require 'ess)

(eval-when-compile
  (require 'comint)
  (require 'ess-inf))

(autoload 'ess-eval-region              "ess-inf" "[autoload]" t)
(autoload 'ess-eval-region-and-go       "ess-inf" "[autoload]" t)
(autoload 'ess-eval-function            "ess-inf" "[autoload]" t)
(autoload 'ess-eval-function-and-go     "ess-inf" "[autoload]" t)
(autoload 'ess-eval-line                "ess-inf" "[autoload]" t)
(autoload 'ess-eval-line-and-go         "ess-inf" "[autoload]" t)
(autoload 'ess-eval-line-and-step       "ess-inf" "[autoload]" t)

(autoload 'comint-previous-prompt       "comint" "[autoload]" t)
(autoload 'comint-next-prompt           "comint" "[autoload]" t)

(autoload 'ess-load-file                "ess-inf" "[autoload]" t)
(autoload 'ess-request-a-process        "ess-inf" "(autoload)" nil)
(autoload 'get-ess-buffer               "ess-inf" "(autoload)" nil)
(autoload 'ess-switch-to-ESS            "ess-inf" "(autoload)" nil)
(autoload 'ess-switch-to-end-of-ESS     "ess-inf" "(autoload)" nil)
(autoload 'ess-eval-linewise            "ess-inf" "(autoload)" nil)
(autoload 'inferior-ess-get-old-input   "ess-inf" "(autoload)" nil)

 ; ess-transcript-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The major mode ess-transcript-mode
;;;; * Commands for ess-transcript-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*;; Major mode definition
(defvar ess-transcript-mode-map nil "Keymap for `ess-transcript-mode'.")
(unless ess-transcript-mode-map
  (setq ess-transcript-mode-map (make-sparse-keymap))

  (define-key ess-transcript-mode-map "\C-c\C-s" 'ess-switch-process)
  (define-key ess-transcript-mode-map "\C-c\C-r" 'ess-eval-region)
  (define-key ess-transcript-mode-map "\C-c\M-r" 'ess-eval-region-and-go)
  ;;  (define-key ess-transcript-mode-map "\M-\C-x"  'ess-eval-function)
  ;;  (define-key ess-transcript-mode-map "\C-c\M-f" 'ess-eval-function-and-go)
  ;;  (define-key ess-transcript-mode-map "\C-c\C-j" 'ess-eval-line)
  ;;  (define-key ess-transcript-mode-map "\C-c\M-j" 'ess-eval-line-and-go)

  (define-key ess-transcript-mode-map "\C-c\C-k"    'ess-force-buffer-current)
  (define-key ess-transcript-mode-map "\C-c\C-q"    'ess-quit)

  (define-key ess-transcript-mode-map "\C-c\C-j" 'ess-transcript-send-command)
  (define-key ess-transcript-mode-map "\C-c\M-j" 'ess-transcript-send-command-and-move)
  (define-key ess-transcript-mode-map "\M-\C-a"  'ess-goto-end-of-function-or-para)
  (define-key ess-transcript-mode-map "\M-\C-e"  'ess-goto-end-of-function-or-para)
  (define-key ess-transcript-mode-map "\C-c\C-y" 'ess-switch-to-ESS)
  (define-key ess-transcript-mode-map "\C-c\C-z" 'ess-switch-to-end-of-ESS)
  (define-key ess-transcript-mode-map "\C-c\C-v" 'ess-display-help-on-object)
  (define-key ess-transcript-mode-map "\C-c\C-d" 'ess-dump-object-into-edit-buffer)
  (define-key ess-transcript-mode-map "\C-c\C-t" 'ess-execute-in-tb)
  (define-key ess-transcript-mode-map "\C-c\t"   'ess-complete-object-name)
  (define-key ess-transcript-mode-map "\C-a"     'comint-bol)
  (define-key ess-transcript-mode-map "\M-\t"    'comint-replace-by-expanded-filename)
  (define-key ess-transcript-mode-map "\M-?"     'comint-dynamic-list-completions)
  (define-key ess-transcript-mode-map "\C-c\C-k" 'ess-request-a-process)
  (define-key ess-transcript-mode-map "{"        'ess-electric-brace)
  (define-key ess-transcript-mode-map "}"        'ess-electric-brace)
  (define-key ess-transcript-mode-map "\e\C-h"   'ess-mark-function)
  (define-key ess-transcript-mode-map "\e\C-q"   'ess-indent-exp)
                                        ;(define-key ess-transcript-mode-map "\177"    'backward-delete-char-untabify)
  (define-key ess-transcript-mode-map "\t"       'ess-indent-command)

  (define-key ess-transcript-mode-map "\C-c\C-p" 'comint-previous-prompt)
  (define-key ess-transcript-mode-map "\C-c\C-n" 'comint-next-prompt)
  ;; (define-key ess-transcript-mode-map "\C-c\C-n"    'ess-eval-line-and-step)

  (define-key ess-transcript-mode-map "\r"       'ess-transcript-send-command-and-move)
  (define-key ess-transcript-mode-map "\M-\r"    'ess-transcript-send-command)
  (define-key ess-transcript-mode-map "\C-c\r"   'ess-transcript-copy-command)
  (define-key ess-transcript-mode-map "\C-c\C-w" 'ess-transcript-clean-region))

(easy-menu-define
  ess-transcript-mode-menu ess-transcript-mode-map
  "Menu for use in S transcript mode."
  '("ESS-trans"
    ["What is this? (beta)" ess-mouse-me                        t]
    ["Describe"         describe-mode                   t]
    ["About"           (ess-goto-info "Transcript Mode") t]
    ["Send bug report"  ess-submit-bug-report           t]
    "------"
    ["Mark cmd group"   mark-paragraph          t]
    ["Previous prompt"  comint-previous-prompt  t]
    ["Next prompt"      comint-next-prompt      t]
    "------"
    ["Send and move" ess-transcript-send-command-and-move t]
    ["Copy command"  ess-transcript-copy-command                t]
    ["Send command"  ess-transcript-send-command                t]
    ["Clean Region"  ess-transcript-DO-clean-region     t]
    ["Switch S process" ess-switch-process              t]
    ))

(unless (featurep 'xemacs)
  (if (featurep 'ess-trans)
      (define-key ess-transcript-mode-map [menu-bar ess-trans]
        (cons "ess-trans" ess-transcript-mode-menu))
    (eval-after-load "ess-trans"
      '(define-key ess-transcript-mode-map
         [menu-bar ess-trans]
         (cons "ess-trans"
               ess-transcript-mode-menu)))))

(when (featurep 'xemacs)
  (defun ess-transcript-mode-xemacs-menu ()
    "Hook to install `ess-transcript-mode' menu for XEmacs (w/ easymenu)."
    (if 'ess-transcript-mode
        (easy-menu-add ess-transcript-mode-menu)
      (easy-menu-remove ess-transcript-mode-menu)))

  (add-hook 'ess-transcript-mode-hook 'ess-transcript-mode-xemacs-menu))


(defun ess-transcript-mode (alist &optional proc)
  "Major mode for manipulating {ESS} transcript files.

Type \\[ess-transcript-send-command] to send a command in the
transcript to the current S process. \\[ess-transcript-copy-command]
copies the command but does not execute it, allowing you to edit it in
the process buffer first.

Type \\[ess-transcript-clean-region] to delete all outputs and prompts
in the region, leaving only the S commands.  Other keybindings are:

\\{ess-transcript-mode-map}"
  (interactive)
  (require 'ess-inf)
  (kill-all-local-variables)
  (setq buffer-read-only t) ;; to protect the buffer.
  (ess-setq-vars-local alist); (current-buffer))
  (setq major-mode 'ess-transcript-mode)
  (setq mode-name "ESS Transcript")
  (use-local-map ess-transcript-mode-map)
  (set-syntax-table ess-mode-syntax-table)
  (setq mode-line-process
        '(" [" ess-local-process-name "]"))
  (make-local-variable 'ess-local-process-name)
  (setq ess-local-process-name nil)
  (unless inferior-ess-prompt ;; For S languages it is set in custom-alist
    (setq inferior-ess-prompt
          ;; Do not anchor to bol with `^'
          (concat "\\("
                  inferior-ess-primary-prompt
                  "\\|"
                  inferior-ess-secondary-prompt
                  "\\)")))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^" inferior-ess-prompt "\\|^\^L"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "^\^L")
  (make-local-variable 'comint-use-prompt-regexp)
  (setq comint-use-prompt-regexp t)
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))

  ;; font-lock support

  (when inferior-ess-font-lock-keywords ;; new system
    (setq inferior-ess-font-lock-defaults
          (ess--extract-default-fl-keywords inferior-ess-font-lock-keywords)))
  
  (set (make-local-variable 'font-lock-defaults)
       '(inferior-ess-font-lock-defaults nil nil ((?\. . "w") (?\_ . "w") (?' . "."))))

  ;;; Keep <tabs> out of the code.
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  (run-hooks 'ess-transcript-mode-hook))

;;*;; Commands used in S transcript mode

(defun ess-transcript-send-command ()
  "Send the command at point in the transcript to the ESS process.
The line should begin with a prompt.  The ESS process buffer is displayed if it
is not already."
  (interactive)
  (let* ((proc (or ess-local-process-name
                   (ess-request-a-process "Evaluate into which process? " t)))
         (ess-buf (get-ess-buffer proc)))
    (setq ess-local-process-name proc)
    (if (get-buffer-window ess-buf) nil
      (display-buffer ess-buf t))
    (let ((input (inferior-ess-get-old-input)))
      (with-current-buffer ess-buf
        (goto-char (point-max))
        (ess-eval-linewise input)))))

(defun ess-transcript-send-command-and-move ()
  "Send the command on this line, and move point to the next command."
  (interactive)
  ;; (ess-transcript-send-command) ;; This doesn't work properly
  ;; replacement code begins
  (let* ((proc (or ess-local-process-name
                   (ess-request-a-process "Evaluate into which process? " t)))
         (ess-buf (get-ess-buffer proc)))
    (setq ess-local-process-name proc)
    (if (get-buffer-window ess-buf) nil
      (display-buffer ess-buf t))
    (let ((input (inferior-ess-get-old-input)))
      (with-current-buffer ess-buf
        (goto-char (point-max))
        (ess-eval-linewise input nil nil nil 1))))
  ;; replacement code ends
  (goto-char ess-temp-point)
  (comint-next-prompt 1))

(defun ess-transcript-copy-command ()
  "Copy the command at point to the command line of the ESS process."
  (interactive)
  (let* ((proc (or ess-local-process-name
                   (ess-request-a-process "Evaluate into which process? " t)))
         (ess-buf (process-buffer (get-process proc)))
         (input (inferior-ess-get-old-input)))
    (setq ess-local-process-name proc)
    (if (get-buffer-window ess-buf) nil
      (display-buffer ess-buf t))
    (with-current-buffer ess-buf
      (goto-char (point-max))
      (insert input)))
  (ess-switch-to-end-of-ESS))

(defun ess-transcript-clean-region (beg end even-if-read-only)
  "Strip the transcript in the region, leaving only (R/S/Lsp/..) commands.
Deletes any lines not beginning with a prompt, and then removes the
prompt from those lines that remain.  Prefix argument means to 
clean even if the buffer is \\[read-only]."
  (interactive "r\nP")
  (unless inferior-ess-prompt
    (error "Cannot clean ESS transcript region in this mode!
 That only works in ess-transcript-mode or inferior-ess-mode ('*R*' etc)."
           ;; Maybe call ess-clean-region-in-new-transcript ?"))
           ))
  (let ((do-toggle (and buffer-read-only even-if-read-only))
        (ess-prompt-rx (concat "^" inferior-ess-prompt)))
    (save-excursion
      (if do-toggle (setq buffer-read-only 0))
      (save-restriction
        (unless (featurep 'xemacs) ;; does not exist in xemacs:
          (deactivate-mark))
        (narrow-to-region beg end)
        (goto-char (point-min))
        (delete-non-matching-lines ess-prompt-rx)
        (goto-char (point-min))
        ;; (replace-regexp  *  * ) :
        (while (re-search-forward ess-prompt-rx nil t)
          (replace-match "" nil nil)))

      (if do-toggle (setq buffer-read-only 1)))))


;; unfinished idea :-----------------------

;; (defun ess-clean-region-in-new-transcript (beg end)
;;   "Copy the region into a new ess-transcript buffer, and clean it there,
;;  using \\[ess-transcript-clean-region]."
;;   (interactive "r")

;;   (let ((bname (buffer-file-name)))
;;     (setq bname (if bname .. ..))
;;     (let
;;       (fbase (if fname (file-name-sans-extension (file-name-nondirectory fname))
;;                (buffer-name)))
;;
;;       ;; the buffer name should be like a file name
;;       (buf-nam ....)
;;       (trns-buf (get-buffer-create fbase))
;;     (pop-to-buffer trns-buf)
;;     (ess-transcript-mode .....)
;; )))




(defun ess-transcript-DO-clean-region (beg end)
  "Clean the current via \\[ess-transcript-clean-region] even if the buffer is read-only."
  (interactive "r")
  (ess-transcript-clean-region beg end 'In-ANY-case))

(defun ess-transcript-clean-buffer ()
  "Cleanup the whole buffer.
Use point-min/max to obey narrow-to-region."
  (interactive)
  (ess-transcript-clean-region (point-min) (point-max) 'In-ANY-case))

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

;;; ess-trns.el ends here
