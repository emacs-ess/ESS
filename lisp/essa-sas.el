;;; essa-sas.el -- ESS local customizations for SAS, part a.

;; Copyright (C) 1997--2000 Rodney Sparapani, A.J. Rossini, 
;; Martin Maechler, Kurt Hornik, and Richard M. Heiberger.

;; Author: Rodney Sparapani <rodney.sparapani@duke.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 17 November 1999
;; Modified: $Date: 2000/03/06 16:57:57 $
;; Version: $Revision: 1.5 $
;; RCS: $Id: essa-sas.el,v 1.5 2000/03/06 16:57:57 maechler Exp $

;; Keywords: ESS, ess, SAS, sas, asynchronous.

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; In short: you may use this code any way you like, as long as you
;; don't charge money for it, remove this notice, or hold anyone liable
;; for its results.

;; Code:

(require 'ess-site)


;;; Managing submitted SAS jobs

; logical window setting for ESS
(defun ess-same-window-async ()
  (if (boundp 'same-window-buffer-names)
      (add-to-list 'same-window-buffer-names "*Async Shell Command*")))


(defvar ess-sas-root "."
  "Root of the file to perform operations on.")

(defvar ess-sas-dir "."
  "Directory where .sas, .log, .lst and .txt files are stored")

(defvar ess-sas-sas "."
  "The .sas file to perform operations on.")

(defvar ess-sas-log "."
  "The .log file to perform operations on.")

(defvar ess-sas-lst "."
  "The .lst file to perform operations on.")

(defvar ess-sas-txt "."
  "The .txt file to perform operations on.")

(defvar ess-sas-submit-command "sas"
        "Command to invoke SAS.")

(defun ess-sas-file (ess-sas-arg)
  "Create buffer and associate with the appropriate file"
  
  (if (not (get-file-buffer ess-sas-arg)) 
      (progn
	(create-file-buffer ess-sas-arg)
	(switch-to-buffer ess-sas-arg)
	(if (and (= emacs-major-version 19)
		 (< emacs-minor-version 30))
	    (set-visited-file-name (concat ess-sas-dir ess-sas-arg))
	  (set-visited-file-name (concat ess-sas-dir ess-sas-arg) t))
	(set-visited-file-modtime (list -1 -1))
	(ess-sas-revert-wisely ess-sas-arg))
    (switch-to-buffer ess-sas-arg)))



(defun ess-sas-set ()
  "Set file names according to usage.  
This sets up ess-sas-root, ess-sas-dir, ess-sas-sas, ess-sas-log,
ess-sas-lst and ess-sas-txt"
  (if (or (string= ".sas" (substring (buffer-name) -4)) 
	  (string= ".log" (substring (buffer-name) -4))
	  (string= ".lst" (substring (buffer-name) -4))
	  (string= ".txt" (substring (buffer-name) -4)))
      (progn
	(setq ess-sas-root (substring (buffer-name) 0 -4))
	(setq ess-sas-dir (file-name-directory (buffer-file-name)))
	(setq ess-sas-sas (concat ess-sas-root ".sas"))
	(setq ess-sas-log (concat ess-sas-root ".log"))
	(setq ess-sas-lst (concat ess-sas-root ".lst"))
	(setq ess-sas-txt (concat ess-sas-root ".txt")))))

(defun ess-sas-submit ()
  "Save the .sas file and submit to shell."
  (interactive)
  (ess-sas-set)
  (if (not (string= ess-sas-sas (buffer-name))) (ess-sas-file ess-sas-sas))
  (save-buffer)
  (shell-command (concat ess-sas-submit-command " " ess-sas-root " &"))
  (switch-to-buffer ess-sas-sas))

(defun ess-sas-goto-sas ()
  "Switch to the .sas file"
  (interactive)
  (ess-sas-set)
  (ess-sas-file ess-sas-sas))

(defun ess-sas-revert ()
  "Revert from disk with no questions asked."
  (interactive)
  (ess-sas-set)
  (if (fboundp 'vc-backend-deduce)
      (if (vc-backend-deduce ess-sas-sas) (vc-revert-buffer)
	(revert-buffer t t))
    (if (vc-backend ess-sas-sas) (vc-revert-buffer)
      (revert-buffer t t))))

(defun ess-sas-revert-wisely (ess-sas-arg)
  "Revert from disk if file and buffer last modification times are different."
  (interactive)
  
  (if (not (verify-visited-file-modtime (find-buffer-visiting ess-sas-arg)))
      (ess-sas-revert)))

(defun ess-sas-goto-log ()
  "Switch to the .log file, revert from disk and search for ^ERROR messages."
  (interactive)
  (ess-sas-set)
  
  (if (not (string= ess-sas-log (buffer-name)))
      (progn
	(ess-sas-file ess-sas-log)
	(goto-char 1)))

  (ess-sas-revert-wisely ess-sas-log)
  (setq case-fold-search nil)
  (search-forward-regexp "^ERROR [0-9]+-[0-9]+:\\|^ERROR:\\|_ERROR_=1 _\\|_ERROR_=1$\\|NOTE: MERGE statement has more than one data set with repeats of BY values.\\|NOTE: Variable .* is uninitialized.\\|WARNING: Apparent symbolic reference .* not resolved." nil t))

(defun ess-sas-goto-lst ()
  "Switch to the .lst file and revert from disk."
  (interactive)
  (ess-sas-set)
  (ess-sas-file ess-sas-lst)
  (ess-sas-revert-wisely ess-sas-lst))

(defun ess-sas-goto-txt ()
  "Switch to the .txt file and revert from disk."
  (interactive)
  (ess-sas-set)
  (ess-sas-file ess-sas-txt)
  (ess-sas-revert-wisely ess-sas-txt))

(defun ess-sas-goto-shell ()
  "Switch to the asynchronous shell buffer"
  (interactive)
  (switch-to-buffer "*Async Shell Command*"))

(defvar ess-sas-global-unix-keys nil
  "Non-nil if function keys use Unix-like SAS key definitions in all modes.")
(defun ess-sas-global-unix-keys ()
  "Unix/Mainframe-like SAS key definitions"
  (global-set-key [f2] 'ess-sas-revert)
  (global-set-key [f3] 'ess-sas-submit)
  (global-set-key [f4] 'ess-sas-goto-sas)
  (global-set-key [f5] 'ess-sas-goto-log)
  (global-set-key [f6] 'ess-sas-goto-lst)
  (global-set-key [f7] 'ess-sas-goto-txt)
  (global-set-key [f8] 'ess-sas-goto-shell))

(defvar ess-sas-global-pc-keys nil
  "Non-nil if function keys use PC-like SAS key definitions in all modes.")
(defun ess-sas-global-pc-keys ()
  "PC-like SAS key definitions"
  (global-set-key [f2] 'ess-sas-revert)
  (global-set-key [f3] 'ess-sas-goto-shell)
  (global-set-key [f4] 'ess-sas-goto-txt)
  (global-set-key [f5] 'ess-sas-goto-sas)
  (global-set-key [f6] 'ess-sas-goto-log)
  (global-set-key [f7] 'ess-sas-goto-lst)
  (global-set-key [f8] 'ess-sas-submit))


(defvar ess-sas-local-unix-keys nil
  "Non-nil if function keys use Unix-like SAS key definitions
in SAS-mode and related modes.")
(defun ess-sas-local-unix-keys ()
  "Unix/Mainframe-like SAS key definitions"
  (define-key sas-mode-local-map [f2] 'ess-sas-revert)
  (define-key sas-mode-local-map [f3] 'ess-sas-submit)
  (define-key sas-mode-local-map [f4] 'ess-sas-goto-sas)
  (define-key sas-mode-local-map [f5] 'ess-sas-goto-log)
  (define-key sas-mode-local-map [f6] 'ess-sas-goto-lst)
  (define-key sas-mode-local-map [f7] 'ess-sas-goto-txt)
  (define-key sas-mode-local-map [f8] 'ess-sas-goto-shell))

(defvar ess-sas-local-pc-keys nil
  "Non-nil if function keys use PC-like SAS key definitions
in SAS-mode and related modes.")
(defun ess-sas-local-pc-keys ()
  "PC-like SAS key definitions"
  (define-key sas-mode-local-map [f2] 'ess-sas-revert)
  (define-key sas-mode-local-map [f3] 'ess-sas-goto-shell)
  (define-key sas-mode-local-map [f4] 'ess-sas-goto-txt)
  (define-key sas-mode-local-map [f5] 'ess-sas-goto-sas)
  (define-key sas-mode-local-map [f6] 'ess-sas-goto-log)
  (define-key sas-mode-local-map [f7] 'ess-sas-goto-lst)
  (define-key sas-mode-local-map [f8] 'ess-sas-submit))



;;; Editing SAS-mode files.

;; compute tab stops for use in SAS-mode
(defvar ess-sas-tab-stop-alist
 '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
  "List of tab stop positions used by `tab-to-tab-stop' in SAS-mode")


(defun ess-sas-backward-delete-tab ()
  "Moves the cursor to the previous tab-stop, deleting any characters on the way."
  (interactive)
  
  (let* (;; current-column
	 (ess-sas-column (current-column))
	 ;; remainder of current-column and sas-indent-width
	 (ess-sas-remainder (% ess-sas-column sas-indent-width)))

    (if (not (= ess-sas-column 0)) 
	(progn
	  (if (= ess-sas-remainder 0) 
	      (setq ess-sas-remainder sas-indent-width))
	  
	  (backward-delete-char-untabify ess-sas-remainder t)
	  (move-to-column (- ess-sas-column ess-sas-remainder))))))

(defvar ess-sas-edit-keys-toggle 0
  "0 to bind TAB to 'sas-indent-line.
Positive to bind TAB and C-TAB to 'tab-to-tab-stop and 'ess-sas-backward-delete-tab")
(defun ess-sas-edit-keys-toggle (&optional arg)
  "Toggle TAB key in SAS-mode.
If arg is 0, TAB is 'sas-indent-line.
if arg is positive, TAB is 'tab-to-tab-stop and C-tab is ess-sas-backward-delete-tab.
Without arg, toggle between these options."
  (interactive "P")
  (setq ess-sas-edit-keys-toggle
	(if (null arg) (not ess-sas-edit-keys-toggle)
	  (> (prefix-numeric-value arg) 0)))
  (if ess-sas-edit-keys-toggle
      (progn
	(if (and (equal emacs-major-version 19) (equal emacs-minor-version 28))
	    (define-key sas-mode-local-map [C-tab] 'ess-sas-backward-delete-tab)
	  (define-key sas-mode-local-map [(control tab)] 'ess-sas-backward-delete-tab))
	(define-key sas-mode-local-map "\t" 'tab-to-tab-stop))
    (define-key sas-mode-local-map "\t" 'sas-indent-line)))

(provide 'essa-sas)

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

;;; essa-sas.el ends here
