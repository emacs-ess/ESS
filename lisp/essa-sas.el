;;; essa-sas.el -- ESS local customizations for SAS, part a.

;; Copyright (C) 1997--2000 Rodney Sparapani, A.J. Rossini, 
;; Martin Maechler, Kurt Hornik, and Richard M. Heiberger.

;; Author: Rodney Sparapani <rodney.sparapani@duke.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 17 November 1999
;; Modified: $Date: 2000/02/29 17:17:29 $
;; Version: $Revision: 1.4 $
;; RCS: $Id: essa-sas.el,v 1.4 2000/02/29 17:17:29 ess Exp $

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

; logical window setting for ESS

(if (boundp 'same-window-buffer-names)
    (add-to-list 'same-window-buffer-names "*Async Shell Command*"))


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

(defun ess-sas-tab-stop ()
  "Initializes the the tab-stop-list."
  (interactive)
  
  (let ((ess-sas-tab sas-indent-width))
    (setq tab-stop-list (list sas-indent-width))
    
    (while (and (> sas-indent-width 0) (< ess-sas-tab 120))
      (setq ess-sas-tab (+ ess-sas-tab sas-indent-width))
      (setq tab-stop-list (append tab-stop-list (list ess-sas-tab))))))

;; re-compute tab stops 
(ess-sas-tab-stop)

;; AJR: What the ??? is the above doing in the open code?  It only
;; gets run upon load of essa-sas.el?

(defun ess-sas-backwards-tab ()
  "Moves the cursor to the previous tab-stop."
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

(defun ess-sas-global-unix-keys ()
  "Unix/Mainframe-like SAS key definitions"
  (if (and (equal emacs-major-version 19) (equal emacs-minor-version 28))
      (global-set-key [C-tab] 'ess-sas-backwards-tab)
    (global-set-key [(control tab)] 'ess-sas-backwards-tab))
  (global-set-key [f2] 'ess-sas-revert)
  (global-set-key [f3] 'ess-sas-submit)
  (global-set-key [f4] 'ess-sas-goto-sas)
  (global-set-key [f5] 'ess-sas-goto-log)
  (global-set-key [f6] 'ess-sas-goto-lst)
  (global-set-key [f7] 'ess-sas-goto-txt)
  (global-set-key [f8] 'ess-sas-goto-shell))

(defun ess-sas-global-pc-keys ()
  "PC-like SAS key definitions"
  (if (and (equal emacs-major-version 19) (equal emacs-minor-version 28))
      (global-set-key [C-tab] 'ess-sas-backwards-tab)
    (global-set-key [C-tab] 'ess-sas-backwards-tab))
  (global-set-key [f2] 'ess-sas-revert)
  (global-set-key [f3] 'ess-sas-goto-shell)
  (global-set-key [f4] 'ess-sas-goto-txt)
  (global-set-key [f5] 'ess-sas-goto-sas)
  (global-set-key [f6] 'ess-sas-goto-log)
  (global-set-key [f7] 'ess-sas-goto-lst)
  (global-set-key [f8] 'ess-sas-submit))


