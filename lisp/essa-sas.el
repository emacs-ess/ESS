;;; essa-sas.el -- Possible local customizations for SAS with ESS.

;; Copyright (C) 1997--2000 Rodney Sparapani, A.J. Rossini, 
;; Martin Maechler, Kurt Hornik, and Richard M. Heiberger.

;; Author: Rodney Sparapani <spara002@duke.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 17 November 1999
;; Modified: $Date: 1999/11/22 21:56:39 $
;; Version: $Revision: 1.1 $
;; RCS: $Id: essa-sas.el,v 1.1 1999/11/22 21:56:39 ess Exp $

;; Keywords: editing and process modes.

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

;;; Code:

;; Variables
;;
(setq same-window-buffer-names
      '("*scratch" "*info*" "*Help*" "*Buffer List*"
	"*Async Shell Command*" "*Apropos*" "*Directory*"
	"*Messages*" "*ESS*"))

;;
;; Modes
;;
;; ESS
;;(require 'ess-site)
;;(setq ess-ask-for-ess-directory nil)
;;(setq ess-fancy-comments nil)
;;(setq ess-default-style 'CLB)
;;(setq sas-indent-width 0)

;;(setq ess-tab-always-indent nil)
;;(setq ess-indent-level 0)
;;(setq ess-continued-statement-offset 0)

;;
;; SAS
;; 
(global-font-lock-mode t)
(setq font-lock-face-attributes 
      '(                                            ;   S      TeX
	(font-lock-comment-face       "Firebrick")  ; #...    %...
	(font-lock-string-face        "SeaGreen")   ; "..."   "..."
	(font-lock-keyword-face       "MediumBlue") ; if      \end
	(font-lock-function-name-face "VioletRed")  ; talk<-  {center}
	(font-lock-variable-name-face "Blue")       ; xv
	(font-lock-type-face          "Goldenrod")  ; T,F       ?
	(font-lock-reference-face     "Magenta")))  ; <-      {eq1}

(defvar esas-root "."
  "Root of the file to perform operations on.")

(defvar esas-dir "."
  "Directory where .sas, .log, .lst and .txt files are stored")

(defvar esas-sas "."
  "The .sas file to perform operations on.")

(defvar esas-log "."
  "The .log file to perform operations on.")

(defvar esas-lst "."
  "The .lst file to perform operations on.")

(defvar esas-txt "."
  "The .txt file to perform operations on.")

(defun esas-file (esas-arg)
  "Create buffer and associate with the appopriate file"
  
  (if (not (get-file-buffer esas-arg))
      (progn
	(create-file-buffer esas-arg)
	(switch-to-buffer esas-arg)
	(set-visited-file-name (concat esas-dir esas-arg) t)
	(esas-revert)
	)
    (switch-to-buffer esas-arg)))

(defun esas-set ()
  "Set esas-root, esas-dir, esas-sas, esas-log, esas-lst and esas-txt"
  (if (or (string= ".sas" (substring (buffer-name) -4)) 
	  (string= ".log" (substring (buffer-name) -4))
	  (string= ".lst" (substring (buffer-name) -4))
	  (string= ".txt" (substring (buffer-name) -4))) 
      (progn
	(setq esas-root (substring (buffer-name) 0 -4))
	(setq esas-dir (file-name-directory (buffer-file-name)))
	(setq esas-sas (concat esas-root ".sas"))
	(setq esas-log (concat esas-root ".log"))
	(setq esas-lst (concat esas-root ".lst"))
	(setq esas-txt (concat esas-root ".txt")))))

(defun esas-submit ()
  "Save the .sas file and submit to shell."
  (interactive)
  (esas-set)
  (if (not (string= esas-sas (buffer-name))) (esas-file esas-sas))
  (save-buffer)
  (shell-command (concat "sas " esas-root " &"))
  ;;un-comment the following line for emacs for Windows
  ;;(shell-command (concat "c:\\progra~1\\sas\\sas.exe " esas-root " -nosplash &"))
  (switch-to-buffer esas-sas))

(defun esas-goto-sas ()
  "Switch to the .sas file"
  (interactive)
  (esas-set)
  (esas-file esas-sas))

(defun esas-revert ()
  "Revert from disk with no questions asked."
  (interactive)
  (revert-buffer t t))

(defun esas-goto-log ()
  "Switch to the .log file, revert from disk and search for ^ERROR messages."
  (interactive)
  (esas-set)
  
  (if (not (string= esas-log (buffer-name)))
      (progn
	(esas-file esas-log)
	(goto-char 1)))

  (esas-revert)
  (setq case-fold-search nil)
  (search-forward-regexp "^ERROR [0-9]+-[0-9]+:\\|^ERROR:\\|_ERROR_=1 _N_=\\|NOTE: MERGE statement has more than one data set with repeats of BY values.\\|NOTE: Variable .* is uninitialized.\\|WARNING: Apparent symbolic reference .* not resolved." 
			 nil
			 t))

(defun esas-goto-lst ()
  "Switch to the .lst file and revert from disk."
  (interactive)
  (esas-set)
  (esas-file esas-lst)
  (esas-revert))

(defun esas-goto-txt ()
  "Switch to the .txt file and revert from disk."
  (interactive)
  (esas-set)
  (esas-file esas-txt)
  (esas-revert))

(defun esas-goto-shell ()
  "Switch to the asynchronous shell buffer"
  (interactive)
  (switch-to-buffer "*Async Shell Command*"))

(global-set-key "\C-[\C-[" 'viper-mode)
(global-set-key [f2] 'esas-revert)
;Unix/Mainframe-like SAS key definitions
(global-set-key [f3] 'esas-submit)
(global-set-key [f4] 'esas-goto-sas)
(global-set-key [f5] 'esas-goto-log)
(global-set-key [f6] 'esas-goto-lst)
(global-set-key [f7] 'esas-goto-txt)
(global-set-key [f8] 'esas-goto-shell)
;PC-like SAS key definitions
;(global-set-key [f3] 'esas-goto-shell)
;(global-set-key [f4] 'esas-goto-txt)
;(global-set-key [f5] 'esas-goto-sas)
;(global-set-key [f6] 'esas-goto-log)
;(global-set-key [f7] 'esas-goto-lst)
;(global-set-key [f8] 'esas-submit)
