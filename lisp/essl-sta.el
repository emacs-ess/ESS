;;; essl-sta.el --- Stata customization

;; Copyright (C) 1997 Thomas Lumley and A. J. Rossini

;; Author: Thomas Lumley <thomas@biostat.washington.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 2 Nov 1997
;; Modified: $Date: 1999/03/05 20:10:19 $
;; Version: $Revision: 5.11 $
;; RCS: $Id: essl-sta.el,v 5.11 1999/03/05 20:10:19 rossini Exp $
;;
;; Keywords: start up, configuration.

;; This file is part of ESS (Emacs Speaks Statistics).

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
;;; This is based upon Version 0.4 of Stata mode:




;;
;; Stata modes.  Emacs modes for using the Stata statistical package
;; Modified from S-mode, comint-mode
;;
;; (c) thomas lumley 1997 
;;
;;  version 0.4  20/7/97
;;
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



(defconst STA-help-sec-keys-alist
  '((?d . "Description")
    (?e . "Examples")
    (?o . "Options")
    (?s . "Also see:"))
  "Help section keys for S4.
`key' indicates the keystroke to use to search for the section heading
`string' in an Stata help file. `string' is used as part of a
regexp-search, and so specials should be quoted.
")

(defconst STA-help-sec-keys-alist
  '((?a . "\\s *Arguments:")
    (?d . "\\s *Description:")
    (?e . "\\s *Examples:")
    (?n . "\\s *Note:")
    (?r . "\\s *References:")
    (?s . "\\s *See Also:")
    (?v . "\\s *Value[s]?")	;
    )) ;; "Alist of (key . string) pairs for use in section searching."

(defconst ess-help-STA-sec-regex "^[A-Z. ---]+:$"
  "Reg(ular) Ex(pression) of section headers in help file")


(defvar STA-syntax-table nil "Syntax table for S code.")
(if STA-syntax-table
    nil
  (setq STA-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" STA-syntax-table)
  (modify-syntax-entry ?+  "."  STA-syntax-table)
  (modify-syntax-entry ?-  "."  STA-syntax-table)
  (modify-syntax-entry ?=  "."  STA-syntax-table)
  (modify-syntax-entry ?%  "."  STA-syntax-table)
  (modify-syntax-entry ?<  "."  STA-syntax-table)
  (modify-syntax-entry ?>  "."  STA-syntax-table)
  (modify-syntax-entry ?&  "."  STA-syntax-table)
  (modify-syntax-entry ?|  "."  STA-syntax-table)
  (modify-syntax-entry ?\' "\"" STA-syntax-table)
  (modify-syntax-entry ?#  "<"  STA-syntax-table) ; open comment
  (modify-syntax-entry ?\n ">"  STA-syntax-table) ; close comment
  ;;(modify-syntax-entry ?.  "w"  STA-syntax-table) ; "." used in S obj names
  (modify-syntax-entry ?.  "_"  STA-syntax-table) ; see above/below,
					; plus consider separation.
  (modify-syntax-entry ?$  "_"  STA-syntax-table) ; foo.bar$hack is 1 symbol
  (modify-syntax-entry ?_  "."  STA-syntax-table)
  (modify-syntax-entry ?*  "."  STA-syntax-table)
  (modify-syntax-entry ?<  "."  STA-syntax-table)
  (modify-syntax-entry ?>  "."  STA-syntax-table)
  (modify-syntax-entry ?/  "."  STA-syntax-table))


(defvar STA-editing-alist
  '((paragraph-start              . (concat "^$\\|" page-delimiter))
    (paragraph-separate           . (concat "^$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (require-final-newline        . t)
    (comment-start                . "#")
    (comment-start-skip           . "#+ *")
    (comment-column               . 40)
    ;;(comment-indent-function  . 'S-comment-indent)
    ;;(ess-comment-indent           . 'S-comment-indent)
    ;;(ess-indent-line                      . 'S-indent-line)
    ;;(ess-calculate-indent           . 'S-calculate-indent)
    (indent-line-function            . 'S-indent-line)
    (parse-sexp-ignore-comments   . t)
    (ess-set-style                . ess-default-style)
    (ess-local-process-name       . nil)
    ;;(ess-keep-dump-files          . 'ask)
    (ess-mode-syntax-table        . STA-syntax-table)
    (font-lock-defaults           . '(ess-mode-font-lock-keywords
				      nil nil ((?\. . "w")))))
  "General options for editing Stata do and ado source files.")



;; YOU USED TO HAVE TO (with Thomas's version): 
;;;;; Add the following to your .emacs file
;;
;;(autoload 'stata "~/essl-sta.el" "inferior stata mode" t )
;;(autoload 'stata-help "stata" "stata help mode" t)
;;(autoload 'stata-mode "~/essl-sta.el" "stata mode" t)
;;
;;(if (assoc "\\.do$" auto-mode-alist) nil
;;  (setq auto-mode-alist
;;	(append 
;;	 '(("\\.do$" . stata-mode)
;;	   ("\\.ado$" . stata-mode))
;;	 auto-mode-alist)))
;;


;; QUESTIONS TO ASK THOMAS:
;; 1 - are 'help' and 'lookup' the same?
;; 2 - what is the point of the review buffer?
;; 3 - how to quit?

;;
;; NOTE: all of Thomas's functions have been left here, to be removed
;; or merged into real locations as we work on this.
;;


;;;;;;;;; Things to change 

(defvar stata-switches "-q" 
  "*Switches to apply to stata invocation")

(defvar stata-profile "~/.stataprofile"  "File to read on startup (nil for no file")

;;;;;;;;;;;;;;; 

;;(require 'comint)

(defun stata-help (the-subject) "Stata help in other buffer"
  (interactive "sHelp on: ")
  (let* ((stata-process (get-process "stata"))
	 (stata-buffer (process-buffer stata-process))
	 oldpf oldpb oldpm)
    (set-buffer stata-buffer)
    (setq oldpf (process-filter stata-process))
    (setq oldpb (process-buffer stata-process))
    (setq oldpm (marker-position (process-mark stata-process)))
    (save-excursion
      (if stata-process nil (error "Stata is not running."))
      (beginning-of-line)
      (if (looking-at ". ") nil  (error "Stata not ready."))
      (save-excursion
	(set-process-buffer stata-process (get-buffer-create "*stata help*"))
	(set-buffer "*stata help*")
	(setq buffer-read-only nil)
	(set-process-filter stata-process 'ordinary-insertion-filter)
	(erase-buffer)
	(process-send-string stata-process "help ")
	(process-send-string stata-process the-subject)
	(process-send-string stata-process "\n")
	(stata-prompt-wait stata-process)
	(stata-help-mode)
	(set-buffer stata-buffer)
	(set-process-buffer stata-process oldpb)
	(set-process-filter stata-process oldpf)
	(set-marker (process-mark stata-process) oldpm)))
    (display-buffer "*stata help*")))
  
(defun stata-lookup (the-subject) "Stata lookup in other buffer"
  (interactive "sLook up: ")
  (let* ((stata-process (get-process "stata"))
	 (stata-buffer (process-buffer stata-process))
	 oldpf oldpb oldpm)
    (set-buffer stata-buffer)
    (setq oldpf (process-filter stata-process))
    (setq oldpb (process-buffer stata-process))
    (setq oldpm (marker-position (process-mark stata-process)))
    (save-excursion
      (if stata-process nil (error "Stata is not running."))
      (beginning-of-line)
      (if (looking-at ". ") nil  (error "Stata not ready."))
      (save-excursion
	(set-process-buffer stata-process (get-buffer-create "*stata help*"))
	(set-buffer "*stata help*")
	(setq buffer-read-only nil)
	(set-process-filter stata-process 'ordinary-insertion-filter)
	(erase-buffer)
	(process-send-string stata-process "lookup ")
	(process-send-string stata-process the-subject)
	(process-send-string stata-process "\n")
	(stata-prompt-wait stata-process)
	(stata-help-mode)
	(set-buffer stata-buffer)
	(set-process-buffer stata-process oldpb)
	(set-process-filter stata-process oldpf)
	(set-marker (process-mark stata-process) oldpm)))
    (display-buffer "*stata help*")))
  
(defun stata-variables () "Stata variable list in other buffer"
  (interactive)
  (let* ((stata-process (get-process "stata"))
	 (stata-buffer (if stata-process
			   (process-buffer stata-process)
			 (error "Stata is not running.")))
	 oldpf oldpb oldpm)
    (set-buffer stata-buffer)
    (setq oldpf (process-filter stata-process))
    (setq oldpb (process-buffer stata-process))
    (setq oldpm (marker-position (process-mark stata-process)))
    (save-excursion
      (if stata-process nil (error "Stata is not running."))
      (beginning-of-line)
      (if (looking-at ". ") nil  (error "Stata not ready."))
       (save-excursion
	(set-process-buffer stata-process
			    (get-buffer-create "*stata variables*"))
	(set-process-filter stata-process 'ordinary-insertion-filter)
	(set-buffer "*stata variables*")
	(setq buffer-read-only nil)
	(erase-buffer)
	(process-send-string stata-process "desc \n ")
	(stata-prompt-wait stata-process)
	(setq buffer-read-only t)
	(set-buffer stata-buffer)
	(set-process-buffer stata-process oldpb)
	(set-marker (process-mark stata-process) oldpm)
	(set-process-filter stata-process oldpf)))
    (display-buffer "*stata variables*")
    (goto-char (point-max))))

(defun stata-review-window ()
  (interactive)
  (display-buffer "*stata review*"))

(defun stata-rehelp () 
  (interactive)
  (stata-help (current-word)))

(defun ordinary-insertion-filter (proc string)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point) (process-mark proc)))
	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))

      
;;;; <IGNORE>
;;; This doesn't do anything at the moment.  I have vague plans of
;;; implementing a menu interface using emacs
;;;
(defun stata-watch-for-menu-filter (proc string)
  (if (string-match "^!!!window!!!" string)
      (stata-handle-menu-code proc string)
    (comint-output-filter proc string)))

(defun stata-handle-menu-code (proc string)
   (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point)
			  (process-mark proc)))
	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	    (goto-char (process-mark proc))
	    (insert "Handling menu code\n")
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))

;;;; </IGNORE>
    
(defun stata-add-to-review-buffer (string)
"Adds input to review buffer"
(save-excursion
  (set-buffer (get-buffer-create "*stata review*"))
  (goto-char (point-max))
  (insert string)))

(defun stata-prompt-wait (proc &optional start-of-output)
  "Wait for a prompt to appear at BOL of current buffer
PROC is the stata process. Does not change point"
  (if start-of-output nil (setq start-of-output (point-min)))
  (save-excursion
    (while (progn
	     ;; get output if there is some ready
	     (accept-process-output proc 0 50) 
	     (goto-char (marker-position (process-mark proc)))
	     (beginning-of-line)
	     (if (< (point) start-of-output) (goto-char start-of-output))
	     (not (looking-at "^. "))))))

;;(defvar inferior-stata-mode-map nil
;;  "Keymap for Stata mode")

;;(setq inferior-stata-mode-map (cons 'keymap comint-mode-map))
;;(define-key inferior-stata-mode-map "\M-\t" 'comint-replace-by-expanded-filename)
;;(define-key inferior-stata-mode-map "\C-c\C-v" 'stata-variables)
;;(define-key inferior-stata-mode-map "\C-c\C-h" 'stata-help)
;;(define-key inferior-stata-mode-map "\C-c\C-u" 'stata-lookup)
;;(define-key inferior-stata-mode-map "\C-c\C-r"   'stata-review-window)
;;(define-key inferior-stata-mode-map [menu-bar stata] 
;;  (cons "Stata" (make-sparse-keymap "Stata")))
;;(define-key inferior-stata-mode-map [menu-bar stata statahelp]
;;  '("Help on..." . stata-help))
;;(define-key inferior-stata-mode-map [menu-bar stata lookup]
;;  '("Look up..." . stata-lookup))
;;(define-key inferior-stata-mode-map [menu-bar stata variables]
;;  '("Variables" . stata-variables))
;;(define-key inferior-stata-mode-map [menu-bar stata review]
;;  '("Review" . stata-review-window))


;;(defvar stata-mode-map nil
;;  "Keymap for Stata mode")
  
;;(setq stata-mode-map (make-sparse-keymap))
;;(define-key stata-mode-map "\C-c\C-r"    'stata-eval-region)
;;(define-key stata-mode-map "\C-c\M-r" 'stata-eval-region-and-go)
;;(define-key stata-mode-map "\C-c\C-b"    'stata-eval-buffer)
;;(define-key stata-mode-map "\C-c\M-b" 'stata-eval-buffer-and-go)
;;(define-key stata-mode-map "\C-c\C-f"    'stata-eval-function)
;;(define-key stata-mode-map "\C-c\C-n"     'stata-eval-line-and-next-line)
;;(define-key stata-mode-map "\C-c\C-j"    'stata-eval-line)
;;(define-key stata-mode-map "\C-c\C-r"   'stata-review-window)
;;(define-key stata-mode-map "\C-c\M-j" 'stata-eval-line-and-go)
;;(define-key stata-mode-map "\C-c\C-y"    'stata-switch-to-stata)
;;(define-key stata-mode-map "\C-c\C-z" 'stata-switch-to-end-of-stata)
;;;;(define-key stata-mode-map "\C-c\C-l"    'stata-load-file)
;;(define-key stata-mode-map "\C-c\C-h"    'stata-help)
;;(define-key stata-mode-map "\C-c\C-v"    'stata-variables)
;;(define-key stata-mode-map "\M-\t" 'comint-replace-by-expanded-filename)
;;(define-key stata-mode-map "\177" 'backward-delete-char-untabify)
;;(define-key stata-mode-map "\C-c\C-u" 'stata-lookup)
;;(define-key stata-mode-map [menu-bar stata] 
;;  (cons "Stata" (make-sparse-keymap "Stata")))
;;(define-key stata-mode-map [menu-bar stata lookup]
;;  '("Look up..." . stata-lookup))
;;(define-key stata-mode-map [menu-bar stata statahelp]
;;  '("Help on..." . stata-help))
;;(define-key stata-mode-map [menu-bar stata variables]
;;  '("Variables" . stata-variables))
;;(define-key stata-mode-map [menu-bar stata review]
;;  '("Review" . stata-review-window))
;;(define-key stata-mode-map [menu-bar stata eval-line]
;;  '("Eval line" . stata-eval-line))
;;(define-key stata-mode-map [menu-bar stata eval-next]
;;  '("Eval line and next line" . stata-eval-line-and-next-line))
;;(define-key stata-mode-map [menu-bar stata eval-go]
;;  '("Eval line and go" . stata-eval-line-and-go))
;;(define-key stata-mode-map [menu-bar stata eval-buff]
;;  '("Eval buffer" . stata-eval-buffer))
;;(define-key stata-mode-map [menu-bar stata eval-buff-go]
;;  '("Eval buffer and go" . stata-eval-buffer-and-go))
;;(define-key stata-mode-map [menu-bar stata to-stata]
;;  '("Switch to stata" . stata-switch-to-stata))
;;
;;
;;
;;(defvar stata-help-mode-map nil)
;;(setq stata-help-mode-map (cons 'keymap help-mode-map))
;;(define-key stata-help-mode-map [mouse-2] 'stata-rehelp)
;;(define-key stata-help-mode-map "\C-c\C-r" 'stata-rehelp)
;;(define-key stata-help-mode-map "\C-c\C-h" 'stata-help)
;;(define-key stata-help-mode-map [menu-bar stata] 
;;  (cons "Stata" (make-sparse-keymap "Stata")))
;;(define-key stata-help-mode-map [menu-bar stata statahelp]
;;  '("Help on..." . stata-help))
;;(define-key stata-help-mode-map [menu-bar stata rehelp]
;;  '("rehelp (hyperlink)" . stata-rehelp))
;;


;;(defun inferior-stata-mode ()
;;"Major mode for running Stata. Based on comint-mode.
;;Features include Help (\\[stata-help]), Review (\\[stata-review-window]) and
;;Variables (\\[stata-variables]) mimicking the help, review and 
;;variables windows of Stata for Windows
;;\\{inferior-stata-mode-map}"
;;  (interactive)
;;  (make-comint "stata" "stata" 
;;	       (and stata-profile
;;		    (or (file-exists-p stata-profile)
;;			(null (message "Startup file %s not found."
;;				       stata-profile))) stata-profile)
;;	       stata-switches)
;;  (switch-to-buffer "*stata*" )
;;  (setq comint-process-echoes t)
;;  (set-process-filter (get-process "stata") 'stata-watch-for-menu-filter)
;;  (setq comint-input-filter-functions
;;	(cons 'stata-add-to-review-buffer comint-input-filter-functions))
;;  (save-excursion
;;    (set-buffer (get-buffer-create "*stata review*"))
;;    (stata-mode))
;;  (setq major-mode 'inferior-stata-mode)
;;  (setq mode-name "inferior Stata")
;;  (use-local-map inferior-stata-mode-map))
;;
;;(defun stata ()
;;  (interactive)
;;  (inferior-stata-mode))
;;
;;(defun stata-help-mode ()
;;"Major mode for displaying Stata help in a read-only
;;buffer. Active commands are Help (\\[stata-help]) and
;;hyperlink (\\[stata-rehelp] or mouse-2)"
;;  (interactive)
;;  (setq major-mode 'stata-help-mode)
;;  (setq mode-name "Stata help")
;;  (use-local-map stata-help-mode-map)
;;  (setq buffer-read-only t))
;;
;;
;;(defun stata-mode ()
;;"Major mode for editing Stata files. Commands for sending lines to
;;Stata (\\[stata-eval-line], \\[stata-eval-line-and-go],
;;\\[stata-eval-line-and-next-line]) 
;;and for displaying Stata help (\\[stata-help]), variables (\\[stata-variables])
;; and review window (\\[stata-review-window])
;;\\{stata-mode-map}"
;;  (interactive)
;;  (kill-all-local-variables)
;;  (setq major-mode 'stata-mode)
;;  (setq mode-name "Stata")
;;  (use-local-map stata-mode-map))
;;
;;
(defun stata-eval-region (start end)
  "Send the current region to the inferior stata process."
  (interactive "r")
  (process-send-region "stata" start end)
  (process-send-string "stata" "\n"))



(defun stata-eval-buffer ()
  "Send the current buffer to the inferior stata process."
  (interactive)
  (stata-eval-region (point-min) (point-max)))

(defun stata-eval-line ()
  "Send the current line to the inferior stata process."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (stata-eval-region (point) end))))

(defun stata-eval-line-and-next-line ()
  "Evaluate the current line  and move to the next line."
  ;; From an idea by Rod Ball (rod@marcam.dsir.govt.nz)
  (interactive)
  (display-buffer (process-buffer (get-process "stata")))
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      ;; RDB modified to go to end of S buffer so user can see result
      ;;(stata-eval-visibly (buffer-substring (point) end) nil t)))
      (stata-eval-region (point) end))) 
  (next-line 1))


(defun stata-eval-region-and-go (start end )
  "Send the current region to the inferior S and switch to the process buffer."
  (interactive "r\nP")
  (stata-eval-region start end)
  (stata-switch-to-stata t))

(defun stata-eval-buffer-and-go ()
  "Send the current buffer to the inferior stata and switch to the process buffer."
  (interactive)
  (stata-eval-buffer)
  (stata-switch-to-stata t))


(defun stata-eval-line-and-go ()
  "Send the current line to the inferior stata process and switch to the
process buffer."
  (interactive)
  (stata-eval-line)
  (stata-switch-to-stata t))


(defun stata-switch-to-stata (eob-p)
  "Switch to the current inferior stata process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (let (stata-process (get-process "stata"))
    (if stata-process 
	(progn
	  (switch-to-buffer (process-buffer stata-process))
	  (if eob-p (goto-char (point-max))))
      (progn 
	(message "No inferior stata process")
	(ding)))))

(defun stata-switch-to-end-of-stata nil
  "Switch to the end of the inferior stata process buffer."
  (interactive)
  (stata-switch-to-stata t))



(provide 'essl-sta)
