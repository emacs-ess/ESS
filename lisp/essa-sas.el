;;; essa-sas.el -- ESS local customizations for SAS, part a.

;; Copyright (C) 1997--2001 Rodney Sparapani, A.J. Rossini, 
;; Martin Maechler, Kurt Hornik, and Richard M. Heiberger.

;; Author: Rodney Sparapani <rsparapa@mcw.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 17 November 1999
;; Modified: $Date: 2001/05/02 19:40:55 $
;; Version: $Revision: 1.19 $
;; RCS: $Id: essa-sas.el,v 1.19 2001/05/02 19:40:55 ess Exp $

;; Keywords: ESS, ess, SAS, sas, BATCH, batch 

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

;;; Table of Contents
;;; Section 1:  Variable Definitions
;;; Section 2:  Function Definitions
;;; Section 3:  Key Definitions


;;; Section 1:  Variable Definitions

(defcustom ess-sas-data-view-options 
    (if (eq system-type 'windows-nt) "-noenhancededitor -nosysin -log NUL:"
	"-nodms -nosysin -log /dev/null")
    "*The options necessary for your enviromment and your operating system."
    :group 'ess-sas
    :type  'string
)

(defvar ess-sas-file-path "."
    "Full path-name of the sas file to perform operations on.")

(defcustom ess-sas-submit-command sas-program
    "*Command to invoke SAS in batch; may differ from interactive SAS command."
    :group 'ess-sas
)

(defcustom ess-sas-submit-post-command (if (w32-shell-dos-semantics) " " "&")
    "*Command-line statement to post-modify SAS invocation, e.g. &"
    :group 'ess-sas
    :type  'string
)

(defcustom ess-sas-submit-pre-command (if (w32-shell-dos-semantics) "start" " ")
    "*Command-line statement to pre-modify SAS invocation, e.g. start"
    :group 'ess-sas
    :type  'string
)


(defvar ess-sas-submit-method 
  (if (and (eq system-type 'windows-nt)
	   (not (w32-shell-dos-semantics)))
      'sh
    system-type)
  "Method used by `ess-sas-submit'.
The default is based on the value of the emacs variable `system-type'
and, on Windows machines, the function `w32-shell-dos-semantics'.
'Apple-Macintosh  AppleScript
'windows-nt       if *shell* runs MS-DOS semantics
'iESS             inferior ESS, may be local or remote
'sh and other     if *shell* runs sh or bash

Windows users running an MS-DOS shell will get 'windows-nt by default.

Windows users running bash in their *shell* buffer will get 'sh by default.

Unix users will get `system-type', which behaves like 'sh, by default.

Users accessing a remote machine with `telnet', `rlogin', `ssh',
or `ESS-elsewhere' should use
   (setq-default ess-sas-submit-method 'iESS)
in ess-site.el or in .emacs.")


(defcustom ess-sas-suffix-1 "txt"
    "*The ess-sas-suffix-1 file to perform operations on."
    :group 'ess-sas
    :type  'string
)

(defcustom ess-sas-suffix-2 "csv"
    "*The ess-sas-suffix-2 file to perform operations on."
    :group 'ess-sas
    :type  'string
)

(defcustom ess-sleep-for 5
    "*Default for ess-sas-submit-sh is to sleep for 5 seconds."
    :group 'ess-sas
    :type  'number
)

(defvar ess-sas-tab-stop-alist
 '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
  "List of tab stop positions used by `tab-to-tab-stop' in `SAS-mode'.")


;;; Section 2:  Function Definitions

(defun ess-add-ess-process ()
  "Execute this command from within a buffer running a process to add
the process to `ess-process-name-alist'.  This command will normally
be run in a telnet buffer connected to another computer or in a shell
or comint buffer on the local computer."
  (interactive)
  (let ((ess-process-name (process-name (get-buffer-process (buffer-name)))))
    (setq ess-process-name-list
	  (cons (cons ess-process-name nil) ess-process-name-list))
    (setq ess-current-process-name ess-process-name)))

(defun ess-exit-notify-sh (string)
  "Detect completion or failure of submitted job and notify the user."
  (let* ((exit-done "\\[[0-9]+\\]\\ *\\+*\\ *\\(Exit\\|Done\\).*\\.sas")
	 (beg (string-match exit-done string)))
    (if beg
	(message (substring string beg (match-end 0))))))


(defun ess-revert-wisely ()
  "Revert from disk if file and buffer last modification times are different."
  (interactive)
  
  (if (not(verify-visited-file-modtime (current-buffer)))
      (cond ((and (fboundp 'vc-backend-deduce)
		  (vc-backend-deduce (buffer-file-name))) (vc-revert-buffer))
	    ((and (fboundp 'vc-backend)
		  (vc-backend (buffer-file-name))) (vc-revert-buffer))
	    (t (revert-buffer t t)))))

(defun ess-sas-append-log ()
    "Append ess-temp.log to the current .log file."
    (interactive)
    (ess-sas-file "log" 'revert)
    (goto-char (point-max))
    (insert-file-contents "ess-temp.log")
    (save-buffer))

(defun ess-sas-append-lst ()
    "Append ess-temp.lst to the current .lst file."
    (interactive)
    (ess-sas-file "lst" 'revert)
    (goto-char (point-max))
    (insert-file-contents "ess-temp.lst")
    (save-buffer))

(defun ess-sas-backward-delete-tab ()
  "Moves the cursor to the previous tab-stop, deleting any characters
on the way."
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

(defun ess-sas-data-list ()
  "Parse .sas file and return a list of permanent datasets."
  (interactive)

  (save-excursion (let ((search-match nil)
    (search-match-begin)
    (search-match-end)
    (search-match-list (list)))

    (goto-char (point-min))

    (while (search-forward-regexp "\\(^\\|[ \t]\\)\\([a-zA-Z_][a-zA-Z_0-9]*\\.[a-zA-Z_][a-zA-Z_0-9]*\\)" nil t)
	(setq search-match-end (point))
	(setq search-match-begin (match-beginning 2))
	(setq search-match (downcase (buffer-string search-match-begin search-match-end)))

	(if (not (and (equal (substring search-match 0 5) "first.") (equal (substring search-match 0 4) "last."))) (progn
	    (add-to-list 'search-match-list search-match)
	;;    (message search-match-list)
	))
    )
    search-match-list))
)

(defun ess-sas-data-view ()
  "Open a dataset for viewing with PROC FSVIEW."
    (interactive)
    (save-excursion 
      (if (get-buffer "*shell*") (set-buffer "*shell*")
          (shell))
	(insert (concat ess-sas-submit-pre-command " " ess-sas-submit-command 
	    " -initstmt \"proc fsview data=" (read-string "SAS Dataset: ")
	    "; run;\" " ess-sas-data-view-options " " ess-sas-submit-post-command))
    (comint-send-input)
    )
)

(defun ess-sas-file (suffix &optional revert)
  "Find a file associated with the SAS file and revert if necessary."
  (let* ((tail (if (fboundp 'file-name-extension) (file-name-extension (buffer-name))
		 (substring (buffer-name) -3)))
	 (tail-in-tail-list (member tail (list "sas" "log" "lst"
			     ess-sas-suffix-1 ess-sas-suffix-2)))
	 (root (if tail-in-tail-list (expand-file-name (buffer-name))
		 ess-sas-file-path))
	 (ess-sas-arg (concat (file-name-sans-extension root) "." suffix))
	 (ess-sas-buf (get-file-buffer ess-sas-arg)))
    (if (equal tail suffix) (if revert (ess-revert-wisely))
	(if (not ess-sas-buf) (find-file ess-sas-arg)
	    (switch-to-buffer ess-sas-buf)
	    (if revert (ess-revert-wisely))))))

(defun ess-sas-file-path ()
 "Define the variable `ess-sas-file-path' to be the file in the current buffer"
  (interactive)
  (setq ess-sas-file-path (expand-file-name (buffer-name))))

(defun ess-sas-goto-file-1 ()
  "Switch to ess-sas-file-1 and revert from disk."
  (interactive)
  (ess-sas-file ess-sas-suffix-1 'revert))

(defun ess-sas-goto-file-2 ()
  "Switch to ess-sas-file-2 and revert from disk."
  (interactive)
  (ess-sas-file ess-sas-suffix-2 'revert))

(defun ess-sas-goto-log ()
  "Switch to the .log file, revert from disk and search for error messages."
  (interactive)
  (ess-sas-file "log" 'revert)

  (let ((ess-sas-error (concat "^ERROR [0-9]+-[0-9]+:\\|^ERROR:\\|_ERROR_=1 _\\|_ERROR_=1[ ]?$"
    "\\|NOTE: MERGE statement has more than one data set with repeats of BY values."
    "\\|NOTE: Variable .* is uninitialized."
    "\\|WARNING: Apparent symbolic reference .* not resolved."
    "\\|Bus Error In Task\\|Segmentation Violation In Task")))

  (if (not (search-forward-regexp ess-sas-error nil t)) 
        (if (search-backward-regexp ess-sas-error nil t) 
            (progn
                (goto-char (point-min))
                (search-forward-regexp ess-sas-error nil t)
            )
        )
    ))
)

(defun ess-sas-goto-lst ()
  "Switch to the .lst file and revert from disk."
  (interactive)
  (ess-sas-file "lst" 'revert))

(defun ess-sas-goto-sas ()
  "Switch to the .sas file."
  (interactive)
  (ess-sas-file "sas"))

;;
;;(defun ess-sas-goto-shell ()
;; "Set variable `ess-sas-file-path' to file in current buffer and goto *shell*"
;;  (interactive)
;;  (ess-sas-file-path)
;;  (switch-to-buffer "*shell*")
;;)


(defun ess-sas-submit ()
  "Save the .sas file and submit to shell using a function that
depends on the value of  `ess-sas-submit-method'"
  (interactive)
  (ess-sas-goto-sas)
  (save-buffer)
  (ess-sas-file-path)
  (cond
   ((eq ess-sas-submit-method 'Apple-Macintosh) 
	(ess-sas-submit-mac ess-sas-submit-command))
   ((eq ess-sas-submit-method 'windows-nt) 
	(ess-sas-submit-windows ess-sas-submit-command))
   ((eq ess-sas-submit-method 'iESS) 
	(ess-sas-submit-iESS ess-sas-submit-command))
   ((eq ess-sas-submit-method 'sh) 
	(ess-sas-submit-sh ess-sas-submit-command)) 
   (t (ess-sas-submit-sh ess-sas-submit-command)))
  (ess-sas-goto-sas))

(defun ess-sas-submit-iESS (ess-sas-arg)
  "iESS
Submit a batch job in an inferior-ESS buffer.  The buffer should
(1) have telnet access and be running a shell on a remote machine
or
(2) be running a shell on the local machine.

The user can telnet to the remote computer and then declare the
*telnet-buffer* to be an inferior ESS buffer with the `ess-add-ess-process'
command.  When using a remote computer, the .sas file must live on the
remote computer and be accessed through `ange-ftp'.  When
`ess-sas-submit' saves a file, it is therefore saved on the remote
computer.  The various functions such as `ess-sas-goto-lst' retrieve
their files from the remote computer.  Local copies of the .sas .lst
.log and others may be made manually with `write-buffer'."
  (ess-force-buffer-current "Process to load into: ")
  (ess-eval-linewise (concat "cd " default-directory))
  (ess-eval-linewise (concat ess-sas-arg " " (buffer-name) " &")))

(defun ess-sas-submit-mac (ess-sas-arg)
  "Mac
`ess-sas-arg' is assumed to be the AppleScript command
\"invoke SAS using program file\"."
  (do-applescript (concat ess-sas-arg
			  " \""
			  (unix-filename-to-mac default-directory)
			  (buffer-name) "\"")))

(defun ess-sas-submit-region ()
    "Write region to temporary file, and submit to SAS."
    (interactive)
    (write-region (region-beginning) (region-end) "ess-temp.sas")
    (save-excursion 
      (if (get-buffer "*shell*") (set-buffer "*shell*")
          (shell))
      (insert (concat ess-sas-submit-pre-command " " ess-sas-submit-command 
          " ess-temp.sas " ess-sas-submit-post-command))
      (comint-send-input)
    )
)

(defun ess-sas-submit-sh (ess-sas-arg)
  "Unix or bash in the *shell* buffer.
Multiple processing is supported on this platform.
SAS may not be found in your PATH.  You can alter your PATH to include
SAS or you can specify the PATHNAME (PATHNAME can NOT contain spaces),
i.e. let `ess-sas-arg' be your local equivalent of
\"/usr/local/sas612/sas\"."
    (shell)
    (add-hook 'comint-output-filter-functions 'ess-exit-notify-sh) ;; 19.28
                                          ;; nil t) works for newer emacsen
    (insert "cd " (file-name-directory ess-sas-file-path))
    (comint-send-input)
    (insert ess-sas-arg " " ess-sas-file-path " &")
    (comint-send-input)
    (if (featurep 'xemacs) (sleep-for ess-sleep-for)
       (sleep-for 0 (truncate (* ess-sleep-for 1000)))
    )
    (comint-send-input))


(defun ess-sas-submit-windows (ess-sas-arg)
  "Windows using MS-DOS prompt in the *shell* buffer.
Multiple processing is supported on this platform.
On most Windows installations, SAS will not be found in your
PATH.  You can set `ess-sas-submit-command' to 
\"sas -icon -rsasuser\" and alter your PATH to include SAS, i.e.

SET PATH=%PATH%;C:\\Program Files\\SAS

Or you can specify the PATHNAME directly (you must escape 
spaces by enclosing the string in \\\"'s), i.e. let 
`ess-sas-submit-command' be \"\\\"C:\\Program Files\\SAS\\sas.exe\\\"\".
Keep in mind that the maximum command line length in MS-DOS is
127 characters so altering your PATH is preferable."
    (shell)
    (if (string-equal ":" (substring ess-sas-file-path 1 2)) 
	(progn
		(insert (substring ess-sas-file-path 0 2))
		(comint-send-input)
	)
    )
    (insert "cd \"" (convert-standard-filename 
	(file-name-directory ess-sas-file-path)) "\"")
    (comint-send-input)
    (insert "start " ess-sas-arg " -sysin \"" 
	(convert-standard-filename ess-sas-file-path) "\"")
    (comint-send-input))


(defun ess-sas-toggle-sas-mode ()
  "Toggle SAS-mode for .log files."
  (interactive)

  (ess-sas-file "log")
  (kill-buffer nil)

  (if (assoc "\\.log\\'" auto-mode-alist) 
    (setq auto-mode-alist (delete '("\\.log\\'" . SAS-mode) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.log\\'" . SAS-mode)) auto-mode-alist)))

  (if (assoc "\\.LOG\\'" auto-mode-alist) 
    (setq auto-mode-alist (delete '("\\.LOG\\'" . SAS-mode) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.LOG\\'" . SAS-mode)) auto-mode-alist)))

  (ess-sas-file "log")
)


;;; Section 3:  Key Definitions

(defvar ess-sas-edit-keys-toggle 0
  "0 to bind TAB to `sas-indent-line'.
  Positive to bind TAB and C-TAB to `tab-to-tab-stop' and
`ess-sas-backward-delete-tab'.")

(defun ess-sas-edit-keys-toggle (&optional arg)
  "Toggle TAB key in `SAS-mode'.
If arg is 0, TAB is `sas-indent-line'.
If arg is positive, TAB is `tab-to-tab-stop' and C-tab is
`ess-sas-backward-delete-tab'.
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

(defvar ess-sas-global-pc-keys nil
  "Non-nil if function keys use PC-like SAS key definitions in all modes.")
(defun ess-sas-global-pc-keys ()
  "PC-like SAS key definitions"
  (global-set-key [f2] 'ess-revert-wisely)
  (global-set-key [f3] 'shell)
  (global-set-key [f4] 'ess-sas-goto-file-1)
  (global-set-key [f5] 'ess-sas-goto-sas)
  (global-set-key [f6] 'ess-sas-goto-log)
  (global-set-key [(control f6)] 'ess-sas-append-log)
  (global-set-key [f7] 'ess-sas-goto-lst)
  (global-set-key [(control f7)] 'ess-sas-append-lst)
  (global-set-key [f8] 'ess-sas-submit)
  (global-set-key [(control f8)] 'ess-sas-submit-region)
  (global-set-key [f9] 'ess-sas-data-view)
  (global-set-key [f10] 'ess-sas-toggle-sas-mode)
  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path))

(defvar ess-sas-global-unix-keys nil
  "Non-nil if function keys use Unix-like SAS key definitions in all modes.")
(defun ess-sas-global-unix-keys ()
  "Unix/Mainframe-like SAS key definitions"
  (global-set-key [f2] 'ess-revert-wisely)
  (global-set-key [f3] 'ess-sas-submit)
  (global-set-key [(control f3)] 'ess-sas-submit-region)
  (global-set-key [f4] 'ess-sas-goto-sas)
  (global-set-key [f5] 'ess-sas-goto-log)
  (global-set-key [(control f5)] 'ess-sas-append-log)
  (global-set-key [f6] 'ess-sas-goto-lst)
  (global-set-key [(control f6)] 'ess-sas-append-lst)
  (global-set-key [f7] 'ess-sas-goto-file-1)
  (global-set-key [f8] 'shell)
  (global-set-key [f9] 'ess-sas-data-view)
  (global-set-key [f10] 'ess-sas-toggle-sas-mode)
  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path))

(defvar ess-sas-local-pc-keys nil
  "Non-nil if function keys use PC-like SAS key definitions
in SAS-mode and related modes.")
(defun ess-sas-local-pc-keys ()
  "PC-like SAS key definitions."
  (define-key sas-mode-local-map [f2] 'ess-revert-wisely)
  (define-key sas-mode-local-map [f3] 'shell)
  (define-key sas-mode-local-map [f4] 'ess-sas-goto-file-1)
  (define-key sas-mode-local-map [f5] 'ess-sas-goto-sas)
  (define-key sas-mode-local-map [f6] 'ess-sas-goto-log)
  (define-key sas-mode-local-map [(control f6)] 'ess-sas-append-log)
  (define-key sas-mode-local-map [f7] 'ess-sas-goto-lst)
  (define-key sas-mode-local-map [(control f7)] 'ess-sas-append-lst)
  (define-key sas-mode-local-map [f8] 'ess-sas-submit)
  (define-key sas-mode-local-map [(control f8)] 'ess-sas-submit-region)
  (define-key sas-mode-local-map [f9] 'ess-sas-data-view)
  (define-key sas-mode-local-map [f10] 'ess-sas-toggle-sas-mode)
  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path))

(defvar ess-sas-local-unix-keys nil
  "Non-nil if function keys use Unix-like SAS key definitions
in SAS-mode and related modes.")
(defun ess-sas-local-unix-keys ()
  "Unix/Mainframe-like SAS key definitions"
  (define-key sas-mode-local-map [f2] 'ess-revert-wisely)
  (define-key sas-mode-local-map [f3] 'ess-sas-submit)
  (define-key sas-mode-local-map [(control f3)] 'ess-sas-submit-region)
  (define-key sas-mode-local-map [f4] 'ess-sas-goto-sas)
  (define-key sas-mode-local-map [f5] 'ess-sas-goto-log)
  (define-key sas-mode-local-map [(control f5)] 'ess-sas-append-log)
  (define-key sas-mode-local-map [f6] 'ess-sas-goto-lst)
  (define-key sas-mode-local-map [(control f6)] 'ess-sas-append-lst)
  (define-key sas-mode-local-map [f7] 'ess-sas-goto-file-1)
  (define-key sas-mode-local-map [f8] 'shell)
  (define-key sas-mode-local-map [f9] 'ess-sas-data-view)
  (define-key sas-mode-local-map [f10] 'ess-sas-toggle-sas-mode)
  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path))


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
