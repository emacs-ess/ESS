;;; essl-bug.el -- ESS BUGS customization

;; Copyright (C) 2001 Rodney Sparapani

;; Author: Rodney Sparapani <rsparapa@mcw.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 27 February 2001
;; Modified: $Date: 2001/07/24 20:08:19 $
;; Version: $Revision: 1.6 $
;; RCS: $Id: essl-bug.el,v 1.6 2001/07/24 20:08:19 ess Exp $

;; Keywords: BUGS, bugs, BACKBUGS, backbugs.

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

(require 'font-lock)
(require 'comint)
(require 'ess-emcs)

(if (not (w32-shell-dos-semantics)) (add-hook 'comint-output-filter-functions 'ess-exit-notify-sh))

(if (assoc "\\.bug\\'" auto-mode-alist) nil
    (setq auto-mode-alist
	(append
	   '(("\\.bug\\'"	. ess-bugs-mode)
	       ("\\.BUG\\'"	. ess-bugs-mode)
	       ("\\.cmd\\'"	. ess-bugs-mode)
	       ("\\.CMD\\'"	. ess-bugs-mode)
	       ("\\.log\\'"     . ess-bugs-mode)
	       ("\\.LOG\\'"     . ess-bugs-mode)
	    )
	auto-mode-alist)
    )
)


(defvar ess-bugs-mode-hook nil 
   "ESS[BUGS]:  List of functions to call upon entering mode.")


(defvar ess-bugs-mode-map nil
   "ESS[BUGS]:  Keymap for mode.")

(if ess-bugs-mode-map nil
    (setq ess-bugs-mode-map (make-keymap))
    (define-key ess-bugs-mode-map [f2]  'ess-revert)
    (define-key ess-bugs-mode-map [f3]  'ess-bugs-next-action)
)


(defvar ess-bugs-syntax-table nil
   "ESS[BUGS]:  Syntax table for mode.")

(if ess-bugs-syntax-table nil
    (setq ess-bugs-syntax-table (make-syntax-table))
    (modify-syntax-entry ?\\ "."  ess-bugs-syntax-table)
    (modify-syntax-entry ?#  "<"  ess-bugs-syntax-table)
    (modify-syntax-entry ?\n ">"  ess-bugs-syntax-table)
    (modify-syntax-entry ?(  "()" ess-bugs-syntax-table)
    (modify-syntax-entry ?)  ")(" ess-bugs-syntax-table)
    (modify-syntax-entry ?.  "w"  ess-bugs-syntax-table)
)


(defvar ess-bugs-font-lock-keywords
    (list
	;; .bug files
	(cons "#.*\n"			font-lock-comment-face)

	(cons "^[ \t]*\\(model\\|const\\|data\\|inits\\|var\\)\\>"
					font-lock-keyword-face)

	(cons "\\<in\\>"		font-lock-keyword-face)

	(cons (concat "\\<d\\(bern\\|beta\\|bin\\|cat\\|chisq\\|"
		"dexp\\|dirch\\|exp\\|gamma\\|lnorm\\|logis\\|"
		"mnorm\\|multi\\|negbin\\|norm\\|par\\|pois\\|"
		"t\\|unif\\|weib\\|wish\\)[ \t\n]*(")
					font-lock-reference-face)

	(cons (concat "\\<\\(for\\|cloglog\\|equals\\|exp\\|inprod\\|"
		"inverse\\|log\\(det\\|fact\\|gam\\|it\\)?\\|max\\|"
		"mean\\|min\\|phi\\|pow\\|probit\\|sd\\|sqrt\\|"
		"step\\|sum\\|I\\)[ \t\n]*(")
					font-lock-function-name-face)

	;; .cmd files
	(cons (concat "\\<\\(clear\\|checkpoint\\|compile\\|data\\|"
		"diag\\|help\\|inits\\|iter\\|model\\|monitor\\|"
		"out\\|q\\|save\\|stats\\|update\\)[ \t\n]*(")
					font-lock-function-name-face)

	;; .dat files
	(cons (concat "\\<\\(c\\|list\\)[ \t\n]*(")
					font-lock-function-name-face)
    )
    "ESS[BUGS]:  Font lock keywords."
)


(defcustom ess-bugs-batch-command "backbugs" 
"ESS[BUGS]:  Set to name and location of \"backbugs\" script.
Note that the script that comes with ESS[BUGS] is an enhanced version."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-batch-pre-command
    (if (w32-shell-dos-semantics) "start" "nohup")
    "ESS[BUGS]:  Modifiers at the beginning of the backbugs command line."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-batch-post-command
    (if (w32-shell-dos-semantics) " " "&")
    "ESS[BUGS]:  Modifiers at the end of the backbugs command line."
    :group 'ess-bugs
    :type  'string
)

(defvar ess-bugs-file "."
   "ESS:  BUGS file with PATH.")

(defvar ess-bugs-file-root "."
   "ESS:  Root of BUGS file.")

(defvar ess-bugs-file-suffix "."
   "ESS:  Suffix of BUGS file.")

(defvar ess-bugs-file-dir "."
   "ESS:  Directory of BUGS file.")

(defvar ess-bugs-file-data "."
   "ESS:  BUGS data file.")

(defcustom ess-bugs-init-suffix ".ini"
   "ESS:  BUGS init file suffix."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-data-suffix ".dat"
   "ESS:  BUGS data file suffix."
    :group 'ess-bugs
    :type  'string
)

(defun ess-bugs-file ()
   "ESS:  Set `ess-bugs-file', `ess-bugs-file-root', `ess-bugs-file-suffix' and `ess-bugs-file-dir'."
   (interactive)

   (let ((ess-bugs-temp-string (buffer-name)))
        (setq ess-bugs-file (expand-file-name ess-bugs-temp-string))
        (setq ess-bugs-file-dir (file-name-directory ess-bugs-file))
        (setq ess-bugs-file-root (file-name-nondirectory (file-name-sans-extension ess-bugs-file)))

        (if (fboundp 'file-name-extension) (setq ess-bugs-file-suffix (file-name-extension ess-bugs-temp-string))
	     (setq ess-bugs-temp-string (split-string ess-bugs-temp-string "[.]"))
	     (setq ess-bugs-file-suffix (nth (- (length ess-bugs-temp-string) 1) ess-bugs-temp-string)))
	(setq ess-bugs-file-suffix (concat "." ess-bugs-file-suffix))
   )
)


(defun ess-switch-to-suffix (suffix)
   "ESS:  Switch to file with suffix."
   (find-file (concat ess-bugs-file-dir ess-bugs-file-root suffix))

   (if (equal 0 (buffer-size)) (progn
	(if (equal ".bug" suffix) (progn
	    (insert (concat "model %MODEL;\n"))
	    (insert (concat "const N = 0;#%N\n"))
	    (insert "var ;\n")
	    (insert (concat "data  in \"%DATA\";\n"))
	    (insert (concat "inits in \"%INIT\";\n"))
	    (insert "{\n")
            (insert "    for (i in 1:N) {\n    \n")
            (insert "    }\n")
            (insert "}\n")
	))	

	(if (equal ".cmd" suffix) (progn
	    (insert (concat "compile(\"" ess-bugs-file-dir ess-bugs-file-root ".bug\")\n"))
	    (insert "update( )\n")
	    (insert "monitor( )\n")
	    (insert "checkpoint( )\n")
	    (insert "update( )\n")
	    (insert "stats( )\n")
	    (insert "q( )\n")
	))
    ))
)
		
(defun ess-exit-notify-sh (string)
  "Detect completion or failure of submitted job and notify the user."
  (let* ((exit-done "\\[[0-9]+\\]\\ *\\+*\\ *\\(Exit\\|Done\\).*$")
	 (beg (string-match exit-done string)))
    (if beg
	(message (substring string beg (match-end 0))))))

(defun ess-revert ()
  "ESS: Revert from disk if file and buffer modification times are different."
  (interactive)
  
  (if (not(verify-visited-file-modtime (current-buffer)))
	(revert-buffer t t)))


(defun ess-bugs-next-action ()
   "ESS[BUGS]:  Perform the appropriate next action."
   (interactive)
   (ess-bugs-file)

   (if (equal ".bug" ess-bugs-file-suffix) 
	(if (equal 0 (buffer-size)) (ess-switch-to-suffix ".bug")
	    (save-excursion 
		(goto-char (point-min))

	        (if (search-forward "%MODEL" nil t) 
		    (replace-match ess-bugs-file-root t t))

	        (if (search-forward "%DATA" nil t) (progn
		    (setq ess-bugs-file-data (concat ess-bugs-file-dir ess-bugs-file-root ess-bugs-data-suffix))
		    (replace-match ess-bugs-file-data t t))
	        ;;else
	        (if (search-forward-regexp "data.+in[ \t\n]+\"\\(.*\\)\"" nil t)
		    (setq ess-bugs-file-data (match-string 1))))

	        (if (search-forward "%INIT" nil t) 
		    (replace-match (concat ess-bugs-file-dir ess-bugs-file-root ess-bugs-init-suffix) t t))
 
		(let ((ess-bugs-temp-string " ")
		    (ess-bugs-buffer-ptr nil))
		    (goto-char (point-min))
		
		    (if (search-forward-regexp "N[ \t\n]*=[ \t\n]*[0-9]+;#%N" nil t) (progn
			(save-excursion 
			    (setq ess-bugs-buffer-ptr (find-buffer-visiting ess-bugs-file-data))

			    (if ess-bugs-buffer-ptr (set-buffer ess-bugs-buffer-ptr)
				(set-buffer (create-file-buffer ess-bugs-file-data))
				(insert-file-contents ess-bugs-file-data t))

			    (setq ess-bugs-temp-string (concat "N = " (int-to-string (count-lines (point-min) (point-max))) ";#%N"))
			)

			(replace-match ess-bugs-temp-string t t)
		    ))

		)
	    )

	    (save-buffer)
	    (ess-switch-to-suffix ".cmd")
	)
    )

   (if (equal ".cmd" ess-bugs-file-suffix) (progn
	(save-buffer)
	(shell)

    (if (w32-shell-dos-semantics)
	(if (string-equal ":" (substring ess-bugs-file 1 2)) 
	    (progn
		(insert (substring ess-bugs-file 0 2))
		(comint-send-input)
	    )
	)
    )

	(insert (concat "cd \"" (convert-standard-filename (file-name-directory ess-bugs-file)) "\""))
	(comint-send-input)

	(insert (concat ess-bugs-batch-pre-command " " ess-bugs-batch-command " " 
	    ess-bugs-file-root " " ess-bugs-file " " ess-bugs-batch-post-command))

	(comint-send-input)
   ))
)

   
;;(defun ess-log-toggle ()
;;  "Toggle ESS sub-mode for .log files."
;;  (interactive)
;;
;;  (if (member '("\\.log\\'" . SAS-mode) auto-mode-alist) 
;;    (setq auto-mode-alist (delete '("\\.log\\'" . SAS-mode) auto-mode-alist))
;;    (if (member '("\\.log\\'" . ess-bugs-mode) auto-mode-alist) 
;;      (setq auto-mode-alist (delete '("\\.log\\'" . ess-bugs-mode) auto-mode-alist))))
;;
;;  (setq auto-mode-alist (append '(("\\.log\\'" . ess-bugs-mode)) auto-mode-alist))
;;
;;  (ess-switch-to-suffix "log")
;;)


(defun ess-bugs-mode ()
   "ESS[BUGS]:  Major mode for Classic BUGS."
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'ess-bugs-mode)
   (setq mode-name "ESS[BUGS]")
   (use-local-map ess-bugs-mode-map)
   (setq font-lock-auto-fontify t)
   (make-local-variable 'font-lock-defaults)
   (setq font-lock-defaults '(ess-bugs-font-lock-keywords nil t))
   (run-hooks 'ess-bugs-mode-hook)
)


(provide 'essl-bug)
    
