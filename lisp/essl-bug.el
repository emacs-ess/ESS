;;; essl-bug.el -- ESS BUGS customization

;; Copyright (C) 2001 Rodney Sparapani

;; Author: Rodney Sparapani <rsparapa@mcw.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 27 February 2001
;; Modified: $Date: 2001/07/27 19:21:13 $
;; Version: $Revision: 1.11 $
;; RCS: $Id: essl-bug.el,v 1.11 2001/07/27 19:21:13 ess Exp $

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


(if (assoc "\\.bug\\'" auto-mode-alist) nil
    (setq auto-mode-alist
	(append
	   '(("\\.bug\\'"	. ess-bugs-mode)
	       ("\\.BUG\\'"	. ess-bugs-mode)
	       ("\\.bmd\\'"	. ess-bugs-mode)
	       ("\\.BMD\\'"	. ess-bugs-mode)
	       ("\\.bog\\'"     . ess-bugs-mode)
	       ("\\.BOG\\'"     . ess-bugs-mode)
	    )
	auto-mode-alist)
    )
)

(defcustom ess-bugs-batch-command 
	(convert-standard-filename (concat ess-lisp-directory "/../etc/" "backbugs")) 
    "ESS[BUGS]:  Set to name and location of ESS \"backbugs\" script."
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

(defcustom ess-bugs-default-bins "32"
"ESS[BUGS]:  number of bins to use in the Griddy algorithm (Metropolis sampling)."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-default-burn-in "500"
    "ESS[BUGS]:  burn-in iterations to discard."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-default-update "1000"
    "ESS[BUGS]:  iterations to store."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-default-checkpoint "100"
    "ESS[BUGS]:  make a snapshot every this many iterations."
    :group 'ess-bugs
    :type  'string
)

(defvar ess-bugs-file "."
   "ESS[BUGS]:  BUGS file with PATH.")

(defvar ess-bugs-file-root "."
   "ESS[BUGS]:  Root of BUGS file.")

(defvar ess-bugs-file-suffix "."
   "ESS[BUGS]:  Suffix of BUGS file.")

(defvar ess-bugs-file-dir "."
   "ESS[BUGS]:  Directory of BUGS file.")

(defvar ess-bugs-file-data "."
   "ESS[BUGS]:  BUGS data file.")

(defcustom ess-bugs-inits-suffix ".in"
   "ESS[BUGS]:  BUGS init file suffix."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-data-suffix ".dat"
   "ESS[BUGS]:  BUGS data file suffix."
    :group 'ess-bugs
    :type  'string
)

(defvar ess-bugs-monitor-vars " "
    "ESS[BUGS]:  List of BUGS variables to be written out to a file.")

(defvar ess-bugs-stats-vars " "
    "ESS[BUGS]:  List of BUGS variables to be summarized with statistics.")

(defvar ess-bugs-mode-hook nil 
    "ESS[BUGS]:  List of functions to call upon entering mode.")


(defvar ess-bugs-mode-map nil
   "ESS[BUGS]:  Keymap for mode.")

(if ess-bugs-mode-map nil
    (setq ess-bugs-mode-map (make-keymap))
    (define-key ess-bugs-mode-map [f2]  'ess-revert)
    (define-key ess-bugs-mode-map [f12] 'ess-bugs-next-action)
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

	;; .bmd files
	(cons (concat "\\<\\(clear\\|checkpoint\\|compile\\|data\\|"
		"diag\\|help\\|inits\\|iter\\|model\\|monitor\\|"
		"out\\|q\\|save\\|stats\\|update\\)[ \t\n]*(")
					font-lock-function-name-face)

	;; .dat files
	;;(cons (concat "\\<\\(c\\|list\\)[ \t\n]*(")
	;;				font-lock-function-name-face)
    )
    "ESS[BUGS]:  Font lock keywords."
)


(defun ess-bugs-file ()
"ESS[BUGS]:  Set `ess-bugs-file', `ess-bugs-file-root', `ess-bugs-file-suffix'"
" and `ess-bugs-file-dir'."
   (interactive)

   (let ((ess-bugs-temp-string (buffer-name)))
        (setq ess-bugs-file (expand-file-name ess-bugs-temp-string))
        (setq ess-bugs-file-dir 
	    (convert-standard-filename (file-name-directory ess-bugs-file)))
        (setq ess-bugs-file-root 
	    (file-name-nondirectory (file-name-sans-extension ess-bugs-file)))

        (if (fboundp 'file-name-extension) 
	    (setq ess-bugs-file-suffix (file-name-extension ess-bugs-temp-string))
	    ;;else
	    (setq ess-bugs-temp-string (split-string ess-bugs-temp-string "[.]"))
	    (setq ess-bugs-file-suffix 
		(nth (- (length ess-bugs-temp-string) 1) ess-bugs-temp-string)))

	(setq ess-bugs-file-suffix 
	    (nth 0 (split-string (concat "." ess-bugs-file-suffix) "[<]")))
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
	    (insert (concat "inits in \"%INITS\";\n"))
	    (insert "{\n")
            (insert "    for (i in 1:N) {\n    \n")
            (insert "    }\n")
            (insert "}\n")
	))	

	(if (equal ".bmd" suffix) (progn
	    (insert (concat "compile(\"" ess-bugs-file-dir ess-bugs-file-root ".bug\")\n"))
	    (insert (concat "save(\"" ess-bugs-file-dir ess-bugs-file-root ".in0\")\n"))
	    (insert (concat "update(" ess-bugs-default-burn-in ")\n"))
	    (insert (concat "save(\"" ess-bugs-file-dir ess-bugs-file-root ".in1\")\n"))
	    (insert "#%MONITOR\n\n#%MONITOR\n")
	    (insert (concat "checkpoint(" ess-bugs-default-checkpoint ")\n"))
	    (insert (concat "update(" ess-bugs-default-update ")\n"))
	    (insert (concat "save(\"" ess-bugs-file-dir ess-bugs-file-root ".in2\")\n"))
	    (insert "#%STATS\n\n#%STATS\n")
	    (insert "q(\"" ess-bugs-file-dir ess-bugs-file-root ".bog\")\n")
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

   (if (equal ".bug" ess-bugs-file-suffix) (ess-bugs-na-bug))
   ;;else 
   (if (equal ".bmd" ess-bugs-file-suffix) (ess-bugs-na-bmd))
)

(defun ess-bugs-na-bmd ()
    "ESS[BUGS]:  Perform the Next-Action for .bmd."

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

	(insert (concat "cd \"" ess-bugs-file-dir "\""))
	(comint-send-input)

	(insert (concat ess-bugs-batch-pre-command " " ess-bugs-batch-command " " 
	    ess-bugs-default-bins " " ess-bugs-file-root " " ess-bugs-file " " 
	    ess-bugs-batch-post-command))

	(comint-send-input)
)

   
(defun ess-bugs-na-bug ()
    "ESS[BUGS]:  Perform Next-Action for .bug"

	(if (equal 0 (buffer-size)) (ess-switch-to-suffix ".bug")
	    (save-excursion 
		(goto-char (point-min))

	        (if (search-forward "%MODEL" nil t) 
		    (replace-match ess-bugs-file-root t t))

	        (if (search-forward "%DATA" nil t) (progn
		    (setq ess-bugs-file-data 
			(concat ess-bugs-file-dir ess-bugs-file-root ess-bugs-data-suffix))
		    (replace-match ess-bugs-file-data t t))
	        ;;else
	        (if (search-forward-regexp "data.+in[ \t\n]+\"\\(.*\\)\"" nil t)
		    (setq ess-bugs-file-data (match-string 1))))

	        (if (search-forward "%INITS" nil t) 
		    (replace-match 
			(concat ess-bugs-file-dir ess-bugs-file-root ess-bugs-inits-suffix) t t))
 
		(let ((ess-bugs-temp-string " ")
		    (ess-bugs-buffer-ptr nil))
		    (goto-char (point-min))
		
		    (if (search-forward-regexp 
			    "N[ \t]*=[ \t]*[0-9]+[ \t]*;[ \t]*#[ \t]*%N" nil t) (progn

			(save-excursion (save-match-data
			    (setq ess-bugs-buffer-ptr (find-buffer-visiting ess-bugs-file-data))

			    (if ess-bugs-buffer-ptr (set-buffer ess-bugs-buffer-ptr)
				(set-buffer (create-file-buffer ess-bugs-file-data))
				(insert-file-contents ess-bugs-file-data t))

			    (setq ess-bugs-temp-string 
				(concat "N = " 
				    (int-to-string (count-lines (point-min) (point-max))) ";#%N"))
			))

			(replace-match ess-bugs-temp-string t t)
		    ))

		)

		(let (
		    (ess-bugs-search-min nil)
		    (ess-bugs-search-max nil))

		    (goto-char (point-min))

		    (if (search-forward "%MONITOR" nil t) (setq ess-bugs-search-min (point))
		    ;;else
			(setq ess-bugs-search-min (search-forward "var"))
		    )

		    (setq ess-bugs-search-max (search-forward-regexp ";"))
		    
		    (goto-char ess-bugs-search-min)
		    (setq ess-bugs-monitor-vars "")
		    
		    (while (search-forward-regexp 
			"[, \t\n#]+\\([a-zA-Z0-9.]+\\)\\(\\(\\[\\)[a-zA-Z0-9]*\\(\\]\\)\\)?" 
			ess-bugs-search-max t)

			(setq ess-bugs-monitor-vars 
			    (concat ess-bugs-monitor-vars "monitor(" 
				(match-string 1) (match-string 3) (match-string 4) ")\n"))
		    )

		    (setq ess-bugs-monitor-vars 
			(concat "#%MONITOR\n" ess-bugs-monitor-vars "#%MONITOR\n"))

		    (goto-char (point-min))

		    (if (search-forward "%STATS" nil t) (progn
			(setq ess-bugs-search-min (point))
			(setq ess-bugs-search-max (search-forward-regexp ";"))
		    
			(goto-char ess-bugs-search-min)
			(setq ess-bugs-stats-vars "")
		    
			(while (search-forward-regexp 
			    "[, \t\n#]+\\([a-zA-Z0-9.]+\\)\\(\\(\\[\\)[a-zA-Z0-9]*\\(\\]\\)\\)?" 
			    ess-bugs-search-max t)

			    (setq ess-bugs-stats-vars 
				(concat ess-bugs-stats-vars "stats(" 
				    (match-string 1) (match-string 3) (match-string 4) ")\n"))
			)

			(setq ess-bugs-stats-vars (concat "#%STATS\n" ess-bugs-stats-vars "#%STATS\n"))
		    )

;; replace-in-string may not be available, work-around necessary; see below
;;			(setq ess-bugs-stats-vars 
;;			    (replace-in-string ess-bugs-monitor-vars "#%MONITOR" "#%STATS"))
;;			(setq ess-bugs-stats-vars 
;;			    (replace-in-string ess-bugs-stats-vars "monitor" "stats" t))

		    ;;else
		    (setq ess-bugs-stats-vars ess-bugs-monitor-vars)

		    (while (string-match "#%MONITOR" ess-bugs-stats-vars)
			(setq ess-bugs-stats-vars 
			    (replace-match "#%STATS" t t ess-bugs-stats-vars)))

		    (while (string-match "monitor" ess-bugs-stats-vars)
			(setq ess-bugs-stats-vars 
			    (replace-match "stats" t t ess-bugs-stats-vars)))

		    )
		)
		    
	    )

	    (save-buffer)
	    (ess-switch-to-suffix ".bmd")

    (save-excursion
	(goto-char (point-min))

	(if (search-forward-regexp "#%MONITOR\\(.\\|\n\\)*#%MONITOR\n" nil t) 
	    (replace-match ess-bugs-monitor-vars t))

	(if (search-forward-regexp "#%STATS\\(.\\|\n\\)*#%STATS\n" nil t) 
	    (replace-match ess-bugs-stats-vars t))
    )

	)	
)


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

   (if (not (w32-shell-dos-semantics)) 
	(add-hook 'comint-output-filter-functions 'ess-exit-notify-sh))
)


(provide 'essl-bug)
    
