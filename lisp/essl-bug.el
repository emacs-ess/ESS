;;; essl-bug.el -- ESS[BUGS] 
;;;WARNING! deprecated; see essl-bugs.el, essd-bugs.el, essd-jags.el

;; Copyright (C) 2001 Rodney Sparapani
;; Copyright (C) 2002--2004 Free Software Foundation, Inc.
;; Copyright (C) 2002--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: Rodney Sparapani <rsparapa@mcw.edu>
;; Created: 27 February 2001
;; Maintainers: ESS-core <ESS-core@stat.math.ethz.ch>

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

(if (assoc "\\.[bB][uUoO][gG]\\'" auto-mode-alist) nil
    (setq auto-mode-alist
	(append
	   '(("\\.[bB][uUoO][gG]\\'"	. ess-bugs-mode)
	     ("\\.[bB][mM][dD]\\'"	. ess-bugs-mode))
	auto-mode-alist)
    )
)

(defgroup ess-bugs nil
  "ESS: BUGS."
  :group 'ess
  :prefix "ess-")

(defcustom ess-bugs-batch-method
  (if ess-microsoft-p
    (if (w32-shell-dos-semantics) 'ms-dos 'sh)
    (if (equal system-type 'Apple-Macintosh) 'apple-script 'sh))
  "Method used by `ess-bugs-batch'.
The default is based on the value of the emacs variable `system-type'
and, on Windows machines, the function `w32-shell-dos-semantics'.
'ms-dos           if *shell* follows MS-DOS semantics
'sh               if *shell* runs sh, ksh, csh, tcsh or bash
'apple-script     *shell* unavailable, use AppleScript

Windows users running MS-DOS in *shell* will get 'ms-dos by default.

Windows users running bash in *shell* will get 'sh by default.

Unix users will get 'sh by default.

Users whose default is not 'sh, but are accessing a remote machine with
`telnet', `rlogin', or `ssh', should have the following in ~/.emacs
   (setq-default ess-bugs-batch-method 'sh)"
    :group 'ess-bugs
)

(defcustom ess-bugs-batch-version "0.6"
  "*ESS[BUGS]: The batch BUGS version to use."
    :group 'ess-bugs
    :type  'string
)

;; SJE Thu 13 May 2004
;; The "backbugs" scripts can be found in ess-etc-directory, so maybe we
;; can use ess-etc-directory here too.
(defcustom ess-bugs-batch-command
    (if (equal ess-bugs-batch-version "0.6") "backbugs" 
      (if (equal ess-bugs-batch-version "0.5") "backbug5"
        (if (equal ess-bugs-batch-version "jags") "jags" "backbugs")))
  "*ESS[BUGS]: The name of the command to run BUGS in batch mode.

Set to the name of the batch BUGS script that comes with ESS or
to the name of BUGS command. Make sure it is in your PATH or
add path to the command name."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-batch-post-command
    (if (equal ess-bugs-batch-method 'sh) "&" " ")
    "*ESS[BUGS]: Modifiers at the end of the batch BUGS command line."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-batch-pre-command
    (if (equal ess-bugs-batch-method 'sh) "test -f nohup.out && rm -f nohup.out || true; nohup"
	(if ess-microsoft-p "start"))
    "*ESS[BUGS]: Modifiers at the beginning of the batch BUGS command line."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-default-bins (if (equal ess-bugs-batch-version "0.6") "32" " ")
"ESS[BUGS]: Number of bins to use in the Griddy algorithm (Metropolis sampling)."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-default-burn-in "500"
    "ESS[BUGS]: Burn-in iterations to discard."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-default-update "1000"
    "ESS[BUGS]: Iterations to store."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-default-checkpoint "100"
    "ESS[BUGS]: Make a snapshot every this many iterations."
    :group 'ess-bugs
    :type  'string
)

(defvar ess-bugs-file "."
   "ESS[BUGS]: BUGS file with PATH.")

(defvar ess-bugs-file-root "."
   "ESS[BUGS]: Root of BUGS file.")

(defvar ess-bugs-file-suffix "."
   "ESS[BUGS]: Suffix of BUGS file.")

(defvar ess-bugs-file-dir "."
   "ESS[BUGS]: Directory of BUGS file.")

(defvar ess-bugs-file-data "..."
   "ESS[BUGS]: BUGS data file.")

(defcustom ess-bugs-inits-suffix ".in"
   "ESS[BUGS]: BUGS init file suffix."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-data-suffix ".dat"
   "ESS[BUGS]: BUGS data file suffix."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-suffix-regexp
    (concat "[.]\\([bB][oOuU][gG]\\|[bB][mM][dD]\\|"
	(if ess-bugs-inits-suffix (concat
	    "\\|" (downcase ess-bugs-inits-suffix) "\\|" (upcase ess-bugs-inits-suffix)))
	(if ess-bugs-data-suffix (concat
	    "\\|" (downcase ess-bugs-data-suffix) "\\|" (upcase ess-bugs-data-suffix)))
        "\\)")
    "*ESS[BUGS]: Regular expression for BUGS suffixes."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-mode-hook nil
    "*ESS[BUGS]: List of functions to call upon entering mode."
    :group 'ess-bugs
    :type 'hook)

(defvar ess-bugs-monitor-vars " "
    "ESS[BUGS]: List of BUGS variables to be written out to a file.")

(defvar ess-bugs-stats-vars " "
    "ESS[BUGS]: List of BUGS variables to be summarized with statistics.")

(defvar ess-bugs-mode-map nil
   "ESS[BUGS]: Keymap for mode.")

(if ess-bugs-mode-map nil
    (setq ess-bugs-mode-map (make-keymap))
    (define-key ess-bugs-mode-map (quote [f2])  'ess-revert-wisely)
    (define-key ess-bugs-mode-map (quote [f12]) 'ess-bugs-next-action)
)


(defvar ess-bugs-syntax-table nil
   "ESS[BUGS]: Syntax table for mode.")

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

	(cons "^[ \t]*\\(const\\|data\\|in\\(it\\(ialize\\|s\\)\\)?\\|model\\|var\\)\\>"
					font-lock-keyword-face)

	;;(cons "\\<in[ \t]+[1-9]\\>"	font-lock-keyword-face)

	(cons (concat "\\<d\\(bern\\|beta\\|bin\\|cat\\|chisqr\\|"
		"dexp\\|dirch\\|exp\\|gamma\\|interval\\|lnorm\\|logis\\|"
		"mnorm\\|multi\\|negbin\\|norm\\|par\\|pois\\|sum\\|"
		"t\\|unif\\|weib\\|wish\\)[ \t\n]*(")
					font-lock-reference-face)

	(cons (concat "\\<\\(for\\|cloglog\\|equals\\|exp\\|inprod\\|"
		"inverse\\|log\\(det\\|fact\\|gam\\|it\\)?\\|max\\|"
		"mean\\|min\\|phi\\|pow\\|probit\\|sd\\|sort\\|sqrt\\|"
		"step\\|sum\\|I\\|T\\)[ \t\n]*(")
					font-lock-function-name-face)

	;; .bmd files
	(cons (concat "\\<\\(by\\|clear\\|chain\\|checkpoint\\|compile\\|data\\|"
		"diag\\|help\\|inits\\|iter\\|model\\|monitor\\|"
		"out\\|q\\|save\\|stats\\|stem\\|thin\\|update\\)[ \t\n]*(")
					font-lock-function-name-face)

	(cons "^[ \t]*\\(clear\\|coda\\|exit\\|monitor\\|parameters\\|seed\\|set\\|to\\)\\>"
					font-lock-keyword-face)

	;; .dat files
	;;(cons (concat "\\<\\(c\\|list\\)[ \t\n]*(")
	;;				font-lock-function-name-face)
    )
    "ESS[BUGS]: Font lock keywords."
)


(defun ess-bugs-file ()
"ESS[BUGS]: Set internal variables dealing with BUGS files.
Set `ess-bugs-file', `ess-bugs-file-root', `ess-bugs-file-suffix'
and `ess-bugs-file-dir'."
   (let ((ess-bugs-temp-string (buffer-name)))
        (setq ess-bugs-file (expand-file-name ess-bugs-temp-string))
        (setq ess-bugs-file-dir
	    (convert-standard-filename (file-name-directory ess-bugs-file)))
        (setq ess-bugs-file-root
	    (file-name-nondirectory (file-name-sans-extension ess-bugs-file)))

        (if (fboundp 'file-name-extension)
	    (setq ess-bugs-file-suffix (file-name-extension ess-bugs-temp-string))
	    ;;else
	    (setq ess-bugs-file-suffix (car (last (split-string ess-bugs-temp-string "[.]")))))

	(setq ess-bugs-file-suffix
	    (downcase (car (split-string (concat "." ess-bugs-file-suffix) "[<]"))))

	(setq ess-bugs-file (concat ess-bugs-file-dir ess-bugs-file-root ess-bugs-file-suffix))
   )
)


(defun ess-switch-to-suffix (suffix)
   "ESS: Switch to file with suffix."
   (find-file (concat ess-bugs-file-dir ess-bugs-file-root suffix))

   (if (equal 0 (buffer-size)) (progn
	(if (equal ".bug" suffix) (progn
	    (insert (concat "model %MODEL;\n"))
	    (insert (concat "const N = 0;#%N\n"))
	    (insert "var ;\n")
	    (insert "#%MONITOR;\n")
	    (insert "#%STATS;\n")
	    (insert (concat "data  in \"%DATA\";\n"))
	    (insert (concat "inits in \"%INITS\";\n"))
	    (insert "{\n")
            (insert "    for (i in 1:N) {\n    \n")
            (insert "    }\n")
            (insert "}\n")
	))

	(if (equal ".bmd" suffix) (let
	    ((tmp-bugs-file-dir (if (equal ess-bugs-batch-version "0.6") ess-bugs-file-dir)))
	    (insert (concat "compile(\"" tmp-bugs-file-dir ess-bugs-file-root ".bug\")\n"))
	    (insert (concat "save(\"" tmp-bugs-file-dir ess-bugs-file-root ".in0\")\n"))
	    (insert (concat "update(" ess-bugs-default-burn-in ")\n"))
	    (insert (concat "save(\"" tmp-bugs-file-dir ess-bugs-file-root ".in1\")\n"))
	    (insert "#%MONITOR\n\n#%MONITOR\n")
	    (if (equal ess-bugs-batch-version "0.6")
		(insert (concat "checkpoint(" ess-bugs-default-checkpoint ")\n")))
	    (insert (concat "update(" ess-bugs-default-update ")\n"))
	    (insert (concat "save(\"" tmp-bugs-file-dir ess-bugs-file-root ".in2\")\n"))
	    (insert "#%STATS\n\n#%STATS\n")
	    (insert "q()\n")
	    ;;(insert "q(\"" ess-bugs-file-dir ess-bugs-file-root ".bog\")\n")
	))
    ))
)

(defun ess-exit-notify-sh (string)
  "Detect completion or failure of submitted job and notify the user."
  (let* ((exit-done "\\[[0-9]+\\]\\ *\\+*\\ *\\(Exit\\|Done\\).*$")
	 (beg (string-match exit-done string)))
    (if beg (message (substring string beg (match-end 0))))))

(defun ess-bugs-next-action ()
   "ESS[BUGS]: Perform the appropriate next action."
   (interactive)
   (ess-bugs-file)

   (if (equal ".bug" ess-bugs-file-suffix) (ess-bugs-na-bug))
   ;;else
   (if (equal ".bmd" ess-bugs-file-suffix) (ess-bugs-na-bmd))
)

(defun ess-bugs-na-bmd ()
    "ESS[BUGS]: Perform the Next-Action for .bmd."

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
	    (if (equal ess-bugs-batch-version "0.6") ess-bugs-default-bins)
	     " " ess-bugs-file-root " "
	    (if (equal ess-bugs-batch-version "0.6")
		 ess-bugs-file (concat ess-bugs-file-root ".bmd"))
	     " " ess-bugs-batch-post-command))

	(comint-send-input)
)


(defun ess-bugs-na-bug ()
    "ESS[BUGS]: Perform Next-Action for .bug"

	(if (equal 0 (buffer-size)) (ess-switch-to-suffix ".bug")
	    (save-excursion (let
		((tmp-bugs-file-dir (if (equal ess-bugs-batch-version "0.6") ess-bugs-file-dir)))
		(goto-char (point-min))

	        (if (search-forward "%MODEL" nil t)
		    (replace-match ess-bugs-file-root t t))

	        (if (search-forward "%DATA" nil t) (progn
		    (setq ess-bugs-file-data
			(concat tmp-bugs-file-dir ess-bugs-file-root ess-bugs-data-suffix))
		    (replace-match ess-bugs-file-data t t))
	        ;;else
	        (if (search-forward-regexp "data.+in[ \t\n]+\"\\(.*\\)\"" nil t)
		    (setq ess-bugs-file-data (match-string 1))
		;;else
		    (setq ess-bugs-file-data "...")
		))

	        (if (search-forward "%INITS" nil t)
		    (replace-match
			(concat tmp-bugs-file-dir ess-bugs-file-root ess-bugs-inits-suffix) t t))

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
		    (ess-bugs-search-max nil)
		    (ess-bugs-search-vars
"\\([a-zA-Z0-9.]+\\)\\(\\(\\[\\)[a-zA-Z0-9]*\\(,\\)?[a-zA-Z0-9]*\\(\\]\\)\\)?[ \t]*[,]?[ \t]*\\(#.*\\)?[\n]?"
		    ))

		    (goto-char (point-min))

		    (if (search-forward-regexp "%MONITOR[ \t]+" nil t)
			(setq ess-bugs-search-min (point))
		    ;;else
			(setq ess-bugs-search-min (search-forward "var"))
		    )

		    (setq ess-bugs-search-max (search-forward-regexp ";"))

		    (goto-char ess-bugs-search-min)
		    (setq ess-bugs-monitor-vars "")

		    (while (search-forward-regexp ess-bugs-search-vars ess-bugs-search-max t)

			(setq ess-bugs-monitor-vars
			    (concat ess-bugs-monitor-vars "monitor("
				(match-string 1) (match-string 3) (match-string 4) (match-string 5) ")\n"))
		    )

		    (setq ess-bugs-monitor-vars
			(concat "#%MONITOR\n" ess-bugs-monitor-vars "#%MONITOR\n"))

		    (goto-char (point-min))

		    (if (search-forward-regexp "%STATS[ \t]+" nil t) (progn
			(setq ess-bugs-search-min (point))
			(setq ess-bugs-search-max (search-forward-regexp ";"))

			(goto-char ess-bugs-search-min)
			(setq ess-bugs-stats-vars "")

			(while (search-forward-regexp ess-bugs-search-vars ess-bugs-search-max t)

			    (setq ess-bugs-stats-vars
				(concat ess-bugs-stats-vars "stats("
				    (match-string 1) (match-string 3) (match-string 4) (match-string 5) ")\n"))
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

	    ))

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
   "ESS[BUGS]: Major mode for Classic BUGS."
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

(defun ess-bugs-sci-to-round-4-dp () 
    "ESS[BUGS]: round output from +/-0.000E+/-0 to 4 decimal places."
    (interactive)
    (setq buffer-read-only nil)
    (save-excursion (goto-char 0)
    (save-match-data (let ((ess-bugs-replacement-string nil)                            
			    (ess-bugs-replacement-9 0)
			    (ess-bugs-replacement-diff 0))
     (while (search-forward-regexp "-?[0-9][.][0-9][0-9][0-9]E[+-][0-9]" nil t)
	    (setq ess-bugs-replacement-string 
		  (int-to-string (string-to-number (match-string 0))))	 
	    (setq ess-bugs-replacement-diff (- (match-end 0) (match-beginning 0)))
	    (save-match-data
	        (setq ess-bugs-replacement-9 
		    (string-match "99999999999$" ess-bugs-replacement-string))

		(if (not ess-bugs-replacement-9)
		    (setq ess-bugs-replacement-9 
			(string-match "000000000001$" ess-bugs-replacement-string))))
	
	    (if ess-bugs-replacement-9	
		(setq ess-bugs-replacement-string 
		    (substring ess-bugs-replacement-string 0 ess-bugs-replacement-9)))

	    (setq ess-bugs-replacement-diff 
		(- ess-bugs-replacement-diff (string-width ess-bugs-replacement-string)))

	   (while (> ess-bugs-replacement-diff 0)
		(setq ess-bugs-replacement-string (concat ess-bugs-replacement-string " "))
		(setq ess-bugs-replacement-diff (- ess-bugs-replacement-diff 1)))

           (replace-match ess-bugs-replacement-string))))))

;;; ESS[BUGS-Shell] for running BUGS interactively
(defgroup ess-bugs-shell nil
  "ESS: BUGS-Shell."
  :group 'ess-bugs
  :prefix "ess-")

(defcustom ess-bugs-shell-buffer-name "BUGS"
  "*ESS[BUGS-Shell]: The name of the BUGS-Shell buffer."
  :group 'ess-bugs-shell
  :type  'string)

(defcustom ess-bugs-shell-command "bugs"
  "*ESS[BUGS-Shell]: The name of the command to run BUGS interactively.

Set to the name of the batch BUGS script that comes with ESS or
to the name of BUGS command. Make sure it is in your PATH or
add path to the command name."
  :group 'ess-bugs-shell
  :type  'string)

(defcustom ess-bugs-shell-default-output-file-root "bugs"
  "*ESS[BUGS-Shell]: Default value for the root of output files."
  :group 'ess-bugs-shell
  :type  'string)

(defcustom ess-bugs-shell-mode-hook nil
  "*ESS[BUGS-Shell]: List of functions to call upon entering mode."
  :group 'ess-bugs-shell
  :type 'hook)

(defun ess-bugs-shell ()
  "Create a buffer with BUGS running as a subprocess."
  (interactive)
  (require 'shell)
  (switch-to-buffer (concat "*" ess-bugs-shell-buffer-name "*"))
  (make-comint ess-bugs-shell-buffer-name ess-bugs-shell-command nil
	       ess-bugs-default-bins ess-bugs-shell-default-output-file-root)
  (comint-mode)
  (setq shell-dirtrackp t
	major-mode 'bugs-shell-mode
	mode-name "ESS[BUGS-Shell]"
	comint-prompt-regexp "^Bugs> *")
   (make-local-variable 'font-lock-defaults)
   (setq font-lock-defaults '(ess-bugs-font-lock-keywords nil t))
   (run-hooks 'ess-bugs-shell-mode-hook)
  )

(provide 'essl-bug)
