;;; essd-bugs.el -- ESS[BUGS] dialect

;; Copyright (C) 2008-2009 Rodney Sparapani

;; Original Author: Rodney Sparapani
;; Created: 13 March 2008
;; Maintainers: ESS-help <ess-help@stat.math.ethz.ch>

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

(require 'essl-bugs)

(setq auto-mode-alist 
    (delete '("\\.[bB][uU][gG]\\'" . ess-jags-mode) auto-mode-alist))

(setq auto-mode-alist 
    (append '(("\\.[bB][uU][gG]\\'" . ess-bugs-mode)) auto-mode-alist))

(defcustom ess-bugs-batch-version "0.6"
"ESS[BUGS]: Major version of BUGS, i.e. 0.6 or 0.5"
    :group 'ess-bugs
    :type  'string
)

(setq ess-bugs-batch-command 
    (if (equal ess-bugs-batch-version "0.5") "backbug5" "backbugs"))

(defcustom ess-bugs-default-bins "32"
"ESS[BUGS]: Number of bins for the Griddy algorithm (Metropolis sampling)."
    :group 'ess-bugs
    :type  'string
)

(defcustom ess-bugs-default-checkpoint "100"
    "ESS[BUGS]: Make a snapshot every this many iterations."
    :group 'ess-bugs
    :type  'string
)

(defvar ess-bugs-font-lock-keywords
    (list
	;; .bug files
	(cons "#.*\n"			font-lock-comment-face)

	(cons "^[ \t]*\\(model\\|const\\|data\\|inits\\|var\\)\\>"
					font-lock-keyword-face)

	(cons "\\<in[ \t]+[1-9]\\>"	font-lock-keyword-face)

	(cons (concat "\\<d\\(bern\\|beta\\|bin\\|cat\\|chisqr\\|"
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
    )
    "ESS[BUGS]: Font lock keywords."
)

(defun ess-bugs-switch-to-suffix (suffix)
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

	(if (equal 0 (buffer-size)) (ess-bugs-switch-to-suffix ".bug")
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
	    (ess-bugs-switch-to-suffix ".bmd")

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
   ;; SJE: Basic comment functionality.
   (setq comment-start "#")
   (run-hooks 'ess-bugs-mode-hook)

   (if (not (w32-shell-dos-semantics))
	(add-hook 'comint-output-filter-functions 'ess-bugs-exit-notify-sh))
)

(setq features (delete 'essd-jags features))
(provide 'essd-bugs)
