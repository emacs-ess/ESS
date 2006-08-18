;;; essd-jags.el -- ESS[JAGS] dialect

;; Copyright (C) 2006 Rodney Sparapani

;; Original Author: Rodney Sparapani <rsparapa@mcw.edu>
;; Created: 16 August 2006
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
    (delete '("\\.[bB][uU][gG]\\'" . ess-bugs-mode) auto-mode-alist))

(setq auto-mode-alist 
    (append '(("\\.[bB][uU][gG]\\'" . ess-jags-mode)) auto-mode-alist))

(setq ess-bugs-batch-command "jags")

(defvar ess-bugs-font-lock-keywords
    (list
	;; .bug files
	(cons "#.*\n"			font-lock-comment-face)

	(cons "^[ \t]*\\(model\\|var\\)\\>"
					font-lock-keyword-face)

	;(cons "\\<in[ \t]+[1-9]\\>"	font-lock-keyword-face)

	(cons (concat "\\<d\\(bern\\|beta\\|bin\\|cat\\|chisqr\\|"
		"dexp\\|dirch\\|exp\\|gamma\\|interval\\|lnorm\\|logis\\|"
		"mnorm\\|multi\\|negbin\\|norm\\|par\\|pois\\|"
		"sum\\|t\\|unif\\|weib\\|wish\\)[ \t\n]*(")
					font-lock-reference-face)

	(cons (concat "\\<\\(for\\|cloglog\\|equals\\|exp\\|inprod\\|"
		"inverse\\|log\\(det\\|fact\\|gam\\|it\\)?\\|max\\|"
		"mean\\|min\\|phi\\|pow\\|probit\\|sd\\|sort\\|sqrt\\|"
		"step\\|sum\\|T\\)[ \t\n]*(")
					font-lock-function-name-face)

	;; .jmd files
	(cons (concat "\\<\\(clear\\|coda\\|compile\\|data\\|"
		"exit\\|in\\|initialize\\|monitor\\|parameters\\|"
		"seed\\|set\\|to\\|update\\)\\>")
					font-lock-keyword-face)

	(cons (concat "\\<\\(by\\|chain\\|nchains\\|stem\\|thin\\)[ \t\n]*(")
					font-lock-function-name-face)
    )
    "ESS[JAGS]: Font lock keywords."
)

(defun ess-bugs-switch-to-suffix (suffix)
   "ESS[JAGS]: Switch to file with suffix."
   (find-file (concat ess-bugs-file-dir ess-bugs-file-root suffix))

   (if (equal 0 (buffer-size)) (progn
	(if (equal ".bug" suffix) (progn
	    (insert "var ;\n")
	    (insert "#%MONITOR;\n")
	    (insert "model {\n")
            (insert "    for (i in 1:N) {\n    \n")
            (insert "    }\n")
            (insert "}\n")
	))

	(if (equal ".jmd" suffix) (progn
	    (insert (concat "model in \"" ess-bugs-file-dir ess-bugs-file-root ".bug\"\n"))
	    (insert (concat "data in \"" ess-bugs-file-dir ess-bugs-file-root ".bug\"\n"))
	    (insert "compile\n")
	    (insert (concat "parameters in \"" ess-bugs-file-dir ess-bugs-file-root ".in\"\n"))
	    (insert "initialize\n")
	    (insert (concat "parameters to \"" ess-bugs-file-dir ess-bugs-file-root ".in0\"\n"))
	    (insert (concat "update " ess-bugs-default-burn-in "\n"))
	    (insert (concat "parameters to \"" ess-bugs-file-dir ess-bugs-file-root ".in1\"\n"))
	    (insert "#%MONITOR\n\n#%MONITOR\n")
	    (insert (concat "update " ess-bugs-default-update "\n"))
	    (insert (concat "parameters to \"" ess-bugs-file-dir ess-bugs-file-root ".in2\"\n"))
	    (insert "exit\n")
	))
    ))
)

(defun ess-bugs-next-action ()
   "ESS[JAGS]: Perform the appropriate next action."
   (interactive)
   (ess-bugs-file)

   (if (equal ".bug" ess-bugs-file-suffix) (ess-bugs-na-bug))
   ;;else
   (if (equal ".jmd" ess-bugs-file-suffix) (ess-bugs-na-jmd))
)

(defun ess-bugs-na-jmd ()
    "ESS[JAGS]: Perform the Next-Action for .jmd."

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
	     (concat ess-bugs-file-root ".jmd") " " ess-bugs-batch-post-command))

	(comint-send-input)
)

(defun ess-bugs-na-bug ()
    "ESS[JAGS]: Perform Next-Action for .bug"

	(if (equal 0 (buffer-size)) (ess-bugs-switch-to-suffix ".bug")
	    (save-excursion 
		(goto-char (point-min))

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
			    (concat ess-bugs-monitor-vars "monitor set "
				(match-string 1) (match-string 3) (match-string 4) (match-string 5) "\n"))
		    )

		    (setq ess-bugs-monitor-vars
			(concat "#%MONITOR\n" ess-bugs-monitor-vars "#%MONITOR\n"))
	    ))

	    (save-buffer)
	    (ess-bugs-switch-to-suffix ".jmd"))

    (save-excursion
	(goto-char (point-min))

	(if (search-forward-regexp "#%MONITOR\\(.\\|\n\\)*#%MONITOR\n" nil t)
	    (replace-match ess-bugs-monitor-vars t))
	)
)

(defun ess-jags-mode ()
   "ESS[JAGS]: Major mode for JAGS."
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'ess-jags-mode)
   (setq mode-name "ESS[JAGS]")
   (use-local-map ess-bugs-mode-map)
   (setq font-lock-auto-fontify t)
   (make-local-variable 'font-lock-defaults)
   (setq font-lock-defaults '(ess-bugs-font-lock-keywords nil t))
   (run-hooks 'ess-bugs-mode-hook)

   (if (not (w32-shell-dos-semantics))
	(add-hook 'comint-output-filter-functions 'ess-bugs-exit-notify-sh))
)

(setq features (delete 'essd-bugs features))
(provide 'essd-jags)
