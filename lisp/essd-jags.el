;;; essd-jags.el -- ESS[JAGS] dialect

;; Copyright (C) 2006-2008 Rodney Sparapani

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
(require 'ess-utils)

(setq auto-mode-alist 
    (delete '("\\.[bB][uU][gG]\\'" . ess-bugs-mode) auto-mode-alist))

(setq auto-mode-alist 
    (append '(("\\.[bB][uU][gG]\\'" . ess-jags-mode)) auto-mode-alist))

(setq ess-bugs-batch-command "jags")

(defvar ess-jags-monitor-vars nil)
(make-local-variable 'ess-jags-monitor-vars)

(defvar ess-jags-thin-vars nil)
(make-local-variable 'ess-jags-thin-vars)

(defvar ess-jags-monitor-vars nil)

(defvar ess-bugs-font-lock-keywords
    (list
	;; .bug files
	(cons "#.*\n"			font-lock-comment-face)

	(cons "^[ \t]*\\(model\\|var\\)\\>"
					font-lock-keyword-face)

	(cons (concat "\\<d\\(bern\\|beta\\|bin\\|cat\\|chisq\\|"
		"dexp\\|dirch\\|exp\\|\\(gen[.]\\)?gamma\\|hyper\\|"
		"interval\\|lnorm\\|logis\\|mnorm\\|mt\\|multi\\|"
		"negbin\\|norm\\(mix\\)?\\|par\\|pois\\|sum\\|t\\|"
		"unif\\|weib\\|wish\\)[ \t\n]*(")
					font-lock-reference-face)

	(cons (concat "\\<\\(abs\\|cos\\|dim\\|\\(i\\)?cloglog\\|equals\\|"
		"exp\\|for\\|inprod\\|interp[.]line\\|inverse\\|length\\|"
		"\\(i\\)?logit\\|logdet\\|logfact\\|loggam\\|max\\|mean\\|"
		"mexp\\|min\\|phi\\|pow\\|probit\\|prod\\|rank\\|round\\|"
		"sd\\|sin\\|sort\\|sqrt\\|step\\|sum\\|t\\|trunc\\|T\\)[ \t\n]*(")
					font-lock-function-name-face)

	;; .jmd files
	(cons (concat "\\<\\(adapt\\|cd\\|clear\\|coda\\|compile\\|data\\|dir\\|"
		"exit\\|in\\(itialize\\)?\\|load\\|model\\|monitor\\(s\\)?\\|parameters\\|"
		"pwd\\|run\\|samplers\\|to\\|update\\)[ \t\n]")
					font-lock-keyword-face)

	(cons (concat "\\<\\(by\\|chain\\|nchains\\|stem\\|thin\\|type\\)[ \t\n]*(")
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
	    (insert "model {\n")
            (insert "    for (i in 1:N) {\n    \n")
            (insert "    }\n")
            (insert "}\n")
	    (insert "#Local Variables:\n")
	    (insert "#ess-jags-monitor-vars:()\n")
	    (insert "#ess-jags-thin-vars:\"1\"\n")
	    (insert "#End:\n")
	))

	(if (equal ".jmd" suffix) (progn
	    (insert (concat "model in \"" ess-bugs-file-dir ess-bugs-file-root ".bug\"\n"))
	    (insert (concat "data in \"" ess-bugs-file-dir ess-bugs-file-root ".txt\"\n"))
	    (insert "compile\n")
	    (insert (concat "parameters in \"" ess-bugs-file-dir ess-bugs-file-root ".in\"\n"))
	    (insert "initialize\n")
	    (insert (concat "parameters to \"" ess-bugs-file-dir ess-bugs-file-root ".in0\"\n"))
	    (insert (concat "update " ess-bugs-default-burn-in "\n"))
	    (insert (concat "parameters to \"" ess-bugs-file-dir ess-bugs-file-root ".in1\"\n"))
	    (insert ess-bugs-monitor-vars)
	    (insert (concat "update " ess-bugs-default-update "\n"))
	    (insert (concat "parameters to \"" ess-bugs-file-dir ess-bugs-file-root ".in2\"\n"))
	    (insert (concat "coda " 
		(if ess-microsoft-p (if (w32-shell-dos-semantics) "*" "\\*") "\\*") 
		", stem(\"" ess-bugs-file-dir ess-bugs-file-root "\")\n"))
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
		ess-bugs-file-dir ess-bugs-file-root ".jmd") 
		(if (or (equal shell-file-name "/bin/csh") 
			(equal shell-file-name "/bin/tcsh")
			(equal shell-file-name "/bin/zsh")) 
			    (concat " >& " ess-bugs-file-dir ess-bugs-file-root ".out ")
			    (concat " > "  ess-bugs-file-dir ess-bugs-file-root ".out 2>&1 ")) 
		(concat ess-bugs-batch-post-command))

	(comint-send-input)
)

(defun ess-bugs-na-bug ()
    "ESS[JAGS]: Perform Next-Action for .bug"

	(if (equal 0 (buffer-size)) (ess-bugs-switch-to-suffix ".bug")
	;else
	    (ess-save-and-set-local-variables)
   
	    (setq ess-bugs-monitor-vars nil)

		(while (and (listp ess-jags-monitor-vars) (consp ess-jags-monitor-vars))
		    (setq ess-bugs-monitor-vars 
			(concat ess-bugs-monitor-vars 
			    "monitor " (car ess-jags-monitor-vars) 
			    ", thin(" ess-jags-thin-vars ")\n"))
		    (setq ess-jags-monitor-vars (cdr ess-jags-monitor-vars)))

	    (ess-bugs-switch-to-suffix ".jmd"))
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
