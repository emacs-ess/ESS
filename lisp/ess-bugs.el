(require 'font-lock)
(require 'comint)


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


(defvar ess-bugs-batch-command "backbugs" 
   "ESS[BUGS]:  Set to name of backbugs script that comes with ESS[BUGS].")

(defvar ess-bugs-batch-tail " " 
   "ESS[BUGS]:  Modifiers at the end of the backbugs command line.")

(defvar ess-file "."
   "ESS:  file with PATH.")

(defvar ess-file-root "."
   "ESS:  Root of file.")

(defvar ess-file-suffix "."
   "ESS:  Suffix of file.")

(defvar ess-file-dir "."
   "ESS:  Directory of file.")


(defun ess-file ()
   "ESS:  Set `ess-file', `ess-file-root', `ess-file-suffix' and `ess-file-dir'."
   (interactive)

   (let ((temp (buffer-name)))
        (setq ess-file (expand-file-name temp))
        (setq ess-file-dir (file-name-directory ess-file))
        (setq ess-file-root (file-name-nondirectory (file-name-sans-extension ess-file)))

        (if (fboundp 'file-name-extension) (setq ess-file-suffix (file-name-extension temp))
	     (setq temp (split-string temp "[.]"))
	     (setq ess-file-suffix (nth (- (length temp) 1) temp)))
	(setq ess-file-suffix (concat "." ess-file-suffix))
   )
)


(defun ess-switch-to-suffix (suffix)
   "ESS:  Switch to file with suffix."
   (find-file (concat ess-file-dir ess-file-root suffix))

   (if (equal 0 (buffer-size)) (progn
	(if (equal ".bug" suffix) (let ((temp 0))
	    (insert (concat "model " ess-file-root ";\n"))
	    (save-excursion
		(find-file (concat ess-file-dir ess-file-root ".dat"))
	        (setq temp (int-to-string (count-lines (point-min) (point-max))))
		(ess-switch-to-suffix ".bug")
	    )
	    (insert (concat "const N = " temp ";\n"))
	    (insert (concat "data  in \"" ess-file-dir ess-file-root ".dat\";\n"))
	    (insert (concat "inits  in \"" ess-file-dir ess-file-root ".in\";\n"))
	    (insert "var ;\n")
	    (insert "{\n")
            (insert "    for (i in 1:N) {\n    \n")
            (insert "    }\n")
            (insert "}\n")
	))

	(if (equal ".cmd" suffix) (progn
	    (insert (concat "compile(\"" ess-file-dir ess-file-root ".bug\")\n"))
	    (insert "update( )\n")
	    (insert "monitor( )\n")
	    (insert "update( )\n")
	    (insert "stats( )\n")
	    (insert "q( )\n")
	))
    ))
)
		

(defun ess-revert ()
  "ESS: Revert from disk if file and buffer modification times are different."
  (interactive)
  
  (if (not(verify-visited-file-modtime (current-buffer)))
	(revert-buffer t t)))


(defun ess-bugs-next-action ()
   "ESS[BUGS]:  Perform the appropriate next action."
   (interactive)
   (ess-file)

   (if (equal ".bug" ess-file-suffix) 
	(if (equal 0 (buffer-size)) (ess-switch-to-suffix ".bug")
	    (save-buffer)
	    (ess-switch-to-suffix ".cmd"))
   )

   (if (equal ".cmd" ess-file-suffix) (progn
	(save-buffer)
	(shell)
	(insert (concat ess-bugs-batch-command " " ess-file-root " " ess-file " " ess-bugs-batch-tail))
	(comint-send-input)
	(ess-switch-to-suffix ".log")
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


(provide 'ess-bugs)
    
