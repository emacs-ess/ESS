;;; essa-sas.el -- ESS local customizations for SAS, part a.

;; Copyright (C) 1997--2000 Rodney Sparapani, A.J. Rossini, 
;; Martin Maechler, Kurt Hornik, and Richard M. Heiberger.

;; Author: Rodney Sparapani <rodney.sparapani@duke.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 17 November 1999
;; Modified: $Date: 2000/02/14 14:15:02 $
;; Version: $Revision: 1.3 $
;; RCS: $Id: essa-sas.el,v 1.3 2000/02/14 14:15:02 rossini Exp $

;; Keywords: ESS, ess, SAS, sas, asynchronous.

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



; logical window setting for ESS

;(setq same-window-buffer-names (append same-window-buffer-names
;	(list "*Async Shell Command*" "*ESS*"))
;)


; assign characters to punctuation, words or comments for syntax highlighting

(setq SAS-syntax-table (make-syntax-table))

  (if (equal system-type 'windows-nt)
      (modify-syntax-entry ?\\ "."  SAS-syntax-table)  ;; backslash is punctuation (used in MS file names)
    (modify-syntax-entry ?\\ "\\" SAS-syntax-table)  ;; backslash is an escape character
    )
  (modify-syntax-entry ?+  "."  SAS-syntax-table)
  (modify-syntax-entry ?-  "."  SAS-syntax-table)
  (modify-syntax-entry ?=  "."  SAS-syntax-table)
  (modify-syntax-entry ?%  "w"  SAS-syntax-table)
  (modify-syntax-entry ?<  "."  SAS-syntax-table)
  (modify-syntax-entry ?>  "."  SAS-syntax-table)
  (modify-syntax-entry ?&  "w"  SAS-syntax-table)
  (modify-syntax-entry ?|  "."  SAS-syntax-table)
  (modify-syntax-entry ?\' "\"" SAS-syntax-table)
  (modify-syntax-entry ?*  ". 23"  SAS-syntax-table) ; comment character
  (modify-syntax-entry ?\; "."  SAS-syntax-table) 
  (modify-syntax-entry ?_  "w"  SAS-syntax-table)  
  (modify-syntax-entry ?<  "."  SAS-syntax-table)
  (modify-syntax-entry ?>  "."  SAS-syntax-table)
  (modify-syntax-entry ?/  ". 14"  SAS-syntax-table) ; comment character
  (modify-syntax-entry ?.  "w"  SAS-syntax-table)


; assign each SAS keyword to a syntax highlighting type

(setq SAS-mode-font-lock-keywords
  '(

    ; SAS comments

    ("^[ \t]*%?\\*.*;"		    . font-lock-comment-face)
    (";[ \t]*%?\\*.*;"		    . font-lock-comment-face)
    ("/\\*\\([^*/]\\)*\\*/"  0 font-lock-comment-face t)


    ; SAS execution blocks, DATA/RUN, PROC/RUN, %MACRO/%MEND

    ("\\<\\(data\\|run\\|%macro\\|%mend\\)\\>" 
	. font-lock-reference-face)

    ("\\<proc[ \t]+[a-z][a-z_0-9]+"
	. font-lock-reference-face)


    ; SAS statements

    ("\\<\\(abort\\|array\\|attrib\\|by\\|delete\\|display\\|dm\\)\\>"
	. font-lock-keyword-face)

    ("\\<%?do\\([ \t]+\\(over\\|%?until\\|%?while\\)\\)?\\>"
	. font-lock-keyword-face)

    ("\\<\\(drop\\|error\\|file\\|filename\\|footnote\\(10?\\|[2-9]\\)?\\)\\>"
	. font-lock-keyword-face)

	("\\<\\(format\\|%?go[ \t]*to\\|%?if\\|%?then\\|%?else\\)\\>"
	. font-lock-keyword-face)

    ("\\<\\(infile\\|informat\\|%?input\\|keep\\|label\\)\\>"
	. font-lock-keyword-face)

    ("\\<\\(length\\|libname\\|link\\)\\>"
	. font-lock-keyword-face)

    ("\\<\\(merge\\|missing\\|modify\\|note\\|g?options\\|output\\)\\>"
	. font-lock-keyword-face)

    ("\\<\\(otherwise\\|%?put\\|rename\\|retain\\|select\\|when\\|set\\)\\>"
	. font-lock-keyword-face)

    ("\\<\\(skip\\|title\\(10?\\|[2-9]\\)?\\|where\\|window\\|update\\)\\>"
	. font-lock-keyword-face)


    ; SAS statements that must be followed by a semi-colon

    ("\\<\\(cards4?\\|%?end\\|endsas\\|list\\|lostcard\\|page\\|return\\|stop\\)[ \t]*;"
	. font-lock-keyword-face)


    ; SAS macro statements not handled above

    ("\\<\\(%global\\|%include\\|%local\\|%let\\|%sysexec\\)\\>"
	. font-lock-keyword-face)


	; SAS/GRAPH statements not handled above

    ("\\<\\(axis\\|legend\\|pattern\\|symbol\\)\\([1-9][0-9]?\\)?\\>"
	. font-lock-keyword-face)


	; SAS functions and SAS macro functions

    ("%[a-z_][a-z_0-9]*[ \t]*[(;]"
	. font-lock-function-name-face)

    ("\\<call[ \t]+[a-z_][a-z_0-9]*[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(abs\\|arcos\\|arsin\\|atan\\|betainv\\|byte\\|ceil\\|cinv\\)[ \t]*("
	. font-lock-function-name-face)

	("\\<\\(collate\\|compress\\|cosh?\\|css\\|cv\\)[ \t]*("
	. font-lock-function-name-face)

	("\\<\\(\\(dacc\\|dep\\)\\(db\\|dbsl\\|sl\\|syd\\|tab\\)\\)[ \t]*("
	. font-lock-function-name-face)

	("\\<\\(date\\(jul\\|part\\|time\\)?\\|day\\|d?hms\\|dif\\)[ \t]*("
	. font-lock-function-name-face)

	("\\<\\(digamma\\|dim\\|erfc?\\|exp\\|finv\\)[ \t]*("
	. font-lock-function-name-face)

	("\\<\\(fip\\(namel?\\|state\\)\\|floor\\|fuzz\\|gaminv\\|gamma\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(hbound\\|hour\\|indexc?\\|input\\|int\\(ck\\|nx\\|rr\\)?\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(irr\\|juldate\\|kurtosis\\|lag\\|lbound\\|left\\|length\\)[ \t]*("
	. font-lock-function-name-face)

	("\\<\\(lgamma\\|log\\(10\\|2\\)?\\|max\\|mdy\\|mean\\|min\\|minute\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(mod\\|month\\|mort\\|n\\|netpv\\|nmiss\\|normal\\|npv\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(prob\\(beta\\|bnml\\|chi\\|f\\|gam\\|hypr\\|it\\|negb\\|norm\\|t\\)\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(ordinal\\|poisson\\|put\\|qtr\\|range\\|rank\\|repeat\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(ran\\(bin\\|cau\\|exp\\|gam\\|nor\\|poi\\|tbl\\|tri\\|uni\\)\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(reverse\\|right\\|round\\|saving\\|scan\\|second\\|sign\\|sinh?\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(sqrt\\|std\\|stderr\\|st\\(fips\\|namel?\\)\\|substr\\|sum\\)[ \t]*("
	. font-lock-function-name-face)

	("\\<\\(symget\\|tanh?\\|time\\(part\\)?\\|tinv\\|today\\|translate\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(trigamma\\|trim\\|trunc\\|uniform\\|upcase\\|uss\\|var\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(verify\\|weekday\\|year\\|yyq\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(zip\\(fips\\|namel?\\|state\\)\\)[ \t]*("
	. font-lock-function-name-face)


    ; SAS functions introduced in Technical Report P-222

    ("\\<\\(airy\\|band\\|b[lr]shift\\|bnot\\|bor\\|bxor\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\([cft]nonct\\|compbl\\|dairy\\|dequote\\|[ij]bessel\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(indexw\\|input[cn]\\|int\\(ck\\|nx\\)\\|lowcase\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(put[cn]\\|quote\\|resolve\\|soundex\\|sysprod\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(tranwrd\\|trimn\\)[ \t]*("
	. font-lock-function-name-face)


	; SCL functions that are known to work with SAS macro function %sysfunc

    ("\\<\\(%sysfunc\\|attr[cn]\\|cexist\\|close\\|dclose\\|dnum\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(dopen\\|dread\\|exist\\|fclose\\|fetchobs\\|fileexist\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(filename\\|finfo\\|fopen\\|fput\\|fwrite\\|getoption\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(getvar[cn]\\|libname\\|libref\\|open\\|optgetn\\|optsetn\\)[ \t]*("
	. font-lock-function-name-face)

    ("\\<\\(pathname\\|sysmsg\\|varfmt\\|varlabel\\|varnum\\|vartype\\)[ \t]*("
	. font-lock-function-name-face)


	; SAS PROC statements not handled above 

    ("\\<\\(change\\|class\\|exchange\\|exclude\\|freq\\|id\\|index\\)\\>"
	. font-lock-keyword-face)
	
    ("\\<\\(model\\|plot\\|save\\|sum\\|tables?\\|var\\|weight\\|with\\)\\>"
	. font-lock-keyword-face)
	
  )
)


; re-compute tab stops and assign appropriate functions to tab and return keys

(add-hook 'ess-mode-hook 
	(lambda ()
		(ess-sas-tab-stop)
		(define-key ess-mode-map [tab] 'tab-to-tab-stop)
	)
)


(require 'ess-site)


(defvar ess-sas-root "."
	"Root of the file to perform operations on."
)


(defvar ess-sas-dir "."
	"Directory where .sas, .log, .lst and .txt files are stored"
)


(defvar ess-sas-sas "."
	"The .sas file to perform operations on."
)


(defvar ess-sas-log "."
	"The .log file to perform operations on."
)


(defvar ess-sas-lst "."
	"The .lst file to perform operations on."
)


(defvar ess-sas-txt "."
	"The .txt file to perform operations on."
)


(defvar ess-submit-SAS-program-name "sas"
	"Command to invoke SAS."
)


(defun ess-sas-file (ess-sas-arg)
	"Create buffer and associate with the appopriate file"

	(if (not (get-file-buffer ess-sas-arg)) (progn
		(create-file-buffer ess-sas-arg)
		(switch-to-buffer ess-sas-arg)
		(if (= emacs-major-version 19)
		    (set-visited-file-name (concat ess-sas-dir ess-sas-arg))
		  (set-visited-file-name (concat ess-sas-dir ess-sas-arg) t))
		(ess-sas-revert)
		)
	(switch-to-buffer ess-sas-arg)
	)
)


(defun ess-sas-set ()
	"Set ess-sas-root, ess-sas-dir, ess-sas-sas, ess-sas-log, ess-sas-lst and ess-sas-txt"
	(if (or (string= ".sas" (substring (buffer-name) -4)) 
		(string= ".log" (substring (buffer-name) -4))
		(string= ".lst" (substring (buffer-name) -4))
		(string= ".txt" (substring (buffer-name) -4))
		) 

		(progn
			(setq ess-sas-root (substring (buffer-name) 0 -4))
			(setq ess-sas-dir (file-name-directory (buffer-file-name)))
			(setq ess-sas-sas (concat ess-sas-root ".sas"))
			(setq ess-sas-log (concat ess-sas-root ".log"))
			(setq ess-sas-lst (concat ess-sas-root ".lst"))
			(setq ess-sas-txt (concat ess-sas-root ".txt"))


		)
	)
)


(defun ess-sas-submit ()
	"Save the .sas file and submit to shell."
	(interactive)
	(ess-sas-set)
	(if (not (string= ess-sas-sas (buffer-name))) (ess-sas-file ess-sas-sas))
	(save-buffer)
	(shell-command (concat ess-submit-SAS-program-name " " ess-sas-root " &"))
	(switch-to-buffer ess-sas-sas)
)


(defun ess-sas-goto-sas ()
	"Switch to the .sas file"
	(interactive)
	(ess-sas-set)
	(ess-sas-file ess-sas-sas)
)


(defun ess-sas-revert ()
	"Revert from disk with no questions asked."
	(interactive)
	(ess-sas-set)

	(if (fboundp 'vc-backend-deduce)
	    (if (vc-backend-deduce ess-sas-sas) (vc-revert-buffer)
	      (revert-buffer t t))
	  (if (vc-backend ess-sas-sas) (vc-revert-buffer)
	    (revert-buffer t t)))
)


(defun ess-sas-goto-log ()
	"Switch to the .log file, revert from disk and search for ^ERROR messages."
	(interactive)
	(ess-sas-set)

	(if (not (string= ess-sas-log (buffer-name))) (progn
		(ess-sas-file ess-sas-log)
		(goto-char 1)
	))

	(ess-sas-revert)
	(setq case-fold-search nil)
	(search-forward-regexp "^ERROR [0-9]+-[0-9]+:\\|^ERROR:\\|_ERROR_=1 _\\|_ERROR_=1$\\|NOTE: MERGE statement has more than one data set with repeats of BY values.\\|NOTE: Variable .* is uninitialized.\\|WARNING: Apparent symbolic reference .* not resolved." nil t)
)


(defun ess-sas-goto-lst ()
	"Switch to the .lst file and revert from disk."
	(interactive)
	(ess-sas-set)
	(ess-sas-file ess-sas-lst)
	(ess-sas-revert)
)


(defun ess-sas-goto-txt ()
	"Switch to the .txt file and revert from disk."
	(interactive)
	(ess-sas-set)
	(ess-sas-file ess-sas-txt)
	(ess-sas-revert)
)


(defun ess-sas-goto-shell ()
	"Switch to the asynchronous shell buffer"
	(interactive)
	(switch-to-buffer "*Async Shell Command*")
)


(defun ess-sas-tab-stop ()
	"Initializes the the tab-stop-list."
	(interactive)

	(let ((ess-sas-tab sas-indent-width))
		(setq tab-stop-list (list sas-indent-width))

		(while (< ess-sas-tab 120)
			(setq ess-sas-tab (+ ess-sas-tab sas-indent-width))
			(setq tab-stop-list (append tab-stop-list (list ess-sas-tab)))
		)
	)
)


(defun ess-sas-backwards-tab ()
	"Moves the cursor to the previous tab-stop."
	(interactive)

	(let* (
		; current-column
		(ess-sas-column (current-column))

		; remainder of current-column and sas-indent-width
		(ess-sas-remainder (% ess-sas-column sas-indent-width))
		)

		(if (not (= ess-sas-column 0)) 
			(progn
				(if (= ess-sas-remainder 0) 
					(setq ess-sas-remainder sas-indent-width)
				)

				(backward-delete-char-untabify ess-sas-remainder t)
				(move-to-column (- ess-sas-column ess-sas-remainder))
			)
		)
	)
)


(global-set-key [C-tab] 'ess-sas-backwards-tab)
(global-set-key [f2] 'ess-sas-revert)


;Unix/Mainframe-like SAS key definitions

(global-set-key [f3] 'ess-sas-submit)
(global-set-key [f4] 'ess-sas-goto-sas)
(global-set-key [f5] 'ess-sas-goto-log)
(global-set-key [f6] 'ess-sas-goto-lst)
(global-set-key [f7] 'ess-sas-goto-txt)
(global-set-key [f8] 'ess-sas-goto-shell)


;PC-like SAS key definitions

;(global-set-key [f3] 'ess-sas-goto-shell)
;(global-set-key [f4] 'ess-sas-goto-txt)
;(global-set-key [f5] 'ess-sas-goto-sas)
;(global-set-key [f6] 'ess-sas-goto-log)
;(global-set-key [f7] 'ess-sas-goto-lst)
;(global-set-key [f8] 'ess-sas-submit)



