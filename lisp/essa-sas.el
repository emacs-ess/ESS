;;; essa-sas.el -- ESS local customizations for SAS, part a.

;; Copyright (C) 1997--2002 Rodney A. Sparapani, A.J. Rossini, 
;; Martin Maechler, Kurt Hornik, and Richard M. Heiberger.

;; Author: Rodney A. Sparapani <rsparapa@mcw.edu>
;; Maintainer: Rodney A. Sparapani <rsparapa@mcw.edu>, 
;;             A.J. Rossini <rossini@u.washington.edu>
;; Created: 17 November 1999
;; Modified: $Date: 2004/03/11 15:21:25 $
;; Version: $Revision: 1.155 $
;; RCS: $Id: essa-sas.el,v 1.155 2004/03/11 15:21:25 rsparapa Exp $

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

(defvar ess-sas-file-path "."
    "Full path-name of the sas file to perform operations on.")

(defcustom ess-sas-data-view-libname " "
    "*SAS code to define a library for `ess-sas-data-view'."
    :group 'ess-sas  
    :type  'string
)

(defcustom ess-sas-data-view-fsview-command "; proc fsview data=" 
    "*SAS code to open a SAS dataset with `ess-sas-data-view'."
    :group 'ess-sas  
    :type  'string
)

(defcustom ess-sas-data-view-fsview-statement " "
    "*SAS code to perform a PROC FSVIEW statement with `ess-sas-data-view'."
    :group 'ess-sas  
    :type  'string
)

(make-variable-buffer-local 'ess-sas-data-view-fsview-statement)

(defcustom ess-sas-graph-suffix-regexp 
    "[.]\\([eE]?[pP][sS]\\|[gG][iI][fF]\\|[jJ][pP][eE]?[gG]\\|[tT][iI][fF][fF]?\\)"
    "*GSASFILE suffix regexp."
    :group 'ess-sas  
    :type  'string
)

;;(defcustom ess-sas-smart-back-tab nil
;;    "*Set to t to make C-TAB insert an end/%end; statement to close a block."
;;    :group 'ess-sas
;;)

(defcustom ess-sas-shell-buffer "*shell*"
"*Name that you want to use for the shell buffer; buffer-local."
    :group 'ess-sas
    :type  'string
)

(make-variable-buffer-local 'ess-sas-shell-buffer)

(defcustom ess-sas-shell-buffer-remote-host nil 
"*Remote host that you want to open a shell on."
    :group 'ess-sas
    :type  'string
)

(make-variable-buffer-local 'ess-sas-shell-buffer-remote-host)

(defcustom ess-sas-shell-buffer-remote-init "ssh" 
"*Command to open a shell on a remote host."
    :group 'ess-sas
    :type  'string
)

(make-variable-buffer-local 'ess-sas-shell-buffer-remote-init)

(defcustom ess-sas-submit-mac-virtual-pc nil
"*Non-nil means that you want to run Windows SAS in a
Virtual PC emulator on your Mac; buffer-local."
    :group 'ess-sas)

(make-variable-buffer-local 'ess-sas-submit-mac-virtual-pc)

(defcustom ess-sas-submit-command sas-program
    "*Command to invoke SAS in batch; buffer-local."
    :group 'ess-sas  
    :type  'string
)

(make-variable-buffer-local 'ess-sas-submit-command)

(defcustom ess-sas-submit-command-options " "
    "*Options to pass to SAS in batch; buffer-local."
    :group 'ess-sas  
    :type  'string
)

(make-variable-buffer-local 'ess-sas-submit-command-options)

(defvar ess-sas-submit-method 
  (if ess-microsoft-p 
    (if (w32-shell-dos-semantics) 'ms-dos 'sh)
    (if (or (equal system-type 'Apple-Macintosh) 
	    (and ess-sas-submit-mac-virtual-pc (equal system-type 'darwin)))
	'apple-script 'sh))
"Method used by `ess-sas-submit'.
The default is based on the value of the emacs variable `system-type'
and, on Windows, the function `w32-shell-dos-semantics'.
'sh               if *shell* runs sh, ksh, csh, tcsh or bash
'ms-dos           if *shell* follows MS-DOS semantics
'apple-script     *shell* unavailable in Mac Classic, use AppleScript,
                  also for Windows SAS in Virtual PC on Mac OS X 

Unix users will get 'sh by default.  

Windows users running bash in *shell* will get 'sh by default.

Windows users running MS-DOS in *shell* will get 'ms-dos by default.

Users accessing a remote machine with `telnet', `rlogin', `ssh', etc.,
should set this variable to 'sh regardless of their local shell 
(since their remote shell is 'sh).")

(make-variable-buffer-local 'ess-sas-submit-method)

(defcustom ess-sas-data-view-submit-options 
    (if ess-microsoft-p "-noenhancededitor -nosysin -log NUL:"
	"-nodms -nosysin -log /dev/null")
    "*The options necessary for your enviromment and your operating system."
    :group 'ess-sas  
    :type  'string
)

(defcustom ess-sas-image-viewer
    (if ess-microsoft-p "kodakimg" 
	(if (equal ess-sas-submit-method 'sh) "sdtimage"))
    "*Application to view GSASFILE."
    :group 'ess-sas  
    :type  'string
)

(defcustom ess-sas-submit-post-command 
    (if (equal ess-sas-submit-method 'sh) "-rsasuser &" 
	(if ess-microsoft-p "-rsasuser -icon"))    
    "*Command-line statement to post-modify SAS invocation, e.g. -rsasuser"
    :group 'ess-sas  
    :type  'string
)

(defcustom ess-sas-submit-pre-command 
    (if (equal ess-sas-submit-method 'sh) 
	;; nice is tricky, higher numbers give you lower priorities
	;; if you are using csh/tcsh, the default priority is 4
	;; if you are using most other shells, the default priority is 10,
	;; and some implementations are higher, i.e. zsh unless you 
	;; specify "setopt no_bg_nice" in your ~/.zshrc
	;; therefore, on the same machine, you can run at a higher or
	;; lower priority by changing shells, although, the command
	;; line is the same!
	;; the following code should give you a priority of 10 regardless
	;; of which shell is in use, but it will default to the old
	;; behavior if csh or variant is not recognized
	;; this should avoid the necessity of each user needing to set this
	;; variable correctly based on the shell that they use and provide
	;; an environment where all users are treated equally
	(let* ((temp-shell (getenv "SHELL"))
	       (temp-char (string-match "/" temp-shell)))
            
	    (while temp-char 
		(setq temp-shell (substring temp-shell (+ 1 temp-char)))
		(setq temp-char (string-match "/" temp-shell)))

	    (cond ((or (equal temp-shell "csh") (equal temp-shell "tcsh")) 
			"nohup nice +6")
		   (t "nohup nice")))
	(if ess-microsoft-p "start"))
    "*Command-line statement to precede SAS invocation, e.g. start or nohup"
    :group 'ess-sas  
    :type  'string
)

(defcustom ess-sas-suffix-1 "txt"
    "*The first suffix to associate with SAS."
    :group 'ess-sas  
    :type  'string
)

(defcustom ess-sas-suffix-2 "dat"
    "*The second suffix to associate with SAS."
    :group 'ess-sas  
    :type  'string
)

(defcustom ess-sas-suffix-regexp 
    (concat "[.]\\([sS][aA][sS]\\|[lL][oO][gG]\\|[lL][sS][tT]"
	(if ess-sas-suffix-1 (concat 
	    "\\|" (downcase ess-sas-suffix-1) "\\|" (upcase ess-sas-suffix-1)))
	(if ess-sas-suffix-2 (concat 
	    "\\|" (downcase ess-sas-suffix-2) "\\|" (upcase ess-sas-suffix-2)))
	"\\)")
    "*Regular expression for SAS suffixes."
    :group 'ess-sas  
    :type  'string
)

(defcustom ess-sleep-for (if ess-microsoft-p 5 0)
"*`ess-sas-submit-sh' may need to pause before sending output 
to the shell on Windows when `ess-sas-submit-method' is 'sh."
    :group 'ess-sas  
    :type  'number
)

(defvar ess-sas-tab-stop-alist
 '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
  "List of tab stop positions used by `tab-to-tab-stop' in ESS[SAS].")

(defcustom ess-sas-temp-root "ess-temp"
    "*The root of the temporary .sas file for `ess-sas-submit-region'."
    :group 'ess-sas  
    :type  'string
)


;;; Section 2:  Function Definitions


(defun ess-exit-notify-sh (string)
"Detect completion or failure of submitted job and notify the user."
  (let* ((exit-done "\\[[0-9]+\\]\\ *\\+*\\ *\\(Exit\\|Done\\).*$")
	 (beg (string-match exit-done string)))
    (if beg
	(message (substring string beg (match-end 0))))))



(defun ess-sas-append-log ()
    "Append ess-temp.log to the current .log file."
    (interactive)
    (ess-sas-goto "log" 'revert)
    (goto-char (point-max))
    (insert-file-contents (concat ess-sas-temp-root ".log"))
    (save-buffer))

(defun ess-sas-append-lst ()
    "Append ess-temp.lst to the current .lst file."
    (interactive)
    (ess-sas-goto "lst" 'revert)
    (goto-char (point-max))
    (insert-file-contents (concat ess-sas-temp-root ".lst"))
    (save-buffer))

(defun ess-sas-backward-delete-tab ()
  "Moves the cursor to the previous tab-stop, deleting any characters
on the way."
  (interactive)
  
  (let* (;; point of search 
	 ;;(ess-sas-search-point nil)
	 ;; column of search 
	 ;;(ess-sas-search-column nil)
	 ;; limit of search 
	 ;;(ess-sas-search-limit nil)
	 ;; text to be inserted after a back-tab, if any
	 ;;(ess-sas-end-text "end;")
	 ;; current-column
	 (ess-sas-column (current-column))
	 ;; remainder of current-column and sas-indent-width
	 (ess-sas-remainder (% ess-sas-column sas-indent-width)))

    (if (not (= ess-sas-column 0)) 
	(progn
	  (if (= ess-sas-remainder 0) 
	      (setq ess-sas-remainder sas-indent-width))
	  
	  (backward-delete-char-untabify ess-sas-remainder t)
	  (setq ess-sas-column (- ess-sas-column ess-sas-remainder))
	  (move-to-column ess-sas-column)
	  (setq left-margin ess-sas-column)
    ))
))

;; this feature was far too complicated to perfect
;;      (if ess-sas-smart-back-tab (progn
;;	  (save-excursion
;;	    (setq ess-sas-search-point	    
;;		(search-backward-regexp "end" nil t))

;;	    (if (and ess-sas-search-point
;;		(search-backward-regexp "%" (+ ess-sas-search-point -1) t))
;;		(setq ess-sas-search-point (+ ess-sas-search-point -1))
;;	    )
		
;;	    (if (and ess-sas-search-point
;;		(not (equal ess-sas-column (current-column))))
;;		(setq ess-sas-search-point nil))
;;	    )

;;	  (save-excursion
;;	    (setq ess-sas-search-point	    
;;		(search-backward-regexp "do\\|select" 
;;		    ess-sas-search-point t))

;;	    (setq ess-sas-search-column (current-column))

;;	    (if ess-sas-search-point (progn
;;		(save-excursion
;;		 (search-backward-regexp "^" nil t)
;;		 (setq ess-sas-search-limit (point))
;;		)

;;	        (if (search-backward-regexp "if.*then\\|else" ess-sas-search-limit t)
;;		    (setq ess-sas-search-point (point)))

;;	        (if (search-backward-regexp "%" ess-sas-search-limit t) (progn
;;		    (setq ess-sas-end-text "%end;")
;;		    (setq ess-sas-search-point (point))
;;		))

;;		(setq ess-sas-search-column (current-column))

;;	        (if (not (equal ess-sas-column ess-sas-search-column))
;;		   (setq ess-sas-search-point nil))
;;	  )))

;;	  (if ess-sas-search-point (insert ess-sas-end-text))
;;         ))

(defun ess-sas-create-local-variables-alist (&optional file-or-buffer)
"Create an alist of local variables from file-or-buffer, use the 
current buffer if nil."

(if file-or-buffer (set-buffer (ess-get-file-or-buffer file-or-buffer)))

(ess-change-alist 'ess-kermit-remote-directory ess-kermit-remote-directory nil))

(defun ess-sas-data-view (&optional ess-sas-data)
  "Open a dataset for viewing with PROC FSVIEW."
    (interactive)
    (ess-save-and-set-local-variables)

 (save-excursion (let ((ess-tmp-sas-data nil) 
    (ess-tmp-sas-data-view-fsview-statement ess-sas-data-view-fsview-statement)
    (ess-search-regexp 
    "[ \t=]\\([a-zA-Z_][a-zA-Z_0-9]*[.][a-zA-Z_][a-zA-Z_0-9]*\\)\\(&.*\\)?[ ,()\t;]")
    (ess-search-except 
    "^\\([wW][oO][rR][kK]\\|[fF][iI][rR][sS][tT]\\|[lL][aA][sS][tT]\\)[.]"))

    (if ess-sas-data nil (save-match-data 
	(search-backward-regexp "[ \t=]" nil t)

        (save-excursion 
	    (setq ess-tmp-sas-data 
		(ess-search-except ess-search-regexp ess-search-except)))

        (if (not ess-tmp-sas-data) 
	    (setq ess-tmp-sas-data 
		(ess-search-except ess-search-regexp ess-search-except t)))

	(setq ess-sas-data (read-string "Permanent SAS Dataset: " ess-tmp-sas-data))

        (ess-sas-goto-shell t)

	(insert (concat ess-sas-submit-pre-command " " ess-sas-submit-command 
	    " -initstmt \"" ess-sas-data-view-libname ess-sas-data-view-fsview-command 
	    ess-sas-data ";" ess-tmp-sas-data-view-fsview-statement "; run;\" " 
	    ess-sas-data-view-submit-options " " ess-sas-submit-post-command))
    (comint-send-input)
)))))

(defun ess-sas-graph-view ()
  "Open a GSASFILE for viewing."
  (interactive)
;  (ess-sas-file-path)
  (ess-sas-goto-log 'no-error-check)

  (save-excursion (let ((ess-tmp-sas-graph nil)
        (ess-tmp-sas-glyph nil)
        (ess-tmp-sas-graph-regexp 
	    (concat " RECORDS WRITTEN TO \\(.*" ess-sas-graph-suffix-regexp "\\)")))
;	    (concat "['\"]\\(.*" ess-sas-graph-suffix-regexp "\\)['\"]")))

    (save-match-data 
       (search-backward-regexp "[ \t=]" nil t)

       (save-excursion 
	    (setq ess-tmp-sas-graph (ess-search-except ess-tmp-sas-graph-regexp)))

        (if (not ess-tmp-sas-graph) 
	    (setq ess-tmp-sas-graph (ess-search-except ess-tmp-sas-graph-regexp nil t)))

	(setq ess-tmp-sas-graph (read-string "GSASFILE: " 
	    (or ess-tmp-sas-graph ess-sas-file-path)))
;	    (or ess-tmp-sas-graph (file-name-nondirectory ess-sas-file-path))))

;	    (setq ess-tmp-sas-graph (convert-standard-filename 
;		(concat (file-name-directory ess-sas-file-path) "/" ess-tmp-sas-graph)))

	  (if (fboundp 'ess-xemacs-insert-glyph) (progn
	      (if (string-match "[.][gG][iI][fF]" ess-tmp-sas-graph)
		 (setq ess-tmp-sas-glyph 'gif)
	      ;;else
	      (if (string-match "[.][jJ][pP][eE]?[gG]" ess-tmp-sas-graph)
		 (setq ess-tmp-sas-glyph 'jpeg)))))

	  (if ess-tmp-sas-glyph (progn
		(switch-to-buffer (file-name-nondirectory ess-tmp-sas-graph))
		(ess-xemacs-insert-glyph 
		    (make-glyph (vector ess-tmp-sas-glyph :file ess-tmp-sas-graph)))
	     )
	  ;;else
	  (if (and (boundp 'auto-image-file-mode) auto-image-file-mode
	      (string-match "[.][jJ][pP][eE]?[gG]" ess-tmp-sas-graph))
	      (find-file ess-tmp-sas-graph)
          ;;else
         
            (ess-sas-goto-shell t)

            (insert ess-sas-submit-pre-command " " ess-sas-image-viewer " " 
	      ess-tmp-sas-graph 
	      (if (equal ess-sas-submit-method 'sh) " &"))
            (comint-send-input))
)))))

(defun ess-sas-file-path ()
 "Define `ess-sas-file-path' to be the current buffer depending on suffix."
  (interactive)

  (save-match-data (let ((ess-sas-temp-file (expand-file-name (buffer-name))))
    (if (string-match ess-sas-suffix-regexp ess-sas-temp-file) ;;(progn
	(setq ess-sas-file-path 
	   (nth 0 (split-string ess-sas-temp-file "[<]")))))))
	;; (setq ess-directory (file-name-directory ess-sas-file-path)))))))

(defun ess-sas-file-path-remote-host ()
"Return the remote host, if any, associated with `ess-sas-file-path'."
(interactive)

(let* ((temp-colon-pos (string-match ":" ess-sas-file-path))
       (temp-list 
	(if (or (not temp-colon-pos) (> temp-colon-pos 2))
		(if (equal ess-sas-file-path ".") nil
		    (split-string (file-name-directory ess-sas-file-path) 
			"\\(@\\|:\\|]\\)"))
	(list ess-sas-file-path)))
       (temp-list-length (length temp-list)))	
    (if (= temp-list-length 1) (setq temp-list nil)
	(if (= temp-list-length 2) (setq temp-list (car temp-list))
	    (setq temp-list (nth 1 temp-list))))

    (if temp-list (setq temp-list 
		(car (last (split-string temp-list "/"))))) 
    temp-list))	

(defun ess-sas-goto (suffix &optional revert no-create)
  "Find a file associated with a SAS file by suffix and revert if necessary."
    (let ((ess-temp-regexp (concat ess-sas-suffix-regexp "\\(@.+\\)?")))
	(save-match-data 
	(if (or (string-match ess-temp-regexp (expand-file-name (buffer-name)))
	
	    (string-match ess-temp-regexp ess-sas-file-path))

	(progn
	    (ess-sas-file-path)

	    (let* (
		(ess-sas-temp-file (replace-match (concat "." suffix) t t 
		    ess-sas-file-path))
		(ess-sas-temp-buff (find-buffer-visiting ess-sas-temp-file))
		(ess-temp-kermit-remote-directory ess-kermit-remote-directory))

	    (if ess-sas-temp-buff (switch-to-buffer ess-sas-temp-buff)
	        (if no-create (setq revert nil) (find-file ess-sas-temp-file)))
	
	    (if (and (not no-create) 
		(or (string-equal suffix "log") (string-equal suffix "lst")))
		(ess-kermit-get (file-name-nondirectory ess-sas-temp-file) 
		    ess-temp-kermit-remote-directory))

	    (if revert (ess-revert-wisely) nil)
))))))

;;(defun ess-sas-file (suffix &optional revert)
;;  "Please use `ess-sas-goto' instead."
;;  (let* ((tail (downcase (car (split-string 
;;	    (car (last (split-string (buffer-name) "[.]"))) "[<]"))))
	;;(if (fboundp 'file-name-extension) (file-name-extension (buffer-name))
	;;		 (substring (buffer-name) -3)))
;;	 (tail-in-tail-list (member tail (list "sas" "log" "lst"
;;			     ess-sas-suffix-1 ess-sas-suffix-2)))
;;	 (root (if tail-in-tail-list (expand-file-name (buffer-name))
;;		 ess-sas-file-path))
;;	 (ess-sas-arg (concat (file-name-sans-extension root) "." suffix))
;;	 (ess-sas-buf (find-buffer-visiting ess-sas-arg)))
;;    (if (equal tail suffix) (if revert (ess-revert-wisely))
;;	(if (not ess-sas-buf) (find-file ess-sas-arg)
;;	    (switch-to-buffer ess-sas-buf)
;;	    (if revert (ess-revert-wisely))))))


(defun ess-sas-goto-file-1 ()
  "Switch to ess-sas-file-1 and revert from disk."
  (interactive)
  (ess-sas-goto ess-sas-suffix-1 'revert))

(defun ess-sas-goto-file-2 ()
  "Switch to ess-sas-file-2 and revert from disk."
  (interactive)
  (ess-sas-goto ess-sas-suffix-2 'revert))

(defun ess-sas-goto-log (&optional ess-tmp-no-error-check)
  "Switch to the .log file, revert from disk and search for error messages."
  (interactive)

  (let ((ess-sas-error (concat "^ERROR [0-9]+-[0-9]+:\\|^ERROR:\\|_ERROR_=1 _\\|_ERROR_=1[ ]?$"
    "\\|NOTE: MERGE statement has more than one data set with repeats of BY values."
    "\\|NOTE: Variable .* is uninitialized."
    "\\|WARNING: Apparent symbolic reference .* not resolved."
    "\\|NOTE 485-185: Informat .* was not found or could not be loaded."
    "\\|WARNING: Length of character variable has already been set."
    "\\|Bus Error In Task\\|Segmentation Violation In Task"))
	(ess-sas-save-point nil))

  (if (ess-sas-goto "log" 'revert) (progn
	(setq ess-sas-save-point (point))
	(goto-char (point-min)))
    (setq ess-sas-save-point (point))
  )

(if ess-tmp-no-error-check (goto-char ess-sas-save-point)
  (if (not (search-forward-regexp ess-sas-error nil t)) 
        (if (search-backward-regexp ess-sas-error nil t) 
            (progn
                (goto-char (point-min))
                (search-forward-regexp ess-sas-error nil t)
            )
	    (goto-char ess-sas-save-point)
        )
    )
))
)

(defun ess-sas-goto-lst ()
  "Switch to the .lst file and revert from disk."
  (interactive)
  (ess-sas-goto "lst" 'revert))

(defun ess-sas-goto-sas (&optional revert)
  "Switch to the .sas file."
  (interactive)
  (ess-sas-goto "sas" revert))

(defun ess-sas-goto-shell (&optional set-buffer)
"Set `ess-sas-file-path' and goto `ess-sas-shell-buffer'.  If
optional argument is non-nil, then set-buffer rather than switch."
  (interactive)
  (ess-sas-file-path)

; The following let* block is an attempt to deal with remote directories.
    (let* ((temp-shell-buffer-remote-host 
	    (or ess-sas-shell-buffer-remote-host (ess-sas-file-path-remote-host)))
	(temp-shell-buffer-remote-init ess-sas-shell-buffer-remote-init)
	(temp-shell-buffer 
	    (if temp-shell-buffer-remote-host 
		(concat "*" temp-shell-buffer-remote-host "*")
		ess-sas-shell-buffer))
)

  (if (get-buffer temp-shell-buffer) 
    (if set-buffer (set-buffer temp-shell-buffer) 
		   (switch-to-buffer temp-shell-buffer))
    (shell)
    (rename-buffer temp-shell-buffer)
    
    (if temp-shell-buffer-remote-host (progn
	(insert (concat 
	    temp-shell-buffer-remote-init " " temp-shell-buffer-remote-host))
	(comint-send-input))
    )

    (if (eq ess-sas-submit-method 'sh)
	(add-hook 'comint-output-filter-functions 'ess-exit-notify-sh)) ;; 19.28
                                          ;; nil t) works for newer emacsen
    )
  )

  (goto-char (point-max))
; (insert "cd " ess-temp-directory)
; (comint-send-input))
)

(defun ess-sas-interactive ()
"And now for something completely different."
    (interactive)
    (ess-sas-file-path)

    (let ((ess-temp-sas-file 
(nth 0 (split-string 
(car (last (split-string ess-sas-file-path "\\([a-zA-Z][a-zA-Z]:\\|]\\)"))) "[.]"))))
;;    (message "%s" ess-temp-sas-file)
    (setq ess-sas-shell-buffer "*iESS[SAS]*") 
    (ess-sas-goto-shell)
    (insert (concat ess-sas-submit-command " " ess-sas-submit-command-options 
        " -altlog " ess-temp-sas-file ".log -altprint "
	    ess-temp-sas-file ".lst -stdio < /dev/tty"))
    (comint-send-input)
    (ess-add-ess-process)
    (ess-sas-goto-sas)
    (setq ess-sas-submit-method 'iESS)
    (setq ess-eval-visibly-p nil)
))
;;(defun ess-sas-interactive ()
;;    (interactive)
;;    (ess-sas-file-path)
;;    (setq ess-sas-submit-method 'iESS)
;;
;;    (let ((ess-temp-stderr " ") (ess-temp-stdout " ") (ess-temp-stdin " "))
;;    (setq ess-sas-shell-buffer "*LOG*")
;;    (ess-sas-goto-shell)
;;    (insert "tty")
;;    (comint-send-input)
;;    (ess-sleep)
;;    (save-excursion (setq ess-temp-stderr (ess-search-except "\\(/dev/[a-z0-9/]+\\)" nil t)))
;;    (setq ess-sas-shell-buffer "*OUTPUT*") 
;;    (ess-sas-goto-shell) 
;;    (insert "tty") 
;;    (comint-send-input) 
;;    (ess-sleep) 
;;    (save-excursion (setq ess-temp-stdout (ess-search-except "\\(/dev/[a-z0-9/]+\\)" nil t)))
;;    (setq ess-sas-shell-buffer "*PROGRAM*") 
;;    (ess-sas-goto-shell)
;;;;    (insert "tty")
;;    (comint-send-input)
;;    (ess-sleep)
;;    (insert "sh")
;;    (comint-send-input)
;;    (ess-sleep)
;;    (save-excursion (setq ess-temp-stdin (ess-search-except "\\(/dev/[a-z0-9/]+\\)" nil t)))
;;    (insert (concat ess-sas-submit-command " " ess-sas-submit-command-options " -stdio <"
;;	ess-temp-stdin " >1 " ess-temp-stdout " >2 " ess-temp-stderr))
;;    (comint-send-input)
;;    (ess-add-ess-process)
;;    (ess-sas-goto-sas)
;;))

(defun ess-sas-kill-buffers ()
"Kill all buffers related to a .sas file."
  (interactive)
  (ess-sas-file-path)
  (ess-sas-goto "log" nil t)
  (kill-buffer nil)  
  (ess-sas-goto "lst" nil t)
  (kill-buffer nil)  
  (ess-sas-goto ess-sas-suffix-1 nil t)
  (kill-buffer nil)  
  (ess-sas-goto ess-sas-suffix-2 nil t)
  (kill-buffer nil)  
  (ess-sas-goto "sas" nil t)
  (kill-buffer nil)  
)

(defun ess-sas-rtf-portrait-1 ()
"Creates an MS RTF portrait file from the current buffer."
    (interactive)
    (ess-sas-file-path)
    
    (if (fboundp 'rtf-export) (let 
	((ess-temp-rtf-file (replace-in-string ess-sas-file-path "[.][^.]*$" ".rtf")))
	    ;(expand-file-name (buffer-name)) "[.][^.]*$" ".rtf")))
	(rtf-export ess-temp-rtf-file)
	(ess-sas-goto "rtf" t)
	(goto-char (point-min))
	(replace-regexp "\\\\fmodern .*;" "\\\\fmodern courier;" )
        (goto-char (point-min))
	    
        (while (replace-regexp "\\\\fs[0-9]+" "\\\\fs18" ) nil)
	    
        (save-buffer))))

(defun ess-sas-rtf-us-landscape-1 ()
"Creates an MS RTF US landscape file from the current buffer."
    (interactive)
    (ess-sas-rtf-portrait-1)
    (ess-sas-goto "rtf" t)
    (goto-char (point-min))
    (forward-line 3)
    (insert (concat "{\\*\\pgdsctbl\n"
"{\\pgdsc0\\pgdscuse195\\lndscpsxn\\pgwsxn15840\\pghsxn12240\\marglsxn1800\\margrsxn1800\\margtsxn1440\\margbsxn1440\\pgdscnxt0 Default;}}\n"
"\\landscape\\paperh12240\\paperw15840\\margl1800\\margr1800\\margt1440\\margb1440\\sectd\\sbknone\\lndscpsxn\\pgwsxn15840\\pghsxn12240\\marglsxn1800\\margrsxn1800\\margtsxn1440\\margbsxn1440\\ftnbj\\ftnstart1\\ftnrstcont\\ftnnar\\aenddoc\\aftnrstcont\\aftnstart1\\aftnnrlc\n")) 
    (save-buffer))

(defun ess-sas-rtf-a4-landscape-1 ()
"Creates an MS RTF A4 landscape file from the current buffer."
    (interactive)
    (ess-sas-rtf-portrait-1)
    (ess-sas-goto "rtf" t)
    (goto-char (point-min))
    (forward-line 3)
    (insert (concat "{\\*\\pgdsctbl\n"
"{\\pgdsc0\\pgdscuse195\\lndscpsxn\\pgwsxn16837\\pghsxn11905\\marglsxn1800\\margrsxn1800\\margtsxn1440\\margbsxn1440\\pgdscnxt0 Default;}}\n"
"\\landscape\\paperh11905\\paperw16837\\margl1800\\margr1800\\margt1440\\margb1440\\sectd\\sbknone\\lndscpsxn\\pgwsxn16837\\pghsxn11905\\marglsxn1800\\margrsxn1800\\margtsxn1440\\margbsxn1440\\ftnbj\\ftnstart1\\ftnrstcont\\ftnnar\\aenddoc\\aftnrstcont\\aftnstart1\\aftnnrlc\n"))
    (save-buffer))

(defun ess-sas-submit ()
  "Save the .sas file and submit to shell using a function that
depends on the value of  `ess-sas-submit-method'"
  (interactive)
  (ess-sas-file-path)
  (ess-sas-goto-sas)
  (ess-save-and-set-local-variables)

  (cond
   ((eq ess-sas-submit-method 'apple-script) 
	(ess-sas-submit-mac ess-sas-submit-command 
	    ess-sas-submit-command-options))
   ((eq ess-sas-submit-method 'ms-dos) 
	(ess-sas-submit-windows ess-sas-submit-command 
	    ess-sas-submit-command-options))
   ((eq ess-sas-submit-method 'iESS) 
	(ess-sas-submit-iESS ess-sas-submit-command 
	    ess-sas-submit-command-options))
   ((eq ess-sas-submit-method 'sh) 
	(ess-sas-submit-sh ess-sas-submit-command 
	    ess-sas-submit-command-options)) 
   (t (ess-sas-submit-sh ess-sas-submit-command 
	ess-sas-submit-command-options)))
;  (ess-sas-goto-sas)
)

(defun ess-sas-submit-iESS (arg1 arg2)
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
  ;;  (ess-eval-linewise (concat "cd  default-directory))
  (ess-force-buffer-current "Process to load into: ")
  (ess-eval-linewise 
    (concat "cd " (car (last 
	(split-string (file-name-directory ess-sas-file-path) "\\(:\\|]\\)")))))
  (ess-eval-linewise (concat arg1 " " arg2 " " (buffer-name) " &")))

(defun ess-sas-submit-mac (arg1 arg2)
"If you are using Mac SAS, then arg1, `ess-sas-submit-command', should be 
the AppleScript command \"invoke SAS using program file\", and, if necessary,
arg2, `ess-sas-submit-command-options', is a string of the form 
\"with options { \\\"option-1\\\", \\\"option-2\\\", etc.}\".  If you are
using Windows SAS with the PC emulator Virtual PC, then `ess-sas-submit-command'
should be ..."
  ;(ess-save-and-set-local-variables)

  (do-applescript (concat arg1 " \""
     (if (not ess-sas-submit-mac-virtual-pc) 
	    (unix-filename-to-mac default-directory))
	(buffer-name) "\"" arg2)))

(defun ess-sas-submit-region ()
    "Write region to temporary file, and submit to SAS."
    (interactive)
    (ess-sas-file-path)
    (write-region (region-beginning) (region-end) 
	(concat ess-sas-temp-root ".sas"))

    (save-excursion 
      (ess-sas-goto-shell t)

    (if (and (w32-shell-dos-semantics)
	(string-equal ":" (substring ess-sas-file-path 1 2)))
	(progn
		(insert (substring ess-sas-file-path 0 2))
		(comint-send-input)
    ))

    (insert "cd \"" (convert-standard-filename 
	(file-name-directory ess-sas-file-path)) "\"")
    (comint-send-input)

    (insert (concat ess-sas-submit-pre-command " " ess-sas-submit-command 
          " " ess-sas-temp-root " " ess-sas-submit-post-command))
    (comint-send-input)
    )
)

(defun ess-sas-submit-sh (arg1 arg2)
  "Unix or bash in the *shell* buffer.
Multiple processing is supported on this platform.
SAS may not be found in your PATH.  You can alter your PATH to include
SAS or you can specify the PATHNAME (PATHNAME can NOT contain spaces),
i.e. let arg1 be your local equivalent of
\"/usr/local/sas612/sas\"."
    (if (string-equal (substring 
	    (file-name-nondirectory ess-sas-file-path) 0 1) ess-kermit-prefix)
      (progn
       (ess-kermit-send)
       (ess-sas-goto-shell t)
       (insert ess-sas-submit-pre-command " " arg1 " "  
	 (substring (file-name-sans-extension 
	    (file-name-nondirectory ess-sas-file-path)) 1)
	 " " arg2 " " ess-sas-submit-post-command))
    ;;else
      (ess-sas-goto-shell t)
;      (if ess-microsoft-p
;	  (insert "cd "  (file-name-directory ess-sas-file-path))
;	(insert "cd " (car (last (split-string 
;	    (file-name-directory ess-sas-file-path) "\\(:\\|]\\)")))))
      (insert "cd " (car (last (split-string (file-name-directory ess-sas-file-path) 
"\\([a-zA-Z][a-zA-Z]:\\|]\\)"))))
      (comint-send-input)
      (insert ess-sas-submit-pre-command " " arg1 " "  
	(file-name-sans-extension (file-name-nondirectory ess-sas-file-path)) 
	" " arg2 " " ess-sas-submit-post-command))
    (ess-sleep)
    (comint-send-input))

(defun ess-sas-submit-windows (arg1 arg2)
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
    ;(ess-save-and-set-local-variables)
    (ess-sas-goto-shell t)
    (if (string-equal ":" (substring ess-sas-file-path 1 2)) 
	(progn
		(insert (substring ess-sas-file-path 0 2))
		(comint-send-input)
	)
    )
    (insert "cd \"" (convert-standard-filename 
	(file-name-directory ess-sas-file-path)) "\"")
    (comint-send-input)
    (insert ess-sas-submit-pre-command " " arg1 " -sysin \"" 
	(file-name-sans-extension (file-name-nondirectory ess-sas-file-path)) "\" "
	arg2 " " ess-sas-submit-post-command)
    (comint-send-input))

(defun ess-sas-tab-to-tab-stop ()
  "Tab to next tab-stop and set left margin."
  (interactive)
  (tab-to-tab-stop)
  (setq left-margin (current-column))
)
  
(defun ess-sas-transcript (&optional strip)
"Comment .log messages to create a .sas program; use C-u to strip."
(interactive "P")
(save-excursion
    (goto-char (point-min))

    (while (search-forward-regexp (concat
           "^\\(\\(1[ \t]+The SAS System\\|\\|NOTE\\|WARNING\\|ERROR\\|"
           "[ \t]+\\(\\(real\\|cpu\\) time\\|Licensed to\\|Engine:\\|"
	   "Physical Name:\\|File Name=\\|Pipe command=\\|"
	   "RECFM=[DFNPV],LRECL=\\|[0-9]+:[0-9]+[ /t]+[0-9]+:[0-9]+\\|"
	   "[1-9][0-9]* at [0-9]+:[0-9]+[ /t]+[1-9][0-9]* at [0-9]+:[0-9]+\\)\\).*$"
           "\\|[0-9]+\\([ \t]+!\\)?\\|MPRINT([_A-Z]+):\\|"
	   "[ \t]+\\(values at the places given by: (Line):(Column).\\|"
           "The m\\(in\\|ax\\)imum record length was [1-9][0-9]*.\\|"
	   "One or more lines were truncated.\\|"
	   "Each place is given by: (Number of times) at (Line):(Column).\\|"
           "[0-9][0-9]:[0-9][0-9] [MTWFS][aeioudhnrst]+day, [JFMASOND]"
           "[aeiouybcghlmnprstv]+ [1-9][0-9]?, 20[0-9][0-9]\\)\\)") 
           nil t) (replace-match (if strip " " "/*\\&*/") t))
))

(defun ess-sas-toggle-sas-log-mode (&optional force)
  "Toggle SAS-log-mode for .log files."
  (interactive)

  (if (and (equal (cdr (assoc "\\.[lL][oO][gG]\\'" auto-mode-alist)) 'SAS-log-mode)
	   (not force))
      (setq auto-mode-alist (delete '("\\.[lL][oO][gG]\\'" . SAS-log-mode) auto-mode-alist))
    (setq auto-mode-alist (append '(("\\.[lL][oO][gG]\\'" . SAS-log-mode)) auto-mode-alist)))
  
  (if (equal (downcase (file-name-extension (buffer-file-name))) "log")
      (progn (font-lock-mode 0)
	     (normal-mode)
	     (if (not (equal (prin1-to-string major-mode) "ess-mode"))
		 (ess-transcript-minor-mode 0))
	     (font-lock-mode 1))))

(defun ess-sleep ()
"Put emacs to sleep for `ess-sleep-for' seconds.
Sometimes its necessary to wait for a shell prompt."
(if (featurep 'xemacs) (sleep-for ess-sleep-for)
       (sleep-for 0 (truncate (* ess-sleep-for 1000)))
    )
)

;;; Section 3:  Key Definitions

(defvar ess-sas-edit-keys-toggle nil
  "Toggle TAB key in `SAS-mode'.
nil to bind TAB to `sas-indent-line'.
Non-nil to bind TAB to `ess-sas-tab-to-tab-stop', 
C-TAB to `ess-sas-backward-delete-tab', and
RET to `newline'.")

(defun ess-sas-edit-keys-toggle (&optional arg)
  "Toggle TAB key in `SAS-mode'.
If arg is null, toggle `ess-sas-edit-keys-toggle'.
If arg is nil, TAB is `sas-indent-line',
RET is `newline-and-indent'.
If arg is non-nil, TAB is `ess-sas-tab-to-tab-stop', 
C-TAB is `ess-sas-backward-delete-tab' and
RET is `newline'.
Without args, toggle between these options."
  (interactive "P")
  (setq ess-sas-edit-keys-toggle
	(if (null arg) (not ess-sas-edit-keys-toggle) arg))
  (if ess-sas-edit-keys-toggle
      (progn
	(if (and (equal emacs-major-version 19) (equal emacs-minor-version 28))
	       (define-key sas-mode-local-map [C-tab] 'ess-sas-backward-delete-tab)
	;else
	       (define-key sas-mode-local-map [(control tab)] 'ess-sas-backward-delete-tab))
        (define-key sas-mode-local-map [return] 'newline)
	(define-key sas-mode-local-map "\t" 'ess-sas-tab-to-tab-stop))
  ;else
      (define-key sas-mode-local-map [return] 'newline-and-indent)
      (define-key sas-mode-local-map "\t" 'sas-indent-line)))

(defvar ess-sas-global-pc-keys nil
  "Non-nil if function keys use PC-like SAS key definitions in all modes.")

(defun ess-sas-global-pc-keys ()
  "PC-like SAS key definitions"
  (interactive)
  (global-set-key [(control f1)] 'ess-sas-rtf-portrait-1)
  (global-set-key [(control f2)] 'ess-sas-rtf-us-landscape-1)
  (global-set-key (quote [f2]) 'ess-revert-wisely)
  (global-set-key (quote [f3]) 'ess-sas-goto-shell)
  (global-set-key (quote [f4]) 'ess-sas-goto-file-1)
  (global-set-key (quote [f5]) 'ess-sas-goto-sas)
  (global-set-key (quote [f6]) 'ess-sas-goto-log)
  (global-set-key [(control f6)] 'ess-sas-append-log)
  (global-set-key (quote [f7]) 'ess-sas-goto-lst)
  (global-set-key [(control f7)] 'ess-sas-append-lst)
  (global-set-key (quote [f8]) 'ess-sas-submit)
  (global-set-key [(control f8)] 'ess-sas-submit-region)
  (global-set-key (quote [f9]) 'ess-sas-data-view)
  (global-set-key [(control f9)] 'ess-sas-kill-buffers)
  (global-set-key (quote [f10]) 'ess-sas-toggle-sas-log-mode)
  (global-set-key (quote [f11]) 'ess-sas-goto-file-2)
  (global-set-key (quote [f12]) 'ess-sas-graph-view)
  (if (and ess-sas-edit-keys-toggle
	   (equal emacs-major-version 19) (equal emacs-minor-version 28))
      (global-set-key [C-tab] 'ess-sas-backward-delete-tab)
					;else
    (global-set-key [(control tab)] 'ess-sas-backward-delete-tab))
  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path)
  (setq ess-sas-global-pc-keys t)
  (setq ess-sas-global-unix-keys nil)
  (setq ess-sas-local-pc-keys nil)
  (setq ess-sas-local-unix-keys nil)
)

(defvar ess-sas-global-unix-keys nil
  "Non-nil if function keys use Unix-like SAS key definitions in all modes.")

(defun ess-sas-global-unix-keys ()
  "Unix/Mainframe-like SAS key definitions"
  (interactive)
  (global-set-key [(control f1)] 'ess-sas-rtf-portrait-1)
  (global-set-key [(control f2)] 'ess-sas-rtf-us-landscape-1)
  (global-set-key (quote [f2]) 'ess-revert-wisely)
  (global-set-key (quote [f3]) 'ess-sas-submit)
  (global-set-key [(control f3)] 'ess-sas-submit-region)
  (global-set-key (quote [f4]) 'ess-sas-goto-sas)
  (global-set-key (quote [f5]) 'ess-sas-goto-log)
  (global-set-key [(control f5)] 'ess-sas-append-log)
  (global-set-key (quote [f6]) 'ess-sas-goto-lst)
  (global-set-key [(control f6)] 'ess-sas-append-lst)
  (global-set-key (quote [f7]) 'ess-sas-goto-file-1)
  (global-set-key (quote [f8]) 'ess-sas-goto-shell)
  (global-set-key (quote [f9]) 'ess-sas-data-view)
  (global-set-key [(control f9)] 'ess-sas-kill-buffers)
  (global-set-key (quote [f10]) 'ess-sas-toggle-sas-log-mode)
  (global-set-key (quote [f11]) 'ess-sas-goto-file-2)
  (global-set-key (quote [f12]) 'ess-sas-graph-view)
	(if (and ess-sas-edit-keys-toggle
	    (equal emacs-major-version 19) (equal emacs-minor-version 28))
	    (global-set-key [C-tab] 'ess-sas-backward-delete-tab)
	    ;else
	    (global-set-key [(control tab)] 'ess-sas-backward-delete-tab))
  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path) 
  (setq ess-sas-global-pc-keys nil)
  (setq ess-sas-global-unix-keys t)
  (setq ess-sas-local-pc-keys nil)
  (setq ess-sas-local-unix-keys nil)
)

(defvar ess-sas-local-pc-keys nil
  "Non-nil if function keys use PC-like SAS key definitions
in SAS-mode and related modes.")

(defun ess-sas-local-pc-keys ()
  "PC-like SAS key definitions."
  (interactive)
  (define-key sas-mode-local-map [(control f1)] 'ess-sas-rtf-portrait-1)
  (define-key sas-mode-local-map [(control f2)] 'ess-sas-rtf-us-landscape-1)
  (define-key sas-mode-local-map (quote [f2]) 'ess-revert-wisely)
  (define-key sas-mode-local-map (quote [f3]) 'ess-sas-goto-shell)
  (define-key sas-mode-local-map (quote [f4]) 'ess-sas-goto-file-1)
  (define-key sas-mode-local-map (quote [f5]) 'ess-sas-goto-sas)
  (define-key sas-mode-local-map (quote [f6]) 'ess-sas-goto-log)
  (define-key sas-mode-local-map [(control f6)] 'ess-sas-append-log)
  (define-key sas-mode-local-map (quote [f7]) 'ess-sas-goto-lst)
  (define-key sas-mode-local-map [(control f7)] 'ess-sas-append-lst)
  (define-key sas-mode-local-map (quote [f8]) 'ess-sas-submit)
  (define-key sas-mode-local-map [(control f8)] 'ess-sas-submit-region)
  (define-key sas-mode-local-map (quote [f9]) 'ess-sas-data-view)
  (define-key sas-mode-local-map [(control f9)] 'ess-sas-kill-buffers)
  (define-key sas-mode-local-map (quote [f10]) 'ess-sas-toggle-sas-log-mode)
  (define-key sas-mode-local-map (quote [f11]) 'ess-sas-goto-file-2)
  (define-key sas-mode-local-map (quote [f12]) 'ess-sas-graph-view)
  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path) 
  (setq ess-sas-global-pc-keys nil)
  (setq ess-sas-global-unix-keys nil)
  (setq ess-sas-local-pc-keys t)
  (setq ess-sas-local-unix-keys nil)
)

(defvar ess-sas-local-unix-keys nil
  "Non-nil if function keys use Unix-like SAS key definitions
in SAS-mode and related modes.")

(defun ess-sas-local-unix-keys ()
  "Unix/Mainframe-like SAS key definitions"
  (interactive)
  (define-key sas-mode-local-map [(control f1)] 'ess-sas-rtf-portrait-1)
  (define-key sas-mode-local-map [(control f2)] 'ess-sas-rtf-us-landscape-1)
  (define-key sas-mode-local-map (quote [f2]) 'ess-revert-wisely)
  (define-key sas-mode-local-map (quote [f3]) 'ess-sas-submit)
  (define-key sas-mode-local-map [(control f3)] 'ess-sas-submit-region)
  (define-key sas-mode-local-map (quote [f4]) 'ess-sas-goto-sas)
  (define-key sas-mode-local-map (quote [f5]) 'ess-sas-goto-log)
  (define-key sas-mode-local-map [(control f5)] 'ess-sas-append-log)
  (define-key sas-mode-local-map (quote [f6]) 'ess-sas-goto-lst)
  (define-key sas-mode-local-map [(control f6)] 'ess-sas-append-lst)
  (define-key sas-mode-local-map (quote [f7]) 'ess-sas-goto-file-1)
  (define-key sas-mode-local-map (quote [f8]) 'ess-sas-goto-shell)
  (define-key sas-mode-local-map (quote [f9]) 'ess-sas-data-view)
  (define-key sas-mode-local-map [(control f9)] 'ess-sas-kill-buffers)
  (define-key sas-mode-local-map (quote [f10]) 'ess-sas-toggle-sas-log-mode)
  (define-key sas-mode-local-map (quote [f11]) 'ess-sas-goto-file-2)
  (define-key sas-mode-local-map (quote [f12]) 'ess-sas-graph-view)
  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path) 
  (setq ess-sas-global-pc-keys nil)
  (setq ess-sas-global-unix-keys nil)
  (setq ess-sas-local-pc-keys nil)
  (setq ess-sas-local-unix-keys t)
)

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
