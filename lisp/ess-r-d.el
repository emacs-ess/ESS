;;; ess-r-d.el --- R customization

;; Copyright (C) 1997--2011 A.J. Rossini, Richard M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: A.J. Rossini
;; Created: 12 Jun 1997
;; Maintainers: ESS-core <ESS-core@r-project.org>

;; Keywords: start up, configuration.

;; This file is part of ESS.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; This file defines all the R customizations for ESS.  See ess-s-l.el
;;; for general S language customizations.

;;; Autoloads and Requires

(ess-message "[ess-r-d:] (require 'ess-s-l)")
(require 'ess-s-l)

(require 'ess-r-args); for now --- should the default rather become ess-eldoc?

;; modify S Syntax table:
(setq R-syntax-table S-syntax-table)

;; In R 2.x, back tick now is a quote character, so lets tell Emacs
;; that it is; the problem below for older R should no longer be a
;; serious issue.
;;R >= 1.8: back tick `string` -- unfortunately no *pair* checking:
;; breaks when things like `..' are used:
(modify-syntax-entry ?` "\"" R-syntax-table)
(modify-syntax-entry ?_  "_"  R-syntax-table) ; foo_bar is symbol in R >=1.9

(ess-message "[ess-r-d:] (autoload ..) & (def** ..)")

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

;;; Code:

(defvar R-customize-alist
  (append
   '((ess-local-customize-alist		. 'R-customize-alist)
     (ess-dialect			. "R")
     (ess-suffix			. "R")
     (ess-dump-filename-template	. (ess-replace-regexp-in-string
					   "S$" ess-suffix ; in the one from custom:
					   ess-dump-filename-template-proto))
     (ess-mode-syntax-table		. R-syntax-table)
     (ess-mode-editing-alist	        . R-editing-alist)
     (ess-change-sp-regexp		. ess-R-change-sp-regexp)
     (ess-help-sec-regex		. ess-help-R-sec-regex)
     (ess-help-sec-keys-alist		. ess-help-R-sec-keys-alist)
     (ess-loop-timeout			. ess-S-loop-timeout);fixme: dialect spec.
     (ess-cmd-delay			. ess-R-cmd-delay)
     (ess-function-pattern              . ess-R-function-pattern)
     (ess-object-name-db-file		. "ess-r-namedb.el" )
     (ess-imenu-mode-function		. 'ess-imenu-R)
     (inferior-ess-program		. inferior-R-program-name)
     (inferior-ess-objects-command	. inferior-R-objects-command)
     (inferior-ess-font-lock-keywords   . inferior-ess-R-font-lock-keywords)
     (inferior-ess-search-list-command	. "search()\n")
     ;;(inferior-ess-help-command		. "help(\"%s\", htmlhelp=FALSE)\n")
     (inferior-ess-help-command		. inferior-ess-r-help-command)
     (inferior-ess-help-filetype        . nil)
     (inferior-ess-exit-command		. "q()")
     (inferior-ess-exit-prompt		. "Save workspace image? [y/n/c]: ")
     (inferior-ess-primary-prompt	. "\\([A-Z][][A-Za-z0-9.]*\\)?> ")
     (inferior-ess-secondary-prompt	. "+ ?")
     ;;harmful for shell-mode's C-a: -- but "necessary" for ESS-help?
     (inferior-ess-start-file		. nil) ;; "~/.ess-R"
     (inferior-ess-start-args		. "")
     (ess-STERM		. "iESS")
     (ess-editor	. R-editor)
     (ess-pager		. R-pager)
     )
   S-common-cust-alist)
  "Variables to customize for R -- set up later than emacs initialization.")

(defvar ess-r-versions '("R-1" "R-2" "R-devel" "R-patched")
  "List of partial strings for versions of R to access within ESS.
Each string specifies the start of a filename.  If a filename
beginning with one of these strings is found on `exec-path', a M-x
command for that version of R is made available.  For example, if the
file \"R-1.8.1\" is found and this variable includes the string
\"R-1\", a function called `M-x R-1.8.1' will be available to run that
version of R.
If duplicate versions of the same program are found (which happens if
the same path is listed on `exec-path' more than once), they are
ignored by calling `ess-uniq-list'.
Set this variable to nil to disable searching for other versions of R.
If you set this variable, you need to restart Emacs (and set this variable
before ess-site is loaded) for it to take effect.")

;;;### autoload
(defun R (&optional start-args)
  "Call 'R', the 'GNU S' system from the R Foundation.
Optional prefix (C-u) allows to set command line arguments, such as
--vsize.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to R, put them in the variable `inferior-R-args'."
  (interactive "P")
  ;; get settings, notably inferior-R-program-name :
  (setq ess-customize-alist R-customize-alist)
  (ess-write-to-dribble-buffer   ;; for debugging only
   (format
    "\n(R): ess-dialect=%s, buf=%s, start-arg=%s\n current-prefix-arg=%s\n"
    ess-dialect (current-buffer) start-args current-prefix-arg))
  (let* ((r-always-arg
	  (if (or ess-microsoft-p (eq system-type 'cygwin))
	      "--ess "
	    "--no-readline "))
	 (r-start-args
	  (concat r-always-arg
		  inferior-R-args " " ; add space just in case
		  (if start-args
		      (read-string
		       (concat "Starting Args [other than `"
			       r-always-arg
			       "'] ? "))
		    nil)))
	 use-dialog-box)

    (when (or ess-microsoft-p
	      (eq system-type 'cygwin))
       (setq use-dialog-box nil)
       (if ess-microsoft-p ;; default-process-coding-system would break UTF locales on Unix
	   (setq default-process-coding-system '(undecided-dos . undecided-dos))))

    (inferior-ess r-start-args) ;; -> .. (ess-multi ...) -> .. (inferior-ess-mode) ..
    ;;-------------------------
    (ess-write-to-dribble-buffer
     (format "(R): inferior-ess-language-start=%s\n"
	     inferior-ess-language-start))
    ;; can test only now that R is running:
    (if (ess-current-R-at-least '2.5.0)
	(progn
	  (if ess-use-R-completion ;; use R's completion mechanism (pkg "rcompgen" or "utils")
	      (progn ; nothing to happen here -- is all in ess-complete-object-name
		(ess-write-to-dribble-buffer "resetting completion to 'ess-R-complete-object-name")
		))
	  ;; problem with ess-help-command
	  (let ((my-R-help-cmd
		 (if (ess-current-R-at-least '2.10.0)
		     "help"
		   ;; else R version <= 2.9.2
		   "function(..., help_type) help(..., htmlhelp= (help_type==\"html\"))")))

	    (ess-eval-linewise
	     (concat ".help.ESS <- " my-R-help-cmd) nil nil nil 'wait-prompt)
	  ))

      ;; else R version <= 2.4.1

      ;; for R <= 2.1.x : define baseenv() :
      (ess-eval-linewise
       "if(!exists(\"baseenv\", mode=\"function\")) baseenv <- function() NULL"
       nil nil nil 'wait-prompt);; solving "lines running together"
      )
    (if inferior-ess-language-start
	(ess-eval-linewise inferior-ess-language-start
			   nil nil nil 'wait-prompt))))


;;;### autoload
(defun R-mode  (&optional proc-name)
  "Major mode for editing R source.  See `ess-mode' for more help."
  (interactive "P")
  (setq ess-customize-alist R-customize-alist)
  ;;(setq imenu-generic-expression R-imenu-generic-expression)
  (ess-mode R-customize-alist proc-name)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  ;; ECB needs seminatic stuff.
  ;;  (if (featurep 'semantic)
  ;;      (setq semantic-toplevel-bovine-table r-toplevel-bovine-table))
  (if ess-imenu-use-S
      (progn (require 'ess-menu)
	     (ess-imenu-R)))
  ;; MM:      ^^^^^^^^^^^ should really use ess-imenu-mode-function from the
  ;;     alist above!
  (run-hooks 'R-mode-hook))

(fset 'r-mode 'R-mode)

(defun ess-R-arch-2-bit (arch)
  "Translate R's architecture shortcuts/directory names to 'bits',
 i.e., \"32\" or \"64\" (for now)."
  (if (string= arch "i386")  "32"
    ;; else:
    "64"))

(defun ess-rterm-arch-version (long-path &optional give-cons)
  "Find an architecture-specific name for LONG-PATH, an absolute (long name) path
 to R on Windows. Returns either Name, a string, or a (Name . Path) cons, such as
 (\"R-2.12.1-64bit\"  .  \"C:/Program Files/R/R-2.12.1/bin/x64/Rterm.exe\")

\"R-x.y.z/bin/Rterm.exe\" will return \"R-x.y.z\", for R-2.11.x and older.
\"R-x.y.z/bin/i386/Rterm.exe\" will return \"R-x.y.z-32bit\", for R-2.12.x and newer.
\"R-x.y.z/bin/x64/Rterm.exe\"  will return \"R-x.y.z-64bit\", for R-2.12.x and newer."
  (let* ((dir  (directory-file-name (file-name-directory long-path)))
	 (dir2 (directory-file-name (file-name-directory dir)))
	 (v-1up (file-name-nondirectory dir));; one level up
	 (v-2up (file-name-nondirectory dir2));; two levels up; don't want "bin" ...
	 (v-3up (file-name-nondirectory ;; three levels up; no "bin" for i386, x64 ...
		      (directory-file-name (file-name-directory dir2))))
	 (val (if (string= v-2up "bin")
		  (concat v-3up "-" (ess-R-arch-2-bit v-1up) "bit")
		;; pre R-2.12.x, or when there's no extra arch-specific sub directory:
		v-2up)))
    (if give-cons
	(cons val long-path)
      val)))


(defun ess-r-versions-create ()
  "Generate the `M-x R-x.y.z' functions for starting other versions of R.
On MS Windows, this works using `ess-rterm-version-paths'; otherwise,
see `ess-r-versions' for strings that determine which functions are created.

The result is a list of the new R defuns, if any, that were created.  The
defuns will normally be placed on the menubar and stored as
`ess-r-versions-created' upon ESS initialisation."

  (if (not ess-r-versions)
      nil				;nothing to return
    ;; else, if ess-r-versions is non-nil, let's try to find those R versions.
    ;; This works by creating a temp buffer where the template function is
    ;; edited so that X.Y is replaced by the version name
    (let (versions
	  r-versions-created
	  (eval-buf (get-buffer-create "*ess-temp-r-evals*"))
	  (template
	   ;; This is the template function used for creating M-x R-X.Y.
	   (concat
	    "(defun R-X.Y (&optional start-args)
  \"Call the R version 'R-X.Y' using ESS.
This function was generated by `ess-r-versions-create'.\"
  (interactive \"P\")
  (let ((inferior-R-version \"R-X.Y\")
        (inferior-R-program-name \""
	    (if ess-microsoft-p "Rterm" "R") "-X.Y\"))
    (R start-args)))
"
	    ) ))

      (save-excursion
	(set-buffer eval-buf)
	;; clear the buffer.
	(delete-region (point-min) (point-max))

	;; Find which versions of R we want.  Remove the pathname, leaving just
	;; the name of the executable.
	(setq versions
	      (if ess-microsoft-p
		  (mapcar '(lambda(v) (ess-rterm-arch-version v 'give-cons))
			  ess-rterm-version-paths)
		;;        ^^^^^^^^^^^^^^^^^^^^^^^ from ./ess-site.el at start
		;; else (non-MS):
		(ess-uniq-list
		 (mapcar 'file-name-nondirectory
			 (apply 'nconc
				(mapcar 'ess-find-exec-completions
					ess-r-versions))))))
	(setq r-versions-created ; also for returning at end.
	      (if ess-microsoft-p
		  (mapcar 'car versions)
		versions))
	(ess-write-to-dribble-buffer
	 (format "(R): ess-r-versions-create making M-x defuns for \n %s\n"
		 (mapconcat 'identity r-versions-created "\n ")))

	;; Iterate over each string in VERSIONS, creating a new defun each time.
	(while versions
	  (let* ((version (car versions))
		 (ver (if ess-microsoft-p (car version) version))
		 (beg (point)))

	    (setq versions (cdr versions))
	    (insert template)
	    (goto-char beg)
	    (while (search-forward "R-X.Y" nil t) ;; in all cases
	      (replace-match ver t t))
	    (when ess-microsoft-p
	      (goto-char beg)
	      (while (search-forward "Rterm-X.Y" nil t)
		(replace-match (w32-short-file-name (cdr version)) t t)))
	    (goto-char (point-max)))
	  )
	;; buffer has now been created with defuns, so eval them!
	(eval-buffer))
      (unless (and (boundp 'ess-debugging) ess-debugging)
	(kill-buffer eval-buf))

      r-versions-created)))

(defvar ess-newest-R nil
  "Stores the newest version of R that has been found.  Used as a cache,
within ess-find-newest-R.  Do not use this value directly, but
instead call the function \\[ess-find-newest-R].")

(defun ess-find-newest-R ()
  "Find the newest version of R on the system.  Once the value is found,
cache it in the variable `ess-newest-R' for future use as finding the
newest version of R can be potentially time-consuming."
  (or ess-newest-R
      (progn (message "Finding all versions of R on your system...")
	     ;;(sleep-for 3)
	     nil)
      (setq ess-newest-R
	    (ess-newest-r
	     (if ess-microsoft-p
		 ess-rterm-version-paths
	       (add-to-list 'ess-r-versions-created
			    inferior-R-program-name))))))

(defun ess-check-R-program-name ()
  "Check if `inferior-R-program-name' points to an executable version of R.
If not, try to find the newest version of R elsewhere on the system, and
update `inferior-R-program-name' accordingly."
  (unless (executable-find inferior-R-program-name)
    ;; need to check if we can find another name.
    (let ((newest (ess-find-newest-R)))
      (if newest
	  (setq inferior-R-program-name newest)
	(message "Sorry, no version of R could be found on your system.")))))

(defun R-newest (&optional start-args)
  "Find the newest version of R available, and run it.
Subsequent calls to R-newest will run that version, rather than searching
again for the newest version.  Providing an optional prefix arg (C-u) will
prompt for command line arguments."
  (interactive "P")
  (let ((rnewest (ess-find-newest-R)))
    (if (not rnewest)
	(error "No version of R could be found.")
      ;; Else: we have a working version of R.
      ;; Have to be careful to avoid recursion...
      (message (concat "Newest version of R is " rnewest))
      (fset 'R-newest
	    (intern
	     (if ess-microsoft-p
		 (ess-rterm-arch-version rnewest)
	       rnewest)))
      ;;(fset 'R-newest (intern rnewest))
      (R-newest start-args))))

;; (ess-r-version-date "R-2.5.1") (ess-r-version-date "R-patched")
;; (ess-r-version-date "R-1.2.1") (ess-r-version-date "R-1.8.1")
;; Windows:
;;  (ess-r-version-date "C:/Program Files (x86)/R/R-2.11.1/bin/Rterm.exe")
;; Note that for R-devel, ver-string is something like
;; R version 2.6.0 Under development (unstable) (2007-07-14 r42234)
;; Antique examples are 'R 1.0.1  (April 14, 2000)' or 'R 1.5.1 (2002-06-17).'
(defun ess-r-version-date (rver)
  "Return the date of the version of R named RVER.
The date is returned as a date string.  If the version of R could
not be found from the output of the RVER program, \"-1\" is
returned."
  (let ((date "-1")
	(ver-string (shell-command-to-string
		     ;; here, MS Windows (shell-command) needs a short name:
		     (concat (if ess-microsoft-p (w32-short-file-name rver) rver)
			     " --version"))))
    (when (string-match
	   "R \\(version \\)?[1-9][^\n]+ (\\(2[0-9-]+\\)\\( r[0-9]+\\)?)"
	   ver-string)
      (setq date (match-string 2 ver-string)))
    (cons date rver)))

(defun ess-current-R-version ()
  "Get the version of R currently running in the ESS buffer as a string"
  (ess-make-buffer-current)
  (car (ess-get-words-from-vector "as.character(getRversion())\n")))

(defun ess-current-R-at-least (version)
  "Is the version of R (in the ESS buffer) at least (\">=\") VERSION ?
Examples: (ess-current-R-at-least '2.7.0)
      or  (ess-current-R-at-least \"2.5.1\")"
  (ess-make-buffer-current)
  (string= "TRUE"
	   (car (ess-get-words-from-vector
		 (format "as.character(getRversion() >= \"%s\")\n" version)))))


(defun ess-newest-r (rvers)
  "Check all the versions of RVERS to see which is the newest.
Return the name of the newest version of R."
  (let ((rtimes (mapcar 'ess-r-version-date rvers)))
    ;; SJE: 2007-07-13 -- following line is a temp var to check that
    ;; the newest version of R is found correctly.
    ;; (nowadays gives a compile warning)
    (setq ess-temp-newest rtimes)
    (ess-find-newest-date rtimes)))

;; Test case for following defun:
;; (setq a '( ("2003-10-04" . "R-1.7")
;; 	   ("2006-11-19" . "R-2.2")
;; 	   ("2007-07-01" . "R-dev")
;; 	   ("-1" . "R-broken")
;; 	   ("2005-12-30" . "R-2.0")))
;; (ess-find-newest-date a)
(defun ess-find-newest-date (rvers)
  "Find the newest version of R given in the a-list RVERS.
Each element of RVERS is a dotted pair (date . R-version), where
date is given as e.g.\"2007-11-30\" so that we can compare dates
as strings.  If a date is listed as \"-1\", that version of R
could not be found.

If the value returned is nil, no valid newest version of R could be found."
  (let (new-r this-r
	(new-time "0"))
    (while rvers
      (setq this-r (car rvers)
	    rvers (cdr rvers))
      (when (string< new-time (car this-r))
	(setq new-time (car this-r)
	      new-r    (cdr this-r))))
    new-r))


(defun ess-find-rterm (&optional ess-R-root-dir bin-Rterm-exe)
  "Find the full path of all occurences of Rterm.exe under the ESS-R-ROOT-DIR.
If ESS-R-ROOT-DIR is nil, construct it by looking for an occurence of Rterm.exe
in the exec-path.  If there are no occurences of Rterm.exe in the exec-path,
then use `ess-program-files' (which evaluates to something like \"c:/progra~1/R/\"
in English locales) which is the default location for the R distribution.
If BIN-RTERM-EXE is nil, then use \"bin/Rterm.exe\"."
    (if (not ess-R-root-dir)
	(let ((Rpath (executable-find "Rterm")))
	  (setq ess-R-root-dir
		(expand-file-name
		 (if Rpath
		     (concat (file-name-directory Rpath) "../../")
		   (concat ess-program-files "/R/"))))
	  (ess-write-to-dribble-buffer
	   (format "(ess-find-rterm): ess-R-root-dir = '%s'\n" ess-R-root-dir))
	  ))

    (if (not bin-Rterm-exe) (setq bin-Rterm-exe "bin/Rterm.exe"))

    (when (file-directory-p ess-R-root-dir) ; otherwise file-name-all-.. errors
      (setq ess-R-root-dir
	    (ess-replace-regexp-in-string "[\\]" "/" ess-R-root-dir))
      (let ((R-ver
	     (ess-drop-non-directories
	      (ess-flatten-list
	       (mapcar '(lambda (r-prefix)
			  (file-name-all-completions r-prefix ess-R-root-dir))
		       (append '("rw") ess-r-versions))))))
	(mapcar '(lambda (dir)
		   (let ((R-path
			  (concat ess-R-root-dir
				  (ess-replace-regexp-in-string "[\\]" "/" dir)
				  bin-Rterm-exe)))
		     (if (file-exists-p R-path) R-path)))
		R-ver))))

;; From Jim (James W.) MacDonald, based on code by Deepayan Sarkar,
;; originally named  'alt-ess-complete-object-name'.
;; Use rcompgen in ESS
;; Can be activated by something like
;; (define-key inferior-ess-mode-map "\t" 'ess-R-complete-object-name)
(defun ess-R-complete-object-name ()
  "Completion in R via R's completion utilities (formerly 'rcompgen').
To be used instead of ESS' completion engine for R versions >= 2.5.0
 (or slightly older versions of R with an attached and working 'rcompgen' package)."
  (interactive)
  (ess-make-buffer-current)
  (let* ((comint-completion-addsuffix nil)
	 (beg-of-line (save-excursion (comint-bol nil) (point)))
	 (end-of-line (point-at-eol))
	 (line-buffer (buffer-substring beg-of-line end-of-line))
	 (NS (if (ess-current-R-at-least '2.7.0)
		 "utils:::"
	       "rcompgen:::"))
	 (token-string ;; setup, including computation of the token
	  (progn
	    (ess-command
	     (format (concat NS ".assignLinebuffer('%s')\n") line-buffer))
	    (ess-command (format (concat NS ".assignEnd(%d)\n")
				 (- (point) beg-of-line)))
	    (car (ess-get-words-from-vector
		  (concat NS ".guessTokenFromLine()\n")))))

	 (possible-completions ;; compute and retrieve possible completions
	  (progn
	    (ess-command (concat NS ".completeToken()\n"))
	    (ess-get-words-from-vector
	     (concat NS ".retrieveCompletions()\n")))))

    ;; If there are no possible-completions, should return nil, so
    ;; that when this function is called from
    ;; comint-dynamic-complete-functions, other functions can then be
    ;; tried.
    (if (null possible-completions)
	nil
      (or (comint-dynamic-simple-complete token-string
					  possible-completions)
	  'none))))


;;;### autoload
(defun Rnw-mode ()
  "Major mode for editing Sweave(R) source.
See `noweb-mode' and `R-mode' for more help."
  (interactive)
  (require 'ess-noweb);; << probably someplace else
  (noweb-mode 1); turn it on
  (noweb-set-doc-mode 'latex-mode)
  (noweb-set-code-mode 'R-mode)
  (run-hooks 'Rnw-mode-hook))

(fset 'Snw-mode 'Rnw-mode); just a synonym (for now or ever)


(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files." t)

(defun R-transcript-mode ()
  "Does the right thing."
  (interactive)
  (ess-transcript-mode R-customize-alist))

(fset 'r-transcript-mode 'R-transcript-mode)

(defun R-fix-T-F (&optional from quietly)
  "Fix T/F into TRUE and FALSE *cautiously*, i.e. not in comments and strings;
 starting from the current position (point)."
  (interactive "d\nP"); point and prefix (C-u)
  (save-excursion
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\) *\\)T\\>" "\\1TRUE"
		    'fixcase nil (not quietly))
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\) *\\)F\\>" "\\1FALSE"
		    'fixcase nil (not quietly))))

;; From: Sebastian Luque <spluque@gmail.com>
;; To: ess-help@stat.math.ethz.ch
;; Date: Mon, 01 May 2006 19:17:49 -0500

;; Without knowing how to tell R to use w3m from within Emacs, and after
;; switching to Konqueror's window for the millionth time, I wrote the
;; following function:

;; This emulates some of the functionality of RSiteSearch() and tests ok in
;; my system GNU Emacs 22.0.50.1 (i486-pc-linux-gnu, X toolkit, Xaw3d scroll
;; bars) of 2006-04-27 on pacem, modified by Debian.  This has the benefit of
;; displaying results with whatever you've told browse-url to use; in my
;; case, w3m with the emacs-w3m package.

;; My elisp skills are rather poor, so comments and suggestions for
;; improvement are welcome.
;; --
;; Seb


;; MM _FIXME_: This only works correctly for  Emacs 22.0.50 (alpha)
;;             for 21.x it has problems in the (completing-read-multiple .)
;;             at the end
(defun R-site-search (string)
  "Search the R archives for STRING, using default criteria.  If
called with a prefix, options are available for
  1) matches per page,
  2) sections of the archives to search (separated by value of `crm-default-separator'),
  3) for displaying results in long or short formats, and
  4) for sorting by any given field.
Completion is available for supplying options."
  (interactive "sSearch string: ")
  (let ((site "http://search.r-project.org/cgi-bin/namazu.cgi?query=")
	(okstring (replace-regexp-in-string " +" "+" string)))
    (if current-prefix-arg
	(let ((mpp (concat
		    "&max="
		    (completing-read
		     "Matches per page: "
		     '(("20" 1) ("30" 2) ("40" 3) ("50" 4) ("100" 5)))))
	      (format (concat
		       "&result="
		       (completing-read
			"Format: " '("normal" "short")
			nil t "normal" nil "normal")))
	      (sortby (concat
		       "&sort="
		       (completing-read
			"Sort by: "
			'(("score" 1) ("date:late" 2) ("date:early" 3)
			  ("field:subject:ascending" 4)
			  ("field:subject:decending" 5)
			  ("field:from:ascending" 6) ("field:from:decending" 7)
			  ("field:size:ascending" 8) ("field:size:decending" 9))
			nil t "score" nil "score")))
	      (restrict (concat
			 "&idxname="
			 (mapconcat
			  'identity
			  (completing-read-multiple
			   "Limit search to: "
			   '(("Rhelp02a" 1) ("functions" 2) ("docs" 3)
			     ("Rhelp01" 4))
			   nil t "Rhelp02a,functions,docs" nil
			   "Rhelp02a,functions,docs") "&idxname="))))
	  (browse-url (concat site okstring mpp format sortby restrict)))
      ;; else: without prefix use defaults:
      (browse-url (concat site okstring "&max=20&result=normal&sort=score"
			  "&idxname=Rhelp02a&idxname=functions&idxname=docs")))))


(defun ess-dirs ()
  "Set Emacs' current directory to be the same as the *R* process.
If you change directory within *R* using setwd(), run this command so that
Emacs can update its `default-directory' variable for the *R* buffer.

Currently this function has been tested only for *R*, but should also work for
*S* buffers."
  (interactive)
  (let ((dir (car (ess-get-words-from-vector "getwd()\n"))))
    (message "new (ESS / default) directory: %s" dir)
    (setq ess-directory (file-name-as-directory dir)))
    (setq default-directory ess-directory))


 ; provides

(provide 'ess-r-d)

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

;;; ess-r-d.el ends here
