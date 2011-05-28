;;; ess-inf.el --- Support for running S as an inferior Emacs process

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997-1999 A.J. Rossini <rossini@u.washington.edu>,
;;	Martin Maechler <maechler@stat.math.ethz.ch>.
;; Copyright (C) 2000--2006 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Created: 7 Jan 1994
;; Maintainers: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Code for handling running ESS processes.

;;; Code:

 ; Requires and autoloads

;;*;; Requires
(require 'ess-site)

;; Byte-compiler, SHUT-UP!
(eval-and-compile
  (require 'comint)
  (require 'ess-utils))

;;*;; Autoloads
(autoload 'ess-parse-errors		    "ess-mode"	"(autoload).")
(autoload 'ess-dump-object-into-edit-buffer "ess-mode"	"(autoload).")
(autoload 'ess-beginning-of-function	    "ess-mode"	"(autoload).")
(autoload 'ess-end-of-function		    "ess-mode"	"(autoload).")
(autoload 'ess-display-help-on-object	    "ess-help"	"(autoload).")

(autoload 'ess-extract-word-name	    "ess-utils" "(autoload).")
(autoload 'ess-uniq-list		    "ess-utils" "(autoload).")

(autoload 'ess-transcript-send-command-and-move "ess-trns" "(autoload).")

(autoload 'ess-R-complete-object-name	    "ess-r-d"	"(autoload).")

(autoload 'ess-eval-region-ddeclient	    "ess-dde"	"(autoload).")
(autoload 'ess-eval-linewise-ddeclient	    "ess-dde"	"(autoload).")
(autoload 'ess-load-file-ddeclient	    "ess-dde"	"(autoload).")
(autoload 'ess-command-ddeclient	    "ess-dde"	"(autoload).")

 ;;*;; Process handling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In this section:
;;;
;;; * User commands for starting an ESS process
;;; * Functions called at startup
;;; * Process handling code
;;; * Multiple process implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*;; Starting a process

(defun ess-proc-name (n name)
  "Return name of process N, as a string, with NAME prepended.
If ess-plain-first-buffername, then initial process is number-free."
  (concat name
	  (if (not (and ess-plain-first-buffername
			(= n 1))) ; if not both first and plain-first add number
	      (concat ":" (number-to-string n)))))

(defun inferior-ess (&optional ess-start-args)
  "Start inferior ESS process.

Without a prefix argument, starts a new ESS process, or switches
  to the ESS process associated with the current buffer.
With a prefix, starts the process with those args.
The current buffer is used if it is an `inferior-ess-mode'
or `ess-transcript-mode' buffer.

If `ess-ask-about-transfile' is non-nil, you will be asked for a
transcript file to use. If there is no transcript file, the buffer
name will be like *S* or *S2*.

Takes the program name from the variable `inferior-ess-program'.
An initialization file (dumped into the process) is specified by
`inferior-ess-start-file', and `inferior-ess-start-args' is used to
accompany the call for `inferior-ess-program'.

When creating a new process, the process buffer replaces the
current window if `inferior-ess-same-window' is non-nil.
Alternatively, it can appear in its own frame if
`inferior-ess-own-frame' is non-nil.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  ;; Use the current buffer if it is in inferior-ess-mode or ess-trans-mode
  ;; If not, maybe ask about starting directory and/or transcript file.
  ;; If no transfile, use buffer *S*
  ;;
  ;; This function is primarily used to figure out the Process and
  ;; buffer names to use for inferior-ess.

  ;; Once, long ago, it was used for switching buffers, but we don't
  ;; do that any more (at least not from here).

  (interactive)

  (ess-write-to-dribble-buffer
   (format "(inferior-ess 0): ess-start-args=%s \n" ess-start-args))

  ;; set up for current language (need here, to get ess-language, etc).

  ;; Couldn't we rather set all the default values or Local values now ?
  ;;>>> (ess-setq-vars-default ess-customize-alist (current-buffer))
  ;;>>> (ess-setq-vars-local   ess-customize-alist (current-buffer))
  ;;; AJR sez: I think we should set them later; don't want to nuke if
  ;;; I don't have to.
  ;;- MM: We shouldn't have to use  ess-setq-vars-default _at all_ ;
  ;;	  only do the buffer local ...-vars-local ones
  (let ((temp-ess-dialect (eval (cdr (assoc 'ess-dialect
					    ess-customize-alist))))
	(temp-ess-lang (eval (cdr (assoc 'ess-language
					 ess-customize-alist)))))
    (save-excursion
      ;;- Is this needed? (no, but it's useful to see them there [MM])
      (set-buffer ess-dribble-buffer)
      ;; Hack to work around the following "default" (global) setting of vars:
      ;; make sure our comint-... hack doesn't affect anything else
      ;;(make-variable-buffer-local 'comint-use-prompt-regexp-instead-of-fields)
      (make-local-variable 'comint-use-prompt-regexp-instead-of-fields)
      ;; now the abomination:
      (ess-setq-vars-default ess-customize-alist)

      ;; Emacs 22.0.50: ".. obsolete since 22.1;
      ;;		use `comint-use-prompt-regexp' instead
      (setq-default comint-use-prompt-regexp-instead-of-fields nil) ; re set HACK!
      ;;>> Doesn't set ess-language,
      ;;>> => comint-input-sender is not set to 'ess-input-  ==> no input echo!
      ;;>> => that's why things fail:
      ;;>> (ess-setq-vars-local ess-customize-alist (current-buffer))
      ;;		 ======
      )

    ;; run hooks now, to overwrite the above!
    (run-hooks 'ess-pre-run-hook)
    (ess-write-to-dribble-buffer
     (format "(inf-ess 1): lang=%s, dialect=%s, tmp-dialect=%s, buf=%s\n"
	     ess-language ess-dialect temp-ess-dialect (current-buffer)))
    (let* ((process-environment process-environment)
	   (defdir (or (and ess-directory-function (funcall ess-directory-function))
		       ess-directory default-directory))
	   (temp-dialect (if ess-use-inferior-program-name-in-buffer-name
			     (if (string-equal temp-ess-dialect "R")
				 inferior-R-program-name
			       temp-ess-dialect) ; use temp-ess-dialect
					; if not R, R program name
					; otherwise.
			   temp-ess-dialect))
	   (temp-lang temp-ess-lang)
	   (procname (let ((ntry 0) ;; find a non-existent process
			   (done nil))
		       (while (not done)
			 (setq ntry (1+ ntry)
			       done (not
				     (get-process (ess-proc-name
						   ntry
						   temp-dialect)))))
		       (ess-proc-name ntry temp-dialect)))
	   (startdir nil)
	   (buf nil)
	   (buf-name-str  (concat "*" procname "*")))

      (ess-write-to-dribble-buffer
       (format "(inf-ess 1.1): procname=%s temp-dialect=%s, buf-name=%s \n"
	       procname
	       temp-dialect
	       buf-name-str))
      (cond
       ;; Since it's a new or terminated process, try to use current buffer
       ((and (not buf)
	     (not (comint-check-proc (current-buffer)))
	     (memq major-mode '(inferior-ess-mode)))
	(setq startdir
	      (if ess-ask-for-ess-directory
		  (ess-get-directory (directory-file-name defdir))
		defdir))
	(setq buf (current-buffer))
	(ess-write-to-dribble-buffer
	 (format "(inferior-ess) Method #1 start=%s buf=%s\n" startdir buf)))

       ;;  Not an ESS buffer yet
       ((and (not buf)
	     (get-buffer buf-name-str))
	(setq buf (get-buffer buf-name-str))
	(ess-write-to-dribble-buffer
	 (format "(inferior-ess) Method #2 buf=%s\n" buf)))

       ;; Ask for transcript file and startdir
       ;; FIXME -- this should be in ess-get-transfile
       ;; AJR: Why?  I'm not clear about the logic, i.e. when would it
       ;;      be used again, to justify?
       ((not buf)
	(setq startdir
	      (if ess-ask-for-ess-directory
		  (ess-get-directory (directory-file-name defdir))
		defdir))
	(if ess-ask-about-transfile
	    (let ((transfilename (read-file-name
				  "Use transcript file (default none):"
				  startdir "")))
	      (setq buf (if (string= transfilename "")
			    (get-buffer-create buf-name-str)
			  (find-file-noselect (expand-file-name
					       transfilename)))))
	  (setq buf (get-buffer-create buf-name-str)))
	(ess-write-to-dribble-buffer
	 (format "(inferior-ess) Method #3 start=%s buf=%s\n" startdir buf))))

      (set-buffer buf)
      ;; Now that we have the buffer, set buffer-local variables.
      (ess-setq-vars-local ess-customize-alist)	; buf)
      (if ess-start-args (setq inferior-ess-start-args ess-start-args))
      ;; Was:  if not, set to null.
      ;;(setq inferior-ess-start-args "")) ;; AJR: Errors with XLS?
      ;; I think I might have solved this?

      ;; Write out debug info
      (ess-write-to-dribble-buffer
       (format "(inf-ess 2.1): ess-language=%s, ess-dialect=%s buf=%s \n"
	       ess-language
	       ess-dialect
	       (current-buffer)))
      (ess-write-to-dribble-buffer
       (format "(inf-ess 2.2): start args = %s, inf-ess-start-args=%s \n"
	       ess-start-args
	       inferior-ess-start-args))
      (ess-write-to-dribble-buffer
       (format "(inf-ess finish [%s(%s), %s(%s,%s)]\n"
	       ess-language
	       ess-dialect
	       inferior-ess-program
	       ess-current-process-name
	       ess-local-process-name))

      ;; Start from the "right" directory
      (if startdir (setq default-directory startdir))
      ;; Set up history
      (setq-default ess-history-file
		    (concat "." ess-dialect "history"))
      ;; initialize.
      (let ((ess-directory (if startdir default-directory ess-directory)))
	(ess-multi procname buf inferior-ess-start-args)))))


(defvar inferior-ess-objects-command nil
  "The language/dialect specific command for listing objects.
It is initialized from the corresponding inferior-<lang>-objects-command
and then made buffer local."); and the *-<lang>-* ones are customized!
(make-variable-buffer-local 'inferior-ess-objects-command)

(defvar ess-save-lastvalue-command nil
  "The command to save the last value.	See S section for more details.
Default depends on the ESS language/dialect and hence made buffer local")
(make-variable-buffer-local 'ess-save-lastvalue-command)

(defvar ess-retr-lastvalue-command nil
  "The command to retrieve the last value.  See S section for more details.
Default depends on the ESS language/dialect and hence made buffer local")
(make-variable-buffer-local 'ess-retr-lastvalue-command)

;;; A note on multiple processes: the following variables
;;;	ess-local-process-name
;;;	ess-search-list
;;;	ess-sl-modtime-alist
;;;	ess-sp-change
;;;	ess-prev-load-dir/file
;;;	ess-directory
;;;	ess-object-list
;;; are specific to each ess-process and are buffer-local variables
;;; local to the ESS process buffer. If required, these variables should
;;; be accessed with the function ess-get-process-variable

(defun ess-multi (name &optional buffer inf-ess-start-args)
  "Start or switch to ESS process named NAME in the buffer BUFFER.
BUFFER is only needed if process NAME is not running. BUFFER must
exist.	Default-directory is the ESS starting directory. BUFFER may be
visiting a file.

If ess-process NAME is running, switch to it.  If not, use COMINT to
start up a new process, using NAME and BUFFER (which is needed if
there is no process NAME)."

  (let* ((proc-name name)
	 (special-display-regexps nil)
	 (special-display-frame-alist inferior-ess-frame-alist)
	 (proc (get-process proc-name)))
    (if inferior-ess-own-frame
	(setq special-display-regexps '(".")))
    ;; If ESS process NAME is running, switch to it
    (if (and proc (comint-check-proc (process-buffer proc)))
	(pop-to-buffer (process-buffer proc))
      ;; Otherwise, crank up a new process
      (let* ((symbol-string
	      (concat "inferior-" inferior-ess-program "-args"))
	     (switches-symbol (intern-soft symbol-string))
	     (switches
	      (if (and switches-symbol (boundp switches-symbol))
		  (symbol-value switches-symbol)))
	     (buf-name-str (buffer-name buffer)))
	(ess-write-to-dribble-buffer
	 (format "(ess-multi 0):  inf-ess-start-args=%s, comint-..echoes=%s\n"
	       inf-ess-start-args comint-process-echoes))
	(set-buffer buffer)
	(inferior-ess-mode)
	(ess-write-to-dribble-buffer
	 (format "(ess-multi post inf-ess: start-args=%s, comint-echoes=%s\n"
	       inf-ess-start-args comint-process-echoes))
	(setq ess-local-process-name proc-name)
	(goto-char (point-max))
	;; load past history
	(setq comint-input-ring-file-name
	      (expand-file-name ess-history-file ess-directory))
	(comint-read-input-ring)
	;; create and run process.
	(ess-write-to-dribble-buffer
	 (format "(ess-multi 1):  start-args=%s \n"
	       inf-ess-start-args))
	(set-buffer
	 (if switches
	     (inferior-ess-make-comint buf-name-str
				       proc-name
				       inf-ess-start-args
				       switches)
	   (inferior-ess-make-comint buf-name-str
				     proc-name
				     inf-ess-start-args)))
	;; Set the process sentinel to save the history
	(set-process-sentinel (get-process proc-name) 'ess-process-sentinel)
	;; Add this process to ess-process-name-list, if needed
	(let ((conselt (assoc proc-name ess-process-name-list)))
	  (if conselt nil
	    (setq ess-process-name-list
		  (cons (cons proc-name nil) ess-process-name-list))))
	(ess-make-buffer-current)
	(inferior-ess-wait-for-prompt)
	(goto-char (point-max))
	(setq ess-sl-modtime-alist nil)
	;; Get search list when needed
	(setq ess-sp-change t)

	;; Add the process filter to catch certain output.
	(set-process-filter (get-process proc-name)
			    'inferior-ess-output-filter)

	(run-hooks 'ess-post-run-hook))
      (if (and inferior-ess-same-window (not inferior-ess-own-frame))
	  (switch-to-buffer (process-buffer (get-process proc-name)))
	(pop-to-buffer (process-buffer (get-process proc-name)))))))

(defun inferior-ess-output-filter (proc string)
  "Standard output filter for the inferior ESS process.
Ring Emacs bell if process output starts with an ASCII bell, and pass
the rest to `comint-output-filter'.
Taken from octave-mod.el."
  (comint-output-filter proc (inferior-ess-strip-ctrl-g string)))

(defun inferior-ess-strip-ctrl-g (string)
  "Strip leading `^G' character.
If STRING starts with a `^G', ring the Emacs bell and strip it.
Depending on the value of `visible-bell', either the frame will
flash or you'll hear a beep.  Taken from octave-mod.el."
  (if (string-match "^\a" string)
      (progn
	(ding)
	(setq string (substring string 1))))
  string)


(defun ess-process-sentinel (proc message)
  "Sentinel for use with ESS processes.
This marks the process with a message, at a particular time point."
  (save-excursion
    (setq message (substring message 0 -1)) ; strip newline
    (set-buffer (process-buffer proc))
    (comint-write-input-ring)
    (goto-char (point-max))
    (insert-before-markers
     (format "\nProcess %s %s at %s\n"
	     (process-name proc) message (current-time-string)))))

(defun inferior-ess-make-comint (bufname
				 procname
				 inferior-ess-start-args
				 &rest switches)
  "Make an S comint process in buffer BUFNAME with process PROCNAME.
This was rewritten by KH in April 1996."
;;; This function is a modification of make-comint from the comint.el
;;; code of Olin Shivers.
  (let*	 ((buffer (get-buffer-create bufname))
	  (proc (get-process procname)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode. Otherwise, leave buffer and existing process alone.
    (cond ((or (not proc) (not (memq (process-status proc) '(run stop))))
	   (save-excursion
	     (set-buffer buffer)
	     (if ess-directory (setq default-directory ess-directory))
	     (if (eq (buffer-size) 0) nil
	       (goto-char (point-max))
	       (insert "\^L\n")))    ; page boundaries = Interactive sessions
	   (let ((process-environment
		  (nconc
		   (list "STATATERM=emacs"
			 (format "PAGER=%s" inferior-ess-pager))
		   process-environment)))
	     (ess-write-to-dribble-buffer "Making Process...")
	     (ess-write-to-dribble-buffer
	      (format "Buf %s, Proc %s, Prog %s\n Start File=%s, Args= %s.\n"
		      buffer
		      procname
		      inferior-ess-program
		      inferior-ess-start-file
		      inferior-ess-start-args))
	     (comint-exec buffer
			  procname
			  inferior-ess-program
			  inferior-ess-start-file
			  (ess-line-to-list-of-words
			   inferior-ess-start-args)))))
    buffer))


;;*;; Requester functions called at startup

(defun ess-get-directory (default)
  (let ((prog-version (if (string= ess-dialect "R")
			  inferior-R-version ; notably for the R-X.Y versions
			inferior-ess-program)))
  (ess-prompt-for-directory
	default
	(format "ESS [%s(%s): %s] starting data directory? "
		ess-language ess-dialect prog-version))))

(defun ess-prompt-for-directory (default prompt)
  "`prompt' for a directory, using `default' as the usual."
  (let* ((def-dir (file-name-as-directory default))
	 (the-dir (expand-file-name
		   (file-name-as-directory
		    (if (fboundp 'read-directory-name)
			;; use XEmacs' read-directory-name if exists.
			(read-directory-name prompt def-dir def-dir t nil)
		      (read-file-name prompt def-dir def-dir t nil))))))
    (if (file-directory-p the-dir) nil
      (error "%s is not a valid directory" the-dir))
    the-dir))


;;*;; General process handling code

(defun get-ess-process (name &optional try-another)
  "Return the ESS process named by NAME.  If TRY-ANOTHER is non-nil,
and the process NAME is not running (anymore), try to connect to another if
there is one."
  (if (null name)	    ; should almost never happen at this point
      (error "No ESS process is associated with this buffer now"))
  (update-ess-process-name-list)
  (if (assoc name ess-process-name-list)
      (get-process name)
    ;; else :
    ;; was (error "Process %s is not running" name)
    (ess-write-to-dribble-buffer
     (format "get-ess-process: process '%s' not running" name))
    (if (= 0 (length ess-process-name-list))
	(save-current-buffer
	  (ess-write-to-dribble-buffer
	   (format " .. restart proc %s for language %s (buf %s)\n"
		   name ess-language (current-buffer)))
	  (message "trying to (re)start process %s for language %s ..."
		   name ess-language)
	  (ess-start-process-specific ess-language ess-dialect)
	  ;; and return the process: "call me again"
	  (get-ess-process name))

      ;; else: there are other running processes
      (if try-another ; connect to another running process : the first one
	  (let ((other-name (car (elt ess-process-name-list 0))))
	    ;; "FIXME": try to find the process name that matches *closest*
	    (message "associating with *other* process '%s'" other-name)
	    (get-ess-process other-name))
	;; else
	(ding)
	(if (yes-or-no-p
	     (format "Process %s is not running, but others are. Switch? " name))
	    (progn
	      (ess-force-buffer-current
	       (concat ess-dialect " process to use: ") t)
	      (get-ess-process ess-current-process-name))
	  (error "Process %s is not running" name))))))


(defun inferior-ess-wait-for-prompt ()
  "Wait until the ESS process is ready for input."
  (let* ((cbuffer (current-buffer))
	 (sprocess (get-ess-process ess-current-process-name))
	 (sbuffer (process-buffer sprocess))
	 (r nil)
	 (timeout 0))
    (set-buffer sbuffer)
    (while (progn
	     (if (not (eq (process-status sprocess) 'run))
		 (ess-error "ESS process has died unexpectedly.")
	       (if (> (setq timeout (1+ timeout)) ess-loop-timeout)
		   (ess-error "Timeout waiting for prompt. Check inferior-ess-prompt or ess-loop-timeout."))
	       (accept-process-output)
	       (goto-char (point-max))
	       (beginning-of-line); bol ==> no need for "^" in *-prompt! (MM?)
	       ;; above, except for Stata, which has "broken" i/o,
	       ;; sigh... (AJR)
	       (setq r (looking-at inferior-ess-prompt))
	       (not (or r (looking-at ".*\\?\\s *"))))))
    (goto-char (point-max))
    (set-buffer cbuffer)
    (symbol-value r)))

;;--- Unfinished idea (ESS-help / R-help ) -- probably not worth it...
;;- (defun ess-set-inferior-program-name (filename)
;;-   "Allows to set or change `inferior-ess-program', the program (file)name."
;;-   (interactive "fR executable (script) file: ")
;;-   ;; "f" : existing file {file name completion} !
;;-   (setq inferior-ess-program filename))
;; the inferior-ess-program is initialized in the customize..alist,
;; e.g. from  inferior-R-program-name ... --> should change rather these.
;; However these really depend on the current ess-language!
;; Plan: 1) must know and use ess-language
;;	 2) change the appropriate  inferior-<ESSlang>-program-name
;; (how?) in R/S : assign(paste("inferior-",ESSlang,"-p...."),	filename))

;;*;; Multiple process handling code

(defun ess-make-buffer-current nil
  "Make the process associated with the current buffer the current ESS process.
Returns the name of the process, or nil if the current buffer has none."
  (update-ess-process-name-list)
  (if ess-local-process-name
      (setq ess-current-process-name ess-local-process-name))
  ess-local-process-name)

(defun ess-get-process-variable (name var)
  "Return the variable VAR (symbol) local to ESS process called NAME (string)."
  (save-excursion
    (set-buffer (process-buffer (get-ess-process name)))
    (symbol-value var)))

(defun ess-set-process-variable (name var val)
  "Set variable VAR (symbol) local to ESS process called NAME (string) to VAL."
  (save-excursion
    (set-buffer (process-buffer (get-ess-process name)))
    (set var val)))

(defun ess-start-process-specific (language dialect)
  "Start an ESS process typically from a language-specific buffer, using
LANGUAGE (and DIALECT)."
  (let ((cur-buf (current-buffer)))
    (ess-write-to-dribble-buffer
     (format " ..start-process-specific: lang:dialect= %s:%s, current-buf=%s\n"
	     language dialect cur-buf))
    (cond ((string= language "S")
	   (if (string= dialect "R")
	       (R)
	     ;; else S, but not R
	     (message
	      "ESS process not running, trying to start R, since language = 'S")
	     (R))
	   ;; (save-excursion <the above>) fails, but this "works":
	   (switch-to-buffer cur-buf)
	   )
	  (t
	   ;; else: ess-language is not S
	   ;; FIXME find a better solution than this, at least in some cases:
	   (error "No ESS processes running; not yet implemented to start (%s,%s)"
		  language dialect)))))

(defun ess-request-a-process (message &optional noswitch ask-if-1)
  "Ask for a process, and make it the current ESS process.
If there is exactly one process, only ask if ASK-IF-1 is non-nil.
Also switches to the process buffer unless NOSWITCH is non-nil.	 Interactively,
NOSWITCH can be set by giving a prefix argument.
Returns the name of the selected process."
  (interactive
   (list "Switch to which ESS process? " current-prefix-arg))
					; prefix sets 'noswitch
  (ess-write-to-dribble-buffer "ess-request-a-process: {beginning}\n")
  (update-ess-process-name-list)
  (let ((num-processes (length ess-process-name-list)))
    (if (= 0 num-processes)
	;; try to start "the appropriate" process
	(progn
	  (ess-write-to-dribble-buffer
	   (concat " ... request-a-process:\n  "
		   (format
		    "major mode is %s; ess-language: %s, ess-dialect: %s\n"
		    major-mode ; 'ess-mode; how can we guess R?
		    ess-language ess-dialect)))
	  (ess-start-process-specific ess-language ess-dialect)
	  (ess-write-to-dribble-buffer
	   (format "  ... request-a-process: buf=%s\n" (current-buffer)))
	  (setq num-processes 1)))
    ;; now num-processes >= 1 :
    (let ((proc
	   (if (and (not ask-if-1) (= 1 num-processes))
	       (let ((rr (car (car ess-process-name-list))))
		 (message "using process '%s'" rr)
		 rr)
	     ;; else
	     (completing-read message
			      ess-process-name-list
			      nil	; predicate
			      'require-match
			      ;; If in S buffer, don't offer current process
			      (if (eq major-mode 'inferior-ess-mode)
				  ess-dialect
				ess-current-process-name
				;; maybe ess-local-process-name IF exists?
				)))))
      (save-excursion
	(set-buffer (process-buffer (get-process proc)))
	(ess-make-buffer-current))
      (if noswitch
	  nil
	(ess-show-buffer (buffer-name (process-buffer (get-process proc))) t))
      proc)))


(defun ess-force-buffer-current (&optional prompt force)
  "Make sure the current buffer is attached to an ESS process.
If not, or FORCE (prefix argument) is non-nil,
prompt for a process name with PROMPT.
`ess-local-process-name' is set to the name of the process selected.
`ess-dialect' is set to the dialect associated with the process selected."
  (interactive
   (list (concat ess-dialect " process to use: ") current-prefix-arg))
  (if (and (not force) (ess-make-buffer-current))
      nil ; do nothing
    ;; Make sure the source buffer is attached to a process
    (if (and ess-local-process-name (not force))
	(error "Process %s has died" ess-local-process-name)
      ;; ess-local-process-name is nil -- which process to attach to
      (save-excursion
	(let ((proc (ess-request-a-process prompt 'no-switch))
	      temp-ess-help-filetype
	      dialect)
	  (save-excursion
	    (set-buffer (process-buffer (get-process proc)))
	    (setq temp-ess-help-filetype inferior-ess-help-filetype)
	    (setq dialect ess-dialect))
	  (setq ess-local-process-name proc)
	  (setq inferior-ess-help-filetype temp-ess-help-filetype)
	  (setq ess-dialect dialect))))))

(defun ess-switch-process ()
  "Force a switch to a new underlying process."
  (interactive)
  (ess-force-buffer-current "Process to use: " t))

;;*;;; Commands for switching to the process buffer

(defun ess-switch-to-ESS (eob-p)
  "Switch to the current inferior ESS process buffer.
With (prefix) EOB-P non-nil, positions cursor at end of buffer.
This function should follow the description in `ess-show-buffer'
for showing the iESS buffer, except that the iESS buffer is also
made current."
  (interactive "P")
  (ess-make-buffer-current)
  (if (and ess-current-process-name (get-process ess-current-process-name))
      (progn
	;; Display the buffer, but don't select it yet.
	(ess-show-buffer
	 (buffer-name (process-buffer (get-process ess-current-process-name)))
	 t)
	(if eob-p (goto-char (point-max))))
    (message "No inferior ESS process")
    (ding)))

(defun ess-switch-to-end-of-ESS ()
  "Switch to the end of the inferior ESS process buffer."
  (interactive)
  (ess-switch-to-ESS t))

(defun get-ess-buffer (name)
  "Return the buffer associated with the ESS process named by NAME."
  (process-buffer (get-ess-process name)))

(defun update-ess-process-name-list ()
  "Remove names with no process."
  (let (defunct)
    (mapc
     '(lambda (conselt)
	(let ((proc (get-process (car conselt))))
	  (if (and proc (eq (process-status proc) 'run)) nil
	    (setq defunct (cons conselt defunct)))))
     ess-process-name-list)
    (mapc
     '(lambda (pointer)
	(setq ess-process-name-list (delq pointer ess-process-name-list)))
     defunct))
  (if (eq (length ess-process-name-list) 0)
      (setq ess-current-process-name nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ess-show-buffer
;; Something like this almost works, but problems with XEmacs and Emacs
;; differing implementations of the args to display-buffer make this
;; too tough to pursue.	 The longer version below works.
;; (defun ess-show-buffer (buf)
;;   "Display the buffer BUF, a string, but do not select it.
;; Returns the window corresponding to the buffer."
;;   ;; On XEmacs, I get an error if third arg to display-buffer is t and
;;   ;; the BUF is in another frame.  Emacs does not have this problem.
;;   (if (featurep 'xemacs)
;;	 (display-buffer buf nil (get-frame-for-buffer buf))
;;     (display-buffer buf nil t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ess-show-buffer (buf &optional visit)
  "Ensure the ESS buffer BUF is visible.
The buffer, specified as a string, is typically an iESS (e.g. *R*) buffer.

This handles several cases:

1. If BUF is visible in the current frame, nothing is done.
2. If BUF is visible in another frame, then we ensure that frame is
visible (it may have been iconified).
3. If buffer is not visible in any frame, simply show it in another window
in the current frame.

If VISIT is non-nil, as well as making BUF visible, we also select it
as the current buffer."
  (let ( (frame))
    (if (ess-buffer-visible-this-frame buf)
	;;1. Nothing to do, BUF visible in this frame; just return window
	;; where this buffer is.
	t

      ;; 2. Maybe BUF visible in another frame.
      (setq frame (ess-buffer-visible-other-frame buf))
      (if frame
	  ;; BUF is visible in frame, so just check frame is raised.
	  (if (not (eq (frame-visible-p frame) t))
	      ;; frame is not yet visible, so raise it.
	      (raise-frame frame))
	;; 3. else BUF not visible in any frame, so show it (but do
	;; not select it) in another window in current frame.
	(display-buffer buf)))
    ;; At this stage, the buffer should now be visible on screen,
    ;; although it won't have been made current.
    (when visit
      ;; Need to select the buffer.
      ;;
      ;; First of all, check case 2 if buffer is in another frame
      ;; but that frame may not be selected.
      (if frame
	  (ess-select-frame-set-input-focus frame))
      (select-window (get-buffer-window buf 0)))))


(defvar ess-bufs-in-frame nil)		;silence the compiler.
;; The next few functions are copied from my (SJE) iswitchb library.
(defun ess-get-bufname (win)
  "Used by `ess-get-buffers-in-frames' to walk through all windows."
  (let ((buf (buffer-name (window-buffer win))))
    (if (not (member buf ess-bufs-in-frame))
	;; Only add buf if it is not already in list.
	;; This prevents same buf in two different windows being
	;; put into the list twice.
	(setq ess-bufs-in-frame
	      (cons buf ess-bufs-in-frame)))))

(defun ess-get-buffers-in-frames (&optional current)
 "Return the list of buffers that are visible in the current frame.
If optional argument CURRENT is given, restrict searching to the
current frame, rather than all frames."
 (let ((ess-bufs-in-frame nil))
   (walk-windows 'ess-get-bufname nil (if current nil 0))
   ess-bufs-in-frame))

(defun ess-buffer-visible-this-frame (buf)
 "Return t if BUF is visible in current frame."
 (member buf (ess-get-buffers-in-frames t)))

(defun ess-buffer-visible-other-frame (buf)
 "Return t if BUF is visible in another frame.
Assumes that buffer has not already been in found in current frame."
 (if (member buf (ess-get-buffers-in-frames))
     (window-frame (get-buffer-window buf 0))
   nil))


 ; Functions for evaluating code

(defun ess-ddeclient-p ()
  "Returns t iff `ess-local-process-name' is associated with an
inferior-ess-ddeclient, and nil if the ess-process is running as an
ordinary inferior process.  Alway nil on Unix machines."
  (interactive)
  (if ess-microsoft-p
      (progn
	;; Debug: C-c C-l fails (to start R or give good message) in Windows
	(ess-write-to-dribble-buffer
	 (format "*ddeclient-p: ess-loc-proc-name is '%s'" ess-local-process-name))
	(ess-force-buffer-current "Process to load into: ")
	(not (equal (ess-get-process-variable
		     ess-local-process-name 'inferior-ess-ddeclient)
		    (default-value 'inferior-ess-ddeclient))))))

(defun ess-prompt-wait (proc &optional start-of-output sleep)
  "Wait for a prompt to appear at BOL of current buffer.
PROC is the ESS process. Does not change point"
  (if sleep (sleep-for sleep)); we sleep here, *and* wait below
  (if start-of-output nil (setq start-of-output (point-min)))
  (save-excursion
    (while (progn
	     ;; get output if there is some ready

;;	     (if (and ess-microsoft-p ess-ms-slow)
;;		 (accept-process-output proc 0 1500) ; Microsoft is slow
	       (accept-process-output proc 0 500)
;;	       )
	     (goto-char (marker-position (process-mark proc)))
	     (beginning-of-line)
	     (if (< (point) start-of-output) (goto-char start-of-output))
	     (not (looking-at inferior-ess-primary-prompt))))))

(defun ordinary-insertion-filter (proc string)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point) (process-mark proc)))
	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))

(defun ess-command (com &optional buf sleep no-prompt-check)
  "Send the ESS process command COM and delete the output
from the ESS process buffer.  If an optional second argument BUF exists
save the output in that buffer. BUF is erased before use.
COM should have a terminating newline.
Guarantees that the value of .Last.value will be preserved.
When optional third arg SLEEP is non-nil, `(sleep-for (* a SLEEP))'
will be used in a few places where `a' is proportional to `ess-cmd-delay'."
  ;; Use this function when you need to evaluate some S code, and the
  ;; result is needed immediately. Waits until the output is ready

  ;; the ddeclient-p checks needs to use the local-process-name
  (unless buf
    (setq buf (get-buffer-create " *ess-command-output*")))
  (with-current-buffer buf
    (unless ess-local-process-name
      (setq ess-local-process-name ess-current-process-name)))
  (if (ess-ddeclient-p)
      (ess-command-ddeclient com buf sleep)

    ;; else: "normal", non-DDE behavior:

    (let* ((sprocess (get-ess-process ess-current-process-name))
	   sbuffer
	   do-sleep end-of-output
	   oldpb oldpf oldpm
	   )
      (if (null sprocess)
	;; should hardly happen, since (get-ess-process *)  already checked:
	(error "Process %s is not running!" ess-current-process-name))
      (setq sbuffer (process-buffer sprocess))
      (save-excursion
	(set-buffer sbuffer)
	(setq do-sleep		    ; only now when in sprocess buffer
	      (progn
		(if sleep (if (numberp sleep) nil (setq sleep 1))) ; t means 1
		(and ess-cmd-delay sleep)))
	(if do-sleep (setq sleep (* sleep ess-cmd-delay)))
	(ess-if-verbose-write (format "(ess-command %s ..)" com))
	(save-excursion
	  (goto-char (marker-position (process-mark sprocess)))
	  (beginning-of-line)
	  (unless no-prompt-check
	    (if (looking-at inferior-ess-primary-prompt)
		nil
	      (ess-error
	       "ESS process not ready. Finish your command before trying again."))))
	(setq oldpf (process-filter sprocess))
	(setq oldpb (process-buffer sprocess))
	(setq oldpm (marker-position (process-mark sprocess)))
	;; need the buffer-local values in result buffer "buf":
	(unwind-protect
	    (progn
	      (set-process-buffer sprocess buf)
	      (set-process-filter sprocess 'ordinary-insertion-filter)
	      ;; Output is now going to BUF:
	      (save-excursion
		(set-buffer buf)
		(erase-buffer)
		(set-marker (process-mark sprocess) (point-min))
		(process-send-string sprocess com)
		;; need time for ess-create-object-name-db on PC
		(if no-prompt-check
		    (sleep-for 0.020); 0.1 is noticable!
		    ;; else: default
		  (ess-prompt-wait sprocess nil
				   (and do-sleep (* 0.4 sleep))));MS: 4
		;;(if do-sleep (sleep-for (* 0.0 sleep))); microsoft: 0.5

		;; goto end, making sure final prompt is deleted
		;; FIXME? do this with much less code
		(goto-char (point-max))
		(save-excursion
		  (beginning-of-line)
		  (setq end-of-output (point)))
		(delete-region end-of-output (point-max))
		))
	  (ess-if-verbose-write " .. ok{ess-command}\n")
	  ;; Restore old values for process filter
	  (set-process-buffer sprocess oldpb)
	  (set-process-filter sprocess oldpf)
	  (set-marker (process-mark sprocess) oldpm))))))

(defun ess-replace-in-string (str regexp newtext &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\."
  (if (not (stringp str))
      (error "(replace-in-string): First argument must be a string: %s" str))
  (if (stringp newtext)
      nil
    (error "(replace-in-string): 3rd arg must be a string: %s"
	   newtext))
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	      rtn-str
	      (substring str prev-start match)
	      (cond (literal newtext)
		    (t (mapconcat
			 (function
			   (lambda (c)
			     (if special
				 (progn
				   (setq special nil)
				   (cond ((eq c ?\\) "\\")
					 ((eq c ?&)
					  (substring str
						     (match-beginning 0)
						     (match-end 0)))
					 ((and (>= c ?0) (<= c ?9))
					  (if (> c (+ ?0 (length
							   (match-data))))
					      ;; Invalid match num
					      (error "(replace-in-string) Invalid match num: %c" c)
					    (setq c (- c ?0))
					    (substring str
						       (match-beginning c)
						       (match-end c))))
					 (t (char-to-string c))))
			       (if (eq c ?\\) (progn (setq special t) nil)
				 (char-to-string c)))))
			 newtext ""))))))
    (concat rtn-str (substring str start))))



;;*;;  Evaluating lines, paragraphs, regions, and buffers.

;;--- The two basic building blocks [called by all other ess-eval..] are
;;	(ess-eval-linewise ....)
;; and
;;	(ess-eval-region   ....)

(defun ess-eval-linewise (text-withtabs &optional
					invisibly eob even-empty
					wait-last-prompt sleep-sec timeout-ms)
  ;; RDB 28/8/92 added optional arg eob
  ;; AJR 971022: text-withtabs was text.
  ;; MM 2006-08-23: added 'timeout-ms' -- but the effect seems "nil"
  ;; MM 2007-01-05: added 'sleep-sec'
  "Evaluate TEXT-WITHTABS in the ESS process buffer as if typed in w/o tabs.
Waits for prompt after each line of input, so won't break on large texts.

If optional second arg INVISIBLY is non-nil, don't echo commands.  If it
is a string, just include that string.	If optional third arg
EOB is non-nil go to end of ESS process buffer after evaluation.  If
optional 4th arg EVEN-EMPTY is non-nil, also send empty text (e.g. an
empty line).  If 5th arg WAIT-LAST-PROMPT is non-nil, also wait for
the prompt after the last line;  if 6th arg SLEEP-SEC is a number, ESS
will call '(\\[sleep-for] SLEEP-SEC) at the end of this function.  If the
7th arg TIMEOUT-MS is set to number, it will be used instead of the
default 100 ms and be passed to \\[accept-process-output]."
;; but the effect is unclear
  (if (ess-ddeclient-p)
      (ess-eval-linewise-ddeclient text-withtabs
				   invisibly eob even-empty
				   (if wait-last-prompt
				       ess-eval-ddeclient-sleep))

    ;; else: "normal", non-DDE behavior:

    ;; Use this to evaluate some code, but don't wait for output.
    (let* ((deactivate-mark); keep local {do *not* deactivate wrongly}
	   (cbuffer (current-buffer))
	   (sprocess (get-ess-process ess-current-process-name))
	   (sbuffer (process-buffer sprocess))
	   (text (ess-replace-in-string text-withtabs "\t" " "))
	   start-of-output
	   com pos txt-gt-0)

      (unless (numberp timeout-ms)
	(setq timeout-ms 100));; << make '100' into a custom-variable

      ;;(message "'ess-eval-linewise: sbuffer = %s" sbuffer)
      (set-buffer sbuffer)

      ;; the following is required to make sure things work!
      (when (string= ess-language "STA")
	(if ess-sta-delimiter-friendly;; RAS: mindless replacement of semi-colons
	    (setq text (ess-replace-in-string text ";" "\n")))
	(setq invisibly t))
      ;; dbg:
      ;; dbg(ess-write-to-dribble-buffer
      ;; dbg (format "(eval-visibly 1): lang %s (invis=%s, eob=%s, even-empty=%s)\n"
      ;; dbg	 ess-language invisibly eob even-empty))

      (goto-char (marker-position (process-mark sprocess)))
      (if (stringp invisibly)
	  (insert-before-markers (concat "*** " invisibly " ***\n")))
      ;; dbg:
      ;; dbg (ess-write-to-dribble-buffer
      ;; dbg  (format "(eval-visibly 2): text[%d]= '%s'\n" (length text) text))
      (while (or (setq txt-gt-0 (> (length text) 0))
		 even-empty)
	(if even-empty (setq even-empty nil))
	(if txt-gt-0
	    (progn
	      (setq pos (string-match "\n\\|$" text))
	      (setq com (concat (substring text 0 pos) "\n"))
	      (setq text (substring text (min (length text) (1+ pos)))))
	  ;; else 0-length text
	  (setq com "\n")
	  )
	(goto-char (marker-position (process-mark sprocess)))
	(if (not invisibly)
	    ;; Terrible kludge -- need to insert after all markers *except*`
	    ;; the process mark
	    (let ((dokludge (eq (point)
				(marker-position (process-mark sprocess)))))
	      (insert com)
	      (if dokludge (set-marker (process-mark sprocess) (point)))))
	(setq start-of-output (marker-position (process-mark sprocess)))
	;; A kludge to prevent the delay between insert and process output
	;; affecting the display.	 A case for a comint-send-input-hook?
	;; (save-excursion
	;;   ;; comint-postoutput-scroll-to-bottom can change
	;;   ;; current-buffer. Argh.
	;;   (let ((functions comint-output-filter-functions))
	;;     (while functions
	;;	 (funcall (car functions) com)
	;;	 (setq functions (cdr functions)))))
	(process-send-string sprocess com)
	;; wait for the prompt - after the last line of input only if wait-last:
	(if (or wait-last-prompt
		(> (length text) 0))
	  (while (progn
		   (accept-process-output sprocess 0 timeout-ms)
		   (goto-char (marker-position (process-mark sprocess)))
		   (beginning-of-line)
		   (if (< (point) start-of-output)
		       (goto-char start-of-output))
		   (not (looking-at inferior-ess-prompt))))))

      (goto-char (marker-position (process-mark sprocess)))
      (if eob
	  (progn
	    (ess-show-buffer (buffer-name sbuffer) nil)
	    ;; Once SBUFFER is visible, we can then move the point in that
	    ;; window to the end of the buffer.
	    (set-window-point (get-buffer-window sbuffer t)
			      (with-current-buffer sbuffer (point-max))))
	(set-buffer cbuffer))
      (if (numberp sleep-sec)
	  (sleep-for sleep-sec))))); in addition to timeout-ms


;;;*;;; Evaluate only

(defun ess-eval-region (start end toggle &optional message)
  "Send the current region to the inferior ESS process.
With prefix argument toggle the meaning of `ess-eval-visibly-p';
this does not apply when using the S-plus GUI, see `ess-eval-region-ddeclient'."
  (interactive "r\nP")
  ;;(untabify (point-min) (point-max))
  ;;(untabify start end); do we really need to save-excursion?
  (ess-force-buffer-current "Process to load into: ")
  (message "Starting evaluation...")

  (if (ess-ddeclient-p)
      (ess-eval-region-ddeclient start end 'even-empty)
    ;; else: "normal", non-DDE behavior:
    (let ((visibly (if toggle (not ess-eval-visibly-p) ess-eval-visibly-p)))
      (if visibly
	  (ess-eval-linewise (buffer-substring start end))
	(if ess-synchronize-evals
	    (ess-eval-linewise (buffer-substring start end)
			       (or message "Eval region"))
	  ;; else [almost always!]
	  (let ((sprocess (get-ess-process ess-current-process-name)))
	    (process-send-region sprocess start end)
	    (process-send-string sprocess "\n"))))))

  (message "Finished evaluation")
  (if (and (fboundp 'deactivate-mark) ess-eval-deactivate-mark)
      (deactivate-mark))
  ;; return value
  (list start end))

(defun ess-eval-buffer (vis)
  "Send the current buffer to the inferior ESS process.
Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-region (point-min) (point-max) vis "Eval buffer"))

(defun ess-eval-buffer-from-beg-to-here (vis)
  (interactive "P")
  (ess-eval-region (point-min) (point) vis "Eval buffer from the beginning
of the buffer until here, i.e. 'point'"))

(defun ess-eval-buffer-from-here-to-end (vis)
  (interactive "P")
  (ess-eval-region (point) (point-max) vis "Eval buffer from here ('point') until
the end of the buffer"))


(defun ess-eval-function (vis)
  "Send the current function to the inferior ESS process.
Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (save-excursion
    (let* ((beg-end (ess-end-of-function))
	   (beg (nth 0 beg-end))
	   (end (nth 1 beg-end))
	   name)
      (goto-char beg)
      (setq name (ess-read-object-name-default))
      (princ (concat "Loading: " name) t)
      (ess-eval-region beg end vis
		       (concat "Eval function " name)))))

;; This is from	 Mary Lindstrom <lindstro@Biostat.Wisc.Edu>
;; 31 Aug 1995 14:11:43		To: S-mode@stat.math.ethz.ch
(defun ess-eval-paragraph (vis)
  "Send the current paragraph to the inferior ESS process.
Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
  (interactive "P")
  (save-excursion
    (forward-paragraph)
    (let ((end (point)))
      (backward-paragraph)
      (ess-eval-region (point) end vis "Eval paragraph"))))

;; ;; Experimental - after suggestion from Jenny Brian for an 'eval-multiline'
;; ;; 'sentence' is too much : almost like 'paragraph'
;; ;; 'sexp'     is close, but too little [when point is inside function call;
;; ;;	      it moves all the way to the end - which is fine]
;; (defun ess-eval-sexp (vis)
;;   "Send the current sexp to the inferior ESS process.
;; Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
;;   (interactive "P")
;;   (save-excursion
;;     (forward-sexp)
;;     (let ((end (point)))
;;       (backward-sexp)
;;       (ess-eval-region (point) end vis "Eval sexp"))))


(defun ess-eval-function-or-paragraph-and-step (vis)
  "Send the current function if \\[point] is inside one, otherwise the current
paragraph other to the inferior ESS process.
Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
  (interactive "P")
  (let ((beg (ess-beginning-of-function 'no-error)))
    (if beg ;; inside a function
	(let ((end-fun (cadr (ess-end-of-function beg)))
	      name)
	  (goto-char beg)
	  (setq name (ess-read-object-name-default))
	  (princ (concat "Loading: " name) t)
	  (ess-eval-region beg end-fun vis
			   (concat "Eval function " name))
	  (goto-char end-fun)
	  (ess-next-code-line))
      ;; else: not in a function
      (ess-eval-paragraph-and-step vis))))


(defun ess-eval-line (vis)
  "Send the current line to the inferior ESS process.
Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (princ (concat "Loading line: " (ess-extract-word-name) " ...") t)
      (ess-eval-region (point) end vis "Eval line"))))



;; Contributed by  Stephen Eglen <stephen@anc.ed.ac.uk> {idea from octave int.}
(defun ess-next-code-line (&optional arg)
  "Move ARG lines of code forward (backward if ARG is negative).
Skips past all empty and comment lines.	 Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc)); n=0 is success
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))

(defun ess-eval-line-and-step (&optional simple-next even-empty invisibly)
  "Evaluate the current line visibly and step to the \"next\" line.
\"next\" = the next line with non-comment code _unless_ SIMPLE-NEXT is non-nil,
possibly via prefix arg.  If 2nd arg EVEN-EMPTY [prefix as well],
also send empty lines.	When the variable `ess-eval-empty' is non-nil
both SIMPLE-NEXT and EVEN-EMPTY are interpreted as true."
  ;; From an idea by Rod Ball (rod@marcam.dsir.govt.nz)
  (interactive "P\nP"); prefix sets BOTH !
  (save-excursion
    (ess-force-buffer-current "Process to load into: ")
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      ;; go to end of process buffer so user can see result
      (ess-eval-linewise (buffer-substring (point) end)
			 invisibly 'eob (or even-empty ess-eval-empty))))
  (if (or simple-next ess-eval-empty)
      (forward-line 1)
    (ess-next-code-line 1)))

(defun ess-eval-line-and-step-invisibly ()
   "Evaluate the current line invisibly and step to the next line.
Evaluate all comments and empty lines."
  (interactive)
  (ess-eval-line-and-step t t t))

;; goes to the real front, in case you do double function definition
;; 29-Jul-92 -FER
;; don't know why David changed it.

;; FER's versions don't work properly with nested functions. Replaced
;; mine. DMS 16 Nov 92

;;;*;;; Evaluate and switch to S

(defun ess-eval-region-and-go (start end vis)
  "Send the current region to the inferior S and switch to the process buffer.
Arg has same meaning as for `ess-eval-region'."
  (interactive "r\nP")
  (ess-eval-region start end vis)
  (ess-switch-to-ESS t))

(defun ess-eval-buffer-and-go (vis)
  "Send the current buffer to the inferior S and switch to the process buffer.
Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-buffer vis)
  (ess-switch-to-ESS t))

(defun ess-eval-function-and-go (vis)
  "Send the current function to the inferior ESS process and switch to
the process buffer. Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-function vis)
  (ess-switch-to-ESS t))

(defun ess-eval-line-and-go (vis)
  "Send the current line to the inferior ESS process and switch to the
process buffer. Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-line vis)
  (ess-switch-to-ESS t))

(defun ess-eval-paragraph-and-go (vis)
  "Send the current paragraph to the inferior ESS process and switch to the
process buffer. Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-paragraph vis)
  (ess-switch-to-ESS t))

(defun ess-eval-paragraph-and-step (vis)
  "Send the current paragraph to the inferior ESS process and move forward to
the next paragraph.  Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (let ((beg-end (ess-eval-paragraph vis)))
    (goto-char (cadr beg-end))
    (ess-next-code-line))
)

;;; Related to the ess-eval-* commands, there are the ess-load
;;; commands.	Need to add appropriate stuff...


(defun ess-load-file (filename)
  "Load an S source file into an inferior ESS process."
  (interactive (list
		(or
		 (and (eq major-mode 'ess-mode)
		      (buffer-file-name))
		 (expand-file-name
		  (read-file-name "Load S file: " nil nil t)))))
  (ess-make-buffer-current)
  (if ess-microsoft-p
      (setq filename (ess-replace-in-string filename "[\\]" "/")))
  (let ((source-buffer (get-file-buffer filename)))
    (if (ess-check-source filename)
	(error "Buffer %s has not been saved" (buffer-name source-buffer)))
    ;; else
    (if (ess-ddeclient-p)
	(ess-load-file-ddeclient filename)

      ;; else: "normal", non-DDE behavior:

      ;; Find the process to load into
      (if source-buffer
	  (save-excursion
	    (set-buffer source-buffer)
	    (ess-force-buffer-current "Process to load into: ")
	    (ess-check-modifications)))
      (let ((errbuffer (ess-create-temp-buffer ess-error-buffer-name))
	    (filename (if (and (fboundp 'tramp-tramp-file-p)
			       (tramp-tramp-file-p filename))
			  (tramp-file-name-localname (tramp-dissect-file-name filename))
			filename))
	    error-occurred nomessage)
	(ess-command (format inferior-ess-load-command filename) errbuffer) ;sleep ?
	(save-excursion
	  (set-buffer errbuffer)
	  (goto-char (point-max))
	  (setq error-occurred (re-search-backward ess-dump-error-re nil t))
	  (setq nomessage (= (buffer-size) 0)))
	(if error-occurred
	    (message "Errors: Use %s to find error."
		     (substitute-command-keys
		      "\\<inferior-ess-mode-map>\\[ess-parse-errors]"))
	  ;; Load did not cause an error
	  (if nomessage (message "Load successful.")
	    ;; There was a warning message from S
	    (ess-display-temp-buffer errbuffer))
	  ;; Consider deleting the file
	  (let ((skdf (if source-buffer
			  (save-excursion
			    (set-buffer source-buffer)
			    ess-keep-dump-files)
			ess-keep-dump-files))) ;; global value
	    (cond
	     ((null skdf)
	      (delete-file filename))
	     ((memq skdf '(check ask))
	      (let ((doit (y-or-n-p (format "Delete %s " filename))))
		(if doit (delete-file filename))
		(and source-buffer
		     (local-variable-p 'ess-keep-dump-files source-buffer)
		     (save-excursion
		       (set-buffer source-buffer)
		       (setq ess-keep-dump-files doit)))))))
	  (ess-switch-to-ESS t))))))

 ; Inferior S mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The major mode inferior-ess-mode
;;;; * Process handling code
;;;; * Completion code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*;; Major mode definition

(if inferior-ess-mode-map
    nil

  (cond ((featurep 'xemacs)
	 ;; Code for XEmacs
	 (setq inferior-ess-mode-map (make-keymap))
	 (set-keymap-parent inferior-ess-mode-map comint-mode-map))
	((not (featurep 'xemacs))
	 ;; Code for GNU Emacs
	 (setq inferior-ess-mode-map (cons 'keymap comint-mode-map))))

  ;; Use syntax valid *both* for GNU emacs and XEmacs :
  (define-key inferior-ess-mode-map "\r"       'inferior-ess-send-input)
  (define-key inferior-ess-mode-map "\C-a"     'comint-bol)

  ;; 2010-06-03 SJE
  ;; disabled this in favour of ess-dirs.  Martin was not sure why this
  ;; key was defined anyway in this mode.
  ;;(define-key inferior-ess-mode-map "\M-\r"    'ess-transcript-send-command-and-move)
  (define-key inferior-ess-mode-map "\C-c\C-l" 'ess-load-file)
  ;; the above OVERRIDES  comint-dynamic-list-input-ring --> re-assign:
  (define-key inferior-ess-mode-map "\C-c\M-l" 'comint-dynamic-list-input-ring)
  (define-key inferior-ess-mode-map "\C-c`"    'ess-parse-errors)
  (define-key inferior-ess-mode-map "\C-c\C-d" 'ess-dump-object-into-edit-buffer)
  (define-key inferior-ess-mode-map "\C-c\C-v" 'ess-display-help-on-object)
  (define-key inferior-ess-mode-map "\C-c\C-q" 'ess-quit)
  (define-key inferior-ess-mode-map "\C-c\C-t" 'ess-execute)
  (define-key inferior-ess-mode-map "\C-c\C-s" 'ess-execute-search)
  (define-key inferior-ess-mode-map "\C-c\C-x" 'ess-execute-objects)
  ;;(define-key inferior-ess-mode-map "\C-c\C-a" 'ess-execute-attach)
  (define-key inferior-ess-mode-map "\C-c\034" 'ess-abort) ; \C-c\C-backslash
  (define-key inferior-ess-mode-map "\C-c\C-z" 'ess-abort) ; mask comint map
  (define-key inferior-ess-mode-map "\C-d"     'delete-char)   ; EOF no good in S
  (define-key inferior-ess-mode-map "\t"       'comint-dynamic-complete)
  (define-key inferior-ess-mode-map "\C-c\t"   'ess-complete-object-name)
  (define-key inferior-ess-mode-map "\M-\t"    'comint-replace-by-expanded-filename)
  (define-key inferior-ess-mode-map "\M-?"     'ess-list-object-completions)
  (define-key inferior-ess-mode-map "\C-c\C-k" 'ess-request-a-process))

(easy-menu-define
 inferior-ess-mode-menu inferior-ess-mode-map
 "Menu for use in Inferior S mode"
 '("iESS"
   ["What is this? (beta)"   ess-mouse-me		   t]
   ["Resynch S completions"  ess-resynch		   t]
   ["Quit S"		     ess-quit			   t]
   ["Display search list"    ess-execute-search		   t]
   ["Display object list"    ess-execute-objects	   t]
   ["Get help on S object"   ess-display-help-on-object	   t]
   ["Enter S command"	     ess-execute		   t]
   ["Attach directory"	     ess-execute-attach		   t]
   "------"
   ["Send and move"  ess-transcript-send-command-and-move  t]
   ["Copy command"   comint-copy-old-input		   t]
   ["Send command"   inferior-ess-send-input		   t]
   "------"
   ["Jump to Error"  ess-parse-errors			   t]
   ;; need a toggle switch for above, AJR.
   ["Load source file"	ess-load-file			   t]
   ["Edit S Object"	ess-dump-object-into-edit-buffer   t]
   "------"
   ["Describe"	       describe-mode			   t]
   ["About"	       (ess-goto-info "Entering Commands") t]
   ["Send bug report"  ess-submit-bug-report		   t]
   ))


(defun inferior-ess-mode-xemacs-menu ()
  "Hook to install `ess-mode' menu for XEmacs (w/ easymenu)."
  (if 'inferior-ess-mode
	(easy-menu-add inferior-ess-mode-menu)
    (easy-menu-remove inferior-ess-mode-menu)))

(if (string-match "XEmacs" emacs-version)
    (add-hook 'inferior-ess-mode-hook 'inferior-ess-mode-xemacs-menu))

(if ess-mode-minibuffer-map  nil

  (cond ((featurep 'xemacs)
	 ;; Code for XEmacs
	 (setq ess-mode-minibuffer-map (make-keymap))
	 (set-keymap-parent ess-mode-minibuffer-map minibuffer-local-map)))

  (cond ((not (featurep 'xemacs))
	 ;; Code for Emacs
	 (setq ess-mode-minibuffer-map (cons 'keymap minibuffer-local-map))))

  (define-key ess-mode-minibuffer-map "\t" 'ess-complete-object-name)
  (define-key ess-mode-minibuffer-map "\C-c\C-s" 'ess-execute-search)
  (define-key ess-mode-minibuffer-map "\C-c\C-x" 'ess-execute-objects))

(defun inferior-ess-mode ()
  "Major mode for interacting with an inferior ESS process.
Runs an S interactive job as a subprocess of Emacs, with I/O through an
Emacs buffer.  Variable `inferior-ess-program' controls which S
is run.

Commands are sent to the ESS process by typing them, and pressing
\\[inferior-ess-send-input].  Pressing \\[complete-dynamic-complete]
completes known object names or filenames, as appropriate.  Other
keybindings for this mode are:

\\{inferior-ess-mode-map}

When editing S objects, the use of \\[ess-load-file] is advocated.
`ess-load-file' keeps source files (if `ess-keep-dump-files' is non-nil) in
the directory specified by `ess-source-directory', with the
filename chosen according to `ess-dump-filename-template'. When a file is
loaded, `ess-mode' parses error messages and jumps to the appropriate file
if errors occur. The ess-eval- commands do not do this.

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-ess-mode-hook' (in that order).

You can send text to the inferior ESS process from other buffers containing
S source. The key bindings of these commands can be found by typing
C-h m (help for mode) in the other buffers.
    `ess-eval-region' sends the current region to the ESS process.
    `ess-eval-buffer' sends the current buffer to the ESS process.
    `ess-eval-function' sends the current function to the ESS process.
    `ess-eval-line' sends the current line to the ESS process.
    `ess-beginning-of-function' and `ess-end-of-function' move the point to
	the beginning and end of the current S function.
    `ess-switch-to-ESS' switches the current buffer to the ESS process buffer.
    `ess-switch-to-end-of-ESS' switches the current buffer to the ESS process
	buffer and puts point at the end of it.

    `ess-eval-region-and-go', `ess-eval-buffer-and-go',
	`ess-eval-function-and-go', and `ess-eval-line-and-go' switch to the S
	process buffer after sending their text.
    `ess-dump-object-into-edit-buffer' moves an S object into a temporary file
	and buffer for editing
    `ess-load-file' sources a file of commands to the ESS process.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Crosshatches start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  ;;(message " at very beg. of (inferior-ess-mode): inf.-ess-prompt= %s"
  ;;	   inferior-ess-prompt)
  (comint-mode)
  ;;

  ;; SJE: is this the proper place for setting inferior-ess-prompt,
  ;; rather than within ess-multi?  Tony - have you remembered yet
  ;; about the setq-default, as I changed it back to setq.
  (setq inferior-ess-prompt
	;; shouldn't be setq-default!  And I've
	;; forgotten why!   (AJR)
	;; Do not anchor to bol with `^'
	(concat "\\("
		inferior-ess-primary-prompt
		"\\|"
		inferior-ess-secondary-prompt
		"\\)"))
  (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))
  (setq major-mode 'inferior-ess-mode)
  (setq mode-name "iESS")		;(concat "iESS:" ess-dialect))
  (setq mode-line-process
	'(" [" ess-local-process-name "]: %s"))
  (use-local-map inferior-ess-mode-map)
  (if ess-mode-syntax-table
      (set-syntax-table ess-mode-syntax-table)
    ;; FIXME: need to do something if not set!	Get from the proper place!
    )
  (add-hook 'comint-input-filter-functions 'ess-search-path-tracker)
  (setq comint-get-old-input 'inferior-ess-get-old-input)

  (ess-write-to-dribble-buffer
   (format "(i-ess 1): buf=%s, lang=%s, comint..echo=%s, comint..sender=%s,\n"
	   (current-buffer) ess-language
	   comint-process-echoes comint-input-sender))

  (make-local-variable 'comint-process-echoes)
  (make-local-variable 'comint-input-sender)
  (set (make-local-variable 'process-connection-type) t)

  ;; Configuration for SAS/XLispStat input handling
  ;; We set comint-process-echoes to t because inferior-ess-input-sender
  ;; recopies the input. If comint-process-echoes was *meant* to be t ...
  ;;
  ;; except that XLS doesn't like it.  This is an ugly hack that ought
  ;; to go into the dialect configuration...
  (setq comint-process-echoes (not (member ess-language '("SAS" "XLS"))))

  ;; Configuration for S/R input handling
  ;; AJR: add KH's fix.	 This is ugly, change to do it right.
  ;; i.e. put it as a buffer local var, in S or R defuns...
  ;;
  ;; SJE: Do you mean that we should put this code into (R) and the S
  ;; dialects?  I agree that would be cleaner. e.g. in ess-r-d.el, for
  ;; the R defun we could have:
  ;; (inferior-ess r-start-args) ;; (R)
  ;; (setq comint-input-sender 'inferior-R-input-sender) ;; <<- add this.
  (if (or (string= ess-language "S"))
      (cond
       ((string= ess-dialect "R")
	(setq comint-input-sender 'inferior-R-input-sender))
       ( (member ess-dialect '("S3" "S4" "S+3" "S+4" "S+5" "S+6" "S"))
	(setq comint-input-sender 'inferior-ess-input-sender))))

  (when (string= ess-language "S")
    (local-set-key "\M-\r"    'ess-dirs))

  ;; Configuration for Stata input handling
  ;; AJR: Stata is hell.   This is the primary configuration point.
  (when (string= ess-language "STA")
    (setq comint-input-sender 'inferior-ess-input-sender) ; was STA
    (setq comint-process-echoes t))

  ;; Configuration for Omegahat input handling
  ;; SJE: cleanup
  (when (string= ess-language "OMG")
    ;; the following doesn't exist (until needed?)
    ;;(setq comint-input-sender 'inferior-OMG-input-sender)
    (setq comint-process-echoes nil))

  (ess-write-to-dribble-buffer
   (format "(i-ess 2): buf=%s, lang=%s, comint..echo=%s, comint..sender=%s,\n"
	   (current-buffer) ess-language
	   comint-process-echoes comint-input-sender))
  ;; Font-lock support
  ;; AJR: This (the following local-var is already the case!
  ;; KH sez: only in XEmacs :-(.  (& Emacs 22.1, SJE).
  (set (make-local-variable 'font-lock-defaults)
       '(inferior-ess-font-lock-keywords nil nil ((?' . "."))))

  ;; SJE 2007-06-28: Emacs 22.1 has a bug in that comint-mode will set
  ;; this variable to t, when we need it to be nil.  The Emacs 22
  ;; solution to this bug is to use define-derived-mode to derive
  ;; inferior-ess-mode from comint-mode.  Not sure if we can go down
  ;; that route yet.  I've used the when condition so that if the var
  ;; is nil, don't bother setting it -- as setting it will make a new
  ;; local var.
  (when font-lock-keywords-only
    (setq font-lock-keywords-only nil))

  (ess-setq-vars-local ess-customize-alist) ; (current-buffer))

  (ess-write-to-dribble-buffer
   (format "(i-ess 3): curr-buf=%s, comint..echo=%s, comint..sender=%s,\n"
	   (current-buffer) comint-process-echoes comint-input-sender))

  ;;; Completion support ----------------

  ;; SJE: comint-dynamic-complete-functions is regarded as a hook, rather
  ;; than a regular variable.  Note order of completion (thanks David Brahm):

  (add-hook 'comint-dynamic-complete-functions
	    'ess-complete-filename 'append 'local)
  (add-hook 'comint-dynamic-complete-functions
	    'ess-complete-object-name 'append 'local)
  (add-hook 'comint-dynamic-complete-functions
	    'comint-replace-by-expanded-history 'append 'local)

  ;; When a hook is buffer-local, the dummy function `t' is added to
  ;; indicate that the functions in the global value of the hook
  ;; should also be run.  SJE: I have removed this, as I think it
  ;; interferes with our normal completion.
  (remove-hook 'comint-dynamic-complete-functions 't 'local)

  ;; MM: in *R* in GNU emacs and in Xemacs, the c*-dyn*-compl*-fun* are now
  ;; (ess-complete-filename
  ;;  ess-complete-object-name
  ;;  comint-replace-by-expanded-history)

  ;; However this fails in Xemacs 21.4.17 where the value in *shell* is
  ;; -- the same as in GNU emacs *shell* :
  ;; (comint-replace-by-expanded-history shell-dynamic-complete-environment-variable shell-dynamic-complete-command shell-replace-by-expanded-directory comint-dynamic-complete-filename)

  ;; and the (Xemacs) global  'Default-value' is
  ;; (comint-replace-by-expanded-history comint-dynamic-complete-filename)

  ;; (setq comint-completion-addsuffix nil) ; To avoid spaces after filenames
  ;; KH: next 2 lines solve.
  (set (make-local-variable 'comint-completion-addsuffix)
       (cons "/" ""))

  (setq comint-input-autoexpand t) ; Only for completion, not on input.

  ;;; Keep <tabs> out of the code.
  (set (make-local-variable 'indent-tabs-mode) nil)

  (set (make-local-variable 'paragraph-start)
       (concat inferior-ess-primary-prompt "\\|\^L"))
  (set (make-local-variable 'paragraph-separate) "\^L")

  ;; SJE Tue 28 Dec 2004: do not attempt to load object name db.
  ;; (ess-load-object-name-db-file)
  ;; (sleep-for 0.5)
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'ess-kill-buffer-function)
  (run-hooks 'inferior-ess-mode-hook)
  (message
   (concat (substitute-command-keys
	    "Type \\[describe-mode] for help on ESS version ")
	   ess-version)))

;;*;; Commands used exclusively in inferior-ess-mode

;;;*;;; Main user commands

(defun inferior-ess-input-sender (proc string)
  (ess-eval-linewise (concat string "\n") nil nil ess-eval-empty))

(defun inferior-STA-input-sender (proc string)
  (ess-eval-linewise (concat string "\n") t t))

;;> <PD writes>:
;;> Also, invoking help() from the command line may lead to confusing
;;> output, somewhat worse with R than with S. You're not really supposed
;;> to do that (use C-c C-v to invoke ess-display-help-on-object), but it's
;;> an obvious newcomer's mistake.
;;>
;;> (I wonder: could the elisp-code not quite easily recognize help
;;> calls (at least in the ?xxx form) and do the right thing automagically?)
;;>
;;> As promised, here is a quick hack:
;;  ___hack much improved by MM___ , both help(.) and ?... now work
;; FIXME: Note that  '??' nicely works in *R*, but
;;	  'type ? topic' doesn't use ess-help {but display in *R*}
(defconst inferior-R-1-input-help (format "^ *help *(%s)" ess-help-arg-regexp))
(defconst inferior-R-2-input-help (format "^ *\\? *%s" ess-help-arg-regexp))
(defconst inferior-R-page	  (format "^ *page *(%s)" ess-help-arg-regexp))

(defun inferior-R-input-sender (proc string)
  ;; next line only for debugging: this S_L_O_W_S D_O_W_N [here AND below]
  ;;(ess-write-to-dribble-buffer (format "(inf..-R-..): string='%s'; " string))
  ;; rmh: 2002-01-12 catch page() in R
  (save-current-buffer
    (let ((help-string (or (string-match inferior-R-1-input-help string)
                           (string-match inferior-R-2-input-help string)))
          (page-string	 (string-match inferior-R-page	       string)))
      (if (or help-string page-string)
          (let* ((string2 (match-string 2 string)))
            ;;(ess-write-to-dribble-buffer (format " new string='%s'\n" string2))
            (beginning-of-line)
            (if (looking-at inferior-ess-primary-prompt)
                (progn
                  (end-of-line)
                  (insert-before-markers string)) ;; emacs 21.0.105 and older
              (delete-backward-char 1)) ;; emacs 21.0.106 and newer
            (if help-string ; more frequently
		(progn
		  (ess-display-help-on-object
		   (if (string= string2 "") "help" string2))
		  (ess-eval-linewise "\n"))

	      ;; else  page-string
	      (let ((str2-buf (concat string2 ".rt")))
		(ess-command (concat string2 "\n")
			     (get-buffer-create str2-buf))
		(ess-eval-linewise "\n")
		(switch-to-buffer-other-window str2-buf)
		(R-transcript-mode))))
        ;; else:	normal command
        (inferior-ess-input-sender proc string)))))


(defun inferior-ess-send-input ()
  "Sends the command on the current line to the ESS process."
  (interactive)
  (ess-make-buffer-current)
  (run-hooks 'ess-send-input-hook)
  (comint-send-input)
  (setq ess-object-list nil)) ;; Will be reconstructed from cache if needs be

(defun inferior-ess-get-old-input ()
  "Return the ESS command surrounding point."
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at inferior-ess-prompt))
	(ess-error "No command on this line."))
    (if (looking-at inferior-ess-primary-prompt) nil
	(re-search-backward (concat "^" inferior-ess-primary-prompt)))
    (comint-skip-prompt)
    (let (command
	   (beg (point)))
      (end-of-line)
      (setq command (buffer-substring beg (point)))
      (forward-line 1)
      (while (looking-at inferior-ess-secondary-prompt)
	(comint-skip-prompt)
	(setq beg (point))
	(end-of-line)
	(setq command (concat command "\n" (buffer-substring beg (point))))
	(forward-line 1))
      (forward-line -1)
      (setq ess-temp-point (point))
      command)))

;;;*;;; Hot key commands

(defun ess-execute-objects (posn)
  "Send the objects() command to the ESS process.
By default, gives the objects at position 1.
A prefix argument toggles the meaning of `ess-execute-in-process-buffer'.
A prefix argument of 2 or more means get objects for that position.
A negative prefix argument gets the objects for that position
  and toggles `ess-execute-in-process-buffer' as well."
  (interactive "P")
  (ess-make-buffer-current)
  (let* ((num-arg (if (listp posn)
		      (if posn -1 1)
		    (prefix-numeric-value posn)))
	(the-posn (if (< num-arg 0) (- num-arg) num-arg))
	(invert (< num-arg 0))
	(the-command (format inferior-ess-objects-command the-posn ".*"))
	(the-message (concat ">>> Position "
			     (number-to-string the-posn)
			     " ("
			     (nth (1- the-posn) (ess-search-list))
			     ")\n")))
    (ess-execute the-command invert "S objects" the-message)))

;;;; S4 Version
;;;; soup-up the interactive usage: allow modifications to a default pattern
;;(defun ess-execute-objects (posn)
;;  "Send the `inferior-ess-objects-command' to the ESS process.
;;No prefix argument uses position 1 and pattern inferior-ess-objects-pattern.
;;A nonnegative prefix gets objects for that position and prompts for
;;  the pattern.
;;A negative prefix also toggles ess-execute-in-process-buffer."
;;  (interactive "P")
;;  (ess-make-buffer-current)
;;  (let* ((num-arg (if (listp posn) 1
;;		    (prefix-numeric-value posn)))
;;	 (the-posn (if (< num-arg 0) (- num-arg) num-arg))
;;	 (invert (< num-arg 0))
;;	 (pattern (if current-prefix-arg (read-string "Pattern (.*): ")
;;		    inferior-ess-objects-pattern))
;;	 (pattern (if (string= pattern "") ".*" pattern))
;;	 (the-command (format inferior-ess-objects-command the-posn pattern))
;;	 (the-message (concat ">>> Position "
;;			      the-posn
;;			      " ("
;;			      (nth (1- the-posn) (ess-search-list))
;;			      ") (pattern = "
;;			      pattern
;;			      ")\n")))
;;    (ess-execute the-command invert "S objects" the-message)))

(defun ess-execute-search (invert)
  "Send the `inferior-ess-search-list-command' command to the `ess-language' process.
 [search(..) in S]"
  (interactive "P")
  (ess-execute inferior-ess-search-list-command	 invert "S search list"))

;; FIXME --- this *only* works in S / S-plus; not in R
;; -----     ("at least" is not assigned to any key by default)
(defun ess-execute-attach (dir &optional posn)
  "Attach a directory in the `ess-language' process with the attach() command.
When used interactively, user is prompted for DIR to attach and
prefix argument is used for POSN (or 2, if absent.)
Doesn't work for data frames."
  (interactive "Attach directory: \nP")
  (ess-execute (concat "attach(\""
		     (directory-file-name (expand-file-name dir))
		     "\""
		     (if posn (concat "," (number-to-string
					   (prefix-numeric-value posn))))
		     ")") 'buffer)
  (setq ess-sp-change t))

(defun ess-execute-screen-options ()
  "Cause S to set the \"width\" option to 1 less than the frame width.
Also sets the \"length\" option to 99999.
This is a good thing to put in `ess-post-run-hook' --- for the S dialects."
  (interactive)
  (if (string= ess-language "S")
      (let ((ess-current-process-name)); local, used as S-process buffer below
	;; when run inside an ESS process buffer, use that one
	(if (and (comint-check-proc (current-buffer)); has running proc
		 (memq major-mode '(inferior-ess-mode)))
	    (setq ess-current-process-name ess-local-process-name))

	(ess-eval-linewise (format "options(width=%d,length=99999)"
				   (1- (window-width)))
			   nil nil nil 'wait-prompt))))

(defun ess-execute (command &optional invert buff message)
  "Send a command to the ESS process.
A newline is automatically added to COMMAND.  Prefix arg (or second arg
INVERT) means invert the meaning of
`ess-execute-in-process-buffer'.  If INVERT is 'buffer, output is
forced to go to the process buffer.  If the output is going to a
buffer, name it *BUFF*.	 This buffer is erased before use.  Optional
fourth arg MESSAGE is text to print at the top of the buffer (defaults
to the command if BUFF is not given.)"
  (interactive (list
		(read-from-minibuffer "Execute> "
				      nil
				      ess-mode-minibuffer-map)
		current-prefix-arg))
  (ess-make-buffer-current)
  (let ((the-command (concat command "\n"))
	(buff-name (concat "*" (or buff "ess-output") "*"))
	(in-pbuff (if invert (or (eq invert 'buffer)
				 (not ess-execute-in-process-buffer))
		    ess-execute-in-process-buffer)))
    (if in-pbuff
	(ess-eval-linewise the-command)
      (let ((buff (ess-create-temp-buffer buff-name)))
	(save-excursion
	  (set-buffer buff)
	  (ess-command the-command (get-buffer buff-name));; sleep?
	  (goto-char (point-min))
	  (if message (insert message)
	    (if buff nil
	      ;; Print the command in the buffer if it has not been
	      ;; given a special name
	      (insert "> " the-command)))
	  (setq ess-local-process-name ess-current-process-name))
	(ess-display-temp-buffer buff)))))

;;;*;;; Quitting

(defun ess-quit ()
  "Issue an exiting command to the inferior process, additionally
also running \\[ess-cleanup].  For R, runs \\[ess-quit-r], see there."
  (interactive)
  (if (string-equal ess-dialect "R")
      (ess-quit-r)
    (ess-force-buffer-current "Process to quit: ")
    (ess-make-buffer-current)
    (let ((sprocess (get-ess-process ess-current-process-name)))
      (if (not sprocess) (error "No ESS process running"))
      (when (yes-or-no-p (format "Really quit ESS process %s? " sprocess))
	(ess-cleanup)
	(goto-char (marker-position (process-mark sprocess)))
	(insert inferior-ess-exit-command)
	(process-send-string sprocess inferior-ess-exit-command)
	;;SJE - suggest no need to rename buffer upon exit.
	;;(rename-buffer (concat (buffer-name) "-exited") t)
	))))

(defun ess-quit-r ()
  "Issue an exiting command to an inferior R process, and optionally clean up.
This version is for killing *R* processes; it asks the extra question
regarding whether the workspace image should be saved."
  (ess-force-buffer-current "Process to quit: ")
  (ess-make-buffer-current)
  (let (cmd
;;Q	response
	(sprocess (get-ess-process ess-current-process-name)))
    (if (not sprocess) (error "No ESS process running"))
;;Q	(setq response (completing-read "Save workspace image? "
;;Q				    '( ( "yes".1) ("no" . 1) ("cancel" . 1))
;;Q				    nil t))
;;Q	(if (string-equal response "")
;;Q	(setq response "default")); which will ask again (in most situations)
;;Q	(unless (string-equal response "cancel")
      (ess-cleanup)
;;Q   (setq cmd (format "q(\"%s\")\n" response))
      (setq cmd "q()\n")
      (goto-char (marker-position (process-mark sprocess)))
      (process-send-string sprocess cmd)
      ;;(rename-buffer (concat (buffer-name) "-exited") t)
;;Q	 )
  ))

(defun ess-abort ()
  "Kill the ESS process, without executing .Last or terminating devices.
If you want to finish your session, use \\[ess-quit] instead."
;;; Provided as a safety measure over the default binding of C-c C-z in
;;; comint-mode-map.
  (interactive)
  (ding)
  (message "WARNING: \\[inferior-ess-exit-command] will not be executed and graphics devices won't finish properly!")
  (sit-for 2)
  (if (yes-or-no-p "Still abort? ")
      (comint-quit-subjob)
    (message "Good move.")))

(defun ess-cleanup ()
  "Possibly kill or offer to kill, depending on the value of
`ess-S-quit-kill-buffers-p', all buffers associated with this ESS process.
Leaves you in the ESS process buffer.  It's a good idea to run this
before you quit.  It is run automatically by \\[ess-quit]."
  (interactive)
  (let ((the-procname (or (ess-make-buffer-current) ess-local-process-name)))
    (if the-procname nil
      (error "I don't know which ESS process to clean up after!"))
    (if
	(or (eq ess-S-quit-kill-buffers-p t)
	    (and
	     (eq ess-S-quit-kill-buffers-p 'ask)
	     (y-or-n-p
	      (format
	       "Delete all buffers associated with process %s? " the-procname))))
	(save-excursion
	  (mapc '(lambda (buf)
		   (set-buffer buf)
		   ;; Consider buffers for which
		   ;; ess-local-process-name is the same as
		   ;; the-procname
		   (if (and (not (get-buffer-process buf))
			    ess-local-process-name
			    (equal ess-local-process-name the-procname))
		       (kill-buffer buf)))
		(buffer-list))))
    (ess-switch-to-ESS nil)))

(defun ess-kill-buffer-function nil
  "Function run just before an ESS process buffer is killed."
  ;; This simply deletes the buffers process to avoid an Emacs bug
  ;; where the sentinel is run *after* the buffer is deleted
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc (delete-process proc))))

;;*;; Object name completion

;;;*;;; The user completion command
(defun ess-complete-object-name (&optional listcomp)
  "Perform completion on `ess-language' object preceding point.
Uses \\[ess-R-complete-object-name] when `ess-use-R-completion' is non-nil,
or \\[ess-internal-complete-object-name] otherwise."
  (interactive "P");; FIXME : the `listcomp' argument is NOT used
  (if ess-use-R-completion
      (ess-R-complete-object-name)
    (ess-internal-complete-object-name listcomp)))

(defun ess-internal-complete-object-name (&optional listcomp)
  "Perform completion on `ess-language' object preceding point.
The object is compared against those objects known by
`ess-get-object-list' and any additional characters up to ambiguity are
inserted.  Completion only works on globally-known objects (including
elements of attached data frames), and thus is most suitable for
interactive command-line entry, and not so much for function editing
since local objects (e.g. argument names) aren't known.

Use \\[ess-resynch] to re-read the names of the attached directories.
This is done automatically (and transparently) if a directory is
modified (S only!), so the most up-to-date list of object names is always
available.  However attached dataframes are *not* updated, so this
command may be necessary if you modify an attached dataframe.

If ARG is non-nil, no completion is attempted, but the available
completions are listed [__UNIMPLEMENTED__]."
  (interactive "P");; FIXME : the `listcomp' argument is NOT used
  (ess-make-buffer-current)
  (if (memq (char-syntax (preceding-char)) '(?w ?_))
      (let* ((comint-completion-addsuffix nil)
	     (end (point))
	     (buffer-syntax (syntax-table))
	     (beg (unwind-protect
		      (save-excursion
			(set-syntax-table ess-mode-syntax-table)
			(backward-sexp 1)
			(point))
		    (set-syntax-table buffer-syntax)))
	     (full-prefix (buffer-substring beg end))
	     (pattern full-prefix)
	     ;; See if we're indexing a list with `$'
	     (listname (if (string-match "\\(.+\\)\\$\\(\\(\\sw\\|\\s_\\)*\\)$"
					 full-prefix)
			   (progn
			     (setq pattern
				   (if (not (match-beginning 2)) ""
				     (substring full-prefix
						(match-beginning 2)
						(match-end 2))))
			     (substring full-prefix (match-beginning 1)
					(match-end 1)))))
	     ;; are we trying to get a slot via `@' ?
	     (classname (if (string-match "\\(.+\\)@\\(\\(\\sw\\|\\s_\\)*\\)$"
					 full-prefix)
			   (progn
			     (setq pattern
				   (if (not (match-beginning 2)) ""
				     (substring full-prefix
						(match-beginning 2)
						(match-end 2))))
			     (ess-write-to-dribble-buffer
			      (format "(ess-C-O-Name : slots..) : patt=%s"
				      pattern))
			     (substring full-prefix (match-beginning 1)
					(match-end 1)))))
	     (components (if listname
			     (ess-object-names listname)
			   (if classname
			       (ess-slot-names classname)
			     ;; Default case: It hangs here when
			     ;;    options(error=recover) :
			     (ess-get-object-list ess-current-process-name)))))
	;; always return a non-nil value to prevent history expansions
	(or (comint-dynamic-simple-complete  pattern components) 'none))))

(defun ess-list-object-completions nil
  "List all possible completions of the object name at point."
  (interactive)
  (ess-complete-object-name t));; FIXME: NOT WORKING since argument is unused!

;;;*;;; Support functions
(defun ess-extract-onames-from-alist (alist posn &optional force)
  "Return the object names in position POSN of ALIST.
ALIST is an alist like `ess-sl-modtime-alist'. POSN should be in 1 .. (length
ALIST).	 If optional third arg FORCE is t, the corresponding element
of the search list is re-read. Otherwise it is only re-read if it's a
directory and has been modified since it was last read."
  (let* ((entry (nth (1- posn) alist))
	 (dir (car entry))
	 (timestamp (car (cdr entry)))
	 (new-modtime (ess-dir-modtime dir)))
    ;; Refresh the object listing if necessary
    (if (or force (not (equal new-modtime timestamp)))
	(setcdr (cdr entry) (ess-object-names dir posn)))
    (cdr (cdr entry))))

(defun ess-dir-modtime (dir)
  "Return the last modtime if DIR is a directory, and nil otherwise."
  (and ess-filenames-map
       (file-directory-p dir)
       (nth 5 (file-attributes dir))))

(defun ess-object-modtime (object)
  "Return the modtime of the S object OBJECT (a string).
Searches along the search list for a file named OBJECT and returns its modtime
Returns nil if that file cannot be found, i.e., for R or any non-S language!"
  (let ((path (ess-search-list))
	result)
    (while (and (not result) path)
      (setq result (file-attributes
		    (concat (file-name-as-directory (car path))
			    object)))
      (setq path (cdr path)))
    (nth 5 result)))

(defun ess-modtime-gt (mod1 mod2)
  "Return t if MOD1 is later than MOD2."
  (and mod1
       (or (> (car mod1) (car mod2))
	   (and (= (car mod1) (car mod2))
		(> (car (cdr mod1)) (car (cdr mod2)))))))

(defun ess-get-object-list (name)
  "Return a list of current S object names associated with process NAME,
using `ess-object-list' if that is non-nil."
  (or ess-object-list ;; <<-  MM: this is now always(?) nil; we cache the *-modtime-alist
      (save-excursion
	(set-buffer (process-buffer (get-ess-process name)))
	(ess-make-buffer-current)
	(ess-write-to-dribble-buffer (format "(get-object-list %s) .." name))
	(if (or (not ess-sl-modtime-alist) ess-sp-change)
	    (progn (ess-write-to-dribble-buffer "--> (ess-get-modtime-list)\n")
		   (ess-get-modtime-list))
	  ;;else
	  (ess-write-to-dribble-buffer " using existing ess-sl-modtime-alist\n")
	  )
	(let* ((alist ess-sl-modtime-alist)
	       (i 2)
	       (n (length alist))
	       result)
	  (ess-write-to-dribble-buffer (format " (length alist) : %d\n" n))
	  ;; Always force a re-read of position 1 :
	  (setq result (ess-extract-onames-from-alist alist 1 'force))
	  (ess-write-to-dribble-buffer
	   (format " have re-read pos=1: -> length %d\n" (length result)))
	  ;; Re-read remaining directories if necessary.
	  (while (<= i n)
	    (setq result
		  (append result
			  (ess-extract-onames-from-alist alist i)))
	    (setq i (1+ i)))
	  (setq ess-object-list (ess-uniq-list result))))))

(defun ess-get-words-from-vector (command &optional no-prompt-check)
  "Evaluate the S command COMMAND, which returns a character vector.
Return the elements of the result of COMMAND as an alist of strings.
COMMAND should have a terminating newline."
  (let ((tbuffer (get-buffer-create
		  " *ess-get-words*")); initial space: disable-undo
	words)
    (save-excursion
      (set-buffer tbuffer)
      (ess-if-verbose-write (format "ess-get-words*(%s).. " command))
      (ess-command command tbuffer 'sleep no-prompt-check)
      (ess-if-verbose-write " [ok] ..")
      (goto-char (point-min))
      (if (not (looking-at "\\s-*\\[1\\]"))
	  (progn (ess-if-verbose-write "not seeing \"[1]\".. ")
		 (setq words nil)
	  )
	(goto-char (point-max))
	(while (re-search-backward "\"\\([^\"]*\\)\"" nil t)
	  (setq words (cons (buffer-substring (match-beginning 1)
					      (match-end 1)) words))))
      ;;DBG, do *not* (i.e., comment):
      (kill-buffer tbuffer)
      )
    (ess-if-verbose-write
	 (if (> (length words) 5)
	     (format " |-> (length words)= %d\n" (length words))
	   (format " |-> words= '%s'\n" words)))
    words))

(defun ess-compiled-dir (dir)
  "Return non-nil if DIR is an S object directory with special files.
I.e. if the filenames in DIR are not representative of the objects in DIR."
  (or (file-exists-p (concat (file-name-as-directory dir) "___nonfile"))
      (file-exists-p (concat (file-name-as-directory dir) "__BIGIN"))
      (file-exists-p (concat (file-name-as-directory dir) "___NONFI"))))

(defun ess-object-names (obj &optional pos)
  "Return alist of S object names in directory (or object) OBJ.
If OBJ is a directory name (begins with `/') returns a listing of that dir.
   This may use the search list position POS if necessary.
If OBJ is an object name, returns result of S command names(OBJ).
If OBJ is nil or not a directory, POS must be supplied, and objects(POS) is returned.
In all cases, the value is an list of object names."

;; FIXME: in both cases below, use the same fallback "objects(POS)" -- merge!
  (if (and obj (file-accessible-directory-p obj))
      ;; Check the pre-compiled object list in ess-object-name-db first

      ;; FIXME: If used at all, ess-object-name-db should not only
      ;; -----	be used in the directory case !!
      (or (cdr-safe (assoc obj ess-object-name-db))
	  ;; Take a directory listing
	  (and ess-filenames-map
	       ;; first try .Data subdirectory:
	       ;;FIXME: move ".Data" or ``this function'' to ess-sp6-d.el etc:
	       (let ((dir (concat (file-name-as-directory obj) ".Data")))
		 (if (not (file-accessible-directory-p dir))
		     (setq dir obj))
		 (and (not (ess-compiled-dir dir))
		      (directory-files dir))))
	  ;; Get objects(pos) instead
	  (and (or (ess-write-to-dribble-buffer
		    (format "(ess-object-names ..): directory %s not used\n" obj))
		   t)
	       pos
	       (ess-get-words-from-vector
		(format inferior-ess-objects-command pos))))
	  ;; "else" should really give an error!
	  ;; would need	 pos = which(obj = search())

    ;; else
    (or (and obj  ;; want names(obj)
	     (or (ess-write-to-dribble-buffer
		  (format "(ess-object-names obj=%s): no directory - trying names\n"
			  obj))
		 t)
	     (ess-get-words-from-vector
	      (format inferior-ess-safe-names-command obj)))
	(and nil; must return nil
	     (ess-write-to-dribble-buffer
	      (format "(ess-object-names obj=%s): no dir.; -> objects()\n" obj)))
	;; get objects(pos)
	(ess-get-words-from-vector
	 (format inferior-ess-objects-command pos))))) ; had 2nd arg ".*"
					; s4 needs 2
					; args, rest only need 1 ?
					; changes needed to allow for
					; pattern argument to
					; .SmodeObs

(defun ess-slot-names (obj)
  "Return alist of S4 slot names of S4 object OBJ."
  (ess-get-words-from-vector (format "slotNames(%s)\n" obj)))


;;; SJE: Wed 29 Dec 2004 --- remove this function.
;;; rmh: Wed 5 Jan 2005 --- bring it back for use on Windows
(defun ess-create-object-name-db ()
  "Create a database of object names in standard S directories.	 This
database is saved in the file specified by `ess-object-name-db-file',
and is loaded when `ess-mode' is loaded. It defines the variable
`ess-object-name-db', which is used for completions.

Before you call this function, modify the S search list so that it contains
all the non-changing (i.e. system) S directories.  All positions of the search
list except for position 1 are searched and stored in the database.

After running this command, you should move ess-namedb.el to a directory in
the `load-path'."
  (interactive)
  (setq ess-object-name-db nil)
  (let ((search-list (cdr (ess-search-list)))
	(pos 2)
	name
	(buffer (get-buffer-create " *ess-db*"))
	(temp-object-name-db nil)
	(temp-object-name-db-file ess-object-name-db-file))

    (ess-write-to-dribble-buffer
     (format "(object db): search-list=%s \n " search-list))
    (while search-list
      (message "Searching %s" (car search-list))
      (setq temp-object-name-db (cons (cons (car search-list)
					    (ess-object-names nil pos))
				      temp-object-name-db))
      (setq search-list (cdr search-list))
      (ess-write-to-dribble-buffer
       (format "(object db): temp-obj-name-db=%s \n pos=%s"
	       temp-object-name-db pos))
      (setq pos (1+ pos)))

    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert "(setq ess-object-name-db '")
      (prin1 temp-object-name-db (current-buffer))
      (insert ")\n")
      (setq name (expand-file-name ess-object-name-db-file))
      (write-region (point-min) (point-max) name)
      (message "Wrote %s" name))
    (kill-buffer buffer)
    (setq ess-object-name-db temp-object-name-db)))

(defun ess-resynch nil
  "Reread all directories/objects in variable `ess-search-list' to
form completions."
 (interactive)
 (if (ess-make-buffer-current) nil
   (error "Not an ESS process buffer"))
 (setq ess-sl-modtime-alist nil)
 (setq ess-object-list nil)
 (setq ess-object-name-db nil)		; perhaps it would be better to reload?
 (setq ess-sp-change t)
 (ess-get-modtime-list))


(defun ess-complete-filename ()
  "Do file completion only within strings, or when ! call is being used."
  (if (comint-within-quotes
       (1- (process-mark (get-buffer-process (current-buffer)))) (point))
      ;; (- comint-last-input-start 1) (point))	 <- from S4 modeadds.
      ;; changed on 4/12/96 (dxsun)
      ;; This is sensible, but makes it hard to use history refs
      ;; (or
      ;;  (save-excursion
      ;;    (goto-char comint-last-input-start)
      ;;    (looking-at "\\s-*!"))
      ;;  (comint-within-quotes comint-last-input-start (point)))
      (progn
	;;DBG (ess-write-to-dribble-buffer "ess-complete-f.name: within-quotes")
	(if (featurep 'xemacs) ;; work around Xemacs bug
	    (comint-dynamic-complete-filename)
	  ;; GNU emacs and correctly working Xemacs:
	  ;;(comint-replace-by-expanded-filename))
	  (comint-dynamic-complete-filename))
	;; always return t if in a string
	t)))

(defun ess-after-pathname-p nil
  ;; Heuristic: after partial pathname if it looks like we're in a
  ;; string, and that string looks like a pathname. Not the best for
  ;; use with unix() (or it's alias, !). Oh well.
  (save-excursion
    (save-match-data
      (let ((opoint (point)))
	(and (re-search-backward "\\(\"\\|'\\)[~/#$.a-zA-Z0-9][^ \t\n\"']*"
				 nil t)
	     (eq opoint (match-end 0)))))))

;;*;; Functions handling the search list

(defun ess-search-list ()
  "Return the current search list as a list of strings.
Elements which are apparently directories are expanded to full dirnames.
Is *NOT* used by \\[ess-execute-search],
but by \\[ess-resynch], \\[ess-get-object-list], \\[ess-get-modtime-list],
\\[ess-execute-objects], \\[ess-object-modtime], \\[ess-create-object-name-db],
and (indirectly) by \\[ess-get-help-files-list]."
  (save-excursion
    (let ((result nil))
      (set-buffer (get-ess-buffer ess-current-process-name));to get *its* local vars
      (if (and ess-search-list (not ess-sp-change))
	  ;; use cache:
	  ess-search-list
	;; else, re-compute:
	(ess-write-to-dribble-buffer " (ess-search-list: re-computing..) ")
	(let ((tbuffer (get-buffer-create " *search-list*"))
	      (homedir ess-directory)
	      (my-search-cmd inferior-ess-search-list-command); from ess-buffer
	      elt)
	  (save-excursion
	    (set-buffer tbuffer)
	    ;; guaranteed by the initial space in its name: (buffer-disable-undo)
	    (ess-command my-search-cmd tbuffer 0.2); <- sleep; does (erase-buffer)
	    (goto-char (point-min))
	    (ess-write-to-dribble-buffer
	     (format "after '%s', point-max=%d\n" my-search-cmd (point-max)))
	    (while (re-search-forward "\"\\([^\"]*\\)\"" nil t)
	      (setq elt (buffer-substring (match-beginning 1) (match-end 1)))
	      ;;Dbg: (ess-write-to-dribble-buffer (format "  .. elt= %s \t" elt))
	      (if (and (string-match "^[^/]" elt)
		       (file-directory-p (concat ess-directory elt)))
		  (progn
		    ;;Dbg: (ess-write-to-dribble-buffer "*IS* directory\n")
		    (setq elt (concat homedir elt)))
		;;else
		;;dbg
		;;-		(ess-write-to-dribble-buffer "not dir.\n")
		)
	      (setq result (append result (list elt))))
	    (kill-buffer tbuffer)))
	(setq ess-search-list result)
	(setq ess-sp-change nil)
	result))))

;;; ess-sl-modtime-alist is a list with elements as follows:
;;;  * key	       (directory or object name)
;;;  * modtime	       (list of 2 integers)
;;;  * name, name ...  (accessible objects in search list posn labeled by key)
;;; It is a buffer-local variable (belonging to e.g. *R*, *S+6*, .. etc)
;;; and has the same number of elements and is in the same order as the
;;; S search list

(defun ess-get-modtime-list ()
  "Record the modification times of the directories in the search list,
and the objects in those directories.
The result is stored in `ess-sl-modtime-alist'."
  ;; Operation applies to process of current buffer
  (let* ((searchlist (ess-search-list))
	 (index 1)
	 posn
	 newalist)
    (while searchlist
      (setq posn (car searchlist))
      (setq newalist
	    (append
	     newalist
	     (list (or (assoc posn ess-sl-modtime-alist)
		       (append
			(list posn (ess-dir-modtime posn))
			(prog2
			    (message "Forming completions for %s..." posn)
			    (ess-object-names posn index)
			  (message "Forming completions for %s...done" posn)
			  ))))))
      (setq index (1+ index))
      (setq searchlist (cdr searchlist)))
    ;;DBG:
    (ess-write-to-dribble-buffer
     (format "(ess-get-modtime-list): created new alist of length %d\n"
	     (length newalist)));; todo : also give length of components!

    (setq ess-sl-modtime-alist newalist)))


(defun ess-search-path-tracker (str)
  "Check if input STR changed the search path.
This function monitors user input to the inferior ESS process so that
Emacs can keep the variable `ess-search-list' up to date. `completing-read' in
\\[ess-read-object-name] uses this list indirectly when it prompts for help or
for an object to dump."
  (if (string-match ess-change-sp-regexp str)
      (setq ess-sp-change t)))

 ; Miscellaneous routines

;;;*;;; Routines for reading object names

(defun ess-read-object-name (p-string)
  "Read an S object name from the minibuffer with completion, and return it.
P-STRING is the prompt string."
  (let* ((default (ess-read-object-name-dump))
	 (prompt-string (if default
			    (format "%s(default %s) " p-string default)
			  p-string))
	 (object-list (mapcar 'list (ess-get-object-list ess-local-process-name)))
	 (spec (completing-read prompt-string object-list)))
    (list (cond
	   ((string= spec "") default)
	   (t spec)))))

(defun ess-read-object-name-default ()
  "Return the object name at point, or nil if none."
  (condition-case ()
      (save-excursion
	;; The following line circumvents an 18.57 bug in following-char
	(if (eobp) (backward-char 1)) ; Hopefully buffer is not empty!
	;; Get onto a symbol
	(catch 'nosym ; bail out if there's no symbol at all before point
	  (while (/= (char-syntax (following-char)) ?w)
	    (if (bobp) (throw 'nosym nil) (backward-char 1)))
	  (let*
	      ((end (progn (forward-sexp 1) (point)))
	       (beg (progn (backward-sexp 1) (point))))
	    (buffer-substring beg end))))
    (error nil)))

(defun ess-read-object-name-dump ()
  "Return the object name at point, or \"Temporary\" if none."
  (condition-case ()
      (save-excursion
	;; The following line circumvents an 18.57 bug in following-char
	(if (eobp) (backward-char 1)) ; Hopefully buffer is not empty!
	;; Get onto a symbol
	(catch 'nosym ; bail out if there's no symbol at all before point
	  (while (/= (char-syntax (following-char)) ?w)
	    (if (bobp) (throw 'nosym nil) (backward-char 1)))
	  (let*
	      ((end (progn (forward-sexp 1) (point)))
	       (beg (progn (backward-sexp 1) (point)))
	       (object-name (buffer-substring beg end)))
	    (or object-name "Temporary"))))
    (error nil)))


;;*;; Temporary buffer handling

;(defun ess-create-temp-buffer (name)
;  "Create an empty buffer called NAME, but doesn't display it."
;  (let ((buff (get-buffer-create name)))
;    (save-excursion
;      (set-buffer buff)
;      (erase-buffer))
;    buff))


;; Ed Kademan's version:
;From: Ed Kademan <kademan@phz.com>
;Subject: Re: ess-mode 5.1.16; search list
;To: rossini@biostat.washington.edu (A.J. Rossini)
;Cc: Martin Maechler <maechler@stat.math.ethz.ch>, ess-bugs@stat.math.ethz.ch
;Date: 26 Jul 2000 16:12:12 -0400
;
;Dear Tony Rossini,
;
;I was having trouble looking at the search list under ess.  When I
;started up multiple inferior processes---each for a different
;dialect---ess-mode would issue the wrong variant of the "search"
;command when I typed C-c C-s.	In case it is useful let me tell you
;what I did to get it to work for me.
;
;I added the component:
;  (inferior-ess-search-list-command . "search()\n")
;to S+3-customize-alist and R-customize-alist, and then I redefined the
;ess-create-temp-buffer function as follows:
(defun ess-create-temp-buffer (name)
  "Create an empty buffer called NAME."
  (let ((buff (get-buffer-create name))
	(elca (eval ess-local-customize-alist)))
    (save-excursion
      (set-buffer buff)
      (erase-buffer)
      (ess-setq-vars-local elca buff))
    buff))
;;These two steps seem to insure that the temporary buffer in which the
;;search results appear has the correct version of the local variables.
;;I am not that well acquainted with the ess code and don't know whether
;;this is a good fundamental way of fixing the problem, or even whether
;;or not this breaks some other feature of ess-mode that I never use.
;;Thanks for listening.
;;Ed K.
;;--
;;Ed Kademan		  508.651.3700
;;PHZ Capital Partners	  508.653.1745 (fax)
;;321 Commonwealth Road	  <kademan@phz.com>
;;Wayland, MA 01778



(defun ess-display-temp-buffer (buff)
  "Display the buffer BUFF using `temp-buffer-show-function' and respecting
`ess-display-buffer-reuse-frames'."
  (let ((display-buffer-reuse-frames ess-display-buffer-reuse-frames))
    (funcall (or temp-buffer-show-function 'display-buffer) buff)))

;;*;; Error messages

(defun ess-error (msg)
  "Something bad has happened.
Display the S buffer, and cause an error displaying MSG."
  (display-buffer (process-buffer (get-ess-process ess-current-process-name)))
  (error msg))

 ; Provide package

(provide 'ess-inf)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:	  ^L ;
;;; Sections:	 ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:	 defuns, defvars, defconsts
;;;		 Random code beginning with a ;;;;* comment

;;; Local variables:
;;; mode: emacs-lisp
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-inf.el ends here
