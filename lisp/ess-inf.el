;;; ess-inf.el --- Support for running S as an inferior Emacs process

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997, A.J. Rossini

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 7 Jan 1994
;; Modified: $Date: 1997/11/26 15:49:57 $
;; Version: $Revision: 4.53 $
;; RCS: $Id: ess-inf.el,v 4.53 1997/11/26 15:49:57 rossini Exp $

;; This file is part of ESS

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

;; Code for dealing with running ESS processes.

;;; Code:

 ; Requires and autoloads

;;*;; Requires
(require 'ess-site)

;; Byte-compiler, SHUT-UP!
(eval-and-compile
  (require 'comint))

;;*;; Autoloads
(autoload 'ess-parse-errors "ess-mode" "(autoload)." t)
(autoload 'ess-dump-object-into-edit-buffer "ess-mode" "(autoload)." t)
(autoload 'ess-end-of-function "ess-mode" "(autoload)." t)
(autoload 'ess-beginning-of-function "ess-mode" "(autoload)." t)
(autoload 'ess-extract-word-name "ess-mode" "(autoload)." t)


(autoload 'ess-transcript-send-command-and-move "ess-trns" "(autoload)." t)

 ;;*;; Variables

;; Moved to ess-vars.el

 ;;*;; Process handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * User commands for starting an ESS process
;;;; * Functions called at startup
;;;; * Process handling code
;;;; * Multiple process implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;*;; Starting a process

(defun ess-proc-name (n name)
  "Return process name of process N, as a string, with NAME
  prepended."
  (if ess-plain-first-buffername
      (if (> n 1)
	  (concat name ":" n)
	(concat name))
    (concat name ":" n)))
  
(defun inferior-ess (&optional ess-start-args)
  "Start inferior ESS process.

Without a prefix argument, starts a new ESS process, or switches
  to the ESS process associated with the current buffer.
With a prefix, starts the process with those args.
The current buffer is used if it is an inferior-ess-mode
or ess-transcript-mode buffer.

If ess-ask-about-transfile is non-nil, you will be asked for a
transcript file to use. If there is no transcript file, the buffer
name will be like *S* or *S2*.

Takes the program name from the variable inferior-ess-program.
An initialization file (dumped into the process) is specified by
`inferior-ess-start-file', and `inferior-ess-start-args' is used to
accompany the call for inferior-ess-program.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  ;; Use the current buffer if it is in inferior-ess-mode or ess-trans-mode
  ;; If not, maybe ask about starting directory and/or transcript file.
  ;; If no transfile, use buffer *S*
  ;;
  ;; This function is primarily used to figure out the Process and
  ;; buffer names to use for inferior-ess.

  (interactive)

  (ess-write-to-dribble-buffer
     (format "(inferior-ess 0): ess-start-args=%s \n"
	     ess-start-args))
  ;; set up for current language (need here, to get ess-language,
  ;; etc).
  (let ((temp-ess-dialect (cdr (rassoc ess-dialect
				       ess-customize-alist)))
	(temp-ess-lang (cdr (rassoc ess-language
				       ess-customize-alist))))
    (save-excursion
      (set-buffer ess-dribble-buffer)
      ;; next line isn't necessary now???
      (ess-setq-vars-default ess-customize-alist (current-buffer))
      (setq temp-ess-dialect (cdr (rassoc ess-dialect ess-customize-alist)))
      (setq temp-ess-lang (cdr (rassoc ess-language ess-customize-alist))))
    
    ;; run hooks now, to overwrite the above!
    (run-hooks 'ess-pre-run-hook)
    (ess-write-to-dribble-buffer
     (format "(inferior-ess 1): ess-lang=%s, ess-dialect=%s, temp-dialect=%s, buf=%s \n"
	     ess-language
	     ess-dialect
	     temp-ess-dialect
	     (current-buffer)))
    (let* ((defdir (directory-file-name (or ess-directory default-directory)))
	   (temp-dialect temp-ess-dialect)
	   (temp-lang temp-ess-lang)
	   (procname ;;(or ;;(and (not (comint-check-proc (current-buffer)))
			      ;; Don't start a new process in current buffer if
			      ;; one is already running
			      ;;ess-local-process-name)
			 ;; find a non-existent process
		      (let ((ntry 0)
			    (done nil))
			(while (not done)
			  (setq ntry (1+ ntry)
				done (not
				      (get-process (ess-proc-name
						    ntry
						    temp-dialect)))))
			(ess-proc-name ntry temp-dialect))) ;)
;;	   (procname-lang (or (and (not (comint-check-proc (current-buffer)))
;;			      ;; Don't start a new process in current buffer if
;;			      ;; one is already running
;;			      ess-local-process-name)
;;			 ;; find a non-existent process
;;			 (let ((ntry 0)
;;			       (done nil))
;;			   (while (not done)
;;			     (setq ntry (1+ ntry)
;;				   done (not
;;					 (get-process (ess-proc-name
;;						       ntry
;;						       temp-lang)))))
;;			   (ess-proc-name ntry temp-lang))))

	   (startdir nil)
	   (buf nil)
	   (buf-name-str  (concat "*" procname "*")))

      (ess-write-to-dribble-buffer
       (format "(inf-ess 1.1): procname=%s temp-dialect=%s, buf-name=%s \n"
	       procname
	       temp-dialect
	       buf-name-str))
      (cond
       ;; If process is running, we use it: 
       ;; AJR- NO!  We no longer use this for switching buffers!
       ;;((get-process procname)
	;; (setq buf (process-buffer (get-process procname))))

       ;; Else (it's a new or terminated process) try to use current buffer
       ((and (not buf)
	     (not (comint-check-proc (current-buffer)))
	     (memq major-mode '(inferior-ess-mode))) ; ess-transcript-mode)))
	(setq startdir
	      (if ess-ask-for-ess-directory (ess-get-directory defdir)
		ess-directory))
	(setq buf (current-buffer)))

       ;;  Not an ESS buffer yet
       ((and (not buf)
	     (get-buffer buf-name-str))
	(setq buf (get-buffer buf-name-str)))

       ;; Ask for transcript file and startdir
       ;; FIXME -- this should be in ess-get-transfile
       ((not buf)
	(setq startdir
	      (if ess-ask-for-ess-directory (ess-get-directory defdir)
		ess-directory))
	(if ess-ask-about-transfile
	    (let ((transfilename (read-file-name
				  "Use transcript file (default none):"
				  startdir "")))
	      ;;(if (string= transfilename "")
	      ;;    (setq transfilename nil)
	      ;;  (setq transfilename (expand-file-name transfilename)))
	      (setq buf (if (string= transfilename "")
			    (get-buffer-create buf-name-str)
			  (find-file-noselect (expand-file-name
					       transfilename)))))
	  (setq buf (get-buffer-create buf-name-str)))))

      (set-buffer buf)
      ;; Now that we have the buffer, set buffer-local variables.
      (ess-setq-vars-local ess-customize-alist buf)
      (if ess-start-args (setq inferior-ess-start-args ess-start-args))
      (ess-write-to-dribble-buffer
       (format "(inf-ess 2.1): ess-language=%s, ess-dialect=%s buf=%s \n"
	       ess-language
	       ess-dialect
	       (current-buffer)))
      (ess-write-to-dribble-buffer
       (format "(inf-ess 2.2): start args = %s, inf-ess-start-args=%s \n"
	       ess-start-args
	       inferior-ess-start-args))
      (if startdir (setq default-directory startdir))
      (setq-default ess-history-file
		    (concat "." ess-dialect "history"))
      (ess-multi procname buf inferior-ess-start-args))))

;; Old code:

;;  ;; If this process is running, switch to it
;;  (if (get-process inferior-ess-procname)
;;      (let ((my-buff-name (process-buffer
;;			  (get-process inferior-ess-procname))))
;;	(message "debug first way")
;;	(ess-multi inferior-ess-procname my-buff-name))
;;    ;; This is a new or terminated process.
;;    ;; If no arg, try to use current buffer
;;    (if (and (not n)
;;	     (not (comint-check-proc (current-buffer)))
;;	     (memq major-mode '(inferior-ess-mode ess-transcript-mode)))
;;	(progn
;;	  (setq default-directory
;;		(if ess-ask-for-ess-directory (ess-get-directory ess-defdir)
;;		  ess-directory))
;;	  (message "debug second way")
;;	  (ess-multi inferior-ess-procname (current-buffer)))
;;      ;; Not an S buffer
;;      (let ((buf-name-str (concat "*" inferior-ess-procname "*")))
;;	(if (get-buffer buf-name-str)
;;	    (progn
;;	      (message "debug third way")
;;	      (ess-multi inferior-ess-procname (get-buffer buf-name-str)))
;;	  ;; Ask for transcript file and startdir
;;	  ;; FIXME -- this should be in ess-get-transfile
;;	  (run-hooks 'S-pre-run-hook);;rmh
;;	  (let* ((startdir (if ess-ask-for-ess-directory
;;			       (ess-get-directory ess-defdir)
;;			     ess-directory))
;;		 (transfilename nil)
;;		 (buf nil)
;;		 (procname inferior-ess-procname))
;;	    (if ess-ask-about-transfile
;;		(progn
;;		  (setq transfilename
;;			(read-file-name "Use transcript file (default none):"
;;					startdir ""))
;;		  (if (string= transfilename "")
;;		      (setq transfilename nil)
;;		    (setq transfilename (expand-file-name transfilename)))))
;;	    (setq buf (if transfilename (find-file-noselect transfilename)
;;			(get-buffer-create buf-name-str)))
;;	    (set-buffer buf)
;;	    (setq inferior-ess-procname procname)
;;	    (setq default-directory startdir)
;;	    (message "debug fourth way %s" inferior-ess-procname)
;;	    (message "debug fourth way %s" procname)
;;	    (ess-multi inferior-ess-procname buf))))))) ;)for let.

;;; A note on multiple processes: the following variables
;;;     ess-local-process-name
;;;     ess-search-list
;;;     ess-sl-modtime-alist
;;;     ess-sp-change
;;;     ess-prev-load-dir/file
;;;     ess-directory
;;;     ess-object-list
;;; are specific to each ess-process and are buffer-local variables
;;; local to the ESS process buffer. If required, these variables should
;;; be accessed with the function ess-get-process-variable

(defun ess-multi (name &optional buffer inf-ess-start-args)
  "Start or switch to ESS process named NAME in the buffer BUFFER.
BUFFER is only needed if process NAME is not running. BUFFER must
exist.  Default-directory is the ESS starting directory. BUFFER may be
visiting a file."

;; If ess-process NAME is running, switch to it.  If not, use COMINT
;; to start up a new process, using NAME and BUFFER (which is needed
;; if there is no process NAME).

  ;; (if (< n 1) (error "Argument to ess-multi must be 1 or greater"))
  (let* ((proc-name name)
	 (proc (get-process proc-name)))
    ;; If ESS process NAME is running, switch to it
    (if (and proc (comint-check-proc (process-buffer proc)))
	(switch-to-buffer (process-buffer proc))
      ;; Otherwise, crank up a new process
      (let* ((symbol-string
	      (concat "inferior-" inferior-ess-program "-args"))
	     (switches-symbol (intern-soft symbol-string))
	     (switches
	      (if (and switches-symbol (boundp switches-symbol))
		  (symbol-value switches-symbol)))
	     (buf-name-str (buffer-name buffer)))
	(set-buffer buffer)
	(setq-default inferior-ess-prompt 	; shouldn't be
						; setq-default!
		      ;; Do not anchor to bol with `^'
		      (concat "\\("
			      inferior-ess-primary-prompt
			      "\\|"
			      inferior-ess-secondary-prompt
			      "\\)"))
	(inferior-ess-mode)
	(setq ess-local-process-name proc-name)
	(goto-char (point-max))
	(setq comint-input-ring-file-name
	      (expand-file-name ess-history-file ess-directory))
	(comint-read-input-ring)
	;;(run-hooks 'ess-pre-run-hook) ONLY IN 'inferior-ess'
	(ess-write-to-dribble-buffer
	 (format "(ess-multi 1):  inf-ess-start-args=%s \n"
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
	;; Add this process to ess-process-name-list, if need be
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
	(run-hooks 'ess-post-run-hook))
      (switch-to-buffer (process-buffer (get-process proc-name))))))

(defun ess-process-sentinel (proc message)
  "Sentinel for use with ESS processes."
  (save-excursion
    (setq message (substring message 0 -1)) ; strip newline
    (set-buffer (process-buffer proc))
    (comint-write-input-ring)
    (goto-char (point-max))
    (insert-before-markers
     (format "\nProcess %s %s at %s\n"
	     (process-name proc) message (current-time-string)))))

;; Original version
;;(defun inferior-ess-make-comint (bufname procname &rest switches)
;;  "Make an S comint process in buffer NAME with process called PROC."
;;;;; This function is a modification of make-comint from the comint.el
;;;;; code of Olin Shivers.
;;  (let*  ((buffer (get-buffer-create bufname))
;;	  (proc (get-process procname)))
;;    ;; If no process, or nuked process, crank up a new one and put buffer in
;;    ;; comint mode. Otherwise, leave buffer and existing process alone.
;;    (cond ((or (not proc) (not (memq (process-status proc) '(run stop))))
;;           (save-excursion
;;             (set-buffer buffer)
;;             (setq default-directory ess-directory)
;;	     (if (eq (buffer-size) 0) nil
;;	       (goto-char (point-max))
;;	       (insert "\^L\n"))	; page boundaries are S sessions
;;             )
;;           (comint-exec buffer procname inferior-ess-program nil switches)))
;;    buffer))

;;KH's version, 6Apr96
(defun inferior-ess-make-comint (bufname
				 procname
				 inferior-ess-start-args
				 &rest switches)
  "Make an S comint process in buffer NAME with process called PROC."
;;; This function is a modification of make-comint from the comint.el
;;; code of Olin Shivers.
  (let*  ((buffer (get-buffer-create bufname))
	  (proc (get-process procname)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode. Otherwise, leave buffer and existing process alone.
    (cond ((or (not proc) (not (memq (process-status proc) '(run stop))))
           (save-excursion
             (set-buffer buffer)
             (if ess-directory (setq default-directory ess-directory))
	     (if (eq (buffer-size) 0) nil
	       (goto-char (point-max))
	       (insert "\^L\n")))	; page boundaries are S sessions
	   (let ((process-environment
                  (nconc
                   (list
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
  "Request and return S starting directory"
  (let ((the-dir
	 (expand-file-name
	  (file-name-as-directory
	   (read-file-name
	    "ESS starting data directory? " (file-name-as-directory default) (file-name-as-directory default) t nil)))))
    (if (file-directory-p the-dir) nil
      (error "%s is not a valid directory" the-dir))
    the-dir))

;;*;; General process handling code

(defun get-ess-process (name)
  "Return the ESS process named by NAME."
  (update-ess-process-name-list)
  (if (null name)
      (error "No ESS process is running now.")
    (if (assoc name ess-process-name-list)
	(get-process name)
      (error "Process %s is not running." name))))

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
	       (beginning-of-line)
	       (setq r (looking-at inferior-ess-prompt))
	       (not (or r (looking-at ".*\\?\\s *"))))))
    (goto-char (point-max))
    (set-buffer cbuffer)
    (symbol-value r)))

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
  ;;(let (proc (get-ess-process name))
    (save-excursion
      (set-buffer (process-buffer (get-ess-process name)))
      (symbol-value var))
    ;;)
    )

(defun ess-set-process-variable (name var val)
  "Set the variable VAR (symbol) local to ESS process called NAME (string) to VAL."
  ;;(let* (proc (get-ess-process name))
  (save-excursion
    (set-buffer (process-buffer (get-ess-process name)))
    (set var val))
  ;)
  )

(defun ess-request-a-process (message &optional noswitch)
  "Ask for a process, and make it the current ESS process.
Also switches to the process buffer, if second arg NOSWITCH (prefix) is non-nil.
Returns the name of the selected process."
  (interactive
   (list "Switch to which ESS process? " current-prefix-arg)) ;prefix sets 'noswitch
  (update-ess-process-name-list)
  (if (eq (length ess-process-name-list) 0)
      (error "No ESS processes running."))
  (let ((proc (completing-read message
			       ess-process-name-list
			       nil ; predicate
			       'require-match
			       ;; If in S buffer, don't offer current process
			       (if (eq major-mode 'inferior-ess-mode)
				   ess-language
				 ess-current-process-name
				 ;; maybe rather  ess-local-process-name IF exists
				 ))))
    (save-excursion
      (set-buffer (process-buffer (get-process proc)))
      (ess-make-buffer-current))
    (if noswitch nil (switch-to-buffer (process-buffer (get-process proc))))
    proc))

;;(defun ess-switch-local-process (message)
;; "Ask for a process, and make it the current ESS process.
;;Also switches to the process buffer, if second arg NOSWITCH (prefix) is non-nil.
;;Returns the name of the selected process."
;;  (interactive
;;   (list "Switch to which ESS process? ")) ;prefix sets 'noswitch
;;  (update-ess-process-name-list)
;;  (if (eq (length ess-process-name-list) 0)
;;      (error "No ESS processes running."))
;;  (let ((proc (completing-read message
;;			       ess-process-name-list
;;			       nil ; predicate
;;			       'require-match
;;			       ;; If in S buffer, don't offer current process
;;			       ;; maybe rather
;;			       ;; ess-local-process-name IF exists
;;			       (if (eq major-mode 'inferior-ess-mode)
;;				   ess-language
;;				 ess-current-process-name))))
;;    (setq ess-local-process-name proc)))

(defun ess-force-buffer-current (prompt &optional force)
  "Make sure the current buffer is attached to an ESS process.
If not, or  FORCE (prefix argument) is non-nil,
prompt for a process name with PROMPT.
ess-local-process-name is set to the name of the process selected."
  (interactive
   (list (concat ess-dialect " process to use: ") current-prefix-arg))
  (if (and (not force) (ess-make-buffer-current))
      nil ; do nothing
    ;; Make sure the source buffer is attached to a process
    (if (and ess-local-process-name (not force))
	(error "Process %s has died." ess-local-process-name)
      ;; ess-local-process-name is nil -- which process to attach to
      (save-excursion
	(let ((proc (ess-request-a-process prompt 'no-switch)))
	  (setq ess-local-process-name proc))))))

(defun ess-switch-process ()
  "Force a switch to a new underlying process."
  (interactive)
  (ess-force-buffer-current "Process to use: " t))

;;*;;; Commands for switching to the process buffer

(defun ess-switch-to-ESS (eob-p)
  "Switch to the current inferior ESS process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (ess-make-buffer-current)
  (if (and ess-current-process-name (get-process ess-current-process-name))
      (progn
	(pop-to-buffer
	 (process-buffer (get-process ess-current-process-name)))
	(if eob-p (goto-char (point-max))))
    (message "No inferior ESS process")
    (ding)))

(defun ess-switch-to-end-of-ESS nil
  "Switch to the end of the inferior ESS process buffer."
  (interactive)
  (ess-switch-to-ESS t))


(defun get-ess-buffer (name)
  "Return the buffer associated with the ESS process named by NAME"
  (process-buffer (get-ess-process name)))

(defun update-ess-process-name-list ()
  "Remove names with no process."
  (let (defunct)
    (mapcar
     '(lambda (conselt)
	(let ((proc (get-process (car conselt))))
	  (if (and proc (eq (process-status proc) 'run)) nil
	    (setq defunct (cons conselt defunct)))))
     ess-process-name-list)
    (mapcar
     '(lambda (pointer)
	(setq ess-process-name-list (delq pointer ess-process-name-list)))
     defunct))
  (if (eq (length ess-process-name-list) 0)
      (setq ess-current-process-name nil)))

 ; Functions for evaluating code

(defun ess-prompt-wait (proc &optional start-of-output)
  "Wait for a prompt to appear at BOL of current buffer
PROC is the ESS process. Does not change point"
  (if start-of-output nil (setq start-of-output (point-min)))
  (save-excursion
    (while (progn
	     ;; get output if there is some ready
	     (accept-process-output proc 0 500)
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

;;; FIXME: the third arg (visible) of ess-command is used nowhere - get
;;; rid of it. The process filter should no run for ess-command --
;;; comint Doesn't Need to Know
(defun ess-command (com &optional buf obsolete)
  "Send the ESS process command COM and delete the output
from the ESS process buffer.  If an optional second argument BUF exists
save the output in that buffer. BUF is erased before use.
COM should have a terminating newline.
Guarantees that the value of .Last.value will be preserved."
  ;; Use this function when you need to evaluate so S code, and the
  ;; result is needed immediately. Waits until the output is ready
  (let* ((sprocess (get-ess-process ess-current-process-name))
         sbuffer
	 end-of-output
	 oldpb
	 oldpf
	 oldpm)
    ;; FIXME: replace calls to ess-command with 2-arg form
    (if obsolete (ess-error "Invalid call to ess-command"))
    (if sprocess nil
      (error "Process %s is not running!" ess-current-process-name))
    (setq sbuffer (process-buffer sprocess))
    (save-excursion
      (set-buffer sbuffer)
      (save-excursion
	(goto-char (marker-position (process-mark sprocess)))
	(beginning-of-line)
	(if (looking-at inferior-ess-primary-prompt) nil
	  (ess-error
	   "ESS process not ready. Finish your command before trying again.")))
      (if buf nil (setq buf (get-buffer-create " *ess-command-output*")))
      (setq oldpf (process-filter sprocess))
      (setq oldpb (process-buffer sprocess))
      (setq oldpm (marker-position (process-mark sprocess)))
      (unwind-protect
	  (progn
	    (set-process-buffer sprocess buf)
	    (set-process-filter sprocess 'ordinary-insertion-filter)
	    ;; Output is now going to BUF
	    (save-excursion
	      (set-buffer buf)
	      (erase-buffer)
	      (set-marker (process-mark sprocess) (point-min))
	      (process-send-string sprocess ess-save-lastvalue-command)
	      (ess-prompt-wait sprocess)
	      (erase-buffer)
	      (process-send-string sprocess com)
	      (ess-prompt-wait sprocess)
	      (goto-char (point-max))
	      (save-excursion
		(beginning-of-line)	; so prompt will be deleted
		(setq end-of-output (point)))
	      (process-send-string sprocess ess-retr-lastvalue-command)
	      (ess-prompt-wait sprocess (point))
	      ;; Get rid out output from last assin
	      (delete-region end-of-output (point-max))))
	;; Restore old values for process filter
	(set-process-buffer sprocess oldpb)
	(set-process-filter sprocess oldpf)
	(set-marker (process-mark sprocess) oldpm)))))

(defun ess-replace-in-string (str regexp newtext &optional literal)
  "Replaces all matches in STR for REGEXP with NEWTEXT string.
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


;;; RDB 28/8/92 added optional arg eob
;;; AJR 971022:  text-withtabs was text.
(defun ess-eval-visibly (text-withtabs &optional invisibly eob)
  "Evaluate TEXT-WITHTABS in the ESS process buffer as if typed in w/o tabs.
If optional second arg INVISIBLY is non-nil, don't echo commands. If
if is a string, just include that string. If optional third arg
eob is non-nil go to end of S buffer after evaluation.
Waits for prompt after each line of input, so won't break on large texts."
  ;; Use this to evaluate some code, but don't wait for output.
  (let* ((cbuffer (current-buffer))
         (sprocess (get-ess-process ess-current-process-name))
         (sbuffer (process-buffer sprocess))
	 (text (ess-replace-in-string text-withtabs "\t" " ")) ; AJR 971022
	 start-of-output
	 com pos)
    ;;(message "'ess-eval-visibly: sbuffer = %s" sbuffer)
    (set-buffer sbuffer)
    (goto-char (marker-position (process-mark sprocess)))
    (if (stringp invisibly)
	(insert-before-markers (concat "*** " invisibly " ***\n")))
    (while (> (length text) 0)
      (setq pos (string-match "\n\\|$" text))
      (setq com (concat (substring text 0 pos) "\n"))
      (setq text (substring text (min (length text) (1+ pos))))
      (goto-char (marker-position (process-mark sprocess)))
      (if invisibly nil
	;; Terrible kludge -- need to insert after all markers *except*`
	;; the process mark
	(let ((dokludge (eq (point)
			    (marker-position (process-mark sprocess)))))
	  (insert com)
	  ;; Is this next line REALLY NEEDED?   (AJR).
	  (setq comint-last-input-end (point-marker))
	  (if dokludge (set-marker (process-mark sprocess) (point)))))
      (setq start-of-output (marker-position (process-mark sprocess)))
      ;; A kludge to prevent the delay between insert and process output
      ;; affecting the display.  A case for a comint-send-input-hook?
      ;; (save-excursion
      ;;   ;; comint-postoutput-scroll-to-bottom can change
      ;;   ;; current-buffer. Argh.
      ;;   (let ((functions comint-output-filter-functions))
      ;;     (while functions
      ;;       (funcall (car functions) com)
      ;;       (setq functions (cdr functions)))))
      (process-send-string sprocess com)
      ;; Don't hang around waiting for the prompt after the last
      ;; line of input
      (if (eq (length text) 0) nil
	  (while (progn
		   (accept-process-output nil 0 100)
		   (goto-char (marker-position (process-mark sprocess)))
		   (beginning-of-line)
		   (if (< (point) start-of-output)
		       (goto-char start-of-output))
		   (not (looking-at inferior-ess-prompt))))))
    (goto-char (marker-position (process-mark sprocess)))
    (if eob
	(progn
	  (set-buffer cbuffer)
	  (switch-to-buffer-other-window sbuffer)
	  (goto-char (point-max))
	  (switch-to-buffer-other-window cbuffer))
      (set-buffer cbuffer))))

;;*;;  Evaluating lines, paragraphs, regions, and buffers.

;;;*;;; Evaluate only



;; This is from  Mary Lindstrom <lindstro@Biostat.Wisc.Edu>
;; 31 Aug 1995 14:11:43		To: ess-mode@stat.math.ethz.ch
(defun ess-eval-paragraph (vis-toggle)
  "Send the current paragraph to the inferior ESS process.
Prefix arg. VIS-TOGGLE  toggle visibility of ess-code  as for ess-eval-region."
  (interactive "P")
  (ess-force-buffer-current "Process to load into: ")
  (save-excursion
    (forward-paragraph)
    (let ((end (point)))
      (backward-paragraph)
      (ess-eval-region (point) end
		       vis-toggle "Eval paragraph"))))



(defun ess-eval-region (start end toggle &optional message)
  "Send the current region to the inferior ESS process.
With prefix argument, toggle meaning of ess-eval-visibly-p."
  (interactive "r\nP")
  ;;(untabify (point-min) (point-max))
  ;;(untabify start end); do we really need to save-excursion?
  (ess-force-buffer-current "Process to load into: ")
  (message "Starting evaluation...")
  (let ((visibly (if toggle (not ess-eval-visibly-p) ess-eval-visibly-p)))
    (if visibly
 	(ess-eval-visibly (buffer-substring start end))
      (if ess-synchronize-evals
	  (ess-eval-visibly (buffer-substring start end)
			  (or message "Eval region"))
	(process-send-region (get-ess-process ess-current-process-name)
			     start end)
	(process-send-string (get-ess-process ess-current-process-name)
			     "\n"))))
  (message "Finished evaluation"))

(defun ess-eval-buffer (vis)
  "Send the current buffer to the inferior ESS process.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-force-buffer-current "Process to load into: ")
  (ess-eval-region (point-min) (point-max) vis "Eval buffer"))

(defun ess-eval-function (vis)
  "Send the current function to the inferior ESS process.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-force-buffer-current "Process to load into: ")
  (save-excursion
    (ess-end-of-function)
    (let ((end (point)))
      (ess-beginning-of-function)
      (princ (concat "Loading: " (ess-extract-word-name)) t)
      (ess-eval-region (point) end vis
		     (concat "Eval function " (ess-extract-word-name))))))

(defun ess-eval-line (vis)
  "Send the current line to the inferior ESS process.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-force-buffer-current "Process to load into: ")
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (princ (concat "Loading line: " (ess-extract-word-name) " ...") t)
      (ess-eval-region (point) end vis "Eval line"))))

(defun ess-eval-line-and-next-line ()
  "Evaluate the current line visibly and move to the next line."
  ;; From an idea by Rod Ball (rod@marcam.dsir.govt.nz)
  (interactive)
  (ess-force-buffer-current "Process to load into: ")
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      ;; RDB modified to go to end of S buffer so user can see result
      (ess-eval-visibly (buffer-substring (point) end) nil t)))
  (next-line 1))

;; goes to the real front, in case you do double function definition
;; 29-Jul-92 -FER
;; don't know why David changed it.

;; FER's versions don't work properly with nested functions. Replaced
;; mine. DMS 16 Nov 92

;;;*;;; Evaluate and switch to S

(defun ess-eval-region-and-go (start end vis)
  "Send the current region to the inferior S and switch to the process buffer.
Arg has same meaning as for ess-eval-region."
  (interactive "r\nP")
  (ess-eval-region start end vis)
  (ess-switch-to-ESS t))

(defun ess-eval-buffer-and-go (vis)
  "Send the current buffer to the inferior S and switch to the process buffer.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-eval-buffer vis)
  (ess-switch-to-ESS t))

(defun ess-eval-function-and-go (vis)
  "Send the current function to the inferior ESS process and switch to
the process buffer. Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-eval-function vis)
  (ess-switch-to-ESS t))

(defun ess-eval-line-and-go (vis)
  "Send the current line to the inferior ESS process and switch to the
process buffer. Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-eval-line vis)
  (ess-switch-to-ESS t))

;;; Related to the ess-eval-* commands, there are the ess-load
;;; commands.   Need to add appropriate stuff...


(defun ess-load-file (filename)
  "Load an S source file into an inferior ESS process."
  (interactive (list
		(or
		 (and (eq major-mode 'ess-mode)
		      (buffer-file-name))
		 (expand-file-name
		  (read-file-name "Load S file: " nil nil t)))))
  (require 'ess-inf)
  (ess-make-buffer-current)
  (let ((source-buffer (get-file-buffer filename)))
    (if (ess-check-source filename)
	(error "Buffer %s has not been saved" (buffer-name source-buffer))
      ;; Find the process to load into
      (if source-buffer
	  (save-excursion
	    (set-buffer source-buffer)
    (ess-force-buffer-current "Process to load into: ")
	    (ess-check-modifications))))
    (let ((errbuffer (ess-create-temp-buffer ess-error-buffer-name))
	  error-occurred nomessage)
      (ess-command (format inferior-ess-load-command filename) errbuffer)
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
	(ess-switch-to-ESS t)))))



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

  (cond ((string-match "XEmacs\\|Lucid" emacs-version)
	 ;; Code for XEmacs
 	 (setq inferior-ess-mode-map (make-keymap))
	 (set-keymap-parent inferior-ess-mode-map comint-mode-map))
	((not (string-match "XEmacs\\|Lucid" emacs-version))
	 ;; Code for GNU Emacs
	 (setq inferior-ess-mode-map (cons 'keymap comint-mode-map))))

  (define-key inferior-ess-mode-map "\r"       'inferior-ess-send-input)
  (define-key inferior-ess-mode-map "\M-\r"    'ess-transcript-send-command-and-move)
  (define-key inferior-ess-mode-map "\C-c\C-l" 'ess-load-file)
  (define-key inferior-ess-mode-map "\C-c`"    'ess-parse-errors)
  (define-key inferior-ess-mode-map "\C-c\C-d" 'ess-dump-object-into-edit-buffer)
  (define-key inferior-ess-mode-map "\C-c\C-v" 'ess-display-help-on-object)
  (define-key inferior-ess-mode-map "\C-c\C-q" 'ess-quit)
  (define-key inferior-ess-mode-map "\C-c\C-t" 'ess-execute)
  (define-key inferior-ess-mode-map "\C-c\C-s" 'ess-execute-search)
  (define-key inferior-ess-mode-map "\C-c\C-x" 'ess-execute-objects)
  ;;(define-key inferior-ess-mode-map "\C-c\C-a" 'ess-execute-attach)
  (define-key inferior-ess-mode-map "\C-c\034" 'ess-abort)       ; \C-c\C-backslash
  (define-key inferior-ess-mode-map "\C-c\C-z" 'ess-abort)       ; mask comint map
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
   ["Resynch S completions"  ess-resynch                   t]
   ["Quit S"                 ess-quit                      t]
   ["Display search list"    ess-execute-search            t]
   ["Display object list"    ess-execute-objects           t]
   ["Get help on S object"   ess-display-help-on-object    t]
   ["Enter S command"        ess-execute                   t]
   ["Attach directory"       ess-execute-attach            t]
   "------"
   ["Send and move"  ess-transcript-send-command-and-move  t]
   ["Copy command"   comint-copy-old-input                 t]
   ["Send command"   inferior-ess-send-input               t]
   "------"
   ["Jump to Error"  ess-parse-errors                      t]
   ;; need a toggle switch for above, AJR.
   ["Load source file"  ess-load-file                      t]
   ["Edit S Object"     ess-dump-object-into-edit-buffer   t]
   "------"
   ["Describe"         describe-mode                       t]
   ["About"            (ess-goto-info "Entering Commands") t]
   ["Send bug report"  ess-submit-bug-report               t]
   ))


(defun inferior-ess-mode-xemacs-menu ()
  "Hook to install ess-mode menu for XEmacs (w/ easymenu)"
  (if 'inferior-ess-mode
        (easy-menu-add inferior-ess-mode-menu)
    (easy-menu-remove inferior-ess-mode-menu)))

(if (string-match "XEmacs" emacs-version)
    (add-hook 'inferior-ess-mode-hook 'inferior-ess-mode-xemacs-menu))




(defvar ess-mode-minibuffer-map nil)

(if ess-mode-minibuffer-map  nil

  (cond ((string-match "XEmacs\\|Lucid" emacs-version)
	 ;; Code for XEmacs
 	 (setq ess-mode-minibuffer-map (make-keymap))
	 (set-keymap-parent ess-mode-minibuffer-map minibuffer-local-map)))

  (cond ((not (string-match "XEmacs\\|Lucid" emacs-version))
	 ;; Code for Emacs
	 (setq ess-mode-minibuffer-map (cons 'keymap minibuffer-local-map))))

  (define-key ess-mode-minibuffer-map "\t" 'ess-complete-object-name)
  (define-key ess-mode-minibuffer-map "\C-c\C-s" 'ess-execute-search)
  (define-key ess-mode-minibuffer-map "\C-c\C-x" 'ess-execute-objects))

(defun inferior-ess-mode ()
  "Major mode for interacting with an inferior ESS process.
Runs an S interactive job as a subprocess of Emacs, with I/O through an
Emacs buffer.  Variable inferior-ess-program controls which S
is run.

Commands are sent to the ESS process by typing them, and pressing
\\[inferior-ess-send-input].  Pressing \\[complete-dynamic-complete]
completes known object names or filenames, as appropriate.  Other
keybindings for this mode are:

\\{inferior-ess-mode-map}

When editing S objects, the use of \\[ess-load-file] is advocated.
ess-load-file keeps source files (if ess-keep-dump-files is non-nil) in
the directory specified by ess-source-directory, with the
filename chosen according to ess-dump-filename-template. When a file is
loaded, ess-mode parses error messages and jumps to the appropriate file
if errors occur. The ess-eval- commands do not do this.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-ess-mode-hook (in that order).

You can send text to the inferior ESS process from other buffers containing
S source. The key bindings of these commands can be found by typing
C-h m (help for mode) in the other buffers.
    ess-eval-region sends the current region to the ESS process.
    ess-eval-buffer sends the current buffer to the ESS process.
    ess-eval-function sends the current function to the ESS process.
    ess-eval-line sends the current line to the ESS process.
    ess-beginning-of-function and ess-end-of-function move the point to
        the beginning and end of the current S function.
    ess-switch-to-ESS switches the current buffer to the ESS process buffer.
    ess-switch-to-end-of-ESS switches the current buffer to the ESS process
        buffer and puts point at the end of it.

    ess-eval-region-and-go, ess-eval-buffer-and-go,
        ess-eval-function-and-go, and ess-eval-line-and-go switch to the S
        process buffer after sending their text.

    ess-dump-object-into-edit-buffer moves an S object into a temporary file
        and buffer for editing
    ess-load-file sources a file of commands to the ESS process.

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
  ;;(message "  at very beg. of (inferior-ess-mode): inf.-ess-prompt= %s"
  ;;	   inferior-ess-prompt)
  (comint-mode)
  (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))
  (setq major-mode 'inferior-ess-mode)
  (setq mode-name "iESS") ;(concat "iESS:" ess-dialect))
  (setq mode-line-process
	'(" [" ess-local-process-name "]: %s"))
  (use-local-map inferior-ess-mode-map)
  (if ess-mode-syntax-table
      (set-syntax-table ess-mode-syntax-table)
    ; FIXME: need to do something if not set!  Get from the proper place!
    )
  (add-hook 'comint-input-filter-functions 'ess-search-path-tracker)
  (setq comint-get-old-input 'inferior-ess-get-old-input)

  ;; We set comint-process-echoes to t because inferior-ess-input-sender
  ;; recopies the input. If comint-process-echoes was *meant* to be t ...
  ;;
  ;; except that XLS doesn't like it.  This is an ugly hack that ought
  ;; to go into the dialect configuration...
  (if (or (string= ess-language "XLS")
	  (string= ess-language "SAS"))
      (setq comint-process-echoes nil)
    (setq comint-process-echoes t))

  ;; AJR: add KH's fix.  This is ugly, change to do it right.
  ;; i.e. put it as a buffer local var, in S or R defuns...
  (if (string= ess-language "S")
      (cond ((string= ess-dialect "S3")
	     (setq comint-input-sender 'inferior-ess-input-sender))
	    ((string= ess-dialect "S4")
	     (setq comint-input-sender 'inferior-ess-input-sender))
	    ((string= ess-dialect "S+3")
	     (setq comint-input-sender 'inferior-ess-input-sender))
	    ((string= ess-dialect "S")
	     (setq comint-input-sender 'inferior-ess-input-sender))
	    ((string= ess-dialect "R")
	     (setq comint-input-sender 'inferior-R-input-sender))))

  ;; Font-lock support
  ;; AJR: This (the following local-var is already the case!
  ;; KH sez; only in XEmacs :-(.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(inferior-ess-font-lock-keywords nil nil ((?' . "."))))

  (ess-setq-vars-local ess-customize-alist (current-buffer))

  ;; Completion support
  (setq comint-dynamic-complete-functions
	'(ess-complete-filename
	  ess-complete-object-name
	  comint-replace-by-expanded-history))

  ;; (setq comint-completion-addsuffix nil) ; To avoid spaces after filenames
  ;; KH: next 2 lines solve.
  (make-local-variable 'comint-completion-addsuffix)
  (setq comint-completion-addsuffix (cons "/" ""))

  (setq comint-input-autoexpand t) ; Only for completion, not on input.

  ;;; Keep <tabs> out of the code.
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat inferior-ess-primary-prompt "\\|\^L"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "\^L")


  (ess-load-object-name-db-file)
  (sleep-for 0.5)
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
  (ess-eval-visibly (concat string "\n")))

;;> <PD writes>:
;;> Also, invoking help() from the command line may lead to confusing
;;> output, somewhat worse with R than with S. You're not really supposed
;;> to do that (use C-c C-v to invoke ess-display-help-on-object), but it's
;;> an obvious newcomer's mistake. (I wonder: could the elisp-code not
;;> quite easily recognize help calls (at least in the ?xxx form) and do
;;> the right thing automagically?)
;;
;;As promised, here is a quick hack:
;;
(defun inferior-R-input-sender (proc string)
  (if (string-match "help *\(\\([^)]*\\)\)" string)
      (progn
	(insert-before-markers string)
	(let ((string (match-string 1 string)))
	  (ess-display-help-on-object
	   (if (string= string "") "help" string)))
	(ess-eval-visibly "\n"))
    (inferior-ess-input-sender proc string)))

;;
;;(add-hook 'inferior-ess-mode-hook
;;	  '(lambda ()
;;	     (setq comint-input-sender 'inferior-R-input-sender)))
;;
;;Of course, this (currently) only works for the `help(name)' form, and
;;not for `?name'.  And of course, it breaks for `help(data = name)',
;;because ess-display-help-on-object does not support this.


(defun inferior-ess-send-input ()
  "Sends the command on the current line to the ESS process."
  (interactive)
  (ess-make-buffer-current)
  (run-hooks 'ess-send-input-hook)
  (comint-send-input)
  (setq ess-object-list nil)) ;; Will be reconstructed from cache if needs be

(defun inferior-ess-get-old-input ()
  "Returns the ESS command surrounding point."
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
      command)))

;;;*;;; Hot key commands

(defun ess-execute-objects (posn)
  "Send the objects() command to the ESS process.
By default, gives the objects at position 1.
A prefix argument toggles the meaning of ess-execute-in-process-buffer.
A prefix argument of 2 or more means get objects for that position.
A negative prefix argument gets the objects for that position
  and toggles ess-execute-in-process-buffer as well."
  (interactive "P")
  (ess-make-buffer-current)
  (let* ((num-arg (if (listp posn)
		      (if posn -1 1)
		    (prefix-numeric-value posn)))
	(the-posn (if (< num-arg 0) (- num-arg) num-arg))
	(invert (< num-arg 0))
	(the-command (format inferior-ess-objects-command the-posn))
	(the-message (concat ">>> Position "
			     the-posn
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
  "Send the `inferior-ess-search-list-command' [search()] command to the S
process."
  (interactive "P")
  (ess-execute inferior-ess-search-list-command  invert "S search list"))

(defun ess-execute-attach (dir &optional posn)
  "Attach a directory in the ESS process with the attach() command.
When used interactively, user is prompted for DIR to attach and
prefix argument is used for POSN (or 2, if absent.)
Doesn't work for data frames."
  (interactive "DAttach directory: \nP")
  (ess-execute (concat "attach(\""
		     (directory-file-name (expand-file-name dir))
		     "\""
		     (if posn (concat "," (prefix-numeric-value posn)))
		     ")") 'buffer)
  (setq ess-sp-change t))

(defun ess-execute-screen-options ()
  "Cause S to set the `width' option to 1 less than the frame width.
Also sets the `length' option to 99999.
This is a good thing to put in `ess-post-run-hook'."
  (interactive)
  (ess-eval-visibly (format "options(width=%d,length=99999)" (1- (frame-width)))))

(defun ess-execute (command &optional invert buff message)
  "Send a command to the ESS process.
A newline is automatically added to COMMAND. Prefix arg (or second arg INVERT)
means invert the meaning of ess-execute-in-process-buffer. If INVERT is 'buffer,
output is forced to go to the process buffer.
If the output is going to a buffer, name it *BUFF*. This buffer is erased
before use. Optional fourth arg MESSAGE is text to print at the top of the
buffer (defaults to the command if BUFF is not given.)"
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
	(ess-eval-visibly the-command)
      (let ((buff (ess-create-temp-buffer buff-name)))
	(save-excursion
	  (set-buffer buff)
	  (ess-command the-command (get-buffer buff-name) nil)
	  (goto-char (point-min))
	  (if message (insert message)
	    (if buff nil
	      ;; Print the command in the buffer if it has not been
	      ;; given a special name
	      (insert "> " the-command)))
	  ;;(make-local-variable 'ess-local-process-name)
	  (setq ess-local-process-name ess-current-process-name))
	(ess-display-temp-buffer buff)))))

;;;*;;; Quitting

(defun ess-quit ()
  "Issue an exiting command to the inferior process, and clean up."
  (interactive)
  (ess-force-buffer-current "Process to quit: ")
  (ess-make-buffer-current)
  (let ((sprocess (get-ess-process ess-current-process-name)))
    (if (not sprocess) (error "No ESS process running."))
    (if (yes-or-no-p "Really quit from ESS? ")
	(save-excursion
	  (ess-cleanup)
	  (ess-switch-to-ESS nil)
	  (goto-char (marker-position (process-mark sprocess)))
	  (insert inferior-ess-exit-command)
	  (process-send-string sprocess inferior-ess-exit-command)))))

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
  "Kill (or offer to kill) all buffers associated with this ESS process
Leaves you in the ESS process buffer. It's a good idea to run this
before you quit. It is run automatically by \\[ess-quit]."
  (interactive)
  (let ((the-procname (or (ess-make-buffer-current) ess-local-process-name)))
    (if the-procname nil
      (error "I don't know which ESS process to clean up after!"))
    (if (y-or-n-p
	 (format
	  "Delete all buffers associated with process %s? "
	  the-procname))
	(progn
	  (mapcar '(lambda (buf)
		     (set-buffer buf)
		     ;; Consider buffers for which
		     ;; ess-local-process-name is the same as
		     ;; the-procname
		     (if (and (not (get-buffer-process buf))
			      ess-local-process-name
			      (equal ess-local-process-name
				     the-procname))
			 (kill-buffer buf)))
		  (buffer-list))
	  (ess-switch-to-ESS nil)))))

(defun ess-kill-buffer-function nil
  "Function run just before an ESS process buffer is killed"
  ;; This simply deletes the buffers process to avoid an Emacs bug
  ;; where the sentinel is run *after* the buffer is deleted
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc (delete-process proc))))

;;*;; Object name completion

;;;*;;; The user completion command
(defun ess-complete-object-name (&optional listcomp)
  "Perform completion on S object preceding point.
The object is compared against those objects known by
ess-get-object-list and any additional characters up to ambiguity are
inserted.  Completion only works on globally-known objects (including
elements of attached data frames), and thus is most suitable for
interactive command-line entry, and not so much for function editing
since local objects (e.g.  argument names) aren't known.

Use \\[ess-resynch] to re-read the names of the attached directories.
This is done automatically (and transparently) if a directory is
modified, so the most up-to-date list of object names is always
available. However attached dataframes are *not* updated, so this
command may be necessary if you modify an attached dataframe.

If ARG is non-nil, no completion is attempted, but the available
completions are listed."
  (interactive "P")
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
	     ;; See if we're indexing a list with `$'
	     (pattern full-prefix)
	     ;; components  ; WHY IS THIS HERE?
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
	     (components (if listname (ess-object-names listname)
			   (ess-get-object-list
			    ess-current-process-name))))
	;; always return a non-nil value to prevent history expansions
	(or (comint-dynamic-simple-complete  pattern components) 'none))))

(defun ess-list-object-completions nil
  "List all possible completions of the object name at point."
  (interactive)
  (ess-complete-object-name t))

;;;*;;; Support functions
(defun ess-extract-onames-from-alist (alist posn &optional force)
  "Return the object names in position POSN of ALIST.
ALIST is an alist like ess-sl-modtime alist. POSN should be 1 .. (length
ALIST).  If optional third arg FORCE is t, the corresponding element
of the search list is re-read. Otherwise it is only re-read if it's a
directory and has been modified since it was last read"
  (let* ((entry (nth (1- posn) alist))
	 (dir (car entry))
	 (timestamp (car (cdr entry)))
	 (new-modtime (ess-dir-modtime dir)))
    ;; Refresh the object listing if necessary
    (if (and (not force) (equal new-modtime timestamp)) nil
      (setq timestamp new-modtime)
      (setcdr (cdr entry) (ess-object-names dir posn)))
    (cdr (cdr entry))))

(defun ess-dir-modtime (dir)
  "Return the last modtime if dir is a directory, and nil otherwise."
  ;; Attached dataframes return a modtime of nil.
  (and ess-filenames-map (string-match "^/" dir)
       (nth 5 (file-attributes dir))))

(defun ess-object-modtime (object)
  "Return the modtime of the S object OBJECT (a string)
Searches along the search list for a file named OBJECT and returns its modtime
Returns nil if that file cannot be found"
  (let ((path (ess-search-list))
	result)
    (while (and (not result) path)
      (setq result (file-attributes
		  (concat (file-name-as-directory (car path))
			  object)))
      (setq path (cdr path)))
    (nth 5 result)))

(defun ess-modtime-gt (mod1 mod2)
  "Return t if mod1 is later than mod2"
  (and mod1
       (or (> (car mod1) (car mod2))
	   (and (= (car mod1) (car mod2))
		(> (car (cdr mod1)) (car (cdr mod2)))))))

(defun ess-get-object-list (name)
  "Return the alist of current S object names associated with process NAME"
  (or ess-object-list
      (save-excursion
	(set-buffer (process-buffer (get-ess-process name)))
	(ess-make-buffer-current)
	(if (or (not ess-sl-modtime-alist) ess-sp-change)
	    (ess-get-modtime-list))
	(let* ((alist ess-sl-modtime-alist)
	       (i 2)
	       (n (length alist))
	       result)
	  ;; Always force a re-read of position 1 if it's a database
	  (setq result (ess-extract-onames-from-alist alist 1 t))
	  ;; Re-read remaining directories if necessary.
	  (while (<= i n)
	    (setq result
		  (append result
			  (ess-extract-onames-from-alist alist i)))
	    (setq i (1+ i)))
	  (setq ess-object-list result)))))

(defun ess-get-words-from-vector (command)
  "Evaluate the S command COMMAND, which returns a character vector.
Return the elements of the result of COMMAND as an alist of strings.
A newline is automatically added to COMMAND."
  (let ((tbuffer (get-buffer-create " *ess-names-list*"))
	(names))
    (save-excursion
      (set-buffer tbuffer)
      (buffer-disable-undo tbuffer)
      ;(ess-command (concat command "\n") tbuffer)
      (ess-command command tbuffer) ; thanks, KH.
      (goto-char (point-min))
      (if (not (looking-at "\\s-*\\[1\\]"))
	  (setq names nil)
	(goto-char (point-max))
	(while (re-search-backward "\"\\([^\"]*\\)\"" nil t)
	  (setq names (cons (buffer-substring (match-beginning 1)
					      (match-end 1)) names))))
      (kill-buffer tbuffer))
    names))

(defun ess-compiled-dir (dir)
  "Return non-nil if DIR is an S object directory with special files.
I.e. if the filenames in DIR are not representative of the objects in DIR."
  (or (file-exists-p (concat (file-name-as-directory dir) "__BIGIN"))
      (file-exists-p (concat (file-name-as-directory dir) "___NONFI"))))

(defun ess-object-names (obj &optional pos)
  "Return alist of S object names in directory (or object) OBJ.
If OBJ is a directory name (begins with `/') returns a listing of that dir.
   This may use the search list position POS if necessary.
If OBJ is an object name, returns result of S command names(OBJ).
If OBJ is nil, POS must be supplied, and objects(POS) is returned.
In all cases, the value is an list of object names."
  (if (and obj (file-accessible-directory-p obj))
      ;; Check the pre-compiled object list in ess-object-name-db first
      (or (cdr-safe (assoc obj ess-object-name-db))
	  ;; Take a directory listing
	  (and ess-filenames-map (not (ess-compiled-dir obj))
	       (directory-files obj))
	  ;; Get objects(pos) instead
	  (ess-get-words-from-vector
	   (format inferior-ess-objects-command pos)))
    ;; Want names(obj)
    (or (and obj (ess-get-words-from-vector
		  (format inferior-ess-names-command obj)))
	;; get objects(pos)
	(ess-get-words-from-vector
	 (format inferior-ess-objects-command pos ""))))) ; s4 needs 2
					; args, rest only need 1 ?
					; changes needed to allow for
					; pattern argument to
					; .SmodeObs


(defun ess-create-object-name-db ()
  "Create a database of object names in standard S directories.
This database is saved in the file specified by ess-object-name-db-file,
and is loaded when ess-mode is loaded.  It defines the variable
ess-object-name-db, which is used for completions.

Before you call this function, modify the S search list so that it contains
all the non-changing (i.e. system) S directories.  All positions of the search
list except for position 1 are searched and stored in the database.

After running this command, you should move ess-namedb.el to a directory in
the load-path."
  (interactive)
  (setq ess-object-name-db nil)
  (let ((search-list (cdr (ess-search-list)))
	(pos 2) ; was 2
	name
	(buffer (get-buffer-create " *ess-db*"))
	(temp-object-name-db nil)
	(temp-object-name-db-file ess-object-name-db-file))
    
    (ess-write-to-dribble-buffer
       (format "(object db): search-list=%s \n "
	       search-list))
    (while search-list
      (message "Searching %s" (car search-list))
      (setq temp-object-name-db (cons (cons (car search-list)
					   (ess-object-names nil pos))
				     ess-object-name-db))
      (setq search-list (cdr search-list))
      (ess-write-to-dribble-buffer
       (format "(object db): ess-obj-name-db=%s \n pos=%s"
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
"Reread all directories/objects in ess-search-list to form completions."
 (interactive)
 (if (ess-make-buffer-current) nil
   (error "Not an ESS process buffer"))
 (setq ess-sl-modtime-alist nil)
 (setq ess-object-list nil)
 (setq ess-object-name-db nil)		; perhaps it would be better to reload?
 (setq ess-sp-change t)
 (ess-get-modtime-list))


(defun ess-complete-filename nil
  ;; Do file completion only within strings, or when the ! call is being used
  (if (comint-within-quotes
       (1- (process-mark (get-buffer-process (current-buffer)))) (point))
      ;; (- comint-last-input-start 1) (point))  <- from S4 modeadds.
      ;; changed on 4/12/96 (dxsun)
      ;; This is sensible, but makes it hard to use history refs
      ;; (or
      ;;  (save-excursion
      ;;    (goto-char comint-last-input-start)
      ;;    (looking-at "\\s-*!"))
      ;;  (comint-within-quotes comint-last-input-start (point)))
      (progn
	(comint-replace-by-expanded-filename)
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
  "Return the current search list as a list of strings
Elements which are apparently directories are expanded to full dirnames"
  (save-excursion
    (let ((result  ""))
      (set-buffer (get-ess-buffer ess-current-process-name))
      (if (and ess-search-list (not ess-sp-change))
	  ess-search-list
	(let ((tbuffer (get-buffer-create "search-list"))
	      (homedir ess-directory)
	      elt)
	  (save-excursion
	    (buffer-disable-undo tbuffer)
	    (set-buffer tbuffer)
	    (erase-buffer)
	    (ess-command inferior-ess-search-list-command tbuffer)
	    (goto-char (point-min))
	    (while (re-search-forward "\"\\([^\"]*\\)\"" nil t)
	      (setq elt (buffer-substring (match-beginning 1) (match-end 1)))
	      (if (and (string-match "^[^/]" elt)
		       (file-directory-p (concat ess-directory elt)))
		  (setq elt (concat homedir elt)))
	      (setq result (append result (list elt))))
	    (kill-buffer tbuffer)))
	(setq ess-search-list result)
	(setq ess-sp-change nil)
	result))))

;;; ess-sl-modtime-alist is a list with elements as follows:
;;;  * key             (directory or object name)
;;;  * modtime         (list of 2 integers)
;;;  * name, name ...  (accessible objects in search list posn labeled by key)
;;; It has the same number of elements and is in the same order as the
;;; S search list

(defun ess-get-modtime-list ()
  "Record the modification times of the directories in the search list,
and the objects in those directories.
The result is stored in ess-sl-modtime-alist"
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
    (setq ess-sl-modtime-alist newalist)))


(defun ess-search-path-tracker (str)
  "Check if input STR changed the search path."
;;; This function monitors user input to the inferior ESS process so that
;;; emacs can keep the ess-search-list up to date.  Completing-read uses this
;;; list indirectly when it prompts for help or for an object to dump.
  (if (string-match ess-change-sp-regexp str)
      (setq ess-sp-change t)))

 ; Miscellaneous routines

;;;*;;; Routines for reading object names

(defun ess-read-object-name (p-string)
  "Read an S object name from the minibuffer with completion, and return it"
  (let* ((default (ess-read-object-name-dump))
         (prompt-string (if default
                            (format "%s(default %s) " p-string default)
                          p-string))
         (ess-object-list (mapcar 'list (ess-get-object-list ess-local-process-name)))
         (spec (completing-read prompt-string ess-object-list)))
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

(defun ess-create-temp-buffer (name)
  "Create an empty buffer called NAME, but doesn't display it."
  (let ((buff (get-buffer-create name)))
    (save-excursion
      (set-buffer buff)
      (erase-buffer))
    buff))

(defun ess-display-temp-buffer (buff)
  "Display the buffer BUFF using temp-buffer-show-function"
  (funcall (or temp-buffer-show-function 'display-buffer) buff))

;;*;; Error messages

(defun ess-error (msg)
  "Something bad has happened. Display the S buffer, and cause an error
displaying MSG."
  (display-buffer (process-buffer (get-ess-process ess-current-process-name)))
  (error msg))

 ; Provide package

(provide 'ess-inf)

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

;;; ess-inf.el ends here
