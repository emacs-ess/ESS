;;; ess-inf.el --- Support for running S as an inferior Emacs process

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Maintainers: Hornik,
;;                       Maechler <maechler@stat.math.ethz.ch>,
;;                       Rossini <rossini@stat.sc.edu>
;; Created: 7 Jan 1994
;; Modified: $Date: 1997/07/07 21:33:43 $
;; Version: $Revision: 1.35 $
;; RCS: $Id: ess-inf.el,v 1.35 1997/07/07 21:33:43 rossini Exp $

;;
;; $Log: ess-inf.el,v $
;; Revision 1.35  1997/07/07 21:33:43  rossini
;; stuff.
;;
;; Revision 1.34  1997/07/07 20:01:39  rossini
;; removed useless comments.
;;
;; Revision 1.33  1997/07/07 19:59:18  rossini
;; cleaned up doc and doc strings in inferior-ess and ess-multi.
;;
;; Revision 1.32  1997/07/07 16:20:48  rossini
;; cleaned up doc-strings.
;;
;; Revision 1.31  1997/07/03 14:28:34  rossini
;; messages better.
;;
;; Revision 1.30  1997/07/03 14:27:30  rossini
;; stuff...
;;
;; Revision 1.29  1997/07/03 14:17:43  rossini
;; added messages for debuggin.
;;
;; Revision 1.28  1997/07/03 14:09:55  rossini
;; ess-customize-alist will NOT be buffer-local.  Not unless really
;; necessary (should only have to initialize things ONCE.
;;
;; Revision 1.27  1997/07/03 14:01:25  rossini
;; must use, at this point.
;;
;; Revision 1.26  1997/07/03 13:44:09  rossini
;; added variable setup to beginning of inferior-ess (needed here AS
;; WELL, before finding buffer (and then setting buffer local)!
;;
;; S-pre-run -> ess-pre-run.
;;
;; Revision 1.25  1997/07/03 13:36:56  rossini
;; --alist -> -alist.  typo.
;;
;; Revision 1.24  1997/07/03 13:26:54  rossini
;; added customization stuff.
;;
;; Revision 1.23  1997/07/02 16:07:55  rossini
;; rehashed inferior-ess.  Should work better, now, I hope!
;;
;; Revision 1.22  1997/07/01 17:23:45  rossini
;; Broken.  Need to go home.
;;
;; Revision 1.21  1997/07/01 16:37:53  rossini
;; let (done 0).
;;
;; Revision 1.20  1997/07/01 16:25:18  rossini
;; last one taken care off.
;;
;; Revision 1.19  1997/07/01 16:23:43  rossini
;; make-local-variables moved to make-variable-... in ess.el
;;
;; Revision 1.18  1997/07/01 16:16:30  rossini
;; more local-variable def's moved to ess.el
;;
;; Revision 1.17  1997/07/01 16:11:44  rossini
;; removed "make-local-variables" which should've been handled in ess.el.
;;
;; Revision 1.16  1997/07/01 15:51:23  rossini
;; removed (make-local-variable 'font-lock-defaults).
;;
;; Revision 1.15  1997/07/01 14:51:39  rossini
;; doc-string change, ess-quit.
;;
;; Revision 1.14  1997/06/30 22:43:03  rossini
;; ess-trans -> ess-trns
;;
;; Revision 1.13  1997/06/30 22:30:41  rossini
;; cleaned up code.
;;
;; Revision 1.12  1997/06/30 22:25:49  rossini
;; ess-directory always should be defined, now!
;;
;; Revision 1.11  1997/06/30 22:22:05  rossini
;; removed localized ess-directory, moved to proper place.
;;
;; Revision 1.10  1997/06/19 20:46:42  rossini
;; added run-hooks for BEING ABLE TO set local directory to be default
;; (different from doing it -- still have to do it...).
;;
;; Revision 1.9  1997/06/18 18:40:08  rossini
;; ess -> S (thanks, MM).
;;
;; Revision 1.8  1997/06/18 15:09:06  rossini
;;  ess-force-buffer-current & ess-request-a-process :
;;        current-prefix-arg  ('prefix-arg' is wrong!)
;;  ess-force-buffer-current  : need another   (and (not force) ...)
;;
;; (this and the last, were due to Martin M.)
;;
;; Revision 1.7  1997/06/18 15:06:11  rossini
;;  force-buffer-current:
;;     Finally made (prefix) argument  FORCE really do something
;;
;; Revision 1.6  1997/06/15 08:50:14  rossini
;; S data dir -> ESS data dir
;;
;; Revision 1.5  1997/06/15 08:29:31  rossini
;; changed inferior mode name to just infESS, since process type is
;; already given.
;;
;; Revision 1.4  1997/05/21 20:06:24  rossini
;; conversion to ess complete.
;;
;; Revision 1.3  1997/05/21 18:54:04  rossini
;; changed requires, removed stupid require.
;;
;; Revision 1.2  1997/05/21 18:43:26  rossini
;; S -> ess.
;;
;; Revision 1.53  1997/04/23 03:13:30  rossini
;; seems to work now?
;;
;; Revision 1.52  1997/04/23 00:29:58  rossini
;; *** empty log message ***
;;
;; Revision 1.51  1997/04/22 23:20:33  rossini
;; *** empty log message ***
;;
;; Revision 1.50  1997/04/22 02:00:02  rossini
;; *** empty log message ***
;;
;; Revision 1.49  1997/04/21 00:11:50  rossini
;; moved S-force-buffer-current here.
;;
;; Revision 1.48  1997/04/18 16:55:56  rossini
;; added autoloads.  for bytecompiler.
;;
;; Revision 1.47  1997/04/18 16:03:44  rossini
;; moved eval-* commands here from S-mode.
;;
;; Revision 1.46  1997/04/16 23:57:25  rossini
;; fixed most of the byte-compiler errors.
;;
;; Revision 1.45  1997/04/14 00:43:01  rossini
;; thinking about menus...
;;
;; Revision 1.44  1997/04/14 00:36:06  rossini
;; patch from Martin to fix the .Xhistory files.
;;
;; Revision 1.43  1997/04/08 19:03:58  rossini
;; Emacs -> GNU Emacs when appropriate.
;;
;; Revision 1.42  1997/04/08 10:31:18  rossini
;; removed FSF GNU.
;;
;; Revision 1.41  1997/04/08 01:28:22  rossini
;; still byte compiler errors.  Need to fix!
;;
;; Revision 1.40  1997/04/08 01:12:56  rossini
;; moved get-S-process
;;
;; Revision 1.39  1997/04/08 00:52:15  rossini
;; variables moved to S.el
;;
;; Revision 1.38  1997/04/07 21:25:57  rossini
;; moved eval-when-compile, requires.
;;
;; Revision 1.37  1997/04/07 18:55:16  rossini
;; removed S,R,XLS.  S0-> inferior S.
;;
;; Revision 1.36  1997/04/07 18:47:19  rossini
;; S0 -> inferior-S
;;
;; Revision 1.35  1997/04/07 02:15:06  rossini
;; S-inf-pager -> inferior-S-pager.
;;
;; Revision 1.34  1997/04/06 18:24:48  rossini
;; added new variable, inferior-S-pager, and replaced function,
;; inferior-S-make-comint
;;
;; Revision 1.33  1997/04/04 17:13:23  rossini
;; fixed 2 errors in coding (syntax whoops).
;;
;; Revision 1.32  1997/04/03 22:54:48  rossini
;; cleaned up code.
;;
;; Revision 1.31  1997/04/03 22:26:02  rossini
;; "S-namedb" is no longer hardwired.
;;
;; Revision 1.30  1997/04/03 21:55:51  rossini
;; started removing hardwired S-namedb.
;;
;; Revision 1.29  1997/04/03 21:45:16  rossini
;; cleaned up.
;;
;; Revision 1.28  1997/04/03 20:03:52  rossini
;; variable naming made consistent (inferior-S-..., not S-inf-...).
;;
;; Revision 1.27  1997/04/03 19:58:04  rossini
;; changed maintainer names.
;;
;; Revision 1.26  1997/04/03 19:56:46  rossini
;; changed name of S-inf-filenames-map to S-filenames-map
;;
;; Revision 1.25  1997/03/10 16:17:35  rossini
;; added hooks for inf-S mode menus (XEmacs).
;;
;; Revision 1.24  1997/03/07 23:35:14  rossini
;; moved relevant S-menu stuff into S-inf.el
;;
;; Revision 1.23  1997/03/07 21:48:48  rossini
;; Commented out the (eval-when-compile...).
;;
;; Revision 1.22  1997/02/10 20:20:16  rossini
;; reformatted stuff -- no changes (thought I'd find some from Martin's
;; old work).
;;
;; Revision 1.21  1997/02/10 16:39:13  rossini
;; corrected, based on Dave Smith's changes.
;;
;; Revision 1.20  1997/02/09 21:34:54  rossini
;; added log file.  Should've done this a longer time ago!  Whoops.
;;
;;

;; This file is part of S-mode

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

;; Code for dealing with running S processes. See S.el for more
;; details.

;;; Code:

 ; Requires and autoloads

;;*;; Requires
(require 'ess)
(require 'comint)

;; Byte-compiler, SHUT-UP! 
(eval-when-compile
  (load "comint"))

;;*;; Autoloads
(autoload 'ess-load-file "ess-mode" "(autoload)." t)
(autoload 'ess-parse-errors "ess-mode" "(autoload)." t)
(autoload 'ess-dump-object-into-edit-buffer "ess-mode" "(autoload)." t)
(autoload 'ess-end-of-function "ess-mode" "(autoload)." t)
(autoload 'ess-beginning-of-function "ess-mode" "(autoload)." t)
(autoload 'ess-extract-word-name "ess-mode" "(autoload)." t)


(autoload 'ess-transcript-send-command-and-move "ess-trns" "(autoload)." t)

 ;;*;; Variables

;; Moved to S.el

 ;;*;; Process handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * User commands for starting an S process
;;;; * Functions called at startup
;;;; * Process handling code
;;;; * Multiple process implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;*;; Starting a process

(defun ess-proc-name (n)
  "Return process name of process N, as a string."
  (concat ess-proc-prefix (if (> n 1) n)))

;; AJR: Moved S,R,XLS to ess-site.

;; AJR: was S0().
(defun inferior-ess (&optional n)
  "Start or switch to inferior S process N.

Without a prefix argument, either starts a new S process, or switches
  to the S process associated with the current buffer.
With a numeric prefix, starts or switches to process N.
The current buffer is used if it is an inferior-ess-mode
or ess-transcript-mode buffer.

If ess-ask-about-transfile is non-nil, you will be asked for a
transcript file to use. If there is no transcript file, the buffer
name will be like *S* or *S2*.

Takes the program name from the variable inferior-ess-program.
The S program name is used to make a symbol name such as `inferior-ess-args'.
If that symbol is a variable its value is used as a string of arguments
when invoking S.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  ;; With prefix arg N: switch to process N if it is running, if not,
  ;; start a new process N.
  ;;
  ;; Use the current buffer if it is in inferior-ess-mode or ess-trans-mode
  ;; If not, maybe ask about starting directory and/or transcript file.
  ;; If no transfile, use buffer *S*
  ;;
  ;; Without prefix arg: switch to buffer of ess-local-process-name if it
  ;; exists, maybe starting a new process; If not, find first N
  ;; s.t. there is no process `ess-proc-prefix'+N.
  ;; Ask as above. 
  ;;
  ;; This function is primarily used to figure out the Process and
  ;; buffer names to use for inferior-ess.

  (interactive "P")
  ;; set up for current language (need here, to get ess-proc-prefix,
  ;; etc).
  (set-buffer ess-dribble-buffer)
  (ess-set-vars-default ess-customize-alist (current-buffer))

  ;; run hooks now, to overwrite the above!
  (run-hooks 'ess-pre-run-hook)    
  (message "(inferior-ess 1): ess-proc-prefix=%s, buf=%s"
	   ess-proc-prefix (current-buffer))
  (let* ((defdir (directory-file-name (or ess-directory default-directory)))
;;	(procname
;;	 (if n (ess-proc-name (prefix-numeric-value n))
;;	   ;; no prefix arg
;;	   (or (and (not (comint-check-proc (current-buffer)))
;;		    ;; Don't start a new process in current buffer if
;;		    ;; one is already running
;;		    ess-local-process-name)
;;	       ;; find a non-existent process
;;	       (let ((ntry 0) done)
;;		 (while (not done)
;;		   (setq ntry (1+ ntry))
;;		   (setq done (not (get-process (ess-proc-name ntry)))))
;;		 (ess-proc-name ntry))))
	 (procname (if n (ess-proc-name (prefix-numeric-value n))
		     ;; no prefix arg
		     (or (and (not (comint-check-proc (current-buffer)))
			      ;; Don't start a new process in current buffer if
			      ;; one is already running
			      ess-local-process-name)
			 ;; find a non-existent process
			 (let ((ntry 0)
			       (done nil))
			   (while (not done)
			     (setq ntry (1+ ntry)
				   done (not
					 (get-process (ess-proc-name ntry)))))
			   (ess-proc-name ntry)))))
	 (startdir nil)
	 (buf nil)
	 (buf-name-str  (concat "*" procname "*")))

    (cond (;; If process is running, we use it:
	   (get-process procname)
	   (setq buf (process-buffer (get-process procname))))
	  
	  (;; Else (it's a new or terminated process) try to use current buffer
	   (and (not buf) 
		(not n)
		(not (comint-check-proc (current-buffer)))
		(memq major-mode '(inferior-ess-mode ess-transcript-mode)))
	   (setq startdir
		 (if ess-ask-for-ess-directory (ess-get-directory defdir)
		   ess-directory))
	   ;;(message "debug second way")
	   (setq buf (current-buffer)))
	  
	  
	  (;;  Not an ESS buffer yet
	   (and (not buf)
		(get-buffer buf-name-str))
	   ;;(message "debug third way")
	   (setq buf (get-buffer buf-name-str)))
	  
	  (;; Ask for transcript file and startdir
	   ;; FIXME -- this should be in ess-get-transfile
	   (not buf)
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
    (ess-set-vars ess-customize-alist buf)
    (message "(inferior-ess 2): ess-proc-prefix=%s , buf=%s"
	     ess-proc-prefix (current-buffer))
    (if startdir (setq default-directory startdir))
    (setq ess-history-file (concat "." ess-proc-prefix "history"))
    (ess-multi procname buf)))

;;; Old code:

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
;;; local to the S process buffer. If required, these variables should
;;; be accessed with the function ess-get-process-variable

(defun ess-multi (name &optional buffer)
  "Start or switch to S process named NAME in the buffer BUFFER.
BUFFER is only needed if process NAME is not running. BUFFER must exist.
Default-directory is the S starting directory. BUFFER may be visiting a file."

;; If ess-process NAME is running, switch to it.  If not, use COMINT
;; to start up a new process, using NAME and BUFFER (which is needed
;; if there is no process NAME).

  ;; (if (< n 1) (error "Argument to ess-multi must be 1 or greater"))
  (let* ((proc-name name)
	 (proc (get-process proc-name)))
    ;; If S process NAME is running, switch to it
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
	(run-hooks 'ess-pre-run-hook)
	(set-buffer
	 (if switches
	     (inferior-ess-make-comint buf-name-str proc-name switches)
	   (inferior-ess-make-comint buf-name-str proc-name)))
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
  "Sentinel for use with S processes."
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
(defun inferior-ess-make-comint (bufname procname &rest switches)
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
	     (comint-exec buffer procname inferior-ess-program nil
			  switches))))
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

;;;;* define two commands consistent with other comint modes, run-s &
;;;;  run-S.
(fset 'run-s (fset 'run-S (symbol-function 'S)))

;;*;; General process handling code

(defun get-ess-process (name)
  "Return the S process named by NAME."
  (update-ess-process-name-list)
  (if (null name)
      (error "No S process is running now.")
    (if (assoc name ess-process-name-list)
	(get-process name)
      (error "Process %s is not running." name))))

(defun inferior-ess-wait-for-prompt ()
  "Wait until the S process is ready for input."
  (let* ((cbuffer (current-buffer))
         (sprocess (get-ess-process ess-current-process-name))
         (sbuffer (process-buffer sprocess))
         (r nil)
	 (timeout 0))
    (set-buffer sbuffer)
    (while (progn
	     (if (not (eq (process-status sprocess) 'run))
		 (ess-error "S process has died unexpectedly.")
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
  "Make the process associated with the current buffer the current S process.
Returns the name of the process, or nil if the current buffer has none."
  (update-ess-process-name-list)
  (if ess-local-process-name
      (setq ess-current-process-name ess-local-process-name))
  ess-local-process-name)

(defun ess-get-process-variable (name var)
  "Return the variable VAR (symbol) local to S process called NAME (string)."  
  ;;(let (proc (get-ess-process name))
    (save-excursion
      (set-buffer (process-buffer (get-ess-process name)))
      (symbol-value var))
    ;;)
    )

(defun ess-set-process-variable (name var val)
  "Set the variable VAR (symbol) local to S process called NAME (string) to VAL." 
  ;;(let* (proc (get-ess-process name))
  (save-excursion
    (set-buffer (process-buffer (get-ess-process name)))
    (set var val))
  ;)
  )

(defun ess-request-a-process (message &optional noswitch)
  "Ask for a process, and make it the current S process.
Also switches to the process buffer, if second arg NOSWITCH (prefix) is non-nil.
Returns the name of the selected process."
  (interactive
   (list "Switch to which S process? " current-prefix-arg)) ;prefix sets 'noswitch
  (update-ess-process-name-list)
  (if (eq (length ess-process-name-list) 0)
      (error "No S processes running."))
  (let ((proc (completing-read message
			       ess-process-name-list
			       nil ; predicate
			       'require-match
			       ;; If in S buffer, don't offer current process
			       (if (eq major-mode 'inferior-ess-mode)
				   ess-proc-prefix
				 ess-current-process-name
				 ;; maybe rather  ess-local-process-name IF exists
				 ))))
    (save-excursion
      (set-buffer (process-buffer (get-process proc)))
      (ess-make-buffer-current))
    (if noswitch nil (switch-to-buffer (process-buffer (get-process proc))))
    proc))



(defun ess-force-buffer-current (prompt &optional force)
  "Make sure the current buffer is attached to an S process. 
If not, or  FORCE (prefix argument) is non-nil,
prompt for a process name with PROMPT.
ess-local-process-name is set to the name of the process selected."
  (interactive 
   (list (concat ess-proc-prefix " process to use: ") current-prefix-arg))
  (if (and (not force) (ess-make-buffer-current))
      nil ; do nothing
    ;; Make sure the source buffer is attached to a process
    (if (and ess-local-process-name (not force))
	(error "Process %s has died." ess-local-process-name)
      ;; ess-local-process-name is nil -- which process to attach to
      (save-excursion
	(let ((proc (ess-request-a-process prompt 'no-switch)))
	  ;;(make-local-variable 'ess-local-process-name)
	  (setq ess-local-process-name proc)
	  ;; why is the mode line not updated ??
	  )))))



;;*;;; Commands for switching to the process buffer

(defun ess-switch-to-S (eob-p)
  "Switch to the current inferior S process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (ess-make-buffer-current)
  (if (and ess-current-process-name (get-process ess-current-process-name))
      (progn
	(pop-to-buffer
	 (process-buffer (get-process ess-current-process-name)))
	(if eob-p (goto-char (point-max))))
    (message "No inferior S process")
    (ding)))

(defun ess-switch-to-end-of-S nil
  "Switch to the end of the inferior S process buffer."
  (interactive)
  (ess-switch-to-S t))


(defun get-ess-buffer (name)
  "Return the buffer associated with the S process named by NAME"
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

(defvar ess-save-lastvalue-command
  "assign(\"smode.lvsave\",.Last.value,frame=0)\n")

(defvar ess-retr-lastvalue-command
  ".Last.value <- get(\"smode.lvsave\",frame=0)\n")

(defun ess-prompt-wait (proc &optional start-of-output)
  "Wait for a prompt to appear at BOL of current buffer
PROC is the S process. Does not change point"
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
  "Send the S process command COM and delete the output
from the S process buffer.  If an optional second argument BUF exists
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
	   "S process not ready. Finish your command before trying again.")))
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

;;; RDB 28/8/92 added optional arg eob

(defun ess-eval-visibly (text &optional invisibly eob)
  "Evaluate TEXT in the S process buffer as if it had been typed in.
If optional second arg INVISIBLY is non-nil, don't echo commands. If
if is a string, just include that string. If optional third arg
eob is non-nil go to end of S buffer after evaluation.
Waits for prompt after each line of input, so won't break on large texts."
  ;; Use this to evaluate some code, but don't wait for output.
  (let* ((cbuffer (current-buffer))
         (sprocess (get-ess-process ess-current-process-name))
         (sbuffer (process-buffer sprocess))
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
	(let ((dokludge (eq (point) (marker-position (process-mark sprocess)))))
	  (insert com)
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
  "Send the current paragraph to the inferior S process.
Prefix arg. VIS-TOGGLE  toggle visibility of ess-code  as for ess-eval-region."
  (interactive "P")
  (save-excursion
    (forward-paragraph)
    (let ((end (point)))
      (backward-paragraph)
      (ess-eval-region (point) end
		       vis-toggle "Eval paragraph"))))



(defun ess-eval-region (start end toggle &optional message)
  "Send the current region to the inferior S process.
With prefix argument, toggle meaning of ess-eval-visibly-p."
  (interactive "r\nP")
  ;; next line is STUPID.
  ;;  (require 'ess-inf)			; for ess-eval-visibly-p
  (ess-force-buffer-current "Process to load into: ")
  (let ((visibly (if toggle (not ess-eval-visibly-p) ess-eval-visibly-p)))
    (if visibly
 	(ess-eval-visibly (buffer-substring start end))
      (if ess-synchronize-evals
	  (ess-eval-visibly (buffer-substring start end)
			  (or message "Eval region"))
	(process-send-region (get-ess-process ess-current-process-name)
			     start end)
	(process-send-string (get-ess-process ess-current-process-name)
			     "\n")))))

(defun ess-eval-buffer (vis)
  "Send the current buffer to the inferior S process.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-eval-region (point-min) (point-max) vis "Eval buffer"))

(defun ess-eval-function (vis)
  "Send the current function to the inferior S process.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (save-excursion
    (ess-end-of-function)
    (let ((end (point)))
      (ess-beginning-of-function)
      (princ (concat "Loading: " (ess-extract-word-name)) t)
      (ess-eval-region (point) end vis
		     (concat "Eval function " (ess-extract-word-name))))))

(defun ess-eval-line (vis)
  "Send the current line to the inferior S process.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
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
  (ess-switch-to-S t))

(defun ess-eval-buffer-and-go (vis)
  "Send the current buffer to the inferior S and switch to the process buffer.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-eval-buffer vis)
  (ess-switch-to-S t))

(defun ess-eval-function-and-go (vis)
  "Send the current function to the inferior S process and switch to
the process buffer. Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-eval-function vis)
  (ess-switch-to-S t))

(defun ess-eval-line-and-go (vis)
  "Send the current line to the inferior S process and switch to the
process buffer. Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-eval-line vis)
  (ess-switch-to-S t))


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
  (define-key inferior-ess-mode-map "\M-\t"    'comint-replace-by-expanded-filename)
  (define-key inferior-ess-mode-map "\M-?"     'ess-list-object-completions)
  (define-key inferior-ess-mode-map "\C-c\C-k" 'ess-request-a-process))

(easy-menu-define
 inferior-ess-mode-menu inferior-ess-mode-map
 "Menu for use in Inferior S mode"
 '("ess-run"
   ["Describe"  describe-mode                            t]
   ;;["About" (lambda nil (interactive)  (ess-goto-info "Entering Commands")) t]
   ["Send bug report"  ess-submit-bug-report               t]
   "------"
   ["Resynch S completions"  ess-resynch                   t]
   ["Quit S"                 ess-quit                      t]
   ["Display search list"    ess-execute-search            t]
   ["Display object list"    ess-execute-objects           t]
   ["Get help on S object"   ess-display-help-on-object    t]
   ["Enter S command"        ess-execute                   t]
   ["Attach directory"       ess-execute-attach            t]
   "------"
   ["Send and move"  ess-transcript-send-command-and-move  t]
   ["Copy command"   comint-copy-old-input               t]
   ["Send command"   inferior-ess-send-input               t]
   "------"
   ["Jump to Error"  ess-parse-errors                      t]
   ;; need a toggle switch for above, AJR.
   ["Load source file"  ess-load-file                      t]
   ["Edit S Object"     ess-dump-object-into-edit-buffer   t]
   ))


(if (not (string-match "XEmacs" emacs-version))
    (progn
      (if (featurep 'ess-inf)
	  (define-key inferior-ess-mode-map [menu-bar ess-run]
	     (cons "ess-run" inferior-ess-mode-menu))
	(eval-after-load "ess-inf"
			  '(define-key inferior-ess-mode-map
			     [menu-bar ess-run]
			     (cons "ess-run"
				   inferior-ess-mode-menu))))))


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
  "Major mode for interacting with an inferior S process.
Runs an S interactive job as a subprocess of Emacs, with I/O through an
Emacs buffer.  Variable inferior-ess-program controls which S
is run.

Commands are sent to the S process by typing them, and pressing
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

You can send text to the inferior S process from other buffers containing
S source. The key bindings of these commands can be found by typing
C-h m (help for mode) in the other buffers.
    ess-eval-region sends the current region to the S process.
    ess-eval-buffer sends the current buffer to the S process.
    ess-eval-function sends the current function to the S process.
    ess-eval-line sends the current line to the S process.
    ess-beginning-of-function and ess-end-of-function move the point to
        the beginning and end of the current S function.
    ess-switch-to-S switches the current buffer to the S process buffer.
    ess-switch-to-end-of-S switches the current buffer to the S process
        buffer and puts point at the end of it.

    ess-eval-region-and-go, ess-eval-buffer-and-go,
        ess-eval-function-and-go, and ess-eval-line-and-go switch to the S
        process buffer after sending their text.

    ess-dump-object-into-edit-buffer moves an S object into a temporary file
        and buffer for editing
    ess-load-file sources a file of commands to the S process.

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
  (setq mode-name "Inf-ESS") ;;(concat "Inferior " ess-proc-prefix))
  (setq mode-line-process
	'(" [" ess-local-process-name "]: %s"))
  (use-local-map inferior-ess-mode-map)
  (set-syntax-table ess-mode-syntax-table)
  (add-hook 'comint-input-filter-functions 'ess-search-path-tracker)
  (setq comint-get-old-input 'inferior-ess-get-old-input)

  ;; We set comint-process-echoes to t because inferior-ess-input-sender
  ;; recopies the input. If comint-process-echoes was *meant* to be t ...
  (setq comint-process-echoes t)
  ;; AJR: add KH's fix.  This is ugly, change to do it right.
  ;; i.e. put it as a buffer local var, in S or R defuns...
  (cond ((string= ess-proc-prefix "S")
	 (setq comint-input-sender 'inferior-ess-input-sender))
	((string= ess-proc-prefix "R")
	 (setq comint-input-sender 'inferior-R-input-sender)))

  ;; Font-lock support
  ;; AJR: This is already the case!
  ;;(make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(inferior-ess-font-lock-keywords nil nil ((?' . "."))))

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

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat inferior-ess-primary-prompt "\\|\^L"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "\^L")

  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'ess-kill-buffer-function)
  (run-hooks 'inferior-ess-mode-hook)
  (message
   (concat (substitute-command-keys
	    "Type \\[describe-mode] for help on ESS version ")
	   ess-mode-version)))

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
	(ess-display-help-on-object (match-string 1 string))
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
  "Sends the command on the current line to the S process."
  (interactive)
  (ess-make-buffer-current)
  (run-hooks 'ess-send-input-hook)
  (comint-send-input)
  (setq ess-object-list nil) ;; Will be reconstucted from cache if needs be
  )

(defun inferior-ess-get-old-input ()
  "Returns the S command surrounding point."
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
  "Send the objects() command to the S process.
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

(defun ess-execute-search (invert)
  "Send the `inferior-ess-search-list-command' [search()] command to the S
process."
  (interactive "P")
  (ess-execute inferior-ess-search-list-command  invert "S search list"))

(defun ess-execute-attach (dir &optional posn)
  "Attach a directory in the S process with the attach() command.
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
  "Send a command to the S process.
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
  "Issue the q() command to S/R, or (exit) to XLS, and clean up."
  (interactive)
  (ess-make-buffer-current)
  (let ((sprocess (get-ess-process ess-current-process-name)))
    (if (not sprocess) (error "No ESS process running."))
    (if (yes-or-no-p "Really quit from ESS? ")
	(save-excursion
	  (ess-cleanup)
	  (ess-switch-to-S nil)
	  (goto-char (marker-position (process-mark sprocess)))
	  (insert inferior-ess-exit-command)
	  (process-send-string sprocess inferior-ess-exit-command)))))

(defun ess-abort ()
  "Kill the S process, without executing .Last or terminating devices.
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
  "Kill (or offer to kill) all buffers associated with this S process
Leaves you in the S process buffer. It's a good idea to run this
before you quit. It is run automatically by \\[ess-quit]."
  (interactive)
  (let ((the-procname (or (ess-make-buffer-current) ess-local-process-name)))
    (if the-procname nil
      (error "I don't know which S process to clean up after!"))
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
	  (ess-switch-to-S nil)))))

(defun ess-kill-buffer-function nil
  "Function run just before an S process buffer is killed"
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
      (ess-command (concat command "\n") tbuffer)
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
	  (ess-get-words-from-vector (format inferior-ess-objects-command pos)))
    ;; Want names(obj)
    (or (and obj (ess-get-words-from-vector (format inferior-ess-names-command obj)))
	;; get objects(pos)
	(ess-get-words-from-vector (format inferior-ess-objects-command pos)))))

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
	(pos 2)
	name
	(buffer (get-buffer-create " *ess-db*")))
    (while search-list
      (message "Searching %s" (car search-list))
      (setq ess-object-name-db (cons (cons (car search-list)
					 (ess-object-names nil pos))
				   ess-object-name-db))
      (setq pos (1+ pos))
      (setq search-list (cdr search-list)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert "(setq ess-object-name-db '")
      (prin1 ess-object-name-db (current-buffer))
      (insert ")\n")
      (setq name (expand-file-name ess-object-name-db-file)) ;;"ess-namedb.el"
      (write-region (point-min) (point-max) name)
      (message "Wrote %s" name))
    (kill-buffer buffer)))

(defun ess-resynch nil
"Reread all directories/objects in ess-search-list to form completions."
 (interactive)
 (if (ess-make-buffer-current) nil
   (error "Not an S process buffer"))
 (setq ess-sl-modtime-alist nil)
 (setq ess-object-list nil)
 (setq ess-object-name-db nil)		; perhaps it would be better to reload?
 (setq ess-sp-change t)
 (ess-get-modtime-list))


(defun ess-complete-filename nil
  ;; Do file completion only within strings, or when the ! call is being used
  (if (comint-within-quotes
       (1- (process-mark (get-buffer-process (current-buffer)))) (point))
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
    (let (result)
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
;;;  * name, name ...  (accessible objects in search list posn labelled by key)
;;; It has the same numner of elements and is in the same order as the
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
;;; This function monitors user input to the inferior S process so that
;;; emacs can keep the ess-search-list up to date.  Completing-read uses this
;;; list indirectly when it prompts for help or for an object to dump.
  (if (string-match ess-change-sp-regexp str)
      (setq ess-sp-change t)))

 ; Miscellaneous routines

;;;*;;; Routines for reading object names

(defun ess-read-object-name (p-string)
  "Read an S object name from the minibuffer with completion, and return it"
  (let* ((default (ess-read-object-name-default))
         (prompt-string (if default
                            (format "%s(default %s) " p-string default)
                          p-string))
         (ess-object-list (mapcar 'list (ess-get-object-list ess-local-process-name)))
         (spec (completing-read prompt-string ess-object-list)))
    (list (cond
           ((string= spec "") default)
           (t spec)))))

(defun ess-read-object-name-default ()
  ;; Return the object name at point, or nil if none
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
