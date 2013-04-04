;;; ess-inf.el --- Support for running S as an inferior Emacs process

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997-1999 A.J. Rossini <rossini@u.washington.edu>,
;;      Martin Maechler <maechler@stat.math.ethz.ch>.
;; Copyright (C) 2000--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2012 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Created: 7 Jan 1994
;; Maintainer: ESS-core <ESS-core@r-project.org>

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

;; Code for handling running ESS processes.

;;; Code:

 ; Requires and autoloads

;;*;; Requires
;; (require 'ess-site)

;; Byte-compiler, SHUT-UP!
(eval-and-compile
  (require 'ess-utils))
(unless (featurep 'xemacs)
  (require 'newcomment nil t))
(require 'comint)
(require 'overlay)

;;; VS: These autoloads are not needed. See coments in ess-mode.el.
;;*;; Autoloads
;; (autoload 'ess-parse-errors                 "ess-mode"  "(autoload).")
;; (autoload 'ess-dump-object-into-edit-buffer "ess-mode"  "(autoload).")
;; (autoload 'ess-beginning-of-function        "ess-mode"  "(autoload).")
;; (autoload 'ess-end-of-function              "ess-mode"  "(autoload).")
;; (autoload 'ess-display-help-on-object       "ess-help"  "(autoload).")

;; (autoload 'ess-extract-word-name            "ess-utils" "(autoload).")
;; (autoload 'ess-uniq-list                    "ess-utils" "(autoload).")

;; (autoload 'ess-transcript-send-command-and-move "ess-trns" "(autoload).")

;; (autoload 'ess-R-complete-object-name       "ess-r-d"   "(autoload).")

(autoload 'ess-eval-region-ddeclient        "ess-dde"   "(autoload).")
(autoload 'ess-eval-linewise-ddeclient      "ess-dde"   "(autoload).")
(autoload 'ess-load-file-ddeclient          "ess-dde"   "(autoload).")
(autoload 'ess-command-ddeclient            "ess-dde"   "(autoload).")

(autoload 'tramp-tramp-file-p           "tramp" "(autoload).")
(autoload 'tramp-file-name-localname    "tramp" "(autoload).")
(autoload 'tramp-dissect-file-name      "tramp" "(autoload).")

;; not really needed as tracebug and developer are loaded in r-d.el
(autoload 'ess-tracebug-send-region       "ess-tracebug"      "(autoload).")
(autoload 'ess-developer-send-function      "ess-developer"     "(autoload).")

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
  ;;      only do the buffer local ...-vars-local ones
  (let ((temp-ess-dialect (eval (cdr (assoc 'ess-dialect
                                            ess-customize-alist))))
        (temp-ess-lang (eval (cdr (assoc 'ess-language
                                         ess-customize-alist)))))
    ;; (with-current-buffer ess-dribble-buffer
    ;;   ;;- Is this needed? (no, but it's useful to see them there [MM])
    ;;   ;; Hack to work around the following "default" (global) setting of vars:
    ;;   ;; make sure our comint-... hack doesn't affect anything else
    ;;   ;;(make-variable-buffer-local 'comint-use-prompt-regexp)
    ;;   (make-local-variable 'comint-use-prompt-regexp)
    ;;   ;; now the abomination:
    ;;   ;; (ess-setq-vars-default ess-customize-alist)

    ;;   (setq-default comint-use-prompt-regexp nil) ; re set HACK!
    ;;   ;;>> Doesn't set ess-language,
    ;;   ;;>> => comint-input-sender is not set to 'ess-input-  ==> no input echo!
    ;;   ;;>> => that's why things fail:
    ;;   ;;>> (ess-setq-vars-local ess-customize-alist (current-buffer))
    ;;   ;;                 ======
    ;;   )

    (run-hooks 'ess-pre-run-hook)
    (ess-write-to-dribble-buffer
     (format "(inf-ess 1): lang=%s, dialect=%s, tmp-dialect=%s, buf=%s\n"
             ess-language ess-dialect temp-ess-dialect (current-buffer)))
    (let* ((process-environment process-environment)
           (defdir (or (and ess-directory-function (funcall ess-directory-function))
                       ess-directory default-directory))

           (temp-dialect (if ess-use-inferior-program-name-in-buffer-name ;VS[23-02-2013]: fixme: this should not be here
                             (if (string-equal temp-ess-dialect "R")
                                 inferior-R-program-name
                               temp-ess-dialect) ; use temp-ess-dialect
                                        ; if not R, R program name
                                        ; otherwise.
                           temp-ess-dialect))
           (temp-lang temp-ess-lang)
           (procname (let ((ntry 0) ;; find the next non-existent process N (*R:N*)
                           (done nil))
                       (while (not done)
                         (setq ntry (1+ ntry)
                               done (not
                                     (get-process (ess-proc-name
                                                   ntry
                                                   temp-dialect)))))
                       (ess-proc-name ntry temp-dialect)))
           (buf-name-str  (concat "*" procname "*"))
           startdir buf method)

      (ess-write-to-dribble-buffer
       (format "(inf-ess 1.1): procname=%s temp-dialect=%s, buf-name=%s \n"
               procname temp-dialect buf-name-str))

      (cond
       ;; 1) try to use current buffer, if inferior-ess-mode but no process
       ((and (not (comint-check-proc (current-buffer)))
             (memq major-mode '(inferior-ess-mode)))
        (setq startdir  (if ess-ask-for-ess-directory
                            (ess-get-directory defdir temp-dialect procname)
                          defdir)
              buf       (current-buffer)
              method    1))

       ;; 2)  Take the *R:N* buffer if already exists (and contains dead proc!)
       ((get-buffer buf-name-str)
        (setq buf       (get-buffer buf-name-str)
              method    2))

       ;; 3)  Pick up a transcript file or create a new buffer
       (t
        (setq startdir  (if ess-ask-for-ess-directory
                            (ess-get-directory defdir temp-dialect procname)
                          defdir)
              buf       (if ess-ask-about-transfile
                            (let ((transfilename (read-file-name "Use transcript file (default none):"
                                                                 startdir "")))
                              (if (string= transfilename "")
                                  (get-buffer-create buf-name-str)
                                (find-file-noselect (expand-file-name  transfilename))))
                          (get-buffer-create buf-name-str))
              method    3)))

      (ess-write-to-dribble-buffer
       (format "(inferior-ess) Method #%d start=%s buf=%s\n" method startdir buf))

      (set-buffer buf)
      ;; Now that we have the buffer, set buffer-local variables.
      (ess-setq-vars-local ess-customize-alist) ; buf)
      (if ess-start-args (setq inferior-ess-start-args ess-start-args))

      ;; Write out debug info
      (ess-write-to-dribble-buffer
       (format "(inf-ess 2.1): ess-language=%s, ess-dialect=%s buf=%s \n"
               ess-language  ess-dialect (current-buffer)))
      ;; (ess-write-to-dribble-buffer
      ;;  (format "(inf-ess 2.2): start args = %s, inf-ess-start-args=%s \n"
      ;;          ess-start-args inferior-ess-start-args))
      ;; (ess-write-to-dribble-buffer
      ;;  (format "(inf-ess finish [%s(%s), %s(%s)]\n"
      ;;          ess-language ess-dialect inferior-ess-program ess-local-process-name))

      ;; Set up history file
      (if ess-history-file
          (if (eq t ess-history-file)
              (setq ess-history-file (concat "." ess-dialect "history"))
            ;; otherwise must be a string "..."
            (unless (stringp ess-history-file)
              (error "`ess-history-file' must be nil, t, or a string"))))

      ;; initialize.
      (if startdir (setq default-directory startdir))
      (let ((ess-directory (or startdir
                               ess-directory)))
        (ess-multi procname buf inferior-ess-start-args)))))


(defvar inferior-ess-objects-command nil
  "The language/dialect specific command for listing objects.
It is initialized from the corresponding inferior-<lang>-objects-command
and then made buffer local."); and the *-<lang>-* ones are customized!
(make-variable-buffer-local 'inferior-ess-objects-command)

(defvar ess-save-lastvalue-command nil
  "The command to save the last value.  See S section for more details.
Default depends on the ESS language/dialect and hence made buffer local")
(make-variable-buffer-local 'ess-save-lastvalue-command)

(defvar ess-retr-lastvalue-command nil
  "The command to retrieve the last value.  See S section for more details.
Default depends on the ESS language/dialect and hence made buffer local")
(make-variable-buffer-local 'ess-retr-lastvalue-command)

;;; A note on multiple processes: the following variables
;;;     ess-local-process-name
;;;     ess-sl-modtime-alist
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
        (when ess-history-file
          (setq comint-input-ring-file-name
                (expand-file-name ess-history-file
                                  (or ess-history-directory ess-directory)))
          (comint-read-input-ring))
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
        (goto-char (point-max))
        (setq ess-sl-modtime-alist nil)

        ;; Add the process filter to catch certain output.
        (set-process-filter (get-process proc-name)
                            'inferior-ess-output-filter)
        ;; (inferior-ess-wait-for-prompt)
        (inferior-ess-mark-as-busy (get-process proc-name))
        (process-send-string (get-process proc-name) "\n") ;; to be sure we catch the prompt if user comp is super-duper fast.
        (ess-write-to-dribble-buffer "(ess-multi 2): waiting for process to start (before hook)\n")
        (ess-wait-for-process (get-process proc-name) nil 0.01)

        ;; arguments cache
        (ess-process-put 'funargs-cache (make-hash-table :test 'equal))
        (ess-process-put 'funargs-pre-cache nil)

        (run-hooks 'ess-post-run-hook)

        ;; EXTRAS
        (ess-load-extras t)
        ;; user initialization can take some time ...
        (ess-write-to-dribble-buffer "(ess-multi 3): waiting for process after hook")
        (ess-wait-for-process (get-process proc-name) nil 0.01)
        )

      (let ((buff (process-buffer (get-process proc-name))))
        (with-current-buffer buff
          (rename-buffer (funcall ess-gen-proc-buffer-name-function proc-name) t))
        (if (and inferior-ess-same-window (not inferior-ess-own-frame))
            (switch-to-buffer buff)
          (pop-to-buffer buff))))))

(defun ess-gen-proc-buffer-name:simple (proc-name)
  "Function to generate buffer name by wrapping PROC-NAME in *proc-name*"
  (format "*%s*" proc-name))

(defun ess-gen-proc-buffer-name:directory (proc-name)
  "Function to generate buffer name by wrapping PROC-NAME in
*proc-name:dir-name* where dir-name is a short directory name."
  (format "*%s:%s*" proc-name (file-name-nondirectory
                               (directory-file-name default-directory))))

(defun ess-gen-proc-buffer-name:abbr-long-directory (proc-name)
  "Function to generate buffer name by wrapping PROC-NAME in
*proc-name:abbreviated-long-dir-name*, where
abbreviated-long-dir-name is an abbreviated full directory name.
Abbreviation performed by `abbreviate-file-name'.
"
  (format "*%s:%s*" proc-name (abbreviate-file-name default-directory)))

(defun inferior-ess-set-status (proc string &optional no-timestamp)
  "Internal function to set the satus of the PROC
If no-timestamp, don't set the last-eval timestamp.
Return the 'busy state."
  ;; todo: do it in one search, use starting position, use prog1
  (let ((busy (not (string-match (concat "\\(" inferior-ess-primary-prompt "\\)\\'") string))))
    (process-put proc 'busy-end? (and (not busy)
                                      (process-get proc 'busy)))
    (if (not busy) (process-put proc 'running-async? nil))
    (process-put proc 'busy busy)
    (process-put proc 'sec-prompt
                 (when inferior-ess-secondary-prompt
                   (string-match (concat "\\(" inferior-ess-secondary-prompt "\\)\\'") string)))
    (unless no-timestamp
      (process-put proc 'last-eval (current-time)))
    busy
    ))

(defun inferior-ess-mark-as-busy (proc)
  (process-put proc 'busy t)
  (process-put proc 'sec-prompt nil))

(defun inferior-ess-run-callback (proc)
  (when (process-get proc 'busy-end?)
    (let ((cb (car (process-get proc 'callbacks))))
      (when cb
        (if ess-verbose
            (ess-write-to-dribble-buffer "executing callback ...\n")
          (process-put proc 'suppress-next-output? t))
        (process-put proc 'callbacks nil)
        (condition-case err
            (funcall cb proc)
          (error (message "%s" (error-message-string err))))
        ))))

(defun ess--if-verbose-write-process-state (proc string &optional filter)
  (ess-if-verbose-write
   (format "\n%s:
    --> busy:%s busy-end:%s sec-prompt:%s interruptable:%s <--
    --> running-async:%s callback:%s suppress-next-output:%s <--
    --> dbg-active:%s is-recover:%s <--
    --> string:%s\n"
           (or filter "NORMAL-FILTER")
           (process-get proc 'busy)
           (process-get proc 'busy-end?)
           (process-get proc 'sec-prompt)
           (process-get proc 'interruptable?)
           (process-get proc 'running-async?)
           (if (process-get proc 'callbacks) "yes")
           (process-get proc 'suppress-next-output?)
           (process-get proc 'dbg-active)
           (process-get proc 'is-recover)
           (if (> (length string) 150)
               (format "%s .... %s" (substring string 0 50) (substring string -50))
             string))))

(defun inferior-ess-output-filter (proc string)
  "Standard output filter for the inferior ESS process.
Ring Emacs bell if process output starts with an ASCII bell, and pass
the rest to `comint-output-filter'.
Taken from octave-mod.el."
  (inferior-ess-set-status proc string)
  (ess--if-verbose-write-process-state proc string)
  (inferior-ess-run-callback proc) ;; protected
  (if (process-get proc 'suppress-next-output?)
      ;; works only for surpressing short output, for time being is enough (for callbacks)
      (process-put proc 'suppress-next-output? nil)
    (comint-output-filter proc (inferior-ess-strip-ctrl-g string))
    (ess--show-process-buffer-on-error string proc)
    ))


(defun ess--show-process-buffer-on-error (string proc)
  (when (string-match "Error\\(:\\| +in\\)" string)
    (ess-show-buffer (process-buffer proc))))

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
  (let*  ((buffer (get-buffer-create bufname))
          (proc (get-process procname)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode. Otherwise, leave buffer and existing process alone.
    (cond ((or (not proc) (not (memq (process-status proc) '(run stop))))
           (with-current-buffer  buffer
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
              (format "Buf %s, :Proc %s, :Prog %s\n :Args= %s\nStart File=%s\n"
                      buffer
                      procname
                      inferior-ess-program
                      inferior-ess-start-args
                      inferior-ess-start-file))
             (comint-exec buffer
                          procname
                          inferior-ess-program
                          inferior-ess-start-file
                          (ess-line-to-list-of-words
                           inferior-ess-start-args)))))
    buffer))


;;*;; Requester functions called at startup

(defun ess-get-directory (default dialect procname)
  (let ((prog-version (cond ((string= dialect "R")
                             (concat ", " inferior-R-version)) ; notably for the R-X.Y versions
                            (inferior-ess-program
                             (concat ", " inferior-ess-program ))
                            (t ""))))
    (ess-prompt-for-directory
     (directory-file-name default)
     (format "ESS (*%s*%s) starting data directory? "
             procname prog-version)
     ;; (format "ESS [%s {%s(%s)}: '%s'] starting data directory? "
     ;;         ;;FIXME: maybe rather tmp-dialect (+ evt drop ess-language?)?
     ;;         procname ess-language ess-dialect prog-version)
     )))


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
(defmacro with-ess-process-buffer (no-error &rest body)
  "Execute BODY with current-buffer set to the process buffer of ess-current-process-name.
If NO-ERROR is t don't trigger an error when there is not current process.

Symbol *proc* is bound to the current process during the evaluation of BODY."
  (declare (indent 1))
  `(let ((*proc* (and ess-local-process-name (get-process ess-local-process-name))))
     (if *proc*
         (with-current-buffer (process-buffer *proc*)
           ,@body)
       (unless ,no-error
         (error "No current ESS process")))))

(defun get-ess-process (&optional name use-another)
  "Return the ESS process named by NAME.  If USE-ANOTHER is non-nil,
and the process NAME is not running (anymore), try to connect to another if
there is one.  By default (USE-ANOTHER is nil), the connection to another
process happens interactively (when possible)."
  (setq name (or name ess-local-process-name))
  (if (null name)           ; should almost never happen at this point
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
      (if use-another ; connect to another running process : the first one
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
               (concat ess-dialect " process to use: ") 'force)
              (get-ess-process ess-current-process-name))
          (error "Process %s is not running" name))))))


;; (defun inferior-ess-wait-for-prompt ()
;;   "Wait until the ESS process is ready for input."
;;   (let* ((cbuffer (current-buffer))
;;       (sprocess (get-ess-process ess-current-process-name))
;;       (sbuffer (process-buffer sprocess))
;;       (r nil)
;;       (timeout 0))
;;     (set-buffer sbuffer)
;;     (while (progn
;;           (if (not (eq (process-status sprocess) 'run))
;;               (ess-error "ESS process has died unexpectedly.")
;;             (if (> (setq timeout (1+ timeout)) ess-loop-timeout)
;;                 (ess-error "Timeout waiting for prompt. Check inferior-ess-prompt or ess-loop-timeout."))
;;             (accept-process-output)
;;             (goto-char (point-max))
;;             (beginning-of-line); bol ==> no need for "^" in *-prompt! (MM?)
;;             ;; above, except for Stata, which has "broken" i/o,
;;             ;; sigh... (AJR)
;;             (setq r (looking-at inferior-ess-prompt))
;;             (not (or r (looking-at ".*\\?\\s *"))))))
;;     (goto-char (point-max))
;;     (set-buffer cbuffer)
;;     (symbol-value r)))

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
;;       2) change the appropriate  inferior-<ESSlang>-program-name
;; (how?) in R/S : assign(paste("inferior-",ESSlang,"-p...."),  filename))

;;*;; Multiple process handling code

(defun ess-make-buffer-current nil
  "Make the process associated with the current buffer the current ESS process.
Returns the name of the process, or nil if the current buffer has none."
  (update-ess-process-name-list)
  ;; (if ess-local-process-name
  ;;     (setq ess-current-process-name ess-local-process-name))
  ess-local-process-name)

(defun ess-get-process-variable (name var)
  "Return the variable VAR (symbol) local to ESS process called NAME (string)."
  (with-current-buffer (process-buffer (get-ess-process name))
    (if (boundp var)
        (symbol-value var))))

(defun ess-set-process-variable (name var val)
  "Set variable VAR (symbol) local to ESS process called NAME (string) to VAL."
  (with-current-buffer (process-buffer (get-ess-process name))
    (set var val)))

(defun ess-process-get (propname)
  "Return the variable PROPNAME (symbol) from the plist of the
current ESS process."
  (process-get (get-process ess-local-process-name) propname))

(defun ess-process-put (propname value)
  "Set the variable PROPNAME (symbol) to VALUE in the plist of
the current ESS process."
  (process-put (get-process ess-local-process-name) propname value))

(defun ess-start-process-specific (language dialect)
  "Start an ESS process typically from a language-specific buffer, using
LANGUAGE (and DIALECT)."

  (unless dialect
    (error "The value of `dialect' is nil"))

  (save-current-buffer
    (let ((dsymb (intern dialect)))
      (ess-write-to-dribble-buffer
       (format " ..start-process-specific: lang:dialect= %s:%s, current-buf=%s\n"
               language dialect (current-buffer)))
      (cond ;; ((string= dialect "R") (R))
            ;; ((string= language "S") ;
            ;;  (message "ESS process not running, trying to start R, since language = 'S")
            ;;  (R))
            ;; ((string= dialect STA-dialect-name) (stata))
            ;;general case
            ((fboundp dsymb)
             (funcall dsymb))
            (t ;; else: ess-dialect is not a function

             ;; Typically triggered from
             ;; ess-force-buffer-current("Process to load into: ")
             ;;  \-->  ess-request-a-process("Process to load into: " no-switch)
             (error "No ESS processes running; not yet implemented to start (%s,%s)"
                    language dialect)))
      ;; save excursion is not working here !!! bad bad bad !!
      )))

(defun ess-request-a-process (message &optional noswitch ask-if-1)
  "Ask for a process, and make it the current ESS process.
If there is exactly one process, only ask if ASK-IF-1 is non-nil.
Also switches to the process buffer unless NOSWITCH is non-nil.  Interactively,
NOSWITCH can be set by giving a prefix argument.
Returns the name of the selected process."
  (interactive
   (list "Switch to which ESS process? " current-prefix-arg))
                                        ; prefix sets 'noswitch
  (ess-write-to-dribble-buffer "ess-request-a-process: {beginning}\n")
  (update-ess-process-name-list)

  (setq ess-dialect
    (or ess-dialect (ess-completing-read
                     "Set `ess-dialect'"
                     (delete-dups (list "R" "S+" S+-dialect-name
                                        "stata" STA-dialect-name
                                        "julia" "SAS" "XLS"  "ViSta")))))

  (let ((num-processes (length ess-process-name-list))
        (inferior-ess-same-window nil) ;; this should produce the inferior process in other window
        (auto-started?))
    (if (or (= 0 num-processes)
            (and (= 1 num-processes)
                 (not (equal ess-dialect ;; don't auto connect if from different dialect
                             (buffer-local-value
                              'ess-dialect
                              (process-buffer (get-process
                                               (caar ess-process-name-list))))))))
        ;; try to start "the appropriate" process
        (progn
          (ess-write-to-dribble-buffer
           (concat " ... request-a-process:\n  "
                   (format
                    "major mode %s; current buff: %s; ess-language: %s, ess-dialect: %s\n"
                    major-mode (current-buffer) ess-language ess-dialect)))
          (ess-start-process-specific ess-language ess-dialect)
          (ess-write-to-dribble-buffer
           (format "  ... request-a-process: buf=%s\n" (current-buffer)))
          (setq num-processes 1
                auto-started? t)))
    ;; now num-processes >= 1 :
    (let* ((proc-buffers (mapcar (lambda (lproc)
                                   (buffer-name (process-buffer (get-process (car lproc)))))
                                 ess-process-name-list))
           (proc
            (if (or auto-started?
                    (and (not ask-if-1) (= 1 num-processes)))
                (progn
                  (message "using process '%s'" (car proc-buffers))
                  (caar ess-process-name-list))
              ;; else
              (unless (and ess-current-process-name
                           (get-process ess-current-process-name))
                (setq ess-current-process-name nil))
              (when message
                (setq message (replace-regexp-in-string ": +\\'" "" message))) ;; <- why is this here??
              ;; ask for buffer name not the *real* process name:
              (let ((buf (ess-completing-read message (append proc-buffers (list "*new*")) nil t nil nil
                                              (and ess-current-process-name
                                                   (buffer-name (process-buffer
                                                                 (get-process ess-current-process-name)))))))
                (if (equal buf "*new*")
                    (progn
                      (ess-start-process-specific ess-language ess-dialect) ;; switches to proc-buff
                      (caar ess-process-name-list))
                  (process-name (get-buffer-process buf))
                  ))
               )))
      (if noswitch
          (pop-to-buffer (current-buffer)) ;; VS: this is weired, but is necessary
        (pop-to-buffer (buffer-name (process-buffer (get-process proc))) t))
      proc)))


(defun ess-force-buffer-current (&optional prompt force no-autostart ask-if-1)
  "Make sure the current buffer is attached to an ESS process.
If not, or FORCE (prefix argument) is non-nil, prompt for a
process name with PROMPT. If NO-AUTOSTART is nil starts the new
process if process associated with current buffer has
died. `ess-local-process-name' is set to the name of the process
selected.  `ess-dialect' is set to the dialect associated with
the process selected. ASK-IF-1 asks user for the process, even if
there is only one process running."
  (interactive
   (list (concat ess-dialect " process to use: ") current-prefix-arg nil))
  ;; fixme: why the above interactive is not working in emacs 24?
  (setq prompt (or prompt "Process to use: "))
  (let ((proc-name (ess-make-buffer-current)))
    (if (and (not force) proc-name (get-process proc-name))
        nil ; do nothing
      ;; Make sure the source buffer is attached to a process
      (if (and ess-local-process-name (not force) no-autostart)
          (error "Process %s has died" ess-local-process-name)
        ;; ess-local-process-name is nil -- which process to attach to
        (let ((proc (ess-request-a-process prompt 'no-switch ask-if-1))
              temp-ess-help-filetype  dialect)
          (with-current-buffer (process-buffer (get-process proc))
            (setq temp-ess-help-filetype inferior-ess-help-filetype))
          (setq ess-local-process-name proc)
          (setq inferior-ess-help-filetype temp-ess-help-filetype)
          )))))

(defun ess-switch-process ()
  "Force a switch to a new underlying process."
  (interactive)
  (ess-force-buffer-current "Process to use: " 'force nil 'ask-if-1))

;;*;;; Commands for switching to the process buffer

(defun ess-switch-to-ESS (eob-p)
  "Switch to the current inferior ESS process buffer.
With (prefix) EOB-P non-nil, positions cursor at end of buffer.
This function should follow the description in `ess-show-buffer'
for showing the iESS buffer, except that the iESS buffer is also
made current."
  (interactive "P")
  (ess-force-buffer-current)
  (if (and ess-current-process-name (get-process ess-current-process-name))
      (progn
        ;; Display the buffer, but don't select it yet.
        (ess-show-buffer
         (buffer-name (process-buffer (get-process ess-current-process-name)))
         t)
        (if eob-p (goto-char (point-max))))
    (message "No inferior ESS process")
    (ding)))

(defun ess-switch-to-ESS-deprecated (eob-p)
  (interactive "P")
  (ess-switch-to-ESS eob-p)
  (message "C-c C-y is deprecated, use C-c C-z instead (ess-switch-to-inferior-or-script-buffer)"))


(defun ess-switch-to-end-of-ESS ()
  "Switch to the end of the inferior ESS process buffer."
  (interactive)
  (ess-switch-to-ESS t))

(defun ess-switch-to-inferior-or-script-buffer (toggle-eob)
  "If in script, switch to the iESS. If in iESS switch to most recent script buffer.

This is a single-key command. Assuming that it is bound to C-c C-z,
you can navigate back and forth between iESS and script buffer
with C-c C-z C-z C-z ...

If variable `ess-switch-to-end-of-proc-buffer' is t (the default)
 this function switches to the end of process buffer.

If TOGGLE-EOB is given, the value of
`ess-switch-to-end-of-proc-buffer' is toggled.
"
  (interactive "P")
  (let ((map (make-sparse-keymap))
        (EOB (if toggle-eob
                 (not ess-switch-to-end-of-proc-buffer)
               ess-switch-to-end-of-proc-buffer)))
    (define-key map (vector last-command-event)
      (lambda (ev eob) (interactive)
        (if (not (eq major-mode 'inferior-ess-mode))
            (ess-switch-to-ESS eob)
          (let ((dialect ess-dialect)
                (loc-proc-name ess-local-process-name)
                (blist (cdr (buffer-list))))
            (while (and blist
                        (with-current-buffer (car blist)
                          (not (or (and (eq major-mode 'ess-mode)
                                        (equal dialect ess-dialect)
                                        (null ess-local-process-name))
                                   (and (eq major-mode 'ess-mode)
                                        (equal loc-proc-name ess-local-process-name))
                                   ))))
              (pop blist))
            (if blist
                (ess-show-buffer (car blist) t)
              (message "Found no buffers for ess-dialect %s associated with process %s"
                       dialect loc-proc-name)))
          )))
    (ess--execute-singlekey-command map nil nil nil EOB)))


(defun get-ess-buffer (name)
  "Return the buffer associated with the ESS process named by NAME."
  (process-buffer (get-ess-process name)))

(defun update-ess-process-name-list ()
  "Remove names with no process."
  (let (defunct)
    (dolist (conselt ess-process-name-list)
      (let ((proc (get-process (car conselt))))
        (unless (and proc (eq (process-status proc) 'run))
          (push conselt defunct))))
    (dolist (pointer defunct)
      (setq ess-process-name-list (delq pointer ess-process-name-list))))
  (if (eq (length ess-process-name-list) 0)
      (setq ess-current-process-name nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ess-show-buffer
;; Something like this almost works, but problems with XEmacs and Emacs
;; differing implementations of the args to display-buffer make this
;; too tough to pursue.  The longer version below works.
;; (defun ess-show-buffer (buf)
;;   "Display the buffer BUF, a string, but do not select it.
;; Returns the window corresponding to the buffer."
;;   ;; On XEmacs, I get an error if third arg to display-buffer is t and
;;   ;; the BUF is in another frame.  Emacs does not have this problem.
;;   (if (featurep 'xemacs)
;;       (display-buffer buf nil (get-frame-for-buffer buf))
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


(defvar ess-bufs-in-frame nil)          ;silence the compiler.
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
  (if (member (buffer-name (get-buffer buf)) (ess-get-buffers-in-frames))
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

;; (defun ess-prompt-wait (proc prompt-reg &optional sleep )
;;   "Wait for a prompt to appear at the end of current buffer.
;; PROC is the ESS process. PROMPT-REG is a regexp of the process
;; prompt to look for. Does not change point. Not to be used in
;; inferior-process buffer. Use `inferior-ess-wait-for-prompt'
;; instead. "
;;   (if sleep (sleep-for sleep)); we sleep here, *and* wait below
;;   (save-excursion
;;       (while (or (accept-process-output proc 0 100)
;;               (progn ;; if no more output, check for prompt
;;                 (goto-char (marker-position (process-mark proc)))
;;                 (beginning-of-line)
;;                 (not (re-search-forward prompt-reg nil t))
;;                 )))))

(defun ess-wait-for-process (proc &optional sec-prompt wait force-redisplay)
  "Wait for 'busy property of the process to become nil.
If SEC-PROMPT is non-nil return if secondary prompt is detected
regardless of whether primary prompt was detected or not.  If
WAIT is non-nil wait for WAIT seconds for process output before
the prompt check, default 0.001s. When FORCE-REDISPLAY is non-nil
force redisplay. You better use WAIT >= 0.1 if you need
FORCE-REDISPLAY to avoid excesive redisplay."
  (unless (eq (process-status proc) 'run)
    (ess-error "ESS process has died unexpectedly."))
  (setq wait (or wait 0.001)) ;;xemacs is stuck if it's 0 here
  (save-excursion
    (while (or (accept-process-output proc wait)
               (if (and sec-prompt (process-get proc 'sec-prompt))
                   nil
                 (if force-redisplay (redisplay 'force))
                 (process-get proc 'busy))))))

;; (defun inferior-ess-ordinary-filter (proc string)
;;   (let ((old-buffer (current-buffer)))
;;     (unwind-protect
;;      (let (moving)
;;        (set-buffer (process-buffer proc))
;;        (setq moving (= (point) (process-mark proc)))
;;        (save-excursion
;;          ;; Insert the text, moving the process-marker.
;;          (goto-char (process-mark proc))
;;          (insert string)
;;          (set-marker (process-mark proc) (point)))
;;        (if moving (goto-char (process-mark proc))))
;;       (set-buffer old-buffer))))

(defun inferior-ess-ordinary-filter (proc string)
  (inferior-ess-set-status proc string t)
  (ess--if-verbose-write-process-state proc string "ordinary-filter")
  (inferior-ess-run-callback proc)
  (with-current-buffer (process-buffer proc)
    ;; (princ (format "%s:" string))
    (insert string)))


(defvar ess-presend-filter-functions nil
  "List of functions to call before sending the input string to the process.
Each function gets one argument, a string containing the text to
be send to the subprocess.  It should return the string sent,
perhaps the same string that was received, or perhaps a modified
or transformed string.

The functions on the list are called sequentially, and each one is
given the string returned by the previous one.  The string returned by
the last function is the text that is actually sent to the process.

You can use `add-hook' to add functions to this list either
globally or locally.

The hook is executed in current buffer. Before execution, the
local value of this hook in the process buffer is appended to the
hook from the current buffer.
")

(defun ess-send-region (process start end &optional visibly message)
  "Low level ESS version of `process-send-region'.
If VISIBLY call `ess-eval-linewise', else call `ess-send-string'.
If MESSAGE is supplied, display it at the end.

Run `comint-input-filter-functions' and curent buffer's and
associated with PROCESS `ess-presend-filter-functions'  hooks.
"
  (if (ess-ddeclient-p)
      (ess-eval-region-ddeclient start end 'even-empty)
    ;; else: "normal", non-DDE behavior:
    (ess-send-string process (buffer-substring start end) visibly message)
    ))

(defvar ess-send-string-function  nil)
(make-variable-buffer-local 'ess-send-string-function)

(defun ess-send-string (process string &optional visibly message)
  "ESS wrapper for `process-send-string'.
Removes empty lines during the debugging.
STRING  need not end with \\n.

Run `comint-input-filter-functions' and current buffer's and
PROCESS' `ess-presend-filter-functions' hooks on the input
STRING.
"
  (setq string (ess--run-presend-hooks process string))
  (inferior-ess--interrupt-subjob-maybe process)
  (inferior-ess-mark-as-busy process)
  (if (fboundp (buffer-local-value 'ess-send-string-function
                                   (current-buffer)))
      ;; sending function is overloaded
      (funcall ess-send-string-function process string visibly)
    (when (and (eq visibly t)
               (null inferior-ess-secondary-prompt)) ; cannot evaluate visibly
      (setq visibly 'nowait))
    (cond ((eq visibly t) ;; wait after each line
           (let ((ess--inhibit-presend-hooks t))
             (ess-eval-linewise string)))
          ((eq visibly 'nowait) ;; insert command and eval invisibly .
           (with-current-buffer (process-buffer process)
             (save-excursion
                 (goto-char (process-mark process))
                 (insert-before-markers
                  (propertize (format "%s\n" (replace-regexp-in-string  "\n[ \t]" "\n+ " string))
                              'font-lock-face 'comint-highlight-input)))
             (process-send-string process (ess--concat-new-line-maybe string))))
          (t
           (process-send-string process (ess--concat-new-line-maybe string)))))
  (if message (message message)))

(defvar ess--inhibit-presend-hooks nil
  "If non-nil don't run presend hooks.")


(defun ess--run-presend-hooks (process string)
  ;; run ess-presend-filter-functions and comint-input-filter-functions
  (if ess--inhibit-presend-hooks
      string
    ;;return modified string
    (let* ((pbuf (process-buffer process))
           ;; also run proc buffer local hooks
           (functions (unless (eq pbuf (current-buffer))
                        (buffer-local-value 'ess-presend-filter-functions pbuf))))
      (setq functions (append  (delq t (copy-sequence functions)) ;; even in let, delq distructs
                               ess-presend-filter-functions))
      (while (and functions string)
        ;; cannot use run-hook-with-args here because string must be passed from one
        ;; function to another
        (if (eq (car functions) t)
            (let ((functions
                   (default-value 'ess-presend-filter-functions)))
              (while (and functions string)
                (setq string (funcall (car functions) string))
                (setq functions (cdr functions))))
          (setq string (funcall (car functions) string)))
        (setq functions (cdr functions)))

      (with-current-buffer pbuf
        (run-hook-with-args 'comint-input-filter-functions string))

      string)))

(defun ess--concat-new-line-maybe (string)
  "Append \\n at the end of STRING if missing."
  (if (string-match "\n\\'" string (max (- (length string) 2) 0))
      string
    (concat string "\n")))


(defvar ess--dbg-del-empty-p t
  "Internal variable to control removal of empty lines during the
debugging.  Let-bind it to nil before calling
`ess-send-string' or `ess-send-region' if no
removal is necessary.")

(defun inferior-ess--interrupt-subjob-maybe (proc)
  "Internal. Interrupt the process if interruptable? process variable is non-nil.
Hide all the junk output in temporary buffer."
  (when (process-get proc 'interruptable?)
    (let ((cb (cadr (process-get proc 'callbacks)))
          (buf (get-buffer-create " *ess-temp-buff*"))
          (old-filter (process-filter proc))
          (old-buff (process-buffer proc)))
      (unwind-protect
          (progn
            (ess-if-verbose-write "interrupting subjob ... start")
            (process-put proc 'interruptable? nil)
            (process-put proc 'callbacks nil)
            (process-put proc 'running-async? nil)
            ;; this is to avoid putting junk in user's buffer on process
            ;; interruption
            (set-process-buffer proc buf)
            (set-process-filter proc 'inferior-ess-ordinary-filter)
            (interrupt-process proc)
            (when cb
              (ess-if-verbose-write "executing interruption callback ... ")
              (funcall cb proc))
            ;; should be very fast as it inputs only the prompt
            (ess-wait-for-process proc)
            (ess-if-verbose-write "interrupting subjob ... finished")
            )
        (set-process-buffer proc old-buff)
        (set-process-filter proc old-filter)
        ))))

(defun ess-async-command-delayed (com &optional buf proc callback delay)
  "Delayed asynchronous ess-command.
COM and BUF are as in `ess-command'. DELAY is a number of idle
seconds to wait before starting the execution of the COM. On
interruption (by user's evaluation) ESS tries to rerun the job
after next DELAY seconds, and the whole process repeats itself
until the command manages to run completely.

DELAY defaults to `ess-idle-timer-interval' + 3 seconds

You should always provide PROC for delayed evaluation, as the
current process might change, leading to unpredictable
consequences.

This function is a wrapper of `ess-async-command' with an
explicit interrupt-callback.
"
  (unless proc
    (error "You must provide PROC argument to ess-async-command-delayed"))
  (let* ((timer (make-symbol "timer"))
         (delay (or delay
                    (+ ess-idle-timer-interval 3)))
         (int-cb  `(lambda (proc)
                     (ess-async-command-delayed ,com ,buf proc ,callback ,delay)))
         (com-fun `(lambda ()
                     (when (eq (process-status ,proc)  'run) ; do nothing if not running
                       (if (or (process-get ,proc 'busy) ; if busy, try later
                               (process-get ,proc 'running-async?))
                           ;; idle timer doesn't work here
                           (run-with-timer ,delay nil 'ess-async-command-delayed
                                           ,com ,buf ,proc ,callback ,delay))
                         (ess-async-command ,com ,buf ,proc ,callback ',int-cb)))))
    (run-with-idle-timer delay nil com-fun)
    ))

;; ;;; VS[03-09-2012]: Test Cases:
;; (ess-command "a<-0\n" nil nil nil nil (get-process "R"))
;; (ess-async-command-delayed "Sys.sleep(5);a<-a+1;cat(1:10)\n" nil
;;                            (get-process "R") (lambda (proc) (message "done")))

;; (ess-async-command-delayed "Sys.sleep(5)\n" nil (get-process "R")
;;                            (lambda (proc) (message "done")))

;; (process-get (get-process "R") 'running-async?)

;; (ess-async-command "{cat(1:5);Sys.sleep(5);cat(2:6)}\n" nil (get-process "R")
;;                    (lambda (proc) (message "done")))
;; (ess-async-command "{cat(1:5);Sys.sleep(5);cat(2:6)}\n" nil (get-process "R")
;;                    (lambda (proc) (message "done"))
;;                    t)
;; (ess-async-command "{cat(1:5);Sys.sleep(5);cat(2:6)}\n" nil (get-process "R")
;;                    (lambda (proc) (message "done"))
;;                    (lambda (proc2) (message "name: %s" (process-name proc2))))


(defun ess-async-command (com &optional buf proc callback interrupt-callback)
  "Asynchronous version of ess-command.
COM, BUF, WAIT and PROC are as in `ess-command'.

CALLBACK is a function to run after the successful
execution. DELAY is a number of seconds to delay execution. When
CAN-INTERRUPT is non-nil, user evaluation can interrupt the
job. In this case the job will be resumed again on
`ess-idle-timer-interval' seconds.

NOTE: Currently this function should be used only for background
jobs like caching. ESS tries to suppress any output from the
asynchronous command, but long output of COM will most likely end
up in user's main buffer.
"
  (let ((proc (or proc (get-process ess-local-process-name))))
    (if (not (and proc
                  (eq (process-status proc) 'run)))
        (error "Process %s is dead" ess-local-process-name)
      (if (or (process-get proc 'busy)
              (process-get proc 'running-async?))
          (error "Process %s is busy or already running an async command." ess-local-process-name)
        (when (eq interrupt-callback t)
          (setq interrupt-callback (lambda (proc))))
        (process-put proc 'callbacks (list callback interrupt-callback))
        (process-put proc 'interruptable? (and interrupt-callback t))
        (process-put proc 'running-async? t)
        (ess-command com buf nil 'no-prompt-check .02 proc)
        ))))


(defun ess-command (com &optional buf sleep no-prompt-check wait proc force-redisplay)
  "Send the ESS process command COM and delete the output from
the ESS process buffer.  If an optional second argument BUF
exists save the output in that buffer. BUF is erased before
use. COM should have a terminating newline.  Guarantees that the
value of .Last.value will be preserved.  When optional third arg
SLEEP is non-nil, `(sleep-for (* a SLEEP))' will be used in a few
places where `a' is proportional to `ess-cmd-delay'.  WAIT and
FORCE-REDISPLAY are as in `ess-wait-for-process' and are passed
to `ess-wait-for-process'.

PROC should be a process, if nil the process name is taken from
`ess-local-process-name'. This command doesn't set 'last-eval
process variable.

Note: for critical, or error prone code you should consider
wrapping the code into:

local({
    olderr <- options(error=NULL)
    on.exit(options(olderr))
    ...
})
"
  ;; Use this function when you need to evaluate some S code, and the
  ;; result is needed immediately. Waits until the output is ready

  ;; the ddeclient-p checks needs to use the local-process-name
  (unless buf
    (setq buf (get-buffer-create " *ess-command-output*")))

  (if (ess-ddeclient-p)
      (ess-command-ddeclient com buf sleep)

    ;; else: "normal", non-DDE behavior:

    (let* ((sprocess (or proc (get-ess-process ess-local-process-name)))
           sbuffer primary-prompt end-of-output oldpb oldpf oldpm
           )

      (unless sprocess
        ;; should hardly happen, since (get-ess-process *)  already checked:
        (error "Process %s is not running!" ess-current-process-name))
      (setq sbuffer (process-buffer sprocess))
      (with-current-buffer sbuffer
        (unless ess-local-process-name
          (setq ess-local-process-name (process-name sprocess))) ; let it be here (calling functions need not set it explicitly)
        (setq primary-prompt  inferior-ess-primary-prompt)
        (ess-if-verbose-write (format "n(ess-command %s ..)" com))
        (unless no-prompt-check
          (when (process-get sprocess 'busy) ;;(looking-at inferior-ess-primary-prompt)
            (ess-error
             "ESS process not ready. Finish your command before trying again.")))
        (setq oldpf (process-filter sprocess))
        (setq oldpb (process-buffer sprocess))
        (setq oldpm (marker-position (process-mark sprocess)))
        ;; need the buffer-local values in result buffer "buf":
        (unwind-protect
            (progn
              (set-process-buffer sprocess buf)
              (set-process-filter sprocess 'inferior-ess-ordinary-filter)
              ;; Output is now going to BUF:
              (with-current-buffer buf
                (setq inferior-ess-primary-prompt primary-prompt) ;; set local value
                (setq buffer-read-only nil)
                (erase-buffer)
                (set-marker (process-mark sprocess) (point-min))
                (inferior-ess-mark-as-busy sprocess)
                (process-send-string sprocess com)
                ;; need time for ess-create-object-name-db on PC
                (if no-prompt-check
                    (sleep-for 0.020); 0.1 is noticeable!
                  ;; else: default
                  (ess-wait-for-process sprocess nil wait force-redisplay)
                  (goto-char (point-max))
                  ;; remove prompt
                  (delete-region (point-at-bol) (point-max)))
              (ess-if-verbose-write " .. ok{ess-command}")
              ))

          (ess-if-verbose-write " .. exiting{ess-command}\n")
          ;; Restore old values for process filter
          (set-process-buffer sprocess oldpb)
          (set-process-filter sprocess oldpf)
          (set-marker (process-mark sprocess) oldpm))))
    buf))


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
;;      (ess-eval-linewise ....)
;; and
;;      (ess-eval-region   ....)

(defun ess-eval-linewise (text &optional invisibly eob even-empty
                               wait-last-prompt sleep-sec wait-sec)
  ;; RDB 28/8/92 added optional arg eob
  ;; MM 2006-08-23: added 'timeout-ms' -- but the effect seems "nil"
  ;; VS 2012-01-18 it was actually nil, replaced with wait-sec - 0.001 default
  ;; MM 2007-01-05: added 'sleep-sec' VS:? this one seems redundant to wait-last-prompt
  "Evaluate TEXT in the ESS process buffer as if typed in w/o tabs.
Waits for prompt after each line of input, so won't break on large texts.

If optional second arg INVISIBLY is non-nil, don't echo commands.
If it is a string, just include that string. If optional third
arg EOB is non-nil go to end of ESS process buffer after
evaluation.  If optional 4th arg EVEN-EMPTY is non-nil, also send
empty text (e.g. an empty line).  If 5th arg WAIT-LAST-PROMPT is
non-nil, also wait for the prompt after the last line; if 6th arg
SLEEP-SEC is a number, ESS will call '(\\[sleep-for] SLEEP-SEC)
at the end of this function.  If the 7th arg WAIT-SEC is set, it
will be used instead of the default .001s and be passed to
\\[ess-wait-for-process].

Run `comint-input-filter-functions' and
`ess-presend-filter-functions' of the associated PROCESS on the
TEXT.
"

  (if (ess-ddeclient-p)
      (ess-eval-linewise-ddeclient text
                                   invisibly eob even-empty
                                   (if wait-last-prompt
                                       ess-eval-ddeclient-sleep))

    ;; else: "normal", non-DDE behavior:
    (unless (numberp wait-sec)
      (setq wait-sec 0.001))  ;;don't make it lower (0.); xemacs is stuck

    (ess-force-buffer-current "Process to use: ")

    ;; Use this to evaluate some code, but don't wait for output.
    (let* ((deactivate-mark); keep local {do *not* deactivate wrongly}
           (cbuffer (current-buffer))
           (sprocess (get-ess-process ess-current-process-name))
           (sbuffer (process-buffer sprocess))
           (win (get-buffer-window sbuffer t))
           ;; (text (ess-replace-in-string text "\t" " "))
           com pos txt-gt-0)

      (let ((comint-input-filter-functions nil)) ;; comint runs them, don't run twise.
        (setq text (ess--concat-new-line-maybe
                    (ess--run-presend-hooks sprocess text))))

      (with-current-buffer sbuffer

        ;; (when (and win
        ;;            (null eob)
        ;;            (<= (process-mark sprocess) (point)))
        ;;   (setq eob t))
        ;; (setq wait-last-prompt t)
        ;; (dbg eob)

        ;; the following is required to make sure things work!
        (when (string= ess-language "STA")
          (if ess-sta-delimiter-friendly;; RAS: mindless replacement of semi-colons
              (setq text (ess-replace-in-string text ";" "\n")))
          (setq invisibly t))
        (setq text (propertize text 'field 'input 'front-sticky t))

        (goto-char (marker-position (process-mark sprocess)))
        (if (stringp invisibly)
            (insert-before-markers (concat "*** " invisibly " ***\n")))
        ;; dbg:
        ;; dbg (ess-write-to-dribble-buffer
        ;; dbg  (format "(eval-visibly 2): text[%d]= '%s'\n" (length text) text))
        (while (or (setq txt-gt-0 (> (length text) 0))
                   even-empty)
          (setq even-empty nil)
          (if txt-gt-0
              (progn
                (setq pos (string-match "\n\\|$" text))
                (setq com (concat (substring text 0 pos) "\n"))
                (setq text (substring text (min (length text) (1+ pos)))))
            ;; else 0-length text
            (setq com "\n"))
          (goto-char (marker-position (process-mark sprocess)))
          (if win (set-window-point win (process-mark sprocess)))
          (when (not invisibly)
            (insert (propertize com 'font-lock-face 'comint-highlight-input)) ;; for consistency with comint :(
            (set-marker (process-mark sprocess) (point)))
          (inferior-ess-mark-as-busy sprocess)
          (process-send-string sprocess com)
          (when (or wait-last-prompt
                    (> (length text) 0))
            (ess-wait-for-process sprocess t wait-sec))
          )
        (if eob (ess-show-buffer (buffer-name sbuffer) nil))
        (goto-char (marker-position (process-mark sprocess)))
        (when win
          (with-selected-window win
            (goto-char (point))
            (recenter (- -1 scroll-margin))) ;; this recenter is crucial to avoid reseting window-point
          )))

    (if (numberp sleep-sec)
        (sleep-for sleep-sec)))); in addition to timeout-ms


;; VS[06-01-2013]: this how far I got in investingating the emacs reseting of
;; window-point. It really happens out of the blue :(

;; (defun test ()
;;   (let* ((cbuffer (get-buffer "*R*"))
;;          (proc (get-buffer-process cbuffer))
;;          (win (get-buffer-window cbuffer t)))
;;     (with-current-buffer cbuffer
;;       (proc-send-test proc win "ls()\n")
;;       (ess-wait-for-process proc t 0.005)
;;       ;; (goto-char (marker-position (process-mark proc)))
;;       ;; (set-window-point win (point))
;;       (proc-send-test proc win "NA\n")
;;       ;; (when win
;;       ;;   (dbg (point))
;;       ;;   (set-window-point win (point-max)))
;;       )))


;; (defun proc-send-test (proc win com)
;;   (with-current-buffer (process-buffer proc)
;;     (goto-char (marker-position (process-mark proc)))
;;     (inferior-ess-mark-as-busy proc)
;;     (insert com)
;;     (set-marker (process-mark proc) (point))
;;     (set-window-point win (point))
;;     (process-send-string proc com)
;;     (dbg (window-point win) (window-end win))
;;     ))

;;;*;;; Evaluate only

(defvar  ess-current-region-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  'highlight)
    overlay)
  "The overlay for highlighting currently evaluated region or line.")

(defun ess-blink-region (start end)
  (when ess-blink-region
    (move-overlay ess-current-region-overlay start end)
    (run-with-timer ess-blink-delay nil
                    (lambda ()
                      (delete-overlay ess-current-region-overlay)))))


(defun ess-eval-region (start end toggle &optional message)
  "Send the current region to the inferior ESS process.
With prefix argument toggle the meaning of `ess-eval-visibly';
this does not apply when using the S-plus GUI, see `ess-eval-region-ddeclient'."
  (interactive "r\nP")
  ;;(untabify (point-min) (point-max))
  ;;(untabify start end); do we really need to save-excursion?
  (ess-force-buffer-current "Process to use: ")
  (message "Starting evaluation...")
  (setq message (or message "Eval region"))

  (save-excursion
    ;; don't send new lines (avoid screwing the debugger)
    (goto-char start)
    (skip-chars-forward "\n\t ")
    (setq start (point))

    (unless mark-active
      (ess-blink-region start end))

    ;; don't send new lines at the end (avoid screwing the debugger)
    (goto-char end)
    (skip-chars-backward "\n\t ")
    (setq end (point)))

  (let* ((proc (get-process ess-local-process-name))
         (visibly (if toggle (not ess-eval-visibly) ess-eval-visibly))
         (dev-p (process-get proc 'developer))
         (tb-p  (process-get proc 'tracebug)))
    (cond
     (dev-p     (ess-developer-send-region proc start end visibly message tb-p))
     (tb-p      (ess-tracebug-send-region proc start end visibly message))
     (t         (ess-send-region proc start end visibly message))
     ))

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


(defun ess-eval-function (vis &optional no-error)
  "Send the current function to the inferior ESS process.
Arg has same meaning as for `ess-eval-region'.

If NO-ERROR is non-nil and the function was successfully
evaluated, return '(beg end) representing the beginning and end
of the current function, otherwise (in case of an error) return
nil."
  (interactive "P")
  (ess-force-buffer-current "Process to use: ")
  (save-excursion
    (ignore-errors
      ;; evaluation is forward oriented
      (forward-line -1)
      (ess-next-code-line 1))
    (let ((beg-end (ess-end-of-function nil no-error)))
      (if beg-end
          (let* ((beg (nth 0 beg-end))
                 (end (nth 1 beg-end))
                 (proc (get-process ess-local-process-name))
                 (tb-p  (process-get proc 'tracebug))
                 (dev-p (process-get proc 'developer))
                 (name (progn (goto-char beg)
                              (forward-word) ;;func names starting with . are not recognized??
                              (ess-read-object-name-default)))
                 (mess (format "Eval function %s"
                               (propertize (or name "???")
                                           'face 'font-lock-function-name-face)))
                 (visibly (if vis (not ess-eval-visibly) ess-eval-visibly)))

            (ess-blink-region beg end)
            (cond
             (dev-p     (ess-developer-send-function proc beg end name visibly mess tb-p))
             (tb-p      (ess-tracebug-send-region    proc beg end      visibly mess 'func))
             (t         (ess-send-region             proc beg end      visibly mess)))
            beg-end)
        nil))))


;; This is from  Mary Lindstrom <lindstro@Biostat.Wisc.Edu>
;; 31 Aug 1995 14:11:43         To: S-mode@stat.math.ethz.ch
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
;; ;;         it moves all the way to the end - which is fine]
;; (defun ess-eval-sexp (vis)
;;   "Send the current sexp to the inferior ESS process.
;; Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
;;   (interactive "P")
;;   (save-excursion
;;     (forward-sexp)
;;     (let ((end (point)))
;;       (backward-sexp)
;;       (ess-eval-region (point) end vis "Eval sexp"))))


(defun ess-eval-function-or-paragraph (vis)
  "Send the current function if \\[point] is inside one, otherwise the current
paragraph other to the inferior ESS process.
Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
  (interactive "P")
  (let ((beg-end (ignore-errors (ess-eval-function vis 'no-error)))) ;; ignore-errors is a hack, ess-eval-function gives stupid errors sometimes
    (if (null beg-end) ; not a function
        (ess-eval-paragraph vis)
      )))

(defun ess-eval-function-or-paragraph-and-step (vis)
  "Send the current function if \\[point] is inside one, otherwise the current
paragraph other to the inferior ESS process.
Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
  (interactive "P")
  (let ((beg-end (ignore-errors (ess-eval-function vis 'no-error)))) ;; ignore-errors is a hack, ess-eval-function gives stupid errors sometimes
    (if (null beg-end) ; not a function
        (ess-eval-paragraph-and-step vis)
      (goto-char (cadr beg-end))
      (if ess-eval-empty
          (forward-line 1)
        (ess-next-code-line 1)
      ))))

(defun ess-eval-region-or-function-or-paragraph (vis)
  "Send the current region if mark is active, if not, send
function if \\[point] is inside one, otherwise the current
paragraph.

 Prefix arg VIS toggles visibility of ess-code as for
`ess-eval-region'."
  (interactive "P")
  (if (and transient-mark-mode mark-active ;; xemacs doesn't have use-region-p
           (> (region-end) (region-beginning)))
      (ess-eval-region (region-beginning) (region-end) vis)
    (ess-eval-function-or-paragraph vis)))


(defun ess-eval-region-or-function-or-paragraph-and-step (vis)
  "Send the current region if mark is active, if not, send
function if \\[point] is inside one, otherwise the current
paragraph. After evaluation step to the next code line or to the
end of region if region was active.

 Prefix arg VIS toggles visibility of ess-code as for
`ess-eval-region'."
  (interactive "P")
  (if (and transient-mark-mode mark-active ;; xemacs doesn't have use-region-p
           (> (region-end) (region-beginning)))
      (let ((end (region-end)))
        (ess-eval-region (region-beginning) end vis)
        (goto-char end))
    (ess-eval-function-or-paragraph-and-step vis)))


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



(defun ess-next-code-line (&optional arg skip-to-eob)
  "Move ARG lines of code forward (backward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.
Don't skip the last empty and comment lines in the buffer unless
SKIP-TO-EOB is non-nil.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((pos (point))
        (n 0)
        (inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc)); n=0 is success
      (if (not (fboundp 'comment-beginning))
          (while (and (= n 0)
                      (looking-at "\\s-*\\($\\|\\s<\\)"))
            (setq n (forward-line inc)))
        (comment-beginning)
        (beginning-of-line)
        (forward-comment (* inc (buffer-size))) ;; as suggested in info file
        )
      (if (or skip-to-eob
              (not (looking-at ess-no-skip-regexp))) ;; don't go to eob or whatever
          (setq arg (- arg inc))
        (goto-char pos)
        (setq arg 0)
        (forward-line 1));; stop at next empty line
      (setq pos (point)))
    (goto-char pos)
    n))

(defun ess-eval-line-and-step (&optional simple-next even-empty invisibly)
  "Evaluate the current line visibly and step to the \"next\" line.
If SIMPLE-NEXT is non-nil, possibly via prefix arg, first skip
empty and commented lines. If 2nd arg EVEN-EMPTY [prefix as
well], also send empty lines.  When the variable `ess-eval-empty'
is non-nil both SIMPLE-NEXT and EVEN-EMPTY are interpreted as
true."
  ;; From an idea by Rod Ball (rod@marcam.dsir.govt.nz)
  (interactive "P\nP"); prefix sets BOTH !
  (ess-force-buffer-current "Process to load into: ")
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      ;; go to end of process buffer so user can see result
      (ess-eval-linewise (buffer-substring (point) end)
                         invisibly 'eob (or even-empty ess-eval-empty))))
  (if (or simple-next ess-eval-empty even-empty)
      (forward-line 1)
    (ess-next-code-line 1)))

(defun ess-eval-region-or-line-and-step (&optional vis)
  "Evaluate region if there is an active one, otherwise the current line.

 Prefix arg VIS toggles visibility of ess-code when evaluating
 the region (as for `ess-eval-region') and has no effect for
 evaluation of the line.
"
  (interactive "P")
  (if (and transient-mark-mode mark-active ;; xemacs doesn't have use-region-p
           (> (region-end) (region-beginning)))
      (ess-eval-region (region-beginning) (region-end) vis)
    (ess-eval-line-and-step)
  ))

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
  "Send the current paragraph to the inferior ESS process and
move forward to the first line after the paragraph.  If not
inside a paragraph, evaluate next one. Arg has same meaning as
for `ess-eval-region'."
  (interactive "P")
  (let ((beg-end (ess-eval-paragraph vis)))
    (goto-char (cadr beg-end))
    (if ess-eval-empty
        (forward-line 1)
      (ess-next-code-line 1)))
  )

;;; Related to the ess-eval-* commands, there are the ess-load
;;; commands.   Need to add appropriate stuff...


(defun ess-load-file (&optional filename)
  "Load an S source file into an inferior ESS process."
  (interactive (list
                (or
                 (and (eq major-mode 'ess-mode)
                      (buffer-file-name))
                 (expand-file-name
                  (read-file-name "Load S file: " nil nil t)))))
  (ess-force-buffer-current "Process to load into: ")
  (if (ess-process-get  'developer)
      (ess-developer-source-current-file filename)
    (if (fboundp (ess-process-get 'source-file-function))
        (funcall (ess-process-get 'source-file-function))

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
              (with-current-buffer source-buffer
                (ess-force-buffer-current "Process to load into: ")
                (ess-check-modifications)))
          (let ((errbuffer (ess-create-temp-buffer ess-error-buffer-name))
                (filename (if (and (fboundp 'tramp-tramp-file-p)
                                   (tramp-tramp-file-p filename))
                              (tramp-file-name-localname (tramp-dissect-file-name filename))
                            filename))
                error-occurred nomessage)
            (ess-command (format inferior-ess-load-command filename) errbuffer) ;sleep ?
            (with-current-buffer errbuffer
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
                              (with-current-buffer source-buffer
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
                         (with-current-buffer source-buffer
                           (setq ess-keep-dump-files doit)))))))
              (ess-switch-to-ESS t))))))))

;; C-c C-l  *used to* eval code:
(defun ess-msg-and-comint-dynamic-list-input-ring ()
 "Display a list of recent inputs entered into the current buffer."
  (interactive)
  (message "C-c C-l  no longer loads a source file in [iESS], rather use C-c M-l instead")
  (comint-dynamic-list-input-ring))

 ; Inferior S mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The major mode inferior-ess-mode
;;;; * Process handling code
;;;; * Completion code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*;; Major mode definition

(defvar inferior-ess-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)

    (define-key map "\C-y"              'ess-yank)
    ;; Use syntax valid *both* for GNU emacs and XEmacs :
    (define-key map "\r"       'inferior-ess-send-input)
    (define-key map "\C-a"     'comint-bol)

    ;; 2010-06-03 SJE
    ;; disabled this in favour of ess-dirs.  Martin was not sure why this
    ;; key was defined anyway in this mode.
    ;;(define-key map "\M-\r"    'ess-transcript-send-command-and-move)
    (define-key map "\C-c\M-l" 'ess-load-file);; no longer overwrites C-c C-l;
    ;; but for now the user deserves a message:
    (define-key map "\C-c\C-l" 'ess-msg-and-comint-dynamic-list-input-ring)
    (define-key map "\C-c`"    'ess-parse-errors)
    (define-key map "\C-c\C-d" 'ess-dump-object-into-edit-buffer)
    (define-key map "\C-c\C-v" 'ess-display-help-on-object)
    (define-key map "\C-c\C-q" 'ess-quit)
    (define-key map "\C-c\C-t" 'ess-execute)
    (define-key map "\C-c\C-s" 'ess-execute-search)
    (define-key map "\C-c\C-x" 'ess-execute-objects)
    (define-key map "\C-c\034" 'ess-abort) ; \C-c\C-backslash
    (define-key map "\C-c\C-z" 'ess-switch-to-inferior-or-script-buffer) ; mask comint map
    (define-key map "\C-d"     'delete-char)   ; EOF no good in S
    (if (and (featurep 'emacs) (>= emacs-major-version 24))
        (define-key map "\t"       'completion-at-point)
      (define-key map "\t"       'comint-dynamic-complete)
      (define-key map "\M-\t"    'comint-dynamic-complete))
    (define-key map "\C-c\t"   'ess-complete-object-name-deprecated)
    (define-key map "\M-?"     'ess-list-object-completions)
    (define-key map "\C-c\C-k" 'ess-request-a-process)
    (define-key map ","        'ess-smart-comma)

    (define-key map "\C-c\C-d"   'ess-doc-map)
    (define-key map "\C-c\C-e"   'ess-extra-map)
    (define-key map "\C-c\C-t"   'ess-dev-map)
    map)
  "Keymap for `inferior-ess' mode.")

(easy-menu-define
  inferior-ess-mode-menu inferior-ess-mode-map
  "Menu for use in Inferior S mode"
  '("iESS"
    ["What is this? (beta)"   ess-mouse-me                  t]
    ["Quit"                 ess-quit                        t]
    ;; ["Send and move"  ess-transcript-send-command-and-move  t]
    ["Copy command"   comint-copy-old-input                 t]
    ["Send command"   inferior-ess-send-input               t]
    ["Switch to Script Buffer" ess-switch-to-inferior-or-script-buffer t]
    ["Get help on S object"   ess-display-help-on-object    t]
    "------"
    ("Process"
     ["Process Echoes" (lambda () (interactive)
                         (setq comint-process-echoes (not comint-process-echoes)))
      :active t
      :style toggle
      :selected comint-process-echoes]
     ("Eval visibly "
      :filter ess--generate-eval-visibly-submenu ))
    "------"
    ("Utils"
     ;; need a toggle switch for above, AJR.
     ["Attach directory"       ess-execute-attach            t]
     ["Display object list"    ess-execute-objects           t]
     ["Display search list"    ess-execute-search            t]
     ["Edit S Object"    ess-dump-object-into-edit-buffer   t]
     ["Enter S command"        ess-execute                   t]
     ["Jump to Error"  ess-parse-errors                      t]
     ["Load source file" ess-load-file                      t]
     ["Resynch S completions"  ess-resynch                   t]
     )
    "------"
    ("start-dev" :visible nil)
    ("end-dev" :visible nil)
    "------"
    ("Font Lock"
     :active inferior-ess-font-lock-keywords
     :filter ess-generate-font-lock-submenu)
    "------"
    ["Describe"         describe-mode                       t]
    ["Send bug report"  ess-submit-bug-report               t]
    ["About"            (ess-goto-info "Entering Commands") t]
    ))



(defun inferior-ess-mode-xemacs-menu ()
  "Hook to install `ess-mode' menu for XEmacs (w/ easymenu)."
  (if 'inferior-ess-mode
      (easy-menu-add inferior-ess-mode-menu)
    (easy-menu-remove inferior-ess-mode-menu)))

(if (string-match "XEmacs" emacs-version)
    (add-hook 'inferior-ess-mode-hook 'inferior-ess-mode-xemacs-menu))

(defvar ess-mode-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)

    (define-key map "\t" 'ess-complete-object-name)
    (define-key map "\C-c\C-s" 'ess-execute-search)
    (define-key map "\C-c\C-x" 'ess-execute-objects)
    map)
  "Keymap used in `ess-execute'"
  )

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

  (comint-mode)

  (set (make-local-variable 'comint-input-sender) 'inferior-ess-input-sender)
  (set (make-local-variable 'process-connection-type) t)
  ;; initialize all custom vars:
  (ess-setq-vars-local ess-customize-alist) ; (current-buffer))

  ;; If comint-process-echoes is t  inferior-ess-input-sender
  ;; recopies the input, otherwise not. VS[03-09-2012]: should be in customize-alist
  (set (make-local-variable 'comint-process-echoes)
       (not (member ess-language '("SAS" "XLS" "OMG" "julia")))) ;; these don't echo

  (when (and (member ess-dialect '("R")) ;; S+ echoes!!
             (not (eq ess-eval-visibly t)))
    ;; when 'nowait or nil, don't wait for process
    (setq comint-process-echoes nil))

  (when comint-use-prompt-regexp ;; why comint is not setting this? bug?
    (set (make-local-variable 'inhibit-field-text-motion) t))

  (unless inferior-ess-prompt ;; build when unset
    (setq inferior-ess-prompt
          (concat "\\("
                  inferior-ess-primary-prompt
                  (when inferior-ess-secondary-prompt "\\|")
                  inferior-ess-secondary-prompt
                  "\\)")))
  (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))
  (setq comint-get-old-input 'inferior-ess-get-old-input) ;; todo: this is R specific
  (add-hook 'comint-input-filter-functions 'ess-search-path-tracker nil 'local) ;; R and S specific

  (setq major-mode 'inferior-ess-mode)
  (setq mode-name "iESS")               ;(concat "iESS:" ess-dialect))
  (setq mode-line-process
        '(" [" ess-mode-line-indicator "]: %s"))
  (use-local-map inferior-ess-mode-map)
  (if ess-mode-syntax-table
      (set-syntax-table ess-mode-syntax-table)
    ;; FIXME: need to do something if not set!  Get from the proper place!
    )

  (ess-write-to-dribble-buffer
   (format "(i-ess 1): buf=%s, lang=%s, comint..echo=%s, comint..sender=%s,\n"
           (current-buffer) ess-language
           comint-process-echoes comint-input-sender))

  (when (string= ess-language "S") ;; todo: what is this doing here?
    (local-set-key "\M-\r"    'ess-dirs))

  ;; Font-lock support
  ;; AJR: This (the following local-var is already the case!
  ;; KH sez: only in XEmacs :-(.  (& Emacs 22.1, SJE).
  (when inferior-ess-font-lock-keywords ;; new system
    (setq inferior-ess-font-lock-defaults
          (ess--extract-default-fl-keywords inferior-ess-font-lock-keywords)))

  (set (make-local-variable 'font-lock-defaults)
       '(inferior-ess-font-lock-defaults nil nil ((?\. . "w") (?\_ . "w") (?' . "."))))

  ;; SJE 2007-06-28: Emacs 22.1 has a bug in that comint-mode will set
  ;; this variable to t, when we need it to be nil.  The Emacs 22
  ;; solution to this bug is to use define-derived-mode to derive
  ;; inferior-ess-mode from comint-mode.  Not sure if we can go down
  ;; that route yet.  I've used the when condition so that if the var
  ;; is nil, don't bother setting it -- as setting it will make a new
  ;; local var.
  (when font-lock-keywords-only
    (setq font-lock-keywords-only nil))

  ;;; Completion support ----------------

  ;; SJE: comint-dynamic-complete-functions is regarded as a hook, rather
  ;; than a regular variable.  Note order of completion (thanks David Brahm):

  (if (and (featurep 'emacs ) (>= emacs-major-version 24))
      (progn
        (remove-hook 'completion-at-point-functions 'comint-completion-at-point t) ;; reset the thook
        (add-hook 'completion-at-point-functions 'comint-c-a-p-replace-by-expanded-history nil 'local)
        (add-hook 'completion-at-point-functions 'ess-object-completion nil 'local) ;;only for R, is it ok?
        (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
        )
    (add-hook 'comint-dynamic-complete-functions
              'ess-complete-filename 'append 'local)
    (add-hook 'comint-dynamic-complete-functions ;; only for R, is it ok?
              'ess-complete-object-name 'append 'local)
    (add-hook 'comint-dynamic-complete-functions
              'comint-replace-by-expanded-history 'append 'local)

    ;; When a hook is buffer-local, the dummy function `t' is added to
    ;; indicate that the functions in the global value of the hook
    ;; should also be run.  SJE: I have removed this, as I think it
    ;; interferes with our normal completion.
    (remove-hook 'comint-dynamic-complete-functions 't 'local))

  ;; (setq comint-completion-addsuffix nil) ; To avoid spaces after filenames
  ;; KH: next 2 lines solve.
  (set (make-local-variable 'comint-completion-addsuffix)
       (cons "/" ""))

  (setq comint-input-autoexpand t) ; Only for completion, not on input.

  ;; timers
  (add-hook 'ess-idle-timer-functions 'ess-cache-search-list nil 'local)
  (add-hook 'ess-idle-timer-functions 'ess-synchronize-dirs nil 'local)

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

  (ess-write-to-dribble-buffer
   (format "(i-ess end): buf=%s, lang=%s, comint..echo=%s, comint..sender=%s,\n"
           (current-buffer) ess-language
           comint-process-echoes comint-input-sender))

  (message
   (concat (substitute-command-keys
            "Type \\[describe-mode] for help on ESS version ")
           ess-version)))

;;*;; Commands used exclusively in inferior-ess-mode

;;;*;;; Main user commands

(defun inferior-ess-input-sender (proc string)
  (inferior-ess--interrupt-subjob-maybe proc)
  (if comint-process-echoes
      (ess-eval-linewise string nil nil ess-eval-empty)
    (ess-send-string proc string)))


(defvar ess-help-arg-regexp "\\(['\"]?\\)\\([^,=)'\"]*\\)\\1"
  "Reg(ular) Ex(pression) of help(.) arguments.  MUST: 2nd \\(.\\) = arg.")
(defconst inferior-R--input-help (format "^ *help *(%s)" ess-help-arg-regexp))
;; (defconst inferior-R-2-input-help (format "^ *\\? *%s" ess-help-arg-regexp))
(defconst inferior-R--input-?-help-regexp
  "^ *\\(?:\\(?1:[a-zA-Z ]*?\\?\\{1,2\\}\\)\\(?2:.+\\)\\)") ; "\\?\\{1,2\\}\\) *['\"]?\\([^,=)'\"]*\\)['\"]?") ;;catch ??
(defconst inferior-R--page-regexp (format "^ *page *(%s)" ess-help-arg-regexp))

(defun ess-R--sanitize-help-topic (string)
  (if (string-match "\\([^:]*:+\\)\\(.*\\)$" string)
      (format "%s`%s`" (match-string 1 string) (match-string 2 string))
    (format "`%s`" string)))

(defun inferior-R-input-sender (proc string)
  (save-current-buffer
    (let ((help-match (and (string-match inferior-R--input-help string)
                           (match-string 2 string)))
          (help-?-match (and (string-match inferior-R--input-?-help-regexp string)
                             (format "%s%s" (match-string 1 string)
                                     (ess-R--sanitize-help-topic (match-string 2 string)))))
          (page-match   (and (string-match inferior-R--page-regexp string)
                             (match-string 2 string))))
      (cond (help-match
             (ess-display-help-on-object help-match)
             (process-send-string proc "\n"))
            (help-?-match
             (if (string-match "\\?\\?\\(.+\\)" help-?-match)
                 (ess--display-indexed-help-page (concat help-?-match "\n") "^\\([^ \t\n]+::[^ \t\n]+\\)[ \t\n]+"
                                                 (format "*ess-apropos[%s](%s)*"
                                                         ess-current-process-name (match-string 1 help-?-match))
                                                 'appropos)
               (ess-display-help-on-object help-?-match "%s\n"))
             (process-send-string proc "\n"))
            (page-match
             (switch-to-buffer-other-window
              (ess-command (concat page-match "\n")
                           (get-buffer-create (concat page-match ".rt"))))
             (R-transcript-mode)
             (process-send-string proc "\n"))

            (t ;; normal command
             (inferior-ess-input-sender proc string)
             )))))

(defun inferior-ess-send-input ()
  "Sends the command on the current line to the ESS process."
  (interactive)
  (run-hooks 'ess-send-input-hook)
  ;; (let ((proc (get-buffer-process (current-buffer))))
  ;;   (if (not proc)
  ;;       (user-error "Current buffer has no process")
  ;;     (let ((comint-process-echoes (or comint-process-echoes
  ;;                                      (< (point) (marker-position (process-mark proc))))))
  ;;       (comint-send-input))))
  (comint-send-input)
  (setq ess-object-list nil)) ;; Will be reconstructed from cache if needs be

(defun inferior-ess--goto-input-start:field ()
  "Move point to the begining of input skiping all continuation lines.
If in the output field, goes to the begining of previous input
field.
Note: inferior-ess-secondary-prompt should match exactly.
"
  (goto-char (field-beginning))
  ;; move to the begining of non-output field
  (while (and (not (= (point) (point-min)))
              (eq (field-at-pos (point)) 'output))
    (goto-char (field-beginning nil t)))
  ;; skip all secondary prompts
  (let ((pos (field-beginning (point) t))
        (secondary-prompt (concat "^" inferior-ess-secondary-prompt)))
    (while (and pos
                (if (eq (get-text-property pos 'field) 'output)
                    (string-match secondary-prompt (field-string-no-properties pos))
                  t))
      (goto-char pos)
      (setq pos (previous-single-property-change pos 'field))))
  )

(defun inferior-ess--goto-input-end:field ()
  "Move point to the end of input skiping all continuation lines.
If in the output field,  goes to the begining of previous input field.

NOTE: to be used only with fields, see `comint-use-prompt-regexp'.
" ;; this func is not used but might be useful some day
  (goto-char (field-end))
  (let ((pos (point))
        (secondary-prompt (concat "^" inferior-ess-secondary-prompt)))
    (while (and pos
                (if (eq (get-text-property pos 'field) 'output)
                    (string-match secondary-prompt (field-string-no-properties pos))
                  t))
      (goto-char pos)
      (setq pos (next-single-property-change pos 'field)))
    ))

(defun inferior-ess--get-old-input:field ()
  "Return the ESS command surrounding point (use with fields)."
  (save-excursion
    (if (eq (field-at-pos (point)) 'output)
        (if (called-interactively-p 'any)
            (error "No command on this line")
          ;; else, just return ""
          "")
      (inferior-ess--goto-input-start:field)
      (let ((command (field-string-no-properties (point)))
            (pos (next-single-property-change (point) 'field ))
            (secondary-prompt (concat "^" inferior-ess-secondary-prompt)))
        (while (and pos
                    (cond
                     ((eq (get-text-property pos 'field) 'input)
                      (setq command (concat command "\n" (field-string-no-properties pos))))
                     ((eq (get-text-property pos 'field) 'output)
                      (string-match secondary-prompt (field-string-no-properties pos)))
                     (t)));; just skip if unknown
          (setq pos (next-single-property-change pos 'field)))
        command))))

;; todo: error when entering a multiline function
;; check.integer <- function(N){
;;      is.integer(N) | !length(grep("[^[:digit:]]", as.character(N)))
;; }
(defun inferior-ess--goto-input-start:regexp ()
  "Move point to the begining of input skiping all continuation lines.
If in the output field, goes to the begining of previous input.
"
  (beginning-of-line)
  (unless (looking-at inferior-ess-prompt)
    (re-search-backward (concat "^" inferior-ess-prompt)))
  ;; at bol
  (when (and inferior-ess-secondary-prompt
             (looking-at inferior-ess-secondary-prompt))
    (while (and (> (forward-line -1) -1)
                (looking-at inferior-ess-secondary-prompt))))
  (unless (looking-at inferior-ess-prompt)
    (ess-error "Beggining of input not found"))
  (comint-skip-prompt)
  )

(defun inferior-ess--get-old-input:regexp ()
  "Return the ESS command surrounding point (use regexp)."
  ;;VS[03-09-2012]: This should not rise errors!! Troubles comint-interrupt-subjob
  (save-excursion
    (let* ((inhibit-field-text-motion t)
           command)
      (beginning-of-line)
      (when  (and inferior-ess-secondary-prompt
                  (looking-at inferior-ess-secondary-prompt))
        (inferior-ess--goto-input-start:regexp))
      (beginning-of-line)
      (if (looking-at inferior-ess-prompt) ; cust.var, might not include sec-prompt
          (progn
            (comint-skip-prompt)
            (setq command (buffer-substring-no-properties (point) (point-at-eol)))
            (when inferior-ess-secondary-prompt
              (while (progn (forward-line 1)
                            (looking-at inferior-ess-secondary-prompt))
                (re-search-forward inferior-ess-secondary-prompt (point-at-eol))
                (setq command (concat command "\n"
                                      (buffer-substring-no-properties (point) (point-at-eol))))
                ))
            (forward-line -1)
            (setq ess-temp-point (point)) ;; this is ugly, used by transcript
            command)
        (message "No command at this point")
        "")
      )))

(defun inferior-ess-get-old-input ()
  "Return the ESS command surrounding point."
  (if comint-use-prompt-regexp
      (inferior-ess--get-old-input:regexp)
    (inferior-ess--get-old-input:field))
  )

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
;;                  (prefix-numeric-value posn)))
;;       (the-posn (if (< num-arg 0) (- num-arg) num-arg))
;;       (invert (< num-arg 0))
;;       (pattern (if current-prefix-arg (read-string "Pattern (.*): ")
;;                  inferior-ess-objects-pattern))
;;       (pattern (if (string= pattern "") ".*" pattern))
;;       (the-command (format inferior-ess-objects-command the-posn pattern))
;;       (the-message (concat ">>> Position "
;;                            the-posn
;;                            " ("
;;                            (nth (1- the-posn) (ess-search-list))
;;                            ") (pattern = "
;;                            pattern
;;                            ")\n")))
;;    (ess-execute the-command invert "S objects" the-message)))

(defun ess-execute-search (invert)
  "Send the `inferior-ess-search-list-command' command to the `ess-language' process.
 [search(..) in S]"
  (interactive "P")
  (ess-execute inferior-ess-search-list-command  invert "S search list"))

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
  (ess-process-put 'sp-for-help-changed? t))

(defun ess-execute-screen-options ()
  "Cause S to set the \"width\" option to 1 less than the window width.
Also sets the \"length\" option to 99999.
This is a good thing to put in `ess-post-run-hook' --- for the S dialects."
  (interactive)
  (if (string= ess-language "S")
      (ess-eval-linewise (format "options(width=%d, length=99999)"
                                 (1- (window-width)))
                         nil nil nil 'wait-prompt)))

(defun ess-execute (command &optional invert buff message)
  "Send a command to the ESS process.
A newline is automatically added to COMMAND.  Prefix arg (or second arg
INVERT) means invert the meaning of
`ess-execute-in-process-buffer'.  If INVERT is 'buffer, output is
forced to go to the process buffer.  If the output is going to a
buffer, name it *BUFF*.  This buffer is erased before use.  Optional
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
        (ess-command the-command buff);; sleep?
        (with-current-buffer buff
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
    ;; else:  non-R
    (ess-force-buffer-current "Process to quit: " nil 'no-autostart)
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
  (ess-force-buffer-current "Process to quit: " nil 'no-autostart)
  (ess-make-buffer-current)
  (let (cmd
        ;;Q     response
        (sprocess (get-ess-process ess-current-process-name)))
    (if (not sprocess) (error "No ESS process running"))
    ;;Q (setq response (completing-read "Save workspace image? "
    ;;Q                                 '( ( "yes".1) ("no" . 1) ("cancel" . 1))
    ;;Q                                 nil t))
    ;;Q (if (string-equal response "")
    ;;Q (setq response "default")); which will ask again (in most situations)
    ;;Q (unless (string-equal response "cancel")
    (ess-cleanup)
    ;;Q   (setq cmd (format "q(\"%s\")\n" response))
    (setq cmd "q()\n")
    (goto-char (marker-position (process-mark sprocess)))
    (process-send-string sprocess cmd)
    ;;(rename-buffer (concat (buffer-name) "-exited") t)
    ;;Q      )
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
    (unless the-procname
      (error "I don't know which ESS process to clean up after!"))
    (when
        (or (eq ess-S-quit-kill-buffers-p t)
            (and
             (eq ess-S-quit-kill-buffers-p 'ask)
             (y-or-n-p
              (format
               "Delete all buffers associated with process %s? " the-procname))))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          ;; Consider buffers for which ess-local-process-name is
          ;; the same as the-procname
          (when (and (not (get-buffer-process buf))
                     ess-local-process-name
                     (equal ess-local-process-name the-procname))
            (kill-buffer buf)))))
    (ess-switch-to-ESS nil)))

(defun ess-kill-buffer-function ()
  "Function run just before an ESS process buffer is killed."
  ;; This simply deletes the buffers process to avoid an Emacs bug
  ;; where the sentinel is run *after* the buffer is deleted
  (let ((proc (get-buffer-process (current-buffer))))
    (if (processp proc) (delete-process proc))))

;;*;; Object name completion

;;;*;;; The user completion command
(defun ess-object-completion ()
  "Return completions at point in a format required by `completion-at-point-functions'. "
  (if (ess-make-buffer-current)
      (let* ((funstart (cdr (ess--funname.start)))
             (completions (ess-R-get-rcompletions funstart))
             (token (pop completions)))
        (when completions
          (list (- (point) (length token)) (point) completions)))
    (when (string-match "complete" (symbol-name last-command))
      (message "No ESS process associated with current buffer")
      nil)
    ))

(defun ess-complete-object-name (&optional listcomp)
  "Perform completion on `ess-language' object preceding point.
Uses \\[ess-R-complete-object-name] when `ess-use-R-completion' is non-nil,
or \\[ess-internal-complete-object-name] otherwise."
  (interactive "P");; FIXME : the `listcomp' argument is NOT used
  (if (ess-make-buffer-current)
      (if ess-use-R-completion
          (ess-R-complete-object-name)
        (ess-internal-complete-object-name listcomp))
    ;; else give a message on second invocation
    (when (string-match "complete" (symbol-name last-command))
      (message "No ESS process associated with current buffer")
      nil)
    ))

(defun ess-complete-object-name-deprecated ()
  "Gives a deprecated message "
  (interactive)
  (ess-complete-object-name)
  (message "C-c TAB is deprecated, completions has been moved to [M-TAB] (aka C-M-i)")
  (sit-for 2 t)
  )

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
ALIST).  If optional third arg FORCE is t, the corresponding element
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

(defun ess-get-object-list (name &optional exclude-first)
  "Return a list of current S object names associated with process NAME,
using `ess-object-list' if that is non-nil.
If exclude-first is non-nil, don't return objects in first positon (.GlobalEnv)."
  (or ess-object-list ;; <<-  MM: this is now always(?) nil; we cache the *-modtime-alist
      (with-current-buffer (process-buffer (get-ess-process name))
        (ess-make-buffer-current)
        (ess-write-to-dribble-buffer (format "(get-object-list %s) .." name))
        (if (or (not ess-sl-modtime-alist)
                (ess-process-get 'sp-for-help-changed?))
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
          (unless exclude-first
            ;; re-read of position 1 :
            (setq result (ess-extract-onames-from-alist alist 1 'force)))
          (ess-write-to-dribble-buffer
           (format " have re-read pos=1: -> length %d\n" (length result)))
          ;; Re-read remaining directories if necessary.
          (while (<= i n)
            (setq result
                  (append result
                          (ess-extract-onames-from-alist alist i)))
            (setq i (1+ i)))
          (setq ess-object-list (ess-uniq-list result))))))

(defun ess-get-words-from-vector (command &optional no-prompt-check wait)
  "Evaluate the S command COMMAND, which returns a character vector.
Return the elements of the result of COMMAND as an alist of
strings.  COMMAND should have a terminating newline. WAIT is
passed to `ess-command'.

To avoid truncation of long vectors, wrap your
command (%s) like this, or a version with explicit options(max.print=1e6):

local({ out <- try({%s}); print(out, max=1e6) })\n
"
  (let ((tbuffer (get-buffer-create
                  " *ess-get-words*")); initial space: disable-undo
        words)
    (ess-if-verbose-write (format "ess-get-words*(%s).. " command))
    (ess-command command tbuffer 'sleep no-prompt-check wait)
    (ess-if-verbose-write " [ok] ..")
    (with-current-buffer tbuffer
      (goto-char (point-min))
      ;; this is bad, only R specific test
      ;; (if (not (looking-at "[+ \t>\n]*\\[1\\]"))
      ;;     (progn (ess-if-verbose-write "not seeing \"[1]\".. ")
      ;;            (setq words nil)
      ;;            )
      (while (re-search-forward "\"\\(\\(\\\\\\\"\\|[^\"]\\)*\\)\"\\( \\|$\\)" nil t);match \"
        (setq words (cons (buffer-substring (match-beginning 1)
                                            (match-end 1)) words))))
    (ess-if-verbose-write
     (if (> (length words) 5)
         (format " |-> (length words)= %d\n" (length words))
       (format " |-> words= '%s'\n" words)))
    (reverse words)))

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
      ;; -----  be used in the directory case !!
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
    ;; would need  pos = which(obj = search())

    ;; else
    (or (and obj  ;; want names(obj)
             (or (ess-write-to-dribble-buffer
                  (format "(ess-object-names obj=%s): no directory - trying names\n"
                          obj))
                 t)
             (ess-get-words-from-vector
              (format inferior-ess-safe-names-command obj)))
        (and (ess-write-to-dribble-buffer
              (format "(ess-object-names obj=%s): no dir.; -> objects()\n" obj))
             nil); must return nil
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
  "Create a database of object names in standard S directories.  This
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

    (with-current-buffer buffer
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
  (setq ess-object-name-db nil)         ; perhaps it would be better to reload?
  (ess-process-put 'sp-for-help-changed? t)
  (ess-get-modtime-list))

(defun ess-filename-completion ()
  ;; > emacs 24
  "Return completion only within string or comment."
  (save-restriction ;; explicitely handle inferior-ess
    (ignore-errors
      (when (and (eq major-mode 'inferior-ess-mode)
                 (> (point) (process-mark (get-buffer-process (current-buffer)))))
        (narrow-to-region (process-mark (get-buffer-process (current-buffer)))
                          (point-max))))
    (when (ess-inside-string-or-comment-p (point))
      (append (comint-filename-completion) '(:exclusive no)))))


(defun ess-complete-filename ()
  "Do file completion only within strings."
  (save-restriction ;; explicitely handle inferior-ess
    (ignore-errors
      (when (and (eq major-mode 'inferior-ess-mode)
                 (> (point) (process-mark (get-buffer-process (current-buffer)))))
        (narrow-to-region (process-mark (get-buffer-process (current-buffer)))
                          (point-max))))
    (when (or (ess-inside-string-or-comment-p (point))) ;; usable within ess-mode as well
      (comint-dynamic-complete-filename))))

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

(defun ess-search-list (&optional force-update)
  "Return the current search list as a list of strings.
Elements which are apparently directories are expanded to full dirnames.
Don't try to use cache if FORCE-UPDATE is non-nil.

Is *NOT* used by \\[ess-execute-search],
but by \\[ess-resynch], \\[ess-get-object-list], \\[ess-get-modtime-list],
\\[ess-execute-objects], \\[ess-object-modtime], \\[ess-create-object-name-db],
and (indirectly) by \\[ess-get-help-files-list]."
  (with-current-buffer
      (get-ess-buffer ess-current-process-name);to get *its* local vars
    (let ((result nil)
          (slist (ess-process-get 'search-list))
          (tramp-mode nil)) ;; hack for bogus file-directory-p below
      (if (and slist
               (not force-update)
               (not (ess-process-get 'sp-for-help-changed?)))
          slist
        ;; else, re-compute:
        (ess-write-to-dribble-buffer " (ess-search-list ... ) ")
        (let ((tbuffer (get-buffer-create " *search-list*"))
              (homedir ess-directory)
              (my-search-cmd inferior-ess-search-list-command); from ess-buffer
              elt)
          (ess-command my-search-cmd tbuffer 0.05); <- sleep for dde only; does (erase-buffer)
          (with-current-buffer tbuffer
            ;; guaranteed by the initial space in its name: (buffer-disable-undo)
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
                ;;-             (ess-write-to-dribble-buffer "not dir.\n")
                )
              (setq result (append result (list elt))))
            (kill-buffer tbuffer)))
        result))))

;;; ess-sl-modtime-alist is a list with elements as follows:
;;;  * key             (directory or object name)
;;;  * modtime         (list of 2 integers)
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
This function monitors user input to the inferior ESS process so
that Emacs can keep the process variable 'search-list' up to
date. `ess-completing-read' in \\[ess-read-object-name] uses this
list indirectly when it prompts for help or for an object to
dump.

From ESS 12.09 this is not necessary anymore, as the search path
is checked on idle time. It is kept for robustness and backward
compatibility only."
  (when ess-change-sp-regexp
    (if (string-match ess-change-sp-regexp str)
        (ess-process-put 'sp-for-help-changed? t))))

 ; Miscellaneous routines

;;;*;;; Routines for reading object names
(defun ess-read-object-name (p-string)
  "Read an S object name from the minibuffer with completion, and return it.
P-STRING is the prompt string."
  (let* ((default (ess-read-object-name-dump))
         (object-list (ess-get-object-list ess-local-process-name))
         (spec (ess-completing-read p-string object-list nil nil nil nil default)))
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
            (buffer-substring-no-properties beg end))))
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

;;;; start of ess-smart-operators
;;;; inspired by slime repl shortcuts

(defvar ess--handy-history nil)

(defun ess-handy-commands ()
  "Request and execute a command from `ess-handy-commands' list."
  (interactive)
  (let* ((commands (or ess--local-handy-commands
                       ess-handy-commands))
         (hist (and (assoc (car ess--handy-history)
                           commands)
                    (car ess--handy-history))))
    (call-interactively
     (cdr (assoc (ess-completing-read "Execute"
                                      (sort (mapcar 'car commands)
                                            'string-lessp) nil t nil
                                            'ess--handy-history hist)
                 commands)))))

(defun ess-smart-comma ()
  "If comma is invoked at the process marker of an ESS inferior
buffer, request and execute a command from `ess-handy-commands'
list."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc
             (eq (point) (marker-position (process-mark proc))))
        (ess-handy-commands)
      (if ess-smart-operators
          (progn
            (delete-horizontal-space)
            (insert ", "))
        (insert ","))
      )))



 ; directories

(defun ess-set-working-directory (path &optional no-error)
  "Set the current working directory to PATH for both ESS
subprocess and Emacs buffer `default-directory'."
  (interactive "DChange working directory to: ")
  (if ess-setwd-command
      (when (and (file-exists-p path)
                 (ess-command (format ess-setwd-command path))
                 ;; use file-name-as-directory to ensure it has trailing /
                 (setq default-directory (file-name-as-directory path))))
    (unless no-error
      (error "Not implemented for dialect %s" ess-dialect))))

(defalias 'ess-change-directory 'ess-set-working-directory)

(defun ess-get-working-directory (&optional no-error)
  "Retrive the current working directory from the current ess process."
  (if ess-getwd-command
      (car (ess-get-words-from-vector ess-getwd-command))
    (unless no-error
      (error "Not implemented for dialect %s" ess-dialect))))


(defun ess-synchronize-dirs ()
  "Set Emacs' current directory to be the same as the subprocess directory.
Used in `ess-idle-timer-functions'."
  (when (and ess-can-eval-in-background
             ess-getwd-command)
    (ess-when-new-input last-sync-dirs
      (ess-if-verbose-write "\n(ess-synchronize-dirs)\n")
      (setq default-directory
            (car (ess-get-words-from-vector ess-getwd-command)))
      default-directory
      )))

(defun ess-dirs ()
  "Set Emacs' current directory to be the same as the *R* process.
"
  ;; Note: This function is not necessary anymore. The Emacs
  ;; default-directory and subprocess working directory are
  ;; synchronized automatically.
  (interactive)
  (let ((dir (car (ess-get-words-from-vector "getwd()\n"))))
    (message "(ESS / default) directory: %s" dir)
    (setq default-directory (file-name-as-directory dir))))

;; (make-obsolete 'ess-dirs 'ess-synchronize-dirs "ESS 12.09")

;; search path
(defun ess--mark-search-list-as-changed ()
  "Internal. Marks all the search-list related variables as
changed."
  ;; other guys might track their own
  (ess-process-put 'sp-for-help-changed? t)
  (ess-process-put 'sp-for-ac-changed? t)
  )

(defun ess-cache-search-list ()
  "Used in `ess-idle-timer-functions', to set
search path related variables."
  (when (and ess-can-eval-in-background
             inferior-ess-search-list-command)
    (ess-when-new-input last-cache-search-list
      (let ((path (ess-search-list 'force))
            (old-path (process-get *proc* 'search-list)))
        (when (not (equal path old-path))
          (process-put *proc* 'search-list path)
          (ess--mark-search-list-as-changed)
          path
          )))))


;;*;; Temporary buffer handling

;; (defun ess-create-temp-buffer (name)
;;  "Create an empty buffer called NAME, but doesn't display it."
;;  (let ((buff (get-buffer-create name)))
;;    (save-excursion
;;      (set-buffer buff)
;;      (erase-buffer))
;;    buff))


;; Ed Kademan's version:
;; From: Ed Kademan <kademan@phz.com>
;; Subject: Re: ess-mode 5.1.16; search list
;; To: rossini@biostat.washington.edu (A.J. Rossini)
;; Cc: Martin Maechler <maechler@stat.math.ethz.ch>, ess-bugs@stat.math.ethz.ch
;; Date: 26 Jul 2000 16:12:12 -0400

;; Dear Tony Rossini,

;; I was having trouble looking at the search list under ess.  When I
;; started up multiple inferior processes---each for a different
;; dialect---ess-mode would issue the wrong variant of the "search"
;; command when I typed C-c C-s.  In case it is useful let me tell you
;; what I did to get it to work for me.

;; I added the component:
;;  (inferior-ess-search-list-command . "search()\n")
;; to S+3-customize-alist and R-customize-alist, and then I redefined the
;; ess-create-temp-buffer function as follows:
(defun ess-create-temp-buffer (name)
  "Create an empty buffer called NAME."
  (let ((buff (get-buffer-create name))
        (elca (eval ess-local-customize-alist)))
    (with-current-buffer buff
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
;;Ed Kademan              508.651.3700
;;PHZ Capital Partners    508.653.1745 (fax)
;;321 Commonwealth Road   <kademan@phz.com>
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
