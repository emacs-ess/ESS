;;; S mode for GNU Emacs     6-Nov-91
;;; Copyright 1989,1991      Doug Bates    bates@stat.wisc.edu
;;;                          Ed Kademan    kademan@stat.wisc.edu
;;;                          Frank Ritter  ritter@psy.cmu.edu
;;;                                            (or  @cs.cmu.edu)
;;; BRIEF OVERVIEW
;;; Supports stuctured editing of S (a statistics package)
;;; functions that is integrated with a running S process in a
;;; buffer.  
;;; 

;;; GENERAL DISCLAIMER
;;; 
;;; This program is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation; either
;;; version 1, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;; 
;;; You should have received a copy of the GNU General Public
;;; License along with this program; if not, write to the Free
;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;; 

;;; OVERVIEW OF S MODE
;;; 
;;; S is a statistics package available from Bell Labs
;;; particularly suited for descriptive and exploratory
;;; statistics.  s-mode is built on top of comint (the general
;;; command interpreter mode written by Olin Shivers), and so
;;; comint.el (or comint.elc) should be either loaded or in your
;;; load path when you invoke it.  You might want to put
;;; something like the following in your .emacs file:
;;; 
;;;     (autoload 'S "~/elisp/S" "" t)
;;; 
;;; where "~/elisp/S.el" is the path name of this file.  That
;;; way, all you will have to do to get S running is to type
;;; "M-x S" from within emacs.
;;; 
;;; Aside from the general features offered by comint such as
;;; command history editing and job control, inferior S mode
;;; allows you to dump and load S objects into and from external
;;; files, and to display help on functions.  It also provides
;;; name completion while you do these.  For more detailed
;;; information see the documentation strings for S,
;;; inferior-S-mode, S-mode, and comint-mode.  There are also
;;; many variables and hooks available for customizing (see
;;; the variables below that have document strings that start
;;; with an "*").

;;; GETTING LATER RELEASES OF S MODE
;;; The latest version is available from statlib by sending a
;;; blank message with subject "send index from S" to
;;; statlib@stat.cmu.edu, and following the directions from
;;; there.  Comint is probably already available at your site, 
;;; and already in your load path.  If it is not, you can get it
;;; from archive.cis.ohio-state.edu (login anonymous, passwd id)
;;; in directory /pub/gnu/emacs/elisp-archive/as-is/comint.el.Z
;;; This version has been tested and works with (at least) 
;;; comint-version 2.02.  You probably have copies of comint.el 
;;; on your system.  Copies are also available from ritter@cs.cmu.edu,
;;; and shivers@cs.cmu.edu.


;;; RELEASE 2.1 INFORMATION
;;;
;;; Improvements since last release (unnumbered of Summer 1990):
;;; * Better description provided of functions loaded.
;;; * Better header for this file.
;;; * S-directory is now a prescriptive rather than just 
;;;   descriptive variable.  
;;; * better syntax table, so |#; are better recognized and
;;;   commands using them work better.
;;; * we have a version number.
;;;
;;; Remaining Bugs:
;;; 
;;; * Inferior S mode doesn't do a very good job of offering
;;;   defaults when it prompts for names and it is often not 
;;;   wise to accept them even when they look right.
;;; * It would be nice to use .Last.value when running S+
;;; * It would be nice to use S VERSION when running S+

;;; Inits and provides
;;;=====================================================
;;;

(require 'comint)
(provide 'S)

;; this will appear for just a short while, but it's a
;; chance to teach...
(message "Type ^h m for help on s-mode.")

(defvar S-mode-version "2.1" 
  "Version of S-mode currently loaded.")

;;; User changable variables
;;;=====================================================
;;; Users note: Variables with document strings starting
;;; with a * are the ones you can generally change safely, and
;;; may have to upon occasion.

(defvar inferior-S-program "S"
  "*Program name for invoking an inferior S.")

(defvar explicit-S-args nil
  "*String of arguments passed to the S process on startup if the name of
the S program is `S'.")

(defvar inferior-S-prompt "^\\(\\+\\|[^>]*>\\) *"
  "*The regular expression inferior S mode uses for recognizing prompts")

(defvar S-scratch-file nil
  "*The name of the scratch source file that receives dumped objects.")

(defvar S-scratch-directory (file-name-as-directory "/tmp")
  "*The directory inferior S puts the scratch source files into.  It
must end in a slash.")

(defvar S-directory (file-name-as-directory (getenv "HOME"))
  "*The directory S is run from.  It must end in a slash.")

(defvar S-mode-hook '()
  "*Hook for customizing S mode each time it is entered.")

(defvar S-mode-load-hook '()
  "*Hook to call each time S mode is loaded.")

(defvar S-pre-run-hook nil
  "*Hook to call before starting up S, good for setting up your directory.")

;; You can put something like:
;; (setq S-directory (file-name-as-directory (concat (getenv "HOME") "/S"))))
;; in your ~/.emacs file and S will always start up in your ~/S directory.
;; Alternatively, you can get S to start up in the directory you start 
;; Emacs from by putting this in your .emacs:
;; (setq S-pre-run-hook '((lambda () (setq S-directory default-directory))))

;(defvar use-S-set-dir-file! t 
;  "*If t (default), calling S or run-S sets up a shell file, that sets (cd's)
;the directory for S, and then calls the inferior-S-program.  If nil, just
;calls the inferior-S-program.  If you are already calling a shell script 
;to start up S, this should not hurt you either way.")




;;; System variables
;;;=====================================================
;;; Users note: You will rarely have to change these 
;;; variables.

(defvar S-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|collection(\\|library(\\)"
  "The regular expression for matching the S commands that change the
search path.")

(defvar S-function-pattern
  "^[^\n\t]*\\(<-\\|_\\)[ \n]*function[ \t]*("
  "The regular expression for matching the beginning of an S
function.")

(defvar S-source-modes '(S-mode)
  "A list of modes used to determine if a buffer contains S source code.
If a file is loaded into a buffer that is in one of these major modes, it
is considered an S source file.  The function S-load-file uses this to
determine defaults.")

(defvar inferior-S-load-command "source(\"%s\")\n"
  "Format-string for building the S command to load a file.
This format string should use %s to substitute a file name
and should result in an S expression that will command the inferior S
to load that file.")

(defvar inferior-S-dump-command "dump(\"%s\",file=\"%s\")\n"
  "Format-string for building the S command to dump an object into a file.
This format string should use %s to substitute an object and a file name.")

(defvar inferior-S-help-command "help(\"%s\")\n"
  "Format-string for building the S command to ask for help on an object.
This format string should use %s to substitute an object name.")

(defvar inferior-S-search-list-command "search()\n"   
  "S command that prints out the search list---the list of
directories and (recursive) objects that S uses when it searches for
objects.")
;changed from "attach()\n"

(defvar S-search-list nil
  "The list of directories and (recursive) objects to search for S
objects.")

(defvar S-sp-change nil
  "This symbol flags a change in the S search path.")

(defvar S-prev-load-dir/file nil
  "This symbol saves the (directory . file) pair used in the last
S-load-file command.  Used for determining the default in the next one.")

;(defvar internal-S-file-name ".run-s" 
;  "This is the file name where S sets up the shell script to set the directory
;and run S.")
;
;(defvar modified-S-program nil 
;  "The name for S that includes the change directory if requested.")

(defvar internal-S-file-name-header 
  "# This file is how s-mode calls S.  It is rebuilt each time run-s or S 
# is called by s-mode.  You may delete it if you wish.")


;;; S-mode helper functions and code
;;;=====================================================
;;;

(defvar inferior-S-mode-map nil)
(if inferior-S-mode-map
    nil
  (setq inferior-S-mode-map (full-copy-sparse-keymap comint-mode-map))
  (define-key inferior-S-mode-map "\r" 'S-send-input)
  (define-key inferior-S-mode-map "\C-cl" 'S-load-file)
  (define-key inferior-S-mode-map "\C-cd" 'S-dump-object-into-scratch)
  (define-key inferior-S-mode-map "\C-ch" 'S-display-help-on-object))

(defvar S-mode-syntax-table nil "Syntax table for S-mode.")
(if S-mode-syntax-table
    nil
  (setq S-mode-syntax-table (make-syntax-table c-mode-syntax-table))
  (modify-syntax-entry ?# "<" S-mode-syntax-table)  ; now an open comment
  (modify-syntax-entry ?\n ">" S-mode-syntax-table) ; close comment
  (modify-syntax-entry ?_ "." S-mode-syntax-table)  
  (modify-syntax-entry ?* "." S-mode-syntax-table)
  (modify-syntax-entry ?/ "." S-mode-syntax-table))

(defvar inferior-S-mode-hook '()
  "*Hook for customizing inferior S mode")

(defun S ()
  "Run an inferior S process, input and output via buffer *S*.
If there is a process already running in *S*, just switch to that buffer.
Takes the program name from the variable inferior-S-program.
The S program name is used to make a symbol name such as `explicit-S-args'.
If that symbol is a variable its value is used as a string of arguments
when invoking S.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive)
  (if (not (comint-check-proc "*S*"))
      (let* ((symbol-string
              (concat "explicit-" inferior-S-program "-args"))
             (switches-symbol (intern-soft symbol-string))
             (switches
              (if (and switches-symbol (boundp switches-symbol))
                  (symbol-value switches-symbol))))
        ;(if use-S-set-dir-file!
        ;    (make-s-run-file)
        ;    (setq modified-S-program inferior-S-program))
        (set-buffer
         (if switches
             (make-S-comint switches)
           (make-S-comint)))
        (inferior-S-mode)
        (wait-for-S-prompt)
        (goto-char (point-max))
        (get-S-search-list)))
  (pop-to-buffer "*S*"))

;;; define two commands consistent with other comint modes, run-s &
;;; run-S.
(fset 'run-s (fset 'run-S (symbol-function 'S)))

;;; this is inelegant, but will work for now
;(defun make-s-run-file ()
;  (run-hooks 'S-pre-run-hook)
;  (setq file-name
;        (concat (file-name-as-directory (getenv "HOME")) internal-S-file-name))
;  (if (file-readable-p file-name)
;      (delete-file file-name))
;  (switch-to-buffer internal-S-file-name)
;  (erase-buffer)
;  (insert "#!/bin/sh\n")
;  (insert internal-S-file-name-header)
;  (insert "\n# so cd to the user's S-directory")
;  (insert (format "\ncd %s" S-directory))
;  (insert "\n# and then run his/her inferior-S-program")
;  (insert (format "\n%s\n" inferior-S-program))
;  (write-file file-name)
;  (kill-buffer internal-S-file-name)
;  (shell-command (format "chmod +x %s" file-name))
;  (setq modified-S-program file-name))

(defun inferior-S-mode () 
  "Major mode for interacting with an inferior S process.  
Runs an S interactive job as a subprocess of Emacs, with I/O through an
Emacs buffer.  Variable inferior-S-program controls which S
is run.

\\{inferior-S-mode-map}

Do not type \\[S-dump-object-into-scratch] or \\[S-display-help-on-object]
when you are in the middle of delivering a multi-line command to S and S is
prompting you with its secondary prompt. It's ok to do this if S hasn't
seen the initial lines yet---that is, if you ended those lines with
something other than a \"send input\" command (usually bound to RETURN).

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-S-mode-hook (in that order).

You can send text to the inferior S process from other buffers containing
S source. The key bindings of these commands can be found by typing 
^h m (help for mode) in the other buffers.
    S-eval-region sends the current region to the S process.
    S-eval-buffer sends the current buffer to the S process.
    S-eval-function sends the current function to the S process.
    S-eval-line sends the current line to the S process.
    beginning-of-S-function and end-of-S-function move the point to
        the beginning and end of the current S function.
    switch-to-S switches the current buffer to the S process buffer.
    switch-to-end-of-S switches the current buffer to the S process
        buffer and puts point at the end of it.

    S-eval-region-and-go, S-eval-buffer-and-go,
        S-eval-function-and-go, and S-eval-line-and-go switch to the S
        process buffer after sending their text.

    S-dump-object-into-scratch moves an S object into a temporary file
        and buffer for editing
    S-load-file sources a file of commands to the S process.

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
  (setq comint-prompt-regexp inferior-S-prompt)
  (setq major-mode 'inferior-S-mode)
  (setq mode-name "Inferior S")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-S-mode-map)
  (set-syntax-table S-mode-syntax-table)
  (setq comint-input-sentinel 'S-search-path-tracker)
  (run-hooks 'inferior-S-mode-hook))

;;; This function is a modification of make-comint from the comint.el
;;; code of Olin Shivers.
(defun make-S-comint (&rest switches)
  (let* ((name "S")
         (buffer (get-buffer-create (concat "*" name "*")))
         (proc (get-buffer-process buffer)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode. Otherwise, leave buffer and existing process alone.
    (cond ((or (not proc) (not (memq (process-status proc) '(run stop))))
           (save-excursion
             (set-buffer buffer)
             (setq default-directory S-directory)
             (comint-mode)) ; Install local vars, mode, keymap, ...
           (comint-exec buffer name inferior-S-program nil switches)))
    buffer))

(defun S-send-input ()
  (interactive)
  (comint-send-input)
  (if (and S-sp-change
           (wait-for-S-prompt))
      (progn
        (get-S-search-list)
        (setq S-sp-change nil))))

(defun wait-for-S-prompt ()
  (let* ((cbuffer (current-buffer))
         (sprocess (get-process "S"))
         (sbuffer (process-buffer sprocess))
         r)
    (set-buffer sbuffer)
    (while (progn
             (accept-process-output sprocess)
             (goto-char (point-max))
             (beginning-of-line)
             (setq r (looking-at inferior-S-prompt))
             (not (or r (looking-at ".*\\?\\s *")))))
    (goto-char (point-max))
    (set-buffer cbuffer)
    (symbol-value r)))

(defun S-dump-object-into-scratch (object)
  "Dump the S object into a file (and buffer) for editing."
  (interactive (find-S-object "Object to edit: "))
  (let* ((filename (concat S-scratch-directory
                           (or S-scratch-file (make-temp-name "scr."))))
         (complete-dump-command (format inferior-S-dump-command
                                        object filename))
         old-scratch-buffer)
    (command-to-S complete-dump-command)
    (if (setq old-scratch-buffer (get-file-buffer filename))
        (kill-buffer old-scratch-buffer)) ;make sure we start fresh
    (find-file-other-window filename)
    (S-mode)
    (setq S-prev-load-dir/file
          (cons (file-name-directory filename)
                (file-name-nondirectory filename)))))

(defun find-S-object (p-string)
  (let* ((default (find-S-object-default))
         (prompt-string (if default
                            (format "%s(default %s) " p-string default)
                          p-string))
         (S-object-list (get-S-object-list))
         (spec (completing-read prompt-string S-object-list)))
    (list (cond
           ((string= spec "") default)
           (t spec)))))

(defun find-S-object-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s.")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s." nil t)
        (progn (forward-char 1)
               (buffer-substring (point)
                                 (progn (forward-sexp -1)
                                        (while (looking-at "\\s'")
                                          (forward-char 1))
                                        (point))))
      nil)))

(defun get-S-search-list ()
  "Get the list of directories and (recursive) objects that S searches
when it looks for objects."
  (let ((tbuffer (generate-new-buffer "search-list"))
        dir)
    (setq S-search-list nil)
    (buffer-flush-undo tbuffer)
    (set-buffer tbuffer)
    (command-to-S inferior-S-search-list-command tbuffer)
    (goto-char (point-max))
    (while (re-search-backward "\"\\([^\"]*\\)\"" nil t)
      (setq dir (buffer-substring (match-beginning 1) (match-end 1)))
      (if (string-match "^[^/]" dir)
          (setq dir (concat S-directory dir)))
      (setq S-search-list (cons dir S-search-list)))
    (kill-buffer tbuffer)))

(defun get-S-object-list ()
  "Return the alist of current S object names (suitable for use with
completing-read)."
  (get-S-object-list-r S-search-list))

(defun get-S-object-list-r (s-list)
  "Return the alist of current S object names, recursive version.  S-LIST
is the search list of directories (or objects) for S." 
  (let* ((dir (car s-list))
         (dir-list (cdr s-list)))
    (if (null dir)
        nil
      (append (if (file-directory-p dir)
                  (mapcar 'list (directory-files dir)))
              (get-S-object-list-r dir-list)))))

;(defun command-to-S (com &optional buf)
;  "Send the S process a COMMAND and delete the output from the S process
;buffer.  If an optional second argument BUFFER exists save the output
;there. (BUFFER cannot be the S process buffer.)"
;  (let* ((cbuffer (current-buffer))
;         (sprocess (get-process "S"))
;         (sbuffer (process-buffer sprocess))
;         place-holder
;         last)
;    (set-buffer sbuffer)
;    (setq place-holder (point-marker))
;    (kill-region (process-mark sprocess) (point-max))
;    (setq last (point-max))
;    (process-send-string sprocess com)
;    (while (progn
;             (accept-process-output sprocess)
;             (goto-char (point-max))
;             (beginning-of-line)
;             (or (= (point-max) last)
;                 (not (looking-at inferior-S-prompt)))))
;    (if buf
;        (append-to-buffer buf last (point)))
;    (delete-region last (point-max))
;    (yank)                              ;possible command in process
;    (goto-char (marker-position place-holder))
;    (set-buffer cbuffer)))

;; revised form that uses a register
(defun command-to-S (com &optional buf)
  "Send the S process a COMMAND and delete the output from the S process
buffer.  If an optional second argument BUFFER exists save the output
there. (BUFFER cannot be the S process buffer.)"
  (let* ((cbuffer (current-buffer))
         (sprocess (get-process "S"))
         (sbuffer (process-buffer sprocess))
         place-holder
         last)
    (set-buffer sbuffer)
    (setq place-holder (point-marker))
    ;; copy partial, unentered code to somewhere safer, deleting it for now
    (copy-to-register ?S (process-mark sprocess) (point-max) t) 
    (setq last (point-max))
    (process-send-string sprocess com)
    (while (progn
             (accept-process-output sprocess)
             (goto-char (point-max))
             (beginning-of-line)
             (or (= (point-max) last)
                 (not (looking-at inferior-S-prompt)))))
    (if buf
        (append-to-buffer buf last (point)))
    (delete-region last (point-max))
    (if (get-register ?S)
        (insert-register ?S))                  ;possible command in process
    (goto-char (marker-position place-holder))
    (set-buffer cbuffer)))

(defun S-display-help-on-object (object)
  "Display the help page for OBJECT in the *Help* buffer."
  (interactive (find-S-object "Help on: "))
  (let (tbuffer)
    (pop-to-buffer "*Help*")
    (setq tbuffer (current-buffer))
    (delete-region (point-min) (point-max))
    (command-to-S (format inferior-S-help-command object) tbuffer)
    (nuke-S-help-bs)
    (goto-char (point-min))))

;;; This function is a modification of nuke-nroff-bs in man.el from the
;;; standard emacs 18 lisp library.
(defun nuke-S-help-bs ()
  (interactive "*")
  ;; Nuke underlining and overstriking (only by the same letter)
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let* ((preceding (char-after (- (point) 2)))
           (following (following-char)))
      (cond ((= preceding following)
             ;; x\bx
             (delete-char -2))
            ((= preceding ?\_)
             ;; _\b
             (delete-char -2))
            ((= following ?\_)
             ;; \b_
             (delete-region (1- (point)) (1+ (point)))))))
  ;; Crunch blank lines
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n\n*" nil t)
    (replace-match "\n\n"))
  ;; Nuke blanks lines at start.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point)))

(defun S-load-file (filename)
  "Load an S source file into an inferior S process."
  (interactive (comint-get-source "Load S file: "
                                  S-prev-load-dir/file
                                  S-source-modes
                                  nil))
  (comint-check-source filename)
  (setq S-prev-load-dir/file
        (cons (file-name-directory filename)
              (file-name-nondirectory filename)))
  (process-send-string "S" (format inferior-S-load-command
                                   filename))
  (switch-to-S t))

(defun S-search-path-tracker (str)
  "This function monitors user input to the inferior S process so that
emacs can keep the S-search-list up to date.  Completing-read uses this
list indirectly when it prompts for help or for an object to dump."
  (if (string-match S-change-sp-regexp str)
      (setq S-sp-change t)))



;;; S mode
;;;======================================================
;;;

(defvar S-mode-map nil)
(if S-mode-map
    nil
  (setq S-mode-map (make-sparse-keymap))
  (define-key S-mode-map "\C-cr"    'S-eval-region)
  (define-key S-mode-map "\C-c\C-r" 'S-eval-region-and-go)
  (define-key S-mode-map "\C-cb"    'S-eval-buffer)
  (define-key S-mode-map "\C-c\C-b" 'S-eval-buffer-and-go)
  (define-key S-mode-map "\C-ce"    'S-eval-function)
  (define-key S-mode-map "\M-\C-x"  'S-eval-function)
  (define-key S-mode-map "\C-c\C-e" 'S-eval-function-and-go)
  (define-key S-mode-map "\C-ck"    'S-eval-line)
  (define-key S-mode-map "\C-c\C-k" 'S-eval-line-and-go)
  (define-key S-mode-map "\M-\C-a"  'beginning-of-S-function)
  (define-key S-mode-map "\M-\C-e"  'end-of-S-function)
  (define-key S-mode-map "\C-cz"    'switch-to-S)
  (define-key S-mode-map "\C-c\C-z" 'switch-to-end-of-S)
  (define-key S-mode-map "\C-cl"    'S-load-file)
  (define-key S-mode-map "\C-ch"    'S-display-help-on-object))

(defun S-mode ()
  "Major mode for editing S source.

\\{S-mode-map}

Customization: Entry to this mode runs the hooks in S-mode-hook.

You can send text to the inferior S process from other buffers containing
S source.
    S-eval-region sends the current region to the S process.
    S-eval-buffer sends the current buffer to the S process.
    S-eval-function sends the current function to the S process.
    S-eval-line sends the current line to the S process.
    beginning-of-S-function and end-of-S-function move the point to
        the beginning and end of the current S function.
    switch-to-S switches the current buffer to the S process buffer.
    switch-to-end-of-S switches the current buffer to the S process
        buffer and puts point at the end of it.

    S-eval-region-and-go, S-eval-buffer-and-go,
        S-eval-function-and-go, and S-eval-line-and-go switch to the S
        process buffer after sending their text.

    S-load-file sources a file of commands to the S process.
    make-S-function inserts a function template in the buffer."

  (interactive)
  (setq major-mode 'S-mode)
  (setq mode-name "S")
  (use-local-map S-mode-map)
  (set-syntax-table S-mode-syntax-table)
  (run-hooks 'S-mode-hook))

;;; Emacs will set the mode for a file based on the file's header.
;;; The mode name is indicated by putting it between -*- on the top line. 
;;; (Other commands can go here too, see an Emacs manual.)
;;; For a file you also load, you will want a leading # (comment to S)
;;; Emacs will downcase the name of the mode, e.g., S, so we must provide
;;; s-mode in lower case too.  That is, "#-*-S-*-" invokes s-mode and not S-mode.
(fset 's-mode 'S-mode)

(defun S-eval-region (start end)
  "Send the current region to the inferior S process."
  (interactive "r")
  (process-send-region "S" start end)
  (process-send-string "S" "\n"))

(defun S-eval-region-and-go (start end)
  "Send the current region to the inferior S and switch to the process
buffer."
  (interactive "r")
  (S-eval-region start end)
  (switch-to-S t))

(defun S-eval-buffer ()
  "Send the current buffer to the inferior S process."
  (interactive)
  (S-eval-region (point-min) (point-max)))

(defun S-eval-buffer-and-go ()
  "Send the current buffer to the inferior S and switch to the process
buffer."
  (interactive)
  (S-eval-buffer)
  (switch-to-S t))

(defun S-eval-function ()
  "Send the current function to the inferior S process."
  (interactive)
  (save-excursion
    (end-of-S-function)
    (let ((end (point)))
      (beginning-of-S-function)
      (message (concat "Loading: " (extract-word-name)))
      (S-eval-region (point) end))))

(defun S-eval-function-and-go ()
  "Send the current function to the inferior S process and switch to
the process buffer."
  (interactive)
  (S-eval-function)
  (switch-to-S t))

(defun S-eval-line ()
  "Send the current line to the inferior S process."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (message (concat "Loading line: " (extract-word-name) "..."))
      (S-eval-region (point) end))))

(defun S-eval-line-and-go ()
  "Send the current line to the inferior S process and switch to the
process buffer."
  (interactive)
  (S-eval-line)
  (switch-to-S t))

(defun beginning-of-S-function (&optional recurse)  
  "Leave the point at the beginning of the current S function."
  (interactive)
  (let ((cp (point)))
    (end-of-line) 
    (re-search-backward S-function-pattern (point-min) t)
    (beginning-of-line)  
    (if (and (not (bobp))   
             (not recurse)
             (= (point) cp)) ;you are where you were, at the end
        (progn (re-search-backward S-function-pattern (point-min) t)
               (beginning-of-S-function t)))))

(defun end-of-S-function (&optional recursive)
  "Leave the point at the end of the current S function."
  (interactive)
  (let ((cp (point)))
    (beginning-of-S-function t)
    (re-search-forward "{" (point-max) t)
    (forward-char -1)
    (forward-sexp)
    (if (and (not (eobp))
             (not recursive)
             (= (point) cp)) ;you are where you were, at the end
        (progn (re-search-forward S-function-pattern (point-max) t)
               (end-of-S-function t)))))

(defun extract-word-name ()
  "Get the word you're on."
  (interactive)
  (save-excursion
    (buffer-substring
     (point)
     ;; search until you get a non-word character
     (progn (re-search-forward "[ <_=#-*&]") (point)))))

(defun switch-to-S (eob-p)
  "Switch to the inferior S process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (cond ((comint-check-proc "*S*")
         (pop-to-buffer "*S*")
         (cond (eob-p
                (push-mark)
                (goto-char (point-max)))))
        (t
         (message "No inferior S process")
         (ding))))

(defun switch-to-end-of-S (eob-p)
  "Switch to the end of the inferior S process buffer."
  (interactive "P")
  (switch-to-S t))

(defun make-S-function ()
  "Insert a function template."
  (interactive)
  (insert "fu <- function()\n{\n\t\n}\n")
  (forward-line -2)
  (end-of-line))

(run-hooks 'S-mode-load-hook)


;;; Revision notes:
;;  Release 2.1 on October 14, 1991 to statlib@stat.cmu.edu, 
;;     and to the elisp archives at OSU (brennan@dg-rtp.dg.com (Dave Brennan))
;;  and announced on internet.s-news, netnews.gnu.emacs.sources, & 
;;    andrew.programs.S
;; -------------------------------------------------------
;;     Jul 26          1991  Frank Ritter
;;   * added S-mode-load-hook & S-pre-run-hook
;;     and testing by neilc@research.att.com
;;     Jul 9           1991  Frank Ritter
;;   * Changed command-to-S to use a register rather than 
;;       the kill ring.
;;   * Better file header, comments now at 60 col so 
;;       mailers wont' eat them.
;;   * Better extract-word-name.
;;   * Added S-mode-version variable
;;   * Changed syntax table to read |#; appropriately
;;
;; Wed Nov 28 11:03:50 1990  Ed Kademan  (kademan at hermes)
;;   * Make the S-mode-syntax-table a slightly modified
;;       version of the c-mode-syntax-table instead of a
;;       version of the one for lisp.
;; 
;; Sat Nov 10 12:41:52 1990  Ed Kademan  (kademan at hermes)
;;   * Made run-S and run-s commands synonymous with the
;;       function S.
;; 
;; Fri Oct 19 12:41:52 1990  Ed Kademan  (kademan at hermes)
;;   * Made S-directory a user modifiable variable.  S will
;;       run from that directory.
;; 
;; Thu Oct 18 12:41:52 1990  Ed Kademan  (kademan at hermes)
;;   * Added function nuke-S-help-bs to clean up nroff
;;       style text in the S help buffer.  This function is
;;       a modification of nuke-nroff-bs from man.el.
;; -------------------------------------------------------
;; Unnumbered version released dated Thu Jun 14 09:56:56 CDT 1990

