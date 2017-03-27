;;; ess-utils.el --- General Emacs utility functions used by ESS

;; Copyright (C) 1998--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2017 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: Martin Maechler <maechler@stat.math.ethz.ch>
;; Created: 9 Sept 1998
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS (Emacs Speaks Statistics).

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Code:

(eval-when-compile
  (require 'tramp)
  ;; We can't use cl-lib whilst supporting Emacs <= 24.2 users
  (with-no-warnings (require 'cl)))


;;*;; Internal ESS tools and variables

(defvar ess-lisp-directory
  (directory-file-name (file-name-directory (locate-library "ess-site")))
  "Directory containing ess-site.el(c) and other ESS lisp files.")

(defvar ess-etc-directory nil
  "Location of the ESS etc/ directory.
The ESS etc directory stores various auxillary files that are useful
for ESS, such as icons.")

(defvar ess-etc-directory-list
  '("../etc/ess/" "../etc/" "../../etc/ess/" "./etc/")
  "List of directories, relative to `ess-lisp-directory', to search for etc.")

(while (and (listp ess-etc-directory-list) (consp ess-etc-directory-list))
  (setq ess-etc-directory
        (expand-file-name (concat ess-lisp-directory "/"
                                  (car ess-etc-directory-list))))
  (if (file-directory-p ess-etc-directory)
      (setq ess-etc-directory-list nil)
    (setq ess-etc-directory nil)
    (setq ess-etc-directory-list (cdr ess-etc-directory-list))
    (when (null ess-etc-directory-list)
      (beep 0) (beep 0)
      (message (concat
                "ERROR:ess-site.el:ess-etc-directory\n"
                "Relative to ess-lisp-directory, one of the following must exist:\n"
                "../etc/ess, ../etc, ../../etc/ess or ./etc"))
      (sit-for 4))))

(defun ess-message (format-string &rest args)
  "Shortcut for \\[message] only if `ess-show-load-messages' is non-nil."
  (when (bound-and-true-p ess-show-load-messages)
    (message format-string args)))


;;*;; elisp tools

(defun ess-goto-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ess-line-end-position (&optional N)
  "return the 'point' at the end of N lines. N defaults to 1, i.e., current line."
  (save-excursion
    (end-of-line N)
    (point)))

(defun ess-search-except (regexp &optional except backward)
  "Search for a regexp, store as match 1, optionally ignore
strings that match exceptions."
  (interactive)

  (let ((continue t) (exit nil))

    (while continue
      (if (or (and backward (search-backward-regexp regexp nil t))
              (and (not backward) (search-forward-regexp regexp nil t)))
          (progn
            (setq exit (match-string 1))
            (setq continue (and except (string-match except exit)))
            (if continue (setq exit nil)))
        ;;else
        (setq continue nil))
      )

    exit))

(defun ess-save-and-set-local-variables ()
  "If buffer was modified, save file and set Local Variables if defined.
Return t if buffer was modified, nil otherwise."
  (interactive)

  (let ((ess-temp-point (point))
        (ess-temp-return-value (buffer-modified-p)))
    ;; if buffer has changed, save buffer now (before potential revert)
    (if ess-temp-return-value (save-buffer))

    ;; If Local Variables are defined, update them now
    ;; since they may have changed since the last revert
    ;;  (save-excursion
    (beginning-of-line -1)
    (save-match-data
      (if (search-forward "End:" nil t) (revert-buffer t t)))
    ;; save-excursion doesn't save point in the presence of a revert
    ;; so you need to do it yourself
    (goto-char ess-temp-point)

    ess-temp-return-value))

(defun ess-get-file-or-buffer (file-or-buffer)
  "Return file-or-buffer if it is a buffer; otherwise return the buffer
associated with the file which must be qualified by it's path; if the
buffer does not exist, return nil."
  (interactive)

  (if file-or-buffer
      (if (bufferp file-or-buffer) file-or-buffer
        (find-buffer-visiting file-or-buffer))))

(defun ess-set-local-variables (alist &optional file-or-buffer)
  "Set local variables from ALIST in current buffer; if file-or-buffer
is specified, perform action in that buffer."
  (interactive)
  (if file-or-buffer (set-buffer (ess-get-file-or-buffer file-or-buffer)))

  (mapcar (lambda (pair)
            (make-local-variable (car pair))
            (set (car pair) (eval (cdr pair))))
          alist))

(defun ess-clone-local-variables (from-file-or-buffer
                                  &optional to-file-or-buffer)
  "Clone local variables from one buffer to another buffer."
  (interactive)
  (ess-set-local-variables
   (ess-sas-create-local-variables-alist from-file-or-buffer)
   to-file-or-buffer))

(defun ess-return-list (ess-arg)
  "Given an item, if it is a list return it, else return item in a list."
  (if (listp ess-arg) ess-arg (list ess-arg)))

;; Copyright (C) 1994 Simon Marshall.
;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; LCD Archive Entry:
;; unique|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Functions and commands to uniquify lists or buffer text (cf. sort).
;; 23-Apr-1994|1.00|~/packages/unique.el.Z|
;;
;; MM: renamed from 'unique' to 'ess-unique', then
(defun ess-uniq (list predicate)
  "Uniquify LIST, stably, deleting elements using PREDICATE.
Return the list with subsequent duplicate items removed by side effects.
PREDICATE is called with an element of LIST and a list of elements from LIST,
and should return the list of elements with occurrences of the element removed.
This function will work even if LIST is unsorted.  See also `ess-uniq-list'."
  (let ((list list))
    (while list
      (setq list (setcdr list (funcall predicate (car list) (cdr list))))))
  list)

(defun ess-uniq-list (items)
  "Delete all duplicate entries in ITEMS list, calling `ess-uniq'."
  (ess-uniq items 'delete))

(defun ess-flatten-list (&rest list)
  "Take the arguments and flatten them into one long list.
Drops 'nil' entries."
  ;; Taken from lpr.el
  ;; `lpr-flatten-list' is defined here (copied from "message.el" and
  ;; enhanced to handle dotted pairs as well) until we can get some
  ;; sensible autoloads, or `flatten-list' gets put somewhere decent.

  ;; (ess-flatten-list '((a . b) c (d . e) (f g h) i . j))
  ;; => (a b c d e f g h i j)
  (ess-flatten-list-1 list))

(defun ess-flatten-list-1 (list)
  (cond
   ((null list) (list))
   ((consp list)
    (append (ess-flatten-list-1 (car list))
            (ess-flatten-list-1 (cdr list))))
   (t (list list))))

(defun ess-delete-blank-lines ()
  "Convert 2 or more lines of white space into one."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while (search-forward-regexp "^[ \t]*\n[ \t]*\n" nil t)
        ;;(goto-char (match-beginning 0))
        (delete-blank-lines)))))

;; Parse a line into its constituent parts (words separated by
;; whitespace).    Return a list of the words.
;; Taken from rlogin.el, from the comint package, from XEmacs 20.3.
(defun ess-line-to-list-of-words (line)
  (if (listp line)
      line
    (let ((list nil)
          (posn 0))
      ;; (match-data (match-data)))
      (while (string-match "[^ \t\n]+" line posn)
        (setq list (cons (substring line (match-beginning 0) (match-end 0))
                         list))
        (setq posn (match-end 0)))
      (store-match-data (match-data))
      (nreverse list))))


;;*;; System

(defun ess-revert-wisely ()
  "Revert from disk if file and buffer last modification times are different."
  (interactive)

                                        ; whether or not a revert is needed, force load local variables
                                        ; for example, suppose that you change the local variables and then
                                        ; save the file, a revert is unneeded, but a force load is
  (hack-local-variables)

  (if (not (verify-visited-file-modtime (current-buffer))) (progn
                                                             (let ((ess-temp-store-point (point)))
                                                               (revert-buffer t t)
                                                               (goto-char ess-temp-store-point))
                                                             t)
    nil))

(defun ess-kermit-get (&optional ess-file-arg ess-dir-arg)
  "Get a file with Kermit.  WARNING:  Experimental!  From your *shell*
buffer, start kermit and then log in to the remote machine.  Open
a file that starts with `ess-kermit-prefix'.  From that buffer,
execute this command.  It will retrieve a file from the remote
directory that you specify with the same name, but without the
`ess-kermit-prefix'."

  (interactive)

  ;;     (save-match-data
  (let ((ess-temp-file (if ess-file-arg ess-file-arg (buffer-name)))
        (ess-temp-file-remote-directory ess-dir-arg))

    (if (string-equal ess-kermit-prefix (substring ess-temp-file 0 1))
        (progn
          ;; I think there is a bug in the buffer-local variable handling in GNU Emacs 21.3
          ;; Setting ess-kermit-remote-directory every time is somehow resetting it to the
          ;; default on the second pass.  So, here's a temporary work-around.  It will fail
          ;; if you change the default, so maybe this variable should not be customizable.
          ;; In any case, there is also trouble with local variables in XEmacs 21.4.9 and
          ;; 21.4.10.  XEmacs 21.4.8 is fine.
          (if ess-temp-file-remote-directory
              (setq ess-kermit-remote-directory ess-temp-file-remote-directory)

            (if (string-equal "." ess-kermit-remote-directory)
                (setq ess-kermit-remote-directory (read-string "Remote directory to transfer file from: "
                                                               ess-kermit-remote-directory))))

          (setq ess-temp-file-remote-directory ess-kermit-remote-directory)
          ;;        (setq ess-temp-file (substring ess-temp-file (match-end 0)))
          (ess-sas-goto-shell)
          (insert "cd " ess-temp-file-remote-directory "; " ess-kermit-command " -s "
                  (substring ess-temp-file 1) " -a " ess-temp-file)
          (comint-send-input)
          ;;          (insert (read-string "Press Return to connect to Kermit: " nil nil "\C-\\c"))
          ;;        (comint-send-input)
          ;;        (insert (read-string "Press Return when Kermit is ready to recieve: " nil nil
          ;;                (concat "receive ]" ess-sas-temp-file)))
          ;;        (comint-send-input)
          ;;        (insert (read-string "Press Return when transfer is complete: " nil nil "c"))
          ;;        (comint-send-input)
          (insert (read-string "Press Return when shell is ready: "))
          (comint-send-input)
          (switch-to-buffer (find-buffer-visiting ess-temp-file))
          (ess-revert-wisely)
          ))))

(defun ess-kermit-send ()
  "Send a file with Kermit.  WARNING:  Experimental!  From
a file that starts with `ess-kermit-prefix',
execute this command.  It will transfer this file to the remote
directory with the same name, but without the `ess-kermit-prefix'."

  (interactive)

  ;;     (save-match-data
  (let ((ess-temp-file (expand-file-name (buffer-name)))
        (ess-temp-file-remote-directory nil))

    (if (string-equal ess-kermit-prefix (substring (file-name-nondirectory ess-temp-file) 0 1))
        (progn
          ;; I think there is a bug in the buffer-local variable handling in GNU Emacs 21.3
          ;; Setting ess-kermit-remote-directory every time is somehow resetting it to the
          ;; default on the second pass.  Here's a temporary work-around.  It will fail
          ;; if you change the default, so maybe this variable should not be customizable.
          ;; In any case, there is also trouble with local variables in XEmacs 21.4.9 and
          ;; 21.4.10.  XEmacs 21.4.8 is fine.
          (if (string-equal "." ess-kermit-remote-directory)
              (setq ess-kermit-remote-directory (read-string "Remote directory to transfer file to: "
                                                             ess-kermit-remote-directory)))

          (setq ess-temp-file-remote-directory ess-kermit-remote-directory)

          ;;        (setq ess-temp-file (substring ess-temp-file (match-end 0)))
          (ess-sas-goto-shell)
          (insert "cd " ess-temp-file-remote-directory "; " ess-kermit-command " -a "
                  (substring (file-name-nondirectory ess-temp-file) 1) " -g "  ess-temp-file)
          (comint-send-input)
          ;;          (insert (read-string "Press Return to connect to Kermit: " nil nil "\C-\\c"))
          ;;        (comint-send-input)
          ;;        (insert (read-string "Press Return when Kermit is ready to recieve: " nil nil
          ;;                (concat "receive ]" ess-sas-temp-file)))
          ;;        (comint-send-input)
          ;;        (insert (read-string "Press Return when transfer is complete: " nil nil "c"))
          ;;        (comint-send-input)
          (insert (read-string "Press Return when shell is ready: "))
          (comint-send-input)
          (switch-to-buffer (find-buffer-visiting ess-temp-file))
          (ess-revert-wisely)
          ))))


(defun ess-find-exec (ess-root-arg ess-root-dir)
  "Given a root directory and the root of an executable file name,
find it's full name and path, if it exists, anywhere in the sub-tree."
  (let* ((ess-tmp-dirs (directory-files ess-root-dir t "^[^.]"))
         (ess-tmp-return (ess-find-exec-completions ess-root-arg ess-root-dir))
         (ess-tmp-dir nil))

    (while ess-tmp-dirs
      (setq ess-tmp-dir (car ess-tmp-dirs)
            ess-tmp-dirs (cdr ess-tmp-dirs))
      (if (file-accessible-directory-p ess-tmp-dir)
          (setq ess-tmp-return
                (nconc ess-tmp-return
                       (ess-find-exec ess-root-arg ess-tmp-dir)))))
    ess-tmp-return))

(defun ess-find-exec-completions (ess-root-arg &optional ess-exec-dir)
  "Given the root of an executable file name, find all possible completions.
Search for the executables in ESS-EXEC-DIR (which defaults to
`exec-path' if no value is given)."
  (let* ((ess-exec-path
          (if ess-exec-dir (ess-return-list ess-exec-dir) exec-path))
         (ess-tmp-exec nil)
         (ess-tmp-path-count (length ess-exec-path))
         (ess-tmp-dir nil)
         (ess-tmp-files nil)
         (ess-tmp-file nil))

    (while ess-exec-path
      (setq ess-tmp-dir (car ess-exec-path)
            ess-exec-path (cdr ess-exec-path))
      (when
          (and (> (length ess-tmp-dir) 0)
               (file-accessible-directory-p ess-tmp-dir))
        ;; the first test above excludes "" from exec-path, which can be
        ;; problematic with Tramp.
        (setq ess-tmp-files
              (file-name-all-completions ess-root-arg ess-tmp-dir))

        (while ess-tmp-files
          (setq ess-tmp-file
                (concat (file-name-as-directory ess-tmp-dir)
                        (car ess-tmp-files))
                ess-tmp-files (cdr ess-tmp-files))
          (if (and (file-executable-p ess-tmp-file)
                   (not (file-directory-p ess-tmp-file)))
              ;; we have found a possible executable, so keep it.
              (setq ess-tmp-exec
                    (nconc ess-tmp-exec (list ess-tmp-file)))))))
    ess-tmp-exec))

(defun ess-drop-non-directories (file-strings)
  "Drop all entries that do not \"look like\" directories."
  (ess-flatten-list (mapcar 'file-name-directory file-strings)))


;;*;; Interaction with inferiors

(defmacro ess-when-new-input (time-var &rest body)
  "BODY is evaluate only if the value of procss variable TIME-VAR
is bigger than the time of the last user input (stored in
'last-eval' process variable). TIME-VAR is the name of the
process variable which holds the access time. See the code for
`ess-synchronize-dirs' and `ess-cache-search-list'.

Returns nil when no current process, or process is busy, or
time-var > last-eval. Otherwise, execute BODY and return the last
value.

If BODY is executed, set process variable TIME-VAR
to (current-time).

Variable  *proc*  is bound  to  the  current process  during  the
evaluation of BODY.

Should be used in `ess-idle-timer-functions' which call the
process to avoid excessive requests.
"
  (declare (indent 1) (debug t))
  `(with-ess-process-buffer 'no-error
     (let ((le (process-get *proc* 'last-eval))
           (tv (process-get *proc* ',time-var)))
       (when (and (or (null tv) (null le) (time-less-p tv le))
                  (not (process-get *proc* 'busy)))
         (let ((out (progn ,@body)))
           (process-put *proc* ',time-var (current-time))
           out)))))

(defmacro ess-execute-dialect-specific (command &optional prompt &rest args)
  "Execute dialect specific command.

-- If command is nil issue warning 'Not available for dialect X'
-- If command is a elisp function, execute it with ARGS
-- If a string starting with 'http' or 'www', browse with `browse-url',
   otherwise execute the command in inferior process.
-- If a string, interpret as a command to subprocess, and
   substitute ARGS with `(format ,command ,@args).

When PROMPT is non-nil ask the user for a string value and
prepend the response to ARGS.

If prompt is a string just pass it to `read-string'. If a list, pass it
to `ess-completing-read'.
"
  `(if (null ,command)
       (message "Not implemented for dialect %s" ess-dialect)
     (let* ((com  (if (symbolp ,command)
                     (symbol-function ,command)
                   ,command))
            (prompt ',prompt)
            (resp (and prompt
                       (if (stringp  prompt)
                           (read-string  prompt)
                         (apply 'ess-completing-read prompt))))
            (args (append (list resp) ',args)))
       (cond ((functionp com)
              (apply com args))
             ((and (stringp com)
                   (string-match "^\\(http\\|www\\)" com))
              (setq com (apply 'format com args))
              (require 'browse-url)
              (browse-url com))
             ((stringp com)
              (unless (string-match "\n$" com)
                (setq com (concat com "\n")))
              (setq com (apply 'format com args))
              (ess-eval-linewise com))
             (t
              (error "Argument COMMAND must be either a function or a string"))))))

(defun ess--inject-code-from-file (file)
  ;; this is different from ess-load-file
  (let ((content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
    (when (string= ess-dialect "R")
      ;; don't detect intermediate prompts
      (setq content (concat "{" content "}\n")))
    (ess-command content)))

(defcustom ess-idle-timer-interval 1
  "Number of idle seconds to wait before running function in
  `ess-idle-timer-functions'."
  :group 'ess)

(defvar ess-idle-timer-functions nil
  "A list of functions to run each `ess-idle-timer-interval' idle seconds.

If your function calls the process, you better use
`ess-when-new-input' to wrap your call. If you call the
subprocess please respect `ess-can-eval-in-background' variable.

These functions are run with `run-hooks'. Use `add-hook' to add
symbols to this variable.

Most likely you will need a local hook. Then you should specify
the LOCAL argument to `add-hook' and initialise it in
`ess-mode-hook' or `ess-post-run-hook', or one of the more
specialised hooks `ess-r-post-run-hook',`ess-stata-post-run-hook'
etc.
")

(defun ess--idle-timer-function nil
  "Internal function executed by `ess--idle-timer'"
  ;; (while-no-input
  (run-hooks 'ess-idle-timer-functions))

(require 'timer)
(defvar ess--idle-timer
  (run-with-idle-timer ess-idle-timer-interval 'repeat 'ess--idle-timer-function)
  "Timer used to run `ess-idle-timer-functions'.")


;;*;; Emacs Integration

;;;*;;; Menus

(defun ess--generate-eval-visibly-submenu (menu)
  '(["yes" (lambda () (interactive) (setq ess-eval-visibly t))
     :style radio :enable t :selected (eq ess-eval-visibly t)]
    ["nowait" (lambda () (interactive) (setq ess-eval-visibly 'nowait))
     :style radio :enable t :selected (eq ess-eval-visibly 'nowait) ]
    ["no" (lambda () (interactive) (setq ess-eval-visibly nil))
     :style radio :enable t :selected (eq ess-eval-visibly nil) ]))

;;;*;;; Font Lock

(defun ess--extract-default-fl-keywords (keywords)
  "Extract the t-keywords from `ess-font-lock-keywords'."
  (delq nil (mapcar (lambda (c)
                      (when (cdr c) (symbol-value (car c))))
                    (if (symbolp keywords)
                        (symbol-value keywords)
                      keywords))))

(defun ess-font-lock-toggle-keyword (keyword)
  (interactive
   (list (intern (ess-completing-read
                  "Keyword to toggle"
                  (mapcar (lambda (el) (symbol-name (car el)))
                          (symbol-value ess-font-lock-keywords))
                  nil t))))
  (let* ((kwds (symbol-value (if (eq major-mode 'ess-mode)
                                 ess-font-lock-keywords
                               inferior-ess-font-lock-keywords)))
         (kwd (assoc keyword kwds)))
    (unless kwd (error "Keyword %s was not found in (inferior-)ess-font-lock-keywords list" keyword))
    (if (cdr kwd)
        (setcdr kwd nil)
      (setcdr kwd t))
    (let ((mode major-mode)
          (dialect ess-dialect)
          (fld (ess--extract-default-fl-keywords kwds)))
      ;; refresh font-lock defaults in all necessary buffers
      (mapc (lambda (b)
              (with-current-buffer b
                (when (and (eq major-mode mode)
                           (eq ess-dialect dialect))
                  (setcar font-lock-defaults fld)
                  (font-lock-refresh-defaults))))
            (buffer-list)))))

(defun ess--generate-font-lock-submenu (menu)
  "Internal, used to generate ESS font-lock submenu"
  (append (mapcar (lambda (el)
                    `[,(symbol-name (car el))
                      (lambda () (interactive)
                        (ess-font-lock-toggle-keyword ',(car el)))
                      :style toggle
                      :enable t
                      :selected ,(cdr el)])
                  (cond ((eq major-mode 'ess-mode)
                         (symbol-value ess-font-lock-keywords))
                        ((eq major-mode 'inferior-ess-mode)
                         (symbol-value inferior-ess-font-lock-keywords))))
          (list "-----"
                ["Save to custom" (lambda () (interactive)
                                    (let ((kwd (if (eq major-mode 'ess-mode)
                                                   ess-font-lock-keywords
                                                 inferior-ess-font-lock-keywords)))
                                      (customize-save-variable kwd (symbol-value kwd)))) t])))


;;;*;;; External modes

(defun ess-completing-read (prompt collection &optional predicate
                                   require-match initial-input hist def)
  "Read a string in the minibuffer, with completion.
Use `ido-completing-read' if IDO interface is present, or fall
back on classical `completing-read' otherwise. Meaning of
arguments is as in `completing-read' (PROMPT is automatically
suffixed with ': ' and (default %s) when needed). If HIST
is null use `ess--completing-hist' as history.

See also `ess-use-ido'."
  (let ((use-ido (and ess-use-ido (featurep 'ido))))
    (setq hist (or hist 'ess--completing-hist))
    (when (and def (not use-ido)) ;; ido places in front and highlights the default
      (setq prompt (format "%s(default %s)" prompt def)))
    (setq prompt (concat prompt ": "))
    (if use-ido
        (let ((reset-ido (and use-ido (not ido-mode))) ;people not using ido but having it)
              (ido-current-directory nil)
              (ido-directory-nonreadable nil)
              (ido-directory-too-big nil)
              (ido-context-switch-command 'ignore)
              (ido-enable-flex-matching ess-ido-flex-matching) ;it's fast and useful, may be get into options
              (ido-choice-list (copy-sequence collection)) ;ido removes the match (reported)
              sel)
          (unwind-protect
              (progn
                (ido-init-completion-maps)
                (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
                (add-hook 'choose-completion-string-functions 'ido-choose-completion-string)
                (setq sel (ido-read-internal 'list prompt hist def require-match initial-input))
                (when hist  ;; ido does not push into hist the whole match if C-SPC or RET is used (reported)
                  (unless (string= sel (car (symbol-value hist)))
                    (set hist (cons sel  (symbol-value hist))))))
            (when reset-ido
              (remove-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
              (remove-hook 'choose-completion-string-functions 'ido-choose-completion-string)))
          sel)
      ;; else usual completion
      (when (and (featurep 'xemacs) ;; xemacs workaround
                 (not (listp (car collection))))
        (setq collection (mapcar 'list collection)))
      (completing-read prompt collection predicate require-match initial-input hist def))))

(defun ess-load-extras (&optional inferior)
  "Load all the extra features depending on custom settings."

  (let ((mode (if inferior 'inferior-ess-mode 'ess-mode))
        (isR (string-match "^R" ess-dialect)))

    ;; auto-complete
    (when (and (boundp 'ac-sources)
               (if inferior
                   (eq ess-use-auto-complete t)
                 ess-use-auto-complete))
      (add-to-list 'ac-modes mode)
      ;; files should be in front; ugly, but needed
      (when ess-ac-sources
        (setq ac-sources
              (delq 'ac-source-filename ac-sources))
        (mapcar (lambda (el) (add-to-list 'ac-sources el))
                ess-ac-sources)
        (add-to-list 'ac-sources 'ac-source-filename)))

    ;; company
    (when (and (boundp 'company-backends)
               (if inferior
                   (eq ess-use-company t)
                 ess-use-company))
      (when ess-company-backends
        (set (make-local-variable 'company-backends)
             (copy-list (append ess-company-backends company-backends)))
        (delq 'company-capf company-backends)))

    ;; eldoc)
    (require 'eldoc)
    (when (and ess-eldoc-function ;; if mode provide this, it suports eldoc
               (or (and (not inferior) ess-use-eldoc)
                   (and inferior (eq ess-use-eldoc t))))
      (when (> eldoc-idle-delay 0.4) ;; default is too slow for paren help
        (set (make-local-variable 'eldoc-idle-delay) 0.1))
      (set (make-local-variable 'eldoc-documentation-function) ess-eldoc-function)
      (turn-on-eldoc-mode))

    ;; tracebug
    (when (and ess-use-tracebug inferior isR)
      (ess-tracebug 1))))

(defmacro ess--execute-electric-command (map &optional prompt wait exit-form &rest args)
  "Execute single-key comands defined in MAP till a key is pressed which is not part of map.

Return the value of the lastly executed command.

Single-key input commands are those that once executed do not
requre the prefix command for subsequent invocation.

PROMPT is passed to `read-event'.

If WAIT is t, wait for next input and ignore the keystroke which
triggered the command.

Each command in map should accept one at least one argument, the
most recent event (as read by `read-event'). ARGS are the
supplementary arguments passed to the commands.

EXIT-FORM should be supplied for a more refined control of the
read-even loop. The loop is exited when EXIT-FORM evaluates to
t. See examples in the tracebug code.
"
  ;;VS[09-06-2013]: check: it seems that set-temporary-overlay-map is designed
  ;;for this type of things; see also repeat.el package.
  `(let* ((ev last-command-event)
          (command (lookup-key ,map (vector ev)))
          out exit )
     (if (not (or ,wait command))
         (message "%s is undefined" (key-description (this-command-keys)))
       (unless ,wait
         (setq out (and command (funcall command ev ,@args))))
       (while (and (not exit)
                   (setq command
                         (lookup-key ,map
                                     (vector (setq ev (read-event ,prompt))))))
         (setq out (funcall command ev ,@args))
         (sleep-for .01)
         (setq exit ,exit-form))
       (unless exit ;; push only if an event triggered the exit
         (push ev unread-command-events))
       out)))

(defvar ess-build-tags-command nil
  "Command passed to generate tags.

If nil, `ess-build-tags-for-directory' uses the mode's imenu
regexpresion. Othersiwe, it should be a string with two %s
formats: one for directory and another for the output file.")

;;;*;;; Emacs itself

(defun ess-yank-cleaned-commands ()
  "Yank and strip the code, leaving only (R/S/Lsp/..) commands.
Deletes any lines not beginning with a prompt, and then removes
the prompt from those lines that remain.

Invoke this command with C-u C-u C-y."
  (setq yank-window-start (window-start))
  (let ((beg (point)))
    (push-mark beg)
    (setq this-command t)
    (insert-for-yank (current-kill 0))
    (ess-transcript-clean-region beg (point) nil)
    (if (eq (point) beg)
        (message "No commands found"))
    (if (eq this-command t)
        (setq this-command 'yank))
    ))

(defun ess-yank (&optional ARG)
  "With double prefix (C-u C-u) call `ess-yank-cleaned-commands"
  (interactive "*P")
  (if (equal '(16) ARG)
      (ess-yank-cleaned-commands)
    (let* ((remapped (command-remapping 'yank (point)))
           (command (cond ((eq remapped 'ess-yank) 'yank)
                          ((null remapped) 'yank)
                          (t remapped))))
      (funcall command ARG))))

(put 'ess-yank 'delete-selection 'yank)

(defun ess-build-tags-for-directory (dir tagfile)
  "Ask for directory and tag file and build tags for current dialect.

If the current language defines `ess-build-tags-command' use it
and ask the subprocess to build the tags. Otherwise use imenu
regexp and call find .. | etags .. in a shell command. You must
have 'find' and 'etags' programs installed.

Use M-. to navigate to a tag. M-x `visit-tags-table' to
append/replace the currently used tag table.

If prefix is given, force tag generation based on imenu. Might be
useful when different language files are also present in the
directory (.cpp, .c etc)."
  (interactive "DDirectory to tag:
GTags file (default TAGS): ")
  (when (or (eq (length (file-name-nondirectory tagfile)) 0)
            (file-directory-p tagfile))
    (setq tagfile (concat (file-name-as-directory tagfile) "TAGS")))
  ;; emacs find-tags doesn't play well with remote TAG files :(
  (when (file-remote-p tagfile)
    (require 'tramp)
    (setq tagfile (with-parsed-tramp-file-name tagfile foo foo-localname)))
  (when (file-remote-p dir)
    (setq dir (with-parsed-tramp-file-name dir foo foo-localname)))
  (if (and ess-build-tags-command (null current-prefix-arg))
      (ess-eval-linewise (format ess-build-tags-command dir tagfile))
    ;; else generate from imenu
    (unless (or imenu-generic-expression ess-imenu-generic-expression) ;; need both!!
      (error "No ess-tag-command found, and no imenu-generic-expression defined"))
    (let* ((find-cmd
            (format "find %s -type f -size 1M \\( -regex \".*\\.\\(cpp\\|jl\\|[RsrSch]\\(nw\\)?\\)$\" \\)" dir))
           (regs (delq nil (mapcar (lambda (l)
                                     (if (string-match "'" (cadr l))
                                         nil ;; remove for time being
                                       (format "/%s/\\%d/"
                                               (replace-regexp-in-string "/" "\\/" (nth 1 l) t)
                                               (nth 2 l))))
                                   imenu-generic-expression)))
           (tags-cmd (format "etags -o %s --regex='%s' -" tagfile
                             (mapconcat 'identity regs "' --regex='"))))
      (message "Building tags: %s" tagfile)
      ;; (dbg (format "%s | %s" find-cmd tags-cmd))
      (when (= 0 (shell-command (format "%s | %s" find-cmd tags-cmd)))
        (message "Building tags .. ok!")))))


;;;*;;; System

;; trying different viewers; thanks to an original patch for
;; ess-swv.el from Leo <sdl@web.de> :
(defun ess-get-ps-viewer ()
  "Get external PostScript viewer to be used from ESS.
Use `ess-ps-viewer-pref' when that is executably found by \\[executable-find].
Otherwise try a list of fixed known viewers."
  (file-name-nondirectory
   (or (and ess-ps-viewer-pref          ; -> ./ess-custom.el
            (executable-find ess-ps-viewer-pref))
       (executable-find "gv")
       (executable-find "evince")
       (executable-find "kghostview"))))

(defun ess-get-pdf-viewer ()
  "Get external PDF viewer to be used from ESS.
Use `ess-pdf-viewer-pref' when that is executably found by \\[executable-find].
Otherwise try a list of fixed known viewers.
"
  (let ((viewer (or ess-pdf-viewer-pref
                    ;; (and (stringp ess-pdf-viewer-pref)         ; -> ./ess-custom.el
                    ;;      (executable-find ess-pdf-viewer-pref))
                    (executable-find "evince")
                    (executable-find "kpdf")
                    (executable-find "okular")
                    (executable-find "xpdf")
                    (executable-find "acroread")
                    (executable-find "xdg-open")
                    ;; this one is wrong, (ok for time being as it is used only in swv)
                    (car (ess-get-words-from-vector
                          "getOption(\"pdfviewer\")\n"))
                    )))
    (when (stringp viewer)
      (setq viewer (file-name-nondirectory viewer)))
    viewer))


;;*;; UI

(defvar ess-current-region-overlay
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

(defun ess-deactivate-mark ()
  (cond ((and (featurep 'evil) evil-mode)
         (when (evil-visual-state-p)
           (evil-normal-state)))
        ((fboundp 'deactivate-mark)
         (deactivate-mark))))

;; SJE: 2009-01-30 -- this contribution from
;; Erik Iverson <iverson@biostat.wisc.edu>

(defun ess-tooltip-show-at-point (text xo yo)
  "Show a tooltip displaying 'text' at (around) point, xo and yo are x-
and y-offsets for the toolbar from point."
  (let (
        (fx (frame-parameter nil 'left))
        (fy (frame-parameter nil 'top))
        (fw (frame-pixel-width))
        (fh (frame-pixel-height))
        frame-left frame-top my-x-offset my-y-offset)

    ;; The following comment was found before code looking much like that
    ;; of frame-left and frame-top below in the file
    ;; tooltip-help.el. I include it here for acknowledgement, and I did observe
    ;; the same behavior with the Emacs window maximized under Windows XP.

    ;; -----original comment--------
    ;; handles the case where (frame-parameter nil 'top) or
    ;; (frame-parameter nil 'left) return something like (+ -4).
    ;; This was the case where e.g. Emacs window is maximized, at
    ;; least on Windows XP. The handling code is "shamelessly
    ;; stolen" from cedet/speedbar/dframe.el
    ;; (contributed by Andrey Grigoriev)

    (setq frame-left (if (not (consp fx))
                         fx
                       (if (eq (car fx) '-)
                           (- (x-display-pixel-width) (car (cdr fx)) fw)
                         (car (cdr fx)))))

    (setq frame-top (if (not (consp fy))
                        fy
                      (if (eq (car fy) '-)
                          (- (x-display-pixel-height) (car (cdr fy)) fh)
                        (car (cdr fy)))))

    ;; calculate the offset from point, use xo and yo to adjust to preference
    (setq my-x-offset (+ (car(window-inside-pixel-edges))
                         (car(posn-x-y (posn-at-point)))
                         frame-left xo))

    (setq my-y-offset (+ (cadr(window-inside-pixel-edges))
                         (cdr(posn-x-y (posn-at-point)))
                         frame-top yo))

    (let ((tooltip-frame-parameters
           (cons (cons 'top my-y-offset)
                 (cons (cons 'left my-x-offset)
                       tooltip-frame-parameters))))
      (tooltip-show text))
    ))

(defun ess-select-frame-set-input-focus (frame)
  "Select FRAME, raise it, and set input focus, if possible.
Copied almost verbatim from gnus-utils.el (but with test for mac added)."
  (cond ((featurep 'xemacs)
         (raise-frame frame)
         (select-frame frame)
         (focus-frame frame))
        ;; The function `select-frame-set-input-focus' won't set
        ;; the input focus under Emacs 21.2 and X window system.
        ;;((fboundp 'select-frame-set-input-focus)
        ;; (defalias 'gnus-select-frame-set-input-focus
        ;;   'select-frame-set-input-focus)
        ;; (select-frame-set-input-focus frame))
        (t
         (raise-frame frame)
         (select-frame frame)
         (cond ((and
                 (memq window-system '(x mac))
                 (fboundp 'x-focus-frame))
                (x-focus-frame frame))
               ((eq window-system 'w32)
                (w32-focus-frame frame)))
         (when focus-follows-mouse
           (set-mouse-position frame (1- (frame-width frame)) 0)))))

(defun ess-do-auto-fill ()
  "This is the same as \\[do-auto-fill] in GNU emacs 21.3, with one major
difference: if we could not find a suitable place to break the line,
we simply do not break it (instead of breaking after the first word)."
  (let (fc justify bol give-up
           (fill-prefix fill-prefix))
    (if (or (not (setq justify (current-justification)))
            (null (setq fc (current-fill-column)))
            (and (eq justify 'left)
                 (<= (current-column) fc))
            (save-excursion (beginning-of-line)
                            (setq bol (point))
                            (and auto-fill-inhibit-regexp
                                 (looking-at auto-fill-inhibit-regexp))))
        nil ;; Auto-filling not required
      (if (memq justify '(full center right))
          (save-excursion (unjustify-current-line)))

      ;; Choose a fill-prefix automatically.
      (if (and adaptive-fill-mode
               (or (null fill-prefix) (string= fill-prefix "")))
          (let ((prefix
                 (fill-context-prefix
                  (save-excursion (backward-paragraph 1) (point))
                  (save-excursion (forward-paragraph 1) (point)))))
            (and prefix (not (equal prefix ""))
                 (setq fill-prefix prefix))))

      (while (and (not give-up) (> (current-column) fc))
        ;; Determine where to split the line.
        (let* (after-prefix
               (fill-point
                (let ((opoint (point))
                      bounce
                      (first t))
                  (save-excursion
                    (beginning-of-line)
                    (setq after-prefix (point))
                    (and fill-prefix
                         (looking-at (regexp-quote fill-prefix))
                         (setq after-prefix (match-end 0)))
                    (move-to-column (1+ fc))
                    ;; Move back to the point where we can break the line.
                    ;; We break the line between word or
                    ;; after/before the character which has character
                    ;; category `|'.  We search space, \c| followed by
                    ;; a character, or \c| following a character.  If
                    ;; not found, place the point at beginning of line.
                    (while (or first
                               ;; If this is after period and a single space,
                               ;; move back once more--we don't want to break
                               ;; the line there and make it look like a
                               ;; sentence end.
                               (and (not (bobp))
                                    (not bounce)
                                    sentence-end-double-space
                                    (save-excursion (forward-char -1)
                                                    (and (looking-at "\\. ")
                                                         (not (looking-at "\\.  ")))))
                               (and (not (bobp))
                                    (not bounce)
                                    fill-nobreak-predicate
                                    (funcall fill-nobreak-predicate)))
                      (setq first nil)
                      (re-search-backward "[ \t]\\|\\c|.\\|.\\c|\\|^")
                      ;; If we find nowhere on the line to break it,
                      ;; do not break it.  Set bounce to t
                      ;; so we will not keep going in this while loop.
                      (if (<= (point) after-prefix)
                          (setq bounce t)
                        (if (looking-at "[ \t]")
                            ;; Break the line at word boundary.
                            (skip-chars-backward " \t")
                          ;; Break the line after/before \c|.
                          (forward-char 1))))
                    (if enable-multibyte-characters
                        ;; If we are going to break the line after or
                        ;; before a non-ascii character, we may have
                        ;; to run a special function for the charset
                        ;; of the character to find the correct break
                        ;; point.
                        (if (not (and (eq (charset-after (1- (point))) 'ascii)
                                      (eq (charset-after (point)) 'ascii)))
                            (fill-find-break-point after-prefix)))

                    ;; Let fill-point be set to the place where we end up.
                    ;; But move back before any whitespace here.
                    (skip-chars-backward " \t")
                    (point)))))

          ;; See whether the place we found is any good.
          (if (save-excursion
                (goto-char fill-point)
                (and (not (bolp))
                     ;; There is no use breaking at end of line.
                     (not (save-excursion (skip-chars-forward " ") (eolp)))
                     ;; It is futile to split at the end of the prefix
                     ;; since we would just insert the prefix again.
                     (not (and after-prefix (<= (point) after-prefix)))
                     ;; Don't split right after a comment starter
                     ;; since we would just make another comment starter.
                     (not (and comment-start-skip
                               (let ((limit (point)))
                                 (beginning-of-line)
                                 (and (re-search-forward comment-start-skip
                                                         limit t)
                                      (eq (point) limit)))))))
              ;; Ok, we have a useful place to break the line.  Do it.
              (let ((prev-column (current-column)))
                ;; If point is at the fill-point, do not `save-excursion'.
                ;; Otherwise, if a comment prefix or fill-prefix is inserted,
                ;; point will end up before it rather than after it.
                (if (save-excursion
                      (skip-chars-backward " \t")
                      (= (point) fill-point))
                    (funcall comment-line-break-function t)
                  (save-excursion
                    (goto-char fill-point)
                    (funcall comment-line-break-function t)))
                ;; Now do justification, if required
                (if (not (eq justify 'left))
                    (save-excursion
                      (end-of-line 0)
                      (justify-current-line justify nil t)))
                ;; If making the new line didn't reduce the hpos of
                ;; the end of the line, then give up now;
                ;; trying again will not help.
                (if (>= (current-column) prev-column)
                    (setq give-up t)))
            ;; No good place to break => stop trying.
            (setq give-up t))))
      ;; Justify last line.
      (justify-current-line justify t t)
      t)))


;;*;; Syntax

(defun ess-containing-sexp-position ()
  (cadr (syntax-ppss)))

(defun ess-code-end-position ()
  "Like (line-end-position) but stops at comments"
  (save-excursion
    (or (and (re-search-forward "#" (line-end-position) t)
             (match-beginning 0))
        (line-end-position))))

;; FIXME: The following function pattern stuff is specific to R but is
;; used throughout ESS

(defvar ess-r-set-function-start
  "^set[MGAR][Ma-z]+\\s-?(")

(defvar ess-function-pattern nil
  "Regexp to match the beginning of a function in S buffers.")

(defvar ess-r-symbol-pattern
  "\\(\\sw\\|\\s_\\)"
  "The regular expression for matching an R symbol")

(defvar ess-r-name-pattern
  (concat "\\(" ess-r-symbol-pattern "+\\|\\(`\\).+`\\)")
  "The regular expression for matching a R name.")

(let* ((Q     "\\s\"")                    ; quote
       (repl "\\(<-\\)?")                 ; replacement (function)
       (Sym-0 "\\(\\sw\\|\\s_\\)")        ; symbol
       (Symb (concat Sym-0 "+"))
       (xSymb "[^ \t\n\"']+") ;; (concat "\\[?\\[?" Sym-0 "*")); symbol / [ / [[ / [symbol / [[symbol
       ;; FIXME: allow '%foo%' but only when quoted; don't allow [_0-9] at beg.
       (_or_  "\\)\\|\\(")                ; OR
       (space "\\(\\s-\\|\n\\)*")         ; white space

       (part-1 (concat
                "\\(" ;;--------outer Either-------
                "\\(\\("          ; EITHER
                Q xSymb Q         ; any function name between quotes
                _or_
                "\\(^\\|[ ]\\)" Symb ; (beginning of name) + ess-r-symbol-pattern
                "\\)\\)"))        ; END EITHER OR

       (set-S4-exp
        (concat
         "^set\\(As\\|Method\\|Generic\\|GroupMethod\\|ReplaceMethod\\)(" ; S4 ...
         Q xSymb Q "," space
         ;; and now often `` signature(......), : ''
         ".*" ;; <<< FIXME ???
         ))

       (part-2 (concat
                "\\|" ;;--------outer Or ---------
                set-S4-exp
                "\\)" ;;--------end outer Either/Or-------

                "\\(" space "\\s<.*\\s>\\)*"      ; whitespace, comment
                ;; FIXME: in principle we should skip 'definition *= *' here
                space "function\\s-*(" ; whitespace, function keyword, parenthesis
                )))

  (defvar ess-r-function-pattern
    (concat part-1
            "\\s-*\\(<-\\|=\\)" ; whitespace, assign
            part-2)
    "The regular expression for matching the beginning of an R function.")

  (defvar ess-s-function-pattern
    (concat part-1
            "\\s-*\\(<-\\|_\\|=\\)" ; whitespace, assign (incl. "_")
            part-2)
    "The regular expression for matching the beginning of an S function."))


(defvar ess--funname.start nil)

(defun ess--funname.start (&optional look-back)
  "If inside a function call, return (FUNNAMME . START) where
FUNNAME is a function name found before ( and START is where
FUNNAME starts.

LOOK-BACK is a number of characters to look back; defaults to
2000. As the search might get quite slow for files with thousands
of lines.

Also store the cons in 'ess--funname.start for potential use
later."
  (save-excursion
    (save-restriction
     (let* ((proc (get-buffer-process (current-buffer)))
            (mark (and proc (process-mark proc))))

       (if (and mark (>= (point) mark))
           (narrow-to-region mark (point)))

       (and ess-noweb-mode
            (ess-noweb-narrow-to-chunk))

       (unless (ess-inside-string-p)
         (setq ess--funname.start
               (condition-case nil ;; check if it is inside a functon
                   (progn
                     ;; for the sake of big buffers, look only 1000 chars back
                     (narrow-to-region (max (point-min) (- (point) 1000)) (point))
                     (up-list -1)
                     (while (not (looking-at "("))
                       (up-list -1))
                     (let ((funname (symbol-name (symbol-at-point))))
                       (when (and funname
                                  (not (member funname ess-S-non-functions)))
                         (cons funname (- (point) (length funname))))
                       ))
                 (error nil))
               ))))))

(defun ess-function-arguments (funname &optional proc)
  "Get FUNARGS from cache or ask the process for it.

Return FUNARGS - a list with the first element being a
cons (package_name . time_stamp_of_request), second element is a
string giving arguments of the function as they appear in
documentation, third element is a list of arguments of all
methods.

If package_name is nil, and time_stamp is less recent than the
time of the last user interaction to the process, then update the
entry.

Package_name is also nil when funname was not found, or funname
is a special name that contains :,$ or @.

If PROC is given, it should be an ESS process which should be
queried for arguments.
"

  (when (and funname ;; usually returned by ess--funname.start (might be nil)
             (or proc (ess-process-live-p)))
    (let* ((proc (or proc (get-process ess-local-process-name)))
           (args (gethash funname (process-get proc 'funargs-cache)))
           (pack (caar args))
           (ts   (cdar args)))
      (when (and args
                 (and (time-less-p ts (process-get proc 'last-eval))
                      (or (null pack)
                          (equal pack ""))))
        ;; reset cache
        (setq args nil))
      (or args
          (cadr (assoc funname (process-get proc 'funargs-pre-cache)))
	  (and
	   (not (process-get proc 'busy))
	   (with-current-buffer (ess-command (format ess-funargs-command
						     (ess-quote-special-chars funname))
					     nil nil nil nil proc)
	     (goto-char (point-min))
	     (when (re-search-forward "(list" nil t)
	       (goto-char (match-beginning 0))
	       (setq args (ignore-errors (eval (read (current-buffer)))))
	       (if args
		   (setcar args (cons (car args) (current-time)))))
	     ;; push even if nil
	     (puthash (substring-no-properties funname) args (process-get proc 'funargs-cache))))))))

(defun ess-symbol-at-point ()
  "Like `symbol-at-point' but consider fully qualified names.
Fully qualified names include accessor symbols (like aaa$bbb and
aaa@bbb in R)."
  (with-syntax-table (or ess-mode-completion-syntax-table
                         ess-mode-syntax-table
                         (syntax-table))
    (symbol-at-point)))

(defun ess-symbol-start ()
  "Get initial position for objects completion.
Symbols are fully qualified names that include accessor
symbols (like aaa$bbb and aaa@bbb in R)."
  (let ((beg (car (with-syntax-table (or ess-mode-completion-syntax-table
                                         ess-mode-syntax-table
                                         (syntax-table))
                    (bounds-of-thing-at-point 'symbol)))))
    (when (and beg (not (save-excursion (goto-char beg)
                                        (looking-at "/\\|.[0-9]"))))
      beg)))

(defun ess-arg-start ()
  "Get initial position for args completion"
  (when (not (ess-inside-string-p))
    (when (ess--funname.start)
      (if (looking-back "[(,]+[ \t\n]*")
          (point)
        (ess-symbol-start)))))

(defun ess-inside-string-or-comment-p (&optional pos)
  "Return non-nil if POSition [defaults to (point)] is inside string or comment
 (according to syntax)."
  ;;FIXME (defun ess-calculate-indent ..)  can do that ...
  (interactive)
  (setq pos (or pos (point)))
  (let ((ppss (syntax-ppss pos)))
    (or (car (setq ppss (nthcdr 3 ppss)))
        (car (setq ppss (cdr ppss)))
        (nth 3 ppss))))

(defun ess-inside-string-p (&optional pos)
  "Return non-nil if point is inside string (according to syntax)."
  (interactive)
  ;; when narrowing the buffer in iESS the ppss cahce is screwed:( But it is
  ;; very fast, so don't bother for now.
  (let ((pps (syntax-ppss pos)))
    (nth 3 pps))
  ;; (nth 3 (parse-partial-sexp (point-min) pos))
  )

(defun ess-inside-comment-p (&optional pos)
  "Return non-nil if point is inside string (according to syntax)."
  (interactive)
  (setq pos (or pos (point)))
  (save-excursion
    (or (when font-lock-mode ;; this is a shortcut (works well usually)
	  (let ((face (get-char-property pos 'face)))
	    (eq 'font-lock-comment-face face)))
	(nth 4 (parse-partial-sexp (progn (goto-char pos) (point-at-bol)) pos)))))

(defun ess-inside-brackets-p (&optional pos curly?)
  "Return t if position POS is inside brackets.
POS defaults to point if no value is given. If curly? is non nil
also return t if inside curly brackets."
  (save-excursion
    (let ((ppss (syntax-ppss pos))
          (r nil))
      (while (and (> (nth 0 ppss) 0)
                  (not r))
        (goto-char (nth 1 ppss))
        (when (or (char-equal ?\[ (char-after))
                  (and curly?
                       (char-equal ?\{ (char-after))))
          (setq r t))
        (setq ppss (syntax-ppss)))
      r)))


;;*;; String manipulation

(defun ess-quote-special-chars (string)
  (replace-regexp-in-string
   "\"" "\\\\\\&"
   (replace-regexp-in-string ;; replace backslashes
    "\\\\" "\\\\" string nil t)))

;; simple alternative to ess-read-object-name-default of ./ess-inf.el :
;; is "wrongly" returning   "p1"  for word "p1.part2" :
(defun ess-extract-word-name ()
  "Get the word you're on (cheap algorithm). Use `ess-read-object-name-default'
for a better but slower version."
  (save-excursion
    (re-search-forward "\\<\\w+\\>" nil t)
    (buffer-substring (match-beginning 0) (match-end 0))))

(defun ess-rep-regexp (regexp to-string &optional fixedcase literal verbose)
  "Instead of (replace-regexp..) -- do NOT replace in strings or comments.
 If FIXEDCASE is non-nil, do *not* alter case of replacement text.
 If LITERAL   is non-nil, do *not* treat `\\' as special.
 If VERBOSE   is non-nil, (message ..) about replacements."
  (let ((case-fold-search (and case-fold-search
                               (not fixedcase))); t  <==> ignore case in search
        (ppt (point)); previous point
        (p))
    (while (and (setq p (re-search-forward regexp nil t))
                (< ppt p))
      (setq ppt p)
      (cond ((not (ess-inside-string-or-comment-p (1- p)))
             (if verbose
                 (let ((beg (match-beginning 0)))
                   (message "buffer in (match-beg.,p)=(%d,%d) is '%s'"
                            beg p (buffer-substring beg p))))
             (replace-match to-string fixedcase literal)
             ;;or (if verbose (setq pl (append pl (list p))))
             )))
    ;;or (if (and verbose pl)
    ;;or  (message "s/%s/%s/ at %s" regexp to-string pl))
    ) )

(defun ess-replace-regexp-dump-to-src
  (regexp to-string &optional dont-query verbose ensure-mode)
  "Depending on dont-query, call `ess-rep-regexp' or `query-replace-regexp'
from the beginning of the buffer."
  (save-excursion
    (if (and ensure-mode
             (not (equal major-mode 'ess-mode)))
        (ess-mode))
    (goto-char (point-min))
    (if dont-query
        (ess-rep-regexp     regexp to-string nil nil verbose)
      (query-replace-regexp regexp to-string nil))))

(defun ess-space-around (word &optional from verbose)
  "Replace-regexp .. ensuring space around all occurences of WORD,
 starting from FROM {defaults to (point)}."
  (interactive "d\nP"); Defaults: point and prefix (C-u)
  (save-excursion
    (goto-char from)
    (ess-rep-regexp (concat "\\([^ \t\n]\\)\\(\\<" word "\\>\\)")
                    "\\1 \\2" nil nil verbose)
    (goto-char from)
    (ess-rep-regexp (concat "\\(\\<" word "\\>\\)\\([^ \t\n]\\)")
                    "\\1 \\2" nil nil verbose)
    )
  )

(defun ess-time-string (&optional clock)
  "Returns a string for use as a timestamp. + hr:min if CLOCK is non-nil,
 like \"13 Mar 1992\".  Redefine to taste."
  (format-time-string (concat "%e %b %Y" (if clock ", %H:%M"))))

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

;;- From: friedman@gnu.ai.mit.edu (Noah Friedman)
;;- Date: 12 Feb 1995 21:30:56 -0500
;;- Newsgroups: gnu.emacs.sources
;;- Subject: nuke-trailing-whitespace
;;-
;;- This is too trivial to make into a big todo with comments and copyright
;;- notices whose length exceed the size of the actual code, so consider it
;;- public domain.  Its purpose is along similar lines to that of
;;- `require-final-newline', which is built in.  I hope the names make it
;;- obvious.

;; (add-hook 'write-file-hooks 'nuke-trailing-whitespace)
;;or at least
;; (add-hook 'ess-mode-hook
;;         (lambda ()
;;           (add-hook 'local-write-file-hooks 'nuke-trailing-whitespace)))

(defvar ess-nuke-trailing-whitespace-p nil;disabled by default  'ask
  "*[Dis]activates (ess-nuke-trailing-whitespace).
 Disabled if `nil'; if `t', it works unconditionally, otherwise,
 the user is queried.
 Note that setting the default to `t' may not be a good idea when you edit
 binary files!")

;;; MM: Newer Emacsen now have  delete-trailing-whitespace
;;; --  but no customization like  nuke-trailing-whitespace-p ..
(defun ess-nuke-trailing-whitespace ()
  "Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on write-file-hooks.

If the variable `ess-nuke-trailing-whitespace-p' is `nil', this function is
disabled.  If `t', unreservedly strip trailing whitespace.
If not `nil' and not `t', query for each instance."
  (interactive)
  (let ((bname (buffer-name)))
    (cond ((or
            (string= major-mode "rmail-mode")
            (string= bname "RMAIL")
            nil)); do nothing..

          (t
           (and (not buffer-read-only)
                ess-nuke-trailing-whitespace-p
                (save-match-data
                  (save-excursion
                    (save-restriction
                      (widen)
                      (goto-char (point-min))
                      (cond ((eq ess-nuke-trailing-whitespace-p t)
                             (while (re-search-forward "[ \t]+$" (point-max) t)
                               (delete-region (match-beginning 0)
                                              (match-end 0))))
                            (t
                             (query-replace-regexp "[ \t]+$" "")))))))))
    ;; always return nil, in case this is on write-file-hooks.
    nil))


;;*;; Debugging tools

(defun ess-write-to-dribble-buffer (text)
  "Write TEXT to dribble ('*ESS*') buffer."
  (unless (buffer-live-p ess-dribble-buffer)
    ;; ESS dribble buffer must be re-created.
    (setq ess-dribble-buffer (get-buffer-create "*ESS*")))
  (let (deactivate-mark)
    (with-current-buffer ess-dribble-buffer
      (goto-char (point-max))
      (insert-before-markers text))))

;; Shortcut to render "dribbling" statements less cluttering:
(defun ess-if-verbose-write (text)
  "Write TEXT to dribble buffer ('*ESS*') only *if* `ess-verbose'."
  (if ess-verbose (ess-write-to-dribble-buffer text)))


(defvar ess-current-region-overlay
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

(defun ess-deactivate-mark ()
  (cond ((and (featurep 'evil) evil-mode)
         (when (evil-visual-state-p)
           (evil-normal-state)))
        ((fboundp 'deactivate-mark)
         (deactivate-mark))))

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

(defun ess-kill-last-line ()
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (delete-region (point-at-eol) (point-max))))

(defvar ess-adjust-chunk-faces t
  "Whether to adjust background color in code chunks.")

(defvar-local ess-buffer-has-chunks nil
  "Internal usage: indicates whether a buffer has chunks.
This is used to make face adjustment a no-op when a buffer does
not contain chunks.")

(defvar ess-adjust-face-intensity 2
  "Default intensity for adjusting faces.")

(defun ess-adjust-face-background (start end &optional intensity)
  "Adjust face background between BEG and END.
On dark background, lighten.  Oposite on light."
  (let* ((intensity (or intensity ess-adjust-face-intensity))
         (color (color-lighten-name
                 (face-background 'default)
                 (if (eq (frame-parameter nil 'background-mode) 'light)
                     (- intensity)
                   intensity)))
         (face (list (cons 'background-color color))))
    (with-silent-modifications
      (ess-adjust-face-properties start end 'face face))))

;; Taken from font-lock.el.
(defun ess-adjust-face-properties (start end prop value)
  "Tweaked `font-lock-prepend-text-property'.
Adds the `ess-face-adjusted' property so we only adjust face once."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop nil end)
            prev (get-text-property start prop))
      ;; Canonicalize old forms of face property.
      (and (memq prop '(face font-lock-face))
           (listp prev)
           (or (keywordp (car prev))
               (memq (car prev) '(foreground-color background-color)))
           (setq prev (list prev)))
      (add-text-properties start next
                           (list prop (append val (if (listp prev) prev (list prev)))
                                 'ess-face-adjusted t))
      (setq start next))))

(defun ess-find-overlay (pos prop)
  (cl-some (lambda (overlay)
             (when (overlay-get overlay prop)
               overlay))
           (overlays-at pos)))

(provide 'ess-utils)

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
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-utils.el ends here
