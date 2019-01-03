;;; ess-utils.el --- General Emacs utility functions used by ESS  -*- lexical-binding: t; -*-

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
;; https://www.r-project.org/Licenses/


;;; Commentary:
;; Various utilities for ESS.

;;; Code:
(require 'cl-lib)
(eval-when-compile
  (require 'tramp))
;; The only ESS file this file should depend on is ess-custom.el
(require 'cl-lib)
(require 'comint)
(require 'ess)
(require 'ess-custom)
(require 'ido)
(require 'newcomment)
(defvar ac-modes)
(declare-function ess-eval-linewise "ess-inf")
(declare-function evil-visual-state-p "evil")
(declare-function evil-normal-state "evil")
(declare-function color-lighten-name "color")
(declare-function tramp-dissect-file-name "tramp")
;; The following declares can be removed once we drop Emacs 25
(declare-function tramp-file-name-method "tramp")
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-file-name-hop "tramp")

;;; FIXME: Used in ./obsolete/xyz.el only; remove once gone.
(defun ess-message (format-string &rest args)
  "Shortcut for \\[message] only if `ess-show-load-messages' is non-nil."
  (when (bound-and-true-p ess-show-load-messages)
    (message format-string args)))


;;*;; elisp tools

(defun ess-next-code-line (&optional arg skip-to-eob)
  "Move ARG lines of code forward (backward if ARG is negative).
If `ess-eval-empty' is non-nil, skip past all empty and comment
lines. Default for ARG is 1. Don't skip the last empty and
comment lines in the buffer unless SKIP-TO-EOB is non-nil. On
success, return 0. Otherwise, go as far as possible and return
-1."
  (interactive "p")
  (or arg (setq arg 1))
  (if (or ess-eval-empty
          (and (fboundp 'ess-roxy-entry-p)
               (ess-roxy-entry-p)))
      (forward-line arg)
    (beginning-of-line)
    (let ((pos (point))
          (n 0)
          (inc (if (> arg 0) 1 -1)))
      (while (and (/= arg 0) (= n 0))
        (setq n (forward-line inc))     ; n=0 is success
        (comment-beginning)
        (beginning-of-line)
        (forward-comment (* inc (buffer-size))) ;; as suggested in info file
        (if (or skip-to-eob
                (not (looking-at ess-no-skip-regexp))) ;; don't go to eob or whatever
            (setq arg (- arg inc))
          (goto-char pos)
          (setq arg 0)
          (forward-line 1)) ;; stop at next empty line
        (setq pos (point)))
      (goto-char pos)
      n)))

(defun ess-goto-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ess-skip-thing (thing)
  "Leave point at the end of THING.
THING can be 'function, 'paragraph, or 'line."
  (cond
   ((eql thing 'line) (goto-char (line-end-position)))
   ((eql thing 'paragraph) (forward-paragraph))
   ((eql thing 'function) (end-of-defun) (skip-chars-backward " \t\n"))))

(defun ess-line-end-position (&optional N)
  "Return the 'point' at the end of N lines. N defaults to 1, i.e., current line."
  (save-excursion
    (end-of-line N)
    (point)))

(defun ess-search-except (regexp &optional except backward)
  "Search for a REGEXP and store as match 1.
Optionally ignore strings that match exceptions."
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
        (setq continue nil)))
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
  "Return FILE-OR-BUFFER if it is a buffer.
Otherwise return the buffer associated with the file which must
be qualified by it's path; if the buffer does not exist, return
nil."
  (interactive)
  (if file-or-buffer
      (if (bufferp file-or-buffer) file-or-buffer
        (find-buffer-visiting file-or-buffer))))

(defun ess-return-list (ess-arg)
  "If ESS-ARG is a list return it, else return ESS-ARG in a list."
  (if (listp ess-arg) ess-arg (list ess-arg)))

(defun ess-uniq (list predicate)
  "Uniquify LIST, stably, deleting elements using PREDICATE.
Return the list with subsequent duplicate items removed by side effects.
PREDICATE is called with an element of LIST and a list of elements from LIST,
and should return the list of elements with occurrences of the element removed.
This function will work even if LIST is unsorted.  See also `delete-dups'."
  (declare (obsolete 'delete-dups "ESS 19.04"))
  (let ((list list))
    (while list
      (setq list (setcdr list (funcall predicate (car list) (cdr list))))))
  list)

(define-obsolete-function-alias 'ess-uniq-list 'delete-dups "ESS 19.04")

(defun ess-flatten-list (&rest list)
  "Take the arguments and flatten them into one long LIST.
Drops 'nil' entries."
  ;; Taken from lpr.el
  ;; `lpr-flatten-list' is defined here (copied from "message.el" and
  ;; enhanced to handle dotted pairs as well) until we can get some
  ;; sensible autoloads, or `flatten-list' gets put somewhere decent.
  (ess-flatten-list-1 list))

(defun ess-flatten-list-1 (list)
  (cond
   ((null list) (list))
   ((consp list)
    (append (ess-flatten-list-1 (car list))
            (ess-flatten-list-1 (cdr list))))
   (t (list list))))

(define-obsolete-function-alias 'ess-delete-blank-lines
  'delete-blank-lines "ESS 19.04")

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
  ;; whether or not a revert is needed, force load local variables
  ;; for example, suppose that you change the local variables and then
  ;; save the file, a revert is unneeded, but a force load is
  (hack-local-variables)
  (unless (verify-visited-file-modtime (current-buffer))
    (progn
      (let ((ess-temp-store-point (point)))
        (revert-buffer t t)
        (goto-char ess-temp-store-point))
      t)))

(define-obsolete-function-alias 'ess-find-exec 'ess-find-exec-completions "ESS 19.04")
(defun ess-find-exec-completions (ess-root-arg &optional ess-exec-dir)
  "Given the root of an executable file name, find all possible completions.
Search for the executables in ESS-EXEC-DIR (which defaults to
`exec-path' if no value is given)."
  (let* ((ess-exec-path
          (if ess-exec-dir (ess-return-list ess-exec-dir) exec-path))
         (ess-tmp-exec nil)
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
                   (not (backup-file-name-p ess-tmp-file))
                   (not (file-directory-p ess-tmp-file)))
              ;; we have found a possible executable, so keep it.
              (setq ess-tmp-exec
                    (nconc ess-tmp-exec (list ess-tmp-file)))))))
    ess-tmp-exec))

(defun ess-drop-non-directories (file-strings)
  "Drop all entries in FILE-STRINGS that do not \"look like\" directories."
  (ess-flatten-list (mapcar 'file-name-directory file-strings)))

(defun ess--parent-dir (path n)
  "Return Nth parent of PATH."
  (let ((opath path))
    (while (and path (> n 0))
      (setq path (file-name-directory (directory-file-name opath)))
      (if (equal path opath)
          (setq path nil)
        (setq opath path
              n (1- n))))
    path))


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
process to avoid excessive requests."
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
  "Execute dialect specific COMMAND.

-- If command is nil issue warning 'Not available for dialect X'
-- If command is a elisp function, execute it with ARGS
-- If a string starting with 'http' or 'www', browse with `browse-url',
   otherwise execute the command in inferior process.
-- If a string, interpret as a command to subprocess, and
   substitute ARGS with `(format ,command ,@args).

When PROMPT is non-nil ask the user for a string value and
prepend the response to ARGS.

If prompt is a string just pass it to `read-string'. If a list, pass it
to `ess-completing-read'."
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


;;; Emacs Integration

(defun ess-derived-mode-p ()
  "Non-nil if the current major mode is an ESS major mode."
  (or (derived-mode-p 'ess-mode)
      (derived-mode-p 'ess-julia-mode)))

(defun ess--generate-eval-visibly-submenu (_menu)
  '(["yes" (lambda () (interactive) (setq ess-eval-visibly t))
     :style radio :enable t :selected (eq ess-eval-visibly t)]
    ["nowait" (lambda () (interactive) (setq ess-eval-visibly 'nowait))
     :style radio :enable t :selected (eq ess-eval-visibly 'nowait) ]
    ["no" (lambda () (interactive) (setq ess-eval-visibly nil))
     :style radio :enable t :selected (eq ess-eval-visibly nil) ]))

;; Font Lock

(defun ess--fl-keywords-values ()
  "Return a cons (STANDARD-VALUE . CUSTOM-VALUE) of `ess-font-lock-keywords'."
  (let ((sym ess-font-lock-keywords))
    (if (and (symbolp sym)
             (custom-variable-p sym))
        (cons
         (eval (car (get sym 'standard-value)))
         (symbol-value sym))
      (error "`ess-font-lock-keywords' must be a symbol of a custom variable"))))

(defun ess--extract-fl-keywords ()
  (let ((values (ess--fl-keywords-values)))
    (mapcar (lambda (kv)
              (let ((cust-kv (assoc (car kv) (cdr values))))
                (when cust-kv
                  (setcdr kv (cdr cust-kv))))
              kv)
            (copy-alist (car values)))))

(defun ess-build-font-lock-keywords ()
  "Retrieve `font-lock-keywords' from ess-[dialect]-font-lock-keywords.
Merge the customized values of that variable on top of the
standard values and return the new list. For this to work,
`ess-font-lock-keywords' should be a name of the
ess-[dialect]-font-lock-keywords variable."
  (delq nil
        (mapcar (lambda (c)
                  (when (cdr c)
                    (symbol-value (car c))))
                (ess--extract-fl-keywords))))

(defun ess-font-lock-toggle-keyword (&optional keyword)
  (interactive)
  (let* ((values (ess--fl-keywords-values))
         (keyword (or keyword
                      (if (called-interactively-p 'any)
                          (intern (ess-completing-read
                                   "Keyword to toggle"
                                   (mapcar (lambda (el) (symbol-name (car el)))
                                           (car values))
                                   nil t))
                        (error "Wrong number of arguments"))))
         (kwd (cond
               ;; already in custom values
               ((assoc keyword (cdr values)))
               ;; if keyword is not already in custom values (can happen if
               ;; we add new keywords but the user has the old value saved in
               ;; .emacs-custom.el)
               ((let ((kwd (assoc keyword (car values)))
                      (sym ess-font-lock-keywords))
                  (when kwd
                    (set sym (cons kwd (symbol-value sym)))
                    kwd)))
               (t (error "Invalid keyword %s" keyword)))))
    (setcdr kwd (not (cdr kwd)))
    (let ((mode major-mode)
          (dialect ess-dialect))
      ;; refresh font-lock defaults in all relevant buffers
      (mapc (lambda (b)
              (with-current-buffer b
                (when (and (eq major-mode mode)
                           (eq ess-dialect dialect))
                  (font-lock-refresh-defaults))))
            (buffer-list)))))

(defun ess--generate-font-lock-submenu (_menu)
  "Generate ESS font-lock submenu."
  (append (mapcar (lambda (el)
                    `[,(symbol-name (car el))
                      (lambda () (interactive)
                        (ess-font-lock-toggle-keyword ',(car el)))
                      :style toggle
                      :enable t
                      :selected ,(cdr el)])
                  (ess--extract-fl-keywords))
          (list "-----"
                ["Save to custom"
                 (lambda () (interactive)
                   (customize-save-variable ess-font-lock-keywords
                                            (ess--extract-fl-keywords)))
                 t])))


;;; External modes

;; Define these here for the byte compiler since ido dynamically
;; let-binds them:
(defvar ido-choice-list)
(defvar ido-context-switch-command)
(defvar ido-directory-too-big)
(defvar ido-directory-nonreadable)

(defun ess-completing-read (prompt collection &optional predicate
                                   require-match initial-input hist def)
  "Read a string in the minibuffer, with completion.
Use `ido-completing-read' if IDO interface is present, or fall
back on classical `completing-read' otherwise. Meaning of
arguments is as in `completing-read' (PROMPT is automatically
suffixed with ': ' and (default %s) when needed). If HIST is null
use `ess--completing-hist' as history. See also `ess-use-ido'."
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
      (completing-read prompt collection predicate require-match initial-input hist def))))

(defun ess--setup-auto-complete (sources &optional inferior)
  "Setup auto-complete depending on user settings.
SOURCES gets added to `ac-sources', INFERIOR should be t for
inferior buffers."
  ;; auto-complete
  (when (and (boundp 'ac-sources)
             (if inferior
                 (eq ess-use-auto-complete t)
               ess-use-auto-complete))
    (add-to-list 'ac-modes major-mode)
    ;; files should be in front; ugly, but needed
    (setq ac-sources
          (delq 'ac-source-filename ac-sources))
    (mapc (lambda (el) (add-to-list 'ac-sources el))
          sources)
    (add-to-list 'ac-sources 'ac-source-filename)))

(defun ess--setup-company (sources &optional inferior)
  "Setup company depending on user settings.
SOURCES gets added to `company-backends', and when t, INFERIOR
specifies inferior buffers."
  ;; company
  (when (and (boundp 'company-backends)
             (if inferior
                 (eq ess-use-company t)
               ess-use-company))
    (setq-local company-backends
                (cl-copy-list (append sources company-backends)))
    (delq 'company-capf company-backends)))

(defmacro ess--execute-electric-command (map &optional prompt wait exit-form &rest args)
  "Execute single-key comands defined in MAP till a key is pressed which is not part of map.
Single-key input commands are those that once executed do not
require the prefix command for subsequent invocation. Return the
value of the lastly executed command. PROMPT is passed to
`read-event'.

If WAIT is t, wait for next input and ignore the keystroke which
triggered the command.

Each command in map should accept one at least one argument, the
most recent event (as read by `read-event'). ARGS are the
supplementary arguments passed to the commands.

EXIT-FORM should be supplied for a more refined control of the
read-even loop. The loop is exited when EXIT-FORM evaluates to
t. See examples in the tracebug code."
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

(cl-defgeneric ess-build-tags-command ()
  "Command passed to generate tags.
If nil, `ess-build-tags-for-directory' uses the mode's imenu
expression. Otherwise, it should be a string with two %s
formats: one for directory and another for the output file."
  nil)


;;; Emacs itself

(defun ess-yank-cleaned-commands ()
  "Yank and strip the code, leaving only (R/S/Lsp/..) commands.
Deletes any lines not beginning with a prompt, and then removes
the prompt from those lines that remain. Invoke this command with
C-u C-u C-y."
  (setq yank-window-start (window-start))
  (let ((beg (point)))
    (push-mark beg)
    (setq this-command t)
    (insert-for-yank (current-kill 0))
    (when (and (require 'ess-trns) (fboundp 'ess-transcript-clean-region))
      (ess-transcript-clean-region beg (point) nil))
    (if (eq (point) beg)
        (message "No commands found"))
    (if (eq this-command t)
        (setq this-command 'yank))))

(defun ess-yank (&optional arg)
  "With double prefix ARG (C-u C-u) call `ess-yank-cleaned-commands'."
  (interactive "*P")
  (if (equal '(16) arg)
      (ess-yank-cleaned-commands)
    (let* ((remapped (command-remapping 'yank (point)))
           (command (cond ((eq remapped 'ess-yank) 'yank)
                          ((null remapped) 'yank)
                          (t remapped))))
      (funcall command arg))))

(put 'ess-yank 'delete-selection 'yank)

(defun ess-build-tags-for-directory (dir tagfile)
  "Ask for directory and tag file and build tags for current dialect.
If the current language defines `ess-build-tags-command' use it
and ask the subprocess to build the tags. Otherwise use imenu
regexp and call find .. | etags .. in a shell command. You must
have 'find' and 'etags' programs installed.

Use M-. to navigate to a tag. \\[visit-tags-table] to
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
    (require 'tramp)
    (setq dir (with-parsed-tramp-file-name dir foo foo-localname)))
  (if (and (ess-build-tags-command) (null current-prefix-arg))
      (ess-eval-linewise (format (ess-build-tags-command) dir tagfile))
    ;; else generate from imenu
    (unless imenu-generic-expression
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
      (when (= 0 (shell-command (format "%s | %s" find-cmd tags-cmd)))
        (message "Building tags .. ok!")))))


;;; System

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
Otherwise try a list of fixed known viewers."
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
                    (when (fboundp 'ess-get-words-from-vector)
                      (car (ess-get-words-from-vector
                            "getOption(\"pdfviewer\")\n"))))))
    (when (stringp viewer)
      (setq viewer (file-name-nondirectory viewer)))
    viewer))


;;; UI

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
  (cond ((and (featurep 'evil) (bound-and-true-p evil-mode))
         (when (evil-visual-state-p)
           (evil-normal-state)))
        (mark-active
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
                           (- (display-pixel-width) (car (cdr fx)) fw)
                         (car (cdr fx)))))
    (setq frame-top (if (not (consp fy))
                        fy
                      (if (eq (car fy) '-)
                          (- (display-pixel-height) (car (cdr fy)) fh)
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
      (tooltip-show text))))

(defun ess-select-frame-set-input-focus (frame)
  "Select FRAME, raise it, and set input focus, if possible.
Copied almost verbatim from gnus-utils.el (but with test for mac added)."
  ;; The function `select-frame-set-input-focus' won't set
  ;; the input focus under Emacs 21.2 and X window system.
  ;;((fboundp 'select-frame-set-input-focus)
  ;; (defalias 'gnus-select-frame-set-input-focus
  ;;   'select-frame-set-input-focus)
  ;; (select-frame-set-input-focus frame))
  (raise-frame frame)
  (select-frame frame)
  (cond ((and
          (memq window-system '(x mac))
          (fboundp 'x-focus-frame))
         (x-focus-frame frame))
        ((and (eq window-system 'w32)
              ;; silence byte compiler warnings about w32-fns
              (fboundp 'w32-focus-frame))
         (w32-focus-frame frame)))
  (when focus-follows-mouse
    (set-mouse-position frame (1- (frame-width frame)) 0)))

(define-obsolete-function-alias 'ess-do-auto-fill 'do-auto-fill "ESS 19.04")


;;; Syntax

(defun ess-containing-sexp-position ()
  (cadr (syntax-ppss)))

(defun ess-code-end-position ()
  "Like (line-end-position) but stops at comments."
  (save-excursion
    (goto-char (1+ (line-end-position)))
    (forward-comment -1)
    (point)))

;; FIXME: The following function pattern stuff is specific to R but is
;; used throughout ESS
(defvar ess-r-set-function-start
  ;; [MGAR].. <=>  {setMethod(), set[Group]Generic(), setAs(), setReplaceMethod()}
  ;; see also set-S4-exp in ess-r-function-pattern below
  "^set[MGAR][GMa-z]+\\s-?(")

(defvar ess-function-pattern nil ; in R set to ess-r-function-pattern
  "Regexp to match the beginning of a function in S buffers.")

(defvar ess-r-symbol-pattern
  "\\(\\sw\\|\\s_\\)"
  "The regular expression for matching an R symbol.")

(defvar ess-r-name-pattern
  (concat "\\(" ess-r-symbol-pattern "+\\|\\(`\\).+`\\)")
  "The regular expression for matching a R name.")

(defvar ess--r-s-function-pattern
  (let* ((Q     "\\s\"")                    ; quote
         (Sym-0 "\\(\\sw\\|\\s_\\)")        ; symbol
         (Symb (concat Sym-0 "+"))
         (xSymb "[^ \t\n\"']+") ;; (concat "\\[?\\[?" Sym-0 "*")); symbol / [ / [[ / [symbol / [[symbol
         ;; FIXME: allow '%foo%' but only when quoted; don't allow [_0-9] at beg.
         (regex-or  "\\)\\|\\(")                ; OR
         (space "\\(\\s-\\|\n\\)*")         ; white space

         (part-1 (concat
                  "\\(" ;;--------outer Either-------
                  "\\(\\("          ; EITHER
                  Q xSymb Q         ; any function name between quotes
                  regex-or
                  "\\(^\\|[ ]\\)" Symb ; (beginning of name) + ess-r-symbol-pattern
                  "\\)\\)"))        ; END EITHER OR

         (set-S4-exp
          (concat
           "^set\\(As\\|Method\\|Generic\\|GroupGeneric\\|ReplaceMethod\\)(" ; S4 ...
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
    `(,part-1 ,part-2))
  "Placeholder for use in constructing `ess-r-function-pattern' and `ess-s-function-pattern'.")

(defvar ess-r-function-pattern
  (concat (car ess--r-s-function-pattern)
          "\\s-*\\(<-\\|=\\)" ; whitespace, assign
          (nth 1 ess--r-s-function-pattern))
  "The regular expression for matching the beginning of an R function.")

(defvar ess-s-function-pattern
  (concat (car ess--r-s-function-pattern)
          "\\s-*\\(<-\\|_\\|=\\)" ; whitespace, assign (incl. "_")
          (nth 1 ess--r-s-function-pattern))
  "The regular expression for matching the beginning of an S function.")

(defvar ess--fn-name-start-cache nil)

(defun ess--fn-name-start ()
  "Return (FN-NAME . START-POS).
FN-NAME is a function name located before the pointer. START-POS
is the position where FN-NAME starts. Store this cons in variable
`ess--fn-name-start-cache'."
  (save-excursion
    (save-restriction
      (let* ((proc (get-buffer-process (current-buffer)))
             (mark (and proc (process-mark proc))))
        (if (and mark (>= (point) mark))
            (narrow-to-region mark (point)))
        (and (fboundp 'ess-noweb-narrow-to-chunk)
             (bound-and-true-p ess-noweb-mode)
             (ess-noweb-narrow-to-chunk))
        (unless (ess-inside-string-p)
          (setq ess--fn-name-start-cache
                (condition-case nil ;; check if it is inside a functon
                    (progn
                      ;; for the sake of big buffers, look only 1000 chars back
                      (narrow-to-region (max (point-min) (- (point) 1000)) (point))
                      (up-list -1)
                      (while (not (looking-at "("))
                        (up-list -1))
                      (let ((funname (symbol-name (ess-symbol-at-point))))
                        (when (and funname
                                   (not (member funname ess-S-non-functions)))
                          (cons funname (- (point) (length funname))))))
                  (error nil))))))))

(defun ess-symbol-at-point ()
  "Like `symbol-at-point' but consider fully qualified names.
Fully qualified names include accessor symbols (like aaa$bbb and
aaa@bbb in R)."
  (with-syntax-table (or ess-mode-completion-syntax-table
                         (syntax-table))
    (symbol-at-point)))

(defun ess-bounds-of-symbol ()
  "Get bounds of symbol at point.
Intended for completion."
  (let ((bounds (with-syntax-table (or ess-mode-completion-syntax-table
                                       (syntax-table))
                  (bounds-of-thing-at-point 'symbol))))
    (and bounds
         (not (save-excursion
                (goto-char (car bounds))
                (looking-at "/\\|.[0-9]")))
         bounds)))

(defun ess-symbol-start ()
  "Get initial position for objects completion.
Symbols are fully qualified names that include accessor
symbols (like aaa$bbb and aaa@bbb in R)."
  (car (ess-bounds-of-symbol)))

(defun ess-arg-start ()
  "Get initial position for args completion."
  (when (not (ess-inside-string-p))
    (when (ess--fn-name-start)
      (if (looking-back "[(,]+[ \t\n]*" nil)
          (point)
        (ess-symbol-start)))))

(defun ess-inside-string-p (&optional pos)
  "Return non-nil if POS is inside string.
POS defaults to `point'."
  (let ((pos (or pos (point))))
    (nth 3 (syntax-ppss pos))))

(defun ess-inside-comment-p (&optional pos)
  "Return non-nil if POS is inside string.
POS defaults to `point'."
  (let ((pos (or pos (point))))
    (nth 4 (syntax-ppss pos))))

(defun ess-inside-string-or-comment-p (&optional pos)
  "Return non-nil if POS is inside a string or comment.
POS defaults to `point'."
  (or (ess-inside-string-p pos)
      (ess-inside-comment-p pos)))

(defun ess-inside-brackets-p (&optional pos curly?)
  "Return t if position POS is inside brackets.
POS defaults to point if no value is given. If CURLY?? is non nil
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


;;; String manipulation

(defun ess-quote-special-chars (string)
  (replace-regexp-in-string
   "\"" "\\\\\\&"
   (replace-regexp-in-string ;; replace backslashes
    "\\\\" "\\\\" string nil t)))

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
             (replace-match to-string fixedcase literal))))))

(defun ess-replace-regexp-dump-to-src (regexp to-string &optional dont-query verbose)
  "Replace REGEXP matches from beginning of buffer with TO-STRING.
If DONT-QUERY is non-nil, call `ess-rep-regexp' else call
`query-replace-regexp'. VERBOSE can be passed to `ess-rep-regexp'."
  (save-excursion
    (goto-char (point-min))
    (if dont-query
        (ess-rep-regexp     regexp to-string nil nil verbose)
      (query-replace-regexp regexp to-string nil))))

(defun ess-space-around (word &optional from verbose)
  "Replace-regexp .. ensuring space around all occurences of WORD.
Start at from FROM, which defaults to point."
  (interactive "d\nP"); Defaults: point and prefix (C-u)
  (save-excursion
    (goto-char from)
    (ess-rep-regexp (concat "\\([^ \t\n]\\)\\(\\<" word "\\>\\)")
                    "\\1 \\2" nil nil verbose)
    (goto-char from)
    (ess-rep-regexp (concat "\\(\\<" word "\\>\\)\\([^ \t\n]\\)")
                    "\\1 \\2" nil nil verbose)))

(defun ess-time-string (&optional clock)
  "Return a string for use as a timestamp, like \"13 Mar 1992\".
Include hr:min if CLOCK is non-nil. Redefine to taste."
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
  "[Dis]activates (ess-nuke-trailing-whitespace).
Disabled if nil; if t, it works unconditionally, otherwise,
the user is queried.
Note that setting the default to t may not be a good idea when you edit
binary files!")

;;; MM: Newer Emacsen now have  delete-trailing-whitespace
;;; --  but no customization like  nuke-trailing-whitespace-p ..
(defun ess-nuke-trailing-whitespace ()
  "Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs. This is a useful
function to put on `write-file-hooks'. If the variable
`ess-nuke-trailing-whitespace-p' is nil, this function is
disabled. If t, unreservedly strip trailing whitespace. If not
nil and not t, query for each instance."
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


;;; Debugging tools

(defun ess-write-to-dribble-buffer (text)
  "Write TEXT to `ess-dribble-buffer'."
  (when (or ess-verbose ess-write-to-dribble)
    (with-current-buffer (get-buffer-create ess-dribble-buffer)
      (goto-char (point-max))
      (insert-before-markers text))))

(defun ess-if-verbose-write (text)
  "Write TEXT to `ess-dribble-buffer' only if `ess-verbose' is non-nil."
  (when ess-verbose (ess-write-to-dribble-buffer text)))

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
  "Adjust face background between START and END.
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

(defun ess-sleep ()
  "Put Emacs to sleep for `ess-sleep-for-shell' seconds (floats work)."
  (sleep-for ess-sleep-for-shell))

(defun ess-setq-vars-local (alist &optional buf)
  "Set language variables from ALIST, in buffer BUF, if desired."
  (when buf (set-buffer buf))
  (mapc (lambda (pair)
          (make-local-variable (car pair))
          (set (car pair) (eval (cdr pair)))
          (when ess--make-local-vars-permanent
            (put (car pair) 'permanent-local t))) ;; hack for Rnw
        alist))

(defvar ess-error-regexp   "^\\(Syntax error: .*\\) at line \\([0-9]*\\), file \\(.*\\)$"
  "Regexp to search for errors.")

(define-obsolete-function-alias 'ess-beginning-of-function 'beginning-of-defun "ESS 19.04")
(define-obsolete-function-alias 'ess-end-of-function 'end-of-defun "ESS 19.04")

(provide 'ess-utils)

;;; ess-utils.el ends here
