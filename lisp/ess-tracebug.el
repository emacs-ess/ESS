;; ess-tracebug.el --- Tracing and debugging facilities for ESS.
;;
;; Copyright (C) 2011--2015 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.
;;
;; Filename: ess-tracebug.el
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2010-2012, Vitalie Spinu, all rights reserved.
;; Created: Oct 14 14:15:22 2010
;; URL: http://code.google.com/p/ess-tracebug/
;; Keywords: tools, languages
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;
;; Features that might be required by this library:
;;
;;   ESS - required
;;   ido, face-remap, cl -  desirable and are part of default emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  Ess-tracebug is a package for interactive debugging of R code from
;;  ESS and provides such features as:
;;  - visual debugging
;;  - browser, recover and conditional  breakpoints
;;  - watch window and loggers
;;  - on the fly  debug/undebug of R functions and methods
;;  - highlighting of error source references and easy error navigation
;;  - interactive traceback.
;;
;;  For a complete description please see the documentation at
;;  http://code.google.com/p/ess-tracebug/ and a brief tutorial at
;;  http://code.google.com/p/ess-tracebug/wiki/GettingStarted
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'tramp)
  (require 'compile)
  (require 'overlay)
  (require 'cl))

(autoload 'ess-helpobjs-at-point        "ess-help" "[autoload]" nil) ;;todo: rename and put into a more neutral place
(defvar text-scale-mode-amount)
(autoload 'text-scale-mode              "face-remap" "[autoload]" nil)

(defgroup ess-tracebug nil
  "Error navigation and debugging for ESS.
Currently only R is supported."
  :link '(emacs-library-link :tag "Source Lisp File" "ess-tracebug.el")
  :group 'ess)


(defcustom ess-tracebug-prefix nil
  "*Key to be used as prefix for all ess-tracebug commands.

Set this to a key cominbation you don't use often, like:

 (setq ess-tracebug-prefix \"\\M-t\")

The postfix keys are defined in `ess-tracebug-map':
\\{ess-tracebug-map}
"
  :type 'string
  :group 'ess-tracebug)

;; (make-obsolete-variable 'ess-tracebug-prefix "ess-tracebug-prefix will be removed in future versions.
;; Electric debug keys are now on C-c and ess-dev maps." "ESS 13.05")
;; (define-obsolete-variable-alias 'ess-tracebug-command-prefix 'ess-tracebug-prefix)


(defcustom ess-inject-source 'function-and-buffer
  "Control the source injection into evaluated code.

If t,  always inject source reference.
If function,  inject only for functions,
If function-and-buffer, inject for functions and whole buffer (the default),
If nil, never inject.

When tracebug is active (the default), ESS instructs the
subprocess to keep the source code references.

If this variable is t, you won't be able to execute blocks which
don't form a valid R expression. That is, if your expression
spreads multiple paragraphs, and you call C-c C-c on first
paragraph, R will report an error."
  :group 'ess-tracebug
  :type '(choice (const nil) (const function) (const function-and-buffer) (const t)))

(define-obsolete-variable-alias 'ess-tracebug-inject-source-p 'ess-inject-source "ESS v13.09")

(defcustom ess-tracebug-enter-hook nil
  "List of functions to call on entry to ess-tracebug mode.
Use `add-hook' to insert append your functions to this list.
"
  :group 'ess-tracebug
  :type 'hook)

(defcustom ess-tracebug-exit-hook nil
  "List of functions to call on exit of ess-tracebug mode.
Use `add-hook' to insert append your functions to this list.
"
  :group 'ess-tracebug
  :type 'hook)

(autoload 'ess-dev-map "ess-r-d" "[autoload]" nil)
(defvaralias 'ess-tracebug-map 'ess-dev-map)

(defvar ess--tracebug-eval-index 0
  "This is used by to track source references in evaluation with source.
For example, each time ess-eval-function is called the evaluated
region is marked.  When debugger enteres the code it desplayes
this reference number. Ess-debug finds this number in the
referenced buffer.")

;; these vars are org variables that store the src block locations
(defvar org-edit-src-beg-marker nil)
(defvar org-babel-current-src-block-location nil
  "Marker pointing to the src block currently being executed.
This may also point to a call line or an inline code block.  If
multiple blocks are being executed (e.g., in chained execution
through use of the :var header argument) this marker points to
the outer-most code block.")

;; hash to store soruce references of the form: tmpname -> (filename . src_start)
(defvar ess--srcrefs (make-hash-table :test 'equal :size 100))

(defvar ess-tracebug-original-buffer-marker nil
  "Marker pointing to the beginning of original source code.

If non-nil, tracebug will insert the source references based on
this location instead of the current buffer. This is useful for
applications, like org-babel,  that call ess evaluation functions
from temporary buffers.")

(defun ess-tracebug-p ()
  (ess-process-get 'tracebug))

(defun ess-make-source-refd-command (beg end visibly process)
  "Saves a region to a temporary file in order to add source references.
BEG and END delimit the region.

Returns a string containing an inferior process command for
loading the temporary file.  This command conforms to VISIBLY."
  (let* ((filename buffer-file-name)
         (proc-dir (ess-get-process-variable 'default-directory))
         (remote (when (file-remote-p proc-dir)
                   (require 'tramp)
                   ;; should this be done in process buffer?
                   (tramp-dissect-file-name proc-dir)))
         (orig-marker (or ess-tracebug-original-buffer-marker
                          org-edit-src-beg-marker
                          org-babel-current-src-block-location))
         orig-beg)
    (setq ess--tracebug-eval-index (1+ ess--tracebug-eval-index))
    (goto-char beg)
    (skip-chars-forward " \t\n")
    (setq beg (point))
    (goto-char end)
    (skip-chars-backward " \t\n")
    (setq end (point)
          orig-beg beg)

    ;; Delete all old temp files
    (when (and (not (ess-process-get 'busy))
               (< 1 (float-time
                     (time-subtract (current-time)
                                    (ess-process-get 'last-eval)))))
      (dolist (f (ess-process-get 'temp-source-files))
        (and (file-exists-p f)
             (delete-file f)))
      (ess-process-put 'temp-source-files nil))

    (when (markerp orig-marker)
      (setq filename (buffer-file-name (marker-buffer orig-marker)))
      (setq orig-beg (+ beg (marker-position orig-marker))))

    (let ((tmpfile
           (expand-file-name (make-temp-name
                              (concat (file-name-nondirectory
                                       (or filename "unknown")) "!"))
                             (if remote
                                 (tramp-get-remote-tmpdir remote)
                               temporary-file-directory))))

      (ess-process-put 'temp-source-files
                       (cons tmpfile (ess-process-get 'temp-source-files)))

      (when remote
        ;; Get local name (should this be done in process buffer?)
        (setq tmpfile (with-parsed-tramp-file-name tmpfile nil localname)))

      (if (not filename)
          (puthash tmpfile (list nil ess--tracebug-eval-index nil) ess--srcrefs)
        (puthash tmpfile (list filename ess--tracebug-eval-index orig-beg) ess--srcrefs)
        (puthash (file-name-nondirectory tmpfile) ; R sometimes strips dirs
                 (list filename ess--tracebug-eval-index orig-beg) ess--srcrefs)
        (with-silent-modifications
          (put-text-property beg end 'tb-index ess--tracebug-eval-index)))
      (let ((string (ess-process-buffer-substring process start end)))
        (or
         ;; Sending string to subprocess is considerably faster than tramp file
         ;; transfer. So, give priority to `ess-eval-command' if available
         (ess-build-eval-command string visibly t tmpfile)
         ;; When no `ess-eval-command' available, use `ess-load-command'
         (progn
           (write-region beg end tmpfile nil 'silent)
           (ess-build-load-command tmpfile visibly t)))))))

(defun ess-process-buffer-substring (process start end)
  (ess--run-presend-hooks process (buffer-substring-no-properties start end)))

(defun ess-tracebug-send-region (process start end &optional visibly message type)
  "Send region to process adding source references as specified
by `ess-inject-source' variable."
  (let* ((inject-p  (cond ((eq type 'function)
                           ess-inject-source)
                          ((eq type 'buffer)
                           (or (eq ess-inject-source t)
                               (eq ess-inject-source 'function-and-buffer)))
                          (t (or (eq ess-inject-source t)
                                 ;; we need to always inject with namespaced
                                 ;; evaluation (fixme: not right place for this)
                                 (ess-r-get-evaluation-env)))))
         (ess--dbg-del-empty-p (unless inject-p ess--dbg-del-empty-p))
         (string (if inject-p
                     (ess-make-source-refd-command start end visibly process)
                   (ess-process-buffer-substring process start end)))
         (message (if (fboundp ess-build-eval-message-function)
                      (funcall ess-build-eval-message-function message)
                    message))
         ;; Visible evaluation is not nice when sourcing temporary files
         ;; You get .ess.eval(*code*) instead of *code*
         (visibly (unless inject-p visibly)))
    ;; Don't run the presend hooks twice
    (let ((ess--inhibit-presend-hooks t))
      (ess-send-string process string visibly message))))

(defun ess-tracebug-send-function (proc start end &optional visibly message)
  "Like `ess-tracebug-send-region' but with tweaks for functions."
  (ess-tracebug-send-region proc start end visibly message 'function))

(defvar ess-tracebug-help nil
  "ess-dev-map prefix: \\[ess-dev-map]

* Breakpoints (`ess-dev-map'):

 b   . Set BP (repeat to cycle BP type) . `ess-bp-set'
 B   . Set conditional BP               . `ess-bp-set-conditional'
 k   . Kill BP                          . `ess-bp-kil'
 K   . Kill all BPs                     . `ess-bp-kill-all'
 o   . Toggle BP state                  . `ess-bp-toggle-state'
 l   . Set logger BP                    . `ess-bp-set-logger'
 n   . Goto next BP                     . `ess-bp-next'
 p   . Goto previous BP                 . `ess-bp-previous'

  (C- prefixed equivalents are also defined)

* Debugging (`ess-dev-map'):
 `   . Show traceback                       . `ess-show-traceback' (also on C-c `)
 ~   . Show callstack                       . `ess-show-call-stack' (also on C-c ~)
 e   . Toggle error action (repeat to cycle). `ess-debug-toggle-error-action'
 d   . Flag for debugging                   . `ess-debug-flag-for-debugging'
 u   . Unflag for debugging                 . `ess-debug-unflag-for-debugging'
 w   . Watch window                         . `ess-watch'

  (C- prefixed equivalents are also defined)

* Interactive Debugging (`ess-debug-minor-mode-map'):

 M-C   . Continue                  . `ess-debug-command-continue'
 M-C-C . Continue multi            . `ess-debug-command-continue-multi'
 M-N   . Next step                 . `ess-debug-command-next'
 M-C-N . Next step multi           . `ess-debug-command-next-multi'
 M-U   . Up frame                  . `ess-debug-command-up'
 M-Q   . Quit debugging            . `ess-debug-command-quit'

* Navigation to errors (general emacs functionality):

 C-x `, M-g n   . `next-error'
 M-g p          . `previous-error'

")


;; * Input Ring:

;; i   . Goto input event marker forwards     . `ess-debug-goto-input-event-marker'
;; I   . Goto input event marker backwards    . `ess-debug-goto-input-event-marker'


(defun ess-tracebug-show-help (&optional ev)
  (interactive)
  (describe-variable 'ess-tracebug-help))

(defun ess-tracebug--propertize (dummy bitmap face &optional string )
  "If window-system propertize DUMMY with fringe BITMAP and FACE,
else propertize  line-prefix and margin  with STRING and  FACE"
  (unless string
    (setq string dummy))
  (if window-system
      (propertize dummy 'display (list 'left-fringe bitmap face))
    (propertize dummy
                'display (list '(margin left-margin)
                               (propertize string
                                           'font-lock-face face
                                           'face face)))))


(defun ess-tracebug (&optional arg)
  "Toggle ess-tracebug mode.
With ARG, turn ess-tracebug mode on if and only if ARG is
positive.

This mode adds to ESS the interactive debugging, breakpoint and
error navigation functionality.  Strictly speaking ess-tracebug
is not a minor mode. It integrates globally into ESS and iESS.

Note: Currently, ess-tracebug does not detect some of R's debug
related messages in non-English locales. To set your R messages
to English add the following line to your .Rprofile init file:

   Sys.setlocale(\"LC_MESSAGES\", \"C\")


See `ess-tracebug-help' for the overview of ess-tracebug functionality."

  ;; Note: The functionality in ess-tracebug is divided on conceptual
  ;; grounds in tracing and debugging and could be
  ;; activated/deactivate separately with `ess--tb-start' and
  ;; `ess-debug-start' respectively.

  (interactive "P")
  (ess-force-buffer-current "R process to activate tracebug in: ")
  (with-current-buffer (process-buffer (get-process ess-local-process-name))
    (when (equal ess-dialect "R")
      (setq arg
            (if arg
                (prefix-numeric-value arg)
              (if (ess-process-get 'tracebug) -1 1)))
      (if (> arg 0)
          (unless (ess-process-get 'tracebug) ;; only if already not active
            (ess--tb-start)
            (ess-debug-start)
            ;; (dolist (bf (buffer-list))
            ;;   (with-current-buffer bf
            ;;     (when (and (eq major-mode 'ess-mode)
            ;;                (equal ess-dialect "R"))
            ;;       (ess-bp-recreate-all))))
            ;; watch functionality
            (if ess-tracebug-prefix
                (let ((comm (key-binding ess-tracebug-prefix)))
                  ;; (message "ess-tracebug-prefix will be removed in future versions. Electric debug keys are now on [C-c] and [C-c C-t] maps.")
                  ;; (sit-for 1)
                  (when (commandp comm)
                    (define-key ess-tracebug-map ess-tracebug-prefix comm))
                  (define-key ess-mode-map ess-tracebug-prefix ess-tracebug-map)
                  (define-key inferior-ess-mode-map ess-tracebug-prefix ess-tracebug-map)
                  (define-key ess-watch-mode-map ess-tracebug-prefix ess-tracebug-map)))
            (run-hooks 'ess-tracebug-enter-hook)
            (ess-process-put 'tracebug t)
            (message "ess-tracebug mode enabled"))
        (when (ess-process-get 'tracebug) ;;only when active
          (ess-process-put  'tracebug nil)
          ;; unset the map
          (when ess-tracebug-prefix
            (define-key ess-mode-map ess-tracebug-prefix nil)
            (define-key inferior-ess-mode-map ess-tracebug-prefix nil))
          (ess--tb-stop)
          (ess-debug-stop)
          (run-hooks 'ess-tracebug-exit-hook)
          (message "ess-tracebug mode disabled"))))))

(defalias 'ess-toggle-tracebug 'ess-tracebug)


;;;_* TRACEBACK

;; (defface ess--tb-last-input-face
;;   '((((class grayscale)
;;       (background light)) (:background "DimGray"))
;;     (((class grayscale)
;;       (background dark))  (:background "LightGray"))
;;     (((class color) (background light) (min-colors 88))
;;      (:overline "medium blue" ))
;;     (((class color) (background dark) (min-colors 88))
;;      (:overline "deep sky blue" ))
;;     (((background light))  (:weight bold))
;;     (((background dark)) (:weight bold))
;;     )
;;   "Face to highlight currently debugged line."
;;   :group 'ess-tracebug )

(defface ess-tracebug-last-input-fringe-face
  '((((background light) (min-colors 88)) (:foreground "medium blue" :overline "medium blue"))
    (((background dark) (min-colors 88))  (:foreground "deep sky blue" :overline "deep sky blue"))
    (((background light) (min-colors 8))  (:foreground "blue"))
    (((background dark) (min-colors 8))  (:foreground "syan")))
  "Face for fringe bitmap for last-input position."
  :group 'ess-tracebug)

(if (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'last-input-arrow
      [#b00011111
       #b00010000
       #b00010000
       #b00010000
       #b00010000
       #b00010000
       #b00010000
       #b00010000
       #b00010000
       #b00010000
       #b11010111
       #b01111100
       #b00111000
       #b00010000] nil nil 'top))


(defvar ess--tb-last-input (make-marker)
  "Marker pointing to the last user input position in iESS buffer.
This is the place where `ess--tb-last-input-overlay' is moved.
Local in iESS buffers with `ess-tracebug' mode enabled.")

(defvar ess--tb-last-input-overlay nil
  "Overlay to highlight the position of last input in iESS buffer.
Local in iESS buffers.")

(defvar ess--busy-count 0
  "Used to compute the busy indicator")
(make-variable-buffer-local 'ess--busy-count)

;; (unless (boundp 'ess--busy-slash)
;; (defvar ess--busy-slash '(32 ?\u2014 92 47))
;; (setq ess--busy-slash (mapcar (lambda (el) (format " %c " el))
;;                               ess--busy-slash))
;; )


(defvar ess--busy-slash '("   " " - " " \\ " " / "))
(defvar ess--busy-B '("   " " B " "   "))
(defvar ess--busy-stars '("      " "      " " *    " " **   " " ***  " " **** "))
(defvar ess--busy-vbars '("      " "      " " |    " " ||   " " |||  " " |||| "))

(defcustom ess-busy-strings ess--busy-slash
  "List of strings to replace in turn for busy indication.
The first element of the list is used as an indicator of the
process being ready (i.e. not busy). Implemented lists that you
can use `ess--busy-slash', `ess--busy-B',`ess--busy-stars',
`ess--busy-vbars'"
  :group 'ess)

(defvar ess--busy-timer nil
  "Timer used for busy process indication")

(defcustom inferior-ess-split-long-prompt t
  "If non-nil, long prompt '> > > > > + + + + > ' is split."
  :group 'ess-tracebug)

(defcustom inferior-ess-replace-long+ t
  "If non-nil,  '+ + + + ' containing more than 4 + is replaced by `ess-long+replacement'"
  :group 'ess-tracebug)

(defvar ess-long+replacement "+ . + "
  "Replacement used for long + prompt.
Customization of this variable is not recommended. You can set it
to '. '. If you set it to anything else you will have to change
`inferior-S-prompt' to assure the correct prompt navigation
in inferior buffers.  ")

(defmacro ess-copy-key (from-map to-map fun)
  `(define-key ,to-map
     (car (where-is-internal ,fun  ,from-map))
     ,fun))

;;;_ + traceback functions
(defun ess--tb-make-last-input-overlay (beg end)
  "Create an overlay to indicate the last input position."
  (let   ((ove (make-overlay beg end)))
    (overlay-put ove 'before-string
                 (ess-tracebug--propertize "!"  'last-input-arrow 'ess-tracebug-last-input-fringe-face))
    ;; (overlay-put ove 'face  'ess--tb-last-input-face)
    (overlay-put ove 'evaporate t)
    ove))


(defun ess--tb-start ()
  "Start traceback session "
  (with-current-buffer (process-buffer (get-process ess-local-process-name))
    (unless ess-error-regexp-alist
      (error "Can not activate the traceback for %s dialect" ess-dialect))
    (set (make-local-variable 'compilation-error-regexp-alist)
         ess-error-regexp-alist)
    (let (compilation-mode-font-lock-keywords)
     (compilation-setup t))
    (setq next-error-function 'ess-tracebug-next-error-function)
    ;; new locals
    (make-local-variable 'ess--tb-last-input)
    (make-local-variable 'ess--tb-last-input-overlay)
    (make-local-variable 'compilation-search-path)
    (setq compilation-search-path ess-tracebug-search-path) ;; todo: make this dialect specific
    (ess-tracebug--set-left-margin)
    (save-excursion
      (goto-char comint-last-input-start)
      (setq ess--tb-last-input (point))
      (setq ess--tb-last-input-overlay
            (ess--tb-make-last-input-overlay
             (point-at-bol) (point-at-eol))))
    ;; busy timer
    (setq mode-line-buffer-identification
          (list (car (propertized-buffer-identification "%3b"))
                `(:eval  (nth ess--busy-count ess-busy-strings)))) ;; 'face 'mode-line-buffer-id))))
    (make-local-variable 'ess--busy-timer)
    (setq ess--busy-timer
          (run-with-timer 2 .5 (ess--make-busy-timer-function (get-buffer-process (current-buffer)))))
    (add-hook 'kill-buffer-hook (lambda () (when ess--busy-timer (cancel-timer ess--busy-timer))))
    (add-hook 'comint-input-filter-functions  'ess-tracebug-set-last-input nil 'local)

    ;; redefine
    ;; todo: all this part should go (partially gone now)
    (unless (fboundp 'orig-ess-parse-errors)
      (defalias 'orig-ess-parse-errors (symbol-function 'ess-parse-errors))
      (defalias 'ess-parse-errors (symbol-function 'next-error)))))

(defun ess--tb-stop ()
  "Stop ess traceback session in the current ess process"
  (with-current-buffer (process-buffer (get-process ess-current-process-name))
    ;; restore original definitions
    (when (equal ess-dialect "R")
      (when (fboundp 'orig-ess-parse-errors)
        (defalias 'ess-parse-errors (symbol-function 'orig-ess-parse-errors))
        (fmakunbound 'orig-ess-parse-errors)))
    (if (local-variable-p 'ess--tb-last-input-overlay)
        (delete-overlay ess--tb-last-input-overlay))
    (kill-local-variable 'ess--tb-last-input-overlay)
    (kill-local-variable 'ess--tb-last-input)
    (font-lock-remove-keywords nil (compilation-mode-font-lock-keywords))
    (font-lock-fontify-buffer)
    (kill-local-variable 'compilation-error-regexp-alist)
    (kill-local-variable 'compilation-search-path)
    (cancel-timer ess--busy-timer)
    (remove-hook 'comint-input-filter-functions  'ess-tracebug-set-last-input 'local)
    (setq mode-line-buffer-identification (propertized-buffer-identification "%12b"))))

(defvar ess--dbg-forward-ring (make-ring 10)
  "Ring of markers to the positions of the user inputs
 when the  debugger or traceback events are initiated.  It is used in
 `ess--dbg-goto-input-point'.")

(defvar ess--dbg-backward-ring (make-ring 10)
  "Ring of markers to the positions from which `ess--dbg-goto-input-point' is called.
 See the also `ess--dbg-goto-debug-point'")


;; (setq ess-R--tb-regexp-alist '(R R2 R3 R-recover))
;;(pop compilation-error-regexp-alist-alist)

(defun ess-show-traceback ()
  "Display R traceback and last error message.
Pop up a compilation/grep/occur like buffer. Usual global key
bindings are available \(\\[next-error] and \\[previous-error]\)
for `next-error' and `previous-error' respectively.

You can bind 'no-select' versions of this commands:
\(define-key compilation-minor-mode-map [(?n)] 'next-error-no-select\)
\(define-key compilation-minor-mode-map [(?p)] 'previous-error-no-select\)
"
  (interactive)
  (if (null ess-traceback-command)
      (error "Not implemented for dialect %s" ess-dialect)
    (ring-insert ess--dbg-forward-ring (point-marker))
    (ess-force-buffer-current "R process to use: ")
    (let ((trbuf  (get-buffer-create "*ess-traceback*"))
          (lproc-name ess-local-process-name)
          (alist ess-mode-editing-alist)
          (cmd ess-traceback-command))
      (setq next-error-last-buffer trbuf)
      (with-current-buffer trbuf
        (setq buffer-read-only nil)
        (setq ess-local-process-name lproc-name)
        (ess-command cmd trbuf)
        (goto-char (point-min))
        ;; fixme: this is R specific check
        (if (re-search-forward "No traceback available" nil t)
            (message "No traceback available")
          (ess-dirs)
          (message nil)
          (make-local-variable 'compilation-error-regexp-alist)
          (setq compilation-error-regexp-alist ess-R-error-regexp-alist)
          (make-local-variable 'compilation-search-path)
          (setq compilation-search-path ess-tracebug-search-path)
          (ess-setq-vars-local alist)
          (font-lock-refresh-defaults)
          (compilation-minor-mode 1)
          (setq next-error-function 'ess-tracebug-next-error-function)
                                        ;(use-local-map ess-traceback-minor-mode-map)
          ;; ess keys
          (local-set-key "\C-c\C-s" 'ess-watch-switch-process)
          (local-set-key "\C-c\C-y" 'ess-switch-to-ESS)
          (local-set-key "\C-c\C-z" 'ess-switch-to-end-of-ESS)

          (setq buffer-read-only t)
          (pop-to-buffer trbuf))))))

(defvar ess-call-stack-command nil)
(defun ess-show-call-stack ()
  "Display current call stack.
Also see `ess-show-traceback'"
  (interactive)
  (let ((ess-traceback-command ess-call-stack-command))
    (ess-show-traceback)))

(defalias 'ess-show-R-traceback 'ess-show-traceback)

(defun ess--tb-next-error-goto-process-marker ()
  ;; assumes current buffer is the process buffer with compilation enabled
  ;; used in ess-tracebug-next-error-function
                                        ;  (with-current-buffer (process-buffer (get-process ess-local-process-name)) ; already in comint buffer .. no need
  (comint-goto-process-mark)
  (set-window-point (get-buffer-window) (point))  ;moves the cursor
  ;; FIXME: Should jump to current-debug-position,  but messes the things if in recover
  ;; (when (ess-debug-is-active)
  ;;   (ess-debug-goto-current-debug-position)
  ;;   )
  )

(defun ess-tracebug-next-error-function (n &optional reset)
  "Advance to the next error message and visits the file.
This is the value of `next-error-function' in iESS buffers."
  ;; Modified version of `compilation-next-error-function'.
  (interactive "p")
  (if reset  (goto-char (point-max)))
  (let* ((columns compilation-error-screen-columns) ; buffer's local value
         ;; (proc (or (get-buffer-process (current-buffer))
         ;;                         (error "Current buffer has no process")))
         (pbuff-p (get-buffer-process (current-buffer)))
         (last 1)
         (n (or n 1))
         timestamp
         (beg-pos  ; from where the search for next error starts
          (if (and pbuff-p
                   (>= n 0)
                   (comint-after-pmark-p))
              ess--tb-last-input
            (point)))
         (at-error t)
         (msg
          (condition-case err
              (compilation-next-error n  nil beg-pos)
            (error
             (when pbuff-p
               (ess--tb-next-error-goto-process-marker))
             (if (< n 0)
                 (message "Before first reference")
               (message "Beyond last reference"));(error-message-string err))
             (setq at-error nil))))
         (msg (if (or (not pbuff-p)
                      (eq n 0)
                      (> (point) ess--tb-last-input))
                  msg
                (ess--tb-next-error-goto-process-marker)
                (message "Beyond last-input marker")
                (setq at-error nil)))
         (marker (point-marker))
         loc end-loc)
    (when at-error
      (setq compilation-current-error (point-marker)
            overlay-arrow-position (if (bolp)
                                       compilation-current-error
                                     (copy-marker (line-beginning-position)))
            loc (if (fboundp 'compilation--message->loc)
                    (compilation--message->loc msg)
                  (car msg))
            end-loc (if (fboundp  'compilation--message->end-loc) ;; emacs 24
                        (compilation--message->end-loc msg)
                      (nth 2 msg)))
      (let* ((file (caar (nth 2 loc)))
             (col (car loc))
             (line (cadr loc))
             (mkrs (ess--dbg-create-ref-marker file line col)))
        (if mkrs
            ;; is this really needed? Shall we go directly to the location?
            (compilation-goto-locus marker (car mkrs) (cadr mkrs))
          (message "Reference to '%s' not found" file))))))


(defun inferior-ess-move-last-input-overlay ()
  "Move the overlay to the point."
  (let ((pbol (point-at-bol)))
    (move-overlay ess--tb-last-input-overlay
                  pbol (max (- (point) 2) (+ pbol 2)))))


;;;_* DEBUGGER
(defgroup ess-debug nil
  "Debugging for ESS"
  :link '(emacs-library-link :tag "Source Lisp File" "ess-tracebug.el")
  :group 'ess-tracebug
  :prefix "ess-debug-")

(defcustom  ess-debug-error-action-alist
  '(( ""   "NONE"       "NULL" )
    ( " r" "RECOVER"    "utils::recover")
    ( " t" "TRACEBACK"  "base::traceback"))
  "Alist of 'on-error' actions.
Toggled with `ess-debug-toggle-error-action'.  Each element must
have the form (DISP SYMB ACTION) where DISP is the string to be
displayed in the mode line when the action is in place. SYMB is
the symbolic name of an action. ACTION is the string giving the
actual expression to be assigned to 'error' user option. See R's
help ?options for more details.
"
  :type '(alist :key-type string
                :value-type (string string))
  :group 'ess-debug)

(defvar ess--dbg-output-buf-prefix " *ess.dbg"
  "The prefix of the buffer name the R debug output is directed to."  )

(defvar ess--dbg-current-ref (make-marker)
  "Current debug reference in *ess.dbg* buffers (a marker).")
(make-variable-buffer-local 'ess--dbg-current-ref)

(defvar ess--dbg-last-ref-marker (make-marker)
  "Last debug reference in *ess.dbg* buffer (a marker).")
(make-variable-buffer-local 'ess--dbg-last-ref-marker)

(defcustom ess-tracebug-search-path nil
  "List of directories to search for source files.
Elements should be directory names, not file names of directories.
"
  :type '(choice (const :tag "Unset" nil)
                 (repeat :tag "Directory list" (string :tag "Directory")))
  :group 'ess-debug)
(defalias 'ess--dbg-search-path 'ess-tracebug-search-path)
(make-obsolete 'ess--dbg-search-path 'ess-tracebug-search-path "ESS[12.09]")

(defvar ess--dbg-buf-p nil
  "This is t in ess.dbg buffers.")
(make-variable-buffer-local 'ess--dbg-buf-p)

;; (defcustom ess--dbg-auto-single-key-p t
;;   "If t entering the debug state triggers single-key mode.
;; Set it to nil if you want to trigger single-key mode manually
;; with the `ess-tracebug-prefix' key.
;; ")

(defvar ess--dbg-current-debug-position (make-marker)
  "Marker to the current debugged line.
 It always point to the beginning of the currently debugged line
and is used by overlay-arrow.
In no-windowed emacs an `overlay-arrow' is displayed at this position.")

(unless  window-system
  (add-to-list 'overlay-arrow-variable-list 'ess--dbg-current-debug-position))

(defface ess-debug-current-debug-line-face
  '((default (:inherit highlight)))
  "Face used to highlight currently debugged line."
  :group 'ess-debug)


(defvar  ess--dbg-current-debug-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  'ess-debug-current-debug-line-face)
    (overlay-put overlay 'evaporate t)
    overlay)
  ;; should be global variable!!
  "The overlay for currently debugged line.")


(defcustom ess-debug-blink-interval .2
  "Time in seconds to blink the background
 of the current debug line on exceptional events.
 Currently two exceptional events are defined 'ref-not-found'
 and 'same-ref'. Blinking colors for these events can be
 customized by corresponding faces."
  :group 'ess-debug
  :type 'float)

(defface ess-debug-blink-ref-not-found-face
  '((((class grayscale) (background light)) (:background "DimGray"))
    (((class grayscale) (background dark))  (:background "LightGray"))
    (((class color) (background light) (min-colors 88)) (:background "IndianRed4"))
    (((class color) (background dark) (min-colors 88))  (:background "dark red"))
    (((background light) (min-colors 8))  (:foreground "red"))
    (((background dark) (min-colors 8))  (:foreground "red")))
  "Face used to blink currently debugged line's background
 when the reference file is not found. See also `ess-debug-ask-for-file'"
  :group 'ess-debug )

(defface ess-debug-blink-same-ref-face
  '((((class grayscale) (background light)) (:background "DimGray"))
    (((class grayscale) (background dark))  (:background "LightGray"))
    (((class color) (background light) (min-colors 88)) (:background "steel blue"))
    (((class color) (background dark) (min-colors 88))  (:background "midnight blue"))
    (((background light) (min-colors 8))  (:foreground "blue"))
    (((background dark) (min-colors 8))  (:foreground "cyan")))
  "Face used to highlight currently debugged line when new debug
reference is the same as the preceding one. It is highlighted for
`ess-debug-blink-interval' seconds."
  :group 'ess-debug )

(defcustom ess-debug-ask-for-file nil
  "If non nil, ask for file if the current debug reference is not found.

If nil, the currently debugged line is highlighted for
`ess-debug-blink-interval' seconds."
  :group 'ess-debug
  :type 'boolean)

(defcustom ess-debug-skip-first-call t
  "If non-nil, skip first debugger call.

In R first call doesn't contain source references and is skipped
by default."
  :group 'ess-debug
  :type 'boolean)

(defvar ess-electric-selection-map
  (let (ess-electric-selection-map)
    (define-prefix-command 'ess-electric-selection-map)
    ;; command-c and command-Q are not always working reliably
    (define-key ess-electric-selection-map "\M-N" 'ess-debug-command-continue)
    (define-key ess-electric-selection-map "\M-C" 'ess-debug-command-continue)
    (define-key ess-electric-selection-map "\M-Q" 'ess-debug-command-quit)
    (define-key ess-electric-selection-map "0" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "1" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "2" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "3" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "4" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "5" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "6" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "7" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "8" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "9" 'ess-debug-command-digit)
    (define-key ess-electric-selection-map "?" 'ess-tracebug-show-help)
    ess-electric-selection-map)
  "Keymap used to define commands for single key input mode.
This commands are triggered by `ess-electric-selection' .

\\{ess-electric-selection-map}
")

(ess-if-verbose-write "\n<- debug-vars done")

;;;_ + debug functions
(defun ess-debug-set-error-action (spec)
  "Set the on-error action. The ACTION should be  one
of components of `ess-debug-error-action-alist' (a cons!)."
  (let ((proc (get-process ess-local-process-name)))
    (if spec
        (with-current-buffer (process-buffer proc)
          (process-put proc 'on-error-action (car spec))
          (ess-command (format "options(error= %s )\n" (nth 2 spec))))
      (error "Unknown action."))))

(defun ess-debug-toggle-error-action ()
  "Toggle the 'on-error' action.
The action list is in `ess-debug-error-action-alist'. "
  (interactive)
  (ess-force-buffer-current)
  (let* ((alist ess-debug-error-action-alist)
         (ev last-command-event)
         (com-char  (event-basic-type ev))
         (cur-action (or (ess-process-get 'on-error-action)
                         "-"))
         actions act)
    (setq actions
          (cdr (member (assoc cur-action ess-debug-error-action-alist)
                       ess-debug-error-action-alist)))
    (unless actions
      (setq actions ess-debug-error-action-alist))
    (setq act (pop actions))
    (ess-debug-set-error-action act)
    (message "On-error action set to: %s"
             (propertize (cadr act) 'face 'font-lock-function-name-face))
    (while  (eq (event-basic-type (setq ev (read-event))) com-char)
      (unless actions
        (setq actions ess-debug-error-action-alist))
      (setq act (pop actions))
      (ess-debug-set-error-action act)
      (force-mode-line-update)
      (message "On-error action set to: %s"
               (propertize (cadr act) 'face 'font-lock-function-name-face)))
    (push ev unread-command-events)))

(defun ess--dbg-activate-overlays ()
  "Initialize active debug line overlays."
  (move-overlay ess--dbg-current-debug-overlay
                (point-at-bol) (1+ (point-at-eol)) (current-buffer))
  ;; used by overlay-arrow functionality on no-X,  should be bol
  (move-marker ess--dbg-current-debug-position (point-at-bol)))

(defun ess--dbg-deactivate-overlays ()
  "Deletes markers and overlays. Overlay arrow remains to indicate the last debug position."
  (delete-overlay ess--dbg-current-debug-overlay)
  (set-marker ess--dbg-current-debug-position nil))


;;;_ + Work Flow
(defun ess-debug-goto-input-event-marker ()
  "Jump to the point where the last debugger/traceback etc event occurred.

   Mainly useful during/after debugging, to jump to the place
from where the code was initialy executed.  This is an
electric-command, which means that after the command is triggered a
single key event is enough to navigate through the input-event-S-ring.
If the key-event which triggered the command is Shift modified
the input-event-S-ring is traversed backwards.

The input-event-S-ring is a virtual object which consists of two
rings `ess--dbg-forward-ring' and `ess--dbg-backward-ring' which
are joint at their tops.

See the more info at http://code.google.com/p/ess-tracebug/#Work-Flow
"
  (interactive)
  (let* ((ev last-command-event)
         (com-char  (event-basic-type ev))
         (ring-el 0)
         input-point)
    (if (memq 'shift (event-modifiers ev))
        (setq input-point (ring-ref ess--dbg-backward-ring 0))
      (ring-insert ess--dbg-backward-ring (point-marker)) ;; insert in backward ring ;;todo: check if the marker to this (close by?) position is already in the ring
      (setq input-point (ring-ref ess--dbg-forward-ring 0)))
    (when (marker-buffer input-point) ;; todo: give a message here if buff is not found
      (switch-to-buffer (marker-buffer input-point))
      (when (marker-position input-point)
        (goto-char (marker-position input-point))))
    (while  (eq (event-basic-type  (event-basic-type (setq ev (read-event)))) com-char)
      (if (memq 'shift (event-modifiers ev))
          (setq ring-el (1- ring-el))
        (setq ring-el (1+ ring-el)))
      (if (< ring-el 0)
          (setq input-point (ring-ref ess--dbg-backward-ring (- ring-el)))  ;; get it from backward-ring
        ;; get it from forward-ring
        (setq input-point (ring-ref ess--dbg-forward-ring ring-el)) )
      (when (marker-buffer input-point)
        (switch-to-buffer (marker-buffer input-point))
        (when (marker-position input-point)
          (goto-char (marker-position input-point)))))
    (push ev unread-command-events)))

(defun ess-debug-goto-debug-point ()
  "Returns to the debugging position.
Jump to markers stored in `ess--dbg-backward-ring'. If debug
session is active, first jump to current debug line.

This is an electric-command. Shift triggers the opposite traverse
of the ring."
  (interactive)
  (let* ((debug-point (ring-ref ess--dbg-backward-ring 0))
         (ev last-command-event)
         (com-char  (event-basic-type ev))
         (ring-el 0))
    (if (ess--dbg-is-active-p)
        (progn
          (switch-to-buffer (marker-buffer ess--dbg-current-debug-position))
          (goto-char (marker-position ess--dbg-current-debug-position ))
          (back-to-indentation))
      (switch-to-buffer (marker-buffer debug-point))
      (goto-char (marker-position debug-point)))
    (while  (eq (event-basic-type (setq ev (read-event))) com-char)
      (if (memq 'shift (event-modifiers ev))
          (setq ring-el (1- ring-el))
        (setq ring-el (1+ ring-el)))
      (setq debug-point (ring-ref ess--dbg-backward-ring ring-el))
      (when (marker-buffer debug-point)
        (switch-to-buffer (marker-buffer debug-point))
        (when (marker-position debug-point)
          (goto-char (marker-position debug-point)))))
    (push ev unread-command-events)))

(defun ess-debug-insert-in-forward-ring ()
  (interactive)
  "Inserts point-marker into the forward-ring."
  (ring-insert ess--dbg-forward-ring (point-marker))
  (message "Point inserted into the forward-ring"))

(defvar ess-debug-indicator " DB"
  "String to be displayed in mode-line alongside the process
  name. Indicates that ess-debug-mode is turned on. When the
  debugger is in active state this string is shown in upper case
  and highlighted.")

(defvar-local ess--dbg-mode-line-debug
  '(:eval (let ((proc (get-process ess-local-process-name)))
            (if (and proc (process-get proc 'dbg-active))
                (let ((str ess-debug-indicator))
                  (ess-debug-minor-mode 1) ; activate the keymap
                  (put-text-property 1 (length str)
                                     'face '(:foreground "white" :background "red")
                                     str)
                  str)
              (ess-debug-minor-mode -1)
              ""))))
(put 'ess--dbg-mode-line-debug 'risky-local-variable t)

(defvar-local ess--dbg-mode-line-error-action
  '(:eval (or (and (ess-process-live-p)
                   (ess-process-get 'on-error-action))
              "")))
(put 'ess--dbg-mode-line-error-action 'risky-local-variable t)

(defun ess--dbg-remove-empty-lines (string)
  "Remove empty lines (which interfere with evals) during debug.

This function is placed in `ess-presend-filter-functions'.
"
  ;; the process here is an ugly reliance on dynamic scope
  (if (and ess--dbg-del-empty-p (process-get process 'dbg-active))
      (replace-regexp-in-string "\n\\s *$" "" string)
    string))


(defun ess-debug-start ()
  "Start the debug session.
Add to ESS the interactive debugging functionality, breakpoints,
watch and loggers.  Integrates into ESS and iESS modes by binding
`ess-tracebug-map' to `ess-tracebug-prefix' in
`ess-mode-map' and `inferior-ess-mode-map' respectively."
  (interactive)
  (let ((dbuff (get-buffer-create (concat ess--dbg-output-buf-prefix "." ess-current-process-name "*"))) ;todo: make dbuff a string!
        (proc (ess-get-process ess-local-process-name))
        (lpn ess-local-process-name))
    (process-put proc 'dbg-buffer dbuff); buffer were the look up takes place
    (process-put proc 'dbg-active nil)  ; t if the process is in active debug state.
                                        ; Active debug states are usually those, in which prompt start with Browser[d]>
    (set-process-filter proc 'inferior-ess-tracebug-output-filter)
    (with-current-buffer (process-buffer proc)
      (unless (equal ess-dialect "R")
        (error "Can not activate the debugger for %s dialect" ess-dialect))
      (add-to-list 'ess--mode-line-process-indicator 'ess--dbg-mode-line-debug t)
      (add-to-list 'ess--mode-line-process-indicator 'ess--dbg-mode-line-error-action t)

      (add-hook 'ess-presend-filter-functions 'ess--dbg-remove-empty-lines nil 'local))
    (with-current-buffer dbuff
      (setq ess-local-process-name lpn)
      (buffer-disable-undo)
      ;; (setq buffer-read-only nil)
      (make-local-variable 'overlay-arrow-position) ;; indicator for next-error functionality in the *ess.dbg*,  useful??
      (goto-char (point-max))
      (setq ess--dbg-buf-p t  ;; true if in  *ess.dbg* buffer
            ess--dbg-current-ref (point-marker)  ;; used by goto-error functionality
            ess--dbg-last-ref-marker (point-marker)  ;; gives marker to reference of the last debugged line
            )
      ;;      (beginning-of-line)
      ;; (setq buffer-read-only t)
      )))

(defun ess-debug-stop ()
  "End the debug session.
Kill the *ess.dbg.[R_name]* buffer."
  ;;; process plist is not removed, todo?low priority
  (interactive)
  (let ((proc (get-process ess-current-process-name))) ;;local?
    (with-current-buffer (process-buffer proc)
      (if (member ess-dialect '("XLS" "SAS" "STA"))
          (error "Can not deactivate the debugger for %s dialect" ess-dialect))
      (delq 'ess--dbg-mode-line-debug ess--mode-line-process-indicator)
      (delq 'ess--dbg-mode-line-error-action ess--mode-line-process-indicator)
      (remove-hook 'ess-presend-filter-functions 'ess--dbg-remove-empty-lines 'local))
    (set-process-filter proc 'inferior-ess-output-filter)
    (kill-buffer (process-get proc 'dbg-buffer))
    (process-put proc 'dbg-buffer nil)
    (process-put proc 'dbg-active nil)
    ;; (when (buffer-live-p ess--dbg-buffer)
    ;;   ;; (with-current-buffer ess--dbg-buffer
    ;;   ;;   (set-buffer-modified-p nil)
    ;;   ;;   )
    ;;   (kill-buffer ess--dbg-buffer)
    ;;   )
    ))


(defun ess--make-busy-timer-function (process)
  "Display the spiner of prompt if ess-process is busy."
  `(lambda ()
     (let ((pb ,process))
       (when (eq (process-status pb) 'run) ;; only when the process is alive
         (with-current-buffer (process-buffer pb)
           (if (not (process-get pb 'busy)) ;; if ready
               (when (> ess--busy-count 0)
                 (setq ess--busy-count 0)
                 (force-mode-line-update)
                 (redisplay))
             (setq ess--busy-count (1+ (mod  ess--busy-count  (1- (length ess-busy-strings)))))
             (force-mode-line-update)
             ;; looks like redisplay is necessary for emacs > 24.4
             (redisplay)))))))

;; (ess--make-busy-prompt-function (get-process "R"))

(defun ess--dbg-is-active-p ()
  "Return t if the current R process is in active debugging state."
  (and (ess-process-live-p)
       (ess-process-get  'dbg-active)))

(defun ess--dbg-is-recover-p ()
  "Return t if the current R process is in active debugging state."
  (and (ess-process-live-p)
       (ess-process-get  'is-recover)))

(defvar ess--dbg-regexp-reference "debug \\w+ +\\(.+\\)#\\([0-9]+\\):")
(defvar ess--dbg-regexp-jump "debug \\w+ ") ;; debug at ,debug bei ,etc
(defvar ess--dbg-regexp-skip
  ;; don't anchor to bol; secondary prompt can occur before (anything else?)
  ;; "\\(\\(?:Called from: \\)\\|\\(?:debugging in: \\)\\|\\(?:#[0-9]*: +recover()\\)\\)")
  "\\(\\(?:Called from: \\)\\|\\(?:#[0-9]*: +recover()\\)\\)")

(defvar ess--dbg-regexp-no-skip
  ;; exceptions for first skip (magrittr)
  "debug_pipe")

(defvar ess--dbg-regexp-debug  "\\(\\(?:Browse[][0-9]+\\)\\|\\(?:debug: \\)\\)")
(defvar ess--dbg-regexp-selection "\\(Selection: \\'\\)")
(defvar ess--dbg-regexp-input (concat ess--dbg-regexp-debug "\\|"
                                     ess--dbg-regexp-selection))

(defvar ess--suppress-next-output? nil)



;;; MPI

(defvar ess-mpi-control-regexp "\\([^]+\\)\\([^]+\\)")

(defvar ess-mpi-alist
  '(("message" . message)
    ("error" . ess-mpi:error)
    ("eval" . ess-mpi:eval)
    ("y-or-n" . ess-mpi:y-or-n)))

(defun ess-mpi:error (msg)
  (message (format "Error in inferior: %s" msg)))

(defun ess-mpi:eval (expr &optional callback)
  "Evaluate EXP as emacs expression.
If present, the CALLBACK string is passed through `format' with
returned value from EXPR and then sent to the subprocess."
  (let ((result (eval (read expr))))
    (when callback
      (ess-send-string (ess-get-process) (format callback result)))))

(defun ess-mpi:y-or-n (prompt callback)
  "Ask `y-or-n-p' with PROMPT.
The CALLBACK string is passed through `format' with returned
value from EXPR and then sent to the subprocess."
  (let ((result (y-or-n-p prompt)))
    (when callback
      (ess-send-string (ess-get-process) (format callback result)))))

(defun ess-mpi-handle-messages (buf)
  "Handle all mpi messages in BUF and delete them."
  (let ((obuf (current-buffer)))
    (with-current-buffer buf
      (goto-char (point-min))
      ;; This should be smarter because emacs might cut it in the middle of the
      ;; message. In practice this almost never happen because we are
      ;; accumulating output into the cache buffer.
      (while (re-search-forward  ess-mpi-control-regexp nil t)
        (let* ((mbeg (match-beginning 0))
               (mend (match-end 0))
               (head (match-string 1))
               (payload (split-string (match-string 2) ""))
               (handler (cdr (assoc head ess-mpi-alist))))
          (if handler
              (condition-case-unless-debug err
                  (with-current-buffer obuf
                    (apply handler payload))
                (error (message (format "Error in mpi `%s' handler: %%s" head)
                                (error-message-string err))))
            ;; don't throw error here. The buffer must be cleaned first.
            (message "Now handler defined for MPI message '%s" head))
          (goto-char mbeg)
          (delete-region mbeg mend))))))

(defun ess--flush-process-output-cache (proc)
  (let ((pbuf (get-buffer-create (process-get proc 'accum-buffer-name))))
    (ess-mpi-handle-messages pbuf)
    (let ((string (with-current-buffer pbuf
                    (prog1 (buffer-string)
                      (erase-buffer)))))
      (when (> (length string) 0)
        (process-put proc 'last-flush-time (and (process-get proc 'busy)
                                                (float-time)))
        (comint-output-filter proc string)
        (ess--show-process-buffer-on-error string proc)))))


(defun inferior-ess-tracebug-output-filter (proc string)
  "Standard output filter for the inferior ESS process
when `ess-debug' is active. Call `inferior-ess-output-filter'.

Check for activation expressions (defined in
`ess--dbg-regexp-debug',...), when found puts iESS in the debugging state.
If in debugging state, mirrors the output into *ess.dbg* buffer."
  (let* ((is-iess (member major-mode (list 'inferior-ess-mode 'ess-watch-mode)))
         (pbuf (process-buffer proc))
         (dbuff (process-get proc 'dbg-buffer))
         (wbuff (get-buffer ess-watch-buffer))
         (was-in-dbg (process-get proc 'dbg-active))
         (was-in-recover (process-get proc 'is-recover))
         (input-point (point-marker))
         (match-jump (string-match ess--dbg-regexp-jump string))
         (match-input (string-match ess--dbg-regexp-input string))
         (match-selection (and match-input
                               (match-string 2 string))) ;; Selection:
         (match-skip (and ess-debug-skip-first-call
                          (string-match ess--dbg-regexp-skip string)
                          (not (string-match ess--dbg-regexp-no-skip string))))
         (match-dbg (or match-skip (and match-input (not match-selection))))
         ;;check for main  prompt!! the process splits the output and match-end == nil might indicate this only
         ;; (prompt-regexp "^>\\( [>+]\\)*\\( \\)$") ;; default prompt only
         (prompt-replace-regexp "\\(^> \\|^\\([>+] \\)\\{2,\\}\\)\\(?1: \\)") ;; works only with the default prompt
         (is-ready (not (inferior-ess-set-status proc string)))
         (new-time (float-time))
         (last-time (process-get proc 'last-flush-time))
         (flush-timer (process-get proc 'flush-timer)))
    ;; current-buffer is still the user's input buffer here
    (ess--if-verbose-write-process-state proc string)
    (inferior-ess-run-callback proc string)
    (process-put proc 'is-recover match-selection)

    (if (or (process-get proc 'suppress-next-output?)
            ess--suppress-next-output?)
        ;; works only for surpressing short output, for time being is enough (for callbacks)
        (process-put proc 'suppress-next-output? nil)

      ;; FIXME: this should be in comint filters!!
      ;; insert \n after the prompt when necessary
      (setq string (replace-regexp-in-string prompt-replace-regexp " \n" string nil nil 1))

      ;; replace long prompts
      (when inferior-ess-replace-long+
        (setq string (replace-regexp-in-string "\\(\\+ \\)\\{4\\}\\(\\+ \\)+" ess-long+replacement string)))

      ;; COMINT
      (with-current-buffer (get-buffer-create (process-get proc 'accum-buffer-name))
        (goto-char (point-max))
        (insert string))

      ;; Need this timer here; process might be waiting for user's input!
      (when (timerp flush-timer)
        ;; cancel the timer each time we enter the filter
        (cancel-timer flush-timer)
        (process-put proc 'flush-timer nil))

      ;; insert "\n" after prompt
      (when (or (null last-time)
                (> (- new-time last-time) .5))

        ;; Very slow in long comint buffers, but it's not a real issue, as it is
        ;; executed periodically.
        (with-current-buffer pbuf
          (save-excursion
            (let ((pmark (process-mark proc))
                  (inhibit-modification-hooks t))
              (goto-char pmark)
              (when (looking-back inferior-ess-primary-prompt)
                (insert-before-markers "\n")
                (set-marker pmark (point)))))))

      (unless last-time ;; don't flush first time
        (setq last-time new-time)
        (process-put proc 'last-flush-time new-time))

      (when (or is-ready
                (process-get proc 'sec-prompt) ; for the sake of ess-eval-linewise
                ;; flush periodically
                (> (- new-time last-time) .6))

        (ess--flush-process-output-cache proc))

      ;; setup a new flush timer (check for edebug to be able to debug mpi handler)
      (unless (and (boundp 'edebug-mode) edebug-mode)
        (process-put proc 'flush-timer
                     (run-at-time .2 nil 'ess--flush-process-output-cache proc)))

      )

    ;; WATCH
    (when (and is-ready wbuff) ;; refresh only if the process is ready and wbuff exists, (not only in the debugger!!)
      (ess-watch-refresh-buffer-visibly wbuff))

    ;; JUMP to line if debug expression was matched
    (when match-jump
      (with-current-buffer dbuff              ;; insert string in *ess.dbg* buffer
        (goto-char (point-max))
        (insert (concat "|-" string "-|")))
      (ess--dbg-goto-last-ref-and-mark dbuff is-iess))

    ;; (with-current-buffer dbuff ;; uncomment to see the value of STRING just before  debugger exists
    ;;   (let ((inhibit-read-only t))
    ;;     (goto-char (point-max))
    ;;     (insert (concat " ---\n " string "\n ---"))
    ;;     ))

    ;; SKIP if needed
    (when (and match-skip (not was-in-recover))
      (process-send-string proc  "n\n"))

    ;; EXIT the debugger
    (when (and was-in-dbg
               (not (or match-jump match-dbg))
               (or is-ready match-selection))
      (ess--dbg-deactivate-overlays)
      (process-put proc 'dbg-active nil)
      ;; (message "|<-- exited debugging -->|")
      (when wbuff
        (ess-watch-refresh-buffer-visibly wbuff)))

    ;; ACTIVATE the debugger if entered for the first time
    (when (and (not was-in-dbg)
               (not match-selection)
               (or match-jump match-dbg))
      (unless is-iess
        (ring-insert ess--dbg-forward-ring input-point))
      (process-put proc 'dbg-active t)
      (message
       (ess--debug-keys-message-string))
      (unless match-jump
        ;; no source reference, simply show the inferiro
        (ess-show-buffer pbuf)))

    (when match-selection ;(and (not was-in-recover) match-selection)
      (ess-electric-selection t))))


(defvar ess-debug-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-C") 'ess-debug-command-continue)
    (define-key map [(control meta ?C)] 'ess-debug-command-continue-multi)
    (define-key map (kbd "M-N") 'ess-debug-command-next)
    (define-key map [(control meta ?N)] 'ess-debug-command-next-multi)
    (define-key map (kbd "M-Q") 'ess-debug-command-quit)
    (define-key map (kbd "M-U") 'ess-debug-command-up)
    map)
  "Keymap active when ESS process is in debugging state.
\\{ess-debug-minor-mode-map}")


(define-minor-mode ess-debug-minor-mode
  "Minor mode activated when ESS process is in debugging state."
  :lighter nil
  :keymap ess-debug-minor-mode-map)

(defun ess--dbg-goto-last-ref-and-mark (dbuff &optional other-window)
  "Open the most recent debug reference, and set all the
necessary marks and overlays.

It's called from `inferior-ess-tracebug-output-filter'.  DBUFF must be
the *ess.dbg* buffer associated with the process. If OTHER-WINDOW
is non nil, attempt to open the location in a different window."
  (interactive)
  (let (t-debug-position ref)
    (with-current-buffer dbuff
      (setq ref (ess--dbg-get-next-ref -1 (point-max) ess--dbg-last-ref-marker
                                       ess--dbg-regexp-reference)) ; sets point at the end of found ref
      (when ref
        (move-marker ess--dbg-last-ref-marker (point-at-eol))
        ;; each new step repositions the current-ref!
        (move-marker ess--dbg-current-ref ess--dbg-last-ref-marker)))
    (when ref
      (let ((buf (apply 'ess--dbg-goto-ref other-window ref)))
        (if buf
            ;; if referenced buffer has been found, put overlays:
            (with-current-buffer buf
              (setq t-debug-position (copy-marker (point-at-bol)))
              (if (equal t-debug-position ess--dbg-current-debug-position)
                  (progn ;; highlights the overlay for ess--dbg-blink-interval seconds
                    (overlay-put ess--dbg-current-debug-overlay 'face 'ess--dbg-blink-same-ref-face)
                    (run-with-timer ess-debug-blink-interval nil
                                    (lambda ()
                                      (overlay-put ess--dbg-current-debug-overlay 'face 'ess-debug-current-debug-line-face))))
                ;; else
                (ess--dbg-activate-overlays)))
          ;;else, buffer is not found: highlight and give the corresponding message
          (overlay-put ess--dbg-current-debug-overlay 'face 'ess--dbg-blink-ref-not-found-face)
          (run-with-timer ess-debug-blink-interval nil
                          (lambda ()
                            (overlay-put ess--dbg-current-debug-overlay 'face 'ess-debug-current-debug-line-face)))
          (message "Reference %s not found" (car ref)))))))

(defun ess--dbg-goto-ref (other-window file line &optional col)
  "Opens the reference given by FILE, LINE and COL,
Try to open in a different window if OTHER-WINDOW is nil.  Return
the buffer if found, or nil otherwise be found.
`ess--dbg-find-buffer' is used to find the FILE and open the
associated buffer. If FILE is nil return nil."
  (let ((mrk (car (ess--dbg-create-ref-marker file line col)))
        (lpn ess-local-process-name))
    (when mrk
      (let ((buf (marker-buffer mrk)))
	(if (not other-window)
	    (switch-to-buffer buf)
	  (let ((this-frame (window-frame (get-buffer-window (current-buffer)))))
	    (display-buffer buf)
	    ;; simple save-frame-excursion
	    (unless (eq this-frame (window-frame (get-buffer-window buf t)))
	      (ess-select-frame-set-input-focus this-frame))))
	;; set or re-set to lpn as this is the process with debug session on
	(with-current-buffer buf
	  (setq ess-local-process-name lpn)
	  (goto-char mrk)
      (set-window-point (get-buffer-window buf) mrk))
	buf))))

;; temporary, hopefully org folks implement something similar
(defvar org-babel-tangled-file nil)

(defun ess--dbg-create-ref-marker (file line &optional col)
  "Create markers to the reference given by FILE, LINE and COL.
Return list of two markers MK-start and MK-end. MK-start is the
position of error. Mk-end is the end of the line where error
occurred.

If buffer associated with FILE is not found, or line is nil, or
TB-INDEX is not found return nil."
  (if (stringp line) (setq line (string-to-number line)))
  (if (stringp col) (setq col (string-to-number col)))
  (let* ((srcref (gethash file ess--srcrefs))
         (file (replace-regexp-in-string "^\n" "" ;; hack for gnu regexp
                                         (or (car srcref) file)))
         (tb-index (cadr srcref))
         (buffer (ess--dbg-find-buffer file))
         pos)
    (when (and buffer line)
      (save-excursion
        (with-current-buffer buffer
          (save-restriction
            (widen) ;; how does this behave in narrowed buffers? tothink:
            (goto-char 1)
            (setq pos (point))
            (when tb-index
              (while (and (not (eq tb-index (get-text-property pos 'tb-index)))
                          (setq pos (next-single-property-change pos 'tb-index)))))
            (unless pos
              ;; use beg position if index not found
              (setq pos (nth 2 srcref)))
            (when pos
              (goto-char pos)
              (forward-line (1- line))
              (if col
                  (goto-char (+ (point-at-bol) col))
                (back-to-indentation))
              (when org-babel-tangled-file
                (org-babel-tangle-jump-to-org))
              (list (point-marker) (copy-marker (point-at-eol))))))))))


(defun ess--dbg-find-buffer (filename)
  "Find a buffer for file FILENAME.
If FILENAME is not found at all, ask the user where to find it if
`ess--dbg-ask-for-file' is non-nil.  Search the directories in
`ess-tracebug-search-path'."
  (let ((dirs ess-tracebug-search-path)
        (spec-dir default-directory)
        buffsym buffer thisdir fmts name buffername)
    (setq dirs (cons spec-dir dirs)) ;; current does not have priority!! todo:should be R working dir
    ;; 1. search already open buffers for match (associated file might not even exist yet)
    (dolist (bf (buffer-list))
      (with-current-buffer  bf
        (when (and buffer-file-name
                   (or (and (file-name-absolute-p filename)
                            (string-match (format "%s\\'" filename) buffer-file-name))
                       (equal filename (file-name-nondirectory buffer-file-name))))
          (setq buffer bf)
          (return))))
    ;; 2. The file name is absolute.  Use its explicit directory as
    ;; the first in the search path, and strip it from FILENAME.
    (when (and (null  buffer)
               (file-name-absolute-p filename))
      (setq filename (abbreviate-file-name (expand-file-name filename))
            dirs (cons (file-name-directory filename) dirs)
            filename (file-name-nondirectory filename)))
    ;; 3. Now search the path.
    (while (and (null buffer)
                dirs )
      (setq thisdir (car dirs))
      (setq name (expand-file-name filename thisdir)
            buffer (and (file-exists-p name)
                        (find-file-noselect name)))
      (setq dirs (cdr dirs)))
    ;; 4. Ask for file if not found (tothink: maybe remove this part?)
    (if (and (null buffer)
             ess-debug-ask-for-file)
        (save-excursion            ;This save-excursion is probably not right.
          (let* ((pop-up-windows t)
                 (name (read-file-name
                        (format "Find next line in (default %s): "  filename)
                        spec-dir filename t nil))
                 (origname name))
            (cond
             ((not (file-exists-p name))
              (message "Cannot find file `%s'" name)
              (ding) (sit-for 2))
             ((and (file-directory-p name)
                   (not (file-exists-p
                         (setq name (expand-file-name filename name)))))
              (message "No `%s' in directory %s" filename origname)
              (ding) (sit-for 2))
             (t
              (setq buffer (find-file-noselect name)))))))
    ;; nil if not found
    buffer))

(defun ess--dbg-get-next-ref (n &optional pt BOUND REG nF nL nC)
  "Move point to the next reference in the *ess.dbg* buffer.

Must be called from *ess.dbg* buffer.
It returns the reference in the form (file line col) /all strings/ ,
or NIL if not found .  Prefix arg N says how many error messages
to move forwards (or backwards, if negative).  Optional arg PT,
if non-nil, specifies the value of point to start looking for the
next message, default to (point).  BOUND is the limiting position
of the search.  REG is the regular expression to search with.  nF
- sub-expression of REG giving the 'file'; defaults to 1.  nL -
giving the 'line'; defaults to 2.  nC - sub-expr giving the
'column'; defaults to 3.
"
  (interactive "p")
  (unless ess--dbg-buf-p
    (error "Not in *ess.dbg* buffer."))
  (setq nF (or nF 1)
        nL (or nL 2)
        nC (or nC 3))
  (or pt (setq pt (point)))
  ;; (message "ess--dbg-last-ref-marker%s vs  pt%s vs point-max%s" ess--dbg-last-ref-marker pt (point-max))
  (goto-char pt)
  (if (search-forward-regexp REG BOUND t n)
      (list (match-string nF) (match-string-no-properties nL) (match-string-no-properties nC))
    nil))

(defun ess--dbg-next-ref-function (n &optional reset)
  "Advance to the next reference and visit the location
given by the reference.  This is the value of
`next-error-function' in *ess.dbg* buffers."
  (interactive "p")
  (if reset
      (set-marker ess--dbg-current-ref ess--dbg-last-ref-marker))
  (let ((loc (ess--dbg-get-next-ref n nil ess--dbg-current-ref))  ;; moves point to next/prev ref if any
                                        ; loc is  (file . line_nr)
        dbuff)
    (if loc
        (progn
          (set-marker ess--dbg-current-ref (line-end-position))
          (set-marker overlay-arrow-position (line-beginning-position))
          (setq dbuff (ess--dbg-find-buffer  (car loc)))
          (switch-to-buffer dbuff)
          (save-restriction
            (widen)
            (goto-char 1)
            (forward-line (1- (cdr loc))))
          (move-marker ess--dbg-current-debug-position (line-beginning-position)) ; move the overlay-arrow
          (back-to-indentation))
      (if (>= 0 (or n 1))
          (error "Moved past first debug line")
        (error "Moved past last debug line")))))


;; not used; remove in 13.09
(defvar ess-electric-debug-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'ess-debug-command-continue)
    (define-key map "C" 'ess-debug-command-continue-multi)
    (define-key map "n" 'ess-debug-command-next)
    (define-key map "N" 'ess-debug-command-next-multi)
    (define-key map "q" 'ess-debug-command-quit)
    (define-key map "u" 'ess-debug-command-up)
    map)
  "Keymap used to define commands for single key input mode.
This commands are triggered by `ess-electric-debug' .
\\{ess-electric-debug-map}")
(make-obsolete-variable 'ess-electric-debug-map nil "ESS 13.05")

(defun ess--debug-keys-message-string (&optional map)
  (let ((overriding-local-map (or map ess-debug-minor-mode-map)))
    (substitute-command-keys
     (mapconcat 'identity
                '("(\\[ess-debug-command-continue])cont"
                  "(\\[ess-debug-command-continue-multi])cont-multi"
                  "(\\[ess-debug-command-next])next"
                  "(\\[ess-debug-command-next-multi])next-multi"
                  "(\\[ess-debug-command-up])up"
                  "(\\[ess-debug-command-quit])quit")
                " "))))

;; not used anywhere; remove in ESS 13.09
(defun ess-electric-debug (&optional wait)
  "Call commands defined in `ess-electric-debug-map'.
Single-key input commands are those that once invoked do not
requre the prefix command for subsequent invocation.

For example, if the prefix key is 'C-c C-t' and
`ess-debug-command-next' is bound to 'n' and `ess-debug-command-continue' is
bound to 'c' then 'C-c C-t n n c' executes `ess-debug-command-next'
twice and `ess-debug-command-continue' once. Any other input not defined
in `ess-electric-debug-map' will cause the exit from single-key
input mode.

If WAIT is t, wait for next input and ignore the keystroke which
triggered the command."
  (interactive)
  (let ((help-mess (ess--debug-keys-message-string
                    ess-electric-debug-map)))
    (ess--execute-electric-command
     ess-electric-debug-map help-mess wait
     (not (ess-process-get 'dbg-active)))))

(define-obsolete-function-alias
  'ess-singlekey-debug 'ess-electric-debug  "ESS 13.05")
(make-obsolete 'ess-electric-debug nil "ESS 13.05")

(defun ess-electric-selection (&optional wait)
  "Call commands defined in `ess-electric-selection-map'.
Single-key input commands are those, which once executed do not
requre the prefix command for subsequent invocation. See
`ess-electric-debug' for more.

If WAIT is t, wait for next input and ignore the keystroke which
triggered the command."

  (interactive)
  (ess--execute-electric-command ess-electric-selection-map
                                 "Selection: " wait
                                 (not (ess-process-get 'is-recover))))

(make-obsolete 'ess-singlekey-selection 'ess-electric-selection "ESS 13.05")

(defun ess-debug-command-digit (&optional ev)
  "Digit commands in selection mode.
If suplied ev must be a proper key event or a string representing the digit."
  (interactive)
  (unless ev
    (setq ev last-command-event))
  (let* ((ev-char (if (stringp ev)
                      ev
                    (char-to-string (event-basic-type ev))))
         (proc (get-process ess-current-process-name))
         (mark-pos (marker-position (process-mark proc)))
         (comint-prompt-read-only nil)
         prompt  depth)
    (if (process-get proc 'is-recover)
        (with-current-buffer (process-buffer proc)
          (goto-char mark-pos)
          (save-excursion
            (when (re-search-backward "\\(?: \\|^\\)\\([0-9]+\\):[^\t]+Selection:" ess--tb-last-input t)
              (setq depth (string-to-number (match-string 1)))
              (when (> depth 9)
                (setq ev-char (ess-completing-read "Selection" (mapcar 'number-to-string
                                                                       (number-sequence depth 0 -1))
                                                   nil t ev-char nil)))))
          (setq prompt (delete-and-extract-region  (point-at-bol) mark-pos))
          (insert (concat  prompt ev-char "\n"))
          (ess-send-string proc ev-char)
          (move-marker (process-mark proc) (max-char)))
      (error "Recover is not active"))))

(defun ess-debug-command-next (&optional ev)
  "Step next in debug mode.
Equivalent to 'n' at the R prompt."
  (interactive)
  (if (not (ess--dbg-is-active-p))
      (error "Debugger is not active")
    (if (ess--dbg-is-recover-p)
        (ess-send-string (get-process ess-current-process-name) "0")
      (ess-send-string (get-process ess-current-process-name) "n"))))

(defun ess-debug-command-next-multi (&optional ev N)
  "Ask for N and step (n) N times in debug mode."
  (interactive)
  (if (not (ess--dbg-is-active-p))
      (error "Debugger is not active")
    (let ((N (or N (read-number "Number of steps: " 10)))
          (proc (get-process ess-local-process-name))
          (ess--suppress-next-output? t))
      (while (and (ess--dbg-is-active-p) (> N 0))
        (ess-debug-command-next)
        (ess-wait-for-process proc)
        (setq N (1- N))))
    (ess-debug-command-next)))

(defun ess-debug-command-continue-multi (&optional ev N)
  "Ask for N, and continue (c) N times in debug mode."
  (interactive)
  (if (not (ess--dbg-is-active-p))
      (error "Debugger is not active")
    (let ((N (or N (read-number "Number of continuations: " 10)))
          (proc (get-process ess-local-process-name))
          (ess--suppress-next-output? t))
      (while (and (ess--dbg-is-active-p) (> N 1))
        (ess-debug-command-continue)
        (ess-wait-for-process proc)
        (setq N (1- N))))
    (ess-debug-command-continue)))


(defun ess-debug-command-up (&optional ev)
  "Step up one call frame.
Equivalent to 'n' at the R prompt."
  (interactive)
  (if (not (ess--dbg-is-active-p))
      (error "Debugger is not active")
    (let ((proc (get-process ess-local-process-name)))
      (ess-send-string proc
                       "try(browserSetDebug(), silent=T)\nc\n"))))

;; (defun ess-debug-previous-error (&optional ev)
;;   "Go to previous reference during the debug process.
;; R doesn't support step backwards. This command just takes you  through
;; debug history."
;;   (interactive)
;;   (previous-error))

(defun ess-debug-command-quit (&optional ev)
  "Quits the browser/debug in R process.
 Equivalent to 'Q' at the R prompt."
  (interactive)
  (let ((proc (get-process ess-current-process-name) ))
    (if (not (or (process-get proc 'dbg-active)
                 (process-get proc 'is-recover)))
        (error "Debugger is not active")
      (when (ess--dbg-is-recover-p)
        (ess-send-string proc "0")
        ;; if recover is called in a loop the following stalls emacs
        ;; (ess-wait-for-process proc nil 0.05)
        )
      (if (and (process-get proc 'dbg-active)
               (not (process-get proc 'is-recover))); still in debug mode
          (ess-send-string proc "Q")))))

(defun ess-debug-command-continue (&optional ev)
  "Continue the code execution.
 Equivalent of 'c' at the R prompt."
  (interactive)
  (let ((proc (get-process ess-current-process-name)))
    (if (not (or (process-get proc 'dbg-active)
                 (process-get proc 'is-recover)))
        (error "Debugger is not active")
      (when (ess--dbg-is-recover-p)
        (ess-send-string proc "0")
        ;; (ess-wait-for-process proc nil 0.05) <- when in a loop, gets stuck
        ) ;; get out of recover mode
      (if (and (process-get proc 'dbg-active) ; still in debug mode
               (not (process-get proc 'is-recover))); still in debug mode
          (ess-send-string proc "c")))))

(defun ess-tracebug-set-last-input (&rest ARGS)
  "Move `ess--tb-last-input' marker to the process mark.
ARGS are ignored to allow using this function in process hooks."
  (let* ((last-input-process (get-process ess-local-process-name))
         (last-input-mark (copy-marker (process-mark last-input-process))))
    (with-current-buffer (process-buffer last-input-process)
      (when (local-variable-p 'ess--tb-last-input) ;; TB might not be active in all processes
        (save-excursion
          (setq ess--tb-last-input last-input-mark)
          (goto-char last-input-mark)
          (inferior-ess-move-last-input-overlay))))))

;;;_ + BREAKPOINTS

(defface ess-bp-fringe-inactive-face
  '((((class color) (background light) (min-colors 88)) (:foreground "DimGray"))
    (((class color) (background dark) (min-colors 88))  (:foreground "LightGray"))
    (((background light) (min-colors 8))  (:foreground "blue"))
    (((background dark) (min-colors 8))  (:foreground "cyan")))
  "Face used to highlight inactive breakpoints."
  :group 'ess-debug)

(defface ess-bp-fringe-logger-face
  '((((class color) (background light) (min-colors 88)) (:foreground "dark red"))
    (((class color) (background dark) (min-colors 88))  (:foreground "tomato1"))
    (((background light) (min-colors 8))  (:foreground "blue"))
    (((background dark) (min-colors 8))  (:foreground "cyan")))
  "Face used to highlight loggers."
  :group 'ess-debug)

(defface ess-bp-fringe-browser-face
  '((((class color) (background light) (min-colors 88)) (:foreground "medium blue"))
    (((class color) (background dark) (min-colors 88))  (:foreground "deep sky blue"))
    (((background light) (min-colors 8))  (:foreground "blue"))
    (((background dark) (min-colors 8))  (:foreground "cyan")))
  "Face used to highlight 'browser' breakpoints."
  :group 'ess-debug)

(defface ess-bp-fringe-recover-face
  '((((class color) (background light) (min-colors 88)) (:foreground "dark magenta"))
    (((class color) (background dark) (min-colors 88))  (:foreground "magenta"))
    (((background light) (min-colors 8))  (:foreground "magenta"))
    (((background dark) (min-colors 8))  (:foreground "magenta")))
  "Face used to highlight 'recover' breakpoints fringe."
  :group 'ess-debug)

(defun ess--bp-pipe-block-p ()
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
          (inhibit-field-text-motion t))
      (forward-line -1)
      (end-of-line)
      (looking-back "\\%>\\%[ \t]*"))))

(defvar ess--bp-identifier 1)
(defcustom ess-bp-type-spec-alist
  '((pipe    ".ess_pipe_browser() %%>%%" "B %>%\n" filled-square ess-bp-fringe-browser-face ess--bp-pipe-block-p)
    (browser "browser(expr=is.null(.ESSBP.[[%s]]));" "B>\n" filled-square  ess-bp-fringe-browser-face)
    (recover "recover()" "R>\n"   filled-square  ess-bp-fringe-recover-face))
  "List of lists of breakpoint types.
Each sublist  has five elements:
1- symbol giving the name of specification
2- R expression to be inserted (%s is substituted with unique identifier).
3- string to be displayed instead of the expression
4- fringe bitmap to use
5- face for fringe and displayed string
6- optional, a function which should return nil if this BP doesn't apply to current context."
  :group 'ess-debug
  :type '(alist :key-type symbol
                :value-type (group string string symbol face)))

(defcustom ess-bp-inactive-spec
  '(inactive     "##"    filled-square  ess-bp-fringe-inactive-face)
  "List giving the inactive breakpoint specifications."
  ;; List format is identical to that of the elements of
  ;; `ess-bp-type-spec-alist' except that the second element giving
  ;; the R expression is meaningless here." ;;fixme: second element is missing make it nil for consistency with all other specs
  :group 'ess-debug)

(defcustom ess-bp-conditional-spec
  '(conditional     "browser(expr={%s})"  "CB[ %s ]>\n"  question-mark  ess-bp-fringe-browser-face)
  "List giving the conditional breakpoint specifications.
List format is identical to that of the elements of
`ess-bp-type-spec-alist'.  User is asked for the conditional
expression to be replaced instead of %s in the second and third
elements of the specifications."
  :group 'ess-debug)

(defcustom ess-bp-logger-spec
  '(logger     ".ess_log_eval('%s')"  "L[ \"%s\" ]>\n"  hollow-square  ess-bp-fringe-logger-face)
  "List giving the loggers specifications.
List format is identical to that of `ess-bp-type-spec-alist'."
  :group 'ess-debug)


(defun ess-bp-get-bp-specs (type &optional condition no-error)
  "get specs for TYPE "
  (let ((spec-alist (cond
                     ((eq type 'conditional)
                      (let ((tl (copy-sequence  ess-bp-conditional-spec)))
                        (when (eq (length condition) 0)
                          (setq condition "TRUE"))
                        (setcar (cdr tl) (format (cadr tl) condition))
                        (setcar (cddr tl) (format (caddr tl) condition))
                        (list tl)))
                     ((eq type 'logger)
                      (let ((tl (copy-sequence ess-bp-logger-spec)))
                        (when (eq (length condition) 0)
                          (setq condition "watchLog"))
                        (setcar (cdr tl) (format (cadr tl) condition))
                        (setcar (cddr tl) (format (caddr tl) condition))
                        (list tl)))
                     (t (copy-sequence ess-bp-type-spec-alist)))))
    (or (assoc type spec-alist)
        (if no-error
            nil
          (error "Undefined breakpoint type %s" type)))))

(defun ess-bp-create (type &optional condition no-error)
  "Set breakpoint for the current line.
 Returns the begging position of the hidden text."
  (let* ((bp-specs (ess-bp-get-bp-specs type condition no-error))
         (init-pos (point-marker))
         (fringe-bitmap (nth 3 bp-specs))
         (fringe-face (nth 4 bp-specs))
         (displ-string (nth 2 bp-specs))
         (bp-id (format "\"@%s@\""
                        (setq ess--bp-identifier (1+ ess--bp-identifier))))
         (bp-command (concat  (format (nth 1 bp-specs) bp-id)
                              "##:ess-bp-end:##\n"))
         (bp-length (length bp-command))
         (dummy-string (format "##:ess-bp-start::%s@%s:##\n"  (car bp-specs) condition))
         (dummy-length (length dummy-string))
         insertion-pos)
    (when bp-specs
      (set-marker init-pos (1+ init-pos))
      (setq displ-string (propertize displ-string
                                     'face fringe-face
                                     'font-lock-face fringe-face))
      (setq bp-command (propertize bp-command
                                   'ess-bp t
                                   'bp-id bp-id
                                   'bp-active t
                                   'intangible 'ess-bp
                                   'rear-nonsticky '(intangible ess-bp bp-type)
                                   'bp-type type
                                   'bp-substring 'command
                                   'display displ-string))
      (setq dummy-string (propertize
                          (ess-tracebug--propertize dummy-string fringe-bitmap fringe-face "*")
                          'ess-bp t
                          'intangible 'ess-bp
                          'bp-type type
                          'bp-substring 'dummy))
      (ess-tracebug--set-left-margin)
      (back-to-indentation)
      (setq insertion-pos (point) )
      (insert (concat   dummy-string bp-command))
      (indent-for-tab-command)
      (goto-char (1- init-pos))  ;; sort of save-excursion
      insertion-pos)))

(defun ess-bp-recreate-all ()
  "internal function to recreate all bp"
  (save-excursion
    (save-restriction
      (with-silent-modifications
        (widen)
        (goto-char (point-min))
        (while (re-search-forward
                "\\(##:ess-bp-start::\\(.*\\):##\n\\)\\(.+##:ess-bp-end:##\n\\)" nil t)
          (let ((dum-beg (match-beginning 1))
                (dum-end (match-end 1))
                (comm-beg (match-beginning 3))
                (comm-end (match-end 3))
                (type (match-string 2))
                (bp-command (match-string 3))
                bp-id dum-props condition)
            (when (string-match "^\\(\\w+\\)@\\(.*\\)\\'" type)
              (setq condition (match-string 2 type))
              (setq type (match-string 1 type)))
            (setq bp-id
                  (if (string-match "\"@[0-9]+@\"" bp-command)
                      (match-string 0 bp-command)
                    (setq ess--bp-identifier (1+ ess--bp-identifier))))
            (setq type (intern type))
            (let* ((bp-specs (ess-bp-get-bp-specs  type condition t))
                   (displ-string (nth 2 bp-specs))
                   (fringe-face (nth 4 bp-specs))
                   (fringe-bitmap (nth 3 bp-specs)))
              (when bp-specs
                (setq displ-string (propertize displ-string
                                               'face fringe-face
                                               'font-lock-face fringe-face))
                (add-text-properties comm-beg comm-end
                                     (list 'ess-bp t
                                           'bp-id bp-id
                                           'intangible 'ess-bp
                                           'rear-nonsticky '(intangible ess-bp bp-type)
                                           'bp-type type
                                           'bp-substring 'command
                                           'display displ-string))
                (setq dum-props
                      (if window-system
                          (list 'display (list 'left-fringe fringe-bitmap fringe-face))
                        (list 'display (list '(margin left-margin)
                                             (propertize "dummy"
                                                         'font-lock-face fringe-face
                                                         'face fringe-face)))))
                (add-text-properties dum-beg dum-end
                                     (append dum-props
                                             (list 'ess-bp t
                                                   'intangible 'ess-bp
                                                   'bp-type type
                                                   'bp-substring 'dummy)))
                ;; (when comment-beg
                ;;   (add-text-properties comment-beg comment-end
                ;;                        (list 'ess-bp t
                ;;                              'bp-id bp-id
                ;;                              'intangible 'ess-bp
                ;;                              'display (propertize (nth 1 ess-bp-inactive-spec) 'face fringe-face)
                ;;                              'bp-type type
                ;;                              'bp-substring 'comment)))
                ))))))))

(add-hook 'R-mode-hook 'ess-bp-recreate-all)


(defun ess-bp-get-bp-position-nearby ()
  "Return the cons (beg . end) of breakpoint limit points
closest to the current position.  Only currently visible region of the
buffer is searched.  This command is intended for use in
interactive commands like `ess-bp-toggle-state' and `ess-bp-kill'.
Use `ess-bp-previous-position' in programs."
  (interactive)
  (let*  ((pos-end (if (get-char-property (1- (point)) 'ess-bp)
                       (point)
                     (previous-single-property-change (point) 'ess-bp nil (window-start))))
          (pos-start (if (get-char-property (point) 'ess-bp) ;;check for bobp
                         (point)
                       (next-single-property-change (point) 'ess-bp nil (window-end))))
          pos dist-up dist-down)
    (unless (eq pos-end (window-start))
      (setq dist-up (- (line-number-at-pos (point))
                       (line-number-at-pos pos-end))))
    (unless (eq pos-start (window-end))
      (setq dist-down (- (line-number-at-pos pos-start)
                         (line-number-at-pos (point)))))
    (if (and dist-up dist-down)
        (if (< dist-up dist-down)
            (cons (previous-single-property-change pos-end 'ess-bp nil (window-start)) pos-end)
          (cons pos-start (next-single-property-change pos-start 'ess-bp nil (window-end))))
      (if dist-up
          (cons (previous-single-property-change pos-end 'ess-bp nil (window-start)) pos-end)
        (if dist-down
            (cons pos-start (next-single-property-change pos-start 'ess-bp nil (window-end))))))))


(defun ess-bp-previous-position ()
  "Returns the cons (beg . end) of breakpoint limit points closest
to the current position, nil if not found. "
  (let* ( (pos-end (if (get-char-property (1- (point)) 'ess-bp)
                       (point)
                     (previous-single-property-change (point) 'ess-bp ))))
    (if pos-end
        (cons (previous-single-property-change pos-end 'ess-bp) pos-end))))

(defun ess-bp-set ()
  (interactive)
  (let* ((pos (ess-bp-get-bp-position-nearby))
         (same-line (and pos
                         (<=  (point-at-bol) (cdr pos))
                         (>= (point-at-eol) (car pos))))
         (types ess-bp-type-spec-alist)
         (ev last-command-event)
         (com-char  (event-basic-type ev))
         bp-type)
    (when same-line
      ;; set bp-type to next type in types
      (setq bp-type (get-text-property (car pos) 'bp-type))
      (setq types (cdr (member (assq bp-type types) types))) ; nil if bp-type is last in the list
      (when (null types)
        (setq types ess-bp-type-spec-alist))
      (ess-bp-kill)
      (indent-for-tab-command))
    ;; skip contextual bps
    (while (and (nth 5 (car types))
                (not (funcall (nth 5 (car types)))))
      (pop types))
    (setq bp-type (pop types))
    (ess-bp-create (car bp-type))
    (while  (eq (event-basic-type (setq ev (read-event (format "'%c' to cycle" com-char))))
                com-char)
      (if (null types) (setq types ess-bp-type-spec-alist))
      (ess-bp-kill)
      ;; skip contextual bps
      (while (and (nth 5 (car types))
                  (not (funcall (nth 5 (car types)))))
        (pop types))
      (setq bp-type (pop types))
      (ess-bp-create (car bp-type))
      (indent-for-tab-command))
    (push ev unread-command-events)))


(defun ess-bp-set-conditional (condition)
  (interactive "sBreakpoint condition: ")
  (ess-bp-create 'conditional condition)
  (indent-for-tab-command))

(defun ess-bp-set-logger (name)
  (interactive "sLogger name : ")
  (ess-bp-create 'logger name)
  (indent-for-tab-command))

(defun ess-bp-kill (&optional interactive?)
  "Remove the breakpoint nearby"
  (interactive "p")
  (let ((pos (ess-bp-get-bp-position-nearby))
        (init-pos (make-marker)))
    (if (null pos)
        (if interactive? (message "No breakpoints nearby"))
      (if (eq (point) (point-at-eol))
          (goto-char (1- (point)))) ;; work-arround for issue  3
      (set-marker init-pos  (point))
      (goto-char (car pos))
      (delete-region (car pos) (cdr pos))
      (indent-for-tab-command)
      (goto-char init-pos)
      (if (eq (point) (point-at-eol)) (forward-char)))))

(defun ess-bp-kill-all nil
  "Delete all breakpoints in current buffer."
  (interactive)
  (let ((count 0)
        (init-pos (make-marker))
        pos)
    (set-marker init-pos (1+ (point)))
    (save-excursion   ;; needed if error
      (goto-char (point-max))
      (while (setq pos (ess-bp-previous-position))
        (goto-char (car pos))
        (delete-region (car pos) (cdr pos))
        (indent-for-tab-command)
        (setq count (1+ count)))
      (if (eq count 1)
          (message "Killed 1 breakpoint")
        (message "Killed %d breakpoint(s)" count)))
    (goto-char (1- init-pos))))


(defun ess-bp-toggle-state ()
  "Toggle the breakpoint between active and inactive states.

For standard breakpoints, the effect of this command is
immediate, that is you don't need to source your code and it
works even in the process of debugging.

For loggers, recover and conditional breakpoints this command
just comments the breakpoint in the source file.

If there is no active R session, this command triggers an error."
  (interactive)
  (unless (and ess-local-process-name
               (get-process ess-local-process-name))
    (error "No R session in this buffer"))
  (save-excursion
    (let ((pos (ess-bp-get-bp-position-nearby))
          (fringe-face (nth 3 ess-bp-inactive-spec))
          (inhibit-point-motion-hooks t) ;; deactivates intangible property
          bp-id beg-pos-dummy end-pos-comment bp-specs beg-pos-command)
      (if (null pos)
          (message "No breakpoints in the visible region")
        (goto-char (car pos))
        (setq beg-pos-command (previous-single-property-change
                               (cdr pos) 'bp-substring nil (car pos))
              bp-id (get-char-property beg-pos-command 'bp-id))
        (goto-char beg-pos-command)
        (if (get-char-property beg-pos-command 'bp-active)
            (progn
              (put-text-property  (car pos) beg-pos-command ;; dummy display change
                                  'display (list 'left-fringe (nth 2 ess-bp-inactive-spec) fringe-face))
              (put-text-property beg-pos-command (cdr pos)
                                 'bp-active nil)
              (ess-command (format ".ESSBP.[[%s]] <- TRUE\n" bp-id)))
          (setq bp-specs (assoc (get-text-property (point) 'bp-type) ess-bp-type-spec-alist))
          (put-text-property beg-pos-command (cdr pos)
                             'bp-active t)
          (put-text-property  (car pos) beg-pos-command
                              'display (list 'left-fringe (nth 3 bp-specs) (nth 4 bp-specs)))
          (ess-command (format ".ESSBP.[[%s]] <- NULL\n" bp-id))
          ;; (insert (propertize "##"
          ;;                     'ess-bp t
          ;;                     'intangible 'ess-bp
          ;;                     'display (propertize (nth 1 ess-bp-inactive-spec) 'face fringe-face)
          ;;                     'bp-type (get-char-property (point) 'bp-type)
          ;;                     'bp-substring 'comment))
          )))))


(defun ess-bp-make-visible ()
  "Makes bp text visible."
  (interactive)
  (let ((pos (ess-bp-get-bp-position-nearby)))
    (set-text-properties (car pos) (cdr pos) (list 'display nil))))



(defun ess-bp-next nil
  "Goto next breakpoint."
  (interactive)
  (let ((cur-pos (point))
        (bp-pos (next-single-property-change (point) 'ess-bp)))
    (when bp-pos
      (save-excursion
        (goto-char bp-pos)
        (when (get-text-property (1- (point)) 'ess-bp)
          (setq bp-pos (next-single-property-change bp-pos 'ess-bp)))))
    (if bp-pos
        (goto-char bp-pos)
      (message "No breakpoints found"))))


(defun ess-bp-previous nil
  "Goto previous breakpoint."
  (interactive)
  (let ((cur-pos (point))
        (bp-pos (previous-single-property-change (point) 'ess-bp)))
    (if bp-pos
        (goto-char (or (previous-single-property-change bp-pos 'ess-bp)
                       bp-pos))
      ;;
      (message "No breakpoints before the point found"))))

;;;_ + WATCH

(defvar ess-watch-command
  ;; assumes that every expression is a structure of length 1 as returned by parse.
  ".ess_watch_eval()\n")

(if (fboundp 'define-fringe-bitmap) ;;not clear to me why is this not bound in SSH session? - :todo check
    (define-fringe-bitmap 'current-watch-bar
      [#b00001100] nil nil '(top t)))

(defun ess-tracebug--set-left-margin ()
  "Set the margin on non-X displays"
  (unless window-system
    (when (= left-margin-width 0)
      (setq left-margin-width 1)
      (set-window-buffer (selected-window) (current-buffer)))))

(defun ess-watch-mode ()
  "Major mode in ess-watch window.
\\{ess-watch-mode-map}"
  (let ((cur-block (max 1 (ess-watch-block-at-point)))
        (dummy-string
         (ess-tracebug--propertize "|" 'current-watch-bar 'font-lock-keyword-face)))
    (kill-all-local-variables)
    (ess-tracebug--set-left-margin)
    (make-local-variable 'revert-buffer-function)
    (setq revert-buffer-function 'ess-watch-revert-buffer)
    (use-local-map ess-watch-mode-map)
    (setq major-mode 'ess-watch-mode)
    (setq mode-name (concat "watch " ess-current-process-name))
    (turn-on-font-lock)
    (setq ess-watch-current-block-overlay
          (make-overlay (point-min) (point-max)))
    (overlay-put ess-watch-current-block-overlay 'line-prefix dummy-string)
    (overlay-put ess-watch-current-block-overlay 'face 'ess-watch-current-block-face)
    (ess-watch-set-current cur-block) ;;
    (when (require 'face-remap nil t)
      ;; scale the font
      (setq text-scale-mode-amount ess-watch-scale-amount)
      (text-scale-mode 1)                                        ;    (text-scale-mode -1) ;;restore to default
      )))

(defun ess-watch ()
  "Run ess-watch mode on R objects.
This is the trigger function.  See documentation of
`ess-watch-mode' for more information.

\\{ess-watch-mode-map}
"
  (interactive)
  (ess-force-buffer-current)
  (let ((wbuf (get-buffer-create ess-watch-buffer))
        (pname ess-local-process-name)
        (alist (symbol-value ess-local-customize-alist)))
    (set-buffer wbuf)
    (ess-setq-vars-local alist)
    (setq ess-local-process-name pname)
    (ess-watch-mode)
    (ess-watch-refresh-buffer-visibly wbuf) ;; evals the ess-command and displays the buffer if not visible
    (pop-to-buffer wbuf)
    (set-window-dedicated-p (selected-window) 1) ;; not strongly dedicated
    ))


(defun ess-watch-refresh-buffer-visibly (wbuf &optional sleep no-prompt-check)
  "Eval `ess-watch-command' and direct the output into the WBUF.
Call `ess-watch-buffer-show' to make the buffer visible, without
selecting it.

This function is used for refreshing the watch window after each step during
the debugging."
  ;; assumes that the ess-watch-mode is on!!
  ;; particularly ess-watch-current-block-overlay is installed
  (interactive)
  (ess-watch-buffer-show wbuf) ;; if visible do nothing
  (let ((pname ess-local-process-name)) ;; watch might be used from different dialects, need to reset
    (with-current-buffer wbuf
      (let ((curr-block (max 1 (ess-watch-block-at-point)))) ;;can be 0 if
        (setq buffer-read-only nil)
        (when pname
          (setq ess-local-process-name pname))
        (ess-command  ess-watch-command wbuf sleep no-prompt-check)
        ;; delete the ++++++> line  ;; not very reliable but works fine so far.
        (goto-char (point-min))
        (delete-region (point-at-bol) (+ 1 (point-at-eol)))
        (ess-watch-set-current curr-block)
        (set-window-point (get-buffer-window wbuf) (point))
        (setq buffer-read-only t)))))

(defun ess-watch-buffer-show (buffer-or-name)
  "Make watch buffer BUFFER-OR-NAME visible, and position acordingly.
If already visible, do nothing.

Currently the only positioning rule implemented is to split the R
process window in half.  The behavior is controlled by
`split-window-sensibly' with parameters `split-height-threshold'
and `split-width-threshold' replaced by
`ess-watch-height-threshold' and `ess-watch-width-threshold'
respectively."
  (interactive)
  (unless (get-buffer-window ess-watch-buffer 'visible)
    (save-selected-window
      (ess-switch-to-ESS t)
      (let* ((split-width-threshold (or ess-watch-width-threshold
                                        split-width-threshold))
             (split-height-threshold (or ess-watch-height-threshold
                                         split-height-threshold))
             (win (split-window-sensibly (selected-window))))
        (if win
            (set-window-buffer win buffer-or-name)
          (display-buffer buffer-or-name) ;; resort to usual mechanism if could not split
          )))))


(defun ess-watch-revert-buffer (ignore noconfirm)
  "Update the watch buffer
Arguments IGNORE and NOCONFIRM currently not used."
  (ess-watch)
  (message "Watch reverted"))

(defvar ess-watch-mode-map nil
  "Keymap for the *R watch* buffer.

\\{ess-watch-mode-map}
")

(unless ess-watch-mode-map
  (setq ess-watch-mode-map (make-sparse-keymap))
  (when (boundp 'special-mode-map)
    (set-keymap-parent ess-watch-mode-map special-mode-map))
  (define-key ess-watch-mode-map "?" 'ess-watch-help)
  (define-key ess-watch-mode-map "k" 'ess-watch-kill)
  ;; (define-key ess-watch-mode-map "u" 'ess-watch-undelete)
  ;; editing requires a little more work.
  (define-key ess-watch-mode-map "a" 'ess-watch-add)
  (define-key ess-watch-mode-map "i" 'ess-watch-insert)
  (define-key ess-watch-mode-map "e" 'ess-watch-edit-expression)
  (define-key ess-watch-mode-map "r" 'ess-watch-rename)
  (define-key ess-watch-mode-map "q" 'ess-watch-quit)
  (define-key ess-watch-mode-map "u" 'ess-watch-move-up)
  (define-key ess-watch-mode-map "U" 'ess-watch-move-down)
  (define-key ess-watch-mode-map "d" 'ess-watch-move-down)
  (define-key ess-watch-mode-map "n" 'ess-watch-next-block)
  (define-key ess-watch-mode-map "p" 'ess-watch-previous-block)
  ;; R mode keybindings.
  (define-key ess-watch-mode-map "\C-c\C-s" 'ess-watch-switch-process)
  (define-key ess-watch-mode-map "\C-c\C-y" 'ess-switch-to-ESS)
  (define-key ess-watch-mode-map "\C-c\C-z" 'ess-switch-to-end-of-ESS)
  ;; Debug keys:
  )


(defface ess-watch-current-block-face
  '((default (:inherit highlight)))
  "Face used to highlight current watch block."
  :group 'ess-debug)

(defvar  ess-watch-current-block-overlay nil
  "The overlay for currently selected block in the R watch buffer .")
(make-variable-buffer-local 'ess-watch-current-block-overlay)

(defvar ess-watch-buffer "*R watch*"
  "Name of the watch buffer.")

(defvar  ess-watch-start-block "@----"  ;; fixme: make defcustom and modify the injected command correspondingly
  "String indicating the beginning of a block in watch buffer."
  ;; :group 'ess-debug
  ;; :type 'string
  )

(defvar ess-watch-start-expression "@---:"
  "String indicating the beginning of an R expression in watch buffer."
  ;; :group 'ess-debug
  ;; :type 'string
  )

(defcustom ess-watch-height-threshold nil
  "Minimum height for splitting *R* windwow sensibly to make space for watch window.
See `split-height-threshold' for a detailed description.

If nil, the value of `split-height-threshold' is used."
  :group 'ess-debug
  :type 'integer)

(defcustom ess-watch-width-threshold nil
  "Minimum width for splitting *R* windwow sensibly to make space for watch window.
See `split-width-threshold' for a detailed description.

If nil, the value of `split-width-threshold' is used."
  :group 'ess-debug
  :type 'integer)

(defcustom  ess-watch-scale-amount -1
  "The number of steps to scale the watch font down (up).
Each step scales the height of the default face in the watch
window by the variable `text-scale-mode-step' (a negative number
of steps decreases the height by the same amount)")

(defvar ess-watch-help nil
  "Keymap for the *R watch* buffer.

\\{ess-watch-mode-map}
")

(defun ess-watch-help ()
  (interactive)
  (describe-variable 'ess-watch-help))

(defun ess-watch-block-limits-at-point ()
  "Return start and end positions of the watch block."
  (interactive)
  (save-excursion
    (let ((curr (point))
          start-pos end-pos)
      (end-of-line)
      (setq start-pos
            (if (re-search-backward ess-watch-start-block nil t )
                (point)
              (point-min)))
      (goto-char curr)
      (beginning-of-line)
      (setq end-pos
            (if (re-search-forward ess-watch-start-block nil t)
                (match-beginning 0)
              (point-max)))
      (list start-pos end-pos))))

(defun ess-watch-block-at-point ()
  "return the current block's order count, 0 if no block was found."
  (save-excursion
    (let ((cur-point (point))
          (count 0))
      (goto-char (point-min))
      (while (re-search-forward ess-watch-start-block cur-point t)
        (setq count (1+ count)))
      count)))

(defun ess-watch-set-current (nr)
  "Move the overlay over the block with count NR in current watch buffer"
  (goto-char (point-min))
  (re-search-forward ess-watch-start-expression nil t nr)
  (goto-char (match-end 0))
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-limits-at-point)))


(defun ess-watch--make-alist ()
  "Create an association list of expression from current buffer (better be a watch buffer).
Each element of assoc list is of the form (pos name expr) where
pos is an unique integer identifying watch blocks by position,
name is a string giving the name of expression block, expr is a
string giving the actual R expression."
  (interactive)
  (save-excursion
    (let* ((reg-name (concat "^" ess-watch-start-block " *\\(\\S-*\\).*$"))
           (reg-expr (concat "^" ess-watch-start-expression "\\s-*\\(.*\\)$"))
           (reg-all (concat "\\(" reg-name "\\)\n\\(" reg-expr "\\)"))
           (pos 0) wal name expr)
      (goto-char (point-min))
      (while (re-search-forward reg-all nil t)
        (setq pos  (+ 1 pos))
        (setq name (match-string-no-properties 2))
        (setq expr (match-string-no-properties 4))
        (if (not (eq (string-to-number name) 0))  ;;if number of any kind set the name to ""
            (setq name ""))
        (setq wal
              (append wal (list (list pos name expr)))))
      wal)))

(defun ess-watch--parse-assoc (al)
  "Return a string of the form 'assign(\".ess_watch_expressions\", list(a = parse(expr_a), b= parse(expr_b)), envir = .GlobalEnv)'
ready to be send to R process. AL is an association list as return by `ess-watch--make-alist'"
  (concat ".ess_watch_assign_expressions(list("
          (mapconcat (lambda (el)
                       (if (> (length  (cadr el) ) 0)
                           (concat "`" (cadr el) "` = parse(text = '" (caddr el) "')")
                         (concat "parse(text = '" (caddr el) "')")))
                     al ", ")
          "))\n"))

(defun ess-watch--install-.ess_watch_expressions ()
  ;; used whenever watches are added/deleted/modified from the watch
  ;; buffer. this is the only way  to insert expressions into
  ;; .ess_watch_expressions object in R. Assumes R watch being the current
  ;; buffer, otherwise will most likely install empty list.
  (interactive)
  (process-send-string (ess-get-process ess-current-process-name)
                       (ess-watch--parse-assoc (ess-watch--make-alist)))
  ;;todo: delete the prompt at the end of proc buffer todo: defun ess-send-string!!
  (sleep-for 0.05)  ;; need here, if ess-command is used immediately after,  for some weird reason the process buffer will not be changed
  )


(defun ess-watch-quit ()
  "Quit (kill) the watch buffer.
If watch buffer exists, it is displayed during the debug
process. The only way to avoid the display, is to kill the
buffer."
  (interactive)
  (kill-buffer) ;; dedicated, window is deleted unless not the only one
  )

;;;_  + MOTION
(defun ess-watch-next-block (&optional n)
  "Move the overlay over the next block.
Optional N if supplied gives the number of steps forward backward-char."
  (interactive "P")
  (setq n (prefix-numeric-value n))
  (goto-char (overlay-end ess-watch-current-block-overlay))
  (unless (re-search-forward ess-watch-start-expression nil t n)
    (goto-char (point-min)) ;;circular but always moves to start!
    (re-search-forward ess-watch-start-expression nil t 1))
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-limits-at-point)))

(defun ess-watch-previous-block (&optional n)
  "Move the overlay over the previous block.
Optional N if supplied gives the number of backward steps."
  (interactive "P")
  (setq n (prefix-numeric-value n))
  (goto-char (overlay-start ess-watch-current-block-overlay))
  (unless (re-search-backward ess-watch-start-expression nil t n)
    (goto-char (point-max)) ;;circular but always moves to last!
    (re-search-backward ess-watch-start-expression nil t 1))
  (goto-char (match-end 0))
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-limits-at-point)))

;;;_  + BLOCK MANIPULATION and EDITING
(defun ess-watch-rename ()
  "Rename the currently selected watch block. "
  (interactive)
  (end-of-line)
  (unless (re-search-backward ess-watch-start-block nil 1)
    (error "Can not find a watch block"))
  (let ((reg-name (concat ess-watch-start-block " *\\(\\S-*\\).*$"))
        name start end)
    ;; (reg-expr (concat "^" ess-watch-start-expression "\\s-*\\(.*\\)$"))
    ;; (reg-all (concat "\\(" reg-name "\\)\n\\(" reg-expr "\\)"))
    ;; (pos 0) wal name expr)
    (unless (re-search-forward reg-name (point-at-eol) 1)
      (error "Can not find the name substring in the current watch block "))
    (setq name (match-string-no-properties 1))
    (setq start (match-beginning 1))
    (setq end (match-end 1))
    (goto-char start)
    ;; todo: highlight the name in R-watch here
    (setq name (read-string (concat "New name (" name "): ") nil nil name) )
    (setq buffer-read-only nil)
    (delete-region start end)
    (insert name)
    (setq buffer-read-only t)
    (ess-watch--install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly (current-buffer))))

(defun ess-watch-edit-expression ()
  "Edit in the minibuffer the R expression from the current watch block. "
  (interactive)
  (end-of-line)
  (unless (re-search-backward ess-watch-start-block nil 1)
    (error "Can not find a watch block"))
  (let ((reg-expr (concat ess-watch-start-expression " *\\(.*\\)$"))
        expr start end)
    (unless (re-search-forward reg-expr nil 1)
      (error "Can not find an expression string in the watch block"))
    (setq expr (match-string-no-properties 1))
    (setq start (match-beginning 1))
    (setq end (match-end 1))
    (goto-char start)
    ;; todo: highlight the name in R-watch here
    (setq expr (read-string  "New expression: " expr nil expr) )
    (setq buffer-read-only nil)
    (delete-region start end)
    (insert expr)
    (setq buffer-read-only t)
    (ess-watch--install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly (current-buffer))))

(defun ess-watch-add ()
  "Ask for new R expression and name and append it to the end of the list of watch expressions"
  (interactive)
  (let (nr expr name)
    (goto-char (point-max))
    (setq nr (number-to-string (1+ (ess-watch-block-at-point))))
    (setq name nr)
    ;; (setq name (read-string (concat "Name (" nr "):") nil nil nr ))  ;;this one is quite annoying and not really needed than for logging
    (setq expr (read-string "New expression: " nil nil "\"Empty watch!\""))
    (setq buffer-read-only nil)
    (insert (concat "\n" ess-watch-start-block " " name " -@\n" ess-watch-start-expression " " expr "\n"))
    (setq buffer-read-only t)
    (ess-watch--install-.ess_watch_expressions)))

(defun ess-watch-insert ()
  "Ask for new R expression and name and insert it in front of current watch block"
  (interactive)
  (let (nr expr name)
    (setq nr (number-to-string (ess-watch-block-at-point)))
    (setq name nr)
    ;; (setq name (read-string (concat "Name (" nr "):") nil nil nr ))
    (setq expr (read-string "New expression: " nil nil "\"Empty watch!\""))
    (re-search-backward ess-watch-start-block nil 1) ;;point-min if not found
    (setq buffer-read-only nil)
    (insert (concat "\n" ess-watch-start-block " " name " -@\n" ess-watch-start-expression " " expr "\n"))
    (setq buffer-read-only t)
    (ess-watch--install-.ess_watch_expressions)))

(defun ess-watch-move-up ()
  "Move the current block up."
  (interactive)
  (let ((nr (ess-watch-block-at-point))
        wbl)
    (when (> nr 1)
      (setq buffer-read-only nil)
      (setq wbl (apply 'delete-and-extract-region  (ess-watch-block-limits-at-point)))
      (re-search-backward ess-watch-start-block nil t 1) ;; current block was deleted, point is at the end of previous block
      (insert wbl)
      (ess-watch--install-.ess_watch_expressions)
      (setq buffer-read-only t))))


(defun ess-watch-move-down ()
  "Move the current block down."
  (interactive)
  (let ((nr (ess-watch-block-at-point))
        (nr-all (save-excursion (goto-char (point-max))
                                (ess-watch-block-at-point)))
        wbl)
    (when (< nr nr-all)
      (setq buffer-read-only nil)
      (setq wbl (apply 'delete-and-extract-region  (ess-watch-block-limits-at-point)))
      (end-of-line)
      (when (re-search-forward ess-watch-start-block nil 1 1) ;; current block was deleted, point is at the end of previous block or point-max
        (goto-char (match-beginning 0)))
      (insert wbl)
      (ess-watch--install-.ess_watch_expressions)
      (setq buffer-read-only t))))

(defun ess-watch-kill ()
  "Kill the current block"
  (interactive)
  (setq buffer-read-only nil)
  (apply 'delete-region (ess-watch-block-limits-at-point))
  (ess-watch--install-.ess_watch_expressions))

;;;_ + Debug/Undebug at point
(defun ess--dbg-get-signatures (method)
  "Get signatures for the method METHOD"
  (let ((tbuffer (get-buffer-create " *ess-command-output*")); initial space: disable-undo
        signatures curr-point)
    (save-excursion
      (ess-if-verbose-write (format "ess-get-signatures*(%s).. " method))
      (ess-command (concat "showMethods(\"" method "\")\n") tbuffer)
      (message ess-local-process-name)
      (message ess-current-process-name)
      (ess-if-verbose-write " [ok] ..\n")
      (set-buffer tbuffer)
      (goto-char (point-min))
      (if (not (re-search-forward "Function:" nil t))
          (progn (ess-if-verbose-write "not seeing \"Function:\".. \n")
                 (error (buffer-string))
                 ;; (error "Cannot trace  method '%s' (Is it a primitive method which you have already traced?)" method)
                 )
        ;; (setq curr-point (point))
        ;; (while (re-search-forward ", " nil t) ;replace all ", " with  ":" for better redability in completion buffers??
        ;;   (replace-match ":"))
        ;; (goto-char curr-point)
        (while (re-search-forward "^.+$" nil t)
          (setq signatures (cons (match-string-no-properties 0) signatures))))
                                        ;      (kill-buffer tbuffer)
      )
    signatures))


(defun ess-debug-flag-for-debugging ()
  "Set the debugging flag on a function.
Ask the user for a function and if it turns to be generic, ask
for signature and trace it with browser tracer."
  (interactive)
  (ess-force-buffer-current "Process to use: ")
  (let* ((tbuffer (get-buffer-create " *ess-command-output*")) ;; output buffer name is hard-coded in ess-inf.el
         (pkg (car (ess-r-package-get-info)))
         (all-functions (ess-get-words-from-vector
                         (if pkg
                             (format ".ess_all_functions(c('%s'))\n" pkg)
                           ".ess_all_functions()\n")))
         (obj-at-point (ess-helpobjs-at-point--read-obj))
         (default (and
                   obj-at-point
                   (let* ((reg (regexp-quote obj-at-point))
                          (matches (loop for el in all-functions
                                         if (string-match reg el) collect el)))
                     (car (sort matches (lambda (a b) (< (length a) (length b))))))))
         (ufunc (ess-completing-read "Debug" all-functions
                                     nil nil nil nil (or default obj-at-point)))
         signature default-string out-message)
    ;; FIXME: Most of the following logic should be in R
    (if (ess-boolean-command (format "as.character(isGeneric('%s'))\n" ufunc))

        ;; it's S4 generic:
        (save-excursion
          ;; ask for exact signature
          (setq signature
                (ess-completing-read (concat "Method for generic '" ufunc "'")
                                     (ess--dbg-get-signatures ufunc) ;signal an error if not found
                                     nil t nil nil "*default*"))
          (if (equal signature "*default*")
              ;;debug, the default ufunc
              (ess-command (format "trace('%s', tracer = browser)\n" ufunc) tbuffer)
            (ess-command (format "trace('%s', tracer = browser, signature = c('%s'))\n" ufunc signature) tbuffer))
          (with-current-buffer tbuffer
            ;; give appropriate message or error
            (message (buffer-substring-no-properties (point-min) (point-max)))))

      ;;else, not an S4 generic
      (when (ess-boolean-command (format "as.character(.knownS3Generics['%s'])\n" ufunc))
        ;; it's S3 generic:
        (setq all-functions
              (ess-get-words-from-vector
               (format "local({gens<-methods('%s');as.character(gens[attr(gens, 'info')$visible])})\n" ufunc)))
        (setq all-functions
              ;; cannot debug non-visible methods
              (delq nil (mapcar (lambda (el)
                                  (if (not (char-equal ?* (aref el (1- (length el))))) el))
                                all-functions)))
        (setq ufunc (ess-completing-read (format "Method for S3 generic '%s'" ufunc)
                                         (cons ufunc all-functions) nil t)))
      (ess-command (format ".ess_dbg_flag_for_debuging('%s')\n" ufunc)))))


(defun ess-debug-unflag-for-debugging ()
  "Prompt for the debugged/traced function or method and undebug/untrace it."
  (interactive)
  (let ((tbuffer (get-buffer-create " *ess-command-output*")); initial space: disable-undo\
        (debugged (ess-get-words-from-vector
                   (if nil ;; FIXME: was checking `ess-developer-packages`
                       (format ".ess_dbg_getTracedAndDebugged(c('%s'))\n"
                               (mapconcat 'identity ess-developer-packages "', '"))
                       ".ess_dbg_getTracedAndDebugged()\n")))
        out-message fun def-val)
    ;; (prin1 debugged)
    (if (eq (length debugged) 0)
        (setq out-message "No debugged or traced functions/methods found")
      (setq def-val (if (eq (length debugged) 1)
                        (car debugged)
                      "*ALL*"))
      (setq fun (ess-completing-read "Undebug" debugged nil t nil nil def-val))
      (if (equal fun "*ALL*" )
          (ess-command (concat ".ess_dbg_UndebugALL(c(\"" (mapconcat 'identity debugged "\", \"") "\"))\n") tbuffer)
        (ess-command (format ".ess_dbg_UntraceOrUndebug(\"%s\")\n" fun) tbuffer))
      (with-current-buffer  tbuffer
        (if (= (point-max) 1) ;; not reliable todo:
            (setq out-message (format  "Undebugged '%s' " fun))
          (setq out-message (buffer-substring-no-properties (point-min) (point-max))) ;; untrace info or warning, or error occurred
          )))
    (message out-message)))

;;;_ * Kludges and Fixes
;;; delete-char and delete-backward-car do not delete whole intangible text
(defadvice delete-char (around ess-delete-backward-char-intangible activate)
  "When about to delete a char that's intangible, delete the whole intangible region
Only do this when #chars is 1"
  (if (and (eq major-mode 'ess-mode)
           (= (ad-get-arg 0) 1)
           (get-text-property (point) 'intangible))
      (progn
        (kill-region (point) (or (next-single-property-change (point) 'intangible)
                                 (point-max)))
        (indent-for-tab-command))
    ad-do-it))

(defadvice delete-backward-char (around ess-delete-backward-char-intangible activate)
  "When about to delete a char that's intangible, delete the whole intangible region
Only do this when called interactively and  #chars is 1"
  (if (and (eq major-mode 'ess-mode)
           (= (ad-get-arg 0) 1)
           (> (point) (point-min))
           (get-text-property (1- (point)) 'intangible))
      (progn
        (let ((beg (or (previous-single-property-change (point) 'intangible)
                       (point-min))))
          (kill-region beg (point))))
    ad-do-it))

;; reported as bug#21368

;; ;; previous-line gets stuck if next char is intangible
;; reported 
;; (defadvice previous-line (around ess-fix-cursor-stuck-at-intangible-text activate)
;;   "When about to move to previous line when next char is
;; intangible, step char backward first"
;;   (when (and (eq major-mode 'ess-mode)
;;              (or (null (ad-get-arg 0))
;;                  (= (ad-get-arg 0) 1))
;;              (get-text-property (point) 'intangible))
;;     (goto-char (1- (point))))
;;   ad-do-it)

;; (ad-remove-advice 'previous-line 'around 'ess-fix-cursor-stuck-at-intangible-text)

(make-obsolete-variable 'ess-dbg-blink-ref-not-found-face  'ess-debug-blink-ref-not-found-face "ESS 13.05")
(make-obsolete-variable 'ess-dbg-blink-same-ref-face  'ess-debug-blink-same-ref-face "ESS 13.05")
(make-obsolete-variable 'ess-dbg-current-debug-line-face 'ess-debug-current-debug-line-face "ESS 13.05")
(make-obsolete-variable 'ess-dbg-error-action nil "ESS 13.05")
(make-obsolete-variable 'ess-dbg-error-action-alist 'ess-debug-error-action-alist "ESS 13.05")
(make-obsolete-variable 'ess-dbg-blink-interval 'ess-debug-blink-interval "ESS 13.05")
(make-obsolete-variable 'ess-dbg-indicator 'ess-debug-indicator "ESS 13.05")
(make-obsolete-variable 'ess-dbg-ask-for-file 'ess-debug-ask-for-file "ESS 13.05")
(make-obsolete 'ess-dbg-set-error-action 'ess-debug-set-error-action "ESS 13.05")
(make-obsolete 'ess-dbg-toggle-error-action 'ess-debug-toggle-error-action "ESS 13.05")
(make-obsolete 'ess-dbg-goto-input-event-marker 'ess-debug-goto-input-event-marker "ESS 13.05")
(make-obsolete 'ess-dbg-goto-debug-point 'ess-debug-goto-debug-point "ESS 13.05")
(make-obsolete 'ess-dbg-insert-in-forward-ring 'ess-debug-insert-in-forward-ring "ESS 13.05")
(make-obsolete 'ess-dbg-start 'ess-debug-start "ESS 13.05")
(make-obsolete 'ess-dbg-stop 'ess-debug-stop "ESS 13.05")
(make-obsolete 'ess-dbg-command-digit 'ess-debug-command-digit "ESS 13.05")
(make-obsolete 'ess-dbg-command-n 'ess-debug-command-next "ESS 13.05")
(make-obsolete 'ess-dbg-command-Q 'ess-debug-command-quit "ESS 13.05")
(make-obsolete 'ess-dbg-command-c 'ess-debug-command-continue "ESS 13.05")
(make-obsolete 'ess-dbg-flag-for-debugging 'ess-debug-flag-for-debugging "ESS 13.05")
(make-obsolete 'ess-dbg-unflag-for-debugging 'ess-debug-unflag-for-debugging "ESS 13.05")
(make-obsolete-variable 'ess-tb-last-input-fringe-face 'ess-tracebug-last-input-fringe-face "ESS 13.05")
(make-obsolete-variable 'ess-tb-next-error-function 'ess-tracebug-next-error-function "ESS 13.05")

(ess-if-verbose-write "\n<- debug done")
(provide 'ess-tracebug)

;;; ess-tracebug.el ends here
