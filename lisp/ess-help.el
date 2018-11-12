;;; ess-help.el --- Support for viewing ESS help files  -*- lexical-binding: t; -*-

;; Copyright (C) 1989-1994, 2017 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997, A.J. Rossini <rossini@stat.sc.edu>
;; Copyright (C) 1998--2001 A.J. Rossini, Martin Maechler, Kurt Hornik and
;;      Richard M. Heiberger <rmh@temple.edu>.
;; Copyright (C) 2001--2010 A.J. Rossini, Richard M. Heiberger, Martin
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

;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/

;;; Commentary:

;; Code for dealing with ESS help files.  See README.<LANGUAGE> where
;; <LANGUAGE> is one of `S', `SAS', `Stata'or `XLispStat'.

;;; Code:

 ; Requires and autoloads
(require 'cl-lib)
(eval-when-compile
  (require 'tramp)
  (require 'reporter))
(require 'easymenu)
(require 'info)
(require 'ess-mode)
(require 'ess-inf)
(require 'ess-utils)

(declare-function ess-r-help-mode 'ess-r-mode)
(declare-function ess-stata-help-mode "ess-stata-lang")

(defvar ess--help-frame nil
  "Stores the frame used for displaying R help buffers.")

 ; ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The function ess-display-help-on-object
;;;; * The major mode ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ess--help-major-mode (&optional dialect)
  "Determine which help major mode to call, and call it.
DIALECT defaults to `ess-dialect'."
  (let ((ess-dialect (or dialect ess-dialect)))
    (cond ((string= ess-dialect "R") (require 'ess-r-mode) (ess-r-help-mode))
          ((string= ess-dialect "stata") (require 'ess-stata-lang) (ess-stata-help-mode))
          (t (ess-help-mode)))))

(defun ess--help-get-bogus-buffer-substring (buffer &optional nr-first)
  "Return non-nil if BUFFER looks like a bogus ESS help buffer.
Return the pair (match-beg. match-end) which can be used in error
message. NR-FIRST is the number of characters at the start of the
buffer to examine when deciding if the buffer if bogus. If nil,
the first 150 characters of the buffer are searched."
  (if (not nr-first) (setq nr-first 150))
  (with-current-buffer buffer
    (let ((PM (point-min))
          (case-fold-search t)
          searching res)
      (setq res
            (or  ;; evaluate up to first non-nil (or end):
             (< (- (point-max) PM) 80); buffer less than 80 chars
             (not (setq searching t))
             ;; todo: move to customize-alist
             (progn (goto-char PM) ;; R:
                    (re-search-forward "Error in help"    nr-first t))
             (progn (goto-char PM) ;; S-plus 5.1 :
                    (re-search-forward "^cat: .*--"       nr-first t))
             (progn (goto-char PM) ;; S version 3 ; R :
                    (re-search-forward "no documentation .+" nr-first t))
             (progn (goto-char PM) ;; stata
                    (re-search-forward "^help .*not found" nr-first t))))
      (ess-write-to-dribble-buffer
       (format " |--> %s [searching %s]\n" res searching))
      (when res
        (if searching
            (buffer-substring (match-beginning 0) (match-end 0))
          (buffer-string))))))

(defun ess-help-get-local-help-buffers ()
  (ess-force-buffer-current)
  (cl-remove-if-not
   (lambda (buffer)
     (let* ((pattern (concat "*help[" ess-current-process-name "]("))
            (name (buffer-name buffer))
            (candidate (when (> (length name) (length pattern))
                         (substring name 0 (length pattern))) ))
       (when (string= pattern candidate)
         buffer)))
   (buffer-list)))

(defvar ess-help-type nil
  "Type of help file, help, index, vingettes etc.
Local in `ess-help' buffers.")
(make-variable-buffer-local 'ess-help-type)

(defvar ess-help-object nil
  "Name of the object the help is displayed for.
Is name of the package for package index.
Local in `ess-help' buffers.")
(make-variable-buffer-local 'ess-help-object)

;;;###autoload
(defun ess-display-help-on-object (object &optional command)
  "Display documentation for OBJECT in another window.
If prefix arg is given, force an update of the cached help topics
and query the ESS process for the help file instead of reusing an
existing buffer if it exists. Uses the variable
`inferior-ess-help-command' for the actual help command. Prompts
for the object name based on the cursor location for all cases
except the S-Plus GUI. With S-Plus on Windows (both GUI and in an
inferior Emacs buffer) the GUI help window is used. If COMMAND is
suplied, it is used instead of `inferior-ess-help-command'."
  (interactive
   (progn
     (ess-force-buffer-current)
     (when current-prefix-arg ;update cache if prefix
       (ess-process-put 'sp-for-help-changed? t))
     (list (ess-find-help-file "Help on"))))
  (if (fboundp ess-display-help-on-object-function)
      (funcall ess-display-help-on-object-function object command)
    (let* ((hb-name (concat "*help[" ess-current-process-name "]("
                            (replace-regexp-in-string "^\\?\\|`" "" object) ")*"))
           (old-hb-p (get-buffer hb-name))
           (tbuffer (get-buffer-create hb-name)))
      (when (or (not old-hb-p)
                current-prefix-arg
                (ess--help-get-bogus-buffer-substring old-hb-p))
        (ess-with-current-buffer tbuffer
          (setq ess-help-object object
                ess-help-type 'help)
          (setq buffer-read-only t)
          (ess--flush-help-into-current-buffer object command)))
      (unless (ess--help-kill-bogus-buffer-maybe tbuffer)
        (ess--switch-to-help-buffer tbuffer)))))

;;;###autoload
(defalias 'ess-help 'ess-display-help-on-object)

(defun ess-build-help-command (object)
  (if (fboundp ess-build-help-command-function)
      (funcall ess-build-help-command-function object)
    ;; TODO-CLEANUP: Remove the inferior- prefix for consistency
    (format inferior-ess-help-command object)))

(defun ess--flush-help-into-current-buffer (object &optional command)
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (let ((command (if (and command (string-match-p "%s" command))
                       (format command object)
                     command)))
      (ess-command (or command (ess-build-help-command object)) (current-buffer)))
    (ess-help-underline)
    (unless (string= ess-language "STA")
      (ess-nuke-help-bs))
    (goto-char (point-min))
    (set-buffer-modified-p 'nil)
    (setq truncate-lines nil)
    (ess--help-major-mode)))

(defun ess--help-kill-bogus-buffer-maybe (buffer)
  "Internal, try to kill bogus BUFFER with message. Return t if killed."
  (when ess-help-kill-bogus-buffers
    (let ((bog-mes  (ess--help-get-bogus-buffer-substring buffer)))
      (when bog-mes
        ;; The following is giving erroneous messages when help is displayed in the browser
        ;; (when (< (length bog-mes) 10) ;;no message at all, how to treat this properly?
        ;;   (setq bog-mes (format "No documentation found; %s" bog-mes)))
        (ess-write-to-dribble-buffer
         (format "(ess-help: kill bogus buffer %s ..\n" (buffer-name buffer)))
        (message "%s" (replace-regexp-in-string  "\n" "" bog-mes))
        ;; (ding) ;; don't ding, in julia a lot of doc strings are very short
        (kill-buffer buffer)))))

(defun ess-display-help-in-browser ()
  "Displaying html help where available, using \\[browse-url]."
  (interactive)
  ;; Three ways to find html help, 1) ask sub-process 2) get url/file from subproces
  ;; 3) call elisp function to get the file path
  ;; For 2 and 3 call browse-url on the output
  (let (com-html-help                   ;1) command for sub-process to trigger
                                        ;help, must contain %s for help obj
        com-get-file-path               ;2) command for sub-process to return a
                                        ; location for the help page, must
                                        ; contain %s for help obj
        fun-get-file-path               ;3) elisp function to return the
                                        ;location, gets one argument, help topic
        not-implemented
        )
    (cond
     ((string-match "^R" ess-dialect)
      (setq com-html-help "help('%s', help_type='html')\n"))
     (t (setq not-implemented t))
     )
    (if not-implemented
        (message "Sorry, not implemented for %s " ess-dialect)
      (if (or (not ess-help-object)
              (not (eq ess-help-type 'help)))
          (message "No help topic found")
        (if com-html-help
            (ess-command (format com-html-help  ess-help-object))
          (require 'browse-url)
          (if com-get-file-path
              (browse-url (car (ess-get-words-from-vector
                                (format com-get-file-path ess-help-object))))
            (when (functionp fun-get-file-path)
              (browse-url (funcall fun-get-file-path ess-help-object)))))))))

(defun ess--button-action (&optional button)
  "Provide help on object at the beginning of line.
It's intended to be used in R-index help pages. Load the package
if necessary.  It is bound to RET and C-m in R-index pages."
  (interactive)
  (let* ((string (button-label button))
         (command (when (fboundp ess-build-help-command-function)
                    (funcall ess-build-help-command-function string))))
    (ess-display-help-on-object string command)))

(defun ess-display-package-index ()
  "Prompt for package name and display its index."
  (interactive)
  (let (
        pack all-packs
        ;; Available customization for ess languages/dialects:
        com-package-for-object ;command to get the package of current help object
        com-packages ;command to get a list of available packages (REQUIRED)
        com-package-index ;command to get the package index (REQUIRED)
        reg-keyword ;regexp used to find keywords for linking in index listing
                                        ; only (1st subexpression is used)
        reg-start ;regexp from where to start searching for keywords in index listing
        )
    (cond
     ((string-match "^R" ess-dialect)
      ;; carefully using syntax to be parsed in old R versions (no '::', '_'):
      (setq com-package-for-object "sub('package:', '', .ess.findFUN('%s'))\n"
            com-packages           ".packages(all.available=TRUE)\n"
            com-package-index      ".ess.help(package='%s', help.type='text')\n"
            reg-keyword             "^\\([^ \t\n:]+\\)"
            reg-start              "^Index:"))
     ((string-match "julia" ess-dialect)
      (setq com-packages           "_ess_list_categories()\n"
            com-package-index      "_ess_print_index(\"%s\")\n"
            reg-keyword             "^\\(.*+\\):$*"
            reg-start              ":"
            ))
     (t (error "Not implemented for %s " ess-dialect)))
    (when (and com-package-for-object
               ess-help-object
               (eq ess-help-type 'help))
      (setq pack (car (ess-get-words-from-vector
                       (format com-package-for-object ess-help-object)))))
    (setq all-packs (ess-get-words-from-vector com-packages))
    (unless pack ;try symbol at point
      (setq pack  (car (member (ess-read-object-name-default) all-packs))))
    (setq pack (ess-completing-read "Index of"
                                    all-packs nil nil nil nil pack))
    (ess--display-indexed-help-page
     (format com-package-index pack)
     reg-keyword
     (format "*help[%s](index:%s)*"  ess-dialect pack)
     'index nil nil reg-start pack)))

(defalias 'ess-display-index 'ess-display-package-index)
(make-obsolete 'ess-display-index 'ess-display-package-index "ESS[12.09]")

(defun ess--display-indexed-help-page (command item-regexp title help-type
                                               &optional action help-echo reg-start help-object)
  "Internal function to display help pages with linked actions.
COMMAND to produce the indexed help page,
ITEM-REGEXP -- first subexpression is highlighted,
TITLE of the help page,
HELP-TYPE to be stored in `ess-help-type' local variable,
ACTION is a function with no argument (default is `ess--button-action'),
HELP-ECHO if given becomes the help-echo property of the button,
REG-START gives the start location from where to search linkifying, and HELP-OBJECT becomes `ess-help-object'."
  (interactive)
  (let ((inhibit-modification-hooks t)
        (alist          ess-local-customize-alist)
        (pname ess-local-process-name)
        (buff (get-buffer-create title)))
    (with-current-buffer buff
      (setq ess-help-object help-object)
      (ess-setq-vars-local (eval alist))
      (setq ess-help-sec-regex "\\(^\\s-.*\n\\)\\|\\(^\n\\)"
            ess-local-process-name pname)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (ess--help-major-mode)
      (ess-command command buff)
      (ess-help-underline)
      (set-buffer-modified-p 'nil)
      (goto-char (point-min))
      (when reg-start  ;; go to the beginning of listing
        (re-search-forward  reg-start  nil t))
      (when item-regexp
        ;;linkify the buffer
        (save-excursion
          (while (re-search-forward item-regexp nil t)
            (make-text-button (match-beginning 1) (match-end 1)
                              'mouse-face 'highlight
                              'action (or action #'ess--button-action)
                              'help-object (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                              'follow-link t
                              'help-echo (or help-echo "help on object")))))
      ;; (save-excursion ;; why R places all these spaces?
      ;;   (goto-char (point-min))
      ;;   (while (re-search-forward " \\{10,\\} *" nil t)
      ;;     (replace-match "\t\t\t")))
      (setq buffer-read-only t)
      (setq ess-help-type help-type)
      (setq truncate-lines nil))
    (unless (ess--help-kill-bogus-buffer-maybe buff)
      (ess--switch-to-help-buffer buff))))

(defun ess-display-help-apropos (&optional pattern)
  "Create an ess-apropos buffer with a *linked* list of apropos topics."
  (interactive "sPattern: ")
  (let (com regexp)
    (cond ((equal ess-dialect "R")
           (setq com "help.search('%s')\n"
                 regexp "^\\([^ \t\n:]+::[^ \t\n:]+\\)[ \t\n]+"))
          ((equal ess-dialect "julia")
           (setq com "apropos(\"%s\")\n"
                 regexp "^\\(\\(\\w\\|\\s_\\)+\\)("))
          ((equal ess-dialect "stata")
           (setq com "hsearch %s\n"
                 regexp "^[\t ]*[0-9]+\\.[\t ]+\\(.+\\)$"))
          (t (error "Not implemented for dialect %s" ess-dialect)))
    (ess--display-indexed-help-page
     (format com pattern) regexp
     (format "*ess-apropos[%s](%s)*" ess-current-process-name pattern)
     'appropos)))

(defun ess-display-demos ()
  "Create an ess-demos buffer with a *linked* list of available demos."
  (interactive)
  (let (com regexp)
    (cond ((equal ess-dialect "R")
           (setq com "demo()\n"
                 regexp "^\\([^ \n:]+\\)  +"))
          (t (error "Not implemented for dialect %s" ess-dialect)))
    (ess--display-indexed-help-page
     com regexp
     (format "*ess-demos[%s]*" ess-current-process-name)
     'demos #'ess--action-demo)))

(defun ess--action-demo (&optional button)
  "Provide help on object at the beginning of line.
It's intended to be used in R-index help pages. Load the package
if necessary.  It is bound to RET and C-m in R-index pages."
  (interactive)
  (let* ((string (button-label button))
         (command
          (cond ((equal ess-dialect "R")
                 (format "demo('%s')\n" string))
                (t (error "Not implemented for dialect %s" ess-dialect)))))
    (ess-eval-linewise command)
    (ess-switch-to-end-of-ESS)))

(defun ess-display-vignettes (&optional all)
  "Display vignettes if available for the current dialect.
With (prefix) ALL non-nil, use `vignette(*, all=TRUE)`, i.e., from all installed
 packages, which can be *very* slow."
  (interactive "P")
  (cond
   ((equal ess-dialect "R") (ess-R-display-vignettes all))
   (t (message "Sorry, not implemented for %s" ess-dialect))))

(defun ess-R-display-vignettes (&optional all)
  "Display R vignettes in ess-help-like buffer..
With (prefix) ALL non-nil, use `vignette(*, all=TRUE)`, i.e., from all installed
 packages, which can be *very* slow."
  (interactive "P")
  (let* ((vslist (with-current-buffer
                     (ess-command
                      (format ".ess_vignettes(%s)\n" (if all "TRUE" "")))
                   (goto-char (point-min))
                   (when (re-search-forward "(list" nil t)
                     (goto-char (match-beginning 0))
                     (ignore-errors (eval (read (current-buffer)))))))
         (proc-name ess-current-process-name)
         (alist ess-local-customize-alist)
         (remote (file-remote-p default-directory))
         (buff (get-buffer-create (format "*[%s]vignettes*" ess-dialect)))
         (inhibit-modification-hooks t))
    (with-current-buffer buff
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (ess-setq-vars-local (eval alist))
      (setq ess-help-sec-regex "^\\w+:$"
            ess-help-type 'vignettes
            ess-local-process-name proc-name)
      (ess--help-major-mode)
      (set-buffer-modified-p 'nil)
      (goto-char (point-min))
      (dolist (el vslist)
        (let ((pack (car el)))
          (insert (format "\n\n%s:\n\n" (propertize pack 'face 'underline)))
          (dolist (el2 (cdr el))
            (let ((path (if remote
                            (with-no-warnings
                              ;; Have to wrap this in with-no-warnings because
                              ;; otherwise the byte compiler complains about
                              ;; calling tramp-make-tramp-file-name with an
                              ;; incorrect number of arguments on Both 26+ and 25 emacses.
                              (if (>= emacs-major-version 26)
                                  (with-parsed-tramp-file-name default-directory nil
                                                               (tramp-make-tramp-file-name method user domain host port (nth 1 el2)))
                                (with-parsed-tramp-file-name default-directory nil
                                                             (tramp-make-tramp-file-name method user host (nth 1 el2)))))
                          (nth 1 el2))))
              (insert-text-button "Pdf"
                                  'mouse-face 'highlight
                                  'action (if remote
                                              #'ess--action-open-in-emacs
                                            #'ess--action-R-open-vignette)
                                  'follow-link t
                                  'vignette (file-name-sans-extension (nth 2 el2))
                                  'package pack
                                  'help-echo (concat path "/doc/" (nth 2 el2)))
              (insert " ")
              (insert-text-button "Rnw"
                                  'mouse-face 'highlight
                                  'action #'ess--action-open-in-emacs
                                  'follow-link t
                                  'help-echo (concat path "/doc/" (nth 3 el2)))
              (insert " ")
              (insert-text-button "R"
                                  'mouse-face 'highlight
                                  'action #'ess--action-open-in-emacs
                                  'follow-link t
                                  'help-echo (concat path "/doc/" (nth 4 el2)))
              (insert (format "\t%s\n" (nth 0 el2)))))))
      (goto-char (point-min))
      (insert (propertize "\t\t**** Vignettes ****\n" 'face 'bold-italic))
      (unless (eobp) (delete-char 1))
      (setq buffer-read-only t))
    (ess--switch-to-help-buffer buff)))

(defun ess--action-open-in-emacs (pos)
  (display-buffer (find-file-noselect (get-text-property pos 'help-echo))))
(defun ess--action-R-open-vignette (pos)
  (ess-command (format "vignette('%s', package='%s')\n"
                       (get-text-property pos 'vignette)
                       (get-text-property pos 'package))))

(defalias 'ess-help-quit 'quit-window)
(make-obsolete 'ess-help-quit 'quit-window "16.04")

(defun ess--find-displayed-help-window ()
  (catch 'win
    (dolist (f (frame-list))
      (when (frame-visible-p f)
        (dolist (w (window-list f))
          (when (with-current-buffer (window-buffer w) (derived-mode-p 'ess-help-mode))
            (throw 'win w)))))))

(defun ess--switch-to-help-buffer (buff)
  "Switch to an ESS help BUFF.
For internal use.  Take into account variable `ess-help-own-frame'."
  (if (eq ess-help-own-frame t)
      ;; 0) always pop new frame
      (let* ((frame (make-frame ess-help-frame-alist))
             (action `(display-buffer-same-window (reusable-frames . ,frame))))
        (ess--display-help buff frame action))
    (let ((help-win (or
                     ;; if this doc is already displayed, reuse
                     (get-buffer-window buff 'all-frames)
                     ;; if help window visible, reuse
                     (and ess-help-reuse-window
                          (ess--find-displayed-help-window)))))
      (cond
       ;; 1) existent help window lying somewhere; reuse
       (help-win
        (set-window-buffer help-win buff)
        (let ((action '(display-buffer-reuse-window (reusable-frames . t))))
          (ess--display-help buff nil action)))
       ;; 2)
       ((eq ess-help-own-frame 'one)
        (let* ((frame (if (frame-live-p ess--help-frame)
                          ess--help-frame
                        (make-frame ess-help-frame-alist)))
               (action `(display-buffer-same-window (reusable-frames . ,frame))))
          (setq ess--help-frame frame)
          (ess--display-help buff frame action)))
       ;; 3) display help in other window
       (t
        (ess--display-help buff))))))

(defun ess--display-help (buff &optional frame action)
  (let ((action (or action `(nil (reusable-frames . ,(or frame ess-display-buffer-reuse-frames))))))
    (when (and (framep frame)
               (not (eq (selected-frame) frame)))
      ;; VS[04-05-2018]: `display-buffer` framework has no satisfactory
      ;; functionality to display buffers in selected windows. So, do some extra
      ;; frame selection here.
      (raise-frame frame)
      (if ess-help-pop-to-buffer
          (select-frame-set-input-focus frame)
        (select-frame frame)))
    (if ess-help-pop-to-buffer
        (pop-to-buffer buff action)
      (display-buffer buff action))))

(defun ess-help-web-search ()
  "Search the web for documentation."
  (interactive)
  (ess-execute-dialect-specific ess-help-web-search-command "Search for: "))

(defun ess-manual-lookup ()
  "Search manual for topic."
  (interactive)
  (ess-execute-dialect-specific ess-manual-lookup-command ))

(defun ess-reference-lookup ()
  "Search manual for topic."
  (interactive)
  (ess-execute-dialect-specific ess-reference-lookup-command))

(defvar ess-help-sec-map nil "Sub-keymap for ESS help mode.")
;; this breaks "s ?" rather than to fix any (unbroken !) thing:
;; (make-variable-buffer-local 'ess-help-sec-map)

(defvar ess-doc-map
  (let (ess-doc-map)
    (define-prefix-command 'ess-doc-map)
    (define-key ess-doc-map "\C-e" 'ess-describe-object-at-point)
    (define-key ess-doc-map "e" 'ess-describe-object-at-point)
    (define-key ess-doc-map "\C-d" 'ess-display-help-on-object)
    (define-key ess-doc-map "d" 'ess-display-help-on-object)
    (define-key ess-doc-map "\C-i" 'ess-display-package-index)
    (define-key ess-doc-map "i" 'ess-display-package-index)
    (define-key ess-doc-map "\C-a" 'ess-display-help-apropos)
    (define-key ess-doc-map "a" 'ess-display-help-apropos)
    (define-key ess-doc-map "\C-v" 'ess-display-vignettes)
    (define-key ess-doc-map "v" 'ess-display-vignettes)
    (define-key ess-doc-map "\C-o" 'ess-display-demos)
    (define-key ess-doc-map "o" 'ess-display-demos)
    (define-key ess-doc-map "\C-w" 'ess-help-web-search)
    (define-key ess-doc-map "w" 'ess-help-web-search)
    (define-key ess-doc-map "\C-m" 'ess-manual-lookup)
    (define-key ess-doc-map "m" 'ess-manual-lookup)
    (define-key ess-doc-map "\C-r" 'ess-reference-lookup)
    (define-key ess-doc-map "r" 'ess-reference-lookup)
    ess-doc-map
    )
  "ESS documentation map.")


(defvar ess-help-mode-map
  (let ((map (make-keymap))); Full keymap, in order to
    (define-key map "\C-m" 'next-line)
    ;; (define-key map "s" ess-help-sec-map)
    (define-key map "h" 'ess-display-help-on-object)
    (define-key map "w" 'ess-display-help-in-browser)
    (define-key map "i" 'ess-display-package-index)
    (define-key map "a" 'ess-display-help-apropos)
    (define-key map "v" 'ess-display-vignettes)
    ;; (define-key map [mouse-2] 'ess-display-help-on-object)
    (define-key map "l" 'ess-eval-line-visibly-and-step)
    (define-key map "r" 'ess-eval-region-and-go)
    (define-key map "f" 'ess-eval-function-or-paragraph-and-step)
    (define-key map "n" 'ess-skip-to-next-section)
    (define-key map "p" 'ess-skip-to-previous-section)
    (define-key map "/" 'isearch-forward)
    (define-key map "x" 'ess-kill-buffer-and-go)
    (define-key map "k" 'kill-this-buffer)
    ;;-- those should be "inherited" from ess-mode-map ( ./ess-mode.el )
    (define-key map "\C-c\C-s" 'ess-switch-process)
    (define-key map "\C-c\C-r" 'ess-eval-region)
    (define-key map "\C-c\M-r" 'ess-eval-region-and-go)
    (define-key map "\C-c\C-f" 'ess-eval-function)
    (define-key map "\M-\C-x"  'ess-eval-function)
    (define-key map "\C-c\M-f" 'ess-eval-function-and-go)
    (define-key map "\C-c\C-j" 'ess-eval-line)
    (define-key map "\C-c\C-n" 'ess-eval-line-visibly-and-step)
    (define-key map "\C-c\C-c"   'ess-eval-region-or-function-or-paragraph-and-step)
    (define-key map [(control return)] 'ess-eval-region-or-line-visibly-and-step)
    (define-key map "\C-c\M-j" 'ess-eval-line-and-go)
    (define-key map "\M-\C-a"  'ess-goto-beginning-of-function-or-para)
    (define-key map "\M-\C-e"  'ess-goto-end-of-function-or-para)
    (define-key map "\C-c\C-y" 'ess-switch-to-ESS)
    (define-key map "\C-c\C-z" 'ess-switch-to-end-of-ESS)
    (define-key map "\C-c\C-l" 'ess-load-file)
    (define-key map "\C-c\M-l" 'ess-load-file); alias, as in 'iESS' where C-c C-l is comint-list-*
    (define-key map "\C-c\C-v" 'ess-display-help-on-object)
    (define-key map "\C-c\C-k" 'ess-request-a-process)
    (define-key map "\C-c\C-d"   'ess-doc-map)
    (define-key map "\C-c\C-e"   'ess-extra-map)
    (define-key map "\C-c\C-t"   'ess-dev-map)
    map)
  "Keymap for ESS help mode.")

;; One reason for the following menu is to <TEACH> the user about key strokes
(defvar ess-help-mode-menu
  '("ESS-help"
    ["Search Forward"		isearch-forward t]
    ["Next Section"			ess-skip-to-next-section t]
    ["Previous Section"		ess-skip-to-previous-section t]
    ["Help on Section Skipping"	ess-describe-sec-map t]
    ["Beginning of Buffer"		beginning-of-buffer t]
    ["End of Buffer"		end-of-buffer t]
    "-"
    ["Help on ..."			ess-display-help-on-object t]
    ["Apropos of ..."		ess-display-help-apropos t]
    ["Index of ..."			ess-display-package-index t]
    ["Vignettes"			ess-display-vignettes t]
    ["Open in Browser"		ess-display-help-in-browser t]
    "-"
    ["Eval Line"			ess-eval-line-and-step t]
    ["Eval Paragraph & step"	ess-eval-paragraph-and-step t]
    ["Eval Region & Go"		ess-eval-region-and-go t]
    ["Switch to ESS Process"	ess-switch-to-ESS t]
    ["Switch to End of ESS Proc."	ess-switch-to-end-of-ESS t]
    ["Switch _the_ Process"		ess-switch-process t]
    "-"
    ["Kill Buffer"			kill-this-buffer t]
    ["Kill Buffer & Go"		ess-kill-buffer-and-go t]
    "-"
    ["Handy commands"		ess-handy-commands t])
  "Menu used in ess-help mode.")

(easy-menu-define ess-help-mode-menu-map ess-help-mode-map
  "Menu keymap for ess-help mode." ess-help-mode-menu)

(define-derived-mode ess-help-mode special-mode "ESS Help"
  "Mode for viewing ESS help files."
  ;; FIXME
  ;; (if ess-mode-syntax-table ;;set in advance by ess-setq-local
  ;;     (set-syntax-table ess-mode-syntax-table))
  (setq show-trailing-whitespace nil)

  ;; Add the keys for navigating among sections; this is done
  ;; dynamically since different languages (e.g. S vs R) have different
  ;; section headings.
  ;; FIXME: define ess-r-help-mode and others, move these there:
  ;; (setq ess-help-sec-map (make-sparse-keymap))
  ;; (dolist (pair ess-help-sec-keys-alist)
  ;;   (define-key ess-help-sec-map (char-to-string (car pair))
  ;;     'ess-skip-to-help-section))
  ;; (define-key ess-help-sec-map "?" 'ess-describe-sec-map)
  ;; (define-key ess-help-sec-map ">" 'end-of-buffer)
  ;; (define-key ess-help-sec-map "<" 'beginning-of-buffer)
  )


;;*;; User commands defined in ESS help mode

(defun ess-skip-to-help-section nil
  "Jump to a section heading of a help buffer.
The section selected is determined by the command letter used to
invoke the command, as indicated by `ess-help-sec-keys-alist'.
Use \\[ess-describe-sec-map] to see which keystrokes find which
sections."
  (interactive)
  (let ((old-point (point))
        (case-fold-search nil))
    (goto-char (point-min))
    (let ((the-sec (cdr (assoc last-command-event ess-help-sec-keys-alist))))
      (if (not the-sec) (error "Invalid section key: %c"
                               last-command-event)
        (if (re-search-forward (concat "^" the-sec) nil t)
            (recenter)
          (message "No %s section in this help. Sorry." the-sec)
          (goto-char old-point))))))

(defun ess-skip-to-next-section nil
  "Jump to next section in ESS help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (if (re-search-forward ess-help-sec-regex nil 'no-error) nil
      (message "No more sections."))))

(defun ess-skip-to-previous-section nil
  "Jump to previous section in ESS help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (if (re-search-backward ess-help-sec-regex nil 'no-error) nil
      (message "No previous section."))))

(defun ess-kill-buffer-and-go nil
  "Kill the current buffer and switch back to the ESS process."
  (interactive)
  (kill-buffer (current-buffer))
  (when (and ess-current-process-name (get-process ess-current-process-name))
    (ess-switch-to-ESS nil)))

(defun ess-describe-sec-map nil
  "Display help for the `s' key."
  (interactive)
  (let ((keys-alist ess-help-sec-keys-alist))
    (describe-function 'ess-skip-to-help-section)

    (with-current-buffer "*Help*"
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (insert "\n\nCurrently defined keys are:

Keystroke    Section
---------    -------\n")
      (dolist (cs keys-alist)
        (insert "    "
                (car cs)
                "      "
                (cdr cs) "\n"))
      (insert "\nFull list of key definitions:\n"
              (substitute-command-keys
               "\\{ess-help-sec-map}")))))

(defun ess-helpobjs-at-point--read-obj ()
  (let* ((obj (ess-read-object-name-default)))
    ;; Exclude numbers
    (unless (and obj (not (string-match "[[:alpha:]]" obj)))
      obj)))

(defun ess-unqualify-symbol (object)
  (if (string-match "^[[:alnum:].]+::?" object)
      (substring object (match-end 0))
    object))

(defun ess-helpobjs-at-point (slist)
  "Return a list (def obj fun).
Obj is a name at point, fun is the name of the function call
point is in, and def is either obj or fun (in that order) which
has a a help file, i.e. it is a member of slist (string-list).
nil otherwise."
  (let* ((obj (ess-helpobjs-at-point--read-obj))
         (unqualified-obj (and obj (ess-unqualify-symbol obj)))
         ;; FIXME: probably should use syntactic logic here
         (fun (ignore-errors
                (save-excursion
                  (save-restriction
                    (narrow-to-region (max (point-min) (- (point) 1000))
                                      (point-max))
                    (backward-up-list 1)
                    (backward-char 1)
                    (ess-read-object-name-default))))))
    (list (or (car (member obj slist))
              (when (member unqualified-obj slist)
                obj)
              (car (member fun slist)))
          obj fun)))

(defun ess-find-help-file (p-string)
  "Find help, prompting for P-STRING.
Note that we can't search SAS, Stata or XLispStat for additional information."
  (ess-make-buffer-current)
  (cond
   ((fboundp ess-find-help-file-function)
    (funcall ess-find-help-file-function p-string))
   ;; Fixme: Are `ess-find-help-file-function' and
   ;; `ess-get-help-topics-function' redundant?
   ((fboundp ess-get-help-topics-function)
    (let* ((help-files-list (funcall ess-get-help-topics-function ess-current-process-name))
           (hlpobjs (ess-helpobjs-at-point help-files-list)))
      (ess-completing-read p-string (append (delq nil hlpobjs) help-files-list)
                           nil nil nil nil (car hlpobjs))))
   (t
    (read-string (format "%s: " p-string)))))


;;*;; Utility functions

(defun ess-get-help-files-list ()
  "Return a list of files which have help available."
  (apply 'nconc
         (mapcar (lambda (str)
                   (let ((dirname (concat str "/.Help")))
                     (and (file-directory-p dirname)
                          (directory-files dirname))))
                 (ess-search-list))))

(defun ess-get-help-aliases-list ()
  "Return a list of aliases which have help available."
  (message "Retrieving RDS aliases...")
  ;; ess-command locks display, make sure the above message is visible
  (redisplay t)
  (ess-write-to-dribble-buffer "Processing RDS files ...\n")
  (prog1 (ess-get-words-from-vector ".ess.getHelpAliases()\n")
    (message "Retrieving RDS aliases...done")))

(defun ess-nuke-help-bs ()
  "Remove ASCII underlining and overstriking performed by ^H codes."
  ;; This function is a modification of nuke-nroff-bs in man.el from the
  ;; standard emacs 18 lisp library.
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
  (goto-char (point-min))
  (let ((case-fold-search nil)); 'URL' != 'url' ('libcurl: ' on ?capabilities)
    (while (re-search-forward "\\bURL: " nil t); test with ?rtags
      ;; quick fix for C-x f confusion (getting into tramp)
      (delete-region (match-beginning 0) (match-end 0))))
  ;; Crunch blank lines
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n\n*" nil t)
    (replace-match "\n\n"))
  ;; Nuke blanks lines at start.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point)))

(defun ess-help-underline ()
  "Replace _^H codes with underline face."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "_" nil t)
      (backward-delete-char 2)
      (put-text-property (point) (1+ (point)) 'face 'underline))))

;;*;; Link to Info
;;;###autoload
(defun ess-goto-info (node)
  "Display node NODE from `ess-mode' info."
  (require 'info)
  (split-window)
  ;;(other-window 1)
  (Info-goto-node (concat "(ess)" node)))


;; describe object at point

(defvar ess-describe-object-at-point-commands nil
  "Commands cycled by `ess-describe-object-at-point'.
Dialect specific.")
(make-variable-buffer-local 'ess-describe-at-point-commands)

(defvar ess--descr-o-a-p-commands nil)

(defun ess-describe-object-at-point ()
  "Get info for object at point, & display it in an electric buffer or tooltip.
If region is active use it instead of the object at point.

This is an electric command (`ess--execute-electric-command'),
which means that you can use the last key to cycle through the
action set (in this case `C-e'). After invocation of this command
all standard Emacs commands, except those containing 'window' in
their names, remove the electric *ess-describe* buffer. Use
`other-window' to switch to *ess-describe* window.

Customize `ess-describe-at-point-method' if you wan to display
the description in a tooltip. See also
`ess-r-describe-object-at-point-commands' (and similar option for
other dialects)."
  (interactive)
  (if (not ess-describe-object-at-point-commands)
      (message "Not implemented for dialect %s" ess-dialect)
    (ess-force-buffer-current)
    (let ((map (make-sparse-keymap))
          (objname (or (and (use-region-p)
                            (buffer-substring-no-properties (point) (mark)))
                       (ess-symbol-at-point)))
          ess--descr-o-a-p-commands) ;; used in ess--describe-object-at-point
      (unless objname (error "No object at point "))
      (define-key map (vector last-command-event) 'ess--describe-object-at-point)
      ;; todo: put digits into the map
      (let* ((inhibit-quit t) ;; C-g removes the buffer
             (buf (ess--execute-electric-command
                   map (format "Press %s to cycle"
                               (single-key-description last-command-event))
                   nil nil objname))
             ;; read full command
             (keys (read-key-sequence-vector ""))
             (command (and keys (key-binding keys))))
        (when (and (commandp command)
                   (bufferp buf)
                   (or (not (symbolp command)) ;; kill on lambdas
                       (not (string-match "window" (symbol-name command)))))
          (kill-buffer buf)) ;; bury does not work here :( (emacs bug?)
        (setq unread-command-events
              (append keys unread-command-events))))))

(defun ess--describe-object-at-point (_ev objname)
  (setq ess--descr-o-a-p-commands (or ess--descr-o-a-p-commands
                                      (symbol-value ess-describe-object-at-point-commands)))
  (let* ((com (format (car (pop ess--descr-o-a-p-commands)) objname))
         (buf (get-buffer-create "*ess-describe*"))
         pos)
    (unless (eq ess-describe-at-point-method 'tooltip)
      ;; can take some time for the command to execute
      (display-buffer buf))
    (sit-for .01)
    (ess-command (concat com "\n") buf) ;; erases buf
    (with-current-buffer buf
      (goto-char (point-min))
      (insert (propertize (format "%s:\n\n" com) 'face 'font-lock-string-face))
      (forward-line -1)
      (setq pos (point))
      ;; set the keys that we are used to in help mode
      (special-mode)
      (local-set-key "k" 'kill-this-buffer))
    (if (eq ess-describe-at-point-method 'tooltip)
        (ess-tooltip-show-at-point
         (with-current-buffer buf (buffer-string))  0 30)
      (display-buffer buf)
      (set-window-point (get-buffer-window buf) pos) ;; don't move window point
      buf)))


;;; Bug Reporting

;;;###autoload
(defun ess-submit-bug-report ()
  "Submit a bug report to the ESS maintainers."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p 't))
    (reporter-submit-bug-report
     "ess-bugs@r-project.org"
     (concat "ess-mode " (ess-version-string))
     (list 'ess-language
           'ess-dialect
           'ess-ask-for-ess-directory
           'ess-ask-about-transfile
           'default-directory
           'ess-keep-dump-files
           'ess-source-directory
           'ess-use-ido
           'ess-use-eldoc
           'ess-use-tracebug
           'ess-use-auto-complete
           'ess-use-company
           'ess-eval-visibly-p
           'ess-can-eval-in-background
           'ess-local-process-name)
     nil
     (lambda ()
       ;;(goto-char (point-max))
       (rfc822-goto-eoh)
       (forward-line 1)
       (insert "\n\n-------------------------------------------------------\n")
       (insert "This bug report will be sent to the ESS bugs email list\n")
       (insert "Press C-c C-c when you are ready to send your message.\n")
       (insert "-------------------------------------------------------\n\n")
       (insert (with-current-buffer ess-dribble-buffer
                 (goto-char (point-max))
                 (forward-line -100)
                 (buffer-substring-no-properties (point) (point-max))))))))


(provide 'ess-help)
;;; ess-help.el ends here
