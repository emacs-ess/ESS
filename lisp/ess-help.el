;;; ess-help.el --- Support for viewing ESS help files

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith
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

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Code for dealing with ESS help files.  See README.<LANGUAGE> where
;; <LANGUAGE> is one of `S', `SAS', `Stata'or `XLispStat'.

;;; Code:

 ; Requires and autoloads

(eval-when-compile
  (require 'reporter)
  (require 'ess-inf)
  (require 'info))

(require 'ess)

(autoload 'ess-eval-region              "ess-inf" "[autoload]" t)
(autoload 'ess-eval-region-and-go       "ess-inf" "[autoload]" t)
(autoload 'ess-eval-function            "ess-inf" "[autoload]" t)
(autoload 'ess-eval-function-and-go     "ess-inf" "[autoload]" t)
(autoload 'ess-eval-line                "ess-inf" "[autoload]" t)
(autoload 'ess-eval-line-and-go         "ess-inf" "[autoload]" t)
(autoload 'ess-eval-line-and-step       "ess-inf" "[autoload]" t)

(autoload 'ess-goto-beginning-of-function-or-para    "ess-mode" "[autoload]" t)
(autoload 'ess-goto-end-of-function-or-para          "ess-mode" "[autoload]" t)

(autoload 'ess-load-file                "ess-inf" "[autoload]" t)
(autoload 'ess-command                  "ess-inf" "(autoload)" nil)
(autoload 'ess-display-temp-buffer      "ess-inf" "(autoload)" nil)
(autoload 'ess-switch-to-ESS            "ess-inf" "(autoload)" nil)
(autoload 'ess-read-object-name-default "ess-inf" "(autoload)" nil)
(autoload 'ess-make-buffer-current      "ess-inf" "(autoload)" nil)
(autoload 'ess-search-list              "ess-inf" "(autoload)" nil)
(autoload 'ess-get-object-list          "ess-inf" "(autoload)" nil)

(autoload 'ess-ddeclient-p              "ess-inf" "(autoload)" nil)

(autoload 'ess-display-help-on-object-ddeclient "ess-dde" "(autoload)" nil)


 ; ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The function ess-display-help-on-object
;;;; * The major mode ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ess--help-get-bogus-buffer-substring (buffer &optional nr-first)
  "Return non-nil if  BUFFER  looks like a bogus ESS help buffer.
Return the pair (match-beg. match-end) which can be used in error message.
NR-FIRST is the number of characters at the start of the buffer
to examine when deciding if the buffer if bogus.  If nil, the
first 150 characters of the buffer are searched."

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
                    (re-search-forward "^help .*not found" nr-first t))
             ))
      (ess-write-to-dribble-buffer
       (format " |--> %s [searching %s]\n" res searching))

      (when res
        (if searching
            (buffer-substring (match-beginning 0) (match-end 0))
          (buffer-string))))))

(defvar ess-help-type nil
  "Type of help file, help, index, vingettes etc.
Local in ess-help buffers.")
(make-variable-buffer-local 'ess-help-type)

(defvar ess-help-object nil
  "Name of the object the help is displayed for.
Is name of the package for package index.
Local in ess-help buffers.")
(make-variable-buffer-local 'ess-help-object)


(defun ess-display-help-on-object (object &optional command)
  "Display documentation for OBJECT in another window.
If prefix arg is given, force an update of the cached help topics
and query the ESS process for the help file instead of reusing an
existing buffer if it exists.  Uses the variable
`inferior-ess-help-command' for the actual help command.  Prompts
for the object name based on the cursor location for all cases
except the S-Plus GUI.  With S-Plus on Windows (both GUI and in
an inferior emacs buffer) the GUI help window is used.

If COMMAND is suplied, it is used instead of `inferior-ess-help-command'.
"
  (interactive
   (progn
     (ess-force-buffer-current)
     (when current-prefix-arg ;update cache if prefix
       (with-current-buffer (process-buffer (ess-get-process ess-current-process-name))
         (ess-process-put 'sp-for-help-changed? t)))
     (if (ess-ddeclient-p)
         (list (read-string "Help on: "))
       (list (ess-find-help-file "Help on")))))

  (if (or (ess-ddeclient-p)
          (equal inferior-ess-help-filetype "chm"))
      (if (ess-ddeclient-p)
          (ess-display-help-on-object-ddeclient object) ;; ddeclient version
        (ess-eval-linewise (concat "help(" object ")"))) ;; "chm" version

    ;; else: "normal", non-DDE behavior:
    (let* ((hb-name (concat "*help["
                            ess-current-process-name
                            "](" (replace-regexp-in-string "^\\?\\|`" "" object) ")*"))
           (old-hb-p    (get-buffer hb-name))
           (tbuffer     (get-buffer-create hb-name)))
      (when (or (not old-hb-p)
                current-prefix-arg
                (ess--help-get-bogus-buffer-substring old-hb-p))
        (ess-with-current-buffer tbuffer
          (setq ess-help-object object
                ess-help-type 'help)
          (ess--flush-help-into-current-buffer object command)))
      (unless (ess--help-kill-bogus-buffer-maybe tbuffer)
        (ess--switch-to-help-buffer tbuffer)))))

(defun ess--flush-help-into-current-buffer (object &optional command)
  (ess-write-to-dribble-buffer
   (format "(ess-help '%s' start  ..\n" (buffer-name (current-buffer))))

  ;; Ask the corresponding ESS process for the help file:
  (if buffer-read-only (setq buffer-read-only nil))
  (delete-region (point-min) (point-max))
  (ess-help-mode)
  (when (and (null command)
             (string-match "^R" ess-dialect))
    ;;VS[16-12-2012]: ugly hack to avoid tcl/tk dialogs (should go away soon)
    (let ((packs (ess-get-words-from-vector
                  (format "as.character(help('%s'))\n" object))))
      (when (> (length packs) 1)
        (setq
         command (format ;; crippled S3 :(
                  "do.call(structure, c('%s', attributes(help('%s'))))\n"
                  (ess-completing-read "Choose location" packs nil t)
                  object)))))
  (ess-command (format (or command inferior-ess-help-command)
                       object) (current-buffer))
  (ess-help-underline)
  ;;VS[03-09-2012]: todo: this should not be here:
  ;; Stata is clean, so we get a big BARF from this.
  (unless (string= ess-language "STA")
    (ess-nuke-help-bs))
  (goto-char (point-min))
  (set-buffer-modified-p 'nil)
  (setq buffer-read-only t)
  (setq truncate-lines nil))


(defun ess--help-kill-bogus-buffer-maybe (buffer)
  "Internal, try to kill bogus buffer with message. Return t if killed."
  (when ess-help-kill-bogus-buffers
    (let ((bog-mes  (ess--help-get-bogus-buffer-substring buffer)))
      (when bog-mes
        (when (< (length bog-mes) 10) ;;no message at all, how to treat this properly?
          (setq bog-mes (format "No documentation found; %s" bog-mes)))
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
              (browse-url (car (ess-get-words-from-vector (format com-get-file-path ess-help-object))))
            (when (functionp fun-get-file-path)
              (browse-url (funcall fun-get-file-path ess-help-object))))))
      )))

(defun ess--action-help-on-object (&optional button)
  "Provide help on object at the beginning of line.
It's intended to be used in R-index help pages. Load the package
if necessary.  It is bound to RET and C-m in R-index pages."
  (interactive)
  (let* ((string (button-label button))
         (command
          (when (equal ess-dialect "R")
            (cond ((string-match"::" string)
                   (format "?%s\n" (ess-R--sanitize-help-topic string)))
                  ((eq ess-help-type 'index)
                   (concat "?" ess-help-object "::`%s`\n"))
                  ))))
    (ess-display-help-on-object string command)
    ))


(defun ess-display-package-index ()
  "Prompt for package name and display its index."
  (interactive)
  (let ((object (buffer-name))
        (alist ess-local-customize-alist)
        (pname ess-local-process-name)
        pack buff all-packs  not-implemented
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
      (setq com-package-for-object "sub('package:', '', utils::find('%s'))\n"
            com-packages           ".packages(all.available=TRUE)\n"
            com-package-index      "help(package='%s', help_type='text')\n"
            reg-keyword             "^\\([-a-zA-Z0-9._@$]+\\)[^:\n]*$"
            reg-start              "^Index:"))
     ((string-match "julia" ess-dialect)
      (setq  com-packages           "_ess_list_categories()\n"
             com-package-index      "_ess_print_index(\"%s\")\n"
             reg-keyword             "^\\(.*+\\):$*"
             reg-start              ":"
             ))
     (t (error "Sorry, not implemented for %s " ess-dialect)))
    
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
    ;; (setq buff  (get-buffer-create (format "*help[%s](index:%s)*"  ess-dialect pack)))

    
    (ess--display-indexed-help-page
     (format com-package-index pack)
     reg-keyword
     (format "*help[%s](index:%s)*"  ess-dialect pack)
     'index nil nil reg-start pack)
    ))


(defalias 'ess-display-index 'ess-display-package-index)
(make-obsolete 'ess-display-index 'ess-display-package-index "ESS[12.09]")

(defun ess--display-indexed-help-page (command item-regexp title help-type
                                               &optional action help-echo reg-start help-object)
  "Internal function to display help pages with linked actions
  ;; COMMAND to produce the indexed help page
  ;; ITEM-REGEXP -- first subexpression is highlighted
  ;; TITLE of the help page
  ;; HELP-TYPE to be stored in `ess-help-type' local variable
  ;; ACTION is a function with no argument (default is `ess--action-help-on-object')
  ;; HELP-ECHO
  ;; REG-START gives the start location from where to search linkifying"
  (interactive)
  (let ((object (buffer-name))
        (alist          ess-local-customize-alist)
        (pname ess-local-process-name)
        (buff (get-buffer-create title))
        )
    (with-current-buffer buff
      (setq ess-help-object help-object)
      (ess-setq-vars-local (eval alist))
      (setq ess-help-sec-regex "\\(^\\s-.*\n\\)\\|\\(^\n\\)"
            ess-local-process-name pname)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (ess-help-mode)
      (ess-command command buff)
      (ess-help-underline)
      (set-buffer-modified-p 'nil)
      (goto-char (point-min))
      (when reg-start  ;; go to the beginning of listing
        (re-search-forward  reg-start  nil t))
      (when (and item-regexp (featurep 'emacs))
        ;;linkify the buffer
        (save-excursion
          (while (re-search-forward item-regexp nil t)
            (make-text-button (match-beginning 1) (match-end 1)
                              'mouse-face 'highlight
                              'action (or action #'ess--action-help-on-object)
                              'help-object (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                              'follow-link t
                              'help-echo (or help-echo "help on object")))
          ))
      
      ;; (save-excursion ;; why R places all these spaces?
      ;;   (goto-char (point-min))
      ;;   (while (re-search-forward " \\{10,\\} *" nil t)
      ;;     (replace-match "\t\t\t")))
      (setq buffer-read-only t)
      (setq ess-help-type help-type)
      (setq truncate-lines nil)
      )
    (unless (ess--help-kill-bogus-buffer-maybe buff)
      (ess--switch-to-help-buffer buff))
    ))


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

(defun ess-display-vignettes ()
  "Display vignettes if available for the current dialect."
  (interactive)
  (cond
   ((string-match "^R" ess-dialect) (ess-R-display-vignettes))
   (t (message "Sorry, not implemented for %s" ess-dialect))))

(defun ess-R-display-vignettes ()
  "Display R vignettes in ess-help-like buffer."
  (interactive)
  (if (featurep 'xemacs)
      (ess-eval-linewise "browseVignettes()\n") ;VS:cannot make regexp bellow work in xemacs
    (let ((buff (get-buffer-create " *ess-command-output*"))
          (alist ess-local-customize-alist)
          packs details
          p row)
      (ess-command "local({oo <- options(width=1000);print.default(browseVignettes());options(oo)})\n" buff)
      (with-current-buffer buff
        (save-excursion
          (goto-char (point-max))
          (while (re-search-backward "\\(?:\\$\\(\\sw+\\)$\\)\\|\\(?:^ ?\\[[0-9]+,\\]\\s-+\"\\(.*\\)\"\\s-*$\\)" nil t)
            (when (setq row (match-string-no-properties 2))
              (setq details
                    (append (list (split-string row  "\"\\s-+\""))
                            details))
              )
            (when (setq p (match-string-no-properties 1))
              (setq packs (append (list (cons p details)) packs))
              (setq details nil)))
          ))
      (setq buff  (get-buffer-create (format "*[%s]vignettes*"  ess-dialect)))
      (ess-with-current-buffer buff
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))
        (ess-setq-vars-local (eval alist))
        (setq ess-help-sec-regex "^\\w+:$"
              ess-help-type 'vignettes
              ess-local-process-name ess-current-process-name)
        (ess-help-mode)
        (set-buffer-modified-p 'nil)
        (goto-char (point-min))
        (dolist (el packs)
          (let ((pack (pop el)) path)
            (insert (format "\n\n%s:\n\n" (propertize pack 'face 'underline)))
            (dolist (el2 el)
              (setq path (nth 0 el2))
              ;; (if xemacs-p
              ;;     (insert (format "Dir: %s \t%s\n" (concat path "/doc/") (nth 2 el2)))
              (insert-text-button "Pdf"
                                  'mouse-face 'highlight
                                  'action #'ess--action-R-open-vignete
                                  'follow-link t
                                  'vignette (file-name-sans-extension (nth 4 el2))
                                  'package pack
                                  'help-echo (concat path "/doc/" (nth 4 el2)))
              (insert " ")
              (insert-text-button "Rnw"
                                  'mouse-face 'highlight
                                  'action #'ess--action-open-in-emacs
                                  'follow-link t
                                  'help-echo (concat path "/doc/" (nth 1 el2)))
              (insert " ")
              (insert-text-button "R"
                                  'mouse-face 'highlight
                                  'action #'ess--action-open-in-emacs
                                  'follow-link t
                                  'help-echo (concat path "/doc/" (nth 3 el2)))
              (insert (format "\t%s\n" (nth 2 el2)))
              )))
        (goto-char (point-min))
        (insert (propertize "\t\t**** Vignettes ****\n" 'face 'bold-italic))
        (delete-char 1)
        (setq buffer-read-only t))
      (ess--switch-to-help-buffer buff)
      )))

(defun ess--action-open-in-emacs (pos)
  (display-buffer (find-file-noselect (get-text-property pos 'help-echo))))
(defun ess--action-R-open-vignete (pos)
  (ess-command (format "vignette('%s', package='%s')\n"
                       (get-text-property pos 'vignette)
                       (get-text-property pos 'package))))

(defun ess-help-quit (&optional kill)
  "Quit help."
  ;;VS: `quit-window', doesn't focus previously selected buffer, which is annoying
  (interactive "P")
  (let* ((buffer (window-buffer))
         (obuffer (other-buffer buffer t)))
    (bury-buffer)
    (pop-to-buffer obuffer)
    (if kill
        (kill-buffer buffer))))

(defun ess-help-kill ()
  "Quit and kill the help buffer"
  (interactive)
  (ess-help-quit t))

(defun ess--switch-to-help-buffer (buff &optional curr-major-mode)
  "Switch to help buffer and take into account `ess-help-own-frame'.
For internal use. Used in `ess-display-help-on-object',
`ess-display-package-index', and `ess-display-vignettes'.
 CURR-MAJOR-MODE default to current major mode.
"
  (setq curr-major-mode (or curr-major-mode major-mode))
  (let ((special-display-regexps (if ess-help-own-frame '(".") nil))
        (special-display-frame-alist ess-help-frame-alist)
        (special-display-function (if (eq ess-help-own-frame 'one)
                                      'ess-help-own-frame
                                    special-display-function)))
    (if (eq curr-major-mode 'ess-help-mode)
        (if ess-help-own-frame
            (pop-to-buffer buff)
          (switch-to-buffer buff))
      (if ess-help-pop-to-buffer
          (pop-to-buffer buff)
        (ess-display-temp-buffer buff))
      )))



(defvar ess-help-frame nil
  "Stores the frame used for displaying R help buffers.")

(defun  ess-help-own-frame (buffer &rest ignore)
  "Put all ESS help buffers into `ess-help-frame'."
  ;; SJE: Code adapted from Kevin Rodgers.
  (if (frame-live-p ess-help-frame)
      (progn
        (or (frame-visible-p ess-help-frame)
            (make-frame-visible ess-help-frame))
        (raise-frame ess-help-frame)
        (select-frame ess-help-frame)
        (switch-to-buffer buffer)
        (selected-window))
    ;; else
    (let ((window (special-display-popup-frame buffer)))
      (set-window-dedicated-p window nil)
      (setq ess-help-frame (window-frame window))
      window)))



(defun ess-help-web-search ()
  "Search the web for documentation"
  (interactive)
  (ess-execute-dialect-specific ess-help-web-search-command "Search for: "))

;;*;; Major mode definition


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
    ess-doc-map
    )
  "ESS documentaion map.")


(defvar ess-help-mode-map
  (let ((map (make-keymap))); Full keymap, in order to
    (suppress-keymap map)   ; suppress all usual "printing" characters
    (when (boundp 'special-mode-map)
      (set-keymap-parent map special-mode-map))
    (define-key map "q" 'ess-help-quit)  ;was 'ess-switch-to-end-of-ESS)
    (define-key map "\C-m" 'next-line)
    ;; (define-key map "s" ess-help-sec-map)
    (define-key map "h" 'ess-display-help-on-object)
    (define-key map "w" 'ess-display-help-in-browser)
    (define-key map "i" 'ess-display-package-index)
    (define-key map "a" 'ess-display-help-apropos)
    (define-key map "v" 'ess-display-vignettes)
    ;; TODO: `electric mouse-2'
    ;; (define-key map [mouse-2] 'ess-display-help-on-object)
    (define-key map "l" 'ess-eval-line-and-step)
    (define-key map "r" 'ess-eval-region-and-go)
    (define-key map "f" 'ess-eval-function-or-paragraph-and-step)
    (define-key map "n" 'ess-skip-to-next-section)
    (define-key map "p" 'ess-skip-to-previous-section)
    (define-key map "/" 'isearch-forward)
    (define-key map "x" 'ess-kill-buffer-and-go)
    (define-key map "k" 'kill-this-buffer)
    (define-key map "?" 'ess-describe-help-mode)
    ;;-- those should be "inherited" from ess-mode-map ( ./ess-mode.el )
    (define-key map "\C-ch"   'ess-handy-commands)
    (define-key map "\C-c\C-s" 'ess-switch-process)
    (define-key map "\C-c\C-r" 'ess-eval-region)
    (define-key map "\C-c\M-r" 'ess-eval-region-and-go)
    (define-key map "\C-c\C-f" 'ess-eval-function)
    (define-key map "\M-\C-x"  'ess-eval-function)
    (define-key map "\C-c\M-f" 'ess-eval-function-and-go)
    (define-key map "\C-c\C-j" 'ess-eval-line)
    (define-key map "\C-c\C-n" 'ess-eval-line-and-step)
    (define-key map "\C-c\C-c"   'ess-eval-region-or-function-or-paragraph-and-step)
    (define-key map [(control return)] 'ess-eval-region-or-line-and-step)
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
  (list "ESS-help"
        ["Search Forward"               isearch-forward t]
        ["Next Section"                 ess-skip-to-next-section t]
        ["Previous Section"             ess-skip-to-previous-section t]
        ["Help on Section Skipping"     ess-describe-sec-map t]
        ["Beginning of Buffer"          beginning-of-buffer t]
        ["End of Buffer"                end-of-buffer t]
        "-"
        ["Help on ..."                  ess-display-help-on-object t]
        ["Apropos of ..."               ess-display-help-apropos t]
        ["Index of ..."                 ess-display-package-index t]
        ["Vignettes"                    ess-display-vignettes t]
        ["Open in Browser"              ess-display-help-in-browser t]
        "-"
        ["Eval Line"                    ess-eval-line-and-step t]
        ["Eval Paragraph & step"        ess-eval-paragraph-and-step t]
        ["Eval Region & Go"             ess-eval-region-and-go t]
        ["Switch to ESS Process"        ess-switch-to-ESS t]
        ["Switch to End of ESS Proc."   ess-switch-to-end-of-ESS t]
        ["Switch _the_ Process"         ess-switch-process t]
        "-"
        ["Kill Buffer"                  kill-this-buffer t]
        ["Kill Buffer & Go"             ess-kill-buffer-and-go t]
        "-"
        ["Handy comomands"              ess-handy-commands t]
        ["Describe ESS-help Mode"       ess-describe-help-mode t]
        )
  "Menu used in ess-help mode.")

(defun ess-help-mode ()
;;; Largely ripped from more-mode.el,
;;; originally by Wolfgang Rupprecht wolfgang@mgm.mit.edu
  "Mode for viewing ESS help files.
Use SPC and DEL to page back and forth through the file.
Use `n'  and `p' to move to next and previous section,
    `s' to jump to a particular section;   `s ?' for help.
Use `q' to return to your ESS session; `x' to kill this buffer first.
The usual commands for evaluating ESS source are available.
Other keybindings are as follows:
\\{ess-help-mode-map}"
  (interactive)
  (setq major-mode 'ess-help-mode)
  (setq mode-name "ESS Help")
  (use-local-map ess-help-mode-map)

  ;;; Keep <tabs> out of the code.
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  (if ess-mode-syntax-table ;;set in advance by ess-setq-local
      (set-syntax-table ess-mode-syntax-table))

  (require 'easymenu)
  (easy-menu-define ess-help-mode-menu-map ess-help-mode-map
    "Menu keymap for ess-help mode." ess-help-mode-menu)
  (easy-menu-add ess-help-mode-menu-map ess-help-mode-map)

  ;; Add the keys for navigating among sections; this is done
  ;; dynamically since different languages (e.g. S vs R) have different
  ;; section headings.

  (setq ess-help-sec-map (make-sparse-keymap))
  (dolist (pair ess-help-sec-keys-alist)
    (define-key ess-help-sec-map (char-to-string (car pair))
      'ess-skip-to-help-section))
  (define-key ess-help-sec-map "?" 'ess-describe-sec-map)
  (define-key ess-help-sec-map ">" 'end-of-buffer)
  (define-key ess-help-sec-map "<" 'beginning-of-buffer)
  (define-key ess-help-mode-map "s" ess-help-sec-map)

  (run-hooks 'ess-help-mode-hook))

;;*;; User commands defined in ESS help mode

(defun ess-skip-to-help-section nil
  "Jump to a section heading of a help buffer.  The section selected
is determined by the command letter used to invoke the command, as
indicated by `ess-help-sec-keys-alist'.  Use \\[ess-describe-sec-map]
to see which keystrokes find which sections."
  (interactive)
  (let ((old-point (point))
        (case-fold-search nil))
    (goto-char (point-min))
    (let ((the-sec (cdr (assoc (if (featurep 'xemacs) last-command-char last-command-event)
                               ess-help-sec-keys-alist))))
      (if (not the-sec) (error "Invalid section key: %c"
                               last-command-event)
        (if (re-search-forward (concat "^" the-sec) nil t) nil
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

(defun ess-describe-help-mode nil
  "Display help for `ess-mode'."
  (interactive)
  (describe-function 'ess-help-mode))

(defun ess-kill-buffer-and-go nil
  "Kill the current buffer and switch back to the ESS process."
  (interactive)
  (kill-buffer (current-buffer))
  (ess-switch-to-ESS nil))

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

(defun ess-helpobjs-at-point (slist)
  ;;; Return a list (def obj fun) where OBJ is a name at point, FUN - name of
  ;;; the function call point is in. DEF is either OBJ or FUN (in that order)
  ;;; which has a a help file, i.e. it is a member of slist (string-list). nil
  ;;; otherwise
  (let ((obj (ess-read-object-name-default))
        (fun (condition-case ()
                 (save-excursion
                   (save-restriction
                     (narrow-to-region (max (point-min) (- (point) 1000))
                                       (point-max))
                     (backward-up-list 1)
                     (backward-char 1)
                     (ess-read-object-name-default)))
               (error nil))))
    (if (and obj (not (string-match "[[:alpha:]]" obj))) ;;exclude numbers
      (setq obj nil))
    (list (or (car (member obj slist))
              (car (member fun slist)))
          obj fun)))

;; defunct old name:
;; (defun ess-read-helpobj-name-default (slist)
;;   (car (delq nil (ess-helpobjs-at-point slist))))

(defun ess-find-help-file (p-string)
  "Find help, prompting for P-STRING.  Note that we can't search SAS,
Stata or XLispStat for additional information."
  (ess-make-buffer-current)
  (if ess-get-help-topics-function
      (let* ((help-files-list (funcall ess-get-help-topics-function ess-current-process-name))
             (hlpobjs (ess-helpobjs-at-point help-files-list)))
        (ess-completing-read p-string (append (delq nil hlpobjs) help-files-list)
                             nil nil nil nil (car hlpobjs)))
    ;; (string-match "\\(XLS\\)\\|\\(STA\\)\\|\\(SAS\\)" ess-language)
    (read-string (format "%s: " p-string))))

;;*;; Utility functions

(defun ess-get-S-help-topics-function (name)
  "Return a list of current S help topics associated with process NAME.
If 'sp-for-help-changed?' process variable is non-nil or
`ess-help-topics-list' is nil, (re)-populate the latter and
return it.  Otherwise, return `ess-help-topics-list'."
  (with-ess-process-buffer nil
    (ess-write-to-dribble-buffer
     (format "(ess-get-help-topics-list %s) .." name))
    (if (or (not ess-help-topics-list)
            (ess-process-get 'sp-for-help-changed?))
        (progn
          (ess-process-put 'sp-for-help-changed? nil)
          (setq ess-help-topics-list
                (ess-uniq-list
                 (append (ess-get-object-list name 'exclude-1st)
                         (ess-get-help-files-list)
                         (ess-get-help-aliases-list)
                         ))))
      ;; else return the existing list
      ess-help-topics-list)))

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
  (let ((readrds (if (ess-current-R-at-least "2.13.0")
                     "readRDS"
                   ".readRDS")))
    (apply 'nconc
           (mapcar (lambda (str)
                     (let ((a-file (concat str "/help/aliases.rds")))
                       (and (file-exists-p a-file)
                            (ess-get-words-from-vector
                             (format "names(%s(\"%s\"))\n" readrds a-file)))))
                   (ess-get-words-from-vector "searchpaths()\n")))))

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

(defun ess-goto-info (node)
  "Display node NODE from ess-mode info."
  (require 'info)
  (split-window)
  ;;(other-window 1)
  (Info-goto-node (concat "(ess)" node)))


 ;; describe object at point

(defvar ess-describe-object-at-point-commands nil
  "Commands cycled by `ess-describe-object-at-point'. Dialect
specific.")
(make-variable-buffer-local 'ess-describe-at-point-commands)

(defvar ess--descr-o-a-p-commands nil)

(defun ess-describe-object-at-point ()
  "Get info for object at point, and display it in an electric buffer or tooltip.
This is an electric command (see `ess--execute-electric-command').

If region is active (`region-active-p') use it instead of the
object at point.

After invocation of this command, all standard emacs commands,
except those containing 'window' in their names, remove the
electric *ess-describe* buffer. Use `other-window' to switch to
*ess-describe* window.

Customize `ess-describe-at-point-method' if you wan to display
the description in a tooltip.

See also `ess-R-describe-object-at-point-commands' (and similar
option for other dialects).
"
  (interactive)
  (if (not ess-describe-object-at-point-commands)
      (message "Not implemented for dialect %s" ess-dialect)
    (ess-force-buffer-current)
    (let ((map (make-sparse-keymap))
          (objname (or (and (region-active-p)
                            (buffer-substring-no-properties (point) (mark)))
                       (symbol-at-point)))
          bs ess--descr-o-a-p-commands) ;; used in ess--describe-object-at-point
      (unless objname (error "No object at point "))
      (define-key map (vector last-command-event) 'ess--describe-object-at-point)
      ;; todo: put digits into the map
      (let* ((inhibit-quit t) ;; C-g removes the buffer
             (buf (ess--execute-electric-command
                   map (format "Press %s to cycle" (single-key-description last-command-event))
                   nil nil objname))
             ;; read full command
             (keys (read-key-sequence-vector ""))
             (command (and keys (key-binding keys))))
        (when (and (commandp command)
                   (bufferp buf)
                   (not (string-match "window" (symbol-name command))))
          (kill-buffer buf)) ;; bury does not work here :( (emacs bug?)
        (setq unread-command-events
              (append keys unread-command-events)))
      )))

(defun ess--describe-object-at-point (ev objname)
  (setq ess--descr-o-a-p-commands (or ess--descr-o-a-p-commands
                                      (symbol-value ess-describe-object-at-point-commands)))
  (let* ((com (format (car (pop ess--descr-o-a-p-commands)) objname))
         (buf (get-buffer-create "*ess-describe*"))
         pos)
    (unless (eq ess-describe-at-point-method 'tooltip)
      ;; can take some time for the command to execute
      (display-buffer buf))
    (sit-for .01)
    (ess-command (concat com "\n") buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (insert (propertize (format "%s:\n\n" com) 'face 'font-lock-string-face))
      (forward-line -1)
      (setq pos (point))
      (setq buffer-read-only t))
    (if (eq ess-describe-at-point-method 'tooltip)
        (ess-tooltip-show-at-point
         (with-current-buffer buf (buffer-string))  0 30)
      (display-buffer buf)
      (set-window-point (get-buffer-window buf) pos) ;; don't move window point
      buf)))


 ; Bug Reporting

(defun ess-submit-bug-report ()
  "Submit a bug report on the ess-mode package."
  (interactive)
  (require 'ess-mode)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p 't))
    (reporter-submit-bug-report
     "ess-bugs@r-project.org"
     (concat "ess-mode " (ess-version-string))
     (list 'ess-language
           'ess-dialect
           'ess-ask-for-ess-directory
           'ess-ask-about-transfile
           'ess-directory
           'ess-keep-dump-files
           'ess-source-directory
           'ess-use-ido
           'ess-use-eldoc
           'ess-use-tracebug
           'ess-use-auto-complete
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
       (insert (with-current-buffer "*ESS*"
                 (goto-char (point-max))
                 (forward-line -100)
                 (buffer-substring-no-properties (point) (point-max))))
       ))))



;;; Provide

(provide 'ess-help)

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

;;; ess-help.el ends here
