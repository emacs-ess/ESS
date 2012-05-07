;;; ess-help.el --- Support for viewing ESS help files

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997, A.J. Rossini <rossini@stat.sc.edu>
;; Copyright (C) 1998--2001 A.J. Rossini, Martin Maechler, Kurt Hornik and
;;      Richard M. Heiberger <rmh@temple.edu>.
;; Copyright (C) 2001--2010 A.J. Rossini, Rich M. Heiberger, Martin
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

(autoload 'ess-beginning-of-function    "ess-mode" "[autoload]" t)
(autoload 'ess-end-of-function          "ess-mode" "[autoload]" t)

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

(defun ess-help-bogus-buffer-p (buffer &optional nr-first return-match debug)
  "Return non-nil if  BUFFER  looks like a bogus ESS help buffer.
NR-FIRST is the number of characters at the start of the buffer
to examine when deciding if the buffer if bogus.  If nil, the
first 120 characters of the buffer are searched.  Return pair
of (match-beg. match-end) when optional RETURN-MATCH is non-nil.
Utility used in \\[ess-display-help-on-object]."

  ;; search in first nr-first (default 120) chars only
  (if (not nr-first) (setq nr-first 150))

  (let* ((searching nil)
         (buffer-ok (bufferp buffer))
         (res
          (or (not buffer-ok)
              (save-excursion;; ask for new buffer if old one looks bogus ..
                (set-buffer buffer)
                (if debug
                    (ess-write-to-dribble-buffer
                     (format "(ess-help-bogus-buffer-p %s)" (buffer-name))))

                (let
                    ((PM (point-min))
                     (case-fold-search t) )
                  ;; todo: move to customize-alist
                  (or  ;; evaluate up to first non-nil (or end):
                   (not (setq searching t))
                   (progn (goto-char PM) ;; R:
                          (re-search-forward "Error in help"    nr-first t))
                   (progn (goto-char PM) ;; S-plus 5.1 :
                          (re-search-forward "^cat: .*--"       nr-first t))
                   (progn (goto-char PM) ;; S version 3 ; R :
                          (re-search-forward "no documentation for [^ \t\n]+" nr-first t))
                   (progn (goto-char PM) ;; stata
                          (re-search-forward "^help for.*not found" nr-first t))
                   (< (- (point-max) PM) 80); buffer less than 80 chars
                   )))
              )))
    (if debug
        (ess-write-to-dribble-buffer
         (format " |--> %s [searching %s]\n" res searching)))

    (if (and res return-match searching)
        (list (match-beginning 0) (match-end 0))
      ;; else
      res)))

(defvar ess-help-type nil
  "Type of help file, help, index, vingettes etc.
Local in ess-help buffers.")
(make-variable-buffer-local 'ess-help-type)

(defvar ess-help-object nil
  "Name of the object the help is displayed for.
Is name of the package for package index.
Local in ess-help buffers.")
(make-variable-buffer-local 'ess-help-object)


(defun ess-display-help-on-object (object)
  "Display documentation for OBJECT in another window.
If prefix arg is given, force an update of the cached help topics
and query the ESS process for the help file instead of reusing an
existing buffer if it exists.  Uses the variable
`inferior-ess-help-command' for the actual help command.  Prompts
for the object name based on the cursor location for all cases
except the S-Plus GUI.  With S-Plus on Windows (both GUI and in
an inferior emacs buffer) the GUI help window is used."
  (interactive
   (progn
     (ess-force-buffer-current)
     (when current-prefix-arg ;update cache if prefix
       (with-current-buffer (process-buffer (get-ess-process ess-current-process-name))
         (setq ess-sp-change t)))
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
                            "](" object ")*"))
           (old-hb-p    (get-buffer hb-name))
           (tbuffer     (get-buffer-create hb-name))
           ;;VS: this curr-* kludge is not needed here,
           ;;everything should be set by ess-setq-vars-local latter
           ;; (curr-help-sec-regex              ess-help-sec-regex)
           ;; (curr-help-sec-keys-alist ess-help-sec-keys-alist)
           ;; ....
           (alist               ess-local-customize-alist))
      (with-current-buffer tbuffer
        (ess-setq-vars-local (eval alist))
        (set-syntax-table ess-mode-syntax-table)
        (setq ess-help-object object
              ess-help-type 'help)

        (if (or (not old-hb-p)
                current-prefix-arg
                (ess-help-bogus-buffer-p old-hb-p nil nil 'debug)
                )

            ;; Ask the corresponding ESS process for the help file:
            (progn
              (if buffer-read-only (setq buffer-read-only nil))
              (delete-region (point-min) (point-max))
              (ess-help-mode)
              (setq ess-local-process-name ess-current-process-name)
              (ess-command (format inferior-ess-help-command object) tbuffer)
              (ess-help-underline)
              ;; Stata is clean, so we get a big BARF from this.
              (if (not (string= ess-language "STA"))
                  (ess-nuke-help-bs))

              (goto-char (point-min))))

        (let ((PM (point-min))
              (nodocs
               (ess-help-bogus-buffer-p (current-buffer) nil 'give-match )))
          (goto-char PM)
          (if (and nodocs
                   ess-help-kill-bogus-buffers)
              (progn
                (if (not (listp nodocs))
                    (setq nodocs (list PM (point-max))))
                (ess-write-to-dribble-buffer
                 (format "(ess-help: error-buffer '%s' nodocs (%d %d)\n"
                         (buffer-name) (car nodocs) (cadr nodocs)))
                ;; Avoid using 'message here -- may be %'s in string
                ;;(princ (buffer-substring (car nodocs) (cadr nodocs)) t)
                ;; MM [3/2000]: why avoid?  Yes, I *do* want message:
                (message "%s" (buffer-substring (car nodocs) (cadr nodocs)))
                ;; ^^^ fixme : remove new lines from the above {and abbrev.}
                (ding)
                (kill-buffer tbuffer))

            ;; else : show the help buffer.

            ;; Check if this buffer describes where help can be found in
            ;; various packages. (R only).  This is a kind of bogus help
            ;; buffer, but it should not be killed immediately even if
            ;; ess-help-kill-bogus-buffers is t.

            ;; e.g. if within R, the user does:

            ;; > options("help.try.all.packages" = TRUE)

            ;; > ?rlm

            ;; then a list of packages for where ?rlm is defined is
            ;; shown.  (In this case, rlm is in package MASS).  This
            ;; help buffer is then renamed *help[R](rlm in packages)* so
            ;; that after MASS is loaded, ?rlm will then show
            ;; *help[R](rlm)*

            (if (equal inferior-ess-program inferior-R-program-name)
                ;; this code should be used only for R processes.
                (save-excursion
                  (goto-char (point-min))
                  (if (looking-at "Help for topic")
                      (let
                          ( (newbuf
                             (concat "*help[" ess-current-process-name
                                     "](" object " in packages)*")))
                        ;; if NEWBUF already exists, remove it.
                        (if (get-buffer newbuf)
                            (kill-buffer newbuf))
                        (rename-buffer  newbuf)))))

            ;;dbg (ess-write-to-dribble-buffer
            ;;dbg        (format "(ess-help '%s' before switch-to..\n" hb-name)
            (set-buffer-modified-p 'nil)
            (toggle-read-only t))))
      (when (buffer-live-p tbuffer)
        (ess--switch-to-help-buffer tbuffer))
      )))


(defun ess-display-help-in-browser ()
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

(defun ess--action-help-on-object (&optional pos)
  "Provide help on object at the beginning of line.
It's intended to be used in R-index help pages. Load the package
if necessary.  It is bound to RET and C-m in R-index pages."
  (interactive)
  (save-excursion
    (let ((package (buffer-name))
          (link-beg (previous-single-property-change pos 'button))
          (link-end (next-single-property-change pos 'button))
          pre-commands ;command to send to process, %s is replaced by ess-help-object
          obj)
      (cond
       ((string-match "^R" ess-dialect)
        (setq pre-commands "require('%s')\n"))  ;;might not be loaded
       )
      (when (and  pre-commands
                  ess-help-object)
        (ess-command (format pre-commands ess-help-object)))
      (ess-display-help-on-object (get-text-property pos 'help-object))
      ))
  )

(defun ess-display-index ()
  "Prompt for package name and display its index."
  (interactive)
  (let ((object (buffer-name))
        (alist          ess-local-customize-alist)
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
     ((string-match "R" ess-dialect)
      (setq com-package-for-object "sub('package:', '', utils::find('%s'))\n"
            com-packages           ".packages(all.available = TRUE)\n"
            com-package-index      "help(package='%s', help_type = 'text')\n"
            reg-keyword             "^\\([-a-zA-Z0-9._@$]+\\)[^:\n]*$"
            reg-start              "^Index:"))
     (t (setq not-implemented t)))
    (if not-implemented
        (message "Sorry, not implemented for %s " ess-dialect)
      (when (and com-package-for-object
                 ess-help-object
                 (eq ess-help-type 'help))
        (setq pack (car (ess-get-words-from-vector
                         (format com-package-for-object ess-help-object))))
        )
      (setq all-packs (ess-get-words-from-vector com-packages))
      (unless pack ;try symbol at point
        (setq pack  (car (member (ess-read-object-name-default) all-packs))))
      (setq pack (ess-completing-read "Index of"
                                      all-packs nil nil nil nil pack))
      (setq buff  (get-buffer-create (format "*help[%s](index:%s)*"  ess-dialect pack)))
      (with-current-buffer buff
        (ess-setq-vars-local (eval alist))
        (set-syntax-table ess-mode-syntax-table)
        (setq ess-help-sec-regex "\\(^\\s-.*\n\\)\\|\\(^\n\\)"
              ess-help-type 'index
              ess-help-object pack
              ess-local-process-name ess-current-process-name)
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))
        (ess-help-mode)
        (ess-command (format com-package-index pack) buff)
        (ess-help-underline)
        (set-buffer-modified-p 'nil)
        (goto-char (point-min))
        (when reg-start  ;; go to the beginning of listing
          (re-search-forward  reg-start  nil t))
        (when (and reg-keyword (featurep 'emacs))
          ;;linkify the buffer
          (save-excursion
            (while (re-search-forward reg-keyword nil t)
              (make-text-button (match-beginning 1) (match-end 1)
                                'mouse-face 'highlight
                                'action #'ess--action-help-on-object
                                'help-object (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                                'follow-link t
                                'help-echo "help on object")))
          )
        (toggle-read-only t))
      (ess--switch-to-help-buffer buff)
      )))


(defun ess-display-vignettes ()
  (interactive)
  (cond
   ((string-match "^R" ess-dialect) (ess-R-display-vignettes))
   (t (message "Sorry, not implemented for %s" ess-dialect))
   ))

(defun ess-R-display-vignettes ()
  "Display R vignettes in ess-help-like buffer."
  (interactive)
  (if (featurep 'xemacs)
      (ess-eval-linewise "browseVignettes()\n") ;VS:cannot make regexp bellow work in xemacs
    (let ((buff (get-buffer-create " *ess-command-output*"))
          (alist                ess-local-customize-alist)
          packs details
          p row)
      (ess-command "local({oo <- options(width = 1000);print.default(browseVignettes());options(oo)})\n" buff)
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
      (with-current-buffer buff
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))
        (ess-setq-vars-local (eval alist))
        (set-syntax-table ess-mode-syntax-table)
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
        (toggle-read-only t))
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
`ess-display-index', and `ess-display-vignettes'.
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



;;; THIS WORKS!
;;(require 'w3)
                                        ;(defun ess-display-w3-help-on-object-other-window (object)
                                        ;  "Display R-documentation for OBJECT using W3"
                                        ;  (interactive "s Help on :")
                                        ;  (let* ((ess-help-url (concat ess-help-w3-url-prefix
                                        ;                              ess-help-w3-url-funs
                                        ;                              object
                                        ;                              ".html")))
;;(w3-fetch-other-window ess-help-url)
                                        ;    ))


;;*;; Major mode definition


(defvar ess-help-sec-map nil "Sub-keymap for ESS help mode.")
;; this breaks "s ?" rather than to fix any (unbroken !) thing:
;; (make-variable-buffer-local 'ess-help-sec-map)

(defvar ess-help-mode-map nil "Keymap for ESS help mode.")
(unless ess-help-mode-map
  (setq ess-help-mode-map (make-keymap)); Full keymap, in order to
  (suppress-keymap ess-help-mode-map)   ; suppress all usual "printing" characters
  (when (boundp 'special-mode-map)
    (set-keymap-parent ess-help-mode-map special-mode-map))
  (define-key ess-help-mode-map "q" 'ess-help-quit)  ;was 'ess-switch-to-end-of-ESS)
  (define-key ess-help-mode-map "\C-m" 'next-line)
  ;; (define-key ess-help-mode-map "s" ess-help-sec-map)
  (define-key ess-help-mode-map "h" 'ess-display-help-on-object)
  (define-key ess-help-mode-map "w" 'ess-display-help-in-browser)
  (define-key ess-help-mode-map "i" 'ess-display-index)
  (define-key ess-help-mode-map "v" 'ess-display-vignettes)
  ;; TODO: `electric mouse-2'
  ;; (define-key ess-help-mode-map [mouse-2] 'ess-display-help-on-object)
  (define-key ess-help-mode-map "l" 'ess-eval-line-and-step)
  (define-key ess-help-mode-map "r" 'ess-eval-region-and-go)
  (define-key ess-help-mode-map "f" 'ess-eval-function-or-paragraph-and-step)
  (define-key ess-help-mode-map "n" 'ess-skip-to-next-section)
  (define-key ess-help-mode-map "p" 'ess-skip-to-previous-section)
  (define-key ess-help-mode-map "/" 'isearch-forward)
  (define-key ess-help-mode-map "x" 'ess-kill-buffer-and-go)
  (define-key ess-help-mode-map "k" 'ess-help-kill)
  (define-key ess-help-mode-map "?" 'ess-describe-help-mode)
  ;;-- those should be "inherited" from ess-mode-map ( ./ess-mode.el )
  (define-key ess-help-mode-map "\C-c\C-s" 'ess-switch-process)
  (define-key ess-help-mode-map "\C-c\C-r" 'ess-eval-region)
  (define-key ess-help-mode-map "\C-c\M-r" 'ess-eval-region-and-go)
  (define-key ess-help-mode-map "\C-c\C-f" 'ess-eval-function)
  (define-key ess-help-mode-map "\M-\C-x"  'ess-eval-function)
  (define-key ess-help-mode-map "\C-c\M-f" 'ess-eval-function-and-go)
  (define-key ess-help-mode-map "\C-c\C-j" 'ess-eval-line)
  (define-key ess-help-mode-map "\C-c\M-j" 'ess-eval-line-and-go)
  (define-key ess-help-mode-map "\M-\C-a"  'ess-beginning-of-function)
  (define-key ess-help-mode-map "\M-\C-e"  'ess-end-of-function)
  (define-key ess-help-mode-map "\C-c\C-y" 'ess-switch-to-ESS)
  (define-key ess-help-mode-map "\C-c\C-z" 'ess-switch-to-end-of-ESS)
  (define-key ess-help-mode-map "\C-c\C-l" 'ess-load-file)
  (define-key ess-help-mode-map "\C-c\C-v" 'ess-display-help-on-object)
  (define-key ess-help-mode-map "\C-c\C-k" 'ess-request-a-process)
  )

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
        ["Index of ..."                 ess-display-index t]
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
        ["Describe ESS-help Mode"       ess-describe-help-mode t]
        "-"
        ["Kill Buffer"                  kill-this-buffer t]
        ["Kill Buffer & Go"             ess-kill-buffer-and-go t]
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
      (toggle-read-only nil)
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
    (unless (string-match "[[:alpha:]]" obj) ;;exclude numbers
      (setq obj nil))
    (list (or (car (member obj slist))
              (car (member fun slist)))
          obj fun)))

;; defunct old name:
(defun ess-read-helpobj-name-default (slist)
  (car (delq nil (ess-helpobjs-at-point slist))))

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
    (read-string (format "%s: " p-string))
    ))

;;*;; Utility functions
(defun ess-get-S-help-topics (&optional name)
  "Return a list of current S help topics associated with process NAME.
If `ess-sp-change' is non-nil or `ess-help-topics-list' is nil, (re)-populate
the latter and return it.  Otherwise, return `ess-help-topics-list'."
  (save-excursion
    (setq name (or name ess-local-process-name))
    (set-buffer (process-buffer (get-ess-process name)))
    (ess-make-buffer-current)
    (ess-write-to-dribble-buffer
     (format "(ess-get-S-help-topics %s) .." name))
    (if (or (not ess-help-topics-list) ess-sp-change)
        (setq ess-help-topics-list
              (ess-uniq-list
               (append (ess-get-object-list name 'exclude-1st)
                       (ess-get-help-files-list)
                       (ess-get-help-aliases-list)
                       )))
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
           'ess-source-directory)
     nil
     (lambda ()
       ;;(goto-char (point-max))
       (rfc822-goto-eoh)
       (forward-line 1)
       (insert "\nThis bug report will be sent to the ESS bugs email list\n")
       (insert "Press C-c C-c when you are ready to send your message.\n\n")
       (insert "\n\n\n")
       (insert-buffer-substring "*ESS*")))))


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
