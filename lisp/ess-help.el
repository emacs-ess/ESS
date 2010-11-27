;;; ess-help.el --- Support for viewing ESS help files

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997, A.J. Rossini <rossini@stat.sc.edu>
;; Copyright (C) 1998--2001 A.J. Rossini, Martin Maechler, Kurt Hornik and
;;	Richard M. Heiberger <rmh@temple.edu>.
;; Copyright (C) 2001--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Created: 7 Jan 1994
;; Maintainers: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
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

(autoload 'ess-eval-region		"ess-inf" "[autoload]" t)
(autoload 'ess-eval-region-and-go	"ess-inf" "[autoload]" t)
(autoload 'ess-eval-function		"ess-inf" "[autoload]" t)
(autoload 'ess-eval-function-and-go	"ess-inf" "[autoload]" t)
(autoload 'ess-eval-line		"ess-inf" "[autoload]" t)
(autoload 'ess-eval-line-and-go		"ess-inf" "[autoload]" t)
(autoload 'ess-eval-line-and-step	"ess-inf" "[autoload]" t)

(autoload 'ess-beginning-of-function	"ess-mode" "[autoload]" t)
(autoload 'ess-end-of-function		"ess-mode" "[autoload]" t)

(autoload 'ess-load-file		"ess-inf" "[autoload]" t)
(autoload 'ess-command			"ess-inf" "(autoload)" nil)
(autoload 'ess-display-temp-buffer	"ess-inf" "(autoload)" nil)
(autoload 'ess-switch-to-ESS		"ess-inf" "(autoload)" nil)
(autoload 'ess-read-object-name-default "ess-inf" "(autoload)" nil)
(autoload 'ess-make-buffer-current	"ess-inf" "(autoload)" nil)
(autoload 'ess-search-list		"ess-inf" "(autoload)" nil)
(autoload 'ess-get-object-list		"ess-inf" "(autoload)" nil)

(autoload 'ess-ddeclient-p		"ess-inf" "(autoload)" nil)

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
  (if (not nr-first) (setq nr-first 120))

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
		  (or  ;; evaluate up to first non-nil (or end):
		   (< (- (point-max) PM) 80); buffer less than 80 chars
		   (not (setq searching t))
		   (progn (goto-char PM) ;; R:
			  (re-search-forward "Error in help"	nr-first t))
		   (progn (goto-char PM) ;; S-plus 5.1 :
			  (re-search-forward "^cat: .*--"	nr-first t))
		   (progn (goto-char PM) ;; S version 3 ; R :
			  (re-search-forward "no documentation" nr-first t))
		   )))
	      )))
    (if debug
	(ess-write-to-dribble-buffer
	 (format " |--> %s [searching %s]\n" res searching)))

    (if (and res return-match searching)
	(list (match-beginning 0) (match-end 0))
      ;; else
      res)))

(defun ess-display-help-on-object (object)
  "Display documentation for OBJECT in another window.
If prefix arg is given, forces a query of the  ESS process for the help
file.  Otherwise just pops to an existing buffer if it exists.
Uses the variable `inferior-ess-help-command' for the actual help command.
Prompts for the object name based on the cursor location for all cases
except the S-Plus GUI.  With S-Plus on Windows (both GUI and in an inferior
emacs buffer) the GUI help window is used."
  (interactive
   (if (ess-ddeclient-p)
       (list (read-string "Help on: "))
     (ess-find-help-file "Help on: ")))

  (if (or (ess-ddeclient-p)
	  (equal inferior-ess-help-filetype "chm"))
      (if (ess-ddeclient-p)
	  (ess-display-help-on-object-ddeclient object) ;; ddeclient version
	(ess-eval-linewise (concat "help(" object ")"))) ;; "chm" version

    ;; else: "normal", non-DDE behavior:
    (let* ((hb-name (concat "*help["
			    ess-current-process-name
			    "](" object ")*"))
	   (old-hb-p	(get-buffer hb-name))
	   (curr-win-mode major-mode)
	   (tbuffer	(get-buffer-create hb-name))
	   (curr-help-command		inferior-ess-help-command)
	   ;;-- pass the buffer-local 'ess-help-sec-..'  to the ess-help buffer:
	   (curr-help-sec-regex		ess-help-sec-regex)
	   (curr-help-sec-keys-alist	ess-help-sec-keys-alist)
	   (curr-help-syntax-table	(syntax-table))
	   (curr-help-filetype	        inferior-ess-help-filetype)
	   (alist		ess-local-customize-alist))

      (set-buffer tbuffer)
      (ess-setq-vars-local (eval alist))
      (setq ess-help-sec-regex	  curr-help-sec-regex)
      (setq ess-help-sec-keys-alist curr-help-sec-keys-alist)
      (setq inferior-ess-help-filetype curr-help-filetype)
      ;; see above, do same for inferior-ess-help-command... (i.e. remove
      ;; hack, restore old code :-).

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
	    (ess-command (format curr-help-command object) tbuffer)
	    ;; was inferior-ess-help-command

	    (ess-help-underline)
	    ;; Stata is clean, so we get a big BARF from this.
	    (if (not (string= ess-language "STA"))
		(ess-nuke-help-bs))

	    (goto-char (point-min))))

      (save-excursion
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
	    ;;dbg	 (format "(ess-help '%s' before switch-to..\n" hb-name)
	    (let ((special-display-regexps
		   (if ess-help-own-frame '(".") nil))
		  (special-display-frame-alist ess-help-frame-alist)
		  (special-display-function
		   (if (eq ess-help-own-frame 'one)
		       'ess-help-own-frame
		     special-display-function)))
	      (if (eq curr-win-mode 'ess-help-mode)
		  (if ess-help-own-frame
		      (pop-to-buffer tbuffer)
		    (switch-to-buffer tbuffer))
		(ess-display-temp-buffer tbuffer)))
	    (if curr-help-syntax-table
		(set-syntax-table curr-help-syntax-table))
	    (set-buffer-modified-p 'nil)
	    (toggle-read-only t)))))))

(defvar ess-help-frame nil
  "Stores the frame used for displaying R help buffers.")

(defun	ess-help-own-frame (buffer &rest ignore)
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
;			       ess-help-w3-url-funs
;			       object
;			       ".html")))
    ;;(w3-fetch-other-window ess-help-url)
;    ))


;;*;; Major mode definition


(defvar ess-help-sec-map nil "Sub-keymap for ESS help mode.")
;; this breaks "s ?" rather than to fix any (unbroken !) thing:
;; (make-variable-buffer-local 'ess-help-sec-map)

(defvar ess-help-mode-map nil "Keymap for ESS help mode.")
(if ess-help-mode-map
    nil
  (setq ess-help-mode-map (make-keymap)); Full keymap, in order to
  (suppress-keymap ess-help-mode-map)	; suppress all usual "printing" characters
  (define-key ess-help-mode-map " " 'scroll-up)
  (define-key ess-help-mode-map "b" 'scroll-down)
  (define-key ess-help-mode-map "\177" 'scroll-down) ; DEL
  (define-key ess-help-mode-map "q" 'ess-switch-to-end-of-ESS)
  (define-key ess-help-mode-map "\C-m" 'next-line)
  ;; (define-key ess-help-mode-map "s" ess-help-sec-map)
  (define-key ess-help-mode-map "h" 'ess-display-help-on-object)
;; TODO: `electric mouse-2'
;; (define-key ess-help-mode-map [mouse-2] 'ess-display-help-on-object)
  (define-key ess-help-mode-map "l" 'ess-eval-line-and-step)
  (define-key ess-help-mode-map "r" 'ess-eval-region-and-go)
  (define-key ess-help-mode-map "f" 'ess-eval-function-or-paragraph-and-step)
  (define-key ess-help-mode-map "n" 'ess-skip-to-next-section)
  (define-key ess-help-mode-map "p" 'ess-skip-to-previous-section)
  (define-key ess-help-mode-map "/" 'isearch-forward)
  (define-key ess-help-mode-map ">" 'end-of-buffer)
  (define-key ess-help-mode-map "<" 'beginning-of-buffer)
  (define-key ess-help-mode-map "x" 'ess-kill-buffer-and-go)
  (define-key ess-help-mode-map "k" 'kill-buffer)
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
  (define-key ess-help-mode-map "\C-c\C-k" 'ess-request-a-process))

;; One reason for the following menu is to <TEACH> the user about key strokes
(defvar ess-help-mode-menu
  (list "ESS-help"
	["Search Forward"		isearch-forward t]
	["Next Section"			ess-skip-to-next-section t]
	["Previous Section"		ess-skip-to-previous-section t]
	["Help on Section Skipping"	ess-describe-sec-map t]
	["Beginning of Buffer"		beginning-of-buffer t]
	["End of Buffer"		end-of-buffer t]
	"-"
	["Help on ..."			ess-display-help-on-object t]
	"-"
	["Eval Line"			ess-eval-line-and-step t]
	["Eval Paragraph & step" 	ess-eval-paragraph-and-step t]
	["Eval Region & Go"		ess-eval-region-and-go t]
	["Switch to ESS Process"	ess-switch-to-ESS t]
	["Switch to End of ESS Proc."	ess-switch-to-end-of-ESS t]
	["Switch _the_ Process"		ess-switch-process t]
	"-"
	["Describe ESS-help Mode"	ess-describe-help-mode t]
	"-"
	["Kill Buffer"			kill-buffer t]
	["Kill Buffer & Go"		ess-kill-buffer-and-go t]
	)
  "Menu used in ess-help mode.")


(defun ess-help-mode ()
;;; Largely ripped from more-mode.el,
;;; originally by Wolfgang Rupprecht wolfgang@mgm.mit.edu
  "Mode for viewing ESS help files.
Use SPC and DEL to page back and forth through the file.
Use `n'	 and `p' to move to next and previous section,
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
  (mapc '(lambda (key)
	   (define-key ess-help-sec-map (char-to-string key)
	     'ess-skip-to-help-section))
	(mapcar 'car ess-help-sec-keys-alist))
  (define-key ess-help-sec-map "?" 'ess-describe-sec-map)
  (define-key ess-help-sec-map ">" 'end-of-buffer)
  (define-key ess-help-sec-map "<" 'beginning-of-buffer)
  (define-key ess-help-mode-map "s" ess-help-sec-map)

  (run-hooks 'ess-help-mode-hook))

;;*;; User commands defined in ESS help mode

(defun ess-skip-to-help-section nil
  "Jump to a section heading of a help buffer.	The section selected
is determined by the command letter used to invoke the command, as
indicated by `ess-help-sec-keys-alist'.	 Use \\[ess-describe-sec-map]
to see which keystrokes find which sections."
  (interactive)
  (let ((old-point (point))
	(case-fold-search nil))
    (goto-char (point-min))
    (let ((the-sec (cdr (assoc last-command-event
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
      (mapc '(lambda (cs) (insert "    "
				  (car cs)
				  "      "
				  (cdr cs) "\n"))
	    keys-alist)
      (insert "\nFull list of key definitions:\n"
	      (substitute-command-keys
	       "\\{ess-help-sec-map}")))))

(defun ess-read-helpobj-name-default (olist)
  ;;; Returns the object name at point, or else the name of the
  ;;; function call point is in if that has a help file. A name has a
  ;;; help file if it is a member of olist.
  (or (car (assoc (ess-read-object-name-default) olist))
      (condition-case ()
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (max (point-min) (- (point) 1000))
				(point-max))
	      (backward-up-list 1)
	      (backward-char 1)
	      (car (assoc (ess-read-object-name-default) olist))))
	(error nil))))

(defun ess-find-help-file (p-string)
  "Find help, prompting for P-STRING.  Note that we can't search SAS,
Stata or XLispStat for additional information."
  (ess-make-buffer-current)
  (if (not
       (or
	(string-match "XLS" ess-language)
	(string-match "STA" ess-language)
	(string-match "SAS" ess-language)))
      (let* ((help-files-list
	      (ess-get-help-topics-list ess-current-process-name))
	     (default (ess-read-helpobj-name-default help-files-list))
	     (prompt-string (if default
				(format "%s(default %s) " p-string default)
			      p-string))
	     (spec (completing-read prompt-string help-files-list)))
	(list (cond
	       ((string= spec "") default)
	       (t spec))))
    (let* ((spec (read-string p-string)))
      (list spec))))


;;*;; Utility functions

(defun ess-get-help-topics-list (name)
  "Return a list of current S help topics associated with process NAME.
If `ess-sp-change' is non-nil or `ess-help-topics-list' is nil, (re)-populate
the latter and return it.  Otherwise, return `ess-help-topics-list'."
  (save-excursion
    (set-buffer (process-buffer (get-ess-process name)))
    (ess-make-buffer-current)
    (ess-write-to-dribble-buffer
     (format "(ess-get-help-topics-list %s) .." name))
    (if (or (not ess-help-topics-list) ess-sp-change)
	(setq ess-help-topics-list
	      (ess-uniq-list
	       (mapcar 'list
		       (append (ess-get-help-files-list)
			       (ess-get-help-aliases-list)
			       (ess-get-object-list name)))))
      ;; else return the existing list
      ess-help-topics-list)))

(defun ess-get-help-files-list ()
  "Return a list of files which have help available."
  (apply 'append
	 (mapcar '(lambda (dirname)
		    (if (file-directory-p dirname)
			(directory-files dirname)))
		 (mapcar '(lambda (str) (concat str "/.Help"))
			 (ess-search-list)))))

(defun ess-get-help-aliases-list ()
  "Return a list of aliases which have help available."
  (apply 'append
	 (mapcar '(lambda (a-file)
		    (if (file-exists-p a-file)
			(ess-get-words-from-vector
			 (format
			  "names(.readRDS(\"%s\"))\n" a-file))))
		 (mapcar '(lambda (str) (concat str "/help/aliases.rds"))
			 (ess-get-words-from-vector
			  "searchpaths()\n")))))

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
     (concat "ess-mode " ess-version)
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
;;; Chapters:	  ^L ;
;;; Sections:	 ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:	 defuns, defvars, defconsts
;;;		 Random code beginning with a ;;;;* comment

;;; Local variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-help.el ends here
