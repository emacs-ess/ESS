;;; ess-help.el --- Support for viewing ESS help files

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997, A.J. Rossini <rossini@stat.sc.edu>

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 7 Jan 1994
;; Modified: $Date: 1997/12/02 14:09:55 $
;; Version: $Revision: 5.1 $
;; RCS: $Id: ess-help.el,v 5.1 1997/12/02 14:09:55 rossini Exp $

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
;; <LANGUAGE> is one of `S', `SAS', or `XLispStat'.

;;; Code:

 ; Requires and autoloads

(eval-when-compile
  (require 'reporter)
  (require 'ess-inf)
  (require 'info))

(require 'ess)

(autoload 'ess-eval-region "ess-mode" "[autoload]" t)
(autoload 'ess-eval-region-and-go "ess-mode" "[autoload]" t)
(autoload 'ess-eval-function "ess-mode" "[autoload]" t)
(autoload 'ess-eval-function-and-go "ess-mode" "[autoload]" t)
(autoload 'ess-eval-line "ess-mode" "[autoload]" t)
(autoload 'ess-eval-line-and-go "ess-mode" "[autoload]" t)
(autoload 'ess-eval-line-and-next-line "ess-mode" "[autoload]" t)
(autoload 'ess-beginning-of-function "ess-mode" "[autoload]" t)
(autoload 'ess-end-of-function "ess-mode" "[autoload]" t)

(autoload 'ess-load-file "ess-inf" "[autoload]" t)
(autoload 'ess-command "ess-inf" "(autoload)" nil)
(autoload 'ess-display-temp-buffer "ess-inf" "(autoload)" nil)
(autoload 'ess-switch-to-ESS "ess-inf" "(autoload)" nil)
(autoload 'ess-read-object-name-default "ess-inf" "(autoload)" nil)
(autoload 'ess-make-buffer-current "ess-inf" "(autoload)" nil)
(autoload 'ess-search-list "ess-inf" "(autoload)" nil)
(autoload 'ess-get-object-list "ess-inf" "(autoload)" nil)



 ; ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The function ess-display-help-on-object
;;;; * The major mode ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*;; Access function for displaying help

(defun ess-display-help-on-object (object)
  "Display the ESS documentation for OBJECT in another window.
If prefix arg is given, forces a query of the ESS process for the help
file.  Otherwise just pops to an existing buffer if it exists."
  (interactive (ess-find-help-file "Help on: "))
  (let* ((hb-name (concat "*help["
			  ess-current-process-name
			  "]("
			  object
			  ")*"))
	 (old-hb-p (get-buffer hb-name))
	 (curr-win-mode major-mode)
	 (tbuffer (get-buffer-create hb-name))
	 (curr-help-command inferior-ess-help-command)
	 ;;-- pass the buffer-local 'ess-help-sec-..'  to the ess-help buffer:
	 (curr-help-sec-regex      ess-help-sec-regex)
	 (curr-help-sec-keys-alist ess-help-sec-keys-alist)
	 (alist ess-local-customize-alist))

    (set-buffer tbuffer)
    (ess-setq-vars-local (eval alist) (current-buffer))
    (setq ess-help-sec-regex      curr-help-sec-regex)
    (setq ess-help-sec-keys-alist curr-help-sec-keys-alist)
    ;; see above, do same for inferior-ess-help-command... (i.e. remove
    ;; hack, restore old code :-).

    (if (or (not old-hb-p) current-prefix-arg)
	;; Ask ESS for the help file
	(progn
	  (delete-region (point-min) (point-max))
	  (ess-help-mode)
	  (setq ess-local-process-name ess-current-process-name)
	  (ess-command (format curr-help-command object) tbuffer) ;; was
	  ;; inferior-ess-help-command
	  (ess-nuke-help-bs)
	  (goto-char (point-min))))
    (let (nodocs)
      (save-excursion
	(goto-char (point-min))
	(setq nodocs
	      (re-search-forward
	       "\\`No documentation available.*$"
	       nil t))
	(if nodocs
	    (progn
	      (princ (buffer-substring (match-beginning 0)
				       (match-end 0)) t)
	      ;; Avoid using 'message here -- may be %'s in string
	      (ding)
	      (kill-buffer tbuffer))
	  (if (eq curr-win-mode 'ess-help-mode)
	      (switch-to-buffer tbuffer)
	    (ess-display-temp-buffer tbuffer))
	  (set-buffer-modified-p 'nil)
	  (toggle-read-only t))))))


;;; THIS WORKS!
;;(require 'w3)
;;(defun ess-display-w3-help-on-object-other-window (object)
;;  "Display R-documentation for OBJECT using W3"
;;  (interactive "s Help on :")
;;  (let* ((ess-help-url (concat ess-help-w3-url-prefix
;;			   ess-help-w3-url-funs
;;			   object
;;			   ".html")))
;;    (w3-fetch-other-window ess-help-url)))


;;*;; Major mode definition


(defvar ess-help-sec-map nil "Sub-keymap for ESS help mode.")
(if ess-help-sec-map
    nil
  (setq ess-help-sec-map (make-sparse-keymap))
  (mapcar '(lambda (key)
	    (define-key ess-help-sec-map (char-to-string key)
	      'ess-skip-to-help-section))
	    (mapcar 'car ess-help-sec-keys-alist))
  (define-key ess-help-sec-map "?" 'ess-describe-sec-map)
  (define-key ess-help-sec-map ">" 'end-of-buffer)
  (define-key ess-help-sec-map "<" 'beginning-of-buffer)
)

(defvar ess-help-mode-map nil "Keymap for ESS help mode.")
(if ess-help-mode-map
    nil
  (setq ess-help-mode-map (make-keymap)); Full keymap, in order to
  (suppress-keymap ess-help-mode-map)   ; suppress all usual "printing" characters
  (define-key ess-help-mode-map " " 'scroll-up)
  (define-key ess-help-mode-map "b" 'scroll-down)
  (define-key ess-help-mode-map "q" 'ess-switch-to-end-of-ESS)
  (define-key ess-help-mode-map "\C-m" 'next-line)
  (define-key ess-help-mode-map "\177" 'scroll-down) ; DEL
  (define-key ess-help-mode-map "s" ess-help-sec-map)
  (define-key ess-help-mode-map "h" 'ess-display-help-on-object)
  (define-key ess-help-mode-map "l" 'ess-eval-line-and-next-line)
  (define-key ess-help-mode-map "r" 'ess-eval-region-and-go)
  (define-key ess-help-mode-map "n" 'ess-skip-to-next-section)
  (define-key ess-help-mode-map "p" 'ess-skip-to-previous-section)
  (define-key ess-help-mode-map "/" 'isearch-forward)
  (define-key ess-help-mode-map ">" 'end-of-buffer)
  (define-key ess-help-mode-map "<" 'beginning-of-buffer)
  (define-key ess-help-mode-map "x" 'ess-kill-buffer-and-go)
  (define-key ess-help-mode-map "k" 'kill-buffer)
  (define-key ess-help-mode-map "?" 'ess-describe-help-mode)
  ;;-- those should be "inherited" from ess-mode-map :
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
  (make-local-variable 'ess-local-process-name)

  ;;; Keep <tabs> out of the code.
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  (run-hooks ess-help-mode-hook))

;;*;; User commands defined in ESS help mode

(defun ess-skip-to-help-section nil
  "Jump to a section heading of a help buffer. The section selected is
determined by the command letter used to invoke the command, as
indicated by ess-help-sec-keys-alist. Use \\[ess-describe-sec-map] to see
which keystrokes find which sections."
  (interactive)
  (let ((old-point (point))
	(case-fold-search nil))
    (goto-char (point-min))
    (let ((the-sec (cdr (assoc last-command-char
			       ess-help-sec-keys-alist))))
      (if (not the-sec) (error "Invalid section key: %c"
			       last-command-char)
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
"Display help for ess-mode"
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
  (describe-function 'ess-skip-to-help-section)
  (save-excursion
    (set-buffer "*Help*")
    (goto-char (point-max))
    (insert "\n\nCurrently defined keys are:

Keystroke    Section
---------    -------\n")
    (mapcar '(lambda (cs) (insert "    "
				  (car cs)
				  "        "
				  (cdr cs) "\n"))
	    ess-help-sec-keys-alist)
    (insert "\nFull list of key definitions:\n"
	    (substitute-command-keys
	     "\\{ess-help-sec-map}"))))

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
  "Find help, prompting for p-string.  Note that we can't search SAS
or XLispStat for additional information."
  (ess-make-buffer-current)
  (if (not
       (or
	(string-match "XLS" ess-language)
	(string-match "SAS" ess-language)))
      (let* ((help-files-list (or (ess-get-help-files-list)
				  (mapcar 'list
					  (ess-get-object-list
					   ess-current-process-name))))
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

(defun ess-get-help-files-list ()
  "Return a list of files which have available help."
  (mapcar 'list
	  (apply 'append
		 (mapcar '(lambda (dirname)
			    (if (file-directory-p dirname)
				(directory-files dirname)))
			 (mapcar '(lambda (str) (concat str "/.Help"))
				 (ess-search-list))))))

(defun ess-nuke-help-bs ()
  (interactive "*")
;;; This function is a modification of nuke-nroff-bs in man.el from the
;;; standard emacs 18 lisp library.
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
     "ess-bugs@stat.math.ethz.ch"
     (concat "ess-mode " ess-version)
     (list 'ess-language
	   'ess-dialect
	   'ess-ask-for-ess-directory
	   'ess-ask-about-transfile
	   'ess-directory
	   'ess-keep-dump-files
	   'ess-source-directory)
     nil
     (lambda () (goto-char (point-max)) (insert-buffer "*ESS*")))))


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
