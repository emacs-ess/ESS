;;; ess-help.el --- Support for viewing S help files

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Maintainer: Anthony Rossini <rossini@stat.sc.edu>
;; Created: 7 Jan 1994
;; Modified: $Date: 1997/08/26 22:54:46 $
;; Version: $Revision: 1.5 $
;; RCS: $Id: ess-help.el,v 1.5 1997/08/26 22:54:46 rossini Exp $

;; This file is part of ess-mode

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

;; Code for dealing with S help files. See S.el for more
;; details.

;;
;; $Log: ess-help.el,v $
;; Revision 1.5  1997/08/26 22:54:46  rossini
;; *** empty log message ***
;;
;; Revision 1.4  1997/07/24 11:21:22  rossini
;; ess-mode-version -> ESS-version
;;
;; Revision 1.3  1997/06/15 21:56:38  rossini
;; *** empty log message ***
;;
;; Revision 1.2  1997/05/21 18:45:46  rossini
;; S -> ess
;;
;; Revision 1.1  1997/05/21 18:40:53  rossini
;; Initial revision
;;
;; Revision 1.19  1997/04/24 23:13:47  rossini
;; added MM's change.
;;
;; Revision 1.18  1997/04/24 22:11:42  rossini
;; change for keymap from Martin M.
;;
;; Revision 1.17  1997/04/23 13:33:01  rossini
;; added Martin's patch.
;;
;; Revision 1.16  1997/04/23 03:21:05  rossini
;; Looks good...?
;;
;; Revision 1.15  1997/04/16 19:00:46  rossini
;; fixed byte compile problem.
;;
;; Revision 1.14  1997/04/14 00:50:03  rossini
;; added prompt for bug reports. (thanks, MM).
;;
;; Revision 1.13  1997/04/08 01:07:01  rossini
;; removed variables (moved to S.el)
;;
;; Revision 1.12  1997/04/04 18:38:56  rossini
;; GNU Emacs didn't like (require 'S-inf).  Why?
;;
;; Revision 1.11  1997/04/03 23:59:57  rossini
;; w3 fetch works, postponed until 5.0
;;
;; Revision 1.10  1997/04/03 23:26:53  rossini
;; added preliminary w3 functions for R help.  Not linked anywhere.
;;
;; Revision 1.9  1997/04/03 22:56:08  rossini
;; changed reporter to send to rossini.
;;
;; Revision 1.8  1997/04/02 15:35:45  rossini
;; no real changes.
;;
;; Revision 1.7  1997/03/07 23:36:51  rossini
;; We did NOT need to (require 'S-mode, 'S-inf...).
;;
;;

;;; Code:

 ; Requires and autoloads

(eval-when-compile
  (require 'reporter))

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
(autoload 'ess-load-file "ess-mode" "[autoload]" t)

(autoload 'ess-command "ess-inf" "(autoload)" nil)
(autoload 'ess-display-temp-buffer "ess-inf" "(autoload)" nil)
(autoload 'ess-switch-to-S "ess-inf" "(autoload)" nil)
(autoload 'ess-read-object-name-default "ess-inf" "(autoload)" nil)
(autoload 'ess-make-buffer-current "ess-inf" "(autoload)" nil)
(autoload 'ess-search-list "ess-inf" "(autoload)" nil)



 ; ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The function ess-display-help-on-object
;;;; * The major mode ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*;; Access function for displaying help

(defun ess-display-help-on-object (object)
  "Display the S documentation for OBJECT in another window.
If prefix arg is given, forces a query of the S process for the help
file.  Otherwise just pops to an existing buffer if it exists."
  (interactive (ess-find-help-file "Help on: "))
  (let* ((hb-name (concat "*help(" object ")*"))
	 (old-hb-p (get-buffer hb-name))
	 (curr-win-mode major-mode)
	 (tbuffer (get-buffer-create hb-name))
	 (curr-help-command inferior-ess-help-command)
	 ;;-- pass the buffer-local 'ess-help-sec-..'  to the ess-help buffer:
	 (curr-help-sec-regex      ess-help-sec-regex)
	 (curr-help-sec-keys-alist ess-help-sec-keys-alist)
	 )
    (set-buffer tbuffer)
    (setq ess-help-sec-regex      curr-help-sec-regex)
    (setq ess-help-sec-keys-alist curr-help-sec-keys-alist)
    ;; see above, do same for inferior-ess-help-command... (i.e. remove
    ;; hack, restore old code :-).

    (if (or (not old-hb-p) current-prefix-arg)
	;; Ask S for the help file
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
	  (if (eq curr-win-mode 'ess-help-mode) (switch-to-buffer tbuffer)
	    (ess-display-temp-buffer tbuffer)))))))


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


(defvar ess-help-sec-map nil "Sub-keymap for S help mode.")
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

(defvar ess-help-mode-map nil "Keymap for S help mode.")
(if ess-help-mode-map
    nil
  (setq ess-help-mode-map (make-keymap)); Full keymap, in order to
  (suppress-keymap ess-help-mode-map)   ; suppress all usual "printing" characters
  (define-key ess-help-mode-map " " 'scroll-up)
  (define-key ess-help-mode-map "b" 'scroll-down)
  (define-key ess-help-mode-map "q" 'ess-switch-to-end-of-S)
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
  (define-key ess-help-mode-map "\C-c\C-r" 'ess-eval-region)
  (define-key ess-help-mode-map "\C-c\M-r" 'ess-eval-region-and-go)
  (define-key ess-help-mode-map "\C-c\C-f" 'ess-eval-function)
  (define-key ess-help-mode-map "\M-\C-x"  'ess-eval-function)
  (define-key ess-help-mode-map "\C-c\M-f" 'ess-eval-function-and-go)
  (define-key ess-help-mode-map "\C-c\C-j" 'ess-eval-line)
  (define-key ess-help-mode-map "\C-c\M-j" 'ess-eval-line-and-go)
  (define-key ess-help-mode-map "\M-\C-a"  'ess-beginning-of-function)
  (define-key ess-help-mode-map "\M-\C-e"  'ess-end-of-function)
  (define-key ess-help-mode-map "\C-c\C-y" 'ess-switch-to-S)
  (define-key ess-help-mode-map "\C-c\C-z" 'ess-switch-to-end-of-S)
  (define-key ess-help-mode-map "\C-c\C-l" 'ess-load-file)
  (define-key ess-help-mode-map "\C-c\C-v" 'ess-display-help-on-object)
  (define-key ess-help-mode-map "\C-c\C-k" 'ess-request-a-process))

(defun ess-help-mode ()
;;; Largely ripped from more-mode.el,
;;; originally by Wolfgang Rupprecht wolfgang@mgm.mit.edu
  "Mode for viewing S help files.
Use SPC and DEL to page back and forth through the file.
Use `n'  and `p' to move to next and previous section,
    `s' to jump to a particular section;   `s ?' for help.
Use `q' to return to your S session; `x' to kill this buffer first.
The usual commands for evaluating S source are available.
Other keybindings are as follows:
\\{ess-help-mode-map}"
  (interactive)
  (setq major-mode 'ess-help-mode)
  (setq mode-name "ESS Help")
  (use-local-map ess-help-mode-map)
  (make-local-variable 'ess-local-process-name)
  (run-hooks ess-help-mode-hook))

;;*;; User commands defined in S help mode

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
  "Jump to next section in S help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (if (re-search-forward ess-help-sec-regex nil 'no-error) nil
      (message "No more sections."))))

(defun ess-skip-to-previous-section nil
  "Jump to previous section in S help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (if (re-search-backward ess-help-sec-regex nil 'no-error) nil
      (message "No previous section."))))

(defun ess-describe-help-mode nil
"Display help for ess-mode"
 (interactive)
 (describe-function 'ess-help-mode))

(defun ess-kill-buffer-and-go nil
  "Kill the current buffer and switch back to S"
  (interactive)
  (kill-buffer (current-buffer))
  (ess-switch-to-S nil))

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
  (ess-make-buffer-current)
  (let* ((help-files-list  (or (ess-get-help-files-list)
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
           (t spec)))))

;;*;; Utility functions

(defun ess-get-help-files-list nil
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
  (other-window 1)
  (Info-goto-node (concat "(ess-mode)" node)))

 ; Bug Reporting

(defun ess-submit-bug-report ()
  "Submit a bug report on the ess-mode package."
  (interactive)
  (require 'ess-mode)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p 't))
    (reporter-submit-bug-report
     "rossini@stat.sc.edu"
     (concat "ess-mode " ess-version)
     (list 'ess-program-name
	   'ess-language
	   'ess-dialect
	   'ess-ask-for-ess-directory
	   'ess-ask-about-transfile
	   'ess-directory
	   'ess-keep-dump-files
	   'ess-source-directory))))


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
