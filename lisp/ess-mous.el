;;; ess-mous.el --- Support for mouse- or cursor-sensitive actions

;; Copyright (C) 2001 Richard M. Heiberger <rmh@sbm.temple.edu>

;; Author: Richard M. Heiberger <rmh@sbm.temple.edu>
;; Maintainer: Richard M. Heiberger <rmh@sbm.temple.edu>
;; Created: 25 Mar 2001
;; Modified: $Date: 2001/05/23 22:10:19 $
;; Version: $Revision: 
;; RCS: $Id: ess-mous.el

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

;; Support for mouse- or cursor-sensitive actions.  This is based on
;; and uses mouseme.el.  mouseme.el only does mouse sensititivity.
;; The new functions ess-mouse-me and ess-mouse-me-helper do similar
;; things based on the cursor, not the mouse, and can be bound to a
;; keystroke.

;;; Code:

 ; Requires and autoloads

;;*;; Requires
(require 'mouseme)




;;; Instructions:

;;From: Rich Heiberger <rmh@surfer.sbm.temple.edu>
;;Subject: Re: ess-send and ess-send2... which is right?
;;To: Rich Heiberger <rmh@surfer.sbm.temple.edu>, rossini@u.washington.edu
;;Date: Wed, 23 May 2001 17:30:42 -0400 (EDT)

;;as i recall, both are preliminary and the real version is the one that'
;;is in the cvs tree as ess-mous.el.  I checked ess-mous a few days ago and
;;it works as long as you individually and manually execute the lines
;;(make-variable-buffer-local 'mouse-me-menu-commands)
;;(setq mouse-me-menu-commands ess-S-mouse-me-menu-commands-alist)
;;for each ess buffer.  That needs to be turned on permanently and globally
;;(within all ESS buffers).  And the ess-mous needs to be required in ess-site.

;;get back to me after you try it out.  I can respond in the next few days.

;;rich

(if (not ess-running-xemacs)
    (defun ess-mouse-me ()
      "Popup a menu of functions to run on selected string or region."
      (interactive)
      (ess-mouse-me-helper
       #'(lambda ()
	   (or (x-popup-menu (list '(0 0) 
				   (get-buffer-window (get-buffer (buffer-name))))
			     (funcall mouse-me-build-menu-function name))
	       (error "No command to run")))))
  (defun ess-mouse-me ()
      "Popup a menu of functions to run on selected string or region."
      (interactive)
      (ess-mouse-me-helper
       #'(lambda ()
	   (or (x-popup-menu (list '(0 0) 
				   (get-buffer-window (get-buffer (buffer-name))))
			     (funcall mouse-me-build-menu-function name))
	       (error "No command to run"))))))

    
  
(defun ess-mouse-me-helper (func)
  "Determine the string to use to process EVENT and call FUNC to get cmd."
  (let (name sp sm mouse beg end cmd mmtype)
    ;; temporarily goto where the event occurred, get the name clicked
    ;; on and enough info to figure out what to do with it
    (save-match-data
      (save-excursion
        (setq sp (point))               ; saved point
        (setq sm (mark t))              ; saved mark
;;;     (set-buffer (window-buffer (posn-window (event-start event))))
;;;     (setq mouse (goto-char (posn-point (event-start event))))
	(setq mouse (point))  ;; ess-mouse-me-helper
        ;; if there is a region and point is inside it
        ;; check for sm first incase (null (mark t))
        ;; set name to either the thing they clicked on or region
        (if (and sm
                 (or (and transient-mark-mode mark-active)
                     (eq last-command 'mouse-drag-region))
                 (>= mouse (setq beg (min sp sm)))
                 (<= mouse (setq end (max sp sm))))
            (setq name (buffer-substring beg end))
          (setq name (funcall mouse-me-get-string-function))
          (if (listp name)
              (setq beg (nth 1 name)
                    end (nth 2 name)
                    name (car name))
            (goto-char mouse)
            (while (not (looking-at (regexp-quote name)))
              (backward-char 1))
            (setq beg (point))
            (setq end (search-forward name))))))
    ;; check if name is null, meaning they clicked on no word
    (if (or (null name)
            (and (stringp name) (string= name "" )))
        (error "No string to pass to function"))
    ;; popup a menu to get a command to run
    (setq cmd (funcall func))
    ;; run the command, eval'ing if it was a list
    (if (listp cmd)
        (setq cmd (eval cmd)))
    (setq mmtype (get cmd 'mouse-me-type))
    (cond ((eq mmtype 'region)
           (funcall cmd beg end))
          ((eq mmtype 'string)
           (funcall cmd name))
          (t
           (funcall cmd name)))))

(defcustom ess-S-mouse-me-menu-commands-alist
  '(("print"       . ess-mouse-me-print)
    ("summary"     . ess-mouse-me-summary)
    ("show"        . ess-mouse-me-show)
    ("help"        . ess-mouse-me-help)
    ("Edit.data"   . ess-mouse-me-Edit.data)
    ("args"        . ess-mouse-me-args)
    "----"
    ("Browser on"  . ess-mouse-me-browser-on)
    ("Browser off" . ess-mouse-me-browser-off))
    "*Command menu used by `mouse-me-build-menu'.
A alist of elements where each element is either a cons cell or a string.
If a cons cell the car is a string to be displayed in the menu and the
cdr is either a function to call passing a string to, or a list which evals
to a function to call passing a string to.  If the element is a string
it makes a non-selectable element in the menu.  To make a separator line
use a string consisting solely of hyphens.

The function returned from this menu will be called with one string
argument.  Or if the function has the symbol property `mouse-me-type'
and if its value is the symbol `region' it will be called with the
beginning and ending points of the selected string.  If the value is
the symbol `string' it will be called with one string argument."
  :type '(repeat sexp)
  :group 'mouseme)

(defun ess-mouse-me-eval-expanded (string &optional head tail commands-buffer)
  "Send the expanded STRING to the inferior-ess
process after first concating the head and tail."
  (interactive)
  (if (not head) (setq head "summary("))
  (if (not tail) (setq tail ")"))
  (if (not commands-buffer) (setq commands-buffer
				  (get-buffer-create "tmp-buffer")))
  (ess-command (concat head string tail)
	       commands-buffer))


(defun ess-mouse-me-print (string)
  (ess-mouse-me-eval-expanded string "print(" ")"))
(defun ess-mouse-me-summary (string)
  (ess-mouse-me-eval-expanded string "summary(" ")"))
(defun ess-mouse-me-show (string)
  (ess-mouse-me-eval-expanded string "show(" ")"))
(defun ess-mouse-me-help (string)
  (ess-mouse-me-eval-expanded string "help(" ")"))
(defun ess-mouse-me-Edit.data (string)
  (ess-mouse-me-eval-expanded string "Edit.data(" ")"))
(defun ess-mouse-me-args (string)
  (ess-mouse-me-eval-expanded string "args(" ")"))

(defun ess-mouse-me-browser-on (string)
  (ess-mouse-me-eval-expanded string "trace(" ", exit=browser)"))
(defun ess-mouse-me-browser-off  (string)
  (ess-mouse-me-eval-expanded string "untrace(" ")"))


 ; Provide package

(provide 'ess-mous)



;;;;;;;; STARTUP STUFF ;;;;;;;;;;;;

;;; place this in a customize-alist, do it manually in each ESS buffer
;;; while we are designing this function.
(make-variable-buffer-local 'mouse-me-menu-commands)
(setq mouse-me-menu-commands ess-S-mouse-me-menu-commands-alist)
;;(mouse-me-build-menu "ESS")  ;; this seems not to be needed.


(if (not ess-running-xemacs)
	 (global-set-key [S-mouse-2] 'mouse-me)
  (global-set-key [(shift button2)] 'mouse-me))

(define-key ess-mode-map "\C-c\C-w" 'ess-mouse-me)


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
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-mous.el ends here
