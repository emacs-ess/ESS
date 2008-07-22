;;; ess.el --- Emacs Speaks Statistics: statistical programming within Emacs

;; Copyright (C) 1989--1996 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997--2005 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Authors: Doug Bates, Ed Kademan, Frank Ritter, David Smith
;; Created: October 14, 1991
;; Maintainers: ESS-core <ESS-core@stat.math.ethz.ch>

;; Keywords: statistical support
;; Summary: general functions for ESS

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; In short: you may use this code any way you like, as long as you
;; don't charge money for it, remove this notice, or hold anyone liable
;; for its results.

;;; Commentary:

;;; PURPOSE
;;;
;;; Interface to the S, SAS, and XLisp dialects of statistical
;;; programming languages, with potential extensions to other
;;; languages.	Designed to be extendable to most other interactive
;;; statistical programming situations.

;;; BRIEF OVERVIEW
;;;
;;; Supports structured editing of S, SAS, and XLisp (statistics
;;; programming languages) functions that are integrated with a
;;; running process in a buffer.

;;; THE ESS MAILING LIST
;;;
;;; There is an informal mailing list for discussions of ESS. Alpha
;;; and beta releases of ESS are also announced here. Send mail
;;; to ess-request@stat.math.ethz.ch to join.

;;; OVERVIEW OF ESS
;;;
;;; S is a statistics programming language developed at Bell Labs
;;; particularly suited for descriptive and exploratory statistics.
;;; s-mode is built on top of comint (the general command interpreter
;;; mode written by Olin Shivers), and so comint.el (or comint.elc)
;;; should be either loaded or in your load path when you invoke it.
;;;
;;; Aside from the general features offered by comint such as
;;; command history editing and job control, inferior S mode
;;; allows you to dump and load S objects into and from external
;;; files, and to display help on functions.  It also provides
;;; name completion while you do these.	 For more detailed
;;; information see the documentation strings for inferior-ess,
;;; inferior-ess-mode, ess-mode, and comint-mode.  There are also
;;; many variables and hooks available for customizing (see
;;; the variables below that have document strings that start
;;; with an "*").

;;; INSTALLATION
;;; See README and S-site for details.

;;; GETTING RELEASES OF ESS
;;; ===> http://ess.r-project.org
;;;

;;; CREDITS.
;;; Thanks to shiba@shun.isac.co.jp (Ken'ichi "Modal" Shibayama) for
;;;   the indenting code.
;;; Thanks also to maechler@stat.math.ethz.ch (Martin Maechler) for
;;;   suggestions and bug fixes.
;;; ess-eval-line-and-step is based on a function by Rod Ball
;;;   (rod@marcam.dsir.govt.nz)
;;; Also thanks from David Smith to the previous authors for all their
;;; help and suggestions.
;;; And thanks from Richard M. Heiberger, Kurt Hornik, Martin
;;; Maechler, and A.J. Rossini to David Smith.

;;; BUG REPORTS
;;; Please report bugs to ess-bugs@stat.math.ethz.ch
;;; Comments, suggestions, words of praise and large cash donations
;;; are also more than welcome, but should generally be split between
;;; all authors :-).

;;; Code:

;;*;; Requires and autoloads
;;;=====================================================
;;;

(require 'easymenu)
(if (or window-system
	noninteractive ; compilation!
	)
    (require 'font-lock))

(require 'ess-emcs)

(eval-and-compile
  (require 'ess-cust))

 ; ess-mode: editing S/R/XLS/SAS source

(autoload 'inferior-ess "ess-inf"
  "Run [inferior-ess-program], an ess process, in an Emacs buffer" t)

(autoload 'ess-dump-object-into-edit-buffer "ess-mode"
  "Edit an S object." t)

(autoload 'ess-parse-errors "ess-mode"
  "Jump to the last error generated from a sourced file." t)

(autoload 'ess-load-file "ess-inf" "Source a file into S.")

(autoload 'ess-inside-string-or-comment-p "ess-utils"
  "Non-nil, if inside string or comment." t)
(autoload 'ess-inside-string-p "ess-utils" "Non-nil if inside string." t)

(autoload 'ess-do-auto-fill "ess-utils" "substitute for \\[do-auto-fill]." t)

(autoload 'ess-rep-regexp "ess-utils" "Replace, but not in string/comment." t)

(autoload 'ess-time-string "ess-utils" "Return time-stamp string." t)

(autoload 'ess-nuke-trailing-whitespace "ess-utils"
  "Maybe get rid of trailing blanks." t)

 ; ess-transcript-mode: editing ``outputs'

(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files." t)

(autoload 'ess-display-help-on-object "ess-help"
  "Display help on an S object." t)

(defalias 'ess-help 'ess-display-help-on-object)

(autoload 'ess-goto-info "ess-help"
  "Jump to the relevant section in the `ess-mode' manual." t)

(autoload 'ess-submit-bug-report "ess-help"
  "Submit a bug report on the `ess-mode' package." t)

;;==> ess-inf.el  has its OWN autoload's !


 ; Set up for menus, if necessary
;;;
;;;  --> is done in ess-mode.el, ess-inf.el, etc


;;; Completion and Database code

(defun ess-load-object-name-db-file ()
  "Load object database file if present, mention if not."
  (if (string= ess-language "S")
      (progn
	(make-local-variable 'ess-object-name-db)
	(condition-case ()
	    (load ess-object-name-db-file)
	  (error
	   ;;(message "%s does not exist.  Consider running ess-create-object-name-db."
		;;    ess-object-name-db-file)
;;	      (ding)
	      (sit-for 1))))))



;;; Running these must be done "every time" before use, since
;;; they depend on a customizable variable.

;; trying different viewers; thanks to an original patch for
;; ess-swv.el from Leo <sdl@web.de> :
(defun ess-get-ps-viewer ()
  "Get external PostScript viewer to be used from ESS.
Use `ess-ps-viewer-pref' when that is executably found by \\[executable-find].
Otherwise try a list of fixed known viewers."
  (file-name-nondirectory
   (or (and ess-ps-viewer-pref		; -> ./ess-cust.el
	    (executable-find ess-ps-viewer-pref))
       (executable-find "gv")
       (executable-find "evince")
       (executable-find "kghostview"))))

(defun ess-get-pdf-viewer ()
  "Get external PDF viewer to be used from ESS.
Use `ess-pdf-viewer-pref' when that is executably found by \\[executable-find].
Otherwise try a list of fixed known viewers."
  (file-name-nondirectory
   (or (and ess-pdf-viewer-pref		; -> ./ess-cust.el
	    (executable-find ess-pdf-viewer-pref))
       (car (ess-get-words-from-vector
	     "getOption(\"pdfviewer\")\n"))
       (executable-find "evince")
       (executable-find "kpdf")
       (executable-find "xpdf")
       (executable-find "acroread"))))



 ; Buffer local customization stuff

;; Parse a line into its constituent parts (words separated by
;; whitespace).	   Return a list of the words.
;; Taken from rlogin.el, from the comint package, from XEmacs 20.3.
(defun ess-line-to-list-of-words (line)
  (let ((list nil)
	(posn 0))
	;; (match-data (match-data)))
    (while (string-match "[^ \t\n]+" line posn)
      (setq list (cons (substring line (match-beginning 0) (match-end 0))
		       list))
      (setq posn (match-end 0)))
    (store-match-data (match-data))
    (nreverse list)))

(defun ess-write-to-dribble-buffer (text)
  "Write TEXT to dribble ('*ESS*') buffer."
  (unless (buffer-live-p ess-dribble-buffer)
    ;; ESS dribble buffer must be re-created.
    (setq ess-dribble-buffer (get-buffer-create "*ESS*")))
  (let (deactivate-mark)
    (with-current-buffer ess-dribble-buffer
      (goto-char (point-max))
      (insert text))))

;; Shortcut to render "dribbling" statements less cluttering:
(defun ess-if-verbose-write (text)
  "Write TEXT to dribble buffer ('*ESS*') only *if* `ess-verbose'."
  (if ess-verbose (ess-write-to-dribble-buffer text)))

(defun ess-setq-vars-local (alist &optional buf)
  "Set language variables from ALIST, in buffer BUF, if desired."
  (if buf (set-buffer buf))
  (mapcar (lambda (pair)
	    (make-local-variable (car pair))
	    (if (cdr pair)
		(set (car pair) (eval (cdr pair)))))
	  alist)
  (ess-write-to-dribble-buffer
   (format "(ess-setq-vars-LOCAL): language=%s, dialect=%s, buf=%s, comint..echoes=%s, comint..sender=%s\n"
	   ess-language ess-dialect buf comint-process-echoes comint-input-sender)))

(defun ess-setq-vars-default (alist &optional buf)
  "Set language variables from ALIST, in buffer BUF, if desired."
  (ess-write-to-dribble-buffer
   (format "ess-setq-vars-default 0: ess-language=%s, -dialect=%s, buf=%s, comint..echoes=%s, comint..sender=%s\n"
	   ess-language ess-dialect buf comint-process-echoes comint-input-sender))
  (if buf (set-buffer buf))
  (mapcar (lambda (pair)
	    (set-default (car pair) (eval (cdr pair))))
	  alist)
  (ess-write-to-dribble-buffer
   (format "ess-setq-vars-default 1: ess-language=%s, -dialect=%s, buf=%s, comint..echoes=%s, comint..sender=%s\n"
	   ess-language ess-dialect buf comint-process-echoes comint-input-sender))
)

;;; versions thanks to Barry Margolin <barmar@bbnplanet.com>.
;;; unfortunately, requires 'cl.  Whoops.
;;(defun ess-setq-vars (var-alist &optional buf)
;;  "Set language variables from alist, in buffer `buf', if desired."
;;  (if buf (set-buffer buf))
;;  (dolist (pair var-alist)
;;    (set (car pair) (eval (cdr pair))))
;;  (ess-write-to-dribble-buffer
;;    (format "(ess-setq-vars): ess-language=%s, buf=%s \n"
;;	   ess-language buf)))
;;(defun ess-setq-vars-default (var-alist &optional buf)
;;  "Set language variables from alist, in buffer `buf', if desired."
;;  (if buf (set-buffer buf))
;;  (dolist (pair var-alist)
;;    (set-default (car pair) (eval (cdr pair))))
;;  (ess-write-to-dribble-buffer
;;    (format "(ess-setq-vars-default): ess-language=%s, buf=%s \n"
;;	   ess-language buf)))

;; Toby Speight <Toby.Speight@ansa.co.uk>
;;> ;; untested
;;> (let ((l R-customize-alist))	    ; or whatever
;;>   (while l
;;>	(set (car (car l)) (cdr (car l)))   ; set, not setq!
;;>	(setq l (cdr l))))
;;
;;
;;If they are to be buffer-local, you may need to
;;
;;>	;; untested
;;>	(set (make-local-variable (car (car l))) (cdr (car l)))
;;


;; Erik Naggum <erik@naggum.no>
;;
;;(mapcar (lambda (pair) (set (car pair) (cdr pair)))
;;	  R-customize-alist)
;;
;;if you want to evaluate these things along the way, which it appears that
;;you want, try:
;;
;;(mapcar (lambda (pair) (set (car pair) (eval (cdr pair))))
;;	  R-customize-alist)

;; jsa@alexandria.organon.com (Jon S Anthony)
;;(mapcar #'(lambda (x)
;;	    (set-variable (car x) (cdr x)))
;;	R-customize-alist)



; Run load hook and provide package

(run-hooks 'ess-mode-load-hook)

(provide 'ess)

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

;;; ess.el ends here
