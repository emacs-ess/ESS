;;; -*- Mode: Emacs-Lisp -*- 
;;; ess.el --- A package for running Statistical Software within Emacs 
;;; (Emacs Speaks Statistics).

;; Copyright (C) 1989--1996 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1996--1997 Rossini, Heiberger, Hornik, and Maechler.

;; Authors: Doug Bates, Ed Kademan, Frank Ritter, David Smith
;; Maintainers: A.J. Rossini <rossini@stat.sc.edu>
;;                       Martin Maechler  <maechler@stat.math.ethz.ch>
;;                       Kurt Hornik <hornik@ci.tuwien.ac.at>  <-- CHANGE
;;                       Richard M. Heiberger <rmh@fisher.stat.temple.edu>
;; Created: October 14, 1991
;; Modified: $Date: 1997/09/01 18:10:50 $
;; Version: $Revision: 1.58 $
;; RCS: $Id: ess.el,v 1.58 1997/09/01 18:10:50 rossini Exp $
;; Lisp-dir-entry  : ESS |
;;                   R. Heiberger, K. Hornik, M. Maechler, A.J. Rossini|
;;                   rossini@stat.sc.edu|
;;                   Generic Interface for Statistical Software Packages|
;;                   92-06-29|
;;                   4.9|
;;                   /ftp.math.sc.edu:rossini/ESS-4.9.tar.gz
;; This file is part of ess-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; In short: you may use this code any way you like, as long as you
;; don't charge money for it, remove this notice, or hold anyone liable
;; for its results.

;; Copyright 1989,1991,1992, Doug Bates    bates@stat.wisc.edu
;;            1993, 1994     Ed Kademan    kademan@stat.wisc.edu
;;                           Frank Ritter  ritter@psychology.nottingham.ac.uk
;;            1994--1997     David Smith <maa036@lancaster.ac.uk>
;;                           
;;            1996--1997     Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
;;            1996--1997     Martin Maechler <maechler@stat.math.ethz.ch>
;;            1996--1997     A.J. Rossini <rossini@stat.sc.edu>
;;

;;; Commentary:

;;; PURPOSE
;;; Interface to the S and XLisp dialects of statistical programming
;;; languages 

;;; BRIEF OVERVIEW
;;;
;;; Supports structured editing of S and XLisp (statistics programming
;;; languages) functions that are integrated with a running process in
;;; a buffer.

;;; THE ESS-MODE MAILING LIST 
;;;
;;; There is an informal mailing list for discussions of ess-mode. Alpha
;;; and beta releases of ess-mode are also announced here. Send mail to
;;; S-mode-request@stat.math.ethz.ch to join.

;;; OVERVIEW OF S MODE
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
;;; name completion while you do these.  For more detailed
;;; information see the documentation strings for inferior-ess,
;;; inferior-ess-mode, ess-mode, and comint-mode.  There are also
;;; many variables and hooks available for customizing (see
;;; the variables below that have document strings that start
;;; with an "*").

;;; INSTALLATION
;;; See README and S-site for details.

;;; GETTING LATER RELEASES OF S MODE
;;; <-- NEED NEW STUFF HERE -->


;;; CREDITS.
;;; Thanks to shiba@shun.isac.co.jp (Ken'ichi "Modal" Shibayama) for
;;;   the indenting code.
;;; Thanks also to maechler@stat.math.ethz.ch (Martin Maechler) for
;;;   suggestions and bug fixes.
;;; ess-eval-line-and-next-line is based on a function by Rod Ball 
;;;   (rod@marcam.dsir.govt.nz)
;;; Also thanks from David Smith to the previous authors for all their
;;; help and suggestions.
;;; And thanks from Hornik/Maechler/Rossini to David Smith.

;;; BUG REPORTS
;;; Please report bugs to ess-bugs@stat.math.ethz.ch
;;; Comments, suggestions, words of praise and large cash donations
;;; are also more than welcome, but should generally be split between
;;; the authors :-).  

;;; Code:

;;*;; Requires and autoloads
;;;=====================================================
;;;

(require 'easymenu)
(require 'font-lock)
(require 'ess-vars)




;;; autoloads originally in ess-site.  

(autoload 'S-mode "ess-mode"
  "Major mode for editing S source code." t)
(autoload 'R-mode "ess-mode"
  "Major mode for editing R source code." t)
(autoload 'XLS-mode "ess-mode"
  "major mode for editing XLispStat code." t)
(autoload 'S-transcript-mode
  "ess-trns" "ESS source eval mode" t)
(autoload 'R-transcript-mode
  "ess-trns" "ESS source eval mode" t)
(autoload 'inferior-ess "ess-inf"
  "Run [inferior-ess-program], an ess process, in an Emacs buffer" t)




 ; ess-mode: editing S source

(autoload 'ess-dump-object-into-edit-buffer "ess-mode"
  "Edit an S object" t)

(autoload 'ess-parse-errors "ess-mode"
  "Jump to the last error generated from a sourced file" t)

(autoload 'ess-load-file "ess-mode"
  "Source a file into S.")

;;;;* Alias ess-mode to s-mode
;;; Emacs will set the mode for a file based on the file's header.
;;; The mode name is indicated by putting it between -*- on the top line. 
;;; (Other commands can go here too, see an Emacs manual.)
;;; For a file you also load, you will want a leading # (comment to S)
;;; Emacs will downcase the name of the mode, e.g., S, so we must provide
;;; s-mode in lower case too.  That is, "#-*- S-*-" invokes s-mode and 
;;; not S-mode.
(fset 's-mode 'S-mode)
(fset 'r-mode 'R-mode)

 ; ess-transcript-mode

(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files" t)

(fset 's-transcript-mode 'ess-transcript-mode)
(fset 'S-transcript-mode 'ess-transcript-mode)
(fset 'r-transcript-mode 'ess-transcript-mode)
(fset 'R-transcript-mode 'ess-transcript-mode)

(autoload 'ess-display-help-on-object "ess-help"
  "Display help on an S object" t)

(defalias 'ess-help 'ess-display-help-on-object)

(autoload 'ess-goto-info "ess-help"
  "Jump to the relevant section in the ess-mode manual" t)

(autoload 'ess-submit-bug-report "ess-help"
  "Submit a bug report on the ess-mode package" t)

 ; Set up for menus, if necessary
;;;
;;;	nn.	Set up the keymaps for the simple-menus
;;;

;;(if ess-use-menus
;;    (require 'ess-menu))


;;; Function Menu (func-menu) for XEmacs:
;;(defvar fume-function-name-regexp-smode
;;  " "
;;  "Expression to get function names")
;;
;;(append 
;; '((s-mode  . fume-function-name-regexp-smode)
;;   (r-mode  . fume-function-name-regexp-smode))
;; fume-function-name-regexp-alist)

;;; Imenu for Emacs...



 ; Buffer local customization stuff

;; Parse a line into its constituent parts (words separated by
;; whitespace).    Return a list of the words.
;; Taken from rlogin.el, from the comint package, from XEmacs 20.3.
(defun ess-line-to-list-of-words (line)
  (let ((list nil)
	(posn 0)
        (match-data (match-data)))
    (while (string-match "[^ \t\n]+" line posn)
      (setq list (cons (substring line (match-beginning 0) (match-end 0))
                       list))
      (setq posn (match-end 0)))
    (store-match-data (match-data))
    (nreverse list)))

(defun ess-write-to-dribble-buffer (text)
  "Write `text' to dribble buffer."
  (save-excursion
    (set-buffer ess-dribble-buffer)
    (goto-char (point-max))
    (insert-string text)))

(defun ess-setq-vars-local (alist &optional buf) 
  "Set language variables from ALIST, in buffer `BUF', if desired."
  (if buf (set-buffer buf))
  (mapcar (lambda (pair)
            (set (make-local-variable (car pair)) (eval (cdr pair))))
          alist)
  (ess-write-to-dribble-buffer 
   (format "(ess-setq-vars-local): ess-language=%s, buf=%s \n"
           ess-language buf)))

(defun ess-setq-vars-default (alist &optional buf) 
  "Set language variables from ALIST, in buffer `BUF', if desired."
  (if buf (set-buffer buf))
  (mapcar (lambda (pair)
            (set-default (car pair) (eval (cdr pair))))
          alist)
  (ess-write-to-dribble-buffer 
   (format "(ess-setq-vars-default): ess-language=%s, buf=%s \n"
           ess-language buf)))

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
;;> (let ((l R-customize-alist))            ; or whatever
;;>   (while l
;;>     (set (car (car l)) (cdr (car l)))   ; set, not setq!
;;>     (setq l (cdr l))))
;;
;;
;;If they are to be buffer-local, you may need to
;;
;;>     ;; untested
;;>     (set (make-local-variable (car (car l))) (cdr (car l)))
;;


;; Erik Naggum <erik@naggum.no>
;;
;;(mapcar (lambda (pair) (set (car pair) (cdr pair)))
;;        R-customize-alist)
;;
;;if you want to evaluate these things along the way, which it appears that
;;you want, try:
;;
;;(mapcar (lambda (pair) (set (car pair) (eval (cdr pair))))
;;        R-customize-alist)

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

;;; ess.el ends here
