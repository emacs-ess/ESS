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
;; Modified: $Date: 1997/08/25 14:31:04 $
;; Version: $Revision: 1.54 $
;; RCS: $Id: ess.el,v 1.54 1997/08/25 14:31:04 rossini Exp $
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
;;; Please report bugs to rossini@stat.sc.edu
;;; Comments, suggestions, words of praise and large cash donations
;;; are also more than welcome, but should generally be split between
;;; the authors :-).  

;;
;; $Log: ess.el,v $
;; Revision 1.54  1997/08/25 14:31:04  rossini
;; *** empty log message ***
;;
;; Revision 1.53  1997/07/31 10:56:11  rossini
;; removed the require cl, cl-macs from here (to do-comp).
;;
;; Revision 1.52  1997/07/30 12:36:53  rossini
;; added M.
;;
;; Revision 1.51  1997/07/30 12:25:19  rossini
;; added (require 'cl), (require 'cl-macs), since older Emacsen don't
;; have such things...
;; (and we need them, for "dolist" macro).
;;
;; Revision 1.50  1997/07/29 10:49:55  rossini
;; added [rR]-transcript-mode
;;
;; Revision 1.49  1997/07/28 12:34:01  rossini
;; made ess-setq-vars* clean.  No ugly hacks!
;; cleaned up authorship.
;;
;; Revision 1.48  1997/07/26 01:43:50  rossini
;; added ways to go through an alist and set variables based on it...
;; thanks, USENET!  (Barry Margolin, Erik Naggum, Toby Speight, and Jon
;; Anthony)
;;
;; Revision 1.47  1997/07/25 21:05:01  rossini
;; moved variables to ess-vars.  only thing left here are routines
;; necessary for handling "separate kinds of stat program" routines
;;
;; Revision 1.46  1997/07/24 11:21:36  rossini
;; ess-mode-version -> ESS-version
;;
;; Revision 1.45  1997/07/24 11:17:48  rossini
;; ess-mode-version updated.
;;
;; Revision 1.44  1997/07/17 20:56:55  rossini
;; removed weird debugging code.
;;
;; Revision 1.43  1997/07/17 20:33:42  rossini
;; need to be care WHEN to set buffer local (set again, as late as
;; possible!)
;;
;; Revision 1.42  1997/07/17 20:12:29  rossini
;; ess-set -> make sure we are setting up localvars!
;;
;; Revision 1.41  1997/07/17 19:24:22  rossini
;; stuff.
;;
;; Revision 1.40  1997/07/17 19:12:28  rossini
;; *** empty log message ***
;;
;; Revision 1.39  1997/07/17 19:09:26  rossini
;; added a "set".  Is this really required for buffer-local?
;;
;; Revision 1.38  1997/07/17 18:40:30  rossini
;; whoops.
;;
;; Revision 1.37  1997/07/17 18:37:14  rossini
;; write to dribble.
;;
;; Revision 1.36  1997/07/07 21:46:23  rossini
;; messages changed to reflect which ess-set-vars-* we are in.
;;
;; Revision 1.35  1997/07/07 21:33:25  rossini
;; added ess-write-to-dribble-buffer.
;;
;; Revision 1.34  1997/07/07 21:24:06  rossini
;; added dribble buffer for messages.
;;
;; Revision 1.33  1997/07/07 16:41:43  rossini
;; ess-set-vars split into a setq-default, setq version.
;;
;; Revision 1.32  1997/07/07 16:39:18  rossini
;; setq -> setq-default.  But this isn't right, I think?!
;;
;; Revision 1.31  1997/07/07 16:25:41  rossini
;; added ess-history-file setup...
;;
;; Revision 1.30  1997/07/03 14:32:33  rossini
;; referencing error: need to do symbols, not vars!
;;
;; Revision 1.29  1997/07/03 14:21:02  rossini
;; trying setq again.
;;
;; Revision 1.28  1997/07/03 14:18:16  rossini
;; added messages for debugging.
;;
;; Revision 1.27  1997/07/03 14:10:59  rossini
;; ess-customize-alist should NOT be buffer-local (only set variables
;; once or twice...).
;;
;; Revision 1.26  1997/07/03 14:03:45  rossini
;; ess-set-vars: setq -> setq-default
;;
;; Revision 1.25  1997/07/03 13:58:08  rossini
;; *** empty log message ***
;;
;; Revision 1.24  1997/07/03 13:46:24  rossini
;; ess-loop-timeout -> buffer-local.
;;
;; Revision 1.23  1997/07/03 13:33:15  rossini
;; redid doc string.
;;
;; Revision 1.22  1997/07/03 13:27:57  rossini
;; made ess-customize-alist buffer-local
;;
;; Revision 1.21  1997/07/03 13:21:52  rossini
;; added functions, alist var, for configuring variables.
;;
;; Revision 1.20  1997/07/02 14:59:54  rossini
;; inferior-ess-procname should also be a (let ...) variable!
;;
;; Revision 1.19  1997/07/02 14:59:09  rossini
;; ess-defdir should've been a (let ...) variable.  bad programming
;; (AJR).
;;
;; Revision 1.18  1997/07/01 16:24:06  rossini
;; moved make-local-... to here, with make-variable-...
;;
;; Revision 1.17  1997/07/01 16:16:44  rossini
;; local variables defined here, from ess-inf.el
;;
;; Revision 1.16  1997/07/01 16:13:36  rossini
;; inferior-ess-objects-command: buffer local.
;; inferior-ess-secondary-prompt: corrected (to be buf-loc).
;;
;; Revision 1.15  1997/06/30 22:47:42  rossini
;; changed ess-mode-version (we're at beta-8).
;;
;; Revision 1.14  1997/06/30 22:41:41  rossini
;; ess-trans -> ess-trns
;;
;; Revision 1.13  1997/06/30 22:35:36  rossini
;; one more ) to remove.
;;
;; Revision 1.12  1997/06/30 22:33:21  rossini
;; ess-directory defaults to "nil".
;;
;; Revision 1.11  1997/06/30 22:21:05  rossini
;; forgot an ")".
;; whoops.
;;
;; Revision 1.10  1997/06/30 22:19:34  rossini
;; made ess-directory buffer-local in the RIGHT place.
;;
;; Revision 1.9  1997/06/19 21:15:53  rossini
;; matrix -> matriux.
;;
;; Revision 1.8  1997/06/19 20:51:32  rossini
;; removed comments about customization, moved into ess-site.el
;;
;; Revision 1.7  1997/06/18 18:49:33  rossini
;; edited a bit more
;; .
;;
;; Revision 1.6  1997/06/18 18:31:44  rossini
;; incremented version to b7.
;;
;; Revision 1.5  1997/06/15 08:38:19  rossini
;; documentation cleanup
;;
;; Revision 1.4  1997/05/21 20:07:00  rossini
;; conversion to ess complete
;;
;; Revision 1.3  1997/05/21 18:40:29  rossini
;; changed S -> ess...
;;
;; Revision 1.2  1997/05/20 16:03:52  rossini
;; S -> ess
;;
;; Revision 1.1  1997/05/20 15:59:44  rossini
;; Initial revision
;;
;; Revision 1.53  1997/04/24 23:12:58  rossini
;; *** empty log message ***
;;
;; Revision 1.52  1997/04/23 15:38:00  rossini
;; changed version variable.
;;
;; Revision 1.51  1997/04/23 13:31:03  rossini
;; added Martin's patch.
;;
;; Revision 1.50  1997/04/23 03:13:16  rossini
;; seems to work now?
;;
;; Revision 1.49  1997/04/23 01:10:42  rossini
;; stuff.
;;
;; Revision 1.48  1997/04/23 01:07:09  rossini
;; error in defvar.
;;
;; Revision 1.47  1997/04/23 01:03:10  rossini
;; buffer-local-stuff
;;
;; Revision 1.46  1997/04/23 00:30:14  rossini
;; *** empty log message ***
;;
;; Revision 1.45  1997/04/22 01:45:28  rossini
;; removed make-local from here...
;;
;; Revision 1.44  1997/04/22 01:37:35  rossini
;; inferior-S-program can't be buffer-local.  Weird.
;;
;; Revision 1.43  1997/04/21 00:16:43  rossini
;; try setq-default?
;;
;; Revision 1.42  1997/04/20 23:08:03  rossini
;; changed some variables to buffer-local.
;;
;; Revision 1.41  1997/04/17 16:59:10  rossini
;; changed ("<-" . font-lock-reference-face)
;; to ("<<?-" . font-lock-reference-face)
;;
;; Revision 1.40  1997/04/16 20:29:53  rossini
;; ...
;;
;; Revision 1.39  1997/04/16 18:38:20  rossini
;; update the version.
;;
;; Revision 1.38  1997/04/09 02:11:28  rossini
;; changed calling vars.
;;
;; Revision 1.37  1997/04/08 01:06:34  rossini
;; added (req 'font-lock)
;;
;; Revision 1.36  1997/04/08 00:58:23  rossini
;; added S-inf variables.
;;
;; Revision 1.35  1997/04/08 00:38:29  rossini
;; moved non-local user/system variables here from S-mode.
;;
;; Revision 1.34  1997/04/07 14:34:36  rossini
;; more doc fixes.
;;
;; Revision 1.33  1997/04/07 14:29:32  rossini
;; docs cleaned, migrated to NEWS.
;;
;; Revision 1.32  1997/04/07 12:16:57  rossini
;; Commentary:  deferred installation instructions to README and S-site.
;;
;; Removed Comint stuff, since present in 19.x emacsen.
;;
;; Revision 1.31  1997/04/04 17:29:38  rossini
;; added comments for imenu.  (WHY?)
;;
;; Revision 1.30  1997/04/04 17:22:20  rossini
;; changed internal version number.
;;
;; Revision 1.29  1997/04/03 23:42:30  rossini
;; edited comments for S-version.
;;
;; Revision 1.28  1997/04/03 23:40:20  rossini
;; moved defvar for S-proc-prefix accordingly
;;
;; Revision 1.27  1997/04/03 21:57:03  rossini
;; fixed up.
;;
;;

;;; Code:

;;*;; Requires and autoloads
;;;=====================================================
;;;

(require 'easymenu)
(require 'font-lock)
(require 'ess-vars)

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

(defun ess-write-to-dribble-buffer (text)
  "Write `text' to dribble buffer."
  (save-excursion
    (set-buffer ess-dribble-buffer)
    (goto-char (point-max))
    (insert-string text)))

(defun ess-setq-vars (alist &optional buf) 
  "Set language variables from ALIST, in buffer `BUF', if desired."
  (if buf (set-buffer buf))
  (mapcar (lambda (pair)
            (set (car pair) (eval (cdr pair))))
          alist)
  (ess-write-to-dribble-buffer 
   (format "(ess-setq-vars): ess-proc-prefix=%s, buf=%s \n"
           ess-proc-prefix buf)))

(defun ess-setq-vars-default (alist &optional buf) 
  "Set language variables from ALIST, in buffer `BUF', if desired."
  (if buf (set-buffer buf))
  (mapcar (lambda (pair)
            (set-default (car pair) (eval (cdr pair))))
          alist)
  (ess-write-to-dribble-buffer 
   (format "(ess-setq-vars-default): ess-proc-prefix=%s, buf=%s \n"
           ess-proc-prefix buf)))

;;; versions thanks to Barry Margolin <barmar@bbnplanet.com>.
;;(defun ess-setq-vars (var-alist &optional buf) 
;;  "Set language variables from alist, in buffer `buf', if desired."
;;  (if buf (set-buffer buf))
;;  (dolist (pair var-alist)
;;    (set (car pair) (eval (cdr pair))))
;;  (ess-write-to-dribble-buffer 
;;    (format "(ess-setq-vars): ess-proc-prefix=%s, buf=%s \n"
;;	   ess-proc-prefix buf)))
;;(defun ess-setq-vars-default (var-alist &optional buf) 
;;  "Set language variables from alist, in buffer `buf', if desired."
;;  (if buf (set-buffer buf))
;;  (dolist (pair var-alist)
;;    (set-default (car pair) (eval (cdr pair))))
;;  (ess-write-to-dribble-buffer 
;;    (format "(ess-setq-vars-default): ess-proc-prefix=%s, buf=%s \n"
;;	   ess-proc-prefix buf)))

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
