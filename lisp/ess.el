;;; -*- Mode: Emacs-Lisp -*- 
;;; ess.el --- A package for running Statistical Software within Emacs 
;;; (Emacs Speaks Statistics).

;; Copyright (C) 1989--1996 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1996--1997 Rossini, Maechler, and Hornik.

;; Author: Doug Bates, Ed Kademan, Frank Ritter, David Smith
;; Maintainers: A.J. Rossini <rossini@stat.sc.edu>
;; Created: October 14, 1991
;; Modified: $Date: 1997/07/07 16:41:43 $
;; Version: $Revision: 1.33 $
;; RCS: $Id: ess.el,v 1.33 1997/07/07 16:41:43 rossini Exp $
;; Lisp-dir-entry  : ess-mode|
;;                   K. Hornik, M. Maechler, A.J. Rossini|
;;                   rossini@stat.sc.edu|
;;                   General Interface to statistical software packages|
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

;;; THE ess-MODE MAILING LIST 
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

(defconst ess-mode-version "4.9-b8" 
  "Version of ess-mode currently loaded.")

 ; User changeable variables

;;; User changeable variables
;;;=====================================================
;;; Users note: Variables with document strings starting
;;; with a * are the ones you can generally change safely, and
;;; may have to upon occasion.

;;*;; Options and Initialization

(defvar ess-use-menus t
  "If t, use the menu system.")

(defvar ess-ask-for-ess-directory t
  "*If non-nil, the process directory will be requested each time S is run")

(defvar ess-ask-about-transfile nil
  "*If non-nil, asks about a transcript file before running ess")

(defvar ess-proc-prefix "S"
  "*Prefix of all ess processes. Can be changed, e.g., to 'R'.
Use `setq-default' if setting it in .emacs")

(make-variable-buffer-local 'ess-proc-prefix)
(setq-default ess-proc-prefix "S")


(defvar ess-directory nil
  "*The directory ess is run from.  It must end in a slash.
Provided as a default if ess-ask-for-ess-directory is non-nil.
A nil value means use the current buffer's default directory.
Buffer-local: in process buffers, this contains the directory ess was
run from.")

(make-variable-buffer-local 'ess-directory)
(setq-default ess-directory nil)

;; MM, 13Mar97.  This should be set buffer-local!
(defvar ess-history-file (concat "." ess-proc-prefix "history")
  "*File to pick up history from. 
If this is a relative file name, it is relative to ess-directory.")

(make-variable-buffer-local 'ess-history-file)
(setq-default ess-history-file (concat "." ess-proc-prefix "history"))

;;*;; Variables relating to the ess executable

(defvar ess-version-running "3.0"
  "Version of ess being run.
The value of this variable affects the default values of the following
variables:
 	 inferior-ess-help-command
	 inferior-ess-search-list-command
	 ess-dump-error-re

Modifications to these variables are made at *load* time (provided, of
course, they have not already been given values), hence changing the
value of ess-version-running after this package is loaded will have no
effect.
 
Currently the string \"3.0\" is the only value of this variable with
any real meaning; in this case the defaults are set to comply with the
August '91 (3.0) version of S/Splus, defaults which also work for
version 2.3. Any other value than \"3.0\" sets the defaults to comply
with the 1988 version of S/Splus.

Please reserve the following values as special:
   \"3.0\"    Version 3.0 (August '91) of S/Splus
   \"2.3\"    Version 2.3 of S/Splus
   \"old\"    Any older version

Choices should include:  old, 2.x, 3.x, 4.x, or S+3.x, S+4.x, or R
Also need this buffer-local.")

(make-variable-buffer-local 'ess-version-running)
(setq-default ess-version-running "3.0")

;;*;; Variables concerning editing behaviour

(defvar ess-filenames-map t
  "Declares if the filenames in an attached directory are the same 
as objects in that directory (when t). This is not true for DOS and
other OS's with limited filename lengths.  Even if this is set
incorrectly, the right things will probably still happen, however.")

(defvar ess-keep-dump-files 'check
  "*Variable controlling whether to delete dump files after a successful load.
If nil: always delete.  If `ask', confirm to delete.  If `check', confirm
to delete, except for files created with ess-dump-object-into-edit-buffer.
Anything else, never delete.  This variable only affects the behaviour
of ess-load-file.  Dump files are never deleted if an error occurs
during the load. ")

;;; Boolean flag which determines what to do with the dump files
;;; generated by \\[ess-dump-object-into-edit-buffer], as follows:
;;; 
;;; 	If nil: dump files are deleted after each use, and so appear
;;; only transiently. The one exception to this is when a loading error
;;; occurs, in which case the file is retained until the error is
;;; corrected and the file re-loaded.
;;; 
;;; 	If non-nil: dump files are not deleted, and backups are kept
;;; as usual.  This provides a simple method for keeping an archive of S
;;; functions in text-file form.
;;; 
;;; Auto-save is always enabled in dump-file buffers to enable recovery
;;; from crashes.

(defvar ess-delete-dump-files t
  "*If non-nil, delete dump files after they are created.
This applies to dump files created with ess-dump-object-into-edit-buffer, only.

This is useful to prevent sources file being created for objects
you don't actually modify.  Once the buffer is modified and saved
however, the file is not subsequently unless ess-keep-dump-files is
nil, and the file is successfully loaded back into S.")


;;; From ess-mode:


(defvar ess-mode-silently-save t
  "*If non-nil, automatically save ess source buffers before loading")

;;*;; Variables controlling editing

;;;*;;; Edit buffer processing
(defvar ess-function-template " <- function( )\n{\n\n}\n"
  "If non-nil, function template used when editing nonexistent objects.

The edit buffer will contain the object name in quotes, followed by
this string. Point will be placed after the first parenthesis or
bracket.")

;;; By K.Shibayama 5.14.1992
;;; Setting any of the following variables in your .emacs is equivalent
;;; to modifying the DEFAULT style.

;;;*;;; Indentation parameters

(defvar ess-auto-newline nil
  "*Non-nil means automatically newline before and after braces
inserted in S code.")

(defvar ess-tab-always-indent t
  "*Non-nil means TAB in S mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defvar ess-indent-level 2
  "*Indentation of S statements with respect to containing block.")

(defvar ess-brace-imaginary-offset 0
  "*Imagined indentation of a S open brace that actually follows a statement.")

(defvar ess-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")

(defvar ess-continued-statement-offset 2
  "*Extra indent for lines not starting new statements.")

(defvar ess-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to ess-continued-statement-offset.")

(defvar ess-arg-function-offset 2
  "*Extra indent for internal substatements of function `foo' that called
in `arg=foo(...)' form.
If not number, the statements are indented at open-parenthesis following foo.")


;; PD, 1Apr97 : 
;;The default ess-else-offset should be 0, not 2 IMHO (try looking at
;;the ls() function, for instance).  Was 2.
(defvar ess-else-offset 0
  "*Extra indent for `else' lines.")

(defvar ess-expression-offset 4
  "*Extra indent for internal substatements of `expression' that specified
in `obj <- expression(...)' form.
If not number, the statements are indented at open-parenthesis following
`expression'.")

;;;*;;; Editing styles

(defvar ess-default-style-list
  (list 'DEFAULT
	(cons 'ess-indent-level ess-indent-level)
	(cons 'ess-continued-statement-offset ess-continued-statement-offset)
	(cons 'ess-brace-offset ess-brace-offset)
	(cons 'ess-expression-offset ess-expression-offset)
	(cons 'ess-else-offset ess-else-offset)
	(cons 'ess-brace-imaginary-offset ess-brace-imaginary-offset)
	(cons 'ess-continued-brace-offset ess-continued-brace-offset)
	(cons 'ess-arg-function-offset ess-arg-function-offset))
  "Default style constructed from initial values of indentation variables.")

(defvar ess-style-alist
  (cons ess-default-style-list
	'((GNU (ess-indent-level . 2)
	       (ess-continued-statement-offset . 2)
	       (ess-brace-offset . 0)
	       (ess-arg-function-offset . 4)
	       (ess-expression-offset . 2)
	       (ess-else-offset . 0))
	  (BSD (ess-indent-level . 8)
	       (ess-continued-statement-offset . 8)
	       (ess-brace-offset . -8)
	       (ess-arg-function-offset . 0)
	       (ess-expression-offset . 8)
	       (ess-else-offset . 0))
	  (K&R (ess-indent-level . 5)
	       (ess-continued-statement-offset . 5)
	       (ess-brace-offset . -5)
	       (ess-arg-function-offset . 0)
	       (ess-expression-offset . 5)
	       (ess-else-offset . 0))
	  (C++ (ess-indent-level . 4)
	       (ess-continued-statement-offset . 4)
	       (ess-brace-offset . -4)
	       (ess-arg-function-offset . 0)
	       (ess-expression-offset . 4)
	       (ess-else-offset . 0))))
  "Predefined formatting styles for ess code")

(defvar ess-default-style 'DEFAULT
  "*The default value of ess-style")

(defvar ess-style ess-default-style
  "*The buffer specific ess indentation style.")

;;*;; Variables controlling behaviour of dump files

(defvar ess-source-directory "/tmp/"
  "*Directory in which to place dump files.
This can be a string (an absolute directory name ending in a slash) or
a lambda expression of no arguments which will return a suitable string
value.  The lambda expression is evaluated with the process buffer as the
current buffer.")
;;; Possible value:
;;; '(lambda () (file-name-as-directory
;;;	      (expand-file-name (concat (car ess-search-list) "/.Src"))))
;;; This always dumps to a sub-directory (".Src") of the current ess
;;; working directory (i.e. first elt of search list)

(defvar ess-dump-filename-template (concat (user-login-name) ".%s.S")
  "*Template for filenames of dumped objects.
%s is replaced by the object name.")
;;; This gives filenames like `user.foofun.S', so as not to clash with
;;; other users if you are using a shared directory. Other alternatives:
;;; "%s.S" ; Don't bother uniquifying if using your own directory(ies)
;;; "dump" ; Always dump to a specific filename. This makes it impossible
;;;          to edit more than one object at a time, though.
;;; (make-temp-name "scr.") ; Another way to uniquify



;;*;; Hooks

(defvar ess-mode-hook '()
  "*Hook for customizing ess mode each time it is entered.")

(defvar ess-mode-load-hook '()
  "*Hook to call when ess.el is loaded.")

(defvar ess-pre-run-hook nil
  "*Hook to call before starting up ess.
Good for setting up your directory.")

(defvar ess-post-run-hook nil
  "*Hook to call just after the ess process starts up.
Good for evaluating ess code.")

(defvar inferior-ess-mode-hook '()
  "*Hook for customizing inferior ess mode.
Called after inferior-ess-mode is entered and variables have been initialised.")

(defvar ess-help-mode-hook nil 
  "Functions to call when entering ess-help-mode. ")

(defvar ess-send-input-hook nil
  "Hook called just before line input is sent to the process")

(defvar ess-transcript-mode-hook nil
  "Hook for customizing ess transcript mode.")

 ; System variables

(defvar ess-local-process-name nil
  "The name of the ess process associated with the current buffer.")

(make-variable-buffer-local 'ess-local-process-name)


;;*;; Regular expressions

(defvar ess-function-pattern
  (concat
   "\\(" ; EITHER
   "\\s\"" ; quote
   "\\(\\sw\\|\\s_\\)+" ; symbol
   "\\s\"" ; quote
   "\\s-*\\(<-\\|_\\)\\(\\s-\\|\n\\)*" ; whitespace, assign, whitespace/nl
   "function\\s-*(" ; function keyword, parenthesis
   "\\)\\|\\(" ; OR
   "\\<\\(\\sw\\|\\s_\\)+" ; symbol
   "\\s-*\\(<-\\|_\\)\\(\\s-\\|\n\\)*" ; whitespace, assign, whitespace/nl
   "function\\s-*(" ; function keyword, parenthesis
   "\\)")
  "The regular expression for matching the beginning of an S function.")

(defvar ess-dumped-missing-re
  "\\(<-\nDumped\n\\'\\)\\|\\(<-\\(\\s \\|\n\\)*\\'\\)"
  "If a dumped object's buffer matches this re, then it is replaced
by ess-function-template.")

(defvar ess-dump-error-re
  (if (string= ess-version-running "3.0") "\nDumped\n\\'" "[Ee]rror")
  "Regexp used to detect an error when loading a file.")

;;*;; Miscellaneous system variables

(defvar ess-source-modes '(ess-mode)
  "A list of modes used to determine if a buffer contains ess source code.")
;;; If a file is loaded into a buffer that is in one of these major modes, it
;;; is considered an ess source file.  The function ess-load-file uses this to
;;; determine defaults.

(defvar ess-error-buffer-name "*ESS-errors*"
  "Name of buffer to keep error messages in.")

;;*;; Font-lock support
(defvar ess-mode-font-lock-keywords
 '(("\\s\"?\\(\\(\\sw\\|\\s_\\)+\\)\\s\"?\\s-*\\(<-\\|_\\)\\(\\s-\\|\n\\)*function" 1 font-lock-function-name-face t)
   ("<<?-" . font-lock-reference-face)
   ("\\<\\(TRUE\\|FALSE\\|T\\|F\\|NA\\|NULL\\|Inf\\|NaN\\)\\>" . font-lock-type-face)
   ("\\<\\(library\\|attach\\|detach\\|source\\)\\>" . font-lock-reference-face)
   "\\<\\(while\\|for\\|in\\|repeat\\|if\\|else\\|switch\\|break\\|next\\|return\\|stop\\|warning\\|function\\)\\>")
 "Font-lock patterns used in ess-mode bufffers.")


 ; ess-inf: variables for inferior-ess.

;;; User changeable variables

;;*;; System dependent variables

(defvar inferior-S-program-name "Splus"
  "*Program name for invoking an inferior S with S().")

(defvar inferior-R-program-name "R"
  "*Program name for invoking an inferior S with R().")

(defvar inferior-XLS-program-name "xlispstat"
  "*Program name for invoking an inferior S with XLS().")

(defvar inferior-S4-program-name "S4"
  "*Program name for invoking an inferior S with S4().")


(defvar inferior-ess-program nil ;inferior-S-program-name
  "*Default program name for invoking inferior-S().
The other variables ...-program-name should be changed, for the
corresponding program.")

;;(make-local-variable 'inferior-S-program)
(make-variable-buffer-local 'inferior-ess-program)
(setq-default inferior-ess-program inferior-S-program-name)
;;- (setq inferior-S-program
;;-       (cond ((string= S-proc-prefix "S") "Splus")
;;- 	    ((string= S-proc-prefix "R") "R")
;;- 	    (t "S")
;;- 	    ))



;; AJR: this is a generic function
(defvar inferior-ess-args nil
  "*String of arguments passed to the S process on startup if the name
of the S program is `Splus'; also called if one uses `R', but this
isn't implemented yet.")

(defvar inferior-ess-pager "cat"
  "*Pager to use for reporting help files and similar things.")

(defvar S-plus (assoc inferior-ess-program '(("Splus") ("S+")))
  "Set to t if Splus is being used instead of vanilla S")
;;; Used for setting default values of other variables, and hence
;;; has no effect after S.el has been loaded.

(defvar inferior-ess-primary-prompt "[a-zA-Z0-9() ]*> ?"
  "Regular expression used by ess-mode to detect the primary prompt.
Do not anchor to bol with `^'.")

(make-variable-buffer-local 'inferior-ess-primary-prompt)
(setq-default inferior-ess-primary-prompt "[a-zA-Z0-9() ]*> ?")

(defvar inferior-ess-secondary-prompt "+ ?"
  "Regular expression used by ess-mode to detect the secondary prompt.
(This is issued by S to continue an incomplete expression). Do not
anchor to bol with `^'.")

(make-variable-buffer-local 'inferior-ess-secondary-prompt)
(setq-default inferior-ess-secondary-prompt "+ ?")

;;*;; Variables controlling interaction with the S process

(defvar ess-execute-in-process-buffer nil
  "*If non-nil, the ess-execute- commands output to the process buffer.
Otherwise, they get their own temporary buffer.")

(defvar ess-eval-visibly-p t
  "*If non-nil, the ess-eval- commands display the text to be evaluated
in the process buffer.")

(defvar ess-synchronize-evals nil
  "*If t, then all evaluations will synchronize with ess process. This
means ess-mode will wait for S to dent a prompt before sending the next
line of code. This allows users of version 18.57 or less of Emacs to
evaluate large regions of code without causing an error. Users or
18.58 or later usually do not want this feature, since it locks up use
of Emacs until the code has been successfully evaluated by S.")

(defvar ess-eval-visibly-at-end t
  "*If non-nil, the ess-eval- commands display the results of evaluation
  at the bottom of the process buffer.")


 ; System variables

;;*;; Variables relating to multiple processes

(defvar ess-current-process-name nil
  "Name of the current S process.")

;; defconst ess-local-process-name now done in S.el

(defvar ess-process-name-list nil
  "Alist of active ess processes.")

;;*;; Inferior ess commands

(defvar inferior-ess-load-command "source(\"%s\")\n"
  "Format-string for building the ess command to load a file.")
;;; This format string should use %s to substitute a file name
;;; and should result in an ess expression that will command the inferior ess
;;; to load that file.

(defvar inferior-ess-dump-command "dump(\"%s\",file=\"%s\")\n"
  "Format-string for building the ess command to dump an object into a file.")
;;; Use first %s to substitute an object name
;;;     second %s substitutes the dump file name.

(defvar inferior-ess-help-command "help(\"%s\")\n"
  "Format-string for building the ess command to ask for help on an object.")
;;; This format string should use %s to substitute an object name.

(make-variable-buffer-local 'inferior-ess-help-command)
(setq-default inferior-ess-help-command "help(\"%s\")\n")


(defvar inferior-ess-exit-command "q()\n"
  "Format-string for building the ess command to exit.")
;;; This format string should use %s to substitute an object name.

(make-variable-buffer-local 'inferior-ess-exit-command)
(setq-default inferior-ess-exit-command "q()\n")


(defvar inferior-ess-search-list-command "search()\n"
  "ess command that prints out the search list.")
;;; i.e. The list of directories and (recursive) objects that ess uses when
;;; it searches for objects.

(defvar inferior-ess-names-command "names(%s)"
  "Format string for ess command to extract names from an object.")
;;; %s is replaced by the object name -- usually a list or data frame

(defvar inferior-ess-objects-command "ls()" ;; others: in (S) or (R)
  "Format string for ess command to get a list of objects at position %d")
;;; Don't include a newline at the end! Used in ess-execute-objects

(make-variable-buffer-local 'inferior-ess-objects-command)
(setq-default inferior-ess-objects-command "ls()")

(defvar inferior-ess-get-prompt-command "options()$prompt\n"
  "Command to find the value of the current S prompt.")

;;*;; Regular expressions

(defvar inferior-ess-prompt nil
  "The regular expression inferior ess mode uses for recognizing prompts.
 Constructed at run time from 'inferior-ess-primary-prompt and
'inferior-ess-secondary-prompt") 

(make-variable-buffer-local 'inferior-ess-prompt)

(defvar ess-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|collection(\\|library(\\)"
  "The regexp for matching the ess commands that change the search path.")

;;*;; Process-dependent variables

(defvar ess-search-list nil
  "Cache of list of directories and objects to search for ess objects.")

(make-variable-buffer-local 'ess-search-list)

(defvar ess-sl-modtime-alist nil
  "Alist of modtimes for all ess directories accessed this session.")

(make-variable-buffer-local 'ess-sl-modtime-alist)

(defvar ess-sp-change nil
  "This symbol flags a change in the ess search path.")

(make-variable-buffer-local 'ess-sp-change)

(defvar ess-prev-load-dir/file nil
  "This symbol saves the (directory . file) pair used in the last
ess-load-file command.  Used for determining the default in the next one.")

(make-variable-buffer-local 'ess-prev-load-dir/file)

(defvar ess-object-list nil
  ;; This is a list of the currently known object names.  It is
  ;; current only for one command entry; it exists under the
  ;; assumption that the list of objects doesn't change while entering
  ;; a command.
  "Cache of object names")

(make-variable-buffer-local 'ess-object-list)

;;*;; Miscellaneous system variables

(defvar inferior-ess-mode-map nil
  "Keymap for inferior-ess mode.")

(defvar ess-object-name-db-file "ess-namedb"
  "File containing definitions for ess-object-name-db.")

(defvar ess-object-name-db nil
  "Alist of lists of object names, with directory names as keys.
The file ess-namedb.el is loaded (if it exists) to define this variable.
See also function ess-create-object-name-db.")

;;; This is EVIL.  We don't want this.
;;(condition-case ()
;;    (load ess-object-name-db-file)
;;  (error
;;   (message "%s does not exist.  Consider running ess-create-object-name-db."
;;	    ess-object-name-db-file)
;;   (ding)
;;   (sit-for 1)))

(defvar ess-loop-timeout 50000
  "Integer specifying how many loops ess-mode will wait for the prompt for
before signaling an error.")

(make-variable-buffer-local 'ess-loop-timeout)
(setq-default ess-loop-timeout 100000)

;;*;; Font-lock patterns

(defvar inferior-ess-font-lock-keywords
 '(("^[>+]" . font-lock-keyword-face)	; prompt
   ("^[>+]\\(.*$\\)" (1 font-lock-variable-name-face keep t)) ; input
   ("<-\\|_" . font-lock-reference-face)		; assign
   ("^\\*\\*\\\*.*\\*\\*\\*\\s *$" . font-lock-comment-face) ; ess-mode msg
   ("\\[,?[1-9][0-9]*,?\\]" . font-lock-reference-face)	; Vector/matrix labels
   ("\\<\\(TRUE\\|FALSE\\|T\\|F\\|NA\\|NULL\\|Inf\\|NaN\\)\\>"
    . font-lock-type-face) ; keywords
   )
 "Font-lock patterns used in inferior-ess-mode buffers.")


;;;*;;; ess-help variables

 ; ess-help-mode
;; This will never need to be loaded independently of any of the other
;; modules, but they can all call it so we may as well put it here.

;;*;; Variables relating to ess-help-mode

(defconst ess-help-S-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:") (?B . "BUGS:")
    (?d . "DETAILS:") (?D . "DESCRIPTION:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:") (?o . "OPTIONAL ARGUMENTS:") (?r . "REQUIRED ARGUMENTS:")
    (?R . "REFERENCES:")
    (?s . "SIDE EFFECTS:") (?S . "SEE ALSO:") (?u . "USAGE:") (?v . "VALUE:"))
  "Alist of (key . string) pairs for use in section searching.")
;;; `key' indicates the keystroke to use to search for the section heading
;;; `string' in an S help file. `string' is used as part of a
;;; regexp-search, and so specials should be quoted.

(defconst ess-help-R-sec-keys-alist 
  '((?a . "\\s *Arguments:") 
    (?d . "\\s *Description:")
    (?n . "\\s *Note:")
    (?r . "\\s *References:") 
    (?v . "\\s *Value[s]?")	;
    (?s . "\\s *See Also:") 
    (?e . "\\s *Examples:") 
    )) ;; "Alist of (key . string) pairs for use in section searching."

(defconst ess-help-S-sec-regex "^[A-Z. ---]+:$"
  "Reg(ular) Ex(pression) of section headers in help file")
(defconst ess-help-R-sec-regex "^\\s *[A-Z[a-z. ---]+:$")



 ; User changeable variables
;;;=====================================================
;;; Users note: Variables with document strings starting
;;; with a * are the ones you can generally change safely, and
;;; may have to upon occasion.

(defvar ess-help-form 'separate-buffer
  "*Place to show help.   NOT IMPLEMENTED YET.
Choices are `separate-buffer', `s-process', `www'.  The latter uses
browse-url to find the location")

;; WWW Help NOT included yet.  Be patient.
(defvar ess-help-w3-url-prefix "http://www.stat.sc.edu/~rossini/R/"
  "*Head URL for finding function help")

(defvar ess-help-w3-url-funs "funs/"
  "Place to find functions")


 ; System variables
;;;=====================================================
;;; Users note: You will rarely have to change these
;;; variables.

;;*;; Variables relating to ess-help-mode

;;-- ess-help-S-.. and  ess-help-R-.. constants now  in   S.el (are used in ess-inf).

(defvar ess-help-sec-keys-alist nil
  "Alist of (key . string) pairs for use in section searching.")

(defvar ess-help-sec-regex nil
  "Reg(ular) Ex(pression) of section headers in help file")

(make-variable-buffer-local 'ess-help-sec-keys-alist)
(make-variable-buffer-local 'ess-help-sec-regex)


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


;;; This syntax table is required by ess-mode.el, ess-inf.el and
;;; ess-trans.el, so we provide it here.
(defvar ess-mode-syntax-table nil "Syntax table for ess-mode.")
(if ess-mode-syntax-table
    nil
  (setq ess-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" ess-mode-syntax-table)
  (modify-syntax-entry ?+  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?-  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?=  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?%  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?<  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?>  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?&  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?|  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" ess-mode-syntax-table)
  (modify-syntax-entry ?#  "<"  ess-mode-syntax-table) ; open comment
  (modify-syntax-entry ?\n ">"  ess-mode-syntax-table) ; close comment
  (modify-syntax-entry ?.  "w"  ess-mode-syntax-table) ; used in S obj names
  (modify-syntax-entry ?$  "_"  ess-mode-syntax-table) ; foo.bar$hack is 1 symbol
  (modify-syntax-entry ?_  "."  ess-mode-syntax-table)  
  (modify-syntax-entry ?*  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?<  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?>  "."  ess-mode-syntax-table)
  (modify-syntax-entry ?/  "."  ess-mode-syntax-table))

 ; ess-transcript-mode

(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files" t)

(fset 's-transcript-mode 'ess-transcript-mode)
(fset 'S-transcript-mode 'ess-transcript-mode)

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

(defvar ess-customize-alist nil
  "Variable settings to use for proper behavior.")

;;(make-variable-buffer-local 'ess-customize-alist)
;;(setq-default ess-customize-alist nil)

(defun ess-set-vars (var-alist &optional buf) 
  "Set language variables from alist, in buffer `buf', if desired.
This is SO UGLY.  But it'll work for now... 
the basic idea: (setq ---  (cdr (assq --- var-alist)))."

  (if buf (set-buffer buf))
  ;;(setq-default ess-customize-alist var-alist)
  (setq ess-proc-prefix  (cdr (assq 'ess-proc-prefix var-alist)))
  (setq ess-version-running (cdr (assq 'ess-version-running var-alist)))
  (setq inferior-ess-program (cdr (assq 'inferior-ess-program var-alist)))
  (setq inferior-ess-objects-command (cdr (assq 'inferior-ess-objects-command var-alist)))
  (setq ess-help-sec-regex (cdr (assq 'ess-help-sec-regex var-alist)))
  (setq ess-help-sec-keys-alist (cdr (assq 'ess-help-sec-keys-alist var-alist)))
  (setq inferior-ess-help-command (cdr (assq 'inferior-ess-help-command var-alist)))
  (setq inferior-ess-exit-command (cdr (assq 'inferior-ess-exit-command var-alist)))
  (setq ess-loop-timeout (cdr (assq 'ess-loop-timeout var-alist)))
  (setq inferior-ess-primary-prompt (cdr (assq 'inferior-ess-primary-prompt var-alist)))
  (setq ess-history-file (concat "." ess-proc-prefix "history"))

  (message "(ess-set-vars): ess-proc-prefix=%s" ess-proc-prefix))


(defun ess-set-vars-default (var-alist &optional buf) 
  "Set language variables from alist, in buffer `buf', if desired.
This is SO UGLY.  But it'll work for now... 
the basic idea: (setq ---  (cdr (assq --- var-alist)))."

  (if buf (set-buffer buf))
  ;;(setq-default ess-customize-alist var-alist)
  (setq-default	 ess-proc-prefix              (cdr (assq 'ess-proc-prefix              var-alist)))
  (setq-default	 ess-version-running          (cdr (assq 'ess-version-running          var-alist)))
  (setq-default	 inferior-ess-program         (cdr (assq 'inferior-ess-program         var-alist)))
  (setq-default	 inferior-ess-objects-command (cdr (assq 'inferior-ess-objects-command var-alist)))
  (setq-default	 ess-help-sec-regex           (cdr (assq 'ess-help-sec-regex           var-alist)))
  (setq-default	 ess-help-sec-keys-alist      (cdr (assq 'ess-help-sec-keys-alist      var-alist)))
  (setq-default	 inferior-ess-help-command    (cdr (assq 'inferior-ess-help-command    var-alist)))
  (setq-default	 inferior-ess-exit-command    (cdr (assq 'inferior-ess-exit-command    var-alist)))
  (setq-default	 ess-loop-timeout             (cdr (assq 'ess-loop-timeout             var-alist)))
  (setq-default	 inferior-ess-primary-prompt  (cdr (assq 'inferior-ess-primary-prompt  var-alist)))
  (setq-default  ess-history-file (concat "." ess-proc-prefix "history"))

  (message "(ess-set-vars): ess-proc-prefix=%s" ess-proc-prefix))

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
