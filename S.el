;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : S.el
;;;; Authors         : Doug Bates
;;;;                 : Ed Kademan
;;;;                 : Frank Ritter
;;;;                 : David Smith
;;;; Created On      : October 14, 1991
;;;; Last Modified By: David Smith
;;;; Last Modified On: Mon Jun 29 15:04:26 CST 1992
;;;; Version         : 3.41
;;;; 
;;;; Lisp-dir-entry  : S-mode|
;;;;                   Doug Bates, Ed Kademan, Frank Ritter, David Smith|
;;;;                   dsmith@stats.adelaide.edu.au|
;;;;                   Interface to the S/Splus statistical software packages|
;;;;                   92-06-29|
;;;;                   3.4|
;;;;                   /attunga.stats.adelaide.edu.au:pub/S-mode/S-mode3.4.tar.Z
;;;;
;;;; PURPOSE
;;;; 	Interface to the S/Splus statistical software packages
;;;; 
;;;; Copyright 1989,1991,1992 Doug Bates    bates@stat.wisc.edu
;;;;                          Ed Kademan    kademan@stat.wisc.edu
;;;;                          Frank Ritter  ritter@psy.cmu.edu
;;;;                                            (or  @cs.cmu.edu)
;;;;                          David Smith   dsmith@stats.adelaide.edu.au
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The Changelog is at the end of this file.

;;; CREDITS.
;;; Thanks to shiba@shun.isac.co.jp (Ken'ichi "Modal" Shibayama) for
;;;   the indenting code.
;;; Thanks also to maechler@stat.math.ethz.ch (Martin Maechler) for
;;;   suggestions and bug fixes.
;;; S-eval-line-and-next-line is based on a function by Rod Ball 
;;;   (rod@marcam.dsir.govt.nz)
;;;
;;; Also thanks from David Smith to the previous authors for all their
;;; help and suggestions.

;;; BRIEF OVERVIEW
;;; Supports stuctured editing of S (a statistics package)
;;; functions that is integrated with a running S process in a
;;; buffer.  

;;; GENERAL DISCLAIMER
;;; 
;;; This program is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation; either
;;; version 1, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;; 
;;; You should have received a copy of the GNU General Public
;;; License along with this program; if not, write to the Free
;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;; 
;;; In short: you may use this code any way you like, as long as you
;;; don't charge money for it, remove this notice, or hold anyone liable
;;; for its results.

;;; OVERVIEW OF S MODE
;;; 
;;; S is a statistics package available from Bell Labs
;;; particularly suited for descriptive and exploratory
;;; statistics.  s-mode is built on top of comint (the general
;;; command interpreter mode written by Olin Shivers), and so
;;; comint.el (or comint.elc) should be either loaded or in your
;;; load path when you invoke it.
;;; 
;;; Aside from the general features offered by comint such as
;;; command history editing and job control, inferior S mode
;;; allows you to dump and load S objects into and from external
;;; files, and to display help on functions.  It also provides
;;; name completion while you do these.  For more detailed
;;; information see the documentation strings for S,
;;; inferior-S-mode, S-mode, and comint-mode.  There are also
;;; many variables and hooks available for customizing (see
;;; the variables below that have document strings that start
;;; with an "*").

;;; INSTALLATION
;;; Save this file in an appropriate directory and put the following
;;; line in your .emacs:
;;; 
;;;     (autoload 'S "~/elisp/S" "" t)
;;; 
;;; where "~/elisp/S.el" is the path name of this file.  That
;;; way, all you will have to do to get S running is to type
;;; "M-x S" from within emacs. You may also want to change some
;;; options, by putting lines such as the following in your .emacs:
;;; 
;;;     (setq inferior-S-program "S") ; command to run S
;;;	(setq S-version-running "2.3") ; Running the old version
;;;	(setq S-ask-about-display t) ; Ask for an X-display
;;;     (setq S-source-directory
;;;	  (expand-file-name "~/S-Src/"))
;;;	(setq S-keep-dump-files t)
;;;                  ; Make a directory of backup object source files
;;;
;;; See the section "User changeable variables" below for more options.

;;; GETTING LATER RELEASES OF S MODE
;;; The latest version is available from statlib by sending a
;;; blank message with subject "send index from S" to
;;; statlib@stat.cmu.edu, and following the directions from
;;; there.  Comint is probably already available at your site, 
;;; and already in your load path.  If it is not, you can get it
;;; from archive.cis.ohio-state.edu (login anonymous, passwd id)
;;; in directory /pub/gnu/emacs/elisp-archive/as-is/comint.el.Z
;;; This version has been tested and works with (at least) 
;;; comint-version 2.03.  You probably have copies of comint.el 
;;; on your system.  Copies are also available from ritter@cs.cmu.edu,
;;; and shivers@cs.cmu.edu.
;;;
;;; S-mode is also available for anonymous FTP from
;;; attunga.stats.adelaide.edu.au in the directory pub/S-mode. It is
;;; alsa avaliable from the Emacs-lisp archive on
;;; archive.cis.ohio-state.edu.

;;; RELEASE 2.1 INFORMATION
;;;
;;; Improvements since last release (unnumbered of Summer 1990):
;;; * Better description provided of functions loaded.
;;; * Better header for this file.
;;; * S-directory is now a prescriptive rather than just 
;;;   descriptive variable.  
;;; * better syntax table, so |#; are better recognized and
;;;   commands using them work better.
;;; * we have a version number.
;;;
;;; RELEASE 3.4 INFORMATION
;;; 
;;; * Works with version 3.0 S
;;; * Command-line completion of S object names
;;; * Recognition of attached data frames 
;;; * Dedicated S Help mode
;;; * Tek graphics support
;;; * Several bugfixes and code cleanups
;;; * Texinfo documentation
;;;
;;; Remaining Bugs:
;;; 
;;; * It would be nice to use .Last.value when running S+
;;; * It would be nice to use S VERSION when running S+
;;; Until the end of August 1992, please report bugs to me at
;;; dsmith@stats.adelaide.edu.au. After this date, mail to that address
;;; will not be answered for some time; please contact Frank Ritter
;;; (Frank_Ritter@SHAMO.SOAR.CS.CMU.EDU) or any of the other authors then
;;; (please CC: to me as well though -- you never know, I might just
;;; answer!) Comments, suggestions, words of praise and large cash
;;; donations are also more than welcome.

;;; Inits and provides
;;;=====================================================
;;;

(require 'comint)
(require 'comint-extra)
(autoload 'comint-isearch "comint-isearch" 
	  "Isearch for comint [full documentation when loaded]" t)
(provide 'S)

(defconst S-mode-version "3.41" 
  "Version of S-mode currently loaded.")

;; this will appear for just a short while, but it's a
;; chance to teach...
(message 
 (concat (substitute-command-keys
	  "Type \\[describe-mode] for help on S-mode version ")
	 S-mode-version))



;;; User changeable variables
;;;=====================================================
;;; Users note: Variables with document strings starting
;;; with a * are the ones you can generally change safely, and
;;; may have to upon occasion.

;;; System dependent variables

(defvar inferior-S-program "Splus"
  "*Program name for invoking an inferior S.")

(defvar inferior-Splus-args nil
  "*String of arguments passed to the S process on startup if the name of
the S program is `Splus'.")

(defvar S-version-running "3.0"
  "Version of S being run.")
;;; The value of this variable affects the
;;; default values of the following variables:
;;; 
;;;	 inferior-S-help-command
;;;	 inferior-S-search-list-command
;;;	 S-dump-error-re
;;; 
;;; Modifications to these variables are made at *load* time (provided, of
;;; course, they have not already been given values), hence changing the
;;; value of S-version-running after this package is loaded will have no
;;; effect.
;;; 
;;; Currently the string \"3.0\" is the only value of this variable with
;;; any real meaning; in this case the defaults are set to comply with the
;;; August '91 (3.0) version of S/Splus, defaults which also work for
;;; version 2.3. Any other value than \"3.0\" sets the defaults to comply
;;; with the 1988 version of S/Splus.")
;;;
;;; Please reserve the following values as special:
;;;   "3.0"    Version 3.0 (August '91) of S/Splus
;;;   "2.3"    Version 2.3 of S/Splus
;;;   "old"    Any older version

(defvar S-plus (assoc inferior-S-program '(("Splus") ("S+")))
  "Set to t if Splus is being used instead of vanilla S")
;;; Used for setting default values of other variables, and hence
;;; has no effect after S.el has been loaded.

(defvar inferior-S-prompt "\\(\\+\\|[a-zA-Z0-9() ]*>\\) *"
  "The regular expression inferior S mode uses for recognizing prompts
Do not anchor to bol with `^'.")

(defvar inferior-S-primary-prompt "[a-zA-Z0-9() ]*> *"
  "Regular expression used by S-mode to detect the primary prompt.
Do not anchor to bol with `^'.")

;;; Initialising the environment

(defvar S-ask-for-S-directory t
  "*If non-nil, the process directory will be requested each time S is run")

(defvar S-ask-about-display nil
  "*If non-nil, asks for a value for the DISPLAY environment
variable, to make X-windows work with S")

(defvar X-displays-list '("unix:0.0")
  "List of strings that are candidates for the DISPLAY environment variable.")

(defvar S-directory (file-name-as-directory (getenv "HOME"))
  "*The directory S is run from.  It must end in a slash.
Provided as a default if S-ask-for-S-directory is non-nil.")

;;; Editing functions

(defvar S-insert-function-templates t
  "*Boolean flag specifying action when editing a non-existent object.
If t, then when the text of a dumped object contains S-dumped-missing-re,
then it will be replaced by S-function-template.")

  ;; By K.Shibayama 5.14.1992

(defvar S-indent-level 2
  "*Indentation of S statements with respect to containing block.")

(defvar S-brace-imaginary-offset 0
  "*Imagined indentation of a S open brace that actually follows a statement.")

(defvar S-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")

(defvar S-continued-statement-offset 2
  "*Extra indent for lines not starting new statements.")

(defvar S-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to S-continued-statement-offset.")

(defvar S-arg-function-offset 2
  "*Extra indent for internal substatements of function `foo' that called
in `arg=foo(...)' form. 
If not number, the statements are indented at open-parenthesis following foo.")

(defvar S-expression-offset 4
  "*Extra indent for internal substatements of `expression' that specified
in `obj <- expression(...)' form. 
If not number, the statements are indented at open-parenthesis following 
`expression'.")

(defvar S-auto-newline nil
  "*Non-nil means automatically newline before and after braces
inserted in S code.")

(defvar S-tab-always-indent t
  "*Non-nil means TAB in S mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")
   
(defvar S-default-style 'GNU
  "*The default value of S-style")

(defvar S-style S-default-style
  "*The buffer specific S indentation style.")

;;; Dump files

(defvar S-source-directory "/tmp/"
  "*Directory in which to place dump files.  
The directory generated by S-source-directory-generator (if it is
non-nil) is used preferentially, and the value of S-source-directory
is used only of the generated directory can not be written or
created.")

(defvar S-source-directory-generator nil
  "*Function which, when called with no args, will return a directory
name (ending in a slash) into which S objects should be dumped. If this is
nil of the directory does not exist and cannot be created, the value of
S-source-directory is used.")
;;; Possible value:
;;; '(lambda () (file-name-as-directory 
;;;	      (expand-file-name (concat (car S-search-list) "/.Src"))))
;;; This always dumps to a sub-directory (".Src") of the current S
;;; working directory (i.e. first elt of search list)

(defvar S-dump-filename-template (concat (user-login-name) ".%s.S")
  "*Template for filenames of dumped objects.
%s is replaced by the object name.")
;;; This gives filenames like `user.foofun.S', so as not to clash with
;;; other users if you are using a shared directory. Other alternatives:
;;; "%s.S" ; Don't bother uniquifying if using your own directory(ies)
;;; "dump" ; Always dump to a specific filename. This makes it impossible
;;;          to edit more than one object at a time, though.
;;; (make-temp-name "scr.") ; Another way to uniquify

(defvar S-keep-dump-files nil
  "*If nil, delete dump files ater use. Otherwise, never delete.")
;;; Boolean flag which determines what to do with the dump files
;;; generated by \\[S-dump-object-into-edit-buffer], as follows:
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

(defvar S-function-template " function( )\n{\n\n}\n"
  "Function template used when editing nonexistent objects. 
The edit buffer will contain the object name in quotes, followed by
\"<-\", followed by this string.")

;;; Interacting with the S process

(defvar S-execute-in-process-buffer nil
  "*If non-nil, the S-execute- commands output to the process buffer.
Otherwise, they get their own temporary buffer.")

(defvar S-eval-visibly-p nil
  "*If non-nil, the S-eval- commands display the text to be evaluated 
in the process buffer.")

(defvar S-tek-mode nil
  "*Grab Tek Graphics?
Toggle with \\[S-tek-mode-toggle].")

(defvar S-tek-possible-graph-prompts "Selection: "
  "Prompts that might follow TEK graphics. 
If S mode seems to lock up when grabbing graphics, it probably means
you need something else in here. Your prompt is assumed: you don't
need to include it. Separate options with \\|")

(defvar S-tek-pause-for-graphics (not (string= (getenv "TERM") "xterm"))
  "If t, wait for a key to be pressed before returning to text mode.
Use this option when graphics and text share the same screen.")

;;; Help mode

(defvar S-help-sec-keys-alist 
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

;;; Hooks

(defvar S-mode-hook '()
  "*Hook for customizing S mode each time it is entered.")

(defvar S-mode-load-hook '()
  "*Hook to call when S.el is loaded.")

(defvar S-pre-run-hook nil
  "*Hook to call before starting up S.
Good for setting up your directory.")
;; You can put something like:
;; (setq S-directory (file-name-as-directory (concat (getenv "HOME") "/S")))
;; in your ~/.emacs file and S will always start up in your ~/S directory.
;; Alternatively, you can get S to start up in the directory you start 
;; Emacs from by putting this in your .emacs:
;; (setq S-pre-run-hook '((lambda () (setq S-directory default-directory))))



;;; System variables
;;;=====================================================
;;; Users note: You will rarely have to change these 
;;; variables.

(defvar S-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|collection(\\|library(\\)"
  "The regexp for matching the S commands that change the search path.")

(defvar S-function-pattern
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

(defvar S-source-modes '(S-mode)
  "A list of modes used to determine if a buffer contains S source code.")
;;; If a file is loaded into a buffer that is in one of these major modes, it
;;; is considered an S source file.  The function S-load-file uses this to
;;; determine defaults.

(defvar inferior-S-load-command "source(\"%s\")\n"
  "Format-string for building the S command to load a file.")
;;; This format string should use %s to substitute a file name
;;; and should result in an S expression that will command the inferior S
;;; to load that file.

(defvar inferior-S-dump-command "dump(\"%s\",file=\"%s\")\n"
  "Format-string for building the S command to dump an object into a file.")
;;; Use first %s to substitute an object name
;;;     second %s substitutes the dump file name.

(defvar inferior-S-help-command 
  (if S-plus
      "help(\"%s\",pager=\"cat\",window=F)\n" 
    "help(\"%s\")\n")
  "Format-string for building the S command to ask for help on an object.")
;;; This format string should use %s to substitute an object name.

(defvar inferior-S-search-list-command "search()\n"
  "S command that prints out the search list.")
;;; i.e. The list of directories and (recursive) objects that S uses when
;;; it searches for objects.

(defvar inferior-S-names-command "names(%s)\n"
  "Format string for S command to extract names from an object.")
;;; %s is replaced by the object name -- usually a list or data frame

(defvar inferior-S-objects-command 
  (if (string= S-version-running "3.0")
      "objects(%d)"
    "ls()")
  "Format string for S command to get a list of objects at position %d")
;;; Don't include a newline at the end! Used in S-execute-objects

(defvar S-dumped-missing-re "\nDumped\n\\'"
  "If a dumped object's buffer matches this re, then it is replaced
by S-function-template.")

(defvar S-dump-error-re 
  (if (string= S-version-running "3.0") "\nDumped\n\\'" "[Ee]rror")
  "Regexp used to detect an error when loading a file.")

(defvar S-error-buffer-name " *S-errors*"
  "Name of buffer to keep error messages in.")

(defvar S-loop-timeout 20000
  "Integer specifying how many loops S-mode will wait for the prompt for
before signalling an error.")

(defvar S-search-list nil
  "The list of directories and (recursive) objects to search for S objects.")

(defvar S-sl-modtime-alist nil
  "Alist of modtimes for all S directories accessed this session.")

(defvar S-sp-change nil
  "This symbol flags a change in the S search path.")

(defvar S-prev-load-dir/file nil
  "This symbol saves the (directory . file) pair used in the last
S-load-file command.  Used for determining the default in the next one.")

(defvar inferior-S-get-prompt-command "options()$prompt\n"
  "Command to find the value of the current S prompt.")

(defvar S-temp-buffer-p nil
  "*Flags whether the current buffer is a temporary buffer created by S-mode.
Such buffers will be killed by \\[S-quit] or \\[S-cleanup].
Source buffers and help buffers have this flag set.
This is a buffer-local variable.")
(make-variable-buffer-local 'S-temp-buffer-p)

(defvar S-local-variables-string "

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
")

(defvar S-style-alist 
'((GNU (S-indent-level . 2)
       (S-continued-statement-offset . 2)
       (S-brace-offset . 0)
       (S-arg-function-offset . 4)
       (S-expression-offset . 2))
  (BSD (S-indent-level . 8)
       (S-continued-statement-offset . 8)
       (S-brace-offset . -8)
       (S-arg-function-offset . 0)
       (S-expression-offset . 8))
  (K&R (S-indent-level . 5)
       (S-continued-statement-offset . 5)
       (S-brace-offset . -5)
       (S-arg-function-offset . 0)
       (S-expression-offset . 5))
  (C++ (S-indent-level . 4)
       (S-continued-statement-offset . 4)
       (S-brace-offset . -4)
       (S-arg-function-offset . 0)
       (S-expression-offset . 4)))
"Predefined formatting styles for S code")

(defvar S-tek-simple-prompt nil
  "Explicit version of primary S prompt.")



;;; S-mode helper functions and code
;;;=====================================================
;;;

(defvar inferior-S-mode-map nil)
(if inferior-S-mode-map
    nil
  (setq inferior-S-mode-map (full-copy-sparse-keymap comint-mode-map))
  (define-key inferior-S-mode-map "\r" 'inferior-S-send-input)
  (define-key inferior-S-mode-map "\eP" 'comint-msearch-input)
  (define-key inferior-S-mode-map "\eN" 'comint-psearch-input)
  (define-key inferior-S-mode-map "\C-c\C-b" 'comint-msearch-input-matching)
  (define-key inferior-S-mode-map "\eS" 'comint-next-similar-input)
  (define-key inferior-S-mode-map "\er" 'comint-isearch)
  (define-key inferior-S-mode-map "\C-c\C-l" 'S-load-file)
  (define-key inferior-S-mode-map "\C-x`" 'S-parse-errors)
  (define-key inferior-S-mode-map "\C-c\C-d" 'S-dump-object-into-edit-buffer)
  (define-key inferior-S-mode-map "\C-c\C-h" 'S-display-help-on-object)
  (define-key inferior-S-mode-map "\C-c\C-t" 'S-tek-mode-toggle)
  (define-key inferior-S-mode-map "\C-c\C-q" 'S-quit)
  (define-key inferior-S-mode-map "\C-c\C-e" 'S-execute)
  (define-key inferior-S-mode-map "\C-c\C-s" 'S-execute-search)
  (define-key inferior-S-mode-map "\C-c\C-x" 'S-execute-objects)
  (define-key inferior-S-mode-map "\C-c\C-a" 'S-execute-attach)
  (define-key inferior-S-mode-map "\C-c\C-z" 'S-abort)       ; these mask in
  (define-key inferior-S-mode-map "\C-c\C-o" 'S-kill-output) ; comint-m-map
  (define-key inferior-S-mode-map "\C-c\C-v" 'S-view-at-bottom)
  (define-key inferior-S-mode-map "\t" 'S-complete-object-name)) 

(defvar S-mode-syntax-table nil "Syntax table for S-mode.")
(if S-mode-syntax-table
    nil
  (setq S-mode-syntax-table (make-syntax-table c-mode-syntax-table))
  (modify-syntax-entry ?# "<" S-mode-syntax-table)  ; now an open comment
  (modify-syntax-entry ?\n ">" S-mode-syntax-table) ; close comment
  (modify-syntax-entry ?_ "." S-mode-syntax-table)  
  (modify-syntax-entry ?. "w" S-mode-syntax-table)  ; making S names same as
  (modify-syntax-entry ?$ "w" S-mode-syntax-table)  ; words makes coding easier
  (modify-syntax-entry ?* "." S-mode-syntax-table)
  (modify-syntax-entry ?< "." S-mode-syntax-table)
  (modify-syntax-entry ?> "." S-mode-syntax-table)
  (modify-syntax-entry ?/ "." S-mode-syntax-table))


(defvar inferior-S-mode-hook '()
  "*Hook for customizing inferior S mode.
Called after inferior-S-mode is entered and variables have been initialised.")


;;;
;;; Starting up
;;;

(defun S ()
  "Run an inferior S process, input and output via buffer *S*.
If there is a process already running in *S*, just switch to that buffer.
Takes the program name from the variable inferior-S-program.
The S program name is used to make a symbol name such as `inferior-S-args'.
If that symbol is a variable its value is used as a string of arguments
when invoking S.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive)
  (if (not (comint-check-proc "*S*"))
      (let* ((symbol-string
              (concat "inferior-" inferior-S-program "-args"))
             (switches-symbol (intern-soft symbol-string))
             (switches
              (if (and switches-symbol (boundp switches-symbol))
                  (symbol-value switches-symbol))))
        (run-hooks 'S-pre-run-hook)
	(if S-ask-for-S-directory (S-set-directory))
	(if S-ask-about-display (S-set-display))
	(set-buffer
         (if switches
             (inferior-S-make-comint switches)
           (inferior-S-make-comint)))
        (inferior-S-mode)
        (inferior-S-wait-for-prompt)
        (goto-char (point-max))
	(setq S-sl-modtime-alist nil)
	(S-tek-get-simple-prompt)
	(S-get-search-list)))
  (switch-to-buffer "*S*"))

(defun S-set-directory nil
  "Interactively set S-directory."
  (setq S-directory
	(expand-file-name
	 (file-name-as-directory
	  (read-file-name
	   "From which directory? " S-directory S-directory t)))))

(defun S-set-display nil
  "Interactively set DISPLAY variable for S process"
  (let* ((matches (append (mapcar
			   '(lambda (envelt)
			      (if (string-match "^DISPLAY=\\(.*\\)$" envelt)
				  (substring envelt (match-beginning 1) (match-end 1))))
			   process-environment)
			  (list (getenv "DISPLAY"))))
	 (initial (eval (cons 'or matches))))
    (setq process-environment 
	  (comint-update-env
	   process-environment
	   (list (concat
		  "DISPLAY="
		  (completing-read 
		   "Which X-display? "
		   (mapcar 'list X-displays-list)
		   nil
		   nil
		   initial)))))))

;;; define two commands consistent with other comint modes, run-s &
;;; run-S.
(fset 'run-s (fset 'run-S (symbol-function 'S)))

(defun inferior-S-mode () 
  "Major mode for interacting with an inferior S process.  
Runs an S interactive job as a subprocess of Emacs, with I/O through an
Emacs buffer.  Variable inferior-S-program controls which S
is run.

Commands are sent to the S process by typing them, and pressing
\\[inferior-S-send-input]. Pressing \\[S-complete-object-name] completes known
object names. Other keybindings for this mode are:

\\{inferior-S-mode-map}

When editing S objects, the use of \\[S-load-file] is advocated.
S-load-file keeps source files (if S-keep-dump-files is non-nil) in
the directory specified by S-source-directory-generator, with the
filename chosen according to S-dump-filename-template. When a file is
loaded, S-mode parses error messages and jumps to the appropriate file
if errors occur. The S-eval- commands do not do this.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-S-mode-hook (in that order).

You can send text to the inferior S process from other buffers containing
S source. The key bindings of these commands can be found by typing 
^h m (help for mode) in the other buffers.
    S-eval-region sends the current region to the S process.
    S-eval-buffer sends the current buffer to the S process.
    S-eval-function sends the current function to the S process.
    S-eval-line sends the current line to the S process.
    S-beginning-of-function and S-end-of-function move the point to
        the beginning and end of the current S function.
    S-switch-to-S switches the current buffer to the S process buffer.
    S-switch-to-end-of-S switches the current buffer to the S process
        buffer and puts point at the end of it.

    S-eval-region-and-go, S-eval-buffer-and-go,
        S-eval-function-and-go, and S-eval-line-and-go switch to the S
        process buffer after sending their text.

    S-dump-object-into-edit-buffer moves an S object into a temporary file
        and buffer for editing
    S-load-file sources a file of commands to the S process.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Crosshatches start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp (concat "^" inferior-S-prompt))
  (setq major-mode 'inferior-S-mode)
  (setq mode-name "Inferior S")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-S-mode-map)
  (set-syntax-table S-mode-syntax-table)
  (setq comint-input-sentinel 'S-search-path-tracker)
  (setq comint-get-old-input 'inferior-S-get-old-input)
  (make-local-variable 'scroll-step)
  (setq scroll-step 4)
  (make-local-variable 'input-ring-size)
  (setq input-ring-size 50)
  (run-hooks 'inferior-S-mode-hook))

;;; This function is a modification of make-comint from the comint.el
;;; code of Olin Shivers.
(defun inferior-S-make-comint (&rest switches)
  (let* ((name "S")
         (buffer (get-buffer-create (concat "*" name "*")))
         (proc (get-buffer-process buffer)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode. Otherwise, leave buffer and existing process alone.
    (cond ((or (not proc) (not (memq (process-status proc) '(run stop))))
           (save-excursion
             (set-buffer buffer)
             (setq default-directory S-directory)
             (comint-mode)) ; Install local vars, mode, keymap, ...
           (comint-exec buffer name inferior-S-program nil switches)))
    buffer))

(defun inferior-S-send-input ()
  "Sends the command on the current line to the S process."
  (interactive)
  (comint-send-input)
  (if (and S-sp-change
           (inferior-S-wait-for-prompt))
      (progn
        (S-get-search-list)
        (setq S-sp-change nil))
    ;; Is this TEK graphics output?
    (if S-tek-mode 
	(progn
	  (require 'S-tek)
	  (S-tek-snarf-graphics)))))

(defun inferior-S-get-old-input ()
  "Returns the S command surrounding point."
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at inferior-S-prompt))
	(S-error "No command on this line."))
    (if (looking-at inferior-S-primary-prompt) nil
	(re-search-backward (concat "^" inferior-S-primary-prompt)))
    (comint-skip-prompt)
    (let (command
	   (beg (point)))
      (end-of-line)
      (setq command (buffer-substring beg (point)))
      (forward-line 1)
      (while (and (looking-at inferior-S-prompt) 
		  (not (looking-at inferior-S-primary-prompt)))
	;; looking at secondary prompt
	(comint-skip-prompt)
	(setq beg (point))
	(end-of-line)
	(setq command (concat command " " (buffer-substring beg (point))))
	(forward-line 1))
      command)))

(defun S-error (msg)
  "Something bad has happened. Display the S buffer, and cause an error 
displaying MSG."
  (display-buffer (process-buffer (get-process "S")))
  (error msg))
		      
(defun inferior-S-wait-for-prompt ()
  "Wait until the S process is ready for input."
  (let* ((cbuffer (current-buffer))
         (sprocess (get-process "S"))
         (sbuffer (process-buffer sprocess))
         r
	 (timeout 0))
    (set-buffer sbuffer)
    (while (progn
	     (if (not (eq (process-status sprocess) 'run))
		 (S-error "S process has died unexpectedly.")
	       (if (> (setq timeout (1+ timeout)) S-loop-timeout)
		   (S-error "Timeout waiting for prompt. Check inferior-S-prompt or S-loop-timeout."))
	       (accept-process-output)
	       (goto-char (point-max))
	       (beginning-of-line)
	       (setq r (looking-at inferior-S-prompt))
	       (not (or r (looking-at ".*\\?\\s *"))))))
    (goto-char (point-max))
    (set-buffer cbuffer)
    (symbol-value r)))

(defun S-dump-object-into-edit-buffer (object)
  "Edit an S object in its own buffer.  Without a prefix argument,
this simply finds the file pointed to by S-dump-filename. If this file
does not exist, or if a prefix argument is given, a dump() command is
sent to the S process to generate the source buffer."
  (interactive (S-read-object-name "Object to edit: "))
  (let* ((filename (concat (if S-source-directory-generator 
			       (funcall S-source-directory-generator) 
			     S-source-directory)
			   (format S-dump-filename-template object)))
         (complete-dump-command (format inferior-S-dump-command
                                        object filename))
	 (old-buff (get-file-buffer filename)))
    (if S-source-directory-generator
	(let ((the-dir (file-name-directory filename)))
	  ;; If the directory doesn't exist, create if possible and approved.
	  (if (not (file-writable-p filename)) ; Can't create file
	      (if (and (not (file-exists-p the-dir)) ; No such directory
		       (file-writable-p	; Can we create dir in parent?
			(file-name-directory (directory-file-name the-dir)))
		       (y-or-n-p	; Approved
			(format "Directory %s does not exist. Create it? " the-dir))) ; and we want to create it
		  (make-directory (directory-file-name the-dir))
		(setq filename (concat S-source-directory 
				       (format S-dump-filename-template object)))))))
    ;; Try and find a buffer or filename before asking S
    (catch 'found-text
      (if (not current-prefix-arg)
	  (cond 
	   (old-buff 
	    (pop-to-buffer old-buff)
	    (message "Popped to edit buffer.")
	    (throw 'found-text nil))
	   ((file-exists-p filename) 
	    (find-file-other-window filename)
	    (message "Read %s" filename)
	    (throw 'found-text nil))))
      (S-command complete-dump-command)
      (let ((old-buff (get-file-buffer filename)))
	(if old-buff
	    (kill-buffer old-buff)))	;make sure we start fresh
      ;; Generate a buffer with the dumped data
      (find-file-other-window filename)
      (S-mode)
      (auto-save-mode 1)		; Auto save in this buffer
      (setq S-temp-buffer-p t)		; Flag as a temp buffer
      (if S-insert-function-templates
	  (progn 
	    (goto-char (point-max))
	    (if (re-search-backward S-dumped-missing-re nil t)
		(replace-match S-function-template t t))
	    (goto-char (point-min))))	;It might be nice to go between braces here
      ;; Insert the local variables stuff
      (save-excursion
	(goto-char (point-max))
	(insert S-local-variables-string)
	(if S-keep-dump-files nil
	  (set-buffer-modified-p nil)))
      (message "Dumped in %s" filename)
      (if S-keep-dump-files nil 
	  (delete-file filename))) ; In case buffer is killed
    (setq S-prev-load-dir/file
	  (cons (file-name-directory filename)
		(file-name-nondirectory filename)))))

(defun S-read-object-name (p-string)
  (let* ((default (S-read-object-name-default))
         (prompt-string (if default
                            (format "%s(default %s) " p-string default)
                          p-string))
         (S-object-list (S-get-object-list))
         (spec (completing-read prompt-string S-object-list)))
    (list (cond
           ((string= spec "") default)
           (t spec)))))

(defun S-read-object-name-default ()
 (save-excursion
   ;; The following line circumvents an 18.57 bug in following-char
   (if (eobp) (backward-char 1)) ; Hopefully buffer is not empty!
   ;; Get onto a symbol
   (catch 'nosym ; bail out if there's no symbol at all before point
     (while (/= (char-syntax (following-char)) ?w)
       (if (bobp) (throw 'nosym nil) (backward-char 1)))
     (let* 
	 ((end (progn (forward-sexp 1) (point)))
	  (beg (progn (backward-sexp 1) (point))))
       (buffer-substring beg end)))))

(defun S-object-names (dir)
  "Return alist of S object names in directory (or object) DIR"
  (if (string-match "^/" dir) 
      (mapcar 'list (directory-files dir))
    ;;It might be an object name; try to get names
    (let ((tbuffer (generate-new-buffer "names-list"))
	  (objname dir)
	  names)
      (save-excursion
	(set-buffer tbuffer)
	(buffer-flush-undo tbuffer)
	(S-command (format inferior-S-names-command objname) tbuffer)
	(goto-char (point-min))
	(if (not (looking-at "\\s-*\\[1\\]"))
	    (setq names nil)
	  (goto-char (point-max))
	  (while (re-search-backward "\"\\([^\"]*\\)\"" nil t)
	    (setq names (cons (buffer-substring (match-beginning 1)
						(match-end 1)) names))))
	(kill-buffer tbuffer))
      (mapcar 'list names))))

(defun S-resynch nil
"Reread all directories/objects in S-search-list to form completions."
 (interactive)
 (setq S-sl-modtime-alist nil)
 (S-get-search-list))
       
(defun S-extract-onames-from-alist (dir) 
"Extract the object names for directory (or object) DIR from S-sl-modtime-alist
generating a new set if the directory has been recently modified."
  (let* ((assoc-res (assoc dir S-sl-modtime-alist))
	 (data-cell (cdr assoc-res))
	 (last-mtime (car data-cell))
	 (new-mtime (S-dir-modtime dir))
	 (old-objs (cdr data-cell)))
    (if (equal new-mtime last-mtime) old-objs
      (setcar data-cell new-mtime)
      (setcdr data-cell (S-object-names dir)))))

(defun S-dir-modtime (dir)
"Return the last modtime if dir is a directory, and nil otherwise."
;; Attached dataframes return a modtime of nil. It probably wouldn't be
;; too difficult to find the modtime of the actual object by searching for 
;; it along S-search-list, but one hardly ever modifies dataframes after
;; they're attached, and I couldn't be bothered anyway.
  (if (string-match "^/" dir) 
      (nth 5 (file-attributes dir))))

(defun S-get-search-list ()
  "Get the list of directories and (recursive) objects that S searches
when it looks for objects."
  (save-excursion
  (let ((tbuffer (generate-new-buffer "search-list"))
	dir-assoc
        dir)
    (setq S-search-list nil)
    (buffer-flush-undo tbuffer)
    (set-buffer tbuffer)
    (S-command inferior-S-search-list-command tbuffer)
    (goto-char (point-max))
    (while (re-search-backward "\"\\([^\"]*\\)\"" nil t)
      (setq dir (buffer-substring (match-beginning 1) (match-end 1)))
      (if (and (string-match "^[^/]" dir)
	       (file-directory-p (concat S-directory dir)))
          (setq dir (concat S-directory dir)))
      (setq S-search-list (cons dir S-search-list))
      (setq dir-assoc (assoc dir S-sl-modtime-alist))
      (if (not dir-assoc)
	  (let (conselt)
	    (setq conselt (cons dir
				(cons (S-dir-modtime dir)
				      (S-object-names dir))))
	    (setq S-sl-modtime-alist (cons conselt S-sl-modtime-alist)))))
    (kill-buffer tbuffer))))

(defun S-get-object-list ()
  "Return the alist of current S object names."
;;; suitable for use with completing-read
  (S-get-object-list-r S-search-list))

(defun S-get-object-list-r (s-list)
  "Return the alist of current S object names, recursive version.
S-LIST is the search list of directories (or objects) for S." 
  (let* ((dir (car s-list))
         (dir-list (cdr s-list)))
    (if (null dir)
        nil
      (append (S-extract-onames-from-alist dir)
              (S-get-object-list-r dir-list)))))

(defun S-command (com &optional buf visible)
  "Send the S process command COM and delete the output
from the S process buffer.  If an optional second argument BUF exists
save the output in that buffer. If optional third arg VISIBLE is
non-nil, both the command and the output appear in the S process
buffer."
  (let* ((cbuffer (current-buffer))
         (sprocess (get-process "S"))
         sbuffer
	 start-of-output
	 point-holder)
    (if sprocess nil (error "No S process running!"))
    (setq sbuffer (process-buffer sprocess))
    (set-buffer sbuffer)
    (setq point-holder (point-marker))
    (goto-char (marker-position (process-mark sprocess)))
    (beginning-of-line)
    (if (looking-at inferior-S-primary-prompt) nil
      (goto-char (marker-position point-holder))
      (S-error 
       "S process not ready. Finish your command before trying again."))
    (if visible
	(progn
	  (goto-char (marker-position (process-mark sprocess)))
	  (insert-before-markers com) ))
    (setq start-of-output (marker-position (process-mark sprocess)))
    (process-send-string sprocess com)
    (while (progn
             (accept-process-output sprocess)
             (goto-char (marker-position (process-mark sprocess)))
             (beginning-of-line)
	     (if (< (point) start-of-output) (goto-char start-of-output))
	     (not (looking-at inferior-S-primary-prompt))))
    (if buf
        (append-to-buffer buf start-of-output (point)))
    (if visible (goto-char (marker-position (process-mark sprocess)))
      (delete-region start-of-output
		     (marker-position (process-mark sprocess)))
      (goto-char (marker-position point-holder)))
    (set-buffer cbuffer)))

(defun S-eval-visibly (text &optional invisibly)
  "Evaluate TEXT in the S process buffer as if it had been typed in.
If optional secod arg INVISIBLY is non-nil, don't echo commands. If 
if is a string, just include that string.
Waits for prompt after each line of input, so won't break on large texts."
  (let* ((cbuffer (current-buffer))
         (sprocess (get-process "S"))
         (sbuffer (process-buffer sprocess))
	 start-of-output
	 com pos)
    (set-buffer sbuffer)
    (goto-char (marker-position (process-mark sprocess)))
    (setq comint-last-input-end (point-marker))
    (if (stringp invisibly)
	(insert-before-markers (concat "*** " invisibly " ***\n")))
    (while (> (length text) 0)
      (setq pos (string-match "\n\\|$" text))
      (setq com (concat (substring text 0 pos) "\n"))
      (setq text (substring text (min (length text) (1+ pos))))
      (goto-char (marker-position (process-mark sprocess)))
      (if invisibly nil (insert-before-markers com))
      (setq start-of-output (marker-position (process-mark sprocess)))
      (process-send-string sprocess com)
      (while (progn
	       (accept-process-output sprocess)
	       (goto-char (marker-position (process-mark sprocess)))
	       (beginning-of-line)
	       (if (< (point) start-of-output) (goto-char start-of-output))
	       (not (looking-at inferior-S-prompt)))))
    (goto-char (marker-position (process-mark sprocess)))
    (set-buffer cbuffer)))

(defun S-execute (command &optional invert buff message)
  "Send a command to the S process.
A newline is automatically added to COMMAND. Prefix arg (or second arg INVERT)
means invert the meaning of S-execute-in-process-buffer. If INVERT is 'buffer,
output is forced to go to the process buffer.
If the output is going to a buffer, name it *BUFF*. This buffer is erased
before use. Optional fourth arg MESSAGE is text to print at the top of the
buffer (defaults to the command if BUFF is not given.)"
  (interactive "sCommand: \nP")
  (let ((the-command (concat command "\n"))
	(buff-name (concat "*" (or buff "S-output") "*"))
	(in-pbuff (if invert (or (eq invert 'buffer) 
				 (not S-execute-in-process-buffer))
		    S-execute-in-process-buffer)))
    (if in-pbuff 
	(S-eval-visibly the-command)
      (with-output-to-temp-buffer buff-name
	(if message (princ message)
	  (if buff nil
	      ;; Print the command in the buffer if it has not been
	      ;; given a special name
	    (princ "> ")
	    (princ the-command)))
	(S-command the-command (get-buffer buff-name) nil))
      (save-excursion
	(set-buffer (get-buffer buff-name))
	(setq S-temp-buffer-p t)))))

(defun S-execute-in-tb nil
  "Like S-execute, but always evaluates in temp buffer."
  (interactive)
  (let ((S-execute-in-process-buffer nil))
    (call-interactively 'S-execute)))

(defun S-execute-objects (posn)
  "Send the objects() command to the S process.
By default, gives the objects at position 1.
A prefix argument toggles the meaning of S-execute-in-process-buffer.
A prefix argument of 2 or more means get objects for that position.
A negative prefix argument gets the objects for that position
  and toggles S-execute-in-process-buffer as well."
  (interactive "P")
  (let* ((num-arg (if (listp posn) 
		      (if posn -1 1)
		    (prefix-numeric-value posn)))
	(the-posn (if (< num-arg 0) (- num-arg) num-arg))
	(invert (< num-arg 0))
	(the-command (format inferior-S-objects-command the-posn))
	(the-message (concat ">>> Position "
			     the-posn
			     " ("
			     (nth (1- the-posn) S-search-list)
			     ")\n")))
    (S-execute the-command invert "S objects" the-message)))

(defun S-execute-search (invert)
  "Send the search() command to the S process."
  (interactive "P")
  (S-execute "search()" invert "S search list"))

(defun S-execute-attach (dir &optional posn)
  "Attach a directory in the S process with the attach() command.
When used interactively, user is prompted for DIR to attach and
prefix argument is used for POSN (or 2, if absent.) 
Doesn't work for data frames."
  (interactive "DAttach directory: \nP")
  (S-execute (concat "attach(\"" 
		     (directory-file-name (expand-file-name dir))
		     "\""
		     (if posn (concat "," (prefix-numeric-value posn)))
		     ")") 'buffer))

(defun S-view-at-bottom ()
  "Move to the end of the buffer, and place cursor on bottom line of window."
  (interactive)
  (goto-char (point-max))
  (recenter -1))

(defun S-kill-output ()
  "Kill all output from last S command."
  ;; A version of comint-kill-output that doesn't nuke the prompt.
  (interactive)
  (let* ((sprocess (get-process "S"))
	(pmark (process-mark sprocess))
	(oldpoint (point-marker)))
    (goto-char pmark)
    (re-search-backward inferior-S-primary-prompt)
    (kill-region comint-last-input-end (point))
    (insert "*** output flushed ***\n")
    (goto-char oldpoint)
    (recenter -1)))

(defun S-load-file (filename)
  "Load an S source file into an inferior S process."
  (interactive (comint-get-source "Load S file: "
                                  S-prev-load-dir/file
                                  S-source-modes
                                  nil))
  (catch 'give-up	       	; In case we don't want to load after all
    (let ((buff (get-file-buffer filename))
	  tbuffer-p)
      (if buff		    	; Buffer exists
	  (save-excursion
	    (set-buffer buff)
	    (setq tbuffer-p S-temp-buffer-p)
	    (if (buffer-modified-p buff) ; Buff exists and has changed
		;; save BUFF, but don't make a backup
		;; if we're about to delete it
		(if tbuffer-p		; i.e. a result from a dump command
		    (save-buffer (if S-keep-dump-files 1 0))
		  ;; Better check if it's just any old buffer
		  (if (y-or-n-p (format "Buffer %s modified. Save it? "
					(buffer-name buff)))
		      (save-buffer)
		    ;; Maybe we should just give up here ...
		    (message
		     "Using current disk version (don't say I didn't warn you!)")))
	      ;; Buffer hasn't changed lately, might need to write
	      ;; it back if the file is gone
	      (if tbuffer-p
		  (if (y-or-n-p 
		       "Buffer hasn't changed. Really load it into S? ")
		      (if (file-exists-p (buffer-file-name buff)) nil
			(set-buffer-modified-p t) ; so save will work
			(save-buffer 0))
		    (message "No load performed.")
		    (throw 'give-up nil))))))
      (setq S-prev-load-dir/file
	    (cons (file-name-directory filename)
		  (file-name-nondirectory filename)))
      (let ((errbuffer (get-buffer-create S-error-buffer-name)))
	(save-excursion 
	  (set-buffer errbuffer)
	  (erase-buffer)
	  (S-command (format inferior-S-load-command filename) errbuffer)
	  (goto-char (point-max))
	  (if (re-search-backward S-dump-error-re nil t)
	      (progn
		(message "Errors: Use %s to find error." 
			 (substitute-command-keys 
			  "\\<inferior-S-mode-map>\\[S-parse-errors]"))
		;; This load failed, so set buffer as modified so the
		;; user will be warned if he tries to kill it
		(if buff
		    (progn
		      (set-buffer buff)
		      (set-buffer-modified-p t)))) 
	    (message "Load successful.")
	    (if (and tbuffer-p (not S-keep-dump-files)) 
		(delete-file filename)))))))
  (S-switch-to-S t))

(defun S-parse-errors (showerr)
  "Jump to error in last loaded S source file.
With prefix argument, only shows the errors S reported."
  (interactive "P")
  (let ((errbuff (get-buffer S-error-buffer-name)))
    (if (not errbuff)
	(error "You need to do a load first!")
      (set-buffer errbuff)
      (goto-char (point-max))
      (if 
	  (re-search-backward
	   "^\\(Syntax error: .*\\) at line \\([0-9]*\\), file \\(.*\\)$"
	   nil
	   t)
	  (let* ((filename (buffer-substring (match-beginning 3) (match-end 3))) 
		 (fbuffer (get-file-buffer filename)) 
		 (linenum (string-to-int (buffer-substring (match-beginning 2) (match-end 2))))
		 (errmess (buffer-substring (match-beginning 1) (match-end 1))))
	    (if showerr 
		(display-buffer errbuff)
	      (if fbuffer nil
		(setq fbuffer (find-file-noselect filename))
		(save-excursion
		  (set-buffer fbuffer)
		  (S-mode))) 
	      (pop-to-buffer fbuffer)
	      (goto-line linenum))
	    (princ errmess t))
	(message "Not a syntax error.")
	(display-buffer errbuff)))))

      
(defun S-search-path-tracker (str)
  "Check if input STR changed the search path."
;;; This function monitors user input to the inferior S process so that
;;; emacs can keep the S-search-list up to date.  Completing-read uses this
;;; list indirectly when it prompts for help or for an object to dump.
  (if (string-match S-change-sp-regexp str)
      (setq S-sp-change t)))

(defun S-cleanup ()
  "Delete all of S-mode's temporary buffers and files
(if S-keep-dump-files is nil) leaving you in the S process buffer.
Auto-save files and S help buffers are also deleted. Buffers whose
contents do not match with S's idea of the objects value *usually*
have the modified flag set, and you will be warned before such buffers
are killed. The exception to this is buffers which were saved in a
previous session before being loaded into S, and then read this
session.

It's a good idea to run this before you quit. It is run automatically by 
\\[S-quit]."
  (interactive)
  (if (yes-or-no-p "Delete all temporary files and buffers? ")
      (progn
	(mapcar '(lambda (buf)
		   (set-buffer buf)
		   (let ((fname (buffer-file-name buf))
			 (asfnm buffer-auto-save-file-name))
		     (if S-temp-buffer-p
			 (progn
			   (kill-buffer buf)
			   (if (or (buffer-name buf)
				   S-keep-dump-files)
			       ;; Don't do anything if buffer was not
			       ;; killed or dump files are kept
			       nil 
			     ;; if the file exists, it stays! (consider
			     ;; dumping an object with an existing file)
;;;				  (if (and fname (file-exists-p fname))
;;;				      (delete-file fname))
			     ;; Auto-save files can go, since they're
			     ;; only associated with modified buffers
			     (if (and asfnm (file-exists-p asfnm))
				 (delete-file asfnm)))))))
		(buffer-list))
	(S-switch-to-S nil))))

(defun S-quit ()
  "Issue the q() command to S, and clean up."
  (interactive)
  (let ((sprocess (get-process "S")))
    (if (not sprocess) (error "No S process running."))
    (if (yes-or-no-p "Really quit from S? ")
	(save-excursion 
	  (S-cleanup)
	  (S-switch-to-S nil)
	  (goto-char (marker-position (process-mark sprocess)))
	  (insert "q()\n")
	  (process-send-string sprocess "q()\n")))))

(defun S-abort ()
  "Kill the S process, without executing .Last or terminating devices.
If you want to finish your session, use \\[S-quit] instead."
;;; Provided as a safety measure over the default binding of C-c C-z in 
;;; comint-mode-map. 
  (interactive)
  (ding)
  (message "WARNING: q() will not be executed and graphics devices won't finish properly!")
  (sit-for 5)
  (if (yes-or-no-p "Still abort? ")
      (comint-quit-subjob)
    (message "Good move.")))
      


;;;
;;; Tek terminal Graphics support
;;;

(defun S-tek-mode-toggle nil
  "Toggle S-tek-mode.
Resets the S-tek-simple-prompt when S-tek-mode is turned on."
  (interactive)
  (message (if (setq S-tek-mode (not S-tek-mode))
	       "Tek mode is now ON." 
	     "Tek mode is now OFF."))
  (if S-tek-mode (S-tek-get-simple-prompt)))

(defun S-tek-get-simple-prompt nil
  "Find the exact version of the current prompt."
  (interactive)
  (let ((tbuffer (generate-new-buffer "*S-exact-prompt*")))
    (buffer-flush-undo tbuffer)
    (set-buffer tbuffer)
    (S-command inferior-S-get-prompt-command tbuffer)
    (goto-char (point-max))
    (re-search-backward "\"\\([^\"]*\\)\"" nil t)
    (setq S-tek-simple-prompt
	  (buffer-substring (match-beginning 1) (match-end 1)))
    (kill-buffer tbuffer)))

;;; 25/6/92 dsmith
;;; Rest of code moved to S-tek.el



;;; S mode
;;;======================================================
;;;

(defvar S-mode-map nil)
(if S-mode-map
    nil
  (setq S-mode-map (make-sparse-keymap))
  (define-key S-mode-map "\C-c\C-r"    'S-eval-region)
  (define-key S-mode-map "\C-c\M-r" 'S-eval-region-and-go)
  (define-key S-mode-map "\C-c\C-b"    'S-eval-buffer)
  (define-key S-mode-map "\C-c\M-b" 'S-eval-buffer-and-go)
  (define-key S-mode-map "\C-c\C-f"    'S-eval-function)
  (define-key S-mode-map "\C-c\M-f" 'S-eval-function-and-go)
  (define-key S-mode-map "\M-\C-x"  'S-eval-function)
  (define-key S-mode-map "\C-c\C-n"     'S-eval-line-and-next-line)
  (define-key S-mode-map "\C-c\C-j"    'S-eval-line)
  (define-key S-mode-map "\C-c\M-j" 'S-eval-line-and-go)
  (define-key S-mode-map "\M-\C-a"  'S-beginning-of-function)
  (define-key S-mode-map "\M-\C-e"  'S-end-of-function)
  (define-key S-mode-map "\C-c\C-y"    'S-switch-to-S)
  (define-key S-mode-map "\C-c\C-z" 'S-switch-to-end-of-S)
  (define-key S-mode-map "\C-c\C-l"    'S-load-file)
  (define-key S-mode-map "\C-c\C-h"    'S-display-help-on-object)
  (define-key S-mode-map "\C-c\C-d" 'S-dump-object-into-edit-buffer)
  (define-key S-mode-map "\C-c\C-e" 'S-execute-in-tb)
  (define-key S-mode-map "\M-\t"    'S-complete-object-name)
  (define-key S-mode-map "{" 'S-electric-brace)
  (define-key S-mode-map "}" 'S-electric-brace)
  (define-key S-mode-map "\e\C-h" 'S-mark-function)
  (define-key S-mode-map "\e\C-q" 'S-indent-exp)
  (define-key S-mode-map "\177" 'backward-delete-char-untabify)
  (define-key S-mode-map "\t" 'S-indent-command)
)

(defun S-mode ()
  "Major mode for editing S source.

\\{S-mode-map}

Customization: Entry to this mode runs the hooks in S-mode-hook.

You can send text to the inferior S process from other buffers containing
S source.
    S-eval-region sends the current region to the S process.
    S-eval-buffer sends the current buffer to the S process.
    S-eval-function sends the current function to the S process.
    S-eval-line sends the current line to the S process.
    S-beginning-of-function and S-end-of-function move the point to
        the beginning and end of the current S function.
    S-switch-to-S switches the current buffer to the S process buffer.
    S-switch-to-end-of-S switches the current buffer to the S process
        buffer and puts point at the end of it.

    S-eval-region-and-go, S-eval-buffer-and-go,
        S-eval-function-and-go, and S-eval-line-and-go switch to the S
        process buffer after sending their text.

    S-load-file sources a file of commands to the S process.
    S-make-function inserts a function template in the buffer.

\\[S-indent-command] indents for S code. 
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
Comments are indented in a similar way to Emacs-lisp mode:
       `###'     beginning of line
       `##'      the same level of indentation as the code
       `#'       the same column on the right, or to the right of such a
                 column if that is not possible.(default value 40). 
                 \\[indent-for-comment] command automatically inserts such a
                 `#' in the right place, or aligns such a comment if it is 
                 already inserted.
\\[S-indent-exp] command indents each line of the S grouping following point.

Variables controlling indentation style:
 S-tab-always-indent
    Non-nil means TAB in S mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 S-auto-newline
    Non-nil means automatically newline before and after braces inserted in S 
    code.
 S-indent-level
    Indentation of S statements within surrounding block.
    The surrounding block's indentation is the indentation of the line on 
    which the open-brace appears.
 S-continued-statement-offset
    Extra indentation given to a substatement, such as the then-clause of an 
    if or body of a while.
 S-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to S-continued-statement-offset.
 S-brace-offset
    Extra indentation for line if it starts with an open brace.
 S-arg-function-offset 
    Extra indent for internal substatements of function `foo' that called
    in `arg=foo(...)' form. 
   If not number, the statements are indented at open-parenthesis following 
   `foo'.
 S-expression-offset
    Extra indent for internal substatements of `expression' that specified
    in `obj <- expression(...)' form. 
    If not number, the statements are indented at open-parenthesis following 
    `expression'.
 S-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.

Furthermore, \\[S-set-style] command enables you to set up predefined S-mode 
indentation style. At present, predefined style are `BSD', `GNU', `K&R' `C++'
 (quoted from C language style)."
  (interactive)
  (setq major-mode 'S-mode)
  (setq mode-name "S")
  (use-local-map S-mode-map)
  (set-syntax-table S-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'S-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'S-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (run-hooks 'S-mode-hook))

;;; Emacs will set the mode for a file based on the file's header.
;;; The mode name is indicated by putting it between -*- on the top line. 
;;; (Other commands can go here too, see an Emacs manual.)
;;; For a file you also load, you will want a leading # (comment to S)
;;; Emacs will downcase the name of the mode, e.g., S, so we must provide
;;; s-mode in lower case too.  That is, "#-*-S-*-" invokes s-mode and not S-mode.
(fset 's-mode 'S-mode)

(defun S-eval-region (start end toggle &optional message)
  "Send the current region to the inferior S process.
With prefix argument, toggle meaning of S-eval-visibly-p."
  (interactive "r\nP")
  (let ((visibly (if toggle (not S-eval-visibly-p) S-eval-visibly-p)))
    (if visibly
	(S-eval-visibly (buffer-substring start end))
      (S-eval-visibly (buffer-substring start end)
		      (or message "Eval region")))))

(defun S-eval-region-and-go (start end vis)
  "Send the current region to the inferior S and switch to the process buffer.
Arg has same meaning as for S-eval-region."
  (interactive "r\nP")
  (S-eval-region start end vis)
  (S-switch-to-S t))

(defun S-eval-buffer (vis)
  "Send the current buffer to the inferior S process.
Arg has same meaning as for S-eval-region."
  (interactive "P")
  (S-eval-region (point-min) (point-max) vis "Eval buffer"))

(defun S-eval-buffer-and-go (vis)
  "Send the current buffer to the inferior S and switch to the process buffer.
Arg has same meaning as for S-eval-region."
  (interactive)
  (S-eval-buffer vis)
  (S-switch-to-S t))

(defun S-eval-function (vis)
  "Send the current function to the inferior S process.
Arg has same meaning as for S-eval-region."
  (interactive "P")
  (save-excursion
    (S-end-of-function)
    (let ((end (point)))
      (S-beginning-of-function)
      (princ (concat "Loading: " (S-extract-word-name)) t)
      (S-eval-region (point) end vis 
		     (concat "Eval function " (S-extract-word-name))))))

(defun S-eval-function-and-go (vis)
  "Send the current function to the inferior S process and switch to
the process buffer. Arg has same meaning as for S-eval-region."
  (interactive "P")
  (S-eval-function vis)
  (S-switch-to-S t))

(defun S-eval-line (vis)
  "Send the current line to the inferior S process.
Arg has same meaning as for S-eval-region."
  (interactive "P")
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (princ (concat "Loading line: " (S-extract-word-name) " ...") t)
      (S-eval-region (point) end vis "Eval line"))))

(defun S-eval-line-and-go (vis)
  "Send the current line to the inferior S process and switch to the
process buffer. Arg has same meaning as for S-eval-region."
  (interactive "P")
  (S-eval-line vis)
  (S-switch-to-S t))

(defun S-eval-line-and-next-line ()
  "Evaluate the current line visibly and move to the next line."
  ;; From an idea by Rod Ball (rod@marcam.dsir.govt.nz)
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (S-eval-visibly (buffer-substring (point) end))))
  (next-line 1))

(defun S-beginning-of-function nil
  "Leave the point at the beginning of the current S function."
  (interactive)
  (let ((init-point (point))
	beg end done)
    (if (search-forward "(" nil t) (forward-char 1))
    ;; in case we're sitting in a function header
    (while (not done)
      (if 
	  (re-search-backward S-function-pattern (point-min) t)
	  nil
	(goto-char init-point)
	(error "Point is not in a function."))
      (setq beg (point))
      (forward-list 1) ; get over arguments
      (forward-sexp 1) ; move over braces
      (setq end (point))
      (goto-char beg)
      ;; current function must begin and end around point  
      (setq done (and (>= end init-point) (<= beg init-point))))))

(defun S-end-of-function nil
  "Leave the point at the end of the current S function."
  (interactive)
  (S-beginning-of-function)
  (forward-list 1) ; get over arguments
  (forward-sexp 1) ; move over braces
  )

(defun S-extract-word-name ()
  "Get the word you're on."
  (save-excursion
    (re-search-forward "\\<\\w+\\>" nil t)
    (buffer-substring (match-beginning 0) (match-end 0))))

(defun S-switch-to-S (eob-p)
  "Switch to the inferior S process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (cond ((comint-check-proc "*S*")
         (pop-to-buffer "*S*")
         (cond (eob-p
                (goto-char (point-max)))))
        (t
         (message "No inferior S process")
         (ding))))

(defun S-switch-to-end-of-S nil
  "Switch to the end of the inferior S process buffer."
  (interactive)
  (S-switch-to-S t))

(defun S-make-function ()
  "Insert a function template."
  (interactive)
  (insert "fu <- function()\n{\n\t\n}\n")
  (forward-line -2)
  (end-of-line))

(defun S-complete-object-name (&optional listcomp)
  ;;Based on lisp-complete-symbol
  "Perform completion on S object preceding point.  The object is
compared against those objects known by S-get-object-list and any
additional characters up to ambiguity are inserted.  Completion only
works on globally-known objects (including elements of attached data
frames), and thus is most suitable for interactive command-line entry,
and not so much for function editing since local objects (e.g.
argument names) aren't known.

Use \\[S-resynch] to re-read the names of the attached directories.
This is done automatically (and transparently) if a directory is
modified, so the most up-to-date list of object names is always
available. However attached dataframes are *not* updated, so this
command may be necessary if you modify an attached dataframe.

If ARG is non-nil, no completion is attempted, but the available
completions are listed.

If the character proceding point is not a symbol element,
indent-for-tab-command is run."
  (interactive "P")
  (if (memq (char-syntax (preceding-char)) '(?w ?_)) 
      (let* ((end (point))
	     (buffer-syntax (syntax-table))
	     (beg (unwind-protect
		      (save-excursion
			(set-syntax-table S-mode-syntax-table)
			(backward-sexp 1)
			(point))
		    (set-syntax-table buffer-syntax)))
	     (full-prefix (buffer-substring beg end))
	     ;; See if we're indexing a list with `$'
	     (pattern full-prefix)
	     components
	     (listname (if (string-match "\\(.+\\)\\$\\(\\sw\\|\\s_\\)*$"
					 full-prefix) 
			   (progn
			     (setq pattern 
				   (if (not (match-beginning 2)) ""
				     (substring full-prefix
						(match-beginning 2)
						(match-end 2))))
			     (substring full-prefix (match-beginning 1)
					(match-end 1)))))
	     (completion (try-completion pattern
					 (if listname
					     (setq components
						   (S-object-names listname))
					   (S-get-object-list)))))
	(if listcomp (setq completion full-prefix))
	(cond ((eq completion t)
	       (message "[sole completion]"))
	      ((null completion)
	       (message "Can't find completion for \"%s\"" full-prefix)
	       (ding))
	      ((not (string= pattern completion))
	       (delete-region 
		(if listname (+ beg (length listname) 1) beg)
		end)
	       (insert completion))
	      (t 
	       (message "Making completion list...")
	       (let ((list (all-completions pattern
					    (if listname components
					      (S-get-object-list)))))
		 (with-output-to-temp-buffer " *Completions*"
		   (display-completion-list list)))
	       (message "Making completion list...%s" "done"))))
    (indent-for-tab-command)))

;;; S code formatting functions

(defun S-comment-indent ()
  (if (looking-at "###")
      (current-column)
    (if (looking-at "##")
	(let ((tem (S-calculate-indent)))
	  (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column))))

(defun S-electric-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if S-auto-newline (progn (S-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (S-indent-line)
	  (if S-auto-newline
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(S-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun S-indent-command (&optional whole-exp)
  "Indent current line as S code, or in some cases insert a tab character.
If S-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as S
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (S-indent-line))
	    beg end)
	(save-excursion
	  (if S-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (backward-up-list 1)
	  (forward-list 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt)))
    (if (and (not S-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (S-indent-line))))

(defun S-indent-line ()
  "Indent current line as S code.
Return the amount the indentation changed by."
  (let ((indent (S-calculate-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  (t
	   (skip-chars-forward " \t")
	   (if (looking-at "###")
	       (setq indent 0))
	   (if (and (looking-at "#") (not (looking-at "##")))
	       (setq indent comment-column)
	     (if (eq indent t) (setq indent 0))
	     (if (listp indent) (setq indent (car indent)))
	     (cond ((and (looking-at "else\\b")
			 (not (looking-at "else\\s_")))
		    (setq indent (save-excursion
				   (S-backward-to-start-of-if)
				   (current-indentation))))
		   ((= (following-char) ?})
		    (setq indent (- indent S-indent-level)))
		   ((= (following-char) ?{)
		    (setq indent (+ indent S-brace-offset)))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  
      ;; Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun S-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as S code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition,
	     0)   ; Unless it starts a function body
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char containing-sexp)
	     (let ((bol (save-excursion (beginning-of-line) (point))))
	       (cond ((and (numberp S-arg-function-offset)
			    (re-search-backward "=[ \t]*\\s\"*\\(\\w\\|\\s_\\)+\\s\"*[ \t]*" bol t))
		      (forward-sexp -1)
		      (+ (current-column) S-arg-function-offset))
		     ((and (numberp S-expression-offset)
			   (re-search-backward "<-[ \t]*expression[ \t]*" bol t))
		      (forward-sexp -1)
		      (+ (current-column) S-expression-offset))
		     (t
		      (progn (goto-char (1+ containing-sexp))
			     (current-column))))))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (S-backward-to-noncomment containing-sexp)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (eq (preceding-char) ?\,)
	       (S-backward-to-start-of-continued-exp containing-sexp)
	       (beginning-of-line)
	       (S-backward-to-noncomment containing-sexp))
	     ;; Now we get the answer.
	     (if (S-continued-statement-p)
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  S-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (S-backward-to-start-of-continued-exp containing-sexp)
		   (+ S-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (skip-chars-forward " \t")
					  (eq (following-char) ?{))
			  S-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   (while (progn (skip-chars-forward " \t\n")
				 (looking-at "#"))
		     ;; Skip over comments following openbrace.
		     (forward-line 1))
		   ;; The first following code counts
		   ;; if it is before the line we want to indent.
		   (and (< (point) indent-point)
			(current-column)))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open brace in column zero, don't let statement
		 ;; start there too.  If S-indent-level is zero,
		 ;; use S-brace-offset + S-continued-statement-offset instead.
		 ;; For open-braces not the first thing in a line,
		 ;; add in S-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop S-indent-level))
			(+ S-brace-offset S-continued-statement-offset)
		      S-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the S-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 S-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun S-continued-statement-p ()
  (let ((eol (point)))
    (save-excursion
      (cond ((memq (preceding-char) '(nil ?\, ?\; ?\} ?\{ ?\]))
	     nil)
	    ((bolp))
	    ((= (preceding-char) ?\))
	     (forward-sexp -2)
	     (looking-at "if\\b[ \t]*(\\|function\\b[ \t]*(\\|for\\b[ \t]*(\\|while\\b[ \t]*("))
	    ((progn (forward-sexp -1) 
		    (and (looking-at "else\\b\\|repeat\\b")
			 (not (looking-at "else\\s_\\|repeat\\s_"))))
	     (skip-chars-backward " \t")
	     (or (bolp)
		 (= (preceding-char) ?\;)))
	    (t
	     (progn (goto-char eol)
		    (skip-chars-backward " \t")
		    (or (and (> (current-column) 1)
			     (save-excursion (backward-char 1)
					     (looking-at "[-:+*/_><=]")))
			(and (> (current-column) 3)
			     (progn (backward-char 3)
				    (looking-at "%[^ \t]%"))))))))))

(defun S-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq stop (or (not (looking-at "#")) (<= (point) lim)))
      (if stop (goto-char opoint)
	(beginning-of-line)))))

(defun S-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun S-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (not (zerop if-level))
      (backward-sexp 1)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun S-mark-function ()
  "Put mark at end of S function, point at beginning."
  (interactive)
  (push-mark (point))
  (S-end-of-function)
  (push-mark (point))
  (S-beginning-of-function))

(defun S-indent-exp ()
  "Indent each line of the S grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	at-else at-brace
	(opoint (point))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (S-indent-line))
	  (if (nth 4 state)
	      (and (S-indent-line)
		   (setcar (nthcdr 4 state) nil)))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq innerloop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack (or (car (cdr state))
					(save-excursion (forward-sexp -1)
							(point)))))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		;; Lines inside parens are handled specially.
		(if (/= (char-after (car contain-stack)) ?{)
		    (setq this-indent (car indent-stack))
		  ;; Line is at statement level.
		  ;; Is it a new statement?  Is it an else?
		  ;; Find last non-comment character before this line
		  (save-excursion
		    (setq at-else (looking-at "else\\W"))
		    (setq at-brace (= (following-char) ?{))
		    (S-backward-to-noncomment opoint)
		    (if (S-continued-statement-p)
			;; Preceding line did not end in comma or semi;
			;; indent this line  S-continued-statement-offset
			;; more than previous.
			(progn
			  (S-backward-to-start-of-continued-exp (car contain-stack))
			  (setq this-indent
				(+ S-continued-statement-offset (current-column)
				   (if at-brace S-continued-brace-offset 0))))
		      ;; Preceding line ended in comma or semi;
		      ;; use the standard indent for this level.
		      (if at-else
			  (progn (S-backward-to-start-of-if opoint)
				 (setq this-indent (current-indentation)))
			(setq this-indent (car indent-stack))))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (S-calculate-indent
			   (if (car indent-stack)
			       (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (= (following-char) ?})
		(setq this-indent (- this-indent S-indent-level)))
	    (if (= (following-char) ?{)
		(setq this-indent (+ this-indent S-brace-offset)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(= (following-char) ?\#)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(if (re-search-forward comment-start-skip (save-excursion (end-of-line) (point)) t)
		    (progn (indent-for-comment) (beginning-of-line)))))))))
; (message "Indenting S expression...done")
  )

;; Predefined styles
(defun S-set-style (&optional style)
  "Set up the S-mode style variables from the S-style variable or if
  STYLE argument is given, use that.  It makes the S indentation style 
  variables buffer local."

  (interactive)

  (let ((S-styles (mapcar 'car S-style-alist)))
	
    (if (interactive-p)
	(setq style
	      (let ((style-string ; get style name with completion
		     (completing-read
		      (format "Set S mode indentation style to (default %s): "
			      S-default-style)
		      (vconcat S-styles)
		      (function (lambda (arg) (memq arg S-styles)))
		      )))
		(if (string-equal "" style-string)
		    S-default-style
		  (intern style-string))
		)))
    
    (setq style (or style S-style)) ; use S-style if style is nil
    
    (make-local-variable 'S-style)
    (if (memq style S-styles)
	(setq S-style style)
      (error (concat "Bad S style: " style))
      )
    (message "S-style: %s" S-style)
      
    ; finally, set the indentation style variables making each one local
    (mapcar (function (lambda (S-style-pair)
			(make-local-variable (car S-style-pair))
			(set (car S-style-pair)
			     (cdr S-style-pair))))
	    (cdr (assq S-style S-style-alist)))
    S-style))



;;; S-help-mode
;;;======================================================
;;;

(defvar S-help-mode-map nil "Keymap for S help mode.")
(defvar S-help-mode-hook nil "Functions to call when entering more mode. ")

(defvar S-help-sec-map nil "Sub-keymap for S help mode.")

(defun S-skip-to-help-section nil
  "Jump to a section heading of a help buffer. The section selected is
determined by the command letter used to invoke the command, as indicated
by S-help-sec-keys-alist. Use \\[S-describe-sec-map] to see which keystrokes
find which sections."
  (interactive)
  (let ((old-point (point)))
    (goto-char (point-min))
    (let ((the-sec (cdr (assoc last-command-char S-help-sec-keys-alist))))
      (if (not the-sec) (error "Invalid section key: %c" last-command-char)
	(if (re-search-forward (concat "^" the-sec) nil t) nil
	    (message "No %s section in this help. Sorry." the-sec)
	    (goto-char old-point))))))

(defun S-skip-to-next-section nil
  "Jump to next section in S help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (if (re-search-forward "^[A-Z. ---]+:$" nil t) nil
      (message "No more sections."))))

(defun S-skip-to-previous-section nil
  "Jump to previous section in S help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (if (re-search-backward "^[A-Z. ---]+:$" nil t) nil
      (message "No previous section."))))

(defun S-describe-help-mode nil
"Display help for S-mode"
 (interactive)
 (describe-function 'S-help-mode))

(defun S-kill-buffer-and-go nil
  "Kill the current buffer and switch back to S"
  (interactive)
  (kill-buffer (current-buffer))
  (S-switch-to-S nil))

(defun S-describe-sec-map nil
  "Display help for the `s' key."
  (interactive)
  (describe-function 'S-skip-to-help-section)
  (save-excursion
    (set-buffer "*Help*")
    (goto-char (point-max))
    (insert "\n\nCurrently defined keys are:

Keystroke    Section
---------    -------\n")
    (mapcar '(lambda (cs) (insert "    " (car cs) "        " (cdr cs) "\n")) S-help-sec-keys-alist)
    (insert "\nFull list of key definitions:\n" (substitute-command-keys "\\{S-help-sec-map}"))))

(defun S-find-help-file (p-string)
  (let* ((default (S-read-object-name-default))
         (prompt-string (if default
                            (format "%s(default %s) " p-string default)
                          p-string))
	 (help-files-list (S-get-help-files-list))
         (spec (completing-read prompt-string help-files-list)))
    (list (cond
           ((string= spec "") default)
           (t spec)))))

(defun S-get-help-files-list nil
  (mapcar 'list
	  (apply 'append
		 (mapcar '(lambda (dirname)
			    (if (file-directory-p dirname) 
				(directory-files dirname)))
			 (mapcar '(lambda (str) (concat str "/.Help"))
				 S-search-list)))))
	  
(if S-help-sec-map
    nil
  (setq S-help-sec-map (make-keymap))
  (mapcar '(lambda (key) 
	    (define-key S-help-sec-map (char-to-string key) 
	      'S-skip-to-help-section))
	    (mapcar 'car S-help-sec-keys-alist))
  (define-key S-help-sec-map "?" 'S-describe-sec-map)
  (define-key S-help-sec-map ">" 'end-of-buffer)
  (define-key S-help-sec-map "<" 'beginning-of-buffer)
)

(defun S-display-help-on-object (object)
  "Display the help page for OBJECT in the *Help* buffer. 
If prefix arg is given, forces a query of the S process for the help
file.  Otherwise just pops to an existing buffer if it exists."
  (interactive (S-find-help-file "Help on: "))
  (let* ((hb-name (concat "*help(" object ")*"))
	 (old-hb-p (get-buffer hb-name))
	 (tbuffer (get-buffer-create hb-name)))
    (set-buffer tbuffer)
    (if (or (not old-hb-p) current-prefix-arg)
	;; Ask S for the help file
	(progn
	  (setq S-temp-buffer-p t)		; Flag as a temp buffer
	  (delete-region (point-min) (point-max))
	  (S-help-mode)
	  (S-command (format inferior-S-help-command object) tbuffer)
	  (S-nuke-help-bs)
	  (goto-char (point-min))))
    (let (nodocs)
      (save-excursion
	(goto-char (point-min))
	(setq nodocs 
	      (re-search-forward "\\`No documentation available.*$" nil t))
	(if nodocs
	    (progn
	      (princ (buffer-substring (match-beginning 0) (match-end 0)) t)
	      ;; Avoid using 'message here -- may be %'s in string
	      (ding)
	      (kill-buffer tbuffer))
	  (if (eq major-mode 'S-help-mode) (switch-to-buffer tbuffer)
	    (pop-to-buffer tbuffer)))))))

;;; This function is a modification of nuke-nroff-bs in man.el from the
;;; standard emacs 18 lisp library.
(defun S-nuke-help-bs ()
  (interactive "*")
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

(if S-help-mode-map
    nil
  (setq S-help-mode-map (make-keymap))
  (suppress-keymap S-help-mode-map)  
  (define-key S-help-mode-map " " 'scroll-up)
  (define-key S-help-mode-map "b" 'scroll-down)
  (define-key S-help-mode-map "q" 'S-switch-to-end-of-S)
  (define-key S-help-mode-map "\177" 'scroll-down) ; DEL
  (define-key S-help-mode-map "s" S-help-sec-map)
  (define-key S-help-mode-map "h" 'S-display-help-on-object)
  (define-key S-help-mode-map "r" 'S-eval-region)
  (define-key S-help-mode-map "n" 'S-skip-to-next-section)
  (define-key S-help-mode-map "p" 'S-skip-to-previous-section)
  (define-key S-help-mode-map "/" 'isearch-forward)
  (define-key S-help-mode-map ">" 'end-of-buffer)
  (define-key S-help-mode-map "<" 'beginning-of-buffer)
  (define-key S-help-mode-map "x" 'S-kill-buffer-and-go)
  (define-key S-help-mode-map "?" 'S-describe-help-mode)
  (define-key S-help-mode-map "\C-c\C-r"    'S-eval-region)
  (define-key S-help-mode-map "\C-c\M-r" 'S-eval-region-and-go)
  (define-key S-help-mode-map "\C-c\C-f"    'S-eval-function)
  (define-key S-help-mode-map "\M-\C-x"  'S-eval-function)
  (define-key S-help-mode-map "\C-c\M-f" 'S-eval-function-and-go)
  (define-key S-help-mode-map "\C-c\C-j"    'S-eval-line)
  (define-key S-help-mode-map "\C-c\M-j" 'S-eval-line-and-go)
  (define-key S-help-mode-map "\M-\C-a"  'S-beginning-of-function)
  (define-key S-help-mode-map "\M-\C-e"  'S-end-of-function)
  (define-key S-help-mode-map "\C-c\C-y"    'S-switch-to-S)
  (define-key S-help-mode-map "\C-c\C-z" 'S-switch-to-end-of-S)
  (define-key S-help-mode-map "\C-c\C-l"    'S-load-file)
  (define-key S-help-mode-map "\C-c\C-h"    'S-display-help-on-object))

;;; Largely ripped from more-mode.el,
;;;  by Wolfgang Rupprecht wolfgang@mgm.mit.edu

(defun S-help-mode ()
  "Mode for viewing S help files.
Use SPC and DEL to page back and forth through the file.
Use `s' to jump to a particular section; `s ?' for help.
Use `q' to return to your S session; `x' to kill this buffer first.
The usual commands for evaluating S source are available.
Other keybindings are as follows:
\\{S-help-mode-map}"
  (interactive)
  (setq major-mode 'S-help-mode)
  (setq mode-name "S Help")
  (use-local-map S-help-mode-map)
  (run-hooks S-help-mode-hook))

(run-hooks 'S-mode-load-hook)


;;; Revision notes:
;;  Release 2.1 on October 14, 1991 to statlib@stat.cmu.edu, 
;;     and to the elisp archives at OSU (brennan@dg-rtp.dg.com (Dave Brennan))
;;  and announced on internet.s-news, netnews.gnu.emacs.sources, & 
;;    andrew.programs.S
;; -------------------------------------------------------
;;     Jul 26          1991  Frank Ritter
;;   * added S-mode-load-hook & S-pre-run-hook
;;     and testing by neilc@research.att.com
;;     Jul 9           1991  Frank Ritter
;;   * Changed S-command to use a register rather than 
;;       the kill ring.
;;   * Better file header, comments now at 60 col so 
;;       mailers wont' eat them.
;;   * Better S-extract-word-name.
;;   * Added S-mode-version variable
;;   * Changed syntax table to read |#; appropriately
;;
;; Wed Nov 28 11:03:50 1990  Ed Kademan  (kademan at hermes)
;;   * Make the S-mode-syntax-table a slightly modified
;;       version of the c-mode-syntax-table instead of a
;;       version of the one for lisp.
;; 
;; Sat Nov 10 12:41:52 1990  Ed Kademan  (kademan at hermes)
;;   * Made run-S and run-s commands synonymous with the
;;       function S.
;; 
;; Fri Oct 19 12:41:52 1990  Ed Kademan  (kademan at hermes)
;;   * Made S-directory a user modifiable variable.  S will
;;       run from that directory.
;; 
;; Thu Oct 18 12:41:52 1990  Ed Kademan  (kademan at hermes)
;;   * Added function S-nuke-help-bs to clean up nroff
;;       style text in the S help buffer.  This function is
;;       a modification of nuke-nroff-bs from man.el.
;; -------------------------------------------------------
;; Unnumbered version released dated Thu Jun 14 09:56:56 CDT 1990
;;
;; Fri Jan 17 1992 Dave Smith (dsmith@stats.adelaide.edu.au)
;;   * Help mode for reading files. When asking for an object to
;;     run help on, completion is over those help files that exist.
;;   * Added object name completion, and made S-get-object-list
;;     efficient enough to make it worthwile.
;;   * Error parsing for loaded files
;;   * Better customization of file-names, with sensible defaults
;;   * Sensible buffer names for object buffers
;;   * Corrected definition for `.' in syntax table
;;   * Improved (and simplified) S-read-object-name-default
;;   * Included pager='cat' to default help-command specification
;;   * Added a call to run-hook for S-pre-run-hook
;;   * Changed keymaps to conform with GNU guidelines
;;     (i.e. no \C-letter bindings)
;;   * S-command has a new third argument, visible
;;
;; Tue May 27 1992 Dave Smith (dsmith@stats.adelaide.edu.au)
;;   * now copes with dynamically changing prompts (reported by Doug Bates)
;;
;; Thu May 29 1992 Dave Smith (dsmith@stats.adelaide.edu.au)
;;   * Added S-execute, modified S-execute-* to use it.
;;
;; Mon Jun 22 1992 dsmith
;;   * Added S-mode editing commands written by Ken'ichi Shibayama
;;     (shiba@isac.co.jp). A big win. 
;;   * Removed the redundant argument to S-switch-to-end-of-S
;;   * S-function-pattern improved
;;   * added S-eval-visibly, S-eval-visibly-p and modified S-eval-*
;;     to use them
;;   * added S-eval-line-and-next-line
;;   * eval commands can now echo in the process buffer
;;   * added S-kill-output and S-view-at-bottom
;;   * added a binding for comint-isearch and autoloaded it
;;   * added S-execute-in-tb. S-parse-errors now takes prefix arg.
;;
;; Thu Jun 25 1992 dsmith
;;   * Moved some doctrings to comments (Frank Ritter)
;;   * The Tek stuff now lives in a separate file (Frank Ritter)
;;   * Fiddly C-c ESC M-. bindings in S mode and Help mode moved
;;       to C-c M-. bindings (Martin Maechler)
;;   * S-execute-objects now uses variable inferior-S-objects-command
;;       whose value depends on S version. (Ken'ichi Shibayama)
;;   * Symbols given uniform prefixes: S- or inferior-S- (Frank Ritter)

