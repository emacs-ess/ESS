;;; ess-custom.el --- Customize variables for ESS

;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2015 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: A.J. Rossini <blindglobe@gmail.com>
;; Created: 05 June 2000
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS

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
;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Code:

(require 'custom)
(require 'executable)
(require 'font-lock)

;; Customization Groups

(defgroup ess nil
  "ESS: Emacs Speaks Statistics."
  :group 'local)

(defgroup ess-edit nil
  "ESS: editing behavior, including coments/indentation."
  :group 'ess
  :prefix "ess-")

(defgroup ess-proc nil
  "ESS: process control."
  :group 'ess
  :prefix "ess-")

(defgroup ess-command nil
  "ESS: Commands for various things."
  :group 'ess
  :prefix "ess-")

(defgroup ess-help nil
  "ESS: help functions."
  :group 'ess
  :prefix "ess-")

(defgroup ess-hooks nil
  "ESS: hooks for customization."
  :group 'ess
  :prefix "ess-")

(defgroup ess-S nil
  "ESS: S Languages."
  :group 'ess
  :prefix "ess-")

(defgroup ess-origS nil
  "ESS: Original S Dialect from Bell Labs/AT&T."
  :group 'ess-S
  :prefix "ess-")

(defgroup ess-SPLUS nil
  "ESS: S-PLUS Dialect of S."
  :group 'ess-S
  :prefix "ess-")

(defgroup ess-R nil
  "ESS: R Dialect of S."
  :group 'ess-S
  :prefix "ess-")

(defgroup ess-Julia nil
  "ESS: Julia."
  :group 'ess
  :prefix "julia-")

(defgroup ess-sas nil
  "ESS: SAS."
  :group 'ess
  :prefix "ess-")

(defgroup ess-Stata nil
  "ESS: Stata."
  :group 'ess
  :prefix "ess-")

(defgroup ess-XLS nil
  "ESS: XLispStat."
  :group 'ess
  :prefix "ess-")

(defgroup ess-OMG nil
  "ESS: Omegahat."
  :group 'ess
  :prefix "ess-")

(defgroup ess-mouse nil ;; FIXME: this is not used yet <--> ./ess-mous.el
  "ESS: Mouse."
  :group 'ess
  :prefix "ess-")

(defgroup ess-roxy nil
  "Mode for editing in-code Roxygen documentation."
  :group 'ess
  :group 'convenience
  :group 'ess-extras
  :prefix "ess-" ;; << -- added for ESS integration  FIXME??
  :group 'tools)

(defgroup ess-sweave nil
  "Mode for editing Sweave (*.[SR]nw) files."
  :group 'ess-S
  :prefix "ess-")

(defgroup ess-extras nil
  "Extra utilities for ESS"
  :group 'ess
  :prefix "ess-")
;; Variables (not user-changeable)

(defvar ess-version "15.09-devel" ;; updated by 'make'
  "Version of ESS currently loaded.")

(defvar ess-revision nil ;; set
  "The subversion revision and date of ESS.
Is set  by \\[ess-version-string].")


(defvar no-doc
  "This function is part of ESS, but has not yet been loaded.
Full documentation will be available after autoloading the function."
  "Documentation for autoload functions.")


 ; User changeable variables

;;; Common user changeable variable are described and documented in
;;; ess-site.el.  Please check there first!
;;;=====================================================
;;; In general: Variables with document strings starting with a * are
;;; the ones you can generally change safely, and may have to upon
;;; occasion.

;;*;; Options and Initialization

;; Menus and pulldowns.

(defcustom ess-funcmenu-use-p (fboundp 'func-menu)
  "Non-nil means use func-menu."
  :group 'ess
  :type  'boolean)

(defcustom ess-speedbar-use-p (fboundp 'speedbar)
  "Non-nil means use speedbar."
  :group 'ess
  :type  'boolean)

(defcustom ess-imenu-use-p (fboundp 'imenu)
  "Non-nil means use imenu facility.
This value can be overridden by mode-specific variables, such
as `ess-imenu-use-S'."
  :group 'ess
  :type  'boolean)

(defcustom ess-imenu-use-S ess-imenu-use-p
  "*Non-nil means include an Imenu menu item in S buffers."
  :group 'ess
  :type  'boolean)

(defvar ess-imenu-generic-expression nil
  "Placeholder for imenu-generic-expression. Dialect specific.")



;;

(defcustom ess-handy-commands '(("change-directory"     . ess-change-directory)
                                ("install.packages"     . ess-install.packages)
                                ("library"              . ess-library)
                                ("objects[ls]"          . ess-execute-objects)
                                ("help-apropos"         . ess-display-help-apropos)
                                ("help-index"           . ess-display-package-index)
                                ("help-object"          . ess-display-help-on-object)
                                ("search"               . ess-execute-search)
                                ("set-width"            . ess-execute-screen-options)
                                ("setRepos"             . ess-setRepositories)
                                ("sos"                  . ess-sos)
                                ("vignettes"            . ess-display-vignettes)
                                )
  "An alist of custom ESS commands available for call by
`ess-handy-commands' and `ess-smart-comma' function."
  :group 'ess
  :type (if (featurep 'emacs) 'alist 'list))

(defvar ess--local-handy-commands nil
  "Store handy commands locally")
(make-variable-buffer-local 'ess--local-handy-commands)



(defcustom ess-describe-at-point-method nil
  "Whehter `ess-describe-object-at-point' should use a tooltip.
If nil display in an electric buffer. If 'tooltip display in
a tooltip.

See also `tooltip-hide-delay' and `tooltip-delay'.
 "
  :group 'ess-utils
  :type '(choice (const :tag "buffer" :value nil ) (const tooltip))
  )

(defcustom ess-R-describe-object-at-point-commands
  '(("str(%s)")
    ("htsummary(%s, hlength = 20, tlength = 20)")
    ("summary(%s, maxsum = 20)"))
  "A list of commands cycled by `ess-describe-object-at-point'.
%s is substituted with the name at point.

The value of each element is nil and is not used in current
implementation."
  :group 'R
  :type 'alist)


(defcustom ess-S-describe-object-at-point-commands
  ess-R-describe-object-at-point-commands
  "An alist of commands cycled by `ess-describe-object-at-point'.
%s is substitute with the name at point. The value is not used as
 yet."
  :group 'S+
  :type 'alist)


(defcustom ess-can-eval-in-background nil
  "If non-nil ESS can perform caching and other background
 activities by calling the subprocess on idle time.")

(defcustom ess-user-full-name (user-full-name)
  "The full name of the user."
  :group 'ess
  :type 'string)

(defcustom ess-blink-region t
  "If t evaluated region is highlighted for a shortwhile.
See also `ess-blink-delay'"
  :group 'ess
  :type 'boolean)

(defcustom ess-blink-delay .3
  "Number of seconds to highlight the evaluated region."
  :group 'ess
  :type 'number)

(defcustom ess-ask-for-ess-directory t
  "Non-nil means request the process directory each time S is run."
  :group 'ess
  :type 'boolean)

(defcustom ess-ask-about-transfile nil
  "Non-nil means ask about a transcript file before running ESS."
  :group 'ess
  :type 'boolean)

(defcustom ess-display-buffer-reuse-frames t
  "Non-nil means \\[display-buffer] reuses existing frames; see
`display-buffer-reuse-frames'."
  :group 'ess
  :type 'boolean)

(defvar ess-language nil
  "Prefix of all ESS processes, and defines the dialect in use.
Currently acceptable values are `S',  `XLS', `SAS'.
Can be changed, e.g., to `R'.  Use `setq-default' if setting it in
.emacs (also see ess-site.el).")
;; :group 'ess
;; :type '(choice (const :tag "Initial" :value nil)
;;                (const :tag "S"       :value "S")
;;                (const :tag "XLS"     :value "XLS")
;;                (const :tag "SAS"     :value "SAS")
;;                (const :tag "R"       :value "R")))

(make-variable-buffer-local 'ess-language)

(defvar ess-dialect nil
  "String version of the dialect being run for the inferior process.
This, plus `ess-language', should be able to determine the exact
version of the statistical package being executed in the particular
buffer.

Current values could include:
for `ess-dialect' = S3, S4, Sp3, Sp4, Sp5, S+, R, XLS, SAS, Stata, Julia

Used to adjust for changes in versions of the program.")

(make-variable-buffer-local 'ess-dialect)
;;(setq-default ess-dialect "Initial-dialect")
(setq-default ess-dialect nil)
;;; SJE -- why use "Initial-dialect"?  If we use nil, it matches "None"
;;; in the custom choice.

;; (defcustom ess-etc-directory
;;   (expand-file-name (concat ess-lisp-directory "/../etc/"))
;;   "*Location of the ESS etc/ directory.
;; The ESS etc directory stores various auxillary files that are useful
;; for ESS, such as icons."
;;   :group 'ess
;;   :type 'directory)

(defcustom ess-directory-function nil
  "Function to return the directory that ESS is run from.
If nil or if the function returns nil then you get `ess-directory'."
  :group 'ess
  :type '(choice (const nil) function))

(defcustom ess-setup-directory-function nil
  "Function to setup the directory that ESS is run from.
This function can be called to set environment variables or to create
a workspace."
  :group 'ess
  :type '(choice (const nil) function))

(defcustom ess-directory nil
  "The directory ESS is run from.  It must end in a slash.
Provided as a default if `ess-ask-for-ess-directory' is non-nil.
A nil value means use the current buffer's default directory."
  :group 'ess
  :type '(choice (const nil) directory))

(defcustom ess-history-directory nil
  "Directory to pick up `ess-history-file' from.
If this is nil, the history file is relative to `ess-directory'."
  :group 'ess
  :type '(choice (const nil) directory))

(defcustom ess-history-file t
  "File to pick up history from.  nil means *no* history is read or written.
t means something like \".Rhistory\".
If this is a relative file name, it is relative to `ess-history-directory'.
Consequently, if that is set explicitly, you will have one history file
for all projects."
  :group 'ess
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)
                 file))

(defcustom ess-plain-first-buffername t
  "No fancy process buffname for the first process of each type (novice mode)."
  :group 'ess
  :type 'boolean)


(defcustom ess-use-inferior-program-name-in-buffer-name nil
  "For R, use e.g., 'R-2.1.0' or 'R-devel' (the program name) for buffer name.
Avoids the plain dialect name."
  :group 'ess
  :type 'boolean)

(defcustom  ess-use-ido t
  "If t ess will try to use ido completion whenever possible.

By default ESS uses enables IDO flex matching. See
`ido-enable-flex-matching' for details on flex matching and
`ess-ido-flex-matching' on how to disable it for ESS, if you
don't want it.

Some useful keys for IDO completion:

 - C-s (next) or C-r (previous) to move through the list.
 - C-SPC   to restrict the list to currently matched items.
 - TAB     to display possible completion in a buffer
 - C-t     `ido-toggle-regexp'
"
  :group 'ess
  :type 'boolean)


(defcustom ess-tab-complete-in-script nil
  "If non-nil, TAB in script buffers tries to complete if there is nothing to indent.
See also `ess-first-tab-never-complete'.")

(defcustom ess-first-tab-never-complete 'symbol
  "If t, first TAB never tries to complete in ess-mode.
If 'symbol first TAB doesn't try to complete if next char is a
valid symbol constituent.

If 'symbol-or-paren  don't complete if next char is closed paren
)}] or symbol character.

If 'symbol-or-paren-or-punct don't complete if next char is
punctuation +-=% etc, or closed paren or symbol.

If 'unless-eol - first TAB completes only at end of line.

If nil first TAB always tries to complete (this might be too
aggressive and dangerous).
"
  :group 'ess
  :type '(choice (const nil)
                 (const symbol)
                 (const symbol-or-paren)
                 (const symbol-or-paren-or-punct)
                 (const unless-eol)
                 (const t)))

(defvaralias 'ess-first-tab-never-completes-p  'ess-first-tab-never-complete)

(defcustom ess-use-eldoc t
  "If t, activate eldoc in ess-mode and inferior-ess-mode buffers.
If 'script-only activate in ess-mode buffers only.

See also `ess-eldoc-show-on-symbol'."
  :group 'ess-extras
  :type '(choice (const t) (const script-only) (const nil)))


(defcustom ess-eldoc-show-on-symbol nil
  "If non-nil, show help string whenever the point is on a symbol.
If nil show only when the point is in a function call, i.e. after (."
  :group 'ess-extras
  :type  'boolean)


(defcustom ess-eldoc-abbreviation-style 'normal
  "How ess-eldoc string should be abbreviated when it doesn't fit into one line
A symbol which can be
nil: do nothing
mild:  Replace TRUE, FALSE with T,F
normal: Try mild + shorten the default values longer than 10 characters.
strong: Try normal + completely remove default values except =F,=T,=d where d is a digit.
aggressive (or t): Try strong + truncate the doc string to fit into minibuffer.

The default style is 'normal.

Ess-eldoc also honors the value of
`eldoc-echo-area-use-multiline-p'. If this variable is not t (the
default), doc strings are truncated to fit into minibufer. This
allows the use of different abbreviation styles with the
truncation."
  :group 'ess
  :type '(choice (const nil) (const mild) (const normal) (const strong) (const aggressive) (const t)))


(defcustom ess-use-auto-complete t
  "If t, activate auto-complete support  in ess-mode and inferior-ess-mode buffers.
If 'script-only activate in ess-mode buffers only.

If non-nil add `ac-source-R' and `ac-source-filename' to the
`ac-sources' buffer local variable.

ESS defines three AC sources `ac-source-R',`ac-source-R-objects'
and `ac-source-R-args'. See auto-complete package
documentation (http://cx4a.org/software/auto-complete/) for how
to install your custom sources."
  :group 'ess-extras
  :type '(choice (const t) (const script-only) (const nil)))

(defcustom ess-use-company t
  "If t, activate company support in ess-mode and inferior-ess-mode buffers.
If non-nil add `company-R-args' and `company-R-objects' to the
`company-backends'. If 'script-only activate in ess-mode buffers
only."
  :group 'ess-extras
  :type '(choice (const t) (const script-only) (const nil)))

(defcustom ess-company-arg-prefix-length nil
  "Minimum prefix for ess company function argument completion."
  :group 'ess-extras
  :type '(choice (const :tag "Default" nil)
                 integer))

(defcustom ess-use-tracebug t
  "If t, load ess-tracebug when R process starts."
  :group 'ess-extras
  :type  'boolean)

(defcustom  ess-ido-flex-matching t
  "If t, ido for ESS completion uses flex matching.
See `ido-enable-flex-matching' for details.
If you have an old computer, or you load lot of packages, you
might want to set this to nil.
"
  :group 'ess
  :type 'boolean)


(defvar ess-ac-sources nil
  "Dialect specific, ESS specific list of ac-sources")

(defvar ess-company-backends nil
  "Dialect specific, ESS specific list of `company-backends'")

(defvar ess--completing-hist nil
  "Variable to store completion history.
Used by `ess-completion-read' command.")

(defvar ess-smart-operators ()
  "List of smart operators to be used in ESS and IESS modes.
Not to be set by users. It is redefined by mode specific
settings, such as `ess-R-smart-operators'.")
(make-variable-buffer-local 'ess-smart-operators)

(defvar ess-R-smart-operators nil
  "If nil, don't use any of smart operators.
If t, use all. If an axplicit list of operators, use only those
operators.

In current verion of ESS, it controls the behavior of
ess-smart-comma only, but will be enriched in the near future.")

(defvar ess-no-skip-regexp "[ \t\n]*\\'"
  "If `ess-next-code-line' sees this line, it doesn't jump over.

Used to avoid annoying jumping by ess-eval.*-and-step to end of
buffer or end chunks etc.")

(defcustom ess-S-assign " <- "
  "String used for left assignment in all S dialects.
 Used by \\[ess-smart-S-assign]."
  :group 'ess-S
  :type 'string)

(defcustom ess-smart-S-assign-key "_"
  "Key used by `ess-smart-S-assign'. By default bound to
underscore, but can be set to any key. If this key is customized,
you must add

 (ess-toggle-S-assign nil)
 (ess-toggle-S-assign nil)

after the line that sets the customization and evaluate these
lines or reboot emacs. The first call clears the default
`ess-smart-S-assign' assignment and the second line re-assigns
it to the customized setting. "
  :group 'ess-S
  :type 'character)

;;*;; Variables concerning editing behaviour

(defcustom ess-filenames-map t
  "Declares if the filenames in an attached directory are the same
as objects in that directory (when t). This is not true for DOS and
other OS's with limited filename lengths.  Even if this is set
incorrectly, the right things will probably still happen, however."
  :group 'ess-edit
  :type 'boolean)

;;; SJE -- this is set in ess-site.el to be "always", so I changed
;;; value t to be "always", so that ess-site.el does not need editing.
;;; However, this is a bit messy, and would be nicer if ess-site.el
;;; value was t rather than "always".
(defcustom ess-keep-dump-files 'ask
  "Variable controlling whether to delete dump files after a successful load.
If nil: always delete.  If `ask', confirm to delete.  If `check', confirm
to delete, except for files created with ess-dump-object-into-edit-buffer.
Anything else, never delete.  This variable only affects the behaviour
of `ess-load-file'.  Dump files are never deleted if an error occurs
during the load. "
  :group 'ess-edit
  :type '(choice (const :tag "Check" :value  'check)
                 (const :tag "Ask"   :value  'ask)
                 (const :tag "Always keep"   :value "always")
                 (const :tag "Always delete"   :value nil)
                 ))


(defcustom ess-delete-dump-files nil
  "Non-nil means delete dump files after they are created.
This applies to dump files created with
`ess-dump-object-into-edit-buffer', only.

Boolean flag which determines what to do with the dump files
generated by \\[ess-dump-object-into-edit-buffer], as follows:

        If nil: dump files are deleted after each use, and so appear
only transiently. The one exception to this is when a loading error
occurs, in which case the file is retained until the error is
corrected and the file re-loaded.

        If non-nil: dump files are not deleted, and backups are kept
as usual.  This provides a simple method for keeping an archive of S
functions in text-file form.

Auto-save is always enabled in dump-file buffers to enable recovery
from crashes.

This is useful to prevent sources file being created for objects
you don't actually modify.  Once the buffer is modified and saved
however, the file is not subsequently unless `ess-keep-dump-files' is
nil, and the file is successfully loaded back into S."
  :group 'ess-edit
  :type 'boolean)

;;; From ess-mode:

(defcustom ess-mode-silently-save t
  "Non-nil means automatically save ESS source buffers before loading."
  :group 'ess-edit
  :type 'boolean)

;;*;; Variables controlling editing

;;;*;;; Edit buffer processing
(defcustom ess-function-template " <- function( )\n{\n\n}\n"
  "If non-nil, function template used when editing nonexistent objects.

The edit buffer will contain the object name in quotes, followed by
this string. Point will be placed after the first parenthesis or
bracket."
  :group 'ess-edit
  :type 'string)

;;; By K.Shibayama 5.14.1992
;;; Setting any of the following variables in your .emacs is equivalent
;;; to modifying the DEFAULT style.

;;;*;;; Indentation parameters

(defcustom ess-auto-newline nil
  "Non-nil means automatically newline before and after braces
inserted in S code."
  :type 'boolean
  :group 'ess-edit)

(defcustom ess-tab-always-indent t
  "Non-nil means TAB in S mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'ess-edit)

(defvar ess-indent-line-function nil
  "Function to be used for the current dialect
nil means to use R/S indentation.")
(make-variable-buffer-local 'ess-indent-line-function)

(defvar ess-indent-offset 2
  "Main indentation offset that is commonly inherited by other offsets.
See `ess-style-alist' for all available offsets.")

(define-obsolete-variable-alias 'ess-indent-level 'ess-indent-offset "15.09")

(defvar ess-offset-arguments 'open-delim
  "Indent for function arguments or bracket indexing.
This variables has an effect only when the ( or [ are not
directly followed by a new line. See
`ess-offset-arguments-newline' for indentation after closing
newline.

If 'open-delim, the arguments are indented at the opening
delimiter following foo:

  object <- some_function(other_function(arg1,
                                         arg2,
                                         arg3)


If 'prev-call, the arguments are aligned at the beginning of the
closest function call + N characters:

  object <- some_function(other_function(arg1,
                              arg2,
                              arg3)


If 'prev-line, the arguments are indented at the previous line
indentation + N characters:

  object <- some_function(other_function(arg1,
      arg2,
      arg3)


The 'prev-call and 'prev-line settings are actually equivalent to
'(prev-call . t) and '(prev-line . t), where `t' represents the
base indent size. More generally, you can supply '(prev-call . N)
to control the size of indentation.

See `ess-style-alist' for other offsets.")

(defvar ess-offset-arguments-newline 'prev-call
  "Indent of arguments when ( or [ is followed by a new line.

If 'open-delim, the arguments are indented at the opening
delimiter following foo:

  object <- some_function(other_function(
                                         arg1,
                                         arg2)


If 'prev-call, the arguments are aligned at the beginning of the
closest function call + N characters:

  object <- some_function(other_function(
                              arg1,
                              arg2)


If prev-line, the arguments are indented at the previous line
indentation + N characters:

  object <- some_function(other_function(
      arg1,
      arg2)


The 'prev-call and 'prev-line settings are actually equivalent to
'(prev-call . t) and '(prev-line . t), where `t' represents the
base indent size. More generally, you can supply '(prev-call . N)
to control the size of indentation.")

(defvar ess-offset-block 'prev-line
  "Indentation for blocks.
A block is usually declared with braces but a statement wrapped
in anonymous parentheses is also considered a block.

If nil, blocks are indented from the opening delimiter:

  {
      fun_call(parameter = {
                               stuff
                           }, {
                                  stuff
                              })

      lapply(data, function(x) {
                                   body
                               })
  }


If a number N, blocks are indented relative to the opening
parenthesis of the closest function call:

  {
      fun_call(parameter = {
                   stuff
               }, {
                   stuff
               })

      lapply(data, function(x) {
                 body
             })
  }

In this case, the value types of `ess-offset-arguments' and
`ess-offset-arguments-newline' are taken into account for
consistency.


If a list of the form '(N) where N is a number, blocks are
indented at the previous line indentation + N characters:

  {
      fun_call(parameter = {
          stuff
      }, {
          stuff
      })

      lapply(data, function(x) {
          body
      })
  }

You can refer to `ess-indent-offset' by setting this parameter to t
or '(t) instead of N or '(N).

See `ess-style-alist' for other offsets.")

(defvar ess-offset-continued 'straight
  "This setting controls indentation of continued statements, that is,
consecutive statements separated by operators.

When set to 'straight, continued statements are indented as follows:

  object %>%
      some_function() %>%
      other_function()

When set to 'cascade:

  object %>%
      some_function() %>%
          other_function()

The 'straight and 'cascade settings are actually equivalent to
'(straight . t) and '(cascade . t), where `t' represents the
base indent size. More generally, you can supply '(straight . N)
to control the size of indentation.")

(defvar ess-indent-align-declaration-args t
  "When non-nil, `ess-offset-arguments' has no effect on function declarations.
All arguments are then aligned from the opening parenthesis.

If `ess-offset-arguments' is set to 4, then function calls are
indented as in:

  some_function(argument1,
      argument2, argument3
  )

but function declarations are aligned on open (;

  fun <- function(argument1,
                  argument2
                  argument3) {
      body
  }

See `ess-style-alist' for further details.")

(defvar ess-indent-align-nested-calls 'ifelse
  "When set to a symbol or a list of symbols,
`ess-offset-arguments-newline' is ignored for corresponding calls
and are vertically aligned instead. The default is `ifelse',
resulting in the following indentation for nested ifelse calls:

    object <- ifelse(condition1, out1,
              ifelse(condition2, out2, out3))")

(defvar ess-indent-align-braced-continuations t
  "When non-nil, continuations inside parentheses or brackets
will be indented from the opening delimiter:

  if (test1 || test2 ||
      test3 || test4) {
    10 + (1 + 2 +
          3 + 4)
  }

instead of

  if (test1 || test2 ||
        test3 || test4) {
    10 + (1 + 2 +
            3 + 4)
  }

Definition operators (`<-', `=', `:=' and `~') still trigger an
indentation in all cases.")

(defvar ess-indent-prev-call-lhs nil
  "When non-nil, indent arguments from the left-hand side of an assignment.

This setting only has an effect when indentation of arguments or
blocks is relative to the innermost function call. That is, when
`ess-offset-arguments', `ess-offset-arguments-newline' or
`ess-offset-block' are set to a number N as opposed to nil or
'(N).

If nil:

  some_function(parameter = other_function(
                                argument
                            ))

  object <- some_function(
                argument1,
                argument2
            )

If t:

  some_function(parameter = other_function(
                    argument
                ))

  object <- some_function(
      argument1,
      argument2
  )

See `ess-style-alist' for for an overview of ESS indentation.")

(defvar ess-indent-prev-call-chains t
  "This switch adjusts the behaviour of
ess-offset-arguments(-newline) and ess-offset-block when they are
set to `t'. In this case, arguments are indented starting from
the function call. When ess-indent-prev-call-chains is
`prev-call' as well, chained calls will be treated as if they
were one call and indentation will start from the first one.

For example, with ess-offset-arguments-newline set to `prev-call'
and ess-indent-prev-call-chains set to `nil', we have:

  some_function(other_function(
                    argument
                ))

And when the latter is set to `t' instead:

  some_function(other_function(
      argument
  ))")

;;added rmh 2Nov97 at request of Terry Therneau
(defcustom ess-indent-with-fancy-comments t
  "Non-nil means distiguish between #, ##, and ### for indentation.
See `ess-style-alist' for for an overview of ESS indentation."
  :type 'boolean
  :group 'ess-edit)

(define-obsolete-variable-alias 'ess-fancy-comments 'ess-indent-with-fancy-comments "15.09")
(define-obsolete-variable-alias 'ess-arg-function-offset 'ess-indent-prev-call-lhs "15.09")
(define-obsolete-variable-alias 'ess-arg-function-offset-new-line 'ess-offset-arguments-newline "15.09")
(define-obsolete-variable-alias 'ess-first-continued-statement-offset 'ess-offset-continued "15.09")
(define-obsolete-variable-alias 'ess-continued-statement-offset 'ess-offset-continued "15.09")


;;;*;;; Editing styles

(defvar ess-style-alist
  `((BSD
     (ess-indent-offset . 8)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . prev-call)
     (ess-offset-block . prev-call)
     (ess-offset-continued . straight)
     (ess-indent-align-declaration-args . t)
     (ess-indent-align-nested-calls . ifelse)
     (ess-indent-align-braced-continuations . t)
     (ess-indent-prev-call-lhs . t)
     (ess-indent-prev-call-chains . t)
     (ess-indent-with-fancy-comments . t))

    (C++
     (ess-indent-offset . 4)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . prev-call)
     (ess-offset-block . prev-call)
     (ess-offset-continued . straight)
     (ess-indent-align-declaration-args . t)
     (ess-indent-align-nested-calls . ifelse)
     (ess-indent-align-braced-continuations . t)
     (ess-indent-prev-call-lhs . t)
     (ess-indent-prev-call-chains . t)
     (ess-indent-with-fancy-comments . t))
    
    ;; CLB added rmh 2Nov97 at request of Terry Therneau
    (CLB
     (ess-indent-offset . 2)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . prev-call)
     (ess-offset-block . prev-line)
     (ess-offset-continued . (straight 4))
     (ess-indent-align-declaration-args . t)
     (ess-indent-align-nested-calls . ifelse)
     (ess-indent-align-braced-continuations . t)
     (ess-indent-prev-call-lhs . t)
     (ess-indent-prev-call-chains . t)
     (ess-indent-with-fancy-comments . t))

    (GNU
     (ess-indent-offset . 2)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . (prev-call 4))
     (ess-offset-block . prev-line)
     (ess-offset-continued . straight)
     (ess-indent-align-declaration-args . t)
     (ess-indent-align-nested-calls . ifelse)
     (ess-indent-align-braced-continuations . t)
     (ess-indent-prev-call-lhs . t)
     (ess-indent-prev-call-chains . t)
     (ess-indent-with-fancy-comments . t))
    
    (K&R
     (ess-indent-offset . 5)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . prev-call)
     (ess-offset-block . prev-call)
     (ess-offset-continued . straight)
     (ess-indent-align-declaration-args . t)
     (ess-indent-align-nested-calls . ifelse)
     (ess-indent-align-braced-continuations . t)
     (ess-indent-prev-call-lhs . t)
     (ess-indent-prev-call-chains . t)
     (ess-indent-with-fancy-comments . t))
    
    ;; R added ajr 17Feb04 to match "common R" use
    (RRR
     (ess-indent-offset . 4)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . prev-call)
     (ess-offset-block . prev-line)
     (ess-offset-continued . straight)
     (ess-indent-align-declaration-args . t)
     (ess-indent-align-nested-calls . ifelse)
     (ess-indent-align-braced-continuations . t)
     (ess-indent-prev-call-lhs . t)
     (ess-indent-prev-call-chains . t)
     (ess-indent-with-fancy-comments . t))
    
    (RStudio
     (ess-indent-offset . 2)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . prev-line)
     (ess-offset-block . prev-line)
     (ess-offset-continued . straight)
     (ess-indent-align-declaration-args . t)
     (ess-indent-align-nested-calls . nil)
     (ess-indent-align-braced-continuations . nil)
     (ess-indent-prev-call-lhs . t)
     (ess-indent-prev-call-chains . t)
     (ess-indent-with-fancy-comments . nil))
    
    (DEFAULT
      (ess-indent-offset . ,(default-value 'ess-indent-offset))
      (ess-offset-arguments . ,(default-value 'ess-offset-arguments))
      (ess-offset-arguments-newline . ,(default-value 'ess-offset-arguments-newline))
      (ess-offset-block . ,(default-value 'ess-offset-block))
      (ess-offset-continued . ,(default-value 'ess-offset-continued))
      (ess-indent-align-declaration-args . ,(default-value 'ess-indent-align-declaration-args))
      (ess-indent-align-nested-calls . ,(default-value 'ess-indent-align-nested-calls))
      (ess-indent-align-braced-continuations . ,(default-value 'ess-indent-align-braced-continuations))
      (ess-indent-prev-call-lhs . ,(default-value 'ess-indent-prev-call-lhs))
      (ess-indent-prev-call-chains . ,(default-value 'ess-indent-prev-call-chains))
      (ess-indent-with-fancy-comments . ,(default-value 'ess-indent-with-fancy-comments))))
  
  "Predefined formatting styles for ESS code.
Values for all groups, except OWN, are fixed.  To change the
value of variables in the OWN group, customize the variable
`ess-own-style-list'.  RRR style is the common R style that
adheres closely to R internal standards. RStudio style closely
mimics the indentation of the RStudio editor. DEFAULT style picks
default (aka global) values from ESS indentation variables.  The
actual style that is applied in R buffers is set by
`ess-default-style'.

ESS indentation is fully specified by the following offsets and
variables. See the documentation of these variables for examples.

Offsets:

 - `ess-indent-offset': main offset inherited by other settings

 - `ess-offset-arguments': offset for function and bracket
   arguments

 - `ess-offset-arguments-newline': offset of arguments when ( or
   [ is followed by a new line.

 - `ess-offset-block': offset for brace and anonymous parenthesis
   blocks

 - `ess-offset-continued': offset for continuation lines in
   multiline statements

 - `ess-offset-continued-first': extra offset for first
   continuation line (i.e. second line of a multiline expression)

Control variables:

 - `ess-indent-align-declaration-args': whether to ignore
   `ess-offset-arguments' for function argument declarations

 - `ess-indent-align-nested-calls': functions whose nested calls
   should be aligned.

 - `ess-indent-align-braced-continuations': whether to ignore
   `ess-offset-continued' and `ess-offset-continued-first' inside
   parenthesis and braces.

 - `ess-indent-prev-call-lhs': whether to indent arguments from
   left-hand side of an assignment or parameter declaration.

 - `ess-indent-prev-call-chains': whether to indent arguments from
   the first of several consecutive calls.

 - `ess-indent-with-fancy-comments': whether to indent #,## and ### comments
   distinctly.
")

(defun ess-add-style (key entries)
  "Add a new style to `ess-style-list', with the key KEY.
Remove any existing entry with the same KEY before adding the new one.
               (ess-offset-continued-first . 0)
This can be used"
  (setq ess-style-alist (assq-delete-all key ess-style-alist))
  (add-to-list 'ess-style-alist (cons key entries)))

(defcustom ess-own-style-list (cdr (assoc 'RRR ess-style-alist))
  "Indentation variables for your own style.
Set `ess-default-style' to 'OWN to use these values. To change
these values, use the customize interface. See the documentation
of each variable for its meaning. "
  :group 'ess-edit
  :type 'alist
  :initialize 'custom-initialize-set
  :set (lambda (symbol value)
         (set symbol value)
         (ess-add-style 'OWN value)))

(defcustom ess-default-style 'RRR
  "The default value of `ess-indent-style'.
See the variable `ess-style-alist' for how these groups (RRR,
DEFAULT, GNU, BSD, ...) map onto different settings for
variables. OWN style is defined in `ess-own-style-list' and you
can customize it to your needs. DEFAULT style picks default (aka
global) values from ESS indentation variables."
  :type '(choice (const OWN)
                 (const GNU)
                 (const BSD)
                 (const C++)
                 (const CLB)
                 (const K&R)
                 (const RRR)
                 (const RStudio)
                 (const DEFAULT))
  :group 'ess-edit)

;; the real setting of this happens via <foo>-editing-alist:
(defvar ess-style ess-default-style
  "Current ESS indentation style, see `ess-style-alist' for more.")

;;*;; Variables controlling behaviour of dump files

(defcustom ess-source-directory
  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp")
  "Directory in which to place dump files.
This can be a string (an absolute directory name ending in a slash) or
a lambda expression of no arguments which will return a suitable string
value.  The lambda expression is evaluated with the process buffer as the
current buffer.

This always dumps to a sub-directory (\".Src\") of the current ess
working directory (i.e. first elt of search list)."
  :group 'ess-edit
  :type 'directory)

(defvar ess-dump-filename-template nil
  "Internal. Initialized by dialects")

(defcustom ess-dump-filename-template-proto (concat (user-login-name) ".%s.S")
  "Prototype template for filenames of dumped objects.
The ending `S' is replaced by the current \\[ess-suffix], to give
`ess-dump-filename-template' when an inferior ESS process starts.

By default, gives filenames like `user.foofun.S', so as not to clash with
other users if you are using a shared directory. Other alternatives:
\"%s.S\" ; Don't bother uniquifying if using your own directory(ies)
\"dumpdir\"; Always dump to a specific filename. This makes it impossible
to edit more than one object at a time, though.
 (make-temp-name \"scr.\") ; Another way to uniquify"
  ;; MM: The last 3-4 lines above suck (I don't understand them) -- FIXME --

  :group 'ess-edit
  :type 'string)


;;*;; Hooks

(defcustom ess-mode-hook nil
  "Hook for customizing ESS each time it is entered."
  :group 'ess-hooks
  :type 'hook)

(defcustom ess-mode-load-hook nil
  "Hook to call when ess.el is loaded."
  :group 'ess-hooks
  :type 'hook)

(defcustom ess-pre-run-hook nil
  "Hook to call before starting up ESS.
Good for setting up your directory."
  :group 'ess-hooks
  :type 'hook)

(defcustom ess-post-run-hook nil
  "Hook to call just after the ESS process starts up.
Good for evaluating ESS code."
  :group 'ess-hooks
  :type 'hook)

(defcustom inferior-ess-mode-hook nil
  "Hook for customizing inferior ESS mode.  Called after
`inferior-ess-mode' is entered and variables have been initialised."
  :group 'ess-hooks
  :type 'hook)

;;; make it possible to save an inferior-ess-mode buffer without losing
;;; the connection to the running ESS process.
(put 'inferior-ess-mode 'mode-class 'special)
;; FIXME AJR: Should the above be there?  I don't think so!
;;       MM : the functionality should be, right? Move statement to ./ess.el ?
;;       AJR: No, we should move the statement to ./ess-inf.el

(defcustom ess-help-mode-hook nil
  "Functions to call when entering `ess-help-mode'."
  :group 'ess-hooks
  :type 'hook)

(defcustom ess-send-input-hook nil
  "Hook called just before line input is sent to the process."
  :group 'ess-hooks
  :type 'hook)

(defcustom ess-transcript-mode-hook nil
  "Hook for customizing ESS transcript mode."
  :group 'ess-hooks
  :type 'hook)

(defcustom R-mode-hook nil
  "Hook run when entering R mode."
  :type 'hook
  :group 'ess-R)

(defcustom Rnw-mode-hook nil
  "Hook run when entering Rnw mode."
  :type 'hook
  :group 'ess-R)

(defcustom SAS-mode-hook nil
  "Hook to run when entering SAS mode."
  :type 'hook
  :group 'ess-sas)

(defcustom ess-pdf-viewer-pref nil
  "External pdf viewer you like to use from ESS.
Can be a string giving a name of the program or a list with car
giving heprogram and the tail giving the arguments. For example
'(\"okular\" \"--unique\")."
  :type '(choice (const nil) (repeat :tag "Command with arguments" string) (string :tag "Command"))
  :group 'ess)

(defcustom ess-ps-viewer-pref nil
  "External PostScript viewer you like to use from ESS.
If nil, ESS will try finding one from a list."
  :type '(choice (const nil) string)
  :group 'ess)

;; ---- ./ess-roxy.el : ------------

(defcustom ess-roxy-package "roxygen2"
  "The name of the R package to use for Roxygen."
  :group 'ess-roxy
  :type 'string)

(defcustom ess-roxy-tags-noparam '("export" "noRd")
  "The tags used in roxygen fields that can be used alone.  Used
to decide highlighting and tag completion."
  :group 'ess-roxy
  :type '(repeat string))

(defcustom ess-roxy-tags-param '("author" "aliases" "concept" "details"
                                 "examples" "format" "keywords"
                                 "method" "exportMethod"
                                 "name" "note" "param"
                                 "include" "references" "return"
                                 "seealso" "source" "docType"
                                 "title" "TODO" "usage" "import"
                                 "exportClass" "exportPattern" "S3method"
                                 "inheritParams"
                                 "importFrom" "importClassesFrom"
                                 "importMethodsFrom" "useDynLib"
                                 "rdname" "section" "slot")
  "The tags used in roxygen fields that require a parameter.
Used to decide highlighting and tag completion."
  :group 'ess-roxy
  :type '(repeat string))

(defcustom ess-roxy-template-alist
  (list (cons "description"  ".. content for \\description{} (no empty lines) ..")
        (cons "details" ".. content for \\details{} ..")
        (cons "title" "")
        (cons "param"  "")
        (cons "return" "")
        (cons "author" ess-user-full-name))
  "The tags and defaults to insert when creating empty templates.
Param is a place holder for where to enter
parameters. Description and details do not use @ tags, but are
instead placed at the beginning of the entry (and should
therefore also be at the beginning of this template to give
syntactically correct roxygen entries)"
  :group 'ess-roxy
  :type '(alist :value-type (group string)))

(defcustom ess-roxy-fill-param-p nil
  "Non-nil causes parameter descriptions to be filled (word-wrapped) upon `ess-roxy-update-entry'."
  :group 'ess-roxy
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ess-roxy-hide-show-p nil
  "Non-nil means ess-roxy uses hs-minor-mode for block hiding with TAB."
  :group 'ess-roxy
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ess-roxy-start-hidden-p nil
  "Non-nil means all blocks should be hidden from start."
  :group 'ess-roxy
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ess-roxy-str "##'"
  "Prefix string to insert before each line in new roxygen
blocks. In existing roxygen blocks, the prefix is taken from
the line at point"
  :group 'ess-roxy
  :type 'string)

(defcustom ess-roxy-re "^#+'"
  "Regular expression to recognize roxygen blocks."
  :group 'ess-roxy
  :type 'string)

(defcustom ess-swv-pdflatex-commands '("texi2pdf" "pdflatex" "make")
  "Commands to run a version of pdflatex in  \\[ess-swv-PDF];
the first entry is the default command."
  :group 'ess-sweave
  :type 'list)

(defcustom ess-swv-plug-into-AUCTeX-p nil
  "Non-nil means add commands to AUCTeX's \\[TeX-command-list]
to sweave the current noweb file and latex the result."
  :group 'ess-sweave
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))


 ; System variables

;; SJE -- this should not be defcustom - user does not set it.
(defvar ess-local-process-name nil
  "The name of the ESS process associated with the current buffer.")
(put 'ess-local-process-name 'risky-local-variable t)
(put 'ess-local-process-name 'permanent-local t)
(make-variable-buffer-local 'ess-local-process-name)

(defcustom ess-switch-to-end-of-proc-buffer t
  "If t, `ess-switch-to-inferior-or-script-buffer goes to end of
process buffer."
  :group 'ess
  :type 'boolean)

(defcustom ess-gen-proc-buffer-name-function 'ess-gen-proc-buffer-name:simple
  "Function used for generation of the buffer name of the newly created ESS process.
It should accept one argument PROC-NAME, a string specifying
internal process name (R, R:2, etc).

Provided default options are:

  `ess-gen-proc-buffer-name:simple' -- *proc*
  `ess-gen-proc-buffer-name:directory' -- *proc:dir*
  `ess-gen-proc-buffer-name:abbr-long-directory' -- *proc:abbr-long-dir*
"
  :group 'ess
  :type '(choice (const :tag "*proc*" ess-gen-proc-buffer-name:simple)
                 (const :tag "*proc:dir*" ess-gen-proc-buffer-name:directory)
                 (const :tag "*proc:abbr-long-dir*" ess-gen-proc-buffer-name:abbr-long-directory)
                 function))


(defcustom ess-kermit-command "gkermit -T"
  "Kermit command invoked by `ess-kermit-get' and `ess-kermit-send'."
  :group 'ess
  :type  'string)

(defcustom ess-kermit-prefix "#"
  "String files must begin with to use kermit file transfer."
  :group 'ess
  :type  'string)

(defcustom ess-kermit-remote-directory "."
  "Buffer local variable that designates remote directory of file."
  :group 'ess
  :type  'string)
(make-variable-buffer-local 'ess-kermit-remote-directory)

;;*;; Regular expressions

;; -- Note: Some variables not-to-customize moved to ./ess-mode.el :
;; ess-set-function-start

;; Fixme: the following is just for S dialects :
(defcustom ess-dumped-missing-re
  "\\(<-\nDumped\n\\'\\)\\|\\(<-\\(\\s \\|\n\\)*\\'\\)"
  "If a dumped object's buffer matches this re, then it is replaced
by `ess-function-template'."
  :group 'ess
  :type 'regexp)

(defcustom ess-dump-error-re
  (if (string= ess-language "S") "\nDumped\n\\'"
    "[Ee]rror")
  "Regexp used to detect an error when loading a file."
  :group 'ess
  :type 'regexp)


 ; ess-inf: variables for inferior-ess.

;;*;; System dependent variables

;; If you need to change the *-program-name variables, do so in
;; ess-site.el.  Do NOT make the changes here!!
;; Keep a copy of your revised ess-site.el to use as a starting point
;; for upgrades of ESS.

(defcustom inferior-ess-own-frame nil
  "Non-nil means that inferior ESS buffers should start in their own frame.
The parameters of this frame are stored in `inferior-ess-frame-alist'."
  :group 'ess-proc
  :type 'boolean)

(defcustom inferior-ess-frame-alist default-frame-alist
  "Alist of frame parameters used to create new frames for iESS buffers.
This defaults to `default-frame-alist' and is used only when
the variable `inferior-ess-own-frame' is non-nil."
  :group 'ess-proc
  :type 'alist)

(defcustom inferior-ess-same-window t
  "Non-nil indicates new inferior ESS process appears in current window.
Otherwise, the new inferior ESS buffer is shown in another window in the
current frame.  This variable is ignored if `inferior-ess-own-frame' is
non-nil."
  :group 'ess-proc
  :type 'boolean)


(defcustom inferior-R-program-name
  (if ess-microsoft-p "Rterm"  "R")
  "Program name for invoking an inferior ESS with \\[R]."
  :group 'ess-R
  :type 'string)

(defcustom inferior-R-args ""
  "String of arguments (see 'R --help') used when starting R.
These arguments are currently not passed to other versions of R that have
been created using the variable `ess-r-versions'."
  :group 'ess-R
  :type 'string)

(defcustom ess-R-readline nil
  "nil indicates that \"--no-readline \" should be used as argument when starting R.
This has been the default since 1998 and may very slightly speedup interaction.
On the other hand, readline is necessary for expansion of \"~username/\" in paths.
Note that readline interprets tabs (tabular characters) in R source files as asking
for file name completion.  This can mess up ess evaluation completely."
  :group 'ess-R
  :type 'boolean)

(defcustom inferior-STA-start-file nil
  "Initialization file for Stata."
  :group 'ess-Stata)

(defcustom inferior-STA-start-args ""
  "String of switches used when starting stata.
Don't use this to send initialization command to stata, use
`inferior-STA-start-file' instead. Also see
`inferior-STA-program-name'."
  :group 'ess-Stata
  :type 'string)

(defcustom inferior-R-objects-command "print(objects(pos=%d, all.names=TRUE), max=1e6)\n"
  "Format string for R command to get a list of objects at position %d.
Used in e.g., \\[ess-execute-objects] or \\[ess-display-help-on-object]."
  :group 'ess-command
  :type 'string)

(defcustom ess-getwd-command nil
  "Command string retriving the working directory from the process.")

(defcustom ess-setwd-command nil
  "Command string to set working directory.
Should contain a formating %s to be replaced by a
path (as in 'setwd(%s)\\n'.")

(defcustom ess-program-files ;; 32 bit version
  (if ess-microsoft-p
      (if (getenv "ProgramW6432")
          (w32-short-file-name (getenv "ProgramFiles(x86)"));; always 32 on 64 bit OS
        (w32-short-file-name (getenv "ProgramFiles")))      ;; always 32 on 32 bit OS
    nil)
  "Safe (no embedded blanks) 8.3 name for 32-bit programs that works across internationalization."
  :group 'ess
  :type 'string)

(defcustom ess-program-files-64 ;; 64 bit version
  (if (and ess-microsoft-p (getenv "ProgramW6432"))
      (w32-short-file-name (getenv "ProgramW6432"))
    nil)
  "Safe (no embedded blanks) 8.3 name for 64-bit programs that works across internationalization."
  :group 'ess
  :type 'string)

(defcustom ess-directory-containing-R nil
  "nil (the default) means the search for all occurences of R
on the machine will use the default location of the R directory
 (inside \"c:/Program Files\" in English locale Windows systems).
Non-nil values mean use the specified location as the
directory in which \"R/\" is located.  For example, setting
`ess-directory-containing-R' to \"c:\" will tell ESS to search
for R versions with pathnames of the form \"c:/R/R-x.y.z\".

Currently only used when `ess-microsoft-p'.  If you change the
value of this variable, you need to restart Emacs for it to take
effect.  It also needs to be set before you load ess-site as its
value is used once only when ESS is loaded."

  :group 'ess
  :type 'directory)

(defcustom ess-rterm-version-paths nil
  "Stores the full path file names of Rterm versions, computed via
\\[ess-find-rterm].  If you have versions of R in locations other than
in ../../R-*/bin/Rterm.exe or ../../rw*/bin/Rterm.exe, relative to the
directory in the `exec-path' variable containing your default location
of Rterm, you will need to redefine this variable with a
`custom-set-variables' statement in your site-start.el or .emacs
file."
  :group 'ess-R
  :type '(repeat string))

(defcustom ess-SHOME-versions
  ;;   ess-program-files  ~= "c:/progra~1"  for typical locales/languages
  (mapcar
   (lambda (ch) (concat ess-program-files ch))
   '("/Insightful/splus62"
     "/Insightful/splus61"
     "/MathSoft/splus6"
     "/spls45se"
     "/Insightful/splus62netclient"
     "/Insightful/splus62net/server"
     "/Insightful/splus61netclient"
     "/Insightful/splus61net/server"
     "/Insightful/splus6se"
     "/Insightful/splus61se"
     "/Insightful/splus62se"
     "/Insightful/splus70"
     "/Insightful/splus71"
     "/Insightful/splus8.0.1"
     "/Insightful/splus8.0.4"
     "/Insightful/splus80"
     "/TIBCO/splus81"
     "/TIBCO/splus82"
     ))
  "List of possible values of the environment variable SHOME for recent
releases of S-Plus.  These are the default locations for several
current and recent releases of S-Plus.  If any of these pathnames
correspond to a directory on your machine, running the function
`ess-sqpe-versions-create' will create a function, for example,
\\[splus70], that will start the corresponding version Sqpe inside an
emacs buffer in iESS[S] mode.  If you have versions of S-Plus in
locations other than these default values, redefine this variable with
a `custom-set-variables' statement in your site-start.el or .emacs
file.  The list of functions actually created appears in the *ESS*
buffer and should appear in the \"ESS / Start Process / Other\"
menu."
  :group 'ess-SPLUS
  :type '(repeat string))

(defcustom ess-SHOME-versions-64
  ;;   ess-program-files-64  ~= "c:/progra~1"  for typical locales/languages
  (mapcar
   (lambda (ch) (concat ess-program-files-64 ch))
   '("/TIBCO/splus82"
     ))
  "List of possible values of the environment variable SHOME for recent
releases of 64-bit S-Plus.  These are the default locations for several
current and recent releases of S-Plus.  If any of these pathnames
correspond to a directory on your machine, running the function
`ess-sqpe-versions-create' will create a function, for example,
\\[splus70], that will start the corresponding version Sqpe inside an
emacs buffer in iESS[S] mode.  If you have versions of 64-bit S-Plus in
locations other than these default values, redefine this variable with
a `custom-set-variables' statement in your site-start.el or .emacs
file.  The list of functions actually created appears in the *ESS*
buffer and should appear in the \"ESS / Start Process / Other\"
menu."
  :group 'ess-SPLUS
  :type '(repeat string))

(defcustom inferior-S3-program-name "/disk05/s/S"
  "Program name for invoking an inferior ESS with S3()."
  :group 'ess-S
  :type 'string)

(defcustom inferior-S+3-program-name "Splus"
  "Program name for invoking an inferior ESS with S+3()."
  :group 'ess-SPLUS
  :type 'string)

(defcustom inferior-S+4-program-name
  (concat ess-program-files "/spls45se/cmd/Splus.exe")
  "Program name for invoking an external GUI S+4.
The default value is correct for a default installation of
S-Plus 4.5 Student Edition and with bash as the shell.
For any other version or location, change this value in ess-site.el or
site-start.el.  Use the 8.3 version of the pathname.
Use double backslashes if you use the msdos shell."
  :group 'ess-SPLUS
  :type 'string)

(defcustom inferior-S+4-print-command "S_PRINT_COMMAND=emacsclientw.exe"
  "Destination of print icon in S+4 Commands window."
  :group 'ess-SPLUS
  :type 'string)

(defcustom inferior-S+4-editor-pager-command
  "options(editor='emacsclient.exe', pager='emacsclientw.exe')"
  "Programs called by the editor() and pager() functions
in S+4 Commands window and in Sqpe+4 buffer."
  :group 'ess-S
  :type 'string)

(defcustom inferior-Sqpe+4-program-name
  (concat ess-program-files "/spls45se/cmd/Sqpe.exe")
  "Program name for invoking an inferior ESS with Sqpe+4()."
  :group 'ess-SPLUS
  :type 'string)

(defcustom inferior-Sqpe+4-SHOME-name
  (if ess-microsoft-p (concat ess-program-files "/spls45se" ""))
  "SHOME name for invoking an inferior ESS with Sqpe+4().
The default value is correct for a default installation of
S-Plus 4.5 Student Edition.  For any other version or location,
change this value in ess-site.el or site-start.el.  Use the 8.3
version of the pathname."
  :group 'ess-SPLUS
  :type 'string)
;;(if ess-microsoft-p
;;    (let* ((SHOME (getenv "SHOME"))
;;         (PATH (getenv "PATH"))
;;         (split-PATH (split-string PATH ";")) ;; Unix uses ":"
;;         (num 0)
;;         pathname)
;;      (if (not SHOME)
;;        (while (< num (length split-PATH))
;;          (setq pathname (concat (nth num split-PATH) "/Sqpe.exe"))
;;          (if (not (file-exists-p pathname))
;;              (setq num (1+ num))
;;            (progn
;;              (setq num (length split-PATH))
;;              (setq SHOME (expand-file-name (concat pathname "/../..")))))))
;;      (setq-default inferior-Sqpe+4-SHOME-name SHOME)))


(defcustom inferior-S-elsewhere-program-name "sh"
  "Program name to invoke an inferior ESS with S on a different computer."
  :group 'ess-proc
  :type 'string)

(defcustom inferior-ESS-elsewhere-program-name "sh"
  "Program name to invoke an inferior ESS with program on a
different computer."
  :group 'ess-proc
  :type 'string)

(defcustom inferior-S4-program-name "S4"
  "Program name to invoke an inferior ESS with S4()."
  :group 'ess-S
  :type 'string)

(defcustom inferior-S+5-program-name "Splus5"
  "Program name to invoke an inferior ESS with S+5()."
  :group 'ess-SPLUS
  :type 'string)

(defvaralias 'S+6-dialect-name 'S+-dialect-name)
(defcustom S+-dialect-name "S+"
  "Name of 'dialect' for S-PLUS 6.x and later.
Easily changeable in a user's `.emacs'."
  :group 'ess-SPLUS
  :type 'string)

(defvaralias 'inferior-S+6-program-name 'inferior-S+-program-name)
(if ess-microsoft-p
    (defcustom inferior-S+-program-name
      (concat ess-program-files "/TIBCO/splus82/cmd/Splus.exe")
      "Program name to invoke an external GUI S+ for Windows.
The default value is correct for a default installation of
S-Plus 8.1 and with bash as the shell.
For any other version or location, change this value in ess-site.el or
site-start.el.  Use the 8.3 version of the pathname.
Use double backslashes if you use the msdos shell."
      :group 'ess-SPLUS
      :type 'string)
  (defcustom inferior-S+-program-name "Splus"
    "Program name to invoke an inferior ESS with S+ for Unix."
    :group 'ess-SPLUS
    :type 'string))

(defvaralias 'inferior-S+6-start-args 'inferior-S+-start-args)
(defvaralias 'inferior-Splus-args 'inferior-S+-start-args)
(defcustom inferior-S+-start-args ""
  "String of arguments used when starting S.
These arguments are currently passed only to S+6 and higher."
  :group 'ess-SPLUS
  :type 'string)


(defvaralias 'inferior-Sqpe-start-args 'inferior-Sqpe+-start-args)
(defcustom inferior-Sqpe+-start-args " "
  "Default is empty.  Can be used for license manager information, for example
`(setq inferior-Sqpe+-start-args \" S_ELMHOST=@123.456.789.012  ELMTIMEOUT=60 \")'."
  ;; (setq inferior-Sqpe+-start-args " S_ELMHOST=@123.456.789.012  ELMTIMEOUT=60 ")  ;; use this line as the model for your site-start.el
  :group 'ess-SPLUS
  :type 'string
  )


(defcustom inferior-Splus-objects-command "objects(where=%d)\n"
  "Format string for R command to get a list of objects at position %d.
Used in e.g., \\[ess-execute-objects] or \\[ess-display-help-on-object]."
  :group 'ess-command
  :type 'string)

(defvaralias 'inferior-S+6-print-command 'inferior-S+-print-command)

(defcustom inferior-S+-print-command "S_PRINT_COMMAND=emacsclientw.exe"
  "Destination of print icon in S+ for Windows Commands window."
  :group 'ess-SPLUS
  :type 'string)

(defvaralias 'inferior-S+6-editor-pager-command 'inferior-S+-editor-pager-command)
(defcustom inferior-S+-editor-pager-command
  "options(editor='emacsclient.exe', pager='emacsclientw.exe')"
  "Programs called by the editor() and pager() functions in S+
for Windows Commands window and in Sqpe+6 for Windows buffer."
  :group 'ess-SPLUS
  :type 'string)

(defvaralias 'inferior-Sqpe+6-program-name 'inferior-Sqpe+-program-name)
(defcustom inferior-Sqpe+-program-name
  (concat ess-program-files "/TIBCO/splus82/cmd/Sqpe.exe")
  "Program name for invoking an inferior ESS with Sqpe+6() for Windows."
  :group 'ess-S
  :type 'string)

(defvaralias 'inferior-Sqpe+6-SHOME-name 'inferior-Sqpe+-SHOME-name)
(defcustom inferior-Sqpe+-SHOME-name
  (if ess-microsoft-p (concat ess-program-files "/TIBCO/splus82" ""))
  "SHOME name for invoking an inferior ESS with Sqpe+6 and higher for Windows.
The default value is correct for a default installation of
S-Plus 8.1.  For any other version or location,
change this value in ess-site.el or site-start.el.  Use the 8.3
version of the pathname."
  :group 'ess-SPLUS
  :type 'string)
;;(if ess-microsoft-p
;;    (let* ((SHOME (getenv "SHOME"))
;;         (PATH (getenv "PATH"))
;;         (split-PATH (split-string PATH ";")) ;; Unix uses ":"
;;         (num 0)
;;         pathname)
;;      (if (not SHOME)
;;        (while (< num (length split-PATH))
;;          (setq pathname (concat (nth num split-PATH) "/Sqpe.exe"))
;;          (if (not (file-exists-p pathname))
;;              (setq num (1+ num))
;;            (progn
;;              (setq num (length split-PATH))
;;              (setq SHOME (expand-file-name (concat pathname "/../..")))))))
;;      (setq-default inferior-Sqpe+6-SHOME-name SHOME)))

(defcustom ess-S-quit-kill-buffers-p nil
  "Controls whether S buffers should also be killed once a process is killed.
This is used only when an iESS process is killed using C-c C-q.
Possible values:
nil - do not kill any S buffers associated with the process.
t - kill S buffers associated with the process.
ask - ask the user whether the S buffers should be killed."
  :group 'ess-S
  :type '(choice (const nil) (const t) (const ask)))

(defcustom inferior-XLS-program-name "xlispstat"
  "Program name for invoking an inferior ESS with \\[XLS]."
  :group 'ess-XLS
  :type 'string)

(defcustom inferior-VST-program-name "vista"
  "Program name for invoking an inferior ESS with \\[ViSta]."
  :group 'ess-XLS
  :type 'string)

(defcustom inferior-ARC-program-name "arc"
  "Program name for invoking an inferior ESS with \\[ARC]."
  :group 'ess-XLS
  :type 'string)

(defcustom inferior-SAS-program-name "sas"
  "Program name for invoking an inferior ESS with SAS()."
  :group 'ess-sas
  :type 'string)

(defcustom inferior-STA-program-name "stata"
  "Program name for invoking an inferior ESS with stata().
This is NOT Stata, because we need to call stata with TERM=emacs in
order for it to work right.  And Emacs is too smart for it."
  :group 'ess-Stata
  :type 'string)

(defcustom ess-sta-delimiter-friendly nil
  "Non-nil means convert embedded semi-colons to newlines for Stata processing."
  :group 'ess-Stata
  :type 'string)

(defcustom inferior-OMG-program-name "omegahat"
  "Program name for invoking an inferior ESS with omegahat()."
  :group 'ess-OMG
  :type 'string)


;;;;; names for setting the pager and editor options of the
;;;;; inferior-ess-process
;;;
;;; S-editor and S-pager,
;;; R-editor and R-pager,
;;; ess-editor and ess-pager,
;;; and inferior-ess-language-start
;;; apply in principle to the 15 files essd[s-]*.el
;;; Several of the files (ess-sp4-d.el and ess-sp6w-d.el) have more
;;; than one *-customize-alist.
;;; These variables are currently used only with the S language files for
;;; S S-Plus R.

(defcustom R-editor
  (if ess-microsoft-p "emacsclient.exe"
    (if (equal system-type 'Apple-Macintosh) nil
      (if (featurep 'xemacs) "gnuclient" "emacsclient"))) ;; unix
  "Editor called by R process with 'edit()' command."
  :group 'ess
  :type 'string)

(defcustom R-pager 'nil ; Usually nil is correct as ESS and page() cooperate.
  "Pager called by R process with 'page()' command."
  :group 'ess
  :type '(choice (const nil) string))


;; FIXME:  For GNU emacs, "emacsclient" (without ".exe") also works on Windoze
;;   (if (>= emacs-major-version 22) "emacsclient" ; for all platforms
(defcustom S-editor
  (if ess-microsoft-p "emacsclient.exe"
    (if (equal system-type 'Apple-Macintosh) nil
      ;; unix:
      (if (featurep 'xemacs) "gnuclient" "emacsclient")))
  "Editor called by S process with 'edit()' command."
  :group 'ess
  :type 'string)

(defcustom S-pager
  (if ess-microsoft-p "emacsclientw.exe"
    (if (equal system-type 'Apple-Macintosh) nil
      (if (featurep 'xemacs) "gnuclient" "emacsclient")))
  "Pager called by S process with 'page()' command."
  ;; Change made to provide a better help(function) experience with
  ;; S+6 and xemacs
  ;; gnuclient -q will open a buffer with an HTML help file
  ;; you can view it with M-x browse-url-of-buffer
  :group 'ess
  :type 'string)

(defvar ess-editor nil
  "*Editor by which the process sends information to an emacs buffer
for editing and then to be returned to the process.")

(defvar ess-pager nil
  "*Pager by which the process sends information to an emacs buffer.")

(defvar inferior-ess-language-start nil
  "*Initialization commands sent to the ESS process.")

(make-variable-buffer-local 'ess-editor)
(make-variable-buffer-local 'ess-pager)
(make-variable-buffer-local 'inferior-ess-language-start)



;;;;; names for S-Plus help files on MS-Windows

(defcustom inferior-ess-help-filetype nil
  "S-Plus and Sqpe for Windows use the \"chm\" (compiled html) filetype
for help files.  The default value is nil for other systems."
  :group 'ess-proc
  :type 'string)
(make-variable-buffer-local 'inferior-ess-help-filetype)
(setq-default inferior-ess-help-filetype nil)


;;;;; names for communication using MS-Windows 9x/NT ddeclient mechanism

(defcustom inferior-ess-ddeclient nil
  "ddeclient is the intermediary between emacs and the stat program."
  :group 'ess-proc
  :type 'string)

(make-variable-buffer-local 'inferior-ess-ddeclient)

(defcustom inferior-ess-client-name nil
  "Name of ESS program ddeclient talks to."
  :group 'ess-proc
  :type 'string)

(make-variable-buffer-local 'inferior-ess-client-name)

(defcustom inferior-ess-client-command nil
  "ddeclient command sent to the ESS program."
  :group 'ess-proc
  :type '(choice (const nil) string))

(make-variable-buffer-local 'inferior-ess-client-command)

;;;;; user settable defaults
(defvar inferior-S-program-name  inferior-S+3-program-name
  "*Program name for invoking an inferior ESS with S().")
;;- (setq inferior-S-program
;;-       (cond ((string= S-proc-prefix "S") "Splus")
;;-         ((string= S-proc-prefix "R") "R")
;;-         (t "S")
;;-         ))
;;(make-local-variable 'inferior-S-program)

(defvar inferior-ess-program nil ;inferior-S-program-name
  "*Default program name for invoking inferior-ess().
The other variables ...-program-name should be changed, for the
corresponding program.")

(make-variable-buffer-local 'inferior-ess-program)
;; (setq-default inferior-ess-program inferior-S-program-name)


(defvar inferior-R-version "R (default)"
  "A (short) name of the current R version.  A global variable for
ESS internal communication.")

(defvar inferior-ess-start-args ""
  "String of arguments passed to the ESS process.
If you wish to pass arguments to a process, see e.g. `inferior-R-args'.")

(defcustom inferior-ess-start-file nil
  "File dumped into process, if non-nil."
  :group 'ess-proc
  :type '(choice (const nil) file))

(defcustom inferior-ess-pager "cat"
  "Pager to use for reporting help files and similar things."
  :group 'ess-proc
  :type 'string)

;; does it make sense to customize here, as we currently set this *directly*
;; in the FOO-BAR-cust-alist's ???
;; VS: Right. It only confuses users. It should be set in post-run-hook if
;; desired;  inferior-S-prompt should be customized instead.
(defvar inferior-ess-primary-prompt "> "
  "Regular expression used by `ess-mode' to detect the primary prompt.")

(make-variable-buffer-local 'inferior-ess-primary-prompt)
;; (setq-default inferior-ess-primary-prompt "> ")

(defvar inferior-ess-secondary-prompt nil
  "Regular expression used by ess-mode to detect the secondary prompt.
This is issued by S to continue an incomplete expression.
Set to nil if language doesn't support secondary prompt.")
;; :group 'ess-proc
;; :type 'string)

(make-variable-buffer-local 'inferior-ess-secondary-prompt)
;; (setq-default inferior-ess-secondary-prompt "+ ")

(defvar ess-traceback-command nil
  "Command to generate error traceback.")

;; need to recognise  + + + > > >
;; and "+ . + " in tracebug prompt
(defcustom inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)*> "
  "Regexp used in S and R inferior and transcript buffers for prompt navigation.
Customise it to make `comint-previous-prompt' quiqly navigate to
interesting portions of the buffer.
 "
  :group 'ess-proc
  :type 'string)

(defvaralias 'inferior-ess-S-prompt 'inferior-S-prompt)
;;*;; Variables controlling interaction with the ESS process

(defcustom ess-execute-in-process-buffer nil
  "Non-nil means the ess-execute- commands output to the process buffer.
Otherwise, they get their own temporary buffer."
  :group 'ess-proc
  :type 'boolean)

(defcustom ess-eval-empty nil
  "Non-nil means `ess-eval-line*' will send empty lines to the ESS process."
  :group 'ess-proc
  :type 'boolean)

(defvaralias 'ess-eval-visibly-p 'ess-eval-visibly)

(defcustom ess-eval-visibly t
  "Non-nil means ess-eval- commands display commands in the process buffer.
If t, ESS waits after each line of the command for the process
output. This results in a nice sequence of input and output but
stalls emacs on long output (like Sys.sleep(5) in R).

If 'nowait, ESS still shows the input commands, but don't wait
for the process. Thus all the output is printed after the input
lines.

If nil, ESS doesn't print input commands and doesn't wait for the process.

This variable also affect the evaluation of input code in
iESS. The effect is similar to the above. If t then ess waits for
the process output, otherwise not.
"
  :group 'ess-proc
  :type '(choice (const t) (const nowait) (const nil)))

;; (when (boundp 'ess-eval-visibly-p)
;;   (setq ess-eval-visibly ess-eval-visibly-p))



(defcustom ess-eval-deactivate-mark (fboundp 'deactivate-mark); was nil till 2010-03-22
  "Non-nil means that after ess-eval- commands the mark is deactivated,
 (see \\[deactivate-mark]).  The default is true since ESS version 5.9,
 except on XEmacs which doesn't have \\[deactivate-mark] and friends:
 only affects the situation where `transient-mark-mode' is non-nil."
  :group 'ess-proc
  :type 'boolean)

(defcustom ess-synchronize-evals nil
  "Non-nil means all evaluations will synchronize with the ESS process.
This means ess-mode will wait for S to dent a prompt before sending the next
line of code. This allows users of Emacs version 18.57 or less to
evaluate large regions of code without causing an error.  Users of newer
Emacsen usually do not want this feature, since it locks up use
of Emacs until the code has been successfully evaluated."
  :group 'ess-proc
  :type 'boolean)

(defcustom ess-eval-visibly-at-end t
  "Non-nil means ess-eval- commands display output at end of process buffer."
  :group 'ess-proc
  :type 'boolean)

(defcustom ess-use-R-completion t
  "Non-nil means use R-builtin completion mechanism when available."
  :group 'ess-proc
  :type 'boolean)

(defcustom ess-eval-ddeclient-sleep 0.06
  "If non-nil, a number specifying *seconds* to wait after certain
\\[ess-eval-linewise-ddeclient] calls, such as those at startup."
  ;; i.e this currently only applies to (if microsoft-p ...) !
  :group 'ess-proc
  :type '(choice (const nil) number))

(defcustom ess-sleep-for-shell (if ess-microsoft-p 5 1)
  "*Pause before sending output to the shell."
  :group 'ess-proc
  :type  'number)

 ; System variables

;;*;; Variables relating to multiple processes

;; VS[17-08-2012]: all of the occurrences in the code should should eventually
;; go away, (once we are sure this doesn't break anything)
(defvaralias 'ess-current-process-name 'ess-local-process-name)

(defvar ess--mode-line-process-indicator '("" ess-local-process-name)
  "List of ESS mode-line indicators.
Local in process buffers and must start with a string. Changes of
this variable are automatically reflected in mode-lines of the
process and all associated with it buffers.

Each symbol must evaluate ot one of the standard mode line
objecst. See info node `(elisp)Mode Line Data').  Add a symbol
with `add-to-list' and remove with `delq'. Note that the symbols
which are part of this list should better have
'risky-local-variable property set to t, otherwise the text
properties are not displayed.

External utilities such as `ess-tracebug' and `ess-developer'
customize this variable to indicate changes in the process
status.
")
(put 'ess--mode-line-process-indicator 'risky-local-variable t)
(make-variable-buffer-local 'ess--mode-line-process-indicator)

(defvar ess--local-mode-line-process-indicator '("")
  "List of local process indicators.
See `ess--mode-line-process-indicator' for how to set it.

This is an internal varialbe used by tools like `ess-developer'
and `ess-tracebug'.")
(put 'ess--local-mode-line-process-indicator 'risky-local-variable t)
(make-variable-buffer-local 'ess--local-mode-line-process-indicator)

(defvar ess-process-name-list nil
  "Alist of active ESS processes.")

;;*;; Inferior ESS commands

(defvar ess-load-command "source(\"%s\")\n"
  "Dialect specific format-string for building the ess command to load a file.

This format string should use %s to substitute a file name and should
result in an ESS expression that will command the inferior ESS to load
that file.")
(define-obsolete-variable-alias 'inferior-ess-load-command 'ess-load-command "ESS v13.09")

(defvar ess-load-visibly-command nil
  "Dialect specific format-string for building the ess command to
  load a file with echo.")

(defvar ess-load-visibly-noecho-command nil
  "Dialect specific format-string for building the ess command to
load a file with visible output but no echo.")

(defvar ess-eval-command nil
  "Dialect specific format-string for building the command to evaluate a string.

This format string should use %s as a placeholder for the string
to be evaluated and, optionally, %f for the file name to be
reported in the error references.

The resulting command should not echo code or print any
transitory output. See also `ess-eval-visibly-command' and
`ess-eval-visibly-noecho-command'.")

(defvar ess-eval-visibly-command nil
  "Dialect specific format-string for building the command to
  evaluate a string with visible output and code echo.
See ")

(defvar ess-eval-visibly-noecho-command nil
  "Dialect specific format-string for building the command to
  evaluate a string with visible output but no echo.")

(defcustom inferior-ess-dump-command "dump(\"%s\",file=\"%s\")\n"
  "Format-string for building the ess command to dump an object into a file.

Use first %s to substitute an object name
Use second %s to substitute the dump file name."
  :group 'ess-command
  :type 'string)

(defvar inferior-ess-help-command "help(\"%s\")\n"
  "Format-string for building the ESS command to ask for help on an object.

This format string should use %s to substitute an object name.")

(make-variable-buffer-local 'inferior-ess-help-command)
(setq-default inferior-ess-help-command "help(\"%s\")\n")


(defcustom inferior-ess-r-help-command ".ess.help(\"%s\", help.type=\"text\")\n"
  "Format-string for building the R command to ask for help on an object.

This format string should use %s to substitute an object name.
If set, changes will take effect when next R session is started."
  :group 'ess-command
  :type 'string)

(defvar ess-get-help-topics-function nil
  "Dialect specific help topics retrieval")
(make-variable-buffer-local 'ess-get-help-topics-function)

(defcustom inferior-ess-exit-command "q()\n"
  "Format-string for building the ess command to exit.

This format string should use %s to substitute an object name."
  :group 'ess-command
  :type 'string)

(make-variable-buffer-local 'inferior-ess-exit-command)
(setq-default inferior-ess-exit-command "q()\n")

(defvar inferior-ess-search-list-command nil
  "`ess-language' command that prints out the search list;
i.e. the list of directories and (recursive) objects that `ess-language' uses
when it searches for objects.

Really set in <ess-lang>-customize-alist in ess[dl]-*.el")
(make-variable-buffer-local 'inferior-ess-search-list-command)

;; and hence made buffer-local via that scheme...

;; ;; FIXME: this is nowhere used :
;; (defcustom inferior-ess-names-command "names(%s)\n"
;;   "Format string for ESS command to extract names from an object.

;; %s is replaced by the object name -- usually a list or data frame."
;;   :group 'ess-command
;;   :type 'string)

(defcustom inferior-ess-safe-names-command
  "tryCatch(base::print(base::names(%s), max=1e6), error=function(e){})\n"
  "Format string for ESS command to extract names from an object *safely*.

%s is replaced by an \"object name\" -- usually a list or data frame, but in R also
 e.g., 'package:stats'."

  :group 'ess-command
  :type 'string)

(defcustom inferior-ess-get-prompt-command "options()$prompt\n"
  "Command to find the value of the current S prompt."
  :group 'ess-command
  :type 'string)

(defvar ess-cmd-delay nil
  "*Set to a positive number if ESS will include delays proportional to
`ess-cmd-delay'  in some places. These delays are introduced to
prevent timeouts in certain processes, such as completion.

This variable has no effect from ESS12.03
")
(make-variable-buffer-local 'ess-cmd-delay)

(defvar ess-R-cmd-delay nil
  "Used to initialize `ess-cmd-delay'.

This variable has no effect from ESS12.03
")

(defvar ess-S+-cmd-delay 1.0
  "Used to initialize `ess-cmd-delay'.
This variable has no effect from ESS12.03
")

;;*;; Regular expressions
(defvar inferior-ess-prompt nil
  "The regular expression  used for recognizing prompts.

It is always used in transcript mode.  In inferior ess mode it is
used only if `comint-use-prompt-regexp' is t.

If not set in language's customise-alist it is constructed at run time
from `inferior-ess-primary-prompt' and `inferior-ess-secondary-prompt'.")

(make-variable-buffer-local 'inferior-ess-prompt)

(defvar ess-change-sp-regexp ""
  "The regexp for matching the S/R/.. commands that change the search path.")
(make-variable-buffer-local 'ess-change-sp-regexp)

(defvar ess-S+-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|collection(\\|library(\\|module(\\|source(\\)"
  "The regexp for matching the S-plus commands that change the search path.")

(defvar ess-S-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|library(\\|source(\\)"
  "The regexp for matching the S commands that change the search path.")

(defvar ess-R-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|library(\\|require(\\|source(\\)"
  "The regexp for matching the R commands that change the search path.")

;;*;; Process-dependent variables

(defvar ess-search-list nil
  "Deprecated. Use (ess-search-list) or (ess-process-get 'search-list) instead.")
(make-obsolete-variable 'ess-search-list nil "ESS[12.09]")

(defvar ess-sl-modtime-alist nil
  "Alist of modification times for all ess directories accessed this session.")
(make-variable-buffer-local 'ess-sl-modtime-alist)

(defvar ess-sp-change nil
  "Variable not used. Use (ess-process-get 'sp-for-help-changed?) instead.")
(make-obsolete-variable 'ess-sp-change nil "ESS[12.09]")
;; (make-variable-buffer-local 'ess-sp-change)

(defvar ess-prev-load-dir/file nil
  "This symbol saves the (directory . file) pair used in the last
`ess-load-file' command.  Used for determining the default in the next one.")

(make-variable-buffer-local 'ess-prev-load-dir/file)

(defvar ess-object-list nil
  ;; This is a list of the currently known object names.  It is
  ;; current only for one command entry; it exists under the
  ;; assumption that the list of objects doesn't change while entering
  ;; a command.
  "Cache of object names")

(make-variable-buffer-local 'ess-object-list)

(defvar ess-help-topics-list nil
  ;; List of currently known help topics.
  "Cache of help topics")

(make-variable-buffer-local 'ess-help-topics-list)

;;*;; Miscellaneous system variables

(defvar ess-temp-point nil
  "Variable used to retain a buffer position past let or let*.")


;; SJE: Wed 29 Dec 2004 - following 3 ess-object* variables can be removed
;; soon if no-one needs the completion code.
(defvar ess-object-name-db-file "ess-namedb"
  "File containing definitions for `ess-object-name-db'.")

(defvar ess-object-name-db-file-loaded '()
  "List of programs whose name-db file has been loaded.")

(defvar ess-object-name-db nil
  "Alist of lists of object names, with directory names as keys.
The file ess-namedb.el is loaded (if it exists) to define this variable.
See also function `ess-create-object-name-db'.")

(make-variable-buffer-local 'ess-object-name-db)
(setq-default ess-object-name-db nil)

;; SJE: 2007-07-16 -- add to quieten byte compile.
(defvar ess-loop-timeout nil
  "Number of loops ess-mode will wait for prompt before signalling an error.")

(defcustom ess-S-loop-timeout 2000000
  "Integer specifying how many loops ess-mode will wait for the prompt
before signaling an error. Will be set to `ess-loop-timeout' in the S dialects'
alists.  Increase this, if you have a fast(er) machine."
  :group 'ess-proc
  :type 'integer)

(defcustom ess-XLS-loop-timeout 50000
  "Integer specifying how many loops ess-mode will wait for the prompt
before signaling an error. Will be set to `ess-loop-timeout' in the XLispStat
dialects' alists.  Increase this, if you have a fast(er) machine."
  :group 'ess-proc
  :type 'integer)

;; NOTA BENE: Other languages/dialect currently set `ess-loop-timeout'
;;            **directly** in their ess-*-d.el alist !!

;;;*;;; Font-lock support

;;; for programming, transcript, and inferior process modes.

(defcustom ess-font-lock-mode global-font-lock-mode
  "Non-nil means we use font lock support for ESS buffers.
Default is t, to use font lock support.
If you change the value of this variable, restart Emacs for it to take effect."
  :group 'ess
  :type 'boolean)

(defvar inferior-ess-font-lock-input t
  "

This variable has no effect. Customize
`inferior-ess-font-lock-keywords' directly.
")
(make-obsolete-variable 'inferior-ess-font-lock-input nil "ESS[12.09]")

;; "Reserved Words" -- part 1 --
(defvar ess-RS-constants
  '("TRUE" "FALSE" "NA" "NULL" "Inf" "NaN"))
(defvar ess-R-constants
  (append ess-RS-constants
          '("NA_integer_" "NA_real_" "NA_complex_" "NA_character_")))
(defvar ess-S-constants
  (append ess-RS-constants
          '("T" "F")))

(defvar ess-R-keywords
  ;; "Reserved Words" -- part 2 --
  '("while" "for" "in" "repeat" "if" "else" "switch" "break" "next" "function"
    ;; note that these are *NOT* reserved words in R:
    "return" "message" "warning" "stop"))

(defvar ess-S-keywords
  (append ess-R-keywords '("terminate")))

;; only some of these keywords "look like functions but are not":
(defvar ess-S-non-functions
  '("if" "for" "function" "while"))

;; first the common ones
(defvar ess-S-modifyiers
  '("library" "attach" "detach" "source" "module"))
(defvar ess-R-modifyiers
  '("library" "attach" "detach" "source" "require"))


(defvar ess-R-message-prefixes
  '("Error:" "Error in"
    "Warning:" "Warning in"
    "Warning messages"))
(defvar ess-S-message-prefixes
  (append ess-R-message-prefixes
          '("Syntax error:" "Dumped")))

;;
(defvar ess-R-assign-ops
  '("<<-" "<-" "->") ; don't want "=" here which is not only for assign
  ;; VS??: it's good to have different colour for = anyhow,
  ;; very helpful to read code like foo(x=xa, p=pa, x_not_na)
  )
(defvar ess-S-assign-ops ess-R-assign-ops) ; since "_" is deprecated for S-plus as well


;; ;; Note: \\s\" is really \s" which means match a char belonging to the
;; ;; "quote character" syntax class.
;; (defvar ess-R-function-name-regexp
;;   (concat "\\s\"?\\(\\(\\sw\\|\\s_\\)+"
;;           "\\(<-\\)?\\)\\s\"?\\s-*\\(<-\\)"
;;           "\\(\\s-\\|\n\\)*function")
;;   )


;; VS: simpler and more general:
(defvar ess-R-function-name-regexp
  (concat "\\(\\(?2:\\s\"\\).+\\2\\|\\sw+\\)"
          "\\s-*\\(<-\\)"
          "[ \t\n]*function"))


(defvar ess-S-function-name-regexp
  ess-R-function-name-regexp ; since "_" is deprecated for S-plus as well
  )


(defvar ess-font-lock-keywords nil
  "Internal. Holds a name of the dialect sepcific font-lock
keywords in the current buffer. See `ess-R-font-lock-keywords'
for an example.")
(make-variable-buffer-local 'ess-font-lock-keywords)

(defvar ess-font-lock-defaults nil
  "Internal. Holds dialect sepcific font-lock defaults in the
current buffer. Old system. From ESS[12.09] switched to new
system described in `ess-font-lock-keywords'.")
(make-variable-buffer-local 'ess-font-lock-defaults)



(defvar ess-fl-keyword:fun-calls
  (cons "\\(\\sw+\\) ?(" '(1 ess-function-call-face keep))
  "Font lock for function calls.")

(defvar ess-fl-keyword:numbers
  (cons "\\b\\.?[0-9]+[.eEL]?[0-9]*\\b" 'ess-numbers-face)
  "Numbers")

(defvar ess-fl-keyword:delimiters
  (cons "\\s(\\|\\s)" 'font-lock-builtin-face)
  "Parenthesis")

(defvar ess-fl-keyword:=
  (cons "=" 'font-lock-constant-face)
  "=")

(defvar ess-fl-keyword:operators
  (cons "[-=+></%]+" 'font-lock-constant-face)
  "Operators.")

;;; fl-keywords S
(defvar ess-S-fl-keyword:modifiers
  (cons (concat "\\<" (regexp-opt ess-S-modifyiers 'enc-paren) "\\>")
        'font-lock-constant-face)     ; modify search list or source (i.e. directives)
  "Font-lock keyword R modifiers")

(defvar ess-S-fl-keyword:fun-defs
  (cons ess-S-function-name-regexp
        '(1 font-lock-function-name-face t)  ; override
        )
  "Font-lock function deffinitions keyword.")

(defvar ess-S-fl-keyword:keywords
  (cons (concat "\\<" (regexp-opt ess-S-keywords 'enc-paren) "\\>")
        'font-lock-keyword-face))

(defvar ess-S-fl-keyword:assign-ops
  (cons (regexp-opt ess-S-assign-ops) 'font-lock-constant-face)
  "Font-lock assign operators")

(defvar ess-S-fl-keyword:constants
  (cons (concat "\\<" (regexp-opt ess-S-constants 'enc-paren) "\\>")
        'font-lock-type-face)
  "Font-lock constants keyword.")


(defcustom ess-S-font-lock-keywords
  '((ess-S-fl-keyword:modifiers . t)
    (ess-S-fl-keyword:fun-defs  . t)
    (ess-S-fl-keyword:keywords  . t)
    (ess-S-fl-keyword:assign-ops        . t)
    (ess-S-fl-keyword:constants . t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=)
    )
  "An alist of available font-lock keywords for the S mode.
The key of each cons cell is a name of the keyword. The value
should be t or nil to indicate if the keyword is activated by
default or not."
  :group 'ess-S
  :type 'alist)


;;; fl-keywords R
(defvar ess-R-fl-keyword:modifiers
  (cons (concat "\\<" (regexp-opt ess-R-modifyiers 'enc-paren) "\\>")
        'font-lock-constant-face)     ; modify search list or source (i.e. directives)
  "Font-lock keyword R modifiers")

(defvar ess-R-fl-keyword:fun-defs
  (cons ess-R-function-name-regexp
        '(1 font-lock-function-name-face t)  ; override
        )
  "Font-lock keyword - function defintions for R.")

(defvar ess-R-fl-keyword:keywords
  (cons (concat "\\<" (regexp-opt ess-R-keywords 'enc-paren) "\\>")
        'font-lock-keyword-face))

(defvar ess-R-fl-keyword:assign-ops
  (cons (regexp-opt ess-R-assign-ops) 'font-lock-constant-face)
  "Font-lock assign operators")

(defvar ess-R-fl-keyword:constants
  (cons (concat "\\<" (regexp-opt ess-R-constants 'enc-paren) "\\>")
        'font-lock-type-face)
  "Font-lock constants keyword.")

(defvar ess-R-fl-keyword:numbers
  (cons "\\b[0-9]*[.eE]?[0-9]+[eEL]?\\b" 'ess-numbers-face)
  "Font-lock numbers")

(defvar ess-R-fl-keyword:F&T
  (cons "\\b[FT]\\b" 'font-lock-type-face)
  "Highlight T and F in addition to TRUE and FALSE in R.")

(defvar ess-R-fl-keyword:%op%
  (cons "%[^ \t]*%" 'ess-%op%-face)
  "Highlight %op% operators.")

(defcustom ess-R-font-lock-keywords
  '((ess-R-fl-keyword:modifiers  . t)
    (ess-R-fl-keyword:fun-defs   . t)
    (ess-R-fl-keyword:keywords   . t)
    (ess-R-fl-keyword:assign-ops . t)
    (ess-R-fl-keyword:constants  . t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=)
    (ess-R-fl-keyword:F&T)
    (ess-R-fl-keyword:%op%))
  "An alist of available font-lock keywords for the R mode.
The key of each cons cell is a name of the keyword. The value
should be t or nil to indicate if the keyword is active or not."
  :group 'ess-R
  :type 'alist)


(defvar inferior-ess-font-lock-keywords nil
  "Internal. Holds a name of the dialect sepcific font-lock
keywords in the current buffer. See
`inferior-R-font-lock-keywords' for an example.")
(make-variable-buffer-local 'inferior-ess-font-lock-keywords)

(defvar inferior-ess-font-lock-defaults nil
  "Internal. Holds dialect sepcific font-lock defaults in the
current buffer. Old system. From ESS[12.09] switched to new
system described in `inferior-ess-font-lock-keywords'.")
(make-variable-buffer-local 'inferior-ess-font-lock-defaults)

(defvar comint-highlight-prompt 'comint-highlight-prompt)
;; needed for proper font-lock

(defvar ess-S-fl-keyword:prompt
  (cons (concat "^" inferior-S-prompt) 'comint-highlight-prompt)
  "Highlight prompts missed by comint.")

;; (defvar ess-S-fl-keyword:input-line
;;   (cons "^[a-zA-Z0-9 ]*[>+]\\(.*$\\)" '(1 font-lock-variable-name-face keep t)))

(defvar ess-fl-keyword:matrix-labels
  (cons "\\[,?[1-9][0-9]*,?\\]" 'font-lock-constant-face)
  "Matrix and vector numeric labels.
") ;; also matches subsetting

(defvar ess-R-fl-keyword:messages
  (cons (regexp-opt ess-R-message-prefixes 'enc-paren)
        'font-lock-warning-face)
  "Inferior-ess problems or errors.")

(defcustom inferior-R-font-lock-keywords
  '((ess-S-fl-keyword:prompt   . t) ;; comint does that, but misses some prompts
    ;; (ess-S-fl-keyword:input-line) ;; comint boguously highlights input with text props, no use for this
    (ess-R-fl-keyword:messages  . t)
    (ess-R-fl-keyword:modifiers . t)
    (ess-R-fl-keyword:fun-defs  . t)
    (ess-R-fl-keyword:keywords  . t)
    (ess-R-fl-keyword:assign-ops	. t)
    (ess-R-fl-keyword:constants . t)
    (ess-fl-keyword:matrix-labels	. t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=)
    (ess-R-fl-keyword:F&T)
    ;;VS[17-09-2012]: what is this matching?
    ;; (cons "^\\*\\*\\*.*\\*\\*\\*\\s *$" 'font-lock-comment-face); ess-mode msg

    ;; (cons "#" 'font-lock-comment-face) ; comment
    ;; (cons "^[^#]*#\\(.*$\\)" '(1 font-lock-comment-face keep t)) ; comments
    )
  "Font-lock patterns (alist) used in inferior-R-mode buffers.
The key of each cons cell is a name of the keyword. The value
should be t or nil to indicate if the keyword is active or not."
  :group 'ess-R
  :type 'alist
  )


(defvar ess-S-common-font-lock-keywords nil
  "
NOT used. See `inferior-S-font-lock-keywords'")
(make-obsolete-variable 'ess-S-common-font-lock-keywords nil "ESS[12.09]")

(defvar ess-S-fl-keyword:messages
  (cons (regexp-opt ess-S-message-prefixes 'enc-paren)
        'font-lock-warning-face)
  "Inferior-ess problems or errors.")

(defcustom inferior-S-font-lock-keywords
  '((ess-S-fl-keyword:prompt    . t)
    (ess-S-fl-keyword:messages  . t)
    (ess-S-fl-keyword:modifiers . t)
    (ess-S-fl-keyword:fun-defs  . t)
    (ess-S-fl-keyword:keywords  . t)
    (ess-S-fl-keyword:assign-ops        . t)
    (ess-S-fl-keyword:constants . t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=))
  "Font-lock patterns used in inferior-S-mode buffers.
The key of each cons cell is a name of the keyword. The value
should be t or nil to indicate if the keyword is active by
default."
  :group 'ess-S
  :type 'alist)

;; use the inferior-* ones directly in ess-trns.el
;; (defvar ess-trans-font-lock-keywords
;;   inferior-ess-font-lock-keywords
;;   "Font-lock patterns used in `ess-transcript-mode' buffers.")


;;;*;;; ess-help variables

 ; ess-help-mode
;; This will never need to be loaded independently of any of the other
;; modules, but they can all call it so we may as well put it here.

;;*;; Variables relating to ess-help-mode


(defcustom ess-help-pop-to-buffer t
  "If non-nil ess-help buffers are given focus during the display.
The default is t (except when `focus-follows-mouse' and
`mouse-autoselect-window' are both t)."
  :group 'ess-help
  :type 'boolean)

(defcustom ess-help-own-frame nil
  "Controls whether ESS help buffers should start in a different frame.

Possible values are:
   nil: Display help in current frame.
  'one: All help buffers are shown in one dedicated frame.
     t: Each help buffer gets its own frame.

The parameters of this frame are stored in `ess-help-frame-alist'.
See also `inferior-ess-own-frame'."
  :group 'ess-help
  :type '(choice (const nil) (const one) (const t)))

(defcustom ess-help-reuse-window t
  "If t, ESS tries to display new help buffers in the existing help window"
  :type 'boolean
  :group 'ess-help)

(defcustom ess-help-frame-alist special-display-frame-alist
  "Alist of frame parameters used to create help frames.
This defaults to `special-display-frame-alist' and is used only when
the variable `ess-help-own-frame' is non-nil."
  :group 'ess-help
  :type 'alist)


 ; User changeable variables
;;;=====================================================
;;; Users note: Variables with document strings starting
;;; with a * are the ones you can generally change safely, and
;;; may have to upon occasion.

(defvar ess-function-call-face 'ess-function-call-face
  "Face name to use for highlighting function calls.")

(defvar ess-numbers-face 'ess-numbers-face
  "Face name to use for highlighting numbers.")

(defvar ess-%op%-face 'ess-%op%-face
  "Face name to use for highlighting %op% operators.")

(defface ess-function-call-face
  '((default (:slant normal :inherit font-lock-function-name-face)))
  "Font Lock face used to highlight function calls in ess buffers."
  :group 'ess)

(defface ess-numbers-face
  '((default (:slant normal :inherit font-lock-type-face)))
  "Font Lock face used to highlight numbers in ess-mode buffers."
  :group 'ess)

(defface ess-%op%-face
  '((default (:slant normal :inherit font-lock-keyword-face)))
  "Font Lock face used to highlight %op% operators in ess-mode buffers."
  :group 'ess)


(defcustom ess-help-kill-bogus-buffers t
  "Non-nil means kill ESS help buffers immediately if they are \"bogus\"."
  :group 'ess-help
  :type 'boolean)

(defvar ess-help-form 'separate-buffer
  "*Place to show help.   NOT IMPLEMENTED YET.
Choices are `separate-buffer', `s-process', `www'.  The latter uses
`browse-url' to find the location.")

(defvar ess-help-web-search-command nil
  "Dialect specific command web help search.
Passed to `ess-execute-dialect-specific' which see. ")
(make-variable-buffer-local 'ess-help-web-search-command)

(defvar ess-manual-lookup-command nil
  "Dialect specific command manual lookup.
Passed to `ess-execute-dialect-specific' which see. ")
(make-variable-buffer-local 'ess-manual-lookup-command)

(defvar ess-reference-lookup-command nil
  "Dialect specific command for reference lookup..
Passed to `ess-execute-dialect-specific' which see. ")
(make-variable-buffer-local 'ess-reference-lookup-command)

(defvar ess-funargs-command  nil
  "Dialect specific command to return a list of function arguments.
See `ess-function-arguments' and .ess_funargs command in R and
S+ for details of the format that should be returned.")
(make-variable-buffer-local 'ess-funargs-command)

(defvar ess-eldoc-function nil
  "Holds a dialect specific eldoc function,
See `ess-R-eldoc-function' and `ess-julia-eldoc-function' for examples.")

(defcustom ess-r-args-noargsmsg "No args found."
  "Message returned if \\[ess-r-args-get] cannot find a list of arguments."
  :group 'ess-R
  :type 'string)

(defcustom ess-r-args-show-prefix "ARGS: "
  "A prefix string that is shown before the arguments list."
  :group 'ess-R
  :type 'string)

(defcustom ess-r-args-show-as 'message
  "How ess-r-args-show should show the argument list. Possible values
are: 'message' (the default) or 'tooltip'."
  :group 'ess-R
  :type '(choice
          (const :tag "message" :value 'message)
          (const :tag "tooltip" :value 'tooltip)))

(defcustom ess-r-args-keep-silent ess-S-non-functions
  "List of functions names which should *not* trigger \\[ess-r-args-show];
Defaults to `ess-S-non-functions'."
  :group 'ess-R
  :type '(repeat string))

(defcustom ess-r-args-electric-paren nil
  "Non-nil means re-assign \"(\" to \\[ess-r-args-auto-show]."
  :group 'ess-R
  :type 'boolean)


 ; System variables
;;;=====================================================
;;; Users note: You will rarely have to change these
;;; variables.

;;*;; Variables relating to ess-help-mode

;;-- ess-help-S-.. and  ess-help-R-.. : in  ess-s-l.el (are used in ess-inf).

(defvar ess-help-sec-keys-alist nil
  "Alist of (key . string) pairs for use in section searching.")

(defvar ess-help-sec-regex nil
  "Reg(ular) Ex(pression) of section headers in help file")

(make-variable-buffer-local 'ess-help-sec-keys-alist)
(make-variable-buffer-local 'ess-help-sec-regex)


 ; julia-mode
(defcustom inferior-julia-program-name (if (executable-find "julia-basic")
                                           "julia-basic"
                                         "julia")
  "julia' executable.
Need to be a full path if julia executable is not in the `exec-path'"
  :group 'ess-Julia)

(defvar julia-basic-offset 4
  "Offset for julia code editing")



 ; ess-mode: editing S source

;;; This syntax table is required by ess-mode.el, ess-inf.el and
;;; ess-trns.el, so we provide it here.
(defvar ess-mode-syntax-table nil "Syntax table for `ess-mode'.")
(make-variable-buffer-local 'ess-mode-syntax-table)


 ; Buffer local customization stuff

(defvar ess-source-modes '(ess-mode)
  "A list of modes used to determine if a buffer contains ess source code.")
;;; If a file is loaded into a buffer that is in one of these major modes, it
;;; is considered an ess source file.  The function ess-load-file uses this to
;;; determine defaults.

(defcustom ess-error-buffer-name "*ESS-errors*"
  "Name of buffer to keep process error messages in.
Created for each process."
  :group 'ess-proc
  :type 'string)

(defvar ess-error-regexp-alist nil
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(defcustom ess-verbose nil
  "Non-nil means write more information to `ess-dribble-buffer' than usual."
  :group 'ess-proc
  :type 'boolean)

(defvar ess-dribble-buffer (generate-new-buffer "*ESS*")
  "Buffer for temporary use for setting default variable values.
Used for recording status of the program, mainly for debugging.")

(defvar ess-customize-alist nil
  "Variable settings to use for proper behavior.
Not buffer local!")
;; TODO: fixme We cannot make it local as yet, Not list is set on inferior startup.
;; (make-variable-buffer-local 'ess-customize-alist)
;; (defvaralias 'ess-local-customize-alist 'ess-customize-alist)

(defvar ess-local-customize-alist nil
  "Buffer local settings for proper behavior.
Used to store the values for passing on to newly created buffers.")

(make-variable-buffer-local 'ess-local-customize-alist)

(defvar ess-mode-editing-alist nil
  "Variable settings for ess-mode.")

(defvar ess-transcript-minor-mode nil
  "Non-nil if using `ess-transcript-mode' as a minor mode of some other mode.")

(make-variable-buffer-local 'ess-transcript-minor-mode)

(defvar ess-listing-minor-mode nil
  "Non-nil if using ess-listing-minor-mode.")

(make-variable-buffer-local 'ess-listing-minor-mode)

(provide 'ess-custom)

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

;;; ess-custom.el ends here
