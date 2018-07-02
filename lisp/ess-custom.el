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

;; FIXME:  When Emacs is started from Cygwin shell in Windows,
;;         we have (equal window-system 'x) -and should use "--ess" in *d-r.el
(defvar ess-microsoft-p (memq system-type '(ms-dos windows-nt))
  "Value is t if the OS is one of Microsoft's, nil otherwise.")


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

(defgroup ess-faces nil
  "Faces and face options for ESS modes."
  :group 'ess
  :prefix "ess-")

;; Variables (not user-changeable)

(defvar ess-version "17.11" ;; updated by 'make'
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
                                ("install.packages"     . ess-install-library)
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
  :type 'alist)

(defvar ess--local-handy-commands nil
  "Store handy commands locally")
(make-variable-buffer-local 'ess--local-handy-commands)

(defvar ess-install-library-function nil
  "Dialect-specific function to install a library.")
(make-variable-buffer-local 'ess-install-library-function)


(defcustom ess-describe-at-point-method nil
  "Whether `ess-describe-object-at-point' should use a tooltip.
If nil display in an electric buffer. If 'tooltip display in
a tooltip.

See also `tooltip-hide-delay' and `tooltip-delay'.
 "
  :group 'ess-utils
  :type '(choice (const :tag "buffer" :value nil ) (const tooltip))
  )

(defcustom ess-r-describe-object-at-point-commands
  '(("str(%s)")
    ("htsummary(%s, hlength = 20, tlength = 20)")
    ("summary(%s, maxsum = 20)"))
  "A list of commands cycled by `ess-describe-object-at-point'.
%s is substituted with the name at point.

The value of each element is nil and is not used in current
implementation."
  :group 'R
  :type 'alist)
(defvaralias
  'ess-R-describe-object-at-point-commands
  'ess-r-describe-object-at-point-commands)


(defcustom ess-S-describe-object-at-point-commands
  ess-R-describe-object-at-point-commands
  "An alist of commands cycled by `ess-describe-object-at-point'.
%s is substitute with the name at point. The value is not used as
 yet."
  :group 'S+
  :type 'alist)


(defcustom ess-can-eval-in-background t
  "If non-nil ESS can perform caching and other background
 activities by calling the subprocess on idle time."
  :group 'ess
  :type 'boolean)

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

(defcustom ess-directory-function nil
  "Function to return the directory that ESS is run from.
If nil or if the function returns nil then you get `ess-startup-directory'."
  :group 'ess
  :type '(choice (const nil) function))

(defcustom ess-setup-directory-function nil
  "Function to setup the directory that ESS is run from.
This function can be called to set environment variables or to create
a workspace."
  :group 'ess
  :type '(choice (const nil) function))

(defcustom ess-startup-directory nil
  "The directory ESS is run from.  It must end in a slash.
Provided as a default if `ess-ask-for-ess-directory' is non-nil.
A nil value means use the current buffer's default directory."
  :group 'ess
  :type '(choice (const nil) directory))
(defvaralias 'ess-directory 'ess-startup-directory)

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


(defcustom ess-use-inferior-program-in-buffer-name nil
  "For R, use e.g., 'R-2.1.0' or 'R-devel' (the program name) for buffer name.
Avoids the plain dialect name."
  :group 'ess
  :type 'boolean)
(define-obsolete-variable-alias
  'ess-use-inferior-program-name-in-buffer-name
  'ess-use-inferior-program-in-buffer-name "2018-05-23")

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
See also `ess-first-tab-never-complete'."
  :group 'ess
  :type 'boolean)

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

(defcustom ess-use-flymake t
  "If non-nil activate flymake in ess-mode buffers."
  :group 'ess
  :type 'boolean)

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
settings, such as `ess-r-smart-operators'.")
(make-variable-buffer-local 'ess-smart-operators)

(defvar ess-r-smart-operators nil
  "If nil, don't use any of smart operators.
If t, use all. If an axplicit list of operators, use only those
operators.

In current verion of ESS, it controls the behavior of
ess-smart-comma only, but will be enriched in the near future.")
(defvaralias 'ess-R-smart-operators 'ess-r-smart-operators)

(defvar ess-no-skip-regexp "[ \t\n]*\\'"
  "If `ess-next-code-line' sees this line, it doesn't jump over.

Used to avoid annoying jumping by ess-eval.*-and-step to end of
buffer or end chunks etc.")

(defcustom ess-smart-S-assign-key "_"
  "Key used by `ess-smart-S-assign'.
Should be nil or a \"simple\" key, in other words no key
modifiers.

You may change this to nil at any time. However, if you change it
to another string, it must be set before ESS is loaded."
  :group 'ess-S
  :type '(choice (const :tag "Nothing" :value nil) string))

(defcustom ess-assign-list (cons (if (boundp 'ess-S-assign) ess-S-assign " <- ")
                                 '(" <<- " " = " " -> " " ->> "))
  "List of assignment operators.
`ess-cycle-assignment' uses this list.  These strings must
contain spaces on either side."
  ;; Note that spaces on either side is not strictly true (as in the
  ;; function won't error), but matching <-/<<- is broken without
  ;; them.
  :type '(repeat string)
  :group 'ess)
(defvar ess-S-assign)
(make-obsolete-variable 'ess-S-assign 'ess-assign-list "2018-07-01")

(defcustom ess-r-prettify-symbols
  '(("<-" . (?\s (Br . Bl) ?\s (Bc . Bc) ?←))
    ("->" . (?\s (Br . Bl) ?\s (Bc . Bc) ?→))
    ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                   (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
                   (Bc . Bl) ?- (Br . Br) ?>))
    ("<<-" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                   (Bl . Bl) ?< (Bc . Br) ?- (Bc . Bc) ?-
                   (Bc . Bl) ?< (Br . Br) ?-)))
  ;; Setup prettify-symbols-alist to show "pretty" arrows, but make
  ;; sure that they arrows use the same amount of spacing as <- and
  ;; <<- to ensure indentation does not change when
  ;; prettify-symbols-mode is turned on/off.
  "Alist of symbols prettifications, see `prettify-symbols-alist'.
This gets appended to `prettify-symbols-alist', so set it to nil
if you want to disable R specific prettification."
  :group 'ess-R
  :type '(alist :key-type string :value-type symbol))

;;*;; Variables concerning editing behaviour

(defcustom ess-filenames-map t
  "Declares if the filenames in an attached directory are the same
as objects in that directory (when t). This is not true for DOS and
other OS's with limited filename lengths.  Even if this is set
incorrectly, the right things will probably still happen, however."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-keep-dump-files t
  "Variable controlling whether to delete dump files after a successful load.
If nil: always delete.  If `ask', confirm to delete.  If `check', confirm
to delete, except for files created with ess-dump-object-into-edit-buffer.
Anything else, never delete.  This variable only affects the behaviour
of `ess-load-file'.  Dump files are never deleted if an error occurs
during the load. "
  :group 'ess-edit
  :type '(choice (const :tag "Check" :value  'check)
                 (const :tag "Ask"   :value  'ask)
                 (const :tag "Always keep"   :value t)
                 (const :tag "Always delete"   :value nil)
                 ))

(defcustom ess-delete-dump-files nil
  "Non-nil means delete dump files after they are created.
This applies to dump files created with
`ess-dump-object-into-edit-buffer', only.

Boolean flag which determines what to do with the dump files
generated by \\[ess-dump-object-into-edit-buffer], as follows:

        If non-nil: dump files are deleted after each use, and so appear
only transiently. The one exception to this is when a loading error
occurs, in which case the file is retained until the error is
corrected and the file re-loaded.

        If nil: dump files are not deleted, and backups are kept
as usual.  This provides a simple method for keeping an archive of S
functions in text-file form.

Auto-save is always enabled in dump-file buffers to enable recovery
from crashes.

This is useful to prevent source files being created for objects
you don't actually modify.  Once the buffer is modified and saved
however, the file is not subsequently deleted unless
`ess-keep-dump-files' is nil, and the file is successfully loaded
back into S."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-fill-calls t
  "If non-nil, refilling a paragraph inside a function or
indexing call will arrange the arguments according to
`fill-column' as in:

  fun_call(argument1, argument2,
           argument3, argument4)


Refilling repeatedly cycles through different styles and
eventually to the original formatting.

 The second style formats according to one argument per line:

  fun_call(argument1,
           argument2,
           argument3,
           argument4)

When `ess-fill-calls-newlines' is t, the second style becomes:

  fun_call(
      argument1,
      argument2,
      argument3,
      argument4
  )


Setting `ess-offset-arguments' to `prev-line' or `prev-call'
activates a third style. It keeps one argument per line except
for the first N arguments. N is controlled with a prefix. For
example, calling M-q three times sets N to 1 while calling M-q
twice then C-U 2 M-q sets N to 2. Here what the default produces:

  fun_call(argument1,
      argument2,
      argument3,
      argument4,
      argument5
  )

This style is useful for refilling R6 or ggproto class
definitions.


The blinking of the refilled region can be disabled with
`ess-blink-refilling'."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-fill-continuations t
  "If non-nil, refilling a paragraph inside a continuation of
statements (expressions separated by operators) will arrange all
its elements, never going past `fill-column'.

  lm(outcome ~ pred1 + pred2 +
       pred3 + pred4, data)

Refilling repeatedly cycles through different styles and
eventually to the original formatting.

The second style lay out the statements according to one
expression per line:

  lm(outcome ~
       pred1 +
       pred2 +
       pred3 +
       pred4, data)

The blinking of the refilled region can be disabled with
`ess-blink-refilling'."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-fill-calls-newlines nil
  "When non-nil, the second refilling style produces newlines
after and before the opening and closing delimiters. This is
intended for example for dplyr-style code:

  fun_call(
      argument1,
      argument2,
      argument3,
      argument4
  )

Note that this setting is temporary and likely to be replaced in
the next ESS version by a more comprehensive and flexible way to
set refill styles."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-blink-refilling t
  "When non-nil, refilling a call or a continuation will first
blink the filling region."
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
  "Indent for arguments of function calls or indexing brackets.
This variables has an effect only when the ( or [ are not
directly followed by a new line. See
`ess-offset-arguments-newline' for indentation after closing
newline.

When set to `open-delim', arguments are indented relative to the
opening parenthesis of the closest function call:

  object <- call(argument, other_call(argument,
                                      other_argument))


When set to `prev-call', arguments are indented relative to the
closest function call:

  object <- call(argument, other_call(argument,
                               other_argument))


When set to `prev-line', arguments are indented relative to the
preceding line:

  object <- call(argument, other_call(argument,
      other_argument))

This setting can also be set to a list containing the the offset
type and the offset size, such as `'(prev-call 2)'. Otherwise,
`ess-indent-offset' is used as a default. See `ess-style-alist'
for other offsets controlling indentation.")

(defvar ess-offset-arguments-newline 'prev-call
  "Indent of arguments when ( or [ is followed by a new line.

When set to `open-delim', arguments on a new line are indented
relative to the opening parenthesis of the closest function call:

  object <- call(argument, other_call(
                                      argument,
                                      other_argument
                                      ))


Wnen set to `prev-call', arguments on a new line are indented relative to
the closest function call:

  object <- call(argument, other_call(
                               argument,
                               other_argument
                           ))

You can control the details of indentation at `prev-call' with
`ess-indent-from-lhs' and `ess-indent-from-chain-start'.


When set to `prev-line', arguments on a new line are indented
relative to the preceding line:

  object <- call(argument, other_call(
      argument,
      other_argument
  ))

This setting can also be set to a list containing the the offset
type and the offset size, such as `'(prev-call 2)'. Otherwise,
`ess-indent-offset' is used as a default. See `ess-style-alist'
for other offsets controlling indentation.")

(defvar ess-offset-block 'prev-line
  "Indentation for blocks. A block is usually declared with
braces but a statement wrapped in anonymous parentheses is also
considered a block. This offset can be either `prev-call',
`prev-line' or `open-delim'.

When set to `open-delim', blocks are indented relative to the
opening parenthesis of the closest function call:

  call(argument, other_call(parameter = {
                                stuff
                            }, {
                                stuff
                            }))

  call(argument, lapply(data, function(x) {
                            body
                        }))


When set to `prev-call', blocks are indented relative to the
closest function call:

  call(argument, other_call(parameter = {
                     stuff
                 }, {
                     stuff
                 }))

  call(argument, lapply(data, function(x) {
                     body
                 }))

You can control the details of indentation at `prev-call' with
`ess-indent-from-lhs' and `ess-indent-from-chain-start'.


When set to `prev-line', blocks are indented relative to the
preceding line:

  call(argument, other_call(parameter = {
      stuff
  }, {
      stuff
  }))

  call(argument, lapply(data, function(x) {
      body
  }))

This setting can also be set to a list containing the the offset
type and the offset size, such as `'(prev-call 2)'. Otherwise,
`ess-indent-offset' is used as a default. See `ess-style-alist'
for other offsets controlling indentation.")

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
to control the size of indentation.

See `ess-style-alist' for for an overview of ESS indentation.")

(defvar ess-align-nested-calls '("ifelse")
  "List of strings declaring function calls for which
`ess-offset-arguments-newline' should be ignored. These calls
will be vertically aligned instead. The default is `ifelse',
resulting in the following indentation for nested ifelse calls:

    object <- ifelse(condition1, out1,
              ifelse(condition2, out2, out3))

See `ess-style-alist' for for an overview of ESS indentation.")

(defvar ess-align-arguments-in-calls '("function[ \t]*(")
  "List of regexes specifying the calls where
`ess-offset-arguments' should have no effect on function
declarations. The arguments of those calls will be aligned from
the opening parenthesis.

By default, function declarations are overridden. If for example
`ess-offset-arguments' is set to `prev-line', then function calls
are normally indented as in:

  some_function(argument1,
      argument2, argument3
  )

However, the parameters of function declarations will be
vertically aligned:

  fun <- function(argument1,
                  argument2
                  argument3) {
      body
  }

See `ess-style-alist' for further details.")

(defvar ess-align-continuations-in-calls t
  "Whether continuations inside calls should be indented from the
opening delimiter. This produces the following indentation:

  10 + (1 + 2 +
        3 + 4)
  object[variable1 +
         variable2]

  if (test1 || test2 ||
      test3 || test4) {
      any(test5 &
          test6)
  }

instead of

  10 + (1 + 2 +
            3 + 4)
  object[variable1 +
             variable2]

  if (test1 || test2 ||
        test3 || test4) {
      any(test5 &
            test6)
  }

Definition operators (`<-', `=', `:=' and `~') still trigger an
indentation in all cases. Also, operators at top level and in
curly brackets are not affected by this setting and always induce
an offset:

  {
      var1 +
          var2
  }

See `ess-style-alist' for for an overview of ESS indentation.")

(defvar ess-align-blocks '(control-flow)
  "List of block types for which `ess-offset-blocks' should be
ignored. The overridden blocks are vertically aligned. The list
can contain either or both of the symbols `control-flow' and
`fun-decl'.

With `control-flow', if, else for and while blocks will always be
aligned vertically. With `fun-decl', the body of a function
declaration will always be aligned with the call to
`function'.")

(defvar ess-indent-from-lhs '(arguments fun-decl-opening)
  "List of syntactic elements that should be indented from the
left-hand side of an assignment. The list accepts the symbol
`arguments' and `fun-decl-opening'.

For arguments, this setting only has an effect for offsets set to
`prev-call'. When set, this indentation is produced:

  some_function(parameter = other_function(
                                argument
                            ))

  object <- some_function(
                argument1,
                argument2
            )

instead of:

  some_function(parameter = other_function(
                    argument
                ))

  object <- some_function(
      argument1,
      argument2
  )


`fun-decl-opening' refers to the opening curly following a function
declaration. Setting it produces:

  object <-
      function(argument)
  {
      body
  }

instead of:

  object <-
      function(argument)
      {
          body
      }

This is useful when (a) you have a long function name and want to
break a line after `<-' so that you have room to lay out the
arguments within `fill-column' characters; (b) you still want to
align the function body from the LHS to save horizontal space.

See `ess-style-alist' for for an overview of ESS indentation.")

(defvar ess-indent-from-chain-start t
  "When non-nil, chained calls will be treated as if they were
one call and indentation will start from the first one. This
setting only has an effect for offsets set to `prev-call' or
block offsets set to `opening-delim'.

If `nil':

  some_function(other_function(
                    argument
                ))

If `t':

  some_function(other_function(
      argument
  ))

See `ess-style-alist' for for an overview of ESS indentation.")

;;added rmh 2Nov97 at request of Terry Therneau
(defcustom ess-indent-with-fancy-comments t
  "Non-nil means distiguish between #, ##, and ### for indentation.
See `ess-style-alist' for for an overview of ESS indentation."
  :type 'boolean
  :group 'ess-edit)

(define-obsolete-variable-alias 'ess-fancy-comments 'ess-indent-with-fancy-comments "15.09")
(define-obsolete-variable-alias 'ess-arg-function-offset 'ess-indent-from-lhs "15.09")
(define-obsolete-variable-alias 'ess-arg-function-offset-new-line 'ess-offset-arguments-newline "15.09")
(define-obsolete-variable-alias 'ess-first-continued-statement-offset 'ess-offset-continued "15.09")
(define-obsolete-variable-alias 'ess-continued-statement-offset 'ess-offset-continued "15.09")


;;;*;;; Editing styles

(defvar ess-style-alist
  `((BSD
     (ess-indent-offset                . 8)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . prev-call)
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (C++
     (ess-indent-offset                . 4)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . prev-call)
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . (arguments))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    ;; CLB added rmh 2Nov97 at request of Terry Therneau
    (CLB
     (ess-indent-offset                . ,(default-value 'ess-indent-offset))
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . (straight 4))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (GNU
     (ess-indent-offset                . ,(default-value 'ess-indent-offset))
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . (prev-call 4))
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (K&R
     (ess-indent-offset                . 5)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . prev-call)
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    ;; added ajr 17.Feb'04 to match "common R" use (== DEFAULT apart from  offset = 4)
    (RRR
     (ess-indent-offset                . 4)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (RRR+
     (ess-indent-offset                . 4)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . open-delim)
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . (arguments))
     (ess-indent-from-chain-start      . nil)
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (RStudio
     (ess-indent-offset                . ,(default-value 'ess-indent-offset))
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . prev-line)
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . nil)
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . nil)
     (ess-align-blocks                 . nil)
     (ess-indent-from-lhs              . (arguments))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . nil))

    (RStudio-
     (ess-indent-offset                . ,(default-value 'ess-indent-offset))
     (ess-offset-arguments             . prev-line)
     (ess-offset-arguments-newline     . prev-line)
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . nil)
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . nil)
     (ess-align-blocks                 . nil)
     (ess-indent-from-lhs              . (arguments))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . nil))

    (DEFAULT
      (ess-indent-offset                . ,(default-value 'ess-indent-offset))
      (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
      (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
      (ess-offset-block                 . ,(default-value 'ess-offset-block))
      (ess-offset-continued             . ,(default-value 'ess-offset-continued))
      (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
      (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
      (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
      (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
      (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
      (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
      (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments))))

  "Predefined formatting styles for ESS code. Use
`ess-default-style' to apply a style in all R buffers. The values
of all styles except OWN are fixed. To change the value of
variables in the OWN group, customize the variable
`ess-own-style-list'. DEFAULT style picks default (aka global)
values from ESS indentation variables. In addition, ESS provides
many indentation styles, the most important being the RRR and the
RStudio variants.

RRR is the common R style that adheres closely to R internal
standards. RRR+ is the same except it also aligns blocks in
function calls with the opening delimiter, producing more
indentation. The C++ style (named like this for historical
reasons rather than any resemblance to existing C++ indentation
schemes) is halfway between these two styles and indent block
arguments from the start of the surrounding function's name.

The RStudio style closely mimics the indentation of the RStudio
editor. RStudio- is the same except it does not align arguments
in function calls, which corresponds to the settings of some
RStudio users.

ESS indentation is fully specified by the following offsets and
variables. See the documentation of these variables for examples.

Offsets:

 - `ess-indent-offset': main offset inherited by other settings

 - `ess-offset-arguments': offset type for function and bracket
   arguments

 - `ess-offset-arguments-newline': offset type of arguments
   when ( or [ is followed by a new line.

 - `ess-offset-block': offset type for brace and anonymous
   parenthesis blocks

 - `ess-offset-continued': offset type for continuation lines in
   multiline statements


Overrides (implies vertical alignment):

 - `ess-align-nested-calls': functions whose nested calls
   should be aligned.

 - `ess-align-arguments-in-calls': calls where
   `ess-offset-arguments' should be ignored

 - `ess-align-continuations-in-calls': whether to ignore
   `ess-offset-continued' in calls.

 - `ess-align-blocks': whether to ignore `ess-offset-blocks' for
   function declarations or control flow statements.


Control variables:

 - `ess-indent-from-lhs': whether to indent arguments from
   left-hand side of an assignment or parameter declaration.

 - `ess-indent-from-chain-start': whether to indent arguments from
   the first of several consecutive calls.

 - `ess-indent-with-fancy-comments': whether to indent #, ## and
   ### comments distinctly.")

(defun ess-add-style (key entries)
  "Add a new style to `ess-style-list', with the key KEY.
Remove any existing entry with the same KEY before adding the new one."
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
                 (const RRR+)
                 (const RStudio)
                 (const RStudio-)
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
                                 "rdname" "section" "slot" "description"
                                 "md")
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
  :type '(alist :key-type string :value-type string))

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
  :type '(repeat string))

(defcustom ess-swv-plug-into-AUCTeX-p nil
  "Non-nil means add commands to AUCTeX's \\[TeX-command-list]
to sweave the current noweb file and latex the result."
  :group 'ess-sweave
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defvar ess-roxy-insert-prefix-on-newline t
  "When non-nil, `ess-newline-and-indent' will make sure the new
line starts with the roxy prefix.")

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

(defcustom ess-gen-proc-buffer-name-function 'ess-gen-proc-buffer-name:projectile-or-simple
  "Function used for generation of the buffer name of the newly created ESS process.
It should accept one argument PROC-NAME, a string specifying
internal process name (R, R:2, etc).

Provided default options are:

  `ess-gen-proc-buffer-name:simple'    -- *proc*
  `ess-gen-proc-buffer-name:directory' -- *proc:dir*
  `ess-gen-proc-buffer-name:abbr-long-directory' -- *proc:abbr-long-dir*
  `ess-gen-proc-buffer-name:projectile-or-simple'    -- *proc:projectile-root* or *proc*.
  `ess-gen-proc-buffer-name:projectile-or-directory' -- *proc:projectile-root* or *proc:dir*.

Strategies based on projectile default to built-in strategies if
projectile.el is not loaded or there is no project root in the
current directory.
"
  :group 'ess
  :type '(choice (const :tag "*proc*"     ess-gen-proc-buffer-name:simple)
                 (const :tag "*proc:dir*" ess-gen-proc-buffer-name:directory)
                 (const :tag "*proc:abbr-long-dir*" ess-gen-proc-buffer-name:abbr-long-directory)
                 (const :tag "*proc:projectile-root* or *proc*"     ess-gen-proc-buffer-name:projectile-or-simple)
                 (const :tag "*proc:projectile-root* or *proc:dir*" ess-gen-proc-buffer-name:projectile-or-directory)
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
;; ess-r-set-function-start

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

(defcustom inferior-ess-jit-lock-chunk-size 10000
  "Default for (buffer local) `jit-lock-chunk-size' in inferior ESS buffers."
  :group 'ess-proc
  :type 'integer)


(defcustom inferior-ess-r-program (or (executable-find "Rterm")
                                      (executable-find "R")
                                      "R")
  "Program name for invoking an inferior ESS with \\[R]."
  :group 'ess-R
  :type '(choice (string) file))
(defvaralias 'inferior-R-program 'inferior-ess-r-program)
(define-obsolete-variable-alias 'inferior-R-program-name
  'inferior-ess-r-program "2018-05-23")
(define-obsolete-variable-alias 'inferior-ess-r-program-name
  'inferior-ess-r-program "2018-05-23")

(defcustom inferior-R-args ""
  "String of arguments (see 'R --help') used when starting R,
including the versions of R created via variable `ess-r-versions'."
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
  :group 'ess-Stata
  :type '(choice (const nil) string))

(defcustom inferior-STA-start-args ""
  "String of switches used when starting stata.
Don't use this to send initialization command to stata, use
`inferior-STA-start-file' instead. Also see
`inferior-STA-program'."
  :group 'ess-Stata
  :type 'string)

(defcustom inferior-ess-r-objects-command "print(objects(pos=%d, all.names=TRUE), max=1e6)\n"
  "Format string for R command to get a list of objects at position %d.
Used in e.g., \\[ess-execute-objects] or \\[ess-display-help-on-object]."
  :group 'ess-command
  :type 'string)
(defvaralias 'inferior-R-objects-command 'inferior-ess-r-objects-command)

(defvar ess-getwd-command nil
  "Command string retriving the working directory from the process.")

(defvar ess-setwd-command nil
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
  :type '(choice (string) (const nil)))

(defcustom ess-program-files-64 ;; 64 bit version
  (if (and ess-microsoft-p (getenv "ProgramW6432"))
      (w32-short-file-name (getenv "ProgramW6432"))
    nil)
  "Safe (no embedded blanks) 8.3 name for 64-bit programs that works across internationalization."
  :group 'ess
  :type '(choice (string) (const nil)))

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
  :type '(choice (directory) (const nil)))

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

(defcustom inferior-S3-program "/disk05/s/S"
  "Program name for invoking an inferior ESS with S3()."
  :group 'ess-S
  :type 'string)
(define-obsolete-variable-alias 'inferior-S3-program-name
  'inferior-S3-program "2018-05-23")

(defcustom inferior-S+3-program (or (executable-find "Splus")
                                    "Splus")
  "Program name for invoking an inferior ESS with S+3()."
  :group 'ess-SPLUS
  :type '(choice (string) file))
(define-obsolete-variable-alias 'inferior-S+3-program-name
  'inferior-S+3-program "2018-05-23")

(defcustom inferior-S+4-program
  (concat ess-program-files "/spls45se/cmd/Splus.exe")
  "Program name for invoking an external GUI S+4.
The default value is correct for a default installation of
S-Plus 4.5 Student Edition and with bash as the shell.
For any other version or location, change this value in ess-site.el or
site-start.el.  Use the 8.3 version of the pathname.
Use double backslashes if you use the msdos shell."
  :group 'ess-SPLUS
  :type 'string)
(define-obsolete-variable-alias 'inferior-S+4-program-name
  'inferior-S+4-program "2018-05-23")

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

(defcustom inferior-Sqpe+4-program
  (concat ess-program-files "/spls45se/cmd/Sqpe.exe")
  "Program name for invoking an inferior ESS with Sqpe+4()."
  :group 'ess-SPLUS
  :type '(choice (const nil) (string)))
(define-obsolete-variable-alias 'inferior-Sqpe+4-program-name
  'inferior-Sqpe+4-program "2018-05-23")

(defcustom inferior-Sqpe+4-SHOME-name
  (if ess-microsoft-p (concat ess-program-files "/spls45se" ""))
  "SHOME name for invoking an inferior ESS with Sqpe+4().
The default value is correct for a default installation of
S-Plus 4.5 Student Edition.  For any other version or location,
change this value in ess-site.el or site-start.el.  Use the 8.3
version of the pathname."
  :group 'ess-SPLUS
  :type '(choice (const nil) (string)))
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


(defcustom inferior-S-elsewhere-program "sh"
  "Program name to invoke an inferior ESS with S on a different computer."
  :group 'ess-proc
  :type 'string)
(define-obsolete-variable-alias
  'inferior-S-elsewhere-program-name
  'inferior-S-elsewhere-program "2018-05-23")

(defcustom inferior-ESS-elsewhere-program "sh"
  "Program name to invoke an inferior ESS with program on a
different computer."
  :group 'ess-proc
  :type 'string)
(define-obsolete-variable-alias
  'inferior-ESS-elsewhere-program-name
  'inferior-ESS-elsewhere-program "2018-05-23")

(defcustom inferior-S4-program (or (executable-find "S4")
                                   "S4")
  "Program name to invoke an inferior ESS with S4()."
  :group 'ess-S
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-S4-program-name
  'inferior-S4-program "2018-05-23")

(defcustom inferior-S+5-program (or (executable-find "Splus5")
                                    "Splus5")
  "Program name to invoke an inferior ESS with S+5()."
  :group 'ess-SPLUS
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-S+5-program-name
  'inferior-S+5-program "2018-05-23")

(defvaralias 'S+6-dialect-name 'S+-dialect-name)
(defcustom S+-dialect-name "S+"
  "Name of 'dialect' for S-PLUS 6.x and later.
Easily changeable in a user's `.emacs'."
  :group 'ess-SPLUS
  :type 'string)

(defvaralias 'inferior-S+6-program 'inferior-S+-program)
(define-obsolete-variable-alias 'inferior-S+6-program-name
  'inferior-S+-program "2018-05-23")

(defcustom inferior-S+-program
  (if ess-microsoft-p
      (concat ess-program-files "/TIBCO/splus82/cmd/Splus.exe")
    (or (executable-find "Splus")
        "Splus"))
  "Program name to invoke S+.
On Unix/Linux, use the Splus executable.  On Windows, the default
value is correct for a default installation of S-Plus 8.1 and
with bash as the shell.  For any other version or location,
change this value in ess-site.el or site-start.el.  Use the 8.3
version of the pathname.  Use double backslashes if you use the
msdos shell."
  :group 'ess-SPLUS
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-S+-program-name
  'inferior-S+-program "2018-05-23")

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

(defvaralias 'inferior-Sqpe+6-program 'inferior-Sqpe+-program)
(defcustom inferior-Sqpe+-program
  (concat ess-program-files "/TIBCO/splus82/cmd/Sqpe.exe")
  "Program name for invoking an inferior ESS with Sqpe+6() for Windows."
  :group 'ess-S
  :type '(choice (const nil) (string)))
(define-obsolete-variable-alias 'inferior-Sqpe+-program-name
  'inferior-Sqpe+-program "2018-05-23")

(defvaralias 'inferior-Sqpe+6-SHOME-name 'inferior-Sqpe+-SHOME-name)
(defcustom inferior-Sqpe+-SHOME-name
  (if ess-microsoft-p (concat ess-program-files "/TIBCO/splus82" ""))
  "SHOME name for invoking an inferior ESS with Sqpe+6 and higher for Windows.
The default value is correct for a default installation of
S-Plus 8.1.  For any other version or location,
change this value in ess-site.el or site-start.el.  Use the 8.3
version of the pathname."
  :group 'ess-SPLUS
  :type '(choice (const nil) (string)))
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

(defcustom inferior-XLS-program (or (executable-find "xlispstat")
                                    "xlispstat")
  "Program name for invoking an inferior ESS with \\[XLS]."
  :group 'ess-XLS
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-XLS-program-name
  'inferior-XLS-program "2018-05-23")

(defcustom inferior-VST-program (or (executable-find "vista")
                                    "vista")
  "Program name for invoking an inferior ESS with \\[ViSta]."
  :group 'ess-XLS
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-VST-program-name
  'inferior-VST-program "2018-05-23")

(defcustom inferior-ARC-program (or (executable-find "arc")
                                    "arc")
  "Program name for invoking an inferior ESS with \\[ARC]."
  :group 'ess-XLS
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-ARC-program-name
  'inferior-ARC-program "2018-05-23")

(defcustom inferior-SAS-program (or (executable-find "sas")
                                    "sas")
  "Program name for invoking an inferior ESS with SAS()."
  :group 'ess-sas
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-SAS-program-name
  'inferior-SAS-program "2018-05-23")

(defcustom inferior-STA-program (or (executable-find "stata")
                                    "stata")
  "Program name for invoking an inferior ESS with stata().
This is NOT Stata, because we need to call stata with TERM=emacs in
order for it to work right.  And Emacs is too smart for it."
  :group 'ess-Stata
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-STA-program-name
  'inferior-STA-program "2018-05-23")

(defcustom ess-sta-delimiter-friendly nil
  "Non-nil means convert embedded semi-colons to newlines for Stata processing."
  :group 'ess-Stata
  :type 'boolean)

(defcustom inferior-OMG-program (or (executable-find "omegahat")
                                    "omegahat")
  "Program name for invoking an inferior ESS with omegahat()."
  :group 'ess-OMG
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-OMG-program-name
  'inferior-OMG-program "2018-05-23")


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

(defcustom ess-r-editor "emacsclient"
  "Editor called by R process with 'edit()' command."
  :group 'ess
  :type 'string)
(defvaralias 'R-editor 'ess-r-editor)

(defcustom ess-r-pager 'nil ; Usually nil is correct as ESS and page() cooperate.
  "Pager called by R process with 'page()' command."
  :group 'ess
  :type '(choice (const nil) string))
(defvaralias 'R-pager 'ess-r-pager)


(defcustom S-editor "emacsclient"
  "Editor called by S process with 'edit()' command."
  :group 'ess
  :type 'string)

(defcustom S-pager
  (if ess-microsoft-p "emacsclientw.exe" "emacsclient")
  "Pager called by S process with 'page()' command."
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
  :type '(choice (const nil) (string)))
(make-variable-buffer-local 'inferior-ess-help-filetype)
(setq-default inferior-ess-help-filetype nil)


;;;;; names for communication using MS-Windows 9x/NT ddeclient mechanism

(defcustom inferior-ess-ddeclient nil
  "ddeclient is the intermediary between emacs and the stat program."
  :group 'ess-proc
  :type '(choice (const nil) (string)))

(defcustom inferior-ess-client-name nil
  "Name of ESS program ddeclient talks to."
  :group 'ess-proc
  :type '(choice (const nil) (string)))

(defcustom inferior-ess-client-command nil
  "ddeclient command sent to the ESS program."
  :group 'ess-proc
  :type '(choice (const nil) string))

(make-variable-buffer-local 'inferior-ess-client-name)
(make-variable-buffer-local 'inferior-ess-ddeclient)
(make-variable-buffer-local 'inferior-ess-client-command)


;;;;; user settable defaults
(defvar inferior-S-program  inferior-S+3-program
  "Program name for invoking an inferior ESS with S.")
;;- (setq inferior-S-program
;;-       (cond ((string= S-proc-prefix "S") "Splus")
;;-         ((string= S-proc-prefix "R") "R")
;;-         (t "S")
;;-         ))
;;(make-local-variable 'inferior-S-program)
(define-obsolete-variable-alias 'inferior-S-program-name
  'inferior-S+3-program "2018-05-23")

(defvar inferior-ess-program nil ;inferior-S-program
  "Default program name for invoking inferior-ess.
The other variables ...-program should be changed, for the
corresponding program.")
(define-obsolete-variable-alias 'inferior-ess-program-name
  'inferior-ess-program "2018-05-23")

(make-variable-buffer-local 'inferior-ess-program)
;; (setq-default inferior-ess-program inferior-S-program)


(defvar inferior-ess-start-args ""
  "String of arguments passed to the ESS process.
If you wish to pass arguments to a process, see e.g. `inferior-R-args'.")

(defcustom inferior-ess-start-file nil
  "File dumped into process, if non-nil."
  :group 'ess-proc
  :type '(choice (const nil) file))

(defcustom inferior-ess-pager (if ess-microsoft-p "console" "cat")
  "Pager to use for reporting help files and similar things."
  :group 'ess-proc
  :type 'string)

(defvar inferior-ess-primary-prompt "> "
  "Regular expression used by `ess-mode' to detect the primary prompt.")
(make-variable-buffer-local 'inferior-ess-primary-prompt)

(defvar inferior-ess-secondary-prompt nil
  "Regular expression used by ess-mode to detect the secondary prompt.
This is issued by S to continue an incomplete expression.
Set to nil if language doesn't support secondary prompt.")
(make-variable-buffer-local 'inferior-ess-secondary-prompt)

(defvar ess-traceback-command nil
  "Command to generate error traceback.")

;; need this to recognise  + + + > > >
;; and "+ . + " in tracebug prompt
(defvar inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)+"
  "Regexp used in S and R inferior and transcript buffers for prompt navigation.
Must be anchored to BOL.")

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
 (see \\[deactivate-mark])."
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
;; VS[06-04-2016]: fixme: move all inf vars into ess-inf.el.

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

(defvar ess-load-command "source('%s')\n"
  "Dialect specific format-string for building the ess command to load a file.

This format string should use %s to substitute a file name and should
result in an ESS expression that will command the inferior ESS to load
that file.")

(defvar ess-eval-command nil
  "Dialect specific format-string for building the command to evaluate a string.

It is usually faster to send a string to remote processes than a
file.  The latter involves Tramp and can be quite slow.  When
possible, a dialect should implement that command and use it
preferentially.

This format string should use %s as a placeholder for the string
to be evaluated and, optionally, %f for the file name to be
reported in the error references.

The resulting command should not echo code or print any
transitory output.  See also `ess-eval-visibly-command' and
`ess-eval-visibly-noecho-command'.")

(defvar ess-build-eval-message-function nil
  "Dialect-specific function for formatting an evaluation message.")

(make-variable-buffer-local 'ess-eval-command)
(make-variable-buffer-local 'ess-load-command)
(make-variable-buffer-local 'ess-build-eval-message-function)

(define-obsolete-variable-alias 'inferior-ess-load-command 'ess-load-command "ESS v13.09")

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


(defcustom inferior-ess-r-help-command ".ess.help('%s')\n"
  "Format-string for building the R command to ask for help on an object.

This format string should use %s to substitute an object name.
If set, changes will take effect when next R session is started."
  :group 'ess-command
  :type 'string)

(defvar ess-get-help-topics-function nil
  "Dialect specific help topics retrieval")

(defvar ess-display-help-on-object-function nil
  "Dialect specific function for displaying help on object.")

(defvar ess-find-help-file-function nil
  "Dialect specific function for displaying help on object.")

(defvar ess-build-help-command-function nil
  "Dialect specific function for building an help command.")

(make-variable-buffer-local 'ess-get-help-topics-function)
(make-variable-buffer-local 'ess-display-help-on-object-function)
(make-variable-buffer-local 'ess-find-help-file-function)
(make-variable-buffer-local 'ess-build-help-command-function)

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

(defvar ess-r-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|library(\\|require(\\|source(\\)"
  "The regexp for matching the R commands that change the search path.")
(defvaralias 'ess-R-change-sp-regexp 'ess-r-change-sp-regexp)

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
  '("if" "else" "repeat" "while" "function" "for" "in" "next" "break")
  "Reserved words in the R language.")

(defvar ess-R-control-flow-keywords
  '("switch" "function" "return" "on.exit" "stop"
    "tryCatch" "withRestarts" "invokeRestart"
    "recover" "browser")
  "Keywords that impact control flow.
These keywords either cause a control flow jump or establish a
jump target.")

(defvar ess-R-signal-keywords
  '("message" "warning" "signalCondition" "withCallingHandlers")
  "Keywords for condition signalling.
These keywords might cause a control flow jump but do not necessarily.")

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
  '("<<-" "<-" "->" "->>") ; don't want "=" here which is not only for assign
  ;; VS??: it's good to have different colour for = anyhow,
  ;; very helpful to read code like foo(x=xa, p=pa, x_not_na)
  )
(defvar ess-S-assign-ops ess-R-assign-ops) ; since "_" is deprecated for S-plus as well

(defvar ess-R-function-name-regexp
  (concat "\\("      "\\sw+" "\\)"
          "[ \t]*"   "\\(<-\\)"
          "[ \t\n]*" "function\\b"))

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
  (cons "\\(\\sw+\\)[\t ]*(" '(1 ess-function-call-face keep))
  "Font lock for function calls.")

(defvar ess-fl-keyword:numbers
  (cons "\\b\\.?[0-9]+[.eEL]?[0-9]*\\b" 'ess-numbers-face)
  "Font lock for numbers.")

(defvar ess-fl-keyword:delimiters
  (cons "\\s(\\|\\s)" 'ess-paren-face)
  "Font lock for parenthesis.")

(defvar ess-fl-keyword:=
  (cons "=" 'ess-paren-face)
  "Font lock for equal sign (=).")

(defvar ess-fl-keyword:operators
  (cons "[-=+></%]+" 'ess-operator-face)
  "Operators.")

;;; fl-keywords S
(defvar ess-S-fl-keyword:modifiers
  (cons (regexp-opt ess-S-modifyiers 'words)
        'ess-modifiers-face)     ; modify search list or source (i.e. directives)
  "Font lock keyword R modifiers.")

(defvar ess-S-fl-keyword:fun-defs
  (cons ess-S-function-name-regexp
        '(1 font-lock-function-name-face t)  ; override
        )
  "Font-lock function definitions keyword.")

(defvar ess-S-fl-keyword:keywords
  (cons (regexp-opt ess-S-keywords 'words) 'ess-keyword-face))

(defvar ess-S-fl-keyword:assign-ops
  (cons (regexp-opt ess-S-assign-ops) 'ess-assignment-face)
  "Font-lock assign operators")

(defvar ess-S-fl-keyword:constants
  (cons (regexp-opt ess-S-constants 'words) 'ess-constant-face)
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
  :group 'ess-faces
  :type 'alist)


;;; fl-keywords R
(defvar ess-R-fl-keyword:modifiers
  (cons (concat "\\(" (regexp-opt ess-R-modifyiers 'words) "\\)\\s-*(")
        '(1 ess-modifiers-face))     ; modify search list or source (i.e. directives)
  "Font-lock keyword R modifiers.")

(defvar ess-R-fl-keyword:fun-defs
  (cons ess-R-function-name-regexp
        '(1 font-lock-function-name-face nil))
  "Font-lock keyword - function defintions for R.")

(defvar ess-r--bare-keywords
  '("in" "else" "break" "next" "repeat"))

(defvar ess-R-fl-keyword:bare-keywords
  (cons (regexp-opt ess-r--bare-keywords 'words)
        'ess-keyword-face)
  "Font-lock keywords that do not precede an opening parenthesis.")

(defvar ess-R-fl-keyword:keywords
  (let ((function-kwords
         (delq nil
               (mapcar (lambda (k) (unless (member k ess-r--bare-keywords) k))
                       ess-R-keywords))))
    (cons (concat "\\(" (regexp-opt function-kwords 'words) "\\)\\s-*(")
          '(1 ess-keyword-face)))
  "Font-lock keywords that precede an opening parenthesis.")

(defvar ess-R-fl-keyword:control-flow-keywords
  (cons (concat "\\(" (regexp-opt ess-R-control-flow-keywords 'words) "\\)\\s-*(")
        '(1 ess-r-control-flow-keyword-face)))

(defvar ess-R-fl-keyword:signal-keywords
  (cons (concat "\\(" (regexp-opt ess-R-signal-keywords 'words) "\\)\\s-*(")
        '(1 ess-r-signal-keyword-face)))

(defvar ess-R-fl-keyword:assign-ops
  (cons (regexp-opt ess-R-assign-ops) 'ess-assignment-face)
  "Font-lock assign operators.")

(defvar ess-R-fl-keyword:constants
  (cons (regexp-opt ess-R-constants 'words) 'ess-constant-face)
  "Font-lock constants keyword.")

(defvar ess-R-fl-keyword:numbers
  (cons "\\b[0-9]*[.eE]?[0-9]+[eEL]?\\b" 'ess-numbers-face)
  "Font-lock numbers")

(defvar ess-R-fl-keyword:F&T
  (cons "\\b[FT]\\b" 'ess-f-t-face)
  "Highlight T and F in addition to TRUE and FALSE in R.")

(defcustom ess-R-font-lock-keywords
  '((ess-R-fl-keyword:modifiers  . t)
    (ess-R-fl-keyword:fun-defs   . t)
    (ess-R-fl-keyword:keywords . t)
    (ess-R-fl-keyword:bare-keywords . t)
    (ess-R-fl-keyword:control-flow-keywords . t)
    (ess-R-fl-keyword:signal-keywords . t)
    (ess-R-fl-keyword:assign-ops . t)
    (ess-R-fl-keyword:constants  . t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=)
    (ess-R-fl-keyword:F&T))
  "An alist of available font-lock keywords for the R mode.
The key of each cons cell is a name of the keyword. The value
should be t or nil to indicate if the keyword is active or not."
  :group 'ess-R
  :group 'ess-faces
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

(defvar ess-fl-keyword:matrix-labels
  ;; also matches subsetting
  (cons "\\[,?[1-9][0-9]*,?\\]" 'ess-matrix-face)
  "Matrix and vector numeric labels.")

(defvar ess-R-fl-keyword:messages
  (cons (regexp-opt ess-R-message-prefixes 'enc-paren)
        'font-lock-warning-face)
  "Inferior-ess problems or errors.")

(defcustom inferior-ess-r-font-lock-keywords
  '((ess-S-fl-keyword:prompt   . t) ;; comint is bad at prompt highlighting
    (ess-R-fl-keyword:messages  . t)
    (ess-R-fl-keyword:modifiers . t)
    (ess-R-fl-keyword:fun-defs  . t)
    (ess-R-fl-keyword:bare-keywords . t)
    (ess-R-fl-keyword:keywords . t)
    (ess-R-fl-keyword:assign-ops	. t)
    (ess-R-fl-keyword:constants . t)
    (ess-fl-keyword:matrix-labels	. t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=)
    (ess-R-fl-keyword:F&T))
  "Font-lock patterns used in inferior-R-mode buffers.
The key of each cons cell is a name of the keyword.  The value
should be t or nil to indicate if the keyword is active or not."
  :group 'ess-R
  :group 'ess-faces
  :type 'alist
  )
(defvaralias 'inferior-R-font-lock-keywords 'inferior-ess-r-font-lock-keywords)


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
The key of each cons cell is a name of the keyword.  The value
should be t or nil to indicate if the keyword is active by
default."
  :group 'ess-S
  :group 'ess-faces
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

Note if you set this to t you should also set
`ess-help-reuse-window' to nil to ensure that help buffers are
displayed in a new frame.

The parameters of the own frame are stored in `ess-help-frame-alist'.
See also `inferior-ess-own-frame'."
  :group 'ess-help
  :type '(choice (const :tag "Display in current frame" nil)
                 (const :tag "Display in one frame" one)
                 (const :tag "Always display in a new frame" t)))

(defcustom ess-help-reuse-window t
  "If t, ESS tries to display new help buffers in the existing help window."
  :type 'boolean
  :group 'ess-help)

(defcustom ess-help-frame-alist default-frame-alist
  "Alist of frame parameters used to create help frames.
This defaults to `default-frame-alist' and is used only when
the variable `ess-help-own-frame' is non-nil."
  :group 'ess-help
  :type 'alist)


 ; Faces
;;;=====================================================

(defconst ess-function-call-face 'ess-function-call-face)
(defface ess-function-call-face
  '((default (:slant normal :inherit font-lock-function-name-face)))
  "Font Lock face used to highlight function calls in ess buffers."
  :group 'ess-faces)

(defconst ess-numbers-face 'ess-numbers-face)
(defface ess-numbers-face
  '((default (:slant normal :inherit font-lock-type-face)))
  "Font Lock face used to highlight numbers in ess-mode buffers."
  :group 'ess-faces)

(defconst ess-backquoted-face 'ess-backquoted-face)
(defface ess-backquoted-face
  '((default (:inherit default)))
  "Font Lock face for backquoted names."
  :group 'ess-faces)

(defconst ess-operator-face 'ess-operator-face)
(defface ess-operator-face
  '((default (:inherit font-lock-constant-face)))
  "Font Lock face for operators."
  :group 'ess-faces)

(defconst ess-%op%-face 'ess-%op%-face)
(defface ess-%op%-face
  '((default (:inherit ess-operator-face)))
  "Font Lock face used to highlight %op% operators in ess-mode buffers."
  :group 'ess-faces)

(defconst ess-assignment-face 'ess-assignment-face)
(defface ess-assignment-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight assignment operators."
  :group 'ess-faces)

(defconst ess-paren-face 'ess-paren-face)
(defface ess-paren-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight parentheses."
  :group 'ess-faces)

(defconst ess-operator-face 'ess-operator-face)
(defface ess-operator-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight operators."
  :group 'ess-faces)

(defconst ess-modifiers-face 'ess-modifiers-face)
(defface ess-modifiers-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight modifiers.
In `R-mode', for example, this includes \"library,\" \"attach,\"
and others, see `ess-R-modifyiers'."
  :group 'ess-faces)

(defconst ess-constant-face 'ess-constant-face)
(defface ess-constant-face
  '((default (:inherit font-lock-type-face)))
  "Font lock face used to highlight constants.
In `R-mode', for example, this includes TRUE, FALSE, Inf and
others. See `ess-R-constants'."
  :group 'ess-faces)

(defconst ess-f-t-face 'ess-f-t-face)
(defface ess-f-t-face
  '((default (:inherit ess-constant-face)))
  "Font lock face used to highlight F and T."
  :group 'ess-faces)

(defconst ess-matrix-face 'ess-matrix-face)
(defface ess-matrix-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight row/column labels in matrices."
  :group 'ess-faces)

(defconst ess-keyword-face 'ess-keyword-face)
(defface ess-keyword-face
  '((default (:inherit font-lock-keyword-face)))
  "Font lock face used to highlight reserved keywords.
In `R-mode', for example, this includes \"while,\" \"if/else\",
\"function,\" and others. See `ess-R-keywords'."
  :group 'ess-faces)

(defconst ess-r-control-flow-keyword-face 'ess-r-control-flow-keyword-face)
(defface ess-r-control-flow-keyword-face
  '((default (:inherit ess-keyword-face)))
  "Font lock face used to highlight control flow keywords.
In `R-mode', for example, this includes \"switch(),\" \"tryCatch()\",
and \"stop(),\". See `ess-R-control-flow-keywords'.

By default, these keywords are highlighted with the same face as
`ess-R-keywords'"
  :group 'ess-faces)

(defconst ess-r-signal-keyword-face 'ess-r-signal-keyword-face)
(defface ess-r-signal-keyword-face
  '((default (:inherit ess-modifiers-face)))
  "Font lock face used to highlight weak keywords.
In `R-mode', for example, this includes \"message(),\" \"warning()\",
and \"withCallingHandlers(),\". See `ess-R-signal-keywords'.

By default, these keywords are highlighted with the same face as
`ess-R-modifyiers'"
  :group 'ess-faces)

(defcustom ess-help-kill-bogus-buffers t
  "Non-nil means kill ESS help buffers immediately if they are \"bogus\"."
  :group 'ess-help
  :type 'boolean)

(defvar ess-execute-screen-options-command nil
  "Dialect specific command run by `ess-execute-screen-options'.")

(defvar ess-help-web-search-command nil
  "Dialect specific command web help search.
Passed to `ess-execute-dialect-specific' which see.")
(make-variable-buffer-local 'ess-help-web-search-command)

(defvar ess-manual-lookup-command nil
  "Dialect specific command manual lookup.
Passed to `ess-execute-dialect-specific' which see.")
(make-variable-buffer-local 'ess-manual-lookup-command)

(defvar ess-reference-lookup-command nil
  "Dialect specific command for reference lookup.
Passed to `ess-execute-dialect-specific' which see.")
(make-variable-buffer-local 'ess-reference-lookup-command)

(defvar ess-funargs-command  nil
  "Dialect specific command to return a list of function arguments.
See `ess-function-arguments' and .ess_funargs command in R and
S+ for details of the format that should be returned.")
(make-variable-buffer-local 'ess-funargs-command)

(defvar ess-eldoc-function nil
  "Holds a dialect specific eldoc function.
See `ess-r-eldoc-function' and `ess-julia-eldoc-function' for examples.")

 ; System variables
;;;=====================================================
;;; Users note: You will rarely have to change these
;;; variables.

;;*;; Variables relating to ess-help-mode

;;-- ess-help-S-.. and  ess-help-R-.. : in  ess-s-lang.el (are used in ess-inf).

(defvar ess-help-sec-keys-alist nil
  "Alist of (key . string) pairs for use in section searching.")

(defvar ess-help-sec-regex nil
  "Reg(ular) Ex(pression) of section headers in help file.")

(make-variable-buffer-local 'ess-help-sec-keys-alist)
(make-variable-buffer-local 'ess-help-sec-regex)


 ; julia-mode
(defcustom inferior-julia-program (or (executable-find "julia-basic")
                                      (executable-find "julia")
                                      "julia")
  "Executable for Julia.
Should be an absolute path to the julia executable."
  :group 'ess-Julia
  :type '(choice (string) (file)))
(define-obsolete-variable-alias 'inferior-julia-program-name
  'inferior-julia-program "2018-05-23")

(defvar julia-basic-offset 4
  "Offset for julia code editing.")


 ; ess-mode: editing S source

;;; This syntax table is required by ess-mode.el, ess-inf.el and
;;; ess-trns.el, so we provide it here.
(defvar ess-mode-syntax-table nil "Syntax table for `ess-mode'.")
(defvar ess-mode-completion-syntax-table nil "Completion and help syntax table for `ess-mode'.")
(make-variable-buffer-local 'ess-mode-syntax-table)
(make-variable-buffer-local 'ess-mode-completion-syntax-table)

(defvar inferior-ess-mode-syntax-table nil "Syntax table for `inferior-ess-mode'.")
(make-variable-buffer-local 'inferior-ess-mode-syntax-table)


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

(defcustom ess-write-to-dribble t
  "Non-nil means write to `ess-dribble-buffer'.
See also `ess-verbose'."
  :group 'ess-proc
  :type 'boolean)

(defcustom ess-verbose nil
  "Non-nil means write more information to `ess-dribble-buffer' than usual."
  :group 'ess-proc
  :type 'boolean)

(defvar ess-dribble-buffer "*ESS*"
  "Name of buffer for temporary use for setting default variable values.
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
  "Variable settings for `ess-mode'.")

(defvar ess-transcript-minor-mode nil
  "Non-nil if using `ess-transcript-mode' as a minor mode of some other mode.")

(make-variable-buffer-local 'ess-transcript-minor-mode)

(defvar ess-listing-minor-mode nil
  "Non-nil if using `ess-listing-minor-mode'.")

(make-variable-buffer-local 'ess-listing-minor-mode)

(defvar ess--enable-experimental-projects nil
  "Enable experimental project support in ESS.")

(provide 'ess-custom)

;;; ess-custom.el ends here
