;;; ess-mode.el --- Support for editing ESS source code

;; Copyright (C) 1989-1994 Doug Bates, Ed Kademan, Frank Ritter, David Smith.
;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2012 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Created: 7 Jan 1994
;; Maintainer: ESS-core <ESS-core@r-project.org>
;; Package-Requires: ((julia-mode "0.3"))

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; Code for editing ESS source code.

;;; Code:

(require 'cl)

(autoload 'ess-turn-on-eldoc            "ess-r-d" "" nil)
;; (autoload 'ess-ddeclient-p              "ess-inf" "(autoload)" nil)
(autoload 'ess-dump-object-ddeclient        "ess-dde" "(autoload)" nil)
(autoload 'SAS                              "ess-sas-d.el" "(autoload)" t)

(require 'ess-utils)

(defun ess-line-end-position (&optional N)
  "return the 'point' at the end of N lines. N defaults to 1, i.e., current line."
  (save-excursion
    (end-of-line N)
    (point)))



;;; ESS mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The major mode ess-mode
;;;; * Commands for ess-mode
;;;; * Code evaluation commands
;;;; * Indenting code and commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*;; Major mode definition


(defvar ess-mode-map
  (let ((map (make-sparse-keymap)))

    ;; By popular demand:
    (define-key map "\C-m"       'newline-and-indent); = [RETURN]
    (define-key map [remap yank] 'ess-yank)

    (define-key map "\C-c\C-r"   'ess-eval-region)
    (define-key map "\C-c\M-r"   'ess-eval-region-and-go)
    (define-key map "\C-c\C-b"   'ess-eval-buffer)
    (define-key map "\C-c\M-b"   'ess-eval-buffer-and-go)
    (define-key map (kbd "C-c C-<up>")   'ess-eval-buffer-from-beg-to-here)
    (define-key map (kbd "C-c C-<down>") 'ess-eval-buffer-from-here-to-end)
    (define-key map "\C-c\C-f"   'ess-eval-function)
    (define-key map "\C-c\M-f"   'ess-eval-function-and-go)
    (define-key map "\C-c\C-c"   'ess-eval-region-or-function-or-paragraph-and-step)
    (define-key map "\C-c\C-p"   'ess-eval-paragraph-and-step)
    (define-key map "\C-c\M-p"   'ess-eval-paragraph-and-go)
    (define-key map "\C-\M-x"    'ess-eval-region-or-function-or-paragraph)
    (define-key map "\C-c\C-n"   'ess-eval-line-and-step)
    (define-key map "\C-c\C-j"   'ess-eval-line)
    (define-key map [(control return)] 'ess-eval-region-or-line-and-step)
    (define-key map "\C-c\M-j"   'ess-eval-line-and-go)
    ;; the next three can only work in S/R - mode {FIXME}
    (define-key map "\C-\M-a"    'ess-goto-beginning-of-function-or-para)
    (define-key map "\C-\M-e"    'ess-goto-end-of-function-or-para)
    (define-key map "\C-xnd"     'ess-narrow-to-defun-or-para)
    (define-key map "\C-xnf"     'ess-narrow-to-defun-or-para)
    (define-key map "\C-c\C-y"   'ess-switch-to-ESS-deprecated)
    (define-key map "\C-c\C-z"   'ess-switch-to-inferior-or-script-buffer)
    (define-key map "\C-c\C-l"   'ess-load-file)
    (define-key map "\C-c\M-l"   'ess-load-file); alias, as in 'iESS' where C-c C-l is comint-list-*
    (define-key map "\C-c\C-v"   'ess-display-help-on-object)
    ;;(define-key map "\C-c5\C-d"'ess-dump-object-into-edit-buffer-other-frame)
    (define-key map "\C-c\C-s"   'ess-switch-process) ; use a
    ;; different process for the buffer.
    ;; (define-key map "\C-c\C-t"   'ess-execute-in-tb)
    (define-key map "\C-c\t"     'ess-complete-object-name-deprecated)
    ;;M  (define-key map "\C-c\t"        'comint-dynamic-complete-filename)
    (unless (and (featurep 'emacs) (>= emacs-major-version 24))
      (define-key map "\M-\t"    'comint-dynamic-complete))
    (define-key map "\M-?"       'ess-list-object-completions)
    ;; wrong here (define-key map "\C-c\C-k" 'ess-request-a-process)
    (define-key map "\C-c\C-k"   'ess-force-buffer-current)
    (define-key map "\C-c`"      'ess-show-traceback)
    (define-key map [(control ?c) ?~] 'ess-show-call-stack)
    (define-key map "\C-c."      (lambda () (interactive) (message "ess-set-style moved to C-c C-e C-s. Sorry for the inconvenience")))
    (define-key map "{"          'ess-electric-brace)
    (define-key map "}"          'ess-electric-brace)
    (define-key map "\C-\M-q"    'ess-indent-exp)
    (define-key map "\C-\M-h"    'ess-mark-function-or-para)
    (if (featurep 'xemacs) ;; work around Xemacs bug (\C-\M-h redefines M-BS):
        (define-key map [(meta backspace)] 'backward-kill-word))
    ;;(define-key map [delete]   'backward-delete-char-untabify)
    (define-key map "\t"         'ess-indent-or-complete)
    (define-key map "\C-c\C-q"   'ess-quit)
    (define-key map "\M-\r"      'ess-use-this-dir)

    ;; smart operators; most likely will go in the future into a separate local map
    (define-key map ","          'ess-smart-comma)

    (define-key map "\C-c\C-d"   'ess-doc-map)
    (define-key map "\C-c\C-e"   'ess-extra-map)
    (define-key map "\C-c\C-t"   'ess-dev-map)
    map)
  "Keymap for `ess-mode'.")


(defvar ess-eval-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "\C-r"    'ess-eval-region)
    ;; (define-key map "\M-r"    'ess-eval-region-and-go)
    ;; (define-key map "\C-b"    'ess-eval-buffer)
    ;; (define-key map "\M-b"    'ess-eval-buffer-and-go)
    ;; (define-key map "\C-f"    'ess-eval-function)
    ;; (define-key map "\M-f"    'ess-eval-function-and-go)
    ;; (define-key map "\C-x"    'ess-eval-function)
    ;; (define-key map "\C-n"    'ess-eval-line-and-step)
    ;; (define-key map "\C-j"    'ess-eval-line)
    ;; (define-key map "\M-j"    'ess-eval-line-and-go)
    map)
  "Keymap for ess-eval functions.")
(make-obsolete-variable 'ess-eval-map nil "ESS[12.09.1]")

(defvar ess-extra-map
  (let (ess-extra-map)
    (define-prefix-command 'ess-extra-map)
    (define-key ess-extra-map "\C-d" 'ess-dump-object-into-edit-buffer)
    (define-key ess-extra-map "d" 'ess-dump-object-into-edit-buffer)
    (define-key ess-extra-map "\C-e" 'ess-execute)
    (define-key ess-extra-map "e" 'ess-execute)
    (define-key ess-extra-map "\C-i" 'ess-install-library)
    (define-key ess-extra-map "i" 'ess-install-library)
    (define-key ess-extra-map "\C-l" 'ess-load-library)
    (define-key ess-extra-map "l" 'ess-load-library)
    (define-key ess-extra-map "\C-s" 'ess-set-style)
    (define-key ess-extra-map "s" 'ess-set-style)
    (define-key ess-extra-map "\C-t" 'ess-build-tags-for-directory)
    (define-key ess-extra-map "t" 'ess-build-tags-for-directory)
    (define-key ess-extra-map "\C-w" 'ess-execute-screen-options)
    (define-key ess-extra-map "w" 'ess-execute-screen-options)
    (define-key ess-extra-map "/" 'ess-set-working-directory)
    ess-extra-map)
  "ESS extra map")


(require 'ess-noweb-mode)

(easy-menu-define
  ess-mode-menu ess-mode-map
  "Menu for use in `ess-mode'."
  '("ESS" ; ESS-mode
    ["What is this? (beta)"    ess-mouse-me                     t]
    ["Load file"                ess-load-file t]
    ["Eval region | func | para" ess-eval-region-or-function-or-paragraph t]
    ["Eval region | func | para & step" ess-eval-region-or-function-or-paragraph-and-step t]
    ["Eval region | line" ess-eval-region-or-line-and-step t]
    ["Enter expression" ess-execute                 t]
    ;; sub menus
    "------"
    ("Process"
     ["Goto end of process buffer"  ess-switch-to-end-of-ESS        t]
     ["Switch to process buffer"    ess-switch-to-inferior-or-script-buffer t]
     ["Switch Process"   ess-switch-process              t]
     ["Recreate R and S versions known to ESS" (ess-r-s-versions-creation+menu) t]
     ("Start Process"
      ;; SJE - :help not yet recognised in XEmacs.
      ["R"     R   t] ;; :help "Start a new R process" :active t
      ["S"     S   t] ;; :help "Start a new S process" :active t
      ["Sqpe" Sqpe ess-microsoft-p] ;; :help "Start a new Sqpe process" :active t
      ["S+6-exisiting" S+6-existing ess-microsoft-p] ;; :help "Access an existing S process" :active t
      ["SAS"   SAS-menu t] ;;  :help "Start a new SAS process" :active t
      ;; The following menu item "Other" is a place-holder that will
      ;; be replaced with the other versions of R and Sqpe that can be run.
      ;; See `ess-r-versions-create' and ess-site.el
      ("Other"
       ["No other R or Sqpe versions" nil nil])
      ["About"
       (ess-goto-info "Starting up") t]
      ;; :help "Read about starting a new ESS process" :active t]
      )
     ("Eval visibly "
      :filter ess--generate-eval-visibly-submenu))
    "------"
    ("ESS Eval"
     ["Eval region | func | para" ess-eval-region-or-function-or-paragraph t]
     ["Eval region | func | para & step" ess-eval-region-or-function-or-paragraph-and-step t]
     ["Eval region | line" ess-eval-region-or-line-and-step t]
     "-----"
     ["Eval buffer"     ess-eval-buffer                   t]
     ["Eval buffer till here" ess-eval-buffer-from-beg-to-here t]
     ["Eval buffer from here" ess-eval-buffer-from-here-to-end t]
     ["Eval region"     ess-eval-region                   t]
     ["Eval function"   ess-eval-function                 t]
     ["Eval line"       ess-eval-line                     t]
     ["Eval line & step" ess-eval-line-and-step            t]
     ["Eval paragraph"   ess-eval-paragraph                t]
     ["Eval paragraph & step" ess-eval-paragraph-and-step      t]
     ["Eval chunk"      ess-eval-chunk           ess-noweb-mode]
     ["Eval chunk and step"      ess-eval-chunk-and-step  ess-noweb-mode]
     ["Eval thread"     ess-eval-thread          ess-noweb-mode]
     ["About"           (ess-goto-info "Evaluating code") t]
     )
    ("Eval and Go"
     ["Eval buffer"     ess-eval-buffer-and-go            t]
     ["Eval region"     ess-eval-region-and-go            t]
     ["Eval function"   ess-eval-function-and-go          t]
     ["Eval line"       ess-eval-line-and-go              t]
     ["Eval paragraph"   ess-eval-paragraph-and-go         t]
     ["Eval chunk"      ess-eval-chunk-and-go    ess-noweb-mode]
     ["Eval thread"     ess-eval-thread-and-go   ess-noweb-mode]
     ["About"           (ess-goto-info "Evaluating code") t]
     )
    ("Motion"
     ["Beginning of function or para"   ess-goto-beginning-of-function-or-para       t]
     ["End of function or para"         ess-goto-end-of-function-or-para             t]
     "-----"
     ["Backward list"           backward-list                   t]
     ["Forward list"            forward-list                    t]
     ["Next parenthesis"                down-list                       t]
     ["Enclosing parenthesis"   backward-up-list                t]
     ["Backward sexp"           backward-sexp                   t]
     ["Forward sexp"            forward-sexp                    t]
     ["About"                   (Info-goto-node "(Emacs)Lists") t]
     )
    ("ESS Edit"
     ["Edit new object"         ess-dump-object-into-edit-buffer t]
     ["Complete Filename" comint-replace-by-expanded-filename   t]
     ["Complete File or Object"   ess-indent-or-complete        t]
     ["Kill sexp"         kill-sexp                             t]
     ["Mark function"     ess-mark-function-or-para             t]
     ["Indent expression" ess-indent-exp                        t]
     ["Indent line"       ess-indent-command                    t]
     ["Toggle Auto-Fill Mode" auto-fill-mode                    t]
     ["Undo"              undo                                  t]
     ["About"             (ess-goto-info "Edit buffer")         t]
     )
    "------"
    ("start-dev" :visible nil)
    ("end-dev" :visible nil)
    "------"
    ("Font Lock"
     :active ess-font-lock-keywords
     :filter ess--generate-font-lock-submenu)
    "------"
    ["Describe"         describe-mode                   t]
    ["About editing" (ess-goto-info "Editing")  t]
    ["Read ESS info" (ess-goto-info "") t]
    ["Send bug report"  ess-submit-bug-report           t]
    ))



;; (defun test-gen-menu (men)
;;   '(
;;     ["About editing" (ess-goto-info "Editing")  t]
;;     ["Read ESS info" (ess-goto-info "") t]
;;     ["Send bug report"  ess-submit-bug-report           t]))

(defun SAS-menu ()
  "Start SAS from the menu."
  (interactive)
  (if ess-microsoft-p
      ;; replace with other choices for starting SAS under XEmacs?
      (error "SAS cannot be started this way in ESS on Windows.")
    (SAS)))

(defun ess-mode-xemacs-menu ()
  "Hook to install `ess-mode' menu for XEmacs (w/ easymenu)."
  (if 'ess-mode
      (easy-menu-add ess-mode-menu)
    (easy-menu-remove ess-mode-menu)))

(if (featurep 'xemacs)
    (add-hook 'ess-mode-hook 'ess-mode-xemacs-menu))

(defun ess-mode (&optional alist proc-name is-derived)
  "Major mode for editing ESS source.
Optional arg ALIST describes how to customize the editing mode.
Optional arg PROC-NAME is name of associated inferior process.

\\{ess-mode-map}

Extra binding to note:  'ESC C-\\' indent-region.

Entry to this mode runs the hooks in ess-mode-hook.

You can send text to the inferior ESS process from other buffers containing
ESS source.
    `ess-eval-region' sends the current region to the ESS process.
    `ess-eval-buffer' sends the current buffer to the ESS process.
    `ess-eval-function' sends the current function to the ESS process.
    `ess-eval-line' sends the current line to the ESS process.
    `ess-beginning-of-function' and `ess-end-of-function' move the point to
        the beginning and end of the current ESS function.
    `ess-switch-to-ESS' switches the current buffer to the ESS process buffer.
    `ess-switch-to-end-of-ESS' switches the current buffer to the ESS process
        buffer and puts point at the end of it.

    `ess-eval-region-and-go', `ess-eval-buffer-and-go',
        `ess-eval-function-and-go', and `ess-eval-line-and-go' switch to the S
        process buffer after sending their text.

    `ess-load-file' sources a file of commands to the ESS process.

\\[ess-indent-command] indents for ESS code.
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
Comments are indented in a similar way to Emacs-lisp mode:
       `###'     beginning of line
       `##'      the same level of indentation as the code
       `#'       the same column on the right, or to the right of such a
                 column if that is not possible.(default value 40).
                 \\[indent-for-comment] command automatically inserts such a
                 `#' in the right place, or aligns such a comment if it is
                 already inserted.
\\[ess-indent-exp] command indents each line of the ESS grouping following point.

Variables controlling indentation style:
 `ess-tab-always-indent'
    Non-nil means TAB in ESS mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `ess-auto-newline'
    Non-nil means automatically newline before and after braces inserted in S
    code.
 `ess-indent-offset'
    Indentation of ESS statements within surrounding block.
    The surrounding block's indentation is the indentation of the line on
    which the open-brace appears.
 `ess-offset-block'
    Indentation of blocks opened with curly braces or anonymous parentheses.
 `ess-offset-arguments'
    Indentation of function arguments or bracket indices.
 `ess-offset-arguments-newline'
    Indentation of function arguments or bracket indices when the opening
    delimiter is immediately followed by a newline.
 `ess-offset-continued'
    Indentation style for continued statements.
 `ess-align-nested-calls'
    Functions whose nested calls should be aligned.
 `ess-align-arguments-in-calls'
    Calls in which arguments should be aligned.
 `ess-align-continuations-in-calls'
    Whether ignore indentation after an operator in calls
 `ess-align-blocks'
    Blocks that should always be aligned vertically.
 `ess-indent-from-lhs'
    Whether function calls given as argument should be indented from the
    parameter name.
 `ess-indent-from-chain-start'
    Whether to indent arguments from the first of several consecutive calls.
 `ess-indent-with-fancy-comments'
    Non-nil means distinguish between #, ##, and ### for indentation.

Furthermore, \\[ess-set-style] command enables you to set up predefined ess-mode
indentation style. At present, predefined style are `BSD', `GNU', `K&R', `C++',
`CLB' (quoted from C language style)."
  (setq alist (or alist
		  (buffer-local-value 'ess-local-customize-alist (current-buffer))
		  (error "Customise alist is not specified, nor  ess-local-customize-alist is set.")))
  (unless is-derived
    (kill-all-local-variables)) ;; NOTICE THIS! *** NOTICE THIS! *** NOTICE THIS! ***
  (ess-setq-vars-local alist)
  ;; must happen here, since the mode map is set up too early:
  (if ess-r-args-electric-paren (define-key ess-mode-map "(" 'ess-r-args-auto-show))
  (ess-write-to-dribble-buffer
   (format "(ess-mode-1): ess-language=%s, ess-dialect=%s buf=%s \n"
           ess-language
           ess-dialect
           (current-buffer)))
  ;; (ess-write-to-dribble-buffer
  ;;  (format "(ess-mode-1.2): ess-process=%s \n"
  ;;   (ess-local-process-name ess-local-process-name "none")))
  (ess-write-to-dribble-buffer
   (format "(ess-mode-1.5): alist=%s \n" alist))
  (unless is-derived
    (setq major-mode 'ess-mode)
    (setq mode-name (concat "ESS[" ess-language "]"))) ; was ess-dialect
  ;; The following line does the next 20 or so :-).
  (ess-write-to-dribble-buffer
   (format "(ess-mode-1.6): editing-alist=%s \n"
           ess-mode-editing-alist))
  (ess-setq-vars-local ess-mode-editing-alist)

  (ess-set-style ess-style t)
  (use-local-map ess-mode-map)
  (when ess-mode-syntax-table
    (set-syntax-table ess-mode-syntax-table))

  ;; Keep <tabs> out of the code.
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  (put 'ess-local-process-name 'permanent-local t) ; protect from RCS
  (setq mode-line-process
        '(" ["
          (:eval (ess--get-mode-line-indicator))
          ess--local-mode-line-process-indicator
          "]"))
  ;; completion
  (if (and (featurep 'emacs)
           (>= emacs-major-version 24))
      (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
    (add-hook 'comint-dynamic-complete-functions 'ess-complete-filename nil 'local)
    (delq t comint-dynamic-complete-functions)
    )
  (set (make-local-variable 'comint-completion-addsuffix)
       (cons "/" ""))
  ;; timer
  (add-hook 'ess-idle-timer-functions 'ess-synchronize-dirs nil 'local)
;;; extras
  (ess-load-extras)
  ;; SJE Tue 28 Dec 2004: do not attempt to load object name db.
  ;; (ess-load-object-name-db-file)
  (if (> emacs-major-version 21)
      (run-mode-hooks 'ess-mode-hook)
    ;; old emacs 21.x
    (run-hooks 'ess-mode-hook))
  (ess-write-to-dribble-buffer "\nFinished setting up ESS-mode.\n"))


(defun ess--get-mode-line-indicator ()
  "Get `ess--mode-line-process-indicator' from process buffer.
Internal function to be used for dynamic mode-line dysplay in
ess-mode."
  (if ess-local-process-name
      (let* ((proc (get-process ess-local-process-name))
             (buff (when proc (process-buffer proc))))
        (if (and proc (buffer-live-p buff))
            (with-current-buffer buff (mapcar 'eval ess--mode-line-process-indicator))
          "none"))
    "none"))



;;*;; User commands in ess-mode

;;;*;;; Handy commands

(defun ess-execute-in-tb ()
  "Like `ess-execute', but always evaluates in temp buffer."
  (interactive)
  (let ((ess-execute-in-process-buffer nil))
    (call-interactively 'ess-execute)))

(defun ess-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun ess-containing-sexp-position ()
  (cadr (syntax-ppss)))

(defun ess-code-end-position ()
  "Like (line-end-position) but stops at comments"
  (save-excursion
    (or (and (re-search-forward "#" (line-end-position) t)
             (match-beginning 0))
        (line-end-position))))

;;;*;;; Buffer motion/manipulation commands

(defvar ess-set-function-start
  ;; setAs, setGeneric;  setMethod, setReplaceMethod, setGroupMethod
  "^set[MGAR][Ma-z]+\\s-?("
  )

;; common R and S
;; SJE: 2007-07-16 add to quieten byte-compiler.
(defvar ess-function-pattern nil
  "Regexp to match the beginning of a function in S buffers.")

(defvar ess-R-symbol-pattern
  "\\(\\sw\\|\\s_\\)"
  "The regular expression for matching an R symbol")

(defvar ess-R-name-pattern
  (concat "\\(" ess-R-symbol-pattern "+\\|\\(`\\).+`\\)")
  "The regular expression for matching a R name.")

(let*
    ((Q     "\\s\"")                    ; quote
     (repl "\\(<-\\)?")                 ; replacement (function)
     (Sym-0 "\\(\\sw\\|\\s_\\)")        ; symbol
     (Symb (concat Sym-0 "+"))
     (xSymb "[^ \t\n\"']+") ;; (concat "\\[?\\[?" Sym-0 "*")); symbol / [ / [[ / [symbol / [[symbol
     ;; FIXME: allow '%foo%' but only when quoted; don't allow [_0-9] at beg.
     (_or_  "\\)\\|\\(")                ; OR
     (space "\\(\\s-\\|\n\\)*")         ; white space

     (part-1 (concat
              "\\(" ;;--------outer Either-------
              "\\(\\("          ; EITHER
              Q xSymb Q         ; any function name between quotes
              _or_
              "\\(^\\|[ ]\\)" Symb ; (beginning of name) + ess-R-symbol-pattern
              "\\)\\)"))        ; END EITHER OR

     (set-S4-exp
      (concat
       "^set\\(As\\|Method\\|Generic\\|GroupMethod\\|ReplaceMethod\\)(" ; S4 ...
       Q xSymb Q "," space
       ;; and now often `` signature(......), : ''
       ".*" ;; <<< FIXME ???
       ))

     (part-2 (concat
              "\\|" ;;--------outer Or ---------
              set-S4-exp
              "\\)" ;;--------end outer Either/Or-------

              "\\(" space "\\s<.*\\s>\\)*"      ; whitespace, comment
              ;; FIXME: in principle we should skip 'definition *= *' here
              space "function\\s-*(" ; whitespace, function keyword, parenthesis
              ))
     )

  (defvar ess-R-function-pattern
    (concat part-1
            "\\s-*\\(<-\\|=\\)" ; whitespace, assign
            part-2)
    "The regular expression for matching the beginning of an R function.")

  (defvar ess-S-function-pattern
    (concat part-1
            "\\s-*\\(<-\\|_\\|=\\)" ; whitespace, assign (incl. "_")
            part-2)
    "The regular expression for matching the beginning of an S function.")

  ); {end let}

(defun ess-beginning-of-function (&optional no-error)
  "Leave (and return) the point at the beginning of the current ESS function.
If the optional argument NO-ERROR is non-nil, the function returns nil when
it cannot find a function beginning."
  ;; FIXME: should not throw error in accordance with beginning-of-defun and
  ;; beginning-of-defun-function specification

  (interactive)
  (let ((init-point (point))
        (in-set-S4 nil)
        beg end done)

    ;; Note that we must be sure that we are past the 'function (' text,
    ;; such that ess-function-pattern is found in BACKwards later.
    ;; In case we're sitting in a function or setMethod() header,
    ;; we need to move further.
    ;; But not too far! {wrongly getting into next function}
    (if (search-forward "("
                        (ess-line-end-position 2) t) ; at most end of next line
        (forward-char 1))
    ;; TODO: replace the above by hopefully more sucessful logic:
    ;; 1. If we have 'function *(' in the same line, move to end of that line
    ;; 2. if *not*, skip all comment lines (concat space comment-char .* "\n")
    ;;    and only* then do something like the
    ;;    (search-forward '(' .. (..line-end.. 2) )  above

    (setq end (point))             ; = init-point when nothing found

    (ess-write-to-dribble-buffer
     (format "ess-BEG-of-fun after 'search-FWD (': Ini-pt %d, (p)-Ini-pt = %d\n"
             init-point (- end init-point)))
    (if (and (> end 1)
             (re-search-backward ;; in case of setMethod() etc ..
              ess-set-function-start
              ;; at most 1 line earlier {2 is too much: finds previous sometimes}
              (+ 1 (ess-line-end-position -1)) t))

        (progn ;; yes we *have* an S4  setMethod(..)-like
          (setq in-set-S4 t
                beg (point))
          (ess-write-to-dribble-buffer
           (format " set*() function start at position %d" beg))
          ;; often need to move even further to have 'function(' to our left
          ;;        (if (search-forward "function" end t)
          ;;            (ess-write-to-dribble-buffer
          ;;             (format " -> 'function' already at pos %d\n" (point)))
          ;;          ;; else need to move further
          (goto-char end)
          ;; search 4 lines, we are pretty sure now:
          (search-forward
           "function" (ess-line-end-position 4) t)
          ;;        )
          (search-forward "(" (ess-line-end-position) t))
      ;; else: regular function; no set*Method(..)
      (ess-write-to-dribble-buffer "ELSE  not in setMethod() header ...\n"))

    (while (not done)
      ;; Need this while loop to skip over local function definitions

      ;; In the case of non-success, it is inefficiently
      ;; going back in the buffer through all function definitions...
      (unless
          (and (re-search-backward ess-function-pattern (point-min) t)
               (not (ess-inside-string-or-comment-p (point))))
        (goto-char init-point)
        (if no-error
            (setq  done t  beg nil)
          ;; else [default]:
          (error "Point is not in a function according to 'ess-function-pattern'.")))
      (unless done
        (setq beg (point))
        (ess-write-to-dribble-buffer
         (format "\tMatch,Pt:(%d,%d),%d\n"
                 (match-beginning 0) (match-end 0) beg))
        (setq in-set-S4 (looking-at ess-set-function-start))
        (forward-list 1)              ; get over arguments

        ;; The following used to bomb  "Unbalanced parentheses", n1, n2
        ;; when the above (search-forward "(" ..) wasn't delimited :
        (unless in-set-S4 (forward-sexp 1)) ; move over braces
        ;;DBG (ess-write-to-dribble-buffer "|")
        (setq end (point))
        (goto-char beg)
        ;; current function must begin and end around point
        (setq done (and (>= end init-point) (<= beg init-point)))))
    beg))

(defun ess-end-of-function (&optional beginning no-error)
  "Leave the point at the end of the current ESS function.
Optional argument for location of beginning.  Return '(beg end)."
  (interactive)
  (if beginning
      (goto-char beginning)
    (setq beginning (ess-beginning-of-function no-error)))
  (if beginning
      ;; *hack* only for S (R || S+): are we in setMethod(..) etc?
      (let ((in-set-S4 (looking-at ess-set-function-start))
            (end-pos) (npos))
        (ess-write-to-dribble-buffer
         (format "ess-END-of-fun: S4=%s, beginning = %d\n" in-set-S4 beginning))
        (forward-list 1)      ; get over arguments || whole set*(..)
        (unless in-set-S4 (forward-sexp 1)) ; move over braces
        (ess-write-to-dribble-buffer
         (format "ess-END-of-fun: found #1 : %d\n" (point)))

        ;; For one-line functions withOUT '{ .. }' body  -- added 2008-07-23 --
        ;; particularly helpful for C-c C-c (ess-eval-function-or-paragraph-and-step):
        (setq end-pos (ess-line-end-position))
        (while (< (point) end-pos) ; if not at end of line, move further forward
          (goto-char ;; careful not to move too far; e.g. *not* over empty lines:
           (min (save-excursion (forward-sexp 1) (point))
                (save-excursion (forward-paragraph 1) (point)))))
        (list beginning (point)))
    ;; else: 'no-error': we are not in a function
    nil))

(defun ess-goto-beginning-of-function-or-para ()
  "If inside a function go to the beginning of it, otherwise go to the beginning
  of paragraph."
  (interactive)
  (or (ess-beginning-of-function 'no-error)
      (backward-paragraph))
  (point))

(defun ess-goto-end-of-function-or-para ()
  "If inside a function go to end of it, otherwise go to the end
  of paragraph."
  (interactive)
  (or (ess-end-of-function nil 'no-error)
      (forward-paragraph))
  (point))

(defun ess-mark-function-or-para ()
  "Put mark at end of ESS function, point at beginning."
  (interactive)
  (ess-goto-beginning-of-function-or-para)
  (push-mark (point))
  (ess-goto-end-of-function-or-para)
  (exchange-point-and-mark))

(define-obsolete-function-alias 'ess-mark-function 'ess-mark-function-or-para "15.09")

(defun ess-narrow-to-defun-or-para ()
  "Make text outside current function invisible.
If text is already narrowed, this is removed before narrowing to the
current function."
  (interactive)
  ;; if point is not in a function, ess-end-of-function catches the error.
  (save-excursion
    (widen)
    (let* ((beg (ess-goto-beginning-of-function-or-para))
           (end (ess-goto-end-of-function-or-para)))
      (narrow-to-region beg end))))

(define-obsolete-function-alias 'ess-narrow-to-defun 'ess-narrow-to-defun-or-para "15.09")


;;*;; Loading files

(defun ess-check-modifications nil
  "Check whether loading this file would overwrite some ESS objects
which have been modified more recently than this file, and confirm
if this is the case."
  ;; FIXME: this should really cycle through all top-level assignments in
  ;; the buffer
  ;;VS[02-04-2012|ESS 12.03]: this is sooo ugly
  (when (> (length ess-change-sp-regexp) 0)
    (and (buffer-file-name) ess-filenames-map
         (let ((sourcemod (nth 5 (file-attributes (buffer-file-name))))
               (objname))
           (save-excursion
             (goto-char (point-min))
             ;; Get name of assigned object, if we can find it
             (setq objname
                   (and
                    (re-search-forward
                     "^\\s *\"?\\(\\(\\sw\\|\\s_\\)+\\)\"?\\s *[<_]"
                     nil
                     t)
                    (buffer-substring (match-beginning 1)
                                      (match-end 1)))))
           (and
            sourcemod			; the file may have been deleted
            objname			; may not have been able to
                                        ; find name
            (ess-modtime-gt (ess-object-modtime objname) sourcemod)
            (not (y-or-n-p

                  (format
                   "The ESS object %s is newer than this file. Continue?"
                   objname)))
            (error "Aborted"))))))

(defun ess-check-source (fname)
  "If file FNAME has an unsaved buffer, offer to save it.
Returns t if the buffer existed and was modified, but was not saved."
  (let ((buff (get-file-buffer fname)))
    ;; RMH: Corrections noted below are needed for C-c C-l to work
    ;; correctly when issued from *S* buffer.
    ;; The following barfs since
    ;; 1. `if' does not accept a buffer argument, `not' does.
    ;; 2. (buffer-file-name) is not necessarily defined for *S*
    ;;(if buff
    ;; (let ((deleted (not (file-exists-p (buffer-file-name)))))
    ;; Next 2 lines are RMH's solution:
    (if (not(not buff))
        (let ((deleted (not (file-exists-p fname))))
          (if (and deleted (not (buffer-modified-p buff)))
              ;; Buffer has been silently deleted, so silently save
              (with-current-buffer buff
                (set-buffer-modified-p t)
                (save-buffer))
            (if (and (buffer-modified-p buff)
                     (or ess-mode-silently-save
                         (y-or-n-p
                          (format "Save buffer %s first? "
                                  (buffer-name buff)))))
                (with-current-buffer buff
                  (save-buffer))))
          (buffer-modified-p buff)))))

(defvar ess-error-regexp   "^\\(Syntax error: .*\\) at line \\([0-9]*\\), file \\(.*\\)$"
  "Regexp to search for errors.")

(defun ess-parse-errors (&optional showerr reset)
  "Jump to error in last loaded ESS source file.
With prefix argument, only shows the errors ESS reported."
  ;; reset argument is for compatibility with emacs next-error (tracebug
  ;; rebinds ess-parse-errors to next-error), This silences the compiler.
  (interactive "P")
  (ess-make-buffer-current)
  (let ((errbuff (get-buffer ess-error-buffer-name)))
    (if (not errbuff)
        (error "You need to do a load first!")
      (set-buffer errbuff)
      (goto-char (point-max))
      (if
          (re-search-backward
           ;; FIXME: R does not give "useful" error messages -
           ;; -----  by default: We (ESS) could try to use a more useful one, via
           ;;   options(error=essErrorHandler)
           ess-error-regexp
           nil
           t)
          (let* ((filename (buffer-substring (match-beginning 3) (match-end 3)))
                 (fbuffer (get-file-buffer filename))
                 (linenum
                  (string-to-number
                   (buffer-substring (match-beginning 2) (match-end 2))))
                 (errmess (buffer-substring (match-beginning 1) (match-end 1))))
            (if showerr
                (ess-display-temp-buffer errbuff)
              (if fbuffer nil
                (setq fbuffer (find-file-noselect filename))
                (with-current-buffer fbuffer
                  (ess-mode)))
              (pop-to-buffer fbuffer)
              (ess-goto-line linenum))
            (princ errmess t))
        (message "Not a syntax error.")
        (ess-display-temp-buffer errbuff)))))



;;*;; ESS code formatting/indentation

;;;*;;; User commands

(defun ess-electric-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  ;; skeleton-pair takes precedence
  (if (and (boundp 'skeleton-pair) skeleton-pair (featurep 'skeleton))
      (skeleton-pair-insert-maybe "{")
    ;; else
    (let (insertpos)
      (if (and (not arg)
               (eolp)
               (or (save-excursion
                     (skip-chars-backward " \t")
                     (bolp))
                   (if ess-auto-newline (progn (ess-indent-line) (newline) t) nil)))
          (progn
            (insert (if (featurep 'xemacs) (event-to-character last-command-event) last-command-event))
            (ess-indent-line)
            (if ess-auto-newline
                (progn
                  (newline)
                  ;; (newline) may have done auto-fill
                  (setq insertpos (- (point) 2))
                  (ess-indent-line)))
            (save-excursion
              (if insertpos (goto-char (1+ insertpos)))
              (delete-char -1))))
      (if insertpos
          (save-excursion
            (goto-char insertpos)
            (self-insert-command (prefix-numeric-value arg)))
        (self-insert-command (prefix-numeric-value arg))))))

;; fixeme: move into ess-indent-or-complete, indentation functions are overly
;; scattered around
(defun ess-indent-command (&optional whole-exp)
  "Indent current line as ESS code, or in some cases insert a tab character.
If `ess-tab-always-indent' is non-nil (the default), always indent
current line.  Otherwise, indent the current line only if point is at
the left margin or in the line's indentation; otherwise insert a tab.
A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as S
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (ess-indent-line))
            beg end)
        (save-excursion
          (if ess-tab-always-indent
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
    (if (and (not ess-tab-always-indent)
             (save-excursion
               (skip-chars-backward " \t")
               (not (bolp))))
        (insert-tab)
      ;; call ess-indent-line
      (funcall indent-line-function))))

(defun ess-indent-or-complete ()
  "When region is selected indent the region, otherwise, if
`ess-tab-complete-in-script' is non-nil, try to indent, if code
is already indented, complete instead.

The default of `ess-tab-complete-in-script' is nil.  Also see
`ess-first-tab-never-complete'."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (let ((shift (ess-indent-command)))
      (when (and ess-tab-complete-in-script
                 (numberp shift) ;; can be nil if ess-tab-always-indent is nil
                 (equal shift 0)
                 (or (eq last-command 'ess-indent-or-complete)
                     (null ess-first-tab-never-complete)
                     (and (eq ess-first-tab-never-complete 'unless-eol)
                          (looking-at "\\s-*$"))
                     (and (eq ess-first-tab-never-complete 'symbol)
                          (not (looking-at "\\w\\|\\s_")))
                     (and (eq ess-first-tab-never-complete 'symbol-or-paren)
                          (not (looking-at "\\w\\|\\s_\\|\\s)")))
                     (and (eq ess-first-tab-never-complete 'symbol-or-paren-or-punct)
                          (not (looking-at "\\w\\|\\s_\\|\\s)\\|\\s.")))
                     ))
        (if (and (featurep 'emacs) (>= emacs-major-version 24))
            (completion-at-point)
          (comint-dynamic-complete)
          )))))

(defun ess-indent-exp ()
  "Indent each line of the ESS grouping following point."
  (interactive)
  (save-excursion
    (let ((start (point))
          (end (ignore-errors (forward-sexp 1) (point))))
      (when end
        (indent-region start end)))))



;;;*;;; Indentation Engine
;; Written by Lionel Henry in mid 2015

(defun ess-comment-indent ()
  (if (or (looking-at "###")
          (and (looking-at "#!") (= 1 (line-number-at-pos))))
      (current-column)
    (if (looking-at "##")
        (let ((tem (ess-calculate-indent)))
          (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun ess-indent-line ()
  "Indent current line as ESS code.
Return the amount the indentation changed by."
  ;; fixme: make this work with standard indent-line-function
  (if (fboundp ess-indent-line-function)
      (funcall ess-indent-line-function)
    ;; else S and R default behavior
    (let ((indent (ess-calculate-indent nil))
          beg shift-amt
          (case-fold-search nil)
          (pos (- (point-max) (point))))
      (beginning-of-line)
      (setq beg (point))
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
        (when (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))
      shift-amt)))

(defun ess-offset (offset)
  (setq offset (eval (intern (concat "ess-offset-" (symbol-name offset)))))
  (when (and (not (eq offset nil))
             (listp offset)
             (or (numberp (cadr offset))
                 (eq (cadr offset) t)
                 (error "Malformed offset")))
    (setq offset (cadr offset)))
  (cond ((numberp offset)
         offset)
        ((null offset)
         0)
        (t
         ess-indent-level)))

(defun ess-offset-type (offset)
  (setq offset (eval (intern (concat "ess-offset-" (symbol-name offset)))))
  (if (listp offset)
      (car offset)
    offset))

(defun ess-calculate-indent--nested-calls ()
  (when ess-align-nested-calls
    (let ((calls (mapconcat 'identity ess-align-nested-calls "\\|"))
          match)
      (save-excursion
        (and containing-sexp
             (looking-at (concat "\\(" calls "\\)("))
             (setq match (match-string 1))
             (goto-char containing-sexp)
             (looking-at "(")
             (ess-backward-sexp)
             (looking-at (concat match "("))
             (current-column))))))

(defun ess-block-opening-p ()
  (save-excursion
    (cond
     ((looking-at "{"))
     ;; Opening parenthesis not attached to a function opens up a
     ;; block too. Only pick up those that are last on their line
     ((ess-looking-at-block-paren-p)))))

(defun ess-block-closing-p ()
  (save-excursion
    (cond
     ((looking-at "}"))
     ((looking-at ")")
      (forward-char)
      (backward-sexp)
      (not (looking-back
            (concat ess-R-name-pattern "[[:blank:]]*")
            (line-beginning-position)))))))

(defun ess-call-closing-p ()
  (save-excursion
    (when (cond ((looking-at ")")
                 (ess-up-list -1))
                ((looking-at "]")
                 (when (ess-up-list -1)
                   (prog1 t (ess-climb-chained-brackets)))))
      (ess-looking-back-attached-name-p))))

(defvar ess-block-funs-patterns
  (mapcar (lambda (fun) (concat fun "\\b"))
          '("function" "if" "for" "while")))

(defun ess-overridden-blocks ()
  (append (when (memq 'fun-decl ess-align-blocks)
            (list (car ess-block-funs-patterns)))
          (when (memq 'control-flow ess-align-blocks)
            (append (cdr ess-block-funs-patterns)
                    '("}?[ \t]*else")))))

(defun ess-unbraced-block-p (&optional ignore-ifelse)
  "This indicates whether point is in front of an unbraced block
following a control flow statement. Returns position of the
control flow function (if, for, while, etc)."
  (save-excursion
    (and (ess-backward-sexp)
         (or (and (looking-at "else\\b")
                  (not ignore-ifelse))
             (and (looking-at "(")
                  (ess-backward-sexp)
                  (some 'looking-at ess-block-funs-patterns)
                  (if ignore-ifelse
                      (not (looking-at "if\\b"))
                    t)))
         (point))))

(defun ess-block-p ()
  (or (save-excursion
        (when containing-sexp
          (goto-char containing-sexp)
          (ess-block-opening-p)))
      (ess-unbraced-block-p)))

(defun ess-looking-at-block-paren-p ()
  (and (looking-at "(")
       (not (ess-looking-back-attached-name-p))))

(defun ess-looking-at-call-opening (pattern)
  (and (looking-at pattern)
       (ess-looking-back-attached-name-p)))

;; Should be called just before the opening brace
(defun ess-looking-back-attached-name-p ()
  (save-excursion
    (ess-climb-object)))

(defun ess-looking-back-operator-p (&optional fun-arg)
  (save-excursion
    (and (ess-climb-operator)
         (if (not fun-arg)
             (not (ess-looking-at-parameter-op-p))
           t))))

(defun ess-looking-at-parameter-op-p ()
  "Are we looking at a function argument? To be called just
before the `=' sign."
  (save-excursion
    (and (looking-at "[ \t]*=[^=]")
         (ess-climb-object)
         (looking-back "[(,][ \t\n]*" (line-beginning-position 0)))))

(defun ess-looking-at-parameter-p ()
  (save-excursion
    (ess-jump-parameter)))

(defun ess-looking-at-parameter-name-p ()
  (save-excursion
    (ess-jump-parameter-name)))

(defun ess-point-in-call-p ()
  "Is point in a function or indexing call?"
  (let ((containing-sexp (if (boundp 'containing-sexp)
                             containing-sexp
                           (ess-containing-sexp-position))))
    (save-excursion
      (and containing-sexp
           (goto-char containing-sexp)
           (or (ess-looking-at-call-opening "(")
               (looking-at "\\["))))))

(defun ess-point-in-continuation-p ()
  (unless (or (looking-at ",")
              (ess-looking-at-call-opening "[[(]"))
    (save-excursion
      (ess-jump-object)
      (and (not (ess-looking-at-parameter-op-p))
           (ess-looking-at-operator-p)))))

(defun ess-point-on-call-name-p ()
  (save-excursion
    (ess-jump-name)
    (ess-looking-at-call-opening "[[(]")))

(defun ess-point-in-comment-p (&optional state)
  (let ((state (or state (syntax-ppss))))
        (eq (syntax-ppss-context state) 'comment)))

(defun ess-point-in-string-p (&optional state)
  (let ((state (or state (syntax-ppss))))
        (eq (syntax-ppss-context state) 'string)))

;; The three following wrappers return t if successful, nil on error
(defun ess-backward-sexp (&optional N)
  (ess-forward-sexp (- (or N 1))))

(defun ess-forward-sexp (&optional N)
  (or N (setq N 1))
  (condition-case nil
      (prog1 t
        (goto-char (or (scan-sexps (point) N)
                       (buffer-end N))))
    (error nil)))

(defun ess-up-list (&optional N)
  (condition-case nil
      (progn (up-list N) t)
    (error nil)))

;; Going forth and back is a fast and reliable way of skipping in
;; front of the next sexp despite blanks, newlines and comments that
;; may be in the way.
(defun ess-forth-and-back-sexp ()
  (ess-save-excursion-when-nil
    (and (ess-forward-sexp)
         (ess-backward-sexp))))

(defun ess-back-and-forth-sexp ()
  (ess-save-excursion-when-nil
    (and (ess-backward-sexp)
         (ess-forward-sexp))))

;; Avoids let-binding a variable just to check a returned position is
;; not nil
(defun ess-goto-char (pos)
  (when pos
    (goto-char pos)))

(defun ess-looking-at (regex)
  "Compared to a simple `(looking-at)', this uses sexp motions to
skip any blanks, newlines and comments. Should be more reliable
and possibly faster than using complicated regexes."
  (save-excursion
    (and (ess-forth-and-back-sexp)
         (looking-at regex)
         (point))))

(defun ess-back-to-indentation ()
  "Move point to the first non-whitespace character on this line.
This non-interactive version of (back-to-indentation) should not
be advised"
  (beginning-of-line 1)
  (skip-syntax-forward " " (line-end-position))
  ;; Move back over chars that have whitespace syntax but have the p flag.
  (backward-prefix-chars))

(defmacro ess-save-excursion-when-nil (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  `(let ((orig-point (point)))
     (cond ((progn ,@body))
           (t (prog1 nil
                (goto-char orig-point))))))

(defmacro ess-while (test &rest body)
  "Like (while) but return `t' when body gets executed once."
  (declare (indent 1)
           (debug (&rest form)))
  `(let (executed)
     (while ,test
       (setq executed t)
       ,@body)
     executed))

(defmacro ess-at-indent-point (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  `(save-excursion
     (goto-char indent-point)
     (ess-back-to-indentation)
     (progn ,@body)))

(defmacro ess-at-containing-sexp (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  '(when (null containing-sexp)
     (error "Internal error: containing-sexp is nil"))
  `(save-excursion
     (goto-char containing-sexp)
     (progn ,@body)))

(defmacro ess-any (&rest forms)
  "Evaluates all arguments and return non-nil if one of the
arguments is non-nil. This is useful to trigger
side-effects. FORMS follows the same syntax as arguments to
`(cond)'."
  (declare (indent 0) (debug nil))
  `(let ((forms (list ,@(mapcar (lambda (form) `(progn ,@form)) forms))))
     (some 'identity (mapcar 'eval forms))))

(defun ess-climb-block (&optional ignore-ifelse)
  (ess-save-excursion-when-nil
    (cond
     ((and (not ignore-ifelse)
           (ess-climb-if-else 'to-start)))
     ((and (eq (char-before) ?\})
           (prog2
               (forward-char -1)
               (ess-up-list -1)
             (ess-climb-block-opening)))))))

(defun ess-climb-block-opening (&optional ignore-ifelse)
  "Should be called either in front of a naked block or in front
of the curly brackets of a braced block."
  (or (and (not ignore-ifelse)
           (prog1 (ess-climb-if-else-call)
             (when (looking-at "else\\b")
               (ess-skip-curly-backward))))
      (let ((pos (ess-unbraced-block-p ignore-ifelse)))
        (when pos
          (goto-char pos)))))

(defun ess-jump-block ()
  (ess-save-excursion-when-nil
    (or
     ;; if-else blocks
     (ess-jump-if-else)
     ;; Block calls such as `function() {}'
     (and (some 'looking-at ess-block-funs-patterns)
          (ess-forward-sexp 2)
          (prog1 t
            (when (looking-at "[ \t]{")
              (ess-forward-sexp))))
     ;; Naked blocks
     (and (or (looking-at "{")
              (ess-looking-at-block-paren-p))
          (ess-forward-sexp)))))

(defun ess-climb-lhs (&optional no-fun-arg climb-line)
  (ess-save-excursion-when-nil
    (let ((start-line (line-number-at-pos)))
      (ess-climb-operator)
      (when (and (or climb-line (equal (line-number-at-pos) start-line))
                 (ess-looking-at-definition-op-p no-fun-arg))
        (prog1 t
          (ess-climb-expression))))))

(defun ess-jump-lhs ()
  (ess-save-excursion-when-nil
    (and (ess-jump-name)
         (ess-looking-at-definition-op-p)
         (ess-jump-operator))))

(defun ess-looking-at-function-p ()
  (looking-at "function[ \t]*("))

(defun ess-climb-function-decl (&optional from-block)
  (let ((times (if from-block 2 1)))
    (ess-save-excursion-when-nil
      (and (ess-backward-sexp times)
           (ess-looking-at-function-p)
           (point)))))

;; Useful to check presence of operators. Need to check for
;; (point-min) because that won't work if there is no previous sexp
;; Should be called right at the beginning of current sexp.
(defun ess-climb-operator ()
  (ess-save-excursion-when-nil
    (let ((orig-pos (point)))
      (when (ess-backward-sexp)
        ;; When there is only empty space or commented code left to
        ;; climb (e.g. roxygen documentation), there is no previous
        ;; SEXP, but (ess-backward-sexp) will nevertheless climb the
        ;; empty space without failing. So we need to skip it.
        (while (and (looking-at "[[:space:]]*\\(#\\|$\\)")
                    (/= (point) (point-max)))
          (forward-line)
          (ess-back-to-indentation))
        ;; Handle %op% operators
        (when (and (eq (char-before) ?%)
                   (looking-at (concat ess-R-symbol-pattern "+%")))
          (ess-backward-sexp))
        (when (and (< (point) orig-pos)
                   (ess-forward-sexp)
                   (ess-looking-at-operator-p))
          (prog1 t
            (when (and (equal (char-after) ?=)
                       (equal (char-before) ?:))
              (forward-char -1))))))))

(defun ess-jump-operator ()
  (when (ess-looking-at-operator-p)
    (ess-forward-sexp)
    ;; Handle `:=' operator
    (when (and (equal (char-after) ?=)
               (equal (char-before) ?:))
      (ess-forward-sexp))
    (ess-backward-sexp)))

(defun ess-jump-continuation ()
  (and (ess-jump-operator)
       (ess-jump-expression)))

(defun ess-jump-continuations ()
  (let (last-pos)
    (when (ess-while (and (or (null last-pos)
                              (/= (point) last-pos))
                          (setq last-pos (point))
                          (ess-jump-continuation)))
      ;; In calls, operators can start on newlines
      (let ((start-line (line-number-at-pos)))
        (when (ess-save-excursion-when-nil
                (and (ess-point-in-call-p)
                     (ess-skip-blanks-forward t)
                     (/= (line-number-at-pos) start-line)
                     (ess-looking-at-operator-p)))
          (ess-jump-continuations)))
      t)))

(defun ess-looking-at-continuation-p (&optional or-parameter)
  (or (save-excursion
        (ess-skip-blanks-backward t)
        (ess-back-and-forth-sexp)
        (when (ess-looking-at-operator-p)
          (if or-parameter t
            (not (ess-looking-at-parameter-op-p)))))
      (save-excursion
        (ess-climb-block-opening))
      (save-excursion
        (ess-climb-if-else))))

(defun ess-skip-blanks-backward (&optional newlines)
  "Skip blanks and newlines backward, taking end-of-line comments
into account."
  (when (and (/= (point) (point-min))
             (ess-any
               ((/= 0 (skip-chars-backward " \t")))
               ((when (and newlines
                           (= (point) (line-beginning-position)))
                  (forward-line -1)
                  (goto-char (line-end-position))
                  (ess-climb-comment)
                  (ess-skip-blanks-backward newlines)))))
    t))

(defun ess-skip-blanks-forward (&optional newlines)
  "Skip blanks and newlines forward, taking end-of-line comments
into account."
  (skip-chars-forward " \t")
  (when (and newlines
             (= (point) (ess-code-end-position)))
    (forward-line)
    (ess-back-to-indentation)
    (ess-skip-blanks-forward newlines)))

(defun ess-climb-comment ()
  (when (ess-point-in-comment-p)
    (prog1 (comment-beginning)
     (skip-chars-backward "#+[ \t]*"))))

;; Should  climb any names, including backquoted ones or those
;; containing `@' or `$'. Difficult to achieve with regexps, but
;; skipping chars is faster anyway.
(defun ess-climb-object ()
  (ess-save-excursion-when-nil
    (let (climbed)
      (ess-skip-blanks-backward)
      ;; Backquoted names can contain any character
      (if (eq (char-before) ?`)
          (progn
            (forward-char -1)
            (while (not (memq (char-before) '(?` ?\C-J)))
              (forward-char -1)
              (setq climbed t))
            (when climbed
              (forward-char -1)))
        (while (some (apply-partially '/= 0)
                     `(,(skip-syntax-backward "w_")
                       ,(skip-chars-backward "\"'")))
          (setq climbed t)))
      ;; Recurse if we find an indexing char
      (when (memq (char-before) '(?$ ?@))
        (forward-char -1)
        (ess-climb-object))
      climbed)))

;; Todo: split name and object climbing
(defun ess-climb-name ()
  (ess-climb-object))

;; This jumps both object names and atomic objects like strings or
;; numbers.
(defun ess-jump-object ()
  (let (climbed quote-char)
    (cond
     ;; Jump over object names
     ((ess-jump-name))
     ;; Jump over strings))
     ((ess-save-excursion-when-nil
        (skip-chars-forward " \t")
        (or (and (eq (char-after) ?')
                 (setq quote-char "'"))
            (and (eq (char-after) ?\")
                 (setq quote-char "\""))))
      (forward-char)
      (while (and (skip-chars-forward (concat "^" quote-char))
                  (setq climbed t)
                  (looking-back quote-char (1- (point)))))
      (when (eq (char-after) ?\")
        (forward-char))
      climbed))))

(defun ess-jump-name ()
  (ess-save-excursion-when-nil
    (let (climbed quote-char)
      (skip-chars-forward " \t")
      ;; Jump over backquoted names
      (cond ((and (eq (char-after) ?`)
                  (looking-back ess-R-symbol-pattern
                                (1- (point))))
             (forward-char)
             (setq climbed t))
            ((eq (char-after) ?`)
             (forward-char)
             (when (ess-while (not (memq (char-after) '(?` ?\C-J)))
                     (forward-char))
               (setq climbed t)
               (forward-char)))
            ;; Jump over regular names
            ((when (/= 0 (skip-syntax-forward "w_"))
               ;; Maybe point was inside backticks
               (when (eq (char-after) ?`)
                 (forward-char))
               (setq climbed t))))
      climbed)))

(defun ess-jump-parameter-name ()
  (ess-save-excursion-when-nil
    (and (ess-jump-name)
         (when (looking-at "[ \t]*=\\([^=]\\)")
           (goto-char (match-beginning 1))
           (ess-skip-blanks-forward)
           t))))

(defun ess-jump-parameter ()
  (ess-any ((ess-jump-parameter-name))
           ((ess-jump-expression))
           ((ess-jump-continuations))))

(defun ess-climb-call ()
  "Climb functions (e.g. ggplot) and parenthesised expressions."
  (ess-climb-chained-brackets)
  (ess-save-excursion-when-nil
    (ess-backward-sexp)
    (when (looking-at "[[({]")
      (prog1 t
        (when (ess-looking-back-attached-name-p)
          (ess-backward-sexp))))))

(defun ess-jump-call ()
  (ess-save-excursion-when-nil
    (or (and (ess-jump-object)
             (cond ((eq (char-before) ?\)))
                   ((looking-at "\\[")
                    (ess-jump-chained-brackets))
                   ((looking-at "(")
                    (ess-forward-sexp))))
        (and (looking-at "[ \t]*(")
             (ess-forward-sexp)))))

(defun ess-looking-at-call-p ()
  (save-excursion
    (ess-jump-object)
    (ess-skip-blanks-forward)
    (looking-at "[[(]")))

(defun ess-climb-expression (&optional ignore-ifelse)
  (ess-save-excursion-when-nil
    (let ((climbed
           (or (ess-climb-block ignore-ifelse)
               (ess-climb-call)
               (ess-climb-object))))
      (if (and climbed ignore-ifelse)
          (not (ess-block-opening-p))
        climbed))))

(defun ess-jump-expression ()
  (or (ess-jump-block)
      (ess-jump-call)
      (ess-jump-object)))

(defun ess-climb-if-else-call (&optional multi-line)
  "Climb if, else, and if else calls."
  (ess-save-excursion-when-nil
    (ess-backward-sexp)
    (cond ((looking-at "(")
           (when (and (ess-backward-sexp)
                      (looking-at "if\\b"))
             ;; Check for `else if'
             (prog1 t
               (ess-save-excursion-when-nil
                 (let ((orig-line (line-number-at-pos)))
                   (and (ess-backward-sexp)
                        (or multi-line
                            (eq orig-line (line-number-at-pos)))
                        (looking-at "else\\b")))))))
          ((looking-at "else\\b")))))

(defun ess-climb-if-else-body ()
  (cond
   ;; Climb braced body
   ((ess-save-excursion-when-nil
      (and (when (progn (ess-skip-blanks-backward)
                        (eq (char-before) ?\}))
             (prog1 t (forward-char -1)))
           (ess-up-list -1))))
   ;; Climb unbraced body
   ((ess-save-excursion-when-nil
      (ess-skip-blanks-backward t)
      (prog1 (ess-climb-expression 'ignore-ifelse)
        (or (ess-climb-continuations nil 'ignore-ifelse)
            (ess-climb-block-opening 'ignore-ifelse)))))))

(defun ess-climb-if-else (&optional to-start)
  "Climb horizontal as well as vertical if-else chains, with or
without curly braces."
  ;; Don't climb if we're atop the current chain of if-else
  (unless (looking-at "if\\b")
    (ess-save-excursion-when-nil
      (let ((from-else (looking-at "else\\b")))
        (when (and (ess-climb-if-else-body)
                   (ess-climb-if-else-call to-start))
          ;; If we start from a final else and climb to another else, we
          ;; are in the wrong chain of if-else. In that case,
          ;; climb-recurse to the top of the current chain and climb
          ;; again to step in the outer chain.
          (when (and from-else (ess-looking-at-final-else))
            (ess-climb-if-else 'to-start)
            (ess-climb-block-opening 'ignore-ifelse)
            (ess-climb-if-else-call nil))
          (ess-maybe-climb-broken-else)
          (when to-start
            (ess-climb-if-else to-start))
          t)))))

;; Handles multi-line such as if \n else, with comments in the way etc
(defun ess-looking-at-final-else ()
  (or (save-excursion
        (and (looking-at "else\\b")
             (ess-forward-sexp)
             (ess-forth-and-back-sexp)
             (not (looking-at "if\\b"))))))

;; Broken else: if \n else
(defun ess-maybe-climb-broken-else (&optional same-line)
  (ess-save-excursion-when-nil
    ;; Don't record current line if not needed (expensive operation)
    (let ((cur-line (when same-line (line-number-at-pos))))
      (and (ess-backward-sexp)
           (looking-at "else\\b")
           (if same-line
               (= cur-line (line-number-at-pos))
             t)))))

(defun ess-skip-curly-backward ()
  (re-search-backward "}[ \t]*" (line-beginning-position) t))

(defun ess-jump-if-else ()
  (ess-while (ess-save-excursion-when-nil
               (ess-skip-blanks-forward t)
               (cond
                ((ess-jump-if))
                ((looking-at "else")
                 (ess-forward-sexp)
                 (or (ess-jump-if)
                     (progn
                       (ess-skip-blanks-forward t)
                       (ess-jump-expression))))
                (t
                 nil)))))

(defun ess-jump-if ()
  (ess-save-excursion-when-nil
    (ess-skip-blanks-forward t)
    (and (looking-at "if[ \t\n]*(")
         (ess-forward-sexp 2)
         (progn
           (ess-skip-blanks-forward t)
           (ess-jump-expression)))))

(defun ess-climb-chained-brackets ()
  "Should be called with point between `]['."
  (ess-while (ess-save-excursion-when-nil
               (when (eq (char-before) ?\])
                 (ess-backward-sexp)))))

(defun ess-jump-chained-brackets ()
  (ess-while (ess-save-excursion-when-nil
               (when (eq (char-after) ?\[)
                 (ess-forward-sexp)))))

(defun ess-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as ESS code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let* ((indent-point (point))
           (state (syntax-ppss))
           (containing-sexp (cadr state))
           (prev-containing-sexp (car (last (butlast (nth 9 state))))))
      (ess-back-to-indentation)
      (cond
       ;; Strings
       ((ess-point-in-string-p state)
        (current-indentation))
       ;; Comments
       ((ess-calculate-indent--comments))
       ;; Indentation of commas
       ((looking-at ",")
        (ess-calculate-indent--comma))
       ;; Arguments: Closing
       ((ess-call-closing-p)
        (ess-calculate-indent--args 0))
       ;; Block: Contents (easy cases)
       ((ess-calculate-indent--block-relatively))
       ;; Continuations
       ((ess-calculate-indent--continued))
       ;; Block: Overridden contents
       ((ess-calculate-indent--aligned-block))
       ;; Block: Opening
       ((ess-block-opening-p)
        (ess-calculate-indent--block-opening))
       ;; Bare line
       ((and (null containing-sexp)
             (not (ess-unbraced-block-p)))
        0)
       ;; Block: Closing
       ((ess-block-closing-p)
        (ess-calculate-indent--block 0))
       ;; Block: Contents
       ((ess-block-p)
        (ess-calculate-indent--block))
       ;; Arguments: Nested calls override
       ((ess-calculate-indent--nested-calls))
       ;; Arguments: Contents
       (t
        (ess-calculate-indent--args))))))

(defun ess-calculate-indent--comments ()
  (when ess-indent-with-fancy-comments
    (cond
     ;; ### or #!
     ((or (looking-at "###")
          (and (looking-at "#!")
               (= 1 (line-number-at-pos))))
      0)
     ;; Single # comment
     ((looking-at "#[^#']")
      comment-column))))

(defun ess-calculate-indent--comma ()
  (when (ess-point-in-call-p)
    (let ((indent (save-excursion
                    (ess-calculate-indent--args)))
          (unindent (progn (skip-chars-forward " \t")
                           ;; return number of skiped chars
                           (skip-chars-forward ", \t"))))
      (- indent unindent))))

(defun ess-calculate-indent--block-opening ()
  (cond
   ;; Block is an argument in a function call
   ((when containing-sexp
      (ess-at-containing-sexp
        (ess-looking-at-call-opening "[[(]")))
    (ess-calculate-indent--block 0))
   ;; Top-level block
   ((null containing-sexp) 0)
   ;; Block is embedded in another block
   ((ess-at-containing-sexp
      (equal (char-after) ?\{)
      (+ (current-indentation)
         (ess-offset 'block))))))

(defun ess-calculate-indent--aligned-block ()
  ;; Check for `else' opening
  (if (and (memq 'control-flow ess-align-blocks)
           (looking-at "else\\b"))
      (progn
        (ess-climb-if-else)
        (when (looking-at "else\\b")
          (ess-skip-curly-backward))
        (current-column))
    ;; Check for braced and unbraced blocks
    (ess-save-excursion-when-nil
      (let ((offset (if (looking-at "[{}()]")
                        0 (ess-offset 'block))))
        (when (and (cond
                    ;; Unbraced blocks
                    ((ess-climb-block-opening))
                    ;; Braced blocks
                    (containing-sexp
                     (goto-char containing-sexp)
                     (and (looking-at "{")
                          (ess-climb-block-opening))))
                   (some 'looking-at (ess-overridden-blocks))
                   ;; This ensures that we indent call-prefixed blocks
                   ;; from their lhs if they have one, even when
                   ;; `ess-align-blocks' says to align
                   (not (save-excursion
                          (and (some 'looking-at ess-block-funs-patterns)
                               (ess-looking-back-definition-op-p t)))))
          (+ (current-column) offset))))))

(defun ess-calculate-indent--block-relatively ()
  (ess-save-excursion-when-nil
    (let ((offset (if (looking-at "[})]") 0 (ess-offset 'block)))
          (start-line (line-number-at-pos)))
      (cond
       ;; By now comments can be indented relatively in all cases
       ((looking-at "#")
        (when (ess-save-excursion-when-nil
                (forward-line -1)
                (ess-back-to-indentation)
                (looking-at "#"))
          (current-column)))
       ;; Braceless block continuations: only when not in a call
       ((ess-save-excursion-when-nil
          (and (not (looking-at "{"))
               (ess-goto-char (ess-unbraced-block-p))
               (not (looking-at "function\\b"))
               (or (null containing-sexp)
                   (ess-at-containing-sexp
                     (not (looking-at "("))))))
        (ess-maybe-climb-broken-else 'same-line)
        (ess-skip-curly-backward)
        (+ (current-column)
           (ess-offset 'block)))
       ;; Don't indent relatively other continuations
       ((ess-looking-at-continuation-p)
        nil)
       ;; If a block already contains an indented line, we can indent
       ;; relatively from that first line
       ((ess-save-excursion-when-nil
          (and (not (looking-at "}"))
               containing-sexp
               (goto-char containing-sexp)
               (looking-at "{")
               (progn
                 (forward-line)
                 (ess-back-to-indentation)
                 (/= (line-number-at-pos) start-line))
               (not (looking-at "[ \t]*\\(#\\|$\\)"))
               (save-excursion
                 (or (ess-jump-expression)
                     (ess-jump-continuations))
                 (< (line-number-at-pos) start-line))))
        (current-column))
       ;; If a block is not part of a call, we can indent relatively
       ;; from the opening {. First check that enclosing { is first
       ;; thing on line
       ((and containing-sexp
             (not (ess-unbraced-block-p))
             (goto-char containing-sexp)
             (ess-block-opening-p)
             (equal (point) (save-excursion
                              (ess-back-to-indentation)
                              (point))))
        (+ (current-column) offset))))))

(defun ess-arg-block-p ()
  (unless (or (null containing-sexp)
              ;; Unbraced blocks in a { block are not arg blocks
              (and (ess-unbraced-block-p)
                   (ess-at-containing-sexp
                     (looking-at "{"))))
    (cond
     ;; Unbraced body
     ((ess-at-indent-point
        (and (ess-unbraced-block-p)
             (goto-char containing-sexp)
             (ess-looking-at-call-opening "[[(]")))
      'body)
     ;; Indentation of opening brace as argument
     ((ess-at-containing-sexp
        (ess-looking-at-call-opening "[[(]"))
      'opening)
     ;; Indentation of body or closing brace as argument
     ((ess-at-containing-sexp
        (and (or (looking-at "{")
                 (ess-looking-at-block-paren-p))
             prev-containing-sexp
             (goto-char prev-containing-sexp)
             (ess-looking-at-call-opening "[[(]")))
      'body))))

(defun ess-calculate-indent--block (&optional offset)
  (let ((arg-block (ess-arg-block-p)))
    (cond (arg-block
           (ess-calculate-indent--arg-block offset arg-block))
          (t
           ;; Block is not part of an arguments list. Climb over any
           ;; block opening (function declaration, etc) to indent from
           ;; starting indentation.
           (or (ess-climb-block-opening)
               (and (goto-char containing-sexp)
                    (ess-climb-block-opening)))
           (+ (current-indentation) (or offset (ess-offset 'block)))))))

(defun ess-calculate-indent--arg-block (offset arg-block)
  (let* ((block-type (cond ((or (ess-at-containing-sexp
                                  (and (eq arg-block 'body)
                                       (ess-climb-function-decl t)))
                                (ess-at-indent-point
                                  (and (eq arg-block 'opening)
                                       (ess-backward-sexp 2)
                                       (looking-at "function\\b"))))
                            'fun-decl)
                           ((ess-at-indent-point
                              (ess-unbraced-block-p))
                            'unbraced)
                           ((ess-at-containing-sexp
                              (not (ess-looking-back-attached-name-p)))
                            'bare-block)
                           (t)))
         (call-pos (if (and (not (eq block-type 'unbraced))
                            (not (eq arg-block 'opening)))
                       (goto-char prev-containing-sexp)
                     (prog1 containing-sexp
                       (goto-char indent-point)))))
    (ess-calculate-indent--args offset (ess-offset-type 'block)
                                call-pos indent-point block-type)))

;; This function is currently the speed bottleneck of the indentation
;; engine. This is due to the need to call (ess-maximum-args-indent)
;; to check if some previous arguments have been pushed off from their
;; natural indentation: we need to check the whole call. This is very
;; inefficient especially when indenting a region containing a large
;; function call (e.g. some dplyr's data cleaning code). Should be
;; solved by implementing a cache as in (syntax-ppss), though it's
;; probably not worth the work.
(defun ess-calculate-indent--args (&optional offset type call-pos to block)
  (let* ((call-pos (or call-pos containing-sexp))
         (max-col (prog1 (unless (eq type 'prev-line)
                           (ess-maximum-args-indent call-pos to))
                    (goto-char call-pos)))
         (override (and ess-align-arguments-in-calls
                        (save-excursion
                          (ess-climb-object)
                          (some 'looking-at ess-align-arguments-in-calls))))
         (type-sym (cond (block 'block)
                         ((looking-at "[[:blank:]]*[([][[:blank:]]*\\($\\|#\\)")
                          'arguments-newline)
                         (t 'arguments)))
         (type (or type
                   (and override 'open-delim)
                   (ess-offset-type type-sym)))
         (offset (or offset
                     (and (not block) (eq type 'open-delim) 0)
                     (ess-offset type-sym)))
         (indent
          (cond
           ;; Indent from opening delimiter
           ((eq type 'open-delim)
            (ess-calculate-indent--args-open-delim))
           ;; Indent from attached name
           ((eq type 'prev-call)
            (ess-calculate-indent--args-prev-call))
           ;; Indent from previous line indentation
           ((eq type 'prev-line)
            (ess-calculate-indent--args-prev-line))
           (t
            (error "Malformed offset")))))
    (if max-col
        (ess-adjust-argument-indent indent offset max-col block)
      (+ indent offset))))

(defun ess-calculate-indent--args-open-delim ()
  (forward-char)
  (current-column))

(defun ess-calculate-indent--args-prev-call ()
  ;; Handle brackets chains such as ][ (cf data.table)
  (ess-climb-chained-brackets)
  ;; Handle call chains
  (if ess-indent-from-chain-start
      (while (and (ess-backward-sexp)
                  (when (looking-back "[[(][ \t,]*" (line-beginning-position))
                    (goto-char (match-beginning 0)))))
    (ess-backward-sexp))
  (when ess-indent-from-lhs
    (ess-climb-lhs))
  (if (and nil
           (eq block 'fun-decl)
           (not (eq arg-block 'opening))
           (not (eq (ess-offset-type type-sym) 'open-delim)))
      (+ (ess-offset 'block) (current-column))
    (current-column)))

(defun ess-calculate-indent--args-prev-line ()
  (ess-at-indent-point
    (cond
     ;; Closing delimiters are actually not indented at
     ;; prev-line, but at opening-line
     ((looking-at "[]})]")
      (ess-up-list -1)
      (when (looking-at "{")
        (ess-climb-block-opening))
      (current-indentation))
     ;; Function blocks need special treatment
     ((and (eq type 'prev-line)
           (eq block 'fun-decl))
      (goto-char containing-sexp)
      (ess-climb-block-opening)
      (current-indentation))
     ;; Regular case
     (t
      ;; Find next non-empty line to indent from
      (while (and (= (forward-line -1) 0)
                  (looking-at "[ \t]*\\($\\|#\\)")))
      (goto-char (ess-code-end-position))
      ;; Climb relevant structures
      (unless (ess-climb-block-opening)
        (when (eq (char-before) ?,)
          (forward-char -1))
        (ess-climb-expression)
        (ess-climb-continuations))
      ;; The following ensures that only the first line
      ;; counts. Otherwise consecutive statements would get
      ;; increasingly more indented.
      (when (and block
                 containing-sexp
                 (not (eq block 'unbraced))
                 (save-excursion
                   (/= (line-number-at-pos)
                       (progn (goto-char containing-sexp)
                              (line-number-at-pos)))))
        (setq offset 0))
      (current-indentation)))))

;; Indentation of arguments needs to keep track of how previous
;; arguments are indented. If one of those has a smaller indentation,
;; we push off the current line from its natural indentation. For
;; block arguments, we still need to push off this column so we ignore
;; it.
(defun ess-adjust-argument-indent (base offset max-col push)
  (if push
      (+ (min base max-col) offset)
    (min (+ base offset) max-col)))

;; When previous arguments are shifted to the left (can happen in
;; several situations) compared to their natural indentation, the
;; following lines should not get indented past them. The following
;; function checks the minimum indentation for all arguments of the
;; current function call or bracket indexing.
(defun ess-maximum-args-indent (&optional from to)
  (let* ((to (or to (point)))
         (to-line (line-number-at-pos to))
         (from-line (progn
                      (goto-char (1+ (or from containing-sexp)))
                      (line-number-at-pos)))
         (prev-pos (1- (point)))
         max-col)
    (while (< (line-number-at-pos) to-line)
      (forward-line)
      (ess-back-to-indentation)
      ;; Ignore the line with the function call, the line to be
      ;; indented, and empty lines.
      (unless (or (>= (line-number-at-pos) to-line)
                  (looking-at "[ \t]*\\($\\|#\\)"))
        (let ((indent (cond
                       ;; First line: minimum indent is right after (
                       ((= (line-number-at-pos) from-line)
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (current-column)))
                       ;; Handle lines starting with a comma
                       ((save-excursion
                          (looking-at ","))
                        (+ (current-indentation) 2))
                       (t
                        (current-indentation)))))
          (setq max-col (min indent (or max-col indent))))))
    max-col))

(defvar ess-R-operator-pattern "<-\\|!=\\|%[^ \t]*%\\|[-:+*/><=&|~]"
  "Regular expression for an operator")

(defvar ess-R-definition-op-pattern "<-\\|:=\\|~"
  "Regular expression for a definition operator")

(defun ess-looking-at-operator-p ()
  (looking-at (concat "[[:blank:]]*\\(" ess-R-operator-pattern "\\)")))

(defun ess-looking-at-definition-op-p (&optional no-fun-arg)
  (save-excursion
    (skip-chars-forward "[ \t]")
    (or (looking-at ess-R-definition-op-pattern)
        (and (looking-at "=[^=]")
             (if no-fun-arg
                 (not (ess-looking-at-parameter-op-p))
               t)))))

(defun ess-looking-back-definition-op-p (&optional no-fun-arg)
  (save-excursion
    (and (ess-backward-sexp)
         (ess-forward-sexp)
         (ess-looking-at-definition-op-p no-fun-arg))))

;; Move to leftmost side of a call (either the first letter of its
;; name or its closing delim)
(defun ess-move-to-leftmost-side ()
  (when (or (looking-at "[({]")
            (ess-looking-at-call-p))
    (ess-save-excursion-when-nil
      (let ((start-col (current-column)))
        (skip-chars-forward "^{[(")
        (forward-char)
        (ess-up-list)
        (forward-char -1)
        (< (current-column) start-col)))))

(defun ess-max-col ()
  (let ((max-col (point)))
    (save-excursion
      (while (< (point) indent-point)
        (setq max-col (current-column))
        (forward-line)
        (ess-back-to-indentation)))
    max-col))

(defun ess-calculate-indent--continued ()
  "If a continuation line, return an indent of this line,
otherwise nil."
  (save-excursion
    (let* ((start-line (line-number-at-pos))
           (prev-pos 0)
           (cascade (eq (ess-offset-type 'continued) 'cascade))
           (climbed (progn
                      ;; Try to climb block opening
                      (ess-save-excursion-when-nil
                        (and (looking-at "{")
                             (ess-climb-block-opening)
                             ;; But only if it's on its own line
                             (= (save-excursion
                                  (back-to-indentation)
                                  (point))
                                (point))))
                      (ess-climb-continuations cascade)))
           max-col)
      (when climbed
        (cond
         ;; Overridden calls
         ((and ess-align-continuations-in-calls
               (not (eq climbed 'def-op))
               containing-sexp
               (save-excursion
                 (goto-char containing-sexp)
                 (looking-at "[[(]")))
          (setq max-col (ess-max-col))
          (ess-move-to-leftmost-side)
          (+ (min (current-column) max-col)
             (if (eq climbed 'def-op)
                 (ess-offset 'continued)
               0)))
         ;; Regular case
         (t
          (let ((first-indent (or (eq climbed 'def-op)
                                  (save-excursion
                                    (when (ess-looking-back-closing-p)
                                      (ess-climb-expression))
                                    (not (ess-climb-continuations cascade))))))
            ;; Record all indentation levels between indent-point and
            ;; the line we climbed. Some lines may have been pushed off
            ;; their natural indentation. These become the new
            ;; reference.
            (setq max-col (ess-max-col))
            ;; Indenting continuations from the front of closing
            ;; delimiters looks better
            (when
                (ess-looking-back-closing-p)
              (backward-char))
            (+ (min (current-column) max-col)
               (cond
                ((eq (ess-offset-type 'continued) 'cascade)
                 (ess-offset 'continued))
                (first-indent
                 (ess-offset 'continued))
                (t
                 0))))))))))

(defun ess-looking-back-closing-p ()
  (memq (char-before) '(?\] ?\} ?\))))

(defun ess-climb-continuations (&optional cascade ignore-ifelse)
  (let ((start-line (line-number-at-pos))
        (moved 0)
        (last-pos (point))
        last-line prev-point def-op expr)
    (setq last-line start-line)
    (when (ess-while (and (<= moved 1)
                          (ess-climb-operator)
                          (ess-climb-continuations--update-state 'op)
                          (ess-climb-expression ignore-ifelse)
                          (/= last-pos (point)))
            (ess-climb-continuations--update-state)
            (setq last-pos (point)))
      (when (and prev-point
                 (or (= moved 3)
                     (not expr)))
        (goto-char prev-point))
      (if def-op 'def-op (< (line-number-at-pos) start-line)))))

(defun ess-climb-continuations--update-state (&optional op)
  ;; Climbing multi-line expressions should not count as moving up
  (when op
    (setq expr (ess-looking-back-closing-p)))
  (let ((cur-line (line-number-at-pos)))
    (when (and last-line
               (< cur-line last-line)
               (or cascade (not expr)))
      (setq moved (1+ moved))
      (setq last-line cur-line)))
  ;; Don't update counter after climbing operator or climbing too high
  (when (and (not op)
             (<= moved 1))
    (setq prev-point (point)))
  (when (and (ess-looking-at-definition-op-p)
             (<= moved 1))
    (setq def-op t))
  t)

(defun ess-indent-call (&optional start)
  (save-excursion
    (when (ess-climb-function-calls)
      (setq start (or start (point)))
      (skip-chars-forward "^[(")
      (forward-char)
      (ess-up-list)
      (indent-region start (point)))))

(defun ess-climb-function-calls ()
  (let (containing-sexp finished)
    (ess-while (and (not finished)
                    (or
                     ;; Point inside call
                     (ess-save-excursion-when-nil
                       (and (setq containing-sexp
                                  (ess-containing-sexp-position))
                            (goto-char containing-sexp)
                            (ess-climb-name)))
                     ;; Point on function name
                     (ess-save-excursion-when-nil
                       (and (ess-jump-name))
                       (and (looking-at "[[(]")
                            (ess-climb-name)
                            (setq finished t))))))))


;;;*;;; Predefined indentation styles

(defun ess-set-style (&optional style quiet)
  "Set up the `ess-mode' style variables from the `ess-style' variable
or if STYLE argument is given, use that.  It makes the ESS indentation
style variables buffer local."

  (interactive)
  (let ((ess-styles (mapcar 'symbol-name (mapcar 'car ess-style-alist))))
    (unless style
      (setq style
            (intern (ess-completing-read "Set ESS mode indentation style"
                                         ess-styles nil t nil nil ess-default-style))))
    (setq style (or style ess-style))
    (make-local-variable 'ess-style)
    (if (memq (symbol-name style) ess-styles)
        (setq ess-style style)
      (error (format "Bad ESS style: %s" style)))
    (if (not quiet)
        (message "ESS-style: %s" ess-style))
    ;; finally, set the indentation style variables making each one local
    (mapc (lambda (ess-style-pair)
            (make-local-variable (car ess-style-pair))
            (set (car ess-style-pair)
                 (cdr ess-style-pair)))
          (cdr (assq ess-style ess-style-alist)))
    ess-style))


;;;*;;; Call filling engine

;; Unroll arguments to a single line until closing marker is found.
(defun ess-fill--unroll-lines (bounds &optional jump-cont)
  (let* ((last-pos (point-min))
         (containing-sexp (ess-containing-sexp-position))
         prefix-break)
    (goto-char (car bounds))
    (goto-char (ess-code-end-position))
    (while (and (/= (point) last-pos)
                (< (line-end-position)
                   (cadr bounds))
                (not prefix-break))
      (setq last-pos (point))
      ;; Check whether we ended up in a sub call. In this case, jump
      ;; over it, otherwise, join lines.
      (let ((contained-sexp (ess-containing-sexp-position)))
        (cond ((and contained-sexp
                    (not (= containing-sexp contained-sexp)))
               (goto-char (1+ contained-sexp))
               (ess-up-list))
              ;; Jump over continued statements
              ((and jump-cont (ess-looking-back-operator-p))
               (ess-skip-blanks-forward t)
               (ess-jump-continuations))
              ;; Jump over comments
              ((looking-at "#")
               (forward-line)
               (ess-indent-line))
              (t
               (join-line 1))))
      (goto-char (ess-code-end-position)))
    (goto-char (car bounds))))

;; This should be called inside a call
(defun ess-args-bounds ()
  (save-excursion
    (let* ((containing-sexp (ess-containing-sexp-position))
           (beg (1+ containing-sexp))
           (call-beg (ess-at-containing-sexp
                       (ess-climb-name)
                       (point))))
      (and beg
           ;; (ess-up-list) can't find its way when point is on a
           ;; backquoted name, so start from `beg'.
           (goto-char beg)
           (ess-up-list)
           (prog1 t
             (forward-char -1))
           (list beg (point-marker) call-beg)))))

(defun ess-jump-char (char)
  (when (looking-at (concat "[ \t]*\\(" char "\\)"))
    (goto-char (match-end 1))))

(defvar ess-fill--orig-pos nil
  "Original position of cursor.")

(defvar ess-fill--orig-state nil
  "Backup of original code to cycle back to original state.")

(defvar ess-fill--second-state nil
  "Backup of code produce by very first cycling. If this is equal
  to orig-state, no need to cycle back to original state.")

(defvar ess-fill--style-level nil
  "Filling style used in last cycle.")

(defun ess-fill--substring (bounds)
  (buffer-substring (car bounds) (marker-position (cadr bounds))))

;; Detect repeated commands
(defun ess-fill-style (type bounds)
  (let ((max-level
         ;; This part will be simpler once we have the style alist
         (cond ((eq type 'calls)
                ;; No third style either when ess-offset-arguments is
                ;; set to 'open-delim, or when ess-fill-calls-newlines
                ;; is nil and no numeric prefix is given
                (if (and (not (eq (ess-offset-type 'arguments)
                                  'open-delim))
                         (or ess-fill-calls-newlines
                             (numberp current-prefix-arg)))
                    3
                  2))
               ((eq type 'continuations)
                2))))
    (if (not (memq last-command '(fill-paragraph-or-region
                                  fill-paragraph)))
        (progn
          ;; Record original state on first cycling
          (setq ess-fill--orig-state (ess-fill--substring bounds))
          (setq ess-fill--orig-pos (point))
          (setq ess-fill--second-state nil)
          (setq ess-fill--style-level 1))
      ;; Also record state on second cycling
      (when (and (= ess-fill--style-level 1)
                 (null ess-fill--second-state))
        (setq ess-fill--second-state (ess-fill--substring bounds)))
      (cond ((>= ess-fill--style-level max-level)
             (let ((same-last-and-orig (string= (ess-fill--substring bounds)
                                                ess-fill--orig-state))
                   (same-2nd-and-orig (string= ess-fill--orig-state
                                               ess-fill--second-state)))
               ;; Avoid cycling to the same state twice
               (cond ((and same-last-and-orig
                           same-2nd-and-orig)
                      (setq ess-fill--style-level 2))
                     ((or same-last-and-orig
                          same-2nd-and-orig)
                      (setq ess-fill--style-level 1))
                     (t
                      (setq ess-fill--style-level 0)))))
            (ess-fill--style-level
             (setq ess-fill--style-level (1+ ess-fill--style-level))))))
  ess-fill--style-level)

(defun ess-fill-args ()
  (let ((start-pos (point-min))
        (orig-col (current-column))
        (orig-line (line-number-at-pos))
        (bounds (ess-args-bounds))
        ;; Set undo boundaries manually
        (undo-inhibit-record-point t)
        last-pos last-newline prefix-break
        infinite style)
    (when (not bounds)
      (error "Could not find function bounds"))
    (setq style (ess-fill-style 'calls bounds))
    (if (= style 0)
        (progn
          (delete-region (car bounds) (marker-position (cadr bounds)))
          (insert ess-fill--orig-state)
          ;; Restore the point manually. (save-excursion) wouldn't
          ;; work here because we delete the text rather than just
          ;; modifying it.
          (goto-char ess-fill--orig-pos)
          (message "Back to original formatting"))
      (when ess-blink-refilling
        (ess-blink-region (nth 2 bounds)
                          (1+ (marker-position (cadr bounds)))))
      (undo-boundary)
      (save-excursion
        (ess-fill--unroll-lines bounds t)
        (cond
         ;; Second level, start with first argument on a newline
         ((and (= style 2)
               ess-fill-calls-newlines
               (not (looking-at "[ \t]*#")))
          (newline-and-indent))
         ;; Third level, start a newline after N arguments
         ((and (= style 3)
               (not (looking-at "[ \t]*#")))
          (let ((i (if (numberp current-prefix-arg)
                       current-prefix-arg
                     1)))
            (while (and (> i 0)
                        (ess-jump-parameter)
                        (ess-jump-char ","))
              (setq i (1- i))))
          (newline-and-indent)))
        (while (and (not (looking-at "[])]"))
                    (/= (point) (or last-pos 1))
                    (not infinite))
          (setq prefix-break nil)
          ;; Record start-pos as future breaking point to avoid breaking
          ;; at `=' sign
          (while (looking-at "[ \t]*[\n#]")
            (forward-line)
            (ess-back-to-indentation))
          (setq start-pos (point))
          (while (and (< (current-column) fill-column)
                      (not (looking-at "[])]"))
                      (/= (point) (or last-pos 1))
                      ;; Break after one pass if prefix is active
                      (not prefix-break))
            (when (memq style '(2 3))
              (setq prefix-break t))
            (ess-jump-char ",")
            (setq last-pos (point))
            ;; Jump expression and any continuations. Reindent all lines
            ;; that were jumped over
            (let ((cur-line (line-number-at-pos))
                  end-line)
              (when (ess-jump-parameter)
                (setq last-newline nil))
              (save-excursion
                (when (< cur-line (line-number-at-pos))
                  (setq end-line (line-number-at-pos))
                  (ess-goto-line (1+ cur-line))
                  (while (<= (line-number-at-pos) end-line)
                    (ess-indent-line)
                    (forward-line))))))
          (when (or (>= (current-column) fill-column)
                    prefix-break
                    ;; May be useful later
                    (and (= style 4)
                         (looking-at "[ \t]*[])]")
                         (setq last-pos (point))))
            (if (and last-pos (/= last-pos start-pos))
                (goto-char last-pos)
              (ess-jump-char ","))
            (cond ((looking-at "[ \t]*[#\n]")
                   (forward-line)
                   (ess-indent-line)
                   (setq last-newline nil))
                  ;; With levels 2 and 3, closing delim goes on a newline
                  ((looking-at "[ \t]*[])]")
                   (when (and (memq style '(2 3))
                              ess-fill-calls-newlines
                              (not last-newline))
                     (newline-and-indent)
                     ;; Prevent indenting infinitely
                     (setq last-newline t)))
                  ((not last-newline)
                   (newline-and-indent)
                   (setq last-newline t))
                  (t
                   (setq infinite t)))))
        ;; Reindent surrounding context
        (ess-indent-call (car bounds)))
      ;; Signal marker for garbage collection
      (set-marker (cadr bounds) nil)
      (undo-boundary))))

(defun ess-continuations-bounds ()
  (save-excursion
    (let ((orig-point (point))
          (beg (progn
                 (ess-climb-object)
                 (while (ess-climb-continuations))
                 (ess-jump-parameter-name)
                 (point))))
      (when beg
        (goto-char orig-point)
        (ess-jump-object)
        (ess-jump-continuations)
        (list beg (point-marker))))))

(defun ess-fill-continuations ()
  (let ((bounds (ess-continuations-bounds))
        (undo-inhibit-record-point t)
        (last-pos (point-min))
        style last-newline infinite)
    (when (not bounds)
      (error "Could not find statements bounds"))
    (setq style (ess-fill-style 'continuations bounds))
    (if (= style 0)
        (progn
          (delete-region (car bounds) (marker-position (cadr bounds)))
          (insert ess-fill--orig-state)
          (goto-char ess-fill--orig-pos)
          (message "Back to original formatting"))
      (when ess-blink-refilling
        (ess-blink-region (car bounds) (marker-position (cadr bounds))))
      (undo-boundary)
      (save-excursion
        (ess-fill--unroll-lines bounds)
        (while (and (< (point) (cadr bounds))
                    (/= (point) (or last-pos 1))
                    (not infinite))
          (setq last-pos (point))
          (when (and (ess-jump-expression)
                     (indent-according-to-mode)
                     (not (> (current-column) fill-column)))
            (setq last-newline nil))
          (ess-jump-operator)
          (if (or (and (> (current-column) fill-column)
                       (goto-char last-pos))
                  (= style 2))
              (progn
                (ess-jump-operator)
                (unless (= (point) (cadr bounds))
                  (when last-newline
                    (setq infinite t))
                  (newline-and-indent)
                  (setq last-newline t)))
            (setq last-newline nil)))
        (ess-indent-call (car bounds)))
      (set-marker (cadr bounds) nil)
      (undo-boundary))))


;;*;; Creating and manipulating dump buffers

;;;*;;; The user command
(defun ess-dump-object-into-edit-buffer (object)
  "Edit an ESS object in its own buffer.

Without a prefix argument, this simply finds the file pointed to by
`ess-source-directory'. If this file does not exist, or if a
prefix argument is given, a dump() command is sent to the ESS process to
generate the source buffer."
  (interactive
   (progn
     (ess-force-buffer-current "Process to dump from: ")
     (if (ess-ddeclient-p)
         (list (read-string "Object to edit: "))
       (ess-read-object-name "Object to edit"))))

  (let* ((dirname (file-name-as-directory
                   (if (stringp ess-source-directory)
                       ess-source-directory
                     (with-current-buffer (process-buffer (ess-get-process
                                                           ess-local-process-name))
                       (ess-setq-vars-local ess-customize-alist)
                       (apply ess-source-directory nil)))))
         (filename (concat dirname (format ess-dump-filename-template object)))
         (old-buff (get-file-buffer filename)))

    ;; If the directory doesn't exist, offer to create it
    (if (file-exists-p (directory-file-name dirname)) nil
      (if (y-or-n-p                     ; Approved
           (format "Directory %s does not exist. Create it? " dirname))
          (make-directory (directory-file-name dirname))
        (error "Directory %s does not exist." dirname)))

    ;; Three options:
    ;;  (1) Pop to an existing buffer containing the file in question
    ;;  (2) Find an existing file
    ;;  (3) Create a new file by issuing a dump() command to S
    ;; Force option (3) if there is a prefix arg

    (if current-prefix-arg
        (ess-dump-object object filename)
      (if old-buff
          (progn
            (pop-to-buffer old-buff)
            (message "Popped to edit buffer."))
        ;; No current buffer containing desired file
        (if (file-exists-p filename)
            (progn
              (ess-find-dump-file-other-window filename)
              (message "Read %s" filename))
          ;; No buffer and no file
          (ess-dump-object object filename))))))

(defun ess-dump-object (object filename)
  "Dump the ESS object OBJECT into file FILENAME."
  (let ((complete-dump-command (format inferior-ess-dump-command
                                       object filename)))
    (if (file-writable-p filename) nil
      (error "Can't dump %s as %f is not writeable." object filename))

    (if (ess-ddeclient-p)
        ;; ddeclient version
        (ess-dump-object-ddeclient object filename)

      ;; else: "normal", non-DDE behavior:

      ;; Make sure we start fresh
      (if (get-file-buffer filename)
          (kill-buffer (get-file-buffer filename)))

      (ess-command complete-dump-command)
      (message "Dumped in %s" filename)

      (ess-find-dump-file-other-window filename)

      ;; PD, 1Apr97
      ;;This ensures that the object gets indented according to ess-mode,
      ;;not as the R/S deparser does it. At the same time, it gets rid
      ;;of the mess generated by sending TAB characters to the readline
      ;;functions in R when you eval-buffer-*.
      (indent-region (point-min-marker) (point-max-marker) nil)
      (set-buffer-modified-p nil) ; no need to safe just because of indenting

      ;; Don't make backups for temporary files; it only causes clutter.
      ;; The ESS object itself is a kind of backup, anyway.
      (unless ess-keep-dump-files
        (make-local-variable 'make-backup-files)
        (setq make-backup-files nil))

      ;; Don't get confirmation to delete dumped files when loading
      (if (eq ess-keep-dump-files 'check)
          (setq ess-keep-dump-files nil))

      ;; Delete the file if necessary
      (if ess-delete-dump-files
          (delete-file (buffer-file-name))))))

(defun ess-find-dump-file-other-window (filename)
  "Find ESS source file FILENAME in another window."

  (if (file-exists-p filename) nil
    (ess-write-to-dribble-buffer
     (format "%s does not exist. Bad dump, starting fresh." filename)))

  ;; Generate a buffer with the dumped data
  (find-file-other-window filename)
  (ess-mode ess-customize-alist)

  (auto-save-mode 1)            ; Auto save in this buffer
  (setq ess-local-process-name ess-current-process-name)

  (if ess-function-template
      (progn
        (goto-char (point-max))
        (if (re-search-backward ess-dumped-missing-re nil t)
            (progn
              (replace-match ess-function-template t t)
              (set-buffer-modified-p nil) ; Don't offer to save if killed now
              (goto-char (point-min))
              (condition-case nil
                  ;; This may fail if there are no opens
                  (down-list 1)
                (error nil)))))))

;; AJR: XEmacs, makes sense to dump into "other frame".
(defun ess-dump-object-into-edit-buffer-other-frame (object)
  "Edit an ESS object in its own frame."
  (switch-to-buffer-other-frame (ess-dump-object-into-edit-buffer object)))

(provide 'ess-mode)

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

;;; ess-mode.el ends here
