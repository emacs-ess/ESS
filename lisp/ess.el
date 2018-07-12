;;; ess.el --- Emacs Speaks Statistics

;; Copyright (C) 1989-1994 Doug Bates, Ed Kademan, Frank Ritter, David Smith.
;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2017 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Created: 7 Jan 1994
;; Maintainer: ESS-core <ESS-core@r-project.org>

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

(eval-when-compile
  (require 'cl)
  (require 'cl-lib))
(require 'ess-custom)
(require 'ess-utils)
(require 'ess-generics)
(require 'ess-inf)

;; FIXME: should this be optional?
(require 'ess-noweb-mode)

 ; ESS mode

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
    (define-key map (kbd "RET")  'ess-newline-and-indent)
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
    ;; FIXME: The next three can only work in S/R - mode
    (define-key map "\C-\M-a"    'ess-goto-beginning-of-function-or-para)
    (define-key map "\C-\M-e"    'ess-goto-end-of-function-or-para)
    (define-key map "\C-xnd"     'ess-narrow-to-defun-or-para)
    (define-key map "\C-xnf"     'ess-narrow-to-defun-or-para)
    (define-key map "\C-c\C-y"   'ess-switch-to-ESS-deprecated)
    (define-key map "\C-c\C-z"   'ess-switch-to-inferior-or-script-buffer)
    (define-key map "\C-c\C-l"   'ess-load-file)
    ;;; Make an alias because C-c C-l is taken up by comint in inferiors
    (define-key map "\C-c\M-l"   'ess-load-file)
    (define-key map "\C-c\C-v"   'ess-display-help-on-object)
    (define-key map "\C-c\C-s"   'ess-switch-process)
    (define-key map "\C-c\t"     'ess-complete-object-name-deprecated)
    (define-key map "\M-?"       'ess-list-object-completions)
    (define-key map "\C-c\C-k"   'ess-force-buffer-current)
    (define-key map "\C-c`"      'ess-show-traceback)
    (define-key map [(control ?c) ?~] 'ess-show-call-stack)
    (define-key map "\C-\M-q"    'ess-indent-exp)
    (define-key map "{"          'skeleton-pair-insert-maybe)
    (define-key map "}"          'skeleton-pair-insert-maybe)
    (define-key map "\C-\M-h"    'ess-mark-function-or-para)
    (define-key map "\t"         'ess-indent-or-complete)
    (define-key map "\C-c\C-q"   'ess-quit)
    (define-key map "\M-\r"      'ess-use-this-dir)
    (define-key map ","          'ess-smart-comma)
    (define-key map "\C-c\C-d"   'ess-doc-map)
    (define-key map "\C-c\C-e"   'ess-extra-map)
    (define-key map "\C-c\C-t"   'ess-dev-map)
    (when ess-smart-S-assign-key
      (define-key map ess-smart-S-assign-key 'ess-insert-assign))
    (define-key map (kbd "C-c C-=") 'ess-cycle-assignment)
    map)
  "Keymap for `ess-mode'.")

;; Redefine substituted commands
(substitute-key-definition 'indent-new-comment-line
                           'ess-indent-new-comment-line
                           ess-mode-map global-map)

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
    (define-key ess-extra-map "\C-r" 'inferior-ess-reload)
    (define-key ess-extra-map "r" 'inferior-ess-reload)
    (define-key ess-extra-map "\C-s" 'ess-set-style)
    (define-key ess-extra-map "s" 'ess-set-style)
    (define-key ess-extra-map "\C-t" 'ess-build-tags-for-directory)
    (define-key ess-extra-map "t" 'ess-build-tags-for-directory)
    (define-key ess-extra-map "\C-w" 'ess-execute-screen-options)
    (define-key ess-extra-map "w" 'ess-execute-screen-options)
    (define-key ess-extra-map "/" 'ess-set-working-directory)
    ess-extra-map)
  "ESS extra map")

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
     ;; ["Recreate R and S versions known to ESS" (ess-r-s-versions-creation+menu) t]
     ("Start Process"
      ["R"     R   :help "Start a new R process" :active t]
      ["S"     S   :help "Start a new S process" :active t]
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
    ["Send bug report"  ess-submit-bug-report           t]))

;;;###autoload
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
\\[ess-indent-exp] command indents each line of the syntactic unit following point.

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
    (setq mode-name (concat "ESS[" (or ess-dialect ess-language) "]")))
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
  (add-hook 'comint-dynamic-complete-functions 'ess-complete-filename nil 'local)
  (delq t comint-dynamic-complete-functions)
  (set (make-local-variable 'comint-completion-addsuffix)
       (cons "/" ""))

  (add-hook 'ess-idle-timer-functions 'ess-synchronize-dirs nil 'local)
  (ess-load-extras)
  (run-mode-hooks 'prog-mode-hook)
  (run-mode-hooks 'ess-mode-hook)
  (ess-write-to-dribble-buffer "\nFinished setting up ESS-mode.\n"))

;; Set parent to `prog-mode'
(put 'ess-mode 'derived-mode-parent 'prog-mode)
(set-keymap-parent ess-mode-map prog-mode-map)

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


;;*;; Dispatching infrastructure for dialects

(defvar ess--make-local-vars-permanent nil
  "If this variable is non-nil in a buffer make all variable permannet.
Used in noweb modes.")
(make-variable-buffer-local 'ess--make-local-vars-permanent)
(put 'ess--make-local-vars-permanent 'permanent-local t)

(defun ess-setq-vars-local (alist &optional buf)
  "Set language variables from ALIST, in buffer BUF, if desired."
  (if buf (set-buffer buf))
  (mapc (lambda (pair)
          (make-local-variable (car pair))
          (set (car pair) (eval (cdr pair)))
          (when ess--make-local-vars-permanent
            (put (car pair) 'permanent-local t)) ;; hack for Rnw
          )
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
  (mapc (lambda (pair)
          (set-default (car pair) (eval (cdr pair))))
        alist)
  (ess-write-to-dribble-buffer
   (format "ess-setq-vars-default 1: ess-language=%s, -dialect=%s, buf=%s, comint..echoes=%s, comint..sender=%s\n"
           ess-language ess-dialect buf comint-process-echoes comint-input-sender)))


;;*;; User commands in ess-mode

;;;*;;; Miscellaneous

(defun ess-install-library ()
  "Install library/package for current dialect.
Currently works only for R."
  (interactive)
  (cond
   ((fboundp ess-install-library-function)
    (funcall ess-install-library-function))
   (t
    (error "Sorry, not available for %s" ess-dialect))))


;;;*;;; Motion / manipulation commands

(defun ess-beginning-of-function (&optional no-error)
  "Leave (and return) the point at the beginning of the current ESS function.
If the optional argument NO-ERROR is non-nil, the function returns nil when
it cannot find a function beginning."
  ;; FIXME: should not throw error in accordance with beginning-of-defun and
  ;; beginning-of-defun-function specification
  ;; FIXME: should __WORK__ in the crucial case: large function w/ internal function defs
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
              ess-r-set-function-start
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
        (setq in-set-S4 (looking-at ess-r-set-function-start))
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
      (let ((in-set-S4 (looking-at ess-r-set-function-start))
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

(defun ess-newline-and-indent ()
  (interactive)
  (cond ((string= ess-dialect "R")
         (ess-roxy-newline-and-indent))
        (t
         (newline-and-indent))))

(defun ess-indent-new-comment-line ()
  (interactive)
  (cond ((string= ess-dialect "R")
         (ess-roxy-indent-new-comment-line))
        (t
         (indent-new-comment-line))))


;;;*;;; Formatting / indentation

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

;; FIXME: Move into ess-indent-or-complete, indentation functions are overly
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
                          (not (looking-at "\\w\\|\\s_\\|\\s)\\|\\s.")))))
        (completion-at-point)))))

(defun ess-indent-exp ()
  "Indent each line of the ESS grouping following point."
  (interactive)
  (cond ((string= ess-dialect "R")
         (ess-r-indent-exp))
        (t
         (save-excursion
           (let ((start (point))
                 (end (ignore-errors (forward-sexp 1) (point))))
             (when end
               (indent-region start end)))))))

(defun ess-indent-line ()
  "Indent current line as ESS code.
Return the amount the indentation changed by."
  ;; fixme: make this work with standard indent-line-function
  (if (fboundp ess-indent-line-function)
      (funcall ess-indent-line-function)
    ;; else S and R default behavior
    (ess-r-indent-line)))


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

;;;###autoload
(defun ess-parse-errors (&optional showerr reset)
  "Jump to error in last loaded ESS source file.
With prefix argument, only shows the errors ESS reported.

RESET is for compatibility with `next-error' and is ignored."
  (interactive "P")
  (ess-make-buffer-current)
  (let ((errbuff (get-buffer ess-error-buffer-name)))
    (when (not errbuff)
      (error "You need to do a load first!"))
    (set-buffer errbuff)
    (goto-char (point-max))
    ;; FIXME: R does not give "useful" error messages by default. We
    ;; could try to use a more useful one, via
    ;; options(error=essErrorHandler)
    (cond ((re-search-backward ess-error-regexp nil t)
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
             (princ errmess t)))
          (t
           (message "Not a syntax error.")
           (ess-display-temp-buffer errbuff)))))


;;*;; Creating and manipulating dump buffers
;;;###autoload
(defun ess-dump-object-into-edit-buffer (object)
  "Edit an ESS OBJECT in its own buffer.
Without a prefix argument, this simply finds the file pointed to by
`ess-source-directory'.  If this file does not exist, or if a
prefix argument is given, a dump() command is sent to the ESS process to
generate the source buffer."
  (interactive
   (progn
     (ess-force-buffer-current "Process to dump from: ")
     (ess-read-object-name "Object to edit")))

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

(ess-defgeneric ess-dump-object (object filename)
  "Dump the ESS object OBJECT into file FILENAME."
  (let ((complete-dump-command (format inferior-ess-dump-command
                                       object filename)))
    (if (file-writable-p filename) nil
      (error "Can't dump %s as %f is not writeable." object filename))

    (:override
     ;; Make sure we start fresh
     (when (get-file-buffer filename)
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
     (when (eq ess-keep-dump-files 'check)
       (setq ess-keep-dump-files nil))

     ;; Delete the file if necessary
     (when ess-delete-dump-files
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

(defun ess-define-runner (name dialect &optional path)
  "Create a function NAME.
This function starts the inferior process with the specified
version. DIALECT can be \"R,\" \"S,\", \"SAS.\" If given, PATH
should be the absolute path to the program. It defaults to NAME."
  (lexical-let ((name name)
                (dialect dialect)
                (path path))
    (fset (intern name)
          (lambda (&optional start-args)
            "Start this process version in an inferior ESS buffer.
Function defined using `ess-define-runner'."
            (interactive "P")
            (cond ((string= dialect "R")
                   (let ((inferior-ess-r-program (or path name)))
                     (R start-args)))
                  ((string= dialect "S")
                   (let ((inferior-S+-program (or path name)))
                     (require 'ess-sp6-d)
                     (S+)))
                  ((string= dialect "SAS")
                   (let ((inferior-SAS-program (or path name)))
                     (require 'ess-sas-d)
                     (SAS))))))))

(defun ess-version ()
  (interactive)
  (message (format "ess-version: %s (loaded from %s)"
                   (ess-version-string)
                   (file-name-directory ess-lisp-directory))))

(defun ess-version-string ()
  (let* ((ess-dir (file-name-directory ess-lisp-directory)) ; if(<from source>) the top-level 'ess/'
         (is-release (file-exists-p (concat ess-etc-directory ".IS.RELEASE")))
         (rel-string (if is-release "Released "))
         (git-ref-fn (concat ess-dir ".git/HEAD"))
         (git-ref (when (file-exists-p git-ref-fn)
                    (with-current-buffer (find-file-noselect git-ref-fn)
                      (goto-char (point-min))
                      (when (re-search-forward "ref: \\(.*\\)\n" nil t)
                        (match-string 1)))))
         (git-fname (if git-ref
                        (concat ess-dir ".git/" git-ref)
                      ;; For release
                      (concat ess-etc-directory "git-ref")))
         (git-rev (when (file-exists-p git-fname)
                    (with-current-buffer (find-file-noselect git-fname)
                      (goto-char (point-min))
                      (concat "git: "(buffer-substring 1 (point-at-eol))))))
         (elpa-fname (concat ess-dir "ess-pkg.el"))
         (elpa-rev (when (file-exists-p elpa-fname)
                     ;; Get it from ELPA dir name, (probably won't work if installed manually)
                     (concat "elpa: "
                             (replace-regexp-in-string "ess-" ""
                                                       (file-name-nondirectory
                                                        (substring ess-dir 1 -1)))))))
    ;; Set the "global" ess-revision:
    (setq ess-revision (format "%s%s%s"
                               (or rel-string "")
                               (or git-rev "")
                               (or elpa-rev "")))
    (when (string= ess-revision "")
      (setq ess-revision "<unknown>"))
    (concat ess-version " [" ess-revision "]")))


(provide 'ess)

;;; ess.el ends here
