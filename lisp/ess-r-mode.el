;;; ess-r-mode.el --- R customization

;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2017 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: A.J. Rossini
;; Created: 12 Jun 1997
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages, statistics

;; This file is part of ESS.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/

;;; Commentary:

;; This file defines all the R customizations for ESS.  See ess-s-lang.el
;; for general S language customizations.

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'cl-lib)
(require 'compile)
(require 'easymenu)
(require 'eldoc)
(require 'seq)
(require 'ess-mode)
(require 'ess-help)
(require 'ess-s-lang)
(require 'ess-roxy)
(require 'ess-r-completion)
(require 'ess-r-syntax)
(require 'ess-r-package)
(require 'ess-trns)
(require 'ess-r-xref)
(when (>= emacs-major-version 26) (require 'ess-r-flymake)) ; Flymake rewrite in Emacs 26

;; Silence the byte compiler
(defvar add-log-current-defun-header-regexp)

;; TODO: Refactor so as to not rely on dynamic scoping.  After that
;; refactor, also remove the file-local-variable byte-compile-warnings
;; (not lexical) at the bottom.
(defvar block)
(defvar containing-sexp)
(defvar indent-point)
(defvar infinite)
(defvar last-newline)
(defvar last-pos)
(defvar offset)
(defvar prefix-break)
(defvar prev-containing-sexp)
(defvar start)
(defvar start-pos)
(defvar style)
(defvar type)



;;*;; Mode definition

;;;*;;; UI (Keymaps / Menus)
;;;###autoload
(defvar ess-dev-map
  (let (ess-dev-map)
    (define-prefix-command 'ess-dev-map)
    (define-key ess-dev-map "\C-s" 'ess-r-set-evaluation-env)
    (define-key ess-dev-map "s" 'ess-r-set-evaluation-env)
    (define-key ess-dev-map "T" 'ess-toggle-tracebug)
    (define-key ess-dev-map "\C-l" 'ess-r-devtools-load-package)
    (define-key ess-dev-map "l" 'ess-r-devtools-load-package)
    (define-key ess-dev-map "`" 'ess-show-traceback)
    (define-key ess-dev-map "~" 'ess-show-call-stack)
    (define-key ess-dev-map "\C-w" 'ess-watch)
    (define-key ess-dev-map "w" 'ess-watch)
    (define-key ess-dev-map "\C-d" 'ess-debug-flag-for-debugging)
    (define-key ess-dev-map "d" 'ess-debug-flag-for-debugging)
    (define-key ess-dev-map "\C-u" 'ess-debug-unflag-for-debugging)
    (define-key ess-dev-map "u" 'ess-debug-unflag-for-debugging)
    (define-key ess-dev-map [(control ?D)] 'ess-debug-unflag-for-debugging)
    (define-key ess-dev-map "\C-b" 'ess-bp-set)
    (define-key ess-dev-map "b" 'ess-bp-set)
    (define-key ess-dev-map [(control ?B)] 'ess-bp-set-conditional)
    (define-key ess-dev-map "B" 'ess-bp-set-conditional)
    (define-key ess-dev-map "\C-L" 'ess-bp-set-logger)
    (define-key ess-dev-map "L" 'ess-bp-set-logger)
    (define-key ess-dev-map "\C-o" 'ess-bp-toggle-state)
    (define-key ess-dev-map "o" 'ess-bp-toggle-state)
    (define-key ess-dev-map "\C-k" 'ess-bp-kill)
    (define-key ess-dev-map "k" 'ess-bp-kill)
    (define-key ess-dev-map "\C-K" 'ess-bp-kill-all)
    (define-key ess-dev-map "K" 'ess-bp-kill-all)
    (define-key ess-dev-map "\C-n" 'ess-bp-next)
    (define-key ess-dev-map "n" 'ess-bp-next)
    (define-key ess-dev-map "i" 'ess-debug-goto-input-event-marker)
    (define-key ess-dev-map "I" 'ess-debug-goto-input-event-marker)
    (define-key ess-dev-map "\C-p" 'ess-bp-previous)
    (define-key ess-dev-map "p" 'ess-bp-previous)
    (define-key ess-dev-map "\C-e" 'ess-debug-toggle-error-action)
    (define-key ess-dev-map "e" 'ess-debug-toggle-error-action)
    (define-key ess-dev-map "0" 'ess-electric-selection)
    (define-key ess-dev-map "1" 'ess-electric-selection)
    (define-key ess-dev-map "2" 'ess-electric-selection)
    (define-key ess-dev-map "3" 'ess-electric-selection)
    (define-key ess-dev-map "4" 'ess-electric-selection)
    (define-key ess-dev-map "5" 'ess-electric-selection)
    (define-key ess-dev-map "6" 'ess-electric-selection)
    (define-key ess-dev-map "7" 'ess-electric-selection)
    (define-key ess-dev-map "8" 'ess-electric-selection)
    (define-key ess-dev-map "9" 'ess-electric-selection)
    (define-key ess-dev-map "?" 'ess-tracebug-show-help)
    ess-dev-map)
  "Keymap for commands related to development and debugging.")

(defvar ess-r-package-check-map
  (let (ess-r-package-check-map)
    (define-prefix-command 'ess-r-package-check-map)
    (define-key ess-r-package-check-map "\C-c" 'ess-r-devtools-check-package)
    (define-key ess-r-package-check-map "c" 'ess-r-devtools-check-package)
    (define-key ess-r-package-check-map "\C-w" 'ess-r-devtools-check-with-winbuilder)
    (define-key ess-r-package-check-map "w" 'ess-r-devtools-check-with-winbuilder)
    (define-key ess-r-package-check-map "h" 'ess-r-rhub-check-package)
    ess-r-package-check-map)
  "Keymap for R package checks.")

(defvar ess-r-package-dev-map
  (let (ess-r-package-dev-map)
    (define-prefix-command 'ess-r-package-dev-map)
    (define-key ess-r-package-dev-map "\C-s" 'ess-r-package-set-package)
    (define-key ess-r-package-dev-map "s"    'ess-r-package-set-package)
    (define-key ess-r-package-dev-map "\C-a" 'ess-r-devtools-ask)
    (define-key ess-r-package-dev-map "a"    'ess-r-devtools-ask)
    (define-key ess-r-package-dev-map "\C-e" 'ess-r-devtools-execute-command)
    (define-key ess-r-package-dev-map "e"    'ess-r-devtools-execute-command)
    (define-key ess-r-package-dev-map "\C-b" 'ess-r-devtools-build)
    (define-key ess-r-package-dev-map "b"    'ess-r-devtools-build)
    (define-key ess-r-package-dev-map "\C-c" 'ess-r-package-check-map)
    (define-key ess-r-package-dev-map "c" 'ess-r-package-check-map)
    (define-key ess-r-package-dev-map "\C-d" 'ess-r-devtools-document-package)
    (define-key ess-r-package-dev-map "d"    'ess-r-devtools-document-package)
    (define-key ess-r-package-dev-map "g"    'ess-r-devtools-install-github)
    (define-key ess-r-package-dev-map "\C-i" 'ess-r-devtools-install-package)
    (define-key ess-r-package-dev-map "i"    'ess-r-devtools-install-package)
    (define-key ess-r-package-dev-map "\C-l" 'ess-r-devtools-load-package)
    (define-key ess-r-package-dev-map "l"    'ess-r-devtools-load-package)
    (define-key ess-r-package-dev-map "\C-t" 'ess-r-devtools-test-package)
    (define-key ess-r-package-dev-map "t"    'ess-r-devtools-test-package)
    (define-key ess-r-package-dev-map "\C-u" 'ess-r-devtools-unload-package)
    (define-key ess-r-package-dev-map "u"    'ess-r-devtools-unload-package)
    ess-r-package-dev-map))

(easy-menu-define ess-roxygen-menu nil
  "Roxygen submenu."
  '("Roxygen"
    :visible (and ess-dialect (string-match "^R" ess-dialect))
    ["Update/Generate Template" ess-roxy-update-entry           t]
    ["Preview Rd"        ess-roxy-preview-Rd                    t]
    ["Preview HTML"      ess-roxy-preview-HTML                  t]
    ["Preview text"      ess-roxy-preview-text                  t]
    ["Hide all"          ess-roxy-hide-all                      t]
    ["Toggle Roxygen Prefix"     ess-roxy-toggle-roxy-region    t]))

(easy-menu-define ess-tracebug-menu nil
  "Tracebug submenu."
  '("Tracebug"
    :visible (and ess-dialect (string-match "^R" ess-dialect))
    ;; :enable ess-local-process-name
    ["Active?"  ess-toggle-tracebug
     :style toggle
     :selected (or (and (ess-process-live-p)
                        (ess-process-get 'tracebug))
                   ess-use-tracebug)]
    ["Show traceback" ess-show-traceback (ess-process-live-p)]
    ["Show call stack" ess-show-call-stack (ess-process-live-p)]
    ["Watch" ess-watch  (and (ess-process-live-p)
                             (ess-process-get 'tracebug))]
    ["Error action cycle" ess-debug-toggle-error-action (and (ess-process-live-p)
                                                             (ess-process-get 'tracebug))]
    "----"
    ["Flag for debugging" ess-debug-flag-for-debugging ess-local-process-name]
    ["Unflag for debugging" ess-debug-unflag-for-debugging ess-local-process-name]
    "----"
    ["Set BP" ess-bp-set t]
    ["Set conditional BP" ess-bp-set-conditional t]
    ["Set logger BP" ess-bp-set-logger t]
    ["Kill BP" ess-bp-kill t]
    ["Kill all BPs" ess-bp-kill-all t]
    ["Next BP" ess-bp-next t]
    ["Previous BP" ess-bp-previous t]
    "-----"
    ["About" ess-tracebug-show-help t]))

(easy-menu-define ess-r-package-menu nil
  "Package Development submenu."
  '("Package development"
    :visible (and ess-dialect (string-match "^R" ess-dialect))
    ["Active?" ess-r-package-mode
     :style toggle
     :selected ess-r-package-mode]
    ["Select package for evaluation" ess-r-set-evaluation-env t]))

(easy-menu-add-item ess-mode-menu nil ess-roxygen-menu "end-dev")
(easy-menu-add-item ess-mode-menu nil ess-r-package-menu "end-dev")
(easy-menu-add-item ess-mode-menu nil ess-tracebug-menu "end-dev")
(easy-menu-add-item inferior-ess-mode-menu nil ess-r-package-menu "end-dev")
(easy-menu-add-item inferior-ess-mode-menu nil ess-tracebug-menu "end-dev")

(defvar ess-r-mode-syntax-table
  (let ((table (copy-syntax-table S-syntax-table)))
    ;; Letting Emacs treat backquoted names and %ops% as strings solves
    ;; many problems with regard to nested strings and quotes
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?% "\"" table)
    ;; Underscore is valid in R symbols
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?$ "." table)
    table)
  "Syntax table for `ess-r-mode'.")


(defvar ess-r-completion-syntax-table
  (let ((table (copy-syntax-table ess-r-mode-syntax-table)))
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Syntax table used for completion and help symbol lookup.
It makes underscores and dots word constituent chars.")

(defvar ess-r-namespaced-load-verbose t
  "Whether to display information on namespaced loading.
When t, loading a file into a namespaced will output information
about which objects are exported and which stay hidden in the
namespace.")

(defun ess-r-font-lock-syntactic-face-function (state)
  (let ((string-end (save-excursion
                      (and (nth 3 state)
                           (ess-goto-char (nth 8 state))
                           (ess-forward-sexp)
                           (point)))))
    (when (eq (nth 3 state) ?`)
      (put-text-property (nth 8 state) string-end 'ess-r-backquoted t))
    (cond
     ((eq (nth 3 state) ?%)
      (if (eq (point) (1- string-end))
          (when (cdr (assq 'ess-fl-keyword:operators ess-R-font-lock-keywords))
            'ess-operator-face)
        (if (cdr (assq 'ess-R-fl-keyword:%op% ess-R-font-lock-keywords))
            'ess-%op%-face
          'default)))
     ((save-excursion
        (and (cdr (assq 'ess-R-fl-keyword:fun-defs ess-R-font-lock-keywords))
             (ess-goto-char string-end)
             (ess-looking-at "<-")
             (ess-goto-char (match-end 0))
             (ess-looking-at "function\\b" t)))
      font-lock-function-name-face)
     ((save-excursion
        (and (cdr (assq 'ess-fl-keyword:fun-calls ess-R-font-lock-keywords))
             (ess-goto-char string-end)
             (ess-looking-at "(")))
      ess-function-call-face)
     ((eq (nth 3 state) ?`)
      'default)
     ((nth 3 state)
      font-lock-string-face)
     (t
      font-lock-comment-face))))

(defvar ess-r--non-fn-kwds
  '("in" "else" "break" "next" "repeat"))

(defvar-local ess-r--keyword-regexp nil)
(defun ess-r--find-fl-keyword (limit)
  "Search for R keyword and set the match data.
To be used as part of `font-lock-defaults' keywords."
  (unless ess-r--keyword-regexp
    (let (fn-kwds non-fn-kwds)
      (dolist (kw ess-R-keywords)
        (if (member kw ess-r--non-fn-kwds)
            (push kw non-fn-kwds)
          (push kw fn-kwds)))
      (setq ess-r--keyword-regexp
            (concat "\\("
                    (regexp-opt non-fn-kwds 'words)
                    "\\)\\|\\("
                    (regexp-opt fn-kwds 'words)
                    "\\)"))))
  (let (out)
    (while (and (not out)
                (re-search-forward ess-r--keyword-regexp limit t))
      (save-match-data
        (setq out (if (match-beginning 1)
                      ;; Non-function-like keywords: Always fontified
                      ;; except for `in` for which we check it's part
                      ;; of a `for` construct. Ideally we'd check that
                      ;; other keywords like `break` or `next` are
                      ;; part of the right syntactic construct but
                      ;; that requires robust and efficient detection
                      ;; of complete expressions.
                      (if (string= (match-string 1) "in")
                          (save-excursion
                            (goto-char (match-beginning 1))
                            (and (ess-backward-up-list)
                                 (forward-word -1)
                                 (looking-at "for\\s-*(")))
                        t)
                    ;; Function-like keywords: check if they are
                    ;; followed by an open paren
                    (looking-at "\\s-*(")))))
    out))

(define-obsolete-variable-alias 'R-customize-alist 'ess-r-customize-alist "ESS 18.10.2")
(defvar ess-r-customize-alist
  (append
   '((ess-local-customize-alist             . 'ess-r-customize-alist)
     (ess-dialect                           . "R")
     (ess-suffix                            . "R")
     (ess-build-tags-command                . ess-r-build-tags-command)
     (ess-traceback-command                 . ess-r-traceback-command)
     (ess-call-stack-command                . ess-r-call-stack-command)
     (ess-mode-completion-syntax-table      . ess-r-completion-syntax-table)
     (ess-build-eval-message-function       . #'ess-r-build-eval-message)
     (ess-format-eval-command-function      . #'ess-r-format-eval-command)
     (ess-format-load-command-function      . #'ess-r-format-load-command)
     (ess-send-region-function              . #'ess-r-send-region)
     (ess-load-file-function                . #'ess-r-load-file)
     (ess-make-source-refd-command-function . #'ess-r-make-source-refd-command)
     (ess-format-eval-message-function      . #'ess-r-format-eval-message)
     (ess-help-web-search-command           . #'ess-r-sos)
     (ess-build-help-command-function       . #'ess-r-build-help-command)
     (ess-dump-filename-template            . ess-r-dump-filename-template)
     (ess-change-sp-regexp                  . ess-r-change-sp-regexp)
     (ess-help-sec-regex                    . ess-help-r-sec-regex)
     (ess-help-sec-keys-alist               . ess-help-r-sec-keys-alist)
     (ess-function-pattern                  . ess-r-function-pattern)
     (ess-object-name-db-file               . "ess-r-namedb.el")
     (ess-smart-operators                   . ess-r-smart-operators)
     (inferior-ess-program                  . inferior-ess-r-program)
     (inferior-ess-objects-command          . inferior-ess-r-objects-command)
     (inferior-ess-search-list-command      . "search()\n")
     (inferior-ess-help-command             . inferior-ess-r-help-command)
     (inferior-ess-help-filetype            . nil)
     (inferior-ess-exit-command             . "q()")
     (inferior-ess-exit-prompt              . "Save workspace image? [y/n/c]: ")
     (inferior-ess-start-file               . nil)
     (inferior-ess-start-args               . "")
     (ess-error-regexp-alist                . ess-r-error-regexp-alist)
     (ess-describe-object-at-point-commands . 'ess-r-describe-object-at-point-commands)
     (ess-STERM                             . "iESS")
     (ess-editor                            . ess-r-editor)
     (ess-pager                             . ess-r-pager))
   S-common-cust-alist)
  "Variables to customize for R.")

;; VS[24-08-2018]: FIXME: make initialization of custom-alist tail-to-top and
;; put this into the above alist
(setcdr (assoc 'ess-font-lock-keywords ess-r-customize-alist)
        (quote 'ess-R-font-lock-keywords))
(setcdr (assoc 'inferior-ess-font-lock-keywords ess-r-customize-alist)
        (quote 'inferior-ess-r-font-lock-keywords))

(defvar ess-r-build-tags-command
  "rtags('%s', recursive = TRUE, pattern = '\\\\.[RrSs](rw)?$',ofile = '%s')")

(defvar ess-r-traceback-command
  "local({cat(geterrmessage(), \
'---------------------------------- \n', \
fill=TRUE); try(traceback(), silent=TRUE)})\n")

(defvar ess-r-call-stack-command "traceback(1)\n")

(defvar ess-r-dump-filename-template
  (replace-regexp-in-string
   "S$" "R" ess-dump-filename-template-proto))

(defvar ess-r-ac-sources
  '(ac-source-R))

(defvar ess-r-company-backends
  '((company-R-library company-R-args company-R-objects :separate)))

(defconst ess-help-r-sec-regex "^[A-Z][A-Za-z].+:$"
  "Reg(ular) Ex(pression) of section headers in help file.")

(defconst ess-help-r-sec-keys-alist
  '((?a . "\\s *Arguments:")
    (?d . "\\s *Description:")
    (?D . "\\s *Details:")
    (?t . "\\s *Details:")
    (?e . "\\s *Examples:")
    (?n . "\\s *Note:")
    (?r . "\\s *References:")
    (?s . "\\s *See Also:")
    (?u . "\\s *Usage:")
    (?v . "\\s *Value[s]?")     ;
    )
  "Alist of (key . string) pairs for use in help section searching.")

(defvar ess-r-error-regexp-alist '(R R1 R2 R3 R4 R-recover)
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

;; Takes precidence over R1 below in english locales, and allows spaces in file path
(add-to-list 'compilation-error-regexp-alist-alist
             '(R "\\(\\(?: at \\|(@\\)\\([^#()\n]+\\)[#:]\\([0-9]+\\)\\)"  2 3 nil 2 1))

(add-to-list 'compilation-error-regexp-alist-alist
             ;; valgrind style (stl_numeric.h:183)
             '(R1 "(\\([^ ):\n]+\\):\\([0-9]+\\)?)" 1 2 nil 2))

(add-to-list 'compilation-error-regexp-alist-alist
             '(R2 "(\\(\\w+ \\([^())\n]+\\)#\\([0-9]+\\)\\))"  2 3 nil 2 1))

;; Precedes R4 and allows spaces in file path
(add-to-list 'compilation-error-regexp-alist-alist
             ;; Starts at bol or with ": " (patterns 3,4,5,6,9)
             '(R3 "\\(?:^ *\\|: ?\\)\\([^-+[:digit:] \t\n]:?[^: \t\n]*\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)?"  1 2 3 2 1))

(add-to-list 'compilation-error-regexp-alist-alist
             ;; Don't start with digit; no spaces
             '(R4 "\\([^-+ [:digit:]][^: \t\n]+\\):\\([0-9]+\\):\\([0-9]+\\):"  1 2 3 2 1))

(add-to-list 'compilation-error-regexp-alist-alist
             '(R-recover " *[0-9]+: +\\([^:\n\t]+?\\)#\\([0-9]+:\\)"  1 2 nil 2 1))

;; gnu C errors
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(R_C "^\\([^-+ [:digit:]][^: \t\n]+\\):\\([0-9]+\\):\\([0-9]+\\):"  2 3 nil 2 1))


(defvar ess-r-versions
  (let ((r-ver '("R-1" "R-2" "R-3" "R-devel" "R-patched")))
    (if (eq system-type 'darwin) (append r-ver '("R32" "R64")) r-ver))
  "List of partial strings for versions of R to access within ESS.
Each string specifies the start of a filename.  If a filename
beginning with one of these strings is found on `exec-path', a
command for that version of R is made available.  For example, if the
file \"R-1.8.1\" is found and this variable includes the string
\"R-1\", a function called `M-x R-1.8.1' will be available to run that
version of R.
If duplicate versions of the same program are found (which happens if
the same path is listed on `exec-path' more than once), they are
ignored by calling `delete-dups'.
Set this variable to nil to disable searching for other versions of R.
If you set this variable, you need to restart Emacs (and set this variable
before ess-site is loaded) for it to take effect.")

(define-obsolete-variable-alias 'ess-r-versions-created 'ess-r-created-runners "ESS 18.10")
(defvar ess-r-created-runners nil
  "List of R-versions found from `ess-r-versions' on the system.")


;;;*;;; Mode init

(define-obsolete-variable-alias 'ess-R-post-run-hook 'ess-r-post-run-hook "ESS 18.10.2")
(defvar ess-r-post-run-hook nil
  "Functions run in process buffer after the initialization of R process.")

;;;###autoload
(defun run-ess-r (&optional start-args)
  "Call 'R', the 'GNU S' system from the R Foundation.
Optional prefix (\\[universal-argument]) allows to set command line arguments, such as
--vsize.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to R, put them in the variable `inferior-R-args'.

START-ARGS can be a string representing an argument, a list of
such strings, or any other non-nil value.  In the latter case, you
will be prompted to enter arguments interactively."
  (interactive "P")
  (ess-write-to-dribble-buffer   ;; for debugging only
   (format
    "\n(R): ess-dialect=%s, buf=%s, start-arg=%s\n current-prefix-arg=%s\n"
    ess-dialect (current-buffer) start-args current-prefix-arg))
  (let* ((r-always-arg
          (if (or ess-microsoft-p (eq system-type 'cygwin))
              "--ess "
            ;; else: "unix alike"
            (if (not ess-R-readline) "--no-readline ")))
         (start-args
          (cond ((stringp start-args)
                 start-args)
                ((and start-args
                      (listp start-args)
                      (cl-every 'stringp start-args))
                 (mapconcat 'identity start-args " "))
                (start-args
                 (read-string
                  (concat "Starting Args"
                          (if r-always-arg
                              (concat " [other than '" r-always-arg "']"))
                          " ? ")))))
         (r-start-args
          (concat r-always-arg
                  inferior-R-args " "   ; add space just in case
                  start-args))
         (cust-alist (copy-alist ess-r-customize-alist))
         (gdbp (string-match-p "gdb" r-start-args))
         use-dialog-box)

    (when gdbp
      (setcdr (assoc 'inferior-ess-secondary-prompt cust-alist)
              (format "\\(%s\\)\\|\\((gdb) \\)"
                      (cdr (assoc 'inferior-ess-secondary-prompt cust-alist)))))

    (when (or ess-microsoft-p
              (eq system-type 'cygwin))
      (setq use-dialog-box nil)
      (when ess-microsoft-p ;; default-process-coding-system would break UTF locales on Unix
        (setq default-process-coding-system '(undecided-dos . undecided-dos))))

    (inferior-ess r-start-args cust-alist gdbp)

    (ess-process-put 'funargs-pre-cache ess-r--funargs-pre-cache)

    (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
    (add-hook 'completion-at-point-functions 'ess-r-object-completion nil 'local)
    (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
    (add-hook 'xref-backend-functions #'ess-r-xref-backend nil 'local)
    (setq comint-input-sender 'inferior-ess-r-input-sender)

    (if gdbp
        (progn
          ;; We need to use callback, because R might start with a gdb process
          (ess-process-put 'callbacks '(R-initialize-on-start))
          ;; trigger the callback
          (process-send-string (get-process ess-local-process-name) "\n"))
      (ess-wait-for-process)
      (R-initialize-on-start)
      (comint-goto-process-mark))

    (ess-write-to-dribble-buffer
     (format "(R): inferior-ess-language-start=%s\n"
             inferior-ess-language-start))))

;;;###autoload
(defalias 'R #'run-ess-r)

(defun inferior-ess-r--adjust-startup-directory (dir dialect)
  "Adjust startup directory DIR if DIALECT is R.
If in a package project, prefer the tests directory but only if
the package directory was selected in the first place."
  (if (string= dialect "R")
      (let* ((project-dir (cdr (ess-r-package-project)))
             (tests-dir (expand-file-name (file-name-as-directory "tests")
                                          project-dir)))
        (if (and project-dir
                 (string= project-dir dir)
                 (string= default-directory tests-dir))
            tests-dir
          dir))
    dir))

(defun R-initialize-on-start (&optional proc string)
  "This function is run after the first R prompt.
Executed in process buffer."
  (interactive)
  ;; sometimes needed (MM w/ Emacs 25.1, on F24 where PAGER is 'more'):
  ;; carefully set "pager" option  "when needed":
  (let ((pager-cmd (format
                    "if(identical(getOption('pager'),
                                  file.path(R.home(), 'bin', 'pager')))
                        options(pager='%s')\n"
                    inferior-ess-pager)))
    (ess-command pager-cmd))
  (inferior-ess-r-load-ESSR)
  (when inferior-ess-language-start
    (ess-command (concat inferior-ess-language-start "\n")))
  ;; tracebug
  (when ess-use-tracebug (ess-tracebug 1))
  (with-ess-process-buffer nil
    (add-hook 'ess-presend-filter-functions 'ess-R-scan-for-library-call nil 'local)
    (run-mode-hooks 'ess-r-post-run-hook)))

;;;###autoload
(define-derived-mode ess-r-mode ess-mode "ESS[R]"
  "Major mode for editing R source.  See `ess-mode' for more help."
  (ess-setq-vars-local ess-r-customize-alist)
  (setq-local paragraph-start (concat "\\s-*$\\|" page-delimiter))
  (setq-local paragraph-separate (concat "\\s-*$\\|" page-delimiter))
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local indent-line-function  'ess-indent-line)
  (setq-local ess-style ess-default-style)
  (setq-local add-log-current-defun-header-regexp "^\\(.+\\)\\s-+<-[ \t\n]*function")
  (setq-local font-lock-syntactic-face-function #'ess-r-font-lock-syntactic-face-function)
  ;; eldoc
  (add-function :before-until (local 'eldoc-documentation-function)
                #'ess-r-eldoc-function)
  (when ess-use-eldoc (eldoc-mode))
  ;; auto-complete
  (ess--setup-auto-complete ess-r-ac-sources)
  ;; company
  (ess--setup-company ess-r-company-backends)
  (setq-local prettify-symbols-alist ess-r-prettify-symbols)
  (setq font-lock-defaults '(ess-build-font-lock-keywords nil nil ((?\. . "w") (?\_ . "w"))))
  (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  (add-hook 'completion-at-point-functions 'ess-r-object-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
  (add-hook 'xref-backend-functions #'ess-r-xref-backend nil 'local)

  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  (when ess-imenu-use-S
    (setq imenu-generic-expression ess-imenu-S-generic-expression)
    (imenu-add-to-menubar "Imenu-R"))

  ;; useful for swankr/slime:
  (setq-local beginning-of-defun-function
              (lambda (&optional arg)
                (skip-chars-backward " \t\n")
                (ess-beginning-of-function 'no-error)))
  (setq-local end-of-defun-function
              'ess-end-of-function)
  (ess-roxy-mode))
;;;###autoload
(defalias 'R-mode 'ess-r-mode)
;;;###autoload
(defalias 'r-mode 'ess-r-mode)


;;;###autoload
(add-to-list 'auto-mode-alist '("/R/.*\\.q\\'" . ess-r-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . ess-r-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[rR]profile\\'" . ess-r-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("NAMESPACE\\'" . ess-r-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("CITATION\\'" . ess-r-mode))


;;*;; Miscellaneous

(defun ess-R-arch-2-bit (arch)
  "Translate R's architecture shortcuts/directory names to 'bits'.
ARCH \"32\" or \"64\" (for now)."
  (if (string= arch "i386")  "32"
    ;; else:
    "64"))

(defun ess-rterm-arch-version (long-path &optional give-cons)
  "Find a name for LONG-PATH, an absolute path to R on Windows.
Returns either Name, a string, or a (Name . Path) cons, such as

(\"R-2.12.1-64bit\"  .  \"C:/Program Files/R/R-2.12.1/bin/x64/Rterm.exe\")

\"R-x.y.z/bin/Rterm.exe\" will return \"R-x.y.z\", for R-2.11.x and older.
\"R-x.y.z/bin/i386/Rterm.exe\" will return \"R-x.y.z-32bit\", for R-2.12.x and newer.
\"R-x.y.z/bin/x64/Rterm.exe\"  will return \"R-x.y.z-64bit\", for R-2.12.x and newer."
  (let* ((dir  (directory-file-name (file-name-directory long-path)))
         (dir2 (directory-file-name (file-name-directory dir)))
         (v-1up (file-name-nondirectory dir));; one level up
         (v-2up (file-name-nondirectory dir2));; two levels up; don't want "bin" ...
         (v-3up (file-name-nondirectory ;; three levels up; no "bin" for i386, x64 ...
                 (directory-file-name (file-name-directory dir2))))
         (val (if (string= v-2up "bin")
                  (concat v-3up "-" (ess-R-arch-2-bit v-1up) "bit")
                ;; pre R-2.12.x, or when there's no extra arch-specific sub directory:
                v-2up)))
    (if give-cons
        (cons val long-path)
      val)))

(defun ess-r-define-runners ()
  "Generate functions for starting other versions of R.
See `ess-r-versions' for strings that determine which functions
are created.  On MS Windows, this works using
`ess-rterm-version-paths' instead.

The functions will normally be placed on the menubar and stored
as `ess-r-created-runners' upon ESS initialization."
  (when ess-r-versions
    ;; try to find R versions in ess-r-versions.
    (let ((versions
           ;; Find which versions of R we want.  Remove the pathname, leaving just
           ;; the name of the executable.
           (if ess-microsoft-p
               (mapcar (lambda (v) (car (ess-rterm-arch-version v 'give-cons)))
                       ess-rterm-version-paths)
             (delete-dups
              (mapcar #'file-name-nondirectory
                      (apply #'nconc
                             (mapcar #'ess-find-exec-completions
                                     ess-r-versions)))))))
      ;; Iterate over each string in VERSIONS, creating a new defun each time.
      (setq ess-r-created-runners versions)
      (if ess-microsoft-p
          (cl-mapcar (lambda (v p) (ess-define-runner v "R" p)) versions ess-rterm-version-paths)
        (mapc (lambda (v) (ess-define-runner v "R")) versions))
      ;; Add to menu
      (when ess-r-created-runners
        ;; new-menu will be a list of 3-vectors, of the form:
        ;; ["R-1.8.1" R-1.8.1 t]
        (let ((new-menu (mapcar (lambda(x) (vector x (intern x) t))
                                ess-r-created-runners)))
          (easy-menu-add-item ess-mode-menu '("Start Process")
                              (cons "Other" new-menu)))))))
(define-obsolete-function-alias
  'ess-r-versions-create 'ess-r-define-runners "ESS 18.10")

(defvar ess-newest-R nil
  "Stores the newest version of R that has been found.
Used as a cache, within `ess-find-newest-R'. Do not use this value
directly, but instead call the function \\[ess-find-newest-R].")


(defcustom ess-prefer-higher-bit t
  "Non-nil means prefer higher bit architectures of R.
e.g. prefer 64 bit over 32 bit.  This is currently used only
by the code on Windows for finding the newest version of R."
  :group 'ess-R
  :type 'boolean)

(defun ess-rterm-prefer-higher-bit ()
  "Optionally remove 32bit Rterms from being candidate for `R-newest'.
Return the list of candidates for being `R-newest'. Filtering is
done iff `ess-prefer-higher-bit' is non-nil. This is used only by
Windows when running `ess-find-newest-R'."
  (if ess-prefer-higher-bit
      ;; filter out 32 bit elements
      (let ((filtered
             (delq nil
                   (mapcar (lambda (x) (unless (string-match "/i386/Rterm.exe" x) x))
                           ess-rterm-version-paths))))
        (if (null filtered)
            ;; if none survived filtering, keep the original list
            ess-rterm-version-paths
          filtered))
    ess-rterm-version-paths))


(defun ess-find-newest-R ()
  "Find the newest version of R on the system.
Once the value is found, cache it in the variable `ess-newest-R'
for future use as finding the newest version of R can be
potentially time-consuming."
  (or ess-newest-R
      (progn (message "Finding all versions of R on your system...")
             ;;(sleep-for 3)
             nil)
      (setq ess-newest-R
            (ess-newest-r
             (if ess-microsoft-p
                 (ess-rterm-prefer-higher-bit)
               (add-to-list 'ess-r-created-runners
                            inferior-ess-r-program))))))

(defun ess-check-R-program ()
  "Check if `inferior-ess-r-program' points to an executable version of R.
If not, try to find the newest version of R elsewhere on the system, and
update `inferior-ess-r-program' accordingly."
  (unless (executable-find inferior-ess-r-program)
    ;; need to check if we can find another name.
    (let ((newest (ess-find-newest-R)))
      (if newest
          (setq inferior-ess-r-program newest)
        (message "Sorry, no version of R could be found on your system.")))))

(defun R-newest (&optional start-args)
  "Find the newest version of R available, and run it.
Subsequent calls to `R-newest' will run that version, rather than searching
again for the newest version.  Providing an optional prefix START-ARGS (\\[universal-argument]) will
prompt for command line arguments."
  (interactive "P")
  (let ((rnewest (ess-find-newest-R)))
    (if (not rnewest)
        (error "No version of R could be found")
      ;; Else: we have a working version of R.
      ;; Have to be careful to avoid recursion...
      (message "%s" (concat "Newest version of R is " rnewest))
      (fset 'R-newest
            (intern
             (if ess-microsoft-p
                 (ess-rterm-arch-version rnewest)
               rnewest)))
      ;;(fset 'R-newest (intern rnewest))
      (R-newest start-args))))

;; (ess-r-version-date "R-2.5.1") (ess-r-version-date "R-patched")
;; (ess-r-version-date "R-1.2.1") (ess-r-version-date "R-1.8.1")
;; Windows:
;;  (ess-r-version-date "C:/Program Files (x86)/R/R-2.11.1/bin/Rterm.exe")
;; Note that for R-devel, ver-string is something like
;; R version 2.6.0 Under development (unstable) (2007-07-14 r42234)
;; Antique examples are 'R 1.0.1  (April 14, 2000)' or 'R 1.5.1 (2002-06-17).'
(defun ess-r-version-date (rver)
  "Return the date of the version of R named RVER.
The date is returned as a date string.  If the version of R could
not be found from the output of the RVER program, \"-1\" is
returned."
  (let ((date "-1")
        (ver-string (shell-command-to-string
                     ;; here, MS Windows (shell-command) needs a short name:
                     (concat (if (and ess-microsoft-p
                                      ;; silence byte compiler warns about w32-fns
                                      (fboundp 'w32-short-file-name))
                                 (w32-short-file-name rver)
                               rver)
                             " --version"))))
    (when (string-match
           "R \\(version \\)?[1-9][^\n]+ (\\(2[0-9-]+\\)\\( r[0-9]+\\)?)"
           ver-string)
      (setq date (match-string 2 ver-string)))
    (cons date rver)))

(defun ess-current-R-version ()
  "Get the version of R currently running in the ESS buffer as a string."
  (ess-make-buffer-current)
  (car (ess-get-words-from-vector "as.character(.ess.Rversion)\n")))

(defun ess-current-R-at-least (version)
  "Is the version of R (in the ESS buffer) at least (\">=\") VERSION ?
Examples: (ess-current-R-at-least '2.7.0)
      or  (ess-current-R-at-least \"2.5.1\")"
  (ess-make-buffer-current)
  (string= "TRUE"
           (car (ess-get-words-from-vector
                 (format "as.character(.ess.Rversion >= \"%s\")\n" version)))))

(defvar ess-temp-newest nil)

(defun ess-newest-r (rvers)
  "Check all the versions of RVERS to see which is the newest.
Return the name of the newest version of R."
  (let ((rtimes (mapcar 'ess-r-version-date rvers)))
    ;; SJE: 2007-07-13 -- following line is a temp var to check that
    ;; the newest version of R is found correctly.
    ;; (nowadays gives a compile warning)
    (setq ess-temp-newest rtimes)
    (ess-find-newest-date rtimes)))

(defun ess-find-newest-date (rvers)
  "Find the newest version of R given in the a-list RVERS.
Each element of RVERS is a dotted pair (date . R-version), where
date is given as e.g.\"2007-11-30\" so that we can compare dates
as strings.  If a date is listed as \"-1\", that version of R
could not be found.

If the value returned is nil, no valid newest version of R could be found."
  (let (new-r this-r
              (new-time "0"))
    (while rvers
      (setq this-r (car rvers)
            rvers (cdr rvers))
      (when (string< new-time (car this-r))
        (setq new-time (car this-r)
              new-r    (cdr this-r))))
    new-r))

(defun ess-find-rterm (&optional ess-R-root-dir bin-Rterm-exe)
  "Find the full path of all occurences of Rterm.exe under the ESS-R-ROOT-DIR.
If ESS-R-ROOT-DIR is nil, construct it by looking for an
occurence of Rterm.exe in the `exec-path'. If there are no
occurences of Rterm.exe in the `exec-path', then use
`ess-program-files' (which evaluates to something like
\"c:/progra~1/R/\" in English locales) which is the default
location for the R distribution. If BIN-RTERM-EXE is nil, then
use \"bin/Rterm.exe\"."
  (if (not ess-R-root-dir)
      (let ((Rpath (executable-find "Rterm")))
        (setq ess-R-root-dir
              (expand-file-name
               (if Rpath
                   (concat (file-name-directory Rpath) "../../")
                 (concat ess-program-files "/R/"))))
        (ess-write-to-dribble-buffer
         (format "(ess-find-rterm): ess-R-root-dir = '%s'\n" ess-R-root-dir))))

  (if (not bin-Rterm-exe) (setq bin-Rterm-exe "bin/Rterm.exe"))

  (when (file-directory-p ess-R-root-dir) ; otherwise file-name-all-.. errors
    (setq ess-R-root-dir
          (replace-regexp-in-string "[\\]" "/" ess-R-root-dir))
    (let ((R-ver
           (ess-drop-non-directories
            (ess-flatten-list
             (mapcar (lambda (r-prefix)
                       (file-name-all-completions r-prefix ess-R-root-dir))
                     (append '("rw") ess-r-versions))))))
      (mapcar (lambda (dir)
                (let ((R-path
                       (concat ess-R-root-dir
                               (replace-regexp-in-string "[\\]" "/" dir)
                               bin-Rterm-exe)))
                  (if (file-exists-p R-path) R-path)))
              R-ver))))

;;;###autoload
(defun Rnw-mode ()
  "Major mode for editing Sweave(R) source.
See `ess-noweb-mode' and `ess-r-mode' for more help."
  (interactive)
  (require 'ess-noweb);; << probably someplace else
  (setq ess--make-local-vars-permanent t)
  (ess-noweb-mode 1); turn it on
  (ess-noweb-set-doc-mode 'latex-mode)
  (ess-noweb-set-code-mode 'ess-r-mode)
  (setq ess--local-handy-commands
        (append '(("weave"      . ess-swv-weave)
                  ("tangle"     . ess-swv-tangle))
                ess-handy-commands)
        ess-dialect "R"
        ess-language "S")
  (put 'ess--local-handy-commands 'permanent-local t)
  (run-mode-hooks 'Rnw-mode-hook))

(fset 'Snw-mode 'Rnw-mode); just a synonym (for now or ever)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . Rnw-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[sS]nw\\'" . Snw-mode))

;;;###autoload
(define-derived-mode ess-r-transcript-mode ess-transcript-mode "ESS R Transcript"
  "A Major mode for R transcript files."
  :syntax-table ess-r-mode-syntax-table
  (ess-setq-vars-local ess-r-customize-alist)
  (setq-local paragraph-start (concat "\\s-*$\\|" page-delimiter))
  (setq-local paragraph-separate (concat "\\s-*$\\|" page-delimiter))
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local indent-line-function  'ess-indent-line)
  (setq-local ess-style ess-default-style)
  (setq-local add-log-current-defun-header-regexp "^\\(.+\\)\\s-+<-[ \t\n]*function")
  (setq-local font-lock-syntactic-face-function #'ess-r-font-lock-syntactic-face-function)
  (setq-local prettify-symbols-alist ess-r-prettify-symbols)
  (setq inferior-ess-font-lock-keywords 'inferior-ess-r-font-lock-keywords)
  (setq font-lock-defaults '(ess-build-font-lock-keywords
                             nil nil ((?\. . "w") (?\_ . "w") (?' . ".")))))

(fset 'r-transcript-mode 'ess-r-transcript-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Rr]out" . ess-r-transcript-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("Rscript" . ess-r-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("r" . ess-r-mode))

(defun ess-r-fix-T-F (&optional from quietly)
  "Change T/F into TRUE and FALSE cautiously.
Do not change in comments and strings. Start at FROM, which
defaults to point, and change to end of buffer. When QUIETLY, do
not issue messages."
  (interactive "d\nP"); point and prefix (C-u)
  (save-excursion
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\) *\\)T\\>" "\\1TRUE"
                    'fixcase nil (not quietly))
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\) *\\)F\\>" "\\1FALSE"
                    'fixcase nil (not quietly))))
(define-obsolete-function-alias 'R-fix-T-F 'ess-r-fix-T-F
  "ESS 18.10")

(defvar ess--packages-cache nil
  "Cache var to store package names.
Used by `ess-r-install-library'.")

(defvar ess--CRAN-mirror nil
  "CRAN mirror name cache.")

(cl-defmethod ess-install-library--override
  (update package &context ((string= ess-dialect "R") (eql t)))
  "Prompt and install R PACKAGE.
With argument UPDATE, update cached packages list."
  (inferior-ess-r-force)
  (when (equal "@CRAN@" (car (ess-get-words-from-vector "getOption('repos')[['CRAN']]\n")))
    (ess-set-CRAN-mirror ess--CRAN-mirror)
    (ess-wait-for-process (get-process ess-current-process-name))
    (unless package (setq update t)))
  (when (or update
            (not ess--packages-cache))
    (message "Fetching R packages ... ")
    (setq ess--packages-cache
          (ess-get-words-from-vector "print(rownames(available.packages()), max=1e6)\n")))
  (let* ((ess-eval-visibly-p t)
         (package (or package
                      (ess-completing-read "Package to install" ess--packages-cache))))
    (process-send-string (get-process ess-current-process-name)
                         (format "install.packages('%s')\n" package))
    (display-buffer (buffer-name (process-buffer (get-process ess-current-process-name))))))

(defun ess-setRepositories ()
  "Call setRepositories()."
  (interactive)
  (if (not (string-match "^R" ess-dialect))
      (message "Sorry, not available for %s" ess-dialect)
    (ess-eval-linewise "setRepositories(FALSE)\n")))

(defun ess-set-CRAN-mirror (&optional mirror)
  "Set cran MIRROR."
  (interactive)
  (let ((mirror-cmd "local({r <- getOption('repos'); r['CRAN'] <- '%s';options(repos=r)})\n"))
    (if mirror
        (ess-command (format mirror-cmd mirror))
      (when-let ((M1 (ess-get-words-from-vector "local({out <- getCRANmirrors(local.only=TRUE); print(paste(out$Name,'[',out$URL,']', sep=''))})\n"))
                 (mirror (ess-completing-read "Choose CRAN mirror" M1 nil t))
                 (url (seq-contains M1 mirror #'string=)))
        (setq ess--CRAN-mirror (progn (string-match "\\(.*\\)\\[\\(.*\\)\\]$" url)
                                      (match-string 2 url)))
        (ess-command (format mirror-cmd ess--CRAN-mirror)))))
  (message "CRAN mirror: %s" (car (ess-get-words-from-vector "getOption('repos')[['CRAN']]\n"))))
(define-obsolete-function-alias 'ess-setCRANMiror 'ess-set-CRAN-mirror "ESS 18.10")

(defun ess-r-check-install-package (pkg)
  "Check if package PKG is installed and offer to install if not."
  (unless (ess-boolean-command (format "print(requireNamespace('%s', quietly = TRUE))\n" pkg))
    (if (y-or-n-p (format "Package '%s' is not installed. Install? " pkg))
        (ess-eval-linewise (format "install.packages('%s')\n" pkg))
      (signal 'quit nil))))

(defun ess-r-sos (cmd)
  "Interface to findFn in the library sos."
  (interactive  "sfindFn: ")
  (ess-r-check-install-package "sos")
  (ess-eval-linewise (format "sos::findFn(\"%s\", maxPages=10)" cmd)))

(defun ess-R-scan-for-library-call (string)
  "Detect `library/require' call in STRING and update tracking vars.
Placed into `ess-presend-filter-functions' for R dialects."
  (when (string-match-p "\\blibrary(\\|\\brequire(" string)
    (ess--mark-search-list-as-changed))
  string)

(defun ess-load-library ()
  "Prompt and load dialect specific library/package/module.

Note that add-ons in R are called 'packages' and the name of this
function has nothing to do with R package mechanism, but it
rather serves a generic, dialect independent purpose. It is also
similar to `load-library' Emacs function."
  (interactive)
  (if (not (string-match "^R" ess-dialect))
      (message "Sorry, not available for %s" ess-dialect)
    (let ((ess-eval-visibly-p t)
;;; FIXME? .packages() does not cache; installed.packages() does but is slower first time
          (packs (ess-get-words-from-vector "print(.packages(T), max=1e6)\n"))
          pack)
      (setq pack (ess-completing-read "Load" packs))
      (ess-eval-linewise (format "library('%s')\n" pack))
      (ess--mark-search-list-as-changed)
      (display-buffer (buffer-name (process-buffer (get-process ess-current-process-name)))))))

(define-obsolete-function-alias 'ess-library 'ess-load-library "ESS[12.09-1]")

;;; smart-comma was a bad idea
(eval-after-load "eldoc"
  '(eldoc-add-command "ess-smart-comma"))


;;*;; Interaction with R

;;;*;;; Evaluation

(defun ess-r-arg (param value &optional wrap)
  (let ((value (if wrap
                   (concat "'" value "'")
                 value)))
    (concat ", " param " = " value)))

(defun ess-r-build-args (visibly output namespace)
  (let ((visibly (ess-r-arg "visibly" (if visibly "TRUE" "FALSE")))
        (output (ess-r-arg "output" (if output "TRUE" "FALSE")))
        (pkg (when namespace (ess-r-arg "package" namespace t)))
        (verbose (when (and namespace
                            ess-r-namespaced-load-verbose)
                   (ess-r-arg "verbose" "TRUE"))))
    (concat visibly output pkg verbose)))

(cl-defmethod ess-build-eval-command--override
  (string &context ((string= ess-dialect "R") (eql t))
          &optional visibly output file &rest args)
  "R method to build eval command."
  (let* ((namespace (caar args))
         (namespace (unless ess-debug-minor-mode
                      (or namespace (ess-r-get-evaluation-env))))
         (cmd (if namespace ".ess.ns_eval" ".ess.eval"))
         (file (when file (ess-r-arg "file" file t)))
         (rargs (ess-r-build-args visibly output namespace)))
    (concat cmd "(\"" string "\"" rargs file ")\n")))

(cl-defmethod ess-build-load-command (string &context ((string= ess-dialect "R") (eql t))
                                             &optional visibly output file &rest args)
  (let* ((namespace (or file (ess-r-get-evaluation-env)))
         (cmd (if namespace ".ess.ns_source" ".ess.source"))
         (rargs (ess-r-build-args visibly output namespace)))
    (concat cmd "('" string "'" rargs ")\n")))

(defun ess-r-build-eval-message (message)
  (let ((env (cond (ess-debug-minor-mode
                    (substring-no-properties ess-debug-indicator 1))
                   ((ess-r-get-evaluation-env)))))
    (if env
        (format "[%s] %s" env message)
      message)))

(defvar-local ess-r-evaluation-env nil
  "Environment into which code should be evaluated.
When this variable is nil, code is evaluated in the current
environment. Currently only packages can be set as evaluation
environments. Use `ess-r-set-evaluation-env' to set this
variable.")

(defun ess-r-get-evaluation-env ()
  "Get current evaluation env."
  (or ess-r-evaluation-env
      (and ess-current-process-name
           (ess-get-process-variable 'ess-r-evaluation-env))))

(defun ess-r-set-evaluation-env (&optional arg)
  "Select a package namespace for evaluation of R code.

Call interactively with a prefix argument to disable evaluation
in a namespace.  When calling from a function, ARG can be a
string giving the package to select, any other non-nil value to
disable, or nil to prompt for a package.

If `ess-r-prompt-for-attached-pkgs-only' is non-nil, prompt only for
attached packages."
  (interactive "P")
  (let ((env (cond ((stringp arg) arg)
                   ((null arg) (ess-r--select-package-name))
                   (t "*none*"))))
    (if (equal env "*none*")
        (let ((cur-env (ess-r-get-evaluation-env)))
          ;; fixme: does not work if env is set at process level
          (setq ess-r-evaluation-env nil)
          (delq 'ess-r--evaluation-env-mode-line ess--local-mode-line-process-indicator)
          (message (format "Evaluation in %s disabled" (propertize cur-env 'face font-lock-function-name-face))))
      (setq ess-r-evaluation-env env)
      (add-to-list 'ess--local-mode-line-process-indicator 'ess-r--evaluation-env-mode-line t)
      (message (format "Evaluating in %s" (propertize env 'face font-lock-function-name-face))))
    (force-mode-line-update)))

(defvar-local ess-r--evaluation-env-mode-line
  '(:eval (let ((env (ess-r-get-evaluation-env)))
            (if env
                (format " %s"
                        (propertize  (if (equal env (ess-r-package-name))
                                         "pkg"
                                       env)
                                     'face 'mode-line-emphasis))
              ""))))
(put 'ess-r--evaluation-env-mode-line 'risky-local-variable t)

(defvar ess-r-namespaced-load-only-existing t
  "Whether to load only objects already existing in a namespace.")

(cl-defmethod ess-load-file--override (file &context ((string= ess-dialect "R") (eql t)))
  (cond
   ;; Namespaced evaluation
   ((ess-r-get-evaluation-env)
    (ess-r-load-file-namespaced file))
   ;; Evaluation into current env via .ess.source()
   (t
    (let ((command (ess-build-load-command file nil t)))
      (ess-send-string (ess-get-process) command)))))

(defun ess-r-load-file-namespaced (&optional file)
  "Load FILE into a package namespace.

This prompts for a package when no package is currently
selected (see `ess-r-set-evaluation-env')."
  (interactive)
  (ess-force-buffer-current "R process to use: ")
  (let* ((pkg-name (ess-r-get-evaluation-env))
         (command (ess-build-load-command file nil t pkg-name)))
    (ess-send-string (ess-get-process) command)))

(cl-defmethod ess-send-region--override (process start end visibly message type
                                                 &context ((string= ess-dialect "R") (eql t)))
  (cond
   ;; Namespaced evaluation
   ((ess-r-get-evaluation-env)
    (ess-r-send-region-namespaced process start end visibly message))
   ;; Evaluation into current env
   (t
    (ess-send-string process (buffer-substring start end) visibly message type))))

(defun ess-r-send-region-namespaced (proc beg end &optional visibly message)
  "Ask for for the package and devSource region into it."
  (let* ((pkg-name (or (ess-r-get-evaluation-env)
                       (ess-r-set-evaluation-env))))
    (message (ess-r-build-eval-message (or message "Eval region"))))
  (ess-send-string proc (buffer-substring start end) visibly message))


;;;*;;; Help

(defun ess-r-namespaced-object-p (object)
  (string-match "^[[:alnum:].]+::" object))

(defun ess-r-build-help-command--qualified (object)
  (when (ess-r-namespaced-object-p object)
    (let* ((pkg-name (substring object (match-beginning 0) (- (match-end 0) 2)))
           (object (concat "'" (substring object (match-end 0)) "'"))
           (pkg (ess-r-arg "package" pkg-name t)))
      (concat ".ess.help(" object pkg ")\n"))))

(defun ess-r-build-help-command--get-package-dir (object dont-ask)
  ;; Ugly hack to avoid tcl/tk dialogues
  (let ((pkgs (ess-get-words-from-vector
               (format "as.character(utils::help('%s'))\n" object))))
    (when (> (length pkgs) 1)
      (if dont-ask
          (car pkgs)
        (ess-completing-read "Choose location" pkgs nil t)))))

(defun ess-r-build-help-command--unqualified (object dont-ask)
  (if (eq ess-help-type 'index)
      ;; we are in index page, qualify with namespace
      (ess-r-build-help-command--qualified (format "%s::%s" ess-help-object object))
    (let ((pkg-dir (ess-r-build-help-command--get-package-dir object dont-ask))
          (command (format inferior-ess-r-help-command object)))
      (if pkg-dir
          ;; Invoking `print.help_files_with_topic'
          (format "do.call(structure, c('%s', attributes(%s)))\n" pkg-dir command)
        command))))

(defun ess-r-build-help-command (object &optional dont-ask)
  (or (ess-r-build-help-command--qualified object)
      (ess-r-build-help-command--unqualified object dont-ask)))

(defconst inferior-ess-r--input-help (format "^ *help *(%s)" ess-help-arg-regexp))
(defconst inferior-ess-r--input-?-help-regexp "^ *\\(?:\\(?1:[a-zA-Z ]*?\\?\\{1,2\\}\\) *\\(?2:.+\\)\\)")
(defconst inferior-ess-r--page-regexp (format "^ *page *(%s)" ess-help-arg-regexp))

(defvar ess-help-r--last-help-type nil
  "Variable holding the last known help type.
If it changes, we flush the cache.")

(defun ess-help-r--check-last-help-type ()
  (let ((help-type (ess-string-command "getOption('help_type')\n")))
    (when (not (string= help-type ess-help-r--last-help-type))
      (let ((help-buffers (ess-help-get-local-help-buffers)))
        (mapc #'kill-buffer help-buffers))
      (setq ess-help-r--last-help-type help-type))))

(defun ess-help-r--process-help-input (proc string)
  (let ((help-match (and (string-match inferior-ess-r--input-help string)
                         (match-string 2 string)))
        (help-?-match (and (string-match inferior-ess-r--input-?-help-regexp string)
                           string))
        (page-match   (and (string-match inferior-ess-r--page-regexp string)
                           (match-string 2 string))))
    (when (or help-match help-?-match page-match)
      (ess-help-r--check-last-help-type)
      (cond (help-match
             (ess-display-help-on-object help-match)
             (process-send-string proc "\n"))
            (help-?-match
             (ess-help-r--display-help-? proc string help-?-match)
             (process-send-string proc "\n"))
            (page-match
             (switch-to-buffer-other-window
              (ess-command (concat page-match "\n")
                           (get-buffer-create (concat page-match ".rt"))))
             (ess-r-transcript-mode)
             (process-send-string proc "\n")))
      t)))

(defun ess-help-r--display-help-? (proc string help-?-match)
  (cond ((string-match "\\?\\?\\(.+\\)" help-?-match)
         (ess--display-indexed-help-page (concat help-?-match "\n")
                                         "^\\([^ \t\n]+::[^ \t\n]+\\)[ \t\n]+"
                                         (format "*ess-apropos[%s](%s)*"
                                                 ess-current-process-name (match-string 1 help-?-match))
                                         'appropos))
        ((string-match "^ *\\? *\\([^ \t]+\\)$" help-?-match)
         (ess-display-help-on-object (match-string 1 help-?-match)))
        ;; Anything else we send to process almost unchanged
        (t
         (let ((help-?-match (and (string-match inferior-ess-r--input-?-help-regexp string)
                                  (format "%s%s" (match-string 1 string)
                                          (ess-help-r--sanitize-topic (match-string 2 string))))))
           (ess-display-help-on-object help-?-match "%s\n")))))

(defun ess-help-r--sanitize-topic (string)
  "Enclose help topic STRING into `` to avoid ?while ?if etc hangs."
  (if (string-match "\\([^:]*:+\\)\\(.*\\)$" string) ; treat foo::bar corectly
      (format "%s`%s`" (match-string 1 string) (match-string 2 string))
    (format "`%s`" string)))


;;;*;;; Utils for inferior R process

(defun inferior-ess-r-input-sender (proc string)
  (save-current-buffer
    (or (ess-help-r--process-help-input proc string)
        (inferior-ess-input-sender proc string))))

(defun inferior-ess-r-load-ESSR ()
  "Load/INSTALL/Update ESSR."
  (let* ((pkg-dir (expand-file-name "ESSR" ess-etc-directory))
         (src-dir (expand-file-name "R" pkg-dir)))
    (if (not (or (and (boundp 'ess-remote) ess-remote)
                 (file-remote-p (ess-get-process-variable 'default-directory))))
        (inferior-ess-r-load-ESSR--local pkg-dir src-dir)
      (inferior-ess-r-load-ESSR--remote pkg-dir src-dir))))

(defun inferior-ess-r-load-ESSR--local (pkg-dir src-dir)
  (let ((cmd (format "local({
                          source('%s/.load.R', local=TRUE) #define load.ESSR
                          load.ESSR('%s')
                      })\n"
                     src-dir src-dir)))
    (ess-write-to-dribble-buffer (format "load-ESSR cmd:\n%s\n" cmd))
    (with-current-buffer (ess-command cmd)
      (let ((msg (buffer-string)))
        (when (> (length msg) 1)
          (message (format "load ESSR: %s" msg)))))))

(defun inferior-ess-r-load-ESSR--remote (pkg-dir src-dir)
  (let* ((loadremote (expand-file-name "LOADREMOTE" pkg-dir))
         (r-load-code (if (file-exists-p loadremote)
                          (with-temp-buffer
                            (insert-file-contents loadremote)
                            (buffer-string))
                        (error "Cannot find ESSR source code"))))
    (unless (ess-boolean-command (format r-load-code essr-version) nil 0.1)
      (let ((errmsg (with-current-buffer " *ess-command-output*" (buffer-string)))
            (files (directory-files src-dir t "\\.R$")))
        (ess-write-to-dribble-buffer (format "error loading ESSR.rda: \n%s\n" errmsg))
        ;; should not happen, unless extrem conditions (ancient R or failed download))
        (message "Failed to download ESSR.rda (see *ESS* buffer). Injecting ESSR code from local machine")
        ;; For this case we cannot do it at R level
        (ess-command (format ".ess.ESSRversion <- '%s'\n" essr-version))
        (mapc #'ess--inject-code-from-file files)))))

(cl-defmethod ess-quit--override (arg &context ((string= ess-dialect "R") (eql t)))
  "With ARG, do not offer to save the workspace."
  (let (cmd
        (sprocess (ess-get-process ess-current-process-name)))
    (when (not sprocess) (error "No ESS process running"))
    (ess-cleanup)
    (setq cmd (format "base::q('%s')\n" (if arg "no" "default")))
    (goto-char (marker-position (process-mark sprocess)))
    (insert cmd)
    (process-send-string sprocess cmd)))

(defcustom inferior-ess-r-reload-hook nil
  "Hook run when reloading the R inferior buffer."
  :type 'hook
  :group 'ess-R)

(cl-defmethod inferior-ess-reload--override (start-args
                                             &context ((string= ess-dialect "R") (eql t)))
  "Call `run-ess-r' with START-ARGS.
Then run `inferior-ess-r-reload-hook'."
  (run-ess-r start-args)
  (run-hooks 'inferior-ess-r-reload-hook))

(defun inferior-ess-r-force (&optional prompt force no-autostart ask-if-1)
  (setq ess-dialect "R")
  (ess-force-buffer-current prompt force no-autostart ask-if-1))


;;*;; Editing Tools

;;;*;;; Indentation Engine

;; Written by Lionel Henry in mid 2015

(defun ess-r-indent-line ()
  "Indent current line as ESS R code.
Return the amount the indentation changed by."
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
    shift-amt))

(defun ess-r-indent-exp ()
  (save-excursion
    (when current-prefix-arg
      (ess-climb-to-top-level))
    (let* ((bounds (ess-continuations-bounds))
           (end (cadr bounds))
           (beg (if current-prefix-arg
                    (car bounds)
                  (forward-line)
                  (point))))
      (indent-region beg end))))

(defun ess-indent-call (&optional start)
  (save-excursion
    (when (ess-escape-calls)
      (setq start (or start (point)))
      (skip-chars-forward "^[(")
      (forward-char)
      (ess-up-list)
      (indent-region start (point)))))

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
         ess-indent-offset)))

(defun ess-offset-type (offset)
  (setq offset (eval (intern (concat "ess-offset-" (symbol-name offset)))))
  (if (listp offset)
      (car offset)
    offset))

(defun ess-overridden-blocks ()
  (append (when (memq 'fun-decl ess-align-blocks)
            (list (car ess-prefixed-block-patterns)))
          (when (memq 'control-flow ess-align-blocks)
            (append (cdr ess-prefixed-block-patterns)
                    '("}?[ \t]*else")))))

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
      (back-to-indentation)
      (cond
       ;; Strings
       ((ess-inside-string-p)
        (current-indentation))
       ;; Comments
       ((ess-calculate-indent--comments))
       ;; Indentation of commas
       ((looking-at ",")
        (ess-calculate-indent--comma))
       ;; Arguments: Closing
       ((ess-call-closing-p)
        (ess-calculate-indent--call-closing-delim))
       ;; Block: Contents (easy cases)
       ((ess-calculate-indent--block-relatively))
       ;; Block: Prefixed block
       ((ess-calculate-indent--prefixed-block-curly))
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
  (when (ess-inside-call-p)
    (let ((indent (save-excursion
                    (ess-calculate-indent--args)))
          (unindent (progn (skip-chars-forward " \t")
                           ;; return number of skiped chars
                           (skip-chars-forward ", \t"))))
      (- indent unindent))))

(defun ess-calculate-indent--call-closing-delim ()
  (cond ((save-excursion
           (ess-skip-blanks-backward t)
           (eq (char-before) ?,))
         (ess-calculate-indent--args nil))
        ((save-excursion
           (and (ess-ahead-operator-p)
                (or (ess-ahead-definition-op-p)
                    (not ess-align-continuations-in-calls))))
         (ess-calculate-indent--continued))
        (t
         (ess-calculate-indent--args 0))))

(defun ess-calculate-indent--block-opening ()
  (cond
   ;; Block is an argument in a function call
   ((when containing-sexp
      (ess-at-containing-sexp
        (ess-behind-call-opening-p "[[(]")))
    (ess-calculate-indent--block 0))
   ;; Top-level block
   ((null containing-sexp) 0)
   ;; Block is embedded in another block
   ((ess-at-containing-sexp
      (+ (current-indentation)
         (ess-offset 'block))))))

(defun ess-calculate-indent--aligned-block ()
  ;; Check for `else' opening
  (if (and (memq 'control-flow ess-align-blocks)
           (looking-at "else\\b")
           (ess-climb-if-else))
      (progn
        (when (looking-at "else\\b")
          (ess-skip-curly-backward))
        (current-column))
    ;; Check for braced and unbraced blocks
    (ess-save-excursion-when-nil
      (let ((offset (if (looking-at "[{})]")
                        0 (ess-offset 'block))))
        (when (and (cond
                    ;; Unbraced blocks
                    ((ess-climb-block-prefix))
                    ;; Braced blocks
                    (containing-sexp
                     (when (ess-at-containing-sexp
                             (looking-at "{"))
                       (ess-escape-prefixed-block))))
                   (cl-some 'looking-at
                            (ess-overridden-blocks)))
          (+ (current-column) offset))))))

(defun ess-calculate-indent--block-relatively ()
  (ess-save-excursion-when-nil
    (let ((offset (if (looking-at "[})]") 0 (ess-offset 'block)))
          (start-line (line-number-at-pos)))
      (cond
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
       ((ess-ahead-continuation-p)
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
                 (back-to-indentation)
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
                              (back-to-indentation)
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
             (ess-behind-call-opening-p "[[(]")))
      'body)
     ;; Indentation of opening brace as argument
     ((ess-at-containing-sexp
        (ess-behind-call-opening-p "[[(]"))
      'opening)
     ;; Indentation of body or closing brace as argument
     ((ess-at-containing-sexp
        (and (or (looking-at "{")
                 (ess-behind-block-paren-p))
             prev-containing-sexp
             (goto-char prev-containing-sexp)
             (ess-behind-call-opening-p "[[(]")))
      'body))))

(defun ess-calculate-indent--block (&optional offset)
  (let ((arg-block (ess-arg-block-p)))
    (cond (arg-block
           (ess-calculate-indent--arg-block offset arg-block))
          (t
           ;; Block is not part of an arguments list. Climb over any
           ;; block opening (function declaration, etc) to indent from
           ;; starting indentation.
           (or (ess-climb-block-prefix)
               (and (goto-char containing-sexp)
                    (ess-climb-block-prefix)))
           (+ (current-indentation) (or offset (ess-offset 'block)))))))

(defun ess-calculate-indent--arg-block (offset arg-block)
  (let* ((block-type (cond ((or (ess-at-containing-sexp
                                  (and (eq arg-block 'body)
                                       (ess-climb-block-prefix "function")))
                                (ess-at-indent-point
                                  (and (eq arg-block 'opening)
                                       (ess-backward-sexp 2)
                                       (looking-at "function\\b"))))
                            'fun-decl)
                           ((ess-at-indent-point
                              (ess-unbraced-block-p))
                            'unbraced)
                           ((ess-at-containing-sexp
                              (not (ess-ahead-attached-name-p)))
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
                          (cl-some 'looking-at
                                   ess-align-arguments-in-calls))))
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
  (ess-climb-chained-delims)
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
        (ess-climb-block-prefix))
      (current-indentation))
     ;; Function blocks need special treatment
     ((and (eq type 'prev-line)
           (eq block 'fun-decl))
      (goto-char containing-sexp)
      (ess-climb-block-prefix)
      (current-indentation))
     ;; Regular case
     (t
      ;; Find next non-empty line to indent from
      (while (and (= (forward-line -1) 0)
                  (looking-at "[ \t]*\\($\\|#\\)")))
      (goto-char (ess-code-end-position))
      ;; Climb relevant structures
      (unless (ess-climb-block-prefix)
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
      (back-to-indentation)
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

;; Move to leftmost side of a call (either the first letter of its
;; name or its closing delim)
(defun ess-move-to-leftmost-side ()
  (when (or (looking-at "[({]")
            (ess-behind-call-p))
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
        (unless (and ess-indent-with-fancy-comments
                     (looking-at "### "))
          (setq max-col (min max-col (current-column))))
        (forward-line)
        (back-to-indentation)))
    max-col))

(defun ess-calculate-indent--prefixed-block-curly ()
  (when (looking-at "{")
    (ess-save-excursion-when-nil
      (let ((block-type (ess-climb-block-prefix)))
        (cond ((ess-save-excursion-when-nil
                 (and (memq 'fun-decl-opening ess-indent-from-lhs)
                      (string= block-type "function")
                      (ess-climb-operator)
                      (ess-behind-assignment-op-p)
                      (ess-climb-expression)))
               (current-column))
              ((= (save-excursion
                    (back-to-indentation)
                    (point))
                  (point))
               (ess-calculate-indent--continued)))))))

(defun ess-calculate-indent--continued ()
  "If a continuation line, return an indent of this line, otherwise nil."
  (save-excursion
    (let* ((start-line (line-number-at-pos))
           (prev-pos 0)
           (cascade (eq (ess-offset-type 'continued) 'cascade))
           (climbed (ess-climb-continuations cascade))
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
                                    (when (ess-ahead-closing-p)
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
                (ess-ahead-closing-p)
              (backward-char))
            (+ (min (current-column) max-col)
               (cond
                ((eq (ess-offset-type 'continued) 'cascade)
                 (ess-offset 'continued))
                (first-indent
                 (ess-offset 'continued))
                (t
                 0))))))))))

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
                    containing-sexp
                    (not (= containing-sexp contained-sexp)))
               (goto-char (1+ contained-sexp))
               (ess-up-list))
              ;; Jump over continued statements
              ((and jump-cont (ess-ahead-operator-p 'strict))
               (ess-climb-token)
               (ess-jump-continuations))
              ;; Jump over comments
              ((looking-at "#")
               (forward-line)
               (ess-indent-line))
              (t
               (join-line 1))))
      (goto-char (ess-code-end-position)))
    (goto-char (car bounds))))

(defvar ess-fill--orig-pos nil
  "Original position of cursor.")

(defvar ess-fill--orig-state nil
  "Backup of original code to cycle back to original state.")

(defvar ess-fill--second-state nil
  "Backup of code produce by very first cycling.
If this is equal to orig-state, no need to cycle back to original
state.")

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

(defun ess-fill-args (&optional style)
  (let ((start-pos (point-min))
        (orig-col (current-column))
        (orig-line (line-number-at-pos))
        (bounds (ess-args-bounds 'marker))
        ;; Set undo boundaries manually
        (undo-inhibit-record-point t)
        last-pos last-newline prefix-break
        infinite)
    (when (not bounds)
      (error "Could not find function bounds"))
    (setq style (or style (ess-fill-style 'calls bounds)))
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
         ;; Some styles start with first argument on a newline
         ((and (memq style '(2 4))
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
                        (ess-jump-arg)
                        (ess-jump-char ","))
              (setq i (1- i))))
          (newline-and-indent)))
        (ess-fill-args--roll-lines)
        ;; Reindent surrounding context
        (ess-indent-call (car bounds)))
      ;; Signal marker for garbage collection
      (set-marker (cadr bounds) nil)
      (undo-boundary))))

(defun ess-fill-args--roll-lines ()
  (while (and (not (looking-at "[])]"))
              (/= (point) (or last-pos 1))
              (not infinite))
    (setq prefix-break nil)
    ;; Record start-pos as future breaking point to avoid breaking
    ;; at `=' sign
    (while (looking-at "[ \t]*[\n#]")
      (forward-line)
      (back-to-indentation))
    (setq start-pos (point))
    (while (and (< (current-column) fill-column)
                (not (looking-at "[])]"))
                (/= (point) (or last-pos 1))
                ;; Break after one pass if prefix is active
                (not prefix-break))
      (when (memq style '(2 3))
        (setq prefix-break t))
      (ess-jump-token ",")
      (setq last-pos (point))
      ;; Jump expression and any continuations. Reindent all lines
      ;; that were jumped over
      (let ((cur-line (line-number-at-pos))
            end-line)
        (cond ((ess-jump-arg)
               (setq last-newline nil))
              ((ess-token-after= ",")
               (setq last-newline nil)
               (setq last-pos (1- (point)))))
        (save-excursion
          (when (< cur-line (line-number-at-pos))
            (setq end-line (line-number-at-pos))
            (ess-goto-line (1+ cur-line))
            (while (and (<= (line-number-at-pos) end-line)
                        (/= (point) (point-max)))
              (ess-indent-line)
              (forward-line))))))
    (when (or (>= (current-column) fill-column)
              prefix-break
              ;; Ensures closing delim on a newline
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
             (when (and (memq style '(2 3 4))
                        ess-fill-calls-newlines
                        (not last-newline))
               (newline-and-indent)
               ;; Prevent indenting infinitely
               (setq last-newline t)))
            ((not last-newline)
             (newline-and-indent)
             (setq last-newline t))
            (t
             (setq infinite t))))))

(defun ess-fill-continuations (&optional style)
  (let ((bounds (ess-continuations-bounds 'marker))
        (undo-inhibit-record-point t)
        (last-pos (point-min))
        last-newline infinite)
    (when (not bounds)
      (error "Could not find statements bounds"))
    (setq style (or style (ess-fill-style 'continuations bounds)))
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



;;;*;;; Inferior R mode

(defvar inferior-ess-r-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\r" 'ess-dirs)
    map)
  "Keymap for `inferior-ess-r-mode'.")

;; TOTHINK: Prevent string delimiting characters from messing up output in the
;; inferior buffer
(defvar inferior-ess-r-mode-syntax-table
  (let ((table (copy-syntax-table ess-r-mode-syntax-table)))
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?\' "." table)
    table)
  "Syntax table for `inferior-ess-r-mode'.")

(define-derived-mode inferior-ess-r-mode inferior-ess-mode "iESS"
  "Major mode for interacting with inferior R processes."
  (setq-local comint-process-echoes (eql ess-eval-visibly t))
  ;; eldoc
  (add-function :before-until (local 'eldoc-documentation-function)
                #'ess-r-eldoc-function)
  (when ess-use-eldoc (eldoc-mode))
  ;; auto-complete
  (ess--setup-auto-complete ess-r-ac-sources t)
  ;; company
  (ess--setup-company ess-r-company-backends t)
  (setq comint-get-old-input #'inferior-ess-get-old-input)
  (add-hook 'comint-input-filter-functions 'ess-search-path-tracker nil 'local))



;;;*;;; R Help mode

(defvar ess-r-help-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 ess-help-mode-map))
    (define-key map "s<" 'beginning-of-buffer)
    (define-key map "s>" 'end-of-buffer)
    (define-key map "sa" 'ess-skip-to-help-section)
    (define-key map "sd" 'ess-skip-to-help-section)
    (define-key map "sD" 'ess-skip-to-help-section)
    (define-key map "st" 'ess-skip-to-help-section)
    (define-key map "se" 'ess-skip-to-help-section)
    (define-key map "sn" 'ess-skip-to-help-section)
    (define-key map "sr" 'ess-skip-to-help-section)
    (define-key map "ss" 'ess-skip-to-help-section)
    (define-key map "su" 'ess-skip-to-help-section)
    (define-key map "sv" 'ess-skip-to-help-section)
    map)
  "Keymap for `ess-r-help-mode'.")

(define-derived-mode ess-r-help-mode ess-help-mode "R Help"
  "Major mode for help buffers."
  (setq ess-dialect "R"
        ess-help-web-search-command #'ess-r-sos
        ess-build-help-command-function #'ess-r-build-help-command
        ess-help-sec-regex ess-help-r-sec-regex
        ess-help-sec-keys-alist ess-help-r-sec-keys-alist ; TODO: Still necessary?
        inferior-ess-help-command inferior-ess-r-help-command
        ess-get-help-topics-function #'ess-s-get-help-topics-function
        inferior-ess-help-filetype nil)
  (ess-r-help-add-links))

(define-button-type 'ess-r-help-link
  'follow-link t
  'action (lambda (_) (ess-r-help-button-action)))

(defun ess-r-help-button-action ()
  "Display help for button at point."
  (let ((text (get-text-property (point) 'ess-r-help-link-text)))
    (ess-display-help-on-object text)))

(defun ess-r-help-add-links ()
  "Add links to the help buffer."
  (save-excursion
    ;; Search for fancy quotes only. If users have
    ;; options(useFancyQuotes) set to something other than TRUE this
    ;; probably won't work. If it's FALSE, R outputs ascii ', but
    ;; searching through the whole buffer takes too long.
    (while (re-search-forward "‘[^[:space:]]+?’" nil t)
      (let ((inhibit-read-only t)
            (beg (match-beginning 0))
            (end (match-end 0))
            (text (match-string 0)))
        ;; Remove surrounding quotes so the text property is the
        ;; name of the link:)
        (progn
          ;; TODO: Replace all this with:
          ;; (string-trim (match-string 0) "[\"‘]*" "[\"’]*")
          ;; after dropping support for Emacs 25
          (setq text
                (if (string-match "^[\"‘]*" text)
                    (substring text (match-end 0))
                  text))
          (setq text (if (string-match "[\"’]*$" text)
                         (substring text 0 (match-beginning 0))
                       text)))
        (add-text-properties beg end (list 'ess-r-help-link-text text))
        (make-text-button beg end
                          'type 'ess-r-help-link
                          'help-echo (format "mouse-2, RET: Help on %s" text))))))




;; Create functions that can be called for running different versions
;; of R.
;; FIXME: Should be set in ess-custom
(setq ess-rterm-version-paths
      (ess-flatten-list
       (delete-dups
        (if (not ess-directory-containing-R)
            (if (getenv "ProgramW6432")
                (let ((P-1 (getenv "ProgramFiles(x86)"))
                      (P-2 (getenv "ProgramW6432")))
                  (nconc
                   ;; Always 32 on 64 bit OS, nil on 32 bit OS
                   (ess-find-rterm (concat P-1 "/R/") "bin/Rterm.exe")
                   (ess-find-rterm (concat P-1 "/R/") "bin/i386/Rterm.exe")

                   ;; Keep this both for symmetry and because it can happen:
                   (ess-find-rterm (concat P-1 "/R/") "bin/x64/Rterm.exe")

                   ;; Always 64 on 64 bit OS, nil on 32 bit OS
                   (ess-find-rterm (concat P-2 "/R/") "bin/Rterm.exe")
                   (ess-find-rterm (concat P-2 "/R/") "bin/i386/Rterm.exe")
                   (ess-find-rterm (concat P-2 "/R/") "bin/x64/Rterm.exe")))
              (let ((PF (getenv "ProgramFiles")))
                (nconc
                 ;; Always 32 on 32 bit OS, depends on 32 or 64 process on 64 bit OS
                 (ess-find-rterm (concat PF "/R/") "bin/Rterm.exe")
                 (ess-find-rterm (concat PF "/R/") "bin/i386/Rterm.exe")
                 (ess-find-rterm (concat PF "/R/") "bin/x64/Rterm.exe"))))
          (let ((PF ess-directory-containing-R))
            (nconc
             (ess-find-rterm (concat PF "/R/") "bin/Rterm.exe")
             (ess-find-rterm (concat PF "/R/") "bin/i386/Rterm.exe")
             (ess-find-rterm (concat PF "/R/") "bin/x64/Rterm.exe")))))))
(ess-r-define-runners)

;;*;; Provide and auto-loads

;;;###autoload
(add-to-list 'auto-mode-alist '("/Makevars\\(\\.win\\)?$" . makefile-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("DESCRIPTION$" . conf-colon-mode))

(provide 'ess-r-mode)

;;; Local variables:
;;; mode: emacs-lisp
;;; byte-compile-warnings: (not lexical)
;;; End:

;;; ess-r-mode.el ends here
