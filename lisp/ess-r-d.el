;;; ess-r-d.el --- R customization

;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2015 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
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
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; This file defines all the R customizations for ESS.  See ess-s-l.el
;; for general S language customizations.

;;; Code:

(ess-message "[ess-r-d:] (require 'ess-s-l)")
(require 'cl)
(require 'compile)
(require 'easymenu)
(require 'eldoc)
(require 'ess-utils)
(require 'ess-help)
(require 'ess-tracebug)
(require 'ess-s-l)
(require 'ess-roxy)
(require 'ess-r-completion)
(require 'ess-r-syntax)
(require 'ess-r-package)

(autoload 'ess-r-args-show      "ess-r-args" "(Autoload)" t)
(autoload 'ess-r-args-auto-show "ess-r-args" "(Autoload)" t)

(defvar ess-dev-map
  (let (ess-dev-map)
    (define-prefix-command 'ess-dev-map)
    (define-key ess-dev-map "\C-s" 'ess-r-select-evaluation-namespace)
    (define-key ess-dev-map "T" 'ess-toggle-tracebug)
    (define-key ess-dev-map "\C-l" 'ess-r-package-load-package)
    (define-key ess-dev-map "l" 'ess-r-package-load-package)
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

(defvar ess-r-package-dev-map
  (let (ess-r-package-dev-map)
    (define-prefix-command 'ess-r-package-dev-map)
    (define-key ess-r-package-dev-map "\C-a" 'ess-r-package-select-package)
    (define-key ess-r-package-dev-map "\C-c" 'ess-r-devtools-check-package)
    (define-key ess-r-package-dev-map "\C-d" 'ess-r-devtools-document-package)
    (define-key ess-r-package-dev-map "\C-i" 'ess-r-devtools-install-package)
    (define-key ess-r-package-dev-map "\C-l" 'ess-r-devtools-load-package)
    (define-key ess-r-package-dev-map "\C-r" 'ess-r-devtools-revdep-check-package)
    (define-key ess-r-package-dev-map "\C-t" 'ess-r-devtools-test-package)
    (define-key ess-r-package-dev-map "\C-u" 'ess-r-devtools-unload-package)
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
  "Developer submenu."
  '("Developer"
    :visible (and ess-dialect (string-match "^R" ess-dialect))
    ["Active?" ess-r-package-mode
     :style toggle
     :selected ess-r-package-mode]
    ["Select package" ess-r-package-select-package t]))

(easy-menu-add-item ess-mode-menu nil ess-roxygen-menu "end-dev")
(easy-menu-add-item ess-mode-menu nil ess-r-package-menu "end-dev")
(easy-menu-add-item ess-mode-menu nil ess-tracebug-menu "end-dev")
(easy-menu-add-item inferior-ess-mode-menu nil ess-r-package-menu "end-dev")
(easy-menu-add-item inferior-ess-mode-menu nil ess-tracebug-menu "end-dev")

;; Inherit from S Syntax table:
(setq ess-r-syntax-table S-syntax-table)
(modify-syntax-entry ?` "\"" ess-r-syntax-table)
(modify-syntax-entry ?_  "_"  ess-r-syntax-table)

(defvar R-customize-alist
  (append
   '((ess-local-customize-alist             . 'R-customize-alist)
     (ess-dialect                           . "R")
     (ess-suffix                            . "R")
     (ess-ac-sources                        . ess-r-ac-sources)
     (ess-company-backends                  . ess-r-company-backends)
     (ess-build-tags-command                . ess-r-build-tags-command)
     (ess-traceback-command                 . ess-r-traceback-command)
     (ess-call-stack-command                . ess-r-call-stack-command)
     (ess-format-eval-command-function      . #'ess-r-format-eval-command)
     (ess-format-load-command-function      . #'ess-r-format-load-command)
     (ess-send-region-function              . #'ess-r-send-region)
     (ess-load-file-function                . #'ess-r-load-file)
     (ess-make-source-refd-command-function . #'ess-r-make-source-refd-command)
     (ess-format-eval-message-function      . #'ess-r-format-eval-message)
     (ess-install-library-function          . #'ess-r-install-library)
     (ess-eldoc-function                    . #'ess-r-eldoc-function)
     (ess-help-web-search-command           . #'ess-r-sos)
     (ess-dump-filename-template            . ess-r-dump-filename-template)
     (ess-mode-syntax-table                 . ess-r-syntax-table)
     (ess-mode-editing-alist                . ess-r-editing-alist)
     (ess-change-sp-regexp                  . ess-R-change-sp-regexp)
     (ess-help-sec-regex                    . ess-help-r-sec-regex)
     (ess-help-sec-keys-alist               . ess-help-r-sec-keys-alist)
     (ess-loop-timeout                      . ess-r-loop-timeout)
     (ess-function-pattern                  . ess-r-function-pattern)
     (ess-object-name-db-file               . "ess-r-namedb.el")
     (ess-smart-operators                   . ess-R-smart-operators)
     (inferior-ess-program                  . inferior-R-program-name)
     (inferior-ess-objects-command          . inferior-R-objects-command)
     (inferior-ess-font-lock-keywords       . 'inferior-R-font-lock-keywords)
     (inferior-ess-search-list-command      . "search()\n")
     (inferior-ess-help-command             . inferior-ess-r-help-command)
     (inferior-ess-help-filetype            . nil)
     (inferior-ess-exit-command             . "q()")
     (inferior-ess-exit-prompt              . "Save workspace image? [y/n/c]: ")
     (inferior-ess-start-file               . nil)
     (inferior-ess-start-args               . "")
     (inferior-ess-quit-function            . #'inferior-ess-r-quit)
     (ess-error-regexp-alist                . ess-r-error-regexp-alist)
     (ess-describe-object-at-point-commands . 'ess-R-describe-object-at-point-commands)
     (ess-STERM                             . "iESS")
     (ess-editor                            . R-editor)
     (ess-pager                             . R-pager)
     (prettify-symbols-alist                . '(("<-" . ?←)
                                                ("<<-" . ?↞)
                                                ("->"  . ?→)
                                                ("->>" . ?↠))))
   S-common-cust-alist)
  "Variables to customize for R -- set up later than emacs initialization.")

(defvar ess-r-build-tags-command
  "rtags('%s', recursive = TRUE, pattern = '\\\\.[RrSs](rw)?$',ofile = '%s')")

(defvar ess-r-traceback-command
  "local({cat(geterrmessage(), \
'---------------------------------- \n', \
fill=TRUE); try(traceback(), silent=TRUE)})\n")

(defvar ess-r-call-stack-command "traceback(1)\n")

(defvar ess-r-dump-filename-template
  (ess-replace-regexp-in-string
   "S$" "R" ess-dump-filename-template-proto))

(defvar ess-r-ac-sources
  '(ac-source-R))

(defvar ess-r-company-backends
  '((company-R-args company-R-objects)))

(defvar ess-r-loop-timeout
  2000000)

(defvar ess-r-editing-alist
  ;; copy the S-alist and modify :
  (let ((S-alist (copy-alist S-editing-alist)))
    (setcdr (assoc 'ess-font-lock-defaults S-alist)
            '(ess--extract-default-fl-keywords ess-R-font-lock-keywords))
    (setcdr (assoc 'ess-font-lock-keywords S-alist)
            (quote 'ess-R-font-lock-keywords))
    S-alist)
  "General options for editing R source files.")

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
             '(R "\\(\\(?:at \\|(@\\)\\([^#]+\\)[#:]\\([0-9]+\\)\\)"  2 3 nil 2 1))

(add-to-list 'compilation-error-regexp-alist-alist
             '(R1 " \\([^ \t\n]+\\)#\\([0-9]+\\)[: ]"  1 2 nil 2))

(add-to-list 'compilation-error-regexp-alist-alist
             '(R2 "(\\(\\w+ \\([^())\n]+\\)#\\([0-9]+\\)\\))"  2 3 nil 2 1))

;; Precedes R4 and allows spaces in file path
(add-to-list 'compilation-error-regexp-alist-alist
             ;; Start with bol,: but don't start with digit
             '(R3 "\\(?:^ +\\|: +\\)\\([^-+[:digit:]\n]:?[^:\n]*\\):\\([0-9]+\\):\\([0-9]+\\):"  1 2 3 2 1))

(add-to-list 'compilation-error-regexp-alist-alist
             ;; Don't start with digit, don't contain spaces
             '(R4 "\\([^-+ [:digit:]][^: \t\n]+\\):\\([0-9]+\\):\\([0-9]+\\):"  1 2 3 2 1))

(add-to-list 'compilation-error-regexp-alist-alist
             '(R-recover " *[0-9]+: +\\([^:\n\t]+?\\)#\\([0-9]+:\\)"  1 2 nil 2 1))

;; gnu C errors
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(R_C "^\\([^-+ [:digit:]][^: \t\n]+\\):\\([0-9]+\\):\\([0-9]+\\):"  2 3 nil 2 1))

(let ((r-ver '("R-1" "R-2" "R-3" "R-devel" "R-patched")))
  (defvar ess-r-versions
    (if (eq system-type 'darwin) (append r-ver '("R32" "R64")) r-ver)
    "List of partial strings for versions of R to access within ESS.
Each string specifies the start of a filename.  If a filename
beginning with one of these strings is found on `exec-path', a M-x
command for that version of R is made available.  For example, if the
file \"R-1.8.1\" is found and this variable includes the string
\"R-1\", a function called `M-x R-1.8.1' will be available to run that
version of R.
If duplicate versions of the same program are found (which happens if
the same path is listed on `exec-path' more than once), they are
ignored by calling `ess-uniq-list'.
Set this variable to nil to disable searching for other versions of R.
If you set this variable, you need to restart Emacs (and set this variable
before ess-site is loaded) for it to take effect."))

;;; Create functions for calling different (older or newer than default)
;;;  versions of R and S(qpe).
(defvar ess-versions-created nil
  "List of strings of all S- and R-versions found on the system.")

;; is currently used (updated) by ess-find-newest-R
(defvar ess-r-versions-created nil
  "List of strings of all R-versions found on the system.")

(defun ess-r-s-versions-creation ()
  "(Re)Create ESS  R-<..> commands FILENAME sans final \"extension\".
The extension, in a file name, is the part that follows the last `.'."
  (interactive)
  ;; Create ess-versions-created, ess-r-versions-created, and on
  ;; Windows, ess-rterm-version-paths
  (let ((R-newest-list '("R-newest"))
        (ess-s-versions-created
         (if ess-microsoft-p
             (nconc
              (ess-sqpe-versions-create ess-SHOME-versions) ;; 32-bit
              (ess-sqpe-versions-create ess-SHOME-versions-64 "-64-bit")) ;; 64-bit
           (ess-s-versions-create))))
    (if ess-microsoft-p
        (setq ess-rterm-version-paths
              (ess-flatten-list
               (ess-uniq-list
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
                     (ess-find-rterm (concat PF "/R/") "bin/x64/Rterm.exe"))))))))
    (ess-message "[ess-site:] (let ... before (ess-r-versions-create) ...")

    (setq ess-r-versions-created   ;; For Unix *and* Windows, using either
          (ess-r-versions-create)) ;; ess-r-versions or ess-rterm-version-paths (above!)

    ;; Add the new defuns, if any, to the menu.
    ;; Check that each variable exists, before adding.
    ;; e.g. ess-sqpe-versions-created will not be created on Unix.
    (setq ess-versions-created
          (ess-flatten-list
           (mapcar (lambda(x) (if (boundp x) (symbol-value x) nil))
                   '(R-newest-list
                     ess-r-versions-created
                     ess-s-versions-created))))))

(defun ess-r-s-versions-creation+menu ()
  "Call `\\[ess-r-s-versions-creation] creaing `ess-versions-created' and
update the \"Start Process\" menu."
  (interactive)
  (ess-message "[ess-site:] before (ess-r-s-versions-creation) ...")
  (ess-r-s-versions-creation)

  (when ess-versions-created
    ;; new-menu will be a list of 3-vectors, of the form:
    ;; ["R-1.8.1" R-1.8.1 t]
    (let ((new-menu (mapcar (lambda(x) (vector x (intern x) t))
                            ess-versions-created)))
      (easy-menu-add-item ess-mode-menu '("Start Process")
                          (cons "Other" new-menu))))
  ess-versions-created)

(defvar ess-R-post-run-hook nil
  "Functions run in process buffer after the initialization of R
  process.")

(defun ess-r-mode-p ()
  "Check whether we have a buffer running in R mode.

This is to get around the lack of proper derived modes in ESS."
  (and (eq major-mode 'ess-mode)
       (string= ess-dialect "R")))

;;;### autoload
(defun R (&optional start-args)
  "Call 'R', the 'GNU S' system from the R Foundation.
Optional prefix (C-u) allows to set command line arguments, such as
--vsize.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to R, put them in the variable `inferior-R-args'.

START-ARGS can be a string representing an argument, a list of
such strings, or any other non-nil value. In the latter case, you
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
                      (every 'stringp start-args))
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
         (cust-alist (copy-alist R-customize-alist))
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
    (setq comint-input-sender 'inferior-R-input-sender)

    (if gdbp
        (progn
          ;; We need to use callback, because R might start with a gdb process
          (ess-process-put 'callbacks '(R-initialize-on-start))
          ;; trigger the callback
          (process-send-string (get-process ess-local-process-name) "\n"))
      (ess-wait-for-process)
      (R-initialize-on-start))

    (ess-write-to-dribble-buffer
     (format "(R): inferior-ess-language-start=%s\n"
             inferior-ess-language-start))))

(defun R-initialize-on-start (&optional proc string)
  "This function is run after the first R prompt.
Executed in process buffer."
  (interactive)

  (when ess-can-eval-in-background
    (ess-async-command-delayed
     "invisible(installed.packages())\n" nil (get-process ess-local-process-name)
     ;; "invisible(Sys.sleep(10))\n" nil (get-process ess-local-process-name) ;; test only
     (lambda (proc) (process-put proc 'packages-cached? t))))

  (inferior-ess-r-load-ESSR)

  (when inferior-ess-language-start
    (ess-eval-linewise inferior-ess-language-start
                       nil nil nil 'wait-prompt))

  (with-ess-process-buffer nil
    (when ess-microsoft-p
      (ess-eval-linewise "options(chmhelp=FALSE, help_type=\"text\")"
                         nil nil nil 'wait))
    (add-hook 'ess-presend-filter-functions 'ess-R-scan-for-library-call nil 'local)
    (run-mode-hooks 'ess-R-post-run-hook)))


;; (defun ess--R-cache-installed-packages ()
;;   "Run by `ess-delayed-init' in R process buffer.
;; Useses internal R caching of installed packages."
;;   (ess-command "invisible(installed.packages())\n"
;;                nil nil nil .2 nil 'redisplay)
;;   (ess-process-put 'packages-cached? t)
;;   )

;;;### autoload
(defun R-mode  (&optional proc-name)
  "Major mode for editing R source.  See `ess-mode' for more help."
  (interactive "P")
  (setq ess-customize-alist R-customize-alist)
  (ess-mode R-customize-alist proc-name)
  ;; for emacs < 24
  (add-hook 'comint-dynamic-complete-functions 'ess-complete-object-name t 'local)
  ;; for emacs >= 24
  (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  (add-hook 'completion-at-point-functions 'ess-r-object-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)

  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  (when (and ess-imenu-use-S (require 'ess-menu))
    (setq imenu-generic-expression ess-imenu-generic-expression)
    (imenu-add-to-menubar "Imenu-R"))

  ;; useful for swankr/slime:
  (set (make-local-variable 'beginning-of-defun-function)
       (lambda (&optional arg)
         (skip-chars-backward " \t\n")
         (ess-beginning-of-function 'no-error)))
  (set (make-local-variable 'end-of-defun-function)
       'ess-end-of-function)

  (ess-roxy-mode t)
  (ad-activate 'fill-paragraph)
  (ad-activate 'move-beginning-of-line)
  (ad-activate 'back-to-indentation)
  (ad-activate 'ess-eval-line-and-step)

  ;; FIXME: Why advice our own function?
  (when ess-roxy-hide-show-p
    (ad-activate 'ess-indent-command))

  (run-hooks 'R-mode-hook))

(fset 'r-mode 'R-mode)

(defun ess-R-arch-2-bit (arch)
  "Translate R's architecture shortcuts/directory names to 'bits',
 i.e., \"32\" or \"64\" (for now)."
  (if (string= arch "i386")  "32"
    ;; else:
    "64"))

(defun ess-rterm-arch-version (long-path &optional give-cons)
  "Find an architecture-specific name for LONG-PATH, an absolute (long name) path
 to R on Windows. Returns either Name, a string, or a (Name . Path) cons, such as
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


(defun ess-r-versions-create ()
  "Generate the `M-x R-x.y.z' functions for starting other versions of R.
On MS Windows, this works using `ess-rterm-version-paths'; otherwise,
see `ess-r-versions' for strings that determine which functions are created.

The result is a list of the new R defuns, if any, that were created.  The
defuns will normally be placed on the menubar and stored as
`ess-r-versions-created' upon ESS initialisation."

  (if (not ess-r-versions)
      nil                               ;nothing to return
    ;; else, if ess-r-versions is non-nil, let's try to find those R versions.
    ;; This works by creating a temp buffer where the template function is
    ;; edited so that X.Y is replaced by the version name
    (let (versions
          r-versions-created
          (eval-buf (get-buffer-create "*ess-temp-r-evals*"))
          (template
           ;; This is the template function used for creating M-x R-X.Y.
           (concat
            "(defun R-X.Y (&optional start-args)
  \"Call the R version 'R-X.Y' using ESS.
This function was generated by `ess-r-versions-create'.\"
  (interactive \"P\")
  (let ((inferior-R-version \"R-X.Y\")
        (inferior-R-program-name \""
            (if ess-microsoft-p "Rterm" "R") "-X.Y\"))
    (R start-args)))
")))

      (with-current-buffer eval-buf
        ;; clear the buffer.
        (delete-region (point-min) (point-max))

        ;; Find which versions of R we want.  Remove the pathname, leaving just
        ;; the name of the executable.
        (setq versions
              (if ess-microsoft-p
                  (mapcar (lambda(v) (ess-rterm-arch-version v 'give-cons))
                          ess-rterm-version-paths)
                ;;        ^^^^^^^^^^^^^^^^^^^^^^^ from ./ess-site.el at start
                ;; else (non-MS):
                (ess-uniq-list
                 (mapcar 'file-name-nondirectory
                         (apply 'nconc
                                (mapcar 'ess-find-exec-completions
                                        ess-r-versions))))))
        (setq r-versions-created ; also for returning at end.
              (if ess-microsoft-p
                  (mapcar 'car versions)
                versions))
        (ess-write-to-dribble-buffer
         (format "(R): ess-r-versions-create making M-x defuns for \n %s\n"
                 (mapconcat 'identity r-versions-created "\n ")))

        ;; Iterate over each string in VERSIONS, creating a new defun each time.
        (while versions
          (let* ((version (car versions))
                 (ver (if ess-microsoft-p (car version) version))
                 (beg (point)))

            (setq versions (cdr versions))
            (insert template)
            (goto-char beg)
            (while (search-forward "R-X.Y" nil t) ;; in all cases
              (replace-match ver t t))
            (when ess-microsoft-p
              (goto-char beg)
              (while (search-forward "Rterm-X.Y" nil t)
                (replace-match (w32-short-file-name (cdr version)) t t)))
            (goto-char (point-max))))
        ;; buffer has now been created with defuns, so eval them!
        (eval-buffer))
      (unless (and (boundp 'ess-debugging) ess-debugging)
        (kill-buffer eval-buf))

      r-versions-created)))

(defvar ess-newest-R nil
  "Stores the newest version of R that has been found.  Used as a cache,
within ess-find-newest-R.  Do not use this value directly, but
instead call the function \\[ess-find-newest-R].")


(defcustom ess-prefer-higher-bit t
  "Non-nil means prefer higher bit architectures of R.
e.g. prefer 64 bit over 32 bit.  This is currently used only
by the code on Windows for finding the newest version of R."
  :group 'ess-R
  :type 'boolean)

(defun ess-rterm-prefer-higher-bit ()
  "Optionally remove 32bit Rterms from being candidate for R-newest.
Return the list of candidates for being R-newest.  Filtering is done
iff `ess-prefer-higher-bit' is non-nil.
This is used only by Windows when running `ess-find-newest-R'."
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
  "Find the newest version of R on the system.  Once the value is found,
cache it in the variable `ess-newest-R' for future use as finding the
newest version of R can be potentially time-consuming."
  (or ess-newest-R
      (progn (message "Finding all versions of R on your system...")
             ;;(sleep-for 3)
             nil)
      (setq ess-newest-R
            (ess-newest-r
             (if ess-microsoft-p
                 (ess-rterm-prefer-higher-bit)
               (add-to-list 'ess-r-versions-created
                            inferior-R-program-name))))))

(defun ess-check-R-program-name ()
  "Check if `inferior-R-program-name' points to an executable version of R.
If not, try to find the newest version of R elsewhere on the system, and
update `inferior-R-program-name' accordingly."
  (unless (executable-find inferior-R-program-name)
    ;; need to check if we can find another name.
    (let ((newest (ess-find-newest-R)))
      (if newest
          (setq inferior-R-program-name newest)
        (message "Sorry, no version of R could be found on your system.")))))

(defun R-newest (&optional start-args)
  "Find the newest version of R available, and run it.
Subsequent calls to R-newest will run that version, rather than searching
again for the newest version.  Providing an optional prefix arg (C-u) will
prompt for command line arguments."
  (interactive "P")
  (let ((rnewest (ess-find-newest-R)))
    (if (not rnewest)
        (error "No version of R could be found.")
      ;; Else: we have a working version of R.
      ;; Have to be careful to avoid recursion...
      (message (concat "Newest version of R is " rnewest))
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
                     (concat (if ess-microsoft-p (w32-short-file-name rver) rver)
                             " --version"))))
    (when (string-match
           "R \\(version \\)?[1-9][^\n]+ (\\(2[0-9-]+\\)\\( r[0-9]+\\)?)"
           ver-string)
      (setq date (match-string 2 ver-string)))
    (cons date rver)))

(defun ess-current-R-version ()
  "Get the version of R currently running in the ESS buffer as a string"
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

;; Test case for following defun:
;; (setq a '( ("2003-10-04" . "R-1.7")
;;         ("2006-11-19" . "R-2.2")
;;         ("2007-07-01" . "R-dev")
;;         ("-1" . "R-broken")
;;         ("2005-12-30" . "R-2.0")))
;; (ess-find-newest-date a)
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
If ESS-R-ROOT-DIR is nil, construct it by looking for an occurence of Rterm.exe
in the exec-path.  If there are no occurences of Rterm.exe in the exec-path,
then use `ess-program-files' (which evaluates to something like \"c:/progra~1/R/\"
in English locales) which is the default location for the R distribution.
If BIN-RTERM-EXE is nil, then use \"bin/Rterm.exe\"."
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
          (ess-replace-regexp-in-string "[\\]" "/" ess-R-root-dir))
    (let ((R-ver
           (ess-drop-non-directories
            (ess-flatten-list
             (mapcar (lambda (r-prefix)
                       (file-name-all-completions r-prefix ess-R-root-dir))
                     (append '("rw") ess-r-versions))))))
      (mapcar (lambda (dir)
                (let ((R-path
                       (concat ess-R-root-dir
                               (ess-replace-regexp-in-string "[\\]" "/" dir)
                               bin-Rterm-exe)))
                  (if (file-exists-p R-path) R-path)))
              R-ver))))

;;;###autoload
(defun Rnw-mode ()
  "Major mode for editing Sweave(R) source.
See `ess-noweb-mode' and `R-mode' for more help."
  (interactive)
  (require 'ess-noweb);; << probably someplace else
  (setq ess--make-local-vars-permanent t)
  (ess-noweb-mode 1); turn it on
  (ess-noweb-set-doc-mode 'latex-mode)
  (ess-noweb-set-code-mode 'R-mode)
  (setq ess--local-handy-commands
        (append '(("weave"      . ess-swv-weave)
                  ("tangle"     . ess-swv-tangle))
                ess-handy-commands)
        ess-dialect "R"
        ess-language "S")
  (put 'ess--local-handy-commands 'permanent-local t)
  (run-hooks 'Rnw-mode-hook))

(fset 'Snw-mode 'Rnw-mode); just a synonym (for now or ever)

(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files." t)

(defun R-transcript-mode ()
  "Does the right thing."
  (interactive)
  (ess-transcript-mode R-customize-alist))
(fset 'r-transcript-mode 'R-transcript-mode)

(defun R-fix-T-F (&optional from quietly)
  "Fix T/F into TRUE and FALSE *cautiously*, i.e. not in comments and strings;
 starting from the current position (point)."
  (interactive "d\nP"); point and prefix (C-u)
  (save-excursion
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\) *\\)T\\>" "\\1TRUE"
                    'fixcase nil (not quietly))
    (goto-char from)
    (ess-rep-regexp "\\(\\([][=,()]\\|<-\\) *\\)F\\>" "\\1FALSE"
                    'fixcase nil (not quietly))))

(defvar ess--packages-cache nil
  "Cache var to store package names. Used by
  `ess-r-install-library'.")

(defvar ess--CRAN-mirror nil
  "CRAN mirror name cache.")

(defun ess-r-install-library (&optional update pack)
  "Prompt and install R package. With argument, update cached packages list."
  (interactive "P")
  (when (equal "@CRAN@" (car (ess-get-words-from-vector "getOption('repos')[['CRAN']]\n")))
    (ess-setCRANMiror ess--CRAN-mirror)
    (ess-wait-for-process (get-process ess-current-process-name))
    (unless pack (setq update t)))
  (when (or update
            (not ess--packages-cache))
    (message "Fetching R packages ... ")
    (setq ess--packages-cache
          (ess-get-words-from-vector "print(rownames(available.packages()), max=1e6)\n")))
  (let* ((ess-eval-visibly-p t)
         (pack (or pack
                   (ess-completing-read "Package to install" ess--packages-cache))))
    (process-send-string (get-process ess-current-process-name)
                         (format "install.packages('%s')\n" pack))
    (display-buffer (buffer-name (process-buffer (get-process ess-current-process-name))))))

(defun ess-setRepositories ()
  "Call setRepositories()"
  (interactive)
  (if (not (string-match "^R" ess-dialect))
      (message "Sorry, not available for %s" ess-dialect)
    (ess-eval-linewise "setRepositories(FALSE)\n")))

(defun ess-setCRANMiror (&optional mirror)
  "Set cran mirror"
  (interactive)
  (let ((mirror-cmd "local({r <- getOption('repos'); r['CRAN'] <- '%s';options(repos=r)})\n"))
    (if mirror
        (ess-command (format mirror-cmd mirror))
      (let* ((M1 (ess-get-words-from-vector "local({out <- getCRANmirrors(local.only=TRUE); print(paste(out$Name,'[',out$URL,']', sep=''))})\n"))
             (M2 (mapcar (lambda (el)
                           (string-match "\\(.*\\)\\[\\(.*\\)\\]$" el)
                           (propertize (match-string 1 el) 'URL (match-string 2 el)))
                         M1))
             (mirror  (ess-completing-read "Choose CRAN mirror" M2 nil t)))
        (when mirror
          (setq mirror (get-text-property 0 'URL mirror))
          (setq ess--CRAN-mirror mirror)
          (ess-command (format mirror-cmd mirror))))))
  (message "CRAN mirror: %s" (car (ess-get-words-from-vector "getOption('repos')[['CRAN']]\n"))))

(defun ess-r-sos (cmd)
  "Interface to findFn in the library sos."
  (interactive  "sfindFn: ")
  (unless (equal "TRUE" (car (ess-get-words-from-vector "as.character(suppressPackageStartupMessages(require(sos)))\n")))
    (if (y-or-n-p "Library 'sos' is not installed. Install? ")
        (progn (ess-eval-linewise "install.packages('sos')\n")
               (ess-eval-linewise "library(sos)\n"))
      (signal 'quit nil)))
  (message nil)
  (ess-eval-linewise (format "findFn(\"%s\", maxPages=10)" cmd)))

(define-obsolete-function-alias 'ess-sos 'ess-r-sos "ESS[12.09-1]")

(defun ess-R-scan-for-library-call (string)
  "Detect `library/require' calls in string and update tracking vars.
Placed into `ess-presend-filter-functions' for R dialects."
  (when (string-match-p "\\blibrary(\\|\\brequire(" string)
    (ess--mark-search-list-as-changed))
  string)

(defun ess-load-library ()
  "Prompt and load dialect specific library/package/module.

Note that add-ons in R are called 'packages' and the name of this
function has nothing to do with R package mechanism, but it
rather serves a generic, dialect independent purpose. It is also
similar to `load-library' emacs function."
  (interactive)
  (if (not (string-match "^R" ess-dialect))
      (message "Sorry, not available for %s" ess-dialect)
    (let ((ess-eval-visibly-p t)
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

(defun ess-r-format-args (visibly output namespace)
  (let ((visibly (ess-r-arg "visibly" (if visibly "TRUE" "FALSE")))
        (output (ess-r-arg "output" (if output "TRUE" "FALSE")))
        (pkg (when namespace (ess-r-arg "package" namespace t)))
        (verbose (when (and ess-r-special-evaluation-mode
                            ess-r-namespaced-load-verbose)
                   (ess-r-arg "verbose" "TRUE"))))
    (concat visibly output pkg verbose)))

(defun ess-r-format-eval-command (string &optional visibly output file namespace)
  (let ((cmd (if namespace ".essDev.eval" ".ess.eval"))
        (file (when file (ess-r-arg "file" file t)))
        (args (ess-r-format-args visibly output namespace)))
    (concat cmd "('" string "'" args file ")\n")))

(defun ess-r-format-load-command (file &optional visibly output namespace)
  (let ((cmd (if namespace ".essDev_source" ".ess.source"))
        (args (ess-r-format-args visibly output namespace))
        (msg (concat "cat('"
                     (when namespace (format "[%s] " namespace))
                     (format "Sourced file %s\n')" file))))
    (concat cmd "('" file "'" args "); " msg)))

(defun ess-r-format-eval-message (message)
  (if (ess-r-namespaced-evaluation-p)
      (let ((pkg-name (ess-r--get-evaluation-env)))
        (format "[%s] %s" pkg-name message))
    message))

(defvar ess-r-evaluation-env nil
  "Environment into which code should be evaluated.

When nil, code is evaluated in the global environment if tracebug
is not active, or the evaluation environment of the current
function if it is active.

Currently only namespaces can be set as evaluation environments.
Use `ess-r-select-evaluation-namespace' to select a package
namespace.")
(make-variable-buffer-local 'ess-r-evaluation-env)

(defvar ess-r-prompt-for-attached-pkgs-only nil
  "Whether to look for all installed R packages.

If non-nil, only look for attached packages when selecting a
namespace to source into.")

(defun ess-r-select-evaluation-namespace (&optional arg)
  "Select a package namespace for evaluation of R code.

Call interactively with a prefix argument to disable evaluation
in a namespace.  When calling from a function, ARG can be a
string giving the package to select, any other non-nil value to
disable, or nil to prompt for a package.

If `ess-r-prompt-for-attached-pkgs-only' is non-nil, prompt only for
attached packages."
  (interactive "P")
  (let ((pkg-name (cond ((stringp arg)
                         arg)
                        (arg
                         ess-r-evaluation-env)
                        (t
                         (ess-r--select-package-name)))))
    (cond ((and arg (not (stringp arg)))
           (setq-local ess-r-evaluation-env nil)
           (ess-r-special-evaluation-mode -1)
           (message (format "Evaluation of code in %s disabled" pkg-name)))
          (t
           (setq-local ess-r-evaluation-env pkg-name)
           (ess-r-special-evaluation-mode 1)
           (message (format "Evaluating code in %s" pkg-name))))
    (force-mode-line-update)))

(defcustom ess-r-special-evaluation-mode-line
  '(:eval (if (and ess-r-package-mode
                   (string= ess-r-evaluation-env
                            (car (ess-r-package--local-package-info))))
              ""
            (format " [src:%s]" ess-r-evaluation-env)))
  "Mode line for namespaced evaluation.

The default value handles the interaction with `ess-r-package-mode-line'.
Set this variable to nil to disable the mode line entirely."
  :group 'ess-R
  :type 'sexp
  :risky t)

(define-minor-mode ess-r-special-evaluation-mode
  "Minor mode used for evaluating code into special R environments.

Currently only used for namespaced evaluation.  Its main purpose
is as a placeholder for special settings (e.g. a lighter for the
mode line)."
  :init-value nil
  :lighter ess-r-special-evaluation-mode-line)

(defun ess-r-namespaced-evaluation-p ()
  (and
   ;; Always evaluate in current environment while debugging
   (not ess-debug-minor-mode)
   (or ess-r-evaluation-env
       (ess-get-process-variable 'ess-r-evaluation-env))))

(defun ess-r--get-evaluation-env (&optional ask)
  (cond (ess-r-evaluation-env)
        (ask
         (ess-r-select-evaluation-namespace))
        (t
         (error "Namespaced evaluation is not active"))))

(defvar ess-r-namespaced-load-verbose t
  "Whether to display information on namespaced loading.

When t, loading a file into a namespaced will output information
about which objects are exported and which stay hidden in the
namespace.")

(defvar ess-r-namespaced-load-only-existing t
  "Whether to load only objects already existing in a namespace.")

(defun ess-r-load-file (file)
  (cond
   ;; Namespaced evaluation
   ((ess-r-namespaced-evaluation-p)
    (ess-r-load-file-namespaced file))
   ;; Evaluation into current env via .ess.source()
   (t
    (let ((command (ess-r-format-load-command file nil t)))
      (ess-send-string (ess-get-process) command)))))

(defun ess-r-load-file-namespaced (&optional file)
  "Load FILE into a package namespace.

This prompts for a package when no package is currently
selected (see `ess-r-select-evaluation-namespace')."
  (interactive)
  (ess-force-buffer-current "R process to use: ")
  (let* ((pkg-name (ess-r--get-evaluation-env))
         (command (ess-r-format-load-command file nil t pkg-name)))
    (ess-send-string (ess-get-process) command)))

(defun ess-r-make-source-refd-command (string visibly tmpfile)
  (let ((pkg-name (when (ess-r-namespaced-evaluation-p)
                    (ess-r--get-evaluation-env))))
    (ess-format-eval-command string visibly t tmpfile pkg-name)))

(defun ess-r-send-region (proc start end visibly message)
  (cond
   ;; Namespaced evaluation
   ((ess-r-namespaced-evaluation-p)
    (ess-r-send-region-namespaced proc start end visibly message))
   ;; Evaluation into current env
   (t
    (ess-send-string proc (buffer-substring start end) visibly message))))

(defun ess-r-send-region-namespaced (proc beg end &optional visibly message)
  "Ask for for the package and devSource region into it."
  (let* ((pkg-name (ess-r--get-evaluation-env 'ask-if-nil))
         (message (ess-r-format-eval-message (or message "Eval region"))))
    (ess-send-string proc (buffer-substring start end) visibly message)))


;;;*;;; Utils for inferior R process

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
  (let* ((verfile (expand-file-name "VERSION" pkg-dir))
         (loadremote (expand-file-name "LOADREMOTE" pkg-dir))
         (version (if (file-exists-p verfile)
                      (with-temp-buffer
                        (insert-file-contents verfile)
                        (buffer-string))
                    (error "Cannot find ESSR source code")))
         (r-load-code (with-temp-buffer
                        (insert-file-contents loadremote)
                        (buffer-string))))
    (ess-write-to-dribble-buffer (format "version file: %s\nloadremote file: %s\n"
                                         verfile loadremote))
    (unless (ess-boolean-command (format r-load-code version) nil 0.1)
      (let ((errmsg (with-current-buffer " *ess-command-output*" (buffer-string)))
            (files (directory-files src-dir t "\\.R$")))
        (ess-write-to-dribble-buffer (format "error loading ESSR.rda: \n%s\n" errmsg))
        ;; should not happen, unless extrem conditions (ancient R or failed download))
        (message "Failed to download ESSR.rda (see *ESS* buffer). Injecting ESSR code from local machine")
        (ess-command (format ".ess.ESSRversion <- '%s'\n" version)) ; cannot do this at R level
        (mapc #'ess--inject-code-from-file files)))))

(defun inferior-ess-r-quit (&optional no-save)
  "Issue an exiting command to an inferior R process, and
optionally clean up.  This version is for killing *R* processes;
it asks the extra question regarding whether the workspace image
should be saved unless NO-SAVE is non-nil."
  (ess-force-buffer-current "Process to quit: " nil 'no-autostart)
  (ess-make-buffer-current)
  (let (cmd
        (sprocess (ess-get-process ess-current-process-name)))
    (when (not sprocess) (error "No ESS process running"))
    (ess-cleanup)
    (setq cmd (format "base::q('%s')\n" (if no-save "no" "default")))
    (goto-char (marker-position (process-mark sprocess)))
    (process-send-string sprocess cmd)))

(defcustom inferior-ess-r-reload-hook nil
  "Hook run when reloading the R inferior buffer."
  :type 'hook
  :group 'ess-R)

(defun inferior-ess-r-reload (&optional start-args)
  "Reload R and the currently activated developer package, if any."
  (interactive)
  (ess-force-buffer-current)
  (let ((pkg-info ess-r-package-info)
        (r-proc (ess-get-process)))
    (with-ess-process-buffer nil
      (ess-quit-r 'no-save)
      (while (memq (process-status r-proc) '(run busy))
        (accept-process-output r-proc 0.002))
      (kill-buffer)
      (R start-args)
      (when pkg-info
        (setq-local ess-r-package-info pkg-info)
        (ess-r-package-load-package))
      (run-hooks 'inferior-ess-r-reload-hook))))


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
    (when (ess-climb-outside-calls)
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
         ess-indent-level)))

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
  (when (ess-point-in-call-p)
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
           (and (ess-climb-operator)
                (or (not ess-align-continuations-in-calls)
                    (ess-looking-at-definition-op-p))))
         (ess-calculate-indent--continued))
        (t
         (ess-calculate-indent--args 0))))

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
                       (ess-climb-outside-prefixed-block))))
                   (some 'looking-at (ess-overridden-blocks)))
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
        (unless (and ess-indent-with-fancy-comments
                     (looking-at "### "))
          (setq max-col (min max-col (current-column))))
        (forward-line)
        (ess-back-to-indentation)))
    max-col))

(defun ess-calculate-indent--prefixed-block-curly ()
  (when (looking-at "{")
    (ess-save-excursion-when-nil
      (let ((block-type (ess-climb-block-prefix)))
        (cond ((ess-save-excursion-when-nil
                 (and (memq 'fun-decl-opening ess-indent-from-lhs)
                      (string= block-type "function")
                      (ess-climb-operator)
                      (ess-looking-at-assignment-op-p)
                      (ess-climb-expression)))
               (current-column))
              ((= (save-excursion
                    (back-to-indentation)
                    (point))
                  (point))
               (ess-calculate-indent--continued)))))))

(defun ess-calculate-indent--continued ()
  "If a continuation line, return an indent of this line,
otherwise nil."
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
              (when (ess-jump-arg)
                (setq last-newline nil))
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
                   (setq infinite t)))))
        ;; Reindent surrounding context
        (ess-indent-call (car bounds)))
      ;; Signal marker for garbage collection
      (set-marker (cadr bounds) nil)
      (undo-boundary))))

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

(provide 'ess-r-d)

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

;;; ess-r-d.el ends here
