;;; ess-r-d.el --- R customization

;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2012 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
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

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file defines all the R customizations for ESS.  See ess-s-l.el
;; for general S language customizations.

;;; Code:

;;; Autoloads and Requires

(ess-message "[ess-r-d:] (require 'ess-s-l)")
(require 'ess-s-l)
(require 'eldoc)
(require 'ess-r-args); some. ~/.emacs rely ess-r-args-show .. replace by autoload !?
(require 'ess-developer)
(require 'ess-help)
(require 'ess-roxy)
(when (featurep 'emacs)
  (require 'ess-tracebug))
(require 'compile); for compilation-* below
(require 'easymenu)

(autoload 'ess-help-underline "ess-help" "(Autoload)" t)
(autoload 'ess--flush-help-into-current-buffer "ess-help" "(Autoload)" t)

(defvar ess-dev-map
  (let (ess-dev-map)
    (define-prefix-command 'ess-dev-map)
    ;; Note: some of these comand are automatically redefined by those in
    (define-key ess-dev-map "\C-t" 'ess-toggle-developer)
    (define-key ess-dev-map "t" 'ess-toggle-developer)
    ;; (define-key ess-dev-map "\C-T" 'ess-toggle-tracebug)
    (define-key ess-dev-map "T" 'ess-toggle-tracebug)
    (define-key ess-dev-map "\C-a" 'ess-developer-add-package)
    (define-key ess-dev-map "a" 'ess-developer-add-package)
    (define-key ess-dev-map "\C-r" 'ess-developer-remove-package)
    (define-key ess-dev-map "r" 'ess-developer-remove-package)
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
    (define-key ess-dev-map "\C-l" 'ess-bp-set-logger)
    (define-key ess-dev-map "l" 'ess-bp-set-logger)
    (define-key ess-dev-map "\C-o" 'ess-bp-toggle-state)
    (define-key ess-dev-map "o" 'ess-bp-toggle-state)
    (define-key ess-dev-map "\C-k" 'ess-bp-kill)
    (define-key ess-dev-map "k" 'ess-bp-kill)
    (define-key ess-dev-map "\C-K" 'ess-bp-kill-all)
    (define-key ess-dev-map "K" 'ess-bp-kill-all)
    (define-key ess-dev-map "\C-n" 'ess-bp-next)
    (define-key ess-dev-map "n" 'ess-bp-next)
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

(easy-menu-define ess-developer-menu nil
  "Developer submenu."
  '("Developer"
    :visible (and ess-dialect (string-match "^R" ess-dialect))
    ["Active?"          ess-toggle-developer
     :style toggle
     :selected (and (ess-process-live-p)
                    (ess-process-get 'developer))]
    ["Add package" ess-developer-add-package t]
    ["Remove package" ess-developer-remove-package t]))

(easy-menu-add-item ess-mode-menu nil ess-roxygen-menu "end-dev")
(easy-menu-add-item ess-mode-menu nil ess-developer-menu "end-dev")
(easy-menu-add-item ess-mode-menu nil ess-tracebug-menu "end-dev")

(easy-menu-add-item inferior-ess-mode-menu nil ess-developer-menu "end-dev")
(easy-menu-add-item inferior-ess-mode-menu nil ess-tracebug-menu "end-dev")


;; modify S Syntax table:
(setq R-syntax-table S-syntax-table)

;; In R 2.x, back tick now is a quote character, so lets tell Emacs
;; that it is; the problem below for older R should no longer be a
;; serious issue.
;;R >= 1.8: back tick `string` -- unfortunately no *pair* checking:
;; breaks when things like `..' are used:
(modify-syntax-entry ?` "\"" R-syntax-table)
(modify-syntax-entry ?_  "_"  R-syntax-table) ; foo_bar is symbol in R >=1.9

(ess-message "[ess-r-d:] (autoload ..) & (def** ..)")

;; (autoload 'inferior-ess "ess-inf" "Run an ESS process.")
;; (autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

(defvar R-customize-alist
  (append
   '((ess-local-customize-alist         . 'R-customize-alist)
     (ess-eldoc-function                . 'ess-R-eldoc-function)
     (ess-dialect                       . "R")
     (ess-suffix                        . "R")
     (ess-build-tags-command            . "rtags('%s', recursive = TRUE, pattern = '\\\\.[RrSs](rw)?$',ofile = '%s')")
     (ess-traceback-command             . "local({try(traceback(), silent=TRUE);cat(\n\"---------------------------------- \n\", geterrmessage(), fill=TRUE)})\n")
     (ess-call-stack-command            . "traceback(1)\n")
     (ess-dump-filename-template        . (ess-replace-regexp-in-string
                                           "S$" ess-suffix ; in the one from custom:
                                           ess-dump-filename-template-proto))
     (ess-help-web-search-command       . 'ess-R-sos)
     (ess-mode-syntax-table             . R-syntax-table)
     (ess-mode-editing-alist            . R-editing-alist)
     (ess-change-sp-regexp              . ess-R-change-sp-regexp)
     (ess-help-sec-regex                . ess-help-R-sec-regex)
     (ess-help-sec-keys-alist           . ess-help-R-sec-keys-alist)
     (ess-loop-timeout                  . ess-S-loop-timeout);fixme: dialect spec.
     (ess-cmd-delay                     . ess-R-cmd-delay)
     (ess-function-pattern              . ess-R-function-pattern)
     (ess-object-name-db-file           . "ess-r-namedb.el" )
     (ess-smart-operators               . ess-R-smart-operators)
     (inferior-ess-program              . inferior-R-program-name)
     (inferior-ess-objects-command      . inferior-R-objects-command)
     (inferior-ess-font-lock-keywords   . 'inferior-R-font-lock-keywords)
     (inferior-ess-search-list-command  . "search()\n")
     ;;(inferior-ess-help-command               . "help(\"%s\", htmlhelp=FALSE)\n")
     (inferior-ess-help-command         . inferior-ess-r-help-command)
     (inferior-ess-help-filetype        . nil)
     (inferior-ess-exit-command         . "q()")
     (inferior-ess-exit-prompt          . "Save workspace image? [y/n/c]: ")
     ;;harmful for shell-mode's C-a: -- but "necessary" for ESS-help?
     (inferior-ess-start-file		. nil) ;; "~/.ess-R"
     (inferior-ess-start-args		. "")
     (ess-error-regexp-alist		. ess-R-error-regexp-alist)
     (ess-describe-object-at-point-commands . 'ess-R-describe-object-at-point-commands)
     (ess-STERM		. "iESS")
     (ess-editor	. R-editor)
     (ess-pager		. R-pager)
     )
   S-common-cust-alist)
  "Variables to customize for R -- set up later than emacs initialization.")

(defvar ess-R-error-regexp-alist '(R R2 R3 R-recover)
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(add-to-list 'compilation-error-regexp-alist-alist
             '(R "^.* \\(at \\(.+\\)#\\([0-9]+\\)\\)"  2 3 nil 2 1))

(add-to-list 'compilation-error-regexp-alist-alist
             '(R2 "(\\(from \\(.+\\)#\\([0-9]+\\)\\))"  2 3 nil 2 1))

;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(R2 "\\(?:^ +\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):\\)"  1 2 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(R3 "\\(?:Error.*: .*\n? +\\)\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):"  1 2 3 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(R-recover " *[0-9]+: +\\([^:\n\t]+?\\)#\\([0-9]+:\\)"  1 2 nil 2 1))

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
before ess-site is loaded) for it to take effect.")
  )

(defvar ess-R-post-run-hook nil
  "Functions run in process buffer after the initialization of R
  process.")


;;;### autoload
(defun R (&optional start-args)
  "Call 'R', the 'GNU S' system from the R Foundation.
Optional prefix (C-u) allows to set command line arguments, such as
--vsize.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to R, put them in the variable `inferior-R-args'."
  (interactive "P")
  ;; get settings, notably inferior-R-program-name :
  (setq ess-customize-alist R-customize-alist)
  (ess-write-to-dribble-buffer   ;; for debugging only
   (format
    "\n(R): ess-dialect=%s, buf=%s, start-arg=%s\n current-prefix-arg=%s\n"
    ess-dialect (current-buffer) start-args current-prefix-arg))
  (let* ((r-always-arg
          (if (or ess-microsoft-p (eq system-type 'cygwin))
              "--ess "
            ;; else: "unix alike"
            (if (not ess-R-readline) "--no-readline ")))
         (r-start-args
          (concat r-always-arg
                  inferior-R-args " " ; add space just in case
                  (if start-args
                      (read-string
                       (concat "Starting Args"
                               (if r-always-arg
                                   (concat " [other than '" r-always-arg "']"))
                               " ? "))
                    nil)))
         use-dialog-box)

    (when (or ess-microsoft-p
              (eq system-type 'cygwin))
      (setq use-dialog-box nil)
      (if ess-microsoft-p ;; default-process-coding-system would break UTF locales on Unix
          (setq default-process-coding-system '(undecided-dos . undecided-dos))))
    (inferior-ess r-start-args) ;; -> .. (ess-multi ...) -> .. (inferior-ess-mode) ..
    (ess-process-put 'funargs-pre-cache ess-R--funargs-pre-cache)
    ;;-------------------------
    (setq comint-input-sender 'inferior-R-input-sender)
    (ess-write-to-dribble-buffer
     (format "(R): inferior-ess-language-start=%s\n"
             inferior-ess-language-start))
    ;; can test only now that R is running:
    (ess-write-to-dribble-buffer
     (format "(R): version %s\n"
             (ess-get-words-from-vector "as.character(getRversion())\n")))

    (ess--inject-code-from-file (format "%sESSR.R" ess-etc-directory))

    (when ess-can-eval-in-background
      (ess-async-command-delayed
       "invisible(installed.packages())\n" nil (get-process ess-local-process-name)
       ;; "invisible(Sys.sleep(10))\n" nil (get-process ess-local-process-name) ;; test only
       (lambda (proc) (process-put proc 'packages-cached? t))))

    (if inferior-ess-language-start
        (ess-eval-linewise inferior-ess-language-start
                           nil nil nil 'wait-prompt))

    (with-ess-process-buffer nil
      (run-mode-hooks 'ess-R-post-run-hook))))



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
  ;;(setq imenu-generic-expression R-imenu-generic-expression)
  (ess-mode R-customize-alist proc-name)
  ;; for emacs < 24
  (add-hook 'comint-dynamic-complete-functions 'ess-complete-object-name t 'local)
  ;; for emacs >= 24
  (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  (add-hook 'completion-at-point-functions 'ess-object-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)

  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  ;; ECB needs seminatic stuff.
  ;;  (if (featurep 'semantic)
  ;;      (setq semantic-toplevel-bovine-table r-toplevel-bovine-table))
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
  (ad-activate 'mark-paragraph)
  (ad-activate 'fill-paragraph)
  (ad-activate 'move-beginning-of-line)
  (ad-activate 'newline-and-indent)
  (if ess-roxy-hide-show-p
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
"
            ) ))

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
            (goto-char (point-max)))
          )
        ;; buffer has now been created with defuns, so eval them!
        (eval-buffer))
      (unless (and (boundp 'ess-debugging) ess-debugging)
        (kill-buffer eval-buf))

      r-versions-created)))

(defvar ess-newest-R nil
  "Stores the newest version of R that has been found.  Used as a cache,
within ess-find-newest-R.  Do not use this value directly, but
instead call the function \\[ess-find-newest-R].")

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
                 ess-rterm-version-paths
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
  (car (ess-get-words-from-vector "as.character(getRversion())\n")))

(defun ess-current-R-at-least (version)
  "Is the version of R (in the ESS buffer) at least (\">=\") VERSION ?
Examples: (ess-current-R-at-least '2.7.0)
      or  (ess-current-R-at-least \"2.5.1\")"
  (ess-make-buffer-current)
  (string= "TRUE"
           (car (ess-get-words-from-vector
                 (format "as.character(getRversion() >= \"%s\")\n" version)))))

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
         (format "(ess-find-rterm): ess-R-root-dir = '%s'\n" ess-R-root-dir))
        ))

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


;;; eldoc

(defun ess-R-eldoc-function ()
  "Return the doc string, or nil.
If an ESS process is not associated with the buffer, do not try
to look up any doc strings."
  (interactive)
  (when (and (ess-process-live-p)
             (not (ess-process-get 'busy)))
    (let ((funname (or (and ess-eldoc-show-on-symbol ;; aggressive completion
                            (symbol-at-point))
                       (car (ess--funname.start)))))
      (when funname
        (let* ((args (ess-function-arguments funname))
               (bargs (cadr args))
               (doc (mapconcat (lambda (el)
                                 (if (equal (car el) "...")
                                     "..."
                                   (concat (car el) "=" (cdr el))))
                               bargs ", "))
               (margs (nth 2 args))
               (W (- (window-width (minibuffer-window)) (+ 4 (length funname))))
               doc1)
          (when doc
            (setq doc (ess-eldoc-docstring-format funname doc))
            (when (and margs (< (length doc1) W))
              (setq doc1 (concat doc (propertize "  || " 'face font-lock-function-name-face)))
              (while (and margs (< (length doc1) W))
                (let ((head (pop margs)))
                  (unless (assoc head bargs)
                    (setq doc doc1
                          doc1 (concat doc1 head  "=, ")))))
              (when (equal (substring doc -2) ", ")
                (setq doc (substring doc 0 -2)))
              (when (and margs (< (length doc) W))
                (setq doc (concat doc " {--}"))))
            doc))))))

(defun ess-eldoc-docstring-format (funname doc)
  (save-match-data
    (let* (;; (name (symbol-name sym))
           (truncate (or  (not (eq t eldoc-echo-area-use-multiline-p))
                          (eq ess-eldoc-abbreviation-style 'aggressive)))
           ;; Subtract 1 from window width since will cause a wraparound and
           ;; resize of the echo area.
           (W (1- (- (window-width (minibuffer-window))
                     (+ 2 (length funname)))))
           newdoc
           )
      (setq doc
            (if (or (<= (length doc) W)
                    (null ess-eldoc-abbreviation-style)
                    (eq 'none ess-eldoc-abbreviation-style))
                doc
              ;;MILD filter
              (setq doc (replace-regexp-in-string "TRUE" "T" doc))
              (setq doc (replace-regexp-in-string "FALSE" "F" doc))
              (if (or (<= (length doc) W)
                      (eq 'mild ess-eldoc-abbreviation-style))
                  doc
                ;;NORMAL filter (deal with long defaults)
                (setq doc (replace-regexp-in-string
                           ;; function calls inside default docs foo(xxxx{..})
                           "([^)]\\{8\\}\\([^)]\\{4,\\}\\))"
                           "{.}" doc nil nil 1))
                (if (<= (length doc) W)
                    doc
                  (setq doc (replace-regexp-in-string
                             " +[^ \t=,\"\]+=[^ \t]\\{10\\}\\([^ \t]\\{4,\\}\\)\\(,\\|\\'\\)"
                             "{.}," doc nil nil 1))
                  (if (<= (length doc) W)
                      doc
                    (setq doc (replace-regexp-in-string
                               " +[^ \t=,\"]+=\\([^ \t]\\{10,\\}\\)\\(,\\|\\'\\)"
                               "{.}," doc nil nil 1))
                    (if (or (<= (length doc) W)
                            (eq 'normal ess-eldoc-abbreviation-style))
                        doc
                      ;;STRONG filter (replace defaults)
                      (setq doc (replace-regexp-in-string
                                 " *[^ \t=,\"\\]* = \\([^ \t]\\{4,\\}\\)\\(,\\|\\'\\)"
                                 "{.}," doc nil nil 1))
                      (if (<= (length doc) W)
                          doc
                        (setq doc (replace-regexp-in-string
                                   "\\(=[^FT0-9].+?\\)\\(, [^ =,\"\\]+=\\|\\'\\)"
                                   "" doc nil nil 1))
                        (setq doc (replace-regexp-in-string
                                   "\\(=[^FT0-9].+?\\)\\(, [^ =,\"\\]+,\\|\\'\\)"
                                   "" doc nil nil 1))
                        (if (or (<= (length doc) W)
                                (eq 'strong ess-eldoc-abbreviation-style))
                            doc
                          ;;AGGRESSIVE filter (truncate what is left)
                          (concat (substring doc 0 (- W 4)) "{--}")))))))))
      (when (and truncate
                 (> (length doc) W))
        (setq doc (concat (substring doc 0 (- W 4)) "{--}")))
      (format "%s: %s" (propertize funname 'face 'font-lock-function-name-face) doc))))

;;; function argument completions

;; (defconst ess--funname-ignore '("else"))
;; (defvar ess-objects-never-recache '("print" "plot")
;;   "List of functions of whose arguments to be cashed only once per session.")


(defvar ess-R--funargs-pre-cache
  '(("plot"
     (("graphics")
      (("x" . "")    ("y" . "NULL")    ("type" . "p")    ("xlim" . "NULL")    ("ylim" . "NULL")    ("log" . "")    ("main" . "NULL")    ("sub" . "NULL")    ("xlab" . "NULL")    ("ylab" . "NULL")
       ("ann" . "par(\"ann\")")     ("axes" . "TRUE")    ("frame.plot" . "axes")    ("panel.first" . "NULL")    ("panel.last" . "NULL")    ("asp" . "NA")    ("..." . ""))
      ("x" "y" "..." "ci" "type" "xlab" "ylab" "ylim" "main" "ci.col" "ci.type" "max.mfrow" "ask" "mar" "oma" "mgp" "xpd" "cex.main" "verbose" "scale" "xlim" "log" "sub" "ann" "axes" "frame.plot" "panel.first" "panel.last" "asp" "center" "edge.root" "nodePar" "edgePar" "leaflab" "dLeaf" "xaxt" "yaxt" "horiz" "zero.line" "verticals" "col.01line" "pch" "legend.text" "formula" "data" "subset" "to" "from" "newpage" "vp" "labels" "hang" "freq" "density" "angle" "col" "border" "lty" "add" "predicted.values" "intervals" "separator" "col.predicted" "col.intervals" "col.separator" "lty.predicted" "lty.intervals" "lty.separator" "plot.type" "main2" "par.fit" "grid" "panel" "cex" "dimen" "abbrev" "which" "caption" "sub.caption" "id.n" "labels.id" "cex.id" "qqline" "cook.levels" "add.smooth" "label.pos" "cex.caption" "rows" "levels" "conf" "absVal" "ci.lty" "xval" "do.points" "col.points" "cex.points" "col.hor" "col.vert" "lwd" "set.pars" "range.bars" "col.range" "xy.labels" "xy.lines" "nc" "yax.flip" "mar.multi" "oma.multi")))
    ("print"
     (("base")
      (("x" . "")    ("digits" . "NULL")    ("quote" . "TRUE")    ("na.print" . "NULL")    ("print.gap" . "NULL")    ("right" . "FALSE")    ("max" . "NULL")    ("useSource" . "TRUE")    ("..." . ""))
      ("x" "..." "digits" "signif.stars" "intercept" "tol" "se" "sort" "verbose" "indent" "style" ".bibstyle" "prefix" "vsep" "minlevel" "quote" "right" "row.names" "max" "na.print" "print.gap" "useSource" "diag" "upper" "justify" "title" "max.levels" "width" "steps" "showEnv" "newpage" "vp" "cutoff" "max.level" "give.attr" "units" "abbrCollate" "print.x" "deparse" "locale" "symbolic.cor" "loadings" "zero.print" "calendar")))
    )
  "Alist of cached arguments for very time consuming functions.")


;; (defun ess-get-object-at-point ()
;;   "A very permissive version of symbol-at-point.
;; Suitable for R object's names."
;;   (let ((delim "[-+ ,\"\t\n\\*/()%{}:]"))
;;     (unless (and (looking-back delim)
;;                  (looking-at   delim))
;;       (save-excursion
;;         (let ((beg (re-search-backward delim nil t)))
;;           (setq beg (or (and beg (goto-char (1+ beg)))
;;                         (goto-char (point-min))))
;;           (unless (re-search-forward delim nil t)
;;             (goto-char (point-max)))
;;           (buffer-substring-no-properties beg (1- (point))))
;;         ))))



(defun ess-R-get-rcompletions (&optional start end)
  "Call R internal completion utilities (rcomp) for possible completions.
Needs version of R>2.7.0

Optional START and END delimit the entity to complete, default to bol and point.

First element of a returned list is the completion token.
"
  (let* ((start (or start
                    (save-excursion (comint-bol nil) (point))))
         (end (or end (point)))
         ;; (opts1 (if no-args "op<-rc.options(args=FALSE)" ""))
         ;; (opts2 (if no-args "rc.options(op)" ""))
         (comm (format ".ess_get_completions(\"%s\", %d)\n"
                (ess-quote-special-chars (buffer-substring start end))
                (- end start))))
    (ess-get-words-from-vector comm)))



(defun ess-R-complete-object-name ()
  "Completion in R via R's completion utilities (formerly 'rcompgen').
To be used instead of ESS' completion engine for R versions >= 2.7.0."
  (interactive)
  (let ((possible-completions (ess-R-get-rcompletions))
        token-string)
    ;; If there are no possible-completions, should return nil, so
    ;; that when this function is called from
    ;; comint-dynamic-complete-functions, other functions can also be
    ;; tried.
    (when possible-completions
      (setq token-string (pop possible-completions))
      (or (comint-dynamic-simple-complete token-string
                                          possible-completions)
          'none))))

;;; auto-complete integration http://cx4a.org/software/auto-complete/index.html
(defvar ac-source-R
  '((prefix     . ess-ac-start)
    ;; (requires   . 0) ::)
    (candidates . ess-ac-candidates)
    (document   . ess-ac-help)
    ;; (action  . ess-ac-action-args) ;; interfere with ac-fallback mechanism on RET (which is extremely annoing in inferior buffers)
    )
  "Combined ad-completion source for R function arguments and R objects")

(defun ess-ac-start ()
  (when (and ess-local-process-name
             (get-process ess-local-process-name))
    (or (ess-ac-start-args)
        (ess-ac-start-objects))))

(defun ess-ac-candidates ()
  "OBJECTS + ARGS"
  (let ((args (ess-ac-args)))
    ;; sort of intrusive but right
    (if (< (length ac-prefix) ac-auto-start)
        args
      (if args
          (append args (ess-ac-objects t))
        (ess-ac-objects)))))

(defun ess-ac-help (sym)
  (if (string-match-p "= *\\'" sym)
      (ess-ac-help-arg sym)
    (ess-ac-help-object sym)))

;; OBJECTS
(defvar  ac-source-R-objects
  '((prefix     . ess-ac-start-objects)
    ;; (requires   . 2)
    (candidates . ess-ac-objects)
    (document   . ess-ac-help-object))
  "Auto-completion source for R objects")

(defun ess-ac-objects (&optional no-kill)
  "Get all cached objects"
 (let ((aprf ac-prefix))
   (when aprf
     (unless no-kill ;; workaround
       (kill-local-variable 'ac-use-comphist))
     (if (string-match-p "[]:$@[]" aprf)
         ;; call proc for objects
         (cdr (ess-R-get-rcompletions ac-point))
       ;; else, get the (maybe cached) list of objects
       (with-ess-process-buffer 'no-error ;; use proc buf alist
         (ess-when-new-input last-objlist-update
           (if (and ess-sl-modtime-alist
                    (not  (process-get *proc* 'sp-for-ac-changed?)))
               ;; not changes, re-read .GlobalEnv
               (ess-extract-onames-from-alist ess-sl-modtime-alist 1 'force))
           ;; reread all objects, but not rda, much faster and not needed anyways
           (ess-get-modtime-list)
           (process-put *proc* 'sp-for-ac-changed? nil))
         (apply 'append (mapcar 'cddr ess-sl-modtime-alist)))))))


(defun ess-ac-start-objects ()
  "Get initial position for objects completion."
  (let ((beg (car (bounds-of-thing-at-point 'symbol))))
    (when (and beg (not (save-excursion (goto-char beg)
                                        (looking-at "/\\|.[0-9]"))))
      beg)))

;; (defun ess-ac-start-objects ()
;;   "Get initial position for objects completion."
;;   (let ((chars "A-Za-z0-9.$@_:")
;;         (bad-start-regexp "/\\|.[0-9]") ;; don't use this source if this is the starting string
;;         )
;;     (when (string-match-p  (format "[%s]" chars) (char-to-string (char-before)))
;;       (save-excursion
;;         (when (re-search-backward (format "[^%s]" chars) nil t)
;;           (unless (looking-at bad-start-regexp)
;;             (1+ (point)))
;;           )))))

(defun ess-ac-help-object (sym)
  "Help string for ac."
  (let ((buf (get-buffer-create " *ess-command-output*")))
    (when (string-match ":+\\(.*\\)" sym)
      (setq sym (match-string 1 sym)))
    (ess-with-current-buffer buf
      (ess--flush-help-into-current-buffer sym))
    (with-current-buffer buf
      (ess-help-underline)
      (goto-char (point-min))
      (buffer-string))))

;; ARGS
(defvar  ac-source-R-args
  '((prefix     . ess-ac-start-args)
    ;; (requires   . 0)
    (candidates . ess-ac-args)
    (document   . ess-ac-help-arg)
    ;; (action     . ess-ac-action-args)
    )
  "Auto-completion source for R function arguments")

(defun ess-ac-start-args ()
  "Get initial position for args completion"
  (when (and ess-local-process-name
             (not (eq (get-text-property (point) 'face) 'font-lock-string-face)))
    (when (ess--funname.start)
      (if (looking-back "[(,]+[ \t\n]*")
          (point)
        (ess-ac-start-objects)))))

(defun ess-ac-args ()
  "Get the args of the function when inside parentheses."
  (when  ess--funname.start ;; stored by a coll to ess-ac-start-args
    (let ((args (nth 2 (ess-function-arguments (car ess--funname.start))))
          (len (length ac-prefix)))
      (if args
          (set (make-local-variable 'ac-use-comphist) nil)
        (kill-local-variable 'ac-use-comphist))
      (delete "..." args)
      (mapcar (lambda (a) (concat a ess-ac-R-argument-suffix))
              args))))

;; (defun ess-ac-action-args ()
;;   (when (looking-back "=")
;;     (delete-char -1)
;;     (insert " = ")))


(defun ess-ac-help-arg (sym)
  "Help string for ac."
  (setq sym (replace-regexp-in-string " *= *\\'" "" sym))
  (let ((buff (get-buffer-create " *ess-command-output*"))
        (fun (car ess--funname.start))
        doc)
    (ess-command (format ess--ac-help-arg-command sym fun) buff)
    (with-current-buffer buff
      (goto-char (point-min))
      (forward-line)
      (setq doc (buffer-substring-no-properties (point) (point-max))))))


(defvar ess--ac-help-arg-command
  "
getArgHelp <- function(arg, func=NULL){
    olderr <- getOption('error')
    options(error=NULL)
    on.exit(options(error=olderr))
    fguess <-
        if(is.null(func)) get('fguess', envir=utils:::.CompletionEnv)
        else func
    findArgHelp <- function(fun, arg){
        file <- help(fun, try.all.packages=FALSE)[[1]]
        hlp <- utils:::.getHelpFile(file)
        id <- grep('arguments', tools:::RdTags(hlp), fixed=TRUE)
        if(length(id)){
            arg_section <- hlp[[id[[1L]]]]
            items <- grep('item', tools:::RdTags(arg_section), fixed=TRUE)
            ## cat('items:', items, fill=TRUE)
            if(length(items)){
                arg_section <- arg_section[items]
                args <- unlist(lapply(arg_section,
                                      function(el) paste(unlist(el[[1]][[1]], TRUE, FALSE), collapse='')))
                fits <- grep(arg, args, fixed=TRUE)
                ## cat('args', args, 'fits', fill=TRUE)
                if(length(fits))
                    paste(unlist(arg_section[[fits[1L]]][[2]], TRUE, FALSE), collapse='')
             }
        }
    }
    funcs <- c(fguess, tryCatch(methods(fguess),
                                warning=function(w) {NULL},
                                error=function(e) {NULL}))
    if(length(funcs) > 1 && length(pos <- grep('default', funcs))){
        funcs <- c(funcs[[pos[[1L]]]], funcs[-pos[[1L]]])
    }
    i <- 1L; found <- FALSE
    out <- 'No help found'
    while(i <= length(funcs) && is.null(out <-
            tryCatch(findArgHelp(funcs[[i]], arg),
                     warning=function(w) {NULL},
                     error=function(e) {NULL})
            ))
        i <- i + 1L
    cat(' \n\n', as.character(out), '\n\n')
}; getArgHelp('%s','%s')
")


;;;### autoload
(defun Rnw-mode ()
  "Major mode for editing Sweave(R) source.
See `ess-noweb-mode' and `R-mode' for more help."
  (interactive)
  (require 'ess-noweb);; << probably someplace else
  (setq ess--make-local-vars-permenent t)
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

;; From: Sebastian Luque <spluque@gmail.com>
;; To: ess-help@stat.math.ethz.ch
;; Date: Mon, 01 May 2006 19:17:49 -0500

;; Without knowing how to tell R to use w3m from within Emacs, and after
;; switching to Konqueror's window for the millionth time, I wrote the
;; following function:

;; This emulates some of the functionality of RSiteSearch() and tests ok in
;; my system GNU Emacs 22.0.50.1 (i486-pc-linux-gnu, X toolkit, Xaw3d scroll
;; bars) of 2006-04-27 on pacem, modified by Debian.  This has the benefit of
;; displaying results with whatever you've told browse-url to use; in my
;; case, w3m with the emacs-w3m package.

;; My elisp skills are rather poor, so comments and suggestions for
;; improvement are welcome.
;; --
;; Seb


;; MM _FIXME_: This only works correctly for  Emacs 22.0.50 (alpha)
;;             for 21.x it has problems in the (completing-read-multiple .)
;;             at the end
(defun R-site-search (string)
  "Search the R archives for STRING, using default criteria.  If
called with a prefix, options are available for
  1) matches per page,
  2) sections of the archives to search (separated by value of `crm-default-separator'),
  3) for displaying results in long or short formats, and
  4) for sorting by any given field.
Completion is available for supplying options."
  (interactive "sSearch string: ")
  (let ((site "http://search.r-project.org/cgi-bin/namazu.cgi?query=")
        (okstring (replace-regexp-in-string " +" "+" string)))
    (if current-prefix-arg
        (let ((mpp (concat
                    "&max="
                    (completing-read
                     "Matches per page: "
                     '(("20" 1) ("30" 2) ("40" 3) ("50" 4) ("100" 5)))))
              (format (concat
                       "&result="
                       (completing-read
                        "Format: " '("normal" "short")
                        nil t "normal" nil "normal")))
              (sortby (concat
                       "&sort="
                       (completing-read
                        "Sort by: "
                        '(("score" 1) ("date:late" 2) ("date:early" 3)
                          ("field:subject:ascending" 4)
                          ("field:subject:decending" 5)
                          ("field:from:ascending" 6) ("field:from:decending" 7)
                          ("field:size:ascending" 8) ("field:size:decending" 9))
                        nil t "score" nil "score")))
              (restrict (concat
                         "&idxname="
                         (mapconcat
                          'identity
                          (completing-read-multiple
                           "Limit search to: "
                           '(("Rhelp02a" 1) ("functions" 2) ("docs" 3)
                             ("Rhelp01" 4))
                           nil t "Rhelp02a,functions,docs" nil
                           "Rhelp02a,functions,docs") "&idxname="))))
          (browse-url (concat site okstring mpp format sortby restrict)))
      ;; else: without prefix use defaults:
      (browse-url (concat site okstring "&max=20&result=normal&sort=score"
                          "&idxname=Rhelp02a&idxname=functions&idxname=docs")))))

(defvar ess--packages-cache nil
  "Cache var to store package names. Used by
  `ess-install.packages'.")



(defun ess-R-install.packages (&optional update)
  "Prompt and install R package. With argument, update cached packages list."
  (interactive "P")
  (when (equal "@CRAN@" (car (ess-get-words-from-vector "getOption('repos')[['CRAN']]\n")))
    (ess-setCRANMiror)
    (ess-wait-for-process (get-process ess-current-process-name))
    (setq update t))
  (when (or update
            (not ess--packages-cache))
    (message "Fetching R packages ... ")
    (setq ess--packages-cache
          (ess-get-words-from-vector "print(rownames(available.packages()), max=1e6)\n")))
  (let ((ess-eval-visibly-p t)
        pack)
    (setq pack (ess-completing-read "Package to install" ess--packages-cache))
    (process-send-string (get-process ess-current-process-name)
                         (format "install.packages('%s')\n" pack))
    (display-buffer (buffer-name (process-buffer (get-process ess-current-process-name))))
    ))

(define-obsolete-function-alias 'ess-install.packages 'ess-R-install.packages "ESS[12.09-1]")

(defun ess-install-library ()
  "Install library/package for current dialect.
Currently works only for R."
  (interactive)
  (if (not (string-match "^R" ess-dialect))
      (message "Sorry, not available for %s" ess-dialect)
    (ess-R-install.packages)))


(defun ess-setRepositories ()
  "Call setRepositories()"
  (interactive)
  (if (not (string-match "^R" ess-dialect))
      (message "Sorry, not available for %s" ess-dialect)
    (ess-eval-linewise "setRepositories(FALSE)\n")
    ))

(defun ess-setCRANMiror ()
  "Set cran mirror"
  (interactive)
  (let* ((M1 (ess-get-words-from-vector "local({out <- getCRANmirrors(); print(paste(out$Name,'[',out$URL,']', sep=''))})\n"))
         (M2 (mapcar (lambda (el)
                       (string-match "\\(.*\\)\\[\\(.*\\)\\]$" el)
                       (propertize (match-string 1 el) 'URL (match-string 2 el)))
                     M1))
         (opt  (ess-completing-read "Choose CRAN mirror" M2 nil t)))
    (when opt
      (setq opt (get-text-property 0 'URL opt))
      (ess-command
       (format "local({r <- getOption('repos'); r['CRAN'] <- '%s';options(repos=r)})\n" opt))
      (message "New CHRAN mirror: %s" (car (ess-get-words-from-vector "getOption('repos')[['CRAN']]\n")))
      )))

(defun ess-R-sos (cmd)
  "Interface to findFn in the library sos."
                                        ;(interactive (list (read-from-minibuffer "Web search for:" nil nil t nil (current-word))))
  (interactive  "sfindFn: ")
  (unless (equal "TRUE" (car (ess-get-words-from-vector "as.character(suppressPackageStartupMessages(require(sos)))\n")))
    (if (y-or-n-p "Library 'sos' is not installed. Install? ")
        (progn (ess-eval-linewise "install.packages('sos')\n")
               (ess-eval-linewise "library(sos)\n"))
      (signal 'quit nil)))
  (message nil)
  (ess-eval-linewise (format "findFn(\"%s\", maxPages=10)" cmd))
  )

(define-obsolete-function-alias 'ess-sos 'ess-R-sos "ESS[12.09-1]")


(defun ess-load-library ()
  "Prompt and load dialect specific library/package/module.

Note that add-on code in R are called 'packages' and the name of
this function has nothing to do with R package mechanism, but it
rather serves a generic, dialect independent purpose. It is also
similar to `load-library' emacs function.
"
  (interactive)
  (if (not (string-match "^R" ess-dialect))
      (message "Sorry, not available for %s" ess-dialect)
    (let ((ess-eval-visibly-p t)
          (packs (ess-get-words-from-vector "print(.packages(T), max=1e6)\n"))
          pack)
      (setq pack (ess-completing-read "Load" packs))
      (ess-eval-linewise (format "library('%s')\n" pack))
      (ess--mark-search-list-as-changed)
      (display-buffer (buffer-name (process-buffer (get-process ess-current-process-name))))
      )))

(define-obsolete-function-alias 'ess-library 'ess-load-library "ESS[12.09-1]")

 ; provides

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
