;;; ess-developer.el --- Developer mode for R.

;; Copyright (C) 2011-2015 V. Spinu, A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Vitalie Spinu
;; Created: 12-11-2011
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages, tools

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

;; see apropriate documentation section of ESS user manual

;;; Code:


(defgroup ess-developer nil
  "ESS: developer."
  :group 'ess
  :prefix "ess-developer-")

(defface ess-developer-indicator-face
  '((((class grayscale)) (:background "DimGray"))
    (((class color) (background light))
     (:foreground "red4"  :bold t ))
    (((class color) (background dark))
     (:foreground "deep sky blue"  :bold t )))
  "Face to highlight mode line process name when developer mode is on."
  :group 'ess-developer)

(defcustom ess-r-source-environment-in-packages nil
  "If non-nil, `ess-r-source-environment' is automatically
set within R packages."
  :group 'ess-developer
  :type 'boolean)

(defcustom ess-developer-load-command "library('%n')"
  "Loading command for `ess-developer-add-package'. Can be a
string containing a R command with:

  %n to be replaced by the package name,
  %d to be replaced by the package directory.

Alternatively, can be a quoted Emacs function name such as
`ess-r-devtools-load-package'.

See also `ess-developer-load-package' for related functionality."
  :group 'ess-developer
  :type 'alist)

(defvar ess-developer-package nil
  "Current package.
Cons cell of two strings. CAR is the package name active in the
current buffer. CDR is the path to its source directory.")
(make-variable-buffer-local 'ess-developer-package)

(defvar ess-r-source-environment nil
  "Package name where source code should be injected. If set to
the string \"*current*\", code is sourced into the current
environment. This is useful for step-debugging.")
(make-variable-buffer-local 'ess-r-source-environment)

(defvar ess-developer-library-path nil
  "Default path to find packages.")

(defvar ess-developer-root-file "DESCRIPTION"
  "Presence of this file indicates the project's root.")

(defvar ess-developer-check-all-dirs t
  "If non-nil, the whole hierarchy of directories will be checked for an R package.
This can be slow on remotes. When nil, only typical folders of R
packages are checked (such as `R', `man', `src', etc).")

(defvar ess-developer-package-dirs
  '(("R"        . 1)
    ("r"        . 1)
    ("tests"    . 1)
    ("testthat" . 2)
    ("inst"     . 1)
    ("include"  . 2)
    ("src"      . 1))
  "Alist of directories names and their depth in R package hierarchy.
When `ess-developer-check-all-dirs' is nil, this list is used to
figure out whether the current file belongs to an R package. If
the file specified in `ess-developer-root-file'
(DESCRIPTION by default) is found at the presumed root directory
of the package, the current directory is considered to be part of
a R package.")

(defun ess-developer-load-package ()
  (let* ((pkg-info (ess-developer-current-package-info))
         (cmd (if (stringp ess-developer-load-command)
                  (replace-regexp-in-string "%n" (car pkg-info)
                   (replace-regexp-in-string "%d" (cdr pkg-info)
                    ess-developer-load-command))
                ess-developer-load-command)))
    (cond ((stringp cmd)
           (ess-eval-linewise (concat cmd "\n")))
          ((functionp cmd)
           (funcall cmd)))))


;;; Package UI

(defun ess-developer-current-package-info ()
  "Get package info.
Return a cons cell of two strings whose CAR is a package name and
CDR is a package directory. The package is determined by (in this
order) the buffer-local value of `ess-developer-package',
whether the current file is part of a package, or the value of
`ess-developer-package' in the attached process buffer."
  (or ess-developer-package
      (ess-developer--local-package-info)
      (ess-developer--process-package-info)))

(defun ess-developer--select-package-name (&optional attached-only)
  (ess-force-buffer-current)
  (let ((pkgs (ess-get-words-from-vector
               (format "print(.packages(%s), max = 1e6)\n"
                       (if attached-only "FALSE" "TRUE"))))
        (current-pkg (car (ess-developer-current-package-info))))
    (ess-completing-read "Package: " pkgs nil nil nil nil current-pkg)))

(defun ess-developer-select-package ()
  "Select a package for ESS developer functions.

The package metadata will be written in the file-local variables
section."
  (interactive)
  (let* ((pkg-path (read-directory-name
                    "Path: " (or (ess-developer--find-package-path)
                                 ess-developer-library-path)
                    nil t))
         (pkg-name (ess-developer--find-package-name pkg-path))
         (pkg-info (cons pkg-name pkg-path)))
    (unless (and pkg-name pkg-path
                 (file-exists-p (expand-file-name ess-developer-root-file pkg-path)))
      (error "Not a valid package. No '%s' found in `%s'." ess-developer-root-file pkg-path))
    (message (format "%s selected and added to file-local variables" pkg-name))
    (save-excursion
      (add-file-local-variable 'ess-developer-package pkg-info))
    (setq-local ess-developer-package pkg-info)))

(defun ess-r-set-source-environment (&optional attached-only)
  "Select a package or the current environment where code should
be sourced. Repeated invocations of this command will switch
between package selection and the current environment.

If ATTACHED-ONLY is non-nil, prompt only for attached packages."
  (interactive)
  ;; FIXME: Need better switching
  (cond ((string= ess-r-source-environment "*current*")
         (let ((pkg-name (ess-developer--select-package-name attached-only)))
           (setq-local ess-r-source-environment pkg-name)
           (message (format "Injecting code in %s" pkg-name))))
        (t
         (setq-local ess-r-source-environment "*current*")
         (message "Injecting code in *current*"))))

(defun ess-developer-activate-injection-in-package ()
  (when ess-r-source-environment-in-packages
    (setq-local ess-r-source-environment
                (car (ess-developer-current-package-info)))))

(add-hook 'R-mode-hook 'ess-developer-activate-injection-in-package)

(defun ess-developer-send-process (command &optional msg alt)
  (ess-force-buffer-current)
  (let* ((pkg-info (or (ess-developer-current-package-info)
                       (ess-developer-select-package)))
         (name (car pkg-info))
         (path (concat "'" (cdr pkg-info) "'"))
         (alt (cond ((stringp alt) alt)
                    (alt "")))
         (args (when alt
                 (read-string "Arguments: " alt)))
         (args (unless (or (null args)
                           (string= "" args))
                 (concat ", " args))))
    (message msg name)
    (ess-developer--update-process-local-pkg pkg-info)
    (ess-eval-linewise (format command (concat path args)))))



;;; Package Detection

(defun ess-developer--local-package-info ()
  "Parses DESCRIPTION file in PATH (R specific so far). PATH
defaults to the value returned by
`ess-developer--find-package-path'."
  (let ((pkg-path (ess-developer--find-package-path)))
    (when pkg-path
      (cons (ess-developer--find-package-name pkg-path) pkg-path))))

(defun ess-developer--find-package-path ()
  "Get the root of R package that contains current directory.
Root is determined by locating `ess-developer-root-file'.

If PKG-NAME is given, check that the path found corresponds to
that package.

or if the variable
`ess-developer-package' is locally defined with a cons cell
of the form `(name . path)', iterate over default-directories of
all open R files until the package is found. If not found, return
nil."
  (let* ((path (or (file-name-directory (buffer-file-name))
                   default-directory))
         (current-dir (file-name-nondirectory (directory-file-name path)))
         known-pkg-dir known-path found-path)
    (cond
     ;; First check current directory
     ((file-exists-p (expand-file-name ess-developer-root-file path))
      (setq found-path path))
     ;; Check for known directories in current path
     ((while (and (not (string= path "/"))
                  (not found-path))
        (setq current-dir (file-name-nondirectory (directory-file-name path)))
        (if (and (setq known-pkg-dir (assoc current-dir ess-developer-package-dirs))
                 (setq known-path (ess-climb-path path (cdr known-pkg-dir)))
                 (file-exists-p (expand-file-name ess-developer-root-file known-path)))
            (setq found-path known-path)
          (setq path (ess-climb-path path 1))))))
    found-path))

(defun ess-climb-path (path n)
  "Takes PATH, climbs its hierarchy N times, and returns the new path."
  (dotimes (i n)
    (setq path (file-name-directory (directory-file-name path))))
  path)

(defun ess-developer--find-package-name (path)
  (let ((file (expand-file-name ess-developer-root-file path))
        (case-fold-search t))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (re-search-forward "package: \\(.*\\)")
        (match-string 1)))))

(defun ess-developer--process-package-info ()
  (with-ess-process-buffer t
    (bound-and-true-p ess-developer-package)))

(defun ess-developer--update-process-local-pkg (pkg-info)
  (with-ess-process-buffer nil
    (setq-local ess-developer-package pkg-info)))



;;; Code Injection

(defun ess-developer--get-injection-package (&optional ask)
  (cond (ess-r-source-environment)
        (ask
         (ess-r-set-source-environment))
        (t
         error "Code injection is not active")))

(defun ess-developer-send-region-fallback (proc beg end visibly &optional message tracebug func)
  (if tracebug
      (ess-tracebug-send-region proc beg end visibly message t)
    (ess-send-region proc beg end visibly message)))

(defun ess-developer-source-current-file (&optional filename)
  "Ask for namespace to source the current file into.
If *current* is selected just invoke source('file_name'),
otherwise call devSource."
  (interactive)
  (ess-force-buffer-current "R process to use: ")
  (let* ((filename (or filename buffer-file-name
                       (error "Buffer '%s' doesn't visit a file"
                              (buffer-name (current-buffer)))))
         (file (file-name-nondirectory filename))
         (env (ess-developer--get-injection-package))
         (cmd  (if (equal env "*current*")
                   (format "source(file = '%s', local = TRUE)\n cat(\"Sourced file '%s' into\", capture.output(environment()), '\n')" filename file)
                 (format ".essDev_source(source = '%s',package = '%s')" filename env))))
    (when (buffer-modified-p) (save-buffer))
    (message "devSourcing '%s' in %s ..." file env)
    (ess-developer--command cmd 'ess-developer--propertize-output)))

(defun ess-developer--exists-in-ns (var ns)
  ;; If namespace does not exist, the R code below throw an error. But
  ;; that's equivalent to FALSE.
  (let ((cmd "as.character(exists('%s', envir=asNamespace('%s'), mode='function', inherits=FALSE))\n"))
    (ess-boolean-command
     (format cmd var ns))))

(defun ess-developer-send-function (proc beg end name &optional visibly message tracebug)
  (when (null name)
    (error "Oops, could not find function name (probably a regexp bug)"))
  (save-excursion
    (let ((pkg-name (ess-developer--get-injection-package)))
      (when tracebug (ess-tracebug-set-last-input proc))
      (cond
       ;; if setMethod, setClass etc, do send region
       ((string-match-p ess-set-function-start (concat name "("))
        (ess-developer-send-region proc beg end visibly message tracebug))
       (pkg-name
        (ess-developer-devSource beg end pkg-name message))
       ;; last resort - assign in current env
       (t
        (ess-developer-send-region-fallback proc beg end visibly message tracebug))))))

(defun ess-developer-send-region (proc beg end &optional visibly message tracebug)
  "Ask for for the package and devSource region into it."
  (let ((pkg-name (ess-developer--get-injection-package 'ask-if-nil)))
    (if (equal pkg-name "*current*")
        (ess-developer-send-region-fallback proc beg end visibly message tracebug)
      ;; Ignore VISIBLY here
      (ess-developer-devSource beg end pkg-name message))
    (when message
      (message (format "dev%s into %s..." message pkg-name)))))

(defun ess-developer-devSource (beg end package &optional message)
  (let* ((ess-eval-command
          (format ".essDev.eval('%s', package = '%s', file = '%s')" "%s" package "%f"))
         (ess-eval-visibly-command ess-eval-command)
         (ess-eval-visibly-noecho-command ess-eval-command))
    (ess-developer--command (ess--make-source-refd-command beg end)
                            'ess-developer--propertize-output)
    (when message
      (message (format "dev%s into %s" message package)))))

(defun ess-developer--command (cmd &optional propertize-func)
  "Evaluate the command and popup a message with the output if succed.
On error  insert the error at the end of the inferior-ess buffer.

PROPERTIZE-FUNC is a function called with the output buffer being
current. usually used to manipulate the output, for example to
propertize output text.
"
  (setq cmd (format "eval({cat(\"\\n\")\n%s\ncat(\"!@OK@!\")})\n" cmd))
  (let ((buff (get-buffer-create " *ess-command-output*"))
        out)
    (ess-command cmd buff nil nil 0.1)
    (with-current-buffer buff
      (goto-char (point-min))
      (delete-region (point) (min (point-max) ;; delete + + +
                                  (1+ (point-at-eol))))
      (goto-char (point-max))
      (if (re-search-backward "!@OK@!" nil t)
          (progn
            (when (fboundp propertize-func)
              (save-excursion (funcall propertize-func)))
            (message "%s" (buffer-substring (point-min) (max (point-min)
                                                             (1- (point))))))
        (message "%s" (buffer-substring-no-properties (point-min) (point-max)))))))

(defun ess-developer--propertize-output ()
  (goto-char (point-min))
  (while (re-search-forward "\\(FUN\\|CLS\\|METH\\)\\[" nil t)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face 'font-lock-function-name-face))
  (goto-char (point-min))
  (while (re-search-forward "\\([^ \t]+\\):" nil t)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face 'font-lock-keyword-face)))



;;; Devtools Integration

(defun ess-r-devtools-load-package (&optional alt)
  "Interface for `devtools::load_all()'."
  (interactive "P")
  (ess-developer-send-process "devtools::load_all(%s)\n"
                              "Loading %s"
                              (when alt "recompile = TRUE")))

(defun ess-r-devtools-unload-package ()
  "Interface to `devtools::unload()'."
  (interactive)
  (ess-developer-send-process "devtools::unload(%s)\n"
                              "Unloading %s"))

(defun ess-r-devtools-check-package (&optional alt)
  "Interface for `devtools::check()'."
  (interactive "P")
  (ess-developer-send-process "devtools::check(%s)\n"
                              "Testing %s"
                              (when alt "vignettes = FALSE")))

(defun ess-r-devtools-test-package (&optional alt)
  "Interface for `devtools::test()'."
  (interactive "P")
  (ess-developer-send-process "devtools::test(%s)\n"
                              "Testing %s"
                              alt))

(defun ess-r-devtools-revdep-check-package (&optional alt)
  "Interface for `devtools::revdep_check()'."
  (interactive "P")
  (ess-developer-send-process "devtools::revdep_check(%s)\n"
                              "Checking reverse dependencies of %s"
                              alt))

(defun ess-r-devtools-document-package (&optional alt)
  "Interface for `devtools::document()'."
  (interactive "P")
  (ess-developer-send-process "devtools::document(%s)\n"
                              "Documenting %s"
                              alt))

(defun ess-r-devtools-install-package (&optional alt)
  "Interface to `devtools::install()'."
  (interactive "P")
  (ess-developer-send-process "devtools::install(%s)\n"
                              "Installing %s"
                              alt))


;;; Minor Mode

(defcustom ess-developer-activate-in-package t
  "If non-nil, `ess-developer-mode' is automatically turned on
within R packages."
  :group 'ess-developer
  :type 'boolean)

(defcustom ess-developer-enter-hook nil
  "Normal hook run on entering `ess-developer-mode'."
  :group 'ess-developer
  :type 'hook)

(defcustom ess-developer-exit-hook nil
  "Normal hook run on exiting `ess-developer-mode'."
  :group 'ess-developer
  :type 'hook)

(defcustom ess-developer-mode-line
  '(:eval (let ((pkg-info (ess-developer-current-package-info)))
            (when pkg-info
                (format " [%s]" (car pkg-info)))))
  "Mode line for ESS developer. Set this variable to nil to
disable the mode line entirely."
  :group 'ess-developer
  :type 'sexp
  :risky t)

(define-minor-mode ess-developer-mode
  "Minor mode enabling developer-specific features for working
with R."
  :init-value nil
  :lighter ess-developer-mode-line
  (cond
   (ess-developer-mode
    (run-hooks 'ess-developer-enter-hook))
   (t
    (run-hooks 'ess-developer-exit-hook))))

(add-hook 'R-mode-hook 'ess-developer-activate-in-package)

(defun ess-developer-activate-in-package (&optional package all)
  "Activate developer if current file is part of a package.

If PACKAGE is given, activate only if current file is part of the
PACKAGE.

If ALL is non-nil, perform activation in all R buffers.

This function does nothing if `ess-developer-activate-in-package'
is nil."
  (when ess-developer-activate-in-package
    (if all
        (dolist (bf (buffer-list))
          (with-current-buffer bf
            (ess-developer-activate-in-package package)))
      (let ((pkg-info (ess-developer-current-package-info)))
        (when (and pkg-info
                   (or (null package)
                       (equal (car pkg-info) package)))
          (ess-developer-mode 1))))))

(defun ess-developer-deactivate-in-package (&optional package all)
  "Deactivate developer if current file is part of the R package.

If PACKAGE is given, deactivate only if current package is
PACKAGE.

If ALL is non-nil, deactivate in all open R buffers."
  (if all
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (ess-developer-deactivate-in-package package)))
    (let ((pkg-info (ess-developer-current-package-info)))
      (when (and ess-developer-mode
                 (or (null package)
                     (equal (car pkg-info) package)))
        (ess-developer-mode 0)))))


;;; Deprecated variables and functions
(defun ess-developer (&optional val)
  (error "As of ESS 16.03, `ess-developer' is deprecated. Please
use `ess-developer-mode' instead."))

(defalias 'ess-toggle-developer 'ess-developer)

(make-obsolete-variable 'ess-developer "This variable is
deprecated. Please use `ess-developer-select-package' and
`ess-r-set-source-environment' instead." "16.03")

(make-obsolete-variable 'ess-developer-packages "This variable
is deprecated. Please use `ess-developer-select-package' and
`ess-r-set-source-environment' instead." "16.03")

(make-obsolete-variable 'ess-developer-load-on-add-commands "This
variable is deprecated. Please use `ess-developer-select-package'
and `ess-r-set-source-environment' instead." "16.03")


(provide 'ess-developer)
;;; ess-developer.el ends here
