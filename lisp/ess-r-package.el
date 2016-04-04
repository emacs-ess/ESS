;;; ess-r-package.el --- Package development mode for R.

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

(require 'ess-utils)

(defface ess-r-package-indicator-face
  '((((class grayscale)) (:background "DimGray"))
    (((class color) (background light))
     (:foreground "red4"  :bold t ))
    (((class color) (background dark))
     (:foreground "deep sky blue"  :bold t )))
  "Face to highlight mode line process name when developer mode is on."
  :group 'ess-r-package)

(defcustom ess-r-set-source-environment-in-packages nil
  "If non-nil, `ess-r-evaluation-env' is automatically
set within R packages."
  :group 'ess-r-package
  :type 'boolean)

(defcustom ess-r-package-load-command "library('%n')"
  "Loading command for `ess-r-package-add-package'. Can be a
string containing a R command with:

  %n to be replaced by the package name,
  %d to be replaced by the package directory.

Alternatively, can be a quoted Emacs function name such as
`ess-r-devtools-load-package'.

See also `ess-r-package-load-package' for related functionality."
  :group 'ess-r-package
  :type 'alist)

(defvar ess-r-package-info nil
  "Current package.

Cons cell of two strings. CAR is the package name active in the
current buffer. CDR is the path to its source directory.")
(make-variable-buffer-local 'ess-r-package-info)

(defvar ess-r-package-library-path nil
  "Default path to find packages.")

(defvar ess-r-package-root-file "DESCRIPTION"
  "Presence of this file indicates the project's root.")

(defvar ess-r-package-dirs
  '(("R"        . 1)
    ("r"        . 1)
    ("tests"    . 1)
    ("testthat" . 2)
    ("inst"     . 1)
    ("include"  . 2)
    ("src"      . 1))
  "Alist of directories names and their depth in R package hierarchy.
This list is used to figure out whether the current file belongs
to an R package. If the file specified in `ess-r-package-root-file'
(DESCRIPTION by default) is found at the presumed root directory
of the package, the current directory is considered to be part of
a R package.")

(defun ess-r-package-load-package ()
  (let* ((pkg-info (ess-r-package-current-package-info))
         (cmd (if (stringp ess-r-package-load-command)
                  (replace-regexp-in-string "%n" (car pkg-info)
                   (replace-regexp-in-string "%d" (cdr pkg-info)
                    ess-r-package-load-command))
                ess-r-package-load-command)))
    (cond ((stringp cmd)
           (ess-eval-linewise (concat cmd "\n")))
          ((functionp cmd)
           (funcall cmd)))))


;;;*;;; Package UI

(defun ess-r-package-current-package-info ()
  "Get package info.
Return a cons cell of two strings whose CAR is a package name and
CDR is a package directory. The package is determined by (in this
order) the buffer-local value of `ess-r-package-info',
whether the current file is part of a package, or the value of
`ess-r-package-info' in the attached process buffer."
  (or ess-r-package-info
      (ess-r-package--local-package-info)
      (ess-r-package--process-package-info)))

(defun ess-r-package-select-package ()
  "Select a package for ESS developer functions.

The package metadata will be written in the file-local variables
section."
  (interactive)
  (let* ((pkg-path (read-directory-name
                    "Path: " (or (ess-r-package--find-package-path)
                                 ess-r-package-library-path)
                    nil t))
         (pkg-name (ess-r-package--find-package-name pkg-path))
         (pkg-info (cons pkg-name pkg-path)))
    (unless (and pkg-name pkg-path
                 (file-exists-p (expand-file-name ess-r-package-root-file pkg-path)))
      (error "Not a valid package. No '%s' found in `%s'." ess-r-package-root-file pkg-path))
    (message (format "%s selected and added to file-local variables" pkg-name))
    (save-excursion
      (add-file-local-variable 'ess-r-package-info pkg-info))
    (setq-local ess-r-package-info pkg-info)))

(defun ess-r--select-package-name ()
  (ess-force-buffer-current)
  (let ((pkgs (ess-get-words-from-vector
               (format "print(.packages(%s), max = 1e6)\n"
                       (if ess-r-prompt-for-attached-pkgs-only "FALSE" "TRUE"))))
        (current-pkg (car (ess-r-package-current-package-info))))
    (ess-completing-read "Package: " pkgs nil nil nil nil current-pkg)))

(defun ess-r-package-set-namespaced-evaluation ()
  (when ess-r-set-source-environment-in-packages
    (setq-local ess-r-evaluation-env
                (car (ess-r-package-current-package-info)))))

(add-hook 'R-mode-hook 'ess-r-package-set-namespaced-evaluation)

(defun ess-r-package-send-process (command &optional msg alt)
  (ess-force-buffer-current)
  (let* ((pkg-info (or (ess-r-package-current-package-info)
                       (ess-r-package-select-package)))
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
    (ess-r-package--update-process-local-pkg pkg-info)
    (ess-eval-linewise (format command (concat path args)))))


;;;*;;; Package Detection

(defun ess-r-package--local-package-info ()
  "Parses DESCRIPTION file in PATH (R specific so far). PATH
defaults to the value returned by
`ess-r-package--find-package-path'."
  (let ((pkg-path (ess-r-package--find-package-path)))
    (when pkg-path
      (setq-local ess-r-package-info
                  (cons (ess-r-package--find-package-name pkg-path) pkg-path)))))

(defun ess-r-package--find-package-path ()
  "Get the root of R package that contains current directory.
Root is determined by locating `ess-r-package-root-file'.

If PKG-NAME is given, check that the path found corresponds to
that package.

or if the variable
`ess-r-package-info' is locally defined with a cons cell
of the form `(name . path)', iterate over default-directories of
all open R files until the package is found. If not found, return
nil."
  (let* ((path (if (buffer-file-name)
                   (file-name-directory (buffer-file-name))
                 default-directory))
         (current-dir (file-name-nondirectory (directory-file-name path)))
         known-pkg-dir known-path found-path)
    (cond
     ;; First check current directory
     ((file-exists-p (expand-file-name ess-r-package-root-file path))
      (setq found-path path))
     ;; Check for known directories in current path
     ((while (and (not (string= path "/"))
                  (not found-path))
        (setq current-dir (file-name-nondirectory (directory-file-name path)))
        (if (and (setq known-pkg-dir (assoc current-dir ess-r-package-dirs))
                 (setq known-path (ess-climb-path path (cdr known-pkg-dir)))
                 (file-exists-p (expand-file-name ess-r-package-root-file known-path)))
            (setq found-path known-path)
          (setq path (ess-climb-path path 1))))))
    found-path))

(defun ess-climb-path (path n)
  "Takes PATH, climbs its hierarchy N times, and returns the new path."
  (dotimes (i n)
    (setq path (file-name-directory (directory-file-name path))))
  path)

(defun ess-r-package--find-package-name (path)
  (let ((file (expand-file-name ess-r-package-root-file path))
        (case-fold-search t))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (re-search-forward "package: \\(.*\\)")
        (match-string 1)))))

(defun ess-r-package--process-package-info ()
  (with-ess-process-buffer t
    (bound-and-true-p ess-r-package-info)))

(defun ess-r-package--update-process-local-pkg (pkg-info)
  (with-ess-process-buffer nil
    (setq-local ess-r-package-info pkg-info)))


;;;*;;; Devtools Integration

(defun ess-r-devtools-load-package (&optional alt)
  "Interface for `devtools::load_all()'."
  (interactive "P")
  (ess-r-package-send-process "devtools::load_all(%s)\n"
                              "Loading %s"
                              (when alt "recompile = TRUE")))

(defun ess-r-devtools-unload-package ()
  "Interface to `devtools::unload()'."
  (interactive)
  (ess-r-package-send-process "devtools::unload(%s)\n"
                              "Unloading %s"))

(defun ess-r-devtools-check-package (&optional alt)
  "Interface for `devtools::check()'."
  (interactive "P")
  (ess-r-package-send-process "devtools::check(%s)\n"
                              "Testing %s"
                              (when alt "vignettes = FALSE")))

(defun ess-r-devtools-test-package (&optional alt)
  "Interface for `devtools::test()'."
  (interactive "P")
  (ess-r-package-send-process "devtools::test(%s)\n"
                              "Testing %s"
                              alt))

(defvar ess-r-devtools-revdep-check-cmd
  "local({
  pkg_path <- %s
  res <- devtools::revdep_check(pkg_path)

  if (file.exists(file.path(pkg_path, 'revdep'))) {
    save_path <- file.path(pkg_path, 'revdep')
  } else {
    save_path <- file.path(pkg_path, '.metadata', 'revdep')
  }
  devtools::revdep_check_save_summary(res, save_path)

  logs_path <- file.path(save_path, 'logs')
  if (!dir.exists(logs_path)) {
    dir.create(logs_path)
  }
  devtools::revdep_check_save_logs(res, logs_path)
})
")

(defun ess-r-devtools-revdep-check-package (&optional alt)
  "Interface for `devtools::revdep_check()'.

By default, the revdep summary is saved in `pkg_path/revdep' if
the directory exists, or `pkg_path/.metadata/revdep' if it
doesn't exist. The former path is the default in devtools but
will create problem if it isn't escaped in `.Rbuildignore'. On
the other hand, `.metadata' is always ignored by the R package
builder, which makes it a safe directory to store the revdep
checking results."
  (interactive "P")
  (ess-r-package-send-process ess-r-devtools-revdep-check-cmd
                              "Checking reverse dependencies of %s"
                              alt))

(defun ess-r-devtools-document-package (&optional alt)
  "Interface for `devtools::document()'."
  (interactive "P")
  (ess-r-package-send-process "devtools::document(%s)\n"
                              "Documenting %s"
                              alt))

(defun ess-r-devtools-install-package (&optional alt)
  "Interface to `devtools::install()'."
  (interactive "P")
  (ess-r-package-send-process "devtools::install(%s)\n"
                              "Installing %s"
                              alt))


;;;*;;; Minor Mode

(defcustom ess-r-package-activate-in-package t
  "If non-nil, `ess-r-package-mode' is automatically turned on
within R packages."
  :group 'ess-r-package
  :type 'boolean)

(defcustom ess-r-package-enter-hook nil
  "Normal hook run on entering `ess-r-package-mode'."
  :group 'ess-r-package
  :type 'hook)

(defcustom ess-r-package-exit-hook nil
  "Normal hook run on exiting `ess-r-package-mode'."
  :group 'ess-r-package
  :type 'hook)

(defcustom ess-r-package-mode-line
  '(:eval (let* ((pkg-name (car (ess-r-package-current-package-info)))
                 (src (if (string= pkg-name ess-r-evaluation-env)
                          (format "src:")
                        "")))
            (when pkg-name
              (format " [pkg:%s%s]" src pkg-name))))
  "Mode line for ESS developer. Set this variable to nil to
disable the mode line entirely."
  :group 'ess-r-package
  :type 'sexp
  :risky t)

(defvar ess-r-package-mode-map
  (let ((ess-r-package-mode-map (make-sparse-keymap)))
    (define-key ess-r-package-mode-map "\C-c\C-w" 'ess-r-package-dev-map)
    ess-r-package-mode-map))

(define-minor-mode ess-r-package-mode
  "Minor mode enabling developer-specific features for working
with R."
  :init-value nil
  :keymap ess-r-package-mode-map
  :lighter ess-r-package-mode-line
  (cond
   (ess-r-package-mode
    (run-hooks 'ess-r-package-enter-hook))
   (t
    (run-hooks 'ess-r-package-exit-hook))))

(add-hook 'hack-local-variables-hook 'ess-r-package-activate-in-package)

(defun ess-r-package-activate-in-package (&optional package all)
  "Activate developer if current file is part of a package.

If PACKAGE is given, activate only if current file is part of the
PACKAGE.

If ALL is non-nil, perform activation in all R buffers.

This function does nothing if `ess-r-package-activate-in-package'
is nil."
  (when ess-r-package-activate-in-package
    (if all
        (dolist (bf (buffer-list))
          (with-current-buffer bf
            (ess-r-package-activate-in-package package)))
      (let ((pkg-info (ess-r-package-current-package-info)))
        (when (and pkg-info
                   (or (null package)
                       (equal (car pkg-info) package)))
          (ess-r-package-mode 1))))))

(defun ess-r-package-deactivate-in-package (&optional package all)
  "Deactivate developer if current file is part of the R package.

If PACKAGE is given, deactivate only if current package is
PACKAGE.

If ALL is non-nil, deactivate in all open R buffers."
  (if all
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (ess-r-package-deactivate-in-package package)))
    (let ((pkg-info (ess-r-package-current-package-info)))
      (when (and ess-r-package-mode
                 (or (null package)
                     (equal (car pkg-info) package)))
        (ess-r-package-mode 0)))))


;;;*;;; Deprecated variables and functions
(defun ess-developer (&optional val)
  (error "As of ESS 16.03, `ess-developer' is deprecated. Please
use `ess-developer-mode' instead."))

(defalias 'ess-toggle-developer 'ess-developer)

(make-obsolete-variable 'ess-developer "This variable is
deprecated. Please use `ess-developer-select-package' and
`ess-r-select-evaluation-namespace' instead." "16.03")

(make-obsolete-variable 'ess-developer-load-command "This
variable is deprecated. Please use `ess-r-package-load-command'
instead." "16.03")

(make-obsolete-variable 'ess-developer-root-file "This variable
is deprecated. Please use `ess-r-package-root-file'
instead." "16.03")

(make-obsolete-variable 'ess-developer-packages "This variable
is deprecated. Please use `ess-developer-select-package' and
`ess-r-select-evaluation-namespace' instead." "16.03")

(make-obsolete-variable 'ess-developer-load-on-add-commands "This
variable is deprecated. Please use `ess-developer-select-package'
and `ess-r-select-evaluation-namespace' instead." "16.03")

(make-obsolete-variable 'ess-developer-activate-in-package "This
variable is deprecated. Please use
`ess-r-package-activate-in-package' instead." "16.03")

(make-obsolete-variable 'ess-developer-enter-hook "This variable
is deprecated. Please use `ess-r-package-enter-hook'
instead." "16.03")

(make-obsolete-variable 'ess-developer-exit-hook "This variable
is deprecated. Please use `ess-r-package-exit-hook'
instead." "16.03")


(provide 'ess-r-package)

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

;;; ess-r-package.el ends here
