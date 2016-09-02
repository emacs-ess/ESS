;;; ess-r-package.el --- Package development mode for R.

;; Copyright (C) 2011-2015 Lionel Henry, Vitalie Spinu, A.J. Rossini, Richard
;;      M. Heiberger, Martin Maechler, Kurt Hornik, Rodney Sparapani, and
;;      Stephen Eglen.

;; Author: Lionel Henry, Vitalie Spinu
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

(defcustom ess-r-package-auto-set-evaluation-env t
  "If non-nil, evaluation env is set to package env automatically.
See also `ess-r-set-evaluation-env' and `ess-r-evaluation-env'."
  :group 'ess-r-package
  :type 'boolean)

(defvar-local ess-r-package-info nil
  "Current package info cache.

Cons cell of two strings. CAR is the package name active in the
current buffer. CDR is the path to its source directory.")

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


;;;*;;; Package UI

(defun ess-r-package-get-info ()
  "Get current package info.
Return a cons cell of two strings whose CAR is a package name and
CDR is a package directory. The package is determined by (in this
order) the buffer-local value of `ess-r-package-info',
whether the current file is part of a package, or the value of
`ess-r-package-info' in the attached process buffer."
  (or ess-r-package-info
      (ess-r-package--local-package-info)
      (with-ess-process-buffer t
        ess-r-package-info)))

(defun ess-r-package-set-package ()
  "Set a package for ESS r-package commands."
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
    (setq ess-r-package-info pkg-info)))

(defun ess-r--select-package-name ()
  (ess-force-buffer-current)
  (let ((pkgs (ess-get-words-from-vector
               (format "print(.packages(%s), max = 1e6)\n"
                       (if ess-r-prompt-for-attached-pkgs-only "FALSE" "TRUE"))))
        (current-pkg (car (ess-r-package-get-info))))
    (let ((env (ess-r-get-evaluation-env)))
     (when env
       (setq pkgs (append '("*none*") pkgs))
       (when (equal env current-pkg)
         (setq current-pkg "*none*"))))
    (ess-completing-read "Package" pkgs nil nil nil nil current-pkg)))

(defun ess-r-package-set-namespaced-evaluation ()
  (when ess-r-package-auto-set-evaluation-env
    (let ((pkg (car (ess-r-package-get-info))))
      (when pkg
        (ess-r-set-evaluation-env pkg)))))

(add-hook 'R-mode-hook 'ess-r-package-set-namespaced-evaluation)

(defun ess-r-package-send-process (command &optional msg alt default-alt)
  (ess-force-buffer-current)
  (let* ((pkg-info (or (ess-r-package-get-info)
                       (ess-r-package-set-package)))
         (name (car pkg-info))
         (path (concat "'" (cdr pkg-info) "'"))
         (args (ess-r-command--process-alt-args alt default-alt)))
    (message msg name)
    (with-ess-process-buffer nil
      (setq ess-r-package-info pkg-info))
    (ess-eval-linewise (format command (concat path args)))))

(defun ess-r-command--process-alt-args (alt &optional default-alt)
  (let ((args (cond ((stringp alt) alt)
                    (alt (read-string "Arguments: " default-alt))
                    (t ""))))
    (if (or (null args)
            (string= "" args))
        args
      (concat ", " args))))


;;;*;;; Package Detection

(defun ess-r-package--local-package-info ()
  "Parses DESCRIPTION file in PATH (R specific so far). PATH
defaults to the value returned by
`ess-r-package--find-package-path'."
  (let ((pkg-path (ess-r-package--find-package-path)))
    (setq ess-r-package-info
          (if pkg-path
              (cons (ess-r-package--find-package-name pkg-path) pkg-path)
            ;; cache non-package files as well
            '(nil)))))

(defun ess-r-package--find-package-path ()
  "Get the root of R package that contains current directory.
Root is determined by locating `ess-r-package-root-file'."
  (let* ((path (if (buffer-file-name)
                   (file-name-directory (buffer-file-name))
                 default-directory))
         (pkg-path
          (when path
            (or
             ;; First check current directory
             (and (file-exists-p (expand-file-name ess-r-package-root-file path))
                  path)
             ;; Check for known directories in current path
             (let ((current-dir (file-name-nondirectory (directory-file-name path)))
                   known-pkg-dir known-path presumptive-path)
               (while (and path (not presumptive-path))
                 (setq current-dir (file-name-nondirectory (directory-file-name path)))
                 (if (and (setq known-pkg-dir (assoc current-dir ess-r-package-dirs))
                          (setq known-path (ess--parent-dir path (cdr known-pkg-dir)))
                          (file-exists-p (expand-file-name ess-r-package-root-file known-path)))
                     (setq presumptive-path known-path)
                   (setq path (ess--parent-dir path 1))))
               presumptive-path)))))
    (when pkg-path
      (directory-file-name pkg-path))))

(defun ess--parent-dir (path n)
  "Return Nth parent of PATH."
  (let ((opath path))
    (while (and path (> n 0))
      (setq path (file-name-directory (directory-file-name opath)))
      (if (equal path opath)
          (setq path nil)
        (setq opath path
              n (1- n))))
    path))

(defun ess-r-package--find-package-name (path)
  (let ((file (expand-file-name ess-r-package-root-file path))
        (case-fold-search t))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "package: \\(.*\\)" nil t)
          (match-string 1))))))


;;;*;;; Devtools Integration

(defun ess-r-devtools-load-package (&optional alt)
  "Interface for `devtools::load_all()'."
  (interactive "P")
  (ess-r-package-send-process "devtools::load_all(%s)\n"
                              "Loading %s"
                              alt "recompile = TRUE"))

(defun ess-r-devtools-unload-package ()
  "Interface to `devtools::unload()'."
  (interactive)
  (ess-r-package-send-process "devtools::unload(%s)\n"
                              "Unloading %s"))

(defun ess-r-devtools-check-package (&optional alt)
  "Interface for `devtools::check()'."
  (interactive "P")
  (ess-r-package-send-process "devtools::check(%s)\n"
                              "Checking %s"
                              alt "vignettes = FALSE"))

(defun ess-r-devtools-test-package (&optional alt)
  "Interface for `devtools::test()'.
When called with a non-string prefix argument (as with C-u),
check that the file name starts with `test-' and ends with
`.R'. If that is the case, use the string in-between as default
filter argument. Otherwise, use the whole base filename."
  (interactive "P")
  (let (file-name)
    (when (and alt
               (not (stringp alt))
               buffer-file-name
               (setq file-name (file-name-nondirectory buffer-file-name)))
      (setq alt (if (string-match "test-\\([[:alnum:]]+\\)\\.[rR]" file-name)
                    (match-string-no-properties 1 file-name)
                  (file-name-base buffer-file-name)))
      (setq alt (concat "\"" alt "\""))))
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

(defun ess-r-devtools-install-github (&optional alt repo)
  "Interface to `devtools::install_github()'.
Asks for github repository in the form of user/repo, unless REPO
is supplied. Prompts for additional arguments when called with a
prefix."
  (interactive "P")
  (let ((command "devtools::install_github(%s%s)")
        (repo (concat "'" (or repo (read-string "User/Repo: ")) "'"))
        (args (ess-r-command--process-alt-args alt "ref = ")))
    (ess-eval-linewise (format command repo args))))


;;;*;;; Minor Mode

(defcustom ess-r-package-auto-activate t
  "If non-nil, `ess-r-package-mode' is turned on within R packages."
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
  '(:eval (let* ((pkg-name (car (ess-r-package-get-info))))
            (when pkg-name
              (format " [pkg:%s]" pkg-name))))
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
  "Minor mode for enabling R package development features.

\\{ess-r-package-mode-map}"
  :init-value nil
  :keymap ess-r-package-mode-map
  :lighter ess-r-package-mode-line
  (cond
   (ess-r-package-mode
    (run-hooks 'ess-r-package-enter-hook))
   (t
    (run-hooks 'ess-r-package-exit-hook))))

(add-hook 'after-change-major-mode-hook 'ess-r-package-auto-activate)

(defun ess-r-package-auto-activate ()
  "Activate developer if current file is part of a package."
  (when (and ess-r-package-auto-activate
             (not (memq major-mode '(minibuffer-inactive-mode fundamental-mode)))
             (or (buffer-file-name)
                 default-directory))
    (let ((pkg-info (ess-r-package-get-info)))
      (when (car pkg-info)
        (ess-r-package-mode 1)))))


;;;*;;; Deprecated variables and functions
(defun ess-developer (&optional val)
  (error "As of ESS 16.04, `ess-developer' is deprecated. Use `ess-r-set-evaluation-env' instead."))

(defalias 'ess-toggle-developer 'ess-developer)

(make-obsolete-variable 'ess-developer "Please use `ess-developer-select-package' and `ess-r-set-evaluation-env' instead." "16.04")
(make-obsolete-variable 'ess-developer-root-file "Please use `ess-r-package-root-file' instead." "16.04")
(make-obsolete-variable 'ess-developer-packages "Please use `ess-r-package-set-package' and `ess-r-set-evaluation-env' instead." "16.04")
(make-obsolete-variable 'ess-developer-load-on-add-commands "Please use `ess-r-package-set-package' and `ess-r-set-evaluation-env' instead." "16.04")
(make-obsolete-variable 'ess-developer-activate-in-package "Please use `ess-r-package-auto-activate' instead." "16.04")
(make-obsolete-variable 'ess-developer-enter-hook "Please use `ess-r-package-enter-hook' instead." "16.04")
(make-obsolete-variable 'ess-developer-exit-hook "Please use `ess-r-package-exit-hook' instead." "16.04")


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
