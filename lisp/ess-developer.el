;;; ess-developer.el --- Developer mode for R.

;; Copyright (C) 2011-2012 V. Spinu, A.J. Rossini, Richard M. Heiberger, Martin
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

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; see apropriate documentation section of ESS user manual

;;; Code:

;; (require 'ess-site) ;; need to assigne the keys in the map

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

(defvar ess--developer-local-indicator (propertize "d" 'face 'ess-developer-indicator-face))
(put 'ess--developer-local-indicator 'risky-local-variable t)

(defcustom ess-developer-packages nil
  "List of names of R packages you develop.
Use `ess-developer-add-package' to modify interactively this
list. "
  :group 'ess-developer
  :type 'list)

(defcustom ess-developer-load-package-command "library(devtools)\nload_all('%s')\n"
  "Command issued by `ess-developer-load-package'.
 %s is subsituted with the user supplied directory."
  :group 'ess-developer
  :type 'string)

(defvar ess-developer-root-file "DESCRIPTION"
  "If this file is present in the directory, it is considered a
  project root.")

(defcustom ess-developer-force-attach nil
  "If non-nill all the packages listed in `ess-developer-packages' should be attached
when ess-developer mode is turned on."
  :group 'ess-developer
  :type 'boolean)

(defcustom ess-developer-enter-hook nil
  "Normal hook run on entering `ess-developer' mode."
  :group 'ess-developer
  :type 'hook)

(defcustom ess-developer-exit-hook nil
  "Normal hook run on exiting `ess-developer' mode."
  :group 'ess-developer
  :type 'hook)

(defcustom ess-developer-activate-in-package t
  "If non-nil, ess-developer is automatically toggled in files
within package directory."
  :group 'ess-developer
  :type 'boolean)

(defcustom ess-developer-load-on-add-commands '(("library" . "library(%n)")
                                                ("load_all" . "library(devtools)\nload_all('%d')"))
  "Alist of available load commands what are proposed for loading on `ess-developer-add-package'.

  %n is replaced with package name,
  %d is replaced with package directory.

See also `ess-developer-load-package' for related functionality."
  :group 'ess-developer
  :type 'alist)

(defvar ess--developer-load-hist nil)

(defun ess-developer-add-package (&optional attached-only)
  "Add a package to `ess-developer-packages' list.
With prefix argument only choose from among attached packages."
  (interactive "P")
  (ess-force-buffer-current)
  (let* ((packs (ess-get-words-from-vector
                 (format "print(unique(c(.packages(), %s)), max=1e6)\n"
                         (if attached-only "NULL" ".packages(TRUE)") nil t)))
         (cur-pack (ess--developer-containing-package))
         (sel (ess-completing-read "Add package" packs nil nil nil nil
                                   (unless (member cur-pack ess-developer-packages)
                                     cur-pack)))
         (check-attached (format ".ess_package_attached('%s')\n" sel)))
    (unless (ess-boolean-command check-attached)
      (let* ((fn (if (> (length ess-developer-load-on-add-commands) 1)
                     (ess-completing-read "Package not loaded. Use"
                                          (mapcar 'car ess-developer-load-on-add-commands) nil t
                                          nil 'ess--developer-load-hist (car ess--developer-load-hist))
                   (caar ess-developer-load-on-add-commands)))
             (cmd (cdr (assoc fn ess-developer-load-on-add-commands))))
        (setq cmd (replace-regexp-in-string "%n" sel cmd))
        (when (string-match-p "%d" cmd)
          (let ((dir (read-directory-name
                      "Package: " (ess--developer-locate-package-path sel) nil t nil)))
            (setq cmd (replace-regexp-in-string "%d" dir cmd))))
        (ess-eval-linewise (concat cmd "\n")))
      (ess-wait-for-process)
      (when (not (ess-boolean-command check-attached))
        (error "Package '%s' could not be added" sel)))
    (setq ess-developer-packages
          (ess-uniq-list (append ess-developer-packages (list sel))))
    ;; turn developer in all files from selected package
    (ess-developer-activate-in-package sel 'all)
    (message "You are developing: %s" ess-developer-packages)))

(defun ess-developer-remove-package ()
  "Remove packages from `ess-developer-packages' list; defaults to *ALL*."
  (interactive)
  (unless ess-developer-packages
    (error "Nothing to remove, 'ess-developer-packages' is empty"))
  (let ((sel (ess-completing-read "Remove package(s)"
                                  (append ess-developer-packages (list "*ALL*"))
                                  nil t nil nil "*ALL*")))
    (if (equal "*ALL*" sel)
        (progn
          (setq ess-developer-packages nil)
          (ess-developer-deactivate-in-package nil 'all)
          (message "Removed *ALL* packages from the `ess-developer-packages' list."))
      (setq ess-developer-packages (delete sel ess-developer-packages))
      (ess-developer-deactivate-in-package sel 'all)
      (message "Removed package '%s' from the `ess-developer-packages' list"
               (propertize sel 'face 'font-lock-function-name-face)))))

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
  (unless ess-developer
    (error "Ess-developer mode is not active"))
  (if (not (or filename
               buffer-file-name))
      (error "Buffer '%s' doesn't visit a file" (buffer-name (current-buffer)))
    (let* ((filename (or filename buffer-file-name))
           (file (file-name-nondirectory filename))
           (env (ess-completing-read (format "devSource '%s' into" file)
                                     (append ess-developer-packages (list "*current*" )) nil t))
           (comm  (if (equal env "*current*")
                      (format "source(file=\"%s\", local=F)\n cat(\"Sourced file '%s' into\", capture.output(environment()), '\n')" filename file)
                    (format ".essDev_source(source='%s',package='%s')" filename env))))
      (when (buffer-modified-p) (save-buffer))
      (message "devSourcing '%s' ..." file)
      (ess--developer-command comm 'ess--developer-propertize-output))))

(defun ess-developer-send-function (proc beg end name &optional visibly message tracebug)
  (save-excursion
    (if (null ess-developer-packages)
        (error "`ess-developer-packages' is empty (add packages with C-c C-t C-a).")
      (if (null name)
          (error "Oops, could not find function name (probably a regexp bug)")
        (let ((nms (ess-get-words-from-vector "loadedNamespaces()\n"))
              (dev-packs ess-developer-packages)
              assigned-p ns)
          ;; such a kludge
          (if (string-match-p ess-set-function-start (concat name "("))
              (ess-developer-send-region proc beg end visibly message tracebug)
            (if tracebug (ess-tracebug-set-last-input proc))
            (while (and (setq ns (pop dev-packs))
                        (not assigned-p))
              (when (and (member ns nms) ;;todo: try to load the package if not loaded
                         (ess-boolean-command
                          (format "as.character(exists('%s', envir=asNamespace('%s'), mode='function', inherits=FALSE))\n"
                                  name ns)))
                (ess-developer-devSource beg end ns message)
                (setq assigned-p t)))
            (unless assigned-p
              (ess-developer-send-region-fallback proc beg end visibly message tracebug))))))))

(defvar ess--developer-hist nil)

(defun ess-developer-send-region (proc beg end &optional visibly message tracebug)
  "Ask for for the package and devSource region into it."
  (let* ((all-packs (append ess-developer-packages (list "*current*" )))
         (default (car (member (car ess--developer-hist) all-packs)))
         (package
          (ess-completing-read "devEval into" all-packs
                               nil t nil 'ess--developer-hist default)))
    (message  (if message (format "dev%s ..." message)))
    (if (equal package "*current*")
        (ess-developer-send-region-fallback proc beg end visibly message tracebug)
      ;; else, (ignore VISIBLY here)
      (ess-developer-devSource beg end package message))))

(defun ess-developer-devSource (beg end package &optional message)
  (let* ((ess-load-command
          (format ".essDev_source(source='%s',package='%s')" "%s" package))
         (ess-load-visibly-noecho-command ess-load-command))
    (if message (message message))
    (ess--developer-command (ess--make-source-refd-command beg end)
                            'ess--developer-propertize-output)))

(defun ess--developer-command (comm &optional propertize-func)
  "Evaluate the command and popup a message with the output if succed.
On error  insert the error at the end of the inferior-ess buffer.

PROPERTIZE-FUNC is a function called with the output buffer being
current. usually used to manipulate the output, for example to
propertize output text.
"
  (ess--developer-inject-source-maybe) ; first time only
  (setq comm (format "eval({cat('\\n')\n%s\ncat('!@OK@!')})\n" comm))
  (let ((buff (get-buffer-create " *ess-command-output*"))
        out)
    (ess-command comm buff nil nil 0.1)
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

(defun ess--developer-propertize-output ()
  (goto-char (point-min))
  (while (re-search-forward "\\(FUN\\|CLS\\METH\\)\\[" nil t)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face 'font-lock-function-name-face))
  (goto-char (point-min))
  (while (re-search-forward "\\([^ \t]+\\):" nil t)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face 'font-lock-keyword-face)))

(defvar ess--developer-package-root nil)
(make-variable-buffer-local 'ess--developer-package-root)

(defun ess--developer-containing-package ()
  "Return the name of the container package, or nil if not found.
Currently works for R only and looks at /R/ parent directory."
  (let ((path (directory-file-name default-directory)))
    (when (string-equal "R" (file-name-nondirectory path))
      (file-name-nondirectory (directory-file-name (file-name-directory path))))))

(defun ess--developer-locate-package-path (&optional pack-name)
  "Get the root of R package that contains current directory.
Root is determined by locating `ess-developer-root-file'.

If PACK-NAME is given, iterate over default-directories of all
open R files till package with name pack-name is found (if any)."
  (if pack-name
      (let ((bl (buffer-list))
            path bf)
        (while (and (setq bf (pop bl))
                    (not path))
          (when (buffer-local-value 'ess-dialect bf)
            (with-current-buffer bf
              (setq path (ess--developer-locate-package-path)))))
        path)
    (let ((path default-directory)
          package)
      (while (and (not package) (> (length path) 0))
        (if (file-exists-p (expand-file-name ess-developer-root-file path))
            (setq package path)
          (setq path (file-name-directory (directory-file-name path)))))
      ;; cache locally
      (when path
        (setq ess--developer-package-root path))
      path)))


(defun ess-developer-activate-in-package (&optional package all)
  "Activate developer if current file is part of the package and
package name is registered in `ess-developer-packages'.

If PACKAGE is given, activate only if current file is part of the
PACKAGE, `ess-developer-packages' is ignored in this case.

If ALL is non-nil, perform activation in all R buffers.

This function does nothing if `ess-developer-activate-in-package'
is nil. "
  (when ess-developer-activate-in-package
    (if all
        (dolist (bf (buffer-list))
          (with-current-buffer bf
            (ess-developer-activate-in-package package)))
      (let ((pack (ess--developer-containing-package)))
        (when (and (not ess-developer)
                   (if package
                       (equal pack package)
                     (member pack ess-developer-packages)))
          (ess-developer t))))))

(add-hook 'R-mode-hook 'ess-developer-activate-in-package)

(defun ess-developer-deactivate-in-package (&optional package all)
  "Deactivate developer if current file is part of the R package.

If PACKAGE is given, deactivate only if current package is
PACKAGE.

If ALL is non-nil, deactivate in all open R buffers."
  (if all
      (dolist (bf (buffer-list))
        (with-current-buffer bf
          (ess-developer-deactivate-in-package package)))
    (let ((pack (ess--developer-containing-package)))
      (when (and ess-developer
                 (or (null package)
                     (equal pack package)))
        (ess-developer -1)))))

(defun ess-developer-load-package ()
  "Interface to load_all function in devtools package.
See also `ess-developer-load-all-command'."
  (interactive)
  (let ((package (ess--developer-locate-package-path)))
    (setq package (read-directory-name "Package: " package nil t nil))
    (unless (file-exists-p (expand-file-name ess-developer-root-file package))
      (error "Not a valid package. No '%s' found in `%s'."
             ess-developer-root-file package))
    (message "Loading %s" (abbreviate-file-name package))
    (ess-eval-linewise
     (format ess-developer-load-package-command package))))

(defvar ess-developer nil
  "Non nil in buffers where developer mode is active")
(make-variable-buffer-local 'ess-developer)

(defun ess--developer-inject-source-maybe ()
  ;; puting this into ESSR.R makes loading very slow
  ;; when ESSR is a package, this should go away
  (let ((devR-file (concat (file-name-directory ess-etc-directory)
                           "ess-developer.R")))
    (unless (ess-boolean-command
             "exists('.essDev_source', envir = .ESSR_Env)\n")
      (unless (file-exists-p devR-file)
        (error "Cannot locate 'ess-developer.R' file"))
      (message "Injecting ess-developer code ...")
      (ess--inject-code-from-file devR-file)
      (unless (ess-boolean-command "exists('.essDev_source', envir = .ESSR_Env)\n")
        (error "Could not source ess-developer.R. Please investigate the output of *ess-command-output* buffer for errors")))))

(defun ess-developer (&optional val)
  "Toggle on/off ess-developer functionality.
If optional VAL is non-negative, turn on the developer mode. If
VAL is negative turn it off."
  (interactive)
  (when (eq val t) (setq val 1))
  (let ((ess-dev  (if (numberp val)
                      (if (< val 0) nil t)
                    (not (or ess-developer
                             ;; if t in proc buffer, all associated buffers are in dev-mode
                             (and (ess-process-live-p)
                                  (ess-get-process-variable 'ess-developer)))))))
    (if ess-dev
        (progn
          (run-hooks 'ess-developer-enter-hook)
          (if ess-developer-packages
              (message "You are developing: %s" ess-developer-packages)
            (message "Developer is on (add packages with C-c C-t a)")))
      (run-hooks 'ess-developer-exit-hook)
      (message "Developer is off"))

    (setq ess-developer ess-dev)

    (if (get-buffer-process (current-buffer)) ; in ess process
        (setq ess-local-process-name
              (if ess-dev
                  (propertize ess-local-process-name 'face 'ess-developer-indicator-face)
                (propertize  ess-local-process-name 'face nil)))
      (if ess-dev
          (add-to-list 'ess--local-mode-line-process-indicator 'ess--developer-local-indicator 'append)
        (delq 'ess--developer-local-indicator ess--local-mode-line-process-indicator)))

    (force-window-update)))

(defalias 'ess-toggle-developer 'ess-developer)

(provide 'ess-developer)
;;; ess-developer.el ends here
