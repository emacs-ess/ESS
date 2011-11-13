;;; ess-developer.el --- Developer mode for R.

;; Copyright (C) 2011 V. Spinu, A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: Vitalie Spinu
;; Created: 12-11-2011
;; Maintainers: ESS-core <ESS-core@r-project.org>

;; Keywords: developement, interaction.

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
;; In develper mode `ess-eval-function' (and frends) checks
;; if the function name could be found in the `ess-developer-packages'.  If
;; so, it assigns the function into the namespace using
;; 'assignInNamespace()'.
;; "C-M-d t" to toggle developer mode
;; "C-M-d a" to add a package to your development list (with C-u - remove)
;; "C-M-d r" to remove a package to your development list
;;




(require 'ess-site) ;; need to assigne the keys in the map

(define-key ess-mode-map [(control meta ?d) (?t)] 'ess-developer)
(define-key inferior-ess-mode-map [(control meta ?d) (?t)] 'ess-developer)
(define-key ess-mode-map [(control meta ?d) (?a)] 'ess-developer-add-package)
(define-key inferior-ess-mode-map [(control meta ?d) (?a)] 'ess-developer-add-package)
(define-key ess-mode-map [(control meta ?d) (?r)] 'ess-developer-remove-package)
(define-key inferior-ess-mode-map [(control meta ?d) (?r)] 'ess-developer-remove-package)


(defgroup ess-developer nil
  "ESS: developer."
  :group 'ess
  :prefix "ess-developer-")

(defface ess-developer-indicator-face
  '((((class grayscale)) (:background "DimGray"))
    (((class color))
     (:background "deep sky blue" :foreground "red4"  :bold t ))
    )
  "Face to highlight currently debugged line."
  :group 'ess-developer
  )

(defvar ess--developer-p nil
  "t if ESS is in developer mode for current process.
Use `ess-developer' to set this variable.
")
(make-variable-buffer-local 'ess--developer-p)

(defcustom ess-developer-packages nil
  "List of names of R packages you develop.
Use `ess-developer-add-package' to modify interactively this
list. "
  :group 'ess-developer)

(defcustom ess-developer-enter-source "~/ess-developer-enter.R"
  "File to 'source()' in on entering `ess-developer' mode."
  :group 'ess-developer
  :type 'file)

(defcustom ess-developer-exit-source "~/ess-developer-exit.R"
  "File to 'source()' in on exiting `ess-developer' mode."
  :group 'ess-developer
  :type 'file)

(defcustom ess-developer-enter-hook nil
  "Normal hook run on entering `ess-developer' mode."
  :group 'ess-developer
  :type 'hook)

(defcustom ess-developer-exit-hook nil
  "Normal hook run on exiting `ess-developer' mode."
  :group 'ess-developer
  :type 'hook)

(defun ess-developer-add-package (&optional remove)
  "Add a package to `ess-developer-packages' list.
With prefix argument removes the packages, defaults to *ALL*."
  (interactive "P")
  (if (and remove (null ess-developer-packages))
      (message "Nothing to remove, ess-developer-packages is empty")
    (let ((sel (if remove
                   (ess-completing-read "Remove pakage(s): "
                                        (append ess-developer-packages (list "*ALL*"))
                                        nil t nil nil "*ALL*")
                 (ess-completing-read "Add package: "
                                      (ess-get-words-from-vector ".packages(TRUE)\n") nil t)
                 )))
      (if remove
          (if (equal "*ALL*" sel)
              (progn
                (setq ess-developer-packages nil)
                (message "Removed *ALL* packages from ess-developer-packages."))
            (setq ess-developer-packages (delete sel ess-developer-packages))
            (message "Removed package '%s' from ess-developer-packages list" (propertize sel 'face 'font-lock-function-name-face)))
        (setq ess-developer-packages (ess-uniq-list (append  ess-developer-packages (list sel))))
        (message "You are developing: %s" ess-developer-packages)
        ))))

(defun ess-developer-remove-package ()
  "Remove a package from `ess-developer-packages' list."
  (interactive)
  (ess-developer-add-package t)
  )

(defun ess-developer-assign-function (fname func-def)
  "Assign FNAME function in R namespaces given by `ess-developer-packages'.
If succeeded, return the namespace, nil otherwise.
FUNC-DEF should be a definition of a function including the
  assignment (i.e. name <- function(..){...})"
  (if (null ess-developer-packages)
      (error "ess-developer-packages is empty! Add packages with 'ess-developer-add-package' first")
    (if (null fname)
        (error "Oops, could not find function name (probably a bug)")
      (let ((nms (ess-get-words-from-vector "loadedNamespaces()\n"))
            (dev-packs ess-developer-packages)
            (buff (get-buffer-create " *ess-command-output*"))
            assigned-p ns comm)
        (while (and (setq ns (pop dev-packs))
                    (not assigned-p))
          (when (and (member ns nms)
                     (equal "TRUE" (car (ess-get-words-from-vector
                                         (format "as.character(exists('%s', envir=asNamespace('%s'), mode='function',inherits=FALSE))\n" fname ns)))))
            (setq comm
                  (format  "local({%s;
environment(%s) <- asNamespace('%s');
assignInNamespace(x = '%s', value = %s, ns = '%s');
print('@OK@')})\n"
                           func-def fname ns fname fname ns))
            (ess-command comm buff)
            (with-current-buffer buff
              (goto-char (point-max))
              (if (re-search-backward "@OK@" nil t)
                  (message "Assigned function '%s' in namespace '%s'" fname (propertize ns 'face 'font-lock-function-name-face))
                (message "Assignment of function '%s' in namespace '%s' went wrong:\n%s"
                         fname ns (buffer-substring-no-properties (point-min) (point-max)))))
            (setq assigned-p ns)
            ))
        assigned-p)
      )))

(defun ess-developer (&optional val)
  "Toggle on/off ess-developer functionality.
If optional VAL is non-negative, turn on the developer mode. If
VAL is negative turn it off.

In develper mode `ess-eval-function' (and frends) checks
if the function name could be found in the packages listed.  If
so, it assigns the function into the namespace using
'assignInNamespace()'.
"
  (interactive)
  (when (eq val t) (setq val 1))
  (with-current-buffer (process-buffer (get-process ess-current-process-name))
    (let ((ess-dev  (if (numberp val)
                        (if (< val 0) nil t)
                      (not ess--developer-p))))
      (if ess-dev
          (progn
            (run-hooks 'ess-developer-enter-hook)
            (when (file-readable-p ess-developer-enter-source)
              (ess-eval-linewise (format "source(%s)\n" ess-developer-enter-source)))
            (message "Developer mode is on"))
        (run-hooks 'ess-developer-exit-hook)
        (when (file-readable-p ess-developer-exit-source)
          (ess-eval-linewise (format "source(%s)\n" ess-developer-exit-source)))
        (message "Developer mode is off"))
      (setq ess--developer-p ess-dev))
    (setq ess-local-process-name
          (if ess--developer-p
              (propertize ess-local-process-name 'face 'ess-developer-indicator-face)
            (propertize  ess-local-process-name 'face nil)))
    ))

(provide 'ess-developer)