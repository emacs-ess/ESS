;;; ess-rutils.el --- Convenience features for package and object control.  -*- lexical-binding: t; -*-

;; Author:       J. Alexander Branham
;; Maintainer:   ESS-Core
;; Keywords:     processes

;; Copyright (c) 2019 Free Software Foundation
;; Copyright (c) 2013-2019 ESS Core
;; Copyright (c) 2005-2013 Sebastian P. Luque

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; History:

;; This file was originally written by Sebastian Luque in 2004. It was
;; completely rewritten in 2019 as part of contributing ESS to GNU
;; Emacs.

;;; Commentary:

;; This library provides key bindings for performing basic R functions,
;; such as loading and managing packages, as well as object manipulation
;; (listing, viewing, and deleting), and an alternative to RSiteSearch()
;; that uses the browse-url function.  Load the library with the method you
;; prefer (e.g. M-x load-file), but the easiest is probably to: a) make
;; sure your load-path variable includes the directory where ess-rutils.el
;; resides, and b) include (require 'ess-rutils) statement in your
;; ~/.emacs.
;;
;; TODO: This should be more tightly integrated with ess-r-mode and ESSR.
;; TODO: Should be active in ess-r-mode not only ess-inf
;; TODO: Both S level Utils and this package's Rutils are in the menu; confusing and inconvenient.

;;; Code:

;; Autoloads and requires
(require 'ess-rdired)

(eval-when-compile
  (require 'subr-x))

(declare-function ess-display-help-apropos "ess-help" (&optional pattern))

(define-obsolete-variable-alias 'ess-rutils-buf 'ess-r-package-menu-buf "ESS 19.04")
(define-obsolete-variable-alias 'ess-rutils-mode-map 'ess-r-package-menu-mode-map "ESS 19.04")
(define-obsolete-function-alias 'ess-rutils-mode #'ess-r-package-menu-mode "ESS 19.04")

;;;###autoload
(defvar ess-rutils-map
  (let ((map (define-prefix-command 'ess-rutils-map)))
    (define-key map "l" #'ess-r-package-list-local-packages)
    (define-key map "r" #'ess-r-package-list-available-packages)
    (define-key map "u" #'ess-r-package-update-packages)
    (define-key map "o" #'ess-rdired)
    (define-key map "d" #'ess-change-directory)
    (define-key map "H" #'ess-rutils-html-docs)
    map))

(easy-menu-define ess-rutils-mode-menu inferior-ess-mode-menu
  "Submenu of `inferior-ess-mode' to use with RUtils."
  '("Package management"
    ["List local packages" ess-r-package-list-local-packages t]
    ["List available packages" ess-r-package-list-available-packages t]
    ["Update packages" ess-r-package-update-packages t]))

(easy-menu-add-item inferior-ess-mode-menu nil ess-rutils-mode-menu "Utils")
(easy-menu-add-item ess-mode-menu nil ess-rutils-mode-menu "Process")

(defvar ess-r-package-menu-buf "*R packages*"
  "Name of buffer to display R packages in.")

(defvar ess-r-package-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "l" #'ess-r-package-load)
    (define-key map "i" #'ess-r-package-mark-install)
    (define-key map "x" #'ess-r-package-execute-marks)
    (define-key map "u" #'ess-r-package-unmark)
    map)
  "Keymap for `ess-rutils-mode'.")

(define-derived-mode ess-r-package-menu-mode tabulated-list-mode "R utils"
  "Major mode for `ess-rutils-local-pkgs' and `ess-rutils-repos-pkgs'."
  :group 'ess-R
  (setq ess-dialect "R")
  (setq mode-name (concat "R packages: " ess-local-process-name))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-format
        `[("Name" 10 t)
          ("Description" 50 nil)
          ("Version" 5 t)])
  (tabulated-list-init-header))

(define-obsolete-function-alias 'ess-rutils-local-pkgs #'ess-r-package-list-local-packages "ESS 19.04")

;;;###autoload
(defun ess-r-package-list-local-packages ()
  "List all packages in all libraries."
  (interactive)
  (ess-r-package--list-packages (concat ".ess.rutils.ops <- options(width = 10000);"
                                        "print(installed.packages(fields=c(\"Title\"))[, c(\"Title\", \"Version\")]);"
                                        "options(.ess.rutils.ops); rm(.ess.rutils.ops);"
                                        "\n")))

(defun ess-r-package--list-packages (cmd)
  "Use CMD to list packages."
  (let ((process ess-local-process-name)
        des-col-beginning des-col-end entries)
    (with-current-buffer (ess-command cmd (get-buffer-create " *ess-rutils-pkgs*"))
      (goto-char (point-min))
      (delete-region (point) (1+ (point-at-eol)))
      ;; Now we have a buffer with package name, description, and
      ;; version. description and version are surrounded by quotes,
      ;; description is separated by whitespace.
      (re-search-forward "\\>[[:space:]]+")
      (setq des-col-beginning (current-column))
      (goto-char (point-at-eol))
      ;; Unless someone has a quote character in their package version,
      ;; two quotes back will be the end of the package description.
      (dotimes (_ 2) (search-backward "\""))
      (re-search-backward "[[:space:]]*")
      (setq des-col-end (current-column))
      (beginning-of-line)
      (while (not (eobp))
        (beginning-of-line)
        (let* ((name (string-trim (buffer-substring
                                   (point)
                                   (progn (forward-char (1- des-col-beginning))
                                          (point)))))
               (description (string-trim (buffer-substring
                                          (progn (forward-char 1)
                                                 (point))
                                          (progn (forward-char (- des-col-end des-col-beginning))
                                                 (point)))))
               (version (buffer-substring
                         (progn (end-of-line)
                                (search-backward "\"")
                                (search-backward "\"")
                                (forward-char 1)
                                (point))
                         (progn (search-forward "\"")
                                (backward-char 1)
                                (point)))))
          (push
           (list name
                 `[(,name
                    help-echo "mouse-2, RET: help on this package"
                    action ess-rutils-help-on-package)
                   ,description
                   ,version])
           entries)
          (forward-line)))
      (pop-to-buffer ess-rutils-buf)
      (setq ess-local-process-name process)
      (setq tabulated-list-entries entries)
      (ess-r-package-menu-mode)
      (tabulated-list-print))))

(define-obsolete-function-alias 'ess-rutils-loadpkg #'ess-r-package-load "ESS 19.04")
(defun ess-r-package-load ()
  "Load package from a library."
  (interactive)
  (ess-execute (concat "library('" (tabulated-list-get-id)
                       "', character.only = TRUE)")
               'buffer))

(defun ess-rutils-help-on-package (&optional _button)
  "Display help on the package at point."
  (interactive)
  ;; FIXME: Should go to a help buffer
  (ess-execute (concat "help(" (tabulated-list-get-id) ", package = '"
                       (tabulated-list-get-id)"')")
               'buffer))

(define-obsolete-function-alias 'ess-rutils-repos-pkgs #'ess-r-package-list-available-packages "ESS 19.04")
;;;###autoload
(defun ess-r-package-list-available-packages ()
  "List available packages.
Use the repositories as listed by getOptions(\"repos\") in the
current R session."
  (interactive)
  (ess-r-package--list-packages (concat ".ess.rutils.ops <- options(width = 10000);"
                                        "print(available.packages(fields=c(\"Title\"))[, c(\"Title\", \"Version\")]);"
                                        "options(.ess.rutils.ops); rm(.ess.rutils.ops);"
                                        "\n")))

(define-obsolete-function-alias 'ess-rutils-mark-install #'ess-r-package-mark-install "ESS 19.04")
(defun ess-r-package-mark-install ()
  "Mark the current package for installing."
  (interactive)
  (tabulated-list-put-tag "i" t))

(define-obsolete-function-alias 'ess-rutils-unmark #'ess-r-package-unmark "ESS 19.04")
(defun ess-r-package-unmark ()
  "Unmark the packages."
  (interactive)
  (tabulated-list-put-tag " " t))

(define-obsolete-function-alias 'ess-rutils-execute-marks #'ess-r-package-execute-marks "ESS 19.04")
(defun ess-r-package-execute-marks ()
  "Perform all marked actions."
  (interactive)
  ;; Install
  (save-excursion
    (let ((cmd "install.packages(c(")
          pkgs)
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at-p "i")
          (setq pkgs (concat "\"" (tabulated-list-get-id) "\", " pkgs))
          (tabulated-list-put-tag " "))
        (forward-line))
      (if pkgs
          (progn (setq pkgs (substring pkgs 0 (- (length pkgs) 2)))
                 (setq cmd (concat cmd pkgs "))"))
                 (ess-execute cmd 'buffer))
        (message "No packages marked for install")))))

(define-obsolete-function-alias 'ess-rutils-update-pkgs #'ess-r-package-update-packages "ESS 19.04")
;;;###autoload
(defun ess-r-package-update-packages (lib repo)
  "Update packages in library LIB and repo REPO.
This also uses checkBuilt=TRUE to rebuild installed packages if
needed."
  (interactive
   (list (ess-completing-read "Library to update: " (ess-get-words-from-vector
                                                     "as.character(.libPaths())\n"))
         (ess-completing-read "Repo: " (ess-get-words-from-vector
                                        "as.character(getOption(\"repos\"))\n"))))
  (ess-execute (format "update.packages(lib.loc='%s', repos='%s', ask=FALSE, checkBuilt=TRUE)" lib repo) 'buffer))

(define-obsolete-function-alias 'ess-rutils-apropos #'ess-display-help-apropos "ESS 19.04")

(defun ess-rutils-rm-all ()
  "Remove all R objects."
  (interactive)
  (if (y-or-n-p "Delete all objects? ")
      (ess-execute "rm(list=ls())" 'buffer)))

(defun ess-rutils-load-wkspc (file)
  "Load workspace FILE into R."
  (interactive "fFile with workspace to load: ")
  (ess-execute (concat "load('" file "')") 'buffer))

(defun ess-rutils-save-wkspc (file)
  "Save FILE workspace.
File extension not required."
  (interactive "FSave workspace to file (no extension): ")
  (ess-execute (concat "save.image('" file ".RData')") 'buffer))

(defun ess-rutils-quit ()
  "Kill the ess-rutils buffer and return to the iESS buffer."
  (interactive)
  (ess-switch-to-end-of-ESS)
  (kill-buffer ess-rutils-buf))

(defun ess-rutils-html-docs (&optional remote)
  "Use `browse-url' to navigate R html documentation.
Documentation is produced by a modified help.start(), that
returns the URL produced by GNU R's http server. If called with a
prefix, the modified help.start() is called with update=TRUE. The
optional REMOTE argument should be a string with a valid URL for
the 'R_HOME' directory on a remote server (defaults to NULL)."
  (interactive)
  (let* ((update (if current-prefix-arg "update=TRUE" "update=FALSE"))
         (remote (if (or (and remote (not (string= "" remote))))
                     (concat "remote=" remote) "remote=NULL"))
         (proc ess-local-process-name)
         (rhtml (format ".ess_help_start(%s, %s)\n" update remote)))
    (with-temp-buffer
      (ess-command rhtml (current-buffer) nil nil nil (get-process proc))
      (let* ((begurl (search-backward "http://"))
             (endurl (search-forward "index.html"))
             (url (buffer-substring-no-properties begurl endurl)))
        (browse-url url)))))

(defun ess-rutils-rsitesearch (string)
  "Search the R archives for STRING, and show results using `browse-url'.
If called with a prefix, options are offered (with completion)
for matches per page, sections of the archives to search,
displaying results in long or short formats, and sorting by any
given field. Options should be separated by value of
`crm-default-separator'."
  (interactive "sSearch string: ")
  (let ((site "https://search.r-project.org/cgi-bin/namazu.cgi?query=")
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
                        "Format: " '(("normal" 1) ("short" 2))
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
                           '(("Rhelp02a" 1) ("functions" 2)
                             ("docs" 3) ("Rhelp01" 4))
                           nil t "Rhelp02a,functions,docs" nil
                           "Rhelp02a,functions,docs") "&idxname="))))
          (browse-url (concat site okstring mpp format sortby restrict)))
      (browse-url (concat site okstring "&max=20&result=normal&sort=score"
                          "&idxname=Rhelp02a&idxname=functions&idxname=docs")))))

(defun ess-rutils-help-search (string)
  "Search for STRING using help.search()."
  (interactive "sString to search for? ")
  (let ((proc ess-local-process-name))
    (pop-to-buffer "foobar")
    (ess-command (concat "help.search('" string "')\n")
                 (current-buffer) nil nil nil (get-process proc))))

(make-obsolete 'ess-rutils-rhtml-fn "overwrite .ess_help_start instead." "ESS 18.10")

(provide 'ess-rutils)

;;; ess-rutils.el ends here
