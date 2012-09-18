;;; ess-rutils.el --- R functions and keybindings to use in iESS.

;; Author:       Sebastian Luque <sluque@gmail.com>
;; Created:      Thu Nov 10 02:20:36 2004 (UTC)
;; Last-Updated: Wed Mar  2 21:08:11 2011 (UTC)
;;           By: Sebastian P. Luque
;; Version: $Id$
;; Compatibility: GNU Emacs >= 22.0.50.1

;; Copyright (c) 2005, 2006, 2007, 2008, 2009, 2010, 2011 Sebastian P. Luque

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
;; Usage:
;;
;; Once R is started with M-x R, you should have the key bindings defined
;; at the end of this file working in your iESS process buffers.  Simply
;; type the desired key binding.
;;
;; Acknowledgements:
;;
;; I am grateful to John Fox for having written his init.el file for
;; XEmacs, which motivated this Emacs alternative.  I wanted to add some
;; object management comforts and came across Stephen Eglen's
;; ess-rdired.el, which provides a lot of these.  ess-rutils.el builds upon
;; on a *lot* of ideas from ess-rdired.el.

;;; Code:

;; Autoloads and requires
(autoload 'ess-rdired "ess-rdired" "View *R* objects in a dired-like buffer." t)
(require 'ess-site)

(defvar ess-rutils-buf "*R temp*"
  "Name of temporary R buffer.")

(defvar ess-rutils-mode-map nil
  "Keymap for the *R temp* buffer.")

(defvar ess-rutils-rhtml-fn
  (expand-file-name "ess-rutils-help-start.R" ess-etc-directory)
  "Path to the file defining the R function .rutils.help.start().
This file is loaded into the inferior R process so that
`ess-rutils-html-docs' can use .rutils.help.start().")

(if ess-rutils-mode-map
    ()
  (setq ess-rutils-mode-map (make-sparse-keymap))
  (define-key ess-rutils-mode-map "l" 'ess-rutils-loadpkg)
  (define-key ess-rutils-mode-map "i" 'ess-rutils-mark-install)
  (define-key ess-rutils-mode-map "I" 'ess-rutils-install)
  (define-key ess-rutils-mode-map "u" 'ess-rutils-unmark)
  (define-key ess-rutils-mode-map "q" 'ess-rutils-quit)
  (define-key ess-rutils-mode-map "?" 'ess-rutils-help))

(defun ess-rutils-mode ()
  "Major mode for output from `ess-rutils-local-pkgs' and `ess-rutils-repos-pkgs'.
Useful bindings to handle package loading and installing.
\\{ess-rutils-mode-map}"
  (kill-all-local-variables)
  (use-local-map ess-rutils-mode-map)
  (setq major-mode 'ess-rutils-mode)
  (setq mode-name (concat "R utils " ess-local-process-name)))

(defun ess-rutils-local-pkgs ()
  "List all packages in all libraries."
  (interactive)
  (if (get-buffer ess-rutils-buf)
      (progn
        (set-buffer ess-rutils-buf)
        (setq buffer-read-only nil)))
  (ess-execute
   "writeLines(paste('  ', sort(.packages(all.available=TRUE)), sep=''))"
   nil
   (substring ess-rutils-buf 1 (- (length ess-rutils-buf) 1)))
  (pop-to-buffer ess-rutils-buf)
  (save-excursion
    (beginning-of-line) (open-line 1)
    (insert "**Available packages in all local R libraries**"))
  (setq buffer-read-only t)
  (ess-rutils-mode)
  (if (featurep 'fit-frame)
      (fit-frame)))

(defun ess-rutils-namepkg ()
  "Return name of the package on current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "*")
        nil
      (forward-char 2)
      (let (beg)
        (setq beg (point))
        (end-of-line) ;assume package names are separated by newlines.
        (buffer-substring-no-properties beg (point))))))

(defun ess-rutils-loadpkg ()
  "Load package from a library."
  (interactive)
  (let ((oklocal nil))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "libraries**" nil t)
          (setq oklocal t)))
    (if oklocal
        (progn
          (setq pkg (ess-rutils-namepkg))
          (ess-execute (concat "library('" pkg "', character.only=TRUE)")
                       'buffer))
      nil)))

(defun ess-rutils-repos-pkgs ()
  "List available packages from the repositories as listed by
getOptions(\"repos\") in the current R session."
  (interactive)
  (if (get-buffer ess-rutils-buf)
      (progn
        (set-buffer ess-rutils-buf)
        (setq buffer-read-only nil)))
  (ess-execute (concat "writeLines(paste('  \"', "
                       "rownames(available.packages()), '\"', sep=''))")
               nil
               (substring ess-rutils-buf 1 (- (length ess-rutils-buf) 1)))
  (pop-to-buffer ess-rutils-buf)
  (save-excursion
    (kill-line 5)
    (insert "**packages available to install**\n"))
  (setq buffer-read-only t)
  (ess-rutils-mode)
  (if (featurep 'fit-frame)
      (fit-frame)))

(defun ess-rutils-mark-install (arg)
  "Mark the current package for installing.
ARG lines to mark is passed to `ess-rutils-mark'."
  (interactive "p")
  ;; if this is not an install package buffer return nil.
  (let ((okmark nil))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "install**" nil t)
          (setq okmark t)))
    (if okmark
        (ess-rutils-mark "I" arg)
      nil)))

(defun ess-rutils-unmark (arg)
  "Unmark the packages, passing ARG lines to unmark to `ess-rutils-mark'."
  (interactive "p")
  (ess-rutils-mark " " arg))

;; The next two functions almost verbatim from ess-rdired.el.
(defun ess-rutils-mark (mark-char arg)
  "Use MARK-CHAR to mark package on current line, or next ARG lines."
  ;; If we are on first line, mark all lines.
  (let ((buffer-read-only nil)
        move)
    (if (eq (point-min)
            (save-excursion (beginning-of-line) (point)))
        (progn
          ;; we are on first line, so make a note of point, and count
          ;; how many objects we want to delete.  Then at end of defun,
          ;; restore point.
          (setq move (point))
          (forward-line 1)
          (setq arg (count-lines (point) (point-max)))))
    (while (and (> arg 0) (not (eobp)))
      (setq arg (1- arg))
      (beginning-of-line)
      (progn
        (insert mark-char)
        (delete-char 1)
        (forward-line 1)))
    (if move
        (goto-char move))))

(defun ess-rutils-install ()
  "Install all packages flagged for installation, and return to the iESS buffer.
User is asked for confirmation."
  (interactive)
  (let ((inst "install.packages(c(")
        (count 0))
    (save-excursion
      (goto-line 2)
      ;; as long as number of lines between buffer start and point is smaller
      ;; than the total number of lines in buffer, go to the beginning of the
      ;; line, check if line is flagged, and if it is, advance the counter by
      ;; one, create the root of install function, add the package name,
      ;; insert a comma, and move forward a line.
      (while (< (count-lines (point-min) (point))
                (count-lines (point-min) (point-max)))
        (beginning-of-line)
        (if (looking-at "^I ")
            (setq count (1+ count)
                  inst (concat inst (ess-rutils-namepkg) ", " )))
        (forward-line 1)))
    (if (> count 0)                     ;found packages to install
        (progn
          ;; Fix the install function created before and close it.
          (setq inst (concat
                      (substring inst 0 (- (length inst) 2)) "))"))
          ;;
          (if (yes-or-no-p (format "Install %d %s " count
                                   (if (> count 1) "packages" "package")))
              (progn
                (ess-execute inst 'buffer)
                (ess-rutils-quit))))
      ;; else nothing to install
      (message "no packages flagged to install"))))

(defun ess-rutils-update-pkgs (lib repos)
  "Update packages in library LIB and repos REPOS. Defaults are the first
element returned by .libPaths() for LIB, and the repository named CRAN
returned by getOption(\"repos\") for REPOS. This also uses checkBuilt=TRUE
to rebuild installed packages if needed."
  (interactive "DPath to library to update: \nsrepos: ")
  (if (string= "" lib)
      (setq lib
            (car (ess-get-words-from-vector
                  "as.character(.libPaths())\n"))))
  (if (string= "" repos)
      (setq repos
            (car (ess-get-words-from-vector
                  "as.character(getOption(\"repos\")[\"CRAN\"])\n"))))
  (ess-execute (concat "update.packages(lib.loc='"
                       lib "', repos='" repos
                       "', ask=FALSE, checkBuilt=TRUE)") 'buffer))

(defun ess-rutils-apropos (string)
  "Search for STRING using apropos."
  (interactive "sApropos search for? ")
  (if (get-buffer ess-rutils-buf)
      (progn
        (set-buffer ess-rutils-buf)
        (setq buffer-read-only nil)))
  (ess-execute (concat "apropos('" string "')")
               nil
               (substring ess-rutils-buf 1 (- (length ess-rutils-buf) 1)))
  (pop-to-buffer ess-rutils-buf)
  (setq buffer-read-only t)
  (ess-rutils-mode))

(defun ess-rutils-rm-all ()
  "Remove all R objects."
  (interactive)
  (if (y-or-n-p "Delete all objects? ")
      (ess-execute "rm(list=ls())" 'buffer)))

(defun ess-rutils-objs ()
  "Manipulate R objects; wrapper for `ess-rdired'."
  (interactive)
  (ess-rdired)
  (if (featurep 'fit-frame)
      (fit-frame)))

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
Documentation is produced by a modified help.start(), that returns the URL
produced by GNU R's http server.  This function is defined in a file given
by the path in variable `ess-rutils-rhtml-fn'.  If called with a prefix,
the modified help.start() is called with update=TRUE.  The optional REMOTE
argument should be a string with a valid URL for the 'R_HOME' directory on
a remote server (defaults to NULL)."
  (interactive)
  (let* ((update (if current-prefix-arg "update=TRUE" "update=FALSE"))
         (remote (if (or (and remote (not (string= "" remote))))
                     (concat "remote=" remote) "remote=NULL"))
         (rhtml (format ".rutils.help.start(%s, %s)\n" update remote))
         (tmpbuf (get-buffer-create "**ess-rutils-mode**")))
    (ess-command rhtml tmpbuf)
    (set-buffer tmpbuf)
    (let* ((begurl (search-backward "http://"))
           (endurl (search-forward "index.html"))
           (url (buffer-substring-no-properties begurl endurl)))
      (browse-url url))
    (kill-buffer tmpbuf)))

(defun ess-rutils-rsitesearch (string)
  "Search the R archives for STRING, using default criteria, and show results
using `browse-url'.  If called with a prefix, options are offered (with
completion) for matches per page, sections of the archives to search,
displaying results in long or short formats, and sorting by any given field.
Options should be separated by value of `crm-default-separator'."
  (interactive "sSearch string: ")
  (let ((site "http://search.r-project.org/cgi-bin/namazu.cgi?query=")
        (okstring (ess-replace-regexp-in-string " +" "+" string)))
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

(defun ess-rutils-help ()
  "Show help on `ess-rutils-mode'."
  (interactive)
  (describe-function 'ess-rutils-mode))

(defun ess-rutils-help-search (string)
  "Search for STRING using help.search()."
  (interactive "sString to search for? ")
  (if (get-buffer ess-rutils-buf)
      (progn
        (set-buffer ess-rutils-buf)
        (setq buffer-read-only nil)))
  (ess-execute (concat "help.search('" string "')")
               nil
               (substring ess-rutils-buf 1 (- (length ess-rutils-buf) 1)))
  (pop-to-buffer ess-rutils-buf)
  (setq buffer-read-only t)
  (ess-rutils-mode))

;; Customizable variable to allow ess-rutils-keys to activate default key bindings.
;; Suggested by Richard M. Heiberger.
(defcustom ess-rutils-keys t
  "Non-nil means activate ess-rutils keybindings and menu."
  :group 'ess-R
  :type 'boolean)

;; Keybindings
(defun ess-rutils-keys ()
  "Provide key bindings."
  (interactive)
  (when ess-rutils-keys
    (define-key inferior-ess-mode-map [(control c) (control \.) (l)]
      'ess-rutils-local-pkgs)
    (define-key inferior-ess-mode-map [(control c) (control \.) (r)]
      'ess-rutils-repos-pkgs)
    (define-key inferior-ess-mode-map [(control c) (control \.) (u)]
      'ess-rutils-update-pkgs)
    (define-key inferior-ess-mode-map [(control c) (control \.) (a)]
      'ess-rutils-apropos)
    (define-key inferior-ess-mode-map [(control c) (control \.) (m)]
      'ess-rutils-rm-all)
    (define-key inferior-ess-mode-map [(control c) (control \.) (o)]
      'ess-rutils-objs)
    (define-key inferior-ess-mode-map [(control c) (control \.) (w)]
      'ess-rutils-load-wkspc)
    (define-key inferior-ess-mode-map [(control c) (control \.) (s)]
      'ess-rutils-save-wkspc)
    (define-key inferior-ess-mode-map [(control c) (control \.) (d)]
      'ess-change-directory)
    (define-key inferior-ess-mode-map [(control c) (control \.) (H)]
      'ess-rutils-html-docs)))

(easy-menu-define ess-rutils-mode-menu inferior-ess-mode-menu
  "Submenu of `inferior-ess-mode' to use with RUtils."
  '("RUtils"
    ["Manage objects"          ess-rutils-objs          t]
    ["Remove objects"          ess-rutils-rm-all        t]
    "------"
    ["Local packages"           ess-rutils-local-pkgs   t]
    ["Packages in repositories" ess-rutils-repos-pkgs   t]
    ["Update packages"          ess-rutils-update-pkgs  t]
    "------"
    ["Load workspace"           ess-rutils-load-wkspc   t]
    ["Save workspace"           ess-rutils-save-wkspc   t]
    ["Change directory"        ess-change-directory     t]
    "------"
    ["Browse HTML"             ess-rutils-html-docs     t]
    ["Apropos"                 ess-rutils-apropos       t]))

(when (featurep 'xemacs)
  (defun ess-rutils-mode-xemacs-menu ()
    "Hook to install `ess-rutils-mode' menu for XEmacs (with easymenu)."
    (if 'inferior-ess-mode
        ;; Why does using nil for 2nd arg put menu at top level?
        (easy-menu-add-item inferior-ess-mode-menu nil
                            ess-rutils-mode-menu)
      (easy-menu-remove-item inferior-ess-mode-menu nil
                             ess-rutils-mode-menu)))
  (add-hook 'inferior-ess-mode-hook 'ess-rutils-mode-xemacs-menu t))

(unless (featurep 'xemacs)
  (easy-menu-add-item inferior-ess-mode-menu nil
                      ess-rutils-mode-menu))

(add-hook 'inferior-ess-mode-hook 'ess-rutils-keys t)

(add-hook 'ess-post-run-hook
          (lambda ()
            (ess-load-file ess-rutils-rhtml-fn)) t)


(provide 'ess-rutils)

;;; ess-rutils.el ends here
