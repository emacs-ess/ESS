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
;;  To understand how ess-developer works you must be familiar with namespace
;;  system in R.
;;
;;  In a nutshell, all objects defined in a package 'foo' are stored in an
;;  environment called 'namespace:foo'. Parent environment of 'namespace:foo' is
;;  an environment 'imports:foo' which contains copies of all objects from other
;;  packages which 'foo' imports. Parent environment of 'imports:foo' is the
;;  'namespace:base'. Parent environment of 'namespace:base' is .GlobalEnv. Thus
;;  functions and methods stored in 'namespace:foo' see all the objects in
;;  .GlobalEnv unless shadowed by objects in 'imports:foo', 'namespace:base', or
;;  'namespace:foo' itself. There is another environment associated with 'foo' -
;;  'package:foo'. This environment stores *copies* of exported objects from
;;  'namespace:foo' and is placed on the search() path, i.e. if 'foo' is loaded
;;  and if you start with .GlobalEnv and iteratively call parent.env() you will
;;  get eventually to 'package:foo'. Thus all methods and functions defined in
;;  .GlobalEnv can see the objects in 'package:foo'. See also
;;  http://cran.r-project.org/doc/manuals/R-ints.html#Namespaces
;;

;;  In order to use ess-developer mode you should add names of the packages you
;;  are developing to `ess-developer-packages'. You can add packages
;;  interactively with "C-M-d a" and remove with "C-M-d r".
;;
;;  The `ess-developer-prefix' is by default "C-M-d", you can customize it.
;;  Bindings are in `ess-developer-map':
;;
;;  "t" to toggle developer mode
;;  "a" to add a package to your development list
;;  "r" to remove a package from your  development list
;;  "s" or "C-c l" to 'source' current file into the namespace (asks for the package)
;;
;; In develper mode several ess commands behave differently:
;;
;; -- `ess-eval-function' and frends check if the current function's name can be
;;    found in a namespace:foo or package:foo where 'foo' is listed in
;;    `ess-developer-packages'.  If found, and new function definition differs
;;    from the old one, the function is assigned into that namespace. If not found, it is
;;    assigned into .GlobalEnv.
;;
;; -- `ess-load-file' [C-c l] (`ess-dbg-source-current-file' [M-c s] if
;;    ess-tracebug is active todo:not implemented yet) asks for the package to
;;    source into and inserts all redefined objects into the package:foo or
;;    namespace:foo accordingly:
;;
;;    -- PLAIN OBJECTS and FUNCTIONS: If the object is found in an environment
;;    (packag:foo or namespace:foo), and differs from the old one it is assigned
;;    into the corresponding environment. If the object is not found, it is
;;    assigned into .GlobalEnv. The environment of functions is set to
;;    namespace:foo.
;;
;;    -- CLASSES: same as plain objects, with the difference that even if the
;;    class definition is assigned into .GlobalEnv, it is still associated with
;;    the package foo. Thus if you issue getClassDeff("foo") you will get a
;;    class definition with the slot @package pointing to package "foo".
;;
;;       Note: Occasionally, after adding new classes you might get warnings
;;      from "setClass". This is especially true if new class inherits or is
;;      inherited by a class whose definition is not exported. You might get
;;      something like:
;;
;;         Warning: Class "Boo" is defined (with package slot ‘foo’) but no
;;         metadata object found to revise subclass information---not exported?
;;
;;      In my experience you can safely ignore this warnings.
;;
;;    -- METHODS: Similarly to function definitions modified methods are
;;    assigned in the local method table in the namespace:foo. New methods are
;;    assigned into .GlobalEnv, but with the environment pointing to
;;    namespace:foo. There is a catch with method caching which R uses. See
;;    ess-developer.R for more detailed comments.
;;
;;    Note that if method or generic is exported the *same* table (which is an
;;    environment) is present in package:foo.
;;
;; -- `ess-eval-region' acts as `ess-load-file' but only on the region
;;     todo: not implemented yet


(require 'ess-site) ;; need to assigne the keys in the map

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


(defcustom ess-developer-prefix  "\C-\M-d"
  "Prefix key for ess-developer actions.

Action keys are defined in `ess-developer-map':

\\{ess-developer-map}

It should be a string in the format accepted by define-key such
as '\C-cz'.

Set this to nil if you don't want ess-developer-map to be
installed in ess-mode-map altogether.
"
  :group 'ess-developer
  :type 'string)

(defvar ess-developer-map
  (let ((map (make-sparse-keymap)))
    (define-prefix-command 'map)
    (define-key map "t" 'ess-developer)
    (define-key map "a" 'ess-developer-add-package)
    (define-key map "r" 'ess-developer-remove-package)
    (define-key map "s" 'ess-developer-source-current-file)
    map)
  "Ess-developer keymap.")

(defun ess-developer-install-prefix-key ()
  "Install the prefix key `ess-developer-prefix' into ess-mode-map."
  (when (and ess-developer-prefix
	     (equal ess-dialect "R"))
    (define-key ess-mode-map ess-developer-prefix ess-developer-map)
    (define-key inferior-ess-mode-map ess-developer-prefix ess-developer-map)
    ))

(add-hook 'inferior-ess-mode-hook 'ess-developer-install-prefix-key)

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

(defcustom ess-developer-force-attach nil
  "If non-nill all the packages listed in `ess-developer-packages' are attached,
when ess-developer mode is turned on."
  :group 'ess-developer
  :type 'boolean)

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
      (message "Nothing to remove, 'ess-developer-packages' is empty")
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
  (ess-developer-add-package t))

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

(defun ess-developer-source-current-file ()
  "Ask for namespace to source the current file into.
If *current* is selected just invoke source('file_name'),
otherwise call insertSource."
  (interactive)
  (ess-force-buffer-current "R process to use: ")
  (unless (ess-get-process-variable ess-current-process-name 'ess--developer-p)
    (error "Ess-developer mode is not active"))
  (if (not buffer-file-name)
      (message "Buffer '%s' doesn't visit a file" (buffer-name (current-buffer)))
    (let* ((file (file-name-nondirectory buffer-file-name))
	   (env (ess-completing-read (format "insertSource '%s' into: " file)
				    (append ess-developer-packages (list "*current*" ))
				    nil t))
	   (comm  (if (equal env "*current*")
		      (format "\ninvisible(eval({source(file=\"%s\")\n cat('Sourced file  '%s'\n!@OK@!\n')}))\n"
			      buffer-file-name file)
		    (format "invisible(eval({require('methods');cat('\n')\n.essDev_sourceInNamespace(source='%s',package='%s')\ncat('!@OK@!\n')}))\n"
			    buffer-file-name env))))
      (save-buffer)
      (ess--developer-command comm 'ess--developer-propertize-output)
      )))

(defun ess--developer-command (comm &optional propertize-func)
  "Evaluate the command and message if !@OK@! is found at the end.
On error (when !@OK@! not found) insert the error  at the end of inferior-ess
  buffer.

PROPERTIZE-FUNC is a function called with the output buffer being current.
usually used to manipulate the output, for example insert some text properties.
"
  (let ((buff (get-buffer-create "*ess-command-output*"))
	out)
    (ess-command comm buff)
    (with-current-buffer buff
      (goto-char (point-min))
      (delete-region (point) (min (point-max)
      				  (1+ (point-at-eol))))
      (goto-char (point-max))
      (if (re-search-backward "!@OK@!" nil t)
	  (progn
	    (when (fboundp propertize-func)
	      (save-excursion (funcall propertize-func)))
	    (message "%s" (buffer-substring (point-min) (max (point-min)
							     (1- (point))))))
	(setq out (buffer-substring-no-properties (point-min) (point-max)))
	(save-selected-window
	  ;; if error show it in inferior-ess buffer
	  (ess-switch-to-ESS t)
	  (let ((proc (get-process ess-local-process-name)))
	    (goto-char (process-mark proc))
	    (insert (format "%s\n> " out))
	    (set-marker (process-mark proc) (point)))
	  ))
      )))

(defun ess--developer-propertize-output ()
  (goto-char (point-min))
  (while (re-search-forward "\\([FCMG]\\)\\[" nil t)
    (put-text-property (match-beginning 1) (match-end 1) 'face 'font-lock-function-name-face))
  (goto-char (point-min))
  (while (re-search-forward "^>\\(.+\\):" nil t)
    (put-text-property (match-beginning 1) (match-end 1) 'face 'font-lock-keyword-face)
    ))

(defun ess-developer (&optional val)
  "Toggle on/off ess-developer functionality.
If optional VAL is non-negative, turn on the developer mode. If
VAL is negative turn it off.

See the preamble of the source file for more info. It will get here eventually. todo:
"
  (interactive)
  (when (eq val t) (setq val 1))
  (ess-force-buffer-current "Process to load into: " t)
  (with-current-buffer (process-buffer (get-process ess-current-process-name))
    (let ((ess-dev  (if (numberp val)
                        (if (< val 0) nil t)
                      (not ess--developer-p)))
	  (devR-file (concat (file-name-directory ess-lisp-directory)
			     "etc/ess-developer.R")))
      (if ess-dev
          (progn
	    (unless (or (file-exists-p devR-file)
			;; (setq ess-dev (locate-file "ess-developer.R" load-path))
			)
	      (error "Cannot locate 'ess-developer.R' file"))
	    (ess--developer-command
	     (format "local({cat('\nSourcing %s ....')\nsource('%s')\ncat('!@OK@!\n')})\n"
		     devR-file devR-file))
            (run-hooks 'ess-developer-enter-hook)
            (when (file-readable-p ess-developer-enter-source)
              (ess-eval-linewise (format "source(%s)\n" ess-developer-enter-source)))
	    (unless (fboundp 'orig-ess-load-file)
	      (defalias 'orig-dev-ess-load-file (symbol-function 'ess-load-file))
	      (defalias 'ess-load-file (symbol-function 'ess-developer-source-current-file)))
            (message "Developer mode is on"))
        (run-hooks 'ess-developer-exit-hook)
        (when (file-readable-p ess-developer-exit-source)
          (ess-eval-linewise (format "source(%s)\n" ess-developer-exit-source)))
	(when (fboundp 'orig-ess-load-file)
	  (defalias 'ess-load-file (symbol-function 'orig-ess-load-file))
	  (fmakunbound 'orig-ess-load-file))
        (message "Developer mode is off"))
      (setq ess--developer-p ess-dev))
    (setq ess-local-process-name
          (if ess--developer-p
              (propertize ess-local-process-name 'face 'ess-developer-indicator-face)
            (propertize  ess-local-process-name 'face nil)))
    ))

(provide 'ess-developer)