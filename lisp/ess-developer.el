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


;; (defcustom ess-developer-prefix  "\C-\M-d"
;;   "Prefix key for ess-developer actions.

;; Action keys are defined in `ess-developer-map':

;; \\{ess-developer-map}

;; It should be a string in the format accepted by define-key such
;; as '\C-cz'.

;; Set this to nil if you don't want ess-developer-map to be
;; installed in ess-mode-map altogether.
;; "
;;   :group 'ess-developer
;;   :type 'string)

(defvar ess-developer-map
  (let (ess-developer-map)
    (define-prefix-command 'ess-developer-map)
    (define-key ess-developer-map "t" 'ess-developer)
    (define-key ess-developer-map "a" 'ess-developer-add-package)
    (define-key ess-developer-map "r" 'ess-developer-remove-package)
    (define-key ess-developer-map "s" 'ess-developer-source-current-file)
    ess-developer-map)
  "Ess-developer keymap.")

(define-key ess-mode-map "\C-cd"                ess-developer-map)
(define-key inferior-ess-mode-map "\C-cd"       ess-developer-map)


;; (defun ess-developer-install-prefix-key ()
;;   "Install the prefix key `ess-developer-prefix' into ess-mode-map."
;;   (when (and ess-developer-prefix
;;           (equal ess-dialect "R"))
;;     (define-key ess-mode-map ess-developer-prefix ess-developer-map)
;;     (define-key inferior-ess-mode-map ess-developer-prefix ess-developer-map)
;;     ))

;; (add-hook 'inferior-ess-mode-hook 'ess-developer-install-prefix-key)

;; (defvar ess--developer-p nil
;;   "t if ESS is in developer mode for current process.
;; Use `ess-developer' to set this variable.
;; ")
;; (make-variable-buffer-local 'ess--developer-p)

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
                   (ess-completing-read "Remove pakage(s)"
                                        (append ess-developer-packages (list "*ALL*"))
                                        nil t nil nil "*ALL*")
                 (ess-completing-read "Add package"
                                      (ess-get-words-from-vector "print(.packages(TRUE),max=1e6)\n") nil t)
                 )))
      (if remove
          (if (equal "*ALL*" sel)
              (progn
                (setq ess-developer-packages nil)
                (message "Removed *ALL* packages from the `ess-developer-packages' list."))
            (setq ess-developer-packages (delete sel ess-developer-packages))
            (message "Removed package '%s' from the  ess-`developer-packages' list" (propertize sel 'face 'font-lock-function-name-face)))
        (setq ess-developer-packages (ess-uniq-list (append  ess-developer-packages (list sel))))
        (ess-eval-linewise (format "library('%s')" sel))
        (message "You are developing: %s" ess-developer-packages)
        ))))

(defun ess-developer-remove-package ()
  "Remove a package from `ess-developer-packages' list."
  (interactive)
  (ess-developer-add-package t))

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
  (unless (process-get (get-process ess-local-process-name) 'developer)
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
                    (format "require('methods');.essDev_source(source='%s',package='%s')" filename env))))
      (when (buffer-modified-p) (save-buffer))
      (message "devSourcing '%s' ..." file)
      (ess--developer-command comm 'ess--developer-propertize-output)
      )))

(defun ess-developer-send-function (proc beg end name &optional visibly message tracebug)
  (save-excursion
    (if (null ess-developer-packages)
        (error "Add packages to `ess-developer-packages' first! ")
      (if (null name)
          (error "Oops, could not find function name (probably a regexp bug)")
        (let ((nms (ess-get-words-from-vector "loadedNamespaces()\n"))
              (dev-packs ess-developer-packages)
              assigned-p ns)
          (if tracebug (ess-tb-set-last-input proc))
          (while (and (setq ns (pop dev-packs))
                      (not assigned-p))
            (when (and (member ns nms) ;;todo: try to load the package if not loaded
                       (equal "TRUE" (car (ess-get-words-from-vector
                                           (format "as.character(exists('%s', envir=asNamespace('%s'), mode='function',inherits=FALSE))\n" name ns)))))
              (let ((comm (if tracebug
                              (ess--tb-get-source-refd-string beg end)
                            (buffer-substring beg end))))
                (ess-developer-devSource-string proc comm ns)
                (setq assigned-p t)
                )))
          (unless assigned-p
            (ess-developer-send-region-fallback proc beg end visibly message tracebug))
          )))))

(defun ess-developer-send-region (proc beg end &optional visibly message tracebug)
  "Ask for for the package and devSource region into it."
  (let ((package
         (ess-completing-read "devEval into"
                              (append ess-developer-packages (list "*current*" )) nil t))
        (message  (if message (format "dev%s ..." message))))
    (if (equal package "*current*")
        (ess-developer-send-region-fallback proc beg end visibly message tracebug))
    ;; else, (ignore VISIBLY here)
    (let ((comm  (if tracebug
                     (ess--tb-get-source-refd-string beg end)
                   (buffer-substring-no-properties beg end))))
      (ess-developer-devSource-string proc comm package message))))

(defun ess-developer-devSource-string (proc command package &optional mess)
  "devSource COMMAND into the PACKAGE.
String must be quoted with `ess-quote-special-chars'."
  ;; assumes a started process
  (unless (process-get proc 'developer)
    (error "Ess-developer mode is not active"))
  (let ((comm  (format ".essDev_source(expr={%s}, package='%s')"
                       command package)))
    (if mess (message mess))
    (ess--developer-command comm  'ess--developer-propertize-output)
    ))


(defun ess--developer-command (comm &optional propertize-func)
  "Evaluate the command and popup a message with the output if succed.
On error  insert the error at the end of the inferior-ess buffer.

PROPERTIZE-FUNC is a function called with the output buffer being current.
usually used to manipulate the output, for example insert some text properties.
"
  (setq comm (format "eval({cat('\\n')\n%s\ncat('!@OK@!')})\n" comm))
  (let ((buff (get-buffer-create "*ess-command-output*"))
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
        (setq out (buffer-substring-no-properties (point-min) (point-max)))
        (save-selected-window
          ;; if error show it in inferior-ess buffer
          (ess-switch-to-ESS t)
          (let ((proc (get-process ess-local-process-name)))
            (goto-char (process-mark proc))
            (insert (format "%s\n> " out))
            (set-marker (process-mark proc) (point))))
        (error "Error occurred; dumped into process buffer")
        )
      )))

(defun ess--developer-propertize-output ()
  (goto-char (point-min))
  (while (re-search-forward "\\(FUN\\|CLS\\METH\\)\\[" nil t)
    (put-text-property (match-beginning 1) (match-end 1) 'face 'font-lock-function-name-face))
  (goto-char (point-min))
  (while (re-search-forward "^\\(\\w.+\\):" nil t)
    (put-text-property (match-beginning 1) (match-end 1) 'face 'font-lock-keyword-face)
    ))

(defun ess-developer (&optional val)
  "Toggle on/off ess-developer functionality.
If optional VAL is non-negative, turn on the developer mode. If
VAL is negative turn it off.

See the preamble of the source file for more info. It will get
here eventually. todo:
"
  (interactive)
  (when (eq val t) (setq val 1))
  (ess-force-buffer-current "Process to load into: " nil t)
  (let* ((proc (get-process ess-local-process-name))
         (developer-p (process-get proc 'developer))
         (ess-dev  (if (numberp val)
                       (if (< val 0) nil t)
                     (not developer-p)))
         (devR-file (concat (file-name-directory ess-etc-directory)
                            "ess-developer.R")))
    (if ess-dev
        (progn
          (unless (or (file-exists-p devR-file)
                      ;; (setq ess-dev (locate-file "ess-developer.R" load-path))
                      )
            (error "Cannot locate 'ess-developer.R' file"))
          (ess-command  (format "source('%s')\n" devR-file))
          (run-hooks 'ess-developer-enter-hook)
          (when (file-readable-p ess-developer-enter-source)
            (ess-eval-linewise (format "source(%s)\n" ess-developer-enter-source)))
          (message "Developer mode is on"))
      (run-hooks 'ess-developer-exit-hook)
      (when (file-readable-p ess-developer-exit-source)
        (ess-eval-linewise (format "source(%s)\n" ess-developer-exit-source)))
      (message "Developer mode is off"))
    (process-put proc 'developer ess-dev)
    (with-current-buffer (process-buffer proc)
      (setq ess-local-process-name
            (if ess-dev
                (propertize ess-local-process-name 'face 'ess-developer-indicator-face)
              (propertize  ess-local-process-name 'face nil))))
    ))

(provide 'ess-developer)

;;; ess-developer.el ends here
