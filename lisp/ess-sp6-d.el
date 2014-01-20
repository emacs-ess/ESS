;;; ess-sp6-d.el --- S-Plus 6 & 7 & 8  customization

;; Copyright (C) 2001--2005 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@u.washington.edu>
;; Created: 2001/02/06
;; Maintainer: ESS Core Team <ESS-core@r-project.org>

;; Keywords: languages

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

;; AJR copied S+5 to be S+6.
;; AJR copied S4 to be S+5.
;; DB contributed the changes from ess-sp3-d.el to
;; ess-s4-d.el. (removed the old ugly approach).
;; This file defines Sp5 customizations for ess-mode.  Lots of thanks
;; to RMH and JMC for code and suggestions
;; Thanks to MM for making this sensible.

;;; Code:

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

(require 'ess-s-l)

;; You now need to make sure you've defined if you are running 5.0 or 5.1.
;; Lots of things are broken between them, GRR...

(defun S+-directory-p (directory)
  "Splus 5++ directories have a .Data directory and a __Meta directory within."
  (and directory
       (file-directory-p (concat directory ".Data"))
       (file-directory-p (concat directory ".Data/__Meta"))))

(defvar S+-directory-function
  (lambda ()
    (if (S+-directory-p default-directory)
        default-directory
      (or ess-directory default-directory))))

(defvaralias 'S+6-setup-directory-function 'S+-setup-directory-function)
(defvar S+-setup-directory-function
  (lambda (startdir)
    (when (and startdir (S+-directory-p startdir))
      (setenv "S_WORK"
              (if (getenv "S_WORK")
                  (concat startdir ":" (getenv "S_WORK"))
                ;;(message "adding %s to S_WORK" startdir)
                startdir)))))



(defvaralias 'S+6-customize-alist 'S+-customize-alist)
(defvar S+-customize-alist
  (append
   '((ess-local-customize-alist         . 'S+-customize-alist)
     (ess-dialect                       . S+-dialect-name)
     (ess-loop-timeout                  . ess-S-loop-timeout);fixme: dialect spec.
     (ess-function-pattern              . ess-R-function-pattern)

     (ess-object-name-db-file           . "ess-sp6-namedb.el")
     (inferior-ess-program              . inferior-S+-program-name)
     (inferior-ess-help-command   . "help(\"%s\", pager=\"slynx -dump\", window=FALSE)\n")
     (inferior-ess-help-filetype . nil)
     (inferior-ess-search-list-command  . "searchPaths()\n")

     (ess-directory-function            . S+-directory-function)
     (ess-setup-directory-function      . S+-setup-directory-function)
     (inferior-ess-start-args       . inferior-S+-start-args)
     (ess-STERM  . "iESS")
     )
   S+common-cust-alist)

  "Variables to customize for S+.")

(defvar ess-S+-post-run-hook nil
  "Functions run in process buffer after the initialization of S+
  process.")

(defalias 'S+6 'S+)
(defun S+ (&optional proc-name)
  "Call 'Splus6', based on S version 4, from Bell Labs.
New way to do it."
  (interactive)
  (setq ess-customize-alist S+-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(S+): ess-dialect=%s, buf=%s\n" ess-dialect (current-buffer)))
  (inferior-ess)
  (ess-command ess-S+--injected-code)
  (if inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start))
  (with-ess-process-buffer nil
    (run-mode-hooks 'ess-S+-post-run-hook)))

(defvar ess-S+--injected-code
  ".ess_funargs <- function(funname){
  ## funname <- deparse(substitute(object))
  fun <- try(eval(parse(text=funname)), silent = TRUE)
  if(is.function(fun)) {
    special <- grepl('[:$@[]', funname)
    args <- args(fun)
    fmls <- formals(args)
    fmls.names <- names(fmls)
    fmls <- gsub('\\\"', '\\\\\\\"', as.character(fmls), fixed = TRUE)
    args.alist <- sprintf(\"'(%s)\", paste(\"(\\\"\", fmls.names, \"\\\" . \\\"\", fmls, \"\\\")\", sep = '', collapse = ' '))
    ## envname <- environmentName(environment(fun))
    envname <-  if (special) '' else 'S+'
    cat(sprintf('(list \\\"%s\\\" %s )\\n', envname, args.alist))
  }
}
")


(defalias 'S+6-mode 'S+-mode)
(defun S+-mode (&optional proc-name)
  "Major mode for editing S+ source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S+-customize-alist)
  (ess-mode S+-customize-alist proc-name)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  (if ess-imenu-use-S (ess-imenu-S)))

(defalias 'S+6-transcript-mode 'S+-transcript-mode)
(defun S+-transcript-mode ()
  "S-PLUS 6 transcript mode."
  (interactive)
  (ess-transcript-mode S+-customize-alist))

(defvar ess-s-versions-list nil
  "List of other versions of S to add to ESS.
Each element of this list is itself a list:
  \(FUNCTION PATH ARGS\)
e.g.
  \(\"mysplus\" \"/usr/splus7/bin/splus7\" \"-j\"\)
FUNCTION is the name of the function to be created by Emacs.
PATH is the full path to the variant of S that you want to run.
ARGS (optional) are start-up arguments that you want to pass to S.
")

(defvar ess-s-versions '("Splus")
  "List of partial strings for versions of S to access within ESS.
Each string specifies the start of a filename.  If a filename
beginning with one of these strings is found on `exec-path', a M-x
command for that version of S is made available.  For example, if the
file \"Splus7\" is found and this variable includes the string
\"Splus\", a function called `M-x Splus7' will be available to run that
version of S.
If duplicate versions of the same program are found (which happens if
the same path is listed on `exec-path' more than once), they are
ignored by calling `ess-uniq-list'.
Set this variable to nil to disable searching for other versions
of S using this method.
If you set this variable, you need to restart Emacs (and set this variable
before ess-site is loaded) for it to take effect.

See also `ess-s-versions-list' for another way to add other S
processes to ESS. ")

(defun ess-s-versions-create ()
  "Generate defuns for starting other versions of S.
See `ess-s-versions' for strings that determine which functions are created.
It assumes these versions of S can be run as a substitute for Splus6.

This function returns the list of S defuns, if any, that were
created.  The defuns will normally be placed on the menubar upon
ESS initialisation."

  ;; This works by creating a temp buffer where the template function is
  ;; edited so that X.Y is replaced by the version name
  (let ((template "")
        (template-args)
        (beg)
        (versions)
        (version)
        (eval-buf (get-buffer-create "*ess-temp-s-evals*"))
        (ess-s-versions-created)

        (ess-s-versions-list ess-s-versions-list)
        ;; make local copy so it won't be destroyed globally
        )
    ;;
    ;; This is the template function used for creating M-x Splus
    (setq template "(defun S-X.Y ()
  \"Call S-X.Y, i.e., the S version 'S-X.Y' using ESS.
This function was generated by `ess-s-versions-create'.\"
  (interactive \"\")
  (let ((inferior-S+-program-name \"S-X.Y\"))
    (S+)))

")
    (with-current-buffer eval-buf
      ;; clear the buffer.
      (delete-region (point-min) (point-max))

      (when ess-s-versions
        ;; Find which versions of S we want.  Remove the pathname, leaving just
        ;; the name of the executable.
        (setq versions
              (ess-uniq-list
               (mapcar 'file-name-nondirectory
                       (apply 'nconc
                              (mapcar 'ess-find-exec-completions
                                      ess-s-versions)))))
        (ess-write-to-dribble-buffer
         (format "(S): ess-s-versions-create making M-x defuns for \n %s\n"
                 (mapconcat 'identity versions "\n ")))
        (setq ess-s-versions-created versions) ;keep copy for returning at end.
        ;; Iterate over each string in VERSIONS, creating a new defun each time.
        (while versions
          (setq version (car versions)
                versions (cdr versions))
          (setq beg (point))
          (insert template)
          (goto-char beg)
          (while (search-forward "S-X.Y" nil t)
            (replace-match version t t))
          (goto-char (point-max))
          ))

      ;; Check if we have any static defuns to evaluate.
      (when ess-s-versions-list

        ;; Need a slightly different template for static defuns.
        (setq template "(defun S-X.Y ()
  \"Call S-X.Y, i.e., the S version 'S-X.Y' using ESS.
This function will run S-FULL-PATH
This function was generated by `ess-s-versions-create'.\"
  (interactive \"\")
  (let ((inferior-S+-program-name \"S-FULL-PATH\"))
    (S+)))

")
        ;; need another version of template, with args.
        (setq template-args "(defun S-X.Y ()
  \"Call S-X.Y, i.e., the S version 'S-X.Y' using ESS.
This function will run S-FULL-PATH
This function was generated by `ess-s-versions-create'.\"
  (interactive \"\")
  (let ((inferior-S+-program-name \"S-FULL-PATH\")
        (inferior-S+-start-args \"S-MYARGS\"))
    (S+)))

")
        (while ess-s-versions-list
          (let* ((this-S-version (car ess-s-versions-list))
                 (S-defun (nth 0 this-S-version))
                 (S-path  (nth 1 this-S-version))
                 (S-args  (nth 2 this-S-version)))
            (setq ess-s-versions-list (cdr ess-s-versions-list))
            ;; Could do error checking here, that S-defun is not defined
            ;; before, and that S-path is valid.
            (setq beg (point))
            (insert
             (if S-args
                 template-args
               template))
            (goto-char beg)
            (while (search-forward "S-X.Y" nil t)
              (replace-match S-defun t t))
            (goto-char beg)
            (while (search-forward "S-FULL-PATH" nil t)
              (replace-match S-path t t))
            (when S-args
              (goto-char beg)
              (while (search-forward "S-MYARGS" nil t)
                (replace-match S-args t t)))
            (goto-char (point-max))
            (add-to-list 'ess-s-versions-created S-defun 'append))))

      ;; buffer has now been created with defuns, so eval them!
      (eval-buffer)
      (kill-buffer eval-buf); < comment this for debugging
      ess-s-versions-created)))

 ; Provide package

(provide 'ess-sp6-d)

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

;;; ess-sp6-d.el ends here
