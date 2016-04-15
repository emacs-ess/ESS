;;; ess-generics.el --- Mode-generic functions
;;
;; Copyright (C) ESS-core <ESS-core@r-project.org>
;; Maintainer: ESS-core <ESS-core@r-project.org>
;;
;; This file is part of ESS
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/
;;
;;; Comment
;;
;; Define generic method with `ess-defgeneric' and dialect specific override with
;; `ess-defmethod`. Current implementation is using `{name}-function' and
;; `ess-customize-alist' as the backbone mechanism. This might change in the
;; future.
;;
;;; Code

(defun ess-generics--override (name args body)
  (let ((funname (intern (format "%s-function" name)))
        (arg-list (delq '&rest (delq '&optional (copy-alist args)))))
    `(if (fboundp ,funname)
         ,(if (memq '&rest args)
              `(apply ,funname ,@arg-list)
            `(funcall ,funname ,@arg-list))
       ,@(or body
             `((error (format "`%s' is not implemented for dialect `%s'"
                              ',name ess-dialect)))))))

(defun ess-generics--expand-overrides (name args body)
  ;; ripped off from `mode-local--expand-overrides'
  (let ((forms body)
        (ditto t)
        form xbody)
    (while forms
      (setq form (car forms))
      (cond
       ((atom form))
       ((eq (car form) :override)
        (setq form (ess-generics--override name args (cdr form))))
       ((eq (car form) :override-with-args)
        (setq form (ess-generics--override name (cadr form) (cddr form))))
       ((setq form (ess-generics--expand-overrides name args form))))
      (setq ditto (and ditto (eq (car forms) form))
            xbody (cons form xbody)
            forms (cdr forms)))
    (if ditto body (nreverse xbody))))

(defmacro ess-defgeneric (name args docstring &rest body)
  "Define a new function, as with `defun', which can be overloaded.
NAME is the name of the function to create. ARGS are the
arguments to the function. DOCSTRING is a documentation string to
describe the function.  The docstring will automatically have
details about its overload symbol appended to the end. BODY is
code that would be run when there is no override defined.  The
default is to signal error if {name}-function is not defined."
  (declare (doc-string 3) (indent defun) (debug defun))
  (let ((funname (intern (format "%s-function" name))))
   `(eval-and-compile
      (defvar-local ,funname nil ,(format "When defined this function is called by `%s'." name))
      (defun ,name ,args ,(format "%s\n\nUse `ess-defmethod' to define dialect specific overrides." docstring)
             ,@(ess-generics--expand-overrides name args body)))))

(defmacro ess-defmethod (dialect name args &rest body)
  "Define a dialect specific override of the method NAME.
If NAME wasn't created with `ess-defgeneric' signal an
error. DIALECT is the dialect name this override is being defined
for. ARGS are the function arguments, which should match those of
the same named function created with `ess-defgeneric'. BODY is the
implementation of this function."
  (declare (indent defun) (debug (&define sexp sexp lambda-list def-body)))
  (let ((new-name (intern (format "%s:%s" name dialect)))
        (fun-name (intern (format "%s-function" name)))
        (alist-name (intern (downcase (format "ess-%s-customize-alist" dialect)))))
    `(eval-and-compile
       (unless (boundp ',alist-name)
         (defvar ,alist-name nil
           ,(format "Variables to customize dialect '%s'." dialect)))
       (add-to-list ',alist-name
                    '(,fun-name . ',new-name))
       (defun ,new-name ,args
         ,(format "%s\nThis is an override for `%s' for `%s' dialect."
                  ;; fixme: NAME might be undefined as yet. Look at help-fns-describe-function-functions
                  (or (and (fboundp name)
                           (documentation name)
                           ;; hackish
                           (replace-regexp-in-string "\nUse.*ess-defmethod.*\\." "" (documentation name)))
                      "")
                  name dialect)
         ;; The body for this implementation
         ,@body)
       ;; For find-func to locate the definition of NEW-NAME.
       (put ',new-name 'definition-name ',name))))



(provide 'ess-generics)
