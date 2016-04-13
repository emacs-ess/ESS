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
;; Define generic method with `ess-defmethod' and dialect specific override with
;; `ess-defun`. Current implementation is using `{name}-function' and
;; `ess-customize-alist' as the backbone mechanism. This might change in the
;; future.
;;
;;; Code

(defmacro ess-defmethod (name args docstring &rest body)
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
      (defun ,name ,args ,(format "%s\nUse `ess-defun' to define dialect specific overrides." docstring)
             (if (fboundp ,funname)
                 (funcall ,funname ,@args)
               ,@(or body
                     `((error (format "`%s' is not implemented for dialect `%s'"
                                      ',name ess-dialect)))))))))

(defmacro ess-defun (name dialect args &rest body)
  "Define a dialect specific override of the method NAME.
If NAME wasn't created with `ess-defmethod' signal an
error. DIALECT is the dialect name this override is being defined
for. ARGS are the function arguments, which should match those of
the same named function created with `ess-defmethod'. BODY is the
implementation of this function."
  (declare (indent defun) (debug defun))
  (let ((new-name (intern (format "%s:%s" name dialect)))
        (local-var-name (intern (format "%s-function" name)))
        (cust-alist-name (intern (downcase (format "ess-%s-customize-alist" dialect)))))
    (unless (fboundp name)
      (error "Use `ess-defmethod' to define `%s' generic first"))
    `(eval-and-compile
       (unless (boundp ',cust-alist-name)
         (defvar ,cust-alist-name nil
           ,(format "Variables to customize dialect '%s'." dialect)))
       (add-to-list ',cust-alist-name
                    '(,local-var-name . ,new-name))
       (defun ,new-name ,args
         ,(format "%s\nThis is an override for `%s' for `%s' dialect."
                  (replace-regexp-in-string "\nUse.*ess-defun.*\\." "" (documentation name))
                  name dialect)
         ;; The body for this implementation
         ,@body)
       ;; For find-func to locate the definition of NEW-NAME.
       (put ',new-name 'definition-name ',name))))


(provide 'ess-generics)
