;;; ess-r-xref.el --- An xref backend for R. -*- lexical-binding: t -*-
;;
;; Author: Aaron Jacobs
;; Created: 21 January 2018
;; Maintainer: ESS-core <ESS-core@r-project.org>
;;
;; Keywords: languages, statistics, xref
;;
;; This file is part of ESS.
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
;; https://www.r-project.org/Licenses/

;;; Commentary:

;; This file contains an xref backend for `ess-r-mode'.

;;; Code:

(require 'xref)
(require 'ess-inf)
(require 'ess-utils)
(require 'ess-r-package)
(require 'ess-tracebug)

;; Silence the byte compiler. OK because this file is only loaded by ess-r-mode.
(declare-function inferior-ess-r-force "ess-r-mode")
(declare-function ess-r-source-mode "ess-r-mode")

(defun ess-r-xref-backend ()
  "An `xref-backend-functions' implementation for `ess-r-mode'.
R's xref backend searches for `ess-r-package-library-paths' when
srcrefs point to temporary locations."
  'ess-r)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ess-r)))
  (let ((sym (ess-symbol-at-point)))
    (when sym
      (symbol-name sym))))

(cl-defmethod xref-backend-definitions ((_backend (eql ess-r)) symbol)
  (inferior-ess-r-force)
  (when (and (eq major-mode 'ess-r-mode) (string= ess-language "S"))
    (let ((tempfile (make-temp-file ".ess_attach_libs")))
      (with-temp-file tempfile
        (insert (buffer-string)))
      (ess-command (format ".ess_attach_libs(\"%s\")\n" tempfile))))
  (let ((xref (or (ess-r-xref--srcfile-xref symbol)
                  (ess-r-xref--body-xref symbol))))
    (when xref
      (list xref))))

(cl-defmethod xref-backend-apropos ((_backend (eql ess-r)))
  ;; Not yet supported.
  nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ess-r)))
  (inferior-ess-r-force)
  (ess-get-words-from-vector ".ess_all_functions()\n"))

(defun ess-r-xref--get-symbol (symbol)
  "Transform the R object SYMBOL into an explicit get() call."
  (format "base::get(\"%s\")" symbol))

(defun ess-r-xref--srcref (symbol)
  (inferior-ess-r-force)
  (let ((pkg (if (ess-r-package-name)
                 (format "%s" (ess-r-package-name))
               "NULL")))
    (with-current-buffer (ess-command (format ".ess_srcref(\"%s\", %s)\n" symbol pkg))
      (goto-char (point-min))
      (when (re-search-forward "(" nil 'noerror)
        (goto-char (match-beginning 0))
        (read (current-buffer))))))

(defun ess-r-xref--pkg-srcfile (symbol r-src-file)
  "Look in the source directory of the R package containing symbol SYMBOL for R-SRC-FILE."
  (let* ((pkg (ess-string-command (format ".ess_fn_pkg(\"%s\")\n" symbol)))
         (dir (cl-loop for d in (ess-r-package-library-paths)
                       for p = (expand-file-name pkg d)
                       when (file-exists-p p) return p))
         (file (when dir (expand-file-name r-src-file dir))))
    (if (and file (file-readable-p file))
        file)))

(defun ess-r-xref--srcfile-xref (symbol)
  "Create an xref for the source file reference of R symbol SYMBOL."
  (let ((ref (ess-r-xref--srcref symbol)))
    (when ref
      (let ((file (nth 0 ref))
            (line (nth 1 ref))
            (col  (nth 2 ref)))
        (or
         ;; 1) Result of ESS evaluation
         (let* ((ess-ref (gethash file ess--srcrefs))
                (ess-buff (when ess-ref (ess--dbg-find-buffer (car ess-ref)))))
           (when ess-buff
             ;; FIXME: this breaks when eval is on larger spans than function
             (xref-make symbol (xref-make-buffer-location ess-buff (nth 2 ess-ref)))))
         ;; 2) Actual file location
         (when (file-readable-p file)
           (xref-make symbol (xref-make-file-location file line col)))
         ;; 3) Temporary sources - truncate and locate in ess-r-package-library-paths
         (when (string-match "/\\(R/.*\\)$" file)
           (let ((pkg-file (ess-r-xref--pkg-srcfile symbol (match-string 1 file))))
             (when pkg-file
               (xref-make symbol (xref-make-file-location
                                  (expand-file-name pkg-file) line col))))))))))

(defun ess-r-xref--body-xref (symbol)
  "Creates an xref to a buffer containing the body of the R function SYMBOL."
  (let ((cmd (format "base::cat(\"%s <- \"); base::print.function(%s)\n"
                     symbol (ess-r-xref--get-symbol symbol)))
        (buff (get-buffer-create (format "*definition[R]:%s*" symbol))))
    (ess-with-current-buffer (ess-command cmd buff)
      (ess-r-source-mode)
      (add-hook 'xref-backend-functions #'ess-r-xref-backend nil 'local))
    (xref-make symbol (xref-make-buffer-location buff 0))))

(provide 'ess-r-xref)

;;; ess-r-xref.el ends here
