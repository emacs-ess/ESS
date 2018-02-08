;;; ess-r-xref.el --- An xref backend for R. -*- lexical-binding: t -*-
;;
;; Author: Aaron Jacobs
;; Created: 21 January 2018
;; Maintainer: ESS-core <ESS-core@r-project.org>
;;
;; Keywords: languages, statistics, xref
;; Package-Requires: ((emacs "25"))
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
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; This file contains an xref backend for `R-mode'.

;;; Code:

(require 'subr-x)
(require 'xref)
(require 'ess-utils)
(require 'ess-tracebug)



(defun ess-r-xref-backend ()
  "An `xref-backend-functions' implementation for `R-mode'."
  'ess-r)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ess-r)))
  (symbol-name (ess-symbol-at-point)))

(cl-defmethod xref-backend-definitions ((_backend (eql ess-r)) symbol)
  (let ((xref (ess-r-xref--xref symbol)))
    (when xref (list xref))))

(cl-defmethod xref-backend-apropos ((_backend (eql ess-r)))
  ;; Not yet supported.
  nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ess-r)))
  (inferior-ess-r-force)
  (ess-get-words-from-vector ".ess_all_functions()\n"))


;;; Source File Locations

(defcustom ess-r-xref-pkg-sources nil
  "Alist of R packages and directories of their source code."
  :type '(alist :key-type string :value-type directory))

(defun ess-r-xref--srcref (symbol)
  (inferior-ess-r-force)
  (with-current-buffer (ess-command (format ".ess_srcref(\"%s\")\n" symbol))
    (goto-char (point-min))
    (when (re-search-forward "(" nil 'noerror)
      (goto-char (match-beginning 0))
      (read (current-buffer)))))

(defun ess-r-xref--pkg-srcfile (symbol r-src-file)
  "Look in the source directory of the R package containing symbol SYMBOL for R-SRC-FILE."
  (let* ((env-name (ess-string-command (format ".ess_fn_pkg(\"%s\")\n" symbol)))
         (pkg (if (string-equal env-name "")
                  (error "Can't find package for symbol %s." symbol)
                env-name))
         (dir (or (assoc-default pkg ess-r-xref-pkg-sources)
                  (when ess-r-package-library-path
                    (expand-file-name pkg ess-r-package-library-path))))
         (file (when dir (expand-file-name r-src-file dir))))
    (when file
      (if (file-readable-p file)
          (progn
            ;; Keep track of the package's source directory.
            (unless (assoc-default pkg ess-r-xref-pkg-sources)
              (push `(,pkg . ,dir) ess-r-xref-pkg-sources))
            file)
        (error "Can't read %s." file)))))

(defun ess-r-xref--xref (symbol)
  "Create an xref for the source file reference of R symbol SYMBOL."
  (let ((ref (ess-r-xref--srcref symbol)))
    (when ref
      (let* ((file (nth 0 ref))
             (line (nth 1 ref))
             (col (nth 2 ref))
             ;; Check if the file srcref is cached by ESS.
             (ess-ref (gethash file ess--srcrefs))
             (ess-buff (when ess-ref (ess--dbg-find-buffer (car ess-ref))))
             ;; Check if the file seems like it's in an R package, something
             ;; like "/tmp/R-downloads/pkg/R/src.R".
             (r-src-file (when (and (not ess-ref)
                                    (string-match "/\\(R/.*\\)$" file))
                           (match-string 1 file))))
        (cond
         (ess-buff
          ;; FIXME: this breaks when eval is on larger spans than function
          (xref-make symbol (xref-make-buffer-location ess-buff (nth 2 ess-ref))))
         ((file-readable-p file)
          (xref-make symbol (xref-make-file-location file line col)))
         (r-src-file
          (when-let ((pkg-file (ess-r-xref--pkg-srcfile symbol r-src-file)))
            (xref-make symbol (xref-make-file-location
                               (expand-file-name pkg-file) line col))))
         (t nil))))))

(provide 'ess-r-xref)

;;; ess-r-xref.el ends here
