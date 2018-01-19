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
(require 'ess)


;;; Xref API

(defun ess-r-xref-backend ()
  "An `xref-backend-functions' implementation for `R-mode'."
  'ess-r)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ess-r)))
  (ess-symbol-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql ess-r)) symbol)
  (inferior-ess-r-force)
  (when (ess-r-xref--exists symbol)
    (if (ess-r-xref--has-srcfile symbol)
        (ess-r-xref--find-srcfile symbol)
      ;; TODO: Implement non-srcfile references.
      nil)))


;;; Source File Locations

(defun ess-r-xref--exists (symbol)
  "Check whether SYMBOL is a known R symbol."
  (ess-boolean-command (format "exists(\"%s\")\n" symbol)))

(defun ess-r-xref--has-srcfile (symbol)
  "Checks that the R symbol SYMBOL has a valid source file reference.

Most functions will not have source file references, either
because they were sent to the interpreter directly, or because
they are loaded via byte-compiled packages."
  (ess-boolean-command
   (format
    "!is.null(utils::getSrcref(%s)) && file.exists(utils::getSrcFilename(%s))\n"
    symbol symbol)))

(defun ess-r-xref--find-srcfile (symbol)
  "Creates an xref for the source file reference of R symbol SYMBOL."
  (let ((file (ess-string-command
               (format "cat(utils::getSrcFilename(%s)[1], \"\\n\")\n" symbol)))
        (line (ess-string-command
               (format "cat(utils::getSrcLocation(%s, which = \"line\")[1], \"\\n\")\n" symbol))))
    (list (xref-make (format "%s" symbol)
                     (xref-make-file-location
                      (expand-file-name (string-trim file))
                      (string-to-number line)
                      0)))))

(provide 'ess-r-xref)

;;; ess-r-xref.el ends here
