;;; ess-s3-d.el ---  S 3 (AT&T version) customization

;; Copyright (C) 1997 A. J. Rossini
;; Copyright (C) 1998--2005 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Maintainer: ESS-core <ESS-core@r-project.org>

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

;; This file defines all the S 3 customizations for ess-mode.

;;; Code:

(require 'ess-s-lang)

(defvar S3-customize-alist
  (append
   '((ess-local-customize-alist         . 'S3-customize-alist)
     (ess-dialect                       . "S3")
     (ess-loop-timeout                  . ess-S-loop-timeout);fixme: dialect spec.
     (ess-change-sp-regexp              . ess-S-change-sp-regexp)
     (ess-help-sec-keys-alist           . ess-help-S3-sec-keys-alist)
     (ess-object-name-db-file           . "ess-s3-namedb.el" )
     (inferior-ess-program              . inferior-S3-program) ;        "S")
     (inferior-ess-help-command         . "help(\"%s\")\n")
     (inferior-ess-help-filetype . nil)
     (inferior-ess-search-list-command  . "search()\n")
     (inferior-ess-objects-command      . "objects(%d)\n")
     (inferior-ess-start-file           . nil) ;"~/.ess-S3")
     (inferior-ess-start-args       . "")
     (ess-STERM  . "iESS")
     )
   S+common-cust-alist); use S+ ones here; partly overwritten above!!

  "Variables to customize for S3")

(defun S3 (&optional proc-name)
  "Call 'S 3.x', the version from AT&T."
  (interactive)
  (setq ess-customize-alist S3-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(S3): ess-dialect=%s, buf=%s\n" ess-dialect (current-buffer)))
  (inferior-ess)
  (if inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start)))


(defun S3-mode (&optional proc-name)
  "Major mode for editing S3 source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S3-customize-alist)
  (ess-mode S3-customize-alist proc-name)
  (if ess-imenu-use-S (ess-imenu-S)))


 ; Provide package

(provide 'ess-s3-d)

;;; ess-s3-d.el ends here
