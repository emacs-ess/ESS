;;; ess-site.el --- user customization of ESS

;; Copyright (C) 1993 David M. Smith
;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2018 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, Stephen Eglen,
;;      Vitalie Spinu, and Lionel Henry.

;; Author: David Smith <D.M.Smith@lancaster.ac.uk>
;; Created: 12 Nov 1993
;; Maintainer: ESS-core <ESS-core@r-project.org>
;; Keywords: local

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/


;;; Commentary:

;; This file defines all the site-specific customizations for ESS.  It should be
;; edited on a per-site basis.  Read the comments (1.1 in Section 1 to see if
;; ess-site.el must be edited.  The final directory location of this file must be
;; supplied in ess-lisp-directory.  The editing of remaining sections is
;; optional.  It should then be byte-compiled, and users who wish to use ESS
;; should add the path to ess-site to their `load-path' and require it:
;;
;;    (add-to-list 'load-path "/path/to/ess/lisp-directory");;
;;    (require 'ess-site)

;;; Code:

;;;; Load path, autoloads, and major modes
;;;; ========================================
;;
;; For most users the variable ess-lisp-directory will automatically
;; be set correctly.  If you are working with an old emacs, one in
;; which file-truename is not defined, then you might need to change
;; the value of ess-lisp-directory to the directory which is to
;; contain the file ess-site.elc.  This is probably the current
;; directory, or the value of LISPDIR if it was set in the Makefile.

;; DEBUG: (setq ess-show-load-messages t); instead of nil above

;; This sets `ess-lisp-directory' either from the current directory
;; when the file is being `load'ed, or from the installed location
;; otherwise. This way, users can load ESS without having added ESS to
;; `load-path'.
(defvar ess-lisp-directory
  ;; A nice default
  (directory-file-name
   (file-name-directory
    (if load-file-name
        (file-truename load-file-name)
      (locate-library "ess-site") )))
  "Directory containing ess-site.el(c) and other ESS Lisp files.")

(defvar ess-etc-directory nil
  "Location of the ESS etc/ directory.
The ESS etc directory stores various auxillary files that are useful
for ESS, such as icons.")


;; Depending on how ESS is loaded the `load-path' might not contain
;; the `lisp' directory. For this reason we need to add it before we
;; start requiring ESS files
(add-to-list 'load-path (file-name-as-directory ess-lisp-directory))
;; Add ess-lisp-directory/obsolete to load-path; files here will
;; automatically warn that they are obsolete when loaded.
(add-to-list 'load-path (file-name-as-directory (expand-file-name "obsolete" ess-lisp-directory)))

(require 'ess-utils)
(ess-write-to-dribble-buffer
 (format "[ess-site:] ess-lisp-directory = '%s'" ess-lisp-directory))


;; If ess.info is not found, then ess-lisp-directory/../doc/info is added
;; resurrecting Stephen's version with a bug-fix
(unless (locate-file "ess.info" Info-default-directory-list)
  (add-to-list 'Info-default-directory-list (expand-file-name "../doc/info/" ess-lisp-directory)))


;;; Loading popular dialects (they should become optional in the future)

;; R and Julia
(require 'ess-sp6-d)
(require 'ess-r-mode)

(when (require 'julia-mode nil 'no-error)
  (require 'ess-julia))

;; ess-remote
(require 'essd-els)

;; Stata, SAS, BUGS, and JAGS
(require 'ess-stata-mode)
(require 'ess-sas-d)
(require 'ess-bugs-d)
(require 'ess-jags-d)

(ess-write-to-dribble-buffer
 (format "[ess-site.el]: ess-customize-alist=%s \n"
         ess-customize-alist))

;;; Literate Data Analysis
(require 'ess-noweb)
(require 'ess-swv)

(ess-write-to-dribble-buffer
 (format "[ess-site.el _2_]: ess-customize-alist=%s \n"
         ess-customize-alist))

;;; Speedbar and mouse
(require 'ess-mouse)

;;; Toolbar support
(require 'ess-toolbar)

(ess-write-to-dribble-buffer "[ess-site:] *very* end ...")


(provide 'ess-site)

;;; ess-site.el ends here
