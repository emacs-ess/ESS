;;; ess-site.el --- user customization of ESS  -*- lexical-binding: t; -*-

;; Copyright (C) 1993-2020 Free Software Foundation, Inc.
;; Author: David Smith <D.M.Smith@lancaster.ac.uk>
;; Created: 12 Nov 1993
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>

;;; Commentary:

;; Load path, autoloads, and major modes
;; ========================================
;;
;; This file defines all the site-specific customizations for ESS. It should be
;; edited on a per-site basis. users who wish to use ESS should add the path to
;; ess-site to their `load-path' and require it:
;;
;;    (add-to-list 'load-path "/path/to/ess/lisp-directory");;
;;    (require 'ess-site)
;;
;; For most users the variable ess-lisp-directory will automatically be set
;; correctly. If you are working with an old Emacs, one in which file-truename
;; is not defined, then you might need to change the value of ess-lisp-directory
;; to the directory which is to contain the file ess-site.elc. This is probably
;; the current directory, or the value of LISPDIR if it was set in the Makefile.
;;
;; Debug startup: (setq ess-show-load-messages t)

;;; Code:

(when load-file-name
  ;; Modify this if ess-site.el is not in the ./lisp/ directory
  (defvar ess-lisp-directory (file-name-directory load-file-name)
    "Directory containing ess-site.el(c) and other ESS Lisp files.")
  (add-to-list 'load-path (directory-file-name ess-lisp-directory)))

;;; Loading popular dialects (they should become optional in the future)

(require 'ess-r-mode)
(require 'essd-els) ;; ess-remote
(require 'ess-sas-d)
(require 'ess-bugs-d)
(require 'ess-jags-d)
(require 'ess-toolbar)

(provide 'ess-site)

;;; ess-site.el ends here
