;;; ess-package.el --- user customization of ESS

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
;; We reconstruct the melpa packaging logic to effect a melpa install directly
;; from the github source.  Code entrypoint is `make install-dry' from the lisp
;; subdirectory.

;;; Code:

(require 'package-build)
(setq package-user-dir (expand-file-name "dist"))
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")) ;; dependencies
(package-refresh-contents)
(setq rcp (package-recipe-lookup "ess"))
(unless (file-exists-p package-build-archive-dir)
  (make-directory package-build-archive-dir))
(let ((slug (getenv "TRAVIS_PULL_REQUEST_SLUG"))
      (branch (getenv "TRAVIS_PULL_REQUEST_BRANCH"))
      (commit (getenv "TRAVIS_PULL_REQUEST_SHA")))
  (when slug
    (oset rcp :repo slug))
  (when branch
    (oset rcp :branch branch))
  (when commit
    (oset rcp :commit commit)))
(let ((debug-on-error t))
  (package-build--package rcp (package-build--checkout rcp))
  (package-install-file (car (file-expand-wildcards 
                              (concat package-build-archive-dir "ess*.tar")))))
