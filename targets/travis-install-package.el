;;; travis-install-package.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file simulates a user installing the ess-$VERSION.tar file and
;; then loading ess. It's meant to be used for testing purposes only.

;;; Code:

(require 'package)
(require 'subr-x)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)

;; Get julia-mode and install
(package-refresh-contents)
(package-install 'julia-mode)

;; This file gets called from one directory up
(when-let ((file (directory-files default-directory t ".tar$")))
  (package-install-file (car file)))

(require 'ess-site)


;;; travis-install-package.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
