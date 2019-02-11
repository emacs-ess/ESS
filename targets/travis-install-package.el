;;; travis-install-package.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file simulates a user installing the ess-$VERSION.tar file and
;; then loading ess. It's meant to be used for testing purposes only.

;;; Code:

(require 'package)
(require 'subr-x)

(package-initialize)

;; This file gets called from one directory up
(when-let ((file (directory-files default-directory t ".tar$")))
  (package-install-file (car file)))

(require 'ess-site)


;;; travis-install-package.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
