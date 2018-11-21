;;; create-pkg-file.el --- Create ess-pkg.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; Creates ess-pkg.el


(require 'package)

;; This script is called from one directory up

;; Copy lisp into top-level
(copy-directory "lisp" "." t t t)
(delete-directory "lisp" t nil)
;; Make documentation
(shell-command "make -C doc info/ess.info")
(dolist (file '("doc/info/ess.info" "doc/info/dir"))
  (copy-file file (file-name-nondirectory file))
  (delete-file file))
;; Generate ess-pkg.el
(find-file "ess.el")
(package-generate-description-file (package-buffer-info) "ess-pkg.el")

;;; create-pkg-file.el ends here
