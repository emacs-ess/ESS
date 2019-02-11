;;; create-pkg-file.el --- Create ess-pkg.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; Creates ess-pkg.el


(require 'package)

;; This script is called from one directory up

;; Make documentation
(shell-command "make -C doc")
(dolist (file '("doc/ess.info" "doc/dir"))
  (copy-file file (file-name-nondirectory file))
  (delete-file file))
;; Generate ess-pkg.el
(find-file "ess.el")
(package-generate-description-file (package-buffer-info) "ess-pkg.el")

;;; create-pkg-file.el ends here
