;;; travis-install-package.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file simulates a user installing the ess-$VERSION.tar file and
;; then loading ess. It's meant to be used for testing purposes only.

;;; Code:

(package-initialize)
(require 'subr-x)

;; Get julia-mode and install
(let ((julia (url-retrieve-synchronously
              "https://raw.githubusercontent.com/JuliaEditorSupport/julia-emacs/master/julia-mode.el" nil t)))
  (with-current-buffer julia
    (search-forward ";;; julia-mode.el")
    (delete-region (point-min) (match-beginning 0))
    (package-install-from-buffer)))

;; This file gets called from one directory up
(when-let ((file (directory-files default-directory t ".tar$")))
  (package-install-file (car file)))

(require 'ess-site)


;;; travis-install-package.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
